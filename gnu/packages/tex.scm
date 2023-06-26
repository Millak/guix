;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2022 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016-2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021-2023 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
;;; Copyright © 2022 Simon South <simon@simonsouth.net>
;;; Copyright © 2022 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2022 Fabio Natali <me@fabionatali.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2023 Thomas Albers Raviola <thomas@thomaslabs.org>
;;; Copyright © 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Dominik Delgado Steuter <d@delgado.nrw>
;;; Copyright © 2023 Timothy Sample <samplet@ngyro.com>
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

(define-module (gnu packages tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system texlive)
  #:use-module (guix utils)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages texinfo)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:hide (zip)))

(define-syntax-rule (define-deprecated-package old-name name)
  "Define OLD-NAME as a deprecated package alias for NAME."
  (define-deprecated/public old-name name
    (deprecated-package (symbol->string 'old-name) name)))

(define-public texlive-libkpathsea
  (package
    (name "texlive-libkpathsea")
    (version "20230313")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://tug.org/historic/systems/texlive/"
                           (string-take version 4)
                           "/texlive-" version "-source.tar.xz"))
       (sha256
        (base32
         "1fbrkv7g9j6ipmwjx27l8l9l974rmply8bhf7c2iqc6h3q7aly1q"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        #~(begin
            (with-directory-excursion "libs"
              (for-each
               delete-file-recursively
               (scandir "."
                        (lambda (file)
                          (and (not (member file '("." "..")))
                               (eq? 'directory (stat:type (stat file))))))))
            (with-directory-excursion "texk"
              (let ((preserved-directories '("." ".." "kpathsea")))
                (for-each
                 delete-file-recursively
                 (scandir "."
                          (lambda (file)
                            (and (not (member file preserved-directories))
                                 (eq? 'directory (stat:type (stat file)))))))))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:out-of-source? #t
      #:configure-flags
      #~(list "--disable-static"
              "--disable-native-texlive-build"
              "--enable-shared"
              "--with-banner-add=/GNU Guix"
              "--disable-all-pkgs"
              "--enable-kpathsea")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'customize-texmf.cnf
            ;; The default "texmf.cnf" file is provided by this package.
            ;; Every variable of interest is set relatively to the GUIX_TEXMF
            ;; environment variable defined via a search path below.
            ;;
            ;; This phase must happen before the `configure' phase, because
            ;; the value of the TEXMFCNF variable (modified along with the
            ;; SELFAUTOLOC reference below) is used at compile time to
            ;; generate "paths.h" file.
            (lambda _
              (substitute* "texk/kpathsea/texmf.cnf"
                (("^TEXMFROOT = .*")
                 "TEXMFROOT = {$GUIX_TEXMF}/..\n")
                (("^TEXMF = .*")
                 "TEXMF = {$GUIX_TEXMF}\n")
                (("\\$SELFAUTOLOC(/share/texmf-dist/web2c)" _ suffix)
                 (string-append #$output suffix))
                ;; Don't truncate lines.
                (("^error_line = .*$") "error_line = 254\n")
                (("^half_error_line = .*$") "half_error_line = 238\n")
                (("^max_print_line = .*$") "max_print_line = 1000\n"))))
          (add-after 'unpack 'patch-directory-traversal
            ;; When ST_NLINK_TRICK is set, kpathsea attempts to avoid work
            ;; when searching files by assuming that a directory with exactly
            ;; two links has no subdirectories.  This assumption does not hold
            ;; in our case, so some directories with symlinked sub-directories
            ;; would not be traversed.
            (lambda _
              (substitute* "texk/kpathsea/config.h"
                (("#define ST_NLINK_TRICK") ""))))
          (add-after 'install 'post-install
            (lambda _
              (with-directory-excursion "texk/kpathsea"
                (invoke "make" "install")))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_TEXMF")
            (files '("share/texmf-dist")))))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Path searching library")
    (description
     "Kpathsea is a library whose purpose is to return a filename from a list
of user-specified directories similar to how shells look up executables.")
    (license license:lgpl2.1)))

(define-public texlive-scripts
  (package
    (name "texlive-scripts")
    (version (number->string %texlive-revision))
    ;; We cannot use `texlive-origin' because its locations start out in
    ;; "texmf-dist" directory which is one level below "tlpkg" that we also
    ;; need to pull here.
    (source (origin
              (method svn-multi-fetch)
              (uri (svn-multi-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        %texlive-tag "/Master/"))
                    (locations
                     (list "texmf-dist/doc/man/man1/fmtutil-sys.1"
                           "texmf-dist/doc/man/man1/fmtutil-sys.man1.pdf"
                           "texmf-dist/doc/man/man1/fmtutil-user.1"
                           "texmf-dist/doc/man/man1/fmtutil-user.man1.pdf"
                           "texmf-dist/doc/man/man1/fmtutil.1"
                           "texmf-dist/doc/man/man1/fmtutil.man1.pdf"
                           "texmf-dist/doc/man/man1/install-tl.1"
                           "texmf-dist/doc/man/man1/install-tl.man1.pdf"
                           "texmf-dist/doc/man/man1/mktexfmt.1"
                           "texmf-dist/doc/man/man1/mktexfmt.man1.pdf"
                           "texmf-dist/doc/man/man1/mktexlsr.1"
                           "texmf-dist/doc/man/man1/mktexlsr.man1.pdf"
                           "texmf-dist/doc/man/man1/mktexmf.1"
                           "texmf-dist/doc/man/man1/mktexmf.man1.pdf"
                           "texmf-dist/doc/man/man1/mktexpk.1"
                           "texmf-dist/doc/man/man1/mktexpk.man1.pdf"
                           "texmf-dist/doc/man/man1/mktextfm.1"
                           "texmf-dist/doc/man/man1/mktextfm.man1.pdf"
                           "texmf-dist/doc/man/man1/texhash.1"
                           "texmf-dist/doc/man/man1/texhash.man1.pdf"
                           "texmf-dist/doc/man/man1/tlmgr.1"
                           "texmf-dist/doc/man/man1/tlmgr.man1.pdf"
                           "texmf-dist/doc/man/man1/updmap-sys.1"
                           "texmf-dist/doc/man/man1/updmap-sys.man1.pdf"
                           "texmf-dist/doc/man/man1/updmap-user.1"
                           "texmf-dist/doc/man/man1/updmap-user.man1.pdf"
                           "texmf-dist/doc/man/man1/updmap.1"
                           "texmf-dist/doc/man/man1/updmap.man1.pdf"
                           "texmf-dist/doc/man/man5/fmtutil.cnf.5"
                           "texmf-dist/doc/man/man5/fmtutil.cnf.man5.pdf"
                           "texmf-dist/doc/man/man5/updmap.cfg.5"
                           "texmf-dist/doc/man/man5/updmap.cfg.man5.pdf"
                           "texmf-dist/dvips/tetex/"
                           "texmf-dist/fonts/enc/dvips/tetex/"
                           "texmf-dist/fonts/map/dvips/tetex/"
                           "texmf-dist/scripts/texlive/fmtutil-sys.sh"
                           "texmf-dist/scripts/texlive/fmtutil-user.sh"
                           "texmf-dist/scripts/texlive/fmtutil.pl"
                           "texmf-dist/scripts/texlive/mktexlsr.pl"
                           "texmf-dist/scripts/texlive/mktexmf"
                           "texmf-dist/scripts/texlive/mktexpk"
                           "texmf-dist/scripts/texlive/mktextfm"
                           "texmf-dist/scripts/texlive/tlmgr.pl"
                           "texmf-dist/scripts/texlive/updmap-sys.sh"
                           "texmf-dist/scripts/texlive/updmap-user.sh"
                           "texmf-dist/scripts/texlive/updmap.pl"
                           "texmf-dist/web2c/fmtutil-hdr.cnf"
                           "texmf-dist/web2c/updmap-hdr.cfg"
                           "texmf-dist/web2c/updmap.cfg"
                           "tlpkg/gpg/"
                           "tlpkg/installer/config.guess"
                           "tlpkg/installer/curl/curl-ca-bundle.crt"
                           "tlpkg/TeXLive/"
                           "tlpkg/texlive.tlpdb"))
                    (revision %texlive-revision)))
              (sha256
               "0sqbg5kjpzkpm1fq2c9hpf4f21bvjs3xas944dlbqp44lsqhcmsk")))
    (outputs '("out" "doc"))
    (build-system copy-build-system)
    (arguments
     (list
      #:imported-modules `(,@%copy-build-system-modules
                           (guix build texlive-build-system)
                           (guix build union))
      #:modules '((guix build copy-build-system)
                  ((guix build texlive-build-system) #:prefix tex:)
                  (guix build utils))
      #:install-plan
      #~'(("texmf-dist/dvips/"   "share/texmf-dist/dvips")
          ("texmf-dist/fonts/"   "share/texmf-dist/fonts")
          ("texmf-dist/scripts/" "share/texmf-dist/scripts")
          ("texmf-dist/web2c/"   "share/texmf-dist/web2c")
          ("tlpkg/"              "share/tlpkg"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-scripts
            (lambda _
              ;; First patch shell scripts with ".sh" extension.
              (with-directory-excursion "texmf-dist"
                ((assoc-ref tex:%standard-phases 'patch-shell-scripts)))
              ;; Then patch scripts without such extension.
              (let ((dirs (map (compose dirname which)
                               (list "awk" "cat" "grep" "sed"))))
                (substitute*
                    (find-files "texmf-dist/scripts/" "^mktex(mf|pk|tfm)$")
                  (("^version=" m)
                   (format #false "PATH=\"~{~a:~}$PATH\"; export PATH~%~a"
                           dirs m))))
              ;; Make sure that fmtutil can find its Perl modules.
              (substitute* "texmf-dist/scripts/texlive/fmtutil.pl"
                (("\\$TEXMFROOT/")
                 (string-append #$output "/share/")))
              ;; Likewise for updmap.pl.
              (substitute* "texmf-dist/scripts/texlive/updmap.pl"
                (("\\$TEXMFROOT/tlpkg")
                 (string-append #$output "/share/tlpkg")))
              ;; Likewise for the tlmgr.
              (substitute* "texmf-dist/scripts/texlive/tlmgr.pl"
                ((".*\\$::installerdir = \\$Master.*" all)
                 (format #f "  $Master = ~s;~%~a"
                         (string-append #$output "/share")
                         all)))))
          (add-after 'unpack 'fix-fmtutil
            ;; The line below generates an error when running "fmtutil".
            (lambda _
              (substitute* "texmf-dist/scripts/texlive/fmtutil.pl"
                (("require TeXLive::TLWinGoo if .*") ""))))
          (add-after 'install 'install-doc
            (lambda _
              (let ((doc (string-append #$output:doc "/share/texmf-dist/doc")))
                (mkdir-p doc)
                (copy-recursively "texmf-dist/doc/" doc))))
          (add-after 'install-doc 'link-scripts
            (lambda* (#:key outputs #:allow-other-keys)
              (with-directory-excursion "texmf-dist"
                (apply (assoc-ref tex:%standard-phases 'link-scripts)
                       (list #:outputs outputs
                             #:link-scripts
                             (find-files "scripts")))))))))
    (inputs (list perl))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "TeX Live infrastructure programs")
    (description
     "This package provides core TeX Live scripts such as @code{updmap},
@code{fmtutil}, and @code{tlmgr}.  It is is automatically installed alongside
@code{texlive-bin}.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-hyphen-complete
  (package
    (name "texlive-hyphen-complete")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/dehyph-exptl/"
                   "doc/generic/elhyphen"
                   "doc/generic/huhyphen"
                   "doc/generic/hyph-utf8/"
                   "doc/luatex/hyph-utf8/"
                   "doc/generic/ukrhyph/"
                   "source/generic/hyph-utf8/"
                   "source/luatex/hyph-utf8/"
                   "source/generic/ruhyphen/"
                   "tex/generic/config/"
                   "tex/generic/dehyph/"
                   "tex/generic/dehyph-exptl/"
                   "tex/generic/hyph-utf8/"
                   "tex/generic/hyphen/"
                   "tex/generic/ruhyphen/"
                   "tex/generic/ukrhyph/"
                   "tex/luatex/hyph-utf8/")
             (base32
              "1vzv92jvmnnga1xz5vrv8i6cy0dvrrly5x9nfrfzshlkm9bi3g4c")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:tex-engine "tex"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-ruby-byebug-dependency
            ;; Avoid dependency on byebug to reduce package closure
            ;; significantly, see <https://issues.guix.gnu.org/55997>.
            (lambda _
              (substitute* "source/generic/hyph-utf8/lib/tex/hyphen/language.rb"
                (("require 'byebug'") ""))))
          (add-before 'build 'regenerate-converters
            (lambda _
              (let ((root (getcwd)))
                (for-each delete-file
                          (find-files "tex/generic/hyph-utf8/conversions/"))
                (with-directory-excursion "source/generic/hyph-utf8"
                  (substitute* "generate-converters.rb"
                    (("\\$path_root=File.*")
                     (string-append "$path_root=\"" root "\"\n"))
                    ;; Avoid error with newer Ruby.
                    (("#1\\{%") "#1{%%"))
                  (invoke "ruby" "generate-converters.rb")))))
          (add-before 'build 'regenerate-patterns
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((root (getcwd))
                     (hyph-utf8 (string-append root "/tex/generic/hyph-utf8"))
                     (loaders (string-append hyph-utf8 "/loadhyph"))
                     (patterns (string-append hyph-utf8 "/patterns/txt"))
                     (ptex (string-append hyph-utf8 "/patterns/ptex"))
                     (quote (string-append hyph-utf8 "/patterns/quote")))
                ;; Initial clean-up.  Some files are not generated and need to
                ;; be preserved.
                (for-each delete-file (find-files loaders))
                (let ((preserved (list "hyph-sr-cyrl.hyp.txt"
                                       "hyph-sr-cyrl.pat.txt"
                                       "hyph-de-1901.ec.tex"
                                       "hyph-de-1996.ec.tex"
                                       "hyph-ru.t2a.tex"
                                       "hyph-uk.t2a.tex"
                                       "hyph-zh-latn-pinyin.ec.tex")))
                  (for-each
                   (lambda (directory)
                     (for-each delete-file
                               (find-files directory
                                           (lambda (f _)
                                             (not (member (basename f)
                                                          preserved))))))
                   (list patterns ptex quote)))
                ;; Generate plain patterns.  Write to the local directory.
                ;; Install phase will take care of moving the files to the
                ;; output.
                (with-directory-excursion "source/generic/hyph-utf8/"
                  (substitute* "lib/tex/hyphen/path.rb"
                    (("^([[:blank:]]+)TeXROOT = .*" _ indent)
                     (string-append indent "TeXROOT = \"" root "\"\n")))
                  (substitute* "generate-plain-patterns.rb"
                    ;; Ruby 2 does not need this.
                    (("require 'unicode'") "")
                    (("File\\.join\\(PATH::TXT")
                     (string-append "File.join(\"" patterns "\""))
                    (("File\\.join\\(PATH::QUOTE")
                     (string-append "File.join(\"" quote "\"")))
                  (invoke "ruby" "generate-plain-patterns.rb")
                  ;; Build pattern loaders.
                  (substitute* "generate-pattern-loaders.rb"
                    (("File\\.join\\(PATH::LOADER")
                     (string-append "File.join(\"" loaders "\"")))
                  (invoke "ruby" "generate-pattern-loaders.rb")
                  ;; Build ptex patterns.
                  (substitute* "generate-ptex-patterns.rb"
                    (("File\\.join\\(PATH::PTEX")
                     (string-append "File.join(\"" ptex "\"")))
                  (invoke "ruby" "generate-ptex-patterns.rb"))))))))
    (native-inputs
     (list ruby-2.7
           ruby-hydra-minimal/pinned
           texlive-docstrip
           texlive-tex))
    (home-page "https://ctan.org/pkg/hyph-utf8")
    (synopsis "Hyphenation patterns expressed in UTF-8")
    (description
     "Modern native UTF-8 engines such as XeTeX and LuaTeX need hyphenation
patterns in UTF-8 format, whereas older systems require hyphenation patterns
in the 8-bit encoding of the font in use (such encodings are codified in the
LaTeX scheme with names like OT1, T2A, TS1, OML, LY1, etc).  The present
package offers a collection of conversions of existing patterns to UTF-8
format, together with converters for use with 8-bit fonts in older systems.

This Guix-specific package provides hyphenation patterns for all languages
supported in TeX Live.  It is a strict super-set of code{hyphen-base} package
and should be preferred to it whenever a package would otherwise depend on
@code{hyph-utf8}.")
    ;; Individual files each have their own license.  Most of these files are
    ;; independent hyphenation patterns.
    (license
     (list license:asl2.0
           license:bsd-3
           license:cc0
           license:expat
           license:gpl2
           license:gpl2+
           license:gpl3+
           license:knuth
           license:lgpl2.1
           license:lgpl2.1+
           license:lgpl3+
           license:lppl
           license:lppl1.0+
           license:lppl1.2+
           license:lppl1.3
           license:lppl1.3+
           license:lppl1.3a+
           license:mpl1.1
           license:public-domain
           license:wtfpl2
           (license:fsf-free
            "/tex/generic/hyph-utf8/patterns/tex/hyph-eu.tex")
           (license:non-copyleft
            "file:///tex/generic/hyph-utf8/patterns/tex/hyph-bg.tex"
            "Ancestral BSD variant")
           (license:non-copyleft
            "file:///tex/generic/hyph-utf8/patterns/tex/hyph-en-us.tex"
            "FSF all permissive license")
           (license:non-copyleft
            "/tex/generic/hyph-utf8/patterns/tex/hyph-no.tex"
            "FSF All permissive license")
           (license:non-copyleft
            "file:///tex/generic/hyph-utf8/patterns/tex/hyph-sa.tex")))))

(define-deprecated-package texlive-dehyph texlive-hyphen-complete)
(define-deprecated-package texlive-generic-dehyph-exptl texlive-hyphen-complete)
(define-deprecated-package texlive-generic-hyph-utf8 texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-afrikaans texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-ancientgreek texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-armenian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-basque texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-belarusian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-bulgarian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-catalan texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-chinese texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-churchslavonic texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-coptic texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-croatian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-czech texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-danish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-dutch texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-english texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-esperanto texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-estonian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-ethiopic texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-finnish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-french texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-friulan texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-galician texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-georgian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-german texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-greek texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-hungarian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-icelandic texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-indic texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-indonesian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-interlingua texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-irish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-italian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-kurmanji texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-latin texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-latvian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-lithuanian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-macedonian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-mongolian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-norwegian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-occitan texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-pali texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-piedmontese texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-polish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-portuguese texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-romanian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-romansh texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-russian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-sanskrit texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-schoolfinnish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-serbian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-slovak texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-slovenian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-spanish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-swedish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-thai texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-turkish texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-turkmen texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-ukrainian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-uppersorbian texlive-hyphen-complete)
(define-deprecated-package texlive-hyphen-welsh texlive-hyphen-complete)
(define-deprecated-package texlive-ruhyphen texlive-hyphen-complete)
(define-deprecated-package texlive-ukrhyph texlive-hyphen-complete)

(define-public texlive-bin
  (package/inherit texlive-libkpathsea
    (name "texlive-bin")
    (source
     (origin
       (inherit (package-source texlive-libkpathsea))
       (snippet
        ;; TODO: Unbundle stuff in texk/dvisvgm/dvisvgm-src/libs too.
        #~(with-directory-excursion "libs"
            (let ((preserved-directories '("." ".." "lua53" "luajit" "pplib" "xpdf")))
              ;; Delete bundled software, except Lua which cannot easily be
              ;; used as an external dependency, pplib and xpdf which aren't
              ;; supported as system libraries (see m4/kpse-xpdf-flags.m4).
              (for-each delete-file-recursively
                        (scandir "."
                                 (lambda (file)
                                   (and (not (member file preserved-directories))
                                        (eq? 'directory
                                             (stat:type (stat file))))))))))))
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26))
      #:out-of-source? #t
      #:parallel-tests? #f              ;bibtex8.test fails otherwise
      #:configure-flags
      #~(let ((kpathsea #$(this-package-input "texlive-libkpathsea")))
          (list "--with-banner-add=/GNU Guix"
                "--enable-shared"
                "--disable-native-texlive-build"
                "--disable-static"
                "--disable-linked-scripts"
                "--disable-kpathsea"
                "--with-system-cairo"
                "--with-system-freetype2"
                "--with-system-gd"
                "--with-system-gmp"
                "--with-system-graphite2"
                "--with-system-harfbuzz"
                "--with-system-icu"
                "--with-system-libgs"
                "--with-system-libpaper"
                "--with-system-libpng"
                "--with-system-mpfr"
                "--with-system-pixman"
                "--with-system-potrace"
                "--with-system-teckit"
                "--with-system-zlib"
                "--with-system-zziplib"
                ;; Help locating external kpathsea.  For some reason
                ;; PKG-CONFIG is unable to find it.
                "--with-system-kpathsea"
                (format #f "--with-kpathsea-includes=~a/include" kpathsea)
                (format #f "--with-kpathsea-lib=~a/lib" kpathsea)
                ;; LuaJIT is not ported to some architectures yet.
                #$@(if (or (target-ppc64le?)
                           (target-riscv64?))
                       '("--disable-luajittex"
                         "--disable-luajithbtex"
                         "--disable-mfluajit")
                       '())))
      ;; Disable tests on some architectures to cope with a failure of
      ;; luajiterr.test.
      ;;
      ;; XXX FIXME fix luajit properly on these architectures.
      #:tests? (let ((s (or (%current-target-system)
                            (%current-system))))
                 (not (or (string-prefix? "aarch64" s)
                          (string-prefix? "mips64" s)
                          (string-prefix? "powerpc64le" s))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'locate-external-kpathsea
            ;; Despite our best efforts, the configure scripts below is not
            ;; able to find external kpathsea.
            (lambda _
              (substitute* "texk/web2c/configure"
                (("/usr/include /usr/local/include")
                 (string-append #$(this-package-input "texlive-libkpathsea")
                                "/include")))))
          (add-after 'unpack 'patch-psutils-test
            (lambda _
              ;; This test fails due to a rounding difference with libpaper
              ;; 1.2: <https://github.com/rrthomas/libpaper/issues/23>.
              ;;
              ;; Adjust the expected outcome to account for the minute
              ;; difference.
              (substitute* "texk/psutils/tests/playres.ps"
                (("844\\.647799") "844.647797"))))
          (add-after 'unpack 'configure-ghostscript-executable
            ;; ps2eps.pl uses the "gswin32c" ghostscript executable on
            ;; Windows, and the "gs" ghostscript executable on Unix.  It
            ;; detects Unix by checking for the existence of the /usr/bin
            ;; directory.  Since Guix System does not have /usr/bin, it is
            ;; also detected as Windows.
            (lambda _
              (substitute* "utils/ps2eps/ps2eps-src/bin/ps2eps.pl"
                (("gswin32c") "gs"))))
          (add-after 'unpack 'patch-dvisvgm-build-files
            (lambda _
              ;; XXX: Ghostscript is detected, but HAVE_LIBGS is never set, so
              ;; the appropriate linker flags are not added.
              (substitute* "texk/dvisvgm/configure"
                (("^have_libgs=yes" all)
                 (string-append all "\nHAVE_LIBGS=1")))))
          (add-after 'unpack 'disable-failing-test
            (lambda _
              ;; FIXME: This test fails on 32-bit architectures since Glibc
              ;; 2.28: <https://bugzilla.redhat.com/show_bug.cgi?id=1631847>.
              (substitute* "texk/web2c/omegafonts/check.test"
                (("^\\./omfonts -ofm2opl \\$srcdir/tests/check tests/xcheck \\|\\| exit 1")
                 "./omfonts -ofm2opl $srcdir/tests/check tests/xcheck || exit 77"))))
          #$@(if (or (target-ppc32?)
                     (target-riscv64?))
                 ;; Some mendex tests fail on some architectures.
                 `((add-after 'unpack 'skip-mendex-tests
                     (lambda _
                       (substitute* '("texk/mendexk/tests/mendex.test"
                                      "texk/upmendex/tests/upmendex.test")
                         (("srcdir/tests/pprecA-0.ind pprecA-0.ind1 \\|\\| exit 1")
                          "srcdir/tests/pprecA-0.ind pprecA-0.ind1 || exit 77")))))
                 '())
          #$@(if (target-arm32?)
                 `((add-after 'unpack 'skip-faulty-test
                     (lambda _
                       ;; Skip this faulty test on armhf-linux:
                       ;;   https://issues.guix.gnu.org/54055
                       (substitute* '("texk/mendexk/tests/mendex.test"
                                      "texk/upmendex/tests/upmendex.test")
                         (("^TEXMFCNF=" all)
                          (string-append "exit 77 # skip\n" all))))))
                 '())
          (add-after 'install 'post-install
            (lambda _
              ;; Create symbolic links for the latex variants.  We link
              ;; lualatex to luahbtex; see issue #51252 for details.
              (with-directory-excursion (string-append #$output "/bin/")
                (for-each symlink
                          '("pdftex" "pdftex"   "xetex"   "luahbtex")
                          '("latex"  "pdflatex" "xelatex" "lualatex")))
              ;; texlua shebangs are not patched by the patch-source-shebangs
              ;; phase because the texlua executable does not exist at that
              ;; time.
              (setenv "PATH"
                      (string-append (getenv "PATH") ":" #$output "/bin"))
              (with-directory-excursion #$output
                (assoc-ref %standard-phases 'patch-source-shebangs)))))))
    (native-inputs (list groff-minimal pkg-config))
    (inputs
     (list cairo
           config
           fontconfig
           fontforge
           freetype
           gd
           ghostscript
           gmp
           graphite2
           harfbuzz
           icu4c
           libpaper
           libpng
           libxaw
           libxt
           mpfr
           perl
           pixman
           potrace
           python
           ruby-2.7
           tcsh
           teckit
           zlib
           zziplib))
    (propagated-inputs (list texlive-libkpathsea texlive-scripts))
    (synopsis "TeX Live, a package of the TeX typesetting system")
    (description
     "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the binaries.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
    (home-page "https://www.tug.org/texlive/")))

(define-public texlive-tex
  (package
    (name "texlive-tex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/initex.1"
                   "doc/man/man1/initex.man1.pdf"
                   "doc/man/man1/tex.1"
                   "doc/man/man1/tex.man1.pdf")
             (base32
              "1n4jybv4qghg74anpj7n7kj4l908f476q597vyvvq59fd9k5m7mm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-bin? #f
           #:create-formats #~(list "tex")))
    (propagated-inputs
     (list texlive-cm
           texlive-hyphen-base
           texlive-knuth-lib
           texlive-kpathsea
           texlive-plain))
    (home-page "https://ctan.org/pkg/tex")
    (synopsis "Sophisticated typesetting engine")
    (description
     "TeX is a typesetting system that incorporates a macro processor.  A TeX
source document specifies or incorporates a number of macro definitions that
instruct the TeX engine how to typeset the document.  The TeX engine also uses
font metrics generated by Metafont, or by any of several other mechanisms that
incorporate fonts from other sources into an environment suitable for TeX.
TeX has been, and continues, a basis and an inspiration for several other
programs, including e-TeX and PDFTeX.  The distribution includes the source of
Knuth's TeX book; this source is there to read, as an example of writing TeX ;
it should not be processed without Knuth's direct permission.")
    (license license:knuth)))

(define-public texlive-latex
  (package
    (name "texlive-latex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/base/" "makeindex/latex/"
                   "source/latex/base/" "tex/latex/base/")
             (base32
              "0yqxf70rzhzyrr5jrcqmqay9zhjz8f3qhcxak01g5cywdgvzfmpq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:tex-engine "tex"
      #:tex-format #f
      #:build-targets #~(list "unpack.ins")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-lua-build
            ;; The literal tab in the dtx file is translated to the string
            ;; "^^I" in the generated Lua file, which causes a syntax error.
            (lambda _
              (substitute* "source/latex/base/ltluatex.dtx"
                (("\t") "  ")))))))
    (propagated-inputs
     (list texlive-latex-fonts
           texlive-latexconfig
           texlive-luatex
           texlive-pdftex))
    (home-page "https://ctan.org/pkg/latex")
    (synopsis "TeX macro package that defines LaTeX")
    (description
     "LaTeX is a widely-used macro package for TeX, providing many basic
document formating commands extended by a wide range of packages.")
    (license license:lppl1.3c)))

(define-public texlive-latexbug
  (package
    (name "texlive-latexbug")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/latexbug/"
                   "source/latex/latexbug/"
                   "tex/latex/latexbug/")
             (base32
              "0l9cyw41knbw3prsi4rbd2av4qfpkvzjs754847kv72dq1304m1i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/latexbug")
    (synopsis "Bug-classification for LaTeX related bugs")
    (description
     "The package is written in order to help identifying the rightful
addressee for a bug report.  The LaTeX team asks that it will be loaded in any
test file that is intended to be sent to the LaTeX bug database as part of
a bug report.")
    (license license:lppl1.3c)))

(define-public texlive-bidi
  (package
    (name "texlive-bidi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/bidi/" "source/xelatex/bidi/"
                   "tex/xelatex/bidi/")
             (base32
              "0zrmdgzbd8shzv1m1xvfqz515mwy5igkjwnnc4jrm1csbjf7jnj8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bidi")
    (synopsis "Bidirectional typesetting in plain TeX and LaTeX using XeTeX")
    (description
     "The @code{bidi} package provides a convenient interface for typesetting
bidirectional texts with plain TeX and LaTeX.  The package includes
adaptations for use with many other commonly-used packages.")
    (license license:lppl1.3+)))

(define-public texlive-bidi-atbegshi
  (package
    (name "texlive-bidi-atbegshi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/bidi-atbegshi/"
                   "tex/xelatex/bidi-atbegshi/")
             (base32
              "08gawna9hf5p3rn0v5qzszk61zqknixafvh6d2x37x960x493gn7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bidi-atbegshi")
    (synopsis "Bidi-aware shipout macros")
    (description
     "The package adds some commands to the @code{atbegshi} package for proper
placement of background material in the left and right corners of the output
page, in both LTR and RTL modes.  The package only works with @code{xelatex}
format and should be loaded before the @code{bidi} package.")
    (license license:lppl1.3+)))

(define-public texlive-bidicontour
  (package
    (name "texlive-bidicontour")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/bidicontour/"
                   "tex/xelatex/bidicontour/")
             (base32
              "1kiqbn5map3d9bmlvr5cq1snssw44c772xzjp6yyjvg6wg1zy0bn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bidicontour")
    (synopsis "Bidi-aware coloured contour around text")
    (description
     "The package is a re-implementation of the @code{contour} package, making
it Bidi-aware, and adding support of the xdvipdfmx (when the outline option of
the package is used).")
    (license license:lppl1.3+)))

(define-public texlive-bidipagegrid
  (package
    (name "texlive-bidipagegrid")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/bidipagegrid/"
                   "tex/xelatex/bidipagegrid/")
             (base32
              "19jkg4apf1g3whigcchbcf8p14lpxkz9ih2vrw00akwfh8v3ssrk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bidipagegrid")
    (synopsis "Bidi-aware page grid in background")
    (description "The package provides Bidi-aware page grid in background.  It
is based on @code{pagegrid}.")
    (license license:lppl1.3+)))

(define-public texlive-bidipresentation
  (package
    (name "texlive-bidipresentation")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/bidipresentation/"
                   "tex/xelatex/bidipresentation/")
             (base32
              "0b9md68zzj3nzi9b4hdavjz43nwair1xg3b240p0bp5ly5l6pvmr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bidipresentation")
    (synopsis "Experimental bidi presentation")
    (description
     "This package provides a great portion of the code is borrowed from the
texpower bundle, with modifications to get things working properly in both
right to left and left to right modes.")
    (license license:lppl1.3+)))

(define-public texlive-bidishadowtext
  (package
    (name "texlive-bidishadowtext")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/bidishadowtext/"
                   "tex/xelatex/bidishadowtext/")
             (base32
              "1nq71bgz7vag9k138mx8hsf42cjvgry2g4z9jiqmq4almm23a1gq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bidishadowtext")
    (synopsis "Bidi-aware shadow text")
    (description
     "This package allows you to typeset Bidi-aware shadow text.  It is
a re-implementation of the @code{shadowtext} package adding Bidi support.")
    (license license:lppl1.3+)))

(define-public texlive-alphalph
  (package
    (name "texlive-alphalph")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/alphalph/"
                   "source/latex/alphalph/"
                   "tex/generic/alphalph/")
             (base32
              "0ap59hmg0brg2wlh3bl77jxfxrk7hphhdal8cr05mby9bw35gffy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/alphalph")
    (synopsis "Convert numbers to letters")
    (description
     "This package provides commands @code{\\alphalph} and @code{\\AlphAlph}.
They are like @code{\\number} but the expansion consists of lowercase and
uppercase letters respectively (1 to a, 26 to z, 27 to aa, 52 to zz, 53 to ba,
702 to zz, 703 to aaa, etc.).  Alphalph's commands can be used as
a replacement for LaTeX's @code{\\@@alph} and @code{\\@@Alph} macros.")
    (license license:lppl1.3c+)))

(define-public texlive-antt
  (package
    (name "texlive-antt")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/antt/"
                   "doc/latex/antt/"
                   "fonts/afm/public/antt/"
                   "fonts/enc/dvips/antt/"
                   "fonts/map/dvips/antt/"
                   "fonts/opentype/public/antt/"
                   "fonts/tfm/public/antt/"
                   "fonts/type1/public/antt/"
                   "tex/latex/antt/"
                   "tex/plain/antt/")
             (base32
              "1fvmgb581ixc4fvw5l0g11hlvdpf0cld6db0cg3vysw5yabas3vm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/antt")
    (synopsis "Type 1 family of a Polish traditional type")
    (description
     "Antykwa Torunska is a serif font designed by the late Polish typographer
Zygfryd Gardzielewski, reconstructed and digitized as Type 1.")
    (license license:gfl1.0)))

(define-public texlive-asana-math
  (package
    (name "texlive-asana-math")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/asana-math/"
                   "fonts/opentype/public/asana-math/"
                   "fonts/truetype/public/asana-math/")
             (base32
              "1q934gackj9j7b7bvlq7yv1pr9rxrhhip1as7ywgx8d45ddig2rq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/asana-math")
    (synopsis "Font to typeset maths in Xe(La)TeX and Lua(La)TeX")
    (description
     "The Asana-Math font is an OpenType font that includes almost all
mathematical Unicode symbols and it can be used to typeset mathematical text
with any software that can understand the MATH OpenType table.")
    (license license:silofl1.1)))

(define-public texlive-avantgar
  (package
    (name "texlive-avantgar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/avantgar/"
                   "fonts/afm/adobe/avantgar/"
                   "fonts/afm/urw/avantgar/"
                   "fonts/map/dvips/avantgar/"
                   "fonts/tfm/adobe/avantgar/"
                   "fonts/tfm/urw35vf/avantgar/"
                   "fonts/type1/urw/avantgar/"
                   "fonts/vf/adobe/avantgar/"
                   "fonts/vf/urw35vf/avantgar/"
                   "tex/latex/avantgar/")
             (base32
              "1200x40k7wprm4n7srxvgrax2l52vn6d5ri2x0q7zbzzsfxfzkym")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Avant Garde font")
    (description
     "This package provides a drop-in replacement for the Avant Garde font
from Adobe's basic set.")
    (license license:gpl3+)))

(define texlive-docstrip
  (package
    (name "texlive-docstrip")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "/tex/latex/base/docstrip.tex")
             (base32
              "04cwvqs8cx8l60lrwn60krpjg1ada7i8g5mh6cb6bxaz08yvx9i4")))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-bin? #f))
    (home-page "https://www.ctan.org/texlive")
    (synopsis "Utility to strip documentation from TeX files")
    (description "This package provides the docstrip utility to strip
documentation from TeX files.  It is part of the LaTeX base.")
    (license license:lppl1.3+)))

(define-public texlive-underscore
  (package
    (name "texlive-underscore")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/underscore/" "tex/latex/underscore/")
             (base32
              "0slxsxc9azmv3gsm55jkhkv8a06wafankp55hhsdd6k4prp8szrb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/underscore")
    (synopsis "Control the behaviour of @samp{_} in text")
    (description
     "This package causes @code{\\_} in text mode (i.e.,
@code{\\textunderscore}) to print an underscore so that hyphenation of words
either side of it is not affected; a package option controls whether an actual
hyphenation point appears after the underscore, or merely a break point.  The
package also arranges that, while in text, @samp{_} itself behaves as
@code{\\textunderscore} (the behaviour of @samp{_} in maths mode is not
affected).")
    (license license:lppl1.2+)))

(define-public texlive-unicode-data
  (package
    (name "texlive-unicode-data")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/unicode-data/"
                   "tex/generic/unicode-data/")
             (base32
              "13zff8fk0fwa1ab8wc5yfbay0022jkk1j9zq5azn6gzcxs9szm6q")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/unicode-data")
    (synopsis "Unicode data and loaders for TeX")
    (description "This bundle provides generic access to Unicode Consortium
data for TeX use.  It contains a set of text files provided by the Unicode
Consortium which are currently all from Unicode 8.0.0, with the exception of
@code{MathClass.txt} which is not currently part of the Unicode Character
Database.  Accompanying these source data are generic TeX loader files
allowing this data to be used as part of TeX runs, in particular in building
format files.  Currently there are two loader files: one for general character
set up and one for initializing XeTeX character classes as has been carried
out to date by @code{unicode-letters.tex}.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-unicode-data texlive-unicode-data)

(define-public texlive-hologo
  (package
    (name "texlive-hologo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/hologo/"
                   "source/generic/hologo/"
                   "tex/generic/hologo/")
             (base32
              "0n62zwz93ab6vfb0hd2h0ncj9gwavg01i3djj7wyr7gyj20xb34x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hologo")
    (synopsis "Collection of logos with bookmark support")
    (description
     "The package defines a single command @code{\\hologo}, whose argument is
the usual case-confused ASCII version of the logo.  The command is
bookmark-enabled, so that every logo becomes available in bookmarks without
further work.")
    (license license:lppl1.3c)))

(define-public texlive-hopatch
  (package
    (name "texlive-hopatch")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hopatch/" "source/latex/hopatch/"
                   "tex/latex/hopatch/")
             (base32
              "03hafzf0kpjhn5x392bziwyx0vf6fwcsy0xrn0c0jzn5cq5nqhap")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hopatch")
    (synopsis "Load patches for packages")
    (description
     "Hopatch provides a command with which the user may register of patch code
for a particular package.  Hopatch will apply the patch immediately, if the
relevant package has already been loaded; otherwise it will store the patch
until the package appears.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-base
  (package
    (name "texlive-hyphen-base")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "/tex/generic/config/language.dat"
                   "/tex/generic/config/language.dat.lua"
                   "/tex/generic/config/language.def"
                   "/tex/generic/config/language.us"
                   "/tex/generic/config/language.us.def"
                   "/tex/generic/config/language.us.lua"
                   "/tex/generic/hyphen/dumyhyph.tex"
                   "/tex/generic/hyphen/hyphen.tex"
                   "/tex/generic/hyphen/hypht1.tex"
                   "/tex/generic/hyphen/zerohyph.tex")
             (base32
              "0p3p12pm9gyrhr1zzvzazfmybhavqd9hdi77ygm3ygq8km7raq3h")))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://tug.org/texlive/")
    (synopsis "Core hyphenation support files")
    (description "This package includes Knuth's original @file{hyphen.tex},
@file{zerohyph.tex} to disable hyphenation, @file{language.us} which starts
the autogenerated files @file{language.dat} and @file{language.def} (and
default versions of those), etc.")
    (license license:knuth)))

(define-public texlive-hyphenex
  (package
    (name "texlive-hyphenex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "source/generic/hyphenex/"
                   "tex/generic/hyphenex/")
             (base32
              "1v1p93i56xgp01zly30bkfgb9py8nav1r620dbgz1q7438zbhzpp")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hyphenex")
    (synopsis "US English hyphenation exceptions file")
    (description
     "Exceptions for American English hyphenation patterns are occasionally
published in the TeX User Group journal TUGboat.  This bundle provides
alternative Perl and Bourne shell scripts to convert the source of such an
article into an exceptions file, together with a recent copy of the article
and machine-readable files.")
    (license license:public-domain)))

(define-public texlive-index
  (package
    (name "texlive-index")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/index/" "doc/latex/index/"
                   "makeindex/index/" "source/latex/index/"
                   "tex/latex/index/")
             (base32
              "0f1infc8fcpw16crciampy4cqqhl4hzypyfacbwsk4cnl0fyivns")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/index")
    (synopsis "Extended index for LaTeX including multiple indexes")
    (description
     "This is a reimplementation of LaTeX's indexing macros to provide better
support for indexing.  For example, it supports multiple indexes in a single
document and provides a more robust @code{\\index} command.")
    (license license:lppl1.2+)))        ;from "index.dtx"

(define-public texlive-dvipdfmx
  (package
    (name "texlive-dvipdfmx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/dvipdfm/"
                   "doc/dvipdfmx/"
                   "doc/man/man1/dvipdfm.1"
                   "doc/man/man1/dvipdfm.man1.pdf"
                   "doc/man/man1/dvipdfmx.1"
                   "doc/man/man1/dvipdfmx.man1.pdf"
                   "doc/man/man1/dvipdft.1"
                   "doc/man/man1/dvipdft.man1.pdf"
                   "doc/man/man1/ebb.1"
                   "doc/man/man1/ebb.man1.pdf"
                   "doc/man/man1/extractbb.1"
                   "doc/man/man1/extractbb.man1.pdf"
                   "doc/man/man1/xdvipdfmx.1"
                   "doc/man/man1/xdvipdfmx.man1.pdf"
                   "dvipdfmx/"
                   "fonts/cmap/dvipdfmx/"
                   "fonts/map/dvipdfmx/")
             (base32
              "0p6mlpymrhsalisfda0gbqg6b941fd164kcw0nc51pzc98aws1xz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-map-file
            ;; This map file is supposed to be generated in a profile hook.
            (lambda _
              (delete-file "fonts/map/dvipdfmx/updmap/kanjix.map"))))))
    (propagated-inputs (list texlive-glyphlist))
    (home-page "https://ctan.org/pkg/dvipdfmx")
    (synopsis "Extended version of dvipdfm")
    (description
     "Dvipdfmx (formerly dvipdfm-cjk) is a development of dvipdfm created to
support multi-byte character encodings and large character sets for East Asian
languages.  Dvipdfmx, if called with the name dvipdfm, operates in a dvipdfm
compatibility mode, so that users of the both packages need only keep one
executable.  A secondary design goal is to support as many PDF features as
does pdfTeX.")
    (license license:gpl3+)))

(define-public texlive-dvips
  (package
    (name "texlive-dvips")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/dvips/"
                   "doc/info/dvips.info"
                   "doc/man/man1/afm2tfm.1"
                   "doc/man/man1/afm2tfm.man1.pdf"
                   "doc/man/man1/dvips.1"
                   "doc/man/man1/dvips.man1.pdf"
                   "dvips/base/"
                   "dvips/config/"
                   "fonts/enc/dvips/base/"
                   "tex/generic/dvips/")
             (base32
              "0x11wx9p16z4nxhlbfqlgi5svnr96j1hnvdl9fpv1sr3n1j8m79g")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/dvips")
    (synopsis "DVI to PostScript drivers")
    (description
     "This package provides files needed for converting DVI files to
PostScript.")
    (license license:lppl)))

(define-public texlive-tex-ini-files
  (package
    (name "texlive-tex-ini-files")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/tex-ini-files/" "tex/generic/tex-ini-files/")
             (base32
              "0a18k27fz1vjha5blwskxpnd715k08hmfm7d1yc2f7adaf0rwl3m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://www.ctan.org/pkg/tex-ini-files")
    (synopsis "Files for creating TeX formats")
    (description
     "This bundle provides a collection of model @file{.ini} files for
creating TeX formats.  These files are commonly used to introduced
distribution-dependent variations in formats.  They are also used to allow
existing format source files to be used with newer engines, for example to
adapt the plain e-TeX source file to work with XeTeX and LuaTeX.")
    (license license:public-domain)))

(define-deprecated-package texlive-generic-tex-ini-files texlive-tex-ini-files)

(define-public texlive-metafont
  (package
    (name "texlive-metafont")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/inimf.1"
                   "doc/man/man1/inimf.man1.pdf"
                   "doc/man/man1/mf-nowin.1"
                   "doc/man/man1/mf-nowin.man1.pdf"
                   "doc/man/man1/mf.1"
                   "doc/man/man1/mf.man1.pdf"
                   "metafont/base/"
                   "metafont/config/"
                   "metafont/misc/")
             (base32
              "18pp6vcg1cv38yi39q9rvkv6w11mnxxd79fvf1yy01743jn7ngjh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'generate-mf.base
            ;; Even though the file "mf.base" does not appear in tlpdb, it
            ;; must be generated and provided in metafont package.
            (lambda _
              (let* ((cwd (getcwd))
                     (mf (string-append cwd "/metafont"))
                     (modes #$(this-package-native-input "texlive-modes")))
                (setenv "MFINPUTS"
                        (string-append
                         modes "/share/texmf-dist/fonts/source/public/modes:"
                         mf "/base:"
                         mf "/misc:"
                         mf "/roex:"
                         mf "/feynmf:"
                         mf "/mfpic:"
                         mf "/config")))
              ;; "build" directory was not created during `build' phases since
              ;; there is no ".ins" nor ".dtx" file to process.
              (mkdir-p "build")
              (with-directory-excursion "build"
                (invoke "inimf" "mf.mf")
                (install-file "mf.base"
                              (string-append #$output
                                             "/share/texmf-dist/web2c"))))))))
    (native-inputs
     (list texlive-bin texlive-modes))
    (home-page "https://ctan.org/pkg/metafont")
    (synopsis "Metafont base files")
    (description "This package provides the Metafont base files needed to
build fonts using the Metafont system.")
    (license license:knuth)))

(define-deprecated-package texlive-metafont-base texlive-metafont)

(define-public texlive-mfirstuc
  (package
    (name "texlive-mfirstuc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mfirstuc/"
                   "scripts/mfirstuc/"
                   "source/latex/mfirstuc/"
                   "tex/latex/mfirstuc/")
             (base32
              "1fvdnfybfi7nych97i117s6wqf0w8drgzpf3qzfns9qxjxm0bv3l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mfirstuc")
    (synopsis "Uppercase the first letter of a word")
    (description
     "The package provides commands @code{\\makefirstuc} that uppercases the
first letter in its argument (with a check for a semantic markup command at
the start of the argument), and @code{\\xmakefirstuc} which expands the
argument before uppercasing.  It also provides
@code{\\capitalisewords@{phrase@}} which applies @code{\\makefirstuc} to each
word in the phrase, where the words are separated by regular
spaces.  (Exceptions can be made for words that shouldn't be converted.)")
    (license license:lppl1.3+)))

(define-public texlive-mfnfss
  (package
    (name "texlive-mfnfss")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mfnfss/" "source/latex/mfnfss/"
                   "tex/latex/mfnfss/")
             (base32
              "1775vg12sk38givqq2zjapx4nxlyl95rf596r2inf8mv5phsi704")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mfnfss")
    (synopsis "Packages to typeset old german and Pandora fonts in LaTeX")
    (description
     "This bundle contains two packages: @code{oldgerm}, a package to typeset
with old german fonts designed by Yannis Haralambous, and @code{pandora},
a package to typeset with Pandora fonts designed by Neena Billawala.  Note
that support for the Pandora fonts is also available via the
@code{pandora-latex} package.")
    (license license:lppl)))

(define-public texlive-modes
  (package
    (name "texlive-modes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/modes/"
                   "fonts/source/public/modes/")
             (base32
              "1vz3ygpixswnk7hr3qfn3nffw460cp5wjq09q5ac83ddw3nya1ca")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/modes")
    (synopsis "Collection of Metafont @code{mode_def}'s")
    (description
     "The modes file collects all known Metafont modes for printing or display
devices, of whatever printing technology.  Special provision is made for
write-white printers, and a landscape mode is available, for making suitable
fonts for printers with pixels whose aspect is non-square.  The file also
provides definitions that make @code{\\specials} identifying the mode in
Metafont's GF output, and put coding information and other Xerox-world
information in the TFM file.")
    (license license:public-domain)))

(define-public texlive-mptopdf
  (package
    (name "texlive-mptopdf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             ;; The first location prevents from downloading all ConTeXt
             ;; "doc/context/scripts/mkii" directory.
             (list "doc/context/scripts/mkii/mptopdf.man"
                   "doc/man/man1/mptopdf.1"
                   "doc/man/man1/mptopdf.man1.pdf"
                   "scripts/context/perl/"
                   "tex/context/base/mkii/"
                   "tex/generic/context/mptopdf/")
             (base32
              "08z0hxq0645lf8jyl0wq3kwn1f7xsvj736sqgfin9ldd89zc2ch3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:link-scripts #~(list "mptopdf.pl")
           #:create-formats #~(list "mptopdf")))
    (propagated-inputs (list texlive-plain))
    (home-page "https://ctan.org/pkg/mptopdf")
    (synopsis "mpost to PDF, native MetaPost graphics inclusion")
    (description
     "The @code{mptopdf} script does standalone conversion from mpost to PDF,
using the @file{supp-*} and @file{syst-*} files.  They also allow native
MetaPost graphics inclusion in LaTeX (via pdftex.def) and ConTeXt.  They can
be used independently of the rest of ConTeXt.")
    ;; Use the same licensing as ConTeXt.
    (license (list license:lppl1.3c+
                   license:gpl2+
                   license:cc-by-sa4.0))))

(define-public texlive-fontinst
  (package
    (name "texlive-fontinst")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/fontinst/"
                   "doc/man/man1/fontinst.1"
                   "doc/man/man1/fontinst.man1.pdf"
                   "scripts/texlive-extra/fontinst.sh"
                   "source/fontinst/base/"
                   "tex/fontinst/base/"
                   "tex/fontinst/latinetx/"
                   "tex/fontinst/latinmtx/"
                   "tex/fontinst/mathetx/"
                   "tex/fontinst/mathmtx/"
                   "tex/fontinst/misc/"
                   "tex/fontinst/smbletx/"
                   "tex/fontinst/smblmtx/"
                   "tex/latex/fontinst/")
             (base32
              "1qqggn44w07a0aslsf3jdygwv1gfs90qahkan0gnibxsz7i44kqm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "fontinst.sh")))
    (home-page "https://ctan.org/pkg/fontinst")
    (synopsis "Tools for converting and installing fonts for TeX and LaTeX")
    (description
     "This package provides TeX macros for converting Adobe Font Metric files
to TeX metric and virtual font format.  Fontinst helps mainly with the number
crunching and shovelling parts of font installation.  This means in practice
that it creates a number of files which give the TeX metrics (and related
information) for a font family that TeX needs to do any typesetting in these
fonts.")
    (license license:lppl1.1+)))

(define-deprecated-package texlive-tex-fontinst-base texlive-fontinst)

(define-public texlive-fontname
  (package
    (name "texlive-fontname")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/fontname/" "doc/info/fontname.info"
                   "fonts/map/fontname/")
             (base32
              "014kiwbqz77yn8w58cb6fzqj0vlfmgyq09mxdj15ipjfgxjyvcbj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fontname")
    (synopsis "Scheme for naming fonts in TeX")
    (description
     "This is Fontname, a naming scheme for (the base part of) external TeX
font filenames.  This makes at most eight-character names from (almost)
arbitrarily complex font names, thus helping portability of TeX documents.")
    (license license:gpl3+)))

(define-public texlive-cbfonts          ;71 MiB of greek fonts
  (package
    (name "texlive-cbfonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cbfonts/"
                   "fonts/enc/dvips/cbfonts/"
                   "fonts/map/dvips/cbfonts/"
                   "fonts/source/public/cbfonts/"
                   "fonts/tfm/public/cbfonts/"
                   "fonts/type1/public/cbfonts/")
             (base32
              "0l0dpgvngah227snzk6j5hf3kxs5cz5jvlpkv8pbapw6jx084np6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; FIXME: Font metrics cannot be generated due to "bad pos"
               ;; errors.
               (delete 'generate-font-metrics))))
    (native-inputs (list texlive-cm texlive-metafont))
    (propagated-inputs (list texlive-cbfonts-fd))
    (home-page "https://ctan.org/pkg/cbgreek-complete")
    (synopsis "Complete set of Greek fonts")
    (description
     "This bundle presents the whole of Beccari's original Greek font set,
which use the @i{Lispiakos} font shape derived from the shape of the fonts
used in printers' shops in Lispia.  The fonts are available both as Metafont
source and in Adobe Type 1 format, and at the same wide set of design sizes as
are such font sets as the EC fonts.")
    (license license:lppl1.3c+)))

(define-public texlive-cbfonts-fd
  (package
    (name "texlive-cbfonts-fd")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cbfonts-fd/"
                   "source/fonts/cbfonts-fd/"
                   "tex/latex/cbfonts-fd/")
             (base32
              "1r2kmnccvrq181ac7gyff9y3wn7dydx50jy8f9n6qhnb824pdn78")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cbfonts-fd")
    (synopsis "LaTeX font description files for the CB Greek fonts")
    (description
     "The package provides font description files for all the many shapes
available from the @code{cbfonts} collection.  The files provide the means
whereby the @acronym{NFSS, New Font Selection Scheme} knows which fonts
a LaTeX user is requesting.

Tip: installing @code{texlive-cbfonts} will automatically propagate this one.")
    (license license:lppl1.3c+)))

(define-public texlive-ccicons
  (package
    (name "texlive-ccicons")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/ccicons/"
                   "doc/latex/ccicons/"
                   "fonts/enc/dvips/ccicons/"
                   "fonts/map/dvips/ccicons/"
                   "fonts/opentype/public/ccicons/"
                   "fonts/tfm/public/ccicons/"
                   "fonts/type1/public/ccicons/"
                   "source/fonts/ccicons/"
                   "source/latex/ccicons/"
                   "tex/latex/ccicons/")
             (base32
              "0lyxbjpkxny6hl7pmnpka0gmgx7qv66pibvwcfs9dvwhdcvwjr4x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ccicons")
    (synopsis "LaTeX support for Creative Commons icons")
    (description
     "The package provides the means to typeset Creative Commons icons, in
documents licensed under CC licences.  A font (in Adobe Type 1 format) and
LaTeX support macros are provided.")
    (license (list license:lppl1.3c license:silofl1.1))))

(define-public texlive-cite
  (package
    (name "texlive-cite")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/cite/" "tex/latex/cite/")
             (base32
              "0b1amznayxj80dmqbzcysmj7q8aksbyz98k6djsqi0mhwp1cd0fd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cite")
    (synopsis "Improved citation handling in LaTeX")
    (description
     "The package supports compressed, sorted lists of numerical citations,
and also deals with various punctuation and other issues of representation,
including comprehensive management of break points.  The package is compatible
with both @code{hyperref} and @code{backref}.  The package is (unsurprisingly)
part of the cite bundle of the author's citation-related packages.")
    (license (license:fsf-free "/share/texmf-dist/doc/latex/cite/README"))))

(define-public texlive-cm
  (package
    (name "texlive-cm")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cm/" "fonts/map/dvips/cm/"
                   "fonts/pk/ljfour/public/cm/dpi600/"
                   "fonts/source/public/cm/"
                   "fonts/tfm/public/cm/")
             (base32
              "10adgjc3lkj2z50jp1f9n83bdrx8bqxd76rl605d0d5pb435k97m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'generate-font-metrics 'generate-pk
            (lambda _
              (let* ((cwd (getcwd))
                     (pkdir
                      (string-append cwd "/fonts/pk/ljfour/public/cm/dpi600"))
                     (build-dir (string-append cwd "/build")))
                (with-directory-excursion "fonts/source/public/cm/"
                  (mkdir-p pkdir)
                  (for-each
                   (lambda (font)
                     (let ((font-name (basename font ".mf")))
                       (invoke "gftopk"
                               (string-append build-dir "/" font-name ".600gf")
                               (string-append pkdir "/" font-name ".pk"))))
                   (find-files "." "cm(.*[0-9]+.*|inch)\\.mf$")))))))))
    (native-inputs (list texlive-metafont))
    (home-page "https://ctan.org/pkg/cm")
    (synopsis "Computer Modern fonts for TeX")
    (description
     "This package provides the Computer Modern fonts by Donald Knuth.  The
Computer Modern font family is a large collection of text, display, and
mathematical fonts in a range of styles, based on Monotype Modern 8A.")
    (license license:knuth)))

(define-deprecated-package texlive-fonts-cm texlive-cm)

(define-public texlive-cmextra
  (package
    (name "texlive-cmextra")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/source/public/cmextra/"
                   "fonts/tfm/public/cmextra/")
             (base32
              "00my52lh3cn8bn211fpn7373b7khaylx4fgkrd2m72xgaymil308")))
    (build-system texlive-build-system)
    (native-inputs (list texlive-metafont))
    (home-page "https://ctan.org/pkg/knuth-local")
    (synopsis "Knuth's local information")
    (description
     "This package provides a collection of experimental programs and
developments based on, or complementary to, the matter in his distribution
directories.")
    (license license:public-domain)))

(define-public texlive-cm-lgc
  (package
    (name "texlive-cm-lgc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cm-lgc/"
                   "fonts/afm/public/cm-lgc/"
                   "fonts/enc/dvips/cm-lgc/"
                   "fonts/map/dvips/cm-lgc/"
                   "fonts/ofm/public/cm-lgc/"
                   "fonts/ovf/public/cm-lgc/"
                   "fonts/tfm/public/cm-lgc/"
                   "fonts/type1/public/cm-lgc/"
                   "fonts/vf/public/cm-lgc/"
                   "tex/latex/cm-lgc/")
             (base32
              "1nj3gp0kpzlqkm22lj2ym9c95xw323xc5z91gsyv8xs716868gp2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cm-lgc")
    (synopsis "Type 1 CM-based fonts for Latin, Greek and Cyrillic")
    (description
     "The fonts are converted from METAFONT sources of the Computer Modern
font families, using @command{textrace}.  Supported encodings are: T1 (Latin),
T2A (Cyrillic), LGR (Greek) and TS1.  The package also includes Unicode
virtual fonts for use with Omega.  The font set is not a replacement for any
of the other Computer Modern-based font sets (for example, cm-super for Latin
and Cyrillic, or cbgreek for Greek), since it is available at a single size
only; it offers a compact set for general working.  The fonts themselves are
encoded to external standards, and virtual fonts are provided for use with
TeX.")
    (license license:gpl2+)))

(define-public texlive-cm-super
  (package
    (name "texlive-cm-super")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cm-super/"
                   "dvips/cm-super/"
                   "fonts/afm/public/cm-super/"
                   "fonts/enc/dvips/cm-super/"
                   "fonts/map/dvips/cm-super/"
                   "fonts/map/vtex/cm-super/"
                   "fonts/type1/public/cm-super/"
                   "tex/latex/cm-super/")
             (base32
              "1k3afl0x0bqbr5mnawbnp7rr2126dwn0vwnxzibm9ggvzqilnkm6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cm-super")
    (synopsis "Computer Modern Super family of fonts")
    (description
     "The CM-Super family provides Adobe Type 1 fonts that replace the
T1/TS1-encoded Computer Modern (EC/TC), T1/TS1-encoded Concrete,
T1/TS1-encoded CM bright and LH Cyrillic fonts (thus supporting all European
languages except Greek), and bringing many ameliorations in typesetting
quality.  The fonts exhibit the same metrics as the METAFONT-encoded
originals.")
    ;; With font exception
    (license license:gpl2+)))

(define-deprecated-package texlive-fonts-cm-super texlive-cm-super)

(define-public texlive-courier
  (package
    (name "texlive-courier")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/courier/"
                   "fonts/afm/adobe/courier/"
                   "fonts/afm/urw/courier/"
                   "fonts/map/dvips/courier/"
                   "fonts/tfm/adobe/courier/"
                   "fonts/tfm/urw35vf/courier/"
                   "fonts/type1/adobe/courier/"
                   "fonts/type1/urw/courier/"
                   "fonts/vf/adobe/courier/"
                   "fonts/vf/urw35vf/courier/"
                   "tex/latex/courier/")
             (base32
              "08g6lm12b0k6333pxcaqdf67v87fz5mrqp3jgal8qhrls5ym8q6r")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Courier font")
    (description
     "This package provides a drop-in replacement for the Courier font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-tex-gyre
  (package
    (name "texlive-tex-gyre")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/tex-gyre/"
                   "fonts/afm/public/tex-gyre/"
                   "fonts/enc/dvips/tex-gyre/"
                   "fonts/map/dvips/tex-gyre/"
                   "fonts/opentype/public/tex-gyre/"
                   "fonts/tfm/public/tex-gyre/"
                   "fonts/type1/public/tex-gyre/"
                   "source/fonts/tex-gyre/"
                   "tex/latex/tex-gyre/")
             (base32
              "0bn8g6rav0v47zbf1gjwp64x0l5340wb5iiiw4kdg69qingkj5lq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tex-gyre")
    (synopsis "TeX fonts extending URW fonts")
    (description
     "The TeX-GYRE bundle consist of multiple font families:
@itemize @bullet
@item Adventor, based on the URW Gothic L family of fonts;
@item Bonum, based on the URW Bookman L family;
@item Chorus, based on URW Chancery L Medium Italic;
@item Cursor, based on URW Nimbus Mono L;
@item Heros, based on URW Nimbus Sans L;
@item Pagella, based on URW Palladio L;
@item Schola, based on the URW Century Schoolbook L family;
@item Termes, based on the URW Nimbus Roman No9 L family of fonts.
@end itemize

The constituent standard faces of each family have been greatly extended
(though Chorus omits Greek support and has no small-caps family).  Each
family is available in Adobe Type 1 and Open Type formats, and LaTeX
support (for use with a variety of encodings) is provided.")
    ;; The GUST font license (GFL) is legally identical to the LaTeX Project
    ;; Public License (LPPL), version 1.3c or later, but comes with an
    ;; additional but not legally binding clause.
    (license license:lppl1.3c+)))

(define-public texlive-tex-gyre-math
  (package
    (name "texlive-tex-gyre-math")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/tex-gyre-math/"
                   "fonts/opentype/public/tex-gyre-math/"
                   "source/fonts/tex-gyre-math/")
             (base32
              "1k5fx03bg702636hh7hv4kzzxhbbic26rp5g4lq2bgajd5dgc5xy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tex-gyre-math")
    (synopsis "Maths fonts to match tex-gyre text fonts")
    (description
     "TeX-Gyre-Math is a collection of maths fonts to match the text fonts of
the TeX-Gyre collection.  The collection is available in OpenType format,
only; fonts conform to the developing standards for OpenType maths fonts.
TeX-Gyre-Math-Bonum (to match TeX-Gyre-Bonum), TeX-Gyre-Math-Pagella (to match
TeX-Gyre-Pagella), TeX-Gyre-Math-Schola (to match TeX-Gyre-Schola) and
TeX-Gyre-Math-Termes (to match TeX-Gyre-Termes) fonts are provided.")
    (license license:gfl1.0)))

(define-public texlive-crop
  (package
    (name "texlive-crop")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/crop/" "source/latex/crop/"
                   "tex/latex/crop/")
             (base32
              "1m0dg69bhbvqrq9d2yl6ip36w1bf5cibp386jj2wbywisa2hn3qf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/crop")
    (synopsis "Support for cropmarks")
    (description
     "This package provides a package providing corner marks for camera
alignment as well as for trimming paper stacks, and additional page
information on every page if required.  Most macros are easily adaptable to
personal preferences.  An option is provided for selectively suppressing
graphics or text, which may be useful for printing just colour graphics on
a colour laser printer and the rest on a cheap mono laser printer.  A page
info line contains the time and a new cropmarks index and is printed at the
top of the page.  A configuration command is provided for the info line font.
Options for better collaboration with dvips, pdfTeX and vtex are provided.")
    (license license:lppl)))

(define-public texlive-ctablestack
  (package
    (name "texlive-ctablestack")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/ctablestack/"
                   "source/luatex/ctablestack/"
                   "tex/luatex/ctablestack/")
             (base32
              "13l779436aj3hlchwvhkpiikbyfa2j4swzfrwqkjh9l8bc2cwg7n")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ctablestack")
    (synopsis "Catcode table stable support")
    (description
     "This package provides a method for defining category code table stacks
in LuaTeX.  It is required by the @code{luatexbase} package which uses
@code{ctablestack} to provide a back-compatibility form of this concept.")
    (license license:lppl1.3+)))

(define-public texlive-lm
  (package
    (name "texlive-lm")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/lm/"
                   "fonts/afm/public/lm/"
                   "fonts/enc/dvips/lm/"
                   "fonts/map/dvipdfm/lm/"
                   "fonts/map/dvips/lm/"
                   "fonts/opentype/public/lm/"
                   "fonts/tfm/public/lm/"
                   "fonts/type1/public/lm/"
                   "tex/latex/lm/")
             (base32
              "1zgp0pc30n8jqr7kiv6j77i9i8dzzyh8zv72n24n74lb28k0sfmr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/lm")
    (synopsis "Latin Modern family of fonts")
    (description "The Latin Modern fonts are derived from the famous Computer
Modern fonts designed by Donald E. Knuth and described in Volume E of his
Computers & Typesetting series.")
    ;; The GUST font license (GFL) is legally identical to the LaTeX Project
    ;; Public License (LPPL), version 1.3c or later, but comes with an
    ;; additional but not legally binding clause.
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-fonts-lm texlive-lm)

(define-public texlive-lm-math
  (package
    (name "texlive-lm-math")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/lm-math/"
                   "fonts/opentype/public/lm-math/")
             (base32
              "0gqdk8x3r1iz4n8j6r3pcqbwalxvkihayvmjfq4iv6hwb0pvys8z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lm-math")
    (synopsis "OpenType maths fonts for Latin Modern")
    (description
     "Latin Modern Math is a maths companion for the Latin Modern family of
fonts, in OpenType format.  For use with LuaLaTeX or XeLaTeX, support is
available from the @code{unicode-math} package.")
    (license license:gfl1.0)))

(define-public texlive-lwarp
  (package
    (name "texlive-lwarp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/lwarp/"
                   "scripts/lwarp/"
                   "source/latex/lwarp/"
                   "tex/latex/lwarp/")
             (base32
              "0r1hxihhd7yzhw4s3v9yn1wl36q6fs2cwbdc09z9c1mdz3pywzp6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "lwarpmk.lua")))
    (home-page "https://ctan.org/pkg/lwarp")
    (synopsis "Converts LaTeX to HTML")
    (description
     "This package converts LaTeX to HTML by using LaTeX to process the user's
document and generate HTML tags.  External utility programs are only used for
the final conversion of text and images.  Math may be represented by SVG files
or MathJax.  Hundreds of LaTeX packages are supported, and their load order is
automatically verified.  Documents may be produced by LaTeX, LuaLaTeX,
XeLaTeX, and by several CJK engines, classes, and packages.  A texlua script
automates compilation, index, glossary, and batch image processing, and also
supports latexmk.  Configuration is semi-automatic at the first manual
compile.  Support files are self-generated.  Print and HTML versions of each
document may coexist.  Assistance is provided for HTML import into EPUB
conversion software and word processors.")
    (license license:lppl1.3+)))

(define-public texlive-knuth-lib
  (package
    (name "texlive-knuth-lib")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/source/public/knuth-lib/"
                   "fonts/tfm/public/knuth-lib/"
                   "tex/generic/knuth-lib/"
                   "tex/plain/knuth-lib/")
             (base32
              "0dl8z340n6m6xn7wari4hir0syxqi0kl2fhnf0bvnmkqhqwyzpca")))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/knuth-lib")
    (synopsis "Small library of METAFONT sources")
    (description
     "This is a collection of core TeX and METAFONT macro files from Donald
Knuth, including the plain format, plain base, and the MF logo fonts.")
    (license license:knuth)))

(define-deprecated-package texlive-fonts-knuth-lib texlive-knuth-lib)

(define-public texlive-knuth-local
  (package
    (name "texlive-knuth-local")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/source/public/knuth-local/"
                   "fonts/tfm/public/knuth-local/"
                   "mft/knuth-local/" "tex/plain/knuth-local/")
             (base32
              "02cf32f57fr6bngiv9xiw8bh4sq53p9br034ap74s80h3bgcmn1f")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/knuth-local")
    (native-inputs
     (list texlive-knuth-lib texlive-metafont))
    (synopsis "Knuth's local information")
    (description
     "This package provides a collection of experimental programs and
developments based on, or complementary to, the matter in his distribution
directories.")
    (license license:public-domain)))

(define-public texlive-latex-fonts
  (package
    (name "texlive-latex-fonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/latex-fonts/"
                   "fonts/source/public/latex-fonts/"
                   "fonts/tfm/public/latex-fonts/")
             (base32
              "1bzqzzhs15w7dqz90hfjnaffjqh24q14w2h1h8vnxzvrlsyv21vq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/latex-fonts")
    (synopsis "Collection of fonts used in LaTeX distributions")
    (description
     "This is a collection of fonts for use with standard LaTeX packages and
classes. It includes invisible fonts (for use with the slides class), line and
circle fonts (for use in the @code{picture} environment) and LaTeX symbol
fonts.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-fonts-latex texlive-latex-fonts)

(define-public texlive-mflogo
  (package
    (name "texlive-mflogo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mflogo/"
                   "fonts/source/public/mflogo/"
                   "fonts/tfm/public/mflogo/"
                   "source/latex/mflogo/"
                   "tex/latex/mflogo/")
             (base32
              "1r53qlrcqfwc0dfr7ji1nxnqrj6n0qrlg1rl7fjlw6ap3q9y434k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-knuth-lib texlive-metafont))
    (home-page "https://ctan.org/pkg/mflogo")
    (synopsis "LaTeX support for Metafont logo fonts")
    (description
     "This package provides LaTeX and font definition files to access the
Knuthian mflogo fonts described in The Metafontbook and to typeset Metafont
logos in LaTeX documents.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-mflogo texlive-mflogo)

(define-public texlive-mflogo-font
  (package
    (name "texlive-mflogo-font")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/mflogo-font/"
                   "fonts/afm/hoekwater/mflogo-font/"
                   "fonts/map/dvips/mflogo-font/"
                   "fonts/type1/hoekwater/mflogo-font/")
             (base32
              "094mknjv8ki2pvj1zin0f1z4f1w12g0cfqjiqcsawjsry4yfrmbg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mflogo-font")
    (synopsis "METAFONT logo font")
    (description
     "These fonts were created in Metafont by Knuth, for his own publications.
At some stage, the letters P and S were added, so that the MetaPost logo could
also be expressed.  The fonts were originally issued (of course) as Metafont
source; they have since been autotraced and reissued in Adobe Type 1 format by
Taco Hoekwater.")
    (license license:knuth)))

(define-deprecated-package texlive-fonts-mflogo-font texlive-mflogo-font)

(define-public texlive-mfware
  (package
    (name "texlive-mfware")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/gftodvi.1"
                   "doc/man/man1/gftodvi.man1.pdf"
                   "doc/man/man1/gftopk.1"
                   "doc/man/man1/gftopk.man1.pdf"
                   "doc/man/man1/gftype.1"
                   "doc/man/man1/gftype.man1.pdf"
                   "doc/man/man1/mft.1"
                   "doc/man/man1/mft.man1.pdf"
                   "doc/man/man1/pktogf.1"
                   "doc/man/man1/pktogf.man1.pdf"
                   "doc/man/man1/pktype.1"
                   "doc/man/man1/pktype.man1.pdf"
                   "mft/base/")
             (base32
              "0l0xy2zl7yzb14wbzsg4sz5bdj22ggqlsw54d0yrm430wlr1s6sd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mfware")
    (synopsis "Supporting tools for use with Metafont")
    (description
     "This package provides a collection of programs (as web source) for
processing the output of Metafont.")
    (license license:public-domain)))

(define-public texlive-amiri
  (package
    (name "texlive-amiri")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/amiri/"
                   "fonts/truetype/public/amiri/")
             (base32
              "1d6yrh34fka9371a3vq72df593prik6s7z1i6myd0nix0c9jihp6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/amiri")
    (synopsis "Classical Arabic typeface, Naskh style")
    (description
     "Amiri is a classical Arabic typeface in Naskh style for typesetting books
and other running text.  It is a revival of the beautiful typeface pioneered
in the early 20th century by Bulaq Press in Cairo, also known as Amiria Press,
after which the font is named.  The project aims at the revival of the
aesthetics and traditions of Arabic typesetting, and adapting it to the era of
digital typesetting, in a publicly available form.")
    (license license:silofl1.1)))

(define-public texlive-amsfonts
  (package
    (name "texlive-amsfonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/amsfonts/"
                   "fonts/afm/public/amsfonts/cm/"
                   "fonts/afm/public/amsfonts/cmextra/"
                   "fonts/afm/public/amsfonts/cyrillic/"
                   "fonts/afm/public/amsfonts/euler/"
                   "fonts/afm/public/amsfonts/latxfont/"
                   "fonts/afm/public/amsfonts/symbols/"
                   "fonts/map/dvips/amsfonts/"
                   "fonts/source/public/amsfonts/cmextra/"
                   "fonts/source/public/amsfonts/cyrillic/"
                   "fonts/source/public/amsfonts/dummy/"
                   "fonts/source/public/amsfonts/symbols/"
                   "fonts/tfm/public/amsfonts/cmextra/"
                   "fonts/tfm/public/amsfonts/cyrillic/"
                   "fonts/tfm/public/amsfonts/dummy/"
                   "fonts/tfm/public/amsfonts/euler/"
                   "fonts/tfm/public/amsfonts/symbols/"
                   "fonts/type1/public/amsfonts/cm/"
                   "fonts/type1/public/amsfonts/cmextra/"
                   "fonts/type1/public/amsfonts/cyrillic/"
                   "fonts/type1/public/amsfonts/euler/"
                   "fonts/type1/public/amsfonts/latxfont/"
                   "fonts/type1/public/amsfonts/symbols/"
                   "source/latex/amsfonts/"
                   "tex/latex/amsfonts/"
                   "tex/plain/amsfonts/")
             (base32
              "0phhzcxapa5607pk37agr981rg90zw2p4rqv7sk7i19byr867a1b")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/amsfonts")
    (synopsis "TeX fonts from the American Mathematical Society")
    (description
     "This package provides an extended set of fonts for use in mathematics,
including: extra mathematical symbols; blackboard bold letters (uppercase
only); fraktur letters; subscript sizes of bold math italic and bold Greek
letters; subscript sizes of large symbols such as sum and product; added sizes
of the Computer Modern small caps font; cyrillic fonts (from the University of
Washington); Euler mathematical fonts.  All fonts are provided as Adobe Type
1 files, and all except the Euler fonts are provided as METAFONT source.  The
distribution also includes the canonical Type 1 versions of the Computer
Modern family of fonts.  The Euler fonts are supported by separate packages;
details can be found in the documentation.")
    (license license:silofl1.1)))

(define-deprecated-package texlive-fonts-amsfonts texlive-amsfonts)
(define-deprecated-package texlive-latex-amsfonts texlive-amsfonts)

(define-public texlive-mkpattern
  (package
    (name "texlive-mkpattern")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/plain/mkpattern/" "tex/plain/mkpattern/")
             (base32
              "0sxnkbcc802jl3fj56x9hvg978bpv15lhrwj0aykb4syq29l47ga")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mkpattern")
    (synopsis "Utility for making hyphenation patterns")
    (description
     "Mkpattern is a general purpose program for the generation of hyphenation
patterns, with definition of letter sets and template-like constructions.  It
also provides an easy way to handle different input and output encodings, and
features generation of clean UTF-8 patterns.")
    (license license:lppl)))

;; This provides etex.src which is needed to build various formats, including
;; luatex.fmt and pdflatex.fmt
(define-public texlive-etex
  (package
    (name "texlive-etex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/etex/base/"
                   "doc/man/man1/etex.1"
                   "doc/man/man1/etex.man1.pdf"
                   "fonts/source/public/etex/"
                   "fonts/tfm/public/etex/"
                   "tex/plain/etex/")
             (base32
              "1q48645qgjcl2jmpd0x0ip5wwdan54y9vx06zyvpp51wia30sacy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://www.ctan.org/pkg/etex")
    (synopsis "Extended version of TeX")
    (description
     "This package provides an extended version of TeX (which is capable of
running as if it were TeX unmodified).  E-TeX has been specified by the LaTeX
team as the engine for the development of LaTeX2e; as a result, LaTeX
programmers may assume e-TeX functionality.  The pdftex engine directly
incorporates the e-TeX extensions.")
    (license license:knuth)))

(define-public texlive-etex-pkg
  (package
    (name "texlive-etex-pkg")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/etex-pkg/" "tex/latex/etex-pkg/")
             (base32
              "10bvvn3s3lmzjscnb2qxkj1ba9qxx0q1w2spcsjpwf20dvym19py")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/etex-pkg")
    (synopsis "E-TeX support package")
    (description
     "The package provides support for LaTeX documents to use many of the
extensions offered by e-TeX; in particular, it modifies LaTeX's register
allocation macros to make use of the extended register range.  The
@code{etextools} package provides macros that make more sophisticated use of
e-TeX's facilities.")
    (license license:lppl1.3+)))

(define-public texlive-eulervm
  (package
    (name "texlive-eulervm")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/eulervm/"
                   "fonts/tfm/public/eulervm/"
                   "fonts/vf/public/eulervm/"
                   "source/latex/eulervm/" "tex/latex/eulervm/")
             (base32
              "118yidwnqw4acap2wdykcdx2cxp8q2ganz67ls6rkg6fid325mkq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/eulervm")
    (synopsis "Euler virtual math fonts")
    (description
     "The Euler fonts are suitable for typsetting mathematics in conjunction
with a variety of text fonts that do not provide mathematical character sets
of their own.  Euler-VM is a set of virtual mathematics fonts based on Euler
and CM. This approach has several advantages over immediately using the real
Euler fonts. Most noticeably, less TeX resources are consumed, the quality of
various math symbols is improved and a usable @code{\\hslash} symbol can be
provided.  The virtual fonts are accompanied by a LaTeX package which makes
them easy to use, particularly in conjunction with Type1 PostScript text
fonts.  They are compatible with @code{amsmath}.  A package option allows the
fonts to be loaded at 95% of their nominal size, thus blending better with
certain text fonts, e.g., Minion.")
    (license license:lppl)))

(define-public texlive-plain
  (package
    (name "texlive-plain")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "makeindex/plain/" "tex/plain/base/"
                   "tex/plain/config/")
             (base32
              "0zwvrfw8z28c9dy8nby5qfwbyrd2a0cdfwyd5jndscjczhw0yi62")))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-engine "tex"
      #:tex-format #f
      #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/plain")
    (synopsis "Plain TeX format and supporting files")
    (description
     "This package contains files used to build the Plain TeX format, as
described in the TeXbook, together with various supporting files (some also
discussed in the book).")
    (license license:knuth)))

(define-deprecated-package texlive-tex-plain texlive-plain)

(define-public texlive-pxfonts
  (package
    (name "texlive-pxfonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/pxfonts/"
                   "fonts/afm/public/pxfonts/"
                   "fonts/map/dvips/pxfonts/"
                   "fonts/tfm/public/pxfonts/"
                   "fonts/type1/public/pxfonts/"
                   "fonts/vf/public/pxfonts/"
                   "tex/latex/pxfonts/")
             (base32
              "0z2ls46x2l79saq4l1d2cqwazhpg79b9hqsf90wzx70676mxcwac")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pxfonts")
    (synopsis "Palatino-like fonts in support of mathematics")
    (description
     "Pxfonts supplies virtual text roman fonts using Adobe Palatino (or
URWPalladioL) with some modified and additional text symbols in the OT1, T1,
and TS1 encodings; maths alphabets using Palatino/Palladio; maths fonts
providing all the symbols of the Computer Modern and AMS fonts, including all
the Greek capital letters from CMR; and additional maths fonts of various
other symbols.  The set is complemented by a sans-serif set of text fonts,
based on Helvetica/NimbusSanL, and a monospace set derived from the parallel
TX font set.  All the fonts are in Type 1 format (AFM and PFB files), and are
supported by TeX metrics (VF and TFM files) and macros for use with LaTeX.")
    (license license:gpl3+)))

(define-public texlive-halloweenmath
  (package
    (name "texlive-halloweenmath")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/halloweenmath/"
                   "source/latex/halloweenmath/"
                   "tex/latex/halloweenmath/")
             (base32
              "1xq72k1p820b5q3haxf936g69p6gv34hr30870l96jnxa3ad7y05")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'non-interactive-build
            ;; When it realizes it cannot employ the usedir directive, the
            ;; build process stops and waits for an input before inserting
            ;; generated files in the working directory.  Do not ask for an
            ;; input.
            (lambda _
              (substitute* "source/latex/halloweenmath/halloweenmath.ins"
                (("\\Ask.*") "")
                (("\\(your .*? will be ignored\\).*") "")))))))
    (native-inputs (list texlive-cm))
    (home-page "https://ctan.org/pkg/halloweenmath")
    (synopsis "Scary and creepy math symbols with AMS-LaTeX integration")
    (description
     "The package defines a handful of commands for typesetting mathematical
symbols of various kinds, ranging from large operators to extensible
arrow-like relations and growing arrow-like math accents that all draw from
the classic Halloween-related iconography (pumpkins, witches, ghosts, cats,
and so on) while being, at the same time, seamlessly integrated within the
rest of the mathematics produced by (AmS-)LaTeX.")
    (license license:lppl1.3+)))

(define-public texlive-hardwrap
  (package
    (name "texlive-hardwrap")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hardwrap/" "source/latex/hardwrap/"
                   "tex/latex/hardwrap/")
             (base32
              "0ql3xml1ccll44q945n7w72p6d51y5wcrkawi7cg621gy5d6wzx5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hardwrap")
    (synopsis "Hard wrap text to a certain character length")
    (description
     "The package facilitates wrapping text to a specific character width,
breaking lines by words rather than, as done by TeX, by characters.  The
primary use for these facilities is to aid the generation of messages sent to
the log file or console output to display messages to the user.  Package
authors may also find this useful when writing out arbitrary text to an
external file.")
    (license license:lppl1.3+)))

(define-public texlive-hatching
  (package
    (name "texlive-hatching")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/hatching/"
                   "metapost/hatching/")
             (base32
              "1rkxx57rpzk8i0msbyrzcgmbvmjjrg295qg508w3v3dsy0f0krjj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hatching")
    (synopsis "MetaPost macros for hatching interior of closed paths")
    (description
     "The file @file{hatching}.mp contains a set of MetaPost macros for
hatching interior of closed paths.")
    (license license:public-domain)))

(define-public texlive-helvetic
  (package
    (name "texlive-helvetic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/helvetic/"
                   "fonts/afm/adobe/helvetic/"
                   "fonts/afm/urw/helvetic/"
                   "fonts/map/dvips/helvetic/"
                   "fonts/tfm/adobe/helvetic/"
                   "fonts/tfm/monotype/helvetic/"
                   "fonts/tfm/urw35vf/helvetic/"
                   "fonts/type1/urw/helvetic/"
                   "fonts/vf/adobe/helvetic/"
                   "fonts/vf/monotype/helvetic/"
                   "fonts/vf/urw35vf/helvetic/"
                   "tex/latex/helvetic/")
             (base32
              "0c3f1ly7y6404z0akbfbbfql13sz717v0n0g69qjpr69hi4n0nsl")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Helvetica font")
    (description
     "This package provides a drop-in replacement for the Helvetica font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-inputenx
  (package
    (name "texlive-inputenx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/inputenx/" "source/latex/inputenx/"
                   "tex/latex/inputenx/")
             (base32
              "0snjndrcynm4w8m9iq8gmadzhrbwvsdy4y1ak24ia0hpsicdi4aj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/inputenx")
    (synopsis "Enhanced input encoding handling")
    (description
     "This package deals with input encodings.  It provides a wider range of
input encodings using standard mappings, than does @code{inputenc}.  It also
covers nearly all slots.  In this way, it serves as more up to date
replacement for the @code{inputenc} package.")
    (license license:lppl1.3+)))

(define-public texlive-kpathsea
  (package
    (name "texlive-kpathsea")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/info/dir"
                   "doc/info/kpathsea.info"
                   "doc/info/tds.info"
                   "doc/info/web2c.info"
                   "doc/kpathsea/"
                   "doc/man/man1/kpseaccess.1"
                   "doc/man/man1/kpseaccess.man1.pdf"
                   "doc/man/man1/kpsereadlink.1"
                   "doc/man/man1/kpsereadlink.man1.pdf"
                   "doc/man/man1/kpsestat.1"
                   "doc/man/man1/kpsestat.man1.pdf"
                   "doc/man/man1/kpsewhich.1"
                   "doc/man/man1/kpsewhich.man1.pdf"
                   "doc/web2c/web2c.html"
                   "doc/web2c/web2c.pdf"
                   "web2c/amiga-pl.tcx"
                   "web2c/cp1250cs.tcx"
                   "web2c/cp1250pl.tcx"
                   "web2c/cp1250t1.tcx"
                   "web2c/cp227.tcx"
                   "web2c/cp852-cs.tcx"
                   "web2c/cp852-pl.tcx"
                   "web2c/cp8bit.tcx"
                   "web2c/empty.tcx"
                   "web2c/fmtutil.cnf"
                   "web2c/il1-t1.tcx"
                   "web2c/il2-cs.tcx"
                   "web2c/il2-pl.tcx"
                   "web2c/il2-t1.tcx"
                   "web2c/kam-cs.tcx"
                   "web2c/kam-t1.tcx"
                   "web2c/macce-pl.tcx"
                   "web2c/macce-t1.tcx"
                   "web2c/maz-pl.tcx"
                   "web2c/mktex.cnf"
                   "web2c/mktex.opt"
                   "web2c/mktexdir"
                   "web2c/mktexdir.opt"
                   "web2c/mktexnam"
                   "web2c/mktexnam.opt"
                   "web2c/mktexupd"
                   "web2c/natural.tcx"
                   "web2c/tcvn-t5.tcx"
                   "web2c/texmf.cnf"
                   "web2c/viscii-t5.tcx")
             (base32
              "104kn06vmk7ljpz3sjnsr7r69p0i6nwad2v8gimdl2f38a53s5n3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-references
            (lambda _
              (let ((dirs (map dirname (list (which "sed")
                                             (which "awk")))))
                (substitute* '("web2c/mktexdir"
                               "web2c/mktexnam"
                               "web2c/mktexupd")
                  (("^version=" m)
                   (format #false "PATH=\"~{~a:~}$PATH\"; export PATH~%~a"
                           dirs m)))))))))
    (inputs (list sed gawk))
    (home-page "https://ctan.org/pkg/kpathsea")
    (synopsis "Files related to the path searching library for TeX")
    (description
     "Kpathsea is a library and utility programs which provide path searching
facilities for TeX file types, including the self-locating feature required
for movable installations, layered on top of a general search mechanism.  This
package provides supporting files.")
    (license license:lgpl3+)))

(define-public texlive-kpfonts
  (package
    (name "texlive-kpfonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/kpfonts/"
                   "fonts/afm/public/kpfonts/"
                   "fonts/enc/dvips/kpfonts/"
                   "fonts/map/dvips/kpfonts/"
                   "fonts/tfm/public/kpfonts/"
                   "fonts/type1/public/kpfonts/"
                   "fonts/vf/public/kpfonts/"
                   "tex/latex/kpfonts/")
             (base32
              "0m5waxqrkm1i59i9vbn9ai9zjn7cl0f36iccwn2d73lhrqhbn16q")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/kpfonts")
    (synopsis "Complete set of fonts for text and mathematics")
    (description
     "The family contains text fonts in roman, sans-serif and monospaced
shapes, with true small caps and old-style numbers; the package offers full
support of the textcomp package.  The mathematics fonts include all the AMS
fonts, in both normal and bold weights.  Each of the font types is available
in two main versions: default and light.  Each version is available in four
variants: default; oldstyle numbers; oldstyle numbers with old ligatures such
as ct and st, and long-tailed capital Q; and veryoldstyle with long s.  Other
variants include small caps as default or large small caps, and for
mathematics both upright and slanted shapes for Greek letters, as well as
default and narrow versions of multiple integrals.")
    (license license:gpl3+)))

(define-public texlive-latexconfig
  (package
    (name "texlive-latexconfig")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/latex/latexconfig/")
             (base32
              "1x5fyr2185nx3qlyariykdz44hcy5azimrk9db2p707dg08bjhsd")))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/latexconfig")
    (synopsis "Configuration files for LaTeX-related formats")
    (description "The package provides configuration files for LaTeX-related
formats.")
    (license license:lppl)))

(define-public texlive-latex-bin
  (package
    (name "texlive-latex-bin")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/dvilualatex.1"
                   "doc/man/man1/dvilualatex.man1.pdf"
                   "doc/man/man1/latex.1"
                   "doc/man/man1/latex.man1.pdf"
                   "doc/man/man1/lualatex.1"
                   "doc/man/man1/lualatex.man1.pdf"
                   "doc/man/man1/pdflatex.1"
                   "doc/man/man1/pdflatex.man1.pdf")
             (base32
              "1mhdc8a37b9j64kc8c8171s8p7ixklbf1ijr4vfh7af2k416qf8d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:create-formats #~(list "dvilualatex" "latex" "lualatex" "pdflatex")))
    (propagated-inputs
     (list texlive-atbegshi
           texlive-atveryend
           texlive-babel
           texlive-cm
           texlive-everyshi
           texlive-firstaid
           texlive-graphics
           texlive-hyphen-complete
           texlive-l3backend
           texlive-l3kernel
           texlive-l3packages
           texlive-latex
           texlive-latex-fonts
           texlive-latexconfig
           texlive-lm
           texlive-luahbtex
           texlive-luaotfload
           texlive-luatex
           texlive-pdftex
           texlive-tex-ini-files
           texlive-unicode-data))
    (home-page "https://ctan.org/pkg/latex-bin")
    (synopsis "LaTeX formats and man pages")
    (description
     "This package provides LaTeX format files and man pages along with
several packages that are considered as part of the LaTeX kernel.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-base texlive-latex-bin)

(define-public texlive-atenddvi
  (package
    (name "texlive-atenddvi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/atenddvi/"
                   "source/latex/atenddvi/"
                   "tex/latex/atenddvi/")
             (base32
              "1fwa5233mdgvixhl2rzn9s06zz52j6ml7hfzd4194bn389n9syhk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/atenddvi")
    (synopsis "Provide the @code{\\AtEndDvi} command for older LaTeX format")
    (description
     "This package is unneeded and does nothing when used with a LaTeX format
2020-10-01 or newer as in this case the format provides the @code{\\AtEndDvi}
command.  For older formats it implements @code{\\AtEndDvi}, a counterpart to
@code{\\AtBeginDvi}.  The execution of its argument is delayed to the end of
the document at the end of the last page.  Thus @code{\\special} and
@code{\\write} remain effective, because they are put into the last page.
This is the main difference to @code{\\AtEndDocument}.")
    (license license:lppl1.3c+)))

(define-public texlive-attachfile
  (package
    (name "texlive-attachfile")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bib/attachfile/"
                   "doc/latex/attachfile/"
                   "source/latex/attachfile/"
                   "tex/latex/attachfile/")
             (base32
              "0340c4rvxhhk95wlhf54n9akiwhj6pj0bslys6bkq29x9903zx5h")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/attachfile")
    (synopsis "Attach arbitrary files to a PDF document")
    (description
     "Starting with PDF 1.3, PDF files can contain file attachments, i.e.,
arbitrary files that a reader can extract, just like attachments to an e-mail
message.  The @code{attachfile} package brings this functionality to pdfLaTeX
and provides some additional features such as the ability to use arbitrary
LaTeX code for the file icon.  Settings can be made either globally or on
a per-attachment basis.  @code{attachfile} makes it easy to attach files and
customize their appearance in the enclosing document.")
    (license license:lppl1.3+)))

(define-public texlive-attachfile2
  (package
    (name "texlive-attachfile2")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/attachfile2/"
                   "doc/man/man1/pdfatfi.1"
                   "doc/man/man1/pdfatfi.man1.pdf"
                   "scripts/attachfile2/"
                   "source/latex/attachfile2/"
                   "tex/latex/attachfile2/")
             (base32
              "14glnfyp58ka78l5kc09mc96km7jd77qvsby0p8dpjhvwf6qfzg8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "pdfatfi.pl")))
    (inputs (list perl))
    (home-page "https://ctan.org/pkg/attachfile2")
    (synopsis "Attach files into PDF")
    (description
     "This package can be used to attach files to a PDF document.  It is
a further development of Scott Pakin's package @code{attachfile} for pdfTeX.
Apart from bug fixes, this package adds support for dvips, some new options,
and gets and writes meta information data about the attached files.")
    (license license:lppl1.3c)))

(define-public texlive-atveryend
  (package
    (name "texlive-atveryend")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/atveryend/"
                   "source/latex/atveryend/"
                   "tex/latex/atveryend/")
             (base32
              "1rp805h0m99rxs107a798l951lyahlnp7irfklfadn2a2ljzhafn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip texlive-pdftex))
    (home-page "https://ctan.org/pkg/atveryend")
    (synopsis "Hooks at the very end of a document")
    (description
     "This LaTeX packages provides two hooks for @code{\\end{document}} that
are executed after the hook of @code{\\AtEndDocument}:
@code{\\AfterLastShipout} can be used for code that is to be executed right
after the last @code{\\clearpage} before the @file{.aux} file is closed.
@code{\\AtVeryEndDocument} is used for code after closing and final reading of
the @file{.aux} file.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-atveryend texlive-atveryend)

(define-public texlive-automata
  (package
    (name "texlive-automata")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/automata/"
                   "metapost/automata/")
             (base32
              "0y02wp1w2jkhrxanz7r699kr08bmbfn0h0xaxj3zh4xj9745d7d7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/automata")
    (synopsis "Finite state machines, graphs and trees in MetaPost")
    (description
     "The package offers a collection of macros for MetaPost to make easier to
draw finite-state machines, automata, labelled graphs, etc.  The user defines
nodes, which may be isolated or arranged into matrices or trees; edges connect
pairs of nodes through arbitrary paths.  Parameters, that specify the shapes
of nodes and the styles of edges, may be adjusted.")
    (license license:lppl)))

(define-public texlive-auxhook
  (package
    (name "texlive-auxhook")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/auxhook/" "source/latex/auxhook/"
                   "tex/latex/auxhook/")
             (base32
              "1qfs7bz8ryp4prr2fw4hwypnfc6yr4rc4wd8qy4rpmab0hab0vdy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/auxhook")
    (synopsis "Hooks for auxiliary files")
    (description
     "This package provides hooks for adding code at the beginning of
@file{.aux} files.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-auxhook texlive-auxhook)

(define-public texlive-epstopdf
  (package
    (name "texlive-epstopdf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/epstopdf.1"
                   "doc/man/man1/epstopdf.man1.pdf"
                   "doc/man/man1/repstopdf.1"
                   "doc/man/man1/repstopdf.man1.pdf"
                   "doc/support/epstopdf/"
                   "scripts/epstopdf/")
             (base32
              "0r2dr8f8myc663hdzzrkaiddfqsmmf41xan9y6kd1n049hhw414l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:link-scripts #~(list "epstopdf.pl")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-gs-location
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gs (search-input-file inputs "/bin/gs")))
                (substitute* "scripts/epstopdf/epstopdf.pl"
                  (("\"gs\"") (string-append "\"" gs "\"")))))))))
    (home-page "https://ctan.org/pkg/epstopdf")
    (synopsis "Convert EPS to PDF using Ghostscript")
    (inputs (list ghostscript perl))
    (description
     "Epstopdf is a Perl script that converts an EPS file to an encapsulated
PDF file (a single page file whose media box is the same as the original EPS's
bounding box). The resulting file suitable for inclusion by pdfTeX as an
image.  LaTeX users may make use of the @code{epstopdf} package, which will
run the @code{epstopdf} script on the fly, thus giving the illusion that
pdfLaTeX is accepting EPS graphic files.")
    (license license:bsd-3)))

(define-public texlive-epstopdf-pkg
  (package
    (name "texlive-epstopdf-pkg")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/epstopdf-pkg/"
                   "source/latex/epstopdf-pkg/"
                   "tex/latex/epstopdf-pkg/")
             (base32
              "1ajyc5pkn1niifz5asyf09vbdqvmy05xwl0vxcdl7ik0ll0jcaxp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/epstopdf-pkg")
    (synopsis "Call @command{epstopdf} on the fly")
    (description
     "The package adds support for EPS files in the @code{graphicx} package
when running under pdfTeX.  If an EPS graphic is detected, the package spawns
a process to convert the EPS to PDF, using the script @command{epstopdf}.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-epstopdf-pkg texlive-epstopdf-pkg)

(define-public texlive-filecontents
  (package
    (name "texlive-filecontents")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/filecontents/"
                   "source/latex/filecontents/"
                   "tex/latex/filecontents/")
             (base32
              "0ifhqfdzx91hrmndhg5441rpmv9k4lxrql02kd5yx75xpplxryzw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/filecontents")
    (synopsis "Create an external file from within a LaTeX document")
    (description
     "LaTeX2e's @code{filecontents} and @code{filecontents*} environments
enable a LaTeX source file to generate external files as it runs through
LaTeX.  However, there are two limitations of these environments: they refuse
to overwrite existing files, and they can only be used in the preamble of
a document.  The filecontents package removes these limitations, letting you
overwrite existing files and letting you use @code{filecontents}
@code{filecontents*} anywhere.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-filecontents texlive-filecontents)

(define-public texlive-filehook
  (package
    (name "texlive-filehook")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/filehook/" "source/latex/filehook/"
                   "tex/latex/filehook/")
             (base32
              "1zg9svjhrnh52fa04n3pnb0hrijp0lrr939dacf90cjjzwk36sfn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (home-page "https://ctan.org/pkg/filehook")
    (synopsis "Hooks for input files")
    (description
     "The package provides several file hooks (@code{AtBegin}, @code{AtEnd},
...) for files read by @code{\\input}, @code{\\include} and
@code{\\InputIfFileExists}.  General hooks for all such files (e.g., all
@code{\\included} ones) and file specific hooks only used for named files are
provided; two hooks are provided for the end of @code{\\included} files ---
one before, and one after the final @code{\\clearpage}.")
    (license license:lppl1.3+)))

(define-public texlive-epsf
  (package
    (name "texlive-epsf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/epsf/" "tex/generic/epsf/")
             (base32
              "03jcf0kqh47is965d2590miwj7d5kif3c4mgsnvkyl664jzjkh92")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/epsf")
    (synopsis "Simple macros for EPS inclusion")
    (description
     "This package provides the original (and now obsolescent) graphics
inclusion macros for use with dvips, still widely used by Plain TeX users (in
particular).  For LaTeX users, the package is nowadays (rather strongly)
deprecated in favour of the more sophisticated standard LaTeX latex-graphics
bundle of packages.  (The latex-graphics bundle is also available to Plain TeX
users, via its Plain TeX version.)")
    (license license:public-domain)))

(define-deprecated-package texlive-generic-epsf texlive-epsf)

(define-public texlive-fancyvrb
  (package
    (name "texlive-fancyvrb")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fancyvrb/" "tex/latex/fancyvrb/")
             (base32
              "18nif609wp9y9bc3jn3cz07ihphp95mqa4bfpgqlxsy3m57295s7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fancyvrb")
    (synopsis "Sophisticated verbatim text")
    (description
     "This package provides tools for the flexible handling of verbatim text
including: verbatim commands in footnotes; a variety of verbatim environments
with many parameters; ability to define new customized verbatim environments;
save and restore verbatim text and environments; write and read files in
verbatim mode; build \"example\" environments (showing both result and
verbatim source).")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-fancyvrb texlive-fancyvrb)

(define-public texlive-gentium-tug
  (package
    (name "texlive-gentium-tug")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/gentium-tug/GentiumPlus-6.101/"
                   "doc/fonts/gentium-tug/gentium-tug/"
                   "fonts/afm/public/gentium-tug/"
                   "fonts/enc/dvips/gentium-tug/"
                   "fonts/map/dvips/gentium-tug/"
                   "fonts/map/pdftex/gentium-tug/"
                   "fonts/tfm/public/gentium-tug/"
                   "fonts/truetype/public/gentium-tug/"
                   "fonts/type1/public/gentium-tug/"
                   "source/fonts/gentium-tug/"
                   "tex/latex/gentium-tug/")
             (base32
              "1ns62cn7cqq3hzmljd6yvs2mip2l30pa3js6c1dm90fj01gsjq86")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/gentium-tug")
    (synopsis "Gentium fonts (in two formats) and support files")
    (description
     "Gentium is a typeface family designed to enable the diverse ethnic
groups around the world who use the Latin, Cyrillic and Greek scripts to
produce readable, high-quality publications.  It supports a wide range of
Latin- and Cyrillic-based alphabets.")
    (license (list license:expat license:silofl1.1))))

(define-public texlive-gincltex
  (package
    (name "texlive-gincltex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/gincltex/" "source/latex/gincltex/"
                   "tex/latex/gincltex/")
             (base32
              "1x6bsf445dp8wc5hfgyywlal8vky5w23d69zlpybkp2d9am9a71p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/gincltex")
    (synopsis "Include TeX files as graphics")
    (description
     "The package builds on the standard LaTeX packages @code{graphics} and
allows external LaTeX source files to be included, in the same way as graphic
files, by @code{\\includegraphics}.  In effect, then package adds support for
the @file{.tex} extension.")
    (license license:lppl1.3+)))

(define-public texlive-glyphlist
  (package
    (name "texlive-glyphlist")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/map/glyphlist/")
             (base32
              "12nmmyh77vr2622lzi11nm1z1k2bxs7jz018yl4rwjlpg0sxg1ja")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/glyphlist")
    (synopsis "Adobe Glyph List and TeX extensions")
    (description
     "This package provides a map between traditional Adobe glyph names and
Unicode points; it is maintained by Adobe.  The additional
@file{texglyphlist.txt} is maintained as part of @code{lcdf-typetools}.")
    (license license:asl2.0)))

(define-public texlive-graphics-def
  (package
    (name "texlive-graphics-def")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/graphics-def/"
                   "tex/latex/graphics-def/")
             (base32
              "1kp28k3shsiv0a051lp4fcqadac41c942hxwn506yps7h9y4jg23")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/graphics-def")
    (synopsis "Colour and graphics option files")
    (description
     "This bundle is a combined distribution consisting of @file{dvips.def},
@file{pdftex.def}, @file{luatex.def}, @file{xetex.def}, @file{dvipdfmx.def},
and @file{dvisvgm.def} driver option files for the LaTeX graphics and color
packages.")
    (license license:lppl1.3c+)))

(define-public texlive-graphics-cfg
  (package
    (name "texlive-graphics-cfg")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/graphics-cfg/"
                   "tex/latex/graphics-cfg/")
             (base32
              "00n63adb2laf43lzix39xl68aq0k5k80mmrw602w99w5n7f96gsf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/graphics-cfg")
    (synopsis "Sample configuration files for LaTeX color and graphics")
    (description
     "This bundle includes @file{color.cfg} and @file{graphics.cfg} files that
set default \"driver\" options for the color and graphics packages.")
    (license license:public-domain)))

(define-public texlive-graphics
  (package
    (name "texlive-graphics")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/graphics/"
                   "source/latex/graphics/"
                   "tex/latex/graphics/")
             (base32
              "024hncahwc07yd2l94znv7v72sbykxdri5lpg3w4ip0nf10ywyma")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip texlive-pdftex))
    (propagated-inputs (list texlive-graphics-def texlive-graphics-cfg))
    (home-page "https://ctan.org/macros/latex/required/graphics")
    (synopsis "The LaTeX standard graphics bundle")
    (description
     "This is a collection of LaTeX packages for: producing colour including
graphics (e.g., PostScript) files rotation and scaling of text in LaTeX
documents.  It comprises the packages @code{color}, @code{graphics},
@code{graphicx}, @code{trig}, @code{epsfig}, @code{keyval}, and
@code{lscape}.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-graphics texlive-graphics)

(define-public texlive-greek-fontenc
  (package
    (name "texlive-greek-fontenc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/greek-fontenc/"
                   "tex/latex/greek-fontenc/")
             (base32
              "1vary0vdrg77r55lf6gbfsqiyxqkbvrx1ijk71q3yl2v6adml4iv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/greek-fontenc")
    (synopsis "LICR macros and encoding definition files for Greek")
    (description
     "The package provides Greek LICR macro definitions and encoding
definition files for Greek text font encodings for use with @code{fontenc}.")
    (license license:lppl1.3+)))

(define-public texlive-hycolor
  (package
    (name "texlive-hycolor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hycolor/" "source/latex/hycolor/"
                   "tex/latex/hycolor/")
             (base32
              "0hmkx37wwmznxjqqnca87shy7qrgqrh2cn5r941ddgivnym31xbh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hycolor")
    (synopsis "Colour for packages @code{hyperref} and @code{bookmark}")
    (description
     "This package provides the code for the color option that is used by
packages @code{hyperref} and @code{bookmark}.  It is not intended as a package
for the user.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-hycolor texlive-hycolor)

(define-public texlive-xcolor
  (package
    (name "texlive-xcolor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xcolor/" "dvips/xcolor/"
                   "source/latex/xcolor/" "tex/latex/xcolor/")
             (base32
              "1sh0v60azjbl2fcmg4p77dqw052j59d9pg20saxvj4md32a345py")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xcolor")
    (synopsis "Driver-independent color extensions for LaTeX and pdfLaTeX")
    (description
     "The package starts from the basic facilities of the colorcolor package,
and provides easy driver-independent access to several kinds of color tints,
shades, tones, and mixes of arbitrary colors.  It allows a user to select
a document-wide target color model and offers complete tools for conversion
between eight color models.  Additionally, there is a command for alternating
row colors plus repeated non-aligned material (like horizontal lines) in
tables.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-xcolor texlive-xcolor)

(define-public texlive-xltxtra
  (package
    (name "texlive-xltxtra")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xltxtra/"
                   "source/xelatex/xltxtra/"
                   "tex/xelatex/xltxtra/")
             (base32
              "0dh3nnr4xhb85zp3q4jdzqvfghg8m9zvpsvvrmmp0q3ayf0c1dbj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-metalogo))
    (home-page "https://ctan.org/pkg/xltxtra")
    (synopsis "Extras for LaTeX users of XeTeX")
    (description
     "This package was previously used to provide a number of features that
were useful for typesetting documents with XeLaTeX.  Many of those features
have now been incorporated into the @code{fontspec} package and other
packages, but the package persists for backwards compatibility.  Nowadays,
loading @code{xltxtra} will: load the @code{fontspec}, @code{metalogo}, and
@code{realscripts} packages; redefine @code{\\showhyphens} so it works
correctly; and define two extra commands: @code{\\vfrac} and
@code{\\namedglyph}.")
    (license license:lppl1.3c)))

(define-public texlive-xmltexconfig
  (package
    (name "texlive-xmltexconfig")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/xmltex/xmltexconfig/")
             (base32
              "0drgvmn27snq43kjkpg5k4igkcdi56p1q3fwkrv3ivsiqfp6cdz3")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xmltexconfig")
    (synopsis "Configuration files for @code{xmltex} and @code{pdfxmltex}")
    (description "This package provides configuration files for @code{xmltex}
and @code{pdfxmltex}.")
    (license license:public-domain)))

(define-public texlive-xmltex
  (package
    (name "texlive-xmltex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/otherformats/xmltex/base/"
                   "tex/xmltex/base/")
             (base32
              "1rqwsapba8zs2ijjs7lpzksm20jqb8zbmanpw7wmdp2rq26ahylh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:create-formats #~(list "pdfxmltex" "xmltex")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-wrappers
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((pdftex (search-input-file inputs "/bin/pdftex"))
                    (web2c (string-append #$output "/share/texmf-dist/web2c")))
                (mkdir-p (string-append #$output "/bin"))
                (symlink pdftex (string-append #$output "/bin/xmltex"))
                (symlink pdftex (string-append #$output "/bin/pdfxmltex"))))))))
    (propagated-inputs
     (list texlive-atbegshi
           texlive-atveryend
           texlive-babel
           texlive-cm
           texlive-everyshi
           texlive-firstaid
           texlive-hyphen-complete
           texlive-l3backend
           texlive-l3kernel
           texlive-l3packages
           texlive-latex
           texlive-latex-fonts
           texlive-latexconfig
           texlive-pdftex
           texlive-tex
           texlive-tex-ini-files
           texlive-unicode-data
           texlive-xmltexconfig))
    (home-page "https://ctan.org/pkg/xmltex")
    (synopsis "Support for parsing XML documents")
    (description
     "The package provides an implementation of a parser for documents
matching the XML 1.0 and XML Namespace Recommendations.  Element and attribute
names, as well as character data, may use any characters allowed in XML, using
UTF-8 or a suitable 8-bit encoding.")
    (license license:lppl1.0+)))        ;per xmltex/base/readme.txt

(define-public texlive-hyperref
  (package
    (name "texlive-hyperref")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hyperref/"
                   "source/latex/hyperref/"
                   "tex/latex/hyperref/")
             (base32
              "034bdg1vy2yql4sq9i3i1ss1axh7apdyk5rz3s2ss8fydvvds726")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atbegshi
           texlive-auxhook
           texlive-bitset
           texlive-etexcmds
           texlive-gettitlestring
           texlive-hycolor
           texlive-intcalc
           texlive-kvdefinekeys
           texlive-kvsetkeys
           texlive-letltxmacro
           texlive-ltxcmds
           texlive-pdfescape
           texlive-refcount
           texlive-rerunfilecheck
           texlive-stringenc
           texlive-url
           texlive-zapfding))
    (home-page "https://www.ctan.org/pkg/hyperref")
    (synopsis "Extensive support for hypertext in LaTeX")
    (description
     "The @code{hyperref} package is used to handle cross-referencing commands
in LaTeX to produce hypertext links in the document.  The package provides
backends for the @code{\\special} set defined for HyperTeX DVI processors; for
embedded @code{pdfmark} commands for processing by Acrobat
Distiller (@code{dvips} and Y&Y's @code{dvipsone}); for Y&Y's @code{dviwindo};
for PDF control within pdfTeX and @code{dvipdfm}; for TeX4ht; and for VTeX's
pdf and HTML backends.  The package is distributed with the @code{backref} and
@code{nameref} packages, which make use of the facilities of
@code{hyperref}.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-hyperref texlive-hyperref)

(define-public texlive-hypdoc
  (package
    (name "texlive-hypdoc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hypdoc/" "source/latex/hypdoc/"
                   "tex/latex/hypdoc/")
             (base32
              "14qg7q9r4cx132m2mr132ml0r49psfy99g6my4wir4yaw7y0x6pp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hypdoc")
    (synopsis "Hyper extensions for @file{doc.sty}")
    (description
     "This package adds hypertext features to the package @code{doc} that is
used in the documentation system of LaTeX2e.  Bookmarks are added and
references are linked as far as possible.")
    (license license:lppl1.3c)))

(define-public texlive-hyperxmp
  (package
    (name "texlive-hyperxmp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hyperxmp/"
                   "doc/man/man1/hyperxmp-add-bytecount.1"
                   "doc/man/man1/hyperxmp-add-bytecount.man1.pdf"
                   "scripts/hyperxmp/"
                   "source/latex/hyperxmp/"
                   "tex/latex/hyperxmp/")
             (base32
              "0x2hif61a7bz1ymrc2qz0f9papfj2qx2w0smpihrcjxq73g9dm1b")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:link-scripts #~(list "hyperxmp-add-bytecount.pl")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build
            (lambda _
              (delete-file "source/latex/hyperxmp/hyperxmp-stds.tex"))))))
    (inputs (list perl))
    (home-page "https://ctan.org/pkg/hyperxmp")
    (synopsis "Embed XMP metadata within a LaTeX document")
    (description
     "XMP (eXtensible Metadata Platform) is a mechanism proposed by Adobe for
embedding document metadata, within the document itself.  The metadata is
designed to be easy to extract, even by programs that are oblivious to the
document's file format.  The @code{hyperxmp} package makes it trivial for
LaTeX document authors to store XMP metadata in their documents as well.  It
is compatible with pdfLaTeX, XeLaTeX, LaTeX+dvipdfm, and LaTeX+dvips+ps2pdf.")
    (license license:lppl1.3c)))

(define-public texlive-oberdiek
  (package
    (name "texlive-oberdiek")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bib/oberdiek/" "doc/latex/oberdiek/"
                   "source/latex/oberdiek/"
                   "tex/generic/oberdiek/" "tex/latex/oberdiek/")
             (base32
              "1fm6bcrxr4bw49h5hzrlas6ihaavshp6yjjvdjn869bl6hm6pmlz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-auxhook
           texlive-grfext
           texlive-grffile
           texlive-iftex
           texlive-infwarerr
           texlive-kvoptions
           texlive-pdftexcmds))
    (home-page "https://www.ctan.org/pkg/oberdiek")
    (synopsis "Bundle of packages submitted by Heiko Oberdiek")
    (description
     "The bundle comprises various LaTeX packages, providing among others:
better accessibility support for PDF files; extensible chemists reaction
arrows; record information about document class(es) used; and many more.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-oberdiek texlive-oberdiek)

(define-public texlive-regexpatch
  (package
    (name "texlive-regexpatch")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/regexpatch/"
                   "source/latex/regexpatch/"
                   "tex/latex/regexpatch/")
             (base32
              "1jv8hvkvq0yvc8mh68ybj8fvhf6kcdzwjin1czs45i26s0dpsngj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/regexpatch")
    (synopsis "High level patching of commands")
    (description
     "The package generalises the macro patching commands provided by
P. Lehmann's @code{etoolbox}.  The difference between this package and its
sibling @code{xpatch} is that this package sports a very powerful
@code{\\regexpatchcmd} based on the @code{l3regex} module of the LaTeX3
experimental packages.")
    (license license:lppl1.3c)))

(define-public texlive-rerunfilecheck
  (package
    (name "texlive-rerunfilecheck")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/rerunfilecheck/"
                   "source/latex/rerunfilecheck/"
                   "tex/latex/rerunfilecheck/")
             (base32
              "0m3rvjgw0hg3n7db8hpyq55lq7py4scm35bqbawpc5mn5pmh2zg1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-atveryend texlive-uniquecounter))
    (home-page "https://www.ctan.org/pkg/rerunfilecheck")
    (synopsis "Checksum based rerun checks on auxiliary files")
    (description
     "This package provides additional rerun warnings if some auxiliary files
have changed.  It is based on MD5 checksum, provided by pdfTeX.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-rerunfilecheck texlive-rerunfilecheck)

(define-public texlive-onedown
  (package
    (name "texlive-onedown")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/onedown/"
                   "source/latex/onedown/"
                   "tex/latex/onedown/")
             (base32
              "00yh6nyzzy6d2sj1ha7dgfbsicy2ip1irn3il5jhc29sn3crfahx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/onedown")
    (synopsis "Typeset bridge diagrams")
    (description
     "This is a comprehensive package to draw all sorts of bridge diagrams,
including hands, bidding tables, trick tables, and expert quizzes.

It works for all font sizes.  Different fonts for hands, bidding diagrams and
compass are possible.  It also provides annotations to card and bidding
diagrams, automated check on consistency of suit and hands, and multilingual
output of bridge terms.")
    (license license:lppl1.3+)))

(define-public texlive-tools
  (package
    (name "texlive-tools")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/tools/"
                   "source/latex/tools/"
                   "tex/latex/tools/")
             (base32
              "04zafcgdgiricq0l6lplfxa2qdspx5wbzpql5h7hw90lsdiw8awk")))
    (outputs '("out" "doc"))
    (arguments
     (list #:build-targets #~(list "tools.ins")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tools")
    (synopsis "The LaTeX standard tools bundle")
    (description "This package provides a collection of simple tools that
are part of the LaTeX required tools distribution, comprising the packages:
@code{afterpage}, @code{array}, @code{bm}, @code{calc}, @code{dcolumn},
@code{delarray}, @code{enumerate}, @code{fileerr}, @code{fontsmpl},
@code{ftnright}, @code{hhline}, @code{indentfirst}, @code{layout},
@code{longtable}, @code{multicol}, @code{rawfonts}, @code{showkeys},
@code{somedefs}, @code{tabularx}, @code{theorem}, @code{trace},
@code{varioref}, @code{verbatim}, @code{xr}, and @code{xspace}.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-tools texlive-tools)

(define-public texlive-url
  (package
    (name "texlive-url")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/url/" "tex/latex/url/")
             (base32
              "184m40wgnx939ky2hbxnj0v9aak023ldrhgffp0lgyk9wdqpxlqg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/url")
    (synopsis "Verbatim with URL-sensitive line breaks")
    (description "The command @code{\\url} is a form of verbatim command that
allows linebreaks at certain characters or combinations of characters, accepts
reconfiguration, and can usually be used in the argument to another command.
The command is intended for email addresses, hypertext links,
directories/paths, etc., which normally have no spaces, so by default the
package ignores spaces in its argument.  However, a package option allows
spaces, which is useful for operating systems where spaces are a common part
of file names.")
    ;; The license header states that it is under LPPL version 2 or later, but
    ;; the latest version is 1.3c.
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-url texlive-url)

(define-public texlive-l3experimental
  (package
    (name "texlive-l3experimental")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/l3experimental/"
                   "source/latex/l3experimental/l3benchmark/"
                   "source/latex/l3experimental/l3bitset/"
                   "source/latex/l3experimental/l3draw/"
                   "source/latex/l3experimental/l3graphics/"
                   "source/latex/l3experimental/l3opacity/"
                   "source/latex/l3experimental/l3str/"
                   "source/latex/l3experimental/l3sys-shell/"
                   "source/latex/l3experimental/xcoffins/"
                   "source/latex/l3experimental/xgalley/"
                   "tex/latex/l3experimental/l3benchmark/"
                   "tex/latex/l3experimental/l3bitset/"
                   "tex/latex/l3experimental/l3draw/"
                   "tex/latex/l3experimental/l3graphics/"
                   "tex/latex/l3experimental/l3opacity/"
                   "tex/latex/l3experimental/l3str/"
                   "tex/latex/l3experimental/l3sys-shell/"
                   "tex/latex/l3experimental/xcoffins/"
                   "tex/latex/l3experimental/xgalley/")
             (base32
              "1s4v0f29hk5racmvj4imyn6d2az7i94s4fq3hag11hr08ipvi51v")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-l3kernel))
    (home-page "https://ctan.org/pkg/l3experimental")
    (synopsis "Experimental LaTeX3 concepts")
    (description
     "The l3experimental packages are a collection of experimental
implementations for aspects of the LaTeX3 kernel, dealing with higher-level
ideas such as the Designer Interface.  Some of them work as stand alone
packages, providing new functionality, and can be used on top of LaTeX2e with
no changes to the existing kernel.  The present release includes:
@itemize @code
@item l3benchmark
for measuring the time taken by TeX to run certain code;
@item l3draw
a code-level interface for constructing drawings;
@item l3graphics
an interface for the inclusion of graphics files;
@item l3opacity
support for opacity in PDF output;
@item l3str
support for string manipulation;
@item l3bitset
support for bit vectors;
@item l3sys-shell
which provides abstractions for common shell functions like file
deletion and copying;
@item xcoffins
which allows the alignment of boxes using a series of handle positions,
supplementing the simple TeX reference point;
@item xgalley
which controls boxes receiving text for typesetting.
@end itemize")
    (license license:lppl1.3c)))

(define-public texlive-l3kernel
  (package
    (name "texlive-l3kernel")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/l3kernel/"
                   "source/latex/l3kernel/"
                   "tex/latex/l3kernel/")
             (base32
              "1jbll1x3pdjpm1v70h7kpxgkjsw2mi2zbdilc7qvh251amn0hdbv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-engine "tex"
      #:tex-format #f
      #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip))
    (propagated-inputs (list texlive-l3backend))
    (home-page "https://ctan.org/pkg/l3kernel")
    (synopsis "LaTeX3 programming conventions")
    (description
     "The l3kernel bundle provides an implementation of the LaTeX3 programmers
interface, as a set of packages that run under LaTeX2e.  The interface
provides the foundation on which the LaTeX3 kernel and other future code are
built: it is an API for TeX programmers.  The packages are set up so that the
LaTeX3 conventions can be used with regular LaTeX2e packages.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-l3kernel texlive-l3kernel)

(define-public texlive-l3backend
  (package
    (name "texlive-l3backend")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/l3backend/"
                   "dvips/l3backend/"
                   "source/latex/l3backend/"
                   "tex/latex/l3backend/")
             (base32
              "1pd2drks05k9w3fzgzg7vkj46plpw5z9r0zl43r1kzya9c4ldb38")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:tex-engine "tex"
           #:tex-format #f
           #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip))
    (home-page "https://ctan.org/pkg/l3backend")
    (synopsis "LaTeX3 backend drivers")
    (description
     "This package forms parts of expl3, and contains the code used to
interface with backends (drivers) across the expl3 codebase.  The functions
here are defined differently depending on the engine in use.  As such, these
are distributed separately from l3kernel to allow this code to be updated on
an independent schedule.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-dvips-l3backend texlive-l3backend)
(define-deprecated-package texlive-latex-l3backend texlive-l3backend)

(define-public texlive-l3packages
  (package
    (name "texlive-l3packages")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/l3packages/"
                   "source/latex/l3packages/l3keys2e/"
                   "source/latex/l3packages/xfp/"
                   "source/latex/l3packages/xfrac/"
                   "source/latex/l3packages/xparse/"
                   "source/latex/l3packages/xtemplate/"
                   "tex/latex/l3packages/l3keys2e/"
                   "tex/latex/l3packages/xfp/"
                   "tex/latex/l3packages/xfrac/"
                   "tex/latex/l3packages/xparse/"
                   "tex/latex/l3packages/xtemplate/")
             (base32
              "0l7mfm8ziil59drqmb723n0wjbwdsx54iah051haxlbj7psqmbax")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:build-targets
           '(list "l3keys2e.ins"
                  "xparse.ins"
                  "xfrac.ins"
                  "xfp.ins"
                  "xtemplate.ins")
           #:tex-engine "tex"
           #:tex-format #f
           #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip))
    (propagated-inputs (list texlive-l3kernel))
    (home-page "https://ctan.org/pkg/l3packages")
    (synopsis "High-level LaTeX3 concepts")
    (description
     "This collection contains implementations for aspects of the LaTeX3
kernel, dealing with higher-level ideas such as the Designer Interface.  The
packages here are considered broadly stable (The LaTeX3 Project does not
expect the interfaces to alter radically).  These packages are built on
LaTeX2e conventions at the interface level, and so may not migrate in the
current form to a stand-alone LaTeX3 format.

Packages provided are @code{xparse}, which provides a high-level interface for
declaring document commands @code{xfp}, an expandable IEEE 754 FPU for LaTeX,
@code{l3keys2e}, which makes the facilities of the kernel module l3keys
available for use by LaTeX 2e packages, @code{xtemplate}, which provides
a means of defining generic functions using a key-value syntax, and
@code{xfrac}, which provides flexible split-level fractions.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-l3packages texlive-l3packages)

(define-public texlive-fontspec
  (package
    (name "texlive-fontspec")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fontspec/"
                   "source/latex/fontspec/"
                   "tex/latex/fontspec/")
             (base32
              "0rh3x1h8glpmw0nmqv8lili3vf0zw2lcgffzk680c86k8jpjy4cm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-euenc
           texlive-iftex
           texlive-l3kernel
           texlive-l3packages
           texlive-lm
           texlive-xunicode))
    (home-page "https://ctan.org/pkg/fontspec")
    (synopsis "Advanced font selection in XeLaTeX and LuaLaTeX")
    (description
     "Fontspec is a package for XeLaTeX and LuaLaTeX.  It provides an
automatic and unified interface to feature-rich AAT and OpenType fonts through
the NFSS in LaTeX running on XeTeX or LuaTeX engines.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-fontspec texlive-fontspec)

(define-public texlive-grffile
  (package
    (name "texlive-grffile")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/grffile/"
                   "source/latex/grffile/"
                   "tex/latex/grffile/")
             (base32
              "01mlvdhqwfwj1l91jfvkdfbn1hj95rlb6xhwikzx1r8qrz5whw7n")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/grffile")
    (synopsis "Extended file name support for graphics (legacy package)")
    (description
     "The original @code{grffile} package extended the file name processing
of the @code{graphics} package to support a larger range of file names.  The
base LaTeX code now supports multiple dots and spaces, and this package by
default is a stub that just loads @code{graphicx}.")
    (license license:lppl1.3c+)))

(define-public texlive-sansmath
  (package
    (name "texlive-sansmath")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sansmath/" "tex/latex/sansmath/")
             (base32
              "1lqxfvmxan0c9s270jp8jw1a5s6ya1i4jlfp2xz2k4w0g0fryjpj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/sansmath")
    (synopsis "Maths in a Sans font")
    (description
     "The package defines a new math version Sans, and a command
@code{\\sansmath} that behaves somewhat like @code{\\boldmath}.")
    (license license:public-domain)))

(define-public texlive-sansmathfonts
  (package
    (name "texlive-sansmathfonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/sansmathfonts/"
                   "fonts/map/dvips/sansmathfonts/"
                   "fonts/source/public/sansmathfonts/"
                   "fonts/tfm/public/sansmathfonts/"
                   "fonts/type1/public/sansmathfonts/"
                   "fonts/vf/public/sansmathfonts/"
                   "tex/latex/sansmathfonts/")
             (base32
              "19349dxvqiinhsihn83yfhl6pgcvkd48l37w5jh59myx7sc6p8j6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-amsfonts texlive-cm texlive-ec texlive-metafont))
    (home-page "https://ctan.org/pkg/sansmathfonts")
    (synopsis "Correct placement of accents in sans-serif maths")
    (description
     "This package provides sans serif small caps and math fonts for use with
Computer Modern.")
    (license license:lppl1.3c)))

(define-public texlive-stringenc
  (package
    (name "texlive-stringenc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/stringenc/"
                   "source/latex/stringenc/"
                   "tex/generic/stringenc/")
             (base32
              "1pz9fgn3zc1brbpkw2kkphsv8q6vpvbn51n0smmfl1n2m97fni9j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/stringenc")
    (synopsis "Converting a string between different encodings")
    (description
     "This package provides @code{\\StringEncodingConvert} for converting a
string between different encodings.  Both LaTeX and plain-TeX are supported.")
    (license license:lppl1.3c+)))

(define-public texlive-svn-prov
  (package
    (name "texlive-svn-prov")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/svn-prov/" "source/latex/svn-prov/"
                   "tex/latex/svn-prov/")
             (base32
              "1w416cf1yb1m2j9y38002zq6rbhbmkafi7w100y9m9lrzya0ws06")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/svn-prov")
    (synopsis "Subversion variants of @code{\\Provides}... macros")
    (description
     "The package introduces Subversion variants of the standard LaTeX macros
@code{\\ProvidesPackage}, @code{\\ProvidesClass} and @code{\\ProvidesFile}
where the file name and date is extracted from Subversion Id keywords.  The
file name may also be given explicitly as an optional argument.")
    (license license:lppl)))

(define-public texlive-l3build
  (package
    (name "texlive-l3build")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/l3build/"
                   "doc/man/man1/l3build.1"
                   "doc/man/man1/l3build.man1.pdf"
                   "scripts/l3build/"
                   "source/latex/l3build/"
                   "tex/latex/l3build/")
             (base32
              "19iq80vj1glf35zcrspj1bnk6bf6yr3r3b2c5rgqhz58m2znsla0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "l3build.lua")))
    (propagated-inputs (list texlive-luatex))
    (home-page "https://ctan.org/pkg/l3build")
    (synopsis "Testing and building system for LaTeX")
    (description
     "The @code{l3build} module is designed to support the development of
high-quality LaTeX code by providing: a unit testing system, automated
typesetting of code sources, and a reliable packaging system for CTAN
releases.  The bundle consists of a Lua script to run the tasks and
a @file{.tex} file which provides the testing environment.")
    (license license:lppl1.3c+)))

(define-public texlive-luabidi
  (package
    (name "texlive-luabidi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luabidi/"
                   "tex/lualatex/luabidi/")
             (base32
              "1dwdiwsdfhgwpx8r2271i5kqphcpkh69y3rx1wxfr9hl17lzw2cp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luabidi")
    (synopsis "Bidi functions for LuaTeX")
    (description
     "The package attempts to emulate the XeTeX @code{bidi} package, in the
context of LuaTeX.")
    (license (list license:lppl1.3c license:expat))))

(define-public texlive-luacode
  (package
    (name "texlive-luacode")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luacode/"
                   "source/lualatex/luacode/"
                   "tex/lualatex/luacode/")
             (base32
              "1dyld5yb57p3j7wz591plbgjy7dk7ngn8cxw1lfmvx8iprgk1f8d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luacode")
    (synopsis "Helper for executing Lua code from within TeX")
    (description
     "Executing Lua code from within TeX with directlua can sometimes be
tricky: there is no easy way to use the percent character, counting
backslashes may be hard, and Lua comments don't work the way you expect.  The
package provides the @code{\\luaexec} command and the @code{luacode}
environments to help with these problems.")
    (license license:lppl1.3+)))

(define-public texlive-lualatex-math
  (package
    (name "texlive-lualatex-math")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/lualatex-math/"
                   "source/lualatex/lualatex-math/"
                   "tex/lualatex/lualatex-math/")
             (base32
              "0rzzlq6a0c7sj1x83wqn5iwaiz3w9prcpz4lqbjlkgr7my4m052z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox texlive-filehook))
    (home-page "https://ctan.org/pkg/lualatex-math")
    (synopsis "Fixes for mathematics-related LuaLaTeX issues")
    (description
     "The package patches a few commands of the LaTeX2e kernel and the
@code{amsmath} and @code{mathtools} packages to be more compatible with the
LuaTeX engine.  It is only meaningful for LuaLaTeX documents containing
mathematical formulas, and does not exhibit any new functionality.  The fixes
are mostly moved from the @code{unicode-math} package to this package since
they are not directly related to Unicode mathematics typesetting.")
    (license license:lppl1.3c)))

(define-public texlive-lualibs
  (package
    (name "texlive-lualibs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/lualibs/" "source/luatex/lualibs/"
                   "tex/luatex/lualibs/")
             (base32
              "0x53z6072z8qpp5fh2g9blz95zg20906k82jk6hz1hibv70is0pk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip texlive-pdftex))
    (home-page "https://ctan.org/pkg/lualibs")
    (synopsis "Additional Lua functions for LuaTeX macro programmers")
    (description
     "Lualibs is a collection of Lua modules useful for general programming.
The bundle is based on lua modules shipped with ConTeXt, and made available in
this bundle for use independent of ConTeXt.")
    (license license:gpl2)))

(define-deprecated-package texlive-luatex-lualibs texlive-lualibs)

(define-public texlive-lua-alt-getopt
  (package
    (name "texlive-lua-alt-getopt")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/support/lua-alt-getopt/"
                   "scripts/lua-alt-getopt/")
             (base32
              "0cizxzn33n3pn98xkqnxb8s6vdwkar3xrwhraqrs05pjfdn9d4wz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/lua-alt-getopt")
    (synopsis "Process application arguments as @code{getopt_long}")
    (description
     "This package provides a Lua module for processing application arguments
in the same way as BSD/GNU @code{getopt_long(3)} functions do.")
    (license license:expat)))

(define-public texlive-luatex
  (package
    (name "texlive-luatex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/base/"
                   "doc/man/man1/dviluatex.1"
                   "doc/man/man1/dviluatex.man1.pdf"
                   "doc/man/man1/luatex.1"
                   "doc/man/man1/luatex.man1.pdf"
                   "doc/man/man1/texlua.1"
                   "doc/man/man1/texlua.man1.pdf"
                   "doc/man/man1/texluac.1"
                   "doc/man/man1/texluac.man1.pdf"
                   "tex/generic/config/luatex-unicode-letters.tex"
                   "tex/generic/config/luatexiniconfig.tex"
                   "web2c/texmfcnf.lua")
             (base32
              "14ad9qr5dn0796f8gqbry5axdm2mciibcgzva8kas1vrqs5a9f19")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-bin? #f
           #:create-formats #~(list "dviluatex" "luatex")))
    (propagated-inputs
     (list texlive-cm
           texlive-etex
           texlive-hyphen-complete
           texlive-knuth-lib
           texlive-plain
           texlive-tex-ini-files
           texlive-unicode-data))
    (home-page "https://ctan.org/pkg/luatex")
    (synopsis "Extended version of pdfTeX using Lua")
    (description
     "LuaTeX is an extended version of pdfTeX using Lua as an embedded
scripting language.  The LuaTeX project's main objective is to provide an open
and configurable variant of TeX while at the same time offering downward
compatibility.  LuaTeX uses Unicode (as UTF-8) as its default input encoding,
and is able to use modern (OpenType) fonts (for both text and mathematics).
It should be noted that LuaTeX is still under development; its specification
has been declared stable, but absolute stability may not in practice be
assumed.")
    (license license:gpl2)))

(define-public texlive-luatexbase
  (package
    (name "texlive-luatexbase")
    (version (number->string %texlive-revision))
    (source (texlive-origin

             name version
             (list "doc/luatex/luatexbase/"
                   "source/luatex/luatexbase/"
                   "tex/luatex/luatexbase/")
             (base32
              "1nz2k9czqdmn08v75qa2bwanvcvyp9jmqcgwaxcy4fy4mpbrn8ra")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-ctablestack))
    (home-page "https://ctan.org/pkg/luatexbase")
    (synopsis "Basic resource management for LuaTeX code")
    (description
     "The LaTeX kernel builds in support for LuaTeX functionality, also
available as @file{ltluatex.tex} for users of plain TeX and those with older
LaTeX kernel implementations.  This support is based on ideas taken from the
original @code{luatexbase} package, but there are interface differences.  This
stub package provides a compatibility layer to allow existing packages to
upgrade smoothly to the new support structure.")
    (license license:lppl1.3+)))

(define-public texlive-luatex85
  (package
    (name "texlive-luatex85")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/luatex85/"
                   "source/generic/luatex85/"
                   "tex/generic/luatex85/")
             (base32
              "02j42g9c5smgbrk898fpvrgqlxqcqi8xch23awvnbvg6y54cs573")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luatex85")
    (synopsis "pdfTeX aliases for LuaTeX")
    (description
     "The package provides emulation of pdfTeX primitives for LuaTeX v0.85+.")
    (license license:lppl1.3+)))

(define-public texlive-luahbtex
  (package
    (name "texlive-luahbtex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/luahbtex.1"
                   "doc/man/man1/luahbtex.man1.pdf")
             (base32
              "1hfawh7vig7jsmd1y0qlbn80x6770q56sqd9rx638js4p5a4di3l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:create-formats #~(list "luahbtex")))
    (propagated-inputs
     (list texlive-cm
           texlive-etex
           texlive-hyphen-complete
           texlive-knuth-lib
           texlive-luatex
           texlive-plain
           texlive-tex-ini-files
           texlive-unicode-data))
    (home-page "https://ctan.org/pkg/luahbtex")
    (synopsis "LuaTeX with HarfBuzz library for glyph shaping")
    (description
     "LuaHBTeX is a LuaTeX variant that can use the HarfBuzz engine for glyph
shaping, instead of LuaTeX's built-in shaper.")
    (license (package-license texlive-luatex))))

(define-public texlive-lua-uni-algos
  (package
    (name "texlive-lua-uni-algos")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/lua-uni-algos/"
                   "tex/luatex/lua-uni-algos/")
             (base32
              "1dx70msqkj101mgj88b7fmb28bghlrrrcy5v66m74gqb5i41dnc2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (home-page "https://ctan.org/pkg/lua-uni-algos")
    (synopsis "Unicode algorithms for LuaTeX")
    (description
     "Lua code working with Unicode data has to deal with quite some
challenges.  For example there are many canonically equivalent sequences which
should be treated in the same way, and even identifying a single character
becomes quite different once you have to deal with all kinds of combining
characters, emoji sequences and syllables in different scripts.  Therefore
@code{lua-uni-algos} wants to build a collection of small libraries
implementing algorithms to deal with lots of the details in Unicode, such that
authors of LuaTeX packages can focus on their actual functionality instead of
having to fight against the peculiarities of Unicode.  Given that this package
provides Lua modules, it is only useful in Lua(HB)TeX.  Additionally, it
expects an up-to-date version of the unicode-data package to be present.  This
package is intended for package authors only; no user-level functionality
provided.")
    (license license:lppl1.3+)))

(define-public texlive-luaotfload
  (package
    (name "texlive-luaotfload")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luaotfload/"
                   "doc/man/man1/luaotfload-tool.1"
                   "doc/man/man1/luaotfload-tool.man1.pdf"
                   "doc/man/man5/luaotfload.conf.5"
                   "doc/man/man5/luaotfload.conf.man5.pdf"
                   "scripts/luaotfload/"
                   "source/luatex/luaotfload/"
                   "tex/luatex/luaotfload/")
             (base32
              "0x0vigy6g5sf9n5113p8w885qy9w72skay09p3x6p510lpzaafj5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-bin? #f
           #:link-scripts #~(list "luaotfload-tool.lua")))
    (propagated-inputs
     (list texlive-lm
           texlive-lua-alt-getopt
           texlive-lua-uni-algos
           texlive-lualibs))
    (home-page "https://ctan.org/pkg/luaotfload")
    (synopsis "OpenType font loader for LuaTeX")
    (description
     "Luaotfload is an adaptation of the ConTeXt font loading system for the
Plain and LaTeX formats.  It allows OpenType fonts to be loaded with font
features accessible using an extended font request syntax while providing
compatibilitywith XeTeX.  By indexing metadata in a database it facilitates
loading fonts by their proper names instead of file names.")
    (license license:gpl2)))

(define-deprecated-package texlive-luatex-luaotfload texlive-luaotfload)

(define-public texlive-amsmath
  (package
    (name "texlive-amsmath")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/amsmath/" "source/latex/amsmath/"
                   "tex/latex/amsmath/")
             (base32
              "1x97wjj664hvj73k2ffg7xmllaqm25ikjm8rcfjs2q920f5ixw2h")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/amsmath")
    (synopsis "AMS mathematical facilities for LaTeX")
    (description
     "This is the principal package in the AMS-LaTeX distribution.  It adapts
for use in LaTeX most of the mathematical features found in AMS-TeX; it is
highly recommended as an adjunct to serious mathematical typesetting in LaTeX.
When amsmath is loaded, AMS-LaTeX packages @code{amsbsy} (for bold symbols),
@code{amsopn} (for operator names) and @code{amstext} (for text embedded in
mathematics) are also loaded.  This package is part of the LaTeX required
distribution; however, several contributed packages add still further to its
appeal; examples are @code{empheq}, which provides functions for decorating
and highlighting mathematics, and @code{ntheorem}, for specifying theorem (and
similar) definitions.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-amsmath texlive-amsmath)

(define-public texlive-manfnt
  (package
    (name "texlive-manfnt")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/manfnt/" "source/latex/manfnt/"
                   "tex/latex/manfnt/")
             (base32
              "0iwka406hkxb85yqg4kwr81483s3h250way1pmbfb67vl3x4p60x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/manfnt")
    (synopsis "LaTeX support for the TeX book symbols")
    (description
     "This package provides a LaTeX package for easy access to the symbols of
the Knuth's manual font, such as the Dangerous Bend and Manual-errata Arrow.")
    (license license:lppl)))

(define-public texlive-manfnt-font
  (package
    (name "texlive-manfnt-font")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/afm/hoekwater/manfnt-font/"
                   "fonts/map/dvips/manfnt-font/"
                   "fonts/type1/hoekwater/manfnt-font/")
             (base32
              "1cdd2a3xj00bwsby9z5iz5f2iy2iwsjl35gcack9q83hacbf3ssb")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/manual")
    (synopsis "Knuth's manual fonts")
    (description
     "This package provides Metafont (by Donald Knuth) and Adobe Type 1 (by
Taco Hoekwater) versions of the font containing the odd symbols Knuth uses in
his books.  LaTeX support is available using the @code{manfnt} package.")
    (license license:knuth)))

(define-public texlive-mathdots
  (package
    (name "texlive-mathdots")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/mathdots/"
                   "source/generic/mathdots/"
                   "tex/generic/mathdots/")
             (base32
              "1jaffj343p1chdxs2g7s6lpckvihk0jfw22nw0vmijyjxfiy9yg0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mathdots")
    (synopsis "Commands to produce dots in math that respect font size")
    (description
     "Mathdots redefines @code{\\ddots} and @code{\\vdots}, and defines
@code{\\iddots}.  The dots produced by @code{\\iddots} slant in the opposite
direction to @code{\\ddots}.  All the commands are designed to change size
appropriately in scripts, as well as in response to LaTeX size changing
commands.  The commands may also be used in plain TeX.")
    (license license:lppl)))

(define-public texlive-amscls
  (package
    (name "texlive-amscls")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/amscls/" "doc/latex/amscls/"
                   "source/latex/amscls/" "tex/latex/amscls/")
             (base32
              "1chy1rqwici66p9brphb3gsprmcyhia9cvm5fn9wb5a9cchxqa08")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/amscls")
    (synopsis "AMS document classes for LaTeX")
    (description
     "This bundle contains three AMS classes: @code{amsart} (for writing
articles for the AMS), @code{amsbook} (for books) and @code{amsproc} (for
proceedings), together with some supporting material.  The material is made
available as part of the AMS-LaTeX distribution.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-amscls texlive-amscls)

(define-public texlive-babel
  (package
    (name "texlive-babel")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/babel/"
                   "makeindex/babel/"
                   "source/latex/babel/"
                   "tex/generic/babel/")
             (base32
              "18q99xgga4448vk7wf5r5ry79sx5ymqx0zd7v7l1c4wyyiv4riw9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; This package tries to produce babel.aux twice but refuses to
          ;; overwrite the first one.
          (add-before 'build 'fix-ins
            (lambda _
              (substitute* "source/latex/babel/babel.ins"
                (("askonceonly") "askforoverwritefalse"))))
          (add-before 'build 'generate-locales
            (lambda _
              (let ((locale-directory (string-append (getcwd) "/build/locale")))
                (mkdir-p locale-directory)
                (with-directory-excursion "source/latex/babel/"
                  (invoke "unzip" "locale.zip" "-d" locale-directory))))))))
    (native-inputs (list texlive-docstrip texlive-pdftex unzip))
    (home-page "https://www.ctan.org/pkg/babel")
    (synopsis "Multilingual support for Plain TeX or LaTeX")
    (description
     "The package manages culturally-determined typographical (and other)
rules, and hyphenation patterns for a wide range of languages.  A document may
select a single language to be supported, or it may select several, in which
case the document may switch from one language to another in a variety of
ways.  Babel uses contributed configuration files that provide the detail of
what has to be done for each language.  Users of XeTeX are advised to use the
polyglossia package rather than Babel.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-babel texlive-babel)

(define-public texlive-babelbib
  (package
    (name "texlive-babelbib")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/babelbib/"
                   "doc/bibtex/babelbib/" "tex/latex/babelbib/")
             (base32
              "0q65qy0jmy98zfaha26b09hzr7v02jn8i6942y51hfaphbgh8kpx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babelbib")
    (synopsis "Multilingual bibliographies")
    (description
     "This package enables the user to generate multilingual bibliographies in
cooperation with @code{babel}.  Two approaches are possible: each citation may
be written in another language, or the whole bibliography can be typeset in
a language chosen by the user.  In addition, the package supports commands to
change the typography of the bibliographies.")
    (license license:lppl1.0+)))

(define-public texlive-barcodes
  (package
    (name "texlive-barcodes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/barcodes/"
                   "fonts/source/public/barcodes/"
                   "fonts/tfm/public/barcodes/"
                   "source/latex/barcodes/"
                   "tex/latex/barcodes/")
             (base32
              "1bxvmmmb4x0zpdkqa399rwdhz6yamfzx2kn2i7a8vz172q0b2msc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-metafont))
    (home-page "https://ctan.org/pkg/barcodes")
    (synopsis "Fonts for making barcodes")
    (description
     "The package deals with EAN barcodes; Metafont sources for fonts are
provided, and a set of examples; for some codes, a small Perl script is
needed.")
    (license license:lppl)))

(define-public texlive-bbcard
  (package
    (name "texlive-bbcard")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/bbcard/" "metapost/bbcard/")
             (base32
              "1f1d3l85d0308izqpydmqmb4vpw47lqx1pwjiq14npjs8zfzkp18")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bbcard")
    (synopsis "Bullshit bingo, calendar and baseball-score cards")
    (description
     "This set contains three jiffy packages for creating cards of various
sorts with MetaPost.")
    (license license:public-domain)))

(define-public texlive-blockdraw-mp
  (package
    (name "texlive-blockdraw-mp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/blockdraw_mp/"
                   "metapost/blockdraw_mp/")
             (base32
              "1z0cgjslx4kx8fspf3i5h4idxlzhgpm14i16vq88c96z8cdjb2xy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/blockdraw_mp")
    (synopsis "Block diagrams and bond graphs, with MetaPost")
    (description
     "This package provides a set of simple MetaPost macros to draw block
diagrams and bond graphs. While the task is not itself difficult to program,
it is felt that many users will be happy to have a library for the job.")
    (license license:lppl)))

(define-public texlive-blopentype
  (package
    (name "texlive-blopentype")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/blopentype/"
                   "tex/luatex/blopentype/")
             (base32
              "1zz19a0glxad0bq8kpa2qlrmgysdlda1hpxacf3m42m9n3byrb75")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-gates texlive-texapi texlive-yax))
    (home-page "https://ctan.org/pkg/blopentype")
    (synopsis "Basic LuaTeX OpenType handler")
    (description
     "This is a basic LuaTeX OpenType handler, based on Paul Isambert's PiTeX
code.")
    (license (list license:lppl1.3c license:expat))))

(define-public texlive-bookcover
  (package
    (name "texlive-bookcover")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/bookcover/"
                   "source/latex/bookcover/"
                   "tex/latex/bookcover/")
             (base32
              "0m5km26diwv7xij4r64c3gk2hl8xc22lrbcccqlrk1fhdlz3i8zb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bookcover")
    (synopsis "Class for book covers and dust jackets")
    (description "This class helps typesetting book covers and dust jackets.")
    (license license:lppl1.2+)))

(define-public texlive-bpolynomial
  (package
    (name "texlive-bpolynomial")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/bpolynomial/"
                   "metapost/bpolynomial/")
             (base32
              "1p4c3xc55zij85yy1jhmj7jrkxss935qx5zdxbm79mjlxx1aq1q0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bpolynomial")
    (synopsis "Drawing polynomial functions of up to order 3")
    (description
     "This MetaPost package helps plotting polynomial and root functions up to
order three.  The package provides macros to calculate Bezier curves exactly
matching a given constant, linear, quadratic or cubic polynomial, or square or
cubic root function.  In addition, tangents on all functions and derivatives
of polynomials can be calculated.")
    (license license:lppl)))

(define-public texlive-businesscard-qrcode
  (package
    (name "texlive-businesscard-qrcode")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/businesscard-qrcode/"
                   "tex/xelatex/businesscard-qrcode/")
             (base32
              "0r942lv7mymi9wmmshjgf3vnz2n38m91dzqzrg0dg6bckb09dhmf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/businesscard-qrcode")
    (synopsis "Business cards with QR-Code")
    (description
     "This package generates a configurable business card or visiting card
with full vcard as QR-Code, ready to send to online printers.  You can specify
the exact size of the paper and the content within the paper, including
generation of crop marks.")
    (license license:lgpl3)))

(define-public texlive-cmarrows
  (package
    (name "texlive-cmarrows")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/cmarrows/"
                   "metapost/cmarrows/")
             (base32
              "1awyf474n4f6fivb7ih9mpmz6641f815in8qrg2biagys3czsq96")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cmarrows")
    (synopsis "MetaPost arrows and braces in the Computer Modern style")
    (description
     "This MetaPost package contains macros to draw arrows and braces in the
Computer Modern style.")
    (license license:lppl)))

(define-public texlive-cmexb
  (package
    (name "texlive-cmexb")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cmexb/" "fonts/map/dvips/cmexb/"
                   "fonts/tfm/public/cmexb/"
                   "fonts/type1/public/cmexb/")
             (base32
              "1fwnxzlkcf58n1f91vz8xbcp8nmhqmhjqhswgkzvyrd6lnp5gzdk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cmexb")
    (synopsis "@code{cmexb10} metrics and Type 1")
    (description
     "This package provides Computer Modern Math Extension bold, metrics and
@file{.pfb} file.")
    (license license:public-domain)))   ;see "README-cmexb"

(define-public texlive-cqubeamer
  (package
    (name "texlive-cqubeamer")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/cqubeamer/"
                   "tex/xelatex/cqubeamer/")
             (base32
              "13ma7i3z64w59m2hkrh68j3vqa2k93hhb2lmarqb4pkhr72vk4s7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cqubeamer")
    (synopsis "LaTeX Beamer Template for Chongqing University")
    (description
     "This package provides a LaTeX beamer template designed for researchers
of Chongqing University.  It can be used for academic reports, conferences, or
thesis defense, and can be helpful for delivering a speech.  It should be used
with the XeTeX engine.")
    (license (list license:expat license:cc-by4.0))))

(define-public texlive-cs
  (package
    (name "texlive-cs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/enc/dvips/cs/"
                   "fonts/map/dvips/cs/"
                   "fonts/source/public/cs/"
                   "fonts/tfm/cs/cs-a35/"
                   "fonts/tfm/cs/cs-charter/"
                   "fonts/tfm/public/cs/"
                   "fonts/type1/public/cs/"
                   "fonts/vf/cs/cs-a35/"
                   "fonts/vf/cs/cs-charter/")
             (base32
              "0nzzcg1yvbslhqm5lsfcpqh6sbzkmnmmgyakg9l8855qpa8g9bf3")))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      ;; FIXME: The phase fails for multiple font files with error: "cannot
      ;; find cmb12".
      #~(modify-phases %standard-phases
          (delete 'generate-font-metrics))))
    (native-inputs (list texlive-cm texlive-metafont))
    (propagated-inputs (list texlive-cmexb))
    (home-page "https://ctan.org/pkg/csfonts")
    (synopsis "Czech/Slovak-tuned Computer Modern fonts")
    (description "This package provides Czech/Slovak-tuned Computer Modern
fonts in the Metafont format; Type 1 format versions (csfonts-t1) are also
available.")
    (license license:gpl2+)))           ;see fonts/source/public/cs/cscode.mf

;;; Note: if this package is modified, its name must be changed to comply with
;;; its license.
(define-public texlive-csplain
  (package
    (name "texlive-csplain")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/csplain/base/"
                   "tex/csplain/fonts/"
                   "tex/csplain/opmac/")
             (base32
              "14z479gkiwgw17pdghrxh5q0rlxnij7ccj49kgf3macwgmh5lm0r")))
    (arguments
     (list #:create-formats #~(list "csplain" "luacsplain" "pdfcsplain")))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-cm
           texlive-cs
           texlive-enctex
           texlive-hyphen-complete
           texlive-luatex
           texlive-luatex85
           texlive-plain
           texlive-tex
           texlive-tex-ini-files))
    (home-page "https://petr.olsak.net/csplain-e.html")
    (synopsis "Plain TeX multilanguage support")
    (description
     "CSplain is a small extension of basic Plain TeX macros from which the
formats @code{csplain} and @code{pdfcsplain} can be generated.  It supports:
hyphenation of words for 50+ languages, simple and powerful font loading
system (various sizes of fonts), TeX, pdfTeX, XeTeX and LuaTeX engines, math
fonts simply loaded with full amstex-like features, three internal
encodings (IL2 for Czech/Slovak languages, T1 for many languages with latin
alphabet and Unicode in new TeX engines), natural UTF-8 input in pdfTeX using
encTeX without any active characters, Czech and Slovak special typesetting
features.  An important part of the package is OPmac, which implements most of
LaTeX's features (sectioning, font selection, color, hyper reference and URLs,
bibliography, index, table of contents, tables, etc.) by Plain TeX macros.
The OPmac macros can generate a bibliography without any external program.")
    ;; This custom permissive license includes as a redistribution condition
    ;; that says the package must be renamed from 'csplain' if it is modified.
    (license (license:non-copyleft "file:///tex/csplain/base/csplain.ini"))))

(define-public texlive-ctable
  (package
    (name "texlive-ctable")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ctable/" "source/latex/ctable/"
                   "tex/latex/ctable/")
             (base32
              "1x624vz1gv8bsdafwhx89zaic7f2mz9vlvwjpv8fwzjhjwx4lxmv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ctable")
    (synopsis "Flexible typesetting of table and figure floats")
    (description
     "This package provides commands to typeset centered, left- or
right-aligned table and (multiple-)figure floats, with footnotes.")
    (license license:lppl)))

(define-public texlive-babel-english
  (package
    (name "texlive-babel-english")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-english/"
                   "source/generic/babel-english/"
                   "tex/generic/babel-english/")
             (base32
              "0fh8xxxh79za80yvgypf8clzj0lk237lfyqfkl233id9rlias08d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-english")
    (synopsis "Babel support for English")
    (description
     "The package provides the language definition file for support of
English in @code{babel}.  Care is taken to select british hyphenation patterns
for British English and Australian text, and default (american) patterns for
Canadian and USA text.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-babel-english texlive-babel-english)

(define-public texlive-babel-french
  (package
    (name "texlive-babel-french")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-french/"
                   "source/generic/babel-french/"
                   "tex/generic/babel-french/")
             (base32
              "1p0gprdfvfincbkvqbc4bpyx6jf483k798hz5psg04rhmx5hi4wl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-french")
    (synopsis "Babel contributed support for French")
    (description
     "The package establishes French conventions in a document (or a subset of
the conventions, if French is not the main language of the document).")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-babel-french texlive-babel-french)

(define-public texlive-babel-german
  (package
    (name "texlive-babel-german")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-german/"
                   "source/generic/babel-german/"
                   "tex/generic/babel-german/")
             (base32
              "0iwnn35xnpczi2gxrzrgyilh30qbnj6w05p3q0gvcmnisawfva9q")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-german")
    (synopsis "Babel support for documents written in German")
    (description
     "This bundle is an extension to the babel package for multilingual typesetting.
It provides all the necessary macros, definitions and settings to typeset
German documents.  The bundle includes support for the traditional and
reformed German orthography as well as for the Austrian and Swiss varieties of
German.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-babel-german texlive-babel-german)

(define-public texlive-babel-swedish
  (package
    (name "texlive-babel-swedish")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-swedish/"
                   "source/generic/babel-swedish/"
                   "tex/generic/babel-swedish/")
             (base32
              "0qi2rzhlxikabrk9n0096inbczgp5hwghvy5zn0mph8zmsxlfbdf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-swedish")
    (synopsis "Babel support for typesetting Swedish")
    (description
     "The package provides the language definition file for Swedish.")
    (license license:lppl1.3+)))

(define-public texlive-cyrillic-bin
  (package
    (name "texlive-cyrillic-bin")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/rubibtex.1"
                   "doc/man/man1/rubibtex.man1.pdf"
                   "doc/man/man1/rumakeindex.1"
                   "doc/man/man1/rumakeindex.man1.pdf"
                   "scripts/texlive-extra/rubibtex.sh"
                   "scripts/texlive-extra/rumakeindex.sh")
             (base32
              "09l5f7l91ph6sqfp2ia3yn23pa3s4cyfgyn020ncqvapg00s0mmg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:link-scripts #~(list "rubibtex.sh" "rumakeindex.sh")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'patch-shell-scripts
            (lambda _
              (with-directory-excursion "scripts/texlive-extra/"
                (with-fluids ((%default-port-encoding "ISO-8859-1"))
                  (substitute* (list "rubibtex.sh" "rumakeindex.sh")
                    (("\\b(basename|cat|mkdir|rm|sed)\\b" _ command)
                     (which command))))))))))
    (home-page "https://ctan.org/pkg/cyrillic-bin")
    (synopsis "Cyrillic BibTeX and MakeIndex")
    (description
     "This package provides scripts for Cyrillic versions of BibTeX and
MakeIndex.")
    (license license:public-domain)))

(define-public texlive-cyrillic
  (package
    (name "texlive-cyrillic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/cyrillic/"
                   "source/latex/cyrillic/"
                   "tex/latex/cyrillic/")
             (base32
              "08v670f7s74klnac7pzqsad9m4jsxfkckzkswxb94xxd61kanzdx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:tex-engine "tex"
           #:tex-format #f))
    (native-inputs (list texlive-docstrip))
    (propagated-inputs (list texlive-cyrillic-bin))
    (home-page "https://ctan.org/pkg/cyrillic")
    (synopsis "Support for Cyrillic fonts in LaTeX")
    (description
     "This bundle of macros files provides macro support (including font
encoding macros) for the use of Cyrillic characters in fonts encoded under the
T2* and X2 encodings.  These encodings cover (between them) pretty much every
language that is written in a Cyrillic alphabet.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-cyrillic texlive-cyrillic)

(define-public texlive-passivetex
  (package
    (name "texlive-passivetex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/xmltex/passivetex/")
             (base32
              "1h49v6sqbm27isfwwcql9dzxn4vmcn2algkqh7f1pzj860xw3ygn")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/passivetex")
    (synopsis "Support package for XML/SGML typesetting")
    (description
     "This is a set of packages providing XML parsing, UTF-8 parsing,
Unicode entities, and common formatting object definitions for JadeTeX.")
    ;; License given in "tex/xmltex/passivetex/mlnames.sty".
    (license license:lppl1.0+)))

(define-public texlive-pict2e
  (package
    (name "texlive-pict2e")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pict2e/"
                   "source/latex/pict2e/"
                   "tex/latex/pict2e/")
             (base32
              "0pazv1khsgjhxc673qrhjrbzlkgmcj53qccb9hw7ygdajxrjc2ba")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pict2e")
    (synopsis "New implementation of @code{picture} commands")
    (description
     "This package extends the existing LaTeX @code{picture} environment,
using the familiar technique (the @code{graphics} and @code{color} packages)
of driver files (at present, drivers for dvips, pdfTeX, LuaTeX, XeTeX, VTeX,
dvipdfm, and dvipdfmx are available).  The package documentation has a fair
number of examples of use, showing where things are improved by comparison
with the LaTeX @code{picture} environment.")
    (license license:lppl1.3+)))

(define-public texlive-psnfss
  (package
    (name "texlive-psnfss")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/psnfss/"
                   "fonts/map/dvips/psnfss/"
                   "source/latex/psnfss/"
                   "tex/latex/psnfss/")
             (base32
              "17zxqz32ky99z22yaqayg9ih8lyaswi97d34jykc0s12w4k4i97c")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-cm))
    (propagated-inputs
     (list texlive-graphics texlive-symbol texlive-zapfding))
    (home-page "https://www.ctan.org/pkg/psnfss")
    (synopsis "Font support for common PostScript fonts")
    (description
     "The PSNFSS collection includes a set of files that provide a complete
working setup of the LaTeX font selection scheme (NFSS2) for use with common
PostScript fonts.  The base set of text fonts covered by PSNFSS includes the
AvantGarde, Bookman, Courier, Helvetica, New Century Schoolbook, Palatino,
Symbol, Times Roman and Zapf Dingbats fonts.  In addition, the fonts Bitstream
Charter and Adobe Utopia are covered.  Separate packages are provided to load
each font for use as the main text font.  The package @code{helvet} allows
Helvetica to be loaded with its size scaled to something more appropriate for
use as a Sans-Serif font to match Times, while @code{pifont} provides the
means to select single glyphs from symbol fonts.  The bundle as a whole is
part of the LaTeX required set of packages.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-psnfss texlive-psnfss)

(define-public texlive-updmap.cfg
  (lambda* (#:optional (packages '()))
    "Return a 'texlive-updmap.cfg' package which contains the fonts map
configuration of a base set of packages plus PACKAGES."
    (let ((default-packages
            (list texlive-bin
                  texlive-cm-super
                  texlive-dvips
                  texlive-fontname
                  texlive-kpathsea
                  texlive-latex-bin
                  texlive-latex-fonts
                  texlive-metafont
                  ;; LaTeX packages from the "required" set.
                  texlive-amsmath
                  texlive-amscls
                  texlive-babel
                  texlive-babel-english
                  texlive-cyrillic
                  texlive-psnfss
                  texlive-tools)))
      (package
        (version (number->string %texlive-revision))
        (source (package-source texlive-scripts))
        (name "texlive-updmap.cfg")
        (build-system copy-build-system)
        (arguments
         (list
          #:modules '((guix build copy-build-system)
                      (guix build utils)
                      (ice-9 popen)
                      (ice-9 textual-ports))
          #:install-plan
          #~'(("texmf-dist/web2c/updmap.cfg" "share/texmf-config/web2c/")
              ("texmf-dist/web2c/map" "share/texmf-dist/fonts/map"))
          #:phases
          #~(modify-phases %standard-phases
              (add-before 'install 'regenerate-updmap.cfg
                (lambda _
                  (with-directory-excursion "texmf-dist/web2c"
                    (make-file-writable "updmap.cfg")

                    ;; Disable unavailable map files.
                    (let* ((port (open-pipe* OPEN_WRITE "updmap-sys"
                                             "--syncwithtrees"
                                             "--nohash"
                                             "--cnffile" "updmap.cfg")))
                      (display "Y\n" port)
                      (when (not (zero? (status:exit-val (close-pipe port))))
                        (error "failed to filter updmap.cfg")))

                    ;; Set TEXMFSYSVAR to a sane and writable value; updmap fails
                    ;; if it cannot create its log file there.
                    (setenv "TEXMFSYSVAR" (getcwd))

                    ;; Generate maps.
                    (invoke "updmap-sys"
                            "--cnffile"           "updmap.cfg"
                            "--dvipdfmxoutputdir" "map/dvipdfmx/updmap/"
                            "--dvipsoutputdir"    "map/dvips/updmap/"
                            "--pdftexoutputdir"   "map/pdftex/updmap/")))))))
        (native-inputs (list texlive-scripts))
        (propagated-inputs (map (lambda (package)
                                  (list (package-name package) package))
                                (append default-packages packages)))
        (home-page (package-home-page texlive-bin))
        (synopsis "TeX Live fonts map configuration")
        (description "This package contains the fonts map configuration file
generated for the base TeX Live packages as well as, optionally, user-provided
ones.")
        (license (delete-duplicates
                  (fold (lambda (package result)
                          (match (package-license package)
                            ((lst ...)
                             (append lst result))
                            ((? license:license? license)
                             (cons license result))))
                        '()
                        (append default-packages packages))))))))

(define-deprecated/alias texlive-union texlive-updmap.cfg)
(export texlive-union)

;; For use in package definitions only
(define-public texlive-tiny
  (package
    (inherit (texlive-updmap.cfg))
    (name "texlive-tiny")
    (description "This is a very limited subset of the TeX Live distribution.
It includes little more than the required set of LaTeX packages.")))

(define-public texlive-tipa
  (package
    (name "texlive-tipa")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/tipa/"
                   "fonts/map/dvips/tipa/"
                   "fonts/source/public/tipa/"
                   "fonts/tfm/public/tipa/"
                   "fonts/type1/public/tipa/"
                   "tex/latex/tipa/")
             (base32
              "11gi7yhq2lnfgvqa29i0sidi5mwkzpja5ggdcpvqwv4xljf4vpvh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/tipa")
    (synopsis "Fonts and macros for IPA phonetics characters")
    (description
     "These fonts are considered the ultimate answer to IPA typesetting.  The
encoding of these 8-bit fonts has been registered as LaTeX standard encoding
T3, and the set of addendum symbols as encoding TS3.  Times-like Adobe Type
1 versions are provided for both the T3 and the TS3 fonts.")
    (license license:lppl)))

(define-public texlive-amsrefs
  (package
    (name "texlive-amsrefs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bib/amsrefs/" "bibtex/bst/amsrefs/"
                   "doc/latex/amsrefs/" "source/latex/amsrefs/"
                   "tex/latex/amsrefs/")
             (base32
              "12la66vz5ic6jc1cy96b2zh2fxsbaii9kbs4wrz1ii8v508wdkhv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/amsrefs")
    (synopsis "LaTeX-based replacement for BibTeX")
    (description
     "Amsrefs is a LaTeX package for bibliographies that provides an
archival data format similar to the format of BibTeX database files, but
adapted to make direct processing by LaTeX easier.  The package can be used
either in conjunction with BibTeX or as a replacement for BibTeX.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-amsrefs texlive-amsrefs)

(define-public texlive-bigfoot
  (package
    (name "texlive-bigfoot")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/bigfoot/"
                   "source/latex/bigfoot/"
                   "tex/latex/bigfoot/")
             (base32
              "140b4bbjcgajd1flznmi3ga6lx5pna2nxybr2dqm9515lny8gwf0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bigfoot")
    (synopsis "Footnotes for critical editions")
    (description
     "The package aims to provide a one-stop solution to requirements for
footnotes.  It offers multiple footnote apparatus superior to that of
@code{manyfoot}.  Footnotes can be formatted in separate paragraphs, or be run
into a single paragraph.  Note that the majority of the @code{bigfoot}
package's interface is identical to that of @code{manyfoot}; users should seek
information from that package's documentation.

The @code{bigfoot} bundle also provides the @code{perpage} and @code{suffix}
packages.")
    (license license:gpl2+)))

(define-deprecated-package texlive-latex-bigfoot texlive-bigfoot)

(define-public texlive-blindtext
  (package
    (name "texlive-blindtext")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/blindtext/"
                   "source/latex/blindtext/"
                   "tex/latex/blindtext/")
             (base32
              "1gakawih3mzm5jh01kb44sjpsa4r8c3zwzig5bac37g4ha2vqska")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/blindtext")
    (synopsis "Producing blind text for testing")
    (description
     "The package provides the commands @code{\\blindtext} and
@code{\\Blindtext} for creating \"blind\" text useful in testing new classes
and packages, and @code{\\blinddocument}, @code{\\Blinddocument} for creating
an entire random document with sections, lists, mathematics, etc.  The package
supports three languages, @code{english}, @code{(n)german} and @code{latin};
the @code{latin} option provides a short \"lorem ipsum\" (for a fuller \"lorem
ipsum\" text, see the @code{lipsum} package).")
    (license license:lppl)))

(define-deprecated-package texlive-latex-blindtext texlive-blindtext)

(define-public texlive-dinbrief
  (package
    (name "texlive-dinbrief")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/dinbrief/"
                   "source/latex/dinbrief/"
                   "tex/latex/dinbrief/")
             (base32
              "0l6mmn3y07xglmh3h5f7pnpmyacqb2g6nqgq3q1p6k97kf708c5s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build
            (lambda _
              (with-fluids ((%default-port-encoding "ISO-8859-1"))
                (with-directory-excursion "source/latex/dinbrief"
                  (delete-file "dinbrief.drv")
                  (substitute* "dinbrief.dtx"
                    (("zur Verf.+ung. In der Pr\"aambel")
                     "zur Verf\"ung. In der Pr\"aambel")))))))))
    (home-page "https://ctan.org/pkg/dinbrief")
    (synopsis "German letter DIN style")
    (description
     "This package implements a document layout for writing letters according
to the rules of DIN (Deutsches Institut für Normung, German standardisation
institute).  A style file for LaTeX 2.09 (with limited support of the
features) is part of the package.  Since the letter layout is based on a
German standard, the user guide is written in German, but most macros have
English names from which the user can recognize what they are used for.  In
addition there are example files showing how letters may be created with the
package.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-dinbrief texlive-dinbrief)

(define-public texlive-draftwatermark
  (package
    (name "texlive-draftwatermark")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/draftwatermark/"
                   "source/latex/draftwatermark/"
                   "tex/latex/draftwatermark/")
             (base32
              "04l3gqiq0bhzbz8zxr7428fap2x1skkaq5ppbambc4lk8c7iw6da")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/draftwatermark")
    (synopsis "Put a grey textual watermark on document pages")
    (description
     "This package provides a means to add a textual, light grey watermark
on every page or on the first page of a document.  Typical usage may consist
in writing words such as DRAFT or CONFIDENTIAL across document pages.  The
package performs a similar function to that of @code{draftcopy}, but its
implementation is output device independent, and made very simple by relying
on @code{everypage}.")
    (license license:lppl1.3+)))

(define-public texlive-drv
  (package
    (name "texlive-drv")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/drv/" "metapost/drv/")
             (base32
              "0vjc9x9xa50zzzc3m2csj9x0gqwcamhyqz1xkdbkqwkcy8rfpnlh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/drv")
    (synopsis "Derivation trees with MetaPost")
    (description
     "This package provides a set of MetaPost macros for typesetting
derivation trees (such as used in sequent calculus, type inference,
programming language semantics...).")
    (license license:lppl)))

(define-public texlive-dviincl
  (package
    (name "texlive-dviincl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/dviincl/" "metapost/dviincl/")
             (base32
              "05f6ll7cq4ad4i4nkzrjnlqg1456is06fbmjiinadahf7yrqk3lw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/dviincl")
    (synopsis "Include a DVI page into MetaPost output")
    (description
     "DVItoMP is one of the auxiliary programs available to any MetaPost
package; it converts a DVI file into a MetaPost file.  Using it, one can
envisage including a DVI page into an EPS files generated by MetaPost.  Such
files allow pages to include other pages.")
    (license license:public-domain)))

(define-public texlive-emp
  (package
    (name "texlive-emp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/emp/" "source/latex/emp/"
                   "tex/latex/emp/")
             (base32
              "183qwxxjws4l0jrn92dj4qd8avnv9gq2rk1zqak0h48wxlp0fpyq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/emp")
    (synopsis "Encapsulate MetaPost figures in a document")
    (description
     "Emp is a package for encapsulating MetaPost figures in LaTeX: the
package provides environments where you can place MetaPost commands, and means
of using that code as fragments for building up figures to include in your
document.  So, with Emp, the procedure is to run your document with LaTeX, run
MetaPost, and then complete running your document in the normal way.  Emp is
therefore useful for keeping illustrations in synchrony with the text.  It
also frees you from inventing descriptive names for PostScript files that fit
into the confines of file system conventions.")
    (license license:gpl3+)))

(define-public texlive-enctex
  (package
    (name "texlive-enctex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/enctex/"
                   "tex/generic/enctex/")
             (base32
              "1j8ji1ka8vhskm5kn0iwmkhjfp88ly6rva30pr1c9llsmsac5sf2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/enctex")
    (synopsis "TeX extension that translates input on its way into TeX")
    (description
     "EncTeX is (another) TeX extension, written at the change-file level.  It
provides means of translating input on the way into TeX.  It allows, for
example, translation of multibyte sequences, such as utf-8 encoding.")
    (license license:gpl3+)))

(define-public texlive-environ
  (package
    (name "texlive-environ")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/environ/"
                   "source/latex/environ/"
                   "tex/latex/environ/")
             (base32
              "08a3vhyzc647b9zp3yifdklj0vch9cv2vajh7ig3y01jcdqhjy41")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-trimspaces))
    (home-page "https://ctan.org/pkg/environ")
    (synopsis "New interface for environments in LaTeX")
    (description
     "This package provides the @code{\\collect@@body} command (as in
@code{amsmath}), as well as a @code{\\long} version @code{\\Collect@@Body},
for collecting the body text of an environment.  These commands are used to
define a new author interface to creating new environments.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-environ texlive-environ)

(define-public texlive-epsincl
  (package
    (name "texlive-epsincl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/epsincl/" "metapost/epsincl/")
             (base32
              "1pjnfqayh42gavlbd9wqn86qyqhw1bxrbmwgsv39ycj4s63xjxqr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/epsincl")
    (synopsis "Include EPS in MetaPost figures")
    (description
     "The package facilitates including EPS files in MetaPost figures.")
    (license license:public-domain)))

(define-public texlive-eqparbox
  (package
    (name "texlive-eqparbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/eqparbox/"
                   "source/latex/eqparbox/"
                   "tex/latex/eqparbox/")
             (base32
              "16c5dyd4bz45a2c1ppbq05h9ixg15srk5az5pld5gpv4j0zwzrqw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/eqparbox")
    (synopsis "Create equal-widthed parboxes")
    (description
     "LaTeX users sometimes need to ensure that two or more blocks of text
occupy the same amount of horizontal space on the page.  To that end, the
@code{eqparbox} package defines a new command, @code{\\eqparbox}, which works
just like @code{\\parbox}, except that instead of specifying a width, one
specifies a tag.  All @code{eqparbox}es with the same tag---regardless of
where they are in the document---will stretch to fit the widest
@code{eqparbox} with that tag.  This simple, equal-width mechanism can be used
for a variety of alignment purposes, as is evidenced by the examples in
@code{eqparbox}'s documentation.  Various derivatives of @code{\\eqparbox} are
also provided.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-eqparbox texlive-eqparbox)

(define-public texlive-etoc
  (package
    (name "texlive-etoc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/etoc/" "source/latex/etoc/"
                   "tex/latex/etoc/")
             (base32
              "02xa9091vgz5gdzbsc202mzd4lalvvkh3b7slnzppx827sqbq917")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/etoc")
    (synopsis "Customisable table of contents")
    (description
     "This package gives the user complete control of how the entries of the
table of contents should be constituted from the name, number, and page number
of each sectioning unit.  The layout is controlled by the definition of line
styles for each sectioning level used in the document.

The package provides its own custom line styles (which may be used as
examples), and continues to support the standard formatting inherited from the
LaTeX document classes, but the package can also allow the user to delegate
the details to packages dealing with list making environments (such as
@code{enumitem}).  The package's default global style typesets tables of
contents in a multi-column format, with either a standard heading, or a ruled
title (optionally with a frame around the table).

The @code{\\tableofcontents} command may be used arbitrarily many times in the
same document, while @code{\\localtableofcontents} provides a local table of
contents.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-etoc texlive-etoc)

(define-public texlive-expdlist
  (package
    (name "texlive-expdlist")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/expdlist/"
                   "source/latex/expdlist/"
                   "tex/latex/expdlist/")
             (base32
              "1kylmj615hmbmjbynn5bfzhfz0wk1drxmg0kjqljnglffkb6irv6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/expdlist")
    (synopsis "Expanded description environments")
    (description
     "The package provides additional features for the LaTeX @code{description} environment,
including adjustable left margin.  The package also allows the user to break
a list (for example, to interpose a comment) without affecting the structure
of the list (this works for itemize and eumerate lists and numbered lists
remain in sequence).")
    (license license:lppl)))

(define-deprecated-package texlive-latex-expdlist texlive-expdlist)

(define-public texlive-expressg
  (package
    (name "texlive-expressg")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/expressg/"
                   "metapost/expressg/"
                   "source/metapost/expressg/")
             (base32
              "08f6lxxxmhfld6g2iy2kn68llalz3wayxqka7nd48s1ahm33kmdd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/expressg")
    (synopsis "Diagrams consisting of boxes, lines, and annotations")
    (description
     "This package provides a MetaPost package providing facilities to assist
in drawing diagrams that consist of boxes, lines, and annotations.  Particular
support is provided for creating EXPRESS-G diagrams, for example IDEF1X, OMT,
Shlaer-Mellor, and NIAM diagrams.  The package may also be used to create UML
and most other Box-Line-Annotation charts, but not Gantt charts directly.")
    (license license:lppl)))

(define-public texlive-exteps
  (package
    (name "texlive-exteps")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/exteps/" "metapost/exteps/")
             (base32
              "1mmlmcjn8fk16y14p3q6xfmkcc75vcykblgcyzapzxd1nzy51ak7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/exteps")
    (synopsis "Include EPS figures in MetaPost")
    (description
     "Exteps is a module for including external EPS figures into MetaPost
figures.  It is written entirely in MetaPost, and does not therefore require
any post processing of the MetaPost output.")
    (license license:gpl3+)))

(define-public texlive-featpost
  (package
    (name "texlive-featpost")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/featpost/"
                   "metapost/featpost/")
             (base32
              "0a97syvr3vwpayhasb98ssvgwr99p13plrjnqb9ad24jd6srmmdg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/featpost")
    (synopsis "MetaPost macros for 3D")
    (description
     "These macros allow the production of three-dimensional schemes
containing angles, circles, cylinders, cones and spheres, among other
things.")
    (license license:gpl3+)))

(define-public texlive-feynmf
  (package
    (name "texlive-feynmf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/feynmf/" "metafont/feynmf/"
                   "metapost/feynmf/" "source/latex/feynmf/"
                   "tex/latex/feynmf/")
             (base32
              "1a6zhs4x6rkjl7vapc3y59hmrvmi570ji2bszpsk88w3fi8klckb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/feynmf")
    (synopsis "Macros and fonts for creating Feynman (and other) diagrams")
    (description
     "The @code{feynmf} package provides an interface to Metafont to use
simple structure specifications to produce relatively complex diagrams. While
the package was designed for Feynman diagrams, it could in principle be used
for diagrams in graph and similar theories, where the structure is
semi-algorithmically determined.")
    (license license:gpl3+)))

(define-public texlive-feynmp-auto
  (package
    (name "texlive-feynmp-auto")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/feynmp-auto/"
                   "source/latex/feynmp-auto/"
                   "tex/latex/feynmp-auto/")
             (base32
              "19ghfblv641ghak049v42wbqmh7pfadz0mrbkfbx2jr4bvf57rv2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/feynmp-auto")
    (synopsis "Automatic processing of @code{feynmp} graphics")
    (description
     "The package takes care of running Metapost on the output files produced
by the @code{feynmp} package, so that the compiled pictures will be available
in the next run of LaTeX. The package honours options that apply to
@code{feynmp}.")
    (license license:lppl1.3+)))

(define-public texlive-filemod
  (package
    (name "texlive-filemod")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/filemod/" "tex/generic/filemod/"
                   "tex/latex/filemod/")
             (base32
              "1snsj7kblkj1ig3x3845lsypz7ab04lf0dcpdh946xakgjnz4fb5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/filemod")
    (synopsis "Provide file modification times, and compare them")
    (description
     "This package provides macros to read and compare the modification dates
of files.  The files may be @code{.tex} files, images or other files (as long
as they can be found by LaTeX).  It uses the @code{\\pdffilemoddate} primitive
of pdfLaTeX to find the file modification date as PDF date string, parses the
string and returns the value to the user.  The package will also work for DVI
output with recent versions of the LaTeX compiler which uses pdfLaTeX in DVI
mode.  The functionality is provided by purely expandable macros or by faster
but non-expandable ones.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-filemod texlive-filemod)

(define-public texlive-fix2col
  (package
    (name "texlive-fix2col")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fix2col/" "source/latex/fix2col/"
                   "tex/latex/fix2col/")
             (base32
              "04mzs1qn9ish5sdp1v9adqvl92ljbs9rjxxfsqskalm4g9ckn8dy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fix2col")
    (synopsis "Fix miscellaneous two column mode features")
    (description
     "This package is obsolete; do not use in new documents.  It will do
nothing in LaTeX formats after 2015/01/01 as the fixes that it implements were
incorporated into the @code{fixltx2e} package, which is itself obsolete as
since the 2015/01/01 release these fixes are in the LaTeX format itself.

Fix mark handling so that @code{\\firstmark} is taken from the first column if
that column has any marks at all; keep two column floats like @code{figure*}
in sequence with single column floats like figure.")
    (license license:lppl)))

(define-public texlive-fixlatvian
  (package
    (name "texlive-fixlatvian")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/fixlatvian/"
                   "makeindex/fixlatvian/"
                   "source/xelatex/fixlatvian/"
                   "tex/xelatex/fixlatvian/")
             (base32
              "09bivnc287z57k9rhwfl16w66dkvb55shnwpqv972n6dsjdcrh7j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fixlatvian")
    (synopsis "Improve Latvian language support in XeLaTeX")
    (description
     "The package offers improvement of the Latvian language support in
@code{polyglossia}, in particular in the area of the standard classes.")
    (license license:lppl1.3+)))

(define-public texlive-fiziko
  (package
    (name "texlive-fiziko")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/metapost/fiziko/" "metapost/fiziko/")
                            (base32
                             "160iqgm4f7imnj89gj4822xv5wnnxf336k261v52h6sd0lhnhany")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fiziko")
    (synopsis "MetaPost library for physics textbook illustrations")
    (description
     "This MetaPost library was initially written to automate some elements of
black and white illustrations for a physics textbook.  It provides functions
to draw things like lines of variable width, shaded spheres, and tubes of
different kinds, which can be used to produce images of a variety of objects.
The library also contains functions to draw some objects constructed from
these primitives.")
    (license (list license:gpl3+ license:cc-by-sa4.0))))

(define-public texlive-font-change-xetex
  (package
    (name "texlive-font-change-xetex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/font-change-xetex/"
                   "tex/xetex/font-change-xetex/")
             (base32
              "1qhkxsk3wl6a8isik4ln1jq7ifhk7hbidq2i1lfy18c1py87xw5k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/font-change-xetex")
    (synopsis "Macros to change text and mathematics fonts in plain XeTeX")
    (description
     "This package consists of macros that can be used to typeset plain XeTeX
documents using any OpenType or TrueType font installed on the computer
system.  The macros allow the user to change the text mode fonts and some math
mode fonts.  For any declared font family, various font style, weight, and
size variants like bold, italics, small caps, etc., are available through
standard and custom TeX control statements.  Using the optional argument of
the macros, the available XeTeX font features and OpenType tags can be
accessed.  Other features of the package include activating and deactivating
hanging punctuation, and support for special Unicode characters.")
    (license license:cc-by-sa4.0)))

(define-public texlive-fontbook
  (package
    (name "texlive-fontbook")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/fontbook/"
                   "source/xelatex/fontbook/"
                   "tex/xelatex/fontbook/")
             (base32
              "1n5yn6rgndk0yr91bhglby9nr5mifgfi895klvsixmxc0k5bqp97")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fontbook")
    (synopsis "Generate a font book")
    (description
     "The package provides a means of producing a book of font samples (for
evaluation, etc.).")
    (license license:lppl1.3+)))

(define-public texlive-fontwrap
  (package
    (name "texlive-fontwrap")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/fontwrap/"
                   "tex/xelatex/fontwrap/")
             (base32
              "0nzphhwgm9387mn2rfhmsandyvcwv99lxm5978jg6pycs43dggs7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fontwrap")
    (synopsis "Bind fonts to specific Unicode blocks")
    (description
     "The package (which runs under XeLaTeX) lets you bind fonts to specific
Unicode blocks, for automatic font tagging of multilingual text.")
    (license license:gpl3+)))

(define-public texlive-garrigues
  (package
    (name "texlive-garrigues")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/garrigues/"
                   "metapost/garrigues/")
             (base32
              "0vg1j0r4mkp057v7kjr8yza1danc3zj7k590x5531yi0gzp7hgq2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/garrigues")
    (synopsis "MetaPost macros for the reproduction of Garrigues' Easter nomogram")
    (description
     "This library provides MetaPost macros for the reproduction of Garrigues
Easter nomogram.")
    (license license:lppl)))

(define-public texlive-hanging
  (package
    (name "texlive-hanging")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hanging/" "source/latex/hanging/"
                   "tex/latex/hanging/")
             (base32
              "18ichpmmghz0nmv6m646r64y5jvyp52qz9hj7hadrf34xj1ijmlk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/hanging")
    (synopsis "Typeset hanging paragraphs")
    (description
     "The @code{hanging} package facilitates the typesetting of hanging
paragraphs.  The package also enables typesetting with hanging punctuation,
by making punctuation characters active.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-hanging texlive-hanging)

(define-public texlive-fira
  (package
    (name "texlive-fira")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/fira/"
                   "fonts/enc/dvips/fira/"
                   "fonts/map/dvips/fira/"
                   "fonts/opentype/public/fira/"
                   "fonts/tfm/public/fira/"
                   "fonts/type1/public/fira/"
                   "fonts/vf/public/fira/"
                   "tex/latex/fira/")
             (base32
              "19sb6c1h1crhs1597i3nlvr2ahl20hxj7a1a5xkpfr5vj4n3x5kv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fira")
    (synopsis "Fira fonts with LaTeX support")
    (description
     "This package provides LaTeX, pdfLaTeX, XeLaTeX and LuaLaTeX support for
the Fira Sans family of fonts designed by Erik Spiekermann and Ralph du
Carrois of Carrois Type Design.  Fira Sans is available in eleven weights with
corresponding italics: light, regular, medium, bold, ...")
    (license (list license:lppl
                   license:silofl1.1))))

(define-public texlive-firstaid
  (package
    (name "texlive-firstaid")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/firstaid/" "source/latex/firstaid/"
                   "tex/latex/firstaid/")
             (base32
              "1gpbl3l77mrrf88iqcnfvcgxwraqm2rsvisnngak9fbwbinc489v")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip texlive-pdftex))
    (home-page "https://ctan.org/pkg/firstaid")
    (synopsis
     "First aid for external LaTeX files and packages that need updating")
    (description
     "This package contains some first aid for LaTeX packages or classes that
require updates because of internal changes to the LaTeX kernel that are not
yet reflected in the package's or class's code.  The file
@file{latex2e-first-aid-for-external-files.ltx} provided by this package is
meant to be loaded during format generation and not by the user.")
    (license license:lppl1.3c)))

(define-public texlive-gmp
  (package
    (name "texlive-gmp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/gmp/" "source/latex/gmp/"
                   "tex/latex/gmp/")
             (base32
              "1zvl80wjg4xsika0p0v6jskrwdpy3n7qbfvbg76qbzzpc03n6x6a")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/gmp")
    (synopsis "Enable integration between MetaPost pictures and LaTeX")
    (description
     "The package allows integration between MetaPost pictures and LaTeX.
The main feature is that passing parameters to the MetaPost pictures is
possible and the picture code can be put inside arguments to commands,
including @code{\\newcommand}.")
    (license license:lppl1.3+)))

(define-public texlive-hershey-mp
  (package
    (name "texlive-hershey-mp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/hershey-mp/"
                   "metapost/hershey-mp/")
             (base32
              "1pbybiqh5qgj9zh0yifxm2hn25h73kz1glrv1cz9sw38h6iydlzw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hershey-mp")
    (synopsis "MetaPost support for the Hershey font file format")
    (description
     "This package provides MetaPost support for reading jhf vector font
files, used by (mostly? only?) the so-called Hershey Fonts of the late 1960s.
The package does not include the actual font files, which you can probably
find in the software repository of your operating system.")
    (license license:eupl1.2)))

(define-public texlive-ifplatform
  (package
    (name "texlive-ifplatform")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ifplatform/"
                   "source/latex/ifplatform/"
                   "tex/latex/ifplatform/")
             (base32
              "1llas0xwq3y9nk7gblg40l99cgmkl9r7rbfazrhpnbaz5cshl8pl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ifplatform")
    (synopsis "Conditionals to test which platform is being used")
    (description
     "This package uses the (La)TeX extension @samp{-shell-escape} to
establish whether the document is being processed on a Windows or on
a Unix-like system, or on Cygwin.

Booleans provided are: @code{\\ifwindows}, @code{\\iflinux}, @code{\\ifmacosx}
and @code{\\ifcygwin}.  The package also preserves the output of
@command{uname} on a Unix-like system, which may be used to distinguish
between various classes of Unix systems.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-ifplatform texlive-ifplatform)

(define-public texlive-interchar
  (package
    (name "texlive-interchar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/interchar/"
                   "tex/xelatex/interchar/")
             (base32
              "06pvpaph1gi3rjvwzpvzc2rlx0wb8fqmidbfh8dw2qazzbirdlnz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/interchar")
    (synopsis "Managing character class schemes in XeTeX")
    (description
     "The package manages character class schemes of XeTeX.  Using this
package, you may switch among different character class schemes.  Migration
commands are provided for make packages using this mechanism compatible with
each others.")
    (license license:lppl1.3+)))

(define-public texlive-latexmk
  (package
    (name "texlive-latexmk")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/latexmk.1"
                   "doc/man/man1/latexmk.man1.pdf"
                   "doc/support/latexmk/" "scripts/latexmk/")
             (base32
              "1hgzx4xcny2ffm63afhfh3msy1i9llmcdqq2xf3fqlc95rkzn59z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "latexmk.pl")))
    (inputs (list perl))
    (home-page "https://ctan.org/pkg/latexmk")
    (synopsis "Fully automated LaTeX document generation")
    (description
     "Latexmk completely automates the process of generating a LaTeX document.
Given the source files for a document, @command{latexmk} issues the
appropriate sequence of commands to generate a @file{.dvi}, @file{.ps},
@file{.pdf} or hardcopy version of the document.  An important feature is the
preview continuous mode, where the script watches all of the source files and
reruns LaTeX, etc., whenever a source file has changed.  Thus a previewer can
offer a display of the document's latest state.")
    (license license:gpl2)))

(define-public texlive-latexmp
  (package
    (name "texlive-latexmp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/latexmp/" "metapost/latexmp/")
             (base32
              "1zxwxss5sl16laaqalr8043wmyk2bhlja3al5xlxkizvlnflqy0f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/latexmp")
    (synopsis "Interface for LaTeX-based typesetting in MetaPost")
    (description
     "The MetaPost package @code{latexMP} implements a user-friendly interface
to access LaTeX-based typesetting capabilities in MetaPost.  The text to be
typeset is given as string.  This allows even dynamic text elements, for
example counters, to be used in labels.  Compared to other implementations it
is much more flexible, since it can be used as direct replacement for
@code{btex.etex}, and much faster, compared for example to the solution
provided by @code{tex.mp}.")
    (license license:public-domain)))

(define-public texlive-markdown
  (package
    (name "texlive-markdown")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/markdown/examples/"
                   "doc/generic/markdown/"
                   "doc/latex/markdown/examples/"
                   "scripts/markdown/"
                   "source/generic/markdown/"
                   "tex/context/third/markdown/"
                   "tex/generic/markdown/"
                   "tex/latex/markdown/"
                   "tex/luatex/markdown/")
             (base32
              "0nmw1c4ynn0vzdkgpz2dnqimbxbyl6pc0khl2gbi8bd4g9dkai55")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/markdown")
    (synopsis "Converting and rendering Markdown documents inside TeX")
    (description
     "The package provides facilities for the conversion of Markdown markup to
plain TeX.  These are provided both in form of a Lua module and in form of
plain TeX, LaTeX, and ConTeXt macro packages that enable the direct inclusion
of Markdown documents inside TeX documents.")
    (license license:lppl1.3c)))

(define-public texlive-mcf2graph
  (package
    (name "texlive-mcf2graph")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/mcf2graph/")
             (base32
              "1pji3d2mllfi74kalvs11h7yy3hkm5g5nlmcpq2vn7cxjbaqk9sq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mcf2graph")
    (synopsis "Draw chemical structure diagrams with MetaPost")
    (description
     "The Molecular Coding Format (MCF) is a linear notation for describing
chemical structure diagrams.  This package converts MCF to graphic files using
MetaPost.")
    (license license:expat)))

(define-public texlive-metago
  (package
    (name "texlive-metago")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/metago/" "metapost/metago/")
             (base32
              "13m61ml8wfs5888nifilgzi503m3gz1f2fa44408jdhlr6zk6fa0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/metago")
    (synopsis "MetaPost output of Go positions")
    (description
     "The package allows you to draw Go game positions with MetaPost.  Two
methods of usage are provided, either using the package programmatically, or
using the package via a script (which may produce several images).")
    (license license:lppl)))

(define-public texlive-metaobj
  (package
    (name "texlive-metaobj")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/metaobj/" "metapost/metaobj/")
             (base32
              "1ir6p14h79x5iqbxmj2893x8j1d490bnhs83jwnrbcjxn0yxky6w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/metaobj")
    (synopsis "MetaPost package providing high-level objects")
    (description
     "METAOBJ is a large MetaPost package providing high-level objects.  It
implements many of PSTricks features for node connections, but also trees,
matrices, and many other things.  It more or less contains @code{boxes.mp} and
@code{rboxes.mp}.  It is easily extensible with new objects.")
    (license license:lppl)))

(define-public texlive-metaplot
  (package
    (name "texlive-metaplot")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/metaplot/" "metapost/metaplot/")
             (base32
              "19y8lj28gvky7f7g2i562ixwvmnxnflnfc8l8r0x7zg384hg8835")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/metaplot")
    (synopsis "Plot-manipulation macros for use in MetaPost")
    (description
     "MetaPlot is a set of MetaPost macros for manipulating pre-generated
plots (and similar objects), and formatting them for inclusion in a MetaPost
figure.  The intent is that the plots can be generated by some outside
program, in an abstract manner that does not require making decisions about
on-page sizing and layout, and then they can be imported into MetaPlot and
arranged using the full capabilities of MetaPost.  Metaplot also includes
a very flexible set of macros for generating plot axes, which may be useful in
other contexts as well.")
    (license license:lppl)))

(define-public texlive-metapost-colorbrewer
  (package
    (name "texlive-metapost-colorbrewer")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/metapost-colorbrewer/"
                   "metapost/metapost-colorbrewer/")
             (base32
              "00bpdz2k849k1sfrlflxhdbylncb5y2bixgmylyh0i8rbb467q7l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/metapost-colorbrewer")
    (synopsis
     "Implementation of the @code{colorbrewer2.org} colours for MetaPost")
    (description
     "This package provides two MetaPost include files that define all the
@code{colorbrewer2.org} colours: @code{colorbrewer-cmyk.mp} and
@code{colorbrewer-rgb.mp}. The first defines all the colours as CMYK, the
second as RGB.")
    (license license:gpl3+)))

(define-public texlive-metauml
  (package
    (name "texlive-metauml")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/metauml/" "metapost/metauml/")
             (base32
              "01hs234mjqnr39zm7jl6dpbm5w0k4p73pr0aj35ii0dhakln2jsy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/metauml")
    (synopsis "MetaPost library for typesetting UML diagrams")
    (description
     "MetaUML is a MetaPost library for typesetting UML diagrams, which
provides a usable, human-friendly textual notation for UML, offering now
support for class, package, activity, state, and use case diagrams.")
    (license license:gpl3+)))

(define-public texlive-mfpic
  (package
    (name "texlive-mfpic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/mfpic/" "metafont/mfpic/"
                   "metapost/mfpic/" "source/generic/mfpic/"
                   "tex/generic/mfpic/")
             (base32
              "1s3z31mglmij7qc1f0681vv2a6md9wz9zbi6zlh7zvmhy5hzkjkv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mfpic")
    (synopsis "Draw Metafont/post pictures from (La)TeX commands")
    (description
     "Mfpic is a scheme for producing pictures from (La)TeX commands.
Commands @code{\\mfpic} and @code{\\endmfpic} (in LaTeX, the @code{mfpic}
environment) enclose a group in which drawing commands may be placed.  The
commands generate a Meta-language file, which may be processed by MetaPost (or
even Metafont).  The resulting image file will be read back in to the document
to place the picture at the point where the original (La)TeX commands
appeared.")
    (license license:lppl1.3+)))

(define-public texlive-mfpic4ode
  (package
    (name "texlive-mfpic4ode")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mfpic4ode/"
                   "source/latex/mfpic4ode/"
                   "tex/latex/mfpic4ode/")
             (base32
              "0ssmpvp1apxvinidq42pfpvjimpvd250har85n6rl7dj5fws9j8m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mfpic4ode")
    (synopsis "Macros to draw direction fields and solutions of ODEs")
    (description
     "The package is a small set of macros for drawing direction fields, phase
portraits and trajectories of differential equations and two dimensional
autonomous systems.  The Euler, Runge-Kutta and fourth order Runge-Kutta
algorithms are available to solve the ODEs.  The picture is translated into
@code{mfpic} macros and MetaPost is used to create the final drawing.")
    (license license:lppl)))

(define-public texlive-minim-hatching
  (package
    (name "texlive-minim-hatching")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/minim-hatching/"
                   "metapost/minim-hatching/")
             (base32
              "0qcd5zvhj9hrdhb7il1hbfdh5sgccl6mvwwmpw9fymbwdri9224f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/minim-hatching")
    (synopsis "Create tiling patterns with @code{minim-mp} MetaPost processor")
    (description
     "This is a small proof-of-concept library of tiling patterns for use with
the @code{minim-mp} MetaPost processor.")
    (license license:eupl1.2)))

(define-public texlive-mp3d
  (package
    (name "texlive-mp3d")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/mp3d/" "metapost/mp3d/")
             (base32
              "1qfg4ifm5z72sr8vimibmpmqh0cxzipiy4jr8rbq5qsj2mfqzphd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mp3d")
    (synopsis "3D animations")
    (description
     "This library creates animations of 3-dimensional objects (such as
polyhedra) in MetaPost.")
    (license license:lppl)))

(define-public texlive-mparrows
  (package
    (name "texlive-mparrows")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/mparrows/"
                   "metapost/mparrows/")
             (base32
              "0pc9w5g6qasfpv4dxf0aahahbxk6kjwirdmx8l4i27syjb1lgzw9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mparrows")
    (synopsis "MetaPost module with different types of arrow heads")
    (description
     "This package provides a package to provide different types of arrow
heads to be used with MetaPost commands")
    (license license:public-domain)))

(define-public texlive-mpattern
  (package
    (name "texlive-mpattern")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/mpattern/"
                   "metapost/mpattern/")
             (base32
              "00g250vl6gnvwx6zgmfqcw3nwkh546i1vjz4zjp3dc5n5yj5y6ls")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mpattern")
    (synopsis "Patterns in MetaPost")
    (description
     "This package provides a package for defining and using patterns in MetaPost,
using the Pattern Color Space available in PostScript Level 2.")
    (license license:public-domain)))

(define-public texlive-mpcolornames
  (package
    (name "texlive-mpcolornames")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/mpcolornames/"
                   "metapost/mpcolornames/"
                   "source/metapost/mpcolornames/")
             (base32
              "1s5yb57yamg1fd7w5hmkmfyxyqj3mivhkvrkqzm31dzh4y22qg3k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mpcolornames")
    (synopsis "Extend list of predefined colour names for MetaPost")
    (description
     "The MetaPost format @code{plain.mp} provides only five built-in colour names
(variables), all of which are defined in the RGB model: red, green and blue
for the primary colours and black and white.  The package makes more than 500
colour names from different colour sets in different colour models available
to MetaPost.")
    (license license:lppl)))

(define-public texlive-mpgraphics
  (package
    (name "texlive-mpgraphics")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mpgraphics/"
                   "source/latex/mpgraphics/"
                   "tex/latex/mpgraphics/")
             (base32
              "0z51scc8vimwihdyxv3g1cb7bjbj8w2a2ck1ygjyf8xzz4hcic2s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mpgraphics")
    (synopsis "Process and display MetaPost figures inline")
    (description
     "The package allows LaTeX users to typeset MetaPost code inline and
display figures in their documents with only and only one run of LaTeX,
pdfLaTeX or XeLaTeX (no separate runs of @command{mpost}).  Mpgraphics
achieves this by using the shell escape (@samp{\\write 18}) feature of current
TeX distributions, so that the whole process is automatic and the end user is
saved the tiresome processing.")
    (license license:lppl1.3+)))

(define-public texlive-mptrees
  (package
    (name "texlive-mptrees")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/mptrees/" "metapost/mptrees/")
             (base32
              "17jaj27pjnm3k8qcn1ijkwzgm0nacm0mb3fb7rx3a3cf1pi2qwd2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mptrees")
    (synopsis "Probability trees with MetaPost")
    (description
     "This package provides MetaPost tools for drawing simple probability
trees.  One command and several parameters to control the output are
provided.")
    (license license:lppl1.3+)))

(define-public texlive-na-position
  (package
    (name "texlive-na-position")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/na-position/"
                   "tex/xelatex/na-position/")
             (base32
              "0rp1css44sl6j762kfwzq53k6690djgag6yc85dd9134i837gvqr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/na-position")
    (synopsis
     "Tables of relative positions of curves, asymptotes, tangents in Arabic documents")
    (description
     "This package facilitates, in most cases, the creation of tables of
relative positions of a curve and its asymptote, or a curve and a tangent in
one of its points.  This package has to be used with @code{polyglossia} and
XeLaTeX to produce documents in Arabic.")
    (license license:lppl)))

(define-public texlive-natbib
  (package
    (name "texlive-natbib")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/natbib/" "doc/latex/natbib/"
                   "source/latex/natbib/" "tex/latex/natbib/")
             (base32
              "17hyba6v24wrbjvakgjxkndjb418mmi2wmnhrm7zmfwb0bvy6f2j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/natbib")
    (synopsis "Flexible bibliography support")
    (description
     "The bundle provides a package that implements both author-year and
numbered references, as well as much detailed of support for other
bibliography use.  Also Provided are versions of the standard BibTeX styles
that are compatible with natbib--plainnat, unsrtnat, abbrnat.  The
bibliography styles produced by custom-bib are designed from the start to be
compatible with @code{natbib}.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-natbib texlive-natbib)

(define-public texlive-newfloat
  (package
    (name "texlive-newfloat")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/newfloat/" "source/latex/newfloat/"
                   "tex/latex/newfloat/")
             (base32
              "1hrackdfrzad8cgbl3f3yaagk4p4zjbvq710rm8b1z02fr9z2zkq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/newfloat")
    (synopsis "Define new floating environments")
    (description
     "This package offers the command @code{\\DeclareFloatingEnvironment},
which the user may use to define new floating environments which behave like
the LaTeX standard foating environments @code{figure} and @code{table}.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-newfloat texlive-newfloat)

(define-public texlive-newunicodechar
  (package
    (name "texlive-newunicodechar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/newunicodechar/"
                   "source/latex/newunicodechar/"
                   "tex/latex/newunicodechar/")
             (base32
              "1b3n5mdfw9csp0ri1vw4jh1ibnpsllb5n6pwfkg1jad10ml9wavz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/newunicodechar")
    (synopsis "Definitions of the meaning of Unicode characters")
    (description
     "This package provides a friendly interface for defining the meaning of
Unicode characters.  The document should be processed by (pdf)LaTeX with the
Unicode option of @code{inputenc} or @code{inputenx}, or by XeLaTeX/LuaLaTeX.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-newunicodechar texlive-newunicodechar)

(define-public texlive-newverbs
  (package
    (name "texlive-newverbs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/newverbs/" "source/latex/newverbs/"
                   "tex/latex/newverbs/")
             (base32
              "02mhqgsfd13i1llhm0rgq3f9qs067jih2s15q1zvsfd5bhzls1pq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'copy-ydocstrip.tex
            ;; There's a circular dependency between `newverbs' (where `ydoc'
            ;; should be a native input) and `ydoc' (where `newverbs' is
            ;; a propagated input).  To work around this, install the specific
            ;; "ydocstrip.tex" file from `ydoc' in the build directory and set
            ;; TEXINPUTS variable accordingly so the process can find it.
            (lambda* (#:key inputs #:allow-other-keys)
              (install-file (search-input-file inputs
                                               "tex/generic/ydoc/ydocstrip.tex")
                            "build/")
              (setenv "TEXINPUTS" (string-append (getcwd) "/build:")))))))
    (native-inputs
     (list
      (texlive-origin
       "ydocstrip.tex" (number->string %texlive-revision)
       (list "tex/generic/ydoc/ydocstrip.tex")
       (base32
        "1nixgvmw8c6jznhxys3yfzr3qw1lci8kyx54rs0shm6i63xjgr9i"))))
    (home-page "https://ctan.org/pkg/newverbs")
    (synopsis "Define new versions of @code{\\verb}")
    (description
     "The package allows the definition of @code{\\verb} variants which add
TeX code before and after the verbatim text (e.g., quotes or surrounding
@code{\\fbox@{@}}).  When used together with the @code{shortvrb} package it
allows the definition of short verbatim characters which use this package's
variant instead of the normal @code{\\verb}.  In addition, it is possible to
collect an argument verbatim to either typeset or write it into a file.  The
@code{\\Verbdef} command defines verbatim text to a macro which can later be
used to write the verbatim text to a file.")
    (license license:lppl1.3+)))

(define-public texlive-noto
  (package
    (name "texlive-noto")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/noto/"
                   "fonts/enc/dvips/noto/"
                   "fonts/map/dvips/noto/"
                   "fonts/tfm/google/noto/"
                   "fonts/truetype/google/noto/"
                   "fonts/type1/google/noto/"
                   "fonts/vf/google/noto/"
                   "tex/latex/noto/")
             (base32
              "14nf6xd85cb5s9f1sk8zzshgfhjda0r712dp592j8cb4s5v2hf7p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/noto")
    (synopsis "Support for Noto fonts")
    (description
     "This package provides LaTeX, pdfLaTeX, XeLaTeX and LuaLaTeX support for
the NotoSerif, NotoSans and NotoSansMono families of fonts, designed by Steve
Matteson for Google.")
    (license (list license:lppl license:silofl1.1))))

(define-public texlive-novel
  (package
    (name "texlive-novel")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/novel/"
                   "fonts/opentype/novel/"
                   "tex/lualatex/novel/")
             (base32
              "0qg6rs54w1n1vr5dmi6vjks8xn8x04zngk8zz4cv0035jxn4irpc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/novel")
    (synopsis "Class for printing fiction, such as novels")
    (description
     "This LuaLaTeX document class is specifically written to meet the needs
of original fiction writers, who are typesetting their own novels for
non-color print-on-demand technology.  The package is well suited for
detective novels, science fiction, and short stories.  It is however not
recommended for creating color picture books or dissertations.")
    (license (list license:lppl1.3c license:silofl1.1))))

(define-public texlive-octavo
  (package
    (name "texlive-octavo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/octavo/" "source/latex/octavo/"
                   "tex/latex/octavo/")
             (base32
              "10ycj2xg2v7cq4my51dcc749hrrkg2gd41xyrwv1dg1lp9vh7j4z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/octavo")
    (synopsis "Typeset books following classical design and layout")
    (description
     "The @code{octavo} class is a modification of the standard LaTeX
@code{book} class.  Its purpose is to typeset books following classical design
and layout principles, with the express intention of encouraging the making of
beautiful books by anyone with access to a good printer and with an
inclination towards venerable crafts, e.g., bookbinding.  The @code{octavo}
class differs from the book class by implementing many of the proposals and
insights of respected experts, especially Jan Tschichold and Hugh Williamson.
The documentation discusses methods to organise and print out any text into
signatures, which can then be gathered, folded and sewn into a book.")
    (license license:lppl)))

(define-public texlive-pdftexcmds
  (package
    (name "texlive-pdftexcmds")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/pdftexcmds/"
                   "source/generic/pdftexcmds/"
                   "tex/generic/pdftexcmds/")
             (base32
              "0gad1vi0r5xw7gyj1cb2cp58j4dqrw4awcfxmfrna9xbz91g4sn9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/pdftexcmds")
    (synopsis "LuaTeX support for pdfTeX utility functions")
    (description
     "This package makes a number of utility functions from pdfTeX
available for LuaTeX by reimplementing them using Lua.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-pdftexcmds texlive-pdftexcmds)

(define-public texlive-philokalia
  (package
    (name "texlive-philokalia")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/philokalia/"
                   "fonts/opentype/public/philokalia/"
                   "source/xelatex/philokalia/"
                   "tex/xelatex/philokalia/")
             (base32
              "1pcszddyyc4caqd1ahcl10rf1mn0m1lrdgn1gldv94cpa89653kg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/philokalia")
    (synopsis "Font to typeset the Philokalia Books")
    (description
     "The @code{philokalia} package has been designed to ease the use of the
Philokalia-Regular OpenType font with XeLaTeX.  The font started as a project
to digitize the typeface used to typeset the Philokalia books.")
    (license license:lppl1.3c)))

(define-public texlive-piechartmp
  (package
    (name "texlive-piechartmp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/piechartmp/"
                   "metapost/piechartmp/")
             (base32
              "0xf83k85bwbdy4d1m23zyk5zjg9qw960q4rkgf2i1449w30gf3hp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/piechartmp")
    (synopsis "Draw pie-charts using MetaPost")
    (description
     "The @code{piechartmp} package is an easy way to draw pie-charts with
MetaPost.  The package implements an interface that enables users with little
MetaPost experience to draw charts.  A highlight of the package is the
possibility of suppressing some segments of the chart, thus creating the
possibility of several charts from the same data.")
    (license license:lppl)))

(define-public texlive-placeins
  (package
    (name "texlive-placeins")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/placeins/" "tex/latex/placeins/")
             (base32
              "0785ppjhf4x3by61mskwz289nzvbbw6iw7n0fq2dckgywjw3p2mz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/placeins")
    (synopsis "Control float placement")
    (description
     "This package defines a @code{\\FloatBarrier} command, beyond which
floats may not pass; useful, for example, to ensure all floats for a section
appear before the next @code{\\section} command.")
    (license license:public-domain)))

(define-public texlive-psfrag
  (package
    (name "texlive-psfrag")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/psfrag/"
                   "dvips/psfrag/"
                   "source/latex/psfrag/"
                   "tex/latex/psfrag/")
             (base32
              "06vp5x6rnl4gqwxzzynbl169q23k8pmaxjhb0lbzdcm3ihvzp47z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/psfrag")
    (synopsis "Replace strings in encapsulated PostScript figures")
    (description
     "This package allows LaTeX constructions (equations, picture
environments, etc.) to be precisely superimposed over Encapsulated PostScript
figures, using your own favorite drawing tool to create an EPS figure and
placing simple text tags where each replacement is to be placed, with PSfrag
automatically removing these tags from the figure and replacing them with
a user specified LaTeX construction, properly aligned, scaled, and/or
rotated.")
    (license (license:fsf-free "file://psfrag.dtx"))))

(define-deprecated-package texlive-latex-psfrag texlive-psfrag)

(define-public texlive-pstool
  (package
    (name "texlive-pstool")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pstool/" "tex/latex/pstool/")
             (base32
              "12clzcw2cl7g2chr2phgmmiwxw4859cln1gbx1wgp8bl9iw590nc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pstool")
    (synopsis "Support for @code{psfrag} within pdfLaTeX")
    (description
     "This is a package for processing PostScript graphics with @code{psfrag}
labels within pdfLaTeX documents.  Every graphic is compiled individually,
drastically speeding up compilation time when only a single figure needs
re-processing.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-pstool texlive-pstool)

(define-public texlive-ptext
  (package
    (name "texlive-ptext")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/ptext/" "tex/xelatex/ptext/")
             (base32
              "0ipb1mlg266rziznskdxi3iwi0s6374lp8ky0hhbi5llryg505p9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ptext")
    (synopsis "Lipsum for Persian")
    (description
     "The package provides lipsum-like facilities for the Persian language.
The source of the filling text is the Persian epic @emph{the Shanameh} (100
paragraphs are used).")
    (license license:lppl1.2+)))

(define-public texlive-realscripts
  (package
    (name "texlive-realscripts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/realscripts/"
                   "source/latex/realscripts/"
                   "tex/latex/realscripts/")
             (base32
              "1yk1v1ybd1zv1x6rc8qd2mbbwc5h91i44scnih6v2n5nrh61jfxb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/realscripts")
    (synopsis "Access OpenType subscript and superscript glyphs")
    (description
     "This package replaces @code{\\textsuperscript} and
@code{\\textsubscript} commands by equivalent commands that use OpenType font
features to access appropriate glyphs if possible.  It also patches LaTeX's
default footnote command to use this new @code{\\textsuperscript} for footnote
symbols.")
    (license license:lppl1.3+)))

(define-public texlive-refcount
  (package
    (name "texlive-refcount")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/refcount/" "source/latex/refcount/"
                   "tex/latex/refcount/")
             (base32
              "128cvwdl4wcdshvs59yn5iljdxxdrc5jircbxav77y7kc3l33z7z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/refcount")
    (synopsis "Counter operations with label references")
    (description
     "This package provides the @code{\\setcounterref} and
@code{\\addtocounterref} commands which use the section (or other) number
from the reference as the value to put into the counter.  It also provides
@code{\\setcounterpageref} and @code{\\addtocounterpageref} that do the
corresponding thing with the page reference of the label.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-refcount texlive-refcount)

(define-public texlive-repere
  (package
    (name "texlive-repere")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/repere/" "metapost/repere/")
             (base32
              "06p184mgv0cac36pp1srrd80axvkxf155l3jf01dnvd2x1d7dwlk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/repere")
    (synopsis "MetaPost macros for secondary school mathematics teachers")
    (description
     "This package provides MetaPost macros for drawing secondary school
mathematics figures in a coordinate system: axis, grids points, vectors
functions (curves, tangents, integrals, sequences) statistic diagrams plane
geometry (polygons, circles), arrays and game boards.")
    (license license:lppl1.3c)))

(define-public texlive-roex
  (package
    (name "texlive-roex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "metafont/roex/" "source/metafont/roex/")
             (base32
              "12w5wrrlk8rd4gx57646r01rb49ckmgnzhmx9385ll0sag8c9s2v")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mf-ps")
    (synopsis "Metafont-PostScript conversions")
    (description
     "This package provides a Metafont support package including: @code{epstomf},
a tiny AWK script for converting EPS files into Metafont; @code{mftoeps} for
generating (encapsulated) PostScript files readable, e.g., by CorelDRAW, Adobe
Illustrator and Fontographer; a collection of routines (in folder progs) for
converting Metafont-coded graphics into encapsulated PostScript; and
@code{roex.mf}, which provides Metafont macros for removing overlaps and
expanding strokes.  In @code{mftoeps}, Metafont writes PostScript code to
a log-file, from which it may be extracted by either TeX or AWK.")
    (license license:public-domain)))

(define-public texlive-roundrect
  (package
    (name "texlive-roundrect")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/roundrect/"
                   "metapost/roundrect/"
                   "source/metapost/roundrect/")
             (base32
              "0bx6xfr49rrcrnbyw917fab2qi0x29h66ip4nyhw6477815ic0wq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/roundrect")
    (synopsis "MetaPost macros for configurable rounded rectangles")
    (description
     "The @code{roundrect} macros for MetaPost provide ways to produce rounded
rectangles, which may or may not contain a title bar or text (the title bar
may itself contain text).")
    (license license:lppl1.3+)))

(define-public texlive-sauerj
  (package
    (name "texlive-sauerj")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sauerj/" "source/latex/sauerj/"
                   "tex/latex/sauerj/")
             (base32
              "03dmw150qcravzndikijhq2a89sjdplxgi93fx0zxln2l4flvz5k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/sauerj")
    (synopsis "Bundle of utilities by Jonathan Sauer")
    (description
     "The bundle consists of:
@itemize
@item a tool for collecting text for later re-use,
@item a tool for typesetting the meta-information within a text,
@item a tool for use in constructing macros with multiple optional parameters,
@item a package for multiple column parallel texts,
@item a tool for processing key-value structured lists,
@item macros for typesetting a number as a German-language string.
@end itemize")
    (license license:lppl)))

(define-public texlive-selinput
  (package
    (name "texlive-selinput")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/selinput/" "source/latex/selinput/"
                   "tex/latex/selinput/")
             (base32
              "0x8l98r6xzyi4lc909bv7ii2nbpff8j7j3q4z86l7rrjk1jkx9qi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/selinput")
    (synopsis "Semi-automatic detection of input encoding")
    (description
     "This package selects the input encoding by specifying pairs of input
characters and their glyph names.")
    (license license:lppl1.3+)))

(define-public texlive-semaphor
  (package
    (name "texlive-semaphor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/semaphor/"
                   "fonts/afm/public/semaphor/"
                   "fonts/enc/dvips/semaphor/"
                   "fonts/map/dvips/semaphor/"
                   "fonts/opentype/public/semaphor/"
                   "fonts/source/public/semaphor/"
                   "fonts/tfm/public/semaphor/"
                   "fonts/type1/public/semaphor/"
                   "tex/context/third/semaphor/"
                   "tex/latex/semaphor/"
                   "tex/plain/semaphor/")
             (base32
              "1yigah8x75fd13x0f6ncpp21ly75nyfz6h5y5sfc590n7wcm0gvr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-metafont))
    (home-page "https://ctan.org/pkg/semaphor")
    (synopsis "Semaphore alphabet font")
    (description
     "These fonts represent semaphore in a highly schematic, but very clear,
fashion.  The fonts are provided as Metafont source, and in both OpenType and
Adobe Type 1 formats.")
    (license license:gpl3+)))

(define-public texlive-seminar
  (package
    (name "texlive-seminar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/seminar/" "tex/latex/seminar/")
             (base32
              "1xm78f9qsy3zr1vllb8mgp1azhn7a2jaqkj2lkrsgc3m7ag9w9hh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/seminar")
    (synopsis "Make overhead slides")
    (description
     "This package provides a class that produces overhead
slides (transparencies), with many facilities.  Seminar is not nowadays
reckoned a good basis for a presentation — users are advised to use more
recent classes such as @code{powerdot} or @code{beamer}, both of which are
tuned to 21st-century presentation styles.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-seminar texlive-seminar)

(define-public texlive-sepnum
  (package
    (name "texlive-sepnum")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sepnum/" "tex/latex/sepnum/")
             (base32
              "1vzjhb470imd1f5wlj32jysn825vyygq9xkscqdjaa9jby70a26x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/sepnum")
    (synopsis "Print numbers in a friendly format")
    (description
     "This package provides a command to print a number with (potentially
different) separators every three digits in the parts either side of the
decimal point (the point itself is also configurable).  The macro is fully
expandable and not fragile (unless one of the separators is).  There is also
a command @code{\\sepnumform}, that may be used when defining
@samp{\\the<counter> macros}.")
    (license license:lppl)))

(define-public texlive-seqsplit
  (package
    (name "texlive-seqsplit")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/seqsplit/"
                   "source/latex/seqsplit/"
                   "tex/latex/seqsplit/")
             (base32
              "0x6xcism9s9mhpr278xi4c1gimz3mlqnrpr40vkx5abglr0gzm0j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/seqsplit")
    (synopsis "Split long sequences of characters in a neutral way")
    (description
     "@code{seqsplit} provides a command @code{\\seqsplit}, which makes its
argument splittable anywhere, and then leaves the TeX paragraph-maker to do the
splitting.  The package is suitable for situations when one needs to type long
sequences of letters or of numbers in which there is no obvious break points to
be found, such as in base-sequences in genes or calculations of transcendental
numbers.  While the package may obviously be used to typeset DNA sequences, the
user may consider the @code{dnaseq} as a rather more powerful alternative.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-seqsplit texlive-seqsplit)

(define-public texlive-shapes
  (package
    (name "texlive-shapes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/shapes/" "metapost/shapes/"
                   "source/metapost/shapes/")
             (base32
              "0q93ycxjzmvp73z92rc7vlzvmijbj2w1ldq709r9jflk044lsrh2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/shapes")
    (synopsis "Draw polygons, reentrant stars, and fractions in circles")
    (description
     "The shapes set of macros allows drawing regular polygons; their
corresponding reentrant stars in all their variations; and fractionally filled
circles (useful for visually demonstrating the nature of fractions) in
MetaPost.")
    (license license:lppl1.3+)))

(define-public texlive-showexpl
  (package
    (name "texlive-showexpl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/showexpl/" "source/latex/showexpl/"
                   "tex/latex/showexpl/")
             (base32
              "0vff1yk7a3f4csxibfk6r37s3h6n4wdpnk3qj4dsx7kh5zrcysha")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/showexpl")
    (synopsis "Typesetting LaTeX source code")
    (description
     "This package provides a way to typeset LaTeX source code and the related
result in the same document.")
    (license license:lppl1.2+)))

(define-public texlive-simple-resume-cv
  (package
    (name "texlive-simple-resume-cv")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/simple-resume-cv/"
                   "tex/xelatex/simple-resume-cv/")
             (base32
              "07lqmjfjr58q7jgl59syv67hsy54rsig9f002wkwr1297z02k862")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/simple-resume-cv")
    (synopsis "Template for a simple resume or curriculum vitae (CV)")
    (description
     "This package provides a template for a simple resume or curriculum
vitae (CV), in XeLaTeX.  This simple template can be further customized or
extended, with numerous examples.")
    (license license:public-domain)))

(define-public texlive-simple-thesis-dissertation
  (package
    (name "texlive-simple-thesis-dissertation")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list
              "doc/xelatex/simple-thesis-dissertation/Figures/"
              "tex/xelatex/simple-thesis-dissertation/")
             (base32
              "023bl8ic6bn86297wbxip5lm34wkbq1kcrizkmmsdz7cfxpn6637")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/simple-thesis-dissertation")
    (synopsis "Template for a simple thesis, dissertation, or technical report")
    (description
     "This package provides a template for a simple thesis or dissertation or
technical report, in XeLaTeX.  This simple template that can be further
customized or extended, with numerous examples.")
    (license license:public-domain)))

(define-public texlive-slideshow
  (package
    (name "texlive-slideshow")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/slideshow/"
                   "metapost/slideshow/")
             (base32
              "07r58whn61nyxj0cldzf9bbwhh0kc72d6708ldf03q7sbf5zi37s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/slideshow")
    (synopsis "Generate slideshow with MetaPost")
    (description
     "The package provides a means of creating presentations in MetaPost,
without intervention from other utilities (except a distiller).")
    (license (license:fsf-free "file://metapost/slideshow/slideshow.mp"))))

(define-public texlive-splines
  (package
    (name "texlive-splines")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/splines/" "metapost/splines/"
                   "source/metapost/splines/")
             (base32
              "15pvr4vl5whcrij6gq3al5jaqrshvm2qybvxq7qx0gdxzfsli38z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/splines")
    (synopsis "MetaPost macros for drawing cubic spline interpolants")
    (description
     "This is a small package of macros for creating cubic spline interpolants
in MetaPost or Metafont.  Given a list of points the macros can produce
a closed or a relaxed spline joining them.")
    (license license:lppl1.3+)))

(define-public texlive-stackengine
  (package
    (name "texlive-stackengine")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/stackengine/"
                   "tex/latex/stackengine/")
             (base32
              "0c12ygqxdb6vn1y03jzcjpmdp53r076hq3hgjzwy2ch1dw81cnpd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-listofitems))
    (home-page "https://ctan.org/pkg/stackengine")
    (synopsis "Customised stacking of objects")
    (description
     "The package provides a versatile way to stack objects vertically in a
variety of customizable ways.  A number of useful macros are provided, all
of which make use of the @code{stackengine} core.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-stackengine texlive-stackengine)

(define-public texlive-suanpan
  (package
    (name "texlive-suanpan")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/suanpan/" "metapost/suanpan/")
             (base32
              "1ddhk8d98bc4l2xbx5w6kaynl4n125nnv4hadfp5s19vwcc6slqv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/suanpan")
    (synopsis "MetaPost macros for drawing Chinese and Japanese abaci")
    (description
     "The package provides macros for drawing Chinese and Japanese abaci.")
    (license license:lppl)))

(define-public texlive-tetragonos
  (package
    (name "texlive-tetragonos")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/tetragonos/"
                   "tex/xelatex/tetragonos/")
             (base32
              "191727d2craai25847i2xfzlm04852afvrwdjsk3jjss46a43ixn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tetragonos")
    (synopsis "Four-Corner codes of Chinese characters")
    (description
     "This is a XeLaTeX package for mapping Chinese characters to their codes
in the Four-Corner method.")
    (license license:lppl1.3c)))

(define-public texlive-threeddice
  (package
    (name "texlive-threeddice")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/threeddice/"
                   "metapost/threeddice/")
             (base32
              "1sj4f3bbxjzwbncxlvlbmsnfi7jkf5625gwhbqfh399vlh0nb56j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/threeddice")
    (synopsis "Create images of dice with one, two, or three faces showing")
    (description
     "The package provides MetaPost code to create all possible symmetrical
views (up to rotation) of a right-handed die.")
    (license license:lppl)))

(define-public texlive-textpath
  (package
    (name "texlive-textpath")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/metapost/textpath/"
                   "metapost/textpath/" "tex/latex/textpath/")
             (base32
              "07g7n0hjsvsk0cibprpqid43vvljjzagap07zbp2kirkiv7yq3k0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/textpath")
    (synopsis "Setting text along a path with MetaPost")
    (description
     "This MetaPost package provides macros to typeset text along a free path
with the help of LaTeX, thereby preserving kerning and allowing for 8-bit
input (accented characters).")
    (license license:lppl)))

(define-public texlive-tocloft
  (package
    (name "texlive-tocloft")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/tocloft/"
                   "source/latex/tocloft/"
                   "tex/latex/tocloft/")
             (base32
              "1s09g02pq2zi5ywm3yhyp4lpzq77n9aahr6wnm1c0wb9zawq2mpk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/tocloft")
    (synopsis "Control table of contents")
    (description
     "This package provides control over the typography of the @dfn{Table of
Contents}, @dfn{List of Figures} and @dfn{List of Tables}, and the ability to
create new @samp{List of ...}.  The ToC @code{\\parskip} may be changed.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-tocloft texlive-tocloft)

(define-public texlive-trimspaces
  (package
    (name "texlive-trimspaces")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/trimspaces/"
                   "source/latex/trimspaces/"
                   "tex/latex/trimspaces/")
             (base32
              "0if7pqaib533fbrj9r62mmr4h012hrpszdxs759rwhmyycikg6dk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-format "latex"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-bug
            (lambda _
              ;; The "ins" file refers to the wrong source file.
              (substitute* "source/latex/trimspaces/trimspaces.ins"
                (("pstool\\.tex") "trimspaces.tex")))))))
    (home-page "https://ctan.org/pkg/trimspaces")
    (synopsis "Trim spaces around an argument or within a macro")
    (description
     "This package provides a very short package that allows you to expandably
remove spaces around a token list (commands are provided to remove spaces
before, spaces after, or both); or to remove surrounding spaces within a macro
definition, or to define space-stripped macros.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-trimspaces texlive-trimspaces)

(define-public texlive-tufte-latex
  (package
    (name "texlive-tufte-latex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/tufte-latex/"
                   "doc/latex/tufte-latex/"
                   "tex/latex/tufte-latex/")
             (base32
              "16jqf8assirdj769rajrdb70w8rc0kyj0q07bs6v13kfil5h0bdp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-changepage
           texlive-ifmtarg
           texlive-paralist
           texlive-placeins
           texlive-sauerj
           texlive-xifthen))
    (home-page "https://ctan.org/pkg/tufte-latex")
    (synopsis "Document classes inspired by the work of Edward Tufte")
    (description
     "This package provides two classes inspired, respectively, by handouts
and books created by Edward Tufte.")
    (license license:asl2.0)))

(define-public texlive-ucharclasses
  (package
    (name "texlive-ucharclasses")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/ucharclasses/"
                   "tex/xelatex/ucharclasses/")
             (base32
              "0pgzs730zqmcck693i7fq771p7szq6nqdxb9w8dy6l2b4zdql14m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ucharclasses")
    (synopsis "Font actions in XeTeX according to what is being processed")
    (description
     "The package takes care of switching fonts when you switch from one
Unicode block to another in the text of a document.  This way, you can write
a document with no explicit font selection, but a series of rules of the form
``when entering block ..., switch font to use ...''.")
    (license license:public-domain)))

(define-public texlive-unicode-bidi
  (package
    (name "texlive-unicode-bidi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/unicode-bidi/"
                   "tex/xelatex/unicode-bidi/")
             (base32
              "0anwaaf21qcdgni9z85hqap1wb4y9pv13p96x13w29hbqizmf69l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/unicode-bidi")
    (synopsis "Experimental Unicode Bidi package for XeTeX")
    (description
     "The experimental Unicode-Bidi package allows to mix non-RTL script with
RTL script without any markup.")
    (license license:lppl1.3+)))

(define-public texlive-unimath-plain-xetex
  (package
    (name "texlive-unimath-plain-xetex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/unimath-plain-xetex/"
                   "fonts/misc/xetex/fontmapping/unimath-plain-xetex/"
                   "tex/xetex/unimath-plain-xetex/")
             (base32
              "0vqnqfklg1mj7mipgrfng0qq1i9psiqri0yh30ixrz8j6mkaqb35")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/unimath-plain-xetex")
    (synopsis "OpenType math support in (plain) XeTeX")
    (description
     "This package provides OpenType math font support in plain TeX format.
It only works with the XeTeX engine.")
    (license license:lppl1.3c)))

(define-public texlive-unisugar
  (package
    (name "texlive-unisugar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/unisugar/"
                   "tex/xelatex/unisugar/")
             (base32
              "1bn88ghfn14am1grph1pjw9k0xy1rz8swzhsbxsxyzz6cqk9s161")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/unisugar")
    (synopsis "Define syntactic sugar for Unicode LaTeX")
    (description
     "The package allows the user to define shorthand aliases for single
Unicode characters, and also provides support for such aliases in RTL-text.
The package requires an TeX-alike system that uses Unicode input in a native
way: current examples are XeTeX and LuaTeX.")
    (license license:lppl1.3+)))

(define-public texlive-xebaposter
  (package
    (name "texlive-xebaposter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xebaposter/"
                   "tex/latex/xebaposter/")
             (base32
              "18fnwfhfk3jzkp4yd0dfi49jnf8njccbhhd6k15pvghs4brd9hba")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xebaposter")
    (synopsis "Create scientific Persian/Latin posters using TikZ")
    (description
     "This package is designed for making scientific Persian/Latin posters.
It is a fork of @code{baposter} by Brian Amberg and Reinhold Kainhofer.")
    (license license:lppl1.3+)))

(define-public texlive-xechangebar
  (package
    (name "texlive-xechangebar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xechangebar/"
                   "tex/xelatex/xechangebar/")
             (base32
              "0a7b9bffh5b435gw4qxydmfrpizly79cjgjhhlgywwg6gibvxn4s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xechangebar")
    (synopsis "Extension of package @code{changebar} for use with XeLaTeX")
    (description
     "The package extends package @code{changebar} so it can be used with
XeLaTeX.  It introduces the new option @code{xetex} for use with XeLaTeX.
Everything else remains the same and users should consult the original
documentation for usage information.")
    (license license:lppl1.3+)))

(define-public texlive-xecolor
  (package
    (name "texlive-xecolor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xecolor/"
                   "tex/xelatex/xecolor/")
             (base32
              "0adzg2j1lmclr5zcs8da3m1b9q5xs50rxga6k2pzzxy2x0nh1xpi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xecolor")
    (synopsis "Support for color in XeLaTeX")
    (description
     "This is a simple package which defines about 140 different colours using
XeTeX's colour feature.  The colours can be used in bidirectional texts
without any problem.")
    (license license:lppl1.3+)))

(define-public texlive-xecyr
  (package
    (name "texlive-xecyr")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xecyr/" "tex/xelatex/xecyr/")
             (base32
              "07qvxfnwha6iqzcv84ws074jdi8jn60h42l93jn037n3zj2qxkg8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xecyr")
    (synopsis "Using Cyrillic languages in XeTeX")
    (description
     "This package provides helper tools for using Cyrillic languages with
XeLaTeX and @code{babel}.")
    (license license:lppl1.3+)))

(define-public texlive-xeindex
  (package
    (name "texlive-xeindex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xeindex/"
                   "tex/xelatex/xeindex/")
             (base32
              "1sps9lrzm9y2rrin5pkgzyk56c77xnydvp21ljmvsimqgafr5aqb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xeindex")
    (synopsis "Automatic index generation for XeLaTeX")
    (description
     "The package is based on XeSearch, and will automatically index words or
phrases in an XeLaTeX document.  Words are declared in a list, and every
occurrence then creates an index entry whose content can be fully specified
beforehand.")
    (license license:lppl)))

(define-public texlive-xesearch
  (package
    (name "texlive-xesearch")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/xesearch/" "tex/xetex/xesearch/")
             (base32
              "13fdllqswyyvvyjqn0a4ld18dixxnn7zlpagsdaq8cl1svpaxpk5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xesearch")
    (synopsis "String finder for XeTeX")
    (description
     "The package finds strings (e.g., parts of words or phrases) and
manipulates them, thus turning each word or phrase into a possible command.
It is written in plain XeTeX and should thus work with any format.  The main
application for the moment is XeIndex, an automatic index for XeLaTeX, but
examples are given of simple use to check spelling, count words, and highlight
syntax of programming languages.")
    (license license:lppl)))

(define-public texlive-xespotcolor
  (package
    (name "texlive-xespotcolor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xespotcolor/"
                   "source/xelatex/xespotcolor/"
                   "tex/xelatex/xespotcolor/")
             (base32
              "0pqv4y2idcazwdy94ryyrk5s2g66sb7liy9hypqaq69symn8myf7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xespotcolor")
    (synopsis "Spot colours support for XeLaTeX")
    (description
     "The package provides macros for using spot colours in LaTeX documents.
The package is a reimplementation of the @code{spotcolor} package for use with
XeLaTeX.  As such, it has the same user interface and the same capabilities.")
    (license license:lppl1.3c)))

(define-public texlive-xetex-itrans
  (package
    (name "texlive-xetex-itrans")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xetex-itrans/"
                   "fonts/misc/xetex/fontmapping/xetex-itrans/")
             (base32
              "1chx2s0p6y5p767cr23jh9x61axjrdnyc5lcvf7kyrg74cszsmql")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xetex-itrans")
    (synopsis "Itrans input maps for use with XeLaTeX")
    (description
     "The package provides maps for use with XeLaTeX with coding done using
@code{itrans}.  Fontspec maps are provided for Devanagari (Sanskrit), for
Sanskrit in Kannada and for Kannada itself.")
    (license license:lppl1.3+)))

(define-public texlive-xetex-pstricks
  (package
    (name "texlive-xetex-pstricks")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/xetex-pstricks/"
                   "tex/xelatex/xetex-pstricks/"
                   "tex/xetex/xetex-pstricks/")
             (base32
              "195zjdxddlwl69gnp2x3jg3l8gn36mxsdbz03qs0r940frx533ls")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xetex-pstricks")
    (synopsis "Running PSTricks under XeTeX")
    (description
     "The package provides an indirection scheme for XeTeX to use the PSTricks
@file{xdvipdfmx.cfg} configuration file, so that XeTeX documents will load it
in preference to the standard pstricks.con configuration file.  With this
configuration, many PSTricks features can be used in XeLaTeX or plain XeTeX
documents.")
    (license license:public-domain)))

(define-public texlive-xetex-tibetan
  (package
    (name "texlive-xetex-tibetan")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/xetex-tibetan/"
                   "fonts/misc/xetex/fontmapping/xetex-tibetan/")
             (base32
              "02z3jzf16hi9zj50lpf9map5f6ydvxw66f0k4n5ry88s7frbxwmw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xetex-tibetan")
    (synopsis "XeTeX input maps for Unicode Tibetan")
    (description
     "The package provides a map for use with Jonathan Kew's TECkit, to
translate Tibetan to Unicode (range 0F00-0FFF).")
    (license license:lppl)))

(define-public texlive-xetexfontinfo
  (package
    (name "texlive-xetexfontinfo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/xetexfontinfo/"
                   "tex/xetex/xetexfontinfo/")
             (base32
              "0x1llikcvwlf74anmvaks73gvd99xha9dg49zh75dwki0nwisn2j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xetexfontinfo")
    (synopsis "Report font features in XeTeX")
    (description
     "This package provides a pair of documents to reveal the font features
supported by fonts usable in XeTeX.")
    (license license:asl2.0)))

(define-public texlive-xetexko
  (package
    (name "texlive-xetexko")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xetex/xetexko/" "tex/xetex/xetexko/")
             (base32
              "1jp5caxnyjf5fnndszaqpzsgcm2rhk5iapnpf1ca6mvxbnpf1x5l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xetexko")
    (synopsis "Typeset Korean with Xe(La)TeX")
    (description
     "The package supports typesetting Korean documents (including old Hangul
texts), using XeTeX.  It enhances the existing support, in XeTeX, providing
features that provide quality typesetting.")
    (license license:lppl1.3c)))

(define-public texlive-xevlna
  (package
    (name "texlive-xevlna")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xevlna/" "tex/xelatex/xevlna/")
             (base32
              "0gwh1rd5s01rnb11nxk6nxwyqj32bi739p10hwqz3sw1hfy8mnfi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xevlna")
    (synopsis "Insert non-breakable spaces using XeTeX")
    (description
     "The package will directly insert nonbreakable spaces (in Czech, vlna or
vlnka), after nonsyllabic prepositions and single letter conjuctions, while
the document is being typeset.")
    (license license:lppl1.3+)))

(define-public texlive-currfile
  (package
    (name "texlive-currfile")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/currfile/" "source/latex/currfile/"
                   "tex/latex/currfile/")
             (base32
              "0g28y2bwqnc3xfcp0ak7rxx0c40b88vl85pj7x5dccmvx0yrxy9n")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (home-page "https://ctan.org/pkg/currfile")
    (synopsis "Provide file name and path of input files")
    (description
     "The package provides macros holding file name information (directory,
base name, extension, full name and full path) for files read by LaTeX
@code{\\input} and @code{\\include} macros; it uses the file hooks provided by
the author's @code{filehook}.  In particular, it restores the parent file name
after the trailing @code{\\clearpage} of an @code{\\included} file; as
a result, the macros may be usefully employed in the page header and footer of
the last printed page of such a file.  The depth of inclusion is made
available, together with the parent (including file) and parents (all
including files to the root of the tree).  The package supersedes FiNK.")
    (license license:lppl1.3+)))

(define-public texlive-calrsfs
  (package
    (name "texlive-calrsfs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/calrsfs/" "tex/latex/calrsfs/")
             (base32
              "0aqa0k0zzzicx5nynd29i9pdb7a4j6fvf1xwrbm4qg64pl55i6xa")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/calrsfs")
    (synopsis "Copperplate calligraphic letters in LaTeX")
    (description "This package provides a maths interface to the @code{rsfs}
fonts.")
    (license license:public-domain)))

(define-public texlive-capt-of
  (package
    (name "texlive-capt-of")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/capt-of/" "source/latex/capt-of/"
                   "tex/latex/capt-of/")
             (base32
              "0bf0cdd9ca3kkqxqqkq6jalh5ybs60l80l5gfkl2whk2v4bnzfvz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/capt-of")
    (synopsis "Captions on more than floats")
    (description
     "This package defines a command @code{\\captionof} for putting a caption
to something that's not a float.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-capt-of texlive-capt-of)

(define-public texlive-carlisle
  (package
    (name "texlive-carlisle")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/carlisle/" "source/latex/carlisle/"
                   "tex/latex/carlisle/")
             (base32
              "0kgbs8k6ma3kng2srwpzkla1c51ylzgb7yn8bib2zy46rmysrk86")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/carlisle")
    (synopsis "David Carlisle's small packages")
    (description
     "Many of David Carlisle's more substantial packages stand on their own,
or as part of the LaTeX latex-tools set; this set contains: making dotless
@emph{j} characters for fonts that don't have them; a method for combining the
capabilities of @code{longtable} and @code{tabularx}; an environment for
including plain TeX in LaTeX documents; a jiffy to create slashed characters
for physicists.")
    (license license:lppl)))

(define-public texlive-catchfile
  (package
    (name "texlive-catchfile")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/catchfile/" "source/latex/catchfile/"
                   "tex/generic/catchfile/")
             (base32
              "1dpxy64hs0bjp8d2dmikflc995vazf7fi6z92w51fnj2fidgl8gx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/macros/latex/contrib/catchfile")
    (synopsis "Catch an external file into a macro")
    (description
     "Catchfile catches the contents of a file and puts it in a macro.")
    (license license:lppl1.3+)))

(define-public texlive-ddphonism
  (package
    (name "texlive-ddphonism")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ddphonism/" "tex/latex/ddphonism/")
             (base32
              "1p02ai76nnh6042pnmqv4n30z6yxsvyyk2nb9jk7xlyyc87zzbdd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ddphonism")
    (synopsis "Dodecaphonic diagrams for LaTeX")
    (description
     "This is a music-related package which is focused on notation from the
twelve-tone system, also called dodecaphonism.  It provides LaTeX algorithms
that produce typical dodecaphonic notation based off a musical series, or row
sequence, of variable length.")
    (license license:lppl1.3c)))

(define-public texlive-doi
  (package
    (name "texlive-doi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/doi/" "tex/latex/doi/")
             (base32
              "18z9922lqb3hliqn95h883fndqs4lgyi5yqbnq2932ya0imc3j7h")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/doi")
    (synopsis "Create correct hyperlinks for DOI numbers")
    (description
     "You can hyperlink DOI numbers to doi.org.  However, some publishers have
elected to use nasty characters in their DOI numbering scheme (@code{<},
@code{>}, @code{_} and @code{;} have all been spotted).  This will either
upset LaTeX, or your PDF reader.  This package contains a single user-level
command @code{\\doi{}}, which takes a DOI number, and creates a correct
hyperlink to the target of the DOI.")
    ;; Any version of the LPPL.
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-doi texlive-doi)

(define-public texlive-etoolbox
  (package
    (name "texlive-etoolbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/etoolbox/" "tex/latex/etoolbox/")
             (base32
              "070iaj540rglf0c80l0hjkwg6aa7qyskhh4iwyhf7n8vrg5cjjab")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/etoolbox")
    (synopsis "e-TeX tools for LaTeX")
    (description
     "The package is a toolbox of programming facilities geared primarily
towards LaTeX class and package authors.  It provides LaTeX frontends to some
of the new primitives provided by e-TeX as well as some generic tools which
are not strictly related to e-TeX but match the profile of this package.  The
package provides functions that seem to offer alternative ways of implementing
some LaTeX kernel commands; nevertheless, the package will not modify any part
of the LaTeX kernel.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-etoolbox texlive-etoolbox)

(define-public texlive-fncychap
  (package
    (name "texlive-fncychap")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fncychap/"
                   "tex/latex/fncychap/")
             (base32
              "1javlws18ncrf7rz7qfbx1db9jwk45lm6sa0s67hlr6hqnyjxf94")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fncychap")
    (synopsis "Seven predefined chapter heading styles")
    (description
     "This package provides seven predefined chapter heading styles.  Each
style can be modified using a set of simple commands.  Optionally one can
modify the formatting routines in order to create additional chapter
headings.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-fncychap texlive-fncychap)

(define-public texlive-framed
  (package
    (name "texlive-framed")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/framed/"
                   "tex/latex/framed/")
             (base32
              "09hlzjlhz3q3l62h6gj997pfx1hc4726frhcdc6y5g66c3gh621g")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/framed")
    (synopsis "Framed or shaded regions that can break across pages")
    (description
     "The package creates three environments: @code{framed}, which puts an
ordinary frame box around the region, @code{shaded}, which shades the region,
and @code{leftbar}, which places a line at the left side.  The environments
allow a break at their start (the @code{\\FrameCommand} enables creation of
a title that is “attached” to the environment); breaks are also allowed in the
course of the framed/shaded matter.  There is also a command
@code{\\MakeFramed} to make your own framed-style environments.")
    ;; The header states: "These macros may be freely transmitted, reproduced,
    ;; or modified for any purpose provided that this notice is left intact."
    (license (license:fsf-free "file://framed.sty"))))

(define-deprecated-package texlive-latex-framed texlive-framed)

(define-public texlive-g-brief
  (package
    (name "texlive-g-brief")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/g-brief/"
                   "source/latex/g-brief/"
                   "tex/latex/g-brief/")
             (base32
              "0sicgf3wjw5jymh5xjxby2hsilakhw2lqgywx0f2zax1z854xc2m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/g-brief")
    (synopsis "Letter document class")
    (description
     "This package is designed for formatting formless letters in German; it
can also be used for English (by those who can read the documentation).  There
are LaTeX 2.09 @code{documentstyle} and LaTeX 2e class files for both an old
and a new version of @code{g-brief}.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-g-brief texlive-g-brief)

(define-public texlive-galois
  (package
    (name "texlive-galois")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/galois/"
                   "source/latex/galois/"
                   "tex/latex/galois/")
             (base32
              "1324nw1r1aj6khz6fvrhd1p1sinadrd83j0s2q2fhnsgwp6sw94f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/galois")
    (synopsis "Typeset Galois connections")
    (description
     "The package deals with connections in two-dimensional style, optionally
in colour.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-galois texlive-galois)

(define-public texlive-gcite
  (package
    (name "texlive-gcite")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/gcite/"
                   "source/latex/gcite/"
                   "tex/latex/gcite/")
             (base32
              "0yb7sid13bx25yar3aw6pbf4jmmfi0gdmcd7ynf5hjh9qdfb3zyh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/gcite")
    (synopsis "Citations in a reader-friendly style")
    (description
     "The package allows citations in the German style, which is considered by
many to be particularly reader-friendly.  The citation provides a small amount
of bibliographic information in a footnote on the page where each citation is
made.  It combines a desire to eliminate unnecessary page-turning with the
look-up efficiency afforded by numeric citations.  The package makes use of
BibLaTeX, and is considered experimental.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-gcite texlive-gcite)

(define-public texlive-geometry
  (package
    (name "texlive-geometry")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/geometry/"
                   "source/latex/geometry/"
                   "tex/latex/geometry/")
             (base32
              "09jwdrg4s1c0gqmx7s57byw5kc09cna3li85y9ix0vxa6f6iqzi1")))
    (build-system texlive-build-system)
    (outputs '("out" "doc"))
    (propagated-inputs (list texlive-graphics texlive-iftex))
    (home-page "https://www.ctan.org/pkg/geometry")
    (synopsis "Flexible and complete interface to document dimensions")
    (description
     "This package provides an easy and flexible user interface to customize
page layout, implementing auto-centering and auto-balancing mechanisms so that
the users have only to give the least description for the page layout.  The
package knows about all the standard paper sizes, so that the user need not
know what the nominal real dimensions of the paper are, just its standard
name (such as a4, letter, etc.).  An important feature is the package's
ability to communicate the paper size it's set up to the output.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-geometry texlive-geometry)

(define-public texlive-mdwtools
  (package
    (name "texlive-mdwtools")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mdwtools/"
                   "source/latex/mdwtools/"
                   "tex/latex/mdwtools/")
             (base32
              "08wyw0k6r3fv7vdgwbcpq9ckifldy17fzhpar51s1qn0yib93zdg")))
    (build-system texlive-build-system)
    (outputs '("out" "doc"))
    (home-page "https://www.ctan.org/pkg/mdwtools")
    (synopsis "Miscellaneous tools by Mark Wooding")
    (description
     "This collection of tools includes: @code{atsupport} for short commands
starting with @code{@@}, macros to sanitize the OT1 encoding of the
@code{cmtt} fonts; a @code{doafter} command; improved @code{footnote} support;
@code{mathenv} for various alignment in maths; list handling; @code{mdwmath}
which adds some minor changes to LaTeX maths; a rewrite of LaTeX's
@code{tabular} and @code{array} environments; verbatim handling; and syntax
diagrams.")
    (license license:gpl3+)))

(define-deprecated-package texlive-latex-mdwtools texlive-mdwtools)

(define-public texlive-makecmds
  (package
    (name "texlive-makecmds")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/makecmds/" "source/latex/makecmds/"
                   "tex/latex/makecmds/")
             (base32
              "05y5n265in7mrbpgjxqg339l8r8dmp6lvl4k528pr3rkb8z94qaf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-amsfonts
                  texlive-cm))))
    (home-page "https://www.ctan.org/pkg/makecmds")
    (synopsis "TeX macro to define or redefine a command")
    (description "The package provides a @code{\\makecommand} command, which
is like @code{\\newcommand} or @code{\\renewcommand} except it
always (re)defines a command.  There is also @code{\\makeenvironment} and
@code{\\provideenvironment} for environments.")
    (license license:lppl1.3c+)))

(define-public texlive-makeindex
  (package
    (name "texlive-makeindex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/makeindex.1"
                   "doc/man/man1/makeindex.man1.pdf"
                   "doc/man/man1/mkindex.1"
                   "doc/man/man1/mkindex.man1.pdf"
                   "doc/support/makeindex/"
                   "makeindex/base/"
                   "tex/plain/makeindex/")
             (base32
              "0m01m0x1kf10yvzxgrkvpic0amsr0g6q2r2wsg5f4ngybq4y9gyi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/makeindexk")
    (synopsis "Makeindex development sources")
    (description
     "The package contains the development sources of MakeIndex.")
    (license
     (license:fsf-free "https://mirrors.ctan.org/indexing/makeindex/COPYING"))))

(define-public texlive-marginfix
  (package
    (name "texlive-marginfix")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/marginfix/"
                   "source/latex/marginfix/"
                   "tex/latex/marginfix/")
             (base32
              "0y6lmxm5ws2g0rqisvgdc8kg3whnvabrkl662sb4jvvaz02y3qdi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/marginfix")
    (synopsis "Patch @code{\\marginpar} to avoid overfull margins")
    (description
     "Authors using LaTeX to typeset books with significant margin material
often run into the problem of long notes running off the bottom of the page.
This package implements a solution to make @code{marginpars} just work by
keeping a list of floating inserts and arranging them intelligently in the
output routine.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-marginfix texlive-marginfix)

(define-public texlive-metalogo
  (package
    (name "texlive-metalogo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/metalogo/" "source/latex/metalogo/"
                   "tex/latex/metalogo/")
             (base32
              "1xzy982kc7k5n7gy019rk4hbvxli2mlf4s7h7s11diasmh5fa2gf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/metalogo")
    (synopsis "Extended TeX logo macros")
    (description
     "This package exposes spacing parameters for various TeX logos to the end
user, to optimise the logos for different fonts.  It is written especially for
XeLaTeX users.")
    (license license:lppl1.3c+)))

(define-public texlive-paralist
  (package
    (name "texlive-paralist")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/paralist/" "source/latex/paralist/"
                   "tex/latex/paralist/")
             (base32
              "003gs7rjm97vqh903wdjys3ls96kx45w3s4qghjk1fdjx6qxd32l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/paralist")
    (synopsis "@code{enumerate} and @code{itemize} within paragraphs")
    (description
     "The @code{paralist} package provides @code{enumerate} and @code{itemize}
environments that can be used within paragraphs to format the items either as
running text or as separate paragraphs with a preceding number or symbol.  It
also provides compacted versions of @code{enumerate} and @code{itemize}.")
    (license license:lppl1.0+)))

(define-public texlive-polyglossia
  (package
    (name "texlive-polyglossia")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/polyglossia/"
                   "fonts/misc/xetex/fontmapping/polyglossia/"
                   "source/latex/polyglossia/"
                   "tex/latex/polyglossia/")
             (base32
              "15rqqf0yb09qcs6ibsrkg5jbpzicxkpbj211p6qkfl2fcrc1gndv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-format "xelatex"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'substitute-fonts
            (lambda _
              ;; Use dummy fonts for documentation.  We will install
              ;; pre-generated one anyway. Moreover, adding appropriate fonts
              ;; would bring in some large dependencies, such as webkitgtk for
              ;; Amiri.
              (substitute* "source/latex/polyglossia/polyglossia.dtx"
                (("\\{Serto Jerusalem}") "{FreeSans}")
                (("\\{Amiri-Regular}") "{FreeSans}")
                (("\\{Noto Serif CJK SC}") "{FreeSans}")
                (("\\{GFSPolyglot.otf}") "{FreeSans}"))))
          (add-after 'unpack 'extend-texmf
            (lambda _
              ;; Extend the current TEXMF environment variable to make
              ;; Polyglossia own libraries visible.
              (setenv "GUIX_TEXMF"
                      (string-append (getcwd) ":"
                                     (getenv "GUIX_TEXMF"))))))))
    (native-inputs
     (list font-dejavu
           font-gnu-freefont
           font-linuxlibertine
           font-sil-ezra
           fontconfig                   ;for XDG_DATA_DIRS (to locate fonts)
           texlive-amiri
           texlive-babel
           texlive-bidi
           texlive-booktabs
           texlive-caption
           texlive-context
           texlive-fancyvrb
           texlive-graphics
           texlive-hyperref
           texlive-infwarerr
           texlive-kvoptions
           texlive-latex-fonts
           texlive-libertine
           texlive-metalogo
           texlive-microtype
           texlive-noto
           texlive-paralist
           texlive-pdftexcmds
           texlive-tex
           texlive-tools
           texlive-xcolor
           texlive-xetex
           texlive-zref))
    (propagated-inputs
     (list texlive-etoolbox
           texlive-filehook
           texlive-fontspec
           texlive-iftex
           texlive-makecmds
           texlive-xkeyval))
    (home-page "https://www.ctan.org/pkg/polyglossia")
    (synopsis "Alternative to Babel for XeLaTeX and LuaLaTeX")
    (description "This package provides a complete Babel replacement for users
of LuaLaTeX and XeLaTeX.  It includes support for over 70 different languages,
some of which in different regional or national varieties, or using a
different writing system.  It enables:
@itemize
@item
Loading the appropriate hyphenation patterns.
@item
Setting the script and language tags of the current font (if possible and
available), using the package @code{fontspec}.
@item
Switching to a font assigned by the user to a particular script or language.
@item
Adjusting some typographical conventions in function of the current language
(such as @code{afterindent}, @code{frenchindent}, spaces before or after
punctuation marks, etc.)
@item
Redefining the document strings (like @samp{chapter}, @samp{figure},
@samp{bibliography}).  Adapting the formatting of dates (for non-gregorian
calendars via external packages bundled with @code{polyglossia}: currently the
Hebrew, Islamic and Farsi calendars are supported).
@item
For languages that have their own numeration system, modifying the formatting
of numbers appropriately.
@item
Ensuring the proper directionality if the document contains languages
written from right to left.
@end itemize")
    (license license:expat)))

(define-deprecated-package texlive-latex-polyglossia texlive-polyglossia)

(define-public texlive-supertabular
  (package
    (name "texlive-supertabular")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/supertabular/"
                   "source/latex/supertabular/"
                   "tex/latex/supertabular/")
             (base32
              "1z4kyx20w2zvn6c5a7p702pxj254f2pwlk7x815gzlcc6563js6a")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/supertabular")
    (synopsis "Multi-page tables package")
    (description
     "This package was a predecessor of @code{longtable}; the newer
package (designed on quite different principles) is easier to use and more
flexible, in many cases, but @code{supertabular} retains its usefulness in
a few situations where longtable has problems.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-supertabular texlive-supertabular)

(define-public texlive-texinfo
  (package
    (name "texlive-texinfo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/texinfo/")
             (base32
              "1wdrqwksbhxxx275mzhcr3mc67f76nbflplqs4y1xx67iw724dmx")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/texinfo")
    (synopsis "Texinfo documentation system")
    (description
     "Texinfo is the preferred format for documentation in the GNU project;
the format may be used to produce online or printed output from a single
source.  The Texinfo macros may be used to produce printable output using TeX;
other programs in the distribution offer online interactive use (with
hypertext linkages in some cases).")
    (license license:gpl3+)))

(define-deprecated-package texlive-tex-texinfo texlive-texinfo)

(define-public texlive-textcase
  (package
    (name "texlive-textcase")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/textcase/" "source/latex/textcase/"
                   "tex/latex/textcase/")
             (base32
              "15jb7r1p7vjm1i02lf5c9g5i7fcgkc7a6b59jhyzzk2l7ch41d7f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/textcase")
    (synopsis "Case conversion ignoring mathematics, etc")
    (description
     "The @code{textcase} package offers commands @code{\\MakeTextUppercase}
and @code{\\MakeTextLowercase} are similar to the standard
@code{\\MakeUppercase} and @code{\\MakeLowercase}, but they do not change the
case of any sections of mathematics, or the arguments of @code{\\cite},
@code{\\label} and @code{\\ref} commands within the argument.  A further
command @code{\\NoCaseChange} does nothing but suppress case change within its
argument, so to force uppercase of a section including an environment, one
might say:

@example
\\MakeTextUppercase{...\\NoCaseChange{\\begin{foo}} ...\\NoCaseChange{\\end{foo}}...}
@end example")
    (license license:lppl)))

(define-public texlive-upquote
  (package
    (name "texlive-upquote")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/upquote/"
                   "source/latex/upquote/"
                   "tex/latex/upquote/")
             (base32
              "1manbljqx2859wq9by6bpcx4rnxvc596a05d21cw464484f8a8z2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/upquote")
    (synopsis "Show realistic quotes in verbatim")
    (description
     "Typewriter-style fonts are best for program listings, but Computer
Modern Typewriter prints @samp{`} and @samp{'} as bent opening and closing
single quotes.  Other fonts, and most programming languages, print @samp{`} as
a grave accent and @samp{'} upright; @samp{'} is used both to open and to
close quoted strings.  The package switches the typewriter font to Computer
Modern Typewriter in OT1 encoding, and modifies the behaviour of
@code{verbatim}, @code{verbatim*}, @code{\\verb}, and @code{\\verb*} to print
in the expected way.  It does this regardless of other fonts or encodings in
use, so long as the package is loaded after the other fonts were.  The package
does not affect @code{\\tt}, @code{\\texttt}, etc.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-upquote texlive-upquote)

(define-public texlive-anysize
  (package
    (name "texlive-anysize")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/anysize/"
                   "tex/latex/anysize/")
             (base32
              "155s0v82zpkmv97kwqhhfw52230hka9zl3wzjw1d5ayxd4n11bxq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/anysize")
    (synopsis "Simple package to set up document margins")
    (description
     "This is a simple package to set up document margins.  This package is
considered obsolete; alternatives are the @code{typearea} package from the
@code{koma-script} bundle, or the @code{geometry} package.")
    (license license:public-domain)))

(define-deprecated-package texlive-latex-anysize texlive-anysize)

(define-public texlive-appendix
  (package
    (name "texlive-appendix")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/appendix/"
                   "source/latex/appendix/"
                   "tex/latex/appendix/")
             (base32
              "1vqkqpzs7bc6pbjnafakrwayjyfx9mvadrqxccdf549m8172qvzk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/appendix")
    (synopsis "Extra control of appendices")
    (description
     "The @code{appendix} package provides various ways of formatting the
titles of appendices.  Also (sub)appendices environments are provided that can
be used, for example, for per chapter/section appendices.  An
@code{appendices} environment is provided which can be used instead of the
@code{\\appendix} command.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-appendix texlive-appendix)

(define-public texlive-bookman
  (package
    (name "texlive-bookman")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/bookman/"
                   "fonts/afm/adobe/bookman/"
                   "fonts/afm/urw/bookman/"
                   "fonts/map/dvips/bookman/"
                   "fonts/tfm/adobe/bookman/"
                   "fonts/tfm/urw35vf/bookman/"
                   "fonts/type1/urw/bookman/"
                   "fonts/vf/adobe/bookman/"
                   "fonts/vf/urw35vf/bookman/"
                   "tex/latex/bookman/")
             (base32
              "12wkjwpzxn1a1k3bb41gpnky1jjsh7gzj4xahsjd087fpmrsj9p9")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Bookman font")
    (description
     "This package provides a drop-in replacement for the Bookman font from
Adobe's basic set")
    (license license:gpl3+)))

(define-public texlive-bookmark
  (package
    (name "texlive-bookmark")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/bookmark/"
                   "source/latex/bookmark/"
                   "tex/latex/bookmark/")
             (base32
              "111sjwabcbr8ry8fh94ywpzska032y8r4iz4waxa4kf5l3k0p4bs")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:build-targets #~(list "bookmark.dtx")))
    (home-page "https://www.ctan.org/pkg/bookmark")
    (synopsis "Bookmark (outline) organization for @code{hyperref}")
    (description
     "This package implements a new bookmark (outline) organization for the
@code{hyperref} package.  Bookmark properties such as style and color.  Other
action types are available (URI, GoToR, Named).")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-bookmark texlive-bookmark)

(define-public texlive-changebar
  (package
    (name "texlive-changebar")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/changebar/"
                   "source/latex/changebar/"
                   "tex/latex/changebar/")
             (base32
              "0k6r3f6xqbl5gr3i2kwh82lkbwk76gwyfvj7nsvzi1awjk84hqd2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/changebar")
    (synopsis "Generate changebars in LaTeX documents")
    (description
     "Identify areas of text to be marked with changebars with the
@code{\\cbstart} and @code{\\cbend} commands; the bars may be coloured.  The
package uses drivers to place the bars; the available drivers can work with
@code{dvitoln03}, @code{dvitops}, @code{dvips}, the emTeX and TeXtures DVI
drivers, and VTeX and pdfTeX.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-changebar texlive-changebar)

(define-public texlive-checkcites
  (package
    (name "texlive-checkcites")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/support/checkcites/"
                   "scripts/checkcites/")
             (base32
              "0zfghyyq86xrbnb9bzl7z1p96s0n255b39v2srqslb2z37ppvjyz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/checkcites")
    (synopsis "Check citation commands in a document")
    (description
     "The package provides a Lua script written for the sole purpose of
detecting undefined and unused references from LaTeX auxiliary or bibliography
files.")
    (license license:lppl1.3+)))

(define-public texlive-chickenize
  (package
    (name "texlive-chickenize")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/chickenize/"
                   "source/luatex/chickenize/"
                   "tex/luatex/chickenize/")
             (base32
              "055lkxc2igr0qh1lcdbnh2w0z92v3wkjgp1hpbbrj8r5kvpm7nvy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/chickenize")
    (synopsis "Use Lua callbacks for some textual effects")
    (description
     "The package allows manipulations of any LuaTeX document.  Most of the
package's functions are merely for fun or educational use, but some
functions (for example, @code{colorstretch} for visualising the badness and
font expansion of each line, and @code{letterspaceadjust} doing what its name
says) could be useful in a normal LuaTeX document.")
    (license license:lppl1.3+)))

(define-public texlive-chinese-jfm
  (package
    (name "texlive-chinese-jfm")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/chinese-jfm/"
                   "tex/luatex/chinese-jfm/")
             (base32
              "0ixlwivijp6vvzn2j3drr13hk0ylslcd9ws8df24abda93wjm35r")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/chinese-jfm")
    (synopsis "Luatexja-jfm files for Chinese typesetting")
    (description
     "ChineseJFM is a series of @code{luatexja-jfm} files for better Chinese
typesetting, providing @code{quanjiao}, @code{banjiao}, and @code{kaiming}
three styles and other fancy features.  It can be used for both horizontal and
vertical writing mode in Simplified/Traditional Chinese or Japanese fonts.")
    (license license:expat)))

(define-public texlive-cloze
  (package
    (name "texlive-cloze")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/cloze/" "scripts/cloze/"
                   "source/luatex/cloze/" "tex/luatex/cloze/")
             (base32
              "0i0bsflqgw14bik1r8qlx2287fx6lv8jmha57bx54d7icaswssf9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cloze")
    (synopsis "Lualatex package for creating cloze texts")
    (description
     "This is a LuaTeX or LuaLaTeX package for generating cloze texts.  The
main feature of the package is that the formatting doesn't change when using
the hide and show options.")
    (license license:lppl1.3+)))

(define-public texlive-cmap
  (package
    (name "texlive-cmap")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/cmap/"
                   "tex/latex/cmap/")
             (base32
              "1hag26l3g9mpmmy1kn7lrnfzzr8k0hpm259qp087smggykvsjc4v")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cmap")
    (synopsis "Make PDF files searchable and copyable")
    (description
     "This package embeds CMap tables into PDF files to make search and
copy-and-paste functions work properly.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-cmap texlive-cmap)

(define-public texlive-colorprofiles
  (package
    (name "texlive-colorprofiles")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/colorprofiles/"
                   "tex/generic/colorprofiles/")
             (base32
              "1nxbds0jhn5wvf50iy1a4mpfgk56587kqvs4wxf08ysvqx6xiaxv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/colorprofiles")
    (synopsis "Collection of free ICC profiles")
    (description
     "This package collects ICC profiles that can be used by color profile
aware applications or tools like the @code{pdfx} package, as well as TeX and
LaTeX packages to access them.")
    (license license:lppl1.2+)))        ;per "colorprofiles.sty"

(define-public texlive-colortbl
  (package
    (name "texlive-colortbl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/colortbl/" "source/latex/colortbl/"
                   "tex/latex/colortbl/")
             (base32
              "17hslagzpbi5jq08sjinrc3cv65m8g667mb2zlq89fq5ix9808vx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/colortbl")
    (synopsis "Add colour to LaTeX tables")
    (description
     "The package allows rows and columns to be coloured, and even
individual cells.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-colortbl texlive-colortbl)

(define-public texlive-combofont
  (package
    (name "texlive-combofont")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/combofont/"
                   "tex/lualatex/combofont/")
             (base32
              "05p044znavjjd7cpgjx46i8n6b56rpybhixqs9yaam86nb70ha7n")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/combofont")
    (synopsis "Add NFSS-declarations of combo fonts to LuaLaTeX documents")
    (description
     "This highly experimental package can be used to add NFSS-declarations of
combo fonts to LuaLaTeX documents.")
    (license license:lppl1.3c)))

(define-public texlive-cstypo
  (package
    (name "texlive-cstypo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/cstypo/"
                   "tex/lualatex/cstypo/" "tex/luatex/cstypo/")
             (base32
              "07cgcda6jcmkvhp90n4a8g4x98jmzwf4199gp6nh19718v6n15j8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cstypo")
    (synopsis "Czech typography rules enforced through LuaTeX hooks")
    (description
     "This package provides macros that enforce basic Czech typography rules
through Lua hooks available in LuaTeX.")
    (license license:expat)))

(define-public texlive-fancybox
  (package
    (name "texlive-fancybox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fancybox/"
                   "tex/latex/fancybox/")
             (base32
              "0pb1j0a1va8yhrzig7dwrd8jgq39mbcpygl810jhrv8pl473mfmn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fancybox")
    (synopsis "Variants of @code{\\fbox} and other games with boxes")
    (description
     "This package provides variants of @code{\\fbox}: @code{\\shadowbox},
@code{\\doublebox}, @code{\\ovalbox}, @code{\\Ovalbox}, with helpful tools for
using box macros and flexible verbatim macros.  You can box mathematics,
floats, center, flushleft, and flushright, lists, and pages.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-fancybox texlive-fancybox)

(define-public texlive-fancyhdr
  (package
    (name "texlive-fancyhdr")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fancyhdr/" "source/latex/fancyhdr/"
                   "tex/latex/fancyhdr/")
             (base32
              "0asx5l8kx1zsvja5arnbspr37hwmmjp01837kfrsy7dsm8wfclgr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fancyhdr")
    (synopsis "Extensive control of page headers and footers in LaTeX2e")
    (description
     "The package provides extensive facilities, both for constructing headers
and footers, and for controlling their use (for example, at times when LaTeX
would automatically change the heading style in use).")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-fancyhdr texlive-fancyhdr)

(define-public texlive-fancyref
  (package
    (name "texlive-fancyref")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fancyref/"
                   "source/latex/fancyref/"
                   "tex/latex/fancyref/")
             (base32
              "0njgl53f5farwbd8xkw8im8id0scf6agbfqcdjkbqlk540vdzwbp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fancyref")
    (synopsis "LaTeX package for fancy cross-referencing")
    (description
     "This package provides fancy cross-referencing support, based on the
package's reference commands (@code{\\fref} and @code{\\Fref}) that recognise
what sort of object is being referenced.")
    (license license:gpl3+)))

(define-public texlive-float
  (package
    (name "texlive-float")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/float/"
                   "source/latex/float/"
                   "tex/latex/float/")
             (base32
              "1rfyvk1n83zsmrrp0x643052nrjb00cj935d2cpm37x4pz649f5d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/float")
    (synopsis "Improved interface for floating objects")
    (description
     "The @code{float} package improves the interface for defining floating
objects such as figures and tables.  It introduces the boxed float, the ruled
float and the plaintop float.  You can define your own floats and improve the
behaviour of the old ones.  The package also provides the @samp{H} float
modifier option of the obsolete @code{here} package.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-float texlive-float)

(define-public texlive-footmisc
  (package
    (name "texlive-footmisc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/footmisc/" "source/latex/footmisc/"
                   "tex/latex/footmisc/")
             (base32
              "1vs69z6hqvx9rxqqr0aqs56wvl0y0102szq954hb9gyqzwj2q225")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/footmisc")
    (synopsis "Range of footnote options")
    (description
     "This is a collection of ways to change the typesetting of footnotes.
The package provides means of changing the layout of the footnotes themselves,
a way to number footnotes per page, to make footnotes disappear in a
\"moving\" argument, and to deal with multiple references to footnotes from
the same place.  The package also has a range of techniques for labelling
footnotes with symbols rather than numbers.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-footmisc texlive-footmisc)

(define-public texlive-footnotehyper
  (package
    (name "texlive-footnotehyper")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/footnotehyper/"
                   "source/latex/footnotehyper/"
                   "tex/latex/footnotehyper/")
             (base32
              "0f8d13zr07bl295rvpagj99s9fn4dgrcjzv1xpjmla3h6xhrv914")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/footnotehyper")
    (synopsis "hyperref aware @file{footnote.sty}")
    (description
     "The @code{footnote} package by Mark Wooding dates back to 1997 and has
not been made @code{hyperref} compatible.  The aim of the present package is
to do that.")
    (license license:lppl1.3c)))

(define-public texlive-letltxmacro
  (package
    (name "texlive-letltxmacro")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/letltxmacro/"
                   "source/latex/letltxmacro/"
                   "tex/latex/letltxmacro/")
             (base32
              "16bmwsng9p80jf78sdmib24apwnw3raw306cs1ms50z5s9dsfdby")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/letltxmacro")
    (synopsis "Let assignment for LaTeX macros")
    (description
     "TeX's @code{\\let} assignment does not work for LaTeX macros with
optional arguments or for macros that are defined as robust macros by
@code{\\DeclareRobustCommand}.  This package defines @code{\\LetLtxMacro} that
also takes care of the involved internal macros.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-letltxmacro texlive-letltxmacro)

(define-public texlive-frankenstein
  (package
    (name "texlive-frankenstein")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bib/frankenstein/"
                   "bibtex/bst/frankenstein/"
                   "doc/latex/frankenstein/"
                   "source/latex/frankenstein/"
                   "tex/latex/frankenstein/")
             (base32
              "1x494vl4acl0bhfshs96ap8j47xk4m4njfincfhg2b0mi7l5mj1i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:modules '((guix build texlive-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-TEXINPUTS
            ;; The ".ins" files strip comments from ".sty", turning them into
            ;; faster ".stq" (and ".bsq") files.  Unfortunately, the ".ins"
            ;; and the ".sty" files are not located in the same
            ;; directory. This phase extends TEXINPUTS so everyone can see
            ;; each other, including the docstrip utility.
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TEXINPUTS"
                      (let ((cwd (getcwd)))
                        (string-append
                         cwd "/tex/latex/frankenstein//:"
                         cwd "/source/latex/frankenstein//:"
                         (string-join
                          (map (match-lambda ((_ . dir) dir)) inputs)
                          "//:"))))))
          (add-before 'install 'install-faster-files
            (lambda _
              ;; Replace ".sty" and ".bst" files with their faster
              ;; counterpart.
              (copy-file "build/achicago.bsq"
                         "bibtex/bst/frankenstein/achicago.bst")
              ;; "build/tex/.../xxx.stq" -> "tex/.../xxx.sty"
              (for-each (lambda (file)
                          (copy-file file
                                     (string-append "tex/latex/frankenstein/"
                                                    (basename file ".stq")
                                                    ".sty")))
                        (find-files "build/tex" "\\.stq$")))))))
    (home-page "https://ctan.org/pkg/frankenstein")
    (synopsis "Collection of LaTeX packages")
    (description
     "Frankenstein is a bundle of LaTeX packages serving various purposes
and a BibTeX bibliography style.  The individual packages are: @code{abbrevs},
@code{achicago}, @code{achicago} bibstyle, @code{attrib}, @code{blkcntrl},
@code{compsci}, @code{dialogue}, @code{lips}, @code{moredefs},
@code{newclude}, @code{slemph} and @code{titles}.")
    ;; README mentions an unspecified version of GNU GPL and points to
    ;; COPYING, which is missing. However, the individual files mention LPPL
    ;; 1.2 or later.
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-frankenstein texlive-frankenstein)

(define-public texlive-kantlipsum
  (package
    (name "texlive-kantlipsum")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/kantlipsum/"
                   "source/latex/kantlipsum/"
                   "tex/latex/kantlipsum/")
             (base32
              "1bz08i8b7ihzd2qi4v9r9kjl2kr5a3l516lqb36spxyyrlmmwv4p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/kantlipsum")
    (synopsis "Generate sentences in Kant's style")
    (description
     "The package spits out sentences in Kantian style; the text is provided
by the Kant generator for Python by Mark Pilgrim, described in the book ``Dive
into Python''.  The package is modelled on @code{lipsum}, and may be used for
similar purposes.")
    (license license:lppl1.3c)))

(define-public texlive-kurier
  (package
    (name "texlive-kurier")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/kurier/"
                   "fonts/afm/nowacki/kurier/"
                   "fonts/enc/dvips/kurier/"
                   "fonts/map/dvips/kurier/"
                   "fonts/opentype/nowacki/kurier/"
                   "fonts/tfm/nowacki/kurier/"
                   "fonts/type1/nowacki/kurier/"
                   "tex/latex/kurier/"
                   "tex/plain/kurier/")
             (base32
              "0j89iv0yyy33dyyka0w0v5jykl41sdn7plxrrxa91qmxzqvi2y67")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/kurier")
    (synopsis "Two-element sans-serif typeface")
    (description
     "Kurier is a two-element sans-serif typeface.  It was designed for
a diploma in typeface design at the Warsaw Academy of Fine Arts under the
supervision of Roman Tomaszewski.  This distribution contains a significantly
extended set of characters covering the following modern alphabets:
latin (including Vietnamese), Cyrillic and Greek as well as a number of
additional symbols (including mathematical symbols).  The fonts are prepared
in Type 1 and OpenType formats.  For use with TeX the following encoding files
have been prepared: T1 (ec), T2 (abc), and OT2--Cyrillic, T5 (Vietnamese),
OT4, QX, texansi and--nonstandard (IL2 for the Czech fonts), as well as
supporting macros and files defining fonts for LaTeX.")
    (license license:gfl1.0)))

(define-public texlive-lipsum
  (package
    (name "texlive-lipsum")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/lipsum/" "source/latex/lipsum/"
                   "tex/latex/lipsum/")
             (base32
              "07kcma66p3s68baygzvgcmb7jvwaan7jj6s3hxmx42npcfsp57s3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lipsum")
    (synopsis "Easy access to the Lorem Ipsum dummy text")
    (description
     "This package gives you easy access to the Lorem Ipsum dummy text; an
option is available to separate the paragraphs of the dummy text into
TeX-paragraphs.  All the paragraphs are taken with permission from
@url{http://lipsum.com/}.")
    (license license:lppl1.3+)))

(define-public texlive-listings
  (package
    (name "texlive-listings")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/listings/" "source/latex/listings/"
                   "tex/latex/listings/")
             (base32
              "12db0jnambf3j2c2drnbjz369iwssbbcd7yqjcv0wrzq284lzc0m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    ;; Do not build intermediate "lstdrvrs.ins".
    (arguments (list #:build-targets '(list "listings.ins")))
    (home-page "https://ctan.org/pkg/listings")
    (synopsis "Typeset source code listings using LaTeX")
    (description
     "The package enables the user to typeset programs (programming code)
within LaTeX; the source code is read directly by TeX---no front-end processor
is needed.  Keywords, comments and strings can be typeset using different
styles.  Support for @code{hyperref} is provided.")
    (license license:lppl1.3+)))

(define-public texlive-listingsutf8
  (package
    (name "texlive-listingsutf8")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/listingsutf8/"
                   "source/latex/listingsutf8/"
                   "tex/latex/listingsutf8/")
             (base32
              "152gzkzm7sl3bvggmmfcj1pw74vc40s2kpkbp01fd9i0d0v60wma")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/listingsutf8")
    (synopsis "Allow UTF-8 in listings input")
    (description
     "Package @code{listings} does not support files with multi-byte encodings
such as UTF-8.  In the case of @code{\\lstinputlisting}, a simple workaround
is possible if a one-byte encoding exists that the file can be converted to.
The package requires the e-TeX extensions under pdfTeX (in either PDF or DVI
output mode).")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-listings texlive-listings)

(define-public texlive-jknapltx
  (package
    (name "texlive-jknapltx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/jknapltx/"
                   "tex/latex/jknapltx/")
             (base32
              "0as43yqq123cacxhvp4sbdp4ka3cyp2spmxwayqny0fh5rsk6qaq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/jknapltx")
    (synopsis "Miscellaneous packages by Joerg Knappen")
    (description
     "This package provides miscellaneous macros by Joerg Knappen,
including: represent counters in greek; Maxwell's non-commutative division;
@code{latin1jk}, @code{latin2jk} and @code{latin3jk}, which are
@code{inputenc} definition files that allow verbatim input in the respective
ISO Latin codes; blackboard bold fonts in maths; use of RSFS fonts in maths;
extra alignments for @code{\\parboxes}; swap Roman and Sans fonts;
transliterate semitic languages; patches to make (La)TeX formulae embeddable
in SGML; use maths minus in text as appropriate; simple Young tableaux.")
    (license license:gpl3+)))

(define-deprecated-package texlive-latex-jknapltx texlive-jknapltx)
(define-deprecated-package texlive-jknappen texlive-jknapltx)

(define-public texlive-jmn
  (package
    (name "texlive-jmn")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/afm/jmn/hans/" "fonts/enc/dvips/jmn/"
                   "fonts/map/dvips/jmn/" "fonts/tfm/jmn/hans/"
                   "fonts/type1/jmn/hans/")
             (base32
              "0iq5ky3llx50smw80lpylv11jmqc51m5yrhlslz3sakmgdqgg1yi")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/jmn")
    (synopsis "Special fonts for ConTeXt")
    (description "This ConTeXt module provides special fonts.")
    (license license:bsd-2)))           ;as the whole ConTeXt project

(define-public texlive-kvoptions
  (package
    (name "texlive-kvoptions")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/kvoptions/"
                   "source/latex/kvoptions/"
                   "tex/latex/kvoptions/")
             (base32
              "14f19c53s5m067vp25h7yk1f209h1xm352zkhzv6qk5xc0ckkbxm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/kvoptions")
    (synopsis "Key/value format for package options")
    (description
     "This package provides facilities for using key-value format in
package options.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-kvoptions texlive-kvoptions)

(define-public texlive-ec
  (package
    (name "texlive-ec")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/ec/"
                   "fonts/source/jknappen/ec/"
                   "fonts/tfm/jknappen/ec/")
             (base32
              "1cyi0vv9dnp45s0ilsrbkyznj9ji62s5bhkqgh49461mv2f8qj6p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/ec")
    (synopsis "Computer modern fonts in T1 and TS1 encodings")
    (description
     "The EC fonts are European Computer Modern Fonts, supporting the complete
LaTeX T1 encoding defined at the 1990 TUG conference hold at Cork/Ireland.
These fonts are intended to be stable with no changes being made to the tfm
files.  The set also contains a Text Companion Symbol font, called @code{tc},
featuring many useful characters needed in text typesetting, for example
oldstyle digits, currency symbols (including the newly created Euro symbol),
the permille sign, copyright, trade mark and servicemark as well as a copyleft
sign, and many others.  The fonts are available in (traced) Adobe Type
1 format, as part of the @code{cm-super} bundle.  The other Computer
Modern-style T1-encoded Type 1 set, Latin Modern, is not actually a direct
development of the EC set, and differs from the EC in a number of
particulars.")
    (license (license:fsf-free "https://www.tug.org/svn/texlive/tags/\
texlive-2019.3/Master/texmf-dist/doc/fonts/ec/copyrite.txt"))))

(define-deprecated-package texlive-fonts-ec texlive-ec)

(define-public texlive-ekdosis
  (package
    (name "texlive-ekdosis")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/ekdosis/"
                   "source/lualatex/ekdosis/"
                   "tex/lualatex/ekdosis/")
             (base32
              "0bzydy6gcmikqsdiaji30a2ycifzaafbg0ccv5lq5an7rv2hajmj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ekdosis")
    (synopsis "Typesetting TEI-xml compliant Critical Editions")
    (description
     "@code{ekdosis} is a LuaLaTeX package designed for multilingual critical
editions.  It can be used to typeset texts and different layers of critical
notes in any direction accepted by LuaTeX. Texts can be arranged in running
paragraphs or on facing pages, in any number of columns which in turn can be
synchronized or not.  In addition to printed texts, @code{ekdosis} can convert
@file{.tex} source files so as to produce TEI XML-compliant critical editions.
Database-driven encoding under LaTeX then allows extraction of texts entered
segment by segment according to various criteria: main edited text, variant
readings, translations or annotated borrowings between texts.")
    (license (list license:gpl3+ license:fdl1.3+))))

(define-public texlive-emoji
  (package
    (name "texlive-emoji")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/emoji/" "tex/latex/emoji/")
             (base32
              "17i2kjb1rlgj2ipg6bkni1n8l1yhhnm1m96ynj1nv8gkmgkfklal")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/emoji")
    (synopsis "Emoji support in (Lua)LaTeX")
    (description
     "This package allows users to typeset emojis in LaTeX documents.  It
requires the LuaHBTeX engine.")
    (license license:lppl1.3c)))

(define-public texlive-emojicite
  (package
    (name "texlive-emojicite")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/emojicite/"
                   "tex/lualatex/emojicite/")
             (base32
              "0wfr4alglwhsp728fp91qxdmc9413c6k9j7hgg0qk3j4w464srbj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/emojicite")
    (synopsis "Add emojis to citations")
    (description "This package adds emojis to citations.")
    (license license:lppl1.3c)))

(define-public texlive-enigma
  (package
    (name "texlive-enigma")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/enigma/enigma/"
                   "scripts/context/lua/third/enigma/mtx-t-enigma.lua/"
                   "tex/context/third/enigma/t-enigma.mkv/"
                   "tex/generic/enigma/"
                   "tex/latex/enigma/"
                   "tex/plain/enigma/")
             (base32
              "1rnp2c903bfgq0gqshaccd1zxcb0zpbk2sh7w3lwynpp0zx8d435")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/enigma")
    (synopsis "Encrypt documents with a three rotor Enigma")
    (description
     "The package provides historical encryption (Enigma cipher) for
LuaTeX-based formats.")
    (license license:bsd-2)))

(define-public texlive-ae
  (package
    (name "texlive-ae")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/ae/" "fonts/tfm/public/ae/"
                   "fonts/vf/public/ae/" "source/fonts/ae/"
                   "tex/latex/ae/")
             (base32
              "1xkzg381y0avdq381r2m990wp27czkdff0qkvsp2n5q62yc0bdsw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ae")
    (synopsis "Virtual fonts for T1 encoded CMR-fonts")
    (description
     "This package provides a set of virtual fonts which emulates T1 coded
fonts using the standard CM fonts.  The package name, AE fonts, supposedly
stands for \"Almost European\".  The main use of the package was to produce
PDF files using Adobe Type 1 versions of the CM fonts instead of bitmapped EC
fonts.  Note that direct substitutes for the bitmapped EC fonts are available,
via the CM-super, Latin Modern and (in a restricted way) CM-LGC font sets.")
    (license license:lppl1.3+)))

(define-public texlive-aeguill
  (package
    (name "texlive-aeguill")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/aeguill/" "tex/latex/aeguill/")
             (base32
              "0dbl2dky8gbax9blj0nvk23y2pwkmyikxnbbn27zivpamwc0j8nc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/aeguill")
    (synopsis "Add several kinds of guillemets to the @code{ae} fonts")
    (description
     "The package enables the user to add guillemets from several
source (Polish cmr, Cyrillic cmr, lasy and ec) to the @code{ae} fonts.  This
was useful when the @code{ae} fonts were used to produce PDF files, since the
additional guillemets exist in fonts available in Adobe Type 1 format.")
    (license license:lppl)))

(define-public texlive-incgraph
  (package
    (name "texlive-incgraph")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/incgraph/" "tex/latex/incgraph/")
             (base32
              "18ygl211wpnx433xy4v3jyl7wn9vn0dw23m709xs01kq7pwmsz3i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/incgraph")
    (synopsis "Sophisticated graphics inclusion in a PDF document")
    (description
     "The package provides tools for including graphics at the full size of
the output medium, or for creating pages whose size is that of the graphic
they contain.  A principal use case is documents that require inclusion
of (potentially many) scans or photographs.  Bookmarking is especially
supported.  The tool box has basic macros and a convenience user interface
that wraps @code{\\includegraphics}.")
    (license license:lppl1.3+)))

(define-public texlive-inconsolata
  (package
    (name "texlive-inconsolata")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/inconsolata/"
                   "fonts/enc/dvips/inconsolata/"
                   "fonts/map/dvips/inconsolata/"
                   "fonts/opentype/public/inconsolata/"
                   "fonts/tfm/public/inconsolata/"
                   "fonts/type1/public/inconsolata/"
                   "tex/latex/inconsolata/")
             (base32
              "19lvma52vk7x8d7j4s9ymjwm3w2k08860fh6dkzn76scgpdm4wlb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/inconsolata")
    (synopsis "Monospaced font with support files for use with TeX")
    (description
     "Inconsolata is a monospaced font designed by Raph Levien.  This package
contains the font (in both Adobe Type 1 and OpenType formats) in regular and
bold weights, with additional glyphs and options to control slashed zero,
upright quotes and a shapelier lower-case L, plus metric files for use with
TeX, and LaTeX font definition and other relevant files.")
    (license (list license:lppl1.3+
                   license:silofl1.1
                   license:asl2.0))))

(define-public texlive-innerscript
  (package
    (name "texlive-innerscript")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/innerscript/"
                   "source/lualatex/innerscript/"
                   "tex/lualatex/innerscript/")
             (base32
              "1nq2il8av1169is3kbq761375vk4znb2cc3f8vk9ab3fh5vqkcjv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-format "lualatex"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-documentation-build
            ;; Only extract the ".sty" file.  Documentation build fails with
            ;; "Command \code already defined" error.
            (lambda _
              (substitute* "source/lualatex/innerscript/innerscript.dtx"
                (("\\DocInput\\{innerscript.dtx\\}") "")))))))
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-amsfonts
                  texlive-booktabs
                  texlive-epstopdf-pkg
                  texlive-etoolbox
                  texlive-geometry
                  texlive-hypdoc
                  texlive-hyperref
                  texlive-infwarerr
                  texlive-kvoptions
                  texlive-microtype
                  texlive-pdftexcmds))))
    (home-page "https://ctan.org/pkg/innerscript")
    (synopsis "Modifies automatic mathematics spacing")
    (description
     "This package modifies two aspects of TeX's automatic interatom
mathematics spacing.  It uses LuaTeX's @code{\\Umath} primitives to make
superscripts and subscripts more closely resemble @code{\\textstyle} and
@code{\\displaystyle} math and to treat @code{\\mathinner} subformulas as
@code{\\mathord}, effectively eliminating this class.")
    (license license:lppl1.3c)))

(define-public texlive-interpreter
  (package
    (name "texlive-interpreter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/interpreter/"
                   "tex/luatex/interpreter/")
             (base32
              "03h6bjhvbl3bfkiyssplivqmn2986asv8b1jvr1ahsh7p04bkk4j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/interpreter")
    (synopsis "Translate input files on the fly")
    (description
     "The package preprocesses input files to a Lua(La)TeX run, on the fly.
The user defines Lua regular expressions to search for patterns and modify
input lines (or entire paragraphs) accordingly, before TeX reads the material.
In this way, documents may be prepared in a non-TeX language (e.g., some
lightweight markup language) and turned into proper TeX for processing.")
    (license license:lppl)))

(define-public texlive-times
  (package
    (name "texlive-times")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/times/"
                   "fonts/afm/adobe/times/"
                   "fonts/afm/urw/times/"
                   "fonts/map/dvips/times/"
                   "fonts/tfm/adobe/times/"
                   "fonts/tfm/urw35vf/times/"
                   "fonts/type1/urw/times/"
                   "fonts/vf/adobe/times/"
                   "fonts/vf/urw35vf/times/"
                   "tex/latex/times/")
             (base32
              "13g41a7vbkvsf7ki9dgl7qm100w382mnlqkcngwgl3axp6s5s8l0")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Times font")
    (description
     "This package provides a drop-in replacement for the Times font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-deprecated-package texlive-fonts-adobe-times texlive-times)

(define-public texlive-palatino
  (package
    (name "texlive-palatino")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/palatino/"
                   "fonts/afm/adobe/palatino/"
                   "fonts/afm/urw/palatino/"
                   "fonts/map/dvips/palatino/"
                   "fonts/tfm/adobe/palatino/"
                   "fonts/tfm/urw35vf/palatino/"
                   "fonts/type1/urw/palatino/"
                   "fonts/vf/adobe/palatino/"
                   "fonts/vf/urw35vf/palatino/"
                   "tex/latex/palatino/")
             (base32
              "12jc0av7v99857jigmva47qaxyllhpzsnqis10n0qya2kz44xf22")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Palatino font")
    (description
     "This package provides a drop-in replacement for the Palatino font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-deprecated-package texlive-fonts-adobe-palatino texlive-palatino)

(define-public texlive-poltawski
  (package
    (name "texlive-poltawski")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/poltawski/"
                   "fonts/afm/gust/poltawski/"
                   "fonts/enc/dvips/poltawski/"
                   "fonts/map/dvips/poltawski/"
                   "fonts/opentype/gust/poltawski/"
                   "fonts/tfm/gust/poltawski/"
                   "fonts/type1/gust/poltawski/"
                   "tex/latex/poltawski/")
             (base32
              "1cf8vxah8j6nnaq2lhmiy1q3dnq6swprfmizzxd6y67sc60rznzm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/poltawski")
    (synopsis "Antykwa Poltawskiego Family of Fonts")
    (description
     "The package contains the Antykwa Poltawskiego family of fonts in the
PostScript Type 1 and OpenType formats Following the route set out by the
Latin Modern and TeX Gyre projects, the Antykwa Poltawskiego digitisation
project aims at providing a rich collection of diacritical characters in the
attempt to cover as many Latin-based scripts as possible.  To our knowledge,
the repertoire of characters covers all European languages as well as some
other Latin-based alphabets such as Vietnamese and Navajo; at the request of
users, recent extensions (following the enhancement of the Latin Modern
collection) provide glyphs sufficient for typesetting of romanized
transliterations of Arabic and Sanskrit scripts.  The Antykwa Poltawskiego
family consists of 4 weights (light, normal, medium, bold), each having
upright and italic forms and one of 5 design sizes: 6, 8, 10, 12 and 17pt.")
    (license license:gfl1.0)))

(define-public texlive-zapfchan
  (package
    (name "texlive-zapfchan")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/zapfchan/"
                   "fonts/afm/adobe/zapfchan/"
                   "fonts/afm/urw/zapfchan/"
                   "fonts/map/dvips/zapfchan/"
                   "fonts/tfm/adobe/zapfchan/"
                   "fonts/tfm/urw35vf/zapfchan/"
                   "fonts/type1/urw/zapfchan/"
                   "fonts/vf/adobe/zapfchan/"
                   "fonts/vf/urw35vf/zapfchan/"
                   "tex/latex/zapfchan/")
             (base32
              "1753lvv2bq29g43s4chc884n742695agvgal0b99gsrvlkyfp8gn")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Zapf Chancery font")
    (description
     "This package provides a drop-in replacement for the Zapf Chancery
font from Adobe's basic set.")
    (license license:gpl3+)))

(define-public texlive-zapfding
  (package
    (name "texlive-zapfding")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/zapfding/"
                   "fonts/afm/adobe/zapfding/"
                   "fonts/afm/urw/zapfding/"
                   "fonts/map/dvips/zapfding/"
                   "fonts/tfm/adobe/zapfding/"
                   "fonts/tfm/urw35vf/zapfding/"
                   "fonts/type1/urw/zapfding/"
                   "tex/latex/zapfding/")
             (base32
              "17mls8wilz9api9ivsbcczpiqp1f39qy8wa6ajssi8zhnc5lq7zn")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Zapfding font")
    (description
     "This package provides a drop-in replacement for the Zapfding font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-zbmath-review-template
  (package
    (name "texlive-zbmath-review-template")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/zbmath-review-template/"
                   "tex/xelatex/zbmath-review-template/")
             (base32
              "1m5q03vjscla4wmy9fr3kl23pk8zmqw760i89zg62pj4np8vyplj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/zbmath-review-template")
    (synopsis "Template for a zbMATH Open review")
    (description
     "This package contains a template for zbMATH Open reviews.  It will show
what your review will look like on zbMATH Open and you can test whether your
LaTeX-Code will compile on our system.")
    (license (list license:gpl3 license:cc-by-sa4.0))))

(define-public texlive-zhspacing
  (package
    (name "texlive-zhspacing")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/zhspacing/"
                   "tex/context/third/zhspacing/"
                   "tex/generic/zhspacing/"
                   "tex/xelatex/zhspacing/")
             (base32
              "02hwa7yjwb6wxkkib83mjdbara5zcsixbp5xlawri8n9ah54vxjm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/zhspacing")
    (synopsis "Spacing for mixed CJK-English documents in XeTeX")
    (description
     "The package manages spacing in a CJK document; between consecutive Chinese
letters, spaces are ignored, but a consistent space is inserted between Chinese
text and English (or mathematics).  The package may be used by any document
format under XeTeX.")
    (license license:lppl1.3+)))

(define-public texlive-zref
  (package
    (name "texlive-zref")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/zref/" "source/latex/zref/"
                   "tex/latex/zref/")
             (base32
              "188m3xb2q471mmm54akpdbj65n9sz70n0krapnrbwa8glxjrvlxk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://github.com/ho-tex/zref")
    (synopsis "Reference scheme for LaTeX")
    (description "This package offers a means to remove the limitation, of
only two properties, that is inherent in the way LaTeX's reference system
works.  The package implements an extensible referencing system, where
properties may be defined and used in the course of a document.  It provides
an interface for macro programmers to access the new reference scheme and some
modules that use it.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-fonts-adobe-zapfding texlive-zapfding)

(define-public texlive-rsfs
  (package
    (name "texlive-rsfs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/rsfs/"
                   "fonts/afm/public/rsfs/"
                   "fonts/map/dvips/rsfs/"
                   "fonts/source/public/rsfs/"
                   "fonts/tfm/public/rsfs/"
                   "fonts/type1/public/rsfs/"
                   "tex/plain/rsfs/")
             (base32
              "1sa32wnsj84wbwqji1fb4k9ik99dy5ji7zz4v0xbd7306agyhns5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/rsfs")
    (synopsis "Ralph Smith's Formal Script font")
    (description
     "The fonts provide uppercase formal script letters for use as symbols in
scientific and mathematical typesetting (in contrast to the informal script
fonts such as that used for the calligraphic symbols in the TeX maths symbol
font).  The fonts are provided as Metafont source, and as derived Adobe Type
1 format.  LaTeX support, for using these fonts in mathematics, is available
via one of the packages @code{calrsfs} and @code{mathrsfs}.")
    (license (license:fsf-free "http://mirrors.ctan.org/fonts/rsfs/README"))))

(define-deprecated-package texlive-fonts-rsfs texlive-rsfs)

(define-public texlive-eso-pic
  (package
    (name "texlive-eso-pic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/eso-pic/" "source/latex/eso-pic/"
                   "tex/latex/eso-pic/")
             (base32
              "05bqm4x209wji0q6xk1jrjp0nzqafp44dlq30hlpcagrggjb3d9s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/eso-pic")
    (synopsis "Add picture commands (or backgrounds) to every page")
    (description
     "The package adds one or more user commands to LaTeX's @code{shipout}
routine, which may be used to place the output at fixed positions.  The
@code{grid} option may be used to find the correct places.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-eso-pic texlive-eso-pic)

(define-public texlive-eepic
  (package
    (name "texlive-eepic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/eepic/"
                   "tex/latex/eepic/")
             (base32
              "16v8j3f8bgww9adddpfzpwd5q9kvak7xnp5kkvkrvhw8vshdspaa")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/eepic")
    (synopsis "Extensions to @code{epic} and the LaTeX drawing tools")
    (description
     "This package provides extensions to @code{epic} and the LaTeX picture
drawing environment.  It includes the drawing of lines at any slope, the
drawing of circles in any radii, and the drawing of dotted and dashed lines
much faster with much less TeX memory, and providing several new commands for
drawing ellipses, arcs, splines, and filled circles and ellipses.")
    (license license:public-domain)))

(define-deprecated-package texlive-latex-eepic texlive-eepic)

(define-public texlive-enotez
  (package
    (name "texlive-enotez")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/enotez/" "tex/latex/enotez/")
             (base32
              "0498zr9niylpj9q5ndnj52lb06cj0b424yyq587vqhckxq4l24ik")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/enotez")
    (synopsis "Support for end-notes")
    (description
     "This package allows nested endnotes, supports @code{hyperref} and
provides means for easy customization of the list of notes.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-enotez texlive-enotez)

(define-public texlive-endnotes
  (package
    (name "texlive-endnotes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/endnotes/" "tex/latex/endnotes/")
             (base32
              "1s7j5sg8fbhifng0gfqnghbvalbbh0p7j9v06r660w089364ypwz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/endnotes")
    (synopsis "Place footnotes at the end")
    (description
     "The @code{endnotes} package can be used to accumulate notes (using the
@code{\\endnote} command, which can be used as a replacement for
@code{\\footnote}), and place them at the end of the section, chapter or
document.")
    (license license:lppl1.2+)))

(define-public texlive-enumitem
  (package
    (name "texlive-enumitem")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/enumitem/" "tex/latex/enumitem/")
             (base32
              "0qwbyjb4a82qjxrfmz06v3w5vly75id4ix4sw7lz2az68kz080dv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/enumitem")
    (synopsis "Control layout of itemize, enumerate, description")
    (description
     "This package provides user control over the layout of the three basic
list environments: enumerate, itemize and description.  It supersedes both
@code{enumerate} and @code{mdwlist} (providing well-structured replacements
for all their functionality), and in addition provides functions to compute
the layout of labels, and to clone the standard environments, to create new
environments with counters of their own.")
    (license license:expat)))

(define-deprecated-package texlive-latex-enumitem texlive-enumitem)

(define-public texlive-multido
  (package
    (name "texlive-multido")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/multido/"
                   "source/generic/multido/"
                   "tex/generic/multido/" "tex/latex/multido/")
             (base32
              "1vwf2naw5bgs93s2gcmf226f60ws9z6cmw6gi1562fs8vg4mnjsh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/multido")
    (synopsis "Loop facility for Generic TeX")
    (description
     "The package provides the @code{\\multido} command, which was originally
designed for use with PSTricks.  Fixed-point arithmetic is used when working
on the loop variable, so that the package is equally applicable in graphics
applications like PSTricks as it is with the more common integer loops.")
    (license license:lppl)))

(define-public texlive-multirow
  (package
    (name "texlive-multirow")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/multirow/"
                   "source/latex/multirow/"
                   "tex/latex/multirow/")
             (base32
              "18xnxqbkkzblngws1ydmkiwfrf9gvrriqrjpb6g6kmaxciwypqd6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/multirow")
    (synopsis "Create tabular cells spanning multiple rows")
    (description
     "The package has a lot of flexibility, including an option for specifying
an entry at the natural width of its text.  The package is distributed with
the @code{bigdelim} and @code{bigstrut} packages, which can be used to
advantage with @code{\\multirow} cells.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-multirow texlive-multirow)

(define-public texlive-overpic
  (package
    (name "texlive-overpic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/overpic/"
                   "source/latex/overpic/"
                   "tex/latex/overpic/")
             (base32
              "0z6jkn54b4yfk2ia8cxcb5is3qyg64r0na05ixd8xbirrks9ir7w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/overpic")
    (synopsis "Combine LaTeX commands over included graphics")
    (description
     "The @code{overpic} environment is a cross between the LaTeX
@code{picture} environment and the @code{\\includegraphics} command of
@code{graphicx}.  The resulting picture environment has the same dimensions as
the included graphic.  LaTeX commands can be placed on the graphic at defined
positions; a grid for orientation is available.")
    (license license:lppl1.0+)))

(define-deprecated-package texlive-latex-overpic texlive-overpic)

(define-public texlive-parskip
  (package
    (name "texlive-parskip")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/parskip/"
                   "source/latex/parskip/"
                   "tex/latex/parskip/")
             (base32
              "18yygddxv3kblvf4jhzqa8h1js0n8g1bw723r6ss2hlz4lj64kf0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/parskip")
    (synopsis "Layout with zero @code{\\parindent}, non-zero @code{\\parskip}")
    (description
     "Simply changing @code{\\parskip} and @code{\\parindent} leaves a layout
that is untidy; this package (though it is no substitute for a properly
designed class) helps alleviate this untidiness.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-parskip texlive-parskip)

(define-public texlive-pbox
  (package
    (name "texlive-pbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pbox/"
                   "source/latex/pbox/"
                   "tex/latex/pbox/")
             (base32
              "104x4y22msgxhnlz2x331zq7rw28v129s5ym1jqhsk685izb3hcl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pbox")
    (synopsis "@code{\\parbox} with a variable width")
    (description
     "@code{pbox} defines a @code{\\pbox} command which adjusts the box width
to that of the enclosed text, up to the maximum width given.  The package also
defines some associated length commands.")
    (license license:gpl3+)))

(define-deprecated-package texlive-latex-pbox texlive-pbox)

(define-public texlive-pdfpages
  (package
    (name "texlive-pdfpages")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pdfpages/" "source/latex/pdfpages/"
                   "tex/latex/pdfpages/")
             (base32
              "0ihihrrim9fwmgkmrqxmss4wjcv8mv1gr2cpigihlzl6q6iqggjx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-eso-pic texlive-graphics texlive-oberdiek texlive-tools))
    (home-page "https://ctan.org/pkg/pdfpages")
    (synopsis "Include PDF documents in LaTeX")
    (description
     "This package simplifies the inclusion of external multi-page PDF
documents in LaTeX documents.  Pages may be freely selected and it is possible
to put several logical pages onto each sheet of paper.  Furthermore a lot of
hypertext features like hyperlinks and article threads are provided.  The
package supports pdfTeX (pdfLaTeX) and VTeX.  With VTeX it is even possible to
use this package to insert PostScript files, in addition to PDF files.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-pdfpages texlive-pdfpages)

(define-public texlive-stix2-otf
  (package
    (name "texlive-stix2-otf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/stix2-otf/"
                   "fonts/opentype/public/stix2-otf/")
             (base32
              "05xqlg61rkfky34x7mc92203z440kanr7bwpmw1djq2y36ql3p1l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/stix2-otf")
    (synopsis "OpenType Unicode text and maths fonts")
    (description
     "The Scientific and Technical Information eXchange (STIX) fonts are
intended to satisfy the demanding needs of authors, publishers, printers, and
others working in the scientific, medical, and technical fields.  They combine
a comprehensive Unicode-based collection of mathematical symbols and alphabets
with a set of text faces suitable for professional publishing.")
    (license license:silofl1.1)))

(define-public texlive-sidecap
  (package
    (name "texlive-sidecap")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sidecap/" "source/latex/sidecap/"
                   "tex/latex/sidecap/")
             (base32
              "1h4ysw90dpvnj0x1j9krx40078kyzzs4ynpjz7y50v9hwrrrynjk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-ragged2e))
    (home-page "https://ctan.org/pkg/sidecap")
    (synopsis "Typeset captions sideways")
    (description
     "The @code{sidecap} package defines environments called @code{SCfigure}
and @code{SCtable} (analogous to @code{figure} and @code{table}) to typeset
captions sideways.  Options include @code{outercaption}, @code{innercaption},
@code{leftcaption} and @code{rightcaption}.")
    (license license:lppl1.0+)))

(define-deprecated-package texlive-latex-sidecap texlive-sidecap)

(define-public texlive-stmaryrd
  (package
    (name "texlive-stmaryrd")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/stmaryrd/"
                   "fonts/afm/public/stmaryrd/"
                   "fonts/map/dvips/stmaryrd/"
                   "fonts/source/public/stmaryrd/"
                   "fonts/tfm/public/stmaryrd/"
                   "fonts/type1/public/stmaryrd/"
                   "source/fonts/stmaryrd/"
                   "tex/latex/stmaryrd/")
             (base32
              "0ljrxbf2p301p4cmadf2w0qb5idvgmx4j6y3kq7qg2v8x4maqqj4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-ins
            (lambda _
              (substitute* "source/fonts/stmaryrd/stmaryrd.ins"
                (("^%% LaTeX2e.*") "\\input docstrip\n")
                (("fontdef\\}\\}" line)
                 (string-append line "\n\\endbatchfile"))))))))
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/stmaryrd")
    (synopsis "St Mary Road symbols for theoretical computer science")
    (description
     "The fonts were originally distributed as Metafont sources only, but
Adobe Type 1 versions are also now available.  Macro support is provided for
use under LaTeX; the package supports the @code{only} option (provided by the
@code{somedefs} package) to restrict what is loaded, for those who don't need
the whole font.")
    (license license:lppl)))

(define-deprecated-package texlive-fonts-stmaryrd texlive-stmaryrd)

(define-public texlive-subfig
  (package
    (name "texlive-subfig")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/subfig/" "source/latex/subfig/"
                   "tex/latex/subfig/")
             (base32
              "0bq1328pb1ak91j7q8n1kh2fncr742lvff7apgf8kkxzxjfg2z9r")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/subfig")
    (synopsis "Figures broken into subfigures")
    (description
     "The package provides support for the manipulation and reference of small
or sub figures and tables within a single figure or table environment.  It is
convenient to use this package when your subfigures are to be separately
captioned, referenced, or are to be included in the List-of-Figures.  A new
@code{\\subfigure} command is introduced which can be used inside a figure
environment for each subfigure.  An optional first argument is used as the
caption for that subfigure.")
    (license license:lppl)))

(define-public texlive-subfigure
  (package
    (name "texlive-subfigure")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/subfigure/"
                   "source/latex/subfigure/"
                   "tex/latex/subfigure/")
             (base32
              "1327ygajf6gza5msvhfjjnk6r3sw7vb7rxg23v4gx4dmyxqfqrbi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/subfigure")
    (synopsis "Deprecated: Figures divided into subfigures")
    (description
       "This (deprecated) package provides support for the manipulation and
reference of small, or sub, figures and tables within a single figure or table
environment.  It is convenient to use this package when your subfigures are to
be separately captioned, referenced, or are to be included in the
List-of-Figures.  A new @code{\\subfigure} command is introduced which can be
used inside a figure environment for each subfigure.  An optional first
argument is used as the caption for that subfigure.  The package is now
considered obsolete: it was superseded by @code{subfig}, but users may find
the more recent @code{subcaption} package more satisfactory.")
      (license license:lppl)))

(define-deprecated-package texlive-latex-subfigure texlive-subfigure)

(define-public texlive-tabulary
  (package
    (name "texlive-tabulary")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/tabulary/" "source/latex/tabulary/"
                   "tex/latex/tabulary/")
             (base32
              "00afi9r5264rhfy5kg73fk763i7wm6bvzkmrlg7n17fwl6hx0sa1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tabulary")
    (synopsis "Tabular with variable width columns balanced")
    (description
     "The package defines a @code{tabular*}-like environment, @code{tabulary},
taking a \"total width\" argument as well as the column specifications.  The
environment uses column types @code{L}, @code{C}, @code{R} and @code{J} for
variable width columns (@code{\\raggedright}, @code{\\centering},
@code{\\raggedleft}, and normally justified).  In contrast to
@code{tabularx}'s @code{X} columns, the width of each column is weighted
according to the natural width of the widest cell in the column.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-tabulary texlive-tabulary)

(define-public texlive-threeparttable
  (package
    (name "texlive-threeparttable")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/threeparttable/"
                   "tex/latex/threeparttable/")
             (base32
              "05i50k1y736m52903nz4kf2xl23w6y7rrzyacs4kgd1w6kmjm6f7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/threeparttable")
    (synopsis "Tables with captions and notes all the same width")
    (description
     "This package facilitates tables with titles (captions) and notes.  The
title and notes are given a width equal to the body of the table (a
@code{tabular} environment).  By itself, a @code{threeparttable} does not
float, but you can put it in a @code{table} or a @code{table*} or some other
environment.")
    (license (license:fsf-free "file://threeparttable.sty"))))

(define-public texlive-thumbpdf
  (package
    (name "texlive-thumbpdf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/thumbpdf/"
                   "doc/man/man1/thumbpdf.1"
                   "doc/man/man1/thumbpdf.man1.pdf"
                   "scripts/thumbpdf/" "tex/generic/thumbpdf/")
             (base32
              "0ya18440rpkav0z1zddzii9jh2swybicj87413l5iin2acrssw42")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "thumbpdf.pl")))
    (inputs (list perl))
    (home-page "https://ctan.org/pkg/thumbpdf")
    (synopsis "Thumbnails for pdfTeX and dvips/ps2pdf")
    (description
     "This package provides a Perl script that provides support for thumbnails
in pdfTeX and dvips/ps2pdf.  The script uses Ghostscript to generate the
thumbnails which get represented in a TeX readable file that is read by the
package @code{thumbpdf.sty} to automatically include the thumbnails.  This
arrangement works with both plain TeX and LaTeX.")
    (license license:lppl1.3+)))

(define-public texlive-txfonts
  (package
    (name "texlive-txfonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/txfonts/"
                   "fonts/afm/public/txfonts/"
                   "fonts/enc/dvips/txfonts/"
                   "fonts/map/dvips/txfonts/"
                   "fonts/tfm/public/txfonts/"
                   "fonts/type1/public/txfonts/"
                   "fonts/vf/public/txfonts/"
                   "tex/latex/txfonts/")
             (base32
              "017zjas5y1zlyq0iy4x6mv1qbz23xcy3y5xs0crj6zdnfvnccqgp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/txfonts")
    (synopsis "Times-like fonts in support of mathematics")
    (description
     "Txfonts supplies virtual text roman fonts using Adobe Times (or URW
NimbusRomNo9L) with some modified and additional text symbols in the OT1, T1,
and TS1 encodings; maths alphabets using Times/URW Nimbus; maths fonts
providing all the symbols of the Computer Modern and AMS fonts, including all
the Greek capital letters from CMR; and additional maths fonts of various
other symbols.

The set is complemented by a sans-serif set of text fonts, based on
Helvetica/NimbusSanL, and a monospace set.

All the fonts are in Type 1 format (AFM and PFB files), and are supported by
TeX metrics (VF and TFM files) and macros for use with LaTeX.")
    ;; Any version of the GPL with font exception.
    (license license:gpl3+)))

(define-deprecated-package texlive-fonts-txfonts texlive-txfonts)

(define-public texlive-typehtml
  (package
    (name "texlive-typehtml")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/typehtml/"
                   "source/latex/typehtml/"
                   "tex/latex/typehtml/")
             (base32
              "1nmdh2mhkzdqs5y4k95g9il6vz4rgndzhkikiilknkwg1a04rrzi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/typehtml")
    (synopsis "Typeset HTML directly from LaTeX")
    (description
     "This package typesets HTML directly from LaTeX.  It can handle almost
all of HTML2, and most of the math fragment of the draft HTML3.")
    (license license:lppl)))

(define-public texlive-iwona
  (package
    (name "texlive-iwona")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/iwona/"
                   "fonts/afm/nowacki/iwona/"
                   "fonts/enc/dvips/iwona/"
                   "fonts/map/dvips/iwona/"
                   "fonts/opentype/nowacki/iwona/"
                   "fonts/tfm/nowacki/iwona/"
                   "fonts/type1/nowacki/iwona/"
                   "tex/latex/iwona/"
                   "tex/plain/iwona/")
             (base32
              "1gk80zj711rcnk06cvszic7lpm06nj47kbypg13rpijdzfsvmi8m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/iwona")
    (synopsis "Sans-serif typeface for TeX")
    (description "Iwona is a two-element sans-serif typeface.  It was created
as an alternative version of the Kurier typeface, which was designed in 1975
for a diploma in typeface design at the Warsaw Academy of Fine Arts under the
supervision of Roman Tomaszewski.  Kurier was designed for linotype
typesetting of newspapers and similar periodicals.  The Iwona fonts are an
alternative version of the Kurier fonts.  The difference lies in the absence
of ink traps which typify the Kurier font.")
    (license license:gfl1.0)))

(define-deprecated-package texlive-fonts-iwona texlive-iwona)

(define-public texlive-jadetex
  (package
    (name "texlive-jadetex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/jadetex.1"
                   "doc/man/man1/jadetex.man1.pdf"
                   "doc/man/man1/pdfjadetex.1"
                   "doc/man/man1/pdfjadetex.man1.pdf"
                   "doc/otherformats/jadetex/base/"
                   "source/jadetex/base/"
                   "tex/jadetex/base/")
             (base32
              "0acan496ixymwjvygcd5rx5pmz4p5vffzkmazdryw1kpilhiixcx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:create-formats #~(list "jadetex" "pdfjadetex")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-wrappers
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((pdftex (search-input-file inputs "/bin/pdftex"))
                    (web2c (string-append #$output "/share/texmf-dist/web2c")))
                (mkdir-p (string-append #$output "/bin"))
                (symlink pdftex
                         (string-append #$output "/bin/jadetex"))
                (symlink pdftex
                         (string-append #$output "/bin/pdfjadetex"))))))))
    (propagated-inputs
     (list texlive-amsfonts
           texlive-atbegshi
           texlive-atveryend
           texlive-auxhook
           texlive-babel
           texlive-bigintcalc
           texlive-bitset
           texlive-cm
           texlive-colortbl
           texlive-cyrillic
           texlive-ec
           texlive-etexcmds
           texlive-everyshi
           texlive-fancyhdr
           texlive-firstaid
           texlive-graphics
           texlive-graphics-cfg
           texlive-graphics-def
           texlive-hycolor
           texlive-hyperref
           texlive-hyphen-complete
           texlive-iftex
           texlive-infwarerr
           texlive-intcalc
           texlive-kvdefinekeys
           texlive-kvoptions
           texlive-kvsetkeys
           texlive-l3backend
           texlive-l3kernel
           texlive-l3packages
           texlive-latex
           texlive-latex-fonts
           texlive-latexconfig
           texlive-letltxmacro
           texlive-ltxcmds
           texlive-marvosym
           texlive-passivetex
           texlive-pdfescape
           texlive-pdftex
           texlive-pdftexcmds
           texlive-psnfss
           texlive-rerunfilecheck
           texlive-stmaryrd
           texlive-symbol
           texlive-tex
           texlive-tex-ini-files
           texlive-tipa
           texlive-tools
           texlive-ulem
           texlive-unicode-data
           texlive-uniquecounter
           texlive-url
           texlive-wasysym
           texlive-zapfding))
    (home-page "https://www.ctan.org/pkg/jadetex/")
    (synopsis "TeX macros to produce TeX output using OpenJade")
    (description "JadeTeX is a companion package to the OpenJade DSSSL
processor.  OpenJade applies a DSSSL stylesheet to an SGML or XML document.
The output of this process can be in a number of forms, including a set of
high level LaTeX macros.  It is the task of the JadeTeX package to transform
these macros into DVI/PostScript (using the @command{jadetex} command) or
Portable Document Format (PDF) form (using the @command{pdfjadetex}
command).")
    ;; The license text is found at the header of the jadetex.dtx file.
    (license license:expat)))

(define-public texlive-libertine
  (package
    (name "texlive-libertine")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/libertine/"
                   "fonts/enc/dvips/libertine/"
                   "fonts/map/dvips/libertine/"
                   "fonts/opentype/public/libertine/"
                   "fonts/tfm/public/libertine/"
                   "fonts/type1/public/libertine/"
                   "fonts/vf/public/libertine/"
                   "tex/latex/libertine/")
             (base32
              "04bb0v1fp9adcgx6s4zc0fs5z4f85ihbhbkk9zf5pf0ni3gy70fd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-fontaxes
           texlive-iftex
           texlive-mweights
           texlive-xkeyval))
    (home-page "https://ctan.org/pkg/libertine")
    (synopsis "Use of Linux Libertine and Biolinum fonts with LaTeX")
    (description
     "The package provides the Libertine and Biolinum fonts in both Type 1 and
OTF styles, together with support macros for their use.  Monospaced and
display fonts, and the keyboard set are also included, in OTF style, only.
The @code{mweights} package is used to manage the selection of font weights.")
    (license (list license:gpl2+        ; with font exception
                   license:silofl1.1
                   license:lppl))))

(define-public texlive-dejavu
  (package
    (name "texlive-dejavu")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/dejavu/"
                   "fonts/afm/public/dejavu/"
                   "fonts/enc/dvips/dejavu/"
                   "fonts/map/dvips/dejavu/"
                   "fonts/tfm/public/dejavu/"
                   "fonts/truetype/public/dejavu/"
                   "fonts/type1/public/dejavu/"
                   "fonts/vf/public/dejavu/"
                   "tex/latex/dejavu/")
             (base32
              "0y4qf5jl0xncah9nkcaalmy69wwq02n3j895zp71n2p0nfi24aka")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/dejavu")
    (synopsis "LaTeX support for the DejaVu fonts")
    (description
     "The package contains LaTeX support for the DejaVu fonts, which are
derived from the Vera fonts but contain more characters and styles.  The fonts
are included in the original TrueType format, and in converted Type 1 format.
The (currently) supported encodings are: OT1, T1, IL2, TS1, T2*, X2, QX, and
LGR.  The package doesn't (currently) support mathematics.")
    (license license:lppl)))

(define-public texlive-titlesec
  (package
    (name "texlive-titlesec")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/titlesec/" "tex/latex/titlesec/")
             (base32
              "0331f6f6sv6sfn4dx7qhx2fgnj9lf3hgbqkh603paqpknfmfjyfm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/titlesec")
    (synopsis "Select alternative section titles")
    (description
     "This package provides an interface to sectioning commands for selection
from various title styles, e.g. for marginal titles and to change the font of
all headings with a single command, also providing simple one-step page
styles.  It also includes a package to change the page styles when there are
floats in a page.  You may assign headers/footers to individual floats, too.")
    (license license:expat)))

(define-deprecated-package texlive-latex-titlesec texlive-titlesec)

(define-public texlive-type1cm
  (package
    (name "texlive-type1cm")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/type1cm/" "source/latex/type1cm/"
                   "tex/latex/type1cm/")
             (base32
              "1922af5xvhrh4l8rqwz3bjd1gqvzfkxrfim28rpnvbx4n7jl6sdh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/type1cm")
    (synopsis "Arbitrary size font selection in LaTeX")
    (description
     "LaTeX, by default, restricts the sizes at which you can use its default
computer modern fonts, to a fixed set of discrete sizes (effectively, a set
specified by Knuth).  The @code{type1cm} package removes this restriction;
this is particularly useful when using scalable versions of the @code{cm}
fonts (Bakoma, or the versions from BSR/Y&Y, or True Type versions from Kinch,
PCTeX, etc.).  Note that the LaTeX distribution now contains a package
@code{fix-cm}, which performs the task of @code{type1cm}, as well as doing the
same job for T1- and TS1-encoded @code{ec} fonts.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-type1cm texlive-type1cm)

(define-public texlive-lh
  (package
    (name "texlive-lh")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/lh/"
                   "fonts/source/lh/base/"
                   "fonts/source/lh/lh-XSlav/"
                   "fonts/source/lh/lh-conc/"
                   "fonts/source/lh/lh-lcy/"
                   "fonts/source/lh/lh-ot2/"
                   "fonts/source/lh/lh-t2a/"
                   "fonts/source/lh/lh-t2b/"
                   "fonts/source/lh/lh-t2c/"
                   "fonts/source/lh/lh-t2d/"
                   "fonts/source/lh/lh-x2/"
                   "fonts/source/lh/nont2/"
                   "fonts/source/lh/specific/"
                   "source/fonts/lh/"
                   "source/latex/lh/"
                   "tex/latex/lh/"
                   "tex/plain/lh/")
             (base32
              "0cqwns4zy1847fn3dp8z3wbfpy4dl05cr065nk9k65fmp7wksnjk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-ec texlive-metafont))
    (propagated-inputs (list texlive-ec))
    (home-page "https://ctan.org/pkg/lh")
    (synopsis "Cyrillic fonts that support LaTeX standard encodings")
    (description
     "The LH fonts address the problem of the wide variety of alphabets that
are written with Cyrillic-style characters.  The fonts are the original basis
of the set of T2* and X2 encodings that are now used when LaTeX users need to
write in Cyrillic languages.  Macro support in standard LaTeX encodings is
offered through the latex-cyrillic and t2 bundles, and the package itself
offers support for other (more traditional) encodings.  The fonts, in the
standard T2* and X2 encodings are available in Adobe Type 1 format, in the
CM-Super family of fonts.  The package also offers its own LaTeX support for
OT2 encoded fonts, CM bright shaped fonts and Concrete shaped fonts.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-lh texlive-lh)

(define-public texlive-marvosym
  (package
    (name "texlive-marvosym")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/marvosym/"
                   "fonts/afm/public/marvosym/"
                   "fonts/map/dvips/marvosym/"
                   "fonts/tfm/public/marvosym/"
                   "fonts/truetype/public/marvosym/"
                   "fonts/type1/public/marvosym/"
                   "source/fonts/marvosym/"
                   "tex/latex/marvosym/")
             (base32
              "16s5ibpw6c9d3vzc82hfn90dg643xlracivikdbr9s43f2ayak41")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/marvosym")
    (synopsis "Martin Vogel's Symbols (marvosym) font")
    (description
     "Martin Vogel's Symbol font (marvosym) contains the Euro currency symbol
as defined by the European commission, along with symbols for structural
engineering; symbols for steel cross-sections; astronomy signs (sun, moon,
planets); the 12 signs of the zodiac; scissor symbols; CE sign and others.
The package contains both the original TrueType font and the derived Type
1 font, together with support files for TeX (LaTeX).")
    (license (list license:lppl          ;for TeX support files
                   license:silofl1.1)))) ;for fonts

(define-public texlive-metapost
  (package
    (name "texlive-metapost")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/dvitomp.1"
                   "doc/man/man1/dvitomp.man1.pdf"
                   "doc/man/man1/mpost.1"
                   "doc/man/man1/mpost.man1.pdf"
                   "doc/metapost/base/"
                   "fonts/afm/metapost/"
                   "fonts/enc/dvips/metapost/"
                   "fonts/map/dvips/metapost/"
                   "fonts/tfm/metapost/"
                   "fonts/type1/metapost/"
                   "metapost/base/"
                   "metapost/config/"
                   "metapost/misc/"
                   "metapost/support/charlib/"
                   "tex/generic/metapost/")
             (base32
              "0i6mjq59n7vll81m7r2k83x0q6xx7cg6qcia46298zqc0b0l3qb0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-kpathsea))
    (home-page "https://ctan.org/pkg/metapost")
    (synopsis "Create scalable illustrations")
    (description
     "MetaPost uses a language based on that of Metafont to produce precise
technical illustrations.  Its output is scalable PostScript or SVG, rather
than the bitmaps Metafont creates.")
    (license license:lppl)))

(define-public texlive-acmart
  (package
    (name "texlive-acmart")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/acmart/" "doc/latex/acmart/"
                   "source/latex/acmart/" "tex/latex/acmart/")
             (base32
              "0g27q3r7w83347az77d64xbcxzr9rl64a2qq8l0xs6drfpsq0llb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/acmart")
    (synopsis "Class for typesetting publications of ACM")
    (description
     "This package provides a class for typesetting publications of the
Association for Computing Machinery (ACM).")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-acmart texlive-acmart)

(define-public texlive-varwidth
  (package
    (name "texlive-varwidth")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/varwidth/" "tex/latex/varwidth/")
             (base32
              "0jcrv4klcjpl17ml0zyqfvkrq6qwn2imxv8syqs5m6qk0fk7hg6l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/varwidth")
    (synopsis "Variable-width @code{minipage}")
    (description
     "The @code{varwidth} environment is superficially similar to @code{minipage},
but the specified width is just a maximum value -- the box may get a narrower
natural width.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-varwidth texlive-varwidth)

(define-public texlive-wasy
  (package
    (name "texlive-wasy")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/wasy/" "fonts/source/public/wasy/"
                   "fonts/tfm/public/wasy/" "tex/plain/wasy/")
             (base32
              "1swzxgld3lndi5q0q6zkwbw06ndh13fvp04as7zpwyhh646s0hbx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/wasy")
    (synopsis "Waldi symbol fonts")
    (description "This package provides the @code{wasy} (Waldi symbol) fonts,
in the Metafont and Adobe Type 1 formats.  Support under LaTeX is provided by
the @code{wasysym} package.")
    (license license:public-domain)))

(define-public texlive-wasy-type1
  (package
    (name "texlive-wasy-type1")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/wasy-type1/"
                   "fonts/afm/public/wasy-type1/"
                   "fonts/map/dvips/wasy-type1/"
                   "fonts/type1/public/wasy-type1/")
             (base32
              "01xwryijs787ab4ayfsj44ylf72s0zkm49zm0yk4ghs4vck2qvq3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-wasy))
    (home-page "https://ctan.org/pkg/wasy-type1")
    (synopsis "Type 1 versions of @code{wasy} fonts")
    (description
     "This package provides converted (Adobe Type 1) outlines of the
@code{wasy} fonts.")
    (license license:public-domain)))

(define-public texlive-wasysym
  (package
    (name "texlive-wasysym")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/wasysym/" "source/latex/wasysym/"
                   "tex/latex/wasysym/")
             (base32
              "1n0rrrh510hy04a4fkxqh7skwfhp3xiiji78cw3mc65g06h1jyjc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/wasysym")
    (synopsis "LaTeX support for the @code{wasy} fonts")
    (description
     "The @code{wasy} (Waldi Symbol) font by Roland Waldi provides many glyphs
like male and female symbols and astronomical symbols, as well as the complete
@code{lasy} font set and other odds and ends.  The @code{wasysym} package
implements an easy to use interface for these symbols.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-wasysym texlive-wasysym)

(define-public texlive-willowtreebook
  (package
    (name "texlive-willowtreebook")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/willowtreebook/"
                   "tex/latex/willowtreebook/")
             (base32
              "1rv6pmyl02fpysviz4mvz9az9bgk96p6s7mbi9ykxxp74xzh8jik")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/willowtreebook")
    (synopsis "Easy basic book class, built on @code{memoir}")
    (description
     "The @code{willowtreebook} class is a simple book class, which the author
uses for his lecture notes to be found on his web page Benjamin McKay.  It
actually just selects options for the more sophisticated @code{memoir}
class.")
    (license license:lppl1.3c)))

(define-public texlive-wrapfig
  (package
    (name "texlive-wrapfig")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/wrapfig/" "tex/latex/wrapfig/")
             (base32
              "0wk1vp0dqsp597xzsqbwj8xk80v7d77qmpjir84n54f920rf9ka9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/wrapfig")
    (synopsis "Produces figures which text can flow around")
    (description
     "This package allows figures or tables to have text wrapped around them.
It does not work in combination with list environments, but can be used in a
@code{parbox} or @code{minipage}, and in two-column format.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-wrapfig texlive-wrapfig)

(define-public texlive-ucharcat
  (package
    (name "texlive-ucharcat")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ucharcat/"
                   "source/latex/ucharcat/"
                   "tex/latex/ucharcat/")
             (base32
              "0r6mphnn26053vb4bgw1zjhw7y7xjavii9hnn7607cbscnx5f9di")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ucharcat")
    (synopsis
     "Implementation of the XeTeX @code{\\Ucharcat} command for LuaTeX")
    (description
     "The package implements the @code{\\Ucharcat} command for LuaLaTeX.
@code{\\Ucharcat} is a new primitive in XeTeX, an extension of the existing
@code{\\Uchar} command, that allows the specification of the catcode as well
as character code of the character token being constructed.")
    (license license:lppl)))

(define-public texlive-ucs
  (package
    (name "texlive-ucs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ucs/" "fonts/enc/dvips/ucs/"
                   "source/latex/ucs/" "tex/latex/ucs/")
             (base32
              "1viisfamsf0x984ak53dwznhw0yhb394xv66rbfaml6igizvkj83")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ucs")
    (synopsis "Extended UTF-8 input encoding support for LaTeX")
    (description
     "The bundle provides the @code{ucs} package, and @file{utf8x.def},
together with a large number of support files.  The @file{utf8x.def}
definition file for use with @code{inputenc} covers a wider range of Unicode
characters than does @file{utf8.def} in the LaTeX distribution.  The package
provides facilities for efficient use of its large sets of Unicode characters.
Glyph production may be controlled by various options, which permits use of
non-ASCII characters when coding mathematical formulae.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-ucs texlive-ucs)

(define-public texlive-preview
  (package
    (name "texlive-preview")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/preview/" "source/latex/preview/"
                   "tex/latex/preview/")
             (base32
              "05ixx2il8xajakh1nkpf7qjczc1kvniiimv585b3gkg6fwx92wzb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/preview")
    (synopsis "Extract bits of a LaTeX source for output")
    (description
     "The main purpose of the preview package is the extraction of selected
elements from a LaTeX source, like formulas or graphics, into separate
pages of a DVI file.  A flexible and convenient interface allows it to
specify what commands and constructs should be extracted.  This works
with DVI files postprocessed by either Dvips and Ghostscript or
dvipng, but it also works when you are using PDFTeX for generating PDF
files.")
    (license license:gpl3+)))

(define-deprecated-package texlive-latex-preview texlive-preview)

(define-public texlive-acronym
  (package
    (name "texlive-acronym")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/acronym/" "source/latex/acronym/"
                   "tex/latex/acronym/")
             (base32
              "0p2sws3qy7wv0v6bsy6c5j36n9s1ps7b1z7dmg1370schrjpqnfh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/acronym")
    (synopsis "Expand acronyms at least once")
    (description
     "This package ensures that all acronyms used in the text are spelled out
in full at least once.  It also provides an environment to build a list of
acronyms used.  The package is compatible with PDF bookmarks.  The package
requires the @code{suffix} package, which in turn requires that it runs under
e-TeX.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-acronym texlive-acronym)

(define-public texlive-pdftex
  (package
    (name "texlive-pdftex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/pdfetex.1"
                   "doc/man/man1/pdfetex.man1.pdf"
                   "doc/man/man1/pdftex.1"
                   "doc/man/man1/pdftex.man1.pdf"
                   "doc/pdftex/"
                   "fonts/map/dvips/dummy-space/"
                   "fonts/tfm/public/pdftex/"
                   "fonts/type1/public/pdftex/"
                   "scripts/simpdftex/"
                   "tex/generic/config/pdftex-dvi.tex"
                   "tex/generic/pdftex/")
             (base32
              "1fp8w8pkxqcv6n8y0zy2rdclm2hcyx4zv93h0fmqai1yvgcx6yh6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-bin? #f
           #:link-scripts #~(list "simpdftex")
           #:create-formats #~(list "etex" "pdfetex" "pdftex")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'patch-shell-scripts
                 (lambda _
                   (substitute* "scripts/simpdftex/simpdftex"
                     (("/bin/(cp|date|echo|mv|rm)" _ command)
                      (which command))
                     (("basename|dirname|mkdir|sed" command)
                      (which command))
                     (("(^[ \t]*)(cat|rm)" _ indent command)
                      (string-append indent (which command)))
                     (("(distillerpath=\").*" _ prefix)
                      (string-append prefix
                                     #$(this-package-input "ghostscript")
                                     "/bin\"\n"))))))))
    (inputs (list ghostscript))
    (propagated-inputs
     (list texlive-cm
           texlive-etex
           texlive-hyphen-complete
           texlive-knuth-lib
           texlive-kpathsea
           texlive-plain
           texlive-tex-ini-files))
    (home-page "https://ctan.org/pkg/pdftex")
    (synopsis "TeX extension for direct creation of PDF")
    (description
     "This package is an extension of TeX which can directly generate PDF
documents as well as DVI output.")
    (license license:gpl3+)))

(define-deprecated-package texlive-generic-pdftex texlive-pdftex)

(define texlive-bin-full
  (package/inherit texlive-bin
    (name "texlive-bin-full")
    (arguments
     (substitute-keyword-arguments (package-arguments texlive-bin)
       ((#:configure-flags _)
        #~(let ((kpathsea #$(this-package-input "texlive-libkpathsea")))
            (list "--with-banner-add=/GNU Guix"
                  "--enable-shared"
                  "--disable-native-texlive-build"
                  "--disable-static"
                  "--disable-kpathsea"
                  "--with-system-cairo"
                  "--with-system-freetype2"
                  "--with-system-gd"
                  "--with-system-gmp"
                  "--with-system-graphite2"
                  "--with-system-harfbuzz"
                  "--with-system-icu"
                  "--with-system-libgs"
                  "--with-system-libpaper"
                  "--with-system-libpng"
                  "--with-system-mpfr"
                  "--with-system-pixman"
                  "--with-system-potrace"
                  "--with-system-teckit"
                  "--with-system-zlib"
                  "--with-system-zziplib"
                  ;; Help locating external kpathsea.  For some reason
                  ;; PKG-CONFIG is unable to find it.
                  "--with-system-kpathsea"
                  (format #f "--with-kpathsea-includes=~a/include" kpathsea)
                  (format #f "--with-kpathsea-lib=~a/lib" kpathsea)
                  ;; LuaJIT is not ported to some architectures yet.
                  #$@(if (or (target-ppc64le?)
                             (target-riscv64?))
                         '("--disable-luajittex"
                           "--disable-luajithbtex"
                           "--disable-mfluajit")
                         '()))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'install 'symlink-kpathsea-binaries
              (lambda _
                (let ((bin (string-append
                            #$(this-package-input "texlive-libkpathsea")
                            "/bin"))
                      (files
                       '("kpseaccess" "kpsereadlink" "kpsestat" "kpsewhich")))
                  (with-directory-excursion (string-append #$output "/bin")
                    (for-each (lambda (b) (symlink (string-append bin "/" b) b))
                              files)))))
            (add-after 'install 'merge-core-scripts
              (lambda* (#:key inputs native-inputs #:allow-other-keys)
                (let ((texlive-scripts
                       (dirname
                        (dirname
                         (search-input-file (or native-inputs inputs)
                                            "tlpkg/texlive.tlpdb"))))
                      (tlpkg (string-append #$output "/share/tlpkg")))
                  ;; "tlpkg" directory is neither provided by texlive-bin nor
                  ;; by texlive-texmf.
                  (mkdir-p tlpkg)
                  (copy-recursively (string-append texlive-scripts "/tlpkg")
                                    tlpkg)
                  ;; texlive-bin source doesn't provide this Perl script.
                  ;; Yet, it is referenced in "fmtutil.pl" so we need to move
                  ;; it here too.
                  (install-file
                   (string-append texlive-scripts
                                  "/texmf-dist/scripts/texlive/mktexlsr.pl")
                   (string-append #$output
                                  "/share/texmf-dist/scripts/texlive")))))
            (add-after 'merge-core-scripts 'patch-core-scripts
              (lambda _
                (with-directory-excursion
                    (string-append #$output "/share/texmf-dist/scripts/texlive")
                  ;; Make sure that fmtutil can find its Perl modules.
                  (substitute* "fmtutil.pl"
                    (("\\$TEXMFROOT/")
                     (string-append #$output "/share/")))
                  ;; Likewise for updmap.pl.
                  (substitute* "updmap.pl"
                    (("\\$TEXMFROOT/tlpkg")
                     (string-append #$output "/share/tlpkg")))
                  ;; Likewise for the tlmgr.
                  (substitute* "tlmgr.pl"
                    ((".*\\$::installerdir = \\$Master.*" all)
                     (format #f "  $Master = ~s;~%~a"
                             (string-append #$output "/share")
                             all))))))
            (add-after 'patch-core-scripts 'patch-shell-scripts
              (lambda _
                (with-directory-excursion
                    (string-append #$output "/share/texmf-dist/scripts")
                  ;; First patch shell scripts with ".sh" extension.
                  (let* ((scripts (find-files "." "\\.sh$"))
                         (commands '("awk" "basename" "cat" "grep" "mkdir" "rm"
                                     "sed" "sort" "uname"))
                         (command-regexp
                          (format #f "\\b(~a)\\b" (string-join commands "|")))
                         (iso-8859-1-encoded-scripts
                          '("./texlive-extra/rubibtex.sh"
                            "./texlive-extra/rumakeindex.sh")))
                    (define (substitute-commands scripts)
                      (substitute* scripts
                        ((command-regexp dummy command)
                         (which command))))
                    (substitute-commands
                     (lset-difference string= scripts iso-8859-1-encoded-scripts))
                    (with-fluids ((%default-port-encoding "ISO-8859-1"))
                      (substitute-commands iso-8859-1-encoded-scripts)))
                  ;; Then patch scripts without such extension.
                  (let ((dirs (map (compose dirname which)
                                   (list "awk" "cat" "grep" "sed"))))
                    (substitute* (find-files "texlive" "^mktex(mf|pk|tfm)$")
                      (("^version=" m)
                       (format #false "PATH=\"~{~a:~}$PATH\"; export PATH~%~a"
                               dirs m)))))))))))
    (native-inputs
     (modify-inputs (package-native-inputs texlive-bin)
       (append (package-source texlive-scripts))))
    (inputs
     (modify-inputs (package-inputs texlive-bin)
       (append texlive-libkpathsea)))
    (propagated-inputs '())))

(define texlive-texmf
  (package
    (name "texlive-texmf")
    (version (package-version texlive-bin))
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://tug.org/historic/systems/texlive/"
                                  (string-take version 4)
                                  "/texlive-" version "-texmf.tar.xz"))
              (sha256
               (base32
                "0lqjm11pr9vasvivaci3k9xcmdyd08ldnh31zf8avjjs09xcfkac"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26))
      ;; This package takes 4 GiB, which we can't afford to distribute from
      ;; our servers.
      #:substitutable? #f
      #:install-plan #~'(("texmf-dist/" "share/texmf-dist"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'texmf-config
            (lambda* (#:key inputs native-inputs  #:allow-other-keys)
              (let* ((share (string-append #$output "/share"))
                     (texmf-dist (string-append share "/texmf-dist"))
                     (web2c (string-append texmf-dist "/web2c"))
                     (fmtutil.cnf (string-append web2c "/fmtutil.cnf"))
                     (texlive-bin
                      #$(this-package-native-input "texlive-bin-full")))
                ;; LuaJIT is not ported to powerpc64* yet.
                (if #$(target-ppc64le?)
                    (substitute* fmtutil.cnf
                      (("^(luajittex|luajithbtex|mfluajit)" m)
                       (string-append "#! " m))))
                ;; Register paths in texmfcnf.lua, needed for context.
                (substitute* (string-append web2c "/texmfcnf.lua")
                  (("selfautodir:") #$output)
                  (("selfautoparent:") (string-append share "/")))
                ;; Set path to TeXLive Perl modules
                (setenv "PERL5LIB"
                        (string-append (getenv "PERL5LIB") ":"
                                       (string-append texlive-bin
                                                      "/share/tlpkg")))
                ;; Configure the texmf-dist tree.
                (setenv "GUIX_TEXMF" texmf-dist)
                (setenv "PATH"
                        (string-append (getenv "PATH") ":" texlive-bin "/bin:"))
                (let ((updmap.cfg (string-append web2c "/updmap.cfg")))
                  (invoke (string-append texlive-bin "/bin/updmap-sys")
                          "--nohash" "--syncwithtrees"
                          (string-append "--cnffile=" updmap.cfg)))
                (invoke (string-append texlive-bin "/bin/fmtutil-sys")
                        "--cnffile" fmtutil.cnf
                        "--all"
                        "--fmtdir" web2c)))))))
    (native-inputs (list texlive-bin-full))
    (inputs (list lua perl python-wrapper ruby tcsh))
    (properties `((max-silent-time . 9600))) ; don't time out while grafting
    (synopsis "TeX Live, a package of the TeX typesetting system")
    (description
     "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the complete tree of texmf-dist data.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
    (home-page "https://www.tug.org/texlive/")))

(define-public texlive
  (package
    (name "texlive")
    (version (package-version texlive-bin))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      ;; Build the union of texlive-bin-full and texlive-texmf, but take the
      ;; conflicting subdirectory share/texmf-dist from texlive-texmf.
      #~(begin
          (use-modules (guix build utils))
          (let ((bin #$(this-package-input "texlive-bin-full"))
                (texmf #$(this-package-input "texlive-texmf")))
            (mkdir #$output)
            (with-directory-excursion #$output
              ;; "include/" and "lib/" directories.
              (for-each
               (lambda (name)
                 (symlink (string-append bin "/" name) name))
               '("include" "lib"))
              ;; "bin/" directory.
              (mkdir "bin")
              (with-directory-excursion "bin"
                (for-each
                 (lambda (name) (symlink name (basename name)))
                 (find-files (string-append bin "/bin/") "")))
              ;; "share/info", "share/man", share/texmf-dist/" and
              ;; "share/tlpkg/" directories.
              (mkdir "share")
              (with-directory-excursion "share"
                (for-each
                 (lambda (name)
                   (symlink (string-append bin "/share/" name) name))
                 '("info" "man" "tlpkg"))
                (symlink (string-append texmf "/share/texmf-dist")
                         "texmf-dist"))
              ;; Now everything is in place, generate ls-R file.
              (setenv "PATH"
                      (string-append
                       (getenv "PATH") ":"
                       #$(this-package-input "texlive-bin-full") "/bin"))
              (invoke (string-append bin "/bin/mktexlsr")))))))
    (inputs (list texlive-bin-full texlive-texmf))
    (propagated-inputs (list texlive-libkpathsea))
    (native-search-paths
     (list (search-path-specification
            (variable "TEXMFLOCAL")
            (files '("share/texmf-local")))))
    (synopsis "TeX Live, a package of the TeX typesetting system")
    (description
     "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts that
are free software, including support for many languages around the world.

This package contains the complete TeX Live distribution.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
    (home-page "https://www.tug.org/texlive/")))

(define-public texlive-biber
  (package
    (name "texlive-biber")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/bibtex/biber/" "source/bibtex/biber/")
             (base32
              "0i1hqr9zb7b9d1zjlyg4awa6mkyq6wnrb6z4689j1rg07vnbd7mw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tests? #true
      #:imported-modules `(,@%texlive-build-system-modules
                           (guix build perl-build-system))
      #:modules '((guix build texlive-build-system)
                  ((guix build perl-build-system) #:prefix perl:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unpack-biber-source
            (lambda _
              (mkdir-p "build")
              (with-directory-excursion "build"
                (invoke "tar" "xvf"
                        "../source/bibtex/biber/biblatex-biber.tar.gz"
                        "--strip-components=1"))))
          (add-after 'unpack-biber-source 'build-biber
            (lambda args
              (with-directory-excursion "build"
                (for-each (lambda (phase)
                            (apply (assoc-ref perl:%standard-phases phase) args))
                          '(configure build check install)))))
          (add-after 'install 'wrap-programs
            (lambda _
              (let ((perl5lib (getenv "PERL5LIB")))
                (wrap-program (string-append #$output "/bin/biber")
                  `("PERL5LIB" ":" prefix
                    (,(string-append perl5lib ":"
                                     #$output "/lib/perl5/site_perl"))))))))))
    (native-inputs
     (list perl-config-autoconf
           perl-extutils-libbuilder
           perl-file-which
           perl-module-build
           perl-test-differences))
    (inputs
     (list perl
           perl-autovivification
           perl-class-accessor
           perl-data-dump
           perl-data-compare
           perl-data-uniqid
           perl-datetime-format-builder
           perl-datetime-calendar-julian
           perl-file-slurper
           perl-io-string
           perl-ipc-cmd
           perl-ipc-run3
           perl-list-allutils
           perl-list-moreutils
           perl-mozilla-ca
           perl-regexp-common
           perl-log-log4perl
           perl-parse-recdescent
           perl-unicode-collate
           perl-unicode-normalize
           perl-unicode-linebreak
           perl-encode-eucjpascii
           perl-encode-jis2k
           perl-encode-hanextra
           perl-xml-libxml
           perl-xml-libxml-simple
           perl-xml-libxslt
           perl-xml-writer
           perl-sort-key
           perl-text-csv
           perl-text-csv-xs
           perl-text-roman
           perl-uri
           perl-text-bibtex
           perl-libwww
           perl-lwp-protocol-https
           perl-business-isbn
           perl-business-issn
           perl-business-ismn
           perl-lingua-translit))
    (home-page "https://ctan.org/pkg/biber")
    (synopsis "BibTeX replacement for users of BibLaTeX")
    (description
     "Biber is a BibTeX replacement for users of BibLaTeX.  It supports full
UTF-8, can (re)-encode input and output, supports highly configurable sorting,
dynamic bibliography sets and many other features.")
    (license license:gpl3+)))

(define-deprecated-package biber texlive-biber)

(define-public rubber
  (package
    (name "rubber")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://launchpad.net/rubber/trunk/"
                                        version "/+download/rubber-"
                                        version ".tar.gz")
                         (string-append "http://ebeffara.free.fr/pub/rubber-"
                                        version ".tar.gz")))
              (sha256
               (base32
                "178dmrp0mza5gqjiqgk6dqs0c10s0c517pk6k9pjbam86vf47a1p"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; no `check' target
       #:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; texlive is required to build the PDF documentation; do not
             ;; build it.
             (invoke "python" "setup.py" "build" "--pdf=False" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))))))))
    (native-inputs (list texinfo))
    (home-page "https://launchpad.net/rubber")
    (synopsis "Wrapper for LaTeX and friends")
    (description
     "Rubber is a program whose purpose is to handle all tasks related to the
compilation of LaTeX documents.  This includes compiling the document itself,
of course, enough times so that all references are defined, and running BibTeX
to manage bibliographic references.  Automatic execution of dvips to produce
PostScript documents is also included, as well as usage of pdfLaTeX to produce
PDF documents.")
    (license license:gpl2+)))

(define-public texmaker
  (package
    (name "texmaker")
    (version "5.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.xm1math.net/texmaker/texmaker-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1qnh5g8zkjpjmw2l8spcynpfgs3wpcfcla5ms2kkgvkbdlzspqqx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Qt has its own configuration utility.
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "PREFIX=" out)
                       (string-append "DESKTOPDIR=" out "/share/applications")
                       (string-append "ICONDIR=" out "/share/pixmaps")
                       (string-append "METAINFODIR=" out "/share/metainfo")
                       "texmaker.pro")))))))
    (inputs
     (list poppler-qt5 qtbase-5 qtscript zlib))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.xm1math.net/texmaker/")
    (synopsis "LaTeX editor")
    (description "Texmaker is a program that integrates many tools needed to
develop documents with LaTeX, in a single application.")
    (license license:gpl2+)))

(define-public texstudio
  (package
    (name "texstudio")
    (version "4.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/texstudio-org/texstudio")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bzdcsc0273809hx04zqd2famq05q9rpvqcbqhkjqnqp9vxbisig"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f))                    ;tests work only with debug build
    (native-inputs
     (list pkg-config
           poppler-qt5
           qtdeclarative-5
           qtsvg-5
           qttools-5))
    (home-page "https://www.texstudio.org/")
    (synopsis "Feature-packed LaTeX editor")
    (description
     "TeXstudio is an integrated writing environment for creating LaTeX
documents.  It makes writing LaTeX comfortable by providing features such as
syntax-highlighting, an integrated viewer and reference checking.")
    (license license:gpl3)))

(define-public dvisvgm
  (package
    (name "dvisvgm")
    (version "3.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mgieseki/dvisvgm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11r401yqbw61n1mwsfk5qmwx2c92djwpl0q756qkds5kh25l9ci8"))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         python-wrapper
                         libtool))
    (inputs (list texlive-libkpathsea
                  freetype
                  fontforge
                  clipper
                  ghostscript
                  xxhash
                  google-brotli
                  woff2
                  zlib))
    (build-system gnu-build-system)
    (synopsis "Command-line utility for generating SVG from DVI, EPS and PDF
files")
    (description
     "Dvisvgm converts TeX DVI, EPS and PDF files into an
SVG (Scalable Vector Graphics) image.  It provides full font support including
virtual fonts, font maps and sub-fonts.  The embedded SVG fonts can optionally
be replaced with graphics paths for applications that do not support SVG
fonts.  Dvisvgm supports also colors, emTeX, tpic, papersize, PDF mapfile
and PostScript specials.  A working TeX installation is needed.")
    (home-page "https://dvisvgm.de/")
    (license license:gpl3+)))

(define-public teximpatient
  ;; The homepage seems to be distributing this version which is currently the
  ;; most recent commit
  (let ((commit "e3666abff186832fd9c467ceda3958058f30bac2")
        (revision "0"))
    (package
      (name "teximpatient")
      (version (git-version "2.4" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url
                       "https://git.savannah.gnu.org/git/teximpatient.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r30383nmly7w29il6v3vmilnnyrzak0x0qmabjvnpaga9ansjmi"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ;there are none
         #:allowed-references ("out")
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'fix-build
                      (lambda* (#:key inputs #:allow-other-keys)
                        (chdir "teximpatient")

                        ;; Remove generated files
                        (for-each delete-file
                                  '("book.pdf"
                                    "book.aux"
                                    "book.ccs"
                                    "book.log"
                                    "book.idx"
                                    "config.log"
                                    "config.status"
                                    "configure"
                                    "Makefile"))
                        (delete-file-recursively "autom4te.cache")

                        ;; make build reproducible
                        (substitute* "eplain.tex"
                          (("timestamp.*%")
                           (string-append "timestamp{"
                                          ,version "}"))))))))
      (native-inputs (list autoconf automake
                           (texlive-updmap.cfg (list texlive-amsfonts
                                                     texlive-palatino
                                                     texlive-zapfding
                                                     texlive-knuth-lib
                                                     texlive-mflogo-font
                                                     texlive-pdftex))))
      (home-page "https://www.gnu.org/software/teximpatient/")
      (synopsis "Book on TeX, plain TeX and Eplain")
      (description
       "@i{TeX for the Impatient} is a ~350 page book on TeX,
plain TeX, and Eplain, originally written by Paul Abrahams, Kathryn Hargreaves,
and Karl Berry.")
      (license license:fdl1.3+))))

(define-public lyx
  (package
    (name "lyx")
    (version "2.3.7")
    (source (origin
              (method url-fetch)
              ;; XXX: Upstream version is 2.3.7, but they released a suffixed
              ;; tarball.  This can probably be removed after next release.
              (uri (let ((suffix "-1"))
                     (string-append "https://ftp.lyx.org/pub/lyx/stable/"
                                    (version-major+minor version) ".x/"
                                    "lyx-" version suffix ".tar.xz")))
              (sha256
               (base32
                "1vfq30big55038bcymh83xh9dqp9wn0gnw0f6644xcw6zdj8igir"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "3rdparty")))))
    (build-system qt-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DLYX_USE_QT=QT5"
                   "-DLYX_EXTERNAL_BOOST=1"
                   "-DLYX_INSTALL=1"
                   "-DLYX_RELEASE=1"
                   "-DLYX_PROGRAM_SUFFIX=OFF"
                   (string-append "-DLYX_INSTALL_PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-python
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* '("lib/configure.py"
                                  "src/support/ForkedCalls.cpp"
                                  "src/support/Systemcall.cpp"
                                  "src/support/os.cpp"
                                  "src/support/filetools.cpp")
                     (("\"python ")
                      (string-append "\""
                                     (search-input-file inputs "/bin/python3")
                                     " ")))))
               (add-after 'unpack 'add-missing-test-file
                 (lambda _
                   ;; Create missing file that would cause tests to fail.
                   (with-output-to-file "src/tests/check_layout.cmake"
                     (const #t)))))))
    (inputs
     ;; XXX: Aspell library is properly detected during build, but hunspell
     ;; isn't.  So we use the former here.
     (list aspell
           boost
           libx11
           mythes
           python
           qtbase-5
           qtsvg-5
           zlib))
    (propagated-inputs
     (list (texlive-updmap.cfg (list texlive-ec))))
    (native-inputs
     (list python pkg-config))
    (home-page "https://www.lyx.org/")
    (synopsis "Document preparation system with GUI")
    (description "LyX is a document preparation system.  It excels at letting
you create complex technical and scientific articles with mathematics,
cross-references, bibliographies, indexes, etc.  It is very good for working
with documents of any length in which the usual processing abilities are
required: automatic sectioning and pagination, spell checking and so forth.")
    (license license:gpl2+)))

(define-public texlive-media9
  (package
    (name "texlive-media9")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/media9/"
                   "source/latex/media9/files/"
                   "source/latex/media9/players/"
                   "tex/latex/media9/javascript/"
                   "tex/latex/media9/players/")
             (base32
              "03m59icz29sdp50wpl831pl67q9m6kzpq5fzx4jix50z7cmqvfrm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/media9")
    (synopsis "Multimedia inclusion package with Adobe Reader-9/X compatibility")
    (description
     "The package provides an interface to embed interactive Flash (SWF) and
3D objects (Adobe U3D & PRC), as well as video and sound files or streams in
the popular MP4, FLV and MP3 formats into PDF documents with Acrobat-9/X
compatibility.  Playback of multimedia files uses the built-in Flash Player of
Adobe Reader and does, therefore, not depend on external plug-ins.  Flash
Player supports the efficient H.264 codec for video compression.

The package is based on the RichMedia Annotation, an Adobe addition to the PDF
specification.  It replaces the now obsolete @code{movie15} package.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-media9 texlive-media9)

(define-public texlive-ocgx2
  (package
    (name "texlive-ocgx2")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ocgx2/" "tex/latex/ocgx2/")
             (base32
              "0jpvwy6sp7almdbhxizz22h3jxgfnsl4nirs93p7y1lqdgc4srl4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ocgx2")
    (synopsis "Drop-in replacement for 'ocgx' and 'ocg-p'")
    (description
     "This package serves as a drop-in replacement for the packages
@code{ocgx} by Paul Gaborit and @code{ocg-p} by Werner Moshammer for the
creation of PDF Layers.  It re-implements the functionality of the @code{ocg},
@code{ocgx}, and @code{ocg-p} packages and adds support for all known engines
and back-ends.  It also ensures compatibility with the @code{media9} and
@code{animate} packages.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-ocgx2 texlive-ocgx2)

(define-public texlive-ms
  (package
    (name "texlive-ms")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ms/" "source/latex/ms/"
                   "tex/latex/ms/")
             (base32
              "1cgrpx5mybiirjjdmni8kvqdg37dwfkixq3h9ami0mgxqqqfl2x3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/ms")
    (synopsis "Various LaTeX packages by Martin Schroder")
    (description
     "This package is the remains of a bundle of LaTeX packages by Martin
Schroder; the collection comprises: @code{count1to}, make use of TeX counters;
and @code{multitoc}, typeset the table of contents in multiple columns.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-ms texlive-ms)

(define-public texlive-ncctools
  (package
    (name "texlive-ncctools")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ncctools/" "source/latex/ncctools/"
                   "tex/latex/ncctools/")
             (base32
              "1g3fpvrg6kx2ns97ih6iwdk0rcbxlv043x8rdppxdincl2lvbdx5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ncctools")
    (synopsis "A collection of general packages for LaTeX")
    (description
     "The NCCtools bundle contains many packages for general use under LaTeX;
many are also used by NCC LaTeX.  The bundle includes tools for:
@itemize
@item executing commands after a package is loaded;
@item watermarks;
@item counter manipulation;
@item improvements to the @code{description} environment;
@item hyphenation of compound words;
@item new levels of footnotes;
@item space-filling patterns;
@item poor man's Black Board Bold symbols;
@item alignment of the content of a box; use comma as decimal separator;
@item boxes with their own crop marks;
@item page cropmarks;
@item improvements to fancy headers;
@item float styles, mini floats, side floats;
@item manually marked footnotes;
@item extension of amsmath;
@item control of paragraph skip;
@item an envelope to the @code{graphicx} package;
@item dashed and multiple rules;
@item alternative techniques for declarations of sections, captions, and
toc-entries;
@item generalised text-stretching;
@item generation of new theorem-like environments;
@item control of the text area;
@item centered page layouts;
@item un-numbered top-level section.
@end itemize")
    (license license:lppl)))

(define-public texlive-ncntrsbk
  (package
    (name "texlive-ncntrsbk")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/ncntrsbk/"
                   "fonts/afm/adobe/ncntrsbk/"
                   "fonts/afm/urw/ncntrsbk/"
                   "fonts/map/dvips/ncntrsbk/"
                   "fonts/tfm/adobe/ncntrsbk/"
                   "fonts/tfm/urw35vf/ncntrsbk/"
                   "fonts/type1/urw/ncntrsbk/"
                   "fonts/vf/adobe/ncntrsbk/"
                   "fonts/vf/urw35vf/ncntrsbk/"
                   "tex/latex/ncntrsbk/")
             (base32
              "0i6a48zbn9lg4pwbw8ya2wjjgppwac816fnbpmahm67kknx4d7ij")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's New Century Schoolbook font")
    (description
     "This package provides a drop-in replacement for the New Century
Schoolbook font from Adobe's basic set.")
    (license license:gpl3+)))

(define-public texlive-numprint
  (package
    (name "texlive-numprint")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/numprint/" "source/latex/numprint/"
                   "tex/latex/numprint/")
             (base32
              "1rqbqj4ffcfxxxxbs100pdslaiimwzgg19mf2qzcmm5snxwrf7zj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/numprint")
    (synopsis "Print numbers with separators and exponent if necessary")
    (description
     "The package numprint prints numbers with a separator every three digits
and converts numbers given as @samp{12345.6e789} to @samp{12\\,345,6\\cdot
10^@{789@}}.  Numbers are printed in the current mode (text or math) in order
to use the correct font.

Many things, including the decimal sign, the thousand separator, as well as
the product sign can be changed by the user.  If an optional argument is given
it is printed upright as unit.  Numbers can be rounded to a given number of
digits.  The package supports an automatic, language-dependent change of the
number format.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-numprint texlive-numprint)

(define-public texlive-needspace
  (package
    (name "texlive-needspace")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/latex/needspace/"
                                  "source/latex/needspace/"
                                  "tex/latex/needspace/")
                            (base32
                             "12hbvv1w6b1k29qjvp72bkpnzsxpvrimzshllwinrxh9rx1mn550")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (native-inputs (list texlive-filecontents))
    (home-page "https://ctan.org/pkg/needspace")
    (synopsis "Insert pagebreak if not enough space")
    (description
     "This package provides commands to disable pagebreaking within a given
vertical space.  If there is not enough space between the command and the
bottom of the page, a new page will be started.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-needspace texlive-needspace)

(define-public texlive-changepage
  (package
    (name "texlive-changepage")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/changepage/"
                   "source/latex/changepage/"
                   "tex/latex/changepage/")
             (base32
              "0g9zlbqrgxh3p2vys2s84i8v590qi4fbpppp5lkaqc1di8kw60lm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (native-inputs (list texlive-filecontents))
    (home-page "https://ctan.org/pkg/changepage")
    (synopsis "Margin adjustment and detection of odd/even pages")
    (description
     "The package provides commands to change the page layout in the middle of
a document, and to robustly check for typesetting on odd or even pages.  The
package is an extraction of code from the @code{memoir} class, whose user
interface it shares.  This package will eventually replace the @code{chngpage}
package, which is distributed with the package.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-changepage texlive-changepage)

(define-public texlive-eukdate
  (package
    (name "texlive-eukdate")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/eukdate/" "source/latex/eukdate/"
                   "tex/latex/eukdate/")
             (base32
              "1bz32l4500y4sx7ighpcvzfh0z45lzyyxm1dq1knmhdsv46gqaxi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/eukdate")
    (synopsis "UK format dates, with weekday")
    (description
     "The package is used to change the format of @code{\\today}’s date,
including the weekday, e.g., @samp{Saturday, 26 June 2008}, the UK format,
which is preferred in many parts of the world, as distinct from that which is
used in @code{\\maketitle} of the article class, @samp{June 26, 2008}, the US
format.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-eukdate texlive-eukdate)

(define-public texlive-euler
  (package
    (name "texlive-euler")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/euler/" "source/latex/euler/"
                   "tex/latex/euler/")
             (base32
              "0xd4lniaj243jvmlgfan7rp1zx308cfvpdd02nvvqpdnrwc69irk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/euler")
    (synopsis "Use AMS Euler fonts for math")
    (description
     "This package provides a setup for using the AMS Euler family of fonts
for mathematics in LaTeX documents. ``The underlying philosophy of Zapf's
Euler design was to capture the flavour of mathematics as it might be written
by a mathematician with excellent handwriting.'' The @code{euler} package is
based on Knuth's macros for the book Concrete Mathematics'.  The text fonts
for the Concrete book are supported by the @code{beton} package.")
    (license license:lppl)))

(define-public texlive-extsizes
  (package
    (name "texlive-extsizes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/extsizes/" "tex/latex/extsizes/")
             (base32
              "1akxh0x8y1rhmpq8qzqi2bpbm1ffy8x0212jkyib7gm1i1d9ijgl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/extsizes")
    (synopsis "Extend the standard classes' size options")
    (description
     "This package provides classes @code{extarticle}, @code{extreport},
@code{extletter}, @code{extbook} and @code{extproc} which provide for
documents with a base font size from 8-20pt.  There is also a LaTeX package,
@file{extsizes.sty}, which can be used with nonstandard document classes.  But
it cannot be guaranteed to work with any given class.")
    (license license:lppl)))

(define-public texlive-ulem
  (package
    (name "texlive-ulem")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/ulem/" "tex/generic/ulem/")
             (base32
              "0wcfnw5h6lsg2ilvkkf7mns8jgcn0n5sh45iznfsb49pfb4mming")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ulem")
    (synopsis "Package for underlining")
    (description
     "The package provides an @code{\\ul} (underline) command which will break
over line ends; this technique may be used to replace @code{\\em} (both in
that form and as the @code{\\emph} command), so as to make output look as if
it comes from a typewriter.  The package also offers double and wavy
underlining, and striking out, and crossing out.")
    (license license:lppl1.3c+)))

(define-public texlive-pgf
  (package
    (name "texlive-pgf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/pgf/"
                   "tex/context/third/pgf/basiclayer/"
                   "tex/context/third/pgf/frontendlayer/"
                   "tex/context/third/pgf/math/"
                   "tex/context/third/pgf/systemlayer/"
                   "tex/context/third/pgf/utilities/"
                   "tex/generic/pgf/basiclayer/"
                   "tex/generic/pgf/frontendlayer/tikz/libraries/circuits/"
                   "tex/generic/pgf/frontendlayer/tikz/libraries/datavisualization/"
                   "tex/generic/pgf/frontendlayer/tikz/libraries/graphs/"
                   "tex/generic/pgf/graphdrawing/lua/"
                   "tex/generic/pgf/graphdrawing/tex/experimental/"
                   "tex/generic/pgf/libraries/datavisualization/"
                   "tex/generic/pgf/libraries/decorations/"
                   "tex/generic/pgf/libraries/luamath/pgf/luamath/"
                   "tex/generic/pgf/libraries/shapes/circuits/"
                   "tex/generic/pgf/lua/pgf/"
                   "tex/generic/pgf/math/"
                   "tex/generic/pgf/modules/"
                   "tex/generic/pgf/systemlayer/"
                   "tex/generic/pgf/utilities/"
                   "tex/latex/pgf/basiclayer/"
                   "tex/latex/pgf/compatibility/"
                   "tex/latex/pgf/doc/"
                   "tex/latex/pgf/frontendlayer/libraries/"
                   "tex/latex/pgf/math/"
                   "tex/latex/pgf/systemlayer/"
                   "tex/latex/pgf/utilities/"
                   "tex/plain/pgf/basiclayer/"
                   "tex/plain/pgf/frontendlayer/"
                   "tex/plain/pgf/math/"
                   "tex/plain/pgf/systemlayer/"
                   "tex/plain/pgf/utilities/")
             (base32
              "1rkrp839snkfmxn0ff3kcvgq4k59lj2c3xz05hmmnprzymsx1zvd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atveryend
           texlive-fp
           texlive-graphics
           texlive-ms
           texlive-pdftexcmds
           texlive-xcolor))
    (home-page "https://ctan.org/pkg/pgf")
    (synopsis "Create PostScript and PDF graphics in TeX")
    (description
     "PGF is a macro package for creating graphics.  It is platform- and
format-independent and works together with the most important TeX backend
drivers, including pdfTeX and dvips.  It comes with a user-friendly syntax
layer called TikZ.  Its usage is similar to @code{pstricks} and the standard
@code{picture} environment.  PGF works with plain (pdf-)TeX, (pdf-)LaTeX, and
ConTeXt.  Unlike @code{pstricks}, it can produce either PostScript or PDF
output.")
    ;; The code of the package is dual-license: GPL-2 or LPPL-1.3c+.  The
    ;; documentation is also dual-license: LPPL-1.3c+ or GFDL-1.2.
    (license (list license:gpl2 license:lppl1.3c+ license:fdl1.2+))))

(define-deprecated-package texlive-latex-pgf texlive-pgf)

(define-public texlive-koma-script
  (package
    (name "texlive-koma-script")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/koma-script/"
                   "source/latex/koma-script/doc/"
                   "tex/latex/koma-script/")
             (base32
              "192jlijzrqipdi2bxg1562259zxivpzm8clbnpr0fk1rr1nc2lz1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-footmisc))
    (home-page "https://ctan.org/pkg/koma-script")
    (synopsis "Bundle of versatile classes and packages")
    (description
     "The KOMA-Script bundle provides replacements for the article, report,
and book classes with emphasis on typography and versatility.  There is also
a letter class.  The bundle also offers: a package for calculating type areas
in the way laid down by the typographer Jan Tschichold, packages for easily
changing and defining page styles, a package @code{scrdate} for getting not
only the current date but also the name of the day, and a package
@code{scrtime} for getting the current time.  All these packages may be used
not only with KOMA-Script classes but also with the standard classes.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-koma-script texlive-koma-script)

(define-public texlive-atbegshi
  (package
    (name "texlive-atbegshi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/atbegshi/" "source/latex/atbegshi/"
                   "tex/generic/atbegshi/")
             (base32
              "0vd90wdjwj5w4g4xka4nms3rgixjw63iwf0hj0v1akcfflwvgn69")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-bin? #f))
    (native-inputs (list texlive-docstrip texlive-pdftex))
    (home-page "https://ctan.org/pkg/atbegshi")
    (synopsis "Execute commands at @code{\\shipout} time")
    (description
     "This package is a modern reimplementation of package @code{everyshi},
providing various commands to be executed before a @code{\\shipout} command.
It makes use of e-TeX’s facilities if they are available.  The package may
be used either with LaTeX or with plain TeX.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-atbegshi texlive-atbegshi)

(define-public texlive-bigintcalc
  (package
    (name "texlive-bigintcalc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/bigintcalc/"
                   "source/latex/bigintcalc/"
                   "tex/generic/bigintcalc/")
             (base32
              "1cyv4mcvx83ab782l6h2f86a63ipm845r7hv1m6f1z2336vy7rc5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bigintcalc")
    (synopsis "Integer calculations on very large numbers")
    (description
     "This package provides expandable arithmetic operations with big integers
that can exceed TeX's number limits.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-bigintcalc texlive-bigintcalc)

(define-public texlive-bitset
  (package
    (name "texlive-bitset")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/bitset/" "source/latex/bitset/"
                   "tex/generic/bitset/")
             (base32
              "1q7vk5gr5a4vaa3l20j178cg2q7a99rxdiyxhzpx9a6lfqfkjddz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-bigintcalc))
    (home-page "https://ctan.org/pkg/bitset")
    (synopsis "Handle bit-vector datatype")
    (description
     "This package defines and implements the data type bit set, a vector of
bits.  The size of the vector may grow dynamically.  Individual bits can be
manipulated.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-bitset texlive-bitset)

(define-public texlive-etexcmds
  (package
    (name "texlive-etexcmds")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/etexcmds/" "source/latex/etexcmds/"
                   "tex/generic/etexcmds/")
             (base32
              "13cf1fs5x9d8749b2jgxmgnkrx0r4hwpl389r15kq3ldz9jfl627")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/etexcmds")
    (synopsis "Avoid name clashes with e-TeX commands")
    (description
     "New primitive commands are introduced in e-TeX; sometimes the names
collide with existing macros.  This package solves the name clashes by adding
a prefix to e-TeX’s commands.  For example, ε-TeX’s @code{\\unexpanded} is
provided as @code{\\etex@@unexpanded}.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-etexcmds texlive-etexcmds)

(define-public texlive-etextools
  (package
    (name "texlive-etextools")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/etextools/"
                   "source/latex/etextools/"
                   "tex/latex/etextools/")
             (base32
              "0bfcc8g8q5v1nyqmrg8n17hv4k8yvhsplansvriccpmvyx0w9y9d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/etextools")
    (synopsis "e-TeX tools for LaTeX users and package writers")
    (description
     "The package provides many (purely expandable) tools for LaTeX: extensive
list management; purely expandable loops; conversion; addition/deletion;
expansion and group control; tests on tokens, characters and control
sequences; tests on strings; purely expandable macros with options or
modifiers; some purely expandable numerics.")
    (license license:lppl)))

(define-public texlive-gettitlestring
  (package
    (name "texlive-gettitlestring")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/gettitlestring/"
                   "source/latex/gettitlestring/"
                   "tex/generic/gettitlestring/")
             (base32
              "1vbvmwrpsvy37gbwdmsqbbsicjiww3i0bh1yqnb75jiya9an0sjb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/gettitlestring")
    (synopsis "Clean up title references")
    (description
     "This package provides commands for cleaning up the title string
(such as removing @code{\\label} commands) for packages that typeset such
strings.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-gettitlestring texlive-gettitlestring)

(define-public texlive-infwarerr
  (package
    (name "texlive-infwarerr")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/infwarerr/"
                   "source/latex/infwarerr/"
                   "tex/generic/infwarerr/")
             (base32
              "0lpcrpf3d6xfdp68ri22126x57mvmq5dpj9np68ph8p8lhvhqdjd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/infwarerr")
    (synopsis "Complete set of information/warning/error message macros")
    (description
     "This package provides a complete set of macros for information, warning
and error messages.  Under LaTeX, the commands are wrappers for the
corresponding LaTeX commands; under Plain TeX they are available as complete
implementations.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-infwarerr texlive-infwarerr)

(define-public texlive-intcalc
  (package
    (name "texlive-intcalc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/intcalc/" "source/latex/intcalc/"
                   "tex/generic/intcalc/")
             (base32
              "15alwp9cr8wasfajs3p201p7nqml37vly9mpg1j5l6xv95javk7x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/intcalc")
    (synopsis "Expandable arithmetic operations with integers")
    (description
     "This package provides expandable arithmetic operations with integers,
using the e-TeX extension @code{\\numexpr} if it is available.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-intcalc texlive-intcalc)

(define-public texlive-kvdefinekeys
  (package
    (name "texlive-kvdefinekeys")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/kvdefinekeys/"
                   "source/latex/kvdefinekeys/"
                   "tex/generic/kvdefinekeys/")
             (base32
              "1026h223ph3nzhs6jqbasa0bzsrdg3zgllfcwwcavfzb5i6p9jdf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/kvdefinekeys")
    (synopsis "Define keys for use in the @code{kvsetkeys} package")
    (description
     "This package provides the @code{\\kv@@define@@key} (analogous to
keyval’s @code{\\define@@key}, to define keys for use by @code{kvsetkeys}.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-kvdefinekeys texlive-kvdefinekeys)

(define-public texlive-kvsetkeys
  (package
    (name "texlive-kvsetkeys")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/kvsetkeys/"
                   "source/latex/kvsetkeys/"
                   "tex/latex/kvsetkeys/")
             (base32
              "0c4f4sgb3xpxmvphrvzbyqa2vl7sp2j52hb99spmpbqwgj9j61qx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/kvsetkeys")
    (synopsis "Key value parser with default handler support")
    (description
     "This package provides @code{\\kvsetkeys}, a variant of @code{\\setkeys}
from the @code{keyval} package.  Users can specify a handler that deals with
unknown options.  Active commas and equal signs may be used, and only one
level of curly braces are removed from the values.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-kvsetkeys texlive-kvsetkeys)

(define-public texlive-listofitems
  (package
    (name "texlive-listofitems")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/listofitems/"
                   "tex/generic/listofitems/")
             (base32
              "0yy0hw3631shf9rrdiyywr7hs7dvrhvz2a2vkzbwalnyrqdwbyh5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/listofitems")
    (synopsis "Grab items in lists using user-specified separation character")
    (description
     "This package is designed to read a list, for which the parsing character
has been selected by the user, and to access any of these items with a simple
interface.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-listofitems texlive-listofitems)

(define-public texlive-ltxcmds
  (package
    (name "texlive-ltxcmds")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/ltxcmds/"
                   "source/generic/ltxcmds/"
                   "tex/generic/ltxcmds/")
             (base32
              "1izcw9jl64iij541183hc156sjwamvxm7q9fkpfnz8sppyg31fkb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ltxcmds")
    (synopsis "Some LaTeX kernel commands for general use")
    (description
     "This package exports some utility macros from the LaTeX kernel into
a separate namespace and also makes them available for other formats such as
plain TeX.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-ltxcmds texlive-ltxcmds)

(define-public texlive-ltxmisc
  (package
    (name "texlive-ltxmisc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/latex/ltxmisc/")
             (base32
              "14llkpla8gpx7q6a53sd8a0a42wgk93fg4mbl6pc0v7v1kjblr5m")))
    (build-system texlive-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'delete-non-free-file
                 ;; This file has a non-commercial license.
                 (lambda _ (delete-file "tex/latex/ltxmisc/vrbexin.sty"))))))
    (home-page "https://ctan.org/pkg/ltxmisc")
    (synopsis "Miscellaneous LaTeX packages")
    (description
     "This package provides miscellaneous LaTeX packages and classes.")
    (license (list license:public-domain ;beletter.cls
                   license:gpl2+         ;bibcheck.sty
                   license:lppl          ;iagproc.cls
                   (license:fsf-free "file:/linsys.sty")
                   license:lppl1.0+)))) ;topcapt.sty

(define-public texlive-pdfescape
  (package
    (name "texlive-pdfescape")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pdfescape/"
                   "source/latex/pdfescape/"
                   "tex/generic/pdfescape/")
             (base32
              "16a0rdmpa4wxh6gyf46qwfgyh399rwdind2wc89phqd50ky9b5m4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pdfescape")
    (synopsis "Implements pdfTeX's escape features using TeX or e-TeX")
    (description
     "This package implements pdfTeX's escape features (@code{\\pdfescapehex},
@code{\\pdfunescapehex}, @code{\\pdfescapename}, @code{\\pdfescapestring})
using TeX or e-TeX.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-generic-pdfescape texlive-pdfescape)

(define-public texlive-uniquecounter
  (package
    (name "texlive-uniquecounter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/uniquecounter/"
                   "source/latex/uniquecounter/"
                   "tex/generic/uniquecounter/")
             (base32
              "1ll3iwk8x44l3qx1dhna399ngg66vbllivv8i3lwzriwkx22xbf3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/uniquecounter")
    (synopsis "Provides unlimited unique counter")
    (description
     "This package provides a kind of counter that provides unique number values.
Several counters can be created with different names.  The numeric values are
not limited.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-uniquecounter texlive-uniquecounter)

(define-public texlive-readarray
  (package
    (name "texlive-readarray")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/readarray/" "tex/latex/readarray/")
             (base32
              "0iy3m20761mp83g59qpx9l20bg3fvm28l64vq735rcha7yqbhf22")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/readarray")
    (synopsis "Read, store and recall array-formatted data")
    (description
     "This package allows the user to input formatted data into elements of
a 2-D or 3-D array and to recall that data at will by individual cell number.
The data can be but need not be numerical in nature.  It can be, for example,
formatted text.")
    (license license:lppl1.3)))

(define-deprecated-package texlive-latex-readarray texlive-readarray)

(define-public texlive-verbatimbox
  (package
    (name "texlive-verbatimbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/verbatimbox/"
                   "tex/latex/verbatimbox/")
             (base32
              "00n3x075ya3s2qwmcz2vvn8x70pbbgj2cbwz0ifw89jrc4ljisgi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://www.ctan.org/pkg/verbatimbox")
    (synopsis "Deposit verbatim text in a box")
    (description
     "The package provides a @code{verbbox} environment to place its contents
into a globally available box, or into a box specified by the user.  The
global box may then be used in a variety of situations (for example, providing
a replica of the @code{boxedverbatim} environment itself).  A valuable use is
in places where the standard @code{verbatim} environment (which is based on a
@code{trivlist}) may not appear.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-verbatimbox texlive-verbatimbox)

(define-public texlive-examplep
  (package
    (name "texlive-examplep")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/examplep/" "tex/latex/examplep/")
             (base32
              "0afbl77i57hxngc3l0czdzmmkhcgh2l4h2dpbg9ax9p9dv8c006n")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/examplep")
    (synopsis "Verbatim phrases and listings in LaTeX")
    (description
     "The @code{examplep} package provides sophisticated features for
typesetting verbatim source code listings, including the display of the source
code and its compiled LaTeX or Metapost output side-by-side, with automatic
width detection and enabled page breaks (in the source), without the need for
specifying the source twice.  Special care is taken that section, page and
footnote numbers do not interfere with the main document.  For typesetting
short verbatim phrases, a replacement for the @code{\\verb} command is also
provided in the package, which can be used inside tables and moving arguments
such as footnotes and section titles.")
    ;; No version of the GPL is specified.
    (license license:gpl3+)))

(define-deprecated-package texlive-latex-examplep texlive-examplep)

(define-public texlive-xunicode
  (package
    (name "texlive-xunicode")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/xunicode/"
                   "tex/xelatex/xunicode/")
             (base32
              "1d96i8kd2lhbykc3rxy2jjvws404f2vy1cvdcp5bdr6l9m72q1fa")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-tipa))
    (home-page "https://ctan.org/pkg/xunicode")
    (synopsis "Generate Unicode characters from accented glyphs")
    (description
     "The package supports XeTeX's (and other putative future similar
engines') need for Unicode characters, in a similar way to what the fontenc
does for 8-bit (and the like) fonts: convert accent-glyph sequence to a single
Unicode character for output.  The package also covers glyphs specified by
packages (such as @code{tipa}) which define many commands for single text
glyphs.")
    (license license:lppl1.3+)))

(define-public texlive-xypic
  (package
    (name "texlive-xypic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/xypic/"
                   "dvips/xypic/"
                   "fonts/afm/public/xypic/"
                   "fonts/enc/dvips/xypic/"
                   "fonts/map/dvips/xypic/"
                   "fonts/source/public/xypic/"
                   "fonts/tfm/public/xypic/"
                   "fonts/type1/public/xypic/"
                   "tex/generic/xypic/")
             (base32
              "09b51bbm189xh7039h5n8nmab5nn2bybhh26qjn08763m80zdhjg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/xypic")
    (synopsis "Flexible diagramming macros")
    (description
     "This package provides a package for typesetting a variety of graphs and
diagrams with TeX.  Xy-pic works with most formats (including LaTeX,
AMS-LaTeX, AMS-TeX, and plain TeX).  The distribution includes Michael Barr's
@code{diag} package, which was previously distributed stand-alone.")
    (license license:gpl3+)))

(define-deprecated-package texlive-fonts-xypic texlive-xypic)

(define-deprecated-package texlive-generic-xypic texlive-xypic)

(define-public texlive-bibtex
  (package
    (name "texlive-bibtex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bib/base/"
                   "bibtex/bst/base/"
                   "doc/bibtex/base/"
                   "doc/man/man1/bibtex.1"
                   "doc/man/man1/bibtex.man1.pdf"
                   "tex/generic/bibtex/")
             (base32
              "0iyaxab3wyhy3nw0id892aklpqf17z1cl85v4m3rjy5nmb8darn9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-kpathsea))
    (home-page "https://ctan.org/pkg/bibtex")
    (synopsis "Process bibliographies for LaTeX")
    (description
     "BibTeX allows the user to store his citation data in generic form, while
printing citations in a document in the form specified by a BibTeX style, to
be specified in the document itself (one often needs a LaTeX citation-style
package, such as @command{natbib} as well).")
    (license license:knuth)))

(define-public texlive-charter
  (package
    (name "texlive-charter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/charter/"
                   "fonts/afm/bitstrea/charter/"
                   "fonts/tfm/bitstrea/charter/"
                   "fonts/type1/bitstrea/charter/"
                   "fonts/vf/bitstrea/charter/")
             (base32
              "09l5ymgz48s3hyn776l01g3isk3dnhrj1vdavdw4qq4kfxxpqdn9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     ;; This provides charter.map.
     (list texlive-psnfss))
    (home-page "https://www.ctan.org/pkg/charter")
    (synopsis "Charter fonts for TeX")
    (description "This package provides a copy of the Charter Type-1 fonts
which Bitstream contributed to the X consortium, renamed for use with TeX.
Support for use with LaTeX is available in @code{freenfss}, part of
@command{psnfss}.")
    (license (license:non-copyleft
              "http://mirrors.ctan.org/fonts/charter/readme.charter"))))

(define-deprecated-package texlive-fonts-charter texlive-charter)

(define-public texlive-chngcntr
  (package
    (name "texlive-chngcntr")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/chngcntr/" "tex/latex/chngcntr/")
             (base32
              "0ag1hb1vkl0xdzslp6f0j59dijwr9k9kq7vaada148241q0hflbj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/chngcntr")
    (synopsis "Change the resetting of counters")
    (description
     "This package defines commands @code{\\counterwithin} (which sets up
a counter to be reset when another is incremented) and
@code{\\counterwithout} (which unsets such a relationship).")
    (license license:lppl)))

(define-public texlive-context
  (package
    (name "texlive-context")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/context/mkii/"
                   "context/data/scite/context/documents/"
                   "context/data/scite/context/lexers/data/"
                   "context/data/scite/context/lexers/themes/"
                   "context/data/texworks/TUG/"
                   "context/data/texworks/completion/"
                   "context/data/texworks/configuration/"
                   "context/data/vscode/extensions/context/keybindings/"
                   "context/data/vscode/extensions/context/settings/"
                   "context/data/vscode/extensions/context/syntaxes/"
                   "context/data/vscode/extensions/context/tasks/"
                   "context/data/vscode/extensions/context/themes/"
                   "doc/context/documents/general/leaflets/"
                   "doc/context/documents/general/magazines/"
                   "doc/context/documents/general/manuals/"
                   "doc/context/documents/general/qrcs/"
                   "doc/context/examples/calculator/"
                   "doc/context/examples/clock/"
                   "doc/context/presentations/bachotex/2005/"
                   "doc/context/presentations/bachotex/2009/"
                   "doc/context/presentations/bachotex/2010/"
                   "doc/context/presentations/bachotex/2011/"
                   "doc/context/presentations/bachotex/2012/"
                   "doc/context/presentations/bachotex/2013/"
                   "doc/context/presentations/bachotex/2014/"
                   "doc/context/presentations/bachotex/2015/"
                   "doc/context/presentations/bachotex/2016/"
                   "doc/context/presentations/bachotex/2017/"
                   "doc/context/presentations/bachotex/2018/"
                   "doc/context/presentations/bachotex/2019/"
                   "doc/context/presentations/context/2007/"
                   "doc/context/presentations/context/2010/"
                   "doc/context/presentations/context/2011/"
                   "doc/context/presentations/context/2012/"
                   "doc/context/presentations/context/2013/"
                   "doc/context/presentations/context/2015/"
                   "doc/context/presentations/context/2016/"
                   "doc/context/presentations/context/2017/"
                   "doc/context/presentations/context/2019/"
                   "doc/context/presentations/context/2020/"
                   "doc/context/presentations/context/2021/"
                   "doc/context/presentations/examples/"
                   "doc/context/presentations/tug/2001/"
                   "doc/context/presentations/tug/2007/"
                   "doc/context/scripts/mkii/"
                   "doc/context/scripts/mkiv/"
                   "doc/context/sources/general/leaflets/"
                   "doc/context/sources/general/magazines/"
                   "doc/context/sources/general/manuals/about/"
                   "doc/context/sources/general/manuals/bidi/"
                   "doc/context/sources/general/manuals/canbedone/"
                   "doc/context/sources/general/manuals/charts/"
                   "doc/context/sources/general/manuals/cld/"
                   "doc/context/sources/general/manuals/colors/"
                   "doc/context/sources/general/manuals/columnsets/"
                   "doc/context/sources/general/manuals/details/"
                   "doc/context/sources/general/manuals/epub/"
                   "doc/context/sources/general/manuals/evenmore/"
                   "doc/context/sources/general/manuals/followingup/"
                   "doc/context/sources/general/manuals/fonts/"
                   "doc/context/sources/general/manuals/graphics/"
                   "doc/context/sources/general/manuals/hybrid/"
                   "doc/context/sources/general/manuals/interaction/"
                   "doc/context/sources/general/manuals/languages/"
                   "doc/context/sources/general/manuals/libraries/"
                   "doc/context/sources/general/manuals/lowlevel/"
                   "doc/context/sources/general/manuals/lua/"
                   "doc/context/sources/general/manuals/luametafun/"
                   "doc/context/sources/general/manuals/luametatex/"
                   "doc/context/sources/general/manuals/luatex/"
                   "doc/context/sources/general/manuals/math/"
                   "doc/context/sources/general/manuals/mathml/"
                   "doc/context/sources/general/manuals/metafun/"
                   "doc/context/sources/general/manuals/mk/"
                   "doc/context/sources/general/manuals/musings/"
                   "doc/context/sources/general/manuals/nodes/"
                   "doc/context/sources/general/manuals/notnow/"
                   "doc/context/sources/general/manuals/onandon/"
                   "doc/context/sources/general/manuals/ontarget/"
                   "doc/context/sources/general/manuals/pagecolumns/"
                   "doc/context/sources/general/manuals/primitives/"
                   "doc/context/sources/general/manuals/publications/"
                   "doc/context/sources/general/manuals/readme/"
                   "doc/context/sources/general/manuals/rules/"
                   "doc/context/sources/general/manuals/spacing/"
                   "doc/context/sources/general/manuals/spreadsheets/"
                   "doc/context/sources/general/manuals/sql/"
                   "doc/context/sources/general/manuals/start/en/"
                   "doc/context/sources/general/manuals/start/graphics/"
                   "doc/context/sources/general/manuals/steps/"
                   "doc/context/sources/general/manuals/still/"
                   "doc/context/sources/general/manuals/svg/"
                   "doc/context/sources/general/manuals/swiglib/"
                   "doc/context/sources/general/manuals/templates/"
                   "doc/context/sources/general/manuals/texit/"
                   "doc/context/sources/general/manuals/tiptrick/"
                   "doc/context/sources/general/manuals/tools/"
                   "doc/context/sources/general/manuals/units/"
                   "doc/context/sources/general/manuals/workflows/"
                   "doc/context/sources/general/manuals/xml/"
                   "doc/context/sources/general/manuals/xtables/"
                   "doc/man/man1/context.1"
                   "doc/man/man1/context.man1.pdf"
                   "doc/man/man1/luatools.1"
                   "doc/man/man1/luatools.man1.pdf"
                   "doc/man/man1/mtx-babel.1"
                   "doc/man/man1/mtx-babel.man1.pdf"
                   "doc/man/man1/mtx-base.1"
                   "doc/man/man1/mtx-base.man1.pdf"
                   "doc/man/man1/mtx-bibtex.1"
                   "doc/man/man1/mtx-bibtex.man1.pdf"
                   "doc/man/man1/mtx-cache.1"
                   "doc/man/man1/mtx-cache.man1.pdf"
                   "doc/man/man1/mtx-chars.1"
                   "doc/man/man1/mtx-chars.man1.pdf"
                   "doc/man/man1/mtx-check.1"
                   "doc/man/man1/mtx-check.man1.pdf"
                   "doc/man/man1/mtx-colors.1"
                   "doc/man/man1/mtx-colors.man1.pdf"
                   "doc/man/man1/mtx-context.1"
                   "doc/man/man1/mtx-context.man1.pdf"
                   "doc/man/man1/mtx-dvi.1"
                   "doc/man/man1/mtx-dvi.man1.pdf"
                   "doc/man/man1/mtx-epub.1"
                   "doc/man/man1/mtx-epub.man1.pdf"
                   "doc/man/man1/mtx-evohome.1"
                   "doc/man/man1/mtx-evohome.man1.pdf"
                   "doc/man/man1/mtx-fcd.1"
                   "doc/man/man1/mtx-fcd.man1.pdf"
                   "doc/man/man1/mtx-flac.1"
                   "doc/man/man1/mtx-flac.man1.pdf"
                   "doc/man/man1/mtx-fonts.1"
                   "doc/man/man1/mtx-fonts.man1.pdf"
                   "doc/man/man1/mtx-grep.1"
                   "doc/man/man1/mtx-grep.man1.pdf"
                   "doc/man/man1/mtx-interface.1"
                   "doc/man/man1/mtx-interface.man1.pdf"
                   "doc/man/man1/mtx-metapost.1"
                   "doc/man/man1/mtx-metapost.man1.pdf"
                   "doc/man/man1/mtx-modules.1"
                   "doc/man/man1/mtx-modules.man1.pdf"
                   "doc/man/man1/mtx-package.1"
                   "doc/man/man1/mtx-package.man1.pdf"
                   "doc/man/man1/mtx-patterns.1"
                   "doc/man/man1/mtx-patterns.man1.pdf"
                   "doc/man/man1/mtx-pdf.1"
                   "doc/man/man1/mtx-pdf.man1.pdf"
                   "doc/man/man1/mtx-plain.1"
                   "doc/man/man1/mtx-plain.man1.pdf"
                   "doc/man/man1/mtx-profile.1"
                   "doc/man/man1/mtx-profile.man1.pdf"
                   "doc/man/man1/mtx-rsync.1"
                   "doc/man/man1/mtx-rsync.man1.pdf"
                   "doc/man/man1/mtx-scite.1"
                   "doc/man/man1/mtx-scite.man1.pdf"
                   "doc/man/man1/mtx-server.1"
                   "doc/man/man1/mtx-server.man1.pdf"
                   "doc/man/man1/mtx-spell.1"
                   "doc/man/man1/mtx-spell.man1.pdf"
                   "doc/man/man1/mtx-texworks.1"
                   "doc/man/man1/mtx-texworks.man1.pdf"
                   "doc/man/man1/mtx-timing.1"
                   "doc/man/man1/mtx-timing.man1.pdf"
                   "doc/man/man1/mtx-tools.1"
                   "doc/man/man1/mtx-tools.man1.pdf"
                   "doc/man/man1/mtx-unicode.1"
                   "doc/man/man1/mtx-unicode.man1.pdf"
                   "doc/man/man1/mtx-unzip.1"
                   "doc/man/man1/mtx-unzip.man1.pdf"
                   "doc/man/man1/mtx-update.1"
                   "doc/man/man1/mtx-update.man1.pdf"
                   "doc/man/man1/mtx-vscode.1"
                   "doc/man/man1/mtx-vscode.man1.pdf"
                   "doc/man/man1/mtx-watch.1"
                   "doc/man/man1/mtx-watch.man1.pdf"
                   "doc/man/man1/mtx-youless.1"
                   "doc/man/man1/mtx-youless.man1.pdf"
                   "doc/man/man1/mtxrun.1"
                   "doc/man/man1/mtxrun.man1.pdf"
                   "fonts/afm/hoekwater/context/"
                   "fonts/cid/fontforge/"
                   "fonts/map/dvips/context/"
                   "fonts/map/luatex/context/"
                   "fonts/map/pdftex/context/"
                   "fonts/misc/xetex/fontmapping/context/"
                   "fonts/tfm/hoekwater/context/"
                   "fonts/type1/hoekwater/context/"
                   "metapost/context/base/common/"
                   "metapost/context/base/mpiv/"
                   "metapost/context/base/mpxl/"
                   "metapost/context/fonts/mpiv/"
                   "scripts/context/lua/"
                   "scripts/context/perl/"
                   "tex/context/base/"
                   "tex/context/bib/common/"
                   "tex/context/colors/icc/context/"
                   "tex/context/colors/icc/profiles/"
                   "tex/context/fonts/mkiv/"
                   "tex/context/fonts/mkxl/"
                   "tex/context/interface/mkiv/"
                   "tex/context/modules/common/"
                   "tex/context/modules/mkiv/"
                   "tex/context/modules/mkxl/"
                   "tex/context/patterns/common/"
                   "tex/context/patterns/mkiv/"
                   "tex/context/patterns/mkxl/"
                   "tex/context/sample/common/"
                   "tex/context/sample/third/"
                   "tex/context/test/mkiv/"
                   "tex/generic/context/luatex/"
                   "tex/latex/context/ppchtex/")
             (base32
              "1mmdxi6hcznmj2fjx6dmf76q3zsyhq67irvyr8a125d9qcs1iiml")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:link-scripts #~(list "context.lua" "mtxrun.lua")))
    (propagated-inputs
     (list texlive-amsfonts
           texlive-lm
           texlive-lm-math
           texlive-luatex
           texlive-manfnt-font
           texlive-mflogo-font
           texlive-stmaryrd))
    (home-page "https://ctan.org/pkg/context")
    (synopsis "Full featured, parameter driven macro package for TeX")
    (description "ConTeXt is a full featured, parameter driven macro package,
which fully supports advanced interactive documents.  See the ConTeXt garden
for a wealth of support information.")
    ;; The GPL applies to all code; alternatively, the LaTeX license may be
    ;; used.  The CC-BY-SA license applies to all documentation.
    (license (list license:lppl1.3c+
                   license:gpl2+
                   license:cc-by-sa4.0))))

(define-deprecated-package texlive-context-base texlive-context)

(define-public texlive-context-account
  (package
    (name "texlive-context-account")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/account/"
                   "tex/context/interface/third/"
                   "tex/context/third/account/")
             (base32
              "0pcl7yiajm4q87f05kd6wyxhvdzvsy4hyvy24aihzlmhwrww0c9c")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-account")
    (synopsis "Simple accounting package")
    (description
     "The package deals with accounts of its own specification.")
    (license license:public-domain)))

(define-public texlive-context-algorithmic
  (package
    (name "texlive-context-algorithmic")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/algorithmic/"
                   "tex/context/third/algorithmic/")
             (base32
              "0l2k8a3g3yrirbiwp741h1fisbprhbl2kwzbmd71fij1a80sqfaw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-algorithmic")
    (synopsis "Algorithm handling in ConTeXt")
    (description
     "This module provides support for typesetting algorithms.")
    (license license:gpl3+)))

(define-public texlive-context-animation
  (package
    (name "texlive-context-animation")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/animation/"
                   "tex/context/interface/third/"
                   "tex/context/third/animation/")
             (base32
              "156zpxjb4c7qaibn7wjw13ysqyqg3m7pb8x3r3fzs6m83gp6r46c")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-animation")
    (synopsis "Generate fieldstack based animation with ConTeXt")
    (description
     "The package is a port, to Context (mkvi), of the corresponding LaTeX
package.")
    (license license:gpl3)))

(define-public texlive-context-annotation
  (package
    (name "texlive-context-annotation")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/annotation/"
                   "tex/context/interface/third/"
                   "tex/context/third/annotation/")
             (base32
              "1b908rj51dbpicdiwwycb36wscxl1asmsmccrzcfbdji189kf6ck")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-annotation")
    (synopsis "Annotate text blocks")
    (description
     "The @code{annotation} module lets you create your own commands and
environments to mark text blocks.")
    (license license:gpl3+)))

(define-public texlive-context-bnf
  (package
    (name "texlive-context-bnf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/bnf/"
                   "tex/context/third/bnf/")
             (base32
              "0m7144pgwk1707g7na96dx4apl6il73zzcvq7qd6lmhij3m96nv8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-bnf")
    (synopsis "BNF module for ConTeXt")
    (description
     "The module provides a simple way to write good-looking BNF-style
grammars in ConTeXt.  Grammars are written using the BNF syntax right in your
ConTeXt documents, so there is a clear separation between content and layout.
This allows the user to decide exactly how the grammar is to be displayed,
while also allowing the gist of the grammar to be understood from simply
looking at the source ConTeXt document.")
    (license license:gpl3+)))

(define-public texlive-context-chromato
  (package
    (name "texlive-context-chromato")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/chromato/"
                   "tex/context/third/chromato/")
             (base32
              "0p32iqgd6cbjx5y9ahs59f1q05js2s66nr84bjhzd37vzzl9acxs")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-chromato")
    (synopsis "ConTeXt macros for chromatograms")
    (description "The module provides macros for drawing chromatograms.")
    (license license:gpl3+)))

(define-public texlive-context-cmscbf
  (package
    (name "texlive-context-cmscbf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/cmscbf/"
                   "tex/context/third/cmscbf/")
             (base32
              "0p0xgqr2pqxmhq7mzrzfyn8c37vr1xm273jz99lxzqpc7pxs37cp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-cmscbf")
    (synopsis "Use Computer Modern bold Caps and Small-caps in ConTeXt")
    (description
     "The module makes provision for bold caps and small caps CM fonts,
in ConTeXt.  Such a font may be found in the Computer Modern extra bold font
set.")
    (license license:gpl3+)))

(define-public texlive-context-cmttbf
  (package
    (name "texlive-context-cmttbf")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/cmttbf/"
                   "tex/context/third/cmttbf/")
             (base32
              "0c88d2h6sn2l8s1lvy18h8vd6hc7im5gd85ilway4gq2nhvl4ahw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-cmttbf")
    (synopsis "Use Computer Modern Typewriter bold font in ConTeXt")
    (description
     "The module makes provision for bold typewriter CM fonts, in ConTeXt.
Such a font may be found in the Computer Modern extra bold font set.")
    (license license:gpl3+)))

(define-public texlive-context-construction-plan
  (package
    (name "texlive-context-construction-plan")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/construction-plan/"
                   "tex/context/third/construction-plan/")
             (base32
              "0nch7bwmal1lbsv4h5jwpavif5cx7096jr7m1g2pfjn4qy9slpgs")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-construction-plan")
    (synopsis "Construction plans in ConTeXt")
    (description
     "This module generates a page with a figure at a well-defined scale.")
    (license license:gpl3+)))

(define-public texlive-context-cyrillicnumbers
  (package
    (name "texlive-context-cyrillicnumbers")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/cyrillicnumbers/"
                   "tex/context/interface/third/"
                   "tex/context/third/cyrillicnumbers/")
             (base32
              "0vg3czh7vhwr9pg8iqmry4fd6kkmimpvsf0sdnvf73p7jkn5478y")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-cyrillicnumbers")
    (synopsis "Write numbers as cyrillic glyphs")
    (description
     "The package extends ConTeXt's system of number conversion, by adding
numeration using cyrillic letters.")
    (license license:bsd-2)))

(define-public texlive-context-degrade
  (package
    (name "texlive-context-degrade")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/degrade/"
                   "tex/context/third/degrade/")
             (base32
              "10bny5ipa8sn6wk1k6238f5q7mazlyz9hgfi2d7hspghnfg3sm3g")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-degrade")
    (synopsis "Degrading JPEG images in ConTeXt")
    (description
     "This module provides tools to degrade JPEG images in ConTeXt.")
    (license license:gpl3+)))

(define-public texlive-context-fancybreak
  (package
    (name "texlive-context-fancybreak")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/fancybreak/"
                   "tex/context/interface/third/"
                   "tex/context/third/fancybreak/")
             (base32
              "0v29i2c77ycvl412mwmlyijss4s31c0sbxz7m7ln9vqy0xh3hv5i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-fancybreak")
    (synopsis "Overfull pages with ConTeXt")
    (description
     "The ConTeXt module allows insertion of thought breaks in texts.
With parameters one can adjust the spacing around the content and set
a default symbol.")
    (license license:gpl3+)))

(define-public texlive-context-filter
  (package
    (name "texlive-context-filter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/filter/"
                   "tex/context/third/filter/")
             (base32
              "03nv2zd3aiv9lkq0bwq6awibn1acjprkc6b6jr5jslcyy4sw3k7w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-filter")
    (synopsis
     "Run external programs on the contents of a start-stop environment")
    (description
     "The @code{filter} module provides a simple interface to run external
programs on the contents of a start-stop environment.  Options are available
to run the external program only if the content of the environment has
changed, to specify how the program output should be read back, and to choose
the name of the temporary files that are created.  The module is compatible
with both MkII and MkIV.")
    (license license:bsd-2)))

(define-public texlive-context-french
  (package
    (name "texlive-context-french")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/french/"
                   "tex/context/third/french/")
             (base32
              "06x7dfsc1vrb49zl9xlhlyfavijgpc53hc0yhj5yc6vph8zbq0df")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-french")
    (synopsis "Support for writing French in ConTeXt")
    (description
     "This ConTeXt module deals with spacing around French punctuation.")
    (license license:gpl3+)))

(define-public texlive-context-fullpage
  (package
    (name "texlive-context-fullpage")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/fullpage/"
                   "tex/context/interface/third/"
                   "tex/context/third/fullpage/")
             (base32
              "1phxzaayy8dpxhfg7xj66ms3xgrdfdc3hv18wy92havcky5gq9wc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-fullpage")
    (synopsis "Overfull pages with ConTeXt")
    (description
     "This ConTeXt module copies the functionality of @code{fullpage}, and
adds a styling parameter, given in the @code{\\usemodule} command.")
    (license license:gpl3+)))

(define-public texlive-context-gantt
  (package
    (name "texlive-context-gantt")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/gantt/"
                   "tex/context/third/gantt/")
             (base32
              "0r6gwxqg4pvrgz5z1bsdknahdqhjfjdkdnl148awsg49apqrrbwi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context texlive-hatching))
    (home-page "https://ctan.org/pkg/context-gantt")
    (synopsis "GANTT module for ConTeXt")
    (description
     "Gantt is a module for drawing Gantt charts via MetaPost or PGF/TikZ.")
    (license license:public-domain)))

(define-public texlive-context-gnuplot
  (package
    (name "texlive-context-gnuplot")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/gnuplot/"
                   "metapost/context/third/gnuplot/"
                   "tex/context/third/gnuplot/")
             (base32
              "0bmzww4sq8n37aipy6cvplcblxwlbmpl0by2sqysc3pdyxnixg41")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-gnuplot")
    (synopsis "Inclusion of Gnuplot graphs in ConTeXt")
    (description
     "This ConTeXt module enables simple creation and inclusion of graphs with
Gnuplot.  It writes a script into temporary file, runs Gnuplot and includes
the resulting graphic directly into the document.")
    (license license:gpl3+)))

(define-public texlive-context-handlecsv
  (package
    (name "texlive-context-handlecsv")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/handlecsv/"
                   "tex/context/third/handlecsv/")
             (base32
              "0g5lxyj7v64mvfdhw96sqwjgxcka1nh6glbv040rrkx1p3j2r55x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-handlecsv")
    (synopsis "Data merging for automatic document creation")
    (description
     "The package handles CSV data merging for automatic document creation.")
    (license license:gpl3)))

(define-public texlive-context-layout
  (package
    (name "texlive-context-layout")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/layout/"
                   "tex/context/third/layout/")
             (base32
              "1zl26r94yizzpxzilkwv90xbr61ddxm7fabwpgbkrci0idc9ksv0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-layout")
    (synopsis "Show ConTeXt layouts")
    (description
     "This ConTeXt module draws a representation of the layout of the current
page and displays the sizes of the widths and heights of the margins, header,
footer and text body.")
    (license license:gpl3+)))

(define-public texlive-context-letter
  (package
    (name "texlive-context-letter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/letter/"
                   "tex/context/interface/third/"
                   "tex/context/third/letter/base/"
                   "tex/context/third/letter/style/")
             (base32
              "1bygisva06mj7kqcwdyk9mp247dv8v0qxgh2g1a451q8sqnk52yl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-letter")
    (synopsis "ConTeXt package for writing letters")
    (description
     "This package provides a means of writing vanilla letters and memos is
provided, with support covering ConTeXt Mkii and Mkiv.  The design of
letters may be amended by a wide range of style specifications.")
    (license license:gpl3+)))

(define-public texlive-context-lettrine
  (package
    (name "texlive-context-lettrine")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/lettrine/"
                   "tex/context/interface/third/"
                   "tex/context/third/lettrine/")
             (base32
              "0mhzvnn6ffysfq9qxdj1a6prplppzsh4pb7x2di2r87pnkxwa7f2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-lettrine")
    (synopsis "ConTeXt implementation of lettrines")
    (description
     "This is a re-implementation of the LaTeX package @code{lettrine}.")
    (license license:public-domain)))

(define-public texlive-context-mathsets
  (package
    (name "texlive-context-mathsets")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/mathsets/"
                   "tex/context/interface/third/"
                   "tex/context/third/mathsets/")
             (base32
              "15azsj3fv57yz9q1rrdp0cmjpz9p83aq5mwxkanppiy9sw1dny3b")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-mathsets")
    (synopsis "Set notation in ConTeXt")
    (description
     "Typeset good-looking set notation as well as similar things such as
Dirac braket notation, conditional probabilities, etc.  The package is at
least inspired by @code{braket}.")
    (license license:bsd-2)))           ;from "t-mathsets.tex"

(define-public texlive-context-rst
  (package
    (name "texlive-context-rst")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/rst/"
                   "scripts/context/lua/third/rst/"
                   "tex/context/interface/third/"
                   "tex/context/third/rst/")
             (base32
              "0dfviz3pww0k9ffhiqw0dywj9fs3wkmqavb98z2cq1ldw09fbh01")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-rst")
    (synopsis "Process reStructuredText with ConTeXt")
    (description
     "The package provides a converter and module for typesetting
reStructuredText with ConTeXt.")
    (license license:bsd-2)))           ;from "manual.pdf"

(define-public texlive-context-ruby
  (package
    (name "texlive-context-ruby")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/ruby/"
                   "tex/context/third/ruby/")
             (base32
              "0dgyvsxxfll5zayp0bfb7ixzh7df4bm3yz7z2w974yjlrd17acm0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-ruby")
    (synopsis "Ruby annotations in ConTeXt")
    (description
     "Ruby markup (aka furigana in Japan) are inline annotations above or
below a word to indicate the reading of ideographic characters.  The module
implements the W3C specification for simple Ruby in ConTeXt.  The position and
layout of the base text and the Ruby text can be controlled by parameters.")
    (license license:public-domain)))

(define-public texlive-context-simplefonts
  (package
    (name "texlive-context-simplefonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/simplefonts/"
                   "tex/context/third/simplefonts/")
             (base32
              "1yxxgxnxhg006pq8incc6s6rj7jvvl1mkrdagpvc63lc709ih56s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-simplefonts")
    (synopsis "Simplified font usage for ConTeXt")
    (description
     "The package defines a set of commands for loading and using fonts in
ConTeXt.")
    (license license:gpl3+)))

(define-public texlive-context-simpleslides
  (package
    (name "texlive-context-simpleslides")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/simpleslides/"
                   "scripts/context/lua/third/simpleslides/"
                   "tex/context/interface/third/"
                   "tex/context/third/simpleslides/")
             (base32
              "0vbh26ym42ayxwis6wbkmf07sy167g9kzg5ls889j2f2zn5rf4pr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-simpleslides")
    (synopsis "Module for preparing presentations")
    (description
     "This ConTeXt module provides an easy-to-use interface for creating
presentations for use with a digital projector.  The presentations are
not interactive (no buttons, hyperlinks or navigational tools such as
tables of contents).  Graphics may be mixed with the text of slides.
The module provides several predefined styles, designed for academic
presentation.  Most styles are configurable, and it is easy to design
new styles.")
    (license license:gpl3+)))

(define-public texlive-context-title
  (package
    (name "texlive-context-title")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/title/"
                   "tex/context/interface/third/"
                   "tex/context/third/title/")
             (base32
              "1ah7b5lgqq668s17d5glnl2bwyzh7idsdib4ijxarx7ahph06jx2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-title")
    (synopsis "Place document titles")
    (description
     "The title module provides the @code{\\placetitle} command to put
a title block into your document.  With the command
@code{\\setuptitle} values can be set for @code{\\placetitle} and
change the formatting of the content.")
    (license license:gpl3+)))

(define-public texlive-context-transliterator
  (package
    (name "texlive-context-transliterator")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/transliterator/"
                   "scripts/context/lua/third/transliterator/"
                   "tex/context/interface/third/"
                   "tex/context/third/transliterator/")
             (base32
              "0hrj6sjldi7chqdnf300bs1q9s92v9sl2mfx3h0644mzjgjxw7s1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-transliterator")
    (synopsis "Transliterate text from other alphabets")
    (description
     "The package will read text in one alphabet, and provide a transliterated
version in another; this is useful for readers who cannot read the original
alphabet.  The package can make allowance for hyphenation.")
    (license license:bsd-2)))           ;from "t-transliterator.mkiv"

(define-public texlive-context-typearea
  (package
    (name "texlive-context-typearea")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/typearea/"
                   "tex/context/third/typearea/")
             (base32
              "0np4yn0kjq1m7rrhcfxrai18qk19051188sfn9lrvphj604s7sna")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-typearea")
    (synopsis "Something like Koma-Script typearea")
    (description
     "The module provides a command that calculates the page layout as the
LaTeX package @code{typearea} does.")
    (license license:gpl3+)))

(define-public texlive-context-typescripts
  (package
    (name "texlive-context-typescripts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/typescripts/"
                   "tex/context/third/typescripts/")
             (base32
              "0kz3flqyzg2p7lvr7yy1h1nmk368glzxxhwirizn5g4xv5sdf27b")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-typescripts")
    (synopsis "Small modules to load various fonts for use in ConTeXt")
    (description
     "The package provides files offering interfaces to 33 publicly available
fonts (or collections of fonts from the same foundry); each is available in
a @file{.mkii} and a @file{.mkiv} version.")
    (license license:gpl2)))

(define-public texlive-context-vim
  (package
    (name "texlive-context-vim")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/vim/"
                   "tex/context/third/vim/")
             (base32
              "03cwdqkrx6bgcikmyxrwkyac3jmz7i50cavgb6r8b26zrsm522ca")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context texlive-context-filter))
    (home-page "https://ctan.org/pkg/context-vim")
    (synopsis "Generate ConTeXt syntax highlighting code from Vim")
    (description
     "ConTeXt has excellent pretty printing capabilities for many languages.
The code for pretty printing is written in TeX, and due to catcode juggling,
such verbatim typesetting is perhaps the trickiest part of TeX.  This makes it
difficult for a normal user to define syntax highlighting rules for a new
language.  This module takes the onus of defining syntax highlighting rules
away from the user and uses Vim editor to generate the syntax highlighting.
There is a helper @file{2context.vim} script to do the syntax parsing in
Vim.")
    (license license:bsd-2)))

(define-public texlive-context-visualcounter
  (package
    (name "texlive-context-visualcounter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/context/third/visualcounter/"
                   "source/context/third/visualcounter/"
                   "tex/context/third/visualcounter/")
             (base32
              "0rq2zqrvbidwngc4jyv4ay84y5l854z1shk08cjlvnlbsgrg7lmk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-context))
    (home-page "https://ctan.org/pkg/context-visualcounter")
    (synopsis "Visual display of ConTeXt counters")
    (description
     "This package provides a typical document usually contains many counters:
page numbers, section numbers, itemizations, enumerations, theorems, and so
on.  This module provides a visual display for such counters.")
    (license license:bsd-2)))

(define-public texlive-beamer
  (package
    (name "texlive-beamer")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/beamer/" "tex/latex/beamer/")
             (base32
              "0v5ix5dybf6j2mj9sp5598vdbm4bm1m50nmhj6qsk8faj78g562w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amscls
           texlive-amsfonts
           texlive-amsmath
           texlive-atbegshi
           texlive-etoolbox
           texlive-geometry
           texlive-hyperref
           texlive-iftex
           texlive-pgf
           texlive-translator
           texlive-xcolor))
    (home-page "https://www.ctan.org/pkg/beamer")
    (synopsis "LaTeX class for producing presentations and slides")
    (description
     "The @code{beamer} LaTeX class can be used for producing slides.
The class works in both PostScript and direct PDF output modes, using the
@code{pgf} graphics system for visual effects.  Content is created in the
@code{frame} environment, and each frame can be made up of a number of slides
using a simple notation for specifying material to appear on each slide within
a frame.  Short versions of title, authors, institute can also be specified as
optional parameters.  Whole frame graphics are supported by plain frames.  The
class supports @code{figure} and @code{table} environments, transparency
effects, varying slide transitions and animations.")
    ;; Code is dual licensed under GPLv2+ or LPPL1.3c+; documentation is
    ;; dual-licensed under either FDLv1.3+ or LPPL1.3c+.
    (license (list license:lppl1.3c+ license:gpl2+ license:fdl1.3+))))

(define-deprecated-package texlive-latex-beamer texlive-beamer)

(define-public texlive-xmpincl
  (package
    (name "texlive-xmpincl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xmpincl/" "source/latex/xmpincl/"
                   "tex/latex/xmpincl/")
             (base32
              "1wc48qark5hd593jh3mx1yryxsdcq5hbaxyrhwcaxzgqivdli34p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xmpincl")
    (synopsis "Include eXtensible Metadata Platform data in pdfLaTeX")
    (description
     "The XMP (eXtensible Metadata platform) is a framework to add metadata to
digital material to enhance the workflow in publication.  The essence is that
the metadata is stored in an XML file, and this XML stream is then embedded in
the file to which it applies.")
    (license license:gpl3+)))

(define-deprecated-package texlive-latex-xmpincl texlive-xmpincl)

(define-public texlive-pdfx
  (package
    (name "texlive-pdfx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pdfx/" "source/latex/pdfx/"
                   "tex/latex/pdfx/")
             (base32
              "1z4j4d92k2fjmf8jfap4zn7ij97d9rz2jcs9aslcac07ag4x5bdp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-encoding
            (lambda _
              (substitute* "source/latex/pdfx/pdfx.dtx"
                (("    .+umaczy") "umaczy")))))))
    (home-page "https://ctan.org/pkg/pdfx")
    (synopsis "PDF/X and PDF/A support for pdfTeX, LuaTeX and XeTeX")
    (description
     "The package helps LaTeX users to create PDF/X, PFD/A and other
standards-compliant PDF documents with pdfTeX, LuaTeX and XeTeX.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-pdfx texlive-pdfx)

(define-public texlive-yax
  (package
    (name "texlive-yax")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/yax/" "tex/generic/yax/")
             (base32
              "01hv550qkmxw63m41v4qykfiracvzvjwxk49y6fc6abg89hfvsj6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/yax")
    (synopsis "Yet Another Key System")
    (description
     "YaX is advertised as a key system, but it rather organizes attributes in
parameters, which parameters can be executed, so that YaX is halfway between
key management and macro definition (and actually hopes to provide a user's
interface).  Values assigned to attributes can be retrieved and tested in
various ways, with full expandability ensured as much as possible.  Finally,
YaX's syntax is a quite peculiar (as few braces as possible), but may be
customized.")
    (license license:lppl)))

(define-public texlive-ydoc
  (package
    (name "texlive-ydoc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ydoc/" "source/latex/ydoc/"
                   "tex/generic/ydoc/" "tex/latex/ydoc/")
             (base32
              "00v7vlv7z2xy4sy2zd4arlndjqvgjsqar3i22vdnld4flb03jqb8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox
           texlive-float
           texlive-hyperref
           texlive-listings
           texlive-needspace
           texlive-newverbs
           texlive-showexpl
           texlive-tools
           texlive-url
           texlive-xcolor))
    (home-page "https://ctan.org/pkg/ydoc")
    (synopsis "Macros for documentation of LaTeX classes and packages")
    (description "The package provides macros and environments to document
LaTeX packages and classes.  It is an (as yet unfinished) alternative to the
@code{ltxdoc} class and the @code{doc} or @code{xdoc} packages.  The aim is to
provide a different layout and more modern styles (using the @code{xcolor},
@code{hyperref} packages, etc.)  This is an alpha release, and should probably
not (yet) be used with other packages, since the implementation might
change.")
    (license license:lppl1.3+)))

(define-public texlive-pstricks
  (package
    (name "texlive-pstricks")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/pstricks/" "dvips/pstricks/"
                   "tex/generic/pstricks/" "tex/latex/pstricks/")
             (base32
              "0hyd8rx0a11mwd13fa10s3h3jq3xymff57p7ks6cnryy2860aizq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "http://www.ctan.org/pkg/pstricks-base")
    (synopsis "PostScript macros for TeX")
    (description
     "PSTricks offers an extensive collection of macros for generating
PostScript that is usable with most TeX macro formats, including Plain TeX,
LaTeX, AMS-TeX, and AMS-LaTeX.  Included are macros for colour, graphics, pie
charts, rotation, trees and overlays.  It has many special features, including
a wide variety of graphics (picture drawing) macros, with a flexible interface
and with colour support.  There are macros for colouring or shading the cells
of tables.")
    (license license:lppl1.3+)))

(define-public texlive-pst-text
  (package
    (name "texlive-pst-text")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/pst-text/"
                   "dvips/pst-text/"
                   "tex/generic/pst-text/"
                   "tex/latex/pst-text/")
             (base32
              "146fpzd1xlqi94q5r48z8ni8qww713yh6nwkbr9pw27mjrqdadb9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "http://www.ctan.org/pkg/pst-text")
    (synopsis "Text and character manipulation in PSTricks")
    (description "Pst-text is a PSTricks based package for plotting text along
a different path and manipulating characters.  It includes the functionality
of the old package @code{pst-char}.")
    (license license:lppl)))

(define-public texlive-marginnote
  (package
    (name "texlive-marginnote")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/marginnote/"
                   "source/latex/marginnote/"
                   "tex/latex/marginnote/")
             (base32
              "1drmscvd14akcv7n6wl3d3cnj18cwkd7z7rm2gg7z43b89s7kdqr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/marginnote")
    (synopsis "Notes in the margin")
    (description "This package provides the command @code{\\marginnote} that
may be used instead of @code{\\marginpar} at almost every place where
@code{\\marginpar} cannot be used, e.g., inside floats, footnotes, or in
frames made with the @code{framed} package.")
    (license license:lppl1.3c+)))

(define-public texlive-iftex
  (package
    (name "texlive-iftex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/iftex/" "tex/generic/iftex/")
             (base32
              "05p8iw8c8vjs59zb8pgilwpvlzrlb8zxyf34fhyr67y6bwm8phnf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "http://www.ctan.org/pkg/iftex")
    (synopsis "Determine the currently used TeX engine")
    (description
     "This package, which works both for Plain TeX and for LaTeX, defines the
@code{\\ifPDFTeX}, @code{\\ifXeTeX}, and @code{\\ifLuaTeX} conditionals for
testing which engine is being used for typesetting.  The package also provides
the @code{\\RequirePDFTeX}, @code{\\RequireXeTeX}, and @code{\\RequireLuaTeX}
commands which throw an error if pdfTeX, XeTeX or LuaTeX (respectively) is not
the engine in use.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-iftex texlive-iftex)

(define-deprecated-package texlive-generic-ifxetex texlive-iftex)

(define-public texlive-tabu
  (package
    (name "texlive-tabu")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/tabu/" "source/latex/tabu/"
                   "tex/latex/tabu/")
             (base32
              "0mixyrqavipq4ni38z42x3579cdjbz54cp2qqb4q4yhfbl0a4pka")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-varwidth))
    (home-page "https://ctan.org/macros/latex/contrib/tabu")
    (synopsis "Flexible LaTeX tabulars")
    (description
     "The package provides an environment, @code{tabu}, which will make any
sort of tabular, and an environment @code{longtabu} which provides the
facilities of @code{tabu} in a modified @code{longtable} environment.")
    (license license:lppl1.3+)))

(define-public texlive-xkeyval
  (package
    (name "texlive-xkeyval")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xkeyval/" "source/latex/xkeyval/"
                   "tex/generic/xkeyval/" "tex/latex/xkeyval/")
             (base32
              "0nclsazny3hnzsi2vcixh2g1gsj5lvwxls1v569rms8ykgd9v7z8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:build-targets #~(list "xkeyval.dtx")
      #:tex-format "latex"              ;won't build with luatex
      #:phases
      #~(modify-phases %standard-phases
          ;; This package cannot be built out of tree as it expects to find
          ;; built files in the working directory.
          (add-before 'build 'fix-build
            (lambda _
              (setenv "TEXINPUTS" (string-append (getcwd) "/build:"))
              (substitute* "source/latex/xkeyval/xkeyval.dtx"
                (("usepackage\\{xcolor\\}")
                 "usepackage[dvips]{xcolor}")))))))
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-ec
                  texlive-footmisc
                  texlive-fourier
                  texlive-graphics-def
                  texlive-hypdoc
                  texlive-hyperref
                  texlive-iftex
                  texlive-infwarerr
                  texlive-kvoptions
                  texlive-listings
                  texlive-lm
                  texlive-pgf
                  texlive-pst-text
                  texlive-pstricks
                  texlive-url
                  texlive-xcolor))))
    (home-page "https://ctan.org/pkg/xkeyval")
    (synopsis "Extension of the @code{keyval} package")
    (description
     "This package is an extension of the keyval package and offers additional
macros for setting keys and declaring and setting class or package options.
The package allows the programmer to specify a prefix to the name of the
macros it defines for keys, and to define families of key definitions; these
all help use in documents where several packages define their own sets of
keys.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-xkeyval texlive-xkeyval)

(define-public texlive-standalone
  (package
    (name "texlive-standalone")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/standalone/"
                   "source/latex/standalone/"
                   "tex/latex/standalone/")
             (base32
              "055mz0r837ipb6f0v7lp2imwpy1zh0i45wkd5f1dbpjpb9gf7qny")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (propagated-inputs
     (list texlive-adjustbox
           texlive-currfile
           texlive-filemod
           texlive-gincltex
           texlive-xkeyval))
    (home-page "https://ctan.org/pkg/standalone")
    (synopsis "Compile TeX pictures stand-alone or as part of a document")
    (description
     "This package provides a class and package is provided which allows TeX
pictures or other TeX code to be compiled standalone or as part of a main
document.  Special support for pictures with @code{beamer} overlays is also
provided.  The package is used in the main document and skips extra preambles
in sub-files.  The class may be used to simplify the preamble in sub-files.
By default the @code{preview} package is used to display the typeset code
without margins.")
    (license license:lppl1.3+)))

(define-public texlive-siunitx
  (package
    (name "texlive-siunitx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/siunitx/" "source/latex/siunitx/"
                   "tex/latex/siunitx/")
             (base32
              "14rgn7lm5iy15jxcdwrkkp6rmi569x1x7qir82k89xl49k1rr2d1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-l3kernel texlive-l3packages))
    (home-page "https://www.ctan.org/pkg/siunitx")
    (synopsis "Comprehensive SI units package")
    (description
     "Typesetting values with units requires care to ensure that the combined
mathematical meaning of the value plus unit combination is clear.  In
particular, the SI units system lays down a consistent set of units with rules
on how they are to be used.  However, different countries and publishers have
differing conventions on the exact appearance of numbers (and units).  A
number of LaTeX packages have been developed to provide consistent application
of the various rules.  The @code{siunitx} package takes the best from the
existing packages, and adds new features and a consistent interface.  A number
of new ideas have been incorporated, to fill gaps in the existing provision.
The package also provides backward-compatibility with @code{SIunits},
@code{sistyle}, @code{unitsdef} and @code{units}.  The aim is to have one
package to handle all of the possible unit-related needs of LaTeX users.")
    (license license:lppl1.3c)))

(define-public texlive-booktabs
  (package
    (name "texlive-booktabs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/booktabs/" "source/latex/booktabs/"
                   "tex/latex/booktabs/")
             (base32
              "0pv2kv4hgayqfcij2sz1jmk6kbxqccyaksz8xlw5kvqrbag9vxm3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/booktabs")
    (synopsis "Publication quality tables in LaTeX")
    (description
     "This package enhances the quality of tables in LaTeX, providing extra
commands as well as behind-the-scenes optimisation.  Guidelines are given as
to what constitutes a good table in this context.  The package offers
@code{longtable} compatibility.")
    (license license:lppl1.3+)))

(define-public texlive-csquotes
  (package
    (name "texlive-csquotes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/csquotes/" "tex/latex/csquotes/")
             (base32
              "0657rvaciq5h4qp2hg9d2w2i663p5cnxygi6dj9w61463m4nkpy6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-etoolbox))
    (home-page "https://www.ctan.org/pkg/csquotes")
    (synopsis "Context sensitive quotation facilities")
    (description
     "This package provides advanced facilities for inline and display
quotations.  It is designed for a wide range of tasks ranging from the most
simple applications to the more complex demands of formal quotations.  The
facilities include commands, environments, and user-definable smart quotes
which dynamically adjust to their context.  Quotation marks are switched
automatically if quotations are nested and they can be adjusted to the current
language if the babel package is available.  There are additional facilities
designed to cope with the more specific demands of academic writing,
especially in the humanities and the social sciences.  All quote styles as
well as the optional active quotes are freely configurable.")
    (license license:lppl1.3c+)))

(define-public texlive-logreq
  (package
    (name "texlive-logreq")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/logreq/" "tex/latex/logreq/")
             (base32
              "13difccs3cxlkqlnhw286yb0c7mifrxfd402a2x5wwxv0m1kgfqd")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-etoolbox))
    (home-page "https://ctan.org/pkg/logreq")
    (synopsis "Support for automation of the LaTeX workflow")
    (description
     "The package helps to automate a typical LaTeX workflow that involves
running LaTeX several times, running tools such as BibTeX or makeindex, and so
on.  It will log requests like ``please rerun LaTeX'' or ``please run BibTeX
on file X'' to an external XML file which lists all open tasks in
a machine-readable format.  Compiler scripts and integrated LaTeX editing
environments may parse this file to determine the next steps in the
workflow. In sum, the package will do two things: enable package authors to
use LaTeX commands to issue requests, collect all requests from all packages
and write them to an external XML file at the end of the document.")
    (license license:lppl1.3+)))

(define-public texlive-biblatex
  (package
    (name "texlive-biblatex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bib/biblatex/biblatex/"
                   "bibtex/bst/biblatex/"
                   "doc/latex/biblatex/"
                   "tex/latex/biblatex/bbx/"
                   "tex/latex/biblatex/cbx/"
                   "tex/latex/biblatex/lbx/")
             (base32
              "0s4i9ck77cldf37j01pgjm6qznfwkmy0vmrcdichq8bvzx8w89zg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox
           texlive-kvoptions
           texlive-logreq
           texlive-pdftexcmds
           texlive-url))
    (home-page "https://ctan.org/pkg/biblatex")
    (synopsis "Sophisticated Bibliographies in LaTeX")
    (description
     "BibLaTeX is a complete reimplementation of the bibliographic facilities
provided by LaTeX.  Formatting of the bibliography is entirely controlled by
LaTeX macros, and a working knowledge of LaTeX should be sufficient to design
new bibliography and citation styles.  BibLaTeX uses its own data backend
program called @code{biber} to read and process the bibliographic data.  With
@code{biber}, the range of features provided by BibLaTeX includes full Unicode
support, customisable bibliography labels, multiple bibliographies in the same
document, and subdivided bibliographies, such as bibliographies per chapter or
section.")
    (license license:lppl1.3+)))

(define-public texlive-biblatex-apa
  (package
    (name "texlive-biblatex-apa")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/biblatex-apa/"
                   "tex/latex/biblatex-apa/")
             (base32
              "1igzmgzfchn54zkb78fwsdk2lqs2pp0ydzzcmk1cydhmsfrjya4l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/biblatex-apa")
    (synopsis "BibLaTeX citation and reference style for APA")
    (description
     "This is a fairly complete BibLaTeX style (citations and references) for
APA (American Psychological Association) publications.  It implements and
automates most of the guidelines in the APA 7th edition style guide for
citations and references.")
    (license license:lppl1.3c)))

(define-public texlive-todonotes
  (package
    (name "texlive-todonotes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/todonotes/"
                   "source/latex/todonotes/"
                   "tex/latex/todonotes/")
             (base32
              "0gw9ny0s048kq78m2njrv2m6y4z0rck58i9nc892vl93h7gi4p1v")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-pgf texlive-tools texlive-xcolor texlive-xkeyval))
    (home-page "https://ctan.org/pkg/todonotes")
    (synopsis "Marking things to do in a LaTeX document")
    (description
     "The @code{todonotes} package lets the user mark things to do later, in
a simple and visually appealing way.  The package takes several options to
enable customization and finetuning of the visual appearance.")
    (license license:lppl1.3+)))

(define-public texlive-units
  (package
    (name "texlive-units")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/units/" "source/latex/units/"
                   "tex/latex/units/")
             (base32
              "1mrdsg55f40cvarrx84gbhrnsk8mlv915nll17lnfzfapgmvjsbl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/units")
    (synopsis "Typeset physical units and fractions")
    (description "@code{units} is a package for typesetting physical units in
a standard-looking way.  The package is based upon @code{nicefrac}, a package
for typing fractions.  @code{nicefrac} is included in the @code{units}
bundle.")
    (license license:gpl3+)))

(define-public texlive-microtype
  (package
    (name "texlive-microtype")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/microtype/"
                   "source/latex/microtype/"
                   "tex/latex/microtype/")
             (base32
              "039v1dw3n4lnd8ipazlkb7p5abqcrigjayx797ggh3ak8dcqwlli")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/microtype")
    (synopsis "Subliminal refinements towards typographical perfection")
    (description
     "The package provides a LaTeX interface to the micro-typographic
extensions that were introduced by pdfTeX and have since also propagated to
XeTeX and LuaTeX: most prominently, character protrusion and font expansion,
furthermore the adjustment of interword spacing and additional kerning, as
well as hyphenatable letterspacing (tracking) and the possibility to disable
all or selected ligatures.  These features may be applied to customisable sets
of fonts, and all micro-typographic aspects of the fonts can be configured in
a straight-forward and flexible way.  An alternative package
@code{letterspace}, which also works with plain TeX, is included in the
bundle.")
    (license license:lppl1.3c)))

(define-public texlive-minitoc
  (package
    (name "texlive-minitoc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/minitoc/"
                   "tex/latex/minitoc/")
             (base32
              "0yd1na5b9m7z1j87a5fjwjqddfpiblfbpzcv0vlvql6lwh38mii7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/minitoc")
    (synopsis "Produce a table of contents for each chapter, part or section")
    (description
     "The @code{minitoc} package allows you to add
mini-tables-of-contents (minitocs) at the beginning of every chapter, part or
section.  There is also provision for mini-lists of figures and of tables.  At
the part level, they are parttocs, partlofs and partlots.  If the type of
document does not use chapters, the basic provision is section level secttocs,
sectlofs and sectlots.  The package has provision for language-specific
configuration of its own fixed names, using @file{.mld} files.")
    (license license:lppl1.3+)))

(define-public texlive-minted
  (package
    (name "texlive-minted")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/minted/" "source/latex/minted/"
                   "tex/latex/minted/")
             (base32
              "1z2vagia7sbfa134qj3dfdkppy0v4yjykaj594c6z9qy1z5jn5mc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list python-pygments
           texlive-catchfile
           texlive-etoolbox
           texlive-fancyvrb
           texlive-float
           texlive-framed
           texlive-fvextra
           texlive-graphics
           texlive-ifplatform
           texlive-kvoptions
           texlive-lineno
           texlive-pdftexcmds
           texlive-tools
           texlive-upquote
           texlive-xcolor
           texlive-xstring))
    (home-page "https://ctan.org/pkg/minted")
    (synopsis "Highlighted source code for LaTeX")
    (description
     "The package that facilitates expressive syntax highlighting in LaTeX
using the powerful Pygments library.  The package also provides options to
customize the highlighted source code output using @code{fancyvrb}.")
    (license license:lppl1.3+)))

(define-public texlive-caption
  (package
    (name "texlive-caption")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/caption/" "source/latex/caption/"
                   "tex/latex/caption/")
             (base32
              "1hgd52dxm35k63jb2vxrkghlaq41h89bwbqyihaim2h06kmnpb0r")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/caption")
    (synopsis "Customising captions in floating environments")
    (description
     "The @code{caption} package provides many ways to customise the captions
in floating environments like figure and table, and cooperates with many other
packages.  Facilities include rotating captions, sideways captions, continued
captions (for tables or figures that come in several parts).  A list of
compatibility notes, for other packages, is provided in the documentation.
The package also provides the caption outside float facility, in the same way
that simpler packages like capt-of do.")
    (license license:lppl1.3+)))

(define-public texlive-symbol
  (package
    (name "texlive-symbol")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/symbol/"
                   "fonts/afm/adobe/symbol/"
                   "fonts/afm/urw/symbol/"
                   "fonts/map/dvips/symbol/"
                   "fonts/tfm/adobe/symbol/"
                   "fonts/tfm/monotype/symbol/"
                   "fonts/tfm/urw35vf/symbol/"
                   "fonts/type1/urw/symbol/"
                   "tex/latex/symbol/")
             (base32
              "1pdkpr86bhia5hcmf7q3nhvklnsga4mqqrrirgl8a7al7x6q3ivs")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/urw-base35")
    (synopsis "Replacement for Adobe's Symbol font")
    (description
     "This package provides a drop-in replacement for the Symbol font from
Adobe's basic set.")
    (license license:gpl3+)))

(define-public texlive-mathpazo
  (package
    (name "texlive-mathpazo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mathpazo/"
                   "fonts/afm/public/mathpazo/"
                   "fonts/tfm/public/mathpazo/"
                   "fonts/type1/public/mathpazo/"
                   "fonts/vf/public/mathpazo/"
                   "source/latex/mathpazo/")
             (base32
              "0g10rjgg1kb78lgyxmwjrkgpy24yq3v0m47h6zhbc68rrmmawvwp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-fpl texlive-palatino))
    (home-page "https://ctan.org/pkg/mathpazo")
    (synopsis "Fonts to typeset mathematics to match Palatino")
    (description "The Pazo Math fonts are a family of PostScript fonts
suitable for typesetting mathematics in combination with the Palatino family
of text fonts.  The Pazo Math family is made up of five fonts provided in
Adobe Type 1 format.  These contain glyphs that are usually not available in
Palatino and for which Computer Modern looks odd when combined with Palatino.
These glyphs include the uppercase Greek alphabet in upright and slanted
shapes, the lowercase Greek alphabet in slanted shape, several mathematical
glyphs and the uppercase letters commonly used to represent various number
sets.  LaTeX macro support is provided in package @code{psnfss}.")
    (license license:gpl3+)))

(define-public texlive-mathspec
  (package
    (name "texlive-mathspec")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/mathspec/"
                   "tex/xelatex/mathspec/")
             (base32
              "0bfdzlim2kkvfzs8p7brwlc46qy41hvxb72xr53ijg2kplsqmkh8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mathspec")
    (synopsis "Specify arbitrary fonts for mathematics in XeTeX")
    (description
     "The mathspec package provides an interface to typeset mathematics in
XeLaTeX with arbitrary text fonts using @code{fontspec} as a backend.")
    (license license:lppl)))

(define-public texlive-mathtools
  (package
    (name "texlive-mathtools")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mathtools/"
                   "source/latex/mathtools/"
                   "tex/latex/mathtools/")
             (base32
              "11rs5wd1m5f3y309gdlq47vj3xzs8m87qlbd7b6glhq8lx8wfbd6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mathtools")
    (synopsis "Mathematical tools to use with @code{amsmath}")
    (description
     "Mathtools provides a series of packages designed to enhance the
appearance of documents containing a lot of mathematics.  It is based on
@code{amsmath} and fixes various deficiencies of it and standard LaTeX.  It
provides:
@itemize
@item Extensible symbols, such as brackets, arrows, harpoons, etc.;
@item Various symbols such as \\coloneqq (:=);
@item Easy creation of new tag forms;
@item Showing equation numbers only for referenced equations;
@item Extensible arrows, harpoons and hookarrows;
@item Starred versions of the @code{matrix} environments for specifying the
column alignment;
@item More building blocks: multlined, cases-like environments, new gathered
environments;
@item Maths versions of @code{\\makebox}, @code{\\llap}, @code{\\rlap} etc.;
@item Cramped math styles; and more...
@end itemize")
    (license license:lppl1.3c)))

(define-public texlive-memoir
  (package
    (name "texlive-memoir")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/memoir/" "makeindex/memoir/"
                   "source/latex/memoir/" "tex/latex/memoir/")
             (base32
              "10jf0kiwvgrki8az04b57v0ffm6b3jj1rz3q6fgxhnrbsd68iphr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/memoir")
    (synopsis "Typeset fiction, non-fiction and mathematical books")
    (description
     "The @code{memoir} class is for typesetting poetry, fiction,
non-fiction, and mathematical works.  Permissible document base font sizes
range from 9 to 60pt.  There is a range of page-styles and well over a dozen
chapter-styles to choose from, as well as methods for specifying your own
layouts and designs.  The class also provides the functionality of over thirty
of the more popular packages, thus simplifying document sources.")
    (license license:lppl1.3+)))

(define-public texlive-fp
  (package
    (name "texlive-fp")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fp/" "tex/latex/fp/"
                   "tex/plain/fp/")
             (base32
              "1q555fx71cf88sn3npzb0j2i10ak920k0qc9ccdygz99vqg10dad")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fp")
    (synopsis "Fixed point arithmetic")
    (description
     "This package provides an extensive collection of arithmetic operations
for fixed point real numbers of high precision.")
    (license license:lppl)))

(define-public texlive-fpl
  (package
    (name "texlive-fpl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/fpl/" "fonts/afm/public/fpl/"
                   "fonts/type1/public/fpl/" "source/fonts/fpl/")
             (base32
              "02gkl516z9kn8xqs269pdkqn37sxm8ib0pcs43s4rs2rhyyl5z68")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fpl")
    (synopsis "SC and OsF fonts for URW Palladio L")
    (description "The FPL Fonts provide a set of SC/OsF fonts for URW
Palladio L which are compatible with the Palatino SC/OsF fonts from
Adobe.  LaTeX use is enabled by the mathpazo package, which is part of
the @code{psnfss} distribution.")
    ;; Either LPPL version 1.0 or later, or GPL version 2
    (license (list license:lppl1.0+ license:gpl2))))

(define-public texlive-arabxetex
  (package
    (name "texlive-arabxetex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/xelatex/arabxetex/"
                   "fonts/misc/xetex/fontmapping/arabxetex/"
                   "source/xelatex/arabxetex/"
                   "tex/xelatex/arabxetex/")
             (base32
              "097lh7ksw9rg93f1c7a4fqglgfpydf1qp3sbgy9xfgszcdpknmrk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-format "xelatex"
      #:phases
           #~(modify-phases %standard-phases
               ;; Use dummy fonts to avoid pulling in needless dependencies.
               ;; We're only interested in building the package, not the
               ;; documentation.
               (add-after 'unpack 'change-fonts
                 (lambda _
                   (substitute* "source/xelatex/arabxetex/arabxetex.dtx"
                     (("(newfontfamily.*?\\{)[^}]+}" _ prefix)
                      (string-append prefix "FreeSans}"))
                     (("(set(main|mono|sans)font(\\[.*?])?\\{)[^}]+}" _ prefix)
                      (string-append prefix "FreeSans}"))))))))
    (native-inputs
     (list fontconfig
           font-gnu-freefont
           texlive-amsmath
           texlive-amsfonts
           texlive-bidi
           texlive-fancyvrb
           texlive-fontspec
           texlive-hologo
           texlive-hypdoc
           texlive-hyperref
           texlive-infwarerr
           texlive-kvdefinekeys
           texlive-kvoptions
           texlive-kvsetkeys
           texlive-ltxcmds
           texlive-paralist
           texlive-pdftexcmds
           texlive-supertabular
           texlive-tools
           texlive-xetex
           texlive-xkeyval
           texlive-zref))
    (home-page "https://ctan.org/pkg/arabxetex")
    (synopsis "ArabTeX-like interface for XeLaTeX")
    (description
     "ArabXeTeX provides a convenient ArabTeX-like user-interface for
typesetting languages using the Arabic script in XeLaTeX, with flexible access
to font features.  Input in ArabTeX notation can be set in three different
vocalization modes or in roman transliteration.  Direct UTF-8 input is also
supported.")
    (license license:lppl1.3c)))

(define-public texlive-arev
  (package
    (name "texlive-arev")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/arev/"
                   "fonts/afm/public/arev/"
                   "fonts/enc/dvips/arev/"
                   "fonts/map/dvips/arev/"
                   "fonts/tfm/public/arev/"
                   "fonts/type1/public/arev/"
                   "fonts/vf/public/arev/"
                   "source/fonts/arev/"
                   "tex/latex/arev/")
             (base32
              "1a0zw9vc6z0shxvb4kdhfqdhwpzph5hm9v7klpchlisabvk421y1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/arev")
    (synopsis "Fonts and LaTeX support files for Arev Sans")
    (description
     "The @code{arev} package provides type 1 fonts, virtual fonts and LaTeX
packages for using Arev Sans in both text and mathematics.  Arev Sans is
a derivative of Bitstream Vera Sans, adding support for Greek and Cyrillic
characters and a few variant letters appropriate for mathematics.  The font is
primarily used in LaTeX for presentations, particularly when using a computer
projector.  Arev Sans has large x-height, open letters, wide spacing and thick
stems.  The style is very similar to the SliTeX font @code{lcmss} but heavier.
Arev is one of a very small number of sans-font mathematics support
packages.")
    (license (list license:silofl1.1    ;for Arev Sans
                   license:lppl1.3a     ;for TeX support files
                   license:gpl2))))     ;for ams-mdbch.sty

(define-public texlive-awesomebox
  (package
    (name "texlive-awesomebox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/awesomebox/"
                   "tex/latex/awesomebox/")
             (base32
              "0jmxhas12fs30x2csv1rbsjr71a60fv2r3i5q7xd5n9zmmrnh32f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/awesomebox")
    (synopsis "Draw admonition blocks in your documents")
    (description
     "Awesome Box is all about drawing admonition blocks around text to inform
or alert readers about something particular.  The specific aim of this package
is to use FontAwesome icons to ease the illustration of these blocks.")
    (license license:wtfpl2)))

(define-public texlive-mathdesign
  (package
    (name "texlive-mathdesign")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/mathdesign/"
                   "dvips/mathdesign/"
                   "fonts/enc/dvips/mathdesign/"
                   "fonts/map/dvips/mathdesign/"
                   "fonts/tfm/public/mathdesign/mdbch/"
                   "fonts/tfm/public/mathdesign/mdgreek/"
                   "fonts/tfm/public/mathdesign/mdici/"
                   "fonts/tfm/public/mathdesign/mdpgd/"
                   "fonts/tfm/public/mathdesign/mdpus/"
                   "fonts/tfm/public/mathdesign/mdput/"
                   "fonts/tfm/public/mathdesign/mdugm/"
                   "fonts/type1/public/mathdesign/mdbch/"
                   "fonts/type1/public/mathdesign/mdici/"
                   "fonts/type1/public/mathdesign/mdpgd/"
                   "fonts/type1/public/mathdesign/mdpus/"
                   "fonts/type1/public/mathdesign/mdput/"
                   "fonts/type1/public/mathdesign/mdugm/"
                   "fonts/vf/public/mathdesign/mdbch/"
                   "fonts/vf/public/mathdesign/mdgreek/"
                   "fonts/vf/public/mathdesign/mdici/"
                   "fonts/vf/public/mathdesign/mdpgd/"
                   "fonts/vf/public/mathdesign/mdpus/"
                   "fonts/vf/public/mathdesign/mdput/"
                   "fonts/vf/public/mathdesign/mdugm/"
                   "tex/latex/mathdesign/")
             (base32
              "0jcby2sd0l3ank2drxc0qcf5d1cwa8idzh4g91h4nxk8zrzxj8nr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mathdesign")
    (synopsis "Mathematical fonts to fit with particular text fonts")
    (description
     "The Math Design project offers mathematical fonts that match with
existing text fonts.  To date, three free font families are available: Adobe
Utopia, URW Garamond and Bitstream Charter.  Mathdesign covers the whole LaTeX
glyph set including AMS symbols.  Both roman and bold versions of these
symbols can be used.  Moreover, there is a choice between three greek
fonts (two of them created by the Greek Font Society).")
    (license license:gpl3+)))

(define-public texlive-bera
  (package
    (name "texlive-bera")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/bera/"
                   "fonts/afm/public/bera/"
                   "fonts/map/dvips/bera/"
                   "fonts/tfm/public/bera/"
                   "fonts/type1/public/bera/"
                   "fonts/vf/public/bera/"
                   "tex/latex/bera/")
             (base32
              "1pkmhhr6ah44xhipjr7nianv03hr4w4bn45xcvp264yw6ymqzqwr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bera")
    (synopsis "Bera fonts")
    (description "The @code{bera} package contains the Bera Type 1 fonts and
files to use the fonts with LaTeX.  Bera is a set of three font families: Bera
Serif (a slab-serif Roman), Bera Sans (a Frutiger descendant) and Bera
Mono (monospaced/typewriter).  The Bera family is a repackaging, for use with
TeX, of the Bitstream Vera family.")
    (license license:silofl1.1)))

(define-public texlive-fourier
  (package
    (name "texlive-fourier")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/fourier/"
                   "fonts/afm/public/fourier/"
                   "fonts/map/dvips/fourier/"
                   "fonts/opentype/public/fourier/"
                   "fonts/tfm/public/fourier/"
                   "fonts/type1/public/fourier/"
                   "fonts/vf/public/fourier/"
                   "tex/latex/fourier/")
             (base32
              "10nw0s3820mf4nv4b655cfvm8asjb1q71yd21cnm8zjxj0xpcrpx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fourier")
    (synopsis "Using Utopia fonts for LaTeX documents")
    (description
     "Fourier-GUTenberg is a LaTeX typesetting system which uses Adobe Utopia
as its standard base font.  Fourier-GUTenberg provides all complementary
typefaces needed to allow Utopia based TeX typesetting, including an extensive
mathematics set and several other symbols.  The system is absolutely
stand-alone: apart from Utopia and Fourier, no other typefaces are required.
Utopia is a registered trademark of Adobe Systems Incorporated")
    (license license:lppl)))

(define-public texlive-utopia
  (package
    (name "texlive-utopia")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/utopia/" "fonts/afm/adobe/utopia/"
                   "fonts/tfm/adobe/utopia/"
                   "fonts/type1/adobe/utopia/"
                   "fonts/vf/adobe/utopia/")
             (base32
              "113wgkfz4z0ls2grxxfj17l42a1yv9r5ipcd0156xnfsrqvqzxfc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/utopia")
    (synopsis "Adobe Utopia fonts")
    (description
     "The Adobe Standard Encoding set of the Utopia font family, as
contributed by the X Consortium.  The set comprises upright and italic shapes
in medium and bold weights.  Macro support and matching maths fonts are
provided by the @code{fourier} and the @code{mathdesign} font packages.")
    (license (license:fsf-free
              "http://mirrors.ctan.org/fonts/utopia/README"))))

(define-public texlive-fontaxes
  (package
    (name "texlive-fontaxes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fontaxes/" "source/latex/fontaxes/"
                   "tex/latex/fontaxes/")
             (base32
              "1d9ji2qvjf1ky8l6rfqbag2hw61r0hyjxkzsp18s4pckyq4dqwdm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fontaxes")
    (synopsis "Additional font axes for LaTeX")
    (description "The @code{fontaxes} package adds several new font axes on
top of LaTeX's New Font Selection Scheme (NFSS).  In particular, it splits the
shape axis into a primary and a secondary shape axis and it adds three new
axes to deal with the different figure versions offered by many professional
fonts.")
    (license license:lppl1.3+)))

(define-public texlive-preprint
  (package
    (name "texlive-preprint")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/preprint/" "source/latex/preprint/"
                   "tex/latex/preprint/")
             (base32
              "198xwg6mll3yndc1kf79l6zgnq3nsk7fsh3rlj28nipd26ysw6lq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/preprint")
    (synopsis "Bundle of modules for preprints")
    (description "The bundle comprises: @code{authblk}, which permits
footnote style author/affiliation input in the @command{\\author} command,
@code{balance}, to balance the end of @command{\\twocolumn} pages,
@code{figcaps}, to send figure captions, etc., to end document,
@code{fullpage}, to set narrow page margins and set a fixed page style, and
@code{sublabel}, which permits counters to be subnumbered.")
    (license license:lppl1.3+)))

(define-public texlive-mweights
  (package
    (name "texlive-mweights")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mweights/" "tex/latex/mweights/")
             (base32
              "12493g3yz06mhiybnphqbp49fjzy36clzw63b74mkfhsg1pq7h1b")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mweights")
    (synopsis "Support for multiple-weight font packages")
    (description
     "Many font families available for use with LaTeX are available at
multiple weights.  Many Type 1-oriented support packages for such fonts
re-define the standard @code{\\mddefault} or @code{\\bfdefault} macros.  This
can create difficulties if the weight desired for one font family isn't
available for another font family, or if it differs from the weight desired
for another font family.  The package provides a solution to these
difficulties.")
    (license license:lppl)))

(define-public texlive-cabin
  (package
    (name "texlive-cabin")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/cabin/"
                   "fonts/enc/dvips/cabin/"
                   "fonts/map/dvips/cabin/"
                   "fonts/opentype/impallari/cabin/"
                   "fonts/tfm/impallari/cabin/"
                   "fonts/type1/impallari/cabin/"
                   "fonts/vf/impallari/cabin/"
                   "tex/latex/cabin/")
             (base32
              "1l4s50l8rjmfrknffgy1c84dg8m9rg96817rs3b3cqk97c3l25zy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/cabin")
    (synopsis "Humanist Sans Serif font with LaTeX support")
    (description
     "Cabin is a humanist sans with four weights and true italics and small
capitals.  According to the designer, Pablo Impallari, Cabin was inspired by
Edward Johnston's and Eric Gill's typefaces, with a touch of modernism.  Cabin
incorporates modern proportions, optical adjustments, and some elements of the
geometric sans.  @file{cabin.sty} supports use of the font under LaTeX,
pdfLaTeX, XeLaTeX and LuaLaTeX; it uses the @code{mweights}, to manage the
user's view of all those font weights.  An option is provided to enable Cabin
as the default text font.")
    (license (list license:silofl1.1    ;for Cabin
                   license:lppl))))     ;for support files

(define-public texlive-newtx
  (package
    (name "texlive-newtx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/newtx/"
                   "fonts/afm/public/newtx/"
                   "fonts/enc/dvips/newtx/"
                   "fonts/map/dvips/newtx/"
                   "fonts/opentype/public/newtx/"
                   "fonts/tfm/public/newtx/"
                   "fonts/type1/public/newtx/"
                   "fonts/vf/public/newtx/"
                   "tex/latex/newtx/")
             (base32
              "0lbkip5nwrc0sf1alhc8b4dh6ymvn48l5sv71qjzrc1qg2jnw29b")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-kastrup))
    (home-page "https://www.ctan.org/pkg/newtx")
    (synopsis "Repackaging of the TX fonts with improved metrics")
    (description
     "The @code{newtx} bundle splits @file{txfonts.sty} (from the TX fonts
distribution) into two independent packages, @code{newtxtext.sty} and
@code{newtxmath.sty}, each with fixes and enhancements.  @code{newtxmath}'s
metrics have been re-evaluated to provide a less tight appearance and to
provide a @code{libertine} option that substitutes Libertine italic and Greek
letters for the existing math italic and Greek glyphs, making a mathematics
package that matches Libertine text quite well.")
    (license license:lppl1.3)))

(define-public texlive-xcharter
  (package
    (name "texlive-xcharter")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/xcharter/"
                   "fonts/afm/public/xcharter/"
                   "fonts/enc/dvips/xcharter/"
                   "fonts/map/dvips/xcharter/"
                   "fonts/opentype/public/xcharter/"
                   "fonts/tfm/public/xcharter/"
                   "fonts/type1/public/xcharter/"
                   "fonts/vf/public/xcharter/"
                   "tex/latex/xcharter/")
             (base32
              "178mmdr9ji346cnmwas22vhbm38izb1sy5164a5h250kgm287v2c")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xcharter")
    (synopsis "Extension of Bitstream Charter fonts")
    (description "@code{xcharter} repackages Bitstream Charter with an
extended set of features.  The extension provides small caps, oldstyle
figures and superior figures in all four styles, accompanied by LaTeX
font support files.  The fonts themselves are provided in both Adobe
Type 1 and OTF formats, with supporting files as necessary.")
    (license (list (license:fsf-free
                    "http://mirrors.ctan.org/fonts/xcharter/README")
                   license:lppl1.3))))

(define-public texlive-ly1
  (package
    (name "texlive-ly1")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ly1/"
                   "fonts/enc/dvips/ly1/"
                   "fonts/map/dvips/ly1/"
                   "fonts/tfm/adobe/ly1/"
                   "fonts/vf/adobe/ly1/"
                   "tex/latex/ly1/"
                   "tex/plain/ly1/")
             (base32
              "0mwk8bfpvpzbwjw3jd6plw0w7kykpb499fv50a9bqxh0jqcyh0j5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ly1")
    (synopsis "Support for LY1 LaTeX encoding")
    (description
     "The legacy @code{texnansi} (TeX and ANSI) encoding is known in the LaTeX
scheme of things as @emph{LY1} encoding.  The @code{ly1} bundle includes
metrics and LaTeX macros to use the three basic Adobe Type 1 fonts (Times,
Helvetica and Courier) in LaTeX using LY1 encoding.")
    (license license:lppl1.0+)))

(define-public texlive-section
  (package
    (name "texlive-section")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/section/" "tex/latex/section/")
             (base32
              "0gjajhlwwgyqnmw9bzr0l7dsq12mdhgv8sdkm86m4zn107qab0p9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/section")
    (synopsis "Modifying section commands in LaTeX")
    (description
     "The package implements a pretty extensive scheme to make more manageable
the business of configuring LaTeX output.")
    (license license:lppl)))

(define-public texlive-sectsty
  (package
    (name "texlive-sectsty")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sectsty/" "source/latex/sectsty/"
                   "tex/latex/sectsty/")
             (base32
              "1h7zi622s84vqjl6bi4g6iv639jz9m2imz4g7y1qpc0zdw1mlqv4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://www.ctan.org/pkg/sectsty")
    (synopsis "Control sectional headers")
    (description
     "This is a LaTeX2ε package to help change the style of any or all of
LaTeX's sectional headers in the article, book, or report classes.  Examples
include the addition of rules above or below a section title.")
    (license license:lppl1.2+)))

(define-public texlive-morefloats
  (package
    (name "texlive-morefloats")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/morefloats/"
                   "source/latex/morefloats/"
                   "tex/latex/morefloats/")
             (base32
              "0y8rd3ys71ys9cab172wwhrmbs9b52wqrj6d3p0iy3075z93h51c")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/morefloats")
    (synopsis "Increase the number of simultaneous LaTeX floats")
    (description "LaTeX can, by default, only cope with 18 outstanding floats;
any more, and you get the error “too many unprocessed floats”.  This package
releases the limit; TeX itself imposes limits (which are independent of the
help offered by e-TeX).

However, if your floats can’t be placed anywhere, extending the number of
floats merely delays the arrival of the inevitable error message.")
    (license license:lppl1.3c+)))

(define-public texlive-ifmtarg
  (package
    (name "texlive-ifmtarg")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ifmtarg/" "source/latex/ifmtarg/"
                   "tex/latex/ifmtarg/")
             (base32
              "19bfi12j5ra19k6vjd1q5fjsm68vipa7ida7pg9pf15l5pxwbgqz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (native-inputs (list texlive-filecontents))
    (home-page "https://ctan.org/pkg/ifmtarg")
    (synopsis "If-then-else command for processing potentially empty arguments")
    (description
     "This package provides a command for the LaTeX programmer for testing
whether an argument is empty.")
    (license license:lppl1.3c+)))

(define-public texlive-pagenote
  (package
    (name "texlive-pagenote")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pagenote/" "source/latex/pagenote/"
                   "tex/latex/pagenote/")
             (base32
              "1dffh7ac13w3gs94lvfxgw1i4k6cfkrpcyikj1sfrqaivrxpmqpi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pagenote")
    (synopsis "Notes at end of document")
    (description
     "The @code{pagenote} package provides tagged notes on a separate
page (also known as end notes).")
    (license license:lppl1.3c+)))

(define-public texlive-pagesel
  (package
    (name "texlive-pagesel")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pagesel/" "source/latex/pagesel/"
                   "tex/latex/pagesel/")
             (base32
              "1x4nb53d5gm0vrhiinaqf6ai63bgjpbw0b5kk3c2f6j6gfvp0n53")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pagesel")
    (synopsis "Select pages of a document for output")
    (description
     "This package selects single pages, ranges of pages, odd pages or even
pages for output.  The package is part of the @code{oberdiek} bundle.")
    (license license:lppl1.3c)))

(define-public texlive-titling
  (package
    (name "texlive-titling")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/titling/" "source/latex/titling/"
                   "tex/latex/titling/")
             (base32
              "1wp5r6wwrz1nx3wrmc0hxjfapqppcb126l4wmmzh14sfb1py7mz4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/titling")
    (synopsis "Control typesetting of the @code{\\maketitle} command")
    (description
     "The @code{titling} package provides control over the typesetting of the
@code{\\maketitle} command and @code{\\thanks} commands, and makes the
@code{\\title}, @code{\\author} and @code{\\date} information permanently
available.  Multiple titles are allowed in a single document.  New titling
elements can be added and a @code{titlepage} title can be centered on
a physical page.")
    (license license:lppl)))

(define-public texlive-ifoddpage
  (package
    (name "texlive-ifoddpage")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ifoddpage/"
                   "source/latex/ifoddpage/"
                   "tex/latex/ifoddpage/")
             (base32
              "0mxi28lf97l4zg5kcv524b29n5r167yczrhgy132hql866vkdvyr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (home-page "https://ctan.org/pkg/ifoddpage")
    (synopsis "Determine if the current page is odd or even")
    (description
     "This package provides an @code{\\ifoddpage} conditional to determine if
the current page is odd or even.  The macro @code{\\checkoddpage} must be used
directly before to check the page number using a label.  Two compiler runs are
therefore required to achieve correct results.  In addition, the conditional
@code{\\ifoddpageoronside} is provided which is also true in @code{oneside}
mode where all pages use the odd page layout.")
    (license license:lppl1.3)))

(define-public texlive-storebox
  (package
    (name "texlive-storebox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/storebox/" "source/latex/storebox/"
                   "tex/latex/storebox/")
             (base32
              "1vbjq9aq2kbncq1dn4rk7jspfb6kcxk66h49z0xz1qix5yg94gmx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (home-page "https://ctan.org/pkg/storebox")
    (synopsis "Storing information for reuse")
    (description
     "The package provides store boxes whose user interface matches that of
normal LaTeX save boxes, except that the content of a store box appears at
most once in the output PDF file, however often it is used.  The present
version of the package supports pdfLaTeX and LuaLaTeX; when DVI is output,
store boxes behave the same as save boxes.")
    (license license:lppl1.3+)))

(define-public texlive-collectbox
  (package
    (name "texlive-collectbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/collectbox/"
                   "source/latex/collectbox/"
                   "tex/latex/collectbox/")
             (base32
              "0mn0hdzjhbmziqqh2k7knfz816lxbjil0zld0n30qi3ila5v3gk6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (home-page "https://ctan.org/pkg/collectbox")
    (synopsis "Collect and process macro arguments as boxes")
    (description
     "The package provides macros to collect and process a macro
argument (i.e., something which looks like a macro argument) as a horizontal
box rather than as a real macro argument.  The \"arguments\" are stored as if
they had been saved by @code{\\savebox} or by the @code{lrbox} environment.
Grouping tokens @code{\\bgroup} and @code{\\egroup} may be used, which allows
the user to have the beginning and end of a group in different macro
invocations, or to place them in the begin and end code of an environment.
Arguments may contain verbatim material or other special use of characters.
The macros were designed for use within other macros.")
    (license license:lppl1.3)))

(define-public texlive-collection-basic
  (package
    (name "texlive-collection-basic")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
   (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-amsfonts
           texlive-bibtex
           texlive-bin                  ;set GUIX_TEXMF and engines
           texlive-cm
           texlive-colorprofiles
           texlive-dvipdfmx
           texlive-dvips
           texlive-ec
           texlive-enctex
           texlive-etex
           texlive-etex-pkg
           texlive-glyphlist
           texlive-graphics-def
           texlive-hyphen-complete
           texlive-hyphenex
           texlive-ifplatform
           texlive-iftex
           texlive-knuth-lib
           texlive-knuth-local
           texlive-kpathsea
           texlive-lua-alt-getopt
           texlive-luahbtex
           texlive-luatex
           texlive-makeindex
           texlive-metafont
           texlive-mflogo
           texlive-mfware
           texlive-modes
           texlive-pdftex
           texlive-plain
           texlive-tex
           texlive-tex-ini-files
           texlive-unicode-data
           texlive-xdvi))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Essential programs and files")
    (description
     "These files are regarded as basic for any TeX system, covering plain TeX
macros, Computer Modern fonts, and configuration for common drivers; no
LaTeX.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-collection-context
  (package
    (name "texlive-collection-context")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-collection-basic
           texlive-context
           texlive-context-account
           texlive-context-algorithmic
           texlive-context-animation
           texlive-context-annotation
           texlive-context-bnf
           texlive-context-chromato
           texlive-context-cmscbf
           texlive-context-cmttbf
           texlive-context-construction-plan
           texlive-context-cyrillicnumbers
           texlive-context-degrade
           texlive-context-fancybreak
           texlive-context-filter
           texlive-context-french
           texlive-context-fullpage
           texlive-context-gantt
           texlive-context-gnuplot
           texlive-context-handlecsv
           texlive-context-layout
           texlive-context-letter
           texlive-context-lettrine
           texlive-context-mathsets
           texlive-context-rst
           texlive-context-ruby
           texlive-context-simplefonts
           texlive-context-simpleslides
           texlive-context-title
           texlive-context-transliterator
           texlive-context-typearea
           texlive-context-typescripts
           texlive-context-vim
           texlive-context-visualcounter
           texlive-jmn))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "ConTeXt and packages")
    (description
     "This package provides Hans Hagen's powerful ConTeXt system, along with
third-party ConTeXt packages.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-collection-fontsrecommended
  (package
    (name "texlive-collection-fontsrecommended")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-avantgar
           texlive-bookman
           texlive-charter
           texlive-cm-super
           texlive-cmextra
           texlive-collection-basic
           texlive-courier
           texlive-euro
           texlive-euro-ce
           texlive-eurosym
           texlive-fpl
           texlive-helvetic
           texlive-lm
           texlive-lm-math
           texlive-manfnt-font
           texlive-marvosym
           texlive-mathpazo
           texlive-mflogo-font
           texlive-ncntrsbk
           texlive-palatino
           texlive-pxfonts
           texlive-rsfs
           texlive-symbol
           texlive-tex-gyre
           texlive-tex-gyre-math
           texlive-times
           texlive-tipa
           texlive-txfonts
           texlive-utopia
           texlive-wasy
           texlive-wasy-type1
           texlive-wasysym
           texlive-zapfchan
           texlive-zapfding))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Recommended fonts")
    (description
     "This collection provides recommended fonts, including the base 35
PostScript fonts, Latin Modern, TeX Gyre, and T1 and other encoding support
for Computer Modern, in outline form.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-collection-latex
  (package
    (name "texlive-collection-latex")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-ae
           texlive-amscls
           texlive-amsmath
           texlive-atbegshi
           texlive-atveryend
           texlive-auxhook
           texlive-babel
           texlive-babel-english
           texlive-babelbib
           texlive-bigintcalc
           texlive-bitset
           texlive-bookmark
           texlive-carlisle
           texlive-collection-basic
           texlive-colortbl
           texlive-epstopdf-pkg
           texlive-etexcmds
           texlive-fancyhdr
           texlive-firstaid
           texlive-fix2col
           texlive-geometry
           texlive-gettitlestring
           texlive-graphics
           texlive-graphics-cfg
           texlive-grfext
           texlive-hopatch
           texlive-hycolor
           texlive-hyperref
           texlive-intcalc
           texlive-kvdefinekeys
           texlive-kvoptions
           texlive-kvsetkeys
           texlive-l3backend
           texlive-l3kernel
           texlive-l3packages
           texlive-latex
           texlive-latex-bin
           texlive-latex-fonts
           texlive-latexconfig
           texlive-letltxmacro
           texlive-ltxcmds
           texlive-ltxmisc
           texlive-mfnfss
           texlive-mptopdf
           texlive-natbib
           texlive-oberdiek
           texlive-pagesel
           texlive-pdfescape
           texlive-pslatex
           texlive-psnfss
           texlive-pspicture
           texlive-refcount
           texlive-rerunfilecheck
           texlive-stringenc
           texlive-tools
           texlive-uniquecounter
           texlive-url))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "LaTeX fundamental packages")
    (description
     "These packages are either mandated by the core LaTeX team, or very widely
used and strongly recommended in practice.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-collection-latexrecommended
  (package
    (name "texlive-collection-latexrecommended")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-anysize
           texlive-attachfile2
           texlive-beamer
           texlive-booktabs
           texlive-breqn
           texlive-caption
           texlive-cite
           texlive-cmap
           texlive-collection-latex
           texlive-crop
           texlive-ctable
           texlive-eso-pic
           texlive-etoolbox
           texlive-euenc
           texlive-euler
           texlive-everysel
           texlive-everyshi
           texlive-extsizes
           texlive-fancybox
           texlive-fancyref
           texlive-fancyvrb
           texlive-filehook
           texlive-float
           texlive-fontspec
           texlive-footnotehyper
           texlive-fp
           texlive-grffile
           texlive-hologo
           texlive-index
           texlive-infwarerr
           texlive-jknapltx
           texlive-koma-script
           texlive-l3experimental
           texlive-latexbug
           texlive-lineno
           texlive-listings
           texlive-lwarp
           texlive-mathspec
           texlive-mathtools
           texlive-mdwtools
           texlive-memoir
           texlive-metalogo
           texlive-microtype
           texlive-ms
           texlive-newfloat
           texlive-ntgclass
           texlive-parskip
           texlive-pdfcolfoot
           texlive-pdflscape
           texlive-pdfmanagement-testphase
           texlive-pdfpages
           texlive-pdftexcmds
           texlive-polyglossia
           texlive-psfrag
           texlive-ragged2e
           texlive-rcs
           texlive-sansmath
           texlive-section
           texlive-seminar
           texlive-sepnum
           texlive-setspace
           texlive-subfig
           texlive-textcase
           texlive-thumbpdf
           texlive-translator
           texlive-typehtml
           texlive-ucharcat
           texlive-underscore
           texlive-unicode-math
           texlive-xcolor
           texlive-xkeyval
           texlive-xltxtra
           texlive-xunicode))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "LaTeX recommended packages")
    (description
     "This package provides a collection of recommended add-on packages for
LaTeX which have widespread use.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-collection-metapost
  (package
    (name "texlive-collection-metapost")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-automata
           texlive-bbcard
           texlive-blockdraw-mp
           texlive-bpolynomial
           texlive-cmarrows
           texlive-collection-basic
           texlive-drv
           texlive-dviincl
           texlive-emp
           texlive-epsincl
           texlive-expressg
           texlive-exteps
           texlive-featpost
           texlive-feynmf
           texlive-feynmp-auto
           texlive-fiziko
           texlive-garrigues
           texlive-gmp
           texlive-hatching
           texlive-hershey-mp
           texlive-latexmp
           texlive-mcf2graph
           texlive-metago
           texlive-metaobj
           texlive-metaplot
           texlive-metapost
           texlive-metapost-colorbrewer
           texlive-metauml
           texlive-mfpic
           texlive-mfpic4ode
           texlive-minim-hatching
           texlive-mp3d
           texlive-mparrows
           texlive-mpattern
           texlive-mpcolornames
           texlive-mpgraphics
           texlive-mptrees
           texlive-piechartmp
           texlive-repere
           texlive-roex
           texlive-roundrect
           texlive-shapes
           texlive-slideshow
           texlive-splines
           texlive-suanpan
           texlive-textpath
           texlive-threeddice))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "MetaPost and Metafont packages")
    (description
     "This collection includes all MetaPost and Metafont packages, along
with packages in @code{collection-basic}.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-collection-xetex
  (package
    (name "texlive-collection-xetex")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-arabxetex
           texlive-awesomebox
           texlive-bidi-atbegshi
           texlive-bidicontour
           texlive-bidipagegrid
           texlive-bidipresentation
           texlive-bidishadowtext
           texlive-businesscard-qrcode
           texlive-collection-basic
           texlive-cqubeamer
           texlive-fixlatvian
           texlive-font-change-xetex
           texlive-fontbook
           texlive-fontwrap
           texlive-interchar
           texlive-na-position
           texlive-philokalia
           texlive-ptext
           texlive-realscripts
           texlive-simple-resume-cv
           texlive-simple-thesis-dissertation
           texlive-tetragonos
           texlive-ucharclasses
           texlive-unicode-bidi
           texlive-unimath-plain-xetex
           texlive-unisugar
           texlive-xebaposter
           texlive-xechangebar
           texlive-xecolor
           texlive-xecyr
           texlive-xeindex
           texlive-xesearch
           texlive-xespotcolor
           texlive-xetex
           texlive-xetex-itrans
           texlive-xetex-pstricks
           texlive-xetex-tibetan
           texlive-xetexconfig
           texlive-xetexfontinfo
           texlive-xetexko
           texlive-xevlna
           texlive-zbmath-review-template))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "XeTeX and packages")
    (description
     "This collection includes packages for XeTeX, the Unicode and
OpenType-enabled TeX by Jonathan Kew.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-grfext
  (package
    (name "texlive-grfext")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/grfext/" "source/latex/grfext/"
                   "tex/latex/grfext/")
             (base32
              "1x35r10mkjg8dzx7aj99y4dwyf69jgs41qwapdx523lbglywmgxp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/grfext")
    (synopsis "Manipulate the @code{graphics} package's list of extensions")
    (description
     "This package provides macros for adding to, and reordering the list of
graphics file extensions recognised by package @code{graphics}.")
    (license license:lppl1.3c+)))

(define-public texlive-addliga
  (package
    (name "texlive-addliga")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/addliga/"
                   "tex/lualatex/addliga/")
             (base32
              "1gbdbwskba1rf3bmvwms3i3crj3dcqqg69wdzfdyy1a6ml05qpc7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/addliga")
    (synopsis "Access basic ligatures in legacy TrueType fonts")
    (description
     "This small and simple package allows LuaLaTeX users to access basic
ligatures (ff, fi, ffi, fl, ffl) in legacy TrueType fonts (those
lacking a liga table) accessed via @code{fontspec}.")
    (license license:public-domain)))

(define-public texlive-adjustbox
  (package
    (name "texlive-adjustbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/adjustbox/"
                   "source/latex/adjustbox/"
                   "tex/latex/adjustbox/")
             (base32
              "02iqc3i3n3d16xx8hgfy5s28h26fhnqf1f4kcxap6rssr165jj3h")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-ydoc))
    (propagated-inputs
     (list texlive-collectbox texlive-graphics texlive-xkeyval))
    (home-page "https://ctan.org/pkg/adjustbox")
    (synopsis "Graphics package-alike macros for general boxes")
    (description "The package provides several macros to adjust boxed
content.  One purpose is to supplement the standard @code{graphics} package,
which defines the macros @code{\\resizebox}, @code{\\scalebox} and
@code{\\rotatebox} , with the macros @code{\\trimbox} and @code{\\clipbox}.
The main feature is the general @code{\\adjustbox} macro which extends the
@code{key=value} interface of @code{\\includegraphics} from the
@code{graphics} package and applies it to general text content.  Additional
provided box macros are @code{\\lapbox}, @code{\\marginbox},
@code{\\minsizebox}, @code{\\maxsizebox} and @code{\\phantombox}.")
    (license license:lppl1.3+)))

(define-public texlive-auto-pst-pdf-lua
  (package
    (name "texlive-auto-pst-pdf-lua")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/auto-pst-pdf-lua/"
                   "tex/latex/auto-pst-pdf-lua/")
             (base32
              "1wkhdg47qjpc0yv47z1jmrmpnq8zic6bqfmay3ry6lvv47f775z1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-iftex))
    (home-page "https://ctan.org/pkg/auto-pst-pdf-lua")
    (synopsis "Using LuaLaTeX together with PostScript code")
    (description
     "This package is a slightly modified version of @code{auto-pst-pdf} by
Will Robertson, which itself is a wrapper for @code{pst-pdf} by Rolf
Niepraschk.  The package allows the use of LuaLaTeX together with PostScript
related code, e.g., PSTricks.")
    (license license:lppl1.3c)))

(define-public texlive-barracuda
  (package
    (name "texlive-barracuda")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/barracuda/" "scripts/barracuda/"
                   "tex/luatex/barracuda/")
             (base32
              "1v318nbbbp2klp0rykzb3skqqw4hhj1hbliigmp3qh34iiq3wqh0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/barracuda")
    (synopsis "Draw barcodes with Lua")
    (description
     "The Barracuda library is a modular Lua package for drawing barcode
symbols.  It provides modules for writing barcodes from a LuaTeX document.  It
is also possible to use Barracuda with a standalone Lua interpreter to draw
barcodes in different graphic formats like SVG.")
    (license license:gpl2)))

(define-public texlive-bezierplot
  (package
    (name "texlive-bezierplot")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/bezierplot/"
                   "tex/lualatex/bezierplot/")
             (base32
              "125gdb9d160g729v3fbsrsrw5abk34jwa4x43yil0mzjls2aq550")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bezierplot")
    (synopsis "Approximate smooth function graphs with cubic Bezier splines")
    (description
     "This package consists of a Lua program as well as a (Lua)LaTeX
@file{.sty} file.  Given a smooth function, @code{bezierplot} returns a smooth
Bezier path written in TikZ notation, which also matches MetaPost, that
approximates the graph of the function.  For polynomial functions of degree
lesser or equal to 3 and their inverses the approximation is exact (up to
numeric precision).  @code{bezierplot} also finds special points such as
extreme points and inflection points and reduces the number of used points.")
    (license license:lppl1.3c)))

(define-public texlive-gates
  (package
    (name "texlive-gates")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/gates/" "tex/generic/gates/")
             (base32
              "0brcms8qbnsyiqnnqxyzqp12z1xpvfzb5hgh6kqm28904lr84i4g")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/gates")
    (synopsis "Support for writing modular and customisable code")
    (description
     "The package provides the means of writing code in a modular fashion: big
macros or functions are divided into small chunks (called gates) with names,
which can be externally controlled (e.g., they can be disabled, subjected to
conditionals, loops...) and/or augmented with new chunks.  Thus complex code
may easily be customised without having to rewrite it, or even understand its
implementation: the behavior of existing gates can be modified, and new ones
can be added, without endangering the whole design.  This allows code to be
hacked in ways the original authors might have never envisioned.  The
@code{gates} package is implemented independently for both TeX and Lua.  The
TeX implementation, running in any current environment, requires the
@code{texapi} package, whereas the Lua version can be run with any Lua
interpreter, not just LuaTeX.")
    (license license:lppl)))

(define-public texlive-qrcode
  (package
    (name "texlive-qrcode")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/qrcode/" "source/latex/qrcode/"
                   "tex/latex/qrcode/")
             (base32
              "197v18lsvb90i07gxvc6mrmn1z63q8v0wvcnbk8dnn3hhabpn16y")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/qrcode")
    (synopsis "QR codes without external tools")
    (description
     "The package generates QR (Quick Response) codes in LaTeX, without the
need for PSTricks or any other graphical package.")
    (license license:lppl1.3c+)))

(define-public texlive-tcolorbox
  (package
    (name "texlive-tcolorbox")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/tcolorbox/" "tex/latex/tcolorbox/")
             (base32
              "1vygwa4y9mc7qgwf5awi0aa5c5kakbdcsl5kry0ldr1lkaxs1j8w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tcolorbox")
    (synopsis "Coloured boxes, for LaTeX examples and theorems, etc")
    (description
     "This package provides an environment for coloured and framed text boxes
with a heading line.  Optionally, such a box may be split in an upper and
a lower part; thus the package may be used for the setting of LaTeX examples
where one part of the box displays the source code and the other part shows
the output.  Another common use case is the setting of theorems.  The package
supports saving and reuse of source code and text parts.")
    (license license:lppl1.3c+)))

(define-public texlive-ebproof
  (package
    (name "texlive-ebproof")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ebproof/" "source/latex/ebproof/"
                   "tex/latex/ebproof/")
             (base32
              "1a3203jgxsgihfgb6wwm0gfpaxbf1lg5axcakan9rj316xrrj4lc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ebproof")
    (synopsis "Formal proofs in the style of sequent calculus")
    (description
     "This package provides commands to typeset proof trees in the style of
sequent calculus and related systems.  The commands allow for writing
inferences with any number of premises and alignment of successive formulas on
an arbitrary point.  Various options allow complete control over spacing,
styles of inference rules, placement of labels, etc.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-ebproof texlive-ebproof)

(define-public texlive-bussproofs
  (package
    (name "texlive-bussproofs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/bussproofs/"
                   "tex/latex/bussproofs/")
             (base32
              "1gb8y9g89fqw1kix4d2vb7mj440vlb8hnpsa3jqpk9yicndwcyk6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/bussproofs")
    (synopsis "Proof trees in the style of the sequent calculus")
    (description
     "This package provides commands to typeset proof trees in the style of
sequent calculus and related systems.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-bussproofs texlive-bussproofs)

(define-public texlive-euenc
  (package
    (name "texlive-euenc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/euenc/" "source/latex/euenc/"
                   "tex/latex/euenc/")
             (base32
              "0vhqxhj1v68rhi08xivps8icxmlcq9mv8slqmsmf2qhvzj6x6qx3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     ;; Sole ".dtx" file bundled only generates documentation.
     (list #:build-targets #~(list)))
    (home-page "https://ctan.org/pkg/euenc")
    (synopsis "Unicode font encoding definitions for XeTeX")
    (description
     "The package provides font encoding definitions for unicode fonts loaded
by LaTeX in XeTeX or LuaTeX.  The package provides two encodings: EU1,
designed for use with XeTeX, which the fontspec uses for unicode fonts which
require no macro-level processing for accents, and EU2, which provides the
same facilities for use with LuaTeX.  Neither encoding places any restriction
on the glyphs provided by a font; use of EU2 causes the package
@code{euxunicode} to be loaded (the package is part of this distribution).
The package includes font definition files for use with the Latin Modern
OpenType fonts.")
    (license license:lppl1.3+)))

(define-public texlive-euro
  (package
    (name "texlive-euro")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/euro/" "source/latex/euro/"
                   "tex/latex/euro/")
             (base32
              "1bmswsw4clzrrgxmk473ghsdbnw6zwa2rgbjs32i1i5c37026yfi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/euro")
    (synopsis "Provide Euro values for national currency amounts")
    (description
     "This package converts arbitrary national currency amounts using the Euro
as base unit, and typesets monetary amounts in almost any desired way.
Conversion rates for the initial Euro-zone countries are already built-in.
Further rates can be added easily.")
    (license license:lppl)))

(define-public texlive-euro-ce
  (package
    (name "texlive-euro-ce")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/euro-ce/"
                   "fonts/source/public/euro-ce/"
                   "fonts/tfm/public/euro-ce/")
             (base32
              "0vyh6ln2wdn42g5r1rg0gqf6q39ny7ldv2b6ikw8b029bdg5079p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-metafont))
    (home-page "https://ctan.org/pkg/euro-ce")
    (synopsis "Euro and CE sign font")
    (description
     "This package provides Metafont source for the Euro and CE symbols in
several variants, designed to fit with the Computer Modern-set text.")
    (license license:bsd-3)))           ;from "euro-ce.doc"

(define-public texlive-eurosym
  (package
    (name "texlive-eurosym")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/eurosym/"
                   "fonts/map/dvips/eurosym/"
                   "fonts/source/public/eurosym/"
                   "fonts/tfm/public/eurosym/"
                   "fonts/type1/public/eurosym/"
                   "tex/latex/eurosym/")
             (base32
              "0ml24rxbl1yir4s3fjjxm0z7axklc3p33syg41b76zc7hck9mk8s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs (list texlive-cm texlive-metafont))
    (home-page "https://ctan.org/pkg/eurosym")
    (synopsis "Metafont and macros for Euro sign")
    (description
     "The European currency symbol for the Euro implemented in Metafont, using
the official European Commission dimensions, and providing several
shapes (normal, slanted, bold, outline).  The package also includes a LaTeX
package which defines the macro, pre-compiled @file{tfm} files, and
documentation.")
    (license (license:non-copyleft "file:///doc/fonts/eurosym/COPYING"))))

(define-public texlive-kanaparser
  (package
    (name "texlive-kanaparser")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/kanaparser/"
                   "tex/luatex/kanaparser/")
             (base32
              "0bcvxkdb2y60w7c0swi3f8yncli0yswalrdzxjv7h0xp21c202hp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/kanaparser")
    (synopsis "Kana parser for LuaTeX")
    (description
     "The package provides a kana parser for LuaTeX.  It is a set of four
macros that handle transliteration of text: from hiragana and katakana to
Latin from Latin and katakana to hiragana from Latin and hiragana to katakana
It can be used to write kana directly using only the ASCII character set or
for education purposes.  The package has support for obsolete and rarely used
syllables, some only accessible via the provided toggle macro.")
    ;; License is BSD.  Assuming original BSD-4.
    (license license:bsd-4)))

(define-public texlive-kastrup
  (package
    (name "texlive-kastrup")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/kastrup/"
                   "source/generic/kastrup/"
                   "tex/generic/kastrup/")
             (base32
              "0sbf4xw1jsh9pbjhqw3f0bg067f0rhf936wfnqpzj9kp2167n41j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/binhex")
    (synopsis "Convert numbers into binary, octal and hexadecimal")
    (description
     "The @code{kastrup} package provides the @emph{binhex.tex} file.  This
file provides expandable macros for both fixed-width and minimum-width numbers
to bases 2, 4, 8 and 16.  All constructs TeX accepts as arguments to its
@code{\\number} primitive are valid as arguments for the macros.  The package
may be used under LaTeX and plain TeX.")
    (license (license:fsf-free "file:/binhex.dtx"))))

(define-public texlive-translations
  (package
    (name "texlive-translations")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/translations/"
                   "tex/latex/translations/")
             (base32
              "16jcpb6afjqcqb8hn47dip2w7l9hg7q1vspg791sp1r1dsn81yf4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/translations")
    (synopsis "Internationalisation of LaTeX2e packages")
    (description
     ;; Polish not mentioned on CTAN, but there is a
     ;; translations-basic-dictionary-polish.trsl file.
     "This package (once part of the @code{exsheets} package), provides a
framework for providing multilingual features to a LaTeX package.  The package
has its own basic dictionaries for English, Brazilian, Catalan, Dutch, French,
German, Polish and Spanish.  It aims to use translation material for English,
Dutch, French, German, Italian, Spanish, Catalan, Turkish, Croatian, Hungarian,
Danish and Portuguese from babel or polyglossia if either is in use in the
document.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-translations texlive-translations)

(define-public texlive-translator
  (package
    (name "texlive-translator")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/translator/"
                   "tex/latex/translator/")
             (base32
              "0vmg4w5spl98y9r4h6p89xa43xxfqmv5qlc3sf7kjkyp58px8axs")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/translator")
    (synopsis "Easy translation of strings in LaTeX")
    (description
     "This LaTeX package provides a flexible mechanism for translating
individual words into different languages.  Such a translation mechanism is
useful when the author of some package would like to localize the package such
that texts are correctly translated into the language preferred by the user.
This package is not intended to be used to automatically translate more than
a few words.")
    (license (list license:lppl license:gpl1+))))

(define-public texlive-texapi
  (package
    (name "texlive-texapi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/texapi/" "tex/generic/texapi/")
             (base32
              "0ivg2a2pgl6bmb2242cjjcz7n9cs514dp1r8c89mva2zlm9l323j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/texapi")
    (synopsis "Macros to write format-independent packages")
    (description
     "Texapi provides utility macros to write format-independent (and -aware)
packages.  It is similar in spirit to the etoolbox, except that it isn't tied
to LaTeX.  The tools include engine and format detection, expansion control,
command definition and manipulation, various testing macros, string
operations, and highly customizable while and for loops.")
    (license license:lppl)))

(define-public texlive-textpos
  (package
    (name "texlive-textpos")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/textpos/" "source/latex/textpos/"
                   "tex/latex/textpos/")
             (base32
              "0spxbk9w69kcmgib33nq2x7ls8566fg214rcmkb126yyn7jqg567")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/textpos")
    (synopsis "Place boxes at arbitrary positions on the LaTeX page")
    (description
     "This package provides a package to facilitate placement of boxes at
absolute positions on the LaTeX page.  There are several reasons why this
might be useful, an important one being to help the creation of large-format
conference posters.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-textpos texlive-textpos)

(define-public texlive-unicode-math
  (package
    (name "texlive-unicode-math")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/unicode-math/"
                   "source/latex/unicode-math/"
                   "tex/latex/unicode-math/")
             (base32
              "0w5gp11ccc486lckzag63arg97g1r0zkf29bdnnk13pz4r5m2lgx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "xelatex"))
    (native-inputs (list texlive-xetex))
    (propagated-inputs (list texlive-fontspec texlive-lm-math))
    (home-page "https://ctan.org/pkg/unicode-math")
    (synopsis "Unicode mathematics support for XeTeX and LuaTeX")
    (description
     "This package will provide a complete implementation of unicode maths for
XeLaTeX and LuaLaTeX.  Unicode maths is currently supported by the following
fonts:
@itemize
@item Latin Modern Math,
@item TeX Gyre Bonum Math,
@item TeX Gyre Pagella Math,
@item TeX Gyre Schola Math,
@item TeX Gyre Termes Math,
@item DejaVu Math TeX Gyre,
@item Asana-Math fonts,
@item STIX,
@item XITS Math,
@item Libertinus Math,
@item Fira Math.
@end itemize")
    (license license:lppl1.3c+)))

(define-public texlive-xifthen
  (package
    (name "texlive-xifthen")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xifthen/" "tex/latex/xifthen/")
             (base32
              "0b33mlmnxsj5mi06v2w2zgamk51mgv1lxdr1cax8nkpn9g7n9axw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xifthen")
    (synopsis "Extended conditional commands")
    (description
     "This package extends the @code{ifthen} package by implementing new
commands to go within the first argument of @code{\\\\ifthenelse}: to test
whether a string is void or not, if a command is defined or equivalent to
another.  The package also enables use of complex expressions as introduced by
the package @code{calc}, together with the ability of defining new commands to
handle complex tests.")
    (license license:lppl)))

(define-public texlive-xindy
  (package
    (name "texlive-xindy")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/tex2xindy.1"
                   "doc/man/man1/tex2xindy.man1.pdf"
                   "doc/man/man1/texindy.1"
                   "doc/man/man1/texindy.man1.pdf"
                   "doc/man/man1/xindy.1"
                   "doc/man/man1/xindy.man1.pdf"
                   "doc/xindy/"
                   "scripts/xindy/"
                   "xindy/")
             (base32
              "12j2bi0wwp1hyxr1427hhigqmhsd1fyg90bvghxkm1qck85r24vf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:link-scripts #~(list "texindy.pl" "xindy.pl")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-inputs
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "scripts/xindy/"
                ;; The scripts are encoded in ISO-8859-1 (or iso-latin-1).
                (with-fluids ((%default-port-encoding "ISO-8859-1"))
                  (substitute* (list "texindy.pl" "xindy.pl")
                    (("our \\$clisp = .*")
                     (format #f "our $clisp = ~s;~%"
                             (search-input-file inputs "/bin/clisp")))
                    (("/usr/bin/env perl")
                     (search-input-file inputs "/bin/perl"))))))))))
    (inputs (list clisp perl))
    (home-page "https://ctan.org/pkg/xindy")
    (synopsis "General-purpose index processor")
    (description
     "Xindy was developed after an impasse had been encountered in the attempt
to complete internationalisation of @command{makeindex}.  Xindy can be used to
process indexes for documents marked up using (La)TeX, Nroff family and
SGML-based languages.  Xindy is highly configurable, both in markup terms and
in terms of the collating order of the text being processed.")
    (license license:gpl2+)))

(define-public texlive-xits
  (package
    (name "texlive-xits")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/xits/"
                   "fonts/opentype/public/xits/")
             (base32
              "1359zgi8r4mwjv273zmc5jghyy4i54amkrmkq80z67x4kx092724")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xits")
    (synopsis "Scientific Times-like font")
    (description
     "XITS is a Times-like font for scientific typesetting with proper
mathematical support for modern, Unicode and OpenType capable TeX engines,
namely LuaTeX and XeTeX. For use with LuaLaTeX or XeLaTeX, support is
available from the @code{fontspec} and @code{unicode-math} packages.")
    (license license:silofl1.1)))

(define-public texlive-ntgclass
  (package
    (name "texlive-ntgclass")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ntgclass/"
                   "source/latex/ntgclass/"
                   "tex/latex/ntgclass/")
             (base32
              "04rvr1gldp87nqplmnqkwi3l9jclsjaawj5rym72gy6sw9snpmck")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ntgclass")
    (synopsis "European versions of standard classes")
    (description
     "The bundle offers versions of the standard LaTeX @code{article} and
@code{report} classes, rewritten to reflect a more European design, and the
@code{a4} package, which is better tuned to the shape of a4 paper than is the
@code{a4paper} class option of the standard classes.  The classes include
several for @code{article} and @code{report} requirements, and a @code{letter}
class.  The elements of the bundle were designed by members of the Dutch TeX
Users Group NTG.")
    (license license:lppl1.3+)))

(define-public texlive-nth
  (package
    (name "texlive-nth")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/generic/nth/")
             (base32
              "0716sd99xjdkplm7jdmg4lx8lpfnnx6mxjp1l1sp2bfqcg73p4hm")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/nth")
    (synopsis "Generate English ordinal numbers")
    (description
     "The command @code{\\nth{<number>}} generates English ordinal numbers of
the form 1st, 2nd, 3rd, 4th, etc.  LaTeX package options may specify that the
ordinal mark be superscripted, and that negative numbers may be treated; Plain
TeX users have no access to package options, so need to redefine macros for
these changes.")
    (license license:public-domain)))

(define-public texlive-ntheorem
  (package
    (name "texlive-ntheorem")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ntheorem/" "source/latex/ntheorem/"
                   "tex/latex/ntheorem/")
             (base32
              "16xain8s0azcnhwj5xwh3m365sb9bhdvxanh19kvmnc52dggjc1y")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ntheorem")
    (synopsis "Enhanced theorem environment")
    (description
     "The package offers enhancements for theorem-like environments: easier
control of layout; proper placement of endmarks even when the environment ends
with @code{\\end@{enumerate@}} or @code{\\end@{displaymath@}} (including
support for @code{amsmath} displayed-equation environments); and support for
making a list of theorems, analagous to @code{\\listoffigures}.")
    (license license:lppl)))

(define-public texlive-fmtcount
  (package
    (name "texlive-fmtcount")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fmtcount/" "scripts/fmtcount/"
                   "source/latex/fmtcount/" "tex/latex/fmtcount/")
             (base32
              "1biw0g6s2arq6kq52c1yfkl0vzafja2az65c3d0syq0vgjzj9763")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fmtcount")
    (synopsis "Display the value of a LaTeX counter in a variety of formats")
    (description
     "The package provides commands that display the value of a LaTeX counter
in a variety of formats (ordinal, text, hexadecimal, decimal, octal, binary
etc).  The package offers some multilingual support; configurations for use in
English (both British and American usage), French (including Belgian and Swiss
variants), German, Italian, Portuguese and Spanish documents are provided.")
    (license license:lppl1.3+)))

(define-public texlive-inriafonts
  (package
    (name "texlive-inriafonts")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/fonts/inriafonts/"
                   "fonts/enc/dvips/inriafonts/"
                   "fonts/map/dvips/inriafonts/"
                   "fonts/opentype/public/inriafonts/"
                   "fonts/tfm/public/inriafonts/"
                   "fonts/truetype/public/inriafonts/"
                   "fonts/type1/public/inriafonts/"
                   "fonts/vf/public/inriafonts/"
                   "tex/latex/inriafonts/")
             (base32
              "0ngbpr4pl7r82jmdhiksp32qvbvggf2nawwqq0pkb7cffp95ya49")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/inriafonts")
    (synopsis "Inria fonts with LaTeX support")
    (description
     "Inria is a free font designed by Black[Foundry] for Inria, a French
research institute.  It comes as Serif and Sans Serif, each with three weights
and matching italics.  Using these fonts with XeLaTeX and LuaLaTeX is easy
using the @code{fontspec} package.  The present package provides a way of
using them with LaTeX and pdfLaTeX: it provides two style files,
@file{InriaSerif.sty} and @file{InriaSans.sty}, together with the PostScript
version of the fonts and their associated files.")
    (license (list license:lppl license:silofl1.1))))

(define-public texlive-floatflt
  (package
    (name "texlive-floatflt")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/floatflt/" "source/latex/floatflt/"
                   "tex/latex/floatflt/")
             (base32
              "1piy8ajbbcadsjwp0mhlgxm2ggggnb5sn75arfs5fxiaqrwd572j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/floatflt")
    (synopsis "Wrap text around floats")
    (description
     "The package can float text around figures and tables which do not span
the full width of a page; it improves upon floatfig, and allows tables/figures
to be set left/right or alternating on even/odd pages.")
    (license license:lppl1.3+)))

(define-public texlive-fvextra
  (package
    (name "texlive-fvextra")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/fvextra/" "source/latex/fvextra/"
                   "tex/latex/fvextra/")
             (base32
              "18r3722sf859yn5j1q084ix9gp8sp4znvdlwi2vnrrn36djyvkzj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/fvextra")
    (synopsis "Extensions and patches for @code{fancyvrb}")
    (description
     "This package provides several extensions to fancyvrb, including
automatic line breaking and improved math mode.")
    (license license:lppl1.3+)))

(define-public bibtool
  (package
    (name "bibtool")
    (version "2.68")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/ge-ne/bibtool")
         (commit (string-append
                  "BibTool_"
                  (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                              version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0grnmqj8w5018nd7r6drnq2yvfhf22gj9i3rj8ilhzm7zmz3zn0g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"))
    (native-inputs
     (list perl))
    (home-page "http://www.gerd-neugebauer.de/software/TeX/BibTool/en")
    (synopsis "Tool for manipulating BibTeX databases")
    (description
     "BibTool manipulates BibTeX files.  The possibilities of BibTool include
sorting and merging of BibTeX databases, generation of uniform reference keys,
and selecting references used in a publication.")
    (license license:gpl2+)))

(define-public texlive-apa6
  (package
    (name "texlive-apa6")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/apa6/" "source/latex/apa6/"
                   "tex/latex/apa6/")
             (base32
              "08jn8piyaad4zln33c0gikyhdkcsk2s3ms9l992riq2hbpbm9lcf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build
            ;; Build process wants to generate files in the wrong directory.
            (lambda _
              (substitute* "source/latex/apa6/apa6.ins"
                (("file\\{\\./.*?/") "file{")))))))
    (home-page "https://ctan.org/pkg/apa6")
    (synopsis "Format documents in APA style (6th edition)")
    (description
     "The class formats documents in APA style (6th Edition).  It provides
a full set of facilities in three different output modes (journal-like
appearance, double-spaced manuscript, LaTeX-like document).  The class can
mask author identity for copies for use in masked peer review.")
    (license license:lppl1.3c+)))

(define-public texlive-apacite
  (package
    (name "texlive-apacite")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/apacite/" "doc/bibtex/apacite/"
                   "source/bibtex/apacite/" "tex/latex/apacite/")
             (base32
              "0nc86zngk71xpbinrfm8p0413xphc0v86ddhcw94gi2sl00hsmzq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/apacite")
    (synopsis "Citation style following the rules of the APA")
    (description
     "Apacite provides a BibTeX style and a LaTeX package which are designed
to match the requirements of the American Psychological Association's style
for citations.  The package follows the 6th edition of the APA manual, and is
designed to work with the @code{apa6} class.")
    (license license:lppl)))

(define-public texlive-endfloat
  (package
    (name "texlive-endfloat")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/endfloat/" "source/latex/endfloat/"
                   "tex/latex/endfloat/")
             (base32
              "1zslmc5g28z6adfyd8bdlbw03jawxmgafq0mgwy811hrbcppb2kg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/endfloat")
    (synopsis "Move floats to the end, leaving markers where they belong")
    (description
     "The @code{endfloat} package places all floats on pages by themselves at
the end of the document, optionally leaving markers in the text near to where
the figure (or table) would normally have occurred.")
    (license license:gpl3+)))

(define-public texlive-was
  (package
    (name "texlive-was")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/was/" "source/latex/was/"
                   "tex/latex/was/")
             (base32
              "1c2kmfrm898c69bizw0650w82bjabp3jf57hmqfcb9y625pq0s05")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/was")
    (synopsis "Collection of small packages by Walter Schmidt")
    (description
     "This package provides a bundle of packages that arise in the author's
area of interest: compliance of maths typesetting with ISO standards; symbols
that work in both maths and text modes commas for both decimal separator and
maths; and upright Greek letters in maths.")
    (license license:lppl1.2+)))

(define-public texlive-xpatch
  (package
    (name "texlive-xpatch")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xpatch/" "source/latex/xpatch/"
                   "tex/latex/xpatch/")
             (base32
              "0r08hadnwx9vyppzmbn1bj69b12i5fw1mhk49piw2rqbk01722zk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xpatch")
    (synopsis "Extending @code{etoolbox} patching commands")
    (description
     "The package generalises the macro patching commands provided by Philipp
Lehmann's @code{etoolbox}.")
    (license license:lppl1.3c+)))

(define-public texlive-threeparttablex
  (package
    (name "texlive-threeparttablex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/threeparttablex/"
                   "tex/latex/threeparttablex/")
             (base32
              "19pvw2ifswxcf8dxw0mzjmqhl592477w5hcfh97f4wpya0dv2m9p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/threeparttablex")
    (synopsis "Notes in @code{longtables}")
    (description
     "The package provides the functionality of the @code{threeparttable}
package to tables created using the @code{longtable} package.")
    (license license:lppl1.3+)))

(define-public texlive-ligtype
  (package
    (name "texlive-ligtype")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/ligtype/"
                   "tex/lualatex/ligtype/")
             (base32
              "18m4j01zhp3kj2ixd53b9z0k5f6idbpr6jv45gw149j5niax1dxg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ligtype")
    (synopsis "Comprehensive ligature suppression functionalities")
    (description
     "This package suppresses inappropriate ligatures following specified
rules.  Both font and user kerning are applied correctly, and f-glyphs are
automatically replaced with their short-arm variant (if available).  Also
there is an emphasis on speed.  By default the package applies German language
ligature suppression rules.  With the help of options and macros it can be
used for other languages as well.  The package requires LuaLaTeX.")
    (license license:lppl1.3c)))

(define-public texlive-linebreaker
  (package
    (name "texlive-linebreaker")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/linebreaker/"
                   "tex/lualatex/linebreaker/")
             (base32
              "1rppq2dlj7g5djq5f0hhgk9sgk0ip1ha4vqhx6ajfpzdchg7b1cw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/linebreaker")
    (synopsis "Prevent overflow boxes with LuaLaTeX")
    (description
     "This package tries to prevent overflow lines in paragraphs or boxes.  It
changes LuaTeX's @code{\\linebreak} callback and re-typesets the paragraph
with increased values of @code{\\tolerance} and @code{\\emergencystretch}
until the overflow no longer happens.  If that doesn't help, it chooses the
solution with the lowest badness.")
    (license license:lppl1.3+)))

(define-public texlive-lineno
  (package
    (name "texlive-lineno")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/lineno/" "tex/latex/lineno/")
             (base32
              "1naqdd62gld0hx6ss0d7sllnbqslzxjcgzj7cnycs303lb03h738")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lineno")
    (synopsis "Line numbers on paragraphs")
    (description
     "The @code{lineno} package adds line numbers to selected paragraphs with
reference possible through the LaTeX @code{\\ref} and @code{\\pageref} cross
reference mechanism.  Line numbering may be extended to footnote lines, using
the @code{fnlineno} package.")
    (license license:lppl1.3a+)))

(define-public texlive-lparse
  (package
    (name "texlive-lparse")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/lparse/" "tex/luatex/lparse/")
             (base32
              "1m75w69qm67j82ja0lp38yckdsbn465aipzdbi2kg4y2xz34hli1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lparse")
    (synopsis "Lua module for parsing key-value options")
    (description
     "@code{lparse} is derived from @code{xparse}, but only works with LuaTeX.
Just as with xparse, it is possible to use a special syntax consisting of
single letters to express the arguments of a macro.  However, @code{lparse} is
able to read arguments regardless of the macro systemd used -- whether LaTeX,
or ConTeXt, or even plain TeX.  Of course, LuaTeX must always be used as the
engine.")
    (license license:lppl1.3c)))

(define-public texlive-lt3luabridge
  (package
    (name "texlive-lt3luabridge")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/lt3luabridge/"
                   "source/generic/lt3luabridge/"
                   "tex/generic/lt3luabridge/")
             (base32
              "00nfi3c545kmm6d9hxwl0m3pawg8322g36gm8sfa3y2p8kah6p8d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lt3luabridge")
    (synopsis "Execute Lua code in any TeX engine that exposes the shell")
    (description
     "This is an expl3(-generic) package for plain TeX, LaTeX, and ConTeXt
that allows you to execute Lua code in LuaTeX or any other TeX engine that
exposes the shell.")
    (license license:lppl1.3c)))

(define-public texlive-lua-typo
  (package
    (name "texlive-lua-typo")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/lua-typo/"
                   "source/lualatex/lua-typo/"
                   "tex/lualatex/lua-typo/")
             (base32
              "0lhjgs7jxdwk2cn9zb99mk2c56awi249zk839waqxnzfr18ky63k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lua-typo")
    (synopsis "Highlighting typographical flaws with LuaLaTeX")
    (description
     "This package tracks common typographic flaws in LuaLaTeX documents,
especially widows, orphans, hyphenated words split over two pages, consecutive
lines ending with hyphens, paragraphs ending on too short lines, etc.
Customisable colours are used to highlight these flaws, and the list of pages
on which typographical flaws were found is printed.")
    (license license:lppl1.3c)))

(define-public texlive-lua-uca
  (package
    (name "texlive-lua-uca")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/support/lua-uca/" "scripts/lua-uca/"
                   "source/support/lua-uca/")
             (base32
              "03wpdaz0z3zqfjs43ar65lx521qyf0c2q62b5p53kdx17xfr09jz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lua-uca")
    (synopsis "Unicode Collation Algorithm library for Lua")
    (description
     "The Lua-UCA library provides basic support for Unicode Collation
Algorithm in Lua.  It can be used to sort arrays of strings according to rules
of particular languages.  It can be used in other Lua projects that need to
sort text in a language dependent way, like indexing processors, bibliographic
generators, etc.")
    (license license:expat)))

(define-public texlive-lua-ul
  (package
    (name "texlive-lua-ul")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/lua-ul/"
                   "source/lualatex/lua-ul/"
                   "tex/lualatex/lua-ul/")
             (base32
              "0xpa41hkjzqcws7yjj6s8ys93qyhnv6zfv7479qbv58h5mgfxzhh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lua-ul")
    (synopsis "Underlining for LuaLaTeX")
    (description
     "This package provides underlining, strikethough, and highlighting using
features in LuaLaTeX which avoid the restrictions imposed by other methods.
In particular, kerning is not affected, the underlined text can use arbitrary
commands, hyphenation works etc.")
    (license license:lppl1.3c)))

(define-public texlive-lua-visual-debug
  (package
    (name "texlive-lua-visual-debug")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/lua-visual-debug/"
                   "tex/luatex/lua-visual-debug/")
             (base32
              "1g4n4xfqbgcja2x3b7yi59nn76jq695yndv90610pakask2k560l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lua-visual-debug")
    (synopsis "Visual debugging with LuaLaTeX")
    (description
     "The package uses Lua code to provide visible indications of boxes,
glues, kerns and penalties in the PDF output.  The package is known to work in
LaTeX and Plain TeX documents.")
    (license license:expat)))

(define-public texlive-lua-widow-control
  (package
    (name "texlive-lua-widow-control")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/lua-widow-control/"
                   "source/luatex/lua-widow-control/"
                   "tex/context/third/lua-widow-control/"
                   "tex/lualatex/lua-widow-control/"
                   "tex/luatex/lua-widow-control/"
                   "tex/optex/lua-widow-control/")
             (base32
              "1c2n0hmf4kgvdfqday9pk673nxmpddg2wr91wkgalvrn7jw82js4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lua-widow-control")
    (synopsis "Automatically remove widows and orphans from any document")
    (description
     "Unmodified TeX has very few ways of preventing widows and orphans.  In
documents with figures, section headings, and equations, TeX can stretch the
vertical glue between items in order to prevent widows and orphans, but many
documents have no figures or headings.  TeX can also shorten the page by
1 line, but this will give each page a different length which can make
a document look uneven.  The typical solution is to strategically insert
@samp{\\looseness=1}, but this requires manual editing every time that the
document is edited.  Lua-widow-control is essentially an automation of the
@code{\\looseness} method: it uses Lua callbacks to find stretchy paragraphs,
then it lengthens them to remove widows and orphans.  Lua-widow-control is
compatible with all LuaTeX and LuaMetaTeX-based formats.")
    ;; Use either license.
    (license (list license:mpl2.0 license:cc-by-sa4.0))))

(define-public texlive-luaaddplot
  (package
    (name "texlive-luaaddplot")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luaaddplot/"
                   "source/luatex/luaaddplot/"
                   "tex/luatex/luaaddplot/")
             (base32
              "0r7n2s8isw3rm0g2l0zljh86ysh1zyfbmypci3kgciad5smjzx15")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaaddplot")
    (synopsis "Extension to @code{pgfplots}' @code{\\addplot} macro")
    (description
     "This package is an extension to @code{pgfplots}.  It extends the
@code{\\addplot} macro by a facility which allows modification of data files
while they are read.  With @code{luaaddplot} it is no longer necessary to
pre-process data files generated by measuring devices with external scripts.")
    (license license:lppl1.3+)))

(define-public texlive-luacas
  (package
    (name "texlive-luacas")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luacas/"
                   "tex/lualatex/luacas/_lib/"
                   "tex/lualatex/luacas/algebra/"
                   "tex/lualatex/luacas/calculus/"
                   "tex/lualatex/luacas/core/binaryoperation/"
                   "tex/lualatex/luacas/test/calculus/"
                   "tex/lualatex/luacas/test/expressions/"
                   "tex/lualatex/luacas/test/polynomials/"
                   "tex/lualatex/luacas/test/rings/")
             (base32
              "03z33kphj85y5w0sdgrgsh1pajbxwjiiir0s972cjn3f8ns6xw9x")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luacas")
    (synopsis "Computer algebra system for users of LuaLaTeX")
    (description
     "This package provides a portable computer algebra system capable of
symbolic computation, written entirely in Lua, designed for use in LuaLaTeX.
Its features are: arbitrary-precision integer and rational arithmetic,
factoring of univariate polynomials over the rationals and finite fields,
number theoretic algorithms, symbolic differentiation and integration, and
more.  The target audience for this package are mathematics students,
instructors, and professionals who would like some ability to perform basic
symbolic computations within LaTeX without the need for laborious and
technical setup.")
    (license license:lppl1.3c)))

(define-public texlive-luacensor
  (package
    (name "texlive-luacensor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luacensor/"
                   "tex/lualatex/luacensor/")
             (base32
              "0nw83zzpb9y4vnm4rjz786nz9fxv1vzlz3dv8fjwxqpxngqws4g8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luacensor")
    (synopsis "Securely redact sensitive information using Lua")
    (description
     "This package provides simple tools for creating redacted contents.  Its
tools are useful for lawyers, workers in sensitive industries, and others who
need to easily produce both unrestricted versions of documents (for limited,
secure release) and restricted versions of documents (for general release).
Redaction is done both by hiding all characters and by slightly varying the
length of strings to prevent jigsaw identification.  It also is friendly to
screen readers by adding alt-text indicating redacted content.")
    (license license:lppl1.3+)))

(define-public texlive-luacolor
  (package
    (name "texlive-luacolor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/luacolor/"
                   "source/latex/luacolor/"
                   "tex/latex/luacolor/")
             (base32
              "0c91m5iq095f04wiy3dfnhyh458d9scww51np88x8islz1l3psbp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luacolor")
    (synopsis "Color support based on LuaTeX's node attributes")
    (description
     "This package implements color support based on LuaTeX's node
attributes.")
    (license license:lppl1.3+)))

(define-public texlive-luacomplex
  (package
    (name "texlive-luacomplex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luacomplex/"
                   "tex/lualatex/luacomplex/")
             (base32
              "0fb75jzvl0gqqyfl6vrfwdpsxjfq7dyhmfbfibg9w6rqxzcd013p")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luacomplex")
    (synopsis "Operations on complex numbers inside LaTeX documents using Lua")
    (description
     "The @code{luacomplex} package is developed to define complex numbers and
perform basic arithmetic on complex numbers in LaTeX.  It also loads the
@code{luamathspackage}.  It provides an easy way to define complex numbers and
perform operations on complex numbers.  There is no particular environment for
performing operations on complex numbers.  The package commands can be used in
any environment (including the mathematics environment).")
    (license license:lppl1.3c)))

(define-public texlive-luagcd
  (package
    (name "texlive-luagcd")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luagcd/"
                   "tex/lualatex/luagcd/")
             (base32
              "17d1am1c4gmbc0z1fllk6kf6b8fmihw4bv8bl0vvwvnk2acj4zj9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luagcd")
    (synopsis "Computation of gcd of integers inside LaTeX using Lua")
    (description
     "Using Lua, the @code{luagcd} package is developed to find the greatest
common divisor (gcd) of integers in LaTeX.  The package provides commands to
obtain step-by-step computation of gcd of two integers by using the Euclidean
algorithm.  In addition, the package has the command to express gcd of two
integers as a linear combination.  The Bezout's Identity can be verified for
any two integers using commands in the package.")
    (license license:lppl1.3c)))

(define-public texlive-luahyphenrules
  (package
    (name "texlive-luahyphenrules")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luahyphenrules/"
                   "tex/lualatex/luahyphenrules/")
             (base32
              "1a23bp4a7nix4lhi4dl6kwxp1bs6dvldxxrx143i1ps95l3clasj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luahyphenrules")
    (synopsis "Loading patterns in LuaLaTeX with @file{language.dat}")
    (description
     "Preloading hyphenation patterns (or hyphen rules) into any format based
upon LuaTeX is not required in LuaTeX and recent releases of Babel don't do it
anyway.  This package is addressed to those who just want to select the
languages and load their patterns by means of @file{language.dat} without
loading @code{babel}.")
    (license license:lppl1.3+)))

(define-public texlive-luaimageembed
  (package
    (name "texlive-luaimageembed")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luaimageembed/"
                   "tex/lualatex/luaimageembed/")
             (base32
              "0nn4vw7hjyjr8an04dd8xmaskf9qn3zjv29dj37pias3pjq7nqi5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaimageembed")
    (synopsis "Embed images as base64-encoded strings")
    (description
     "This package allows to embed images directly as base64-encoded strings
into a LuaLaTeX document.  This can be useful, e.g., to package a document
with images into a single TeX file, or with automatically generated
graphics.")
    (license license:expat)))

(define-public texlive-luaindex
  (package
    (name "texlive-luaindex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luaindex/" "scripts/luaindex/"
                   "source/lualatex/luaindex/"
                   "tex/lualatex/luaindex/")
             (base32
              "0a2xfzrc663jmd2s5vi5p38pwd16mbm5602pxc3kszza8whqlfdn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:tex-format "lualatex"
           #:phases
           #~(modify-phases %standard-phases
               ;; FIXME: Building documentation requires shell-escape to be
               ;; allowed.  I couldn't find a way to generate the package
               ;; only.
               (delete 'build))))
    (home-page "https://ctan.org/pkg/luaindex")
    (synopsis "Create index using LuaLaTeX")
    (description
     "Luaindex provides (yet another) index processor, written in Lua.")
    (license license:lppl1.3+)))

(define-public texlive-luainputenc
  (package
    (name "texlive-luainputenc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luainputenc/"
                   "source/lualatex/luainputenc/"
                   "tex/lualatex/luainputenc/")
             (base32
              "17xglqil5mbv47kjzklp22vvsn29vajf6q0v45af4pd6yk9kcc6w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luainputenc")
    (synopsis "Replacing @code{inputenc} for use in LuaTeX")
    (description
     "LuaTeX operates by default in UTF-8 input; thus LaTeX documents that
need 8-bit character-sets need special treatment.  The package, therefore,
replaces the LaTeX standard @code{inputenc} for use under LuaTeX.  With
a current LuaTeX,the package has the same behaviour with LuaTeX as
@code{inputenc} has under pdfTeX.")
    (license license:public-domain)))

(define-public texlive-luaintro
  (package
    (name "texlive-luaintro")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luaintro/")
             (base32
              "0xab7yymknvhsh4c30xnhrlvk798mbnl9fbf7njqx8mbmnv869bi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaintro")
    (synopsis "Examples from the book ``Einfuhrung in LuaTeX und LuaLaTeX''")
    (description
     "The bundle provides source of all the examples published in the German
book ``Einfuhrung in LuaTeX und LuaLaTeX'', published by Lehmans Media and
DANTE, Berlin.")
    (license license:lppl1.3+)))

(define-public texlive-luakeys
  (package
    (name "texlive-luakeys")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luakeys/" "tex/luatex/luakeys/")
             (base32
              "0nsd9d2rfwbjadnfykhi0c4wj39cscpx496acjcixg1irlxii7z8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luakeys")
    (synopsis "Lua module for parsing key-value options")
    (description
     "This package provides a Lua module that can parse key-value options like
the TeX packages @code{keyval}, @code{kvsetkeys}, @code{kvoptions},
@code{xkeyval}, @code{pgfkeys}, etc.")
    (license license:lppl1.3c)))

(define-public texlive-lualatex-doc
  (package
    (name "texlive-lualatex-doc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/lualatex-doc/"
                   "source/lualatex/lualatex-doc/")
             (base32
              "1nbanlp3qfk0hylih64qnkh5cd1mjd3j99jk36rnbjhd3f840d9q")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lualatex-doc")
    (synopsis "Guide to use of LaTeX with LuaTeX")
    (description
     "The document is a guide to the world of LuaLaTeX.  Coverage supports
both new users and package developers.  Apart from the introductory material,
the document gathers information from several sources, and offers links to
others.")
    (license license:fdl1.3+)))

(define-public texlive-lualatex-truncate
  (package
    (name "texlive-lualatex-truncate")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/lualatex-truncate/"
                   "source/lualatex/lualatex-truncate/"
                   "tex/lualatex/lualatex-truncate/")
             (base32
              "1ash4zy97qr9vl3j0gpf16srhi8ymfz3h9khlp59kyq9c921rcla")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lualatex-truncate")
    (synopsis "Wrapper for using the @code{truncate} package with LuaLaTeX")
    (description
     "This package provides a wrapper for the @code{truncate} package, thus
fixing issues related to LuaTeX's hyphenation algorithm.")
    (license license:lppl1.3c)))

(define-public texlive-lualinalg
  (package
    (name "texlive-lualinalg")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/lualinalg/"
                   "tex/lualatex/lualinalg/")
             (base32
              "0f29ymc5adcg8mh8lhgir6cj6f0kni0i53r6g35yqnd1jw9qr24q")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lualinalg")
    (synopsis "Linear algebra package for LaTeX")
    (description
     "The @code{lualinalg} package is developed to perform operations on
vectors and matrices defined over the field of real or complex numbers inside
LaTeX documents.  It provides flexible ways for defining and displaying
vectors and matrices.  No particular environment of LaTeX is required to use
commands in the package.  The package is written in Lua, and @file{.tex} file
is to be compiled with the LuaLaTeX engine.  It may also save users efforts to
copy vectors and matrices from other software (which may not be in
LaTeX-compatible format) and to use them in a TeX file.  The vectors and
matrices of reasonable size can be handled with ease.  The package can be
modified or extended by writing custom Lua programs.")
    (license license:lppl1.3c)))

(define-public texlive-luamathalign
  (package
    (name "texlive-luamathalign")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luamathalign/"
                   "source/lualatex/luamathalign/"
                   "tex/lualatex/luamathalign/")
             (base32
              "0lj5lc6s30l9k83xrwfjva0v7c9ajv22ybsaz07hadgj17fxqbdn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luamathalign")
    (synopsis "More flexible alignment in @code{amsmath} environments")
    (description
     "This package allows aligning mathematical expressions on points where
directly using @samp{&} is not possible, especially in nested macros or
environments.")
    (license license:lppl1.3c)))

(define-public texlive-luamaths
  (package
    (name "texlive-luamaths")
    (version (number->string %texlive-revision))
    (source (texlive-origin name version
                            (list "doc/lualatex/luamaths/"
                                  "tex/lualatex/luamaths/")
                            (base32
                             "1dswh6jjkl7jbs3f5pvwh30874x4xycvw7n9cbfd5m9wfcv97rw9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luamaths")
    (synopsis
     "Provide standard mathematical operations inside LaTeX documents using Lua")
    (description
     "The @code{luamaths} package is developed to perform standard
mathematical operations inside LaTeX documents using Lua.  It provides an easy
way to perform standard mathematical operations.  There is no particular
environment in the package for performing mathematical operations.  The
package commands can be used in any environment (including the mathematics
environment).")
    (license license:lppl1.3c)))

(define-public texlive-luamodulartables
  (package
    (name "texlive-luamodulartables")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luamodulartables/"
                   "tex/lualatex/luamodulartables/")
             (base32
              "18yc3fincdvfk1zj5m2gah35mzlvg6rbby8m71clnjk9knmzjcxy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luamodulartables")
    (synopsis "Generate modular addition and multiplication tables")
    (description
     "This package is developed to generate modular addition and
multiplication tables for positive integers.  It provides an easy way to
generate modular addition and modular multiplication tables for positive
integers in LaTeX documents.  The commands in the package have optional
arguments for the formatting of tables.  These commands can be used in an
environment similar to a tabular or array environment.  The commands can also
be used with the @code{booktabs} package, which provides nice formatting of
tables in LaTeX.")
    (license license:lppl1.3c)))

(define-public texlive-luamplib
  (package
    (name "texlive-luamplib")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luamplib/"
                   "source/luatex/luamplib/"
                   "tex/luatex/luamplib/")
             (base32
              "1f2r93daddjwmj7fcy5rds2yz0c8qmww7yw93vgwffmm49d9ims5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luamplib")
    (synopsis "Use LuaTeX's built-in MetaPost interpreter")
    (description
     "The package enables the user to specify MetaPost diagrams (which may
include colour specifications from the color or xcolor packages) into
a document, using LuaTeX's built-in MetaPost library.  The facility is only
available in PDF mode.")
    (license license:gpl2)))

(define-public texlive-luaoptions
  (package
    (name "texlive-luaoptions")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luaoptions/"
                   "tex/lualatex/luaoptions/")
             (base32
              "143cjll4dkjb3bp9755c5wsq447mvw8k043mqyi01qskz5wq45yf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaoptions")
    (synopsis "Option handling for LuaLaTeX packages")
    (description
     "This LuaLaTeX package provides extensive support for handling options,
on package level and locally.  It allows the declaration of sets of options,
along with defaults, allowed values and limited type checking.  These options
can be enforced as package options, changed at any point during a document, or
overwritten locally by optional macro arguments.  It is also possible to
instantiate an Options object as an independent Lua object, without linking it
to a package.  Luaoptions can be used to enforce and prepopulate options, or
it can be used to simply handle the parsing of optional @samp{key=value}
arguments into proper Lua tables.")
    (license license:expat)))

(define-public texlive-luapackageloader
  (package
    (name "texlive-luapackageloader")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luapackageloader/"
                   "tex/luatex/luapackageloader/")
             (base32
              "0cf2p4zhfvf7f10pbpp332chj46ajknqgb7a8i16v63ivvmgb8m3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-iftex))
    (home-page "https://ctan.org/pkg/luapackageloader")
    (synopsis "Allow LuaTeX to load external Lua packages")
    (description
     "This package allows LuaTeX to load packages from the default
@code{package.path} and @code{package.cpath} locations.  This could be useful
to load external Lua modules, including modules installed via LuaRocks.")
    (license license:lppl1.3c)))

(define-public texlive-luaprogtable
  (package
    (name "texlive-luaprogtable")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luaprogtable/"
                   "tex/lualatex/luaprogtable/")
             (base32
              "0vx4vsqa777hv4bdw8m7x8hrjg5p9h8d68z0l2ji60h67bziy161")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaprogtable")
    (synopsis "Programmable table interface for LuaLaTeX")
    (description
     "This package allows you to modify a cell based on the contents of other
cells using LaTeX macros.")
    (license license:expat)))

(define-public texlive-luaquotes
  (package
    (name "texlive-luaquotes")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luaquotes/"
                   "tex/lualatex/luaquotes/")
             (base32
              "0lla3bd5prh5ld1p88f8c3mfck6c6jn241fq1fzz0a9vdhfsbpv0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaquotes")
    (synopsis "Smart setting of quotation marks")
    (description
     "This package automatically generates quotation marks and punctuation
depending on the selected language.")
    (license (list license:lppl1.3c license:cc-by-sa3.0))))

(define-public texlive-luarandom
  (package
    (name "texlive-luarandom")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luarandom/"
                   "tex/lualatex/luarandom/")
             (base32
              "1hzq29prnwgvp400d26fdjhh812gwwgmlwb22xhj29s9f0k2g1qy")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luarandom")
    (synopsis "Create lists of random numbers")
    (description
     "This package can create lists of random numbers for any given interval.
It is possible to get lists with or without multiple numbers.  The random
generator will be initialized by the system time.")
    (license license:lppl1.3+)))

(define-public texlive-luaset
  (package
    (name "texlive-luaset")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luaset/"
                   "tex/lualatex/luaset/")
             (base32
              "1xbr7m1fb71xnlc3p4lb2gz10i6bz6f4bbzyfcclggriq7bkv1iq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaset")
    (synopsis "Set Operations inside LaTeX documents using Lua")
    (description
     "The @code{luaset} package is developed to define finite sets and perform
operations on them inside LaTeX documents.  There is no particular environment
in the package for performing set operations.  The package commands can be
used in any environment (including the mathematics environment).  It is
written in Lua, and the @file{.tex} file is to be compiled with the LuaLaTeX
engine.")
    (license license:lppl1.3c)))

(define-public texlive-luatexko
  (package
    (name "texlive-luatexko")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luatexko/"
                   "tex/luatex/luatexko/")
             (base32
              "0pbpi8g2bdakrl3lpb4vmsbvccjzcb4d6y2ivnv0pncn0k5zfmfv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luatexko")
    (synopsis "Typeset Korean with Lua(La)TeX")
    (description
     "This is a Lua(La)TeX macro package that supports typesetting Korean
documents including Old Hangul texts.  As LuaTeX has opened up access to
almost all the hidden routines of TeX engine, users can obtain more beautiful
outcome using this package rather than other Hangul macros operating on other
engines.")
    (license license:lppl1.3c)))

(define-public texlive-luatextra
  (package
    (name "texlive-luatextra")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luatextra/"
                   "source/lualatex/luatextra/"
                   "tex/lualatex/luatextra/")
             (base32
              "1i82x51rcdll5r6x63j5qqahb277a1xdrzpck30xlggwwlns0jdc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luatextra")
    (synopsis "Additional macros for Plain TeX and LaTeX in LuaTeX")
    (description
     "The package provides a coherent extended programming environment for use
with LuaTeX.  It loads packages @code{fontspec}, @code{luatexbase} and
@code{lualibs}, and provides additional user-level features and goodies.")
    (license license:public-domain)))

(define-public texlive-luatruthtable
  (package
    (name "texlive-luatruthtable")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/lualatex/luatruthtable/"
                   "tex/lualatex/luatruthtable/")
             (base32
              "18k0zk9zxfprcjxzkmfb2j1lk4ldk5hxi0ch1sy0n29f06qcl740")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luatruthtable")
    (synopsis "Generate truth tables of boolean values in LuaLaTeX")
    (description
     "This package provides an easy way for generating truth tables of boolean
values in LuaLaTeX. The time required for operations is no issue while
compiling with LuaLaTeX. The package supports nesting of commands for multiple
operations.  It can be modified or extended by writing custom lua programs.
There is no need to install lua on users system as TeX distributions (TeX Live
or MikTeX) come bundled with LuaLaTeX.")
    (license license:lppl1.3c)))

(define-public texlive-luavlna
  (package
    (name "texlive-luavlna")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luavlna/" "tex/luatex/luavlna/")
             (base32
              "1daa8gdkava15vvvfgr63mrq85l2ni0ydh9l17i9hyhw40mgz4bb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luavlna")
    (synopsis
     "Prevent line breaks after single letter words, units, or academic titles")
    (description
     "In some languages, like Czech or Polish, there should be no single
letter words at the end of a line, according to typographical norms.  This
package handles such situations using LuaTeX's callback mechanism.  In doing
this, the package can detect languages used in the text and insert spaces only
in parts of the document where languages requiring this feature are used.
Another feature of this package is the inclusion of non-breakable space after
initials (like in personal names), after or before academic degrees, and
between numbers and units.  The package supports both plain LuaTeX and
LuaLaTeX.")
    (license license:lppl1.3+)))

(define-public texlive-luaxml
  (package
    (name "texlive-luaxml")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/luaxml/" "tex/luatex/luaxml/")
             (base32
              "0j0d9n87rigawqq3xlxk0zi5ry31zqg3p19jmfablvnb9lan3nrc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/luaxml")
    (synopsis "Lua library for reading and serialising XML files")
    (description
     "LuaXML is a pure Lua library for reading and serializing XML files.  The
current release is aimed mainly at support for the @code{odsfile} package.")
    ;; It uses explicitly the same license as Lua.
    (license license:x11)))

(define-public texlive-lutabulartools
  (package
    (name "texlive-lutabulartools")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/lutabulartools/"
                   "tex/luatex/lutabulartools/")
             (base32
              "1p7b1gv86xa09g1big3s88w0c8jiyj2bk2r8wq4n3pqqqr7zm79z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lutabulartools")
    (synopsis "Some useful LuaLaTeX-based tabular tools")
    (description
     "This package provides some useful commands for tabular matter.  It uses
LuaLaTeX and offers the ability to combine the facilities of multirow and
makecell with an easy to use syntax.  It also adds some enhanced rules for the
@code{booktabs} package.")
    (license license:expat)))

(define-public texlive-babel-czech
  (package
    (name "texlive-babel-czech")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-czech/"
                   "source/generic/babel-czech/"
                   "tex/generic/babel-czech/")
             (base32
              "036817g9dv7m0m1576igwv4mjk8b41klkih44zzwjigdgdjpwbn9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-czech")
    (synopsis "Babel support for Czech")
    (description
     "This package provides the language definition file for support of Czech
in @code{babel}.  Some shortcuts are defined, as well as translations to Czech
of standard LaTeX names.")
    (license license:lppl1.3+)))

(define-public texlive-babel-dutch
  (package
    (name "texlive-babel-dutch")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-dutch/"
                   "source/generic/babel-dutch/"
                   "tex/generic/babel-dutch/")
             (base32
              "1iqlhs2zh60n50r69yicxlklxx8msrb8k552j0ffmqf5kh64fpqh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-dutch")
    (synopsis "Babel contributed support for Dutch")
    (description
     "This package provides the language definition file for support of Dutch
in @code{babel}.  It provides all the necessary macros, definitions and
settings to typeset Dutch documents.")
    (license license:lppl1.3c+)))

(define-public texlive-babel-finnish
  (package
    (name "texlive-babel-finnish")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-finnish/"
                   "source/generic/babel-finnish/"
                   "tex/generic/babel-finnish/")
             (base32
              "1zwrbcqjwhy31mks31vlc4kxci67d5cfm53jaikaabkd8q6grq6i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-finnish")
    (synopsis "Babel support for Finnish")
    (description
     "This package provides the language definition file for support of
Finnish in @code{babel}.  It provides all the necessary macros, definitions and
settings to typeset Finnish documents.")
    (license license:lppl1.3c+)))

(define-public texlive-babel-norsk
  (package
    (name "texlive-babel-norsk")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-norsk/"
                   "source/generic/babel-norsk/"
                   "tex/generic/babel-norsk/")
             (base32
              "1zsssgcdllhjk7r58k4rv8bh59nmj091syqj45chvp1i667ndryp")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-norsk")
    (synopsis "Babel support for Norwegian")
    (description
     "The package provides the language definition file for support of
Norwegian in Babel.  Some shortcuts are defined, as well as translations to
Norsk of standard LaTeX names.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-generic-babel-norsk texlive-babel-norsk)

(define-public texlive-babel-danish
  (package
    (name "texlive-babel-danish")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-danish/"
                   "source/generic/babel-danish/"
                   "tex/generic/babel-danish/")
             (base32
              "11fhmj850gahjm3l3rg5pg4l8j9x6mma59vgfpmnd4fkxj5acb0r")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-danish")
    (synopsis "Babel contributed support for Danish")
    (description
       "This package provides the language definition file for support of
Danish in @code{babel}.  It provides all the necessary macros, definitions and
settings to typeset Danish documents.")
      (license license:lppl1.3c+)))

(define-public texlive-babel-polish
  (package
    (name "texlive-babel-polish")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/babel-polish/"
                   "source/generic/babel-polish/"
                   "tex/generic/babel-polish/")
             (base32
              "0j86l23y1rq1raq2n5azza07l7xjkpgw9nhm77pzy8xmifp3nzhb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/babel-polish")
    (synopsis "Babel support for Polish")
    (description
     "This package provides the language definition file for support of Polish
in @code{babel}.  Some shortcuts are defined, as well as translations to
Polish of standard LaTeX names.")
    (license license:lppl1.3+)))

(define-public texlive-mdframed
  (package
    (name "texlive-mdframed")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/mdframed/" "source/latex/mdframed/"
                   "tex/latex/mdframed/")
             (base32
              "1i5rm946wg43rjckxlfhx79zfx5cgd3bxk71206hd1dqkrgpdpa8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mdframed")
    (synopsis "Framed environments that can split at page boundaries")
    (description
     "The @code{mdframed} package develops the facilities of @code{framed} in
providing breakable framed and coloured boxes.  The user may instruct the
package to perform its operations using default LaTeX commands, PStricks or
TikZ.")
    (license license:lppl)))

(define-public texlive-setspace
  (package
    (name "texlive-setspace")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/setspace/" "tex/latex/setspace/")
             (base32
              "0bvspbka1jhpysyhh3sd1vkkm6xjj2ahj0mzv2inzqbrrbydr9gr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/setspace")
    (synopsis "Set space between lines")
    (description
     "The @code{setspace} package provides support for setting the spacing
between lines in a document.  Package options include @code{singlespacing},
@code{onehalfspacing}, and @code{doublespacing}.  Alternatively the spacing
can be changed as required with the @code{\\singlespacing},
@code{\\onehalfspacing}, and @code{\\doublespacing} commands.  Other size
spacings also available.")
    (license license:lppl1.3+)))

(define-public texlive-pgfgantt
  (package
    (name "texlive-pgfgantt")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pgfgantt/" "source/latex/pgfgantt/"
                   "tex/latex/pgfgantt/")
             (base32
              "0bm034iizk4sd7p5x7vkj7v57dc0bf2lswpsb32s4qlg4s7h6jqz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-pgf))
    (home-page "https://ctan.org/pkg/pgfgantt")
    (synopsis "Draw Gantt charts with TikZ")
    (description
     "The @code{pgfgantt} package provides an environment for drawing Gantt charts
that contain various elements (titles, bars, milestones, groups and links).
Several keys customize the appearance of the chart elements.")
    (license license:lppl1.3+)))

(define-public texlive-pdfcolfoot
  (package
    (name "texlive-pdfcolfoot")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pdfcolfoot/"
                   "source/latex/pdfcolfoot/"
                   "tex/latex/pdfcolfoot/")
             (base32
              "0k3fwfyj56zb18fr4yay4bp66nbx3a0nb87i8mh8yxkm7qmnx95l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pdfcolfoot")
    (synopsis "Separate color stack for footnotes with pdfTeX")
    (description
     "Since version 1.40 pdfTeX supports several colour stacks.  This package
uses a separate colour stack for footnotes that can break across pages.")
    (license license:lppl1.3+)))

(define-public texlive-pdflscape
  (package
    (name "texlive-pdflscape")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pdflscape/"
                   "source/latex/pdflscape/"
                   "tex/latex/pdflscape/")
             (base32
              "0l1m97ai3w8lfdfndmcbwyd8sdwpw4wp7zn6c4iqkf8bqwrmqyk8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pdflscape")
    (synopsis "Make landscape pages display as landscape")
    (description
     "The @code{pdflscape} package adds PDF support to the @code{landscape}
environment of package @code{lscape}, by setting the PDF @code{/Rotate} page
attribute.  Pages with this attribute will be displayed in landscape
orientation by conforming PDF viewers.")
    (license license:lppl1.3+)))

(define-public texlive-pdfmanagement-testphase
  (package
    (name "texlive-pdfmanagement-testphase")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pdfmanagement-testphase/"
                   "source/latex/pdfmanagement-testphase/"
                   "tex/latex/pdfmanagement-testphase/")
             (base32
              "0lxnr7xzis376fmkcyk7ghyj1x7m9yvlg98fvvy6vc4b26hf2kyk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pdfmanagement-testphase")
    (synopsis "LaTeX PDF management testphase bundle")
    (description
     "This is a temporary package, which is used during a test phase to load
the new PDF management code of LaTeX.  The new PDF management code offers
backend-independent interfaces to central PDF dictionaries, tools to create
annotations, form Xobjects, to embed files, and to handle PDF standards.  The
code is provided, during a testphase, as an independent package to allow users
and package authors to safely test the code.  At a later stage it will be
integrated into the LaTeX kernel (or in parts into permanent support
packages), and the current testphase bundle will be removed.")
    (license license:lppl1.3c)))

(define-public texlive-pslatex
  (package
    (name "texlive-pslatex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "fonts/map/dvips/pslatex/"
                   "fonts/tfm/public/pslatex/"
                   "fonts/vf/public/pslatex/"
                   "source/latex/pslatex/fontinst/"
                   "source/latex/pslatex/shell/"
                   "tex/latex/pslatex/")
             (base32
              "1jazd3wl614c7nxl89aj7bhdahhq3h6rrs3p5cyzwqmw3b8h2zrl")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pslatex")
    (synopsis "Use PostScript fonts by default")
    (description
     "This package provides a small package that makes LaTeX default to
standard PostScript fonts.  It is basically a merger of the @code{times} and
the (obsolete) @code{mathptm} packages from the @code{psnfss} suite.  You must
have installed standard LaTeX and the @code{psnfss} PostScript fonts to use
this package.  The main novel feature is that the @code{pslatex} package tries
to compensate for the visual differences between the Adobe fonts by scaling
Helvetica by 90%, and condensing Courier (i.e.  scaling horizontally) by 85%.
The package is supplied with a (unix) shell file for a @command{pslatex}
command that allows standard LaTeX documents to be processed, without needing
to edit the file.  Note that current @code{psnfss} uses a different technique
for scaling Helvetica, and treats Courier as a lost cause (there are better
free fixed-width available now, than there were when @code{pslatex} was
designed).  As a result, @code{pslatex} is widely considered obsolete.")
    (license license:lppl)))

(define-public texlive-pspicture
  (package
    (name "texlive-pspicture")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/pspicture/" "dvips/pspicture/"
                   "source/latex/pspicture/"
                   "tex/latex/pspicture/")
             (base32
              "06wlnwyn45873zghnbw16lzmfj98r9il218z84p1ixw6jwkwlkh8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/pspicture")
    (synopsis "PostScript picture support")
    (description
     "This package provides a replacement for LaTeX's picture macros, that
uses PostScript @code{\\special} commands.  The package is now largely
superseded by @code{pict2e}.")
    (license license:lppl)))

(define-public texlive-datetime2
  (package
    (name "texlive-datetime2")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/datetime2/"
                   "source/latex/datetime2/"
                   "tex/latex/datetime2/")
             (base32
              "0yjkpfic1ni4j2g61rrjj5hjyd43shc9c0sg1aivbbsmqh30dn33")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox texlive-tracklang texlive-xkeyval))
    (home-page "https://ctan.org/pkg/datetime2")
    (synopsis "Formats for dates, times and time zones")
    (description
     "The @code{datetime2} package provides commands for formatting dates,
times and time zones and redefines @code{\\today} to use the same formatting
style.  In addition to @code{\\today}, you can also use
@code{\\DTMcurrenttime} (current time) or @code{\\DTMnow} (current date and
time).  Dates and times can be saved for later use.  The accompanying
@code{datetime2-calc} package can be used to convert date-times to UTC+00:00.
Language and regional support is provided by independently maintained and
installed modules.  The @code{datetime2-calc} package uses the
@code{pgfcalendar} package (part of the PGF/TikZ bundle).  This package
replaces @code{datetime.sty}, which is now obsolete.")
    (license license:lppl1.3+)))

(define-public texlive-tracklang
  (package
    (name "texlive-tracklang")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/tracklang/"
                   "source/latex/tracklang/"
                   "tex/generic/tracklang/"
                   "tex/latex/tracklang/")
             (base32
              "0y8kdr5v033dp79fqfdc1jpp1x3lv0yjz5fjd6yk2xxw30lps1s9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tracklang")
    (synopsis "Language and dialect tracker")
    (description
     "The @code{tracklang} package is provided for package developers who want
a simple interface to find out which languages the user has requested through
packages such as @code{babel} or @code{polyglossia}.  This package does not
provide any translations!  Its purpose is simply to track which languages have
been requested by the user.  Generic TeX code is in @file{tracklang.tex} for
non-LaTeX users.")
    (license license:lppl1.3+)))

(define-public texlive-ltablex
  (package
    (name "texlive-ltablex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ltablex/" "tex/latex/ltablex/")
             (base32
              "14lmgj820j6zwj1xnd6ad38kzb9w132kp7sp55cv5bk9vhx3621w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/ltablex")
    (synopsis "Table package extensions")
    (description
     "The @code{ltablex} package modifies the @code{tabularx} environment to
combine the features of the @code{tabularx} package (auto-sized columns in
a fixed-width table) with those of the @code{longtable} package (multi-page
tables).")
    (license license:lppl)))

(define-public texlive-ragged2e
  (package
    (name "texlive-ragged2e")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ragged2e/" "source/latex/ragged2e/"
                   "tex/latex/ragged2e/")
             (base32
              "06wr2x7mgd40wdq3dnjg3rp5p41fqk6lsj28652i6g71rhnga3sc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/ragged2e")
    (synopsis "Alternative versions of ragged-type commands")
    (description
     "The @code{ragged2e} package defines new commands @code{\\Centering},
@code{\\RaggedLeft}, and @code{\\RaggedRight} and new environments
@code{Center}, @code{FlushLeft}, and @code{FlushRight}, which set ragged text
and are easily configurable to allow hyphenation (the corresponding commands
in LaTeX, all of whose names are lower-case, prevent hyphenation
altogether).")
    (license license:lppl1.3c)))

(define-public texlive-rcs
  (package
    (name "texlive-rcs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/rcs/" "source/latex/rcs/"
                   "tex/latex/rcs/")
             (base32
              "0lhb5njyk5y60lgbbggy804qqc0szcybbhndmwblpxhhyxiqvmi5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/rcs")
    (synopsis "Use RCS (revision control system) tags in LaTeX documents")
    (description
     "The @code{rcs} package utilizes the inclusion of RCS supplied data in
LaTeX documents.  In particular, you can easily access values of every RCS
field in your document put the checkin date on the titlepage or put RCS fields
in a footline.  You can also typeset revision logs.  You can also configure
the @code{rcs} package easily to do special things for any keyword.")
    (license license:gpl3+)))

(define-public texlive-refstyle
  (package
    (name "texlive-refstyle")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/refstyle/" "source/latex/refstyle/"
                   "tex/latex/refstyle/")
             (base32
              "0ckfm04kfi67babpn3m99nqj4b9r1fs0ivq5m7yz90mz4lqykhbs")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/refstyle")
    (synopsis "Advanced formatting of cross references")
    (description
     "The package provides a consistent way of producing references throughout
a project.  Enough flexibility is provided to make local changes to a single
reference.  The user can configure their own setup.  The package offers
a direct interface to varioref (for use, for example, in large projects such
as a series of books, or a multivolume thesis written as a series of
documents), and name references from the nameref package may be incorporated
with ease.  For large projects such as a series of books or a multi volume
thesis, written as freestanding documents, a facility is provided to interface
to the @code{xr} package for external document references.")
    (license license:lppl)))

(define-public texlive-relsize
  (package
    (name "texlive-relsize")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/relsize/" "tex/latex/relsize/")
             (base32
              "07g9wqxsh3a9rmfbppaqhyic82a1i1habizaf4hpdi3246w6nnby")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/relsize")
    (synopsis "Set the font size relative to the current font size")
    (description
     "The basic command of the package is @code{\\relsize}, whose argument is
a number of @code{\\magsteps} to change size; from this are defined commands
@code{\\larger}, @code{\\smaller}, @code{\\textlarger}, etc.")
    (license license:public-domain)))

(define-public texlive-everypage
  (package
    (name "texlive-everypage")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/everypage/"
                   "source/latex/everypage/"
                   "tex/latex/everypage/")
             (base32
              "1kw7n7az823sc8gjrd4gjkkak1780yn76zswlnwapxmvl62pv9xk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/everypage")
    (synopsis "Provide hooks to be run on every page of a document")
    (description
     "The package provides hooks to perform actions on every page, or on the
current page.  Specifically, actions are performed after the page is composed,
but before it is shipped, so they can be used to prepare the output page in
tasks like putting watermarks in the background, or in setting the next page
layout, etc.")
    (license license:lppl1.3c)))

(define-public texlive-everysel
  (package
    (name "texlive-everysel")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/everysel/" "source/latex/everysel/"
                   "tex/latex/everysel/")
             (base32
              "0skzm2qsk5vpjxgslclp4pvbbcrrnm1w3df8xfvfq252dyd7w8s5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/everysel")
    (synopsis "Provides hooks into @code{\\selectfont}")
    (description
     "The @code{everysel} package provided hooks whose arguments are executed
just after LaTeX has loaded a new font by means of @code{\\selectfont}.  It
has become obsolete with LaTeX versions 2021/01/05 or newer, since LaTeX now
provides its own hooks to fulfill this task.  For newer versions of LaTeX
@code{everysel} only provides macros using LaTeX's hook management due to
compatibility reasons.")
    (license license:lppl1.3c)))

(define-public texlive-everyshi
  (package
    (name "texlive-everyshi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/everyshi/" "source/latex/everyshi/"
                   "tex/latex/everyshi/")
             (base32
              "11y6xazv1nk0m2hzsainjr8ijn5cff04xfccm6a65hzg7ipggraj")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    ;; Building `texlive-everyshi' requires "latex" format, provided by
    ;; `texlive-latex-bin'.  However, `texlive-everyshi' is also a propagated
    ;; inputs from `texlive-latex-bin'.  To work around this cycle, build
    ;; a temporary "latex.fmt" format file, and use it to build the package.
    ;; At the end of the process, remove that temporary format file.
    (arguments
     (list
      #:texlive-latex-bin? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'create-latex-format
            (lambda* (#:key inputs #:allow-other-keys)
              (apply (assoc-ref %standard-phases 'create-formats)
                     (list #:inputs inputs #:create-formats '("latex")))))
          (replace 'build
            (lambda* (#:key inputs build-targets tex-engine #:allow-other-keys)
              (apply (assoc-ref %standard-phases 'build)
                     (list #:inputs inputs
                           #:build-targets build-targets
                           #:tex-engine tex-engine
                           #:tex-format (string-append (getcwd)
                                                       "/web2c/pdftex/latex")))))
          (add-after 'build 'remove-latex-format
            (lambda _
              (delete-file-recursively "web2c"))))))
    (native-inputs
     (list texlive-l3kernel
           texlive-l3packages
           texlive-latex
           texlive-latexconfig
           texlive-tex-ini-files))
    (home-page "https://ctan.org/pkg/everyshi")
    (synopsis "Take action at every @code{\\shipout}")
    (description
     "The @code{everyshi} package provides hooks into @code{\\sshipout} called
@code{\\EveryShipout} and @code{\\AtNextShipout} analogous to
@code{\\AtBeginDocument}.  With the introduction of the LaTeX hook management
this package became obsolete in 2020 and is only provided for backwards
compatibility.  For current versions of LaTeX it is only mapping the hooks to
the original @code{everyshi} macros.  In case you use an older LaTeX format,
@code{everyshi} will automatically fall back to its old implementation by
loading @code{everyshi-2001-05-15}.")
    (license license:lppl1.3c)))

(define-public texlive-abstract
  (package
    (name "texlive-abstract")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/abstract/" "source/latex/abstract/"
                   "tex/latex/abstract/")
             (base32
              "1axm78qgrpml09pkh252g1hsjx9c2w7mbdrm9rdl4yqh5ppwq4y9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/abstract")
    (synopsis "Control the typesetting of the @code{abstract} environment")
    (description
     "The @code{abstract} package gives you control over the typesetting of
the @code{abstract} environment, and in particular provides for a one column
abstract in a two column paper.")
    (license license:lppl)))

(define-public texlive-breqn
  (package
    (name "texlive-breqn")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/breqn/" "source/latex/breqn/"
                   "tex/latex/breqn/")
             (base32
              "03iyxwcr94f2y7ar7qin5nzjcvmamhblv5lxb97dg6r79mk4wr75")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/breqn")
    (synopsis "Automatic line breaking of displayed equations")
    (description
     "This package provides solutions to a number of common difficulties in
writing displayed equations and getting high-quality output.  The single most
ambitious goal of the package is to support automatic linebreaking of
displayed equations.  The bundle also contains the @code{flexisym} and
@code{mathstyle} packages, which are both designated as support for
@code{breqn}.")
    (license license:lppl1.3+)))

(define-public texlive-breakurl
  (package
    (name "texlive-breakurl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/breakurl/" "source/latex/breakurl/"
                   "tex/latex/breakurl/")
             (base32
              "1lihfrihf1i300sddz09rsn6gj30g299warn88gli9hbrfy6nvw5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/breakurl")
    (synopsis "Line-breakable links in @code{hyperref} for dvips/ps2pdf")
    (description
     "This package provides a command much like @code{hyperref}'s @code{\\url}
that typesets a URL using a typewriter-like font.  However, if the dvips
driver is being used, the original @code{\\url} doesn't allow line breaks in
the middle of the created link: the link comes in one atomic piece.  This
package allows such line breaks in the generated links.")
    (license license:lppl)))

(define-public texlive-comment
  (package
    (name "texlive-comment")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/comment/" "tex/latex/comment/")
             (base32
              "1c1mqziwxyf1bqzpw6ji65n7ypygm3lyknblxmf0c70w0ivw76pa")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/comment")
    (synopsis "Selectively include/exclude portions of text")
    (description "This package provides environments for selectively including
or excluding pieces of text, allowing the user to define new, separately
controlled comment versions.")
    (license license:gpl2+)))

(define-public texlive-datatool
  (package
    (name "texlive-datatool")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "bibtex/bst/datatool/" "doc/latex/datatool/"
                   "source/latex/datatool/" "tex/latex/datatool/")
             (base32
              "13v6g3i9pch63159ibpn1r8dwl6ip4hm4cv4z0jx6nn3x8kds5ks")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-fp texlive-substr texlive-xfor texlive-xkeyval))
    (home-page "https://ctan.org/pkg/datatool")
    (synopsis "Tools to load and manipulate data")
    (description
     "This package provides tools to create databases using LaTeX commands
or by importing external files.  Databases may be sorted, filtered, and
visualized using several kinds of configurable plots.  Particular support is
provided for mail merging, indexing, creating glossaries, manipulating
bibliographies, and displaying personal pronouns.")
    (license license:lppl1.3+)))

(define-public texlive-physics
  (package
    (name "texlive-physics")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/physics/" "tex/latex/physics/")
             (base32
              "1wy58wwcv1pv18xs1n71abnm73dqnxqijxvhfxk0rcmvbc6wvwrb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/physics")
    (synopsis "Macros supporting the Mathematics of Physics")
    (description
     "The package defines simple and flexible macros for typesetting equations
in the languages of vector calculus and linear algebra, using Dirac
notation.")
    (license license:lppl)))

(define-public texlive-sourcesanspro
  (package
    (name "texlive-sourcesanspro")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sourcesanspro/"
                   "fonts/enc/dvips/sourcesanspro/"
                   "fonts/map/dvips/sourcesanspro/"
                   "fonts/opentype/adobe/sourcesanspro/"
                   ;; ^ It would be tempting to use our
                   ;; font-adobe-source-sans-pro for these, but the version in
                   ;; texlive could differ from our version: probably the
                   ;; difference would be small, but debugging would not be
                   ;; fun.  If the files are really identical, Guix will
                   ;; hard-link them anyway.
                   "fonts/tfm/adobe/sourcesanspro/"
                   "fonts/type1/adobe/sourcesanspro/"
                   "fonts/vf/adobe/sourcesanspro/"
                   "tex/latex/sourcesanspro/")
             (base32
              "18z7ln8dyh0sp6v0vdvc6qqxnpg3h3ix0f5magjcjbpay54kl0i3")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/sourcesanspro")
    (synopsis "Use Source Sans Pro with TeX(-alike) systems")
    (description
     "This package provides the Source Sans Pro font family from Adobe in both
Adobe Type 1 and OpenType formats, plus macros supporting the use of the fonts
in LaTeX (Type 1) and XeLaTeX/LuaLaTeX (OTF).")
    (license (list license:lppl1.3+ license:silofl1.1))))

(define-public texlive-sourceserifpro
  (package
    (name "texlive-sourceserifpro")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sourceserifpro/"
                   "fonts/enc/dvips/sourceserifpro/"
                   "fonts/map/dvips/sourceserifpro/"
                   "fonts/opentype/adobe/sourceserifpro/"
                   ;; ^ See comment in `texlive-sourcesanspro'.
                   "fonts/tfm/adobe/sourceserifpro/"
                   "fonts/type1/adobe/sourceserifpro/"
                   "fonts/vf/adobe/sourceserifpro/"
                   "tex/latex/sourceserifpro/")
             (base32
              "18xxncg8ybv86r46zq5mvgkrfnvlhx93n55fy8nkk8vdmminrh8w")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/sourceserifpro")
    (synopsis "Use Source Serif Pro with TeX(-alike) systems")
    (description
     "This package provides the Source Serif Pro font family from Adobe in
both Adobe Type 1 and OpenType formats, plus macros supporting the use of the
fonts in LaTeX (Type 1) and XeLaTeX/LuaLaTeX (OTF).")
    (license (list license:lppl1.3+ license:silofl1.1))))

(define-public texlive-sourcecodepro
  (package
    (name "texlive-sourcecodepro")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/sourcecodepro/"
                   "fonts/enc/dvips/sourcecodepro/"
                   "fonts/map/dvips/sourcecodepro/"
                   "fonts/opentype/adobe/sourcecodepro/"
                   ;; ^ See comment in `texlive-sourcesanspro'.
                   "fonts/tfm/adobe/sourcecodepro/"
                   "fonts/type1/adobe/sourcecodepro/"
                   "fonts/vf/adobe/sourcecodepro/"
                   "tex/latex/sourcecodepro/")
             (base32
              "009v9y7d3vsljgq9nw5yx4kzyqavxbwrlvwhfjj83s6rmb9xcrmh")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/sourcecodepro")
    (synopsis "Use Source Code Pro with TeX(-alike) systems")
    (description "This package provides the Source Code Pro font family from
Adobe in both Adobe Type 1 and OpenType formats, plus macros supporting the
use of the fonts in LaTeX (Type 1) and XeLaTeX/LuaLaTeX (OTF).")
    (license (list license:lppl1.3+ license:silofl1.1))))

(define-public texlive-hyphenat
  (package
    (name "texlive-hyphenat")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hyphenat/" "source/latex/hyphenat/"
                   "tex/latex/hyphenat/")
             (base32
              "0gm7s7bidp9b4sfgylvwydban8jylfcskmqrf0sxlwxsqxqq5fy5")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/hyphenat")
    (synopsis "Disable/enable hypenation")
    (description
     "This package can disable all hyphenation or enable hyphenation of
non-alphabetics or monospaced fonts.  The package can also enable hyphenation
within words that contain non-alphabetic characters (e.g., that include
underscores), and hyphenation of text typeset in monospaced (e.g.,
@code{cmtt}) fonts.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-hyphenat texlive-hyphenat)

(define-public texlive-lastpage
  (package
    (name "texlive-lastpage")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/lastpage/" "source/latex/lastpage/"
                   "tex/latex/lastpage/")
             (base32
              "0rb8kmslmxxr41g9nxmk60m0w0f3428kci42xys21lq4jrdsdz0m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/lastpage")
    (synopsis "Reference last page for Page N of M type footers")
    (description
     "This package enables referencing the number of pages in a LaTeX document
through the introduction of a new label which can be referenced like
@code{\\pageref{LastPage}} to give a reference to the last page of a document.
It is particularly useful in the page footer that says: @samp{Page N of M}.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-lastpage texlive-lastpage)

(define-public texlive-tabto-ltx
  (package
    (name "texlive-tabto-ltx")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/tabto-ltx/" "tex/latex/tabto-ltx/")
             (base32
              "0q0v1pc4hvj71nayfpkj6gfwcbi18s5c526r1k7j4p9m5fcqmbgm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/tabto-ltx")
    (synopsis "Tab to a measured position in the line")
    (description
     "This package provides @code{\\tabto{<length>}}, which moves the
typesetting position to @code{<length>} from the left margin of the paragraph.
If the typesetting position is already further along, @code{\\tabto} starts
a new line; the command @code{\\tabto*} will move position backwards if
necessary, so that previous text may be overwritten.  In addition, the command
@code{\\TabPositions} may be used to define a set of tabbing positions, after
which the command @code{\\tab} advances typesetting position to the next
defined tab stop.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-tabto-ltx texlive-tabto-ltx)

(define-public texlive-soul
  (package
    (name "texlive-soul")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/soul/" "source/generic/soul/"
                   "tex/generic/soul/")
             (base32
              "1q6qd5fcqs9n49jxa77ildvdcdcqpw914m2xn9wggkp4d2kvxzh1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/soul")
    (synopsis "Hyphenation for letterspacing, underlining, and more")
    (description
     "The @code{soul} package enables hyphenatable spacing out (letterspacing),
underlining, striking out, etc., using the TeX hyphenation algorithm to find
the proper hyphens automatically.  The package also provides a mechanism that
can be used to implement similar tasks, that have to treat text syllable by
syllable.  This version is a merge of the original @code{soul} package and the
@code{soulutf8} package and supports also UTF-8.")
    (license license:lppl)))

(define-deprecated-package texlive-generic-soul texlive-soul)
(define-deprecated-package texlive-soulutf8 texlive-soul)

(define-public texlive-xstring
  (package
    (name "texlive-xstring")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/xstring/" "tex/generic/xstring/")
             (base32
              "1sm84z6nlxipv10ydaww5yl4l2c31hznx3vzzqzaw1gi2yi2d6bb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xstring")
    (synopsis "String manipulation for (La)TeX")
    (description
     "The @code{xstring} package provides macros for manipulating strings, i.e.,
testing a string's contents, extracting substrings, substitution of substrings
and providing numbers such as string length, position of, or number of
recurrences of, a substring.  The package works equally in Plain TeX and
LaTeX (though e-TeX is always required).  The strings to be processed may
contain (expandable) macros.")
    (license license:lppl1.3c)))

(define-deprecated texlive-generic-xstring texlive-xstring)

(define-public texlive-substr
  (package
    (name "texlive-substr")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/substr/" "tex/latex/substr/")
             (base32
              "0kfd4kq5yrg65f2mpric1cs1xr416wgj9bdixpibgjsdg5fb73sw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/substr")
    (synopsis "Deal with substrings in strings")
    (description
     "The package provides commands to deal with substrings of strings.
Macros are provided to determine if one string is a substring of another,
return the parts of a string before or after a substring, and count the number
of occurrences of a substring.")
    (license license:lppl1.0+)))

(define-public texlive-totcount
  (package
    (name "texlive-totcount")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/totcount/" "source/latex/totcount/"
                   "tex/latex/totcount/")
             (base32
              "1rj9ncip5h2cbdljjqwxqsg14pb4mimzhz290q872n32w7rxkp28")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/totcount")
    (synopsis "Find the last value of a counter")
    (description
     "This package records the value that was last set, for any counter of
interest.  Since most such counters are simply incremented when they are
changed, the recorded value will usually be the maximum value.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-totcount texlive-totcount)

(define-public texlive-totpages
  (package
    (name "texlive-totpages")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/totpages/" "source/latex/totpages/"
                   "tex/latex/totpages/")
             (base32
              "1mmya2fqdskyavw3hvdiygfyp9cll7bl4lpi7pl2jf9s7ds49j5a")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:tex-format "latex"))
    (home-page "https://ctan.org/pkg/totpages")
    (synopsis "Count pages in a document, and report last page number")
    (description
     "The package counts the actual pages in the document (as opposed to
reporting the number of the last page, as does @code{lastpage}).  The counter
itself may be shipped out to the DVI file.")
    (license license:lppl)))

(define-public texlive-xdvi
  (package
    (name "texlive-xdvi")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/man/man1/xdvi.1"
                   "doc/man/man1/xdvi.man1.pdf" "dvips/xdvi/"
                   "xdvi/")
             (base32
              "1iidl3876vyi9k2dyfwd73q5kb53kwckivfyvvxh953n4axbqmi4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xdvi")
    (synopsis "DVI previewer for the X Window System")
    (description
     "Xdvi is the canonical DVI previewer for use on Unix and other X-windows
based systems.  The distribution has been integrated with that of Xdvik, so
that it will build with web2c out of the box.")
    ;; Xdvi is under MIT terms, whereas Xdvik extensions use BS2-2.
    (license (list license:expat license:bsd-2))))

(define-public texlive-xetexconfig
  (package
    (name "texlive-xetexconfig")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/xelatex/xetexconfig/")
             (base32
              "1253njshiwdayzq0xir9cmbi8syhjb3sc4pyrw9p6kzyqvckkcxm")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xetexconfig")
    (synopsis "@file{crop.cfg} for XeLaTeX")
    (description "The @file{crop.cfg} file attempts to persuade
@file{crop.sty} to work with XeTeX.")
    (license license:public-domain)))

(define-public texlive-xetex
  (package
    (name "texlive-xetex")
    (version (number->string %texlive-revision))
    (source  (texlive-origin
              name version
              (list "doc/man/man1/xelatex-unsafe.1"
                    "doc/man/man1/xelatex-unsafe.man1.pdf"
                    "doc/man/man1/xelatex.1"
                    "doc/man/man1/xelatex.man1.pdf"
                    "doc/man/man1/xetex-unsafe.1"
                    "doc/man/man1/xetex-unsafe.man1.pdf"
                    "doc/man/man1/xetex.1"
                    "doc/man/man1/xetex.man1.pdf"
                    "doc/xetex/base/"
                    "fonts/misc/xetex/fontmapping/base/"
                    "scripts/texlive-extra/xelatex-unsafe.sh"
                    "scripts/texlive-extra/xetex-unsafe.sh")
              (base32
               "1fc1b3pmzg6g80jnl7ixqbk79wd6frf477nvgxs1sf56vf9r3vjw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:link-scripts #~(list "xelatex-unsafe.sh" "xetex-unsafe.sh")
           #:create-formats #~(list "xelatex" "xetex")))
    (propagated-inputs
     (list texlive-atbegshi
           texlive-atveryend
           texlive-babel
           texlive-cm
           texlive-dvipdfmx
           texlive-etex
           texlive-everyshi
           texlive-firstaid
           texlive-hyphen-base
           texlive-l3backend
           texlive-l3kernel
           texlive-l3packages
           texlive-latex
           texlive-latex-fonts
           texlive-lm
           texlive-plain
           texlive-tex-ini-files
           texlive-unicode-data
           texlive-xetexconfig))
    (home-page "https://ctan.org/pkg/xetex")
    (synopsis "Extended variant of TeX for use with Unicode sources")
    (description
     "XeTeX is a TeX typesetting engine using Unicode and supporting modern font
technologies such as OpenType, TrueType or Apple Advanced Typography (AAT),
including OpenType mathematics fonts.  XeTeX supports many extensions that
reflect its origins in linguistic research; it also supports micro-typography
(as available in pdfTeX).  XeTeX was developed by the SIL (the first version
was specifically developed for those studying linguistics, and using Macintosh
computers).  XeTeX's immediate output is an extended variant of DVI format,
which is ordinarily processed by a tightly bound processor (called
@code{xdvipdfmx}), that produces PDF.")
    (license license:x11)))

(define-public texlive-xfor
  (package
    (name "texlive-xfor")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/xfor/"
                   "source/latex/xfor/"
                   "tex/latex/xfor/")
             (base32
              "1j241j8sixqkaj2xmcvmrfwm1sdw6xdssnzml1bjs54rqzyh768a")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/xfor")
    (synopsis "Reimplementation of the LaTeX @code{for-loop} macro")
    (description
     "The package redefines the LaTeX internal @code{\\@@for} macro so that
the loop may be prematurely terminated.  The action is akin to the C/Java
break statement, except that the loop does not terminate until the end of the
current iteration.")
    (license license:lppl)))

(define-public texlive-scheme-basic
  (package
    (name "texlive-scheme-basic")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs (list texlive-collection-basic texlive-collection-latex))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Basic scheme (plain and latex)")
    (description
     "This is the basic TeX Live scheme: it is a small set of files sufficient
to typeset plain TeX or LaTeX documents in PostScript or PDF, using the
Computer Modern fonts.  This scheme corresponds to @code{collection-basic} and
@code{collection-latex}.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-deprecated-package texlive-base texlive-scheme-basic)

(define-public texlive-scheme-bookpub
  (package
    (name "texlive-scheme-bookpub")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-barcodes
           texlive-biber
           texlive-biblatex
           texlive-bookcover
           texlive-caption
           texlive-collection-basic
           texlive-collection-latex
           texlive-enumitem
           texlive-fontspec
           texlive-latexmk
           texlive-lipsum
           texlive-listings
           texlive-markdown
           texlive-memoir
           texlive-microtype
           texlive-minted
           texlive-novel
           texlive-octavo
           texlive-pdfpages
           texlive-pgf
           texlive-qrcode
           texlive-shapes
           texlive-titlesec
           texlive-tocloft
           texlive-tufte-latex
           texlive-willowtreebook))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Book publishing scheme (core LaTeX and add-ons)")
    (description
     "This is a book publishing scheme, containing core (Lua)LaTeX and
selected additional packages likely to be useful for non-technical book
publication.  It does not contain additional fonts (different books need
different fonts, and the packages are large), nor does it contain additional
mathematical or other technical packages.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-scheme-context
  (package
    (name "texlive-scheme-context")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs
     (list texlive-antt
           texlive-asana-math
           texlive-ccicons
           texlive-collection-context
           texlive-collection-metapost
           texlive-dejavu
           texlive-eulervm
           texlive-gentium-tug
           texlive-iwona
           texlive-kurier
           texlive-ly1
           texlive-manfnt-font
           texlive-marvosym
           texlive-mflogo-font
           texlive-poltawski
           texlive-pxfonts
           texlive-tex-gyre
           texlive-tex-gyre-math
           texlive-txfonts
           texlive-wasy
           texlive-xits))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "ConTeXt scheme")
    (description "This is the TeX Live scheme for installing ConTeXt.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

(define-public texlive-scheme-minimal
  (package
    (name "texlive-scheme-minimal")
    (version (number->string %texlive-revision))
    (source #f)
    (build-system trivial-build-system)
    (arguments (list #:builder #~(mkdir #$output)))
    (propagated-inputs (list texlive-collection-basic))
    (home-page "https://www.tug.org/texlive/")
    (synopsis "Minimal scheme (plain only)")
    (description
     "This is the minimal TeX Live scheme, with support for only plain
TeX. (No LaTeX macros.) LuaTeX is included because Lua scripts are used in TeX
Live infrastructure.  This scheme corresponds exactly to
@code{collection-basic}.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
