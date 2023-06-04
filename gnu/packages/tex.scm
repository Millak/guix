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

(define hyph-utf8-scripts
  (origin
    (method svn-fetch)
    (uri (texlive-ref "generic" "hyph-utf8"))
    (file-name (string-append "hyph-utf8-scripts-"
                              (number->string %texlive-revision)
                              "-checkout"))
    (patches (search-patches "texlive-hyph-utf8-no-byebug.patch"))
    (sha256
     (base32
      "04xzf5gr3ylyh3ls09imrx4mwq3qp1k97r9njzlan6hlff875rx2"))))

(define (texlive-hyphen-package name code locations hash)
  "Return a TeX Live hyphenation package with the given NAME, using source
files from LOCATIONS with expected checksum HASH.  CODE is not currently in
use."
  (package
    (name name)
    (version (number->string %texlive-revision))
    (source (texlive-origin name version location hash))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((root (string-append #$output "/share/texmf-dist"))
                     (patterns
                      (string-append root "/tex/generic/hyph-utf8/patterns/txt/"))
                     (loaders
                      (string-append root "/tex/generic/hyph-utf8/loadhyph"))
                     (ptex
                      (string-append root "/tex/generic/hyph-utf8/patterns/ptex"))
                     (quote
                      (string-append root "/tex/generic/hyph-utf8/patterns/quote")))
                (mkdir "scripts")
                (copy-recursively
                 (dirname (search-input-file inputs "hyph-utf8.rb"))
                 "scripts")

                ;; Prepare target directories
                (mkdir-p patterns)
                (mkdir-p loaders)
                (mkdir-p ptex)
                (mkdir-p quote)

                ;; Generate plain patterns
                (with-directory-excursion "scripts"
                  (substitute* "lib/tex/hyphen/path.rb"
                    (("^([[:blank:]]+)TeXROOT = .*" _ indent)
                     (string-append indent "TeXROOT = \""
                                    (getcwd) "/..\"\n")))

                  (substitute* "generate-plain-patterns.rb"
                    ;; Ruby 2 does not need this.
                    (("require 'unicode'") "")
                    ;; Write directly to the output directory
                    (("File\\.join\\(PATH::TXT")
                     (string-append "File.join(\"" patterns "\""))
                    (("File\\.join\\(PATH::QUOTE")
                     (string-append "File.join(\"" quote "\"")))
                  (invoke "ruby" "generate-plain-patterns.rb")

                  ;; Build pattern loaders
                  (substitute* "generate-pattern-loaders.rb"
                    (("File\\.join\\(PATH::LOADER")
                     (string-append "File.join(\"" loaders "\"")))

                  (invoke "ruby" "generate-pattern-loaders.rb")

                  ;; Build ptex patterns
                  (substitute* "generate-ptex-patterns.rb"
                    (("File\\.join\\(PATH::PTEX")
                     (string-append "File.join(\"" ptex "\"")))
                  (invoke "ruby" "generate-ptex-patterns.rb"))))))))
    (native-inputs
     (list hyph-utf8-scripts ruby-2.7 ruby-hydra-minimal/pinned))
    (home-page "https://ctan.org/pkg/hyph-utf8")
    (synopsis #f)
    (description #f)
    (license #f)))

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
              "1k7rsi1a74xqvbqr7a84fyqj38jan82sz6h8dcxkx5cg3wa43pji")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
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
     (list ruby
           ruby-hydra-minimal
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
            "FSF all permissive license")))))


(define texlive-extra-src
  (origin
    (method url-fetch)
    (uri "ftp://tug.org/historic/systems/texlive/2021/texlive-20210325-extra.tar.xz")
    (sha256 (base32
             "171kg1n9zapw3d2g47d8l0cywa99bl9m54xkqvp9625ks22z78s6"))))

(define-public texlive-bin
  (package
    (name "texlive-bin")
    (version "20210325")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://tug.org/historic/systems/texlive/2021/"
                           "texlive-" version "-source.tar.xz"))
       (sha256
        (base32
         "0jsq1p66l46k2qq0gbqmx25flj2nprsz4wrd1ybn286p11kdkvvs"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        ;; TODO: Unbundle stuff in texk/dvisvgm/dvisvgm-src/libs too.
        '(with-directory-excursion "libs"
           (let ((preserved-directories '("." ".." "lua53" "luajit" "pplib" "xpdf")))
             ;; Delete bundled software, except Lua which cannot easily be
             ;; used as an external dependency, pplib and xpdf which aren't
             ;; supported as system libraries (see m4/kpse-xpdf-flags.m4).
             (for-each delete-file-recursively
                       (scandir "."
                                (lambda (file)
                                  (and (not (member file preserved-directories))
                                       (eq? 'directory (stat:type (stat file))))))))))))
    (build-system gnu-build-system)
    (inputs
     `(("texlive-extra-src" ,texlive-extra-src)
       ("config" ,config)
       ("texlive-scripts"
        ,(origin
           (method svn-fetch)
           (uri (svn-reference
                 (url (string-append "svn://www.tug.org/texlive/tags/"
                                     %texlive-tag "/Master/texmf-dist/"
                                     "/scripts/texlive"))
                 (revision %texlive-revision)))
           (file-name (string-append "texlive-scripts-"
                                     (number->string %texlive-revision)
                                     "-checkout"))
           (sha256
            (base32
             "1jrphfjhmw17rp1yqsl70shmvka3vg0g8841q6zx2lfn48p7vqf3"))))
       ("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("fontforge" ,fontforge)
       ("freetype" ,freetype)
       ("gd" ,gd)
       ("gmp" ,gmp)
       ("ghostscript" ,ghostscript)
       ("graphite2" ,graphite2)
       ("harfbuzz" ,harfbuzz)
       ("icu4c" ,icu4c)
       ("libpaper" ,libpaper)
       ("libpng" ,libpng)
       ("libxaw" ,libxaw)
       ("libxt" ,libxt)
       ("mpfr" ,mpfr)
       ("perl" ,perl)
       ("pixman" ,pixman)
       ("potrace" ,potrace)
       ("python" ,python)
       ("ruby" ,ruby-2.7)
       ("tcsh" ,tcsh)
       ("teckit" ,teckit)
       ("zlib" ,zlib)
       ("zziplib" ,zziplib)))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:out-of-source? #t
       #:configure-flags
       '("--disable-static"
         "--disable-native-texlive-build"
         "--enable-shared"
         "--with-banner-add=/GNU Guix"
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
         ;; LuaJIT is not ported to some architectures yet.
         ,@(if (or (target-ppc64le?)
                   (target-riscv64?))
             '("--disable-luajittex"
               "--disable-luajithbtex"
               "--disable-mfluajit")
             '()))

      ;; Disable tests on some architectures to cope with a failure of
      ;; luajiterr.test.
      ;; XXX FIXME fix luajit properly on these architectures.
      #:tests? ,(let ((s (or (%current-target-system)
                             (%current-system))))
                  (not (or (string-prefix? "aarch64" s)
                           (string-prefix? "mips64" s)
                           (string-prefix? "powerpc64le" s))))

       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-psutils-test
           (lambda _
             ;; This test fails due to a rounding difference with libpaper 1.2:
             ;;   https://github.com/rrthomas/libpaper/issues/23
             ;; Adjust the expected outcome to account for the minute difference.
             (substitute* "texk/psutils/tests/playres.ps"
               (("844\\.647799")
                "844.647797"))))
         (add-after 'unpack 'configure-ghostscript-executable
           ;; ps2eps.pl uses the "gswin32c" ghostscript executable on Windows,
           ;; and the "gs" ghostscript executable on Unix. It detects Unix by
           ;; checking for the existence of the /usr/bin directory. Since
           ;; Guix System does not have /usr/bin, it is also detected as Windows.
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "utils/ps2eps/ps2eps-src/bin/ps2eps.pl"
               (("gswin32c") "gs"))
             (substitute* "texk/texlive/linked_scripts/epstopdf/epstopdf.pl"
               (("\"gs\"")
                (string-append "\"" (assoc-ref inputs "ghostscript") "/bin/gs\"")))))
         (add-after 'unpack 'patch-dvisvgm-build-files
           (lambda _
             ;; XXX: Ghostscript is detected, but HAVE_LIBGS is never set, so
             ;; the appropriate linker flags are not added.
             (substitute* "texk/dvisvgm/configure"
               (("^have_libgs=yes" all)
                (string-append all "\nHAVE_LIBGS=1")))))
         (add-after 'unpack 'disable-failing-test
           (lambda _
             ;; FIXME: This test fails on 32-bit architectures since Glibc 2.28:
             ;; <https://bugzilla.redhat.com/show_bug.cgi?id=1631847>.
             (substitute* "texk/web2c/omegafonts/check.test"
               (("^\\./omfonts -ofm2opl \\$srcdir/tests/check tests/xcheck \\|\\| exit 1")
                "./omfonts -ofm2opl $srcdir/tests/check tests/xcheck || exit 77"))))
         ,@(if (or (target-ppc32?)
                   (target-riscv64?))
             ;; Some mendex tests fail on some architectures.
             `((add-after 'unpack 'skip-mendex-tests
                 (lambda _
                   (substitute* '("texk/mendexk/tests/mendex.test"
                                  "texk/upmendex/tests/upmendex.test")
                     (("srcdir/tests/pprecA-0.ind pprecA-0.ind1 \\|\\| exit 1")
                      "srcdir/tests/pprecA-0.ind pprecA-0.ind1 || exit 77")))))
             '())
         (add-after 'unpack 'unpack-texlive-extra
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "texlive-extra")
             (with-directory-excursion "texlive-extra"
               (apply (assoc-ref %standard-phases 'unpack)
                      (list #:source (assoc-ref inputs "texlive-extra-src"))))))
         (add-after 'unpack-texlive-extra 'unpack-texlive-scripts
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "texlive-scripts")
             (with-directory-excursion "texlive-scripts"
               (apply (assoc-ref %standard-phases 'unpack)
                      (list #:source (assoc-ref inputs "texlive-scripts")))
               ;; Configure the version string for some scripts.
               ;; Normally this would be done by Subversion.
               ;; See <https://issues.guix.gnu.org/43442#15>.
               (for-each (lambda (file)
                           (substitute* file
                             (("\\$Id\\$")
                              (format #f "$Id: ~a ~a ~a nobody $"
                                      file
                                      ,%texlive-revision
                                      ,%texlive-date))
                             (("\\$Revision\\$")
                              (format #f "$Revision: ~a $"
                                      ,%texlive-revision))
                             (("\\$Date\\$")
                              (format #f "$Date: ~a $"
                                      ,%texlive-date))))
                         '("fmtutil.pl"
                           "mktexlsr"
                           "mktexlsr.pl"
                           "mktexmf"
                           "mktexpk"
                           "mktextfm"
                           "tlmgr.pl"
                           "tlmgrgui.pl"
                           "updmap.pl")))))
         (add-after 'unpack-texlive-scripts 'patch-scripts
           (lambda _
             (let* ((scripts (append (find-files "texk/kpathsea" "^mktex")
                                     (find-files "texk/texlive/linked_scripts"
                                                 "\\.sh$")
                                     (find-files "texlive-scripts" "\\.sh$")))
                    (commands '("awk" "basename" "cat" "grep" "mkdir" "rm"
                                "sed" "sort" "uname"))
                    (command-regexp (format #f "\\b(~a)\\b"
                                            (string-join commands "|")))
                    (iso-8859-1-encoded-scripts
                     '("texk/texlive/linked_scripts/texlive-extra/rubibtex.sh"
                       "texk/texlive/linked_scripts/texlive-extra/rumakeindex.sh")))

               (define (substitute-commands scripts)
                 (substitute* scripts
                   ((command-regexp dummy command)
                    (which command))))

               (substitute-commands (lset-difference string= scripts
                                                     iso-8859-1-encoded-scripts))

               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute-commands iso-8859-1-encoded-scripts)))))
         ;; When ST_NLINK_TRICK is set, kpathsea attempts to avoid work when
         ;; searching files by assuming that a directory with exactly two
         ;; links has no subdirectories.  This assumption does not hold in our
         ;; case, so some directories with symlinked subdirectories would not
         ;; be traversed.
         (add-after 'patch-scripts 'patch-directory-traversal
           (lambda _
             (substitute* "texk/kpathsea/config.h"
               (("#define ST_NLINK_TRICK") ""))))

         ,@(if (target-arm32?)
               `((add-after 'unpack 'skip-faulty-test
                   (lambda _
                     ;; Skip this faulty test on armhf-linux:
                     ;;   https://issues.guix.gnu.org/54055
                     (substitute* '("texk/mendexk/tests/mendex.test"
                                    "texk/upmendex/tests/upmendex.test")
                       (("^TEXMFCNF=" all)
                        (string-append "exit 77 # skip\n" all))))))
               '())

         (add-after 'check 'customize-texmf.cnf
           ;; The default texmf.cnf is provided by this package, texlive-bin.
           ;; Every variable of interest is set relatively to the GUIX_TEXMF
           ;; environment variable defined via a search path specification
           ;; further below.  The configuration file is patched after the test
           ;; suite has run, as it relies on the default configuration to find
           ;; its paths (and the GUIX_TEXMF variable isn't set yet).
           (lambda _
             ;; The current directory is build/ because of the out-of-tree
             ;; build.
             (let* ((source    (first (scandir ".." (cut string-suffix?
                                                         "source" <>))))
                    (texmf.cnf (string-append "../" source
                                              "/texk/kpathsea/texmf.cnf")))
               (substitute* texmf.cnf
                 (("^TEXMFROOT = .*")
                  "TEXMFROOT = {$GUIX_TEXMF}/..\n")
                 (("^TEXMF = .*")
                  "TEXMF = {$GUIX_TEXMF}\n")
                 (("^%TEXMFCNF = .*")
                  "TEXMFCNF = {$GUIX_TEXMF}/web2c\n")
                 ;; Don't truncate lines.
                 (("^error_line = .*$") "error_line = 254\n")
                 (("^half_error_line = .*$") "half_error_line = 238\n")
                 (("^max_print_line = .*$") "max_print_line = 1000\n")))))
         (add-after 'install 'post-install
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((out (assoc-ref outputs "out"))
                    (patch-source-shebangs (assoc-ref %standard-phases
                                                      'patch-source-shebangs))
                    (share (string-append out "/share"))
                    (scripts (string-append share
                                            "/texmf-dist/scripts/texlive"))
                    (source (string-append
                             "../" (first (scandir ".." (cut string-suffix?
                                                             "source" <>)))))
                    (tl-extra-root (string-append source "/texlive-extra"))
                    (tl-extra-dir (first
                                   (scandir tl-extra-root
                                            (negate
                                             (cut member <> '("." ".."))))))
                    (tlpkg-src (string-append tl-extra-root "/" tl-extra-dir
                                              "/tlpkg"))
                    (config.guess (search-input-file inputs
                                                     "/bin/config.guess")))

               ;; Create symbolic links for the latex variants and their man
               ;; pages.  We link lualatex to luahbtex; see issue #51252 for
               ;; details.
               (with-directory-excursion (string-append out "/bin/")
                 (for-each symlink
                           '("pdftex" "pdftex"   "xetex"   "luahbtex")
                           '("latex"  "pdflatex" "xelatex" "lualatex")))
               (with-directory-excursion (string-append share "/man/man1/")
                 (symlink "luatex.1" "lualatex.1"))

               ;; Install tlpkg.
               (copy-recursively tlpkg-src (string-append share "/tlpkg"))

               ;; Install texlive-scripts.
               (copy-recursively (string-append
                                  source "/texlive-scripts/source/")
                                 scripts)

               ;; Patch them.
               (let ((dirs (map dirname (list (which "sed") (which "awk")))))
                 (with-directory-excursion scripts
                   (substitute* '("mktexpk" "mktexmf" "mktexlsr")
                     (("^version=" m)
                      (format #false "PATH=\"~{~a:~}$PATH\"; export PATH~%~a"
                              dirs m)))))

               ;; Make sure that fmtutil can find its Perl modules.
               (substitute* (string-append scripts "/fmtutil.pl")
                 (("\\$TEXMFROOT/")
                  (string-append share "/")))

               ;; Likewise for updmap.pl.
               (substitute* (string-append scripts "/updmap.pl")
                 (("\\$TEXMFROOT/tlpkg")
                  (string-append share "/tlpkg")))

               ;; Likewise for the tlmgr.
               (substitute* (string-append scripts "/tlmgr.pl")
                 ((".*\\$::installerdir = \\$Master.*" all)
                  (format #f "  $Master = ~s;~%~a" share all)))

               ;; Install the config.guess script, required by tlmgr.
               (with-directory-excursion share
                 (mkdir-p "tlpkg/installer/")
                 (symlink config.guess "tlpkg/installer/config.guess"))

               ;; texlua shebangs are not patched by the patch-source-shebangs
               ;; phase because the texlua executable does not exist at that
               ;; time.
               (setenv "PATH" (string-append (getenv "PATH") ":" out "/bin"))
               (with-directory-excursion out
                 (patch-source-shebangs))))))))
    (native-search-paths
     (list (search-path-specification
            (variable "GUIX_TEXMF")
            (files '("share/texmf-dist")))))
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
              "0njmxc6l84j44k48qh7d79n3qznzriz2pf8lkj09i7mkkj9fw9lf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-base #f
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
              "0k2b6qi281cl4zml6l537iyps1zwaq7pip81qq8vlkhb9h5ggpnw")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
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

(define-public texlive-bidi
  (package
    (name "texlive-bidi")
    ;; Take the version from texlive-2022.0 as the one from texlive 2021.0 is
    ;; buggy.
    (version "36.4")
    (source (origin
              (method svn-multi-fetch)
              (uri (svn-multi-reference
                    (url (string-append "svn://www.tug.org/texlive/tags/"
                                        "texlive-2022.0/Master/texmf-dist"))
                    (locations (list "doc/xelatex/bidi/"
                                     "source/xelatex/bidi/"
                                     "tex/xelatex/bidi/"))
                    (revision 62885)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1s2p6zp64q6nh8r8hrdx7bbpzj90sq51jbwslh6zj281yx9mv61s"))))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-iftex
           texlive-ltxcmds
           texlive-hyperref
           texlive-xkeyval
           texlive-zref))
    (home-page "https://ctan.org/pkg/bidi")
    (synopsis "Bidirectional typesetting in plain TeX and LaTeX using XeTeX")
    (description "The @code{bidi} package provides a convenient interface for
typesetting bidirectional texts with plain TeX and LaTeX.  The package
includes adaptations for use with many other commonly-used packages.")
    (license license:lppl1.3+)))

(define-public texlive-libkpathsea
  (package/inherit texlive-bin
    (name "texlive-libkpathsea")
    (source
     (origin
       (inherit (package-source texlive-bin))
       (snippet
        `(begin
           ,(origin-snippet (package-source texlive-bin))
           (with-directory-excursion "texk"
             (let ((preserved-directories '("." ".." "kpathsea")))
               (for-each
                delete-file-recursively
                (scandir "."
                         (lambda (file)
                           (and (not (member file preserved-directories))
                                (eq? 'directory (stat:type (stat file)))))))))))))
    (arguments
     (substitute-keyword-arguments (package-arguments texlive-bin)
       ((#:configure-flags flags)
        `(cons* "--disable-all-pkgs" "--enable-kpathsea"
                "--enable-shared" ,flags))
       ((#:phases phases)
        `(modify-phases %standard-phases
           (add-after 'install 'post-install
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (with-directory-excursion "texk/kpathsea"
                 (invoke "make" "install"))))))))
    (inputs '())
    (synopsis "Path searching library")
    (description "kpathsea is a library, whose purpose is to return a filename
from a list of user-specified directories similar to how shells look up
executables.  It is maintained as a part of TeX Live.")))

(define-syntax-rule (define-deprecated-package old-name name)
  "Define OLD-NAME as a deprecated package alias for NAME."
  (define-deprecated/public old-name name
    (deprecated-package (symbol->string 'old-name) name)))

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
    (propagated-inputs
     (list texlive-intcalc texlive-infwarerr))
    (home-page "https://ctan.org/pkg/alphalph")
    (synopsis "Convert numbers to letters")
    (description
     "This package provides commands @code{\\alphalph} and @code{\\AlphAlph}.
They are like @code{\\number} but the expansion consists of lowercase and
uppercase letters respectively (1 to a, 26 to z, 27 to aa, 52 to zz, 53 to ba,
702 to zz, 703 to aaa, etc.).  Alphalph's commands can be used as
a replacement for LaTeX's @code{\\@@alph} and @code{\\@@Alph} macros.")
    (license license:lppl1.3c+)))

(define texlive-docstrip
  (package
    (name "texlive-docstrip")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "/tex/latex/base/docstrip.tex")
             (base32
              "1pxbqbia0727vg01xv8451szm55z2w8sb0vv3kf4iqx5ibb6m0d2")))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-base #f))
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
              "1d41zvjsig7sqf2j2m89dnbv3gicpb16r04b4ikps4gabhbky83k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
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

(define-public texlive-hopatch
  (package
    (name "texlive-hopatch")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hopatch/" "source/latex/hopatch/"
                   "tex/latex/hopatch/")
             (base32
              "1yc9pzh8h4caaxii197jzd8wmvj754ymdq5x2hvmn171mxqp4d3v")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-ltxcmds))
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
              "1nad1bqpjsywm49hlv7d75mqvgha3j5vayvkvfhv8wwzgdb3mk84")))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
    (home-page "https://tug.org/texlive/")
    (synopsis "Core hyphenation support files")
    (description "This package includes Knuth's original @file{hyphen.tex},
@file{zerohyph.tex} to disable hyphenation, @file{language.us} which starts
the autogenerated files @file{language.dat} and @file{language.def} (and
default versions of those), etc.")
    (license license:knuth)))

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
              "16qvi1id9qb8l337kl182qkl1di7wf16qbjw5k67x38g3p18qqna")))
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
              "0l4rvnb0m5y9rqibrdlbg3dijdzixjdx0nf69qjncvng5694p48m")))
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
    (arguments (list #:texlive-latex-base #f))
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
              "1zzab3b8h2xsp88jqjr64i7f0yiqzd9rmzyvpgbfpyhd4sdl4fk4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
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
              "033ymwwc6q0v6saq0x2jc20vc94d38hna0vb8cymj3d8irqy97x2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox))
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
     (list #:texlive-latex-base #f))
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
                   "scripts/context/stubs/mswin/"
                   "tex/context/base/mkii/"
                   "tex/generic/context/mptopdf/")
             (base32
              "0gbc6si5i7pgh37lnh5fpgnjs9180vz0nxpj6qkz8z2yb84ikq7l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
                   ;; Extract the sole script expected in the package.
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
              "0fbfhjbp7gxbwsbybbb8gm4l6za17nrm2mx2i2xa66lmpqcjbgg7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
      #:texlive-latex-base #f
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
    (synopsis "URW Base 35 font pack for LaTeX")
    (description "This package provides a drop-in replacements for the Courier
font from Adobe's basic set.")
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
              "0229aa7cgw614zlc2n589fi4hfdfnv7dd83f5mfa358zdb8iw54j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-kvoptions))
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
              "0yyk0dr4yms82mwy4dc03zf5igyhgcb65icdah042rk23rlpxygv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
              "0pv3gvy01zkhk39ybjix5lh3x6q4r9pvabrx1wvqr96ma8gyzr5n")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-minitoc))
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
    (arguments (list #:texlive-latex-base #f))
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
    (arguments (list #:texlive-latex-base #f))
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
     "These fonts were created in METAFONT by Knuth, for his own publications.
At some stage, the letters P and S were added, so that the METAPOST logo could
also be expressed.  The fonts were originally issued (of course) as METAFONT
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
              "0v8i6w5kinh11xm0vmgb75sxb4lxznf3l1j78dq6xh55a79330ha")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/mfware")
    (synopsis "Supporting tools for use with Metafont")
    (description
     "This package provides a collection of programs (as web source) for
processing the output of Metafont.")
    (license license:public-domain)))

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
              "17pvh7i9zw8qa5hr53kci7di64fqzx4j35gsn28s36b74x6xj4bc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
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
      #:texlive-latex-base #f))
    (home-page "https://ctan.org/pkg/plain")
    (synopsis "Plain TeX format and supporting files")
    (description
     "This package contains files used to build the Plain TeX format, as
described in the TeXbook, together with various supporting files (some also
discussed in the book).")
    (license license:knuth)))

(define-deprecated-package texlive-tex-plain texlive-plain)

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
    (propagated-inputs
     (list texlive-ifplatform
           texlive-iftex
           texlive-pdftexcmds))
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
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Helvetica font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-public texlive-hyphen-afrikaans
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-afrikaans" "af"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-af.tex")
              (base32
               "1k9k27a27bbrb0gz36191w32l2v6d3zbdh8zhrp4l3ild2pj3n4l")))
    (synopsis "Hyphenation patterns for Afrikaans")
    (description "The package provides hyphenation patterns for the Afrikaans
language.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-ancientgreek
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-ancientgreek" "grc"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-grc.tex"
                    "/tex/generic/hyphen/grahyph5.tex"
                    "/tex/generic/hyphen/ibyhyph.tex")
              (base32
               "01326lb6z0s8krcfgs8i1pnjfrm4gr33rc53gy80f63qbv4ssxrw")))
    (synopsis "Hyphenation patterns for ancient Greek")
    (description "The package provides hyphenation patterns for ancient
Greek.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-armenian
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-armenian" "hy"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-hy.tex")
                   (base32
                    "0hzny0npynsb07syxrpbfa5pkpj8r0j51pj64yxyfl1c0bak1fwp"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Armenian")
      (description "The package provides hyphenation patterns for the Armenian
language.")
      ;; Any version of the LGPL.
      (license license:lgpl3+))))

(define-public texlive-hyphen-basque
  (let ((template (texlive-hyphen-package
                    "texlive-hyphen-basque" "eu"
                    (list "/tex/generic/hyph-utf8/patterns/tex/hyph-eu.tex")
                    (base32
                     "15w969g1jqzn68l2b2lzf7iv7g3kil02aba3if6cag3qcnq92ra9"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Basque")
      (description "The package provides hyphenation patterns for the Basque
language.")
      ;; Similar to Unicode license.
      (license (license:fsf-free
                "/tex/generic/hyph-utf8/patterns/tex/hyph-eu.tex")))))

(define-public texlive-hyphen-belarusian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-belarusian" "be"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-be.tex")
              (base32
               "0ppm12wndaxv9da62dwkbnk7w9nijikn6jkc97m76xis338g2h02")))
    (synopsis "Hyphenation patterns for Belarusian")
    (description "The package provides hyphenation patterns for the Belarusian
language.")
    (license license:expat)))

(define-public texlive-hyphen-bulgarian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-bulgarian" "bg"
              '("/tex/generic/hyph-utf8/patterns/tex/hyph-bg.tex")
              (base32
               "0m254y71j3qrb71klvfalfmic3kjy31l85b9cgpdm5yznlsq3i8d")))
    (synopsis "Hyphenation patterns for Bulgarian")
    (description "The package provides hyphenation patterns for the Bulgarian
language in T2A and UTF-8 encodings.")
    (license (license:non-copyleft
              "file:///tex/generic/hyph-utf8/patterns/tex/hyph-bg.tex"
              "Ancestral BSD variant"))))

(define-public texlive-hyphen-catalan
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-catalan" "ca"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ca.tex")
              (base32
               "10zzlfz5v8d9csg85ibpp2vfvmpqa56vbl85qy5gws099vygpayg")))
    (synopsis "Hyphenation patterns for Catalan")
    (description "The package provides hyphenation patterns for Catalan in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-chinese
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-chinese" "zh-latn-pinyin"
              '("/tex/generic/hyph-utf8/patterns/ptex/hyph-zh-latn-pinyin.ec.tex"
                "/tex/generic/hyph-utf8/patterns/tex/hyph-zh-latn-pinyin.tex")
              (base32
               "1hhh30hcjymm2igpllly04cavsfmd6xrjkd9zax6b2wdxn3ka4pm")))
    (synopsis "Hyphenation patterns for unaccented Chinese pinyin")
    (description "The package provides hyphenation patterns for unaccented
Chinese pinyin T1/EC and UTF-8 encodings.")
    (license license:gpl2+)))

(define-public texlive-hyphen-churchslavonic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-churchslavonic" "cu"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-cu.tex")
              (base32
               "0fhbwaapq2213msbhgr0d1lw06ihmrqirxj092mn73d8ynl13qlh")))
    (synopsis "Hyphenation patterns for Church Slavonic")
    (description "The package provides hyphenation patterns for Church
Slavonic in UTF-8 encoding.")
    (license license:expat)))

(define-public texlive-hyphen-coptic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-coptic" "cop"
              (list "/tex/generic/hyph-utf8/patterns/tex-8bit/copthyph.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-cop.tex")
              (base32
               "1jlxxvyfa2aljizaa3qlcxyhqsrb4dawv3q3fbyp2lxz6ag9fy6m")))
    (synopsis "Hyphenation patterns for Coptic")
    (description "The package provides hyphenation patterns for Coptic in
UTF-8 encoding as well as in ASCII-based encoding for 8-bit engines.")
    ;; No explicit license declaration, so we use the project license.
    (license license:lppl)))

(define-public texlive-hyphen-croatian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-croatian" "hr"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-hr.tex")
              (base32
               "12n9r2winai15jc622sqdwclgcs1s68r6vcf7ic8vvq0x9qhwc5v")))
    (synopsis "Hyphenation patterns for Croatian")
    (description "The package provides hyphenation patterns for Croatian in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-czech
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-czech" "cs"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-cs.tex")
              (base32
               "1q37s6p8yfyi3rp1azbz421lg4lr4aiki8m631i4x9rmps89m8iq")))
    (synopsis "Hyphenation patterns for Czech")
    (description "The package provides hyphenation patterns for Czech in T1/EC
and UTF-8 encodings.")
    (license license:gpl2+)))

(define-public texlive-hyphen-danish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-danish" "da"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-da.tex")
              (base32
               "1vj8nip64rzcrcg3skm4vqad1ggqwgan74znrdns610wjcm1z9qd")))
    (synopsis "Hyphenation patterns for Danish")
    (description "The package provides hyphenation patterns for Danish in
T1/EC and UTF-8 encodings.")
    ;; Either LPPL 1.3 or later, or Expat
    (license (list license:lppl1.3+ license:expat))))

(define-public texlive-hyphen-dutch
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-dutch" "nl"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-nl.tex")
              (base32
               "1bg9g790ksq5cn8qihai6pacmkp9vpf35h4771z361nvwa40l8yk")))
    (synopsis "Hyphenation patterns for Dutch")
    (description "The package provides hyphenation patterns for Dutch in T1/EC
and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-english
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-english" '("en-gb" "en-us")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-en-gb.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-en-us.tex")
              (base32
               "08b3jihjaamcl1pvffi0s47nwavkm66l9mrrmby3l32dfpkprrc5")))
    (synopsis "Hyphenation patterns for American and British English")
    (description "The package provides additional hyphenation patterns for
American and British English in ASCII encoding.")
    (license (license:non-copyleft
              "file:///tex/generic/hyph-utf8/patterns/tex/hyph-en-us.tex"
              "FSF all permissive license"))))

(define-public texlive-hyphen-esperanto
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-esperanto" "eo"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-eo.tex")
              (base32
               "1503kzn9bk4mm4ba35cka2hm8rz0v3j5l30v5rrsd4rqgpibcgic")))
    (synopsis "Hyphenation patterns for Esperanto")
    (description "The package provides hyphenation patterns for Esperanto ISO
Latin 3 and UTF-8 encodings.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-estonian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-estonian" "et"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-et.tex")
              (base32
               "1rdas2450ib02rwy65i69l86nyc9h15bl07xbbwhmhxfnj8zj4v8")))
    (synopsis "Hyphenation patterns for Estonian")
    (description "The package provides hyphenation patterns for Estonian in
T1/EC and UTF-8 encodings.")
    ;; Dual licensed under either license.
    (license (list license:lppl1.3+ license:expat))))

(define-public texlive-hyphen-ethiopic
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-ethiopic" "mul-ethi"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-mul-ethi.tex")
                   (base32
                    "1b93fc6j4aybh0pgq23hsn1njm6asf7sfz803fbj3ai0whsxd10l"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Ethiopic scripts")
      (description "The package provides hyphenation patterns for languages
written using the Ethiopic script for Unicode engines.  They are not supposed
to be linguistically relevant in all cases and should, for proper typography,
be replaced by files tailored to individual languages.")
      (license license:expat))))

(define-public texlive-hyphen-finnish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-finnish" "fi"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fi.tex")
              (base32
               "1pa8sjs9zvnv1y6dma4s60sf9cr4zrvhxwm6i8cnshm84q16w4bc")))
    (synopsis "Hyphenation patterns for Finnish")
    (description "The package provides hyphenation patterns for Finnish in
T1/EC and UTF-8 encodings.")
    (license license:public-domain)))

(define-public texlive-hyphen-schoolfinnish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-schoolfinnish" "fi-x-school"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fi-x-school.tex")
              (base32
               "1w5n6gaclgifbbnafg32vz3mfaibyldvh4yh1ya3sq9fwfmv035c")))
    (synopsis "Hyphenation patterns for Finnish for school")
    (description "The package provides hyphenation patterns for Finnish for
school in T1/EC and UTF-8 encodings.")
    (license license:public-domain)))

(define-public texlive-hyphen-french
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-french" "fr"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fr.tex")
              (base32
               "0jc3kqys6cxjw8x8pzjln7z78l8s7f5rlyrkv7dzr1kiwnwilk9d")))
    (synopsis "Hyphenation patterns for French")
    (description "The package provides hyphenation patterns for French in
T1/EC and UTF-8 encodings.")
    (license license:expat)))

(define-public texlive-hyphen-friulan
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-friulan" "fur"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-fur.tex")
              (base32
               "1dlnh8slpf50mryxv7zzbx08xp54zkdfs1j7y37ipwbrajvd740f")))
    (synopsis "Hyphenation patterns for Friulan")
    (description "The package provides hyphenation patterns for Friulan in
ASCII encodings.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-galician
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-galician" "gl"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-gl.tex")
                   (base32
                    "13zx2r3nrxdr025g2lxrph0ga6wf7cs8dxixn4fhbl6xr1cx028g"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Galician")
      (description "The package provides hyphenation patterns for Galician in
T1/EC and UTF-8 encodings.")
      (license license:lppl1.3))))

(define-public texlive-hyphen-georgian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-georgian" "ka"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ka.tex")
              (base32
               "0l0hk7ka04fr8x11nnw95x151cxyycy0fph772m3a3p8qk4x9wp7")))
    (synopsis "Hyphenation patterns for Georgian")
    (description "The package provides hyphenation patterns for Georgian in
T8M, T8K, and UTF-8 encodings.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-german
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-german" '("de-1901" "de-1996" "de-ch-1901")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-de-1901.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-de-1996.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-de-ch-1901.tex"
                    "/tex/generic/dehyph/dehyphn.tex"
                    "/tex/generic/dehyph/dehypht.tex"
                    "/tex/generic/dehyph/dehyphtex.tex"
                    "/tex/generic/dehyph/README")
              (base32
               "17cc5hd0fr3ykpgly9nxaiz4sik3kmfn2wyxz1fkdnqqhl3i41a0")))
    (synopsis "Hyphenation patterns for German")
    (description "This package provides hyphenation patterns for German in
T1/EC and UTF-8 encodings, for traditional and reformed spelling, including
Swiss German.")
    ;; The patterns are released under the Expat license; the dehyph* files
    ;; are released under the LPPL version 1 or later.
    (license (list license:expat license:lppl1.0+))))

(define-public texlive-hyphen-greek
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-greek" '("el-monoton" "el-polyton")
              (list "/doc/generic/elhyphen/"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-el-monoton.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-el-polyton.tex"
                    "/tex/generic/hyphen/grmhyph5.tex"
                    "/tex/generic/hyphen/grphyph5.tex")
              (base32
               "1qyr6m1nh6d4wj68616cfxv4wjpiy1w2rlldxlx2ajzba381w3hf")))
    (synopsis "Hyphenation patterns for Greek")
    (description "This package provides hyphenation patterns for Modern Greek
in monotonic and polytonic spelling in LGR and UTF-8 encodings.")
    (license license:lppl)))

(define-public texlive-hyphen-hungarian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-hungarian" "hu"
              (list "/doc/generic/huhyphen/"
                    "/doc/generic/hyph-utf8/languages/hu/"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-hu.tex")
              (base32
               "006d2290lcsqzh9ljansbaj9k52s17zgkw0kpsspn5l7a8n00zcl")))
    (synopsis "Hyphenation patterns for Hungarian")
    (description "This package provides hyphenation patterns for Hungarian in
T1/EC and UTF-8 encodings.")
    ;; Any of these licenses
    (license (list license:gpl2 license:lgpl2.1+ license:mpl1.1))))

(define-public texlive-hyphen-icelandic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-icelandic" "is"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-is.tex")
              (base32
               "1m9xj41csj3ldym09d82zjbd3345sg2z10d8pxpvhgibf97mb66h")))
    (synopsis "Hyphenation patterns for Icelandic")
    (description "This package provides hyphenation patterns for Icelandic in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.2+)))

(define-public texlive-hyphen-indic
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-indic"
              '("as" "bn" "gu" "hi" "kn" "ml" "mr" "or" "pa" "ta" "te")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-as.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-bn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-gu.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-hi.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-kn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-ml.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-mr.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-or.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-pa.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-ta.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-te.tex")
              (base32
               "02d2kcd3lpk95fykjwhzw9s2a1s2w1skz8h2mmszrz979d1xzhpm")))
    (synopsis "Indic hyphenation patterns")
    (description "This package provides hyphenation patterns for Assamese,
Bengali, Gujarati, Hindi, Kannada, Malayalam, Marathi, Oriya, Panjabi, Tamil
and Telugu for Unicode engines.")
    (license license:expat)))

(define-public texlive-hyphen-indonesian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-indonesian" "id"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-id.tex")
              (base32
               "1r62w02rf0i4z0jgij54d16qjbj0zyfwm9dwdkqka76jrivij83q")))
    (synopsis "Indonesian hyphenation patterns")
    (description "This package provides hyphenation patterns for
Indonesian (Bahasa Indonesia) in ASCII encoding.  They are probably also
usable for Malay (Bahasa Melayu).")
    (license license:gpl2)))

(define-public texlive-hyphen-interlingua
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-interlingua" "ia"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ia.tex")
              (base32
               "0a9na20vjnzhgjbicaxay0jk4rm5zg1rjyiswr377mjhd9mx5cg3")))
    (synopsis "Interlingua hyphenation patterns")
    (description "This package provides hyphenation patterns for Interlingua
in ASCII encoding.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-irish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-irish" "ga"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ga.tex")
              (base32
               "1h1l9jzkpsb91nyhz6s6c9jfrbz8jx5ip8vyq3dkz0rl6g960i6b")))
    (synopsis "Irish hyphenation patterns")
    (description "This package provides hyphenation patterns for
Irish (Gaeilge) in T1/EC and UTF-8 encodings.")
    ;; Either of these licenses
    (license (list license:gpl2+ license:expat))))

(define-public texlive-hyphen-italian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-italian" "it"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-it.tex")
              (base32
               "03c7jiqslfxvl3gbdx79hggbvrfi2l4z2bnwxc0na8f8lkp1m787")))
    (synopsis "Italian hyphenation patterns")
    (description "This package provides hyphenation patterns for Italian in
ASCII encoding.  Compliant with the Recommendation UNI 6461 on hyphenation
issued by the Italian Standards Institution (Ente Nazionale di Unificazione
UNI).")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-kurmanji
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-kurmanji" "kmr"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-kmr.tex")
              (base32
               "01ylbsi5wymrdrxr9b28nmjmcj72mdhqr657lwsb6m9aj33c9ql6")))
    (synopsis "Kurmanji hyphenation patterns")
    (description "This package provides hyphenation patterns for
Kurmanji (Northern Kurdish) as spoken in Turkey and by the Kurdish diaspora in
Europe, in T1/EC and UTF-8 encodings.")
    (license license:lppl1.3)))

(define-public texlive-hyphen-latin
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-latin" '("la-x-classic" "la-x-liturgic" "la")
              '("/tex/generic/hyph-utf8/patterns/tex/hyph-la-x-classic.tex"
                "/tex/generic/hyph-utf8/patterns/tex/hyph-la-x-liturgic.tex"
                "/tex/generic/hyph-utf8/patterns/tex/hyph-la.tex"
                "/tex/generic/hyph-utf8/patterns/tex-8bit/hyph-la-x-classic.ec.tex")
              (base32
               "119rf6sk1f639ky6zr9njn21nsxzgfmjci94y26745qs8w08ilkl")))
    (synopsis "Liturgical Latin hyphenation patterns")
    (description "This package provides hyphenation patterns for Latin in
T1/EC and UTF-8 encodings, mainly in modern spelling (u when u is needed and v
when v is needed), medieval spelling with the ligatures @code{\\ae} and
@code{\\oe} and the (uncial) lowercase 'v' written as a 'u' is also supported.
Apparently there is no conflict between the patterns of modern Latin and those
of medieval Latin.  It also includes hyphenation patterns for the Classical
Latin in T1/EC and UTF-8 encodings.  Classical Latin hyphenation patterns are
different from those of 'plain' Latin, the latter being more adapted to modern
Latin.  It also provides hyphenation patterns for the Liturgical Latin in
T1/EC and UTF-8 encodings.")
    ;; Either of these licenses
    (license (list license:lppl1.0+ license:expat))))

(define-public texlive-hyphen-latvian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-latvian" "lv"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-lv.tex")
              (base32
               "00jf8xma4ldz0zpqwma97k9q3j0mqx7qdj6b7baph3n5xgc24aaw")))
    (synopsis "Latvian hyphenation patterns")
    (description "This package provides hyphenation patterns for Latvian in
L7X and UTF-8 encodings.")
    ;; Either of these licenses.
    (license (list license:gpl2 license:lgpl2.1))))

(define-public texlive-hyphen-lithuanian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-lithuanian" "lt"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-lt.tex")
              (base32
               "1kfq7j2ajg6nj952s1ygd520sj9z9kl0bqvd291a36ni2b1frzgd")))
    (synopsis "Lithuanian hyphenation patterns")
    (description "This package provides hyphenation patterns for Lithuanian in
L7X and UTF-8 encodings.")
    ;; "Do ... whatever ... as long as you respect the copyright"; as part of
    ;; the hyph-utf8 package we choose the LPPL license.
    (license license:lppl)))

(define-public texlive-hyphen-macedonian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-macedonian" "mk"
              '("/tex/generic/hyph-utf8/patterns/tex/hyph-mk.tex"
                "/tex/generic/hyph-utf8/patterns/tex-8bit/hyph-mk.macedonian.tex")
              (base32
               "1fv6y8gpic5ciw8cclfxc8h3wr5xir1j0a7shixja1pmdyz7db2b")))
    (synopsis "Macedonian hyphenation patterns")
    (description "This package provides hyphenation patterns for Macedonian.")
    ;; XXX: License just says 'GPL'.  Assume GPL2 since the file predates GPL3.
    (license license:gpl2+)))

(define-public texlive-hyphen-mongolian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-mongolian" '("mn-cyrl-x-lmc" "mn-cyrl")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-mn-cyrl-x-lmc.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-mn-cyrl.tex")
              (base32
               "1y1b91ihrdl9bad3rxlsfjpd9wmyd5zzgci3qv9w8qqk33jxhwya")))
    (synopsis "Mongolian hyphenation patterns in Cyrillic script")
    (description "This package provides hyphenation patterns for Mongolian in
T2A, LMC and UTF-8 encodings.")
    ;; Either of these licenses
    (license (list license:lppl1.3+ license:expat))))

(define-public texlive-hyphen-norwegian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-norwegian" '("nb" "nn" "no")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-nb.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-nn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-no.tex")
              (base32
               "08gbwj64p4fckm199k52yp5lx65h9f4wwdkvl4pv4aa7k370jq9y")))
    (synopsis "Norwegian Bokmal and Nynorsk hyphenation patterns")
    (description "This package provides hyphenation patterns for Norwegian
Bokmal and Nynorsk in T1/EC and UTF-8 encodings.")
    (license (license:non-copyleft
              "/tex/generic/hyph-utf8/patterns/tex/hyph-no.tex"
              "FSF All permissive license"))))

(define-public texlive-hyphen-occitan
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-occitan" "oc"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-oc.tex")
              (base32
               "0vhjbq2nr58vhqwwky3cwx4dqiwjmmfwp81rb65mfpf0m8yypdfg")))
    (synopsis "Occitan hyphenation patterns")
    (description "This package provides hyphenation patterns for Occitan in
T1/EC and UTF-8 encodings.  They are supposed to be valid for all the Occitan
variants spoken and written in the wide area called 'Occitanie' by the French.
It ranges from the Val d'Aran within Catalunya, to the South Western Italian
Alps encompassing the southern half of the French pentagon.")
    (license license:lppl1.0+)))

(define-public texlive-hyphen-pali
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-pali" "pi"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pi.tex")
              (base32
               "1fak853s4ijdqgrnhwymaq1lh8jab3qfyxapdmf6qpg6bqd20kxq")))
    (synopsis "Panjabi hyphenation patterns")
    (description "This package provides hyphenation patterns for Panjabi in
T1/EC encoding.")
    ;; Can be used with either license.
    (license (list license:expat license:lgpl3+ license:gpl3+))))

(define-public texlive-hyphen-piedmontese
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-piedmontese" "pms"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pms.tex")
              (base32
               "0xva3l2gwzkqw1sz64k5g5iprhdyr27w1mv8rxp8x62i5y3aqr1k")))
    (synopsis "Piedmontese hyphenation patterns")
    (description "This package provides hyphenation patterns for Piedmontese
in ASCII encoding.  Compliant with 'Gramatica dla lengua piemonteisa' by
Camillo Brero.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-polish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-polish" "pl"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pl.tex")
              (base32
               "1c22g99isxapv4xjrmsw24hhp1xb83wbgcxyd8j24mxdnizywxzm")))
    (synopsis "Polish hyphenation patterns")
    (description "This package provides hyphenation patterns for Polish in QX
and UTF-8 encodings.")
    ;; No differing license declared, so we choose the project license.
    (license license:lppl)))

(define-public texlive-hyphen-portuguese
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-portuguese" "pt"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-pt.tex")
              (base32
               "00rkjy4p7893zs940bq3s4hp7al0skgxqggj5qfax0bx8karf30b")))
    (synopsis "Portuguese hyphenation patterns")
    (description "This package provides hyphenation patterns for Portuguese in
T1/EC and UTF-8 encodings.")
    (license license:bsd-3)))

(define-public texlive-hyphen-romanian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-romanian" "ro"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ro.tex")
              (base32
               "1ykb5v7ip6p3n34wq8qypfyrap4gg946by5rsl6ab0k5gv6ypsbf")))
    (synopsis "Romanian hyphenation patterns")
    (description "This package provides hyphenation patterns for Romanian in
T1/EC and UTF-8 encodings.")
    ;; No differing license declared, so we choose the project license.
    (license license:lppl)))

(define-public texlive-hyphen-romansh
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-romansh" "rm"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-rm.tex")
              (base32
               "0a1q9p6sp5n6a9w6xhwk03vmkrrmnh2md7g1k4qhnf0dc4h7dy9r")))
    (synopsis "Romansh hyphenation patterns")
    (description "This package provides hyphenation patterns for Romansh in
ASCII encodings.  They are supposed to comply with the rules indicated by the
Lia Rumantscha (Romansh language society).")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-russian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-russian" "ru"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-ru.tex")
              (base32
               "00sy7qh5f8ryxw36fwbyd1yi2hxhv7hmk99yp7dwh73n4mxv6lpl")))
    (synopsis "Russian hyphenation patterns")
    (description "This package provides hyphenation patterns for Russian in
T2A and UTF-8 encodings.")
    (license license:lppl1.2+)))

(define-public texlive-hyphen-sanskrit
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-sanskrit" "sa"
              (list "/doc/generic/hyph-utf8/languages/sa/hyphenmin.txt"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-sa.tex")
              (base32
               "1bkzj8swj4lbswf1vr4pb1jg6dixzs7p8h8zm8s8as52h442aida")))
    (synopsis "Sanskrit hyphenation patterns")
    (description "This package provides hyphenation patterns for Sanskrit and
Prakrit in longdesc transliteration, and in Devanagari, Bengali, Kannada,
Malayalam longdesc and Telugu scripts for Unicode engines.")
    ;; "You may freely use, copy, modify and/or distribute this file."
    (license (license:non-copyleft
              "file:///tex/generic/hyph-utf8/patterns/tex/hyph-sa.tex"))))

(define-public texlive-hyphen-serbian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-serbian" '("sh-cyrl" "sh-latn" "sr-cyrl")
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sh-cyrl.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-sh-latn.tex"
                    "/tex/generic/hyph-utf8/patterns/tex/hyph-sr-cyrl.tex")
              (base32
               "0pwc9z0m5y6acq1vqm0da9akg156jbhxzvsfp2f8bsz5b99y5z45")))
    (synopsis "Serbian hyphenation patterns")
    (description "This package provides hyphenation patterns for Serbian in
T1/EC, T2A and UTF-8 encodings.")
    ;; Any version of the GPL.
    (license license:gpl3+)))

(define-public texlive-hyphen-slovak
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-slovak" "sk"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sk.tex")
              (base32
               "0ppp53bbclp5c8wvx748krvrp5y5053khgkjnnv966a90fvp3vgd")))
    (synopsis "Slovak hyphenation patterns")
    (description "This package provides hyphenation patterns for Slovak in
T1/EC and UTF-8 encodings.")
    (license license:gpl2+)))

(define-public texlive-hyphen-slovenian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-slovenian" "sl"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sl.tex")
              (base32
               "02n8l9yf4hqyhbpsc1n6b2mggy09z6lq4dcb8ndiwawb6h0mp7s4")))
    (synopsis "Slovenian hyphenation patterns")
    (description "This package provides hyphenation patterns for Slovenian in
T1/EC and UTF-8 encodings.")
    ;; Either license
    (license (list license:lppl1.0+ license:expat))))

(define-public texlive-hyphen-spanish
  (package
    ;; The source files "eshyph-make.lua" and "eshyph.src" are provided to
    ;; generate obsolete hyphenation patterns, which aren't included in a
    ;; default TeX Live distribution, so we don't include them either.
    (inherit (texlive-hyphen-package
              "texlive-hyphen-spanish" "es"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-es.tex")
              (base32
               "05lbvjkj304xxghyihk8js0kmg97ddlgijld3bp81bc28h4cav0v")))
    (synopsis "Hyphenation patterns for Spanish")
    (description "The package provides hyphenation patterns for Spanish in
T1/EC and UTF-8 encodings.")
    (license license:expat)))

(define-public texlive-hyphen-swedish
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-swedish" "sv"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-sv.tex")
              (base32
               "1n7incy7n24pix1q2i8c3h7i78zpql5ayhskavlmy6mhd7ayncaw")))
    (synopsis "Swedish hyphenation patterns")
    (description "This package provides hyphenation patterns for Swedish in
T1/EC and UTF-8 encodings.")
    (license license:lppl1.2+)))

(define-public texlive-hyphen-thai
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-thai" "th"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-th.tex")
              (base32
               "00gxcs4jfqifd5cnrjipn77m73fmpw2qms4lp216jj3kz4a7h9kf")))
    (synopsis "Thai hyphenation patterns")
    (description "This package provides hyphenation patterns for Thai in LTH
and UTF-8 encodings.")
    (license license:lppl1.3+)))

(define-public texlive-hyphen-turkish
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-turkish" "tr"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-tr.tex")
                   (base32
                    "04sihjgpm31i5bi67rrfp15w3imn7hxwwk70v0vhx053ghxy72vh"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Turkish")
      (description "The package provides hyphenation patterns for Turkish in
T1/EC and UTF-8 encodings.  The patterns for Turkish were first produced for
the Ottoman Texts Project in 1987 and were suitable for both Modern Turkish
and Ottoman Turkish in Latin script, however the required character set didn't
fit into EC encoding, so support for Ottoman Turkish had to be dropped to keep
compatibility with 8-bit engines.")
      (license license:lppl1.0+))))

(define-public texlive-hyphen-turkmen
  (let ((template (texlive-hyphen-package
                   "texlive-hyphen-turkmen" "tk"
                   (list "/tex/generic/hyph-utf8/patterns/tex/hyph-tk.tex")
                   (base32
                    "0g5ip2lw9g47s61mv3cypswc6qm7zy9c4iqq4h19ysvds81adzkr"))))
    (package
      (inherit template)
      (synopsis "Hyphenation patterns for Turkmen")
      (description "The package provides hyphenation patterns for Turkmen in
T1/EC and UTF-8 encodings.")
      (license license:expat))))

(define-public texlive-hyphen-ukrainian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-ukrainian" "uk"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-uk.tex")
              (base32
               "0fbfhx1fmbshxr4ihsjaqgx251h69h7i288p8gh3w6ysgxr53p60")))
    (synopsis "Ukrainian hyphenation patterns")
    (description "This package provides hyphenation patterns for Ukrainian in
T2A and UTF-8 encodings.")
    ;; No version specified
    (license license:lppl)))

(define-public texlive-hyphen-uppersorbian
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-uppersorbian" "hsb"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-hsb.tex")
              (base32
               "0x0051wph3sqmzzw6prvjy6bp7gn02rbmys1bmbc210jk3pkylfj")))
    (synopsis "Upper Sorbian hyphenation patterns")
    (description "This package provides hyphenation patterns for Upper Sorbian
in T1/EC and UTF-8 encodings.")
    (license license:lppl1.3a+)))

(define-public texlive-hyphen-welsh
  (package
    (inherit (texlive-hyphen-package
              "texlive-hyphen-welsh" "cy"
              (list "/tex/generic/hyph-utf8/patterns/tex/hyph-cy.tex")
              (base32
               "1bpxp3jiifdw7waw2idz5j9xgi3526nkxm8mbmsspr4mlf2xyr76")))
    (synopsis "Welsh hyphenation patterns")
    (description "This package provides hyphenation patterns for Welsh in
T1/EC and UTF-8 encodings.")
    ;; Either license
    (license (list license:lppl1.0+ license:expat))))

(define-public texlive-hyph-utf8
  (package
    (name "texlive-hyph-utf8")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/luatex/hyph-utf8/"
                   "source/generic/hyph-utf8/"
                   "source/generic/hyph-utf8/contributed/"
                   "source/generic/hyph-utf8/data/"
                   "source/luatex/hyph-utf8/"
                   "tex/luatex/hyph-utf8/"
                   ;; Documentation; we can't use the whole directory because
                   ;; it includes files from other packages.
                   "doc/generic/hyph-utf8/CHANGES"
                   "doc/generic/hyph-utf8/HISTORY"
                   "doc/generic/hyph-utf8/hyph-utf8.pdf"
                   "doc/generic/hyph-utf8/hyph-utf8.tex"
                   "doc/generic/hyph-utf8/hyphenation-distribution.pdf"
                   "doc/generic/hyph-utf8/hyphenation-distribution.tex"
                   "doc/generic/hyph-utf8/img/miktex-languages.png"
                   "doc/generic/hyph-utf8/img/texlive-collection.png")
             (base32
              "1dm023k05c0pnnyqgbsy1cbpq8layabdp8acln0v59kpsx7flmj9")))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; there are none
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:make-flags
      #~(list "-C" "source/luatex/hyph-utf8/"
              (string-append "DO_TEX = tex --interaction=nonstopmode '&tex' $<")
              (string-append "RUNDIR =" (assoc-ref %outputs "out") "/share/texmf-dist/tex/luatex/hyph-utf8/")
              (string-append "DOCDIR =" (assoc-ref %outputs "doc") "/share/texmf-dist/doc/luatex/hyph-utf8/")
              ;; hyphen.cfg is neither included nor generated, so let's only build the lua file.
              (string-append "UNPACKED = $(NAME).lua"))
      #:phases
      #~(modify-phases %standard-phases
          ;; TeX isn't usable at this point, so we first need to generate the
          ;; tex.fmt.
          (replace 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Target directories must exist.
              (mkdir-p (string-append (assoc-ref %outputs "out")
                                      "/share/texmf-dist/tex/luatex/hyph-utf8/"))
              (mkdir-p (string-append (assoc-ref %outputs "doc")
                                      "/share/texmf-dist/doc/luatex/hyph-utf8/"))

              ;; We cannot build the documentation because that requires a
              ;; fully functional pdflatex, which depends on this package.
              (substitute* "source/luatex/hyph-utf8/Makefile"
                (("all: .*") "all: $(RUNFILES)\n"))

              ;; Find required fonts for building tex.fmt
              (setenv "TFMFONTS"
                      (string-append (assoc-ref inputs "texlive-cm")
                                     "/share/texmf-dist/fonts/tfm/public/cm:"
                                     (assoc-ref inputs "texlive-knuth-lib")
                                     "/share/texmf-dist/fonts/tfm/public/knuth-lib"))
              ;; ...and find all tex files in this environment.
              (setenv "TEXINPUTS"
                      (string-append
                       (getcwd) ":"
                       (string-join
                        (map (match-lambda ((_ . dir) dir)) inputs)
                        "//:")))

              ;; Generate tex.fmt.
              (let ((where "source/luatex/hyph-utf8"))
                (mkdir-p where)
                (with-directory-excursion where
                  (invoke "tex" "-ini"
                          (string-append (assoc-ref inputs "texlive-plain")
                                         "/share/texmf-dist/tex/plain/config/tex.ini"))))))
          (add-before 'build 'build-loaders-and-converters
            (lambda _
              (let* ((root (string-append #$output "/share/texmf-dist"))
                     (conv
                      (string-append root "/tex/generic/hyph-utf8/conversions")))

                ;; Build converters
                (mkdir-p conv)
                (with-directory-excursion "source/generic/hyph-utf8"
                  (substitute* "generate-converters.rb"
                    (("\\$path_root=File.*")
                     (string-append "$path_root=\"" root "\"\n"))
                    ;; Avoid error with newer Ruby.
                    (("#1\\{%") "#1{%%"))
                  (invoke "ruby" "generate-converters.rb")))))
          (replace 'install
            (lambda* (#:key source #:allow-other-keys)
              (mkdir-p #$output:doc)
              (copy-recursively
               (string-append source "/doc")
               (string-append #$output:doc "/doc"))
              (install-file
               (string-append source "/tex/luatex/hyph-utf8/etex.src")
               (string-append #$output "/share/texmf-dist/tex/luatex/hyph-utf8/")))))))
    (native-inputs
     (list ruby-2.7
           texlive-bin
           ;; The following packages are needed for build "tex.fmt", which we
           ;; need for a working "tex".
           texlive-cm
           texlive-docstrip
           texlive-knuth-lib
           texlive-hyphen-base
           texlive-plain))
    (home-page "https://ctan.org/pkg/hyph-utf8")
    (synopsis "Hyphenation patterns expressed in UTF-8")
    (description "Modern native UTF-8 engines such as XeTeX and LuaTeX need
hyphenation patterns in UTF-8 format, whereas older systems require
hyphenation patterns in the 8-bit encoding of the font in use (such encodings
are codified in the LaTeX scheme with names like OT1, T2A, TS1, OML, LY1,
etc).  The present package offers a collection of conversions of existing
patterns to UTF-8 format, together with converters for use with 8-bit fonts in
older systems.  Since hyphenation patterns for Knuthian-style TeX systems are
only read at iniTeX time, it is hoped that the UTF-8 patterns, with their
converters, will completely supplant the older patterns.")
    ;; Individual files each have their own license.  Most of these files are
    ;; independent hyphenation patterns.
    (license (list license:lppl1.0+
                   license:lppl1.2+
                   license:lppl1.3
                   license:lppl1.3+
                   license:lppl1.3a+
                   license:lgpl2.1
                   license:lgpl2.1+
                   license:lgpl3+
                   license:gpl2+
                   license:gpl3+
                   license:mpl1.1
                   license:asl2.0
                   license:expat
                   license:bsd-3
                   license:cc0
                   license:public-domain
                   license:wtfpl2))))

(define-deprecated-package texlive-generic-hyph-utf8 texlive-hyph-utf8)

(define-public texlive-dehyph
  (package
    (name "texlive-dehyph")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "tex/generic/dehyph/")
             (base32
              "0fkqlsknrlxk8zazcqy4q3nisxr3a4x21aiwqhz8s237rdf3w39g")))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/dehyph")
    (synopsis "German hyphenation patterns for traditional orthography")
    (description
     "The package provides older hyphenation patterns for the German language.
Please note that by default only pdfLaTeX uses these patterns (mainly for
backwards compatibility).  The older packages ghyphen and gnhyph are now
bundled together with dehyph, and are no longer be updated.  Both XeLaTeX and
LuaLaTeX use the current German hyphenation patterns taken from Hyphenation
patterns in UTF-8, and using the Experimental hyphenation patterns for the
German language package it is possible to make pdfLaTeX use the new German
patterns as well.")
    (license license:lppl1.0+)))

(define-public texlive-dehyph-exptl
  (package
    (name "texlive-dehyph-exptl")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/dehyph-exptl/"
                   "tex/generic/dehyph-exptl/")
             (base32
              "0l57a0r4gycp94kz6lrxqvh9m57j2shmbr2laf5zjb0qnrisq46d")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
    (propagated-inputs (list texlive-hyph-utf8 texlive-hyphen-base))
    (home-page "https://ctan.org/pkg/dehyph-exptl")
    (synopsis "Hyphenation patterns for German")
    (description
     "The package provides experimental hyphenation patterns for the German
language, covering both traditional and reformed orthography.  The patterns
can be used with packages Babel and @code{hyphsubst} from the Oberdiek
bundle.")
    ;; Hyphenation patterns are under the Expat license; documentation is
    ;; under LPPL.
    (license (list license:expat license:lppl))))

(define-deprecated-package texlive-generic-dehyph-exptl texlive-dehyph-exptl)

(define-public texlive-ukrhyph
  (package
    (name "texlive-ukrhyph")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/ukrhyph/" "tex/generic/ukrhyph/")
             (base32
              "01ma274sixcrbpb7fpqkxwfvrnzfj2srv9b4a42rfnph1pdql74z")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
    (home-page "https://ctan.org/pkg/ukrhyph")
    (synopsis "Hyphenation Patterns for Ukrainian")
    (description "The package provides a range of hyphenation patterns for
Ukrainian, depending on the encoding of the output font including the standard
T2A.")
    (license license:lppl)))

(define-public texlive-ruhyphen
  (package
    (name "texlive-ruhyphen")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "source/generic/ruhyphen/"
                   "tex/generic/ruhyphen/")
             (base32
              "18n1bqhh8jv765vz3a3fjwffy7m71vhwx9yq8zl0p5j7p72q9qcn")))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
    (home-page "https://ctan.org/pkg/ruhyphen")
    (synopsis "Hyphenation patterns for Russian")
    (description "The package provides a collection of Russian hyphenation
patterns supporting a number of Cyrillic font encodings, including T2,
UCY (Omega Unicode Cyrillic), LCY, LWN (OT2), and koi8-r.")
    (license license:lppl)))

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
              "0wfixvszpmri2j19wbg69fqw2iiqmn7blrbxhq17qddbwinm1dbq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
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
                   "fonts/enc/dvips/kpfonts/"
                   "fonts/map/dvips/kpfonts/"
                   "fonts/tfm/public/kpfonts/"
                   "fonts/type1/public/kpfonts/"
                   "fonts/vf/public/kpfonts/"
                   "source/fonts/kpfonts/"
                   "tex/latex/kpfonts/")
             (base32
              "0inai1p9bbjd5x790nsamakjaj0imvwv21mp9f98dwvdlj58vkqb")))
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
    (arguments (list #:texlive-latex-base #f))
    (home-page "https://ctan.org/pkg/latexconfig")
    (synopsis "Configuration files for LaTeX-related formats")
    (description "The package provides configuration files for LaTeX-related
formats.")
    (license license:lppl)))

(define-public texlive-latex-base
  (package
    (name "texlive-latex-base")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/base/" "makeindex/latex/"
                   "source/latex/base/" "tex/latex/base/")
             (base32
              "0k2b6qi281cl4zml6l537iyps1zwaq7pip81qq8vlkhb9h5ggpnw")))
    (build-system texlive-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:texlive-latex-base #f
      #:tex-engine "tex"
      #:tex-format #f
      #:build-targets #~(list "unpack.ins")
      #:create-formats #~(list "dvilualatex" "latex" "lualatex" "pdflatex")))
    (propagated-inputs
     (list texlive-babel
           texlive-cm
           texlive-etex
           texlive-hyphen-complete
           texlive-knuth-lib
           texlive-kpathsea
           texlive-l3backend
           texlive-l3kernel
           texlive-latex-fonts
           texlive-latexconfig
           texlive-luatex
           texlive-pdftex
           texlive-plain
           texlive-tex-ini-files
           texlive-unicode-data
           ;; TODO: This dependency isn't needed for LaTeX version 2021-06-01
           ;; and later. See:
           ;; <https://tug.org/pipermail/tex-live/2021-June/047180.html>
           texlive-l3packages))
    (home-page "https://www.ctan.org/pkg/latex-base")
    (synopsis "Base sources of LaTeX")
    (description
     "This bundle comprises the source of LaTeX itself, together with several
packages which are considered part of the kernel.  This bundle, together with
the required packages, constitutes what every LaTeX distribution should
contain.")
    (license license:lppl1.3c+)))

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
    (propagated-inputs
     (list texlive-atbegshi texlive-zref))
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
    (propagated-inputs
     (list texlive-graphics
           texlive-hyperref
           texlive-iftex))
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
    (propagated-inputs
     (list texlive-grfext
           texlive-infwarerr
           texlive-kvoptions
           texlive-pdftexcmds))
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
             (list "doc/latex/filehook/"
                   "source/latex/filehook/"
                   "tex/latex/filehook/")
             (base32
              "03dsnv8fn111kn8h2fa281w2jvcdrqag1im6mkkfahvjgl1apk6k")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
    (propagated-inputs
     (list texlive-kvoptions
           texlive-pgf))
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
              "0c7y3hfhsvn3qipkq0g5zl9r6aa7bhjvrafxn0w29rpxgs3mc4jj")))
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
    (propagated-inputs
     (list texlive-adjustbox
           texlive-svn-prov))
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
              "0b66fy06safyrd717rfr476g1gz6nqfv1vqvam7ac2yy0g0djb17")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
              "0prw1zcv4fcj3zg0kyhj0k7ax0530adl60bajzvbv3fi16d7rqlq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics-def texlive-graphics-cfg))
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
                   "source/latex/greek-fontenc/"
                   "tex/latex/greek-fontenc/")
             (base32
              "1ncsvj5mlnkgllrvqdnbkv0qwpv2y7jkq3x2wdmm7d3daqq0ka5h")))
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
    (propagated-inputs
     (list texlive-hopatch))
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
              "1d7108b67fcaf1sgyk43ph18l0z5m35iqg3aahqs1ymzwdfnd3f7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-colortbl
           texlive-hyperref
           texlive-lwarp))
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
              "052k1nygm4msaivn8245n86km4h41knivigw80q58b7rc13s6hrk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atbegshi
           texlive-auxhook
           texlive-bitset
           texlive-cm
           texlive-etexcmds
           texlive-gettitlestring
           texlive-graphics             ;for keyval
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
              "177wx80mc6ipl0ciddnwgjjfg9vqv71y9ql0y69sviplyy533ng7")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-build
            (lambda _
              (delete-file "source/latex/hyperxmp/hyperxmp-stds.tex"))))))
    (propagated-inputs
     (list texlive-atenddvi
           texlive-etoolbox
           texlive-hyperref
           texlive-ifmtarg
           texlive-iftex
           texlive-intcalc
           texlive-kvoptions
           texlive-luacode
           texlive-oberdiek             ;for ifdraft
           texlive-pdfescape
           texlive-stringenc
           texlive-totpages))
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
              "00lp24fckawpy997j7zagsxv89jif40wgjq8fw502v06d225ikp3")))
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
    (propagated-inputs
     (list texlive-l3kernel
           texlive-l3packages))
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
              "0f53b6dlnlrxkzj7h7x750p0489i2gg3isfqn0dlpncpq23w1r36")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atveryend
           texlive-infwarerr
           texlive-kvoptions
           texlive-pdftexcmds
           texlive-uniquecounter))
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
              "04ih7i4v96ggwk4k1mpfx3dzcpi2siqablv93wryg7dk4cks5wkl")))
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
              "0c0ixkcvrlzx6sdj25ak3bx0j65qghf51w66yg5wlnpg08d3awrs")))
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

(define-public texlive-tetex
  (package
    (name "texlive-tetex")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "dvips/tetex/"
                   "fonts/enc/dvips/tetex/"
                   "fonts/map/dvips/tetex/")
             (base32
              "05mf8yqdj2wrc1zm3al2j4aam2wx0ky6a7slxw17pkd1c7rmvjrq")))
    (build-system texlive-build-system)
    (arguments (list #:texlive-latex-base #f))
    (home-page "https://www.ctan.org/pkg/tetex")
    (synopsis "Font maps originally from teTeX")
    (description
     "This package provides font maps that were originally part of the now
obsolete teTeX distributions but are still used at the core of the TeX Live
distribution.")
    (license license:public-domain)))

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
              "1y7wcb2643cfwda86f5zpbbw3hj01rji7r143ln77k8nr1919jj1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-engine "tex"
      #:tex-format #f
      #:texlive-latex-base #f))
    (native-inputs
     (list texlive-docstrip))
    (propagated-inputs
     (list texlive-l3backend))
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
              "18i6aczhj7pvqgdwfgkbmjz7a8xgd5w2jhibrv8khqlvxp62in94")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-engine "tex"
      #:tex-format #f
      #:texlive-latex-base #f))
    (native-inputs
     (list texlive-docstrip))
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
              "1k9zms255qz6i24k74g7wnyrdvshl52jgb198pmg6mj9ajhw9sks")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:build-targets
      '(list "l3keys2e.ins"
             "xparse.ins"
             "xfrac.ins"
             "xfp.ins"
             "xtemplate.ins")
      #:tex-engine "tex"
      #:tex-format #f
      #:texlive-latex-base #f))
    (native-inputs
     (list texlive-docstrip))
    (propagated-inputs
     (list texlive-l3kernel))
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
              "1k999jgdd4a9d20rywl53vzpvl3synqxik1fiskxwzlzibjlibv1")))
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
              "1l6q26590kdr2b24psdwgjw199p3sgk2hh74gq6fd6qircc1z3cy")))
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
              "0xxzy3xnq71z3sbkdq8glgnqydvr9g11ih2jmg68fmn5m145w8pi")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
    (propagated-inputs
     (list texlive-bigfoot              ;for "perpage.sty"
           texlive-etoolbox))
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
    (propagated-inputs
     (list texlive-iftex
           texlive-luatexbase))
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
              "1xfr31rwr7zc6d5bsc3v5lwvcfrg109rzfgvvs69w4xs61j06jcg")))
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
              "0gf60vj9y75a7dlrmpbyqgsa00s1717r6if3lm5ldm41i9fm8ywz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
             (list "doc/luatex/base/graphics/"
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
              "1iskvy3i7kq0m39x7k8bs3w2l9bvqzcyzyfllfqr4rwpk4373k30")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:texlive-latex-base #f
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
    (propagated-inputs
     (list texlive-ctablestack))
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

;; TODO: We should be able to build this from the sources on Github with
;; texlive-l3build, but I haven't been able to get it to work.
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
              "15xhnb4kyzmr11lj0md1d502cqrxyq6zdcq738z9394k6bas377f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-lm texlive-lua-alt-getopt texlive-lualibs))
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
              "0kqrgc1kbrgkw9kflazi5imdj8r2fbj2q44x6may362a6izzk2jq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amsfonts))
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
     "This package provides METAFONT (by Donald Knuth) and Adobe Type 1 (by
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
    (propagated-inputs
     (list texlive-amsfonts
           texlive-amsmath
           texlive-url))
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
              "00gl0b55hg912bmrqkpzn5rfyds7hcbsfwdlsmqishq5gjp6pnr0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
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
              "068g31l2ralz03gsv58j67dm85vy0ad58dvdkh93ws0yzf0rnr5f")))
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
              "0cgn4dq5wnlfh9wddjzxsf7p56pk29lyndg56zg6558y7xf67cw8")))
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
              "0a1dcpdgnzf08cd1b9ihdk4229aw19ar0f5sfjr44fqqwkav3l5i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-engine "tex"
      #:tex-format #f
      #:texlive-latex-base #f))
    (native-inputs
     (list texlive-docstrip))
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
    (propagated-inputs
     (list texlive-amsfonts
           texlive-graphics
           texlive-hyperref
           texlive-marvosym
           texlive-psnfss
           texlive-tipa
           texlive-times
           texlive-tools
           texlive-ulem
           texlive-url))
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
    (propagated-inputs
     (list texlive-graphics))
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

;; For user profiles
(define-public texlive-base
  (let ((default-packages
          (list texlive-bin
                texlive-cm
                texlive-cm-super        ; to avoid bitmap fonts
                texlive-dvips
                texlive-fontname
                texlive-graphics
                texlive-kpathsea        ;for mktex.opt
                texlive-latex-base
                texlive-latex-fonts
                texlive-metafont
                ;; LaTeX packages from the "required" set.
                texlive-amsmath
                texlive-amscls
                texlive-babel
                texlive-babel-english
                texlive-cyrillic
                texlive-psnfss
                texlive-tools
                texlive-tetex)))
    (package
      (name "texlive-base")
      (version (number->string %texlive-revision))
      (source #f)
      (build-system trivial-build-system)
      (arguments
       '(#:builder
         (begin (mkdir (assoc-ref %outputs "out")))))
      (propagated-inputs
       (map (lambda (package)
              (list (package-name package) package))
            default-packages))
      (home-page (package-home-page texlive-bin))
      (synopsis "TeX Live base packages")
      (description "This is a very limited subset of the TeX Live distribution.
It includes little more than the required set of LaTeX packages.")
      (license (fold (lambda (package result)
                       (match (package-license package)
                         ((lst ...)
                          (append lst result))
                         ((? license:license? license)
                          (cons license result))))
                     '()
                     default-packages)))))

(define-public texlive-default-updmap.cfg
  (origin
    (method url-fetch)
    (uri (string-append "https://tug.org/svn/texlive/tags/"
                        %texlive-tag "/Master/texmf-dist/web2c/updmap.cfg"
                        "?revision=" (number->string %texlive-revision)))
    (file-name (string-append "updmap.cfg-"
                              (number->string %texlive-revision)))
    (sha256
     (base32
      "0zhpyld702im6352fwp41f2hgfkpj2b4j1kfsjqbkijlcmvb6w2c"))))

;;; TODO: Add a TeX Live profile hook computing fonts maps (and others?)
;;; configuration from the packages in the profile, similar to what's done
;;; below.
(define-public texlive-updmap.cfg
  (lambda* (#:optional (packages '()))
    "Return a 'texlive-updmap.cfg' package which contains the fonts map
configuration of a base set of packages plus PACKAGES."
    (let ((default-packages (match (package-propagated-inputs texlive-base)
                              (((labels packages) ...) packages))))
      (package
        (version (number->string %texlive-revision))
        (source (origin
                  (method url-fetch)
                  (uri (string-append "https://tug.org/svn/texlive/tags/"
                                      %texlive-tag
                                      "/Master/texmf-dist/web2c/updmap.cfg"
                                      "?revision=" version))
                  (file-name "updmap.cfg")
                  (sha256
                   (base32
                    "0zhpyld702im6352fwp41f2hgfkpj2b4j1kfsjqbkijlcmvb6w2c"))))
        (name "texlive-updmap.cfg")
        (build-system copy-build-system)
        (arguments
         '(#:modules ((guix build copy-build-system)
                      (guix build utils)
                      (ice-9 popen)
                      (ice-9 textual-ports))
           #:install-plan '(("updmap.cfg" "share/texmf-config/web2c/")
                            ("map" "share/texmf-dist/fonts/map"))
           #:phases
           (modify-phases %standard-phases
             (add-before 'install 'regenerate-updmap.cfg
               (lambda _
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
                         "--pdftexoutputdir"   "map/pdftex/updmap/"))))))
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
    (propagated-inputs
     (list texlive-hyperref texlive-url))
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
    (propagated-inputs
     (list texlive-etex texlive-ncctools))
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
    (propagated-inputs
     (list texlive-tools))
    (home-page "https://ctan.org/pkg/blindtext")
    (synopsis "Producing 'blind' text for testing")
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
    (propagated-inputs
     (list texlive-everypage texlive-graphics texlive-kvoptions))
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
    (synopsis "A new interface for environments in LaTeX")
    (description
     "This package provides the @code{\\collect@@body} command (as in
@code{amsmath}), as well as a @code{\\long} version @code{\\Collect@@Body},
for collecting the body text of an environment.  These commands are used to
define a new author interface to creating new environments.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-environ texlive-environ)

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
    (propagated-inputs
     (list texlive-environ texlive-tools))
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
             (list "doc/latex/etoc/"
                   "source/latex/etoc/"
                   "tex/latex/etoc/")
             (base32
              "04vjfn4jadxbc38r08r9pwvpj7szvk88hiav35iqhl3p78xri7z4")))
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
title
(optionally with a frame around the table).

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
    (propagated-inputs
     (list texlive-pdftexcmds))
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

(define-public texlive-hanging
  (package
    (name "texlive-hanging")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/hanging"
                   "source/latex/hanging"
                   "tex/latex/hanging/")
             (base32
              "1d9kr163vn9sm9p9dyppnnffdcdjlgrm7848d97s678hdb9cp962")))
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
              "1v3688hziqz4jywfysiv19vsdzfkknrf83zfbi7lhiqpgkpsfsm2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-fontaxes
           texlive-fontspec
           texlive-iftex
           texlive-mweights
           texlive-xkeyval))
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
              "1ahn47kz8a2qdmzdfdgjanf6h5bn8f2rzp1zvwgjpk1plcix8k90")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
    (propagated-inputs
     (list texlive-catchfile
           texlive-iftex
           texlive-pdftexcmds
           texlive-tools))
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
             (list "doc/latex/newfloat/"
                   "source/latex/newfloat/"
                   "tex/latex/newfloat/")
             (base32
              "1hrackdfrzad8cgbl3f3yaagk4p4zjbvq710rm8b1z02fr9z2zkq")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics))
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
             (list "doc/latex/newunicodechar/" "source/latex/newunicodechar"
                   "tex/latex/newunicodechar/")
             (base32
              "0mybqah1n9vmxvi6f587jlxbn7pv3624qw83psq5vwfnddw3n70y")))
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
              "1m3afrpyc75g5gdxfknad565r5jgmwks98skkqycm66i92ky9dqr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
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
    (propagated-inputs
     (list texlive-iftex texlive-infwarerr texlive-ltxcmds))
    (home-page "https://www.ctan.org/pkg/pdftexcmds")
    (synopsis "LuaTeX support for pdfTeX utility functions")
    (description
     "This package makes a number of utility functions from pdfTeX
available for LuaTeX by reimplementing them using Lua.")
    (license license:lppl1.3c+)))

(define-deprecated-package texlive-latex-pdftexcmds texlive-pdftexcmds)

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
    (propagated-inputs
     (list texlive-graphics))
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
    (propagated-inputs
     (list texlive-bigfoot              ; for suffix
           texlive-filemod
           texlive-graphics
           texlive-ifplatform
           texlive-l3kernel             ; for expl3
           texlive-oberdiek
           texlive-psfrag
           texlive-tools                ; for shellesc
           texlive-trimspaces
           texlive-xkeyval))
    (home-page "https://ctan.org/pkg/pstool")
    (synopsis "Support for @code{psfrag} within pdfLaTeX")
    (description
     "This is a package for processing PostScript graphics with @code{psfrag}
labels within pdfLaTeX documents.  Every graphic is compiled individually,
drastically speeding up compilation time when only a single figure needs
re-processing.")
    (license license:lppl1.3c)))

(define-deprecated-package texlive-latex-pstool texlive-pstool)

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
    (propagated-inputs
     (list texlive-infwarerr texlive-ltxcmds))
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
    (propagated-inputs
     (list texlive-kvoptions
           texlive-kvsetkeys
           texlive-stringenc))
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
              "1clgw5xy867khzfn8d210rc5hsw5s7r0pznhk84niybvw4zc7r3f")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-geometry
           texlive-hyperref
           texlive-pstricks))
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
    (propagated-inputs
     (list texlive-attachfile
           texlive-float
           texlive-graphics
           texlive-listings
           texlive-refcount
           texlive-varwidth))
    (home-page "https://ctan.org/pkg/showexpl")
    (synopsis "Typesetting LaTeX source code")
    (description
     "This package provides a way to typeset LaTeX source code and the related
result in the same document.")
    (license license:lppl1.2+)))

(define-public texlive-stackengine
  (package
    (name "texlive-stackengine")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/stackengine/"
                   "tex/latex/stackengine/")
             (base32
              "1rbw3dmb6kl3wlnpxacr8cmp2ivac1kpnb33k7r5s3lp1q59ck38")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox
           texlive-listofitems))
    (home-page "https://ctan.org/pkg/stackengine")
    (synopsis "Customised stacking of objects")
    (description
     "The package provides a versatile way to stack objects vertically in a
variety of customizable ways.  A number of useful macros are provided, all
of which make use of the @code{stackengine} core.")
    (license license:lppl1.3+)))

(define-deprecated-package texlive-latex-stackengine texlive-stackengine)

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
    (propagated-inputs
     (list texlive-filecontents))
    (home-page "https://ctan.org/pkg/trimspaces")
    (synopsis "Trim spaces around an argument or within a macro")
    (description
     "This package provides a very short package that allows you to expandably
remove spaces around a token list (commands are provided to remove spaces
before, spaces after, or both); or to remove surrounding spaces within a macro
definition, or to define space-stripped macros.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-trimspaces texlive-trimspaces)

(define-public texlive-currfile
  (package
    (name "texlive-currfile")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/currfile/" "source/latex/currfile/"
                   "tex/latex/currfile/")
             (base32
              "1l9win5msf80yzgzfx580d1hw8lza1advhqkhpz83i080020asji")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
    (propagated-inputs
     (list texlive-filehook
           texlive-kvoptions))
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
              "139k4n8dv6pbal1mx4m8b239x3i9cw61f6digk9mxscbxwvxfngb")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics
           texlive-tools))
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
    (propagated-inputs
     (list texlive-etexcmds
           texlive-infwarerr
           texlive-ltxcmds))
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
    (propagated-inputs
     (list texlive-etoolbox
           texlive-hyperref
           texlive-l3packages
           texlive-listings
           texlive-pgf
           texlive-tools
           texlive-xstring))
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
    (propagated-inputs
     (list texlive-hyperref))
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
    (propagated-inputs
     (list texlive-etex))
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
    (propagated-inputs
     (list texlive-graphics))
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
    (propagated-inputs
     (list texlive-babel
           texlive-eurosym
           texlive-marvosym))
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
    (propagated-inputs
     (list texlive-graphics))
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
    (propagated-inputs
     (list texlive-biblatex))
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
    (propagated-inputs
     (list texlive-atbegshi
           texlive-graphics             ;for keyval
           texlive-iftex))
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
    (propagated-inputs
     (list texlive-fontspec
           texlive-graphics
           texlive-iftex))
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
    (description "The @code{paralist} package provides @code{enumerate} and
@code{itemize} environments that can be used within paragraphs to format the
items either as running text or as separate paragraphs with a preceding number
or symbol.  It also provides compacted versions of @code{enumerate} and
@code{itemize}.")
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
              "1lkf06mr7p7p1fdkrnhmvj8iamzppjy952d79mc81cilkw5zskah")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:tex-format "xelatex"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'substitute-fonts
            (lambda _
              (substitute* "source/latex/polyglossia/polyglossia.dtx"
                (("\\{Serto Jerusalem}") "{FreeSans}") ;non-free
                ;; XXX: Amiri font would introduce a (native) dependency on
                ;; webkitgtk!  No, thanks.
                (("\\{Amiri}") "{FreeSans}"))))
          (add-after 'unpack 'extend-texmf
            (lambda _
              ;; Extend the current TEXMF environment variable to make
              ;; Polyglossia own libraries visible.
              (setenv "GUIX_TEXMF"
                      (string-append (getcwd) ":"
                                     (getenv "GUIX_TEXMF"))))))))
    (native-inputs
     (list font-dejavu
           font-gfs-ambrosia
           font-gnu-freefont
           font-linuxlibertine
           font-sil-ezra
           fontconfig                   ;for XDG_DATA_DIRS (to locate fonts)
           texlive-babel
           texlive-bidi
           texlive-booktabs
           texlive-caption
           texlive-context
           texlive-fancyvrb
           texlive-graphics
           texlive-hyperref
           texlive-latex-fonts
           texlive-metalogo
           texlive-microtype
           texlive-paralist))
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
              "0wbbhjr1jqiicnssiy6n5s4v5p6axhlilpkfhix4kavbj8mb6mfn")))
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
              "185fibd41wd0v51gnai29ygi32snkk00p00110kcnk1bcnmpiw82")))
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
    (arguments
     (list #:build-targets #~(list "bookmark.dtx")))
    (propagated-inputs
     (list texlive-atenddvi
           texlive-atveryend
           texlive-auxhook
           texlive-hyperref))
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
              "0y32inrdpki6v3dwyymfglf78wbfd29b6xa8vjn337dr4gxlma85")))
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
             (list "doc/latex/colortbl/"
                   "source/latex/colortbl/"
                   "tex/latex/colortbl/")
             (base32
              "0fb4a5l3yqk6l5gr0hlkqwpy004wi8zymyicdzjyhqwcib4jnzjs")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics
           texlive-tools))
    (home-page "https://ctan.org/pkg/colortbl")
    (synopsis "Add colour to LaTeX tables")
    (description
     "The package allows rows and columns to be coloured, and even
individual cells.")
    (license license:lppl)))

(define-deprecated-package texlive-latex-colortbl texlive-colortbl)

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
              "15fainwxs22gg4xhwsv1vmjgdhg34dbkir26nnk4pb6jprpwb83f")))
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
             (list "doc/latex/footmisc/"
                   "source/latex/footmisc/"
                   "tex/latex/footmisc/")
             (base32
              "1v1hkf0xcq6hyz3b32z3hvs53lmwrnkn79y9wxq6pqmhcgilqji3")))
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
    (propagated-inputs
     (list texlive-etoolbox))
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
    (propagated-inputs
     (list texlive-relsize texlive-tools texlive-url))
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
    (propagated-inputs
     (list texlive-l3kernel
           texlive-l3packages))
    (home-page "https://ctan.org/pkg/kantlipsum")
    (synopsis "Generate sentences in Kant's style")
    (description
     "The package spits out sentences in Kantian style; the text is provided
by the Kant generator for Python by Mark Pilgrim, described in the book ``Dive
into Python''.  The package is modelled on @code{lipsum}, and may be used for
similar purposes.")
    (license license:lppl1.3c)))

(define-public texlive-lipsum
  (package
    (name "texlive-lipsum")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/lipsum/" "source/latex/lipsum/"
                   "tex/latex/lipsum/")
             (base32
              "1iwk2iqq5s5sn2z2kr7m59fm5j14dr4nsxivia3lhph8q38p5q6q")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-l3kernel
           texlive-l3packages))
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
              "15dnm0j86305x84ss3ymhhcczcw45b2liq01vrab6fj204wzsahk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     ;; Do not build intermediate "lstdrvrs.ins".
     (list #:build-targets '(list "listings.ins")))
    (propagated-inputs
     (list texlive-fancyvrb texlive-hyperref texlive-url))
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
    (propagated-inputs
     (list texlive-listings
           texlive-pdftexcmds
           texlive-stringenc))
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
    (propagated-inputs
     (list texlive-graphics))
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
              "1b8q93l54160b8gn3fq484n15n6cylrhmf2xk7p42czg2rqw7w3l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-kvsetkeys texlive-ltxcmds))
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

;; FIXME: the fonts should be built from source, but running "tex aefonts.tex"
;; fails with obscure TeX-typical error messages.
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

(define-public texlive-incgraph
  (package
    (name "texlive-incgraph")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/incgraph/" "tex/latex/incgraph/")
             (base32
              "1j5pzhzfbgzd21bq3dh7932pv0052g5l8r0qyx68n3cbsg46lcdk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-bookmark
           texlive-graphics
           texlive-pgf))
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
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Times font from
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
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Palatino font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

(define-deprecated-package texlive-fonts-adobe-palatino texlive-palatino)

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
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a drop-in replacements for the Zapfding font from
Adobe's basic set.")
    ;; No license version specified.
    (license license:gpl3+)))

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
              "1lc83d4qyqljfnf2m3jhq36f3f1yjbi71ys1hc11b9x2a46xk4pf")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atbegshi
           texlive-atveryend
           texlive-gettitlestring
           texlive-iftex
           texlive-kvoptions
           texlive-pdftexcmds
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-graphics))
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
             (list "doc/latex/enotez/"
                   "tex/latex/enotez/")
             (base32
              "1s1wyq6m5932gpbpvvkiw857q94jn1rp7xy9y7hysz9aafjqjyk2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-l3packages
           texlive-translations))
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
    (propagated-inputs
     (list texlive-eepic texlive-graphics))
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
    (propagated-inputs
     (list texlive-etoolbox
           texlive-kvoptions))
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
    (propagated-inputs
     (list texlive-tools))
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
              "0a68vxkygk20fp51fkp7nvs8mc7h6irdvxal8qsnn9zrgr965d76")))
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
              "0i7rd1wn5jgm3gbi779gy78apz63w034ck4pn73xw6s10zgjzmgl")))
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
             (list "doc/latex/sidecap/"
                   "source/latex/sidecap/"
                   "tex/latex/sidecap/")
             (base32
              "0bjb514a6j90ad7dgyyzrwk6pp7rlb3zk9mfy0fv5a615a5gz82x")))
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
     "The fonts were originally distributed as METAFONT sources only, but
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
    (propagated-inputs
     (list texlive-caption
           texlive-graphics))
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
    (propagated-inputs
     (list texlive-tools))
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
    (propagated-inputs
     (list texlive-bookmark
           texlive-hyperref))
    (home-page "https://ctan.org/pkg/threeparttable")
    (synopsis "Tables with captions and notes all the same width")
    (description
     "This package facilitates tables with titles (captions) and notes.  The
title and notes are given a width equal to the body of the table (a
@code{tabular} environment).  By itself, a @code{threeparttable} does not
float, but you can put it in a @code{table} or a @code{table*} or some other
environment.")
    (license (license:fsf-free "file://threeparttable.sty"))))

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
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'bring-ini-files
            (lambda _
              (for-each (lambda (f) (install-file f "build"))
                        (find-files "tex/jadetex/base"))))
          (add-after 'bring-ini-files 'generate-formats
            (lambda* (#:key inputs #:allow-other-keys)
              (mkdir "web2c")
              (with-directory-excursion "build"
                (invoke "fmtutil-sys" "--byfmt" "jadetex"
                        "--fmtdir=../web2c")
                (invoke "fmtutil-sys" "--byfmt" "pdfjadetex"
                        "--fmtdir=../web2c"))))
          (add-after 'install 'install-formats-and-wrappers
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((pdftex (search-input-file inputs "/bin/pdftex"))
                    (web2c (string-append #$output "/share/texmf-dist/web2c")))
                (mkdir-p web2c)
                (copy-recursively "web2c" web2c)
                (for-each delete-file (find-files web2c "\\.log$"))
                ;; Create convenience command wrappers.
                (mkdir-p (string-append #$output "/bin"))
                (symlink pdftex
                         (string-append #$output "/bin/jadetex"))
                (symlink pdftex
                         (string-append #$output "/bin/pdfjadetex"))))))))
    (native-inputs
     (list texlive-amsfonts
           texlive-cm                   ;for cmex10 and others
           texlive-colortbl
           texlive-fancyhdr
           texlive-graphics             ;for color.sty
           texlive-hyperref
           ;; The t1cmr.fd file of texlive-latex-base refers to the ecrm font,
           ;; provided by the jknapltx package collection.
           texlive-jknapltx
           texlive-latex-fonts          ;for lasy6
           texlive-letltxmacro
           texlive-marvosym
           texlive-tex-ini-files        ;for pdftexconfig
           texlive-tools                ;for array.sty
           texlive-ulem))               ;for fmtutil.cnf template
    (propagated-inputs
     (list
      (texlive-updmap.cfg
       (list texlive-amsfonts
             texlive-atbegshi
             texlive-atveryend
             texlive-auxhook
             texlive-bigintcalc
             texlive-bitset
             texlive-colortbl
             texlive-dehyph
             texlive-ec
             texlive-etexcmds
             texlive-everyshi
             texlive-fancyhdr
             texlive-firstaid
             texlive-hycolor
             texlive-hyperref
             texlive-hyph-utf8
             texlive-hyphen-base
             texlive-latexconfig
             texlive-iftex
             texlive-infwarerr
             texlive-intcalc
             texlive-kvdefinekeys
             texlive-kvoptions
             texlive-kvsetkeys
             texlive-l3backend
             texlive-l3kernel
             texlive-l3packages
             texlive-latexconfig
             texlive-letltxmacro
             texlive-ltxcmds
             texlive-marvosym
             texlive-passivetex
             texlive-pdfescape
             texlive-pdftex
             texlive-pdftexcmds
             texlive-rerunfilecheck
             texlive-stmaryrd
             texlive-symbol
             texlive-tipa
             texlive-ulem
             texlive-unicode-data
             texlive-uniquecounter
             texlive-url
             texlive-wasysym
             ;; Propagate the texlive-updmap.cfg input used by xmltex,
             ;; which provides the required fonts for its use.
             texlive-xmltex
             texlive-zapfding))))
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
              "1d5r80isyvs2v3i8pzlhsn7ns6bn8ldkbs82g25widraixlhg6yg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-fontaxes
           texlive-fontspec
           texlive-hyperref
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
    (propagated-inputs
     (list texlive-graphics))
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
              "01nwh4p15xblc3kgivjliihy9kr8yr2cqsf9wn2iwqv1njx0i2zw")))
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
              "04pgi23frfk6ds10zypqvki0852ds7m1s52c5qvbpyl647nfbgc5")))
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
              "0vz0dla2frf5wgp5xrqc9q4z730k9wayfkfj0vg58a2xjriarrzn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-booktabs
           texlive-caption
           texlive-cmap
           texlive-comment
           texlive-draftwatermark
           texlive-environ
           texlive-etoolbox
           texlive-fancyhdr
           texlive-float
           texlive-geometry
           texlive-graphics
           texlive-hyperref
           texlive-hyperxmp
           texlive-iftex
           texlive-inconsolata
           texlive-libertine
           texlive-microtype
           texlive-natbib
           texlive-ncctools
           texlive-newtx
           texlive-preprint
           texlive-refcount
           texlive-setspace
           texlive-textcase
           texlive-totpages
           texlive-xcolor
           texlive-xkeyval
           texlive-xstring))
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
    (propagated-inputs
     (list texlive-bookmark
           texlive-hyperref))
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

(define-public texlive-ucs
  (package
    (name "texlive-ucs")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ucs/" "tex/latex/ucs/data/"
                   "tex/latex/ucs/utils/")
             (base32
              "1hr7dsfx7vggai1j7saba48lsm1a003my9qkbr6qmazgc3lbcvl8")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics
           texlive-hyperref))
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
              "1njw4ziyigmzxky86sh6byn8jjdah51iyd8lkmwx5rxhaqp7snkp")))
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
    (propagated-inputs
     (list texlive-bigfoot texlive-relsize texlive-xstring))
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
              "182q6cy2crn2wwaljsq35g0kcmrngyjc307b4sh2zjdnkf8n58xx")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:texlive-latex-base #f
      #:create-formats #~(list "etex" "pdfetex" "pdftex")))
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
                "070gczcm1h9rx29w2f02xd3nhd84c4k28nfmm8qgp69yq8vd84pz"))))
    (build-system gnu-build-system)
    (inputs
     (list lua
           perl
           python
           ruby
           tcsh
           texlive-bin))
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-26))

      ;; This package takes 4 GiB, which we can't afford to distribute from
      ;; our servers.
      #:substitutable? #f

      #:phases
      #~(modify-phases (map (cut assq <> %standard-phases)
                            '(set-paths unpack patch-source-shebangs))
          (add-after 'unpack 'unset-environment-variables
            (lambda _
              (unsetenv "TEXMF")
              (unsetenv "TEXMFCNF")))
          (add-after 'patch-source-shebangs 'install
            (lambda _
              (let ((share (string-append #$output "/share")))
                (mkdir-p share)
                (invoke "mv" "texmf-dist" share))))
          (add-after 'install 'texmf-config
            (lambda _
              (let* ((share (string-append #$output "/share"))
                     (texmfroot (string-append share "/texmf-dist/web2c"))
                     (texmfcnf (string-append texmfroot "/texmf.cnf"))
                     (fmtutilcnf (string-append texmfroot "/fmtutil.cnf"))
                     (texlive-bin #$(this-package-input "texlive-bin"))
                     (texbin (string-append texlive-bin "/bin"))
                     (tlpkg (string-append texlive-bin "/share/tlpkg")))
                ;; LuaJIT is not ported to powerpc64* yet.
                (if #$(target-ppc64le?)
                    (substitute* fmtutilcnf
                      (("^(luajittex|luajithbtex|mfluajit)" m)
                       (string-append "#! " m))))
                ;; Register SHARE as TEXMFROOT in texmf.cnf.
                (substitute* texmfcnf
                  (("TEXMFROOT = \\$SELFAUTOPARENT")
                   (string-append "TEXMFROOT = " share))
                  (("TEXMFLOCAL = \\$SELFAUTOGRANDPARENT/texmf-local")
                   "TEXMFLOCAL = $SELFAUTODIR/share/texmf-local")
                  (("!!\\$TEXMFLOCAL") "$TEXMFLOCAL"))
                ;; Register paths in texmfcnf.lua, needed for context.
                (substitute* (string-append texmfroot "/texmfcnf.lua")
                  (("selfautodir:") #$output)
                  (("selfautoparent:") (string-append share "/")))
                ;; Set path to TeXLive Perl modules
                (setenv "PERL5LIB"
                        (string-append (getenv "PERL5LIB") ":" tlpkg))
                ;; Configure the texmf-dist tree; inspired from
                ;; http://slackbuilds.org/repository/13.37/office/texlive/
                (setenv "PATH" (string-append (getenv "PATH") ":" texbin))
                (setenv "TEXMFCNF" texmfroot)
                (invoke "updmap-sys" "--nohash" "--syncwithtrees")
                (invoke "mktexlsr")
                (invoke "fmtutil-sys" "--all")))))))
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
    (inputs
     (list bash-minimal                 ;for wrap-program
           texlive-bin
           texlive-texmf))
    (native-search-paths
     (list (search-path-specification
            (variable "TEXMFLOCAL")
            (files '("share/texmf-local")))))
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      ;; Build the union of texlive-bin and texlive-texmf, but take the
      ;; conflicting subdirectory share/texmf-dist from texlive-texmf.
      #~(begin
          (use-modules (guix build utils))
          (let ((bin #$(this-package-input "texlive-bin"))
                (texmf #$(this-package-input "texlive-texmf"))
                (bash #$(this-package-input "bash-minimal")))
            (mkdir #$output)
            (with-directory-excursion #$output
              (for-each
               (lambda (name)
                 (symlink (string-append bin "/" name) name))
               '("include" "lib"))
              (mkdir "bin")
              (with-directory-excursion "bin"
                (setenv "PATH" (string-append bash "/bin"))
                (for-each
                 (lambda (name)
                   (symlink name (basename name))
                   (wrap-program
                       (basename name)
                     `("TEXMFCNF" =
                       (,(string-append texmf "/share/texmf-dist/web2c")))))
                 (find-files (string-append bin "/bin/") "")))
              (mkdir "share")
              (with-directory-excursion "share"
                (for-each
                 (lambda (name)
                   (symlink (string-append bin "/share/" name) name))
                 '("info" "man" "tlpkg"))
                (for-each
                 (lambda (name)
                   (symlink (string-append texmf "/share/" name) name))
                 '("texmf-dist" "texmf-var"))))))))
    (synopsis "TeX Live, a package of the TeX typesetting system")
    (description
     "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts that
are free software, including support for many languages around the world.

This package contains the complete TeX Live distribution.")
    (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
    (home-page "https://www.tug.org/texlive/")))

(define-public perl-text-bibtex
  (package
    (name "perl-text-bibtex")
    (version "0.88")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/A/AM/AMBS/Text-BibTeX-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0b7lmjvfmypps1nw6nsdikgaakm0n0g4186glaqazg5xd1p5h55h"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'add-output-directory-to-rpath
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "inc/MyBuilder.pm"
               (("-Lbtparse" line)
                (string-append "-Wl,-rpath="
                               (assoc-ref outputs "out") "/lib " line)))
             #t))
         (add-after 'unpack 'install-libraries-to-/lib
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Build.PL"
               (("lib64") "lib"))
             #t)))))
    (native-inputs
     (list perl-capture-tiny perl-config-autoconf perl-extutils-libbuilder
           perl-module-build))
    (home-page "https://metacpan.org/release/Text-BibTeX")
    (synopsis "Interface to read and parse BibTeX files")
    (description "@code{Text::BibTeX} is a Perl library for reading, parsing,
and processing BibTeX files.  @code{Text::BibTeX} gives you access to the data
at many different levels: you may work with BibTeX entries as simple field to
string mappings, or get at the original form of the data as a list of simple
values (strings, macros, or numbers) pasted together.")
    (license license:perl-license)))

(define-public biber
  (package
    ;; Note: When updating Biber, make sure it matches our BibLaTeX version by
    ;; checking the Biber/BibLaTeX compatibility matrix in the BibLaTeX manual
    ;; at <https://ctan.org/pkg/biblatex>.
    (name "biber")
    (version "2.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plk/biber/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0586q8y1f2k23mvb02ccm3qsb35cwskafksixsjaih7a7xcf5gxx"))
              (patches (search-patches "biber-adapt-perl-5.36.patch"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perl5lib (getenv "PERL5LIB")))
               (wrap-program (string-append out "/bin/biber")
                 `("PERL5LIB" ":" prefix
                   (,(string-append perl5lib ":" out
                                    "/lib/perl5/site_perl")))))
             #t)))))
    (inputs
     (list perl-autovivification
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
    (native-inputs
     `(("perl-config-autoconf" ,perl-config-autoconf)
       ("perl-extutils-libbuilder" ,perl-extutils-libbuilder)
       ("perl-module-build" ,perl-module-build)
       ;; for tests
       ("perl-file-which" ,perl-file-which)
       ("perl-test-more" ,perl-test-most) ; FIXME: "more" would be sufficient
       ("perl-test-differences" ,perl-test-differences)))
    (home-page "https://biblatex-biber.sourceforge.net/")
    (synopsis "Backend for the BibLaTeX citation management tool")
    (description "Biber is a BibTeX replacement for users of biblatex.  Among
other things it comes with full Unicode support.")
    (license license:artistic2.0)))

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
              "1kx0zbwd7pd4mah0b8l595hyjc03g505kfmn6fv7iaqvkixqrgbi")))
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
              "195zli0l69rvxxd7cs387g6bipppfl0pyfsf5invq191zlv319b2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-l3packages
           texlive-media9
           texlive-pgf))
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
    (propagated-inputs
     (list texlive-everyshi texlive-tools))
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
    (propagated-inputs
     (list texlive-amsmath texlive-graphics))
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
    (propagated-inputs
     (list texlive-tools))
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
    (arguments
     (list #:tex-format "latex"))
    (native-inputs
     (list texlive-filecontents))
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
    (arguments
     (list #:tex-format "latex"))
    (native-inputs
     (list texlive-filecontents))
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
                   "scripts/pgf/"
                   "source/generic/pgf/c/"
                   "source/generic/pgf/testsuite/external/"
                   "source/generic/pgf/testsuite/mathtest/"
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
              "1d6s7sf7dmcqrx652f7j468rylkarihxl0ghg0sy5scjdn3z9bdr")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atveryend
           texlive-everyshi
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
                   "source/latex/koma-script/"
                   "tex/latex/koma-script/")
             (base32
              "0k8mhikpll066x3683gmg3xas7a2mz93b9fip4k56hacxxb6map1")))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'move-required-files
            ;; These files are required by the build process.
            (lambda _
              (for-each (lambda (f)
                          (install-file
                           (string-append "doc/latex/koma-script/" f)
                           "build/"))
                        '("komabug.tex" "manifest.txt" "INSTALL.txt"
                          "INSTALLD.txt" "lppl.txt" "lppl-de.txt")))))))
    (propagated-inputs
     (list texlive-bookmark
           texlive-eso-pic
           texlive-etoolbox
           texlive-graphics
           texlive-l3packages
           texlive-xpatch))
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
    (propagated-inputs
     (list texlive-iftex texlive-infwarerr texlive-ltxcmds))
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
    (propagated-inputs
     (list texlive-pdftexcmds))
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
    (propagated-inputs
     (list texlive-bigintcalc texlive-infwarerr texlive-intcalc))
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
    (propagated-inputs
     (list texlive-iftex texlive-infwarerr))
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
    (propagated-inputs
     (list texlive-etex
           texlive-etoolbox
           texlive-letltxmacro))
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
    (propagated-inputs
     (list texlive-ltxcmds))
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
                   "tex/generic/kvsetkeys/")
             (base32
              "0b2f2r49vi8x54qshm1h9sh8zhdmy0mc2y44yd05kcmmbiiq7hfz")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-infwarerr))
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
             (list "doc/generic/listofitems"
                   "tex/generic/listofitems")
             (base32
              "1vzp4qkpfxzgcll1ak9syyc91sl93k9wr5rgfqvd6d6rgrnh3ava")))
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
    (propagated-inputs
     (list texlive-ltxcmds texlive-pdftexcmds))
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
    (propagated-inputs
     (list texlive-bigintcalc texlive-infwarerr))
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
              "05yi37j8jq5a9pp9n6qg76m2fw899vpmwafzgnxbg0qp2fmch2ch")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-listofitems))
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
    (propagated-inputs
     (list texlive-tools))
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
code and its compiled LaTeX or METAPOST output side-by-side, with automatic
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
    (propagated-inputs
     (list texlive-graphics texlive-tipa))
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
    (propagated-inputs (list texlive-graphics texlive-iftex))
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
              "0h72ckha1mv1a2i5v85l68amfc0kf0km9iyin6vxxal69146j8gp")))
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
                   "context/data/texfont/"
                   "context/data/textadept/context/data/"
                   "context/data/textadept/context/lexers/"
                   "context/data/textadept/context/modules/"
                   "context/data/textadept/context/themes/"
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
                   "doc/context/presentations/examples/"
                   "doc/context/presentations/tug/2001/"
                   "doc/context/presentations/tug/2007/"
                   "doc/context/scripts/mkii/"
                   "doc/context/scripts/mkiv/"
                   "doc/context/sources/general/leaflets/"
                   "doc/context/sources/general/magazines/"
                   "doc/context/sources/general/manuals/about/"
                   "doc/context/sources/general/manuals/bidi/"
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
                   "doc/man/man1/texexec.1"
                   "doc/man/man1/texexec.man1.pdf"
                   "doc/man/man1/texmfstart.1"
                   "doc/man/man1/texmfstart.man1.pdf"
                   "fonts/afm/hoekwater/context/"
                   "fonts/cid/fontforge/"
                   "fonts/enc/dvips/context/"
                   "fonts/map/dvips/context/"
                   "fonts/map/luatex/context/"
                   "fonts/map/pdftex/context/"
                   "fonts/misc/xetex/fontmapping/context/"
                   "fonts/tfm/hoekwater/context/"
                   "fonts/type1/hoekwater/context/"
                   "metapost/context/base/common/"
                   "metapost/context/base/mpii/"
                   "metapost/context/base/mpiv/"
                   "metapost/context/base/mpxl/"
                   "metapost/context/fonts/mpiv/"
                   "scripts/context/lua/"
                   "scripts/context/perl/"
                   "scripts/context/ruby/base/"
                   "scripts/context/ruby/graphics/"
                   "scripts/context/ruby/rslb/"
                   "scripts/context/stubs/install/"
                   "scripts/context/stubs/mswin/"
                   "scripts/context/stubs/setup/"
                   "scripts/context/stubs/source/"
                   "scripts/context/stubs/unix/"
                   "scripts/context/stubs/win64/"
                   "tex/context/base/"
                   "tex/context/bib/common/"
                   "tex/context/bib/mkii/"
                   "tex/context/colors/icc/context/"
                   "tex/context/fonts/mkii/"
                   "tex/context/fonts/mkiv/"
                   "tex/context/interface/mkii/"
                   "tex/context/interface/mkiv/"
                   "tex/context/modules/common/"
                   "tex/context/modules/mkii/"
                   "tex/context/modules/mkiv/"
                   "tex/context/modules/mkxl/"
                   "tex/context/patterns/common/"
                   "tex/context/patterns/mkii/"
                   "tex/context/patterns/mkiv/"
                   "tex/context/sample/common/"
                   "tex/context/sample/third/"
                   "tex/context/test/mkiv/"
                   "tex/context/user/mkii/"
                   "tex/generic/context/luatex/"
                   "tex/generic/context/ppchtex/"
                   "tex/latex/context/ppchtex/")
             (base32
              "1sbh4fnxxymh7lmvldp1ll8p6adcf3jhvqf47jvrayqr91zp4hh9")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs (list texlive-amsfonts
                             texlive-lm
                             texlive-lm-math
                             texlive-luatex
                             texlive-manfnt-font
                             texlive-metapost
                             texlive-mflogo-font
                             texlive-mptopdf
                             texlive-pdftex
                             texlive-stmaryrd
                             texlive-xetex))
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

(define-public texlive-beamer
  (package
    (name "texlive-beamer")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/beamer/"
                   "tex/latex/beamer/")
             (base32
              "091n27n4l3iac911bvmpp735ffryyzaq46mkclgn3q9jsvc4ngiv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amsfonts
           texlive-amsmath
           texlive-atbegshi
           texlive-etoolbox
           texlive-graphics
           texlive-hyperref
           texlive-iftex
           texlive-oberdiek
           texlive-pgf
           texlive-tools
           texlive-translator
           texlive-ucs
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
              "1r9vga6pl8q0p40njr1l04nhga4i0pjyppsd9qmxx0kx408siram")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-iftex))
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
    (propagated-inputs
     (list texlive-colorprofiles
           texlive-everyshi
           texlive-hyperref
           texlive-iftex
           texlive-pdftexcmds
           texlive-stringenc
           texlive-xcolor
           texlive-xmpincl))
    (home-page "https://ctan.org/pkg/pdfx")
    (synopsis "PDF/X and PDF/A support for pdfTeX, LuaTeX and XeTeX")
    (description
     "The package helps LaTeX users to create PDF/X, PFD/A and other
standards-compliant PDF documents with pdfTeX, LuaTeX and XeTeX.")
    (license license:lppl1.2+)))

(define-deprecated-package texlive-latex-pdfx texlive-pdfx)

(define-public texlive-ydoc
  (package
    (name "texlive-ydoc")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/ydoc/" "source/latex/ydoc/"
                   "tex/generic/ydoc/" "tex/latex/ydoc/")
             (base32
              "1z7690vin47mw47gjg7k4h49b4ckg6g96l1zlziyjmjbkyzmyhdn")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'create-missing-directories
            ;; XXX: These directories are not created even though they belong
            ;; to locations in TEXLIVE-ORIGIN.  Create them manually.
            (lambda _
              (mkdir-p "tex/generic/ydoc/")
              (mkdir-p "tex/latex/ydoc/"))))))
    (propagated-inputs
     (list texlive-etoolbox
           texlive-float
           texlive-hyperref
           texlive-listings
           texlive-needspace
           texlive-svn-prov
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
             (list "doc/generic/pstricks/"
                   "dvips/pstricks/"
                   "tex/generic/pstricks/"
                   "tex/latex/pstricks/")
             (base32
              "15c9iqfq2y9c8c78cvqb6vzd5a5rm7qq5x7m05jq1hb8sgqrqb0j")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amsfonts
           texlive-amsmath
           texlive-babel
           texlive-bera
           texlive-biblatex
           texlive-booktabs
           texlive-caption
           texlive-chngcntr
           texlive-eso-pic
           texlive-fancyvrb
           texlive-filecontents
           texlive-footmisc
           texlive-graphics
           texlive-hyperref
           texlive-ifplatform
           texlive-iftex
           texlive-listings
           texlive-multido
           texlive-ragged2e
           texlive-setspace
           texlive-subfig
           texlive-tools
           texlive-xcolor))
    (home-page "http://www.ctan.org/pkg/pstricks")
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
    (propagated-inputs
     (list texlive-pstricks))
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
             (list "/doc/generic/iftex/"
                   "/tex/generic/iftex/")
             (base32
              "147xa5kl4kjs05nj8v3kd7dpr5xkz3xp3gdvjih32ccd7527f5vp")))
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
             (list "doc/latex/tabu/"
                   "source/latex/tabu/"
                   "tex/latex/tabu/")
             (base32 "0mixyrqavipq4ni38z42x3579cdjbz54cp2qqb4q4yhfbl0a4pka")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-tools
           texlive-varwidth))
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
             (list "doc/latex/xkeyval/"
                   "source/latex/xkeyval/"
                   "tex/generic/xkeyval/"
                   "tex/latex/xkeyval/")
             (base32
              "0hcfqxbi907yi9jwq61i638n8g9abf6zc0aazk2lxzshy44h3ms1")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     '(#:build-targets '("xkeyval.dtx")
       #:tex-format "latex"             ;won't build with luatex
       #:phases
       (modify-phases %standard-phases
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
                  texlive-iftex
                  texlive-listings
                  texlive-lm
                  texlive-pgf
                  texlive-pst-text
                  texlive-pstricks
                  texlive-url
                  texlive-xcolor))))
    (propagated-inputs
     (list texlive-tools))
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
                   "tex/latex/standalone/"
                   "tex/plain/standalone/")
             (base32
              "00cs6bxpcpl8fjld280af52njkv44fm81yww9ynhqa9xp49q0p90")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
    (propagated-inputs
     (list texlive-adjustbox
           texlive-currfile
           texlive-filemod
           texlive-gincltex
           texlive-iftex
           texlive-multido
           texlive-pdftexcmds
           texlive-pgf
           texlive-preview
           texlive-pstricks
           texlive-tools
           texlive-varwidth
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
              "05gpl318mpm5gxb9665080yd5qiirmh3hwixg9p4wgydk8wfllnl")))
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
             (list "doc/latex/csquotes/"
                   "tex/latex/csquotes/")
             (base32
              "17y5mrmjmi7n0cgq4cnqr55f4bni6lx1pfdv5pzsmbrzha3mhbfg")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox
           texlive-graphics
           texlive-relsize))
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
    (propagated-inputs
     (list texlive-etoolbox
           texlive-graphics))
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
              "1v3y2i7vng1qfs3p7ma2mf8lvvib422aagc3z6q2vwz6r3y4mr5k")))
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
              "0ivf7xbzj4xd57sqfbi87hbr73rraqifkzvx06yxgq0gmzz0x6wl")))
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
              "0lhqzrvf216j3rzg7lmc1mvnr2mzr0a6c2kqrfwzw6qbpm9v29nk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-pgf
           texlive-tools
           texlive-xcolor
           texlive-xkeyval))
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
              "1r9w6za8g263n16pz0r5adrx5sazhfa78rdhjj9idnif12bgvpq2")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics))
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
              "13cjsjb3b04n9arwp46ayk8fcicylxq5g1864cpxl1lxjxh1yi0l")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list python-pygments
           texlive-etoolbox
           texlive-fancyvrb
           texlive-float
           texlive-framed
           texlive-fvextra
           texlive-graphics
           texlive-ifplatform
           texlive-kvoptions
           texlive-lineno
           texlive-newfloat
           texlive-pdftexcmds
           texlive-tools
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
              "1fg3zfgi54zqx911wbqfb1y24d9ihm6wg59npng4clnqz45lla2i")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics))
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
    (synopsis "URW Base 35 font pack for LaTeX")
    (description
     "This package provides a set of fonts for use as drop-in replacements for
Adobe's basic set, comprising: Century Schoolbook (substituting for Adobe's
New Century Schoolbook); Dingbats (substituting for Adobe's Zapf Dingbats);
Nimbus Mono L (substituting for Abobe's Courier); Nimbus Roman No9
L (substituting for Adobe's Times); Nimbus Sans L (substituting for Adobe's
Helvetica); Standard Symbols L (substituting for Adobe's Symbol); URW Bookman;
URW Chancery L Medium Italic (substituting for Adobe's Zapf Chancery); URW
Gothic L Book (substituting for Adobe's Avant Garde); and URW Palladio
L (substituting for Adobe's Palatino).")
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
    (propagated-inputs
     (list texlive-amsfonts
           texlive-bera))
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
    (propagated-inputs
     (list texlive-graphics
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-graphics))
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
              "038h02n02fii0kv021d5z8ic2p0mqnjzwxdbvcfym4gkcw345fxk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-fontspec
           texlive-iftex))
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
              "1gqqqbj7i18fs1ss5n3axd821hzq5kbv1dl7dqxp4gba619f1rli")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-fontaxes
           texlive-fontspec
           texlive-iftex
           texlive-mweights
           texlive-xkeyval))
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
             (list "/doc/fonts/newtx/"
                   "/fonts/afm/public/newtx/"
                   "/fonts/enc/dvips/newtx/"
                   "/fonts/map/dvips/newtx/"
                   "/fonts/opentype/public/newtx/"
                   "/fonts/tfm/public/newtx/"
                   "/fonts/type1/public/newtx/"
                   "/fonts/vf/public/newtx/"
                   "/tex/latex/newtx/")
             (base32
              "0h0wm3cd0wxag5x7vy3vgr42jd8m6ffkl90pnkvqdxzbnfdjv3l6")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amsmath
           texlive-carlisle
           texlive-etextools
           texlive-etoolbox
           texlive-fontaxes
           texlive-iftex
           texlive-oberdiek
           texlive-trimspaces
           texlive-xkeyval
           texlive-xstring))
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
              "0d8rvcmvxrlxqqxpirxqbhmiijpsz5y4vvldh1jnc018aannjlhm")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-carlisle
           texlive-etoolbox
           texlive-fontaxes
           texlive-xstring
           texlive-xkeyval))
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
             (list "doc/fonts/ly1/"
                   "fonts/enc/dvips/ly1/"
                   "fonts/map/dvips/ly1/"
                   "fonts/tfm/adobe/ly1/"
                   "fonts/vf/adobe/ly1/"
                   "tex/latex/ly1/"
                   "tex/plain/ly1/")
             (base32
              "1lks902rr94m3n3r4rc2lm4vvqhqv9prgrpni5ww64rqrv56h8yy")))
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
    (propagated-inputs
     (list texlive-iftex
           texlive-kvoptions))
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
    (arguments
     (list #:tex-format "latex"))
    (native-inputs
     (list texlive-filecontents))
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
    (propagated-inputs
     (list texlive-ifmtarg))
    (home-page "https://ctan.org/pkg/pagenote")
    (synopsis "Notes at end of document")
    (description
     "The @code{pagenote} package provides tagged notes on a separate
page (also known as end notes).")
    (license license:lppl1.3c+)))

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
              "06xn3dwf6aa8j3lmvvgwfadw2ahw770jx91x8nyl8zir58aiys5s")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
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
    (native-inputs
     (list texlive-ydoc))
    (propagated-inputs
     (list texlive-collectbox
           texlive-iftex))
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
              "106k01lgnvikndk48r5ms9xj3gmynv2xy20090frr7sa3g9k42za")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
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
    (propagated-inputs
     (list texlive-infwarerr
           texlive-kvdefinekeys))
    (home-page "https://ctan.org/pkg/grfext")
    (synopsis "Manipulate the @code{graphics} package's list of extensions")
    (description
     "This package provides macros for adding to, and reordering the list of
graphics file extensions recognised by package @code{graphics}.")
    (license license:lppl1.3c+)))

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
              "01r6cb8aadbgsfcqhqnwaig3xwzgr0nfxci3mzb8ln3k4dghmq97")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (native-inputs
     (list texlive-ydoc))
    (propagated-inputs
     (list texlive-collectbox
           texlive-graphics
           texlive-ifoddpage
           texlive-pgf
           texlive-storebox
           texlive-tools
           texlive-varwidth
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-lm
           texlive-xcolor
           texlive-xkeyval))
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
              "1qnsbblkadzdn1fx2k21xnlwcb35pg9xya24chkm66jmidi22qp0")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amsmath
           texlive-environ
           texlive-etoolbox
           texlive-hyperref
           texlive-incgraph
           texlive-iftex
           texlive-l3packages
           texlive-listings
           texlive-listingsutf8
           texlive-marvosym
           texlive-minted
           texlive-oberdiek             ;for pdfcol
           texlive-pdftexcmds
           texlive-pgf
           texlive-psnfss
           texlive-refcount
           texlive-tools))
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
    (propagated-inputs
     (list texlive-l3kernel))
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
    (synopsis "METAFONT and macros for Euro sign")
    (description
     "The European currency symbol for the Euro implemented in METAFONT, using
the official European Commission dimensions, and providing several
shapes (normal, slanted, bold, outline).  The package also includes a LaTeX
package which defines the macro, pre-compiled @file{tfm} files, and
documentation.")
    (license (license:non-copyleft "file:///doc/fonts/eurosym/COPYING"))))

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
               "0vl7ckpbkjvz3a5snzppb96ncwgmhpwb2p6cg30grfyn421kap3v")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox
           texlive-pdftexcmds))
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
              "13rxdqhvgwc5lz2wsw4jwsb92614wlxsa90rmzxyrc6xjz1jypnk")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-graphics))
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

(define-public texlive-textpos
  (package
    (name "texlive-textpos")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/textpos/" "source/latex/textpos/"
                   "tex/latex/textpos/")
             (base32
              "0gg6b2ckafj8fbrlw85m538c08qyq2cv5z59r9pzcwg1c1xdyn02")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-everyshi
           texlive-graphics))
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
    (propagated-inputs
     (list texlive-amsmath
           texlive-fontspec
           texlive-l3kernel
           texlive-l3packages
           texlive-lm-math
           texlive-lualatex-math))
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
    (propagated-inputs
     (list texlive-ifmtarg
           texlive-tools))
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
              "0rgzckyy6w4rmgxins5kakllkpn2hrccaps7lwb8h2nzvd29yj3m")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list
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
    (propagated-inputs
     (list texlive-amsfonts))
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
    (propagated-inputs
     (list texlive-amsmath
           texlive-etoolbox
           texlive-graphics
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-fontaxes
           texlive-ly1                  ;requires LY1 font encoding
           texlive-mweights
           texlive-xkeyval))
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
    (arguments
     (list #:tex-format "latex"))
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
              "0nawx1fh55yhqspy5jgss2qmwpqmikfrg7628smk931rph9nq0aa")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etoolbox texlive-fancyvrb texlive-lineno texlive-upquote))
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
    (propagated-inputs
     (list texlive-apacite
           texlive-babel
           texlive-biblatex
           texlive-booktabs
           texlive-caption
           texlive-draftwatermark
           texlive-endnotes
           texlive-etoolbox
           texlive-fancyhdr
           texlive-float
           texlive-geometry
           texlive-graphics
           texlive-lm
           texlive-substr
           texlive-threeparttable
           texlive-times
           texlive-tools
           texlive-xstring))
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
    (propagated-inputs
     (list texlive-natbib
           texlive-tools))
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
    (propagated-inputs
     (list texlive-graphics))
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
              "1fp0l9sn9yrhf8hz175dzc2x28byk1ygfirn23am5ak72csmi0cp")))
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
    (propagated-inputs
     (list texlive-etoolbox
           texlive-l3kernel
           texlive-l3packages))
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
    (propagated-inputs
     (list texlive-environ
           texlive-threeparttable))
    (home-page "https://ctan.org/pkg/threeparttablex")
    (synopsis "Notes in @code{longtables}")
    (description
     "The package provides the functionality of the @code{threeparttable}
package to tables created using the @code{longtable} package.")
    (license license:lppl1.3+)))

(define-public texlive-lineno
  (package
    (name "texlive-lineno")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/lineno/" "source/latex/lineno/"
                   "tex/latex/lineno/")
             (base32
              "1xf8ljgcj411yqmng89wc49rqfz19j95yqqpnb35dj3qc1chvm2a")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-tools))
    (home-page "https://ctan.org/pkg/lineno")
    (synopsis "Line numbers on paragraphs")
    (description
     "The @code{lineno} package adds line numbers to selected paragraphs with
reference possible through the LaTeX @code{\\ref} and @code{\\pageref} cross
reference mechanism.  Line numbering may be extended to footnote lines, using
the @code{fnlineno} package.")
    (license license:lppl1.3a+)))

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
              "1s72g2hfnk5nqnrsbiwydh7jb9wy9186h5vy7rh3ngjwkmcfg0pz")))
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
              "0an9csjd4jhz6civdldsrmz7l76hw8zfcgxdp55mj8f1rchsjylx")))
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
    (propagated-inputs
     (list texlive-amsmath
           texlive-booktabs
           texlive-csquotes
           texlive-eso-pic
           texlive-etoolbox
           texlive-iftex
           texlive-geometry
           texlive-graphics
           texlive-kantlipsum
           texlive-kvoptions
           texlive-l3kernel
           texlive-l3packages
           texlive-lipsum
           texlive-listings
           texlive-microtype
           texlive-needspace
           texlive-ntheorem
           texlive-oberdiek
           texlive-pgf
           texlive-selinput
           texlive-tools
           texlive-xcolor
           texlive-zref))
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
              "00ik8qgkw3ivh3z827zjf7gbwkbsmdcmv22c6ap543mpgaqqjcfm")))
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
              "05vvmwd8vlzs2x2rm6pfzlvrrihqf924d7krlrkvc6giiwyfsic4")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-atbegshi
           texlive-graphics
           texlive-iftex))
    (home-page "https://ctan.org/pkg/pdflscape")
    (synopsis "Make landscape pages display as landscape")
    (description
     "The @code{pdflscape} package adds PDF support to the @code{landscape}
environment of package @code{lscape}, by setting the PDF @code{/Rotate} page
attribute.  Pages with this attribute will be displayed in landscape
orientation by conforming PDF viewers.")
    (license license:lppl1.3+)))

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
     (list texlive-etoolbox
           texlive-pgf
           texlive-tracklang
           texlive-xkeyval))
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
            "1386sg25y6zb4ixvrbdv6n1gp54h18mjd984bnwwqda6jafxx4zr")))
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
    (propagated-inputs
     (list texlive-tools))
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
              "1cxj5jdgvr3xk1inrb3yzpm3l386jjawgpqiwsz53k6yshb6yfml")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (arguments
     (list #:tex-format "latex"))
    (propagated-inputs
     (list texlive-everysel
           texlive-footmisc))
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
    (propagated-inputs
     (list texlive-graphics))
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
    (arguments
     (list #:tex-format "latex"))
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
    (arguments
     (list #:tex-format "latex"))
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
              "0w6jk97jmgwgshr9a3isbpwsh0fhrkzp36gywdiai1f5x2sldmpv")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-amsfonts
           texlive-amsmath
           texlive-graphics
           texlive-l3kernel
           texlive-tools))
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
    (propagated-inputs
     (list texlive-iftex texlive-xkeyval))
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
     (list texlive-amsmath
           texlive-etoolbox
           texlive-fp
           texlive-mfirstuc
           texlive-pgf
           texlive-substr
           texlive-tools
           texlive-xfor
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-amsmath
           texlive-l3packages))
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
    (propagated-inputs
     (list texlive-fontaxes
           texlive-iftex
           texlive-mweights
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-fontaxes
           texlive-iftex
           texlive-mweights
           texlive-xkeyval))
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
    (propagated-inputs
     (list texlive-fontaxes
           texlive-iftex
           texlive-mweights
           texlive-xkeyval))
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
              "1cmzl0jkid4w60bjlyxrc5bynbc3lwq5nr77rsip0q9hprxykxks")))
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
              "0ikipdswzsafi4rr6q9xh3hkxk2n2683ym1879qcax41xs6cizdl")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (home-page "https://ctan.org/pkg/soul")
    (synopsis "Hyphenation for letterspacing, underlining, and more")
    (description
     "The @code{soul} package enables hyphenatable spacing out (letterspacing),
underlining, striking out, etc., using the TeX hyphenation algorithm to find
the proper hyphens automatically.  The package also provides a mechanism that
can be used to implement similar tasks, that have to treat text syllable by
syllable.  The package itself does not support UTF-8 input in ordinary
(PDF)LaTeX; some UTF-8 support is offered by package @code{soulutf8}.")
    (license license:lppl)))

(define-deprecated-package texlive-generic-soul texlive-soul)

(define-public texlive-soulutf8
  (package
    (name "texlive-soulutf8")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/latex/soulutf8/" "source/latex/soulutf8/"
                   "tex/generic/soulutf8/")
             (base32
              "0d9lv3xsads8ms642ys3pghxnsa2hlzafkcx66d2hbq224bz1phc")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
    (propagated-inputs
     (list texlive-etexcmds texlive-infwarerr texlive-soul))
    (home-page "https://ctan.org/pkg/soulutf8")
    (synopsis "Permit use of UTF-8 characters in @code{soul}")
    (description
     "This package extends package @code{soul} and adds some support for
UTF-8.  Namely the input encodings in @file{utf8.def} from package
@code{inputenc} and @file{utf8x.def} from package @code{ucs} are supported.")
    (license license:lppl1.3+)))

(define-public texlive-xstring
  (package
    (name "texlive-xstring")
    (version (number->string %texlive-revision))
    (source (texlive-origin
             name version
             (list "doc/generic/xstring/" "tex/generic/xstring/")
             (base32
              "1azpq855kq1l4686bjp8haxim5c8wycz1b6lcg5q7x8kb4g9sppn")))
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
    (propagated-inputs (list texlive-graphics))
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
    (propagated-inputs (list texlive-everyshi texlive-graphics))
    (home-page "https://ctan.org/pkg/totpages")
    (synopsis "Count pages in a document, and report last page number")
    (description
     "The package counts the actual pages in the document (as opposed to
reporting the number of the last page, as does @code{lastpage}).  The counter
itself may be shipped out to the DVI file.")
    (license license:lppl)))

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
    (source (texlive-origin
             name version
             (list "doc/man/man1/xelatex.1"
                   "doc/man/man1/xelatex.man1.pdf"
                   "doc/man/man1/xetex.1"
                   "doc/man/man1/xetex.man1.pdf"
                   "doc/xetex/base/"
                   "fonts/misc/xetex/fontmapping/base/")
             (base32
              "15bjr41p9l5d6837hy3nrhkkylgv04b0150vysyg5730svh3fnan")))
    (outputs '("out" "doc"))
    (build-system texlive-build-system)
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
           texlive-latex-base
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
