;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2019, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Danny Milosavljevic <dannym+a@scratchpost.org>
;;; Copyright © 2018, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2021 Thiago Jung Bauermann <bauermann@kolabnow.com>
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

(define-module (gnu packages texlive)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system trivial)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

;; The starting point for this file is
;; commit ad457d01147b8d6fcb4ee64b2dc2d699caa1d1ee


;; These variables specify the SVN tag and the matching SVN revision.  They
;; are taken from https://www.tug.org/svn/texlive/tags/
(define %texlive-tag "texlive-2021.3")
(define %texlive-revision 59745)

(define %texlive-date "20210325")
(define %texlive-version (string-take %texlive-date 4))

(define texlive-extra-src
  (origin
    (method url-fetch)
    (uri (string-append "ftp://tug.org/historic/systems/texlive/"
                        %texlive-version "/texlive-"
                        %texlive-date "-extra.tar.xz"))
    (sha256 (base32
             "171kg1n9zapw3d2g47d8l0cywa99bl9m54xkqvp9625ks22z78s6"))))

(define texlive-texmf-src
  (origin
    (method url-fetch)
    (uri (string-append "ftp://tug.org/historic/systems/texlive/"
                        %texlive-version "/texlive-"
                        %texlive-date "-texmf.tar.xz"))
    (sha256 (base32
             "070gczcm1h9rx29w2f02xd3nhd84c4k28nfmm8qgp69yq8vd84pz"))))

(define-public texlivebin
  (package
    (name "texlivebin")
    (version %texlive-version)
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://tug.org/historic/systems/texlive/"
                           %texlive-version "/texlive-"
                           %texlive-date "-source.tar.xz"))
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


(define-public texlivetexmf
  (package
   (name "texlivetexmf")
   (version %texlive-version)
   (source texlive-texmf-src)
   (build-system gnu-build-system)
   (inputs
    `(("texlive-bin" ,texlivebin)
      ("lua" ,lua)
      ("perl" ,perl)
      ("python" ,python)
      ("ruby" ,ruby)
      ("tcsh" ,tcsh)))
   (arguments
    `(#:modules ((guix build gnu-build-system)
                 (guix build utils)
                 (srfi srfi-26))

      ;; This package takes 4 GiB, which we can't afford to distribute from
      ;; our servers.
      #:substitutable? #f

      #:phases
        (modify-phases (map (cut assq <> %standard-phases)
                            '(set-paths unpack patch-source-shebangs))
          (add-after 'unpack 'unset-environment-variables
            (lambda _
              (unsetenv "TEXMF")
              (unsetenv "TEXMFCNF")
              #t))
          (add-after 'patch-source-shebangs 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share")))
                (mkdir-p share)
                (invoke "mv" "texmf-dist" share))))
          (add-after 'install 'texmf-config
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (share (string-append out "/share"))
                     (texmfroot (string-append share "/texmf-dist/web2c"))
                     (texmfcnf (string-append texmfroot "/texmf.cnf"))
                     (texlive-bin (assoc-ref inputs "texlive-bin"))
                     (texbin (string-append texlive-bin "/bin"))
                     (tlpkg (string-append texlive-bin "/share/tlpkg")))
                ;; LuaJIT is not ported to powerpc64* yet.
                (if ,(target-ppc64le?)
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
                  (("selfautodir:") out)
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
   (version %texlive-version)
   (source #f)
   (build-system trivial-build-system)
   (inputs `(("bash" ,bash-minimal)     ;for wrap-program
             ("texlive-bin" ,texlivebin)
             ("texlive-texmf" ,texlivetexmf)))
   (native-search-paths
    (list (search-path-specification
           (variable "TEXMFLOCAL")
           (files '("share/texmf-local")))))
   (arguments
    `(#:modules ((guix build utils))
      #:builder
        ;; Build the union of texlive-bin and texlive-texmf, but take the
        ;; conflicting subdirectory share/texmf-dist from texlive-texmf.
        (begin
          (use-modules (guix build utils))
          (let ((out (assoc-ref %outputs "out"))
                (bin (assoc-ref %build-inputs "texlive-bin"))
                (texmf (assoc-ref %build-inputs "texlive-texmf"))
                (bash (assoc-ref %build-inputs "bash")))
               (mkdir out)
               (with-directory-excursion out
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
                     '("texmf-dist" "texmf-var"))))
               #t))))
   (synopsis "TeX Live, a package of the TeX typesetting system")
   (description
    "TeX Live provides a comprehensive TeX document production system.
It includes all the major TeX-related programs, macro packages, and fonts
that are free software, including support for many languages around the
world.

This package contains the complete TeX Live distribution.")
   (license (license:fsf-free "https://www.tug.org/texlive/copying.html"))
   (home-page "https://www.tug.org/texlive/")))

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
                                    "/lib/perl5/site_perl"))))))))))
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

