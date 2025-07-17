;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2013, 2015-2017, 2019, 2022-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2019, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Pierre-Moana Levesque <pierre.moana.levesque@gmail.com>
;;; Copyright © 2019, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 ( <paren@disroot.org>
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

(define-module (gnu packages texinfo)
  #:use-module (gnu packages autotools)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-compression)
  #:use-module (gnu packages readline))

(define-public texinfo
  (package
    (name "texinfo")
    (version "6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                  version ".tar.xz"))
              (patches (search-patches "texinfo-headings-single.patch"))
              (sha256
               (base32
                "1i7yb7mrp3inz25zbzv2pllr4y7d58v818f1as7iz8mw53nm7dwf"))))
    (build-system gnu-build-system)
    (arguments
     ;; When cross-compiling, the package is configured twice: once with the
     ;; native compiler and once with the cross-compiler. During the configure
     ;; with the native compiler, the environment is reset. This leads to
     ;; multiple environment variables missing. Do not reset the environment
     ;; to prevent that.
     `(#:phases
       (if ,(%current-target-system)
            (modify-phases %standard-phases
              (add-before 'configure 'fix-cross-configure
                (lambda _
                  (substitute* "configure"
                    (("env -i")
                     "env "))
                  #t)))
            %standard-phases)

       ,@(if (%current-target-system)
             (list #:configure-flags #~'("texinfo_cv_sys_iconv_converts_euc_cn=yes"))
             '())

       ;; XXX: Work around <https://issues.guix.gnu.org/59616>.
       #:tests? ,(and (not (target-hurd?))
                      (not (%current-target-system)))))
    (inputs (list ncurses perl))

    ;; When cross-compiling, texinfo will build some of its own binaries with
    ;; the native compiler. This means ncurses is needed both in both inputs
    ;; and native-inputs.  Some of its tests require extra locales such as
    ;; fr_FR.UTF-8.
    (native-inputs (list perl ncurses (libc-utf8-locales-for-target)))

    (native-search-paths
     ;; This is the variable used by the standalone Info reader.
     (list (search-path-specification
            (variable "INFOPATH")
            (files '("share/info")))))

    (home-page "https://www.gnu.org/software/texinfo/")
    (synopsis "The GNU documentation format")
    (description
     "Texinfo is the official documentation format of the GNU project.  It
uses a single source file using explicit commands to produce a final document
in any of several supported output formats, such as HTML or PDF.  This
package includes both the tools necessary to produce Info documents from
their source and the command-line Info reader.  The emphasis of the language
is on expressing the content semantically, avoiding physical markup commands.")
    (license gpl3+)))

(define-public texinfo-7
  (package
    (inherit texinfo)
    (version "7.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10kcdb3pf7yakniccvv0krchs2fh3vh1rvhvnqr98ll3cbj3gbii"))))
    (inputs (modify-inputs (package-inputs texinfo)
              (append perl-archive-zip           ;needed for 'tex2any --epub3'
                      perl-unicode-eastasianwidth perl-text-unidecode
                      perl-libintl-perl)))
    (arguments
     (substitute-keyword-arguments (package-arguments texinfo)
       ((#:configure-flags flags
         ''())
        #~(cons* "--with-external-Unicode-EastAsianWidth"
                 "--with-external-Text-Unidecode"
                 "--with-external-libintl-perl"
                 #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'wrap-program
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin"))
                       (program (string-append bin "/texi2any"))
                       (zip (car (find-files
                                  (assoc-ref inputs "perl-archive-zip")
                                  (lambda (file stat)
                                    (and (eq? 'directory (stat:type stat))
                                         (string=? (basename file)
                                                   "Archive")))
                                  #:directories? #t)))
                       (mods (map (lambda (mod)
                                    (string-append (assoc-ref inputs mod)
                                                   "/lib/perl5/site_perl"))
                                  '("perl-unicode-eastasianwidth"
                                    "perl-text-unidecode" "perl-libintl-perl"))))
                  (wrap-program program
                    `("PERL5LIB" prefix
                      ,(cons* (dirname zip) mods))))))))))))

(define-public texinfo-5
  (package (inherit texinfo)
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/texinfo/texinfo-"
                                  version ".tar.xz"))
              (patches (search-patches "texinfo-5-perl-compat.patch"))
              (sha256
               (base32
                "1njfwh2z34r2c4r0iqa7v24wmjzvsfyz4vplzry8ln3479lfywal"))))))

(define-public texinfo-4
  (package (inherit texinfo)
    (version "4.13a")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/texinfo/texinfo-"
                    version
                    ".tar.lzma"))
              (sha256
               (base32
                "1rf9ckpqwixj65bw469i634897xwlgkm5i9g2hv3avl6mv7b0a3d"))))
    (inputs (list ncurses xz))
    (native-inputs
      (modify-inputs (package-native-inputs texinfo)
        (prepend automake)))
    (arguments
     (append
      (substitute-keyword-arguments (package-arguments texinfo)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'unpack 'fix-configure
              (lambda* (#:key inputs native-inputs #:allow-other-keys)
                ;; Replace outdated config.sub and config.guess.
                (with-directory-excursion "build-aux"
                  (for-each
                   (lambda (file)
                     (install-file (string-append
                                    (assoc-ref
                                     (or native-inputs inputs) "automake")
                                    "/share/automake-"
                                    ,(version-major+minor
                                      (package-version automake))
                                    "/" file) "."))
                   '("config.sub" "config.guess")))
                #t))
            ;; Build native version of tools before running 'build phase.
            ,@(if (%current-target-system)
                  `((add-before 'build 'make-native-gnu-lib
                      (lambda* (#:key inputs #:allow-other-keys)
                        (invoke "make" "-C" "tools/gnulib/lib")
                        #t)))
                  '()))))
      (if (or (target-hurd64?) (%current-target-system))
          (list #:configure-flags ''("CFLAGS=-Wno-error=incompatible-pointer-types"))
          '())))))

(define-public info-reader
  ;; The idea of this package is to have the standalone Info reader without
  ;; the dependency on Perl that 'makeinfo' drags.
  ;; Texinfo version must be at least 7.0, which fixed crashes in a pt_BR
  ;; locale; see <https://git.savannah.gnu.org/cgit/texinfo.git/plain/NEWS>.
  (package/inherit texinfo-7
    (name "info-reader")
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments texinfo)
           ((#:phases phases)
            `(modify-phases ,phases
               ;; Make sure 'info-reader' can read compressed info files
               ;; in a pure environment.  There are also a few other
               ;; uncompressors listed in this file (lzip, unxz, bunzip2, ...)
               ;; but let's not include them because info manuals in Guix
               ;; are always compressed with 'gzip'.
               ;; TODO(core-updates): maybe move to the 'texinfo' package.
               (add-after 'unpack 'absolute-binary-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "info/filesys.c"
                     (("gunzip") (search-input-file inputs "/bin/gunzip"))
                     (("gzip") (search-input-file inputs "/bin/gzip")))))
               (add-after 'install 'keep-only-info-reader
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Remove everything but 'bin/info' and associated
                   ;; files.
                   (define (files)
                     (scandir "." (lambda (file)
                                    (not (member file '("." ".."))))))

                   (let ((out (assoc-ref outputs "out")))
                     (with-directory-excursion out
                       (for-each delete-file-recursively
                                 (fold delete (files) '("bin" "share"))))
                     (with-directory-excursion (string-append out "/bin")
                       (for-each delete-file (delete "info" (files))))
                     (with-directory-excursion (string-append out "/share")
                       (for-each delete-file-recursively
                                 (fold delete (files)
                                       '("info" "locale"))))
                     #t))))))
       #:disallowed-references ,(assoc-ref (package-inputs texinfo)
                                           "perl")
       #:modules ((ice-9 ftw) (srfi srfi-1)
                  ,@%default-gnu-modules)))
    (synopsis "Standalone Info documentation reader")
    (inputs (modify-inputs (package-inputs texinfo)
              (prepend gzip)))))

(define-public texi2html
  (package
    (name "texi2html")
    (version "5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/" name "/" name "-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1yprv64vrlcbksqv25asplnjg07mbq38lfclp1m5lj8cw878pag8"))
              (patches
               (search-patches "texi2html-document-encoding.patch"
                               "texi2html-i18n.patch"))
              (snippet
               ;; This file is modified by the patch above, but reset its
               ;; timestamp so we don't trigger the rule to update PO files,
               ;; which would require Gettext.
               ;; See <http://bugs.gnu.org/18247>.
               '(begin
                  (utime "texi2html.pl" 0 0 0 0)
                  #t))))
    (build-system gnu-build-system)
    (inputs (list perl))
    (arguments
     ;; Tests fail because of warnings on stderr from Perl 5.22.  Adjusting
     ;; texi2html.pl to avoid the warnings seems non-trivial, so we simply
     ;; disable the tests.
     '(#:tests? #f))
    (home-page "https://www.nongnu.org/texi2html/")
    (synopsis "Convert Texinfo to HTML")
    (description
     "Texi2HTML is a Perl script which converts Texinfo source files to HTML
output.  It now supports many advanced features, such as internationalization
and extremely configurable output formats.

Development of Texi2HTML moved to the GNU Texinfo repository in 2010, since it
was meant to replace the makeinfo implementation in GNU Texinfo.  The route
forward for authors is, in most cases, to alter manuals and build processes as
necessary to use the new features of the makeinfo/texi2any implementation of
GNU Texinfo.  The Texi2HTML maintainers (one of whom is the principal author
of the GNU Texinfo implementation) do not intend to make further releases of
Texi2HTML.")
    ;; Files in /lib under lgpl2.1+ and x11
    (license gpl2+)))

(define-public texi2html-1.82
  (package
    (inherit texi2html)
    (version "1.82")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/texi2html/"
                           "texi2html-" version ".tar.bz2"))
       (sha256
        (base32 "1wdli2szkgm3l0vx8rf6lylw0b0m47dlz9iy004n928nqkzix76n"))))))

(define-public pinfo
  (let ((commit "3d76eecde211e41ccc28b04e229f159b3f924399")
        (revision "0"))
    (package
      (name "pinfo")
      ;; Latest tag is completely broken and does not build.
      (version (git-version "0.6.13" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/baszoetekouw/pinfo")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0qym323d9my5l4bhw9vry453hhlxhjjvy8mcdi38sk0bqqid0vd1"))))
      (build-system gnu-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'embed-reference-to-clear
                   (lambda* (#:key inputs #:allow-other-keys)
                     (let ((ncurses (assoc-ref inputs "ncurses")))
                       (substitute* (list "src/manual.c"
                                          "src/mainfunction.c"
                                          "src/utils.c")
                         (("\"clear\"")
                          (string-append "\"" ncurses "/bin/clear\"")))))))))
      (inputs
       (list ncurses readline))
      (native-inputs
       (list autoconf automake gettext-minimal libtool texinfo))
      (home-page "https://github.com/baszoetekouw/pinfo")
      (synopsis "Lynx-style Info file and man page reader")
      (description
       "Pinfo is an Info file viewer.  Pinfo is similar in use to the Lynx web
browser.  You just move across info nodes, and select links, follow them, etc.
It supports many colors.  Pinfo also supports viewing of manual pages -- they
are colorized like in the midnight commander's viewer, and additionally they
are hypertextualized.")
      (license gpl2+))))
