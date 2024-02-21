;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012, 2014, 2015, 2017, 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 David Thompson <dthompson2@worcester.edu>
;;; Copyright © 2015, 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2022, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Imran Iqbal <imran@imraniqbal.org>
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

(define-module (gnu packages man)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (guix utils)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages less)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages xml))

(define-public xmltoman
  (package
    (name "xmltoman")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xmltoman/xmltoman/"
                           "xmltoman-" version ".tar.gz/"
                           "xmltoman-" version ".tar.gz"))
       (sha256
        (base32 "1c0lvzr7kdy63wbn1jv6s126ds7add3pxqb0vlxd3v5a2sir91wl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; No tests
       #:make-flags
       (list
        (string-append "PREFIX="
                       (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (propagated-inputs
     (list perl perl-xml-parser))
    (synopsis "XML to Man converter")
    (description "XMLtoMan and XMLMantoHTML are two small scripts to convert xml
to man pages in groff format or html.  It features the usual man page items such
as description, options, see also, etc.")
    (home-page "http://xmltoman.sourceforge.net/")
    (license license:gpl2+)))

(define-public ronn
  (package
    (name "ronn")
    (version "0.7.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/rtomayko/ronn")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0fkniz7j1jp8v3i05m6hks3nsh6rzvjfi0ichpi7h4gwk5byxb94"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))                   ; Library hpricot not available
    (synopsis "Manual authoring tool")
    (description "Ronn builds manuals.  It converts simple, human readable
textfiles to roff for terminal display, and also to HTML for the web.")
    (home-page "https://rtomayko.github.io/ronn/")
    (license license:expat)))

(define-public libpipeline
  (package
    (name "libpipeline")
    (version "1.5.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://savannah/libpipeline/libpipeline-"
                    version ".tar.gz"))
              (sha256
               (base32
                "15xpx7kbzkn63ab8mkghv7jkzji8pdbsyxm7ygjji19rvkkvkyv0"))))
    (build-system gnu-build-system)
    (home-page "https://libpipeline.nongnu.org/")
    (synopsis "C library for manipulating pipelines of subprocesses")
    (description
     "libpipeline is a C library for manipulating pipelines of subprocesses in
a flexible and convenient way.")
    (license license:gpl3+)))

(define-public man-db
  (package
    (name "man-db")
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/man-db/man-db-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "15bak56wf99xdr1p3kaish6pkrrvhl6p2rhgzwiasr17la264nj1"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'patch-source-shebangs 'patch-test-shebangs
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Patch shebangs in test scripts.
                   (let ((out (assoc-ref outputs "out")))
                     (for-each (lambda (file)
                                 (substitute* file
                                   (("#! /bin/sh")
                                    (string-append "#!" (which "sh")))))
                               (find-files "src/tests")))))
               (add-after 'unpack 'patch-absolute-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/man.c"
                     (("\"iconv\"")
                      (string-append "\"" (which "iconv") "\"")))
                   ;; Embed an absolute reference to "preconv", otherwise it
                   ;; falls back to searching in PATH and ultimately fails
                   ;; to render unicode data (see <https://bugs.gnu.org/30785>).
                   (substitute* "lib/encodings.c"
                     (("groff_preconv = NULL")
                      (string-append "groff_preconv = \""
                                     (assoc-ref inputs "groff-minimal")
                                     "/bin/preconv\""))))))
           #:configure-flags
           #~(cons*
              ;; Disable setuid man user.
              "--disable-setuid"
              ;; Don't constrain ownership of system-wide cache files.
              ;; Otherwise creating the manpage database fails with
              ;; man-db > 2.7.5.
              "--disable-cache-owner"
              (string-append "--with-pager="
                             (search-input-file %build-inputs "bin/less"))
              (string-append "--with-gzip="
                             (search-input-file %build-inputs "bin/gzip"))
              (string-append "--with-bzip2="
                             (search-input-file %build-inputs "bin/bzip2"))
              (string-append "--with-xz="
                             (search-input-file %build-inputs "bin/xz"))
              (string-append "--with-zstd="
                             (search-input-file %build-inputs "bin/zstd"))
              (string-append "--with-col="
                             (search-input-file %build-inputs "bin/col"))
              ;; The default systemd directories ignore --prefix.
              (string-append "--with-systemdsystemunitdir="
                             #$output "/lib/systemd/system")
              (string-append "--with-systemdtmpfilesdir="
                             #$output "/lib/tmpfiles.d")
              (map (lambda (prog)
                     (string-append
                      "--with-" prog "="
                      #$(this-package-input "groff-minimal")
                      (string-append "/bin/" prog)))
                   '("nroff" "eqn" "neqn" "tbl" "refer" "pic")))
           ;; At run time we should refer to GROFF-MINIMAL, not GROFF (the latter
           ;; pulls in Perl.)
           #:disallowed-references (list groff)))
    (native-inputs
     (list pkg-config flex
           ;; Groff is needed at build time for troff, grops, soelim, etc.
           groff))
    (inputs
     (list gdbm
           groff-minimal
           less
           libpipeline
           libseccomp
           util-linux
           zstd))
    (native-search-paths
     (list (search-path-specification
            (variable "MANPATH")
            (files '("share/man")))))
    (home-page "https://man-db.nongnu.org/")
    (synopsis "Standard Unix documentation system")
    (description
     "Man-db is an implementation of the standard Unix documentation system
accessed using the man command.  It uses a Berkeley DB database in place of
the traditional flat-text whatis databases.")
    (license license:gpl2+)))

(define-public man2html
  (package
    (name "man2html")
    (version "1.6g-16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/man2html")
                    (commit (string-append "debian/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cxm8b2x4cjmyidi4gfz9q29zrhaxhbnsiqcmlnyr1bdhjsmk786"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      ;; The source include a man page viewer as well as the man2html
      ;; converter.  We're only interested in the converter, so we specify the
      ;; explicit 'manhtml' target.
      #:make-flags #~(list (string-append "bindir=" #$output "/bin")
                           "manhtml")
      #:phases #~(modify-phases %standard-phases
                   (replace 'configure
                     (lambda _
                       (setenv "CC" #$(cc-for-target))
                       (invoke "./configure"
                               (string-append "-prefix=" #$output))))
                   (add-before 'install 'chdir
                     (lambda _
                       (chdir "man2html")))
                   (replace 'install
                     ;; This is needed because the 'manhtml' top level target
                     ;; doesn't exist in man2html/Makefile.
                     (lambda* (#:key make-flags #:allow-other-keys
                               #:rest args)
                       (apply (assoc-ref %standard-phases 'install)
                              `(,@args #:make-flags
                                       ,(delete "manhtml" make-flags))))))))
    (home-page "https://salsa.debian.org/debian/man2html")
    (synopsis "Man pages to HTML format converter")
    (description "@command{man2html} is a command-line tool for converting man
pages into HTML format.")
    (license license:gpl2+)))

(define-public mandoc
  (package
    (name "mandoc")
    (version "1.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://mandoc.bsd.lv/snapshots/mandoc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "174x2x9ws47b14lm339j6rzm7mxy1j3qhh484khscw0yy1qdbw4b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "regress"
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-prefix
                    (lambda* (#:key outputs #:allow-other-keys)
                      (substitute* "configure"
                        (("^CC=.*")
                         (string-append "CC=" ,(cc-for-target) "\n"))
                        (("^DEFCFLAGS=\\\\\"")
                         "DEFCFLAGS=\"-O2 ")
                        (("^UTF8_LOCALE=.*")      ;used for tests
                         "UTF8_LOCALE=en_US.UTF-8\n")
                        (("^MANPATH_(BASE|DEFAULT)=.*" _ which)
                         (string-append "MANPATH_" which "="
                                        "/run/current-system/profile/share/man\n"))
                        (("^PREFIX=.*")
                         (string-append "PREFIX=" (assoc-ref outputs "out")
                                        "\n"))))))))
    (native-inputs (list (libc-utf8-locales-for-target) perl)) ;used to run tests
    (inputs (list zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "MANPATH")
            (files '("share/man")))))
    (synopsis "Tools for BSD mdoc and man pages")
    (description
     "mandoc is a suite of tools compiling mdoc, the roff macro language of
choice for BSD manual pages, and man, the predominant historical language for
UNIX manuals.  It is small and quite fast.  The main component of the toolset
is the @command{mandoc} utility program, based on the libmandoc validating
compiler, to format output for UTF-8 and ASCII UNIX terminals, HTML 5,
PostScript, and PDF.  Additional tools include the @command{man} viewer, and
@command{apropos} and @command{whatis}.")
    (home-page "https://mandoc.bsd.lv/")
    (license license:isc)))

(define-public man-pages
  (package
    (name "man-pages")
    (version "6.02")
    (source
     (origin
       (method url-fetch)
       (uri
        (list (string-append "mirror://kernel.org/linux/docs/man-pages/"
                             "man-pages-" version ".tar.xz")
              (string-append "mirror://kernel.org/linux/docs/man-pages/Archive/"
                             "man-pages-" version ".tar.xz")))
       (sha256
        (base32 "159p60a0w5ri3i7bbfxzjfmj8sbpf030m38spny1ws585fv0kn36"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-html
            ;; As of 6.00, this package tries to convert man pages to HTML with
            ;; man2html.  The only Guix package currently providing that script
            ;; is man-for-txr, but that version seems unable to handle relative
            ;; ‘.so’ statements properly.  Disable HTML generation.
            (lambda _
              (substitute* "lib/build-html.mk"
                (("(html:) .*" _ target) (string-append target "\n")))))
          (delete 'configure))

      ;; The 'all' target depends on three targets that directly populate
      ;; $(MANDIR) based on its current contents.  Doing that in parallel
      ;; leads to undefined behavior (see <http://bugs.gnu.org/18701>.)
      #:parallel-build? #f

      #:tests? #f
      #:make-flags
      #~(list (string-append "mandir=" #$output "/share/man"))))
    (home-page "https://www.kernel.org/doc/man-pages/")
    (synopsis "Development manual pages from the Linux project")
    (description
     "This package provides traditional Unix \"man pages\" documenting the
Linux kernel and C library interfaces employed by user-space programs.")

    ;; Each man page has its own license; some are GPLv2+, some are MIT/X11.
    (license license:gpl2+)))

(define-public man-pages-posix
  (package
    (name "man-pages-posix")
    ;; Make sure that updates are still legally distributable.  2017-a is not.
    (version "2013-a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kernel.org/linux/docs/man-pages/"
                           "man-pages-posix/man-pages-posix-" version
                           ".tar.xz"))
       (sha256
        (base32 "0258j05zdrxpgdj8nndbyi7bvrs8fxdksb0xbfrylzgzfmf3lqqr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags ,#~(list (string-append "prefix=" #$output))
       #:license-file-regexp "POSIX-COPYRIGHT"
       ;; The build phase only compresses documentation, which we already do.
       #:phases (modify-phases %standard-phases (delete 'configure)
                                                (delete 'build))))
    (home-page "https://www.kernel.org/doc/man-pages/")
    (synopsis "Man pages from the POSIX.1-2013 standard")
    (description
     "This package provides excerpts from the POSIX.1-2008 and TC1 standards
(collectively, POSIX.1-2013) in manual page form.  While the Linux man-pages
project documents the system as it exists on Linux- and glibc-based systems,
this package documents the portable software API as nominally implemented by
many Unix-likes.")
    (license (license:fsdg-compatible "file://POSIX-COPYRIGHT"
                                      "Redistribution of this material is permitted so long as this
notice and the corresponding notices within each POSIX manual page are retained
on any distribution, and the nroff source is included."))))

(define-public help2man
  ;; TODO: Manual pages for languages not available from the implicit
  ;; input "locales" contain the original (English) text.
  (package
    (name "help2man")
    (version "1.49.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/help2man/help2man-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0dnxx96lbcb8ab8yrdkqll14cl5n0bch8qpd9qj3c2ky78hhwbly"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no `check' target
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-help2man-with-perl-gettext
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((lib #$(this-package-input "perl-gettext"))
                    (fmt "use lib '~a/lib/perl5/site_perl';~%~a"))
                (substitute* "help2man.PL"
                  (("^use Locale::gettext.*$" load)
                   (format #f fmt lib load)))))))))
    (inputs
     (append
      (list perl)
      (if (%current-target-system)
          '()
          (list perl-gettext))))
    (native-inputs
     (list perl gettext-minimal))
    (home-page "https://www.gnu.org/software/help2man/")
    (synopsis "Automatically generate man pages from program --help")
    (description
     "GNU help2man is a program that converts the output of standard
\"--help\" and \"--version\" command-line arguments into a manual page
automatically.")
    (license license:gpl3+)))

(define-public scdoc
  (package
   (name "scdoc")
   (version "1.11.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~sircmpwn/scdoc")
           (commit version)))
     (file-name (git-file-name name version))
     (sha256
      (base32 "07c2vmdgqifbynm19zjnrk7h102pzrriv73izmx8pmd7b3xl5mfq"))))
   (build-system gnu-build-system)
   (arguments
    `(#:make-flags
      (list (string-append "CC=" ,(cc-for-target))
            (string-append "PREFIX=" (assoc-ref %outputs "out")))
      #:phases
      (modify-phases %standard-phases
        (delete 'configure))))
   (home-page "https://git.sr.ht/~sircmpwn/scdoc")
   (synopsis "Simple man page generator")
   (description "scdoc is a simple man page generator written for POSIX systems
in C99.")
   ;; MIT license, see /share/doc/scdoc-1.6.0/COPYING.
   (license license:expat)))

(define-public txt2man
  (package
    (name "txt2man")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/mvertes/txt2man")
              (commit (string-append "txt2man-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1razjpvlcp85hqli77mwr9nmn5jnv3lm1fxbbqjpx1brv3h1lvm5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no "check" target
       #:make-flags (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (inputs
     (list gawk))
    (home-page "https://github.com/mvertes/txt2man")
    (synopsis "Convert text to man page")
    (description "Txt2man converts flat ASCII text to man page format.")
    (license license:gpl2+)))
