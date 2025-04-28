;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2021, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Alexandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2021, 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2023 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2023 David Pflug <david@pflug.io>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages search)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages ebook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages file)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnunet)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages less)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages pkg-config))

(define-public xapian
  (package
    (name "xapian")
    (version "1.4.29")
    ;; Note: When updating Xapian, remember to update omega and
    ;; python-xapian-bindings below.
    (source (origin
              (method url-fetch)
              (uri (string-append "https://oligarchy.co.uk/xapian/" version
                                  "/xapian-core-" version ".tar.xz"))
              (sha256
               (base32 "1g11wps45rgh7a7z0zmsvk6vg2i1ih4cmbwf44nfrlrsc749np65"))))
    (build-system gnu-build-system)
    (inputs (list zlib
                  `(,util-linux "lib")))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; As of Xapian 1.3.3, the TCP server implementation uses
           ;; getaddrinfo(). This does not work in the build environment,
           ;; so exclude those tests. See HACKING for the list of targets.
           (lambda _
             (invoke "make"
                     "check-inmemory"
                     "check-remoteprog"
                     ;"check-remotetcp"
                     "check-multi"
                     "check-glass"
                     "check-chert"))))))
    (synopsis "Search Engine Library")
    (description
     "Xapian is a highly adaptable toolkit which allows developers to easily
add advanced indexing and search facilities to their own applications.  It
supports the Probabilistic Information Retrieval model and also supports a
rich set of boolean query operators.")
    (home-page "https://xapian.org/")
    (license (list license:gpl2+ license:bsd-3 license:x11))))

(define-public omega
  (package
    (name "omega")
    (version (package-version xapian))
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://oligarchy.co.uk/xapian/" version
                           "/xapian-omega-" version ".tar.xz"))
       (sha256
        (base32
         "0gpq1k1fanx2vpj0xrmkdafacn2r5qsf57201ax4skkb96flxfjg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list file pcre2 perl xapian zlib))
    (home-page "https://xapian.org/")
    (synopsis "Search engine built on Xapian")
    (description
     "Omega is a search application built on Xapian.  It provides indexers and
a CGI web search frontend.")
    (license (list license:gpl2+        ; Main license
                   ;; csvescape.cc, csvescape.h, csvesctest.cc, datetime.cc,
                   ;; datetime.h, jsonescape.cc, jsonescape.h, jsonesctest.cc,
                   ;; mimemap.h, my-html-tok.h, namedents.h, pkglibbindir.cc,
                   ;; pkglibbindir.h, timegm.cc, timegm.h, urldecode.h,
                   ;; urlencode.cc, urlencode.h, urlenctest.cc, common/Tokeniseise.pm,
                   ;; common/keyword.cc, common/keyword.h
                   license:expat))))

(define-public python-xapian-bindings
  (package (inherit xapian)
    (name "python-xapian-bindings")
    (version (package-version xapian))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://oligarchy.co.uk/xapian/" version
                                  "/xapian-bindings-" version ".tar.xz"))
              (sha256
               (base32
                "1kn8dw0zrlxvc417wmqmdkqz76kw3f5802wsv5kyyl38pckyjh0p"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags #~(list "--with-python3")
           #:make-flags
           #~(list (string-append "pkgpylibdir="
                                  #$output
                                  "/lib/python" #$(version-major+minor
                                                   (package-version python))
                                  "/site-packages/xapian")
                   ;; XXX: Otherwise set to "None", which produces _xapianNone
                   ;; and ends up unable to find it.
                   "PYTHON3_SO=.so")))
    (native-inputs
     (list python-sphinx)) ;for documentation
    (inputs
     (list python xapian zlib))
    (synopsis "Python bindings for the Xapian search engine library")
    (license license:gpl2+)))

(define-public perl-search-xapian
  (package
    (name "perl-search-xapian")
    (version "1.2.25.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/O/OL/OLLY/"
                           "Search-Xapian-" version ".tar.gz"))
       (sha256
        (base32 "12xs22li1z10rccpxbb4zflkkdh7q37z9hb8nvx1ywfn2b3vskr0"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-devel-leak))
    (inputs
     (list xapian))
    (home-page "https://metacpan.org/release/Search-Xapian")
    (synopsis "Perl XS frontend to the Xapian C++ search library")
    (description
     "Search::Xapian wraps most methods of most Xapian classes.  The missing
classes and methods should be added in the future.  It also provides a
simplified, more 'perlish' interface to some common operations.")
    (license license:perl-license)))

(define-public libtocc
  (package
    (name "libtocc")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/aidin36/tocc/releases/download/"
                           "v" version "/tocc-" version ".tar.gz"))
       (sha256
        (base32
         "1kd2jd74m8ksc8s7hh0haz0q0c3n0mr39bbky262kk4l58f1g068"))))
    (build-system gnu-build-system)
    (native-inputs (list catch-framework))
    (inputs (list unqlite))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'chdir-source
                    (lambda _
                      (chdir "libtocc/src")
                      #t))
                  (replace 'check
                    (lambda _
                      (with-directory-excursion "../tests"
                        (invoke "./configure"
                                (string-append "CONFIG_SHELL="
                                               (which "sh"))
                                (string-append "SHELL="
                                               (which "sh"))
                                "CPPFLAGS=-I../src"
                                (string-append
                                 "LDFLAGS=-L../src/.libs "
                                 "-Wl,-rpath=../src/.libs"))
                        (invoke "make")
                        (invoke "./libtocctests")))))))
    (home-page "https://t-o-c-c.com/")
    (synopsis "Tool for Obsessive Compulsive Classifiers")
    (description
     "libtocc is the engine of the Tocc project, a tag-based file management
system.  The goal of Tocc is to provide a better system for classifying files
that is more flexible than classic file systems that are based on a tree of
files and directories.")
    (license license:gpl3+)))

(define-public tocc
  (package
    (name "tocc")
    (version (package-version libtocc))
    (source (package-source libtocc))
    (build-system gnu-build-system)
    (inputs
     (list libtocc unqlite))
    (arguments
     `(#:tests? #f                      ;No tests
       #:phases (modify-phases %standard-phases
                  (add-after
                   'unpack 'chdir-source
                   (lambda _ (chdir "cli/src"))))))
    (home-page "https://t-o-c-c.com/")
    (synopsis "Command-line interface to libtocc")
    (description
     "Tocc is a tag-based file management system.  This package contains the
command line tool for interacting with libtocc.")
    (license license:gpl3+)))

(define-public bool
  (package
    (name "bool")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/bool/bool-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1frdmgrmb509fxbdpsxxw3lvvwv7xm1pavqrqgm4jg698iix6xfw"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnu.org/software/bool/")
    (synopsis "Finding text and HTML files that match boolean expressions")
    (description
     "GNU Bool is a utility to perform text searches on files using Boolean
expressions.  For example, a search for \"hello AND world\" would return a
file containing the phrase \"Hello, world!\".  It supports both AND and OR
statements, as well as the NEAR statement to search for the occurrence of
words in close proximity to each other.  It handles context gracefully,
accounting for new lines and paragraph changes.  It also has robust support
for parsing HTML files.")
    (license license:gpl3+)))

(define-public dataparksearch
  (let ((commit "8efa28f31ce1273c0556fd5c7e06abe955197a69")
        (revision "0"))
    (package
      (name "dataparksearch")
      (version (git-version "4.54" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/Maxime2/dataparksearch")
                      (commit commit)))
                (sha256
                 (base32
                  "01z7s3ws5px2p9brzrq9j41jbdh1cvj8n8y3ghx45gfv1n319ipg"))
                (modules '((guix build utils)))
                (snippet
                 #~(for-each delete-file '("config.sub"
                                           "config.guess"
                                           "configure"
                                           "Makefile.in"
                                           "missing"
                                           "depcomp"
                                           "ltmain.sh"
                                           "compile")))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "--with-extra-charsets=all"
                (string-append "--with-aspell=" #$(this-package-input "aspell"))
                (string-append "--with-pgsql="
                               #$(this-package-input "postgresql")))
        #:make-flags
        #~(list "DPS_TEST_DBADDR=postgresql://localhost/tmp/postgresql/")))
      (native-inputs
       (list autoconf automake libtool openjade pkg-config))
      (inputs
       (list aspell
             c-ares
             libextractor
             mbedtls-lts
             postgresql
             zlib))
      (synopsis "Feature rich search engine")
      (description
       "Dataparksearch is a full featured web search engine.
It has support for HTTP, HTTPS, ftp (passive mode), NNTP and news URL schemes,
and other URL schemes with external parsers.  It can tweak URLs with session
IDs and other weird formats, including some JavaScript link decoding.  Options
to query with all words, all words near to each others, any words, or boolean
queries.  A subset of VQL (Verity Query Language) is supported.")
      (home-page "https://www.dataparksearch.org/")
      (license license:gpl2+))))

(define-public fsearch
  (package
    (name "fsearch")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cboxdoerfer/fsearch")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12vj1ymvg561594vdq852ianbkgnvrq585qp5jrrv2kq307jh5sl"))))
    (build-system meson-build-system)
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-resources
           intltool
           libtool
           pkg-config))
    (inputs
     (list gtk+ icu4c pcre2))
    (home-page "https://github.com/cboxdoerfer/fsearch")
    (synopsis "Fast file search utility")
    (description
     "FSearch is a fast file search utility, inspired by Everything
Search Engine.  It is written in C and based on GTK3.")
    (license license:gpl2+)))

(define-public recoll
  (package
    (name "recoll")
    (version "1.37.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.recoll.org/recoll-" version ".tar.gz"))
       (sha256
        (base32 "0m9ibpa27xrngk31kxhgqv878knw2xpigckx8pqvfzmfvqr81zdy"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-webkit"
              "--disable-python-module"
              "--without-systemd"
              "--with-inotify"
              "--enable-recollq"
              (string-append "QMAKEPATH=" #$(this-package-input "qtbase")
                             "/bin/qmake"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-LDFLAGS
            (lambda _
              (setenv "LDFLAGS" (string-append "-Wl,-rpath=" #$output "/lib"))))
          (add-after 'unpack 'patch-default-data-dir
            (lambda _
              (substitute* "python/recoll/recoll/rclconfig.py"
                (("/opt/local") #$output))))
          (add-after 'install 'wrap-filters
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((mapping
                     '(("rclps"
                        "poppler")
                       ("rclpdf.py"
                        "poppler")
                       ("rclpurple"
                        "gawk")
                       ("rcllyx"
                        "libiconv")
                       ("rcltex"
                        "libiconv")
                       ("rclkwd"
                        "unzip" "gzip" "tar" "libxslt")
                       ("rclman"
                        "groff")
                       ("rclgaim"
                        "gawk" "libiconv")
                       ("rclaptosidman"
                        "sed")
                       ("rclscribus"
                        "grep" "gawk" "sed"))))
                (for-each
                 (lambda (program packages)
                   (wrap-program (string-append #$output "/share/recoll/filters/" program)
                     `("PATH" ":" prefix
                       ,(map (lambda (i)
                               (string-append (assoc-ref inputs i) "/bin"))
                             packages))))
                 (map car mapping)
                 (map cdr mapping))

                (wrap-program (string-append #$output "/share/recoll/filters/rclimg")
                  `("PERL5LIB" ":" prefix
                    (,(getenv "PERL5LIB"))))))))))
    (inputs
     (list aspell
           chmlib
           inotify-tools
           libxslt
           libxml2
           python
           qtbase-5
           unzip
           xapian
           zlib
           ;; For filters
           gawk
           grep
           groff
           gzip
           libiconv
           perl
           perl-image-exiftool
           poppler
           sed
           tar))
    (native-inputs
     (list pkg-config qttools-5 which))
    (home-page "https://www.recoll.org")
    (synopsis "Find documents based on their contents or file names")
    (description "Recoll finds documents based on their contents as well as
their file names.  It can search most document formats, but you may need
external applications for text extraction.  It can reach any storage place:
files, archive members, email attachments, transparently handling
decompression.")
    (license license:gpl2+)))

(define-public hyperestraier
  (package
    (name "hyperestraier")
    (version "1.4.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "http://fallabs.com/" name "/"
                            name "-" version ".tar.gz"))
        (sha256
         (base32
          "1qk3pxgzyrpcz5qfyd5xs2hw9q1cbb7j5zd4kp1diq501wcj2vs9"))))
    (inputs
     (list qdbm zlib))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                              (assoc-ref %outputs "out")
                                              "/lib"))))
    (home-page "https://fallabs.com/hyperestraier")
    (synopsis "Full-text search system")
    (description "Hyper Estraier can be used to integrate full-text
search into applications, using either the provided command line and CGI
interfaces, or a C API.")
    (license license:lgpl2.1+)))

(define-public mlocate
  (package
    (name "mlocate")
    (version "0.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://releases.pagure.org/mlocate/"
                                  "mlocate-" version ".tar.xz"))
              (sha256
               (base32
                "0gi6y52gkakhhlnzy0p6izc36nqhyfx5830qirhvk3qrzrwxyqrh"))))
    (build-system gnu-build-system)
    (home-page "https://pagure.io/mlocate")
    (synopsis "Locate files on the file system")
    (description
     "mlocate is a locate/updatedb implementation.  The @code{m} stands for
\"merging\": @code{updatedb} reuses the existing database to avoid rereading
most of the file system, which makes it faster and does not trash the system
caches as much.  The locate(1) utility is intended to be completely compatible
with slocate, and attempts to be compatible to GNU locate when it does not
conflict with slocate compatibility.")
    (license license:gpl2)))

(define-public plocate
  (package
    (name "plocate")
    (version "1.1.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://plocate.sesse.net/download/"
                           "plocate-" version ".tar.gz"))
       (sha256
        (base32 "0j80zcklr7g73wsq54wbj8ggp8rj993hdzrywm2c0bmani0lfziv"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list
        ;; Put the database in /var/cache/plocate.db
        "--sharedstatedir=/var"
        "-Dinstall_systemd=false"
        "-Ddbpath=cache/plocate.db")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-cachedirtag
           (lambda _
             (substitute* "meson.build"
               ;; Remove the script adding a "cachedirtag"
               (("meson.add_install_script") "#")))))))
    (inputs
     (list liburing
           `(,zstd "lib")))
    (native-inputs
     (list pkg-config))
    (home-page "https://plocate.sesse.net/")
    (synopsis "Faster locate")
    (description "Plocate is a @code{locate} based on posting lists,
completely replacing @command{mlocate} with a faster and smaller index.  It is
suitable as a default locate on your system.")
    (license license:gpl2)))

(define-public swish-e
  (package
    (name "swish-e")
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (list (string-append
                          "https://web.archive.org/web/20160730145202/"
                          "http://swish-e.org/distribution/"
                          "swish-e-" version ".tar.gz")
                         (string-append "http://http.debian.net/debian/pool/"
                                        "main/s/swish-e/swish-e_" version
                                        ".orig.tar.gz")))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qkrk7z25yp9hynj21vxkyn7yi8gcagcfxnass5cgczcz0gm9pax"))
              (patches (search-patches "swish-e-search.patch"
                                       "swish-e-format-security.patch"))))
    (build-system gnu-build-system)
    ;; Several other packages and perl modules may be installed alongside
    ;; swish-e to extend its features at runtime, but are not required for
    ;; building: xpdf, catdoc, MP3::Tag, Spreadsheet::ParseExcel,
    ;; HTML::Entities.
    (inputs
     (list bash-minimal perl perl-uri perl-html-parser perl-html-tagset
           perl-mime-types))
    (arguments
     `(;; XXX: This fails to build with zlib (API mismatch) and tests fail
       ;; with libxml2, so disable both.
       #:configure-flags (list (string-append "--without-zlib")
                               (string-append "--without-libxml2"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-programs
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (for-each
                         (lambda (program)
                           (wrap-program program
                             `("PERL5LIB" ":" prefix
                               ,(map (lambda (i)
                                       (string-append (assoc-ref inputs i)
                                                      "/lib/perl5/site_perl"))
                                     ;; These perl modules have no propagated
                                     ;; inputs, so no further analysis needed.
                                     '("perl-uri"
                                       "perl-html-parser"
                                       "perl-html-tagset"
                                       "perl-mime-types")))))
                         (list (string-append out "/lib/swish-e/swishspider")
                               (string-append out "/bin/swish-filter-test")))
                        #t))))))
    (home-page (string-append "https://web.archive.org/web/20160730145202/"
                              "http://swish-e.org"))
    (synopsis "Web indexing system")
    (description
     "Swish-e is Simple Web Indexing System for Humans - Enhanced.  Swish-e
can quickly and easily index directories of files or remote web sites and
search the generated indexes.")
    (license license:gpl2+)))           ; with exception

(define-public xapers
  (package
    (name "xapers")
    (version "0.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://finestructure.net/xapers/releases/xapers-"
             version ".tar.gz"))
       (sha256
        (base32
         "0ykz6hn3qj46w3c99d6q0pi5ncq2894simcl7vapv047zm3cylmd"))))
    (build-system python-build-system)
    (propagated-inputs
     (list poppler python-urwid xclip xdg-utils))
    (inputs
     (list python-latexcodec
           python-pybtex
           python-pycurl
           python-pyyaml
           python-six
           python-xapian-bindings))
    (arguments
     `(#:modules ((ice-9 rdelim)
                  (guix build python-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (define (purge-term-support input output)
               (let loop ((line (read-line input)))
                 (if (string-prefix? "if [[ \"$term\"" line)
                     (begin (display "eval \"$cmd\"\n" output)
                            #t)
                     (begin (display (string-append line "\n") output)
                            (loop (read-line input))))))
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (adder-out (string-append bin "/xapers-adder"))
                    (man1 (string-append out "/share/man/man1")))
               (install-file "man/man1/xapers.1"  man1)
               (install-file "man/man1/xapers-adder.1" man1)
               ;; below is equivalent to setting --no-term option
               ;; permanently on; this is desirable to avoid imposing
               ;; an x-terminal installation on the user but breaks
               ;; some potential xapers-adder uses like auto browser
               ;; pdf handler, but user could instead still use
               ;; e.g. "xterm -e xapers-adder %F" for same use.
               ;; alternatively we could propagate xterm as an input
               ;; and replace 'x-terminal-emulator' with 'xterm'
               (call-with-input-file "bin/xapers-adder"
                 (lambda (input)
                   (call-with-output-file adder-out
                     (lambda (output)
                       (purge-term-support input output)))))
               (chmod adder-out #o555)))))))
    (home-page "https://finestructure.net/xapers/")
    (synopsis "Personal document indexing system")
    (description
     "Xapers is a personal document indexing system,
geared towards academic journal articles build on the Xapian search engine.
Think of it as your own personal document search engine, or a local cache of
online libraries.  It provides fast search of document text and
bibliographic data and simple document and bibtex retrieval.")
    (license license:gpl3+)))

(define-public ugrep
  (package
    (name "ugrep")
    (version "7.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Genivia/ugrep")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "0fjb8pd65zbr0sdljlcvdl59nb9pw8bqv44abpcbcwpk42lx8j35"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file-recursively "bin/win32") ; pre-built
                   (delete-file-recursively "bin/win64") ; pre-built
                   (for-each (lambda (regexp)
                               (for-each delete-file
                                         (find-files "tests" regexp)))
                             '("^archive" "\\.pdf$" "\\.class$"))))))
    (build-system gnu-build-system)
    (inputs
     (list bzip2
           less
           lz4
           lzip ;; lzma
           pcre2
           zlib
           brotli
           `(,zstd "lib")))
    (arguments
     (list
      #:tests? #f                  ; no way to rebuild the binary input files
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'check-setup
            (lambda _
              ;; Unpatch shebangs in tests.
              (substitute* '("tests/Hello.bat"
                             "tests/Hello.sh")
                (("#!/gnu/store/.*/bin/sh") "#!/bin/sh")))))))
    (home-page "https://github.com/Genivia/ugrep/")
    (synopsis "Faster grep with an interactive query UI")
    (description "Ugrep is a ultra fast searcher of file systems, text
and binary files, source code, archives, compressed files, documents, and
more.

While still being compatible with the standard GNU/BSD grep command-line
options, ugrep supports fuzzy search as well as structured and (adjustable)
colored output, piped through \"less\" for pagination.  An interactive query
UI allows refinement and has a built-in help (press F1).  Ugrep implements
multi-threaded and other techniques to speed up search, pattern-matching and
decompression.  Many pre-defined regexps ease searching e.g. C typdefs or XML
attributes.  Results can be output in several structured or self-defined
formats.")
    (license license:bsd-3)))

;;; search.scm ends here
