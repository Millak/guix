;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2016, 2018, 2021, 2024, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016-2018, 2020-2023, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017-2019, 2021, 2025 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2019, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020 Lu hux <luhux@outlook.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2022 ROCKTAKEY <rocktakey@gmail.com>
;;; Copyright © 2022, 2024 Runciter <runciter@whispers-vpn.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2025 Nigko Yerden <nigko.yerden@gmail.com>
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

(define-module (gnu packages dictionaries)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages education)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages search)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wordnet)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public cmudict
  (package
    (name "cmudict")
    (properties '((commit . "0f8072f814306c5ee4fbf992ed853601b12c01f9")
                  (revision . "0")))
    (version (git-version "0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cmusphinx/cmudict")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ri9r9ljbwv282lmv9cp3gmbwlanf99nhzvw83fjf12bc4nxl0qd"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'install-license-files))))
    (home-page "https://github.com/cmusphinx/cmudict")
    (synopsis "Pronouncing English dictionary")
    (description
     "CMUdict (the Carnegie Mellon Pronouncing Dictionary) is a free
pronouncing dictionary of English, suitable for uses in speech technology and
is maintained by the Speech Group in the School of Computer Science at
Carnegie Mellon University.")
    (license (license:fsdg-compatible
              "https://github.com/cmusphinx/cmudict/blob/master/LICENSE"))))

(define-public dico
  (package
    (name "dico")
    (version "2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/dico/dico-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1xvahrav8aml90qcj4cj3a33y0n7nm1k0ywgks1zy2q91v2qk2vj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--disable-static")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-shell-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              ;; This code invokes "/bin/sh -c 'm4 -s ...'".
              (substitute* "grecs/src/grecs-lex.c"
                (("\"/bin/sh\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/sh")
                                "\"")))))
          (add-before 'check 'silence-guile
            (lambda _
              ;; Guile is too talkative, which disturbs the test
              ;; infrastructure.  Gag it.
              (setenv "GUILE_AUTO_COMPILE" "0")
              (setenv "GUILE_WARN_DEPRECATED" "no"))))))
    (native-inputs (list groff))
    (inputs
     (list m4                           ;used at run time
           bash-minimal                 ;likewise
           pcre
           python-wrapper
           guile-3.0
           gsasl
           readline
           zlib
           wordnet
           libxcrypt                    ;for 'crypt'
           libltdl))
    (home-page "https://www.gnu.org.ua/software/dico/")
    (synopsis "Implementation of DICT server (RFC 2229)")
    (description
     "GNU Dico implements a flexible dictionary server and client according to
RFC 2229 (DICT Server).  It is able to access any database available,
regardless of format, thanks to its modular structure.  New modules may be
written in C, Guile or Python.  Dico also includes a command-line client,
which may be used to query remote dictionary databases.")
    (license license:gpl3+)))

(define-public libmaa
  (package
    (name "libmaa")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dict/libmaa/"
                           "libmaa-"
                           version
                           "/libmaa-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1idi4c30pi79g5qfl7rr9s17krbjbg93bi8f2qrbsdlh78ga19ar"))))
    (native-inputs (list libtool))
    (arguments
     (list
      ;; Change -Werror to -Wno-error, reproduce other default flags
      ;; Do not error out on warnings related to snprintf function
      #:make-flags #~'("CFLAGS=-DHAVE_CONFIG_H  -Wall -Wno-error -g -O2 $(VERCFLAGS) -I. -I${srcdir}")
      #:test-target "test"))
    (build-system gnu-build-system)
    (synopsis "Low-level data structures used by the dictd program")
    (description
     "The libmaa library provides many low-level
data structures which are helpful for writing compilers, including hash
tables, sets, lists, debugging support, and memory management.  Although
libmaa was designed and implemented as a foundation for the Khepara
transformation system, the data structures are generally applicable to a
wide range of programming problems.

The memory management routines are especially helpful for improving the
performance of memory-intensive applications.")
    (home-page "https://sourceforge.net/projects/dict/")
    (license license:gpl2+)))

(define-public dictd
  (package
    (name "dictd")
    (version "1.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dict/dictd/"
                           "dictd-"
                           version
                           "/dictd-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "0w8i7w3xs53kj5v72xf1zq24kz4qa6fcg1lmibs279wgnggjj88r"))))
    (inputs (list libmaa zlib))
    (native-inputs (list libtool bison flex))
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-configure
            (lambda _
              ;; workaround for missing yylex in yywrap lex check
              (substitute* "configure"
                (("yywrap [(]void[)];")
                 "yywrap (void); int yylex () { return 0; }")))))))
    (build-system gnu-build-system)
    (synopsis "@command{dict}, @command{dictd} and @command{dictfmt} programs")
    (description
     "The DICT Interchange Format (DICF) is a human-readable
 format for the interchange of dictionary databases for the use with
DICT protocol client/server software.

This package provides a client @command{dict} and a server program
@command{dictd} for the DICT protocol, as well as a utility
@command{dictfmt} to convert various dictionary formats into
dictionaries that can be served by @command{dictd} or Dico.")
    (home-page "https://sourceforge.net/projects/dict/")
    (license license:gpl2+)))

(define-public vera
  (package
    (name "vera")
    (version "1.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/vera/vera-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1j5p679vw72bv766acbg6g89k31ynmrzlpg7s3wzy4krlwdf92xc"))))
    (build-system trivial-build-system)
    (arguments
     `(#:builder (begin
                   (use-modules (guix build utils))

                   (let* ((out    (assoc-ref %outputs "out"))
                          (info   (string-append out "/share/info"))
                          (html   (string-append out "/share/html"))
                          (source (assoc-ref %build-inputs "source"))
                          (tar    (assoc-ref %build-inputs "tar"))
                          (gz     (assoc-ref %build-inputs "gzip"))
                          (texi   (assoc-ref %build-inputs "texinfo")))
                     (setenv "PATH" (string-append gz "/bin"))
                     (invoke (string-append tar "/bin/tar") "xvf" source)

                     (chdir (string-append "vera-" ,version))
                     (mkdir-p info)
                     (mkdir-p html)

                     ;; Change a ‘Malformed UTF-8 character: \xd7\x34 (unexpected
                     ;; non-continuation byte 0x34, immediately after start byte
                     ;; 0xd7; need 2 bytes, got 1) in pattern match (m//)’.
                     (substitute* "vera.h"
                       (("320.480") "320x480"))

                     ;; XXX: Use '--force' because the document is unhappy
                     ;; with Texinfo 5 (yes, documents can be unhappy.)
                     (invoke (string-append texi "/bin/makeinfo")
                             "vera.texi" "--force" "-o"
                             (string-append info "/vera.info"))
                     (invoke (string-append texi "/bin/makeinfo")
                             "vera.texi" "--force" "--html" "-o"
                             (string-append html "/vera.html"))))
      #:modules ((guix build utils))))
    (native-inputs (list texinfo tar gzip))
    (home-page "https://savannah.gnu.org/projects/vera/")
    (synopsis "List of acronyms")
    (description
     "V.E.R.A. (Virtual Entity of Relevant Acronyms) is a list of computing
acronyms distributed as an info document.")
    (license license:fdl1.3+)))

(define-public gcide
  (package
    (name "gcide")
    (version "0.54")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnu/gcide/gcide-" version ".tar.xz"))
              (sha256
               (base32
                "0hhxqlkgp3kkin4pqzfgfd24ckai29sm9dw8qc6icnqp6rpnyh92"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan
       '(("." "share/gcide/" #:exclude ("COPYING")))))
    (synopsis "GNU Collaborative International Dictionary of English")
    (description
     "GCIDE is a free dictionary based on a combination of sources.  It can
be used via the GNU Dico program or accessed online at
http://gcide.gnu.org.ua/")
    (home-page "https://gcide.gnu.org.ua/")
    (license license:gpl3+)))

(define-public diction
  ;; Not quite a dictionary, not quite a spell checker either…
  (package
    (name "diction")
    (version "1.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.moria.de/~michael/diction/diction-"
                           version ".tar.gz"))
       (sha256
        (base32 "1z6p5x3l1a00h4v4s33qa82fznzc1jdqdnlc4dnmd9nblnrjy0fs"))))
    (build-system gnu-build-system)
    (synopsis "Identifies wordy and commonly misused phrases")
    (description
     "A package providing two classic Unix commands, style and diction.
Diction is used to identify wordy and commonly misused phrases in a
body of text.  Style instead analyzes surface aspects of a written
work, such as sentence length and other readability measures.")
    (home-page "https://www.gnu.org/software/diction/")
    (license license:gpl3+)))

(define-public ding
  (package
    (name "ding")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.tu-chemnitz.de/pub/Local/urz/" name
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rcqn04l6hvsf15mqqai533p31nc04r2yd9s0mn2hnkqrwgwi9k9"))))
    (build-system gnu-build-system)
    (inputs (list tk))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((bindir (string-append
                            (assoc-ref %outputs "out") "/bin"))
                   (wish (search-input-file inputs "/bin/wish8.6"))
                   (sharedir (string-append
                              (assoc-ref %outputs "out")
                              "/share/applications"))
                   (libdir (string-append
                            (assoc-ref %outputs "out") "/lib")))
               (mkdir-p bindir)
               (mkdir-p libdir)
               (mkdir-p sharedir)

               (substitute* "ding.desktop"
                 (("Exec=/usr/bin/ding")
                  (string-append "Exec=" bindir "/ding")))
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* "ding" (("exec wish") (string-append "exec " wish))))
               (substitute* "install.sh"
                 (("/bin/cp") "cp")
                 (("/bin/mv") "mv")
                 (("NEEDPROG=\"wish\"")
                  (string-append "NEEDPROG=\"" wish "\""))
                 (("DEFBINDIR=\"/usr/local/bin\"")
                  (string-append "DEFBINDIR=\"" bindir "\""))
                 (("DEFLIBDIR=\"/usr/local/lib\"")
                  (string-append "DEFLIBDIR=\"" libdir "\"")))
               (install-file "ding.desktop" sharedir)
               (install-file "ding.png" sharedir)
               (invoke "./install.sh")))))))
    (synopsis "Dictionary lookup program with a German-English dictionary")
    (description "Ding is a dictionary lookup program for the X window system.
It comes with a German-English dictionary with approximately 270,000 entries.")
    (home-page  "https://www-user.tu-chemnitz.de/~fri/ding/")
    (license license:gpl2+)))

(define-public grammalecte
  (package
    (name "grammalecte")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri (string-append "https://grammalecte.net/zip/"
                           "Grammalecte-fr-v" version ".zip"))
       (sha256
        (base32 "0kv8h0116sfcrqnpd3jv2b098ychda7v27fciq1phxw50jbj395a"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;no tests provided with the project
    (native-inputs
     (list python-setuptools))
    (home-page "https://grammalecte.net")
    (synopsis "French spelling and grammar checker")
    (description "Grammalecte is a grammar checker for the French language,
derived from Lightproof.

Grammalecte helps writing a proper French, without distracting users with
false positives.  This grammar checker follows the principle: the less false
positives, the better; if it cannot know with a good chance that a dubious
expression is wrong, it keeps silent.

The package provides the command line interface, along with a server
and a Python library.")
    (license license:gpl3+)))

(define-public translate-shell
  (package
    (name "translate-shell")
    (version "0.9.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url"https://github.com/soimort/translate-shell")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jfrypcz963pfvwwaz2i0xvwp2909ldzp15v68mgd2mbqkqw9d90"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'unpack 'remove-unnecessary-file
           ;; This file gets generated during the build phase.
           (lambda _
             (delete-file "translate")))
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out     (assoc-ref outputs "out"))
                    (bin     (string-append out "/bin/trans"))
                    (curl    (assoc-ref inputs "curl"))
                    (fribidi (assoc-ref inputs "fribidi"))
                    (rlwrap  (assoc-ref inputs "rlwrap")))
               (wrap-program bin
                             `("PATH" ":" prefix
                               (,(string-append out "/bin:"
                                                curl "/bin:"
                                                fribidi "/bin:"
                                                rlwrap "/bin")))))))
         (add-after 'unpack 'emacs-make-autoloads
           (assoc-ref emacs:%standard-phases 'make-autoloads))
         (add-after 'install 'emacs-install
           (assoc-ref emacs:%standard-phases 'install))
         (add-after 'emacs-install 'emacs-build
           (assoc-ref emacs:%standard-phases 'build)))
       #:make-flags (list (string-append "PREFIX=" %output)
                          "NETWORK_ACCESS=no test")
       #:imported-modules (,@%default-gnu-imported-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils))
       #:test-target "test"))
    (inputs
     (list bash-minimal curl fribidi rlwrap))
    (native-inputs
     (list emacs-minimal
           util-linux))                 ; hexdump, for the test
    (home-page "https://www.soimort.org/translate-shell/")
    (synopsis "Translations from the command line")
    (description
     "Translate Shell (formerly Google Translate CLI) is a command-line
translator powered by Google Translate (default), Bing Translator,
Yandex.Translate and Apertium.  It gives you easy access to one of these
translation engines from your terminal.")
    (license license:public-domain)))


(define-public lttoolbox
  (package
    (name "lttoolbox")
    (version "3.7.6")
    (source
     (origin
       (method git-fetch)
       (file-name (git-file-name "lttoolbox" version))
       (uri
        (git-reference
          (url "https://github.com/apertium/lttoolbox")
          (commit (string-append "v" version))))
       (sha256
        (base32 "03dm7sdrggqkqsgdnqldv7fh5kbw4g6ssggfy4bgdh6n38997pag"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; The included ./autogen.sh unconditionally runs ./configure before
           ;; its shebangs have been patched.
           (lambda _
             (invoke "autoreconf" "-vfi"))))))
    (inputs
     (list libxml2 icu4c utfcpp-2))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://wiki.apertium.org/wiki/Lttoolbox")
    (synopsis "Lexical processing toolbox")
    (description "Lttoolbox is a toolbox for lexical processing, morphological
analysis and generation of words.  Analysis is the process of splitting a
word (e.g. cats) into its lemma \"cat\" and the grammatical information
@code{<n><pl>}.  Generation is the opposite process.")
    (license (list license:gpl2 ; main license
                   license:expat)))) ; utf8/*

(define-public apertium
  (package
    (name "apertium")
    (version "3.9.12")
    (source
       (origin
        (method git-fetch)
        (file-name (git-file-name name version))
        (uri
         (git-reference
          (url "https://github.com/apertium/apertium")
          (commit (string-append "v" version))))
        (sha256 (base32 "1wkb8dqcamk42y67plj77n8d27g5qlsnpkgxnkvm0wv2pr35gmzw"))))
    (build-system gnu-build-system)
    (inputs
     (list libxml2 libxslt lttoolbox pcre icu4c utfcpp-2 zip unzip
          (libc-utf8-locales-for-target)));; tests require UTF-8
    (native-inputs
     `(("apertium-get"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/apertium/apertium-get")
                 (commit "692d030e68008fc123089cf2446070fe8c6e3a3b")))
           (sha256
            (base32
             "0kgp68azvds7yjwfz57z8sa5094fyk5yr0qxzblrw7bisrrihnav"))))
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("flex" ,flex)
       ("pkg-config" ,pkg-config)
       ;; Only required for running the test suite:
       ("python" ,python)
       ("python-lxml" ,python-lxml)
       ("libzip" ,libzip)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; If apertium-get does not exist in the source tree, the build tries
         ;; to download it using an svn checkout. To avoid this, copy
         ;; apertium-get into the source tree.
         (add-after 'unpack 'unpack-apertium-get
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "apertium-get")
                               "apertium/apertium-get")))
         (add-after 'unpack 'reconf
           (lambda _
            (invoke "autoreconf" "-vfi")))
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "apertium/Makefile.am"
               (("xmllint") (search-input-file inputs "/bin/xmllint")))
             (substitute* "apertium/apertium-header.sh"
               (("locale -a")
                (string-append (search-input-file inputs "/bin/locale") " -a"))
               ;; 'locale -a' does not report properly in guix;
               ;; a  `guix shell -C glibc glibc-locales - locale -a`
               ;; will still only list C and POSIX
               ;; alternatively we could replace the `locale -a` invocations
               ;; with something along the lines of `{ locale -a ; echo $LC_ALL }`
               ;; or `{ locale -a; locale | grep LC_CTYPE | cut -d= -f2 | tr -d '"' }.`
               (("^locale_utf8\n$")
                "# locale_utf8 # disabled by guix \n" )
               ;; replace in everywhere but the shebang
               (("bash([^\n])" _ suffix)
                (string-append (search-input-file inputs "/bin/bash") suffix))
               (("\\b(grep|head|cat|rm|unzip|zip|gawk|awk|find|xmllint|lt-tmxproc)\\b" _ program)
                (search-input-file inputs (format #f "/bin/~a" program))))))
         ;; We want the shebang to be patched so that the invocation during
         ;; tests does not need /usr/bin/env
         (add-after 'build 'patch-apertium-shebang
           (lambda _ (patch-shebang "apertium/apertium"))))))
    (home-page "https://www.apertium.org/")
    (synopsis "Rule based machine translation system")
    (description "Apertium is a rule based machine translation system
featuring a shallow-transfer machine translation engine.  The design of the
system makes translations fast (translating tens of thousands of words per
second on ordinary desktop computers) and, in spite of the errors, reasonably
intelligible and easily correctable.")
    (license (list license:gpl2 ; main license
                   license:expat)))) ; utf8/*

(define-public sdcv
  (package
    (name "sdcv")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Dushistov/sdcv/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17jwcgc3jdp41rvxqi7zdsysfpjgzqq4z1l345qwffp1an6yaaqk"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_TESTS=YES"
              ;; https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=1078404
              "-DCMAKE_CXX_FLAGS=-fpermissive")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'build-lang
            (lambda _
              (invoke "make" "lang")))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gettext-minimal
           pkg-config
           ;; For tests.
           jq))
    (inputs
     (list glib ncurses readline zlib))
    (home-page "https://dushistov.github.io/sdcv/")
    (synopsis "Console version of StarDict")
    (description "sdcv is simple text-based utility for work with dictionaries
in StarDict's format.")
    (license license:gpl2+)))

(define-public skk-jisyo
  (let ((commit "38c81dbc74cdbdead843364023dea837f239a48c")
        (revision "0"))
    (package
      (name "skk-jisyo")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/skk-dev/dict")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "121j73bgd7fvj7zvaidqvgzfdvimig3f0vsyfv4vndwfgskh2r7z"))))
      (build-system copy-build-system)
      (arguments
       '(#:install-plan '(("./" "share/skk"
                           #:include-regexp ("SKK-JISYO\\.*")
                           #:exclude-regexp ("\\.gz$" "\\.md5$")
                           #:exclude ("SKK-JISYO.pubdic+"
                                      "SKK-JISYO.wrong.annotated"
                                      "SKK-JISYO.wrong")))
         #:phases (modify-phases %standard-phases
                    (add-before 'install 'decompress
                      (lambda* (#:key outputs #:allow-other-keys)
                        (map (lambda (arg)
                               (invoke "gzip" "-v" "-d" arg))
                             (find-files "." "SKK-JISYO\\..*\\.gz$"))
                        (invoke "tar" "xvf" "zipcode.tar.gz"))))))
      (home-page "https://skk-dev.github.io/dict/")
      (synopsis "Jisyo (dictionary) files for the SKK Japanese-input software")
      (description
       "This package provides @file{SKK-JISYO.L}, the standard dictionary file
for SKK Japanese input systems, and various dictionary files.
@file{SKK-JISYO.L} can be used with @code{emacs-ddskk} or @code{uim} package.")
      (license license:gpl2+))))

(define-public freedict-tools
  (package
    (name "freedict-tools")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/freedict/tools")
             (commit "3596640e6e0582cc5fb76a342e5d8e7413aa4b34"))) ;"0.6.0" tag
       (file-name (git-file-name name version))
       (sha256
        (base32 "1raayynvn1j8x0ck8pnbbljl6zxnsyzzil7y54xz03dpj7k9w7mk"))
       (patches (search-patches "freedict-tools-fix-determinism.patch"))))
    (inputs (list espeak-ng
                  python
                  perl
                  gzip
                  tar
                  libxslt
                  dictd
                  perl-libxml))
    (arguments
     (list
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   (add-before 'build 'set-prefix-in-makefile
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("PREFIX \\?=.*")
                          (string-append "PREFIX = "
                                         #$output "\n")))
                       (substitute* "mk/dicts.mk"
                         (("available_platforms := src dictd slob")
                          "available_platforms := dictd")))))))
    (build-system gnu-build-system)
    (synopsis "Build and manage FreeDict dictionaries")
    (description
     "FreeDict is a project that offers over 140 free
dictionaries in about 45 languages, with the right to study, change and
modify them.  You can use them offline on your computer or mobile phone
and export them to any format and application.

In order to limit store size and build complexity, only the build
targets that build dictionaries in dictd format are retained when this
Guix package is installed.")
    (home-page "https://freedict.org")
    (license license:gpl2+)))

(define-public freedict-dictionaries
  (let ((commit "914b5f754b695e9422bf951837b0682a077e244e")
        (revision "0"))
    (package
      (name "freedict-dictionaries")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/freedict/fd-dictionaries")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0a8k5rq94rl1nmz0354sx2gmyqica0yjavirh5v5wdybkfq8nv83"))))
      (inputs (list espeak-ng libxslt dictd perl-libxml freedict-tools))
      (native-inputs (list python perl))
      (arguments
       (list
        ;; "validation" target fails because of hardcoded
        ;; path /usr/bin/xmllint in freedict-tools
        #:tests? #f
        #:make-flags #~(list (string-append "PREFIX="
                                            #$output))
        #:phases #~(modify-phases %standard-phases
                     (delete 'configure)
                     (add-before 'build 'set-tools-prefix-in-makefile
                       (lambda* (#:key inputs #:allow-other-keys)
                         (substitute* "Makefile"
                           (("FREEDICT_TOOLS \\?= ../tools")
                            (string-append "export FREEDICT_TOOLS = "
                                           #$(file-append
                                              (this-package-input "freedict-tools")
                                              "/share/freedict")))))))))
      (build-system gnu-build-system)
      (synopsis "Multilingual dictionaries compiled to the DICT format")
      (description
       "FreeDict is a project that offers over 140 free
 dictionaries in about 45 languages, with the right to study, change and
 modify them.  You can use them offline on your computer or mobile phone
 and export them to any format and application.")
      (home-page "https://freedict.org")
      (license license:gpl2+))))

(define-public python-cmudict
  (package
    (name "python-cmudict")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/prosegrinder/python-cmudict")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ia9flqchgr975zjc9l1p379sglg9b56q9m09n05sag8368409k4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                ((".*importlib-resources.*")
                 ""))))
          (add-after 'ensure-no-mtimes-pre-1980 'inject-data
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((cmudict (search-input-file inputs "cmudict.dict")))
                (rmdir "src/cmudict/data")
                (copy-recursively (dirname cmudict) "src/cmudict/data"))))
          (add-after 'install 'replace-by-symlink
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((cmudict (search-input-file inputs "cmudict.dict"))
                    (target (string-append (site-packages inputs outputs)
                                           "/cmudict/data")))
                (delete-file-recursively target)
                (symlink (dirname cmudict) target)))))))
    (native-inputs (list python-poetry-core python-pytest python-setuptools))
    (inputs (list cmudict))
    (home-page "https://github.com/prosegrinder/python-cmudict")
    (synopsis "Python wrapper for The CMU Pronouncing Dictionary data files")
    (description
     "This package provides a versioned python wrapper package for The CMU
Pronouncing Dictionary data files.")
    (license license:gpl3+)))

(define-public goldendict-ng
  (let ((commit "2cfc27361d061103a164705e7f85dbdf6cd6056f")
        (revision "0"))
    (package
      (name "goldendict-ng")
      (version (git-version "25.10.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/xiaoyifang/goldendict-ng")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0r3aykiwg1jxam72mn1yrgmndnbf0r6nz5l1brqyqbr4wq3ir0p2"))
         (modules '((guix build utils)))
         (snippet #~(for-each delete-file-recursively
                              (list "thirdparty/eb"
                                    "thirdparty/fmt"
                                    "thirdparty/tomlplusplus")))))
      (build-system qt-build-system)
      (arguments
       (list #:qtbase qtbase
             #:configure-flags
             #~(list
                ;; use system fmt and toml++ instead of the bundled ones
                "-DUSE_SYSTEM_FMT=ON"
                "-DUSE_SYSTEM_TOML=ON")
             ;; no tests
             #:tests? #f))
      (native-inputs (list pkg-config qttools))
      (inputs (list `(,zstd "lib")
                    bzip2
                    ffmpeg
                    fmt
                    hunspell
                    icu4c
                    libeb
                    libvorbis
                    libx11
                    libxtst
                    libzim
                    lzo
                    opencc
                    qt5compat
                    qtmultimedia
                    qtsvg
                    qtwebchannel
                    qtwebengine
                    tomlplusplus
                    xapian
                    xz
                    zlib))
      (synopsis "Advanced dictionary lookup program")
      (description "GoldenDict-ng is an advanced dictionary lookup program
that supports many dictionary formats, such as MDX, DSL, StarDict, Zim, etc.,
as well as special types of \"dictionaries\", such as external program,
website, audio files.  Among ather features are full text search,
Anki integration, transliteration for some languages, word stemming and
spelling correction via Hunspell's morphology analysis, unicode case,
diacritics, punctuation and whitespace folding.  It is forked from the
original GoldenDict which was developed at http://goldendict.org/.")
      (home-page "https://xiaoyifang.github.io/goldendict-ng/")
      (license license:gpl3+))))
