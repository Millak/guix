;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christopher Allan Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017 ng0 <ng0@no-reply.pragmatique.xyz>
;;; Copyright © 2016 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016, 2017 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
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

(define-module (gnu packages mail)
  #:use-module (gnu packages)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages django)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages search)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages docbook)
  #:use-module ((guix licenses)
                #:select (gpl2 gpl2+ gpl3 gpl3+ lgpl2.1 lgpl2.1+ lgpl3+
                           non-copyleft (expat . license:expat) bsd-3
                           public-domain bsd-4 isc (openssl . license:openssl)
                           bsd-2 x11-style agpl3 asl2.0))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial))

(define-public mailutils
  (package
    (name "mailutils")
    (version "3.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mailutils/mailutils-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "0c06yj5hgqibi24ib9sx865kq6i1h18wn201g6iwcfbpi2a7psdm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
           (lambda _
             ;; Use the right file name for `cat'.
             (substitute* "testsuite/lib/mailutils.exp"
               (("/bin/cat")
                (which "cat")))

             ;; Tests try to invoke 'maidag' such that it looks up the
             ;; 'root' user, which does not exist in the build
             ;; environment.
             (substitute* "maidag/tests/testsuite"
               (("root <")         "nobody <")
               (("spool/root")     "spool/nobody")
               (("root@localhost") "nobody@localhost"))

             ;; The 'pipeact.at' tests generate a shell script; make
             ;; sure it uses the right shell.
             (substitute* '("sieve/tests/testsuite"
                            "mh/tests/testsuite")
               (("#! /bin/sh")
                (string-append "#!" (which "sh"))))

             (substitute* "mh/tests/testsuite"
               (("moreproc: /bin/cat")
                (string-append "moreproc: " (which "cat"))))

             ;; XXX: The comsatd tests rely on being able to open
             ;; /dev/tty, but that gives ENODEV in the build
             ;; environment.  Thus, ignore test failures here.
             (substitute* "comsat/tests/Makefile.in"
               (("\\$\\(SHELL\\) \\$\\(TESTSUITE\\)" all)
                (string-append "-" all)))

             #t)))
       ;; TODO: Add `--with-sql'.
       #:configure-flags '("--sysconfdir=/etc")
       #:parallel-tests? #f))
    (inputs
     `(("dejagnu" ,dejagnu)
       ("m4" ,m4)
       ("texinfo" ,texinfo)
       ("guile" ,guile-2.0)
       ("gnutls" ,gnutls)
       ("ncurses" ,ncurses)
       ("readline" ,readline)
       ("linux-pam" ,linux-pam)
       ("libltdl" ,libltdl)
       ("gdbm" ,gdbm)))
    (home-page "https://mailutils.org")
    (synopsis "Utilities and library for reading and serving mail")
    (description
     "GNU Mailutils is a collection of programs for managing, viewing and
processing electronic mail.  It contains both utilities and server daemons
and all operate in a protocol-agnostic way.  The underlying libraries are
also available, simplifying the addition of mail capabilities to new
software.")
    (license
     ;; Libraries are under LGPLv3+, and programs under GPLv3+.
     (list gpl3+ lgpl3+))))

(define-public fetchmail
  (package
    (name "fetchmail")
    (version "6.3.26")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://sourceforge/fetchmail/branch_6.3/fetchmail-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0l78ayvi9dm8hd190gl139cs2xqsrf7r9ncilslw20mgvd6cbd3r"))))
    (build-system gnu-build-system)
    (inputs
     `(("openssl" ,openssl)))
    (arguments
     `(#:configure-flags (list (string-append "--with-ssl="
                                              (assoc-ref %build-inputs "openssl")))))
    (home-page "http://www.fetchmail.info/")
    (synopsis "Remote-mail retrieval and forwarding utility")
    (description
     "Fetchmail is a full-featured, robust, well-documented remote-mail
retrieval and forwarding utility intended to be used over on-demand
TCP/IP links (such as SLIP or PPP connections).  It supports every
remote-mail protocol now in use on the Internet: POP2, POP3, RPOP, APOP,
KPOP, all flavors of IMAP, ETRN, and ODMR.  It can even support IPv6
and IPSEC.

Fetchmail retrieves mail from remote mail servers and forwards it via SMTP,
so it can then be read by normal mail user agents such as mutt, elm
or BSD Mail.  It allows all your system MTA's filtering, forwarding, and
aliasing facilities to work just as they would on normal mail.")
    (license gpl2+))) ; most files are actually public domain or x11

(define-public mutt
  (package
    (name "mutt")
    (version "1.8.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://bitbucket.org/mutt/mutt/downloads/"
                                 "mutt-" version ".tar.gz"))
             (sha256
              (base32
               "1b8dggq5x1b77a9i9250b3jhv2iddfzhr9rix1yfzckdms65mr8b"))
             (patches (search-patches "mutt-store-references.patch"))))
    (build-system gnu-build-system)
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("gdbm" ,gdbm)
       ("gpgme" ,gpgme)
       ("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("perl" ,perl)))
    (arguments
     `(#:configure-flags '("--enable-smtp"
                           "--enable-imap"
                           "--enable-pop"
                           "--enable-gpgme"
                           "--enable-hcache" ; for header caching
                           "--enable-sidebar"
                           "--with-ssl"
                           "--with-sasl"
                           ;; so that mutt does not check whether the path
                           ;; exists, which it does not in the chroot
                           "--with-mailpath=/var/mail")))
    (home-page "http://www.mutt.org/")
    (synopsis "Mail client")
    (description
     "Mutt is a small but very powerful text-based mail client for Unix
operating systems.")
    (license gpl2+)))

(define-public neomutt
  (package
    (inherit mutt)
    (name "neomutt")
    (version "20170421")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/" name "/" name
                           "/archive/" name "-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "09f1abad0vdn08x80hadjccjpnzcbn5fjpj749gb819biyqkl0y2"))))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("gdbm" ,gdbm)
       ("gpgme" ,gpgme)
       ("ncurses" ,ncurses)
       ("gnutls" ,gnutls)
       ("openssl" ,openssl) ;For smime
       ("perl" ,perl)
       ("kyotocabinet" ,kyotocabinet)
       ("libxslt" ,libxslt)
       ("libidn" ,libidn)
       ("libxml2" ,libxml2)
       ("lmdb" ,lmdb)
       ("docbook-xsl" ,docbook-xsl)
       ("notmuch" ,notmuch)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list "--enable-smtp"
             "--enable-imap"
             "--enable-pop"
             "--enable-gpgme"

             ;; database, implies header caching
             "--without-tokyocabinet"
             "--without-qdbm"
             "--without-bdb"
             "--with-lmdb"
             (string-append "--with-kyotocabinet="
                            (assoc-ref %build-inputs "kyotocabinet"))
             "--with-gdbm"

             "--with-gnutls"
             "--without-ssl"
             "--with-sasl"

             "--with-regex"
             "--enable-smime"
             "--enable-notmuch"
             "--with-idn"

             ;; If we do not set this, neomutt wants to check
             ;; whether the path exists, which it does not
             ;; in the chroot. The workaround is this.
             "--with-mailpath=/var/mail"

             "--with-external-dotlock"
             "--enable-nntp"
             "--enable-compressed"

             (string-append "--with-curses="
                            (assoc-ref %build-inputs "ncurses")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoconf
           (lambda _
             (zero? (system* "sh" "autoreconf" "-vfi")))))))
    (home-page "https://www.neomutt.org/")
    (synopsis "Command-line mail reader based on Mutt")
    (description
     "NeoMutt is a command-line mail reader which is based on mutt.
It adds a large amount of new and improved features to mutt.")))

(define-public gmime
  (package
    (name "gmime")
    (version "2.6.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gmime/"
                                  (version-major+minor version)
                                  "/gmime-" version ".tar.xz"))
              (sha256
               (base32
                "0slzlzcr3h8jikpz5a5amqd0csqh2m40gdk910ws2hnaf5m6hjbi"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gnupg" ,gnupg)))               ; for tests only
    (inputs `(("glib" ,glib)
              ("gpgme" ,gpgme)
              ("zlib" ,zlib)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'patch-paths-in-tests
          (lambda _
            ;; The test programs run several programs using 'system' with
            ;; hard-coded paths.  Here we patch them all.
            ;; We use ISO-8859-1 here because test-iconv.c contains
            ;; raw byte sequences in several different encodings.
            (with-fluids ((%default-port-encoding #f))
              (substitute* (find-files "tests" "\\.c$")
                (("(system *\\(\")(/[^ ]*)" all pre prog-path)
                 (let* ((base (basename prog-path))
                        (prog (which base)))
                   (string-append pre
                                  (or prog (error "not found: " base))))))))))))
    (home-page "http://spruce.sourceforge.net/gmime/")
    (synopsis "MIME message parser and creator library")
    (description
     "GMime provides a core library and set of utilities which may be used for
the creation and parsing of messages using the Multipurpose Internet Mail
Extension (MIME).")
    (license (list lgpl2.1+ gpl2+ gpl3+))))

(define-public bogofilter
  (package
    (name "bogofilter")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/bogofilter/bogofilter-"
                                  version "/bogofilter-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "1d56n2m9inm8gnzm88aa27xl2a7sp7aff3484vmflpqkinjqf0p1"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             (substitute* "src/tests/t.frame"
               (("GREP=/bin/grep")
                (string-append "GREP=" (which "grep") "\n")))
             #t)))))
    (native-inputs `(("flex" ,flex)))
    (inputs `(("bdb" ,bdb)))
    (home-page "http://bogofilter.sourceforge.net/")
    (synopsis "Mail classifier based on a Bayesian filter")
    (description
     "Bogofilter is a mail filter that classifies mail as spam or ham
 (non-spam) by a statistical analysis of the message's header and
content (body).  The program is able to learn from the user's classifications
and corrections.  It is based on a Bayesian filter.")
    (license gpl2)))

(define-public offlineimap
  (package
    (name "offlineimap")
    (version "7.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OfflineIMAP/offlineimap/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1r0sbgwyirpbks82ri9g88raf3mp8shq9rg0r92gkr7h6888v6fw"))))
    (build-system python-build-system)
    (native-inputs
     `(("asciidoc" ,asciidoc)))
    (inputs `(("python2-pysqlite" ,python2-pysqlite)
              ("python2-six" ,python2-six)))
    (arguments
     ;; The setup.py script expects python-2.
     `(#:python ,python-2
      ;; Tests require a modifiable IMAP account.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-documentation
           (lambda _
             (substitute* "docs/Makefile"
               ;; Prevent xmllint and xsltproc from downloading a DTD file.
               (("a2x -v") "a2x --no-xmllint --xsltproc-opts=--nonet -v"))
             (zero? (system* "make" "-C" "docs" "man"))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (install-file "docs/offlineimap.1" (string-append man "/man1"))
               (install-file "docs/offlineimapui.7" (string-append man "/man7"))
               #t)))
         (add-after 'install-documentation 'wrap-binary
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/offlineimap")))
               (wrap-program bin
                 `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH"))))
               #t))))))
    (home-page "http://www.offlineimap.org")
    (synopsis "Sync emails between two repositories")
    (description
     "OfflineImap synchronizes emails between two repositories, so that you
can read the same mailbox from multiple computers.  It supports IMAP as REMOTE
repository and Maildir/IMAP as LOCAL repository.")
    (license gpl2+)))

(define-public emacs-mew
  (package
    (name "emacs-mew")
    (version "6.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mew.org/Release/mew-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03fzky2kz73vgx4cbps2psbbnrgqgkk5q7jwfldisymkzr9iz03y"))))
    (native-inputs
     `(("emacs" ,emacs)))
    (propagated-inputs
     `(("ruby-sqlite3" ,ruby-sqlite3) ; optional for the database of messages
       ("ruby" ,ruby))) ; to set GEM_PATH so ruby-sqlite3 is found at runtime
    (build-system gnu-build-system)
    (arguments
     (let ((elisp-dir "/share/emacs/site-lisp/guix.d/mew")
           (icon-dir  "/share/mew"))
       `(#:modules ((guix build gnu-build-system)
                    (guix build utils)
                    (guix build emacs-utils))
         #:imported-modules (,@%gnu-build-system-modules
                             (guix build emacs-utils))
         #:configure-flags
         (list (string-append "--with-elispdir=" %output ,elisp-dir)
               (string-append "--with-etcdir=" %output ,icon-dir))
         #:phases
         (modify-phases %standard-phases
           (add-after 'configure 'patch-mew-icon-directory
             (lambda* (#:key outputs #:allow-other-keys)
               (emacs-substitute-sexps "mew-key.el"
                 ("(def.* mew-icon-directory"
                  `(progn
                    (add-to-list 'image-load-path 'mew-icon-directory)
                    ,(string-append (assoc-ref outputs "out") ,icon-dir))))
               #t))
           (add-after 'install 'generate-autoloads
             (lambda* (#:key outputs #:allow-other-keys)
               (emacs-generate-autoloads
                "mew" (string-append (assoc-ref outputs "out") ,elisp-dir))
               #t)))
         #:tests? #f)))
    (home-page "http://www.mew.org")
    (synopsis "Emacs e-mail client")
    (description "Mew (Messaging in the Emacs World) is a user interface
for text messages, multimedia messages (MIME), news articles and
security functionality including PGP, S/MIME, SSH, and SSL.")
    (license bsd-3)))

(define-public mu
  (package
    (name "mu")
    (version "0.9.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/djcb/mu/releases/"
                                  "download/" version "/mu-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02g82zvxfgn17wzy846bfxj0izjj7yklhwdnhwxy1y2kin4fqnb5"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("glib" ,glib "bin")             ; for gtester
       ("emacs" ,emacs-minimal)))
    ;; TODO: Add webkit and gtk to build the mug GUI.
    (inputs
     `(("xapian" ,xapian)
       ("guile" ,guile-2.0)
       ("glib" ,glib)
       ("gmime" ,gmime)
       ("tzdata" ,tzdata)))             ;for mu/test/test-mu-query.c
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-configure
           ;; By default, elisp code goes to "share/emacs/site-lisp/mu4e",
           ;; so our Emacs package can't find it.  Setting "--with-lispdir"
           ;; configure flag doesn't help because "mu4e" will be added to
           ;; the lispdir anyway, so we have to modify "configure.ac".
           (lambda _
             (substitute* "configure"
               (("^ +lispdir=\"\\$\\{lispdir\\}/mu4e/\".*") ""))
             #t))
         (add-before 'check 'check-tz-setup
           (lambda* (#:key inputs #:allow-other-keys)
             ;; For mu/test/test-mu-query.c
             (setenv "TZDIR"
                     (string-append (assoc-ref inputs "tzdata")
                                    "/share/zoneinfo"))
             #t))
         (add-after 'install 'install-emacs-autoloads
           (lambda* (#:key outputs #:allow-other-keys)
             (emacs-generate-autoloads
              "mu4e"
              (string-append (assoc-ref outputs "out")
                             "/share/emacs/site-lisp"))
             #t)))))
    (home-page "http://www.djcbsoftware.nl/code/mu/")
    (synopsis "Quickly find emails")
    (description
     "Mu is a tool for dealing with e-mail messages stored in the
Maildir-format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
    (license gpl3+)))

(define-public alot
  (package
    (name "alot")
    (version "0.4")
    (source (origin
              (method url-fetch)
              ;; package author intends on distributing via github rather
              ;; than pypi:
              ;; https://github.com/pazz/alot/issues/877#issuecomment-230173331
              (uri (string-append "https://github.com/pazz/alot/archive/"
                                  version ".tar.gz"))
              (file-name (string-append "alot-" version ".tar.gz"))
              (sha256
               (base32
                "0sl1kl2fhkv208llnbny4blcvrfdk4vx6bcw5pnyh9ylwb0pipi2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests
       ;; python 3 is currently unsupported, more info:
       ;; https://github.com/pazz/alot/blob/master/docs/source/faq.rst
       #:python ,python-2))
    (inputs
     `(("python2-magic" ,python2-magic)
       ("python2-configobj" ,python2-configobj)
       ("python2-twisted" ,python2-twisted)
       ("python2-urwid" ,python2-urwid)
       ("python2-urwidtrees" ,python2-urwidtrees)
       ("python2-pygpgme" ,python2-pygpgme)
       ("python2-notmuch" ,python2-notmuch)))
    (home-page "https://github.com/pazz/alot")
    (synopsis "Commandline MUA using notmuch")
    (description
     "Alot is an experimental terminal mail user agent (MUA) based on
@code{notmuch} mail.  It is written in Python using the @code{urwid} toolkit.")
    (license gpl3+)))

(define-public notifymuch
  (let
      ((commit "9d4aaf54599282ce80643b38195ff501120807f0")
       (revision "1"))
    (package
      (name "notifymuch")
      (version (string-append "0.1-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kspi/notifymuch.git")
               (commit commit)))
         (sha256
          (base32
           "1lssr7iv43mp5v6nzrfbqlfzx8jcc7m636wlfyhhnd8ydd39n6k4"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (inputs
       `(("python-notmuch" ,python-notmuch)
         ("python-pygobject" ,python-pygobject)
         ("gobject-introspection" ,gobject-introspection)
         ("libnotify" ,libnotify)
         ("gtk+" ,gtk+)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-binary
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/notifymuch")))
                 (wrap-program bin
                   `("PYTHONPATH" ":" prefix (,(getenv "PYTHONPATH")))
                   `("GI_TYPELIB_PATH" ":" prefix
                     (,(getenv "GI_TYPELIB_PATH")
                      ,(string-append out "/lib/girepository-1.0")))))
               #t)))))
      (home-page "https://github.com/kspi/notifymuch")
      (synopsis "Displays notifications for changes in the notmuch email database")
      (description "notifymuch displays desktop notifications for messages in
the notmuch database.  The notifications are sent using libnotify to a
notification daemon.  The query to find messages to send a notification about
is configurable, and a notification for the same message will not be send
within a configurable period (defaults to 48 hours).  To use notifymuch, run
@command{notifymuch} after new mail is indexed, this can be automated by
invoking @command{notifymuch} from the post-new hook.")
      (license gpl3))))

(define-public notmuch
  (package
    (name "notmuch")
    (version "0.24.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://notmuchmail.org/releases/notmuch-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "18rw0rim6zxhnr2nggial029x4raaxqcgf9klfbdhv89qvi7s4gs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags (list "V=1") ; Verbose test output.
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-notmuch-lib.el
                    (lambda _
                      (substitute* "emacs/notmuch-lib.el"
                        (("/bin/sh") (which "sh")))
                      #t))
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (setenv "CC" "gcc")
                      (setenv "CONFIG_SHELL" (which "sh"))

                      (let ((out (assoc-ref outputs "out")))
                        (zero? (system* "./configure"
                                        (string-append "--prefix=" out))))))
                  (add-before 'check 'prepare-test-environment
                    (lambda _
                      (setenv "TEST_CC" "gcc")
                      ;; Patch various inline shell invocations.
                      (substitute* (find-files "test" "\\.sh$")
                        (("/bin/sh") (which "sh")))
                      #t)))))
    (native-inputs
     `(("bash-completion" ,bash-completion)
       ("emacs" ,emacs-no-x) ; Minimal lacks libxml, needed for some tests.
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("python-docutils" ,python2-docutils)
       ("python-sphinx" ,python2-sphinx)

       ;; The following are required for tests only.
       ("which" ,which)
       ("dtach" ,dtach)
       ("gnupg" ,gnupg)
       ("man" ,man-db)
       ("perl" ,perl)))
    (inputs
     `(("glib" ,glib)
       ("gmime" ,gmime)
       ("talloc" ,talloc)
       ("xapian" ,xapian)
       ("zlib" ,zlib)))
    (home-page "https://notmuchmail.org/")
    (synopsis "Thread-based email index, search, and tagging")
    (description
     "Notmuch is a command-line based program for indexing, searching, read-
ing, and tagging large collections of email messages.")
    (license gpl3+)))

(define-public notmuch-addrlookup-c
  (package
    (name "notmuch-addrlookup-c")
    (version "7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/aperezdc/" name "/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rslg2ifgyhl6asv3yr1f62m9xjfcinv7i6qb07h2k217jqlmrri"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc"
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  ;; Remove vim code completion config, it's not needed to
                  ;; build (or be patched).
                  (add-before 'patch-source-shebangs 'delete-ycm-file
                              (lambda _ (delete-file ".ycm_extra_conf.py")))
                  (replace 'install
                           (lambda* (#:key outputs #:allow-other-keys)
                             (let ((bin (string-append
                                         (assoc-ref outputs "out") "/bin")))
                               (install-file "notmuch-addrlookup" bin)))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("notmuch" ,notmuch)))
    (home-page "https://github.com/aperezdc/notmuch-addrlookup-c")
    (synopsis "Address lookup tool for Notmuch")
    (description "This is an address lookup tool using a Notmuch database,
useful for email address completion.")
    (license license:expat)))

(define-public python-notmuch
  (package
    (name "python-notmuch")
    (version (package-version notmuch))
    ;; Notmuch python bindings are now unavailable on pypi.  The
    ;; bindings are distributed via the notmuch release tarball.
    (source (package-source notmuch))
    (build-system python-build-system)
    (inputs `(("notmuch" ,notmuch)))
    (arguments
     `(#:tests? #f  ; no "test" target
       #:phases
       (modify-phases %standard-phases
         ;; This python package lives in a subdirectory of the notmuch source
         ;; tree, so chdir into it before building.
         (add-after 'unpack 'enter-python-dir
           (lambda _ (chdir "bindings/python") #t))
         ;; Make sure the correct notmuch shared library gets loaded.
         (add-before 'build 'set-libnotmuch-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((notmuch (assoc-ref inputs "notmuch")))
               (substitute* "notmuch/globals.py"
                 (("libnotmuch\\.so\\.")
                  (string-append notmuch "/lib/libnotmuch.so.")))
               #t))))))
    (home-page (package-home-page notmuch))
    (synopsis "Python bindings of the Notmuch mail indexing library")
    (description
     "This package provides Python bindings to use the Notmuch mail indexing
and search library.")
    (license gpl3+)))

(define-public python2-notmuch
  (package-with-python2 python-notmuch))

(define-public getmail
  (package
    (name "getmail")
    (version "4.52.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pyropus.ca/software/getmail/old-versions/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32
         "0pzplrlxwbxydvfw4kkwn60l40hk1h5sxawaa6pi0k75c220k4ni"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:python ,python-2))
    (home-page "http://pyropus.ca/software/getmail/")
    (synopsis "Mail retriever")
    (description
     "A flexible, extensible mail retrieval system with support for
POP3, IMAP4, SSL variants of both, maildirs, mboxrd files, external MDAs,
arbitrary message filtering, single-user and domain-mailboxes, and many other
useful features.")

    ;; License is specified in file '__init__.py'.
    (license gpl2)))

(define-public libetpan
  (package
    (name "libetpan")
    (version "1.7.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/dinhviethoa/" name
                   "/archive/" version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
               (base32 "081ixgj3skglq9i7v0jb835lmfx21zi4i5b7997igwr0lj174y9j"))))
    (build-system gnu-build-system)
    (native-inputs `(("autoconf" ,(autoconf-wrapper))
                     ("automake" ,automake)
                     ("libtool" ,libtool)
                     ("pkg-config" ,pkg-config)))
    (propagated-inputs
     ;; 'libetpan-config --libs' returns '-lssl -lcrypto -lsasl2', so these
     ;; libraries need to be propagated.
     `(("cyrus-sasl" ,cyrus-sasl)
       ("openssl" ,openssl)))
    (inputs
     `(("curl" ,curl)
       ("expat" ,expat)
       ("zlib" ,zlib)))
    (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'autogen
            (lambda _
              (setenv "NOCONFIGURE" "true")
              (zero? (system* "sh" "autogen.sh")))))
        #:configure-flags
        '("--disable-static" "--disable-db")))
    (home-page "http://www.etpan.org/libetpan.html")
    (synopsis "Portable middleware for email access")
    (description
     "The purpose of this mail library is to provide a portable, efficient
framework for different kinds of mail access: IMAP, SMTP, POP and NNTP.  It
provides an API for C language.  It's the low-level API used by MailCore and
MailCore 2.")
    (license (non-copyleft "file://COPYING"))))

(define-public compface
  (package
    (name "compface")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.heanet.ie/mirrors/"
                                  "ftp.xemacs.org/aux/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "09b89wg63hg502hsz592cd2h87wdprb1dq1k1y07n89hym2q56d6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Portrait image compressor")
    (description "This packages takes your 48x48x1 portrait image and
compresses it.")
    (home-page "http://www.cs.indiana.edu/pub/faces/")
    (license (x11-style "file://README"))))

(define-public claws-mail
  (package
    (name "claws-mail")
    (version "3.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.claws-mail.org/releases/" name "-" version
                    ".tar.xz"))
              (sha256
               (base32
                "0bnwd3l04y6j1nw3h861rdy6k6lyjzsi51j04d33vbpq8c6jskaf"))))
    (build-system gnu-build-system)
    (native-inputs `(("pkg-config" ,pkg-config)))
    (inputs `(("bogofilter" ,bogofilter)
              ("curl" ,curl)
              ("dbus-glib" ,dbus-glib)
              ("enchant" ,enchant)
              ("expat" ,expat)
              ("ghostscript" ,ghostscript)
              ("hicolor-icon-theme" ,hicolor-icon-theme)
              ("gnupg" ,gnupg)
              ("gnutls" ,gnutls)
              ("gpgme" ,gpgme)
              ("gtk" ,gtk+-2)
              ("libarchive" ,libarchive)
              ("libcanberra" ,libcanberra)
              ("libetpan" ,libetpan)
              ("libnotify" ,libnotify)
              ("libsm" ,libsm)
              ("libxml2" ,libxml2)
              ("perl" ,perl)
              ("python-2" ,python-2)
              ("mime-info" ,shared-mime-info)))
    (arguments
      '(#:configure-flags
        '("--enable-gnutls" "--enable-pgpmime-plugin" "--enable-enchant")
        #:phases (modify-phases %standard-phases
                   (add-before 'build 'patch-mime
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "src/procmime.c"
                         (("/usr/share/mime/globs")
                          (string-append (assoc-ref inputs "mime-info")
                                         "/share/mime/globs"))))))))
    (synopsis "GTK-based Email client")
    (description
     "Claws-Mail is an email client (and news reader) based on GTK+.  The
appearance and interface are designed to be familiar to new users coming from
other popular email clients, as well as experienced users.  Almost all commands
are accessible with the keyboard.  Plus, Claws-Mail is extensible via addons
which can add many functionalities to the base client.")
    (home-page "http://www.claws-mail.org/")
    (license gpl3+))) ; most files are actually public domain or x11

(define-public msmtp
  (package
    (name "msmtp")
    (version "1.6.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/msmtp/msmtp/" version
                           "/msmtp-" version ".tar.xz"))
       (sha256
        (base32
         "0ppvww0sb09bnsrpqnvlrn8vx231r24xn2iiwpy020mxc8gxn5fs"))))
    (build-system gnu-build-system)
    (inputs
     `(("libidn" ,libidn)
       ("libsecret" ,libsecret)
       ("gnutls" ,gnutls)
       ("zlib" ,zlib)
       ("gsasl" ,gsasl)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://msmtp.sourceforge.net/")
    (arguments
     `(#:configure-flags (list "--with-libgsasl"
                               "--with-libidn"
                               "--with-tls=gnutls")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-msmtpq
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/msmtp"))
                    (msmtpq (string-append "scripts/msmtpq")))
               (install-file (string-append msmtpq "/msmtpq") bin)
               (install-file (string-append msmtpq "/msmtp-queue") bin)
               (install-file (string-append msmtpq "/README.msmtpq") doc)
               #t))))))
    (synopsis
     "Simple and easy to use SMTP client with decent sendmail compatibility")
    (description
     "msmtp is an SMTP client.  In the default mode, it transmits a mail to
an SMTP server (for example at a free mail provider) which takes care of further
delivery.")
    (license gpl3+)))

(define-public exim
  (package
    (name "exim")
    (version "4.87.1")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "ftp://ftp.exim.org/pub/exim/exim4/exim-"
                                 version ".tar.bz2")
                  (string-append "ftp://ftp.exim.org/pub/exim/exim4/old/exim-"
                                 version ".tar.bz2")))
       (sha256
        (base32
         "050m2gjzpc6vyik458h1j0vi8bxplkzjsyndkyd2y394i569kdyl"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("gnutls" ,gnutls)
       ("gzip" ,gzip)
       ("bzip2" ,bzip2)
       ("xz" ,xz)
       ("perl" ,perl)
       ("libxt" ,libxt)
       ("libxaw" ,libxaw)))
    (native-inputs
     `(("pcre" ,pcre "bin")
       ("perl" ,perl)))
    (arguments
     '(#:phases
       (alist-replace
        'configure
        ;; We'd use #:make-flags but the top-level Makefile calls others
        ;; recursively, so just set all variables this way.
        (lambda* (#:key outputs inputs #:allow-other-keys)
          (substitute* '("Makefile" "OS/Makefile-Default")
            (("(RM_COMMAND=).*" all var)
             (string-append var "rm\n")))
          (copy-file "src/EDITME" "Local/Makefile")
          (copy-file "exim_monitor/EDITME" "Local/eximon.conf")
          (let ((out (assoc-ref outputs "out"))
                (gzip (assoc-ref inputs "gzip"))
                (bzip2 (assoc-ref inputs "bzip2"))
                (xz (assoc-ref inputs "xz")))
            (substitute* '("Local/Makefile")
              (("(BIN_DIRECTORY=).*" all var)
               (string-append var out "/bin\n"))
              (("(CONFIGURE_FILE=).*" all var)
               (string-append var out "/etc/exim.conf\n"))
              (("(EXIM_USER=).*" all var)
               (string-append var "nobody\n"))
              (("(FIXED_NEVER_USERS=).*" all var)
               (string-append var "\n"))  ;XXX no root in build environment
              (("(COMPRESS_COMMAND=).*" all var)
               (string-append var gzip "/bin/gzip\n"))
              (("(ZCAT_COMMAND=).*" all var)
               (string-append var gzip "/bin/zcat\n")))
            ;; This file has hardcoded names for tools despite the zcat
            ;; configuration above.
            (substitute* '("src/exigrep.src")
              (("'zcat'") (string-append "'" gzip "/bin/zcat'"))
              (("'bzcat'") (string-append "'" bzip2 "/bin/bzcat'"))
              (("'xzcat'") (string-append "'" xz "/bin/xzcat'"))
              (("'lzma'") (string-append "'" xz "/bin/lzma'")))))
        (alist-cons-before
         'build 'fix-sh-paths
         (lambda* (#:key inputs #:allow-other-keys)
           (substitute* '("scripts/lookups-Makefile" "scripts/reversion")
             (("SHELL=/bin/sh") "SHELL=sh"))
           (substitute* '("scripts/Configure-config.h")
             (("\\| /bin/sh") "| sh"))
           (let ((bash (assoc-ref inputs "bash")))
             (substitute* '("scripts/Configure-eximon")
               (("#!/bin/sh") (string-append "#!" bash "/bin/sh")))))
         %standard-phases))
       #:make-flags '("INSTALL_ARG=-no_chown")
       ;; No 'check' target.
       #:tests? #f))
    (home-page "http://www.exim.org/")
    (synopsis
     "Message Transfer Agent (MTA) developed at the University of Cambridge")
    (description
     "Exim is a message transfer agent (MTA) developed at the University of
Cambridge for use on Unix systems connected to the Internet.  In style it is
similar to Smail 3, but its facilities are more general.  There is a great
deal of flexibility in the way mail can be routed, and there are extensive
facilities for checking incoming mail.")
    (license gpl2+)))

(define-public dovecot
  (package
    (name "dovecot")
    (version "2.2.29.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.dovecot.org/releases/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.gz"))
       (sha256 (base32
                "127kn3fgmahw9fvgz2w3zaghq98ip4j8640wqa3rw7mrgvxrzync"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("openssl" ,openssl)
       ("zlib" ,zlib)
       ("bzip2" ,bzip2)
       ("sqlite" ,sqlite)
       ("linux-pam" ,linux-pam)))
    (arguments
     `(#:configure-flags '("--sysconfdir=/etc"
                           "--localstatedir=/var")
       #:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'pre-configure
                   (lambda _
                     ;; Simple hack to avoid installing in /etc
                     (substitute* '("doc/Makefile.in"
                                    "doc/example-config/Makefile.in")
                       (("pkgsysconfdir = .*")
                        "pkgsysconfdir = /tmp/etc"))
                     #t))
                  (add-after
                   'unpack 'patch-other-shebangs
                   (lambda _
                     (substitute*
                       "src/lib-program-client/test-program-client-local.c"
                       (("/bin/echo") (which "echo"))
                       (("/bin/cat") (which "cat"))
                       (("/bin/false") (which "false")))
                     #t)))))
    (home-page "https://www.dovecot.org")
    (synopsis "Secure POP3/IMAP server")
    (description
     "Dovecot is a mail server whose major goals are security and reliability.
It supports mbox/Maildir and its own dbox/mdbox formats.")
    ;; Most source files are covered by either lgpl2.1 or expat.  The SHA code
    ;; is covered by a variant of BSD-3, and UnicodeData.txt is covered by the
    ;; Unicode, Inc. License Agreement for Data Files and Software.
    (license (list lgpl2.1 license:expat (non-copyleft "file://COPYING")))))

(define-public dovecot-trees
  (package
    (name "dovecot-trees")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://0xacab.org/riseuplabs/trees/repository/"
                           "archive.tar.gz?ref=v" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0rkk10b1bsjz979sc864vpgcdchy7yxwmyv4ik50lar1h6awdnrf"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("dovecot" ,dovecot)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("libsodium" ,libsodium)))
    (arguments
     `(#:tests? #f ;No tests exist.
       #:configure-flags (list (string-append "--with-dovecot="
                                              (assoc-ref %build-inputs "dovecot")
                                              "/lib/dovecot"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))))
    (home-page "https://0xacab.org/riseuplabs/trees")
    (synopsis "NaCL-based Dovecot email storage encryption plugin")
    (description
     "Technology for Resting Email Encrypted Storage (TREES) is a NaCL-based
Dovecot encryption plugin.  This plugin adds individually encrypted mail
storage to the Dovecot IMAP server.  It is inspired by Posteo's scrambler
which uses OpenSSL and RSA keypairs.  TREES works in a similar way, but uses
the Sodium crypto library (based on NaCL).

How it works:
@enumerate
@item On IMAP log in, the user's cleartext password is passed to the plugin.
@item The plugin creates an argon2 digest from the password.
@item This password digest is used as a symmetric secret to decrypt a libsodium secretbox.
@item Inside the secretbox is stored a Curve25519 private key.
@item The Curve25519 private key is used to decrypt each individual message,
using lidsodium sealed boxes.
@item New mail is encrypted as it arrives using the Curve25519 public key.
@end enumerate\n")
    (license agpl3)))

(define-public dovecot-libsodium-plugin
  (let ((commit "044de73c01c35385df0105f6b387bec5d5317ce7")
        (revision "1"))
    (package
      (name "dovecot-libsodium-plugin")
      (version (string-append "0.0.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/LuckyFellow/dovecot-libsodium-plugin")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "13h07l7xy713zchnj2p9fhvq7fdl4zy1ai94li3ygkqjjj8hrgas"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("automake" ,automake)
         ("autoconf" ,autoconf)
         ("libtool" ,libtool)
         ("dovecot" ,dovecot)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("libsodium" ,libsodium)))
      (arguments
       `(#:tests? #f ;No tests exist.
         #:configure-flags (list (string-append "--with-dovecot="
                                                (assoc-ref %build-inputs "dovecot")
                                                "/lib/dovecot"))
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'autogen
             (lambda _
               (zero? (system* "./autogen.sh")))))))
      (home-page "https://github.com/LuckyFellow/dovecot-libsodium-plugin")
      (synopsis "Libsodium password hashing schemes plugin for Dovecot")
      (description
       "@code{dovecot-libsodium-plugin} provides libsodium password
hashing schemes plugin for @code{Dovecot}.")
      (license gpl3+))))

(define-public isync
  (package
    (name "isync")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/isync/isync/"
                           version "/isync-" version ".tar.gz"))
       (sha256 (base32
                "1bij6nm06ghkg98n2pdyacam2fyg5y8f7ajw0d5653m0r4ldw5p7"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("openssl" ,openssl)))
    (home-page "http://isync.sourceforge.net/")
    (synopsis "Mailbox synchronization program")
    (description
     "isync/mbsync is command line tool for two-way synchronization of
mailboxes.  Currently Maildir and IMAP are supported types.")
    (license gpl2+)))

(define-public perl-email-abstract
  (package
    (name "perl-email-abstract")
    (version "3.008")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Abstract-" version ".tar.gz"))
       (sha256
        (base32
         "0h42rhvp769wb421cpbbg6v6xjp8iv86mvz70pqgfgf4nsn6jwgw"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-simple" ,perl-email-simple)
       ("perl-module-pluggable" ,perl-module-pluggable)
       ("perl-mro-compat" ,perl-mro-compat)))
    (home-page "http://search.cpan.org/dist/Email-Abstract")
    (synopsis "Interface to mail representations")
    (description "Email::Abstract provides module writers with the ability to
write simple, representation-independent mail handling code.")
    (license (package-license perl))))

(define-public perl-email-address
  (package
    (name "perl-email-address")
    (version "1.908")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Address-" version ".tar.gz"))
       (sha256
        (base32
         "0i6ljdvpy279hpbqf6lgv4figr376rb2sh4yphj86xkdzsyn1y75"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Email-Address")
    (synopsis "Email address parsing and creation")
    (description "Email::Address implements a regex-based RFC 2822 parser that
locates email addresses in strings and returns a list of Email::Address
objects found.  Alternatively you may construct objects manually.")
    (license (package-license perl))))

(define-public perl-email-date-format
  (package
    (name "perl-email-date-format")
    (version "1.005")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Date-Format-" version ".tar.gz"))
       (sha256
        (base32
         "012ivfwpnbl3wr50f9c6f4azhdlxnm31pdn72528g79v61z6372p"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Email-Date-Format")
    (synopsis "Produce RFC 2822 date strings")
    (description "Email::Date::Format provides a means for generating an RFC
2822 compliant datetime string.")
    (license (package-license perl))))

(define-public perl-email-messageid
  (package
    (name "perl-email-messageid")
    (version "1.406")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MessageID-" version ".tar.gz"))
       (sha256
        (base32
         "1f22sdnfq169qw1l0lg7y74pmiam7j9v95bggjnf3q4mygdmshpc"))))
    (build-system perl-build-system)
    (home-page "http://search.cpan.org/dist/Email-MessageID")
    (synopsis "Generate world unique message-ids")
    (description "Email::MessageID generates recommended message-ids to
identify a message uniquely.")
    (license (package-license perl))))

(define-public perl-email-mime
  (package
    (name "perl-email-mime")
    (version "1.940")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-" version ".tar.gz"))
       (sha256
        (base32
         "0pnxbr16cn5qy96xqhp9zmd94ashc9ivqh10qbgbc3f637a0mfir"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-address" ,perl-email-address)
       ("perl-email-messageid" ,perl-email-messageid)
       ("perl-email-mime-contenttype" ,perl-email-mime-contenttype)
       ("perl-email-mime-encodings" ,perl-email-mime-encodings)
       ("perl-email-simple" ,perl-email-simple)
       ("perl-mime-types" ,perl-mime-types)))
    (home-page "http://search.cpan.org/dist/Email-MIME")
    (synopsis "MIME message handling")
    (description "Email::MIME is an extension of the Email::Simple module, to
handle MIME encoded messages.  It takes a message as a string, splits it up
into its constituent parts, and allows you access to various parts of the
message.  Headers are decoded from MIME encoding.")
    (license (package-license perl))))

(define-public perl-email-mime-contenttype
  (package
    (name "perl-email-mime-contenttype")
    (version "1.018")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-ContentType-" version ".tar.gz"))
       (sha256
        (base32
         "1y8hpwm7p5a9y2azy0cgvlv2i2d0nj66ajfa0fj51wdq4w9cs23m"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "http://search.cpan.org/dist/Email-MIME-ContentType")
    (synopsis "Parse MIME Content-Type headers")
    (description "Email::MIME::ContentType parses a MIME Content-Type
header.")
    (license (package-license perl))))

(define-public perl-email-mime-encodings
  (package
    (name "perl-email-mime-encodings")
    (version "1.315")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-Encodings-" version ".tar.gz"))
       (sha256
        (base32
         "0p5b8g9gh35m8fqrpx60g4bp98rvwd02n5b0vm9wh7mk0xah8wac"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (home-page "http://search.cpan.org/dist/Email-MIME-Encodings")
    (synopsis "Unified interface to MIME encoding and decoding")
    (description "This module wraps MIME::Base64 and MIME::QuotedPrint.")
    (license (package-license perl))))

(define-public perl-email-sender
  (package
    (name "perl-email-sender")
    (version "1.300028")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Sender-" version ".tar.gz"))
       (sha256
        (base32
         "0c5dv1x9856nryj5mcbgb67a4irmadz80g0qnf4van3bd8wbj72a"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-capture-tiny" ,perl-capture-tiny)))
    (propagated-inputs
     `(("perl-email-abstract" ,perl-email-abstract)
       ("perl-email-address" ,perl-email-address)
       ("perl-email-simple" ,perl-email-simple)
       ("perl-list-moreutils" ,perl-list-moreutils)
       ("perl-module-runtime" ,perl-module-runtime)
       ("perl-moo" ,perl-moo)
       ("perl-moox-types-mooselike" ,perl-moox-types-mooselike)
       ("perl-sub-exporter" ,perl-sub-exporter)
       ("perl-throwable" ,perl-throwable)
       ("perl-try-tiny" ,perl-try-tiny)))
    (home-page "http://search.cpan.org/dist/Email-Sender")
    (synopsis "Perl library for sending email")
    (description "Email::Sender replaces the old and sometimes problematic
Email::Send library.")
    (license (package-license perl))))

(define-public perl-email-simple
  (package
    (name "perl-email-simple")
    (version "2.213")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "1ibwsng63gvqqc6r2135mjwfdzazxkb1x8q7f87wqcbjcjfpmffd"))))
    (build-system perl-build-system)
    (propagated-inputs
     `(("perl-email-date-format" ,perl-email-date-format)))
    (home-page "http://search.cpan.org/dist/Email-Simple")
    (synopsis "Parsing of RFC 2822 messages")
    (description "Email::Simple provides simple parsing of RFC 2822 message
format and headers.")
    (license (package-license perl))))

(define-public libesmtp
  (package
    (name "libesmtp")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://pkgs.fedoraproject.org/repo/pkgs/"
                                 "libesmtp/libesmtp-" version ".tar.bz2/"
                                 "bf3915e627fd8f35524a8fdfeed979c8/libesmtp-"
                                 version ".tar.bz2")
                  ;; XXX This site is offline, so we fetch Fedora's cached copy
                  ;; of the source tarball.
                  (string-append "http://www.stafford.uklinux.net/libesmtp/libesmtp-"
                                 version ".tar.bz2")))
       (sha256
        (base32
         "02zbniyz7qys1jmx3ghx21kxmns1wc3hmv80gp7ag7yra9f1m9nh"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("openssl" ,openssl)))
    (home-page "http://www.stafford.uklinux.net/libesmtp/")
    (synopsis "Library for sending mail via remote hosts using SMTP")
    (description "libESMTP is an SMTP client which manages posting (or
submission of) electronic mail via a preconfigured Mail Transport Agent (MTA).
It may be used as part of a Mail User Agent (MUA) or other program that must
be able to post electronic mail where mail functionality may not be that
program's primary purpose.")
    (license (list lgpl2.1+ gpl2+))))

(define-public esmtp
  (package
    (name "esmtp")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andywingo/esmtp.git")
             (commit "01bf9fc")))
       (sha256
        (base32
         "1ay282rrl92h0m0m8z5zzjnwiiagi7c78aq2qvhia5mw7prwfyw2"))
       (file-name (string-append name "-" version "-checkout"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before
                   'configure 'autoconf
                   (lambda _ (zero? (system* "autoreconf" "-vfi")))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bison" ,bison)
       ("flex" ,flex)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (inputs
     `(("libesmtp" ,libesmtp)))
    (home-page "https://sourceforge.net/projects/esmtp/")
    (synopsis "Relay-only mail transfer agent (MTA)")
    (description "Esmtp is a simple relay-only mail transfer agent built using
libESMTP.  It sends e-mail via a remote SMTP server using credentials from the
user's @file{$HOME/.esmtprc} configuration file; see the @command{esmtprc} man
page for more on configuration.  This package also provides minimal
compatibility shims for the @command{sendmail}, @command{mailq}, and
@command{newaliases} commands.")
    (license gpl2+)))

(define-public fdm
  (package
    (name "fdm")
    (version "1.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/nicm/fdm/releases/download/"
                                 version "/fdm-"
                                 version ".tar.gz"))
             (file-name (string-append name "-" version ".tar.gz"))
             (sha256
               (base32 "054rscijahiza5f9qha79rg3siji3bk5mk10f8c2vqx7m4w6qh8n"))))
    (build-system gnu-build-system)
    (inputs
     `(("tdb" ,tdb)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (home-page "https://github.com/nicm/fdm")
    (synopsis "Mail Retrieval Agent (MRA) and Mail Delivery Agent (MDA)")
    (description "fdm is a program designed to fetch mail from POP3
or IMAP servers, or receive local mail from stdin, and
deliver it in various ways.")
    (license
     ;; Why point to a source file?  Well, all the individual files have a
     ;; copy of this license in their headers, but there's no seprate file
     ;; with that information.
     (non-copyleft "https://github.com/nicm/fdm/blob/master/command.c"))))


(define-public procmail
  (package
    (name "procmail")
    (version "3.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.fu-berlin.de/pub/unix/mail/procmail/procmail-"
             version
             ".tar.gz"))
       (sha256
        (base32
         "05z1c803n5cppkcq99vkyd5myff904lf9sdgynfqngfk9nrpaz08"))
       ;; The following patch fixes an ambiguous definition of
       ;; getline() in formail.c.  The patch is provided by Debian as
       ;; patch 24.
       (patches (search-patches "procmail-ambiguous-getline-debian.patch"
                                "procmail-CVE-2014-3618.patch"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda _
                      (substitute* "Makefile"
                        (("/bin/sh")
                         (which "sh"))
                        (("/usr")
                         (assoc-ref %outputs "out"))
                        (("/bin/rm")
                         (which "rm")))
                      #t)))
       #:tests? #f)) ;; There are no tests indicating a successful
    ;; build.  Some tests of basic locking mechanisms provided by the
    ;; file system are performed during 'make install'.  However, these
    ;; are performed before the actual build process.
    (build-system gnu-build-system)
    (inputs `(("exim" ,exim)))
    (home-page "http://www.procmail.org/")
    (synopsis "Versatile mail delivery agent (MDA)")
    (description "Procmail is a mail delivery agent (MDA) featuring support
for a variety of mailbox formats such as mbox, mh and maildir.  Incoming mail
can be sorted into separate files/directories and arbitrary commands can be
executed on mail arrival.  Procmail is considered stable, but is no longer
maintained.")
    (license gpl2+))) ;; procmail allows to choose the
                      ;; nonfree Artistic License 1.0
                      ;; as alternative to the GPL2+.
                      ;; This option is not listed here.

(define-public khard
  (package
    (name "khard")
    (version "0.11.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri name version))
              (sha256
               (base32
                "1v66khq5w17xdbkpb00pf9xbl84dlzx4lq286fvzskb949b3y4yn"))))
    (build-system python-build-system)
    (arguments
      `(#:phases
        (modify-phases %standard-phases
          (add-after 'install 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (doc (string-append out "/share/doc/khard")))
                (copy-recursively "misc/khard" doc)
                #t))))
        ;; FIXME: check phase fails with
        ;; "Config file /tmp/.config/khard/khard.conf not available"
        #:tests? #f))
    (propagated-inputs
     `(("python-vobject" ,python-vobject)
       ("python-pyyaml" ,python-pyyaml)
       ("python-atomicwrites" ,python-atomicwrites)
       ("python-configobj" ,python-configobj)))
    (synopsis "Console address book using CardDAV")
    (description "Khard is an address book for the console.  It creates, reads,
modifies and removes CardDAV address book entries at your local machine.  For
synchronizing with a remote address book, @command{vdirsyncer} is recommended.
Khard can also be used from within the email client @command{mutt}.")
    (home-page "https://github.com/scheibler/khard")
    (license gpl3+)))

(define-public perl-mail-spf
 (package
  (name "perl-mail-spf")
  (version "v2.9.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/J/JM/JMEHNLE/mail-spf/Mail-SPF-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0qk1rfgfm5drj4iyniiabrasrpqv570vzhgz66lwgb67y4amkjv1"))))
  (build-system perl-build-system)
  (native-inputs
    `(("perl-module-build" ,perl-module-build)
      ("perl-net-dns-resolver-programmable"
       ,perl-net-dns-resolver-programmable)))
  (arguments
   `(#:phases (modify-phases %standard-phases
       (add-before 'configure 'modify-Build.PL
         (lambda* (#:key outputs #:allow-other-keys)
           (substitute* "Build.PL"
             (("'/usr/sbin'") (string-append "'"
                                             (assoc-ref outputs "out")
                                             "/sbin'")))
             #t)))))
  (inputs
    `(("perl-error" ,perl-error)
      ("perl-net-dns" ,perl-net-dns)
      ("perl-netaddr-ip" ,perl-netaddr-ip)
      ("perl-uri" ,perl-uri)))
  (home-page
    "http://search.cpan.org/dist/Mail-SPF")
  (synopsis
    "Perl implementation of Sender Policy Framework")
  (description "Mail::SPF is the Sender Policy Framework implemented
in Perl.")
  (license bsd-3)))

(define-public mb2md
  (package
    (name "mb2md")
    (version "3.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://batleth.sapienti-sat.org/projects/mb2md/mb2md-"
                    version ".pl.gz"))
              (sha256
               (base32
                "0bvkky3c90738h3skd2f1b2yy5xzhl25cbh9w2dy97rs86ssjidg"))))
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (out (assoc-ref %outputs "out"))
                (bin (string-append out "/bin"))
                (perl (assoc-ref %build-inputs "perl"))
                (gzip (assoc-ref %build-inputs "gzip"))
                (perl-timedate (assoc-ref %build-inputs "perl-timedate"))
                (perl5lib (string-append perl-timedate "/lib/perl5/site_perl")))
           (mkdir-p bin)
           (with-directory-excursion bin
             (copy-file source "mb2md.gz")
             (system* (string-append gzip "/bin/gzip") "-d" "mb2md.gz")
             (substitute* "mb2md"
               (("#!/usr/bin/perl")
                (string-append "#!/usr/bin/perl -I " perl5lib)))
             (patch-shebang "mb2md" (list (string-append perl "/bin")))
             (chmod "mb2md" #o555))
           #t))))
    (native-inputs `(("gzip", gzip)))
    (inputs `(("perl" ,perl)
              ("perl-timedate" ,perl-timedate)))
    (home-page "http://batleth.sapienti-sat.org/projects/mb2md/")
    (synopsis "Mbox to maildir converter")
    (description
     "Mb2md is a Perl script that takes one or more mbox format files and
converts them to maildir format directories.")
    (license public-domain)))

(define-public mpop
  (package
    (name "mpop")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mpop/mpop/" version
                           "/mpop-" version ".tar.xz"))
       (sha256
        (base32
         "0p1ix63jh64dibrlccch8q7gxl9nn18wd2qpyr5z1h4gs2fpmv4z"))))
    (build-system gnu-build-system)
    (inputs
     `(("gnutls" ,gnutls)
       ("libidn" ,libidn)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags (list "--with-tls=gnutls")))
    (home-page "http://mpop.sourceforge.net/")
    (synopsis "POP3 mail client")
    (description "mpop is a small and fast POP3 client suitable as a
fetchmail replacement.

mpop supports multiple accounts, header based mail filtering, delivery
to mbox files, maildir folders or a Mail Delivery Agent (MDA),
TLS/SSL, several authentication methods, Internationalized Domain
Names (IDN) and SOCKS proxies.")
    (license gpl3+)))

(define-public mhonarc
  (package
    (name "mhonarc")
    (version "2.6.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/EH/EHOOD/MHonArc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0ll3v93yji334zqp6xfzfxc0127pmjcznmai1l5q6dzawrs2igzq"))))
    (build-system perl-build-system)
    (home-page "https://www.mhonarc.org/")
    (synopsis "Create HTML archives of mail/news messages")
    (description
     "MHonArc is a Perl mail-to-HTML converter.  MHonArc
provides HTML mail archiving with index, mail thread linking,
etc; plus other capabilities including support for MIME and
powerful user customization features.")
    (license gpl2+)))


(define-public sendmail
  (package
    (name "sendmail")
    (version "8.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.sendmail.org/pub/sendmail/sendmail."
             version ".tar.gz"))
       (sha256
        (base32
         "0fdl9ndmspqspdlmghzxlaqk56j3yajk52d7jxcg21b7sxglpy94"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'replace-/bin/sh
           (lambda _
             (substitute*
                 (append
                  (list "smrsh/smrsh.c" "sendmail/conf.c" "contrib/mailprio"
                        "contrib/mmuegel" "devtools/bin/configure.sh")
                  (find-files "." ".*\\.m4")
                  (find-files "." ".*\\.cf"))
               (("/bin/sh") (which "sh")))

             (substitute* "devtools/bin/Build"
               (("SHELL=/bin/sh") (string-append "SHELL=" (which "sh"))))
             #t))
         (replace 'configure
           (lambda _

             ;; Render harmless any attempts to chown or chgrp
             (substitute* "devtools/bin/install.sh"
               (("owner=\\$2") "owner=''")
               (("group=\\$2") "group=''"))

             (with-output-to-file "devtools/Site/site.config.m4"
               (lambda ()
                 (format #t "
define(`confCC', `gcc')
define(`confOPTIMIZE', `-g -O2')
define(`confLIBS', `-lresolv')
define(`confINSTALL', `~a/devtools/bin/install.sh')
define(`confDEPEND_TYPE', `CC-M')
define(`confINST_DEP', `')
" (getcwd))))))
         (replace 'build
           (lambda _
             (and (zero? (system* "sh" "Build"))
                  (with-directory-excursion "cf/cf"
                    (begin
                      (copy-file "generic-linux.mc" "sendmail.mc")
                      (zero? (system* "sh" "Build" "sendmail.cf")))))))
         (add-before 'install 'pre-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (mkdir-p (string-append out "/usr/bin"))
               (mkdir-p (string-append out "/usr/sbin"))
               (mkdir-p (string-append out "/etc/mail"))
               (setenv "DESTDIR" out)
               (with-directory-excursion "cf/cf"
                 (zero? (system* "sh" "Build" "install-cf")))))))
       ;; There is no make check.  There are some post installation tests, but those
       ;; require root privileges
       #:tests? #f))
    (inputs
     `(("m4" ,m4)
       ("perl" ,perl)))
    (home-page "http://sendmail.org")
    (synopsis
     "Highly configurable Mail Transfer Agent (MTA)")
    (description
     "Sendmail is a mail transfer agent (MTA) originally developed by Eric
Allman.  It is highly configurable and supports many delivery methods and many
transfer protocols.")
    (license (non-copyleft "file://LICENSE"
                           "See LICENSE in the distribution."))))

(define-public opensmtpd
  (package
    (name "opensmtpd")
    (version "6.0.2p1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.opensmtpd.org/archives/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b4h64w45hpmfq5721smhg4s0shs64gbcjqjpx3fbiw4hz8bdy9a"))))
    (build-system gnu-build-system)
    (inputs
     `(("bdb" ,bdb)
       ("libressl" ,libressl)
       ("libevent" ,libevent)
       ("libasr" ,libasr)
       ("linux-pam" ,linux-pam)
       ("zlib" ,zlib)))
    (native-inputs
     `(("bison" ,bison)
       ("groff" ,groff)))
    (arguments
     `(#:configure-flags
       (list "--with-table-db" "--localstatedir=/var"
             "--with-user-smtpd=smtpd" "--with-user-queue=smtpq"
             "--with-group-queue=smtpq"
             "--with-path-socket=/var/run"
             "--with-path-CAfile=/etc/ssl/certs/ca-certificates.crt")
       #:phases
       (modify-phases %standard-phases
         ;; OpenSMTPD provides a single utility smtpctl to control the daemon and
         ;; the local submission subsystem.  To accomodate systems that require
         ;; historical interfaces such as sendmail, newaliases or makemap, the
         ;; smtpctl utility can operate in compatibility mode if called with the
         ;; historical name.
         (add-after 'install 'install-compabilitymode
           (lambda _
             (let* ((out (assoc-ref %outputs "out"))
                    (sbin (string-append out "/sbin/")))
               (for-each (lambda (cmd)
                           (symlink "smtpctl" (string-append sbin cmd)))
                         '("makemap" "sendmail" "send-mail"
                           "newaliases" "mailq")))
             #t)))))
    (synopsis "Lightweight SMTP daemon")
    (description
     "OpenSMTPD is an implementation of the server-side SMTP protocol, with
some additional standard extensions.  It allows ordinary machines to exchange
e-mails with other systems speaking the SMTP protocol.")
    (home-page "https://www.opensmtpd.org")
    (license (list bsd-2 bsd-3 bsd-4 (non-copyleft "file://COPYING")
                   public-domain isc license:openssl))))

(define-public opensmtpd-extras
  (package
    (name "opensmtpd-extras")
    (version "5.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.opensmtpd.org/archives/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kld4hxgz792s0cb2gl7m2n618ikzqkj88w5dhaxdrxg4x2c4vdm"))))
    (build-system gnu-build-system)
    (inputs
     `(("libressl" ,libressl)
       ("libevent" ,libevent)
       ("libasr" ,libasr)
       ("python-2" ,python-2)
       ("opensmtpd" ,opensmtpd)
       ("perl" ,perl)
       ("lua" ,lua)
       ("postgresql" ,postgresql)
       ("sqlite" ,sqlite)
       ("linux-pam" ,linux-pam)))
    (native-inputs
     `(("bison" ,bison)
       ("pkg-config" ,pkg-config)
       ("groff" ,groff)
       ("automake" ,automake)
       ("autoconf" ,autoconf)))
    (arguments
     `(;; We have to configure it like this because the default checks for for example
       ;; python in /usr/local/bin, /usr/bin and fails otherwise.
       #:configure-flags (list
                          "--with-filter-clamav"    "--with-filter-dkim-signer"
                          "--with-filter-dnsbl"     "--with-filter-lua"
                          "--with-filter-monkey"    "--with-filter-pause"
                          "--with-filter-perl"      "--with-filter-python"
                          "--with-filter-regex"     "--with-filter-spamassassin"
                          "--with-filter-stub"      "--with-filter-trace"
                          "--with-filter-void"

                          "--with-queue-null"       "--with-queue-python"
                          "--with-queue-ram"        "--with-queue-stub"

                          "--with-scheduler-python" "--with-scheduler-ram"
                          "--with-scheduler-stub"

                          "--with-table-ldap"       ; "--with-table-mysql"
                          "--with-table-passwd"     "--with-table-postgres"
                          "--with-table-python"     "--with-table-socketmap"
                          "--with-table-sqlite"     "--with-table-stub"
                          ;;"--with-table-redis"    ; TODO: package hiredis

                          "--with-user=smtpd"       "--with-privsep-user=smtpd"
                          "--localstatedir=/var"    "--sysconfdir=/etc"
                          "--with-lua-type=lua"     ; can use lua or luajit

                          (string-append "--with-python="
                                         (assoc-ref %build-inputs "python-2"))
                          (string-append "--with-lua="
                                         (assoc-ref %build-inputs "lua")))))
    (license (list bsd-2 bsd-3 bsd-4
                   public-domain isc license:openssl))
    (synopsis "Extra tables, filters, and various other addons for OpenSMTPD")
    (description
     "This package provides extra tables, filters, and various other addons
for OpenSMTPD to extend its functionality.")
    (home-page "https://www.opensmtpd.org")))

(define-public python-mailmanclient
  (package
    (name "python-mailmanclient")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mailmanclient" version))
       (sha256
        (base32
         "1cfjh45fgbsax5hjj2inq9nk33dhdvh63xhysc8dhnqidgqgm8c5"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Requires mailman running
    (propagated-inputs
     `(("python-six" ,python-six)
       ("python-httplib2" ,python-httplib2)))
    (home-page "https://launchpad.net/mailman.client")
    (synopsis "Python bindings for the Mailman 3 REST API")
    (description
     "The mailmanclient library provides official Python bindings for
the GNU Mailman 3 REST API.")
    (license lgpl3+)))

(define-public python2-mailmanclient
  (package-with-python2 python-mailmanclient))

(define-public mlmmj
  (package
    (name "mlmmj")
    (version "1.2.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://mlmmj.org/releases/mlmmj-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1piwvcxkqadjwk5x8jicaiyz9nngmaj3w13ghdqgaki32xd7zk9v"))))
    (build-system gnu-build-system)
    (inputs
     `(("perl" ,perl))) ; For "contrib/web/"
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       ;; mlmmj-receive-strip is a replacement for mlmmj-receive
       ;; It opens the files control/mimedeny and control/mimestrip to get a list
       ;; of mimetypes for parts of multipart/mime messages that should be denied
       ;; or stripped. The parts then get stripped directly when the mail is
       ;; received. mlmmj-receive-strip also appends an extra header
       ;; X-ThisMailContainsUnwantedMimeParts: Y when the mail contains unwanted
       ;; mime parts
       (list "--enable-receive-strip")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'install-contrib
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (share (string-append out "/share/mlmmj"))
                    (contrib (string-append share "/contrib/web"))
                    (texts (string-append share "/listtexts")))
               (copy-recursively "contrib/web/" contrib)
               (copy-recursively "listtexts" texts)
               (rename-file texts (string-append share "/texts"))
               #t))))))
    (home-page "http://mlmmj.org")
    (synopsis "Mailing list managing made joyful")
    (description
     "Mlmmj is a simple and slim mailing list manager (MLM) inspired by ezmlm.
It works with many different Mail Transport Agents (MTAs) and is simple for a
system administrator to install, configure and integrate with other software.
As it uses very few resources, and requires no daemons, it is ideal for
installation on systems where resources are limited.  Its features include:
@enumerate
@item Archive, Custom headers / footer,
@item Fully automated bounce handling (similar to ezmlm),
@item Complete requeueing functionality, Moderation functionality, Subject prefix,
@item Subscribers only posting, Regular expression access control,
@item Functionality to retrieve old posts, Web interface, Digests,
@item No-mail subscription, VERP support,
@item Delivery Status Notification (RFC1891) support,
@item Rich and customisable texts for automated operations.
@end enumerate\n")
    (license license:expat)))

(define-public python-django-mailman3
  (package
    (name "python-django-mailman3")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "django-mailman3" version))
       (sha256
        (base32
         "1adxyh8knw9knjlh73xq0jpn5adml0ck4alsv0swakm95wfyx46z"))))
    (build-system python-build-system)
    (inputs
     `(("python-django" ,python-django)))
    (propagated-inputs
     `(("python-requests" ,python-requests)
       ("python-requests-oauthlib" ,python-requests-oauthlib)
       ("python-openid" ,python-openid)
       ("python-mailmanclient" ,python-mailmanclient)
       ("python-django-allauth" ,python-django-allauth)
       ("python-django-gravatar2" ,python-django-gravatar2)
       ("python-pytz" ,python-pytz)))
    (home-page "https://gitlab.com/mailman/django-mailman3")
    (synopsis "Django library for Mailman UIs")
    (description
     "Libraries and templates for Django-based interfaces
interacting with Mailman.")
    (license gpl3+)))

(define-public python2-django-mailman3
  (let ((base (package-with-python2
               python-django-mailman3)))
    (package
      (inherit base)
      (propagated-inputs
       `(("python2-openid" ,python2-openid)
         ,@(package-propagated-inputs base))))))

(define-public postorius
  (package
    (name "postorius")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "postorius" version "+post2.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1wymcpv2icjjy8h1ni52p6dr7wwxf71ivqgbqhzx4i82yqphcaq5"))))
    (build-system python-build-system)
    (arguments
     `(; One test dependency relies on Persona, which was shut down in
       ;; November 2016.
       #:tests? #f
       ;; The part of the frontend of Mailman is still python 2.7.
       #:python ,python-2))
    (inputs
     `(("python2-django" ,python2-django)
       ("python2-django-mailman3" ,python2-django-mailman3)
       ("python2-mailmanclient" ,python2-mailmanclient)))
    (home-page "https://gitlab.com/mailman/postorius")
    (synopsis "Web user interface for GNU Mailman")
    (description
     "Postorius is a Django app which provides a web user interface
to access GNU Mailman.")
    (license (list gpl3+ lgpl3+))))

(define-public blists
  (package
    (name "blists")
    (version "1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.openwall.net/pub/projects/"
                           "blists/blists-" version ".tar.gz"))
       (sha256
        (base32
         "1gp51kmb8yv8d693wcpdslmwlbw5w2kgz4kxhrcaf7y89w8wy4qd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "bindex" bin)
               (install-file "bit" bin)
               #t))))))
    (home-page "http://www.openwall.com/blists/")
    (synopsis "Web interface to mailing list archives")
    (description
     "Blists is a web interface to mailing list archives that works off
indexed mbox files.  There are two programs: @code{bindex} and @code{bit}.
@code{bindex} generates or updates the index file (incremental updates
are supported).  @code{bit} is a CGI/SSI program that generates web pages
on the fly.  Both programs are written in C and are very fast.")
    (license license:expat)))

(define-public swaks
  (package
    (name "swaks")
    (version "20170101.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://jetmore.org/john/code/swaks/files/swaks-"
             version ".tar.gz"))
       (sha256
        (base32
         "0pli4mlhasnqqxmmxalwyg3x7n2vhcbgsnp2xgddamjavv82vrl4"))))
    (build-system perl-build-system)
    (inputs
     `(("perl-net-dns" ,perl-net-dns)
       ("perl-net-ssleay" ,perl-net-ssleay)))
    (arguments
     `(#:tests? #f ; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (zero? (system* "pod2man" "doc/ref.pod" "swaks.1"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "swaks" (string-append out "/bin"))
               (install-file "swaks.1" (string-append out "/share/man/man1")))
             #t))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/swaks")
               `("PERL5LIB" ":" = (,(getenv "PERL5LIB"))))
             #t)))))
    (home-page "http://jetmore.org/john/code/swaks/")
    (synopsis "Featureful SMTP test tool")
    (description "Swaks is a flexible, scriptable, transaction-oriented SMTP
test tool.  It handles SMTP features and extensions such as TLS,
authentication, and pipelining; multiple versions of the SMTP protocol
including SMTP, ESMTP, and LMTP; and multiple transport methods including
unix-domain sockets, internet-domain sockets, and pipes to spawned processes.
Options can be specified in environment variables, configuration files, and
the command line allowing maximum configurability and ease of use for
operators and scripters.")
    (license gpl2+)))

(define-public alpine
  (package
    (name "alpine")
    (version "2.21")
    (source
     (origin
       (method url-fetch)
       ;; There are two versions: the plain continuation of Alpine without extra
       ;; patches and the version which adds extra fixes. Every distro uses
       ;; the patched version, and so do we to not break expectations.
       ;; http://patches.freeiz.com/alpine/readme/README.patches
       (uri (string-append "http://patches.freeiz.com/alpine/patches/alpine-"
                           version "/alpine-" version ".tar.xz"))
       (sha256
        (base32
         "1k9hcfjywfk3mpsl71hjza3nk6icgf1b6xxzgx10kdzg5yci5x5m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "CC=gcc")
       #:configure-flags (list (string-append "--with-ssl-include-dir="
                                              (assoc-ref %build-inputs "openssl")
                                              "/include/openssl")
                               (string-append "--with-ssl-dir="
                                              (assoc-ref %build-inputs "openssl"))
                               (string-append "--with-ssl-certs-dir="
                                              "/etc/ssl/certs/")
                               (string-append "--with-ssl-lib-dir="
                                              (assoc-ref %build-inputs "openssl")
                                              "/lib")
                               (string-append "--with-interactive-spellcheck="
                                              (assoc-ref %build-inputs "aspell")
                                              "/bin/aspell"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; This removes time-dependent code to make alpine reproducible.
             (substitute* "pico/blddate.c"
               (("%02d-%s-%d") "1970-01-01"))
             (substitute* (list "alpine/Makefile.in"
                                "web/src/alpined.d/Makefile.in")
               (("`date`") "1970-01-01"))
             #t)))))
    (inputs
     `(("ncurses" ,ncurses)
       ("openssl" ,openssl)
       ("gnutls" ,gnutls)
       ("openldap" ,openldap)
       ("cyrus-sasl" ,cyrus-sasl)
       ("mit-krb5" ,mit-krb5)
       ("aspell" ,aspell)
       ("tcl" ,tcl)
       ("linux-pam" ,linux-pam)))
    (home-page "http://patches.freeiz.com/alpine/")
    (synopsis "Alternatively Licensed Program for Internet News and Email")
    (description
     "Alpine is a text-based mail and news client.  Alpine includes several
tools and applications:
@enumerate
@item alpine, the Alpine mailer
@item pico, the standalone text editor, GNU nano's predecessor
@item pilot, the standalone file system navigator
@end enumerate\n")
    (license asl2.0)))

(define-public balsa
  (package
    (name "balsa")
    (version "2.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pawsa.fedorapeople.org/balsa/balsa-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "15jkwp3ylbwd8iha4dr37z1xb6mkk31ym90vv3h2a5xk2rmym5mq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '(;; Balsa tries to install additional MIME icons
         ;; under gtk+ directory.
         "--enable-extra-mimeicons=no"
         "--with-gtksourceview"
         "--with-canberra"
         "--with-spell-checker=gtkspell"
         "--with-gpgme"
         "--with-sqlite"
         "--with-compface"
         "--with-ldap")))
    (inputs
     `(("cyrus-sasl" ,cyrus-sasl)
       ("enchant" ,enchant)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gmime" ,gmime)
       ("gnutls" ,gnutls)
       ("gpgme" ,gpgme)
       ("gtk+" ,gtk+)
       ("gtksourceview" ,gtksourceview)
       ("gtkspell3" ,gtkspell3)
       ("libcanberra" ,libcanberra)
       ("libesmtp" ,libesmtp)
       ("libnotify" ,libnotify)
       ("openldap" ,openldap)
       ("sqlite" ,sqlite)
       ("webkitgtk" ,webkitgtk)))
    (native-inputs
     `(("compface" ,compface)
       ("glib" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("yelp-tools" ,yelp-tools)))
    (home-page "https://pawsa.fedorapeople.org/balsa")
    (synopsis "E-mail client for GNOME")
    (description "Balsa is a highly configurable and robust mail client for
the GNOME desktop.  It supports both POP3 and IMAP servers as well as the
mbox, maildir and mh local mailbox formats.  Balsa also supports SMTP and/or
the use of a local MTA such as Sendemail.")
    (license gpl3+)))
