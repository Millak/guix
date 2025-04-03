;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2017, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2014, 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015, 2016, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Al McElrath <hello@yrns.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Lukas Gradl <lgradl@openmailbox.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2016–2025 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2017–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2018, 2020 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2018, 2019, 2020, 2021, 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018-2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019–2022 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Justus Winter <justus@sequoia-pgp.org>
;;; Copyright © 2020 Eric Brown <ecbrown@ericcbrown.com>
;;; Copyright © 2020, 2021, 2022, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021 Alexey Abramov <levenson@mmer.org>
;;; Copyright © 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2020, 2021 Alexandru-Sergiu Marton <brown121407@posteo.ro>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020 divoplade <d@divoplade.fr>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Benoit Joly <benoit@benoitj.ca>
;;; Copyright © 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2022 Thiago Jung Bauermann <bauermann@kolabnow.com>
;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2022, 2024 jgart <jgart@dismail.de>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022 Mathieu Laparie <mlaparie@disr.it>
;;; Copyright © 2023 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Arjan Adriaanse <arjan@adriaan.se>
;;; Copyright © 2023 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Benjamin Slade <slade@lambda-y.net>
;;; Copyright © 2024 Jean Simard <woshilapin@tuziwo.info>
;;; Copyright © 2024, 2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2025 Tanguy Le Carrour <tanguy@bioneland.org>
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

(define-module (gnu packages mail)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages c)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages dejagnu)
  #:use-module (gnu packages django)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mercury)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages perl-web)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system guile)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public abook
  (package
    (name "abook")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://abook.sourceforge.io/devel/abook-" version ".tar.gz"))
       (sha256
        (base32 "1yf0ifyjhq2r003pnpn92mn0924bn9yxjifxxj2ldcsgd7w0vagh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Fix "undefined reference to `field_id'" errors.
         (add-after 'unpack 'fix-build-with-recent-gcc
           (lambda _
             (substitute* '("database.c" "database.h")
               (("^inline int" all) (string-append "extern " all)))))
         ;; Fix following error during bootstrap: "gettext infrastructure
         ;; mismatch: using a Makefile.in.in from gettext version 0.18 but the
         ;; autoconf macros are from gettext version 0.20".
         (add-before 'bootstrap 'fix-gettext-macro-version
           (lambda _
             (substitute* "po/Makefile.in.in"
               (("0.18") "0.20"))))
         (replace 'bootstrap
           (lambda _
             (invoke "aclocal")
             (invoke "automake" "--add-missing")
             (invoke "autoconf"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)))
    (inputs
     (list ncurses readline))
    (home-page "https://abook.sourceforge.io/")
    (synopsis "Text-based address book")
    (description
     "Abook is a text-based address book program designed to use with the Mutt
mail client.")
    (license license:gpl2)))

(define-public anubis
  (package
    (name "anubis")
    ;; This 4.2.90 alpha release adds support for Guile 3 and has fixes for
    ;; other issues.
    (version "4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/anubis/anubis-"
             version ".tar.gz"))
       (sha256
        (base32
         "0b5ghaccy09l6fv0bg4my3yrxbw807wpwk14xvjih8j6ghrz62pz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf gettext-minimal m4))                     ;for the test suite
    (inputs
     (list gdbm
           gnutls
           gpgme
           gsasl
           guile-3.0
           libgcrypt ;gnutls support depends on libgcrypt
           libgpg-error))
    (outputs '("out" "debug"))
    (synopsis "SMTP message submission daemon")
    (description "Anubis is a daemon that sits between the Mail User
Agent (MUA) and the Mail Transfer Agent (MTA).  When a mail is sent by a user
in the MUA, it is first passed to Anubis, which performs additional processing
to the message before passing it on for delivery by the MTA.  Anubis may, for
example, modify the message headers or body, or encrypt or sign the message.")
    (home-page "https://www.gnu.org/software/anubis/manual/")
    (license license:gpl3+)))

(define-public mailutils
  (package
    (name "mailutils")
    (version "3.18")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/mailutils/mailutils-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "0xqm2cd263zym1zn3rfp0lfjz9mpkffi6v4hi9942q2iv344k42m"))
             (patches
              (search-patches "mailutils-variable-lookup.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'prepare-test-suite
                 (lambda _
                   ;; Use the right file name for `cat'.
                   (substitute* "testsuite/lib/mailutils.exp"
                     (("/bin/cat")
                      (which "cat")))

                   ;; Tests try to invoke 'mda' such that it looks up the
                   ;; 'root' user, which does not exist in the build
                   ;; environment.
                   (substitute* '("mda/mda/tests/testsuite"
                                  "mda/lmtpd/tests/testsuite")
                     (("root <")         "nobody <")
                     (("spool/root")     "spool/nobody")
                     (("root@localhost") "nobody@localhost"))

                   ;; The 'pipeact.at' tests generate a shell script; make
                   ;; sure it uses the right shell.
                   (substitute* '("sieve/tests/testsuite"
                                  "mh/tests/testsuite"
                                  "libmailutils/tests/lock.at")
                     (("#! ?/bin/sh")
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

                   ;; XXX: The ‘moderator: program discard’ test does not
                   ;; specify an explicit From: but does expect an exact
                   ;; match.  But why are all other tests unaffected?
                   (substitute* "sieve/tests/testsuite"
                     (("gray@")
                      "nixbld@"))

                   ;; 'frm' tests expect write access to $HOME.
                   (setenv "HOME" (getcwd))

                   ;; Avoid the message "I'm going to create the standard MH
                   ;; path for you", which would lead to one test failure
                   ;; (when diffing stdout of 'fmtcheck'.)
                   (call-with-output-file ".mh_profile"
                     (lambda (port)
                       (format port "Path: ~a/Mail-for-tests~%"
                               (getcwd))))

                   (substitute* "imap4d/tests/testclient.c"
                     (("\"/bin/sh\"")
                      (string-append "\"" (which "sh") "\""))))))
           #:configure-flags
           #~(list "--sysconfdir=/etc"
                   "--disable-static"

                   ;; Add "/X.Y" to the installation directory.
                   (string-append "--with-guile-site-dir="
                                  (assoc-ref %outputs "out")
                                  "/share/guile/site/"
                                  #$(match (assoc "guile"
                                                  (package-inputs this-package))
                                      (("guile" guile)
                                       (version-major+minor
                                        (package-version guile))))))))
    (native-inputs
     ;; Regeneration of the build system is triggered by touching the
     ;; 'libmailutils/tests/lock.at' file.
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("m4" ,m4)
       ("perl" ,perl)                           ;for 'gylwrap'
       ("texinfo" ,texinfo)
       ("dejagnu" ,dejagnu)))
    (inputs
     (list guile-3.0
           gsasl
           gnutls
           ncurses
           readline
           linux-pam
           libltdl
           libxcrypt
           gdbm
           ;; Required for SEARCH CHARSET.
           libunistring))
    (home-page "https://mailutils.org")
    (synopsis "Utilities and library for reading and serving mail")
    (description
     "GNU Mailutils is a collection of programs for managing, viewing and
processing electronic mail.  It contains both utilities and server daemons
and all operate in a protocol-agnostic way.  The underlying libraries are
also available, simplifying the addition of mail capabilities to new
software.  GNU Mailutils provides the following commands:
@itemize @command
@item dotlock
@item decodemail
@item frm
@item from
@item guimb
@item mail
@item mailutils
@item mailutils-config
@item messages
@item mimeview
@item movemail
@item popauth
@item putmail
@item readmsg
@item sieve
@end itemize")
    (license
     ;; Libraries are under LGPLv3+, and programs under GPLv3+.
     (list license:gpl3+ license:lgpl3+))))

(define-public mairix
  (let ((commit "1cc06f4a73ba4b940008c1ffc398d2ac708cd6d6")
        (revision "0"))
    (package
      (name "mairix")
      (version (git-version "0.24" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/vandry/mairix")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "12bhmk5j77cl3vjda48cmdysq1c2yjzvfv6zm4hlky6d5g3l49d7"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:parallel-tests? #f
        #:phases #~(modify-phases %standard-phases
                     (replace 'configure
                       (lambda* (#:key inputs #:allow-other-keys)
                         (invoke "./configure"
                                 (string-append "--prefix=" #$output)))))))
      (native-inputs
       (list bison flex))
      (inputs
       (list bzip2
             openssl
             perl
             xz
             zlib))
      (home-page "https://github.com/vandry/mairix")
      (synopsis "Program for indexing and searching email messages")
      (description
       "Mairix is a program for indexing and searching email messages stored in
Maildir, MH, MMDF or mbox folders.")
      (license license:gpl2))))

(define-public nmail
  (package
    (name "nmail")
    (version "4.54")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/d99kris/nmail/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bk2kq0pk1r4w5xv94yh37vrwxs8lczjg11gfraxh9cxyjigwsrp"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (invoke "ctest" "--output-on-failure")))))))
    (inputs
     (list curl
           cyrus-sasl
           expat
           file
           libetpan
           ncurses
           openssl
           sqlite
           (list util-linux "lib")
           xapian
           zlib))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/d99kris/nmail")
    (synopsis "Terminal-based email client")
    (description
     "@command{nmail} is an easily configurable terminal-based email client
with a @code{ncurses} user interface similar to @code{alpine} and
@code{pine}.")
    (license license:expat)))

(define-public goimapnotify
  (package
    (name "goimapnotify")
    (version "2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/shackra/goimapnotify")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06gmhrmfl31icr2lld9g2bnqjs0y2fq7kjfzm8zjg8d3n3vs7rl9"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "gitlab.com/shackra/goimapnotify"))
    (native-inputs
     (list go-github-com-emersion-go-imap
           go-github-com-emersion-go-imap-id
           go-github-com-emersion-go-imap-idle
           go-github-com-emersion-go-sasl
           go-github-com-fatih-color
           go-github-com-sirupsen-logrus
           go-github-com-spf13-viper))
    (home-page "https://gitlab.com/shackra/goimapnotify")
    (synopsis "Execute scripts on IMAP mailbox changes")
    (description
     "This package provides a CLI application to execute scripts on IMAP
mailbox changes (new/deleted/updated messages) using
@url{https://en.wikipedia.org/wiki/IMAP_IDLE, IDLE} and it is mostly
compatible with the configuration of
@url{https://github.com/a-sk/python-imapnotify, imapnotify made with
Python}.")
    (license license:gpl3+)))

(define-public go-gitlab.com-shackra-goimapnotify
  (deprecated-package "go-gitlab.com-shackra-goimapnotify" goimapnotify))

(define-public guile2.2-mailutils
  (package
    (inherit mailutils)
    (name "guile2.2-mailutils")
    (inputs
     (modify-inputs (package-inputs mailutils)
       (replace "guile" guile-2.2)))))

(define-public nullmailer
  (package
    (name "nullmailer")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "https://untroubled.org/nullmailer/"
                            "nullmailer-" version ".tar.gz")
             ;; Previous releases are moved to this subdirectory.
             (string-append "https://untroubled.org/nullmailer/archive/"
                            "nullmailer-" version ".tar.gz")))
       (sha256
        (base32 "0md8cf90fl2yf3zh9njjy42a673v4j4ygyq95xg7fzkygdigm1lq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-tls"
                   "--localstatedir=/var"
                   "--sysconfdir=/etc")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'patch-test-FHS-file-names
                 (lambda _
                   (with-directory-excursion "test"
                     (substitute* (list "functions.in"
                                        "tests/send")
                       ;; Fix some shebangs later generated on the fly.
                       (("/bin/sh") (which "bash"))))))
               (add-before 'check 'pass-PATH-to-tests
                 ;; ‘runtest’ launches each test through ‘env -’, clearing
                 ;; $PATH. The tests then source ‘functions’, which first
                 ;; demands a working $PATH only to clobber it later.  Pass
                 ;; our $PATH to the test environment and don't touch it after
                 ;; that.
                 (lambda _
                   (with-directory-excursion "test"
                     (substitute* "runtests"
                       (("env - bash")
                        (string-append "env - PATH=\"" (getenv "PATH") "\" bash")))
                     (substitute* "functions.in"
                       (("export PATH=.*") "")))))
               (add-before 'check 'delete-failing-tests
                 (lambda _
                   (with-directory-excursion "test/tests"
                     (for-each
                      delete-file
                      (list
                       ;; XXX ‘nullmailer-inject: nullmailer-queue failed: 15’
                       "inject/queue"
                       ;; XXX These require the not-yet-packaged tcpserver.
                       "protocols" "smtp-auth")))))
               (add-before 'install 'skip-install-data-local
                 ;; Don't try to install run-time files outside of the store.
                 (lambda _
                   (substitute* "Makefile"
                     ((" install-data-local") "")))))))
    (native-inputs
     ;; For tests.
     (list daemontools))   ; for svc
    (inputs
     (list gnutls))
    (home-page "https://untroubled.org/nullmailer/")
    (synopsis "Simple relay-only mail transfer agent")
    (description
     "Nullmailer is a simple replacement @acronym{MTA, Mail Transfer Agent} for
hosts that receive no local mail and only relay mail to a fixed set of smart
relays.  It's useful for systems such as Web servers that must be able to send
email notifications, without having to run a full-blown MTA such as sendmail
or qmail.

Nullmailer is designed to be simple to configure, easy to extend, and secure.
It requires little ongoing administration.  The included @command{sendmail}
emulator front-end should allow most (if not all) sendmail-compatible programs
to run without any changes.")
    (license (list license:lgpl2.1+ ; lib/cli++/ (but some files lack headers)
                   license:gpl2+)))) ; everything else

(define-public fetchmail
  (package
    (name "fetchmail")
    (version "6.4.37")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/fetchmail/branch_"
                           (version-major+minor version) "/"
                           "fetchmail-" version ".tar.xz"))
       (sha256
        (base32 "1sk9grjiibmaq8swfkr30vbfdz2i4ra1xrvsqdmbx6iyi5fjw62a"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-ssl="
                                  #$(this-package-input "openssl")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-fetchmailconf
                 (lambda _
                   (wrap-program (string-append #$output "/bin/fetchmailconf")
                    `("GUIX_PYTHONPATH" ":"
                      prefix (,(getenv "GUIX_PYTHONPATH")))))))))
    (inputs
     (list openssl
           ;; Needed for fetchmailconf
           bash-minimal
           python-future
           python-wrapper))
    (home-page "https://www.fetchmail.info/")
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
    (license license:gpl2+))) ; most files are actually public domain or x11

(define-public mutt
  (package
    (name "mutt")
    (version "2.2.14")
    (source (origin
             (method url-fetch)
             (uri (list
                    (string-append "https://bitbucket.org/mutt/mutt/downloads/"
                                   "mutt-" version ".tar.gz")
                    (string-append "http://ftp.mutt.org/pub/mutt/mutt-"
                                   version ".tar.gz")))
             (sha256
              (base32
               "1vqlvqjlldcrkb4m5nl44my0rfw7wsvlkyb2dwyz8fhy95nznqni"))
             (patches (search-patches "mutt-store-references.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list cyrus-sasl
           gdbm
           gpgme
           libidn2
           ncurses
           openssl
           perl
           sqlite))
    (arguments
     `(#:configure-flags '("--enable-smtp"
                           "--enable-imap"
                           "--enable-pop"
                           "--enable-gpgme"
                           "--enable-hcache" ; for header caching
                           "--enable-sidebar"
                           "--enable-autocrypt"
                           "--with-ssl"
                           "--with-sasl"
                           "--with-sqlite3" ; required for Autocrypt
                           "--with-idn2" ; recommended for Autocrypt
                           ;; So that mutt does not check whether the path
                           ;; exists, which it does not in the chroot.
                           "--with-mailpath=/var/mail")))
    (home-page "http://www.mutt.org/")
    (synopsis "Mail client")
    (description
     "Mutt is a small but very powerful text-based mail client for Unix
operating systems.")
    (license license:gpl2+)))

(define-public neomutt
  (package
    (name "neomutt")
    (version "20230517")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/neomutt/neomutt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kjllp2scgmpkl8yd0hwz6jmm98hr2r7qkb75ps9753fl96i4bfn"))))
    (build-system gnu-build-system)
    (inputs
     (list cyrus-sasl
           gdbm
           gpgme
           ncurses
           gnutls
           openssl ; for S/MIME
           perl
           kyotocabinet
           libxslt
           libidn2
           libxml2
           lmdb
           notmuch))
    (native-inputs
     `(("automake" ,automake)
       ("gettext-minimal" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("docbook-xsl" ,docbook-xsl)
       ("docbook-xml" ,docbook-xml-4.2)
       ("w3m" ,w3m)
       ("tcl" ,tcl)

       ;; Test file data for the unit tests included in the neomutt source.
       ("neomutt-test-files"
        ,(let ((commit "8629adab700a75c54e8e28bf05ad092503a98f75"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/neomutt/neomutt-test-files")
                   (commit commit)))
             (file-name (git-file-name "neomutt-test-files" commit))
             (sha256
              (base32 "1ci04nqkab9mh60zzm66sd6mhsr6lya8wp92njpbvafc86vvwdlr")))))))
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (list "--gpgme"

             ;; Database, implies header caching.
             "--disable-tokyocabinet"
             "--disable-qdbm"
             "--disable-bdb"
             "--lmdb"
             "--kyotocabinet"

             "--gdbm"

             "--gnutls"
             "--disable-ssl"
             "--sasl"
             (string-append "--with-sasl="
                            (assoc-ref %build-inputs "cyrus-sasl"))


             "--smime"
             "--notmuch"
             "--disable-idn"
             "--idn2"

             ;; If we do not set this, neomutt wants to check
             ;; whether the path exists, which it does not
             ;; in the chroot.
             "--with-mailpath=/var/mail"

             "--with-ui=ncurses"
             (string-append "--with-ncurses="
                            (assoc-ref %build-inputs "ncurses"))
             (string-append "--prefix="
                            (assoc-ref %outputs "out"))
             "--debug")
       #:phases
       (modify-phases %standard-phases
         ;; TODO: autosetup is meant to be included in the source,
         ;; but we should package autosetup and use our own version of it.
         (replace 'configure
           (lambda* (#:key outputs inputs configure-flags #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (flags `(,@configure-flags))
                    (bash (which "bash")))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               (apply invoke bash
                      (string-append (getcwd) "/configure")
                      flags))))
         (add-before 'check 'prepare-test-files
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "neomutt-test-files") "tests")
             (with-directory-excursion "tests"
               (setenv "NEOMUTT_TEST_DIR" (getcwd)) ; must be absolute
               (invoke "bash" "setup.sh")))))))
    (home-page "https://neomutt.org/")
    (synopsis "Command-line mail reader based on Mutt")
    (description
     "NeoMutt is a command-line mail reader which is based on mutt.
It adds a large amount of new and improved features to mutt.")
    (license license:gpl2+)))

(define-public gmime
  (package
    (name "gmime")
    (version "3.2.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jstedfast/gmime")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0493dngbasd2nngzsp4b8v4cnl4vb88hc1qga5y9w7l6c89hxn1w"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-gtk-doc=yes"
                   "--enable-introspection=yes")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-paths-in-tests
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
                                         (or prog (error "not found: "
                                                         base))))))))))))
    (native-inputs
     (list autoconf-2.71
           automake
           pkg-config
           gnupg                        ; for tests only
           gobject-introspection
           gtk-doc
           libtool
           vala
           which))                      ; to find libtool, &c.
    (inputs (list glib gpgme zlib))
    (home-page "https://spruce.sourceforge.net/gmime/")
    (synopsis "MIME message parser and creator library")
    (description
     "GMime provides a core library and set of utilities which may be used for
the creation and parsing of messages using the Multipurpose Internet Mail
Extension (MIME).")
    (license (list license:lgpl2.1+ license:gpl2+ license:gpl3+))))

(define-public altermime
  (package
    (name "altermime")
    (version "0.3.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pldaniels.com/altermime/altermime-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15zxg6spcmd35r6xbidq2fgcg2nzyv1sbbqds08lzll70mqx4pj7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:tests? #f                       ; there are none
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'fix-bugs
            (lambda _
              (substitute* "MIME_headers.c"
                (("hinfo->filename, sizeof\\(hinfo->name\\)")
                 "hinfo->filename, sizeof(hinfo->filename)")
                (("memset\\(hinfo->defects, 0, _MIMEH_DEFECT_ARRAY_SIZE\\);")
                 "memset(hinfo->defects, 0, sizeof(hinfo->defects));"))
              (substitute* "pldstr.c"
                (("if \\(\\(st->start\\)&&\\(st->start != '\\\\0'\\)\\)")
                 "if ((st->start)&&(*st->start != '\\0'))"))
              (substitute* "qpe.c"
                (("if \\(lineend != '\\\\0'\\)")
                 "if (*lineend != '\\0')"))))
          (add-after 'unpack 'install-to-prefix
            (lambda _
              (substitute* "Makefile"
                (("/usr/local") "${PREFIX}")
                (("cp altermime.*") "install -D -t ${PREFIX}/bin altermime\n"))))
          (add-after 'unpack 'disable-Werror
            (lambda _
              (substitute* "Makefile"
                (("-Werror") ""))))
          (add-after 'unpack 'do-not-strip
            (lambda _
              (substitute* "Makefile"
                (("\\bstrip\\b") "true")))))))
    (home-page "https://pldaniels.com/altermime/")
    (synopsis "Modify MIME-encoded messages")
    (description
     "alterMIME is a small program which is used to alter your mime-encoded
mailpack.  What can alterMIME do?

@enumerate
@item Insert disclaimers,
@item insert arbitrary X-headers,
@item modify existing headers,
@item remove attachments based on filename or content-type,
@item replace attachments based on filename.
@end enumerate
.")
    ;; MIME_headers.c is distributed under BSD-3; the rest of the code is
    ;; published under the alterMIME license.
    (license (list (license:non-copyleft "file://LICENSE")
                   license:bsd-3))))

(define-public astroid
  (package
    (name "astroid")
    (version "0.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astroidmail/astroid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17m99llggkg7xg72k8xaf7iipax7sgfhqa2a1qnlylndwa42f57b"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; https://github.com/astroidmail/astroid/pull/685
           (substitute* "tests/test_composed_message.cc"
             (("\\\\n\\.\\.\\.") "\\n...\\n"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules ((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils)
                  (ice-9 match))
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
       #:configure-flags (list "-GNinja")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-markdown-test
           ;; This test relies on the plugins and the test suite
           ;; cannot find the Astroid module.
           ;;  gi.require_version ('Astroid', '0.2')
           ;; ValueError: Namespace Astroid not available
           (lambda _
             (substitute* "tests/CMakeLists.txt"
               ((".*markdown.*") ""))))
         (replace 'build
           (lambda _
             (invoke "ninja" "-j" (number->string (parallel-job-count)))))
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server")))
               (setenv "HOME" (getcwd))
               (system (format #f "~a/bin/Xvfb :1 &" xorg-server))
               (setenv "DISPLAY" ":1"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
               (invoke "ctest" "."))))
         (replace 'install
           (lambda _
             (invoke "ninja" "install")))
         (add-after 'install 'wrap-with-GI_TYPELIB_PATH
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (paths (map (match-lambda
                                 ((outputs . directory)
                                  (let ((girepodir (string-append
                                                    directory
                                                    "/lib/girepository-1.0")))
                                    (if (file-exists? girepodir)
                                        girepodir
                                        #f))))
                               inputs)))
               (wrap-program (string-append out "/bin/astroid")
                 `("GI_TYPELIB_PATH" ":" prefix ,(filter identity paths))))))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list glib-networking
           gsettings-desktop-schemas
           gnupg
           ninja
           pkg-config
           ronn
           w3m
           xorg-server-for-tests))
    (inputs
     (list bash-minimal
           boost
           gmime
           gobject-introspection        ; it is referenced
           gtkmm-3
           libpeas
           libsass
           notmuch
           protobuf
           python-wrapper
           python-pygobject
           webkitgtk-with-libsoup2))
    (propagated-inputs
     (list adwaita-icon-theme)) ; Required for the thread view
    (home-page "https://astroidmail.github.io/")
    (synopsis "GTK frontend to the notmuch mail system")
    (description
     "Astroid is a lightweight and fast Mail User Agent that provides a
graphical interface to searching, display and composing email, organized in
thread and tags.  Astroid uses the notmuch backend for searches through tons of
email.  Astroid searches, displays and compose emails — and relies on other
programs for fetching, syncing and sending email.")
    (license (list license:gpl3+        ; 'this program'
                   license:lgpl2.1+)))) ; code from geary, gmime

(define-public ripmime
  ;; Upstream does not tag or otherwise provide any releases (only a version
  ;; number in the source)
  (let ((commit "a556ffe08d620602475c976732e8e1a82f3169e9")
        (revision "1"))
    (package
      (name "ripmime")
      (version (git-version "1.4.0.10" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/inflex/ripMIME")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1z8ar8flvkd9q3ax4x28sj5pyq8ykk5pq249y967lj2406lxparh"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Source has no configure script
           (delete 'configure)
           ;; Buildcodes make the build non-reproducible; remove them
           (add-after 'unpack 'strip-buildcodes
             (lambda _
               (substitute* "generate-buildcodes.sh"
                 (("`date \\+%s`") "0")
                 (("`date`") "0")
                 (("`uname -a`") "Guix"))
               #t))
           ;; https://github.com/inflex/ripMIME/pull/16 makes 'mkdir-p-bin-man unnecessary
           (add-before 'install 'mkdir-p-bin-man
             (lambda _
               (mkdir-p (string-append (assoc-ref %outputs "out") "/bin"))
               (mkdir-p (string-append (assoc-ref %outputs "out") "/man"))
               #t)))
         ;; Makefile has no tests
         #:tests? #f
         #:make-flags (list (string-append "LOCATION=" (assoc-ref %outputs "out"))
                            "CC=gcc")))
      (synopsis "Extract attachments from MIME-encoded email")
      (description
       "ripMIME is a small program to extract the attached files out of a
MIME-encoded email package.")
      (home-page "https://github.com/inflex/ripMIME")
      (license license:bsd-3))))

(define-public mailcap
  (let* ((version "2.1.53")
         (tag ;; mailcap tags their releases like this: rMajor-minor-patch
          (string-append "r" (string-join (string-split version #\.) "-"))))
    (package
      (name "mailcap")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://pagure.io/mailcap.git")
               (commit tag)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "14939pq7h25rh9100z72vzzx810yqg98im9gz2fbhh47iaj1wrbb"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (add-before 'install 'set-dest-dir
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (setenv "DESTDIR" out)
                 (substitute* "Makefile"
                   (("/usr") ""))       ; This allows the man page to install.
                 #t))))))
      (native-inputs
       (list python))           ; for tests
      (synopsis "MIME type associations for file types")
      (description
       "This package provides MIME type associations for file types.")
      (home-page "https://pagure.io/mailcap")
      (license (list license:expat              ; mailcap.5
                     license:public-domain))))) ; mailcap and mime.types

(define-public bogofilter
  (package
    (name "bogofilter")
    (version "1.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/bogofilter/bogofilter-stable/"
                           "bogofilter-" version ".tar.xz"))
       (sha256
        (base32 "1sl9xrnnlk2sn8gmibhn8li09vnansjbxb9l1182qmgz7cvs2j1j"))))
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
    (native-inputs (list flex))
    (inputs (list bdb))
    (home-page "https://bogofilter.sourceforge.io/")
    (synopsis "Mail classifier based on a Bayesian filter")
    (description
     "Bogofilter is a mail filter that classifies mail as spam or ham
 (non-spam) by a statistical analysis of the message's header and
content (body).  The program is able to learn from the user's classifications
and corrections.  It is based on a Bayesian filter.")
    (license license:gpl3+)))

(define-public offlineimap3
  (package
    (name "offlineimap3")
    (version "8.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OfflineIMAP/offlineimap3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y3giaz9i8vvczlxkbwymfkn3vi9fv599dy4pc2pn2afxsl4mg2w"))))
    (build-system python-build-system)
    (native-inputs
     (list asciidoc))
    (inputs
     (list python-distro python-imaplib2 python-rfc6555))
    (arguments
     `(;; Tests require a modifiable IMAP account.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-documentation
           (lambda _
             (substitute* "docs/Makefile"
               ;; Prevent xmllint and xsltproc from downloading a DTD file.
               (("a2x -v") "a2x --no-xmllint --xsltproc-opts=--nonet -v"))
             (invoke "make" "-C" "docs" "man")))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (install-file "docs/offlineimap.1" (string-append man "/man1"))
               (install-file "docs/offlineimapui.7" (string-append man "/man7"))))))))
    (home-page "https://www.offlineimap.org")
    (synopsis "Sync emails between two repositories")
    (description
     "OfflineImap synchronizes emails between two repositories, so that you
can read the same mailbox from multiple computers.  It supports IMAP as REMOTE
repository and Maildir/IMAP as LOCAL repository.")
    (license license:gpl2+)))

(define-public offlineimap
  (deprecated-package "offlineimap" offlineimap3))

(define-public emacs-mew
  (let ((commit "35772ee0b44dd7e56b0f3899b27fa545b2bc6f03")
        (revision "1"))
    (package
      (name "emacs-mew")
      (version (git-version "6.9" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/kazu-yamamoto/Mew")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0xazygwdc328m5l31rxjazq9giv2xrygp2p2q455lf3jhdxwq1km"))))
      (build-system gnu-build-system)
      (arguments
       (let ((icon-dir  #~(string-append #$output "/share/mew")))
         (list
          #:modules '((guix build gnu-build-system)
                      (guix build utils)
                      ((guix build emacs-build-system) #:prefix emacs:)
                      (guix build emacs-utils))
          #:imported-modules %emacs-build-system-modules
          #:tests? #f
          #:configure-flags
          #~(list (string-append "--with-elispdir="
                                 (emacs:elpa-directory #$output))
                  (string-append "--with-etcdir=" #$icon-dir))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'configure 'patch-mew-icon-directory
                (lambda _
                  (emacs-substitute-sexps "elisp/mew-key.el"
                    ("(def.* mew-icon-directory"
                     `(progn
                       (add-to-list 'image-load-path 'mew-icon-directory)
                       ,#$icon-dir)))))
              (add-after 'unpack 'generate-autoloads
                (lambda _
                  (emacs-generate-autoloads "mew" "elisp")
                  (substitute* "elisp/mew-autoloads.el"
                    ((";; no-byte-compile.*") ""))
                  ;; Add generated autoloads to Makefile, so they get compiled
                  (substitute* "elisp/Makefile"
                    (("OBJS =") "OBJS = mew-autoloads.elc")
                    (("SRCS =") "SRCS = mew-autoloads.el"))))))))
      (native-inputs
       (list emacs))
      (propagated-inputs
       (list ruby        ; to set GEM_PATH so ruby-sqlite3 is found at runtime
             ruby-sqlite3))            ; optional for the database of messages
      (home-page "https://mew.org")
      (synopsis "Emacs e-mail client")
      (description "Mew (Messaging in the Emacs World) is a user interface
for text messages, multimedia messages (MIME), news articles and
security functionality including PGP, S/MIME, SSH, and SSL.")
      (license license:bsd-3))))

(define-public mu
  (package
    (name "mu")
    (version "1.12.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djcb/mu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jgphx4yd899zg8g0w065ml962jwj848hlc4svpzqxpsgg2bb8m3"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config
           emacs-minimal
           gnupg                        ; for tests
           texinfo))
    (inputs
     (list glib gmime guile-3.0 xapian readline python))
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build emacs-utils)
                  ((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  (guix build utils))
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build guile-build-system)
                           (guix build emacs-utils))
      #:configure-flags
      #~(list (format #f "-Dguile-extension-dir=~a/lib" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-bin-references
            (lambda _
              (substitute* '("guile/tests/test-mu-guile.cc"
                             "mu/tests/test-mu-query.cc")
                (("/bin/sh") (which "sh")))
              (substitute* '("lib/tests/bench-indexer.cc"
                             "lib/utils/mu-test-utils.cc")
                (("/bin/rm") (which "rm")))
              (substitute* '("lib/mu-maildir.cc")
                (("/bin/mv") (which "mv")))))
          (add-after 'install 'fix-ffi
            (lambda _
              (substitute* (find-files #$output "mu.scm")
                (("\"libguile-mu\"")
                 (format #f "\"~a/lib/libguile-mu\"" #$output)))))
          (add-after 'install 'install-emacs-autoloads
            (lambda _
              (emacs-generate-autoloads
               "mu4e"
               (string-append #$output
                              "/share/emacs/site-lisp/mu4e"))))
          (add-after 'install 'wrap-executable
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (version (target-guile-effective-version))
                     (scm (string-append #$output "/share/guile/site/" version)))
                (wrap-program (string-append bin "/mu")
                  `("GUILE_LOAD_PATH" ":" prefix (,scm)))))))))
    (home-page "https://www.djcbsoftware.nl/code/mu/")
    (synopsis "Quickly find emails")
    (description
     "Mu is a tool for dealing with e-mail messages stored in the
Maildir format.  Mu's purpose in life is to help you to quickly find the
messages you need; in addition, it allows you to view messages, extract
attachments, create new maildirs, and so on.")
    (license license:gpl3+)))

(define-public alot
  (package
    (name "alot")
    (version "0.10")
    (source (origin
              (method git-fetch)
              ;; package author intends on distributing via github rather
              ;; than pypi:
              ;; https://github.com/pazz/alot/issues/877#issuecomment-230173331
              (uri (git-reference
                     (url "https://github.com/pazz/alot")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0awf1phdy1wqm01cy9zmvqlw6c8pvkxm2f9ncjd0cmzxqnmq1dyn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-before 'check 'fix-tests
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((gnupg (assoc-ref inputs "gnupg")))
              (substitute* "tests/test_crypto.py"
                (("gpg2") (string-append gnupg "/bin/gpg")))
              #t)))
        (add-before 'check 'disable-failing-tests
         ;; FIXME: Investigate why these tests are failing.
         (lambda _
          (substitute* "tests/test_helper.py"
            (("def test_env_set") "def _test_env_set"))
          (substitute* "tests/commands/test_global.py"
            (("def test_no_spawn_no_stdin_attached")
             "def _test_no_spawn_no_stdin_attached"))
          ;; FIXME: Investigate why this test hangs.
          (substitute* "tests/db/test_manager.py"
            (("def test_save_named_query")
             "def _test_save_named_query"))
          #t)))))
    (native-inputs
     (list procps python-mock))
    (inputs
     (list gnupg
           python-magic
           python-configobj
           python-twisted
           python-service-identity
           python-urwid
           python-urwidtrees
           python-gpg
           python-notmuch2))
    (home-page "https://github.com/pazz/alot")
    (synopsis "Command-line MUA using Notmuch")
    (description
     "Alot is a terminal-based mail user agent based on the Notmuch mail
indexer.  It is written in Python using the @code{urwid} toolkit and features
a modular and command prompt driven interface to provide a full mail user
agent (@dfn{MUA}) experience as an alternative to the Emacs mode shipped with
Notmuch.")
    (license license:gpl3+)))

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
               (url "https://github.com/kspi/notifymuch")
               (commit commit)))
         (sha256
          (base32
           "1lssr7iv43mp5v6nzrfbqlfzx8jcc7m636wlfyhhnd8ydd39n6k4"))
         (file-name (string-append name "-" version "-checkout"))))
      (build-system python-build-system)
      (inputs
       (list bash-minimal python-notmuch python-pygobject gobject-introspection
             libnotify gtk+))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'install 'wrap-binary
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin/notifymuch")))
                 (wrap-program bin
                   `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
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
      (license license:gpl3))))

(define-public notmuch
  (package
    (name "notmuch")
    (version "0.38.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://notmuchmail.org/releases/notmuch-"
                           version ".tar.xz"))
       (sha256
        (base32 "0zll3s39s065pl9228xpklkjklllkyb3bf1szh0fw0rbfkjfp0jj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list "V=1"                      ; verbose test output
              "NOTMUCH_TEST_TIMEOUT=1h") ; don't fail on slow machines
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "CC" #$(cc-for-target))
              (setenv "CONFIG_SHELL" (search-input-file inputs "/bin/sh"))
              (invoke "./configure"
                      (string-append "--prefix=" #$output)
                      "--without-emacs")))
          (add-before 'check 'prepare-test-environment
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TEST_CC" #$(cc-for-target))
              ;; Patch various inline shell invocations.
              (let ((sh (search-input-file inputs "/bin/sh")))
                (substitute* (find-files "test" "\\.sh$")
                  (("/bin/sh") sh))))))))
    (native-inputs
     (list bash-completion
           pkg-config
           python
           python-docutils
           python-sphinx
           texinfo
           ;; The following are required for tests only.
           emacs-no-x           ; -minimal lacks libxml, needed for some tests
           which
           dtach
           git-minimal/pinned
           gnupg
           man-db
           perl))
    (inputs
     (list glib gmime sfsexp talloc xapian zlib))
    (home-page "https://notmuchmail.org/")
    (synopsis "Thread-based email index, search, and tagging")
    (description
     "Notmuch is a command-line based program for indexing, searching, read-
ing, and tagging large collections of email messages.")
    (license license:gpl3+)))

(define-public emacs-notmuch
  (package
    (inherit notmuch)
    (name "emacs-notmuch")
    (build-system emacs-build-system)
    (native-inputs '())
    (inputs
     (list notmuch))
    (arguments
     (list
      #:exclude #~(cons* "make-deps.el" "rstdoc.el" %default-exclude)
      #:include #~(cons* "notmuch-logo.svg" %default-include)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "emacs")))
          (add-after 'chdir 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((notmuch (search-input-file inputs "/bin/notmuch")))
                (substitute* "notmuch-lib.el"
                  (("\"notmuch\"")
                   (string-append "\"" notmuch "\""))))))
          ;; Install desktop files so that mailto URIs can be opened using
          ;; emacs-notmuch.
          (add-after 'install 'install-desktop-files
            (lambda* (#:key inputs #:allow-other-keys)
              (install-file "notmuch-emacs-mua"
                            (string-append #$output "/bin"))
              (let ((applications (string-append #$output "/share/applications")))
                (install-file "notmuch-emacs-mua.desktop"
                              applications)
                (copy-file "notmuch-emacs-mua.desktop"
                           (string-append applications
                                          "/notmuch-emacsclient-mua.desktop"))
                (substitute* (string-append applications
                                            "/notmuch-emacsclient-mua.desktop")
                  (("Exec=notmuch-emacs-mua" all)
                   (string-append all " --client")))))))))
    (synopsis "Run Notmuch within Emacs")
    (description
     "This package provides an Emacs-based interface to the Notmuch mail
system.")))

(define-public notmuch-addrlookup-c
  (package
    (name "notmuch-addrlookup-c")
    (version (string-append "9"))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aperezdc/notmuch-addrlookup-c")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1j3zdx161i1x4w0nic14ix5i8hd501rb31daf8api0k8855sx4rc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; no tests
       #:make-flags (list "CC=gcc")
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
     (list pkg-config))
    (inputs
     (list glib notmuch))
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
    (inputs (list notmuch))
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
                  (string-append notmuch "/lib/libnotmuch.so.")))))))))
    (home-page (package-home-page notmuch))
    (synopsis "Python bindings of the Notmuch mail indexing library")
    (description
     "This package provides Python bindings to use the Notmuch mail indexing
and search library.")
    (license license:gpl3+)))

(define-public python-notmuch2
  (package
    (inherit python-notmuch)
    (name "python-notmuch2")
    (version (package-version notmuch))
    (propagated-inputs (list python-cffi))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; This python package lives in a subdirectory of the notmuch source
          ;; tree, so chdir into it before building.
          (add-after 'unpack 'enter-python-dir
            (lambda _ (chdir "bindings/python-cffi")))
          ;; python-build-system does not invoke the configure script
          ;; so _notmuch_config.py is missing
          (add-after 'enter-python-dir 'create-notmuch-config
            (lambda* (#:key inputs #:allow-other-keys)
              (with-output-to-file "_notmuch_config.py"
                (lambda _
                  (display
                   (string-append
                    "NOTMUCH_INCLUDE_DIR="
                    "'" (dirname (search-input-file inputs "include/notmuch.h")) "'\n"
                    "NOTMUCH_LIB_DIR="
                    "'" (dirname (search-input-file inputs "lib/libnotmuch.so")) "'"))))))
          ;; version.txt is not included in notmuch, so we patch in the version number
          (add-after 'create-notmuch-config 'patch-setup.py
            (lambda _
              (substitute* "setup.py"
                (("NOTMUCH_VERSION_FILE")
                 "'/dev/null'")
                (("version=VERSION,")
                 (string-append "version='" #$version "',"))))))))
    (synopsis "Pythonic bindings for the notmuch mail database using CFFI")
    (license license:gpl3+)))

(define-public bower
  (package
    (name "bower")
    (version "1.1")
    (home-page "https://github.com/wangp/bower")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zh2zlwdmpq6195kg87q5dh864jvabxnrvfvzhks53pf9wjkv80a"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list "bower" "man"
                           (string-append "CC=" #+(cc-for-target))
                           (string-append "prefix=" #$output))
      #:parallel-tests? #f              ;parallelism breaks test suite
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-executables
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/detect_mime_type.m"
                (("\"file\"")
                 (format #f "~s" (search-input-file inputs "bin/file"))))
              (substitute* "src/compose.m"
                (("\"base64\"")
                 (format #f "~s" (search-input-file inputs "bin/base64"))))
              (substitute* "src/prog_config.m"
                (("shell_quoted\\(\"false\")")
                 (format #f "shell_quoted(~s)"
                         (search-input-file inputs "bin/false")))
                (("shell_quoted\\(\"notmuch\")")
                 (format #f "shell_quoted(~s)"
                         (search-input-file inputs "bin/notmuch")))
                (("/usr/bin/sendmail")
                 (search-input-file inputs "/sbin/sendmail")))))
          (replace 'check
            (lambda* (#:key parallel-tests? tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "-C" "tests"
                        "-j" (if parallel-tests?
                                 (number->string (parallel-job-count))
                                 "1")))))
          (replace 'install
            (lambda* _
              (install-file "bower" (string-append #$output "/bin"))
              (install-file "bower.1" (string-append #$output
                                                     "/share/man/man1")))))))
    (native-inputs
     (list diffutils
           gawk
           mercury
           pandoc
           util-linux))
    (inputs
     (list coreutils
           gpgme
           ncurses
           notmuch
           sendmail))
    (synopsis "Terminal client for the Notmuch email system")
    (description "@code{bower} is a curses front-end for the Notmuch email
system, written in the Mercury language.")
    (license license:gpl3+)
    (properties `((cpe-name . "bower-cpe-refers-to-a-different-bower")))))

(define-public muchsync
  (package
    (name "muchsync")
    (version "7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.muchsync.org/src/" "muchsync-"
                           version ".tar.gz"))
       (sha256
        (base32 "1b5ylf0xgb59x6hna5gllm6nb1jn50wqvp7xfls83x0frmpjygpq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pandoc pkg-config))
    (inputs
     (list openssl notmuch sqlite xapian))
    (home-page "https://www.muchsync.org/")
    (synopsis "Synchronize notmuch mail across machines")
    (description
     "Muchsync brings Notmuch to all of your computers by synchronizing your
mail messages and Notmuch tags across machines.  The protocol is heavily
pipelined to work efficiently over high-latency networks such as mobile
broadband.  Muchsync supports arbitrary pairwise synchronization among
replicas.  A version-vector-based algorithm allows it to exchange only the
minimum information necessary to bring replicas up to date regardless of which
pairs have previously synchronized.")
    (license license:gpl2+)))           ; with OpenSSL libcrypto exception

(define-public getmail6
  (package
    (name "getmail6")
    (version "6.18.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/getmail6/getmail6")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dr2grcxnn21prv6dj8sd9c68zs1fxy00wc676rnghcs4yfnb78h"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))      ;tests require docker
    (home-page "https://github.com/getmail6/getmail6")
    (synopsis "Mail retriever")
    (description
     "A flexible, extensible mail retrieval system with support for POP3,
IMAP4, SSL variants of both, maildirs, mboxrd files, external MDAs, arbitrary
message filtering, single-user and domain-mailboxes, and many other useful
features.  This is a fork derived from getmail 5.14, aimed at Python 3
compatibility.")
    (license license:gpl2+)))           ;see docs/COPYING

(define-public getmail
  (deprecated-package "getmail" getmail6))

(define-public libetpan
  (package
    (name "libetpan")
    (version "1.9.4")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url  "https://github.com/dinhviethoa/libetpan")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
               (base32 "0g7an003simfdn7ihg9yjv7hl2czsmjsndjrp39i7cad8icixscn"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool pkg-config))
    (propagated-inputs
     ;; 'libetpan-config --libs' returns '-lssl -lcrypto -lsasl2', so these
     ;; libraries need to be propagated.
     (list cyrus-sasl openssl))
    (inputs
     (list curl expat zlib))
    (arguments
      '(#:configure-flags
        '("--disable-static" "--disable-db")))
    (home-page "https://www.etpan.org/libetpan.html")
    (synopsis "Portable middleware for email access")
    (description
     "The purpose of this mail library is to provide a portable, efficient
framework for different kinds of mail access: IMAP, SMTP, POP and NNTP.  It
provides an API for C language.  It's the low-level API used by MailCore and
MailCore 2.")
    (license license:bsd-3)))

(define-public compface
  (package
    (name "compface")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ftp.heanet.ie/mirrors/"
                                  "ftp.xemacs.org/aux/"
                                  "compface-" version ".tar.gz"))
              (sha256
               (base32
                "09b89wg63hg502hsz592cd2h87wdprb1dq1k1y07n89hym2q56d6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis "Portrait image compressor")
    (description "This package takes your 48x48x1 portrait image and
compresses it.")
    (home-page "https://legacy.cs.indiana.edu/ftp/faces/")
    (license (license:x11-style "file://README"))))

(define-public claws-mail
  (package
    (name "claws-mail")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.claws-mail.org/releases/claws-mail-"
                       version ".tar.xz"))
       (sha256
        (base32 "1q8wb2fh5fmbbyrvzdwkhxkzdbsvyk5w783z8qlg05mris41vp4m"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-demo-plugin")
       #:make-flags
       ;; Disable updating icon cache since it's done by the profile hook.
       ;; Conflict with other packages in the profile would be inevitable
       ;; otherwise.
       (list
        "gtk_update_icon_cache=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use absolute paths to referenced programs.
             (substitute* "src/common/defs.h"
               (("/usr/bin/mh/inc")
                (search-input-file inputs "/bin/mu-mh/inc"))
               (("/usr/sbin/sendmail")
                (search-input-file inputs "/sbin/sendmail")))))
         (add-before 'build 'patch-mime
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/procmime.c"
               (("/usr/share/mime/globs")
                (search-input-file inputs "/share/mime/globs"))))))))
    (native-inputs
     (list bison
           ;;docbook-utils
           flex
           gettext-minimal
           gobject-introspection
           intltool
           pkg-config))
    (inputs
      (list bogofilter
            cairo
            compface
            curl
            dbus
            dbus-glib
            enchant
            expat
            fontconfig
            ghostscript
            glib
            gnupg
            gnutls
            gpgme
            gsettings-desktop-schemas
            gtk+
            gumbo-parser
            ;;j-pilot
            libarchive
            libcanberra
            libetpan
            libgdata
            libical
            libindicator
            libnotify
            (librsvg-for-system)
            libsm
            libsoup
            libxml2
            mailutils
            nettle
            network-manager
            openldap
            perl
            poppler
            python
            python-pygobject
            sendmail
            shared-mime-info
            startup-notification
            ;;webkitgtk
            ytnef))
    (propagated-inputs
     (list dconf))
    (synopsis "GTK-based Email client")
    (description "Claws-Mail is an email client (and news reader) based on GTK+.
The appearance and interface are designed to be familiar to new users coming
from other popular email clients, as well as experienced users.  Almost all
commands are accessible with the keyboard.  Plus, Claws-Mail is extensible via
addons which can add many functionalities to the base client.")
    (home-page "https://www.claws-mail.org/")
    (license license:gpl3+))) ; most files are actually public domain or x11

(define-public msmtp
  (package
    (name "msmtp")
    (version "1.8.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://marlam.de/msmtp/releases"
                           "/msmtp-" version ".tar.xz"))
       (sha256
        (base32 "1n363w94s2jjkijnqg5mb4m7wk0dy20s9bk0gqk8kwff8j1liz3c"))))
    (build-system gnu-build-system)
    (inputs
     (list libsecret gnutls zlib gsasl))
    (native-inputs
     (list pkg-config))
    (home-page "https://marlam.de/msmtp/")
    (arguments
     (list
       #:configure-flags
       #~(list "--with-libgsasl"
               "--with-libidn"
               "--with-tls=gnutls")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'install-additional-files
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out #$output)
                      (bin (string-append out "/bin"))
                      (doc (string-append out "/share/doc/msmtp"))
                      (msmtpq "scripts/msmtpq")
                      (msmtpqueue "scripts/msmtpqueue")
                      (vimfiles (string-append
                                  out "/share/vim/vimfiles/pack/guix/start/msmtp/syntax")))
                 (install-file (string-append msmtpq "/msmtpq") bin)
                 (install-file (string-append msmtpq "/msmtp-queue") bin)
                 (install-file (string-append msmtpqueue "/msmtp-enqueue.sh") bin)
                 (install-file (string-append msmtpqueue "/msmtp-listqueue.sh") bin)
                 (install-file (string-append msmtpqueue "/msmtp-runqueue.sh") bin)
                 (install-file (string-append msmtpq "/README.msmtpq") doc)
                 (install-file "scripts/vim/msmtp.vim" vimfiles)))))))
    (properties
     '((release-monitoring-url . "https://marlam.de/msmtp/download/")))
    (synopsis
     "Simple and easy to use SMTP client with decent sendmail compatibility")
    (description
     "msmtp is an SMTP client.  In the default mode, it transmits a mail to
an SMTP server (for example at a free mail provider) which takes care of further
delivery.")
    (license license:gpl3+)))

(define-public exim
  (package
    (name "exim")
    (version "4.98")
    (source
     (origin
       (method url-fetch)
       (uri (let ((file-name (string-append "exim-" version ".tar.xz")))
              (list (string-append "https://ftp.exim.org/pub/exim/exim4/"
                                   file-name)
                    ;; ‘Fix’ releases (exim-x.y.z.f) are kept separately.
                    (string-append "https://ftp.exim.org/pub/exim/exim4/fixes/"
                                   file-name)
                    ;; After a new non-fix release, the old one is moved here.
                    (string-append "https://ftp.exim.org/pub/exim/exim4/old/"
                                   file-name))))
       (sha256
        (base32 "1xsjb2hqasxsqsmrcv98c2dvfgcsiy0j0g229fx974lzfy511g0f"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 ;; We'd use #:make-flags but the top-level Makefile calls
                 ;; others recursively, so just set all variables this way.
                 (lambda* (#:key outputs inputs #:allow-other-keys)
                   (substitute* (list "Makefile" "OS/Makefile-Default")
                     (("(RM_COMMAND=).*" all var)
                      (string-append var "rm\n")))
                   (copy-file "src/EDITME" "Local/Makefile")
                   (copy-file "exim_monitor/EDITME" "Local/eximon.conf")
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "Local/Makefile"
                       (("(BIN_DIRECTORY=).*" all var)
                        (string-append var out "/bin\n"))
                       (("(CONFIGURE_FILE=).*" all var)
                        (string-append var out "/etc/exim.conf\n"))
                       (("(EXIM_USER=).*" all var)
                        (string-append var "nobody\n"))
                       (("(FIXED_NEVER_USERS=).*" all var)
                        (string-append var "\n")) ; no root in build environment
                       (("(COMPRESS_COMMAND=).*" all var)
                        (string-append var (search-input-file inputs "bin/gzip")
                                       "\n"))
                       (("(ZCAT_COMMAND=).*" all var)
                        (string-append var (search-input-file inputs "bin/zcat")
                                       "\n"))
                       (("# (USE_GNUTLS(|_PC)=.*)" all line)
                        (string-append line "\n"))
                       (("# (AUTH_CRAM_MD5=yes)" all line) line)
                       (("# (AUTH_DOVECOT=yes)" all line) line)
                       (("# (AUTH_EXTERNAL=yes)" all line) line)
                       (("# (AUTH_PLAINTEXT=yes)" all line) line)
                       (("# (AUTH_SPA=yes)" all line) line)
                       (("# (AUTH_TLS=yes)" all line) line))
                     ;; This file has hard-coded relative file names for tools
                     ;; despite the zcat configuration above.
                     (substitute* "src/exigrep.src"
                       (("'(bzcat|xzcat|zcat|lzma)'" _ command)
                        (format #f "'~a'"
                                (search-input-file
                                 inputs (string-append "bin/" command))))))))
               (add-before 'build 'fix-sh-file-names
                 (lambda _
                   (substitute* (list "scripts/lookups-Makefile"
                                      "scripts/reversion")
                     (("SHELL=/bin/sh") "SHELL=sh"))
                   (substitute* "scripts/Configure-config.h"
                     (("\\| /bin/sh") "| sh"))
                   (patch-shebang "scripts/Configure-eximon")))
               (add-before 'build 'fix-perl-file-names
                 (lambda _
                  (substitute* (list  "Local/Makefile"
                                      "OS/Makefile-Default")
                    (("PERL_COMMAND=/usr/bin/perl")
                     (string-append "PERL_COMMAND=" #$perl "/bin/perl")))))
               (add-before 'build 'build-reproducibly
                 (lambda _
                   ;; The ‘compilation number’ increments on every build in the
                   ;; same source tree and varies across different (parallel?)
                   ;; builds.  Make it a ‘constant number’ instead.
                   (substitute* "src/version.c"
                     (("#include \"cnumber.h\"") "1")))))
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   "INSTALL_ARG=-no_chown")
           ;; No ‘check’ target.  The ‘test/’ suite assumes that particular
           ;; build options were (not) used and that it can freely ‘sudo’.
           #:tests? #f))
    (native-inputs
     (list pcre2 perl pkg-config))
    (inputs
     (list bdb-5.3     ; ‘#error Version 6 and later BDB API is not supported’
           bzip2
           gnutls/dane
           gzip
           libnsl
           libxaw
           libxcrypt
           libxt
           perl
           perl-file-fcntllock
           xz))
    (home-page "https://www.exim.org/")
    (synopsis
     "Message Transfer Agent (MTA) developed at the University of Cambridge")
    (description
     "Exim is a message transfer agent (MTA) developed at the University of
Cambridge for use on Unix systems connected to the Internet.  In style it is
similar to Smail 3, but its facilities are more general.  There is a great
deal of flexibility in the way mail can be routed, and there are extensive
facilities for checking incoming mail.")
    (properties '((lint-hidden-cve . ("CVE-2020-28017"))))
    (license license:gpl2+)))

(define-public dovecot
  (package
    (name "dovecot")
    ;; Also update dovecot-pigeonhole when updating to a new minor version.
    (version "2.3.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.dovecot.org/releases/"
                           (version-major+minor version) "/"
                           "dovecot-" version ".tar.gz"))
       (sha256
        (base32 "0zh9971d49dl5q1km31jnrd3vg53j9aaxnppic412xi9qiwa341d"))
       (patches
        (search-patches "dovecot-opensslv3.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list bzip2
           clucene
           icu4c
           libsodium ; extra password algorithms
           libstemmer
           libunwind
           libxcrypt
           linux-pam
           lz4
           openssl
           sqlite
           zlib
           `(,zstd "lib")))
    (arguments
     `(#:configure-flags '("--sysconfdir=/etc"
                           "--localstatedir=/var"
                           "--with-sqlite"  ; not auto-detected
                           "--with-lucene"
                           "--with-moduledir=/usr/lib/dovecot") ; not auto-detected
       ;; The -rdynamic linker flag is needed for the backtrace() function to
       ;; have symbol names rather than just addresses.  Dovecot's tests rely
       ;; on this, see https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=962630.
       #:make-flags (list "LDFLAGS=-rdynamic")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-names
           (lambda _
             (substitute* "src/lib-program-client/test-program-client-local.c"
               (("(/bin/| )cat") (which "cat"))
               (("/bin/echo") (which "echo"))
               (("/bin/false") (which "false"))
               (("/bin/sh") (which "bash"))
               (("head") (which "head"))
               (("sleep") (which "sleep")))
             (substitute* (list "src/lib-smtp/test-bin/sendmail-exit-1.sh"
                                "src/lib-smtp/test-bin/sendmail-success.sh")
               (("cat") (which "cat")))))
         (replace 'install
           (lambda* (#:key outputs make-flags #:allow-other-keys)
             ;; The .la files don't like having the moduledir moved.
             (for-each delete-file (find-files "." "\\.la"))
             ;; Simple hack to avoid installing a trivial README in /etc.
             (apply invoke "make" "install" "sysconfdir=/tmp/bogus"
                    (string-append "moduledir=" (assoc-ref outputs "out") "/lib/dovecot")
                    make-flags))))))
    (home-page "https://www.dovecot.org")
    (synopsis "Secure POP3/IMAP server")
    (description
     "Dovecot is a mail server whose major goals are security and reliability.
It supports mbox/Maildir and its own dbox/mdbox formats.")
    ;; Most source files are covered by either lgpl2.1 or expat.  The SHA code
    ;; is covered by a variant of BSD-3, and UnicodeData.txt is covered by the
    ;; Unicode, Inc. License Agreement for Data Files and Software.
    (license (list license:lgpl2.1 license:expat
                   (license:non-copyleft "file://COPYING")))))

(define-public dovecot-pigeonhole
  (let ((dovecot-version (version-major+minor (package-version dovecot))))
    (package
      (name "dovecot-pigeonhole")
      (version "0.5.21.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append
               "https://pigeonhole.dovecot.org/releases/" dovecot-version "/"
               "dovecot-" dovecot-version "-pigeonhole-" version ".tar.gz"))
         (sha256
          (base32 "14j6bj9dc0c2f6pi251jyhfiwyg7n9gi2c840vg261v29cldnxq3"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             ;; RFC licencing is ad-hoc and rarely free.  Remove them all.
             (delete-file-recursively "doc/rfc")
             (substitute* "configure"
               (("doc/rfc/Makefile") ""))
             (substitute* "doc/Makefile.in"
               (("rfc ") ""))))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (list "--disable-static"
               "--with-dovecot-install-dirs=no"
               (string-append "--with-dovecot="
                              (assoc-ref %build-inputs "dovecot")
                              "/lib/dovecot")
               (string-append "--docdir="
                              (assoc-ref %outputs "out")
                              "/share/doc/" ,name "-" ,version)
               (string-append "--with-moduledir="
                              (assoc-ref %outputs "out")
                              "/lib/dovecot"))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-file-names
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (libexec (string-append out "/libexec/dovecot")))
                 (substitute* "src/managesieve/managesieve-settings.c"
                   (("\\.executable = \"managesieve\"")
                    (string-append ".executable = \"" libexec
                                   "/managesieve\"")))
                 (substitute* "src/managesieve-login/managesieve-login-settings.c"
                   (("\\.executable = \"managesieve-login\"")
                    (string-append ".executable = \"" libexec
                                   "/managesieve-login\"")))))))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list dovecot))
      (home-page "https://pigeonhole.dovecot.org")
      (synopsis "Dovecot Sieve mail filtering plug-in and ManageSieve service")
      (description
       "Pigeonhole adds support for the Sieve language (RFC 5228) and the
ManageSieve protocol (RFC 5804) to the Dovecot e-mail server.

@dfn{Sieve} is a language for filtering incoming mail.  Messages can be
forwarded or sorted into separate folders.  Unwanted messages can be rejected
or discarded, and, when the user is not available, the Sieve interpreter can
send an automated reply.

Sieve is meant to be simple, extensible, and system-independent.  The
intention is to make it impossible to write anything more complex (and
dangerous) than simple mail filters.  Unlike most other mail filtering script
languages, Sieve does not allow users to execute arbitrary programmes.

Through the @dfn{ManageSieve} protocol, users can remotely manage their Sieve
scripts without needing file system access.  The server accepts only valid
scripts to prevent embarrassing errors later on.")
      (license license:lgpl2.1))))

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
         "0rkk10b1bsjz979sc864vpgcdchy7yxwmyv4ik50lar1h6awdnrf"))
       (patches
        (search-patches "dovecot-trees-support-dovecot-2.3.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf libtool dovecot pkg-config))
    (inputs
     (list libsodium))
    (arguments
     `(#:tests? #f ;No tests exist.
       #:configure-flags (list (string-append "--with-dovecot="
                                              (assoc-ref %build-inputs "dovecot")
                                              "/lib/dovecot"))))
    (home-page "https://0xacab.org/riseuplabs/trees")
    (synopsis "NaCL-based Dovecot email storage encryption plugin")
    (description
     "Technology for Resting Email Encrypted Storage (TREES) is a NaCL-based
Dovecot encryption plugin.  This plugin adds individually encrypted mail
storage to the Dovecot IMAP server.  It is inspired by Posteo's scrambler
which uses OpenSSL and RSA key pairs.  TREES works in a similar way, but uses
the Sodium crypto library (based on NaCL).

How it works:
@enumerate
@item On IMAP log in, the user's cleartext password is passed to the plugin.
@item The plugin creates an argon2 digest from the password.
@item This password digest is used as a symmetric secret to decrypt a libsodium secretbox.
@item Inside the secretbox is stored a Curve25519 private key.
@item The Curve25519 private key is used to decrypt each individual message,
using libsodium sealed boxes.
@item New mail is encrypted as it arrives using the Curve25519 public key.
@end enumerate\n")
    (license license:agpl3)))

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
       (list automake autoconf libtool dovecot pkg-config))
      (inputs
       (list libsodium))
      (arguments
       `(#:tests? #f ;No tests exist.
         #:configure-flags (list (string-append "--with-dovecot="
                                                (assoc-ref %build-inputs "dovecot")
                                                "/lib/dovecot"))))
      (home-page "https://github.com/LuckyFellow/dovecot-libsodium-plugin")
      (synopsis "Libsodium password hashing schemes plugin for Dovecot")
      (description
       "@code{dovecot-libsodium-plugin} provides a libsodium password
hashing scheme (such as scrypt) plug-in for @code{Dovecot}.")
      (license license:gpl3+))))

(define-public isync
  (package
    (name "isync")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/isync/isync/"
                           version "/isync-" version ".tar.gz"))
       (sha256 (base32
                "1yi2pdjq4iyj9dpgvxwn7c09jxrri8bwggq7adpmpainh0l91k18"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'substitute-openssl-path
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (substitute* (string-append #$output "/bin/mbsync-get-cert")
                 (("openssl s_client")
                  (string-append (search-input-file inputs "/bin/openssl")
                                 " s_client"))))))))
    (native-inputs
     (list perl))
    (inputs
     (list bdb cyrus-sasl openssl zlib))
    (home-page "https://isync.sourceforge.io/")
    (synopsis "Mailbox synchronization program")
    (description
     "isync/mbsync is a command-line tool for two-way synchronization of
mailboxes.  Currently Maildir and IMAP are supported types.")
    (license license:gpl2+)))

(define-public perl-email-abstract
  (package
    (name "perl-email-abstract")
    (version "3.009")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Abstract-" version ".tar.gz"))
       (sha256
        (base32 "1z01wbflg49nbgzl81x260cp8x6qr7xdpz3dkrg82m1fwa9742q4"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-email-simple perl-module-pluggable perl-mro-compat))
    (home-page "https://metacpan.org/release/Email-Abstract")
    (synopsis "Interface to mail representations")
    (description "Email::Abstract provides module writers with the ability to
write simple, representation-independent mail handling code.")
    (license license:perl-license)))

(define-public perl-email-address
  (package
    (name "perl-email-address")
    (version "1.912")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Address-" version ".tar.gz"))
       (sha256
        (base32 "1vzr0vx4zsw4zbc9xdffc31wnkc1raqmyfiyws06fbyck197i8qg"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Email-Address")
    (synopsis "Email address parsing and creation")
    (description "Email::Address implements a regex-based RFC 2822 parser that
locates email addresses in strings and returns a list of Email::Address
objects found.  Alternatively you may construct objects manually.")
    (license license:perl-license)))

(define-public perl-email-address-xs
  (package
    (name "perl-email-address-xs")
    (version "1.04")
    (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://cpan/authors/id/P/PA/PALI/"
                          "Email-Address-XS-" version ".tar.gz"))
      (sha256
       (base32
        "0gjrrl81z3sfwavgx5kwjd87gj44mlnbbqsm3dgdv1xllw26spwr"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Email-Address-XS")
    (synopsis "Parse and format RFC 5322 email addresses and groups")
    (description
     "Email::Address::XS implements RFC 5322 parser and formatter of email
addresses and groups.  Unlike Email::Address, this module does not use regular
expressions for parsing but instead is implemented in XS and uses shared code
from Dovecot IMAP server.")
    (license license:perl-license)))

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
    (home-page "https://metacpan.org/release/Email-Date-Format")
    (synopsis "Produce RFC 2822 date strings")
    (description "Email::Date::Format provides a means for generating an RFC
2822 compliant datetime string.")
    (license license:perl-license)))

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
    (home-page "https://metacpan.org/release/Email-MessageID")
    (synopsis "Generate world unique message-ids")
    (description "Email::MessageID generates recommended message-ids to
identify a message uniquely.")
    (license license:perl-license)))

(define-public perl-email-mime
  (package
    (name "perl-email-mime")
    (version "1.946")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-" version ".tar.gz"))
       (sha256
        (base32
         "0z1k3i0lzp2k421gc8f3wq0jbqflkbw2xqd2k7n7pmv56417kvk8"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-email-address
           perl-email-messageid
           perl-email-mime-contenttype
           perl-email-mime-encodings
           perl-email-simple
           perl-mime-types
           perl-module-runtime))
    (home-page "https://metacpan.org/release/Email-MIME")
    (synopsis "MIME message handling")
    (description "Email::MIME is an extension of the Email::Simple module, to
handle MIME encoded messages.  It takes a message as a string, splits it up
into its constituent parts, and allows you access to various parts of the
message.  Headers are decoded from MIME encoding.")
    (license license:perl-license)))

(define-public perl-email-mime-contenttype
  (package
    (name "perl-email-mime-contenttype")
    (version "1.022")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-MIME-ContentType-" version ".tar.gz"))
       (sha256
        (base32
         "042kxhs3bp1ab9z0mbr1wy21ld4lxd6v2a2mmrashqnsn2075fws"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-capture-tiny))
    (home-page "https://metacpan.org/release/Email-MIME-ContentType")
    (synopsis "Parse MIME Content-Type headers")
    (description "Email::MIME::ContentType parses a MIME Content-Type
header.")
    (license license:perl-license)))

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
     (list perl-capture-tiny))
    (home-page "https://metacpan.org/release/Email-MIME-Encodings")
    (synopsis "Unified interface to MIME encoding and decoding")
    (description "This module wraps MIME::Base64 and MIME::QuotedPrint.")
    (license license:perl-license)))

(define-public perl-email-sender
  (package
    (name "perl-email-sender")
    (version "1.300035")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Sender-" version ".tar.gz"))
       (sha256
        (base32 "0yfssp3rqdx1dmgvnygarzgkpkhqm28r5sd0gh87ksk8yxndhjql"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-capture-tiny))
    (propagated-inputs
     (list perl-email-abstract
           perl-email-address
           perl-email-simple
           perl-list-moreutils
           perl-module-runtime
           perl-moo
           perl-moox-types-mooselike
           perl-sub-exporter
           perl-throwable
           perl-try-tiny))
    (home-page "https://metacpan.org/release/Email-Sender")
    (synopsis "Perl library for sending email")
    (description "Email::Sender replaces the old and sometimes problematic
Email::Send library.")
    (license license:perl-license)))

(define-public perl-email-simple
  (package
    (name "perl-email-simple")
    (version "2.216")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/R/RJ/RJBS/"
                           "Email-Simple-" version ".tar.gz"))
       (sha256
        (base32
         "1m4brbjvalyp5kjqslqv4155dzwg977shxin208i7lc8236n6pyq"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-email-date-format))
    (home-page "https://metacpan.org/release/Email-Simple")
    (synopsis "Parsing of RFC 2822 messages")
    (description "Email::Simple provides simple parsing of RFC 2822 message
format and headers.")
    (license license:perl-license)))

(define-public libesmtp
  (package
    (name "libesmtp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libesmtp/libESMTP")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bhh8hlsl9597x0bnfl563k2c09b61qnkb9mfyqcmzlq63m1zw5y"))))
    (build-system meson-build-system)
    (propagated-inputs
     (list openssl))
    (home-page "http://www.stafford.uklinux.net/libesmtp/")
    (synopsis "Library for sending mail via remote hosts using SMTP")
    (description
     "libESMTP is an @acronym{SMTP, Simple Mail Transfer Protocol} client that
manages posting (or submission of) electronic mail via a preconfigured
@acronym{MTA, Mail Transport Agent}.

It may be used as part of a @acronym{MUA, Mail User Agent}, or other program
that must be able to post electronic mail where mail functionality may not be
that program's primary purpose.

libESMTP's high-level API shields developers from the complexity of SMTP.  It
transparently handles many SMTP extensions including authentication,
@acronym{TLS, Transport-Level Security}, and PIPELINING for performance.  Even
without a pipelining server, libESMTP offers much better performance than would
be expected from a simple client.")
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public esmtp
  (package
    (name "esmtp")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/andywingo/esmtp")
             (commit "01bf9fc")))
       (sha256
        (base32
         "1ay282rrl92h0m0m8z5zzjnwiiagi7c78aq2qvhia5mw7prwfyw2"))
       (file-name (string-append name "-" version "-checkout"))
       (patches (search-patches "esmtp-add-lesmtp.patch"))))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'bootstrap
                   (lambda _ (invoke "autoreconf" "-vfi"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison flex autoconf automake libtool))
    (inputs
     (list libesmtp))
    (home-page "https://sourceforge.net/projects/esmtp/")
    (synopsis "Relay-only mail transfer agent (MTA)")
    (description "Esmtp is a simple relay-only mail transfer agent built using
libESMTP.  It sends e-mail via a remote SMTP server using credentials from the
user's @file{$HOME/.esmtprc} configuration file; see the @command{esmtprc} man
page for more on configuration.  This package also provides minimal
compatibility shims for the @command{sendmail}, @command{mailq}, and
@command{newaliases} commands.")
    (license license:gpl2+)))

(define-public fdm
  (package
    (name "fdm")
    (version "2.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://github.com/nicm/fdm/releases/download/"
                                 version "/fdm-" version ".tar.gz"))
             (sha256
               (base32 "05kczdk44cbk3rg77rwgp47hw75al6b09wlv3cff4d4qh8bx3ajk"))))
    (build-system gnu-build-system)
    (inputs
     (list tdb openssl zlib))
    (home-page "https://github.com/nicm/fdm")
    (synopsis
     "@acronym{MRA, Mail Retrieval Agent} and @acronym{MDA, Mail Delivery Agent}")
    (description "fdm fetches and delivers mail in various ways.

Mail may be fetched from IMAP or POP3 servers, from local maildirs, or read
from standard input.  It is then filtered based on regular expressions, its
size or age, or the output of a (shell) command.  It can be rewritten by an
external process, dropped, left on the server or delivered into maildirs,
mboxes, to a file or pipe, or any combination.

fdm is primarily designed for use by a single user, but can use privilege
separation to safely deliver mail in multi-user setups.")
    (license
     ;; Why point to a source file?  Well, all the individual files have a
     ;; copy of this license in their headers, but there's no seprate file
     ;; with that information.
     (license:non-copyleft
      "https://github.com/nicm/fdm/blob/master/command.c"))))


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
                                "procmail-CVE-2014-3618.patch"
                                "procmail-CVE-2017-16844.patch"))))
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
    (inputs (list exim))
    (home-page "https://www.procmail.org/")
    (synopsis "Versatile mail delivery agent (MDA)")
    (description "Procmail is a mail delivery agent (MDA) featuring support
for a variety of mailbox formats such as mbox, mh and maildir.  Incoming mail
can be sorted into separate files/directories and arbitrary commands can be
executed on mail arrival.  Procmail is considered stable, but is no longer
maintained.")
    (license license:gpl2+))) ;; procmail allows choosing the
                              ;; nonfree Artistic License 1.0
                              ;; as alternative to the GPL2+.
                              ;; This option is not listed here.

(define-public khard
  (package
    (name "khard")
    (version "0.19.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri name version))
              (sha256
               (base32
                "1464j728hjjpzlc89v4rbml3p4b38zp1igjd9yq3xnn3lc6hmwsr"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (zsh (string-append out "/share/zsh/site-functions")))
               (copy-recursively "misc/zsh" zsh)))))))
    (native-inputs
     (list python-setuptools-scm))
    (inputs
     (list python-atomicwrites python-configobj python-ruamel.yaml
           python-unidecode python-vobject))
    (synopsis "Console address book using CardDAV")
    (description "Khard is an address book for the console.  It creates, reads,
modifies and removes CardDAV address book entries at your local machine.  For
synchronizing with a remote address book, @command{vdirsyncer} is recommended.
Khard can also be used from within the email client @command{mutt}.")
    (home-page "https://github.com/scheibler/khard")
    (license license:gpl3+)))

(define-public perl-mail-spf
  (package
    (name "perl-mail-spf")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/J/JM/JMEHNLE/mail-spf/Mail-SPF-v"
             version
             ".tar.gz"))
       (sha256
        (base32 "0qk1rfgfm5drj4iyniiabrasrpqv570vzhgz66lwgb67y4amkjv1"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-net-dns-resolver-programmable))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'modify-Build.PL
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Build.PL"
               (("'/usr/sbin'") (string-append "'"
                                               (assoc-ref outputs "out")
                                               "/sbin'")))
             #t)))))
    (inputs
     (list perl-error perl-net-dns perl-netaddr-ip perl-uri))
    (home-page "https://metacpan.org/release/Mail-SPF")
    (synopsis "Perl implementation of Sender Policy Framework")
    (description "Mail::SPF is the Sender Policy Framework implemented
in Perl.")
    (license license:bsd-3)))

(define-public perl-mail-authenticationresults
  (package
    (name "perl-mail-authenticationresults")
    (version "1.20180923")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "mirror://cpan/authors/id/M/MB/MBRADSHAW/"
                     "Mail-AuthenticationResults-" version ".tar.gz"))
              (sha256
               (base32
                "1g1wym9vcbhldwvi4w5pl0fhd4jh2icj975awf4wr5xmkli9mxbz"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-exception))
    (home-page "https://metacpan.org/release/Mail-AuthenticationResults")
    (synopsis "Object Oriented Authentication-Results Headers")
    (description "Mail::AuthenticationResults parses the message header field
that indicates the message authentication status as per RFC7601.  This module
is not fully compliant with the RFC but it tries to implement most styles of
Authentication-Results header seen in the wild.")
    (license license:perl-license)))

(define-public perl-mail-dkim
  (package
    (name "perl-mail-dkim")
    (version "1.20230630")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "mirror://cpan/authors/id/M/MB/MBRADSHAW/Mail-DKIM-"
                     version
                     ".tar.gz"))
              (sha256
               (base32
                "1m6ka1smkcmv682pgqh7npg4fzdfcn1654bs068sqhqgl29rm80g"))))
    (build-system perl-build-system)
    (propagated-inputs
     (list perl-crypt-openssl-rsa
           perl-cryptx
           perl-mail-authenticationresults
           perl-mailtools
           perl-net-dns))
    (native-inputs
     (list perl-net-dns-resolver-mock
           perl-test-requiresinternet
           perl-yaml-libyaml))
    (home-page "https://metacpan.org/release/Mail-DKIM")
    (synopsis "Signs/verifies Internet mail with DKIM/DomainKey signatures")
    (description "Mail::DKIM is a Perl module that implements the @acronym{DKIM,
Domain Keys Identified Mail} standard, and the older Yahoo! DomainKeys standard,
both of which sign and verify emails using digital signatures and DNS records.
Mail-DKIM can be used by any Perl program that wants to provide support for
DKIM and/or DomainKeys.")
    (license license:gpl3+)))

(define-public dkimproxy
  (package
    (name "dkimproxy")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "mirror://sourceforge/dkimproxy/dkimproxy/"
                     version "/dkimproxy-" version ".tar.gz"))
              (sha256
               (base32
                "1gc5c7lg2qrlck7b0lvjfqr824ch6jkrzkpsn0gjvlzg7hfmld75"))
              (patches
               (search-patches "dkimproxy-add-ipv6-support.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'make-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (wrap.pl (lambda (scripts keys)
                               (for-each
                                (lambda (script)
                                  (wrap-program (string-append out script)
                                    `("PERL5LIB" ":" prefix
                                      ,(map (λ (input)
                                              (string-append
                                               (assoc-ref inputs input)
                                               "/lib/perl5/site_perl"))
                                            keys))))
                                scripts))))
               (wrap.pl (list "/bin/dkimproxy.in"
                              "/bin/dkimproxy.out")
                        (list "perl-crypt-openssl-rsa"
                              "perl-cryptx"
                              "perl-io-socket-inet6"
                              "perl-mailtools"
                              "perl-mail-authenticationresults"
                              "perl-mail-dkim"
                              "perl-net-dns"
                              "perl-net-server"
                              "perl-socket6"))
               (wrap.pl (list "/bin/dkim_responder.pl")
                        (list "perl-crypt-openssl-rsa"
                              "perl-cryptx"
                              "perl-mail-dkim"
                              "perl-mailtools"
                              "perl-mime-tools"
                              "perl-net-dns"
                              "perl-timedate"))))))))
    (inputs
     (list bash-minimal
           perl
           perl-crypt-openssl-rsa
           perl-cryptx
           perl-io-socket-inet6
           perl-mailtools
           perl-mail-authenticationresults
           perl-mail-dkim
           perl-mime-tools
           perl-net-dns
           perl-net-server
           perl-socket6
           perl-timedate))
    (home-page "https://dkimproxy.sourceforge.net")
    (synopsis "SMTP proxy to sign and verify Internet mail with DKIM headers")
    (description
     "DKIMproxy is an SMTP proxy that signs and verifies Internet mail using the
@code{Mail::DKIM} Perl module.  It comprises two separate proxies: an outbound
proxy for signing outgoing email, and an inbound proxy for verifying signatures
of incoming messages.

It was designed for Postfix, but can be used to add DKIM support to nearly any
existing mail server.  With Postfix, the proxies can operate as either
@code{Before-Queue} or @code{After-Queue} content filters.")
    (license license:gpl2+)))

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
             (invoke (string-append gzip "/bin/gzip") "-d" "mb2md.gz")
             (substitute* "mb2md"
               (("#!/usr/bin/perl")
                (string-append "#!/usr/bin/perl -I " perl5lib)))
             (patch-shebang "mb2md" (list (string-append perl "/bin")))
             (chmod "mb2md" #o555))
           #t))))
    (native-inputs (list gzip))
    (inputs (list perl perl-timedate))
    (home-page "http://batleth.sapienti-sat.org/projects/mb2md/")
    (synopsis "Mbox to maildir converter")
    (description
     "Mb2md is a Perl script that takes one or more mbox format files and
converts them to maildir format directories.")
    (license license:public-domain)))

(define-public mblaze
  (package
    (name "mblaze")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leahneukirchen/mblaze")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fa8s9dp5ilwmfcwkx72x2b5i0maa5sl97hv2cdknqmc27gv0b1c"))))
    (outputs '("out" "contrib"))
    (build-system gnu-build-system)
    (inputs (list bash-minimal
                  coreutils
                  gawk
                  glibc
                  gnupg
                  ncurses
                  openssl
                  ruby
                  sed))
    (native-inputs (list perl))
    (arguments
     (list
      #:make-flags
      #~(list #$(string-append "CC=" (cc-for-target))
              "PREFIX="
              (string-append "DESTDIR=" #$output))
      #:modules '((ice-9 ftw)
                  (guix build utils)
                  (guix build gnu-build-system))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'install 'install-contrib
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (contrib (assoc-ref outputs "contrib"))
                     (contrib-bin (string-append contrib "/bin"))
                     (exe? (lambda (file)
                             (let ((s (stat file)))
                               (and (eq? 'regular (stat:type s))
                                    (logtest #o100 (stat:perms s)))))))
                (mkdir-p contrib-bin)
                (with-directory-excursion "contrib"
                  (for-each
                    (lambda (prog)
                      (install-file prog contrib-bin)
                      (wrap-program (string-append contrib-bin "/" prog)
                       `("PATH" =
                         (,contrib-bin
                          ,(string-append out "/bin")
                          ,(string-append (assoc-ref inputs "coreutils") "/bin")
                          ,(string-append (assoc-ref inputs "gawk") "/bin")
                          ,(string-append (assoc-ref inputs "glibc") "/bin")
                          ,(string-append (assoc-ref inputs "ncurses") "/bin")
                          ,(string-append (assoc-ref inputs "openssl") "/bin")
                          ,(string-append (assoc-ref inputs "sed") "/bin")))))
                    (scandir "." exe?)))))))))
    (home-page "https://github.com/leahneukirchen/mblaze")
    (synopsis "Unix utilities to deal with Maildir")
    (description
     "The mblaze message system is a set of Unix utilities for processing and
interacting with mail messages which are stored in maildir folders.

Its design is roughly inspired by MH, the RAND Message Handling System, but it
is a complete implementation from scratch.

mblaze is a classic command line MUA and has no features for receiving or
transferring messages; you can operate on messages in a local maildir spool,
or fetch your messages using fdm(1), getmail(1), offlineimap(1), or similar
utilities, and send it using dma(8), msmtp(1), sendmail(8), as provided by
OpenSMTPD, Postfix, or similar.

mblaze operates directly on maildir folders and doesn't use its own caches or
databases.  There is no setup needed for many uses.  All utilities have been
written with performance in mind.  Enumeration of all messages in a maildir is
avoided unless necessary, and then optimized to limit syscalls.  Parsing
message metadata is optimized to limit I/O requests.  Initial operations on a
large maildir may feel slow, but as soon as they are in the file system cache,
everything is blazingly fast.  The utilities are written to be memory
efficient (i.e. not wasteful), but whole messages are assumed to fit into RAM
easily (one at a time).")
    (license (list license:public-domain
                   license:expat))))    ; mystrverscmp.c and mymemmem

(define-public mpop
  (package
    (name "mpop")
    (version "1.4.20")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://marlam.de/mpop/releases/"
                           "mpop-" version ".tar.xz"))
       (sha256
        (base32 "0jxz6la8h8zlszflyxmkq4xai0lcix468d468d3wmnixgvhpvk1m"))))
    (build-system gnu-build-system)
    (inputs
     (list gnutls))
    (native-inputs
     (list pkg-config))
    (home-page "https://marlam.de/mpop/")
    (properties
     '((release-monitoring-url . "https://marlam.de/mpop/download/")))
    (synopsis "POP3 mail client")
    (description "mpop is a small and fast POP3 client suitable as a
fetchmail replacement.

mpop supports multiple accounts, header based mail filtering, delivery
to mbox files, maildir folders or an @acronym{MDA, Mail Delivery Agent},
TLS/SSL, several authentication methods, @acronym{IDN, Internationalized Domain
Names} and SOCKS proxies.")
    (license license:gpl3+)))

(define-public mhonarc
  (package
    (name "mhonarc")
    (version "2.6.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/L/LD/LDIDRY/MHonArc-"
                           version ".tar.gz"))
       (sha256
        (base32 "0cszh619i8bfjpyxhfgph20v8lic5zpirr990xdbg7759qvwfza5"))))
    (build-system perl-build-system)
    (home-page "https://www.mhonarc.org/")
    (synopsis "Create HTML archives of mail/news messages")
    (description
     "MHonArc is a Perl mail-to-HTML converter.  MHonArc
provides HTML mail archiving with index, mail thread linking,
etc; plus other capabilities including support for MIME and
powerful user customization features.")
    (license license:gpl2+)))

(define-public sendmail
  (package
    (name "sendmail")
    (version "8.18.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.sendmail.org/pub/sendmail/sendmail."
             version ".tar.gz"))
       (sha256
        (base32 "0w07iw4imp9wvczd2mijns7zxl8p1wk29b9yrzvhcj4fqc4z7wfb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-build-timestamps
           ;; Avoid embedding timestamps for reproducible build
           (lambda _
             (substitute* '("devtools/bin/configure.sh"
                            "cf/sh/makeinfo.sh")
               (("on `date`") ""))))
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
               (("SHELL=/bin/sh") (string-append "SHELL=" (which "sh"))))))
         (add-before 'build 'replace-/usr
           (lambda _
             (substitute*
                 '("devtools/OS/Linux"
                   "cf/ostype/mklinux.m4"
                   "cf/ostype/linux.m4")
               (("/usr/sbin") "/sbin"))))
         (replace 'configure
           (lambda _

             ;; Render harmless any attempts to chown or chgrp
             (substitute* "devtools/bin/install.sh"
               (("owner=\\$2") "owner=''")
               (("group=\\$2") "group=''"))

             (with-output-to-file "devtools/Site/site.config.m4"
               (lambda ()
                 (format #t "
define(`confEBINDIR', `/sbin')
define(`confSBINDIR', `/sbin')
define(`confMBINDIR', `/sbin')
define(`confUBINDIR', `/bin')
define(`confLINKS', `')
define(`confCC', `gcc')
define(`confOPTIMIZE', `-g -O2')
define(`confLIBS', `-lresolv')
define(`confINSTALL', `~a/devtools/bin/install.sh')
define(`confDEPEND_TYPE', `CC-M')
define(`confINST_DEP', `')
" (getcwd))))))
         (replace 'build
           (lambda _
             (invoke "sh" "Build")
             (with-directory-excursion "cf/cf"
               (copy-file "generic-linux.mc" "sendmail.mc")
               (invoke "sh" "Build" "sendmail.cf"))))
         (add-before 'install 'pre-install
           (lambda _
             (let ((out (assoc-ref %outputs "out")))
               (mkdir-p (string-append out "/bin"))
               (mkdir-p (string-append out "/sbin"))
               (mkdir-p (string-append out "/etc/mail"))
               (setenv "DESTDIR" out)
               (with-directory-excursion "cf/cf"
                 (invoke "sh" "Build" "install-cf")))))
         (add-after 'install 'post-install
           (lambda _
             ;; Make symbolic links manually, because build script uses
             ;; absolute paths for them and ignores DESTDIR.
             (for-each
              (lambda (name)
                (symlink "../sbin/sendmail" (string-append %output "/bin/" name)))
              '("hoststat" "newaliases" "mailq" "purgestat")))))
       ;; There is no make check.  There are some post installation tests, but those
       ;; require root privileges.
       #:tests? #f))
    (inputs (list m4 perl))
    (home-page "https://sendmail.org")
    (synopsis "Highly configurable Mail Transfer Agent (MTA)")
    (description
     "Sendmail is a mail transfer agent (MTA) originally developed by Eric
Allman.  It is highly configurable and supports many delivery methods and many
transfer protocols.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public sieve-connect
  (package
    (name "sieve-connect")
    (version "0.90")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://people.spodhuis.org/phil.pennock/software/"
                           "sieve-connect-" version ".tar.bz2"))
       (sha256
        (base32 "00vnyzr67yr2ilnprbd388gfnwmrmbdx1jsig9d0n5q902jqn62a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'create-output-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (subdirectory)
                           (mkdir-p (string-append out "/" subdirectory)))
                         (list "bin"
                               "man/man1"))
               #t)))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (path (getenv "PERL5LIB")))
               (wrap-script (string-append out "/bin/sieve-connect")
                 #:guile (search-input-file inputs "bin/guile")
                 `("PERL5LIB" ":" = (,path)))
               #t))))))
    (inputs
     `(("guile" ,guile-3.0)             ; for wrap-script
       ("perl" ,perl)
       ("perl-authen-sasl" ,perl-authen-sasl)
       ("perl-io-socket-inet6" ,perl-io-socket-inet6)
       ("perl-io-socket-ssl" ,perl-io-socket-ssl)
       ("perl-net-dns" ,perl-net-dns)
       ("perl-socket6" ,perl-socket6)
       ("perl-term-readkey" ,perl-term-readkey)
       ("perl-term-readline" ,perl-term-readline-gnu)))
    (home-page
     "https://people.spodhuis.org/phil.pennock/software/#sieve-connect")
    (synopsis "ManageSieve client for managing Sieve e-mail filters")
    (description
     "Sieve-connect lets you view, upload, edit, delete, and otherwise manage
Sieve scripts on any mail server that speaks the @dfn{ManageSieve} protocol,
as specified in RFC 5804.

@dfn{Sieve} (RFC 5228) is a specialised language for e-mail filtering.  Sieve
scripts are stored on the server and run whenever mail arrives.  They can
automatically sort new messages into folders, silently reject them, send an
automated response, and more.

@command{sieve-connect} is designed to be both a tool which can be invoked
from scripts as well as a decent interactive client.  It supports TLS for
connection privacy, as well as authentication with SASL or GSSAPI client
certificates.  It should be a drop-in replacement for @command{sieveshell}
from the Cyrus IMAP project.")
      (license license:bsd-3)))

(define-public opensmtpd
  (package
    (name "opensmtpd")
    (version "7.6.0p1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.opensmtpd.org/archives/"
                           "opensmtpd-" version ".tar.gz"))
       (sha256
        (base32 "15sa1vzh6rbl0c8fwl4kz5zrlarp8mxaw47q6wk3lrd6h9lq0z5j"))))
    (build-system gnu-build-system)
    (inputs
     ;; OpenSMTPd bundled (a subset of) libasr and libtls, which we use.  See
     ;; https://www.mail-archive.com/misc@opensmtpd.org/msg05909.html for why.
     (list bash-minimal    ;sh invoked at run time
           bdb
           coreutils       ;for cat
           gzip            ;for zcat
           libbsd          ;https://github.com/OpenSMTPD/OpenSMTPD/issues/1233
           libevent
           libressl
           linux-pam
           libxcrypt
           zlib))
    (native-inputs
     (list autoconf
           automake
           bison
           groff                        ;for man pages
           pkg-config))
    (arguments
     (list
      #:configure-flags
      #~(list "--localstatedir=/var"
              ;; Allow work with /etc/mailname.
              "--sysconfdir=/etc"
              "--with-libbsd"
              ;; This is the default only if it exists at build time—it doesn't.
              "--with-path-socket=/var/run"
              "--with-path-CAfile=/etc/ssl/certs/ca-certificates.crt"
              "--with-user-smtpd=smtpd"
              "--with-user-queue=smtpq" "--with-group-queue=smtpq"
              "--with-auth-pam"
              "--with-table-db"

              ;; This is called at run time but defaults to the native zcat in
              ;; $PATH, breaking cross-compilation.
              (string-append "ac_cv_path_ZCAT="
                             #$(this-package-input "gzip") "/bin/zcat"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Fix some incorrectly hard-coded external tool file names.
          (add-after 'unpack 'patch-FHS-file-names
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "mk/pathnames"
                ;; avoids warning smtpd: couldn't enqueue offline message
                ;; smtpctl exited abnormally
                (("(-DPATH_SMTPCTL=).*\\\\" all def)
                 (string-append def "\\\"/run/privileged/bin/smtpctl\\\" \\"))
                (("(-DPATH_MAKEMAP=).*\\\\" all def)
                 (string-append def "\\\"/run/privileged/bin/makemap\\\" \\")))
              (substitute* "usr.sbin/smtpd/smtpctl.c"
                ;; ‘gzcat’ is auto-detected at compile time, but ‘cat’ isn't.
                (("/bin/cat" file) (search-input-file inputs file)))
              (substitute* "usr.sbin/smtpd/mda_unpriv.c"
                (("/bin/sh" file) (search-input-file inputs file)))))
          ;; Avoid install smtpd.conf to /etc.
          (add-after 'unpack 'fix-smtpd.conf-install-path
            (lambda _
              (let ((etc (string-append #$output "/etc")))
                (mkdir-p etc)
                (substitute* "mk/smtpd/Makefile.am"
                  (("\\$\\(DESTDIR\\)\\$\\(sysconfdir\\)/smtpd\\.conf")
                   (string-append etc "/smtpd.conf"))))))
          ;; OpenSMTPD provides a single smtpctl utility to control both the
          ;; daemon and the local submission subsystem.  To accomodate systems
          ;; that require historical interfaces such as sendmail, newaliases or
          ;; makemap, smtpctl operates in compatibility mode if called with the
          ;; historical name.
          (add-after 'install 'install-compability-links
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out  (assoc-ref outputs "out"))
                     (sbin (string-append out "/sbin/")))
                (for-each (lambda (command)
                            (symlink "smtpctl" (string-append sbin command)))
                          (list "mailq" "makemap" "newaliases"
                                "send-mail" "sendmail"))))))))
    (synopsis "Lightweight SMTP daemon")
    (description
     "OpenSMTPD is an implementation of server-side @acronym{SMTP, Simple Mail
Transfer Protocol}, with some additional standard extensions.  It allows
ordinary machines to exchange e-mails with other systems speaking the SMTP
protocol, or to deliver them to local users.

In order to simplify the use of SMTP, OpenSMTPD implements a smaller set of
functionality than those available in other SMTP daemons.  The objective is to
provide enough features to satisfy typical usage at the risk of unsuitability
to esoteric or niche requirements.")
    (home-page "https://www.opensmtpd.org")
    (license (list license:bsd-2 license:bsd-3 license:bsd-4
                   (license:non-copyleft "file://COPYING")
                   license:public-domain license:isc license:openssl))))

(define-public opensmtpd-extras
  (package
    (name "opensmtpd-extras")
    (version "6.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.opensmtpd.org/archives/"
                                  "opensmtpd-extras-" version ".tar.gz"))
              (sha256
               (base32
                "1b1mx71bvmv92lbm08wr2p60g3qhikvv3n15zsr6dcwbk9aqahzq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     `(("libressl" ,libressl)
       ("libevent" ,libevent)
       ("libxcrypt" ,libxcrypt)         ;required by Python.h
       ("mysql" ,mariadb "dev")
       ("opensmtpd" ,opensmtpd)
       ("postgresql" ,postgresql)
       ("python" ,python-2)
       ("sqlite" ,sqlite)))
    (arguments
     `(#:configure-flags
       (list "--sysconfdir=/etc"
             "--localstatedir=/var"

             "--with-queue-null"
             "--with-queue-python"
             "--with-queue-ram"
             "--with-queue-stub"

             "--with-table-ldap"
             "--with-table-mysql"
             "--with-table-postgres"
             ;; "--with-table-redis"    ; TODO: package hiredis
             "--with-table-socketmap"
             "--with-table-passwd"
             "--with-table-python"
             "--with-table-sqlite"
             "--with-table-stub"

             "--with-scheduler-ram"
             "--with-scheduler-stub"
             "--with-scheduler-python"

             "--with-user-smtpd=smtpd"

             ;; We have to configure it like this because the default checks for
             ;; for example Python in /usr/{,local/}bin and fails otherwise.
             (string-append "--with-python="
                            (assoc-ref %build-inputs "python")))))
    (home-page "https://www.opensmtpd.org")
    (synopsis "Extra tables, filters, and various other addons for OpenSMTPD")
    (description
     "This package provides extra tables, filters, and various other addons
for OpenSMTPD to extend its functionality.")
    (license (list license:bsd-2 license:bsd-3 ; openbsd-compat
                   license:isc))))             ; everything else

(define-public libopensmtpd
  ;; Private source dependency of opensmtpd-filter-dkimsign (by the same
  ;; author), until any project actually uses it in its compiled form.
  (package
    (name "libopensmtpd")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://imperialat.at/releases/"
                                 "libopensmtpd-" version ".tar.gz")
                  (string-append "https://distfiles.sigtrap.nl/"
                                 "libopensmtpd-" version ".tar.gz")))
       (sha256
        (base32 "04x610mvwba7m0n9h0wbnsw58rb4khq44fm4blkgjqvh3bhxbmnd"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "-f" "Makefile.gnu"
             (string-append "CC=" ,(cc-for-target))
             (string-append "LOCALBASE=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'inherit-ownership
           (lambda _
             (substitute* "Makefile.gnu"
               (("-o \\$\\{...OWN\\} -g \\$\\{...GRP\\}") ""))))
         (delete 'configure))))         ; no configure script
    (native-inputs
     (list mandoc))           ; silently installs empty man page without
    (inputs
     (list libevent))
    (home-page "https://imperialat.at/dev/libopensmtpd/")
    (synopsis "OpenSMTPd filter C API")
    (description
     "The @code{osmtpd} API is an event-based C programming interface for
writing OpenSMTPd filters.")
    (license license:expat)))

(define-public opensmtpd-filter-dkimsign
  (package
    (name "opensmtpd-filter-dkimsign")
    (version "0.6")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://imperialat.at/releases/"
                                 "filter-dkimsign-" version ".tar.gz")
                  (string-append "https://distfiles.sigtrap.nl/"
                                 "filter-dkimsign-" version ".tar.gz")))
       (sha256
        (base32 "1hrn31hayr0hb32km5c42hhbaxw7g3jcgm59p0v0ydlj1fs0sprv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "-f" "Makefile.gnu"
             (string-append "CC=" ,(cc-for-target))
             "HAVE_ED25519=yep-but-is-openssl-only"
             (string-append "LOCALBASE=" (assoc-ref %outputs "out")))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile.gnu
           (lambda _
             (substitute* "Makefile.gnu"
               (("pkg-config") ,(pkg-config-for-target))
               (("-o \\$\\{...OWN\\} -g \\$\\{...GRP\\}") ""))))
         (delete 'configure))))         ; no configure script
    (native-inputs
     (list mandoc))           ; silently installs empty man page without
    (inputs
     (list libevent libopensmtpd
           ;; Our OpenSMTPd package uses libressl, but this package currently
           ;; supports HAVE_ED25519 only with openssl.
           openssl))
    (home-page "https://imperialat.at/dev/filter-dkimsign/")
    (synopsis "OpenSMTPd filter for signing mail with DKIM")
    (description
     "The @command{filter-dkimsign} OpenSMTPd filter signs outgoing e-mail
messages with @acronym{DKIM, DomainKeys Identified Mail} (RFC 4871).")
    (license license:expat)))

(define-public opensmtpd-filter-rspamd
  (package
    (name "opensmtpd-filter-rspamd")
    (version "0.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/poolpOrg/filter-rspamd")
                    (commit (string-append "v" version))))
              (sha256
               (base32 "1zjmxfr22gvrxy3nfngl66h1fsd1sv4kxdn91r8byqijy6p65pai"))
              (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/poolpOrg/filter-rspamd"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-bootstrap-variables
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Tell the build system where to install binaries
             (let* ((out (assoc-ref outputs "out"))
                    (libexec (string-append out "/libexec/opensmtpd")))
               (setenv "GOBIN" libexec)))))))
    (native-inputs
     (list opensmtpd))
    (home-page "https://github.com/poolpOrg/filter-rspamd")
    (synopsis "OpenSMTPd filter to request an Rspamd analysis")
    (description
     "The @command{filter-rspamd} OpenSMTPd filter implements the
Rspamd protocol and allows OpenSMTPd to request an Rspamd analysis of
an SMTP transaction before a message is committed to queue.")
    (license license:isc)))

(define-public mailman
  (package
    (name "mailman")
    (version "3.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mailman" version))
        (sha256
         (base32 "0a5ckbf8hc3y28b7p5psp0d4bxk601jlr5pd3hhh545xd8d9f0dg"))))
    (build-system python-build-system)
    (propagated-inputs
     (list gunicorn
           python-aiosmtpd
           python-alembic
           python-atpublic
           python-authheaders
           python-authres
           python-click
           python-dateutil
           python-dnspython
           python-falcon
           python-flufl-bounce
           python-flufl-i18n
           python-flufl-lock
           python-importlib-resources
           python-lazr-config
           python-passlib
           python-requests
           python-sqlalchemy
           python-zope-component
           python-zope-configuration
           python-zope-event
           python-zope-interface))
    (native-inputs
     (list python-nose))
    (home-page "https://www.list.org")
    (synopsis "Mailing list manager")
    (description
     "GNU Mailman is software for managing email discussion and mailing
lists.  Both users and administrators generally perform their actions in a
web interface, although email and command-line interfaces are also provided.
The system features built-in archiving, automatic bounce processing, content
filtering, digest delivery, and more.")
    (license license:gpl3+)))

(define-public python-mailmanclient
  (package
    (name "python-mailmanclient")
    (version "3.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mailmanclient" version))
       (sha256
        (base32
         "0ppqnv84v7npgjykhqdy5c38vanz4l0qw871kpsl27z4fm365zlj"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; Requires mailman running
    (propagated-inputs
     (list python-requests))
    ;(native-inputs
    ; `(("mailman" ,mailman)
    ;   ("python-falcon" ,python-falcon)
    ;   ("python-pytest" ,python-pytest)
    ;   ("python-pytest-services" ,python-pytest-services)))
    (home-page "https://www.list.org/")
    (synopsis "Python bindings for the Mailman 3 REST API")
    (description
     "The mailmanclient library provides official Python bindings for
the GNU Mailman 3 REST API.")
    (license license:lgpl3+)))


(define-public mlmmj
  (package
    (name "mlmmj")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://mlmmj.org/releases/mlmmj-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "0hpj10qad821ci11si8xc2qnmkzfn90y13s43fm4fca38f0qjp8w"))))
    (build-system gnu-build-system)
    (inputs
     (list perl)) ; For "contrib/web/"
    (native-inputs
     (list pkg-config))
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
    (version "1.3.15")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "django_mailman3" version))
        (sha256
         (base32
          "06yiqsqyvngq7ls24xlh6kwpq0x0y55mrgypc6xdbidrkhk6p4gr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; AttributeError: 'SocialLogin' object has no attribute 'account'
      #:test-flags #~(list "-k" "not test_social_account_added")))
    (native-inputs
     (list python-pdm-backend
           python-pytest
           python-pytest-django
           python-tzdata))
    (propagated-inputs
     (list python-django
           python-django-allauth
           python-django-gravatar2
           python-mailmanclient))
    (home-page "https://gitlab.com/mailman/django-mailman3")
    (synopsis "Django library to help interaction with Mailman")
    (description
     "This package contains libraries and templates for Django-based
interfaces interacting with Mailman.")
    (license license:gpl3+)))

(define-public python-mailman-hyperkitty
  (package
    (name "python-mailman-hyperkitty")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mailman-hyperkitty" version))
        (sha256
         (base32
          "1ni6vf1yi14c0l895fk278x4na7ymhpkl1q0vnpzbkzplpa7200i"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-requests python-zope-interface))
    (inputs
     (list mailman))
    (native-inputs
     (list python-mock python-nose python-nose2))
    (home-page "https://gitlab.com/mailman/mailman-hyperkitty/")
    (synopsis "Mailman archiver plugin for HyperKitty")
    (description
     "Mailman3 allows emails sent to its mailing lists to be archived by any
software provided that there is a plugin (loadable by Mailman3) designed to
communicate with it properly.  This module contains a Mailman3 archiver plugin
which sends emails to HyperKitty, the official Mailman3 web archiver.")
    (license license:gpl3+)))

(define-public python-hyperkitty
  (package
    (name "python-hyperkitty")
    (version "1.3.12")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hyperkitty" version))
        (sha256
         (base32
          "078nrxkwdrv4d7ysdzp1c2dl5nm4fvxnpn6mq6lrxg65gs9q5dfy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "example_project/manage.py" "test"
                       "--settings=hyperkitty.tests.settings_test"
                       "--pythonpath=.")))))))
    (propagated-inputs
     (list python-dateutil
           python-django
           python-django-compressor
           python-django-debug-toolbar
           python-django-extensions
           python-django-gravatar2
           python-django-haystack
           python-django-mailman3
           python-django-q2
           python-django-rest-framework
           python-elasticsearch
           python-flufl-lock
           python-isort
           python-lxml
           python-mailmanclient
           python-mistune
           python-networkx
           python-robot-detection
           python-tzdata
           python-whoosh))
    (native-inputs
     (list tzdata-for-tests
           python-beautifulsoup4
           python-pdm-backend
           python-whoosh))
    (home-page "https://gitlab.com/mailman/hyperkitty")
    (synopsis "Web interface to access GNU Mailman v3 archives")
    (description
     "The hyperkitty Django app provides a web user interface to access GNU
Mailman3 archives, and manage it.  This interface uses django, and requires
some configuration.")
    (license license:gpl3)))    ; Some files are gpl2+

(define-public postorius
  (package
    (name "postorius")
    (version "1.3.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "postorius" version))
       (sha256
        (base32
         "08hs2g2yfya869chi74xwhqkrq9l4dm1k5ddx14hv42j91ffybb0"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (if tests?
                 (invoke "python" "example_project/manage.py" "test"
                         "--settings=test_settings" "postorius")
                 #t))))
       #:tests? #f)) ; Tests try to run a mailman instance to test against.
    (inputs
     (list python-readme-renderer
           python-mailmanclient
           python-django
           python-django-mailman3))
    (native-inputs
     (list python-beautifulsoup4
           python-isort
           python-mock
           python-pdm-backend
           python-pytest
           python-vcrpy))
    (home-page "https://gitlab.com/mailman/postorius")
    (synopsis "Web user interface for GNU Mailman")
    (description
     "Postorius is a Django app which provides a web user interface
to access GNU Mailman.")
    (license (list license:gpl3+ license:lgpl3+))))

(define-public blists
  (package
    (name "blists")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.openwall.net/pub/projects/"
                           "blists/blists-" version ".tar.gz"))
       (sha256
        (base32
         "1xll5wn7py3bbncbwrj172f56nz75c9gwfsa80rwd96ss9gfmp3c"))))
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
    (version "20201014.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jetmore/swaks")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "131i2b1yxhnbqkfk4kky40pfanqw2c5lcgbnjhfqp5cvpawpk2ai"))))
    (build-system perl-build-system)
    (inputs
     (list bash-minimal
           perl-io-socket-inet6
           perl-net-dns
           perl-net-ssleay
           perl-socket6)) ; used by perl-io-socket-inet6
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-build_version
           (lambda _
             (substitute* "swaks"
               (("\"DEVRELEASE\"") (format #f "\"~a\"" ,version)))))
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "pod2man" "doc/base.pod" "swaks.1")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "swaks" (string-append out "/bin"))
               (install-file "swaks.1" (string-append out "/share/man/man1")))))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/swaks")
               `("PERL5LIB" ":" = (,(getenv "PERL5LIB")))))))))
    (home-page "https://jetmore.org/john/code/swaks/")
    (synopsis "Featureful SMTP test tool")
    (description "Swaks is a flexible, scriptable, transaction-oriented SMTP
test tool.  It handles SMTP features and extensions such as TLS,
authentication, and pipelining; multiple versions of the SMTP protocol
including SMTP, ESMTP, and LMTP; and multiple transport methods including
unix-domain sockets, internet-domain sockets, and pipes to spawned processes.
Options can be specified in environment variables, configuration files, and
the command line allowing maximum configurability and ease of use for
operators and scripters.")
    (license license:gpl2+)))

(define-public alpine
  (package
    (name "alpine")
    (version "2.26")
    (source
     (origin
       (method git-fetch)
       ;; There are two versions: the plain continuation of Alpine without extra
       ;; patches and the version which adds extra fixes. Every distro uses
       ;; the patched version, and so do we to not break expectations.
       ;; http://alpine.freeiz.com/alpine/readme/README.patches
       (uri (git-reference
             (url "https://repo.or.cz/alpine.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1padh9kgn9blzjf0016i2f15c615fk17m8vg8kx301jhmc2r973h"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove pre-built binaries scattered across the source repository.
           (for-each delete-file (find-files "." "\\.(dll|exe)"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target)))
       #:parallel-build? #f             ;fails otherwise
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
                                              "/bin/aspell")
                               "--with-date-stamp=Thu  1 Jan 01:00:01 CET 1970")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'assume-shadow-passwords
           ;; Alpine's configure script confuses ‘shadow password support’ with
           ;; ‘/etc/shadow exists in the build environment’.  It does not.
           (lambda _
             (substitute* "configure"
               (("test -f /etc/shadow") "true"))))
         (add-after 'unpack 'make-reproducible
           (lambda _
             ;; This removes time-dependent code to make alpine reproducible.
             (substitute* "pico/blddate.c"
               (("%02d-%s-%d") "1970-01-01")))))))
    (inputs
     (list ncurses
           openssl
           gnutls
           openldap
           cyrus-sasl
           mit-krb5
           aspell
           tcl
           libxcrypt
           linux-pam))
    (home-page "https://repo.or.cz/alpine.git")
    (synopsis "Alternatively Licensed Program for Internet News and Email")
    (description
     "Alpine is a text-based mail and news client.  Alpine includes several
tools and applications:
@enumerate
@item alpine, the Alpine mailer
@item pico, the standalone text editor, GNU nano's predecessor
@item pilot, the standalone file system navigator
@end enumerate\n")
    (license license:asl2.0)))

(define-public balsa
  (package
    (name "balsa")
    (version "2.6.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://pawsa.fedorapeople.org/balsa/"
                           "balsa-" version ".tar.xz"))
       (sha256
        (base32 "1hcgmjka2x2igdrmvzlfs12mv892kv4vzv5iy90kvcqxa625kymy"))))
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
         "--with-ldap")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'adjust-for-new-webkitgtk
                    (lambda _
                      (substitute* "configure"
                        (("webkit2gtk-4.0")
                         "webkit2gtk-4.1")))))))
    (inputs
     (list cyrus-sasl
           enchant
           gdk-pixbuf
           gmime
           gnutls
           gpgme
           gtk+
           gtksourceview-4
           gtkspell3
           libassuan ; in gpgme.pc Requires
           libcanberra
           libesmtp
           libical
           libnotify
           libsecret
           openldap
           sqlite
           webkitgtk-for-gtk3))
    (native-inputs
     (list compface
           `(,glib "bin") intltool pkg-config yelp-tools))
    (home-page "https://pawsa.fedorapeople.org/balsa")
    (synopsis "E-mail client for GNOME")
    (description "Balsa is a highly configurable and robust mail client for
the GNOME desktop.  It supports both POP3 and IMAP servers as well as the
mbox, maildir and mh local mailbox formats.  Balsa also supports SMTP and/or
the use of a local MTA such as Sendmail.")
    (properties
     '((release-monitoring-url
       . "https://pawsa.fedorapeople.org/balsa/download.html")))
    (license license:gpl3+)))

(define-public afew
  (package
    (name "afew")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "afew" version))
       (sha256
        (base32
         "0wpfqbqjlfb9z0hafvdhkm7qw56cr9kfy6n8vb0q42dwlghpz1ff"))))
    (build-system python-build-system)
    (inputs
     (list notmuch python-chardet python-dkimpy python-notmuch))
    (native-inputs
     (list python-freezegun python-setuptools-scm))
    (home-page "https://github.com/afewmail/afew")
    (synopsis "Initial tagging script for notmuch mail")
    (description "afew is an initial tagging script for notmuch mail.  It
provides automatic tagging each time new mail is registered with notmuch.  It
can add tags based on email headers or Maildir folders and can handle spam and
killed threads.")
    (license license:isc)))

(define-public pan
  (package
    (name "pan")
    (version "0.149")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://pan.rebelbase.com/download/releases/"
                           version "/source/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1sl5rdgalswxya61vhkf28r0fb4b3pq77qgzhhsfagmpvgbx0d2x"))))
    (arguments
     (list #:configure-flags
           #~(list "--with-gtk3" "--with-gtkspell" "--with-gnutls"
                   "--enable-libnotify" "--enable-manual"
                   "--enable-gkr")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-gpg2
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "pan/usenet-utils/gpg.cc"
                     (("\"gpg2\"")
                      (string-append "\""
                                     (search-input-file inputs "/bin/gpg")
                                     "\""))))))))
    (inputs
     (list gmime
           gnupg
           gnutls
           gtk+
           gtkspell3
           libnotify
           libsecret
           libxml2
           zlib))
    (native-inputs
     (list gettext-minimal itstool pkg-config))
    (build-system gnu-build-system)
    (home-page "http://pan.rebelbase.com/")
    (synopsis "Usenet newsreader")
    (description "@code{pan} is a Usenet newsreader that's good at both text
and binaries. It supports offline reading, scoring and killfiles, yEnc, NZB,
PGP handling, multiple servers, and secure connections.")
    ;; License of the docs: fdl-1.1; Others: gpl2.
    (license (list license:fdl1.1+ license:gpl2))))

(define-public imapfilter
  (package
    (name "imapfilter")
    (version "2.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lefcha/imapfilter")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0di9gwavgyr1xkd8sfh52ldkn2lq1kdad76ww2x4y0lhimnxw7gc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))         ; no configure script
    (inputs
     (list lua pcre2 openssl))
    (home-page "https://github.com/lefcha/imapfilter")
    (synopsis "IMAP mail filtering utility")
    (description "IMAPFilter is a mail filtering utility.  It connects
to remote mail servers using IMAP, sends searching queries to the server and
processes mailboxes based on the results.  It can be used to delete, copy,
move, flag, etc. messages residing in mailboxes at the same or different mail
servers.  The 4rev1 and 4 versions of IMAP are supported.")
    (license license:expat)))

(define-public urlscan
  (package
    (name "urlscan")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "urlscan" version))
        (sha256
         (base32 "10a5rgvzy6baqmr0x4lmzsr73a4jpbm04xyjmqlkr493vq08kgrv"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ; No tests.
    (propagated-inputs
     (list python-urwid))
    (native-inputs
     (list python-hatch-vcs python-hatchling))
    (home-page "https://github.com/firecat53/urlscan")
    (synopsis "View/select the URLs in an email message or file")
    (description
     "Urlscan is a small program that is designed to integrate with the
Mutt mail reader to allow you to easily launch a Web browser for URLs
contained in email messages.  It parses an email message or file and scans it
for URLs and email addresses.  It then displays the URLs and their context
within the message, and allows you to choose one or more URLs to send to your
Web browser.  Alternatively, it send a list of all URLs to standard output.
It is a replacement for the @command{urlview} program.")
    (license license:gpl2)))

(define-public tnef
  (package
    (name "tnef")
    (version "1.4.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/verdammelt/tnef")
             (commit version)))
       (sha256
        (base32 "104g48mcm00bgiyzas2vf86331w7bnw7h3bc11ib4lp7rz6zqfck"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (arguments `(#:parallel-tests? #f)) ;tests are side-effect'y
    (home-page "https://github.com/verdammelt/tnef")
    (synopsis "Unpack @code{application/ms-tnef} attachments")
    (description
     "TNEF is a tar-like program that unpacks MIME attachments of type
@code{application/ms-tnef}.")
    (license license:gpl2+)))

(define-public mumi
  (package
    (name "mumi")
    (version "0.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/guix/mumi.git/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04mcd1xkdpvxlvpf4k4mvnwi06sdy8vy1di6gxxsr9msgdb366ir"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((guix build gnu-build-system)
                  ((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  (guix build utils))
      #:imported-modules `((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)

      #:configure-flags '(list "--localstatedir=/var")

      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'install-picocss
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((pico (dirname (search-input-file inputs "/scss/pico.scss"))))
                (mkdir-p "assets/pico/scss")
                (copy-recursively pico "assets/pico/scss"))))
          (add-after 'install 'wrap-executable
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (version (target-guile-effective-version))
                     (scm (string-append #$output "/share/guile/site/" version))
                     (go  (string-append #$output "/lib/guile/" version
                                         "/site-ccache")))
                (wrap-program (string-append bin "/mumi")
                  `("GUILE_LOAD_PATH" ":" prefix
                    (,scm ,(getenv "GUILE_LOAD_PATH")))
                  `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                    (,go ,(getenv "GUILE_LOAD_COMPILED_PATH"))))))))))
    (inputs
     (list bash-minimal
           guile-avatar
           guile-email
           guile-gcrypt
           guile-gnutls
           guile-kolam
           guile-redis
           guile-syntax-highlight
           guile-webutils
           guile-xapian
           guile-3.0
           mailutils
           xdg-utils))
    (native-inputs
     (list autoconf automake pkg-config sassc
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/picocss/pico.git")
                   (commit "3052db4bd3439e236479dc0f98069f7d3b559486")))
             (file-name (git-file-name "pico" "1.5.6"))
             (sha256
              (base32
               "1gs1li48hqizx7lc4n2fdxn9i2v4vafkqpza7svvfpcamfz29jpi")))))
    (home-page "https://git.savannah.gnu.org/cgit/guix/mumi.git/")
    (synopsis "Debbugs web interface")
    (description "Mumi is a Debbugs web interface.")
    (license license:agpl3+)))

(define-public ytnef
  (package
    (name "ytnef")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yeraze/ytnef")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rwgr98jn86d37xmgj57pl488kw62q7vq8jjicbbqkxl6vjgh1li"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           '(list "--disable-static")))
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/Yeraze/ytnef/")
    (synopsis "TNEF stream reader for winmail.dat files")
    (description "This package provides a TNEF stream reader library and
related tools to process winmail.dat files.")
    (license license:gpl2+)))

(define-public l2md
  ;; No official release.
  (let ((commit "9db252bc1716ebaf0abd3a47a59ea78e4e6253d6")
        (revision "2"))
    (package
      (name "l2md")
      (version (git-version "0.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.kernel.org/pub/scm/linux/kernel/git/dborkman/l2md.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hfbngwdavdhw5ghnadmi0djg2yrr0wrkv15jdd9wcqh9h6mxy8z"))
         (snippet
          #~(begin (use-modules (guix build utils))
                   ;; Don't try to redefine loff_t.
                   (substitute* "utils.c"
                     (("typedef off_t loff_t;")
                      (string-append "#ifdef __APPLE__\n"
                                     "typedef off_t loff_t;\n"
                                     "#endif\n")))))))
      (build-system gnu-build-system)
      (inputs
       (list libgit2))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)          ;no configure scripts
           (delete 'check)              ;no tests
           (add-before 'install 'mkdir
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((l2md (string-append (assoc-ref outputs "out") "/bin")))
                 (mkdir-p l2md)))))
         #:make-flags
         (list ,(string-append "CC=" (cc-for-target))
               (string-append "PREFIX=" %output "/bin"))))
      (home-page
       "https://git.kernel.org/pub/scm/linux/kernel/git/dborkman/l2md.git")
      (synopsis "Import public-inbox archives via Git")
      (description
       "The @command{l2md} command line tool imports public-inbox archives via
Git and exports them in maildir format or to an MDA through a pipe.")
      (license license:gpl2))))

(define-public bubger
  (package
    (name "bubger")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://git.causal.agency/bubger/snapshot/bubger-"
                       version ".tar.gz"))
       (sha256
        (base32 "014r9p7f0ismhybvcs4p3s4ph3lcygn15kfdkd73i09fb82pqyw6"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; There are no tests.
           #:make-flags
           #~(list
              (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))))
    (native-inputs
     (list pkg-config universal-ctags))
    (inputs (list libressl))
    (home-page "https://code.causal.agency/june/bubger")
    (synopsis "IMAP archive generator")
    (description
     "@command{bubger} is a mailing list archive generator for mail stored in
IMAP.  It produces static files of HTML, Atom and mboxrd, making its output
easy to serve from a host without IMAP access.  It requires the IMAP THREAD
extension.")
    (license license:gpl3+)))

(define-public public-inbox
  (package
    (name "public-inbox")
    (version "1.9.0")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://public-inbox.org")
                   (commit (string-append "v" version))))
             (sha256
              (base32
               "0cgvxg0f32nvb3079x46gjkfis4bc98s6nx6kl8rm90kmb1kxkx9"))
             (file-name (git-file-name name version))))
    (build-system perl-build-system)
    (arguments
     `(#:modules ((guix build perl-build-system)
                  (guix build utils)
                  (ice-9 match))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'qualify-paths
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Use absolute paths for 'xapian-compact'.
             (substitute* "lib/PublicInbox/Xapcmd.pm"
               (("'xapian-compact'")
                (format #f "'~a'" (search-input-file inputs
                                                     "/bin/xapian-compact"))))
             (substitute* "lib/PublicInbox/TestCommon.pm"
               ;; This is only used for tests, but get it from ‘inputs’ so
               ;; that cross builds won't hold a reference to a package built
               ;; for another architecture.
               (("/bin/cp") (search-input-file inputs "/bin/cp")))))
         (add-before 'check 'pre-check
           (lambda _
             (invoke "./certs/create-certs.perl")))
         (replace 'check
           (lambda* (#:key target
                     (tests? (not target)) (test-flags '())
                     #:allow-other-keys)
             (if tests?
                 (match (primitive-fork)
                   (0                     ;child process
                    ;; lei tests build UNIX domain sockets in the temporary
                    ;; directory, but the path of those sockets can be at most
                    ;; 108 chars and Guix' default value for the variables
                    ;; below already use 47 chars. Use the shortest temporary
                    ;; path possible to avoid hitting the limit.
                    (setenv "TEMP" "/tmp")
                    (setenv "TEMPDIR" "/tmp")
                    (setenv "TMP" "/tmp")
                    (setenv "TMPDIR" "/tmp")

                    (apply execlp "make"
                           "make" "check" test-flags))
                   (make-pid
                    ;; Reap child processes; otherwise, lei-daemon is kept as
                    ;; a zombie and the testsuite fails thinking that it
                    ;; didn't quit as it should.
                    (let loop ()
                      (match (waitpid WAIT_ANY)
                        ((pid . status)
                         (if (= pid make-pid)
                             (unless (zero? status)
                               (error "`make check' exited with status"
                                      status))
                             (loop)))))))
                 (format #t "test suite not run~%"))))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (prog)
                  (wrap-program prog
                    ;; Let those scripts find their perl modules.
                    `("PERL5LIB" ":" prefix
                      (,(string-append out "/lib/perl5/site_perl")
                       ,(getenv "PERL5LIB")))
                    ;; 'git' is invoked in various files of the PublicInbox
                    ;; perl module.
                    `("PATH" ":" prefix
                      (,(dirname (search-input-file inputs "/bin/git"))
                       ,(dirname (search-input-file inputs "/bin/curl"))))))
                (find-files (string-append out "/bin")))))))))
    (native-inputs
     (list ;; For testing.
           lsof openssl))
    (inputs
     (append
      (if (not (target-64bit?))
          ;; Required by test t/pop3d.t, otherwise fails with
          ;; “sizeof(off_t)=8 requires File::FcntlLock”.
          (list perl-file-fcntllock)
          '())
      (list bash-minimal
            curl
            git
            perl-dbd-sqlite
            perl-dbi
            perl-email-address-xs
            perl-email-mime-contenttype
            perl-email-mime
            perl-email-simple
            perl-net-server
            perl-plack-middleware-deflater
            perl-plack-middleware-reverseproxy
            perl-plack
            perl-search-xapian
            perl-socket-msghdr
            perl-timedate
            perl-uri-escape
            perl-inline-c
            perl-parse-recdescent
            perl-linux-inotify2
            ;; FIXME: Perl modules are unable to find the config file for highlight
            ;; https://issues.guix.gnu.org/48033#4
            ;; ("highlight" ,highlight)
            ;; For testing.
            perl-ipc-run
            perl-xml-feed
            xapian)))
    (home-page "https://public-inbox.org/README.html")
    (synopsis "Archive mailing lists in Git repositories")
    (description
     "public-inbox implements the sharing of an email inbox via Git to
complement or replace traditional mailing lists.  Readers may read via NNTP,
IMAP, Atom feeds or HTML archives.")
    (license license:agpl3+)))

(define-public sylpheed
  (package
    (name "sylpheed")
    (version "3.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sylpheed.sraoss.jp/sylpheed/v3.7/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0j9y5vdzch251s264diw9clrn88dn20bqqkwfmis9l7m8vmwasqd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list bogofilter
           compface
           gnupg-1
           gpgme
           gtk+-2
           gtkspell3
           libnsl
           openldap
           openssl))
    (home-page "https://sylpheed.sraoss.jp/en/")
    (synopsis "Lightweight GTK+ email client")
    (description
     "Sylpheed is a simple, lightweight but featureful, and easy-to-use e-mail
client.  Sylpheed provides intuitive user-interface.  Sylpheed is also
designed for keyboard-oriented operation.")
    (properties '((release-monitoring-url
                   . "https://sylpheed.sraoss.jp/en/download.html")))
    (license license:gpl2+)))

(define-public python-authres
  (package
    (name "python-authres")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "authres" version))
        (sha256
         (base32
          "1dr5zpqnb54h4f5ax8334l1dcp8j9083d7v4vdi1xqkwmnavklck"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           ;; Run doctests as described in the README.
           (lambda _
             (invoke "python" "-m" "authres" "-v"))))))
    (home-page "https://launchpad.net/authentication-results-python")
    (synopsis "Authentication-Results email header creator and parser")
    (description
     "This Python module can be used to generate and parse RFC 5451/7001/7601
@code{Authentication-Results} email headers.  It supports extensions such as:
@itemize
@item RFC 5617 DKIM/ADSP
@item RFC 6008 DKIM signature identification (@code{header.b})
@item RFC 6212 @acronym{VBR, Vouch By Reference}
@item RFC 6577 @acronym{SPF, Sender Policy Framework}
@item RFC 7281 @code{Authentication-Results} registration for S/MIME
@item RFC 7293 The @code{Require-Recipient-Valid-Since} header field
@item RFC 7489 @acronym{DMARC, Domain-based Message Authentication Reporting
and Conformance}
@item @acronym{ARC, Authenticated Received Chain}
(draft-ietf-dmarc-arc-protocol-08)
@end itemize\n")
    (license license:asl2.0)))

(define-public python-dkimpy
  (package
    (name "python-dkimpy")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "dkimpy" version))
        (sha256
         (base32 "088iz5cqjqh4c7141d94pvn13bh25aizqlrifwv6fs5g16zj094s"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'patch-source-shebangs 'patch-more-source
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((openssl (assoc-ref inputs "openssl")))
               (substitute* "dkim/dknewkey.py"
                 (("/usr/bin/openssl") (string-append openssl "/bin/openssl"))))
             #t))
         (replace 'check
           (lambda _
             (invoke "python" "test.py"))))))
    (propagated-inputs
     (list python-dnspython))
    (native-inputs
     (list python-authres python-pynacl))
    (inputs
     (list openssl))
    (home-page "https://launchpad.net/dkimpy")
    (synopsis "DKIM (DomainKeys Identified Mail)")
    (description "Python module that implements @dfn{DKIM} (DomainKeys
Identified Mail) email signing and verification (RFC6376).  It also provides
helper scripts for command line signing and verification.  It supports DKIM
signing/verifying of ed25519-sha256 signatures (RFC 8463).  It also supports
the RFC 8617 Authenticated Received Chain (ARC) protocol.")
    (license license:bsd-3)))

(define-public python-authheaders
  (package
    (name "python-authheaders")
    (version "0.13.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "authheaders" version))
        (sha256
         (base32
          "14k6i72k5f8dyvps8vc0aq0cczc8lvqpgjfjzsy6qqychjvjcmwk"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-authres python-dkimpy python-dnspython
           python-publicsuffix2))
    (home-page "https://github.com/ValiMail/authentication-headers")
    (synopsis "Library wrapping email authentication header verification and generation")
    (description
     "This is a Python library for the generation of email authentication
headers.  The library can perform DKIM, SPF, and DMARC validation, and the
results are packaged into the Authentication-Results header.  The library can
DKIM and ARC sign messages and output the corresponding signature headers.")
    ;; The package's metadata claims it were MIT licensed, but the source file
    ;; headers disagree. MPL-2 for the public suffix list.
    (license (list license:zpl2.1 license:zlib license:mpl2.0))))

(define-public python-aiosmtpd
  (package
    (name "python-aiosmtpd")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aio-libs/aiosmtpd")
             (commit version)))
       (sha256
        (base32 "0083d6nf75xv8nq1il6jabz36v6c452svy4p402csxwwih5pw6sk"))
       (file-name (git-file-name name version))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-failing-tests
           (lambda _
             ;; This test uses an expired certificate.
             (delete-file "aiosmtpd/tests/test_smtps.py")
             #t))
         (replace 'check
           (lambda _
             (invoke "python" "-m" "nose2" "-v"))))))
    (native-inputs
     (list python-flufl-testing python-nose2))
    (propagated-inputs
     (list python-atpublic))
    (home-page "https://aiosmtpd.readthedocs.io/")
    (synopsis "Asyncio based SMTP server")
    (description
     "This project is a reimplementation of the Python stdlib @code{smtpd.py}
based on asyncio.")
    (license (list license:asl2.0
                   license:lgpl3))))    ; only for setup_helpers.py

(define-public python-imaplib2
  (package
    (name "python-imaplib2")
    (version "3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "imaplib2" version))
       (sha256
        (base32
         "0nqyb274hq30agg1c0zkb5ijmcirgg35sp4dp4n292l665dlijwn"))))
    (build-system python-build-system)
    (home-page "https://github.com/jazzband/imaplib2/")
    (synopsis "Threaded Python IMAP4 client")
    (description "This package provides a threaded Python IMAP4 client, based
on RFC 3501 and original @code{imaplib} module.")
    (license license:expat)))

(define-public rspamd
  (package
    (name "rspamd")
    (version "3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rspamd/rspamd")
             (commit version)))
       (sha256
        (base32 "1ra18c3wczbdqrg9p69k04smjskjkdpxcfff9ff4yi7pmqjaxr8s"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DENABLE_LUAJIT=ON"
                           "-DLOCAL_CONFDIR=/etc/rspamd")))
    (inputs
     (list file
           glib
           icu4c
           libsodium
           luajit
           openssl
           pcre2
           perl
           ragel
           sqlite
           zlib))
    (native-inputs
     (list pkg-config))
    (synopsis "Spam filtering system")
    (description "Rspamd is an advanced spam filtering system that
allows evaluation of messages by a number of rules including regular
expressions, statistical analysis and custom services such as URL
black lists.  Each message is analysed by Rspamd and given a spam
score.")
    (home-page "https://www.rspamd.com/")
    (license license:asl2.0)))

(define-public undbx
  (package
    (name "undbx")
    (version "0.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/undbx/undbx-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ncs1dzhrn9nlaxpyap2ipf61fc7k9bkkqacp3w6bngfj2c0p6yj"))))
    (build-system gnu-build-system)
    (home-page "https://undbx.sourceforge.io/")
    (synopsis "Extract email messages from Outlook Express .dbx files")
    (description "This package provides a tool to extract, recover and
undelete email messages from Outlook Express .dbx files.")
    (license license:gpl3+)))

(define-public libdbx
  (package
    (name "libdbx")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ol2mbox/LibDBX/v"
                                  version "/libdbx_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0fs4268qcy99nhl8345sv257b002530y77idkf6z9i7qxmqghq4w"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                   ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'install
            (lambda _
              (for-each (lambda (file)
                          (install-file file
                                        (string-append #$output "/bin")))
                        (list "readdbx" "readoe")))))))
    (home-page "http://sourceforge.net/projects/ol2mbox/")
    (synopsis "Tools for conversion of Outlook Express files to mailbox format")
    (description "This package provides tools for the conversion of Outlook
Express data files to standard mailbox format.")
    (license license:gpl2+)))

(define-public libpst
  (package
    (name "libpst")
    (version "0.6.76")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.five-ten-sg.com/libpst/packages/"
                           "libpst-" version ".tar.gz"))
       (sha256
        (base32
         "0hhbbb8ddsgjhv9y1xd8s9ixlhdnjmhw12v06jwx4j6vpgp1na9x"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'fix-python-detection
             (lambda _
               (delete-file "configure")
               (substitute* "m4/ax_python.m4"
                 (("python3\\.9")
                  "python3.12 python3.11 python3.10 python3.9")))))))
    (inputs
     (list boost libgsf python zlib))
    (native-inputs
     (list autoconf automake gettext-minimal libtool pkg-config))
    (home-page "https://www.five-ten-sg.com/libpst/")
    (synopsis "Tools to process Outlook email archives")
    (description "The Libpst utilities include @code{readpst} which can
convert email messages to both mbox and MH mailbox formats, @code{pst2ldif}
which can convert the contacts to @code{.ldif} format for import into LDAP
databases, and other tools to process Outlook email archives.")
    (license license:gpl2+)))

(define-public neatmail
  (package
    (name "neatmail")
    (version "02")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/aligrudi/neatmail")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00d453d57d3v6ja2gpmjd8vch804c72g38pc1nr5shmz3hkgd52d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no tests
       #:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configure script
         (replace 'install              ;no install target in the Makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin")))
               (rename-file "mail" "neatmail")
               (install-file "neatmail" bin)))))))
    (home-page "https://litcave.rudi.ir/")
    (synopsis "Text-mode mail client")
    (description
     "@command{neatmail} is a noninteractive mail client.  It generates
a listing of the messages in a mailbox in mbox format and executes a list of
ex-like commands on it.")
    (license license:isc)))

(define-public crm114
  (package
    (name "crm114")
    (version "20100106")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://crm114.sourceforge.net/tarballs/crm114-"
                           version "-BlameMichelson.src.tar.gz"))
       (sha256
        (base32
         "0awcjc5j2mclkkpbjyijj9mv8xjz3haljvaj0fyc4fm4xir68qpv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils)
                  (ice-9 string-fun))
       #:imported-modules (,@%default-gnu-imported-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:make-flags (list (string-append "prefix=" %output)
                          "LDFLAGS=")   ; disable static linking
       ;; Test suite is not fully automated. It requires a human to read the
       ;; results and determine if the tests have passed.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-build
           (lambda _
             ;; Inline functions can only be used from the same compilation
             ;; unit. This causes the build to fail.
             (substitute* "crm_svm_matrix.c"
               (("^inline ") ""))
             ;; Building with gcc 10 fails without the -fcommon flag. Add it
             ;; to CFLAGS.
             (substitute* "Makefile"
               (("CFLAGS \\+= -DVERSION")
                "CFLAGS += -fcommon -DVERSION"))))
         (add-before 'install 'pre-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               ;; Install maillib.crm library.
               (install-file "maillib.crm" (string-append out "/share/crm"))
               ;; Set absolute store paths.
               (substitute* "mailreaver.crm"
                 (("insert maillib.crm")
                  (string-append "insert " out "/share/crm/maillib.crm"))
                 (("\\\\/bin\\\\/ls")
                  (string-replace-substring (which "ls") "/" "\\/"))
                 ((":\\*:trainer_invoke_command:")
                  (string-append out "/bin/mailtrainer.crm")))
               ;; Install mail related crm scripts.
               (for-each (lambda (file)
                           (install-file file (string-append out "/bin")))
                         (list "mailfilter.crm" "mailreaver.crm" "mailtrainer.crm")))))
         ;; Run phases from the emacs build system.
         (add-after 'unpack 'make-autoloads
           (assoc-ref emacs:%standard-phases 'make-autoloads))
         (add-after 'install 'install-emacs-mode
           (assoc-ref emacs:%standard-phases 'install))
         (add-after 'install-emacs-mode 'emacs-build
           (assoc-ref emacs:%standard-phases 'build))
         (add-after 'emacs-build 'validate-compiled-autoloads
           (assoc-ref emacs:%standard-phases 'validate-compiled-autoloads)))))
    (inputs
     (list tre))
    (native-inputs
     `(("emacs" ,emacs-minimal)))
    (home-page "https://crm114.sourceforge.net/")
    (synopsis "Controllable regex mutilator")
    (description "CRM114 is a system to examine incoming e-mail, system log
streams, data files or other data streams, and to sort, filter, or alter the
incoming files or data streams according to the user's wildest desires.
Criteria for categorization of data can be via a host of methods, including
regexes, approximate regexes, a Hidden Markov Model, Orthogonal Sparse
Bigrams, WINNOW, Correlation, KNN/Hyperspace, or Bit Entropy (or by other
means--it's all programmable).")
    (license license:gpl3)))

(define-public rss2email
  (package
    (name "rss2email")
    (version "3.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rss2email/rss2email")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rmcwvf8whf49qq5rgp5hhmhfjli1vhjlc7fjhj24gyy1kkjir2k"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "test"
                  ;; Skip networking tests
                  (substitute* "test.py"
                    (("( *)class (:?TestSend|TestFetch).*" match indent)
                     (string-append indent
                                    "@unittest.skip(\"Networking stuff skipped\")\n"
                                    indent match)))
                  (invoke "python" "-m" "unittest")))))
          (add-after 'install 'install-documentation
            (lambda _
              (install-file "r2e.1" (string-append #$output "share/man/man1")))))))
    (inputs
     (list python-feedparser python-html2text))
    (home-page "https://github.com/rss2email/rss2email")
    (synopsis "Converts RSS/Atom newsfeeds to email")
    (description "The RSS2email program (@command{r2e}) fetches RSS/Atom news
feeds, converts them into emails, and sends them.")
    ;; GPL version 2 or 3.  NOT 2+.
    (license (list license:gpl2
                   license:gpl3))))

(define-public sendgmail
  (let ((commit "e3229155a4037267ce40f1a3a681f53221aa4d8d")
        (revision "1"))
    (package
      (name "sendgmail")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/gmail-oauth2-tools")
               (commit commit)))
         (file-name (git-file-name name version))
         (patches (search-patches
                   "sendgmail-remove-domain-restriction.patch"
                   "sendgmail-accept-ignored-gsuite-flag.patch"))
         (sha256
          (base32 "1cxpkiaajhq1gjsg47r2b5xgck0r63pvkyrkm7af8c8dw7fyn64f"))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:tests? #f ; no tests
        #:unpack-path "github.com/google/gmail-oauth2-tools"
        #:import-path "github.com/google/gmail-oauth2-tools/go/sendgmail"))
      (inputs
       (list go-golang-org-x-oauth2
             go-cloud-google-com-go-compute-metadata))
      (home-page "https://github.com/google/gmail-oauth2-tools")
      (synopsis
       "Sendmail-compatible tool for using Gmail with @code{git send-email}")
      (description
       "The @command{sendgmail} command provides a minimal sendmail-compatible
front-end that connects to Gmail using OAuth2.  It is specifically designed
for use with @code{git send-email}.  The command needs a Gmail API key to
function.

Guix's version of @command{sendgmail} has been patched for compatibility with
all known forks, including support for non-@code{@@gmail.com} email
addresses.")
      (license license:asl2.0))))

(define-public smtpmail
  (package
    (name "smtpmail")
    (version "0.4.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/smtpmail/smtpmail-"
                           version ".tar.gz"))
       (sha256
        (base32 "08ap2l2g2avkq2jx05jy993517vvapmypg7j5cwl8gvpq436gdh5"))))
    (build-system gnu-build-system)
    (home-page "https://www.nongnu.org/smtpmail/")
    (synopsis "SMTP utility")
    (description
     "smtpmail is a little console-based tool for users who have no local
mailserver on their machine.  It enables these users to send their mail over a
remote SMTP server.")
    (license license:gpl2+)))

(define-public aerc
  (package
    (name "aerc")
    (version "0.20.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~rjarry/aerc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04412inhzj8vwybafqz6nw8wrsyf51zjv0881aacr6zc9bfcq510"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~rjarry/aerc"
           ;; Installing the source is only necessary for Go libraries.
           #:install-source? #f
           #:build-flags
           #~(list (string-append
                    "-ldflags=-X main.Version=" #$version
                    " -X git.sr.ht/~rjarry/aerc/config.libexecDir="
                    #$output "/libexec/aerc"
                    " -X git.sr.ht/~rjarry/aerc/config.shareDir="
                    #$output "/share/aerc"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-paths
                 (lambda* (#:key import-path inputs #:allow-other-keys)
                   (with-directory-excursion (string-append "src/" import-path)
                     (substitute* (find-files "." "\\.go$")
                       ;; Patch all occurrences to "sh" with absolute path to
                       ;; the shell available in Guix.
                       (("\"sh\"")
                        (format #f "~s" (which "sh"))))
                     (let ((zoxide #$(this-package-input "zoxide")))
                       (when zoxide
                         (substitute* "commands/z.go"
                           (("\"zoxide\"")
                            (format #f "~s"
                                    (string-append zoxide "/bin/zoxide"))))))
                     (substitute* (list "lib/crypto/gpg/gpg.go"
                                        "lib/crypto/gpg/gpg_test.go"
                                        "lib/crypto/gpg/gpgbin/keys.go"
                                        "lib/crypto/gpg/gpgbin/gpgbin.go")
                       (("\"gpg\"")
                        (format #f "~s"
                                (string-append #$(this-package-input "gnupg")
                                               "/bin/gpg")))
                       (("strings\\.Contains\\(stderr\\.String\\(\\), .*\\)")
                        "strings.Contains(stderr.String(), \"gpg\")")))))
               (add-after 'build 'doc
                 (lambda* (#:key import-path build-flags #:allow-other-keys)
                   (invoke "make" "doc" "-C"
                           (string-append "src/" import-path))))
               ;; XXX: This phase does the build and overwrites
               ;; go-build-system's build one.
               (replace 'install
                 (lambda* (#:key import-path build-flags #:allow-other-keys)
                   (invoke "make"
                           (string-append "CC=" #$(cc-for-target))
                           (string-append "GOFLAGS=-tags=notmuch " (getenv "GOFLAGS"))
                           (string-append "PREFIX=" #$output)
                           "install" "-C"
                           (string-append "src/" import-path)))))))
    (inputs
     (append
      (list gnupg
            notmuch
            python
            python-vobject)
      (if (supported-package? zoxide)
          (list zoxide)
          '())))
    (native-inputs
     (list go-git-sr-ht-rjarry-go-opt-v2
           go-git-sr-ht-rockorager-go-jmap
           go-git-sr-ht-rockorager-vaxis
           go-github-com-protonmail-go-crypto
           go-github-com-arran4-golang-ical
           go-github-com-danwakefield-fnmatch
           go-github-com-emersion-go-imap
           go-github-com-emersion-go-imap-sortthread
           go-github-com-emersion-go-maildir
           go-github-com-emersion-go-mbox
           go-github-com-emersion-go-message
           go-github-com-emersion-go-msgauth
           go-github-com-emersion-go-pgpmail
           go-github-com-emersion-go-sasl
           go-github-com-emersion-go-smtp
           go-github-com-fsnotify-fsnotify
           go-github-com-gatherstars-com-jwz
           go-github-com-go-ini-ini
           go-github-com-lithammer-fuzzysearch
           go-github-com-mattn-go-isatty
           go-github-com-mattn-go-runewidth
           go-github-com-pkg-errors
           go-github-com-riywo-loginshell
           go-github-com-stretchr-testify
           go-github-com-syndtr-goleveldb
           go-golang-org-x-image
           go-golang-org-x-oauth2
           go-golang-org-x-sys
           go-golang-org-x-tools
           scdoc))
    (home-page "https://git.sr.ht/~rjarry/aerc")
    (synopsis "Email client for the terminal")
    (description "@code{aerc} is a textual email client for terminals.  It
features:
@enumerate
@item First-class support for using patches and @code{git send-email}
@item Vi-like keybindings and command system
@item A built-in console
@item Support for multiple accounts
@end enumerate")
    ;; The license given is MIT/Expat; however, linking against notmuch
    ;; effectively makes it GPL-3.0-or-later. See this thread discussing it:
    ;; <https://lists.sr.ht/~rjarry/aerc-devel/%3Cb5cb213a7d0c699a886971658c2476
    ;; 1073eb2391%40disroot.org%3E>
    (license license:gpl3+)))

(define-public hydroxide
  (package
    (name "hydroxide")
    (version "0.2.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/hydroxide")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11gbikrgm7nf0zjav64202wsnr9pvrmslm2rzg9d9rbvwdqcq1jl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/emersion/hydroxide/cmd/hydroxide"
      #:unpack-path "github.com/emersion/hydroxide"))
    (native-inputs
     (list go-github-com-protonmail-go-crypto
           go-github-com-boltdb-bolt
           go-github-com-emersion-go-bcrypt
           go-github-com-emersion-go-imap
           go-github-com-emersion-go-mbox
           go-github-com-emersion-go-message
           go-github-com-emersion-go-smtp
           go-github-com-emersion-go-vcard
           go-github-com-emersion-go-webdav
           go-golang-org-x-crypto
           go-golang-org-x-term))
    (home-page "https://github.com/emersion/hydroxide")
    (synopsis "ProtonMail CardDAV, IMAP and SMTP bridge")
    (description
     "This package implements a functionality to translate standard
protocols (SMTP, IMAP, CardDAV) into ProtonMail API requests.")
    (license license:expat)))
