;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015, 2017, 2022 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016 Alex Sassmannshausen <alex@pompo.co>
;;; Copyright © 2016-2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Erik Edrosa <erik.edrosa@gmail.com>
;;; Copyright © 2016, 2019-2021, 2023 Eraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2021 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@openmailbox.org>
;;; Copyright © 2016, 2021 Amirouche <amirouche@hypermove.net>
;;; Copyright © 2016, 2019, 2021, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 David Thompson <davet@gnu.org>
;;; Copyright © 2017, 2018, 2019, 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2021, 2022, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2018–2025 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 swedebugia <swedebugia@riseup.net>
;;; Copyright © 2019, 2020 Amar Singh <nly@disroot.org>
;;; Copyright © 2019, 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020 Evan Straw <evan.straw99@gmail.com>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Julien Lepiler <julien@lepiller.eu>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021 Masaya Tojo <masaya@tojo.tokyo>
;;; Copyright © 2020 Jesse Gibbons <jgibbons2357@gmail.com>
;;; Copyright © 2020 Mike Rosset <mike.rosset@gmail.com>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020, 2021, 2022 pukkamustard <pukkamustard@posteo.net>
;;; Copyright © 2021 Bonface Munyoki Kilyungi <me@bonfacemunyoki.com>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Leo Le Bouter <lle-bout@zaclys.net>
;;; Copyright © 2021 Zelphir Kaltstahl <zelphirkaltstahl@posteo.de>
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021, 2022, 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Antero Mejr <antero@mailbox.org>
;;; Copyright © 2022 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022, 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022, 2025 Evgeny Pisemsky <mail@pisemsky.site>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2024 Ilya Chernyshov <ichernyshovvv@gmail.com>
;;; Copyright © 2024 Artyom Bologov <mail@aartaka.me>
;;; Copyright © 2024 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2024 Alec Barreto <mrh57@posteo.net>
;;; Copyright © 2024 Josep Bigorra <jjbigorra@gmail.com>
;;; Copyright © 2024 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2025 Florian Pelz <pelzflorian@pelzflorian.de>
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

(define-module (gnu packages guile-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)         ;pandoc
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mes)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages noweb)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tree-sitter)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system guile)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (ice-9 match)
  #:use-module ((srfi srfi-1) #:select (alist-delete))
  #:use-module (srfi srfi-26))

(define-public artanis
  (package
    (name "artanis")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/artanis/artanis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "013rs623075bbf824hf6jxng0kwbmg587l45fis9mmpq5168kspq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "artanis/third-party/json.scm")
                  (delete-file-recursively "artanis/third-party/redis.scm")
                  (substitute* '("artanis/artanis.scm"
                                 "artanis/lpc.scm"
                                 "artanis/i18n/json.scm"
                                 "artanis/oht.scm"
                                 "artanis/tpl/parser.scm")
                    (("(#:use-module \\()artanis third-party (json\\))" _
                      use-module json)
                     (string-append use-module json)))
                  (substitute* '("artanis/lpc.scm"
                                 "artanis/session.scm")
                    (("(#:use-module \\()artanis third-party (redis\\))" _
                      use-module redis)
                     (string-append use-module redis)))
                  (substitute* "artanis/oht.scm"
                    (("([[:punct:][:space:]]+)(->json-string)([[:punct:][:space:]]+)"
                      _ pre json-string post)
                     (string-append pre
                                    "scm" json-string
                                    post)))
                  (substitute* "artanis/artanis.scm"
                    (("[[:punct:][:space:]]+->json-string[[:punct:][:space:]]+")
                     ""))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal guile-3.0 nspr nss))
    ;; FIXME the bundled csv contains one more exported procedure
    ;; (sxml->csv-string) than guile-csv. The author is maintainer of both
    ;; projects.
    ;; TODO: Add guile-dbi and guile-dbd optional dependencies.
    (propagated-inputs
     (list guile-json-4 guile-curl guile-readline guile-redis))
    (native-inputs
     (list bash-minimal                           ;for the `source' builtin
           pkg-config
           util-linux))                           ;for the `script' command
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:make-flags
       ;; TODO: The documentation must be built with the `docs' target.
       (let* ((out (assoc-ref %outputs "out"))
              ;; We pass guile explicitly here since this executes before the
              ;; set-paths phase and therefore guile is not yet in PATH.
              (effective-version (target-guile-effective-version
                                  (assoc-ref %build-inputs "guile")))
              (scm (string-append out "/share/guile/site/" effective-version))
              (go (string-append out "/lib/guile/" effective-version "/site-ccache")))
         ;; Don't use (%site-dir) for site paths.
         (list (string-append "MOD_PATH=" scm)
               (string-append "MOD_COMPILED_PATH=" go)))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-site-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "artanis/commands/help.scm"
               (("\\(%site-dir\\)")
                (string-append "\""
                               (assoc-ref outputs "out")
                               "/share/guile/site/"
                               (target-guile-effective-version)
                               "\"")))))
         (add-after 'unpack 'patch-reference-to-libnss
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "artanis/security/nss.scm"
               (("ffi-binding \"libnss3\"")
                (string-append
                 "ffi-binding \""
                 (assoc-ref inputs "nss") "/lib/nss/libnss3.so"
                 "\""))
               (("ffi-binding \"libssl3\"")
                (string-append
                 "ffi-binding \"" (assoc-ref inputs "nss") "/lib/nss/libssl3.so\"")))))
         (add-before 'install 'substitute-root-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "out")))
               (substitute* "Makefile"   ;ignore the execution of bash.bashrc
                 ((" /etc/bash.bashrc") " /dev/null"))
               (substitute* "Makefile"   ;set the root of config files to OUT
                 ((" /etc") (string-append " " out "/etc")))
               (mkdir-p (string-append out "/bin")) )))
         (add-after 'install 'wrap-art
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective-version (target-guile-effective-version))
                    (bin (string-append out "/bin"))
                    (scm (string-append out "/share/guile/site/" effective-version))
                    (go (string-append out "/lib/guile/" effective-version
                                       "/site-ccache")))
               (wrap-program (string-append bin "/art")
                 `("GUILE_LOAD_PATH" ":" prefix
                   (,scm ,(getenv "GUILE_LOAD_PATH")))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                   (,go ,(getenv "GUILE_LOAD_COMPILED_PATH"))))))))))
    (synopsis "Web application framework written in Guile")
    (description "GNU Artanis is a web application framework written in Guile
Scheme.  A web application framework (WAF) is a software framework that is
designed to support the development of dynamic websites, web applications, web
services and web resources.  The framework aims to alleviate the overhead
associated with common activities performed in web development.  Artanis
provides several tools for web development: database access, templating
frameworks, session management, URL-remapping for RESTful, page caching, and
more.")
    (home-page "https://www.gnu.org/software/artanis/")
    (license (list license:gpl3+ license:lgpl3+)))) ;dual license

(define-public guilescript
  (package
    (name "guilescript")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aconchillo/guilescript")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15bvgklv77kvkl8dizriqblfir6rid5nm79ymi3m2fvpd7wf77qy"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list "GUILE_AUTO_COMPILE=0")
           #:modules `(((guix build guile-build-system)
                        #:select (target-guile-effective-version))
                       ,@%default-gnu-modules)
           #:imported-modules `((guix build guile-build-system)
                                ,@%default-gnu-imported-modules)
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'wrap-guilescript
                          (lambda _
                            (let* ((bin (string-append #$output "/bin"))
                                   (version (target-guile-effective-version))
                                   (scm (string-append "/share/guile/site/"
                                                       version))
                                   (go (string-append "/lib/guile/"
                                                      version
                                                      "/site-ccache")))
                              (wrap-program (string-append bin "/guilescript")
                                `("GUILE_LOAD_PATH" prefix
                                  (,(string-append #$output scm)))
                                `("GUILE_LOAD_COMPILED_PATH" prefix
                                  (,(string-append #$output go))))))))))
    (native-inputs (list autoconf automake pkg-config))
    (inputs (list guile-3.0 bash-minimal))
    (home-page "https://github.com/aconchillo/guilescript")
    (synopsis "Guile to JavaScript compiler")
    (description
     "GuileScript is a toy compiler that aims to compile Guile to JavaScript.  It
currently does not do much, but it might in the future.")
    (license license:gpl3+)))

(define-public guile-oauth
  (package
    (name "guile-oauth")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aconchillo/guile-oauth")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "002hgs4xfrrz0rqa6n1078cn7vz5f70azw1kpljvb4dmv228gfxq"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake pkg-config))
    (inputs (list guile-3.0 gnutls guile-gcrypt guile-gnutls guile-json-4))
    (home-page "https://github.com/aconchillo/guile-oauth")
    (synopsis "OAuth module for Guile")
    (description
     "This package provides Guile modules to interface with the OAuth and
OAuth2 protocols.")
    (license license:gpl3+)))

(define-public guile-openai
  (let ((commit "751cd5db5f8bb7c00e60042a7ec86100930b0f02")
        (revision "1"))
    (package
      (name "guile-openai")
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/flatwhatson/guile-openai")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1rl15wkm682xwzj2fjn4czp1haxnxlcjsk3g69j2a9qlwc4w0g4a"))))
      (build-system gnu-build-system)
      (arguments (list #:strip-binaries? #f))
      (inputs (list guile-3.0-latest imagemagick))
      (propagated-inputs
       (list guile-colorized
             guile-gnutls
             guile-json-4
             guile-picture-language))
      (native-inputs (list autoconf automake pkg-config))
      (home-page "https://gitlab.com/flatwhatson/guile-openai")
      (synopsis "Guile implementation of the OpenAI API")
      (description
       "Guile OpenAI is an implementation of the OpenAI API in Guile Scheme,
providing a convenient interface for interactive programming with their AI
models.")
      (license license:agpl3+))))

;; There are no releases yet of this package.
(define-public guile-pipe
  (let ((commit "0746ec38d19d844dff0c6f62f209b2b6c8d8872e")
        (revision "0"))
    (package
      (name "guile-pipe")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/joshwalters/guile-pipe")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "038gwrhfywgs8372q478wn4623lhcmkknfj4p8yaa93bykfc0fml"))))
      (build-system guile-build-system)
      (native-inputs
       (list guile-3.0))
      (home-page "https://github.com/joshwalters/guile-pipe")
      (synopsis "Guile pipe macros for functional chaining")
      (description
       "This package provides macros for functional chaining in Guile, similar
to UNIX pipes (@code{|}), Clojure's threading macros (@code{->} and
@code{->>}).")
      (license license:gpl3+))))

(define-public guile-pubstrate
  (let ((commit "b11b7df5e7ffefa45c5859b868d8125c4d939418")
        (revision "1"))
    (package
      (name "guile-pubstrate")
      (version (git-version "0.1.dev" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/dustyweb/pubstrate")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1qk45b3hjhzzq3dim699jrbmlc7ryr4s1fiz99ljz16rag2rr5p4"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        '(list "GUILE_AUTO_COMPILE=0")
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'fix-build-system
             (lambda _
               (substitute* "configure.ac"
                 (("GUILE_PROGS" m)
                  (string-append m "
guilemoduledir=\"${datarootdir}/guile/site/${GUILE_EFFECTIVE_VERSION}\"
AC_SUBST([guilemoduledir])
AC_SUBST([GUILE_EFFECTIVE_VERSION])
")))
               ;; The user.scm line is doubled
               (substitute* "Makefile.am"
                 ((".*pubstrate/webapp/user.scm.*") "")
                 ((".*pubstrate/webapp/app.scm.*" m)
                  (string-append m "pubstrate/webapp/user.scm \\\n"))
                 (("/ccache") "/site-ccache"))))
           (add-after 'unpack 'fix-srfi64-tests
             (lambda _
               (substitute* (find-files "tests/" "test-.*\\.scm$")
                 (("\\(test-exit\\)") "")
                 (("\\(test-end.*" m)
                  (string-append "(test-exit)" m))))))))
      (native-inputs
       (list autoconf
             automake
             pkg-config
             texinfo))
      (inputs
       (list guile-3.0
             libgcrypt))
      (propagated-inputs
       (list gnutls
             guile-8sync-for-pubstrate
             guile-gcrypt
             guile-gdbm-ffi
             guile-irregex
             guile-lib
             guile-sjson
             guile-webutils))
      (home-page "https://gitlab.com/dustyweb/pubstrate/")
      (synopsis "ActivityStreams and ActivityPub implementation in Guile")
      (description "This package provides an implementation of ActivityStreams
and ActivityPub in Guile.  It includes a full (currently demo) web server.")
      (license license:gpl3+))))

(define-public guile-f-scm
  (package
    (name "guile-f-scm")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~brown121407/f.scm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14wyrs3m1649l3km4pl2175dmap1372j5h8nkhykrbxg5xqp6ivd"))))
    (build-system guile-build-system)
    (native-inputs
     (list guile-3.0))
    (home-page "https://git.sr.ht/~brown121407/f.scm")
    (synopsis "Library for working with files and directories")
    (description
     "f.scm is a library intended to facilitate working with files and
directories (the file system in general).  It was initially inspired by the
f library for Emacs.")
    (license license:gpl3+)))

;; There has not been any release yet.
(define-public guildhall
  (let ((commit "2fe2cc539f4b811bbcd69e58738db03eb5a2b778")
        (revision "1"))
    (package
      (name "guildhall")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ijp/guildhall")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "115bym7bg66h3gs399yb2vkzc2ygriaqsn4zbrg8f054mgy8wzn1"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Tests fail without this fix because they try to load the bash
           ;; executable as a Scheme file.  See bug report at
           ;; https://github.com/ijp/guildhall/issues/22
           (add-after 'unpack 'fix-bug-22
             (lambda _
               (substitute* "Makefile.am"
                 (("TESTS_ENVIRONMENT=.*")
                  "AM_TESTS_ENVIRONMENT=srcdir=$(abs_top_srcdir)/tests/
TEST_EXTENSIONS = .scm
SCM_LOG_COMPILER= $(top_builddir)/env $(GUILE)
AM_SCM_LOG_FLAGS =  --no-auto-compile -s")
                 ;; FIXME: one of the database tests fails for unknown
                 ;; reasons.  It does not fail when run outside of Guix.
                 (("tests/database.scm") ""))
               #t)))))
      (inputs
       (list guile-2.0))
      (native-inputs
       (list zip ; for tests
             autoconf automake texinfo))
      (synopsis "Package manager for Guile")
      (description
       "Guildhall is a package manager written for Guile Scheme.  A guild is
an association of independent craftspeople.  A guildhall is where they meet.
This Guildhall aims to make a virtual space for Guile wizards and journeyfolk
to share code.

On a practical level, Guildhall lets you share Scheme modules and programs
over the internet, and install code that has been shared by others.  Guildhall
can handle dependencies, so when a program requires several libraries, and
each of those has further dependencies, all of the prerequisites for the
program can be installed in one go.")
      (home-page "https://github.com/ijp/guildhall")
      (license license:gpl3+))))

(define-public guile-aspell
  (package
    (name "guile-aspell")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://lonelycactus.com/tarball/guile_aspell-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0vpk5xj9m9qc702z3khmkwhgpb949qbsyz8kw2qycda6qnxk0077"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-guilesitedir
                    (lambda _
                      (substitute* "Makefile.in"
                        (("^guilesitedir =.*$")
                         "guilesitedir = \
$(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
                      #t))
                  (add-before 'build 'set-libaspell-file-name
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let ((aspell (assoc-ref inputs "aspell")))
                        (substitute* "aspell.scm"
                          (("\"libaspell\\.so\"")
                           (string-append "\"" aspell
                                          "/lib/libaspell\"")))
                        #t))))))
    (native-inputs (list pkg-config))
    (inputs (list guile-2.2 aspell))
    (home-page "https://github.com/spk121/guile-aspell")
    (synopsis "Spell-checking from Guile")
    (description
     "guile-aspell is a Guile Scheme library for comparing a string against a
dictionary and suggesting spelling corrections.")
    (license license:gpl3+)))

(define-public guile-avatar
  (let ((commit "c2860952fd09ecc878c3d4f1ee2f1678668fbb7a")
        (revision "0"))
    (package
      (name "guile-avatar")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://codeberg.org/lechner/guile-avatar")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dq9zcx4q3m5s3cpixq4zvlmdrlcc626mpqb7nmmj7vnda8k717f"))))
      (build-system guile-build-system)
      (arguments
       (list #:source-directory "scm"))
      (inputs (list guile-3.0))
      (propagated-inputs (list guile-hashing))
      (home-page "https://codeberg.org/lechner/guile-avatar")
      (synopsis "Get Libravatar URLs from an email address")
      (description
       "@code{guile-avatar} helps you to determine avatars (or profile
pictures) for email addresses using the Libravatar specification.")
      (license license:agpl3+))))

(define-public guile2.0-bash
  ;; This project is currently retired.  It was initially announced here:
  ;; <https://lists.gnu.org/archive/html/guile-user/2015-02/msg00003.html>.
  (let ((commit "1eabc563ca5692b3e08d84f1f0e6fd2283284469")
        (revision "0"))
    (package
      (name "guile2.0-bash")
      (version (string-append "0.1.6-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (commit commit)
                      (url "https://git.sr.ht/~kaction/guile-bash")))
                (sha256
                 (base32
                  "097vny990wp2qpjij6a5a5gwc6fxzg5wk56inhy18iki5v6pif1p"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list (string-append "CPPFLAGS=-I" ; match bash.pc
                               (assoc-ref %build-inputs "bash:include")
                               "/include/bash/include")
                ;; The '.a' file is useless.
                "--disable-static"
                ;; Install 'lib/bash' as Bash 4.4 expects.
                (string-append "--libdir=" #$output "/lib/bash"))))
      (native-inputs
       (list autoconf
             automake
             bash                    ; with loadable module support, for tests
             gettext-minimal         ; for AC_LIB_LINKFLAGS_FROM_LIBS
             libtool
             pkg-config))
      (inputs `(("guile" ,guile-2.0)
                ("bash:include" ,bash "include")))
      (home-page "https://git.sr.ht/~kaction/guile-bash")
      (synopsis "Extend Bash using Guile")
      (description
       "Guile-Bash provides a shared library and set of Guile modules,
allowing you to extend Bash in Scheme.  Scheme interfaces allow you to access
the following aspects of Bash:

@itemize
@item aliases;
@item setting and getting Bash variables;
@item creating dynamic variables;
@item creating Bash functions with a Scheme implementation;
@item reader macro for output capturing;
@item reader macro for evaluating raw Bash commands.
@end itemize

To enable it, run:

@example
enable -f ~/.guix-profile/lib/bash/libguile-bash.so scm
@end example

and then run @command{scm example.scm}.")
      (license license:gpl3+))))

(define-public guile-bash
  (package
    (inherit guile2.0-bash)
    (name "guile-bash")
    (inputs
     (modify-inputs (package-inputs guile2.0-bash)
       (replace "guile" guile-3.0-latest)))
    (arguments
     (substitute-keyword-arguments (package-arguments guile2.0-bash)
       ;; XXX The tests succeed with Guile 2.0 but fail with 3.0.
       ((#:tests? _ #f) #f)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'install 'install-guile
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (copy-recursively
                 (string-append (assoc-ref outputs "out")
                                (assoc-ref inputs "guile") "/share")
                 (string-append (assoc-ref outputs "out") "/share"))))))))))

(define-public guile-8sync
  (let ((commit "183b4f02e68279d4984e79b79e06bfcf1861fcbf") (revision "0"))
    (package
      (name "guile-8sync")
      (version (git-version "0.4.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (commit commit)
                      (url "https://git.savannah.gnu.org/git/8sync.git")))
                (sha256
                 (base32
                  "0r22kxasv1zqnf1ykzyx6c226qxn1wgjb1gc54526bid24x508ij"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake guile-3.0 pkg-config texinfo))
      (arguments
       (list #:make-flags
             #~(list "GUILE_AUTO_COMPILE=0")))
      (home-page "https://gnu.org/s/8sync/")
      (synopsis "Asynchronous actor model library for Guile")
      (description
       "GNU 8sync (pronounced \"eight-sync\") is an asynchronous programming
library for GNU Guile based on the actor model.")
      (properties '((upstream-name . "8sync")))
      (license license:lgpl3+))))

(define guile-8sync-for-pubstrate
  (let ((commit "7972787723d08a491379b63e6e5dc1cc6a3fac87")
        (revision "0"))
    (package
      (inherit guile-8sync)
      (name "guile-8sync-for-pubstrate")
      (version (git-version "0.4.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (commit commit)
                      (url "https://git.savannah.gnu.org/git/8sync.git")))
                (sha256
                 (base32
                  "0m3k3cizi89frnw58dws3g4jcssck6jf1ahpadxxg3ncclqzad8r"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 '(substitute* "Makefile.am"
                    (("2.2") "3.0")))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        '(list "GUILE_AUTO_COMPILE=0")
        #:phases
        '(modify-phases %standard-phases
           ;; See commit ee371103855e5bfe8aae3debe442a24c6353e172
           (add-after 'unpack 'fix-srfi64-tests
             (lambda _
               (substitute* '("tests/test-actors.scm"
                              "tests/test-rmeta-slot.scm")
                 (("\\(test-exit\\)") "")
                 (("\\(test-end.*" m)
                  (string-append "(test-exit)" m))))))))
      (native-inputs (list autoconf automake guile-3.0 pkg-config texinfo))
      (propagated-inputs (list guile-fibers)))))

(define-public guile-daemon
  (package
    (name "guile-daemon")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "08gaqrgjlly9k5si72vvpbr4xhq5v52l5ma5y6a7spid5dd057cy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-3.0))
    (home-page "https://github.com/alezost/guile-daemon")
    (synopsis "Evaluate code in a running Guile process")
    (description
     "Guile-Daemon is a small Guile program that loads your initial
configuration file, and then reads and evaluates Guile expressions that
you send to a FIFO file.")
    (license license:gpl3+)))

(define-public guile-dsv
  (package
    (name "guile-dsv")
    (version "0.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/guile-dsv")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iavc1dg1899v519hvbzcmvdc16rahcwwvj68jycqdc5px5z285i"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf
                         automake
                         pkg-config
                         texinfo
                         help2man
                         ;; needed when cross-compiling.
                         guile-3.0
                         guile-lib
                         guile-smc))
    (inputs (list bash-minimal guile-3.0))
    (propagated-inputs (list guile-lib guile-smc))
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:phases (modify-phases %standard-phases
                  (delete 'strip)
                  (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (guile-lib (assoc-ref inputs "guile-lib"))
                             (version (target-guile-effective-version))
                             (scm (string-append "/share/guile/site/" version))
                             (go (string-append "/lib/guile/" version
                                                "/site-ccache")))
                        (wrap-program (string-append bin "/dsv")
                          `("GUILE_LOAD_PATH" prefix
                            (,(string-append out scm) ,(string-append
                                                        guile-lib scm)))
                          `("GUILE_LOAD_COMPILED_PATH" prefix
                            (,(string-append out go) ,(string-append guile-lib
                                                       go))))) #t)))))
    (home-page "https://github.com/artyom-poptsov/guile-dsv")
    (synopsis "DSV module for Guile")
    (description
     "Guile-DSV is a GNU Guile module for working with the delimiter-separated
values (DSV) data format.  Guile-DSV supports the Unix-style DSV format and RFC 4180
style format.  Also Guile-DSV includes a console program named @code{dsv} that allows
to view and process DSV data, including such operations as delimiter change,
conversion from one DSV standard to another and printing the data as pseudographics
tables.")
    (license license:gpl3+)))

(define-public guile2.2-dsv
  (package
    (inherit guile-dsv)
    (name "guile2.2-dsv")
    (native-inputs (modify-inputs (package-native-inputs guile-dsv)
                     (replace "guile-smc" guile2.2-smc)
                     (replace "guile" guile-2.2)
                     (replace "guile-lib" guile2.2-lib)))
    (inputs (modify-inputs (package-inputs guile-dsv)
              (replace "guile"  guile-2.2)
              (replace "guile-lib" guile2.2-lib)))
    (propagated-inputs (modify-inputs (package-propagated-inputs guile-dsv)
                         (replace "guile-lib" guile2.2-lib)
                         (replace "guile-smc" guile2.2-smc)))))

(define-public guile-fibers
  (package
    (name "guile-fibers")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wingo/fibers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wvdi4l58f9a5c9wi3cdc9l1bniscsixb6w2zj86mch7j7j814lc"))
              (patches
               (search-patches "guile-fibers-libevent-32-bit.patch"
                               "guile-fibers-libevent-timeout.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list "GUILE_AUTO_COMPILE=0")
           #:phases
           (if (and (target-x86-64?) (not (target-hurd?)))
               #~%standard-phases
               #~(modify-phases %standard-phases
                   (add-before 'check 'disable-some-tests
                     (lambda _
                       ;; This test can take more than an hour on some systems.
                       (substitute* "tests/basic.scm"
                         ((".*spawn-fiber loop-to-1e4.*") ""))

                       ;; These tests can take more than an hour and/or segfault.
                       (substitute* "Makefile"
                         (("tests/speedup.scm") ""))

                       (when #$(target-aarch64?)
                         ;; The tests below have issues on aarch64 systems.
                         ;; They pass on an Apple M1 but take a very long time
                         ;; on a Hetzner aarch64 VM.  Skip them.
                         (substitute* "tests/basic.scm"
                           ((".*spawn-fiber-chain 5000000.*") ""))
                         (substitute* "tests/channels.scm"
                           ((".*assert-run-fibers-terminates .*pingpong.*") "")))))
                   #$@(if (%current-target-system)
                          #~((add-after 'unpack 'apply-cross-build-fix-patch
                               (lambda _
                                 (let ((patch-file
                                        #$(local-file
                                           (search-patch
                                            "guile-fibers-cross-build-fix.patch"))))
                                   (invoke "patch" "--force" "-p1" "-i"
                                           patch-file)))))
                          #~())))))
    (native-inputs
     (list texinfo pkg-config autoconf-2.71 automake libtool
           guile-3.0            ;for 'guild compile
           ;; Gettext brings 'AC_LIB_LINKFLAGS_FROM_LIBS'
           gettext-minimal))
    (inputs
     (append (list guile-3.0)           ;for libguile-3.0.so
             (if (target-hurd?)
                 (list libevent)
                 '())))
    (synopsis "Lightweight concurrency facility for Guile")
    (description
     "Fibers is a Guile library that implements a a lightweight concurrency
facility, inspired by systems like Concurrent ML, Go, and Erlang.  A fiber is
like a \"goroutine\" from the Go language: a lightweight thread-like
abstraction.  Systems built with Fibers can scale up to millions of concurrent
fibers, tens of thousands of concurrent socket connections, and many parallel
cores.  The Fibers library also provides Concurrent ML-like channels for
communication between fibers.

Note that Fibers makes use of some Guile 2.1/2.2-specific features and
is not available for Guile 2.0.")
    (home-page "https://github.com/wingo/fibers")
    (properties '((upstream-name . "fibers")))
    (license license:lgpl3+)))

(define-public guile-fibers-1.3 guile-fibers)

(define-public guile-fibers-1.1
  (package
    (inherit guile-fibers)
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wingo/fibers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name "guile-fibers" version))
              (sha256
               (base32
                "0ll63d7202clapg1k4bilbnlmfa4qvpjnsd7chbkka4kxf5klilc"))
              (patches
               (search-patches "guile-fibers-wait-for-io-readiness.patch"
                               "guile-fibers-epoll-instance-is-dead.patch"
                               "guile-fibers-fd-finalizer-leak.patch"))))
    (native-inputs
     (list texinfo pkg-config autoconf automake libtool
           guile-3.0            ;for 'guild compile
           ;; Gettext brings 'AC_LIB_LINKFLAGS_FROM_LIBS'
           gettext-minimal))
    (arguments
     (if (%current-target-system)
         (substitute-keyword-arguments (package-arguments guile-fibers)
           ((#:phases phases)
            #~(modify-phases #$phases
                (delete 'apply-cross-build-fix-patch))))
         (package-arguments guile-fibers)))
    (inputs
     (list guile-3.0))                            ;for libguile-3.0.so
    (supported-systems
     ;; This version requires 'epoll' and is thus limited to Linux-based
     ;; systems, which is fixed in 1.2.0:
     ;; <https://github.com/wingo/fibers/pull/53>.
     (filter (cut string-suffix? "-linux" <>) %supported-systems))))

(define-public guile-fibers-1.0
  (package
    (inherit guile-fibers-1.1)
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://wingolog.org/pub/fibers/fibers-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0vjkg72ghgdgphzbjz9ig8al8271rq8974viknb2r1rg4lz92ld0"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("search=\"2\\.2\"")
                     "search=\"3.0 2.2\""))

                  ;; Explicitly include system headers rather than relying on
                  ;; <libguile.h> to do it for us.
                  (substitute* "epoll.c"
                    (("#include.*libguile\\.h.*$" all)
                     (string-append "#include <unistd.h>\n"
                                    "#include <string.h>\n"
                                    all "\n")))

                  ;; Import (ice-9 threads) for 'current-processor-count'.
                  (substitute* "tests/channels.scm"
                    (("#:use-module \\(fibers\\)")
                     (string-append "#:use-module (fibers)\n"
                                    "#:use-module (ice-9 threads)\n")))
                  #t))
              (patches
               ;; fixes a resource leak that causes crashes in the tests
               (search-patches "guile-fibers-destroy-peer-schedulers.patch"))))
    (arguments
     '(;; The code uses 'scm_t_uint64' et al., which are deprecated in 3.0.
       #:configure-flags '("CFLAGS=-Wno-error=deprecated-declarations")
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'mode-guile-objects
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; .go files are installed to "lib/guile/X.Y/cache".
                      ;; This phase moves them to "…/site-ccache".
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib/guile"))
                             (old (car (find-files lib "^ccache$"
                                                   #:directories? #t)))
                             (new (string-append (dirname old)
                                                 "/site-ccache")))
                        (rename-file old new)
                        #t))))))))

(define-public guile2.2-fibers
  (package
    (inherit guile-fibers-1.1)
    (name "guile2.2-fibers")
    (inputs
     (modify-inputs (package-inputs guile-fibers-1.1)
       (replace "guile" guile-2.2)))
    (native-inputs
     (modify-inputs (package-native-inputs guile-fibers-1.1)
       (replace "guile" guile-2.2)))))

(define-public guile-fibers-next
  (let ((commit "96cd1f4d4639b5c0f0b2fb7ebfd29b339a368dcc")
        (revision "0"))
    (package
      (inherit guile-fibers)
      (name "guile-fibers-next")
      (version (git-version "1.3.1"
                            revision
                            commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/wingo/fibers")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kmsbdcpw2qdl20ihjsdcbw3nlii9f6zpkhhrwqmlyqi46hyq9xl")))))))

(define-public guile-filesystem
  (package
    (name "guile-filesystem")
    (version "0.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/lilyp/guile-filesystem")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0waiaxcha584d0dc15nvs6gxh4clrfm2bwjidjsbqajgb03l4ngm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/lilyp/guile-filesystem")
    (synopsis "Complementary library to Guile's built-in file system procedures")
    (description "@code{guile-filesystem} provides a set of utility functions,
that augment Guile's support for handling files and their names.")
    (license license:lgpl3+)))

(define-public guile2.2-filesystem
  (package
    (inherit guile-filesystem)
    (name "guile2.2-filesystem")
    (inputs (list guile-2.2))))

(define-public guile-swayer
  (package
    (name "guile-swayer")
    (version "0.4.1")
    (home-page "https://github.com/ebeem/guile-swayer")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ebeem/guile-swayer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14w126444jsbpqyhic1ibg1zwzmai9hlnzdk4gg1cj7fclq6vxpb"))))
    (native-inputs (list autoconf automake guile-3.0 pkg-config))
    (build-system gnu-build-system)
    (synopsis "Extensible Guile bindings for SwayWM")
    (description
     "This package provides extensible Guile bindings for the Sway window
manager.  It can be used to query Sway, assign keybindings and listen to
events in Guile.")
    (license license:expat)))

(define-public guile-taglib
  (let ((commit "c056ac9eb375459c53284aa20f35b0778cfa3cea")
        (version "0")
        (revision "0"))
    (package
      (name "guile-taglib")
      (version (git-version version revision commit))
      (home-page "https://github.com/sbarbit/guile-taglib")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sbarbit/guile-taglib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "07z65hqxjm6rd9cdk2b9dcxj8hgz7c9dg4iprr19jrvj4ymzrbff"))))
      (build-system guile-build-system)
      (native-inputs (list guile-3.0))
      (propagated-inputs (list taglib))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-taglib-path
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "taglib.scm"
                  (("libtag_c.so")
                   (search-input-file inputs "/lib/libtag_c.so"))))))))
      (synopsis "Guile bindings for the taglib library")
      (description
       "This package provides Guile bindings for the taglib C library.
It can be used to access and modify metadata for audio files.")
      (license license:gpl2+))))

(define-public guile-syntax-highlight
  (package
    (name "guile-syntax-highlight")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/"
                                  "guile-syntax-highlight/"
                                  "guile-syntax-highlight-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0q4nz10l66hx1lyf83qyhkkz1bi6i860662a7kslc4d61w08qnk9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-3.0))
    (synopsis "General-purpose syntax highlighter for GNU Guile")
    (description "Guile-syntax-highlight is a general-purpose syntax
highlighting library for GNU Guile.  It can parse code written in various
programming languages into a simple s-expression that can be converted to
HTML (via SXML) or any other format for rendering.")
    (home-page "https://dthompson.us/projects/guile-syntax-highlight.html")
    (license license:lgpl3+)))

(define-public guile2.2-syntax-highlight
  (package
    (inherit guile-syntax-highlight)
    (name "guile2.2-syntax-highlight")
    (inputs (list guile-2.2))))

(define-public guile-sjson
  (package
    (name "guile-sjson")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://dustycloud.org/misc/sjson-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "08sr16fg5cqvik3wblav6k4b6djc5ydhgfvxa49bc5bh1irqvrcn"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/dustyweb/guile-sjson")
    (synopsis "S-expression based json reader/writer for Guile")
    (description "guile-sjson is a json reader/writer for Guile.
It has a nice, simple s-expression based syntax.")
    (license license:lgpl3+)))

(define-public guile2.2-sjson
  (package
    (inherit guile-sjson)
    (name "guile2.2-sjson")
    (inputs (list guile-2.2))))

(define-public guile-scheme-json-rpc
  (package
    (name "guile-scheme-json-rpc")
    (version "0.4.5a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/rgherdt/scheme-json-rpc.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0356hm6phcfgvwvx3ys6b927v40jzb7qrfgvql7g78na24zp2cmi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'change-to-guile-dir
                    (lambda _
                      (chdir "guile"))))))
    (inputs (list guile-3.0))
    (propagated-inputs (list guile-srfi-145 guile-srfi-180))
    (native-inputs (list pkg-config))
    (synopsis "Library providing JSON-RPC capability for Guile Scheme")
    (description
     "This library implements parts of the
@uref{https://www.jsonrpc.org/specification,JSON-RPC specification}, allowing
for calling methods on remote servers by exchanging JSON objects.")
    (home-page "https://codeberg.org/rgherdt/scheme-json-rpc/")
    (license license:expat)))

(define-public guile-ares-rs
  (package
    (name "guile-ares-rs")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~abcdw/guile-ares-rs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "06fc5kbcniysqixadi54vv96hy8l4wx6hqcii134fkb1d93078lq"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:source-directory "src/guile"))
    ;; Remove guile-next dependency, when guile package get custom text port
    (inputs `(("guile" ,guile-next)))
    (propagated-inputs (list guile-fibers))
    (home-page "https://git.sr.ht/~abcdw/guile-ares-rs")
    (synopsis "Asynchronous Reliable Extensible Sleek RPC Server for Guile")
    (description "Asynchronous Reliable Extensible Sleek RPC Server for
 Guile.  It's based on nREPL protocol and can be used for programmable
 interactions with a running guile processes, for implementing REPLs, IDEs,
 test runners or other tools.")
    (license license:gpl3+)))

(define-public guile-squee
  (let ((commit "9f2609563fc53466e46d37c8d8d2fbcfce67b2ba")
        (revision "5"))
    (package
      (name "guile-squee")
      (version (string-append "0-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://notabug.org/cwebber/guile-squee.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0r322mfxx08siw656h7bm31rgzkchmp3yrgjpkc2d3qw286ilqi7"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "squee.scm"
                  (("dynamic-link \"libpq\"")
                   (string-append
                    "dynamic-link \""
                    (search-input-file inputs "/lib/libpq.so")
                    "\""))))))))
      (inputs
       (list postgresql))
      (native-inputs
       (list guile-3.0))
      (home-page "https://notabug.org/cwebber/guile-squee")
      (synopsis "Connect to PostgreSQL using Guile")
      (description
       "@code{squee} is a Guile library for connecting to PostgreSQL databases
using Guile's foreign function interface.")
      (license license:lgpl3+))))

(define-public guile2.2-squee
  (package
    (inherit guile-squee)
    (name "guile2.2-squee")
    (native-inputs (modify-inputs (package-native-inputs guile-squee)
                     (replace "guile" guile-2.2)))))

(define-public guile-colorized
  (package
    (name "guile-colorized")
    (version "0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/NalaGinrut/guile-colorized.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10mv8c63159r3qvwwdvsgnsvdg7nc2ghak85zapwqpv4ywrqp9zc"))))
    (build-system guile-build-system)
    (native-inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/NalaGinrut/guile-colorized")
    (synopsis "Colorized REPL for Guile")
    (description
     "Guile-colorized provides you with a colorized REPL for GNU Guile.")
    (license license:gpl3+)))

(define-public guile2.2-colorized
  (package
    (inherit guile-colorized)
    (name "guile2.2-colorized")
    (native-inputs (list guile-2.2))))

(define-public guile-pfds
  (package
    (name "guile-pfds")
    (version "0.3")
    (home-page "https://github.com/ijp/pfds")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "19y33wg94pf0n98dkfqd1zbw93fgky4sawxsxl6s3vyqwl0yi5vh"))
              (file-name (git-file-name name version))))
    (build-system guile-build-system)
    (arguments
     '(#:source-directory "src"
       #:compile-flags '("--r6rs" "-Wunbound-variable" "-Warity-mismatch")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-sources
                    ;; Initially reported here:
                    ;; https://github.com/ijp/pfds/pull/6, and merged into
                    ;; other projects such as IronScheme (see:
                    ;; https://github.com/IronScheme/pfds/pull/1).
                    (lambda _
                      (substitute* "hamts.sls"
                        (("subtrie-vector vector")
                         "subtrie-vector trie"))))
                  (add-after 'patch-sources 'move-files-around
                    (lambda _
                      ;; Move files under a pfds/ directory to reflect the
                      ;; module hierarchy.
                      (mkdir-p "src/pfds")
                      (for-each (lambda (file)
                                  (rename-file
                                   file (string-append "src/pfds/" file)))
                                '("bbtrees.sls"
                                  "deques"
                                  "deques.sls"
                                  "dlists.sls"
                                  "fingertrees.sls"
                                  "hamts.sls"
                                  "heaps.sls"
                                  "private"
                                  "psqs.sls"
                                  "queues"
                                  "queues.sls"
                                  "sequences.sls"
                                  "sets.sls")))))))
    (native-inputs
     (list guile-3.0))
    (synopsis "Purely functional data structures for Guile")
    (description
     "This package provides purely functional data structures written in R6RS
Scheme and compiled for Guile.  It has been tested with Racket, Guile 2,
Vicare Scheme and IronScheme.  Right now it contains:

@itemize
@item queues
@item deques
@item bbtrees
@item sets
@item dlists
@item priority search queues (PSQs)
@item finger trees
@item sequences
@item heaps
@item hash array mapped tries (HAMTs).
@end itemize\n")
    (license license:bsd-3)))

(define-public guile2.0-pg
  (package
    (name "guile2.0-pg")
    (version "0.49")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-pg/guile-pg-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1fizcqga96p9n2jjhi9nprhry20hg9wvcl5b8gya4vhzwz6qhysp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'install 'patch-src/Makefile
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/Makefile"
               (("\\/share\\/guile\\/site") "/share/guile/site/2.0"))
             #t)))))
    (native-inputs
     (list procps ; fake-cluster-control uses ps
           guile-2.0 postgresql))
    (inputs
     (list guile-2.0 postgresql))
    (home-page "https://www.nongnu.org/guile-pg/")
    (synopsis "Guile modules for accessing PostgreSQL")
    (description
     "Guile-PG is a collection of modules for Guile allowing access to the
PostgreSQL RDBMS from Scheme programs.

This has been tested against PostgreSQL 10 through 13, but currently only
works with Guile 1.4.x to 2.0.x.")
    (license license:gpl3+)))

(define-public guile-prometheus
  (let ((commit "e63335e64a1f63967b94ba6dd97889f9f565ca48")
        (revision "7"))
    (package
    (name "guile-prometheus")
    (version (git-version "0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.cbaines.net/git/guile/prometheus")
                    (commit commit)))
              (sha256
               (base32
                "0f8rykqx3mdbi7mgvvanx65i9gn5wmb768vlzrbg002v38284bf2"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config autoconf automake guile-3.0))
    (inputs
     (list guile-3.0))
    (home-page "https://git.cbaines.net/guile/prometheus")
    (synopsis "Prometheus client library for Guile")
    (description
     "This Guile library provides instrumentation code intended to be used
with the Prometheus time series service.  Counter, gauge and histogram metric
types are supported.")
    (license license:gpl3+))))

(define-public guile2.2-pfds
  (package
    (inherit guile-pfds)
    (name "guile2.2-pfds")
    (native-inputs (list guile-2.2))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-pfds)
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'work-around-guile-bug)
           (add-after 'move-files-around 'sls->scm
             (lambda _
               ;; In Guile <= 2.2.4, there's no way to tell 'guild
               ;; compile' to accept the ".sls" extension.  So...
               (for-each (lambda (file)
                           (rename-file file
                                        (string-append
                                         (string-drop-right file 4)
                                         ".scm")))
                         (find-files "." "\\.sls$"))
               #t))))))))

(define-public guile-aa-tree
  (package
    (name "guile-aa-tree")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-aa-tree/guile-aa-tree-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0044c105r3q9vpl17pv3phl1b79kjm1llhkakqgiasixyav01blh"))))
    (build-system guile-build-system)
    (inputs (list guile-3.0))
    (arguments
     (list
      #:scheme-file-regexp "^aa-tree\\.scm"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install-documentation 'check
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((guile      #$(this-package-input "guile"))
                     (effective  (target-guile-effective-version guile))
                     (go-dir     (string-append #$output "/lib/guile/"
                                                effective "/site-ccache/")))
                (invoke (search-input-file inputs "/bin/guile")
                        "--no-auto-compile"
                        "-C" go-dir
                        "-c" (string-append
                              "(load \"" (getcwd) "/test-aa-tree.scm\")"))))))))
    ;; https://savannah.nongnu.org/projects/guile-aa-tree
    (home-page "https://qlfiles.net/guile-aa-tree/")
    (synopsis "AA tree data structure for Guile")
    (description
     "This package provides an implementation of @dfn{AA trees}, a
self-balancing binary tree data structure, for Guile.  It ensure @math{O(log
n)} worst case performance for core operations.  The module provides
non-mutating insert, delete, and search operations, with support for
convenient nested tree operations.")
    (license license:gpl3+)))

(define-public guile-algorithms
  (package
    (name "guile-algorithms")
    (version "0.1")
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://git@git.sr.ht/~filiplajszczak/guile-algorithms")
                 (commit (string-append "v" version))))
          (file-name (git-file-name name version))
          (sha256
            (base32
             "1a4ffnnhw92gqphjji5ajy3xfaqzww7xv3h8p82gkawx0rqvj5ni"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake pkg-config texinfo))
    (inputs (list guile-3.0))
    (synopsis "Guile port of racket-algorithms")
    (description
     "Guile port of @url{https://docs.racket-lang.org/algorithms/index.html,
racket-algorithms}, a package containing useful algorithms borrowed from other
programming languages).")
    (home-page "https://guile-algorithms.lajszczak.dev/")
    (license license:gpl3+)))

(define-public guile-aws
  (let ((commit "f32bea12333e1054b97ab50e58a72636edabb5b7")
        (revision "1"))
    (package
      (name "guile-aws")
      (version (git-version "0.1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/guile-aws.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0z2mrjw1dry14vjqsh9xi199bavlmy6cajshnv015n7p5my0cx9z"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list guile-3.0))
      (propagated-inputs
       (list guile-json-4 guile-gcrypt))
      (home-page "https://git.elephly.net/software/guile-aws.git")
      (synopsis "Scheme DSL for the AWS APIs")
      (description
       "This package provides a DSL for a number of @dfn{Amazon Web
Services} (AWS) APIs, including EFS, EC2, Route53, and more.  Guile AWS uses
the Guile compiler tower to generate the DSL from AWS JSON specifications.")
      (license license:gpl3+))))

(define-public guile-mqtt
  (package
    (name "guile-mqtt")
    (version "0.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/mdjurfeldt/" name
                           "/releases/download/v" version
                           "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "16a3r6yk41yskwv4qbkrsi0f5rvc7aw2s5di74i8y89j1x9yp9zs"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list "GUILE_AUTO_COMPILE=0")))
    (native-inputs (list guile-3.0 pkg-config))
    (inputs (list mosquitto))
    (home-page "https://github.com/mdjurfeldt/guile-mqtt")
    (synopsis "Guile bindings for the libmosquitto library")
    (description
     "This package provides Guile bindings for the libmosquitto MQTT client library.
The bindings are written in GOOPS, and the user can extend the client class by
inheritance.")
    (license license:lgpl3+)))

(define-public guile-simple-zmq
  (let ((commit "d25d1865e3378d93c44e2b4f5246a70b078a489d")
        (revision "11"))
    (package
      (name "guile-simple-zmq")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jerry40/guile-simple-zmq")
               (commit commit)))
         (sha256
          (base32 "1aq1s0f0z5g6qsv9jqr0663qv4rwxd9j1pmg1g8v6rl09xb8g8lp"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:make-flags
        #~'("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
      (native-inputs (list autoconf automake pkg-config guile-3.0))
      (inputs (list zeromq))
      (home-page "https://github.com/jerry40/guile-simple-zmq")
      (synopsis "Guile wrapper over ZeroMQ library")
      (description
       "This package provides a Guile programming interface to the ZeroMQ
messaging library.")
      (license license:gpl3+))))

(define-public guile2.2-simple-zmq
  (package
    (inherit guile-simple-zmq)
    (name "guile2.2-simple-zmq")
    (native-inputs (modify-inputs (package-native-inputs guile-simple-zmq)
                     (replace "guile" guile-2.2)))))

(define-public jupyter-guile-kernel
  (let ((commit "f25fb90b95529b17a006a807bd04e6aee12ea304")
        (revision "2"))
    (package
      (name "jupyter-guile-kernel")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/jerry40/guile-kernel")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0zr1fasdb2yv9kn21yll993y9higqss4jnfs030ndhjb93raa9sr"))))
      (build-system guile-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-after 'unpack 'set-openssl-file-name
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; Record the absolute file name of the 'openssl'
                        ;; command.
                        (substitute* "src/hmac.scm"
                          (("openssl")
                           (search-input-file inputs "/bin/openssl")))))

                    ;; XXX: The code uses 'include' to include its own source
                    ;; files, and "-L src" isn't enough in this case.
                    (add-before 'build 'chdir
                      (lambda _ (chdir "src") #t))
                    (add-after 'build 'chdir-back
                      (lambda _ (chdir "..") #t))

                    (add-after 'chdir-back 'install-kernel
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (let* ((out   (assoc-ref outputs "out"))
                               (json  (assoc-ref inputs "guile-json"))
                               (zmq   (assoc-ref inputs "guile-simple-zmq"))
                               (deps  (list json zmq))
                               (dir   (string-append
                                       out "/share/jupyter/kernels/guile"))
                               (effective (target-guile-effective-version)))
                          ;; Install kernel.
                          (install-file "src/kernel.json" dir)

                          ;; Fix hard-coded file name in the kernel.
                          (substitute* (string-append dir "/kernel.json")
                            (("/usr/local/.*/guile-jupyter-kernel.scm")
                             (string-append out "/share/guile/site/"
                                            (target-guile-effective-version)
                                            "/guile-jupyter-kernel.scm"))
                            (("\"guile\"")
                             (string-append "\"" (assoc-ref inputs "guile")
                                            "/bin/guile\""))
                            (("-s")
                             ;; Add '-L' and '-C' flags so that the kernel
                             ;; finds its dependencies.
                             (let ((-L (map (lambda (item)
                                              (string-append "\"" item
                                                             "/share/guile/site/"
                                                             effective "\""))
                                            deps))
                                   (-C (map (lambda (item)
                                              (string-append "\"" item
                                                             "/lib/guile/"
                                                             effective
                                                             "/site-ccache\""))
                                            deps)))
                               (string-append "--no-auto-compile\""
                                              (string-join -L ", \"-L\", "
                                                           'prefix)
                                              (string-join -C ", \"-C\", "
                                                           'prefix)
                                              ", \"-s"))))
                          #t))))))
      (inputs
       (list openssl guile-3.0 guile-json-3 guile-simple-zmq))
      (synopsis "Guile kernel for the Jupyter Notebook")
      (description
       "This package provides a Guile 2.x kernel for the Jupyter Notebook.  It
allows users to interact with the Guile REPL through Jupyter.")
      (home-page "https://github.com/jerry40/guile-kernel")
      (license license:gpl3+))))

(define-public guile-sparql
  (package
   (name "guile-sparql")
   (version "0.0.8")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/roelj/guile-sparql/releases/download/"
                  version "/guile-sparql-" version ".tar.gz"))
            (sha256
             (base32 "1jf4972f9fpm0rd865xpnc9mzl3xv6vhfnp0iygadydy905z9nln"))))
   (build-system gnu-build-system)
   (native-inputs
    (list pkg-config))
   (inputs
    (list guile-3.0))
   (home-page "https://github.com/roelj/guile-sparql")
   (synopsis "SPARQL module for Guile")
   (description "This package provides the functionality to query a SPARQL
endpoint.  Additionally, it provides an interface to write SPARQL queries
using S-expressions.")
   (license license:gpl3+)))

(define-public guile-debbugs
  (package
    (name "guile-debbugs")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/guile-debbugs/guile-debbugs-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cc63nw3xdfjrfk8c58r6d5lidmfq5cpqcy32yd5xp81yccprvn9"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list guile-email))
    (native-inputs
     (list guile-3.0 pkg-config))
    (home-page "https://savannah.gnu.org/projects/guile-debbugs/")
    (synopsis "Guile interface to the Debbugs bug tracking service")
    (description
     "This package provides a Guile library to communicate with a Debbugs bug
tracker's SOAP service, such as @url{https://bugs.gnu.org}.")
    (license license:gpl3+)))

(define-public guile-email
  (package
    (name "guile-email")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://guile-email.systemreboot.net/releases/"
                           "guile-email-" version ".tar.lz"))
       (sha256
        (base32
         "1qcrnzzhmvc8py1nybdznzrgqhlppazas0sbmm6zqjb6pd2kqqx0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list lzip texinfo))
    (inputs
     (list guile-3.0))
    (arguments
     (list #:make-flags #~(list (string-append "prefix=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (home-page "https://guile-email.systemreboot.net")
    (synopsis "Guile email parser")
    (description "guile-email is a collection of email utilities implemented
in pure guile.  It supports parsing MIME (Multipurpose Internet Mail
Extensions) compliant email messages and reading emails from the mbox
format.")
    (license license:agpl3+)))

(define-public guile2.2-email
  (package
    (inherit guile-email)
    (name "guile2.2-email")
    (inputs (modify-inputs (package-inputs guile-email)
              (replace "guile" guile-2.2)))
    (arguments
     (substitute-keyword-arguments (package-arguments guile-email)
       ((#:make-flags make-flags '())
        #~(cons "guile_effective_version=2.2"
                #$make-flags))))))

(define-public guile-newra
  ;; There has been no release let.
  (let ((commit "266e72ef433cab44f60f8595e2435247b225d457")
        (revision "0"))
    (package
      (name "guile-newra")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://notabug.org/lloda/guile-newra")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0g1fk6fp7ym54183bc9f6g5wqfazlkwwvb67swfi94j4sns0l9dk"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "mod"
        #:compile-flags '(list "--r6rs")))
      ;; guile-3.0 fails to compile with --r6rs
      (inputs (list guile-3.0-latest))
      (home-page "https://notabug.org/lloda/guile-newra")
      (synopsis "Scheme replacement for Guile's array system")
      (description
       "guile-newra (newra) wants to replace the current (3.0) Guile array
system, which is almost entirely implemented in C.  The new implementation
should be at least as fast.")
      (license license:gpl3+))))

(define-public guile-newt
  (package
    (name "guile-newt")
    (version "0.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mothacehe/guile-newt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hbznqigdkyh0kdkpnkp7sz2qd0g5dvmibcdi1rki02zg78mzypv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (inputs
     (list guile-3.0 newt))
    (native-inputs
     (list autoconf automake pkg-config guile-3.0))
    (synopsis "Guile bindings to Newt")
    (description
     "This package provides bindings for Newt, a programming library for
color text mode, widget based user interfaces.  The bindings are written in pure
Scheme by using Guile’s foreign function interface.")
    (home-page "https://gitlab.com/mothacehe/guile-newt")
    (license license:gpl3+)))

(define-public guile2.2-newt
  (package
    (inherit guile-newt)
    (name "guile2.2-newt")
    (inputs (modify-inputs (package-inputs guile-newt)
              (replace "guile" guile-2.2)))))

(define-public guile-mastodon
  (let ((commit "0a94ae6bbca63d440eb3f6d7e636630aca6d2b52")
        (revision "1"))
    (package
      (name "guile-mastodon")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://framagit.org/prouby/guile-mastodon.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "17ic44jypv1yq296w8b4nm99189fdgmdw1pdx0172x97dicsf2j6"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake emacs-minimal pkg-config texinfo))
      (inputs
       (list guile-3.0 gnutls guile-json-4))
      (home-page "https://framagit.org/prouby/guile-mastodon")
      (synopsis "Guile Mastodon REST API module")
      (description "This package provides Guile modules to access the
@uref{https://docs.joinmastodon.org/api/, REST API of Mastodon}, a federated
microblogging service.")
      (license license:gpl3+))))

(define-public guile-parted
  (package
    (name "guile-parted")
    (version "0.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/mothacehe/guile-parted")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h9q5plmnc1zd5ikz2x538v4a8lmriai6yyfv53bk5vjls7mrf3r"))
              (modules '((guix build utils)))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0"))) ;to prevent guild warnings
    (native-inputs
     (list autoconf automake guile-3.0 guile-bytestructures pkg-config))
    (inputs
     (list guile-3.0 parted))
    (propagated-inputs
     (list guile-bytestructures))
    (synopsis "Guile bindings to GNU Parted")
    (description
     "This package provides bindings for GNU Parted library, a C library
allowing disk partition tables creation and manipulation.  The bindings are
written in pure Scheme by using Guile's foreign function interface.")
    (home-page "https://gitlab.com/mothacehe/guile-parted")
    (license license:gpl3+)))

(define-public guile2.2-parted
  (package
    (inherit guile-parted)
    (name "guile2.2-parted")
    (inputs (modify-inputs (package-inputs guile-parted)
              (replace "guile" guile-2.2)))
    (propagated-inputs
     `(("guile-bytestructures" ,guile2.2-bytestructures)))))

(define-public guile-xosd
  (package
    (name "guile-xosd")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "10r29bpyrsvjalnzkam2falj9k34lvxmch05zs606zp1nk93whp3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-cpath
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPATH"
                     (string-append
                      (assoc-ref inputs "guile") "/include/guile/3.0:"
                      (or (getenv "CPATH") "")))
             #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-3.0 libx11 libxext libxinerama xosd))
    (home-page "https://github.com/alezost/guile-xosd")
    (synopsis "XOSD bindings for Guile")
    (description
     "Guile-XOSD provides Guile bindings for @code{libxosd},
@uref{http://sourceforge.net/projects/libxosd/, the X On Screen Display
library}.")
    (license license:gpl3+)))

(define-public guile-yamlpp
  (package
    (name "guile-yamlpp")
    (version "0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/yorgath/guile-yamlpp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ik69y0vddg0myp0zdbkmklma0qkkrqzwlqwkij1zirklz6hl1ss"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list guile-3.0 yaml-cpp))
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_EXTENSIONS_PATH")
            (files (list "lib/guile/3.0")))))
    (home-page "https://gitlab.com/yorgath/guile-yamlpp")
    (synopsis "Guile YAML reader/writer based on @code{yaml-cpp}")
    (description
     "A module for GNU Guile to read and write YAML files.  It works using
bindings to the @code{yaml-cpp} C++ library.")
    (license license:gpl3+)))

(define-public guile-dbi
  (package
    (name "guile-dbi")
    (version "2.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencog/guile-dbi")
                    (commit (string-append "guile-dbi-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123m4j82bi60s1v95pjh4djb7bh6zdwmljbpyg7zq8ni2gyal7lw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:make-flags '("LDFLAGS=\"-Wl,-allow-multiple-definition\"")
       #:configure-flags
       (list (string-append
              "--with-guile-site-dir=" %output "/share/guile/site/"
              (target-guile-effective-version (assoc-ref %build-inputs "guile"))))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             ;; The upstream Git repository contains all the code, so change
             ;; to the directory specific to guile-dbi.
             (chdir "guile-dbi")))
         (add-after 'install 'patch-extension-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dbi.scm (string-append out "/share/guile/site/"
                                            (target-guile-effective-version
                                             (assoc-ref inputs "guile"))
                                            "/dbi/dbi.scm"))
                    (ext (string-append out "/lib/libguile-dbi")))
               (substitute* dbi.scm (("libguile-dbi") ext))))))))
    (native-inputs
     (list autoconf automake libtool perl texinfo libltdl))
    (propagated-inputs
     (list guile-3.0))
    (synopsis "Guile database abstraction layer")
    (home-page "https://github.com/opencog/guile-dbi")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  Database programming with guile-dbi is generic in that the same
programming interface is presented regardless of which database system is used.
It currently supports MySQL, Postgres and SQLite3.")
    (license license:gpl2+)
    (native-search-paths
     (list (search-path-specification
            (variable "GUILE_DBD_PATH")
            (files '("lib")))))))

(define-public guile-dbd-sqlite3
  (package
    (inherit guile-dbi)
    (name "guile-dbd-sqlite3")
    (arguments
     (substitute-keyword-arguments (package-arguments guile-dbi)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbd-sqlite3.
               (chdir "guile-dbd-sqlite3")))
           (delete 'patch-extension-path)))))
    (inputs
     (list sqlite zlib))
    (native-inputs
     (modify-inputs (package-native-inputs guile-dbi)
       (prepend guile-dbi ; only required for headers
                pkg-config)))
    (synopsis "Guile DBI driver for SQLite")
    (description
     "guile-dbi is a library for Guile that provides a convenient interface to
SQL databases.  This package implements the interface for SQLite.")))

(define-public guile-dbd-postgresql
  (package
    (inherit guile-dbi)
    (name "guile-dbd-postgresql")
    (arguments
     (substitute-keyword-arguments (package-arguments guile-dbi)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbd-mysql.
               (chdir "guile-dbd-postgresql")))
           (add-after 'chdir 'patch-src
             (lambda _
               (substitute* "src/guile-dbd-postgresql.c"
                 (("postgresql/libpq-fe\\.h") "libpq-fe.h"))))
           (delete 'patch-extension-path)))))
    (inputs
     (list postgresql zlib))
    (native-inputs
     (modify-inputs (package-native-inputs guile-dbi)
       (prepend guile-dbi ; only required for headers
                )))
    (synopsis "Guile DBI driver for PostgreSQL")
    (description
     "@code{guile-dbi} is a library for Guile that provides a convenient
interface to SQL databases.  This package implements the interface for
PostgreSQL.")))

(define-public guile-dbd-mysql
  (package
    (inherit guile-dbi)
    (name "guile-dbd-mysql")
    (arguments
     (substitute-keyword-arguments (package-arguments guile-dbi)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'chdir
             (lambda _
               ;; The upstream Git repository contains all the code, so change
               ;; to the directory specific to guile-dbd-mysql.
               (chdir "guile-dbd-mysql")))
           (add-after 'chdir 'patch-src
             (lambda _
               (substitute* "configure.ac"
                 (("mariadbclient") "mariadb"))
               (substitute* "src/guile-dbd-mysql.c"
                 (("<mariadb/") "<mysql/"))))
           (delete 'patch-extension-path)))))
    (inputs
     (list `(,mariadb "dev")
           `(,mariadb "lib") zlib))
    (native-inputs
     (modify-inputs (package-native-inputs guile-dbi)
       (prepend guile-dbi ; only required for headers
                )))
    (synopsis "Guile DBI driver for MySQL")
    (description "@code{guile-dbi} is a library for Guile that provides a
convenient interface to SQL databases.  This package implements the interface
for MySQL.")
    (license license:gpl2+)))

(define-public guile-lmdb
  (let ((commit "56a986f5db5b70b6bec3ba3e6c161267dd8fda29")
        (revision "2"))
    (package
      (name "guile-lmdb")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/aartaka/guile-lmdb")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pzqgdm0dlz3v7nac1akpxrmxgbi2ycyxs7fnn3kba65424vigm7"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "modules"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'substitute-lmdb-so
              (lambda _
                (let ((lmdb (string-append
                             #$(this-package-input "lmdb") "/lib/liblmdb.so")))
                  (substitute* "modules/lmdb/lmdb.scm"
                    (("liblmdb.so") lmdb))))))))
      (native-inputs (list guile-3.0))
      (inputs (list guile-3.0 lmdb))
      (home-page "https://github.com/aartaka/guile-lmdb")
      (synopsis "Bindings for Lightning Memory-Mapped Database in Guile")
      (description "This package provides a Scheme wrapper around liblmdb.so.
Most names are the same as LMDB ones, except for prefix absence.
Several conveniences are added on top:
@itemize
@item @code{call-with-env-and-txn}, @code{call-with-cursor}, and
@code{call-with-wrapped-cursor} helpers and respective @code{with-} macros.
@item @code{for-cursor} procedure for cursor iteration.
@item @code{val} and @code{stat} types.
@item @code{set-compare!} and @code{set-dupsort!} to configure entry
sorting.
@item Error signaling instead of integer return values.
@end itemize")
      (license license:gpl3+))))

(define-public guile-config
  (package
    (name "guile-config")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/a-sassmannshausen/guile-config")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32
                "0s708k6qnk9155bjrcy1f1v7lqhlpaj4mjip46sr3iw85hca92wz"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs (list guile-3.0))
    (synopsis
     "Guile application configuration parsing library")
    (description
     "Guile Config is a library providing a declarative approach to
application configuration specification.  The library provides clean
configuration declaration forms, and processors that take care of:
configuration file creation; configuration file parsing; command-line
parameter parsing using getopt-long; basic GNU command-line parameter
generation (--help, --usage, --version); automatic output generation for the
above command-line parameters.")
    (home-page
     "https://gitlab.com/a-sassmannshausen/guile-config")
    (license license:gpl3+)))

(define-public guile2.2-config
  (package
    (inherit guile-config)
    (name "guile2.2-config")
    (inputs (modify-inputs (package-inputs guile-config)
              (replace "guile" guile-2.2)))))

(define-public guile-hall
  ;; There are many unreleased bug fixes; use the latest commit for now.
  (let ((commit "7558ba906d4281a5b825e3c1c87f2810312414b6")
        (revision "1"))
    (package
      (name "guile-hall")
      (version (git-version "0.4.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/a-sassmannshausen/guile-hall")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sqm6nyzc37p0xgjj21m9dar2iqik9gfwlcacp2v6y10lh2f1yps"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:modules `(((guix build guile-build-system)
                     #:select
                     (target-guile-effective-version))
                    ,@%default-gnu-modules)
        #:phases
        (with-imported-modules `((guix build guile-build-system)
                                 ,@%default-gnu-imported-modules)
          #~(modify-phases %standard-phases
              (add-after 'install 'hall-wrap-binaries
                (lambda* (#:key inputs #:allow-other-keys)
                  (let* ((version (target-guile-effective-version))
                         (site-ccache (string-append "/lib/guile/"
                                                     version "/site-ccache"))
                         (site (string-append "/share/guile/site/" version))
                         (dep-path
                          (lambda (env path)
                            (list env ":" 'prefix
                                  (cons (string-append #$output path)
                                        (map (lambda (input)
                                               (string-append
                                                (assoc-ref inputs input)
                                                path))
                                             (list "guile-config"
                                                   "guile-lib"))))))
                         (bin (string-append (ungexp output) "/bin/")))
                    (wrap-program (string-append bin "hall")
                      (dep-path "GUILE_LOAD_PATH" site)
                      (dep-path "GUILE_LOAD_COMPILED_PATH" site-ccache)))))))))
      (native-inputs
       (list autoconf
             automake
             gettext-minimal
             guile-3.0
             pkg-config
             texinfo))
      (inputs
       (list bash-minimal
             guile-3.0
             guile-config
             guile-lib))
      (propagated-inputs
       (list guile-config))
      (synopsis "Guile project tooling")
      (description
       "Hall is a command-line application and a set of Guile libraries that
allow you to quickly create and publish Guile projects.  It allows you to
transparently support the GNU build system, manage a project hierarchy &
provides tight coupling to Guix.")
      (home-page "https://gitlab.com/a-sassmannshausen/guile-hall")
      (license license:gpl3+))))

(define-public guile-ics
  (package
    (name "guile-ics")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artyom-poptsov/guile-ics")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zxclhyrsbp9v6sj36kmphiwqhb06rcm1zjskg5091py8361wjd6"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (delete 'strip))))
    (native-inputs
     (list autoconf
           automake
           texinfo
           gettext-minimal ;Gettext brings 'AC_LIB_LINKFLAGS_FROM_LIBS'.
           help2man
           pkg-config
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib
           guile-smc))
    (inputs (list guile-3.0))
    (propagated-inputs (list guile-lib guile-smc guile-dsv))
    (home-page "https://github.com/artyom-poptsov/guile-ics")
    (synopsis "Guile parser library for the iCalendar format")
    (description
     "Guile-ICS is an iCalendar (RFC5545) and vCard (RFC6350) format parser
library written in pure Scheme.  The library can be used to read and write
iCalendar/vCadr data and convert the data from/to various formats.

The library is shipped with documentation in Info format and usage examples.")
    (license license:gpl3+)))

(define-public guile2.2-ics
  (package
    (inherit guile-ics)
    (name "guile2.2-ics")
    (native-inputs
     (modify-inputs (package-native-inputs guile-ics)
       (replace "guile" guile-2.2)
       (replace "guile-lib" guile2.2-lib)
       (replace "guile-smc" guile2.2-smc)))
    (inputs (list guile-2.2))
    (propagated-inputs (list guile2.2-lib guile2.2-dsv guile2.2-smc))))

(define-public guile-imanifest
  (let ((commit "ccd5a2111b008d778106f5595a3a585954d95d0")
        (revision "0"))
    (package
      (name "guile-imanifest")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.sr.ht/~brown121407/guile-imanifest")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0i5qllcrhdjhspyj7j9h4dc9y37d3cfbpackmybm3030qgfxqirf"))))
      (build-system guile-build-system)
      (native-inputs
       (list guile-3.0))
      (propagated-inputs
       (list guile-readline guile-colorized guix))
      (home-page "https://sr.ht/~brown121407/guile-imanifest")
      (synopsis "Interactive Guix manifests")
      (description "This package provides functions to generate Guix manifests
interactively.  It works by scanning an alist of package categories, to ask the
user which package sets would they like to install from it.")
      (license license:gpl3+))))

(define-public guile-wisp
  (package
    (name "guile-wisp")
    (version "1.0.12")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://hg.sr.ht/~arnebab/wisp")
                    (changeset (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0m5ssl4ngk2jl1zk0fnsss0asyvwanjaa5rrcksldqnh2ikcr4bm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build emacs-build-system) #:prefix emacs:)
                  (guix build utils)
                  (guix build emacs-utils)
                  (ice-9 rdelim)
                  (ice-9 popen))
       #:imported-modules (,@%default-gnu-imported-modules
                           (guix build emacs-build-system)
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "autoreconf" "-vif")))
         (add-before 'configure 'patch-/usr/bin/env
           (lambda _
             (substitute* "Makefile.in"
               (("/usr/bin/env bash") (which "bash")))))
         ;; auto compilation breaks, but if we set HOME to /tmp,
         ;; that works ok
         (add-before 'check 'auto-compile-hacky-workaround
           (lambda _ (setenv "HOME" "/tmp") #t))
         (add-after 'install 'install-go-files
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (module-dir (string-append out "/share/guile/site/"
                                               effective))
                    (object-dir (string-append out "/lib/guile/" effective
                                               "/site-ccache"))
                    (prefix     (string-length module-dir)))
               ;; compile to the destination
               (for-each (lambda (file)
                           (let* ((base (string-drop (string-drop-right file 4)
                                                     prefix))
                                  (go   (string-append object-dir base ".go")))
                             (invoke "guild" "compile" "-L" module-dir
                                     file "-o" go)))
                         (find-files module-dir "\\.scm$")))))
         (add-after 'unpack 'make-autoloads
           (assoc-ref emacs:%standard-phases 'make-autoloads))
         (add-after 'install 'install-emacs-files
           (assoc-ref emacs:%standard-phases 'install))
         (add-after 'install-emacs-files 'compile-emacs-files
           (assoc-ref emacs:%standard-phases 'build)))))
    (home-page "https://www.draketo.de/english/wisp")
    (inputs
     (list guile-3.0))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("emacs" ,emacs-minimal)
       ("python" ,python)
       ("pkg-config" ,pkg-config)))
    (synopsis "Whitespace to lisp syntax for Guile")
    (description "Wisp is a syntax for Guile which provides a Python-like
whitespace-significant language.  It may be easier on the eyes for some
users and in some situations.")
    (license (list license:gpl3+        ; the project as a whole
                   license:expat))))    ; the language spec (see also SRFI 119)

(define-public guile2.2-wisp
  (package
    (inherit guile-wisp)
    (name "guile2.2-wisp")
    (inputs (list guile-2.2))))

(define-public guile-udev
  (package
    (name "guile-udev")
    (version "0.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/guile-udev")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zvn7ph6sbz5q8jnbkrxxlbxlyf0j8q34hr4a2yxklvg29ya7sd3"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-extension-path
            (lambda _
              ;; Provide the absolute path of the guile-libudev extension to
              ;; ensure the dlopen call always succeeds.
              (substitute* (find-files "." "\\.scm")
                (("load-extension \"libguile-udev\"")
                 (format #f "load-extension \"~a/lib/libguile-udev.so\""
                         #$output)))))
          (delete 'check)               ;moved after install
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check)))))
    (native-inputs (list autoconf
                         automake
                         gettext-minimal
                         libtool
                         texinfo
                         pkg-config
                         which))
    (inputs (list guile-3.0 eudev))
    (home-page "https://github.com/artyom-poptsov/guile-udev")
    (synopsis "Guile bindings to libudev")
    (description "Guile-Udev provides GNU Guile bindings to libudev.")
    (license license:gpl3+)))

(define-public guile-sly
  (package
    (name "guile-sly")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/sly/sly-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1svzlbz2vripmyq2kjh0rig16bsrnbkwbsm558pjln9l65mcl4qq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    (("_guile_required_version=\"2.0.11\"")
                     "_guile_required_version=\"2\"")
                    (("ac_subst_vars='")
                     "ac_subst_vars='GUILE_EFFECTIVE_VERSION\n"))
                  (substitute* (find-files "." "Makefile.in")
                    (("moddir = .*$")
                     (string-append
                      "moddir = "
                      "$(prefix)/share/guile/site/@GUILE_EFFECTIVE_VERSION@\n"))
                    (("godir = .*$")
                     (string-append
                      "godir = "
                      "$(prefix)/lib/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-libfreeimage-prefix="
                            (assoc-ref %build-inputs "freeimage"))
             (string-append "--with-libgslcblas-prefix="
                            (assoc-ref %build-inputs "gsl")))))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list guile-sdl guile-opengl))
    (inputs
     (list guile-2.2 gsl freeimage mesa))
    (synopsis "2D/3D game engine for GNU Guile")
    (description "Sly is a 2D/3D game engine written in Guile Scheme.  Sly
features a functional reactive programming interface and live coding
capabilities.")
    (home-page "https://dthompson.us/projects/sly.html")
    (license license:gpl3+)))

(define* (g-golf-source #:key version hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://git.savannah.gnu.org/git/g-golf.git")
          (commit (string-append "v" version))))
    (file-name (git-file-name "g-golf" version))
    (hash hash)))

(define-public guile-g-golf
  (package
    (name "guile-g-golf")
    (version "0.8.1")
    (source
     (g-golf-source #:version version
                    #:hash
                    (content-hash
                     "044iidjd24cjncvx510ai46is9jxni72iz8pxyi34g4p7gbbcbi7")))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--with-guile-site=no")
      #:parallel-build? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-guile-site-directory
            (lambda _
              (substitute* "configure.ac"
                (("SITEDIR=.*$")
                 "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\";\n")
                (("SITECCACHEDIR=\"\\$libdir/g-golf/")
                 "SITECCACHEDIR=\"$libdir/"))))
          (add-before 'configure 'tests-work-arounds
            (lambda* (#:key inputs #:allow-other-keys)
              ;; In build environment, There is no /dev/tty
              (substitute* "test-suite/tests/gobject.scm"
                (("/dev/tty") "/dev/null"))))
          (add-before 'configure 'substitute-libs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (define (get lib)
                (search-input-file inputs (string-append "lib/" lib ".so")))

              (let* ((libgi      (get "libgirepository-1.0"))
                     (libglib    (get "libglib-2.0"))
                     (libgobject (get "libgobject-2.0"))
                     (libg-golf (string-append #$output "/lib/libg-golf")))
                (substitute* "g-golf/init.scm"
                  (("libgirepository-1.0") libgi)
                  (("libglib-2.0") libglib)
                  (("libgobject-2.0") libgobject)
                  (("\\(dynamic-link \"libg-golf\"\\)")
                   (format #f "~s"
                           `(catch #t
                              (lambda ()
                                (dynamic-link "libg-golf"))
                              (lambda _
                                (dynamic-link ,libg-golf))))))
                (setenv "GUILE_AUTO_COMPILE" "0")
                #t)))
          (add-before 'check 'start-xorg-server
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The test suite requires a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")
              #t)))))
    (inputs
     (list guile-3.0 guile-lib glib))
    (native-inputs
     (list autoconf
           automake
           texinfo
           gettext-minimal
           libtool
           pkg-config
           ;; required for tests
           gtk+
           xorg-server-for-tests))
    (propagated-inputs
     (list gobject-introspection))
    (home-page "https://www.gnu.org/software/g-golf/")
    (synopsis "Guile Object Library for GNOME")
    (description
     "G-Golf (Gnome: (Guile Object Library for)) is a library for developing
modern applications in Guile Scheme.  It comprises a direct binding to the
GObject Introspection API and higher-level functionality for importing Gnome
libraries and making GObject classes (and methods) available in Guile's
object-oriented programming system, GOOPS.

Note: Currently, when developing with G-Golf in @command{guix shell}, there is
a @uref{https://bugs.gnu.org/75157, grafts bug in Guix}.  To avoid it, use
Guix' @code{--no-grafts} option.  Guix packages that use @code{wrap-program}
are unaffected.")
    (license license:lgpl3+)))

(define-public g-golf
  ;; Use ‘guile-g-golf’ above in package inputs and other code.  This alias
  ;; exists as a compromise, see
  ;; https://lists.gnu.org/archive/html/guix-devel/2025-02/msg00296.html.
  (package
    (inherit guile-g-golf)
    (name "g-golf")))

(define-public guile2.2-g-golf
  (package
    (inherit guile-g-golf)
    (name "guile2.2-g-golf")
    (inputs
     (modify-inputs (package-inputs guile-g-golf)
       (replace "guile" guile-2.2)
       (replace "guile-lib" guile2.2-lib)))))

(define-public g-golf-gtk-4-examples
  (package
    (inherit guile-g-golf)
    (name "g-golf-gtk-4-examples")
    (arguments
     (list
      #:modules `(((guix build guile-build-system)
                   #:select
                   (target-guile-effective-version))
                  (srfi srfi-26)
                  ,@%default-gnu-modules)
      #:phases
      (with-imported-modules `((guix build guile-build-system)
                               ,@%default-gnu-imported-modules)
        #~(modify-phases %standard-phases
            (add-after 'unpack 'prepare-examples
              (lambda _
                (chdir "examples/gtk-4")
                ;; Re-use the existing Makefile for its wildcard syntax.
                (rename-file "Makefile.am" "Makefile")
                ;; Add a rule to install the examples.  We install to the
                ;; documentation directory where examples are usually located,
                ;; but we will later create a copy in /bin for `guix shell'.
                (let ((port (open-file "Makefile" "al")))
                  (format port "
prefix = ~a
bindir = $(prefix)/bin
examplesdir = $(prefix)/share/doc/g-golf/examples/gtk-4
.PHONY: install
install:
	mkdir -p $(bindir)
	mkdir -p $(examplesdir)/css
	mkdir -p $(examplesdir)/demos
	mkdir -p $(examplesdir)/images
	mkdir -p $(examplesdir)/ui
	for f in $(EXTRA_DIST); do      \\
	  cp $$f $(examplesdir)/$$f;    \\
	done
	install demos/libfpt.so $(examplesdir)/demos
" #$output)
                  (close-port port))))
            (delete 'configure)
            (replace 'build
              (lambda _
                ;; The layout-manager-2 example calls `make', GCC at run-time.
                ;; But since it would compile to the read-only /gnu/store, we
                ;; deviate by compiling in advance in the build phase,
                ;; ignoring failing `make' calls.  We do not propagate `make'.
                (with-directory-excursion "demos"
                  (when #$(%current-target-system)
                    (substitute* "Makefile"
                      (("^CC = gcc$")
                       (string-append "CC = " #$(cc-for-target) "\n"))))
                  (system* "make"))))
            ;; There are no tests for examples, but we do an installcheck phase,
            ;; which respects when #:tests? is turned off.  So delete 'check.
            (delete 'check)
            (add-before 'install 'patch-scm-files
              (lambda* (#:key inputs #:allow-other-keys)
                ;; `current-filename' calls in examples are broken.
                (map (lambda (binary)
                       (let ((installed-binary (string-append
                                                #$output "\
/share/doc/g-golf/examples/gtk-4/" binary)))
                         (substitute* binary
                           (("\\(current-filename\\)")
                            (string-append "\"" installed-binary "\""))
                           (("\\(getcwd\\)")
                            (string-append "\"" #$output "\
/share/doc/g-golf/examples/gtk-4\""))
                           (("^exec guile ")
                            (string-append
                             "exec " (search-input-file inputs "/bin/guile")
                             " ")))))
                     (map (cut string-drop <> 2) ;strip ./ prefix
                          (find-files "." (lambda (file stat)
                                        ;executables or .scm modules
                                            (or (= (stat:perms stat) #o755)
                                                (string-suffix? ".scm"
                                                                file))))))))
            (add-after 'install 'wrap-binaries
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((version (target-guile-effective-version))
                       (g-golf (assoc-ref inputs "guile-g-golf"))
                       (gcairo (assoc-ref inputs "guile-cairo-next"))
                       (adwaita-icons (assoc-ref inputs "adwaita-icon-theme"))
                       (scm (string-append "/share/guile/site/" version))
                       (go (string-append "/lib/guile/"
                                          version "/site-ccache"))
                       (binaries
                        (find-files "." (lambda (file stat) ;executables
                                          (= (stat:perms stat) #o755)))))
                  (map (lambda (binary)
                         (let ((installed-binary (string-append
                                                  #$output "/\
share/doc/g-golf/examples/gtk-4/" binary)))
                           (wrap-program installed-binary
                             `("GUILE_LOAD_PATH" prefix
                               (,(string-append g-golf scm)
                                ,(string-append gcairo scm)))
                             `("GUILE_LOAD_COMPILED_PATH" prefix
                               (,(string-append g-golf go)
                                ,(string-append gcairo go)))
                             `("GI_TYPELIB_PATH" prefix
                               (,(getenv "GI_TYPELIB_PATH")))
                             ;; Library path for libraries loaded by binaries.
                             `("LD_LIBRARY_PATH" prefix
                               (,(string-append gcairo "/lib")))
                             `("XDG_DATA_DIRS" suffix
                               (,(string-append adwaita-icons "/share"))))
                           ;; Also create a copy in /bin for `guix shell'.
                           (copy-file installed-binary
                                      (string-append #$output "/bin/" binary))))
                       binaries))))
            ;; Add installcheck to ensure nothing breaks.
            (add-after 'strip 'installcheck
              (lambda* (#:key inputs tests? #:allow-other-keys)
                (cond
                 ((not tests?)
                  (display "test suite not run\n"))
                 (#$(%current-target-system)
                  (display "cross-compiling; reftest skipped\n"))
                 (else
                  ;; Start an X server.
                  (system "Xvfb :1 &")
                  (setenv "DISPLAY" ":1")
                  (let* ((g-golf-drawing (string-append
                                          #$output "\
/share/doc/g-golf/examples/gtk-4/drawing-widget"))
                         (pid (spawn g-golf-drawing `(,g-golf-drawing)
                                     #:search-path? #f)))
                    (sleep 10) ;2s is enough on my machine
                    (display "Taking a screenshot with G-Golf.\n")
                    (system* "import" "-window" "root"
                             "drawing-widget.out.png")
                    (sleep 5) ;1s is enough on my machine
                    (kill pid SIGINT)
                    (waitpid pid))
                  (let* ((python (search-input-file inputs "/bin/python3"))
                         (pygobject-drawing #$(this-package-native-input
                                               "drawing-widget.py"))
                         (pid (spawn python `(,python ,pygobject-drawing)
                                     #:search-path? #f)))
                    (sleep 5) ;1s is enough on my machine
                    (display "Taking a screenshot with Pygobject.\n")
                    (system* "import" "-window" "root"
                             "drawing-widget.ref.png")
                    (sleep 5)
                    (kill pid SIGINT)
                    (waitpid pid))
                  (if (= (pk (system* "compare" "-metric" "AE" "-fuzz" "1%"
                                      "drawing-widget.out.png"
                                      "drawing-widget.ref.png"
                                      "drawing-widget.dif.png")) 0)
                      (display "All good; they look the same.\n")
                      (error "Reftest failed; screenshots differ."))))))))))
    (inputs
     (list adwaita-icon-theme
           bash-minimal
           gtk
           guile-3.0
           guile-cairo-next
           guile-g-golf))
    (native-inputs
     (list pkg-config
           which
           ;; For installcheck:
           imagemagick
           python-minimal
           python-pygobject
           xorg-server-for-tests
           ;; Python version of drawing-widget from
           ;; https://lists.gnu.org/archive/html/guile-user/2024-05/msg00032.html
           (origin
             (method url-fetch)
             (uri "\
https://lists.gnu.org/archive/html/guile-user/2024-05/txtT_80XuINsX.txt")
             (sha256
              (base32
               "07j2v159a3bb99i8kwbqrcgslcmhhnqa5ah53q2b9bdk8042grsx"))
             (file-name "drawing-widget.py"))))
    (propagated-inputs (list))
    (synopsis "G-Golf GTK 4 examples")
    (description
     "G-Golf port of (a subset of) the upstream @code{gtk4-demo} examples in
the @code{gtk:bin} Guix package output.  Run @command{guix edit
g-golf-gtk-4-examples} for inspiration how to wrap G-Golf applications when
writing a Guix package.")))

(define-public g-golf-adw-1-examples
  (package
    (inherit guile-g-golf)
    (name "g-golf-adw-1-examples")
    ;; XXX: Update version when we have a recent enough libadwaita.
    (version "0.8.0")
    (source
     (g-golf-source #:version version
                    #:hash
                    (content-hash
                     "14b6pjchra0axqifpm90m7jbxla2sarhd7bfhzqbn7d14b74sv2d")))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #f ;there are no tests for examples
      #:modules `(((guix build guile-build-system)
                   #:select
                   (target-guile-effective-version))
                  (guix build glib-or-gtk-build-system)
                  (srfi srfi-26)
                  ,@%default-gnu-modules)
      #:phases
      (with-imported-modules `((guix build guile-build-system)
                               ,@%default-gnu-imported-modules)
        ;; With above modules, %standard-phases would not be from
        ;; glib-or-gtk-build-system anymore:
        #~(modify-phases (@ (guix build glib-or-gtk-build-system)
                            %standard-phases)
            (add-after 'unpack 'prepare-examples
              (lambda _
                (chdir "examples/adw-1")
                ;; Re-use the existing Makefile for its wildcard syntax.
                (rename-file "Makefile.am" "Makefile")
                (substitute* "Makefile"
                  ;; Fix syntax error.
                  (("hello-world")
                   "hello-world \\"))
                ;; Add a rule to install the examples.  We install to the
                ;; documentation directory where examples are usually located,
                ;; but we will later create a copy in /bin for `guix shell'.
                (let ((port (open-file "Makefile" "al")))
                  (format port "
prefix = ~a
bindir = $(prefix)/bin
examplesdir = $(prefix)/share/doc/g-golf/examples/adw-1
.PHONY: install
install:
	mkdir -p $(bindir)
	mkdir -p $(examplesdir)/demo/icons
	mkdir -p $(examplesdir)/demo/pages
	for f in $(EXTRA_DIST); do      \\
	  cp -r $$f $(examplesdir)/$$f; \\
	done
	cp demo/g-resources $(examplesdir)/demo/g-resources
" #$output)
                  (close-port port))))
            (delete 'configure)
            (replace 'build
              (lambda _
                ;; Create files for adwaita-1-demo needed in install phase.
                (with-directory-excursion "demo"
                  (system* "make")
                  (system* "glib-compile-resources"
                           "--target" "g-resources"
                           "g-resources.xml"))))
            (add-before 'install 'patch-scm-files
              (lambda* (#:key inputs #:allow-other-keys)
                ;; `current-filename' calls in examples are broken.
                (map (lambda (binary)
                       (let ((installed-binary (string-append
                                                #$output "\
/share/doc/g-golf/examples/adw-1/" binary)))
                         (substitute* binary
                           (("\\(current-filename\\)")
                            (string-append "\"" installed-binary "\""))
                           (("^exec guile ")
                            (string-append
                             "exec " (search-input-file inputs "/bin/guile")
                             " ")))))
                     (map (cut string-drop <> 2) ;strip ./ prefix
                          (find-files "." (lambda (file stat)
                                        ;executables or .scm modules
                                            (or (= (stat:perms stat) #o755)
                                                (string-suffix? ".scm"
                                                                file))))))))
            (add-after 'install 'wrap-binaries
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((version (target-guile-effective-version))
                       (g-golf (assoc-ref inputs "guile-g-golf"))
                       (adwaita-icons (assoc-ref inputs "adwaita-icon-theme"))
                       (libadwaita-icons (assoc-ref inputs "libadwaita"))
                       (scm (string-append "/share/guile/site/" version))
                       (go (string-append "/lib/guile/"
                                          version "/site-ccache"))
                       (binaries
                        (find-files "." (lambda (file stat) ;executables
                                          (= (stat:perms stat) #o755)))))
                  (map (lambda (binary)
                         (let ((installed-binary (string-append
                                                  #$output "\
/share/doc/g-golf/examples/adw-1/" binary)))
                           (wrap-program installed-binary
                             `("GUILE_LOAD_PATH" prefix
                               (,(string-append g-golf scm)))
                             `("GUILE_LOAD_COMPILED_PATH" prefix
                               (,(string-append g-golf go)))
                             `("GI_TYPELIB_PATH" prefix
                               (,(getenv "GI_TYPELIB_PATH")))
                             `("GDK_PIXBUF_MODULE_FILE" =
                               (,(getenv "GDK_PIXBUF_MODULE_FILE")))
                             `("XDG_DATA_DIRS" suffix
                               (,(string-append #$output "/bin/demo")
                                ,(string-append adwaita-icons "/share")
                                ,(string-append libadwaita-icons "/share"))))))
                       binaries))))
            (add-after 'wrap-binaries 'copy-binaries
              (lambda _
                (copy-file (string-append
                            #$output "\
/share/doc/g-golf/examples/adw-1/demo/adwaita-1-demo")
                             (string-append
                              #$output "/bin/adwaita-1-demo"))
                                (copy-file (string-append
                            #$output "\
/share/doc/g-golf/examples/adw-1/hello-world")
                             (string-append
                              #$output "/bin/hello-world"))))))))
    (inputs
     (list adwaita-icon-theme
           bash-minimal
           libadwaita
           (librsvg-for-system)
           gtk
           guile-3.0
           guile-g-golf))
    (native-inputs (list `(,glib "bin") ;for glib-compile-resources
                         guile-3.0))
    (propagated-inputs (list))
    (synopsis "G-Golf Adw-1 examples")
    (description
     "G-Golf port of the upstream @code{adwaita-1-demo} example in the
@code{libadwaita} Guix package.  It adds one simple hello-world example as
well.  Run @command{guix edit g-golf-adw-1-examples} for inspiration how to
wrap G-Golf applications when writing a Guix package.")))

(define-public g-wrap
  (package
    (name "g-wrap")
    (version "1.9.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/g-wrap/g-wrap-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0ak0bha37dfpj9kmyw1r8fj8nva639aw5xr66wr5gd3l1rqf5xhg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list guile-2.2 guile-lib))
    (inputs
     (list libffi))
    (arguments
     `(#:configure-flags '("--disable-Werror")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* (find-files "." "^Makefile.in$")
                 (("guilemoduledir =.*guile/site" all)
                  (string-append all "/@GUILE_EFFECTIVE_VERSION@")))
               #t))))))
    (synopsis "Generate C bindings for Guile")
    (description "G-Wrap is a tool and Guile library for generating function
wrappers for inter-language calls.  It currently only supports generating Guile
wrappers for C functions.  Given a definition of the types and prototypes for
a given C interface, G-Wrap will automatically generate the C code that
provides access to that interface and its types from the Scheme level.")
    (home-page "https://www.nongnu.org/g-wrap/index.html")
    (license license:lgpl2.1+)))

(define-public guile-miniadapton
  (let ((commit "1b5749422304567c96ac5367f2221dda9eff5880")
        (revision "1"))
    (package
      (name "guile-miniadapton")
      (version (string-append "0-" revision "." (string-take commit 9)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/fisherdj/miniAdapton")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "09q51zkw2fypad5xixskfzw2cjhjgs5cswdp3i7cpp651rb3zndh"))))
      (build-system guile-build-system)
      (native-inputs
       (list guile-2.2))
      (home-page "https://github.com/fisherdj/miniAdapton")
      (synopsis "Minimal implementation of incremental computation in Guile
Scheme")
      (description "This package provides a complete Scheme implementation of
miniAdapton, which implements the core functionality of the Adapton system for
incremental computation (also known as self-adjusting computation).  Like
Adapton, miniAdapton allows programmers to safely combine mutation and
memoization.  miniAdapton is built on top of an even simpler system,
microAdapton.  Both miniAdapton and microAdapton are designed to be easy to
understand, extend, and port to host languages other than Scheme.")
      (license license:expat))))

(define-public guile-raw-strings
  (let ((commit "aa1cf783f2542811b473f797e12490920b779baa")
        (revision "0"))
    (package
      (name "guile-raw-strings")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lloda/guile-raw-strings")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1r2gx86zw5hb6byllra3nap3fw9p7q7rvdmg6qn9myrdxyjpns3l"))))
      (build-system guile-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'build 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "guile" "-L" "." "-s" "test.scm")))))))
      (native-inputs
       (list guile-3.0))
      (home-page "https://github.com/lloda/guile-raw-strings")
      (synopsis "Guile reader extension for `raw strings'")
      (description "This package provides A Guile reader extension for `raw
strings', it lets you write verbatim strings without having to escape double
quotes.")
      (license license:public-domain))))

(define-public guile-reader
  (package
    (name "guile-reader")
    (version "0.6.3")
    (source  (origin
               (method url-fetch)
               (uri (string-append "mirror://savannah/guile-reader/guile-reader-"
                                   version ".tar.gz"))
               (sha256
                (base32
                 "1fyjckmygkhq22lq8nqc86yl5zzbqd7a944dnz5c1f6vx92b9hiq"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config gperf))
    (inputs (list guile-3.0))
    (synopsis "Framework for building readers for GNU Guile")
    (description
     "Guile-Reader is a simple framework for building readers for GNU Guile.

The idea is to make it easy to build procedures that extend Guile’s read
procedure.  Readers supporting various syntax variants can easily be written,
possibly by re-using existing “token readers” of a standard Scheme
readers.  For example, it is used to implement Skribilo’s R5RS-derived
document syntax.

Guile-Reader’s approach is similar to Common Lisp’s “read table”, but
hopefully more powerful and flexible (for instance, one may instantiate as
many readers as needed).")
    (home-page "https://www.nongnu.org/guile-reader/")
    (license license:gpl3+)))

(define-public guile2.2-reader
  (package
    (inherit guile-reader)
    (name "guile2.2-reader")
    (inputs (list guile-2.2))))

(define-public guile-ncurses
  (package
    (name "guile-ncurses")
    (version "3.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/guile-ncurses/guile-ncurses-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "0cypz1ikw66n8bc2klsnnaj1plpl22dwq6pwyc7dvffamz7fi2gf"))))
    (build-system gnu-build-system)
    (inputs (list ncurses guile-3.0))
    (native-inputs (list pkg-config))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  ((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  (guix build utils))
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:configure-flags (list "--with-gnu-filesystem-hierarchy")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-libguile-ncurses-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "make" "install"
                     "-C" "src/ncurses"
                     "-j" (number->string
                           (parallel-job-count)))
             (let* ((out   (assoc-ref outputs "out"))
                    (dir   "src/ncurses")
                    (files (find-files dir ".scm")))
               (substitute* files
                 (("\"libguile-ncurses\"")
                  (format #f "\"~a/lib/guile/~a/libguile-ncurses\""
                          out (target-guile-effective-version))))))))))
    (home-page "https://www.gnu.org/software/guile-ncurses/")
    (synopsis "Guile bindings to ncurses")
    (description
     "guile-ncurses provides Guile language bindings for the ncurses
library.")
    (license license:lgpl3+)))

(define-public guile2.2-ncurses
  (package
    (inherit guile-ncurses)
    (name "guile2.2-ncurses")
    (inputs (list ncurses guile-2.2))))

(define-public guile-ncurses/gpm
  (package
    (inherit guile-ncurses)
    (name "guile-ncurses-with-gpm")
    (inputs `(("ncurses" ,ncurses/gpm)
              ("guile" ,guile-3.0)))))

(define-public guile-lib
  (package
    (name "guile-lib")
    (version "0.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/guile-lib/guile-lib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1nb7swbliw9vx1ivhgd2m0r0p7nlkszw6s41zcgfwb5v1kp05sb4"))
              (patches (search-patches "guile-lib-fix-tests-for-guile2.2.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0") ;placate guild warnings
       #:phases
       (modify-phases %standard-phases
         (delete 'strip)
         (add-before 'configure 'patch-module-dir
           (lambda _
             (substitute* "src/Makefile.in"
               (("^moddir = ([[:graph:]]+)")
                "moddir = $(datadir)/guile/site/@GUILE_EFFECTIVE_VERSION@\n")
               (("^godir = ([[:graph:]]+)")
                "godir = \
$(libdir)/guile/@GUILE_EFFECTIVE_VERSION@/site-ccache\n")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("guile" ,guile-3.0)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list guile-3.0))           ;for cross-compilation
    (home-page "https://www.nongnu.org/guile-lib/")
    (synopsis "Collection of useful Guile Scheme modules")
    (description
     "Guile-Lib is intended as an accumulation place for pure-scheme Guile
modules, allowing for people to cooperate integrating their generic Guile
modules into a coherent library.  Think \"a down-scaled, limited-scope CPAN
for Guile\".  It provides the following modules:
@itemize
@item (apicheck) Describe and verify library programming interfaces.
@item (config load) Loading configuration files.
@item (container async-queue) A thread-safe message queue.
@item (container nodal-tree) A tree consisting of nodes with attributes.
@item (container delay-tree) A nodal tree with lazily evaluated fields.
@item (debugging assert) Helpful assert macro.
@item (debugging time) A simple macro to time the execution of an expression.
@item (graph topological-sort) Routines to perform topological sorts.
@item (htmlprag) Neil Van Dyke's permissive (\"pragmatic\") HTML parser.
@item (io string) SLIB's IO routines dealing with strings.
@item (logging logger) A flexible logging system.
@item (logging port-log) A logger that outputs to a port.
@item (logging rotating-log) A logger that rotates its output files.
@item (match-bind) Nifty and concise regular expression routines.
@item (math minima) A golden-section minimum finder.
@item (math primes) Functions related to prime numbers and factorization.
@item (os process) Spawning processes and capturing their output.
@item (scheme documentation) Macros to define different kinds of variables
with documentation.
@item (scheme kwargs) Defining functions with flexible keyword arguments.
@item (search basic) Classic search functions.
@item (string completion) Building blocks for tab completion.
@item (string soundex) The SOUNDEX string categorization algorithm.
@item (string transform) Beyond SRFI-13.
@item (string wrap) A versatile string formatter.
@item (term ansi-color) Generate ANSI color escape sequences.
@item (unit-test) A JUnit-style unit testing framework.
@end itemize")
    ;; The whole is under GPLv3+, but some modules are under laxer
    ;; distribution terms such as LGPL and public domain.  See `COPYING' for
    ;; details.
    (license license:gpl3+)))

(define-public guile-simple-iterators
  (let ((commit "50f16a2b2aa57e657e52e19fb3c35bdc182cfa36")
        (revision "0"))
    (package
      (name "guile-simple-iterators")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/dustyweb/guile-simple-iterators")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1m1wirlnfwmp5a4rpszd5qsbwabz4ji033w6p2714p1r524ylah8"))))
      (build-system guile-build-system)
      (native-inputs (list guile-3.0))
      (home-page "https://gitlab.com/dustyweb/guile-simple-iterators")
      (synopsis "Simple iterators for Guile")
      (description
       "This is a collection of iteration macros for Guile. They are inspired by
@code{racket}'s family of iterators. Specifically, the following iterators are
available:
@itemize
@item @code{for}
@item @code{for/map}
@item @code{for/c}
@item @code{for/fold}
@item @code{for/fold-right}
@item @code{for/folder}
@item @code{folder}
@end itemize")
      (license license:asl2.0))))

(define-public guile2.0-lib
  (package
    (inherit guile-lib)
    (name "guile2.0-lib")
    (native-inputs
     (alist-replace "guile" (list guile-2.0)
                    (package-native-inputs guile-lib)))
    (inputs
     (alist-replace "guile" (list guile-2.0)
                    (package-inputs guile-lib)))))

(define-public guile2.2-lib
  (package
    (inherit guile-lib)
    (name "guile2.2-lib")
    (native-inputs
     (alist-replace "guile" (list guile-2.2)
                    (package-native-inputs guile-lib)))
    (inputs
     (alist-replace "guile" (list guile-2.2)
                    (package-inputs guile-lib)))))

(define-public guile-minikanren
  (package
    (name "guile-minikanren")
    (version "20150424.e844d85")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ijp/minikanren")
                    (commit "e844d85512f8c055d3f96143ee506007389a25e3")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r50jlpzi940jlmxyy3ddqqwmj5r12gb4bcv0ssini9v8km13xz6"))))
    (build-system guile-build-system)
    (native-inputs
     (list guile-3.0))
    (home-page "https://github.com/ijp/minikanren")
    (synopsis "MiniKanren declarative logic system, packaged for Guile")
    (description
     "MiniKanren is a relational programming extension to the Scheme
programming Language, written as a smaller version of Kanren suitable for
pedagogical purposes.  It is featured in the book, The Reasoned Schemer,
written by Dan Friedman, William Byrd, and Oleg Kiselyov.

This is Ian Price's r6rs packaged version of miniKanren, which deviates
slightly from miniKanren mainline.

See http://minikanren.org/ for more on miniKanren generally.")
    (license license:expat)))

(define-public guile2.0-minikanren
  (package
    (inherit guile-minikanren)
    (name "guile2.0-minikanren")
    (native-inputs (list guile-2.0))))

(define-public guile2.2-minikanren
  (package
    (inherit guile-minikanren)
    (name "guile2.2-minikanren")
    (native-inputs (list guile-2.2))))

(define-public guile-irregex
  (package
    (name "guile-irregex")
    (version "0.9.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://synthcode.com/scheme/irregex/irregex-"
                    version ".tar.gz"))
              (sha256
               (base32
                "026kzl96pmwbjqdc7kh8rdh8ng813sjvdsik0dag5acza20sjm19"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'move-files-around
            (lambda _
              ;; Copy the relevant source files to src/ and create the
              ;; rx/ directory to match the expected module hierarchy.
              (mkdir-p "src/rx/source")
              (copy-file "irregex-guile.scm"
                         "src/rx/irregex.scm")
              (copy-file "irregex.scm"
                         "src/rx/source/irregex.scm")

              (mkdir-p "src/rx/irregex")
              (copy-file "irregex-utils-guile.scm"
                         "src/rx/irregex/utils.scm")
              (copy-file "irregex-utils.scm"
                         "src/rx/source/irregex-utils.scm")))
          (add-after 'build 'check
            (lambda _
              (for-each (lambda (f)
                          (invoke "guile" "--no-auto-compile" "-L" "." "-s" f))
                        (find-files "tests" "^guile-.*\\.scm"))))
          (add-after 'check 'check-installed
            (lambda _
              (define-values (scm go) (target-guile-scm+go #$output))
              (for-each
               (lambda (f)
                 (substitute* f
                   (("\\(load-from-path \"irregex\"\\)")
                    "(use-modules (rx irregex))")
                   (("\\(load-from-path \"irregex-utils\"\\)")
                    "(use-modules (rx irregex utils))"))
                 (invoke "guile" "-L" scm "-C" go "-L" "tests" f))
               (delete "tests/guile-cset.scm" ; Tests non-exported API
                       (find-files "tests" "^guile-.*\\.scm"))))))
      #:source-directory "src"))
    (native-inputs
     (list guile-3.0))
    (home-page "https://synthcode.com/scheme/irregex")
    (synopsis "S-expression based regular expressions")
    (description
     "Irregex is an s-expression based alternative to your classic
string-based regular expressions.  It implements SRFI 115 and is deeply
inspired by the SCSH regular expression system.")
    (license license:bsd-3)))

(define-public guile2.0-irregex
  (package
    (inherit guile-irregex)
    (name "guile2.0-irregex")
    (arguments
     (substitute-keyword-arguments (package-arguments guile-irregex)
       ((#:phases phases)
        #~(modify-phases #$phases
            ;; For some reason guile 2.0 cannot load foo.scm using
            ;; (load-from-path "foo").  So create symlinks to work around it.
            (add-before 'check 'create-symlinks
              (lambda _
                (use-modules (ice-9 regex))
                (for-each
                 (lambda (f)
                   (symlink (regexp-substitute #f (string-match "/([^/]+)$" f)
                                               1 ".scm")
                            f))
                 '("tests/guile/test-support"
                   "tests/test-cset"
                   "tests/test-irregex"
                   "tests/test-irregex-from-gauche"
                   "tests/test-irregex-pcre"
                   "tests/test-irregex-scsh"
                   "tests/test-irregex-utf8"))))))))
    (native-inputs (list guile-2.0))))

(define-public guile2.2-irregex
  (package
    (inherit guile-irregex)
    (name "guile2.2-irregex")
    (native-inputs (list guile-2.2))))

(define-public haunt
  (package
    (name "haunt")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/haunt/haunt-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0awrk4a2gfnk660m4kg9cy1w8z7bj454355w7rn0cjp5dg8bxflq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 match) (ice-9 ftw)
                  ,@%default-gnu-modules)
       #:tests? #f ; test suite is non-deterministic :(
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-haunt
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Wrap the 'haunt' command to refer to the right
                      ;; modules.
                      (let* ((out  (assoc-ref outputs "out"))
                             (bin  (string-append out "/bin"))
                             (site (string-append
                                    out "/share/guile/site"))
                             (guile-reader (assoc-ref inputs "guile-reader"))
                             (deps `(,@(if guile-reader
                                           (list guile-reader)
                                           '())
                                     ,(assoc-ref inputs "guile-commonmark"))))
                        (match (scandir site)
                          (("." ".." version)
                           (let ((modules (string-append site "/" version))
                                 (compiled-modules (string-append
                                                    out "/lib/guile/" version
                                                    "/site-ccache")))
                             (wrap-program (string-append bin "/haunt")
                               `("GUILE_LOAD_PATH" ":" prefix
                                 (,modules
                                  ,@(map (lambda (dep)
                                           (string-append dep
                                                          "/share/guile/site/"
                                                          version))
                                         deps)))
                               `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                                 (,compiled-modules
                                  ,@(map (lambda (dep)
                                           (string-append dep "/lib/guile/"
                                                          version
                                                          "/site-ccache"))
                                         deps)))))))))))))
    (native-inputs
     (list pkg-config texinfo))
    (inputs
     ;; Depend on the latest Guile to avoid bytecode compatibility issues when
     ;; using modules built against the latest version.
     (list bash-minimal guile-3.0-latest))
    (propagated-inputs
     (list guile-reader guile-commonmark))
    (synopsis "Functional static site generator")
    (description "Haunt is a static site generator written in Guile
Scheme.  Haunt features a functional build system and an extensible
interface for reading articles in any format.")
    (home-page "https://dthompson.us/projects/haunt.html")
    (license license:gpl3+)))

(define-public guile2.2-haunt
  (package
    (inherit haunt)
    (name "guile2.2-haunt")
    (inputs (list guile-2.2))
    (propagated-inputs
     `(("guile-reader" ,guile2.2-reader)
       ("guile-commonmark" ,guile2.2-commonmark)))))

(define-public guile2.0-haunt
  (package
    (inherit haunt)
    (name "guile2.0-haunt")
    (inputs (list guile-2.0))))

(define-public guile-redis
  (package
    (name "guile-redis")
    (version "2.2.0")
    (home-page "https://github.com/aconchillo/guile-redis")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url home-page)
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cb31vj88f3hj93v1lzxcqjyz7ym2gmpk31gv5i2dqv721frnlyj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf automake pkg-config guile-3.0))
    (synopsis "Redis client library for Guile")
    (description "Guile-redis provides a Scheme interface to the Redis
key-value cache and store.")
    (license license:lgpl3+)))

(define-public guile2.2-redis
  (package
    (inherit guile-redis)
    (name "guile2.2-redis")
    (native-inputs (modify-inputs (package-native-inputs guile-redis)
                     (replace "guile" guile-2.2)))))

(define-public guile2.0-redis
  (package
    (inherit guile-redis)
    (name "guile2.0-redis")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; put-string is in (rnrs io ports) in guile2.0,
             ;; not in (ice-9 textual-ports)
             (substitute* "redis/utils.scm"
               (("\\(ice-9 textual-ports\\)")
                "(rnrs io ports)"))
             #t)))
       ,@(package-arguments guile-redis)))
    (native-inputs (modify-inputs (package-native-inputs guile-redis)
                     (replace "guile" guile-2.0)))))

(define-public guile-commonmark
  (package
    (name "guile-commonmark")
    (version "0.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OrangeShark/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "17lrsdisa3kckh24q114vfmzdc4wkqa6ccwl4hdlrng5wpn1iman"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Allow builds with Guile 3.0.
                  (substitute* "configure"
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0"))
                  ;; The 'en_US.utf8' locale is missing, but C.UTF-8 is
                  ;; enough.
                  (substitute* (find-files "tests/inlines" "\\.scm$")
                    (("en_US.utf8")
                     "C.UTF-8"))))))
    (build-system gnu-build-system)
    ;; The tests throw exceptions with Guile 3.0.5, because they evaluate
    ;; (exit ...).
    ;;
    ;; This has been fixed upstream, but there has not been a new release
    ;; containing this change.
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests-when-building-with-guile-3.0.5
           (lambda _
             (substitute* (find-files "tests" "\\.scm$")
               (("\\(exit.*") ""))
             #t)))))
    (inputs
     (list guile-3.0))
    (native-inputs
     (list pkg-config))
    (synopsis "CommonMark parser for Guile")
    (description
     "guile-commonmark is a library for parsing CommonMark, a fully specified
variant of Markdown.  The library is written in Guile Scheme and is designed
to transform a CommonMark document to SXML.  guile-commonmark tries to closely
follow the @uref{http://commonmark.org/, CommonMark spec}, the main difference
is no support for parsing block and inline level HTML.")
    (home-page "https://github.com/OrangeShark/guile-commonmark")
    (license license:lgpl3+)))

(define-public guile2.2-commonmark
  (package
    (inherit guile-commonmark)
    (name "guile2.2-commonmark")
    (inputs (list guile-2.2))))

(define-public guile2.0-commonmark
  (package
    (inherit guile-commonmark)
    (name "guile2.0-commonmark")
    (inputs (list guile-2.0))))

(define-public mcron
  (package
    (name "mcron")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/mcron.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07gqwbjfsgf16ff624hkav0qhl10dv579y10fxas2kbjavqm4yx5"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "--with-sendmail=/run/privileged/bin/sendmail -t")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'adjust-tests
            (lambda _
              (substitute* "tests/job-specifier.scm"
                ;; (getpw) fails with "entry not found" in the build
                ;; environment, so pass an argument.
                (("\\(getpw\\)")
                 "(getpwnam (getuid))")
                ;; The build environment lacks an entry for root in
                ;; /etc/passwd.
                (("\\(getpw 0\\)")
                 "(getpwnam \"nobody\")")
                ;; FIXME: Skip the 4 faulty tests (see above).
                (("\\(test-equal \"next-year\"" all)
                 (string-append "(test-skip 4)\n" all))))))))
    (native-inputs (list autoconf
                         automake
                         guile-3.0    ;for 'guild compile'
                         help2man
                         pkg-config
                         tzdata-for-tests
                         texinfo))
    (inputs (list guile-3.0))
    (home-page "https://www.gnu.org/software/mcron/")
    (synopsis "Run jobs at scheduled times")
    (description
     "GNU Mcron is a complete replacement for Vixie cron.  It is used to run
tasks on a schedule, such as every hour or every Monday.  Mcron is written in
Guile, so its configuration can be written in Scheme; the original cron
format is also supported.")
    (license license:gpl3+)))

(define-public guile-picture-language
  (let ((commit "a1322bf11945465241ca5b742a70893f24156d12")
        (revision "5"))
    (package
      (name "guile-picture-language")
      (version (git-version "0.0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/guile-picture-language.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03i528z92ainccgm28shg4haxiav5x4cyhyi5dggq1rm027vbm99"))))
      (build-system gnu-build-system)
      (inputs
       (list guile-3.0))
      (propagated-inputs
       (list guile-cairo guile-rsvg))
      (native-inputs
       (list autoconf automake (librsvg-for-system) pkg-config texinfo))
      (home-page "https://git.elephly.net/software/guile-picture-language.git")
      (synopsis "Picture language for Guile")
      (description
       "This package provides a simple SVG-based picture language for Guile.
The picture values can directly be displayed in Geiser.")
      ;; (pict base64) is under GPLv3+, the rest is under LGPLv3+
      (license (list license:lgpl3+
                     license:gpl3+)))))

(define-public guile2.2-picture-language
  (package
    (inherit guile-picture-language)
    (name "guile2.2-picture-language")
    (inputs (list guile-2.2))
    (propagated-inputs
     (list guile2.2-cairo guile2.2-rsvg))))

(define-public guile-studio
  (let ((commit "dd0ad42e51feafebda7cc29afe7c8bc7a182a842")
        (revision "1"))
    (package
      (name "guile-studio")
      (version (git-version "0.1.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.elephly.net/software/guile-studio.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1cpqilly8dqmai1qsgjxy99zs34sfz95zwxhzx979wryqb69vi0q"))))
      (build-system gnu-build-system)
      (arguments
       `(#:modules
         ((ice-9 match)
          (srfi srfi-1)
          ,@%default-gnu-modules)
         #:tests? #f                    ; there are none
         #:make-flags
         (list (string-append "PICT_DIR="
                              (assoc-ref %build-inputs "guile-picture-language"))
               (string-append "EMACS_DIR="
                              (assoc-ref %build-inputs "emacs"))
               (string-append "GUILE_DIR="
                              (assoc-ref %build-inputs "guile"))
               (string-join (cons "INPUTS="
                                  (filter-map
                                   (lambda (input)
                                     (match input
                                       ((label . pkg)
                                        (and (string-prefix? "emacs" label) pkg))))
                                   %build-inputs)))
               (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (delete 'install))))
      (inputs
       (list guile-3.0
             guile-picture-language
             emacs
             emacs-f ; needed by doom-modeline
             emacs-memoize ; needed by all-the-icons
             emacs-all-the-icons ; needed by doom-modeline
             emacs-all-the-icons-dired
             emacs-dired-sidebar
             emacs-doom-modeline
             emacs-modus-themes
             emacs-geiser
             emacs-geiser-guile
             emacs-company
             emacs-ivy
             emacs-flycheck
             emacs-flycheck-guile
             emacs-paren-face))
      (native-inputs
       (list texinfo))
      (home-page "https://gnu.org/software/guile")
      (synopsis "IDE for Guile")
      (description
       "This is Emacs with a few settings that make working with Guile easier
for people new to Emacs.  Features include: CUA mode, Geiser, tool bar icons
to evaluate Guile buffers, support for Guile's very own picture language, code
completion, a simple mode line, etc.")
      (license license:gpl3+))))

(define-public guile-stis-parser
  (package
    (name "guile-stis-parser")
    (version "1.2.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/tampe/stis-parser")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fvxdfvc80zqhwzq5x3kxyr6j8p4b51yx85fx1gr3d4gy2ddpx5w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f             ; not supported
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "modules") #t))
         (add-after 'chdir 'delete-broken-symlink
           (lambda _
             (delete-file "parser/stis-parser/lang/.#calc.scm")
             #t)))))
    (inputs
     (list guile-3.0))
    (native-inputs
     (list autoconf automake pkg-config))
    (home-page "https://gitlab.com/tampe/stis-parser")
    (synopsis "Parser combinator framework")
    (description
     "This package provides a functional parser combinator library that
supports backtracking and a small logical framework. The idea is to build up
chunks that are memoized and there is no clear scanner/parser separation,
chunks can be expressions as well as simple tokens.")
    (license license:lgpl2.0+)))

(define-public guile-persist
  (package
    (name "guile-persist")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/tampe/guile-persist")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "19f8hqcax4v40858kx2j8fy1cvzc2djj99r0n17dy1xxmwa097qi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-prefix
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "src/Makefile.am"
               (("/usr/local/lib/guile")
                (string-append (assoc-ref outputs "out") "/lib/guile"))
               (("/usr/local/include/guile")
                (search-input-directory inputs "/include/guile"))
               (("-L/usr/local/lib")
                (string-append "-L" (assoc-ref inputs "guile") "/lib")))
             #t))
         (add-after 'unpack 'patch-library-reference
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "persist/persistance.scm"
                 (("\"libguile-persist\"")
                  (format #f "\"~a/lib/guile/3.0/extensions/libguile-persist\"" out)))
               #t))))))
    (inputs
     (list guile-3.0))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://gitlab.com/tampe/guile-persist")
    (synopsis "Persistence programming framework for Guile")
    (description
     "This is a serialization library for serializing objects like classes
and objects, closures and structs.  This currently does not support
serializing continuations or delimited continuations.")
    (license license:lgpl2.0+)))

(define-public python-on-guile
  (package
    (name "python-on-guile")
    (version "1.2.3.5")
    (home-page "https://gitlab.com/python-on-guile/python-on-guile")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05xrvcj6a4gzq1ybyin270qz8wamgc7w2skyi9iy6hkpgdhxy8vf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f                   ;not supported
       #:make-flags '("GUILE_AUTO_COMPILE=0") ;to prevent guild warnings
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "modules") #t))
         (add-after 'chdir 'augment-GUILE_LOAD_PATH
           (lambda _
             ;; TODO: It would be better to patch the Makefile.
             (setenv "GUILE_LOAD_PATH"
                     (string-append ".:"
                                    (getenv "GUILE_LOAD_PATH")))))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Wrap the 'python' executable so it can find its
             ;; dependencies and own modules.
             (let* ((out (assoc-ref outputs "out"))
                    (guile-version ,(version-major+minor
                                     (package-version guile-3.0)))
                    (scm (string-append out "/share/guile/site/"
                                        guile-version))
                    (ccache (string-append out "/lib/guile/" guile-version
                                           "/site-ccache"))
                    (load-path (string-join
                                (cons scm
                                      ;; XXX: cdr because we augment it above.
                                      (cdr (string-split
                                            (getenv "GUILE_LOAD_PATH") #\:)))
                                ":"))
                    (compiled-path (string-append
                                    ccache ":"
                                    (getenv "GUILE_LOAD_COMPILED_PATH"))))
               (wrap-program (string-append out "/bin/python")
                 `("GUILE_LOAD_PATH" ":" prefix
                   (,load-path))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                   (,compiled-path)))))))))
    (inputs
     (list bash-minimal guile-3.0 guile-persist guile-readline guile-stis-parser))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "Python implementation in Guile")
    (description
     "This package allows you to compile a Guile Python file to any target
from @code{tree-il}.")
    (license license:lgpl2.0+)))

(define-public guile-hoot
  (package
    (name "guile-hoot")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://spritely.institute/files/releases"
                                  "/guile-hoot/guile-hoot-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0miq9bv09xvzdrcvzdrca9vychsznpzi4jj87f5r1mwz0xxpvxjb"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0"
                      "WASM_HOST=hoot")))
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs
     (list guile-next))
    (synopsis "WebAssembly compiler backend for Guile")
    (description "Guile Hoot is a WebAssembly compiler backend for GNU Guile
and standalone WASM toolchain.")
    (home-page "https://spritely.institute/hoot")
    (license (list license:asl2.0 license:lgpl3+))))

(define-public guile-file-names
  (package
    (name "guile-file-names")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://brandon.invergo.net/software/download/"
                                  "guile-file-names/guile-file-names-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01chizdxkhw6aqv629vxka9f5x3534ij7r0jqndawsg2vxm1r9sz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-target-directory
           (lambda _
             (substitute* "src/Makefile.in"
               (("guilemoddir = \\$\\(GUILE_SITE\\)")
                "guilemoddir = $(datadir)/guile/site/$(GUILE_EFFECTIVE_VERSION)\n"))
             #t)))))
    (inputs
     (list guile-2.2))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.com/brandoninvergo/guile-file-names")
    (synopsis "Manipulate file names")
    (description
     "The @code{(file-names)} module provides tools for manipulating file
names.  The module was built on the idea that doing anything more than a
non-trivial modification of a file name string is a pain (making sure all
slashes are present and accounted for, resolving @code{.} and @code{..}, etc).
Inevitably, you have to break the string up into chunks and operate on that
list of components.  This module takes care of that for you.")
    (license license:lgpl3+)))

(define-public guile-gi
  (package
    (name "guile-gi")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://lonelycactus.com/tarball/guile_gi-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "019mbhgyga57k2074kg97mh3qsa8ny9l0kjgqids8cg3c6vbjdby"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--with-gnu-filesystem-hierarchy")
       #:modules ((guix build glib-or-gtk-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim))
       #:disallowed-references ,(list gtk+ webkitgtk-for-gtk3)
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-dotted-circle-from-combining-character
           ;; The test/string.scm files contain ◌̀, which is a dotted circle
           ;; (U+25cc) followed by an upper combining character (U+0300). The
           ;; old guile 3.0.2 reader incorrectly ignores the dotted circle,
           ;; and parses it as the combining character alone, but the new
           ;; guile reader does not.
           ;; See https://github.com/spk121/guile-gi/issues/112
           (lambda* _
             (substitute* "test/string.scm"
               (("#\\\\◌̀") "#\\x0300"))))
         (add-after 'unpack 'patch-references-to-extension
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((effective (read-line
                               (open-pipe* OPEN_READ
                                           "guile" "-c"
                                           "(display (effective-version))"))))
               (substitute* (find-files "module" ".*\\.scm")
                 (("\\(load-extension \"libguile-gi\" \"(.*)\"\\)" m arg)
                  (format #f "~s"
                          `(load-extension
                            (format #f "~alibguile-gi"
                                    (if (getenv "GUILE_GI_UNINSTALLED")
                                        ""
                                        ,(format #f "~a/lib/guile/~a/extensions/"
                                                 (assoc-ref outputs "out")
                                                 effective)))
                            ,arg)))))
             (setenv "GUILE_GI_UNINSTALLED" "1")
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The init_check test requires a running X server.
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t)))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin") ; for glib-compile-resources
           libtool pkg-config xorg-server))
    (propagated-inputs (list gobject-introspection))
    (inputs (list guile-3.0 glib
                  ;; For tests, only relevant when compiling natively
                  gtk+ webkitgtk-for-gtk3))
    (home-page "https://github.com/spk121/guile-gi")
    (synopsis "GObject bindings for Guile")
    (description
     "Guile-GI is a library for Guile that allows using GObject-based
libraries, such as GTK+3.  Its README comes with the disclaimer: This is
pre-alpha code.")
    (license license:gpl3+)))

(define-public guile2.2-gi
  (package
    (inherit guile-gi)
    (name "guile2.2-gi")
    (inputs
     (modify-inputs (package-inputs guile-gi)
       (replace "guile" guile-2.2)))))

(define-public guile-srfi-89
  (package
    (name "guile-srfi-89")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/mjbecze/guile-srfi-89.git")
             (commit version)))
       (sha256
         (base32
           "1981c0rrzxqx3md9jx8ir7j3m2mzg9m72b33p5jvw36zirbzpl20"))
       (file-name (git-file-name name version))))
    (build-system guile-build-system)
    (native-inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/mjbecze/guile-srfi-89")
    (synopsis "Hygienic implementation of SRFI-89 for Guile")
    (description
     "This package provides SRFI-89 optional positional and named
parameters, which  define* and lambda* special forms")
    (license license:gpl3+)))

(define-public guile-srfi-128
  (package
    (name "guile-srfi-128")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://inqlab.net/git/guile-srfi-128.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "03d85q5l2gc2c8cmri6zd4pfndvnadlhwh77hsx6ixvvm8vwq4sy"))
       (file-name (git-file-name name version))))
    (build-system guile-build-system)
    (native-inputs
     (list guile-3.0))
    (home-page "https://inqlab.net/git/guile-srfi-128.git")
    (synopsis "SRFI 128 Comparators (reduced) port for Guile")
    (description
     "This package provides an implementation of SRFI 128 for Guile.
SRFI 128 defines comparators, which bundles a test type predicate, an
equality predicate, an ordering predicate and a hash function into a
single Scheme object.  This can be used in the implementation of data
structures.  This package re-uses the SRFI sample implementation.")
    (license
     (list license:lgpl3+
           ;; contains ISC code from the SRFI sample implementation
           license:isc))))

(define-public guile-srfi-133
  (package
    (name "guile-srfi-133")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scheme-requests-for-implementation/srfi-133")
             (commit "db81a114cd3e23375f024baec15482614ec90453")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0a7srl72291yah0aj6rwddhj041v2spximhknjj7hczlparsrm7f"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'move-create-and-delete-files
            (lambda _
              (rename-file "vectors" "srfi")
              (rename-file "srfi/vectors-test.scm" "srfi/srfi-test.scm")
              (rename-file "srfi/vectors-impl.scm" "srfi/srfi-impl.scm")
              (with-output-to-file "srfi/srfi-133.scm"
                (lambda ()
                  (display "(define-module (srfi srfi-133)
  #:replace (;; Constructors
             vector-copy

             ;; Mutators
             vector-fill! vector-copy!

             ;; Conversion
             vector->list list->vector)
  #:export (;; Constructors
            vector-unfold vector-unfold-right vector-reverse-copy
            vector-append vector-concatenate vector-append-subvectors

            ;; Predicates
            vector-empty? vector=

            ;; Iteration
            vector-fold vector-fold-right vector-map vector-map!
            vector-for-each vector-count vector-cumulate

            ;; Searching
            vector-index vector-index-right vector-skip vector-skip-right
            vector-binary-search vector-any vector-every vector-partition

            ;; Mutators
            vector-swap! vector-reverse!
            vector-reverse-copy! vector-unfold! vector-unfold-right!

            ;; Conversion
            reverse-vector->list reverse-list->vector
            vector->string string->vector))

(include \"srfi-impl.scm\")")))
              (for-each (lambda (filename)
                          (delete-file filename))
                        '("tests/run.scm"
                          "srfi/vectors.sld"
                          "srfi/vectors.scm")))))))
    (native-inputs
     (list guile-3.0))
    (home-page "https://github.com/scheme-requests-for-implementation/srfi-133")
    (synopsis "R7RS-compatible vector library for Guile")
    (description
     "This package provides a Guile implementation of
@uref{https://srfi.schemers.org/srfi-133/srfi-133.html, SRFI-133}, a
comprehensive library of vector operations.")
    (license license:expat)))

(define-public guile-srfi-145
  (package
    (name "guile-srfi-145")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/mjbecze/guile-srfi-145.git")
             (commit version)))
       (sha256
         (base32
           "1gssa8cmcp8640fil9z8dpil8v5l279wlalqjcx3fls5jwv13q1b"))
       (file-name (git-file-name name version))))
    (build-system guile-build-system)
    (native-inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/mjbecze/guile-srfi-145")
    (synopsis "SRFI-145 port for Guile")
    (description
     "This package provides SRFI-145.  This provides the means to
denote the invalidity of certain code paths in a Scheme program.")
    (license license:gpl3+)))

(define-public guile-srfi-146
  (package
    (name "guile-srfi-146")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://inqlab.net/git/guile-srfi-146.git")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "13dbzlav4fql8lcfr021z5368lwri6i15x0ykv8llzyghlbbx2w6"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list guile-3.0
           guile-srfi-128 guile-srfi-145 guile-srfi-158
           autoconf automake pkg-config))
    (inputs (list guile-3.0))
    (propagated-inputs
     (list guile-srfi-128 guile-srfi-145 guile-srfi-158))
    (synopsis "SRFI 146 (Mappings) for Guile")
    (description
     "This package provides an implementation of SRFI 146 for Guile.
SRFI 146 defines datastructures that implement mappings (finite sets
of associations consisting of a key and a value).  Two types of
mappings are defined: One using a comparator to define an order on the
keys and another using a hash function on the keys.  The
datastructures and procedures are by default purely-functional.  This
package re-uses the SRFI sample implementation that is based on
red-black trees and Hash Array Mapped Trie (HAMT).")
    (home-page "https://inqlab.net/git/guile-srfi-146.git")
    (license
     (list license:lgpl3+
           ;; contains ISC code from the SRFI sample implementation
           license:isc))))

(define-public guile-srfi-158
  (let ((commit "13126d1ed37892c864337a600a43d6876625fb99")
        (revision "0"))
    (package
      (name "guile-srfi-158")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/mjbecze/guile-srfi-158.git")
               (commit commit)))
         (sha256
          (base32
           "0hg57l3w5qamip1clkab0q01np5nqln9y054q39smm4ki0svdl8w"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       (list guile-3.0 autoconf automake pkg-config))
      (inputs (list guile-3.0))
      (home-page "https://gitlab.com/samplet/guile-srfi-158")
      (synopsis "SRFI 158 (Generators and Accumulators) for Guile")
      (description "This package provides an implementation of SRFI 158
for Guile.  SRFI 158 defines utility procedures that create,
transform, and consume generators.  It also defines procedures that
return accumulators.  It is implemented by wrapping the sample
implementation in a thin Guile compatibility layer.")
      (license license:gpl3+))))

(define-public guile-srfi-159
  (let ((commit "1bd98abda2ae4ef8f36761a167903e55c6bda7bb")
        (revision "0"))
    (package
      (name "guile-srfi-159")
      (version (git-version "0" revision commit))
      (home-page "https://bitbucket.org/bjoli/guile-srfi-159")
      (source (origin
                (method hg-fetch)
                (uri (hg-reference (changeset commit)
                                   (url home-page)))
                (sha256
                 (base32
                  "1zw6cmcy7xdbfiz3nz9arqnn7l2daidaps6ixkcrc9b6k51fdv3p"))
                (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       ;; The *-impl.scm files are actually included from module files; they
       ;; should not be compiled separately, but they must be installed.
       '(#:not-compiled-file-regexp "-impl\\.scm$"))
      (inputs
       (list guile-2.2))
      (synopsis "Formatting combinators for Guile")
      (description
       "The @code{(srfi-159)} module and its sub-modules implement the
formatting combinators specified by
@uref{https://srfi.schemers.org/srfi-159/srfi-159.html, SRFI-159}.  These are
more expressive and flexible than the traditional @code{format} procedure.")
      (license license:bsd-3))))

(define-public guile-srfi-180
  (let ((commit "9188bf9724c6d320ef804579d222e855b007b193")
        (revision "0"))
    (package
      (name "guile-srfi-180")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme-requests-for-implementation/srfi-180")
               (commit commit)))
         (sha256
          (base32
           "08lf70rsak8mwfij55xc37pg9zg7c87fizmhz7ln46skzj68sl3y"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file-recursively "srfi/files")
             (delete-file "srfi/run-r7rs-checks.guile.scm")
             (delete-file "srfi/run-r7rs-checks.scm")
             (delete-file "srfi/check.scm")
             #t))
         (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       '(#:not-compiled-file-regexp "body\\.scm$"))
      (native-inputs
       (list guile-3.0))
      (propagated-inputs
       (list guile-srfi-145))
      (home-page "https://srfi.schemers.org/srfi-180/")
      (synopsis "JSON parser and printer for Guile")
      (description
       "This library implements a JavaScript Object Notation (JSON) parser and printer.
It also supports parsing JSON objects that may be bigger than memory with a streaming
API.")
      (license license:expat))))

(define-public guile-srfi-189
  (let ((commit "659e3cd0fc2bfca9085424eda8cad804ead2a9ea")
        (revision "1"))
    (package
      (name "guile-srfi-189")
      ;; 'final' is the name of the latest git tag.
      (version (git-version "final" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme-requests-for-implementation/srfi-189")
               (commit commit)))
         (sha256
          (base32
           "0iqv4sjwbp4k87r9l9abzbs5yjcljm69m91kb1ypb03b0rx7napy"))
         (modules '((guix build utils)))
         (snippet
          '(begin
             (delete-file "test-syntax.scm")
             (delete-file "test.scm")))
         (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       '(#:not-compiled-file-regexp "srfi/189\\.scm$")) ; it's INCLUDE'd
      (native-inputs
       (list guile-3.0))
      (propagated-inputs
       (list guile-srfi-145))
      (home-page "https://srfi.schemers.org/srfi-189/")
      (synopsis "Scheme SRFI implementation of Maybe and Either")
      (description
       "This SRFI defines two disjoint immutable container types known as
Maybe and Either, both of which can contain objects collectively known
as their payload.  A Maybe object is either a Just object or the unique
object Nothing (which has no payload); an Either object is either a Right
object or a Left object.  Maybe represents the concept of optional values;
Either represents the concept of values which are either correct (Right)
or errors (Left).")
      (license license:expat))))

(define-public guile-srfi-197
  ;; There is minor fix to the documentation after the final tag, so use
  ;; the newest commit instead.
  (let ((commit "d31b8be86460bf837cccf2737a1b9b9c01788573")
        (revision "0"))
    (package
      (name "guile-srfi-197")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/scheme-requests-for-implementation/srfi-197")
               (commit commit)))
         (sha256
          (base32
           "1c1jjzqgavjwfzs352wssdbjga5ymv4g3lkl0zxhjw7pfrr5xx1m"))
         (file-name (git-file-name name version))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "src"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'create-module
              (lambda _
                (use-modules (ice-9 textual-ports))
                (mkdir-p "src/srfi")
                (call-with-output-file "src/srfi/srfi-197.scm"
                  (lambda (port)
                    (write '(define-module (srfi srfi-197)
                              #:use-module (scheme base)
                              #:export (chain
                                        chain-and
                                        chain-when
                                        chain-lambda
                                        nest
                                        nest-reverse))
                           port)
                    (call-with-input-file "srfi-197-syntax-case.scm"
                      (lambda (in-port)
                        (display (get-string-all in-port) port)))))))
            (add-after 'build 'check-installed
              (lambda _
                (define-values (scm go) (target-guile-scm+go #$output))
                (invoke "guile" "-L" scm "-C" go
                        "--use-srfi=197" "./test.scm"))))))
      (native-inputs
       (list guile-3.0))
      (home-page "https://srfi.schemers.org/srfi-197/")
      (synopsis "Pipeline operators for Guile")
      (description
       "This library provides a reference implementation for SRFI-197.  This
SRFI defines a family of chain and nest pipeline operators, which can rewrite
nested expressions like @code{(a b (c d (e f g)))} as a sequence of
operations: @code{(chain g (e f _) (c d _) (a b _))}.")
      (license license:expat))))

(define-public guile-srfi-232
  (package
    (name "guile-srfi-232")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scheme-requests-for-implementation/srfi-232")
             (commit "c3f580d220778cd71492aba4fdd0c7040968e705")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0lp4zcqjjj6hwfh3ix71wak1nffgg4npzsg7cdxfn9hf6iwf9xby"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'move-and-delete-things
            (lambda _
              (let* ((srfi-directory (string-append #$output "/srfi")))
                (mkdir-p "srfi")
                (with-output-to-file "srfi/srfi-232.scm"
                  (lambda ()
                    (display "(define-library (srfi srfi-232)
 (export curried define-curried)
 (import (only (guile) import)
         (scheme base))
 (include \"../srfi-232.scm\"))")))
                (for-each (lambda (filename)
                            (delete-file filename))
                          '("test-body.scm"
                            "test-chibi.scm"
                            "test-srfi-64.scm"))))))))
    (native-inputs
     (list guile-3.0))
    (home-page "https://github.com/scheme-requests-for-implementation/srfi-232")
    (synopsis "Flexible curried procedures")
    (description
     " This package provides an implementation of
@uref{https://srfi.schemers.org/srfi-232/srfi-232.html, SRFI-232}, which
describes @code{curried}, a variant of @code{lambda} that creates true curried
procedures which also behave just like ordinary Scheme procedures.  They can
be applied to their arguments one by one, all at once, or anywhere in between,
without any novel syntax.  @code{curried} also supports nullary and variadic
procedures, and procedures created with it have predictable behavior when
applied to surplus arguments.")
    (license license:expat)))

(define-public guile-srfi-235
  (let ((version "1.0.0")
        (revision "1")
        (commit "643a44aa9d6872962257995ecb0a31eb06a71d88"))
    (package
      (name "guile-srfi-235")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url
                "https://github.com/scheme-requests-for-implementation/srfi-235")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1slkcr67ad12ipkbhjdzjhbnsyvq5wi7cssvgv110fr2dy4rciwp"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'move-create-and-delete-files
                       (lambda _
                         (substitute* "srfi/235.sld"
                           (("srfi 235")
                            "srfi srfi-235"))
                         (rename-file "srfi/235.sld" "srfi/srfi-235.scm"))))))
      (native-inputs (list guile-3.0))
      (home-page
       "https://github.com/scheme-requests-for-implementation/srfi-235")
      (synopsis "Combinators for Guile Scheme")
      (description
       "This SRFI contains various procedures that accept and return procedures, as
well as a few others, drawn from an earlier version of Chicken.
Common Lisp has a few of them too, and more come from the Standard
Prelude from Programming Praxis.  Using these procedures helps to keep
code terse and reduce the need for ad hoc lambdas.")
      (license license:expat))))

(define-public emacsy
  (package
    (name "emacsy")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/emacsy/emacsy-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cpb85dl1nibd34c2x2h7vfmjpkgh353p5b1w20v6cs6gmvgg4np"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "configure"
                    ;; Allow builds with Guile 3.0.
                    (("2\\.2 2\\.0")
                     "3.0 2.2 2.0")

                    ;; Freeglut 3.2 provides 'glut.pc', not 'freeglut.pc'.
                    (("freeglut >= ")
                     "glut >= "))

                  (substitute* '("emacsy/emacsy.c"
                                 "example/hello-emacsy.c")
                    (("#include <libguile\\.h>")
                     (string-append "#include <stdlib.h>\n"
                                    "#include <stdio.h>\n"
                                    "#include <string.h>\n"
                                    "#include <unistd.h>\n"
                                    "#include <libguile.h>\n")))))))
    (build-system gnu-build-system)
    (native-inputs
     `(("bzip2" ,bzip2)
       ("guile" ,guile-3.0)
       ("gettext" ,gettext-minimal)
       ("libtool" ,libtool)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)
       ("texlive" ,(texlive-updmap.cfg (list texlive-epsf)))))
    (inputs
     (list bash-minimal
           dbus-glib
           guile-3.0
           guile-lib
           guile-readline
           freeglut
           webkitgtk-with-libsoup2))
    (propagated-inputs
     `(("glib-networking" ,glib-networking)
       ("gssettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (ice-9 regex)
                  (ice-9 ftw)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'setenv
           (lambda _
             (setenv "GUILE_AUTO_COMPILE" "0")))
         (add-after 'install 'wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (effective (read-line
                                (open-pipe* OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (deps (map (cut assoc-ref inputs <>)
                               '("guile-lib" "guile-readline")))
                    (scm-path (map (cut string-append <> "/share/guile/site/"
                                        effective) `(,out ,@deps)))
                    (go-path (map (cut string-append <> "/lib/guile/" effective
                                       "/site-ccache/") `(,out ,@deps)))
                    (examples (filter (cut string-match "emacsy" <>)
                                      (scandir (string-append out "/bin/"))))
                    (progs (map (cut string-append out "/bin/" <>)
                                examples)))
               (map (cut wrap-program <>
                         `("GUILE_LOAD_PATH" ":" prefix ,scm-path)
                         `("GUILE_LOAD_COMPILED_PATH" ":" prefix ,go-path))
                    progs)))))))
    (home-page "https://savannah.nongnu.org/projects/emacsy")
    (synopsis "Embeddable GNU Emacs-like library using Guile")
    (description
     "Emacsy is an embeddable Emacs-like library that uses GNU Guile
as extension language.  Emacsy can give a C program an Emacsy feel with
keymaps, minibuffer, recordable macros, history, tab completion, major
and minor modes, etc., and can also be used as a pure Guile library.  It
comes with a simple counter example using FreeGLUT and browser examples
in C using Gtk+-3 and WebKitGtk.")
    (license license:gpl3+)))

(define-public emacsy-minimal
  (let ((commit "v0.4.1-37-g5f91ee6"))
    (package
      (inherit emacsy)
      (name "emacsy-minimal")
      (version (string-drop commit 1))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://git.savannah.gnu.org/git/emacsy.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "03ym14g9qhjqmryr5z065kynqm8yhmvnbs2djl6vp3i9cmqln8cl"))))
      (build-system gnu-build-system)
      (native-inputs
       `(("autoconf" ,autoconf)
         ("automake" ,automake)
         ("gettext" ,gettext-minimal)
         ("libtool" ,libtool)
         ("makeinfo" ,texinfo)
         ("pkg-config" ,pkg-config)))
      (inputs
       `(("guile" ,guile-2.2)
         ("guile-lib" ,guile2.2-lib)
         ("guile-readline" ,guile2.2-readline)))
      (propagated-inputs '())
      (arguments
       `(#:configure-flags '("--without-examples")
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'setenv
             (lambda _
               (setenv "GUILE_AUTO_COMPILE" "0")
               #t))))))))

(define-public guile-jpeg
  (let ((commit "6a1673578b297c2c1b28e44a76bd5c49e76a5046")
        (revision "0"))
    (package
      (name "guile-jpeg")
      (version (git-version "0.0" revision commit))
      (home-page "https://gitlab.com/wingo/guile-jpeg")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "05z9m408w3h6aqb5k3r3qa7khir0k10rxwvsrzhkcq1hr5vbmr4m"))
                (file-name (git-file-name name version))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Install .go files in the right place.
                    (substitute* "Makefile.am"
                      (("/ccache") "/site-ccache"))
                    #t))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake pkg-config guile-2.2))
      (synopsis "JPEG file parsing library for Guile")
      (description
       "Guile-JPEG is a Scheme library to parse JPEG image files and to
perform geometrical transforms on JPEG images.")
      (license license:gpl3+))))

(define-public guile-jtd
  (package
    (name "guile-jtd")
    (version "220323a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mwette/guile-jtd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l8fyqhvksarvpbr903i3ss3432jzvyvhgcqa15j922ngqh4ds6f"))))
    (build-system guile-build-system)
    (native-inputs (list guile-3.0))
    (home-page "https://github.com/mwette/guile-jtd")
    (synopsis "Python's @code{pdb.set_trace()} but for Guile")
    (description
     "The @code{(jtd)} module for Guile provides a procedure
@code{jump-to-debugger} for escaping to the Guile REPL for the purpose of
debugging code.")
    (license license:lgpl2.1+)))

(define-public guile-png
  (package
    (name "guile-png")
    (version "0.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/guile-png")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hgdp8fgyg6rdy130fsn4nnb58c98lsrayjyy5491l53814ggy65"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list "GUILE_AUTO_COMPILE=0") ;to prevent guild warnings
      #:phases #~(modify-phases %standard-phases
                   (delete 'strip))))
    (native-inputs (list autoconf
                         automake
                         pkg-config
                         texinfo
                         ;; needed when cross-compiling.
                         guile-3.0
                         guile-lib
                         guile-zlib
                         guile-smc))
    (inputs (list bash-minimal guile-3.0 guile-lib guile-zlib))
    (propagated-inputs (list guile-smc))
    (home-page "https://github.com/artyom-poptsov/guile-png")
    (synopsis "PNG file parsing library for Guile")
    (description
     "@code{guile-png} is a GNU Guile library for working with the
@url{https://en.wikipedia.org/wiki/PNG, PNG format}.  This library provides API for
reading and writing PNG data, as well as some graphic primitives and basic image
processing filters.")
    (license license:gpl3+)))

(define-public nomad
  (package
    (name "nomad")
    (version "0.2.0-alpha-199-g3e7a475")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.savannah.gnu.org/git/nomad.git/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p0ha6prp7pyadp61clbhc6b55023vxzfwy14j2qygb2mkq7fhic"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bash" ,bash)
       ("pkg-config" ,pkg-config)
       ("libtool" ,libtool)
       ("guile" ,guile-2.2)
       ("glib:bin" ,glib "bin")
       ("texinfo" ,texinfo)
       ("gettext" ,gettext-minimal)
       ("perl" ,perl)))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ;; Guile
       ("guile" ,guile-2.2)
       ("guile-lib" ,guile2.2-lib)
       ("guile-readline" ,guile2.2-readline)
       ("guile-gcrypt" ,guile2.2-gcrypt)
       ("gnutls" ,gnutls)
       ("g-golf" ,guile2.2-g-golf)
       ("shroud" ,shroud)
       ("emacsy" ,emacsy-minimal)
       ;; Gtk
       ("glib" ,glib)
       ("dbus-glib" ,dbus-glib)
       ("glib-networking" ,glib-networking)
       ("gtk+" ,gtk+)
       ("gtk+:bin" ,gtk+ "bin")
       ("webkitgtk" ,webkitgtk-for-gtk3)
       ("gtksourceview" ,gtksourceview-4)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("vte" ,vte/gtk+-3)
       ;; Gstreamer
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-ugly" ,gst-plugins-ugly)
       ;; Util
       ("xorg-server" ,xorg-server)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 rdelim)
                  (srfi srfi-26))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-webkitgtk
           (lambda _
             ;; Adapt to the version we have in Guix.
             (substitute* "configure.ac"
               (("webkit2gtk-4\\.0") "webkit2gtk-4.1")
               (("webkit2gtk-web-extension-4\\.0")
                "webkit2gtk-web-extension-4.1"))

             (substitute* "typelib/Makefile.am"
               (("WebKit2-4\\.0") "WebKit2-4.1"))))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system (format #f "~a/bin/Xvfb :1 &"
                             (assoc-ref inputs "xorg-server")))
             (setenv "DISPLAY" ":1")
             #t))
         (add-after 'install 'wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out        (assoc-ref outputs "out"))
                    (effective  (read-line (open-pipe*
                                            OPEN_READ
                                            "guile" "-c"
                                            "(display (effective-version))")))
                    (gst-plugins (map (lambda (i)
                                        (string-append (assoc-ref inputs i)
                                                       "/lib/gstreamer-1.0"))
                                      `("gstreamer"
                                        "gst-plugins-base"
                                        "gst-plugins-good"
                                        "gst-plugins-bad"
                                        "gst-plugins-ugly")))
                    (out-append (lambda (. args)
                                  (apply string-append out args)))
                    (gi-path    (out-append "/lib/girepository-1.0"))
                    (load-path  (out-append "/share/guile/site/" effective))
                    (comp-path  (out-append "/lib/guile/"
                                            effective "/site-ccache"))
                    (ext-path   (out-append "/libexec/nomad")))
               (wrap-program (string-append out "/bin/nomad")
                 `("GUILE_LOAD_PATH" ":" prefix
                   (,load-path
                    ,(getenv "GUILE_LOAD_PATH")))
                 `("GUILE_LOAD_COMPILED_PATH" ":" prefix
                   (,comp-path
                    ,(getenv "GUILE_LOAD_COMPILED_PATH")))
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,gi-path ,(getenv "GI_TYPELIB_PATH")))
                 `("GIO_EXTRA_MODULES" ":" prefix
                   (,(getenv "GIO_EXTRA_MODULES")))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix ,gst-plugins)
                 `("NOMAD_WEB_EXTENSION_DIR" ":" prefix (,ext-path)))
               #t))))))
    (home-page "https://savannah.nongnu.org/projects/nomad/")
    (synopsis "Extensible Web Browser in Guile Scheme")
    (description "Nomad is a Emacs-like web browser that consists of a modular
feature-set, fully programmable in Guile Scheme.")
    (license license:gpl3+)))

(define-public guile-cv
  (package
    (name "guile-cv")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/guile-cv/guile-cv-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00620zxm1rxlws7vn1zp2zzcb6y6r3szzj6b4b9fyjb86k972izb"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list "GUILE_AUTO_COMPILE=0") ; to prevent guild warnings
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-guile-site-directory
            (lambda _
              (substitute* "configure.ac"
                (("SITEDIR=\"\\$datadir/guile-cv\"")
                 "SITEDIR=\"$datadir/guile/site/$GUILE_EFFECTIVE_VERSION\"")
                (("SITECCACHEDIR=\"\\$libdir/guile-cv/")
                 "SITECCACHEDIR=\"$libdir/"))))
          (add-after 'unpack 'substitute-libs
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "cv/init.scm"
                (("\\(dynamic-link \"libvigra_c\"\\)")
                 (string-append "(dynamic-link \""
                                (assoc-ref inputs "vigra-c")
                                "/lib/libvigra_c\")"))
                (("\\(dynamic-link \"libguile-cv\"\\)")
                 (format #f "~s"
                         `(dynamic-link
                           (format #f "~alibguile-cv"
                                   (if (getenv "GUILE_CV_UNINSTALLED")
                                       ""
                                       ,(string-append #$output "/lib/")))))))
              (setenv "GUILE_CV_UNINSTALLED" "1")
              ;; Only needed to satisfy the configure script.
              (setenv "LD_LIBRARY_PATH"
                      (string-append (assoc-ref inputs "vigra-c") "/lib")))))))
    (inputs
     (list vigra vigra-c guile-3.0))
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-booktabs
                  texlive-iwona
                  texlive-lm
                  texlive-siunitx
                  texlive-standalone
                  texlive-xcolor))
           pkg-config
           autoconf
           automake
           texinfo
           libtool
           gettext-minimal))
    (propagated-inputs
     (list guile-lib))
    (home-page "https://www.gnu.org/software/guile-cv/")
    (synopsis "Computer vision library for Guile")
    (description "Guile-CV is a Computer Vision functional programming library
for the Guile Scheme language.  It is based on Vigra (Vision with Generic
Algorithms), a C++ image processing and analysis library.  Guile-CV contains
bindings to Vigra C (a C wrapper to most of the Vigra functionality) and is
enriched with pure Guile Scheme algorithms, all accessible through a nice,
clean and easy to use high level API.")
    (license license:gpl3+)))

(define-public guile-ffi-cblas
  (let ((commit "4458d50f84786d7ace0181c6588345eed7474996")
        (revision "0"))
    (package
      (name "guile-ffi-cblas")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/lloda/guile-ffi-cblas")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "050s0lq64v286hkxqczkfkx3fp1vr3jm5w236hxx67br9najb1cp"))))
      (build-system guile-build-system)
      (arguments
       (list #:source-directory "mod"
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'set-blas-file-name
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "mod/ffi/cblas.scm"
                       (("\"libcblas\"")
                        (string-append "\""
                                       (search-input-file
                                        inputs "/lib/libopenblas.so")
                                       "\"")))))
                 (add-after 'build 'check
                   (lambda _
                     (invoke "guile" "-C" "mod" "-L" "mod"
                             "test/test-ffi-cblas.scm"))))))
      (native-inputs (list guile-3.0))
      (inputs (list openblas))
      (synopsis "Guile bindings for CBLAS, the linear algebra library")
      (description
       "This package provides Guile FFI bindings for CBLAS, the library of
linear algebra subprograms.

To use the bindings, import @code{(ffi cblas)}.  CBLAS will be loaded from the
default dynamic library path.  There are up to three bindings for each
function: raw, typed, and functional.")
      (license license:lgpl3+))))

(define-public guile-gsl
  (let ((commit "d33de9219a167561132721ce79c94bcaf67724b0")
        (revision "2"))
    (package
      (name "guile-gsl")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aartaka/guile-gsl")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02ngki3z64cs5mabs61vnx2chagcc8srmgfvccpr4zkn36fw3cx8"))))
      (build-system guile-build-system)
      (arguments
       (list
        #:source-directory "modules"
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'substitute-gsl-so
              (lambda _
                (let ((gsl (string-append #$(this-package-input "gsl")
                                          "/lib/libgsl.so"))
                      (gslcblas (string-append #$(this-package-input "gsl")
                                               "/lib/libgslcblas.so")))
                  (substitute* '("modules/gsl/core.scm")
                    (("libgsl.so") gsl)
                    (("libgslcblas.so") gslcblas))))))))
      (native-inputs (list guile-3.0))
      (inputs (list guile-3.0 gsl))
      (home-page "https://github.com/aartaka/guile-gsl")
      (synopsis "Bindings for GNU Scientific library in Guile")
      (description
       "This package provides a Guile Scheme wrapper for @code{libgsl.so}.
Implements
@itemize
@item GSL vectors.
@item Matrices.
@item BLAS operations.
@item Eigensystem solutions.
@item One-dimensional root solvers.
@end itemize")
      (license license:gpl3+))))

(define-public guile-ffi-fftw
  (let ((commit "294ad9e7491dcb40026d2fec9be2af05263be1c0")
        (revision "2"))
    (package
      (name "guile-ffi-fftw")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lloda/guile-ffi-fftw")
                      (commit commit)))
                (file-name (git-file-name "guile-ffi-fftw" version))
                (sha256
                 (base32
                  "08j40a5p6a8pgvhffmzb5rfdnrav2mksy3gfjkdqy93jfj1z5afg"))))
      (build-system guile-build-system)
      (arguments
       `(#:source-directory "mod"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'prepare-build
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "mod/ffi/fftw.scm"
                 (("\\(getenv \"GUILE_FFI_FFTW_LIBFFTW3_PATH\"\\)")
                  (format #f "\"~a/lib\"" (assoc-ref inputs "fftw"))))
               #t))
           (add-after 'build 'check
             (lambda _
               (invoke "guile" "-L" "mod"
                       "-s" "test/test-ffi-fftw.scm"))))))
      (inputs
       (list fftw guile-2.2))
      (home-page "https://github.com/lloda/guile-ffi-fftw/")
      (synopsis "Access FFTW through Guile's FFI")
      (description "This is a minimal set of Guile FFI bindings for the FFTW
library's ‘guru interface’.  It provides two functions: @code{fftw-dft! rank
sign in out} and @code{fftw-dft rank sign in}.  These bindings being minimal,
there is no support for computing & reusing plans, or split r/i transforms, or
anything other than straight complex DFTs.")
      (license license:lgpl3+))))

(define-public srfi-64-driver
  (package
    (name "srfi-64-driver")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ngyro.com/srfi-64-driver/"
                                  "srfi-64-driver-" version ".tar.gz"))
              (sha256
               (base32
                "188b6mb7sjjg0a8zldikinglf40ky8mg8rwh5768gjmch6gkk3ph"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'build))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-2.2))
    (home-page "https://ngyro.com/software/srfi-64-driver.html")
    (synopsis "Automake test driver for SRFI 64 test suites")
    (description "This package provides an Automake test driver that can
run SRFI 64 test suites.  It gives Automake insight into the individual
tests being run, resulting clearer and more specific output.")
    (license license:gpl3+)))

(define-public guile-semver
  (package
    (name "guile-semver")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ngyro.com/guile-semver/"
                                  "guile-semver-" version ".tar.gz"))
              (sha256
               (base32
                "109p4n39ln44cxvwdccf9kgb96qx54makvd2ir521ssz6wchjyag"))))
    (build-system gnu-build-system)
    (native-inputs
     (list guile-3.0 pkg-config))
    (inputs
     (list guile-3.0))
    (home-page "https://ngyro.com/software/guile-semver.html")
    (synopsis "Semantic Versioning (SemVer) for Guile")
    (description "This Guile library provides tools for reading,
comparing, and writing Semantic Versions.  It also includes ranges in
the style of the Node Package Manager (NPM).")
    (license license:gpl3+)))

(define-public guile2.2-semver
  (package
    (inherit guile-semver)
    (name "guile2.2-semver")
    (native-inputs
     (list guile-2.2 pkg-config))
    (inputs
     (list guile-2.2))))

(define-public guile-hashing
  (package
    (name "guile-hashing")
    (version "1.2.0")
    (home-page "https://gitlab.com/weinholt/hashing")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "f138deaec38d54ddb621c082764ece276deebe7f")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1362d3lmpv7slmv1zmr9wy8panq9sjr9787gc2hagd646mpsfpkl"))))
    (build-system guile-build-system)
    (arguments
     `(#:modules ((guix build guile-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw))
       #:implicit-inputs? #f                      ;needs nothing but Guile
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'move-sls-files
                    (lambda _
                      ;; Move the source under hashing/ in order to match
                      ;; module names, and rename .sls files to .scm.
                      (define (target file)
                        (string-append "hashing/" file))

                      (define (sls->scm sls)
                        (string-append (string-drop-right sls 4)
                                       ".scm"))

                      (mkdir "hashing")
                      (for-each (lambda (file)
                                  (rename-file file (sls->scm file)))
                                (find-files "." "\\.sls$"))
                      (for-each (lambda (file)
                                  (rename-file file (target file)))
                                (scandir "." (cut string-suffix? ".scm" <>)))
                      (rename-file "private" "hashing/private")
                      #t)))))
    (native-inputs
     (list guile-3.0))
    (synopsis "Cryptographic hash functions implemented in Scheme")
    (description
     "The @code{(hashing @dots{})} modules implement cryptographic hash
functions in pure R6RS Scheme: CRC, HMAC, MD5, SHA-1, and SHA-2 (SHA-256,
SHA-512).")
    (license license:expat)))

(define-public guile2.2-hashing
  (package
    (inherit guile-hashing)
    (name "guile2.2-hashing")
    (native-inputs
     (list guile-2.2))))

(define-public guile-packrat
  (package
    (name "guile-packrat")
    (version "0.1.1")
    (home-page "https://gitlab.com/weinholt/packrat")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit "4201ebe741b605db58a21d70195cfb7db3c38eae")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aga17164fkhbgllqc7ni6fk5zl8mkmgkl5zcsy67x7ngpyalbby"))))
    (build-system guile-build-system)
    (arguments
     `(#:implicit-inputs? #f                      ;needs nothing but Guile
       #:compile-flags '("--r6rs" "-Wunbound-variable" "-Warity-mismatch")
       #:not-compiled-file-regexp "/extensible\\.scm$"
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'no-srfi-23
                    (lambda _
                      (substitute* "packrat.sls"
                        (("\\(srfi :23 error\\)")
                         (object->string '(only (guile) error))))
                      #t)))))
    (native-inputs
     (list guile-3.0))
    (synopsis "Packrat parser library in R6RS Scheme")
    (description
     "This is an R6RS Scheme adaptation of the
@uref{https://bford.info/packrat/, packrat parsing}.  Packrat parsing is a
memoizing, backtracking, recursive-descent parsing technique that runs in time
and space linear in the size of the input text.")
    (license license:expat)))

(define-public guile-ac-d-bus
  (package
    (name "guile-ac-d-bus")
    (version "1.0.0-beta.0")
    (home-page "https://gitlab.com/weinholt/ac-d-bus/")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rl809qimhgz6b0rixakb42r2l4g53jr09a2g0s1hxgab0blz0kb"))
              (patches (search-patches "guile-ac-d-bus-fix-tests.patch"))))
    (build-system guile-build-system)
    (arguments
     (list
      #:compile-flags #~(list "--r6rs" "-Wunbound-variable" "-Warity-mismatch")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'adjust-for-guile
            (lambda _
              ;; Adjust source file names for Guile.
              (define (guile-sls->sls file)
                (string-append (string-drop-right
                                file (string-length ".guile.sls"))
                               ".sls"))

              ;; Remove files targeting other implementations: *.mosh.sls,
              ;; etc.
              (for-each delete-file
                        (find-files
                         "compat"
                         (lambda (file stat)
                           (not (string-contains file ".guile.")))))

              ;; Rename *.guile.sls to *.sls so the ".guile" bit does not
              ;; appear in .go file names.
              (for-each (lambda (file)
                          (rename-file file (guile-sls->sls file)))
                        (find-files "compat" "\\.guile\\.sls"))

              ;; Move directories under d-bus/ to match module names.
              (mkdir "d-bus")
              (for-each (lambda (directory)
                          (rename-file directory
                                       (string-append "d-bus/"
                                                      directory)))
                        '("compat" "protocol"))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "docs"
                (invoke "makeinfo" "ac-d-bus"))))
          (add-after 'build-doc 'check
            (lambda* (#:key (tests? #t) #:allow-other-keys)
              (when tests?
                ;; There is no locale for the ö character, which crashes
                ;; substitute*; reset the conversion strategy to workaround it.
                (with-fluids ((%default-port-conversion-strategy 'substitute))
                  (substitute* (find-files "tests")
                    (("#!/usr/bin/env scheme-script")
                     (string-append "#!" (which "guile")))))
                (invoke "./run-tests.sh"))))
          (add-after 'build-doc 'install-doc
            (lambda _
              (install-file "docs/ac-d-bus.info"
                            (string-append #$output "/share/info")))))))
    (native-inputs
     (list bash-minimal guile-3.0 texinfo))
    (propagated-inputs
     (list guile-packrat))
    (synopsis "D-Bus protocol implementation in R6RS Scheme")
    (description
     "AC/D-Bus is an implementation of the D-Bus wire protocol.  D-Bus is an
interprocess communication protocol popular on GNU/Linux systems to
communicate with a variety of services.  Originally designed for desktop
environments, it is now used by programs like VLC media player, BlueZ,
NetworkManager, Pulseaudio, systemd (including logind and resolved), Polkit,
gnome-keyring, and many more.")
    (license license:expat)))

(define-public guile-webutils
  (let ((commit "d309d65a85247e4f3cea63a17defd1e6d35d821f")
        (revision "1"))
    (package
      (name "guile-webutils")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://notabug.org/cwebber/guile-webutils.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1a3bblk5zaldkkxn0a94s544drqm0w2i5fsjpghagd64m149blf0"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake pkg-config texinfo))
      (inputs
       (list guile-3.0))
      (propagated-inputs
       (list guile-irregex guile-gcrypt))
      (home-page "https://notabug.org/cwebber/guile-webutils")
      (synopsis "Web application authoring utilities for Guile")
      (description
       "This package provides tooling to write web applications in Guile, such
as signed sessions, multipart message support, etc.")
      (license license:gpl3+))))

(define-public guile2.2-webutils
  (package
    (inherit guile-webutils)
    (name "guile2.2-webutils")
    (inputs
     (list guile-2.2))
    (propagated-inputs
     (list guile2.2-irregex guile2.2-gcrypt))))

(define-public guile-web-driver-ng
  (package
    (name "guile-web-driver-ng")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artyom-poptsov/guile-web-driver-ng")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0saljgf3kj3p9b1mk5211s8np2vwkzf072xp6j2xnc10vdn891ja"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf
                         automake
                         pkg-config
                         texinfo
                         ;; needed when cross-compiling.
                         guile-lib
                         guile-json-4
                         guile-gnutls
                         guile-3.0))
    (propagated-inputs (list guile-json-4 guile-gnutls guile-lib guile-3.0
                             inetutils))
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'strip))))
    (home-page "https://github.com/artyom-poptsov/guile-web-driver-ng")
    (synopsis "Web driver (Selenium) client for Guile")
    (description
     "This is a web-driver, or Selenium 2, client.  It's purpose is to automate
browsers, specifically for automatic web server testing.  Chrome or Firefox can be
used as the automated browsers, or it can connect to arbitrary server providing
webdriver interface.  The client implements most of the WebDriver
@url{https://www.w3.org/TR/webdriver2/, specification}.

@code{guile-web-driver-ng} also provides a proxy implemented as a Guile module.  If
configured, the proxy can intercept and modify HTTP/HTTPS traffic (for example, add,
delete and replace HTTP headers) which is useful for Selenium WebDriver as it does
not provide a way to change the headers on its own.")
    (license license:gpl3+)))

(define-public guile-lens
  (let ((commit "14b15d07255f9d3f55d40a3b750d13c9ee3a154f")
        (revision "0"))
    (package
      (name "guile-lens")
      (version (git-version "0.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/a-sassmannshausen/guile-lens.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0w8jzqyla56yrgj7acsgd4nspyir6zgp3vgxid4xmwhg9wmf1ida"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'run-hall
             (lambda _
               (setenv "HOME" "/tmp")   ; for ~/.hall
               (invoke "hall" "build-system" "-x"))))))
      (native-inputs
       (list autoconf
             automake
             guile-3.0
             guile-hall
             pkg-config
             texinfo))
      (home-page "https://gitlab.com/a-sassmannshausen/guile-lens.git")
      (synopsis "Composable lenses for data structures in Guile")
      (description
       "Guile-Lens is a library implementing lenses in Guile.  The library is
currently a re-implementation of the lentes library for Clojure.  Lenses
provide composable procedures, which can be used to focus, apply functions
over, or update a value in arbitrary data structures.")
      (license license:gpl3+))))

(define-public guile2.2-lens
  (package
    (inherit guile-lens)
    (name "guile2.2-lens")
    (native-inputs
     (modify-inputs (package-native-inputs guile-lens)
       (replace "guile" guile-2.2)))))

(define-public guile-xapian
  (package
    (name "guile-xapian")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://guile-xapian.systemreboot.net/releases/guile-xapian-"
                           version ".tar.lz"))
       (sha256
        (base32
         "1szjwha8rin65mdm0dviha4pybiij89pq1wfjmrir1js4w5mk1hr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0"))) ; to prevent guild warnings
    (inputs
     (list guile-3.0 xapian zlib))
    (native-inputs
     (list pkg-config
           lzip
           swig))
    (propagated-inputs
     (list guile-lib))
    (home-page "https://guile-xapian.systemreboot.net")
    (synopsis "Guile bindings for Xapian")
    (description "@code{guile-xapian} provides Guile bindings for Xapian, a
search engine library.  Xapian is a highly adaptable toolkit which allows
developers to easily add advanced indexing and search facilities to their own
applications.  It has built-in support for several families of weighting
models and also supports a rich set of boolean query operators.")
    (license license:gpl2+)))

(define-public guile2.2-xapian
  (package
    (inherit guile-xapian)
    (name "guile2.2-xapian")
    (inputs
     (modify-inputs (package-inputs guile-xapian)
       (replace "guile" guile-2.2)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs guile-xapian)
       (replace "guile-lib" guile2.2-lib)))))

(define-public guile-torrent
  (package
    (name "guile-torrent")
    (version "0.1.3")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url
                           "https://github.com/o-nly/torrent")
                          (commit version)))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      "1yiagi55ncq1x7s9n7salzywjm4l96y3n7y3s47a9anvz87mrmim"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           guile-2.2
           texinfo
           perl
           pkg-config))
    (propagated-inputs
     (list guile2.2-gcrypt))
    (home-page "https://github.com/o-nly/torrent")
    (synopsis "Torrent library for GNU Guile")
    (description "This package provides facilities for working with
@code{.torrent} or metainfo files.  Implements a bencode reader and writer
according to Bitorrent BEP003.")
    (license license:gpl3+)))

(define-public guile-ts
  (package
    (name "guile-ts")
    (version "0.3.0")
    (source (origin (method git-fetch)
                    (uri (git-reference
                          (url
                           "https://github.com/Z572/guile-ts")
                          (commit (string-append "v" version))))
                    (file-name (git-file-name name version))
                    (sha256
                     (base32
                      "1p98mq7ik3kw7h7ki60253qxdw5qs13f1xldjp37c3s7zvhn0nac"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list "GUILE_AUTO_COMPILE=0")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-extension-path
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute*
                       (find-files "." ".*\\.scm")
                     (("\\(load-extension \"libguile_ts\" *\"(.*)\"\\)" _ o)
                      (string-append
                       (object->string
                        `(or (false-if-exception
                              (load-extension "libguile_ts" ,o))
                             (load-extension
                              ,(string-append
                                #$output
                                "/lib/libguile_ts.so")
                              ,o)))))))))))
    (native-inputs
     (list autoconf automake libtool texinfo pkg-config guile-3.0))
    (inputs
     (list guile-3.0 tree-sitter))
    (native-search-paths
     (list (search-path-specification
            (variable "TREE_SITTER_GRAMMAR_PATH")
            (files '("lib/tree-sitter")))))
    (synopsis "Guile bindings to the Tree-sitter parsing library")
    (description "This package provides Guile bindings to the Tree-sitter
parsing library.")
    (home-page "https://github.com/Z572/guile-ts")
    (license license:gpl3+)))

(define-public guile-irc
  (let ((commit "7d08ce6fdcb87ac668c5d3bfd5584247805507bb")
        (revision "1"))
    (package
      (name "guile-irc")
      (version (git-version "0.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rekado/guile-irc")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1jx8704200l29ndg9bfyamgxrzknya0f0vwb2sxhd0k3b8r94avw"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags '("--enable-gnutls=yes")
         #:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'fix-autogen.sh
             (lambda _
               (substitute* "autogen.sh"
                 ((" #!") "#!")))))))
      (native-inputs
       (list autoconf automake texinfo pkg-config))
      (inputs
       (list gnutls guile-gnutls guile-3.0))
      (home-page "https://github.com/rekado/guile-irc")
      (synopsis "IRC library for Guile")
      (description "This package provides a Guile library for @dfn{Internet
Relay Chat} (IRC).")
      ;; Some file headers incorrectly say LGPLv2+.
      (license license:lgpl2.1+))))

(define-public guile-websocket
  (package
    (name "guile-websocket")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.dthompson.us/guile-websocket/"
                                  "guile-websocket-" version ".tar.gz"))
              (sha256
               (base32
                "143ng1x5xwy218wd1svj718ikqnrglwsywyzpd3ap9jnivw66g7f"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags
       '("GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list guile-3.0 guile-gnutls))
    (synopsis "Websocket server/client for Guile")
    (description "Guile-websocket provides an implementation of the
WebSocket protocol as defined by RFC 6455.")
    (home-page "https://dthompson.us/projects/guile-websocket.html")
    (license license:lgpl3+)))

(define-public guile-gemini
  (package
    (name "guile-gemini")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flatwhatson/guile-gemini")
                    (commit "6d70c5dc6b35c26103f560f7e63c770a424dbca2")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rvqqirwsqn3nspr6z1smfp3rj7gc7hfq7cmadx7zxbr3yg5y04x"))))
    (build-system guile-build-system)
    (arguments
     '(#:source-directory "src"))
    (inputs (list guile-3.0-latest))
    (propagated-inputs (list guile-fibers-1.1 guile-gnutls))
    (home-page "https://github.com/flatwhatson/guile-gemini")
    (synopsis "Guile implementation of the Gemini protocol")
    (description
     "Guile Gemini is an implementation of the Gemini protocol in Guile Scheme,
providing both client and server functionality.  It uses GnuTLS to meet
Gemini's TLS requirements, and Guile Fibers for concurrency.")
    (license license:lgpl3+)))

(define-public guile-rdf
  (package
    (name "guile-rdf")
    (version "1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://framagit.org/tyreunom/guile-rdf")
               (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0dwn3app1fscbpmpgvjs5jy1y0gwy3j5gdx8br79af6a88zjlnqf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)); tests require network
    (inputs
     (list guile-3.0))
    (native-inputs
     (list automake autoconf pkg-config texinfo))
    (home-page "https://framagit.org/tyreunom/guile-rdf")
    (synopsis "Guile implementation of the RDF abstract and concrete syntaxes")
    (description "Guile RDF is an implementation of the RDF (Resource Description
Framework) format defined by the W3C for GNU Guile.  RDF structures include
triples (facts with a subject, a predicate and an object), graphs which are
sets of triples, and datasets, which are collections of graphs.

RDF specifications include the specification of concrete syntaxes and of
operations on graphs.  This library implements some basic functionalities,
such as parsing and producing turtle and nquads syntax, as well as
manipulating graphs and datasets.")
    (license license:gpl3+)))

(define-public guile-jsonld
  (package
    (name "guile-jsonld")
    (version "1.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://framagit.org/tyreunom/guile-jsonld")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ryyvh71899z2inivqglb8d78zzp1sd0wv9a56kvcmrxf1966z6r"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f))                ; require network
    (propagated-inputs
     (list guile-gnutls guile-json-4 guile-rdf))
    (inputs
     (list guile-3.0))
    (native-inputs
     (list automake autoconf pkg-config texinfo))
    (home-page "https://framagit.org/tyreunom/guile-jsonld")
    (synopsis "Guile implementation of the JsonLD API specification")
    (description
     "Guile JsonLD is an implementation of the JsonLD (Json for Linked Data)
API defined by the W3C for GNU Guile.  It allows you to express links between
data, in a way that is very similar to WikiData or RDF for instance.  An
object can have relations (in the form of an IRI) that relates it to one or
more objects or strings, represented by a Json object or an IRI.")
    (license license:gpl3+)))

(define-public guile-struct-pack
  (package
    (name "guile-struct-pack")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/weinholt/struct-pack")
             (commit "11b71963793ed4a3bf761efdd83cf2fe123239ee")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hd72m821pahjphzyjn26i55542v8makr55xzjll2cycja4wsbc1"))))
    (build-system guile-build-system)
    (arguments
     `(#:compile-flags '("--r6rs" "-Wunbound-variable" "-Warity-mismatch")
       #:modules ((guix build guile-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'move-sls-files
                    (lambda _
                      ;; Move files under a struct/ directory to reflect the
                      ;; module hierarchy.
                      (define dst-folder "struct")
                      (define (target file)
                        (string-append dst-folder "/" file))
                      (define files
                        (scandir "." (negate (cut member <> '("." "..")))))
                      (mkdir dst-folder)
                      (for-each (lambda (file)
                                  (rename-file file (target file)))
                                files)
                      #t)))))
    (native-inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/weinholt/struct-pack")
    (synopsis "R6RS library for working with packed byte structures")
    (description
     "This is an R6RS library for working with packed byte structures.  It is
similar to struct in Python or pack and unpack in Perl.")
    (license license:expat)))

(define-public guile-machine-code
  (package
    (name "guile-machine-code")
    (version "2.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/weinholt/machine-code")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yjzpg5p082kg4vaqlwbwddrrhxyxar6gsx9ql72hpwah4ka82h5"))))
    (build-system guile-build-system)
    (arguments
     `(#:compile-flags '("--r6rs" "-Wunbound-variable" "-Warity-mismatch")
       #:modules ((guix build guile-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'move-sls-files
                    (lambda _
                      ;; Move files under a struct/ directory to reflect the
                      ;; module hierarchy.
                      (define dst-folder "machine-code")
                      (define (target file)
                        (string-append dst-folder "/" file))
                      (define files
                        (scandir "." (negate (cut member <> '("." "..")))))
                      (mkdir dst-folder)
                      (for-each (lambda (file)
                                  (rename-file file (target file)))
                                files)
                      #t)))))
    (native-inputs
     (list guile-3.0))
    (propagated-inputs
     (list guile-struct-pack))
    (home-page "https://gitlab.com/weinholt/machine-code")
    (synopsis "Tools that relate to machine code and object formats")
    (description
     "This project is about the development of tools that relate to machine
code and object formats; for all architectures.  Here you'll find libraries
for working with binary code: assembly, disassembly, instruction tables,
object formats and related areas.")
    (license license:expat)))

(define-public guile-laesare
  (package
    (name "guile-laesare")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/weinholt/laesare")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15q619gzw717r8r1ql23zfdaibpnp9qqs96032vdc3rj74msxc92"))))
    (build-system guile-build-system)
    (arguments
     `(#:compile-flags '("--r6rs" "-Wunbound-variable" "-Warity-mismatch")
       #:modules ((guix build guile-build-system)
                  (guix build utils)
                  (srfi srfi-26)
                  (ice-9 ftw))
       #:phases (modify-phases %standard-phases
                  (add-before 'build 'move-sls-files
                    (lambda _
                      ;; Move files under a laesare directory to reflect
                      ;; the module hierarchy.
                      (define dst-folder "laesare")
                      (define (target file)
                        (string-append dst-folder "/" file))
                      (define files
                        (scandir "." (negate (cut member <> '("." "..")))))
                      (mkdir dst-folder)
                      (for-each (lambda (file)
                                  (rename-file file (target file)))
                                files)
                      #t)))))
    (native-inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/weinholt/laesare")
    (synopsis "R6RS Scheme library that provides a reader")
    (description
     "This is an R6RS Scheme library that provides a reader with some extra
features not found in the standard read procedure such as a compatible mode
with support for other RnRS standards and a tolerant mode that continues on
errors.")
    (license license:expat)))

(define-public guile-avahi
  (package
    (name "guile-avahi")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sv.gnu.org/git/guile-avahi.git/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pxdi13kr4ylpms0xyf3xwwbhg025k7a2liwnbha1gw6ls58xgv2"))))
    (build-system gnu-build-system)
    (inputs
     (list guile-3.0 avahi))
    (native-inputs
     (list autoconf automake libtool pkg-config texinfo guile-3.0))
    (synopsis "Guile bindings to Avahi")
    (description
     "This package provides bindings for Avahi.  It allows programmers to
use functionalities of the Avahi client library from Guile Scheme programs.
Avahi itself is an implementation of multicast DNS (mDNS) and DNS Service
Discovery (DNS-SD).")
    (home-page "https://www.nongnu.org/guile-avahi/")
    (license license:lgpl3+)))

(define-public guile-dns
  (package
    (name "guile-dns")
    (version "0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.lysator.liu.se/hugo/guile-dns")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18skivracv6jh1zab9dknkcpbizc416n0pb2mcwb20dpzc2md9yf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "PREFIX=" #$output)
                                ;; Prevent guild warnings.
                                "GUILE_AUTO_COMPILE=0"
                                ;; Make tests verbose and disable coverage
                                ;; report. The coverage report fails on
                                ;; i686-linux.
                                "TEST_FLAGS=--verbose")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-makefile
                 (lambda _
                   (substitute* "Makefile"
                     ;; CURDIR is a standard GNU Make variable. Prefer it to
                     ;; PWD. PWD is set by the shell and is absent in the
                     ;; build process.
                     (("PWD") "CURDIR")
                     ;; Install info file at share/info, not at share.
                     (("share doc") "share/info doc"))))
               (delete 'configure))))
    (inputs
     (list guile-3.0))
    (native-inputs
     (list texinfo))
    (home-page "https://git.lysator.liu.se/hugo/guile-dns")
    (synopsis "Guile DNS library")
    (description "@code{guile-dns} is a DNS library written in pure Guile
Scheme.")
    (license license:gpl3+)))

(define-public guile-jwt
  (package
    (name "guile-jwt")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aconchillo/guile-jwt")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04c5aibcfb83bl194j27cw5x8bfbqvq5939brckaaapbrff6il6k"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (propagated-inputs
     (list guile-gcrypt guile-json-4))
    (inputs
     (list guile-3.0))
    (home-page "https://github.com/aconchillo/guile-jwt")
    (synopsis "JSON Web Token library for Guile")
    (description
     "guile-jwt is a JSON Web Token module for Guile.  JSON Web Tokens are an
open, industry standard (RFC 7519) method for representing claims securely
between two parties.  guile-jwt allows you to decode, verify and generate
JWT.  Supported algorithms: HS256, HS384, HS512.")
    (license license:gpl3+)))

(define-public guile-sodium
  (package
    (name "guile-sodium")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://inqlab.net/git/guile-sodium.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "189jsj87hycs57a54x0b9lifwvhr63nypb9vfxdrq7rwrpcvi5f8"))))
    (build-system gnu-build-system)
    (arguments `())
    (native-inputs
      (list autoconf automake pkg-config texinfo))
    (inputs (list guile-3.0))
    (propagated-inputs (list libsodium))
    (synopsis "Guile bindings to the libsodium cryptographic library")
    (description
     "This package provides Guile bindings to the libsodium cryptographic library
which provides core cryptographic primitives needed to build higher-level
tools.")
    (home-page "https://inqlab.net/git/guile-sodium.git")
    (license license:gpl3+)))

(define-public guile-eris
  (package
    (name "guile-eris")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/eris/guile-eris.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "0d4wbjwwaxk0zn5gjhl86qhvk1aisgzp1vnvy4xbvrv5ydqpgyqm"))))
    (build-system gnu-build-system)
    (arguments '())
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           ;; test dependency
           guile-srfi-180
           guile-quickcheck))
    (inputs (list guile-3.0))
    (propagated-inputs
     (list guile-sodium))
    (synopsis "Guile implementation of the Encoding for Robust Immutable Storage (ERIS)")
    (description
     "Guile-ERIS is a Guile implementation of the @url{http://purl.org/eris,
Encoding for Robust Immutable Storage (ERIS)}.  ERIS allows arbitrary content
to be encoded into uniformly sized, encrypted blocks that can be reassembled
using a short read-capability.")
    (home-page "https://codeberg.org/eris/guile-eris")
    (license license:gpl3+)))

(define-public guile-rsv
  (let ((commit "41b04c85eef31d4d51001c6d66e8fd339fcc614c")
        (revision "1"))
    (package
      (name "guile-rsv")
      (version (git-version "0.2.0" revision commit))
      (home-page "https://codeberg.org/kakafarm/guile-rsv/")
      (source
       (origin
         (uri (git-reference (url home-page) (commit commit)))
         (method git-fetch)
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1w9jbkpmh13zrxkj915nm3l537smm0jsrdzrzcxylb6w59vqpw6l"))))
      (inputs (list guile-3.0 bash))
      (build-system guile-build-system)
      (arguments
       (list
        #:phases #~(modify-phases %standard-phases
                     (add-after 'build 'link-and-wrap-executable
                       (lambda _
                         (let* ((bin (string-append #$output "/bin"))
                                ;; bin directory for PATH.
                                (site-version (target-guile-effective-version))
                                (scm (string-append "/share/guile/site/"
                                                    site-version))
                                (go (string-append "/lib/guile/" site-version
                                                   "/site-ccache")))
                           (mkdir-p bin)
                           (for-each (lambda (command-name)
                                       (let ((source-script (string-append #$output
                                                                           scm "/"
                                                                           command-name
                                                                           ".scm"))
                                             (target-command (string-append
                                                              bin "/"
                                                              command-name)))
                                         (symlink source-script target-command)
                                         (wrap-program target-command
                                           #:sh (which "bash")
                                           `("GUILE_LOAD_PATH" prefix
                                             (,(string-append #$output scm)))
                                           `("GUILE_LOAD_COMPILED_PATH" prefix
                                             (,(string-append #$output go))))))
                                     (list "scm2rsv" "rsv2scm"))))))))
      (synopsis "Reading and writing @acronym{RSV, rows of string values} data format")
      (description
       "R7RS-small Scheme library for reading and writing @acronym{RSV, rows
of string values} data format, a very simple binary format for storing tables
of strings.  It is a competitor for CSV (Comma Separated Values) and TSV (Tab
Separated Values).  Its main benefit is that the strings are represented as
Unicode encoded as UTF-8, and the value and row separators are byte values
that are never used in UTF-8, so the strings do not need any error prone
escaping and thus can be written and read verbatim.

The RSV format is specified in
@url{https://github.com/Stenway/RSV-Specification}.")
      (license license:gpl3+))))

(define-public guile-r6rs-protobuf
  (package
    (name "guile-r6rs-protobuf")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/joolean/r6rs-protobuf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xmn7jlr1xiqgd35klq910p1bnil1iwdvqxkjr3zzml3spy8p2aa"))))
    (build-system guile-build-system)
    (arguments
     `(#:compile-flags '("--r6rs")))
    (inputs
     (list guile-3.0))
    (home-page "https://gitlab.com/joolean/r6rs-protobuf/")
    (synopsis "Scheme implementation of Protocol Buffers")
    (description
     "This project provides a pure Scheme implementation of Protocol Buffers,
including parsing and code generation.")
    (license license:gpl3+)))

(define-public guile-shapefile
  (package
    (name "guile-shapefile")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HugoNikanor/guile-shapefile")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zvrpc8bshw9w0vhdpmhv00j07mzsdyg2f9hfabr83v08zhfi8ml"))))
    (build-system guile-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-pre-generated-docs
           (lambda _
             (delete-file-recursively "docs")
             #t))
         (add-after 'build 'install-info-documentation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((share (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append share "/doc/" ,name "-" ,version))
                    (info (string-append share "/info/"))
                    (makeinfo (search-input-file inputs
                                                 "/bin/makeinfo")))
               (invoke makeinfo "guile-shapefile.texi" "-o" info)
               #t))))))
    (inputs
     (list guile-3.0))
    (native-inputs
     (list texinfo))
    (home-page "https://github.com/HugoNikanor/guile-shapefile")
    (synopsis "Parse shapefiles in Guile")
    (description
     "Guile Shapefile is a Guile library for reading shapefiles.")
    (license license:expat)))

(define-public guile-drmaa
  (package
    (name "guile-drmaa")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.elephly.net/software/guile-drmaa.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1pail39f3iwllcdma4pk4sxsaypplgb5zjyvjwqf5hdv8s3y211x"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config texinfo sed))
    (inputs
     (list guile-3.0))
    (propagated-inputs
     (list guile-bytestructures nyacc))
    (home-page "https://git.elephly.net/software/guile-drmaa.git")
    (synopsis "Guile bindings to DRMAA")
    (description "This project provides Guile bindings to the DRMAA library
version 1.  DRMAA is a resource management library implemented by HPC cluster
schedulers.")
    (license license:gpl3+)))

(define-public guile-libyaml
  (package
    (name "guile-libyaml")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mwette/guile-libyaml")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bssby1ri1vjll2rvi8b33xr2ghwjyxsd4yc15najj3h8n2ss87i"))
       (snippet #~(for-each delete-file
                            '("guix.scm" "demo1.yml" "demo1.scm")))))
    (build-system guile-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'build-ffi
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "GUILE_AUTO_COMPILE" "0")
              (invoke "guild" "compile-ffi" "--no-exec" "yaml/libyaml.ffi")
              (substitute* "yaml/libyaml.scm"
                (("dynamic-link \"libyaml\"")
                 (format #f "dynamic-link \"~a/lib/libyaml\""
                         (assoc-ref inputs "libyaml")))))))))
    (native-inputs (list gcc guile-3.0 nyacc))
    (inputs (list libyaml))
    (propagated-inputs (list guile-bytestructures nyacc))
    (home-page "https://github.com/mwette/guile-libyaml")
    (synopsis "Guile wrapper for libyaml")
    (description
     "This package provides a simple yaml module for Guile using the ffi-helper from
nyacc.")
    (license license:lgpl3+)))

(define-public schmutz
  (let ((commit "f8043e6c258d2e29d153bc37cb17b130fee0579f")
        (revision "2"))
    (package
      (name "schmutz")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arximboldi/schmutz")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0cgk0b27f1vik3wnv1cz47ip7d8rkmqxfdlqba4avi9h5fah7xrw"))))
      (build-system cmake-build-system)
      (arguments `(#:tests? #f))
      (native-inputs
       (list pkg-config))
      (inputs
       (list guile-3.0))
      (home-page "https://github.com/arximboldi/schmutz")
      (synopsis "Bind C++ code to Scheme")
      (description "Schmutz is a header-only library to declare Scheme bindings
for C++ code using a simple embedded DSL.  Think of it as @code{Boost.Python}
or @code{LuaBind} but for Scheme.")
      (license license:boost1.0))))

(define-public guile-cbor
  (package
    (name "guile-cbor")
    (version "0.1.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://inqlab.net/git/guile-cbor.git")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256 (base32 "0bdqg3ifayf7m2j09lqrgdfprbdaa67azf09bcq9b4k71inxfnxl"))))
    (build-system gnu-build-system)
    (arguments `())
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs (list guile-3.0))
    (synopsis "Guile implementation of CBOR")
    (description
     "The Concise Binary Object Representation (CBOR), as specified by RFC 8949, is
a binary data serialization format.  CBOR is similar to JSON but serializes to
binary which is smaller and faster to generate and parse.  This package provides
a Guile implementation of CBOR.")
    (home-page "https://inqlab.net/git/guile-cbor.git")
    (license license:gpl3+)))

(define-public guile-qr-code
  (package
    (name "guile-qr-code")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artyom-poptsov/guile-qr-code")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rs23xk6wpf618ln9n0aj3k6vhg4s80qpyp6z3j5gdrpfsgp7nf7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (delete 'strip))))
    (native-inputs (list autoconf
                         automake
                         pkg-config
                         texinfo
                         ;; needed when cross-compiling.
                         guile-3.0
                         guile-lib
                         guile-png))
    (inputs (list bash-minimal guile-3.0 guile-lib guile-png))
    (synopsis "Guile QR Code library")
    (description
     "GNU Guile QR code generator that can create QR codes from text and binary data.
The resulting QR codes can be rendered to ASCII art strings or to PNG images (using
@url{https://github.com/artyom-poptsov/guile-png, Guile-PNG} API.)")
    (home-page "https://github.com/artyom-poptsov/guile-qr-code")
    (license (list license:gpl3+ license:expat))))

(define-public guile-quickcheck
  (package
    (name "guile-quickcheck")
    (version "0.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://files.ngyro.com/"
                                  "guile-quickcheck/guile-quickcheck-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03mwi1l3354x52nar0zwhcm0x29yai9xjln4p4gbchwvx5dsr6fb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-3.0))
    (home-page "https://ngyro.com/software/guile-quickcheck.html")
    (synopsis "Randomized property-based testing for Guile")
    (description "Guile-Quickcheck is a library for random testing of program
properties inspired by ghc-quickcheck.  You can use it to express properties,
which functions should satisfy, as Scheme code and then check whether they hold
in a large number of randomly generated test cases.")
    (license license:gpl3+)))

(define-public guile-fslib
  (package
    (name "guile-fslib")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://notabug.org/ZelphirKaltstahl/guile-fslib/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "118d84p443w7hrslv8hjyhgws631ia08mggiyklkmk0b9plfdsvz"))))
    (build-system guile-build-system)
    (inputs
     (list guile-3.0))
    (home-page "https://notabug.org/ZelphirKaltstahl/guile-fslib")
    (synopsis "Helper functions for working with locations in file systems")
    (description
     "This package contains helper functions for working with file system
locations.")
    (license license:agpl3+)))

(define-public guile-netlink
  (package
    (name "guile-netlink")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://git.lepiller.eu/git/guile-netlink")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "181drjshcz7pi5zwydwd702h7v8p1nh50q6slsw9q372k2bzyl4a"))))
    (build-system gnu-build-system)
    (inputs
     (list guile-3.0))
    (native-inputs
     (list automake
           autoconf
           pkg-config
           guile-3.0 ;for 'guild compile' + guile.m4
           texinfo))
    (home-page "https://git.lepiller.eu/guile-netlink")
    (synopsis "Netlink protocol implementation for Guile")
    (description "Guile Netlink is a GNU Guile library providing an implementation
of the netlink protocol.

It provides a generic library for writing implementations of a netlink
protocol, a low-level rtnetlink implementation that uses that library and a
high-level API for network management that uses rtnetlink.")
    (license license:gpl3+)))

(define-public guile-gitlab
  (package
    (name "guile-gitlab")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artyom-poptsov/guile-gitlab")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version))
       (sha256
        (base32 "0srkmchd4kmfa7q65r6fdzwklhgdlck1ll0s7smzs8ddjdgz2lwm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0") ;to prevent guild warnings
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-program
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin"))
                             (guile-lib (assoc-ref inputs "guile2.2-lib"))
                             (json (assoc-ref inputs "guile2.2-json"))
                             (tls (assoc-ref inputs "guile2.2-gnutls"))
                             (version (target-guile-effective-version))
                             (scm (string-append "/share/guile/site/" version))
                             (go (string-append "/lib/guile/" version
                                                "/site-ccache")))
                        (wrap-program (string-append bin "/gitlab-cli")
                          `("GUILE_LOAD_PATH" prefix
                            (,(string-append out scm) ,(string-append
                                                        guile-lib scm)
                             ,(string-append json scm)
                             ,(string-append tls scm)))
                          `("GUILE_LOAD_COMPILED_PATH" prefix
                            (,(string-append out go) ,(string-append guile-lib
                                                       go)
                             ,(string-append json go)
                             ,(string-append tls go))))))))))
    (native-inputs (list autoconf automake pkg-config texinfo))
    (inputs (list bash-minimal guile-2.2 guile2.2-json guile2.2-lib
                  guile2.2-gnutls))

    (home-page "https://github.com/artyom-poptsov/guile-gitlab")
    (synopsis "Guile interface to GitLab")
    (description
     "This package provides bindings to the GitLab Community Edition REST API
as well as the @samp{gitlab-cli} command line tool for interacting with a
GitLab instance.")
    (license license:gpl3)))

(define-public guile-smc
  (package
    (name "guile-smc")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/artyom-poptsov/guile-smc")
             (commit (string-append "v" version))))
       (file-name (string-append name "-" version))
       (sha256
        (base32
         "1gjwz1l2ls4xkkgg4d2vw3a1klc4var03ab4k6lq1jifdvc8n51f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0")     ;to prevent guild warnings
       #:modules (((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-modules)
       #:imported-modules ((guix build guile-build-system)
                           ,@%default-gnu-imported-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'strip)
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out       (assoc-ref outputs "out"))
                    (bin       (string-append out "/bin"))
                    (guile-lib (assoc-ref inputs "guile-lib"))
                    (version   (target-guile-effective-version))
                    (scm       (string-append "/share/guile/site/"
                                              version))
                    (go        (string-append  "/lib/guile/"
                                               version "/site-ccache")))
               (wrap-program (string-append bin "/smc")
                 `("GUILE_LOAD_PATH" prefix
                   (,(string-append out scm)
                    ,(string-append guile-lib scm)))
                 `("GUILE_LOAD_COMPILED_PATH" prefix
                   (,(string-append out go)
                    ,(string-append guile-lib go))))))))))
    (native-inputs
     (list autoconf
           automake
           pkg-config
           texinfo
           help2man
           which
           ;; needed when cross-compiling.
           guile-3.0
           guile-lib))
    (inputs
     (list bash-minimal guile-3.0 guile-lib inetutils))
    (home-page "https://github.com/artyom-poptsov/guile-smc")
    (synopsis "GNU Guile state machine compiler")
    (description
     "Guile-SMC is a state machine compiler that allows users to describe
finite state machines (FSMs) in Scheme in terms of transition tables.  It is
capable to generate such transition tables from a @url{https://plantuml.com/,
PlantUML} state diagrams.

A transition table can be verified and checked for dead-ends and infinite
loops.  Also Guile-SMC FSMs gather statistics when they run.

Guile-SMC comes with a Scheme program called @command{smc} -- a state machine
compiler itself.  It produces a Scheme code for an FSM from the PlantUML
format.  This tool is meant to be called on a PlantUML file when a program
with a FSM is being built (for example, from a Makefile.)")
    (license license:gpl3)))

(define-public guile2.2-smc
  (package
    (inherit guile-smc)
    (name "guile2.2-smc")
    (native-inputs (modify-inputs (package-native-inputs guile-smc)
                     (replace "guile" guile-2.2)
                     (replace "guile-lib" guile2.2-lib)))
    (inputs (modify-inputs (package-inputs guile-smc)
              (replace "guile" guile-2.2)
              (replace "guile-lib" guile2.2-lib)))))

(define-public guile-ini
  (package
    (name "guile-ini")
    (version "0.5.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artyom-poptsov/guile-ini")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "191kz6xlci16b93m0s6vzvk5jbc89rx61aljnqmd81ha33nhk0yg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("GUILE_AUTO_COMPILE=0") ;to prevent guild warnings
       #:phases (modify-phases %standard-phases
                  (delete 'strip))))
    (native-inputs (list autoconf
                         automake
                         pkg-config
                         texinfo
                         ;; needed when cross-compiling.
                         guile-3.0
                         guile-lib
                         guile-smc))
    (inputs (list bash-minimal guile-3.0 guile-lib))
    (propagated-inputs (list guile-lib guile-smc))
    (home-page "https://github.com/artyom-poptsov/guile-ini")
    (synopsis "Guile library for INI format support")
    (description
     "@code{guile-ini} is a GNU Guile library for working with the
@url{https://en.wikipedia.org/wiki/INI_file, INI format}.  This library
provides API for reading and writing INI data.")
    (license license:gpl3)))

(define-public guile-schemetran
  (let ((commit "3f5e15273ee88ba60ad8caf2de6302ad2bab582b")
        (revision "1"))
    (package
      (name "guile-schemetran")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/codetk/schemetran")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1r4nq7wmy854hvbkcc23sidn4kq3p7r4p15y5czwvd52p9djff3m"))))
      (build-system guile-build-system)
      (arguments
       (list #:not-compiled-file-regexp "/doc/.*\\.scm$"
             #:source-directory "src"))
      (inputs
       (list guile-3.0))
      (home-page "https://gitlab.com/codetk/schemetran")
      (synopsis "Write Fortran in Scheme")
      (description
       "Fortran is great in expressing operations on multi-dimensional arrays
of numbers.  Scheme is great at expressing your coding thoughts.  This project
is an attempt to combine both into something useful.")
      (license license:asl2.0))))

(define-public guile-knots
  (let ((commit "e1858dfff5e360b36b167cd94c806759f9b4e7e8")
        (revision "16"))
    (package
    (name "guile-knots")
    (version (git-version "0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.cbaines.net/git/guile/knots")
                    (commit commit)))
              (sha256
               (base32
                "1342a7rw9zqxgii5q4ahiqabmgqbvczjdx1308yf9k3d8x6gk41q"))
              (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           autoconf
           automake
           guile-next
           guile-lib
           guile-fibers-next))
    (inputs
     (list guile-next))
    (propagated-inputs
     (list guile-fibers-next))
    (home-page "https://git.cbaines.net/guile/knots")
    (synopsis "Patterns and functionality to use with Guile Fibers")
    (description
     "Guile Knots is a collection of patterns and functionality that is useful
when using Guile Fibers.  This includes higher level concurrency utilities,
support for timeouts and an alternative web server implementation.")
    (license license:gpl3+))))

(define-public guile-kolam
  (package
    (name "guile-kolam")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://kolam.systemreboot.net/releases/kolam-"
                           version ".tar.lz"))
       (sha256
        (base32
         "083r3n3wvzysa9jhlwjj1xppdm6ja56rkizr6hvj4q1806v8n6mn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:make-flags '("GUILE_AUTO_COMPILE=0"))) ; to prevent guild warnings
    (native-inputs
     (list guile-3.0 lzip))
    (propagated-inputs
     (list guile-json-4))
    (home-page "https://kolam.systemreboot.net")
    (synopsis "GraphQL implementation for Scheme")
    (description "@code{guile-kolam} is a GraphQL implementation for Scheme.  kolam
features a parser to parse and serialize GraphQL documents, a type system to
create GraphQL schemas, an execution engine to execute GraphQL queries, and a
HTTP handler to implement a HTTP GraphQL endpoint.")
    (license license:agpl3+)))

(define-public guile-libnotify
  (package
    (name "guile-libnotify")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ekaitz-zarraga/guile-libnotify")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "055d3xjx819yr1mhph3lvciqn17hxmqrh3vp8cjz4905yr0bf7r2"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake pkg-config texinfo))
    (inputs (list guile-3.0 libnotify))
    (synopsis "Guile bindings for libnotify")
    (description "Provides bindings for GNOME's libnotify C library to Guile")
    (home-page "https://github.com/ekaitz-zarraga/guile-libnotify")
    (license license:gpl3+)))

(define-public lokke
  (let ((commit "92d36370dc6d218ff3bf315e56ebef93808c1b79")
        (revision "1"))
    (package
      (name "lokke")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/lokke-org/lokke")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1c913md4dcfb0x4n26wbx9wdw453wxg3c5rn49k3f6j8zjqv63yv"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'pre-bootstrap
             (lambda _
               (for-each patch-shebang
                         '("setup" "gen-makefile"
                           "dev/gen-module-paths"
                           "dev/refresh"))
               (invoke "./setup")))
           (add-before 'build 'set-home
             (lambda _
               (setenv "HOME" (getcwd)))))))
      (native-inputs
       (list autoconf
             automake
             libtool
             gnu-gettext
             pkg-config

             ;; Use Guile >= 3.0.8 to work around
             ;; <https://bugs.gnu.org/49305>.
             guile-3.0-latest))
      (inputs
       (list pcre2))
      (synopsis "Clojure implementation in Guile")
      (description
       "Lokke intends to provide a full dialect of Clojure for Guile.  It also
consists of a set of Guile modules providing some of Clojure's functionality
in two different guises.")
      ;; Dual license: LGPLv2.1+ or EPLv1.0+ at the user's option.
      (license (list license:lgpl2.1+ license:epl1.0)))))

(define-public guile-tap
  (package
    (name "guile-tap")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ft/guile-tap")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0yimi9ci5h6wh7bs3ir7p181pwbd2hxlhx7pqq53gr54mnad8qv4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (substitute* "Makefile"
                     (("PREFIX = /usr/local") (string-append "PREFIX="
                                                             #$output)))
                   (substitute* "bin/tap-harness"
                     ((" guile ") (string-append " " (which "guile") " ")))))
               (replace 'build
                 (lambda _
                   (invoke "make")
                   (invoke "make" "-C" "doc" "man")
                   (invoke "make" "install")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "make" "test")))))))
    (native-inputs (list guile-3.0 pandoc))
    (home-page "https://github.com/ft/guile-tap")
    (synopsis "Guile test framework that emits TAP output")
    (description
     "guile-tap is a library for GNU Guile that implements a framework for
specifying test cases that emit output that adheres to the Test Anything
Protocol (TAP).  It comes with an experimental harness (tap-harness).")
    (license license:bsd-2)))

(define-public guile-termios
  (package
    (name "guile-termios")
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ft/guile-termios")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "020p3c84z09wyyb6gfzj2x6q2rfmvas7c92fcm2hhg8z1q60sqkg"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (substitute* "Makefile"
                     (("CC = cc") (string-append "CC="
                                                 #$(cc-for-target)))
                     (("PREFIX = /usr/local") (string-append "PREFIX="
                                                             #$output)))))
               (replace 'build
                 (lambda _
                   (invoke "make")
                   (invoke "make" "-C" "doc" "man")
                   (invoke "make" "install")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "make" "test")))))))
    (native-inputs (list guile-3.0 guile-tap pandoc perl perl-io-tty))
    (home-page "https://github.com/ft/guile-termios")
    (synopsis "POSIX termios interface for GNU Guile")
    (description
     "To query and change settings of serial devices on POSIX systems, the
termios API is used.  GNU Guile doesn't have an interface for that built in.
This module implements this interface by use of Guile's dynamic FFI.")
    (license license:bsd-2)))

(define-public guile-goblins
  (package
    (name "guile-goblins")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://files.spritely.institute/releases"
                           "/guile-goblins/guile-goblins-"
                           version ".tar.gz"))
       (sha256
        (base32
         "05qqryhhs9rci01j08nbchmif1h9889bwqqv830ywygl1bld50ys"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list "GUILE_AUTO_COMPILE=0")))
    (native-inputs
     (list pkg-config texinfo))
    (inputs (list guile-3.0))
    (propagated-inputs
     (list guile-fibers guile-gnutls guile-websocket))
    (home-page "https://spritely.institute/goblins")
    (synopsis "Distributed programming environment for Guile")
    (description
     "@code{guile-goblins} is the Guile version of
@url{https://spritely.institute/goblins, Spritely Goblins}, a transactional,
distributed programming environment following object capability security
designs.  Goblins is a general toolkit, and also the core layer of Spritely's
work to support healthy distributed networked communities.  Goblins allows for
cooperation between networked programs in a mutually suspicious network
through OCapN, the Object Capability Network.  This includes collaboration
across runtimes; for instance, programs written in the Guile and Racket
versions of Goblins are able to speak to each other.")
    (license license:asl2.0)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
