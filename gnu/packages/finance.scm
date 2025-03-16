;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016-2018, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018 Adriano Peluso <catonano@gmail.com>
;;; Copyright © 2018-2022, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019-2024 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019 Sebastian Schott <sschott@mailbox.org>
;;; Copyright © 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2020 Tom Zander <tomz@freedommail.ch>
;;; Copyright © 2020, 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2022, 2024, 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Carlo Holl <carloholl@gmail.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 ZmnSCPxj jxPCSnmZ <ZmnSCPxj@protonmail.com>
;;; Copyright © 2021 François J <francois-oss@avalenn.eu>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Kyle Meyer <kyle@kyleam.com>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2023 dan <i@dan.games>
;;; Copyright © 2022 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2023 Frank Pursel <frank.pursel@gmail.com>
;;; Copyright © 2023 Skylar Hill <stellarskylark@posteo.net>
;;; Copyright © 2023 Foundation Devices, Inc. <hello@foundationdevices.com>
;;; Copyright © 2023 Attila Lendvai <attila@lendvai.name>
;;; Copyright © 2024 Saku Laesvuori <saku@laesvuori.fi>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages finance)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system qt)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (srfi srfi-26)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages emacs-xyz)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-web)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages gnuzilla))

(define-public bitcoin-core
  ;; The support lifetimes for bitcoin-core versions can be found in
  ;; <https://bitcoincore.org/en/lifecycle/#schedule>.
  (package
    (name "bitcoin-core")
    (version "28.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://bitcoincore.org/bin/bitcoin-core-"
                              version "/bitcoin-" version ".tar.gz"))
              (sha256
               (base32
                "1fl312ns86syc6871il9l3lzf96nm6jhnj92qyvxkyf78782vbn5"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           python ; for the tests
           util-linux ; provides the hexdump command for tests
           qttools-5))
    (inputs
     (list bdb-4.8 ; 4.8 required for compatibility
           boost
           libevent
           miniupnpc
           qtbase-5
           sqlite))
    (arguments
     `(#:configure-flags
       (list
        ;; Boost is not found unless specified manually.
        (string-append "--with-boost="
                       (assoc-ref %build-inputs "boost"))
        ;; XXX: The configure script looks up Qt paths by
        ;; `pkg-config --variable=host_bins Qt5Core`, which fails to pick
        ;; up executables residing in 'qttools-5', so we specify them here.
        (string-append "ac_cv_path_LRELEASE="
                       (assoc-ref %build-inputs "qttools")
                       "/bin/lrelease")
        (string-append "ac_cv_path_LUPDATE="
                       (assoc-ref %build-inputs "qttools")
                       "/bin/lupdate"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'make-qt-deterministic
           (lambda _
             ;; Make Qt deterministic.
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
         (add-before 'build 'set-no-git-flag
           (lambda _
             ;; Make it clear we are not building from within a git repository
             ;; (and thus no information regarding this build is available
             ;; from git).
             (setenv "BITCOIN_GENBUILD_NO_GIT" "1")
             #t))
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" (getenv "TMPDIR")) ; tests write to $HOME
             #t))
         (add-after 'check 'check-functional
           (lambda _
             (invoke
              "python3" "./test/functional/test_runner.py"
              (string-append "--jobs=" (number->string (parallel-job-count))))
             #t)))))
    (home-page "https://bitcoincore.org/")
    (synopsis "Bitcoin peer-to-peer client")
    (description
     "Bitcoin is a digital currency that enables instant payments to anyone
anywhere in the world.  It uses peer-to-peer technology to operate without
central authority: managing transactions and issuing money are carried out
collectively by the network.  Bitcoin Core is the reference implementation
of the bitcoin protocol.  This package provides the Bitcoin Core command
line client and a client based on Qt.")
    (license license:expat)))

(define-public ghc-hledger
  (package
    (name "ghc-hledger")
    (version "1.27.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hledger" version))
              (sha256
               (base32
                "0qdg87m7ys2ykqqq32p7h7aw827w4f5bcqx4dspxxq6zqlvzddqb"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hledger")))
    (inputs (list ghc-decimal
                  ghc-diff
                  ghc-aeson
                  ghc-ansi-terminal
                  ghc-breakpoint
                  ghc-cmdargs
                  ghc-data-default
                  ghc-extra
                  ghc-githash
                  ghc-hashable
                  ghc-hledger-lib
                  ghc-lucid
                  ghc-math-functions
                  ghc-megaparsec
                  ghc-microlens
                  ghc-regex-tdfa
                  ghc-safe
                  ghc-shakespeare
                  ghc-split
                  ghc-tabular
                  ghc-tasty
                  ghc-temporary
                  ghc-timeit
                  ghc-unordered-containers
                  ghc-utf8-string
                  ghc-utility-ht
                  ghc-wizards))
    (home-page "http://hledger.org")
    (synopsis "Command-line interface for the hledger accounting system")
    (description
     "The command-line interface for the hledger accounting system.  Its basic
function is to read a plain text file describing financial transactions and
produce useful reports.

hledger is a robust, cross-platform set of tools for tracking money, time, or
any other commodity, using double-entry accounting and a simple, editable file
format, with command-line, terminal and web interfaces.  It is a Haskell
rewrite of Ledger, and one of the leading implementations of Plain Text
Accounting.")
    (license license:gpl3)))

(define-public hledger
  (package
    (inherit ghc-hledger)
    (name "hledger")
    (arguments
     (list
      #:haddock? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-doc
            (lambda _
              (install-file "hledger.info" (string-append #$output "/share/info"))
              (install-file "hledger.1" (string-append #$output "/man/man1"))))
           (add-after 'register 'remove-libraries
             (lambda* (#:key outputs #:allow-other-keys)
               (delete-file-recursively (string-append (assoc-ref outputs "out") "/lib")))))))))

(define-public homebank
  (package
    (name "homebank")
    (version "5.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.gethomebank.org/public/sources"
                                  "/homebank-" version ".tar.gz"))
              (sha256
               (base32
                "15wy6fhw0604xhf7zjj7gnxa1b8ns3j3ysidii39a0w4awg4bd2f"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list gtk+ libofx libsoup-minimal))
    (home-page "https://gethomebank.org/en/index.php")
    (synopsis "Graphical personal accounting application")
    (description "HomeBank allows you to manage your personal accounts at
home.  The seeks to be lightweight, simple and easy to use.  It brings
features that allow you to analyze your finances in a detailed way instantly
and dynamically with report tools based on filtering and graphical charts.")
    (license license:gpl2+)))

(define-public ledger
  (package
    (name "ledger")
    (version "3.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ledger/ledger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vchc97952w3fkkdn3v0nzjlgzg83cblwsi647jp3k9jq6rvhaak"))))
    (build-system cmake-build-system)
    (arguments
     `(#:modules (,@%cmake-build-system-modules
                  ((guix build python-build-system) #:select (python-version)))
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:configure-flags
       `("-DBUILD_DOCS:BOOL=ON"
         "-DBUILD_WEB_DOCS:BOOL=ON"
         "-DUSE_PYTHON:BOOL=ON"
         "-DCMAKE_INSTALL_LIBDIR:PATH=lib")
       #:phases
       (modify-phases (@ (guix build cmake-build-system) %standard-phases)
         (add-after 'unpack 'fix-python-installation-directory
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; By default the package attempts to install its Python bindings
             ;; to the Python store directory, which obviously does not work.
             ;; Passing -DPython_SITEARCH in #:configure-flags has no effect.
             (let ((python-version (python-version (assoc-ref inputs "python")))
                   (out (assoc-ref outputs "out")))
               (substitute* "src/CMakeLists.txt"
                 (("DESTINATION \\$\\{Python_SITEARCH\\}")
                  (string-append "DESTINATION " out "/lib/python"
                                 python-version "/site-packages")))
               #t)))
         (add-before 'configure 'install-examples
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((examples (string-append (assoc-ref outputs "out")
                                            "/share/doc/ledger/examples")))
               (install-file "test/input/sample.dat" examples)
               (install-file "test/input/demo.ledger" examples)
               (install-file "contrib/report" examples))
             #t))
         (add-after 'build 'build-doc
           (lambda _ (invoke "make" "doc")))
         (add-before 'check 'check-setup
           ;; One test fails if it can't set the timezone.
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (search-input-directory inputs
                                             "share/zoneinfo"))
             #t)))))
    (inputs
     (list boost
           gmp
           libedit
           mpfr
           python
           utfcpp))
    (native-inputs
     (list groff texinfo tzdata-for-tests))
    (home-page "https://ledger-cli.org/")
    (synopsis "Command-line double-entry accounting program")
    (description
     "Ledger is a powerful, double-entry accounting system that is
accessed from the UNIX command-line.  This may put off some users, since
there is no flashy UI, but for those who want unparalleled reporting
access to their data there are few alternatives.

Ledger uses text files for input.  It reads the files and generates
reports; there is no other database or stored state.  To use Ledger,
you create a file of your account names and transactions, run from the
command line with some options to specify input and requested reports, and
get output.  The output is generally plain text, though you could generate
a graph or html instead.  Ledger is simple in concept, surprisingly rich
in ability, and easy to use.")
    ;; There are some extra licenses in files which do not presently get
    ;; installed when you build this package.  Different versions of the GPL
    ;; are used in the contrib and python subdirectories.  The bundled version
    ;; of utfcpp is under the Boost 1.0 license. Also the file
    ;; `tools/update_copyright_year` has an Expat license.
    (license (list license:bsd-3
                   license:asl2.0     ; src/strptime.cc
                   (license:non-copyleft
                    "file://src/wcwidth.cc"
                    "See src/wcwidth.cc in the distribution.")))))

(define-public emacs-ledger-mode
  ;; The last release was on Nov 8, 2019 and doesn't build with Emacs 28.
  (let ((commit "356d8049ede02c06db4f487d1d6076f74d6098c5")
        (revision "1"))
    (package
      (name "emacs-ledger-mode")
      (version (git-version "4.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ledger/ledger-mode")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1wssagczhils0nx12b2nq2jk2gp9j26jn8nrqdrj255nzl40aia1"))))
      (build-system emacs-build-system)
      (arguments
       (list
        #:tests? #t
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'do-not-require-tests-at-runtime
              (lambda _
                (substitute* "ledger-mode.el"
                  (("\\(require 'ledger-test\\)") ""))))
            (add-after 'unpack 'patch-path
              (lambda* (#:key inputs #:allow-other-keys)
                (emacs-substitute-variables "ledger-exec.el"
                  ("ledger-binary-path" (search-input-file inputs "/bin/ledger")))))
            (add-after 'build 'build-doc
              (lambda _
                (let ((target (string-append #$output "/share/info")))
                  (mkdir-p target)
                  (invoke "makeinfo" "-o" target
                          "../source/doc/ledger-mode.texi"))))
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (with-directory-excursion "../source/test"
                    ;; Test does not respect `ledger-binary-path' and thus fails
                    (delete-file-recursively "report-test.el")
                    (invoke "make" "test"))))))))
      (inputs
       (list ledger))
      (native-inputs
       (list texinfo))
      (home-page "https://ledger-cli.org/")
      (synopsis "Command-line double-entry accounting program")
      (description
       "Ledger is a powerful, double-entry accounting system that is
accessed from the UNIX command-line.  This may put off some users, since
there is no flashy UI, but for those who want unparalleled reporting
access to their data there are few alternatives.

Ledger uses text files for input.  It reads the files and generates
reports; there is no other database or stored state.  To use Ledger,
you create a file of your account names and transactions, run from the
command line with some options to specify input and requested reports, and
get output.  The output is generally plain text, though you could generate
a graph or html instead.  Ledger is simple in concept, surprisingly rich
in ability, and easy to use.

This package provides the Emacs mode.")
      (license license:gpl2+))))

(define-public emacs-hledger-mode
  (let ((commit "400bde42a8d2712af80cd7c773c9cdfbb63a515a")
        (revision "1"))
    (package
      (name "emacs-hledger-mode")
      (version (git-version "20220515" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/narendraj9/hledger-mode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0xmcfpr3rxli1adwypg18npl8hb8ak5rg6a6i26inzzqja6vr897"))))
      (build-system emacs-build-system)
      (propagated-inputs
       (list emacs-popup
             emacs-async
             emacs-htmlize))
      (arguments
       '(#:include '("^[^/]+.el$")
         #:exclude '()))
      (home-page "https://github.com/narendraj9/hledger-mode")
      (synopsis "Mode for writing journal entries for hledger")
      (description
       "This major mode for Emacs enables writing and managing hledger
journal files.  It generates some useful reports along with some financial
ratios that can help you keep a check on your financial health for users of
the plaintext accounting system hledger.")
      (license license:gpl3))))

(define-public geierlein
  (package
    (name "geierlein")
    (version "0.9.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stesie/geierlein")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00zpwr3lk2vdmd60fgdwdk0xxs52wvnm19ln2m75yfphydvkglic"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                  ; would require npm, python and a lot more
       #:phases
        (modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (add-after 'unpack 'override-target-directory-and-tool-paths
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "Makefile"
                (("prefix := .*")
                 (string-append "prefix := " (assoc-ref outputs "out") "\n"))
                ;; Required for tests, unused for now:
                ;;(("PYTHON := .*")
                ;; (string-append (which "python") "\n")))
                (("INSTALL := .*")
                 (string-append "INSTALL := " (which "install") "\n")))
              (substitute* "bin/xgeierlein.in"
                ;; Use icecat as XULRUNNER
                (("^for search ")
                 (string-append "XULRUNNER=" (which "icecat") "\n"
                                "for search ")))
              #t)))))
    (inputs
     (list icecat))
    (home-page "https://stesie.github.io/geierlein/")
    (synopsis "Free Elster client, for sending Germany VAT declarations")
    (description
     "Geierlein is a free Elster client, i.e. an application that
sends VAT declarations to Germany's fiscal authorities.

Currently it is *not* possible to send returns that are due annually
(especially the income tax return) since the fiscal authority doesn't
allow doing that off the ERiC library (which is proprietary however).
It's not clear at the moment whether one day it will be possible to
do so.")
    (license license:agpl3+)))

(define-public electrum
  (package
    (name "electrum")
    (version "4.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.electrum.org/"
                           version "/Electrum-"
                           version ".tar.gz"))
       (sha256
        (base32 "1f0hb8xmqv1j9pf82xpyvxnn2dzmi93rhf0sh0iqakja2pbl4707"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete the bundled dependencies.
           (delete-file-recursively "packages")))))
    (build-system python-build-system)
    (inputs
     (list libsecp256k1
           python-aiohttp
           python-aiohttp-socks
           python-aiorpcx
           python-attrs
           python-bitstring
           python-btchip-python
           python-certifi
           python-cryptography
           python-dnspython
           python-hidapi
           python-ledgerblue
           python-protobuf
           python-pyqt
           python-qdarkstyle
           python-qrcode
           zbar))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'use-libsecp256k1-input
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "electrum/ecc_fast.py"
               (("library_paths = \\[\\]")
                (string-append "library_paths = ['"
                               (assoc-ref inputs "libsecp256k1")
                               "/lib/libsecp256k1.so']"))))))))
    (home-page "https://electrum.org/")
    (synopsis "Bitcoin wallet")
    (description
     "Electrum is a lightweight Bitcoin client, based on a client-server
protocol.  It supports Simple Payment Verification (SPV) and deterministic key
generation from a seed.  Your secret keys are encrypted and are never sent to
other machines/servers.  Electrum does not download the Bitcoin blockchain.")
    (license license:expat)))

(define-public electron-cash
  (package
    (name "electron-cash")
    (version "4.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Electron-Cash/Electron-Cash")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11xhlssr7bvdv3p256k87y35vjzyfd93p72w8f2xy7j5jh6abhp1"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:modules '((guix build python-build-system)
                  (guix build qt-utils)
                  (guix build utils))
      #:imported-modules `(,@%python-build-system-modules
                           (guix build qt-utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'create-output-directories
            (lambda _
              ;; setup.py installs to ~/.local/share if this doesn't exist.
              (mkdir-p (string-append #$output "/share"))))
          (add-after 'unpack 'use-libsecp256k1-input
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "electroncash/secp256k1.py"
                (("libsecp256k1.so.0")
                 (search-input-file inputs "lib/libsecp256k1.so.0")))))
          (add-after 'install 'wrap-qt
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-qt-program "electron-cash"
                                 #:output out #:inputs inputs)))))))
    (inputs
     (list bash-minimal
           libevent
           libsecp256k1-bitcoin-cash
           openssl
           python-cython
           python-dateutil
           python-dnspython
           python-ecdsa
           python-hidapi
           python-jsonrpclib-pelix
           python-keepkey
           python-pathvalidate
           python-protobuf
           python-pyaes
           python-pyqt
           python-pysocks
           python-qdarkstyle
           python-qrcode
           python-requests
           python-stem
           python-trezor
           qtsvg-5
           zlib))
    (home-page "https://electroncash.org/")
    (synopsis "Bitcoin Cash wallet")
    (description
     "Electroncash is a lightweight Bitcoin Cash client, based on a client-server
protocol.  It supports Simple Payment Verification (SPV) and deterministic key
generation from a seed.  Your secret keys are encrypted and are never sent to
other machines/servers.  Electroncash does not download the Bitcoin Cash
blockchain.")
    (license license:expat)))

(define-public monero
  ;; This package bundles easylogging++ and lmdb.
  ;; The bundled easylogging++ is modified, and the changes will not be
  ;; upstreamed.
  ;; The devs deem the lmdb driver too critical a consenus component, to use
  ;; the system's dynamically linked library.
  (package
    (name "monero")
    (version "0.18.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monero-project/monero")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (patches (search-patches "monero-use-system-miniupnpc.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled dependencies.
           (for-each
            delete-file-recursively
            '("external/miniupnp" "external/rapidjson"))))
       (sha256
        (base32 "0rjyxcggg7pdp5026kbb49mk7vnvldvbr7qlkn76n4sq20cpk3v9"))))
    (build-system cmake-build-system)
    (native-inputs
     (list doxygen
           graphviz
           pkg-config
           protobuf
           python
           qttools-5))
    (inputs
     (list boost
           cppzmq
           expat
           hidapi
           libsodium
           libunwind
           libusb
           miniupnpc
           openssl
           protobuf
           rapidjson
           readline
           unbound
           xz
           zeromq))
    (arguments
     (list #:out-of-source? #t
           #:configure-flags
           #~(list "-DARCH=default"
                   "-DBUILD_TESTS=ON"
                   (string-append "-DReadline_ROOT_DIR="
                                  #$(this-package-input "readline")))
           #:phases
           #~(modify-phases %standard-phases
               ;; tests/core_tests need a valid HOME
               (add-before 'configure 'set-home
                 (lambda _
                   (setenv "HOME" (getcwd))))
               (add-after 'set-home 'change-log-path
                 (lambda _
                   (substitute* "contrib/epee/src/mlog.cpp"
                     (("epee::string_tools::get_current_module_folder\\(\\)")
                      "\".bitmonero\"")
                     (("return \\(")
                      "return ((std::string(getenv(\"HOME\"))) / "))))
               (add-after 'change-log-path 'fix-file-permissions-for-tests
                 (lambda _
                   (for-each make-file-writable
                             (find-files "tests/data/" "wallet_9svHk1.*"))))
               (replace 'check
                 ;; Only try tests that don't need access to network or system
                 (lambda* (#:key tests? #:allow-other-keys)
                   ;; Core tests sometimes fail, at least on i686-linux.
                   ;; Let's disable them for now and just try hash tests
                   ;; and unit tests.
                   ;; (invoke "make" "ARGS=-R 'hash|core_tests' --verbose" "test")))
                   (when tests?
                     (invoke "make" "ARGS=-R 'hash' --verbose" "test")
                     (let ((excluded-unit-tests
                            (string-join
                             '("AddressFromURL.Success"
                               "AddressFromURL.Failure"
                               "DNSResolver.IPv4Success"
                               "DNSResolver.DNSSECSuccess"
                               "DNSResolver.DNSSECFailure"
                               "DNSResolver.GetTXTRecord"
                               "is_hdd.linux_os_root")
                             ":")))
                       (invoke "tests/unit_tests/unit_tests"
                               (string-append "--gtest_filter=-"
                                              excluded-unit-tests))))))
               (add-after 'install 'delete-unused-files
                 (lambda* (#:key outputs #:allow-other-keys)
                   (delete-file-recursively
                    (string-append #$output "/include")))))))
    (home-page "https://web.getmonero.org/")
    (synopsis "Command-line interface to the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides
the Monero command line client and daemon.")
    (license license:bsd-3)))

(define-public monero-gui
  (package
    (name "monero-gui")
    (version "0.18.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/monero-project/monero-gui")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete bundled monero sources, we already have them.
           ;; See the 'extract-monero-sources' phase.
           (delete-file-recursively "monero")))
       (sha256
        (base32 "09x8184kbpw74qlak2x9amz7j3qkacnk9l0c1cws1d0fghlm6n9m"))))
    (build-system qt-build-system)
    (native-inputs
     `(,@(package-native-inputs monero)
       ("monero-source" ,(package-source monero))))
    (inputs
     (modify-inputs (package-inputs monero)
       (append libgcrypt
               monero
               p2pool
               qtbase-5
               qtdeclarative-5
               qtgraphicaleffects
               qtquickcontrols-5
               qtquickcontrols2-5
               qtsvg-5
               qtwayland-5
               qtxmlpatterns)))
    (arguments
     (list #:tests? #f ; No tests
           #:configure-flags
           #~(list "-DARCH=default"
                   "-DENABLE_PASS_STRENGTH_METER=ON"
                   (string-append "-DReadline_ROOT_DIR="
                                  #$(this-package-input "readline")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'extract-monero-sources
                 ;; Some of the monero package source code is required
                 ;; to build the GUI.
                 (lambda* (#:key inputs #:allow-other-keys)
                   (mkdir-p "monero")
                   (copy-recursively (assoc-ref inputs "monero-source")
                                     "monero")))
               (add-after 'extract-monero-sources 'fix-build
                 (lambda _
                   (substitute* "src/version.js.in"
                     (("@VERSION_TAG_GUI@")
                      #$version))
                   (substitute* "external/CMakeLists.txt"
                     (("add_library\\(quirc" all)
                      (string-append
                       "set(CMAKE_C_FLAGS \"${CMAKE_C_FLAGS} -fPIC\")\n"
                       all)))))
               (add-after 'unpack 'fix-p2pool-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/p2pool/P2PoolManager.cpp"
                     ;; Location for files created by P2Pool
                     (("m_p2poolPath = QApplication::applicationDirPath\\(\\);")
                      "m_p2poolPath = QStandardPaths::writableLocation(QStandardPaths::CacheLocation);")
                     ;; Location of p2pool program
                     (("m_p2pool = m_p2poolPath \\+ \"/p2pool\";")
                      (string-append "m_p2pool = \""
                                     (search-input-file inputs "/bin/p2pool")
                                     "\";")))))
               (replace 'install
                 (lambda _
                   ;; Binary
                   (let ((dir (string-append #$output "/bin")))
                     (mkdir-p dir)
                     (install-file "../build/bin/monero-wallet-gui" dir))
                   ;; Icons
                   (for-each
                    (lambda (size)
                      (let ((dir (string-append #$output
                                                "/share/icons/hicolor/"
                                                size
                                                "/apps")))
                        (mkdir-p dir)
                        (copy-file (string-append "../source/images/appicons/"
                                                  size ".png")
                                   (string-append dir
                                                  "/monero-gui.png"))))
                    '("16x16" "24x24" "32x32" "48x48"
                      "64x64" "96x96" "128x128" "256x256"))
                   ;; Menu entry file
                   (let ((dir (string-append #$output "/share/applications")))
                     (mkdir-p dir)
                     (call-with-output-file
                         (string-append dir "/monero-gui.desktop")
                       (lambda (port)
                         (format port
                                 "[Desktop Entry]~@
                                  Type=Application~@
                                  Name=Monero wallet~@
                                  Exec=~a/bin/monero-wallet-gui~@
                                  Icon=monero-gui~@
                                  Categories=Office;Finance;~%"
                                 #$output))))))
               (add-after 'qt-wrap 'install-monerod-link
                 ;; The monerod program must be available so that
                 ;; monero-wallet-gui can start a Monero daemon if necessary.
                 (lambda* (#:key inputs #:allow-other-keys)
                   (symlink (search-input-file inputs "/bin/monerod")
                            (string-append #$output "/bin/monerod")))))))
    (home-page "https://web.getmonero.org/")
    (synopsis "Graphical user interface for the Monero currency")
    (description
     "Monero is a secure, private, untraceable currency.  This package provides
the Monero GUI client.")
    (license license:bsd-3)))

(define-public python-bech32
  (package
    (name "python-bech32")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bech32" version))
              (sha256
               (base32
                "16fq5cfy5id9hp123ylhpl55pf38xwk0hv7sziqpig838qhvhvbx"))))
    (build-system python-build-system)
    (home-page "https://github.com/fiatjaf/bech32")
    (synopsis "Reference implementation for Bech32 and Segwit addresses")
    (description "This package provides a python reference implementation for
Bech32 and segwit addresses.")
    (license license:expat)))

(define-public python-trezor-agent
  ;; It is called 'libagent' in pypi; i.e. this is the library as opposed to
  ;; the toplevel app called trezor-agent.
  (package
    (name "python-trezor-agent")
    (version "0.14.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/romanz/trezor-agent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04dds5bbw73nk36zm8d02qw6qr92nrlcf8r1cq8ba96mzi34jbk0"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-requires-backports-shutil-which
           ;; Remove requires on backport of shutil_which, as python 3.4+ has
           ;; a built-in implementation supported in python-trezor-agent.
           (lambda _
             (substitute* "setup.py"
               (("'backports.shutil_which>=3.5.1',") ""))))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-v")))))))
    (propagated-inputs
     (list python-bech32
           python-configargparse
           python-cryptography
           python-daemon
           python-docutils
           python-ecdsa
           python-hidapi
           python-mnemonic
           python-pymsgbox
           python-pynacl
           python-semver
           python-unidecode
           python-wheel))
    (native-inputs ; Only needed for running the tests
     (list gnupg
           python-mock
           python-pytest))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "Use hardware wallets as SSH and GPG agent")
    (description
     "@code{libagent} is a library that allows using TREZOR, Keepkey and
Ledger Nano as a hardware SSH/GPG agent.")
    (license license:lgpl3)))

(define-public trezor-gpg-pinentry-tk
  (package
    (name "trezor-gpg-pinentry-tk")
    (version "0.0.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rendaw/trezor-gpg-pinentry-tk/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mblx4favmw4nf7k9rfl00ivv77kgdiwghyz4xv5cp0v410kjaqc"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))        ; No test suite.
    (inputs
     `(("python-tkinter" ,python "tk")))
    (home-page "https://github.com/rendaw/trezor-gpg-pinentry-tk")
    (synopsis "GPG pinentry program for use with @code{trezor-agent}")
    (description
     "This package provides a GPG pinentry program for use with
@code{trezor-agent}, or for people with number-only PINs.  It displays
a grid of unlabeled buttons and supports configurable keyboard
settings.")
    (license license:bsd-2)))

(define-public python-mnemonic
  (package
    (name "python-mnemonic")
    (version "0.20")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "mnemonic" version))
        (sha256
          (base32 "1xi5qvj2rvi5almf9c89rl7hz1z4ms04d53pg818i4vpkmivavvw"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pbkdf2))
    (home-page "https://github.com/trezor/python-mnemonic")
    (synopsis "Implementation of Bitcoin BIP-0039")
    (description "@code{mnemonic} is a library that provides an implementation
of Bitcoin BIP-0039.")
    (license license:expat)))

(define-public python-u2flib-host
  ;; The package is obsolete and superseded by python-fido2, but
  ;; needed for python-ledgerblue@0.1.44.
  (package
    (name "python-u2flib-host")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-u2flib-host" version))
              (sha256
               (base32
                "02pwafd5kyjpc310ys0pgnd0adff1laz18naxxwsfrllqafqnrxb"))))
    (build-system python-build-system)
    (propagated-inputs (list python-hidapi python-requests))
    (native-inputs (list python-cryptography))
    (home-page "https://github.com/Yubico/python-u2flib-host")
    (synopsis "Python based U2F host library")
    (description
     "The package provides library functionality for communicating with a U2F device over USB.")
    (license license:bsd-2)))

(define-public python-ledgerblue
  (package
    (name "python-ledgerblue")
    (version "0.1.54")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "ledgerblue" version))
        (sha256
          (base32
            "0ghpvxgih1zarp788qi1xh5xmprv6yhaxglfbix4974i7r4pszqy"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f)) ; no tests
    (native-inputs
     (list python-setuptools python-wheel))
    (propagated-inputs
     (list python-bleak
           python-pyelftools
           python-pycryptodome
           python-ecpy
           python-future
           python-gnupg
           python-hidapi
           python-nfcpy
           python-pillow
           python-protobuf
           python-pycryptodomex
           python-pyscard
           python-u2flib-host
           python-websocket-client))
    (home-page "https://github.com/LedgerHQ/blue-loader-python")
    (synopsis "Python library to communicate with Ledger Blue/Nano S")
    (description "@code{ledgerblue} is a Python library to communicate with
Ledger Blue/Nano S.")
    (license license:asl2.0)))

(define-public python-btchip-python
  (package
    (name "python-btchip-python")
    (version "0.1.32")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "btchip-python" version))
        (sha256
          (base32
            "0mcg3gfd0qk8lhral3vy9cfd4pii9kzs42q71pf6b3y0c70y1x9l"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; those require PyQt4
    (propagated-inputs
      (list python-ecdsa python-hidapi))
    (home-page "https://github.com/LedgerHQ/btchip-python")
    (synopsis "Python library to communicate with Ledger Nano dongle")
    (description
      "This package provides a Python library to communicate with Ledger
Nano dongle.")
    (license license:asl2.0)))

(define-public python-trezor
  (package
    (name "python-trezor")
    (version "0.13.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trezor/trezor-firmware/")
             (commit (string-append "python/v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13wyl9b15c8iscfakprwfvh2akw180hfqdjww79b78ywz51y7hdh"))
       (modules
        '((guix build utils)
          (srfi srfi-26)
          (srfi srfi-1)
          (ice-9 ftw)))
       (snippet
        '(begin
           ;; Delete everything except ./python/
           (for-each delete-file-recursively
                     (scandir "./" (negate (cut member <> '("python" "." "..")
                                                string=))))
           ;; Move ./python/* to the toplevel.
           (for-each (lambda (file-name)
                       (rename-file (string-append "./python/" file-name)
                                    (string-append "./" file-name)))
                     (scandir "./python/"
                              (negate (cut member <> '("." "..") string=))))
           (delete-file-recursively "./python")
           ;; Delete now broken symbolic links.
           (for-each delete-file
                     (append (find-files "." "^CHANGELOG.unreleased$")
                             (find-files "." "^.towncrier.template.md$")))))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-attrs
           python-click
           python-construct-classes
           python-ecdsa
           python-hidapi
           python-libusb1
           python-mnemonic
           python-requests
           python-typing-extensions))
    (native-inputs ; Only needed for running the tests
     (list protobuf
           python-black
           python-isort
           python-pillow
           python-protobuf
           python-pyqt
           python-pytest
           python-simple-rlp
           python-wheel))
    (home-page "https://github.com/trezor/python-trezor")
    (synopsis "Python library for communicating with TREZOR Hardware Wallet")
    (description "@code{trezor} is a Python library for communicating with
TREZOR Hardware Wallet.")
    (license license:lgpl3)))

(define-public python-keepkey
  (package
    (name "python-keepkey")
    (version "6.0.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keepkey" version))
        (sha256
          (base32
            "0z3d0m6364v9dv0njs4cd5m5ai6j6v35xaaxfxl90m9vmyxy81vd"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (apply invoke "python" (find-files "tests/unit" "\\.py$")))))))
    (propagated-inputs
     (list python-ecdsa python-hidapi python-libusb1 python-mnemonic
           python-protobuf))
    (home-page "https://github.com/keepkey/python-keepkey")
    (synopsis "Python library for communicating with KeepKey Hardware Wallet")
    (description "@code{keepkey} is a Python library for communicating with
the KeepKey Hardware Wallet.")
    (license license:lgpl3)))

(define-public ledger-agent
  (package
    (name "ledger-agent")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ledger_agent" version))
       (sha256
        (base32
         "03zj602m2rln9yvr08dswy56vzkbldp8b074ixwzz525dafblr92"))))
    (build-system python-build-system)
    (inputs
     (list python-ledgerblue python-trezor-agent))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "Ledger as hardware SSH/GPG agent")
    (description "This package allows using Ledger as hardware SSH/GPG agent.")
    (license license:lgpl3)))

(define-public trezor-agent
  (package
    (name "trezor-agent")
    ;; There are multiple Python apps/packages in the same git repo.  The git
    ;; tag seems to track libagent's version (which is called
    ;; python-trezor-agent in the Guix namespace). Currently trezor-agent's
    ;; version is set in `agents/trezor/setup.py` to a different value than
    ;; libagent, but as discussed with upstream in issue
    ;; https://github.com/romanz/trezor-agent/issues/369, we are copying our
    ;; version from that of libagent.
    (version (package-version python-trezor-agent))
    (source
     (origin
       (method git-fetch)
       (uri (origin-uri (package-source python-trezor-agent)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "04dds5bbw73nk36zm8d02qw6qr92nrlcf8r1cq8ba96mzi34jbk0"))
       (modules
        '((guix build utils)
          (ice-9 ftw)
          (srfi srfi-1)
          (srfi srfi-26)))
       (snippet
        '(begin
           ;; Delete everything except ./agents/trezor/
           (for-each delete-file-recursively
                     (filter (lambda (full-name)
                               (not (string-prefix? "./agents/trezor/" full-name)))
                             (find-files ".")))
           ;; Move ./agents/trezor/* to the toplevel
           (for-each (lambda (file-name)
                       (rename-file (string-append "./agents/trezor/" file-name)
                                    (string-append "./" file-name)))
                     (scandir "./agents/trezor/"
                              (negate (cut member <> '("." "..") string=))))
           (delete-file-recursively "./agents")
           ;; Without deleting ./contrib the sanity-check phase fails. Reported
           ;; upstream as https://github.com/romanz/trezor-agent/issues/429.
           (delete-file-recursively "./contrib")
           ;; Without deleting ./libagent setuptools complains as follows:
           ;; "error: Multiple top-level packages discovered in a flat-layout: ['contrib', 'libagent']."
           (delete-file-recursively "./libagent")))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("'trezor\\[hidapi]>=0.12.0,<0.13'")
                "'trezor[hidapi]>=0.13'"))))
         (add-after 'wrap 'fixup-agent-py
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               ;; The wrap phase also wraps trezor_agent.py (besides the
               ;; public facing executable called trezor-agent). We need to
               ;; undo that wrapping. The reason this is needed is that the
               ;; python easy install generates a toplevel script (?) that
               ;; messes with argv[0] and then re-opens the python
               ;; module. This fails when the wrapped file is actually a shell
               ;; script, not a python file.
               (delete-file (string-append out "/bin/.trezor_agent.py-real"))
               ;; Overwrite the wrapped one with the real thing.
               (install-file "./trezor_agent.py"
                             (string-append out "/bin"))))))))
    (build-system python-build-system)
    (inputs
     (list python-trezor python-trezor-agent))
    (native-inputs ; Only needed for running the tests
     (list python-attrs
           python-bech32
           python-simple-rlp))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "Using Trezor as hardware SSH/GPG agent")
    (description "This package allows using Trezor as a hardware SSH/GPG
agent.")
    (license license:lgpl3)))

(define-public keepkey-agent
  (package
    (name "keepkey-agent")
    (version "0.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "keepkey_agent" version))
        (sha256
          (base32
            "03779gvlx70i0nnry98i4pl1d92604ix5x6jgdfkrdgzqbh5vj27"))))
    (build-system python-build-system)
    (inputs
     (list python-keepkey python-trezor-agent))
    (home-page "https://github.com/romanz/trezor-agent")
    (synopsis "KeepKey as hardware SSH/GPG agent")
    (description "This package allows using KeepKey as a hardware SSH/GPG
agent.")
    (license license:lgpl3)))

(define-public kitsas
  (package
    (name "kitsas")
    (version "5.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/artoh/kitupiikki")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1fapl6y18jq279yyhvzdbbiks863w6q15b883w35py1xak1ladnm"))))
    (build-system qt-build-system)
    (inputs (list libzip qtsvg qtwebengine qt5compat))
    (arguments
     (list #:tests? #f           ; tests do not even build with Qt6 anymore
           #:test-target "check"
           #:qtbase qtbase       ; use Qt6
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda* _
                   (invoke "qmake" "kitsasproject.pro" "CONFIG+=release")))
               ;; The tests are not maintained and some don't even build
               (add-before 'configure 'disable-broken-tests
                 (lambda _
                   (substitute* "kitsasproject.pro"
                     ((" *(unittest|testit).*") "")
                     (("\\\\") ""))))
               (replace 'install
                 (lambda* _
                   (install-file "kitsas/kitsas"
                                 (string-append #$output "/bin/"))
                   (install-file "kitsas.png"
                                 (string-append #$output "/share/icons/"))
                   (install-file "kitsas.desktop"
                                 (string-append #$output "/share/applications/")))))))
    (home-page "https://kitsas.fi")
    (synopsis "Finnish bookkeeping software for small organisations")
    (description
     "Kitsas is a Finnish accounting program with the following goals and
features:

@itemize @bullet
@item Ease of use
@item Digital management of documents
@item Creating a digital archive
@item Built-in invoicing
@item Creating reports.
@end itemize")
    ;; GPLv3+ with additional terms:
    ;; - Modified versions of this software should be clearly mentioned as modified
    ;; - Kitsas Oy will not support any modified version of this software
    ;; - The name Kitsas Oy should not be used in any modified version
    (license license:gpl3+)))

(define-public python-stdnum
  (package
    (name "python-stdnum")
    (version "1.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-stdnum" version))
       (sha256
        (base32 "1h5y4qx75b6i2051ch8k0pcwkvhxzpaqd9mpsajkvqlsqkcn7ixw"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda _
                      (invoke "nosetests"))))))
    (native-inputs
     (list python-nose))
    (home-page "https://arthurdejong.org/python-stdnum/")
    (synopsis "Python module to handle standardized number and code formats")
    (description
     "This is a Python library that aims to provide functions to handle,
parse and validate standard numbers.
The module supports more than 100 different number formats
amongst which a great number of VAT and other tax numbers,
personal identity and company identification codes,
international standard numbers (ISBN, IBAN, EAN, etc.)
and various other formats.
The module also includes implementations of the Verhoeff,
Luhn and family of ISO/IEC 7064 check digit algorithms.")
    (license license:lgpl2.1+)))

(define-public python-duniterpy
  (package
    (name "python-duniterpy")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "duniterpy" version))
       (sha256
        (base32 "0h0fsawsrjd50lb1bkysb21ph39qlhmiymd3r5vs695qxvbwaqaa"))))
    (build-system pyproject-build-system)
    (arguments
     ;; FIXME: Tests fail with: "TypeError: block_uid() missing 1 required
     ;; positional argument: 'value'".
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'loosen-requirements
                    (lambda _
                      (substitute* "pyproject.toml"
                        (("mnemonic = \"\\^0\\.19")
                         "mnemonic = \">=0.19")
                        (("jsonschema = \"\\^3\\.2")
                         "jsonschema = \">=3.2"))))
                  (add-after 'unpack 'adjust-for-new-libnacl
                    (lambda _
                      ;; Mimic upstream commit ad8f6a26e9e7067; remove
                      ;; for newer versions of duniterpy.
                      (substitute* "pyproject.toml"
                        (("libnacl = \"1\\.8")
                         "libnacl = \">=1.9"))
                      (substitute* "duniterpy/key/ascii_armor.py"
                        (("from libnacl\\.version import version as libnacl_version")
                         "import importlib.metadata
libnacl_version = importlib.metadata.version('libnacl')")))))))
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-attrs
           python-base58
           python-graphql-core
           python-jsonschema
           python-libnacl
           python-mnemonic
           python-pyaes
           python-pypeg2
           python-websocket-client))
    (home-page "https://git.duniter.org/clients/python/duniterpy")
    (synopsis "Python implementation of Duniter API")
    (description "@code{duniterpy} is an implementation of
@uref{https://github.com/duniter/duniter/, duniter} API.  Its
main features are:
@itemize
@item Support Duniter's Basic Merkle API and protocol
@item Asynchronous/synchronous without threads
@item Support HTTP, HTTPS and Web Socket transport for Basic Merkle API
@item Support Elasticsearch Duniter4j API
@item Duniter signing key
@item Sign/verify and encrypt/decrypt messages with the Duniter credentials
@end itemize")
    (license license:gpl3+)))

(define-public silkaj
  (package
    (name "silkaj")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "silkaj" version))
       (sha256
        (base32 "0p8jqnswrrxri8i2ikdz8mij7gks0yab3wdcb37jf2kjwmrwanpk"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core))
    (propagated-inputs
     (list python-click
           python-duniterpy
           python-pendulum
           python-tabulate
           python-texttable))
    (home-page "https://git.duniter.org/clients/python/silkaj")
    (synopsis "Command line client for Duniter network")
    (description "Silkaj is a command line client for the
@uref{https://github.com/duniter/duniter/, Duniter} network.

Its features are:
@itemize
@item information about currency,
@item issuers difficulty to generate next block,
@item network view of nodes,
@item list of last issuers,
@item send transactions,
@item get account amount.
@end itemize")
    (license license:agpl3+)))

(define-public grisbi
  (package
    (name "grisbi")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/grisbi/grisbi%20stable/"
             (version-major+minor version) ".x/" version
             "/grisbi-" version ".tar.bz2"))
       (sha256
        (base32
         "0gvsqw1z5wkakyi3bkq71pqb094a8lv2nbgnxw2zqkabzjmxnfmx"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--without-ofx")))
    (propagated-inputs
     (list dconf))
    (native-inputs
     (list `(,glib "bin") ; glib-compile-schemas
           pkg-config intltool))
    (inputs
     (list gtk+ libgsf))
    (synopsis "Personal accounting application")
    (description "Grisbi is a personal accounting application written by
French developers that is designed to follow French accounting rules.
Grisbi can manage multiple accounts, currencies and users.  It manages
third party, expenditure and receipt categories, budgetary lines,
financial years, budget estimates, bankcard management and other
information.")
    (home-page "https://grisbi.org")
    (license license:gpl2+)))

(define-public gbonds
  ;; The last "upstream" commit is from about 2008, but the Debian maintainers
  ;; have effectively become the upstream with an extensive series of patches.
  ;; However, the patches are stored "unapplied", and some enhancements (like
  ;; a decade's worth of new data files) rely on the Debian packaging tools,
  ;; so building normally even from the patched sources would miss them.
  ;; Here, we do all of the patching in the origin, so that the result of
  ;; `guix build --source` is actually usable for building without Guix.
  (let ((revision "1")
        (commit "3054ee2f90cc7c03ed6b131177d09701c7a4fced"))
    (package
      (name "gbonds")
      (version (git-version "2.0.3" revision commit))
      (source
       (let ((unapplied
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://salsa.debian.org/debian/gbonds.git")
                      (commit commit)))
                (sha256
                 (base32
                  "1sqzzfymzxbnq6cjs5wvjbnvcrkdlimlmj2h7mlcaa9qqdpsgfki"))
                (file-name (git-file-name name version)))))
         (origin
           (inherit unapplied)
           (patches
            ;; The order matters.
            (map (cut file-append unapplied "/debian/patches/" <>)
                 '("desktop-file"
                   "POTFILES"
                   "POTFILES.missing"
                   "commands-compile"
                   "egg-recent-model-compile"
                   "gbonds-name-case"
                   "copyright-update"
                   "website-url"
                   "link-libm"
                   "xmldocs"
                   "configure-compiler-warnings"
                   "omf"
                   "desktop-file-keywords"
                   "replace-g_strcasecmp"
                   "gtk3-port"
                   "gsettings-port"
                   "no-rarian-compat"
                   "extern-gb_prefs"
                   "use-treasury-api.patch")))
           (snippet
            #~(begin
                (use-modules (guix build utils)
                             (srfi srfi-26))

                ;; Remove generated files, which have not been patched.
                (for-each (lambda (pth)
                            (when (file-exists? pth)
                              (delete-file pth)))
                          `(;; Things `make maintainer-clean` would do.
                            "gbonds.spec"
                            "src/marshal.c"
                            "src/marshal.h"
                            ;; Things upstream's distclean missed.
                            "intltool-extract"
                            "intltool-merge"
                            "intltool-update"
                            ;; Autotools generated files.
                            "aclocal.m4"
                            "config.guess"
                            "config.h.in"
                            "config.log"
                            "config.sub"
                            "configure"
                            "depcomp"
                            "intltool-extract.in"
                            "intltool-merge.in"
                            "intltool-update.in"
                            "ltmain.sh"
                            ,@(find-files "." "^Makefile\\.in$")))

                ;; Arrange for `make install` to handle the additional
                ;; redemption data files added in the Debian packaging.
                (let* ((new-redemption-data-files
                        (find-files "debian" "^sb[[:digit:]]+\\.asc$"))
                       (names
                        (map (cut substring <> (string-length "debian/"))
                             new-redemption-data-files)))
                  (for-each rename-file
                            new-redemption-data-files
                            (map (cut string-append "data/" <>)
                                 names))
                  (substitute* "data/Makefile.am"
                    (("redemption_DATA = \\\\")
                     (apply string-append
                            "redemption_DATA = \\"
                            (map (cut string-append "\n\t" <> " \\")
                                 names))))))))))
      (outputs '("out" "debug"))
      (inputs (list gtk+
                    glib
                    json-glib
                    libxml2
                    libsoup-minimal-2
                    cairo
                    pango))
      (native-inputs (list autoconf
                           automake
                           intltool
                           libtool
                           patch
                           pkg-config))
      (build-system glib-or-gtk-build-system)
      (home-page "https://gbonds.sourceforge.net")
      (synopsis "@acronym{U.S.} Savings Bond inventory program for GNOME")
      (description
       "GBonds is a @acronym{U.S.} Savings Bond inventory program for the
GNOME desktop environment.  It allows you to track the current redemption
value and performance of your @acronym{U.S.} Savings Bonds and keep a valuable
record of the bonds you own.")
      (license license:gpl2+))))

(define-public trezord-udev-rules
  (let ((commit "bff7fdfe436c727982cc553bdfb29a9021b423b0")
        (revision "0"))
      (package
        (name "trezord-udev-rules")
        (version (git-version "0.0.0" revision commit))
        (source
         (origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/trezor/trezor-common")
                 (commit commit)))
           (sha256
            (base32
             "14mrirrn68if7ja6qdk9qlxs1hv0f21vrxy5ncnms0gx9iwakp2l"))
           (file-name (git-file-name name version))))
        (build-system copy-build-system)
        (arguments
         '(#:install-plan
           '(("./udev/51-trezor.rules" "lib/udev/rules.d/"))))
        (home-page "https://github.com/trezor/trezor-common")
        (synopsis "Udev rules for trezord")
        (description
         "This contains the udev rules for trezord.  This will let a user run
trezord as a regular user instead of needing to it run as root.")
        (license license:lgpl3+))))

(define-public trezord
  (package
    (name "trezord")
    (version "2.0.33")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trezor/trezord-go")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0nnfh9qkb8ljajkxwrn3nn85zrsw10hp7c5i4zh60qgfyl0djppw"))
       (file-name (git-file-name name version))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.18
      #:install-source? #f
      #:import-path "github.com/trezor/trezord-go"))
    (native-inputs
     (list go-github-com-gorilla-csrf
           go-github-com-gorilla-handlers
           go-github-com-gorilla-mux
           go-gopkg-in-natefinch-lumberjack-v2))
    (home-page "https://trezor.io")
    (synopsis "Trezor Communication Daemon aka Trezor Bridge (written in Go)")
    (description
     "This allows a Trezor hardware wallet to communicate to the Trezor
wallet.")
    (license license:lgpl3+)))

(define-public libofx
  (package
    (name "libofx")
    (version "0.10.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libofx/libofx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "120hyhs4fkxrgpvd2p0hpf5v8dq0jjql2fzllk77m33m1c82pr18"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-build? #f              ;fails with -j64
      #:configure-flags
      #~(list "--disable-static"
              (string-append "--with-opensp-includes="
                             (search-input-directory %build-inputs
                                                     "include/OpenSP")))))
    (native-inputs
     (list autoconf
           automake
           gengetopt
           help2man
           libtool
           pkg-config))
    (inputs
     (list curl
           libxml++-2
           opensp))
    (home-page "http://libofx.sourceforge.net/")
    (synopsis "Library supporting the Open Financial Exchange format")
    (description
     "The LibOFX library is an API designed to allow applications to very easily
support OFX command responses, usually provided by financial institutions.  The
following three utilities are included with the library:
@enumerate
@item @code{ofxdump}
@item @code{ofx2qif}
@item @code{ofxconnect}
@end enumerate")
    (license license:gpl2+)))

(define-public bitcoin-unlimited
  (package
    (name "bitcoin-unlimited")
    (version "2.1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/bitcoinunlimited/BCHUnlimited.git/")
             (commit (string-append "BCHunlimited" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cny12s03wsgx8iijg5cbr7r6wif9ck7dn98hsv9sz8xq1i5vjk4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           python ; for the tests
           util-linux ; provides the hexdump command for tests
           qttools-5))
    (inputs
     (list bdb-4.8
           boost
           libevent
           miniupnpc
           openssl
           protobuf
           qrencode
           qtbase-5
           zeromq
           zlib))
    (arguments
     `(#:configure-flags
       (list
        ;; Boost is not found unless specified manually.
        (string-append "--with-boost="
                       (assoc-ref %build-inputs "boost"))
        ;; XXX: The configure script looks up Qt paths by
        ;; `pkg-config --variable=host_bins Qt5Core`, which fails to pick
        ;; up executables residing in 'qttools-5', so we specify them here.
        (string-append "ac_cv_path_LRELEASE="
                       (assoc-ref %build-inputs "qttools")
                       "/bin/lrelease")
        (string-append "ac_cv_path_LUPDATE="
                       (assoc-ref %build-inputs "qttools")
                       "/bin/lupdate")
        "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Disable utilprocess_tests because it never ends.
             ;; It looks like it tries to start /bin/sleep and waits until it
             ;; is in the list of running processes, but /bin/sleep doesn't
             ;; exist.
             (substitute* "src/Makefile.test.include"
               (("test/utilprocess_tests.cpp")
                ""))
             ;; Disable PaymentServer failing test because it's using
             ;; an expired SSL certificate.
             (substitute* "src/qt/test/test_main.cpp"
               (("if \\(QTest::qExec\\(&test2\\) != 0\\)")
                "if (QTest::qExec(&test2) == 0)"))
             ;; The following test passes with OpenSSL 1.1, but fails with
             ;; OpenSSL 3.
             (substitute* "src/secp256k1/src/tests.c"
               (("run_ecdsa_der_parse\\(\\);")
                ""))))
         (add-before 'check 'set-home
           (lambda _
             ;; Tests write to $HOME
             (setenv "HOME" (getenv "TMPDIR")))))))
    (home-page "https://www.bitcoinunlimited.info/")
    (synopsis "Client for the Bitcoin Cash protocol")
    (description
     "Bitcoin Unlimited is a client for the Bitcoin Cash peer-to-peer
electronic cash system.  This package provides a command line client and
a Qt GUI.")
    (license license:expat)))

(define-public fulcrum
  (package
    (name "fulcrum")
    (version "1.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cculianu/Fulcrum")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
                #~(for-each delete-file-recursively
                            '("src/Json/simdjson"
                              "src/bitcoin/secp256k1"
                              "src/robin_hood"
                              "src/zmq"
                              "staticlibs")))
              (sha256
               (base32
                "1110vanl6aczlq25i4ck9j4vr81in5icw4z383wyhjpcy6rwxsw2"))
              (patches
               (search-patches "fulcrum-1.9.1-unbundled-libraries.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "CONFIG+=config_without_bundled_cppzmq"
                   "CONFIG+=config_without_bundled_robin_hood"
                   "CONFIG+=config_without_bundled_secp256k1"
                   "LIBS+=-lrocksdb"
                   #$@(if (target-64bit?) '("LIBS+=-lsimdjson") '())
                   (format #f "DEFINES+=GIT_COMMIT=\"\\\\\\~s\\\\\\\""
                           #$version)
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (apply invoke "qmake" configure-flags))))))
    (native-inputs (list pkg-config qttools-5))
    (inputs
     (append (list cppzmq
                   jemalloc
                   python
                   qtbase-5
                   robin-hood-hashing
                   rocksdb
                   zeromq
                   zlib)
             (if (target-64bit?)
                 (list simdjson-0.6)
                 '())))
    (home-page "https://github.com/cculianu/Fulcrum")
    (synopsis "Payment verification server for Bitcoin-like crypto-currencies")
    (description
     "Fulcrum is a @acronym{SPV, Simplified Payment Verification} server for
Bitcoin-like crypto-currencies.  The server indexes the blockchain of the
crypto-currency used, and the resulting index can be used by wallets to
perform queries to keep real-time track of balances.

Supported crypto-currencies:

@itemize
@item Bitcoin Core.
@item Bitcoin Cash-like.
@item Litecoin.
@end itemize")
    (license license:gpl3+)))

(define-public flowee
  (package
    (name "flowee")
    (version "2020.04.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gitlab.com/FloweeTheHub/thehub/-/archive/"
                            version "/thehub-" version ".tar.gz"))
       (sha256
         (base32 "1vwvaxm3b71pfx8l4rrv06wqks6xdf2333w856b36s1bzvj53rhc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-Dbuild_tests=ON" "-Denable_gui=OFF")
       #:phases
        (modify-phases %standard-phases
          (add-before 'configure 'make-qt-deterministic
            (lambda _
              ;; Make Qt deterministic.
              (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t))
          (add-before 'configure 'disable-black-box
            ;; the black-box testing runs full hubs and lets them interact.
            ;; this is more fragile and a slow machine, or low memory machine, may
            ;; make the tests timeout and fail.  We just disable them here.
            (lambda _
              (substitute* "testing/CMakeLists.txt"
                (("test_api") ""))
              (substitute* "testing/CMakeLists.txt"
                (("add_subdirectory\\(api\\)") ""))
              #t))
          (add-after 'configure 'set-build-info
            ;; Their genbuild.sh to generate a build.h fails in guix (no .git dir) .
            ;; Its purpose is to write the tag name in the build.h file. We do that
            ;; here instead.
            (lambda _
              (with-output-to-file "include/build.h"
                (lambda _
                  (display
                    (string-append "#define BUILD_DESC " "\"", version "\""))))))
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" (getenv "TMPDIR")) ; tests write to $HOME
              #t))
          (replace 'check
            (lambda _
              (invoke "make" "check" "-C" "testing"))))))
    (inputs
     (list boost
           gmp
           libevent
           miniupnpc
           openssl
           qtbase-5))
    (native-inputs
     (list pkg-config qttools-5 util-linux))       ; provides the hexdump command for tests
    (home-page "https://flowee.org")
    (synopsis "Infrastructure tools and services")
    (description
     "Flowee packages all tier-1 applications and services from the Flowee group.
This includes components like The Hub and Indexer which and various others
that allows you to run services and through them access the Bitcoin Cash networks.")
    (license license:gpl3+)))


(define-public beancount
  (package
    (name "beancount")
    (version "2.3.6")
    (source
     (origin
       (method git-fetch) ; no test data files in PyPI archive
       (uri (git-reference
             (url "https://github.com/beancount/beancount")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1slxsjw29cyr2kbirdpijhpqspk55k38rpmk3zc02pr1wll62qsv"))
       (patches (search-patches "beancount-disable-googleapis-fonts.patch"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Remove broken experiments.
            (delete-file-recursively "experiments")
            ;; Remove bundled packages.
            (delete-file-recursively "third_party")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-append
                    ;; ModuleNotFoundError: No module named 'pytest'
                    "not test_parse_stdin"
                    ;; AssertionError: 5 not greater than 20
                    " and not test_setup"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.py"
                ;; Use compatible fork, and do not fail during sanity check.
                (("\"pdfminer2\",") ""))))
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list gnupg
           python-pdfminer-six
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-beautifulsoup4
           python-bottle
           python-chardet
           python-dateutil
           python-google-api-client
           python-google-auth-oauthlib
           python-lxml
           python-magic
           python-oauth2client
           python-ply
           python-requests))
    (home-page "https://beancount.github.io/")
    (synopsis "Command-line double-entry accounting tool")
    (description
     "Beancount is a double-entry bookkeeping computer language that lets you
define financial transaction records in a text file, read them in memory,
generate a variety of reports from them, and provides a web interface.")
    (license license:gpl2)))

(define-public fava
  (package
    (name "fava")
    (version "1.27")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fava" version))
       (sha256
        (base32 "0cw3pmyrknsw0h4w3v9vyk6wrii68zwkywsyyvjzyl2qz3xq8srk"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list beancount
           python-babel
           python-cheroot
           python-click
           python-flask
           python-flask-babel
           python-jinja2
           python-markdown2
           python-ply
           python-simplejson
           python-werkzeug))
    (native-inputs
     (list python-babel
           python-mypy
           python-pytest
           python-pytest-cov
           python-setuptools
           python-twine
           python-types-setuptools
           python-types-simplejson
           python-wheel))
    (home-page "https://beancount.github.io/fava/")
    (synopsis "Web interface for the accounting tool Beancount")
    (description "Fava is a web interface for the double-entry bookkeeping
software Beancount with a focus on features and usability.")
    (license license:expat)))

(define-public emacs-beancount
  (package
    (name "emacs-beancount")
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/beancount/beancount-mode")
             (commit version)))
       (sha256
        (base32
         "01ivxgv1g0pkr0xi43366pghc3j3mmhk5bshis6kkn04bq04cx7f"))
       (file-name (git-file-name name version))))
    (build-system emacs-build-system)
    (home-page "https://github.com/beancount/beancount-mode")
    (synopsis "Emacs mode for Beancount")
    (description
     "Emacs-beancount is an Emacs mode for the Beancount accounting tool.")
    (license license:gpl3+)))

(define-public hledger-web
  (package
    (name "hledger-web")
    (version "1.27.1")
    (source (origin
              (method url-fetch)
              (uri (hackage-uri "hledger-web" version))
              (sha256
               (base32
                "151dxci7dld8626dzw823sr3d9iaac92wfzbfcbdz4jh9f7n07wa"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "hledger-web")))
    (inputs (list ghc-decimal
                  ghc-aeson
                  ghc-base64
                  ghc-blaze-html
                  ghc-blaze-markup
                  ghc-breakpoint
                  ghc-case-insensitive
                  ghc-clientsession
                  ghc-cmdargs
                  ghc-conduit
                  ghc-conduit-extra
                  ghc-data-default
                  ghc-extra
                  ghc-hjsmin
                  ghc-hledger
                  ghc-hledger-lib
                  ghc-hspec
                  ghc-http-client
                  ghc-http-conduit
                  ghc-http-types
                  ghc-megaparsec
                  ghc-network
                  ghc-shakespeare
                  ghc-unix-compat
                  ghc-unordered-containers
                  ghc-utf8-string
                  ghc-wai
                  ghc-wai-cors
                  ghc-wai-extra
                  ghc-wai-handler-launch
                  ghc-warp
                  ghc-yaml
                  ghc-yesod
                  ghc-yesod-core
                  ghc-yesod-form
                  ghc-yesod-static
                  ghc-yesod-test))
    (arguments
     (list
      #:haddock? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests write to $HOME.
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp")))
           (add-after 'register 'remove-libraries
             (lambda* (#:key outputs #:allow-other-keys)
               (delete-file-recursively (string-append (assoc-ref outputs "out") "/lib")))))))
    (home-page "http://hledger.org")
    (synopsis "Web-based user interface for the hledger accounting system")
    (description
     "This package provides a simple Web-based User
Interface (UI) for the hledger accounting system.  It can be used as a
local, single-user UI, or as a multi-user UI for viewing, adding, and
editing on the Web.")
    (license license:gpl3)))

(define-public quantlib
  (package
    (name "quantlib")
    (version "1.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lballabio/QuantLib/releases/download/v"
             version "/QuantLib-" version ".tar.gz"))
       (sha256
        (base32 "0l7yn9bal0csyix0ydzcfj003kma4sx7w5hyfxhh6mbnxn6am1zb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ,#~(list "--disable-static"
                (string-append "--prefix=" #$output))))
    (inputs (list boost))
    (home-page "https://www.quantlib.org")
    (synopsis "Library for quantitative finance")
    (description
     "The QuantLib project is aimed at providing a comprehensive software
framework for quantitative finance.  QuantLib is a library for modeling,
trading, and risk management in real-life.")
    (license license:bsd-2)))

(define-public optionmatrix
  (package
    (name "optionmatrix")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/optionmatrix/optionmatrix-"
             version ".tar.xz"))
       (sha256
        (base32 "1zd0pfiphnijh1l94swb3mjrpmjsn37z11mklamd7zw6h2d4zh4d"))))
    (build-system gnu-build-system)
    (inputs
     (list gsl gtk+ ncurses))
    (native-inputs
     (list pkg-config texinfo
           (texlive-updmap.cfg (list texlive-epsf texlive-texinfo))))
    (home-page "https://anthonybradford.github.io/optionmatrix/")
    (synopsis "Financial derivative calculator")
    (description
     "The OptionMatrix programs are financial derivative calculators.  These
calculators are real-time multi-model option chain pricers with analytics and
interactive controls.  This package provides a GTK+ graphical user interface
(@code{optionmatrix}) and a curses interface (@code{optionmatrix_console}).")
    (license license:gpl3+)))

(define-public python-ta-lib
  (package
    (name "python-ta-lib")
    (version "0.6.3")
    (source
     (origin
       ;; Git repo contains Make rules to regenerate precompiled files
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TA-Lib/ta-lib-python")
             (commit (string-append "TA_Lib-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qf00rnsn3s38yxqym1q4bdh98yykik5jdw492gn5ymddr499n1f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Ignore Polars test (not packaged, depends on Rust)
      #:test-flags #~(list "--ignore" "tests/test_polars.py")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'delete-precompiled-files
                     (lambda _
                       (delete-file "talib/_ta_lib.c")))
                   (add-before 'build 'regenerate-talibc
                     (lambda _
                       (invoke "make" "cython"))))))
    (inputs (list ta-lib))
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-cython-3
                         python-pandas
                         python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/mrjbq7/ta-lib")
    (synopsis "Python wrapper for TA-Lib")
    (description
     "This is a Python wrapper for TA-Lib based on Cython.  TA-Lib is a library
providing common functions for the technical analysis of financial market data.")
    (license license:bsd-2)))

(define-public ta-lib
  (package
    (name "ta-lib")
    (version "0.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TA-Lib/ta-lib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "012sp7s1fxp6lnh3am0r6r46ya9jwnarkvlvq21w2nndqd4n4d39"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; no tests
           #:configure-flags #~(list "--disable-static")))
    (native-inputs (list autoconf-2.71 automake libtool))
    (home-page "https://ta-lib.org")
    (synopsis "Technical analysis library")
    (description
     "TA-Lib is a library providing common functions for the technical
analysis of financial market data.")
    (license license:bsd-3)))

(define-public python-mt-940
  (package
    (name "python-mt-940")
    (version "4.23.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WoLpH/mt940.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z9w1qalcphsck3j6vkrs7k47ah9zq2rv0lm9nmcsgwpyp59qkyf"))))
    (properties '(("upstream-name" #{.}# "mt-940")))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'check
                    (lambda* (#:key tests? inputs outputs #:allow-other-keys)
                      (when tests?
                        ;; Remove custom --cov flags.
                        (delete-file "pytest.ini")
                        (invoke "pytest" "-vv")))))))
    (native-inputs (list python-flake8
                         python-pytest
                         python-pyyaml))
    (home-page "https://mt940.readthedocs.io/")
    (synopsis "Python parser for MT940-encoded SWIFT data")
    (description
     "A library to parse MT940 files, a bank account statement exchange
format used by SWIFT.  It returns smart Python collections for statistics
and manipulation.")
    (license license:bsd-3)))

(define-public xmrig
  (package
    (name "xmrig")
    (version "6.22.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xmrig/xmrig")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256 (base32 "1h3qcs176xbfs1k2silr5rf13y0nag6qgsaz14qi3nrxxc0d8n4h"))
       (modules '((guix build utils)))
       (snippet
        ;; TODO: Try to use system libraries instead of bundled ones in
        ;; "src/3rdparty/". It requires changes to some "cmake/..." scripts
        ;; and to some source files.
        #~(begin
            (delete-file-recursively "src/3rdparty/hwloc")
            (substitute* "src/donate.h"
              (("constexpr const int kDefaultDonateLevel = 1;")
               "constexpr const int kDefaultDonateLevel = 0;")
              (("constexpr const int kMinimumDonateLevel = 1;")
               "constexpr const int kMinimumDonateLevel = 0;"))))))
    (build-system cmake-build-system)
    (inputs
     (list
      `(,hwloc "lib")
      libuv
      openssl))
    (arguments
     (list
      ;; There are no tests.
      #:tests? #f
      #:phases
      #~(modify-phases
         %standard-phases
         (replace 'install
           ;; There is no 'install' target, we must install xmrig manually
           (lambda* (#:key #:allow-other-keys)
             (install-file "xmrig"
                           (string-append #$output "/bin")))))))
    (home-page "https://xmrig.com/")
    (synopsis "Monero miner")
    (description
     "XMRig is a high-performance, cross-platform RandomX, KawPow,
CryptoNight and GhostRider unified CPU/GPU miner and RandomX benchmark.

Warning: upstream, by default, receives a percentage of the mining time.  This
anti-functionality has been neutralized in Guix, but possibly not in all other
distributions.

Warning: this software, because of its nature, has high energy consumption.
Also, the energy expenses might be higher than the cryptocurrency gained by
mining.")
    (license license:gpl3+)))

(define-public p2pool
  (package
    (name "p2pool")
    (version "4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SChernykh/p2pool")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256 (base32 "1hfdhanbdfjxv2n355m6b9n0ihxgcdlgxgnsqz5f6q59957fcyiw"))
       (modules '((guix build utils)))
       (snippet
        #~(for-each delete-file-recursively
                    '("external/lib"
                      "external/src/cppzmq"
                      "external/src/curl"
                      "external/src/libuv"
                      "external/src/libzmq"
                      "external/src/rapidjson"
                      "external/src/robin-hood-hashing")))))
    (build-system cmake-build-system)
    (inputs
     (list cppzmq curl libuv rapidjson robin-hood-hashing zeromq))
    (arguments
     (list ; FIXME: Linking fails when LTO is activated.
           #:configure-flags #~(list "-DWITH_LTO=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (mkdir-p "tests")
                     (chdir "tests")
                     (invoke "cmake" "-DWITH_LTO=OFF" "../../source/tests")
                     (invoke "make" "-j" (number->string (parallel-job-count)))
                     (invoke "gzip" "-d" "sidechain_dump.dat.gz")
                     (invoke "gzip" "-d" "sidechain_dump_mini.dat.gz")
                     (invoke "./p2pool_tests")
                     (chdir ".."))))
               (replace 'install
                 (lambda _
                   (install-file "p2pool" (string-append #$output "/bin")))))))
    (home-page "https://p2pool.io/")
    (synopsis "Decentralized Monero mining pool")
    (description "Monero P2Pool is a peer-to-peer Monero mining pool.  P2Pool
combines the advantages of pool and solo mining; you still fully control your
Monero node and what it mines, but you get frequent payouts like on a regular
pool.")
    (license license:gpl3)))

(define-public opentaxsolver
  ;; The OTS version is formatted like tax-year_version. So, at time of
  ;; writing, the version is 2023_21.03. Each part of this is used in
  ;; different places in the source uri, so it's convenient to have them
  ;; separately like this.
  (let ((tax-year "2023")
        (ots-version "21.03"))
    (package
      (name "opentaxsolver")
      (version (string-append tax-year "_" ots-version))
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://sourceforge/opentaxsolver/OTS_"
                             tax-year "/v" ots-version
                             "_linux/OpenTaxSolver" version "_linux64.tgz"))
         (sha256
          (base32
           "1i543bvclnyiwnyjlskhr2bxlsigggvwdhg2519rf12lsghgfszq"))
         (patches (search-patches "opentaxsolver-file-browser-fix.patch"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       (list
        #:tests? #f                     ;no tests
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)         ;no configure script
            ;; OTS does provide a shellscript that does exactly this, but we
            ;; need to do it ourselves to specify the correct compiler and to
            ;; delete the GUI binaries.
            (replace 'build
              (lambda _
                (delete-file "Run_taxsolve_GUI")
                (delete-file-recursively "bin")
                (mkdir "bin")
                (chdir "src/Gui_gtk")
                (invoke "make"
                        (string-append "CC=" #$(cc-for-target)))
                (chdir "..")
                (invoke "make"
                        (string-append "CC=" #$(cc-for-target)))))
            ;; OTS doesn't provide a `make install` target, because it assumes
            ;; it'll be run from the tarball. So, we do it ourselves, making
            ;; sure to replicate the directory structure of the tarball.
            (replace 'install
              (lambda _
                (copy-recursively "../bin"
                                  (string-append #$output "/bin"))
                (symlink (string-append #$output "/bin/ots_gui2")
                         (string-append #$output "/bin/Run_taxsolve_GUI"))
                (copy-recursively "../tax_form_files"
                                  (string-append #$output "/tax_form_files"))
                (copy-recursively "formdata"
                                  (string-append #$output "/src/formdata")))))))
      (native-inputs (list pkg-config))
      (inputs (list gtk+-2))
      (synopsis "Yearly tax preparation tool")
      (description
       "OpenTaxSolver is a program for calculating tax form entries for
federal and state personal income taxes.  It automatically fills out and
prints your forms for mailing.")
      (home-page "https://opentaxsolver.sourceforge.net/")
      (license license:gpl2+))))
