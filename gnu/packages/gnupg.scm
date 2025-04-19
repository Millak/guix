;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2021, 2023-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015, 2018 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2018 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Paul van der Walt <paul@denknerd.org>
;;; Copyright © 2015-2021, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015, 2016, 2017, 2019, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2016 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2016 Troy Sankey <sankeytms@gmail.com>
;;; Copyright © 2017, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Fredrik Salomonsson <plattfot@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2021 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages gnupg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages enlightenment)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pth)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages xdisorg)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (ice-9 match)
  #:use-module (guix build-system meson)
  #:use-module (srfi srfi-1))

(define-public libgpg-error
  (package
    (name "libgpg-error")
    (version "1.51")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libgpg-error/libgpg-error-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1cp64xa58977fysj1z1rgj60qxbjkzqpkpww6raysgmrnqnin3xy"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-install-gpg-error-config")
      #:phases
      #~(modify-phases %standard-phases
      #$@(cond
          ((%current-target-system)
           ;; If this is left out, some generated header
           ;; files will be sprinkled with ‘\c’, which
           ;; the compiler won't like.
           #~((add-after 'unpack 'fix-gen-lock-obj.sh
                (lambda _
                  (substitute* "src/gen-lock-obj.sh"
                    (("if test -n `echo -n`") "if ! test -n `echo -n`"))))
              ;; When cross-compiling, some platform specific properties cannot
              ;; be detected. Create a symlink to the appropriate platform
              ;; file if required. Note that these platform files depend on
              ;; both the operating system and architecture!
              ;;
              ;; See Cross-Compiling section at:
              ;; https://github.com/gpg/libgpg-error/blob/master/README
              (add-after 'unpack 'cross-symlinks
                (lambda _
                  (define (link triplet source)
                    (symlink (string-append "lock-obj-pub." triplet ".h")
                             (string-append "src/syscfg/lock-obj-pub."
                                            source ".h")))
                  #$(let ((target (%current-target-system)))
                      (cond ((target-linux? target)
                             (match (string-take target
                                                 (string-index target #\-))
                                    ("armhf"
                                     `(link "arm-unknown-linux-gnueabi" "linux-gnu"))
                                    ("mips64el"
                                     `(link "mips-unknown-linux-gnu" "linux-gnu"))
                                    ;; Don't always link to the "linux-gnu"
                                    ;; configuration, as this is not correct for
                                    ;; all architectures.
                                    (_ #t)))
                            (#t #t)))))))
          ((system-hurd?)
           #~((add-after 'unpack 'skip-tests
                (lambda _
                  (substitute* "tests/t-syserror.c"
                    (("(^| )main *\\(.*" all)
                     (string-append all "{\n  exit (77);//")))))))
          (else #~())))))
    (native-inputs (list gettext-minimal))
    (home-page "https://gnupg.org")
    (synopsis "Library of error values for GnuPG components")
    (description
     "Libgpg-error is a small library that defines common error values
for all GnuPG components.  Among these are GPG, GPGSM, GPGME,
GPG-Agent, libgcrypt, Libksba, DirMngr, Pinentry, SmartCard
Daemon and possibly more in the future.")
    (license license:lgpl2.0+)))

(define-public libgcrypt
  (package
    (name "libgcrypt")
    (version "1.11.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnupg/libgcrypt/libgcrypt-"
                                 version ".tar.bz2"))
             (sha256
              (base32
               "172vd1c1zn27mqd7cdb14hpjz35rhr9pg8dass0j0zyfcyc0q4h9"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("libgpg-error-host" ,libgpg-error)))
    (native-inputs
     ;; Needed here for the 'gpg-error' program.
     `(("libgpg-error-native" ,libgpg-error)))
    (arguments
     ;; The '--with-gpg-error-prefix' argument is needed because otherwise
     ;; 'configure' uses 'gpg-error-config' to determine the '-L' flag, and
     ;; the 'gpg-error-config' it runs is the native one---i.e., the wrong one.
     `(#:configure-flags
       (list (string-append "--with-libgpg-error-prefix="
                            (assoc-ref %build-inputs "libgpg-error-host"))
             ;; libgcrypt is transitioning from gpg-error-config to
             ;; gpgrt-config, and in the process the
             ;; --with-libgpg-error-config prefix defined above is
             ;; not respected.  See <https://dev.gnupg.org/T5365>.
             ;; TODO: transition to pkg-config instead of these scripts.
             (string-append "ac_cv_path_GPGRT_CONFIG="
                            (assoc-ref %build-inputs
                                       "libgpg-error-host")
                            "/bin/gpgrt-config")
             ,@(if (%current-target-system)
                   ;; When cross-compiling, _gcry_mpih_lshift etc are undefined.
                   `("--disable-asm")
                   '()))
       ,@(if (system-hurd?)
             (list
              #:phases
              #~(modify-phases %standard-phases
                  (add-before 'configure 'setenv
                    (lambda _
                      (setenv "GCRYPT_NO_BENCHMARKS" "t")))))
             '())))
    (outputs '("out" "debug"))
    (home-page "https://gnupg.org/")
    (synopsis "Cryptographic function library")
    (description
     "Libgcrypt is a general-purpose cryptographic library.  It provides the
standard cryptographic building blocks such as symmetric ciphers, hash
algorithms, public key algorithms, large integer functions and random number
generation.")
    (license license:lgpl2.0+)))

(define-public libassuan
  (package
    (name "libassuan")
    (version "3.0.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/libassuan/libassuan-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1ccly6aqyxv3hgshhls6qw177salcrawp0x4lsqs9ph3c4pg9w68"))))
    (build-system gnu-build-system)
    (arguments (if (%current-target-system)
                   (list #:configure-flags
                         #~(list (string-append
                                  "--with-libgpg-error-prefix="
                                  #$(this-package-input "libgpg-error"))))
                   '()))
    (propagated-inputs (list libgpg-error))
    (home-page "https://gnupg.org")
    (synopsis
     "IPC library used by GnuPG and related software")
    (description
     "Libassuan is a small library implementing the so-called Assuan
protocol.  This protocol is used for IPC between most newer
GnuPG components.  Both, server and client side functions are
provided.")
    (license license:lgpl2.0+)))

(define-public libksba
  (package
    (name "libksba")
    (version "1.6.7")
    (source
     (origin
      (method url-fetch)
      (uri (string-append
            "mirror://gnupg/libksba/libksba-"
            version ".tar.bz2"))
      (sha256
       (base32
        "0qxpmadxggx5808326i9g4ya0xrnv14mfxpg7rlvckmviq5m2wng"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list libgpg-error))
    (arguments
     `(#:configure-flags
       (list ,@(if (%current-target-system)
                   '("CC_FOR_BUILD=gcc")
                   '())
             (string-append "--with-gpg-error-prefix="
                            (assoc-ref %build-inputs "libgpg-error")))))
    (home-page "https://www.gnupg.org")
    (synopsis "CMS and X.509 access library")
    (description
     "KSBA (pronounced Kasbah) is a library to make X.509 certificates
as well as the CMS easily accessible by other applications.  Both
specifications are building blocks of S/MIME and TLS.")
    (license license:gpl3+)))

(define-public npth
  (package
    (name "npth")
    (version "1.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnupg/npth/npth-" version ".tar.bz2"))
       (sha256
        (base32 "0gnaj176jjfi6ldrq1l1sx7ym0z7kjx8ms96bdp5s1m34d7lpllb"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnupg.org")
    (synopsis "Non-preemptive thread library")
    (description
     "Npth is a library to provide the GNU Pth API and thus a non-preemptive
threads implementation.

In contrast to GNU Pth is is based on the system's standard threads
implementation.  This allows the use of libraries which are not
compatible to GNU Pth.")
    (license (list license:lgpl3+ license:gpl2+)))) ;dual license

(define-public gnupg
  (package
    (name "gnupg")
    ;; Note: Odd minor versions are usually for development purposes.  See
    ;; <https://gnupg.org/download/index.html> for how to pick the right
    ;; version.
    (version "2.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (patches (search-patches "gnupg-default-pinentry.patch"))
              (sha256
               (base32
                "0ipbhlxwr79l66b907640a0j67s04w826s50djqf7q579mp7093v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list gnutls
           libassuan
           libgcrypt
           libgpg-error
           libksba
           npth
           openldap
           pcsc-lite
           readline
           sqlite
           zlib))
    (arguments
     (list
      #:configure-flags
      ;; Always use quasiquote on the next core-updates cycle.
      #~(#$(if (%current-target-system)
               #~quasiquote
               #~quote)
         (#$@(if (%current-target-system)
                 #~(,(string-append
                      "--with-libgpg-error-prefix="
                      #$(this-package-input "libgpg-error"))
                    ,(string-append
                      "--with-libgcrypt-prefix="
                      #$(this-package-input "libgcrypt"))
                    ,(string-append
                      "--with-libassuan-prefix="
                      #$(this-package-input "libassuan"))
                    ,(string-append
                      "--with-ksba-prefix="
                      #$(this-package-input "libksba"))
                    ,(string-append
                      "--with-npth-prefix="
                      #$(this-package-input "npth")))
                 #~())
          ;; Otherwise, the test suite looks for the `gpg`
          ;; executable in its installation directory in
          ;; /gnu/store before it has been installed.
          "--enable-gnupg-builddir-envvar"
          "--enable-all-tests"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libpcsclite.so (search-input-file inputs
                                                       "lib/libpcsclite.so")))
                (substitute* "scd/scdaemon.c"
                  (("libpcsclite\\.so")
                   libpcsclite.so)))))
          (add-after 'build 'patch-scheme-tests
            (lambda _
              (substitute* (find-files "tests" ".\\.scm$")
                (("/usr/bin/env gpgscm")
                 (string-append (getcwd) "/tests/gpgscm/gpgscm")))))
          (add-before 'build 'patch-test-paths
            (lambda _
              (substitute* '("tests/pkits/inittests"
                             "tests/pkits/common.sh"
                             "tests/pkits/Makefile")
                (("/bin/pwd") (which "pwd")))
              (substitute* "common/t-exectool.c"
                (("/bin/cat") (which "cat"))
                (("/bin/true") (which "true"))
                (("/bin/false") (which "false"))))))))
    (home-page "https://gnupg.org/")
    (synopsis "GNU Privacy Guard")
    (description
     "The GNU Privacy Guard is a complete implementation of the OpenPGP
standard.  It is used to encrypt and sign data and communication.  It
features powerful key management and the ability to access public key
servers.  It includes several libraries: libassuan (IPC between GnuPG
components), libgpg-error (centralized GnuPG error values), and
libskba (working with X.509 certificates and CMS data).")
    (license license:gpl3+)))

(define-public gnupg-1
  (package (inherit gnupg)
    (version "1.4.23")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gnupg/gnupg-" version
                                  ".tar.bz2"))
              (sha256
               (base32
                "1fkq4sqldvf6a25mm2qz95swv1qjg464736091w51djiwqbjyin9"))
              (patches (search-patches "gnupg-1-build-with-gcc10.patch"))))
    (native-inputs '())
    (inputs
     (list zlib bzip2 curl readline libgpg-error))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-check-sh
           (lambda _
             (substitute* "checks/Makefile.in"
               (("/bin/sh") (which "sh"))))))))))

(define-public gpgme
  (package
    (name "gpgme")
    (version "1.24.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnupg/gpgme/gpgme-" version ".tar.bz2"))
      (sha256
       (base32 "0px87fbp90xp8vf1wms02flk14zmrqsfr135f5his1kiiqjx01ga"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(let ((gpg-bins (dirname (search-input-file %build-inputs "/bin/gpg"))))
          (list (string-append "--enable-fixed-path=" gpg-bins)))))
    (inputs
     (list gnupg))
    (propagated-inputs
     ;; As required by the pkg-config's Requires.private.
     (list libgpg-error libassuan))
    (home-page "https://www.gnupg.org/related_software/gpgme/")
    (synopsis "Library providing simplified access to GnuPG functionality")
    (description
     "GnuPG Made Easy (GPGME) is a library designed to make access to GnuPG
easier for applications.  It provides a High-Level Crypto API for encryption,
decryption, signing, signature verification and key management.  Currently
it uses GnuPG as its backend but the API isn't restricted to this engine.

Because the direct use of GnuPG from an application can be a complicated
programming task, it is suggested that all software should try to use GPGME
instead.  This way bug fixes or improvements can be done at a central place
and every application benefits from this.")
    (license license:lgpl2.1+)))

(define-public qgpgme
  (package
    (inherit gpgme)
    (name "qgpgme")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir-and-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gpgme (assoc-ref inputs "gpgme")))
               (symlink (string-append gpgme "/lib/libgpgmepp.la")
                        "lang/cpp/src/libgpgmepp.la")
               (symlink (string-append gpgme "/lib/libgpgme.la")
                        "src/libgpgme.la"))
             (chdir "lang/qt"))))))
    (propagated-inputs (list gpgme))    ;required by QGpgmeConfig.cmake
    (native-inputs
     (modify-inputs (package-native-inputs gpgme)
       (prepend pkg-config)))
    (inputs
     (modify-inputs (package-inputs gpgme)
       (prepend qtbase-5)))
    (synopsis "Qt API bindings for gpgme")
    (description "QGpgme provides a very high level Qt API around GpgMEpp.

QGpgME was originally developed as part of libkleo and incorporated into
gpgpme starting with version 1.7.")
    (license license:gpl2+))) ;; Note: this differs from gpgme

(define-public qgpgme-qt6
  (package
    (inherit gpgme)
    (name "qgpgme-qt6")
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir-and-symlink
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gpgme (assoc-ref inputs "gpgme")))
               (symlink (string-append gpgme "/lib/libgpgmepp.la")
                        "lang/cpp/src/libgpgmepp.la")
               (symlink (string-append gpgme "/lib/libgpgme.la")
                        "src/libgpgme.la"))
             (chdir "lang/qt"))))))
    (propagated-inputs (list gpgme))    ;required by QGpgmeConfig.cmake
    (native-inputs
     (modify-inputs (package-native-inputs gpgme)
       (prepend pkg-config)))
    (inputs
     (modify-inputs (package-inputs gpgme)
       (prepend qtbase)))
    (synopsis "Qt API bindings for gpgme")
    (description "QGpgme provides a very high level Qt API around GpgMEpp.")
    (license license:gpl2+)))

(define-public guile-gcrypt
  (package
    (name "guile-gcrypt")
    (version "0.4.0")
    (home-page "https://notabug.org/cwebber/guile-gcrypt")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append home-page ".git"))
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0m75h9q10yb27kzjsvhhq0yk3jaxiy9bpbfd9qg269hf9gabgfdx"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     ;; Work around <https://bugs.gnu.org/20272> to achieve reproducible
     ;; builds.
     '(#:parallel-build? #f

       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'add-libgrypt-config
           (lambda* (#:key inputs target #:allow-other-keys)
             (when target
               ;; When cross-compiling, the bash script 'libgcrypt-config'
               ;; must be accessible during the configure phase.
               (setenv "PATH"
                       (string-append
                        (dirname
                         (search-input-file inputs "bin/libgcrypt-config"))
                        ":" (getenv "PATH")))))))))
    (native-inputs
     (list pkg-config autoconf automake texinfo guile-3.0))
    (inputs
     (list guile-3.0 libgcrypt))
    (synopsis "Cryptography library for Guile using Libgcrypt")
    (description
     "Guile-Gcrypt provides a Guile interface to a subset of the
GNU Libgcrypt crytographic library.  It provides modules for cryptographic
hash functions, message authentication codes (MAC), public-key cryptography,
strong randomness, and more.  It is implemented using the foreign function
interface (FFI) of Guile.")
    (license license:gpl3+)))

(define-public guile2.0-gcrypt
  (package (inherit guile-gcrypt)
    (name "guile2.0-gcrypt")
    (native-inputs
     (modify-inputs (package-native-inputs guile-gcrypt)
       (replace "guile" guile-2.0)))
    (inputs
     (modify-inputs (package-inputs guile-gcrypt)
       (replace "guile" guile-2.0)))))

(define-public guile2.2-gcrypt
  (package
    (inherit guile-gcrypt)
    (name "guile2.2-gcrypt")
    (native-inputs
     (modify-inputs (package-native-inputs guile-gcrypt)
       (replace "guile" guile-2.2)))
    (inputs
     (modify-inputs (package-inputs guile-gcrypt)
       (replace "guile" guile-2.2)))))

(define-public python-gpg
  (package
    (name "python-gpg")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gpg" version))
              (sha256
               (base32
                "1ji3ynhp36m1ccx7bmaq75dhij9frpn19v9mpi4aajn8csl194il"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-environment
           (lambda _
             ;; GPGME is built with large file support, so we need to set
             ;; _FILE_OFFSET_BITS to 64 in all users of the GPGME library.
             ,@(if (or (target-x86-32?) (target-arm32?))
                   `((substitute* "setup.py"
                       (("extra_macros = dict\\(\\)")
                        "extra_macros = { \"_FILE_OFFSET_BITS\": 64 }")))
                   '())
             (substitute* "setup.py"
               (("cc") (which "gcc")))
             #t)))
       #:tests? #f)) ; No test suite.
    (inputs
     (list gpgme))
    (native-inputs
     (list swig))
    (home-page (package-home-page gpgme))
    (synopsis "Python bindings for GPGME GnuPG cryptography library")
    (description "This package provides Python bindings to the GPGME GnuPG
cryptographic library.  It is developed in the GPGME source code, and then
distributed separately.")
    (license license:lgpl2.1+)))

(define-public python-pygpgme
  (package
    (name "python-pygpgme")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pygpgme" version))
       (sha256
        (base32
         "1px1c5nqsls3fxg0zkyd9sgc5rxpdagvsadnp8fd5bmgrrjka5ws"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list gnupg python-setuptools python-wheel))
    (inputs
     (list gpgme))
    (home-page "https://github.com/jhenstridge/pygpgme")
    (synopsis "Python module for working with OpenPGP messages")
    (description
     "PyGPGME is a Python module that lets you sign, verify, encrypt and
decrypt messages using the OpenPGP format by making use of GPGME.")
    (license license:lgpl2.1+)))

(define-public python-gnupg
  (package
    (name "python-gnupg")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-gnupg" version))
       (sha256
        (base32
         "0ali2zz6k568yzhdgzm8f14v6s5ymihlyffbvfxc9q60gww8wxbh"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (substitute* "test_gnupg.py"
                 ;; Unsure why this test fails.
                 (("'test_search_keys'") "True")
                 (("def test_search_keys") "def disabled__search_keys"))
               (setenv "USERNAME" "guixbuilder")
               ;; The doctests are extremely slow and sometimes time out,
               ;; so we disable them.
               (invoke "python" "test_gnupg.py" "--no-doctests")))))))
    (native-inputs
     (list gnupg python-setuptools python-wheel))
    (home-page "https://pythonhosted.org/python-gnupg/index.html")
    (synopsis "Wrapper for the GNU Privacy Guard")
    (description
      "This module allows easy access to GnuPG’s key management, encryption
and signature functionality from Python programs.")
    (license license:bsd-3)))

(define-public perl-gnupg-interface
  (package
    (name "perl-gnupg-interface")
    (version "0.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/A/AL/ALEXMV/"
                                  "GnuPG-Interface-" version ".tar.gz"))
              (sha256
               (base32
                "0dgx8yhdsmhkazcrz14n4flrk1afv7azgl003hl4arxvi1d9yyi4"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; FIXME: This test fails for unknown reasons
         (add-after 'unpack 'delete-broken-test
           (lambda _
             (delete-file "t/encrypt_symmetrically.t")
             #t)))))
    (inputs
     (list gnupg-1))
    (propagated-inputs
     (list perl-moo perl-moox-handlesvia perl-moox-late))
    (native-inputs
     (list which perl-module-install))
    (home-page "https://metacpan.org/release/GnuPG-Interface")
    (synopsis "Perl interface to GnuPG")
    (description "@code{GnuPG::Interface} and its associated modules are
designed to provide an object-oriented method for interacting with GnuPG,
being able to perform functions such as but not limited to encrypting,
signing, decryption, verification, and key-listing parsing.")
    (license license:perl-license)))

(define-public pius
  (package
    (name "pius")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jaymzh/pius/releases/download/v"
                    version "/pius-" version ".tar.bz2"))
              (sha256
               (base32
                "11fhmfvr0avxl222rv43wjd2xjbpxrsmcl8xwmn0nvf1rw95v9fn"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-gpg-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libpius/constants.py"
               (("/usr/bin/gpg2")
                (search-input-file inputs "bin/gpg"))))))))
    (inputs (list perl                  ;for 'pius-party-worksheet'
                  gnupg))
    (synopsis "Programs to simplify GnuPG key signing")
    (description
     "Pius (PGP Individual UID Signer) helps attendees of PGP key signing
parties.  It is the main utility and makes it possible to quickly and easily
sign each UID on a set of PGP keys.  It is designed to take the pain out of
the sign-all-the-keys part of PGP key signing parties while adding security to
the process.  The @command{pius-keyring-mgr} and
@command{pius-party-worksheet} commands help organizers of PGP key signing
parties.")
    (license license:gpl2)
    (home-page "https://www.phildev.net/pius/index.shtml")))

(define-public signing-party
  (package
    (name "signing-party")
    (version "2.11")
    (home-page "https://salsa.debian.org/signing-party-team/signing-party")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aig5ssabzbk4mih7xd04vgr931bw0flbi8dz902wlr610gyv5s5"))))
    (build-system gnu-build-system)
    (native-inputs
     ;; autoconf-wrapper is required due to the non-standard
     ;; 'configure phase.
     (list autoconf-wrapper automake))
    (inputs (list bash-minimal
                  perl
                  perl-text-template
                  perl-mime-tools
                  perl-gnupg-interface
                  perl-net-idn-encode
                  libmd))
    (arguments
     `(#:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "keyanalyze/Makefile"
                 (("LDLIBS") (string-append "CC=" (which "gcc") "\nLDLIBS")))
               (substitute* "keyanalyze/Makefile"
                 (("\\./configure") (string-append "./configure --prefix=" out)))
               (substitute* "gpgwrap/Makefile"
                 (("\\} clean")
                  (string-append "} clean\ninstall:\n\tinstall -D bin/gpgwrap "
                                 out "/bin/gpgwrap\n")))
               (substitute* '("gpgsigs/Makefile" "keyanalyze/Makefile"
                              "keylookup/Makefile" "sig2dot/Makefile"
                              "springgraph/Makefile")
                 (("/usr") out))
               (setenv "CONFIG_SHELL" (which "sh")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys #:rest args)
             (let ((out (assoc-ref outputs "out"))
                   (install (assoc-ref %standard-phases 'install)))
               (apply install args)
               (for-each
                (lambda (dir file)
                  (copy-file (string-append dir "/" file)
                             (string-append out "/bin/" file)))
                '("caff" "caff" "caff" "gpgdir" "gpg-key2ps"
                  "gpglist" "gpg-mailkeys" "gpgparticipants")
                '("caff" "pgp-clean" "pgp-fixkey" "gpgdir" "gpg-key2ps"
                  "gpglist" "gpg-mailkeys" "gpgparticipants"))
               (for-each
                (lambda (dir file)
                  (copy-file (string-append dir "/" file)
                             (string-append out "/share/man/man1/" file)))
                '("caff" "caff" "caff" "gpgdir"
                  "gpg-key2ps" "gpglist" "gpg-mailkeys"
                  "gpgparticipants" "gpgsigs" "gpgwrap/doc"
                  "keyanalyze" "keyanalyze/pgpring" "keyanalyze")
                '("caff.1" "pgp-clean.1" "pgp-fixkey.1" "gpgdir.1"
                  "gpg-key2ps.1" "gpglist.1" "gpg-mailkeys.1"
                  "gpgparticipants.1" "gpgsigs.1" "gpgwrap.1"
                  "process_keys.1" "pgpring.1" "keyanalyze.1")))))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (wrap-program
                   (string-append out "/bin/caff")
                 `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB"))))))))))
    (synopsis "Collection of scripts for simplifying gnupg key signing")
    (description
     "Signing-party is a collection for all kinds of PGP/GnuPG related things,
including tools for signing keys, keyring analysis, and party preparation.
@enumerate
@item caff: CA - Fire and Forget signs and mails a key
@item pgp-clean: removes all non-self signatures from key
@item pgp-fixkey: removes broken packets from keys
@item gpg-mailkeys: simply mail out a signed key to its owner
@item gpg-key2ps: generate PostScript file with fingerprint paper strips
@item gpgdir: recursive directory encryption tool
@item gpglist: show who signed which of your UIDs
@item gpgsigs: annotates list of GnuPG keys with already done signatures
@item gpgparticipants: create list of party participants for the organiser
@item gpgwrap: a passphrase wrapper
@item keyanalyze: minimum signing distance (MSD) analysis on keyrings
@item keylookup: ncurses wrapper around gpg --search
@item sig2dot: converts a list of GnuPG signatures to a .dot file
@item springgraph: creates a graph from a .dot file
@end enumerate")
    ;; gpl2+ for almost all programs, except for keyanalyze: gpl2
    ;; and caff and gpgsigs: bsd-3, see
    ;; http://packages.debian.org/changelogs/pool/main/s/signing-party/current/copyright
    (license license:gpl2)))

(define-public pinentry-tty
  (package
    (name "pinentry-tty")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/pinentry/pinentry-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "014crqmr05lsfv13sj6jkcn6w1rvwpxc5hwn32mhg413qwkywwmw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-pinentry-tty")))
    (inputs
     (list ncurses libassuan
           `(,libsecret "out")))
    (native-inputs
     (list pkg-config))
    (home-page "https://gnupg.org/aegypten2/")
    (synopsis "GnuPG's interface to passphrase input")
    (description
     "Pinentry provides a console that allows users to enter a passphrase when
@code{gpg} is run and needs it.")
    (license license:gpl2+)
    (properties
     '((release-monitoring-url . "https://gnupg.org/ftp/gcrypt/pinentry/")
       (upstream-name . "pinentry")))))

(define-public pinentry-emacs
  (package
    (inherit pinentry-tty)
    (name "pinentry-emacs")
    (arguments
     `(#:configure-flags '("--enable-pinentry-emacs")))
    (description
     "Pinentry provides a console and an Emacs interface that allows users to
enter a passphrase when required by @code{gpg} or other software.")))

(define-public pinentry-gtk2
  (package
    (inherit pinentry-tty)
    (name "pinentry-gtk2")
    (arguments
     `(#:configure-flags '("--enable-fallback-curses")))
    (inputs
     (modify-inputs (package-inputs pinentry-tty)
       (prepend gtk+-2 glib)))
    (description
     "Pinentry provides a console and a GTK+ GUI that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-gnome3
  (package
    (inherit pinentry-tty)
    (name "pinentry-gnome3")
    (inputs
     (modify-inputs (package-inputs pinentry-tty)
       (prepend gtk+-2 gcr-3 glib)))
    (arguments
     `(#:configure-flags '("--enable-pinentry-gnome3"
                           "--enable-fallback-curses")))
    (description
     "Pinentry provides a console and a GUI designed for use with GNOME@tie{}3
that allows users to enter a passphrase when required by @code{gpg} or other
software.")))

(define-public pinentry-qt
  (package
    (inherit pinentry-tty)
    (name "pinentry-qt")
    (arguments
     `(#:configure-flags '("--enable-fallback-curses")))
    (inputs
     (modify-inputs (package-inputs pinentry-tty)
       (prepend qtbase qtwayland)))
  (description
   "Pinentry provides a console and a Qt GUI that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-qt5
  (package
    (inherit pinentry-qt)
    (name "pinentry-qt5")
    (inputs
     (modify-inputs (package-inputs pinentry-qt)
       (replace "qtbase" qtbase-5)
       (replace "qtwayland" qtwayland-5)))))

(define-public pinentry-efl
  (package
    (inherit pinentry-tty)
    (name "pinentry-efl")
    (arguments
     '(#:configure-flags '("--enable-pinentry-efl"
                           "--enable-fallback-curses")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             (invoke "sh" "autogen.sh"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ,@(package-native-inputs pinentry-tty)))
    (inputs
     (modify-inputs (package-inputs pinentry-tty)
       (prepend efl)))
    (description
   "Pinentry provides a console and a graphical interface for @acronym{EFL,
the Enlightenment Foundation Libraries} that allows users to enter a
passphrase when @code{gpg} is run and needs it.")))

(define-public pinentry-rofi
  (package
    (name "pinentry-rofi")
    (version "3.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plattfot/pinentry-rofi/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0m3lv6cydx2jg7743m6lab0v1myz84xys9z2sdb1b9jiq4xmayhq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules
       ((ice-9 match)
        (ice-9 ftw)
        ,@%default-gnu-modules)
       #:phases
       (modify-phases
           %standard-phases
         (add-after 'install 'hall-wrap-binaries
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (site (string-append out "/share/guile/site"))
                    (rofi-bin (string-append (assoc-ref inputs "rofi") "/bin")))
               (match (scandir site)
                 (("." ".." version)
                  (wrap-program
                      (string-append bin "pinentry-rofi")
                    (list "PATH" ":" 'prefix `(,rofi-bin))))))))
         (add-after 'compress-documentation 'installcheck
           (lambda* rest
             (invoke "make" "installcheck"))))))
    (native-inputs
     (list autoconf autoconf-archive automake pkg-config texinfo))
    (inputs (list bash-minimal guile-3.0 rofi))
    (synopsis "Rofi GUI for GnuPG's passphrase input")
    (description "Pinentry-rofi is a simple graphical user interface for
passphrase or PIN when required by @code{gpg} or other software.  It is using
the Rofi application launcher as the user interface.  Which makes it combined
with @code{rofi-pass} a good front end for @code{password-store}.")
    (home-page "https://github.com/plattfot/pinentry-rofi/")
    (license license:gpl3+)))

(define-public pinentry-bemenu
  (package
    (name "pinentry-bemenu")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/t-8ch/pinentry-bemenu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09nw49pyfs65m35a40kpzh6h0mf5yyjzmzq3jxp660885m0b29g8"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list bemenu libassuan libgpg-error popt))
    (home-page "https://github.com/t-8ch/pinentry-bemenu")
    (synopsis "Pinentry implementation based on @code{bemenu}")
    (description
     "This package provides a Pinentry implementation based on Bemenu.")
    (license license:gpl3+)))

(define-public pinentry
  (package (inherit pinentry-gtk2)
    (name "pinentry")))

(define-public paperkey
  (package
    (name "paperkey")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.jabberwocky.com/"
                                  "software/paperkey/paperkey-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xq5gni6gksjkd5avg0zpd73vsr97appksfx0gx2m38s4w9zsid2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'patch-check-scripts
           (lambda _
             (substitute* '("checks/roundtrip.sh"
                            "checks/roundtrip-raw.sh")
               (("/bin/echo") "echo"))
             #t)))))
    (home-page "https://www.jabberwocky.com/software/paperkey/")
    (synopsis "Backup OpenPGP keys to paper")
    (description
     "Paperkey extracts the secret bytes from an OpenPGP (GnuPG, PGP, etc) key
for printing with paper and ink, which have amazingly long retention
qualities.  To reconstruct a secret key, you re-enter those
bytes (whether by hand, OCR, QR code, or the like) and paperkey can use
them to transform your existing public key into a secret key.")
    (license license:gpl2+)))

(define-public pgpdump
  (package
    (name "pgpdump")
    (version "0.35")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.mew.org/~kazu/proj/pgpdump/pgpdump-"
                           version ".tar.gz"))
       (sha256
        (base32 "0kslr62h3wazg4x0l38lsmswvh2dizpnwcrdsmqz62b3plnr40jf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no make check
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target)))))
    (inputs
     (list zlib))
    (home-page "https://www.mew.org/~kazu/proj/pgpdump/en/")
    (synopsis "PGP packet visualizer")
    (description "pgpdump displays the sequence of OpenPGP or PGP version 2
packets from a file.

The output of this command is similar to GnuPG's list packets command,
however, pgpdump produces more detailed and easier to understand output.")
    (license license:bsd-3)))

(define-public gpa
  (package
    (name "gpa")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnupg/gpa/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1k1kvxffyb4nm83yp3mnx9bfmcciwb7vfw8c3xscnh85yxdzma16"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gnupg (assoc-ref inputs "gnupg")))
               (wrap-program (string-append out "/bin/gpa")
                 `("PATH" ":" prefix (,(string-append gnupg "/bin"))))))))))
    (native-inputs (list pkg-config))
    (inputs
     (list bash-minimal
           gnupg
           gpgme
           libassuan
           libgpg-error
           gtk+))
    (home-page "https://gnupg.org/software/gpa/")
    (synopsis "Graphical user interface for GnuPG")
    (description
     "GPA, the GNU Privacy Assistant, is a graphical user interface for
@uref{https://gnupg.org, GnuPG}.  It can be used to encrypt, decrypt, and sign
files, to verify signatures, and to manage the private and public keys.")
    (license license:gpl3+)))

(define-public parcimonie
  (package
    (name "parcimonie")
    (version "0.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gaffer.boum.org/intrigeri/files/"
                                  "parcimonie/App-Parcimonie-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10gal2h8ihg7nnzy3adw942axd2ia1rcn1fw3a3v07n5mm8kqrx9"))))
    (build-system perl-build-system)
    (inputs
     (list bash-minimal
           gnupg
           perl-clone
           perl-config-general
           perl-file-homedir
           perl-file-sharedir
           perl-file-which
           perl-gnupg-interface
           perl-ipc-system-simple
           perl-json
           perl-list-moreutils
           perl-moo
           perl-moox-late
           perl-moox-options
           perl-moox-strictconstructor
           perl-namespace-clean
           perl-net-dbus
           perl-pango
           perl-path-tiny
           perl-time-duration
           perl-time-duration-parse
           perl-try-tiny
           perl-type-tiny
           perl-types-path-tiny
           torsocks))
    (native-inputs
     (list perl-file-which
           perl-gnupg-interface
           perl-list-moreutils
           perl-lwp-online
           perl-module-build
           perl-strictures-2
           perl-test-most
           perl-test-trap
           xorg-server-for-tests))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Needed for using gpg-connect-agent during tests.
         (add-before 'check 'prepare-for-tests
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((Xvfb (search-input-file inputs "/bin/Xvfb")))
               (system (string-append Xvfb " :1 &"))
               (setenv "DISPLAY" ":1")
               (setenv "HOME" "/tmp")
               ;; These tests expect usable gnupg configurations.
               (delete-file "t/32-keyserver_defined_on_command_line.t")
               (delete-file "t/33-checkGpgHasDefinedKeyserver.t"))))
         (add-before 'install 'fix-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "lib/App/Parcimonie/GnuPG/Interface.pm"
               ;; Skip check whether dependencies are in the PATH
               (("defined which.*") ""))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (perllib (string-append out "/lib/perl5/site_perl/"
                                            ,(package-version perl))))
               (wrap-program (string-append out "/bin/parcimonie")
                 `("PERL5LIB" ":"
                   prefix (,(string-append perllib ":" (getenv "PERL5LIB")))))))))))
    (home-page "https://salsa.debian.org/intrigeri/parcimonie")
    (synopsis "Incrementally refreshes a GnuPG keyring")
    (description "Parcimonie incrementally refreshes a GnuPG keyring in a way
that makes it hard to correlate the keyring content to an individual, and
makes it hard to locate an individual based on an identifying subset of her
keyring content.  Parcimonie is a daemon that fetches one key at a time using
the Tor network, waits a bit, changes the Tor circuit being used, and starts
over.")
    (properties '((upstream-name . "App-Parcimonie")))
    (license license:gpl1+)))

(define-public jetring
  (package
    (name "jetring")
    (version "0.31")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://salsa.debian.org/debian/jetring")
               (commit "535380166eb1b222ba34864af07f3e36f4fb52c9")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "19m7rj446pr4nql44khwq0cfxfrm8cslj5v9jll08p7nk6glq5px"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-before 'install 'hardlink-gnupg
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gpg (search-input-file inputs "/bin/gpg")))
               (substitute* (find-files "." "jetring-[[:alpha:]]+$")
                 (("gpg -") (string-append gpg " -"))
                 (("\\\"gpg\\\"") (string-append "\"" gpg "\""))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man")))
               (for-each (lambda (file)
                           (install-file file (string-append out "/bin/")))
                         (find-files "." "jetring-[[:alpha:]]+$"))
               (for-each (lambda (file)
                           (install-file file (string-append man "/man1/")))
                         (find-files "." ".*\\.1$"))
               (install-file "jetring.7" (string-append man "/man7/"))))))
       #:tests? #f))                    ; no tests
    (inputs
     (list gnupg perl))
    (home-page "https://joeyh.name/code/jetring/")
    (synopsis "GnuPG keyring maintenance using changesets")
    (description
     "Jetring is a collection of tools that allow for gpg keyrings to be
maintained using changesets.  It was developed with the Debian keyring in mind,
and aims to solve the problem that a gpg keyring is a binary blob that's hard
for multiple people to collaboratively edit.

With jetring, changesets can be submitted, reviewed to see exactly what they
will do, applied, and used to build a keyring.  The origin of every change made
to the keyring is available for auditing, and gpg signatures can be used for
integrity guarantees.")
    (license license:gpl2+)))
