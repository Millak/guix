;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2012-2017, 2019-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2020, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2019, 2021-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2016 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021, 2023, 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Greg Hogan <code@greghogan.com>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system meson)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE))
  #:use-module (gnu packages compression)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages time)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages base)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35))

(define-public libtasn1
  (package
    (name "libtasn1")
    (version "4.19.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/libtasn1/libtasn1-"
                          version ".tar.gz"))
      (sha256
       (base32
        "0yizlr2y6gfjh86v68qw5wjcfg16arnw1f731kndd17l3jng04qn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs (list perl))
    (home-page "https://www.gnu.org/software/libtasn1/")
    (synopsis "ASN.1 library")
    (description
     "GNU libtasn1 is a library implementing the ASN.1 notation.  It is used
for transmitting machine-neutral encodings of data objects in computer
networking, allowing for formal validation of data according to some
specifications.")
    (license license:lgpl2.0+)))

(define-public asn1c
  (package
    (name "asn1c")
    (version "0.9.28")
    (source (origin
      (method url-fetch)
      (uri (string-append "https://lionet.info/soft/asn1c-"
                          version ".tar.gz"))
      (sha256
       (base32
        "1fc64g45ykmv73kdndr4zdm4wxhimhrir4rxnygxvwkych5l81w0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (home-page "https://lionet.info/asn1c")
    (synopsis "ASN.1 to C compiler")
    (description "The ASN.1 to C compiler takes ASN.1 module
files and generates C++ compatible C source code.  That code can be
used to serialize the native C structures into compact and unambiguous
BER/XER/PER-based data files, and deserialize the files back.

Various ASN.1 based formats are widely used in the industry, such as to encode
the X.509 certificates employed in the HTTPS handshake, to exchange control
data between mobile phones and cellular networks, to car-to-car communication
in intelligent transportation networks.")
    (license license:bsd-2)))

(define-public p11-kit
  (package
    (name "p11-kit")
    (version "0.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/p11-glue/p11-kit/releases/"
                           "download/" version "/p11-kit-" version ".tar.xz"))
       (sha256
        (base32 "1y5fm9gwhkh902r26p90qf1g2h1ziqrk4hgf9i9sxm2wzlz7ignq"))))
    (build-system gnu-build-system)
    (native-inputs
     (append (list pkg-config)
             (if (target-hurd?)
                 (list autoconf automake gettext-minimal libtool)
                 '())))
    (inputs
     (append (list libffi libtasn1)
             (if (target-hurd?)
                 (list libbsd)
                 '())))
    (arguments
     (list #:configure-flags
           ;; Use the default certificates so that users such as flatpak
           ;; find them.  See <https://issues.guix.gnu.org/49957>.
           #~'("--with-trust-paths=/etc/ssl/certs/ca-certificates.crt")
           #:phases #~(modify-phases %standard-phases
                        #$@(if (target-hurd?)
                               #~((add-after 'unpack 'apply-hurd-patch
                                    (lambda* (#:key inputs #:allow-other-keys)
                                      (define patch
                                        #$(local-file
                                           (search-patch "p11-kit-hurd.patch")))
                                      (invoke "patch" "-p1" "--batch" "-i"
                                              patch)))
                                  (replace 'bootstrap
                                    (lambda _
                                      (invoke "autoreconf" "-fiv"))))
                               #~())
                        (add-before 'check 'prepare-tests
                          (lambda _
                            ;; "test-runtime" expects XDG_RUNTIME_DIR to be set up
                            ;; and looks for .cache and other directories (only).
                            ;; For simplicity just drop it since it is irrelevant
                            ;; in the build container.
                            (substitute* "Makefile"
                              (("test-runtime\\$\\(EXEEXT\\)") "")))))))
    (home-page "https://p11-glue.github.io/p11-glue/p11-kit.html")
    (synopsis "PKCS#11 library")
    (description
     "p11-kit provides a way to load and enumerate PKCS#11 modules.  It
provides a standard configuration setup for installing PKCS#11 modules
in such a way that they are discoverable.  It also solves problems with
coordinating the use of PKCS#11 by different components or libraries
living in the same process.")
    (license license:bsd-3)))

(define-public gnutls
  (package
    (name "gnutls")
    (version "3.8.3")
    (source (origin
              (method url-fetch)
              ;; Note: Releases are no longer on ftp.gnu.org since the
              ;; schism (after version 3.1.5).
              (uri (string-append "mirror://gnupg/gnutls/v"
                                  (version-major+minor version)
                                  "/gnutls-" version ".tar.xz"))
              (patches (search-patches "gnutls-skip-trust-store-test.patch"))
              (sha256
               (base32
                "0ghpyhhfa3nsraph6dws50jb3dc8g2cfl7dizdnyrm179fawakzp"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? (not (or (%current-target-system)
                             (target-hurd?)))
           ;; Ensure we don't keep a reference to the tools used for testing.
           #:disallowed-references (if (target-hurd?)
                                       '()
                                       (list net-tools iproute socat))
           #:configure-flags
           #~(cons*
              ;; GnuTLS doesn't consult any environment variables to specify
              ;; the location of the system-wide trust store.  Instead it has a
              ;; configure-time option.  Unless specified, its configure script
              ;; attempts to auto-detect the location by looking for common
              ;; places in the file system, none of which are present in our
              ;; chroot build environment.  If not found, then no default trust
              ;; store is used, so each program has to provide its own
              ;; fallback, and users have to configure each program
              ;; independently.  This seems suboptimal.
              "--with-default-trust-store-dir=/etc/ssl/certs"

              (let ((system #$(or (%current-target-system)
                                  (%current-system))))
                (if (string-prefix? "mips64el" system)
                    (list
                     ;; FIXME: Temporarily disable p11-kit support since it is
                     ;; not working on mips64el.
                     "--without-p11-kit")
                    '())))

           #:phases
           #~(modify-phases %standard-phases
               ;; fastopen.sh fails to connect to the server in the builder
               ;; environment (see:
               ;; https://gitlab.com/gnutls/gnutls/-/issues/1095).
               (add-after 'unpack 'disable-failing-tests
                 (lambda _
                   (substitute* "tests/fastopen.sh"
                     (("^unset RETCODE")
                      "exit 77\n"))))   ;skip
               #$@(if (target-ppc32?)
                      ;; https://gitlab.com/gnutls/gnutls/-/issues/1354
                      ;; Extend the test timeout from the default of 20 * 1000
                      #~((add-after 'unpack 'increase-test-timeout
                           (lambda _
                             (setenv "GNUTLS_TEST_TIMEOUT" "60000"))))
                      #~())
               (add-after 'install 'move-doc
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Copy the 4.1 MiB of section 3 man pages to "doc".
                   (let* ((out    (assoc-ref outputs "out"))
                          (doc    (assoc-ref outputs "doc"))
                          (mandir (string-append doc "/share/man/man3"))
                          (oldman (string-append out "/share/man/man3")))
                     (mkdir-p mandir)
                     (copy-recursively oldman mandir)
                     (delete-file-recursively oldman)))))))
    (outputs '("out"                    ;4.4 MiB
               "debug"
               "doc"))                  ;4.1 MiB of man pages
    (native-inputs
     (append (list pkg-config texinfo which
                   util-linux)          ;one test needs 'setsid'
             (if (target-hurd?)
                 '()
                 (list net-tools
                       iproute          ;for 'ss'
                       socat            ;several tests rely on it
                       datefudge))))    ;tests rely on 'datefudge'
    (inputs (list libunistring))
    (propagated-inputs
     ;; These are all in the 'Requires.private' field of gnutls.pc.
     (append (list libtasn1 libidn2 nettle zlib)
             (let ((system (or (%current-target-system)
                               (%current-system))))
               (if (string-prefix? "mips64el" system)
                   '()
                   (list p11-kit)))))
    (home-page "https://gnutls.org")
    (synopsis "Transport layer security library")
    (description
     "GnuTLS is a secure communications library implementing the SSL, TLS
and DTLS protocols.  It is provided in the form of a C library to support the
protocols, as well as to parse and write X.509, PKCS #12, OpenPGP and other
required structures.")
    (license license:lgpl2.1+)
    (properties
     ;; Since gnutls.org doesn't have a page with a direct link to the
     ;; tarball, defer to fellow LFS hackers.
     '((release-monitoring-url
        . "https://www.linuxfromscratch.org/blfs/view/svn/postlfs/gnutls.html")
       (upstream-name . "gnutls")))))

(define-deprecated/public-alias gnutls-latest gnutls)

(define-public gnutls/dane
  ;; GnuTLS with build libgnutls-dane, implementing DNS-based
  ;; Authentication of Named Entities.  This is required for GNS functionality
  ;; by GNUnet and gnURL.  This is done in an extra package definition
  ;; to have the choice between GnuTLS with Dane and without Dane.
  (package/inherit gnutls
    (name "gnutls-dane")
    (inputs (modify-inputs (package-inputs gnutls)
              (prepend unbound)))))

(define-public guile-gnutls
  (package
    ;; This package supersedes the Guile bindings that came with GnuTLS until
    ;; version 3.7.8 included.
    (name "guile-gnutls")
    (version "4.0.0")
    (home-page "https://gitlab.com/gnutls/guile/")
    (source (origin
              ;; url-fetch is used here to avoid a circular dependency with
              ;; git-download, see https://issues.guix.gnu.org/63331
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.com/gnutls/guile/uploads/"
                    "9060bc55069cedb40ab46cea49b439c0"
                    "/guile-gnutls-" version ".tar.gz"))
              (sha256
               (base32
                "0fdjmy9vfjwk2v616nan1zz6iy9i086vrh5mdcsfqxi00ckbjk2v"))))
    (build-system gnu-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:configure-flags
      ;; Tell the build system that we want Guile bindings installed to the
      ;; output instead of Guiles own module directory.
      #~(list "--disable-static"
              (string-append "--with-guile-site-dir="
                             "$(datarootdir)/guile/site/$(GUILE_EFFECTIVE_VERSION)")
              (string-append "--with-guile-site-ccache-dir="
                             "$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache")
              (string-append "--with-guile-extension-dir="
                             "$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/extensions"))

      ;; The 'gnutls' package currently lacks support for SRP, making this
      ;; test fail.
      #:make-flags #~'("XFAIL_TESTS=tests/srp-base64.scm")))
    (native-inputs
     (list libtool
           pkg-config
           texinfo
           gnutls
           guile-3.0))            ;XXX: 'guile-snarf' invokes the native 'cpp'
    (inputs
     (list gnutls
           guile-3.0))
    (properties '((release-tag-prefix . "v")
                  (release-tag-version-delimiter . ".")))
    (synopsis "Guile bindings to GnuTLS")
    (description
     "This package provides Guile bindings to GnuTLS, a library implementation
the @acronym{TLS, Transport-Layer Security} protocol.  It supersedes the Guile
bindings that were formerly provided as part of GnuTLS.")
    (license license:lgpl2.1+)))

(define-public guile2.2-gnutls
  (package/inherit guile-gnutls
    (name "guile2.2-gnutls")
    (native-inputs
     (modify-inputs (package-native-inputs guile-gnutls)
       (replace "guile" guile-2.2)))
    (inputs
     (modify-inputs (package-inputs guile-gnutls)
       (replace "guile" guile-2.2)))))

(define (target->openssl-target pkg target)
  "Return the value to set CONFIGURE_TARGET_ARCH to when cross-compiling
OpenSSL for TARGET."
  ;; Keep this code outside the build code,
  ;; such that new targets can be added
  ;; without causing rebuilds for other targets.
  (if (target-mingw? target)
      (string-append
       "mingw"
       (if (target-x86-64? target)
           "64"
           ""))
      (let ((kernel
             (cond ((target-hurd? target)
                    "hurd")
                   ((and (target-linux? target)
                         (or (target-riscv64? target)
                             (target-loongarch64? target)))
                    "linux64")
                   ((target-linux? target)
                    "linux")
                   (else
                    (raise (condition
                            (&package-unsupported-target-error
                             (package pkg)
                             (target target)))))))
            (arch
             (cond
              ((target-x86-32? target)
               "x86")
              ((target-x32? target)
               "x32")
              ((target-x86-64? target)
               "x86_64")
              ((target-mips64el? target)
               "mips64")
              ((target-arm32? target)
               "armv4")
              ((target-aarch64? target)
               "aarch64")
              ((target-ppc64le? target)
               "ppc64le")
              ((target-ppc32? target)
               "ppc")
              ((and (target-powerpc? target)
                    (target-64bit? target))
               "ppc64")
              ((target-loongarch64? target)
               "loongarch64")
              ((target-riscv64? target)
               "riscv64")
              ((target-64bit? target)
               "generic64")
              (else
               (raise (condition
                       (&package-unsupported-target-error
                        (package pkg)
                        (target target))))))))
        (string-append kernel "-" arch))))

(define-public openssl-1.1
  (package
    (name "openssl")
    (version "1.1.1u")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://www.openssl.org/source/openssl-"
                                        version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/"
                                        "openssl-" version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/old/"
                                        (string-trim-right version char-set:letter)
                                        "/openssl-" version ".tar.gz")))
              (patches (search-patches "openssl-1.1-c-rehash-in.patch"))
              (sha256
               (base32
                "1ipbcdlqyxbj5lagasrq2p6gn0036wq6hqp7gdnd1v1ya95xiy72"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"        ;6.8 MiB of man3 pages and full HTML documentation
               "static"))   ;6.4 MiB of .a files
    (native-inputs (list perl))
    (arguments
     (list
      #:parallel-tests? #f
      #:make-flags
      ;; 'test_ssl_new.t' in 1.1.1n and 3.0.3 fails due to an expired
      ;; certificate: <https://github.com/openssl/openssl/issues/18441>.  Skip
      ;; it.
      #~(list #$@(if (or (target-arm?) (target-riscv64?))
                     ;; 'test_afalg' seems to be dependent on kernel features:
                     ;; <https://github.com/openssl/openssl/issues/12242>.
                     #~("TESTS=-test_afalg -tls_ssl_new")
                     #~("TESTS=-test_ssl_new")))
      #:test-target "test"
      ;; Changes to OpenSSL sometimes cause Perl to "sneak in" to the closure,
      ;; so we explicitly disallow it here.
      #:disallowed-references (list (canonical-package perl))
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (%current-target-system)
                 #~((add-before 'configure 'set-cross-compile
                      (lambda* (#:key target #:allow-other-keys)
                        (setenv "CROSS_COMPILE" (string-append target "-"))
                        (setenv "CONFIGURE_TARGET_ARCH"
                                #$(target->openssl-target
                                   this-package
                                   (%current-target-system))))))
                 #~())
          #$@(if (target-hurd?)
                 #~((add-after 'unpack 'patch-configure
                      (lambda _
                        (substitute* "config"
                          (("case \"\\$GUESSOS\" in.*" all)
                           (string-append all "hurd-x86*) OUT=hurd-x86;;\n"))))))
                 #~())
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; It's not a shebang so patch-source-shebangs misses it.
              (substitute* "config"
                (("/usr/bin/env")
                 (which "env")))
              (apply
               invoke #$@(if (%current-target-system)
                             #~("./Configure")
                             #~("./config"))
               "shared"                 ;build shared libraries
               "--libdir=lib"

               ;; The default for this catch-all directory is
               ;; PREFIX/ssl.  Change that to something more
               ;; conventional.
               (string-append "--openssldir=" #$output
                              "/share/openssl-"
                              #$(package-version this-package))

               (string-append "--prefix=" #$output)
               (string-append "-Wl,-rpath," (string-append #$output "/lib"))
               #$@(if (%current-target-system)
                      #~((getenv "CONFIGURE_TARGET_ARCH"))
                      #~())
               configure-flags)
              ;; Output the configure variables.
              (invoke "perl" "configdata.pm" "--dump")))
          (add-after 'install 'move-static-libraries
            (lambda _
              ;; Move static libraries to the "static" output.
              (let* ((lib    (string-append #$output "/lib"))
                     (slib   (string-append #$output:static "/lib")))
                (for-each (lambda (file)
                            (install-file file slib)
                            (delete-file file))
                          (find-files
                           lib
                           #$(if (target-mingw?)
                                 '(lambda (filename _)
                                    (and (string-suffix? ".a" filename)
                                         (not (string-suffix? ".dll.a"
                                                              filename))))
                                 "\\.a$"))))))
          (add-after 'install 'move-extra-documentation
            (lambda _
              ;; Move man pages and full HTML documentation to "doc".
              (let* ((man    (string-append #$output "/share/man"))
                     (html   (string-append #$output "/share/doc/openssl"))
                     (man-target (string-append #$output:doc "/share/man"))
                     (html-target (string-append
                                   #$output:doc "/share/doc/openssl")))
                (mkdir-p (dirname man-target))
                (mkdir-p (dirname html-target))
                (rename-file man man-target)
                (rename-file html html-target))))
          (add-after 'install 'remove-miscellany
            (lambda _
              ;; The 'misc' directory contains random undocumented shell and
              ;; Perl scripts.  Remove them to avoid retaining a reference on
              ;; Perl.
              (delete-file-recursively
               (string-append #$output "/share/openssl-"
                              #$(package-version this-package) "/misc")))))))
    (native-search-paths
     (list $SSL_CERT_DIR $SSL_CERT_FILE))
    (synopsis "SSL/TLS implementation")
    (description "OpenSSL is an implementation of SSL/TLS.")
    (license license:openssl)
    (home-page "https://www.openssl.org/")))

(define-public openssl-3.0
  (package
    (inherit openssl-1.1)
    (version "3.0.8")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "https://www.openssl.org/source/openssl-"
                                        version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/"
                                        "openssl-" version ".tar.gz")
                         (string-append "ftp://ftp.openssl.org/source/old/"
                                        (string-trim-right version char-set:letter)
                                        "/openssl-" version ".tar.gz")))
              (patches (search-patches "openssl-3.0-c-rehash-in.patch"))
              (sha256
               (base32
                "0gjb7qjl2jnzs1liz3rrccrddxbk6q3lg8z27jn1xwzx72zx44vc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments openssl-1.1)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-before 'configure 'configure-perl
              (lambda* (#:key native-inputs inputs #:allow-other-keys)
                (setenv "HASHBANGPERL"
                        (search-input-file (or native-inputs inputs)
                                           "/bin/perl"))))
            #$@(if (target-hurd?)
                   #~((delete 'patch-configure))
                   #~())
            #$@(if (target-hurd64?)
                   #~((add-after 'unpack 'apply-hurd-patch
                        (lambda _
                          (let ((patch-file
                                 #$(local-file
                                    (search-patch "openssl-hurd64.patch"))))
                            (invoke "patch" "--force" "-p1" "-i"
                                    patch-file)))))
                   #~())))
       ((#:configure-flags flags #~'())
        (if (system-hurd?)              ;must not be used when
            #~(append #$flags           ;cross-compiling!
                      #$(if (target-hurd64?)
                            #~'("hurd-x86_64")
                            #~'("hurd-x86")))
            flags))))
    (license license:asl2.0)))

(define-public openssl openssl-3.0)

(define-public bearssl
  (package
    (name "bearssl")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.bearssl.org/"
                                  "bearssl-" version ".tar.gz"))
              (sha256
               (base32
                "057zhgy9w4y8z2996r0pq5k2k39lpvmmvz4df8db8qa9f6hvn1b7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list #$(string-append "CC=" (cc-for-target))
              #$(string-append "LD=" (cc-for-target))
              #$(string-append "LDDLL=" (cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ;no configure script
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "build"
                  (invoke "./testcrypto" "all")
                  (invoke "./testx509")))))
          (replace 'install             ;no install rule
            (lambda _
              (let* ((out #$output)
                     (bin (string-append out "/bin"))
                     (doc (string-append out "/share/doc/" #$name "-" #$version))
                     (lib (string-append out "/lib"))
                     (include (string-append out "/include")))
                (install-file "build/brssl" bin)
                (for-each (lambda (f) (install-file f include))
                          (find-files "inc" "\\.h$"))
                (install-file "LICENSE.txt" doc)
                (install-file "build/libbearssl.so" lib)))))))
    (home-page "https://bearssl.org/")
    (synopsis "Small SSL/TLS library")
    (description "BearSSL is an implementation of the SSL/TLS
protocol (RFC 5246) written in C.  It aims at being correct and
secure.  In particular, insecure protocol versions and choices of
algorithms are not supported, by design; cryptographic algorithm
implementations are constant-time by default.  It should also be
small, both in RAM and code footprint.  For instance, a minimal server
implementation may fit in about 20 kilobytes of compiled code and 25
kilobytes of RAM.")
    (license license:expat)))

(define-public libressl
  (package
    (name "libressl")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://openbsd/LibreSSL/"
                                  "libressl-" version ".tar.gz"))
              (sha256
               (base32
                "1r518q11qwx9zr1niqjh4ci63x1s51gx6g8f3p3xzhxcy1aik12d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        ;; Do as if 'getentropy' were missing: Linux kernels before 3.17 lack its
        ;; underlying 'getrandom' system call and ENOSYS isn't properly handled.
        ;; See <https://lists.gnu.org/archive/html/guix-devel/2017-04/msg00235.html>.
        "ac_cv_func_getentropy=no"
        ;; FIXME It's using it's own bundled certificate, instead it should
        ;; behave like OpenSSL by using environment variables.
        (string-append "--with-openssldir=" (assoc-ref %outputs "out")
                       "/share/libressl-"
                       ,(package-version this-package))
        ;; Provide a TLS-enabled netcat.
        "--enable-nc")))
    (properties
     `((release-monitoring-url . "https://ftp.openbsd.org/pub/OpenBSD/LibreSSL/")))
    (home-page "https://www.libressl.org/")
    (synopsis "SSL/TLS implementation")
    (description "LibreSSL is a version of the TLS/crypto stack, forked from
OpenSSL in 2014 with the goals of modernizing the codebase, improving security,
and applying best practice development processes.  This package also includes a
netcat implementation that supports TLS.")
    ;; Files taken from OpenSSL keep their license, others are under various
    ;; non-copyleft licenses.
    (license (list license:openssl
                   (license:non-copyleft
                     "file://COPYING"
                     "See COPYING in the distribution.")))))

(define-public python-acme
  (package
    (name "python-acme")
    ;; Remember to update the hash of certbot when updating python-acme.
    (version "2.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "acme" version))
              (sha256
               (base32
                "1z6293g4pyxvx5w7v07j8wnaxyr7srsqfqvgly888b8k52fq9ipa"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-documentation
           (lambda _
             (invoke "make" "-C" "docs" "man" "info")))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (man (string-append out "/share/man/man1"))
                    (info (string-append out "/info")))
               (install-file "docs/_build/texinfo/acme-python.info" info)
               (install-file "docs/_build/man/acme-python.1" man))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv")))))))
    (native-inputs
     (list python-pytest
           ;; For documentation
           python-sphinx
           python-sphinxcontrib-programoutput
           python-sphinx-rtd-theme
           texinfo))
    (propagated-inputs
     (list python-chardet
           python-josepy
           python-requests
           python-requests-toolbelt
           python-pytz
           python-pyrfc3339
           python-pyasn1
           python-cryptography
           python-pyopenssl))
    (home-page "https://github.com/certbot/certbot")
    (synopsis "ACME protocol implementation in Python")
    (description "ACME protocol implementation in Python")
    (license license:asl2.0)))

(define-public certbot
  (package
    (name "certbot")
    ;; Certbot and python-acme are developed in the same repository, and their
    ;; versions should remain synchronized.
    (version (package-version python-acme))
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "certbot" version))
              (sha256
               (base32
                "12nd9nmdj3bf1xlvhj1ln473xbyv4qzxf6qhz0djbca7jl59zlwk"))))
    (build-system python-build-system)
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments python-acme)
           ((#:phases phases)
            `(modify-phases ,phases
              (replace 'install-documentation
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (man1 (string-append out "/share/man/man1"))
                         (man7 (string-append out "/share/man/man7"))
                         (info (string-append out "/info")))
                    (install-file "docs/_build/texinfo/Certbot.info" info)
                    (install-file "docs/_build/man/certbot.1" man1)
                    (install-file "docs/_build/man/certbot.7" man7)
                    #t))))))))
    (native-inputs
     (list python-mock
           python-pytest
           ;; For documentation
           python-sphinx
           python-sphinx-rtd-theme
           python-sphinx-repoze-autointerface
           python-sphinxcontrib-programoutput
           texinfo))
    (propagated-inputs
     (list python-acme
           python-cryptography
           python-pyrfc3339
           python-pyopenssl
           python-configobj
           python-configargparse
           python-distro
           python-parsedatetime
           python-psutil
           python-requests
           python-pytz))
    (synopsis "Let's Encrypt client by the Electronic Frontier Foundation")
    (description "Certbot automatically receives and installs X.509 certificates
to enable Transport Layer Security (TLS) on servers.  It interoperates with the
Let’s Encrypt certificate authority (CA), which issues browser-trusted
certificates for free.")
    (home-page "https://certbot.eff.org/")
    (license license:asl2.0)))

(define-public letsencrypt
  (package (inherit certbot)
    (name "letsencrypt")
    (properties `((superseded . ,certbot)))))

(define-public perl-net-ssleay
  (package
    (name "perl-net-ssleay")
    (version "1.92")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://cpan/authors/id/C/CH/CHRISN/"
                                  "Net-SSLeay-" version ".tar.gz"))
              (sha256
               (base32
                "1acnjd5180dca26dmjq0b9ib0dbavlrzd6fnf4nidrzj02rz5hj7"))))
    (build-system perl-build-system)
    (inputs (list openssl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'set-ssl-prefix
          (lambda* (#:key inputs #:allow-other-keys)
            (setenv "OPENSSL_PREFIX" (assoc-ref inputs "openssl"))
            #t)))))
    (synopsis "Perl extension for using OpenSSL")
    (description
     "This module offers some high level convenience functions for accessing
web pages on SSL servers (for symmetry, the same API is offered for accessing
http servers, too), an sslcat() function for writing your own clients, and
finally access to the SSL api of the SSLeay/OpenSSL package so you can write
servers or clients for more complicated applications.")
    (license license:perl-license)
    (home-page "https://metacpan.org/release/Net-SSLeay")))

(define-public perl-crypt-openssl-rsa
 (package
  (name "perl-crypt-openssl-rsa")
  (version "0.33")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/T/TO/TODDR/Crypt-OpenSSL-RSA-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "0r6qxx2nyvdsv859zl8vz17ndj1a4xvrknbafhjh6m3gdl7n7gmx"))))
  (build-system perl-build-system)
  (native-inputs
   (list perl-crypt-openssl-guess))
  (inputs
    (list perl-crypt-openssl-bignum perl-crypt-openssl-random openssl))
  (arguments perl-crypt-arguments)
  (home-page
    "https://metacpan.org/release/Crypt-OpenSSL-RSA")
  (synopsis
    "RSA encoding and decoding, using the openSSL libraries")
  (description "Crypt::OpenSSL::RSA does RSA encoding and decoding (using the
OpenSSL libraries).")
  (license license:perl-license)))

(define perl-crypt-arguments
   `(#:phases (modify-phases %standard-phases
      (add-before 'configure 'patch-Makefile.PL
        (lambda* (#:key inputs #:allow-other-keys)
          (substitute* "Makefile.PL"
            (("'LIBS'.*=>.*") (string-append "'LIBS' => ['-L"
                                             (assoc-ref inputs "openssl")
                                             "/lib -lcrypto'],")))
          #t)))))

(define-public perl-crypt-openssl-bignum
 (package
  (name "perl-crypt-openssl-bignum")
  (version "0.09")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/K/KM/KMX/Crypt-OpenSSL-Bignum-"
             version
             ".tar.gz"))
      (sha256
        (base32
          "1p22znbajq91lbk2k3yg12ig7hy5b4vy8igxwqkmbm4nhgxp4ki3"))))
  (build-system perl-build-system)
  (inputs (list openssl))
  (arguments perl-crypt-arguments)
  (home-page
    "https://metacpan.org/release/Crypt-OpenSSL-Bignum")
  (synopsis
    "OpenSSL's multiprecision integer arithmetic in Perl")
  (description "Crypt::OpenSSL::Bignum provides multiprecision integer
arithmetic in Perl.")
  ;; At your option either gpl1+ or the Artistic License
  (license license:perl-license)))

(define-public perl-crypt-openssl-guess
  (package
    (name "perl-crypt-openssl-guess")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/A/AK/AKIYM/Crypt-OpenSSL-Guess-"
             version ".tar.gz"))
       (sha256
        (base32
         "0rvi9l4ljcbhwwvspq019nfq2h2v746dk355h2nwnlmqikiihsxa"))))
    (build-system perl-build-system)
    (home-page "https://metacpan.org/release/Crypt-OpenSSL-Guess")
    (synopsis "Guess the OpenSSL include path")
    (description
     "The Crypt::OpenSSL::Guess Perl module provides helpers to guess the
correct OpenSSL include path.  It is intended for use in your
@file{Makefile.PL}.")
    (license license:perl-license)))

(define-public perl-crypt-openssl-random
 (package
  (name "perl-crypt-openssl-random")
  (version "0.15")
  (source
    (origin
      (method url-fetch)
      (uri (string-append
             "mirror://cpan/authors/id/R/RU/RURBAN/Crypt-OpenSSL-Random-"
             version
             ".tar.gz"))
      (sha256
        (base32 "1x6ffps8q7mnawmcfq740llzy7i10g3319vap0wiw4d33fm6z1zh"))))
  (build-system perl-build-system)
  (native-inputs
   (list perl-crypt-openssl-guess))
  (inputs
   (list openssl))
  (arguments perl-crypt-arguments)
  (home-page
    "https://metacpan.org/release/Crypt-OpenSSL-Random")
  (synopsis
    "OpenSSL/LibreSSL pseudo-random number generator access")
  (description "Crypt::OpenSSL::Random is a OpenSSL/LibreSSL pseudo-random
number generator")
  (license license:perl-license)))

(define-public mbedtls-lts
  (package
    (name "mbedtls")
    (version "2.28.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ARMmbed/mbedtls")
             (commit (string-append "mbedtls-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "070i5pxciw04swfqk1rmdprhsafn4cias3dlmkm467pqpjnhb394"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DUSE_SHARED_MBEDTLS_LIBRARY=ON"
                   "-DUSE_STATIC_MBEDTLS_LIBRARY=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'make-source-writable
                 (lambda _
                   (for-each make-file-writable (find-files ".")))))))
    (native-inputs
     (list perl python))
    (synopsis "Small TLS library")
    (description
     "@code{mbed TLS}, formerly known as PolarSSL, makes it trivially easy
for developers to include cryptographic and SSL/TLS capabilities in their
(embedded) products, facilitating this functionality with a minimal
coding footprint.")
    (home-page "https://www.trustedfirmware.org/projects/mbed-tls/")
    (license (list license:asl2.0 license:gpl2+)))) ;dual licensed

(define-public mbedtls
  (package
    (inherit mbedtls-lts)
    (name "mbedtls")
    (version "3.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ARMmbed/mbedtls")
                    (commit (string-append "mbedtls-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wsjrx98h74q0q4zqwsghiqvjz4aqgvigpxb9f8xjw0w4sfsclcm"))))))

(define-public mbedtls-apache
  (deprecated-package "mbedtls-apache" mbedtls-lts))

;; The Hiawatha Web server requires some specific features to be enabled.
(define-public mbedtls-for-hiawatha
  (hidden-package
   (package
     (inherit mbedtls-lts)
     (arguments
      (substitute-keyword-arguments (package-arguments mbedtls-lts)
        ((#:phases phases)
         #~(modify-phases #$phases
             (add-before 'configure 'configure-extra-features
               (lambda _
                 (for-each (lambda (feature)
                             (invoke "scripts/config.pl" "set" feature))
                           (list "MBEDTLS_THREADING_C"
                                 "MBEDTLS_THREADING_PTHREAD")))))))))))

(define-public dehydrated
  (package
    (name "dehydrated")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dehydrated-io/dehydrated")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mhf3v9ynwrxrkqawqpxnwfn5dmrlkqcvkxdrk59nkxjpdx1wkrb"))))
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils)
                  (srfi srfi-26))
      #:builder
      #~(begin
          (use-modules (guix build utils)
                       (srfi srfi-26))
          (let* ((source (assoc-ref %build-inputs "source"))
                 (gzip (search-input-file %build-inputs "bin/gzip"))
                 (bin  (string-append #$output "/bin"))
                 (doc  (string-append #$output "/share/doc/"
                                      #$name "-" #$version))
                 (man  (string-append #$output "/share/man"))
                 (bash (in-vicinity (assoc-ref %build-inputs "bash") "bin")))

            (chdir source)

            (copy-recursively "docs" doc)
            (install-file "LICENSE" doc)

            (mkdir-p man)
            (rename-file (string-append doc "/man")
                         (string-append man "/man1"))
            (for-each (cut invoke gzip "-9n" <>)
                      (find-files man ".*"))

            (install-file "dehydrated" bin)
            (with-directory-excursion bin
              (patch-shebang "dehydrated" (list bash))

              ;; Do not try to write to the store.
              (substitute* "dehydrated"
                (("SCRIPTDIR=\"\\$.*\"") "SCRIPTDIR=~/.dehydrated"))

              (setenv "PATH" bash)
              (wrap-program "dehydrated"
                `("PATH" ":" prefix
                  ,(map (lambda (file)
                          (dirname (search-input-file %build-inputs file)))
                        (list
                         ;; From check_dependencies() — keep them in sync.
                         "bin/grep"
                         "bin/diff"
                         "bin/sed"
                         "bin/awk"
                         "bin/curl"
                         "bin/cut"      ; also mktemp, head, tail
                         "bin/hexdump"
                         ;; Additional requirements.
                         "bin/openssl")))))))))
    (inputs
     (list bash
           coreutils
           curl
           diffutils
           gawk
           grep
           openssl
           sed
           util-linux+udev))
    (native-inputs
     (list gzip))
    ;; The following definition is copied from the cURL package to prevent a
    ;; cycle between the curl and tls modules.
    (native-search-paths
     (list (search-path-specification
            (variable "CURL_CA_BUNDLE")
            (file-type 'regular)
            (separator #f)
            (files '("etc/ssl/certs/ca-certificates.crt")))))
    (home-page "https://dehydrated.io/")
    (synopsis "ACME client implemented as a shell script")
    (description "Dehydrated is a client for obtaining certificates from an
ACME server (such as Let's Encrypt) implemented as a relatively simple Bash
script.")
    (license license:expat)))

(define-public go-github-com-certifi-gocertifi
  (let ((commit "a5e0173ced670013bfb649c7e806bc9529c986ec")
        (revision "1"))
    (package
      (name "go-github-com-certifi-gocertifi")
      (version (git-version "2018.01.18" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/certifi/gocertifi")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n9drccl3q1rr8wg3nf60slkf1lgsmz5ahifrglbdrc6har3rryj"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/certifi/gocertifi"))
      (synopsis "X.509 TLS root certificate bundle for Go")
      (description "This package is a Go language X.509 TLS root certificate bundle,
derived from Mozilla's collection.")
      (home-page "https://certifi.io")
      (license license:mpl2.0))))

(define-public s2n
  (package
    (name "s2n")
    ;; Update only when updating aws-crt-cpp.
    (version "1.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aws/s2n-tls")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cw8f846zvjgdwaqadnhdb0cxksx4jd9x4nan9x02xz2w5hcqw04"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_SHARED_LIBS=ON")))
    (propagated-inputs (list aws-lc))
    (supported-systems %64bit-supported-systems)
    (synopsis "SSL/TLS implementation in C99")
    (description
     "This library provides a C99 implementation of SSL/TLS.  It is designed to
be familiar to users of the widely-used POSIX I/O APIs.  It supports blocking,
non-blocking, and full-duplex I/O.  There are no locks or mutexes.

As it can be difficult to keep track of which encryption algorithms and
protocols are best to use, s2n-tls features a simple API to use the latest
default set of preferences.  Remaining on a specific version for backwards
compatibility is also supported.")
    (home-page "https://github.com/aws/s2n-tls")
    (license license:asl2.0)))

(define-public wolfssl
  (package
    (name "wolfssl")
    (version "5.7.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wolfSSL/wolfssl")
                    (commit (string-append "v" version "-stable"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q1sqwx6njsjbwmwr87ky0yrg2cin7ssa12gl14i8x1v35rpcxdb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-distro"
                   "--enable-pkcs11"
                   "--disable-examples"
                   "--enable-jobserver=no")))
    (native-inputs
     (list autoconf automake libtool))
    (synopsis "SSL/TLS implementation")
    (description "The wolfSSL embedded SSL library (formerly CyaSSL) is an
SSL/TLS library written in ANSI C and targeted for embedded, RTOS, and
resource-constrained environments - primarily because of its small size, speed,
and feature set.  wolfSSL supports industry standards up to the current TLS 1.3
and DTLS 1.2, is up to 20 times smaller than OpenSSL, and offers progressive
ciphers such as ChaCha20, Curve25519, NTRU, and Blake2b.")
    (home-page "https://www.wolfssl.com/")
    (license license:gpl2+))) ; Audit

(define-public aws-lc
  (package
    (name "aws-lc")
    ;; Update only when updating aws-crt-cpp.
    (version "1.49.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/awslabs/" name))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1403l9xdidym2gp6l9qhxcsv0bhg205p322rf45v8jysf76jsxl2"))))
    (build-system cmake-build-system)
    (native-inputs (list perl))
    (arguments
     '(#:test-target "run_minimal_tests"
       #:configure-flags
       '("-DBUILD_SHARED_LIBS=ON" "-DDISABLE_GO=ON")))
    (synopsis "General purpose cryptographic library")
    (description "AWS libcrypto (aws-lc) contains portable C implementations
of algorithms needed for TLS and common applications, and includes optimized
assembly versions for x86 and ARM.")
    (home-page "https://github.com/awslabs/aws-lc")
    (license license:asl2.0)))
