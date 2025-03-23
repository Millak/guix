;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2017, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Adonay "adfeno" Felipe Nogueira <https://libreplanet.org/wiki/User:Adfeno> <adfeno@openmailbox.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Simon Streit <simon@netpanic.org>
;;; Copyright © 2024 Jordan Moore <lockbox@struct.foo>
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

(define-module (gnu packages samba)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system copy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public cifs-utils
  (package
    (name "cifs-utils")
    (version "7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.samba.org/pub/linux-cifs/"
                           "cifs-utils/cifs-utils-" version ".tar.bz2"))
       (sha256 (base32
                "0ajvbc5lbwvzlhq2f2grsgy3rpzb314vm29icp4kcsjadbvj64d2"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config
           ;; To generate the manpages.
           python-docutils)) ; rst2man
    (inputs
     (list keyutils libcap-ng linux-pam mit-krb5 samba talloc))
    (arguments
     (list #:configure-flags #~(list "--enable-man")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'trigger-bootstrap
                 ;; The shipped configure script is buggy, e.g., it contains a
                 ;; unexpanded literal ‘LIBCAP_NG_PATH’ line).
                 (lambda _
                   (delete-file "configure")))
               (add-before 'configure 'set-root-sbin
                 ;; Don't try to install into "/sbin".
                 (lambda _
                   (setenv "ROOTSBINDIR" (string-append #$output "/sbin")))))))
    (synopsis "User-space utilities for Linux CIFS (Samba) mounts")
    (description "@code{cifs-utils} is a set of user-space utilities for
mounting and managing @acronym{CIFS, Common Internet File System} shares using
the Linux kernel CIFS client.")
    (home-page "https://wiki.samba.org/index.php/LinuxCIFS_utils")
    ;; cifs-utils is licensed as GPL3 or later, but 3 files contain LGPL code.
    (license license:gpl3+)))

(define-public iniparser
  (package
    (name "iniparser")
    (version "4.1")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                    (url "https://github.com/ndevilla/iniparser")
                    (commit (string-append "v" version))))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0dhab6pad6wh816lr7r3jb6z273njlgw2vpw8kcfnmi7ijaqhnr5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("Makefile" "test/Makefile")
               (("/usr/lib")
                (string-append (assoc-ref outputs "out") "/lib")))
             #t))
         (replace 'build
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "libiniparser.so.1"
                    make-flags)))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (lib  (string-append out "/lib"))
                    (inc  (string-append out "/include"))
                    (doc  (string-append out "/share/doc/" ,name))
                    (html (string-append doc "/html")))
               (define (install dir)
                 (lambda (file)
                   (install-file file dir)))
               (for-each (install lib)
                         (find-files "." "^lib.*\\.so"))
               (with-directory-excursion lib
                 (symlink "libiniparser.so.1" "libiniparser.so"))
               (for-each (install inc)
                         (find-files "src" "\\.h$"))
               (for-each (install html)
                         (find-files "html" ".*"))
               (for-each (install doc)
                         '("AUTHORS" "INSTALL" "LICENSE" "README.md"))
               #t))))))
    (home-page "https://github.com/ndevilla/iniparser")
    (synopsis "Simple @file{.ini} configuration file parsing library")
    (description
     "The iniParser C library reads and writes Windows-style @file{.ini}
configuration files.  These are simple text files with a basic structure
composed of sections, properties, and values.  While not expressive, they
are easy to read, write, and modify.

The library is small, thread safe, and written in portable ANSI C with no
external dependencies.")
    (license license:x11)))

(define-public samba/pinned
  (hidden-package
   (package
     (name "samba")
     (version "4.18.1")
     (source
      ;; For updaters: the current PGP fingerprint is
      ;; 81F5E2832BD2545A1897B713AA99442FB680B620.
      (origin
        (method url-fetch)
        (uri (string-append "https://download.samba.org/pub/samba/stable/"
                            "samba-" version ".tar.gz"))
        (sha256
         (base32 "03ncp49pfpzjla205y3xpb9iy61dz4pryyrvyz26422a4hpsmpnf"))))
     (build-system gnu-build-system)
     (arguments
      (list
       #:make-flags #~(list "TEST_OPTIONS=--quick") ;some tests are very long
       #:phases
       #~(modify-phases %standard-phases
           (add-before 'configure 'setup-docbook-stylesheets
             (lambda* (#:key inputs #:allow-other-keys)
               ;; Append Samba's own DTDs to XML_CATALOG_FILES
               ;; (c.f. docs-xml/build/README).
               (copy-file "docs-xml/build/catalog.xml.in"
                          "docs-xml/build/catalog.xml")
               (substitute* "docs-xml/build/catalog.xml"
                 (("/@abs_top_srcdir@")
                  (string-append (getcwd) "/docs-xml")))
               ;; Honor XML_CATALOG_FILES.
               (substitute* "buildtools/wafsamba/wafsamba.py"
                 (("XML_CATALOG_FILES=\"\\$\\{SAMBA_CATALOGS\\}" all)
                  (string-append all " $XML_CATALOG_FILES")))))
           (replace 'configure
             ;; Samba uses a custom configuration script that runs WAF.
             (lambda* (#:key inputs #:allow-other-keys)
               (let* ((libdir (string-append #$output "/lib")))
                 (invoke "./configure"
                         #$@(if (member (%current-system)
                                        (package-transitive-supported-systems
                                         python-cryptography))
                                '("--enable-selftest")
                                '())
                         "--enable-fhs"
                         (string-append "--prefix=" #$output)
                         "--sysconfdir=/etc"
                         "--localstatedir=/var"
                         ;; Install public and private libraries into
                         ;; a single directory to avoid RPATH issues.
                         (string-append "--libdir=" libdir)
                         (string-append "--with-privatelibdir=" libdir)
                         "--with-system-mitkrb5"
                         (string-append "--with-system-mitkdc="
                                        (search-input-file inputs "sbin/krb5kdc"))
                         "--with-experimental-mit-ad-dc"))))
           (add-before 'install 'disable-etc,var-samba-directories-setup
             (lambda _
               (substitute* "dynconfig/wscript"
                 (("bld\\.INSTALL_DIR.*") "")))))
       ;; FIXME: The test suite seemingly hangs after failing to provision the
       ;; test environment.
       #:tests? #f))
     (inputs
      (list acl
            avahi
            cmocka
            cups
            gamin
            dbus
            gpgme
            gnutls
            jansson
            libarchive
            libtirpc
            libxcrypt
            linux-pam
            lmdb
            mit-krb5
            openldap
            perl
            python
            popt
            readline
            tdb))
     (propagated-inputs
      ;; In Requires or Requires.private of pkg-config files.
      (list ldb talloc tevent))
     (native-inputs
      (append
       (list perl-parse-yapp
             pkg-config)
       ;; The python-cryptography dependency is needed for the krb5 tests.
       ;; Since python-cryptography requires Rust, add it conditionally
       ;; depending on such support.
       (if (member (%current-system)
                   (package-transitive-supported-systems
                    python-cryptography))
           (list python-cryptography)
           '())
       (list python-dnspython
             python-iso8601
             python-markdown
             rpcsvc-proto               ;for 'rpcgen'
             python-pyasn1              ;for krb5 tests
             ;; For generating man pages.
             docbook-xml-4.2
             docbook-xsl
             libxslt)))
     (home-page "https://www.samba.org/")
     (synopsis
      "The standard Windows interoperability suite of programs for GNU and Unix")
     (description
      "Since 1992, Samba has provided secure, stable and fast file and print
services for all clients using the SMB/CIFS protocol, such as all versions of
DOS and Windows, OS/2, GNU/Linux and many others.

Samba is an important component to seamlessly integrate Linux/Unix Servers and
Desktops into Active Directory environments using the winbind daemon.")
     (license license:gpl3+))))

(define-public samba
  (package
    (inherit samba/pinned)
    (version "4.18.1")
    (source
     ;; For updaters: the current PGP fingerprint is
     ;; 81F5E2832BD2545A1897B713AA99442FB680B620.
     (origin
       (method url-fetch)
       (uri (string-append "https://download.samba.org/pub/samba/stable/"
                           "samba-" version ".tar.gz"))
       (sha256
        (base32 "03ncp49pfpzjla205y3xpb9iy61dz4pryyrvyz26422a4hpsmpnf"))))
    (properties (alist-delete 'hidden? (package-properties samba/pinned)))))

(define-public talloc
  (package
    (name "talloc")
    (version "2.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/talloc/talloc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01b5pq39z1l26f86dy8jqb37fsjbvsvx5ji65jmy8rsy4sz9x7qp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-crypt-reference
           ;; The following is needed because Python.h propagates
           ;; HAVE_CRYPT_H, which is then seen from lib/replace/ but talloc
           ;; doesn't need it at all.
           (lambda _
             (substitute* "lib/replace/replace.h"
               (("#include <crypt.h>")
                ""))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; talloc uses a custom configuration script that runs a Python
             ;; script called 'waf', and doesn't tolerate unknown options.
             (setenv "CONFIG_SHELL" (which "sh"))
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out))))))))
    (native-inputs
     (list which))
    (inputs
     (list python))
    (home-page "https://talloc.samba.org")
    (synopsis "Hierarchical, reference counted memory pool system")
    (description
     "Talloc is a hierarchical, reference counted memory pool system with
destructors.  It is the core memory allocator used in Samba.")
    (license license:gpl3+))) ;; The bundled "replace" library uses LGPL3.

(define-public talloc/static
  (package
    (inherit talloc)
    (name "talloc-static")
    (synopsis
     "Hierarchical, reference counted memory pool system (static library)")
    (arguments
     (substitute-keyword-arguments (package-arguments talloc)
       ((#:phases phases)
        ;; Since Waf, the build system talloc uses, apparently does not
        ;; support building static libraries from a ./configure flag, roll our
        ;; own build process.  No need to be ashamed, we're not the only ones
        ;; doing that:
        ;; <https://github.com/proot-me/proot-static-build/blob/master/GNUmakefile>.
        ;; :-)
        `(modify-phases ,phases
           (replace 'build
             (lambda _
               (invoke "gcc" "-c" "-Ibin/default" "-I" "lib/replace"
                       "-I." "-Wall" "-g" "-D__STDC_WANT_LIB_EXT1__=1"
                       "talloc.c")
               (invoke "ar" "rc" "libtalloc.a" "talloc.o")))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out     (assoc-ref outputs "out"))
                      (lib     (string-append out "/lib"))
                      (include (string-append out "/include")))
                 (mkdir-p lib)
                 (install-file "libtalloc.a" lib)
                 (install-file "talloc.h" include)
                 #t)))
           (delete 'check)))))))            ;XXX: tests rely on Python modules

(define-public tevent
  (package
    (name "tevent")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/tevent/tevent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "030x6ziapxiqvmi2m23ri2p9rsa202gfqr7b3cv48lx5gy8plhxr"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-crypt-reference
           ;; The following is needed because Python.h propagates
           ;; HAVE_CRYPT_H, which is then seen from lib/replace/ but talloc
           ;; doesn't need it at all.
           (lambda _
             (substitute* "lib/replace/replace.h"
               (("#include <crypt.h>")
                ""))))
         (replace 'configure
           ;; tevent uses a custom configuration script that runs waf.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       "--bundled-libraries=NONE")))))))
    (native-inputs
     (list cmocka pkg-config python which))
    (propagated-inputs
     (list talloc)) ; required by tevent.pc
    (synopsis "Event system library")
    (home-page "https://tevent.samba.org/")
    (description
     "Tevent is an event system based on the talloc memory management library.
It is the core event system used in Samba.  The low level tevent has support for
many event types, including timers, signals, and the classic file descriptor events.")
    (license license:lgpl3+)))

(define-public ldb
  (package
    (name "ldb")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.samba.org/ftp/ldb/ldb-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1j9n2yzhd35xjh0mdfgym58xfbma1d27bcavjv1q4rzqgpvh6x26"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each (lambda (file)
                              ;; Delete everything except the build tools.
                              (unless (or (string-prefix? "third_party/waf" file)
                                          (string-suffix? "wscript" file))
                                (delete-file file)))
                            (find-files "third_party"))))))
    (build-system gnu-build-system)
    (arguments
     '(;; LMDB is only supported on 64-bit systems, yet the test suite
       ;; requires it.
       #:tests? (assoc-ref %build-inputs "lmdb")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-crypt-reference
           ;; The following is needed because Python.h propagates
           ;; HAVE_CRYPT_H, which is then seen from lib/replace/ but talloc
           ;; doesn't need it at all.
           (lambda _
             (substitute* "lib/replace/replace.h"
               (("#include <crypt.h>")
                ""))))
         (replace 'configure
           ;; ldb use a custom configuration script that runs waf.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (invoke "./configure"
                       (string-append "--prefix=" out)
                       (string-append "--with-modulesdir=" out
                                      "/lib/ldb/modules")
                       "--bundled-libraries=NONE")))))))
    (native-inputs
     (list cmocka pkg-config python which))
    (propagated-inputs
     ;; ldb.pc refers to all these.
     (list talloc tdb))
    (inputs
     `(,@(if (target-64bit?)
             `(("lmdb" ,lmdb))
             '())
       ("popt" ,popt)
       ("tevent" ,tevent)))
    (synopsis "LDAP-like embedded database")
    (home-page "https://ldb.samba.org/")
    (description
     "Ldb is a LDAP-like embedded database built on top of TDB.  What ldb does
is provide a fast database with an LDAP-like API designed to be used within an
application.  In some ways it can be seen as a intermediate solution between
key-value pair databases and a real LDAP database.")
    (license license:lgpl3+)))

(define-public ppp
  (package
    (name "ppp")
    (version "2.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ppp-project/ppp")
                    (commit (string-append "ppp-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pkvaxi5318lvsadspj603wdkxah01vgjhywri7rv249p249vfr7"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no tests
           #:configure-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "--with-openssl="
                                  (assoc-ref %build-inputs "openssl")))
           #:make-flags #~(list "V=1")  ; better build logs
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'bootstrap 'no-/bin/sh
                 (lambda _ (delete-file "autogen.sh"))))))
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list libpcap libxcrypt openssl))
    (synopsis "Implementation of the Point-to-Point Protocol")
    (home-page "https://ppp.samba.org/")
    (description
     "The Point-to-Point Protocol (PPP) provides a standard way to establish
a network connection over a serial link.  At present, this package supports IP
and IPV6 and the protocols layered above them, such as TCP and UDP.")
    ;; pppd, pppstats and pppdump are under BSD-style notices.
    ;; some of the pppd plugins are GPL'd.
    ;; chat is public domain.
    (license (list license:bsd-3
                   license:bsd-4
                   license:gpl2+
                   license:public-domain))))

(define-public ppp-2.4.9
  (package
    (inherit ppp)
    (name "ppp")
    (version "2.4.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ppp-project/ppp")
                    (commit (string-append "ppp-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bhhksdclsnkw54a517ndrw55q5zljjbh9pcqz1z4a2z2flxpsgk"))))
         (arguments
    (list #:tests? #f                    ;; No "check" target
          #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
          #:phases
          #~(modify-phases %standard-phases
              (add-before 'configure 'patch-Makefile
                (lambda* (#:key inputs #:allow-other-keys)
                  (let ((openssl (assoc-ref inputs "openssl"))
                        (libpcap (assoc-ref inputs "libpcap")))
                    (substitute* "pppd/Makefile.linux"
                      (("/usr/include/openssl")
                       (string-append openssl "/include"))
                      (("-DPPP_FILTER")
                       (string-append "-DPPP_FILTER -I" libpcap "/include")))
                    (substitute* "pppd/pppcrypt.h"
                      (("des\\.h") "openssl/des.h")))
                  #t)))))))

(define-public wsdd
  (package
    (name "wsdd")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/christgau/wsdd")
                           (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16kk7x80jlargrvh643m23j277p0drs2yylqz54f9inf5ld5bxn5"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ; no test suite, only examples
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)           ; no configure script
          (delete 'build)               ; nothing to build
          (replace 'install
            (lambda _
              (with-directory-excursion "src"
                (rename-file "wsdd.py" "wsdd")
                (install-file "wsdd" (string-append #$output "/bin")))
              (for-each
               (lambda (file)
                 (install-file file
                               (string-append #$output "/share/man/man"
                                              (string-take-right file 1))))
               (find-files "man" "\\.[0-9]$")))))))
    (inputs
     (list python))
    (home-page "https://github.com/christgau/wsdd")
    (synopsis "Web Service Discovery host daemon")
    (description "This daemon allows (Samba) hosts to be found by Web
Service Discovery Clients.  It also implements the client side of the
discovery protocol which searches for devices implementing
WSD.")
    (license license:expat)))
