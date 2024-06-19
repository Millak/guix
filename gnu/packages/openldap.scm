;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017, 2018, 2019, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Lars-Dominik Braun <ldb@leibniz-psychology.org>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Brian Cully <bjc@spork.org>
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

(define-module (gnu packages openldap)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:select (openldap2.8 lgpl2.1+ gpl3+ psfl expat))
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public openldap
  (package
    (name "openldap")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              ;; See <http://www.openldap.org/software/download/> for a list of
              ;; mirrors.
              (uri (list (string-append
                          "http://repository.linagora.org/OpenLDAP"
                          "/openldap-release/openldap-" version ".tgz")
                         (string-append
                          "https://www.openldap.org/software/download/OpenLDAP/"
                          "openldap-release/openldap-" version ".tgz")
                         (string-append
                          "ftp://ftp.dti.ad.jp/pub/net/OpenLDAP/"
                          "openldap-release/openldap-" version ".tgz")))
              (sha256
               (base32
                "1489li52sjxm1f97v927jxaxzfk6v9sa32ixrw30qhvq07jh85ym"))))
    (build-system gnu-build-system)
    (inputs (list bdb-5.3 cyrus-sasl gnutls libgcrypt zlib))
    (native-inputs (list libtool groff bdb-5.3))
    (arguments
     (list
      #:tests? #f
      #:configure-flags
      #~(list "--disable-static"
              #$@(if (%current-target-system)
                     '("--with-yielding_select=yes"
                       "ac_cv_func_memcmp_working=yes")
                     '()))
      ;; Disable install stripping as it breaks cross-compiling.
      #:make-flags
      #~(list "STRIP=")
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (%current-target-system)
                 '((add-before 'configure 'fix-cross-gcc
                     (lambda* (#:key target #:allow-other-keys)
                       (setenv "CC" (string-append target "-gcc"))
                       (setenv "STRIP" (string-append target "-strip")))))
                 '()))))
    (synopsis "Implementation of the Lightweight Directory Access Protocol")
    (description
     "OpenLDAP is a free implementation of the Lightweight Directory Access Protocol.")
    (license openldap2.8)
    (home-page "https://www.openldap.org/")))

;; This is an incompatible fork of openldap that adds types needed for
;; liblinphone.
(define-public openldap-for-linphone
  (hidden-package
   (package
     (inherit openldap)
     (name "openldap")
     (version "2.6.4")
     (source (origin
               (method git-fetch)
               (uri (git-reference
                     (url "https://gitlab.linphone.org/BC/public/external/openldap/")
                     (commit "8a885896a3fb88098d970ab96316c0b7f18367b8")))
               (file-name (git-file-name name version))
               (sha256
                (base32
                 "1yd3cnngr5z3nymnml8fynspxgdzap7y7glp601nbkdj67wyg0k8")))))))

(define-public nss-pam-ldapd
  (package
    (name "nss-pam-ldapd")
    (version "0.9.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://arthurdejong.org/nss-pam-ldapd/"
                                  "nss-pam-ldapd-" version ".tar.gz"))
              (sha256
               (base32
                "050fzcmxmf6y15dlcffc4gxr3wkk7fliqqwhlwqzbjwk8vkn3mn6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-pam-seclib-dir="
                            (assoc-ref %outputs "out") "/lib/security/")
             ;; nslcd cannot be convinced to look at run-time for its
             ;; configuration file at a location that differs from the
             ;; configured location.
             "--with-ldap-conf-file=/etc/nslcd.conf")
       #:phases
       (modify-phases %standard-phases
         ;; This is necessary because we tell nslcd with configure flags that
         ;; it should look for its configuration file at /etc/nslcd.conf.  The
         ;; build system tries to install a default configuration to that very
         ;; location.
         (add-after 'unpack 'override-nslcd.conf-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "Makefile.in"
               (("\\$\\(DESTDIR\\)\\$\\(NSLCD_CONF_PATH\\)")
                (string-append (assoc-ref outputs "out")
                               "/etc/nslcd.conf.example"))))))))
    (inputs
     (list linux-pam openldap mit-krb5 python))
    (home-page "https://arthurdejong.org/nss-pam-ldapd")
    (synopsis "NSS and PAM modules for LDAP")
    (description "nss-pam-ldapd provides a @dfn{Name Service Switch} (NSS)
module that allows your LDAP server to provide user account, group, host name,
alias, netgroup, and basically any other information that you would normally
get from @file{/etc} flat files or NIS.  It also provides a @dfn{Pluggable
Authentication Module} (PAM) to do identity and authentication management with
an LDAP server.")
    (license lgpl2.1+)))

(define-public python-ldap
  (package
    (name "python-ldap")
    (version "3.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python-ldap" version))
       (sha256
        (base32
         "1872bvrakypb96wrsf932f3xflnbqniiyf8h58x48apgl0cwa9mb"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure-openldap-locations
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((slapd (search-input-file inputs "libexec/slapd"))
                   (schema (search-input-directory
                            inputs "etc/openldap/schema")))
               (setenv "SLAPD" slapd)
               (setenv "SCHEMA" schema)))))))
    (inputs
     (list openldap cyrus-sasl mit-krb5))
    (propagated-inputs
     (list python-pyasn1 python-pyasn1-modules))
    (home-page "https://www.python-ldap.org/")
    (synopsis "Python modules for implementing LDAP clients")
    (description
     "This package provides an object-oriented API to access LDAP directory
servers from Python programs.")
    (license psfl)))

(define-public 389-ds-base
  (package
    (name "389-ds-base")
    ;; More recent versions require rust.  That's not bad, but it's a
    ;; challenge to integrate three build systems.
    (version "2.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/389ds/389-ds-base")
                    (commit (string-append "389-ds-base-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sdvfbjfg0091f47562gw3gdc2vgvvhyhdi21lrpwnw9lqc8xdxk"))
              (modules '((guix build utils)))
              (snippet
               ;; Put '#define f_type' after '#include <sys/statvfs.h>' to
               ;; avoid name conflict.
               '(substitute* "ldap/servers/slapd/slap.h"
                  (("#include <sys/types\\.h>")
                   "#include <sys/types.h>
#include <sys/statvfs.h>")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules '((srfi srfi-1)
                  (guix build gnu-build-system)
                  ((guix build python-build-system)
                   #:select (add-installed-pythonpath python-version))
                  (guix build utils))
      #:imported-modules `((guix build python-build-system)
                           ,@%default-gnu-imported-modules)
      #:disallowed-references (list httpd)
      #:configure-flags
      #~(list "--enable-cmocka"
              (string-append "--with-db="
                             #$(this-package-input "bdb"))
              (string-append "--with-netsnmp="
                             #$(this-package-input "net-snmp"))
              (string-append "--with-selinux="
                             #$(this-package-input "libselinux"))
              "--localstatedir=/var"
              "--with-instconfigdir=/etc/dirsrv")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-references
            (lambda _
              ;; Avoid dependency on systemd-detect-virt
              (substitute* "src/lib389/lib389/instance/setup.py"
                (("container_result = subprocess.*") "container_result = 1\n")
                (("container_result.returncode") "container_result"))
              (substitute* "ldap/servers/plugins/sync/sync_persist.c"
                (("nspr4") "nspr"))
              (substitute* "src/lib389/lib389/utils.py"
                (("'/sbin/ip'")
                 (string-append "'" (which "ip") "'")))
              (substitute* "src/lib389/lib389/nss_ssl.py"
                (("'/usr/bin/certutil'")
                 (string-append "'" (which "certutil") "'"))
                (("'/usr/bin/openssl'")
                 (string-append "'" (which "openssl") "'")))))
          (add-after 'unpack 'overwrite-default-locations
            (lambda _
              (substitute* "src/lib389/lib389/paths.py"
                (("/usr/share/dirsrv/inf/defaults.inf")
                 (string-append #$output "/share/dirsrv/inf/defaults.inf")))
              ;; This directory can only be specified relative to sysconfdir.  This
              ;; is used to determine where to look for installed directory
              ;; servers, so in the absence of a search path it needs to be global.
              (substitute* "ldap/admin/src/defaults.inf.in"
                (("^initconfig_dir =.*")
                 "initconfig_dir = /etc/dirsrv/registry\n"))
              ;; This is used to determine where to write certificate files
              ;; when installing new directory server instances.
              (substitute* "src/lib389/lib389/instance/setup.py"
                (("etc_dirsrv_path = .*")
                 "etc_dirsrv_path = '/etc/dirsrv/'\n"))))
          (add-after 'unpack 'fix-install-location-of-python-tools
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((pythondir (string-append
                                #$output "/lib/python"
                                (python-version #$(this-package-input "python"))
                                "/site-packages/")))
                ;; Install directory must be on PYTHONPATH.
                (add-installed-pythonpath inputs outputs)
                ;; Install directory must exist.
                (mkdir-p pythondir)
                (setenv "INSTALL_PREFIX" #$output)
                (substitute* "Makefile.am"
                  (("setup.py install --skip-build" m)
                   (string-append
                    m " --prefix=" #$output
                    " --root=/ --single-version-externally-managed"))))))
          (add-after 'build 'build-python-tools
            (lambda* (#:key make-flags #:allow-other-keys)
              ;; Set DETERMINISTIC_BUILD to override the embedded mtime in pyc
              ;; files.
              (setenv "DETERMINISTIC_BUILD" "1")
              ;; Use deterministic hashes for strings, bytes, and datetime
              ;; objects.
              (setenv "PYTHONHASHSEED" "0")
              (apply invoke "make" "lib389" make-flags)))
          (add-after 'install 'install-python-tools
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "lib389-install" make-flags)))
          (add-after 'install-python-tools 'wrap-python-tools
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((pythonpath (getenv "GUIX_PYTHONPATH")))
                (for-each (lambda (file)
                            (wrap-program (string-append #$output file)
                              `("GUIX_PYTHONPATH" ":" prefix (,pythonpath))))
                          '("/sbin/dsconf"
                            "/sbin/dscreate"
                            "/sbin/dsctl"
                            "/sbin/dsidm"
                            "/bin/ds-logpipe.py"
                            "/bin/ds-replcheck"))))))))
    (inputs
     (list bash-minimal
           bdb
           cracklib
           cyrus-sasl
           gnutls
           icu4c
           iproute
           json-c
           libevent
           libselinux
           linux-pam
           libxcrypt
           lmdb
           mit-krb5
           net-snmp
           nspr
           nss
           (list nss "bin")             ;for certutil
           openldap
           openssl                      ;#included by net-snmp
           pcre
           python
           python-pyasn1
           python-pyasn1-modules
           python-pytest
           python-dateutil
           python-six
           python-argcomplete
           python-argparse-manpage
           python-ldap))
    (native-inputs
     (list autoconf
           automake
           cmocka
           doxygen
           gettext-minimal
           httpd
           libtool
           rsync
           pkg-config))
    (home-page "https://directory.fedoraproject.org")
    (synopsis "Enterprise-class LDAP server")
    (description "389ds is an enterprise-class LDAP server.  It is hardened by
real-world use, is full-featured, and supports multi-master replication.

Other features include:

@enumerate
@item Online, zero downtime, LDAP-based update of schema, configuration, and
  management including @dfn{Access Control Information} (ACIs);
@item Asynchronous Multi-Master Replication, to provide fault tolerance and
  high write performance;
@item Extensive documentation;
@item Secure authentication and transport (TLS, and SASL);
@item LDAPv3 compliant server.
@end enumerate\n")
    ;; GPLv3+ with OpenSSL linking exception.
    (license gpl3+)))

(define-public python-bonsai
  (package
    (name "python-bonsai")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "bonsai" version))
       (sha256
        (base32
         "013bl6h1m3f7vg1lk89d4vi28wbf31zdcs4f9g8css7ngx63v6px"))))
    (build-system python-build-system)
    (inputs
     (list mit-krb5 cyrus-sasl openldap))
    ;; disabling tests, since they require docker and extensive setup
    (arguments `(#:tests? #f))
    (home-page "https://github.com/noirello/bonsai")
    (synopsis "Access LDAP directory servers from Python")
    (description
     "This is a module for handling LDAP operations in Python.  LDAP entries
are mapped to a special Python case-insensitive dictionary, tracking the
changes of the dictionary to modify the entry on the server easily.")
    (license expat)))
