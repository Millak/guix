;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021, 2024 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2021, 2022 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages sssd)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix modules)
  #:use-module (guix utils)
  #:use-module (guix build utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages augeas)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages jose)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public adcli
  (package
    (name "adcli")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/realmd/adcli.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lg181hpv07zcdybpykjq6h4v6f5sjc60gxqbklm0wqxa8m4sakn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; The net tool is used to update the stored machine key for samba.
       (list (string-append "--with-samba-data-tool="
                            (search-input-file %build-inputs "bin/net")))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml-4.3
           docbook-xsl
           libtool
           libxslt
           util-linux                   ;For `rev` command used in tests.
           xmlto))
    (inputs (list cyrus-sasl mit-krb5 samba openldap zlib))
    (home-page "https://gitlab.freedesktop.org/realmd/adcli/")
    (synopsis "Helper library and tools for Active Directory client operations")
    (description "@command{adcli} is a command‐line tool to join a computer to
an Active Directory domain.  It can also update the machine password and
manage user, group and computer accounts for a domain.")
    (license license:lgpl2.1+)))

(define-public ding-libs
  (package
    (name "ding-libs")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SSSD/ding-libs")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17x3gj2yrjb6h7ml97xlim310x8s54n238p3ns2bj3mxifqkx0mf"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           '(list "--disable-static")))
    (native-inputs (list autoconf automake gettext-minimal libtool pkg-config))
    (home-page "https://pagure.io/SSSD/ding-libs/")
    (synopsis "Libraries for SSSD")
    (description
     "DING-LIBS (DING Is Not Glib) are a set of small, useful libraries that
the @dfn{System Security Services Daemon} (SSSD) uses and makes available to
other projects.  They include: libdhash, an implementation of a dynamic hash
table which will dynamically resize to achieve optimal storage and access time
properties; ini_config, a library for parsing and managing @code{INI} files;
path_utils, a library to manage UNIX paths and subsets of paths; collection, a
generic, hierarchical grouping mechanism for complex data sets; ref_array, a
dynamically-growing, reference-counted array; libbasicobjects, a set of
fundamental object types for C.")
    (license license:lgpl3+)))

(define-public sssd
  (package
    (name "sssd")
    (version "2.9.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SSSD/sssd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "056l6b067bc5yi3dvlv41kg1a5hl3j3fq2xll3yfwwz4phcx8qd9"))
       (patches (search-patches "sssd-system-directories.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:imported-modules (source-module-closure
                          '((guix build python-build-system)))
      #:make-flags
      #~(list (string-append "CFLAGS=-DRENEWAL_PROG_PATH=\\\""
                             #$(this-package-input "adcli") "/sbin/adcli"
                             "\\\"")
              (string-append "DOCBOOK_XSLT="
                             #$(this-package-native-input "docbook-xsl")
                             "/xml/xsl/docbook-xsl-"
                             #$(package-version (this-package-native-input "docbook-xsl"))
                             "/manpages/docbook.xsl"))
      #:configure-flags
      #~(list "--localstatedir=/var" ; for /var/lib/sss, /var/run/sssd.pid, etc.
              "--sysconfdir=/etc"    ; /etc/sssd

              "--disable-cifs-idmap-plugin"
              "--without-nfsv4-idmapd-plugin"
              (string-append "--with-plugin-path="
                             #$output "/lib/sssd")
              (string-append "--with-krb5-plugin-path="
                             #$output "/lib/krb5/plugins/libkrb5")
              (string-append "--with-cifs-plugin-path="
                             #$output "/lib/cifs-utils")
              (string-append "--with-init-dir="
                             #$output "/etc/init.d")
              (string-append "--with-ldb-lib-dir="
                             #$output "/lib/ldb/modules/ldb")
              ;; Upstream defaults to /etc/xml/catalog, and despite the "path"
              ;; name, only expects one file -- so we can't use
              ;; $XML_CATALOG_FILES, which has docbook-xml and docbook-xsl
              ;; entries.
              (string-append "--with-xml-catalog-path="
                             #$(this-package-native-input "docbook-xml")
                             "/xml/docbook/"
                             #$(package-version (this-package-native-input "docbook-xml"))
                             "/catalog.xml"))
      #:modules '((guix build gnu-build-system)
                  (guix build utils)
                  ((guix build python-build-system)
                   #:select (ensure-no-mtimes-pre-1980)))
      #:imported-modules (append %default-gnu-imported-modules
                                 %python-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'ensure-no-mtimes-pre-1980
                     ensure-no-mtimes-pre-1980)
          ;; sssd looks in lib/samba, but the Guix package puts things in lib/
          ;; Patch the path before we autoreconf.
          (add-before 'bootstrap 'patch-samba-pkgconfig
            (lambda _
              (substitute* '("src/external/samba.m4")
                (("(sambalibdir=.*/)samba" _ prefix)
                 prefix))))
          (add-after 'patch-source-shebangs 'patch-more-shebangs
            (lambda _
              (substitute* '("src/tools/analyzer/sss_analyze"
                             "src/tools/sss_obfuscate")
                (("#!/usr/bin/.*python")
                 (string-append "#!" #$(this-package-input "python") "/bin/python3")))))
          (add-before 'bootstrap 'fix-configure-macros
            (lambda _
              ;; A configure test for nsupdate realm support fails without this.
              (substitute* "src/external/nsupdate.m4"
                (("\\$NSUPDATE ") "$NSUPDATE -i "))
              ;; Let tests find softhsm lib.
              (substitute* "src/external/test_ca.m4"
                (("/usr/lib/softhsm")
                 (string-append #$(this-package-native-input "softhsm")
                                "/lib/softhsm")))))
          (add-before 'configure 'disable-failing-tests
            (lambda _
              ;; Disable tests that needs /etc/passwd.
              (substitute* "Makefile.am"
                (("pam-srv-tests") "")
                (("test-negcache") ""))
              ;; This test fails for unknown reason.
              (substitute* "src/tests/responder_socket_access-tests.c"
                (("tcase_add_test\\(tc_utils, resp_str_to_array_test\\);") ""))))
          (add-before 'configure 'disable-active-directory-tests
            ;; These tests require Active Directory running in a VM.
            (lambda _
              (substitute* "Makefile.am"
                (("ad_gpo_tests") "")
                (("ad_common_tests") ""))))
          (add-before 'check 'set-libpython-path
            (lambda _
              (setenv "LD_LIBRARY_PATH"
                      (string-append #$(this-package-input "python") "/lib"))))
          (add-after 'install 'remove-static-libs
            (lambda _
              ;; Remove a static library that produces a (harmless) warning
              ;; when starting a program that uses sssd’s LDB modules.
              (delete-file
               (string-append #$output "/lib/ldb/modules/ldb/memberof.la"))))
          (add-after 'install 'wrap-binaries
            (lambda _
              (with-directory-excursion #$output
                ;; Set path to LDB modules for sssd and utilities.
                (for-each (lambda (bin)
                            (wrap-program (string-append "sbin/" bin)
                              `("LDB_MODULES_PATH" ":" prefix
                                (,(string-append #$output "/lib/ldb/modules/ldb")))))
                          '("sssd" "sssctl" "sss_cache" "sss_override" "sss_seed"))
                ;; Set path to sssd’s site-packages for scripts.
                (for-each (lambda (script)
                            (wrap-program script
                              `("GUIX_PYTHONPATH" ":" prefix
                                (,(string-append #$output "/lib/python"
                                                 #$(version-major+minor
                                                    (package-version
                                                     (this-package-input "python")))
                                                 "/site-packages")))))
                          '("libexec/sssd/sss_analyze" "sbin/sss_obfuscate"))))))))
    (inputs
     (list adcli
           bash-minimal
           c-ares
           curl ; for OpenID Connect support
           cyrus-sasl
           dbus
           ding-libs
           glib
           gnutls
           http-parser
           `(,isc-bind "utils")
           jansson
           jose ; for OpenID Connect support
           keyutils
           ldb
           libnl
           libselinux
           libsemanage
           libunistring
           linux-pam
           mit-krb5
           nss
           openldap
           openssl
           p11-kit ; for PKCS#11 support
           pcre2
           popt
           python ; for wrap-program phase
           samba/pinned
           talloc
           tdb
           tevent))
    (native-inputs
     (list autoconf
           automake
           bc ; for tests
           check ; for tests
           cmocka ; for tests
           docbook-xml-4.5
           docbook-xsl
           doxygen
           gettext-minimal
           libfaketime ; for tests
           libtool
           libxml2 ; for xmllint
           libxslt
           openssh ; for tests
           pkg-config
           python-toolchain
           po4a
           softhsm ; for tests
           `(,util-linux "lib"))) ; for uuid.h, reqired for KCM
    (home-page "https://pagure.io/SSSD/sssd/")
    (synopsis "System security services daemon")
    (description "SSSD is a system daemon.  Its primary function is to provide
access to identity and authentication remote resource through a common
framework that can provide caching and offline support to the system.  It
provides PAM and NSS modules, and in the future will support D-BUS based
interfaces for extended user information.  It also provides a better database
to store local users as well as extended user data.")
    (license license:gpl3+)))
