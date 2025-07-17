;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2016, 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Jeff Mickey <j@codemac.net>
;;; Copyright © 2016, 2017, 2019, 2021, 2022, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016–2022, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020 Ivan Kozlov <kanichos@yandex.ru>
;;; Copyright © 2020 David Dashyan <mail@davie.li>
;;; Copyright © 2021 Domagoj Stolfa <ds815@gmx.com>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2022 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2022 Lu hui <luhux76@gmail.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Jean-Pierre De Jesus DIAZ <me@jeandudey.tech>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2024 Allan Adair <allan@adair.no>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages vpn)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml))

(define-public bitmask
  (package
    (name "bitmask")
    (version "0.21.11")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://0xacab.org/leap/bitmask-vpn")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zphigfrks1j3snbc748b3mk0qb1r7n2v7p7l6w1xiiil4dql6cs"))
       (modules
        '((guix build utils)))
       (snippet
        `(begin
           (delete-file-recursively "branding/thirdparty")
           (call-with-output-file "pkg/config/version/version.go"
             (lambda (port)
               (format port "package version\n")
               (format port "\n")
               (format port (string-append "var VERSION = \"" ,version "\""))))
           #t))))
    (build-system go-build-system)
    (arguments
     `(#:imported-modules
       ((guix build cmake-build-system)
        (guix build copy-build-system)
        (guix build python-build-system)
        (guix build qt-build-system)
        (guix build qt-utils)
        ,@%go-build-system-modules)
       #:modules
       (((guix build copy-build-system)
         #:prefix copy:)
        ((guix build python-build-system)
         #:prefix python:)
        ((guix build qt-build-system)
         #:prefix qt:)
        (guix build utils)
        (guix build go-build-system))
       #:unpack-path "0xacab.org/leap/bitmask-vpn"
       #:import-path "0xacab.org/leap/bitmask-vpn/cmd/bitmask-helper"
       #:phases
       (modify-phases %standard-phases
         (add-after 'setup-go-environment 'insert-missing-sources
           ;; For some reason this package is left out.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((name "go-0xacab-org-leap-shapeshifter")
                    (shapeshifter (assoc-ref inputs name))
                    (shapeshifter-src (string-append shapeshifter "/src")))
               (copy-recursively shapeshifter-src "src"))))
         (add-after 'unpack 'patch
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (with-directory-excursion "src/0xacab.org/leap/bitmask-vpn"
               ;; Use 'emersion/go-autostart',
               ;; instead of 'ProtonMail/go-autostart',
               ;; as the latter no longer exists.
               (substitute* (find-files "." "\\.go$")
                 (("github.com/ProtonMail/go-autostart")
                  "github.com/emersion/go-autostart"))
               ;; Use correct paths for referenced items.
               (let* ((out (assoc-ref outputs "out"))
                      (policy-dir (string-append out "/share/polkit-1/actions"))
                      (policy-file "se.leap.bitmask.policy")
                      (policy-path (string-append policy-dir "/" policy-file))
                      (ip (string-append (assoc-ref inputs "iproute")
                                         "/sbin/ip"))
                      (iptables (string-append (assoc-ref inputs "iptables")
                                               "/sbin/iptables"))
                      (ip6tables (string-append (assoc-ref inputs "iptables")
                                                "/sbin/ip6tables"))
                      (sysctl (string-append (assoc-ref inputs "procps")
                                             "/sbin/sysctl"))
                      (pkttyagent (string-append (assoc-ref inputs "polkit")
                                                 "/bin/pkttyagent"))
                      (openvpn (string-append (assoc-ref inputs "openvpn")
                                              "/sbin/openvpn"))
                      (bitmask-root (string-append (assoc-ref outputs "out")
                                                   "/sbin/bitmask-root")))
                 (substitute* (find-files "." "(\\.go$|\\.policy$|bitmask-root)")
                   (("swhich\\(\"ip\"\\)")
                    (string-append "\"" ip "\""))
                   (("swhich\\(\"iptables\"\\)")
                    (string-append "\"" iptables "\""))
                   (("swhich\\(\"ip6tables\"\\)")
                    (string-append "\"" ip6tables "\""))
                   (("swhich\\(\"sysctl\"\\)")
                    (string-append "\"" sysctl "\""))
                   (("/usr/(bin|lib|libexec)/.*(kit|agent|agent-1)") pkttyagent)
                   (("/usr/sbin/openvpn") openvpn)
                   (("/usr/sbin/bitmask-root") bitmask-root)
                   (("/usr/local/sbin/bitmask-root") bitmask-root)
                   (("/usr/share.*\\.policy") policy-path)))
               (substitute* (find-files "." "\\.pro$")
                 ;; Use correct path for goshim files,
                 ;; which are generated in 'build-continued phase.
                 (("-L.*/lib") "-L./lib")
                 ;; FIXME: Unable to build i18n files.
                 (("TRANSLATIONS.*i18n.*$") "")
                 (("RESOURCES.*i18n.*$") "")))))
         (add-after 'build 'build-continued
           (lambda _
             ;; Generate goshim library and header files.
             (let* ((dir "src/0xacab.org/leap/bitmask-vpn")
                    (source (string-append dir "/gui/backend.go"))
                    (target (string-append dir "/lib/libgoshim.a")))
               (mkdir-p (string-append dir "/lib"))
               (invoke "go" "build" "-buildmode=c-archive" "-o" target source))
             ;; Build bitmask application.
             (with-directory-excursion "src/0xacab.org/leap/bitmask-vpn"
               (delete-file "Makefile")
               (invoke "qmake" "bitmask.pro")
               (invoke "make"))))
         (add-after 'check 'check-continued
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Run bitmask test-suite.
               (with-directory-excursion "src/0xacab.org/leap/bitmask-vpn"
                 (delete-file "Makefile")
                 (invoke "qmake" "test.pro")
                 ;; Tests require display-server.
                 (setenv "QT_QPA_PLATFORM" "offscreen")
                 ;; Tests look for $XDG_RUNTIME_DIR.
                 (setenv "XDG_RUNTIME_DIR" (getenv "TEMP"))
                 ;; Tests write to $HOME.
                 (setenv "HOME" (getenv "TEMP"))
                 (invoke "make" "check")))))
         (add-after 'install 'install-continued
           (lambda args
             (apply (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    ;; Install bitmask program.
                    '(("src/0xacab.org/leap/bitmask-vpn/release"
                       "bin"
                       #:include ("bitmask"))
                      ;; Install bitmask-root script.
                      ("src/0xacab.org/leap/bitmask-vpn/helpers"
                       "sbin"
                       #:include ("bitmask-root"))
                      ;; Install polkit-policy.
                      ("src/0xacab.org/leap/bitmask-vpn/helpers"
                       "share/polkit-1/actions"
                       #:include ("se.leap.bitmask.policy")))
                    args)))
         (add-after 'install-continued 'post-install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bitmask (string-append out "/bin/bitmask"))
                    (bitmask-root (string-append out "/sbin/bitmask-root")))
               ;; Make bitmask-root script executable.
               (chmod bitmask-root #o777))))
         (add-after 'post-install 'python-wrap
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'python-wrap 'qt-wrap
           (assoc-ref qt:%standard-phases 'qt-wrap)))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("iproute" ,iproute)
       ("iptables" ,iptables)
       ("mesa" ,mesa)
       ("openvpn" ,openvpn)
       ("polkit" ,polkit)
       ("procps" ,procps)
       ("python" ,python)
       ("qtbase" ,qtbase-5)
       ("qtdeclarative-5" ,qtdeclarative-5)
       ("qtgraphicaleffects" ,qtgraphicaleffects)
       ("qtquickcontrols-5" ,qtquickcontrols-5)
       ("qtquickcontrols2-5" ,qtquickcontrols2-5)
       ("qtsvg-5" ,qtsvg-5)))
    (propagated-inputs
     (list go-0xacab-org-leap-shapeshifter
           go-github-com-apparentlymart-go-openvpn-mgmt
           go-github-com-emersion-go-autostart
           go-github-com-keybase-go-ps
           go-github-com-rakyll-statik
           go-github-com-sevlyar-go-daemon
           go-golang-org-x-sys))
    (synopsis "Generic VPN client by LEAP")
    (description "Bitmask, by @acronym{LEAP, LEAP Encryption Access Project},
is an application to provide easy and secure encrypted communication with a
@acronym{VPN, Virtual Private Network}.  It allows you to select from a variety
of trusted service provider all from one app.  Current providers include Riseup
Networks and The Calyx Institute, where the former is default.")
    (home-page "https://bitmask.net/")
    (license license:gpl3+)))

(define-public gp-saml-gui
  ;; No release.
  (let ((commit "258f47cdc4a8ed57a1eef16667f6cad0d1cb49b1")
        (revision "1"))
    (package
      (name "gp-saml-gui")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/dlenski/gp-saml-gui")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qj2mmi6lfkq5c4v6fbzgriajqc27k9kb1i9k2r776pn5pq14pc3"))))
      (build-system python-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'install 'wrap-program
              (lambda _
                (let ((prog (string-append #$output "/bin/gp-saml-gui")))
                  (wrap-program prog
                    `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                    `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
      (inputs
       (list bash-minimal
             python-pygobject
             python-urllib3
             python-requests
             webkitgtk-with-libsoup2))
      (propagated-inputs
       (list openconnect))
      (synopsis "Interactively authenticate to GlobalProtect VPNs that require SAML")
      (description "This is a helper script to allow you to interactively login
to a GlobalProtect VPN that uses SAML authentication, so that you can
subsequently connect with OpenConnect.")
      (license license:gpl3+))))

(define-public gvpe
  (package
    (name "gvpe")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gvpe/gvpe-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cz8n75ksl0l908zc5l3rnfm1hv7130s2w8710799fr5sxrdbszi"))))
    (build-system gnu-build-system)
    (home-page "http://software.schmorp.de/pkg/gvpe.html")
    (native-inputs (list pkg-config))
    (inputs (list openssl zlib))
    (synopsis "Secure VPN among multiple nodes over an untrusted network")
    (description
     "The GNU Virtual Private Ethernet creates a virtual network
with multiple nodes using a variety of transport protocols.  It works
by creating encrypted host-to-host tunnels between multiple
endpoints.")
    (license license:gpl3+)))

(define-public n2n
  (package
    (name "n2n")
    (version "2.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ntop/n2n")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ph2npvnqh1xnmkp96pdzpxm033jkb8zznd3nc59l9arhn0pq4nv"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'bootstrap 'move-configure
            ;; Don't execute configure script in bootstrap.
            (lambda _
              (substitute* "autogen.sh"
                (("./configure") ""))))
          (add-before 'configure 'fix-configure
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (substitute* "configure"
                (("/bin/sh")
                 (search-input-file (or native-inputs inputs) "/bin/sh"))))))
      #:tests? #f))                     ;there is no check target
    (native-inputs
     (list autoconf automake bash-minimal pkg-config))
    (home-page "https://github.com/ntop/n2n")
    (synopsis "Peer-to-peer VPN client and server")
    (description
     "n2n is a light VPN software which makes it easy to create virtual
networks bypassing intermediate firewalls.")
    (license license:gpl3+)))

(define-public strongswan
  (package
    (name "strongswan")
    (version "5.9.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.strongswan.org/strongswan-"
                           version ".tar.bz2"))
       (sha256
        (base32 "0y1nqd7vb4s6wzvyrbmxpbglw9wcvcypvjffqiklrcscvbfjg03j"))
       (snippet
        #~(begin
            (use-modules (guix build utils))
            (with-directory-excursion "src"
              (for-each delete-file
                      '("starter/parser/lexer.c"
                        "libstrongswan/settings/settings_lexer.c"
                        "starter/parser/parser.c"
                        "starter/parser/parser.h"
                        "libstrongswan/settings/settings_parser.c"
                        "libstrongswan/settings/settings_parser.h"
                        "libstrongswan/plugins/bliss/bliss_huffman_code_1.c"
                        "libstrongswan/plugins/bliss/bliss_huffman_code_3.c"
                        "libstrongswan/plugins/bliss/bliss_huffman_code_4.c"
                        "libstrongswan/asn1/oid.c"
                        "libstrongswan/asn1/oid.h")))))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-command-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/ipsec/_ipsec.in"
               (("cat|kill|sleep|rm|uname" command)
                (string-append (assoc-ref inputs "coreutils")
                               "/bin/" command)))
             (substitute* "src/libstrongswan/utils/process.c"
               (("/bin/sh")
                (search-input-file inputs "/bin/sh")))

             (substitute* "src/libstrongswan/tests/suites/test_process.c"
               (("/bin/sh") (which "sh"))
               (("/bin/echo") (which "echo"))
               (("cat") (which "cat")))))
         (add-before 'check 'set-up-test-environment
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (search-input-directory inputs "share/zoneinfo"))
             ;; Speed-up the test suite on some of the architectures.
             ,@(if (not (target-x86-64?))
                   `((setenv "TESTS_SUITES_EXCLUDE" "rsa"))
                   '()))))
       #:configure-flags
       (list
        "--disable-ldap"
        "--disable-mysql"
        "--disable-systemd"

        ;; Disable BSD-4 licensed plugins.
        "--disable-blowfish"
        "--disable-des"

        ;; Make it usable.  The default configuration is far too minimal to be
        ;; used with most common VPN set-ups.
        ;; See <https://wiki.strongswan.org/projects/strongswan/wiki/Autoconf>.
        ;; AESNI expects on hardware support from x86 systems.
        ,@(if (target-x86?)
              `("--enable-aesni")
              `("--disable-aesni"))
        "--enable-attr-sql"
        "--enable-chapoly"
        "--enable-curl"
        "--enable-dhcp"
        "--enable-eap-aka"
        "--enable-eap-aka-3gpp"
        "--enable-eap-dynamic"
        "--enable-eap-identity"
        "--enable-eap-md5"
        "--enable-eap-mschapv2"
        "--enable-eap-peap"
        "--enable-eap-radius"
        "--enable-eap-sim"
        "--enable-eap-sim-file"
        "--enable-eap-simaka-pseudonym"
        "--enable-eap-simaka-reauth"
        "--enable-eap-simaka-sql"
        "--enable-eap-tls"
        "--enable-eap-tnc"
        "--enable-eap-ttls"
        "--enable-ext-auth"
        "--enable-farp"
        "--enable-ha"
        "--enable-led"
        "--enable-md4"
        "--enable-mediation"
        "--enable-openssl"
        "--enable-soup"
        "--enable-sql"
        "--enable-sqlite"
        "--enable-xauth-eap"
        "--enable-xauth-noauth"
        "--enable-xauth-pam"

        ;; Use libcap by default.
        "--with-capabilities=libcap")))
    (inputs
     (list coreutils
           curl
           gmp
           libcap
           libgcrypt
           libsoup-minimal-2
           linux-pam
           openssl))
    (native-inputs
     (list bison coreutils flex perl pkg-config tzdata-for-tests))
    (synopsis "IKEv1/v2 keying daemon")
    (description "StrongSwan is an IPsec implementation originally based upon
the FreeS/WAN project.  It contains support for IKEv1, IKEv2, MOBIKE, IPv6,
NAT-T and more.")
    (home-page "https://strongswan.org/")
    (license
     (list license:gpl2+
           ;; src/aikgen/*
           ;; src/libcharon/plugins/dnscert/*
           ;; src/libcharon/plugins/ext_auth/*
           ;; src/libcharon/plugins/vici/ruby/*
           ;; src/libcharon/plugins/xauth_pam/xauth_pam_listener.[ch]
           license:expat
           ;; src/inclue/sys/*
           license:bsd-3
           ;; src/libstrongswan/plugins/sha3/sha3_keccak.c
           license:public-domain
           ;; src/libstrongswan/plugins/pkcs11/pkcs11.h
           (license:non-copyleft
            "file://src/libstrongswan/plugins/pkcs11/pkcs11.h"
            "pkcs11 contains an unknown permissive license. View the specific
file for more details.")
           ;; These files are not included in the
           ;; build, they are disabled through
           ;; options to ./configure
           ;;
           ;; src/libstrongswan/plugins/blowfish/bf_enc.c
           ;; src/libstrongswan/plugins/blowfish/bf_locl.h
           ;; src/libstrongswan/plugins/blowfish/bf_pi.h
           ;; src/libstrongswan/plugins/blowfish/bf_skey.c
           ;; src/libstrongswan/plugins/blowfish/blowfish_crypter.c
           ;; src/libstrongswan/plugins/des/des_crypter.c
           license:bsd-4))))

(define-public vpnc
  (package
   (name "vpnc")
   (version "0.5.3")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.unix-ag.uni-kl.de/~massar/vpnc/vpnc-"
                                version ".tar.gz"))
            (sha256 (base32
                     "1128860lis89g1s21hqxvap2nq426c9j4bvgghncc1zj0ays7kj6"))))
   (build-system gnu-build-system)
   (native-inputs (append (list perl pkg-config vpnc-scripts)
                          (if (%current-target-system)
                            (list this-package)
                            '())))
   (inputs (list libgcrypt vpnc-scripts))
   (arguments
     (list #:tests? #f ;; There is no check target
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "ETCDIR=" #$output "/etc/vpnc")
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure) ;; No configure script.
               (add-after 'unpack 'use-store-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((vpnc-scripts (assoc-ref inputs  "vpnc-scripts")))
                     (substitute* "config.c"
                       (("/etc/vpnc/vpnc-script")
                        (string-append vpnc-scripts
                                       "/etc/vpnc/vpnc-script"))))))
               (add-after 'unpack 'patch-Makefile
                 (lambda* (#:key target #:allow-other-keys)
                   (let* ((pkg-config #$(pkg-config-for-target))
                          (includedir (string-append pkg-config
                                                     " --variable=includedir"
                                                     " libgcrypt"))
                          (cflags (string-append pkg-config
                                                 " --cflags"
                                                 " libgcrypt"))
                          (libdir (string-append pkg-config
                                                 " --variable=libdir"
                                                 " libgcrypt"))
                          (libs (string-append pkg-config
                                               " --libs"
                                               " libgcrypt")))
                     (substitute* "Makefile"
                       (("\\$\\(shell libgcrypt-config --cflags\\)")
                        (string-append "-I$(shell " includedir ") "
                                       "$(shell " cflags ")"))
                       (("\\$\\(shell libgcrypt-config --libs\\)")
                        (string-append
                          "-L$(shell " libdir ") "
                          "$(shell " libs ")")))
                     ;; When cross-compiling the manpage can't be generated as the
                     ;; Makefile needs to execute the resulting `vpnc' binary.
                     (when target
                       (substitute* "Makefile"
                         (("all : \\$\\(BINS\\) vpnc\\.8 vpnc-script")
                          "all : $(BINS) vpnc-script")
                         (("install -m644 vpnc\\.8.*") ""))))))
               (add-after 'unpack 'install-manpage
                 (lambda* (#:key native-inputs inputs target
                           #:allow-other-keys)
                   ;; As the manpage is not generated. Instead install it from
                   ;; the input vpnc package.
                   (when target
                     (let* ((vpnc (assoc-ref native-inputs "vpnc"))
                            (man (string-append vpnc
                                                "/share/man/man8/vpnc.8.gz"))
                            (output (string-append #$output "/share/man/man8")))
                       (install-file man output))))))))
   (synopsis "Client for Cisco VPN concentrators")
   (description
    "vpnc is a VPN client compatible with Cisco's EasyVPN equipment.
It supports IPSec (ESP) with Mode Configuration and Xauth.  It supports only
shared-secret IPSec authentication with Xauth, AES (256, 192, 128), 3DES,
1DES, MD5, SHA1, DH1/2/5 and IP tunneling.  It runs entirely in userspace.
Only \"Universal TUN/TAP device driver support\" is needed in the kernel.")
   (license (list license:gpl2+ license:bsd-2))
   (home-page "https://www.unix-ag.uni-kl.de/~massar/vpnc/")))

(define-public vpnc-scripts
  (let ((commit "3885f8bbc4ae03fd6da0ada6de12f7223a59595c"))
    (package
      (name "vpnc-scripts")
      (version (string-append "20200925." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri
                 (git-reference
                  (url "git://git.infradead.org/users/dwmw2/vpnc-scripts.git")
                  (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1pmi4n58q81pmn9arvfixhvv6vkkf3rpwac3hwnwyl882q5q0ccx"))))
      (build-system gnu-build-system)
      (inputs (list guile-3.0 ; for the wrapper scripts
                    coreutils
                    grep
                    iproute ; for ‘ip’
                    net-tools ; for ‘ifconfig’, ‘route’
                    sed
                    which))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'use-relative-paths
             ;; Patch the scripts to work with and use relative paths.
             (lambda* _
               (for-each (lambda (script)
                           (substitute* script
                             (("^PATH=.*") "")
                             (("/usr/s?bin/") "")
                             (("\\[ +-x +([^]]+) +\\]" _ command)
                              (string-append "command -v >/dev/null 2>&1 "
                                             command))))
                         (find-files "." "^vpnc-script"))
               #t))
           (delete 'configure)          ; no configure script
           (replace 'build
             (lambda _
               (invoke "gcc" "-g" "-O2"
                       "-Wno-error=implicit-function-declaration"
                       "-o" "netunshare" "netunshare.c")))
           (replace 'install
             ;; There is no Makefile; manually install the relevant files.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (etc (string-append out "/etc/vpnc")))
                 (for-each (lambda (file)
                             (install-file file etc))
                           (append (find-files "." "^vpnc-script")
                                   (list "netunshare"
                                         "xinetd.netns.conf")))
                 #t)))
           (add-after 'install 'wrap-scripts
             ;; Wrap scripts with paths to their common hard dependencies.
             ;; Optional dependencies will need to be installed by the user.
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (guile (search-input-file inputs "bin/guile")))
                 (for-each
                  (lambda (script)
                    (wrap-script (string-append out "/etc/vpnc/" script)
                      #:guile guile
                      `("PATH" ":" prefix
                        ,(map (lambda (name)
                                (let ((input (assoc-ref inputs name)))
                                  (string-append input "/bin:"
                                                 input "/sbin")))
                              (list "coreutils"
                                    "grep"
                                    "iproute2"
                                    "net-tools"
                                    "sed"
                                    "which")))))
                  (list "vpnc-script-ptrtd"
                        "vpnc-script-sshd"
                        "vpnc-script"))
                 #t))))
         #:tests? #f))                  ; no tests
      (home-page "http://git.infradead.org/users/dwmw2/vpnc-scripts.git")
      (synopsis "Network configuration scripts for Cisco VPN clients")
      (description
       "This set of scripts configures routing and name services when invoked
by the VPNC or OpenConnect Cisco @dfn{Virtual Private Network} (VPN) clients.

The default @command{vpnc-script} automatically configures most common
connections, and provides hooks for performing custom actions at various stages
of the connection or disconnection process.

Alternative scripts are provided for more complicated set-ups, or to serve as an
example for writing your own.  For example, @command{vpnc-script-sshd} contains
the entire VPN in a network namespace accessible only through SSH.")
      (license license:gpl2+))))

(define-public ocproxy
  (package
    (name "ocproxy")
    (version "1.60")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cernekee/ocproxy")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03323nnhb4y9nzwva04mq7xg03dvdrgp689g89f69jqc261skcqx"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list libevent))
    (home-page "https://github.com/cernekee/ocproxy")
    (synopsis "OpenConnect proxy")
    (description
     "User-level @dfn{SOCKS} and port forwarding proxy for OpenConnect based
on LwIP.  When using ocproxy, OpenConnect only handles network activity that
the user specifically asks to proxy, so the @dfn{VPN} interface no longer
\"hijacks\" all network traffic on the host.")
    (license license:bsd-3)))

(define-public openconnect
  (package
    (name "openconnect")
    (version "9.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "ftp://ftp.infradead.org/pub/openconnect/"
                                  "openconnect-" version ".tar.gz"))
              (sha256
               (base32 "0gj1nba1pygvcjasqdakxxnx94dwx3l4hzj0dvipbzjdmbixrgm2"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-vpnc-script="
                                  (search-input-file %build-inputs
                                                     "etc/vpnc/vpnc-script")))))
    (native-inputs (list gettext-minimal pkg-config))
    (inputs (list lz4 vpnc-scripts))
    (propagated-inputs (list libxml2 gnutls zlib))
    (synopsis "Client for Cisco VPN")
    (description
     "OpenConnect is a client for Cisco's AnyConnect SSL VPN, which is
supported by the ASA5500 Series, by IOS 12.4(9)T or later on Cisco SR500, 870,
880, 1800, 2800, 3800, 7200 Series and Cisco 7301 Routers, and probably
others.")
    (license license:lgpl2.1)
    (home-page "https://www.infradead.org/openconnect/")))

(define-public openconnect-sso
  (package
    (name "openconnect-sso")
    (version "0.8.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/vlaci/openconnect-sso")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0l214qxhxx214628mcg6rmbzbzna7mxj5l7rah9q4vvcd88ymp39"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-openconnect
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "openconnect_sso/app.py"
               (("\"openconnect\"")
                (string-append "\""
                               (search-input-file inputs "/sbin/openconnect")
                               "\"")))))
         (add-after 'check 'wrap-qt-process-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/openconnect-sso"))
                    (qt-process-path
                     (search-input-file inputs
                                        "/lib/qt5/libexec/QtWebEngineProcess")))
               (wrap-program bin
                 #:sh (search-input-file inputs "bin/bash")
                 `("QTWEBENGINEPROCESS_PATH" = (,qt-process-path)))))))))
    (inputs
     (list openconnect
           poetry
           python-attrs
           python-colorama
           python-keyring
           python-lxml
           python-prompt-toolkit
           python-requests
           python-pyqt
           python-pyqtwebengine
           python-pysocks
           python-pyxdg
           python-structlog
           python-toml
           qtwebengine-5))
    (native-inputs
     (list python-pytest
           python-pytest-asyncio
           python-pytest-httpserver))
    (home-page "https://github.com/vlaci/openconnect-sso")
    (synopsis "OpenConnect wrapper script supporting Azure AD (SAMLv2)")
    (description
     "This package provides a wrapper script for OpenConnect supporting Azure AD
(SAMLv2) authentication to Cisco SSL-VPNs.")
    (license license:gpl3)))

(define-public openfortivpn
  (package
    (name "openfortivpn")
    (version "1.17.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/adrienverge/openfortivpn")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0an58f0qcyxdx3d5zb5m8vi45a0251b950b5lh16572n8z2g6s2l"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     ;; ppp < 2.5.0 is required due to
     ;; <https://github.com/adrienverge/openfortivpn/pull/1148>.
     (list openssl ppp-2.4.9))
    (home-page "https://github.com/adrienverge/openfortivpn")
    (synopsis "Client for PPP+SSL VPN tunnel services")
    (description "Openfortivpn is a client for PPP+SSL VPN tunnel services.  It
spawns a pppd process and operates the communication between the gateway and
this process.  It is compatible with Fortinet VPNs.")
    (license license:gpl3+)))

(define-public openvpn
  (package
    (name "openvpn")
    (version "2.6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://swupdate.openvpn.org/community/releases/openvpn-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0a8r3bvg4aic9b7dix0h7990g3j1gq17wd3w6vqk8vk8xgfhyq8w"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-iproute2=yes")))
    (native-inputs (list iproute pkg-config))
    (inputs (append (list lz4 lzo openssl linux-pam)
                    (if (target-linux?) (list libcap-ng) '())))
    (home-page "https://openvpn.net/")
    (synopsis "Virtual private network daemon")
    (description
     "OpenVPN implements virtual private network (@dfn{VPN}) techniques
for creating secure point-to-point or site-to-site connections in routed or
bridged configurations and remote access facilities.  It uses a custom
security protocol that utilizes SSL/TLS for key exchange.  It is capable of
traversing network address translators (@dfn{NAT}s) and firewalls.")
    (license license:gpl2)))

(define-public protonvpn-cli
  (package
    (name "protonvpn-cli")
    (version "2.2.6")
    (source
     (origin
       ;; PyPI has a ".whl" file but not a proper source release.
       ;; Thus, fetch code from Git.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonVPN/linux-cli")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y7v9ikrmy5dbjlpbpacp08gy838i8z54m8m4ps7ldk1j6kyia3n"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f ; no tests in repo
       #:phases
       (modify-phases %standard-phases
         (add-after 'wrap 'wrap-wrapper
           ;; Wrap entrypoint with paths to its hard dependencies.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((entrypoint (string-append (assoc-ref outputs "out")
                                              "/bin/protonvpn")))
               (wrap-program entrypoint
                            #:sh (search-input-file inputs "bin/bash")
                            `("PATH" ":" prefix
                              ,(map (lambda (name)
                                      (let ((input (assoc-ref inputs name)))
                                        (string-append input "/bin:"
                                                       input "/sbin")))
                                    (list "dialog"
                                          "iproute2"
                                          "iptables"
                                          "ncurses"
                                          "openvpn"
                                          "procps"
                                          "which")))))))
         ;; The `protonvpn' script wants to write to `~user' to initialize its
         ;; logger, so simply setting HOME=/tmp won't cut it.  Remove
         ;; sanity-check.
         (delete 'sanity-check))))
    (native-inputs
     (list python-docopt))
    (inputs
     (list bash-minimal
           dialog
           iproute
           iptables
           ncurses
           openvpn
           procps
           python-jinja2
           python-pythondialog
           python-requests
           which))
    (synopsis "Command-line client for ProtonVPN")
    (description
     "This is the official command-line interface for ProtonVPN, a secure
point-to-point virtual private networking (VPN) service with a gratis tier.
It can automatically find and connect to the fastest servers or use Tor over
VPN.  The gratis tier offers unlimited bandwidth for up to 10 devices.")
    (home-page "https://github.com/ProtonVPN/linux-cli")
    (license license:gpl3+)))

(define-public tinc
  (package
    (name "tinc")
    (version "1.0.36")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tinc-vpn.org/packages/"
                                  "tinc-" version ".tar.gz"))
              (sha256
               (base32
                "021i2sl2mjscbm8g59d7vs74iw3gf0m48wg7w3zhwj6czarkpxs0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--sysconfdir=/etc"
         "--localstatedir=/var")))
    (inputs (list zlib lzo openssl))
    (home-page "https://tinc-vpn.org")
    (synopsis "Virtual Private Network (VPN) daemon")
    (description
     "Tinc is a VPN that uses tunnelling and encryption to create a secure
private network between hosts on the internet.")
    (license license:gpl2+)))

(define-public sshuttle
  (package
    (name "sshuttle")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sshuttle/sshuttle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01hd7z7gxkc2bjxndnv5dw1x98qcakxli9k8w285iq2b7d786f7f"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:modules
           `((guix build pyproject-build-system)
             (guix build utils)
             (ice-9 match)
             (srfi srfi-26))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-FHS-file-names
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "sshuttle/client.py"
                     (("(/usr)?(/bin/env)" _ _ command)
                      (search-input-file inputs command)))
                   (substitute* "sshuttle/ssh.py"
                     (("/bin/sh" command)
                      (search-input-file inputs command)))))
               (add-after 'install 'install-documentation
                 (lambda _
                   (with-directory-excursion "docs"
                     (invoke "make" "info" "man")
                     (with-directory-excursion "_build"
                       (define (install-man-page file)
                         (match (string-split file #\.)
                           ((_ ... section)
                            (install-file file
                                          (string-append #$output
                                                         "/share/man/man"
                                                         section)))))

                       (for-each install-man-page
                                 (find-files "man" "\\.[0-9]$"))
                       (for-each (cut install-file <>
                                      (string-append #$output "/share/info"))
                                 (find-files "texinfo" "\\.info$")))))))))
    (native-inputs
     (list python-setuptools-scm
           ;; For tests only.
           python-flake8
           python-mock
           python-poetry-core
           python-pytest-cov
           python-pytest-runner
           ;; For documentation only.
           python-sphinx
           texinfo))
    (home-page "https://github.com/sshuttle/sshuttle")
    (synopsis "VPN that transparently forwards connections over SSH")
    (description "sshuttle creates an encrypted virtual private network (VPN)
connection to any remote server to which you have secure shell (SSH) access.
The only requirement is a suitable version of Python on the server;
administrative privileges are required only on the client.  Unlike most VPNs,
sshuttle forwards entire sessions, not packets, using kernel transparent
proxying.  This makes it faster and more reliable than SSH's own tunneling and
port forwarding features.  It can forward both TCP and UDP traffic, including
DNS domain name queries.")
    (license license:lgpl2.0))) ; incorrectly identified as GPL in ‘setup.py’

(define-public sshoot
  (package
    (name "sshoot")
    (version "1.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri name version))
       (sha256
        (base32
         "05i54nga4vy660yy9yf6dl376yj0jc51303yr295qk3k9w0k96yd"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda _
             (substitute* "sshoot/tests/test_manager.py"
               (("/bin/sh") (which "sh")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest" "-vv" "--pyargs" "sshoot")))))))
    (inputs
     (list python-argcomplete
           python-prettytable
           python-pyyaml
           python-pyxdg
           python-toolrack))
    ;; For tests only.
    (native-inputs
     (list python-pytest python-pytest-mock))
    (home-page "https://github.com/albertodonato/sshoot")
    (synopsis "VPN session manager (sshuttle)")
    (description "sshoot provides a command-line interface to manage multiple
@command{sshuttle} virtual private networks.  It supports flexible profiles
with configuration options for most of @command{sshuttle}’s features.")
    (license license:gpl3+)))

(define-public badvpn
  (package
    (name "badvpn")
    (version "1.999.130")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ambrop72/badvpn")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rm67xhi7bh3yph1vh07imv5y1pwyldvw3wa5bz471g8mnkc7d3c"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
    (inputs
     (list nspr nss openssl))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/ambrop72/badvpn")
    (synopsis "Peer-to-peer virtual private network (VPN)")
    (description "@code{BadVPN} is a collection of virtual private
network (VPN) tools.  It includes:

@enumerate
@item NCD programming language.\n
NCD (Network Configuration Daemon) is a daemon and programming/scripting
language for configuration of network interfaces and other aspects of the
operating system.
@item Tun2socks network-layer proxifier.\n
The tun2socks program socksifes TCP connections at the network layer.  It
implements a TUN device which accepts all incoming TCP connections (regardless
of destination IP), and forwards the connections through a SOCKS server.
@item Peer-to-peer VPN.\n
The peer-to-peer VPN implements a Layer 2 (Ethernet) network between the peers
(VPN nodes).
@end enumerate")
    ;; This project contains a bundled lwIP. lwIP is also released under the
    ;; 3-clause BSD license.
    (license license:bsd-3)))

(define-public wireguard-linux-compat
  (package
    (name "wireguard-linux-compat")
    (version "1.0.20201221")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.zx2c4.com/wireguard-linux-compat/"
                                  "snapshot/wireguard-linux-compat-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0ci13in0fqq32n5qamch4qhjgbdq86ygrgmfhc9szsh2nsl8jlkf"))))
    (build-system linux-module-build-system)
    (outputs '("out"
               "kernel-patch"))
    (arguments
     `(#:linux ,linux-libre-5.4         ; mustn't have WG built-in
       #:tests? #f                      ; no test suite
       #:modules ((guix build linux-module-build-system)
                  (guix build utils)
                  (ice-9 popen)
                  (ice-9 textual-ports))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'change-directory
           (lambda _
             (chdir "./src")
             #t))
         (add-after 'build 'build-patch
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((patch-builder "../kernel-tree-scripts/create-patch.sh")
                    (port (open-input-pipe patch-builder))
                    (str (get-string-all port)))
               (close-pipe port)
               (call-with-output-file "wireguard.patch"
                 (lambda (port)
                   (format port "~a" str))))
             #t))
         (add-after 'install 'install-patch
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "wireguard.patch"
                           (assoc-ref %outputs "kernel-patch"))
             #t))
         ;; So that 'install-license-files' works...
         (add-before 'install-license-files 'reset-cwd
           (lambda _
             (chdir "..")
             #t)))))
    (home-page "https://git.zx2c4.com/wireguard-linux-compat/")
    (synopsis "WireGuard kernel module for Linux 3.10 through 5.5")
    (description "This package contains an out-of-tree kernel patch and
a loadable module adding WireGuard to Linux kernel versions 3.10 through 5.5.
WireGuard was added to Linux 5.6.")
    (license license:gpl2)))

(define-public wireguard-tools
  (package
    (name "wireguard-tools")
    (version "1.0.20250521")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.zx2c4.com/wireguard-tools.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02xqy774m36930091ng75g8qd1j7yf8r0blysdwqpjzvpvs656s8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             "--directory=src"
             "WITH_BASHCOMPLETION=yes"
             ;; Install the ‘simple and dirty’ helper script wg-quick(8).
             "WITH_WGQUICK=yes"
             (string-append "PREFIX=" (assoc-ref %outputs "out"))
             ;; Currently used only to create an empty /etc/wireguard directory.
             (string-append "SYSCONFDIR=no-thanks"))
       ;; The test suite is meant to be run interactively.  It runs Clang's
       ;; scan-build static analyzer and then starts a web server to display the
       ;; results.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (add-after 'install 'install-contrib-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/wireguard-tools")))
               (copy-recursively "contrib/" doc))))
         (add-after 'install 'wrap-wg-quick
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (inputs-sbin (map (lambda (input)
                                        (string-append (assoc-ref inputs input)
                                                       "/sbin"))
                                      (list "iproute"
                                            "iptables"
                                            "procps"
                                            "resolvconf")))
                    (coreutils (string-append (assoc-ref inputs "coreutils")
                                              "/bin")))
               (wrap-program (string-append out "/bin/wg-quick")
                 #:sh (search-input-file inputs "bin/bash")
                 `("PATH" ":" prefix ,(append inputs-sbin
                                              (list coreutils))))))))))
    (inputs
     `(("resolvconf" ,openresolv)
       ("coreutils" ,coreutils)
       ("bash" ,bash)                   ; for scripts using /dev/tcp
       ("procps" ,procps)
       ("iproute" ,iproute)
       ("iptables" ,iptables)))
    (home-page "https://www.wireguard.com/")
    (synopsis "Tools for configuring WireGuard tunnels")
    (description
     "This package provides the user-space command-line tools for using and
configuring WireGuard tunnels.

WireGuard is a simple and fast general-purpose @acronym{VPN, Virtual Private
Network} that securely encapsulates IP packets over UDP.  It aims to be as easy
to configure and deploy as SSH.  VPN connections are made simply by exchanging
public keys and can roam across IP addresses.")
    (license
     (list license:lgpl2.1+    ; src/netlink.h & contrib/embeddable-wg-library
           license:gpl2))))    ; everything else

(define-public xl2tpd
  (package
    (name "xl2tpd")
    (version "1.3.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xelerance/xl2tpd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06aiidwygywaa1jn8m2pw8l3vnsc2bjnacbjmlsdy1cqgr1f5cc9"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:make-flags
       #~(list (string-append "PREFIX=" #$output)
               (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
           (delete 'configure) ;no configure script
           (add-before 'build 'setup-environment
             (lambda* (#:key inputs #:allow-other-keys)
               (substitute* "l2tp.h"
                 (("/usr/sbin/pppd")
                  (search-input-file inputs "/sbin/pppd")))
               (setenv "KERNELSRC"
                       (assoc-ref inputs "kernel-headers")))))
       #:tests? #f))                    ; no tests provided
    (inputs (list libpcap ppp))
    (home-page "https://www.xelerance.com/software/xl2tpd/")
    (synopsis "Layer 2 Tunnelling Protocol Daemon (RFC 2661)")
    (description
     "xl2tpd is an implementation of the Layer 2 Tunnelling Protocol (RFC 2661).
L2TP allows you to tunnel PPP over UDP.")
    (license license:gpl2)))

(define-public vpn-slice
  (package
    (name "vpn-slice")
    (version "0.16.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "vpn-slice" version))
       (sha256
        (base32 "1anfx4hn2ggm6sbwqmqx68s3l2rjcy4z4l038xqb440jnk8jvl18"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-FHS-file-names
           (lambda _
             (substitute* "vpn_slice/linux.py"
               (("/sbin/iptables")
                (which "iptables"))
               (("/sbin/ip")
                (which "ip"))))))))
    (inputs (list python-dnspython python-setproctitle iproute iptables))
    (home-page "https://github.com/dlenski/vpn-slice")
    (synopsis "Split tunneling replacement for vpnc-script")
    (description
     "vpn-slice is a replacement for @command{vpnc-script} used by
@code{openconnect} and @code{vpnc}.  Instead of trying to copy the behavior of
standard corporate VPN clients, which normally reroute all your network
traffic through the VPN, vpn-slice tries to minimize your contact with an
intrusive VPN.  This is also known as a split-tunnel VPN, since it splits your
traffic between the VPN tunnel and your normal network interfaces.

By default, vpn-slice only routes traffic for specific hosts or subnets
through the VPN.  It automatically looks up named hosts, using the VPN's DNS
servers, and adds entries for them to your @file{/etc/hosts} (which it cleans
up after VPN disconnection), however it does not otherwise alter your
@file{/etc/resolv.conf} at all.")
    (license license:gpl3+)))
