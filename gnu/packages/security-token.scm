;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2016 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017, 2019, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Raphaël Mélotte <raphael.melotte@mind.be>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Antero Mejr <antero@kodmin.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Sergey Trofimov <sarg@sarg.org.ru>
;;; Copyright © 2021 Dhruvin Gandhi <contact@dhruvin.dev>
;;; Copyright © 2021 Ahmad Jarara <git@ajarara.io>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2023 Jake Leporte <jakeleporte@outlook.com>
;;; Copyright © 2023 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Pierre Langlois <pierre.langlois@gmx.com>
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

(define-module (gnu packages security-token)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public ccid
  (package
    (name "ccid")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://ccid.apdu.fr/files/ccid-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "06bjdswbwcwndsn23rsdhz5a7xqsgb66glqnk9lqzd7qws3l94qk"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--enable-usbdropdir="
                             #$output
                             "/pcsc/drivers"))
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'patch-Makefile
           (lambda _
             (substitute* "src/Makefile.in"
               (("/bin/echo") (which "echo")))
             #t)))))
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list libusb pcsc-lite))
    (home-page "https://ccid.apdu.fr/")
    (synopsis "PC/SC driver for USB smart card devices")
    (description
     "This package provides a PC/SC IFD handler implementation for devices
compliant with the CCID and ICCD protocols.  It supports a wide range of
readers and is needed to communicate with such devices through the
@command{pcscd} resource manager.")
    (license license:lgpl2.1+)))

(define-public eid-mw
  (package
    (name "eid-mw")
    (version "5.1.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Fedict/eid-mw")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12hhr5v4shsg47wg10p7l03xhzpc1yk46h4bfxq5c224cbf4qrs8"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           gettext-minimal
           libassuan
           libtool
           perl
           pkg-config))
    (inputs
     (list curl
           openssl
           gtk+
           pcsc-lite
           p11-kit
           libproxy
           libxml2
           cyrus-sasl))
    (arguments
     `(#:configure-flags
       (list "--disable-static"

             ;; With the (prettier) pinentry enabled, eid-viewer will skip
             ;; crucial dialogue when used with card readers with built-in
             ;; keypads such as the Digipass 870, and possibly others too.
             "--disable-pinentry")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _
             ;; configure.ac relies on ‘git --describe’ to get the version.
             ;; Patch it to just return the real version number directly.
             (substitute* "scripts/build-aux/genver.sh"
               (("/bin/sh") (which "sh"))
               (("^(GITDESC=).*" _ match) (string-append match ,version "\n")))
             (invoke "sh" "./bootstrap.sh"))))))
    (synopsis "Belgian electronic identity card (eID) middleware")
    (description "The Belgian eID middleware is required to authenticate with
online services and sign digital documents with Belgian identity cards.

It requires a running pcscd service and a compatible card reader.")
    (home-page "https://github.com/Fedict/eid-mw")
    (license license:lgpl3)))

(define-public libyubikey
  (package
    (name "libyubikey")
    (version "1.13")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-c/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "009l3k2zyn06dbrlja2d4p2vfnzjhlcqxi88v02mlrnb17mx1v84"))))
    (build-system gnu-build-system)
    (synopsis "Development kit for the YubiKey authentication device")
    (description
     "This package contains a C library and command-line tools that make up
the low-level development kit for the Yubico YubiKey authentication device.")
    (home-page "https://developers.yubico.com/yubico-c/")
    (license license:bsd-2)))

(define-public softhsm
  (package
    (name "softhsm")
    (version "2.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dist.opendnssec.org/source/"
                    "softhsm-" version ".tar.gz"))
              (sha256
               (base32
                "1wkmyi6n3z2pak1cj5yk6v6bv9w0m24skycya48iikab0mrr8931"))
              (patches (search-patches "softhsm-fix-openssl3-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-gost"))) ; TODO Missing the OpenSSL
                                               ; engine for GOST
    (inputs
     (list openssl))
    (native-inputs
     (list pkg-config cppunit))
    (synopsis "Software implementation of a generic cryptographic device")
    (description
     "SoftHSM 2 is a software implementation of a generic cryptographic device
with a PKCS #11 Cryptographic Token Interface.")
    (home-page "https://www.opendnssec.org/softhsm/")
    (license license:bsd-2)))

(define-public pcsc-lite
  (package
    (name "pcsc-lite")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://pcsclite.apdu.fr/files/"
                                  "pcsc-lite-" version ".tar.bz2"))
              (sha256
               (base32
                "0mlk32gpzmzjf5v8qn56lpyyba625jzzw8rkrmpyvr8h8nvf5hyn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-usbdropdir=/var/lib/pcsc/drivers"
                           "--disable-libsystemd")))
    (native-inputs
     (list flex
           perl                         ;for pod2man
           pkg-config))
    (inputs
     (list python eudev))
    (home-page "https://pcsclite.apdu.fr/")
    (synopsis "Middleware to access a smart card using PC/SC")
    (description
     "pcsc-lite provides an interface to communicate with smartcards and
readers using the SCard API.  pcsc-lite is used to connect to the PC/SC daemon
from a client application and provide access to the desired reader.")
    (license (list license:bsd-3                ; pcsc-lite
                   license:isc                  ; src/strlcat.c src/strlcpy.c
                   license:gpl3+))))            ; src/spy/*

(define-public pcsc-tools
  (package
    (name "pcsc-tools")
    (version "1.6.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/rousseau/pcsc-tools.git/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16kvw8y5289fp6y3z8l5w61gfrk872kd500a27sgr5k5dpr9vfbk"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-data-paths
                 (lambda _
                   (substitute* "ATR_analysis"
                     (((string-append
                        "\"/usr/local/pcsc/smartcard_list.txt\", "
                        "\"/usr/share/pcsc/smartcard_list.txt\", "
                        "\"/usr/local/share/pcsc/smartcard_list.txt\""))
                      (string-append "\"" #$output
                                     "/share/pcsc/smartcard_list.txt\"")))
                   (substitute* "ATR_analysis.1p"
                     (("^(\\.IR \\./) ,\n$" _ cwd)
                      (string-append cwd "\n"))
                     (("^\\.I /usr/local/pcsc/\n$")
                      "")
                     (("/usr/share/pcsc/\n$")
                      (string-append #$output "/share/pcsc/\n")))))
               (add-after 'patch-shebangs 'wrap-programs
                 (lambda _
                   (for-each
                    (lambda (prog)
                      (wrap-program (string-append #$output "/bin/" prog)
                        `("PERL5LIB" = (,(getenv "PERL5LIB")))))
                    '("ATR_analysis" "gscriptor" "scriptor"))
                   (wrap-program (string-append #$output "/bin/gscriptor")
                     `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs (list autoconf automake libtool gnu-gettext pkg-config))
    (inputs (list bash-minimal          ;for wrap-program
                  perl
                  perl-gtk3
                  pcsc-lite
                  perl-pcsc))
    (synopsis "Smart cards and PC/SC tools")
    (description "This package provides the @command{pcsc_scan},
@command{ATR_analysis}, @command{scriptor}, and @command{gscriptor} commands,
which are useful tools to test a PC/SC driver, card or reader or send commands
in a friendly environment (text or graphical user interface).")
    (home-page "https://pcsc-tools.apdu.fr/")
    (license license:gpl2+)))

(define-public ykclient
  (package
    (name "ykclient")
    (version "2.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubico-c-client/Releases/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "05jhx9waj3pl120ddnwap1v3bjrnbfhvf3lxs2xmhpcmwzpwsqgl"))))
    (build-system gnu-build-system)

    ;; There's just one test, and it requires network access to access
    ;; yubico.com, so skip it.
    (arguments '(#:tests? #f))

    (native-inputs (list pkg-config help2man))
    (inputs (list curl))
    (synopsis "C library to validate one-time-password YubiKeys")
    (description
     "YubiKey C Client Library (libykclient) is a C library used to validate a
one-time-password (OTP) YubiKey against Yubico’s servers.  See the Yubico
website for more information about Yubico and the YubiKey.")
    (home-page "https://developers.yubico.com/yubico-c-client/")
    (license license:bsd-2)))

(define-public opensc
  (package
    (name "opensc")
    (version "0.26.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/OpenSC/OpenSC/releases/download/"
                    version "/opensc-" version ".tar.gz"))
              (sha256
               (base32
                "1jw82r7dahmfzvri16d46wivrz7jx8srybknjh1mfvnq66h92qpi"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; By setting an absolute path here, we arrange for OpenSC to
          ;; successfully dlopen libpcsclite.so.1 by default.  The user can
          ;; still override this if they want to, by specifying a custom OpenSC
          ;; configuration file at runtime.
          (add-after 'unpack 'set-default-libpcsclite.so.1-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libpcsclite (search-input-file inputs "/lib/libpcsclite.so.1")))
                (substitute* "configure"
                  (("DEFAULT_PCSC_PROVIDER=\"libpcsclite\\.so\\.1\"")
                   (string-append "DEFAULT_PCSC_PROVIDER=\"" libpcsclite "\"")))))))))
    (inputs
     (list readline openssl-1.1 pcsc-lite ccid))
    (native-inputs
     (list libxslt docbook-xsl pkg-config))
    (home-page "https://github.com/OpenSC/OpenSC/wiki")
    (synopsis "Tools and libraries related to smart cards")
    (description
     "OpenSC is a set of software tools and libraries to work with smart
cards, with the focus on smart cards with cryptographic capabilities.  OpenSC
facilitate the use of smart cards in security applications such as
authentication, encryption and digital signatures.  OpenSC implements the PKCS
#15 standard and the PKCS #11 API.")
    (license license:lgpl2.1+)))

(define-public pkcs11-helper
  (package
    (name "pkcs11-helper")
    (version "1.30.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/OpenSC/pkcs11-helper/releases/download/pkcs11-helper-"
             version "/pkcs11-helper-" version ".tar.bz2"))
       (sha256
        (base32 "1ac86jfj4qfwzbvsg6l9r4w4bbwxj2i9qi4dy1nz5aqcj6x1an2c"))))
    (build-system gnu-build-system)
    (inputs (list openssl pcsc-lite))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/OpenSC/pkcs11-helper")
    (synopsis "Library that simplifies the interaction with PKCS#11 providers")
    (description
     "Pkcs11-helper is a library that simplifies the interaction with PKCS#11
providers for end-user applications.  PKCS#11 is published standard.  PKCS#11
is the de-facto standard to access cryptographic devices")
    (license (list license:gpl2 license:bsd-3))))


(define-public yubico-piv-tool
  (package
    (name "yubico-piv-tool")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yubico/yubico-piv-tool/")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gxrn2yzs907h22233s2337j5zb8mvygvk0z2macl4rf8w6qf4vk"))))
    (build-system cmake-build-system)
    (inputs
     (list gengetopt perl pcsc-lite openssl))
    (native-inputs
     (list check
           doxygen
           graphviz
           help2man
           pkg-config
           (texlive-local-tree)))
    (home-page "https://developers.yubico.com/yubico-piv-tool/")
    (synopsis "Interact with the PIV application on a YubiKey")
    (description
     "The Yubico PIV tool is used for interacting with the Privilege and
Identification Card (PIV) application on a YubiKey.  With it you may generate
keys on the device, import keys and certificates, create certificate requests,
and other operations.  It includes a library and a command-line tool.")
    ;; The file ykcs11/pkcs11.h also declares an additional, very short free
    ;; license for that one file.  Please see it for details.  The vast
    ;; majority of files are licensed under bsd-2.
    (license license:bsd-2)))

(define-public yubikey-personalization
  (package
    (name "yubikey-personalization")
    (version "1.20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/" name
                    "/Releases/ykpers-" version ".tar.gz"))
              (sha256
               (base32
                "14wvlwqnwj0gllkpvfqiy8ns938bwvjsz8x1hmymmx32m074vj0f"))
              (modules '((guix build utils)))
              (snippet
               ;; Fix build with GCC 10, remove for versions > 1.20.0.
               '(begin
                  (substitute* "ykpers-args.h"
                    (("^const char")
                     "extern const char"))))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--with-udevrulesdir="
                                              (assoc-ref %outputs "out")
                                              "/lib/udev/rules.d"))))
    (inputs
     (list json-c-0.13 libusb
           ;; The library "libyubikey" is also known as "yubico-c".
           libyubikey))
    (native-inputs
     (list pkg-config eudev))
    (home-page "https://developers.yubico.com/yubikey-personalization/")
    (synopsis "Library and tools to personalize YubiKeys")
    (description
     "The YubiKey Personalization package contains a C library and command
line tools for personalizing YubiKeys.  You can use these to set an AES key,
retrieve a YubiKey's serial number, and so forth.")
    (license license:bsd-2)))

(define-public python-pyscard
  (package
    (name "python-pyscard")
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              ;; The maintainer publishes releases on various sites, but
              ;; SourceForge is apparently the only one with a signed release.
              (uri (string-append
                    "mirror://sourceforge/pyscard/pyscard/pyscard%20"
                    version "/pyscard-" version ".tar.gz"))
              (sha256
               (base32
                "1gy1hmzrhfa7bqs132v89pchm9q3rpnqf3a6225vwpx7bx959017"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Tell pyscard where to find the PCSC include directory.
         (add-after 'unpack 'patch-platform-include-dirs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((pcsc-include-dir (search-input-directory
                                      inputs "/include/PCSC")))
               (substitute* "setup.py"
                 (("platform_include_dirs = \\[.*?\\]")
                  (string-append
                   "platform_include_dirs = ['" pcsc-include-dir "']"))))))
         ;; pyscard wants to dlopen libpcsclite, so tell it where it is.
         (add-after 'unpack 'patch-dlopen
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "smartcard/scard/winscarddll.c"
               (("lib = \"libpcsclite\\.so\\.1\";")
                (simple-format
                 #f
                 "lib = \"~a\";"
                 (search-input-file inputs "/lib/libpcsclite.so.1")))))))))
    (inputs
     (list pcsc-lite))
    (native-inputs
     (list swig))
    (home-page "https://github.com/LudovicRousseau/pyscard")
    (synopsis "Smart card library for Python")
    (description
     "The pyscard smart card library is a framework for building smart card
aware applications in Python.  The smart card module is built on top of the
PCSC API Python wrapper module.")
    (license license:lgpl2.1+)))

(define-public yubikey-oath-dmenu
  (package
    (name "yubikey-oath-dmenu")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emlun/yubikey-oath-dmenu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1h6dr4l0gzgdg8zn2c39kx9cx1bgvwqxkz3z95qz9r70xfsghgwk"))))
    (build-system python-build-system)
    (arguments
     (list
      #:tests? #f ; there are no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure script
          (delete 'build)     ; or build
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "yubikey-oath-dmenu.py"
                (("'(dmenu|notify-send|wl-copy|xclip|xdotool)" _ tool)
                 (string-append
                  "'"
                  (search-input-file inputs
                                     (string-append "/bin/" tool)))))))
          (replace 'install
            (lambda _
              (invoke "make" "install"
                      (string-append "PREFIX=" #$output)))))))
    (inputs
     (list dmenu
           libnotify
           python-click
           python-yubikey-manager
           ;; TODO add wtype, once packaged, for type support for Wayland
           wl-clipboard ; optional clipboard support for Wayland
           xclip        ; optional clipboard support for X11
           xdotool))    ; optional type support for X11
    (home-page
     "https://github.com/emlun/yubikey-oath-dmenu/")
    (synopsis "Interface for getting OATH codes from a YubiKey using dmenu")
    (description
     "Yubikey-oath-demenu lets you pick an OATH credential from your YubiKey using
dmenu, and copies the corresponding OTP to the clipboard.  Alternatively, it
can \"type\" the OTP using @code{xdotool} on X11.

Notable features:

@itemize
@item Pick between all credentials on all connected YubiKeys
@item No mouse interaction required
@end itemize\n")
    (license license:gpl3+)))

(define-public libu2f-host
  (package
    (name "libu2f-host")
    (version "1.1.10")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://developers.yubico.com"
                "/libu2f-host/Releases/libu2f-host-" version ".tar.xz"))
              (sha256
               (base32
                "0vrivl1dwql6nfi48z6dy56fwy2z13d7abgahgrs2mcmqng7hra2"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-gtk-doc"
                   (string-append "--with-udevrulesdir=" #$output
                                  "/lib/udev/rules.d"))))
    (inputs (list json-c-0.13 hidapi))
    (native-inputs
     (list help2man
           gengetopt
           pkg-config
           gtk-doc/stable
           docbook-xml-4.3
           eudev))
    (home-page "https://developers.yubico.com/libu2f-host/")
    ;; TRANSLATORS: The U2F protocol has a "server side" and a "host side".
    (synopsis "U2F host-side C library and tool")
    (description
     "Libu2f-host provides a C library and command-line tool that implements
the host-side of the Universal 2nd Factor (U2F) protocol.  There are APIs to
talk to a U2F device and perform the U2F Register and U2F Authenticate
operations.")
    ;; Most files are LGPLv2.1+, but some files are GPLv3+.
    (license (list license:lgpl2.1+ license:gpl3+))))

(define-public libu2f-server
  (package
    (name "libu2f-server")
    (version "1.1.0")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/Yubico/libu2f-server")
                (commit (string-append "libu2f-server-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nmsfq372zza5y6j13ydincjf324bwfcjg950vykh166xkp6wiic"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-gtk-doc"
             "--enable-tests")))
    (inputs
     (list json-c-0.13 libressl))
    (native-inputs
     (list autoconf
           automake
           libtool
           check
           gengetopt
           help2man
           pkg-config
           gtk-doc/stable
           which))
    (home-page "https://developers.yubico.com/libu2f-server/")
    ;; TRANSLATORS: The U2F protocol has a "server side" and a "host side".
    (synopsis "U2F server-side C library")
    (description
     "This is a C library that implements the server-side of the
@dfn{Universal 2nd Factor} (U2F) protocol.  More precisely, it provides an API
for generating the JSON blobs required by U2F devices to perform the U2F
Registration and U2F Authentication operations, and functionality for
verifying the cryptographic operations.")
    (license license:bsd-2)))

(define-public pam-u2f
  (package
    (name "pam-u2f")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/Yubico/pam-u2f")
                (commit (string-append "pam_u2f-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0528r0q3j1d6cb3dzh9vgagr8v2b2y5yylykr1cqjmg9hvp35a4i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-pam-dir="
                            (assoc-ref %outputs "out") "/lib/security"))))
    (inputs
     (list libfido2 linux-pam openssl))
    (native-inputs
     (list asciidoc autoconf automake libtool pkg-config))
    (home-page "https://developers.yubico.com/pam-u2f/")
    (synopsis "PAM module for U2F authentication")
    (description
     "This package provides a module implementing PAM over U2F, providing an
easy way to integrate the YubiKey (or other U2F compliant authenticators) into
your existing infrastructure.")
    (license license:bsd-2)))

(define-public python-fido2
  (package
    (name "python-fido2")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/Yubico/python-fido2/releases/download/"
                version "/fido2-" version ".tar.gz"))
              (sha256
               (base32
                "1hwz0xagkmy6hhcyfl66dxf2vfa69lqqqjrv70vw7harik59bi2x"))
              (snippet
               ;; Remove bundled dependency.
               '(delete-file "fido2/public_suffix_list.dat"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'install-public-suffix-list
            (lambda* (#:key inputs #:allow-other-keys)
              (copy-file
               (search-input-file inputs
                                  (string-append
                                   "/share/public-suffix-list-"
                                   #$(package-version public-suffix-list)
                                   "/public_suffix_list.dat"))
               "fido2/public_suffix_list.dat"))))))
    (propagated-inputs
     (list python-cryptography python-pyscard))
    (native-inputs
     (list python-poetry-core
           python-pytest
           public-suffix-list))
    (home-page "https://github.com/Yubico/python-fido2")
    (synopsis "Python library for communicating with FIDO devices over USB")
    (description
     "This Python library provides functionality for communicating with a Fast
IDentity Online (FIDO) device over Universal Serial Bus (USB) as well as
verifying attestation and assertion signatures.  It aims to support the FIDO
Universal 2nd Factor (U2F) and FIDO 2.0 protocols for communicating with a USB
authenticator via the Client-to-Authenticator Protocol (CTAP 1 and 2).  In
addition to this low-level device access, classes defined in the
@code{fido2.client} and @code{fido2.server} modules implement higher level
operations which are useful when interfacing with an Authenticator, or when
implementing a Relying Party.")
    ;; python-fido2 contains some derivative files originally from pyu2f
    ;; (https://github.com/google/pyu2f).  These files are licensed under the
    ;; Apache License, version 2.0.  The maintainers have customized these
    ;; files for internal use, so they are not really a bundled dependency.
    (license (list license:bsd-2 license:asl2.0))))

(define-public python-yubikey-manager
  (package
    (name "python-yubikey-manager")
    (version "5.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://developers.yubico.com/yubikey-manager/Releases"
                    "/yubikey_manager-" version ".tar.gz"))
              (sha256
               (base32
                "1kma08rxvpzn2gf8b9vxyyb2pvrakm7hhpdmbnb54nwbdnbxp1v4"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-click
           python-cryptography
           python-fido2
           python-keyring
           python-pyopenssl
           python-pyscard
           python-pyusb))
    (inputs
     (list pcsc-lite))
    (native-inputs
     (list python-makefun
           python-poetry-core
           python-pytest
           swig))
    (home-page "https://developers.yubico.com/yubikey-manager/")
    (synopsis "Command line tool and library for configuring a YubiKey")
    (description
     "Python library and command line tool for configuring a YubiKey.  Note
that after installing this package, you might still need to add appropriate
udev rules to your system configuration to be able to configure the YubiKey as
an unprivileged user.")
    (license license:bsd-2)))

(define-public yubikey-manager-qt
  (package
    (name "yubikey-manager-qt")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://developers.yubico.com/" name
                                  "/Releases/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qjp9p7i6957lf6ycwwz0178nmjgdyydb2f235bkin0pfm3rxcp9"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "ykman-gui/deployment.pri"
                (("/usr/bin")
                 (string-append #$output "/bin")))))
          (replace 'configure
            (lambda _
              (invoke "qmake")))
          (add-after 'install 'install-desktop-resources
            (lambda _
              (let ((datadir (string-append #$output "/share")))
                (with-directory-excursion "resources"
                  (install-file "icons/ykman.png"
                                (string-append datadir "/pixmaps"))
                  (install-file "com.yubico.yubikey_manager.metainfo.xml"
                                (string-append datadir "/metainfo"))
                  (install-file "ykman-gui.desktop"
                                (string-append datadir "/applications"))))))
          (add-after 'qt-wrap 'wrap-more
            (lambda _
              (wrap-program (string-append #$output "/bin/ykman-gui")
                ;; Wrap PYTHONPATH so that pyotherside can find the
                ;; yubikey-manager library.
                `("GUIX_PYTHONPATH" prefix
                  (,(getenv "GUIX_PYTHONPATH")))))))))
    (native-inputs (list python-wrapper))
    (inputs (list pyotherside-for-qt5
                  python-yubikey-manager
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtquickcontrols-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qtwayland-5))
    (home-page "https://developers.yubico.com/yubikey-manager-qt/")
    (synopsis "GUI for configuring any YubiKey over all USB interfaces")
    (description "YubiKey Manager (Qt) is a graphical application for
configuring any YubiKey over all USB interfaces.  For a CLI alternative, refer
to the @code{python-yubikey-manager} package.")
    (license license:bsd-2)))

(define-public libnitrokey
  (package
    (name "libnitrokey")
    (version "3.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Nitrokey/libnitrokey")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b95l979y353rli54a79z18iya9bza83cymcjpndr01q1pb134zm"))))
    (build-system cmake-build-system)
    (arguments
     ;; These tests do not require any device to be connected
     '(#:configure-flags (list "-DCOMPILE_OFFLINE_TESTS=ON")))
    (native-inputs (list catch2 doxygen graphviz pkg-config))
    (inputs (list hidapi libusb))
    (home-page "https://github.com/Nitrokey/libnitrokey")
    (synopsis "Communication library for Nitrokey")
    (description "This package provides a communication library for Nitrokey.")
    (license license:lgpl3+)))

(define-public cppcodec
  (package
    (name "cppcodec")
    (version "0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tplgy/cppcodec")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z39f8w0zvra874az0f67ck1al9kbpaidpilggbl8jnfs05010ck"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags (list "-DBUILD_TESTING=on")))
    (native-inputs (list pkg-config qttools-5))
    (inputs (list catch2))
    (home-page "https://github.com/tplgy/cppcodec")
    (synopsis "Header library to encode/decode base64, base64url, etc.")
    (description "This package provides library to encode/decode base64,
base64url, base32, base32hex and hex.")
    (license license:expat)))

(define-public nitrokey-app
  (package
    (name "nitrokey-app")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Nitrokey/nitrokey-app")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1imbvaf0yncz36ckjr99x94jwg2hnid49hsiqlxsv7ccxgk058bk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ;no test suite
    (native-inputs (list pkg-config qttools-5))
    (inputs (list cppcodec
                  hidapi
                  libnitrokey
                  libusb
                  qtbase-5
                  qtsvg-5))
    (home-page "https://github.com/Nitrokey/nitrokey-app")
    (synopsis "GUI tool for Nitrokey devices")
    (description
     "This package provides GUI tool that interfaces with Nitrokey Pro
v0.7/v0.8 and Nitrokey Storage devices.")
    (license license:gpl3+)))

(define-public nitrocli
  (package
    (name "nitrocli")
    (version "0.4.1")
    (source (origin
              (method url-fetch)
              (uri (crate-uri "nitrocli" version))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1djspfvcqjipg17v8hkph8xrhkdg1xqjhq5jk1sr8vr750yavidy"))))
    (build-system cargo-build-system)
    (arguments
     `(#:tests? #f ;; 2/164 tests fail, nitrocli-ext tests failing
       #:cargo-inputs
       (("rust-anyhow" ,rust-anyhow-1)
        ("rust-base32" ,rust-base32-0.4)
        ("rust-directories" ,rust-directories-3)
        ("rust-envy" ,rust-envy-0.4)
        ("rust-libc-0.2" ,rust-libc-0.2)
        ("rust-merge" ,rust-merge-0.1)
        ("rust-nitrokey" ,rust-nitrokey-0.9)
        ("rust-progressing" ,rust-progressing-3)
        ("rust-serde" ,rust-serde-1)
        ("rust-structopt" ,rust-structopt-0.3)
        ("rust-termion" ,rust-termion-1)
        ("rust-toml" ,rust-toml-0.5))
       #:cargo-development-inputs
       (("rust-nitrokey-test" ,rust-nitrokey-test-0.5)
        ("rust-nitrokey-test-state" ,rust-nitrokey-test-state-0.1)
        ("rust-regex" ,rust-regex-1)
        ("rust-tempfile" ,rust-tempfile-3))))
    (inputs
     (list hidapi gnupg))
    (home-page "https://github.com/d-e-s-o/nitrocli")
    (synopsis "Command line tool for Nitrokey devices")
    (description
     "nitrocli is a program that provides a command line interface
for interaction with Nitrokey Pro, Nitrokey Storage, and Librem Key
devices.")
    (license license:gpl3+)))

(define-public ausweisapp
  (package
    (name "ausweisapp")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/Governikus/AusweisApp/releases"
                                  "/download/" version "/AusweisApp-" version ".tar.gz"))
              (sha256
               (base32
                "1rbbgr90ivay1sh8sarp0nd1p8zdyfscjjwg8jdi2ig61jr795zf"))))

    (build-system qt-build-system)
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list qtbase
           qtsvg
           qtscxml
           qtdeclarative
           qtshadertools
           qtwebsockets
           qtgraphicaleffects
           pcsc-lite
           openssl))
    (arguments
     `(#:qtbase ,qtbase
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
             (when tests? (invoke "ctest" "--output-on-failure" "-j"
                                  (if parallel-tests?
                                      (number->string (parallel-job-count))
                                      "1"))))))))
    (home-page "https://github.com/Governikus/AusweisApp")
    (synopsis
     "Authentication program for German ID cards and residence permits")
    (description
     "This application is developed and issued by the German government to be
used for online authentication with electronic German ID cards and residence
titles.  To use this app, a supported RFID card reader or NFC-enabled smart
phone and a running pcscd service are required.")
    (license license:eupl1.2)))

(define-deprecated/public ausweisapp2 ausweisapp
    (deprecated-package "ausweisapp2" ausweisapp))

(define-public libfido2
  (package
    (name "libfido2")
    (version "1.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Yubico/libfido2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256 (base32 "123rysl21bmgk6rmpgg5s21a5ksmxnn1hc32ws88h7z0q4icvj87"))))
    (native-inputs (list pkg-config))
    (inputs (list eudev libcbor openssl zlib))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append
               "-DPKG_CONFIG_EXECUTABLE="
               (search-input-file %build-inputs
                                  (string-append
                                   "/bin/" #$(pkg-config-for-target))))
              (string-append "-DUDEV_RULES_DIR=" #$output "/lib/udev/rules.d"))
      ;; regress tests enabled only for debug builds
      #:tests? #f))
    (synopsis "Library functionality and command-line tools for FIDO devices")
    (description "libfido2 provides library functionality and command-line
tools to communicate with a FIDO device over USB, and to verify attestation
and assertion signatures.

libfido2 supports the FIDO U2F (CTAP 1) and FIDO 2.0 (CTAP 2) protocols.")
    (license license:bsd-2)
    (home-page "https://github.com/Yubico/libfido2")))

(define-public cardpeek
  (package
    (name "cardpeek")
    (version "0.8.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/L1L1/cardpeek")
                    (commit (string-append "cardpeek-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ighpl7nvcvwnsd6r5h5n9p95kclwrq99hq7bry7s53yr57l6588"))))
    (inputs (list curl
                  gtk+
                  lua-5.2
                  openssl
                  pcsc-lite
                  readline))
    (native-inputs (list autoconf
                         automake
                         `(,glib "bin") ;for glib-compile-resources
                         libtool
                         pkg-config))
    (build-system gnu-build-system)
    (synopsis "Tool to read the contents of various smart cards")
    (description
     "Cardpeek is a graphical tool to read the contents of ISO7816 smart cards.
It is extensible with the LUA scripting language.

It supports the following type of cards:
@itemize
@item Bank cards (VISA, MasterCard, CB and UK Post Office Account cards)
@item Passports and the Belgian identity card
@item Transport cards (Navigo, MOBIB, RavKav and VIVA cards)
@item Older GSM SIM cards without USIM data
@item Vitale 2 Health card
@item Moneo Electronic purse card
@item Driver Tachograph cards
@item OpenPGP Cards (beta)
@end itemize
It also has limited support for Mifare Classic compatible cards (Thalys card)")
    (license license:gpl3+)
    (home-page "http://pannetrat.com/Cardpeek")))

(define-public pcsc-cyberjack
  (package
    (name "pcsc-cyberjack")
    (version "3.99.5final.sp15")
    (source
     (origin
       (method url-fetch)
       (uri "https://support.reiner-sct.de/downloads/LINUX/V3.99.5_SP15/pcsc-cyberjack_3.99.5final.SP15.tar.bz2")
       (sha256
        (base32 "0yj6plgb245r218v6lgdabb3422hxyrw8rrpf5b8fwah4j1w5dxc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-usbdropdir=" #$output "/pcsc/drivers")
              (string-append "--bindir=" #$output:tools "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-tools
            (lambda _ (invoke "make" "-C" "tools/cjflash" "install"))))))
    (native-inputs (list pkg-config))
    (inputs (list pcsc-lite libusb))
    (outputs '("out" "tools"))
    (synopsis "PC/SC driver for cyberJack chipcard readers")
    (description
     "This package includes the IFD driver for the cyberJack
contactless (RFID) and contact USB chipcard readers.")
    (home-page "http://www.reiner-sct.com/")
    (license license:lgpl2.1+)))
