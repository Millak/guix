;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2016, 2017, 2018, 2019, 2021, 2022, 2024, 2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2018-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 Andy Patterson <ajpatter@uwaterloo.ca>
;;; Copyright © 2016, 2017, 2018, 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017, 2018, 2020, 2021, 2022, 2023 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2019 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019, 2020 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2020, 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Mason Hock <chaosmonk@riseup.net>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2022 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2020, 2021 Robert Karszniewicz <avoidr@posteo.de>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2023-2024 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2021, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2022 Aleksandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2022, 2023, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Giovanni Biscuolo <g@xelera.eu>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2024 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024, 2025 Igor Goryachev <igor@goryachev.org>
;;; Copyright © 2024 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2025 Evgeny Pisemsky <mail@pisemsky.site>
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

(define-module (gnu packages messaging)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages erlang-xyz)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages less)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages matrix)
  #:use-module (gnu packages mpd)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system go)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system rebar)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public biboumi
  (package
   (name "biboumi")
   (version "9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.louiz.org/biboumi/snapshot/biboumi-"
                                  version ".tar.xz"))
              (sha256
               (base32 "1jvygri165aknmvlinx3jb8cclny6cxdykjf8dp0a3l3228rmzqy"))
              ;; see https://sources.debian.org/patches/biboumi/9.0-5/2001_cmake_ignore_git.patch/
              (patches (search-patches "biboumi-cmake-ignore-git.patch"))))
   (arguments
    ;; Tests seem to partially depend on networking as well as
    ;; louiz/Catch which we remove as a dependency via the patch above as
    ;; the repository seems dead. Deactivating those for now, possibly fix
    ;; some of them later.
    `(#:tests? #f
      #:configure-flags '("-DWITHOUT_SYSTEMD=1")
      #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-substitutions
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("/etc") (string-append (assoc-ref outputs "out") "/etc"))))))))
   (build-system cmake-build-system)
   (inputs (list botan
                 expat
                 libiconv
                 libidn
                 openssl
                 postgresql ;; libpq
                 sqlite
                 ;; TODO: package optional dependency: udns
                 (list util-linux "lib") ;; libuuid
                 pkg-config))
   (home-page "https://biboumi.louiz.org")
   (synopsis "XMPP gateway that connects to IRC")
   (description "Biboumi is a Free, Libre and Open Source XMPP gateway that connects to IRC
servers and translates between the two protocols.  Its goal is to let XMPP
users take part in IRC discussions, using their favourite XMPP client.")
   (license license:zlib)))

(define-public omemo-wget
  (package
    (name "omemo-wget")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/roobre/omemo-wget")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s3vfaicw5xbjl9yiyr4ckrzhzqbvfh1w2ih1igavlfpgw4v7kva"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/roobre/omemo-wget"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((xdg-utils (assoc-ref inputs "xdg-utils"))
                    (xdg-open (string-append xdg-utils "/bin/xdg-open")))
               (substitute* (find-files "." "\\.go$")
                 ;; Correct the import path of 'aesgcm' package.
                 (("roob\\.re/omemo-wget/aesgcm")
                  "github.com/roobre/omemo-wget/aesgcm")
                 ;; Use absolute path of 'xdg-open' program.
                 (("xdg-open") xdg-open))))))))
    (inputs
     (list go-github-com-pkg-errors xdg-utils))
    (home-page "https://roob.re/omemo-wget")
    (synopsis "Program to download and decrypt @code{aesgcm://} URLs")
    (description "OMEMO-wget is a tool to handle cryptographic URLs, generated
by @acronym{OMEMO, OMEMO Multi-End Message and Object Encryption}, during
XMPP-based sessions.")
    (license license:lgpl3+)))

(define-public libgnt
  (package
    (name "libgnt")
    (version "2.14.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/pidgin/libgnt/"
                       version "/libgnt-" version ".tar.xz"))
       (sha256
        (base32 "08v14fjcx2wx6c573wllq015l6zc8qkpz8rrl6qhp7crf9zlbxap"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:glib-or-gtk? #t         ; To wrap binaries and/or compile schemas
           #:configure-flags #~'("-Dpython2=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-ncurses-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "meson.build"
                     (("'/usr'")
                      (string-append "'" #$(this-package-input "ncurses")
                                     "'")))))
               (add-after 'install 'move-doc
                 (lambda _
                   (mkdir-p (string-append #$output:doc "/share"))
                   (rename-file
                    (string-append #$output "/share/gtk-doc")
                    (string-append #$output:doc "/share/gtk-doc")))))))
    (native-inputs
     (list docbook-xml-4.1.2
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           pkg-config))
    (inputs (list libxcrypt ncurses))
    (propagated-inputs (list glib libxml2))
    (synopsis "GLib Ncurses Toolkit")
    (description "GNT is an ncurses toolkit for creating text-mode graphical
user interfaces in a fast and easy way.  It is based on GLib and ncurses.")
    (home-page "https://keep.imfreedom.org/libgnt/libgnt")
    (license license:gpl2+)))

(define-public libgadu
  (package
    (name "libgadu")
    (version "1.12.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/wojtekka/libgadu.git")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1s16cripy5w9k12534qb012iwc5m9qcjyrywgsziyn3kl3i0aa8h"))))
    (build-system gnu-build-system)
    (arguments
     ;; 'test/manual/userconfig.h' contains definitions in lieu of
     ;; declarations, hence '-fcommon'.
     `(#:configure-flags
       (list "--disable-static" "CFLAGS=-O2 -g -fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-shebangs
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "protobufgen.sh"
               (("/bin/sh")
                (search-input-file inputs "/bin/sh"))))))))
    (native-inputs
     (list autoconf
           automake
           bash
           doxygen
           libtool
           perl
           pkg-config))
    (inputs
     `(("curl" ,curl)
       ("expat" ,expat)
       ("libprotobuf-c" ,protobuf-c)
       ("libxml" ,libxml2)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (propagated-inputs
     (list gnutls))
    (synopsis "Library for handling the protocol of Gadu-Gadu")
    (description "LibGadu is library for handling Gadu-Gadu instant messenger
protocol.  The library is written in C and aims to be operating system and
environment independent.")
    (home-page "https://libgadu.net/index.en.html")
    (license license:lgpl2.1+)))

(define-public silc-toolkit
  (package
    (name "silc-toolkit")
    (version "1.1.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/silc/silc/toolkit/sources/silc-toolkit-"
                       version ".tar.gz"))
       (sha256
        (base32 "0mnvf9n7qriadg0p7a8qmvcayhnns2g9fhmcymavlm0v8xrky33y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-ipv6"
        "--enable-stack-trace")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'trigger-bootstrap
           (lambda _
             (delete-file "configure")
             (delete-file "Makefile.in")
             #t)))))
    (native-inputs
     (list autoconf automake libtool perl pkg-config))
    (synopsis "SILC ToolKit")
    (description "SILC (Secure Internet Live Conferencing) is a modern and secure
conferencing protocol.  It provides all the common conferencing services like
private messages, instant messages, channels and groups, and video and audio
conferencing.")
    (home-page "https://silc.github.io/info")
    (license
     ;; Dual-licensed
     (list
      license:gpl2+
      license:bsd-2))))

(define-public qxmpp
  (package
    (name "qxmpp")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qxmpp-project/qxmpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1knpq1jkwk0lxdwczbmzf7qrjvlxba9yr40nbq9s5nqkcx6q1c3i"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DBUILD_EXAMPLES=false"
                               "-DWITH_GSTREAMER=true")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "ctest" "-E"
                       (string-join ;; These tests use the network.
                        (list "tst_qxmppiceconnection"
                              "tst_qxmppcallmanager"
                              "tst_qxmpptransfermanager")
                        "|"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gstreamer qtbase-5))
    (home-page "https://github.com/qxmpp-project/qxmpp")
    (synopsis "XMPP client and server library")
    (description
     "QXmpp is a XMPP client and server library written in C++ and uses the Qt
framework.  It builds XMPP clients complying with the XMPP Compliance Suites
2021 for IM and Advanced Mobile.")
    (license license:lgpl2.1+)))

(define-public meanwhile
  (package
    (name "meanwhile")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/obriencj/meanwhile.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1k1gvmx1ikm0y1mdmm495rzkb00pl170jfaf2dy0n5aiiknkk7q3"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list autoconf automake doxygen libtool pkg-config))
    (propagated-inputs
     (list glib))
    (synopsis "Library for Lotus Instant Messaging")
    (description "Meanwhile is a library for connecting to a LIM (Lotus Instant
Messaging, formerly Lotus Sametime, formerly VPBuddy) community.  It uses a
protocol based in part off of the IMPP draft(*1), and in part off of traces of
TCP sessions from existing clients.")
    (home-page "https://github.com/obriencj/meanwhile")
    (license license:lgpl3)))

(define-public poezio
  (package
    (name "poezio")
    (version "0.14")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://codeberg.org/poezio/poezio")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "15vlmymqlcf94h1g6dvgzjvj15c47dqsm78qs40wl2dlwspvqkxj"))))
    (build-system python-build-system)
    (arguments
      (list #:tests? #f ; tests fails without the OTR plugin
            #:phases
            #~(modify-phases %standard-phases
                (add-after 'unpack 'patch
                  (lambda _
                    (substitute* "setup.py"
                      (("'CC', 'cc'")
                       "'CC', 'gcc'")))))))
    (native-inputs
     (list pkg-config python-setuptools python-sphinx))
    (inputs
     (list python-mpd2
           python-pyasn1
           python-pyasn1-modules
           python-pygments
           python-pyinotify
           python-qrcode
           python-slixmpp
           python-typing-extensions))
    (synopsis "Console Jabber/XMPP Client")
    (description "Poezio is a free console XMPP client (the protocol on which
the Jabber IM network is built).
Its goal is to let you connect very easily (no account creation needed) to the
network and join various chatrooms, immediately.  It tries to look like the
most famous IRC clients (weechat, irssi, etc).  Many commands are identical and
you won't be lost if you already know these clients.  Configuration can be
made in a configuration file or directly from the client.
You'll find the light, fast, geeky and anonymous spirit of IRC while using a
powerful, standard and open protocol.")
    (home-page "https://poez.io/en/")
    (license license:gpl3+)))

(define-public libotr
  (package
    (name "libotr")
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://otr.cypherpunks.ca/libotr-"
                           version ".tar.gz"))
       (sha256
        (base32 "1x8rliydhbibmzwdbyr7pd7n87m2jmxnqkpvaalnf4154hj1hfwb"))
       (modules '((guix build utils)))
       (snippet
        ;; Add missing #include that causes a build failure with glibc 2.35.
        #~(substitute* "tests/regression/client/client.c"
            (("_GNU_SOURCE" all)
             (string-append all "\n#include <sys/socket.h>\n"))))
       (patches
        (search-patches "libotr-test-auth-fix.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))                 ; for the test suite
    (inputs
     (list libgpg-error))
    (propagated-inputs
     (list libgcrypt))    ; libotr headers include gcrypt.h
    (synopsis "Off-the-Record (OTR) Messaging Library and Toolkit")
    (description "OTR allows you to have private conversations over instant
messaging by providing: (1) Encryption: No one else can read your instant
messages.  (2) Authentication: You are assured the correspondent is who you
think it is.  (3) Deniability: The messages you send do not have digital
signatures that are checkable by a third party.  Anyone can forge messages
after a conversation to make them look like they came from you.  However,
during a conversation, your correspondent is assured the messages he sees are
authentic and unmodified.  (4) Perfect forward secrecy: If you lose control of
your private keys, no previous conversation is compromised.")
    (home-page "https://otr.cypherpunks.ca/")
    (license
     (list
      ;; Library
      license:lgpl2.1+
      ;; Others
      license:gpl2+))))

(define-public libsignal-protocol-c
  (package
   (name "libsignal-protocol-c")
   (version "2.3.3")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                   (url "https://github.com/WhisperSystems/libsignal-protocol-c")
                   (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0z5p03vk15i6h870azfjgyfgxhv31q2vq6rfhnybrnkxq2wqzwhk"))))
   (arguments
    `(;; Required for proper linking and for tests to run.
      #:configure-flags '("-DBUILD_SHARED_LIBS=on" "-DBUILD_TESTING=1")))
   (build-system cmake-build-system)
   (inputs (list ;; Required for tests:
                 check openssl))
   (native-inputs (list pkg-config))
   (home-page "https://github.com/WhisperSystems/libsignal-protocol-c")
   (synopsis "Implementation of a ratcheting forward secrecy protocol")
   (description "libsignal-protocol-c is an implementation of a ratcheting
forward secrecy protocol that works in synchronous and asynchronous
messaging environments.  It can be used with messaging software to provide
end-to-end encryption.")
   (license license:gpl3+)))

(define-public libomemo-c
  (package
    (inherit libsignal-protocol-c)
    (name "libomemo-c")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dino/libomemo-c")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1xszd4cjrlwwsy19ri2ymqr676qpqqhxv3cw5zwch3lms68p51hy"))
              (modules '((guix build utils)))
              (snippet
               ;; Delete generated sources.
               #~(for-each delete-file
                           (find-files "." "\\.pb-c\\.[ch]$")))))
    (build-system meson-build-system)
    (arguments (list #:configure-flags #~(list "-Dtests=true")))
    (propagated-inputs (list protobuf-c))
    (home-page "https://github.com/dino/libomemo-c")
    (description "This package provides a fork of libsignal-protocol-c, used
by Dino to provide OMEMO support.")))

(define-public axc
  (package
    (name "axc")
    (version "0.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gkdr/axc")
             (commit (string-append "v" version))))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; Empty directories meant to hold submodules that we provide as
           ;; proper inputs below.
           (delete-file-recursively "lib")))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0b02b9flri374f8aw6xfz7mm9s57rb7393r8mdphv7kcsf76i7i5"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (setenv "CC" "gcc")
                        (setenv "PREFIX" out)))))
       #:parallel-tests? #f))
    (native-inputs (list cmocka pkg-config))
    (inputs (list glib libgcrypt libsignal-protocol-c sqlite))
    (synopsis "Client library for libsignal-protocol-c")
    (description "This is a client library for @code{libsignal-protocol-c}.
It implements the necessary interfaces using @code{libgcrypt} and
@code{sqlite}.")
    (home-page "https://github.com/gkdr/axc")
    (license license:gpl3)))                  ;GPLv3-only, per license headers

(define-public libomemo
  (package
    (name "libomemo")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gkdr/libomemo")
             (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1q3vyj8zk3vm0a4v6w8qya5dhk2yw04bga8799a0zl6907nf122k"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (setenv "CC" "gcc")
                        (setenv "PREFIX" out)))))
       #:parallel-tests? #f))
    (native-inputs (list cmocka pkg-config))
    (inputs (list glib libgcrypt minixml sqlite))
    (synopsis "OMEMO C library")
    (description "This library implements @acronym{OMEMO, OMEMO Multi-End
Message and Object Encryption} of XMPP (XEP-0384) in C.")
    (home-page "https://github.com/gkdr/libomemo")
    (license license:expat)))

(define-public bitlbee
  (package
    (name "bitlbee")
    (version "3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://get.bitlbee.org/src/bitlbee-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0zhhcbcr59sx9h4maf8zamzv2waya7sbsl7w74gbyilvy93dw5cz"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         ;; Note: Change to 'check' for versions > 3.6.
                         check-0.12))
    (inputs (list glib libotr gnutls python perl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-python
           (lambda _ (setenv "PYTHON" (which "python3")) #t))
         (add-after 'install 'install-etc
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-etc" make-flags)))
         (add-after 'install-etc 'install-lib
           (lambda* (#:key (make-flags '()) #:allow-other-keys)
             (apply invoke "make" "install-dev" make-flags)))
         (replace 'configure
           ;; bitlbee's configure script does not tolerate many of the
           ;; variable settings that Guix would pass to it.
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "./configure"
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))
                     "--otr=1"))))))
    (synopsis "IRC to instant messaging gateway")
    (description "BitlBee brings IM (instant messaging) to IRC clients, for
people who have an IRC client running all the time and don't want to run an
additional IM client.  BitlBee currently supports XMPP/Jabber (including
Google Talk), MSN Messenger, Yahoo!  Messenger, AIM and ICQ, and the Twitter
microblogging network (plus all other Twitter API compatible services like
identi.ca and status.net).")
    (home-page "https://www.bitlbee.org/")
    (license (list license:gpl2+ license:bsd-2))))

(define-public bitlbee-purple
  ;; This variant uses libpurple, which provides support for more protocols at
  ;; the expense of a much bigger closure.
  (package/inherit bitlbee
    (name "bitlbee-purple")
    (synopsis "IRC to instant messaging gateway (using Pidgin's libpurple)")
    (inputs (modify-inputs (package-inputs bitlbee)
              (prepend pidgin)))
    (native-search-paths
     (list (search-path-specification
            (variable "PURPLE_PLUGIN_PATH")
            ;; XXX: Should be (version-major (package-version pidgin)) but
            ;; can't due to circular references.
            (files (list (string-append "lib/purple-2")
                         "lib/pidgin")))))
    (arguments
     (substitute-keyword-arguments (package-arguments bitlbee)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (replace 'configure                    ;add "--purple=1"
             (lambda* (#:key outputs #:allow-other-keys)
               (invoke "./configure"
                       (string-append "--prefix="
                                      (assoc-ref outputs "out"))
                       "--otr=1" "--purple=1")))))
       ((#:tests? _ #t)
        ;; XXX: Tests fail to link, and ./configure says that it's "supported
        ;; on a best-effort basis" anyway.
        #f)))))

(define-public bitlbee-discord
  ;; Version 0.4.3 of bitlbee-discord was prepared to work for
  ;; glib@2.68. However, version 2.69 of glib introduced a breaking change
  ;; causing bitlbee-discord to throw:
  ;;
  ;; discord - Login error: Failed to switch to websocket mode
  ;;
  ;; This makes the plugin unable to connect and therefore unusable:
  ;; https://github.com/sm00th/bitlbee-discord/issues/226
  ;; The specified commit fixes incompatibility with glib@2.69 and newer.
  (let ((commit "607f9887ca85f246e970778e3d40aa5c346365a7")
        (revision "1"))
    (package
      (name "bitlbee-discord")
      (version (git-version "0.4.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sm00th/bitlbee-discord")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0jkwhx2walx2ay0vc9x13q0j1qq4r5x30ss03a3j7ks28xvsnxc7"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags
         (let ((out (assoc-ref %outputs "out")))
           (list (string-append "--with-bdatadir=" out "/share/bitlbee/")
                 (string-append "--with-plugindir=" out "/lib/bitlbee/")))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-autogen
             (lambda _
               (let ((sh (which "sh")))
                 (substitute* "autogen.sh" (("/bin/sh") sh))
                 (setenv "CONFIG_SHELL" sh)))))))
      (inputs (list glib))
      (native-inputs (list pkg-config
                           autoconf
                           automake
                           texinfo
                           libtool
                           bitlbee ; needs bitlbee headers
                           bash))
      (synopsis "Discord plugin for Bitlbee")
      (description "Bitlbee-discord is a plugin for Bitlbee which provides
access to servers running the Discord protocol.")
      (home-page "https://github.com/sm00th/bitlbee-discord/")
      (license license:gpl2+))))

(define-public purple-mattermost
  ;; The latest release (1.2) only supports Mattermost's /api/v3.  Choose a
  ;; commit that supports /api/v4.
  (let ((commit "158ce2052af9aaf3d1f6f045f0cfba276e0e91cf")
        (revision "0"))
    (package
      (name "purple-mattermost")
      (version (git-version "1.2" revision commit))
      (home-page "https://github.com/EionRobb/purple-mattermost")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1481zm20pnfq52ncg7hxayjq8cw3a6yh9m4jm1m5s8chsq04015l"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases (modify-phases %standard-phases
                    (replace 'configure
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        ;; Adjust the makefile to install files in the right
                        ;; place.
                        (let ((out (assoc-ref outputs "out")))
                          (substitute* "Makefile"
                            (("MATTERMOST_DEST = .*")
                             (string-append "MATTERMOST_DEST = " out
                                            "/lib/purple-2\n")) ;XXX: hardcoded
                            (("MATTERMOST_ICONS_DEST = .*")
                             (string-append "MATTERMOST_ICONS_DEST = "
                                            out
                                            "/share/pixmaps/pidgin/protocols\n")))
                          #t))))
         #:make-flags (list "CC=gcc"
                            ,(string-append "PLUGIN_VERSION=" version))
         #:tests? #f))
      (inputs (list glib json-glib discount pidgin))
      (native-inputs (list pkg-config))
      (synopsis "Purple plug-in to access Mattermost instant messaging")
      (description
       "Purple-Mattermost is a plug-in for Purple, the instant messaging library
used by Pidgin and Bitlbee, among others, to access
@uref{https://mattermost.com/, Mattermost} servers.")
      (license license:gpl3+))))

(define-public hexchat
  (package
    (name "hexchat")
    (version "2.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/hexchat/hexchat/releases/"
                           "download/v" version "/hexchat-" version ".tar.xz"))
       (sha256
        (base32 "0jhfg6n9r6fn9ld21pdzdz6210d7dms401zcfdrvhx52il53921f"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;need glib-genmarshal
           perl
           pkg-config))
    (inputs
     (list dbus-glib
           dbus
           enchant
           gtk+-2
           libcanberra
           openssl

           ;; Bindings for add-on scripts.
           luajit
           perl-xml-parser
           python
           python-cffi

           ;; For the ensuing WRAP-PROGRAM.
           bash-minimal))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-desktop-database-updates
           (lambda _
             ;; The build scripts update icon and desktop file databases when
             ;; DESTDIR is not set.  We can't update these databases from
             ;; within the build chroot, but we also don't set DESTDIR.  So, we
             ;; just skip this code.
             (substitute* "meson_post_install.py"
               (("if 'DESTDIR' not in os.environ:")
                "if False:"))))
         (add-after 'install 'wrap-program
           ;; Let it ‘initialize the Python-CFFI embedding logic’ at run time.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (wrap-program (string-append bin "/hexchat")
                 `("GUIX_PYTHONPATH" ":" prefix
                   (,(getenv "GUIX_PYTHONPATH"))))))))))
    (synopsis "Graphical IRC client")
    (description
     "HexChat lets you connect to multiple IRC networks at once.  The main
window shows the list of currently connected networks and their channels, the
current conversation and the list of users.  It uses colors to differentiate
between users and to highlight messages.  It checks spelling using available
dictionaries.  HexChat can be extended with multiple addons.")
    (home-page "https://hexchat.net/")
    (license license:gpl2+)))

(define-public ngircd
  (package
    (name "ngircd")
    (version "27")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://arthur.barton.de/pub/ngircd/ngircd-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1f27qa2xg8xwdyij2n5aimdyp24za09rc0f9q5rjwpnx341qi5v8"))))
    (outputs '("out" "debug"))
    (build-system gnu-build-system)
    ;; Needed for the test suite.
    (native-inputs (list procps expect inetutils openssl))
    ;; XXX Add libident.
    (inputs
     (append (list zlib
                   tcp-wrappers
                   gnutls)
             (if (or (target-linux?) (target-hurd?))
                 (list linux-pam)
                 '())))
    (arguments
     (list
      #:configure-flags
      #~(list "--with-gnutls"
              "--with-iconv"
              "--enable-ipv6"
              "--with-tcp-wrappers"
              #$@(if (or (target-linux?) (target-hurd?))
                     #~("--with-pam")
                     #~()))
      #:phases
      #~(modify-phases %standard-phases
          ;; Necessary for the test suite.
          (add-after 'configure 'post-configure
            (lambda _
              (substitute* "src/ngircd/Makefile"
                (("/bin/sh") (which "sh")))
              ;; The default getpid.sh does a sloppy grep over 'ps -ax'
              ;; output, which fails arbitrarily.
              (with-output-to-file "src/testsuite/getpid.sh"
                (lambda ()
                  (display
                   (string-append
                    "#!" (which "sh") "\n"
                    "ps -C \"$1\" -o pid=\n"))))
              ;; Our variant of getpid.sh does not match interpreter names
              ;; when the script's shebang is invoked directly as "./foo".
              ;; Patch cases where the test suite relies on this.
              (substitute* "src/testsuite/start-server.sh"
                ;; It runs 'getpid.sh sh' to test if it works at all.  Run it on
                ;; 'make' instead.
                (("getpid.sh sh") "getpid.sh make")))))))
    (home-page "https://ngircd.barton.de/")
    (synopsis "Lightweight Internet Relay Chat server for small networks")
    (description
     "ngIRCd is a lightweight @dfn{Internet Relay Chat} (IRC) server for small
or private networks.  It is easy to configure, can cope with dynamic IP
addresses, and supports IPv6, SSL-protected connections, as well as PAM for
authentication.")
    (license license:gpl2+)))

(define-public pidgin
  (package
    (name "pidgin")
    (version "2.14.13")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/pidgin/Pidgin/"
                       version "/pidgin-" version ".tar.bz2"))
       (sha256
        (base32 "1a3by4niw5ls67mwgj20p2mr317zj4hzysi5glm9mq0pivf4j00j"))
       (patches
        (search-patches "pidgin-add-search-path.patch"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove stale generated file after applying patches.
           (delete-file "configure")
           #t))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list autoconf ;; For bootstrap
           automake ;; For bootstrap
           check
           graphviz
           gconf
           intltool
           libtool ;; For bootstrap
           pkg-config))
    (inputs
     (list avahi
           cyrus-sasl
           dbus
           dbus-glib
           ;; evolution-data-server
           gnutls
           gstreamer
           ;; gtkspell2
           libgadu
           libgcrypt
           libice
           libidn
           libltdl
           libsm
           libx11
           libxext
           libxml2
           libxscrnsaver
           libxslt
           ;; libzephyr
           meanwhile
           ncurses
           network-manager
           nspr
           nss
           nss-certs
           pango
           perl
           python-wrapper
           python-dbus
           silc-toolkit
           sqlite
           startup-notification
           tcl
           tk))
    (propagated-inputs
     ;; Required by finch.pc, pidgin.pc and purple.pc
     (list glib gtk+-2 libgnt))
    (arguments
     `(#:configure-flags
       (list
        ;; XXX: Disable voice and video calls until Farstream is back to life:
        ;; <https://issues.guix.gnu.org/75739>.
        "--disable-vv"

        "--disable-gtkspell"
        "--disable-gevolution"
        "--enable-cap"
        "--enable-cyrus-sasl"
        ;; Use nss-certs instead of bundled ones.
        (string-append "--with-system-ssl-certs="
                       (assoc-ref %build-inputs "nss-certs")
                       "/etc/ssl/certs")
        (string-append "--with-ncurses-headers="
                       (assoc-ref %build-inputs "ncurses")
                       "/include")
        (string-append "--with-tclconfig="
                       (assoc-ref %build-inputs "tcl")
                       "/lib")
        (string-append "--with-tkconfig="
                       (assoc-ref %build-inputs "tk")
                       "/lib"))))
    (native-search-paths
     (list
      (search-path-specification
       (variable "PURPLE_PLUGIN_PATH")
       (files
        (list
         (string-append "lib/purple-"
                        (version-major version))
         "lib/pidgin")))))
    (home-page "https://www.pidgin.im/")
    (synopsis "Graphical multi-protocol instant messaging client")
    (description "Pidgin is a modular instant messaging client that supports
many popular chat protocols.")
    (license
     (list
      license:gpl2+   ; Most of the code
      license:lgpl2.1 ; GG protocol plugin (libpurple/protocols/gg/lib)
      license:lgpl2.0+ ; OSCAR protocol plugin (libpurple/protocols/oscar)
      ;; The following licenses cover the zephyr protocol plugin:
      (license:non-copyleft
       "file://libpurple/protocols/zephyr/mit-copyright.h"
       "See libpurple/protocols/zephyr/mit-copyright.h in the distribution.")
      (license:non-copyleft
       "file://libpurple/protocols/zephyr/mit-sipb-copyright.h"
       "See libpurple/protocols/zephyr/mit-sipb-copyright.h in the distribution.")))))

(define-public pidgin-otr
  (package
    (name "pidgin-otr")
    (version "4.0.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://otr.cypherpunks.ca/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "1i5s9rrgbyss9rszq6c6y53hwqyw1k86s40cpsfx5ccl9bprxdgl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal intltool pkg-config))
    (inputs
     (list glib
           gtk+-2
           libgcrypt
           libgpg-error
           libotr
           perl
           pidgin))
    (home-page "https://otr.cypherpunks.ca/")
    (synopsis "Off-the-Record Messaging plugin for Pidgin")
    (description "Pidgin-OTR is a plugin that adds support for OTR to the Pidgin
instant messaging client.  OTR (Off-the-Record) Messaging allows you to have
private conversations over instant messaging by providing: (1) Encryption: No
one else can read your instant messages.  (2) Authentication: You are assured
the correspondent is who you think it is.  (3) Deniability: The messages you
send do not have digital signatures that are checkable by a third party.  Anyone
can forge messages after a conversation to make them look like they came from
you.  However, during a conversation, your correspondent is assured the messages
he sees are authentic and unmodified.  (4) Perfect forward secrecy: If you lose
control of your private keys, no previous conversation is compromised.")
    (license license:gpl2+)))

(define-public znc
  (package
    (name "znc")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://znc.in/releases/archive/znc-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0g2gi7207lydmm7zdq52ivw0vhvbnmhsybi89q5m3bcsw60cz9z8"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DWANT_CYRUS=ON"
             "-DWANT_I18N=ON"
             "-DWANT_PERL=ON"
             "-DWANT_PYTHON=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           ;; Copy the googletest sources to where the CMake build expects them.
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((source (assoc-ref inputs "googletest-source"))
                   (target "third_party/googletest"))
               (mkdir-p target)
               (copy-recursively source target)))))))
    (native-inputs
     `(("boost" ,boost)
       ("gettext" ,gettext-minimal)
       ("googletest-source" ,(package-source googletest))
       ("pkg-config" ,pkg-config)))
    (inputs
     ;; FIXME: Package cctz and remove the bundled copy from the source tarball.
     (list argon2
           cyrus-sasl
           icu4c
           openssl
           perl
           python
           zlib))
    (home-page "https://wiki.znc.in/ZNC")
    (synopsis "IRC network bouncer")
    (description "ZNC is an @dfn{IRC network bouncer} or @dfn{BNC}.  It can
detach the client from the actual IRC server, and also from selected channels.
Multiple clients from different locations can connect to a single ZNC account
simultaneously and therefore appear under the same nickname on IRC.")
    (license license:asl2.0)))

(define-public python-nbxmpp
  (package
    (name "python-nbxmpp")
    (version "5.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "nbxmpp" version))
       (sha256
         (base32 "04fnc743d523gb38mm1inii80agmpb9r6hvn3f8ygnj3yq7s2vhn"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: This probably should be an option for pyproject-build-system
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests? (invoke "python" "-m" "unittest" "-v")))))))
    (native-inputs
     (list python-setuptools
           python-wheel))
    (inputs
     (list glib
           glib-networking
           libsoup-minimal
           python-gssapi
           python-idna
           python-packaging
           python-precis-i18n
           python-pygobject))
    (synopsis "Non-blocking XMPP Module")
    (description "Python-nbxmpp is a Python library that provides a way for
Python applications to use the XMPP network.  This library was initially a fork
of xmpppy.")
    (home-page "https://dev.gajim.org/gajim/python-nbxmpp")
    (license license:gpl3+)))

(define-public gajim
  (package
    (name "gajim")
    (version "1.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gajim.org/downloads/"
                           (version-major+minor version)
                           "/gajim-" version ".tar.gz"))
       (sha256
         (base32 "0g2nhy6ypj4jbz216sgiy37spq34bwa0ydn2g73fp9qnxfq4vpvz"))
       (patches
         (search-patches "gajim-honour-GAJIM_PLUGIN_PATH.patch"))))
    (build-system python-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%python-build-system-modules
        (guix build glib-or-gtk-build-system))
      #:modules
      '((guix build python-build-system)
        ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-before 'build 'build-metadata
            (lambda _
              (invoke "./make.py" "build")))
          ;; TODO: Change to pyproject-build-system once it supports
          ;; in-tree build backends.
          (replace 'build
            (lambda _
              (invoke "python" "-m" "build" "--wheel" "--no-isolation"
                      ".")))
          (replace 'install
            (lambda _
              (apply invoke "pip" "--no-cache-dir" "--no-input"
                     "install" "--no-deps" "--prefix" #$output
                     (find-files "dist" "\\.whl$"))))
          (add-after 'install 'install-metadata
            (lambda _
              (invoke "./make.py" "install"
                      (string-append "--prefix=" #$output))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Tests require a running X server.
                (system "Xvfb :1 +extension GLX &")
                (setenv "DISPLAY" ":1")
                ;; For missing '/etc/machine-id'.
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (invoke "dbus-launch" "python" "-m" "unittest"
                        "discover" "-s" "test"))))
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref glib-or-gtk:%standard-phases
                       'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases
                       'glib-or-gtk-wrap))
          (add-after 'install 'wrap-env
            (lambda _
              (for-each
               (lambda (name)
                 (let ((file (string-append #$output "/bin/" name))
                       (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                       (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                       (pixbuf-module-file (getenv "GDK_PIXBUF_MODULE_FILE")))
                   (wrap-program file
                     `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                     `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                     `("GDK_PIXBUF_MODULE_FILE" = (,pixbuf-module-file)))))
               '("gajim" "gajim-remote")))))))
    (native-search-paths
     (list
      (search-path-specification
       (variable "GAJIM_PLUGIN_PATH")
       (separator #f) ; single entry
       (files (list "share/gajim/plugins")))
      ;; Gajim needs to use the propagated inputs of its plugins.
      (search-path-specification
       (variable "GUIX_PYTHONPATH")
       (files
        (list
         (string-append
          "lib/python"
          ;; FIXME: Cannot use this expression as it would
          ;; introduce a circular dependency at the top level.
          ;; (version-major+minor (package-version python))
          "3.10"
          "/site-packages"))))))
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           python-distutils-extra
           python-pypa-build
           python-setuptools
           xorg-server-for-tests))
    (inputs
     (list bash-minimal
           avahi
           dbus
           geoclue
           glib
           glib-networking
           gsettings-desktop-schemas
           gsound
           gspell
           gstreamer
           gst-plugins-base
           gtk+
           gtksourceview-4
           gupnp-igd
           libappindicator
           libnice
           libomemo
           libsecret
           libsoup
           libxscrnsaver
           network-manager
           python-css-parser
           python-dbus
           python-emoji
           python-gssapi
           python-idna
           python-keyring
           python-nbxmpp
           python-omemo-dr
           python-packaging
           python-pillow
           python-precis-i18n
           python-pycairo
           python-pygobject
           python-pyopenssl
           python-qrcode
           python-sqlalchemy-2))
    (propagated-inputs
     (list dconf))
    (synopsis "Fully-featured XMPP client")
    (description "Gajim aims to be an easy to use and fully-featured XMPP chat
client.  It is extensible via plugins, supports end-to-end encryption (OMEMO
and OpenPGP) and available in 29 languages.")
    (home-page "https://gajim.org/")
    (license license:gpl3)))

(define-public gajim-omemo
  (deprecated-package "gajim-omemo" gajim))

(define-public gajim-openpgp
  (package
    (name "gajim-openpgp")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch/zipbomb)
       (uri
        (string-append
         "https://ftp.gajim.org/plugins/master/openpgp/openpgp_"
         version ".zip"))
       (sha256
        (base32 "0m1g5wajpc3kfz5jv8y3i9xy1nqhq15ripv49lgsq7j1f0a3w3wh"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (share (in-vicinity out "share/gajim/plugins/openpgp"))
                (source (assoc-ref %build-inputs "source")))
           (mkdir-p share)
           (copy-recursively source share)
           #t))))
    (propagated-inputs
     (list python-cryptography python-gnupg python-gpg))
    (synopsis "Gajim OpenPGP plugin")
    (description "Gajim-OpenPGP is a plugin that adds support for the OpenPGP
Encryption to Gajim.")
    (home-page "https://dev.gajim.org/gajim/gajim-plugins/-/wikis/OpenPGPplugin")
    (license license:gpl3+)))

(define-public dino
  (package
    (name "dino")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/dino/dino/releases/download/v"
                       version "/dino-" version ".tar.gz"))
       (sha256
        (base32 "1hghyldh95i6sx778nkbmfn5qbi2h7qpv59vzi7zz9anmxgjckli"))))
    (build-system meson-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list #:glib-or-gtk? #t
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'hardcode-version
                 (lambda _
                   ;; XXX: the meson.build code to locate the version script
                   ;; is wrong and raises an error.
                   (substitute* "libdino/src/version.vala.in"
                     (("%VERSION%") #$version))))
               (add-after 'install 'wrap
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (dino (string-append out "/bin/dino"))
                          (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                     (wrap-program dino
                       `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                       `("GDK_PIXBUF_MODULE_FILE" =
                         (,(getenv "GDK_PIXBUF_MODULE_FILE"))))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk "bin")
           pkg-config
           ;; python                       ; for version.py
           vala))
    (inputs
     (list adwaita-icon-theme
           at-spi2-core
           bash-minimal
           cairo
           (librsvg-for-system)
           glib
           glib-networking
           gpgme
           gsettings-desktop-schemas
           gspell                       ;for spell-check support
           gstreamer                    ;for A/V support
           gst-plugins-base
           gst-plugins-good
           gtk
           icu4c                        ;for emoji support
           libadwaita
           libcanberra                  ;for sound-notification support
           libgcrypt
           libgee
           libnice
           libomemo-c
           libsoup
           libsrtp                      ;for calls support
           pango                        ;gtk4 wants pango 1.50+
           qrencode
           sqlite
           webrtc-audio-processing))    ;for A/V support
    (synopsis "Graphical Jabber/XMPP Client using GTK+/Vala")
    (description "Dino is a chat client for the desktop.  It focuses on providing
a minimal yet reliable Jabber/XMPP experience and having encryption enabled by
default.")
    (home-page "https://dino.im")
    (license license:gpl3+)))

(define-public kaidan
  (package
    (name "kaidan")
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/unstable/kaidan/" version
                                  "/kaidan-" version ".tar.xz"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file-recursively "3rdparty")))
              (sha256
               (base32 "1waqv0fdkhvp3cqy2a2g6i2wc9s0zbvgzknymrwxy99mnx9ymw9g"))))
    (build-system qt-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_TESTS=true")))
    (native-inputs (list extra-cmake-modules
                         perl
                         pkg-config
                         python-wrapper))
    (inputs (list kirigami-5
                  knotifications-5
                  qtbase-5
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtlocation-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qtmultimedia-5
                  qtxmlpatterns
                  qqc2-desktop-style
                  qxmpp
                  sonnet
                  zxing-cpp))
    (home-page "https://www.kaidan.im/")
    (synopsis "Qt-based XMPP/Jabber Client")
    (description "Kaidan is a chat client.  It uses the open communication
protocol XMPP (Jabber).  The user interface makes use of Kirigami and QtQuick,
while the back-end of Kaidan is entirely written in C++ using Qt and the
Qt-based XMPP library QXmpp.")
    (license (list
              ;; Graphics
              license:cc-by-sa4.0
              ;; Files:
              ;; src/{StatusBar.cpp|StatusBar.h|singleapp/*|hsluv-c/*}
              ;; utils/generate-license.py
              license:expat
              ;; QrCodeVideoFrame
              license:asl2.0
              ;; Others
              license:gpl3+))))

(define-public prosody
  (package
    (name "prosody")
    (version "0.12.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://prosody.im/downloads/source/"
                                  "prosody-" version ".tar.gz"))
              (sha256
               (base32
                "0mjqss1h2cw0nlyj9nkxdg1bnq1j0zndlv1g8665aa9g7hki5ms7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;tests require "busted"
       #:configure-flags (list "--no-example-certs")
       #:modules ((ice-9 match)
                  (srfi srfi-1)
                  (guix build gnu-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure-script
           (lambda _
             ;; The configure script aborts when it encounters unexpected
             ;; arguments.  Make it more tolerant.
             (substitute* "configure"
               (("exit 1") ""))))
         (add-after 'unpack 'fix-makefile
           (lambda _
             (substitute* "GNUmakefile"
               ;; prosodyctl needs to read the configuration file.
               (("^INSTALLEDCONFIG =.*") "INSTALLEDCONFIG = /etc/prosody\n")
               ;; prosodyctl needs a place to put auto-generated certificates.
               (("^INSTALLEDDATA =.*") "INSTALLEDDATA = /var/lib/prosody\n"))))
         (add-after 'unpack 'invoke-prosody-wrapper
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Invoke the prosody wrapper script instead of invoking lua on
             ;; the actual executable.
             (substitute* "util/prosodyctl.lua"
               (("os.execute\\(lua[^;]*")
                (string-append "os.execute(\""
                               (assoc-ref outputs "out")
                               "/bin/prosody -D\")")))))
         (add-after 'install 'wrap-programs
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure all executables in "bin" find the required Lua
             ;; modules at runtime.
             (let* ((out   (assoc-ref outputs "out"))
                    (bin   (string-append out "/bin/"))
                    (deps  (delete #f (map (match-lambda
                                             ((label . directory)
                                              (if (string-prefix? "lua" label)
                                                  directory #f)))
                                           inputs)))
                    (lua-path (string-join
                               (map (lambda (path)
                                      (string-append
                                       path "/share/lua/5.2/?.lua;"
                                       path "/share/lua/5.2/?/?.lua"))
                                    (cons out deps))
                               ";"))
                    (lua-cpath (string-join
                                (map (lambda (path)
                                       (string-append
                                        path "/lib/lua/5.2/?.so;"
                                        path "/lib/lua/5.2/?/?.so"))
                                     (cons out deps))
                                ";"))
                    (openssl (assoc-ref inputs "openssl"))
                    (coreutils (assoc-ref inputs "coreutils"))
                    (path (map (lambda (dir)
                                 (string-append dir "/bin"))
                               (list openssl coreutils))))
               (for-each (lambda (file)
                           (wrap-program file
                             `("LUA_PATH"  ";" = (,lua-path))
                             `("LUA_CPATH" ";" = (,lua-cpath))
                             `("PATH" ":" prefix ,path)))
                         (find-files bin ".*"))))))))
    (inputs
     (list bash-minimal
           icu4c
           libidn
           openssl
           lua-5.2
           lua5.2-bitop
           lua5.2-expat
           lua5.2-socket
           lua5.2-filesystem
           lua5.2-sec))
    (home-page "https://prosody.im/")
    (synopsis "Jabber (XMPP) server")
    (description "Prosody is a modern XMPP communication server.  It aims to
be easy to set up and configure, and efficient with system resources.
Additionally, for developers it aims to be easy to extend and give a flexible
system on which to rapidly develop added functionality, or prototype new
protocols.")
    (license license:x11)))

(define (prosody-module module-name)
  (let ((changeset "66e7d46b1d4b")
        (revision "3")
        (package-name (string-append
                       "prosody-"
                       (string-replace-substring
                        (if (string-prefix? "mod_" module-name)
                            (substring module-name 4)
                            module-name)
                        "_" "-"))))
    (package
      (name package-name)
      (version (string-append "0-" revision "." (string-take changeset 7)))
      (source (origin
                (method hg-fetch)
                (uri (hg-reference
                      (url "https://hg.prosody.im/prosody-modules/")
                      (changeset changeset)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0wyxvbf335jaaz850m2q6jj6ix4hjlhlh28kzk7462qa9fcw5p7s"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '((,(string-append module-name "/") "."))))
      (home-page (string-append "https://modules.prosody.im/"
                                module-name ".html"))
      (synopsis #f)
      (description #f)
      (license (package-license prosody)))))

(define-public prosody-cloud-notify
  (package
    (inherit (prosody-module "mod_cloud_notify"))
    (synopsis "XEP-0357: Push Notifications")
    (description "This module implements XEP-0357: Push Notifications.

Some platforms, notably Apple’s iOS and many versions of Android, impose
limits that prevent applications from running or accessing the network in the
background.  This makes it difficult or impossible for an XMPP application to
remain reliably connected to a server to receive messages.

In order for messaging and other apps to receive notifications, the OS vendors
run proprietary servers that their OS maintains a permanent connection to in
the background.  Then they provide APIs to application developers that allow
sending notifications to specific devices via those servers.

When you connect to your server with a client that requires push
notifications, it will use this module to set up a “push registration”.  When
you receive a message but your device is not connected to the server, this
module will generate a notification and send it to the push gateway operated
by your application’s developers).  Their gateway will then connect to your
device’s OS vendor and ask them to forward the notification to your device.
When your device receives the notification, it will display it or wake up the
app so it can connect to XMPP and receive any pending messages.")))

(define-public prosody-cloud-notify-encrypted
  (package
    (inherit (prosody-module "mod_cloud_notify_encrypted"))
    (propagated-inputs (list lua5.2-ossl))
    (synopsis "Custom extension to XEP-0357: Push Notifications")
    (description "This module implements support for a Encrypted Push
Notifications, a custom extension to XEP-0357: Push Notifications.")))

(define-public prosody-cloud-notify-filters
  (package
    (inherit (prosody-module "mod_cloud_notify_filters"))
    (synopsis "Filters for XEP-0357: Push Notifications")
    (description "This module implements support for a group of push
notification extensions by the Tigase team that allow a client to specify
filters to be applied to push notifications.  It is a custom extension to
XEP-0357: Push Notifications.")))

(define-public prosody-cloud-notify-priority-tag
  (package
    (inherit (prosody-module "mod_cloud_notify_priority_tag"))
    (synopsis "Tigase priorities for XEP-0357: Push Notifications")
    (description "This module implements support for a Tigase XMPP extension,
Priority of notifications.  It is a custom extension to XEP-0357: Push
Notifications.")))

(define-public prosody-http-upload
  (package
    (inherit (prosody-module "mod_http_upload"))
    (synopsis "XEP-0363: Allow clients to upload files over HTTP")
    (description "This module implements XEP-0363: it allows clients to
upload files over HTTP.")))

(define-public prosody-muc-offline-delivery
  (package
    (inherit (prosody-module "mod_muc_offline_delivery"))
    (synopsis "Deliver MUC messages to users who are not in the room")
    (description "This module implements support for sending messages in a MUC
to affiliated users who are not in the room.  This is a custom extension by
Tigase to allow push notifications from MUCs to users who are not currently
connected.")))

(define-public prosody-smacks
  (package
    (inherit (prosody-module "mod_smacks"))
    (synopsis "XEP-0198: Reliability and fast reconnects for XMPP")
    (description "This module implements XEP-0198: when supported by both
the client and server, it can allow clients to resume a disconnected session,
and prevent message loss.")))

(define-public prosody-vcard-muc
  (package
    (inherit (prosody-module "mod_vcard_muc"))
    (synopsis "Support for MUC vCards and avatars")
    (description "This module adds the ability to set vCard for MUC rooms. One
of the most common use cases is to define avatars for MUC rooms.")))

(define-public libtoxcore
  (let ((revision "2")
        (commit "bf69b54f64003d160d759068f4816b2d9b2e1e21"))
    (package
      (name "libtoxcore")
      (version (string-append "0.0.0" "-"
                              revision "."(string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/irungentoo/toxcore")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "11lqra4yd7v293cp286ynff5lqz1pprzg8vn3wq6vryj08g88zqb"))))
      (build-system gnu-build-system)
      (arguments `(#:tests? #f)) ; FIXME: tests hang, some fail.
      (native-inputs
       (list autoconf automake libtool check pkg-config))
      (inputs
       (list libsodium opus libvpx))
      (synopsis "Library for the Tox encrypted messenger protocol")
      (description
       "C library implementation of the Tox encrypted messenger protocol.")
      (license license:gpl3+)
      (home-page "https://tox.chat"))))

;; Some tox clients move to c-toxcore, which seems to be where all the
;; recent development happens. It is run by the same developers as toxcore,
;; forked into a group namespace.
(define-public c-toxcore
  (package
    (name "c-toxcore")
    (version "0.2.19")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/TokTok/c-toxcore")
             (commit (string-append "v" version))
             ;; XXX: c-toxcore now depends on a package called 'cmp', an
             ;; implementation of MessagePack in C.  Fetch the submodule
             ;; for now, maybe package it later.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0wq6grc5lfjip39gm0ji1cw6b1sdv1zvimg1g40haqzhj51755za"))))
    (arguments
     (list #:tests? #f ; figure out how to run the tests
           #:configure-flags #~(list "-DENABLE_STATIC=false")))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list libsodium opus libvpx))
    (home-page "https://tox.chat")
    (synopsis "Library for the Tox encrypted messenger protocol")
    (description
     "Official fork of the C library implementation of the Tox encrypted
messenger protocol.")
    (license license:gpl3+)))

(define-public utox
  (package
    (name "utox")
    (version "0.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uTox/uTox")
             (commit (string-append "v" version))
             (recursive? #t))) ;; Needed for 'minini' git submodule.
       (file-name (string-append name "-" version "-checkout"))
       (sha256
        (base32
         "01rvlf94d4rkrygnnjak3cg16hrrqyi1rn9nx65y17qk2nbyh68g"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_TESTS=on")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'patch-absolute-filename-libgtk-3
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "../source/src/xlib/gtk.c"
               (("libgtk-3.so")
                (search-input-file inputs "/lib/libgtk-3.so")))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/utox")
               ;; For GtkFileChooserDialog.
               `("GSETTINGS_SCHEMA_DIR" =
                 (,(string-append (assoc-ref inputs "gtk+")
                                  "/share/glib-2.0/schemas")))))))))
    (inputs
     (list bash-minimal                 ;for wrap-program
           dbus
           filteraudio
           fontconfig
           freetype
           c-toxcore
           gtk+
           libvpx
           libx11
           libxext
           libxrender
           openal
           v4l-utils))
    (native-inputs
     (list check pkg-config))
    (synopsis "Lightweight Tox client")
    (description
     "uTox is a lightweight Tox client.  Tox is a distributed and secure
instant messenger with audio and video chat capabilities.")
    (home-page "https://github.com/uTox/uTox")
    (license license:gpl3)))

(define-public qtox
  (package
    (name "qtox")
    (version "1.17.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/qTox/qTox/releases"
                                  "/download/v" version
                                  "/v" version ".tar.gz"))
              (sha256
               (base32
                "1ml8z1xpp3qhip4vkr375jf7y5kc18g0apm91n5am6ricx37c01r"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-reproducibility-issues
                 (lambda _
                   (substitute* "src/main.cpp"
                     (("__DATE__") "\"\"")
                     (("__TIME__") "\"\"")
                     (("TIMESTAMP") "\"\""))))
               (add-after 'unpack 'disable-network-tests
                 (lambda _
                   ;; These tests require network access.
                   (substitute* "cmake/Testing.cmake"
                     (("auto_test\\(core core\\)") "# auto_test(core core)")
                     (("auto_test\\(net bsu\\)") "# auto_test(net bsu)"))))
               ;; Ensure that icons are found at runtime.
               (add-after 'install 'wrap-executable
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (wrap-program (string-append out "/bin/qtox")
                       `("QT_PLUGIN_PATH" prefix
                         ,(list (search-input-directory
                                 inputs "lib/qt5/plugins/"))))))))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list bash-minimal
           ffmpeg
           filteraudio
           glib
           gtk+-2
           libsodium
           c-toxcore
           libvpx
           libxscrnsaver
           libx11
           libexif
           sqlite
           openal
           qrencode
           qtbase-5
           qtsvg-5
           sqlcipher))
    (home-page "https://qtox.github.io/")
    (synopsis "Tox chat client using Qt")
    (description "qTox is a Tox client that follows the Tox design
guidelines.  It provides an easy to use application that allows you to
connect with friends and family without anyone else listening in.")
    (license license:gpl3+)))

(define-public ytalk
  (package
    (name "ytalk")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "ftp://ftp.ourproject.org/pub/ytalk/ytalk-"
                           version  ".tar.gz"))
       (sha256
        (base32
         "1d3jhnj8rgzxyxjwfa22vh45qwzjvxw1qh8fz6b7nfkj3zvk9jvf"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses))
    (home-page "https://ytalk.ourproject.org")
    (synopsis "Multi-user chat program")
    (description "Ytalk is a replacement for the BSD talk program.  Its main
advantage is the ability to communicate with any arbitrary number of users at
once.  It supports both talk protocols (\"talk\" and \"ntalk\") and can communicate
with several different talk daemons at the same time.")
    (license license:gpl2+)))

(define-public gloox
  (package
    (name "gloox")
    (version "1.0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://camaya.net/download/gloox-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1jgrd07qr9jvbb5hcmhrqz4w4lvwc51m30jls1fgxf1f5az6455f"))))
    (build-system gnu-build-system)
    (inputs
     (list libidn gnutls zlib))
    (native-inputs
     (list pkg-config))
    (synopsis "Portable high-level Jabber/XMPP library for C++")
    (description
     "gloox is a full-featured Jabber/XMPP client library,
written in ANSI C++.  It makes writing spec-compliant clients easy
and allows for hassle-free integration of Jabber/XMPP functionality
into existing applications.")
    (home-page "https://camaya.net/gloox")
    (license license:gpl3)))

(define-public perl-net-psyc
  (package
    (name "perl-net-psyc")
    (version "1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://perl.psyc.eu/"
                           "perlpsyc-" version ".zip"))
       (file-name (string-append name "-" version ".zip"))
       (sha256
        (base32
         "0vsjclglkwgbyd9m5ad642fyysxw2x725nhq4r2m9pvqaq6s5yf2"))))
    (build-system perl-build-system)
    (native-inputs
     (list unzip))
    (inputs
     (list bash-minimal perl-curses perl-io-socket-ssl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure) ; No configure script
         ;; There is a Makefile, but it does not install everything
         ;; (leaves out psycion) and says
         ;; "# Just to give you a rough idea". XXX: Fix it upstream.
         (replace 'build
           (lambda _ (invoke "make" "manuals")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/perl-net-psyc"))
                    (man1 (string-append out "/share/man/man1"))
                    (man3 (string-append out "/share/man/man3"))
                    (bin (string-append out "/bin"))
                    (libpsyc (string-append out "/lib/psyc/ion"))
                    (libperl (string-append out "/lib/perl5/site_perl/"
                                            ,(package-version perl))))

               (copy-recursively "lib/perl5" libperl)
               (copy-recursively "lib/psycion" libpsyc)
               (copy-recursively "bin" bin)
               (install-file "cgi/psycpager" (string-append doc "/cgi"))
               (copy-recursively "contrib" (string-append doc "/contrib"))
               (copy-recursively "hooks" (string-append doc "/hooks"))
               (copy-recursively "sdj" (string-append doc "/sdj"))
               (install-file "README.txt" doc)
               (install-file "TODO.txt" doc)
               (copy-recursively "share/man/man1" man1)
               (copy-recursively "share/man/man3" man3))))
         (add-after 'install 'wrap-programs
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure all executables in "bin" find the Perl modules
             ;; provided by this package at runtime.
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin/"))
                    (path (getenv "PERL5LIB")))
               (for-each (lambda (file)
                           (wrap-program file
                             `("PERL5LIB" ":" prefix (,path))))
                         (find-files bin "\\.*$"))))))))
    (description
     "@code{Net::PSYC} with support for TCP, UDP, Event.pm, @code{IO::Select} and
Gtk2 event loops.  This package includes 12 applications and additional scripts:
psycion (a @uref{https://about.psyc.eu,PSYC} chat client), remotor (a control console
for @uref{https://torproject.org,tor} router) and many more.")
    (synopsis "Perl implementation of PSYC protocol")
    (home-page "https://perl.psyc.eu")
    (license (list license:gpl2
                   license:perl-license
                   ;; contrib/irssi-psyc.pl:
                   license:public-domain
                   ;; bin/psycplay states AGPL with no version:
                   license:agpl3+))))

(define-public libpsyc
  (package
    (name "libpsyc")
    (version "20160913")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.psyced.org/files/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "14q89fxap05ajkfn20rnhc6b1h4i3i2adyr7y6hs5zqwb2lcmc1p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl netcat procps))
    (arguments
     `(#:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; The rust bindings are the only ones in use, the lpc bindings
         ;; are in psyclpc.  The other bindings are not used by anything,
         ;; the chances are high that the bindings do not even work,
         ;; therefore we do not include them.
         ;; TODO: Get a cargo build system in Guix.
         (delete 'configure)))) ; no configure script
    (home-page "https://about.psyc.eu/libpsyc")
    (description
     "@code{libpsyc} is a PSYC library in C which implements
core aspects of PSYC, useful for all kinds of clients and servers
including psyced.")
    (synopsis "PSYC library in C")
    (license license:agpl3+)))

(define-public loudmouth
  (package
    (name "loudmouth")
    (version "1.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mcabber.com/files/loudmouth/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "03adv5xc84l9brcx0dpyqyffmsclans8yfrpnd357k6x3wfckjri"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))
       #:make-flags
       (list
        "CFLAGS=-Wno-error=all")))
    (inputs
     `(("glib" ,glib)
       ("gnutls" ,gnutls)
       ("krb5" ,mit-krb5)
       ("libidn" ,libidn)))
    (native-inputs
     (list pkg-config check
           `(,glib "bin") ; gtester
           gtk-doc/stable))
    (home-page "https://mcabber.com/")
    (description
     "Loudmouth is a lightweight and easy-to-use C library for programming
with the XMPP (formerly known as Jabber) protocol.  It is designed to be
easy to get started with and yet extensible to let you do anything the XMPP
protocol allows.")
    (synopsis "Asynchronous XMPP library")
    ;; The files have LGPL2.0+ headers, but COPYING specifies LGPL2.1.
    (license license:lgpl2.0+)))

(define-public mcabber
  (package
    (name "mcabber")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mcabber.com/files/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32 "0q1i5acyghsmzas88qswvki8kkk2nfpr8zapgnxbcd3lwcxl38f4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-otr"
        "--enable-enchant"
        "--enable-aspell")))
    (inputs
     (list gpgme
           libotr
           aspell
           enchant-1.6
           libidn
           glib
           ncurses
           loudmouth))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)))
    (home-page "https://mcabber.com")
    (description
     "Mcabber is a small XMPP (Jabber) console client, which includes features
such as SASL and TLS support, @dfn{Multi-User Chat} (MUC) support, logging,
command-completion, OpenPGP encryption, @dfn{Off-the-Record Messaging} (OTR)
support, and more.")
    (synopsis "Small XMPP console client")
    (license license:gpl2+)))

(define-public freetalk
  (package
    (name "freetalk")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/freetalk/freetalk-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "105mw7pg2mcp85r82cs4rv77nwvbw8025047364jzbq6lwllynxv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (wrap-program (string-append out "/bin/freetalk")
                       `("PATH" ":" suffix
                         ,(map (lambda (command)
                                 (dirname
                                  (search-input-file
                                   inputs (string-append "bin/" command))))
                               ;; This list is not exhaustive: we assume that,
                               ;; e.g., cat is packaged with other coreutils.
                               (list "bash" ; src/{commands,util}.c et al
                                     "cat"  ; extensions/first-time-run.sh
                                     "less")))))))))) ; extensions/history.scm.
    (native-inputs
     (list autoconf automake pkg-config texinfo))
    (inputs
     (list bash
           glib
           guile-3.0
           less
           loudmouth
           readline))
    (synopsis "Extensible console-based Jabber client")
    (description
     "GNU Freetalk is a command-line Jabber/XMPP chat client.  It notably uses
the Readline library to handle input, so it features convenient navigation of
text as well as tab-completion of buddy names, commands and English words.  It
is also scriptable and extensible via Guile.")
    (home-page "https://www.gnu.org/software/freetalk/")
    (license license:gpl3+)))

(define-public libstrophe
  (package
    (name "libstrophe")
    (version "0.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strophe/libstrophe")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ih28ys8nsk66n6x9s31khc946q35rma90fgrq0jvxxhgj2bqwz7"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags '(list "--disable-static")
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'unpack 'patch-make
           (lambda _
             (substitute* "Makefile.am"
               (("'\\^xmpp_'") "'.'"))))
         (add-after 'install 'install-extra-licence-files
           (lambda _
            (let ((license-directory (string-append #$output
                                                    "/share/doc/"
                                                    #$name "-" #$version "/")))
              (install-file "MIT-LICENSE.txt" license-directory)))))))
    (inputs
     (list expat openssl zlib))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (synopsis "C library for writing XMPP clients")
    (description "Libstrophe is a minimal XMPP library written in C.  It has
almost no external dependencies, only an XML parsing library (expat or libxml
are both supported).")
    (home-page "https://strophe.im/libstrophe/")
    ;; Dual-licensed.
    (license (list license:gpl3+ license:x11))))

(define-public profanity
  (package
    (name "profanity")
    (version "0.15.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://profanity-im.github.io/tarballs/profanity-"
                       version ".tar.gz"))
       (sha256
        (base32
         "1yy7x9ycqg6c65k66z47p8mvj48qc0pa4as1lk1agj8ffn7mg7sa"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
       #:configure-flags
         #~(list
           "--disable-static"
           "--enable-notifications"
           "--enable-python-plugins"
           "--enable-c-plugins"
           "--enable-plugins"
           "--enable-otr"
           "--enable-pgp"
           "--enable-omemo"
           "--enable-icons-and-clipboard")))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           cmocka
           libtool
           pkg-config))
    (inputs
     (list curl
           expat
           glib
           gpgme
           gtk+-2
           libgcrypt
           libnotify
           libotr
           libsignal-protocol-c
           libstrophe
           ncurses
           openssl
           python-wrapper
           readline
           sqlite))
    (synopsis "Console-based XMPP client")
    (description "Profanity is a console based XMPP client written in C
using ncurses and libmesode, inspired by Irssi.")
    (home-page "https://profanity-im.github.io")
    (properties `((release-monitoring-url . ,home-page)))
    (license license:gpl3+)))

(define-public libircclient
  (package
    (name "libircclient")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/libircclient/libircclient/"
                           version "/libircclient-" version ".tar.gz"))
       (sha256
        (base32
         "0b9wa0h3xc31wpqlvgxgnvqp5wgx3kwsf5s9432m5cj8ycx6zcmv"))))
    (build-system gnu-build-system)
    (inputs
     (list openssl))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir="
                            (assoc-ref %outputs "out") "/lib")
             "--enable-shared"
             "--enable-ipv6"
             "--enable-openssl")
       #:tests? #f))                    ; no test suite
    (home-page "https://www.ulduzsoft.com/libircclient/")
    (synopsis "Library implementing the client IRC protocol")
    (description "Libircclient is a library which implements the client IRC
protocol.  It is designed to be small, fast, portable and compatible with the
RFC standards as well as non-standard but popular features.  It can be used for
building the IRC clients and bots.")
    (license license:lgpl3+)))

(define-public toxic
  (package
    (name "toxic")
    (version "0.15.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JFreegman/toxic")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1cbgw9my7nd8b215a3db2jc74nibi9kj0yk5q3c9dnh306as6wzs"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                      ; no tests
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-before 'build 'enable-python-scripting
                 (lambda _
                   ;; XXX: For compatibility with Python 3.8, adjust
                   ;; python3-config invocation to include --embed; see
                   ;; <https://github.com/JFreegman/toxic/issues/533>.
                   (substitute* "cfg/checks/python.mk"
                     (("python3-config --ldflags")
                      "python3-config --ldflags --embed"))
                   (setenv "ENABLE_PYTHON" "1"))))))
    (inputs
     (list c-toxcore
           curl
           freealut
           gdk-pixbuf ; for libnotify.pc
           libconfig
           libnotify
           libpng
           libvpx
           libx11
           ncurses
           openal
           python
           qrencode))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/JFreegman/toxic")
    (synopsis "Tox chat client using ncurses")
    (description "Toxic is a console-based instant messaging client, using
c-toxcore and ncurses.  It provides audio calls, sound and desktop
notifications, and Python scripting support.")
    (license license:gpl3+)))

(define-public libqmatrixclient
  (package
    (name "libqmatrixclient")
    (version "0.6.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/quotient-im/libQuotient")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "072d3irpdd0p4w77s5pp0baqf74hk7vqggw7ic7i42lzjdwp3yql"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5 qtmultimedia-5))
    (arguments
     `(#:configure-flags (list "-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                    ; no tests
    (home-page "https://matrix.org/docs/projects/sdk/libqmatrixclient.html")
    (synopsis "Qt5 client library for the Matrix instant messaging protocol")
    (description "libqmatrixclient is a Qt5 library to write clients for the
Matrix instant messaging protocol.  Quaternion is the reference client
implementation.  Quaternion and libqmatrixclient together form the
QMatrixClient project.")
    (license license:lgpl2.1+)))

(define-public mtxclient
  (package
    (name "mtxclient")
    (version "0.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nheko-Reborn/mtxclient")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10iidyxjk3w6cljw2r62i5azx84nw3p8hw97d8vy7r5gh1nrrrcn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list  "-DBUILD_LIB_EXAMPLES=OFF") ; disable example binaries (not installed)
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'disable-network-tests
            (lambda _
              (substitute* "CMakeLists.txt"
                (("add_test\\((BasicConnectivity|ClientAPI|Devices|MediaAPI|Encryption|Pushrules)")
                 "# add_test")))))))
    (inputs
     (list boost
           coeurl
           curl
           nlohmann-json
           libevent
           libsodium
           olm
           openssl
           re2
           spdlog-1.13
           zlib))
    (native-inputs
     (list googletest pkg-config))
    (home-page "https://github.com/Nheko-Reborn/mtxclient")
    (synopsis "Client API library for the Matrix protocol")
    (description "@code{mtxclient} is a C++ library that implements client API
for the Matrix protocol.  It is built on to of @code{Boost.Asio}.")
    (license license:expat)))

(define-public nheko
  (package
    (name "nheko")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nheko-Reborn/nheko")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "113ids1k2pjmvs9cgh025vkpg5mipw295dlkx7n3ydi0r8mzw1l5"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "third_party"))))
    (arguments
     (list
      #:tests? #f                       ;no test target
      #:qtbase qtbase
      #:configure-flags
      #~(list "-DCMAKE_BUILD_TYPE=Release"
              ;; Fix required because we are using a static SingleApplication
              "-DCMAKE_CXX_FLAGS= \"-DQAPPLICATION_CLASS=QApplication\" "
              ;; Compile Qml will make Nheko faster, but you will need to recompile
              ;; it, when you update Qt.  That's fine for us.
              "-DCOMPILE_QML=ON"
              ;; Use system libraries.
              "-DUSE_BUNDLED_BLURHASH=OFF"
              "-DUSE_BUNDLED_CPPHTTPLIB=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-determinism
            (lambda _
              ;; Make Qt deterministic.
              (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")))
          (add-after 'install 'wrap-program
            (lambda _
              (let ((gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                (wrap-program (string-append #$output "/bin/nheko")
                  `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))))))))
    (build-system qt-build-system)
    (inputs
     (list bash-minimal
           blurhash
           brotli
           cmark
           coeurl
           cpp-httplib
           curl
           gst-plugins-base
           gst-plugins-bad              ; sdp & webrtc for voip
           gst-plugins-good-qt          ; rtpmanager for voip
           kdsingleapplication
           libevent
           libnice                      ; for voip
           libxkbcommon
           lmdb
           lmdbxx
           mtxclient
           nlohmann-json
           olm
           openssl
           qtdeclarative
           qtgraphicaleffects
           qtkeychain-qt6
           qtmultimedia
           qtwayland
           qtsvg
           re2
           spdlog-1.13
           vulkan-headers
           vulkan-loader
           xdg-utils                    ; xdg-open for opening URLs
           zlib))
    (native-inputs
     (list asciidoc pkg-config qttools))
    (home-page "https://github.com/Nheko-Reborn/nheko")
    (synopsis "Desktop client for Matrix using Qt and C++14")
    (description "@code{Nheko} want to provide a native desktop app for the
Matrix protocol that feels more like a mainstream chat app and less like an IRC
client.

Many matrix features are supported, including user registration, rooms, typing
notification, emojis, E2E encryption, and voip calls.")
    (license license:gpl3+)))

(define-public quaternion
  (package
    (name "quaternion")
    (version "0.0.95.1")
    (outputs '("out" "debug"))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/quotient-im/Quaternion")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10mzcr4rpyq5bl3h8wzxxlk8rdz7slhiq863xs77bmsq2pzf6lp8"))))
    (build-system qt-build-system)
    (inputs
     (list libqmatrixclient
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtmultimedia-5
           qtquickcontrols-5
           qtquickcontrols2-5
           qtsvg-5
           qttools-5
           qtwayland-5
           xdg-utils))
    (arguments
     `(#:tests? #f))                    ; no tests
    (home-page "https://matrix.org/docs/projects/client/quaternion.html")
    (synopsis "Graphical client for the Matrix instant messaging protocol")
    (description "Quaternion is a Qt5 desktop client for the Matrix instant
messaging protocol.  It uses libqmatrixclient and is its reference client
implementation.  Quaternion and libqmatrixclient together form the
QMatrixClient project.")
    (license (list license:gpl3+        ; all source code
                   license:lgpl3+))))   ; icons/breeze

(define-public hangups
  (package
    (name "hangups")
    (version "0.4.18")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hangups" version))
       (sha256
        (base32 "12mq22lygh6vz2h5dpvyjk18hx3jphb4kkavqsy298c7hw60hn7l"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'relax-dependencies
           ;; Relax overly strict package version specifications.
           (lambda _
             (substitute* "setup.py"
               (("==") ">=")
               ((",<.*'") "'")))))))
    (native-inputs
     (list nss-certs-for-test
           python-httpretty
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-aiohttp
           python-appdirs
           python-async-timeout
           python-configargparse
           python-mechanicalsoup
           python-protobuf
           python-readlike
           python-reparser
           python-requests
           python-urwid))
    (home-page "https://hangups.readthedocs.io/")
    (synopsis "Instant messaging client for Google Hangouts")
    (description
     "Hangups is an instant messaging client for Google Hangouts.  It includes
both a Python library and a reference client with a text-based user interface.

Hangups is implements a reverse-engineered version of Hangouts' proprietary,
non-interoperable protocol, which allows it to support features like group
messaging that aren’t available to clients that connect over XMPP.")
    (license license:expat)))

(define-public telegram-purple
  (package
    (name "telegram-purple")
    (version "1.4.7")
    (home-page "https://github.com/majn/telegram-purple")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))
                    (recursive? #t)))
              (sha256
               (base32
                "14h8lvj0kjvy1b5i84ha2w9rl3akxjwwvsp5j4dcxwfghrkzqgf2"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "Makefile.in"
                    ;; By default these two directories point to Pidgin's own
                    ;; prefix.
                    (("^PLUGIN_DIR_PURPLE=.*")
                     (string-append
                      "exec_prefix := @exec_prefix@\n"
                      "PLUGIN_DIR_PURPLE := @libdir@/purple-2\n"))
                    (("^DATA_ROOT_DIR_PURPLE=.*")
                     "DATA_ROOT_DIR_PURPLE := @datarootdir@\n")

                    ;; Honor sysconfdir instead of trying to write to /etc.
                    (("DESTDIR\\)/etc/telegram-purple")
                     "DESTDIR)@sysconfdir@/telegram-purple"))
                  #t))
              (patches (search-patches "telegram-purple-adjust-test.patch"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("which" ,which)))
    (inputs
     (list pidgin
           libgcrypt
           libwebp
           glib
           gtk+-2
           zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; We're using release tag for repository checkout - let's prepare
         ;; header defining GIT_COMMIT manually instead of running git to
         ;; identify version which is being compiled. Git repository
         ;; is removed anyway and only source code is kept.
         (add-after 'unpack 'prepare-commit.h
           (lambda _
             (with-output-to-file "./commit.h"
               (lambda ()
                 (display
                  (string-append "//generated by guix, use version instead of "
                                 "commit\n"
                                 "#ifndef GIT_COMMIT\n"
                                 "#  define GIT_COMMIT \"v"
                                 ,version "\"\n"
                                 "#endif\n"))))
             #t))
         (add-before 'configure 'set-SHELL-variables
           ;; Set these environment variables so that 'tgl/configure' uses the
           ;; right shell and not /bin/sh.
           (lambda _
             (let ((bash (which "bash")))
               (setenv "SHELL" bash)
               (setenv "CONFIG_SHELL" bash)
               #t))))))
    (synopsis "Telegram messaging support for Pidgin")
    (description
     "Telegram-purple is a plugin for Libpurple, the communication library
used by the Pidgin instant messaging client, that adds support for the
Telegram messenger.

This package is on ``life support'' until @code{tdlib-purple} is a full
replacement.")

    ;; Code under tgl/ (the Telegram library) is LGPLv2.1+, but the plugin
    ;; itself is GPLv2+.
    (license license:gpl2+)))

(define-public tdlib
  (let ((commit "8e29c4d7d21db3ab2c7a88c384626e95ef789f61")
        (revision "0"))
    (package
      (name "tdlib")
      (version (git-version "1.8.45" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/tdlib/td")
               (commit commit)))
         (sha256
          (base32 "16mjw052clfyknn3n3srl35dq3xmyyxwkvz42kml0g5r7qma9ws7"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:build-type "Release"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-failing-tests
              (lambda _
                (substitute* "test/CMakeLists.txt"
                  ;; The test cases are compiled into a distinct binary
                  ;; which uses mtproto.cpp to attempt to connect to
                  ;; a remote server. Removing this file from the sources
                  ;; list disables those specific test cases.
                  (("\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/mtproto.cpp") "")))))))
      (native-inputs
       (list gperf openssl zlib php doxygen))
      (synopsis "Cross-platform library for building Telegram clients")
      (description "Tdlib is a cross-platform library for creating custom
Telegram clients following the official Telegram API.  It can be easily used
from almost any programming language with a C-FFI and features first-class
support for high performance Telegram Bot creation.")
      (home-page "https://core.telegram.org/tdlib")
      (license license:boost1.0))))

(define-public purple-mm-sms
  (package
    (name "purple-mm-sms")
    (version "0.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/purple-mm-sms.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1daf7zl8bhhm1szkgxflpqql69f2w9i9nlgf1n4p1nynxifz1bim"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (let ((out (assoc-ref %outputs "out")))
         ;; Fix hardcoded paths
         (list (string-append "PREFIX=" out)
               (string-append "PLUGIN_DIR_PURPLE=" out "/lib/purple-2")
               (string-append "DATA_ROOT_DIR_PURPLE=" out "/share")))
       #:tests? #f      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)))
    (inputs
     (list modem-manager pidgin))
    (synopsis "Libpurple plugin for SMS via ModemManager")
    (description "Plugin for libpurple to allow sending SMS using ModemManager.")
    (home-page "https://source.puri.sm/Librem5/purple-mm-sms")
    (license license:gpl2+)))

(define-public purple-lurch
  (package
    (name "purple-lurch")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://github.com/gkdr/lurch")
                       (commit (string-append "v" version))))
       (modules '((guix build utils)))
       (snippet
        `(begin
           ;; Submodules
           (delete-file-recursively "lib")))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1ipd9gwh04wbqv6c10yxi02lc2yjsr02hwjycgxhl4r9x8b33psd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          (("^PURPLE_PLUGIN_DIR = .*")
                           (string-append "PURPLE_PLUGIN_DIR = " out
                                          "/lib/purple-2\n")))
                        (setenv "CC" "gcc")))))
       #:parallel-tests? #f))
    (native-inputs (list cmocka pkg-config))
    (inputs (list axc
                  glib
                  libgcrypt
                  libomemo
                  libsignal-protocol-c
                  libxml2
                  minixml
                  pidgin
                  sqlite))
    (synopsis "OMEMO Encryption for libpurple")
    (description "Purple-lurch plugin adds end-to-end encryption support
through the Double Ratchet (Axolotl) algorithm, to @code{libpurple}
applications using @acronym{XMPP, Extensible Messaging and Presence Protocol},
through its standard XEP-0384: @acronym{OMEMO, OMEMO Multi-End Message and
Object Encryption} Encryption.  It provides confidentiality, (weak) forward
secrecy, break-in recovery, authentication, integrity, deniability, and
asynchronicity.")
    (home-page "https://github.com/gkdr/lurch")
    (license license:gpl3+)))

(define-public libphonenumber
  (package
   (name "libphonenumber")
   (version "8.11.3")
   (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/libphonenumber")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (search-patches
                        "libphonenumber-reproducible-build.patch"))
              (sha256
               (base32
                "06y3mh1d1mks6d0ynxp3980g712nkf8l5nyljpybsk326b246hg9"))))
   (arguments
    `(#:test-target "tests"
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'change-directory
          (lambda _ (chdir "cpp"))))))
   (build-system cmake-build-system)
   (native-inputs
    (list googletest pkg-config))
   (inputs
    (list boost protobuf icu4c))
   (synopsis "Library for parsing and using phone numbers")
   (description
    "This package provides a C++ library for parsing, formatting, and
validating international phone numbers.")
   (home-page "https://github.com/google/libphonenumber")
   (license license:asl2.0)))

(define-public chatty
  (package
    (name "chatty")
    (version "0.6.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/chatty.git")
                    (commit (string-append "v" version))
                    ;; Fetch the required subprojects, notably libcmatrix
                    ;; which has no releases and is developed in tandem.
                    ;; Note: this also pulls in libgd, and embeds functionality
                    ;; from it that is not part of the public API, making
                    ;; unbundling difficult.
                    (recursive? #true)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11q07vjrrjf3k00kk41vm79brpq0qigz7l328br3g0li979kz32v"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-updating-desktop-database
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script.*") ""))))
         (add-before 'check 'pre-check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; One test requires a running Xorg server.  Start one.
               (system "Xvfb :1 &")
               (setenv "DISPLAY" ":1")
               ;; HOME must be writable for writing configuration files.
               (setenv "HOME" "/tmp")))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           itstool
           pkg-config
           protobuf
           xorg-server-for-tests))
    (inputs
     (list feedbackd
           folks-with-libsoup2
           gnome-desktop
           gsettings-desktop-schemas
           gspell
           json-glib
           libgcrypt
           libgee
           libhandy
           olm
           libphonenumber
           modem-manager
           pidgin
           purple-mm-sms
           sqlite))
    (propagated-inputs
     (list adwaita-icon-theme evolution-data-server-3.44))
    (synopsis "Mobile client for XMPP and SMS messaging")
    (description "Chatty is a chat program for XMPP and SMS.  It works on mobile
as well as on desktop platforms.  It's based on libpurple and ModemManager.")
    (home-page "https://source.puri.sm/Librem5/chatty")
    (license license:gpl3+)))

(define-public mosquitto
  (package
    (name "mosquitto")
    (version "2.0.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://mosquitto.org/files/source/mosquitto-"
                           version ".tar.gz"))
       (sha256
        (base32 "17c9gf2xncxsi3v8fbgq3abfyb84lyr18in0s1pbplmqmr6fimbs"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DWITH_WEBSOCKETS=ON")))
    (inputs (list openssl libxslt libwebsockets-for-mosquitto))
    (synopsis "Message broker")
    (description
     "This package provides Eclipse Mosquitto, a message broker
that implements the MQTT protocol versions 5.0, 3.1.1 and 3.1.  Mosquitto
is lightweight and is suitable for use on all devices from low power single
board computers to full servers.

The MQTT protocol provides a lightweight method of carrying out messaging
using a publish/subscribe model.  This makes it suitable for Internet of
Things messaging such as with low power sensors or mobile devices such
as phones, embedded computers or microcontrollers.")
    (home-page "https://mosquitto.org/")
    ;; Dual licensed.
    (license (list license:epl1.0 license:edl1.0))))

(define-public python-paho-mqtt
  (package
    (name "python-paho-mqtt")
    (version "1.6.1")
    (source (origin
              (method git-fetch)        ;for tests
              (uri (git-reference
                    (url "https://github.com/eclipse/paho.mqtt.python")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0679iafabd3kvk4fj4lvcl14zg82yq5pz5rji4z659lm2g2zlwgn"))))
    (build-system python-build-system)
    (arguments (list #:phases
                     #~(modify-phases %standard-phases
                         (replace 'check
                           (lambda* (#:key tests? #:allow-other-keys)
                             (when tests?
                               (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest))
    (home-page "https://www.eclipse.org/paho/")
    (synopsis "Python implementation of an MQTT client class")
    (description "MQTT and MQTT-SN are lightweight publish/subscribe messaging
transports for TCP/IP and connection-less protocols (such as UDP).  The
Eclipse Paho project provides client side implementations of MQTT and MQTT-SN
in a variety of programming languages.  This package is for the Python
implementation of an MQTT version client class.")
    (license (list license:epl2.0 license:edl1.0)))) ;dual licensed

(define-public movim-desktop
  (let ((commit "83d583b83629dbd2ec448da9a1ffd81f6c1fb295")
        (revision "3"))
    (package
      (name "movim-desktop")
      (version
       (git-version "0.14.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/movim/movim_desktop")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1lsa3k3wx1d2lk0qs0k5jc5bmapnmpzwynprjf2wihh8c8y3iwlz"))))
      (build-system qt-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* `("CMakeLists.txt" "movim.desktop")
                 (("/usr")
                  (assoc-ref outputs "out"))
                 (("\"build")
                  "\"../build"))
               #t)))))
      (inputs
       (list qtbase-5 qtdeclarative-5 qtwebchannel-5))
      (propagated-inputs
       (list qtwebengine-5))
      (home-page "https://movim.eu/")
      (synopsis "Desktop Application for Movim")
      (description
       "Movim-Desktop is a desktop application, relying on Qt, for the Movim
social and chat platform.")
      (license license:gpl3+))))

(define-public psi-plus
  (package
    (name "psi-plus")
    (version "1.5.1484")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/psi-plus/psi-plus-snapshots")
         (commit version)))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        `(begin
           (delete-file-recursively "3rdparty")))
       (sha256
        (base32 "1jsm39nzzbqkp3zc0xqx7jid6p4q1ra28xad38wjr2l1jb8qjn24"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f                      ; No target
       #:imported-modules
       (,@%qt-build-system-modules
        (guix build glib-or-gtk-build-system))
       #:modules
       ((guix build qt-build-system)
        ((guix build glib-or-gtk-build-system)
         #:prefix glib-or-gtk:)
        (guix build utils))
       #:configure-flags
       (list
        "-DBUILD_PSIMEDIA=ON"           ; For A/V support
        "-DENABLE_PLUGINS=ON"
        "-DUSE_HUNSPELL=OFF"            ; Use Enchant instead
        "-DUSE_ENCHANT=ON"
        "-DUSE_CCACHE=OFF")             ; Not required
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/http-parser/http_parser.h")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qhttp/qhttp.pro")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qite/qite.pro")
                "")
               (("add_subdirectory\\( 3rdparty \\)")
                ""))
             (substitute* "src/CMakeLists.txt"
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qite/libqite")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/http-parser")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qhttp/src/private")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qhttp/src")
                "")
               (("\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty")
                "")
               (("add_dependencies\\(\\$\\{PROJECT_NAME\\} qhttp\\)")
                "target_link_libraries(${PROJECT_NAME} qhttp)"))
             (substitute* "src/src.cmake"
               (("include\\(\\$\\{PROJECT_SOURCE_DIR\\}/3rdparty/qite/libqite/libqite.cmake\\)")
                "list(APPEND EXTRA_LIBS qite)"))
             (substitute* '("src/filesharingmanager.h" "src/widgets/psirichtext.cpp"
                            "src/filesharingmanager.cpp" "src/widgets/psitextview.cpp"
                            "src/chatview_te.cpp" "src/msgmle.cpp")
               (("qite.h")
                "qite/qite.h")
               (("qiteaudio.h")
                "qite/qiteaudio.h")
               (("qiteaudiorecorder.h")
                "qite/qiteaudiorecorder.h"))))
         (add-after 'install 'wrap-env
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (name)
                  (let ((file (string-append out "/bin/" name))
                        (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                        (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                    (wrap-program file
                      `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                      `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))
                '("psi-plus")))))
         (add-after 'wrap-env 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           perl
           pkg-config
           python-wrapper
           ruby))
    (inputs
     (list bash-minimal       ; for wrap-program
           libb2
           dbus
           enchant
           glib
           gstreamer
           gst-plugins-base
           http-parser
           libgcrypt
           libgpg-error
           libidn
           libotr
           libsignal-protocol-c
           tidy-html
           openssl
           qca
           qhttp
           qite
           qtbase-5
           qtkeychain
           qtmultimedia-5
           qtsvg-5
           qtx11extras
           usrsctp
           libx11
           libxext
           libxcb
           libxscrnsaver
           zlib))
    (home-page "https://psi-plus.com/")
    (synopsis "Qt-based XMPP Client")
    (description
     "Psi+ is a spin-off of Psi XMPP client.  It is a powerful XMPP client
designed for experienced users.")
    (license license:gpl2+)))

(define-public psi
  (deprecated-package "psi" psi-plus))

(define-public python-zulip
  (package
    (name "python-zulip")
    (version "0.7.1")
    (source
     (origin
       ;; There is no source on Pypi.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zulip/python-zulip-api")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0da1ki1v252avy27j6d7snnc0gyq0xa9fypm3qdmxhw2w79d6q36"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; TODO: This is fixed upstream in later versions
           (substitute* "zulip/tests/test_default_arguments.py"
             (("optional arguments:") "options:"))))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'cd-to-zulip-dir
           (lambda _ (chdir "zulip")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (let ((test-zulip "../tools/test-zulip"))
               (when tests?
                 (add-installed-pythonpath inputs outputs)
                 (patch-shebang test-zulip)
                 (invoke test-zulip))))))))
    (propagated-inputs
     (list python-matrix-client python-pyopenssl python-requests
           python-six))
    (native-inputs
     (list python-cython python-distro python-pytest))
    (home-page "https://github.com/zulip/python-zulip-api")
    (synopsis "Zulip's API Python bindings")
    (description
     "This package provides Python bindings to Zulip's API.")
    (license license:asl2.0)))

(define-public zulip-term
  (package
    (name "zulip-term")
    (version "0.5.2")
    (source
     (origin
       ;; Pypi package doesn't ship tests.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/zulip/zulip-terminal")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1xhhy3v4wck74a83avil0rnmsi2grrh03cww19n5mv80p2q1cjmf"))
       (modules '((guix build utils)))
       (snippet '(substitute* "setup.py"
                   (("\\=\\=1\\.7") ">=1.7")    ; pytest-mock
                   (("\\=\\=2\\.5") ">=2.5")    ; pytest-cov
                   (("4\\.5\\.2") "4.4.2")))))  ; lxml
    (build-system pyproject-build-system)
    (arguments
     '(#:test-flags '("--ignore=tests/cli/test_run.py")))
    (inputs
     (list python-beautifulsoup4
           python-lxml
           python-mypy-extensions
           python-urwid
           python-urwid-readline
           python-zulip))
    (native-inputs
     (list python-distro python-pytest python-pytest-cov
           python-pytest-mock))
    (home-page "https://github.com/zulip/zulip-terminal")
    (synopsis "Zulip's official terminal client")
    (description "This package contains Zulip's official terminal client.")
    (license license:asl2.0)))

(define-public matterbridge
  (package
    (name "matterbridge")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/42wim/matterbridge")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet '(for-each delete-file-recursively
                  ;; TODO: unbundle the rest as well
                  '("vendor/filippo.io"
                    "vendor/github.com/blang"
                    "vendor/github.com/d5"
                    "vendor/github.com/davecgh"
                    "vendor/github.com/dustin"
                    "vendor/github.com/francoispqt"
                    "vendor/github.com/fsnotify"
                    "vendor/github.com/go-asn1-ber"
                    "vendor/github.com/golang"
                    "vendor/github.com/golang-jwt"
                    "vendor/github.com/google/uuid"
                    "vendor/github.com/gorilla/websocket"
                    "vendor/github.com/hashicorp"
                    "vendor/github.com/jpillora"
                    "vendor/github.com/json-iterator"
                    "vendor/github.com/kballard"
                    "vendor/github.com/klauspost"
                    "vendor/github.com/magiconair"
                    "vendor/github.com/mattn/go-colorable"
                    "vendor/github.com/mattn/go-isatty"
                    "vendor/github.com/mattn/go-runewidth"
                    "vendor/github.com/mgutz/ansi"
                    "vendor/github.com/minio/sha256-simd"
                    "vendor/github.com/mitchellh"
                    "vendor/github.com/modern-go"
                    "vendor/github.com/opentracing"
                    "vendor/github.com/pelletier"
                    "vendor/github.com/pkg"
                    "vendor/github.com/pmezard"
                    "vendor/github.com/rivo"
                    "vendor/github.com/russross"
                    "vendor/github.com/sirupsen"
                    "vendor/github.com/skip2"
                    "vendor/github.com/spf13"
                    "vendor/github.com/stretchr"
                    "vendor/github.com/subosito"
                    "vendor/github.com/valyala/bytebufferpool"
                    "vendor/github.com/vmihailenco/tagparser"
                    "vendor/go.uber.org"
                    "vendor/golang.org"
                    "vendor/google.golang.org/protobuf/"
                    "vendor/gopkg.in/ini.v1"
                    "vendor/gopkg.in/natefinch"
                    "vendor/gopkg.in/yaml.v2"
                    "vendor/gopkg.in/yaml.v3")))
       (sha256
        (base32 "0939fiy7z53izznfhlr7c6vaskbmkbj3ncb09fzx5dmz9cjngy80"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It helps to resolve <golang.org/x/net/publicsuffix/table.go:63:12>:
      ;; pattern data/children: cannot embed irregular file data/children
      #:embed-files #~(list "children" "nodes" "text")
      #:import-path "github.com/42wim/matterbridge"))
    (inputs (list
             ;; golang.org
             go-golang-org-x-crypto
             go-golang-org-x-image
             go-golang-org-x-mod
             go-golang-org-x-oauth2
             go-golang-org-x-sys
             go-golang-org-x-term
             go-golang-org-x-text
             go-golang-org-x-time
             go-golang-org-x-tools
             ;; google.golang.org
             go-google-golang-org-protobuf
             ;; gopkg.in
             go-gopkg-in-ini-v1
             go-gopkg-in-yaml-v2
             go-gopkg-in-yaml-v3
             go-gopkg-in-natefinch-lumberjack-v2
             ;; filippo.io
             go-filippo-io-edwards25519
             ;; uber.org
             go-go-uber-org-atomic
             go-go-uber-org-multierr
             go-go-uber-org-zap
             ;; github.com
             go-github-com-blang-semver
             go-github-com-d5-tengo-v2
             go-github-com-davecgh-go-spew
             go-github-com-dustin-go-humanize
             go-github-com-francoispqt-gojay
             go-github-com-fsnotify-fsnotify
             go-github-com-go-asn1-ber-asn1-ber
             go-github-com-golang-jwt-jwt
             go-github-com-golang-protobuf
             go-github-com-google-uuid
             go-github-com-gorilla-websocket
             go-github-com-hashicorp-errwrap
             go-github-com-hashicorp-go-multierror
             go-github-com-hashicorp-golang-lru
             go-github-com-hashicorp-hcl
             go-github-com-jpillora-backoff
             go-github-com-json-iterator-go
             go-github-com-kballard-go-shellquote
             go-github-com-klauspost-compress
             go-github-com-klauspost-cpuid-v2
             go-github-com-magiconair-properties
             go-github-com-mattn-go-colorable
             go-github-com-mattn-go-isatty
             go-github-com-mattn-go-runewidth
             go-github-com-mgutz-ansi
             go-github-com-minio-sha256-simd
             go-github-com-mitchellh-go-homedir
             go-github-com-mitchellh-mapstructure
             go-github-com-modern-go-concurrent
             go-github-com-modern-go-reflect2
             go-github-com-opentracing-opentracing-go
             go-github-com-pelletier-go-toml
             go-github-com-pelletier-go-toml-v2
             go-github-com-pkg-errors
             go-github-com-pmezard-go-difflib
             go-github-com-rivo-uniseg
             go-github-com-russross-blackfriday
             go-github-com-sirupsen-logrus
             go-github-com-skip2-go-qrcode
             go-github-com-spf13-afero
             go-github-com-spf13-cast
             go-github-com-spf13-jwalterweatherman
             go-github-com-spf13-pflag
             go-github-com-spf13-viper
             go-github-com-stretchr-testify
             go-github-com-subosito-gotenv
             go-github-com-valyala-bytebufferpool
             go-github-com-vmihailenco-tagparser))
    (synopsis "Bridge together various messaging networks and protocols")
    (description
     "Relays messages between different channels from various
messaging networks and protocols.  So far it supports mattermost, IRC, gitter,
xmpp, slack, discord, telegram, rocketchat, twitch, ssh-chat, zulip, whatsapp,
keybase, matrix, microsoft teams, nextcloud, mumble, vk and more with REST
API.  Mattermost is not required.")
    (home-page "https://github.com/42wim/matterbridge")
    (license license:asl2.0)))

(define-public jj
  (package
    (name "jj")
    (version "2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://23.fi/jj/jj-" version ".tar.gz"))
              (sha256
               (base32
                "02xz2ci93bccvil5iff804mh3zr5iqkf6zx5mxgraz17xg0azlgh"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                            ;There are no tests.
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'install
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (bin (string-append out "/bin")))
                     (install-file "jj" bin)))))))
    (native-inputs (list pkg-config))
    (inputs (list glib loudmouth))
    (home-page "https://23.fi/jj/")
    (synopsis "FIFO based Jabber client")
    (description
     "jj is a simple file-system-based Jabber client, inspired by ii IRC
client.  Interaction with jj is done by writing and reading files from the
server directory which jj creates.  It is perfect for bots and
notifications.")
    (license license:expat)))

(define-public pounce
  (package
    (name "pounce")
    (version "3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://git.causal.agency/pounce/snapshot/pounce-"
                           version ".tar.gz"))
       (sha256
        (base32 "0kk0jrfiwfaybr0i5xih3b0yd4i6v3bz866a7xal1j8wddalbwlp"))
       (patches (search-patches "pounce-readable-checks.patch"))))
    (outputs '("out" "debug"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;there are no tests
      #:phases #~(modify-phases %standard-phases
                   (add-before 'configure 'pre-configure
                     (lambda _
                       ;; The build system is peculiar and sets environment
                       ;; variables such as CFLAGS itself, which must not be
                       ;; overridden via Make flags.
                       (setenv "CC" #$(cc-for-target))
                       (setenv "CFLAGS" "-g") ;for debug symbols
                       (setenv "PREFIX" #$output))))))
    (native-inputs
     (list pkg-config universal-ctags))
    (inputs
     (list libressl libxcrypt))
    (home-page "https://git.causal.agency/pounce")
    (synopsis "Simple multi-client TLS-only IRC bouncer")
    (description
     "@command{pounce} is a multi-client, TLS-only IRC bouncer.  It maintains
a persistent connection to an IRC server, acting as a proxy and buffer for
a number of clients.")
    (license license:gpl3+)))

(define-public weechat-matrix
  (package
    (name "weechat-matrix")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/poljar/weechat-matrix")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1iv55n4k05139f7jzkhczgw4qp6qwilrvfsy3c6v2m1kxffj12d3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((weechat-python (string-append (assoc-ref outputs "out")
                                                   "/share/weechat/python")))
               ;; Avoid circular import by renaming the matrix module to
               ;; weechat_matrix.
               (substitute* (cons "main.py"
                                  (append (find-files "matrix")
                                          (find-files "tests")))
                 (("from matrix") "from weechat_matrix")
                 (("import matrix") "import weechat_matrix"))
               ;; Install python modules.
               (invoke "make" "install-lib"
                       (string-append "INSTALLDIR="
                                      (site-packages inputs outputs)
                                      "/weechat_matrix"))
               ;; Extend PYTHONPATH to find installed python modules.
               (add-installed-pythonpath inputs outputs)
               ;; Augment sys.path so that dependencies are found.
               (substitute* "main.py"
                 (("import os\n" all)
                  (apply string-append
                         all
                         "import sys\n"
                         (map (lambda (path)
                                (string-append "sys.path.append('" path "')\n"))
                              (string-split (getenv "GUIX_PYTHONPATH") #\:)))))
               ;; Install script.
               (mkdir-p weechat-python)
               (copy-file "main.py"
                          (string-append weechat-python "/matrix.py")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (inputs
     (list python-matrix-nio python-pygments python-pyopenssl
           python-webcolors))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/poljar/weechat-matrix")
    (synopsis "Weechat Matrix protocol script")
    (description "@code{weechat-matrix} is a Python plugin for Weechat that lets
Weechat communicate over the Matrix protocol.")
    (license license:isc)))

(define-public weechat-wee-slack
  (package
    (name "weechat-wee-slack")
    (version "2.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wee-slack/wee-slack")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0klvvrrvdjh3wph1cdqd4x3nj170v1wirmr2mm91q3sqs5lf3lqj"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Augment sys.path so that dependencies are found.
             (substitute* "wee_slack.py"
               (("import sys\n" all)
                (apply string-append
                       all
                       (map (lambda (path)
                              (string-append "sys.path.append('" path "')\n"))
                            (string-split (getenv "GUIX_PYTHONPATH") #\:)))))
             ;; Install script.
             (install-file "wee_slack.py"
                           (string-append (assoc-ref outputs "out")
                                          "/share/weechat/python"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "pytest")))))))
    (inputs
     (list python-websocket-client))
    (native-inputs
     (list python-mock python-pytest))
    (home-page "https://github.com/wee-slack/wee-slack")
    (synopsis "Weechat Slack script")
    (description "@code{weechat-wee-slack} is a WeeChat native client for
Slack.  It provides supplemental features only available in the web/mobile
clients such as synchronizing read markers, typing notification, threads (and
more)!  It connects via the Slack API, and maintains a persistent websocket
for notification of events.")
    (license license:expat)))

(define-public python-librecaptcha
  (package
    (name "python-librecaptcha")
    (version "0.7.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/taylordotfish/librecaptcha")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0r35ws6vdf31j01kpacvpjplddm254r0cgy0npmhgnfxd5kpjf3s"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-pillow python-requests python-esprima python-pygobject gobject-introspection gtk+))
    (synopsis "Show CAPTCHA without running proprietary code")
    (description "This package shows CAPTCHA without running proprietary code.")
    (home-page "https://github.com/taylordotfish/librecaptcha")
    (license license:gpl3+)))

(define-public python-harmony
  (package
    (name "python-harmony")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/taylordotfish/harmony.git")
                     (commit version)))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1bm9xcnzpnpj6rlhbrnl2abwclzl7ivgh1vb5644y9mnhcs489js"))))
    (build-system python-build-system)
    (native-inputs
     (list python-tox))
    (inputs
     (list python-librecaptcha python-keyring python-requests))
    (synopsis "Discord account management")
    (description "This package provides account management tools for
Discord.")
    (home-page "https://github.com/taylordotfish/harmony")
    (license license:gpl3+)))

(define-public python-pypresence
  (package
    (name "python-pypresence")
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pypresence" version))
              (sha256
               (base32
                "0rp09lfxbc3syd1rhbai2516c3wyfxkzrsw8v4bd57qqr2cay7b9"))))
    (build-system python-build-system)
    (home-page "https://github.com/qwertyquerty/pypresence")
    (synopsis "Discord RPC client")
    (description "This package provides @code{python-pypresence}, a Discord
RPC client written in Python.")
    (license license:expat)))

(define-public pn
  (package
    (name "pn")
    (version "0.9.0")
    (home-page "https://github.com/Orange-OpenSource/pn")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lvzb0yixj7wmmqzsri20k9nn3gf06j0yjvmg2mi1zihywq7s4dx"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'set-lib-destination
                          (lambda _
                            (substitute* "CMakeLists.txt"
                              (("DESTINATION \\$\\{AWKLIBPATH\\}")
                               "DESTINATION lib")))))))
    (inputs (list icu4c libphonenumber protobuf))
    (synopsis "Command-line validation tool for phone numbers")
    (description
     "@code{pn} provides a command line tool that allows users to operate on
phone numbers (get validity information, reformat them, or extract numbers from
a text snippet), using @code{libphonenumber}.")
    (license license:asl2.0)))

(define-public senpai
  (package
    (name "senpai")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.sr.ht/~delthas/senpai")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d16wbqm3hrydcb0308mg5cvgzz85vqq1bnwx0ly4647fr3f21wp"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "git.sr.ht/~delthas/senpai/cmd/senpai"
           #:unpack-path "git.sr.ht/~delthas/senpai"
           #:install-source? #f
           ;; Step away from cmd/senpai to test the whole project.
           #:test-subdirs #~(list "../../...")
           #:phases
           #~(modify-phases
                 %standard-phases
               (add-after 'build 'build-doc
                 (lambda* (#:key unpack-path #:allow-other-keys)
                   (invoke "make" "doc"
                           "-C" (string-append "src/" unpack-path))))
               (add-after 'install 'install-doc
                 (lambda* (#:key unpack-path #:allow-other-keys)
                   (install-file
                    (string-append "src/" unpack-path "/doc/senpai.1")
                    (string-append #$output "/share/man/man1"))
                   (install-file
                    (string-append "src/" unpack-path "/doc/senpai.5")
                    (string-append #$output "/share/man/man5"))))
               (add-after 'install 'install-desktop-file
                (lambda* (#:key unpack-path #:allow-other-keys)
                  (install-file
                    (string-append "src/" unpack-path "/contrib/senpai.desktop")
                    (string-append #$output "/share/applications")))))))
    (native-inputs
     (list go-codeberg-org-emersion-go-scfg
           go-git-sr-ht-rockorager-vaxis
           go-github-com-containerd-console
           go-github-com-delthas-go-libnp
           go-github-com-delthas-go-localeinfo
           go-github-com-disintegration-imaging
           go-github-com-godbus-dbus-v5
           go-github-com-rivo-uniseg
           go-golang-org-x-net
           go-golang-org-x-time
           go-mvdan-cc-xurls-v2
           go-github-com-mattn-go-runewidth
           scdoc
           which))
    (home-page "https://sr.ht/~delthas/senpai")
    (synopsis "Modern terminal IRC client")
    (description
     "@code{senpai} is an IRC client that works best with bouncers.")
    (license license:isc)))

(define-public ejabberd
  (package
    (name "ejabberd")
    (version "24.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/processone/ejabberd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l82d8l4ck60vijzirl4xkyc2wv28jnq6amwi8dralm7r218hg7m"))))
    (build-system rebar-build-system)
    (inputs (list bash-minimal coreutils procps sed))
    (native-inputs
     (list autoconf
           automake
           erlang-base64url
           erlang-cache-tab
           erlang-eimp
           erlang-epam
           erlang-eredis
           erlang-esip
           erlang-ezlib
           erlang-fast-tls
           erlang-fast-xml
           erlang-fast-yaml
           erlang-idna
           erlang-jiffy
           erlang-jose
           erlang-luerl
           erlang-mqtree
           erlang-p1-acme
           erlang-p1-mysql
           erlang-p1-oauth2
           erlang-p1-pgsql
           erlang-p1-utils
           erlang-pc
           erlang-pkix
           erlang-provider-asn1
           erlang-stringprep
           erlang-stun
           erlang-sqlite3
           erlang-unicode-util-compat
           erlang-xmpp
           erlang-yconf))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "CC" "gcc")))
          (add-after 'unpack 'bootstrap
            (lambda _
              (invoke "aclocal" "-I" "m4")
              (invoke "autoconf" "-f")))
          (add-after 'bootstrap 'make-various-fixes
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((sh (search-input-file inputs "/bin/sh")))
                ;; Fix shell locations.
                (substitute* "configure" (("/bin/sh") sh))
                (substitute* "rebar.config.script"
                  (("sh -c '") (string-append sh " -c '")))
                ;; Do not recompile dependences.
                (substitute* "rebar.config"
                  (("\\[\\{\"eimp\", \\[\\]\\},") "[]}.\n{nop, ["))
                ;; Do not include source files into release.
                (substitute* "rebar.config"
                  (("\\{include_src, true\\},") "{include_src, false},"))
                ;; Do not install erl wrapper, we will do it ourselves.
                (substitute* "rebar.config"
                  (("\\{copy, \"rel/files/erl\",")
                   "%{copy, \"rel/files/erl\","))
                ;; It seems ejabberd still needs jiffy due to p1_acme.
                (substitute* "rebar.config"
                  (("\\{if_version_below, \"27\",") "{if_version_below, \"30\","))
                ;; Unpin pinned dependences.
                (substitute* "rebar.lock"
                  ((",1\\}") ",0}"))
                ;; Set proper paths.
                (substitute* "vars.config.in"
                  (("\\{sysconfdir, \".*\"\\}\\.")
                   "{sysconfdir, \"/etc\"}."))
                (substitute* "vars.config.in"
                  (("\\{localstatedir, \".*\"\\}\\.")
                   "{sysconfdir, \"/var\"}."))
                (substitute* "vars.config.in"
                  (("\\{config_dir, \".*\"\\}\\.")
                   "{config_dir, \"/etc/ejabberd\"}."))
                (substitute* "vars.config.in"
                  (("\\{logs_dir, \".*\"\\}\\.")
                   "{logs_dir, \"/var/log/ejabberd\"}."))
                (substitute* "vars.config.in"
                  (("\\{spool_dir, \".*\"\\}\\.")
                   "{spool_dir, \"/var/lib/ejabberd\"}.")))))
          (add-after 'make-various-fixes 'configure
            (lambda _
              (invoke "./configure"
                      (string-append "--prefix=" #$output))))
          (replace 'build
            (lambda _
              (invoke "make" "rel")))
          (replace 'install
            (lambda _
              (let ((ejabberd "_build/prod/rel/ejabberd"))
                (copy-recursively
                 (string-append ejabberd "/conf")
                 (string-append ejabberd "/share/doc/ejabberd-"
                                #$version "/examples"))
                (for-each
                 (lambda (rmdir)
                   (delete-file-recursively
                    (string-append ejabberd "/" rmdir)))
                 '("conf" "database" "logs"))
                (delete-file
                 (string-append (string-append ejabberd "/ejabberd-"
                                               #$version ".tar.gz")))
                (let ((erts (car (find-files ejabberd "erts-.*"
                                             #:directories? #t))))
                  (delete-file (string-append erts "/bin/erl"))
                  (install-file "rel/files/erl"
                                (string-append erts "/bin")))
                (chmod (string-append ejabberd
                                      "/bin/install_upgrade.escript") #o755)
                (copy-recursively ejabberd #$output))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (wrap-program (string-append out "/bin/ejabberdctl")
                  `("PATH" ":" suffix
                    ,(map (lambda (command)
                            (dirname
                             (search-input-file
                              inputs (string-append "bin/" command))))
                          (list "date" "dirname" "grep"
                                "id" "pgrep" "sed"))))))))))
    (synopsis "Robust, Ubiquitous and Massively Scalable Messaging Platform")
    (description "This package provides Ejabberd -- Robust, Ubiquitous and
Massively Scalable Messaging Platform.  It supports XMPP, MQTT and SIP
protocols.")
    (home-page "https://www.ejabberd.im")
    (license license:gpl2+)))

;;; messaging.scm ends here
