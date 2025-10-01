;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Superfly Johnson <superfly.johnson@yahoo.com>
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

(define-module (gnu packages kde-internet)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages linphone)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define-public kdsoap-ws-discovery-client
  (package
    (name "kdsoap-ws-discovery-client")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/kdsoap-ws-discovery-client/"
                           "/kdsoap-ws-discovery-client-" version ".tar.xz"))
       (sha256
        (base32 "0yj2ngw4li5r6zhmkh2lb8fdf8ixz6pp5hxsb4342pz72g04glic"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs (list kdsoap))
    (arguments (list
                ;; test require network.
                #:tests? #f
                #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
                #:qtbase qtbase))
    (home-page "https://caspermeijn.gitlab.io/kdsoap-ws-discovery-client/")
    (synopsis "WS-Discovery client library based on KDSoap")
    (description "This package provides a ws-Discovery client library based on
KDSoap.")
    (license license:gpl3+)))

(define-public qxmpp
  (package
    (name "qxmpp")
    ;; kaidan requires a precise version
    (version "1.10.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://invent.kde.org/libraries/qxmpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qinrbr63b1baqv1a7cph8bma6kj1ib8s8ywq6d9497lc1yl2kgi"))))
    (build-system qt-build-system)
    (arguments
     `(#:qtbase ,qtbase
       #:configure-flags (list "-DBUILD_EXAMPLES=false"
                               "-DWITH_GSTREAMER=true"
                               "-DBUILD_OMEMO=ON") ;needed by kaidan
       #:test-exclude
        (string-join ;; These tests use the network.
         (list "tst_qxmppiceconnection"
               "tst_qxmppcallmanager"
               "tst_qxmpptransfermanager")
         "|")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list
       gstreamer
       libomemo-c
       qca-qt6
       qt5compat))
    (home-page "https://invent.kde.org/libraries/qxmpp")
    (synopsis "XMPP client and server library")
    (description
     "QXmpp is a XMPP client and server library written in C++ and uses the Qt
framework.  It builds XMPP clients complying with the XMPP Compliance Suites
2021 for IM and Advanced Mobile.")
    (license license:lgpl2.1+)))

(define-public snorenotify
  (package
    (name "snorenotify")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/snorenotify/"
                            version "/src/snorenotify-" version ".tar.xz"))
        (sha256
         (base32
          "0jz6ivk90h7iwgyxar7xzzj8yvzn6s1my6cqs9bdnwqswfk1nhbd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f)) ; both tests fail, require display
    (inputs
     (list qtbase-5))
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (home-page "https://techbase.kde.org/Projects/Snorenotify")
    (synopsis "Qt notification framework")
    (description "Snorenotify is a multi platform Qt notification framework.
Using a plugin system it is possible to create notifications with many
different notification systems.")
    (license license:lgpl3)))

(define-public falkon
  (package
    (name "falkon")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/falkon-" version ".tar.xz"))
       (sha256
        (base32
         "1049wwm46cd2dd96f9gwlnpz3sdrk8fs12fsp6qk0apmgzq3lf7x"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config qttools))
    (inputs
     (list karchive
           kcoreaddons
           kcrash
           ki18n
           kio
           kwallet
           openssl
           purpose
           qt5compat
           qtsvg
           qtwebengine
           qtwayland
           xcb-util))
    (home-page "https://www.falkon.org/")
    (synopsis "Qt-based web browser for KDE")
    (description
     "Falkon is is a Qt-based web browser for  KDE.")
    (license license:gpl3+)))

(define-public kaidan
  (package
    (name "kaidan")
    (version "0.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/unstable/kaidan/" version
                                  "/kaidan-" version ".tar.xz"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (delete-file-recursively "3rdparty")))
              (sha256
               (base32 "0q8py100nmvyhm8pfnvpxmghbg445x2vgpw3c519bcrr4w7y6yl0"))))
    (build-system qt-build-system)
    (arguments
     (list
       #:qtbase qtbase
       #:configure-flags #~(list "-DBUILD_TESTS=true")
       #:test-exclude "PublicGroupChatTest"
       #:phases
         #~(modify-phases %standard-phases
           (add-before 'check 'set-home
             (lambda _
               ;; Tests need write permission in $HOME.
               (setenv "HOME" "/tmp"))))))
    (native-inputs (list extra-cmake-modules
                         pkg-config))
    (inputs (list icu4c
                  kcrash
                  kdsingleapplication
                  kio
                  kirigami
                  kirigami-addons
                  knotifications
                  kquickimageeditor
                  prison
                  qqc2-desktop-style
                  qtlocation
                  qtmultimedia
                  qtpositioning
                  qtsvg
                  qttools
                  qtwayland
                  qxmpp
                  sonnet))
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

(define-public kget
  (package
    (name "kget")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kget-" version ".tar.xz"))
       (sha256
        (base32 "0pg2cv1x04gd7wr1i9qw7p22hg16asarzn9sycq4xwifxg1fvbb7"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list boost
           breeze-icons ; default icon set
           gmp
           ;; TODO: enable when we qgpgme support qt6.
           ;; gpgme
           ;; qgpgme
           kcmutils
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           knotifications
           knotifyconfig
           kparts
           kservice
           kstatusnotifieritem
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libgcrypt
           libktorrent
           libmms
           qca-qt6
           qtwayland))
    (arguments
     (list #:qtbase qtbase))
    (home-page "https://www.kde.org/")
    (synopsis "Versatile and user-friendly download manager")
    (description "KGet is an advanced download manager with support for
Metalink and Bittorrent.  Downloads are added to the list, where they can be
paused, queued, or scheduled for later.  KGet supports download via FTP anf
HTTP(S) as well as pausing downloads.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public kdeconnect
  (package
    (name "kdeconnect")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/kdeconnect-kde-"
                           version ".tar.xz"))
       (sha256
        (base32
         "07rmkm8gmfx1hs5n5rql2q9f539hdwv1l8wgjcmd2m5793f0nd4a"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list (string-append "-DQtWaylandScanner_EXECUTABLE="
                                  #$(this-package-native-input "qtwayland")
                                  "/lib/qt6/libexec/qtwaylandscanner")
                   "-DKDE_INSTALL_LIBEXECDIR=libexec"
                   ;; So kdeconnect.so isn't installed to lib/plugins
                   "-DPLUGIN_INSTALL_DIR=lib/qt6/plugins")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-dbus-autostart
                          (lambda _
                            ;; 'dbus-daemon' requires an absolute Exec path.
                            (substitute* "daemon/org.kde.kdeconnect.service.in"
                              (("kdeconnectd")
                               (string-append #$output "/bin/kdeconnectd"))))))
           #:tests? #f)) ; tests fail hard in our build environment
    (native-inputs
     (list extra-cmake-modules
           kdoctools
           libxtst
           pkg-config
           python-wrapper
           wayland-protocols
           qtwayland))
    (inputs
     (list dbus
           kcmutils
           kconfigwidgets
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           kirigami
           kirigami-addons
           kitemmodels
           knotifications
           kpackage
           kpeople
           kstatusnotifieritem
           kwayland
           libfakekey
           openssl
           plasma-wayland-protocols
           pulseaudio-qt
           qca-qt6
           qqc2-desktop-style
           qtbase
           qtconnectivity
           qtdeclarative
           qtmultimedia
           qtwayland
           qtsvg
           sonnet
           wayland
           modemmanager-qt
           libxkbcommon))
    (home-page "https://community.kde.org/KDEConnect")
    (synopsis "Enable your devices to communicate with each other")
    (description "KDE Connect is a project that enables all your devices to
communicate with each other.  Here's a few things KDE Connect can do:
@enumerate
@item Receive your phone notifications on your desktop computer and reply to messages
@item Control music playing on your desktop from your phone
@item Use your phone as a remote control for your desktop
@item Run predefined commands on your PC from connected devices
@item Check your phones battery level from the desktop
@item Ring your phone to help finding it
@item Share files and links between devices
@item Browse your phone from the desktop
@item Control the desktop's volume from the phone
@end enumerate")
    (properties `((upstream-name . "kdeconnect-kde")))
    (license (list license:gpl2 license:gpl3)))) ; dual licensed

(define-public kio-extras
  (package
    (name "kio-extras")
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1n3cidj9rd77gjagdncp3f1s8351cf56h3mfwsv8z5vw5cppbi5a"))
              (modules '((guix build utils)))
              (snippet
               ;; Fix including libproxy.
               '(substitute* "kcms/proxy/wpad-detector/main.cpp"
                  (("libproxy\\/proxy\\.h") "proxy.h")))))
    (build-system cmake-build-system)
    (arguments
     (list #:test-exclude
           (string-append "("
                          (string-join '("filenamesearchtest"
                                         "thumbnailtest"
                                         "testkioarchive")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (when tests?
                   (setenv "HOME" (getcwd))
                   (setenv "TMPDIR" (getcwd))
                   (invoke "ctest" "-E" test-exclude))))
               (add-after 'install 'fix-kiod-path
                 (lambda _
                   (let* ((kio #$(this-package-input "kio"))
                          (kf-version
                           #$(version-major
                              (package-version (this-package-input "kio")))))
                     (substitute* (string-append #$output
                                                 "/share/dbus-1/services/"
                                                 "org.kde.kmtpd5.service")
                       (("Exec=.*$")
                        (string-append "Exec=" kio "/libexec/kf" kf-version
                                       "/kiod" kf-version "\n")))))))))
    (native-inputs (list extra-cmake-modules dbus kdoctools pkg-config qttools))
    ;; TODO: libappimage
    (inputs (list gperf
                  imath
                  plasma-activities
                  plasma-activities-stats
                  karchive
                  kbookmarks
                  kcmutils
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kdnssd
                  kdbusaddons
                  kdsoap
                  kdsoap-ws-discovery-client
                  kguiaddons
                  knotifications
                  ktextwidgets
                  ki18n
                  kio
                  ksyntaxhighlighting
                  libimobiledevice
                  libkexiv2
                  libmtp
                  libplist
                  libproxy
                  libssh
                  libtirpc
                  openexr
                  phonon
                  qtbase
                  qt5compat
                  qcoro-qt6
                  qtsvg
                  samba
                  shared-mime-info
                  solid
                  taglib
                  zlib))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Additional components to increase the functionality of KIO")
    (description
     "This package provides additional components to increase
the functionality of the KDE resource and network access abstractions.")
    (license license:lgpl2.0+)))

(define-public kio-zeroconf
  (package
    (name "kio-zeroconf")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kio-zeroconf-" version ".tar.xz"))
       (sha256
        (base32 "0w27hxmaccw74sycrxpchgh6qgkbqyclc6h7ijsrvvh4l2xhlmc0"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kdbusaddons kdnssd ki18n kio))
    (arguments (list #:qtbase qtbase
                     #:tests? #f
                     #:configure-flags
                     #~(list "-DQT_MAJOR_VERSION=6")))
    (home-page "https://apps.kde.org/kio_zeroconf/")
    (synopsis "DNS-SD Service Discovery Monitor")
    (description "Adds an entry to Dolphin's Network page to show local
services such as printers which advertise themselves with DNSSD (called Avahi
or Bonjour by other projects).")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public konversation
  (package
    (name "konversation")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/konversation-" version ".tar.xz"))
       (sha256
        (base32 "0flm9nhk9sv70by4z81kks4wchcrdy6nbgg3bnpi8gzz9j69zlaw"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools qttools))
    (inputs
     (list karchive
           kbookmarks
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kglobalaccel
           ki18n
           kiconthemes
           kidletime
           kio
           kitemviews
           knewstuff
           knotifications
           knotifyconfig
           kparts
           kstatusnotifieritem
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           breeze-icons ; default icon set
           phonon
           qca-qt6
           qtmultimedia
           qtwayland
           qt5compat
           solid
           sonnet))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/konversation/")
    (synopsis "Graphical Internet Relay Chat (IRC) client for KDE")
    (description "Konversation is a graphical Internet Relay Chat client (IRC)
with KDE support.

Features are:
@itemize
@item Standard IRC features
@item SSL server support
@item Bookmarking support
@item Easy to use graphical user interface
@item Multiple servers and channels in one single window
@item DCC file transfer with resume support
@item Multiple identities for different servers
@item Text decorations and colors
@item Pattern-based message highlighting and OnScreen Display notifications
@item Automatic UTF-8 detection
@item Per channel encoding support
@item Theme support for nick icons
@item Highly configurable
@item Multi-language scripting support (with DCOP)
@item Customizable command aliases
@item NickServ-aware log-on (for registered nicknames)
@item Smart logging
@item Traditional or enhanced-shell-style nick completion
@end itemize")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public krdc
  (package
    (name "krdc")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/krdc-" version ".tar.xz"))
       (sha256
        (base32 "16vnh9aq8hlhi1bnyy0f0mscc025wp5fnd6vswx8h4dnhq0ink8k"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools))
    (inputs
     (list breeze-icons ; default icon set
           gnutls
           kbookmarks
           freerdp-3
           fuse
           kcmutils
           kcompletion
           kconfig
           kcrash
           kdnssd
           ki18n
           kio
           kiconthemes
           knotifications
           knotifyconfig
           kstatusnotifieritem
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           plasma-activities
           libssh
           libvnc
           ;; XXX: libvnc's Libs.private, remove when we use pkgconf
           lzo
           libjpeg-turbo
           libgcrypt
           qtwayland))
    (arguments
     (list #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")
           #:tests? #f
           #:qtbase qtbase))
    (home-page "https://apps.kde.org/krdc/")
    (synopsis "Remote desktop client")
    (description "KRDC is a client application that allows you to view or even
control the desktop session on another machine that is running a compatible
server.  VNC and RDP are supported.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public ktorrent
  (package
    (name "ktorrent")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktorrent-" version ".tar.xz"))
       (sha256
        (base32 "0kvjxhhpzn1knvmmq60fjl5hfl6jpiyzzxfsjwmfvc5xavmc4s5l"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list boost
           gmp
           karchive
           kcmutils
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           kglobalaccel
           ki18n
           kiconthemes
           kio
           knotifications
           knotifyconfig
           kparts
           kplotting
           kstatusnotifieritem
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libgcrypt
           libktorrent
           breeze-icons ; default icon set
           phonon
           qt5compat
           qtwebengine
           solid
           syndication
           taglib))
    (home-page "https://apps.kde.org/ktorrent/")
    (synopsis "BitTorrent client")
    (description "KTorrent is a BitTorrent application by KDE which allows you
to download files using the BitTorrent protocol.  It enables you to run
multiple torrents at the same time and comes with extended features to make it
a full-featured client for BitTorrent.")
    (license license:gpl2+)))

(define-public libktorrent
  (package
    (name "libktorrent")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "00hnmiwbxgwqs90zg07xbirxqi5nv900fpzmcx9gm0012051bqw5"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list boost
           gmp
           kcrash
           ki18n
           kio
           libgcrypt
           qca-qt6
           solid))
    (propagated-inputs (list karchive qt5compat))
    (home-page "https://invent.kde.org/network/libktorrent")
    (synopsis "BitTorrent protocol library for C++ / Qt 6 / KDE Frameworks")
    (description "The KTorrent library supports connectivity to HTTP and UDP
trackers, mainline DHT and the new generation Micro Transport
Protocol (uTP).  In addition, it provides many powerful BitTorrent network
features including but not limited to torrent downloading and seeding, torrent
creation and downloaded data verification, magnet links, advanced peer
management, IP blocking lists.")
    (license license:gpl2+)))

(define-public kunifiedpush
  (package
    (name "kunifiedpush")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1mx3kb2yxnvv6rzmhxkl4xqaxzmdkc6vj5a1rd27b5a36s3h3giz"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E" "connectortest")))))))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcmutils
           kcoreaddons
           ki18n
           kservice
           openssl
           qtwebsockets
           solid))
    (home-page "https://invent.kde.org/libraries/kunifiedpush")
    (synopsis "UnifiedPush client components")
    (description "KUnifiedPush is a @uref{https://unifiedpush.org/,
UnifiedPush} client library and distributor daemon.")
    (license license:lgpl2.0+)))

(define-public neochat
  (package
    (name "neochat")
    (version "25.08.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1dp9yng23vdzmhzrsvb3qh4l8z46pg8jbv51h6756a3zkckmvmws"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DSKIP_LICENSE_TESTS=ON")))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config python-minimal))
    (inputs
     (list cmark
           icu4c
           kcolorscheme
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kio
           kirigami
           kirigami-addons
           kitemmodels
           knotifications
           kquickcharts
           kquickimageeditor
           kstatusnotifieritem
           ksyntaxhighlighting
           kunifiedpush
           kwindowsystem
           libqmatrixclient
           olm
           openssl
           prison
           purpose
           qcoro-qt6
           qqc2-desktop-style
           qthttpserver
           qtkeychain-qt6
           qtlocation
           qtmultimedia
           qtspeech
           qtsvg
           qtwayland
           qtwebview
           sonnet))
    (home-page "https://apps.kde.org/neochat/")
    (synopsis "Matrix client for KDE")
    (description "Neochat is an instant messaging application using the Matrix
protocol, supporting end-to-end encryption.  Its features include:
@itemize
@item individual chats,
@item rooms,
@item spaces,
@item stickers and emojis,
@item spell checking,
@item uploading auttachments,
@item media playback,
@item message URL previews,
@item searching messages,
@item showing unread message information,
@item registering and configuring accounts,
@item importing and exporting encryption keys,
@item multiple accounts and
@item notifications.
@end itemize")
    (license license:gpl3+)))

(define-public ruqola
  (package
    (name "ruqola")
    (version "2.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde//stable/ruqola/ruqola-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "18brrxwn5dh5xj20znmg3v2044m3bw2jyv8abfwa45qk32qjyzi9"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home-directory
                 (lambda _
                   (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kcodecs
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kidletime
           kio
           knotifications
           knotifyconfig
           kstatusnotifieritem
           ksyntaxhighlighting
           ktextaddons
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           plasma-activities
           prison
           purpose
           qtkeychain-qt6
           qtwebsockets
           qtnetworkauth
           qtmultimedia
           qtsvg
           sonnet))
    (home-page "https://apps.kde.org/ruqola/")
    (synopsis "Rocket.Chat client")
    (description
     "Ruqola is a @uref{https://www.rocket.chat/, Rocket.Chat} client for KDE
desktop.  It supports:
@itemize
@item direct and thread messaging,
@item @acronym{OTR, Off-the-Record} messages,
@item individual and group channels,
@item autotranslate support,
@item emojis,
@item videos,
@item GIFs,
@item uploading auttachments,
@item searching messages in a room,
@item showing unread message information,
@item discussion rooms and configuring them,
@item storing messages in a local database,
@item exporting messages,
@item importing/exporting accounts,
@item registering and configuring accounts,
@item two-factor authentication via TOTP or email,
@item multiple accounts,
@item auto-away,
@item blocking/unblocking users,
@item administrator settings,
@item console moderation,
@item message URL previews,
@item channel list styles,
@item forwarding messages,
@item Rocket.Chat marketplace,
@item notifications,
@item replying directly from the notification and
@item DND image to websites or local folder.
@end itemize")
    (license (list license:lgpl2.1+ license:gpl2+))))

(define-public smb4k
  (package
    (name "smb4k")
    (version "4.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference (url "https://invent.kde.org/network/smb4k")
                           (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sbw7fdcgfjadggnmsl3m85kgim80lkn2vakwv4mrkrci0izk1xj"))))
    (build-system qt-build-system)
    (arguments (list
                #:qtbase qtbase
                #:tests? #f
                #:configure-flags #~(list "-DSMB4K_WITH_WS_DISCOVERY=ON")))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list breeze-icons ;; default icon set
           kauth
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           kdsoap
           kdsoap-ws-discovery-client
           ki18n
           kiconthemes
           kio
           kirigami
           kjobwidgets
           knotifications
           knotifyconfig
           kstatusnotifieritem
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libplasma
           qtdeclarative
           qtkeychain-qt6
           qtwayland
           samba
           solid))
    (home-page "https://apps.kde.org/smb4k/")
    (synopsis "Samba (SMB) share advanced browser")
    (description "Smb4K is an network neighborhood browser for the KDE
Software Compilation and a frontend to the programs of the Samba software
suite.

Features:
@itemize
@item Scanning for (active) workgroups, hosts, and shares
@item Support of the CIFS (Linux) and SMBFS (FreeBSD) file system
@item Mounting and unmounting of shares (using the KAuth framework)
@item Access to the files of a mounted share using a file manager or terminal
@item Auto-detection of external mounts and unmounts
@item Remounting of previously used shares on program start
@item Miscellaneous infos about remote network items and mounted shares
@item Network search
@item WINS server support
@item Preview of the contents of a share
@item Several methods to look up the initial list of workgroups and domains
@item Default login
@item Special handling of homes shares
@item Ability to bookmark favorite shares and organize them in groups
@item System tray widget
@item Support of advanced Samba options
@item Support of printer shares
@item KWallet support
@item Synchronization of a remote share with a local copy and vice versa
@item Ability to define custom options for individual servers and shares
@item Laptop support through the Solid hardware device framework
@end itemize")
    (license license:gpl2+)))
