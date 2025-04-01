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
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
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
  #:use-module (gnu packages xorg))

(define-public choqok
  (package
    (name "choqok")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/choqok/"
                           (version-major+minor version)
                           "/src/choqok-" version ".tar.xz"))
       (sha256
        (base32 "0zm4nkpmvd181xlkis7ydzx54p3vn0zgpdzgh54f1hsjy6ahsq16"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools-5 pkg-config))
    (inputs
     (list attica-5
           kcmutils-5
           kconfigwidgets-5
           kcoreaddons-5
           kemoticons
           kglobalaccel-5
           kguiaddons-5
           ki18n-5
           kio-5
           knotifications-5
           knotifyconfig-5
           kparts-5
           ktextwidgets-5
           kwallet-5
           kwidgetsaddons-5
           kxmlgui-5
           ;; TODO: telepathy
           breeze-icons ; default icon set
           purpose-5
           qca
           qoauth
           qtbase-5
           qtnetworkauth-5
           sonnet-5))
    (home-page "https://kde.org/applications/internet/org.kde.choqok")
    (synopsis "Micro-Blogging Client")
    (description "Choqok is a fast, efficient and simple to use micro-blogging
client for KDE.  It currently supports the twitter.com and identi.ca
microblogging services.

Other notable features include:
@itemize
@item Support for user + friends time-lines.
@item Support for @@Reply time-lines.
@item Support for sending and receiving direct messages.
@item Twitpic.com integration.
@item The ability to use multiple accounts simultaneously.
@item Support for search APIs for all services.
@item KWallet integration.
@item Support for automatic shortening urls with more than 30 characters.
@item Support for configuring status lists appearance.
@end itemize")
    (license license:gpl3+)))

(define-public falkon
  (package
    (name "falkon")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/falkon-" version ".tar.xz"))
       (sha256
        (base32
         "1hhljgv5c0na4851r9klwzwgifygmq9xkrii7c8hvd7bnwc0jmwd"))))
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

(define-public kget
  (package
    (name "kget")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kget-" version ".tar.xz"))
       (sha256
        (base32 "0pj7zrmdccbwd4bwrh76p23xfw40544vvqh4hdi7gvmcrkvris3n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list boost
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
           breeze-icons ; default icon set
           qca-qt6))
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

(define-public konversation
  (package
    (name "konversation")
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/konversation-" version ".tar.xz"))
       (sha256
        (base32 "13gy4sgkw2i4cg3xwbm5mlp3ay95yqsd5r7mf92rp6kyk9iikcig"))))
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
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/krdc-" version ".tar.xz"))
       (sha256
        (base32 "0kwsnmvnqyaj53njpd0424fsd7pkdcv5h162dym2binkq710mdvf"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools))
    (inputs
     (list breeze-icons ; default icon set
           kbookmarks
           freerdp
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
           gnutls))
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
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktorrent-" version ".tar.xz"))
       (sha256
        (base32 "178mri9hjlriji43rf36h0bfp5zsy4ky8aczsnxxawrg25c8h2ma"))))
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
    (version "24.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1ik4ak7vb5axr1fs717h15zad9zxvxfs9y5l6y98lpbwjpd94wbf"))))
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
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0hzhbn8rrlgkml47r6qqpcqg01az2za20kcsrasgmc5bf1cwclqw"))))
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
           qtwebsockets))
    (home-page "https://invent.kde.org/libraries/kunifiedpush")
    (synopsis "UnifiedPush client components")
    (description "KUnifiedPush is a @uref{https://unifiedpush.org/,
UnifiedPush} client library and distributor daemon.")
    (license license:lgpl2.0+)))

(define-public neochat
  (package
    (name "neochat")
    (version "25.04.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "00kj66kij8vsmfhzr8cc6vz2bh7vi6w7r5aa0nrcpdgnxi7g30lg"))))
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
           qtkeychain-qt6
           qtlocation
           qtmultimedia
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
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/ruqola/ruqola-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0w05ww6dn1xfmz67i3avkzdlcrb575hjad2lnm2cxd0jds0b1bg5"))))
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
