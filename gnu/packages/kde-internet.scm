;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
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
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
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
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list attica
           kcmutils
           kconfigwidgets
           kcoreaddons
           kemoticons
           kglobalaccel
           kguiaddons
           ki18n
           kio
           knotifications
           knotifyconfig
           kparts
           ktextwidgets
           kwallet
           kwidgetsaddons
           kxmlgui
           ;; TODO: telepathy
           breeze-icons ; default icon set
           purpose
           qca
           qoauth
           qtbase-5
           qtnetworkauth-5
           sonnet))
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
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/falkon-" version ".tar.xz"))
       (sha256
        (base32
         "11r1iwimdzabfah68gsvw6xi67cj539anqa6s1rg33agsi5y56d3"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E"
                             "(locationbartest|qmltabsapitest)")))))))
    (native-inputs
     (list extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list karchive
           kcoreaddons
           kcrash
           ki18n
           kio
           kwallet
           openssl
           purpose
           qtquickcontrols-5
           qtsvg-5
           qtwebengine-5
           qtx11extras
           qtwayland-5
           xcb-util))
    (home-page "https://www.falkon.org/")
    (synopsis "Qt-based web browser for KDE")
    (description
     "Falkon is is a Qt-based web browser for  KDE.")
    (license license:gpl3+)))

(define-public kget
  (package
    (name "kget")
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kget-" version ".tar.xz"))
       (sha256
        (base32 "1n9wnm1si4g4rv8zaqpr8m3c2aav0mj8i7z96m78dk1apippx77r"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list boost
           gmp
           gpgme
           kcmutils
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kdelibs4support ;; KLocale
           ki18n
           kiconthemes
           kio
           kitemviews
           knotifications
           knotifyconfig
           kparts
           kservice
           ktextwidgets
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libgcrypt
           libktorrent
           ;; TODO: libmms
           ;; TODO: LibKWorkspace - plasma-workspace?
           breeze-icons ; default icon set
           qca
           qgpgme
           qtbase-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests? ;; FIXME: two tests fails.
               (invoke "ctest" "-E" "(schedulertest|filedeletertest)"))
             #t)))))
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
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/konversation-" version ".tar.xz"))
       (sha256
        (base32 "1ip0jlz71fad5l0ppbc6w914hqk7h626s12ssbb9p1c2yvlr1j1v"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kbookmarks
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kemoticons
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
           kwallet
           kwidgetsaddons
           kwindowsystem
           breeze-icons ; default icon set
           phonon
           qtbase-5
           qca
           qtmultimedia-5
           solid
           sonnet))
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

(define-public kopete
  (package
    (name "kopete")
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kopete-" version ".tar.xz"))
       (sha256
        (base32 "1ps6g440p1dy2zwbj23f0mzw1d78r02aj88fy3i5sws9p9ra92gi"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config))
    (inputs
     (list alsa-lib
           boost
           expat
           glib
           gpgme
           jsoncpp
           kcmutils
           kconfig
           kcontacts
           kcoreaddons
           kcrash
           kdbusaddons
           kdelibs4support
           kdnssd
           kemoticons
           khtml
           ki18n
           kidentitymanagement
           kjs
           ;; TODO? kleopatra (additionally to libkleo)
           knotifyconfig
           kparts
           kpimtextedit
           ksyntaxhighlighting
           ktexteditor
           kwallet
           ;; TODO: Libgadu
           libidn
           libkleo
           ;; TODO: LibMeanwhile
           libotr
           libsrtp
           libxml2
           libxslt
           mediastreamer2
           openssl
           ortp
           phonon
           qca
           qgpgme
           qtbase-5
           speex
           v4l-utils
           ;; TODO: Xmms
           zlib))
    ;; TODO: enable video support
    (home-page "https://apps.kde.org/kopete/")
    (synopsis "Instant messaging and chat application")
    (description "Kopete is an instant messenger supporting Jabber/XMPP ,AIM,
ICQ, Gadu-Gadu, Novell GroupWise Messenger, and more.  It is designed to be a
flexible and extensible multi-protocol system suitable for personal and
enterprise use.

The goal of Kopete is to provide users with a single easy-to-use way to access
all of their instant messaging systems.  The interface puts people first, and
is integrated with the system address book to let you access your contacts
from other KDE applications.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public krdc
  (package
    (name "krdc")
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/krdc-" version ".tar.xz"))
       (sha256
        (base32 "0jva74n11fpm4ix4sbi0y1xnbly97lnap7dfj0bliw5s2d0sdjr0"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kbookmarks
           freerdp
           kcmutils
           kcompletion
           kconfig
           kdnssd
           ki18n
           kiconthemes
           knotifications
           knotifyconfig
           kwallet
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libssh
           libvnc
           breeze-icons ; default icon set
           qtbase-5))
    (arguments ;; FIXEME: libvnc can't be found for some reason.
     (list #:configure-flags #~(list "-DWITH_VNC=NO")))
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
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ktorrent-" version ".tar.xz"))
       (sha256
        (base32 "17q6ivnbh4zxqnbm1bdzz3hri1434sq2rs9y57lvn4bb2xdwn1z5"))))
    (build-system qt-build-system)
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
           ki18n
           kiconthemes
           kio
           knotifications
           knotifyconfig
           kparts
           kplotting
           kross
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libgcrypt
           libktorrent
           ;; TODO: LibKWorkspace -> plasma-workspace?
           breeze-icons ; default icon set
           phonon
           qtbase-5
           qtscript
           qtwebengine-5
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
    (version "23.04.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/"
                           version "/src/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1m6gyk1bids7qr9wfh6gcfq73ac9j5b2bljvfvfsw9f1ky1cmwab"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list boost
           gmp
           karchive
           kcrash
           ki18n
           kio
           libgcrypt
           qca
           qtbase-5
           solid))
    (home-page "https://invent.kde.org/network/libktorrent")
    (synopsis "BitTorrent protocol library for C++ / Qt 5 / KDE Frameworks")
    (description "The KTorrent library supports connectivity to HTTP and UDP
trackers, mainline DHT and the new generation Micro Transport
Protocol (uTP).  In addition, it provides many powerful BitTorrent network
features including but not limited to torrent downloading and seeding, torrent
creation and downloaded data verification, magnet links, advanced peer
management, IP blocking lists.")
    (license license:gpl2+)))
