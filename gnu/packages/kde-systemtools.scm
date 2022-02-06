;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages kde-systemtools)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages search)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public dolphin
  (package
    (name "dolphin")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/dolphin-" version ".tar.xz"))
       (sha256
        (base32 "0m5nqa8j0mcsrx9wxfcf8z39kxas51k03lschr721vm4x65j64jq"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools ruby ruby-test-unit))
    (inputs
     (list baloo
           baloo-widgets
           kactivities
           kbookmarks
           kcmutils
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kiconthemes
           kinit
           kio
           knewstuff
           knotifications
           kparts
           ktextwidgets
           kuserfeedback
           kwindowsystem
           oxygen-icons ;; default icon set
           phonon
           qtbase-5
           solid))
    (arguments
     `(#:tests? #f)) ;; TODO: 4/15 tests fail even with offscreen
    (home-page "https://apps.kde.org/dolphin/")
    (synopsis "File manager for KDE")
    (description "Dolphin is a file manager for KDE focusing on usability.
The main features of Dolphin are:
@itemize
@item Navigation bar for URLs, which navigates quickly
      through the file hierarchy.
@item View properties are remembered for each folder.
@item Split of views is supported.
@item Network transparency.
@item Undo/redo functionality.
@item Renaming of a variable number of selected items in one step.
@end itemize")
    (license ;; GPL for programs, FDL for documentation
     (list license:gpl2+ license:fdl1.2+))))

(define-public dolphin-plugins
  (package
    (name "dolphin-plugins")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/dolphin-plugins-" version ".tar.xz"))
       (sha256
        (base32 "0rbz6fw98c71h10ry1xjc0pgzvphajmj18lnjm4hf7bbrizsmdb5"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list dolphin
           ki18n
           kio
           ktexteditor
           ksyntaxhighlighting
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5))
    (home-page "http://www.kde.org/")
    (synopsis "VCS-Plugins for Dolphin")
    (description "This package contains plugins that offer integration in
Dolphin with the version control systems: Bzr, Git, Mercurial, Subversion.")
    (license license:gpl2+)))

(define-public khelpcenter
  (package
    (name "khelpcenter")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/khelpcenter-" version ".tar.xz"))
       (sha256
        (base32 "1fj1c57bqs009rx9db4ifvfmhhl4b35r5sfly3wvbfr4dapjqfqr"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list grantlee
           karchive
           kbookmarks
           kcodecs
           kconfig
           kcoreaddons
           kdbusaddons
           khtml
           ki18n
           kinit
           kio
           kjs
           kparts
           kservice
           kwindowsystem
           libxml2
           oxygen-icons ;; default icon set
           qtbase-5
           xapian))
    (arguments
     `(#:tests? #f  ;; 1/1 test fails
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-executable
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               ;; Since qt-wrap selectors do not wrap for /share/kf5
               ;; directories, we need this so khelpcenter can find html4.css.
               (wrap-program (string-append out "/bin/khelpcenter")
                 `("XDG_DATA_DIRS" suffix
                   (,(string-append (assoc-ref inputs "khtml") "/share"))))))))))
    (home-page "https://apps.kde.org/khelpcenter/")
    (synopsis "KDE documentation viewer")
    (description "KHelpCenter uses meta data files which describe the
documentation available in the system.  Each document is represented by a meta
data file and shown as an entry in the KHelpCenter navigation tree view.  The
meta data contains information about title and short description of the
document, the location of the document and some more information like how to
search the document and translations of title and description.  Document
hierarchy is represented as hierarchy of the meta data files.  Directories are
also described by a meta data file which contains the same information as a
document meta data file.")
    (license license:gpl2+)))

(define-public konsole
  (package
    (name "konsole")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/konsole-" version ".tar.xz"))
       (sha256
        (base32 "06sqm2xmairicrdcxnf7imvyvw0wyknrrym334scx2w7mfhjg5qs"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kbookmarks
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kinit
           kio
           knewstuff
           kglobalaccel
           knotifications
           knotifyconfig
           kparts
           kpty
           kservice
           ktextwidgets
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5
           qtscript))
    (arguments
     `(#:tests? #f)) ;; TODO: 2/15 tests fail even with HOME, offscreen, SHELL, debus
    (home-page "http://www.kde.org/")
    (synopsis "Terminal emulator similar for KDE")
    (description "Konsole is a terminal emulator, similar to xterm, built on
the KDE Platform.  It can contain multiple terminal sessions inside one window
using detachable tabs.  Konsole supports customizable schemes, saved sessions,
output monitoring and more.

This package is part of the KDE base applications module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public krfb
  (package
    (name "krfb")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/krfb-" version ".tar.xz"))
       (sha256
        (base32 "1r8lvvh2z8xi0l3pizlpl12nm4fnbpgiwqmx18w8i51x4j27dv0n"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           ki18n
           knotifications
           kwallet
           kwayland
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libvnc
           libxcb
           libxtst
           oxygen-icons ;; default icon set
           pipewire-0.3
           plasma-wayland-protocols
           qtbase-5
           qtwayland
           qtx11extras
           wayland
           xcb-util-image
           zlib))
    (home-page "https://apps.kde.org/krfb/")
    (synopsis "Desktop Sharing utility")
    (description "KDE Desktop Sharing is a server application that allows you
to share your current session with a user on another machine.  The desktop
session can be viewed or even controlled remotely by any VNC or RFB client,
such as the KDE Remote Desktop Connection client.

KDE Desktop Sharing can restrict access to only users who are explicitly
invited, and will ask for confirmation when a user attempts to connect.

This package is part of the KDE networking module.")
    (license ;; GPL for programs, LGPL for libraries, FDL for documentation
     (list license:gpl2+ license:lgpl2.0+ license:fdl1.2+))))

(define-public ksystemlog
  (package
    (name "ksystemlog")
    (version "21.12.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksystemlog-" version ".tar.xz"))
       (sha256
        (base32 "0jkd0rx0xlzwsxa3s40sp5x4r19a9rg1x9klpnjfw0b326vgf2m9"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     ;; Not including Journald since this is not used in guix
     (list karchive
           kcompletion
           kconfig
           kcoreaddons
           ki18n
           kiconthemes
           kio
           kitemviews
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           oxygen-icons ;; default icon set
           qtbase-5))
    (home-page "https://apps.kde.org/ksystemlog/")
    (synopsis "System log viewer")
    (description "This program is developed for being used by beginner users,
which don't know how to find information about their Linux system, and how the
log files are in their computer.  But it is also designed for advanced users,
who want to quickly see problems occurring on their server.

This package is part of the KDE administration module.")
    (license license:gpl2+)))

(define-public yakuake
  (package
    (name "yakuake")
    (version "21.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/yakuake-" version ".tar.xz"))
              (sha256
               (base32
                "10mkr8svkjf2s023mf21cil2c5v986s5b2yp1hm0fzdgmawpwrh9"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list breeze-icons
           karchive
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kglobalaccel
           ki18n
           kiconthemes
           kio
           knewstuff
           knotifications
           knotifyconfig
           konsole
           kparts
           kwayland
           kwidgetsaddons
           kwindowsystem
           qtbase-5
           qtsvg-5
           qtx11extras))
    (home-page "https://apps.kde.org/yakuake/")
    (synopsis "Quad-style terminal emulator for KDE")
    (description "Yakuake is a drop-down terminal emulator based on KDE Konsole
technology.  Features include:
@itemize
@item Smoothly rolls down from the top of your screen
@item Tabbed interface
@item Configurable dimensions and animation speed
@item Skinnable
@item Sophisticated D-Bus interface
@end itemize")
    (license license:gpl2+)))
