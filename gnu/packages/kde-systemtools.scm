;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2017-2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020 Marius Bakke<mbakke@fastmail.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023-2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023-2025 Sughosha <sughosha@disroot.org>
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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (gnu packages search)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg))

(define-public baloo-widgets
  (package
    (name "baloo-widgets")
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/baloo-widgets-" version ".tar.xz"))
       (sha256
        (base32 "1k8v6jkz7mdwnpzr8hvig24iggy6i90warr3qjgnh2sddz5x6v2w"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list baloo kconfig ki18n kio))
    (arguments
     (list #:configure-flags #~(list "-DBUILD_WITH_QT6=ON")
           #:qtbase qtbase))
    (home-page "https://community.kde.org/Baloo")
    (synopsis "Wigets for use with Baloo")
    (description "Baloo is a framework for searching and managing metadata.
This package contains GUI widgets for baloo.")
    (license license:lgpl2.0+)))

(define-public dolphin
  (package
    (name "dolphin")
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/dolphin-" version ".tar.xz"))
       (sha256
        (base32 "14b7lrp768ryl9gvp1bl8svcl752j4yi1cn9p0sgs1v4ixlz3x06"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools ruby ruby-test-unit))
    (inputs
     (list baloo
           baloo-widgets
           breeze-icons ;; default icon set
           kbookmarks
           kcmutils
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
           kio
           knewstuff
           knotifications
           kparts
           ktextwidgets
           kuserfeedback
           kwindowsystem
           libxkbcommon
           plasma-activities
           qtmultimedia
           qtwayland
           solid))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; TODO: 4/15 tests fail even with offscreen
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
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/dolphin-plugins-" version ".tar.xz"))
       (sha256
        (base32 "1j0g2p7hj6ghg84bbchwayr0s0gcy3a8frgvwwjjc0aap5cxs4a3"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list dolphin
           ki18n
           kio
           ktexteditor
           ktextwidgets
           ksyntaxhighlighting
           kxmlgui
           breeze-icons ;; default icon set
           qt5compat))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://www.kde.org/")
    (synopsis "VCS-Plugins for Dolphin")
    (description "This package contains plugins that offer integration in
Dolphin with the version control systems: Bzr, Git, Mercurial, Subversion.")
    (license license:gpl2+)))

(define-public kdf
  (package
    (name "kdf")
    (version "25.08.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/"
                                  version "/src/kdf-" version ".tar.xz"))
              (sha256
               (base32
                "1nq13dz5pa7zr5gd3sb3w1iiqc2jj3gh9qv5dcxbs6w4ypry4nkw"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcmutils
           kconfigwidgets
           kcoreaddons
           kcrash
           ki18n
           kiconthemes
           kio
           knotifications
           kwidgetsaddons
           kstatusnotifieritem
           kxmlgui
           qt5compat
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://kde.org/applications/system/kdk")
    (synopsis "View Disk Usage")
    (description "KDiskFree displays the available file devices (hard drive
partitions, floppy and CD drives, etc.) along with information on their
capacity, free space, type and mount point.  It also allows you to mount and
unmount drives and view them in a file manager.")
    (license license:gpl2+)))

(define-public khelpcenter
  (package
    (name "khelpcenter")
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/khelpcenter-" version ".tar.xz"))
       (sha256
        (base32 "1nmhg3v73p8yapmvp3chac3vi6jpjwmc0jj28ksbmdrp952yaygb"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools perl))
    (inputs
     (list karchive
           kbookmarks
           kcodecs
           kconfig
           kcoreaddons
           kdbusaddons
           ki18n
           kio
           kparts
           kservice
           ktexttemplate
           kwindowsystem
           libxml2
           breeze-icons ;; default icon set
           qtbase
           xapian
           qtwayland
           qtwebengine))
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

(define-public kio-fuse
  (package
    (name "kio-fuse")
    (version "5.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/kio-fuse/kio-fuse-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "051xfqfygq11dg6l6522h097raf0yc0sr4vaf5z9ih2mw1yamxmd"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (native-inputs (list dbus extra-cmake-modules pkg-config))
    (inputs (list fuse kio kcoreaddons qtbase))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "FUSE Interface for KIO")
    (description "This package provides FUSE Interface for KIO.")
    (license license:lgpl2.1+)))

(define-public kpmcore
  (package
    (name "kpmcore")
    (version "25.08.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/release-service/" version
                    "/src/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0mamm43krqzn91y7dwpl4nva8bppywqc59gcbyd1208hw1ll60ir"))))
    (build-system cmake-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     `(("coreutils" ,coreutils)
       ("cryptsetup" ,cryptsetup)
       ("eudev" ,eudev)
       ("kauth" ,kauth)
       ("kcoreaddons" ,kcoreaddons)
       ("ki18n" ,ki18n)
       ("kwidgetsaddons" ,kwidgetsaddons)
       ("lvm2" ,lvm2)
       ("mdadm" ,mdadm)
       ("polkit-qt6" ,polkit-qt6)
       ("qtbase" ,qtbase)
       ("qca-qt6" ,qca-qt6)
       ("smartmontools" ,smartmontools)
       ("util-linux" ,util-linux)
       ("util-linux:lib" ,util-linux "lib")))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-polkit-action-path
            (lambda _
              (substitute* "src/util/CMakeLists.txt"
                (("DESTINATION \\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                 "DESTINATION share/polkit-1/actions"))
              (substitute* "src/backend/corebackend.cpp"
                  (("\\/usr") #$output))))
          (add-before 'configure 'patch-trustedprefixes-file
              (lambda* (#:key inputs #:allow-other-keys)
                (call-with-output-file "src/util/trustedprefixes"
                  (lambda (port)
                    (map (lambda (prefix)
                           (display prefix port)
                           (newline port))
                         (list (assoc-ref inputs "coreutils")
                               (assoc-ref inputs "util-linux")
                               (assoc-ref inputs "eudev")
                               (assoc-ref inputs "cryptsetup")
                               (assoc-ref inputs "lvm2")
                               (assoc-ref inputs "mdadm")
                               (assoc-ref inputs "smartmontools")
                               "/run/current-system/profile"
                               "/usr"
                               "/")))))))))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Library for managing partitions")
    (description "Library for managing partitions.")
    (license license:gpl3+)))

(define-public konsole
  (package
    (name "konsole")
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/konsole-" version ".tar.xz"))
       (sha256
        (base32 "0xz3bzkd21pxpqv7m1xinncvq469szs092kxph833pg5095b83r2"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools zlib))
    (inputs
     (list kbookmarks
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           kguiaddons
           ki18n
           kiconthemes
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
           breeze-icons ;; default icon set
           qt5compat
           qtwayland
           qtmultimedia
           icu4c))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;; TODO: 2/15 tests fail even with HOME, offscreen, SHELL, debus
    (home-page "https://www.kde.org/")
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
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/krfb-" version ".tar.xz"))
       (sha256
        (base32 "1ni85fp464ayrm4pyapslmx5c3xr9agsnqwx3765fb9yjd18l6w8"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f
                     #:configure-flags
                     #~(list (string-append "-DQtWaylandScanner_EXECUTABLE="
                                            #$(this-package-native-input "qtwayland")
                                            "/lib/qt6/libexec/qtwaylandscanner"))))
    (native-inputs
     (list extra-cmake-modules pkg-config kdoctools qtwayland))
    (inputs
     (list kcompletion
           kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kdnssd
           ki18n
           knotifications
           kpipewire
           kstatusnotifieritem
           kwallet
           kwayland
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           libvnc
           libxcb
           libxtst
           breeze-icons ;; default icon set
           pipewire
           plasma-wayland-protocols
           qtwayland
           wayland
           xcb-util-image
           libxkbcommon
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
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/ksystemlog-" version ".tar.xz"))
       (sha256
        (base32 "0g7g0d0hgf2diwjg9mnrs0igsyz26xm6k2485gpqyymh0dg8kvm2"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     ;; Not including Journald since this is not used in guix
     (list breeze-icons ;; default icon set
           karchive
           kcompletion
           kconfig
           kcoreaddons
           kcrash
           ki18n
           kiconthemes
           kio
           kitemviews
           ktextwidgets
           kwidgetsaddons
           kxmlgui
           qtwayland))
    (home-page "https://apps.kde.org/ksystemlog/")
    (synopsis "System log viewer")
    (description "This program is developed for being used by beginner users,
which don't know how to find information about their Linux system, and how the
log files are in their computer.  But it is also designed for advanced users,
who want to quickly see problems occurring on their server.

This package is part of the KDE administration module.")
    (license license:gpl2+)))

(define-public kwalletmanager
  (package
    (name "kwalletmanager")
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/kwalletmanager-" version ".tar.xz"))
       (sha256
        (base32 "0wpvsvvljcynnbdlldj669l1bw1ayhcspkqc5d69wvnlx7w6bads"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list karchive
           kauth
           kcmutils
           kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kjobwidgets
           knotifications
           kservice
           kstatusnotifieritem
           ktextwidgets
           kwallet
           kwindowsystem
           kxmlgui
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://apps.kde.org/kwalletmanager5/")
    (synopsis "Tool to manage passwords on KWallet")
    (description
     "This package provides a tool to manage passwords on @code{kwallet}.")
    (license license:gpl2+)))

(define-public partitionmanager
  (package
    (name "partitionmanager")
    (version "25.08.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/release-service/" version
                           "/src/partitionmanager-" version ".tar.xz"))
       (sha256
        (base32 "0yq4v5m8a3c3nwp2ck458vin0jcnmhbryhlkxqjd7r9qh17f45w1"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kconfigwidgets
           kcoreaddons
           kcrash
           kdbusaddons
           ki18n
           kio
           kjobwidgets
           kpmcore
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           polkit-qt6
           qtwayland))
    (home-page "https://apps.kde.org/partitionmanager/")
    (synopsis "Disk device, partition and file system manager")
    (description "KDE Partition Manager is a utility to help you manage the
disks, partitions, and file systems.  It allows you to easily create, copy,
move, delete, back up, restore, and resize them without losing data.  It
supports a large number of file systems, including ext2/3/4, btrfs, NTFS,
FAT16/32, JFS, XFS and more.")
    (license license:gpl3+)))

(define-public spectacle-ocr-screenshot
  (package
    (name "spectacle-ocr-screenshot")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/funinkina/spectacle-ocr-screenshot")
             (commit version)))
       (sha256
        (base32 "06dyvv4h4m4j8cm3f7ivcczql26rfajkmw84qh7kik71f70qvwjw"))
       (file-name (git-file-name name version))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f ;no tests
           #:modules '((guix build qt-build-system)
                       ((guix build gnu-build-system) #:prefix gnu:)
                       (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda _
                   (invoke "qmake" (string-append "PREFIX=" #$output))))
               (replace 'build (assoc-ref gnu:%standard-phases 'build))
               (replace 'install
                 (lambda _
                   (install-file "spectacle-ocr-screenshot"
                                 (string-append #$output "/bin")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list leptonica
           tesseract-ocr
           zxing-cpp))
    (home-page "https://github.com/funinkina/spectacle-ocr-screenshot")
    (synopsis "Utility to extract text from Spectacle")
    (description "This package provides an application that integrates with KDE
Spectacle screenshot tool with Tesseract OCR to extract text from screenshots
as well as QR codes.")
    (license license:expat)))

(define-public yakuake
  (package
    (name "yakuake")
    (version "25.08.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/yakuake-" version ".tar.xz"))
              (sha256
               (base32
                "07251izhip7q1kwd87839009v3s7b2q1xgxsxgfgcs50v6kgxgkh"))))
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
           kstatusnotifieritem
           kwayland
           kwidgetsaddons
           kwindowsystem
           libxkbcommon
           qtsvg
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
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
