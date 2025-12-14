;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2023-2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023, 2025 Sughosha <sughosha@disroot.org>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages kde-plasma)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages authentication)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-internet)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages kde-pim)
  ;; Including this module breaks the build
  ;#:use-module ((gnu packages kde-systemtools) #:select (konsole))
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages package-management) ; flatpak
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages web)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages opencl))

(define-public libplasma
  (package
    (name "libplasma")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1qg644ph4cqybfzk15qrxyjs16pnmvc071jji0p8ih5pfbcwykqr"))))
    (build-system qt-build-system)
    (propagated-inputs
     (list kpackage kwindowsystem))
    (native-inputs
     (list extra-cmake-modules kdoctools pkg-config
           gettext-minimal
           ;; for wayland-scanner
           wayland))
    (inputs (list
             karchive
             kconfigwidgets
             kglobalaccel
             kguiaddons
             kiconthemes
             kirigami
             kio
             ki18n
             ksvg
             kglobalaccel
             knotifications
             plasma-wayland-protocols
             plasma-activities
             qtdeclarative
             qtsvg
             qtwayland
             wayland
             libxkbcommon))
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("plasma-dialogstatetest"
                                         "plasma-iconitemtest"
                                         "plasma-dialogqmltest"
                                         "plasma-themetest"
                                         "iconitemhidpitest"
                                         "bug485688test"
                                         "dialognativetest")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'check-setup
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))))))))
    (home-page "https://invent.kde.org/plasma/libplasma")
    (synopsis "Libraries, components and tools of Plasma workspaces")
    (description "The plasma framework provides QML components, libplasma and
script engines.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public aurorae
  (package
    (name "aurorae")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lwqknvn9s13y9aknxzg8dr899n8lyxk8fc0z8lpm7gsrxami3gk"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules qttools))
    (inputs
     (list kcmutils
           kcolorscheme
           kconfig
           kdecoration
           ki18n
           knewstuff
           kpackage
           ksvg
           qtdeclarative))
    (synopsis "Themeable window decoration for KWin")
    (description
     "Aurorae is a themeable window decoration for KWin.  It supports theme
files consisting of several SVG files for decoration and buttons.  Themes can
be installed and selected directly in the configuration module of KWin
decorations.")
    (home-page "https://invent.kde.org/plasma/aurorae")
    (license license:gpl2+)))

(define-public bluedevil
  (package
    (name "bluedevil")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1c23vkkvfr28xqi7k5v6i01ijhk6dqg061wxza2wz94hg5mv9hs8"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list kcoreaddons
                  kcmutils
                  kirigami
                  kwidgetsaddons
                  kdbusaddons
                  kjobwidgets
                  ksvg
                  knotifications
                  kwindowsystem
                  libplasma
                  ki18n
                  kio
                  kdeclarative
                  shared-mime-info
                  qtdeclarative))
    (propagated-inputs
     (list bluez-qt))
    (synopsis "Manage the Bluetooth settings from Plasma")
    (description
     "This package provides Bluetooth manager for Plasma Shell.")
    (home-page "https://invent.kde.org/plasma/bluedevil")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public breeze
  (package
    (name "breeze")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "12vsy1n8iy1rlpsbpg6ilb6qvj70kiyxz69p6p43x4fai82myywh"))))
    (build-system qt-build-system)
    ;; TODO: Check whether is makes sence splitting into several outputs.
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcmutils ; optional
           kconfigwidgets
           kcoreaddons
           kde-frameworkintegration ; optional
           kdecoration
           kguiaddons
           ki18n
           kirigami
           kiconthemes
           kpackage
           kwindowsystem
           kcolorscheme
           qtsvg))
    (arguments (list #:qtbase qtbase
                     #:tests? #f
                     #:configure-flags #~(list "-DBUILD_QT5=OFF")))
    (home-page "https://invent.kde.org/plasma/breeze")
    (synopsis "Default KDE Plasma theme")
    (description "Artwork, styles and assets for the Breeze visual style for
the Plasma Desktop.  Breeze is the default theme for the KDE Plasma desktop.")
    (license license:gpl2+)))

(define-public breeze-gtk
  (package
    (name "breeze-gtk")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "1fgxmslq3k720j7i1ysl5lpn7hx1i8kqk5spasw3vl0nyvsxm0n6"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f))                              ;no 'test' target
    (native-inputs (list breeze extra-cmake-modules sassc python
                         python-pycairo))
    (home-page "https://invent.kde.org/plasma/breeze")
    (synopsis "Default KDE Plasma theme (GTK+ port)")
    (description "GTK+ port of the Breeze visual style for the Plasma Desktop.
Breeze is the default theme for the KDE Plasma desktop.")
    (license (list license:bsd-3                  ;cmake/FindSass.cmake
                   license:lgpl2.1+))))           ;<all other files>

(define-public calindori
  (package
    (name "calindori")
    (version "25.08.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/calindori-" version ".tar.xz"))
              (sha256
               (base32
                "1g6cwryjmk5fc54a5mi5d4zabnappw7md75fm2x3cp9wxykabg5p"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules python-minimal))
    (inputs (list kconfig
                  kcoreaddons
                  kdbusaddons
                  ki18n
                  kirigami
                  kcalendarcore
                  knotifications
                  kpeople
                  qtdeclarative
                  qtsvg
                  qtwayland))
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (home-page "https://invent.kde.org/plasma-mobile/calindori")
    (synopsis "Calendar for Plasma Mobile")
    (description
     "This package provides a touch friendly calendar application.")
    (license license:gpl3+)))

(define-public discover
  (package
    (name "discover")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.xz"))
              (sha256
               (base32
                "0ji4mz5rgb2az9wmbjwpdi5302x7d17lpvzcy9cxjlcpbh7iv20r"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude "flatpaktest"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-qmlmodule-required
                 (lambda _
                   (substitute* "CMakeLists.txt"
                     (("1.0 REQUIRED")
                      "1.0"))))
               (add-before 'configure 'set-LDFLAGS
                 (lambda _
                   (setenv "LDFLAGS" (string-append "-Wl,-rpath=" #$output
                                                    "/lib/plasma-discover"))))
               (add-before 'check 'check-setup
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "XDG_DATA_DIRS"
                             (string-append (getcwd)
                                            ":" (getenv "XDG_DATA_DIRS")))))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    ;; TODO: Add packagekit-qt6 when a guix backend for packagekit will be
    ;; available.
    (inputs (list appstream-qt6
                  attica
                  fwupd ; optional
                  flatpak ; optional
                  kauth
                  kiconthemes
                  kstatusnotifieritem
                  kcoreaddons
                  kconfig
                  kcrash
                  kdbusaddons
                  ki18n
                  karchive
                  kxmlgui
                  kirigami
                  kirigami-addons
                  kitemmodels
                  kuserfeedback
                  knewstuff
                  knotifications
                  kio
                  kdeclarative
                  kcmutils
                  kidletime
                  libostree ; required by flatpak
                  markdown
                  purpose
                  qcoro-qt6
                  qt5compat
                  qtdeclarative
                  qtsvg
                  qtwayland
                  qtwebview))
    ;; -- The following features have been disabled:
    ;; * RpmOstree, rpm-ostree binary to manage the system. Required to build the rpm-ostree backend
    ;;
    ;; -- The following OPTIONAL packages have not been found:
    ;; * Snapd, Library that exposes Snapd, <https://www.snapcraft.io>
    ;; Required to build the Snap backend
    (synopsis "KDE and Plasma resources management GUI")
    (description
     "This package provides a way to find and install applications,
games, and tools.")
    (home-page "https://invent.kde.org/plasma/discover")
    (license (list license:gpl2 license:gpl3))))

(define-public flatpak-kcm
  (package
    (name "flatpak-kcm")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/flatpak-kcm-" version ".tar.xz"))
       (sha256
        (base32 "11dc1iichc2ba150h930ilqani5sg1vccdqvzam503r4klaiy2qz"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list flatpak
           kcmutils
           kconfig
           kcoreaddons
           ki18n
           kitemmodels
           kservice
           libostree     ;required by flatpak
           qtdeclarative
           qtsvg))
    (home-page "https://invent.kde.org/plasma/flatpak-kcm")
    (synopsis "Flatpak permission management KCM")
    (description "This package provides a KCModule to configure permissions for
portal interactions.  It also allows changing @code{flatpak} settings via the
subsumed Flatpak KCM.

Note: Some permissions don't make sense to show for non-sandboxed apps as they
are only roxying @code{dbus}.")
    (license license:gpl3+)))

(define-public kactivitymanagerd
  (package
    (name "kactivitymanagerd")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0vfzginrm7j52wjdrbcfvc3gjgd8jck12idvh2j0dy46zkqbaclk"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules))
    (inputs (list boost
                  kcompletion
                  kglobalaccel
                  kio
                  kxmlgui
                  kdbusaddons
                  ki18n
                  kcrash))
    (synopsis "System service to manage user's activities")
    (description "This package provides components for managing the KDE Activity
concept.")
    (home-page "https://invent.kde.org/plasma/kactivitymanagerd")
    (license (list license:gpl2 license:gpl3))))

(define-public klassy
  (package
    (name "klassy")
    (version "6.4.breeze6.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paulmcauley/klassy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hrr8kg988qzpk8mccc8kk9lah9b89wx0h47s1981wvb9bci5dpr"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtsvg
                  kirigami
                  qtdeclarative
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kcolorscheme
                  kdecoration
                  kcmutils
                  kguiaddons
                  kiconthemes
                  kwindowsystem
                  ki18n))
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f ; No tests.
      #:configure-flags
      #~(list "-DBUILD_QT5=OFF")))
    (home-page "https://github.com/paulmcauley/klassy")
    (synopsis "Customizable window decoration for the KDE Plasma desktop")
    (description
     "Klassy is a highly customizable binary Window Decoration,
Application Style and Global Theme plugin for recent versions of the KDE Plasma
desktop.")
    (license (list license:bsd-3
                   license:cc0
                   license:expat
                   license:gpl2
                   license:gpl2+
                   license:gpl3))))

(define-public krdp
  (package
    (name "krdp")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1zsqgnirrf85pchf3p1q7hmnxv4ci0szqg8c41lhcw99y8i2gar0"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f
                     #:phases
                     #~(modify-phases %standard-phases
                         (add-after 'unpack 'hardcode-openssl
                           (lambda* (#:key inputs #:allow-other-keys)
                             (substitute* "src/kcm/kcmkrdpserver.cpp"
                               (("\"openssl\"")
                                (string-append
                                 "\""
                                 (search-input-file
                                  inputs "/bin/openssl")
                                 "\""))))))))
    (native-inputs (list extra-cmake-modules
                         pkg-config
                         ;; for wayland-scanner
                         wayland))
    (inputs (list
             freerdp-3
             kconfig
             kcmutils
             kcoreaddons
             kcrash
             kdbusaddons
             kguiaddons
             ki18n
             kpipewire
             kstatusnotifieritem
             linux-pam
             openssl
             plasma-wayland-protocols
             qtdeclarative
             qtkeychain-qt6
             qtwayland
             wayland-protocols
             wayland))
    (synopsis "Library and examples for creating an RDP server")
    (description "This package provides a library and examples for creating an
RDP server.")
    (home-page "https://invent.kde.org/plasma/krdp")
    (license license:lgpl2.0+)))

(define-public kde-gtk-config
  (package
    (name "kde-gtk-config")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kde-gtk-config-" version ".tar.xz"))
              (sha256
               (base32
                "0acd9vg2m9kjs4i4ys1vqwrgp8zp2caa52rwcrvfc35y04scsdyc"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-gsettings-schemas-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "cmake/modules/FindGSettingSchemas.cmake"
                (("\\$\\{PC_GLIB2_PREFIX\\}")
                 (assoc-ref inputs "gsettings-desktop-schemas"))))))))
    (native-inputs
     (list extra-cmake-modules pkg-config qtsvg sassc))
    (inputs
     (list gsettings-desktop-schemas
           gtk+
           kconfig
           kconfigwidgets
           kcoreaddons
           kguiaddons
           kdbusaddons
           kdecoration
           kwindowsystem
           xsettingsd))
    (home-page "https://invent.kde.org/plasma/kde-gtk-config")
    (synopsis "Sync of KDE settings to GTK applications")
    (description "This package provides tools to sync KDE settings to GTK
applications.")
    (license (list license:bsd-2 license:bsd-3 license:gpl2 license:gpl3))))

(define-public kdecoration
  (package
    (name "kdecoration")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kdecoration-" version ".tar.xz"))
              (sha256
               (base32
                "0xhmc704ql6smxwq2wl3szmbrl646l6q1cyb8kmcf71bzx2dqqr9"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons ki18n))
    (home-page "https://invent.kde.org/plasma/kdecoration")
    (synopsis "Plugin based library to create window decorations")
    (description "KDecoration is a library to create window decorations.
These window decorations can be used by for example an X11 based window
manager which re-parents a Client window to a window decoration frame.")
    (license license:lgpl3+)))

(define-public kde-cli-tools
  (package
    (name "kde-cli-tools")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (patches (search-patches "kde-cli-tools-delay-mime-db.patch"))
              (sha256
               (base32
                "070aib1005q1ciafvj89cv6h0aphl20808b00lnypyf63skiskw3"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f ;TODO: Failing 1 test
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-writable-location
                 (lambda* _
                   (substitute* "keditfiletype/tests/filetypestest.cpp"
                     (("QStandardPaths::writableLocation.QStandardPaths::\
GenericDataLocation.")
                      (string-append "\"" (getcwd) "/\"")))))
               (add-before 'check 'setup-env
                 (lambda* _
                   (setenv "HOME" (getcwd)))))))
    (native-inputs (list extra-cmake-modules pkg-config shared-mime-info))
    (inputs (list kconfig
                  kdesu
                  kdoctools
                  kiconthemes
                  ki18n
                  kcmutils
                  kio
                  kservice
                  kwindowsystem
                  plasma-activities
                  kparts
                  plasma-workspace
                  qtsvg
                  libxkbcommon))
    (synopsis "CLI tools for interacting with KDE")
    (description "This package provides command-line tools based on
KDE Frameworks 5 to better interact with the system.")
    (home-page "https://invent.kde.org/plasma/kde-cli-tools")
    (license license:lgpl2.0+)))

(define-public kdeplasma-addons
  (package
    (name "kdeplasma-addons")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "112np6ikkcksz5h4nza8i2k4y156nn7wrjphkgph7aqci6i1qnwk"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude "converterrunnertest"
           #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'check-setup
                          (lambda* (#:key inputs #:allow-other-keys)

                            (setenv "TZDIR"
                                    (search-input-directory
                                     inputs "share/zoneinfo")))))))
    (native-inputs (list extra-cmake-modules tzdata-for-tests))
    (inputs (list karchive
                  kauth
                  kconfig
                  kcoreaddons
                  kdeclarative
                  kdbusaddons
                  kholidays
                  ki18n
                  kio
                  kcmutils
                  kglobalaccel
                  kxmlgui
                  knotifications
                  krunner
                  kservice
                  ksvg
                  kunitconversion
                  knewstuff
                  libplasma
                  plasma5support
                  purpose
                  sonnet
                  qt5compat
                  ;; qtwebengine ; Optional for online dictionary
                  qtdeclarative))
    (synopsis "Add-ons to improve your Plasma experience")
    (description
     "This package provides multiple addons for the Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/kdeplasma-addons")
    (license license:lgpl2.0)))

(define-public kgamma
  (package
    (name "kgamma")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1vifg3mjlvq1f755cxnmqy9v4y54zypky6kbw5k6szpvrpzm70sk"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests?  #f))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list kauth
                  kcoreaddons
                  kconfig
                  kconfigwidgets
                  kcmutils
                  ki18n))
    (synopsis "Adjust monitor gamma settings")
    (description
     "This package provides a tool to adjust your monitor gamma settings.")
    (home-page "https://invent.kde.org/plasma/kgamma5")
    (license license:gpl2+)))

(define-public kglobalacceld
  (package
    (name "kglobalacceld")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1xpmlimr6y67r3m32mdwyyc1giz0b8ghdx4yyzbzyap2ajvwfm49"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:phases
                     #~(modify-phases %standard-phases
                         (add-before 'check 'setenv
                           (lambda _
                             (setenv "HOME" (getcwd))))
                         (replace 'check
                           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
                             (invoke "dbus-launch" "ctest" "-j"
                                     (if parallel-tests?
                                         (number->string (parallel-job-count))
                                         "1")))))))
    (native-inputs (list extra-cmake-modules dbus))
    (inputs (list kconfig
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kwindowsystem
                  kglobalaccel
                  kservice
                  kio
                  kjobwidgets
                  xcb-util-keysyms
                  libxkbcommon))
    (synopsis "Daemon providing Global Keyboard Shortcut (Accelerator)
functionality")
    (description
     "This package provides a Daemon providing Global Keyboard Shortcut
(Accelerator) functionality.")
    (home-page "https://invent.kde.org/plasma/kglobalacceld")
    (license license:gpl2+)))

(define-public kinfocenter
  (package
    (name "kinfocenter")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ja6c24a33gp8z12y6hbdmrdiqd6jzp4wazga7jrrivnkhb9mq7z"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-path
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((replace (lambda (file cmd)
                               (substitute* file
                                 (((string-append
                                    "\""
                                    cmd
                                    "\""))
                                  (string-append
                                   "\""
                                   (search-input-file
                                    inputs
                                    (string-append "/bin/" cmd))
                                   "\"")))))
                    (dmidecode (search-input-file inputs "/sbin/dmidecode")))
                (substitute* "CMakeLists.txt"
                  (("\\$\\{KDE_INSTALL_FULL_BINDIR\\}/systemsettings")
                   (search-input-file inputs
                                      "/bin/.systemsettings-real")))
                (substitute* "kcms/kwinsupportinfo/kcm_kwinsupportinfo.json.in"
                  (("@QtBinariesDir@/qdbus")
                   (search-input-file inputs "/bin/qdbus")))
                (substitute* "kcms/kwinsupportinfo/main.cpp"
                  (("QLibraryInfo::path\\(QLibraryInfo::BinariesPath\\) \\+ QStringLiteral\\(\"/qdbus\"\\)")
                   (string-append "QStringLiteral(\"" (search-input-file inputs "/bin/qdbus") "\")")))
                (substitute* "kcms/memory/kcm_memory.json"
                  (("pkexec dmidecode")
                   (string-append
                    "pkexec " dmidecode)))
                (substitute* "kcms/memory/main.cpp"
                  (("dmidecode") dmidecode))
                (substitute* '("kcms/firmware_security/main.cpp"
                               "kcms/firmware_security/fwupdmgr.sh"
                               "kcms/firmware_security/kcm_firmware_security.json")
                  (("aha") (search-input-file inputs "/bin/aha"))
                  (("\"fwupdmgr\"") (string-append "\"" (search-input-file inputs "/bin/fwupdmgr") "\""))
                  (("fwupdmgr security") (string-append (search-input-file inputs "/bin/fwupdmgr") " security"))
                  (("sed") (search-input-file inputs "/bin/sed"))
                  (("/bin/sh") (search-input-file inputs "/bin/sh")))
                (replace '("kcms/cpu/kcm_cpu.json"
                           "kcms/cpu/main.cpp") "lscpu")
                (replace '("kcms/opencl/kcm_opencl.json"
                           "kcms/opencl/main.cpp") "clinfo")
                (replace '("kcms/vulkan/kcm_vulkan.json"
                           "kcms/vulkan/main.cpp") "vulkaninfo")
                (replace '("kcms/glx/kcm_glx.json"
                           "kcms/glx/main.cpp") "glxinfo")
                (replace '("kcms/wayland/kcm_wayland.json"
                           "kcms/wayland/main.cpp") "wayland-info")
                (replace '("kcms/egl/kcm_egl.json"
                           "kcms/egl/main.cpp") "eglinfo")
                (replace '("kcms/xserver/kcm_xserver.json"
                           "kcms/xserver/main.cpp") "xdpyinfo")))))))
    (native-inputs (list aha extra-cmake-modules kdoctools pkg-config qttools))
    ;; * vulkaninfo
    ;; Wayland KCM
    (inputs (list bash-minimal
                  clinfo
                  dmidecode
                  eudev
                  fwupd
                  kauth
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kirigami
                  ki18n
                  kcmutils
                  kio
                  kservice
                  libusb
                  kwidgetsaddons
                  kdeclarative
                  kpackage
                  kwayland
                  mesa-utils
                  pciutils
                  libplasma
                  qttools
                  qtwayland
                  solid
                  util-linux
                  vulkan-tools
                  wayland-utils
                  xdpyinfo))
    (propagated-inputs (list system-settings))
    (home-page "https://invent.kde.org/plasma/kinfocenter")
    (synopsis "View information about computer's hardware")
    (description "This package provides tool to view information about
computer's hardware.")
    (license (list license:gpl2 license:gpl3))))

(define-public kmenuedit
  (package
    (name "kmenuedit")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0mfbrhi0yzinlj4sdplspsk50bxawkgq1mz8q6a0sgr79z3yyqcf"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list kcrash
                  ki18n
                  kxmlgui
                  kdbusaddons
                  kiconthemes
                  kio
                  kitemviews
                  sonnet
                  kglobalaccel
                  kwindowsystem
                  qtwayland))
    (synopsis "Menu Editor for Plasma Workspaces")
    (description "This package provides menu editor for Plasma Workspaces.")
    (home-page "https://invent.kde.org/plasma/kmenuedit")
    (license license:gpl2+)))

(define-public knighttime
  (package
    (name "knighttime")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "03czrz2ln4a2xhn98lq6ga24124wakwsknkx09g4w0dkbfjczz6g"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules qttools))
    (inputs (list kconfig
                  kcoreaddons
                  kdbusaddons
                  kholidays
                  ki18n
                  qtpositioning))
    (synopsis "Helpers for scheduling the dark-light cycle")
    (description "KNightTime provides helpers for scheduling the dark-light
cycle.  It can be used to implement features such as adjusting the screen color
temperature based on time of day, etc.")
    (home-page "https://invent.kde.org/plasma/knighttime")
    (license license:lgpl3+)))

(define-public koi
  (package
    (name "koi")
    (version "0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baduhai/Koi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xhrdkfsyn2qpiscp4smd2gr4vfplaid1m6lqh4lhasscvwx45k1"))))
    (build-system qt-build-system)
    (inputs (list kconfig
                  kcoreaddons
                  kconfigwidgets
                  kdbusaddons
                  kde-cli-tools
                  kvantum
                  plasma-workspace
                  qtwayland))
    (arguments
     (list
      #:modules '((ice-9 ftw)
                  (guix build qt-build-system)
                  (guix build utils))
      #:tests? #f
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-hardcoded-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Correct theme search paths.  This won't be needed if the
              ;; following request is implemented:
              ;; https://github.com/baduhai/Koi/issues/123
              (ftw "."
                   (lambda (file _ flag)
                     (when (and (eq? flag 'regular)
                                (or (string-suffix? ".c" file)
                                    (string-suffix? ".cpp" file)))
                       (substitute* file
                         (("/var/run/current-system/sw")
                          "/run/current-system/profile")))))
              ;; Correct executable paths.
              (substitute* "src/utils.cpp"
                (("/usr/bin/kquitapp6")
                 (search-input-file inputs "bin/kquitapp6"))
                (("/usr/bin/kstart")
                 (search-input-file inputs "bin/kstart")))
              (substitute* "src/plugins/plasmastyle.cpp"
                (("/usr/bin/plasma-apply-desktoptheme")
                 (search-input-file inputs "bin/plasma-apply-desktoptheme")))
              (substitute* "src/plugins/kvantumstyle.cpp"
                (("/usr/bin/kvantummanager")
                 (search-input-file inputs "bin/kvantummanager")))
              (substitute* "src/plugins/colorscheme.cpp"
                (("programToLocate = \\{\"plasma-apply-colorscheme\"\\}")
                 (string-append "programToLocate = {\""
                                (search-input-file inputs
                                 "bin/plasma-apply-colorscheme") "\"}")))
              (substitute* "src/plugins/icons.cpp"
                (("programToLocate = \\{\"plasma-changeicons\"\\}")
                 (string-append "programToLocate = {\""
                                (search-input-file inputs
                                 "libexec/plasma-changeicons") "\"}")))
              (substitute* "src/plugins/script.cpp"
                (("programToLocate = \\{\"bash\"\\}")
                 (string-append "programToLocate = {\""
                                (search-input-file inputs "bin/bash") "\"}"))))))))
    (home-page "https://github.com/baduhai/Koi")
    (synopsis "Theme scheduling for the KDE Plasma Desktop")
    (description
     "Koi is a program designed to provide the KDE Plasma Desktop functionality
to automatically switch between light and dark themes.")
    (license license:lgpl3)))

(define-public kpipewire
  (package
    (name "kpipewire")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "12gyj090paplybbgvx0i2h01bsghbkhbg9gaajidwlpw11ylbbss"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (propagated-inputs (list qtbase qtdeclarative
                             ;; include/KPipeWire/dmabufhandler.h include it.
                             libepoxy))
    (inputs (list libxkbcommon
                  libva
                  pipewire
                  ffmpeg-6
                  kcoreaddons
                  ki18n
                  kwayland
                  plasma-wayland-protocols
                  qtwayland
                  wayland
                  wayland-protocols))
    (arguments
     ;; The only test require run pipewire.
     (list #:tests? #f))
    (home-page "https://invent.kde.org/plasma/kpipewire")
    (synopsis "Components relating to pipewire use in Plasma")
    (description "This package offers a set of convenient classes to use
PipeWire in Qt projects.")
    ;; LGPL-2.1-only OR LGPL-3.0-only OR LicenseRef-KDE-Accepted-LGPL
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public kscreen
  (package
    (name "kscreen")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0s64p7bv8pfkg54b0cjp1r601j5zhl527rn9q1pijc4p5za4z0cw"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: All tests fail
     (list #:tests? #f))
    (native-inputs (list extra-cmake-modules qttools pkg-config
                         wayland
                         wayland-protocols
                         qtwayland))
    (inputs (list kcmutils
                  kconfig
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kglobalaccel
                  ki18n
                  kiconthemes
                  kimageformats
                  ksvg
                  kwindowsystem
                  kxmlgui
                  layer-shell-qt
                  libkscreen
                  libplasma
                  libxi
                  libxkbcommon
                  plasma-wayland-protocols
                  qtbase
                  qtsensors
                  qtwayland
                  xcb-util))
    (home-page "https://invent.kde.org/plasma/kscreen")
    (synopsis "Screen management software")
    (description "This package provides the screen management software for
KDE Plasma Workspaces.")
    (license license:gpl2+)))

(define-public ksshaskpass
  (package
    (name "ksshaskpass")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/ksshaskpass-" version ".tar.xz"))
              (sha256
               (base32
                "0qlckp5cjzpx7v5cjsjgwbvm8fshwbfj1mcvq8mpfjs6vsysrkg1"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons ki18n kwallet kwidgetsaddons qtwayland))
    (home-page "https://invent.kde.org/plasma/ksshaskpass")
    (synopsis "Front-end for ssh-add using kwallet")
    (description "Ksshaskpass is a front-end for @code{ssh-add} which stores the
password of the ssh key in KWallet.  Ksshaskpass is not meant to be executed
directly, you need to tell @code{ssh-add} about it.  @code{ssh-add} will then
call it if it is not associated to a terminal.")
    (license license:gpl2+)))

(define-public ksystemstats
  (package
    (name "ksystemstats")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1jkgi3js84lwakb853i2qgmk0nrk0arm2c4ygf9x1r9g4jip5hnq"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude "ksystemstatstest"))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list glib
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  solid
                  networkmanager-qt
                  kiconthemes
                  kio
                  ki18n
                  libksysguard
                  libnl
                  eudev
                  `(,lm-sensors "lib")
                  network-manager))
    (synopsis "Plugin based system monitoring daemon")
    (description
     "This package provides a daemon that collects statistics about
the running system.")
    (home-page "https://invent.kde.org/plasma/ksystemstats")
    (license (list license:gpl2 license:gpl3))))

(define-public layer-shell-qt
  (package
    (name "layer-shell-qt")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/layer-shell-qt-" version ".tar.xz"))
              (sha256
               (base32
                "0gb397a9siyhnz9diy0h6b1xpn92774a0sc1p9c66azvwf4bnc25"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list libxkbcommon
           qtdeclarative
           qtwayland
           wayland
           wayland-protocols))
    (home-page "https://invent.kde.org/plasma/layer-shell-qt")
    (synopsis "Qt component for the Wayland ql-layer-shell protocol")
    (description "Qt component for the Wayland ql-layer-shell protocol.")
    (license license:gpl2+)))

(define-public kscreenlocker
  (package
    (name "kscreenlocker")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kscreenlocker-" version ".tar.xz"))
              (sha256
               (base32
                "1dh41lfqik7pzqk7px6jmbv68pm9ny1mmp8d6x20qjlni158j2fh"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f ;TODO: make tests pass
           #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'check-setup
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (system "Xvfb :1 -screen 0 640x480x24 &")
                            (setenv "DISPLAY" ":1")))
                        (delete 'check)
                        ;; Tests use the installed library and require a DBus session.
                        (add-after 'install 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (if tests?
                                (begin
                                  (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                                  (invoke "dbus-launch" "ctest"))))))))
    (native-inputs (list extra-cmake-modules pkg-config
                         ;; for WaylandScanner
                         wayland
                         ;; For tests.
                         dbus xorg-server-for-tests))
    (inputs (list kcmutils
                  kconfig
                  kcrash
                  kglobalaccel
                  ki18n
                  kio
                  kidletime
                  knotifications
                  kwayland
                  kwindowsystem
                  kxmlgui
                  ksvg
                  layer-shell-qt
                  libkscreen
                  libplasma
                  libxi ;XInput, required for grabbing XInput2 devices
                  linux-pam
                  libxkbcommon
                  elogind ;optional loginctl support
                  qtdeclarative
                  solid
                  wayland
                  xcb-util-keysyms))
    (home-page "https://invent.kde.org/plasma/kscreenlocker")
    (synopsis "Screen locking library")
    (description
     "@code{kscreenlocker} is a library for creating secure lock screens.")
    (license license:gpl2+)))

(define-public libkscreen
  (package
    (name "libkscreen")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0qdsw78na38q42sr2m3f5jrs5rp8zpbl7d3r9pvklzca6ia9f298"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:test-exclude "kscreen-testqscreenbackend"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'check-env-setup
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" (getcwd))
                (with-output-to-file "autotests/BLACKLIST"
                  (lambda _
                    (for-each
                     (lambda (name)
                       (display (string-append "[" name "]\n*\n")))
                     (list
                      "verifyOutputs"
                      ;; also fail on upstream
                      "testEdidParser"
                      "testEnv"
                      "testPreferredBackend"
                      "testFallback"
                      "testModeSwitching"
                      "verifyConfig"))))))))))
    (native-inputs
     (list extra-cmake-modules
           pkg-config
           qttools
           ;; For testing.
           dbus))
    (inputs
     (list kwayland libxrandr plasma-wayland-protocols qtwayland
           wayland
           libxkbcommon))
    (home-page "https://community.kde.org/Solid/Projects/ScreenManagement")
    (synopsis "KDE's screen management software")
    (description "KScreen is the new screen management software for KDE Plasma
Workspaces which tries to be as magic and automatic as possible for users with
basic needs and easy to configure for those who want special setups.")
    (license license:gpl2+)))

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32 "1ys5i4zx6kx3vyci2wigpfm1dg8s9nyf68dszkv7r56af4la48sd"))))
    (native-inputs
     (list bash-minimal extra-cmake-modules pkg-config qttools))
    (inputs
     (list kauth
           kconfig
           kcoreaddons
           ki18n
           knewstuff
           kpackage
           kservice
           libnl
           libcap
           libpcap
           `(,lm-sensors "lib")
           qtdeclarative
           solid
           zlib))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'fix-test
                     (lambda* _
                       (substitute* "autotests/processtest.cpp"
                         (("/bin/sh")
                          (which "bash"))))))))
    (home-page "https://userbase.kde.org/KSysGuard")
    (synopsis "Network enabled task and system monitoring")
    (description "KSysGuard can obtain information on system load and
manage running processes.  It obtains this information by interacting
with a ksysguardd daemon, which may also run on a remote system.")
    (license license:gpl3+)))

(define-public kwallet-pam
  (package
    (name "kwallet-pam")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0xp8i8kgdzb2mr8p1388nfifdixzvv159annhd1dgvhijjjzmhsz"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'fix-socat-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (string-append #$output
                                          "/libexec/pam_kwallet_init")
                (("socat")
                 (search-input-file inputs "bin/socat"))))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list linux-pam kwallet libgcrypt socat))
    (synopsis "PAM Integration with KWallet")
    (description "Provide PAM Integration with KWallet to unlock KWallet when
you login.")
    (home-page "https://invent.kde.org/plasma/kwallet-pam")
    (license (list license:lgpl2.1+))))

(define-public kwin
  (package
    (name "kwin")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "kwin-unwrap-executable-name-for-dot-desktop-search.patch"))
              (sha256
               (base32
                "0x072hzmk4mgx0w41s01nrb70bd3znpbdfjrx3gasb9lzrlxbq1v"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:test-exclude
      (string-append "("
                     (string-join
                         `(;; Fails on an Apple M1 (aarch64) with the following error:
                           ;; TestColorspaces::roundtripConversion fails
                           "kwin-testColorspaces"

                           "kwin-testDrm" ;; require Drm
                           "kwin-testInputMethod"
                           "kwin-testPlasmaWindow" ;; require plasma-workspace qml module.
                           "kwin-testButtonRebind"
                           "kwin-testDecorationInput"
                           "kwin-testPointerInput"
                           "kwin-testXdgShellWindow"
                           "kwin-testXdgShellWindow-waylandonly"
                           "kwin-testSceneOpenGLES"
                           "kwin-testSceneOpenGLES-waylandonly"
                           "kwin-testNightColor"
                           "kwin-testNightColor-waylandonly"
                           "kwin-testScriptedEffects"
                           "kwayland-testServerSideDecoration"
                           "kwayland-testWaylandSurface"

                           "kwin-testLibinputDevice"
                           "kwin-testLockScreen"
                           "kwin-testTabBox"
                           "kwin-testKeyboardInput"
                           "kwin-testKeyboardLayout"
                           "kwin-testQuickTiling"
                           "kwin-testDbusInterface"
                           "kwin-testX11KeyRead"
                           "kwin-testVirtualKeyboardDBus"
                           "kwin-testGlobalShortcuts"
                           "kwin-testKWinBindings"
                           "kwin-testMinimizeAllScript"
                           "kwin-testLibinputDevice"
                           "kwin-testX11Window"
                           "kwin-testXwaylandInput"
                           "kwin-testWindowRules"

                           "kwin-testXdgShellWindowRules"
                           "kwin-testStickyKeys"
                           "kwin-testFractionalRepaint"
                           "kwin-testDrmLegacy"
                           "kwin-testInputCapture"
                           "kwin-testMockDrm"
                           ,@(if (target-aarch64?)
                                 '("kwin-testSecurityContext"
                                   "kwin-testXwaylandSelection")
                                 '()))
                         "|")
                     ")")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute*
                  '("autotests/integration/dont_crash_glxgears.cpp"
                    "autotests/integration/debug_console_test.cpp"
                    "autotests/integration/x11_window_test.cpp")
                (("setProgram\\(QStringLiteral\\(\"glxgears\"\\)")
                 (string-append
                  "setProgram(QByteArrayLiteral(\"" (which "glxgears") "\")")))
              (substitute*
                  '("tests/renderingservertest.cpp"
                    "tests/waylandservertest.cpp")
                (("QByteArrayLiteral\\(\"Xwayland\"\\)")
                 (string-append
                  "QByteArrayLiteral(\"" (which "Xwayland") "\")")))
              (substitute* '("src/xwayland/xwaylandlauncher.cpp")
                (("findExecutable\\(\"Xwayland\"\\)")
                 (string-append
                  "findExecutable(\""
                  (search-input-file inputs "/bin/Xwayland") "\")")))
              ;; https://github.com/NixOS/nixpkgs/blob/6da4bc6cb07cba1b8e53d139cbf1d2fb8061d967/pkgs/desktops/plasma-5/kwin/0003-plugins-qpa-allow-using-nixos-wrapper.patch
              (substitute* "src/plugins/qpa/main.cpp"
                (("(\\(QLatin1String\\(\"kwin_wayland\"\\)\\))" _ start)
                 (string-append start " && !QCoreApplication::applicationFilePath()\
.endsWith(QLatin1String(\".kwin_wayland-real\"))" )))
              (substitute* '("cmake/modules/Findhwdata.cmake")
                (("/usr/share")
                 (string-append #$(this-package-input "hwdata") "/share")))))
          (replace 'check
            (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
              (when tests?
                (setenv "XDG_RUNTIME_DIR" (getcwd))
                (setenv "HOME" (getcwd))
                (setenv "XDG_DATA_DIRS"
                        (string-append #$output "/share:"
                                       (getenv "XDG_DATA_DIRS")))
                (setenv "QT_PLUGIN_PATH"
                        (string-append #$output
                                       "/lib/qt6/plugins:"
                                       (getenv "QT_PLUGIN_PATH")))
                (setenv "DISPLAY" ":1")
                (system "Xvfb :1 &")
                (sleep 5)
                (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
    (native-inputs (list extra-cmake-modules
                         dbus
                         kdoctools
                         mesa-utils
                         pkg-config
                         qttools
                         wayland-protocols
                         xorg-server-for-tests
                         python-minimal
                         ;; for QtWaylandScanner
                         qtwayland))
    (inputs (list aurorae
                  breeze
                  eudev
                  fontconfig
                  freetype
                  hwdata
                  kauth
                  kcmutils
                  kcompletion
                  kconfig
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kdecoration
                  kglobalaccel
                  kglobalacceld
                  kguiaddons
                  ki18n
                  kiconthemes
                  kidletime
                  kio
                  kirigami
                  knewstuff
                  knighttime
                  knotifications
                  kpackage
                  kpipewire
                  krunner
                  kscreenlocker
                  ktextwidgets
                  kwayland
                  kwindowsystem
                  kxmlgui
                  ksvg
                  lcms
                  libcanberra
                  libcap
                  libdisplay-info
                  libei
                  libepoxy
                  libinput
                  libplasma
                  libqaccessibilityclient
                  libxkbcommon
                  pipewire
                  plasma-activities
                  plasma-wayland-protocols
                  qt5compat
                  qtdeclarative
                  qtmultimedia
                  qtwayland
                  qtsensors
                  qtsvg
                  wayland
                  xcb-util ;fails at build time without this
                  xcb-util-cursor
                  xcb-util-keysyms
                  xcb-util-wm
                  xcmsdb
                  xinput ;XXX: Says disabled in configure phase
                  xorg-server-xwayland
                  zlib))
    ;; Runtime-only dependency needed for mapping monitor hardware vendor IDs to full names
    ;; * QtQuick.Controls-QMLModule, QML module 'QtQuick.Controls' is a runtime dependency.
    ;; * org.kde.plasma.core-QMLModule, QML module 'org.kde.plasma.core' is a runtime dependency.
    ;; * org.kde.plasma.components-QMLModule, QML module 'org.kde.plasma.components' is a runtime dependency.
    (home-page "https://userbase.kde.org/KWin")
    (synopsis "KDE Plasma Window Manager")
    (description
     "KWin is an easy to use, but flexible, composited Window Manager for
Xorg windowing systems (Wayland, X11) on Linux.  Its primary usage is in
conjunction with the KDE Plasma Desktop.")
    (license license:gpl2+)))

(define-public kwrited
  (package
    (name "kwrited")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "12895frz78vsabc9072d8jn6v5a38x1z6wrqc2mcwv3hmhlmz580"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons ki18n kpty knotifications))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://invent.kde.org/plasma/kwrited")
    (synopsis "System notification daemon")
    (description
     "This package provides a daemon that listens to system notifications.")
    (license license:gpl2+)))

(define-public milou
  (package
    (name "milou")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1gcvwx0kip5fnh9g1kx34w7l398yz6vnri13096a4khm3b27kcw4"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  ki18n
                  kdeclarative
                  kitemmodels
                  kservice
                  libplasma
                  kwindowsystem
                  krunner
                  ksvg
                  qtdeclarative))
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (synopsis "Dedicated search application built on top of Baloo")
    (description "This package provides a dedicated search application built
on top of Baloo.")
    (home-page "https://invent.kde.org/plasma/milou")
    (license (list license:gpl2+))))

(define-public qqc2-breeze-style
  (package
    (name "qqc2-breeze-style")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10bcjn8lva42wic514bg6lcrw3chpdagmjbqgq626y65cymksj6m"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules))
    (inputs (list qtdeclarative
                  kiconthemes kguiaddons kconfig kirigami
                  kcolorscheme kquickcharts))
    (home-page "https://invent.kde.org/plasma/qqc2-breeze-style")
    (synopsis "Breeze inspired Qt Quick Controls Style")
    (description "This package provides Breeze inspired Qt Quick Controls Style.")
    (license (list license:lgpl2.0+ license:gpl2+))))

(define-public oxygen-sounds
  (package
    (name "oxygen-sounds")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "05z6lav3fm0hs9yqf4734chnf8nmi290d3pf5914h8adzj0smx8h"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Sounds for the KDE desktop")
    (description "This package provides Oxygen sounds for the KDE desktop.")
    (license license:lgpl3+)))

(define-public ocean-sound-theme
  (package
    (name "ocean-sound-theme")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/ocean-sound-theme"  "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "14gf0qpp4wyh3w29sw50w1i1dagbq3d70fihwsiqlkz5169vafvy"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtbase))
    (home-page "https://invent.kde.org/plasma/ocean-sound-theme")
    (synopsis "Ocean Sound Theme for Plasma")
    (description "This package provides Ocean Sound Theme for Plasma.")
    (license license:lgpl3+)))

(define-public xdg-desktop-portal-kde
  (package
    (name "xdg-desktop-portal-kde")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0jdjgayq3ylm1grzy7g3vvj94c6gi9ls6gh2d21xymqhxp19sbka"))))
    (build-system qt-build-system)
    (arguments (list
                #:tests? #f ;; colorschemetest test fail, because require dbus.
                #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules pkg-config
                         ;; require by test.
                         python-minimal
                         python-pygobject))
    (inputs (list cups
                  kcoreaddons
                  kconfig
                  kcrash
                  ki18n
                  kdeclarative
                  kio
                  kirigami
                  knotifications
                  libplasma
                  plasma-wayland-protocols
                  kstatusnotifieritem
                  kwayland
                  kwidgetsaddons
                  kwindowsystem
                  kiconthemes
                  qtdeclarative
                  qtwayland
                  wayland
                  kglobalaccel
                  kguiaddons
                  libxkbcommon
                  wayland-protocols))
    (propagated-inputs
     (list xdg-desktop-portal))
    (synopsis "Backend implementation for xdg-desktop-portal using Qt/KF5")
    (description "This package provides a backend implementation
for xdg-desktop-portal that is using Qt/KF5.")
    (home-page "https://invent.kde.org/plasma/xdg-desktop-portal-kde")
    (license license:lgpl2.0+)))

(define-public plasma
  (package
    (name "plasma")
    (version (package-version breeze))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder #~(begin
                         (mkdir #$output))))
    ;; TODO: cleanup, check what is no need
    (propagated-inputs (list aurorae
                             baloo
                             bluedevil
                             breeze
                             breeze-gtk
                             breeze-icons ;default mouse icon
                             discover
                             kactivitymanagerd
                             kdeclarative ;required by sddm breeze theme
                             kde-cli-tools
                             kdecoration
                             kde-gtk-config
                             kded
                             kdeplasma-addons
                             kglobalaccel
                             kglobalacceld
                             kiconthemes ;required by sddm breeze theme
                             kinfocenter
                             kmenuedit
                             krdp
                             kscreen
                             kscreenlocker
                             ksshaskpass
                             ksvg ;required by sddm breeze theme
                             ksystemstats
                             ktexteditor
                             kwallet
                             kwallet-pam
                             kwin
                             layer-shell-qt
                             libkscreen
                             libksysguard
                             milou
                             ocean-sound-theme
                             oxygen-sounds
                             packagekit     ;for discover
                             plasma5support ;required by sddm breeze theme
                             plasma-browser-integration
                             plasma-desktop
                             plasma-disks
                             plasma-firewall
                             plasma-integration
                             plasma-nm
                             plasma-pa
                             plasma-systemmonitor
                             plasma-vault
                             plasma-welcome
                             plasma-workspace
                             plasma-workspace-wallpapers
                             polkit-kde-agent
                             powerdevil
                             qqc2-breeze-style
                             qqc2-desktop-style ;for qtquickcontrols2 theme
                             qt5compat ;required by sddm breeze theme
                             qtbase ;for QT_PLUGIN_PATH and QML_IMPORT_PATH
                             qtdeclarative
                             qtsvg
                             system-settings
                             xdg-desktop-portal-kde
                             ;; module cyclic referencing
                             (module-ref
                              (resolve-interface
                               '(gnu packages kde-systemtools))
                              'dolphin)
                             (module-ref
                              (resolve-interface
                               '(gnu packages kde-systemtools))
                              'konsole)
                             (module-ref
                              (resolve-interface
                               '(gnu packages kde-systemtools))
                              'kpmcore)
                             (module-ref
                              (resolve-interface
                               '(gnu packages kde-systemtools))
                              'kwalletmanager)
                             (module-ref
                              (resolve-interface
                               '(gnu packages kde-systemtools))
                              'partitionmanager)
                             spectacle))
    ;; plasma-thunderbolt ;waiting for bolt
    (synopsis "The KDE Plasma desktop environment")
    (home-page "https://kde.org/plasma-desktop/")
    (description
     "KDE Plasma is an advanced graphical desktop system.")
    (license license:gpl2+)))

(define-public plasma-activities
  (package
    (name "plasma-activities")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-activities-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "16j4lyzgi2drlivn3m7aph06sx9m879n94ml452l00rw9wr3sy1r"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kconfig
           kcoreaddons
           kwindowsystem
           qtdeclarative
           solid))
    (arguments
     (list
       #:tests? #f
       #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma/plasma-activities")
    (synopsis "Core components for the KDE Activity System")
    (description "KActivities provides the infrastructure needed to manage a
user's activities, allowing them to switch between tasks, and for applications
to update their state to match the user's current activity.  This includes a
daemon, a library for interacting with that daemon, and plugins for integration
with other frameworks.")
    ;; triple licensed
    (license (list license:gpl2+ license:lgpl2.0+ license:lgpl2.1+))))

(define-public plasma-activities-stats
  (package
    (name "plasma-activities-stats")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-activities-stats-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "004hyi99hr3zviigahlgrfr8g0jyr9q1d2g030hh38yp9qb5p9mr"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list boost plasma-activities kconfig qtbase qtdeclarative))
    (home-page "https://invent.kde.org/plasma/plasma-activities-stats")
    (synopsis "Access usage statistics collected by the activity manager")
    (description "The KActivitiesStats library provides a querying mechanism for
the data that the activity manager collects---which documents have been opened
by which applications, and what documents have been linked to which activity.")
    ;; triple licensed
    (license (list license:lgpl2.0+ license:lgpl2.1+ license:lgpl3+))))

(define-public plasma5support
  (package
    (name "plasma5support")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vxqfizki70gr3g586y71lmsysp71y85lqz2j6vl5w2bik4m25c1"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f)) ;no tests
    (native-inputs (list extra-cmake-modules pkg-config))
    (propagated-inputs (list kcoreaddons))
    (inputs (list
             kconfig
             ki18n
             kio
             kidletime
             kguiaddons
             kholidays
             knotifications
             kservice
             kunitconversion
             libksysguard
             networkmanager-qt
             plasma-activities
             qtdeclarative
             solid))
    (home-page "https://invent.kde.org/plasma/plasma5support")
    (synopsis "Support components for porting from KF5/Qt5 to KF6/Qt6")
    (description "This package provides support components for porting from
KF5/Qt5 to KF6/Qt6")
    (license (list license:lgpl2.0+))))

(define-public plasma-browser-integration
  (package
    (name "plasma-browser-integration")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1aq2cvd9r113vdzbvqwyv9whimg7mpmzjphjhbksnjph98qlhnw1"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    ;; TODO: Figure out how to integrate this package into web browsers
    ;; CHROMIUM_EXTENSIONS_DIR - extension for chromium
    ;; MOZILLA_DIR - extension for firefox
    (inputs (list kio
                  ki18n
                  kcoreaddons
                  kconfig
                  kcrash
                  kdbusaddons
                  knotifications
                  kitemmodels
                  krunner
                  plasma-activities
                  purpose
                  kfilemetadata
                  kjobwidgets
                  kstatusnotifieritem
                  qtdeclarative))
    (propagated-inputs (list plasma-workspace))
    (arguments (list #:tests? #f        ; no tests
                     #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma/plasma-browser-integration")
    (synopsis "Integrate browsers into the Plasma Desktop")
    (description
     "This package aims to provide better integration of web browsers with
the KDE Plasma 6 desktop.")
    (license license:gpl3+)))

(define-public plasma-desktop
  (package
    (name "plasma-desktop")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0n3sdhq70g8xcmgfcgcx1ridznbj0x7bprvk0qzzbrn83wsv7cmh"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules
                         dbus
                         kdoctools
                         intltool
                         pkg-config
                         qtsvg
                         qttools
                         libxml2
                         ;; require QtWaylandScanner
                         qtwayland))
    (inputs (list packagekit-qt6
                  signon-plugin-oauth2
                  signond-qt6
                  attica
                  appstream-qt6
                  baloo
                  breeze
                  breeze-icons
                  eudev
                  fontconfig
                  glib
                  ibus
                  kaccounts-integration
                  plasma-activities
                  plasma-activities-stats
                  kauth
                  karchive
                  kcmutils
                  kconfig
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kded
                  kdesu
                  kglobalaccel
                  kguiaddons
                  kholidays
                  ki18n
                  kiconthemes
                  kidletime
                  kio
                  kitemmodels
                  knewstuff
                  knotifications
                  knotifyconfig
                  kpackage
                  kpeople
                  kpipewire
                  krunner
                  kscreenlocker
                  ktexteditor
                  ktextwidgets
                  kunitconversion
                  kuserfeedback
                  kwallet
                  kwayland
                  kwin
                  ksvg
                  plasma5support
                  layer-shell-qt
                  libaccounts-qt6
                  libcanberra
                  libkscreen
                  libksysguard
                  libqalculate
                  libwacom
                  gmp
                  mpfr
                  libsm
                  libxi
                  libxft
                  libxkbcommon
                  libxrender
                  libxtst
                  networkmanager-qt
                  phonon
                  pipewire
                  libplasma
                  plasma-wayland-protocols
                  pulseaudio
                  prison
                  qqc2-desktop-style
                  qt5compat
                  qtdeclarative
                  qtshadertools
                  qtwayland
                  sdl2
                  wayland
                  wayland-protocols
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xdg-user-dirs

                  ;; These are needed for Xserver
                  xf86-input-libinput
                  xf86-input-evdev
                  xorg-server
                  xf86-input-synaptics
                  xkeyboard-config
                  libxkbfile
                  libxcursor
                  libxkbcommon))
    (propagated-inputs (list font-google-noto-emoji
                             iso-codes/pinned
                             kirigami
                             kirigami-addons
                             kcmutils
                             plasma-workspace))
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           ;; The tst_calibrationtool test fails on aarch64 due to floating-point
           ;; precision: QMatrix4x4 comparison fails with tiny differences
           ;; (1.11759e-08 instead of exact 0).
           (string-append "("
                          (string-join `("positionertest"
                                         "kcm-keyboard-keyboard_memory_\
persister_test"
                                         "foldermodeltest"
                                         ,@(if (target-aarch64?)
                                               '("tst_calibrationtool")
                                               '()))
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-wallpaper
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "sddm-theme/theme.conf.cmake"
                     (("background=..KDE_INSTALL_FULL_WALLPAPERDIR.")
                      (string-append "background="
                                     #$(this-package-input "breeze")
                                     "/share/wallpapers")))))
               (add-after 'unpack 'fix-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "kcms/keyboard/iso_codes.h"
                     (("\"/usr/share/xml/iso-codes\"")
                      (string-append "\"" (search-input-directory
                                           inputs "/share/xml/iso-codes")
                                     "\"")))))
               (add-before 'check 'check-setup
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (setenv "XDG_RUNTIME_DIR" (getcwd))
                     (setenv "XDG_CACHE_HOME" (getcwd))))))))
    (home-page "https://kde.org/plasma-desktop/")
    (synopsis "Plasma for the Desktop")
    (description
     "Plasma Desktop offers a beautiful looking desktop that takes
complete advantage of modern computing technology.  Through the use of visual
effects and scalable graphics, the desktop experience is not only smooth but
also pleasant to the eye.  The looks of Plasma Desktop not only provide
beauty, they are also used to support and improve your computer
activities effectively, without being distracting.")
    (license license:gpl2+)))

(define-public plasma-disks
  (package
    (name "plasma-disks")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0yqxypfqh6ainn0mxhkbdawr5yaalrcdh3zvnxrdmm6r74zqbjcs"))))
    (build-system qt-build-system)
    (arguments (list
                #:qtbase qtbase
                #:phases
                #~(modify-phases %standard-phases
                    (add-after 'unpack 'set-smartctl-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        (substitute* "src/helper.cpp"
                          (("\"smartctl\"")
                           (string-append
                            "\""
                            (search-input-file
                             inputs "/sbin/smartctl")
                            "\""))))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  kdbusaddons
                  knotifications
                  ki18n
                  kcmutils
                  solid
                  kservice
                  kio
                  kauth
                  kdeclarative
                  smartmontools))
    (synopsis "Monitors S.M.A.R.T. capable devices for imminent failure")
    (description "This package provides interface to S.M.A.R.T. data of disks.")
    (home-page "https://invent.kde.org/plasma/plasma-disks")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-firewall
  (package
    (name "plasma-firewall")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1ps1b8avl6mwld3mxlair1z7lcnkm04j21srzdpapd57xx6j84ys"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules))
    (inputs (list iproute
                  kauth
                  kcoreaddons
                  kcmutils
                  ki18n
                  kdeclarative
                  python
                  qtdeclarative))
    (synopsis "Control Panel for system firewall")
    (description "This package provides interface to system firewall.")
    (home-page "https://invent.kde.org/plasma/plasma-firewall")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-integration
  (package
    (name "plasma-integration")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1r3ybzpcjyyzl6dpkxirixdjiwi47q0yfkvmhgv9zkv1bbyzq4wa"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags #~(list "-DBUILD_QT5=OFF")
           #:tests? #f)) ;TODO: Failing tests
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list breeze
                  kconfig
                  kconfigwidgets
                  kguiaddons
                  ki18n
                  kiconthemes
                  kio
                  knotifications
                  kstatusnotifieritem
                  kwayland
                  kwidgetsaddons
                  kxmlgui
                  libxcb
                  libxcursor
                  libxkbcommon
                  plasma-wayland-protocols
                  qtdeclarative
                  qtwayland
                  wayland
                  xdg-desktop-portal-kde
                  font-google-noto-sans-cjk
                  font-google-noto-emoji
                  font-hack))
    (home-page "https://invent.kde.org/plasma/plasma-integration")
    (synopsis
     "Qt Platform Theme integration plugins for the Plasma workspaces")
    (description
     "This package provides a set of plugins responsible for better
integration of Qt applications when running on a KDE Plasma workspace.")
    (license license:lgpl2.0)))

(define-public plasma-nano
  (package
    (name "plasma-nano")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-nano-" version ".tar.xz"))
              (sha256
               (base32
                "00xjaj2mnvny6ylqvxpv2lbbhza470l0qkv8k60b0lxa222dcg62"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list qtbase
                  qtdeclarative
                  qtsvg
                  libplasma
                  kservice
                  kitemmodels
                  kwindowsystem
                  kwayland
                  ki18n))
    (arguments (list #:tests? #f))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Minimal Plasma Shell package")
    (description
     "This package provides a minimal implementation of Plasma Shell.")
    (license license:lgpl2.0+)))

(define-public plasma-nm
  (package
    (name "plasma-nm")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1p3zg2x7z82nhdqwbpp8rwgi34p07c83m7jxmkgdirmqi71ykhz7"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules pkg-config))
    (home-page "https://invent.kde.org/plasma/plasma-nm")
    (inputs (list kconfigwidgets
                  kcompletion
                  kcoreaddons
                  kcmutils
                  kdeclarative
                  kdbusaddons
                  kio
                  ki18n
                  networkmanager-qt
                  knotifications
                  kirigami
                  libplasma
                  mobile-broadband-provider-info
                  modemmanager-qt
                  network-manager
                  qca-qt6
                  kservice
                  solid
                  prison
                  kwallet
                  kwidgetsaddons
                  kwindowsystem
                  ksvg
                  qcoro-qt6
                  openconnect
                  qtdeclarative))
    (synopsis "Plasma applet for managing network connections")
    (description "This package provides Plasma applet for managing network
connections.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public plasma-mobile
  (package
    (name "plasma-mobile")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-mobile-" version ".tar.xz"))
              (sha256
               (base32
                "0rw0ic3yk54ax04idjm00nicj7780s6518b4vjpcssnldq25vjkf"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;only small add-hoc test scripts
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list bash-minimal
                  eudev
                  kauth
                  kcmutils
                  kdbusaddons
                  kdeclarative
                  kglobalaccel
                  ki18n
                  kio
                  kirigami-addons
                  kitemmodels
                  knotifications
                  kpipewire
                  kwayland
                  kwin
                  layer-shell-qt
                  libepoxy
                  libkscreen
                  libplasma
                  libxkbcommon
                  modemmanager-qt
                  networkmanager-qt
                  plasma-activities
                  plasma-wayland-protocols
                  plasma-workspace
                  qcoro-qt6
                  qtbase
                  qtsensors
                  qtwayland
                  wayland))
    (home-page "https://plasma-mobile.org/")
    (synopsis
     "General UI components for Plasma Phone including shell, containment and applets")
    (description "This package provides user-friendly, privacy-enabling and
customizable platform for mobile devices.")
    (license (list license:gpl3+ license:lgpl2.1+))))

(define-public plasma-mobile-sounds
  (package
    (name "plasma-mobile-sounds")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/plasma-mobile-sounds/"
                    version "/plasma-mobile-sounds-" version ".tar.xz"))
              (sha256
               (base32
                "1br6kzicrr45vgg0ciqczxlcid21n5lfjm6zc06rw86ys7fx7bpi"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list extra-cmake-modules pkg-config))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Sounds for Plasma Mobile devices")
    (description "This package provides sound files for Plasma Mobile.")
    (license (list license:cc0 license:cc-by4.0))))

(define-public plasma-pa
  (package
    (name "plasma-pa")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "18a1y39adx327yspkj5c2z24b8gh2cdiam6y73c6azxkvx4qc5w5"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     ;; test require selenium-webdriver-at-spi-run
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules kdoctools pkg-config))
    (inputs (list glib
                  kcoreaddons
                  kconfig
                  kcmutils
                  kdeclarative
                  kglobalaccel
                  kstatusnotifieritem
                  knotifications
                  kwindowsystem
                  kirigami
                  ksvg
                  kdbusaddons
                  pulseaudio-qt
                  ki18n
                  qtdeclarative))
    (propagated-inputs (list libcanberra pulseaudio
                             libplasma))
    (home-page "https://invent.kde.org/plasma/plasma-pa")
    (synopsis "Plasma applet for audio volume management using PulseAudio")
    (description
     "This package provides Plasma applet for audio volume management using
PulseAudio.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public plasma-pass
  (package
    (name "plasma-pass")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-pass/"
                                  "plasma-pass-" version ".tar.xz"))
              (sha256
               (base32
                "03ydwkk7qvw8hlr3phh5gzy6zf3rmh872a0n703h5dlacc4jbcwi"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n kitemmodels kwindowsystem kio
                  oath-toolkit
                  plasma5support
                  qgpgme-qt6))
    (propagated-inputs
     ;; QML modules need to be propagated so that QML files can find them in
     ;; $QML_IMPORT_PATH.
     (list kirigami libplasma qtdeclarative))
    (arguments (list #:qtbase qtbase
                     #:tests? #f
                     #:configure-flags #~(list "-DQT_MAJOR_VERSION=6")))
    (home-page "https://invent.kde.org/plasma/plasma-pass")
    (synopsis "Plasma applet for the Pass password manager")
    (description
     "This package provides a Plasma applet for the Pass password manager.")
    (license license:lgpl2.1+)))

(define-public plasma-phonebook
  (package
    (name "plasma-phonebook")
    (version "24.02.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-phonebook/"
                                  "plasma-phonebook-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1f2z3djq8q2z90vrn18k5qbiw8crhs69c5qvdnzxmp3s3f63bk4l"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kpeople
                  kirigami
                  kirigami-addons
                  kcoreaddons
                  kcontacts
                  qtdeclarative
                  qtsvg
                  qtwayland))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Phonebook for Plasma Mobile devices")
    (description "This package provides contacts application which allows
adding, modifying and removing contacts.")
    (license license:lgpl2.0+)))

(define-public plasma-vault
  (package
    (name "plasma-vault")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0sfflvzsk41vn3bv7gd6xzlhclgmpmaq9krqdligddkyd1cghmf0"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kio
                  ki18n
                  kconfig
                  plasma-activities
                  kdbusaddons
                  kiconthemes
                  kitemmodels
                  libksysguard
                  networkmanager-qt
                  libplasma
                  qtdeclarative

                  cryfs
                  fuse-2
                  gocryptfs
                  encfs))
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((fusermount (search-input-file inputs "/bin/fusermount"))
                    (gocryptfs (search-input-file inputs "/bin/gocryptfs"))
                    (cryfs (search-input-file inputs "/bin/cryfs"))
                    (encfs (search-input-file inputs "/bin/encfs"))
                    (encfsctl (search-input-file inputs "/bin/encfsctl")))
                (substitute* "kded/engine/fusebackend_p.cpp"
                  (("\"fusermount\"")
                   (string-append "\"" fusermount "\"")))
                (substitute* "kded/engine/backends/gocryptfs/gocryptfsbackend.cpp"
                  (("\"gocryptfs\"")
                   (string-append "\"" gocryptfs "\"")))
                (substitute* "kded/engine/backends/cryfs/cryfsbackend.cpp"
                  (("\"cryfs\"")
                   (string-append "\"" cryfs "\"")))
                (substitute* "kded/engine/backends/encfs/encfsbackend.cpp"
                  (("\"encfs\"")
                   (string-append "\"" encfs "\""))
                  (("\"encfsctl\"")
                   (string-append "\"" encfsctl "\"")))))))))
    (home-page "https://invent.kde.org/plasma/plasma-vault")
    (synopsis "Plasma applet and services for creating encrypted vaults")
    (description "Provides Plasma applet and services for creating encrypted
vaults.")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-systemmonitor
  (package
    (name "plasma-systemmonitor")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "08fapkw1rzz0pw5j1la30sxi867fqyaphd2d8fkwpx5isyxjjcqa"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kconfig
                  kcrash
                  kdeclarative
                  kservice
                  kiconthemes
                  kglobalaccel
                  kio
                  kdbusaddons
                  kpackage
                  kirigami
                  kirigami-addons
                  knewstuff
                  ksystemstats
                  kitemmodels
                  libksysguard
                  qqc2-desktop-style
                  qtdeclarative
                  qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (synopsis "System sensors, process information and other system resources
monitor")
    (description "This package provides an interface for monitoring system
sensors, process information and other system resources.")
    (home-page "https://invent.kde.org/plasma/plasma-systemmonitor")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-welcome
  (package
    (name "plasma-welcome")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-welcome"
                                  "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0qwm6shd3mfm9009n59nb5llfw44gn4hw1rwvhjjgjxlix56g640"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcoreaddons
           kdbusaddons
           kdeclarative
           ki18n
           kio
           kconfigwidgets
           kcmutils
           ksvg
           kirigami
           kirigami-addons
           knotifications
           kservice
           knewstuff
           kaccounts-integration
           signond-qt6
           kuserfeedback
           libaccounts-qt6
           kwindowsystem
           networkmanager-qt
           libplasma
           qtdeclarative
           qtsvg
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (synopsis "Plasma welcome screen")
    (description
     "This package provides a wizard for Plasma to configure settings.")
    (home-page "https://invent.kde.org/plasma/plasma-welcome")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "19hbdrkyax9qy7caki33a5zmvk6fhv05y7qm5f7489rynd9zmxwb"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools pkg-config qtsvg
                         qttools
                         xorg-server-for-tests
                         python-minimal
                         python-pygobject))
    (inputs (list appmenu-gtk-module
                  appstream-qt6
                  baloo
                  breeze
                  breeze-icons
                  dbus
                  eudev
                  fontconfig
                  gmp
                  gpsd
                  icu4c
                  iso-codes/pinned
                  karchive
                  kauth
                  kcmutils
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kded
                  kdesu
                  kglobalaccel
                  kglobalacceld
                  kguiaddons
                  kholidays
                  ki18n
                  kiconthemes
                  kidletime
                  kio
                  kio-extras
                  kirigami
                  kirigami-addons
                  kirigami-addons
                  kitemmodels
                  knewstuff
                  knighttime
                  knotifications
                  knotifyconfig
                  kpackage
                  kpeople
                  kpipewire
                  kquickcharts
                  kquickcharts
                  krunner
                  kscreen                     ;qml dependency
                  kscreenlocker
                  kstatusnotifieritem
                  ksvg
                  ktexteditor
                  ktextwidgets
                  kuserfeedback
                  kwallet
                  kwayland
                  kwin
                  layer-shell-qt
                  libkexiv2
                  libkscreen
                  libksysguard
                  libplasma
                  libqalculate
                  libsm
                  libxcrypt
                  libxcursor
                  libxft
                  libxkbcommon
                  libxrender
                  libxtst
                  lsof
                  mpfr
                  networkmanager-qt
                  packagekit-qt6
                  phonon
                  pipewire
                  plasma-activities
                  plasma-activities-stats
                  plasma-nm                   ;qml dependency
                  plasma-pa                   ;qml dependency
                  plasma-wayland-protocols
                  plasma-workspace-wallpapers
                  plasma5support
                  polkit-qt6
                  prison
                  qcoro-qt6
                  qqc2-desktop-style
                  qt5compat
                  qtdeclarative
                  qtlocation
                  qtpositioning
                  qtshadertools
                  qtsvg
                  qttools
                  qtwayland
                  ucd
                  wayland
                  wayland-protocols
                  xcb-util
                  xcb-util-cursor
                  xcb-util-image
                  xcb-util-keysyms
                  xdotool                     ;for X11
                  xmessage
                  xrdb
                  xsetroot
                  zlib))
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list
              ;; libkmpris/autotests/CMakeLists.txt find it from
              ;; KDE_INSTALL_FULL_LIBEXECDIR, But we are install to itself prefix.
              ;; so we set it.
              (string-append "-Dkglobalacceld_PATH="
                             #$(this-package-input "kglobalacceld")
                             "/libexec/kglobalacceld"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-qttools-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((qttools #$(this-package-input "qttools")))
                     ;; QT_INSTALL_BINS refers to qtbase, but qdbus is in
                     ;; qttools.
                     (substitute* "CMakeLists.txt"
                       (("ecm_query_qt\\(QtBinariesDir QT_INSTALL_BINS\\)")
                        (string-append "set(QtBinariesDir \"" qttools
                                       "/bin\")"))))))
               (delete 'check)
               (add-after 'install 'check-after-install
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "DISPLAY" ":1")
                     (system "Xvfb +extension GLX :1 &")
                     (setenv "HOME" (getcwd))
                     (setenv "XDG_RUNTIME_DIR" (getcwd))
                     (setenv "XDG_CACHE_HOME" (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     (setenv "QT_PLUGIN_PATH"
                             (string-append #$output
                                            "/lib/qt6/plugins:"
                                            (getenv "QT_PLUGIN_PATH")))
                     (setenv "QML_IMPORT_PATH"
                             (string-append #$output
                                            "/lib/qt6/qml:"
                                            (getenv "QML_IMPORT_PATH")))
                     (invoke "dbus-launch" "ctest"
                             "--output-on-failure"
                             "--rerun-failed"
                             "-E"
                             (string-join
                              (list
                               ;; Fails on an Apple M1 (aarch64) with the following error:
                               ;; Compared values are not the same "2'\uFFFD''\uFFFD'"
                               #$@(if (target-aarch64?)
                                      #~("calculatorrunnertest")
                                      #~())
                               "appstreamtest"
                               "dbusservicewatchertest"
                               "fetchinitialplayertest"
                               "keystatetest"
                               "klippertest"
                               "locationsrunnertest"
                               "lockedtest"
                               "mediakeystest"
                               "mprisdeclarativetest"
                               "shelltest"
                               "tasksmodeltest"
                               "tasktoolstest"

                               ;; Failure in TestDesktop::testRename:
                               ;; 'spyFileRenamed.count() >= 1' returned FALSE.
                               "testdesktop"

                               "testimagebackend"
                               "testimagefinder"
                               "testimagefrontend"
                               "testimagelistmodel"
                               "testimageproxymodel"
                               "testpackageimagelistmodel"
                               "testslidemodel"
                               "tst_triangleFilter"

                               "dbusmethodcalltest"
                               "klipper-testHistoryCycler"
                               "klipper-testHistoryModel"
                               "klipper_v3migrationtest"
                               "testrunnermodel"
                               "dbussignalwatchertest"
                               "dbuspropertiestest"
                               "testdesktop")
                              "|")))))
               ;; share/dbus-1/system-services have same name file
               ;; when dbus-root-service-type merge it, wail report
               ;; "file exists".
               (add-after 'install 'remove-dbus-service
                 (lambda _
                   (delete-file
                    (string-append
                     #$output "/share/dbus-1/services/org.kde.fontinst.service")))))))
    (home-page "https://invent.kde.org/plasma/plasma-workspace")
    (synopsis "Plasma workspace components")
    (description
     "Workspaces provide support for KDE Plasma Widgets, integrated search,
hardware management, and a high degree of customizability.")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-workspace-wallpapers
  (package
    (name "plasma-workspace-wallpapers")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0fsrscchsvl8cr2bzf76h8fganqadjgw9kp5ipqknmh7qqx1vb05"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen wallpapers for the KDE desktop")
    (description
     "This package provides wallpapers for the KDE desktop.")
    (license license:lgpl3+)))

(define-public print-manager
  (package
    (name "print-manager")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/print-manager-" version ".tar.xz"))
       (sha256
        (base32 "1v8jgzjd19c7ppsv5cig5pwnmx2j6hhw63m44rjq90apxb478bsf"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list cups
           kcmutils
           kconfig
           kconfigwidgets
           kcoreaddons
           kdbusaddons
           kirigami
           ki18n
           kio
           knotifications
           kwidgetsaddons
           kwindowsystem
           libplasma
           qtdeclarative
           qtwayland))
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (home-page "https://invent.kde.org/plasma/print-manager")
    (synopsis "Manage print jobs and printers")
    (description
     "This package provides printing management for KDE.")
    (license license:gpl2+)))

(define-public polkit-kde-agent
  (package
    (name "polkit-kde-agent")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-1-" version ".tar.xz"))
              (sha256
               (base32
                "1k04vhnxsyrbgcjxj73r2jnlx65z0l50xx3hg8fkr1zf7xics71m"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
                     #:tests? #f))
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  kcrash
                  kdbusaddons
                  kiconthemes
                  knotifications
                  kwidgetsaddons
                  kwindowsystem
                  polkit-qt6
                  qtdeclarative
                  ki18n))
    (synopsis "Polkit authentication UI for Plasma")
    (description
     "This package contains a daemon providing a Polkit authentication
UI for Plasma")
    (home-page "https://invent.kde.org/plasma/polkit-kde-agent-1")
    (properties '((upstream-name . "polkit-kde-agent-1")))
    (license license:gpl2+)))

(define-public powerdevil
  (package
    (name "powerdevil")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jzlrdj3fpg6ql6hafq86w8xsm88yk6i6mjjmv34mi1wxf5v1zkw"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules qttools pkg-config))
    (inputs (list bluez-qt
                  ddcutil
                  eudev
                  glib
                  kauth
                  kcmutils
                  kconfig
                  kcrash
                  kcrash
                  kdbusaddons
                  kglobalaccel
                  ki18n
                  kidletime
                  kio
                  kirigami
                  kitemmodels
                  knotifications
                  knotifyconfig
                  krunner
                  kscreen
                  kwayland
                  kxmlgui
                  layer-shell-qt
                  libcap
                  libkscreen
                  libplasma
                  libxkbcommon
                  network-manager
                  networkmanager-qt
                  plasma-activities
                  plasma-wayland-protocols
                  plasma-workspace
                  qcoro-qt6
                  qtwayland
                  solid
                  wayland))
    (arguments (list #:qtbase qtbase
                     #:phases #~(modify-phases %standard-phases
                                  (add-before 'check 'setenv
                                    (lambda _
                                      (setenv "HOME" (getcwd)))))))
    (synopsis "Manage power consumption")
    (description "This package provides the power consumption settings
of a Plasma shell.")
    (home-page "https://invent.kde.org/plasma/powerdevil")
    (license license:gpl2+)))

(define-public spectacle
  (package
    (name "spectacle")
    (version "6.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/spectacle-" version ".tar.xz"))
       (sha256
        (base32 "0blr40j7l36a43ybvh33mfvwiq0qaavqszarfzpzsg6v9lbgnha6"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f)) ;no tests
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
           kcoreaddons
           kcrash
           kdbusaddons
           kglobalaccel
           kguiaddons
           ki18n
           kio
           kirigami
           knotifications
           kpipewire
           kstatusnotifieritem
           kwidgetsaddons
           kwindowsystem
           kxmlgui
           opencv
           prison
           purpose
           layer-shell-qt
           qtdeclarative
           qtmultimedia
           qtwayland
           wayland
           wayland-protocols
           plasma-wayland-protocols
           xcb-util
           xcb-util-cursor
           xcb-util-image
           libxkbcommon))
    (home-page "https://apps.kde.org/spectacle/")
    (synopsis "Screenshot capture utility for KDE")
    (description "Spectacle is a screenshot taking utility for the KDE.")
    (license license:gpl2+)))

(define-public system-settings
  (package
    (name "system-settings")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/systemsettings-" version ".tar.xz"))
              (sha256
               (base32
                "1kjbi8yyh48y2j3z5nn4sj465h7r2l1lrzh0kj18rw3044cv5wzn"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kaccounts-integration
                  kaccounts-providers
                  kauth
                  kcrash
                  kitemviews
                  kitemmodels
                  kcmutils
                  ki18n
                  kio
                  kservice
                  kiconthemes
                  kwidgetsaddons
                  kwindowsystem
                  kxmlgui
                  kdbusaddons
                  kconfig
                  kpackage
                  plasma-activities
                  plasma-activities-stats
                  kguiaddons
                  kirigami
                  knotifications
                  krunner
                  plasma-workspace
                  qtdeclarative
                  qtwayland
                  qtwebengine))
    (arguments (list #:qtbase qtbase
                     #:tests? #f))      ; no tests
    (synopsis "Control center to configure Plasma Desktop")
    (description "This package provides configuration UI for Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/systemsettings")
    (properties '((upstream-name . "systemsettings")))
    (license license:gpl2+)))

(define-public wacomtablet
  (package
    (name "wacomtablet")
    (version "6.5.2")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kde/stable/plasma/" version
                              "/wacomtablet-" version ".tar.xz"))
              (sha256
               (base32
                "0qxwg5j5dy3icrfmg364v0fglinrnh4hn4piynk6hvfqgaimj3f0"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:test-exclude
           (string-append "("
                          (string-join '("Test.KDED.DBusTabletService"
                                         "Test.KDED.TabletHandler"
                                         "Test.KDED.XInputAdaptor"
                                         "Test.KDED.XsetWacomAdaptor")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? (test-exclude "") #:allow-other-keys)
                   (when tests?
                     (invoke "dbus-launch" "ctest" "-E" test-exclude)))))))
    (native-inputs (list dbus extra-cmake-modules kdoctools pkg-config))
    (inputs (list kcoreaddons
                  ki18n
                  kio
                  kglobalaccel
                  kconfig
                  kcmutils
                  kxmlgui
                  kwidgetsaddons
                  kwindowsystem
                  knotifications
                  kdbusaddons
                  plasma5support
                  qtdeclarative
                  libwacom
                  libxi
                  libxkbcommon
                  libplasma
                  qtwayland
                  xf86-input-wacom))
    (home-page "https://invent.kde.org/plasma/wacomtablet")
    (synopsis "KDE GUI for the Wacom Linux Drivers")
    (description "Provides KDE GUI for the Wacom Linux Drivers.")
    (license license:gpl2+)))
