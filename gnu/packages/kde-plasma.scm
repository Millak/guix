;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
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
  #:use-module (gnu packages gdb)
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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages package-management) ; flatpak
  #:use-module (gnu packages rdesktop)
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
    (version "6.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0921kh0a8xwxjza1zkxyhbb9c83hly14x1jmdc7hhmpcrxa6qnwl"))))
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
             kcmutils
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
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (invoke "ctest" "-E"
                             (string-append "(plasma-dialogstatetest"
                                            "|plasma-iconitemtest"
                                            "|plasma-dialogqmltest"
                                            "|plasma-themetest"
                                            "|iconitemhidpitest"
                                            "|bug485688test"
                                            "|dialognativetest)"))))))))
    (home-page "https://invent.kde.org/plasma/libplasma")
    (synopsis "Libraries, components and tools of Plasma workspaces")
    (description "The plasma framework provides QML components, libplasma and
script engines.")
    ;; dual licensed
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public bluedevil
  (package
    (name "bluedevil")
    (version "6.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "07nphj557qyrqk96y3yp31xgv595xf63pl1az5awv059kv3hffpv"))))
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
    (version "6.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1d6qsrdyf3j1inb57w4qlvr25vg5zalc3j7lb1lm84cm74yl7f6w"))))
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

(define-public breeze-qt5
  (package
    (inherit breeze)
    (name "breeze-qt5")
    (inputs
     (list kcmutils-5 ; optional
           kconfigwidgets-5
           kcoreaddons-5
           kguiaddons-5
           ki18n-5
           kirigami-5
           kiconthemes-5
           kpackage-5
           kwindowsystem-5))
    (arguments
     (list #:tests? #f
           #:configure-flags
           #~(list "-DBUILD_QT6=OFF")))))

(define-public breeze-gtk
  (package
    (name "breeze-gtk")
    (version "6.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "161sy18w6cg2wm3jd3vy4vmbg215p8kznda2bwplf6j23758h7kp"))))
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
    (version "25.08.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/calindori-" version ".tar.xz"))
              (sha256
               (base32
                "1pmpvaihkxpjkw198w3z3ly3zd4rim5266pgmr2s02a1xvq1xhxf"))))
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
    (version "6.4.5")
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
                "0xl55ybkp67yvx7c3i4fi49il1v1vqgb1lvwvb8lrbnjgbix824j"))))
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

(define-public drkonqi
  (package
    (name "drkonqi")
    (version "6.3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0pg644f91mdgbvlbjmwman3wdda3ppya90j3j2hx3pq665g3qdzf"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags
           #~(list "-DCMAKE_DISABLE_FIND_PACKAGE_Systemd=TRUE"
                   "-DWITH_GDB12=TRUE"
                   "-DWITH_PYTHON_VENDORING=FALSE")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-gdb-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((gdb (search-input-file inputs "/bin/gdb")))
                     (substitute* "src/debugger.cpp"
                       (("u\"gdb")
                        (string-append "u\"" gdb))))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E" "(connectiontest|preambletest)"))))
               (add-after 'install 'wrap-program
                 (lambda _
                   (wrap-program (string-append #$output
                                                "/libexec/drkonqi")
                     `("GUIX_PYTHONPATH" ":" prefix
                       (,(getenv "GUIX_PYTHONPATH")))))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list ki18n
                  kcoreaddons
                  kconfig
                  kservice
                  kdeclarative
                  kjobwidgets
                  kstatusnotifieritem
                  kio
                  kcrash
                  kcompletion
                  kwidgetsaddons
                  kwallet
                  knotifications
                  kidletime
                  kwindowsystem
                  qtdeclarative
                  kuserfeedback

                  python-minimal
                  python-pygdbmi
                  python-chai
                  python-psutil
                  python-sentry-sdk
                  gdb
                  ;; qml module runtime dependency
                  ksyntaxhighlighting
                  kcmutils
                  kitemmodels
                  kirigami))
    (synopsis "Crash handler for KDE software")
    (description "This package provides an automatic handler for crashed apps.")
    (home-page "https://invent.kde.org/plasma/drkonqi")
    (license license:gpl2+)))

(define-public kactivitymanagerd
  (package
    (name "kactivitymanagerd")
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "12x6nqaj60dma1n6dxv87z51013aqw7dzywab939sipw7c32psz9"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "11xphz2q201zwgfq9njh5vwqhc9acf0nwryw40slpw906qwm0jdh"))))
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
             kconfig
             kcrash
             kdbusaddons
             kcmutils
             ki18n
             kcoreaddons
             kstatusnotifieritem
             kpipewire
             openssl
             plasma-wayland-protocols
             freerdp-3
             qtwayland
             qtdeclarative
             qtkeychain-qt6
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kde-gtk-config-" version ".tar.xz"))
              (sha256
               (base32
                "04jrkdav5iahr6nqr27gal91m717pcvanmkxgdsv26w0pi9j5xr2"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kdecoration-" version ".tar.xz"))
              (sha256
               (base32
                "093qxpyqdhzcpjkx62qi1zljibar692f69qjigd5ka47yjyrj3xx"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (patches (search-patches "kde-cli-tools-delay-mime-db.patch"))
              (sha256
               (base32
                "0a3fmwbww9qwphbzba61895s5k03s0g5lrbn54rghplmihpw8k6m"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "16yimbrfkhjrkfdp7s094rsgdvvf68canfjpysalx7z9zrv93bbx"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? inputs #:allow-other-keys)

                            (when tests?
                              (setenv "TZDIR"
                                      (search-input-directory
                                       inputs "share/zoneinfo"))
                              (invoke "ctest" "-E"
                                      "(converterrunnertest)")))))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "03pc2fdq33l1k6wn3i86fl7w2l890sj1id93aqwp60cvzpv8cpyq"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0y23rhxc138dlzdhrihc43jjf7vjjnhx30rzkzlxafhk3dz7i756"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ycaljixrn43hj5ladid5gn15zdfnzz7dxi9yas78akpf2a4jpsd"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "12cwlwmgd9r79af54mh55l56yfvbd9svyazqi05291g1zkpjdywl"))))
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

(define-public koi
  (package
    (name "koi")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baduhai/Koi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z3j3lcfqkwck4i4srpcanjyb2cmizd702f6s5lhfhrmmsbccwkx"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0hlnz8w69n2p6f53d096frqikacb3bdw1p253pnq7fv1rrkdjrhq"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1ab0z87312vb25acin4w273q1pi5yppzlhgbxk7r6qhkl8r71vrp"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: All tests fail
     (list #:tests? #f))
    (native-inputs (list extra-cmake-modules qttools pkg-config
                         wayland
                         wayland-protocols
                         qtwayland))
    (inputs (list kconfig
                  kdbusaddons
                  kdeclarative
                  kglobalaccel
                  ki18n
                  kwindowsystem
                  kiconthemes
                  kcoreaddons
                  kcrash
                  kcmutils
                  kxmlgui
                  layer-shell-qt
                  libkscreen
                  libxi
                  libxkbcommon
                  ksvg
                  plasma-wayland-protocols
                  qtsensors
                  qtbase
                  qtwayland
                  xcb-util
                  libplasma))
    (home-page "https://invent.kde.org/plasma/kscreen")
    (synopsis "Screen management software")
    (description "This package provides the screen management software for
KDE Plasma Workspaces.")
    (license license:gpl2+)))

(define-public ksshaskpass
  (package
    (name "ksshaskpass")
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/ksshaskpass-" version ".tar.xz"))
              (sha256
               (base32
                "045g1dvcjh14y537h9z14a5yk20zabijpvcabgnhf7593ah8jpwn"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "036sx162rbw7jmq2vk20gqk3pc09npclxp7q218ma8ypix8xn6qd"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "ksystemstatstest")))))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/layer-shell-qt-" version ".tar.xz"))
              (sha256
               (base32
                "0z6vvpwm26s43x8n3hzsh7v6n052sv2akyn3qd3rb7idrp04xrik"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kscreenlocker-" version ".tar.xz"))
              (sha256
               (base32
                "1hya3lns7rh5jykym2gxq3g33779krpj98lzlm65467x4r21fh9l"))))
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
    (version "6.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0sxhh7mx4h9s7vgapphfhlwfnxzjx7yydv80w13whlh62420cpx2"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
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
                      "testEnv"))))))))))
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

;; use by lxqt-config
(define-public libkscreen-5
  (package
    (inherit libkscreen)
    (name "libkscreen")
    (version "5.27.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1ary7qavz8vkzbvjx2mxv09h61hxa7i4f7rfgbykldbc83ripdc6"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     (setenv "WAYLAND_DISPLAY" "libkscreen-test-wayland-backend-0")
                     (invoke "ctest" "-E"
                             (string-append "(kscreen-testedid"
                                            "|kscreen-testqscreenbackend"
                                            "|kscreen-testkwaylandbackend"
                                            "|kscreen-testkwaylandconfig"
                                            "|kscreen-testkwaylanddpms)"))))))))
    (native-inputs
     (list extra-cmake-modules
           pkg-config
           qttools-5
           ;; For testing.
           dbus))
    (inputs
     (list kconfig-5 kwayland-5 libxrandr plasma-wayland-protocols
           qtbase-5 qtwayland-5 wayland qtx11extras))))

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "6.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32 "1qijnyz6f4kk2y3zl4c369sbbrzrdbndi6g3dr30w2yjbwqm78sp"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "1phcznsmmhysk0vri33lh60g8p09qfnklzljjrv8x2kv7sh1q588"))))
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

(define-public kwayland-integration
  (package
    (name "kwayland-integration")
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1l72ns4mj7a6dvhzi3hrhv54pa8zj5izm53q856rsbnkw8s23bwv"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (delete 'check)
                        (add-after 'install 'check-after-install
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME" (getcwd))
                              (setenv "XDG_RUNTIME_DIR" (getcwd))
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              ;; https://bugs.gentoo.org/668872
                              (invoke "ctest" "-E" "(idleTest-kwayland-test)"))))
                        (add-before 'check-after-install 'check-setup
                          (lambda* (#:key outputs #:allow-other-keys)
                            (setenv "QT_PLUGIN_PATH"
                                    (string-append #$output
                                                   "/lib/qt5/plugins:"
                                                   (getenv "QT_PLUGIN_PATH"))))))))
    (native-inputs (list extra-cmake-modules wayland-protocols pkg-config))
    (inputs (list kguiaddons-5
                  kidletime-5
                  kwindowsystem-5
                  kwayland-5
                  libxkbcommon
                  wayland
                  plasma-wayland-protocols
                  qtbase-5
                  qtwayland-5))
    (synopsis "KWayland runtime integration plugins")
    (description "This package provides Wayland integration plugins for various
KDE Frameworks components.")
    (home-page "https://invent.kde.org/plasma/kwayland-integration")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public kwin
  (package
    (name "kwin")
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "kwin-unwrap-executable-name-for-dot-desktop-search.patch"))
              (sha256
               (base32
                "1a02npafxbpkqi8phy9n7gqc7vb0p1w2ffjanhz819zc334nhdd1"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
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
            (lambda* (#:key tests? #:allow-other-keys)
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
                (invoke "dbus-launch"
                        "ctest"
                        "-E"
                        (string-join
                         (list
                          ;; Fails on an Apple M1 (aarch64) with the following error:
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
                          "kwin-testX11Window")
                         "|"))))))))
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
    (inputs (list breeze
                  eudev
                  fontconfig
                  freetype

                  hwdata
                  plasma-activities
                  kcmutils
                  kcompletion
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kcrash
                  kdbusaddons
                  kdeclarative
                  kdecoration
                  kglobalaccel
                  kglobalacceld
                  ki18n
                  kiconthemes
                  kidletime
                  kio
                  kirigami
                  knewstuff
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
                  kauth
                  kguiaddons
                  libqaccessibilityclient
                  lcms
                  libcanberra
                  libcap
                  libepoxy
                  libinput
                  libxkbcommon
                  pipewire
                  libplasma
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
                  libdisplay-info
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "12q9vf52p1hwkdql1z0sbh1y1qy4wiqbk1qiyx2vs317sig4n57y"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1wazsc7yhl64czd7kr1ymjmfnygf3y1ivbv8sfdi9f5fc9dwjyfn"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1813sg9l9vvj08mh4p65h3ibvsmc2v5x0dz4hmq3l4q31dsasv1w"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:tests? #f))
    (native-inputs
     (list extra-cmake-modules))
    (inputs (list qtdeclarative
                  kiconthemes kguiaddons kconfig kirigami kcoreaddons
                  kcolorscheme kquickcharts))
    (home-page "https://invent.kde.org/plasma/qqc2-breeze-style")
    (synopsis "Breeze inspired Qt Quick Controls Style")
    (description "This package provides Breeze inspired Qt Quick Controls Style.")
    (license (list license:lgpl2.0+ license:gpl2+))))

(define-public oxygen-sounds
  (package
    (name "oxygen-sounds")
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0arfmwxr6p6hfjsywz9zr0cvaf4qj2bsnqdm0h7fk72cm3wnc1a4"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/ocean-sound-theme"  "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0cl489jvlsd27qiw07ymfnn8ckrfzyd3vnwm6w1fcjzg4q021y5k"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09zwq52wyr2wj5zvgqrzi02xcy610hrvqx1sb4ykps5b0ph73v1z"))))
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
    (propagated-inputs (list baloo
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-activities-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1w4fp6h6kmjzk94mvk73wzkkn3bhq4vyd6jv976zmph2crscm38w"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list boost
           kconfig
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-activities-stats-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0srv5g3xfnwzl6qllz9d5zhcpai3182nkl9wb2wnjncvvbih444r"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1xlcrpsl7w14pcxm8kl05as5nf49fi3z6bz0rf7qmn8qbn9si28y"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase

                     #:phases
                     #~(modify-phases %standard-phases
                         (replace 'check
                           (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
                             (invoke "ctest"
                                     "-E"
                                     ;; also fail in upstream.
                                     "(pluginloadertest)"
                                     "-j"
                                     (if parallel-tests?
                                         (number->string (parallel-job-count))
                                         "1")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (propagated-inputs (list kcoreaddons))
    (inputs (list
             kconfig
             ki18n
             kio
             kidletime
             kguiaddons
             knotifications
             kservice
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jxbg4qqp8ffa0k8zihxdll26gf8q9kyh1dy32aaxhsfmcbygk2q"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "09d5b8j0gc2c7aj1imrd6lg9h7px1zknkwn3chhsjgvbixaky2hr"))))
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
                  icu4c
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
                  qtwayland
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
    (propagated-inputs (list iso-codes/pinned kirigami kcmutils plasma-workspace))
    (arguments
     (list #:qtbase qtbase
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
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (setenv "XDG_RUNTIME_DIR" (getcwd))
                     (setenv "XDG_CACHE_HOME" (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     (invoke "ctest" "-E" "(positionertest|kcm-keyboard-keyboard_memory_persister_test|foldermodeltest)")))))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "04y28s9vfajw2hmiqj0z75znrjnr9i2jvmv250ifgbs2ar3n6vmx"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1366hf33004nnby4fp5l5hi58x1a1q92rc3mh8iv2sjnnr3h0y5w"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1mwb50ri6qijl1qy5qkziljw607vvhj091bkwk13zwxpsx1ydwvi"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags #~(list "-DBUILD_QT5=OFF")
           #:tests? #f                  ;TODO: Failing tests
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (setenv "HOME" (getcwd))
                              (setenv "XDG_RUNTIME_DIR" (getcwd))
                              (setenv "XDG_CACHE_HOME" (getcwd))
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              (invoke "ctest" "-E"
                                      "(frameworkintegration-kdeplatformtheme_unittest|frameworkintegration-kfontsettingsdata_unittest|frameworkintegration-kfiledialog_unittest|qmltests|frameworkintegration-kfiledialogqml_unittest")))))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-nano-" version ".tar.xz"))
              (sha256
               (base32
                "0mlaadv0cmcfxysy5f8ahvy3lm1cn0x8is4ir1vyynpzmmbmnz0a"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0gm18rma3dsvvpsyvn7aq91ksih6j8mi94br4ghhmsal18y8xhv7"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-mobile-" version ".tar.xz"))
              (sha256
               (base32
                "1wqxjspp4hbwlwgh3q2xs4f82h67dk8yn400b47f91z4kmskskz3"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;only small add-hoc test scripts
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list bash-minimal
                  eudev
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "08afzb95z421xivjp8ql60xa66i586zzq4fj33y8dkv9nhx3s3kb"))))
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
  ;; Port to Plasma 6.
  (let ((commit "74c011b634f3bdbdcd80a74b252e6499774756f2")
        (revision "0"))
    (package
      (name "plasma-pass")
      (version (git-version "1.2.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://invent.kde.org/plasma/plasma-pass")
                      (commit commit)))
                (sha256
                 (base32
                  "0wp69ylhcqhy4l282fns0grhpf20w8jrhj2jmv7gdmhxnpnh9lk9"))
                (file-name (git-file-name name version))))
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
      (license license:lgpl2.1+))))

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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1isf0d3nvnc6p7isqfgm0hd1vpp51yb5hpqcrm1m0l06cqf0wydr"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kio
                  ki18n
                  kconfigwidgets
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0c6jjm48w25f8panfigdl877ml6k37wbr7rqx4p369jm4scy6mk8"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-welcome"
                                  "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0qxlf2w5lavz5sddzkdx9ymx4dxbc2j0ix9hzbya97vmkwwxc76p"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1f7dil2bhrdvhl9gqrpqvz62ldlq5ljdnyiac6q05j0ym62z4m3j"))))
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
                  fontconfig
                  icu4c
                  iso-codes/pinned
                  plasma-activities
                  plasma-activities-stats
                  karchive
                  kauth
                  ksvg
                  kstatusnotifieritem
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
                  lsof
                  xdotool
                  qqc2-desktop-style
                  qcoro-qt6
                  kirigami-addons
                  kio-extras
                  kitemmodels
                  kirigami
                  kirigami-addons
                  knewstuff
                  knotifications
                  knotifyconfig
                  kquickcharts
                  kpackage
                  kpeople
                  kpipewire
                  kquickcharts
                  krunner
                  kscreenlocker
                  ktexteditor
                  ktextwidgets
                  kunitconversion
                  kuserfeedback
                  kwallet
                  kwayland
                  kwin
                  layer-shell-qt
                  libkscreen
                  libksysguard
                  libqalculate
                  gmp
                  mpfr
                  eudev
                  libsm
                  libxcrypt
                  libxft
                  libxkbcommon
                  libxrender
                  libxtst
                  networkmanager-qt
                  phonon
                  pipewire
                  libplasma
                  plasma5support
                  plasma-workspace-wallpapers
                  plasma-wayland-protocols
                  prison
                  qtlocation
                  qt5compat
                  qtsvg
                  qtshadertools
                  qtdeclarative
                  qttools
                  qtpositioning
                  qtwayland
                  wayland
                  wayland-protocols
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xrdb
                  xmessage
                  xsetroot
                  polkit-qt6
                  ucd

                  xcb-util-cursor
                  libxcursor
                  libkexiv2
                  gpsd
                  zlib

                  ;; qml dependency
                  plasma-nm
                  plasma-pa
                  kscreen))
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
               (add-after 'unpack 'patch-workspace-bins
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((xmessage (search-input-file inputs "/bin/xmessage"))
                         (xsetroot (search-input-file inputs "/bin/xsetroot"))
                         (xrdb (search-input-file inputs "/bin/xrdb"))
                         (qttools #$(this-package-input "qttools")))
                     (substitute* "applets/devicenotifier/plugin/\
deviceerrormonitor_p.cpp"
                       (("lsof") (search-input-file inputs "/bin/lsof")))
                     (substitute* "startkde/startplasma.cpp"
                       (("xmessage") xmessage))
                     (substitute* "kcms/krdb/krdb.cpp"
                       (("xsetroot") xsetroot))
                     (substitute* (list "kcms/fonts/fontinit.cpp"
                                        "kcms/fonts/fonts.cpp"
                                        "kcms/krdb/krdb.cpp")
                       (("xrdb") xrdb))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1i697bad8d1184jfyjwpmpy723igx3ksg8bqnn1l9nsfnnmivlpm"))))
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
    (version "6.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/print-manager-" version ".tar.xz"))
       (sha256
        (base32 "0d1dld9kbn2h9alyrwkycy4s4krzp40vjk4aq9spx4vdd3ipzzgx"))))
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
           kiconthemes
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-1-" version ".tar.xz"))
              (sha256
               (base32
                "1rw2ar20i6sp2rr9cqq2gb6hmwh0c4wcb8r6b6yxr7rfl07inwr2"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1mib0laspzzsrqm3g2bdlb7iw3zvh6204cq13jgy0pch2k2d0iy2"))))
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
    (version "6.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/spectacle-" version ".tar.xz"))
       (sha256
        (base32 "1n19pcqqmgcj66pv5d287zh79hg96cx8pg902l73yfli3w4zr1lr"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E"
                             "filename_test")))))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/systemsettings-" version ".tar.xz"))
              (sha256
               (base32
                "0mjm62r7wqn7k9ifs0vq87dlag8rsyacmvc64hjgvxgk23bxq76c"))))
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
    (version "6.4.3")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kde/stable/plasma/" version
                              "/wacomtablet-" version ".tar.xz"))
              (sha256
               (base32
                "161k0ccnjcgn3xgkgigfjjq8yqywmmabv2h13xf09azpm7sl6g9m"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "dbus-launch" "ctest" "-E"
                                      "(Test.KDED.DBusTabletService\
|Test.KDED.TabletHandler|Test.KDED.XInputAdaptor|\
Test.KDED.XsetWacomAdaptor)")))))))
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
