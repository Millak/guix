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
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  ;; Including this module breaks the build
  ;#:use-module ((gnu packages kde-systemtools) #:select (konsole))
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages networking)
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

(define-public bluedevil
  (package
    (name "bluedevil")
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1fhgqxrqbm20liap57h35qq6wvh7x5r980r2ibv3k0k7naqp16sm"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1b0mxnsx0nmj7wskvvrvi275xbj7xf8pysa1ywis9bkffh757bwp"))))
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
    (arguments (list #:configure-flags #~(list "-DBUILD_QT6=OFF")))))

(define-public breeze-gtk
  (package
    (name "breeze-gtk")
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "0vsn0mnsqink5y5rvms3hgjj7608ngylrvw0v6li5swl7hfrh5bc"))))
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
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/calindori-" version ".tar.xz"))
              (sha256
               (base32
                "1sz9bsbxwvhccbbk9h4ydravh10ba0g98m9wy5kcjaq16agwbakb"))))
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
                  qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma-mobile/calindori")
    (synopsis "Calendar for Plasma Mobile")
    (description
     "This package provides a touch friendly calendar application.")
    (license license:gpl3+)))

(define-public discover
  (package
    (name "discover")
    (version "6.2.5")
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
                "1954adjgpj0vww1bzrskdgphdqm06xb4dfxh19aasjra760vijwc"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
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
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "XDG_DATA_DIRS"
                             (string-append (getcwd)
                                            ":" (getenv "XDG_DATA_DIRS")))
                     (invoke "ctest" "-E" "knsbackendtest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
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
                  packagekit-qt6
                  purpose
                  qt5compat
                  qtdeclarative
                  qtsvg
                  qtwebview
                  qcoro-qt6))
    ;; -- The following features have been disabled:
    ;; * Ostree, Library to manage ostree repository. Required to build the rpm-ostree backend
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "02s0dd35vhdlj42wridxg7qrpbyayvz2bzsd9yvf69aiz7c793nq"))))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "00kmx5zdfvg0w18hcc4wi1xqfi3n8x46664khf48mcl3gzjval2g"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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

(define-public krdp
  (package
    (name "krdp")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1z3k9b1jbiw1qixvlmi4vrwl4p4qhrcirgr2ksp0rs4ivgikg38s"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase
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
             freerdp
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kde-gtk-config-" version ".tar.xz"))
              (sha256
               (base32
                "197l5nrbgikihdp50q3d95dhxnlpinzp2zhk2mjnwzkfshcgqgpp"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kdecoration-" version ".tar.xz"))
              (sha256
               (base32
                "16kq2claxd52kl03x2vzml894628kdvd1fkdwrb9hdd6ynllkl5r"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (patches (search-patches "kde-cli-tools-delay-mime-db.patch"))
              (sha256
               (base32
                "00l0fan18imzskcacbz1liymwkymd2fm35l7cn3a0q2h97vmgnhx"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "09p42yylsx1j8hslmv73rk80a79ynz75hk225h4vinqs23akilap"))))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1nsdkqnrhl20g6giaglk22vzh2zs1v5qjkhy8zq7z050xsqn2g7w"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1ngaf2zh0gz2zivx6wiyfw09v7afrq1nrzjb4s829dhj35p16zc2"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1yczcgq4rnfmg3g8sb83nym8jmglp0hwhyfwz24n29947bdpxn4y"))))
    (build-system cmake-build-system)
    (arguments
     (list
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
                  dmidecode
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
                  qtbase
                  solid
                  util-linux
                  vulkan-tools
                  wayland-utils
                  xdpyinfo
                  clinfo))
    (propagated-inputs (list system-settings))
    (home-page "https://invent.kde.org/plasma/kinfocenter")
    (synopsis "View information about computer's hardware")
    (description "This package provides tool to view information about
computer's hardware.")
    (license (list license:gpl2 license:gpl3))))

(define-public kmenuedit
  (package
    (name "kmenuedit")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0jk8xn6hjv0wl2s80yawsyx7lyzfdnn7qlyh4fq00jn29xgn8209"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
                  kwindowsystem))
    (synopsis "Menu Editor for Plasma Workspaces")
    (description "This package provides menu editor for Plasma Workspaces.")
    (home-page "https://invent.kde.org/plasma/kmenuedit")
    (license license:gpl2+)))

(define-public kongress
  (package
    (name "kongress")
    (version "24.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/kongress-" version ".tar.xz"))
              (sha256
               (base32
                "06xhlr39wglnis9nbxgvkp5sz74g5hms0cahlvm8pmphvd5aygnv"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules python-minimal))
    ;; NOTE: Reporting bugs is linked to web browser, better not link it and let
    ;; it reslove through xdg-open in the run time
    (inputs (list kirigami
                  kirigami-addons
                  kcrash
                  kdbusaddons
                  ki18n
                  kcalendarcore
                  kconfigwidgets
                  kwindowsystem
                  kcoreaddons
                  kcontacts
                  kitemmodels
                  knotifications
                  kosmindoormap
                  kxmlgui
                  kiconthemes
                  qtdeclarative
                  qtsvg))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/kongress/")
    (synopsis "Companion application for conferences")
    (description "This application provides list of upcoming conferences with
the schedule and venue information.")
    (license license:gpl3+)))

(define-public kpipewire
  (package
    (name "kpipewire")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "06b37sbvvv1cm9zyx6kc4qa13w6fm4gpv9p61vc7nhnay20xahnv"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (propagated-inputs (list qtbase qtdeclarative
                             ;; include/KPipeWire/dmabufhandler.h include it.
                             libepoxy))
    (inputs (list libxkbcommon
                  libva
                  pipewire
                  ffmpeg
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0hlnyq71pjffgw3jia9x94dabk5aqrcg1mr0dw7d3103wxzw8dv2"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: All tests fail
     (list #:tests? #f))
    (native-inputs (list extra-cmake-modules qttools pkg-config))
    (inputs (list kconfig
                  kdbusaddons
                  kdeclarative
                  kglobalaccel
                  ki18n
                  kwindowsystem
                  kiconthemes
                  kcoreaddons
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/ksshaskpass-" version ".tar.xz"))
              (sha256
               (base32
                "0wpb255c6w3m7fl3wx035znwm9s2kwc0grqshsgiklnm6y01rrw5"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons ki18n kwallet kwidgetsaddons))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "18cmjp6jykzg8iiihkfp23ylcgcds3448ym4651fvrbgbvsmdzzg"))))
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

(define-public latte-dock
  (package
    (name "latte-dock")
    (version "0.10.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/latte-dock/"
                                  "latte-dock-" version ".tar.xz"))
              (sha256
               (base32
                "0zj818wpxdiqpzivvwrgbzj26lcmmv49zw8206v4shcms1afbl9j"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtbase-5
                  qtdeclarative-5
                  knotifications
                  kwindowsystem
                  kio
                  plasma-framework
                  kwayland
                  kactivities
                  kcrash
                  kiconthemes
                  knewstuff
                  karchive
                  kguiaddons
                  kdbusaddons
                  kglobalaccel
                  kirigami
                  ki18n
                  kdeclarative
                  kcoreaddons
                  xcb-util
                  qtx11extras
                  libsm))
    (synopsis "Latte is a dock based on plasma frameworks")
    (description
     "Latte is a dock based on plasma frameworks that provides
an elegant and intuitive experience for your tasks and plasmoids.")
    (home-page "https://github.com/KDE/latte-dock")
    (license license:gpl2+)))

(define-public layer-shell-qt
  (package
    (name "layer-shell-qt")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/layer-shell-qt-" version ".tar.xz"))
              (sha256
               (base32
                "0xqc00dhrgj0iar6np6wvcfi4fcg595ysbxdgcvpqf6z3018f2dw"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kscreenlocker-" version ".tar.xz"))
              (sha256
               (base32
                "0klficp9is0nmx22rrslr9m188j0dhmdsp7j1jlc4k9r838d4gis"))))
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
    (version "6.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1f5wgdh844wskw71bw5dc35ilj6k2nxz8yf49gpxqvgd5vxgdnjy"))))
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
    (version "6.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (sha256
        (base32 "1avn3krk5g20lrclq8y2syg10b8fn97d6vlfxf14v2q7npbg754n"))))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "02d39rsk3rnzg4w1vhn07xx3152v8l96aq6fgn50zpjjm9dhrbnp"))))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1zgq0i7r92xvyjrhmigxzpfq81yp7frw2cgk2vv4v4lcbf1mkjj6"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "kwin-unwrap-executable-name-for-dot-desktop-search.patch"))
              (sha256
               (base32
                "0j38fxgxqyvhyj84cagvmab7y8rind82bdr9k72ch18iwjk51i2w"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:configure-flags
      #~(list (string-append "-DQtWaylandScanner_EXECUTABLE="
                             #$(this-package-native-input "qtwayland")
                             "/lib/qt6/libexec/qtwaylandscanner"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("src/plugins/kdecorations/aurorae/src/aurorae.cpp")
                (("(^\\s*QDirIterator it.path, QDirIterator::Subdirectories)(\\);)"
                  _ a b)
                 (string-append a
                                " | QDirIterator::FollowSymlinks" b)))
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
                (("(m_xwaylandProcess->setProgram.QStringLiteral..)(Xwayland)(...;)"
                  _ a Xwayland b)
                 (string-append a
                                (which "Xwayland") b)))
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
                         gcc-14 ;; for <format> header
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1dkmr50v8jnkmx6jxsps8l1z559khnq2g5zpl48maybzpgjcpcpp"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons ki18n kpty knotifications))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma/kwrited")
    (synopsis "System notification daemon")
    (description
     "This package provides a daemon that listens to system notifications.")
    (license license:gpl2+)))

(define-public lightly
  (package
    (name "lightly")
    (version "0.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Luwx/Lightly")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0qkjzgjplgwczhk6959iah4ilvazpprv7yb809jy75kkp1jw8mwk"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kdecoration
                  kcoreaddons
                  kguiaddons
                  kconfigwidgets
                  kwindowsystem
                  ki18n
                  kiconthemes
                  qtx11extras))
    (home-page "https://github.com/Luwx/Lightly")
    (synopsis "Modern style for Qt applications")
    (description
     "Lightly is a fork of the Breeze theme that aims to be visually modern
and minimalistic.")
    (license license:gpl2+)))

(define-public milou
  (package
    (name "milou")
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0dzzwdqig33hgdq70i7lh8z26v9pifm766p29w1qirb65v9dfyxf"))))
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
    (arguments (list #:qtbase qtbase))
    (synopsis "Dedicated search application built on top of Baloo")
    (description "This package provides a dedicated search application built
on top of Baloo.")
    (home-page "https://invent.kde.org/plasma/milou")
    (license (list license:gpl2+))))

(define-public qqc2-breeze-style
  (package
    (name "qqc2-breeze-style")
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1ipra7srk3xdq8b1jls46jkhbn1dcyadbl5gjm0lxqlivr49j87f"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase))
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
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0r90hrwib31wfv0mj42vf19c13642vb79m152rgps590i89yrkj9"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Sounds for the KDE desktop")
    (description "This package provides Oxygen sounds for the KDE desktop.")
    (license license:lgpl3+)))

(define-public ocean-sound-theme
  (package
    (name "ocean-sound-theme")
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/ocean-sound-theme"  "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1v51qhcr0swbajc0skwscp2h3m4qr337l8s145nmiv2scsn7sc86"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list qtbase))
    (home-page "https://invent.kde.org/plasma/ocean-sound-theme")
    (synopsis "Ocean Sound Theme for Plasma")
    (description "This package provides Ocean Sound Theme for Plasma.")
    (license license:lgpl3+)))

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
                             drkonqi
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
                             kpmcore
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
                             partitionmanager
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
                              'kwalletmanager)
                             (module-ref
                              (resolve-interface
                               '(gnu packages kde-systemtools))
                              'spectacle)))
                             ;; plasma-thunderbolt ;waiting for bolt
    (synopsis "The KDE Plasma desktop environment")
    (home-page "https://kde.org/plasma-desktop/")
    (description
     "KDE Plasma is an advanced graphical desktop system.")
    (license license:gpl2+)))

(define-public plasma5support
  (package
    (name "plasma5support")
    (version "6.3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "115x8ij1v8igac07kzbykxplmrr2g0x5n8s4j5mrn93w3v87clns"))))
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

(define-public mpvqt
  (package
    (name "mpvqt")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/mpvqt/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "130p3irs1llv7n1hs7w5xms29amh0aa2bi238wjgc9ww65gvhdwz"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list qtdeclarative))
    (propagated-inputs
     (list mpv))
    (arguments
     (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/libraries/mpvqt")
    (synopsis "libmpv wrapper for QtQuick2 and QML")
    (description "This package provides a libmpv wrapper for QtQuick2 and QML.")
    (license license:lgpl2.1+)))

(define-public plasmatube
  (package
    (name "plasmatube")
    (version "24.12.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/release-service/" version
                                  "/src/plasmatube-" version ".tar.xz"))
              (sha256
               (base32
                "0505s8hz6hcq8bc9cp9qpy4ccyznnczb1spg4x0l0n4ji7bg2m8n"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config python-minimal))
    (inputs
     (list kconfig
           kcoreaddons
           kdbusaddons
           kirigami
           kirigami-addons
           ki18n
           kwindowsystem
           purpose
           qtdeclarative
           qtmultimedia
           qtsvg
           qtkeychain-qt6
           mpvqt
           yt-dlp))
    (arguments (list #:qtbase qtbase))
    (home-page "https://apps.kde.org/plasmatube/")
    (synopsis "Kirigami YouTube video player")
    (description "This package provides YouTube video player based
on QtMultimedia and @command{yt-dlp}.")
    (license license:gpl3+)))

(define-public plasma-active-window-control
(let ((commit "0b1c091b5662fb21917064d7809b3be8b4a8be47")
       (revision "1"))
  (package
    (name "plasma-active-window-control")
    (version (git-version "1.7.3" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://invent.kde.org/plasma/plasma-active-window-control")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lns1n7p6b64z7l3bn27hni100pp3k2whzzzg0adr4hiynycdix6"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs
     (list kwindowsystem
           libsm
           plasma-framework
           qtdeclarative-5
           qtx11extras))
    (home-page "https://invent.kde.org/plasma/plasma-active-window-control")
    (synopsis "Plasma applet for controlling the currently active window")
    (description "This package provides window control applet for the current
active window on Plasma Desktop.")
    (license (list license:gpl2 license:gpl3)))))

(define-public plasma-browser-integration
  (package
    (name "plasma-browser-integration")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1kjxkdnziqn5lc0qghx3650ncbhvcaapv66qkfzs284zcr8r1ni5"))))
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
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma/plasma-browser-integration")
    (synopsis "Integrate browsers into the Plasma Desktop")
    (description
     "This package aims to provide better integration of web browsers with
the KDE Plasma 6 desktop.")
    (license license:gpl3+)))

(define-public plasma-desktop
  (package
    (name "plasma-desktop")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0s6hx5pzxf3f27c24b6b1n4wm8yhgdfn2knqhna09dri40h2jgdp"))))
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
                     (invoke "ctest" "-E" "(kcm-keyboard-keyboard_memory_persister_test|foldermodeltest)")))))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0xbzpqls99cf0vp9c3l9sa6qjp8im2gcwd0khyqhrphbzfvlllcv"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0sfsay0xkjgmal31n88q3xh67hmqk2lvl40ja723lf0s93dbgsgh"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0aa7fr82v0v66gppra2v8b5p24067m3p8r6mgy3i1snyhlifb5ap"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-nano-" version ".tar.xz"))
              (sha256
               (base32
                "1j8svy4r91qdb3snnwp1p0qdfv8mkkmxg6i1mnfzb11nfkn02d3k"))))
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
    (home-page "https://plasma-mobile.org/")
    (synopsis "Minimal Plasma Shell package")
    (description
     "This package provides a minimal implementation of Plasma Shell.")
    (license license:lgpl2.0+)))

(define-public plasma-nm
  (package
    (name "plasma-nm")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0s3c9xx1bpycf4nw79yssdnfxdsim47z6z4ska1mi1g9dj9pcv8l"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-mobile-" version ".tar.xz"))
              (sha256
               (base32
                "1lhqcpn8igc14mr2fjbrc108q1qi58h3r4d5vsgkvz4jnfxbkkmf"))))
    (build-system cmake-build-system)
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
    (native-inputs (list extra-cmake-modules pkg-config))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Sounds for Plasma Mobile devices")
    (description "This package provides sound files for Plasma Mobile.")
    (license (list license:cc0 license:cc-by4.0))))

(define-public plasma-pa
  (package
    (name "plasma-pa")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0iwxy4nc9zwjcvvn1bbk6g2djicri7a4cqdlafllahdcgrx36yai"))))
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
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kpeople
                  kirigami
                  kirigami-addons
                  kcoreaddons
                  kcontacts
                  qtdeclarative
                  qtsvg))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Phonebook for Plasma Mobile devices")
    (description "This package provides contacts application which allows
adding, modifying and removing contacts.")
    (license license:lgpl2.0+)))

(define-public plasma-redshift-control
  (let ((commit "d9f38a5f0bcf030be16db1776166581c16e802cb")
        (revision "1"))
    (package
      (name "plasma-redshift-control")
      (version (git-version "0.1-pre" revision commit))
      (home-page "https://invent.kde.org/plasma/plasma-redshift-control")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wadxhy6iljhikfw2rbj9dhwb86f2sgwyf62r7sfq6cszcpgp0xi"))))
      (build-system qt-build-system)
      (native-inputs (list extra-cmake-modules pkg-config))
      (inputs (list kwindowsystem plasma-framework redshift))
      (synopsis "Adjust color temperature")
      (description
       "This package provides color temperature control applet for the Plasma
Desktop.")
      (license (list license:lgpl2.1 license:lgpl3)))))

(define-public plasma-vault
  (package
    (name "plasma-vault")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0khblpprlf0jv54ix3lac1qabvj8i65ghgjp0qn5xfy677y76zkb"))))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jmj02zm5psi0k4m1vlzw31jq4h91c3miwz2mqkrc8385rbd8xcm"))))
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
                  qtdeclarative))
    (arguments (list #:qtbase qtbase))
    (synopsis "System sensors, process information and other system resources
monitor")
    (description "This package provides an interface for monitoring system
sensors, process information and other system resources.")
    (home-page "https://invent.kde.org/plasma/plasma-systemmonitor")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-welcome
  (package
    (name "plasma-welcome")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/plasma-welcome"
                                  "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "06i6762x93mpz9qyxa2y4nc71pyp51b647s7wzm9zvw2ml6ax8p9"))))
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
           qtsvg))
    (arguments (list #:qtbase qtbase))
    (synopsis "Plasma welcome screen")
    (description
     "This package provides a wizard for Plasma to configure settings.")
    (home-page "https://invent.kde.org/plasma/plasma-welcome")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0bs1dks8b7y2536706j051wzqcldin68574n1gvbiqb2dzj129dq"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools pkg-config qtsvg
                         qttools
                         xorg-server-for-tests
                         python-minimal))
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
                               "klipper-testHistoryModel")
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "105wy5sdaimqkqcbpqv2aa004xgdjsjfj81s9x1i6kfkqcn7mc6q"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen wallpapers for the KDE desktop")
    (description
     "This package provides wallpapers for the KDE desktop.")
    (license license:lgpl3+)))

(define-public print-manager
  (package
    (name "print-manager")
    (version "6.2.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/print-manager-" version ".tar.xz"))
       (sha256
        (base32 "1k7abx5x0bjw6hs6wryw1g10s33ch1wsdgvi5qzjmq2znzmicq05"))))
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
           qtdeclarative))
    (arguments (list #:qtbase qtbase))
    (home-page "https://invent.kde.org/plasma/print-manager")
    (synopsis "Manage print jobs and printers")
    (description
     "This package provides printing management for KDE.")
    (license license:gpl2+)))

(define-public polkit-kde-agent
  (package
    (name "polkit-kde-agent")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-1-" version ".tar.xz"))
              (sha256
               (base32
                "1ywkq982zq1l4jz1gbhjm09jd3y30mslryx84aw3vif68kjb7zxi"))))
    (build-system qt-build-system)
    (arguments (list #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kwindowsystem
                  kdbusaddons
                  kwidgetsaddons
                  kcoreaddons
                  kcrash
                  kiconthemes
                  polkit-qt6
                  qtdeclarative))
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
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0js63y5q5503bq6nd45kpzcxhpz3w1wfh6r0sgklpbpgsnb069bh"))))
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

(define-public system-settings
  (package
    (name "system-settings")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/systemsettings-" version ".tar.xz"))
              (sha256
               (base32
                "0xfkmdlkq5w9vwra1vbq7hxcf66028mbbvxb2vdd7nr58cljy0mm"))))
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
                  qtwebengine))
    (arguments (list #:qtbase qtbase))
    (synopsis "Control center to configure Plasma Desktop")
    (description "This package provides configuration UI for Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/systemsettings")
    (properties '((upstream-name . "systemsettings")))
    (license license:gpl2+)))

(define-public wacomtablet
  (package
    (name "wacomtablet")
    (version "6.2.5")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "mirror://kde/stable/plasma/" version
                              "/wacomtablet-" version ".tar.xz"))
              (sha256
               (base32
                "0yrqrxxa1233fqji12nz0gvkiiffwg0sfvbxhllmrman7s76zcz5"))))
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
                  xf86-input-wacom))
    (home-page "https://invent.kde.org/plasma/wacomtablet")
    (synopsis "KDE GUI for the Wacom Linux Drivers")
    (description "Provides KDE GUI for the Wacom Linux Drivers.")
    (license license:gpl2+)))
