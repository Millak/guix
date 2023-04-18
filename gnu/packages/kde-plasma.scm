;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018 Meiyo Peng <meiyo.peng@gmail.com>
;;; Copyright © 2019 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages kde-plasma)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
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
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-pim)
  ;; Including this module breaks the build
  ;#:use-module ((gnu packages kde-systemtools) #:select (konsole))
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages package-management) ; flatpak
  #:use-module (gnu packages video)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web))

(define-public bluedevil
  (package
    (name "bluedevil")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1dsah7rcx2brcd1d5x7bvhrixx5nvwp1fwq9b8k2zc038xzpx6ys"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config qttools-5))
    (inputs (list kcoreaddons
                  kwidgetsaddons
                  kdbusaddons
                  knotifications
                  kwindowsystem
                  plasma-framework
                  ki18n
                  kio
                  kdeclarative
                  bluez-qt
                  shared-mime-info
                  qtdeclarative-5))
    (synopsis "Manage the Bluetooth settings from Plasma")
    (description
     "This package provides Bluetooth manager for Plasma Shell.")
    (home-page "https://invent.kde.org/plasma/bluedevil")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public breeze
  (package
    (name "breeze")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0m3ggv5znzfcp25afgl7v5w7m27v5hdvv6jw9vrvwbqj8lzq1hlx"))))
    (build-system qt-build-system)
    ;; TODO: Warning at /gnu/store/…-kpackage-5.34.0/…/KF5PackageMacros.cmake:
    ;;   warnings during generation of metainfo for org.kde.breezedark.desktop:
    ;;   Package type "Plasma/LookAndFeel" not found
    ;; TODO: Check whether is makes sence splitting into several outputs, like
    ;; Debian does:
    ;; - breeze-cursor-theme
    ;; - "out", "devel"
    ;; - kde-style-breeze - Widget style
    ;; - kde-style-breeze-qt4 - propably not useful
    ;; - kwin-style-breeze
    ;; - qml-module-qtquick-controls-styles-breeze - QtQuick style
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
           kiconthemes ; for optional kde-frameworkintegration
           kpackage
           kwayland ; optional
           kwindowsystem
           qtbase-5
           qtdeclarative-5 ; optional
           qtx11extras))
    (home-page "https://invent.kde.org/plasma/breeze")
    (synopsis "Default KDE Plasma theme")
    (description "Artwork, styles and assets for the Breeze visual style for
the Plasma Desktop.  Breeze is the default theme for the KDE Plasma desktop.")
    (license license:gpl2+)))

(define-public breeze-gtk
  (package
    (name "breeze-gtk")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name
                                  "-" version ".tar.xz"))
              (sha256
               (base32
                "0wj8qzy2104ggczxagxm45zwsvvpwd6jjnbv1893mhlr6f2zrbmh"))))
    (build-system qt-build-system)
    (arguments
     '(#:tests? #f))                              ;no 'test' target
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
    (version "22.09")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-mobile/" version
                                  "/calindori-" version ".tar.xz"))
              (sha256
               (base32
                "1pwgdqznp76mhk0ikzjhy3c67qgk91kgv69ygqwzlh5hwiw5sl4n"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kconfig
                  kcoreaddons
                  kdbusaddons
                  ki18n
                  kirigami
                  kcalendarcore
                  knotifications
                  kpeople
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtsvg-5
                  qtgraphicaleffects))
    (home-page "https://invent.kde.org/plasma-mobile/calindori")
    (synopsis "Calendar for Plasma Mobile")
    (description
     "This package provides a touch friendly calendar application.")
    (license license:gpl3+)))

(define-public discover
  (package
    (name "discover")
    (version "5.25.5")
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
                "01vdi66c7v60db25p0qi0q73wgqw6dy2kirbk34bvhld41gpxhhv"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'set-LDFLAGS
                 (lambda _
                   (setenv "LDFLAGS" (string-append "-Wl,-rpath=" #$output
                                                    "/lib/plasma-discover"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E" "knsbackendtest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list appstream-qt
                  attica
                  fwupd ; optional
                  flatpak ; optional
                  kcoreaddons
                  kconfig
                  kcrash
                  kdbusaddons
                  ki18n
                  karchive
                  kxmlgui
                  kirigami
                  kuserfeedback
                  knewstuff
                  knotifications
                  kio
                  kdeclarative
                  kcmutils
                  kidletime
                  packagekit-qt5
                  qtdeclarative-5
                  qtgraphicaleffects
                  qtquickcontrols2-5))
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0vw4bfld2jdiwm4g3008x8s1lq1ydf87ckb0fvyzsp11hq9nnnk7"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "connectiontest")))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kcoreaddons
                  kconfig
                  kservice
                  kdeclarative
                  kjobwidgets
                  kio
                  kcrash
                  kcompletion
                  kwidgetsaddons
                  kwallet
                  knotifications
                  kidletime
                  kwindowsystem
                  ksyntaxhighlighting
                  qtdeclarative-5))
    (synopsis "Crash handler for KDE software")
    (description "This package provides an automatic handler for crashed apps.")
    (home-page "https://invent.kde.org/plasma/drkonqi")
    (license license:gpl2+)))

(define-public kactivitymanagerd
  (package
    (name "kactivitymanagerd")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0hsllhqi46n25sr27crnad053ghk3hni2w496g6d2qfmi20l3g4n"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list boost
                  kconfig
                  kcoreaddons
                  kwindowsystem
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

(define-public kdecoration
  (package
    (name "kdecoration")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kdecoration-" version ".tar.xz"))
              (sha256
               (base32
                "1x3scszz5nfwqciwc3bv0r04wgqlxs0c9j1090zvn6mjjg2nsyb7"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules))
    (inputs
     (list kcoreaddons ki18n qtbase-5))
    (home-page "https://invent.kde.org/plasma/kdecoration")
    (synopsis "Plugin based library to create window decorations")
    (description "KDecoration is a library to create window decorations.
These window decorations can be used by for example an X11 based window
manager which re-parents a Client window to a window decoration frame.")
    (license license:lgpl3+)))

(define-public kde-cli-tools
  (package
    (name "kde-cli-tools")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version ".tar.xz"))
              (patches (search-patches "kde-cli-tools-delay-mime-db.patch"))
              (sha256
               (base32
                "0i1lnkyb2bdvbhnr2wsgjy2sjichzxxqkvn30ca85rj21cavk2z3"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f ;TODO: Failing sub-tests 3/7
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
                  kactivities
                  kparts
                  plasma-workspace
                  qtx11extras
                  qtsvg-5))
    (synopsis "CLI tools for interacting with KDE")
    (description "This package provides command-line tools based on
KDE Frameworks 5 to better interact with the system.")
    (home-page "https://invent.kde.org/plasma/kde-cli-tools")
    (license license:lgpl2.0+)))

(define-public kdeplasma-addons
  (package
    (name "kdeplasma-addons")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1a5cq0jz69hlcr22wxi2p5mzxv5xcp88220irxmq0dhpk85kywlx"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E"
                               "(converterrunnertest|spellcheckrunnertest)")))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list karchive
                  kconfig
                  kcoreaddons
                  kdeclarative
                  kholidays
                  ki18n
                  kio
                  kcmutils
                  knotifications
                  krunner
                  kservice
                  kunitconversion
                  knewstuff
                  plasma-framework
                  purpose
                  sonnet
                  qtdeclarative-5))
                 ;qtwebengine-5)) ;; Optional for online dictionary
    (synopsis "Add-ons to improve your Plasma experience")
    (description
     "This package provides multiple addons for the Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/kdeplasma-addons")
    (license license:lgpl2.0)))

(define-public kgamma
  (package
    (name "kgamma")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "5-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1c305bs50km7bsn0phz7yj4x2168sxwsw9zxbpcgw4q3r53c0ywz"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kauth
                  kcoreaddons
                  kconfig
                  kconfigwidgets
                  kdoctools
                  ki18n))
    (synopsis "Adjust monitor gamma settings")
    (description
     "This package provides a tool to adjust your monitor gamma settings.")
    (home-page "https://invent.kde.org/plasma/kgamma5")
    (license license:gpl2+)))

(define-public khotkeys
  (package
    (name "khotkeys")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0c9i4vxiiv90dpzsd2hjwavks87cvwplkj63751z8mazax6r95as"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kdbusaddons
                  kdoctools
                  kglobalaccel
                  ki18n
                  kcmutils
                  kio
                  ktextwidgets
                  kxmlgui
                  kdelibs4support
                  plasma-workspace
                  qtx11extras))
    (synopsis "Trigger actions with the keyboard")
    (description
     "This package provides a way to trigger actions when certain keys
are pressed.")
    (home-page "https://invent.kde.org/plasma/khotkeys")
    (license license:lgpl2.0)))

(define-public kinfocenter
  (package
    (name "kinfocenter")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0zvki76yghkn158s7hb5g9drz7xaqxkmp2747404n2n0gmnmsdif"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-systemsettings-symlink
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "CMakeLists.txt"
                              (("\\$\\{KDE_INSTALL_FULL_BINDIR\\}/systemsettings5")
                               (search-input-file inputs
                                                  "/bin/systemsettings5"))))))))
    (native-inputs (list aha extra-cmake-modules kdoctools pkg-config))
    ;; * vulkaninfo
    ;; Wayland KCM
    (inputs (list dmidecode
                  ;; fwupdmgr ;; Packaged on master branch already
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
                  plasma-framework
                  qtbase-5
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0z85w7w207dnsinsz8yg6s408pwfy3l7wjkcsjpyg8aj8s9x6nl7"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools))
    (inputs (list ki18n
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
    (version "22.09")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-mobile/" version
                                  "/kongress-" version ".tar.xz"))
              (sha256
               (base32
                "0pjp2s774sgw2dklqib8alm1a9fkixy3s92i2v8v00znx08zf2jz"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    ;; NOTE: Reporting bugs is linked to web browser, better not link it and let
    ;; it reslove through xdg-open in the run time
    (inputs (list kirigami
                  kdbusaddons
                  ki18n
                  kcalendarcore
                  kconfigwidgets
                  kwindowsystem
                  kcoreaddons
                  kcontacts
                  kitemmodels
                  knotifications
                  kxmlgui
                  kiconthemes
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtgraphicaleffects
                  qtsvg-5))
    (home-page "https://apps.kde.org/kongress/")
    (synopsis "Companion application for conferences")
    (description "This application provides list of upcoming conferences with
the schedule and venue information.")
    (license license:gpl3+)))

(define-public kscreen
  (package
    (name "kscreen")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0361202n0366jiyv61w06jikh2i4s1fzr6x3chmdykx4fgvbqj7s"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: All tests fail
     (list #:tests? #f))
    (native-inputs (list extra-cmake-modules qttools-5 pkg-config))
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
                  libkscreen
                  libxi
                  plasma-wayland-protocols
                  qtsensors
                  qtbase-5
                  qtx11extras
                  xcb-util))
    (propagated-inputs (list plasma-framework))
    (home-page "https://invent.kde.org/plasma/kscreen")
    (synopsis "Screen management software")
    (description "This package provides the screen management software for
KDE Plasma Workspaces.")
    (license license:gpl2+)))

(define-public ksshaskpass
  (package
    (name "ksshaskpass")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/ksshaskpass-" version ".tar.xz"))
              (sha256
               (base32
                "0z8pp2mh4s98jh8jkf6r8v014gb6svapmp0m1npzw7zcmr6jvpn6"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kcoreaddons ki18n kwallet kwidgetsaddons qtbase-5))
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "07xm6gn2k3vsl1pkrd2n9w8w8b7jq26h3cpslqha4ipw0by2mlqa"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "ksystemstatstest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list glib
                  kcoreaddons
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
    (version "0.10.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/latte-dock/"
                                  "latte-dock-" version ".tar.xz"))
              (sha256
               (base32
                "0ali9i0y0y1c5mdaps5ybhk4nqvzzs5jq27wj8rg8xxqjyfvbah0"))))
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/layer-shell-qt-" version ".tar.xz"))
              (sha256
               (base32
                "1mvhklq7n9hhjing704jr3ybq58ixwr9fdg3phnqhmdnqc2q3w0l"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list libxkbcommon
           qtbase-5
           qtdeclarative-5
           qtwayland-5
           wayland
           wayland-protocols))
    (home-page "https://invent.kde.org/plasma/layer-shell-qt")
    (synopsis "Qt component for the Wayland ql-layer-shell protocol")
    (description "Qt component for the Wayland ql-layer-shell protocol.")
    (license license:gpl2+)))

(define-public kscreenlocker
  (package
    (name "kscreenlocker")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/kscreenlocker-" version ".tar.xz"))
              (sha256
               (base32
                "0mivx0l266310iy52qi94b6wi8w2a8nl7cjn5750x66dz81jl3yj"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ;; TODO: make tests pass
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'check-setup
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (system "Xvfb :1 -screen 0 640x480x24 &")
             (setenv "DISPLAY" ":1")
             #t))
         (delete 'check)
         ;; Tests use the installed library and require a DBus session.
         (add-after 'install 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (if tests?
                 (begin
                   (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                   (invoke "dbus-launch" "ctest")))
             #t)))))
    (native-inputs
     (list extra-cmake-modules pkg-config
           ;; For tests.
           dbus xorg-server-for-tests))
    (inputs
     (list kcmutils
           kconfig
           kcrash
           kdeclarative
           kglobalaccel
           ki18n
           kidletime
           knotifications
           ktextwidgets
           kwayland
           kwindowsystem
           kxmlgui
           layer-shell-qt
           libseccomp ;for sandboxing the look'n'feel package
           libxcursor ;missing in CMakeList.txt
           libxi ;XInput, required for grabbing XInput2 devices
           linux-pam
           elogind ;optional loginctl support
           qtbase-5
           qtdeclarative-5
           qtx11extras
           solid
           wayland
           xcb-util-keysyms))
    (home-page "https://invent.kde.org/plasma/kscreenlocker")
    (synopsis "Screen locking library")
    (description
     "@code{kscreenlocker} is a library for creating secure lock screens.")
    (license license:gpl2+)))

(define-public ksysguard
  (package
    (name "ksysguard")
    (version "5.22.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://kde/stable/ksysguard/" version
                          "/ksysguard-" version ".tar.xz"))
      (sha256
       (base32 "0bb2aj46v7ig0wn3ir68igryl2gblz2n75cddn8fwamvbx76570g"))))
    (build-system qt-build-system)
    ;; TODO: No tests found
    (native-inputs
     (list extra-cmake-modules kdoctools))
    (inputs
     (list kconfig
       kcoreaddons
       kdbusaddons
       ki18n
       kiconthemes
       kinit
       kio
       kitemviews
       knewstuff
       knotifications
       kwindowsystem
       libksysguard
       `(,lm-sensors "lib")
       qtbase-5))
    (home-page "https://www.kde.org/applications/system/ksysguard/")
    (synopsis "Plasma process and performance monitor")
    (description "KSysGuard is a program to monitor various elements of your
system, or any other remote system with the KSysGuard daemon (ksysgardd)
installed.")
    (license license:gpl2+)))

(define-public libkscreen
  (package
    (name "libkscreen")
    (version "5.25.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "17f2pbbkpmw3a32nsqlwd0x5xb71l6dzrh7ldwcqv46nvb54yhfa"))))
    (build-system qt-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (setenv "WAYLAND_DISPLAY" "libkscreen-test-wayland-backend-0")
               (invoke "ctest" "-E"
                "(kscreen-testscreenconfig|kscreen-testqscreenbackend|kscreen-testkwaylandbackend|kscreen-testkwaylandconfig|kscreen-testkwaylanddpms)")))))))
    (native-inputs
     (list extra-cmake-modules
           pkg-config
           ;; For testing.
           dbus))
    (inputs
     (list kwayland libxrandr plasma-wayland-protocols
           qtbase-5 qtwayland-5 wayland qtx11extras))
    (home-page "https://community.kde.org/Solid/Projects/ScreenManagement")
    (synopsis "KDE's screen management software")
    (description "KScreen is the new screen management software for KDE Plasma
Workspaces which tries to be as magic and automatic as possible for users with
basic needs and easy to configure for those who want special setups.")
    (license license:gpl2+)))

(define-public libksysguard
  (package
    (name "libksysguard")
    (version "5.25.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://kde/stable/plasma/" version
                           "/libksysguard-" version ".tar.xz"))
       (patches (search-patches "libksysguard-qdiriterator-follow-symlinks.patch"))
       (sha256
        (base32 "1gqsjsdkp25abqqp4f6cv6ih199q9ad7q1a4lkhjgsh4h8jq1856"))))
    (native-inputs
     (list bash-minimal extra-cmake-modules pkg-config qttools-5))
    (inputs
     (list kauth
           kcompletion
           kconfig
           kconfigwidgets
           kcoreaddons
           kdeclarative
           kglobalaccel
           ki18n
           kiconthemes
           kio
           knewstuff
           kservice
           kwidgetsaddons
           kwindowsystem
           libnl
           libcap
           libpcap
           `(,lm-sensors "lib")
           plasma-framework
           qtbase-5
           qtdeclarative-5
           qtscript
           qtwebchannel-5
           qtwebengine-5
           qtx11extras
           zlib))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "14w803lc3s2c0f4mqzzdhpfy5qnlz7wv00pbrc3v4k3zv381ci8n"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f)) ;no tests
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list linux-pam kwallet libgcrypt socat))
    (synopsis "PAM Integration with KWallet")
    (description "Provide PAM Integration with KWallet to unlock KWallet when
you login.")
    (home-page "https://invent.kde.org/plasma/kwallet-pam")
    (license (list license:lgpl2.1+))))

(define-public kwayland-server
  (package
    (name "kwayland-server")
    (version "5.24.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://kde/stable/plasma/" version
                    "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1wwgb1p177g6vvyf4l5jisigwvy5756ray1x355mlp7bi1pfs664"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list plasma-wayland-protocols
           qtbase-5
           qtwayland-5
           kwayland
           wayland
           wayland-protocols))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-install-path
           (lambda _
             ;; Fixes errors including nonexistant /include/KF5
             (substitute* "src/server/CMakeLists.txt"
               (("KF5_INSTALL") "KDE_INSTALL"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "HOME" (getcwd))
               (setenv "XDG_RUNTIME_DIR" (getcwd))
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (invoke "ctest" "-E"
                       ;; This test fails inconsistently.
                       "kwayland-testDragAndDrop")))))))
    (home-page "https://api.kde.org/kwayland-server/html/index.html")
    (synopsis "KDE wayland server component")
    (description
     "KWayland is a Qt-style API to interact with the wayland-client and
wayland-server API.")
    ;; Most files are LGPL2.1 or LGPL3.0 only, at the users option.
    (license (list license:lgpl2.1 license:lgpl3
                   ;; src/server/drm_fourcc.h carries the MIT license.
                   license:expat))))

(define-public kwayland-integration
  (package
    (name "kwayland-integration")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "10xl7yrj519b9s5vq0hqqfz3vvg1fdwggw96snzm44iwycqbgss8"))))
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
    (inputs (list kguiaddons
                  kidletime
                  kwindowsystem
                  kwayland
                  libxkbcommon
                  wayland
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
    (version "5.25.5")
    (source (origin
               (method url-fetch)
               (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                   name "-" version ".tar.xz"))
               (sha256
                (base32
                 "1dh7ydwxbb9r53p353d53gq7w9vmp7idvsr4s5ldxmah35436v2s"))))
    (build-system qt-build-system)
    (arguments
     (list
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
                  '("src/wayland/tests/renderingservertest.cpp"
                    "src/wayland/tests/waylandservertest.cpp")
                (("QByteArrayLiteral\\(\"Xwayland\"\\)")
                 (string-append
                  "QByteArrayLiteral(\"" (which "Xwayland") "\")")))
              (substitute* '("src/xwayland/xwaylandlauncher.cpp")
                (("(m_xwaylandProcess->setProgram.QStringLiteral..)(Xwayland)(...;)"
                  _ a Xwayland b)
                 (string-append a
                                (which "Xwayland") b)))
              (substitute* '("cmake/modules/Findhwdata.cmake")
                (("/usr/share")
                 (string-append #$hwdata:pnp "/share")))))
          (add-after 'install 'add-symlinks
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((kst5 (string-append #$output
                                         "/share/kservicetypes5/")))
                (symlink (string-append kst5 "kwineffect.desktop")
                         (string-append kst5 "kwin-effect.desktop"))
                (symlink (string-append kst5 "kwinscript.desktop")
                         (string-append kst5 "kwin-script.desktop")))))
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
                                       "/lib/qt5/plugins:"
                                       (getenv "QT_PLUGIN_PATH")))
                (setenv "DISPLAY" ":1")
                (system "Xvfb :1 &")
                (sleep 5)
                (invoke "ctest" "-E"
                        (string-join
                          (list "kwayland-testXdgDecoration"
                                "kwin-testXkb"
                                "kwin-testPointerInput"
                                "kwin-testXdgShellWindow"
                                "kwin-testXdgShellWindow-waylandonly"
                                "kwin-testSceneOpenGLES"
                                "kwin-testSceneOpenGLES-waylandonly"
                                "kwin-testNightColor"
                                "kwin-testNightColor-waylandonly"
                                "kwin-testSceneQPainter"
                                "kwin-testLibinputDevice")
                          "|"))))))))
    (native-inputs (list extra-cmake-modules
                         dbus
                         kdoctools
                         mesa-utils
                         pkg-config
                         qttools-5
                         wayland-protocols
                         xorg-server-for-tests))
    (inputs (list breeze
                  eudev
                  fontconfig
                  freetype
                  `(,hwdata "pnp")
                  kactivities
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
                  ki18n
                  kiconthemes
                  kidletime
                  kio
                  kirigami
                  knewstuff
                  knotifications
                  kpackage
                  krunner
                  kscreenlocker
                  ktextwidgets
                  kwayland
                  kwayland-server
                  kwindowsystem
                  kxmlgui
                  libqaccessibilityclient
                  lcms
                  libcap
                  libepoxy
                  libglvnd ; For OpenGLES
                  libinput
                  libxkbcommon
                  pipewire
                  plasma-framework
                  plasma-wayland-protocols
                  qtbase-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtwayland-5
                  qtx11extras
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "03gw3czdgyf35n6x79x416rk6f7w1ayzmy5pb65v9733nx1j34mh"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons ki18n kpty knotifications))
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0ijdbiq169sy47g0x22pj9x6abldpfxcnski4w1jh9hi9qz1rc25"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  ki18n
                  kdeclarative
                  kitemmodels
                  kservice
                  plasma-framework
                  kwindowsystem
                  krunner
                  qtdeclarative-5))
    (synopsis "Dedicated search application built on top of Baloo")
    (description "This package provides a dedicated search application built
on top of Baloo.")
    (home-page "https://invent.kde.org/plasma/milou")
    (license (list license:gpl2+))))

(define-public oxygen-sounds
  (package
    (name "oxygen-sounds")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "04rn3ccqszznknvblz2i8r406m32hk4d3yzma4vzq93jfk57hdf1"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Sounds for the KDE desktop")
    (description "This package provides Oxygen sounds for the KDE desktop.")
    (license license:lgpl3+)))

(define-public plasma
  (package
    (name "plasma")
    (version "5.25.5")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder #~(begin
                         (mkdir #$output))))
    (propagated-inputs (list bluedevil
                             breeze
                             breeze-gtk
                             discover
                             drkonqi
                             kactivitymanagerd
                             kde-cli-tools
                             ;; kde-gtk-config
                             kdecoration
                             kdeplasma-addons
                             kgamma
                             khotkeys
                             kinfocenter
                             kmenuedit
                             kscreen
                             kscreenlocker
                             ksshaskpass
                             ksystemstats
                             kwallet-pam
                             kwayland-integration
                             kwin
                             kwrited
                             kinit
                             layer-shell-qt
                             libkscreen
                             libksysguard
                             milou
                             ;; oxygen
                             oxygen-sounds
                             plasma-browser-integration
                             plasma-desktop
                             plasma-disks
                             plasma-firewall
                             plasma-integration
                             plasma-nm
                             plasma-pa
                             plasma-systemmonitor
                             ;; plasma-thunderbolt ;; waiting for bolt
                             plasma-vault
                             plasma-workspace
                             plasma-workspace-wallpapers
                             polkit-kde-agent
                             powerdevil
                             sddm
                             system-settings
                             xdg-desktop-portal-kde))
    (synopsis "The KDE Plasma desktop environment")
    (home-page "https://kde.org/plasma-desktop/")
    (description
     "KDE Plasma is an advanced graphical desktop system.")
    (license license:gpl2+)))

(define-public plasma-bigscreen
  (package
    (name "plasma-bigscreen")
    (version "5.25.90")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/unstable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1445j8hzfvh2z91fa8nxrc0z576c67cq5fxcs19pmzpnjjli1ads"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-startplasma
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "bin/plasma-bigscreen-wayland.in"
                              (("^startplasma-wayland")
                               (search-input-file inputs
                                                  "/bin/startplasma-wayland")))
                              (substitute* "bin/plasma-bigscreen-x11"
                                (("startplasma-x11")
                                 (search-input-file inputs
                                                    "/bin/startplasma-x11"))))))))
    (native-inputs (list extra-cmake-modules))
    (inputs (list kactivities
                  kactivities-stats
                  plasma-framework
                  ki18n
                  kirigami
                  kdeclarative
                  kcmutils
                  knotifications
                  kio
                  kwayland
                  kwindowsystem
                  plasma-workspace
                  qtbase-5
                  qtmultimedia-5))
    (home-page "https://invent.kde.org/plasma/plasma-bigscreen")
    (synopsis "Plasma shell for TVs")
    (description
     "This package provides a big launcher designed for large screens.  It
is controllable via voice or TV remote.")
    (license license:gpl2+)))

(define-public plasmatube
  (package
    (name "plasmatube")
    (version "22.09")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-mobile/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "00w9p5fcpv4s406lmcdcbrxf19sgkvf9yy8pfjmf1asvvvi8bpnk"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs
     (list kconfig
           kirigami
           ki18n
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtquickcontrols2-5
           qtsvg-5
           youtube-dl))
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "18pbn5ic5l3m8i1y99yprpwd4x4746aq3abqn1f2cq5h2683h2ia"))))
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
                  kactivities
                  purpose
                  kfilemetadata
                  kjobwidgets
                  qtdeclarative-5))
    (propagated-inputs (list plasma-workspace))
    (home-page "https://invent.kde.org/plasma/plasma-browser-integration")
    (synopsis "Integrate browsers into the Plasma Desktop")
    (description
     "This package aims to provide better integration of web browsers with
the KDE Plasma 5 desktop.")
    (license license:gpl3+)))

(define-public plasma-desktop
  (package
    (name "plasma-desktop")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "05s1pkwr4xmkghp8jrwcyrvjm83n68ngmk2694055xcfgi0pxicg"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules
                         dbus
                         kdoctools
                         intltool
                         pkg-config
                         qtsvg-5
                         qttools-5))
    (inputs (list packagekit-qt5
                  signon-plugin-oauth2
                  signond
                  attica
                  appstream-qt
                  baloo
                  breeze
                  breeze-icons
                  eudev
                  fontconfig
                  glib
                  ibus
                  kaccounts-integration
                  kactivities
                  kactivities-stats
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
                  kdelibs4support
                  kglobalaccel
                  kguiaddons
                  kholidays
                  ki18n
                  kiconthemes
                  kidletime
                  kinit
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
                  layer-shell-qt
                  libaccounts-qt
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
                  plasma-framework
                  plasma-wayland-protocols
                  pulseaudio
                  prison
                  qqc2-desktop-style
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtwayland
                  qtx11extras
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
    (propagated-inputs (list iso-codes kirigami plasma-workspace))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "kcms/keyboard/iso_codes.h"
                     (("\"/usr/share/xml/iso-codes\"")
                      (string-append "\"" (search-input-directory
                                           inputs "/share/xml/iso-codes")
                                     "\"")))))
               (add-after 'unpack 'patch-qml-import-path
                 (lambda _
                   (substitute*
                       '("applets/pager/package/contents/ui/main.qml"
                         "containments/desktop/package/contents/ui/FolderView.qml"
                         "containments/desktop/package/contents/ui/main.qml"
                         "containments/panel/contents/ui/main.qml")
                     (("^import \"(utils|FolderTools|LayoutManager).js\" as "
                       line mod)
                      (string-append "import \"../code/" mod
                                     ".js\" as ")))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (setenv "XDG_RUNTIME_DIR" (getcwd))
                     (setenv "XDG_CACHE_HOME" (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     (invoke "ctest" "-E" "foldermodeltest")))))))
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0vci2cf8vx2lclypys9rmvjb8haakv6ksrvqm5j28pazbq2kskvl"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kcoreaddons
                  kdbusaddons
                  knotifications
                  ki18n
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0k3pc0dcsjr2hhh4hr8vhsvaddn66s6imm8skrr02icxqvljs6jh"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list iproute
                  kcoreaddons
                  kcmutils
                  ki18n
                  kdeclarative
                  python
                  qtdeclarative-5))
    (synopsis "Control Panel for system firewall")
    (description "This package provides interface to system firewall.")
    (home-page "https://invent.kde.org/plasma/plasma-firewall")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-integration
  (package
    (name "plasma-integration")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1pab56cg2zi8fcaar53lhhh98iw7l07f5lkymkqhsh8a5crfc3yr"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                  ;TODO: Failing tests
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
                  kio
                  ki18n
                  kwidgetsaddons
                  kconfigwidgets
                  kiconthemes
                  knotifications
                  libxcb
                  libxcursor
                  plasma-wayland-protocols
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtwayland-5
                  qtx11extras
                  wayland))
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
    (version "5.24.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.kde.org/stable/plasma/"
                                  version "/plasma-nano-" version ".tar.xz"))
              (sha256
               (base32
                "13jxhfi3c3dhg7zdyfqnsii661h1am0w9dsv82dalqvwr1mw28l5"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list qtbase-5
                  qtdeclarative-5
                  plasma-framework
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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "036bx0qjrjanfxy8aiy6ab7rmm2h8l7wlkvlwhzw2hgl1w03xjps"))))
    (build-system qt-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "ctest" "-E" "mobileproviderstest")))))))
    (native-inputs (list extra-cmake-modules pkg-config))
    (home-page "https://invent.kde.org/plasma/plasma-nm")
    (inputs (list kconfigwidgets
                  kcompletion
                  kcoreaddons
                  kdeclarative
                  kdbusaddons
                  kio
                  ki18n
                  networkmanager-qt
                  knotifications
                  kirigami
                  plasma-framework
                  modemmanager-qt
                  network-manager
                  qca
                  kservice
                  solid
                  prison
                  kwallet
                  kwidgetsaddons
                  kwindowsystem
                  openconnect
                  qtdeclarative-5))
    (synopsis "Plasma applet for managing network connections")
    (description "This package provides Plasma applet for managing network
connections.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public plasma-mobile
  (package
    (name "plasma-mobile")
    (version "5.24.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.kde.org/stable/plasma/"
                                  version "/plasma-mobile-" version ".tar.xz"))
              (sha256
               (base32
                "1bwmy7xvd8wmh0snqqjh9jjgawib8ks2g30w48sqxwhplhf3da58"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'wrap-script
                          (lambda* (#:key inputs outputs #:allow-other-keys)
                            (wrap-program (string-append #$output
                                                         "/bin/kwinwrapper")
                                          `("PATH" ":" prefix
                                            (,(string-append #$plasma-framework
                                                             "/bin")))))))))
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list bash-minimal
                  kdeclarative
                  ki18n
                  kio
                  knotifications
                  kwayland
                  kwin
                  modemmanager-qt
                  networkmanager-qt
                  plasma-framework
                  qtbase-5))
    (home-page "https://plasma-mobile.org/")
    (synopsis
     "General UI components for Plasma Phone including shell, containment and applets")
    (description "This package provides user-friendly, privacy-enabling and
customizable platform for mobile devices.")
    (license (list license:gpl3+ license:lgpl2.1+))))

(define-public plasma-mobile-settings
  (package
    (name "plasma-mobile-settings")
    (version "22.02")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-mobile/" version
                                  "/plasma-settings-" version ".tar.xz"))
              (sha256
               (base32
                "0b7lj3r9z9cz2vr0h15sqqxdaa7m07hsk8i2p8nf4a3yh02ywsxy"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list qtbase-5
                  qtdeclarative-5
                  kio
                  modemmanager-qt
                  networkmanager-qt
                  ki18n
                  plasma-framework
                  kdeclarative
                  kdbusaddons))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Settings application for Plasma Mobile")
    (description
     "This package provides Settings application for Plasma Mobile.")
    (license license:gpl2+)))

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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/"
                                  version "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0g06pm1isnzx4hv6380rjpyr22s4j5iw9083s71vnl4npx28npvb"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools pkg-config))
    (inputs (list glib
                  kcoreaddons
                  kdeclarative
                  kglobalaccel
                  knotifications
                  kwindowsystem
                  kirigami
                  ki18n
                  qtdeclarative-5))
    (propagated-inputs (list libcanberra pulseaudio plasma-framework))
    (home-page "https://invent.kde.org/plasma/plasma-pa")
    (synopsis "Plasma applet for audio volume management using PulseAudio")
    (description
     "This package provides Plasma applet for audio volume management using
PulseAudio.")
    (license (list license:lgpl2.1 license:lgpl3))))

(define-public plasma-pass
  (package
    (name "plasma-pass")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/" name "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "107pd6cnkd46px83pm3q7vbw10g5pd0qsw77jmr0c774k4xv1w01"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n kitemmodels kwindowsystem oath-toolkit qtdeclarative-5))
    (propagated-inputs (list plasma-framework))
    (home-page "https://invent.kde.org/plasma/plasma-pass")
    (synopsis "Plasma applet for the Pass password manager")
    (description
     "This package provides a Plasma applet for the Pass password manager.")
    (license license:lgpl2.1+)))

(define-public plasma-phonebook
  (package
    (name "plasma-phonebook")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma-phonebook/"
                                  version "/plasma-phonebook-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "13nnzdzpganlp319sc9dm9w5hsjhw4f3w8rb80q3nd8q6nyrpky8"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kpeople
                  kirigami
                  kpeoplevcard
                  kcoreaddons
                  kcontacts
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtsvg-5))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Phonebook for Plasma Mobile devices")
    (description "This package provides contacts application which allows
adding, modifying and removing contacts.")
    (license license:lgpl2.0+)))

(define-public plasma-phone-components
  (package
    (name "plasma-phone-components")
    (version "5.23.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/plasma-phone-components-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0ml5pyi90nlmx5550sf3x9263f8mypj4jmdskzabzhnz44ck8vy9"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules pkg-config qttools))
    (inputs (list qtbase-5
                  qtdeclarative
                  kactivities
                  kauth
                  kbookmarks
                  kwin
                  kcodecs
                  kcompletion
                  kconfig
                  kconfigwidgets
                  kcoreaddons
                  kdbusaddons
                  kdeclarative
                  ki18n
                  kio
                  kitemviews
                  kjobwidgets
                  knotifications
                  kpackage
                  kpeople
                  kservice
                  kwayland
                  kwidgetsaddons
                  kwindowsystem
                  kxmlgui
                  libphonenumber
                  modemmanager-qt
                  plasma-framework
                  solid))
    (home-page "https://plasma-mobile.org/")
    (synopsis "Modules providing phone functionality for Plasma")
    (description "This package provides user-friendly, privacy-enabling
and customizable platform for mobile devices.")
    (license (list license:gpl3+ license:lgpl2.1+))))

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
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "15nf4myl6sry0930m8qiixpr1i8bh1g58cy2gf304h029vr2fn5j"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules pkg-config))
    (inputs (list kio
                  ki18n
                  kconfigwidgets
                  kconfig
                  kactivities
                  kdbusaddons
                  kiconthemes
                  networkmanager-qt
                  libksysguard
                  plasma-framework
                  qtdeclarative-5))
    (home-page "https://invent.kde.org/plasma/plasma-vault")
    (synopsis "Plasma applet and services for creating encrypted vaults")
    (description "Provides Plasma applet and services for creating encrypted
	vaults.")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-systemmonitor
  (package
    (name "plasma-systemmonitor")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0rs3avhpmfapzha8dkir9ny60ba2m92bbfv4avsd85vz5kggg29h"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kconfig
                  kdeclarative
                  kservice
                  kiconthemes
                  kglobalaccel
                  kio
                  kdbusaddons
                  kirigami
                  knewstuff
                  ksystemstats
                  kitemmodels
                  libksysguard
                  qtdeclarative-5
                  qtquickcontrols2-5))
    (synopsis "System sensors, process information and other system resources
monitor")
    (description "This package provides an interface for monitoring system
sensors, process information and other system resources.")
    (home-page "https://invent.kde.org/plasma/plasma-systemmonitor")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-welcome
(let ((commit "dac7569078782a96f122782c15d34e51737d2b89") ; no tags
      (revision "1"))
  (package
    (name "plasma-welcome")
    (version (git-version "0.1-pre" revision commit))
    (home-page "https://invent.kde.org/plasma/plasma-welcome")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x7ra699r5a9kpa3isdnx6af4j6778kw2pmprnx4s8f1rwk2idhh"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config))
    (inputs
     (list kcoreaddons
           kdbusaddons
           kdeclarative
           ki18n
           kio
           kirigami
           knotifications
           kservice
           kwindowsystem
           networkmanager-qt
           plasma-framework
           qtdeclarative-5
           qtgraphicaleffects
           qtsvg-5
           qtquickcontrols2-5
           system-settings))
    (synopsis "Plasma welcome screen")
    (description
     "This package provides a wizard for Plasma to configure settings.")
    (license (list license:gpl2 license:gpl3)))))

(define-public plasma-workspace
  (package
    (name "plasma-workspace")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pxwg3i07sipfchn4qkmlr6kcgqbhj2s10xq69wi32x1fc13jx5x"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules kdoctools pkg-config qtsvg-5
                         qttools-5))
    (inputs (list appmenu-gtk-module
                  appstream-qt
                  baloo
                  breeze
                  breeze-icons
                  dbus
                  fontconfig
                  iso-codes
                  kactivities
                  kactivities-stats
                  karchive
                  kcmutils
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
                  kinit
                  kio
                  kio-extras
                  kio-fuse
                  kitemmodels
                  kirigami
                  knewstuff
                  knotifications
                  knotifyconfig
                  kquickcharts
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
                  layer-shell-qt
                  libkscreen
                  libksysguard
                  libqalculate
                  gmp
                  mpfr
                  libsm
                  libxft
                  libxkbcommon
                  libxrender
                  libxtst
                  networkmanager-qt
                  phonon
                  pipewire
                  plasma-framework
                  plasma-workspace-wallpapers
                  plasma-wayland-protocols
                  prison
                  qtbase-5
                  qtdeclarative-5
                  qtquickcontrols2-5
                  qtwayland-5
                  qtgraphicaleffects
                  qtx11extras
                  wayland
                  wayland-protocols
                  xcb-util
                  xcb-util-image
                  xcb-util-keysyms
                  xrdb
                  xmessage
                  xsetroot
                  zlib))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-wallpaper
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "sddm-theme/theme.conf.cmake"
                     (("background=..KDE_INSTALL_FULL_WALLPAPERDIR.")
                      (string-append "background="
                                     #$(this-package-input "breeze")
                                     "/share/wallpapers")))))
               (add-after 'unpack 'patch-workspace-bins
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((xmessage (search-input-file inputs "/bin/xmessage"))
                         (xsetroot (search-input-file inputs "/bin/xsetroot"))
                         (xrdb (search-input-file inputs "/bin/xrdb"))
                         (kinit #$(this-package-input "kinit")))
                     (substitute* "startkde/startplasma.cpp"
                       (("xmessage") xmessage)
                       (("xsetroot") xsetroot))
                     (substitute* (list "kcms/fonts/fontinit.cpp"
                                        "kcms/fonts/fonts.cpp"
                                        "kcms/krdb/krdb.cpp")
                       (("xrdb") xrdb))
                     (substitute* "startkde/plasma-session/startup.cpp"
                       (("CMAKE_INSTALL_FULL_LIBEXECDIR_KF5..")
                        (string-append "\"" kinit
                                       "/lib/libexec/kf5")))
                     (substitute* (list
                                   "startkde/startplasma-wayland.cpp"
                                   "startkde/startplasma-x11.cpp")
                       (("kdeinit5_shutdown")
                        (string-append kinit "/bin/kdeinit5_shutdown"))))))
               (delete 'check)
               (add-after 'install 'check-after-install
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" (getcwd))
                     (setenv "XDG_RUNTIME_DIR" (getcwd))
                     (setenv "XDG_CACHE_HOME" (getcwd))
                     (setenv "QT_QPA_PLATFORM" "offscreen")
                     (setenv "QT_PLUGIN_PATH"
                             (string-append #$output
                                            "/lib/qt5/plugins:"
                                            (getenv "QT_PLUGIN_PATH")))
                     (invoke "ctest" "-E"
                             "(appstreamtest|lookandfeel-kcmTest|tst_triangleFilter|systemtraymodeltest|testdesktop| screenpooltest)")))))))
    (home-page "https://invent.kde.org/plasma/plasma-workspace")
    (synopsis "Plasma workspace components")
    (description
     "Workspaces provide support for KDE Plasma Widgets, integrated search,
hardware management, and a high degree of customizability.")
    (license (list license:gpl2 license:gpl3))))

(define-public plasma-workspace-wallpapers
  (package
    (name "plasma-workspace-wallpapers")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0pdgg8h6zwyxf1pj241mg3hd94wh38bqxj68af5jfp10xrwkzcyr"))))
    (build-system cmake-build-system)
    (native-inputs (list extra-cmake-modules))
    (home-page "https://community.kde.org/Frameworks")
    (synopsis "Oxygen wallpapers for the KDE desktop")
    (description
     "This package provides wallpapers for the KDE desktop.")
    (license license:lgpl3+)))

(define-public polkit-kde-agent
  (package
    (name "polkit-kde-agent")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-1-" version ".tar.xz"))
              (sha256
               (base32
                "1qb9nxlkgcv0c30l2vqgf9xs9d220q5pg7ikpb6zjzvj4wsvlvwd"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list ki18n
                  kwindowsystem
                  kdbusaddons
                  kwidgetsaddons
                  kcoreaddons
                  kcrash
                  kiconthemes
                  polkit-qt))
    (synopsis "Polkit authentication UI for Plasma")
    (description
     "This package contains a daemon providing a Polkit authentication
UI for Plasma")
    (home-page "https://invent.kde.org/plasma/polkit-kde-agent-1")
    (license license:gpl2+)))

(define-public powerdevil
  (package
    (name "powerdevil")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0anisirn7z8aw442npdnk1csb5ghpzj2hx49gpw4l6ijk70b76pr"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules qttools-5 pkg-config))
    (inputs (list bluez-qt
                  glib
                  kauth
                  kactivities
                  kscreen
                  kidletime
                  kconfig
                  kdbusaddons
                  solid
                  ki18n
                  kcrash
                  knotifyconfig
                  networkmanager-qt
                  kio
                  kwayland
                  kglobalaccel
                  kcrash
                  knotifications
                  kirigami
                  libcap
                  libkscreen
                  network-manager
                  plasma-workspace
                  eudev
                  qtx11extras))
    (synopsis "Manage power consumption")
    (description "This package provides the power consumption settings
of a Plasma shell.")
    (home-page "https://invent.kde.org/plasma/powerdevil")
    (license license:gpl2+)))

(define-public system-settings
  (package
    (name "system-settings")
    (version "5.25.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version
                                  "/systemsettings-" version ".tar.xz"))
              (sha256
               (base32
                "0n7mf6ygi8fgn1m6pk2fadnqj1h58mxqni3h19xbi373wfypq5fl"))))
    (build-system qt-build-system)
    (native-inputs (list extra-cmake-modules))
    (inputs (list kauth
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
                  kactivities
                  kactivities-stats
                  kguiaddons
                  kirigami
                  knotifications
                  krunner
                  plasma-workspace
                  qtdeclarative-5))
    (synopsis "Control center to configure Plasma Desktop")
    (description "This package provides configuration UI for Plasma Desktop.")
    (home-page "https://invent.kde.org/plasma/systemsettings")
    (license license:gpl2+)))
