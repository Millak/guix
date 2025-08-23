;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2022, 2025 宋文武 <iyzsong@envs.net>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2020 Fakhri Sajadi <f.sajadi@pantherx.org>
;;; Copyright © 2020 André Batista <nandre@riseup.net>
;;; Copyright © 2021, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Aaron Covrig <aaron.covrig.us@ieee.org>
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

(define-module (gnu packages lxqt)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages compton)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages lxde)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages openbox)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))


;; Third party libraries

(define-public libstatgrab
  (package
    (name "libstatgrab")
    (version "0.92.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://ftp.i-scream.org/pub/i-scream/libstatgrab/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "04bcbln3qlilxsyh5hrwdrv7x4pfv2lkwdwa98bxfismd15am22n"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-tests"
                           "--disable-static")))
    (native-inputs
     ;; For testing.
     (list perl))
    (home-page "https://www.i-scream.org/libstatgrab/")
    (synopsis "Provides access to statistics about the system")
    (description "libstatgrab is a library that provides cross platform access
to statistics about the system on which it's run.")
    ;; Libraries are under LGPL2.1+, and programs under GPLv2+.
    (license license:gpl2+)))


;; Base

(define-public lxqt-build-tools
  (package
    (name "lxqt-build-tools")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-build-tools/releases"
                           "/download/" version
                           "/lxqt-build-tools-" version ".tar.xz"))
       (sha256
        (base32 "0brkiq62cv5rp2knq3dbdzh4cv6l670x1bfxq1537k2mdcpdzp0k"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:modules `((ice-9 regex)
                  (guix build cmake-build-system)
                  (guix build utils))
      ;; In phases and configure-flags: Set LXQT_TRANSLATIONS_DIR,
      ;; LXQT_DATA_DIR, etc. to relative paths, so that packages using
      ;; LXQtConfigVars.cmake from lxqt-build-tools will install translations
      ;; and data files into their outputs, remove the need to patch their
      ;; cmake files.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'patch-LXQtConfigVars.cmake
            (lambda _
              (substitute* (string-append #$output
                                          "/share/cmake/lxqt2-build-tools"
                                          "/modules/LXQtConfigVars.cmake")
                (((regexp-quote (string-append #$output "/"))) "")))))
      #:configure-flags
      #~(list "-DLXQT_ETC_XDG_DIR=etc/xdg")))
    (inputs
     (list qtbase))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     ;; Required by lxqt2-transupdate and CMake files.
     (list perl qttools))
    (synopsis "LXQt Build tools")
    (description
     "Lxqt-build-tools is providing several tools needed to build LXQt
itself as well as other components maintained by the LXQt project.")
    (home-page "https://lxqt-project.org")
    (license license:lgpl2.1+)))

(define-public libqtxdg
  (package
    (name "libqtxdg")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/libqtxdg/releases/download/"
             version "/libqtxdg-" version ".tar.xz"))
       (sha256
        (base32 "0ap81y7sbqwcdfsdlyxihs12chfv332fgg6y0sl6zmjhhlhk1m55"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_TESTS=ON"
         "-DQTXDGX_ICONENGINEPLUGIN_INSTALL_PATH=lib/qt6/plugins/iconengines")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Run the tests offscreen.
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     ;; required by Qt6XdgIconLoader.pc
     (list qtbase qtsvg))
    (native-inputs
     (list lxqt-build-tools pkg-config))
    (inputs
     (list glib))
    (home-page "https://github.com/lxqt/libqtxdg")
    (synopsis "Qt implementation of freedesktop.org xdg specifications")
    (description "Libqtxdg implements the freedesktop.org xdg specifications
in Qt.")
    (license license:lgpl2.1+)))

(define-public qtxdg-tools
  (package
    (name "qtxdg-tools")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/qtxdg-tools/releases/download/"
             version "/qtxdg-tools-" version ".tar.xz"))
       (sha256
        (base32 "1bv3immy1idp7jc0csnjkiw8vvqkr8g84hnnvl6k8297g4vnwfjh"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (propagated-inputs (list libqtxdg))
    (native-inputs (list lxqt-build-tools))
    (home-page "https://github.com/lxqt/qtxdg-tools")
    (synopsis "User tools for libqtxdg")
    (description "This package contains a CLI MIME tool, @command{qtxdg-mat},
for handling file associations and opening files with their default
applications.")
    (license license:lgpl2.1+)))

(define-public liblxqt
  (package
    (name "liblxqt")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1j0918fyka0kxwsn3mgnjnfc8cvdp6dsgg0i40877ysry0dqp2aa"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             #t)))))
    (inputs
     (list libxscrnsaver polkit-qt6))
    (propagated-inputs
     ;; Required by lxqt-config.cmake
     (list kwindowsystem
           libqtxdg
           lxqt-build-tools
           qtbase
           qttools))
    (native-inputs
     (list lxqt-build-tools))
    (home-page "https://lxqt-project.org")
    (synopsis "Core utility library for all LXQt components")
    (description "liblxqt provides the basic libraries shared by the
components of the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public libsysstat
  (package
    (name "libsysstat")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0c6lr2z2n5dyyr6mawqxky8qwqlcjib6kdb0ls0lldmi8f65wvlr"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (inputs
     (list qtbase))
    (native-inputs
     (list lxqt-build-tools))
    (home-page "https://lxqt-project.org")
    (synopsis "Library used to query system info and statistics")
    (description "libsysstat is a library to query system information like CPU
and memory usage or network traffic.")
    (license license:lgpl2.1+)))


;; Core

(define-public lxqt-about
  (package
    (name "lxqt-about")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0vypdk0nf2rr7riv8xp7wy1x9694i73rgk0dj9x39gh53jm4xjy5"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt))
    (native-inputs
     (list lxqt-build-tools))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setenv
           (lambda _
             (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")
             #t)))))
    (home-page "https://lxqt-project.org")
    (synopsis "Provides information about LXQt and the system")
    (description "lxqt-about is a dialogue window providing information about
LXQt and the system it's running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-admin
  (package
    (name "lxqt-admin")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "03gkb7gwq0gy9ihvawn38i36nkfmwyslf8wmbp4ssj129i94sphq"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt polkit-qt6))
    (native-inputs
     (list lxqt-build-tools))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("lxqt-admin-user/CMakeLists.txt"
                            "lxqt-admin-time/CMakeLists.txt")
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             #t)))))
    (home-page "https://lxqt-project.org")
    (synopsis "LXQt system administration tool")
    (description "lxqt-admin is providing two GUI tools to adjust settings of
the operating system LXQt is running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-menu-data
  (package
    (name "lxqt-menu-data")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0nw9mr4p5ri3bk43xd871g3ni9irf6rrk8rz4d8ra9wd7bz2d855"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))                ;no tests
    (inputs
     (list qtbase))
    (native-inputs
     (list lxqt-build-tools))
    (home-page "https://lxqt-project.org")
    (synopsis "LXQt menu files")
    (description "This package provides freedesktop compliant menu files for
LXQt Panel, Configuration Center and PCManFM-Qt/libfm-qt.")
    (license license:lgpl2.1+)))

(define-public lxqt-config
  (package
    (name "lxqt-config")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "07ik1ycldpin21v4b29a95zb14q1s4bfbmlvyvw50r0mx0whnysj"))))
    (build-system cmake-build-system)
    (inputs
     (list eudev
           liblxqt
           libxcursor
           libxi
           lxqt-menu-data
           xf86-input-libinput
           xkeyboard-config
           zlib))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    ;; XXX: This is a workaround so libkscreen can find the backends as we
    ;; dont have a way specify them. We may want to  patch like Nix does.
    (propagated-inputs
     (list libkscreen))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-xkeyboard-config-file-name
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Set the file name to xkeyboard-config.
             (let ((xkb (assoc-ref inputs "xkeyboard-config")))
               (substitute* "lxqt-config-input/keyboardlayoutconfig.h"
                 (("/usr/share/X11/xkb/rules/base.lst")
                  (string-append xkb "/share/X11/xkb/rules/base.lst")))
               #t))))))
    (home-page "https://lxqt-project.org")
    (synopsis "Tools to configure LXQt and the underlying operating system")
    (description "lxqt-config is providing several tools involved in the
configuration of both LXQt and the underlying operating system.")
    (license license:lgpl2.1+)))

(define-public lxqt-globalkeys
  (package
    (name "lxqt-globalkeys")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-globalkeys/"
                           "releases/download/" version "/"
                           "lxqt-globalkeys-" version ".tar.xz"))
       (sha256
        (base32 "16lpz4wm6iz065drmgfgfjxkd0gc5g1wqisqcgs6iff0skpdzkv6"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Daemon used to register global keyboard shortcuts")
    (description "lxqt-globalkeys is providing tools to set global keyboard
shortcuts in LXQt sessions, that is shortcuts which apply to the LXQt session
as a whole and are not limited to distinct applications.")
    (license license:lgpl2.1+)))

(define-public lxqt-notificationd
  (package
    (name "lxqt-notificationd")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1i3rly1jk3kgzl0gscsygfqr8g6147r7031j41qgdrf2w5nby8s2"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem layer-shell-qt liblxqt libqtxdg))
    (native-inputs
     (list lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no test target
    (home-page "https://lxqt-project.org")
    (synopsis "The LXQt notification daemon")
    (description "lxqt-notificationd is LXQt's implementation of a daemon
according to the Desktop Notifications Specification.")
    (license license:lgpl2.1+)))

(define-public lxqt-openssh-askpass
  (package
    (name "lxqt-openssh-askpass")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0g3gkfgyk7jgghslabmhlqwzafsd9i8c0sppb5hb4qllg916cadv"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt))
    (native-inputs
     (list lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "GUI to query passwords on behalf of SSH agents")
    (description "lxqt-openssh-askpass is a GUI to query credentials on behalf
of other programs.")
    (license license:lgpl2.1+)))

(define-public libdbusmenu-lxqt
  (package
    (name "libdbusmenu-lxqt")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1y9z4iwpl5kqdn36y2d1dligsv84a79piw2b6jxb17sd796x385d"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; XXX: Tests requires a dbus session and some icons.
      #:tests? #f))
    (propagated-inputs
     (list qtbase))
    (home-page "https://github.com/lxqt/libdbusmenu-lxqt")
    (synopsis "Qt implementation of the DBusMenu spec")
    (description "This library provides a Qt implementation of the DBusMenu
protocol, forked from @code{libdbusmenu-qt}.  The DBusMenu protocol makes it
possible for applications to export and import their menus over DBus.")
    (license license:lgpl2.1+)))

(define-public lxqt-panel
  (package
    (name "lxqt-panel")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1rhm57nnwr0mhii2r40gmawllj4cza9qb25qykkn236jfgpbilgz"))))
    (build-system cmake-build-system)
    (inputs
     (list alsa-lib
           layer-shell-qt
           libdbusmenu-lxqt
           liblxqt
           libqtxdg
           libstatgrab
           libsysstat
           libxcomposite
           libxdamage
           libxkbcommon
           libxrender
           libxtst
           `(,lm-sensors "lib")
           lxqt-globalkeys
           pulseaudio
           qtwayland
           solid
           wayland
           xcb-util
           xcb-util-image
           xkeyboard-config))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (propagated-inputs
     ;; Propagating KWINDOWSYSTEM so that the list of opened applications
     ;; shows up in lxqt-panel's taskbar plugin.
     (list kwindowsystem lxqt-menu-data))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-xkeyboard-config-file-path
                (lambda* (#:key inputs #:allow-other-keys)
                  ;; Set the path to xkeyboard-config.
                  (let ((xkb (assoc-ref inputs "xkeyboard-config")))
                    (substitute* "plugin-kbindicator/src/x11/kbdlayout.cpp"
                      (("/usr/share/X11/xkb/rules/evdev.xml")
                       (string-append xkb "/share/X11/xkb/rules/evdev.xml")))
                  #t))))))
    (home-page "https://lxqt-project.org")
    (synopsis "The LXQt desktop panel")
    (description "lxqt-panel represents the taskbar of LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-policykit
  (package
    (name "lxqt-policykit")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0h0hi7aimjhc6rn4w8wz3kmvpkx7g6bf9i1fclan7j7ic80cf160"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt polkit polkit-qt6))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no test target
    (home-page "https://lxqt-project.org")
    (synopsis "The LXQt PolicyKit agent")
    (description "lxqt-policykit is the polkit authentication agent of
LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-powermanagement
  (package
    (name "lxqt-powermanagement")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0pyzd7pw3mpp0f5193a8fg1bvixwabrapnpqy2q2a707j664mqhj"))))
    (build-system cmake-build-system)
    (inputs
     (list kidletime
           liblxqt
           lxqt-globalkeys
           solid))
    (native-inputs
     (list lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Power management module for LXQt")
    (description "lxqt-powermanagement is providing tools to monitor power
management events and optionally trigger actions like e. g. shut down a system
when laptop batteries are low on power.")
    (license license:lgpl2.1+)))

(define-public lxqt-qtplugin
  (package
    (name "lxqt-qtplugin")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0giql40mnl100zhqcyxi1vxsfyvx5hvi9zibjh5krr6nwrwwflhb"))))
    (build-system cmake-build-system)
    (inputs
     (list libdbusmenu-lxqt
           libfm-qt
           libqtxdg))
    (native-inputs
     (list lxqt-build-tools))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("src/CMakeLists.txt")
               (("DESTINATION \"\\$\\{QT_PLUGINS_DIR\\}")
                "DESTINATION \"lib/qt6/plugins"))
             #t)))))
    (home-page "https://lxqt-project.org")
    (synopsis "LXQt Qt platform integration plugin")
    (description "lxqt-qtplugin is providing a library libqtlxqt to integrate
Qt with LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-runner
  (package
    (name "lxqt-runner")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0pmviizv7cxiqn2868bmbwgqrakmp4fv6a1wzbal0gndgc14yxmw"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
           layer-shell-qt
           liblxqt
           lxqt-globalkeys
           muparser
           menu-cache))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Tool used to launch programs quickly by typing their names")
    (description "lxqt-runner provides a GUI that comes up on the desktop and
allows for launching applications or shutting down the system.")
    (license license:lgpl2.1+)))

(define-public lxqt-session
  (package
    (name "lxqt-session")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "06f8kfy859ri2wbjpz7yx69jw0gfxm80f46729lcl2vd23a0qari"))))
    (build-system cmake-build-system)
    (inputs
     (list bash-minimal
           eudev
           kwindowsystem
           layer-shell-qt
           liblxqt
           qtxdg-tools
           procps
           xdg-user-dirs))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* '("xsession/lxqt.desktop.in")
                 (("Exec=startlxqt") (string-append "Exec=" out "/bin/startlxqt"))
                 (("TryExec=lxqt-session") (string-append "TryExec=" out "/bin/startlxqt"))))))

         (add-after 'unpack 'patch-openbox-permission
           (lambda _
             (substitute* "startlxqt.in"
               ;; Don't add 'etc/xdg' to XDG_CONFIG_DIRS, and 'share' to XDG_DATA_DIRS.
               (("! contains .*;") "false;")
               ;; Add write permission to lxqt-rc.xml file which is stored as
               ;; read-only in store.
               (("cp \"\\$LXQT_DEFAULT_OPENBOX_CONFIG\" \"\\$XDG_CONFIG_HOME/openbox\"")
                 (string-append "cp \"$LXQT_DEFAULT_OPENBOX_CONFIG\" \"$XDG_CONFIG_HOME/openbox\"\n"
                                "        # fix openbox permission issue\n"
                                "        chmod u+w  \"$XDG_CONFIG_HOME/openbox\"/*"))))))))
    (native-search-paths
     (list (search-path-specification
            ;; LXQt applications install their default config files into
            ;; 'share/lxqt' and search them from XDG_CONFIG_DIRS/lxqt.
            (variable "XDG_CONFIG_DIRS")
            (files '("share")))))
    (home-page "https://lxqt-project.org")
    (synopsis "Session manager for LXQt")
    (description "lxqt-session provides the standard session manager
for the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public lxqt-sudo
  (package
    (name "lxqt-sudo")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "17bk4bcvm919q7h63q2sdnzwwbqjpk98kb5ij14rqs9v2psbqfks"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt sudo))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "GUI frontend for sudo/su")
    (description "lxqt-sudo is a graphical front-end of commands sudo and su
respectively.  As such it enables regular users to launch applications with
permissions of other users including root.")
    (license license:lgpl2.1+)))

(define-public lxqt-themes
  (package
    (name "lxqt-themes")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1b0gpmw6capkccysnvjj20j8l2p7hjkkpby72n9y982kb8f11l6d"))))
    (build-system cmake-build-system)
    (native-inputs
     (list lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Themes, graphics and icons for LXQt")
    (description "This package comprises a number of graphic files and themes
for LXQt.")
    ;; The whole package is released under LGPL 2.1+, while the LXQt logo is
    ;; licensed under CC-BY-SA 3.0.
    (license license:lgpl2.1+)))


;; File Manager

(define-public libfm-qt
  (package
    (name "libfm-qt")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1mr93by010scy06kmpgmsvkabg7zn1f0mm9i7grm17mfv3hkx85z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
    (inputs
     (list libexif
           lxqt-menu-data))
    (propagated-inputs
     ;; Required by headers.
     (list glib qtbase libxcb menu-cache))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (home-page "https://lxqt-project.org")
    (synopsis "Qt binding for libfm")
    (description "libfm-qt is the Qt port of libfm, a library providing
components to build desktop file managers which belongs to LXDE.")
    (license license:lgpl2.1+)))

(define-public pcmanfm-qt
  (package
    (name "pcmanfm-qt")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0kvbw1slbcism42rqn09h1q3dirq44g8h3azg1vc86zs3mbqcd76"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-settings.conf.in
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((wallpaper (search-input-file inputs
                                "share/lxqt/wallpapers/waves-logo.png")))
               (substitute* "config/pcmanfm-qt/lxqt/settings.conf.in"
                 (("Wallpaper=.*")
                  (string-append "Wallpaper=" wallpaper "\n")))))))))
    (inputs
     (list layer-shell-qt libfm-qt lxqt-themes))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (home-page "https://lxqt-project.org")
    (synopsis "File manager and desktop icon manager")
    (description "PCManFM-Qt is the Qt port of PCManFM, the file manager of
LXDE.")
    (license license:gpl2+)))


;; Extra

(define-public lximage-qt
  (package
    (name "lximage-qt")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1z7nyx5w9hvrz9zfil6sbpm61h47iap85p1bvwjvg863bqq01xpv"))))
    (build-system cmake-build-system)
    (inputs
     (list libexif libfm-qt qtsvg))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "The image viewer and screenshot tool for lxqt")
    (description "LXImage-Qt is the Qt port of LXImage, a simple and fast
image viewer.")
    (license license:gpl2+)))

(define-public lxqt-wallet
  (package
    (name "lxqt-wallet")
    (version "4.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lxqt/lxqt_wallet")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0krs2x4ivx9n3q98v0q91f53q19rji3qhv5rl9xbhgylxralr135"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f)) ;No tests
    (inputs (list libsecret qtbase qttools kwallet))
    (home-page "https://github.com/lxqt/lxqt_wallet")
    (synopsis "Password storage for LXQt")
    (description
     "The lxqt_wallet project provides secure storage of information that
can be presented in key-values pairs, such as passwords associated to user
names.")
    (license license:bsd-2)))

(define-public obconf-qt
  (package
    (name "obconf-qt")
    (version "0.16.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "030n4jlmd79i3m1kk3jqm9s6f2fgwj78phnlvcjfmxqj15l86j03"))))
    (build-system cmake-build-system)
    (inputs
     (list imlib2
           libsm
           (librsvg-for-system)
           libxft
           libxml2
           openbox
           pango
           pcre
           qtbase))
    (native-inputs
     (list lxqt-build-tools pkg-config))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Openbox configuration tool")
    (description "ObConf-Qt is a Qt port of ObConf, a configuration editor for
window manager OpenBox.")
    (license license:gpl2+)))

(define-public pavucontrol-qt
  (package
    (name "pavucontrol-qt")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1gvb73xxyv7avqx9wk8lvcfisbfdxcr6rc8ncnv35cn09f5gqg3c"))))
    (build-system cmake-build-system)
    (inputs
     (list glib qtbase pulseaudio))
    (native-inputs
     (list pkg-config lxqt-build-tools))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Pulseaudio mixer in Qt")
    (description "@code{pavucontrol-qt} is the Qt port of volume control
@code{pavucontrol} of sound server @code{PulseAudio}.")
    (license license:gpl2+)))

(define-public qps
  (package
    (name "qps")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "05ncgfiqqs53k4wx62845krd7qi26im5pa2xq1kxh8wlng44gjjf"))))
    (build-system cmake-build-system)
    (inputs
     (list liblxqt))
    (native-inputs
     (list lxqt-build-tools))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Qt-based visual process status monitor")
    (description "@code{qps} is a monitor that displays the status of the
processes currently in existence, much like code{top} or code{ps}.")
    (license license:gpl2+)))

(define-public qtermwidget
  (package
    (name "qtermwidget")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0jwfpv9dwifkjgzy2fiffkvj0dd3aw4rf95fnnrhvqcdlg1v5v16"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase utf8proc))
    (native-inputs
     (list lxqt-build-tools))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "The terminal widget for QTerminal")
    (description "QTermWidget is a terminal emulator widget for Qt 5.")
    (license license:gpl2+)))

(define-public qterminal
  (package
    (name "qterminal")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0g11n43msp0dwl68nf5wdh6kwi48xqc43pl9bpg4wsdw8n37hpm6"))))
    (build-system cmake-build-system)
    (inputs
     (list layer-shell-qt qtbase qtermwidget))
    (native-inputs
     (list lxqt-build-tools))
    (arguments
     '(#:tests? #f))                      ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Lightweight Qt-based terminal emulator")
    (description "QTerminal is a lightweight Qt terminal emulator based on
QTermWidget.")
    (license license:gpl2+)))

(define-public screengrab
  (package
    (name "screengrab")
    (version "2.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/screengrab/releases/download/"
                           version "/screengrab-" version ".tar.xz"))
       (sha256
        (base32 "1c7nyz1sjk30qpm00jzz9vq54jm6dyqfajjiwsqlp5hvx9gfgg17"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem libqtxdg qtbase))
    (native-inputs
     (list pkg-config perl qttools))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Crossplatform tool for fast making screenshots")
    (description "ScreenGrab is a program for fast creating screenshots, and
easily publishing them on internet image hosting services.")
    (license license:gpl2+)))


(define-public lxqt-archiver
  (package
    (name "lxqt-archiver")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1ir6dc45qp4g6iv57nyn9z1bh06ih9cm8gmgw646370m1jvh06k9"))))
    (build-system cmake-build-system)
    (inputs
     (list glib json-glib libfm-qt qtbase))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools))
    (arguments
     '(#:tests? #f))
    (home-page "https://lxqt-project.org")
    (synopsis "Simple & lightweight desktop-agnostic Qt file archiver")
    (description
     "This package provides a Qt graphical interface to archiving programs
like @command{tar} and @command{zip}.")
    (license license:gpl2+)))

;; The LXQt Desktop Environment

(define-public lxqt
  (package
    (name "lxqt")
    (version (package-version liblxqt))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (begin (mkdir %output) #t)))
    (propagated-inputs
     (list ;; XDG
           desktop-file-utils
           hicolor-icon-theme
           xdg-user-dirs
           xdg-utils
           ;; Base
           ;; TODO: qtsvg is needed for lxqt apps to display icons. Maybe it
           ;; should be added to their propagated-inputs?
           qtsvg
           ;; Core
           lxqt-about
           lxqt-admin
           lxqt-config
           lxqt-globalkeys
           lxqt-notificationd
           lxqt-openssh-askpass
           lxqt-panel
           lxqt-policykit
           lxqt-powermanagement
           lxqt-qtplugin
           lxqt-runner
           lxqt-session
           lxqt-sudo
           lxqt-themes
           pcmanfm-qt
           ;; Extra
           picom
           font-dejavu
           lximage-qt
           obconf-qt
           openbox
           breeze-icons       ; default by <lxqt-session>/share/lxqt/lxqt.conf
           pavucontrol-qt
           qps
           qterminal))
    (native-search-paths
     (list (search-path-specification
            ;; For finding qtsvg, lxqt-qtplugin, etc.
            (variable "QT_PLUGIN_PATH")
            (files '("lib/qt6/plugins")))))
    (synopsis "The Lightweight Qt Desktop Environment")
    (description "LXQt is a lightweight Qt desktop environment.")
    (home-page "https://lxqt-project.org")
    (license license:gpl2+)))
