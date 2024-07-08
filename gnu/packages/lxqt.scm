;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2022 Sou Bunnbu <iyzsong@gmail.com>
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
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-build-tools/releases"
                           "/download/" version
                           "/lxqt-build-tools-" version ".tar.xz"))
       (sha256
        (base32 "1kjrxc1aj7yfn3v72lwryn58hkwsribsspm480qg4qbw1nfijg7x"))))
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
                                          "/share/cmake/lxqt-build-tools"
                                          "/modules/LXQtConfigVars.cmake")
                (((regexp-quote (string-append #$output "/"))) "")))))
      #:configure-flags
      #~(list "-DLXQT_ETC_XDG_DIR=etc/xdg")))
    (native-inputs
     (list pkg-config glib))
    (inputs
     (list qtbase-5))
    (propagated-inputs
     ;; Dependent projects require Perl via the CMake files.
     (list perl))
    (synopsis "LXQt Build tools")
    (description
     "Lxqt-build-tools is providing several tools needed to build LXQt
itself as well as other components maintained by the LXQt project.")
    (home-page "https://lxqt-project.org")
    (license license:lgpl2.1+)))

(define-public libqtxdg
  (package
    (name "libqtxdg")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/libqtxdg/releases/download/"
             version "/libqtxdg-" version ".tar.xz"))
       (sha256
        (base32 "0alphfvkwxaqfnckjxbrwjkj7wjl4yff7qxzmyjz67c8728lxbny"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags
       '("-DBUILD_TESTS=ON"
         "-DQTXDGX_ICONENGINEPLUGIN_INSTALL_PATH=lib/qt5/plugins/iconengines")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Run the tests offscreen.
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (propagated-inputs
     ;; required by Qt5XdgIconLoader.pc
     (list glib qtbase-5 qtsvg-5))
    (native-inputs
     (list lxqt-build-tools pkg-config))
    (home-page "https://github.com/lxqt/libqtxdg")
    (synopsis "Qt implementation of freedesktop.org xdg specifications")
    (description "Libqtxdg implements the freedesktop.org xdg specifications
in Qt.")
    (license license:lgpl2.1+)))

(define-public qtxdg-tools
  (package
    (name "qtxdg-tools")
    (version "3.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/qtxdg-tools/releases/download/"
             version "/qtxdg-tools-" version ".tar.xz"))
       (sha256
        (base32 "05bfff9hl1fjy59j89cbxkmzf8yswazb0yz9r01a8a1fw4sh7br9"))))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1vr43sd2dzs4gmiaidr1gpm64fl500k30rlxxq7yj9p3iwk5d3xp"))))
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
     (list kwindowsystem-5
           libqtxdg
           libxscrnsaver
           polkit-qt
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
    (home-page "https://lxqt-project.org")
    (synopsis "Core utility library for all LXQt components")
    (description "liblxqt provides the basic libraries shared by the
components of the LXQt desktop environment.")
    (license license:lgpl2.1+)))

(define-public libsysstat
  (package
    (name "libsysstat")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1ghkzgz3ypjii08f00g26pnmw0s5naf344p83dwnf3kfdlykiip6"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (inputs
     (list qtbase-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "08b922gyg4591b399pw0y9zl3gr2627dw20c33abqpl30iq1fmd9"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1p9959rvj5kh1svv2p9dsfnf008xvrhllcccrsxnk4s8vzjhdqjp"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           polkit-qt
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
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

(define-public lxqt-config
  (package
    (name "lxqt-config")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "13v9mrp6dswdw9vq39lfpd5cgz2bg74mk2kp1x7zinzqijsn17wj"))))
    (build-system cmake-build-system)
    (inputs
     (list eudev
           kwindowsystem-5
           liblxqt
           libqtxdg
           libxcursor
           libxi
           qtbase-5
           qtsvg-5
           qtx11extras
           solid-5
           xf86-input-libinput
           xkeyboard-config
           zlib))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
    ;; XXX: This is a workaround so libkscreen can find the backends as we
    ;; dont have a way specify them. We may want to  patch like Nix does.
    (propagated-inputs
     (list libkscreen-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-globalkeys/"
                           "releases/download/" version "/"
                           "lxqt-globalkeys-" version ".tar.xz"))
       (sha256
        (base32 "17km7yl5fqwhjy35w700s4rjxf9ann3vv6mw6l4r1cf7pvzmxhy7"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list pkg-config qttools-5 lxqt-build-tools))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "182mjvc17dvavk8vznhgnk6j1lv594bqv4796p2p1f4zdhq6fgm6"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
    (arguments '(#:tests? #f))          ; no test target
    (home-page "https://lxqt-project.org")
    (synopsis "The LXQt notification daemon")
    (description "lxqt-notificationd is LXQt's implementation of a daemon
according to the Desktop Notifications Specification.")
    (license license:lgpl2.1+)))

(define-public lxqt-openssh-askpass
  (package
    (name "lxqt-openssh-askpass")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "014jpyw4sgr63kjqdmksi6fsaz7pm5gkzr17f5rkaadx640ij4m0"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "GUI to query passwords on behalf of SSH agents")
    (description "lxqt-openssh-askpass is a GUI to query credentials on behalf
of other programs.")
    (license license:lgpl2.1+)))

(define-public lxqt-panel
  (package
    (name "lxqt-panel")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1lnqiw1rd5m9576rsg7lx8v95nc8krrj35fbi54ipc688na3j6s3"))))
    (build-system cmake-build-system)
    (inputs
     (list alsa-lib
           kguiaddons
           libdbusmenu-qt
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
           pcre
           pulseaudio
           qtbase-5
           qtsvg-5
           qtx11extras
           solid-5
           xcb-util
           xcb-util-image
           xkeyboard-config))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
    (propagated-inputs
     ;; Propagating KWINDOWSYSTEM so that the list of opened applications
     ;; shows up in lxqt-panel's taskbar plugin.
     (list kwindowsystem-5 lxmenu-data))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "051lb4kx29rx3ls3whlrbi83r9z9pdiqwyb1wbni56aqilv0svh0"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           pcre
           polkit-qt
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list pkg-config polkit lxqt-build-tools qttools-5))
    (arguments '(#:tests? #f))          ; no test target
    (home-page "https://lxqt-project.org")
    (synopsis "The LXQt PolicyKit agent")
    (description "lxqt-policykit is the polkit authentication agent of
LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-powermanagement
  (package
    (name "lxqt-powermanagement")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "17d1wh50pjjzqyxv3w7b4qlc1ym1p16yvbhyah9bzl2825irz9ar"))))
    (build-system cmake-build-system)
    (inputs
     (list kidletime-5
           kwindowsystem-5
           liblxqt
           libqtxdg
           lxqt-globalkeys
           qtbase-5
           qtsvg-5
           qtx11extras
           solid-5))
    (native-inputs
     (list lxqt-build-tools qttools-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0hdxa9cb39vklx616ywcc7jgipij99p4bd16w0f0cvidh6p1rqhv"))))
    (build-system cmake-build-system)
    (inputs
     (list libdbusmenu-qt
           libfm-qt
           libqtxdg
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* '("src/CMakeLists.txt")
               (("DESTINATION \"\\$\\{QT_PLUGINS_DIR\\}")
                "DESTINATION \"lib/qt5/plugins"))
             #t)))))
    (home-page "https://lxqt-project.org")
    (synopsis "LXQt Qt platform integration plugin")
    (description "lxqt-qtplugin is providing a library libqtlxqt to integrate
Qt with LXQt.")
    (license license:lgpl2.1+)))

(define-public lxqt-runner
  (package
    (name "lxqt-runner")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1916cl12v09x4iqrgvp2dwchy50wa97a0y68q4rdigarpdrp3x7j"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           lxqt-globalkeys
           muparser
           pcre
           qtbase-5
           qtsvg-5
           qtx11extras))
    (native-inputs
     (list pkg-config qttools-5 lxqt-build-tools))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "Tool used to launch programs quickly by typing their names")
    (description "lxqt-runner provides a GUI that comes up on the desktop and
allows for launching applications or shutting down the system.")
    (license license:lgpl2.1+)))

(define-public lxqt-session
  (package
    (name "lxqt-session")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0xa5nqiq9mxwfynnw91i4c2cgpmpapl4nxys084nbs7yd88kbm2l"))))
    (build-system cmake-build-system)
    (inputs
     (list eudev
           kwindowsystem-5
           liblxqt
           qtxdg-tools
           procps
           qtbase-5
           qtsvg-5
           qtx11extras
           xdg-user-dirs))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0pqz2pkzwq3af70s90p9x3a8rvlpl2jjb4mnjwgs1wz30cb7jrii"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           liblxqt
           libqtxdg
           qtbase-5
           qtsvg-5
           qtx11extras
           sudo))
    (native-inputs
     (list pkg-config qttools-5 lxqt-build-tools))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "013mqqjrqpjwm1a2zh97r7mgxyyic2lp1m07kcnbkmf1n9dyqzr1"))))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1rjrbjw6ixn5yw4r2187xfs7xd6v0j28p7fnjwjnv29lvvzgfm8x"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no tests
    (inputs
     (list glib
           libexif
           libfm
           libxcb
           menu-cache
           pcre
           qtbase-5
           qtx11extras))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
    (home-page "https://lxqt-project.org")
    (synopsis "Qt binding for libfm")
    (description "libfm-qt is the Qt port of libfm, a library providing
components to build desktop file managers which belongs to LXDE.")
    (license license:lgpl2.1+)))

(define-public pcmanfm-qt
  (package
    (name "pcmanfm-qt")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "190gfq6sp2z6hs7wy02xw831gdp2sxfrpy6jrhrf0zlfv26f6z3w"))))
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
     (list libfm-qt qtbase-5 qtx11extras lxqt-themes))
    (native-inputs
     (list pkg-config qttools-5 lxqt-build-tools))
    (home-page "https://lxqt-project.org")
    (synopsis "File manager and desktop icon manager")
    (description "PCManFM-Qt is the Qt port of PCManFM, the file manager of
LXDE.")
    (license license:gpl2+)))


;; Extra

(define-public compton-conf
  (package
    (name "compton-conf")
    (version "0.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0haarzhndjp0wndfhcdy6zl2whpdn3w0qzr3rr137kfqibc58lvx"))))
    (build-system cmake-build-system)
    (inputs
     (list libconfig qtbase-5))
    (native-inputs
     (list lxqt-build-tools pkg-config qttools-5))
    (arguments '(#:tests? #f))          ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "GUI configuration tool for compton X composite manager")
    (description "@code{compton-conf} is a configuration tool for X composite
manager Compton.")
    (license license:lgpl2.1+)))

(define-public lximage-qt
  (package
    (name "lximage-qt")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1zrlzx72zqcnqk1j0slwc2jsaxf71v5y1zhcwgv0n4z5548x3n38"))))
    (build-system cmake-build-system)
    (inputs
     (list libexif libfm-qt qtbase-5 qtsvg-5 qtx11extras))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "The image viewer and screenshot tool for lxqt")
    (description "LXImage-Qt is the Qt port of LXImage, a simple and fast
image viewer.")
    (license license:gpl2+)))

(define-public obconf-qt
  (package
    (name "obconf-qt")
    (version "0.16.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0q29f77dkwy005gzrmn2wj2ga1hdnfd2gwp05h72i2dj0qbdla3k"))))
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
           qtbase-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools pkg-config qttools-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0alxsz9q1lw3jc9qv18w9rc1ggib8x85mk3d7c17nbsvld5wfwmc"))))
    (build-system cmake-build-system)
    (inputs
     (list glib pcre pulseaudio qtbase-5 qtx11extras))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
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
    (version "2.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "126zkj9jvjwxrh2fcm9h0c2iq9m5rm5hbkh155swijn2i8airxgx"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5
           libxrender
           liblxqt
           libqtxdg
           qtbase-5
           qtx11extras))
    (native-inputs
     (list lxqt-build-tools qttools-5))
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
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0kl0lmd6np0lka60372sn8xd7l5h67hvy11x4gbf665p5fhsigkl"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5 utf8proc))
    (native-inputs
     (list lxqt-build-tools qttools-5))
    (arguments
     '(#:tests? #f))                    ; no tests
    (home-page "https://lxqt-project.org")
    (synopsis "The terminal widget for QTerminal")
    (description "QTermWidget is a terminal emulator widget for Qt 5.")
    (license license:gpl2+)))

(define-public qterminal
  (package
    (name "qterminal")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1ibhl51mggf81xhvcmrkbsxl7ls8v0sn74rwhjxw4vqk6r6fhiby"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5 qtx11extras qtermwidget))
    (native-inputs
     (list lxqt-build-tools qttools-5))
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
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/screengrab/releases/download/"
                           version "/screengrab-" version ".tar.xz"))
       (sha256
        (base32 "0xc004h7i2hnl3jj4p8v6wkqav2v07k1mzdvys3ya171z4ffmc9j"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem-5 libqtxdg qtbase-5 qtsvg-5 qtx11extras))
    (native-inputs
     (list pkg-config perl qttools-5))
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
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
        (sha256
          (base32 "096fjx86w413k8z8vhmk44k08d25dmagv6w4gc88hpqq4r81klr9"))))
    (build-system cmake-build-system)
    (inputs
      (list glib json-glib libfm-qt qtbase-5 qtx11extras))
    (native-inputs
      (list pkg-config lxqt-build-tools qttools-5))
    (arguments
      '(#:tests? #f))
    (home-page "https://lxqt-project.org")
    (synopsis "Simple & lightweight desktop-agnostic Qt file archiver")
    (description
     "This package provides a Qt graphical interface to archiving programs
like @command{tar} and @command{zip}.")
    (license license:gpl2+)))

(define-public lxqt-connman-applet
  ;; since the main developers didn't release any version yet,  their
  ;; latest commit on `master` branch at the moment used for this version.
  (let ((commit "db1618d58fd3439142c4e44b24cba0dbb68b7141")
        (revision "0"))
    (package
      (name "lxqt-connman-applet")
      (version (git-version "0.15.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
            (url (string-append "https://github.com/lxqt/" name))
            (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "087641idpg7n8yhh5biis4wv52ayw3rddirwqb34bf5fwj664pw9"))))
      (build-system cmake-build-system)
      (inputs
        (list kwindowsystem-5
              qtbase-5
              qtsvg-5
              liblxqt
              qtx11extras
              libqtxdg))
      (native-inputs
        `(("lxqt-build-tools" ,lxqt-build-tools)
          ("qtlinguist" ,qttools-5)))
      (arguments
        `(#:tests? #f                   ; no tests
          #:phases
            (modify-phases %standard-phases
              (add-after 'unpack 'remove-definitions
                (lambda _
                  (substitute* "CMakeLists.txt"
                    (("include\\(LXQtCompilerSettings NO_POLICY_SCOPE\\)")
                     "include(LXQtCompilerSettings NO_POLICY_SCOPE)
remove_definitions(-DQT_NO_CAST_TO_ASCII -DQT_NO_CAST_FROM_ASCII)"))
                  #t)))))
      (home-page "https://github.com/lxqt/lxqt-connman-applet")
      (synopsis "System-tray applet for connman")
      (description "This package provides a Qt-based system-tray applet for
the network management tool Connman, originally developed for the LXQT
desktop.")
      (license license:lgpl2.1+))))

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
           ;; TODO: qtsvg-5 is needed for lxqt apps to display icons. Maybe it
           ;; should be added to their propagated-inputs?
           qtsvg-5
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
    (synopsis "The Lightweight Qt Desktop Environment")
    (description "LXQt is a lightweight Qt desktop environment.")
    (home-page "https://lxqt-project.org")
    (license license:gpl2+)))
