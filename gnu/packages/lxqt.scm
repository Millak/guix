;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015, 2022 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Meiyo Peng <meiyo@riseup.net>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2020 Fakhri Sajadi <f.sajadi@pantherx.org>
;;; Copyright © 2020 André Batista <nandre@riseup.net>
;;; Copyright © 2021, 2022 Brendan Tildesley <mail@brendan.scot>
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
  #:use-module (gnu packages base)
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
    (version "0.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-build-tools/releases"
                           "/download/" version
                           "/lxqt-build-tools-" version ".tar.xz"))
       (sha256
        (base32 "0c4mm167hk0yihryi46d80ghxx2lwzkpivs4yj5wsfvdpbv5q1qh"))))
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
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/libqtxdg/releases/download/"
             version "/libqtxdg-" version ".tar.xz"))
       (sha256
        (base32 "18mz3mxcnz6awkkgxnfg5p43d4lv92vamxk1d0xzdxrp9symfd9v"))))
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
    (version "3.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/qtxdg-tools/releases/download/"
             version "/qtxdg-tools-" version ".tar.xz"))
       (sha256
        (base32 "0iyn0s2mck948vzlmq6hk4p93i9z59h50z6m2kdhzv9ck2axvlik"))))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "057cdxmz6kh3p1vpbwah4i99hdknh22w2w8wqfiws2d4n6wwiavz"))))
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
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0xah1qhzr5q20dj637c91bjrv9gy0z65d3jv6pjsp1kz5cwa8gsr"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1bjmarrra43gcnzi8i0g0lcam12hhgr2yi8dji6klmrcp4k67y89"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
           liblxqt
           libqtxdg
           polkit-qt
           qtsvg-5
           qtx11extras
           tzdata))
    (native-inputs
     (list lxqt-build-tools qttools-5))
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("lxqt-admin-user/CMakeLists.txt"
                            "lxqt-admin-time/CMakeLists.txt")
               (("DESTINATION \"\\$\\{POLKITQT-1_POLICY_FILES_INSTALL_DIR\\}")
                "DESTINATION \"share/polkit-1/actions"))
             (substitute* '("lxqt-admin-time/timeadmindialog.cpp")
               (("/usr/share/zoneinfo/zone.tab")
                (search-input-file inputs "share/zoneinfo/zone.tab"))))))))
    (home-page "https://lxqt-project.org")
    (synopsis "LXQt system administration tool")
    (description "lxqt-admin is providing two GUI tools to adjust settings of
the operating system LXQt is running on.")
    (license license:lgpl2.1+)))

(define-public lxqt-config
  (package
    (name "lxqt-config")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0h0n5an1pp3k50g4p2dxymy0lsnsh7m46gb22ndam69hdkflw71y"))))
    (build-system cmake-build-system)
    (inputs
     (list eudev
           kwindowsystem
           liblxqt
           libqtxdg
           libxcursor
           libxi
           qtbase-5
           qtsvg-5
           qtx11extras
           solid
           xf86-input-libinput
           xkeyboard-config
           zlib))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-globalkeys/"
                           "releases/download/" version "/"
                           "lxqt-globalkeys-" version ".tar.xz"))
       (sha256
        (base32 "1s49b8kly027f3amxcf2bx8id3jmmrl8365x7676bd8x2g5v3va2"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1qlvk12ldqwxskxy283h6yqhn8rp29vynqs1lhwn8byli792nb7y"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0w662issh8cagmza881wdrxbngn33w8shp0jvzna1f8pf9g9f7bj"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1604rb4yg1lgivvd76gaqb6dvq8bv8xy5f2vzj46prh0rbvhnf2b"))))
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
           solid
           xcb-util
           xcb-util-image
           xkeyboard-config))
    (native-inputs
     (list pkg-config lxqt-build-tools qttools-5))
    (propagated-inputs
     ;; Propagating KWINDOWSYSTEM so that the list of opened applications
     ;; shows up in lxqt-panel's taskbar plugin.
     (list kwindowsystem lxmenu-data))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "01wyqhrapim0pzkghr54g0z8drhlmcszwpb6p8qfqds90k1vq6gc"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1b0m3nllfx5fcwpkp73ac7fbw0jn86h90k5y3r674csm46zs3c05"))))
    (build-system cmake-build-system)
    (inputs
     (list kidletime
           kwindowsystem
           liblxqt
           libqtxdg
           lxqt-globalkeys
           qtbase-5
           qtsvg-5
           qtx11extras
           solid))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "06cdx4fffggk2wjfpi8zlbkiapmbqq2qsz25cngg4kqznq0kayb3"))))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0kighmlm9hqmw8h41fbmwqyy8qs06q2bbja6snw8j1a2yvz636qz"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0rjw3rw6kpaa3csrga005qg5bxmdxfgrnn1qngs2nrny35v97ckl"))
       (patches (search-patches "lxqt-session-procps-4.patch"))))
    (build-system cmake-build-system)
    (inputs
     (list eudev
           kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0lx889bpadgizgca37rwll5hlxnbpqpzxnpscyc35n72vyix8m7k"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1w566l7xcqscnx5k4f59c2cp4hb4bf2lvqgd9lvdv7dd43qb848z"))))
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
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "00r35gb4x6fnsv6z6digr3661cwykxn32xq23an1n044v38lry6x"))))
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
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0nz66b9mv6hqaxf5k3ijaf1694za5nv121y6jfq39db3a1qx7rm5"))))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0qz40vlps82nzz62w7d9ar43gmzvlk9wqzm8lwga90vip8gb2zkm"))))
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
           librsvg
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "19r3wgs18mc0m4l8znwhycf137xcd53119ribwb069g4j6hiyvz3"))))
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
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1i8d11yfcx556cm3jb3j49dmnhizx2zrb7g09xfqdwnqbak3ak9r"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "13781ljapvk8dy0xd31grx43fqvn62msyb8rlnsa2fv893zl7fj4"))))
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
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0px42vvmxf3lgd5wwxl260nm18a46943iiqhhmjb0l4xi5s0lhfr"))))
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
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/screengrab/releases/download/"
                           version "/screengrab-" version ".tar.xz"))
       (sha256
        (base32 "0jqzpx44x2hbl5yvlx7md9zg2qrnchh54lbbmk8nmgrsa2wps9rv"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem libqtxdg qtbase-5 qtsvg-5 qtx11extras))
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
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
        (sha256
          (base32 "0vd4klwmg4rr0z4zilv6djd3mfin73hdf6jpw3fsrn24yncg7kgq"))))
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
        (list kwindowsystem
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
