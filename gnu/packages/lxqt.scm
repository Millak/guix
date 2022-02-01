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
    (version "0.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-build-tools/releases"
                           "/download/" version
                           "/lxqt-build-tools-" version ".tar.xz"))
       (sha256
        (base32 "1ff1pkrlxd8h0j8v49p6wrfhnqrz8s5b53hi835m41cvkzjljpfx"))))
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
    (version "3.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/libqtxdg/releases/download/"
             version "/libqtxdg-" version ".tar.xz"))
       (sha256
        (base32 "1kh4hv59bkjifq20ksh1mizf9mp7x30v6fpwccr45mi7hasqvvfi"))))
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
    (version "3.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/qtxdg-tools/releases/download/"
             version "/qtxdg-tools-" version ".tar.xz"))
       (sha256
        (base32 "0qn35v4dv71g0a4cqkbikppwmihxmfa560q9kw5pwk2y0xiwpncr"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/lxqt/" name "/releases/download/"
             version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1fickg1q54pcb8bv3x0ydg4xx02cqykibnjcq09as2kws6xbhk9n"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "03bqhbpdnfpan3l4snzzz6j0054m4r9zcgygcg21znslwicbqnw3"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1zah3xdnif9miaq52mmfbbzvqjhca7w7h81ngrn25j9pvd2bflm8"))))
    (build-system cmake-build-system)
    (inputs
     (list kwindowsystem
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0f0x82qma86kjdvn08qlg0ydxh9fnqikijfhnicynxdqfnp50ia5"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/lxqt-globalkeys/"
                           "releases/download/" version "/"
                           "lxqt-globalkeys-" version ".tar.xz"))
       (sha256
        (base32 "0bbw85aa59w0qnvkdggm3hbacps6yfcvcrs32d34mvvhc7d6g04l"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0bz3qdvv591zvpkxqzqqmh1yq5icc3iinmjr13qzws3ajlj19z44"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1hxix513z2sanmygfzq1fgx30kaxw5rjmmklbyyzl8bv1xzjcwk7"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "11dg18ac6kj8qkkrg940bzpykjih6nnw8y3hfww3wiyg6dka9gd7"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "150ggcfprascnwgsz721vnmay9cbar9annlhp6h2yzkl69iyc49r"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0zy6abbf3iwrxsr18gbxidb4m5spsigpa2778xg7y9r7fwgmqqkk"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1zw79lnm35gj3dyd4vlnk08n1lnr8391n36nbn81d0fgmvs21yx4"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1wfng8g28mq97ibrgpfbj353i15vdimmjp83pfqrmkddx0yvzcdv"))))
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
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0j8q5jfpb2l0vvji3xs8y0jcr792z6sxzj111qqvmdrbpxrkwxnw"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "064w40v43m91y9aywxxf2pj5rpcl4gbsgj7dv97pg4vhj9s790b8"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "18zrp2j0xpsrzy6m2dw8k55zczcc9jzavncasrp5j1dxscnzwrcr"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0m2fq1wh553yqi64a5nrdvm57fk3jnc3kxgaf0ja7h95jw6czvm5"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0pwl2j5kbs86vmq86phavq89bl2i82ic839bjk0v8kmxm9q2mrh9"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0nal8n7nmkafapdbcs9c8rk313md2fak4xjl9m56n10dxcjpi2wb"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "0y3ql25cmg1cmzjvadf7zcb58hh69gcslvr944sxxhaqp4daz10v"))))
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
    (version "2.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "16ybq07xpkl22mszakc1175xlqcayyj21i2h6wlxb8bmb7csg30n"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1m64c1m8dkb06fgfk09da2anjspphph6qdk41rqhds2qymh090v4"))))
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
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
       (sha256
        (base32 "1b9568y5xyxymk8r7pkz878ba24dyaxql2sfiy6blr8szf308c5l"))))
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
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/lxqt/screengrab/releases/download/"
                           version "/screengrab-" version ".tar.xz"))
       (sha256
        (base32 "14kh287d70v1lpd5w8pji88nmw3jd44q4h927vnszrkv6bwplzx7"))))
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
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/lxqt/" name "/releases/download/"
                           version "/" name "-" version ".tar.xz"))
        (sha256
          (base32 "1cxxr7rpflh2ki272pac927gzcw2w1lp3qz8vplflf148laigwc0"))))
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
    (version "17.0")
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
