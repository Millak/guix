;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016, 2017, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Guy Fleury Iteriteka <hoonandon@gmail.com>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu packages mate)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages javascript)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public mate-common
  (package
    (name "mate-common")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-common")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l55jc35mfmla52kb75141i02qxysadz7ay3mf199zy8xi8vcnl6"))
       (patches (search-patches "mate-common-honor-aclocal.patch"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         itstool
                         libtool
                         which))
    (home-page "https://mate-desktop.org/")
    (synopsis "Common files for development of MATE packages")
    (description "Mate Common includes common files and macros used by
MATE applications.")
    (license license:gpl3+)))

(define-public mate-power-manager
  (package
    (name "mate-power-manager")
    (version "1.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-power-manager")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k96w55ids1gyc2fk2j3l6yh30ah171y824f7cm2vdq6v1bb12n4"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         itstool
                         libtool
                         yelp-tools
                         gettext-minimal
                         (list glib "bin") ;glib-gettextize
                         mate-common
                         polkit ;for ITS rules
                         which))
    (inputs (list gtk+
                  glib
                  dbus-glib
                  libgnome-keyring
                  cairo
                  dbus
                  libnotify
                  mate-desktop
                  mate-panel
                  libxrandr
                  libcanberra
                  libsecret
                  startup-notification
                  upower))
    (home-page "https://mate-desktop.org/")
    (synopsis "Power manager for MATE")
    (description
     "MATE Power Manager is a MATE session daemon that acts as a policy agent on
top of UPower.  It listens to system events and responds with user-configurable
actions.")
    (license license:gpl2+)))

(define-public mate-icon-theme
  (package
    (name "mate-icon-theme")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-icon-theme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08xcmhsk4m1pvpajb7sa7fm58fh7ln9s9p7jby84hll6qjqkxkgn"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         itstool
                         libtool
                         icon-naming-utils
                         mate-common
                         which))
    (home-page "https://mate-desktop.org/")
    (synopsis "The MATE desktop environment icon theme")
    (description
     "This package contains the default icon theme used by the MATE desktop.")
    (license license:lgpl3+)))

(define-public mate-icon-theme-faenza
  (package
    (name "mate-icon-theme-faenza")
    (version "1.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://github.com/mate-desktop-legacy-archive/mate-icon-theme-faenza")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jd2qjhn5sbs48kk4hk4zzzcxkf3hxqziqjzlw8mpqdlain3psdx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'autoconf
            (lambda _
              (setenv "SHELL"
                      (which "sh"))
              (setenv "CONFIG_SHELL"
                      (which "sh"))
              (invoke "sh" "autogen.sh"))))))
    (native-inputs
     ;; autoconf-wrapper is required due to the non-standard
     ;; 'autoconf phase.
     (list autoconf-wrapper
           automake
           intltool
           icon-naming-utils
           libtool
           mate-common
           pkg-config
           which))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE desktop environment icon theme faenza")
    (description
     "Icon theme using Faenza and Faience icon themes and some customized
icons for MATE.  Furthermore it includes some icons from Mint-X-F and
Faenza-Fresh icon packs.")
    (license license:gpl2+)))

(define-public mate-themes
  (package
    (name "mate-themes")
    (version "3.22.26")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-themes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0df3lyyz219z65kcrzmd3mk63nvbd2lnak9hj80iyfxmzxnml6xc"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         itstool
                         libtool
                         gdk-pixbuf ;gdk-pixbuf+svg isn't needed
                         gtk+-2
                         mate-common
                         which))
    (home-page "https://mate-desktop.org/")
    (synopsis "Official themes for the MATE desktop")
    (description
     "This package includes the standard themes for the MATE desktop, for
example Menta, TraditionalOk, GreenLaguna or BlackMate.  This package has
themes for both gtk+-2 and gtk+-3.")
    (license (list license:lgpl2.1+ license:cc-by-sa3.0 license:gpl3+
                   license:gpl2+))))

(define-public mate-desktop
  (package
    (name "mate-desktop")
    (version "1.28.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-desktop")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wjl756wzm200qr6f6nx1dpxh2rv8xzvx89wdb14ajcf3zgmal6k"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         `(,glib "bin")
                         gobject-introspection
                         yelp-tools
                         gtk-doc/stable
                         mate-common
                         which))
    (inputs (list gtk+ libxrandr iso-codes/pinned startup-notification))
    (propagated-inputs (list dconf)) ;mate-desktop-2.0.pc
    (home-page "https://mate-desktop.org/")
    (synopsis "Library with common API for various MATE modules")
    (description
     "This package contains a public API shared by several applications on the
desktop and the mate-about program.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.1+))))

(define-public libmateweather
  (package
    (name "libmateweather")
    (version "1.28.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/libmateweather")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15ajz83na76lcnw9cy1m36f9xfzl1nywk9xwwjax1cklfz63vl0g"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--with-zoneinfo-dir=/var/empty")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'fix-tzdata-location
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TZDIR"
                      (search-input-directory inputs "/share/zoneinfo"))
              (substitute* "data/check-timezones.sh"
                (("/usr/share/zoneinfo/zone.tab")
                 (search-input-file inputs "/share/zoneinfo/zone.tab"))
                ;; XXX: Ignore this test for now, which requires tzdata-2023c.
                (("exit 1")
                 "exit 0")))))))
    (native-inputs
     (list autoconf
           autoconf-archive
           automake
           dconf
           (list glib "bin")
           intltool
           gtk-doc/stable
           libtool
           mate-common
           which
           pkg-config))
    (inputs
     (list gtk+
           tzdata-for-tests))
    (propagated-inputs
     ;; both of these are requires.private in mateweather.pc
     (list libsoup-minimal-2
           libxml2))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE library for weather information from the Internet")
    (description
     "This library provides access to weather information from the internet
for the MATE desktop environment.")
    (license license:lgpl2.1+)))

(define-public mate-terminal
  (package
    (name "mate-terminal")
    (version "1.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-terminal")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07336r3pdk6bnkj20071v5v5l0psq36lhmxbklfaw999vasl6w8f"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         itstool
                         gobject-introspection
                         libxml2
                         yelp-tools
                         mate-common
                         which))
    (inputs (list dconf
                  gtk+
                  libice
                  libsm
                  libx11
                  mate-desktop
                  pango
                  vte/gtk+-3))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE Terminal Emulator")
    (description
     "MATE Terminal is a terminal emulation application that you can
use to access a shell.  With it, you can run any application that
is designed to run on VT102, VT220, and xterm terminals.
MATE Terminal also has the ability to use multiple terminals
in a single window (tabs) and supports management of different
configurations (profiles).")
    (license license:gpl3)))

(define-public mate-session-manager
  (package
    (name "mate-session-manager")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-session-manager")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sx7p1z37ynz136rdxjs5yf95p2jy7mkv0v40azfk71kvl34mbnk"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--with-elogind" "--disable-schemas-compile")
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'update-xsession-dot-desktop
                    (lambda* (#:key outputs #:allow-other-keys)
                      ;; Record the absolute file name of 'mate-session' in the
                      ;; '.desktop' file.
                      (let* ((out (assoc-ref outputs "out"))
                             (xsession (string-append out
                                        "/share/xsessions/mate.desktop")))
                        (substitute* xsession
                          (("^Exec=.*$")
                           (string-append "Exec=" out "/bin/mate-session\n"))
                          (("^TryExec=.*$")
                           (string-append "Exec=" out "/bin/mate-session\n")))
                        #t))))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         libxcomposite
                         xtrans
                         gobject-introspection
                         mate-common
                         libxslt
                         docbook-xsl
                         which))
    (inputs (list gtk+ dbus-glib elogind libsm mate-desktop))
    (home-page "https://mate-desktop.org/")
    (synopsis "Session manager for MATE")
    (description
     "Mate-session contains the MATE session manager, as well as a
configuration program to choose applications starting on login.")
    (license license:gpl2)))

(define-public mate-settings-daemon
  (package
    (name "mate-settings-daemon")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-settings-daemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wc26b7c0vwq0k2mk8fyw2c7n7n0mky4n6dsjf8m4qmk5avl59vr"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         gobject-introspection
                         mate-common
                         which))
    (inputs (list cairo
                  dbus
                  dbus-glib
                  dconf
                  fontconfig
                  gtk+
                  libcanberra
                  libmatekbd
                  libmatemixer
                  libnotify
                  libx11
                  libxext
                  libxi
                  libxklavier
                  mate-desktop
                  nss
                  polkit
                  startup-notification))
    (home-page "https://mate-desktop.org/")
    (synopsis "Settings Daemon for MATE")
    (description "Mate-settings-daemon is a fork of gnome-settings-daemon.")
    (license (list license:lgpl2.1 license:gpl2))))

(define-public libmatemixer
  (package
    (name "libmatemixer")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/libmatemixer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d3q9d8nyj5483h6mg7iv6hs2zpp5wwzvdib7klfbmqf6hz21hc2"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         gobject-introspection
                         gtk-doc/stable
                         mate-common
                         which))
    (inputs (list glib pulseaudio alsa-lib))
    (home-page "https://mate-desktop.org/")
    (synopsis "Mixer library for the MATE desktop")
    (description
     "Libmatemixer is a mixer library for MATE desktop.  It provides an abstract
API allowing access to mixer functionality available in the PulseAudio and ALSA
sound systems.")
    (license license:lgpl2.1)))

(define-public libmatekbd
  (package
    (name "libmatekbd")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/libmatekbd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qhs7k6n263bzkqxfhx5284wwp425gpvhlmkqn3n61fvwn50kkza"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         gobject-introspection
                         mate-common
                         which))
    (inputs (list cairo
                  (librsvg-for-system)
                  glib
                  gtk+
                  libx11
                  libxklavier))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE keyboard configuration library")
    (description "Libmatekbd is a keyboard configuration library for the
MATE desktop environment.")
    (license license:lgpl2.1)))

(define-public mate-menus
  (package
    (name "mate-menus")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-menus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nfmgjwggs31c79r9223cr5w1gfqrln36qqvhfjdfsbfb421m3fz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'bootstrap 'fix-introspection-install-dir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* '("configure")
                          (("`\\$PKG_CONFIG --variable=girdir gobject-introspection-1.0`")
                           (string-append "\"" out "/share/gir-1.0/\""))
                          (("\\$\\(\\$PKG_CONFIG --variable=typelibdir gobject-introspection-1.0\\)")
                           (string-append out "/lib/girepository-1.0/"))) #t))))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         gobject-introspection
                         mate-common
                         which))
    (inputs (list glib))
    (home-page "https://mate-desktop.org/")
    (synopsis "Freedesktop menu specification implementation for MATE")
    (description
     "The package contains an implementation of the freedesktop menu
specification, the MATE menu layout configuration files, .directory files and
assorted menu related utility programs.")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public mate-applets
  (package
    (name "mate-applets")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-applets")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w690z66i63fsl6ysi6jbwk30xal33ykx1x975wiq7gk391qv03v"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         libxslt
                         yelp-tools
                         gettext-minimal
                         docbook-xml
                         gobject-introspection
                         mate-common
                         which))
    (inputs (list at-spi2-core
                  dbus
                  dbus-glib
                  glib
                  gucharmap
                  gtk+
                  gtksourceview-4
                  libgtop
                  libmateweather
                  libnl
                  libnotify
                  libx11
                  libxml2
                  libwnck
                  mate-desktop
                  mate-panel
                  pango
                  polkit ;either polkit or setuid
                  upower
                  wireless-tools))
    (propagated-inputs (list python-pygobject))
    (home-page "https://mate-desktop.org/")
    (synopsis "Various applets for the MATE Panel")
    (description
     "Mate-applets includes various small applications for Mate-panel:

@enumerate
@item accessx-status: indicates keyboard accessibility settings,
including the current state of the keyboard, if those features are in use.
@item Battstat: monitors the power subsystem on a laptop.
@item Character palette: provides a convenient way to access
non-standard characters, such as accented characters,
mathematical symbols, special symbols, and punctuation marks.
@item MATE CPUFreq Applet: CPU frequency scaling monitor
@item Drivemount: lets you mount and unmount drives and file systems.
@item Geyes: pair of eyes which follow the mouse pointer around the screen.
@item Keyboard layout switcher: lets you assign different keyboard
layouts for different locales.
@item Modem Monitor: monitors the modem.
@item Invest: downloads current stock quotes from the Internet and
displays the quotes in a scrolling display in the applet. The
applet downloads the stock information from Yahoo! Finance.
@item System monitor: CPU, memory, network, swap file and resource.
@item Trash: lets you drag items to the trash folder.
@item Weather report: downloads weather information from the
U.S National Weather Service (NWS) servers, including the
Interactive Weather Information Network (IWIN).
@end enumerate
")
    (license (list license:gpl2+ license:lgpl2.0+ license:gpl3+))))

(define-public mate-indicator-applet
  (package
    (name "mate-indicator-applet")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-indicator-applet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sm9f1xxggal755qcjls5pw5sv89kmvdqmdn2a81jdq275aff970"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         intltool
                         libtool
                         mate-common
                         which))
    (inputs (list gtk+ libindicator mate-common mate-panel hicolor-icon-theme))
    (home-page "https://mate-desktop.org/")
    (synopsis
     "Applet for displaying application indicators on the MATE panel")
    (description "This applet displays information from various applications
consistently in the MATE panel.")
    (license
     ;; Dual-licensed under GPL-3+ and LGPL-2.1+
     (list
      license:gpl3+
      license:lgpl2.1+))))

(define-public mate-sensors-applet
  (package
    (name "mate-sensors-applet")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-sensors-applet")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lbcnw4ykm2x96700l7gm748hyhj50wpgr1gvyx02f9vnrq27a23"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-in-process")))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         itstool
                         yelp-tools
                         gettext-minimal
                         gobject-introspection
                         libtool
                         mate-common
                         which))
    (inputs (list at-spi2-core
                  dbus
                  dbus-glib
                  glib
                  gtk+
                  libnotify
                  lm-sensors
                  libx11
                  libxml2
                  libxslt
                  libatasmart
                  libwnck
                  mate-desktop
                  mate-menus
                  mate-panel
                  pango
                  polkit ;either polkit or setuid
                  upower
                  wireless-tools))
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE panel applet for hardware monitoring")
    (description
     "MATE Sensors Applet displays readings from hardware sensors in the MATE
panel, these include CPU temperature, fan speeds and voltage reading under
GNU plus Linux distributions.")
    (license license:gpl2+)))

(define-public mate-media
  (package
    (name "mate-media")
    (version "1.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-media")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16wklpb8ggxgygvckaznk0j8waa4xv6mgqaplr6csyr3r002q99z"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list pkg-config intltool gettext-minimal gobject-introspection
           autoconf
           autoconf-archive
           automake
           libtool
           mate-common
           which))
    (inputs
     (list cairo
           gtk+
           libcanberra
           libmatemixer
           libxml2
           mate-applets
           mate-desktop
           mate-panel
           pango
           startup-notification))
    (home-page "https://mate-desktop.org/")
    (synopsis "Multimedia related programs for the MATE desktop")
    (description
     "Mate-media includes the MATE media tools for MATE, including
mate-volume-control, a MATE volume control application and applet.")
    (license (list license:gpl2+ license:lgpl2.0+ license:fdl1.1+))))

(define-public mate-notification-daemon
  (package
    (name "mate-notification-daemon")
    (version "1.28.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-notification-daemon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p1wssgyqyd9b75lgkz3gn3jycwf6wra8cyficgs2v1i18z4vs7f"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         intltool
                         libtool
                         libxml2
                         mate-common
                         which))
    (inputs (list gtk+
                  dbus-glib
                  libwnck
                  libnotify
                  libcanberra
                  mate-desktop
                  mate-panel
                  hicolor-icon-theme))
    (home-page "https://mate-desktop.org/")
    (synopsis "Notification daemon for MATE")
    (description
     "This MATE Desktop component is meant to run on the background and
deliver notifications to the user.")
    (license license:gpl2+)))

(define-public mate-panel
  (package
    (name "mate-panel")
    (version "1.28.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-panel")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ldrvaskpj93qsiismbz2hhbzs1md9nyck9cvhlzpswbzd6qpl8r"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-zoneinfo-dir="
                                              (assoc-ref %build-inputs
                                                         "tzdata")
                                              "/share/zoneinfo")
                               "--with-in-process-applets=all")
       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'fix-timezone-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((tzdata (assoc-ref inputs "tzdata")))
                        (substitute* "applets/clock/system-timezone.h"
                          (("/usr/share/lib/zoneinfo/tab")
                           (string-append tzdata "/share/zoneinfo/zone.tab"))
                          (("/usr/share/zoneinfo")
                           (string-append tzdata "/share/zoneinfo")))) #t))
                  (add-after 'bootstrap 'fix-introspection-install-dir
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* '("configure")
                          (("`\\$PKG_CONFIG --variable=girdir gobject-introspection-1.0`")
                           (string-append "\"" out "/share/gir-1.0/\""))
                          (("\\$\\(\\$PKG_CONFIG --variable=typelibdir gobject-introspection-1.0\\)")
                           (string-append out "/lib/girepository-1.0/"))) #t))))))
    (native-inputs (list pkg-config
                         intltool
                         itstool
                         xtrans
                         yelp-tools
                         gobject-introspection
                         gtk-doc/stable
                         autoconf
                         autoconf-archive
                         automake
                         libtool
                         mate-common
                         which))
    (inputs (list dconf
                  dconf-editor
                  cairo
                  dbus-glib
                  gtk-layer-shell
                  gtk+
                  libcanberra
                  libice
                  libmateweather
                  (librsvg-for-system)
                  libsm
                  libx11
                  libxau
                  libxml2
                  libxrandr
                  libwnck
                  mate-desktop
                  mate-menus
                  pango
                  tzdata
                  wayland))
    (native-search-paths
     (list (search-path-specification
            (variable "MATE_PANEL_APPLETS_DIR")
            (files '("share/mate-panel/applets")))
           (search-path-specification
            (variable "MATE_PANEL_EXTRA_MODULES")
            (files '("lib/mate-panel/modules")))))
    (home-page "https://mate-desktop.org/")
    (synopsis "Panel for MATE")
    (description
     "Mate-panel contains the MATE panel, the libmate-panel-applet library and
several applets.  The applets supplied here include the Workspace Switcher,
the Window List, the Window Selector, the Notification Area, the Clock and the
infamous 'Wanda the Fish'.")
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public atril
  (package
    (name "atril")
    (version "1.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/atril")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d90llj94rmanv40fgkqhdrvp41sfm5ajxz9scrnn8z5azd0r572"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-introspection" "--disable-schemas-compile"
              ;; FIXME: Enable build of Caja extensions.
              "--disable-caja"
              (string-append "--with-openjpeg="
                             #$(this-package-input "openjpeg")))
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-mathjax-path
            (lambda _
              (let* ((mathjax (assoc-ref %build-inputs "js-mathjax"))
                     (mathjax-path (string-append mathjax
                                                  "/share/javascript/mathjax")))
                (substitute* "backend/epub/epub-document.c"
                  (("/usr/share/javascript/mathjax")
                   mathjax-path))) #t))
          (add-after 'bootstrap 'fix-introspection-install-dir
            (lambda _
              (substitute* '("configure")
                (("\\$\\(\\$PKG_CONFIG --variable=girdir gobject-introspection-1.0\\)")
                 (string-append "\""
                                #$output "/share/gir-1.0/\""))
                (("\\$\\(\\$PKG_CONFIG --variable=typelibdir gobject-introspection-1.0\\)")
                 (string-append #$output "/lib/girepository-1.0/")))))
          (add-before 'install 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "data/Makefile"
                (("gtk-update-icon-cache")
                 "true")) #t))
          (add-after 'patch-dot-desktop-files 'patch-dot-thumbnailer-files
            (lambda _
              (define abs-path
                (string-append #$output "/bin/atril-thumbnailer"))
              (substitute* (string-append #$output
                            "/share/thumbnailers/atril.thumbnailer")
                (("TryExec=atril-thumbnailer")
                 (format #f "TryExec=~a" abs-path))
                (("Exec=atril-thumbnailer")
                 (format #f "Exec=~a" abs-path))))))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         itstool
                         yelp-tools
                         (list glib "bin")
                         gobject-introspection
                         gtk-doc/stable
                         texlive-bin ;synctex
                         libxml2
                         libtool
                         mate-common
                         which
                         zlib))
    (inputs (list at-spi2-core
                  cairo
                  caja
                  dconf
                  dbus
                  dbus-glib
                  djvulibre
                  fontconfig
                  freetype
                  ghostscript
                  glib
                  gtk+
                  js-mathjax
                  libcanberra
                  libsecret
                  libspectre
                  libtiff
                  libx11
                  libice
                  libsm
                  libgxps
                  libjpeg-turbo
                  libxml2
                  mate-desktop
                  python-dogtail
                  shared-mime-info
                  gdk-pixbuf
                  gsettings-desktop-schemas
                  libgnome-keyring
                  libarchive
                  marco
                  openjpeg
                  pango
                  ;; texlive
                  ;; TODO:
                  ;; Build libkpathsea as a shared library for DVI support.
                  ;; ("libkpathsea" ,texlive-bin)
                  poppler
                  startup-notification
                  webkitgtk-for-gtk3))
    (home-page "https://mate-desktop.org")
    (synopsis "Document viewer for Mate")
    (description
     "Atril is a simple multi-page document viewer.  It can display and print
@acronym{PostScript, PS}, @acronym{Encapsulated PostScript EPS}, DJVU, DVI, XPS
and @acronym{Portable Document Format PDF} files.  When supported by the
document, it also allows searching for text, copying text to the clipboard,
hypertext navigation, and table-of-contents bookmarks.")
    (license license:gpl2)))

(define-public caja
  (package
    (name "caja")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/caja")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z1ihbbg88kl4fwxix2grq6v37qiq8bvvywlkfmj3sgxh7w0hvda"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #f ;tests fail even with display set
      #:configure-flags
      #~(list "--disable-update-mimedb")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")
              ;; For the missing /etc/machine-id.
              (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         (list glib "bin")
                         xorg-server-for-tests
                         gobject-introspection
                         gtk-doc/stable
                         libtool
                         mate-common
                         which))
    (inputs (list exempi
                  gtk+
                  gvfs
                  libexif
                  libnotify
                  libsm
                  libxml2
                  mate-desktop
                  startup-notification))
    (native-search-paths
     (list (search-path-specification
            (variable "CAJA_EXTENSION_DIRS")
            (files (list "lib/caja/extensions-2.0")))))
    (home-page "https://mate-desktop.org/")
    (synopsis "File manager for the MATE desktop")
    (description
     "Caja is the official file manager for the MATE desktop.
It allows for browsing directories, as well as previewing files and launching
applications associated with them.  Caja is also responsible for handling the
icons on the MATE desktop.  It works on local and remote file systems.")
    ;; There is a note about a TRADEMARKS_NOTICE file in COPYING which
    ;; does not exist. It is safe to assume that this is of no concern
    ;; for us.
    (license license:gpl2+)))


(define-public caja-actions
  (package
    (name "caja-actions")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/caja-actions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a21kz5796prdq88a3yjc8jnd6qv8jg5zji43m057ra46qjbjazf"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-caja-extdir="
                             #$output "/lib/caja/extensions-2.0/"
                             "--disable-static"
                             "--enable-html-manuals"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preconfigure
            (lambda _
              ;; Danish translations cause a segmentation
              ;; fault at compile time. We are removing them
              ;; for now.
              (delete-file-recursively "docs/help/da"))))))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         intltool
                         libice
                         libxml2
                         libtool
                         gobject-introspection
                         gtk-doc/stable
                         mate-common
                         pkg-config
                         yelp-tools
                         which))
    (inputs (list caja
                  dbus
                  dbus-glib
                  gtk+
                  (list glib "bin")
                  libgtop
                  libsm
                  mate-desktop))
    (home-page "https://mate-desktop.org/")
    (synopsis "Execute commands from the caja popup menu")
    (description
     "This package is an extension for the MATE caja file manager
it allows users to add arbitrary programs and launch them through the popup
menu of selected files.")
    (license license:gpl2+)))

(define-public caja-extensions
  (package
    (name "caja-extensions")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/caja-extensions")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gjhbf0gds4ljc2d7z4x518jvvwwifvnbbxamvla2jil8xm830hq"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list "--enable-sendto"
                               ;; TODO: package "gupnp" to enable 'upnp', package
                               ;; "gksu" to enable 'gksu'.
                               (string-append
                                "--with-sendto-plugins=removable-devices,"
                                "caja-burn,emailclient,pidgin,gajim")
                               "--enable-image-converter"
                               "--enable-open-terminal"
                               "--enable-share"
                               "--enable-wallpaper"
                               "--enable-xattr-tags"
                               "--enable-av=yes"

                               (string-append "--with-cajadir="
                                              (assoc-ref %outputs "out")
                                              "/lib/caja/extensions-2.0/"))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         (list glib "bin")
                         gobject-introspection
                         gtk-doc/stable
                         intltool
                         libtool
                         libxml2
                         mate-common
                         which))
    (inputs (list attr
                  brasero
                  caja
                  dbus
                  dbus-glib
                  gajim ;runtime only?
                  gst-plugins-base
                  gtk+
                  graphicsmagick
                  mate-desktop
                  pidgin ;runtime only?
                  startup-notification))
    (home-page "https://mate-desktop.org/")
    (synopsis "Extensions for the File manager Caja")
    (description
     "Caja is the official file manager for the MATE desktop.
It allows for browsing directories, as well as previewing files and launching
applications associated with them.  Caja is also responsible for handling the
icons on the MATE desktop.  It works on local and remote file systems.")
    (license license:gpl2+)))

(define-public python-caja
  (package
    (name "python-caja")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/python-caja")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0av6fxvhardsx93hqf2kap0my57g2hip2g0ls4pp3ymmscib8gw2"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-cajadir="
                             #$output "/lib/caja/extensions-2.0/"))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         mate-common
                         gtk-doc/stable
                         which
                         gettext-minimal
                         python-wrapper))
    (inputs (list caja gtk+ python-pygobject))
    (home-page "https://mate-desktop.org/")
    (synopsis "Python bindings for Caja components")
    (description
     "This package provides Python bindings to Caja, a file manager for the
MATE desktop.")
    (license license:gpl2+)))

(define-public mate-control-center
  (package
    (name "mate-control-center")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-control-center")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dcg2slg1kbdcpy6q7gd2brr02vz548wbpd72kgnp8n1h5mdwm1r"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'use-elogind-as-systemd
            (lambda _
              (substitute* "configure"
                (("systemd")
                 "libelogind"))))
          (add-before 'build 'fix-polkit-action
            (lambda _
              ;; Make sure the polkit file refers to the right
              ;; executable.
              (substitute* "capplets/display/org.mate.randr.policy.in"
                (("/usr/sbin")
                 (string-append #$output "/sbin"))))))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         mate-common
                         which
                         yelp-tools
                         desktop-file-utils
                         xorgproto
                         xmodmap
                         gobject-introspection))
    (inputs (list at-spi2-core
                  cairo
                  caja
                  dconf
                  dbus
                  dbus-glib
                  elogind
                  fontconfig
                  freetype
                  glib
                  gsettings-desktop-schemas
                  gtk+
                  libappindicator
                  libcanberra
                  libgtop
                  libmatekbd
                  libx11
                  libxcursor
                  libxext
                  libxi
                  libxklavier
                  libxml2
                  libxrandr
                  libxrender
                  libxscrnsaver
                  marco
                  mate-desktop
                  mate-menus
                  mate-settings-daemon
                  pango
                  polkit
                  startup-notification
                  udisks))
    (propagated-inputs (list (librsvg-for-system))) ;mate-slab.pc
    (home-page "https://mate-desktop.org/")
    (synopsis "MATE Desktop configuration tool")
    (description
     "MATE control center is MATE's main interface for configuration
of various aspects of your desktop.")
    (license license:gpl2+)))

(define-public marco
  (package
    (name "marco")
    (version "1.28.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/marco")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1clzznp9q6kriy9hcm0pkikk0jckm9v18hivk0pjxiwgv7in93lk"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         mate-common
                         which
                         itstool
                         yelp-tools
                         glib
                         gobject-introspection
                         libxft
                         libxml2
                         zenity))
    (inputs (list gtk+
                  libcanberra
                  libgtop
                  libice
                  libsm
                  libx11
                  libxcomposite
                  libxcursor
                  libxdamage
                  libxext
                  libxfixes
                  libxinerama
                  libxrandr
                  libxrender
                  libxres
                  mate-desktop
                  pango
                  startup-notification))
    (home-page "https://mate-desktop.org/")
    (synopsis "Window manager for the MATE desktop")
    (description
     "Marco is a minimal X window manager that uses GTK+ for drawing
window frames.  It is aimed at non-technical users and is designed to integrate
well with the MATE desktop.  It lacks some features that may be expected by
some users; these users may want to investigate other available window managers
for use with MATE or as a standalone window manager.")
    (license license:gpl2+)))

(define-public mate-user-guide
  (package
    (name "mate-user-guide")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-user-guide")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qw2af4fxcfrdxvdfy5pap5ldjcir7hhkxghqg0gav6sg1r5slni"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'adjust-desktop-file
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((yelp (assoc-ref inputs "yelp")))
                        (substitute* "mate-user-guide.desktop.in.in"
                          (("yelp")
                           (string-append yelp "/bin/yelp")))) #t)))))
    (native-inputs (list pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         libtool
                         mate-common
                         which
                         intltool
                         itstool
                         gettext-minimal
                         yelp-tools
                         yelp-xsl))
    (inputs (list yelp))
    (home-page "https://mate-desktop.org/")
    (synopsis "User Documentation for Mate software")
    (description
     "MATE User Guide is a collection of documentation which details
general use of the MATE Desktop environment.  Topics covered include
sessions, panels, menus, file management, and preferences.")
    (license (list license:fdl1.1+ license:gpl2+))))

(define-public mate-calc
  (package
    (name "mate-calc")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-calc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rj6hrl98d6syf09yv6x3qr3knz5i8y8hvirjn835abwhx3hhakl"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list gettext-minimal
                         intltool
                         pkg-config
                         yelp-tools
                         autoconf
                         autoconf-archive
                         automake
                         libtool
                         mate-common
                         which))
    (inputs (list at-spi2-core
                  glib
                  gtk+
                  libxml2
                  libcanberra
                  mpc
                  mpfr
                  pango))
    (home-page "https://mate-desktop.org/")
    (synopsis "Calculator for MATE")
    (description
     "Mate Calc is the GTK+ calculator application for the MATE Desktop.")
    (license license:gpl2+)))

(define-public mate-backgrounds
  (package
    (name "mate-backgrounds")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-backgrounds")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h1yb0wxhbmmrzih00w5srh1xzkg1sq7jx02carazvq38kn12c55"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list intltool
                         pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         libtool
                         mate-common
                         which))
    (home-page "https://mate-desktop.org/")
    (synopsis "Calculator for MATE")
    (description "This package contains a collection of graphics files which
can be used as backgrounds in the MATE Desktop environment.")
    (license license:gpl2+)))

(define-public mate-netbook
  (package
    (name "mate-netbook")
    (version "1.26.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-netbook")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n0afsmn2k2jj30wzyaq487pbr5dpds01djk13vvvwfkd053y1h1"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list gettext-minimal
                         intltool
                         pkg-config
                         autoconf
                         autoconf-archive
                         automake
                         libtool
                         mate-common
                         which))
    (inputs (list cairo
                  glib
                  gtk+
                  libfakekey
                  libwnck
                  libxtst
                  libx11
                  mate-panel
                  xorgproto))
    (home-page "https://mate-desktop.org/")
    (synopsis "Tool for MATE on Netbooks")
    (description
     "Mate Netbook is a simple window management tool which:

@enumerate
@item Allows you to set basic rules for a window type, such as maximise|undecorate
@item Allows exceptions to the rules, based on string matching for window name
and window class.
@item Allows @code{reversing} of rules when the user manually changes something:
Re-decorates windows on un-maximise.
@end enumerate
")
    (license license:gpl3+)))

(define-public mate-screensaver
  (package
    (name "mate-screensaver")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-screensaver")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k6g60c611v2bkam2f06acmj1wp9i507hqf2dpiycl4gbsy49bwa"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       ;; FIXME: There is a permissions problem with screen locking
       ;; which effectively locks you out completely. Enable locking
       ;; once this has been fixed.
       (list "--enable-locking"
             "--with-kbd-layout-indicator"
             "--with-xf86gamma-ext"
             "--enable-pam"
             "--disable-schemas-compile"
             "--without-console-kit")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'autoconf
                    (lambda _
                      (setenv "SHELL"
                              (which "sh"))
                      (setenv "CONFIG_SHELL"
                              (which "sh"))
                      (substitute* "configure.ac"
                        (("DBUS_SESSION_SERVICE_DIR=`pkg-config.*")
                         "DBUS_SESSION_SERVICE_DIR='${datarootdir}/dbus-1/services'")))))))
    (native-inputs (list automake
                         autoconf
                         autoconf-archive
                         libtool
                         gettext-minimal
                         intltool
                         mate-common
                         pkg-config
                         which
                         xorgproto))
    (inputs (list cairo
                  dconf
                  dbus
                  dbus-glib
                  glib
                  gtk+
                  (librsvg-for-system)
                  libcanberra
                  libglade
                  libmatekbd
                  libnotify
                  libx11
                  libxext
                  libxklavier
                  libxrandr
                  libxrender
                  libxscrnsaver
                  libxxf86vm
                  linux-pam
                  mate-desktop
                  mate-menus
                  pango
                  startup-notification))
    (home-page "https://mate-desktop.org/")
    (synopsis "Screensaver for MATE")
    (description
     "MATE backgrounds package contains a collection of graphics files which
can be used as backgrounds in the MATE Desktop environment.")
    (license license:gpl2+)))

(define-public mate-utils
  (package
    (name "mate-utils")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-utils")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l900d46g2snw1c97m897g263h3yk14wyc5z2jxz2svi4qrb047c"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; Newer itstool does the following--and that causes parallel builds to fail:
     ;; <https://github.com/itstool/itstool/commit/d3adf0264ee2b6fd28b7eff7dec33501d6e75a7c>
     (list
      #:parallel-build? #f))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         (list glib "bin") ;glib-gettextize
                         gtk-doc/stable
                         intltool
                         itstool
                         libice
                         libsm
                         libtool
                         mate-common
                         pkg-config
                         polkit ;for ITS rules
                         which
                         xorgproto
                         yelp-tools))
    (inputs (list at-spi2-core
                  cairo
                  glib
                  gtk+
                  (librsvg-for-system)
                  libcanberra
                  libgtop
                  libx11
                  libxext
                  mate-desktop
                  mate-panel
                  pango
                  startup-notification
                  udisks
                  zlib))
    (home-page "https://mate-desktop.org/")
    (synopsis "Utilities for the MATE Desktop")
    (description "Mate Utilities for the MATE Desktop containing:

@enumerate
@item mate-system-log
@item mate-search-tool
@item mate-dictionary
@item mate-screenshot
@item mate-disk-usage-analyzer
@end enumerate
")
    (license (list license:gpl2 license:fdl1.1+ license:lgpl2.1))))

(define-public eom
  (package
    (name "eom")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/eom")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13976l5dpg2ji2qn55q34bp2mj578877y2y8cnk8vgkz9fpgqx9s"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         gtk-doc/stable
                         gobject-introspection
                         intltool
                         itstool
                         libtool
                         mate-common
                         pkg-config
                         which
                         yelp-tools))
    (inputs (list at-spi2-core
                  cairo
                  dconf
                  dbus
                  dbus-glib
                  exempi
                  glib
                  gtk+
                  libcanberra
                  libx11
                  libxext
                  libpeas
                  libxml2
                  libexif
                  libjpeg-turbo
                  (librsvg-for-system)
                  lcms
                  mate-desktop
                  pango
                  shared-mime-info
                  startup-notification
                  zlib))
    (home-page "https://mate-desktop.org/")
    (synopsis "Eye of MATE")
    (description "Eye of MATE is the Image viewer for the MATE Desktop.")
    (license license:gpl2)))

(define-public engrampa
  (package
    (name "engrampa")
    (version "1.28.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/engrampa")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kwwhgirm0fah4a044c9mf52ch7yiw4hljgpcnr9mbbzr4l1vl7z"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-schemas-compile" "--disable-run-in-place"
              "--enable-magic" "--enable-packagekit"
              (string-append "--with-cajadir="
                             #$output "/lib/caja/extensions-2.0/"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "data/Makefile"
                (("gtk-update-icon-cache")
                 "true")) #t)))))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         gtk-doc/stable
                         intltool
                         itstool
                         libtool
                         mate-common
                         pkg-config
                         which
                         yelp-tools))
    (inputs (list caja
                  file
                  glib
                  gtk+
                  (librsvg-for-system)
                  json-glib
                  libcanberra
                  libx11
                  libsm
                  packagekit
                  pango))
    (home-page "https://mate-desktop.org/")
    (synopsis "Archive Manager for MATE")
    (description "Engrampa is the archive manager for the MATE Desktop.")
    (license license:gpl2)))

(define-public pluma
  (package
    (name "pluma")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/pluma")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17l64v3d3i3i0a37f56d3wpi2c7zp7qljr4169hvaciha894491q"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-python")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-pluma
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/pluma")
                ;; For plugins (same as gedit).
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")))
                `("GUIX_PYTHONPATH" ":" prefix
                  (,(getenv "GUIX_PYTHONPATH")))
                ;; For language-specs.
                `("XDG_DATA_DIRS" ":" prefix
                  (,(string-append #$(this-package-input "gtksourceview")
                                   "/share")))))))
      ;; Tests can not succeed.
      ;; https://github.com/mate-desktop/mate-text-editor/issues/33
      #:tests? #f))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         gtk-doc/stable
                         gobject-introspection
                         intltool
                         itstool
                         libtool
                         mate-common
                         perl
                         pkg-config
                         which
                         yelp-tools))
    (inputs (list at-spi2-core
                  cairo
                  enchant
                  glib
                  gtk+
                  gtksourceview-4
                  gdk-pixbuf
                  iso-codes/pinned
                  libcanberra
                  libx11
                  libsm
                  libpeas
                  libxml2
                  libice
                  mate-desktop
                  packagekit
                  pango
                  python
                  python-pygobject
                  python-wrapper
                  python-pycairo
                  python-six
                  startup-notification))
    (home-page "https://mate-desktop.org/")
    (synopsis "Text Editor for MATE")
    (description "Pluma is the text editor for the MATE Desktop.")
    (license license:gpl2)))

(define-public mate-system-monitor
  (package
    (name "mate-system-monitor")
    (version "1.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-system-monitor")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kdc6x826mbwkcwbdgbcbgk710vl4fl4sxcg5g31pzlqr3h4544h"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--enable-systemd=no")))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         intltool
                         libtool
                         mate-common
                         pkg-config
                         which
                         yelp-tools))
    (inputs (list cairo
                  glib
                  glibmm
                  gtkmm-3
                  gtk+
                  gdk-pixbuf
                  libsigc++
                  libcanberra
                  libxml2
                  libwnck
                  libgtop
                  (librsvg-for-system)
                  polkit))
    (home-page "https://mate-desktop.org/")
    (synopsis "System Monitor for MATE")
    (description "Mate System Monitor provides a tool for for the
MATE Desktop to monitor your system resources and usage.")
    (license license:gpl2)))

(define-public mate-polkit
  (package
    (name "mate-polkit")
    (version "1.28.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mate-polkit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z4awzfmz133sl2h041gnm10vkpq72cl3ngximspvpwhwkpchbpd"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'enable-autostart-for-xfce
            (lambda _
              ;; We also use mate-polkit in Xfce.
              (substitute* (string-append #$output "/etc/xdg/autostart/"
                            "polkit-mate-authentication-agent-1.desktop")
                (("OnlyShowIn=MATE;")
                 "OnlyShowIn=MATE;XFCE;")))))))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         gettext-minimal
                         gtk-doc/stable
                         intltool
                         libtool
                         mate-common
                         pkg-config
                         which))
    (inputs (list accountsservice
                  glib
                  gobject-introspection
                  gtk+
                  gdk-pixbuf
                  polkit))
    (home-page "https://mate-desktop.org/")
    (synopsis "Polkit authentication agent for MATE")
    (description "MATE Polkit is a MATE specific D-Bus service that is
used to bring up authentication dialogs.")
    (license license:lgpl2.1)))

(define-public mozo
  (package
    (name "mozo")
    (version "1.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mate-desktop/mozo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05fk1d4gd3z1k8jj2vrlgq84x0k4zij2yqdp4ndzkv95j7d6ffnm"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules (append %glib-or-gtk-build-system-modules
                                 %pyproject-build-system-modules)
      #:modules '((guix build utils)
                  (guix build glib-or-gtk-build-system)
                  ((guix build pyproject-build-system)
                   #:prefix python:))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/mozo")
                `("GUIX_PYTHONPATH" =
                  (,(getenv "GUIX_PYTHONPATH") ,(python:site-packages inputs
                                                                      outputs)))
                `("GI_TYPELIB_PATH" =
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs (list autoconf
                         autoconf-archive
                         automake
                         intltool
                         libtool
                         mate-common
                         pkg-config
                         which))
    (inputs (list gettext-minimal
                  gtk+
                  mate-menus
                  mate-panel
                  python
                  python-pygobject-3.50))
    (home-page "https://mate-desktop.org/")
    (synopsis "Menu editor for MATE")
    (description "Mozo is a menu editor for MATE using the freedesktop.org
menu specification.")
    (license (list license:lgpl2.1+))))


(define-public mate
  (package
    (name "mate")
    (version (package-version mate-desktop))
    (source #f)
    (build-system trivial-build-system)
    (arguments '(#:builder (mkdir %output)))
    (propagated-inputs
     ;; TODO: Add more packages
     (append (if (or (%current-target-system)
                     (supported-package? gnome-keyring))
                 (list gnome-keyring)
                 '())
             (list at-spi2-core
                   atril
                   caja
                   dbus
                   dconf
                   dconf-editor
                   desktop-file-utils
                   engrampa
                   eom
                   font-abattis-cantarell
                   font-dejavu          ;default font
                   glib-networking
                   gvfs
                   hicolor-icon-theme
                   marco
                   mate-session-manager
                   mate-settings-daemon
                   mate-desktop
                   mate-terminal
                   mate-themes
                   mate-icon-theme
                   mate-power-manager
                   mate-menus
                   mate-notification-daemon
                   mate-panel
                   mate-control-center
                   mate-media
                   mate-applets
                   mate-sensors-applet
                   mate-user-guide
                   mate-calc
                   mate-backgrounds
                   mate-netbook
                   mate-polkit
                   mate-system-monitor
                   mate-utils
                   mozo
                   pluma
                   pinentry-gnome3
                   pulseaudio
                   shared-mime-info
                   yelp
                   zenity)))
    (synopsis "The MATE desktop environment")
    (home-page "https://mate-desktop.org/")
    (description
     "The MATE Desktop Environment is the continuation of GNOME 2.  It provides
an intuitive and attractive desktop environment using traditional metaphors for
GNU/Linux systems.  MATE is under active development to add support for new
technologies while preserving a traditional desktop experience.")
    (license license:gpl2+)))
