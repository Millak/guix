;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2024 宋文武 <iyzsong@envs.net>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016, 2020 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2019 L  p R n  d n <guix@lprndn.info>
;;; Copyright © 2019 Ingo Ruhnke <grumbel@gmail.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2021, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 André A. Gomes <andremegafone@gmail.com>
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

(define-module (gnu packages xfce)
  #:use-module (gnu artwork)
  #:use-module (gnu packages)
  #:use-module (gnu packages apr)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnome-xyz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages search)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:hide (freetype))
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public gtk-xfce-engine
  (package
    (name "gtk-xfce-engine")
    (version "2.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0g86ywkx0ghzhhn96k88p67bbzlm1aqckly85izp07w80l1934ja"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs (list gtk+-2))
    (home-page "https://www.xfce.org/")
    (synopsis "GTK+ theme engine for Xfce")
    (description
     "Default GTK+ engine and themes for Xfce Desktop Environment.")
    (license gpl2+)))

(define-public libxfce4util
  (package
    (name "libxfce4util")
    (version "4.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "libxfce4util/" (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "07xhs4lpirn5hgmpf76j7sc5k6whjxlyxnfa70i2arnwb8x0clla"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config gobject-introspection intltool vala))
    (propagated-inputs (list glib)) ; required by libxfce4util-1.0.pc
    (home-page "https://www.xfce.org/")
    (synopsis "Basic utility library for Xfce")
    (description
     "A general-purpose utility library with core application support for the
Xfce Desktop Environment.")
    (license lgpl2.0+)))

(define-public xfconf
  (package
    (name "xfconf")
    (version "4.18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfconf/" (version-major+minor version) "/"
                                  "xfconf-" version ".tar.bz2"))
              (sha256
               (base32
                "165xbr6y5z4zr235znkqlwkcy2ib9hgfqrdic0n7p57nas8ccv65"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       ;; Run check after install phase to test dbus activation.
       (modify-phases %standard-phases
         ;; tests-end seems to hang forever
         (add-before 'configure 'patchout-tests-end
           (lambda _
             (substitute* "tests/Makefile.in"
               (("tests-end") ""))))
         (add-after 'install 'custom-check
           (lambda _
             (setenv "HOME" (getenv "TMPDIR")) ; xfconfd requires a writable HOME
             ;; Run test-suite under a dbus session.
             (setenv "XDG_DATA_DIRS" ; for finding org.xfce.Xfconf.service
                     (string-append %output "/share"))
             ;; For the missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             (invoke "dbus-launch" "make" "check")))
         (delete 'check))))
    (native-inputs
     (list pkg-config
           intltool
           `(,glib "bin") ;; for gdbus-codegen
           gobject-introspection
           vala
           dbus))
    (propagated-inputs
     ;; libxfconf-0.pc refers to all these.
     (list glib))
    (inputs
     (list libxfce4util))
    (home-page "https://www.xfce.org/")
    (synopsis "Configuration storage and query system for Xfce")
    (description
     "Settings daemon for Xfce, implemented as a D-Bus-based configuration
storage system.")
    (license lgpl2.0+)))

(define-public libxfce4ui
  (package
    (name "libxfce4ui")
    (version "4.18.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0k6b8al8cy2w5zi8iywkzjsz2cgiz9r8ir2dflzxw9kdgiwzxvl7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--with-vendor-info=GNU Guix")))
    (native-inputs
     (list pkg-config intltool gobject-introspection))
    (propagated-inputs
     (list gtk+    ; required by libxfce4ui-2.pc
           ;; libxfce4kbd-private-3.pc refers to all these.
           libxfce4util
           xfconf))
    (inputs (list libgtop libice libsm startup-notification))
    (home-page "https://www.xfce.org/")
    (synopsis "Widgets library for Xfce")
    (description
     "Libxfce4ui is the replacement of the old libxfcegui4 library.  It is used
to share commonly used Xfce widgets among the Xfce applications.")
    (license lgpl2.0+)))

(define-public catfish
  (package
    (name "catfish")
    (version "4.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  "catfish/" (version-major+minor version)
                                  "/catfish-" version ".tar.bz2"))
              (sha256
               (base32
                "16cbsnki7qragwhbfs3h0ja7xg8xlf59ajxhddqm0jkmrirrpbpx"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-command-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "catfish/CatfishSearchEngine.py"
               (("'which'") (string-append "'" (which "which") "'")))
             (substitute* "catfish/CatfishWindow.py"
               (("xdg-mime") (which "xdg-mime"))
               (("xdg-open") (which "xdg-open")))))
         ;; setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/")))
         ;; The check failed to spawn a message bus without /etc/machine-id.
         (delete 'sanity-check)
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/catfish")
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))
       #:tests? #f))
    (native-inputs
     (list pkg-config python-distutils-extra intltool))
    (inputs
     (list bash-minimal which xfconf xdg-utils))
    (propagated-inputs
     (list gtk+ python-dbus python-pexpect python-pycairo
           python-pygobject))
    (home-page "https://docs.xfce.org/apps/catfish/start")
    (synopsis "File searching tool for Xfce")
    (description
     "Catfish is a file searching tool for Linux and Unix.  The interface is
intentionally lightweight and simple, using only GTK+ 3.  You can configure
it to your needs by using several command line options.")
    (license gpl2+)))

(define-public elementary-xfce-icon-theme
  (package
    (name "elementary-xfce-icon-theme")
    (version "0.18")
    (source (origin
              (method git-fetch)
              (uri
               (git-reference
                (url "https://github.com/shimmerproject/elementary-xfce")
                (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "124bdgghkjkpcsfn9a2ad6zqy233pi9jqmv8w8a3ha6q3al2s11s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:make-flags '("CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-git-checkout-writable
           (lambda _
             (for-each make-file-writable (find-files "."))
             #t)))))
    (native-inputs
     (list gtk+ optipng pkg-config))
    (home-page "https://shimmerproject.org/")
    (synopsis "Elementary icons extended and maintained for Xfce")
    (description "This is a fork of the upstream elementary project.  This icon
theme is supposed to keep everything working for Xfce, but gets updates from
upstream occasionally.")
    (license gpl2+)))

(define-public exo
  (package
    (name "exo")
    (version "4.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "exo/" (version-major+minor version) "/"
                                  "exo-" version ".tar.bz2"))
              (sha256
               (base32
                "0mbxv8hys1i8s4p5d0pvgywsjgf21ki0mzcp8avcv2588p862b2g"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (propagated-inputs
     ;; exo-2.pc refers to all these.
     (list gtk+ libxfce4util))
    (inputs
     (list libxfce4ui))
    (home-page "https://www.xfce.org/")
    (synopsis "Extension library for Xfce")
    (description
     "An extension library to Xfce.  While Xfce comes with quite a few libraries
that are targeted at desktop development, libexo is targeted at application
development.")
    ;; Libraries are under LGPLv2+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))))

(define-public garcon
  (package
    (name "garcon")
    (version "4.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "garcon/" (version-major+minor version) "/"
                                  "garcon-" version ".tar.bz2"))
              (sha256
               (base32
                "0ka35nxqhl4cbmyf9x2ysinihixsqmibqywqr2zqz5iiw699530v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list `(,glib "bin") gobject-introspection intltool pkg-config))
    (propagated-inputs
     (list gtk+ ; required by garcon-gtk3-1.pc
           libxfce4ui))     ; required by garcon-gtk3-1.pc
    (home-page "https://www.xfce.org/")
    (synopsis "Implementation of the freedesktop.org menu specification")
    (description
     "Garcon is a freedesktop.org compliant menu implementation based on
GLib and GIO.  It was started as a complete rewrite of the former Xfce menu
library called libxfce4menu, which, in contrast to garcon, was lacking menu
merging features essential for loading menus modified with menu editors.")
    (license lgpl2.0+)))

(define-public tumbler
  (package
    (name "tumbler")
    (version "4.18.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "tumbler/" (version-major+minor version) "/"
                                  "tumbler-" version ".tar.bz2"))
              (sha256
               (base32
                "0ymy6a0hbv5iainphgpd7dfi8snpg7zs7lyqq2cgiiza6p3fwc5m"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool
           `(,glib "bin") ; need glib-genmarshal
           dbus-glib))       ; need dbus-binding-tool
    (propagated-inputs
     (list glib))                 ; required by tumbler-1.pc
    (inputs
     (list dbus
           gdk-pixbuf
           cairo ;; Needed for pdf thumbnails (poppler-glibc.pc)
           freetype
           libjpeg-turbo
           libgsf
           libxfce4util
           poppler
           ;; FIXME Provide gstreamer and gstreamer-tag to get video thumbnails
           ;; ("gstreamer" ,gstreamer)
           ))
    (home-page "https://www.xfce.org/")
    (synopsis "D-Bus service for applications to request thumbnails")
    (description
     "Tumbler is a D-Bus service for applications to request thumbnails for
various URI schemes and MIME types.  It is an implementation of the thumbnail
management D-Bus specification.")
    (license gpl2+)))

(define-public xfce4-panel
  (package
    (name "xfce4-panel")
    (version "4.18.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0qkw1msrvq7hc4mjg9iai0kymgkrpj1nijv04zjbdfcbymhp2cr1"))
              (patches (search-patches "xfce4-panel-plugins.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tzdata-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (string-append "plugins/clock/clock.c")
               (("/usr/share/zoneinfo")
                (search-input-directory inputs "share/zoneinfo"))))))))
    (native-inputs
     (list pkg-config
           intltool
           `(,glib "bin")))
    (propagated-inputs
     (list gtk+ ; required by libxfce4panel-2.0.pc
           libxfce4util)) ; required by libxfce4panel-2.0.pc
    (inputs
     (list tzdata ;; For fix-tzdata-path phase only.
           exo
           gtk+-2
           xfconf
           garcon
           libwnck
           libxfce4ui))
    (native-search-paths
     (list (search-path-specification
            (variable "X_XFCE4_LIB_DIRS")
            (files '("lib/xfce4")))))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce desktop panel")
    (description
     "Desktop panel for Xfce, which contains program launchers, window buttons,
applications menu, workspace switcher and more.")
    ;; Libraries are under LGPLv2.1+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))))

(define-public xfce4-battery-plugin
  (package
    (name "xfce4-battery-plugin")
    (version "1.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "04z2bic6c2cgl6wy4qrhfdigb6c2hsxnqraa20k1xvi0nfzk68km"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config intltool))
    (inputs (list glib gtk+ libxfce4util libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-battery-plugin")
    (synopsis "Battery monitor panel plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list gpl2+ lgpl2.0+))))

(define-public xfce4-clipman-plugin
  (package
    (name "xfce4-clipman-plugin")
    (version "1.6.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-clipman-plugin/" (version-major+minor version) "/"
                                  "xfce4-clipman-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "083i9pbcqjkdzzjx055ld4gd4gn3xnc3rn10vpsqv1vg01dlgb88"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list exo libxfce4ui libxtst xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-clipman-plugin")
    (synopsis "Clipboard manager for Xfce")
    (description
     "Clipman is a clipboard manager for Xfce.  It keeps the clipboard contents
around while it is usually lost when you close an application.  It is able to
handle text and images, and has a feature to execute actions on specific text by
matching them against regular expressions.")
    (license (list gpl2+))))

(define-public xfce4-pulseaudio-plugin
  (package
    (name "xfce4-pulseaudio-plugin")
    (version "0.4.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                           "xfce4-pulseaudio-plugin/"
                           (version-major+minor version) "/"
                           "xfce4-pulseaudio-plugin-" version ".tar.bz2"))
       (sha256
        (base32 "0j037wnx0z22nw11mq0y3cnq1srr52zckjap3klj3hirghh2nx5x"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config dbus-glib dbus))
    (inputs
     (list exo libnotify libxfce4ui pulseaudio xfce4-panel))
    (home-page "https://git.xfce.org/panel-plugins/xfce4-pulseaudio-plugin/")
    (synopsis "PulseAudio panel plugin for Xfce")
    (description
     "Xfce PulseAudio plugin is a plugin for the Xfce panel which provides a
convenient way to adjust the audio volume of the PulseAudio sound system and
to an auto mixer tool like pavucontrol.  It can optionally handle multimedia
keys for controlling the audio volume.")
    (license gpl2+)))

(define-public xfce4-whiskermenu-plugin
  (package
    (name "xfce4-whiskermenu-plugin")
    (version "2.7.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                           "xfce4-whiskermenu-plugin/" (version-major+minor version) "/"
                           "xfce4-whiskermenu-plugin-" version ".tar.bz2"))
       (sha256
        (base32 "1sg6kx52c5j1l7y6z156gxk4b0ppj210i40hzbvqqf35jvn9j9ns"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list xfce4-panel
           garcon
           gettext-minimal
           exo
           gtk
           libxfce4ui))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-shell-script
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (string-append "panel-plugin/xfce4-popup-whiskermenu.in")
               (("@CMAKE_INSTALL_FULL_BINDIR@")
                (string-append (assoc-ref inputs "xfce4-panel") "/bin"))
               (("gettext") (which "gettext")))
             #t)))))
    (home-page "https://goodies.xfce.org/projects/panel-plugins/xfce4-whiskermenu-plugin")
    (synopsis "Application menu panel plugin for Xfce")
    (description
     "This package provides an alternative to the default application menu
panel plugin for Xfce4.  It uses separate sections to display categories and
applications, and includes a search bar to search for applications.")
    ;; The main plugin code is covered by gpl2, but files in panel-plugin directory
    ;; are covered by gpl2+.  The SVG icon is covered by gpl2.
    (license (list gpl2 gpl2+))))

(define-public xfce4-xkb-plugin
  (package
    (name "xfce4-xkb-plugin")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "11s9s0634g9rf5hcj6nga6hsv71wcq9c7ym6nsis077gfmwfkgzh"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list garcon
           (librsvg-for-system)
           libwnck
           libx11
           libxfce4ui
           libxklavier
           xfce4-panel))
    (home-page "https://git.xfce.org/panel-plugins/xfce4-xkb-plugin/")
    (synopsis "XKB layout switching panel plug-in for Xfce")
    (description
     "Xfce XKB plugin makes it possible to set up and use multiple
keyboard layouts.

One can choose the keyboard model, what key combination to
use to switch between the layouts, the actual keyboard layouts,
the way in which the current layout is being displayed (country
flag image or text) and the layout policy, which is whether to
store the layout globally (for all windows), per application or
per window.")
    (license bsd-2)))

(define-public xfce4-appfinder
  (package
    (name "xfce4-appfinder")
    (version "4.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1yck11y86d45yxsppd1yqk894k3cf5vh91a5sm559gl175jylm4q"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list garcon gtk+ libxfce4ui))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce application finder")
    (description
     "Application finder for Xfce, it will show the applications installed on
your system in categories, so you can quickly find and launch them.")
    (license gpl2+)))

(define-public xfce4-session
  (package
    (name "xfce4-session")
    (version "4.18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfce4-session/" (version-major+minor version) "/"
                                  "xfce4-session-" version ".tar.bz2"))
              (sha256
               (base32
                "1v4lfgz7az8iqp01a1ww8sxxjfmr676bijhrhw4r6r7cjvh96brq"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "xfsm-shutdown-helper/main.c"
                    (("/sbin/shutdown -h now")  "halt")
                    (("/sbin/shutdown -r now")  "restart")
                    (("/usr/sbin/pm-suspend")   "pm-suspend")
                    (("/usr/sbin/pm-hibernate") "pm-hibernate"))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xsession-prefix=" %output))
       ;; Disable icon cache update.
       #:make-flags
       '("gtk_update_icon_cache=true")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-xflock
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xset (assoc-ref inputs "xset")))
               (substitute* "scripts/xflock4"
                 (("xset") (string-append xset "/bin/xset")))))))))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list iceauth
           upower
           polkit
           libsm
           libwnck
           libxfce4ui
           xset))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce session manager")
    (description
     "Session manager for Xfce, it will restore your session on startup and
allows you to shut down the computer from Xfce.")
    (license gpl2+)))

(define-public xfce4-settings
  (package
    (name "xfce4-settings")
    (version "4.18.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1p513i8zwc3glv2cf3x2abwm4hvdb7clamj19yadj2036v85a37i"))
              (patches (search-patches "xfce4-settings-defaults.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-pluggable-dialogs"
                           "--enable-sound-settings"
                           "--enable-upower-glib"
                           "--enable-xrandr")))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list colord
           exo
           garcon
           libnotify
           libxcursor
           libxi
           libxklavier
           libxrandr
           libxfce4ui
           upower
           python ;; for xfce4-compose-mail
           xf86-input-libinput))
    (propagated-inputs
     ;; Some operations, such as changing icon themes, require these schemas
     ;; to be in the search path.
     (list gsettings-desktop-schemas))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce settings manager")
    (description
     "Settings manager for Xfce, it can control various aspects of the desktop
like appearance, display, keyboard and mouse settings.")
    (license gpl2+)))

(define-public thunar
  (package
    (name "thunar")
    (version "4.18.10")                           ;stable version = even minor
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "thunar/" (version-major+minor version) "/"
                                  "thunar-" version ".tar.bz2"))
              (sha256
               (base32
                "020xrwzdnk5b7cli8vmv3yn7sfq6pg7bz57m4p0xh0ln2cgqlc78"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags '("--with-custom-thunarx-dirs-enabled")))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list exo
           gobject-introspection
           gvfs
           libexif
           libgudev
           libnotify
           libxfce4ui
           pcre
           xfce4-panel
           startup-notification))
    (native-search-paths
     (list (search-path-specification
            (variable "THUNARX_DIRS")
            (files (list "lib/thunarx-3")))))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce file manager")
    (description
     "A modern file manager for graphical desktop, aiming to be easy-to-use and
fast.")
    (license gpl2+)))

(define-public thunar-volman
  (package
    (name "thunar-volman")
    (version "4.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/xfce/thunar-volman/"
                           (version-major+minor version) "/"
                           "thunar-volman-" version ".tar.bz2"))
       (sha256
        (base32 "0rgwhxp6f3cd4ldl6zirvzl4n64f2k65lach84gs4ip2zdzmrdwk"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list exo libgudev libnotify libxfce4ui))
    (home-page "https://www.xfce.org/")
    (synopsis "Removable media manager for Thunar")
    (description
     "Thunar-volman is an extension for the Thunar File Manager, which enables
automatic management of removable drives and media.  For example, if
thunar-volman is installed and configured properly, and you plug in your
digital camera, it will automatically spawn your preferred photo application
and import the new pictures from your camera.")
    (license gpl2+)))

(define-public thunar-archive-plugin
  (package
    (name "thunar-archive-plugin")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/thunar-plugins/"
                           name "/" (version-major+minor version)
                           "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1qxdsnwjy8z358yd0avi1h2r6y1izfv26dzlp5fsh3pwpivzhyb3"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config intltool))
    (inputs (list exo thunar gtk+))
    (home-page "https://www.xfce.org/")
    (synopsis "Archive plugin for Thunar file manager")
    (description "The Thunar Archive Plugin allows you to create and extract
archive files using the file context menus in the Thunar file manager.")
    (license gpl2+)))

(define-public thunar-shares-plugin
  (package
    (name "thunar-shares-plugin")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/thunar-plugins/"
                           name "/" (version-major+minor version)
                           "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "01rzmjj673sjhzkdh24d8bha57dbahycb44hlr4zld0mr7kda28h"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config intltool))
    (inputs (list thunar gtk+))
    (home-page "https://www.xfce.org/")
    (synopsis "Folder share plugin for Thunar file manager")
    (description
     "The Thunar Shares Plugin allows you to quickly share a folder using
Samba from Thunar (the Xfce file manager) without requiring root access.")
    (license gpl2+)))

(define-public thunar-media-tags-plugin
  (package
    (name "thunar-media-tags-plugin")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/thunar-plugins/"
                           name "/" (version-major+minor version)
                           "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "02n99ymqs8msa798r63sy409pwrycg8p4pxx3yf497k924g134lm"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config intltool))
    (inputs (list exo gtk+ thunar taglib))
    (home-page "https://www.xfce.org/")
    (synopsis "Media tags plugin for Thunar file manager")
    (description
     "Media tags plugin allows tags editing from Thunar file manager and
tags-based file renaming from inside Thunar Bulk Renamer.")
    (license gpl2+)))

(define-public thunar-vcs-plugin
  (package
    (name "thunar-vcs-plugin")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/thunar-plugins/"
                           name "/" (version-major+minor version)
                           "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "1f2d1dwfyi6xv3qkd8l8xh0vhz8wh0601cyigjzn426lqga1d29n"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "CPPFLAGS=-I"
                                  #$(this-package-input "apr-util")
                                  "/include/apr-1"))))
    (native-inputs (list pkg-config intltool utf8proc))
    (inputs
     (list exo
           gtk+
           thunar
           libxfce4util
           apr
           apr-util
           subversion
           git))
    (home-page "https://www.xfce.org/")
    (synopsis "VCS plugin for Thunar file manager")
    (description
     "Thunar VCS Plugin (formerly known as Thunar SVN Plugin) gives SVN and
GIT integration to Thunar, it adds Subversion and GIT actions to the context
menu.")
    (license gpl2+)))

(define-public thunarx-python
  (package
    (name "thunarx-python")
    (version "0.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/bindings/"
                           name "/" (version-major+minor version)
                           "/" name "-" version ".tar.bz2"))
       (sha256
        (base32 "08lz9pvx006a2fypg2q38p61jbhy0yswz8cizlxpiwfcqsvhpnln"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config which))
    (inputs
     (list exo
           thunar
           python
           python-pygobject
           libxfce4ui
           gtk+))
    (home-page "https://gitlab.xfce.org/bindings/thunarx-python")
    (synopsis "Python Bindings for Thunar")
    (description
     "These bindings allow one to create python plugins for Thunar.")
    (license gpl2+)))

(define-public xfwm4
  (package
    (name "xfwm4")
    (version "4.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfwm4/" (version-major+minor version) "/"
                                  "xfwm4-" version ".tar.bz2"))
              (sha256
               (base32
                "0gy1bxp9va6hz9vrysvh3sffg5ld4dn77in10syb8p5jkf41pkcj"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list libdrm
           libwnck
           libxcomposite
           libxdamage
           libxfce4ui
           libxpresent
           libxrandr))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce window manager")
    (description
     "Window manager for Xfce, it handles the placement of windows
on the screen.")
    (license gpl2+)))

(define-public xfwm4-themes
  (package
    (name "xfwm4-themes")
    (version "4.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://archive.xfce.org/src/art/xfwm4-themes/"
                           (version-major+minor version) "/"
                           "xfwm4-themes-" version ".tar.bz2"))
       (sha256
        (base32
         "0xfmdykav4rf6gdxbd6fhmrfrvbdc1yjihz7r7lba0wp1vqda51j"))))
    (build-system gnu-build-system)
    (home-page "https://www.xfce.org/")
    (synopsis "Themes for the Xfce window manager")
    (description "This package provides a set of additional themes for the Xfce
window manager.")
    (license gpl3+)))

(define-public xfdesktop
  (package
    (name "xfdesktop")
    (version "4.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfdesktop/" (version-major+minor version) "/"
                                  "xfdesktop-" version ".tar.bz2"))
              (sha256
               (base32
                "0mpp9znpwkhp1h4cmpxhkhij1qgdr0fa6npzm4i7x1r51hcni4pg"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   (copy-file #$(file-append %artwork-repository "/logo/Guix.svg")
                              "backgrounds/guix-logo.svg")
                   #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'prepare-background-image
                    (lambda _
                      ;; Stick a Guix logo in the background image.  XXX: It
                      ;; has to go to the center because the image might be
                      ;; truncated on the edges.  :-/
                      (invoke "inkscape" "--export-dpi=120"
                              "--export-png=/tmp/guix.png"
                              "backgrounds/guix-logo.svg")
                      (for-each (lambda (image)
                                  (invoke "composite" "-gravity" "center"
                                          "/tmp/guix.png" image
                                          "/tmp/final.jpg")
                                  (copy-file "/tmp/final.jpg" image))
                                '(;; "backgrounds/xfce-blue.jpg"
                                  "backgrounds/xfce-stripes.png"
                                  "backgrounds/xfce-teal.jpg"
                                  "backgrounds/xfce-verticals.png"))
                      #t)))

       #:disallowed-references (,inkscape/stable ,imagemagick)))
    (native-inputs
     (list pkg-config intltool
           ;; For our own ‘prepare-background-image’ phase.
           inkscape/stable imagemagick))
    (inputs
     (list exo
           garcon
           libnotify
           libwnck
           libxfce4ui
           thunar))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce desktop manager")
    (description
     "Desktop manager for Xfce, it sets the background color or image with
optional application menu or icons for minimized applications or launchers,
devices and folders.")
    (license gpl2+)))

(define-public gigolo
  (package
    (name "gigolo")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1l1g9ljvyzir1jpjf0248nnwwf3sakwljlik6cj5wrj4azv88nfj"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config intltool))
    (inputs (list gtk+))
    (home-page "https://www.xfce.org/")
    (synopsis "Manage connections to remote file systems")
    (description
     "Gigolo is a graphical user interface to easily manage connections to
remote file systems using GIO/GVfs.  It allows you to quickly connect/mount
local and remote file systems and manage bookmarks of such.")
    (license gpl2)))                              ;version 2 only

(define-public parole
  (package
    (name "parole")
    (version "4.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0rjsqm582c8i3lyi1n2ghp638cr0chw358i05g7njbw1hjj68wqc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "CPPFLAGS=-I"
                             #$(this-package-input "gst-plugins-base")
                             "/include/gstreamer-1.0"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-parole
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                (wrap-program (string-append #$output "/bin/parole")
                  #:sh (search-input-file inputs "bin/bash")
                  `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))))))))
    (native-inputs
     (list pkg-config intltool gobject-introspection))
    (inputs
     (list bash-minimal                           ;for 'wrap-program'
           dbus-glib
           (list glib "bin")
           gstreamer
           gst-plugins-base
           gst-plugins-good
           libnotify
           libxfce4ui
           libxfce4util
           taglib))
    (home-page "https://www.xfce.org/")
    (synopsis "Media player based on the GStreamer framework")
    (description "Parole is a modern simple media player based on the
GStreamer framework and written to fit well in the Xfce desktop.  Parole
features playback of local media files, DVD/CD and live streams.")
    (license gpl2)))                    ;version 2 only

(define-public xfce4-terminal
  (package
    (name "xfce4-terminal")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0i9xgd0rgw0j59hlp9kyxndw2b35nhxjls09c20rzdj1sj4dak91"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "CFLAGS=-I"
                             #$(this-package-input "libxfce4ui")
                             "/include/xfce4"))))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list libxfce4ui vte))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce terminal emulator")
    (description
     "A lightweight and easy to use terminal emulator for Xfce.  Features
include a simple configuration interface, the ability to use multiple tabs
with terminals within a single window, the possibility to have a
pseudo-transparent terminal background, and a compact mode (where both the
menubar and the window decorations are hidden) that helps you to save space
on your desktop.")
    (license gpl2+)))

(define-public xfce4-dict
  (package
    (name "xfce4-dict")
    (version "0.8.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0h8adjgb7126hafik7r1k9wr3g6d835ggsh8zj9k4k9mf35bfpdf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://www.xfce.org/")
    (synopsis "Dictionary of Xfce desktop")
    (description
     "Xfce4-dict allows you to search different kinds of dictionary services
for words or phrases and shows you the result.  Currently you can query a Dict
server (RFC 2229), any online dictionary service by opening a web browser or
search for words using the aspell/ispell program.

xfce4-dict contains a stand-alone application called “xfce4-dict” and a panel
plugin for the Xfce panel.")
    (license gpl2+)))

(define-public xfdashboard
  (package
    (name "xfdashboard")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name
                                  "/"
                                  (version-major+minor version)
                                  "/"
                                  name
                                  "-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "19a8xz6zv039j2m595v4wld3x4inidc7idfkk0pp42ffwm1l6a55"))))
    (build-system gnu-build-system)
    (native-inputs (list intltool pkg-config `(,glib "bin")))
    (inputs (list clutter
                  garcon
                  gtk+
                  libwnck
                  libxcomposite
                  libxdamage
                  libxfce4util
                  libxfce4ui
                  libxinerama
                  xfconf))
    (home-page "https://www.xfce.org/")
    (synopsis "Gnome shell like dashboard for Xfce")
    (description
     "Xfdashboard provides a GNOME shell dashboard and MacOS Mission Control
like interface for Xfce desktop.  It can be configured to run with any
keyboard shortcut, when executed it provides an overview of applications
currently opened which let user to switch between different applications.  Its
search feature works like Xfce's app finder and makes it convenient to search
for and start applications.")
    (license gpl2+)))

(define-public xfce
  (package
    (name "xfce")
    (version (package-version xfce4-session))
    (source #f)
    (build-system trivial-build-system)
    (arguments
     '(#:modules ((guix build union))
       #:builder
       (begin
         (use-modules (ice-9 match)
                      (guix build union))
         (match %build-inputs
           (((names . directories) ...)
            (union-build (assoc-ref %outputs "out")
                         directories)
            #t)))))
    (inputs
     (list exo
           garcon
           adwaita-icon-theme
           elementary-xfce-icon-theme
           greybird-gtk-theme
           hicolor-icon-theme
           mate-polkit-for-xfce
           mousepad
           ristretto
           shared-mime-info
           thunar
           thunar-volman
           tumbler
           xfce4-appfinder
           xfce4-notifyd                          ;for pop-up notifications
           xfce4-panel
           xfce4-power-manager
           xfce4-screenshooter
           xfce4-session
           xfce4-settings
           xfce4-taskmanager
           xfce4-terminal
           xfconf
           xfdesktop
           xfwm4
           xfwm4-themes
           xkill
           ;; Panel plugins.
           xfce4-battery-plugin
           xfce4-clipman-plugin
           xfce4-pulseaudio-plugin
           xfce4-volumed-pulse
           xfce4-xkb-plugin))
    (propagated-inputs
     ;; Default font that applications such as IceCat require.
     (list font-dejavu))
    (native-search-paths
     ;; For finding panel and thunar plugins.
     (append
      (package-native-search-paths xfce4-panel)
      (package-native-search-paths thunar)))
    (home-page "https://www.xfce.org/")
    (synopsis "Desktop environment (meta-package)")
    (description
     "Xfce is a lightweight desktop environment.  It aims to be fast and low on
system resources, while still being visually appealing and user friendly.")
    (license gpl2+)))

(define-public xfce4-power-manager
  (package
    (name "xfce4-power-manager")
    (version "4.18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfce4-power-manager/" (version-major+minor version) "/"
                                  "xfce4-power-manager-" version ".tar.bz2"))
              (sha256
               (base32
                "1w445v3911cf7l6w5c0f84aphv8s579f8srnhjrhf3drd07xsy8d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list libxrandr gtk+ upower libnotify libxfce4ui))
    (home-page "https://www.xfce.org/")
    (synopsis "Xfce Power Manager")
    (description
     "This is a power manager for the Xfce desktop.  It manages the power
sources on the computer and the devices that can be controlled to reduce their
power consumption (such as LCD brightness level, monitor sleep, CPU frequency
scaling, etc).  In addition, xfce4-power-manager provides a set of
freedesktop-compliant DBus interfaces to inform other applications about current
power level so that they can adjust their power consumption, and it provides the
inhibit interface which allows applications to prevent automatic sleep.")
    (license gpl2+)))

(define-public ristretto
  (package
    (name "ristretto")
    (version "0.13.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/ristretto/"
                                  (version-major+minor version) "/"
                                  "ristretto-" version ".tar.bz2"))
              (sha256
               (base32
                "1nbxfm6ljcw45vn8hhjxwcv3k387hdah4jnn07n9w08n63g5x7vp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool desktop-file-utils
           `(,glib "bin") ; for gdbus-codegen
           pkg-config))
    (inputs
     (list gtk+ libexif libxfce4ui tumbler))
    (home-page "https://docs.xfce.org/apps/ristretto/start")
    (synopsis "Fast and lightweight picture-viewer")
    (description
     "The Ristretto Image Viewer is an application that can be used to view,
and scroll through images.  It can be used to run a slideshow of images, open
images with other applications like an image-editor or configure an image as
the desktop wallpaper.")
    (license gpl2+)))

(define-public xfce4-taskmanager
  (package
    (name "xfce4-taskmanager")
    (version "1.5.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  "xfce4-taskmanager/" (version-major+minor version) "/"
                                  "xfce4-taskmanager-" version ".tar.bz2"))
              (sha256
               (base32
                "1fv83xcbnlwabi32z3fsdik1knh7v45ji529dx9kwlv4b8pq6dk7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libwnck
           libxmu
           gtk+
           libxfce4ui
           ;; FIXME: Remove libxext and libxt when libxmu propagates them.
           libxext
           libxt))
    (home-page "https://goodies.xfce.org/projects/applications/xfce4-taskmanager")
    (synopsis "Easy to use task manager")
    (description
     "This is a task manager for the Xfce desktop.  It displays the CPU and
memory usage graphically, and it can display processes as a tree.")
    (license gpl2+)))

(define-public orage
  (package
    (name "orage")
    (version "4.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1v5385hps6jgcw1ky9vl7w7iryp0rzxz6s4lx72rz8yg4sdv84v3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list
      `(,glib "bin")                    ; for dbus-binding-tool
      intltool
      pkg-config))
    (inputs
     (list dbus-glib gtk+-2 libical libnotify libxfce4ui popt xfce4-panel))
    (home-page "https://www.xfce.org/projects/")
    (synopsis "Simple calendar application with reminders")
    (description
     "This is a simple calendar application for the Xfce desktop.  Orage has
alarms and uses the iCalendar format, making it compatible with many other
calendar applications.  It also includes a panel clock plugin and an
international clock application capable of simultaneously showing clocks from
several different time zones.")
    (license gpl2+)))

(define-public xfce4-notifyd
  (package
    (name "xfce4-notifyd")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "063qxbcy8djijsb0clzkai3mwg43mmlswwrg403vsi2w0n614v5f"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-configure
                 (lambda _
                   (substitute* "configure"
                     (("\\$PKG_CONFIG --variable=gdbus_codegen gio-2.0")
                      "which gdbus-codegen")
                     (("\\$PKG_CONFIG --variable=glib_compile_resources gio-2.0")
                      "which glib-compile-resources")
                     (("\\$PKG_CONFIG --variable=glib_genmarshal glib-2.0")
                      "which glib-genmarshal")
                     (("\\$PKG_CONFIG --variable=glib_mkenums glib-2.0")
                      "which glib-mkenums")))))))
    (native-inputs
     (list intltool pkg-config (list glib "bin") which))
    (inputs
     (list libxfce4ui libnotify sqlite xfce4-panel))
    (home-page "https://goodies.xfce.org/projects/applications/xfce4-notifyd")
    (synopsis "Show notification bubbles on Xfce")
    (description
     "The Xfce Notify Daemon (xfce4-notifyd for short) is a smallish program
that implements the “server-side” portion of the Freedesktop desktop
notifications specification.  Applications that wish to pop up a notification
bubble in a standard way can implicitly make use of xfce4-notifyd to do so by
sending standard messages over D-Bus using the
@code{org.freedesktop.Notifications} interface.")
    (license gpl2)))

(define-public xfburn
  (package
    (name "xfburn")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/xfburn/"
                                  (version-major+minor version) "/"
                                  "xfburn-" version ".tar.bz2"))
              (sha256
               (base32
                "0i8zpgp4mj78pn2023pdhzb7552r3b52phzp2gjr6jq4kakhx5ms"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list exo
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gst-plugins-ugly
           glib
           gtk+
           libburn
           libisofs
           libxfce4ui))
    (home-page "https://goodies.xfce.org/projects/applications/xfburn")
    (synopsis "GTK+ based CD, DVD and Blu-ray burning application")
    (description
     "Xfburn is a simple CD, DVD, and Blu-ray burning tool based on
the libburnia libraries.  It can blank CD/DVD/BD(-RW)s, burn and
create iso images, audio CDs, as well as burn personal compositions
of data to either CD/DVD/BD.")
    (license gpl2+)))

(define-public mousepad
  (package
    (name "mousepad")
    (version "0.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/mousepad/"
                                  (version-major+minor version) "/mousepad-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "17fi33mkdz1nfmsgqlfa20l06wwy0s8lcj21cfg6ikdiihxwpjp7"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '(;; Use the GSettings keyfile backend rather than
                           ;; DConf.
                           "--enable-keyfile-settings")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gtksourceview (assoc-ref inputs "gtksourceview")))
              (wrap-program (string-append out "/bin/mousepad")
                ;; For language-specs.
                `("XDG_DATA_DIRS" ":" prefix (,(string-append gtksourceview
                                                              "/share"))))))))))
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-compile-schemas.
           pkg-config))
    (inputs
     (list bash-minimal gtk+ gtksourceview-4 xfconf))
    (home-page "https://git.xfce.org/apps/mousepad/")
    (synopsis "Simple text editor for Xfce")
    (description
     "Mousepad is a graphical text editor for Xfce based on Leafpad.")
    (license gpl2+)))

(define-public xfce4-screenshooter
  (package
   (name "xfce4-screenshooter")
   (version "1.10.5")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://archive.xfce.org/src/apps/"
                                "xfce4-screenshooter/"
                                (version-major+minor version)
                                "/xfce4-screenshooter-"
                                version ".tar.bz2"))
            (sha256
             (base32
              "0732f1v6s1zkflq47rgdsimq73k7q94gwag1y9sza5smd8m1ywgs"))))
   (build-system gnu-build-system)
   (native-inputs
    (list pkg-config intltool
          `(,glib "bin")))        ; glib-genmarshal
   (inputs
    (list exo libsoup-minimal-2 libxfce4ui xfce4-panel))
   (home-page "https://goodies.xfce.org/projects/applications/xfce4-screenshooter")
   (synopsis "Xfce's application to take screenshots")
   (description
    "This application allows you to capture the entire screen, the active
window or a selected region.  You can set the delay that elapses before the screenshot
is taken and the action that will be done with the screenshot.
A plugin for the Xfce panel is also available.")
   (license gpl2+)))

(define-public xfce4-screensaver
  (package
    (name "xfce4-screensaver")
    (version "4.18.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  "xfce4-screensaver/"
                                  (version-major+minor version)
                                  "/xfce4-screensaver-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0f9sw703pcgz47689qgc550h2hpqlzvsfgggd7z9s6516rhk2wfi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-dbus-1-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dbus-dir (string-append out "/share/dbus-1/services")))
               (substitute* "configure"
                 (("DBUS_SESSION_SERVICE_DIR=.*")
                  (string-append "DBUS_SESSION_SERVICE_DIR="
                                 dbus-dir)))))))))
    (native-inputs
     (list pkg-config
           intltool
           glib ; glib-compile-schemas
           `(,glib "bin")))                 ; glib-compile-schemas
    (inputs
     (list dbus-glib
           linux-pam
           elogind
           garcon
           libxklavier
           libwnck
           libxscrnsaver
           xfconf))
    (home-page "https://docs.xfce.org/apps/screensaver/start")
    (synopsis "Screensaver for the Xfce desktop")
    (description
     "Xfce Screensaver is a screen saver and locker that aims to have simple,
 sane, secure defaults and be well integrated with the Xfce desktop.")
    (license gpl2+)))

(define-public xfce4-volumed-pulse
  (package
    (name "xfce4-volumed-pulse")
    (version "0.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/apps/"
                                  name "/" (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0ql3hlciffjs981jp21hg6phrk25crh64yc01fm0l75k4vvf66d0"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list xfconf libnotify pulseaudio keybinder-3.0 gtk+))
    (home-page "https://goodies.xfce.org/projects/applications/xfce4-volumed")
    (synopsis "XFCE volume keys daemon")
    (description
     "This is a volume keys control daemon for Xfce Desktop environment. It controls
 the volume using multimedia keys. It also provides volume change notifications.")
    (license gpl3+)))

(define-public xfce4-cpugraph-plugin
  (package
   (name "xfce4-cpugraph-plugin")
   (version "1.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-cpugraph-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-cpugraph-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "05frfn3y009xndks9rsw90jgk0v5zfarn5jqaqci45v9ab82sy9p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-cpugraph-plugin")
    (synopsis "Display CPU load as a graph in the Xfce panel")
    (description "This panel plugin offers multiple display
modes (LED, gradient, fire, etc…) to show the current CPU load of the
system.  Various appearance options, like colors or size, are
customizable.

On multi core or multi CPU systems, CPU Graph can either track and
display all of them at once, or at the user's option only a specific
core or CPU.")
    (license gpl2+)))

(define-public xfce4-eyes-plugin
  (package
   (name "xfce4-eyes-plugin")
   (version "4.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-eyes-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-eyes-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "092kmv2i0flg3zg622q0id70mz1kvlz27y12i5y3nfdsi1fz03xd"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-eyes-plugin")
    (synopsis "Display a pair of eyes for the Xfce panel")
    (description "Eyes is a toy Xfce panel plugin that adds eyes which
watch your every step.")
    (license gpl2+)))

(define-public xfce4-datetime-plugin
  (package
   (name "xfce4-datetime-plugin")
   (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-datetime-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-datetime-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1c00rj3h25g0g3ss5n07hp2ziis3rnjd5jr6firnhs2qzdwynbkb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-datetime-plugin")
    (synopsis "Display date and time inside the Xfce panel")
    (description "This plugin shows the date and time in the panel,
and a calendar appears when you left-click on it.")
    (license gpl2+)))

(define-public xfce4-calculator-plugin
  (package
   (name "xfce4-calculator-plugin")
   (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-calculator-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-calculator-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "12q2jh67w0m9vq94gf324xg9k3mji943rwgrddlc340sljz25xni"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-calculator-plugin")
    (synopsis "Calculator for the Xfce panel")
    (description "This plugin is a calculator for the Xfce4 panel.  It
supports common mathematical operators (+, -, *, /, ^) with usual
precedence rules, and the following functions and common constants.")
    (license gpl2+)))

(define-public xfce4-cpufreq-plugin
  (package
   (name "xfce4-cpufreq-plugin")
   (version "1.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-cpufreq-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-cpufreq-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1sbkjphrlyyqsmdggq2624qr45wkkrmvczpv04h5fpkjykcmir07"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-cpufreq-plugin")
    (synopsis "Xfce panel plugin for displaying CPU frequency")
    (description "This panel plugin shows information about the CPU
governor and frequencies supported and used by your system.")
    (license gpl2+)))

(define-public xfce4-diskperf-plugin
  (package
   (name "xfce4-diskperf-plugin")
   (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-diskperf-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-diskperf-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1jgcdwiaqs06l729vbj3kgv67iwimjp8gfy7ydzlvbx6859sc2ar"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-diskperf-plugin")
    (synopsis "Display disk performance in the Xfce panel")
    (description "This Xfce panel plugin displays instant disk/partition
performance (bytes transferred per second).")
    (license gpl2+)))

(define-public xfce4-fsguard-plugin
  (package
   (name "xfce4-fsguard-plugin")
   (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-fsguard-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-fsguard-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0n62dsc25ynv8kk5va50py88fi0lgggvl0gi1r6dd4i2fns8pvw4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-fsguard-plugin")
    (synopsis "Xfce panel plugin to monitor free disk space")
    (description "The panel plugin checks free space on a chosen mount
point frequently and displays a message when a limit is reached.  There
are two limits: a warning limit where only the icon changes, and an
urgent limit that advise the user with a message.  The icon button can
be clicked to open the chosen mount point.")
    (license bsd-2)))

(define-public xfce4-genmon-plugin
  (package
   (name "xfce4-genmon-plugin")
   (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-genmon-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-genmon-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0qh3b818kbf5sc07dshkd54nhqncsk0inlwv21zq8h11bzp0i3cl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-genmon-plugin")
    (synopsis "Generic program output monitor for the Xfce panel")
    (description "This plugin cyclically spawns the indicated
script/program, captures its output (stdout) and displays the
resulting string into the panel.

The string can also contain markup to displayed an image, a bar, a
button and a personalized tooltip.")
    (license gpl2+)))

(define-public xfce4-mailwatch-plugin
  (package
   (name "xfce4-mailwatch-plugin")
   (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-mailwatch-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-mailwatch-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0sh402c7v3sa9nqz8dd2gfn7ml01xbhir680ci46i9jczvln8j85"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list gtk+-2 libxfce4ui exo xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-mailwatch-plugin")
    (synopsis "Mail watch plugin for the Xfce panel")
    (description "The Xfce4 Mailwatch Plugin is a multi-protocol,
multi-mailbox mail watcher.  Currently, the protocols supported are:

@itemize
@item IMAP (SSL/TLS and cleartext, CRAM-MD5)
@item POP3 (SSL/TLS and cleartext, CRAM-MD5)
@item Mbox mail spool (local)
@item Maildir mail spool (local)
@item MH-Maildir mail spool (local)
@item Google Mail (GMail) mailbox (remote) (requires gnutls)
@end itemize")
    (license gpl2)))

(define-public xfce4-mpc-plugin
  (package
   (name "xfce4-mpc-plugin")
   (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-mpc-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-mpc-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0kb6nz3md3cacl7ys6gz4h2qailr050wp28b7qy2v66d396znrq4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://goodies.xfce.org/projects/panel-plugins/xfce4-mpc-plugin")
    (synopsis "Music Player Daemon plugin for the Xfce panel")
    (description "This is a simple client plugin for Music Player Daemon.

Features:
@itemize
@item send Play/Stop/Next/Previous command to MPD.
@item uses media icons names from icon-naming-spec (at least nuvola,
tango and rodent themes provides these icons)
@item decrease/increase volume using the mouse wheel.
@item show the current volume, status and title as a tooltip when
hovering the mouse over the plugin.
@item show a simple playlist window upon middle-click, permitting to
select a track to play
@item configurable MPD host/port/password.
@item toggles repeat/random features + enable/disable MPD outputs in
the right-click menu.
@item launch configurable client (gmpc, xterm -e ncmpc,..) through
right-click menu
@item configurable markup for tooltip and playlist, using a gmpc-like markup
@end itemize")
    (license isc)))

(define-public xfce4-mount-plugin
  (package
   (name "xfce4-mount-plugin")
   (version "1.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-mount-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-mount-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0ca8j2smq20zydj0gbb20fkcisgzcswpnpz5h8laxb3ghr03frlg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-mount-plugin")
    (synopsis "Mount/unmount plugin for the Xfce panel")
    (description "The plugin will display a list of items representing
your various devices.  If you click on an unmounted devices it will
mount it and vice versa.  There is a warning in case a device can't be
mounted or when unmounting fails.")
    (license gpl2+)))

(define-public xfce4-netload-plugin
  (package
   (name "xfce4-netload-plugin")
   (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-netload-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-netload-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0kmlrh29gn6yby8l1lgxp4211pjn3mrd2z0jzd5mh61fslx3mb4z"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-netload-plugin")
    (synopsis "Netload plugin for the Xfce Panel")
    (description "This plugin displays the current load of the network
interfaces of your choice in the panel.")
    (license gpl2+)))

(define-public xfce4-places-plugin
  (package
   (name "xfce4-places-plugin")
   (version "1.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-places-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-places-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0hy7c350x1p1grj517l1x0r8b4asbcv7sl3b5qnb0apj0dnhw7gi"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool desktop-file-utils pkg-config))
    (inputs
     (list gtk+-2 exo libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-places-plugin")
    (synopsis "Gnome-like Places menu for the Xfce panel")
    (description "This plugin provides a menu with quick access to folders,
documents, and removable media.  The places plugin brings much of the
functionality of GNOME's Places menu to Xfce.

The plugin puts a simple button on the panel.  Clicking on this button
opens up a menu with the following:

@itemize
@item System-defined directories (home folder, trash, desktop, file system)
@item Removable media (using thunar-vfs)
@item User-defined bookmarks (reads @file{~/.gtk-bookmarks})
@item Search program launcher (optional)
@item Recent documents submenu
@end itemize")
    (license gpl2+)))

(define-public xfce4-smartbookmark-plugin
  (package
   (name "xfce4-smartbookmark-plugin")
   (version "0.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-smartbookmark-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-smartbookmark-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1lyd64qc9w6qnpqjb5xk0mjq4l7riv6z7l9aws28clalb8prw9ra"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-smartbookmark-plugin")
    (synopsis "Perform custom searches in your browser from the Xfce panel")
    (description "This plugin allows you to send search requests
directly to your browser, such that you can search through your
favorite search engine or bug tracker right from the Xfce panel.")
    (license gpl2+)))

(define-public xfce4-statusnotifier-plugin
  (package
   (name "xfce4-statusnotifier-plugin")
   (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-statusnotifier-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-statusnotifier-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1d2n56g12dhnjznrq7xvr6d3brpp0lmm080xmgjb7ybc1yygpxrc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config
           `(,glib "bin")))
    (inputs
     (list libxfce4ui libdbusmenu xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-statusnotifier-plugin")
    (synopsis "Xfce panel plugin for status notifier items")
(description "This plugin provides a panel area for status
notifier items (application indicators).  Applications may use these
items to display their status and interact with the user.  This
technology is a modern alternative to systray and follows the
freedesktop.org specification.")
    (license gpl2+)))

(define-public xfce4-stopwatch-plugin
  (package
   (name "xfce4-stopwatch-plugin")
   (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-stopwatch-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-stopwatch-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1q840298jzdqlhc9lw49q32xzdhnbzcgvv69qq5slkc704s5w6vw"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-stopwatch-plugin")
    (synopsis "Stopwatch plugin for the Xfce panel")
    (description "This Xfce panel plugin keeps track of elapsed time.")
    (license bsd-2)))

(define-public xfce4-systemload-plugin
  (package
   (name "xfce4-systemload-plugin")
   (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-systemload-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-systemload-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0wmh09w5fnm9srbr6r4gpdima738szqcp3qbzb8kl18f0b1kyc5v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libgtop libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-systemload-plugin")
    (synopsis "System load display plugin for the Xfce panel")
    (description "A system load plugin for the Xfce4 desktop
environment.  It displays the current CPU load, the memory in use, the
swap space and the system uptime in the Xfce4 panel.")
    (license bsd-2)))

(define-public xfce4-time-out-plugin
  (package
   (name "xfce4-time-out-plugin")
   (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-time-out-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-time-out-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1s4f4akj45bjhqhnfb8wim2snw52wrdym1yqpcccfpp9c5is672s"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list gtk+ libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-time-out-plugin")
    (synopsis "Xfce panel plugin that encourages periodical breaks")
    (description "This plugin encourages to take periodical
breaks from the computer every X minutes.  During breaks it locks your
screen.  It optionally allows you to postpone breaks for a certain
time.")
    (license gpl2+)))

(define-public xfce4-timer-plugin
  (package
   (name "xfce4-timer-plugin")
   (version "1.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-timer-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-timer-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0zcbqpsyzzmbyv8cgd5wriqaigfpdcia6h1md0bfh1cmsg1biczy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-timer-plugin")
    (synopsis "Simple countdown and alarm plugin for the Xfce panel")
    (description "This is a simple plugin that lets the user run an
alarm at a specified time or at the end of a specified countdown
period.")
    (license gpl2+)))

(define-public xfce4-verve-plugin
  (package
   (name "xfce4-verve-plugin")
   (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-verve-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-verve-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "17p0kxnzx4ks3rs281x5pvwksl1gh2mg7h7mkccjmkzr3cgi5gz1"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui pcre2 xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-verve-plugin")
    (synopsis "Command line for the Xfce panel")
    (description "The Verve plugin provides a comfortable command line
for the Xfce panel.  It supports several features, such as:
@itemize
@item Opens URLs, e-mail addresses, directories, and programs
@item Command history
@item Auto-completion (including command history)
@item Focus grabbing via D-BUS (so you can bind a shortcut to it)
@item Custom input field width
@end itemize")
    (license gpl2+)))

(define-public xfce4-wavelan-plugin
  (package
   (name "xfce4-wavelan-plugin")
   (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-wavelan-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-wavelan-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0azpv0s3r4ag3gp0bsfvq0jgzycx6ivdsw5p0ga7425pdksw5h31"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-wavelan-plugin")
    (synopsis "Show stats from WLAN interface in Xfce panel")
    (description "This plugin is used to display stats from a wireless
lan interface (signal state, signal quality, network name (SSID)).")
    (license bsd-2)))

(define-public xfce4-weather-plugin
  (package
   (name "xfce4-weather-plugin")
   (version "0.11.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/panel-plugins/"
                                  "xfce4-weather-plugin/"
                                  (version-major+minor version)
                                  "/xfce4-weather-plugin-" version ".tar.bz2"))
              (sha256
               (base32
                "0sw7p8xsgyc7b5w92abigqz9mii79w2vdlprm5c0hmb3g3zhmm35"))))
    (build-system gnu-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list gtk+ json-c libsoup-minimal-2 libxfce4ui libxml2 xfce4-panel))
    (home-page
     "https://goodies.xfce.org/projects/panel-plugins/xfce4-weather-plugin")
    (synopsis "Show information about local weather in the Xfce panel")
    (description "This Xfce panel plugin shows information about your
local weather in the panel, using forecast data provided by the
@uref{https://met.no, Norwegian Meteorological Institute}.")
    (license gpl2+)))

(define-public xfce4-dev-tools
  (package
    (name "xfce4-dev-tools")
    (version "4.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.xfce.org/src/xfce/"
                                  "xfce4-dev-tools/" (version-major+minor version) "/"
                                  "xfce4-dev-tools-" version ".tar.bz2"))
              (sha256
               (base32
                "10bnb8q7sj60ahzfwrb3av4ngr17wk1p6jsnfv0yn8l90kksnb41"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           libxslt))
    (inputs
     (list glib))
    (home-page "https://docs.xfce.org/xfce/xfce4-dev-tools/")
    (synopsis "Xfce developer tools")
    (description
     "The Xfce development tools are a collection of tools and macros for Xfce
developers and people that want to build Xfce from Git In addition it contains
the Xfce developer's handbook.")
    (license gpl2+)))
