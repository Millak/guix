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
;;; Copyright © 2025 Tomáš Čech <sleep_walker@gnu.org>
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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages docbook)
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
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
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
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r24hx200jvixn8rhcg0cbvv6b3jc4hj1iw2bkvmrcf74m4ck9nj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-maintainer-mode" ;for libxfce4util-visibility.c
              "--enable-gtk-doc")))
    (native-inputs (list gobject-introspection
                         vala
                         xfce4-dev-tools))
    (propagated-inputs (list glib)) ;required by libxfce4util-1.0.pc
    (home-page "https://docs.xfce.org/xfce/libxfce4util/")
    (synopsis "Basic utility library for Xfce")
    (description
     "A general-purpose utility library with core application support for the
Xfce Desktop Environment.")
    (license lgpl2.0+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfconf
  (package
    (name "xfconf")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/xfconf")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k4d2gg77p3jdr0rankz2mv50hy7ddf5xl32si1mdby1wvpa9r2k"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-gtk-doc")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda _
              (setenv "HOME"
                      (getenv "TMPDIR")) ;xfconfd requires a writable HOME
              ;; For the missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0")
              ;; Run test-suite under a dbus session.
              (invoke "dbus-launch" "make" "check"))))))
    (native-inputs (list dbus
                         gobject-introspection
                         vala
                         xfce4-dev-tools))
    (propagated-inputs
     ;; libxfconf-0.pc refers to all these.
     (list glib))
    (inputs (list libxfce4util))
    (home-page "https://docs.xfce.org/xfce/xfconf/")
    (synopsis "Configuration storage and query system for Xfce")
    (description
     "Settings daemon for Xfce, implemented as a D-Bus-based configuration
storage system.")
    (license lgpl2.0+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public libxfce4ui
  (package
    (name "libxfce4ui")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/libxfce4ui")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ps8sq8g43dx12qp0shrdb45bjrfhhgkziscj5jnrzfhy6j9mqrk"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--with-vendor-info=GNU Guix"
                   "--enable-maintainer-mode" ; for about-dialog-ui.h, etc.
                   "--enable-gtk-doc")))
    (native-inputs (list gobject-introspection xfce4-dev-tools))
    (propagated-inputs (list
                        ;; required by libxfce4ui-2.pc
                        gtk+
                        ;; libxfce4kbd-private-3.pc refers to all these.
                        libxfce4util xfconf))
    (inputs (list libgtop libice libsm startup-notification))
    (home-page "https://docs.xfce.org/xfce/libxfce4ui/")
    (synopsis "Widgets library for Xfce")
    (description
     "Libxfce4ui is the replacement of the old libxfcegui4 library.  It is used
to share commonly used Xfce widgets among the Xfce applications.")
    (license lgpl2.0+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public catfish
  (package
    (name "catfish")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/apps/catfish")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vyf62n8j2pgxd30j8hf1x6d0yz8r86ng39p9smfpq7m3vll8i7c"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-command-paths
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* "catfish/CatfishSearchEngine.py"
                         (("'which'")
                          (string-append "'"
                                         (which "which") "'")))
                       (substitute* "catfish/CatfishWindow.py"
                         (("xdg-mime")
                          (which "xdg-mime"))
                         (("xdg-open")
                          (which "xdg-open")))))
                   (add-after 'install 'wrap-program
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (wrap-program (string-append out "/bin/catfish")
                           `("GUIX_PYTHONPATH" =
                             (,(getenv "GUIX_PYTHONPATH")))
                           `("GI_TYPELIB_PATH" =
                             (,(getenv "GI_TYPELIB_PATH"))))))))
      #:tests? #f))
    (native-inputs (list desktop-file-utils ;for update-desktop-database
                         gettext-minimal
                         (list gtk+ "bin") ;for gtk-update-icon-cache
                         pkg-config
                         python))
    (inputs (list bash-minimal which xfconf xdg-utils))
    (propagated-inputs (list gtk+ python-dbus python-pexpect python-pycairo
                             python-pygobject))
    (home-page "https://docs.xfce.org/apps/catfish/")
    (synopsis "File searching tool for Xfce")
    (description
     "Catfish is a file searching tool for Linux and Unix.  The interface is
intentionally lightweight and simple, using only GTK+ 3.  You can configure
it to your needs by using several command line options.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public elementary-xfce-icon-theme
  (package
    (name "elementary-xfce-icon-theme")
    (version "0.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shimmerproject/elementary-xfce")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vv0fsi8myyhxc0miaphhkl4w8g0zkbirrarclbxpahp1pmxw3g1"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no check target
      #:make-flags #~(list "CC=gcc")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'make-git-checkout-writable
                     (lambda _
                       (for-each make-file-writable
                                 (find-files ".")) #t)))))
    (native-inputs (list gtk+ optipng pkg-config))
    (home-page "https://shimmerproject.org/")
    (synopsis "Elementary icons extended and maintained for Xfce")
    (description
     "This is a fork of the upstream elementary project. This icon
theme is supposed to keep everything working for Xfce, but gets updates from
upstream occasionally.")
    (license gpl3+)))

(define-public exo
  (package
    (name "exo")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/exo")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zbj5q0ih5kvxvays4ajjsmxm2938jmn4648062agxxjl8asqlcs"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-maintainer-mode" ;for exo-enum-types.c, etc.
              "--enable-gtk-doc")))
    (native-inputs (list docbook-xsl
                         libxslt
                         xfce4-dev-tools))
    (propagated-inputs
     ;; exo-2.pc refers to all these.
     (list gtk+ libxfce4util))
    (inputs (list libxfce4ui))
    (home-page "https://docs.xfce.org/xfce/exo/")
    (synopsis "Extension library for Xfce")
    (description
     "An extension library to Xfce.  While Xfce comes with quite a few libraries
that are targeted at desktop development, libexo is targeted at application
development.")
    ;; Libraries are under LGPLv2+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public garcon
  (package
    (name "garcon")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/garcon")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "076pdyssl5lhm88s2xx94w3rk6glcc4kgfl3jqd6704hpl6n9rii"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-gtk-doc")))
    (native-inputs (list docbook-xsl
                         gobject-introspection
                         libxslt
                         xfce4-dev-tools))
    (propagated-inputs (list gtk+ ;required by garcon-gtk3-1.pc
                             libxfce4ui)) ;required by garcon-gtk3-1.pc
    (home-page "https://docs.xfce.org/xfce/garcon/")
    (synopsis "Implementation of the freedesktop.org menu specification")
    (description
     "Garcon is a freedesktop.org compliant menu implementation based on
GLib and GIO.  It was started as a complete rewrite of the former Xfce menu
library called libxfce4menu, which, in contrast to garcon, was lacking menu
merging features essential for loading menus modified with menu editors.")
    (license lgpl2.0+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public tumbler
  (package
    (name "tumbler")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/tumbler")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18bp1ckv6mzbxhbk7aqp2mxbni1pw8hv8rzxmc9xx488dxs0qq8s"))))
    (build-system gnu-build-system)
    (native-inputs (list xfce4-dev-tools))
    (propagated-inputs (list glib)) ;required by tumbler-1.pc
    (inputs (list dbus
                  gdk-pixbuf
                  cairo ;Needed for pdf thumbnails (poppler-glibc.pc)
                  freetype
                  libjpeg-turbo
                  libgsf
                  libxfce4util
                  poppler
                  ;; FIXME Provide gstreamer and gstreamer-tag to get video thumbnails
                  ;; ("gstreamer" ,gstreamer)
                  ))
    (home-page "https://docs.xfce.org/xfce/tumbler/")
    (synopsis "D-Bus service for applications to request thumbnails")
    (description
     "Tumbler is a D-Bus service for applications to request thumbnails for
various URI schemes and MIME types.  It is an implementation of the thumbnail
management D-Bus specification.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public libxfce4windowing
  (package
    (name "libxfce4windowing")
    (version "4.20.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/libxfce4windowing")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "151bs8rf0q3cln28lla5yk254dr75b508a0611crfavqrsrn23az"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-gtk-doc")))
    (native-inputs (list xfce4-dev-tools))
    (propagated-inputs (list gtk+)) ;required by libxfce4windowing-0.pc
    (inputs (list libdisplay-info
                  libwnck
                  libxrandr
                  wayland
                  wayland-protocols
                  wlr-protocols))
    (home-page "https://docs.xfce.org/xfce/libxfce4windowing/")
    (synopsis "Windowing concept abstraction library for X11 and Wayland")
    (description
     "Libxfce4windowing is an abstraction library that attempts to present
windowing concepts (screens, toplevel windows, workspaces, etc.) in a
windowing-system-independent manner.")
    (license lgpl2.1+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-panel
  (package
    (name "xfce4-panel")
    (version "4.20.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/xfce4-panel")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fmybf6caqdqgw68z7nn4c7a6wxn896niw50m8zf3div8d9s7ddl"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-maintainer-mode" ;for panel-marshal.h, etc
              "--enable-gtk-doc")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'fix-tzdata-path
                     (lambda* (#:key inputs #:allow-other-keys)
                       (substitute* (string-append "plugins/clock/clock.c")
                         (("/usr/share/zoneinfo")
                          (search-input-directory inputs "share/zoneinfo"))))))))
    (native-inputs (list xfce4-dev-tools))
    (propagated-inputs (list gtk+ ;required by libxfce4panel-2.0.pc
                             libxfce4util)) ;required by libxfce4panel-2.0.pc
    (inputs (list tzdata ;For fix-tzdata-path phase only.
                  exo
                  xfconf
                  garcon
                  gtk-layer-shell
                  libwnck
                  libxfce4ui
                  libxfce4windowing))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (home-page "https://docs.xfce.org/xfce/xfce4-panel/")
    (synopsis "Xfce desktop panel")
    (description
     "Desktop panel for Xfce, which contains program launchers, window buttons,
applications menu, workspace switcher and more.")
    ;; Libraries are under LGPLv2.1+, and programs under GPLv2+.
    (license (list gpl2+ lgpl2.1+))
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-battery-plugin
  (package
    (name "xfce4-battery-plugin")
    (version "1.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/panel-plugins/xfce4-battery-plugin")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bd912j22pf6rmqvkc80g5axjil88pbzxqa68krw65l11v73icmm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-maintainer-mode")))
    (native-inputs (list xfce4-dev-tools))
    (inputs (list glib gtk+ libxfce4util libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-battery-plugin/")
    (synopsis "Battery monitor panel plugin for Xfce4")
    (description
     "A battery monitor panel plugin for Xfce4, compatible with APM and ACPI.")
    ;; The main plugin code is covered by gpl2+, but the files containing code
    ;; to read the battery state via ACPI or APM are covered by lgpl2.0+.
    (license (list gpl2+ lgpl2.0+))
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-clipman-plugin
  (package
    (name "xfce4-clipman-plugin")
    (version "1.6.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/panel-plugins/xfce4-clipman-plugin")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0spg9mdlcx98kjisv0c9axp7knhh8am5dqfa2lspj1jbgvah1dwi"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-maintainer-mode")))
    (native-inputs (list xfce4-dev-tools))
    (inputs (list exo libxfce4ui libxtst wlr-protocols xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-clipman-plugin/")
    (synopsis "Clipboard manager for Xfce")
    (description
     "Clipman is a clipboard manager for Xfce.  It keeps the clipboard contents
around while it is usually lost when you close an application.  It is able to
handle text and images, and has a feature to execute actions on specific text by
matching them against regular expressions.")
    (license (list gpl2+))
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-pulseaudio-plugin
  (package
    (name "xfce4-pulseaudio-plugin")
    (version "0.4.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url
              "https://gitlab.xfce.org/panel-plugins/xfce4-pulseaudio-plugin")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hmajys3g56xlpja9hx0rvap54bw0g6vmirh068zn5004wg7i6kc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-maintainer-mode")))
    (native-inputs (list dbus
                         dbus-glib
                         xfce4-dev-tools))
    (inputs (list exo
                  keybinder
                  libnotify
                  libxfce4ui
                  pulseaudio
                  xfce4-panel))
    (home-page
     "https://docs.xfce.org/panel-plugins/xfce4-pulseaudio-plugin/")
    (synopsis "PulseAudio panel plugin for Xfce")
    (description
     "Xfce PulseAudio plugin is a plugin for the Xfce panel which provides a
convenient way to adjust the audio volume of the PulseAudio sound system and
to an auto mixer tool like pavucontrol.  It can optionally handle multimedia
keys for controlling the audio volume.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-i3-workspaces-plugin
  (package
    (name "xfce4-i3-workspaces-plugin")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/denesb/xfce4-i3-workspaces-plugin")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l957xzs479mnl1la6lx3ysaiqc0z8l84jg442wif9k8x9z6iah8"))))
    (build-system gnu-build-system)
    (native-inputs (list xfce4-dev-tools))
    (inputs (list i3ipc-glib libxfce4ui xfce4-panel))
    (home-page "https://github.com/denesb/xfce4-i3-workspaces-plugin")
    (synopsis "Xfce panel workspace switcher plugin for the i3 window manager")
    (description
     "This plugin is designed for switching workspaces and displaying which screen is
currently visible, as well as indicating which workspace has a window requiring
attention.  Although it is intended for the Xfce4 panel, it is custom-built to work
seamlessly with the tiling window manager i3.")
    (license gpl3+)))

(define-public xfce4-whiskermenu-plugin
  (package
    (name "xfce4-whiskermenu-plugin")
    (version "2.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0alxsb1lzlpzwpy4ra26n16inz906r2ssdqjrq6jjwh5gw44lz08"))))
    (build-system cmake-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list accountsservice
           xfce4-panel
           garcon
           exo
           gtk-layer-shell
           libxfce4ui))
    (arguments
     (list #:tests? #f))                ; no tests
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-whiskermenu-plugin/")
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
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iai2myz1zj5vvbl3lz8r50bhv103avfjdc233gqa933wwxgwvd0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list garcon
           (librsvg-for-system)
           libwnck
           libx11
           libxfce4ui
           libxklavier
           xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-xkb-plugin/")
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
    (license bsd-2)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-appfinder
  (package
    (name "xfce4-appfinder")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1irqbabdgj5ybns55g92548jcr0i1k4q0c7s8jn17r7g8ygd12qy"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for appfinder-preferences-ui.h
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list garcon gtk+ libxfce4ui))
    (home-page "https://docs.xfce.org/xfce/xfce4-appfinder/")
    (synopsis "Xfce application finder")
    (description
     "Application finder for Xfce, it will show the applications installed on
your system in categories, so you can quickly find and launch them.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-session
  (package
    (name "xfce4-session")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zzmszwm22nnwybagnvan62qbqcgcr7fnp288qcr9bkkag5y8zcs"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute* "libxfsm/xfsm-shutdown-common.h"
             (("/sbin/shutdown -h now")  "halt")
             (("/sbin/shutdown -r now")  "restart")
             (("/usr/sbin/pm-suspend")   "pm-suspend")
             (("/usr/sbin/pm-hibernate") "pm-hibernate"))
           #t))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-maintainer-mode" ;for xfce4-session-settings_ui.h
              (string-append "--with-xsession-prefix=" #$output)
              (string-append "--with-wayland-session-prefix=" #$output))
       ;; Disable icon cache update.
       #:make-flags
       #~(list "gtk_update_icon_cache=true")
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'install 'patch-command-paths
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out"))
                     (search-command (lambda (cmd)
                                       (search-input-file
                                        inputs
                                        (string-append "bin/" cmd)))))
                 (substitute* (string-append out "/bin/xflock4")
                   (("gdbus call")
                    (string-append (search-command "gdbus") " call")))
                 (substitute* (string-append out "/etc/xdg/xfce4/xinitrc")
                   (("[|] xrdb")
                    (string-append "| " (search-command "xrdb")))
                   (("&& xmodmap")
                    (string-append "&& " (search-command "xmodmap"))))))))))
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list iceauth
           gtk-layer-shell
           upower
           polkit
           libsm
           libwnck
           libxfce4ui
           libxfce4windowing
           (list glib "bin")
           xmodmap
           xrdb))
    (home-page "https://docs.xfce.org/xfce/xfce4-session/")
    (synopsis "Xfce session manager")
    (description
     "Session manager for Xfce, it will restore your session on startup and
allows you to shut down the computer from Xfce.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-settings
  (package
    (name "xfce4-settings")
    (version "4.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hk8jsiczpzvm7zs79g5jk6amg988rg1w1ir4z3x0yklqgalw4gl"))
       (patches (search-patches "xfce4-settings-defaults.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode" ;for appearance-dialog_ui.h
                   "--enable-pluggable-dialogs"
                   "--enable-sound-settings"
                   "--enable-upower-glib"
                   "--enable-xrandr")))
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list colord
           exo
           garcon
           gtk-layer-shell
           libnotify
           libxcursor
           libxi
           libxklavier
           libxrandr
           libxfce4ui
           upower
           python ;; for xfce4-compose-mail
           wlr-protocols
           xf86-input-libinput))
    (propagated-inputs
     ;; Some operations, such as changing icon themes, require these schemas
     ;; to be in the search path.
     (list gsettings-desktop-schemas))
    (home-page "https://docs.xfce.org/xfce/xfce4-settings/")
    (synopsis "Xfce settings manager")
    (description
     "Settings manager for Xfce, it can control various aspects of the desktop
like appearance, display, keyboard and mouse settings.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunar
  (package
    (name "thunar")
    (version "4.20.2")                           ;stable version = even minor
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12qgg4n92v4h9k4acpm4a40gfv3vk2w9hrcwa301bm3bkqh0vqmn"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags '("--enable-maintainer-mode" ;for thunar-marshal.c
                           "--enable-gtk-doc"
                           "--with-custom-thunarx-dirs-enabled")
       ;; XXX: abicheck.sh FAIL, will be fixed in next version.
       ;; See <https://gitlab.xfce.org/xfce/thunar/-/issues/1516>.
       #:tests? #f))
    (native-inputs
     (list docbook-xsl libxslt xfce4-dev-tools))
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
    (home-page "https://docs.xfce.org/xfce/thunar/")
    (synopsis "Xfce file manager")
    (description
     "A modern file manager for graphical desktop, aiming to be easy-to-use and
fast.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunar-volman
  (package
    (name "thunar-volman")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14cs5hscjrgxvmkxr1d5ank76rxwq7ifvm3g844p9jvhykz6r1aw"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list exo libgudev libnotify libxfce4ui))
    (home-page "https://docs.xfce.org/xfce/thunar-volman/")
    (synopsis "Removable media manager for Thunar")
    (description
     "Thunar-volman is an extension for the Thunar File Manager, which enables
automatic management of removable drives and media.  For example, if
thunar-volman is installed and configured properly, and you plug in your
digital camera, it will automatically spawn your preferred photo application
and import the new pictures from your camera.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunar-archive-plugin
  (package
    (name "thunar-archive-plugin")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/thunar-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1aqvfgn2www5m9764ldafcv5wsknhsdy2hibikzxhpbzd51c8j7l"))))
    (build-system gnu-build-system)
    (native-inputs (list xfce4-dev-tools))
    (inputs (list exo thunar gtk+))
    (home-page "https://docs.xfce.org/xfce/thunar/archive")
    (synopsis "Archive plugin for Thunar file manager")
    (description "The Thunar Archive Plugin allows you to create and extract
archive files using the file context menus in the Thunar file manager.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunar-shares-plugin
  (package
    (name "thunar-shares-plugin")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/thunar-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m4xdmfs6zcxsq96p0wgwgqv2av7bqqsbpsrbqkq78mrmn9533nl"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf-2.71 xfce4-dev-tools))
    (inputs (list thunar gtk+ xfconf))
    (home-page "https://docs.xfce.org/xfce/thunar/thunar-shares-plugin")
    (synopsis "Folder share plugin for Thunar file manager")
    (description
     "The Thunar Shares Plugin allows you to quickly share a folder using
Samba from Thunar (the Xfce file manager) without requiring root access.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunar-media-tags-plugin
  (package
    (name "thunar-media-tags-plugin")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/thunar-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "091i975aazkhkxyh0h0msyqkkr1z2dfy068syaawcisyaw1h2mpg"))))
    (build-system gnu-build-system)
    (native-inputs (list xfce4-dev-tools))
    (inputs (list exo gtk+ thunar taglib))
    (home-page "https://docs.xfce.org/xfce/thunar/media-tags")
    (synopsis "Media tags plugin for Thunar file manager")
    (description
     "Media tags plugin allows tags editing from Thunar file manager and
tags-based file renaming from inside Thunar Bulk Renamer.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunar-vcs-plugin
  (package
    (name "thunar-vcs-plugin")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/thunar-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n07d8v7spn2ys1nz9yb5szj9jkd1y7frzq203v5ys9gifa7mnvv"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "CPPFLAGS=-I"
                                  #$(this-package-input "apr-util")
                                  "/include/apr-1"))))
    (native-inputs (list xfce4-dev-tools utf8proc))
    (inputs
     (list exo
           gtk+
           thunar
           libxfce4util
           apr
           apr-util
           subversion
           git))
    (home-page "https://docs.xfce.org/xfce/thunar/thunar-vcs-plugin")
    (synopsis "VCS plugin for Thunar file manager")
    (description
     "Thunar VCS Plugin (formerly known as Thunar SVN Plugin) gives SVN and
GIT integration to Thunar, it adds Subversion and GIT actions to the context
menu.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public thunarx-python
  (package
    (name "thunarx-python")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/bindings/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x39pbdx4186f7bkrc9ab5q7qs783mivm2j1msyf9m1nh09y617f"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
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
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfwm4
  (package
    (name "xfwm4")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "153zfjw6z9nvlw05xjjws9f95097qci5qbxpzf33z8zl06n50ip5"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for xfwm4-dialog_ui.h, etc.
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libdrm
           libwnck
           libxcomposite
           libxdamage
           libxfce4ui
           libxpresent
           libxrandr))
    (home-page "https://docs.xfce.org/xfce/xfwm4/")
    (synopsis "Xfce window manager")
    (description
     "Window manager for Xfce, it handles the placement of windows
on the screen.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

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
    (version "4.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13kzh63dskdl5ayzza8a9db40g16sfzzh0aq7vy6hk0xf4fyq720"))
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
                                '( ;; "backgrounds/xfce-blue.jpg"
                                  "backgrounds/xfce-stripes.svg"
                                  "backgrounds/xfce-teal.svg"
                                  "backgrounds/xfce-verticals.svg"))
                      #t)))

       #:disallowed-references (,inkscape/pinned ,imagemagick)))
    (native-inputs
     (list xfce4-dev-tools
           ;; For our own ‘prepare-background-image’ phase.
           inkscape/pinned imagemagick))
    (inputs
     (list exo
           garcon
           gtk-layer-shell
           libnotify
           libwnck
           libxfce4ui
           libxfce4windowing
           libyaml
           thunar))
    (home-page "https://docs.xfce.org/xfce/xfdesktop/")
    (synopsis "Xfce desktop manager")
    (description
     "Desktop manager for Xfce, it sets the background color or image with
optional application menu or icons for minimized applications or launchers,
devices and folders.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public gigolo
  (package
    (name "gigolo")
    (version "0.5.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1688j4c7d1vzglx9a8a32gy17yfqwrha7p0r272hrhz009jza6w1"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for gigolo_ui.h
    (native-inputs (list xfce4-dev-tools))
    (inputs (list gtk+))
    (home-page "https://docs.xfce.org/apps/gigolo/")
    (synopsis "Manage connections to remote file systems")
    (description
     "Gigolo is a graphical user interface to easily manage connections to
remote file systems using GIO/GVfs.  It allows you to quickly connect/mount
local and remote file systems and manage bookmarks of such.")
    (license gpl2)                                ;version 2 only
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public parole
  (package
    (name "parole")
    (version "4.18.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yzqry3pbgn413nflw7a8cm3xdq96w5pj41b04p5drr7qs44d1qb"))))
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
     (list gobject-introspection xfce4-dev-tools))
    (inputs
     (list bash-minimal                           ;for 'wrap-program'
           dbus-glib
           gstreamer
           gst-plugins-base
           gst-plugins-good
           libnotify
           libxfce4ui
           libxfce4util
           taglib))
    (home-page "https://docs.xfce.org/apps/parole/")
    (synopsis "Media player based on the GStreamer framework")
    (description "Parole is a modern simple media player based on the
GStreamer framework and written to fit well in the Xfce desktop.  Parole
features playback of local media files, DVD/CD and live streams.")
    (license gpl2)                      ;version 2 only
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-terminal
  (package
    (name "xfce4-terminal")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16rcp2f6wh8vdzppkv2xgqlqffg85azi3vdvl90xn2r4ixyrzfas"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for xfce4-terminal.1
    (native-inputs
     (list docbook-xsl libxslt xfce4-dev-tools))
    (inputs
     (list libxfce4ui vte/gtk+-3))
    (home-page "https://docs.xfce.org/apps/xfce4-terminal/")
    (synopsis "Xfce terminal emulator")
    (description
     "A lightweight and easy to use terminal emulator for Xfce.  Features
include a simple configuration interface, the ability to use multiple tabs
with terminals within a single window, the possibility to have a
pseudo-transparent terminal background, and a compact mode (where both the
menubar and the window decorations are hidden) that helps you to save space
on your desktop.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-dict
  (package
    (name "xfce4-dict")
    (version "0.8.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y9ggn3c0ngn621ljmb2ahavr27sksld48z2qs470zcjwiw48nlx"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/apps/xfce4-dict/")
    (synopsis "Dictionary of Xfce desktop")
    (description
     "Xfce4-dict allows you to search different kinds of dictionary services
for words or phrases and shows you the result.  Currently you can query a Dict
server (RFC 2229), any online dictionary service by opening a web browser or
search for words using the aspell/ispell program.

xfce4-dict contains a stand-alone application called “xfce4-dict” and a panel
plugin for the Xfce panel.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfdashboard
  (package
    (name "xfdashboard")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/"
                                 name))
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1blydl57xzqndcj1dkfxbmsaad27s7mqwwj3mmidz2dx9cikabl8"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for enums.c, etc.
    (native-inputs (list xfce4-dev-tools))
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
    (home-page "https://docs.xfce.org/apps/xfdashboard/")
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
           xfce4-screensaver
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
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/xfce/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "089qv5704y5dk2jddl3rij4d591q5i2dqlv8gb6vr250pyp1v9d8"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for xfpm-dbus-marshal.h, etc.
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxrandr gtk+ upower libnotify libxfce4ui polkit
           wlr-protocols xfce4-panel))
    (home-page "https://docs.xfce.org/xfce/xfce4-power-manager/")
    (synopsis "Xfce Power Manager")
    (description
     "This is a power manager for the Xfce desktop.  It manages the power
sources on the computer and the devices that can be controlled to reduce their
power consumption (such as LCD brightness level, monitor sleep, CPU frequency
scaling, etc).  In addition, xfce4-power-manager provides a set of
freedesktop-compliant DBus interfaces to inform other applications about current
power level so that they can adjust their power consumption, and it provides the
inhibit interface which allows applications to prevent automatic sleep.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public ristretto
  (package
    (name "ristretto")
    (version "0.13.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bbcq5spqirh21p0s7vy9na58inz47nsj59asdka35qnvr20g4vh"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for main_window_ui.h, etc.
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list gtk+ libexif libxfce4ui tumbler))
    (home-page "https://docs.xfce.org/apps/ristretto/start")
    (synopsis "Fast and lightweight picture-viewer")
    (description
     "The Ristretto Image Viewer is an application that can be used to view,
and scroll through images.  It can be used to run a slideshow of images, open
images with other applications like an image-editor or configure an image as
the desktop wallpaper.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-taskmanager
  (package
    (name "xfce4-taskmanager")
    (version "1.5.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dmfbxnnyfv6n55krvjmgx8niv96xkpsf3il0bdmk928hzazhqh3"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for process-window_ui.h, etc.
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libwnck
           libxmu
           gtk+
           libxfce4ui
           ;; FIXME: Remove libxext and libxt when libxmu propagates them.
           libxext
           libxt))
    (home-page "https://docs.xfce.org/apps/xfce4-taskmanager/")
    (synopsis "Easy to use task manager")
    (description
     "This is a task manager for the Xfce desktop.  It displays the CPU and
memory usage graphically, and it can display processes as a tree.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public orage
  (package
    (name "orage")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z32b2wg2pfwcvpp81dmdlnwqxnh7ps08mr13syzl7v0qj29p9jm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list dbus-glib gtk+-2 libical libnotify libxfce4ui popt xfce4-panel))
    (home-page "https://docs.xfce.org/apps/orage/")
    (synopsis "Simple calendar application with reminders")
    (description
     "This is a simple calendar application for the Xfce desktop.  Orage has
alarms and uses the iCalendar format, making it compatible with many other
calendar applications.  It also includes a panel clock plugin and an
international clock application capable of simultaneously showing clocks from
several different time zones.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-notifyd
  (package
    (name "xfce4-notifyd")
    (version "0.9.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bj34lkasrrnbgr5gnk8yvhxxypi4n0p3gq20w766f4rg75nh1x6"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list gtk-layer-shell libcanberra libxfce4ui libnotify sqlite xfce4-panel))
    (home-page "https://docs.xfce.org/apps/xfce4-notifyd/")
    (synopsis "Show notification bubbles on Xfce")
    (description
     "The Xfce Notify Daemon (xfce4-notifyd for short) is a smallish program
that implements the “server-side” portion of the Freedesktop desktop
notifications specification.  Applications that wish to pop up a notification
bubble in a standard way can implicitly make use of xfce4-notifyd to do so by
sending standard messages over D-Bus using the
@code{org.freedesktop.Notifications} interface.")
    (license gpl2)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfburn
  (package
    (name "xfburn")
    (version "0.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18i25n81qzgb77w5vc6n4hwnw893204az8c1bg5k9229sz28r7vq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for xfburn.1
    (native-inputs
     (list docbook-xsl libxslt xfce4-dev-tools))
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
    (home-page "https://docs.xfce.org/apps/xfburn/")
    (synopsis "GTK+ based CD, DVD and Blu-ray burning application")
    (description
     "Xfburn is a simple CD, DVD, and Blu-ray burning tool based on
the libburnia libraries.  It can blank CD/DVD/BD(-RW)s, burn and
create iso images, audio CDs, as well as burn personal compositions
of data to either CD/DVD/BD.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public mousepad
  (package
    (name "mousepad")
    (version "0.6.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pchp4jdy7xfgb0bk4pv06bphs6lmf1lr3ykyq2f351s5wqp2nrg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-maintainer-mode" ;for mousepad-marshal.c
                           ;; Use the GSettings keyfile backend rather than
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
     (list xfce4-dev-tools))
    (inputs
     (list bash-minimal gspell gtksourceview-4 libxfce4ui xfconf))
    (home-page "https://docs.xfce.org/apps/mousepad/")
    (synopsis "Simple text editor for Xfce")
    (description
     "Mousepad is a graphical text editor for Xfce based on Leafpad.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-screenshooter
  (package
   (name "xfce4-screenshooter")
   (version "1.11.1")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url (string-append "https://gitlab.xfce.org/apps/" name))
            (commit (string-append name "-" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1wxgc84f5kgs896mjpn2sq7ikh3gpfyilpqfarmlzpmpmmhgvppw"))))
   (build-system gnu-build-system)
   (native-inputs
    (list xfce4-dev-tools))
   (inputs
    (list exo libsoup-minimal-2 libxfce4ui wlr-protocols xfce4-panel))
   (home-page "https://docs.xfce.org/apps/xfce4-screenshooter/")
   (synopsis "Xfce's application to take screenshots")
   (description
    "This application allows you to capture the entire screen, the active
window or a selected region.  You can set the delay that elapses before the screenshot
is taken and the action that will be done with the screenshot.
A plugin for the Xfce panel is also available.")
   (license gpl2+)
   (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-screensaver
  (package
    (name "xfce4-screensaver")
    (version "4.18.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09wbr3p325w9mcmragxi3rkvlrdapmrmlpgj5wshh9dv52pn8k5y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-maintainer-mode")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-dbus-1-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dbus-dir (string-append out "/share/dbus-1/services")))
               (substitute* "configure"
                 (("DBUS_SESSION_SERVICE_DIR=.*")
                  (string-append "DBUS_SESSION_SERVICE_DIR="
                                 dbus-dir)))))))))
    (native-inputs
     (list xfce4-dev-tools))
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
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-volumed-pulse
  (package
    (name "xfce4-volumed-pulse")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/apps/" name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0vr20rm81kd0av85zvfghzbczs4ahkxi1n4qn4hikqnb67iwrcq3"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list xfconf libnotify pulseaudio keybinder-3.0 gtk+))
    (home-page "https://gitlab.xfce.org/apps/xfce4-volumed-pulse")
    (synopsis "XFCE volume keys daemon")
    (description
     "This is a volume keys control daemon for Xfce Desktop environment.  It controls
 the volume using multimedia keys.  It also provides volume change notifications.")
    (license gpl3+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-cpugraph-plugin
  (package
    (name "xfce4-cpugraph-plugin")
    (version "1.2.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z3vbpn6ylx32fnncswi0ghp01bpxqvs0idmr8fc3rl546pgmqa3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-cpugraph-plugin/")
    (synopsis "Display CPU load as a graph in the Xfce panel")
    (description "This panel plugin offers multiple display
modes (LED, gradient, fire, etc…) to show the current CPU load of the
system.  Various appearance options, like colors or size, are
customizable.

On multi core or multi CPU systems, CPU Graph can either track and
display all of them at once, or at the user's option only a specific
core or CPU.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-eyes-plugin
  (package
    (name "xfce4-eyes-plugin")
    (version "4.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wwfxhaxp6r8g4jvj5ax7a3djh4q896ilxdfdcx6n67qzs5ksq8h"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-eyes-plugin/")
    (synopsis "Display a pair of eyes for the Xfce panel")
    (description "Eyes is a toy Xfce panel plugin that adds eyes which
watch your every step.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-datetime-plugin
  (package
    (name "xfce4-datetime-plugin")
    (version "0.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qkaz932qrkbajicd8h8ik405804xvaz1bqf36sh73nmvrj7k4vn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-datetime-plugin/")
    (synopsis "Display date and time inside the Xfce panel")
    (description "This plugin shows the date and time in the panel,
and a calendar appears when you left-click on it.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-calculator-plugin
  (package
    (name "xfce4-calculator-plugin")
    (version "0.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jdsd3qswzv5kvh1ybpmfz79nqwlfh34r9bhhvw5j71339mcpzf9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-calculator-plugin/")
    (synopsis "Calculator for the Xfce panel")
    (description "This plugin is a calculator for the Xfce4 panel.  It
supports common mathematical operators (+, -, *, /, ^) with usual
precedence rules, and the following functions and common constants.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-cpufreq-plugin
  (package
    (name "xfce4-cpufreq-plugin")
    (version "1.2.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m90li6lq7d4fvv3pvqvyhd13pijcmz5ipfnr1z7a0jhjkskabjq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-cpufreq-plugin/")
    (synopsis "Xfce panel plugin for displaying CPU frequency")
    (description "This panel plugin shows information about the CPU
governor and frequencies supported and used by your system.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-diskperf-plugin
  (package
    (name "xfce4-diskperf-plugin")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fav30y6r08yyrrm0yzi9jrn6af6hw8qk7wk7pd172ajqgcyp4ai"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-diskperf-plugin/")
    (synopsis "Display disk performance in the Xfce panel")
    (description "This Xfce panel plugin displays instant disk/partition
performance (bytes transferred per second).")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-fsguard-plugin
  (package
    (name "xfce4-fsguard-plugin")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0df9gs9wjmnb2889a0ssfjvjviswcjkcq6f8211fyzzqaj2gn9n0"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-fsguard-plugin/")
    (synopsis "Xfce panel plugin to monitor free disk space")
    (description "The panel plugin checks free space on a chosen mount
point frequently and displays a message when a limit is reached.  There
are two limits: a warning limit where only the icon changes, and an
urgent limit that advise the user with a message.  The icon button can
be clicked to open the chosen mount point.")
    (license bsd-2)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-genmon-plugin
  (package
    (name "xfce4-genmon-plugin")
    (version "4.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ap5qn2g1f4n3m30h9i0x2h8r84cfdh6pylnp0jc7r9gy910mxgm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-genmon-plugin/")
    (synopsis "Generic program output monitor for the Xfce panel")
    (description "This plugin cyclically spawns the indicated
script/program, captures its output (stdout) and displays the
resulting string into the panel.

The string can also contain markup to displayed an image, a bar, a
button and a personalized tooltip.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-mailwatch-plugin
  (package
    (name "xfce4-mailwatch-plugin")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dad2nl8y5v2y4xcp1lp51s0n1yb7v706igs5w2xrfsp7gp19d98"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list exo gnutls libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-mailwatch-plugin/")
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
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-mpc-plugin
  (package
    (name "xfce4-mpc-plugin")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hxpxvlbz0dmfy8d7kay9c08nx9bs19sqrgpzw2f64jfznv0xaay"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-mpc-plugin/")
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
    (license isc)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-mount-plugin
  (package
    (name "xfce4-mount-plugin")
    (version "1.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s3882dv4a9wmfcjjgn2yk6naswsgfy5cr2ql0wmdgzzg4s3pwl7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-mount-plugin/")
    (synopsis "Mount/unmount plugin for the Xfce panel")
    (description "The plugin will display a list of items representing
your various devices.  If you click on an unmounted devices it will
mount it and vice versa.  There is a warning in case a device can't be
mounted or when unmounting fails.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-netload-plugin
  (package
    (name "xfce4-netload-plugin")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ck3mdwhj7pzrb1xhynyp4550x27bvlja9z50as30mi070vn92l3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-netload-plugin/")
    (synopsis "Netload plugin for the Xfce Panel")
    (description "This plugin displays the current load of the network
interfaces of your choice in the panel.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-places-plugin
  (package
    (name "xfce4-places-plugin")
    (version "1.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09yqnya7hr4vr6pn4ddzs7vx8582yyf1wrrwd1fmd81f7mdns1w7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list desktop-file-utils xfce4-dev-tools))
    (inputs
     (list exo libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-places-plugin/")
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
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-smartbookmark-plugin
  (package
    (name "xfce4-smartbookmark-plugin")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rm2yii0xas9n3aqsq6lb378czcylm8xcb2ng7lnl2r5l8qyb9m4"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-smartbookmark-plugin/")
    (synopsis "Perform custom searches in your browser from the Xfce panel")
    (description "This plugin allows you to send search requests
directly to your browser, such that you can search through your
favorite search engine or bug tracker right from the Xfce panel.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

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
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-statusnotifier-plugin/")
    (synopsis "Xfce panel plugin for status notifier items")
    (description "This plugin provides a panel area for status
notifier items (application indicators).  Applications may use these
items to display their status and interact with the user.  This
technology is a modern alternative to systray and follows the
freedesktop.org specification.")
    (license gpl2+)
    (properties `((superseded . ,xfce4-panel)))))

(define-public xfce4-stopwatch-plugin
  (package
    (name "xfce4-stopwatch-plugin")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12b6r9vmqqwqdk8pwbi6xgcfbwvw26lz298b3k53aajvy80lxgql"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-stopwatch-plugin/")
    (synopsis "Stopwatch plugin for the Xfce panel")
    (description "This Xfce panel plugin keeps track of elapsed time.")
    (license bsd-2)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-systemload-plugin
  (package
    (name "xfce4-systemload-plugin")
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ds1z5zwsrk6m4sxa5fnaqk19vhiy1z7xjn91q1z03cys6j3a025"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libgtop libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-systemload-plugin/")
    (synopsis "System load display plugin for the Xfce panel")
    (description "A system load plugin for the Xfce4 desktop
environment.  It displays the current CPU load, the memory in use, the
swap space and the system uptime in the Xfce4 panel.")
    (license bsd-2)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-time-out-plugin
  (package
    (name "xfce4-time-out-plugin")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fk3h968yp9hq707na2kj1fvqa0dg1pqzy3g987if1sjw1w2d1qm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list gtk+ libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-time-out-plugin/")
    (synopsis "Xfce panel plugin that encourages periodical breaks")
    (description "This plugin encourages to take periodical
breaks from the computer every X minutes.  During breaks it locks your
screen.  It optionally allows you to postpone breaks for a certain
time.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-timer-plugin
  (package
    (name "xfce4-timer-plugin")
    (version "1.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1110s55h4lsspdmyl58cbzfy57gfh8871b1213xjgk3i3q7nmqgc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-timer-plugin/")
    (synopsis "Simple countdown and alarm plugin for the Xfce panel")
    (description "This is a simple plugin that lets the user run an
alarm at a specified time or at the end of a specified countdown
period.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-verve-plugin
  (package
    (name "xfce4-verve-plugin")
    (version "2.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "023d1vv0qa9ig8k15m7bl0b9hgqg7c7lf1w7d8av2y2g7xi8ljwg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui pcre2 xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-verve-plugin/")
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
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-wavelan-plugin
  (package
    (name "xfce4-wavelan-plugin")
    (version "0.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hw1xglcq9niacmnb6masnx8b7vs0z38pbnbviy388mvj5bx54dm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list libxfce4ui xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-wavelan-plugin/")
    (synopsis "Show stats from WLAN interface in Xfce panel")
    (description "This plugin is used to display stats from a wireless
lan interface (signal state, signal quality, network name (SSID)).")
    (license bsd-2)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-weather-plugin
  (package
    (name "xfce4-weather-plugin")
    (version "0.11.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url (string-append "https://gitlab.xfce.org/panel-plugins/"
                                 name))
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cxv5p1472vgy2nks9145m8yajsxrc1a83pmqpmnmvpyyandbdml"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-maintainer-mode"))) ;for weather-config_ui.h
    (native-inputs
     (list xfce4-dev-tools))
    (inputs
     (list gtk+ json-c libsoup-minimal-2 libxfce4ui libxml2 xfce4-panel))
    (home-page "https://docs.xfce.org/panel-plugins/xfce4-weather-plugin/")
    (synopsis "Show information about local weather in the Xfce panel")
    (description "This Xfce panel plugin shows information about your
local weather in the panel, using forecast data provided by the
@uref{https://met.no, Norwegian Meteorological Institute}.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))

(define-public xfce4-dev-tools
  (package
    (name "xfce4-dev-tools")
    (version "4.20.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.xfce.org/xfce/xfce4-dev-tools")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k7qj7vka2ys1ld4bfkdvsbxhpjnvb8lc0awnn5b1c34zxmwsivr"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-maintainer-mode") ;for xdt-csource.1
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-XDT_CHECK_PACKAGE_BINARY
            (lambda _
              (substitute* "m4macros/xdt-depends.m4"
                ;; Our pkg-config doesn't report absolute paths for this
                ;; executable check.
                (("-a -x \"\\$\\$1\"")
                 "-a -x `command -v \"$$1\"`")))))))
    (native-inputs (list autoconf
                         automake
                         docbook-xsl
                         libtool
                         libxslt
                         meson
                         pkg-config))
    (inputs (list glib python))
    (propagated-inputs
     (list
      ;; required by 'xdt-autogen'
      autoconf automake gtk-doc/stable intltool libtool
      ;; required by 'xdt-depends.m4'
      (list glib "bin") ;for glib-genmarshal
      pkg-config))
    (home-page "https://docs.xfce.org/xfce/xfce4-dev-tools/")
    (synopsis "Xfce developer tools")
    (description
     "The Xfce development tools are a collection of tools and macros for Xfce
developers and people that want to build Xfce from Git In addition it contains
the Xfce developer's handbook.")
    (license gpl2+)
    (properties `((release-tag-prefix . ,(string-append name "-"))))))
