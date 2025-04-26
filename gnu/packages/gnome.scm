;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2014, 2016, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014, 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2015, 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015 Mathieu Lirzin <mthl@openmailbox.org>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2015-2020, 2023, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2016, 2017, 2018, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016, 2017, 2018 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2016 Jochem Raat <jchmrt@riseup.net>
;;; Copyright © 2016, 2017, 2019 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2016, 2017 Nikita <nikita@n0.is>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017, 2018 nee <nee-git@hidamari.blue>
;;; Copyright © 2017 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mohammed Sadiq <sadiq@sadiqpk.org>
;;; Copyright © 2017, 2020, 2021 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018 Jovany Leandro G.C <bit4bit@riseup.net>
;;; Copyright © 2018, 2023 Vasile Dumitrascu <va511e@yahoo.com>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018, 2019 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2019 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2019, 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2019 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019 Jonathan Frederickson <jonathan@terracrypt.net>
;;; Copyright © 2019-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2019, 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2019, 2020, 2024 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2019-2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2020 Naga Malleswari <nagamalli@riseup.net>
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2020, 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2020, 2021 Andy Tai <atai@atai.org>
;;; Copyright © 2020, 2021 Sébastien Lerique <sl@eauchat.org>
;;; Copyright © 2021 Trevor Hass <thass@okstate.edu>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021, 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021, 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2022 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2022 Leo Nikkilä <hello@lnikki.la>
;;; Copyright © 2022 Rene Saavedra <nanuui@protonmail.com>
;;; Copyright © 2022 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2022 Arjan Adriaanse <arjan@adriaan.se>
;;; Copyright © 2023 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2023 Dominik Delgado Steuter <d@delgado.nrw>
;;; Copyright © 2023 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2024 Dariqq <dariqq@posteo.net>
;;; Copyright © 2024 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2024 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
;;; Copyright © 2025 Ashvith Shetty <ashvithshetty0010@zohomail.in>
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

(define-module (gnu packages gnome)
  #:use-module (gnu packages)
  #:use-module (gnu packages accessibility)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-check)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-database)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-graphics)
  #:use-module (gnu packages crates-gtk)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages dns)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages docker)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages firmware)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lirc)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lsof)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages man)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages ocr)
  #:use-module (gnu packages openldap)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rdf)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages search)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages slang)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages vpn)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu artwork)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public gupnp-igd
  (package
    (name "gupnp-igd")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1q9bw12ibih3yxpha3gm1dabyqg9gx6yxacbh4kxsgm1i84j0lab"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:glib-or-gtk? #t                 ; To wrap binaries and compile schemas
      #:configure-flags #~(list "-Dgtk_doc=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; A test using GIO expects ~/.config/glib-2.0/settings to be
              ;; writable.
              (setenv "HOME" (getcwd))))
          (add-after 'install 'move-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (mkdir-p (string-append #$output:doc "/share"))
              (rename-file
               (string-append #$output "/share/gtk-doc")
               (string-append #$output:doc "/share/gtk-doc")))))))
    (native-inputs
     (list docbook-xml-4.1.2
           docbook-xsl
           `(,glib "bin")
           gobject-introspection
           gsettings-desktop-schemas
           gtk-doc/stable
           pkg-config))
    (propagated-inputs
     ;; These libraries are required by the .pc file.
     (list glib
           glib-networking
           gupnp-1.4))
    (synopsis "UPnP IGD for GNOME")
    (description "GUPnP-IGD is a library to handle UPnP IGD port mapping.")
    (home-page "https://gitlab.gnome.org/GNOME/gupnp-igd")
    (license license:lgpl2.1+)))

(define-public brasero
  (package
    (name "brasero")
    (version "3.12.3")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/brasero/"
                                 (version-major+minor version) "/"
                                 "brasero-" version ".tar.xz"))
             (sha256
              (base32
               "05gabybkl7xfinwx97i4scp9hic0dlxj7gh03dyj0hd16fp9wx47"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags (list
                          (string-append "--with-girdir="
                                         (assoc-ref %outputs "out")
                                         "/share/gir-1.0")
                          (string-append "--with-typelibdir="
                                         (assoc-ref %outputs "out")
                                         "/lib/girepository-1.0"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'embed-growisofs-reference
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dvd+rw-tools (assoc-ref inputs "dvd+rw-tools")))
               (substitute* "plugins/growisofs/burn-growisofs.c"
                 (("(\")(growisofs)" _ prefix command)
                  (string-append prefix dvd+rw-tools "/bin/" command)))))))))
    (propagated-inputs
     (list hicolor-icon-theme))
    (native-inputs
     (list intltool
           itstool
           `(,glib "bin") ; glib-compile-schemas, etc.
           gobject-introspection
           pkg-config))
    (inputs
     (list dvd+rw-tools
           glib
           gstreamer
           gst-plugins-base
           gtk+
           libcanberra
           libice
           libnotify
           libsm
           libxml2
           totem-pl-parser))
    (home-page "https://wiki.gnome.org/Apps/Brasero")
    (synopsis "CD/DVD burning tool for Gnome")
    (description "Brasero is an application to burn CD/DVD for the Gnome
Desktop.  It is designed to be as simple as possible and has some unique
features to enable users to create their discs easily and quickly.")
    (license license:gpl2+)))

;;; Minimal variant, used to break a cycle with Inkscape.
(define-public libcloudproviders-minimal
  (package
    (name "libcloudproviders-minimal")
    (version "0.3.6")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/libcloudproviders/"
                       (version-major+minor version)
                       "/libcloudproviders-" version ".tar.xz"))
       (sha256
        (base32 "1dvlbsh5pfrnj745dlb1w0m4s1gy063y8h54qp2z9pjg785i2x9v"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t             ; To wrap binaries and/or compile schemas
       #:configure-flags (list "-Dintrospection=false"
                               "-Denable-gtk-doc=false"
                               "-Dvapigen=false")))
    (native-inputs
     (list `(,glib "bin") pkg-config vala))
    (inputs
     (list glib glib-networking))
    (synopsis "Cloudproviders Integration API")
    (description "Libcloudproviders is a DBus API that allows cloud storage sync
clients to expose their services.  Clients such as file managers and desktop
environments can then provide integrated access to the cloud providers
services.")
    (home-page "https://csorianognome.wordpress.com/2015/07/07/cloud-providers/")
    (license license:lgpl3+)))

(define-public libcloudproviders
  (package/inherit libcloudproviders-minimal
    (name "libcloudproviders")
    (outputs (cons "doc" (package-outputs libcloudproviders-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments libcloudproviders-minimal)
       ((#:configure-flags _)
        '(list "-Denable-gtk-doc=true")) ;false by default
       ((#:phases phases '%standard-phases)
        `(modify-phases %standard-phases
           (add-after 'install 'move-doc
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (assoc-ref outputs "doc")))
                 (mkdir-p (string-append doc "/share"))
                 (rename-file
                  (string-append out "/share/gtk-doc")
                  (string-append doc "/share/gtk-doc")))))))))
    (native-inputs
     (append
         `(("gobject-introspection" ,gobject-introspection)
           ("gtk-doc" ,gtk-doc/stable))
         (package-native-inputs libcloudproviders-minimal)))))

(define-public libgrss
  (package
    (name "libgrss")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1nalslgyglvhpva3px06fj6lv5zgfg0qmj0sbxyyl5d963vc02b7"))
       (patches
        (search-patches "libgrss-CVE-2016-2001.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags
           #~(list "--enable-gtk-doc" (string-append "--with-html-dir="
                                                     #$output
                                                     "/share/gtk-doc/html"))))
    (native-inputs (list docbook-xml-4.1.2 gobject-introspection gtk-doc/stable
                         pkg-config))
    (propagated-inputs (list glib libsoup-minimal-2 libxml2))
    (synopsis "Glib library for feeds")
    (description "LibGRSS is a Glib abstraction to handle feeds in RSS, Atom,
and other formats.")
    (home-page "https://wiki.gnome.org/Projects/Libgrss")
    (license license:lgpl3+)))

(define-public gnome-js-common
  (package
    (name "gnome-js-common")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32 "1zv5b9bcbclzj64xd9kgql4ndmbwvvi6cl937ykw8fp21xgh8z7y"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         ,@(if (or (target-riscv64?)
                   (target-ppc64le?)
                   (target-aarch64?))
               `((add-after 'unpack 'update-config-scripts
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (for-each (lambda (file)
                                 (install-file
                                   (search-input-file
                                     (or native-inputs inputs)
                                     (string-append "/bin/" file)) "."))
                               '("config.guess" "config.sub")))))
               '()))))
    (native-inputs
     `(,@(if (or (target-riscv64?)
                 (target-ppc64le?)
                 (target-aarch64?))
             `(("config" ,config))
             `())
       ("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (synopsis "Common JS Modules")
    (description "GNOME-JS-Common provides common modules for GNOME JavaScript
bindings.")
    (home-page "https://wiki.gnome.org/Projects/Seed")
    (license license:gpl3+)))

(define-public seed
  (package
    (name "seed")
    (version "3.8.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "0cmcxaggcdcy13j27gy8id2qsf2p2sl4bz2mwb9zhv3gzavlvjw0"))
       (patches
        (search-patches "seed-webkit.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags
           #~(list "--disable-static"
                   "--enable-xorg-module"
                   (string-append "--with-html-dir=" #$output:doc
                                  "/share/gtk-doc/html")
                   "--with-webkit=4.0")
           #:phases
           #~(modify-phases %standard-phases
               ;; The seed-webkit.patch patches configure.ac.
               ;; So the source files need to be re-bootstrapped.
               (add-after 'unpack 'trigger-bootstrap
                 (lambda _
                   (for-each delete-file
                             (list "configure"
                                   "Makefile.in"))))
               (add-after 'unpack 'patch-tests
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* (find-files "." "\\.js$")
                     (("#!/usr/bin/env seed")
                      (string-append "#!" (getcwd) "/src/seed"))))))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml-4.1.2
           gettext-minimal
           gobject-introspection
           gtk-doc/stable
           intltool
           libtool
           pkg-config))
    (inputs
     (list cairo
           dbus
           dbus-glib
           gnome-js-common
           gtk+
           gtk+-2
           libffi
           libxml2
           mpfr
           readline
           sqlite
           libxscrnsaver))
    (propagated-inputs
     (list glib
           webkitgtk-with-libsoup2))
    (synopsis "GObject JavaScriptCore bridge")
    (description "Seed is a library and interpreter, dynamically bridging
(through GObjectIntrospection) the WebKit JavaScriptCore engine, with the
GNOME platform.  It serves as something which enables you to write standalone
applications in JavaScript, or easily enable your application to be extensible
in JavaScript.")
    (home-page "https://wiki.gnome.org/Projects/Seed")
    (license license:lgpl2.0+)))

(define-public libdmapsharing
  (package
    (name "libdmapsharing")
    (version "3.9.10")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.flyn.org/projects/"
                       name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "152hnddwxv590cn802awv3mn27ixc3s6ac691a7z02d1c5fl45p2"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:tests? #f                  ; Tests require networking.
           #:configure-flags
           #~(list "--disable-static"
                   (string-append "--with-html-dir=" #$output:doc
                                  "/share/gtk-doc/html"))))
    (native-inputs
     (list check
           docbook-xml-4.3
           gobject-introspection
           pedansee
           pkg-config
           vala))
    (inputs
     (list avahi
           (librsvg-for-system)
           libgee
           gst-plugins-base
           gtk+))
    (propagated-inputs
     (list glib glib-networking gstreamer libsoup-minimal-2))
    (synopsis "Media management library")
    (description "Libdmapsharing is a library which allows programs to access,
share and control the playback of media content using DMAP (DAAP, DPAP & DACP).
It is written in C using GObject and libsoup.")
    (home-page "https://launchpad.net/gtx")
    (license license:lgpl2.1+)))

(define-public gtx
  (package
    (name "gtx")
    (version "0.2.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://launchpad.net/gtx/trunk/"
                       version "/+download/gtx-" version ".tar.gz"))
       (sha256
        (base32 "0i4zvn5v4rf0cw3fxylk6j2pyy5lkrswdiw8jdxkys0ph0nan33n"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-gtk-doc"
        (string-append "--with-html-dir="
                       (assoc-ref %outputs "doc")
                       "/share/gtk-doc/html"))))
    (native-inputs
     (list gobject-introspection gtk-doc/stable pkg-config))
    (propagated-inputs
     (list glib))
    (synopsis "GLib Testing Framework")
    (description "GTX is a small collection of convenience functions intended to
enhance the GLib testing framework.  With specific emphasis on easing the pain
of writing test cases for asynchronous interactions.")
    (home-page "https://launchpad.net/gtx")
    (license license:lgpl2.1+)))

(define-public dee
  (package
    (name "dee")
    (version "1.2.7")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://launchpad.net/dee/1.0/"
                       version "/+download/dee-" version ".tar.gz"))
       (sha256
        (base32 "12mzffk0lyd566y46x57jlvb9af152b4dqpasr40zal4wrn37w0v"))
       (patches
        (search-patches "dee-vapi.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags
           #~(list "--disable-maintainer-flags"
                   (string-append "--with-pygi-overrides-dir="
                                  #$output "/lib/python"
                                  #$(version-major+minor
                                     (package-version python))
                                  "/site-packages/gi/overrides")
                   (string-append "--with-html-dir="
                                  #$output "/share/gtk-doc/html"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-failing-tests
                 (lambda _
                   (substitute* "tests/test-icu.c"
                     (("g_test_add \\(DOMAIN\"/Default/AsciiFolder\",\
 Fixture, 0,")
                      "")
                     (("setup, test_ascii_folder, teardown\\);")
                      ""))))
               (add-before 'check 'pre-check
                 (lambda _
                   ;; Tests require a running dbus-daemon.
                   (system "dbus-daemon &")
                   ;; For missing '/etc/machine-id'.
                   (setenv "DBUS_FATAL_WARNINGS" "0"))))))
    (native-inputs
     (list dbus
           dbus-test-runner
           docbook-xml-4.3
           gobject-introspection
           gtk-doc/stable
           ;; Would only be required by configure flag "--enable-extended-tests".
           ;;gtx
           pkg-config
           python-pygobject
           python-wrapper
           vala-0.52))
    (inputs (list icu4c))
    (propagated-inputs (list glib))
    (synopsis "Model to synchronize multiple instances over DBus")
    (description "Dee is a library that uses DBus to provide objects allowing
you to create Model-View-Controller type programs across DBus.  It also consists
of utility objects which extend DBus allowing for peer-to-peer discoverability
of known objects without needing a central registrar.")
    (home-page "https://launchpad.net/dee")
    (license
     ;; Dual-licensed
     (list
      license:lgpl3+
      license:gpl3+))))

(define-public zeitgeist
  (package
    (name "zeitgeist")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/zeitgeist/zeitgeist.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "07b1ahj3vd3m8srwkrh7dl3ymr7d55xiiszny44q13g06pq4svch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list #:configure-flags #~(list "--enable-explain-queries"
                                     "--enable-fts"
                                     "--enable-docs")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-failing-tests
                 (lambda _
                   (substitute* "test/direct/Makefile.am"
                     (("	log-test ")
                      ""))
                   (substitute* "test/c/Makefile.am"
                     (("	test-log ")
                      ""))))
               (add-before 'bootstrap 'remove-autogen-script
                 (lambda _
                   ;; To honor `autoreconf -vif` by build-system.
                   (delete-file "autogen.sh"))))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml-4.3
           gettext-minimal
           gobject-introspection
           gtk-doc/stable
           libtool
           pkg-config
           vala
           xorg-server-for-tests))
    (inputs
     (list dee
           gtk+
           json-glib
           sqlite
           telepathy-glib
           python-wrapper
           python-rdflib
           xapian))
    (propagated-inputs (list glib))
    (synopsis "Desktop Activity Logging")
    (description "Zeitgeist is a service which logs the users’s activities and
events, anywhere from files opened to websites visited and conversations.  It
makes this information readily available for other applications to use.  It is
able to establish relationships between items based on similarity and usage
patterns.")
    (home-page "https://zeitgeist.freedesktop.org/")
    (license
     ;; Dual-licensed
     (list
      license:lgpl2.1+
      license:gpl2+))))

(define-public gnome-recipes
  (package
    (name "gnome-recipes")
    (version "2.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/recipes")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h049mzqnlcfqwrhmzbq3pzzdglvy2bn9fj1p8wql7a60pn8sr32"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson_post_install.py"
                (("gtk-update-icon-cache") (which "true")))))
          (add-after 'unpack 'unpack-libgd
            (lambda _
              (copy-recursively
               #$(this-package-native-input "libgd-checkout")
               "subprojects/libgd"))))))
    (inputs (list glib
                  gnome-autoar
                  gnome-online-accounts
                  gspell
                  gtk+
                  json-glib
                  libcanberra
                  libsoup
                  rest))
    (native-inputs (list desktop-file-utils ;for update-desktop-database
                         gettext-minimal
                         `(,glib "bin")
                         (origin
                           (method git-fetch)
                           (uri (git-reference
                                 (url "https://gitlab.gnome.org/GNOME/libgd")
                                 (commit "c7c7ff4e05d3fe82854219091cf116cce6b19de0")))
                           (file-name "libgd-checkout")
                           (sha256
                            (base32
                             "16yld0ap7qj1n96h4f2sqkjmibg7xx5xwkqxdfzam2nmyfdlrrrs")))
                         itstool
                         pkg-config
                         python))
    (home-page "https://wiki.gnome.org/Apps/Recipes")
    (synopsis "Discover recipes for preparing food")
    (description "GNOME Recipes helps you discover what to cook today,
tomorrow, the rest of the week and for special occasions.")
    (license license:gpl3+)))

(define-public gnome-photos
  (package
    (name "gnome-photos")
    (version "44.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "17l2bkdg8iracgw2mgx5vqfs3d6cdvd22mfdqq4jiinkjw1j33p7"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:disallowed-references (list (this-package-native-input "git-minimal"))
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list "-Ddogtail=false"         ; Not available
              ;; Required for RUNPATH validation.
              (string-append "-Dc_link_args=-Wl,-rpath="
                             #$output "/lib/gnome-photos"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-gtk-update-icon-cache
            (lambda _
              (setenv "DESTDIR" "/")))
          (add-after 'install 'wrap-gnome-photos
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/gnome-photos")
                `("GRL_PLUGIN_PATH" =
                  (,(getenv "GRL_PLUGIN_PATH")))))))))
    (native-inputs
     (list dbus
           desktop-file-utils
           gettext-minimal
           git-minimal/pinned
           `(,glib "bin")
           gobject-introspection
           gsettings-desktop-schemas
           itstool
           pkg-config))
    (inputs
     (list babl
           bash-minimal
           cairo
           gegl-0.4.44
           geocode-glib
           gexiv2
           gfbgraph
           gnome-online-accounts
           gnome-online-miners
           grilo
           grilo-plugins
           gtk+
           libdazzle
           libgdata
           libhandy
           libjpeg-turbo
           libportal
           libpng
           (librsvg-for-system)
           python-pygobject
           rest
           tracker
           tracker-miners))
    (synopsis "Access, organize and share your photos on GNOME desktop")
    (description "GNOME Photos is a simple and elegant replacement for using a
file manager to deal with photos.  Enhance, crop and edit in a snap.  Seamless
cloud integration is offered through GNOME Online Accounts.")
    (home-page "https://wiki.gnome.org/Apps/Photos")
    (license license:gpl3+)))

(define-public gnome-music
  (package
    (name "gnome-music")
    (version "46.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1zdzafirfhaldbp8m8pmqw0ysfxc7ndbakqc7d1xrr4v51n2vghi"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-after 'install 'wrap-gnome-music
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (pylib (string-append out "/lib/python"
                                           #$(version-major+minor
                                              (package-version
                                               (this-package-input "python")))
                                           "/site-packages")))
                (wrap-program (string-append out "/bin/gnome-music")
                  `("GI_TYPELIB_PATH" =
                    (,(getenv "GI_TYPELIB_PATH")))
                  `("GST_PLUGIN_SYSTEM_PATH" suffix
                    (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                  `("GRL_PLUGIN_PATH" =
                    (,(getenv "GRL_PLUGIN_PATH")))
                  `("GUIX_PYTHONPATH" =
                    (,(getenv "GUIX_PYTHONPATH") ,pylib)))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           pkg-config))
    (inputs
     (list bash-minimal
           gnome-online-accounts
           grilo
           grilo-plugins
           gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk
           gvfs
           json-glib
           libadwaita
           libdazzle
           libmediaart
           libsoup
           python-pycairo
           python-pygobject
           python
           tracker
           tracker-miners))
    (synopsis "Simple music player for GNOME desktop")
    (description "GNOME Music is the new GNOME music playing application that
aims to combine an elegant and immersive browsing experience with simple
and straightforward controls.")
    (home-page "https://wiki.gnome.org/Apps/Music")
    (license license:gpl2+)))

(define-public portablexdr
  (package
    (name "portablexdr")
    (version "4.9.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://people.redhat.com/~rjones/" name "/files/"
                       name "-" version ".tar.gz"))
       (sha256
        (base32
         "0b77ipvvi520nv7rr6jb1c3xryhc3m2mywhby7m48kfgag8vvx2w"))))
    (build-system gnu-build-system)
    (synopsis "External Data Representation Library")
    (description "PortableXDR is an implementation of External Data
Representation (XDR) Library.  It is a standard data serialization format, for
uses such as computer network protocols.  It allows data to be transferred
between different kinds of computer systems.")
    (home-page "https://people.redhat.com/~rjones/portablexdr/")
    (license
     (list
      license:gpl2+
      license:lgpl2.1+))))

(define-public tepl
  (package
    (name "tepl")
    (version "6.4.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "08bkp3wrvmcks0082lfw4a0ian9c6j68rdb43px0bkyhd43b4mjy"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-problematic-tests
                    (lambda _
                      ;; Only keep unit tests, as the interactive tests have
                      ;; other dependencies.
                      (substitute* "tests/meson.build"
                        ((".*'interactive-tests'.*") ""))))
                  (add-before 'check 'start-xserver
                    (lambda _
                      (system "Xvfb :1 &")
                      (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           gtk-doc
           libxml2
           pkg-config
           xorg-server-for-tests))
    (inputs
     (list uchardet))
    (propagated-inputs
     ;; These are all required by tepl6.pc.
     (list amtk
           glib
           gsettings-desktop-schemas
           gtk+
           gtksourceview-4))
    (synopsis "Text editor product line")
    (description "Tepl is a library that eases the development of
GtkSourceView-based text editors and IDEs.")
    (home-page "https://wiki.gnome.org/Projects/Tepl")
    (license license:lgpl2.1+)))

(define-public krb5-auth-dialog
  (package
    (name "krb5-auth-dialog")
    (version "3.26.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1w91gsvmifqhiam3xqf88i5rk2w6qadjalmbvvamjdc37j0vdc6x"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool itstool libxml2 pkg-config python-wrapper))
    (inputs
     (list glib gtk+ libnotify mit-krb5 network-manager))
    (synopsis "Popup dialogs for Kerberos 5")
    (description "krb5-auth-dialog is a simple dialog that monitors Kerberos
tickets, and pops up a dialog when they are about to expire.")
    (home-page "https://gitlab.gnome.org/GNOME/krb5-auth-dialog")
    (license license:gpl2+)))

(define-public notification-daemon
  (package
    (name "notification-daemon")
    (version "3.20.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32
         "1rgchqi4j2ll7d6a7lgy7id0w9rrkwkgic1096fbm2zx6n7pc4yx"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list intltool pkg-config))
    (inputs
     (list glib gtk+ libx11))
    (synopsis "Notification Daemon for GNOME Desktop")
    (description "Notification-Daemon is the server implementation of the
freedesktop.org desktop notification specification.")
    (home-page "https://wiki.gnome.org/Projects/NotificationDaemon")
    (license license:gpl2+)))

(define-public metacity
  (package
    (name "metacity")
    (version "3.46.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/metacity/"
                                  (version-major+minor version) "/"
                                  "metacity-" version ".tar.xz"))
              (sha256
               (base32
                "1ifnbpiflaw72m0flysa5qy44c1axd2rr9zcparz5210c7vlkfh0"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list gettext-minimal
           libtool
           autoconf
           automake
           pkg-config
           (list glib "bin")
           grep))
    (inputs
     (list libcanberra
           zenity
           libsm
           libice
           gtk+
           pango
           gsettings-desktop-schemas
           gobject-introspection
           libgtop
           libxcomposite
           libxcursor
           libxfixes
           libxdamage
           libxext
           libxpresent
           libxres
           libxrender
           libxinerama
           libx11
           libxrandr))
    (home-page "https://gitlab.gnome.org/GNOME/metacity")
    (synopsis "Simple compositing window manager")
    (description "Metacity is a window manager with a focus on simplicity and
usability rather than novelties or gimmicks.  Its author has characterized it
as a \"boring window manager for the adult in you.\"")
    (license license:gpl2+)))

(define-public mm-common
  (package
    (name "mm-common")
    (version "1.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/mm-common/"
                                  (version-major+minor version) "/"
                                  "mm-common-" version ".tar.xz"))
              (sha256
               (base32
                "1rv211kalivq8zlq7s7bd3dhm4f33jhrwf1vxrfbrnmwgl1lcp5m"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "util/mm-common-prepare.in"
                (("ln") (search-input-file inputs "/bin/ln"))
                (("cp") (search-input-file inputs "/bin/cp"))
                (("sed") (search-input-file inputs "/bin/sed"))
                (("cat") (search-input-file inputs "/bin/cat"))))))))
    (native-inputs
     (list coreutils gettext-minimal pkg-config sed))
    (inputs
     (list python))
    (synopsis "Module of GNOME C++ bindings")
    (description "The mm-common module provides the build infrastructure
and utilities shared among the GNOME C++ binding libraries.  Release
archives of mm-common include the Doxygen tag file for the GNU C++
Library reference documentation.")
    (home-page "https://gitlab.gnome.org/GNOME/mm-common")
    (license license:gpl2+)))

(define-public phodav
  (package
    (name "phodav")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1l9qs70yvwi9r8ph081mrsdy412kk0m9l9pgy77hsc2hdp8c4bir"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-udev-rules-directory
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out   (assoc-ref outputs "out"))
                    (rules (string-append out "/lib/udev/rules.d")))
               (substitute* "data/meson.build"
                 (("udev\\.get_pkgconfig_variable\\('udevdir'\\)")
                  (format #f "'~a'" rules))))))
         (add-before 'check 'set-temporary-home
           ;; Tests want to write into HOME.
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list docbook-xml-4.3
           gettext-minimal
           `(,glib "bin")
           gsettings-desktop-schemas
           gtk-doc/stable
           pkg-config))
    (inputs
     (list avahi
           libgudev))
    (propagated-inputs
     ;; These inputs are required by the pkg-config file.
     (list glib
           libsoup
           libxml2))
    (synopsis "WebDav server implementation using libsoup")
    (description "PhoDav was initially developed as a file-sharing mechanism for Spice,
but it is generic enough to be reused in other projects,
in particular in the GNOME desktop.")
    (home-page "https://wiki.gnome.org/phodav")
    (license license:lgpl2.1+)))

(define-public gnome-color-manager
  (package
   (name "gnome-color-manager")
   (version "3.36.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0fxdng74d8hwhfx1nwl1i4jx9h9f6c2hkyc12f01kqbjcimrxnwx"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:phases
       (modify-phases %standard-phases
        (add-before
         'check 'pre-check
         (lambda _
           ;; Tests require a running X server.
           (system "Xvfb :1 &")
           (setenv "DISPLAY" ":1")
           #t)))))
   (native-inputs
    `(("desktop-file-utils" ,desktop-file-utils)
      ("gettext" ,gettext-minimal)
      ("glib:bin" ,glib "bin")
      ("gtk+:bin" ,gtk+ "bin")
      ("itstool" ,itstool)
      ("pkg-config" ,pkg-config)
      ("xorg-server" ,xorg-server-for-tests)))
   (inputs
    (list adwaita-icon-theme
          appstream-glib
          colord-gtk
          exiv2
          gnome-desktop
          libcanberra
          libexif
          libtiff
          libxrandr
          libxtst
          libxxf86vm
          vte/gtk+-3
          xorgproto))
   (synopsis "Color profile manager for the GNOME desktop")
   (description "GNOME Color Manager is a session framework that makes
it easy to manage, install and generate color profiles
in the GNOME desktop.")
   (home-page "https://gitlab.gnome.org/GNOME/gnome-color-manager")
   (license license:gpl2+)))

(define-public gnome-online-miners
  (package
    (name "gnome-online-miners")
    (version "3.34.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1n2jz9i8a42zwxx5h8j2gdy6q1vyydh4vl00r0al7w8jzdh24p44"))
              (patches
               (search-patches
                "gnome-online-miners-tracker-3.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure
           (lambda _
             (substitute* "configure.ac"
               (("AX_CHECK_ENABLE_DEBUG.*")
                ""))))
         (add-after 'fix-configure 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vif"))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("gnome-online-accounts" ,gnome-online-accounts)
       ("grilo" ,grilo)
       ("libgdata" ,libgdata)
       ("libgfbgraph" ,gfbgraph)
       ("libzapojit" ,libzapojit)
       ("rest" ,rest)
       ("tracker" ,tracker)))
    (synopsis "Web Crawlers for GNOME")
    (description "GNOME Online Miners provides a set of crawlers that
go through your online content and index them locally in Tracker.
It has miners for Facebook, Flickr, Google, ownCloud and SkyDrive.")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineMiners")
    (license license:gpl2+)))

(define-public gssdp
  (package
    (name "gssdp")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0iil7wgix0nzhf3i2w6g1wjqly49r9rsffca97ai9kr2vfpvbv9g"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dgtk_doc=true"
                   ;; Manpages are built using pandoc.
                   #$@(if (this-package-native-input "pandoc")
                          #~("-Dmanpages=true")
                          #~("-Dmanpages=false")))))
    (native-inputs
     (append
       (if (supported-package? pandoc)
           (list pandoc)
           '())
       (list gettext-minimal
             `(,glib "bin")
             gi-docgen
             gobject-introspection
             pkg-config
             vala)))
    (inputs
     (list gtk))
    (propagated-inputs
     ;; The .pc file "Requires" libsoup.
     (list libsoup))
    (synopsis "GNOME GObject-based API over @acronym{SSDP, Simple Service Discovery Protocol}")
    (description "This package provides a library to handle resource discovery
and announcement over @acronym{SSDP, Simple Service Discovery Protocol} and
a debugging tool, @command{gssdp-device-sniffer}.")
    (home-page "https://gitlab.gnome.org/GNOME/gssdp")
    (license license:lgpl2.0+)))

(define-public gssdp-1.4
  (package
    (inherit gssdp)
    (name "gssdp")
    (version "1.4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "10hm8cgh2p8441xc83kswjgghrrqpzgblvc5523jp0pvayfq8xl6"))))
    (arguments
     (list #:configure-flags
           #~(list "-Dgtk_doc=true")))
    (propagated-inputs (modify-inputs (package-propagated-inputs gssdp)
              (replace "libsoup" libsoup-minimal-2)))))

(define-public gupnp
  (package
    (name "gupnp")
    (version "1.6.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "01gqpvyqmxlgxg91hmx68pq2d8sgrihrdzlwrrh0w9x7m2jxhqaa"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~'("-Dgtk_doc=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a writable HOME.
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gettext-minimal
           gi-docgen
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           pkg-config
           vala))
    (propagated-inputs
     ;; These libraries are required by the .pc file.
     (list glib
           gsettings-desktop-schemas    ;for ‘org.gnome.system.proxy’.
           gssdp
           libsoup
           libxml2))
    (synopsis "PnP API for GNOME")
    (description "This package provides GUPnP, an object-oriented framework
for creating UPnP devices and control points, written in C using
@code{GObject} and @code{libsoup}.")
    (home-page "https://gitlab.gnome.org/GNOME/gupnp")
    (license license:lgpl2.0+)))

(define-public gupnp-1.4
  (package
    (inherit gupnp)
    (name "gupnp")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0vz3ifs8mi3zaz8zj8v27zfkf6xg82y39mcgqspa38jdp01gn3sr"))))
    (arguments
     (substitute-keyword-arguments (package-arguments gupnp)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'meson-compatibility
            (lambda _
              (substitute* "subprojects/gssdp-1.2.wrap"
                (("provides") "provide"))))))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs gupnp)
       (replace "libsoup" libsoup-minimal-2)
       (replace "gssdp" gssdp-1.4)))))

(define-public gupnp-dlna
  (package
   (name "gupnp-dlna")
   (version "0.12.0")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "1rj8j2nz835slqs09bfp6hmyyf6v5azwjzin7v9jhmmwhmpvjlrx"))))
   (build-system meson-build-system)
   (native-inputs
    (list gettext-minimal
          `(,glib "bin")
          gobject-introspection
          gtk-doc/stable
          libxml2
          pkg-config
          vala))
   (inputs
    (list gstreamer gupnp))
   (propagated-inputs
    (list gst-plugins-base gst-plugins-good))
   (synopsis "GUPnP DLNA for GNOME")
   (description "This package provides a small utility library to
support DLNA-related tasks such as media profile guessing, transcoding to a
given profile, etc.  DLNA is a subset of UPnP A/V.")
   (home-page "https://gitlab.gnome.org/GNOME/gupnp-dlna")
   (license license:lgpl2.0+)))

(define-public gupnp-av
  (package
   (name "gupnp-av")
   (version "0.14.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/" name "/"
                                (version-major+minor version) "/"
                                name "-" version ".tar.xz"))
            (sha256
             (base32
              "0kry7arwmvs8b4175wia3l0s02ap1shq68f29g2xjrhc9g6f175p"))))
   (build-system meson-build-system)
   (native-inputs
    (list gettext-minimal
          `(,glib "bin")
          gobject-introspection
          gtk-doc/stable
          libxml2
          pkg-config
          vala))
   (inputs
    (list gtk+ gupnp))
   (synopsis "GUPnP A/V for GNOME")
   (description "This package provides a small library for handling
and implementation of UPnP A/V profiles.")
   (home-page "https://gitlab.gnome.org/GNOME/gupnp-av")
   (license license:lgpl2.0+)))

(define-public libmediaart
  (package
    (name "libmediaart")
    (version "1.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "05vzcxm8fqizz77qzdxlsqb5lsqzc4681sy8z63haf6vswjm1g63"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin")
           gettext-minimal
           gobject-introspection
           pkg-config
           vala))
    (inputs
     (list gdk-pixbuf))
    (synopsis "Media art library for the GNOME desktop")
    (description
     "The libmediaart library is the foundation for media art caching,
extraction, and lookup for applications on the desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/libmediaart")
    (license license:lgpl2.1+)))

(define-public gnome-initial-setup
  (package
    (name "gnome-initial-setup")
    (version "46.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-initial-setup/"
                                  (version-major version)
                                  "/gnome-initial-setup-" version ".tar.xz"))
              (sha256
               (base32
                "00cwlzgg50y1ckgddpwx55vvsxc6mxfmwrakf440awiiz1lk5ild"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dsystemd=false")
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-gkbd-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Allow the "Preview" button in the keyboard layout
              ;; selection dialog to display the layout.
              (substitute* "gnome-initial-setup/pages/keyboard/cc-input-chooser.c"
                (("\"tecla")
                 (string-append "\"" (search-input-file
                                      inputs
                                      "bin/tecla")))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config))
    (inputs
     (list accountsservice
           dconf
           elogind
           gdm
           geoclue
           gnome-desktop
           gnome-online-accounts
           gstreamer
           ibus
           json-glib
           mit-krb5
           libadwaita
           libgweather4
           libnma
           libpwquality
           libsecret
           network-manager
           packagekit
           polkit
           rest-next
           tecla
           upower
           webkitgtk))
    (synopsis "Initial setup wizard for GNOME desktop")
    (description "This package provides a set-up wizard when a
user logs into GNOME for the first time.  It typically provides a
tour of all gnome components and allows the user to set them up.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-initial-setup")
    (license license:gpl2)))

(define-public gnome-user-share
  (package
    (name "gnome-user-share")
    (version "43.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1kiq2n39yz7szcf7wrs5vhd2hdn04zx1pxgp7qskycaq0nm0dwqd"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:configure-flags
           #~(list "-Dsystemduserunitdir=/tmp/empty")))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk "bin")
           pkg-config
           yelp-tools))
    (inputs (list glib gtk))
    (synopsis "File sharing for GNOME desktop")
    (description "GNOME User Share is a small package that binds together
various free software projects to bring easy to use user-level file
sharing to the masses.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-user-share")
    (license license:gpl2+)))

(define-public sushi
  (package
    (name "sushi")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1ipjl1c9mib5gq9m58vhxg9jzfrggv2bbah6qr123arhljm5n24n"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'glib-or-gtk-wrap 'wrap-typelib
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((prog (string-append (assoc-ref outputs "out")
                                         "/bin/sushi")))
                ;; Put existing typelibs before sushi's deps, so as to
                ;; correctly infer gdk-pixbuf.
                (wrap-program prog
                  `("GI_TYPELIB_PATH" suffix
                    (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     (list `(,glib "bin")
           gettext-minimal
           gobject-introspection
           pkg-config))
    (inputs
     (list bash-minimal
           clutter
           clutter-gst
           clutter-gtk
           evince                       ; For file previewing.
           freetype
           gdk-pixbuf
           gjs
           gst-plugins-base
           gstreamer
           gtksourceview-4
           harfbuzz
           libepoxy
           libmusicbrainz
           libxml2
           neon
           webkitgtk-for-gtk3))
    (synopsis "File previewer for the GNOME desktop")
    (description "Sushi is a DBus-activated service that allows applications
to preview files on the GNOME desktop.")
    (home-page "https://gitlab.gnome.org/GNOME/sushi")
    (license license:gpl2+)))

(define-public rygel
  (package
    (name "rygel")
    (version "0.42.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "011k9vph4vz8md9cg03g5r3qjwf18yz5dfma4rg3g20hnq25glj7"))))
    (build-system meson-build-system)
    (arguments
     ;; Disable the tracker plugin.
     (list #:configure-flags
           #~(list "-Dplugins=external,gst-launch,lms,media-export,
mpris,playbin,ruih,tracker3")))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-compile-schemas, etc.
           gobject-introspection
           gtk-doc/stable
           pkg-config
           python
           vala
           docbook-xml
           docbook-xsl
           libxslt))
    (inputs
     (list gdk-pixbuf
           gssdp
           gstreamer
           gst-plugins-base
           gst-editing-services
           gtk+
           gupnp
           gupnp-av
           gupnp-dlna
           json-glib
           libgee
           libmediaart
           libsoup
           libxslt
           libunistring
           tracker))
    (propagated-inputs
     ;; The .pc files require.private gmodule-2.0
     (list glib))
    (synopsis "Share audio, video, and pictures with other devices")
    (description
     "Rygel is a home media solution (@dfn{UPnP AV MediaServer and
MediaRenderer}) for GNOME that allows you to easily share audio, video, and
pictures, and to control a media player on your home network.

Rygel achieves interoperability with other devices by trying to conform to the
strict requirements of DLNA and by converting media on-the-fly to formats that
client devices can handle.")
    (home-page "https://wiki.gnome.org/Projects/Rygel")
    (license (list
              ;; For logo (data/icons/*).
              license:cc-by-sa3.0
              ;; For all others.
              license:lgpl2.1+))))

(define-public libnma
  (package
    (name "libnma")
    (version "1.10.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1avdsw1l61gwr29lzvlr4dh3qz6ypsc3xvfahrcprlqa34mzp9jk"))))
    (build-system meson-build-system)
    (arguments
     ;; GTK 4.x depends on Rust (indirectly) so pull it only on platforms
     ;; where it is supported.
     (list
      #:configure-flags
      (if (supported-package? gtk)
          #~(list "-Dlibnma_gtk4=true")
          #~(list "-Dlibnma_gtk4=false"))
      #:phases
      #~(modify-phases %standard-phases
          ;; We follow upstream's recommendation at
          ;; https://gitlab.gnome.org/GNOME/libnma/-/commit/9166164387b0367becbe3400af696f925fef0ab1
          (add-after 'install 'delete-org.gnome.nm-applet.gschema
            (lambda _
              (delete-file
               (string-append
                #$output
                "/share/glib-2.0/schemas/org.gnome.nm-applet.gschema.xml")))))))
    (native-inputs
     (list docbook-xml-4.3
           gettext-minimal
           `(,glib "bin")
           gtk-doc/stable
           gobject-introspection
           pkg-config
           vala))
    (inputs
     (list gcr-3
           (if (supported-package? gtk) gtk gtk+)
           iso-codes/pinned
           mobile-broadband-provider-info
           network-manager))
    (synopsis "Network Manager's applet library")
    (description "Libnma is an applet library for Network Manager.  It was
initially part of network-manager-applet and has now become a separate
project.")
    (home-page "https://gitlab.gnome.org/GNOME/libnma")
    ;; Some files carry the "GPL-2.0+" SPDX identifier while others say
    ;; "LGPL-2.1+".
    (license license:gpl2+)))

(define-public gnome-menus
  (package
    (name "gnome-menus")
    (version "3.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-menus/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "07xvaf8s0fiv0035nk8zpzymn5www76w2a1vflrgqmp9plw8yd6r"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-introspection-install-dir
            (lambda _
              (substitute* "libmenu/Makefile.in"
                (("@INTROSPECTION_GIRDIR@")
                 (string-append #$output "/share/gir-1.0/"))
                (("@INTROSPECTION_TYPELIBDIR@")
                 (string-append #$output "/lib/girepository-1.0/"))))))))
    (native-inputs
     (list gettext-minimal glib gobject-introspection pkg-config))
    (synopsis "Menu support for GNOME desktop")
    (description "GNOME Menus contains the libgnome-menu library, the layout
configuration files for the GNOME menu, as well as a simple menu editor.")
    (home-page "https://gitlab.gnome.org/GNOME/gnome-menus")
    (license license:lgpl2.0+)))

(define-public deja-dup
  (package
    (name "deja-dup")
    (version "45.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.gnome.org/World/deja-dup/-/archive/"
                                  version "/deja-dup-" version ".tar.bz2"))
              (sha256
               (base32
                "000cwy1haiglkvn5plmhrs2a1fhpcpw6z4mdzck7ybmky795amza"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list
         ;; Otherwise, the RUNPATH will lack the final path component.
         (string-append "-Dc_link_args=-Wl,-rpath="
                        (assoc-ref %outputs "out") "/lib/deja-dup"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((python (assoc-ref inputs "python")))
                (substitute* '("libdeja/duplicity/DuplicityInstance.vala"
                               "libdeja/tests/scripts/instance-error.test")
                  (("/bin/rm")
                   (which "rm")))
                (substitute* "libdeja/tests/runner.vala"
                  (("/bin/sh")
                   (which "sh")))
                (substitute* "libdeja/tests/scripts/instance-error.test"
                  (("`which python3`")
                   (string-append python "/bin/python3"))))))
          (add-after 'unpack 'patch-libgpg-error
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libgpg-error (assoc-ref inputs "libgpg-error")))
                (substitute* "meson.build"
                  (("(gpgerror_libs = ).*" _ var)
                   (format #f "~a '-L~a/lib -lgpg-error'\n" var libgpg-error))))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Add duplicity to the search path
              (wrap-program (string-append (assoc-ref outputs "out")
                                           "/bin/deja-dup")
                `("PATH" ":" prefix
                  (,(dirname (search-input-file inputs "/bin/duplicity"))))))))))
    (inputs
     (list bash-minimal
           duplicity
           gsettings-desktop-schemas
           gtk
           json-glib
           libadwaita
           libgpg-error
           libnotify
           libsecret
           libsoup
           libhandy
           packagekit
           python
           python-pygobject))
    (native-inputs
     (list appstream-glib
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-schemas
           gobject-introspection
           `(,gtk "bin")                ;for gtk-update-icon-cache
           itstool
           pkg-config
           vala))
    (home-page "https://wiki.gnome.org/Apps/DejaDup")
    (synopsis "Simple backup tool, for regular encrypted backups")
    (description
     "Déjà Dup is a simple backup tool, for regular encrypted backups.  It
uses duplicity as the backend, which supports incremental backups and storage
either on a local, or remote machine via a number of methods.")
    (license license:gpl3+)))

(define-public gnome-commander
  (package
    (name "gnome-commander")
    (version "1.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "gnome-commander-" version ".tar.xz"))
       (sha256
        (base32 "0wqnm87skbsc7p89ynn64s3154w0j1d0d1gjkbxd5mmpg90i0ysa"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare-x-for-test
           (lambda _
             (system "Xvfb &")
             (setenv "DISPLAY" ":0"))))))
    (native-inputs
     (list desktop-file-utils
           flex
           gettext-minimal
           `(,glib "bin")
           googletest
           `(,gtk+ "bin")
           itstool
           pkg-config
           xorg-server-for-tests))
    (inputs
     (list glib gtk+))
    (home-page "https://gcmd.github.io/")
    (synopsis "Two-pane graphical file manager for the GNOME desktop")
    (description
     "GNOME Commander is a two-pane graphical file manager using GNOME
libraries.  It aims to fulfill the demands of more advanced users who
like to focus on file management, their work through special applications
and running smart commands.")
    (license license:gpl2+)))

(define-public gnome-user-docs
  (package
   (name "gnome-user-docs")
   (version "46.7")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://gnome/sources/gnome-user-docs/"
                          (version-major version)
                          "/gnome-user-docs-" version ".tar.xz"))
      (sha256
       (base32 "15qzaa1d22l56xflj7jql28ghl4y4lg7rwjrai8i7758aqy6qipl"))))
   (build-system gnu-build-system)
   (native-inputs
    (list gettext-minimal itstool pkg-config libxml2))
   (synopsis "User documentation for the GNOME desktop")
   (description
    "The GNOME User Documentation explains how to use the GNOME desktop and its
components.  It covers usage and setup of the core GNOME programs by end-users
and system administrators.")
   (home-page "https://wiki.gnome.org/DocumentationProject")
   (license license:cc-by3.0)))

(define-public dia
  ;; This version from GNOME's repository includes fixes for compiling with
  ;; recent versions of the build tools.  The latest activity on the
  ;; pre-GNOME version has been in 2014, while GNOME has continued applying
  ;; fixes since.
  (let ((commit "b903dd83aa5aab1b41c7864dd5027d1b6a0a190c")
        (revision "4"))
    (package
      (name "dia")
      (version (git-version "0.97.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.gnome.org/GNOME/dia.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j5q7whwpzzfsinjryp3g0xh3cyy88drwyr0w8x0666mj6h70h6a"))))
      (build-system meson-build-system)
      ;; XXX: Parallel builds may cause: [74/566] [...]
      ;; fatal error: dia-lib-enums.h: No such file or directory
      (arguments '(#:parallel-build? #f))
      (inputs
       (list graphene
             gtk+-2
             libxml2
             libxslt

             ;; XXX: PDF plugin fails to build with poppler 21.07.0.
             ;; poppler

             python))
      (native-inputs
       (list appstream-glib docbook-xsl
             `(,glib "bin") gettext-minimal pkg-config))
      (home-page "https://wiki.gnome.org/Apps/Dia")
      (synopsis "Diagram creation for GNOME")
      (description "Dia can be used to draw different types of diagrams, and
includes support for UML static structure diagrams (class diagrams), entity
relationship modeling, and network diagrams.  The program supports various file
formats like PNG, SVG, PDF and EPS.")
      (license license:gpl2+))))

(define-public libgdata
  (package
    (name "libgdata")
    (version "0.18.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "libgdata-fix-tests.patch"))
              (sha256
               (base32
                "1iq4d1qy0vkmy29xvr13dgz4pxvn5v3yi2swryld0ajinvp951fx"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home-for-tests
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("uhttpmock" ,uhttpmock-with-libsoup2)))
    (inputs
     (list cyrus-sasl glib-networking vala))
    (propagated-inputs
     `(("gcr" ,gcr-3)
       ("glib" ,glib)
       ("gnome-online-accounts" ,gnome-online-accounts)
       ("json-glib" ,json-glib)
       ("liboauth" ,liboauth)
       ("libsoup" ,libsoup-minimal-2)
       ("libxml2" ,libxml2)))
    (home-page "https://wiki.gnome.org/Projects/libgdata")
    (synopsis "Library for accessing online service APIs")
    (description
     "libgdata is a GLib-based library for accessing online service APIs using
the GData protocol — most notably, Google's services.  It provides APIs to
access the common Google services, and has full asynchronous support.")
    (license license:lgpl2.1+)))

(define-public libgxps
  (package
    (name "libgxps")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "07156nj7yrp3h9zyzx6mjwxwmfijialb4gi5d6dwyp53arr8c9vd"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection pkg-config))
    (inputs
     (list gtk+ libjpeg-turbo lcms libtiff))
    (propagated-inputs
     ;; In Requires of libgxps.pc.
     (list cairo glib libarchive))
    (home-page "https://wiki.gnome.org/Projects/libgxps")
    (synopsis "GObject-based library for handling and rendering XPS documents")
    (description
     "libgxps is a GObject-based library for handling and rendering XPS
documents.  This package also contains binaries that can convert XPS documents
to other formats.")
    (license license:lgpl2.1+)))

(define-public gnome-characters
  (package
    (name "gnome-characters")
    (version "46.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/"
                           "gnome-characters/" (version-major version)
                           "/gnome-characters-" version ".tar.xz"))
       (sha256
        (base32
         "0z42blzj4kp0vgwqdaf8fip28i3qag6yg94lk55j0j5z232y5s54"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'disable-gtk-update-icon-cache
                     (lambda _
                       (substitute* "meson.build"
                         (("gtk_update_icon_cache: true")
                          "gtk_update_icon_cache: false")
                         (("update_desktop_database: true")
                          "update_desktop_database: false"))))
                   (add-after 'unpack 'fix-test-setup
                     (lambda _
                       (substitute* "tests/meson.build"
                         (("'GI_TYPELIB_PATH': (.*)," all path)
                          (string-append "'GI_TYPELIB_PATH':"
                                         " ["
                                         path
                                         ", '"
                                         (getenv "GI_TYPELIB_PATH")
                                         "'],")))))
                   (add-after 'install 'wrap
                     (lambda* (#:key outputs #:allow-other-keys)
                       ;; GNOME Characters needs Typelib files from GTK and
                       ;; gnome-desktop.
                       (wrap-program (search-input-file outputs
                                                        "bin/gnome-characters")
                         `("GI_TYPELIB_PATH" ":" prefix
                           (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config
           xorg-server-for-tests
           xvfb-run))
    (inputs
     (list bash-minimal  ;for wrap-program
           gjs
           gnome-desktop
           gtk
           libadwaita
           libunistring))
    (home-page "https://wiki.gnome.org/Apps/Characters")
    (synopsis "Find and insert unusual characters")
    (description "Characters is a simple utility application to find
and insert unusual characters.  It allows you to quickly find the
character you are looking for by searching for keywords.")
    (license license:bsd-3)))

(define-public gnome-common
  (package
    (name "gnome-common")
    (version "3.18.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1kzqi8qvh5p1zncj8msazlmvcwsczjz2hqxp4x2y0mg718vrwmi2"))))
    (build-system gnu-build-system)
    (home-page "https://www.gnome.org/")
    (synopsis "Bootstrap GNOME modules built from Git")
    (description "gnome-common contains various files needed to bootstrap
GNOME modules built from Git.  It contains a common \"autogen.sh\" script that
can be used to configure a source directory checked out from Git and some
commonly used macros.")
    (license license:gpl2+)))

(define-public gnome-contacts
  (package
    (name "gnome-contacts")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-contacts/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pvhcdngaibl4lc1cmqpnsyr6v9pqbfa4mvyrjdpzix16plb9bkh"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; FIXME: Cannot build the Valadoc, because both gtk+ and gtk are in the
      ;; same profile (evolution-data-server propagates both).
      #:configure-flags #~'("-Ddocs=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")))))))
    (native-inputs
     (list desktop-file-utils
           docbook-xml
           docbook-xml-4.2
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           libxslt
           pkg-config))
    (inputs
     (list evolution-data-server
           gnome-desktop
           gnome-online-accounts
           gst-plugins-base
           gtk
           libadwaita
           libgee
           libportal
           qrencode
           telepathy-glib
           vala))
    (propagated-inputs
     (list folks
           telepathy-mission-control))
    (synopsis "GNOME's integrated address book")
    (description
     "GNOME Contacts organizes your contact information from online and
offline sources, providing a centralized place for managing your contacts.")
    (home-page "https://wiki.gnome.org/Apps/Contacts")
    (license license:gpl2+)))

(define-public gnome-desktop
  (package
    (name "gnome-desktop")
    (version "44.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0hlxqprraiwnccf98dykbhx80j31c3scdi7i3jy19fl4bms77is2"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-path
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libgnome-desktop/gnome-languages.c"
               (("\"locale\"")
                (format #f "~s" (search-input-file inputs "bin/locale"))))))
         (add-before 'configure 'patch-bubblewrap
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libgnome-desktop/gnome-desktop-thumbnail-script.c"
               (("\"bwrap\",")
                (string-append "\"" (which "bwrap") "\","))
               (("\"--ro-bind\", \"/usr\", \"/usr\",")
                (string-append "\"--ro-bind\", \""
                               (%store-directory)
                               "\", \""
                               (%store-directory)
                               "\","))
               (("\"--ro-bind\", \"/etc/ld.so.cache\", \"/etc/ld.so.cache\",")
                ""))))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server and locales.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")
             (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
             (setenv "XDG_CONFIG_HOME" "/tmp")
             (setenv "GUIX_LOCPATH"
                     (search-input-directory inputs
                                             "lib/locale")))))))
    (native-inputs
     (list `(,glib "bin")                   ;for gdbus-codegen
           glibc-locales                    ;for tests
           gobject-introspection
           itstool
           intltool
           pkg-config
           libxml2
           xorg-server-for-tests))
    (propagated-inputs
     ;; Required by gnome-desktop-3.0.pc.
     (list gsettings-desktop-schemas
           gtk
           gtk+
           iso-codes/pinned
           libseccomp
           libx11
           xkeyboard-config))
    (inputs
     (list bubblewrap
           gdk-pixbuf
           glib
           libxext
           libxkbfile
           libxrandr))
    (home-page "https://www.gnome.org/")
    (synopsis
     "Libgnome-desktop, gnome-about, and desktop-wide documents")
    (description
     "The libgnome-desktop library provides API shared by several applications
on the desktop, but that cannot live in the platform for various reasons.
There is no API or ABI guarantee, although we are doing our best to provide
stability.  Documentation for the API is available with gtk-doc.

The gnome-about program helps find which version of GNOME is installed.")
                                        ; Some bits under the LGPL.
    (license license:gpl2+)))

(define-public gnome-disk-utility
  (package
    (name "gnome-disk-utility")
    (version "46.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1zmzkfwg1gvqz16i2lplnvi5yhq5f1617jj9wgxbqw2dl0wr8kn2"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dlogind=libelogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")
               (("glib_compile_schemas: true")
                "glib_compile_schemas: false")
               (("update_desktop_database: true")
                "update_desktop_database: false")))))))
    (native-inputs
     (list docbook-xml
           docbook-xsl
           `(,glib "bin")
           gettext-minimal
           pkg-config
           libxml2
           libxslt))
    (inputs
     (list elogind
           glib
           appstream-glib
           gnome-settings-daemon
           gtk+
           libcanberra
           libdvdread
           libhandy
           libnotify
           libpwquality
           libsecret
           udisks))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-disk-utility")
    (synopsis "Disk management utility for GNOME")
    (description "Disk management utility for GNOME.")
    (license license:gpl2+)))

(define-public gnome-font-viewer
  (package
    (name "gnome-font-viewer")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-font-viewer/"
                                  (version-major version)
                                  "/gnome-font-viewer-" version ".tar.xz"))
              (sha256
               (base32
                "0z2k5nyrizzib657c5k2hnk1v5jcw34bays89l2cq0jx90g40bsr"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-post-install-script
            (lambda _
              (substitute* "meson-postinstall.sh"
                (("update-desktop-database") (which "true")))))
          (add-after 'install 'patch-thumbnailer
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute*
                  (search-input-file
                   outputs "share/thumbnailers/gnome-font-viewer.thumbnailer")
                (("gnome-thumbnail-font")
                 (search-input-file outputs "bin/gnome-thumbnail-font"))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config))
    (inputs
     (list glib
           gnome-desktop
           gtk
           libadwaita
           libxml2))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-font-viewer")
    (synopsis "GNOME Fonts")
    (description "Application to show you the fonts installed on your computer
for your use as thumbnails.  Selecting any thumbnails shows the full view of how
the font would look under various sizes.")
    (license license:gpl2+)))

(define-public gcr
  (package
    (name "gcr")
    (version "4.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rbxjrwy88l1b6yml2hrracqamaflvif7a9fq1cd0g1ph1f3ny7d"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-fatal-warnings-option
            ;; Otherwise, the gi-docgen tool would fail because of the
            ;; "Fontconfig error: No writable cache directories" warnings.
            (lambda _
              (substitute* (find-files "." "^meson\\.build$")
                ((".*'--fatal-warnings',.*") ""))))
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
                    (add-before 'check 'pre-check
            (lambda _
              ;; Some tests expect to write to $HOME.
              (setenv "HOME" "/tmp")))
          (replace 'check
            (lambda* (#:key parallel-tests? tests? #:allow-other-keys)
              (when tests?
                (setenv "MESON_TESTTHREADS"
                        (if parallel-tests?
                            (number->string (parallel-job-count))
                            "1"))
                ;; Work around the "mock prompter couldn't get session bus
                ;; address: Cannot spawn a message bus without a machine-id"
                ;; error by manually creating the session bus via
                ;; 'dbus-run-session'.
                (invoke "dbus-run-session" "--"
                        "meson" "test" "-t" "0")))))))
    (inputs
     (list dbus
           gnupg
           libgcrypt
           libsecret))
    (native-inputs
     (list gettext-minimal
           gi-docgen
           `(,glib "bin")
           gobject-introspection
           gtk-doc
           libxml2
           libxslt
           openssh
           pkg-config
           python-wrapper
           vala))
    ;; GLib and p11-kit are mentioned in gck.pc and gcr.pc
    ;; GTK is kept for symmetry with gcr-3, which propagates gtk+.
    (propagated-inputs (list glib gtk p11-kit))
    (home-page "https://www.gnome.org")
    (synopsis "Libraries for displaying certificates and accessing key stores")
    (description
     "The GCR package contains libraries used for displaying certificates and
accessing key stores.  It also provides the viewer for crypto files on the
GNOME Desktop.")
    (license license:lgpl2.1+)))

(define-public gcr-3
  (package
    (inherit gcr)
    (name "gcr")
    (version "3.41.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1dqsgrb62fgmy4w63bjl3b525nil4idrrdcscia1h3isaly0zlds"))))
    (arguments
     (substitute-keyword-arguments (package-arguments gcr)
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'skip-gtk-update-icon-cache
              ;; Don't create 'icon-theme.cache'.
              (lambda _
                (substitute* "meson_post_install.py"
                  (("gtk-update-icon-cache") "true"))))))))
    ;; mentioned in gck.pc, gcr.pc and gcr-ui.pc
    (propagated-inputs (list glib gtk+ p11-kit))))

(define-public gdl
  (package
    (name "gdl")
    (version "3.40.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/gdl.git")
                    (commit (string-append "GDL_" (string-map (match-lambda
                                                                (#\. #\_)
                                                                (c c))
                                                              version)))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11hp93gqk7m64h84q5hndzlwj4w6hl0cbmzrk2pkdn04ikm2zj4v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           `(,glib "bin") ; for glib-genmarshal, etc.
           gnome-common
           gtk-doc/stable
           intltool
           pkg-config
           libtool
           which))
    (inputs
     (list libxml2))
    (propagated-inputs
     ;; The gdl-3.0.pc file 'Requires' GTK+.
     (list gtk+))
    (home-page "https://gitlab.gnome.org/GNOME/gdl/")
    (synopsis "GNOME docking library")
    (description "This library provides docking features for gtk+.")
    (license license:lgpl2.1+)))

;;; A minimal variant used to break a cycle with Inkscape.
(define-public gdl-minimal
  (package/inherit gdl
    (name "gdl-minimal")
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-doc-generation
           ;; XXX: There is no easy way to disable generating the
           ;; documentation.
           (lambda _
             (substitute* "configure.in"
               (("GTK_DOC_CHECK.*") "")
               (("docs/.*") ""))
             (substitute* "Makefile.am"
               (("gdl docs po") "gdl po"))
             #t)))))
    (native-inputs (alist-delete "gtk-doc" (package-native-inputs gdl)))))

(define-public libgnome-keyring
  (package
    (name "libgnome-keyring")
    (version "3.12.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnome/sources/" name "/"
                                 (version-major+minor version)  "/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "10vpjhgbjm7z2djy04qakd02qlzpd02xnbfjhk2aqwjzn3xpihf4"))))
    (build-system gnu-build-system)
    (inputs
     (list libgcrypt dbus))
    (native-inputs
     (list pkg-config
           `(,glib "bin") intltool))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     (list glib))
    (home-page "https://www.gnome.org")
    (synopsis "Accessing passwords from the GNOME keyring")
    (description
     "Client library to access passwords from the GNOME keyring.")

    ;; Though a couple of files are LGPLv2.1+.
    (license license:lgpl2.0+)))

(define-public gnome-keyring
  (package
    (name "gnome-keyring")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "098ryv7xsnf5r58w8kdr6nahzhmrczjb72ycbqlg7dx8p1kcj9mz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         (string-append "--with-pkcs11-config="
                        #$output "/share/p11-kit/modules/")
         (string-append "--with-pkcs11-modules="
                        #$output "/share/p11-kit/modules/"))
      #:parallel-tests? #f              ; XXX: concurrency in dbus tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-/bin/sh-reference
            (lambda _
              (substitute* "po/Makefile.in.in"
                (("/bin/sh") (which "sh")))))
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")  ;some tests require a writable HOME
                (setenv "XDG_DATA_DIRS" (string-append (getenv "XDG_DATA_DIRS")
                                                       ":" #$output "/share"))
                (invoke "dbus-run-session" "make" "check" "-j"
                        (if parallel-tests?
                            (number->string (parallel-job-count))
                            "1"))))))))
    (inputs
     (list dbus
           gcr-3
           libgcrypt
           linux-pam
           openssh))
    (native-inputs
     (list dbus                         ;for tests
           docbook-xml-4.3
           docbook-xml
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           glib                         ;for m4 macros
           libxslt                      ;for documentation
           pkg-config
           python-wrapper))             ;for tests
    (propagated-inputs
     (list gcr-3))

    ;; XXX: There are concerning test failures on i686-linux and other 32-bit
    ;; platforms: <https://gitlab.gnome.org/GNOME/gnome-keyring/-/issues/124>.
    (supported-systems %64bit-supported-systems)

    (home-page "https://www.gnome.org")
    (synopsis "Daemon to store passwords and encryption keys")
    (description
     "@command{gnome-keyring} is a program that keeps passwords and other
secrets for users.  It is run as a daemon in the session, similar to
@command{ssh-agent}, and other applications locate it via an environment
variable or D-Bus.

The program can manage several keyrings, each with its own master password,
and there is also a session keyring which is never stored to disk, but
forgotten when the session ends.")
    (license license:lgpl2.1+)))

(define-public evince
  (package
    (name "evince")
    (version "46.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/evince/"
                           (version-major version) "/"
                           "evince-" version ".tar.xz"))
       (sha256
        (base32 "0z7fxv3ikhj1kxprrsb4hbd801p9b0a1fa976gav0f9qyak20p4l"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:build-type "release"
       #:configure-flags '("-Dnautilus=false"
                           "-Dps=enabled")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("(glib_compile_schemas|gtk_update_icon_cache|\
update_desktop_database): true" _ tool)
                (string-append tool ": false"))))))))
    (inputs
     (list libarchive
           libgxps
           libspectre
           djvulibre
           ghostscript
           poppler
           libtiff
           texlive-libkpathsea          ; for DVI support
           gnome-desktop
           gsettings-desktop-schemas
           gspell
           libgnome-keyring
           adwaita-icon-theme
           gdk-pixbuf
           at-spi2-core
           pango
           gtk+
           glib
           libxml2
           libsm
           libice
           shared-mime-info
           dconf
           libcanberra
           libsecret
           libhandy))
    (native-inputs
     (list itstool
           gettext-minimal
           gi-docgen
           `(,glib "bin")
           gobject-introspection
           pkg-config
           libxml2))
    (home-page "https://wiki.gnome.org/Apps/Evince")
    (synopsis "GNOME's document viewer")
    (description
     "Evince is a document viewer for multiple document formats.  It
currently supports PDF, PostScript, DjVu, TIFF and DVI.  The goal
of Evince is to replace the multiple document viewers that exist
on the GNOME Desktop with a single simple application.")
    (license license:gpl2+)))

(define-public gsettings-desktop-schemas
  (package
    (name "gsettings-desktop-schemas")
    (version "46.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0qca8rqfhiyqb5al27h1cz39rl7ipd4jx0dxdfz8x5d66wa1124v"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-schemas
                    (lambda* (#:key inputs #:allow-other-keys)
                      (substitute* (find-files "schemas"
                                               "\\.gschema\\.xml\\.in$")
                        ;; Provide the correct file name of the default
                        ;; GNOME background, 'adwaita-timed.xml'.
                        (("@datadir@/backgrounds/gnome")
                         (search-input-directory inputs
                                                 "/share/backgrounds/gnome"))
                        ;; Do not reference fonts, that may not exist.
                        (("'Source Code Pro 10'") "'Monospace 11'")))))))
    (inputs (list glib gnome-backgrounds gobject-introspection))
    (native-inputs (list gettext-minimal
                         `(,glib "bin") ;glib-compile-schemas, etc.
                         gobject-introspection
                         pkg-config
                         python))  ;for build-aux/meson/post-install.py
    (home-page "https://launchpad.net/gsettings-desktop-schemas")
    (synopsis "GNOME settings for various desktop components")
    (description "Gsettings-desktop-schemas contains a collection of GSettings
schemas for settings shared by various components of the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public python-liblarch
  (package
    (name "python-liblarch")
    (version "3.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/getting-things-gnome/liblarch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "125rmrdbc84lapfh8c77zxnmwas20xdfamqmilhv1smkxn2q4sh3"))))
    (build-system python-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'check 'start-xserver
                    (lambda* (#:key inputs #:allow-other-keys)
                      (system (format #f "~a/bin/Xvfb :1 &"
                                      (assoc-ref inputs "xorg-server")))
                      (setenv "DISPLAY" ":1"))))))
    (native-inputs (list xorg-server-for-tests))
    (inputs (list gtk+))
    (propagated-inputs (list python-pygobject))
    (home-page "https://wiki.gnome.org/Projects/liblarch")
    (synopsis "Library to easily handle complex data structures")
    (description
     "Liblarch is a Python library built to easily handle data structures such
as lists, trees and acyclic graphs.  There's also a GTK binding that will
allow you to use your data structure in a @code{Gtk.Treeview}.

Liblarch support multiple views of one data structure and complex filtering.
That way, you have a clear separation between your data themselves (Model)
and how they are displayed (View).")
    (license license:lgpl3+)))

(define-public gtg
  (package
    (name "gtg")
    (version "0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/getting-things-gnome/gtg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wcivqp4z3a2jdr2wdrldzwy9v6hlfp5sk0pmh80znlgvl7q3jiv"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/gtg")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                       ,(python:site-packages inputs outputs)))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           pkg-config
           python))
    (inputs
     (list bash-minimal
           (librsvg-for-system)
           gsettings-desktop-schemas
           gtk+
           gtksourceview-4
           pango
           python
           python-dbus
           python-liblarch
           python-lxml
           python-pycairo
           python-pygobject
           python-pyxdg))
    (home-page "https://wiki.gnome.org/Apps/GTG")
    (synopsis "Personal organizer for the GNOME desktop")
    (description
     "Getting Things GNOME! (GTG) is a personal tasks and TODO list items
organizer for the GNOME desktop environment inspired by the Getting Things
Done (GTD) methodology.  GTG is designed with flexibility, adaptability,
and ease of use in mind so it can be used as more than just GTD software.
GTG is intended to help you track everything you need to do and need to
know, from small tasks to large projects.")
    (license license:gpl3+)))

(define-public icon-naming-utils
  (package
    (name "icon-naming-utils")
    (version "0.8.90")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://tango.freedesktop.org/releases/icon-naming-utils-"
                          version ".tar.bz2"))
      (sha256
       (base32
        "1mc3v28fdfqanx3lqx233vcr4glb4c2376k0kx2v91a4vxwqcdxi"))))
    (build-system gnu-build-system)
    (inputs
     (list perl perl-xml-simple))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'set-load-paths
           ;; Tell 'icon-name-mapping' where XML::Simple is.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (prog (string-append out "/libexec/icon-name-mapping")))
               (wrap-program
                   prog
                 `("PERL5LIB" = ,(list (getenv "PERL5LIB")))))
             #t)))))
    (home-page "http://tango.freedesktop.org/Standard_Icon_Naming_Specification")
    (synopsis
     "Utility to implement the Freedesktop Icon Naming Specification")
    (description
     "To help with the transition to the Freedesktop Icon Naming
Specification, the icon naming utility maps the icon names used by the
GNOME and KDE desktops to the icon names proposed in the specification.")
    (license license:lgpl2.1+)))

(define-public adwaita-icon-theme
  (package
    (name "adwaita-icon-theme")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1jxmjq7jvbkf6rv01y2vjx2g8iic7gkxa6085rvblfck8awjdcdy"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-meson
                 ;; Don't create 'icon-theme.cache'.
                 (lambda _ (substitute* "meson.build"
                        (("gtk4?-update-icon-cache") "true")))))))
    (home-page "https://gitlab.gnome.org/GNOME/adwaita-icon-theme")
    (synopsis "GNOME icon theme")
    (description "Icons for the GNOME desktop.")
    (license license:lgpl3))) ; or Creative Commons BY-SA 3.0

(define-public gnome-icon-theme
  (deprecated-package "gnome-icon-theme" adwaita-icon-theme))

(define-public tango-icon-theme
  (package
    (name "tango-icon-theme")
    (version "0.8.90")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://tango.freedesktop.org/releases/"
                                  "tango-icon-theme-" version ".tar.bz2"))
              (sha256
               (base32
                "034r9s944b4yikyfgn602yv7s54wdzlq0qfvqh52b9x6kbx08h79"))))
    (build-system gnu-build-system)
    (native-inputs
     (list icon-naming-utils intltool imagemagick pkg-config))
    (home-page "http://tango-project.org/")
    (synopsis "Tango icon theme")
    (description "This is an icon theme that follows the Tango visual
guidelines.")
    (license license:public-domain)))

(define-public system-config-printer
  (package
    (name "system-config-printer")
    (version "1.5.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/OpenPrinting/system-config-printer/releases/"
             "download/v" version
             "/system-config-printer-" version ".tar.xz"))
       (sha256
        (base32 "1z9pvgifj5c87csnqz10qybbcayh3ak9m606f63ifkvyjh4q9jnb"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules `((guix build python-build-system)
                           ,@%glib-or-gtk-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-files
            (lambda _
              (substitute* "configure.ac"
                (("AC_INIT.*" all)
                 (string-append all "\nAC_CONFIG_MACRO_DIR([m4])\n"))
                ;; XXX: AX macros appear unavailable
                (("AX_REQUIRE_DEFINED.*") ""))
              ;; The Makefile generates some scripts, so set a valid shebang
              (substitute* "Makefile.am"
                (("/bin/bash") (which "bash")))
              (delete-file "configure")))
          #$@(if (this-package-native-input "config")
                 #~((add-after 'unpack 'update-config-scripts
                      (lambda* (#:key native-inputs inputs #:allow-other-keys)
                        (for-each (lambda (file)
                                    (install-file
                                      (search-input-file
                                        (or native-inputs inputs)
                                        (string-append "/bin/" file)) "."))
                                  '("config.guess" "config.sub")))))
                 #~())
          (add-after 'install 'add-install-to-pythonpath
            (@@ (guix build python-build-system) add-install-to-pythonpath))
          (add-after 'add-install-to-pythonpath 'wrap-for-python
            (@@ (guix build python-build-system) wrap))
          (add-after 'install 'wrap
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out               (assoc-ref outputs "out"))
                    (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
                (for-each
                 (lambda (program)
                   (wrap-program program
                     `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                 (map (lambda (name)
                        (string-append out "/bin/" name))
                      '("system-config-printer"
                        "system-config-printer-applet"
                        "install-printerdriver"
                        "scp-dbus-service")))))))))
    (inputs
     (list gsettings-desktop-schemas
           gobject-introspection
           python
           cups
           gtk+
           python-dbus
           python-pygobject
           python-pycups
           python-requests
           python-pycairo
           libnotify
           packagekit))
    (native-inputs
     (append
       (if (target-riscv64?)
           (list config)
           '())
       (list pkg-config
             desktop-file-utils
             glib
             autoconf
             automake
             gettext-minimal
             xmlto
             docbook-xml-4.1.2
             docbook-xsl
             libxml2)))
    (home-page "https://github.com/zdohnal/system-config-printer")
    (synopsis "CUPS administration tool")
    (description
     "system-config-printer is a CUPS administration tool.  It's written in
Python using GTK+, and uses the @acronym{IPP, Internet Printing Protocol} when
configuring CUPS.")
    (license license:gpl2+)))

(define-public hicolor-icon-theme
  (package
    (name "hicolor-icon-theme")
    (version "0.17")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://icon-theme.freedesktop.org/releases/"
                          "hicolor-icon-theme-" version ".tar.xz"))
      (sha256
       (base32
        "1n59i3al3zx6p90ff0l43gzpzmlqnzm6hf5cryxqrlbi48sq8x1i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f)) ; no check target
    (home-page "https://icon-theme.freedesktop.org/releases/")
    (synopsis
     "Freedesktop icon theme")
    (description
     "Freedesktop icon theme.")
    (license license:gpl2)))

(define-public libnotify
  (package
    (name "libnotify")
    (version "0.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1h7nn9pz797bfmpz3d0s46yjv4ydppnzwifzdx0d6shm8vwkx3zf"))))
    (outputs '("out" "doc"))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'move-doc
            (lambda _
              (let* ((old (string-append #$output "/share/doc"))
                     (new (string-append #$output:doc "/share/doc")))
                (mkdir-p (dirname new))
                (rename-file old new)))))))
    (propagated-inputs (list gdk-pixbuf glib)) ;in Requires of libnotify.pc.
    (inputs (list gtk+ libpng))
    (native-inputs
     (list pkg-config
           `(,glib "bin")
           gobject-introspection

           ;; For the documentation.
           gi-docgen
           gtk-doc/stable
           libxslt
           docbook-xsl))
    (home-page "https://gitlab.gnome.org/GNOME/libnotify/")
    (synopsis "GNOME desktop notification library")
    (description
     "Libnotify is a library that sends desktop notifications to a
notification daemon, as defined in the Desktop Notifications spec.  These
notifications can be used to inform the user about an event or display
some form of information without getting in the user's way.")
    (license license:lgpl2.1+)))

(define-public libpeas
  (package
    (name "libpeas")
    (version "1.36.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0k3v4c9xs7pxpckkagl9ba70nlxl2n23w6ixc8bqd3ndrk1bjz19"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~'("-Dvapi=true" "-Dgtk_doc=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xserver
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((disp ":1"))
                (setenv "DISPLAY" disp)
                (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache")
                (setenv "XDG_CONFIG_HOME" "/tmp")
                ;; Tests require a running X server.
                (system (format #f "~a ~a &"
                                (search-input-file inputs "bin/Xvfb")
                                disp))))))))
    (inputs
     (list gtk+
           glade3
           python
           python-pygobject))
    (native-inputs
     (list pkg-config
           gettext-minimal
           gi-docgen
           `(,glib "bin")
           gobject-introspection
           xorg-server-for-tests
           vala))
    (propagated-inputs
     ;; The .pc file "Requires" gobject-introspection.
     (list glib gobject-introspection))
    (home-page "https://wiki.gnome.org/Projects/Libpeas")
    (synopsis "GObject plugin system")
    (description
     "Libpeas is a gobject-based plugin engine, targeted at giving every
application the chance to assume its own extensibility.  It also has a set of
features including, but not limited to: multiple extension points; on-demand
(lazy) programming language support for C, Python and JS; simplicity of the
API.")
    (license license:lgpl2.0+)))

(define-public libpeas-2
  (package
    (inherit libpeas)
    (name "libpeas")
    (version "2.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1pg6km41bp9ayr6z9pi40nc6mkw2ccdxkcdsvl9lxd9isxrjyvrp"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libpeas)
       ((#:configure-flags flags #~(list))
        #~(cons* "-Dlua51=false" #$flags))))
    (inputs
     (list gtk
           gjs
           glade3
           ;; lua-5.1
           ;; lua5.1-lgi
           python
           python-pygobject))))

(define-public gtkglext
  (package
    (name "gtkglext")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gtkglext/gtkglext/"
                                  version "/gtkglext-" version ".tar.gz"))
              (sha256
               (base32 "1ya4d2j2aacr9ii5zj4ac95fjpdvlm2rg79mgnk7yvl1dcy3y1z5"))
              (patches (search-patches
                        "gtkglext-disable-disable-deprecated.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Fix a collision between a local variable and a definition from
         ;; glibc's string.h.
         (add-after 'unpack 'fix-collision
           (lambda _
             (substitute* "gdk/gdkglshapes.c"
               ((" index") " triangle_index"))
             #t)))))
    (inputs (list gtk+-2 mesa glu libx11 libxt))
    (native-inputs (list pkg-config
                         `(,glib "bin")))
    (propagated-inputs (list pangox-compat))
    (home-page "https://projects.gnome.org/gtkglext")
    (synopsis "OpenGL extension to GTK+")
    (description "GtkGLExt is an OpenGL extension to GTK+.  It provides
additional GDK objects which support OpenGL rendering in GTK+ and GtkWidget
API add-ons to make GTK+ widgets OpenGL-capable.")
    (license license:lgpl2.1+)))

(define-public glade3
  (package
    (name "glade")
    (version "3.40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "171x7vshhw0nqgnhkcaqfylpr5qrmhclwmkva6wjm5s9m2pavj9i"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson_post_install.py"
                (("gtk-update-icon-cache") "true"))))

          #$@(if (this-package-input "gjs")
                 '()
                 '((add-after 'unpack 'skip-gjs-test
                     (lambda _
                       ;; When the optional dependency on GJS is missing, skip
                       ;; the GJS plugin tests.
                       (substitute* "tests/modules.c"
                         (("g_test_add.*JavaScript.*" all)
                          (string-append "// " all "\n")))
                       (delete-file "tests/catalogs/gjsplugin.xml")))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              ;; Tests require a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (inputs
     (append
      ;; GJS depends on Rust so remove the GJS dependency on other platforms.
      (if (supported-package? gjs)
          (list gjs)
          '())
      (list gtk+ libxml2)))
    (native-inputs
     (list at-spi2-core                          ;for tests
           docbook-xml-4.2
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           hicolor-icon-theme
           itstool
           libxslt                      ;for xsltproc
           python
           python-pygobject
           pkg-config
           xorg-server-for-tests))
    (home-page "https://glade.gnome.org")
    (synopsis "GTK+ rapid application development tool")
    (description "Glade is a rapid application development (RAD) tool to
enable quick & easy development of user interfaces for the GTK+ toolkit and
the GNOME desktop environment.")
    (license license:lgpl2.0+)
    (native-search-paths (list (search-path-specification
                                (variable "GLADE_CATALOG_SEARCH_PATH")
                                (files '("share/glade/catalogs")))
                               (search-path-specification
                                (variable "GLADE_MODULE_SEARCH_PATH")
                                (files '("lib/glade/modules")))))))

(define-public blueprint-compiler
  (package
    (name "blueprint-compiler")
    (version "0.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://gitlab.gnome.org/jwestman/blueprint-compiler")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1y40kf9yfrjlfr5ax27j7ksv27fsznl7jhvvkzbfifdymjv10wqn"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%meson-build-system-modules
        (guix build python-build-system))
      #:modules
      `((guix build meson-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")))
          (add-after 'install 'wrap-python
            (assoc-ref python:%standard-phases 'wrap))
          (add-after 'wrap-python 'gi-wrap
            (lambda _
              (let ((prog (string-append #$output "/bin/blueprint-compiler")))
                (wrap-program prog
                  `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs (list gtk
                         libadwaita
                         python
                         python-pygobject
                         xorg-server-for-tests))
    (inputs (list python))
    (synopsis "Template markup language")
    (description
     "Blueprint is a markup language for GTK user interfaces.  Internally, it
compiles to GTKBuilder XML.")
    (home-page "https://gitlab.gnome.org/jwestman/blueprint-compiler")
    (license license:lgpl3+)))

(define-public blueprint-compiler-0.4
  (package
    (inherit blueprint-compiler)
    (name "blueprint-compiler")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://gitlab.gnome.org/jwestman/blueprint-compiler")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0hj7f4xhwjc4x32r3lswwclbw37fw3spy806g4plkmym25hz4ydy"))))
    (arguments
     (substitute-keyword-arguments (package-arguments blueprint-compiler)
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'fix-tests)))))))

(define-public cambalache
  (package
    (name "cambalache")
    (version "0.12.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/jpu/cambalache")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1da8d5msk4ivmk5inaq8w0m78dsp7crarr9jmybag1c8qmqsjq4h"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules `((guix build python-build-system)
                           ,@%meson-build-system-modules)
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:tests? #f                       ; XXX: tests spawn a socket...
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "cambalache/cmb_view.py"
                (("GLib\\.find_program_in_path\\('(.*)'\\)" all cmd)
                 (format #f "~s" (search-input-file
                                  inputs (string-append "bin/" cmd)))))))
          (add-after 'unpack 'patch-build
            (lambda _
              (substitute* "meson.build"
                (("find_program\\('gtk-update-icon-cache'.*\\)") "")
                (("find_program\\('update-desktop-database'.*\\)") ""))
              (substitute* "postinstall.py"
                (("gtk-update-icon-cache") "true")
                (("update-desktop-database") "true"))))
          (add-after 'unpack 'fake-cc
            (lambda _
              (substitute* "tools/cmb_init_dev.py"
                (("\"cc") (string-append "\"" #$(cc-for-target))))))
          (add-after 'install 'python-wrap (assoc-ref python:%standard-phases 'wrap))
          (delete 'check)
          (add-after 'install 'add-install-to-pythonpath
            (assoc-ref python:%standard-phases 'add-install-to-pythonpath))
          (add-after 'add-install-to-pythonpath 'pre-check
            (lambda _
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")))
          (add-after 'pre-check 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion ".."
                  (invoke "python3" "-m" "pytest")))))
          (add-after 'glib-or-gtk-wrap 'wrap-typelib
            (lambda _
              (for-each
               (lambda (prog)
                 (unless (wrapped-program? prog)
                   (wrap-program prog
                     `("GI_TYPELIB_PATH" suffix
                       (,(string-append #$output "/lib/girepository-1.0")
                        ,(getenv "GI_TYPELIB_PATH")))
                     ;; icons and schemas
                     `("XDG_DATA_DIRS" suffix
                       #$(map
                          (lambda (input)
                            (file-append (this-package-input input) "/share"))
                          '("adwaita-icon-theme" "hicolor-icon-theme"
                            "gsettings-desktop-schemas")))
                     ;; Wrapping GDK_PIXBUF_MODULE_FILE allows Cambalache to
                     ;; load its own icons in pure environments.
                     `("GDK_PIXBUF_MODULE_FILE" =
                       (,(getenv "GDK_PIXBUF_MODULE_FILE"))))))
               (find-files (string-append #$output "/bin"))))))))
    (inputs
     (list bash-minimal
           adwaita-icon-theme hicolor-icon-theme
           gsettings-desktop-schemas
           gtk
           gtksourceview-4
           `(,gtk+ "bin")               ; broadwayd
           `(,gtk "bin")
           libadwaita
           libhandy
           (librsvg-for-system)
           python
           python-pycairo
           python-pygobject
           python-lxml
           webkitgtk-for-gtk3
           webkitgtk))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           gettext-minimal
           pkg-config
           python-pytest
           weston
           xorg-server-for-tests))
    (home-page "https://gitlab.gnome.org/jpu/cambalache")
    (synopsis "Rapid application development tool")
    (description "Cambalache is a @acronym{RAD, rapid application development}
tool for Gtk 4 and 3 with a clear @acronym{MVC, model-view-controller} design
and data model first philosophy.")
    (license (list license:lgpl2.1
                   license:gpl2)))) ; tools

(define-public libcroco
  (package
    (name "libcroco")
    (version "0.6.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "libcroco-CVE-2020-12825.patch"))
              (sha256
               (base32
                "1m110rbj5d2raxcdp4iz0qp172284945awrsbdlq99ksmqsc4zkn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libxml2 zlib))
    (home-page "https://github.com/GNOME/libcroco")
    (synopsis "CSS2 parsing and manipulation library")
    (description
     "Libcroco is a standalone CSS2 parsing and manipulation library.
The parser provides a low level event driven SAC-like API and a CSS object
model like API.  Libcroco provides a CSS2 selection engine and an experimental
XML/CSS rendering engine.")

    ;; LGPLv2.1-only.
    (license license:lgpl2.1)))

(define-public libgsf
  (package
    (name "libgsf")
    (version "1.14.53")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1fmwx2lz07sp2hj6yaf5x0m3yqlzjv0qqkgykjn9f3y5w239md8f"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "bin" "doc"))
    (arguments
     (list #:configure-flags
           #~(list
              "--disable-static"
              "--enable-introspection"
              (string-append "--with-gir-dir=" #$output
                             "/share/gir-"
                             #$(version-major
                                (package-version gobject-introspection))
                             ".0")
              (string-append "--with-typelib-dir=" #$output
                             "/lib/girepository-"
                             #$(version-major
                                (package-version gobject-introspection))
                             ".0")
              (string-append "--with-html-dir=" #$output
                             "/share/gtk-doc/html")
              "--with-zlib"
              "--with-bz2")))
    (native-inputs
     (list docbook-xml
           gettext-minimal
           gobject-introspection
           perl
           perl-xml-parser
           pkg-config
           python-wrapper))
    (inputs
     (list bzip2
           gdk-pixbuf
           zlib))
    (propagated-inputs
     (list glib
           libxml2))
    (synopsis "G Structured File Library")
    (description "Libgsf aims to provide an efficient extensible I/O abstraction
for dealing with different structured file formats.")
    (home-page "https://gitlab.gnome.org/GNOME/libgsf")
    (license
     (list license:lgpl2.1+             ;library
           license:lgpl2.0+))))         ;others

(define-public librsvg
  (package
    (name "librsvg")
    (version "2.58.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/librsvg/"
                                  (version-major+minor version)  "/"
                                  "librsvg-" version ".tar.xz"))
              (sha256
               (base32
                "0ym2yg94sc7ralh1kwqqrhz3wcc51079z90mbx0qrls7wfh36hi2"))))
    (build-system cargo-build-system)
    (outputs '("out" "doc" "debug"))
    (arguments
     (list
      #:install-source? #f
      #:modules
      '((guix build cargo-build-system)
        (guix build utils)
        ((guix build gnu-build-system) #:prefix gnu:))
      #:cargo-inputs
      `(("rust-anyhow" ,rust-anyhow-1)
        ("rust-cairo-rs" ,rust-cairo-rs-0.19)
        ("rust-cast" ,rust-cast-0.3)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-clap" ,rust-clap-4)
        ("rust-clap-complete" ,rust-clap-complete-4)
        ("rust-cssparser" ,rust-cssparser-0.31)
        ("rust-cstr" ,rust-cstr-0.2)
        ("rust-data-url" ,rust-data-url-0.3)
        ("rust-encoding-rs" ,rust-encoding-rs-0.8)
        ("rust-float-cmp" ,rust-float-cmp-0.9)
        ("rust-gdk-pixbuf" ,rust-gdk-pixbuf-0.19)
        ("rust-gio" ,rust-gio-0.19)
        ("rust-glib" ,rust-glib-0.19)
        ("rust-image" ,rust-image-0.24)
        ("rust-itertools" ,rust-itertools-0.12)
        ("rust-language-tags" ,rust-language-tags-0.3)
        ("rust-libc" ,rust-libc-0.2)
        ("rust-locale-config" ,rust-locale-config-0.3)
        ("rust-markup5ever" ,rust-markup5ever-0.11)
        ("rust-nalgebra" ,rust-nalgebra-0.32)
        ("rust-num-traits" ,rust-num-traits-0.2)
        ("rust-pango" ,rust-pango-0.19)
        ("rust-pangocairo" ,rust-pangocairo-0.19)
        ("rust-rayon" ,rust-rayon-1)
        ("rust-rctree" ,rust-rctree-0.6)
        ("rust-regex" ,rust-regex-1)
        ("rust-rgb" ,rust-rgb-0.8)
        ("rust-selectors" ,rust-selectors-0.25)
        ("rust-string-cache" ,rust-string-cache-0.8)
        ("rust-system-deps" ,rust-system-deps-6)
        ("rust-thiserror" ,rust-thiserror-1)
        ("rust-tinyvec" ,rust-tinyvec-1)
        ("rust-url" ,rust-url-2)
        ("rust-xml5ever" ,rust-xml5ever-0.17)
        ("rust-yeslogic-fontconfig-sys" ,rust-yeslogic-fontconfig-sys-5))
      #:cargo-development-inputs
      `(("rust-anyhow" ,rust-anyhow-1)
        ("rust-assert-cmd" ,rust-assert-cmd-2)
        ("rust-chrono" ,rust-chrono-0.4)
        ("rust-criterion" ,rust-criterion-0.5)
        ("rust-float-cmp" ,rust-float-cmp-0.9)
        ("rust-lopdf" ,rust-lopdf-0.32)
        ("rust-matches" ,rust-matches-0.1)
        ("rust-png" ,rust-png-0.17)
        ("rust-predicates" ,rust-predicates-3)
        ("rust-proptest" ,rust-proptest-1)
        ("rust-quick-error" ,rust-quick-error-2)
        ("rust-serde" ,rust-serde-1)
        ("rust-serde-json" ,rust-serde-json-1)
        ("rust-tempfile" ,rust-tempfile-3)
        ("rust-url" ,rust-url-2))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-gdk-pixbuf-thumbnailer
            (lambda* (#:key inputs #:allow-other-keys)
              ;; The gdk-pixbuf-thumbnailer location is assumed to be relative
              ;; to librsvg's own installation prefix (see:
              ;; https://gitlab.gnome.org/GNOME/librsvg/-/issues/955).
              (substitute* "gdk-pixbuf-loader/librsvg.thumbnailer.in"
                (("@bindir@/gdk-pixbuf-thumbnailer")
                 (string-append #$(this-package-input "gdk-pixbuf")
                                "/bin/gdk-pixbuf-thumbnailer")))))
          (add-after 'unpack 'prepare-for-build
            (lambda _
              ;; In lieu of #:make-flags
              (setenv "CC" #$(cc-for-target))
              (setenv "PKG_CONFIG" #$(pkg-config-for-target))
              #$@(if (%current-target-system)
                     #~((setenv "RUST_TARGET"
                                #$(platform-rust-target
                                    (lookup-platform-by-target
                                      (%current-target-system)))))
                     #~())
              ;; Something about the build environment resists building
              ;; successfully with the '--locked' flag.
              (substitute* '("Makefile.am" "Makefile.in")
                (("--locked") ""))))
          (add-after 'unpack 'loosen-test-boundaries
            (lambda _
              ;; Increase reftest tolerance a bit to account for different
              ;; harfbuzz, pango, etc.
              (setenv "RSVG_TEST_TOLERANCE" "20")
              ;; These tests fail even after loosening the tolerance.
              (substitute* "rsvg/tests/reference.rs"
                ((".*svg1_1_filters_conv_0[24]_f_svg.*") "")
                ((".*rtl_tspan_svg.*") ""))))
          (add-before 'configure 'pre-configure
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "gdk-pixbuf-loader/Makefile.in"
                ;; By default the gdk-pixbuf loader is installed under
                ;; gdk-pixbuf's prefix.  Work around that.
                (("gdk_pixbuf_moduledir = .*$")
                 (string-append "gdk_pixbuf_moduledir = "
                                "$(prefix)/"
                                #$(dirname %gdk-pixbuf-loaders-cache-file) "/"
                                "loaders\n")))
              (substitute* "configure"
                (("gdk_pixbuf_cache_file=.*")
                 (string-append "gdk_pixbuf_cache_file="
                                #$output "/"
                                #$%gdk-pixbuf-loaders-cache-file "\n")))))
          (add-after 'configure 'gnu-configure
            (lambda* (#:key outputs #:allow-other-keys #:rest args)
              (apply (assoc-ref gnu:%standard-phases 'configure)
                     #:configure-flags
                     (list "--disable-static"
                           #$@(if (%current-target-system)
                                #~(;; g-ir-scanner can't import its modules
                                   ;; and vala currently can't be cross-compiled.
                                   "--enable-introspection=no"
                                   "--enable-vala=no"
                                   ;; These two are necessary for cross-compiling.
                                   (string-append
                                     "--build=" #$(nix-system->gnu-triplet
                                                    (%current-system)))
                                   (string-append
                                     "--host=" #$(%current-target-system)))
                                #~("--enable-vala")))
                     args)))
          (add-after 'configure 'dont-vendor-self
            (lambda* (#:key vendor-dir #:allow-other-keys)
              ;; Don't keep the whole tarball in the vendor directory
              (delete-file-recursively
               (string-append vendor-dir "/" #$name "-" #$version ".tar.xz"))))
          (replace 'build
            (assoc-ref gnu:%standard-phases 'build))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys #:rest args)
              (when tests?
                ((assoc-ref gnu:%standard-phases 'check)
                 #:test-target "check"))))
          (replace 'install
            (assoc-ref gnu:%standard-phases 'install)))))
    (native-inputs (list gdk-pixbuf `(,glib "bin") gobject-introspection pkg-config vala))
    (inputs (list freetype gobject-introspection harfbuzz libxml2 pango))
    (propagated-inputs (list cairo gdk-pixbuf glib))
    (synopsis "SVG rendering library")
    (description "Librsvg is a library to render SVG images to Cairo surfaces.
GNOME uses this to render SVG icons.  Outside of GNOME, other desktop
environments use it for similar purposes.  Wikimedia uses it for Wikipedia's SVG
diagrams.")
    (home-page "https://wiki.gnome.org/LibRsvg")
    (license license:lgpl2.1+)))

(define-public librsvg-2.40
  ;; This is the last version implemented in C.
  (package
    (inherit librsvg)
    (version "2.40.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/librsvg/"
                                  (version-major+minor version)
                                  "/librsvg-" version ".tar.xz"))
              (sha256
               (base32
                "1fljkag2gr7c4k5mn798lgf9903xslz8h51bgvl89nnay42qjqpp"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags '(list "--disable-static")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'pre-configure
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "gdk-pixbuf-loader/Makefile.in"
                ;; By default the gdk-pixbuf loader is installed under
                ;; gdk-pixbuf's prefix.  Work around that.
                (("gdk_pixbuf_moduledir = .*$")
                 (string-append "gdk_pixbuf_moduledir = "
                                "$(prefix)/lib/gdk-pixbuf-2.0/2.10.0/"
                                "loaders\n"))
                ;; Drop the 'loaders.cache' file, it's in gdk-pixbuf+svg.
                (("gdk_pixbuf_cache_file = .*$")
                 "gdk_pixbuf_cache_file = $(TMPDIR)/loaders.cache\n"))))
          (add-before 'check 'fix-test-with-pango-1.50
            (lambda _
	      ;; Changes between pango 1.48 and 1.50 caused the text to be one
	      ;; pixel lower in the output image compared to the reference.
              (substitute* "tests/fixtures/reftests/bugs/587721-text-transform.svg"
	        (("660\\.9") "659.9"))))
          (add-before 'check 'remove-failing-tests
            (lambda _
              (with-directory-excursion "tests/fixtures/reftests"
                (for-each delete-file
                          '( ;; This test fails on i686:
                            "svg1.1/masking-path-04-b.svg"
                            ;; This test fails on armhf:
                            "svg1.1/masking-mask-01-b.svg"
                            ;; This test fails on aarch64:
                            "bugs/777834-empty-text-children.svg"
                            ;; These two tests fail due to slightly different
                            ;; text rendering (different kerning or similar),
                            ;; nothing alarming.
                            "bugs/340047.svg"
                            "bugs/749415.svg"
                            ;; These two tests fail with the update to cairo
                            ;; version 1.18.0.
                            "bugs/587721-text-transform.svg"
                            "svg1.1/masking-path-03-b.svg"))))))))
    (native-inputs
     (list pkg-config
           `(,glib "bin") ; glib-mkenums, etc.
           gobject-introspection)) ; g-ir-compiler, etc.
    (inputs
     (list pango libcroco libxml2))
    (propagated-inputs
     ;; librsvg-2.0.pc refers to all of that.
     (list cairo gdk-pixbuf glib))
    (synopsis "Render SVG files using Cairo (ancient C version)")
    (properties '((hidden? . #t)))))

(define* (librsvg-for-system #:optional
                             (system (or (%current-target-system)
                                         (%current-system))))
  ;; Since librsvg 2.50 depends on Rust, and Rust is only correctly supported
  ;; on x86_64, aarch64 and riscv64 so far, use the ancient C version on other
  ;; platforms (FIXME).
  (if (supported-package? librsvg)
      librsvg
      librsvg-2.40))

(export librsvg-for-system)

(define-public libidl
  (package
    (name "libidl")
    (version "0.8.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libIDL/"
                                  (version-major+minor version) "/"
                                  "libIDL-" version ".tar.bz2"))
              (sha256
               (base32
                "08129my8s9fbrk0vqvnmx6ph4nid744g5vbwphzkaik51664vln5"))))
    (build-system gnu-build-system)
    (inputs (list glib))
    (native-inputs
     (list pkg-config flex bison))
    (home-page "http://freecode.com/projects/libidl")
    (synopsis "Create trees of CORBA Interface Definition Language files")
    (description  "Libidl is a library for creating trees of CORBA Interface
Definition Language (idl) files, which is a specification for defining
portable interfaces. libidl was initially written for orbit (the orb from the
GNOME project, and the primary means of libidl distribution).  However, the
functionality was designed to be as reusable and portable as possible.")
    (properties `((upstream-name . "libIDL")))
    (license license:lgpl2.0+)))

(define-public orbit2
  (package
    (name "orbit2")
    (version "2.14.19")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "ORBit2"))
                     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "0l3mhpyym9m5iz09fz0rgiqxl2ym6kpkwpsp1xrr4aa80nlh1jam"))
              (patches
               (search-patches "orbit2-fix-array-allocation-32bit.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The "timeout-server" test hangs when run in parallel.
      #:parallel-tests? #f
      #:configure-flags
      #~'(;; We don't need static libraries, plus they don't build reproducibly
          ;; (non-deterministic ordering of .o files in the archive.)
          "--disable-static"

          ;; The programmer kindly gives us a hook to turn off deprecation
          ;; warnings ...
          "DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
      ;; ... which they then completly ignore !!
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-parallel-build
            ;; Parallel build fails because of a failed dependency,
            ;; https://bugzilla.gnome.org/show_bug.cgi?id=732274
            (lambda _
              (substitute* "src/services/name/Makefile.am"
                (("orbit_name_server_2_DEPENDENCIES = \\$(DEPS) CosNaming.h")
                 "orbit_name_server_2_DEPENDENCIES = \
$(DEPS) CosNaming.h libname-server-2.a"))))
          (add-before 'configure 'ignore-deprecations
            (lambda _
              (substitute* "linc2/src/Makefile.in"
                (("-DG_DISABLE_DEPRECATED")
                 "-DGLIB_DISABLE_DEPRECATION_WARNINGS")))))))
    ;; These are required in the installed pkg-config files.
    (propagated-inputs (list glib libidl))
    (native-inputs (list pkg-config))
    (home-page "https://projects.gnome.org/orbit2/")
    (synopsis "CORBA 2.4-compliant Object Request Broker")
    (description  "ORBit2 is a CORBA 2.4-compliant Object Request Broker (orb)
featuring mature C, C++ and Python bindings.")
    ;; Licence notice is unclear.  The Web page simply say "GPL" without giving
    ;; a version.  SOME of the code files have licence notices for GPLv2+.
    ;; The tarball contains files of the text of GPLv2 and LGPLv2.
    (license license:gpl2+)
    (properties `((upstream-name . "ORBit2")))))

(define-public libbonobo
  (package
    (name "libbonobo")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)
                                  "/" name "-" version ".tar.bz2"))
              (sha256
               (base32 "0swp4kk6x7hy1rvd1f9jba31lvfc6qvafkvbpg9h0r34fzrd8q4i"))
              (patches (search-patches
                        "libbonobo-activation-test-race.patch"))))
    (build-system gnu-build-system)
    (arguments
     ;; The programmer kindly gives us a hook to turn off deprecation warnings ...
     `(#:configure-flags
       '("DISABLE_DEPRECATED_CFLAGS=-DGLIB_DISABLE_DEPRECATION_WARNINGS")
       ;; ... which they then completly ignore !!
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* "activation-server/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))

       ;; There's apparently a race condition between the server stub
       ;; generation and linking of the example under 'samples/echo' that can
       ;; lead do undefined references when building in parallel, as reported
       ;; at <https://forums.gentoo.org/viewtopic-t-223376-start-550.html>.
       ;; Thus, disable parallel builds.
       #:parallel-build? #f))
    (inputs (list popt libxml2))
    ;; The following are Required by the .pc file
    (propagated-inputs
     (list glib orbit2))
    (native-inputs
     (list intltool
           pkg-config
           `(,glib "bin") ; for glib-genmarshal, etc.
           flex
           bison))
    (home-page "https://developer.gnome.org/libbonobo/")
    (synopsis "Framework for creating reusable components for use in GNOME applications")
    (description "Bonobo is a framework for creating reusable components for
use in GNOME applications, built on top of CORBA.")
    ;; Licence not explicitly stated.  Source files contain no licence notices.
    ;; Tarball contains text of both GPLv2 and LGPLv2
    ;; GPLv2 covers both conditions
    (license license:gpl2+)))

(define-public gconf
  (package
    (name "gconf")
    (version "3.2.6")
    (source (origin
              (method url-fetch)
              (uri
               (let ((upstream-name "GConf"))
                 (string-append "mirror://gnome/sources/" upstream-name "/"
                                (version-major+minor version) "/"
                                upstream-name "-" version ".tar.xz")))
              (sha256
               (base32 "0k3q9nh53yhc9qxf1zaicz4sk8p3kzq4ndjdsgpaa2db0ccbj4hr"))))
    (build-system gnu-build-system)
    (inputs (list dbus-glib libxml2))
    (propagated-inputs (list glib ; referred to in the .pc file
                             orbit2))
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    (home-page "https://projects.gnome.org/gconf/")
    (synopsis "Store application preferences")
    (description "Gconf is a system for storing application preferences.  It
is intended for user preferences; not arbitrary data storage.")
    (license license:lgpl2.0+)
    (properties '((upstream-name . "GConf")))))

(define-public gnome-mime-data
  (package
    (name "gnome-mime-data")
    (version "2.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1mvg8glb2a40yilmyabmb7fkbzlqd3i3d31kbkabqnq86xdnn69p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl intltool))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'configure 'use-our-intltool
                    (lambda _
                      ;; Do not use the bundled intltool commands, which lack
                      ;; the "dotless @INC" fixes of our 'intltool' package.
                      (substitute* (find-files "." "^Makefile$")
                        (("^INTLTOOL_(EXTRACT|UPDATE|MERGE) = .*$" _ tool)
                         (string-append "INTLTOOL_" tool " = intltool-"
                                        (string-downcase tool) "\n")))
                      #t)))))
    (home-page "https://www.gnome.org")
    (synopsis "Base MIME and Application database for GNOME")
    (description  "GNOME Mime Data is a module which contains the base MIME
and Application database for GNOME.  The data stored by this module is
designed to be accessed through the MIME functions in GnomeVFS.")
    (license license:gpl2+)))

(define-public gnome-vfs
  (package
    (name "gnome-vfs")
    (version "2.24.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1ajg8jb8k3snxc7rrgczlh8daxkjidmcv3zr9w809sq4p2sn9pk2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'ignore-deprecations
           (lambda _
             (substitute* '("libgnomevfs/Makefile.in"
                            "daemon/Makefile.in")
               (("-DG_DISABLE_DEPRECATED")
                "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))))
         (add-before 'configure 'patch-test-async-cancel-to-never-fail
           (lambda _
             (substitute* "test/test-async-cancel.c"
               (("EXIT_FAILURE") "77")))))))
    (inputs (list libxml2 dbus-glib gconf gnome-mime-data zlib))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums, etc.
           intltool pkg-config))
    (home-page "https://developer.gnome.org/gnome-vfs/")
    (synopsis "Access files and folders in GNOME applications")
    (description
     "GnomeVFS is the core library used to access files and folders in GNOME
applications.  It provides a file system abstraction which allows applications
to access local and remote files with a single consistent API.")
    (license license:lgpl2.0+)))

(define-public libgnome
  (package
    (name "libgnome")
    (version "2.32.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "197pnq8y0knqjhm2fg4j6hbqqm3qfzfnd0irhwxpk1b4hqb3kimj"))
              (patches (search-patches "libgnome-encoding.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'enable-deprecated
           (lambda _
             (substitute* "libgnome/Makefile.in"
               (("-DG_DISABLE_DEPRECATED") "-DGLIB_DISABLE_DEPRECATION_WARNINGS"))
             #t)))))
    (inputs (list libxml2))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums, etc.
           intltool pkg-config))
    ;; The following are listed as Required in the .pc file
    ;; (except for libcanberra -- which seems to be oversight on the part
    ;; of the upstream developers -- anything that links against libgnome,
    ;; must also link against libcanberra
    (propagated-inputs
     (list libcanberra libbonobo gconf gnome-vfs popt))                       ;gnome-program.h includes popt.h
    (home-page "https://developer.gnome.org/libgnome/")
    (synopsis "Useful routines for building applications")
    (description  "The libgnome library provides a number of useful routines
for building modern applications, including session management, activation of
files and URIs, and displaying help.")
    (license license:lgpl2.0+)))


(define-public libart-lgpl
  (package
    (name "libart-lgpl")
    (version "2.3.21")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-name "libart_lgpl"))
                     (string-append "mirror://gnome/sources/" upstream-name "/"
                                    (version-major+minor version) "/"
                                    upstream-name "-" version ".tar.bz2")))
              (sha256
               (base32
                "1yknfkyzgz9s616is0l9gp5aray0f2ry4dw533jgzj8gq5s1xhgx"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (home-page "https://people.gnome.org/~mathieu/libart")
    (synopsis "2D drawing library")
    (description  "Libart is a 2D drawing library intended as a
high-quality vector-based 2D library with antialiasing and alpha composition.")
    (license license:lgpl2.0+)))

(define-public libgnomecanvas
  (package
    (name "libgnomecanvas")
    (version "2.30.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1nhnq4lfkk8ljkdafscwaggx0h95mq0rxnd7zgqyq0xb6kkqbjm8"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs (list libart-lgpl gtk+-2))
    (native-inputs
     (list intltool
           `(,glib "bin") ; for glib-genmarshal, etc.
           pkg-config))
    (home-page "https://developer.gnome.org/libgnomecanvas/")
    (synopsis "Flexible widget for creating interactive structured graphics")
    (description  "The GnomeCanvas widget provides a flexible widget for
creating interactive structured graphics.")
    (license license:lgpl2.0+)))

(define-public libgnomecanvasmm
  (package
    (name "libgnomecanvasmm")
    (version "2.26.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0679hcnpam2gkag2i63sm0wdm35gwvzafnz1354mg6j5gzwpfrcr"))))
    (build-system gnu-build-system)
    (propagated-inputs (list libgnomecanvas))
    (native-inputs
     (list gtkmm-2 pkg-config))
    (home-page "https://gtkmm.org")
    (synopsis "C++ bindings to the GNOME Canvas library")
    (description "C++ bindings to the GNOME Canvas library.")
    (license license:lgpl2.0+)))

(define-public libgnomeui
  (package
    (name "libgnomeui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (patches (search-patches "libgnomeui-utf8.patch"))
              (sha256
               (base32
                "03rwbli76crkjl6gp422wrc9lqpl174k56cp9i96b7l8jlj2yddf"))))
    (build-system gnu-build-system)
    ;; Mentioned as Required in the .pc file
    (propagated-inputs (list libbonoboui libgnome libgnomecanvas
                             libgnome-keyring))
    (inputs (list libjpeg-turbo popt libbonobo libxml2 libglade))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums, etc.
           intltool pkg-config))
    (home-page "https://developer.gnome.org/libgnomeui/")
    (synopsis "Additional widgets for applications")
    (description "The libgnomeui library provides additional widgets for
applications.  Many of the widgets from libgnomeui have already been
ported to GTK+.")
    (license license:lgpl2.0+)))

(define-public libglade
  (package
    (name "libglade")
    (version "2.6.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1v2x2s04jry4gpabws92i0wq2ghd47yr5n9nhgnkd7c38xv1wdk4"))))
    (build-system gnu-build-system)
    (inputs
     (list python)) ;; needed for the optional libglade-convert program
    (propagated-inputs
     (list gtk+-2 libxml2)) ; required by libglade-2.0.pc
    (native-inputs
     (list pkg-config))
    (home-page "https://developer.gnome.org/libglade")
    (synopsis "Load glade interfaces and access the glade built widgets")
    (description "Libglade is a library that provides interfaces for loading
graphical interfaces described in glade files and for accessing the
widgets built in the loading process.")
    (license license:gpl2+))) ; This is correct.  GPL not LGPL

(define-public libbonoboui
  (package
    (name "libbonoboui")
    (version "2.24.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1kbgqh7bw0fdx4f1a1aqwpff7gp5mwhbaz60c6c98bc4djng5dgs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (disp ":1"))

               (setenv "HOME" (getcwd))
               (setenv "DISPLAY" disp)
               ;; There must be a running X server and make check doesn't start one.
               ;; Therefore we must do it.
               (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))))))
    ;; Mentioned as Required by the .pc file
    (propagated-inputs (list libxml2))
    (inputs
     (list popt pangox-compat libgnome libgnomecanvas libglade))
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           intltool
           xorg-server-for-tests ; For running the tests
           pkg-config))
    (home-page "https://developer.gnome.org/libbonoboui/")
    (synopsis "Some user interface controls using Bonobo")
    (description  "The Bonobo UI library provides a number of user interface
controls using the Bonobo component framework.")
    (license license:lgpl2.0+)))

(define-public libwnck
  (package
    (name "libwnck")
    (version "40.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "04b63mh2i7kpq0iymx6fkyzdx8laymw3da2s0wsbwgzg2jhly4q3"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;for glib-mkenums
           gobject-introspection        ;for g-ir-scanner
           pkg-config))
    (propagated-inputs
     (list gtk+
           libxres
           startup-notification))
    (home-page "https://gitlab.gnome.org/GNOME/libwnck/")
    (synopsis "Window Navigator Construction Kit")
    (description
     "Libwnck is the Window Navigator Construction Kit, a library for use in
writing pagers, tasklists, and more generally applications that are dealing
with window management.  It tries hard to respect the Extended Window Manager
Hints specification (EWMH).")
    (license license:lgpl2.0+)))

;; stable version for gtk2, required by xfwm4.
(define-public libwnck-2
  (package (inherit libwnck)
    (name "libwnck")
    (version "2.30.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "15713yl0f8f3p99jzqqfmbicrdswd3vwpx7r3bkf1bgh6d9lvs4b"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config intltool))
    (propagated-inputs
     (list gtk+-2 libxres startup-notification))))

(define-public goffice
  (package
    (name "goffice")
    (version "0.10.53")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/goffice/"
                           (version-major+minor version)  "/"
                           "goffice-" version ".tar.xz"))
       (sha256
        (base32 "0mrzi8bcykn1jdkvqm8zqwg8k80mafl4xhr0076d875adxwmiz97"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ; 4.0 MiB of gtk-doc
    (arguments
     '(#:configure-flags (list (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/gtk-doc/html"))))
    (inputs
     (list gtk+ libgsf (librsvg-for-system) libxslt libxml2))
    (native-inputs
     (list intltool `(,glib "bin") pkg-config))
    (home-page "https://developer.gnome.org/goffice/")
    (synopsis "Document-centric objects and utilities")
    (description "A GLib/GTK+ set of document-centric objects and utilities.")
    (license
     ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     ;; Note: NOT LGPL
     (list license:gpl2 license:gpl3))))

(define-public goffice-0.8
  (package
    (inherit goffice)
    (version "0.8.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" (package-name goffice) "/"
                                  (version-major+minor version)  "/"
                                  (package-name goffice) "-" version ".tar.xz"))
              (sha256
               (base32 "05fvzbs5bin05bbsr4dp79aiva3lnq0a3a40zq55i13vnsz70l0n"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-pcre-check
           (lambda _
             ;; Only glib.h can be included directly.  See
             ;; https://bugzilla.gnome.org/show_bug.cgi?id=670316
             (substitute* "configure"
               (("glib/gregex\\.h") "glib.h")) #t)))

       ,@(package-arguments goffice)))
    (propagated-inputs
     ;; libgoffice-0.8.pc mentions libgsf-1
     (list libgsf))
    (inputs (modify-inputs (package-inputs goffice)
              (replace "gtk+" gtk+-2)))))

(define-public gnumeric
  (package
    (name "gnumeric")
    (version "1.12.52")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnumeric/"
                                  (version-major+minor version)  "/"
                                  "gnumeric-" version ".tar.xz"))
              (sha256
               (base32
                "0fw201j0sks95wgvns3vydgprhwf6z4v4xb2a0ldi892k8277kvk"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(;; The gnumeric developers don't worry much about failing tests.
       ;; See https://bugzilla.gnome.org/show_bug.cgi?id=732387
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before
          'configure 'pre-conf
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make install tries to write into the directory of goffice
             ;; I am informed that this only affects the possibility to embed a
             ;; spreadsheet inside an Abiword document.   So presumably when we
             ;; package Abiword we'll have to refer it to this directory.
             (substitute* "configure"
               (("^GOFFICE_PLUGINS_DIR=.*")
                (string-append "GOFFICE_PLUGINS_DIR="
                               (assoc-ref outputs "out")
                               "/goffice/plugins"))))))))
    (inputs
     (list glib
           gtk+
           goffice
           libgsf
           (librsvg-for-system)
           libxml2
           libxslt
           python
           python-pygobject
           zlib))
    (native-inputs
     (list bison
           docbook-xml
           `(,glib "bin")
           intltool
           itstool
           pkg-config))
    (home-page "http://www.gnumeric.org")
    (synopsis "Spreadsheet application")
    (description
     "GNUmeric is a GNU spreadsheet application, running under GNOME.  It is
interoperable with other spreadsheet applications.  It has a vast array of
features beyond typical spreadsheet functionality, such as support for linear
and non-linear solvers, statistical analysis, and telecommunication
engineering.")
    (license
    ;; Dual licensed under GPLv2 or GPLv3 (both without "or later")
     (list license:gpl2 license:gpl3))))

(define-public drawing
  (package
    (name "drawing")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/maoschanz/drawing")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yazs3jj8i8n64ki54rvh11q0yn46da105hdsjb7b80dpxspvlch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-postinstall-script
            (lambda _
              (setenv "DESTDIR" "/")))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/drawing")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                       ,(python:site-packages inputs outputs)))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           pkg-config
           python))
    (inputs
     (list bash-minimal
           (librsvg-for-system)
           gsettings-desktop-schemas
           gtk+
           pango
           python
           python-pycairo
           python-pygobject))
    (home-page "https://maoschanz.github.io/drawing/")
    (synopsis "Basic image editor for GNOME")
    (description
     "Drawing is a basic image editor aiming at the GNOME desktop.")
    (license license:gpl3+)))

(define-public seahorse
  (package
    (name "seahorse")
    (version "47.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32 "1k6avgd58v853nchp226qc3fgz0pwxnf7744hyvynzqzlvj1f6cw"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")
               (("update_desktop_database: true")
                "update_desktop_database: false"))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a writable HOME.
             (setenv "HOME" (getcwd)))))))
    (inputs
     (list gtk+
           gcr-3
           gnupg
           gpgme
           openldap
           openssh
           avahi
           libhandy
           libpwquality
           libsecret
           libsoup-minimal))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           itstool
           pkg-config
           python
           vala
           libxml2))
    (home-page "https://wiki.gnome.org/Apps/Seahorse")
    (synopsis "Manage encryption keys and passwords in the GNOME keyring")
    (description
     "Seahorse is a GNOME application for managing encryption keys and
passwords in the GNOME keyring.")
    (license license:gpl2+)))

(define-public vala
  (package
    (name "vala")
    (version "0.56.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vala/"
                                  (version-major+minor version) "/"
                                  "vala-" version ".tar.xz"))
              (sha256
               (base32
                "0spd6ill4nnfpj13qm6700yqhrgmgkcl1wbmj9hrq17h9r70q416"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags #~(list "CC=gcc" "--enable-coverage")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-gcc-by-default
            (lambda _
              (substitute* "codegen/valaccodecompiler.c"
                (("cc_command = \"cc\"")
                 "cc_command = \"gcc\""))))
          (add-before 'check 'pre-check
            (lambda _
              (substitute* "valadoc/tests/libvaladoc/tests-extra-environment.sh"
                (("export PKG_CONFIG_PATH=" m)
                 (string-append m "$PKG_CONFIG_PATH:")))))
          ;; Wrapping the binaries breaks vala's behavior adaptations based on
          ;; the file name of the program executed (vala: compile and execute,
          ;; valac: compile into a binary).
          (delete 'glib-or-gtk-wrap))))
    (native-inputs
     (list bison
           dbus                         ; for dbus tests
           docbook-xml-4.4
           docbook-xsl
           flex
           gobject-introspection        ; for gir tests
           help2man
           perl
           pkg-config
           libxslt))
    (propagated-inputs
     (list glib                         ; required by libvala-0.40.pc
           graphviz))
    (home-page "https://wiki.gnome.org/Projects/Vala/")
    (synopsis "Compiler using the GObject type system")
    (description "Vala is a programming language using modern high level
abstractions without imposing additional runtime requirements and without using
a different ABI compared to applications and libraries written in C.  Vala uses
the GObject type system and has additional code generation routines that make
targeting the GNOME stack simple.")
    (license license:lgpl2.1+)))

;;; An older variant kept to build libsoup-minimal-2.
(define-public vala-0.52
  (package/inherit vala
    (version "0.52.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vala/"
                                  (version-major+minor version) "/"
                                  "vala-" version ".tar.xz"))
              (sha256
               (base32
                "12y6p8wdjp01vmfhxg2cgh32xnyqq6ivblvrar9clnj6vc867qhx"))))))

(define-public vte
  (package
    (name "vte")
    (version "0.78.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/vte/"
                                  (version-major+minor version) "/"
                                  "vte-" version ".tar.xz"))
              (sha256
               (base32
                "144qqk638n7fbql8542a02lp0nvh2vhci098l6s4cs1m0zgbrmrm"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~(list "-Dgtk3=false"
                                     "-Dvapi=true"
                                     "-D_systemd=false")))
    (native-inputs
     (list pkg-config
           gettext-minimal
           vala
           gobject-introspection
           `(,glib "bin")               ; for glib-genmarshal, etc.
           gperf
           python
           libxml2))
    (inputs (list lz4))
    (propagated-inputs
     (list gtk                          ; required by vte-2.91.pc
           gnutls                       ; ditto
           pcre2))                      ; ditto
    (home-page "https://www.gnome.org/")
    (synopsis "Virtual Terminal Emulator")
    (description
     "VTE is a library (libvte) implementing a terminal emulator widget for
GTK+, and a minimal sample application (vte) using that.  Vte is mainly used in
gnome-terminal, but can also be used to embed a console/terminal in games,
editors, IDEs, etc.")
    (license license:lgpl2.1+)))

(define-public vte/gtk+-3
  (package/inherit vte
    (name "vte-with-gtk+3")
    (arguments (substitute-keyword-arguments (package-arguments vte)
                 ((#:configure-flags flags #~'())
                  #~(cons "-Dgtk4=false" (delete "-Dgtk3=false" #$flags)))))
    (propagated-inputs (modify-inputs (package-propagated-inputs vte)
                         (replace "gtk" gtk+)))))

(define-public vte-with-sixel
  (package/inherit vte
    (name "vte-with-sixel")
    ;; Choose a version that can render images generated by img2sixel.
    (version "0.73.92")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/vte")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32
                       "0fv6lx7kk1xrfsvc95jm23vxkmyfypriz4nvj0kjy4nshgccwlch"))))
    (arguments (substitute-keyword-arguments (package-arguments vte)
                 ((#:configure-flags flags)
                  #~(append (list "-Dsixel=true") #$flags))))
    (inputs (modify-inputs (package-inputs vte)
              (append libsixel)
              (append lz4)))))

;; Stable version for gtk2, required by gnurobots and lxterminal as of 2020-07.
(define-public vte/gtk+-2
  (package (inherit vte)
    (name "vte")
    (version "0.28.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1bmhahkf8wdsra9whd3k5l5z4rv7r58ksr8mshzajgq2ma0hpkw6"))
              (patches (search-patches
                         "vte-CVE-2012-2738-pt1.patch"
                         "vte-CVE-2012-2738-pt2.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-python")))
    (native-inputs
     (list pkg-config intltool
           `(,glib "bin")))   ; for glib-genmarshal, etc.
    (propagated-inputs
     (list gtk+-2 ; required by libvte.pc
           ncurses)))) ; required by libvte.la

(define-public vinagre
  (package
    (name "vinagre")
    (version "3.22.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "vinagre-newer-freerdp.patch"
                                       "vinagre-newer-rdp-parameters.patch"))
              (sha256
               (base32
                "10jya3jyrm18nbw3v410gbkc7677bqamax44pzgd3j15randn76d"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; Disable -Werror and such, to avoid build failures on compilation
     ;; warnings.
     '(#:configure-flags '("--enable-compile-warnings=minimum"
                           "CFLAGS=-O2 -g -fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'skip-gtk-update-icon-cache
           (lambda _
             ;; Don't create 'icon-theme.cache'
             (substitute* (find-files "." "^Makefile$")
               (("gtk-update-icon-cache") (which "true")))
             #t))
         (add-after 'unpack 'patch-configure
           (lambda _
             (substitute* "configure"
               (("freerdp") "freerdp2"))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("glib-bin" ,glib "bin")))                 ;for glib-compile-schemas
    (inputs
     (list libxml2
           gtk-vnc
           gnome-keyring
           libsecret
           freerdp
           spice
           spice-gtk
           telepathy-glib
           vte/gtk+-3))
    (home-page "https://wiki.gnome.org/Apps/Vinagre")
    (synopsis "Remote desktop viewer for GNOME")
    (description "Vinagre is a remote display client supporting the VNC, SPICE
and RDP protocols.")
    (license license:gpl3+)))

(define-public dconf
  (package
    (name "dconf")
    (version "0.40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major+minor version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0cs5nayg080y8pb9b7qccm1ni8wkicdmqp1jsgc22110r6j24zyg"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; Configure sysconfdir to /etc so that gconf profiles can be written
      ;; there and loaded without having to set GCONF_PROFILE, which cannot be
      ;; safely set globally (as a gconf profile is a per-user thing).
      #:configure-flags #~(list "--sysconfdir=/etc"
                                "-Dgtk_doc=true")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'increase-test-timeout
                     (lambda _
                       ;; On big-memory systems, the engine test may take
                       ;; much longer than the default of 30 seconds.
                       (substitute* "tests/meson.build"
                         (("test\\(unit_test\\[0\\], exe" all)
                          (string-append all ", timeout: 300"))))))))
    (native-inputs
     (list bash-completion
           libxslt                      ;for xsltproc
           docbook-xml-4.2
           docbook-xsl
           `(,glib "bin")
           gtk-doc/stable
           pkg-config
           python
           vala))
    (inputs
     (list gtk+
           dbus))
    (propagated-inputs
     ;; In Requires of dconf.pc.
     (list glib))
    (home-page "https://wiki.gnome.org/action/show/Projects/dconf")
    (synopsis "Low-level GNOME configuration system")
    (description "Dconf is a low-level configuration system.  Its main purpose
is to provide a backend to GSettings on platforms that don't already have
configuration storage systems.")
    (license license:lgpl2.1+)))

(define-public json-glib-minimal
  (package
    (name "json-glib-minimal")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/json-glib/"
                                  (version-major+minor version)
                                  "/json-glib-" version ".tar.xz"))
              (sha256
               (base32
                "0yxg215gpa61sxnx05bgzbqq9dsvbawk6cfz8z0yq1k1v5k8vjhv"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t            ;to wrap binaries, compile schemas
           #:configure-flags #~(list "-Dman=false"
                                     "-Dgtk_doc=disabled")))
    (native-inputs
     (list
      gettext-minimal
      `(,glib "bin")                     ;for glib-mkenums and glib-genmarshal
      pkg-config))
    (inputs
     (list bash-minimal))
    (propagated-inputs
     (list glib))                 ;according to json-glib-1.0.pc
    (home-page "https://wiki.gnome.org/Projects/JsonGlib")
    (synopsis "Glib and GObject implementation of JSON")
    (description "JSON-GLib is a library providing serialization and
described by RFC 4627.  It implements a full JSON parser and generator using
GLib and GObject, and integrates JSON with GLib data types.")
    (license license:lgpl2.1+)))

(define-public json-glib
  (package/inherit json-glib-minimal
    (name "json-glib")
    (outputs (cons "doc" (package-outputs json-glib-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments json-glib-minimal)
       ((#:configure-flags _)
        #~(list "-Dman=true"
                #$@(if (%current-target-system)
                       ;; If enabled, gtkdoc-scangobj will try to execute a
                       ;; cross-compiled binary.
                       #~("-Dgtk_doc=disabled"
                          ;; Trying to build introspection data when cross-compiling
                          ;; causes errors during linking.
                          "-Dintrospection=disabled")
                       #~("-Dgtk_doc=enabled"
                          "-Dintrospection=enabled"))))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            ;; When cross-compiling, there are no docs to move.
            #$@(if (%current-target-system)
                   #~((add-after 'install 'stub-docs
                        (lambda _
                          ;; The daemon doesn't like empty output paths.
                          (mkdir #$output:doc))))
                   #~((add-after 'install 'move-docs
                        (lambda _
                          (mkdir-p (string-append #$output:doc "/share"))
                          (rename-file
                           (string-append #$output "/share/doc")
                           (string-append #$output:doc
                                          "/share/doc"))))))))))
    (native-inputs
     (if (%current-target-system)
         ;; No docs, but rst2man is used for man pages.
         (modify-inputs (package-native-inputs json-glib-minimal)
           (prepend python-docutils))
         (modify-inputs (package-native-inputs json-glib-minimal)
           (prepend gi-docgen gobject-introspection
                    python-docutils))))))

(define-public libxklavier
  (package
    (name "libxklavier")
    (version "5.4")
    (source (origin
              ;; Note: There's no tarball at ftp.gnome.org for this version.
              (method git-fetch)
              (uri (git-reference
                    (url "https://anongit.freedesktop.org/git/libxklavier")
                    (commit (string-append "libxklavier-" version))))
              (sha256
               (base32
                "1w1x5mrgly2ldiw3q2r6y620zgd89gk7n90ja46775lhaswxzv7a"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-xkb-base="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             "--disable-xmodmap-support")))
    (native-inputs
     `(("glib:bin"              ,glib "bin") ; for glib-mkenums, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config"            ,pkg-config)
       ("gtk-doc" ,gtk-doc/stable)
       ("intltool" ,intltool)
       ("which" ,which)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (propagated-inputs
     ;; Required by libxklavier.pc.
     (list glib libxml2))
    (inputs
     (list iso-codes/pinned libxi libxkbfile xkbcomp xkeyboard-config))
    (home-page "https://www.freedesktop.org/wiki/Software/LibXklavier/")
    (synopsis "High-level API for X Keyboard Extension")
    (description
     "LibXklavier is a library providing high-level API for X Keyboard
Extension known as XKB.  This library is intended to support XFree86 and other
commercial X servers.  It is useful for creating XKB-related software (layout
indicators etc).")
    (license license:lgpl2.0+)))

(define-public glib-networking
  (package
    (name "glib-networking")
    (version "2.78.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/glib-networking/"
                                  (version-major+minor version) "/"
                                  "glib-networking-" version ".tar.xz"))
              (sha256
               (base32
                "17zhkf2pjwrghdgk5nhfvzqakb2xwk2jj19316xjr0s9n3djv3z4"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ; for gio-querymodules
           pkg-config gettext-minimal))
    (inputs
     (list glib gnutls gsettings-desktop-schemas libproxy))
    (home-page "https://wiki.gnome.org/Projects/GLib")
    (synopsis "Network extensions for GLib")
    (description
     "Glib-networking contains the implementations of certain GLib networking
features that cannot be implemented directly in GLib itself because of their
dependencies.  Currently it contains GnuTLS and OpenSSL-based implementations of
GTlsBackend, a libproxy-based implementation of GProxyResolver,
GLibproxyResolver, and a GNOME GProxyResolver that uses the proxy information
from the GSettings schemas in gsettings-desktop-schemas.")
    (license license:lgpl2.1+)))

(define-public raider
  (package
    (name "raider")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ADBeveridge/raider/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ll9220d6qf9m7wdi5xhq69p8h8whs7l5h5nzdhlbn99qh5388bz"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-paths
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "data/com.github.ADBeveridge.Raider.gschema.xml"
                     (("/usr/bin/shred")
                      (which "shred")))))
               (add-after 'install 'wrap-program
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (wrap-program (string-append (assoc-ref outputs "out")
                                                "/bin/raider")
                     `("GSETTINGS_SCHEMA_DIR" =
                       (,(string-append (assoc-ref outputs "out")
                                        "/share/glib-2.0/schemas")))))))))
    (native-inputs
     (list gettext-minimal
           pkg-config
           cmake
           `(,glib "bin")
           desktop-file-utils
           itstool
           gobject-introspection
           blueprint-compiler
           `(,gtk "bin")))
    (inputs
     (list libadwaita
           gtk))
    (home-page "https://github.com/ADBeveridge/raider")
    (synopsis "Securely delete your files")
    (description
     "Raider is a simple shredding program built for GNOME.  Also known as
File Shredder, it uses the GNU Core Utility called shred to securely delete
files.")
    (license license:gpl3+)))

(define-public rest
  (package
    (name "rest")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/rest/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1j81bgqmd55s5lxyaxcplym9n6xywcs1cm9wmvafsg2xiv9sl4q5"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; tests require internet connection
       #:configure-flags
       '("--with-ca-certificates=/etc/ssl/certs/ca-certificates.crt")))
    (native-inputs
     (list `(,glib "bin") gobject-introspection pkg-config))
    (propagated-inputs
     ;; rest-0.7.pc refers to all these.
     (list glib libsoup-minimal-2 libxml2))
    (home-page "https://www.gtk.org/")
    (synopsis "RESTful web api query library")
    (description
     "This library was designed to make it easier to access web services that
claim to be \"RESTful\".  It includes convenience wrappers for libsoup and
libxml to ease remote use of the RESTful API.")
    (license license:lgpl2.1+)))

(define-public rest-next
  (package
    (inherit rest)
    (name "rest")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/rest/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qy2291d2vprdbbxmf0sa98izk09nl3znzzv7lckwf6f1v0sarlj"))))
    (build-system meson-build-system)
    (arguments (substitute-keyword-arguments
                 (strip-keyword-arguments
                   '(#:tests?)
                   (package-arguments rest))
                 ((#:configure-flags _)
                  ;; Do not build the optional 'librest-demo' program as it
                  ;; depends on gtksourceview and libadwaita and thus,
                  ;; indirectly, on Rust.
                  #~(list "-Dexamples=false"))
                 ((#:phases phases '%standard-phases)
                  #~(modify-phases #$phases
                      (add-after 'unpack 'disable-problematic-tests
                        ;; These tests require networking.
                        (lambda _
                          (substitute* "tests/meson.build"
                            ((".*'flickr',.*") "")
                            ((".*'lastfm',.*") ""))))
                      (add-before 'check 'prepare-for-tests
                        (lambda _
                          (setenv "HOME" "/tmp")))))))
    (native-inputs
     (modify-inputs (package-native-inputs rest)
       (append gettext-minimal
               gi-docgen
               gsettings-desktop-schemas)))
    (inputs (list json-glib))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs rest)
       (replace "libsoup-minimal" libsoup)
       (append json-glib)))))

(define-public libshumate
  (package
    (name "libshumate")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1m3mvk38cjlkxmhkq0zg75msckylc0vzizll50ii5phw53lac9w2"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? test-options #:allow-other-keys)
                   (when tests?
                     ;; Some tests expect to write to $HOME.
                     (setenv "HOME" "/tmp")
                     (setenv "XDG_RUNTIME_DIR" "/tmp/run")
                     (setenv "XDG_CACHE_HOME" "/tmp/cache")

                     ;; Tests require a running X server.
                     (system "Xvfb :1 &")
                     (setenv "DISPLAY" ":1")

                     (apply invoke "dbus-run-session" "--" "meson" "test"
                            "--print-errorlogs" test-options)))))))
    (native-inputs
     (list gi-docgen
           `(,glib "bin")
           gobject-introspection
           gperf
           pkg-config
           ;; For tests:
           xorg-server-for-tests
           dbus
           at-spi2-core))
    (propagated-inputs
     ;; All the libraries are listed as "Requires' in the .pc file.
     (list cairo
           glib
           gtk
           json-glib
           libsoup
           protobuf-c
           sqlite))
    (home-page "https://wiki.gnome.org/Projects/libshumate")
    (synopsis "GtkWidget C library for displaying maps")
    (description "@code{libshumate} is a C library providing a
@code{GtkWidget} to display maps.  It supports numerous free map sources such
as OpenStreetMap, OpenCycleMap, OpenAerialMap and Maps.")
    (license license:lgpl2.1+)))

;;; A minimal version of libsoup used to prevent a cycle with Inkscape.
(define-public libsoup-minimal
  (package
    (name "libsoup-minimal")
    (version "3.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsoup/"
                                  (version-major+minor version) "/"
                                  "libsoup-" version ".tar.xz"))
              (sha256
               (base32
                "0f7qiahry819c3rv9r0mxybz0pn5js69klsrh76v4wyx5fmg3cff"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Ddocs=disabled")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-tests
            (lambda _
              ;; This test fails due to missing /etc/nsswitch.conf
              ;; in the build environment.
              (substitute* "tests/unix-socket-test.c"
                ((".*/sockets/unconnected.*") ""))

              ;; These fail because "subdomain.localhost" does not resolve in
              ;; the build environment.  Moreover, the hsts-test suite fails on
              ;; i686-linux because of errors from `session_get_uri' like
              ;; "Unexpected status 200 OK (expected 301 Moved Permanently)"
              ;; (see: https://gitlab.gnome.org/GNOME/libsoup/-/issues/239).
              (substitute* "tests/meson.build"
                ((".*'name': 'hsts'.*") ""))
              (substitute* "tests/hsts-db-test.c"
                ((".*/hsts-db/subdomains.*") "")))))))
    (native-inputs
     (list `(,glib "bin") ;for glib-mkenums
           gobject-introspection
           pkg-config
           python-wrapper
           vala
           curl
           gnutls ;for 'certtool'
           httpd/pinned))
    (propagated-inputs
     ;; libsoup-3.0.pc refers to all of these (except where otherwise noted)
     (list brotli
           glib
           glib-networking ; for GIO runtime modules
           libpsl
           nghttp2 ;for pkg-config
           `(,nghttp2 "lib")
           libxml2
           sqlite
           zlib))
    (inputs
     (list mit-krb5 samba/pinned))     ; For ntlm_auth support
    (home-page "https://wiki.gnome.org/Projects/libsoup")
    (synopsis "GLib-based HTTP Library")
    (description
     "LibSoup is an HTTP client/server library for GNOME.  It uses GObjects
and the GLib main loop, to integrate well with GNOME applications.")
    (license license:lgpl2.0+)
    (properties '((upstream-name . "libsoup")))))

;;; An older variant kept to build the 'rest' package.
(define-public libsoup-minimal-2
  (package
    (inherit libsoup-minimal)
    (version "2.74.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsoup/"
                                  (version-major+minor version) "/"
                                  "libsoup-" version ".tar.xz"))
              (sha256
               (base32
                "04rgv6hkyhgi7lak9865yxgbgky6gc635p7w6nhcbj64rx0prdz4"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libsoup-minimal)
       ((#:configure-flags configure-flags)
        ;; The option name changed between libsoup 2 and libsoup 3.
        #~(cons "-Dgtk_doc=false"
                (delete "-Ddocs=disabled" #$configure-flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'adjust-tests
              (lambda _
                ;; Disable the SSL test, failing since 2.68 and resolved in
                ;; libsoup 3.
                (substitute* "tests/meson.build"
                  (("[ \t]*\\['ssl', true, \\[\\]\\],") ""))))))))
    (native-inputs
     (modify-inputs (package-native-inputs libsoup-minimal)
       (replace "vala" vala-0.52)))))

(define-public libsoup
  (package/inherit libsoup-minimal
    (name "libsoup")
    (outputs (cons "doc" (package-outputs libsoup-minimal)))
    (arguments
     (substitute-keyword-arguments (package-arguments libsoup-minimal)
       ((#:configure-flags configure-flags)
        #~(cons "-Ddocs=enabled"
                ;; The default value is 'auto', meaning it could be skipped.
                (delete "-Ddocs=disabled" #$configure-flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'adjust-tests
              (lambda _
                ;; This test fails due to missing /etc/nsswitch.conf
                ;; in the build environment.
                (substitute* "tests/unix-socket-test.c"
                  ((".*/sockets/unconnected.*") ""))

                ;; These fail because "subdomain.localhost" does not resolve in
                ;; the build environment.  Moreover, the hsts-test suite fails on
                ;; i686-linux because of errors from `session_get_uri' like
                ;; "Unexpected status 200 OK (expected 301 Moved Permanently)"
                ;; (see: https://gitlab.gnome.org/GNOME/libsoup/-/issues/239).
                (substitute* "tests/meson.build"
                  ((".*'name': 'hsts'.*") ""))
                (substitute* "tests/hsts-db-test.c"
                  ((".*/hsts-db/subdomains.*") ""))))
            (add-after 'install 'move-doc
              (lambda _
                (mkdir-p (string-append #$output:doc "/share"))
                (rename-file (string-append #$output "/share/doc")
                             (string-append #$output:doc "/share/doc"))))))))
    (native-inputs (modify-inputs (package-native-inputs libsoup-minimal)
                     (prepend gettext-minimal gi-docgen)))))

(define-public libsecret
  (package
    (name "libsecret")
    (version "0.21.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libsecret/"
                    (version-major+minor version) "/"
                    "libsecret-" version ".tar.xz"))
              (patches (search-patches "libsecret-fix-test-paths.patch"))
              (sha256
               (base32
                "081bj59ws08kb261cd1w1mkdkhfbzsjbbkkrm6wllvdyhgbhhg8n"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dgtk_doc=false")        ;requires gi-docgen
      #:phases
      #~(modify-phases %standard-phases
          (delete 'check)
          (add-after 'install 'check
            (lambda* (#:key tests? test-options #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (setenv "XDG_DATA_DIRS" ;for /org/freedesktop/secrets/collection
                        (string-append #$output "/share:"
                                       (getenv "XDG_DATA_DIRS")))
                (apply invoke "dbus-run-session" "--"
                       "meson" "test" "--print-errorlogs" "-t" "0"
                       test-options)))))))
    (native-inputs
     (list dbus
           docbook-xml-4.2
           docbook-xsl
           gettext-minimal
           `(,glib "bin")               ;for gdbus-codegen, etc.
           gobject-introspection
           libxslt
           pkg-config
           python
           python-dbus
           python-pygobject
           vala))
    (propagated-inputs
     (list glib libgcrypt))             ;required by libsecret-1.pc
    (home-page "https://wiki.gnome.org/Projects/Libsecret/")
    (synopsis "GObject bindings for \"Secret Service\" API")
    (description
     "Libsecret is a GObject based library for storing and retrieving passwords
and other secrets.  It communicates with the \"Secret Service\" using DBus.")
    (license license:lgpl2.1+)))

(define-public five-or-more
  (package
    (name "five-or-more")
    (version "3.32.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/five-or-more/"
                           (version-major+minor version) "/"
                           "five-or-more-" version ".tar.xz"))
       (sha256
        (base32 "1x4ys18rn37hsavivh532py2avj9686aycnn8ys29cyyxwpdf41d"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list appstream-glib
           desktop-file-utils
           `(,glib "bin")               ; for glib-compile-resources
           intltool
           itstool
           pkg-config
           vala))
    (inputs
     (list gtk+ libgnome-games-support-1 librsvg libxml2))
    (home-page "https://wiki.gnome.org/Apps/Five%20or%20more")
    (synopsis "Logic puzzle game")
    (description "Five or More is a game where you try to align
 five or more objects of the same color and shape causing them to disappear.
 On every turn more objects will appear, until the board is full.
 Try to last as long as possible.")
    (license license:gpl2+)))

(define-public gi-docgen
  (package
    (name "gi-docgen")
    (version "2024.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "gi_docgen" version))
              (sha256
               (base32
                "0cf237ml2jhqcv1zlb35qbvjg4i8a4blawdah5s7f28iz5lmajia"))))
    (build-system python-build-system)
    (propagated-inputs (list python-jinja2
                             python-markdown
                             python-markupsafe
                             python-packaging
                             python-pygments
                             python-tomli
                             python-typogrify))
    (home-page "https://gitlab.gnome.org/GNOME/gi-docgen")
    (synopsis "Documentation tool for GObject-based libraries")
    (description "GI-DocGen is a document generator for GObject-based
libraries.  GObject is the base type system of the GNOME project.  GI-Docgen
reuses the introspection data generated by GObject-based libraries to generate
the API reference of these libraries, as well as other ancillary
documentation.")
    (license license:gpl3+)))

(define-public gnome-mines
  (package
    (name "gnome-mines")
    (version "40.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0whjwdxhyw5bvibd9qvpm2yc5g7yhy8h3rn027kv5cqwyyryj0im"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list appstream-glib
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")               ; for glib-compile-resources
           itstool
           pkg-config
           python-wrapper               ; for meson_post_install.py
           vala))
    (inputs
     (list gtk+
           libgnome-games-support-1
           librsvg
           yelp))
    (home-page "https://wiki.gnome.org/Apps/Mines")
    (synopsis "Minesweeper game")
    (description
     "Mines (previously gnomine) is a puzzle game where you locate mines
floating in an ocean using only your brain and a little bit of luck.")
    (license license:gpl2+)))

(define-public gnome-multi-writer
  (package
    (name "gnome-multi-writer")
    (version "3.35.90")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gnome-multi-writer/"
                           (version-major+minor version) "/"
                           "gnome-multi-writer-" version ".tar.xz"))
       (sha256
        (base32
         "07vgzjjdrxcp7h73z13h9agafxb4vmqx5i81bcfyw0ilw9kkdzmp"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-post-install
           (lambda _
             (substitute* "meson.build"
               (("meson.add_install_script" &) (string-append "# " &)))
             #t)))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config))
    (inputs
     (list gtk+
           glib ; for gio
           gusb
           udisks
           libgudev
           libcanberra
           polkit))
    (home-page "https://wiki.gnome.org/Apps/MultiWriter")
    (synopsis "Write to multiple USB devices at once")
    (description
     "MultiWriter can be used to write an ISO file to multiple USB devices at
once.")
    (license license:gpl2+)))

(define-public gnome-sudoku
  (package
    (name "gnome-sudoku")
    (version "42.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "13y2qphrj99b0lc7bh71is1f6i0jvyw8adfg8lv48sq2p3fv8bhx"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "build-aux/post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-resources
           itstool
           libxml2
           pkg-config
           python                       ;for 'build-aux/post_install.py'
           vala))
    (inputs
     (list gtk+
           json-glib
           libgee
           (librsvg-for-system)
           qqwing))
    (home-page "https://wiki.gnome.org/Apps/Sudoku")
    (synopsis "Japanese logic game")
    (description
     "Sudoku is a Japanese logic game that exploded in popularity in 2005.
GNOME Sudoku is meant to have an interface as simple and unobstrusive as
possible while still providing features that make playing difficult Sudoku
more fun.")
    (license license:gpl2+)))

(define-public gnome-console
  (package
    (name "gnome-console")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-console/"
                                  (version-major version) "/"
                                  "gnome-console-" version ".tar.xz"))
              (sha256
               (base32
                "0gkc3lirfb59fhbmmm8l8l3sabhriiv593vi0g4w1ckk2xqcw68n"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:configure-flags #~(list "-Dtests=true")))
    (native-inputs (list `(,glib "bin")
                         gettext-minimal
                         sassc
                         pkg-config
                         `(,gtk+ "bin")
                         desktop-file-utils))
    (inputs (list gtk
                  libadwaita
                  vte
                  libgtop
                  gsettings-desktop-schemas))
    (home-page "https://gitlab.gnome.org/GNOME/console")
    (synopsis "GNOME terminal emulator")
    (description
     "Console is a simple terminal emulator for GNOME desktop")
    (license license:gpl3+)))

(define-public gnome-terminal
  (package
    (name "gnome-terminal")
    (version "3.48.3")
    ;; download.gnome.org does not have any version for gnome-terminal more
    ;; recent than 3.44.1, but the repository has several tags newer than
    ;; that.
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/gnome-terminal")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cqh35j57a5ni4xlfjzl46kim6nbhqvxx3jql3gjk414z359i0j6"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-system
            ;; The build system looks for a dbus file from gnome-shell in the
            ;; installation tree of teh package it is configuring...
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/meson.build"
                (("gt_prefix.*'org.gnome.ShellSearchProvider2.xml'")
                 (format #f "'~a'" (search-input-file
                                    inputs "share/dbus-1/interfaces/\
org.gnome.ShellSearchProvider2.xml"))))))
          (add-before 'install 'disable-gtk-update-icon-cache
            (lambda _
              (setenv "DESTDIR" "/"))))))
    (native-inputs
     (list desktop-file-utils
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           itstool
           libxml2
           libxslt
           pkg-config
           python))
    (propagated-inputs
     (list dconf))
    (inputs
     (list gnome-shell
           gnutls
           gsettings-desktop-schemas
           gtk+
           nautilus                     ;for extension
           `(,util-linux "lib")
           vala
           vte/gtk+-3))
    (home-page "https://wiki.gnome.org/Apps/Terminal")
    (synopsis "Terminal emulator")
    (description
     "GNOME Terminal is a terminal emulator application for accessing a
UNIX shell environment which can be used to run programs available on
your system.

It supports several profiles, multiple tabs and implements several
keyboard shortcuts.")
    (license license:gpl3+)))

(define-public gnome-text-editor
  (package
    (name "gnome-text-editor")
    (version "46.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-text-editor/"
                                  (version-major version) "/"
                                  "gnome-text-editor-" version ".tar.xz"))
              (sha256
               (base32
                "0gf74krvsmfsyr7s4mqhg09x3iq6ayyd4j3lw1mfd6wh9884hnq0"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t))
    (native-inputs (list pkg-config
                         cmake-minimal
                         gettext-minimal
                         desktop-file-utils
                         appstream-glib
                         `(,glib "bin")
                         `(,gtk "bin")
                         itstool))
    (inputs (list gtk gtksourceview libadwaita enchant pcre2
                  ;; cyclic module dependency
                  (module-ref
                   (resolve-interface
                    '(gnu packages text-editors))
                   'editorconfig-core-c)))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-text-editor")
    (synopsis "GNOME text editor")
    (description
     "GNOME Text Editor is a simple text editor that focuses on session
management.  It keeps track of changes and state even if you quit the
application.  You can come back to your work even if you've never saved it to a
file.")
    (license license:gpl3+)))

(define-public colord-minimal
  (package
    (name "colord-minimal")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/colord/releases/"
                           "colord-" version ".tar.xz"))
       (sha256
        (base32 "0vwfx06k1in8hci3kdxpc3c0bh81f1vl5bp7favd3rdz4wd661vl"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags #~(list "-Dargyllcms_sensor=false" ;requires spotread
                                "-Dbash_completion=false"
                                "-Ddaemon_user=colord"
                                "-Ddocs=false"
                                "-Dlocalstatedir=/var"
                                "-Dman=false"
                                "-Dsystemd=false") ;no systemd
      ;; Apparently the tests are known to fail on big-endian systems.
      #:tests? (not (or (%current-target-system)
                        (not (target-little-endian?))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              ;; Skip the colord-test-private, which requires a *system* D-Bus
              ;; session, which wants to run as root, among other requirements
              ;; (see: https://github.com/hughsie/colord/issues/97).
              (substitute* "lib/colord/meson.build"
                ((".*test\\('colord-test-private'.*") ""))))
          (add-before 'configure 'patch-build-system
            (lambda _
              (substitute* "rules/meson.build"
                (("udev.get_pkgconfig_variable\\('udevdir'\\)")
                 (string-append "'" #$output "/lib/udev'")))))
          (add-before 'configure 'set-sqlite3-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              ;; "colormgr dump" works by invoking the "sqlite3" command.
              ;; Record its absolute file name.
              (substitute* "client/cd-util.c"
                (("\"sqlite3\"")
                 (format #f "~s" (search-input-file inputs
                                                    "bin/sqlite3")))))))))
    (native-inputs
     (list `(,glib "bin")               ; for glib-compile-resources, etc.
           gettext-minimal
           pkg-config
           vala))
    (propagated-inputs
     ;; colord.pc refers to all these.
     (list glib
           lcms
           eudev))
    (inputs
     (list dbus-glib
           gobject-introspection
           gusb-minimal
           libgudev
           libusb
           polkit
           python-wrapper
           sqlite))
    (home-page "https://www.freedesktop.org/software/colord/")
    (synopsis "Color management service")
    (description "Colord is a system service that makes it easy to manage,
install and generate color profiles to accurately color manage input and
output devices.")
    (license license:gpl2+)))

(define-public colord
  (package/inherit colord-minimal
    (name "colord")
    (version "1.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/colord/releases/"
                           "colord-" version ".tar.xz"))
       (sha256
        (base32 "0vwfx06k1in8hci3kdxpc3c0bh81f1vl5bp7favd3rdz4wd661vl"))))
    (arguments
     (substitute-keyword-arguments (package-arguments colord-minimal)
       ((#:configure-flags flags)
        #~(begin
            (use-modules (srfi srfi-1))
            (append '("-Dbash_completion=true"
                      "-Ddocs=true"
                      "-Dman=true"
                      "-Dsane=true"
                      "-Dvapi=true")
                    (fold delete #$flags '("-Dbash_completion=false"
                                           "-Ddocs=false"
                                           "-Dman=false")))))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'fix-bash-completion-dir
              (lambda _
                (substitute* "data/meson.build"
                  (("bash_completion.get_pkgconfig_variable\
\\('completionsdir'\\)")
                   (string-append "'" #$output
                                  "/etc/bash_completion.d'")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs colord-minimal)
       (append bash-completion
               docbook-xsl
               gtk-doc/stable
               libxslt
               sane-backends
               vala)))))                ;for VAPI, needed by simple-scan

(define-public geoclue
  (package
    (name "geoclue")
    (version "2.7.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://gitlab.freedesktop.org/geoclue/geoclue/-/archive/"
                       version "/geoclue-" version ".tar.bz2"))
       (sha256
        (base32 "1ljn4k1zlfx0ymmdz8ycfb976vx8r61sx68q854r0xinl124mlh1"))
       (patches (search-patches "geoclue-config.patch"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags (list "-Ddbus-srv-user=geoclue")))
    (native-inputs
     (list pkg-config
           gobject-introspection
           modem-manager
           libnotify
           gtk-doc/stable
           gettext-minimal
           vala))
    (inputs
     (list avahi
           `(,glib "bin")
           glib-networking
           json-glib
           libsoup-minimal))
    (home-page "https://gitlab.freedesktop.org/geoclue/geoclue/-/wikis/home")
    (synopsis "Geolocation service")
    (description "Geoclue is a D-Bus service that provides location
information.  The primary goal of the Geoclue project is to make creating
location-aware applications as simple as possible, while the secondary goal is
to ensure that no application can access location information without explicit
permission from user.")
    (license license:gpl2+)))

(define-public geocode-glib
  (package
    (name "geocode-glib")
    (version "3.26.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/geocode-glib/"
                                  (version-major+minor version) "/"
                                  "geocode-glib-" version ".tar.xz"))
              (sha256
               (base32
                "1aipd82qk404qy88pyfgplzgi83db4hi51vkl54h8isqs4k6i6id"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Dsoup2=false")
      #:phases
      #~(modify-phases %standard-phases
          ;; The tests require a bunch of locales.
          (add-before 'check 'set-locales
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "GUIX_LOCPATH"
                      (search-input-directory inputs "lib/locale")))))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-mkenums
           glibc-locales                ;for tests
           gettext-minimal
           gobject-introspection
           gtk-doc/stable
           pkg-config
           json-glib))
    (propagated-inputs
     ;; geocode-glib-2.0.pc refers to GIO.
     (list glib))
    (inputs
     (list libsoup))
    (home-page "https://github.com/GNOME/geocode-glib/")
    (synopsis "Geocoding and reverse-geocoding library")
    (description
     "geocode-glib is a convenience library for geocoding (finding longitude,
and latitude from an address) and reverse geocoding (finding an address from
coordinates) using the Nominatim service.  geocode-glib caches requests for
faster results and to avoid unnecessary server load.")
    (license license:lgpl2.0+)))

(define-public geocode-glib-with-libsoup2
  (package
    (inherit geocode-glib)
    (name "geocode-glib-with-libsoup2")
    (arguments (substitute-keyword-arguments (package-arguments geocode-glib)
                 ((#:configure-flags flags ''())
                  #~(delete "-Dsoup2=false" #$flags))))
    (inputs (modify-inputs (package-inputs geocode-glib)
              (replace "libsoup" libsoup-minimal-2)))))

(define-public upower
  (package
    (name "upower")
    (version "1.90.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/upower/upower")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13xp423ycv8imf2cmgf6lii9f01p7x2v19cny7acrmczkc0cqv7d"))
       (modules '((guix build utils)))
       (snippet
        ;; Upstream commit <https://cgit.freedesktop.org/upower/commit/
        ;; ?id=18457c99b68786cd729b315723d680e6860d9cfa> moved
        ;; 'dbus-1/system.d' from etc/ to share/.  However,
        ;; 'dbus-configuration-directory' in (gnu services dbus) expects it in
        ;; etc/.  Thus, move it back to its previous location.
        #~(substitute* "src/meson.build"
            (("dbusdir / 'system.d'")
             "get_option('sysconfdir') / 'dbus-1/system.d'")
            ;; Avoid writing to /var during the build, this is
            ;; not possible in Guix!
            (("^install_subdir\\('does-not-exist'.*$") "")))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list "-Dsystemdsystemunitdir=no"
              ;; If not specified, udev will try putting history information
              ;; in /gnu/store.
              "-Dhistorydir=/var/lib/upower"
              (string-append "-Dudevrulesdir=" #$output "/bin/udev/rules.d")
              (string-append "-Dudevhwdbdir=" #$output "/lib/udev/hwdb.d"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-test-suite
            (lambda _
              ;; This test calls an unimplemented bluez dbus method.
              (substitute* "src/linux/integration-test.py"
                (("test_bluetooth_hidpp_mouse")
                 "disabled_test_bluetooth_hidpp_mouse"))
              #$@(if (target-x86-32?)
                     ;; Address test failure caused by excess precision
                     ;; on i686:
                     ;; <https://gitlab.freedesktop.org/upower/upower/-/issues/214>.
                     '((substitute* "src/linux/integration-test.py"
                         (("assertEqual(.*)40\\.0" _ middle)
                          (string-append
                           "assertAlmostEqual" middle "40.0"))))
                     '()))))))
    (native-inputs
     (list `(,glib "bin")               ; for gdbus-codegen
           gobject-introspection
           gtk-doc
           intltool
           pkg-config
           python
           ;; For tests.
           python-dbus
           python-dbusmock-minimal
           python-packaging
           python-pygobject
           umockdev
           dbus
           ;; For man pages.
           docbook-xsl
           libxslt))                    ; for 'xsltproc'
    (inputs
     (list libgudev libusb))
    (propagated-inputs
     ;; In Requires of upower-glib.pc.
     (list glib))
    (home-page "https://upower.freedesktop.org/")
    (synopsis "System daemon for managing power devices")
    (description
     "UPower is an abstraction for enumerating power devices,
listening to device events and querying history and statistics.  Any
application or service on the system can access the org.freedesktop.UPower
service via the system message bus.")
    (license license:gpl2+)
    ;; Old versions of upower are tagged as UPOWER_0_99_13, which confuses
    ;; the 'generic-git' updater.  Give it a little help.
    (properties '((release-tag-prefix . "v")
                  (release-tag-version-delimiter . ".")))))

(define-public libgweather
  (package
    (name "libgweather")
    (version "4.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "10s2pyf96yj287929px8jfbkda7bn76vzr2mqgyx3xydadvnf5vh"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:tests? #f                    ;one of two tests requires network access
      #:configure-flags
      #~(list (string-append "-Dzoneinfo_dir="
                             (search-input-directory %build-inputs
                                                     "share/zoneinfo")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-home
            (lambda _
              ;; Build writes to $HOME via fontconfig.
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;for glib-mkenums
           gobject-introspection
           gi-docgen
           pkg-config
           python
           vala
           python-pygobject))
    (propagated-inputs
     ;; gweather-3.0.pc refers to GTK+, GDK-Pixbuf, GLib/GObject, libxml, and
     ;; libsoup.
     (list gtk+
           gdk-pixbuf
           json-glib
           libxml2
           libsoup
           geocode-glib))
    (inputs
     (list tzdata))
    (home-page "https://gnome.pages.gitlab.gnome.org/libgweather/")
    (synopsis "Location, time zone, and weather library for GNOME")
    (description
     "libgweather is a library to access weather information from online
services for numerous locations.")
    (license license:gpl2+)))

;; libgweather no longer follows the GNOME version, and recommends changing
;; the package name in distributions to avoid accidental downgrades.  See
;; <https://discourse.gnome.org/t/changes-in-libgweather-for-gnome-42/7770/2>.
;; TODO: how to prevent the updater from picking version 40?
(define-public libgweather4
  (package
    (inherit libgweather)
    (name "libgweather4")
    (version "4.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgweather/"
                                  (version-major+minor version) "/"
                                  "libgweather-" version ".tar.xz"))
              (sha256
               (base32
                "00v2rb9dizfvcsq3bgrz68bsi1k04ln5fqhx1q06m5yql0nq32mg"))))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dzoneinfo_dir="
                             (search-input-directory %build-inputs
                                                     "share/zoneinfo")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-HOME
            (lambda _
              (setenv "HOME" "/tmp")))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (substitute* "libgweather/tests/meson.build"
                ;; The timezones test fails for unknown reasons (see:
                ;; https://gitlab.gnome.org/GNOME/libgweather/-/issues/188).
                ((".*'name': 'timezones'.*") "")
                ;; The 'metar' test is known to fail, fixed but not yet released
                ;; upstream (see:
                ;; https://gitlab.gnome.org/GNOME/libgweather/-/issues/168).
                ((".*'name': 'metar'.*") ""))))
          (delete 'check)               ;move after the install phase
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check)))))
    (native-inputs
     (list gettext-minimal
           gi-docgen
           `(,glib "bin")               ;for glib-mkenums
           gobject-introspection
           (libc-utf8-locales-for-target)
           gsettings-desktop-schemas
           pkg-config
           python
           python-pygobject
           vala))
    ;; TODO: It would be good to make the package respect TZDIR instead
    ;; of using a "hard coded" version of tzdata.
    (inputs (list tzdata))
    (propagated-inputs
     ;; gweather4.pc refers to all of these.
     (list geocode-glib
           glib
           json-glib
           libsoup
           libxml2))))

(define-public gnome-settings-daemon
  (package
    (name "gnome-settings-daemon")
    (version "47.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0nka7zfl4hzzk4066kk5cc8pmk7v3izhi4wihsh5b3w85s35idqy"))
       (patches (search-patches "gnome-settings-daemon-gc.patch"
                                ;; See https://gitlab.gnome.org/GNOME/gnome-settings-daemon/-/issues/792
                                "gnome-settings-daemon-screensaver-error.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list (string-append "-Dudev_dir=" #$output "/lib/udev")
              "-Dsystemd=false"
              ;; Otherwise, the RUNPATH will lack the final path component.
              (string-append "-Dc_link_args=-Wl,-rpath=" #$output
                             "/lib/gnome-settings-daemon-3.0:"
                             ;; Also add NSS because for some reason Meson
                             ;; > 0.60 does not add it automatically (XXX).
                             (search-input-directory %build-inputs "lib/nss")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-baobab-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Hard-code the file name of Baobab instead of looking
              ;; it up in $PATH.  This ensures users get the "Examine"
              ;; button in the low disk space notification of GDM even
              ;; if they don't have GNOME in their main profile.
              (substitute* "plugins/housekeeping/gsd-disk-space.c"
                (("g_find_program_in_path \\(DISK_SPACE_ANALYZER\\)")
                 (format #f "g_strdup (~s)"
                         (search-input-file inputs "bin/baobab")))))))
      ;; Color management test can't reach the colord system service.
      #:tests? #f))
    (native-inputs
     (list docbook-xml-4.2
           docbook-xsl
           gettext-minimal
           `(,glib "bin")               ;for glib-mkenums
           libxslt
           perl
           pkg-config))
    (inputs
     (list alsa-lib
           baobab
           colord
           cups
           gcr
           geoclue
           geocode-glib
           gnome-desktop
           gsettings-desktop-schemas
           lcms
           libcanberra
           libgudev
           libgweather4
           libnotify
           (librsvg-for-system)
           libwacom
           libx11
           libxtst
           modem-manager
           network-manager
           nss
           polkit
           pulseaudio
           upower
           wayland
           xf86-input-wacom))
    (home-page "https://www.gnome.org")
    (synopsis "GNOME settings daemon")
    (description
     "This package contains the daemon responsible for setting the various
parameters of a GNOME session and the applications that run under it.  It
handles settings such keyboard layout, shortcuts, and accessibility, clipboard
settings, themes, mouse settings, and startup of other daemons.")
    (license license:gpl2+)))

(define-public totem-pl-parser
 (package
   (name "totem-pl-parser")
   (version "3.26.6")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnome/sources/totem-pl-parser/"
                                (version-major+minor version) "/"
                                "totem-pl-parser-" version ".tar.xz"))
            (sha256
             (base32
              "075csd5x0frgf93jvhlqiwv5i0qm24zz3iw17jj7v7fgsml0zpy0"))))
   (build-system meson-build-system)
   (arguments
    ;; FIXME: Tests require gvfs.
    `(#:tests? #f))
   (native-inputs
    (list intltool
          `(,glib "bin") gobject-introspection pkg-config))
   (propagated-inputs
    (list glib gmime libarchive libgcrypt libxml2))
   (inputs
    (list libsoup))
   (home-page "https://projects.gnome.org/totem")
   (synopsis "Library to parse and save media playlists for GNOME")
   (description "Totem-pl-parser is a GObjects-based library to parse and save
playlists in a variety of formats.")
   (license license:lgpl2.0+)))

(define-public aisleriot
  (package
    (name "aisleriot")
    (version "3.22.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/aisleriot/"
                                  (version-major+minor version) "/"
                                  "aisleriot-" version ".tar.xz"))
              (sha256
               (base32
                "0yzdh9cw5cjjgvfh75bihl968czlgfmpmn1z0fdk88sgvpjgzwji"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags
       '("--with-platform=gtk-only"
         "--with-card-theme-formats=svg")))
    (native-inputs
     `(("desktop-file-utils" ,desktop-file-utils)
       ("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     (list gtk+ guile-2.2 libcanberra (librsvg-for-system)))
    (home-page "https://wiki.gnome.org/Apps/Aisleriot")
    (synopsis "Solitaire card games")
    (description
     "Aisleriot (also known as Solitaire or sol) is a collection of card games
which are easy to play with the aid of a mouse.")
    (license license:gpl3+)))

(define-public amtk
  (package
    (name "amtk")
    (version "5.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/amtk/"
                                  (version-major+minor version) "/"
                                  "amtk-" version ".tar.xz"))
              (sha256
               (base32
                "0a1j2ynsa2nx1rzd55mdyp35d89zd9rfxd9ld4lsqal7bjw1a0fm"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection
           `(,glib "bin") ; for glib-mkenums
           gtk-doc/stable pkg-config))
    (inputs
     (list glib gtk+))
    (home-page "https://wiki.gnome.org/Projects/Amtk")
    (synopsis "Actions, Menus and Toolbars Kit for GTK+ applications")
    (description
     "Amtk is the acronym for @acronym{Amtk, Actions Menus and Toolbars Kit}.
It is a basic GtkUIManager replacement based on GAction.  It is suitable for
both a traditional UI or a modern UI with a GtkHeaderBar.")
    (license license:lgpl2.1+)))

(define-public devhelp
  (package
    (name "devhelp")
    (version "43.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "016xhpz16b9b13y7wnvkllymb4s2fb6ixvw190204bir0pyyxkk3"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")))))))
    (propagated-inputs
     (list gsettings-desktop-schemas))
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           `(,glib "bin")               ; for glib-mkmenus
           itstool
           pkg-config))
    (inputs
     (list amtk
           webkitgtk-for-gtk3))
    (home-page "https://wiki.gnome.org/Apps/Devhelp")
    (synopsis "API documentation browser for GNOME")
    (description
     "Devhelp is an API documentation browser for GTK+ and GNOME.  It works
natively with GTK-Doc (the API reference system developed for GTK+ and used
throughout GNOME for API documentation).")
    (license license:gpl2+)))

(define-public devhelp-with-libsoup2
  (hidden-package
   (package/inherit devhelp
     (inputs (modify-inputs (package-inputs devhelp)
               (replace "webkitgtk-for-gtk3" webkitgtk-with-libsoup2))))))

(define-public cogl
  (package
    (name "cogl")
    (version "1.22.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/cogl/"
                           (version-major+minor version) "/"
                           "cogl-" version ".tar.xz"))
       (sha256
        (base32 "0nfph4ai60ncdx7hy6hl1i1cmp761jgnyjfhagzi0iqq36qb41d8"))
       (patches
        (search-patches "cogl-fix-double-free.patch"))))
    ;; NOTE: mutter exports a bundled fork of cogl, so when making changes to
    ;; cogl, corresponding changes may be appropriate in mutter as well.
    (build-system gnu-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin")     ; for glib-mkenums
       ("gobject-introspection" ,gobject-introspection)
       ("xorg-server" ,xorg-server-for-tests)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list glib
           gdk-pixbuf
           libx11
           libxext
           libxfixes
           libxdamage
           libxcomposite
           libxrandr))
    (inputs
     (list mesa
           cairo
           pango
           wayland))
    (arguments
     `(#:disallowed-references (,xorg-server-for-tests)
       #:configure-flags (list "--enable-cogl-gst=no" ;broken and unmaintained
                               "--enable-wayland-egl-platform"
                               "--enable-wayland-egl-server"

                               ;; Arrange to pass an absolute file name to
                               ;; dlopen for libGL.so.
                               (string-append "--with-gl-libname="
                                              (assoc-ref %build-inputs "mesa")
                                              "/lib/libGL.so"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build-with-mesa-20
           (lambda _
             ;; Work around a problem with Mesa 20 where some macros used by
             ;; Cogl went missing from eglext.h.  This can likely be removed
             ;; for newer versions of Cogl or Mesa.
             ;; https://gitlab.gnome.org/GNOME/cogl/-/merge_requests/19
             (substitute* '("configure"
                            "cogl/winsys/cogl-winsys-egl-kms.c")
               (("#include <EGL/eglext.h>" all)
                (string-append all "\n#include <EGL/eglmesaext.h>\n")))
             #t))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key tests? inputs #:allow-other-keys)
             (if tests?
                 (begin
                   ;; The test suite requires a running X server.
                   (system (format #f "~a/bin/Xvfb :1 +extension GLX &"
                                   (assoc-ref inputs "xorg-server")))
                   (setenv "DISPLAY" ":1")
                   #t)
                 (format #t "test suite not run~%"))
             #t)))))
    (home-page "https://www.clutter-project.org")
    (synopsis "Object oriented GL/GLES Abstraction/Utility Layer")
    (description
     "Cogl is a small library for using 3D graphics hardware to draw pretty
pictures.  The API departs from the flat state machine style of OpenGL and is
designed to make it easy to write orthogonal components that can render
without stepping on each others toes.")
    (license (list license:expat       ; most of the code
                   license:bsd-3       ; cogl/cogl-point-in-poly.c
                   license:sgifreeb2.0 ; cogl-path/tesselator/
                   license:asl2.0))))  ; examples/android/

(define-public clutter
  (package
    (name "clutter")
    (version "1.26.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1rn4cd1an6a9dfda884aqpcwcgq8dgydpqvb19nmagw4b70zlj4b"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ;9 MiB of gtk-doc HTML pages
    (native-inputs
     (list `(,glib "bin")               ; for glib-genmarshal
           gobject-introspection
           libxslt
           pkg-config))
    (propagated-inputs
     (list at-spi2-core
           cairo
           cogl
           glib
           gtk+
           json-glib
           libxcomposite
           libxdamage
           libxext
           xinput))
    (inputs
     (list eudev
           libxkbcommon))
    (arguments
     `(#:configure-flags (list "--enable-x11-backend=yes"

                               ;; This produces share/doc/{clutter,cally}.
                               (string-append "--with-html-dir="
                                              (assoc-ref %outputs "doc")
                                              "/share/doc"))
       ;; XXX FIXME: Get test suite working.  It would probably fail in the
       ;; same way the cogl tests fail, since clutter is based on cogl.
       #:tests? #f))
    (home-page "https://blogs.gnome.org/clutter/")
    (synopsis "OpenGL-based interactive canvas library")
    (description
     "Clutter is an OpenGL-based interactive canvas library, designed for
creating fast, mainly 2D single window applications such as media box UIs,
presentations, kiosk style applications and so on.")
    (license license:lgpl2.0+)))

(define-public clutter-gtk
  (package
    (name "clutter-gtk")
    (version "1.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "01ibniy4ich0fgpam53q252idm7f4fn5xg5qvizcfww90gn9652j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config gobject-introspection))
    (propagated-inputs
     ;; clutter-gtk.pc refers to all these.
     (list clutter gtk+))
    (home-page "https://www.clutter-project.org")
    (synopsis "OpenGL-based interactive canvas library GTK+ widget")
    (description
     "Clutter is an OpenGL-based interactive canvas library, designed for
creating fast, mainly 2D single window applications such as media box UIs,
presentations, kiosk style applications and so on.")
    (license license:lgpl2.0+)))

(define-public clutter-gst
  (package
    (name "clutter-gst")
    (version "3.0.27")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/clutter-gst/"
                           (version-major+minor version) "/"
                           "clutter-gst-" version ".tar.xz"))
       (sha256
        (base32 "17czmpl92dzi4h3rn5rishk015yi3jwiw29zv8qan94xcmnbssgy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums
           pkg-config gobject-introspection))
    (inputs
     (list clutter gstreamer gst-plugins-base))
    (home-page "https://www.clutter-project.org")
    (synopsis "Integration library for using GStreamer with Clutter")
    (description
     "Clutter-Gst is an integration library for using GStreamer with Clutter.
It provides a GStreamer sink to upload frames to GL and an actor that
implements the ClutterGstPlayer interface using playbin.  Clutter is an
OpenGL-based interactive canvas library.")
    (license license:lgpl2.0+)))

(define-public libchamplain
  (package
    (name "libchamplain")
    (version "0.12.20")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/libchamplain/0.12/libchamplain-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0rihpb0npqpihqcdz4w03rq6xl7jdckfqskvv9diq2hkrnzv8ch2"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection pkg-config vala))
    (propagated-inputs
     (list libsoup-minimal-2
           sqlite
           clutter
           clutter-gtk
           `(,glib "bin") ;glib-mkenums, etc.
           cairo
           gtk+
           glib))
    (home-page "https://projects.gnome.org/libchamplain/")
    (synopsis "C library providing a ClutterActor to display maps")
    (description
     "libchamplain is a C library providing a ClutterActor to display maps.
It also provides a Gtk+ widget to display maps in Gtk+ applications.  Python
and Perl bindings are also available.  It supports numerous free map sources
such as OpenStreetMap, OpenCycleMap, OpenAerialMap, and Maps for free.")
    (license license:lgpl2.1+)))

(define-public gom
  (package
    (name "gom")
    (version "0.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gom/"
                           (version-major+minor version) "/"
                           "gom-" version ".tar.xz"))
       (sha256
        (base32
         "0xq1s933bxlzp3sqnm03id8apqwlc4v7ka7pxlklssywzc4hk786"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:configure-flags
      #~(list (string-append "-Dpygobject-override-dir="
                             (python:site-packages %build-inputs %outputs)
                             "/gi/overrides"))))
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           pkg-config
           python
           python-pygobject))
    (inputs
     (list glib
           gdk-pixbuf
           sqlite))
    (home-page "https://wiki.gnome.org/Projects/Gom")
    (synopsis "Object mapper from GObjects to SQLite")
    (description
     "Gom provides an object mapper from GObjects to SQLite.  It helps you
write applications that need to store structured data as well as make complex
queries upon that data.")
    (license license:lgpl2.1+)))

(define-public libgnome-games-support
  (package
    (name "libgnome-games-support")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/libgnome-games-support/"
                           (version-major+minor version) "/"
                           "libgnome-games-support-" version ".tar.xz"))
       (sha256
        (base32
         "196jaga70r16bzypv4z07mnwr0xcm93gc91kxygcpp9fwdpiz0jk"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a writable HOME.
             (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list intltool pkg-config vala))
    (propagated-inputs
     ;; Required by libgnome-games-support-1.0.pc
     (list gtk libgee))
    (home-page "https://www.gnome.org/")
    (synopsis "Useful functionality shared among GNOME games")
    (description
     "libgnome-games-support is a small library intended for internal use by
GNOME Games, but it may be used by others.")
    (license license:lgpl3+)))

(define-public libgnome-games-support-1
  (package
    (inherit libgnome-games-support)
    (version "1.8.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/libgnome-games-support/"
                           (version-major+minor version) "/"
                           "libgnome-games-support-" version ".tar.xz"))
       (sha256
        (base32
         "0zggsg7h9nlcwwjcqc13pdjza17iiww325r3q0d76f5hlw24chr8"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       libgnome-games-support)
                         (replace "gtk" gtk+)))))

(define-public gnome-klotski
  (package
    (name "gnome-klotski")
    (version "3.38.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1qm01hdd5yp8chig62bj10912vclbdvywwczs84sfg4zci2phqwi"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson_post_install.py"
               (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list desktop-file-utils
           `(,glib "bin")               ; for glib-compile-resources
           intltool
           itstool
           pkg-config
           vala
           libxml2))
    (inputs
     (list gtk+
           libgnome-games-support-1
           librsvg))
    (home-page "https://wiki.gnome.org/Apps/Klotski")
    (synopsis "Sliding block puzzles")
    (description
     "GNOME Klotski is a set of block sliding puzzles.  The objective is to move
the patterned block to the area bordered by green markers.  To do so, you will
need to slide other blocks out of the way.  Complete each puzzle in as few moves
as possible!")
    (license license:gpl2+)))

(define-public grilo
  (package
    (name "grilo")
    (version "0.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/grilo/"
                           (version-major+minor version) "/"
                           "grilo-" version ".tar.xz"))
       (sha256
        (base32 "15mxffs7f7ndzimpvpq4lj48km5p6i9gyqxa4ggq1qpcqpl80ic8"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'unpack 'set-HOME
                          (lambda _
                            ;; Tests require write access to HOME.
                            (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list `(,glib "bin") ; for glib-mkenums and glib-genmarshal
           gettext-minimal
           gsettings-desktop-schemas
           pkg-config
           gobject-introspection
           gtk-doc/stable
           python
           vala))
    (inputs
     (list cyrus-sasl
           glib
           gtk+
           liboauth
           libsoup
           libxml2
           totem-pl-parser))
    (native-search-paths
     (list (search-path-specification
            (variable "GRL_PLUGIN_PATH")
            (files (list (string-append "lib/grilo-"
                                        (version-major+minor version)))))))
    (home-page "https://wiki.gnome.org/Projects/Grilo")
    (synopsis "Framework for discovering and browsing media")
    (description
     "Grilo is a framework focused on making media discovery and browsing easy
for application developers.")
    (license license:lgpl2.1+)))

(define-public grilo-plugins
  (package
    (name "grilo-plugins")
    (version "0.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1jydhk822sigyda3mswn59j9s01dy81f553382i8nsvcb2z4svzy"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           ;; Gstreamer plugins are required for tests.
           gst-plugins-good
           gst-plugins-bad
           `(,glib "bin")
           itstool
           pkg-config))
    (inputs
     (list avahi
           grilo
           gnome-online-accounts
           gom
           gstreamer
           json-glib
           avahi
           libgdata
           libmediaart
           libsoup
           python-pygobject
           totem-pl-parser
           tracker
           tracker-miners))
    (arguments
     (list
      #:glib-or-gtk? #t
      ;;Disable lua-factory as it needs missing dependencies
      #:configure-flags #~'("-Denable-lua-factory=no")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-shell
            (lambda _
              (setenv "SHELL" (which "bash"))))
          ;; Disable the tracker test that requires the UPower daemon.
          (add-before 'configure 'fix-tests
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tests/tracker3/meson.build"
                (("'test_tracker3'.*") "")))))))
    (home-page "https://wiki.gnome.org/Projects/Grilo")
    (synopsis "Plugins for the Grilo media discovery library")
    (description
     "Grilo is a framework focused on making media discovery and browsing easy
for application developers.  This package provides plugins for common media
discovery protocols.")
    (license license:lgpl2.1+)))

(define-public totem
  (package
    (name "totem")
    (version "43.1")
    (source
     (origin
       (method url-fetch)
              (uri (string-append "mirror://gnome/sources/totem/"
                                  (version-major version) "/"
                                  "totem-" version ".tar.xz"))
       (sha256
        (base32
         "0vcyfna0z58s9h8h3pb0pqmlrx8j097ymr7zndf9hi34khg2js2n"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin")             ;for 'glib-mkenums'
           desktop-file-utils
           gettext-minimal
           gobject-introspection
           intltool
           itstool
           libxml2
           perl                       ;for pod2man
           pkg-config
           xorg-server-for-tests))
    (propagated-inputs
     (list dconf))
    (inputs
     (list (librsvg-for-system)
           adwaita-icon-theme
           at-spi2-core
           bash-minimal
           cairo
           dbus-glib
           gdk-pixbuf
           gnome-desktop
           grilo
           grilo-plugins
           gsettings-desktop-schemas
           gst-libav
           gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk+
           libhandy
           libpeas
           libportal
           libsoup
           libxml2
           libxrandr
           libxtst
           libxxf86vm
           python
           python-pygobject
           totem-pl-parser
           vala
           xorgproto))
    (arguments
     (list
      #:glib-or-gtk? #t
      ;; Disable automatic GStreamer plugin installation via PackageKit and
      ;; all that.
      #:configure-flags
      ;; Do not build .a files for the plugins, it's completely useless.
      ;; This saves 2 MiB.
      #~(list "--default-library" "shared")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-before 'install 'disable-cache-generation
            (lambda _
              (setenv "DESTDIR" "/")))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1")))
          (add-after 'install 'wrap-totem
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out             (assoc-ref outputs "out"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (grl-plugin-path (getenv "GRL_PLUGIN_PATH")))
                (wrap-program (string-append out "/bin/totem")
                  `("GI_TYPELIB_PATH"        ":" suffix (,gi-typelib-path))
                  `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                  `("GRL_PLUGIN_PATH"        ":" prefix (,grl-plugin-path)))
                (wrap-program (string-append out "/bin/totem-video-thumbnailer")
                  `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path)))))))))
    (home-page "https://wiki.gnome.org/Apps/Videos")
    (synopsis "Simple media player for GNOME based on GStreamer")
    (description "Totem is a simple yet featureful media player for GNOME
which can read a large number of file formats.")
    ;; GPL2+ with an exception clause for non-GPL compatible GStreamer plugins
    ;; to be used and distributed together with GStreamer and Totem.  See
    ;; file://COPYING in the source distribution for details.
    (license license:gpl2+)))

(define-public rhythmbox
  (package
    (name "rhythmbox")
    (version "3.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/rhythmbox/"
                                  (version-major+minor version) "/"
                                  "rhythmbox-" version ".tar.xz"))
              (sha256
               (base32
                "0zps1k72n7yycw6djgilgdacwdi993xqh1sh9x9lr9n17z0mcv9g"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-after 'install 'wrap-rhythmbox
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/rhythmbox")
                `("GI_TYPELIB_PATH"        ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")))
                `("GST_PLUGIN_SYSTEM_PATH" ":" prefix
                  (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                `("GRL_PLUGIN_PATH"        ":" prefix
                  (,(getenv "GRL_PLUGIN_PATH")))
                `("GUIX_PYTHONPATH"             ":" prefix
                  (,(getenv "GUIX_PYTHONPATH")))))))))
    (propagated-inputs
     (list dconf))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           gobject-introspection
           `(,glib "bin")
           itstool
           pkg-config
           vala))
    (inputs
     ;; TODO:
     ;;  * libgpod
     ;;  * mx
     ;; TODO: clutter* only used by visualizer plugin, which also requires mx
     ;;clutter
     ;;clutter-gtk
     ;;clutter-gst
     (list adwaita-icon-theme
           at-spi2-core
           bash-minimal
           brasero
           json-glib
           gmime
           gnome-desktop
           grilo
           grilo-plugins
           gsettings-desktop-schemas
           gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk+
           libgudev
           libnotify
           libpeas
           libsecret
           libmtp
           libsoup-minimal
           libxml2
           lirc
           pango
           python
           python-pygobject
           tdb
           totem-pl-parser))
    (home-page "https://wiki.gnome.org/Apps/Rhythmbox")
    (synopsis "Music player for GNOME")
    (description "Rhythmbox is a music playing application for GNOME.  It
supports playlists, song ratings, and any codecs installed through gstreamer.")
    (license license:gpl2+)))

(define-public eog
  (package
    (name "eog")
    (version "47.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0rnyrgh0qg1zdnpmn79kaflk0bra4zly93kxgdm14xad4bsxnpnv"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      ;; Otherwise, the RUNPATH will lack the final 'eog' path component.
      #~(list (string-append "-Dc_link_args=-Wl,-rpath="
                             #$output "/lib/eog"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-after 'install 'wrap-eog
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (wrap-program (search-input-file outputs "bin/eog")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (propagated-inputs
     (list dconf
           libhandy))                   ;libhandy is required by eog.pc
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           libxml2
           pkg-config))
    (inputs
     (list adwaita-icon-theme
           bash-minimal
           exempi
           gnome-desktop
           gtk
           lcms
           libexif
           libjpeg-turbo
           libpeas
           libportal
           librsvg
           shared-mime-info))
    (home-page "https://wiki.gnome.org/Apps/EyeOfGnome")
    (synopsis "GNOME image viewer")
    (description "Eye of GNOME is the GNOME image viewer.  It
supports image conversion, rotation, and slideshows.")
    (license license:gpl2+)))

(define-public eog-plugins
  ;; Note: EOG looks for its plugins (via libpeas) in ~/.local as well as
  ;; $DATA/eog/plugins, where DATA is one of the entries in
  ;; $XDG_DATA_DIRS.  Thus, for EOG to find these, you have to have
  ;; 'XDG_DATA_DIRS' appropriately set.
  (package
    (name "eog-plugins")
    (version "42.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/eog-plugins/"
                                  (version-major version) "/"
                                  "eog-plugins-" version ".tar.xz"))
              (sha256
               (base32
                "0prymlrfh66p03va5aj30wazshp7bn80gzcsj9dgsmss2k512wlb"))))
    (build-system meson-build-system)
    (home-page "https://wiki.gnome.org/Apps/EyeOfGnome/Plugins")
    (synopsis "Extensions for the Eye of GNOME image viewer")
    (native-inputs
     (list gettext-minimal
           pkg-config
           python))
    (inputs
     (list eog
           glib
           gtk+
           libchamplain
           libexif
           libgdata
           libpeas))
    (description
     "This package provides plugins for the Eye of GNOME (EOG) image viewer,
notably:

@itemize
@item @dfn{EXIF Display}, which displays camera (EXIF) information;
@item @dfn{Map}, which displays a map of where the picture was taken on the
side panel;
@item @dfn{Slideshow Shuffle}, to shuffle images in slideshow mode.
@end itemize\n")

    ;; XXX: eog-postasa-plugin-resources.c (which we don't build) contains a
    ;; long suspicious byte stream that goes to a
    ;; ".gresource.eog_postasa_plugin" ELF section.
    (license license:gpl2+)))

(define-public libgudev
  (package
    (name "libgudev")
    (version "238")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1laxgdkgmr30aw44sm4rgpsdybwxx5rszcm8c2y3vmy9myqnl9k1"))))
    (build-system meson-build-system)
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)))
    (propagated-inputs
     (list glib ; in Requires of gudev-1.0.pc
           eudev))               ; in Requires.private of gudev-1.0.pc
    (inputs
     `(("udev" ,eudev)))
    (home-page "https://wiki.gnome.org/Projects/libgudev")
    (synopsis "GObject bindings for libudev")
    (description
     "This library provides GObject bindings for libudev.  It was originally
part of udev-extras, then udev, then systemd.  It's now a project on its own.")
    (license license:lgpl2.1+)))

(define-public msgraph
  (package
    (name "msgraph")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/msgraph/"
                                  (version-major+minor version) "/"
                                  "msgraph-" version ".tar.xz"))
              (sha256
               (base32 "1c6xkxx2c1jqhy56pfbsmnk418n1rm2fgqyrgi3hf2kzrc0fhhpd"))))
    (build-system meson-build-system)
    (native-inputs (list gi-docgen gobject-introspection pkg-config uhttpmock))
    (inputs (list gnome-online-accounts json-glib libsoup rest))
    (home-page "https://gnome.pages.gitlab.gnome.org/msgraph/")
    (synopsis "GLib library for accessing MS Graph APIs")
    (description "This package provides a GLib-based library for accessing
MS Graph APIs.")
    (license license:lgpl3+)))

(define-public gvfs
  (package
    (name "gvfs")
    (version "1.56.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gvfs/"
                                  (version-major+minor version) "/"
                                  "gvfs-" version ".tar.xz"))
              (sha256
               (base32
                "045a17jqh5f3aks5c3wc9ipfxg8f37gb2dz26j3qyr3rqv71qww6"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list "-Dsystemduserunitdir=no"
              "-Dtmpfilesdir=no"
              "-Dman=true"
              ;; Otherwise, the RUNPATH will lack the final path component.
              (string-append "-Dc_link_args=-Wl,-rpath="
                             #$output "/lib/gvfs"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "monitor/udisks2/gvfsudisks2mount.c"
                (("\"lsof -t")
                 (string-append "\"" (search-input-file inputs "bin/lsof")
                                " -t"))
                (("\"umount %s")
                 (string-append "\"" (search-input-file inputs "bin/umount")
                                " %s")))
              (substitute* "monitor/udisks2/gvfsudisks2volume.c"
                (("\"mount \\\\\"%s")
                 (string-append "\"" (search-input-file inputs "bin/mount")
                                " \\\"%s"))))))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-genmarshal, etc.
           gettext-minimal
           gtk-doc/stable
           pkg-config
           libxslt))
    (inputs
     (list avahi
           docbook-xml-4.2
           docbook-xsl
           dbus
           elogind
           fuse
           gcr
           glib
           gnome-online-accounts
           gsettings-desktop-schemas
           libarchive
           libbluray
           libcap
           libcdio-paranoia
           libgcrypt
           libgdata
           libgphoto2
           libgudev
           libimobiledevice
           libmtp
           libnfs
           libsecret
           lsof
           msgraph
           samba
           libsoup
           libxml2
           openssh
           polkit
           udisks
           util-linux))
    (home-page "https://wiki.gnome.org/Projects/gvfs")
    (synopsis "Userspace virtual file system for GIO")
    (description
     "GVFS is a userspace virtual file system designed to work with the I/O
abstraction of GIO.  It contains a GIO module that seamlessly adds GVFS
support to all applications using the GIO API.  It also supports exposing the
GVFS mounts to non-GIO applications using FUSE.

GVFS comes with a set of backends, including trash support, SFTP, SMB, HTTP,
DAV, and others.")
    (license license:lgpl2.0+)))

(define-public gusb-minimal
  (package
    (name "gusb-minimal")
    (version "0.3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hughsie/libgusb")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ifhdqhpyxwsg0z9s1anj7cf5pya5qsqyp5ksh9n7mqwa4lrjkl8"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f          ;libusb fails to initialize.  Wonder what that is.
       #:configure-flags
       (cons "-Ddocs=false"
             (if ,(%current-target-system)
                 ;; Introspection data cannot currently be cross-compiled.
                 '("-Dintrospection=false"
                   ;; Requires introspection data.
                   "-Dvapi=false")
                 '()))))
    (native-inputs
     (list gobject-introspection pkg-config python vala))
    (propagated-inputs
     ;; Both of these are required by gusb.pc.
     (list glib libusb))
    (home-page "https://github.com/hughsie/libgusb")
    (synopsis "GLib binding for libusb1")
    (description
     "GUsb is a GObject wrapper for libusb1 that makes it easy to do
asynchronous control, bulk and interrupt transfers with proper cancellation
and integration into a mainloop.  This makes it easy to integrate low level
USB transfers with your high-level application or system daemon.")
    (license license:lgpl2.1+)))

(define-public gusb
  (package/inherit gusb-minimal
    (name "gusb")
    (arguments
     (substitute-keyword-arguments (package-arguments gusb-minimal)
       ((#:configure-flags flags)
        `(cons "-Ddocs=true"
               (delete "-Ddocs=false" ,flags)))))
    (native-inputs
     (cons `("gtk-doc" ,gtk-doc/stable)
           (package-native-inputs gusb-minimal)))))

(define-public simple-scan
  (package
    (name "simple-scan")
    (version "46.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/simple-scan/"
                           (version-major version) "/"
                           "simple-scan-" version ".tar.xz"))
       (sha256
        (base32 "1aghnkvjdyj73kv55nd9gl5b1xjkpcxjn4j3a6z67r9g2j86avn1"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t))
    (native-inputs
     (list gettext-minimal
           itstool
           `(,glib "bin")               ; glib-compile-schemas, etc.
           pkg-config
           python
           vala
           libxml2))
    (inputs
     (list gtk
           zlib
           cairo
           colord
           gdk-pixbuf
           gusb
           libadwaita
           sane-backends))
    (home-page "https://gitlab.gnome.org/GNOME/simple-scan")
    (synopsis "Document and image scanner")
    (description
     "Document Scanner is an easy-to-use application that lets you connect your
scanner and quickly capture images and documents in an appropriate format.  It
supports any scanner for which a suitable SANE driver is available, which is
almost all of them.")
    (license license:gpl3+)))

(define-public epiphany
  (package
    (name "epiphany")
    (version "48.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/epiphany/"
                                  (version-major version) "/"
                                  "epiphany-" version ".tar.xz"))
              (sha256
               (base32
                "102zq0p18nxjf8mnsqqalsf8f0m31mvir41ncj8v00xdzggzdlf9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (guix build union))
      #:imported-modules `((guix build union)
                           ,@%meson-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "tests/meson.build"
                ;; embed_shell fails, because webkitgtk apparently no longer
                ;; supports overriding the ftp schema web_app_utils fails due
                ;; to missing network access.
                (("(embed_shell|web_app_utils)_test,")
                 "find_program('sh'), args: ['-c', 'exit 77'],")
                ;; web_view_test partially fails, because it can’t run bwrap.
                (("web_view_test,")
                 (string-append
                  "web_view_test, args: ["
                  (string-join
                   (map (lambda (test)
                          (string-append "'-s', '/embed/ephy-web-view/" test "'"))
                        '("load_url"
                          "provisional_load_failure_updates_back_forward_list"
                          "error-pages-not-stored-in-history"))
                   ", ")
                  "],")))))
          (replace 'check
            (lambda* (#:key inputs parallel-tests? tests? #:allow-other-keys)
              (when tests?
                ;(setenv "GALLIUM_DRIVER" "llvmpipe")
                (setenv "XDG_RUNTIME_DIR" (string-append (getcwd)
                                                         "/runtime-dir"))
                (mkdir (getenv "XDG_RUNTIME_DIR"))
                (chmod (getenv "XDG_RUNTIME_DIR") #o700)
                (setenv "MESON_TESTTHREADS"
                        (if parallel-tests?
                            (number->string (parallel-job-count))
                            "1"))
                (setenv "XDG_CACHE_HOME" (getcwd))
                ;; There are too many directories in XDG_DATA_DIRS, so
                ;; dbus-daemon fails to start.  We work around this by
                ;; creating a single union directory of all these directories.
                (setenv "XDG_DATA_DIRS" "/tmp/share")
                (union-build "/tmp/share"
                             (search-path-as-list '("share") (map cdr inputs))
                             #:create-all-directories? #t)
                ;; Tests require a running X server.
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1")
                (invoke "dbus-run-session" "--"
                        "meson" "test" "--print-errorlogs" "-t" "0"))))
         (add-after 'install 'gst-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out             (assoc-ref outputs "out"))
                   (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/epiphany")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" suffix (,gst-plugin-path)))))))
      #:configure-flags
      ;; Otherwise, the RUNPATH will lack the final 'epiphany' path component.
      #~(list (string-append "-Dc_link_args=-Wl,-rpath="
                             #$output "/lib/epiphany"))))
    (propagated-inputs (list dconf))
    (native-inputs
     (list desktop-file-utils           ; for update-desktop-database
           gettext-minimal
           `(,glib "bin")               ; for glib-mkenums
           itstool
           pkg-config
           libxml2
           xorg-server-for-tests))
    (inputs
     (list avahi
           bash-minimal                 ; for wrap-program
           gcr
           glib-networking
           gnome-desktop
           gsettings-desktop-schemas
           gst-plugins-base
           gst-plugins-good
           gstreamer
           iso-codes/pinned
           json-glib
           libadwaita
           libarchive
           libnotify
           libportal
           (librsvg-for-system)         ; for loading SVG files
           libsecret
           libsoup
           libxslt
           nettle                       ; for hogweed
           sqlite
           webkitgtk))
    (home-page "https://wiki.gnome.org/Apps/Web")
    (synopsis "GNOME web browser")
    (description
     "Epiphany is a GNOME web browser targeted at non-technical users.  Its
principles are simplicity and standards compliance.")
    (license license:gpl2+)))

(define-public d-spy
  (package
    (name "d-spy")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0lsyw2h91z8wdmxpbqc77jwiafddh1w19s4yb7d521alqswi4n2m"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")))))))
    (native-inputs
     (list desktop-file-utils           ; for update-desktop-database
           `(,glib "bin")
           gettext-minimal
           gobject-introspection
           pkg-config))
    (inputs
     (list gtk
           libadwaita))
    (home-page "https://gitlab.gnome.org/GNOME/d-spy")
    (synopsis "D-Bus debugger")
    (description
     "D-Spy is a tool to explore and test end-points and interfaces of running
programs via D-Bus.  It also ships a library for integration into development
environments.")
    (license license:gpl2+)))

(define-public d-feet
  (deprecated-package "d-feet" d-spy))

(define-public yelp-xsl
  (package
    (name "yelp-xsl")
    (version "42.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1kav039g62q35h508shdbrcjcfkdsc7k6wcr2g780c35n58f32r3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-doc")))
    (native-inputs
     (list mallard-ducktype
           gettext-minimal
           itstool
           libxml2
           libxslt))
    (synopsis "XSL stylesheets for Yelp")
    (description "Yelp-XSL is a collection of programs and data files to help
you build, maintain, and distribute documentation.  It provides XSLT stylesheets
that can be built upon for help viewers and publishing systems.  These
stylesheets output JavaScript and CSS content, and reference images
provided by yelp-xsl.  It also redistributes copies of the jQuery and
jQuery.Syntax JavaScript libraries.")
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (license
     (list
      ;; XSLT
      license:gpl2+
      ;; Images
      license:lgpl2.1+
      ;; JavaScript
      license:expat))))

(define-public yelp
  (package
    (name "yelp")
    (version "42.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0jxckvzmp3lzg62wrdp7f3c8hw6zbkwd0sy65ir9q259hw3zvid2"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'set-man-file-name
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Invoke 'man' directly instead of searching $PATH.
                   (substitute* '("libyelp/yelp-man-parser.c"
                                  "libyelp/yelp-uri.c")
                     (("\"man\"")
                      (string-append "\""
                                     (search-input-file inputs "bin/man")
                                     "\""))
                     (("G_SPAWN_SEARCH_PATH")
                      "0"))))
               (add-after 'install 'help-man-find-its-dependencies
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   ;; XXX: Currently 'man' looks for 'groff' in $PATH in one
                   ;; case.  This should be fixed in 'man-db' proper.
                   (wrap-program (string-append (assoc-ref outputs "out")
                                                "/bin/yelp")
                     `("PATH" ":" prefix
                       (,(dirname (search-input-file inputs "bin/groff"))))))))))
    (native-inputs
     (list `(,glib "bin") ; for glib-genmarshal, etc.
           intltool
           itstool
           pkg-config))
    (propagated-inputs
     (list dconf))
    (inputs
     (list bash-minimal
           gsettings-desktop-schemas
           libhandy
           libxslt
           man-db                                 ;for URIs like "man:ls"
           groff-minimal                          ;ditto
           gtk+
           sqlite
           webkitgtk-for-gtk3
           yelp-xsl))
    (home-page "https://wiki.gnome.org/Apps/Yelp")
    (synopsis "GNOME help browser")
    (description
     "Yelp is the help viewer in Gnome.  It natively views Mallard, DocBook,
man, info, and HTML documents.  It can locate documents according to the
freedesktop.org help system specification.")
    (license license:gpl2+)))

(define-public yelp-tools
  (package
    (name "yelp-tools")
    (version "42.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "01gr255nlb77462040499qx50sik17x2b2jhzncmn56l4106lj9y"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags #~'("-Dhelp=true")))
    (native-inputs
     (list gettext-minimal pkg-config python python-lxml))
    (inputs
     (list yelp-xsl))
    (propagated-inputs
     ;; Needed by `yelp-build', `yelp-check' or 'yelp.m4'.
     (list itstool libxml2 libxslt))
    (synopsis "Yelp documentation tools")
    (description
     "Yelp-tools is a collection of scripts and build utilities to help create,
manage, and publish documentation for Yelp and the web.  Most of the heavy
lifting is done by packages like yelp-xsl and itstool.  This package just
wraps things up in a developer-friendly way.")
    (home-page "https://wiki.gnome.org/Apps/Yelp/Tools")
    (license license:gpl2+)))

(define-public libgee
  (package
    (name "libgee")
    (version "0.20.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgee/"
                                  (version-major+minor version) "/"
                                  "libgee-" version ".tar.xz"))
              (sha256
               (base32
                "0kbd8y70dd5q40i8gxzvhxkn9niqvp0x6knp4ihwqq0dw7sk9y0v"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-introspection-install-dir
            (lambda _
              (substitute* "gee/Makefile.in"
                (("@INTROSPECTION_GIRDIR@")
                 (string-append #$output "/share/gir-1.0/"))
                (("@INTROSPECTION_TYPELIBDIR@")
                 (string-append #$output "/lib/girepository-1.0/"))))))))
    (native-inputs
     (list `(,glib "bin") pkg-config))
    (inputs
     (list glib gobject-introspection))
    (home-page "https://wiki.gnome.org/Projects/Libgee")
    (synopsis "GObject collection library")
    (description
     "Libgee is a utility library providing GObject-based interfaces and
classes for commonly used data structures.")
    (license license:lgpl2.1+)))

(define-public gexiv2
  (package
    (name "gexiv2")
    (version "0.14.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1j722x96kavgm4prgkcs84n94gb54m5jlgzg9za37cz9aqn4vri1"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))))
    (native-inputs
     (list gcr-3
           `(,glib "bin")
           gobject-introspection
           pkg-config
           python
           python-pygobject
           vala))
    (propagated-inputs
     ;; Listed in "Requires" section of gexiv2.pc
     (list exiv2))
    (inputs
     (list glib))
    (home-page "https://wiki.gnome.org/Projects/gexiv2")
    (synopsis "GObject wrapper around the Exiv2 photo metadata library")
    (description
     "Gexiv2 is a GObject wrapper around the Exiv2 photo metadata library.  It
allows for GNOME applications to easily inspect and update EXIF, IPTC, and XMP
metadata in photo and video files of various formats.")
    (license license:gpl2+)))

(define-public shotwell
  (package
    (name "shotwell")
    (version "0.32.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/shotwell/"
                                  (version-major+minor version) "/"
                                  "shotwell-" version ".tar.xz"))
              (sha256
               (base32
                "1dkh5bczf9k86akl70inpc2z98qkhg8g44jb2kc8rqcimkzs95vm"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false")))))))
    (propagated-inputs
     (list dconf))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           itstool
           pkg-config
           python
           vala))
    (inputs
     (list gcr-3
           gexiv2
           gst-plugins-base
           gstreamer
           json-glib
           libportal
           libgdata
           libgee
           libgphoto2
           libgudev
           libraw
           libsecret
           libwebp
           libxml2
           sqlite
           webkitgtk-for-gtk3))
    (home-page "https://wiki.gnome.org/Apps/Shotwell")
    (synopsis "Photo manager for GNOME 3")
    (description
     "Shotwell is a digital photo manager designed for the GNOME desktop
environment.  It allows you to import photos from disk or camera, organize
them by keywords and events, view them in full-window or fullscreen mode, and
share them with others via social networking and more.")
    (license license:lgpl2.1+)))

(define-public file-roller
  (package
    (name "file-roller")
    (version "3.42.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/file-roller/"
                                  (version-major+minor version) "/"
                                  "file-roller-" version ".tar.xz"))
              (sha256
               (base32
                "1iq24g2z7kf1a6kn9asp96lc59r8pxxjvcmm5r7zy47cadnqwhqw"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:phases #~(modify-phases %standard-phases
                        (add-before 'install 'disable-gtk-update-icon-cache
                          (lambda _
                            (setenv "DESTDIR" "/"))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           itstool
           pkg-config
           python))
    ;; TODO: Add libnautilus.
    (inputs
     (list gtk+
           json-glib
           libarchive
           libhandy
           libnotify
           nettle
           libxml2))
    (synopsis "Graphical archive manager for GNOME")
    (description "File Roller is an archive manager for the GNOME desktop
environment that allows users to view, unpack, and create compressed archives
such as gzip tarballs.")
    (home-page "https://fileroller.sourceforge.net")
    (license license:gpl2+)))

(define-public gnome-session
  (package
    (name "gnome-session")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (patches (search-patches "gnome-session-support-elogind.patch"))
              (sha256
               (base32
                "0m4sgfzpkrhpy9bpmjiig3h8sypsmdl25zlil7hw82q9yr565qf6"))))
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list (string-append "-Dsystemduserunitdir="
                             #$output "/share/systemd"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-gnome-session
            (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
              ;; Make sure 'gnome-session' finds the 'gsettings' program.
              (wrap-program (search-input-file outputs "bin/gnome-session")
                `("PATH" ":" prefix
                  (,(dirname (search-input-file (or native-inputs inputs)
                                                "bin/gdbus"))))))))))
    (build-system meson-build-system)
    (native-inputs
     (list docbook-xml-4.1.2
           docbook-xsl
           `(,glib "bin")               ; for glib-compile-schemas, etc.
           intltool
           libxslt
           pkg-config
           xmlto))
    (inputs
     (list bash-minimal
           elogind
           gnome-desktop
           gsettings-desktop-schemas
           gtk+
           json-glib
           libsm
           libxcomposite
           libxtst
           mesa
           upower
           xtrans))
    (synopsis "Session manager for GNOME")
    (description
     "This package contains the GNOME session manager, as well as a
configuration program to choose applications starting on login.")
    (home-page "https://wiki.gnome.org/Projects/SessionManagement")
    (license license:gpl2+)))

(define-public gjs
  (package
    (name "gjs")
    (version "1.82.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0g39nl4x32x71c9gnwlrlv8jnpl2lnhmc7qpl7jy0vap6rbalfgv"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "installed-tests/scripts/testCommandLine.sh"
                    (("Valentín") "")
                    (("☭") ""))))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dinstalled_tests=false")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; The test suite requires a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")

             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")))
         (add-after 'install 'wrap-gi
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/gjs")
               `("GI_TYPELIB_PATH" suffix
                 (,(dirname
                    (search-input-file
                     inputs
                     "lib/girepository-1.0/GObject-2.0.typelib"))
                  ,(dirname
                    (search-input-file
                     inputs
                     "lib/girepository-1.0/GIRepository-2.0.typelib"))))))))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-compile-resources
           pkg-config
           libxml2
           ;; For testing
           dbus
           dconf                        ;required to properly store settings
           util-linux
           xorg-server-for-tests))
    (propagated-inputs
     ;; These are all in the Requires.private field of gjs-1.0.pc.
     (list cairo gobject-introspection mozjs))
    (inputs
     (list gtk+ readline))
    (synopsis "Javascript bindings for GNOME")
    (home-page "https://wiki.gnome.org/Gjs")
    (description
     "Gjs is a javascript binding for GNOME.  It's mainly based on spidermonkey
javascript engine and the GObject introspection framework.")
    (license license:gpl2+)))

(define-public gedit
  (package
    (name "gedit")
    (version "44.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0j8p7lnf05sbw194babasfhvpd3pp29f17kvzn16ffnh34psn3y9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      ;; Otherwise, the RUNPATH will lack the final path component.
      #~(list (string-append "-Dc_link_args=-Wl,-rpath="
                             #$output "/lib/gedit"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "build-aux/meson/post_install.py"
                (("gtk-update-icon-cache") (which "true")))))
          (add-after 'unpack 'do-not-invoke-git
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "meson.build"
                ((".*git.*") ""))))
          (add-after 'install 'wrap-gedit
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/gedit")
                ;; For plugins.
                `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
                `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                ;; For language-specs.
                `("XDG_DATA_DIRS" ":" prefix
                  (,(string-append #$(this-package-input "gtksourceview")
                                   "/share")))))))))
    (propagated-inputs
     (list dconf))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           `(,glib "bin")               ;for glib-mkenums, etc.
           gobject-introspection
           gtk-doc
           intltool
           itstool
           libxml2
           pkg-config
           python
           vala))
    (inputs
     (list adwaita-icon-theme
           amtk
           bash-minimal
           glib
           gsettings-desktop-schemas
           gspell
           gtk+
           gtksourceview-4
           libpeas
           libsoup
           python
           python-pygobject
           tepl))
    (home-page "https://wiki.gnome.org/Apps/Gedit")
    (synopsis "GNOME text editor")
    (description "While aiming at simplicity and ease of use, gedit is a
powerful general purpose text editor.")
    (license license:gpl2+)))

(define-public zenity
  (package
    (name "zenity")
    (version "3.44.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/zenity/"
                                  (version-major+minor version) "/"
                                  "zenity-" version ".tar.xz"))
              (sha256
               (base32
                "1aiyx7z2vnipfmlpk4m20zc5bgjlmh6hx3ix1d61yhb5r6p00m6n"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'disable-gtk-update-icon-cache
                          ;; The gtk-update-icon-cache tool is only run when
                          ;; DESTDIR is unset.
                          (lambda _
                            (setenv "DESTDIR" "/"))))))
    (native-inputs (list gettext-minimal `(,gtk+ "bin") itstool pkg-config))
    (inputs (list gtk+))
    (synopsis "Display graphical dialog boxes from shell scripts")
    (home-page "https://www.gnome.org")
    (description
     "Zenity is a rewrite of gdialog, the GNOME port of dialog which allows you
to display dialog boxes from the commandline and shell scripts.")
    (license license:lgpl2.0+)))

(define-public mutter
  (package
    (name "mutter")
    (version "46.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ab4xbh72kd28zvhawkdfl04dkdcy57xapy977mx6q605zv1y1xm"))))
    ;; NOTE: Since version 3.21.x, mutter now bundles and exports forked
    ;; versions of cogl and clutter.  As a result, many of the inputs,
    ;; propagated-inputs, and configure flags used in cogl and clutter are
    ;; needed here as well.
    (build-system meson-build-system)
    (arguments
     (list
      #:modules '((guix build meson-build-system)
                  (guix build utils)
                  (ice-9 match))
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list
         ;; Otherwise, the RUNPATH will lack the final path component.
         (string-append "-Dc_link_args=-Wl,-rpath="
                        #$output "/lib,-rpath="
                        #$output "/lib/mutter-14")
         ;; Disable systemd support.
         "-Dsystemd=false"
         ;; The native-unit test suite appears flaky (see:
         ;; https://gitlab.gnome.org/GNOME/mutter/-/issues/3909).
         "-Dnative_tests=false"
         ;; Don't install tests.
         "-Dinstalled_tests=false"
         ;; The following flags are needed for the bundled clutter
         (string-append "-Dxwayland_path="
                        (search-input-file %build-inputs "bin/Xwayland"))
         ;; the remaining flags are needed for the bundled cogl
         (string-append "-Dopengl_libname="
                        (search-input-file %build-inputs "lib/libGL.so"))
         (string-append "-Dgles2_libname="
                        (search-input-file %build-inputs "lib/libGLESv2.so"))
         "-Degl_device=true"            ;false by default
         "-Dwayland_eglstream=true"     ;false by default
         (string-append "-Dudev_dir=" #$output "/lib/udev"))
      #:test-options #~(list "--verbose")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-SOURCE_DIR
            (lambda _
              ;; Just to make our life easier later.
              (setenv "SOURCE_DIR" (getcwd))))
          (add-after 'unpack 'use-RUNPATH-instead-of-RPATH
            (lambda _
              ;; The build system disables RUNPATH in favor of RPATH to work
              ;; around a peculiarity of their CI system.  Ignore that.
              (substitute* "meson.build"
                (("disable-new-dtags")
                 "enable-new-dtags"))))
          (add-after 'unpack 'patch-dlopen-calls
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/wayland/meta-wayland-egl-stream.c"
                (("libnvidia-egl-wayland.so.1")
                 (search-input-file inputs "lib/libnvidia-egl-wayland.so.1")))))
          (add-before 'configure 'set-udev-dir
            (lambda _
              (setenv "PKG_CONFIG_UDEV_UDEVDIR"
                      (string-append #$output "/lib/udev"))))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (with-directory-excursion "src/tests"
                (substitute* "meson.build"
                  ;; The 'sync' variant of the X11 test fails for unknown reason
                  ;; (see: https://gitlab.gnome.org/GNOME/mutter/-/issues/3910).
                  (("foreach mode: \\['', 'sync'\\]")
                   "foreach mode: []")
                  ;; Many (all?) stacking tests are susceptible to fail
                  ;; non-deterministically under high load (see:
                  ;; https://gitlab.gnome.org/GNOME/mutter/-/issues/4035).
                  (("foreach stacking_test: stacking_tests")
                   "foreach stacking_test: []"))
                (substitute* "clutter/conform/meson.build"
                  ;; TODO: Re-instate the gesture test in a 47+ release.
                  ;; The conform/gesture test fails non-deterministically on
                  ;; some machines (see:
                  ;; https://gitlab.gnome.org/GNOME/mutter/-/issues/3521#note_2385427).
                  ((".*'gesture',.*") "")

                  ;; The 'event-delivery' test fails non-deterministically
                  ;; (see:
                  ;; https://gitlab.gnome.org/GNOME/mutter/-/issues/4035#note_2402672).
                  ((".*'event-delivery',.*") "")))))
          (replace 'check
            (lambda* (#:key tests? test-options parallel-tests?
                      #:allow-other-keys)
              (when tests?
                ;; Setup (refer to the 'test-mutter' and its dependents targets
                ;; in the '.gitlab-ci.yml' file.
                (setenv "HOME" "/tmp")
                (setenv "XDG_RUNTIME_DIR" (string-append (getcwd)
                                                         "/runtime-dir"))
                (mkdir (getenv "XDG_RUNTIME_DIR"))
                (chmod (getenv "XDG_RUNTIME_DIR") #o700)

                (setenv "GSETTINGS_SCHEMA_DIR" "data")
                (setenv "MUTTER_DEBUG_DUMMY_MODE_SPECS" "800x600@10.0")
                (setenv "PIPEWIRE_DEBUG" "2")
                (setenv "PIPEWIRE_LOG" "meson-logs/pipewire.log")
                (setenv "XVFB_SERVER_ARGS" "+iglx -noreset")
                (setenv "G_SLICE" "always-malloc")
                (setenv "MALLOC_CHECK" "3")
                (setenv "NO_AT_BRIDGE" "1")

                (invoke "glib-compile-schemas" (getenv "GSETTINGS_SCHEMA_DIR"))
                (invoke "pipewire" "--version") ;check for pipewire

                (setenv "MESON_TESTTHREADS"
                        (if parallel-tests?
                            (number->string (parallel-job-count))
                            "1"))

                (apply invoke "xvfb-run" "-a" "-s" (getenv "XVFB_SERVER_ARGS")
                       (string-append (getenv "SOURCE_DIR")
                                      "/src/tests/meta-dbus-runner.py")
                       "--launch=wireplumber"
                       "meson" "test" "-t" "0"
                       "--setup=plain"
                       "--no-suite=mutter/kvm"
                       "--no-rebuild"
                       "--print-errorlogs"
                       test-options)))))))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           `(,glib "bin")               ;for glib-compile-schemas, etc.
           gettext-minimal
           gobject-introspection
           pkg-config
           xvfb-run
           wayland-protocols
           ;; For tests.
           ;; Warnings are configured to be fatal during the tests; add an icon
           ;; theme to please libxcursor.
           adwaita-icon-theme
           libei
           libxcursor                   ;for XCURSOR_PATH
           pipewire
           python
           python-dbus
           python-dbusmock
           wireplumber-minimal))
    (propagated-inputs
     (list gsettings-desktop-schemas    ;required by libmutter-14.pc
           ;; mutter-clutter-14.pc and mutter-cogl-14.pc refer to these:
           at-spi2-core
           cairo
           eudev
           gdk-pixbuf
           glib
           json-glib
           libinput
           libx11
           libxcomposite
           libxcvt
           libxdamage
           libxext
           libxfixes
           libxkbcommon
           libxml2
           libxrandr
           mesa
           pango
           xinput))
    (inputs
     (list colord
           egl-wayland                  ;for wayland-eglstream-protocols
           elogind
           gnome-desktop
           gnome-settings-daemon
           graphene
           libcanberra
           libdisplay-info
           libgudev
           libice
           libsm
           libwacom
           libxkbfile
           libxrandr
           libxtst
           linux-libre-headers-6.1      ; for dma_buf_export_sync_file
           pipewire
           startup-notification
           sysprof
           upower
           xkeyboard-config
           xorg-server-xwayland))
    (synopsis "Window and compositing manager")
    (home-page "https://www.gnome.org")
    (description
     "Mutter is a window and compositing manager that displays and manages your
desktop via OpenGL.  Mutter combines a sophisticated display engine using the
Clutter toolkit with solid window-management logic inherited from the Metacity
window manager.")
    (license license:gpl2+)))

(define-public gnome-online-accounts
  (package
    (name "gnome-online-accounts")
    (version "3.50.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1y4dn60vm1dy9hajqbm7hvzs9j2468rxnyg5ma3xx53mssq885kd"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-compile-schemas, etc.
           gobject-introspection
           libxslt
           pkg-config
           vala))
    (propagated-inputs
     (list glib                         ; required by goa-1.0.pc
           gtk libadwaita))             ; required by goa-backend-1.0.pc
    (inputs
     (list docbook-xsl
           dbus
           gcr
           json-glib
           libsecret
           mit-krb5
           rest-next
           webkitgtk))
    (synopsis "Single sign-on framework for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeOnlineAccounts")
    (description
     "GNOME Online Accounts provides interfaces so that applications and
libraries in GNOME can access the user's online accounts.  It has providers
for Google, ownCloud, Facebook, Flickr, Windows Live, Pocket, Foursquare,
Microsoft Exchange, Last.fm, IMAP/SMTP, Jabber, SIP and Kerberos.")
    (license license:lgpl2.0+)))

(define-public gnome-online-accounts-3.44
  (package
    (inherit gnome-online-accounts)
    (name "gnome-online-accounts")
    (version "3.44.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0hkkxa3zqyl0i4kw1p3ak4alwxw4wydh9al6fzwbcdgl0r0ms79q"))))
    (build-system glib-or-gtk-build-system)
    (arguments (substitute-keyword-arguments
                   (strip-keyword-arguments
                    '(#:glib-or-gtk?)
                    (package-arguments gnome-online-accounts))
                 ((#:phases phases)
                  #~(modify-phases #$phases
                      (delete 'disable-gtk-update-icon-cache)))))
    (inputs (modify-inputs (package-inputs gnome-online-accounts)
              (replace "rest" rest)
              (replace "webkitgtk" webkitgtk-with-libsoup2)))))

(define-public evolution-data-server
  (package
    (name "evolution-data-server")
    (version "3.56.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0dy141vy50s15jz6h8pfkf66zmsa76apsqscg6az5yd3gl1w0v34"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(let* ((lib (string-append #$output "/lib"))
               (runpaths (map (lambda (s)
                                (string-append lib "/evolution-data-server/" s))
                              '("addressbook-backends" "calendar-backends"
                                "camel-providers" "credential-modules"
                                "registry-modules"))))
          (list "-DENABLE_GOOGLE=OFF"   ;disable Google Contacts support
                "-DENABLE_VALA_BINDINGS=ON"
                (string-append "-DCMAKE_INSTALL_RPATH=" lib ";"
                               (string-append lib "/evolution-data-server;")
                               (string-join runpaths ";"))
                "-DENABLE_INTROSPECTION=ON"      ;required for Vala bindings
                "-DENABLE_OAUTH2_WEBKITGTK4=OFF" ;requires webkitgtk-next
                "-DWITH_PHONENUMBER=ON"))
      #:phases
      #~(modify-phases %standard-phases
          #$@(if (target-aarch64?)
                 #~((add-after 'unpack 'disable-failing-aarch64-tests
                      (lambda _
                        ;; 26/90 Test #26: test-book-client-custom-summary
                        ;; ...........SIGTRAP***Exception: 35.99 sec
                        (substitute* "tests/libebook/client/CMakeLists.txt"
                          (("test-book-client-custom-summary") "")))))
                 '())
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              ;; tests/book-migration/test-migration.c:160:test_fetch_contacts:
              ;; assertion failed (g_slist_length (contacts) == 20): (0 == 20)
              (delete-file-recursively "tests/book-migration")
              (substitute* "tests/CMakeLists.txt"
                (("add_subdirectory\\(book-migration\\)") ""))))
          (add-after 'unpack 'patch-locale-in-test
            (lambda _
              (substitute* "tests/libebook/client/test-book-client-custom-summary.c"
                (("en_US\\.UTF-8") "C.UTF-8"))))
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* '("tests/test-server-utils/e-test-server-utils.c"
                             "tests/libedata-book/data-test-utils.c"
                             "tests/libedata-book/test-book-cache-utils.c"
                             "tests/libedata-cal/test-cal-cache-utils.c")
                (("/bin/rm") (which "rm")))))
          (add-before 'configure 'dont-override-rpath
            (lambda _
              (substitute* "CMakeLists.txt"
                ;; CMakeLists.txt hard-codes runpath to just the libdir.
                ;; Remove it so the configure flag is respected.
                (("SET\\(CMAKE_INSTALL_RPATH .*") "")))))))
    (native-inputs
     (list `(,glib "bin")               ; for glib-mkenums, etc.
           gobject-introspection
           gperf
           gsettings-desktop-schemas
           intltool
           pkg-config
           protobuf
           python-wrapper
           vala))
    (propagated-inputs
     ;; These are all in the Requires field of .pc files.
     (list glib
           gtk
           gtk+
           json-glib
           libical
           libsecret
           libsoup
           nss
           sqlite))
    (inputs
     (list bdb
           boost
           gcr-3
           gnome-online-accounts
           json-glib
           libcanberra
           libgweather4
           libphonenumber
           mit-krb5
           openldap
           webkitgtk-for-gtk3))
    (synopsis "Store address books and calendars")
    (home-page "https://wiki.gnome.org/Apps/Evolution")
    (description
     "This package provides a unified backend for programs that work with
contacts, tasks, and calendar information.  It was originally developed for
Evolution (hence the name), but is now used by other packages as well.")
    (license license:lgpl2.0)))

;;; This version can be used for projects with dependencies stuck on libsoup2.
(define-public evolution-data-server-3.44
  (package
    (inherit evolution-data-server)
    (name "evolution-data-server")
    (version "3.44.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "1sxjrjr31wqbp9g4pf6dwj8rc4mi7c5fbfd489ha92ym7246bin0"))))
    (inputs
     (modify-inputs (package-inputs evolution-data-server)
       (replace "gnome-online-accounts" gnome-online-accounts-3.44)
       (replace "libgweather4" libgweather)
       (replace "webkitgtk-for-gtk3" webkitgtk-with-libsoup2)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs evolution-data-server)
       (delete "gtk")
       (replace "libsoup" libsoup-minimal-2)))))

(define-public caribou
  (package
    (name "caribou")
    (version "0.4.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0mfychh1q3dx0b96pjz9a9y112bm9yqyim40yykzxx1hppsdjhww"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before
          'build 'pre-build
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              ;; Use absolute shared library path in Caribou-1.0.typelib.
              (substitute* "libcaribou/Makefile"
                (("--shared-library=libcaribou.so")
                 (string-append "--shared-library="
                                out "/lib/libcaribou.so")))
              #t)))
         (add-after 'install 'wrap-programs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (python-path (getenv "GUIX_PYTHONPATH"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
              (for-each
               (lambda (prog)
                 (wrap-program prog
                   `("GUIX_PYTHONPATH"      ":" prefix (,python-path))
                   `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
               (list (string-append out "/bin/caribou-preferences")
                     (string-append out "/libexec/antler-keyboard"))))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-compile-schemas, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("vala" ,vala)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; caribou-1.0.pc refers to all these.
     (list libgee libxklavier libxtst gtk+))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("clutter" ,clutter)
       ("dconf" ,dconf)
       ("gtk+-2" ,gtk+-2)
       ("python-pygobject" ,python-pygobject)))
    (synopsis "Text entry and UI navigation application")
    (home-page "https://wiki.gnome.org/Projects/Caribou")
    (description
     "Caribou is an input assistive technology intended for switch and pointer
users.")
    (license license:lgpl2.1)))

(define-public network-manager
  (package
    (name "network-manager")
    ;; Note: NetworkManager still follows the odd/even major version number
    ;; for development/stable releases scheme; be sure to use a stable one.
    (version "1.52.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://gitlab.freedesktop.org/"
                                        "NetworkManager/NetworkManager"))
                    (commit version)))
              (file-name (git-file-name name version))
              (patches (search-patches "network-manager-plugin-path.patch"))
              (sha256
               (base32
                "0fx3yvqrwc9fqphhwvchxls0lgizlz7bxww3riijlvx3pkypqbyr"))))
    (build-system meson-build-system)
    (outputs '("out"
               "doc"))                  ; 8 MiB of gtk-doc HTML
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; Otherwise, the RUNPATH will lack the final 'NetworkManager' path
         ;; component.
         (string-append "-Dc_link_args=-Wl,-rpath="
                        #$output "/lib:"
                        #$output "/lib/NetworkManager/" #$version)
         "-Dsystemd_journal=false"
         "-Dsession_tracking=elogind"
         "-Dsuspend_resume=elogind"
         "-Dsystemdsystemunitdir=no"
         "-Dsession_tracking_consolekit=false"
         "-Dcrypto=gnutls"
         "-Diwd=true"
         "-Dnm_cloud_setup=false"
         "-Dlibaudit=yes"
         "-Dqt=false"
         "-Ddocs=true"
         "--sysconfdir=/etc"
         "--localstatedir=/var"
         (string-append "-Dudev_dir="
                        #$output "/lib/udev")
         (string-append "-Ddbus_conf_dir="
                        #$output "/etc/dbus-1/system.d")
         (string-append "-Dmodprobe=" (search-input-file %build-inputs
                                                         "bin/modprobe")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-dlopen-call-to-libjansson.so
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/libnm-glib-aux/nm-json-aux.c"
                (("(handle = dlopen\\()soname" _ head)
                 (string-append
                  head "\"" (search-input-file inputs
                                               "lib/libjansson.so") "\"")))))
          (add-before 'configure 'pre-configure
            (lambda _
              ;; These tests try to test aspects of network-manager's
              ;; functionality within restricted containers, but they don't
              ;; cope with being already in the Guix build jail as that jail
              ;; lacks some features that they would like to proxy over (like
              ;; a /sys mount).
              (substitute* "src/core/tests/meson.build"
                ((".*test-l3cfg.*") ""))
              (substitute* "src/core/devices/tests/meson.build"
                ((".*test-acd.*") "")
                ((".*test-lldp.*") ""))
              (substitute* "src/core/platform/tests/meson.build"
                ((".*test-address-linux.*") "")
                ((".*test-cleanup-linux.*") "")
                ((".*test-link-linux.*") "")
                ((".*test-lldp.*") "")
                ((".*test-route-linux.*") "")
                ((".*test-tc-linux.*") ""))
              (substitute* "src/libnm-client-impl/meson.build"
                ;; Note: printenv results in bogus newline, that isn't stripped
                (("run_command\\('printenv', '([^']*)',[^\n]*" all var)
                 (string-append "'" (or (getenv var) "") "'")))))
          (add-before 'check 'pre-check
            (lambda _
              ;; For the missing /etc/machine-id.
              (setenv "DBUS_FATAL_WARNINGS" "0")))
          (add-before 'install 'no-polkit-magic
            ;; Meson ‘magically’ invokes pkexec, which fails (not setuid).
            (lambda _
              (setenv "PKEXEC_UID" "something")))
          (add-after 'install 'move-doc
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (for-each (lambda (directory)
                          (copy-recursively (string-append #$output directory)
                                            (string-append #$output:doc
                                                           directory))
                          (delete-file-recursively
                           (string-append #$output directory)))
                        '("/share/doc" "/share/gtk-doc")))))))
    (propagated-inputs
     (list glib))
    (native-inputs
     (list docbook-xml
           docbook-xsl
           gettext-minimal
           `(,glib "bin")               ;for gdbus-codegen
           gobject-introspection
           gtk-doc/stable
           libxml2
           libxslt
           perl
           pkg-config
           python-dbus
           python-pygobject
           python-wrapper
           vala))
    (inputs
     (list audit
           curl
           cyrus-sasl
           dbus-glib
           dhcpcd
           dnsmasq
           elogind
           eudev
           gnutls
           iptables
           isc-dhcp
           iwd                          ;wpa_supplicant alternative
           jansson
           kmod
           libgcrypt
           libgudev
           libndp
           libnl
           libpsl
           libselinux
           mobile-broadband-provider-info
           modem-manager
           nftables
           newt                         ;for the 'nmtui' console interface
           openresolv                   ;alternative resolv.conf manager
           polkit
           ppp
           readline
           util-linux))
    (synopsis "Network connection manager")
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (description
     "NetworkManager is a system network service that manages your network
devices and connections, attempting to keep active network connectivity when
available.  It manages ethernet, WiFi, mobile broadband (WWAN), and PPPoE
devices, and provides VPN integration with a variety of different VPN
services.")
    ;; “This NetworkManager project consists of the daemon, client tools, and
    ;; libnm. libnm is licensed LGPL-2.1+, while the rest is licensed under
    ;; GPL-2.0+.”
    (license (list license:gpl2+
                   license:lgpl2.1+))
    (properties '((upstream-name . "NetworkManager")))))

(define-public network-manager-openvpn
  (package
    (name "network-manager-openvpn")
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openvpn/"
                    (version-major+minor version)
                    "/NetworkManager-openvpn-" version ".tar.xz"))
              (sha256
               (base32
                "11p8ny4swrim80a4axids9ajd4nzv2zc5n69f2nafamxmv0d8gwh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--enable-absolute-paths"
                                "--localstatedir=/var"
                                "--with-gtk4=yes")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'patch-path
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (let* ((ovpn (search-input-file inputs "/sbin/openvpn"))
                     (modprobe (search-input-file inputs "/bin/modprobe"))
                     (pretty-ovpn (string-append "\"" ovpn "\"")))
                (for-each
                 (lambda (file)
                   (substitute* file
                     (("\"/usr/local/sbin/openvpn\"") pretty-ovpn)
                     (("\"/usr/sbin/openvpn\"") pretty-ovpn)
                     (("\"/sbin/openvpn\"") pretty-ovpn)
                     (("/sbin/modprobe") modprobe)))
                 '("src/nm-openvpn-service.c"
                   "properties/nm-openvpn-editor.c"))))))))
    (native-inputs
     (list intltool
           `(,glib "bin")
           pkg-config))
    (inputs
     (list gtk+
           gtk
           kmod
           libnma
           libsecret
           network-manager
           openvpn))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenVPN plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via OpenVPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openvpn")))))

(define-public network-manager-vpnc
  (package
    (name "network-manager-vpnc")
    (version "1.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-vpnc/"
                    (version-major+minor version)
                    "/NetworkManager-vpnc-" version ".tar.xz"))
              (sha256
               (base32
                "1k7vkalslzmz8zvfy76k7z10b9krm7da917gwzyw7zf8afm32pnn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths"
                           "--localstatedir=/var"
                           "--with-gtk4=yes")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((vpnc (search-input-file inputs "/sbin/vpnc"))
                    (modprobe (search-input-file inputs "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" vpnc "\"")))
               (substitute* "src/nm-vpnc-service.c"
                    (("\"/usr/local/sbin/vpnc\"") pretty-ovpn)
                    (("\"/usr/sbin/vpnc\"") pretty-ovpn)
                    (("\"/sbin/vpnc\"") pretty-ovpn)
                    (("/sbin/modprobe") modprobe))))))))
    (native-inputs
     (list `(,glib "bin")
           intltool
           pkg-config))
    (inputs
     (list gtk+
           gtk
           kmod
           vpnc
           network-manager
           libnma
           libsecret))                 ;TODO: remove after it's the default
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "VPNC plug-in for NetworkManager")
    (description
     "Support for configuring virtual private networks based on VPNC.
Compatible with Cisco VPN concentrators configured to use IPsec.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-vpnc")))))

(define-public network-manager-openconnect
  (package
    (name "network-manager-openconnect")
    (version "1.2.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-openconnect/"
                    (version-major+minor version)
                    "/NetworkManager-openconnect-" version ".tar.xz"))
              (sha256
               (base32
                "0r342dinhh1808cb095xb5zfxrm5kaw6sxwclss9gnya9vv6njw4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-absolute-paths"
                           "--localstatedir=/var"
                           "--with-gtk4=yes")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'patch-path
           (lambda* (#:key inputs outputs #:allow-other-keys #:rest args)
             (let* ((openconnect (search-input-file inputs
                                                    "/sbin/openconnect"))
                    (modprobe (search-input-file inputs "/bin/modprobe"))
                    (pretty-ovpn (string-append "\"" openconnect "\"")))
               (substitute* "src/nm-openconnect-service.c"
                 (("\"/usr(/local)?/s?bin/openconnect\"") pretty-ovpn)
                 (("/sbin/modprobe") modprobe))))))))
    (native-inputs
     (list `(,glib "bin")
           intltool
           libnma
           pkg-config))
    (inputs
     (list gcr-3
           gtk
           gtk+
           kmod
           libsecret
           libxml2
           lz4
           network-manager
           openconnect
           webkitgtk-with-libsoup2))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "OpenConnect plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to @acronym{VPNs, virtual private networks} via OpenConnect, an open client for
Cisco's AnyConnect SSL VPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-openconnect")

                  ;; The 'etc/dbus-1/system.d/nm-openconnect-service.conf'
                  ;; file refers to account "nm-openconnect".  Specify it here
                  ;; so that 'network-manager-service-type' creates it.
                  (user-accounts . ("nm-openconnect"))))))

(define-public network-manager-fortisslvpn
  (package
    (name "network-manager-fortisslvpn")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/NetworkManager-fortisslvpn/"
                    (version-major+minor version)
                    "/NetworkManager-fortisslvpn-" version ".tar.xz"))
              (sha256
               (base32
                "1ynsqmv8xz1cffnai4hfh0ab0dmlazpv72krhlsv45mm95iy4mdh"))
              (modules '((guix build utils)))
              (snippet '(substitute* "Makefile.in"
                          ;; do not try to make state directory
                          (("\\$\\(DESTDIR\\)\\$\\(fortisslvpn_statedir\\)")
                           "")
                          ;; use state directory of the NetworkManager service
                          (("\\$\\(fortisslvpn_statedir\\)")
                           "/var/lib/NetworkManager")))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-absolute-paths" "--localstatedir=/var"
                           "--with-gtk4=yes")
       #:phases (modify-phases %standard-phases
                  (add-after 'configure 'patch-path
                    (lambda* (#:key inputs #:allow-other-keys)
                      (let* ((ovpn (search-input-file inputs
                                                      "/bin/openfortivpn"))
                             (pretty-ovpn (string-append "\"" ovpn "\"")))
                        (for-each (lambda (file)
                                    (substitute* file
                                      (("\"/usr/local/bin/openfortivpn\"")
                                       pretty-ovpn)
                                      (("\"/usr/bin/openfortivpn\"")
                                       pretty-ovpn)))
                                  '("src/nm-fortisslvpn-service.c"
                                    "properties/nm-fortisslvpn-editor.c"))))))))
    (native-inputs (list intltool
                         `(,glib "bin") pkg-config))
    (inputs (list gtk+
                  gtk
                  kmod
                  libnma
                  libsecret
                  network-manager
                  openfortivpn

                  ;; ppp < 2.5.0 is currently required:
                  ;; https://gitlab.gnome.org/GNOME/NetworkManager-fortisslvpn/-/commit/084ef529c5fb816927ca54866f66b340265aa9f6
                  ppp-2.4.9))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager/VPN")
    (synopsis "Fortinet SSLVPN plug-in for NetworkManager")
    (description
     "This extension of NetworkManager allows it to take care of connections
to virtual private networks (VPNs) via Fortinet SSLVPN.")
    (license license:gpl2+)
    (properties `((upstream-name . "NetworkManager-fortisslvpn")))))

(define-public mobile-broadband-provider-info
  (package
    (name "mobile-broadband-provider-info")
    (version "20240407")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/"
                    "mobile-broadband-provider-info/" version "/"
                    "mobile-broadband-provider-info-" version ".tar.xz"))
              (sha256
               (base32
                "0jl13k02m63izk35kylv3v8q1c7xa19c4bpzqglzigzl2prfzgw9"))))
    (build-system meson-build-system)
    (native-inputs (list libxml2 libxslt))
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (synopsis "Database of broadband connection configuration")
    (description "Database of broadband connection configuration.")
    (license license:public-domain)))

(define-public network-manager-applet
  (package
    (name "network-manager-applet")
    (version "1.36.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/network-manager-applet/"
                                  (version-major+minor version) "/"
                                  "network-manager-applet-" version ".tar.xz"))
              (sha256
               (base32
                "0lz2lxj5xy65l7qcn3df83spkxxqk2sjmys7bi4f3bx3gr408ix8"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       '("-Dappindicator=yes")))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin") ; for glib-compile-resources, etc.
           gobject-introspection
           gtk-doc/stable
           pkg-config))
    (propagated-inputs
     ;; libnm-gtk.pc refers to all these.
     (list dbus-glib gtk+ network-manager
           ;; nm-applet need by org.gnome.nm-applet.gschema.xml
           libnma))
    (inputs
     (list gcr-3
           libappindicator
           libgudev
           libsecret
           libselinux
           jansson ; for team support
           modem-manager))
    (synopsis "Applet for managing network connections")
    (home-page "https://wiki.gnome.org/Projects/NetworkManager")
    (description
     "This package contains a systray applet for NetworkManager.  It displays
the available networks and allows users to easily switch between them.")
    (license license:gpl2+)))

(define-public libxml++
  (package
    (name "libxml++")
    (version "5.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libxmlplusplus/libxmlplusplus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07h11vl0rv8b0w31as5xiirpx17lprkx7fimphy3f5mkwhz8njba"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list libxml2))                    ;required by .pc file
    (native-inputs
     (list autoconf
           automake
           doxygen
           docbook-xml
           docbook-xsl
           graphviz                     ;for dot
           libtool
           libxslt
           mm-common
           perl
           pkg-config))
    (home-page "https://github.com/libxmlplusplus/libxmlplusplus/")
    (synopsis "C++ bindings to the libxml2 XML parser library")
    (description
     "This package provides a C++ interface to the libxml2 XML parser
library.")
    (license license:lgpl2.1+)))

;; This is needed by tascam-gtk
(define-public libxml++-3
  (package
    (inherit libxml++)
    (name "libxml++")
    (version "3.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libxmlplusplus/libxmlplusplus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07f6l9ka63dnc85npxq5g7bn1ja7lad0w2wixqdlyabdvc4l2hp5"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs libxml++)
                         (append glibmm-2.66)))))

;; This is the last release providing the 2.6 API, hence the name.
(define-public libxml++-2
  (package
    (inherit libxml++)
    (name "libxml++")
    (version "2.42.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libxmlplusplus/libxmlplusplus")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05slsbhc25z7kwlc28ydl3dfyp7rgbmz1fxj9z6gcvpg3hkghj2m"))))
    (propagated-inputs (modify-inputs (package-propagated-inputs libxml++)
                         (append glibmm-2.66)))))

(define-public gdm
  (package
    (name "gdm")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "12r6ppsdq9f0rlhfwldwhilshb1blp6m0944rm872lqn5914bqsf"))
              (patches
               (search-patches
                "gdm-default-session.patch"
                "gdm-remove-hardcoded-xwayland-path.patch"
                "gdm-wayland-session-wrapper-from-env.patch"
                "gdm-pass-gdk-pixbuf-loader-env.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list
         "-Dplymouth=disabled"
         "-Dsystemd-journal=false"

         ;; Using --with-initial-vt=7 allows GDM to run alongside TTY 1,
         ;; instead of having to replace it (i.e., stopping the mingetty
         ;; service for TTY 1 before starting GDM).
         "-Dinitial-vt=7"

         ;; Use elogind instead of systemd.
         "-Dlogind-provider=elogind"
         "-Dsystemdsystemunitdir=no"
         "-Dsystemduserunitdir=no"

         ;; Use '/etc/environment' for locale settings instead of the
         ;; systemd-specific '/etc/locale.conf'.
         "-Dlang-file=/etc/environment"

         (string-append "-Dudev-dir=" #$output "/lib/udev")

         "--localstatedir=/var"
         (string-append "-Ddefault-path="
                        (string-join '("/run/privileged/bin"
                                       "/run/current-system/profile/bin"
                                       "/run/current-system/profile/sbin")
                                     ":"))
         ;; Put GDM in bindir so that glib-or-gtk-build-system wraps the
         ;; XDG_DATA_DIRS so that it finds its schemas.
         "--sbindir" (string-append #$output "/bin"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "daemon/gdm-session.c"
                (("dbus-run-session")
                 (search-input-file inputs "bin/dbus-run-session")))))
          (add-before 'configure 'pre-configure
            (lambda* (#:key inputs #:allow-other-keys)
              ;; We don't want to write to other packages.
              (substitute* "meson.build"
                (("if dconf_dep\\.found\\(\\)" all)
                 (string-append all " and false")))
              ;; We don't have <systemd/sd-daemon.h>.
              (substitute* '("common/gdm-log.c"
                             "daemon/gdm-server.c"
                             "daemon/gdm-session-worker.c"
                             "daemon/gdm-session-worker-job.c")
                (("#include <systemd/sd-daemon\\.h>") ""))
              ;; Use elogind for sd-login.
              (substitute* '("common/gdm-common.c"
                             "daemon/gdm-local-display-factory.c"
                             "daemon/gdm-manager.c"
                             "libgdm/gdm-user-switching.c")
                (("#include <systemd/sd-login\\.h>")
                 "#include <elogind/sd-login.h>"))
              ;; Look for system-installed sessions in
              ;; /run/current-system/profile/share.
              (substitute* '("libgdm/gdm-sessions.c"
                             "daemon/gdm-session.c"
                             "daemon/gdm-display.c"
                             "daemon/gdm-launch-environment.c")
                (("DATADIR \"/x")
                 "\"/run/current-system/profile/share/x")
                (("DATADIR \"/wayland")
                 "\"/run/current-system/profile/share/wayland")
                (("DATADIR \"/gnome")
                 "\"/run/current-system/profile/share/gnome"))
              (let ((propagate '("GDM_CUSTOM_CONF"
                                 "GDM_DBUS_DAEMON"
                                 "GDM_X_SERVER"
                                 "GDM_X_SESSION"
                                 ;; XXX: Remove this once GNOME Shell is
                                 ;; a dependency of GDM.
                                 "XDG_DATA_DIRS")))
                (substitute* "daemon/gdm-session.c"
                  (("set_up_session_environment \\(self\\);")
                   (apply string-append
                          "set_up_session_environment (self);\n"
                          (map (lambda (name)
                                 (string-append
                                  "gdm_session_set_environment_variable "
                                  "(self, \"" name "\","
                                  "g_getenv (\"" name "\"));\n"))
                               propagate)))
                  ;; This is used by remote sessions, such as when using VNC.
                  (("\\(GDMCONFDIR \"/Xsession \\\\\"%s\\\\\"\", command)")
                   "(\"%s \\\"%s\\\"\", g_getenv (\"GDM_X_SESSION\"), command)")))
              ;; Find the configuration file using an environment variable.
              (substitute* '("common/gdm-settings.c")
                (("GDM_CUSTOM_CONF")
                 (string-append "(g_getenv(\"GDM_CUSTOM_CONF\") != NULL"
                                " ? g_getenv(\"GDM_CUSTOM_CONF\")"
                                " : GDM_CUSTOM_CONF)")))
              ;; Use service-supplied path to X.
              (substitute* '("daemon/gdm-server.c")
                (("\\(X_SERVER X_SERVER_ARG_FORMAT")
                 "(\"%s\" X_SERVER_ARG_FORMAT, g_getenv (\"GDM_X_SERVER\")"))
              (substitute* '("daemon/gdm-wayland-session.c"
                             "daemon/gdm-x-session.c")
                (("\"dbus-daemon\"")
                 "g_getenv (\"GDM_DBUS_DAEMON\")")
                (("X_SERVER")
                 "g_getenv (\"GDM_X_SERVER\")")
                (("GDMCONFDIR \"/Xsession\"")
                 "g_getenv (\"GDM_X_SESSION\")"))
              ;; Use an absolute path for GNOME Session.
              (substitute* "daemon/gdm-launch-environment.c"
                (("\"gnome-session\"")
                 (format #f "~s"
                         (search-input-file inputs "bin/gnome-session"))))
              ;; Do not automatically select the placeholder session.
              (substitute* "daemon/gdm-session.c"
                (("!g_str_has_suffix [(]base_name, \"\\.desktop\"[)]")
                 (string-append "!g_str_has_suffix (base_name, \".desktop\") || "
                                "(g_strcmp0(search_dirs[i], \""
                                #$output "/share/gdm/BuiltInSessions/"
                                "\") == 0 && "
                                "g_strcmp0(base_name, \"fail.desktop\") == 0)"))
                (("g_error [(]\"GdmSession: no session desktop files installed, aborting\\.\\.\\.\"[)];")
                 "{ self->fallback_session_name = g_strdup(\"fail\"); goto out; }"))))
          (add-before 'install 'install-logo
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((icon (search-input-file inputs "share/icons/hicolor/\
scalable/apps/guix-white-icon.svg"))
                    (schema (string-append #$output "/share/glib-2.0/schemas/\
org.gnome.login-screen.gschema.override")))
                (mkdir-p (dirname schema))
                (with-output-to-file schema
                  (lambda ()
                    (format #t "\
[org.gnome.login-screen]
logo='~a'~%" icon))))))
          ;; GDM requires that there be at least one desktop entry
          ;; file.  This phase installs a hidden one that simply
          ;; fails.  This enables users to use GDM with a
          ;; '~/.xsession' script with no other desktop entry files.
          ;; See <https://bugs.gnu.org/35068>.
          (add-after 'install 'install-placeholder-desktop-entry
            (lambda _
              (let* ((sessions (string-append #$output
                                              "/share/gdm/BuiltInSessions"))
                     (fail (string-append sessions "/fail.desktop")))
                (mkdir-p sessions)
                (with-output-to-file fail
                  (lambda ()
                    (for-each
                     display
                     '("[Desktop Entry]\n"
                       "Encoding=UTF-8\n"
                       "Type=Application\n"
                       "Name=Fail\n"
                       "Comment=This session fails immediately.\n"
                       "NoDisplay=true\n"
                       "Exec=false\n")))))))
          ;; GDM needs GNOME Session to run these applications.  We link
          ;; their autostart files in `share/gdm/greeter/autostart'
          ;; because GDM explicitly tells GNOME Session to look there.
          ;;
          ;; XXX: GNOME Shell should be linked here too, but currently
          ;; GNOME Shell depends on GDM.
          (add-after 'install 'link-autostart-files
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((autostart (string-append #$output "/share/gdm/"
                                              "greeter/autostart"))
                    (settings #$(this-package-input "gnome-settings-daemon")))
                (mkdir-p autostart)
                (with-directory-excursion autostart
                  (for-each (lambda (desktop)
                              (symlink desktop (basename desktop)))
                            (find-files
                             (string-append settings "/etc/xdg")))))))
          ;; GDM needs some additional programs available via XDG_DATA_DIRS,
          ;; to make accessibility settings and related services available.
          (add-after 'install 'wrap-accessibility-dependencies
            (lambda _
              (wrap-program (string-append #$output "/bin/gdm")
                `("XDG_DATA_DIRS" ":" prefix
                  #$(map (lambda (input)
                           (file-append (this-package-input input) "/share"))
                         '("at-spi2-core"
                           "dconf"
                           "gnome-control-center")))))))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-compile-schemas, etc.
           dconf
           gobject-introspection
           guix-icons
           intltool
           itstool
           libxml2
           pkg-config))
    (inputs
     (list accountsservice
           at-spi2-core
           check                        ; for testing
           dbus
           dconf                        ; for wrap-accessibility-dependencies
           egl-wayland
           elogind
           eudev
           gnome-session
           gnome-control-center
           gnome-settings-daemon
           gtk+
           iso-codes/pinned
           json-glib
           libcanberra
           libgudev
           linux-pam))
    (synopsis "Display manager for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GDM/")
    (description
     "GNOME Display Manager is a system service that is responsible for
providing graphical log-ins and managing local and remote displays.")
    (license license:gpl2+)))

(define-public libgtop
  (package
    (name "libgtop")
    (version "2.41.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgtop/"
                                  (version-major+minor version) "/"
                                  "libgtop-" version ".tar.xz"))
              (sha256
               (base32
                "136snaww293n1dfdswr764yhd0d55s5z4s3m5x2s4blfjpgpcmkp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gobject-introspection intltool perl pkg-config))
    (propagated-inputs
     (list glib)) ; required by libgtop-2.0.pc
    (synopsis "Portable system access library")
    (home-page "https://www.gnome.org/")
    (description
     "LibGTop is a library to get system specific data such as CPU and memory
usage and information about running processes.")
    (license license:gpl2+)))

(define-public gnome-bluetooth
  (package
    (name "gnome-bluetooth")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-bluetooth/"
                                  (version-major version) "/"
                                  "gnome-bluetooth-" version ".tar.xz"))
              (sha256
               (base32
                "0d9n02a4ii7gr29vxn5qbhib6ddx9v6fgk34wnkg3d5qfpngwj0v"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           `(,glib "bin")               ;for gdbus-codegen, etc.
           libxml2
           pkg-config
           python
           python-dbus))
    (propagated-inputs
     ;; These are all required by the gnome-bluetooth .pc file.
     (list glib
           gtk
           libadwaita))
    (inputs
     (list eudev
           gsound
           libnotify
           python-dbus
           upower))
    (synopsis "GNOME Bluetooth subsystem")
    (home-page "https://wiki.gnome.org/Projects/GnomeBluetooth")
    (description
     "This package contains tools for managing and manipulating Bluetooth
devices using the GNOME desktop.")
    (license license:lgpl2.1+)))

(define-public tecla
  (package
    (name "tecla")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "03qvb526zywhh9059adwm8iz682rckbwm4c9fl4qnnkshsmiw22a"))))
    (build-system meson-build-system)
    (arguments (list #:glib-or-gtk? #t))
    (inputs (list gtk libadwaita))
    (native-inputs (list `(,glib "bin") gnu-gettext pkg-config))
    (home-page "https://gitlab.gnome.org/GNOME/tecla")
    (synopsis "Keyboard layout viewer")
    (description "Tecla is a keyboard layout viewer based on GTK 4 and
Libadwaita.")
    (license license:gpl2+)))

(define-public gnome-control-center
  (package
    (name "gnome-control-center")
    (version "46.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0q4frcgq3466f9f12p7s63371msngwhf71lpj1dy6hj25wgagl0s"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "panels/system/datetime/tz.h"
                (("/usr/share/zoneinfo/zone.tab")
                 (search-input-file inputs "share/zoneinfo/zone.tab")))
              (substitute* "tests/datetime/test-endianess.c"
                (("/usr/share/locale")
                 (search-input-directory inputs "share/locale")))
              (substitute* '("panels/network/net-device-bluetooth.c"
                             "panels/network/net-device-mobile.c"
                             "panels/network/connection-editor/net-connection-editor.c")
                (("\"nm-connection-editor")
                 (string-append "\"" (search-input-file
                                      inputs "bin/nm-connection-editor"))))))
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (replace 'check
            (lambda* (#:key parallel-tests? tests? #:allow-other-keys)
              (when tests?
                ;; Tests require a running X server.
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1")
                ;; For the missing /var/lib/dbus/machine-id
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (setenv "NO_AT_BRIDGE" "1")
                (setenv "HOME" "/tmp")
                (setenv "XDG_RUNTIME_DIR" (string-append (getcwd) "/runtime-dir"))
                (mkdir (getenv "XDG_RUNTIME_DIR"))
                (chmod (getenv "XDG_RUNTIME_DIR") #o700)
                (setenv "MESON_TESTTHREADS"
                        (if parallel-tests?
                            (number->string (parallel-job-count))
                            "1"))
                (invoke "dbus-run-session" "--"
                        "meson" "test" "-t" "0")))))))
    (native-inputs
     (list docbook-xsl
           gettext-minimal
           `(,glib "bin")               ;for glib-mkenums, etc.
           libxslt
           pkg-config
           python
           python-dbusmock
           xorg-server-for-tests
           setxkbmap))
    (inputs
     (list accountsservice
           colord-gtk
           cups
           dconf
           gcr
           gnome-bluetooth
           gnome-desktop
           gnome-online-accounts
           gnome-session
           gnome-settings-daemon
           gnutls
           grilo
           gsound
           ibus
           json-glib
           libadwaita
           libgudev
           libgtop
           libnma
           libpwquality
           (librsvg-for-system)             ;for loading SVG files
           libsecret
           libsoup
           libxml2
           libwacom
           mesa
           mit-krb5
           modem-manager
           network-manager-applet
           polkit
           pulseaudio
           samba
           tecla
           tzdata
           udisks
           upower))
    (synopsis "Utilities to configure the GNOME desktop")
    (home-page "https://www.gnome.org/")
    (description
     "This package contains configuration applets for the GNOME desktop,
allowing to set accessibility configuration, desktop fonts, keyboard and mouse
properties, sound setup, desktop theme and background, user interface
properties, screen resolution, and other GNOME parameters.")
    (license license:gpl2+)))

(define-public gnome-shell
  (package
    (name "gnome-shell")
    (version "46.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1dmpv6n05r7ryl4rq39755bv3f1x50kxk049phnlsyfxfn7m1jcs"))))
    (build-system meson-build-system)
    (arguments
     (let ((disallowed-references
            (list (gexp-input glib "bin")
                  (gexp-input libxslt)
                  (gexp-input meson)
                  (gexp-input ruby-sass))))
       (list
        #:glib-or-gtk? #t
        #:disallowed-references disallowed-references
        #:configure-flags
        #~(list "-Dsystemd=false"
                ;; Otherwise, the RUNPATH will lack the final path component.
                (string-append "-Dc_link_args=-Wl,-rpath="
                               #$output "/lib/gnome-shell"))
        #:modules '((guix build meson-build-system)
                    (guix build utils)
                    (ice-9 match)
                    (srfi srfi-1)
                    (srfi srfi-26))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-keysdir
              (lambda _
                (let ((keysdir
                       (string-append #$output
                                      "/share/gnome-control-center/keybindings")))
                  (substitute* "meson.build"
                    (("keysdir =.*")
                     (string-append "keysdir = '" keysdir "'\n"))))))
            (add-after 'unpack 'skip-gtk-update-icon-cache
              ;; Don't create 'icon-theme.cache'.
              (lambda _
                (substitute* "meson.build"
                  (("gtk_update_icon_cache: true")
                   "gtk_update_icon_cache: false"))))
            (add-after 'unpack 'unbreak-shell-tests
              (lambda _
                ;; Lest non-fatal dbus warnings be made fatal again…
                (substitute* "tests/meson.build"
                  (("shell_testenv\\.set\\('G_DEBUG'" all)
                   (string-append "# " all)))))
            (add-before 'configure 'record-absolute-file-names
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((ibus-daemon (search-input-file inputs "bin/ibus-daemon"))
                      (tecla (search-input-file inputs "bin/tecla")))
                  (substitute* "js/misc/ibusManager.js"
                    (("'ibus-daemon'")
                     (string-append "'" ibus-daemon "'")))
                  (substitute* "js/ui/status/keyboard.js"
                    (("'tecla'")
                     (string-append "'" tecla "'"))))))
            (add-before 'check 'pre-check
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Tests require a running X server.
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1")
                ;; For the missing /var/lib/dbus/machine-id
                (setenv "DBUS_FATAL_WARNINGS" "0")
                (setenv "NO_AT_BRIDGE" "1")
                (setenv "HOME" "/tmp")
                (setenv "XDG_RUNTIME_DIR" (string-append (getcwd) "/runtime-dir"))
                (mkdir (getenv "XDG_RUNTIME_DIR"))
                (chmod (getenv "XDG_RUNTIME_DIR") #o700)))
            (add-after 'install 'wrap-programs
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((gi-typelib-path  (getenv "GI_TYPELIB_PATH"))
                      (gst-plugin-path  (getenv "GST_PLUGIN_SYSTEM_PATH"))
                      (python-path
                       (string-join
                        (filter (lambda (item)
                                  (not (any (cut string-prefix? <> item)
                                            '#$disallowed-references)))
                                (string-split (getenv "GUIX_PYTHONPATH") #\:))
                        ":")))
                  (for-each
                   (lambda (prog)
                     (wrap-program (string-append #$output "/bin/" prog)
                       `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                   '("gnome-shell" "gnome-extensions" "gnome-extensions-app"))
                  (substitute* (string-append #$output "/share/gnome-shell/"
                                              "org.gnome.Shell.Extensions")
                    (("^import " all)
                     (string-append "'" gi-typelib-path "'.split(':').forEach("
                                    "path => imports.gi.GIRepository.Repository."
                                    "prepend_search_path(path));\n"
                                    all)))
                  ;; Screencast requires a pipewire service running
                  ;; (i.e. as provided by home-pipewire-service-type)
                  (substitute* (string-append #$output "/share/gnome-shell/"
                                              "org.gnome.Shell.Screencast")
                    (("^import " all)
                     (string-append "'" gi-typelib-path "'.split(':').forEach("
                                    "path => imports.gi.GIRepository.Repository."
                                    "prepend_search_path(path));\n"
                                    "imports.gi.GLib.setenv('GST_PLUGIN_SYSTEM_PATH',"
                                    "[imports.gi.GLib.getenv('GST_PLUGIN_SYSTEM_PATH'),"
                                    "'" gst-plugin-path "'].filter(v => v).join(':'),"
                                    "true);\n"
                                    all))))))
            (add-after 'install 'rewire
              (lambda* (#:key inputs #:allow-other-keys)
                (for-each
                 (lambda (tool)
                   (call-with-output-file (string-append #$output
                                                         "/bin/" tool)
                     (lambda (port)
                       (format port "#!~a
printf '~a is deprecated.  Use the \"gnome-extensions\" CLI or \
\"gnome-extensions-app\" instead.\\n'"
                               (search-input-file inputs "bin/bash")
                               tool))))
                 '("gnome-shell-extension-tool" "gnome-shell-extension-prefs"))))
            (replace 'glib-or-gtk-wrap
              (let ((wrap (assoc-ref %standard-phases 'glib-or-gtk-wrap)))
                (lambda* (#:key inputs outputs #:allow-other-keys)
                  ;; By default glib:bin et al. would end up in the XDG_DATA_DIRS
                  ;; settings of the wrappers created by the 'glib-or-gtk-wrap'
                  ;; phase.  Fix that since we don't need these.
                  (wrap #:inputs
                        (filter (match-lambda
                                  ((label . output)
                                   (not (member output
                                                '#$disallowed-references))))
                                inputs)
                        #:outputs outputs))))))))
    (native-inputs
     (list asciidoc
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-schemas, etc.
           desktop-file-utils           ;for update-desktop-database
           gobject-introspection
           hicolor-icon-theme
           libxslt
           perl
           pkg-config
           python
           python-dbus
           python-dbusmock
           ruby-sass
           sassc
           ;; For tests
           xorg-server-for-tests))
    (inputs
     (list accountsservice
           bash-minimal
           docbook-xsl
           evolution-data-server
           gcr
           gdm
           librsvg
           gjs
           gtk
           gnome-autoar
           gnome-bluetooth
           gnome-desktop
           gnome-settings-daemon
           graphene
           gst-plugins-base
           gst-plugins-good
           ibus
           libcanberra
           libcroco
           libgweather4
           libnma
           libsoup
           mesa-headers
           mutter
           network-manager-applet
           pipewire
           polkit
           pulseaudio
           python-pygobject
           startup-notification
           tecla                        ;for keyboard previews
           telepathy-logger
           upower
           ;; XXX: These requirements were added in 3.24, but no mention in NEWS.
           ;; Missing propagation? See also: <https://bugs.gnu.org/27264>
           librsvg
           geoclue))
    (synopsis "Desktop shell for GNOME")
    (home-page "https://wiki.gnome.org/Projects/GnomeShell")
    (description
     "GNOME Shell provides core user interface functions for the GNOME desktop,
like switching to windows and launching applications.")
    (license license:gpl2+)))

(define-public gtk-vnc
  (package
    (name "gtk-vnc")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/" name "/"
                       (version-major+minor version) "/"
                       name "-" version ".tar.xz"))
       (sha256
        (base32 "1kpih1gnp3hmsx4l6pig10zn7gd3s5fwm0k6icax0n859sn669si"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))   ; To wrap binaries and/or compile schemas
    (native-inputs
     (append
      ;; GJS depends on Rust so remove the GJS dependency on other platforms.
      (if (supported-package? gjs)
          (list gjs)
          '())
       (list gettext-minimal
             `(,glib "bin")
             gobject-introspection
             perl
             pkg-config
             python-wrapper
             vala)))
    (inputs
     (list cairo
           (librsvg-for-system)))
    (propagated-inputs
     ;; These are all in Requires or Requires.private of the .pc files.
     (list cyrus-sasl
           gdk-pixbuf
           glib
           gnutls
           gtk+
           libgcrypt
           libx11
           pulseaudio
           zlib))
    (synopsis "VNC client viewer widget for GTK+")
    (description "GTK-VNC is a project providing client side APIs for the RFB
protocol / VNC remote desktop technology.  It is built using coroutines allowing
it to be completely asynchronous while remaining single threaded.  It provides a
core C library, and bindings for Python (PyGTK).")
    (home-page "https://wiki.gnome.org/Projects/gtk-vnc")
    (license license:lgpl2.1+)))

(define-public gnome-autoar
  (package
    (name "gnome-autoar")
    (version "0.4.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1gns2nkcgfgmfk5mvyq36cg7f5dzyii45qmb4cpsmgrqzh357343"))))
    (build-system meson-build-system)
    (native-inputs
     (list gobject-introspection `(,glib "bin") pkg-config))
    (propagated-inputs
     (list libarchive))  ; Required by gnome-autoar-0.pc
    (inputs
     (list gtk+))
    (synopsis "Archives integration support for GNOME")
    (home-page "https://git.gnome.org/browse/gnome-autoar/")
    (description
     "GNOME Autoar is a library which makes creating and extracting archives
easy, safe, and automatic.")
    (license license:lgpl2.1+)))

(define-public tracker
  (package
    (name "tracker")
    (version "3.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/tracker/"
                                  (version-major+minor version) "/"
                                  "tracker-" version ".tar.xz"))
              (sha256
               (base32
                "1yfi53fpfszfjajrqf1g80cri472k6wxpxj6g3nwa13yjd84lgdb"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:test-options `(list ,@(if (target-riscv64?)
                                  `("--timeout-multiplier" "10")
                                  '("--timeout-multiplier" "2")))
      #:configure-flags
      ;; Otherwise, the RUNPATH will lack the final path component.
      #~(list (string-append "-Dc_link_args=-Wl,-rpath="
                             #$output "/lib:"
                             #$output "/lib/tracker-3.0")
              "-Ddocs=false"
              "-Dsystemd_user_services=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "utils/trackertestutils/__main__.py"
                (("/bin/bash")
                 (search-input-file inputs "bin/bash")))))
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              #$@(if (not (target-64bit?))
                     ;; On 32-bit systems, the far away dates are incorrect,
                     ;; and the floats are not parsed exactly.
                     '((substitute*
                           "tests/libtracker-sparql/tracker-statement-test.c"
                         (("g_assert_cmpfloat *\\((.*), ==, ([0-9.e-]+)\\);"
                           total actual expected)
                          (string-append "g_assert_cmpfloat_with_epsilon ("
                                         actual ", " expected ", 1e-12);")))
                       (substitute* "tests/core/tracker-sparql-test.c"
                         (("\\{ \"datetime/direct-1\", .* \\},")
                          "/* datetime test disabled */")))
                     '())
              *unspecified*))
          (add-before 'configure 'set-shell
            (lambda _
              (setenv "SHELL" (which "bash"))))
          (add-before 'configure 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((manpage "/etc/asciidoc/docbook-xsl/manpage.xsl")
                     (file (search-input-file inputs manpage)))
                (substitute* "docs/manpages/meson.build"
                  (("/etc/asciidoc[^']+")
                   file)))))
          (replace 'check
            (lambda* (#:key tests? test-options #:allow-other-keys)
              (when tests?
                ;; Some tests expect to write to $HOME.
                (setenv "HOME" "/tmp")
                (apply invoke "dbus-run-session" "--" "meson" "test"
                       "--print-errorlogs" test-options)))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           (libc-utf8-locales-for-target)
           gobject-introspection
           docbook-xsl
           docbook-xml
           gsettings-desktop-schemas
           asciidoc
           libxslt
           cmake-minimal
           python-pygobject
           gtk-doc/stable
           dbus
           pkg-config
           python
           vala))
    (inputs
     (list bash-minimal
           dbus
           libsoup))
    (propagated-inputs
     ;; These are in Requires or Requires.private of tracker-sparql-3.0.pc.
     (list glib
           icu4c                ;libunistring gets miner-miner-fs test to fail
           json-glib
           libxml2
           sqlite))
    (synopsis "Metadata database, indexer and search tool")
    (home-page "https://wiki.gnome.org/Projects/Tracker")
    (description
     "Tracker is a search engine and triplestore for desktop, embedded and mobile.

It is a middleware component aimed at desktop application developers who want
their apps to browse and search user content.  It's not designed to be used
directly by desktop users, but it provides a commandline tool named
@command{tracker} for the adventurous.

Tracker allows your application to instantly perform full-text searches across
all documents.  This feature is used by the @{emph{search} bar in GNOME Files, for
example.  This is achieved by indexing the user's home directory in the
background.

Tracker also allows your application to query and list content that the user
has stored.  For example, GNOME Music displays all the music files that are
found by Tracker.  This means that GNOME Music doesn't need to maintain a
database of its own.

If you need to go beyond simple searches, Tracker is also a linked data
endpoint and it understands SPARQL.")
    ;; https://gitlab.gnome.org/GNOME/tracker/-/blob/master/COPYING:
    ;; src/libtracker-*/* and src/tracker-extract/* are covered by lgpl2.1+,
    ;; libstemmer is bsd-3 and the rest is gpl2+.
    (license (list license:gpl2+
                   license:bsd-3
                   license:lgpl2.1+))))

(define-public tracker-miners
  (package
    (name "tracker-miners")
    (version "3.7.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/tracker-miners/"
                                  (version-major+minor version)
                                  "/tracker-miners-" version ".tar.xz"))
              (sha256
               (base32
                "1zm57pih7csgipw3w2b1sgadvfszik70sbz4gr5pn6aw9caqhhz7"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       (list "-Dminer_rss=false"        ; libgrss is required.
             ;; Ensure the RUNPATH contains all installed library locations.
             (string-append "-Dc_link_args=-Wl,-rpath="
                            (assoc-ref %outputs "out")
                            "/lib/tracker-miners-3.0")
             ;; TODO: Check if this is only a build-time failure, or add
             ;; variants to explicitly enable this features, (see:
             ;; https://gitlab.gnome.org/GNOME/tracker-miners/-/issues/300).
             "-Dlandlock=disabled"
             ;; TODO: Enable functional tests. Currently, the following error
             ;; appears:
             ;; Exception: The functional tests require DConf to be the default
             ;; GSettings backend. Got GKeyfileSettingsBackend instead.
             "-Dfunctional_tests=false"
             "-Dsystemd_user_services=false")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-shell
           (lambda _
             (setenv "SHELL" (which "bash"))))
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((manpage "/etc/asciidoc/docbook-xsl/manpage.xsl")
                    (file (search-input-file inputs manpage)))
               (substitute* "docs/manpages/meson.build"
                 (("/etc/asciidoc[^']+")
                  file)))))
         (add-before 'configure 'fix-tests
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Disable those tests that require the functional_tests option
             ;; to be true and the UPower daemon to be started.
             (substitute* "examples/python/meson.build"
               (("foreach example_name:.*")
                "foreach example_name: []"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               ;; Some tests expect to write to $HOME.
               (setenv "HOME" "/tmp")
               (setenv "LANG" "en_US.UTF-8")
               (invoke "dbus-run-session" "--" "meson" "test"
                       "--print-errorlogs"
                       ;; Do not run the slow test, which fail (see:
                       ;; https://gitlab.gnome.org/GNOME/tracker-miners
                       ;; /-/issues/226).
                       "--no-suite" "slow")))))))
    (native-inputs
     (list dbus
           gettext-minimal
           `(,glib "bin")
           docbook-xsl
           docbook-xml
           gsettings-desktop-schemas
           asciidoc
           libxslt
           gobject-introspection
           pkg-config
           python-pygobject))
    (inputs
     (list exempi
           ffmpeg
           flac
           giflib
           glib
           gstreamer
           gst-plugins-base
           icu4c
           json-glib
           libcue
           libexif
           libgsf
           libgxps
           libiptcdata
           libjpeg-turbo
           libosinfo
           libpng
           libseccomp
           libsoup
           libtiff
           libvorbis
           libxml2
           poppler
           shared-mime-info
           taglib
           totem-pl-parser
           tracker
           upower
           zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "TRACKER_CLI_SUBCOMMANDS_DIR")
            (separator #f)              ; single entry
            (files `(,(string-append "libexec/tracker"
                                     (version-major version)))))))
    (synopsis "Metadata database, indexer and search tool")
    (home-page "https://wiki.gnome.org/Projects/Tracker")
    (description
     "Tracker is an advanced framework for first class objects with associated
metadata and tags.  It provides a one stop solution for all metadata, tags,
shared object databases, search tools and indexing.")
    ;; src/libtracker-*/* and src/tracker-extract/* are covered by lgpl2.1+,
    ;; src/gvdb/* are covered by lgpl2.0+, and the rest is gpl2+.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:lgpl2.0+))))

(define-public nautilus
  (package
    (name "nautilus")
    (version "46.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0kb21wjvz9nb6sq29hqpzbrcxfhiiznzszj387gwjvgcyph4ipxh"))
              (patches
               (search-patches "nautilus-extension-search-path.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/nautilus-autorun-software.c"
                (("g_file_new_for_path \\(\"/bin/sh\");")
                 (format #f "g_file_new_for_path (~s);"
                         (search-input-file inputs "bin/sh"))))))
          (add-after 'unpack 'patch-tracker3-command
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/nautilus-tag-manager.c"
                (("\"tracker3\"")
                 (string-append "\""
                                (search-input-file inputs "/bin/tracker3")
                                "\"")))))
          (add-after 'unpack 'fix-tests
            (lambda _
              ;; The tracker test hangs in the build container (see:
              ;; https://gitlab.gnome.org/GNOME/nautilus/-/issues/2486).
              (substitute* "test/automated/displayless/meson.build"
                (("^foreach t: tracker_tests" all)
                 (string-append "tracker_tests = []\n" all)))
              ;; /etc does not have that many files in our build container.
              (substitute* "test/automated/displayless/test-directory.c"
                (("g_assert_cmpint \\(g_list_length \\(files\\), >, 10\\);")
                 "g_assert_cmpint (g_list_length (files), >, 1);"))))
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")    ;some tests require a writable HOME
              (setenv "XDG_DATA_DIRS"
                      (string-append (getenv "XDG_DATA_DIRS")
                                     ":" #$output "/share")))))))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           `(,glib "bin")               ;for glib-mkenums, etc.
           gettext-minimal
           gobject-introspection
           pkg-config
           python
           python-pygobject))
    (inputs
     (list bash-minimal
           dconf
           gexiv2
           gvfs
           exempi
           gnome-desktop
           gnome-autoar
           gst-plugins-base
           json-glib
           libadwaita
           libportal
           libseccomp
           libselinux
           tracker
           tracker-miners
           ;; XXX: gtk is required by libnautilus-extension.pc
           ;;
           ;; Don't propagate it to reduce "profile pollution" of the 'gnome' meta
           ;; package.  See:
           ;; <http://lists.gnu.org/archive/html/guix-devel/2016-03/msg00283.html>.
           gtk
           libexif
           libxml2))
    (native-search-paths
     (list (search-path-specification
            (variable "NAUTILUS_EXTENSION_PATH")
            (files '("lib/nautilus/extensions-4")))))
    (synopsis "File manager for GNOME")
    (home-page "https://wiki.gnome.org/Apps/Nautilus")
    (description
     "Nautilus (Files) is a file manager designed to fit the GNOME desktop
design and behaviour, giving the user a simple way to navigate and manage its
files.")
    (license license:gpl2+)))

(define-public baobab
  (package
    (name "baobab")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z73fad2c6qqv65d4q41kmfng65chp7ynywz02jp2mnhh9ffykff"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")))))))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           gettext-minimal
           `(,glib "bin")
           itstool
           libxml2
           pkg-config
           python
           vala))
    (inputs (list gtk libadwaita))
    (synopsis "Disk usage analyzer for GNOME")
    (description
     "Baobab (Disk Usage Analyzer) is a graphical application to analyse disk
usage in the GNOME desktop environment.  It can easily scan device volumes or
a specific user-requested directory branch (local or remote).  Once the scan
is complete it provides a graphical representation of each selected folder.")
    (home-page "https://wiki.gnome.org/Apps/Baobab")
    (license license:gpl2+)))

(define-public gnome-backgrounds
  (package
    (name "gnome-backgrounds")
    (version "46.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0biw8hl8kpnxdlkprm0qla2qng2dbyvip4h5d23ng8547723mpad"))))
    (build-system meson-build-system)
    (native-inputs (list gettext-minimal))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-backgrounds")
    (synopsis "Background images for the GNOME desktop")
    (description
     "GNOME backgrounds package contains a collection of graphics files which
can be used as backgrounds in the GNOME Desktop environment.  Additionally,
the package creates the proper framework and directory structure so that you
can add your own files to the collection.")
    (license (list license:gpl2+
                   license:cc-by2.0
                   license:cc-by-sa2.0
                   license:cc-by-sa3.0))))

(define-public gnome-screenshot
  ;; GNOME Screenshot hasn't had a release in a long time, and the last one
  ;; (41) doesn't build with a recent Meson.
  (let ((commit "9f067cf428b6bac78ffac31c1a17a20fb2c24843")
        (revision "0"))
    (package
      (name "gnome-screenshot")
      (version (git-version "41.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.gnome.org/GNOME/gnome-screenshot")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10a3yd9qmfhxiw984a9fyvgrfq6i3w2yxayac0n7qqjl9ysxwb31"))))
      (build-system meson-build-system)
      (arguments
       `(#:glib-or-gtk? #t
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'skip-gtk-update-icon-cache
             ;; Don't create 'icon-theme.cache'.
             (lambda _
               (substitute* "build-aux/postinstall.py"
                 (("gtk-update-icon-cache") "true")))))))
      (native-inputs
       (list appstream-glib
             desktop-file-utils           ; for update-desktop-database
             gettext-minimal
             `(,glib "bin")               ; for glib-compile-schemas, etc.
             pkg-config
             python))
      (inputs
       (list gtk+
             libhandy
             libx11
             libxext))
      (home-page "https://gitlab.gnome.org/GNOME/gnome-screenshot")
      (synopsis "Take pictures of your screen")
      (description
       "GNOME Screenshot is a utility used for taking screenshots of the entire
screen, a window or a user defined area of the screen, with optional
beautifying border effects.")
      (license license:gpl2+))))

(define-public dconf-editor
  (package
    (name "dconf-editor")
    (version "45.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1pa1k510qx4v0ywhqwpj3qfjyz8qiiis4565ghhydnpfg1v2k00i"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'disable-gtk-update-icon-cache
                          (lambda _
                            (setenv "DESTDIR" "/"))))))
    (native-inputs
     (list `(,glib "bin")               ;for glib-compile-schemas, gio-2.0
           desktop-file-utils           ;for update-desktop-database
           `(,gtk "bin")                ;for gtk-update-icon-cache
           intltool
           pkg-config
           vala))
    (inputs
     (list dconf
           gtk+
           libhandy
           libxml2))
    (home-page "https://gitlab.gnome.org/GNOME/dconf-editor")
    (synopsis "Graphical editor for GNOME's dconf configuration system")
    (description
     "Dconf-editor is a graphical tool for browsing and editing the dconf
configuration system for GNOME.  It allows users to configure desktop
software that do not provide their own configuration interface.")
    (license license:lgpl2.1+)))

(define-public gnome-default-applications
  (package
    (name "gnome-default-applications")
    (version "0")
    (build-system trivial-build-system)
    (source #f)
    (propagated-inputs
     (list nautilus evince))
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (apps (string-append out "/share/applications")))
           (mkdir-p apps)
           (call-with-output-file (string-append apps "/gnome-mimeapps.list")
             (lambda (port)
               (format port "[Default Applications]\n")
               (format port "inode/directory=org.gnome.Nautilus.desktop\n")
               (format port "application/pdf=evince.desktop\n")
               (format port "application/postscript=evince.desktop\n")))
           #t))))
    (synopsis "Default MIME type associations for the GNOME desktop")
    (description
     "Given many installed packages which might handle a given MIME type, a
user running the GNOME desktop probably has some preferences: for example,
that folders be opened by default by the Nautilus file manager, not the Baobab
disk usage analyzer.  This package establishes that set of default MIME type
associations for GNOME.")
    (license license:gpl3+)
    (home-page #f)))

(define-public libgovirt
  (package
    (name "libgovirt")
    (version "0.3.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0z118di7cg654f5zb8xn5w60ghgqngsc1p7izr1pw01dkxbw6bxi"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'set-home
                          (lambda _
                            ;; The tests require a writable HOME.
                            (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           glib-networking              ; GIO plugin--for the tests
           gobject-introspection
           gsettings-desktop-schemas
           pkg-config))
    (propagated-inputs
     ;; These dependencies are required by govirt-1.0.pc.
     (list glib
           rest-next))
    (synopsis "GoVirt Library")
    (description "GoVirt is a GObject wrapper for the oVirt REST API.")
    (home-page "https://gitlab.gnome.org/GNOME/libgovirt")
    (license license:gpl2+)))

(define-public gnome-weather
  (package
    (name "gnome-weather")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "17fllgkvsbsklnazxap4rg2bg2cf5xwgqkgyy8a2wrygbiq2cf0m"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-service-file
            (lambda _
              (substitute* "data/org.gnome.Weather.service.in"
                (("Exec=[[:graph:]]+")
                 (string-append "Exec=" #$output
                                "/bin/gnome-weather")))))
          (add-after 'unpack 'disable-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))))
          (add-after 'install 'fix-desktop-file
            ;; FIXME: "gapplication launch org.gnome.Weather" fails for some
            ;; reason.  See https://issues.guix.gnu.org/issue/39324.
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (search-input-file
                            outputs
                            "share/applications/org.gnome.Weather.desktop")
                (("Exec=.*") "Exec=gnome-weather\n"))))
          (add-after 'install 'wrap
            (lambda* (#:key outputs #:allow-other-keys)
              ;; GNOME Weather needs the typelib files of GTK+, Pango etc at
              ;; runtime.
              (wrap-program (search-input-file outputs "bin/gnome-weather")
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config))
    (inputs
     (list appstream-glib
           bash-minimal
           geoclue
           gjs
           gsettings-desktop-schemas
           gtk
           libadwaita
           libgweather4))
    (synopsis "Weather monitoring for GNOME desktop")
    (description "GNOME Weather is a small application that allows you to
monitor the current weather conditions for your city, or anywhere in the
world.")
    (home-page "https://wiki.gnome.org/Apps/Weather")
    (license license:gpl2+)))

(define-syntax gnome-meta-package
  (lambda (x)
    (syntax-case x ()
      ((_ field ...)
       (with-syntax ((base (datum->syntax x 'base)))
         #'(let ((base
                  (package
                    (name #f)      ; we're hidden by default, so don't worry
                    (version (package-version gnome-shell))
                    (source #f)
                    (build-system trivial-build-system)
                    (arguments
                     (list #:builder
                           #~(begin (format (current-warning-port)
                                            "Building ~a is useless.  \
Refer to its propagated inputs instead.\n"
                                            #$(package-name this-package))
                                    (mkdir #$output))))
                    (home-page "https://www.gnome.org")
                    (synopsis "Graphical desktop environment")
                    (description "GNOME is a graphical desktop environment.
It includes a wide variety of applications with a common interface for
browsing the web, editing text and images, creating documents and diagrams,
playing media, scanning, and much more.")
                    (license license:gpl2+)
                    (properties `((hidden? . #t))))))
             (package (inherit base)
                      field ...)))))))

(define-public gnome-meta-core-services
  (gnome-meta-package
   (name "gnome-meta-core-services")
   (propagated-inputs
    (list accountsservice
          evolution-data-server         ;for the calendar widget
          network-manager
          packagekit
          upower))))

(define-public gnome-meta-core-shell
  (gnome-meta-package
   (name "gnome-meta-core-shell")
   (propagated-inputs (list adwaita-icon-theme
                            gdm
                            glib-networking
                            gnome-backgrounds
                            gnome-bluetooth
                            gnome-color-manager
                            gnome-control-center
                            gnome-desktop
                            gnome-initial-setup
                            gnome-keyring
                            gnome-menus
                            gnome-remote-desktop
                            gnome-session
                            gnome-settings-daemon
                            gnome-shell
                            gnome-shell-extensions
                            gnome-themes-extra
                            gnome-user-docs
                            gnome-user-share
                            gsettings-desktop-schemas
                            gvfs
                            mutter
                            orca
                            rygel
                            sushi))))

(define-public gnome-meta-core-utilities
  (gnome-meta-package
   (name "gnome-meta-core-utilities")
   (propagated-inputs
     (list baobab
           cheese
           eog
           epiphany
           evince
           file-roller
           gnome-calculator
           gnome-calendar
           gnome-characters
           gnome-clocks
           gnome-connections
           gnome-console
           gnome-contacts
           gnome-disk-utility
           gnome-font-viewer
           gnome-maps
           gnome-music
           gnome-photos
           gnome-screenshot
           gnome-system-monitor
           gnome-text-editor
           gnome-weather
           nautilus
           simple-scan
           totem
           tracker-miners
           xdg-desktop-portal-gnome
           yelp))))

(define-public gnome-essential-extras
  (gnome-meta-package
   (name "gnome-essential-extras")
   (propagated-inputs (list at-spi2-core
                            dbus
                            dconf
                            desktop-file-utils
                            font-abattis-cantarell
                            font-dejavu
                            gnome-default-applications
                            gnome-online-accounts
                            gst-plugins-base
                            gst-plugins-good
                            gucharmap
                            hicolor-icon-theme
                            pinentry-gnome3
                            pulseaudio
                            shared-mime-info
                            system-config-printer
                            xdg-desktop-portal
                            xdg-user-dirs
                            yelp
                            zenity))
   (description "This package provides a list of packages required for
a good GNOME experience, mixed from core dependencies and other implicitly
relied-on packages.")))

(define-deprecated/public gnome #f
  (gnome-meta-package
   (name "gnome")
   (propagated-inputs
    (append-map package-propagated-inputs
                (list gnome-meta-core-services
                      gnome-meta-core-shell
                      gnome-meta-core-utilities
                      gnome-essential-extras)))
   (properties (list))))

(define-public byzanz
  ;; The last stable release of Byzanz was in 2011, but there have been many
  ;; useful commits made to the Byzanz repository since then that it would be
  ;; silly to use such an old release.
  (let ((commit "f7af3a5bd252db84af8365bd059c117a7aa5c4af"))
    (package
      (name "byzanz")
      (version (string-append "0.2-1." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.gnome.org/browse/byzanz")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1l60myzxf9cav27v5v3nsijlslz9r7ip6d5kiirfpkf9k0w26hz3"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (add-before 'bootstrap 'build-without-Werror
             (lambda _
               ;; The build system cleverly detects that we're not building from
               ;; a release tarball and turns on -Werror for GCC.
               ;; Unsurprisingly, there is a warning during compilation that
               ;; causes the build to fail unnecessarily, so we remove the flag.
               (substitute* '("configure.ac")
                 (("-Werror") ""))
               #t)))))
      (native-inputs
       (list autoconf
             automake
             gnome-common
             intltool
             libtool
             pkg-config
             which))
      (inputs
       (list glib gstreamer gst-plugins-base gtk+))
      (synopsis "Desktop recording program")
      (description "Byzanz is a simple desktop recording program with a
command-line interface.  It can record part or all of an X display for a
specified duration and save it as a GIF encoded animated image file.")
      (home-page "https://git.gnome.org/browse/byzanz")
      (license license:gpl2+))))

(define-public gnome-authenticator
  (package
    (name "gnome-authenticator")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/Authenticator.git/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zavax35n048spx097ymiq31s8b879qwbg8xmcxcx73r6m823mic"))))
    (build-system cargo-build-system)
    (arguments
     (list
      #:install-source? #f
      #:vendor-dir "vendor"
      #:cargo-inputs
      (list rust-aes-gcm-0.10
            rust-anyhow-1
            rust-async-std-1
            rust-aperture-0.3
            rust-ashpd-0.6
            rust-data-encoding-2
            rust-diesel-2
            rust-diesel-migrations-2
            rust-futures-channel-0.3
            rust-futures-executor-0.3
            rust-futures-util-0.3
            rust-gettext-rs-0.7
            rust-gtk4-0.7
            rust-hex-0.4
            rust-image-0.24
            rust-libadwaita-0.5
            rust-oo7-0.2
            rust-percent-encoding-2
            rust-prost-0.12
            rust-qrencode-0.14
            rust-quick-xml-0.30
            rust-rand-0.8
            rust-reqwest-0.11
            rust-ring-0.17
            rust-rust-argon2-2
            rust-scrypt-0.11
            rust-search-provider-0.6
            rust-serde-1
            rust-serde-json-1
            rust-svg-metadata-0.4
            rust-tokio-1
            rust-tracing-0.1
            rust-tracing-subscriber-0.3
            rust-url-2
            rust-uuid-1
            rust-zbar-rust-0.0.23   ; any 0.0.*
            rust-zeroize-1)
      #:imported-modules `(,@%meson-build-system-modules
                           ,@%glib-or-gtk-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules `((guix build cargo-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  ((guix build meson-build-system) #:prefix meson:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-after 'unpack 'prepare-for-build
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))
              ;; Help the tests find the Cargo.toml in the sources.
              (substitute* "src/meson.build"
                (("'test'") "'test', cargo_options"))
              (delete-file "Cargo.lock")))
          ;; Add meson-configure phase here and not before 'configure because
          ;; the meson 'configure phase changes to a different directory and
          ;; we need it created before unpacking the crates.
          (add-before 'unpack-rust-crates 'meson-configure
            (lambda args
              (apply (assoc-ref meson:%standard-phases 'configure)
                     #:build-type "debugoptimized"
                     #:configure-flags '()
                     args)))
          (replace 'build
            (assoc-ref meson:%standard-phases 'build))
          (replace 'check
            (lambda args
              (apply (assoc-ref meson:%standard-phases 'check)
                     #:test-options '()
                     args)))
          (replace 'install
            (assoc-ref meson:%standard-phases 'install))
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
          (add-after 'glib-or-gtk-wrap 'wrap-extra-paths
            (lambda _
              (let ((gst-plugins-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                (wrap-program (string-append #$output "/bin/authenticator")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" suffix (,gst-plugins-path))))))
          (add-after 'strip 'shrink-runpath
            (assoc-ref meson:%standard-phases 'shrink-runpath)))))
    (native-inputs (list gettext-minimal
                         `(,glib "bin") ; for glib-compile-schemas
                         meson
                         ninja
                         pkg-config))
    (inputs (list bash-minimal
                  glib
                  gstreamer
                  gst-plugins-base
                  gst-plugins-bad
                  gtk
                  libadwaita
                  openssl
                  pipewire      ; Needed but not listed
                  sqlite
                  zbar))
    (home-page "https://apps.gnome.org/Authenticator")
    (synopsis "Generate two-factor codes")
    (description "Simple application for generating Two-Factor Authentication
Codes:

It features:

@itemize
@item Time-based/Counter-based/Steam methods support
@item SHA-1/SHA-256/SHA-512 algorithms support
@item QR code scanner using a camera or from a screenshot
@item Lock the application with a password
@item Beautiful UI
@item GNOME Shell search provider
@item Backup/Restore from/into known applications like FreeOTP+,
Aegis (encrypted / plain-text), andOTP, Google Authenticator
@end itemize")
    (license license:gpl3+)))

(define-public authenticator
  (deprecated-package "authenticator" gnome-authenticator))

(define-public gsound
  (package
    (name "gsound")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gsound/"
                                  (version-major+minor version) "/"
                                  "gsound-" version ".tar.xz"))
              (sha256
               (base32
                "06l80xgykj7x1kqkjvcq06pwj2rmca458zvs053qc55x3sg06bfa"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config gobject-introspection vala))
    (propagated-inputs
     (list libcanberra))   ; in Requires.private of gsound.pc
    (home-page "https://wiki.gnome.org/Projects/GSound")
    (synopsis "GObject wrapper for libcanberra")
    (description
     "GSound is a small library for playing system sounds.  It's designed to be
used via GObject Introspection, and is a thin wrapper around the libcanberra C
library.")
    (license license:lgpl2.1+)))

(define-public libzapojit
  (let ((revision "1")
        (commit "99d49bac5edc4afdcac742a0a142908e405597b0"))
    (package
      (name "libzapojit")
      (version (git-version "0.0.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.gnome.org/Archive/libzapojit")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "12frqg925rmic3rf37h5vs48xdy3mfi4ip24v0bl73h5sxy8n828"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf autoconf-archive automake
             `(,glib "bin")
             gtk-doc gobject-introspection
             intltool libtool pkg-config))
      (inputs
       (list gnome-online-accounts json-glib rest))
      (home-page "https://wiki.gnome.org/Projects/Zapojit")
      (synopsis "Library for accessing SkyDrive and Hotmail")
      (description
       "Libzapojit is a GLib-based library for accessing online service APIs of
Microsoft SkyDrive and Hotmail, using their REST protocols.")
      (license license:lgpl2.1+))))

(define-public gnome-clocks
  (package
    (name "gnome-clocks")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1iwr9ydg8bvd0xr4npr321km5wvkkdq2cmlbcr77byffrmwcb8za"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")               ; for glib-compile-resources
           itstool
           pkg-config
           vala))
    (inputs
     (list geoclue
           geocode-glib
           glib
           gnome-desktop
           gsound
           gtk
           libadwaita
           libgweather4))
    (home-page "https://wiki.gnome.org/Apps/Clocks")
    (synopsis "GNOME's clock application")
    (description
     "GNOME Clocks is a simple clocks application designed to fit the GNOME
desktop.  It supports world clock, stop watch, alarms, and count down timer.")
    (license license:gpl3+)))

(define-public gnome-calendar
  (package
    (name "gnome-calendar")
    (version "47.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0xhynp8jxydbwln2sjax4pxfllvdb78hkqh73s1dq4jd3hzk20rv"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             (setenv "TZDIR"
                     (search-input-directory
                      (or native-inputs inputs) "share/zoneinfo"))
             (setenv "TZ" "UTC"))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; For glib-compile-schemas
           pkg-config
           tzdata-for-tests))
    (inputs
     (list evolution-data-server
           geoclue
           geocode-glib
           gnome-online-accounts
           gsettings-desktop-schemas
           libadwaita
           libdazzle
           libgweather4))
    (home-page "https://wiki.gnome.org/Apps/Calendar")
    (synopsis "GNOME's calendar application")
    (description
     "GNOME Calendar is a simple calendar application designed to fit the GNOME
desktop.  It supports multiple calendars, month, week and year view.")
    (license license:gpl3+)))

(define-public endeavour
  (package
    (name "endeavour")
    (version "43.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/Endeavour")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gbqmwl1xv5526vlh1mxx9h5mpfnnwikrpr5fk8hxmy9x71r6q6n"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases
                       'check))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "GSETTINGS_SCHEMA_DIR"
                      (string-append #$output "/share/glib-2.0/schemas")))))))
    (native-inputs
     (list gettext-minimal
           gobject-introspection
           `(,glib "bin")               ;for glib-compile-resources
           `(,gtk "bin")                ;for gtk-update-icon-cache
           itstool
           pkg-config))
    (inputs
     (list rest                         ;for Todoist plugin
           gtk
           json-glib                    ;for Todoist plugin
           libadwaita
           evolution-data-server
           libical
           libpeas
           libportal
           python-pygobject
           gnome-online-accounts
           gsettings-desktop-schemas))
    (propagated-inputs
     ;; This is so that the Guix System D-Bus service can find the Evolution
     ;; Data Server schemas.
     (list evolution-data-server))
    (home-page "https://wiki.gnome.org/Apps/Todo")
    (synopsis "GNOME's ToDo Application")
    (description "GNOME To Do is a simplistic personal task manager designed
to perfectly fit the GNOME desktop.")
    (license license:gpl3+)))

(define-public dialect
  (package
    (name "dialect")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dialect-app/dialect")
                    (commit version)
                    (recursive? #t))) ;po module
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wac9r33zslyhvadyj7iaapskk7f9pfvia7zlqfksfhkaji6gmna"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs (list blueprint-compiler
                         desktop-file-utils
                         `(,glib "bin")
                         gnu-gettext
                         gobject-introspection
                         `(,gtk "bin")
                         pkg-config))
    (propagated-inputs (list gstreamer
                             libadwaita
                             libsoup
                             python
                             python-gtts
                             python-pygobject
                             python-requests))
    (home-page "https://apps.gnome.org/app/app.drey.Dialect")
    (synopsis "Translation application for GNOME")
    (description
     "Dialect is a simple translation application that uses Google Translate
(default), LibreTranslate or Lingva Translate.  It includes features
like automatic language detection, text-to-speech and clipboard buttons.")
    (license license:gpl3+)))

(define-public gnome-dictionary
  (package
    (name "gnome-dictionary")
    (version "40.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1d8dhcfys788vv27v34i3s3x3jdvdi2kqn2a5p8c937a9hm0qr9f"))
              (patches
               (search-patches "gnome-dictionary-meson-i18n.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "build-aux/meson/post-install.py"
               (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           intltool
           itstool
           pkg-config
           libxml2))
    (inputs
     (list gsettings-desktop-schemas
           gtk+))
    (home-page "https://wiki.gnome.org/Apps/Dictionary")
    (synopsis "Look up words in dictionary sources")
    (description
     "GNOME Dictionary can look for the definition or translation of a word in
existing databases over the internet.")
    (license license:gpl3+)))

(define-public gnome-tweaks
  (package
    (name "gnome-tweaks")
    (version "46.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gnome-tweaks/"
                                  (version-major version) "/"
                                  "gnome-tweaks-" version ".tar.xz"))
              (patches
               (list (search-patch "gnome-tweaks-search-paths.patch")))
              (sha256
               (base32
                "104v62nf0ng1ycsyljci09r95v11vbcicmw2rwz89mpvhmq2l69g"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags #~(list "-Dlocalstatedir=/tmp"
                                "-Dsysconfdir=/tmp")
      #:imported-modules `((guix build python-build-system)
                           ,@%meson-build-system-modules)
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")
               (("update_desktop_database: true")
                "update_desktop_database: false"))))
          (add-after 'install 'wrap
            (assoc-ref python:%standard-phases 'wrap))
          (add-after 'wrap 'wrap-gi-typelib-and-python
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/gnome-tweaks")
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")))
                `("GUIX_PYTHONPATH" ":" prefix
                  (,(python:site-packages inputs outputs)))))))))
    (native-inputs
     (list `(,glib "bin")               ; for glib-compile-resources, etc.
           gettext-minimal
           pkg-config))
    (inputs
     (list bash-minimal                 ; to execute the wrapper program
           gnome-desktop
           gtk
           gobject-introspection
           gsettings-desktop-schemas
           libadwaita
           libgudev
           libnotify
           libsoup
           nautilus
           python
           python-pygobject))
    (synopsis "Customize advanced GNOME options")
    (home-page "https://wiki.gnome.org/Apps/Tweaks")
    (description
     "GNOME Tweaks allows adjusting advanced configuration settings in
GNOME.  This includes things like the fonts used in user interface elements,
alternative user interface themes, changes in window management behavior,
GNOME Shell appearance and extension, etc.")
    (license license:gpl3+)))

(define-public gnome-shell-extensions
  (package
    (name "gnome-shell-extensions")
    (version "46.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "06fcy79wgr3zyqgp6c433a6yfirv808zrhk50nss6mdgxqgd4cnv"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~'("-Dextension_set=all")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'wrap-extensions
            (lambda _
              (use-modules (ice-9 textual-ports)
                           (guix build utils))
              (for-each
               (lambda (file-to-wrap)
                 (with-atomic-file-replacement file-to-wrap
                   (lambda (source wrapped)
                     (format wrapped "'~a'.split(':').forEach("
                             (getenv "GI_TYPELIB_PATH"))
                     (display
                      (string-append
                       "path => imports.gi.GIRepository.Repository"
                       ".prepend_search_path(path));\n")
                      wrapped)
                     (dump-port source wrapped))))
               (find-files "extensions" "(extension|prefs)\\.js")))))))
    (native-inputs
     (list `(,glib "bin")
           gettext-minimal
           gobject-introspection        ; to set GI_TYPELIB_PATH
           pkg-config))
    (inputs
     (list glib
           gnome-menus))                ; for Applications Menu
    (synopsis "Extensions for GNOME Shell")
    (description "GNOME Shell extensions modify and extend GNOME Shell
functionality and behavior.")
    (home-page "https://extensions.gnome.org/")
    (license license:gpl3+)))

(define-public folks
  (package
    (name "folks")
    (version "0.15.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/folks/"
                    (version-major+minor version) "/"
                    "folks-" version ".tar.xz"))
              (sha256
               (base32
                "0ps1243l4vladlylj6f3h830lam2fi43kp1z2qzz6lf3amrv6493"))))
    (build-system meson-build-system)
    (arguments
     '(;; Tests are broken since GLib 2.80
       ;; See <https://gitlab.gnome.org/GNOME/folks/-/issues/140>.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")))))))
    (inputs
     (list bdb
           dbus-glib
           evolution-data-server
           glib
           libgee
           readline
           telepathy-glib))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           python-dbusmock
           pkg-config
           python
           vala))
    (synopsis "Library to aggregate data about people")
    (description "Libfolks is a library that aggregates information about people
from multiple sources (e.g., Telepathy connection managers for IM contacts,
Evolution Data Server for local contacts, libsocialweb for web service contacts,
etc.) to create metacontacts.  It's written in Vala, which generates C code when
compiled.")
    (home-page "https://wiki.gnome.org/Projects/Folks")
    (license license:lgpl2.1+)))

(define-public folks-with-libsoup2
  (package
    (inherit folks)
    (name "folks-with-libsoup2")
    (inputs
     (modify-inputs (package-inputs folks)
       (replace "evolution-data-server" evolution-data-server-3.44)))))

(define-public gfbgraph
  (package
    (name "gfbgraph")
    (version "0.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/gfbgraph/"
                    (version-major+minor version) "/"
                    "gfbgraph-" version ".tar.xz"))
              (sha256
               (base32
                "1qq3cryhby50xms8zh4s6fmw5p0i7dpg1wvsz5ni78cbyyrq3cww"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs
     (list gobject-introspection
           gtk-doc/stable
           pkg-config))
    (inputs
     (list gnome-online-accounts
           json-glib
           rest))
    (synopsis "GLib/GObject wrapper for the Facebook API")
    (description "This library allows you to use the Facebook API from
GLib/GObject code.")
    (home-page "https://wiki.gnome.org/Projects/GFBGraph")
    (license license:lgpl2.1+)))

(define-public libgnomekbd
  (package
    (name "libgnomekbd")
    (version "3.28.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libgnomekbd/"
                                  (version-major+minor version)  "/"
                                  "libgnomekbd-" version ".tar.xz"))
              (sha256
               (base32
                "0w78ix6f52xv0hw1h6fzqh47pk1fwr077agma19hdh3kdmb5kp12"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf
           automake
           `(,glib "bin")
           libtool
           pkg-config
           gettext-minimal
           gobject-introspection))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     (list glib
           gtk+
           libxklavier))
    (home-page "https://www.gnome.org")
    (synopsis "GNOME keyboard configuration library")
    (description
     "Libgnomekbd is a keyboard configuration library for the GNOME desktop
environment, which can notably display keyboard layouts.")
    (license license:lgpl2.0+)))

;;; This package is no longer maintained:
;;; https://wiki.gnome.org/Attic/LibUnique
;;; "Unique is now in maintenance mode, and its usage is strongly discouraged.
;;; Applications should use the GtkApplication class provided by GTK+ 3.0."
(define-public libunique
  (package
    (name "libunique")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version)  "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0f70lkw66v9cj72q0iw1s2546r6bwwcd8idcm3621fg2fgh2rw58"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--disable-dbus" ; use gdbus
                           "--enable-introspection")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gobject-introspection" ,gobject-introspection)
       ("glib:bin" ,glib "bin")
       ("gtk-doc" ,gtk-doc/stable)))
    (propagated-inputs
     ;; Referred to in .h files and .pc.
     (list gtk+))
    (home-page "https://wiki.gnome.org/Attic/LibUnique")
    (synopsis "Library for writing single instance applications")
    (description
     "Libunique is a library for writing single instance applications.  If you
launch a single instance application twice, the second instance will either just
quit or will send a message to the running instance.  Libunique makes it easy to
write this kind of application, by providing a base class, taking care of all
the IPC machinery needed to send messages to a running instance, and also
handling the startup notification side.")
    (license license:lgpl2.1+)))

(define-public gnome-calculator
  (package
    (name "gnome-calculator")
    (version "46.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "0jvv2gfg2g4x9wrllijg08m7idwgbg5x83gp4469s9cbhd1vycfn"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests require a writable HOME.
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ;for glib-compile-schemas, gio-2.0.
           itstool
           pkg-config
           python
           vala))
    (inputs
     (list gsettings-desktop-schemas
           gtksourceview
           libadwaita
           libgee
           libsoup
           libxml2
           mpc
           mpfr))
    (propagated-inputs
     ;; Marked as requires.private in either .pc
     (list libgee glib gtk))
    (home-page "https://wiki.gnome.org/Apps/Calculator")
    (synopsis "Desktop calculator")
    (description
     "Calculator is an application that solves mathematical equations and
is suitable as a default application in a Desktop environment.")
    (license license:gpl3)))

(define-public xpad
  (package
    (name "xpad")
    (version "5.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/xpad/trunk/"
                           version "/+download/"
                           name "-" version ".tar.bz2"))
       (sha256
        (base32
         "1sc2dz4yxx6glnqpnhiby85g2blnsfn8d3fvbaqhdi2hi0q54q7j"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake
           `(,gtk+ "bin") intltool pkg-config))
    (inputs
     (list gtk+ gtksourceview-4 libsm))
    (home-page "https://wiki.gnome.org/Apps/Xpad")
    (synopsis "Virtual sticky note")
    (description
     "Xpad is a sticky note that strives to be simple, fault tolerant,
and customizable.  Xpad consists of independent pad windows, each is
basically a text box in which notes can be written.")
    (license license:gpl3+)))

(define-public gucharmap
  (let ((unicode-files
         '(("Blocks.txt"
            "041sk54v6rjzb23b9x7yjdwzdp2wc7gvfz7ybavgg4gbh51wm8x1")
           ("DerivedAge.txt"
            "04j92xp07v273z3pxkbfmi1svmw9kmnjl9nvz9fv0g5ybk9zk7r6")
           ("NamesList.txt"
            "0vsq8gx7hws8mvxy3nlglpwxw7ky57q0fs09d7w9xgb2ylk7fz61")
           ("Scripts.txt"
            "18c63hx4y5yg408a8d0wx72d2hfnlz4l560y1fsf9lpzifxpqcmx")
           ("UnicodeData.txt"
            "07d1kq190kgl92ispfx6zmdkvwvhjga0ishxsngzlw8j3kdkz4ap")
           ("Unihan.zip"
            "1kfdhgg2gm52x3s07bijb5cxjy0jxwhd097k5lqhvzpznprm6ibf"))))
    (package
      (name "gucharmap")
      (version "12.0.1")
      (source
       (origin
         (method url-fetch)
         (uri (string-append "mirror://gnome/sources/" name "/"
                             (version-major+minor version) "/"
                             name "-" version ".tar.xz"))
         (sha256
          (base32
           "0m915hm2b2d6r3vs1l80rqpssvg78pv8j6nv54yg62kzknnqmpir"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       `(#:modules ((ice-9 match)
                    (guix build glib-or-gtk-build-system)
                    (guix build utils))
         #:configure-flags
         (list "--with-unicode-data=../unicode-data")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'prepare-unicode-data
             (lambda* (#:key inputs #:allow-other-keys)
               (mkdir-p "../unicode-data")
               (with-directory-excursion "../unicode-data"
                 (for-each (match-lambda
                             ((file _)
                              (install-file (assoc-ref inputs file) ".")))
                           ',unicode-files))
               #t)))))
      (native-inputs
       `(("desktop-file-utils" ,desktop-file-utils)
         ("glib:bin" ,glib "bin")       ; for glib-compile-resources.
         ("gobject-introspection" ,gobject-introspection)
         ("intltool" ,intltool)
         ("itstool" ,itstool)
         ("pkg-config" ,pkg-config)
         ,@(map (match-lambda
                  ((file hash)
                   `(,file
                     ,(origin
                        (method url-fetch)
                        (uri (string-append
                              "http://www.unicode.org/Public/12.0.0/ucd/"
                              file))
                        (sha256 (base32 hash))))))
                unicode-files)
         ("unzip" ,unzip)))
      (inputs
       `(("gtk+" ,gtk+)
         ("xmllint" ,libxml2)))
      (home-page "https://wiki.gnome.org/Apps/Gucharmap")
      (synopsis "Unicode character picker and font browser")
      (description
       "This program allows you to browse through all the available Unicode
characters and categories for the installed fonts, and to examine their
detailed properties.  It is an easy way to find the character you might
only know by its Unicode name or code point.")
      (license license:gpl3+))))

(define-public gcolor3
  (package
    (name "gcolor3")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/gcolor3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1igqmach1vhcrvbpw346pmkbb5kabbb806ssswrvkp569n700wmc"))
       ;; XXX: Remove when upgrading
       (patches (search-patches "gcolor3-update-libportal-usage.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t))
    (native-inputs
     (list desktop-file-utils gettext-minimal
           `(,glib "bin")
           `(,gtk+ "bin") pkg-config))
    (inputs
     (list gsettings-desktop-schemas gtk+ libportal))
    (home-page "https://www.hjdskes.nl/projects/gcolor3/")
    (synopsis "Simple color chooser written in GTK3")
    (description "Color Picker is a simple color chooser written in GTK3.  It
supports both X and Wayland display servers.")
    (license license:gpl2+)))

(define-public bluefish
  (package
    (name "bluefish")
    (version "2.2.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.bennewitz.com/bluefish/stable/source/"
                           "bluefish-" version ".tar.gz"))
       (sha256
        (base32 "0427xihrr7l1475qr3n40hz2xz6bqmfdbdg9pn8q7rvhvajyvjx7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list desktop-file-utils intltool pkg-config))
    (inputs
     (list enchant gtk+ python-wrapper libxml2 gucharmap))
    (home-page "https://bluefish.openoffice.nl")
    (synopsis "Web development studio")
    (description
     "Bluefish is an editor aimed at programmers and web developers,
with many options to write web sites, scripts and other code.
Bluefish supports many programming and markup languages.")
    (license license:gpl3+)))

(define-public gnome-system-monitor
  (package
    (name "gnome-system-monitor")
    (version "46.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "16bmai706vcc373ry51cciap5hg4m71fhwjl4l4c71n6b20j8xjk"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:configure-flags '("-Dsystemd=false")))
    (native-inputs
     (list `(,glib "bin")               ;for glib-mkenums.
           `(,gtk+ "bin")               ;gtk-update-icon-cache
           intltool
           itstool
           libgtop
           polkit
           pkg-config))
    (inputs
     (list gdk-pixbuf                   ;for loading SVG files.
           gtk
           gtkmm
           libadwaita
           (librsvg-for-system)
           libxml2
           libwnck))
    (home-page "https://wiki.gnome.org/Apps/SystemMonitor")
    (synopsis "Process viewer and system resource monitor for GNOME")
    (description
     "GNOME System Monitor is a GNOME process viewer and system monitor with
an attractive, easy-to-use interface.  It has features, such as a tree view
for process dependencies, icons for processes, the ability to hide processes,
graphical time histories of CPU/memory/swap usage and the ability to
kill/reinice processes.")
    (license license:gpl2+)))

(define-public python-pyatspi
  (package
    (name "python-pyatspi")
    (version "2.46.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/pyatspi/"
                    (version-major+minor version)
                    "/pyatspi-" version ".tar.xz"))
              (sha256
               (base32
                "06q4zca83hk4iify8amcb9hfxs3qvlczhjsw7p8hg72f8dbnl7zr"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fix-atk-load
            (lambda _
              (substitute* "pyatspi/__init__.py"
                (("from gi.repository import Atspi")
                 "gi.require_version('Gtk', '3.0')
from gi.repository import Gtk
from gi.repository import Atspi")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list python))
    (propagated-inputs
     (list python-pygobject))
    (synopsis "Python client bindings for D-Bus AT-SPI")
    (home-page "https://wiki.linuxfoundation.org/accessibility\
/atk/at-spi/at-spi_on_d-bus")
    (description
     "This package includes a python client library for the AT-SPI D-Bus
accessibility infrastructure.")
    (license license:lgpl2.0)
    (properties '((upstream-name . "pyatspi")))))

(define-public orca
  (package
    (name "orca")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://gnome/sources/" name "/"
                    (version-major version) "/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "0ppx7svqpjhljf8by3x9xvm46b3gw6f6m7r2gj2k172g3adjjqwg"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))))
         (add-before 'configure 'qualify-programs
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xkbcomp (string-append
                             (assoc-ref inputs "xkbcomp") "/bin/xkbcomp"))
                   (pgrep (string-append
                           (assoc-ref inputs "procps") "/bin/pgrep")))
               (substitute* "src/orca/orca_modifier_manager.py"
                 (("'xkbcomp'") (format #f "'~a'" xkbcomp)))
               (substitute* "src/orca/debug.py"
                 (("'pgrep %s'")
                  (format #f "'~a %s'" pgrep)))
               (substitute* "src/orca/orca_bin.py.in"
                 (("'pgrep -u %s -x orca'")
                  (format #f "'~a -u %s -x orca'" pgrep))))))
         (add-after 'install 'wrap-orca
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (search-input-file outputs "bin/orca")
               `("GI_TYPELIB_PATH" ":" prefix
                 (,(getenv "GI_TYPELIB_PATH")))
               `("GST_PLUGIN_SYSTEM_PATH" ":" prefix
                 (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
               `("GUIX_PYTHONPATH" ":" prefix
                 (,(getenv "GUIX_PYTHONPATH")))))))))
    (native-inputs
     (list gettext-minimal
           itstool
           pkg-config
           libxml2))
    (inputs
     (list at-spi2-core
           brltty
           bash-minimal
           gsettings-desktop-schemas
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gtk+
           liblouis
           `(,liblouis "python")
           procps                       ; for pgrep
           python
           python-pygobject
           python-pyatspi
           speech-dispatcher
           xkbcomp))
    (synopsis
     "Screen reader for individuals who are blind or visually impaired")
    (home-page "https://wiki.gnome.org/Projects/Orca")
    (description
     "Orca is a screen reader that provides access to the graphical desktop
via speech and refreshable braille.  Orca works with applications and toolkits
that support the Assistive Technology Service Provider Interface (AT-SPI).")
    (license license:lgpl2.1+)))

(define-public gspell
  (package
    (name "gspell")
    (version "1.12.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0f91vl42i6fz5yrbw31biffbxqzwa24mw6qbfxmfnk3yhayr7sdl"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     '(#:configure-flags (list "--enable-vala")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-problematic-tests
           (lambda _
             (substitute* "testsuite/test-checker.c"
               ;; This test is known to fail with Aspell, as a comment
               ;; mentions it.  Disable it.
               ((".*g_test_add_func.*test_dashes.*") ""))))
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1")

             ;; For the missing /etc/machine-id.
             (setenv "DBUS_FATAL_WARNINGS" "0")

             ;; Allow Enchant and its Aspell backend to find the en_US
             ;; dictionary.
             (setenv "ASPELL_DICT_DIR"
                     (search-input-directory inputs "/lib/aspell")))))))
    (inputs
     (list iso-codes/pinned))
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           pkg-config
           vala                         ;for VAPI, needed by Geary
           libxml2

           ;; For tests.
           aspell-dict-en
           xorg-server-for-tests))
    (propagated-inputs
     ;; Referred by .pc file.
     (list enchant
           glib
           gtk+))
    (home-page "https://wiki.gnome.org/Projects/gspell")
    (synopsis "GNOME's alternative spell checker")
    (description
     "gspell provides a flexible API to add spell-checking to a GTK+
application.  It provides a GObject API, spell-checking to text entries and
text views, and buttons to choose the language.")
    (license license:gpl2+)))

(define-public gnome-planner
  (package
    (name "gnome-planner")
    (version "0.14.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/planner/"
                                  (version-major+minor version) "/planner-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "15h6ps58giy5r1g66sg1l4xzhjssl362mfny2x09khdqsvk2j38k"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; Disable the Python bindings because the Planner program functions
     ;; without them, and (as of 2017-06-13) we have not packaged all of
     ;; packages that are necessary for building the Python bindings.
     `(#:configure-flags
       (list "--disable-python"
             ,@(if (string=? "aarch64-linux" (%current-system))
                   '("--build=aarch64-unknown-linux-gnu")
                   '()))))
    (inputs
     (list libgnomecanvas
           libgnomeui
           libglade
           gnome-vfs
           gconf
           libxml2
           libxslt
           gtk+
           glib))
    (native-inputs
     (list intltool scrollkeeper pkg-config))
    (home-page "https://wiki.gnome.org/Apps/Planner")
    (synopsis "Project management software for the GNOME desktop")
    (description
     "GNOME Planner is a project management tool based on the Work Breakdown
Structure (WBS).  Its goal is to enable you to easily plan projects.  Based on
the resources, tasks, and constraints that you define, Planner generates
various views into a project.  For example, Planner can show a Gantt chart of
the project.  It can show a detailed summary of tasks including their
duration, cost, and current progress.  It can also show a report of resource
utilization that highlights under-utilized and over-utilized resources.  These
views can be printed as PDF or PostScript files, or exported to HTML.")
    (license license:gpl2+)))

(define-public lollypop
  (package
    (name "lollypop")
    (version "1.4.40")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://adishatz.org/lollypop/"
                           "lollypop-" version ".tar.xz"))
       (sha256
        (base32 "1laj5xwfz2bz29scga2ahhnhlgll4a0n21wwy8mlr4jsl81g0jsa"))))
    (build-system meson-build-system)
    (arguments
     (list #:imported-modules `(,@%meson-build-system-modules
                                (guix build python-build-system))
           #:modules '((guix build meson-build-system)
                       ((guix build python-build-system) #:prefix python:)
                       (guix build utils))
           #:glib-or-gtk? #t
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-gtk-update-icon-cache
                 (lambda _
                   (setenv "DESTDIR" "/")))
               (add-after 'install 'wrap-program
                 (lambda* (#:key outputs #:allow-other-keys)
                   (wrap-program (search-input-file outputs "bin/lollypop")
                     (list "GI_TYPELIB_PATH" ":" 'prefix
                           (list (getenv "GI_TYPELIB_PATH"))))))
               (add-after 'install 'wrap-python
                 (assoc-ref python:%standard-phases 'wrap)))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; For glib-compile-resources
           pkg-config))
    (inputs
     (list bash-minimal
           glib-networking
           gobject-introspection
           gsettings-desktop-schemas
           gst-plugins-base
           libnotify
           libsecret
           libhandy
           libsoup-minimal-2
           python
           python-beautifulsoup4
           python-gst
           python-pillow
           python-pycairo
           python-pygobject
           python-pylast
           totem-pl-parser
           webkitgtk-for-gtk3))
    (propagated-inputs
     (list gst-plugins-good             ;required to start lollypop
           gst-plugins-ugly))           ;required for streaming
    (home-page "https://wiki.gnome.org/Apps/Lollypop")
    (synopsis "GNOME music playing application")
    (description
     "Lollypop is a music player designed to play well with GNOME desktop.
Lollypop plays audio formats such as mp3, mp4, ogg and flac and gets information
from artists and tracks from the web.  It also fetches cover artworks
automatically and it can stream songs from online music services and charts.")
    (license license:gpl3+)))

(define-public gnome-video-effects
  (package
    (name "gnome-video-effects")
    (version "0.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1j6h98whgkcxrh30bwvnxvyqxrxchgpdgqhl0j71xz7x72dqxijd"))))
    (build-system meson-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (home-page "https://wiki.gnome.org/Projects/GnomeVideoEffects")
    (synopsis "Video effects for Cheese and other GNOME applications")
    (description
     "A collection of GStreamer video filters and effects to be used in
photo-booth-like software, such as Cheese.")
    (license license:gpl2+)))

(define-public cheese
  (package
    (name "cheese")
    (version "44.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/" name "-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "13pnz35yvwvmk1iyhcp1a94yal4rh610rxmsp3rdsm4yr728a8az"))))
    (arguments
     (list #:glib-or-gtk? #t
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-gtk-update-icon-cache
                 (lambda _
                   ;; Don't create 'icon-theme.cache'.
                   (substitute* "meson.build"
                     (("gtk_update_icon_cache: true")
                      "gtk_update_icon_cache: false"))))
               (add-after 'install 'wrap-cheese
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (wrap-program (search-input-file outputs "bin/cheese")
                     `("GST_PLUGIN_SYSTEM_PATH" prefix
                       (,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                     `("GST_PRESET_PATH" prefix
                       (,(dirname (search-input-file inputs
                                                     "share/gstreamer-1.0\
/presets/GstVP8Enc.prs"))))))))))
    (build-system meson-build-system)
    (native-inputs
     (list docbook-xml-4.3
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           itstool
           libxml2
           libxslt
           pkg-config
           vala))
    (propagated-inputs
     (list clutter
           clutter-gst
           clutter-gtk
           gdk-pixbuf
           glib
           gnome-video-effects
           gstreamer
           libcanberra))
    (inputs
     (list bash-minimal
           gnome-desktop
           gst-plugins-bad
           gst-plugins-base
           gst-plugins-good
           gtk+
           libx11
           libxtst))
    (home-page "https://wiki.gnome.org/Apps/Cheese")
    (synopsis "Webcam photo booth software for GNOME")
    (description
     "Cheese uses your webcam to take photos and videos.  Cheese can also
apply fancy special effects and lets you share the fun with others.")
    (license license:gpl2+)))

(define-public secrets
  (package
    (name "secrets")
    (version "6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/secrets")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11jd9f0d3fyrs29p8cyzb6i2ib6mzhwwvjnznl55gkggrgnrcb8z"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-postinstall-script
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))
              (setenv "DESTDIR" "/")))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/secrets")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                       ,(python:site-packages inputs outputs)))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config))
    (inputs
     (list bash-minimal
           glib
           gsettings-desktop-schemas
           gtk
           libadwaita
           libhandy
           libpwquality
           python
           python-pygobject
           python-pykeepass
           python-pyotp))
    (home-page "https://gitlab.gnome.org/World/secrets")
    (synopsis "Password manager for the GNOME desktop")
    (description
     "Secrets is a password manager which makes use of the KeePass v4
format.  It integrates perfectly with the GNOME desktop and provides an easy
and uncluttered interface for the management of password databases.")
    (license license:gpl3+)))

(define-public passwordsafe
  (deprecated-package "passwordsafe" secrets))

(define-public sound-juicer
  (package
    (name "sound-juicer")
    (version "3.40.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1rhxmvx2mr22zd5p0azc0svi0mbnzcjnh3sasv3b9gli8ds85s1f"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-after 'install 'wrap-program
            (lambda _
              (let ((prog (string-append #$output "/bin/sound-juicer"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH")))
                (wrap-program prog
                  `("GST_PLUGIN_SYSTEM_PATH"
                    ":" prefix (,gst-plugin-path)))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           itstool
           libxml2
           pkg-config
           python))
    (inputs
     (list bash-minimal
           brasero
           gsettings-desktop-schemas
           gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk+
           iso-codes/pinned
           libcanberra
           libdiscid
           libmusicbrainz
           neon))
    (home-page "https://wiki.gnome.org/Apps/SoundJuicer")
    (synopsis "Audio music cd ripper")
    (description "Sound Juicer extracts audio from compact discs and convert it
into audio files that a personal computer or digital audio player can play.
It supports ripping to any audio codec supported by a GStreamer plugin, such as
mp3, Ogg Vorbis and FLAC")
    (license license:gpl2+)))

(define-public soundconverter
  (package
    (name "soundconverter")
    (version "3.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://launchpad.net/soundconverter/trunk/"
                           version "/+download/"
                           "soundconverter-" version ".tar.xz"))

       (sha256
        (base32 "1jv8m82hi23ilrgdznlc1jhp2jm8bw1yrw0chh3qw2l0sixvkl11"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           (guix build glib-or-gtk-build-system)
                           ,@%default-gnu-imported-modules)

       #:modules ((guix build glib-or-gtk-build-system)
                  (guix build utils)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  ((guix build python-build-system) #:prefix python:))

       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-soundconverter-for-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'install 'wrap-soundconverter
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (gst-plugin-path   (getenv "GST_PLUGIN_SYSTEM_PATH")))
               (wrap-program (string-append out "/bin/soundconverter")
                 `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))))
             #t)))))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("glib:bin" ,glib "bin")))
    (inputs
     (list bash-minimal
           gtk+
           python
           python-pygobject
           gstreamer
           gst-plugins-base))
    (home-page "https://soundconverter.org/")
    (synopsis "Convert between audio formats with a graphical interface")
    (description
     "SoundConverter supports converting between many audio formats including
Opus, Ogg Vorbis, FLAC and more.  It supports parallel conversion, and
configurable file renaming.")
    (license license:gpl3)))

(define-public workrave
  (package
    (name "workrave")
    (version "1.10.52")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcaelers/workrave")
             (commit (string-append "v" (string-map
                                         (match-lambda (#\. #\_) (chr chr))
                                         version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rp6v9a8xmhjy75wmh7pnd092dn9nrb6wd4gcgr3c866qnpp6zsk"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     ;; The only tests are maintainer tests (in po/), which fail.
     `(#:tests? #f))
    (inputs (list glib
                  gtk+
                  gdk-pixbuf
                  gtkmm-3
                  glibmm
                  libx11
                  libxtst
                  dconf
                  libice
                  libsm
                  libxscrnsaver))
    (native-inputs (list boost
                         pkg-config
                         gettext-minimal
                         autoconf
                         autoconf-archive
                         automake
                         libtool
                         intltool
                         gobject-introspection
                         python-3
                         python-jinja2))
    (synopsis "Tool to help prevent repetitive strain injury (RSI)")
    (description
     "Workrave is a program that assists in the recovery and prevention of
repetitive strain injury (@dfn{RSI}).  The program frequently alerts you to take
micro-pauses and rest breaks, and restricts you to your daily limit.")
    (home-page "https://www.workrave.org")
    (license license:gpl3+)))

(define-public ghex
  (package
    (name "ghex")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/ghex/"
                                  (version-major version) "/"
                                  "ghex-" version ".tar.xz"))
              (sha256
               (base32
                "0c8zcsng3925sw3bxffyj4lczna389k7rzv2p0h0v9wpcfipdwm8"))))
    (build-system meson-build-system)
    (arguments
     '(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false")))))))
    (native-inputs
     (list desktop-file-utils           ;for 'desktop-file-validate'
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-schemas
           gnome-common
           pkg-config
           yelp-tools))
    (inputs
     (list at-spi2-core
           gtk
           libadwaita))
    (synopsis "GNOME hexadecimal editor")
    (description "The GHex program can view and edit files in two ways:
hexadecimal or ASCII.  It is useful for editing binary files in general.")
    (home-page "https://wiki.gnome.org/Apps/Ghex")
    (license license:gpl2)))

(define-public libdazzle
  (package
    (name "libdazzle")
    (version "3.44.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libdazzle/"
                                  (version-major+minor version) "/"
                                  "libdazzle-" version ".tar.xz"))
              (sha256
               (base32
                "1blfs61ifv4fywl0wbr1cm3rvmgrv06yiqajbnq0qs72nrgf9lrw"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list `(,glib "bin") ; glib-compile-resources
           gobject-introspection
           pkg-config
           ;; For tests.
           xorg-server-for-tests
           vala))
    (inputs
     (list glib
           gtk+))
    (home-page "https://gitlab.gnome.org/GNOME/libdazzle")
    (synopsis "Companion library to GObject and Gtk+")
    (description "The libdazzle library is a companion library to GObject and
Gtk+.  It provides various features that the authors wish were in the
underlying library but cannot for various reasons.  In most cases, they are
wildly out of scope for those libraries.  In other cases, they are not quite
generic enough to work for everyone.")
    (license license:gpl3+)))

(define-public evolution
  (package
    (name "evolution")
    (version "3.54.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/evolution/"
                                  (version-major+minor version) "/"
                                  "evolution-" version ".tar.xz"))
              (sha256
               (base32
                "1ssmp13finaa9ncxh5507nqz6q3jsr3anbrvylbsy9f3ylgghv3l"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules `(,@%cmake-build-system-modules
                           (guix build glib-or-gtk-build-system))
      #:modules '((guix build cmake-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
      #:configure-flags
      #~(list "-DENABLE_PST_IMPORT=OFF") ;libpst is not packaged
      #:phases
      #~(modify-phases %standard-phases
          ;; The build system attempts to install user interface modules to
          ;; the output directory of the "evolution-data-server" package;
          ;; patch it to install to the same location under #$output prefix.
          (add-after 'unpack 'patch-ui-module-dir
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "src/modules/alarm-notify/CMakeLists.txt"
                (("\\$\\{edsuimoduledir\\}")
                 (string-append
                  #$output "/lib/evolution-data-server/ui-modules")))
              (substitute* "src/modules/rss/camel/CMakeLists.txt"
                (("\\$\\{camel_providerdir}")
                 (string-append
                  #$output "/lib/evolution-data-server/camel-providers")))))
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list `(,glib "bin")               ;glib-mkenums
           intltool
           itstool
           pkg-config))
    (inputs
     (list cmark
           enchant
           evolution-data-server        ;must be the same version
           gcr-3
           gsettings-desktop-schemas
           gnome-autoar
           gnome-desktop
           gspell
           highlight
           libcanberra
           libgweather4
           libnotify
           libsoup
           nss
           openldap
           webkitgtk-for-gtk3
           ytnef))
    (home-page "https://gitlab.gnome.org/GNOME/evolution")
    (synopsis "Manage your email, contacts and schedule")
    (description "Evolution is a personal information management application
that provides integrated mail, calendaring and address book
functionality.")
    ;; See COPYING for details.
    (license (list license:lgpl2.1 license:lgpl3 ; either one of these
                   license:openldap2.8 ; addressbook/gui/component/openldap-extract.h
                   license:lgpl2.1+))))  ; smime/lib/*

(define-public gthumb
  (package
    (name "gthumb")
    (version "3.12.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/gthumb/"
                                  (version-major+minor version) "/"
                                  "gthumb-" version ".tar.xz"))
              (sha256
               (base32
                "1s4lqy883s296mbh4fywd1l3z79811ia00xs57c316pb1an97mmd"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      ;; Ensure the RUNPATH contains all installed library locations.
      #~(list (string-append "-Dc_link_args=-Wl,-rpath=" #$output
                             "/lib/gthumb/extensions")
              (string-append "-Dcpp_link_args=-Wl,-rpath=" #$output
                             "/lib/gthumb/extensions"))))
    (native-inputs
     (list desktop-file-utils   ; for update-desktop-database
           `(,glib "bin")       ; for glib-compile-resources
           `(,gtk+ "bin")       ; for gtk-update-icon-cache
           intltool
           itstool
           pkg-config
           python))
    (inputs
     (list clutter
           clutter-gst
           clutter-gtk
           colord
           exiv2
           gsettings-desktop-schemas
           gstreamer
           gtk+
           libheif
           libjpeg-turbo
           libraw
           (librsvg-for-system)
           libtiff
           libwebp))
    (home-page "https://wiki.gnome.org/Apps/Gthumb")
    (synopsis "GNOME image viewer and browser")
    (description "GThumb is an image viewer, browser, organizer, editor and
advanced image management tool")
    (license license:gpl2+)))

(define-public terminator
  (package
    (name "terminator")
    (version "2.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/gnome-terminator/terminator/"
                           "releases/download/v" version "/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "1s65y2yjrigbvqzgxvwr8pj199199bx7m0nhf7g1vrk2x3nb09xg"))))
    (build-system python-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")         ; for glib-compile-resources
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python-psutil" ,python-psutil)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-pytest" ,python-pytest)))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("cairo" ,cairo)
       ("dbus-glib" ,dbus-glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gtk+" ,gtk+)
       ("python" ,python-wrapper)
       ("python-dbus" ,python-dbus)
       ("python-notify2" ,python-notify2)
       ("python-pycairo" ,python-pycairo)
       ("python-pygobject" ,python-pygobject)
       ("vte" ,vte/gtk+-3)))
    (propagated-inputs
     (list python-configobj))
    (arguments
     ;; One test out of 28 fails due to dbus-python and python-notify; skip
     ;; tests.
     `(#:tests? #f
       #:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'handle-dbus-python
           (lambda _
             ;; python-dbus cannot be found but it's really there.  See
             ;; https://github.com/SpotlightKid/jack-select/issues/2
             (substitute* "setup.py"
               (("'dbus-python',") ""))))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((prog (string-append (assoc-ref outputs "out")
                                        "/bin/terminator"))
                   (pylib (string-append (assoc-ref outputs "out")
                                         "/lib/python"
                                         ,(version-major+minor
                                           (package-version python))
                                         "/site-packages")))
               (wrap-program prog
                 `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                 `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))
         (add-after 'wrap-program 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (home-page "https://gnome-terminator.org/")
    (synopsis "Store and run multiple GNOME terminals in one window")
    (description
     "Terminator allows you to run multiple GNOME terminals in a grid and
tabs, and it supports drag and drop re-ordering of terminals.")
    (license license:gpl2)))

(define-public libhandy
  (package
    (name "libhandy")
    (version "1.8.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libhandy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p80py59mg9hjk2pzp0595cv64ankaqdvqsxlhrsgzsfx940r9nc"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dglade_catalog=enabled"
                   ;; XXX: Generating the documentation fails because the
                   ;; libhandy.devhelp2 document cannot be created. This seems
                   ;; to be caused by a problem during the XSL transformation.
                   "-Dgtk_doc=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'pre-check
                 (lambda _
                   ;; Tests require a running X server.
                   (system "Xvfb :1 &")
                   (setenv "DISPLAY" ":1"))))))
    (inputs (list gtk+ glade3))
    (native-inputs
     (list gobject-introspection        ; for g-ir-scanner
           `(,glib "bin")
           vala
           libxml2
           libxslt
           docbook-xsl
           docbook-xml-4.3
           gtk-doc/stable
           pkg-config
           gettext-minimal

           ;; Test suite dependencies.
           hicolor-icon-theme
           xorg-server-for-tests))
    (home-page "https://gitlab.gnome.org/GNOME/libhandy/")
    (synopsis "Library full of GTK+ widgets for mobile phones")
    (description "The aim of the handy library is to help with developing user
interfaces for mobile devices using GTK+.  It provides responsive GTK+ widgets
for usage on small and big screens.")
    (license license:lgpl2.1+)))

(define-public libhandy-0.0
  (package
    (inherit libhandy)
    (version "0.0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/libhandy")
             (commit (string-append "v" version))))
       (file-name (git-file-name "libhandy" version))
       (sha256
        (base32 "1y23k623sjkldfrdiwfarpchg5mg58smcy1pkgnwfwca15wm1ra5"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libhandy)
       ((#:configure-flags flags)
        '(list "-Dglade_catalog=disabled" "-Dgtk_doc=true"))))))

(define-public libgit2-glib
  (package
    (name "libgit2-glib")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1441w7gzn6m3adhx4g6wcbkmscka2929si963dciyklghgddlc8k"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ;; For glib-mkenums
           gobject-introspection
           pkg-config
           python-pygobject
           python-wrapper
           vala))
    (inputs
     (list libssh2))
    (propagated-inputs
     (list glib libgit2)) ;; In Requires of libgit2-glib.pc.
    (synopsis "GLib wrapper around the libgit2 Git access library")
    (description "libgit2-glib is a GLib wrapper library around the libgit2 Git
access library.  It only implements the core plumbing functions, not really the
higher level porcelain stuff.")
    (home-page "https://wiki.gnome.org/Projects/Libgit2-glib")
    (license license:gpl2+)))

(define-public gitg
  (package
    (name "gitg")
    (version "44")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0c152c1vrkckqkfq3862c02fxp2scv7f7lqv6k6p35mb9ml32ail"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:build-type "release"            ; don't look at -Wformat…
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-post-install-partially
            (lambda _
              (substitute* "meson_post_install.py"
                (("'python'") ; there are no python sources to compile
                 (string-append "'" (which "true") "'"))
                (("gtk-update-icon-cache") (which "true")))))
          (add-after 'unpack 'fix-test-sources
            (lambda _
              (substitute* "tests/libgitg/test-commit.vala"
                (("/bin/bash") (which "bash")))))
          (add-after 'glib-or-gtk-wrap 'wrap-typelib
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((prog (string-append #$output "/bin/gitg")))
                (wrap-program prog
                  `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (inputs
     (list bash-minimal
           glib
           gpgme
           gsettings-desktop-schemas
           gspell
           gtk+
           gtksourceview-4
           json-glib
           libdazzle
           libgee
           libgit2-glib
           libhandy
           libpeas
           libsecret
           libsoup-minimal-2
           libxml2))
    (native-inputs
     (list `(,glib "bin")
           `(,gtk+ "bin")
           gobject-introspection
           intltool
           pkg-config
           python
           vala))
    (synopsis "Graphical user interface for git")
    (description
     "gitg is a graphical user interface for git.  It aims at being a small,
fast and convenient tool to visualize the history of git repositories.
Besides visualization, gitg also provides several utilities to manage your
repository and commit your work.")
    (home-page "https://wiki.gnome.org/Apps/Gitg")
    (license license:gpl2+)))

(define-public gamin
  (package
    (name "gamin")
    (version "0.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.bz2"))
              (sha256
               (base32
                "02n1zr9y8q9lyczhcz0nxar1vmf8p2mmbw8kq0v43wg21jr4i6d5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The 'config.sub' is too old to recognise aarch64.
         ,@(if (or (target-aarch64?) (target-riscv64?))
               `((add-after 'unpack 'replace-config.sub
                   (lambda _
                     (delete-file "config.sub")
                     (symlink (which "config.sub") "config.sub"))))
               '())
         (add-after 'unpack 'remove-deprecated-macro
           (lambda _
             (substitute* '("server/gam_node.c"
                            "server/gam_subscription.h"
                            "server/gam_node.h"
                            "server/gam_subscription.c")
               (("G_CONST_RETURN") "const"))
             #t))
         ;; The configure script runs a test program unconditionally,
         ;; without an option to manually set the test result.
         ;; Override this test anyway.
         ,@(if (%current-target-system)
               `((add-after 'bootstrap 'set-have-abstract-sockets
                   (lambda _
                     (define in-abstract-sockets-test? #f)
                     (substitute* "configure"
                       (("^#### Abstract sockets\n$")
                        (set! in-abstract-sockets-test? #t)
                        "#### Abstract sockets\n")
                       (("^have_abstract_sockets=no\n$")
                        (set! in-abstract-sockets-test? #f)
                        ;; ‘Abstract sockets’ appear to be Linux-only.
                        (string-append "have_abstract_sockets="
                                       ,(if (target-linux?)
                                            "yes"
                                            "no")
                                       "\nif false; then\nif false; then :\n"))
                       (("^(.*\n)$" line)
                        (if in-abstract-sockets-test?
                            "" ; delete
                            line))))))
               '()))))
    (inputs
     (list glib))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ,@(if (or (target-aarch64?) (target-riscv64?))
             `(("config" ,config))
             '())))
    (home-page "https://people.gnome.org/~veillard/gamin/")
    (synopsis "File alteration monitor")
    (description
     "Gamin is a file and directory monitoring system defined to be a subset
of the FAM (File Alteration Monitor) system.  This is a service provided by a
library which detects when a file or a directory has been modified.")
    (license license:gpl2+)))

(define-public gnome-mahjongg
  (package
    (name "gnome-mahjongg")
    (version "3.40.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/gnome-mahjongg")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mc9379lmkcn08gr1wdny8gdwgdadkv11vxmgsiazcdy8bsj5860"))))
    (build-system meson-build-system)
    (arguments (list #:glib-or-gtk? #t))
    (native-inputs
     (list appstream-glib
           gettext-minimal
           `(,glib "bin")             ;for glib-compile-resources
           `(,gtk "bin")              ;for gtk-update-icon-cache
           itstool
           pkg-config
           vala))
    (propagated-inputs
     (list dconf))
    (inputs
     (list glib
           gtk
           libadwaita))
    (synopsis "Mahjongg tile-matching game")
    (description "GNOME Mahjongg is a game based on the classic Chinese
tile-matching game Mahjongg.  It features multiple board layouts, tile themes,
and a high score table.")
    (home-page "https://wiki.gnome.org/Apps/Mahjongg")
    (license license:gpl2+)))

(define-public gnome-themes-extra
  (package
    (name "gnome-themes-extra")
    (version "3.28")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/" name "-"
                           version ".tar.xz"))
       (sha256
        (base32
         "06aqg9asq2vqi9wr29bs4v8z2bf4manhbhfghf4nvw01y2zs0jvw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       ;; Don't create 'icon-theme.cache'.
       (let* ((coreutils (assoc-ref %build-inputs "coreutils"))
              (true      (string-append coreutils "/bin/true")))
         (list (string-append "GTK_UPDATE_ICON_CACHE=" true)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("librsvg" ,(librsvg-for-system))
       ("libxml2" ,libxml2)))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-themes-extra")
    (synopsis "GNOME Extra Themes")
    (description "This package provides themes and related elements that don't
really fit in other upstream packages.  It offers legacy support for GTK+ 2
versions of Adwaita, Adwaita-dark and HighContrast themes.  It also provides
index files needed for Adwaita to be used outside of GNOME.")
    (license license:lgpl2.1+)))

(define-public gnome-themes-standard
  (deprecated-package "gnome-themes-standard" gnome-themes-extra))

(define-public gnote
  (package
    (name "gnote")
    (version "42.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major version)  "/"
                           "gnote-" version ".tar.xz"))
       (sha256
        (base32 "0fam3v9na4ndqdc63866bvhcxrzj478jsx34vsh0777d4ixw883c"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'skip-gtk-update-icon-cache
                    ;; Don't create 'icon-theme.cache'.
                    (lambda _
                      (substitute* "post-install.py"
                        (("gtk-update-icon-cache") "true")))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           itstool
           pkg-config
           python))
    (inputs
     (list glibmm
           gsettings-desktop-schemas
           gspell
           gtk+
           gtkmm-3
           libsecret
           `(,util-linux "lib")
           libxml2
           libxslt))
    (synopsis "Note-taking application for the GNOME desktop")
    (description "Gnote is a note-taking application written for the GNOME
desktop environment.")
    (home-page "https://wiki.gnome.org/Apps/Gnote")
    (license license:gpl3+)))

(define-public polari
  (package
    (name "polari")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/polari/"
                                  (version-major version)
                                  "/polari-" version ".tar.xz"))
              (sha256
               (base32
                "0c8a6q6g1mgpc9g423rgqplbpjwb7zq1bvylad7jk2ci6yg71cfj"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))))
         (add-after 'install 'fix-desktop-file
           ;; Hard-code launcher to be on the safe side.
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* (search-input-file
                           outputs
                           "share/applications/org.gnome.Polari.desktop")
               (("Exec=.*")
                (string-append "Exec=" (search-input-file outputs "bin/polari")
                               "\n")))))
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (search-input-file outputs "bin/polari")
               `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config
           yelp-tools))
    (inputs
     (list bash-minimal
           glib
           gsettings-desktop-schemas
           gspell
           gtk
           gjs
           libadwaita
           libsecret
           libsoup
           telepathy-glib
           telepathy-logger
           tracker))
    (propagated-inputs
     (list telepathy-idle
           telepathy-mission-control))
    (synopsis "Simple IRC Client")
    (description
     "Polari is a simple Internet Relay Chat (IRC) client that is designed to
integrate seamlessly with the GNOME desktop.")
    (home-page "https://wiki.gnome.org/Apps/Polari")
    (license license:gpl2+)))

(define-public gnome-boxes
  (package
    (name "gnome-boxes")
    (version "48.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/gnome-boxes/"
                           (version-major version) "/"
                           "gnome-boxes-" version ".tar.xz"))
       (sha256
        (base32 "1b9ya5pcb5dfii0qs9r167a3kxymdsq624bpi1nvzbwgar15ypyh"))))
    (outputs '("out" "debug"))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'disable-gtk-update-icon-cache
                          (lambda _
                            (substitute* "meson.build"
                              (("gtk_update_icon_cache: true")
                               "gtk_update_icon_cache: false"))))
                        (add-before 'configure 'set-qemu-file-name
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "src/installed-media.vala"
                              (("qemu-img")
                               (search-input-file inputs
                                                  "/bin/qemu-img"))))))))
    (native-inputs
     (list desktop-file-utils           ;for update-desktop-database
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-resources
           itstool
           pkg-config
           python-minimal
           vala))
    (inputs
     (list glib-networking              ;for TLS support
           gsettings-desktop-schemas
           gtk
           gtksourceview-4
           json-glib
           libarchive
           libgudev
           libhandy
           libosinfo
           libportal
           libsecret
           libsoup
           libusb
           libvirt
           libvirt-glib
           libxml2
           qemu-minimal                 ;for qemu-img
           sparql-query
           tracker
           webkitgtk-for-gtk3))
    (propagated-inputs
     ;; Propagating spice-gtk is necessary so that the gnome-desktop-service
     ;; type configures the polkit actions necessary for the USB redirection
     ;; feature to work when gnome-boxes added as a extra GNOME package.
     (list spice-gtk))
    (home-page "https://wiki.gnome.org/Apps/Boxes")
    (synopsis "View, access, and manage remote and virtual systems")
    (description "GNOME Boxes is a simple application to view, access, and
manage remote and virtual systems.  Note that this application requires the
@code{libvirt} and @code{virtlog} daemons to run.  Use the command
@command{info '(guix) Virtualization Services'} to learn how to configure
these services on the Guix System.

To make it possible to redirect USB devices as a non-privileged user, some
extra configuration is necessary: if you use the
@code{gnome-desktop-service-type}, you should add the @code{gnome-boxes}
package to the @code{extra-packages} field of the
@code{gnome-desktop-configuration}, for example:
@lisp
(service gnome-desktop-service-type
         (gnome-desktop-configuration
          (extra-packages (list gnome-boxes gnome-essential-extras))))
@end lisp
If you do @emph{not} use the @code{gnome-desktop-service-type}, you will need
manually extend the @code{polkit-service-type} with the @code{spice-gtk}
package, as well as configure the
@file{libexec/spice-client-glib-usb-acl-helper} executable of @code{spice-gtk}
as setuid, to make it possible to redirect USB devices as a non-privileged
user.")
    (license (list
              ;; For data/icons/empty-boxes.png.
              license:cc-by2.0
              ;; For all others.
              license:lgpl2.0+))))

(define-public geary
  (package
    (name "geary")
    (version "44.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/geary.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cx1jfaxsbkxm8774wf8n7ss2n73bzgk4yi2f9i3ab698ygh0h68"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:configure-flags
           #~(list "-Dprofile=release")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-gtk-update-icon-cache
                 ;; Don't create 'icon-theme.cache'.
                 (lambda _
                   (substitute* "meson.build"
                     (("gtk_update_icon_cache: true")
                      "gtk_update_icon_cache: false"))))
               (add-before 'check 'setup-home
                 (lambda _
                   ;; Tests require a writable HOME.
                   (setenv "HOME" (getcwd))))
               (add-before 'check 'setup-xvfb
                 (lambda _
                   (system "Xvfb :1 &")
                   (setenv "DISPLAY" ":1"))))))
    (inputs
     (list enchant
           folks
           gcr-3
           glib
           gmime
           gnome-online-accounts
           gsettings-desktop-schemas
           gspell
           gsound
           gtk+
           iso-codes/pinned
           json-glib
           libcanberra
           libgee
           libhandy
           libpeas
           libsecret
           libstemmer
           libunwind
           sqlite
           webkitgtk-for-gtk3
           ytnef))
    (native-inputs
     (list appstream-glib
           cmake-minimal
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gnutls                       ; for certtool
           gobject-introspection
           itstool
           libarchive
           (libc-utf8-locales-for-target)
           libxml2
           pkg-config
           python-minimal
           vala
           xorg-server-for-tests))
    (synopsis "GNOME email application built around conversations")
    (description
     "Geary collects related messages together into conversations,
making it easy to find and follow your discussions.  Full-text and keyword
search makes it easy to find the email you are looking for.  Geary's
full-featured composer lets you send rich, styled text with images, links, and
lists, but also send lightweight, easy to read text messages.  Geary
automatically picks up your existing GNOME Online Accounts, and adding more is
easy.  Geary has a clean, fast, modern interface that works like you want it
to.")
    (home-page "https://wiki.gnome.org/Apps/Geary")
    (license (list
              ;; geary
              license:lgpl2.1+
              ;; icons
              license:cc-by3.0
              license:cc-by-sa3.0
              license:public-domain
              ;; snowball
              license:bsd-2))))

(define-public glabels
  (package
    (name "glabels")
    (version "3.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "glabels-" version ".tar.xz"))
       (sha256
        (base32 "0f2rki8i27pkd9r0gz03cdl1g4vnmvp0j49nhxqn275vi8lmgr0q"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list gtk+ (librsvg-for-system) libxml2))
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")))
    (home-page "https://glabels.org/")
    (synopsis "Program for creating labels and business cards")
    (description
     "gLabels is a program for creating labels and business cards.  It is
designed to work with various laser/ink-jet peel-off label and business
card sheets that you’ll find at most office supply stores.")
    (license license:gpl3+)))

(define-public gnome-latex
  (package
    (name "gnome-latex")
    (version "3.44.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version)  "/"
                           "gnome-latex-" version ".tar.xz"))
       (sha256
        (base32 "0i77m431ilbaprcwcnnzfckr1g9bfc03lslnqw0yvir8pm057gc8"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           itstool
           pkg-config
           vala))
    (inputs
     (list dconf
           glib
           gspell
           libgee
           tepl
           uchardet))
    (home-page "https://wiki.gnome.org/Apps/GNOME-LaTeX")
    (synopsis "LaTeX editor for the GNOME desktop")
    (description
     "GNOME LaTeX is a LaTeX editor for the GNOME desktop.  It has features
such as build tools, completion of LaTeX commands, structure navigation,
symbol tables, document templates, project management, spell-checking, menus
and toolbars.")
    (license license:gpl3+)))

(define-public setzer
  (package
    (name "setzer")
    (version "0.4.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cvfosammmm/Setzer")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12w58v7qsd3xfmrxhij8dby9xnvd82hxqb4wc6di7lqz1ayg5lzc"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/setzer")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                       ,(python:site-packages inputs outputs)))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list gettext-minimal
           python))
    (inputs
     (list bash-minimal
           gsettings-desktop-schemas
           gspell
           gtk+
           gtksourceview-4
           pango
           poppler
           python-pdfminer-six
           python-pexpect
           python-pycairo
           python-pygobject
           python-pyxdg
           webkitgtk-with-libsoup2
           xdg-utils))
    (home-page "https://www.cvfosammmm.org/setzer/")
    (synopsis "LaTeX editor written in Python with GTK+")
    (description
     "Setzer is a simple yet full-featured LaTeX editor written in Python with
GTK+.  It integrates well with the GNOME desktop environment.")
    (license license:gpl3+)))

(define-public apostrophe
  (package
    (name "apostrophe")
    (version "2.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/apostrophe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wsvq2434p650cf3vq5w7a6czbk8in0ra7nji45mvwyfahdyn6j4"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build python-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-meson
            (lambda _
              (substitute* "build-aux/meson_post_install.py"
                (("gtk-update-icon-cache") "true"))))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/apostrophe")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                       ,(python:site-packages inputs outputs)))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                `("PATH" prefix (,(dirname
                                   (search-input-file inputs
                                                      "/bin/pandoc"))))))))))
    (inputs
     (list bash-minimal
           glib
           gobject-introspection
           gspell
           gtk+
           libhandy
           pandoc
           python
           python-chardet
           python-levenshtein
           python-regex
           python-pycairo
           python-pygobject
           python-pyenchant
           python-pypandoc
           webkitgtk-with-libsoup2))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config
           sassc))
    (home-page "https://gitlab.gnome.org/World/apostrophe")
    (synopsis "Markdown editor written in Python with GTK+")
    (description "Apostrophe is a GTK+ based distraction-free Markdown editor.
It uses pandoc as back-end for parsing Markdown.")
    (license license:gpl3)))

(define-public libratbag
  (package
    (name "libratbag")
    (version "0.18")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libratbag/libratbag")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09rmzbvh3q996r5vcdiirr56xzzwi5njay26hp50nyk1bq68l1bl"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dsystemd=false"
             "-Dlogind-provider=elogind"
             ,@(if (not (package? (this-package-native-input "valgrind")))
                 `("-Dtests=false")     ; Some tests still run.
                 `()))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (site (string-append
                           "/lib/python"
                           ,(version-major+minor (package-version python))
                           "/site-packages"))
                    (evdev (string-append
                            (assoc-ref inputs "python-evdev") site))
                    (pygo (string-append
                           (assoc-ref inputs "python-pygobject") site))
                    (python-wrap
                     `("GUIX_PYTHONPATH" = (,evdev ,pygo))))
               (wrap-program (string-append out "/bin/" "ratbagctl")
                 python-wrap)
               #t))))))
    (native-inputs
     (append
       (list check pkg-config swig)
       (if (member (%current-system) (package-supported-systems valgrind))
         (list valgrind)
         '())))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("glib" ,glib)
       ("json-glib" ,json-glib)
       ("libevdev" ,libevdev)
       ("libsystemd" ,elogind)
       ("libunistring" ,libunistring)
       ("python" ,python)
       ("python-evdev" ,python-evdev)
       ("python-pygobject" ,python-pygobject)
       ("udev" ,eudev)))
    (home-page "https://github.com/libratbag/libratbag")
    (synopsis "DBus daemon and utility for configuring gaming mice")
    (description "libratbag provides @command{ratbagd}, a DBus daemon to
configure input devices, mainly gaming mice.  The daemon provides a generic
way to access the various features exposed by these mice and abstracts away
hardware-specific and kernel-specific quirks.  There is also the
@command{ratbagctl} command line interface for configuring devices.

libratbag currently supports devices from Logitech, Etekcity, GSkill, Roccat,
Steelseries.

The ratbagd DBus service can be enabled by adding the following service to
your operating-system definition:

  (simple-service 'ratbagd dbus-root-service-type (list libratbag))")
    (license license:expat)))

(define-public piper
  (package
    (name "piper")
    (version "0.8")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/libratbag/piper")
                   (commit version)))
             (sha256
              (base32 "1zkxrgvrg4bdqcj540lgdw35sj41n9cx8zrfhfd3f0y9m0piz7wg"))
             (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%meson-build-system-modules)
       #:modules (((guix build python-build-system) #:prefix python:)
                  (guix build meson-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'dont-update-gtk-icon-cache
           (lambda _
             (substitute* "meson.build"
               (("gtk_update_icon_cache: true")
                "gtk_update_icon_cache: false"))))
         (add-after 'unpack 'do-not-require-flake8
           (lambda _
             (substitute* "meson.build"
               (("find_program\\('flake8'" all)
                (string-append all ", required : false")))))
         (add-after 'install 'wrap-python
           (assoc-ref python:%standard-phases 'wrap))
         (add-after 'wrap-python 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-script (search-input-file outputs "bin/piper")
               `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
               `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                      ,(python:site-packages inputs outputs)))))))))
    (native-inputs
     (list appstream
           desktop-file-utils           ;for update-desktop-database
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config))
    (inputs
     (list bash-minimal
           adwaita-icon-theme
           gtk+
           guile-3.0                    ;for wrap-script
           libratbag
           python
           python-evdev
           python-lxml
           python-pycairo
           python-pygobject))
    (home-page "https://github.com/libratbag/piper/")
    (synopsis "Configure bindings and LEDs on gaming mice")
    (description "Piper is a GTK+ application for configuring gaming mice with
onboard configuration for key bindings via libratbag.  Piper requires
a @command{ratbagd} daemon running with root privileges.  It can be run
manually as root, but is preferably configured as a DBus service that can
launch on demand.  This can be configured by enabling the following service,
provided there is a DBus service present:

  (simple-service 'ratbagd dbus-root-service-type (list libratbag))")
    (license license:gpl2)))

(define-public xdg-desktop-portal-gnome
  (package
    (name "xdg-desktop-portal-gnome")
    (version "46.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1mhngp24k06i993kw6kzq0x8hwbbvkk3nq9s0cnm10w4bsi5ximm"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (delete-file-recursively "subprojects")))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags #~'("-Dsystemduserunitdir=no")))
    (inputs
     (list gnome-desktop
           gsettings-desktop-schemas
           libadwaita
           libxml2
           xdg-desktop-portal
           xdg-desktop-portal-gtk))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config))
    (home-page "https://gitlab.gnome.org/GNOME/xdg-desktop-portal-gnome")
    (synopsis "GNOME backend for xdg-desktop-portal")
    (description "xdg-desktop-portal-gnome implements a back-end for
@command{xdg-desktop-portal} that uses gtk and some more GNOME APIs.")
    (license license:lgpl2.1+)))

(define-public parlatype
  (package
    (name "parlatype")
    (version "3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gkarsay/parlatype")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cqrzbkyksfsm57riirmjkwf2nf2dgl1xpps1wvqxpij475qcb9b"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:tests? #f                      ;require internet access
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           (lambda _
             (substitute* "data/meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'install 'wrap-parlatype
           ;; Add gstreamer plugin provided in this package to system's
           ;; plugins.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gst-plugin-path (string-append
                                      out "/lib/gstreamer-1.0/"
                                      ":"
                                      (getenv "GST_PLUGIN_SYSTEM_PATH"))))
               (wrap-program (string-append out "/bin/parlatype")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" = (,gst-plugin-path)))))))))
    (native-inputs
     (list appstream-glib
           desktop-file-utils           ;for desktop-file-validate
           gettext-minimal
           `(,glib "bin")               ;for glib-compile-resources
           pkg-config
           yelp-tools))
    (inputs
     (list bash-minimal
           gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk+
           iso-codes/pinned
           pocketsphinx
           pulseaudio
           sphinxbase))
    (home-page "https://www.parlatype.org")
    (synopsis "GNOME audio player for transcription")
    (description "Parlatype is an audio player for the GNOME desktop
environment.  Its main purpose is the manual transcription of spoken
audio files.")
    (license license:gpl3+)))

(define-public jsonrpc-glib
  (package
    (name "jsonrpc-glib")
    (version "3.44.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                   name "-" version ".tar.xz"))
              (sha256
               (base32
                "1prhpdw4nrbcb00vvjhgc0w04ifaz2x5jbhhwnplcml0kizd2q8k"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-Denable_gtk_doc=true")))
    (inputs
     (list glib
           json-glib))
    (native-inputs
     (list gi-docgen
           `(,glib "bin") ; for glib-genmarshal, etc.
           gobject-introspection
           pkg-config
           vala))
    (home-page "https://gitlab.gnome.org/GNOME/jsonrpc-glib")
    (synopsis "JSON-RPC library for GLib")
    (description "Jsonrpc-GLib is a library to communicate with JSON-RPC based
peers in either a synchronous or asynchronous fashion.  It also allows
communicating using the GVariant serialization format instead of JSON when
both peers support it.  You might want that when communicating on a single
host to avoid parser overhead and memory-allocator fragmentation.")
    (license license:lgpl2.1+)))

(define-public gmobile
  (package
    (name "gmobile")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/Phosh/gmobile")
             (commit (string-append "v" version))))
       (file-name (git-file-name "gmobile" version))
       (sha256
        (base32
         "1cnm4vkvgrkxf1nnghs5zc13d6f46h4c57vn54rlcy6q7qjkdr74"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-compile-resources
           gobject-introspection
           pkg-config))
    (propagated-inputs
     (list glib json-glib))
    (synopsis "Functions useful in mobile related, glib based projects")
    (description "This package provides functions for mobiles.")
    (home-page "https://gitlab.gnome.org/World/Phosh/gmobile")
    (license license:lgpl2.1+)))

(define-public feedbackd
  (package
    (name "feedbackd")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://source.puri.sm/Librem5/feedbackd.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gfh965rddmg9glyh0gzkzxi27c7kfdakwrkycc7hg7s68p03xgh"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (delete-file-recursively "subprojects")))))
    (build-system meson-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-meson
                 (lambda _
                   (substitute* "meson.build"
                     (("udev.get_variable\\('udevdir'\\)")
                      "prefix / 'lib' / 'udev'")))))))
    (native-inputs
     (list `(,glib "bin") gobject-introspection pkg-config umockdev vala))
    (inputs
     (list dbus gmobile gsound json-glib libgudev))
    (propagated-inputs
     (list glib)) ; in Requires of libfeedback-0.0.pc
    (synopsis "Haptic/visual/audio feedback via DBus")
    (description "Feedbackd provides a DBus daemon to act on events to provide
haptic, visual and audio feedback.  It offers the libfeedbackd library and
GObject introspection bindings.")
     (home-page "https://source.puri.sm/Librem5/feedbackd")
     (license (list license:lgpl2.1+   ; libfeedbackd
                    license:lgpl3+)))) ; the rest

(define-public sysprof
  (package
    (name "sysprof")
    (version "46.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/sysprof/"
                           (version-major version) "/"
                           "sysprof-" version ".tar.xz"))
       (sha256
        (base32 "0xnil6ian02mlgdq9s5rwd4l5vp6ywyp4nm08q4lwgmbxdspxakk"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dsystemdunitdir=" #$output "/share/systemd")
              "-Dhelp=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-post-install
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false")))))))
    (propagated-inputs
     ;; Listed in sysprof-4.pc or sysprof-ui-5.pc
     (list glib json-glib libadwaita libdex polkit))
    (inputs
     (list glib
           gtk
           json-glib
           libadwaita
           libdazzle
           libdex
           libpanel
           libunwind
           polkit))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")          ;for gdbus-codegen, etc.
           itstool
           libxml2
           pkg-config))
    ;; This home page is so woefully out of date as to be essentially useless.
    ;; (home-page "http://www.sysprof.com")
    (home-page "https://wiki.gnome.org/Apps/Sysprof")
    (synopsis "System-wide performance profiler for GNU/Linux")
    (description
     "Sysprof performs detailed, accurate, and fast CPU profiling of an entire
GNU/Linux system including the kernel and all user-space applications.  This
helps find the function(s) in which a program spends most of its time.

It uses the kernel's built-in @code{ptrace} feature and handles shared
libraries.  Applications do not need to be recompiled--or even restarted.")
    (license license:gpl3+)))

(define-public sysprof-3.44
  (package
    (inherit sysprof)
    (name "sysprof")
    (version "3.44.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/sysprof/"
                                  (version-major+minor version) "/"
                                  "sysprof-" version ".tar.xz"))
              (sha256
               (base32 "0nq0icbln0ryqzlybr7wyl19mhr3vkqzs6wasn430fwpf5drypdb"))))
    (inputs (modify-inputs (package-inputs sysprof)
              (replace "glib" glib)
              (replace "gtk" gtk+)))
    (native-inputs (modify-inputs (package-native-inputs sysprof)
                     (replace "glib" `(,glib "bin"))))
    (arguments (substitute-keyword-arguments (package-arguments sysprof)
                 ((#:phases phases '%standard-phases)
                  #~(modify-phases #$phases
                      (replace 'disable-post-install
                        (lambda _
                          (substitute* "build-aux/meson/post_install.sh"
                            (("gtk-update-icon-cache") "true")
                            (("update-desktop-database") "true"))))))))))

(define-public libspelling
  (package
    (name "libspelling")
    (version "0.4.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/GNOME/libspelling")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "030s821sb9rsr1ysl79x7id1bsin9idy8z7p85qr9cvw1w3f2s7r"))))
    (build-system meson-build-system)
    (inputs (list enchant gtk gtksourceview sysprof))
    (native-inputs
     (list gi-docgen
           gobject-introspection
           pkg-config
           vala
           ;; For testing.
           aspell aspell-dict-en))
    (home-page "https://gitlab.gnome.org/GNOME/libspelling/")
    (synopsis "Spell-checking library for GTK 4")
    (description "This package provides a spell-checker for
GtkTextView widgets.")
    (license license:lgpl2.1+)))

(define-public gnome-builder
  (package
    (name "gnome-builder")
    (version "47.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "15dlm6zvq54djx6h1z3jg21fw4v21dwh7i9db9k367nd8wybk1s6"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t                 ;To wrap binaries and compile schemas
      #:configure-flags #~(list "-Dnetwork_tests=false" "-Ddocs=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-meson
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))
              (substitute* "build-aux/meson/post_install.py"
                ;; The post_install script does not seem to respect the
                ;; previous setting regarding gtk-update-icon-cache.
                (("gtk-update-icon-cache") "true")
                (("update-desktop-database") "true"))
              ;; This test is failing for unclear reasons.
              (substitute* "src/tests/meson.build"
                (("test\\('test-shortcuts'")
                 "# test('test-shortcuts'"))))
          (add-before 'build 'set-home
            (lambda _
              ;; Required for documentation.
              (setenv "HOME" (getcwd))))
          (add-before 'check 'pre-check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1"))))
         (add-after 'glib-or-gtk-wrap 'wrap-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (search-input-file outputs "bin/gnome-builder")
               `("GI_TYPELIB_PATH" suffix (,(getenv "GI_TYPELIB_PATH")))))))))
    (inputs
     (list bash-minimal                 ;for wrap-program
           cmark
           clang
           devhelp-with-libsoup2
           d-spy
           ;; Cyclic modular dependency
           (module-ref
            (resolve-interface
             '(gnu packages text-editors))
            'editorconfig-core-c)
           flatpak
           gom
           gtk
           json-glib
           jsonrpc-glib
           libadwaita
           libdazzle
           libdex
           libgit2-glib
           libpanel
           libpeas-2
           libportal
           libsoup
           libspelling
           llvm
           libostree
           python
           python-pygobject
           sysprof
           template-glib
           vte
           webkitgtk))
    (propagated-inputs
     (list gtksourceview))              ;needed for settings
    (native-inputs
     (list desktop-file-utils           ;for desktop-file-validate
           `(,glib "bin")
           gettext-minimal
           ;; GCC 14 seems to be required to not end up in a compilation
           ;; failure.
           ;; See <https://gitlab.gnome.org/GNOME/gnome-builder/-/issues/2176>.
           gcc-14
           gi-docgen
           pkg-config
           python                       ;for meson scripts
           vala
           xorg-server-for-tests))
    (home-page "https://wiki.gnome.org/Apps/Builder")
    (synopsis "Toolsmith for GNOME-based applications")
    (description
     "Builder aims to be an integrated development environment (IDE) for
writing GNOME-based software.  It features fuzzy search, auto-completion,
a mini code map, documentation browsing, Git integration, an integrated
profiler via Sysprof, debugging support, and more.")
    (license license:gpl3+)))

(define-public komikku
  (package
    (name "komikku")
    (version "1.57.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/valos/Komikku/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0z8sigv1a8a96y0hgm21j4qmpy06ziqw8yhlgbp8kbg70g5yhrbg"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-sources
            (lambda _
              (substitute* "komikku/utils.py"
                (("from komikku\\.servers import get_servers_list")
                 ;; code following that line should migrate old databases
                 ;; but the line itself results in an import error
                 "return data_dir_path"))))
          (add-after 'unpack 'skip-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("([a-z_]*): true" all option)
                 (cond                ; cond rather than match saves an import
                  ((string=? option "gtk_update_icon_cache")
                   (string-append option ": false"))
                  (else all))))))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/komikku")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                `("GDK_PIXBUF_MODULE_FILE" =
                  (,(getenv "GDK_PIXBUF_MODULE_FILE")))))))))
    (inputs
     (list bash-minimal
           gtk
           libadwaita
           libnotify
           libsecret
           python
           python-beautifulsoup4
           python-brotli
           python-cloudscraper
           python-colorthief
           python-dateparser
           python-emoji
           python-keyring
           python-lxml
           python-magic
           python-natsort
           python-piexif
           python-pillow
           python-pillow-heif
           python-pure-protobuf
           python-pycairo
           python-pygobject
           python-rarfile
           python-requests
           python-unidecode
           webkitgtk
           webp-pixbuf-loader))
    (native-inputs
     (list blueprint-compiler
           desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config))
    (home-page "https://apps.gnome.org/Komikku")
    (synopsis "Manga reader for GNOME")
    (description "Komikku is an online/offline manga reader for GNOME,
developed with the aim of being used with the Librem 5 phone.")
    (license license:gpl3+)
    (native-search-paths (list (search-path-specification
                                (variable "KOMIKKU_SERVERS_PATH")
                                (files '("lib/komikku/servers")))))))

(define-public komikku-servers
  (package
    (name "komikku-servers")
    (version "1.59.0")                  ; latest version that works with 1.57
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/valos/Komikku/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0sfqmqcpdl3bsbs0wxl4jwvd7wpgigkvvasy1niz6qm2vnp35gzq"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("komikku/servers" "lib/komikku/servers"))
      #:modules '((guix build copy-build-system)
                  (guix build utils)
                  (ice-9 ftw))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-conflicting-files
            (lambda _
              (with-directory-excursion "komikku/servers"
                (for-each delete-file
                          (scandir "."
                                   (lambda (f) (string-suffix? ".py" f)))))))
          (add-after 'install 'compile
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((site-dir (string-append (assoc-ref outputs "out")
                                             "/lib/komikku/servers")))
                (invoke "python" "-m" "compileall"
                        "--invalidation-mode=unchecked-hash" site-dir)))))))
    (native-inputs (list python-wrapper))
    (home-page "https://apps.gnome.org/Komikku")
    (synopsis "Servers for Komikku")
    (description "This package provides more recent servers for Komikku.")
    (license license:gpl3+)))

(define-public libgda
  (package
    (name "libgda")
    (version "6.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnome/sources/" name "/"
                           (version-major+minor version) "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "0w564z7krgjk19r39mi5qn4kggpdg9ggbyn9pb4aavb61r14npwr"))
       (patches (search-patches "libgda-CVE-2021-39359.patch"
                                "libgda-disable-data-proxy-test.patch"
                                "libgda-fix-build.patch"
                                "libgda-fix-missing-initialization.patch"
                                "libgda-skip-postgresql-tests.patch"))))
    (build-system meson-build-system)
    (native-inputs
     (list intltool
           iso-codes/pinned
           `(,glib "bin")
           gnome-common
           gettext-minimal
           gobject-introspection
           gtk-doc/stable
           pkg-config
           python
           vala
           yelp-tools))
    (inputs
     (list json-glib
           glib
           glade3
           gtk+
           libsecret
           libxslt
           openssl
           sqlite
           vala))
    (propagated-inputs
     (list libxml2))                    ; required by libgda-5.0.pc
    (home-page "https://gitlab.gnome.org/GNOME/libgda")
    (synopsis "Uniform data access")
    (description
     "GNU Data Access (GDA) is an attempt to provide uniform access to
different kinds of data sources (databases, information servers, mail spools,
etc).  It is a complete architecture that provides all you need to access
your data.")
    (license license:lgpl2.1+)))

(define-public gtranslator
  (package
    (name "gtranslator")
    (version "42.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0fzi48s3wz9mf6c1ndpkby83bgshgn2116nqjq31n1j3wszvqrra"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"   ;otherwise it tries to fetch stuff via git
           #:glib-or-gtk? #t
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-gtk-update-icon-cache
                 (lambda _
                   (substitute* "build-aux/meson/meson_post_install.py"
                     (("gtk-update-icon-cache") (which "true"))))))))
    (native-inputs
     (list `(,glib "bin")
           gettext-minimal
           itstool
           pkg-config))
    (inputs
     (list json-glib
           jsonrpc-glib
           gettext-minimal
           glib
           gsettings-desktop-schemas
           gspell
           libgda
           libhandy
           libsoup
           pango))
    (propagated-inputs
     (list gtksourceview-4))              ; required for source view
    (home-page "https://wiki.gnome.org/Apps/Gtranslator")
    (synopsis "Translation making program")
    (description
     "gtranslator is a quite comfortable gettext po/po.gz/(g)mo files editor
for the GNOME 3.x platform with many features.  It aims to be a very complete
editing environment for translation issues within the GNU gettext/GNOME desktop
world.")
    (license license:gpl3+)))


(define-public ocrfeeder
  (package
    (name "ocrfeeder")
    (version "0.8.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/ocrfeeder/"
                                  (version-major+minor version) "/"
                                  "ocrfeeder-" version ".tar.xz"))
              (sha256
               (base32
                "1vaaphzk6zn7pp2x9scphdzlbsma910wnbhd9xry50nx95cjlgdh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after
          'install 'wrap-program
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((prog (string-append (assoc-ref outputs "out")
                                       "/bin/" "ocrfeeder"))
                  (pylib (string-append (assoc-ref outputs "out")
                                        "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
              (wrap-program prog
                `("PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH") ,pylib))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     `(("glib:bin" ,glib "bin")                   ; for glib-compile-resources
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")                   ; for gtk-update-icon-cache
       ("intltool" ,intltool)
       ("itstool" ,itstool)
       ("pkg-config" ,pkg-config)
       ("xmllint" ,libxml2)))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("enchant" ,enchant)
       ("glib" ,glib)
       ("goocanvas" ,goocanvas)
       ("gtk" ,gtk+)
       ("gtkspell3" ,gtkspell3)
       ("libjpeg" ,libjpeg-turbo)
       ("libtiff" ,libtiff)
       ("libraw" ,libraw)
       ("ocrad" ,ocrad)
       ("python" ,python-wrapper)
       ("python-pygobject" ,python-pygobject)
       ("python-odfpy" ,python-odfpy)
       ("python-pillow" ,python-pillow)
       ("python-pyenchant" ,python-pyenchant)
       ("python-reportlab" ,python-reportlab)
       ("python-sane" ,python-sane)
       ("sane-backends" ,sane-backends)
       ("tesseract-ocr" ,tesseract-ocr)))
    (home-page "https://wiki.gnome.org/Apps/OCRFeeder")
    (synopsis "Complete OCR Suite")
    (description "OCRFeeder is a complete Optical Character Recognition and
Document Analysis and Recognition program.")
    (license license:gpl3+)))

(define-public libadwaita
  (package
    (name "libadwaita")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libadwaita/"
                                  (version-major+minor version) "/"
                                  "libadwaita-" version ".tar.xz"))
              (sha256
               (base32
                "1lyqvalqc09r4dkgrhpkp01r7c1c0zyf354icc0r375r9j9q7mwy"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Tests require a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           pkg-config
           sassc
           vala
           xorg-server-for-tests))
    (propagated-inputs
     (list appstream gtk))              ;libadwaita-1.pc 'Requires' it
    (home-page "https://gnome.pages.gitlab.gnome.org/libadwaita/")
    (synopsis "Building blocks for GNOME applications")
    (description
     "@code{libadwaita} offers widgets and objects to build GNOME
applications scaling from desktop workstations to mobile phones.  It is the
successor of @code{libhandy} for GTK4.")
    (license license:lgpl2.1+)))

(define-public gnome-power-manager
  (package
    (name "gnome-power-manager")
    (version "3.32.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major+minor version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0drfn3wcc8l4n07qwv6p0rw2dwcd00hwzda282q62l6sasks2b2g"))))
    (build-system meson-build-system)
    (inputs
     (list upower gtk+ gsettings-desktop-schemas adwaita-icon-theme))
    (native-inputs
     (list desktop-file-utils
           `(,glib "bin") gettext-minimal pkg-config))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-power-manager")
    (synopsis "Power management daemon for the GNOME desktop")
    (description "@code{gnome-power-manager} is a tool for viewing present and
historical battery usage and related statistics.")
    (license license:gpl2)))

(define-public xffm+
  (package
    (name "xffm+")
    (version "0.94")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xffm/xffm+/xffm+-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0gwbgmjzlgv9ba95cgaigjnc9njzi7qznhvzp0qrnnlq3nbcm1k1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; No tests exist
       #:configure-flags
       (let ((shared-mime-info (assoc-ref %build-inputs "shared-mime-info"))
             (out (assoc-ref %outputs "out")))
         (list (string-append "-DFREEDESKTOP_GLOBS=" shared-mime-info
                              "/share/mime/globs")
               (string-append "-DFREEDESKTOP_ALIAS=" shared-mime-info
                              "/share/mime/aliases")
               (string-append "-DFREEDESKTOP_ICONS=" shared-mime-info
                              "/share/mime/generic-icons")
               (string-append "-DCMAKE_INSTALL_PREFIX=" out)
               (string-append "-DPREFIX_BIN=" out "/bin")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-installation-destination
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
              ;; This is done so we can override.
              (("^set.CMAKE_INSTALL_PREFIX") "set(QCMAKE_INSTALL_PREFIX")
              ;; This is done so we can override.
              (("`set.PREFIX_BIN") "set(QPREFIX_BIN")))))))
    (native-inputs
     (list cmake pkg-config intltool gnu-gettext))
    (inputs
     (list glib gtk+ libx11 libsm libxv libxaw libxcb libxkbfile
           shared-mime-info))
    (synopsis "File manager")
    (description "This package provides a graphical file manager.")
    (home-page "http://xffm.org/")
    (license license:gpl3+)
    (properties '((upstream-name . "xffm")))))

(define-public gnome-remote-desktop
  (package
    (name "gnome-remote-desktop")
    (version "46.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0h9656vk5f4vn9wajh3ixhqsxk3adi44z4b3iabcj8i1fy1905wi"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~'("-Dsystemd=false"
               ;; RDP support requires CUDA (ffnvcodec)
               "-Drdp=false"
               ;; Enable VNC support
               "-Dvnc=true")
           #:glib-or-gtk? #t
           #:phases
           #~(modify-phases %standard-phases
               (delete 'check)
               (add-after 'install 'check
                 (assoc-ref %standard-phases
                            'check))
               (add-before 'check 'pre-check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "HOME" "/tmp")
                     (setenv "XDG_RUNTIME_DIR" (string-append (getcwd) "/runtime-dir"))
                     (mkdir (getenv "XDG_RUNTIME_DIR"))
                     (chmod (getenv "XDG_RUNTIME_DIR") #o700)
                     (setenv "GSETTINGS_SCHEMA_DIR"
                             (string-append #$output "/share/glib-2.0/schemas"))
                     ;; Unless enabled by the user, the VNC server will not
                     ;; start.
                     (invoke "gsettings"
                             "set"
                             "org.gnome.desktop.remote-desktop.vnc"
                             "enable" "true")
                     ;; Pipewire is required.
                     (setenv "PIPEWIRE_DEBUG" "2")
                     (setenv "PIPEWIRE_LOG" "meson-logs/pipewire.log")
                     (invoke "pipewire" "--version")
                     (system "pipewire &")))))))
    (inputs
     (list cairo
           glib
           libdrm
           libei
           libepoxy
           libgudev
           libnotify
           libsecret
           ;; Cyclic modular dependency
           (module-ref
            (resolve-interface
             '(gnu packages vnc))
            'libvnc)
           pipewire
           wireplumber
           tpm2-tss))
    (native-inputs
     (list asciidoc
           dbus
           docbook-xsl
           docbook-xml-4.3
           gettext-minimal
           `(,glib "bin")
           itstool
           libxml2
           libxslt
           mutter
           pkg-config
           python
           python-dbus
           python-pygobject
           xdg-desktop-portal-gnome))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-remote-desktop")
    (synopsis "Share GNOME desktop with remote sessions")
    (description "This package provides a remote desktop server for GNOME.")
    (license license:gpl2+)))

(define-public libcall-ui
  (package
    (name "libcall-ui")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/Phosh/libcall-ui")
             (commit "6798b38d4d66d069751151b3e9a202c6de8d7f3c")))
       (file-name (git-file-name "libcall-ui" version))
       (sha256
        (base32
         "0zfrxh77ag8garqj319amnxjcdyp3ig12dkxfkl6wbwn1mvyrwx8"))
       (patches (search-patches "libcall-ui-make-it-installable.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" (getcwd))
              ;; Tests require a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (propagated-inputs ; All these in call-ui.pc.
     (list glib
           gtk+
           libcallaudio
           libhandy))
    (native-inputs
     (list `(,glib "bin") ; glib-mkenums
           pkg-config
           xorg-server-for-tests))
    (synopsis "Common User Interfaces for call handling")
    (description "This package provides common user interfaces to make and
receive calls.")
    (home-page "https://gitlab.gnome.org/World/Phosh/libcall-ui")
    (license license:lgpl2.1+)))

(define-public calls
  (package
    (name "calls")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0a21anz81a3pqlrmzgyb1az53wc7kyz61xafxsylpxf41cm8vm0x"))
              (patches
               (search-patches "calls-disable-application-test.patch"
                               "calls-disable-sip-test.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags #~'("-Dgtk_doc=true")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" (getcwd))
              ;; Tests require a running X server.
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (inputs
     (list evolution-data-server
           feedbackd
           folks
           glib
           gom
           gstreamer
           gst-plugins-base
           gst-plugins-good
           gst-plugins-bad
           gtk
           libcall-ui
           libgee
           libpeas
           libadwaita
           modem-manager
           sofia-sip))
    (native-inputs
     (list desktop-file-utils           ;update-desktop-database
           gettext-minimal
           `(,glib "bin")               ;glib-mkenums
           gtk-doc                      ;gtkdoc-scan
           `(,gtk+ "bin")               ;gtk-update-icon-cache
           pkg-config
           python-docutils              ;rst2man
           vala
           xorg-server-for-tests))
    (home-page "https://gitlab.gnome.org/GNOME/calls")
    (synopsis "Phone dialer and call handler")
    (description "Calls can make and answer phone calls using different
backends, such as ModemManager for phones and @acronym{SIP, Session Initiation
Protocol} for @acronym{VoIP, Voice over @acronym{IP, Internet Protocol}}.")
    (license license:gpl3+)))

(define-public confy
  (package
    (name "confy")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~fabrixxm/confy")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0hjj1klndhjmy02lxn15cnid0ydnxi0ki59h4an0zsyaha77s1lm"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:imported-modules `(,@%meson-build-system-modules
                                (guix build python-build-system))
           #:modules '((guix build meson-build-system)
                       ((guix build python-build-system) #:prefix python:)
                       (guix build utils))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-post-install
                 (lambda _
                   (substitute* "meson.build"
                     (("gtk_update_icon_cache: true")
                      "gtk_update_icon_cache: false")
                     (("update_desktop_database: true")
                      "update_desktop_database: false"))))
               (add-after 'unpack 'patch-for-compatibility
                 (lambda _
                   ;; TODO: Remove when Python is updated to >= 3.11.
                   (substitute* (find-files "." "\\.py$")
                     (("import Self") "import Any as Self"))))
               (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (wrap-program (search-input-file outputs "bin/confy")
                     `("GUIX_PYTHONPATH" =
                       (,(getenv "GUIX_PYTHONPATH")
                        ,(python:site-packages inputs outputs)))
                     `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (inputs (list gtk
                  libadwaita
                  libnotify
                  python
                  python-icalendar
                  python-pygobject))
    (native-inputs (list blueprint-compiler
                         gettext-minimal
                         `(,glib "bin")
                         pkg-config))
    (home-page "https://confy.kirgroup.net")
    (synopsis "Conference Schedule Viewer")
    (description "Confy is a conference schedule viewer for GNOME.  It allows
you to mark favorite talks and highlights conflicts between favorited talks.")
    (license license:gpl3+)))

(define-public gtk-frdp
  (package
    (name "gtk-frdp")
    ;; The latest published tag is 3.37.1, but it is very old:
    ;; https://gitlab.gnome.org/GNOME/gtk-frdp/-/issues/39
    (version "3.37.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/GNOME/gtk-frdp")
             (commit "62fc62c5ccb7634f0bc87c57a4673877c24c94ed")))
       (file-name (git-file-name "gtk-frdp" version))
       (sha256
        (base32
         "0msw7qpsyf9hkyq9ddhvl4g4vk1fnyi7g0bddca9x6p9d0arprqz"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t))
    (inputs
     (list freerdp fuse gtk+))
    (native-inputs
     (list `(,glib "bin") gobject-introspection pkg-config vala))
    (home-page "https://gitlab.gnome.org/GNOME/gtk-frdp")
    (synopsis "RDP viewer widget for Gtk")
    (description "This library provides a widget to view
@acronym{RDP, Remote Desktop Protocol} sessions.")
    (license license:gpl3+)))

(define gtk-frdp-for-gnome-connections
  (let ((commit "6cfdc840159bb349310c3b81cd2df949f1522760")
        (revision "1"))
    (package
      (inherit gtk-frdp)
      (version (git-version "45.90" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.gnome.org/GNOME/gtk-frdp")
               (commit commit)))
         (file-name (git-file-name "gtk-frdp" version))
         (sha256
          (base32
           "1xgilpa2zkcnyi9hvj8yw1db19pz5d1xgvm1pm79mjs4ls8pdn0n")))))))

(define-public gnome-connections
  (package
    (name "gnome-connections")
    (version "46.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lr5imma2l2gh7z74y2f9c6k3k9pk85cvdr8vg0vs2wkwxlfl77v"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (delete-file-recursively "subprojects")))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-meson
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))))
          (add-after 'unpack 'disable-onboarding-dialog
            (lambda _
              (substitute* "src/application.vala"
                (("\\(new OnboardingDialog \\(main_window\\)\\).present \\(\\);")
                 "// Skip the onboarding dialog")))))))
    (inputs
     (list gtk+ gtk-frdp-for-gnome-connections gtk-vnc libhandy libsecret libxml2))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           itstool
           pkg-config
           vala))
    (home-page "https://apps.gnome.org/Connections")
    (synopsis "View and use other desktops")
    (description "Connections allows the user to connect to different
real or virtual machines, using @acronym{VNC, Virtual Network Computing}
or @acronym{RDP, Remote Desktop Protocol}.")
    (license license:gpl3+)))

(define-public lock
  (package
    (name "lock")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/konstantintutsch/Lock")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "18xyh7g4qqm2nx9wfd5bxf293dk0ahwr1acj4ypwswasv0isxdf9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-gtk-update-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("(gtk_update_icon_cache|update_desktop_database): true" _ key)
                 (string-append key ": false"))))))))
    (inputs (list gpgme
                  glib
                  libadwaita))
    (native-inputs (list blueprint-compiler
                         gettext-minimal
                         gobject-introspection
                         `(,glib "bin")
                         pkg-config))
    (home-page "https://konstantintutsch.com/Lock")
    (synopsis "Graphical front-end for GNU Privacy Guard")
    (description "This package provides a graphical frontend for
GNU Privacy Guard built with libadwaita.")
    (license license:expat)))

(define-public gnome-software
  (package
    (name "gnome-software")
    (version "46.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://gnome/sources/"
                       name "/"
                       (version-major version) "/"
                       name "-" version ".tar.xz"))
       (sha256 (base32 "0b5y9z64582aarw3v92wjm63yib2q85ylny1k7k4d2y48jivirb9"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:test-options
      ;; The plugins test suite requires a D-Bus system session, which
      ;; attempts to set its session under /var/run and fails.
      #~(list "--no-suite=plugins")
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list "-Dhardcoded_proprietary_webapps=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-iso-codes
            (lambda _
              (with-directory-excursion "src"
                (substitute* "./gs-language.c"
                  (("DATADIR")
                   (format #f "\"~a/share\"" #$iso-codes))))))
          (add-before 'install 'disable-gtk-update-icon-cache
            (lambda _
              (setenv "DESTDIR" "/")
              ;; Needed for complete RUNPATHs, but not actually needed at runtime.
              (copy-file
               "../build/lib/libgnomesoftware.so.20"
               (string-append #$output "/lib/libgnomesoftware.so.20")))))))
    (native-inputs
     (list docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gtk-doc
           libglib-testing
           libxslt                      ;for xsltproc
           pkg-config
           sysprof
           valgrind))
    (inputs
     (list appstream
           flatpak
           fwupd
           gdk-pixbuf
           gtk
           json-glib
           libadwaita
           libgudev
           libostree
           libsoup-minimal
           libxmlb
           malcontent
           packagekit
           polkit))
    (synopsis "Graphical software manager for GNOME")
    (description "GNOME Software allows you to find and install new
applications and system extensions and remove existing installed
applications.")
    (license license:gpl2+)
    (home-page "https://apps.gnome.org/en/Software/")))
