;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages cinnamon)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg))

(define-public libxapp
  (package
    (name "libxapp")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linuxmint/xapp/")
                    (commit version)))
              (sha256
               (base32
                "0cy9g0zqcbx9zscc9qavqmghfyfb8244cg299llv1ha8n6mpxl3s"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:modules
      `((guix build meson-build-system)
        (guix build utils)
        ((guix build python-build-system) #:prefix python:))
      #:imported-modules
      `(,@%meson-build-system-modules
        (guix build python-build-system))
      #:configure-flags
      #~(list (string-append
               "-Dpy-overrides-dir="
               (python:site-packages %build-inputs %outputs) "/gi/overrides"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-gtk-module-path
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "libxapp/meson.build"
                (("gtk3_dep\\.get_pkgconfig_variable[(]'libdir'[)]")
                 (string-append "'" (assoc-ref outputs "out") "/lib'")))

              (substitute* "scripts/pastebin"
                (("'nc'")
                 (string-append "'"
                                (search-input-file inputs "/bin/nc")
                                "'")))

              (substitute* "scripts/upload-system-info"
                (("'inxi'")
                 (string-append "'"
                                (search-input-file inputs "/bin/inxi")
                                "'"))
                (("'/usr/bin/pastebin'")
                 (string-append "'"
                                (assoc-ref outputs "out")
                                "/bin/pastebin'"))
                (("'xdg-open'")
                 (string-append "'"
                                (search-input-file inputs "/bin/xdg-open")
                                "'"))))))))
    (inputs
     (list dbus
           glib                         ; for gio
           gtk+
           inxi-minimal                 ; used by upload-system-info
           libdbusmenu
           libgnomekbd
           netcat                       ; used by pastebin
           xdg-utils))                  ; used by upload-system-info
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-mkenums
           gobject-introspection
           pkg-config
           python
           python-pygobject
           vala))
    (home-page "https://github.com/linuxmint/xapp")
    (synopsis "Library for traditional GTK applications")
    (description
     "The libxapp package contains the components which are common to multiple
GTK desktop environments (Cinnamon, MATE and Xfce) and required to implement
cross-DE solutions.")
    (license license:lgpl3+)))

(define-public cinnamon-desktop
  (package
    (name "cinnamon-desktop")
    (version "3.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/linuxmint/cinnamon-desktop")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "18mjy80ly9361npjhxpm3n0pkmrwviaqr2kixjb7hyxa6kzzh5xw"))))
    (build-system gnu-build-system)
    ;; TODO: package 'libgsystem'.
    (inputs
     (list accountsservice
           gtk+
           glib
           gobject-introspection
           gnome-common
           libxkbfile
           libxrandr
           python-2
           pulseaudio
           xkeyboard-config))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin") ; glib-gettextize
           intltool
           libtool
           pkg-config))
    (home-page "https://github.com/linuxmint/cinnamon-desktop/")
    (synopsis "Library for the Cinnamon Desktop")
    (description
     "The cinnamon-desktop package contains the libcinnamon-desktop library,
as well as some desktop-wide documents.")
    (license (list license:gpl2+ license:lgpl2.0+
                   license:expat)))) ;display-name.c , edid-parse.c
