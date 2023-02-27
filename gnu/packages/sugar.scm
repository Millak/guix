;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages sugar)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages search)
  #:use-module (gnu packages time)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp))

(define-public sugar
  (package
    (name "sugar")
    (version "0.120")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0imhaj49n7ain33kmrqk19rzlfr50m84fbc011vgg1010ddp3vdw"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%glib-or-gtk-build-system-modules
        (guix build python-build-system))
      #:modules
      '((guix build glib-or-gtk-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-system
            (lambda _
              (substitute* "autogen.sh"
                (("^\"\\$srcdir/configure" m)
                 (string-append "#" m)))))
          (add-after 'unpack 'fix-references
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "bin/sugar.in"
                (("exec python3")
                 (string-append "exec " (which "python3"))))
              (substitute* "src/jarabe/main.py"
                (("'metacity'")
                 (string-append "'" (search-input-file inputs "/bin/metacity") "'"))
                (("'metacity-message")
                 (string-append "'" (search-input-file inputs "/bin/metacity-message"))))
              (substitute* "extensions/cpsection/datetime/model.py"
                (("/usr/share/zoneinfo/zone.tab")
                 (search-input-file inputs "/share/zoneinfo/zone.tab")))
              (substitute* "extensions/cpsection/modemconfiguration/model.py"
                (("/usr/share/zoneinfo/iso3166.tab")
                 (search-input-file inputs "/share/zoneinfo/iso3166.tab"))
                (("/usr/share/mobile-broadband-provider-info")
                 (dirname
                  (search-input-file inputs
                                     "/share/mobile-broadband-provider-info/serviceproviders.xml"))))
              ;; XXX: spawn_command_line_sync is not used correctly here, so
              ;; we need to patch invocations.
              (substitute* '("extensions/cpsection/aboutcomputer/model.py"
                             "src/jarabe/model/brightness.py")
                (("spawn_command_line_sync\\(cmd\\)")
                 "spawn_command_line_sync(cmd, 0)"))
              ;; XXX: The brightness component crashes, so we disable it here.
              (substitute* "src/jarabe/main.py"
                (("brightness.get_instance\\(\\)") ""))
              ;; TODO: these locations should be set to places that exist on
              ;; Guix System.
              #;
              (substitute* "extensions/cpsection/background/model.py"
                (("\\('/usr', 'share', 'backgrounds'\\)")
                 "('TODO')"))
              (substitute* "src/jarabe/view/viewhelp.py"
                (("/usr/share/sugar/activities/")
                 "/run/current-system/profile/share/sugar/activities/"))))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (for-each
               (lambda (executable)
                 (wrap-program executable
                   `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                          ,(python:site-packages inputs outputs)))
                   `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
               (find-files (string-append #$output "/bin") "^sugar.*")))))))
    (inputs
     (list gtk+
           metacity
           mobile-broadband-provider-info
           python
           sugar-toolkit-gtk3
           tzdata))
    ;; Some packages are propagated so that they can be used with gobject
    ;; introspection at runtime; others are propagated for their dbus
    ;; services.
    (propagated-inputs
     (list gsettings-desktop-schemas
           gstreamer
           gtk+
           gtksourceview-3
           libsoup-minimal-2
           libwnck
           libxklavier
           network-manager
           python-gwebsockets
           sugar-artwork                ;for cursor theme
           sugar-datastore              ;for org.laptop.sugar.DataStore
           telepathy-glib
           ;; This is for org.freedesktop.Telepathy.AccountManager at runtime
           telepathy-mission-control
           ;; This is for the UPowerGlib namespace
           upower
           webkitgtk-with-libsoup2))
    (native-inputs
     (list autoconf automake
           gettext-minimal
           intltool
           (list glib "bin")
           libtool
           pkg-config
           python-empy))
    (home-page "https://www.sugarlabs.org/")
    (synopsis "Sugar GTK shell")
    (description "Sugar is the desktop environment component of a worldwide
effort to provide every child with an equal opportunity for a quality
education.  Available in more than twenty-five languages, Sugar Activities are
used every school day by children in more than forty countries.")
    (license license:gpl3+)))

(define-public sugar-artwork
  (package
    (name "sugar-artwork")
    (version "0.120")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-artwork")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mjydyx7kbk429s3kswfb8x7g5smjwnai924avwxab1kjsjjksm9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "--without-gtk2")
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure" m)
                (string-append "#" m))))))))
    (inputs (list cairo gtk+))
    (native-inputs
     (list autoconf automake
           icon-naming-utils
           libtool
           pkg-config
           python
           python-empy
           xcursorgen))
    (home-page "https://www.sugarlabs.org/")
    (synopsis "Sugar icons and themes")
    (description "Sugar Artwork provides icons, and GTK+ CSS to build
activities and other Sugar components.")
    (license (list license:lgpl2.1+
                   license:asl2.0))))

(define-public sugar-datastore
  (package
    (name "sugar-datastore")
    (version "0.120")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-datastore")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wf33w6dm26i8a1zpb40339fj3l9vxjznagls9bc845nld318sqc"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%glib-or-gtk-build-system-modules
        (guix build python-build-system))
      #:modules
      '((guix build glib-or-gtk-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure" m)
                (string-append "#" m)))))
         (add-after 'unpack 'patch-reference-to-du
           (lambda _
             (substitute* "src/carquinyol/datastore.py"
               (("/usr/bin/du") (which "du")))))
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (for-each
              (lambda (executable)
                (wrap-program executable
                  `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                         ,(python:site-packages inputs outputs)))
                  `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
              (list (search-input-file outputs "bin/copy-from-journal")
                    (search-input-file outputs "bin/copy-to-journal")
                    (search-input-file outputs "bin/datastore-service"))))))))
    (inputs
     (list python
           sugar-toolkit-gtk3))
    (propagated-inputs
     (list python-dbus
           python-pygobject
           python-xapian-bindings))
    (native-inputs
     (list autoconf automake
           libtool
           pkg-config))
    (home-page "https://www.sugarlabs.org/")
    (synopsis "Service for Sugar activities to store and retrieve data")
    (description "Sugar Datastore provides activities with a way to store data
and metadata, and the journal with querying and full text search.")
    (license license:gpl2+)))

(define-public sugar-toolkit-gtk3
  (package
    (name "sugar-toolkit-gtk3")
    (version "0.120")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-toolkit-gtk3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wqanc38zplyiq1vxda4bj1n0xd78zqlwml6lzklsrbz923llykz"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%glib-or-gtk-build-system-modules
        (guix build python-build-system))
      #:modules
      '((guix build glib-or-gtk-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure" m)
                (string-append "#" m)))))
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (search-input-file outputs "bin/sugar-activity3")
               `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                      ,(python:site-packages inputs outputs)))
               `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (inputs
     (list alsa-lib
           libice
           libsm
           libx11
           libxfixes
           libxi
           python))
    (propagated-inputs
     ;; The gi typelib files are needed by users of this library.
     (list gdk-pixbuf
           gobject-introspection
           gstreamer ;for speech
           gst-plugins-espeak
           gtk+
           (librsvg-for-system)

           ;; This package is used as a Python library by users, so these must
           ;; be propagated.
           python-pycairo
           python-dateutil
           python-dbus
           python-decorator
           python-pygobject
           python-six))
    (native-inputs
     (list autoconf automake
           gettext-minimal
           glib
           (list glib "bin")
           intltool
           libtool
           pkg-config))
    (home-page "https://www.sugarlabs.org/")
    (synopsis "GTK+ widgets and services for Sugar components")
    (description "Sugar Toolkit provides services and a set of GTK+ widgets to
build activities and other Sugar components.  This is the GTK+ 3 binding of
the Sugar Toolkit.")
    (license license:lgpl2.1+)))
