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
  #:use-module (gnu packages abiword)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages search)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
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
              (substitute* "src/jarabe/intro/window.py"
                (("ssh-keygen")
                 (search-input-file inputs "/bin/ssh-keygen")))
              (substitute* "src/jarabe/journal/model.py"
                (("xdg-user-dir")
                 (search-input-file inputs "/bin/xdg-user-dir")))
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
              (substitute* "extensions/cpsection/aboutcomputer/model.py"
                (("ethtool")
                 (search-input-file inputs "/sbin/ethtool")))
              (substitute* "extensions/cpsection/language/model.py"
                (("'locale'")
                 (string-append "'"
                                (search-input-file inputs "/bin/locale")
                                "'")))
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
                   `("GI_TYPELIB_PATH" prefix
                     (,(getenv "GI_TYPELIB_PATH")))))
               (find-files (string-append #$output "/bin") "^sugar.*")))))))
    (inputs
     (list bash-minimal
           ethtool
           gtk+
           metacity
           mobile-broadband-provider-info
           openssh                      ;for ssh-keygen
           python
           sugar-toolkit-gtk3
           tzdata
           xdg-user-dirs))
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
           telepathy-gabble             ;for link-local XMPP
           telepathy-glib
           ;; This is for org.freedesktop.Telepathy.AccountManager at runtime
           telepathy-mission-control
           telepathy-salut              ;for XMPP neighborhood
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
         (add-after 'unpack 'patch-tool-references
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/carquinyol/datastore.py"
               (("/usr/bin/du") (which "du")))
             (substitute* "src/carquinyol/optimizer.py"
               (("'md5sum'")
                (string-append "'"
                               (search-input-file inputs "/bin/md5sum")
                               "'")))))
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
     (list bash-minimal
           coreutils
           python
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
      #~(modify-phases %standard-phases
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
                `("GI_TYPELIB_PATH" prefix
                  (,(getenv "GI_TYPELIB_PATH")
                   ,(string-append #$output "/lib/girepository-1.0")))))))))
    (inputs
     (list alsa-lib
           bash-minimal
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
           python-six

           telepathy-glib
           webkitgtk-with-libsoup2))
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


(define-public sugar-block-party-activity
  (let ((commit "a49e68ec00e647af712d8e284622722f2f78b6bf")
        (revision "1"))
    (package
      (name "sugar-block-party-activity")
      (version (git-version "11" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/block-party-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0hy82c0gn1hr34arhnh9k6mx2789ki85fkgvga4sw6gwh31278pl"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'install
              (lambda _
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (propagated-inputs
       (list gtk+
             gstreamer
             gst-plugins-base
             sugar-toolkit-gtk3))
      (home-page "https://github.com/sugarlabs/block-party-activity")
      (synopsis "Tetris-like game for Sugar desktop environment")
      (description "Block Party is an activity for the Sugar desktop providing
a Tetris-like game.")
      (license license:expat))))

(define-public sugar-browse-activity
  (package
    (name "sugar-browse-activity")
    (version "207")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/browse-activity")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01p1gfdw9fhn92didc9sq23n6a3krs6findbbmicijz91kx8kfb2"))))
    (build-system python-build-system)
    (arguments
     (list
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-reference-to-gschema-compiler
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "browser.py"
                (("glib-compile-schemas")
                 (search-input-file inputs "/bin/glib-compile-schemas")))))
          (add-after 'unpack 'patch-launcher
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "activity/activity.info"
                (("exec = sugar-activity3")
                 (string-append "exec = "
                                (search-input-file inputs "/bin/sugar-activity3"))))))
          (replace 'install
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "install"
                      (string-append "--prefix=" #$output)))))))
    ;; All these libraries are accessed via gobject introspection.
    (propagated-inputs
     (list evince
           gobject-introspection
           gtk+
           (librsvg-for-system)
           libsoup-minimal-2
           python-pygobject
           sugar-toolkit-gtk3
           telepathy-glib
           webkitgtk-with-libsoup2))
    (inputs
     (list (list glib "bin")))
    (native-inputs
     (list gettext-minimal))
    (home-page "https://help.sugarlabs.org/browse.html")
    (synopsis "Sugar activity to browse the internet")
    (description "Browse is a web browser activity for the Sugar desktop.")
    (license (list license:cc0       ;metadata
                   license:lgpl2.0+
                   license:gpl2+
                   license:gpl3+))))

(define-public sugar-help-activity
  (let ((commit "492531e95a4c60af9b85c79c59c24c06c2cd4bb3")
        (revision "1"))
    (package
      (name "sugar-help-activity")
      (version (git-version "20" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/help-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0awjbqyc9f74dx0d7fgjk42vfsygxr8jhwqiv4hpggqcawc02xv8"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'build
              (lambda _ (invoke "make" "html")))
            (replace 'install
              (lambda _
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs
       (list sugar-toolkit-gtk3
             python-sphinx))
      (home-page "https://github.com/sugarlabs/help-activity")
      (synopsis "Sugar activity for accessing documentation and manuals")
      (description "This is an activity for the Sugar environment which aims
to provide users with easy access to documentation and manuals.")
      (license license:gpl3+))))

(define-public sugar-jukebox-activity
  (let ((commit "e11f40c94c1c6302d3e36ddf4dc8101732ffb9d9")
        (revision "1"))
    (package
      (name "sugar-jukebox-activity")
      (version (git-version "36" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/jukebox-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0gm1cj4vrwwdriyshd27w6vc0palwpg9pnnab5axinrnkzczyk1v"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gtk+
             gstreamer
             gst-plugins-base
             sugar-toolkit-gtk3))
      (inputs
       (list gettext-minimal))
      (home-page "https://help.sugarlabs.org/jukebox.html")
      (synopsis "Media player for the Sugar learning environment")
      (description "Jukebox is the media player to play different kinds of
audio and video files including online streams.  It also supports playlists
like @file{.m3u} and @file{.pls}.")
      (license license:gpl2+))))

(define-public sugar-log-activity
  (let ((commit "0c45118958be14a1844456703f0b392de250bc88")
        (revision "1"))
    (package
      (name "sugar-log-activity")
      (version (git-version "42" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/log-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0pacd677gfhyym153x5grwimk8wgm4c9k0a463pq6fdrhm1g5wpc"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-locations
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "logcollect.py"
                  (("'/sbin/ifconfig'")
                   (string-append "'" (search-input-file inputs "/bin/ifconfig") "'"))
                  (("'/sbin/route")
                   (string-append "'" (search-input-file inputs "/bin/route")))
                  (("'/bin/df")
                   (string-append "'" (search-input-file inputs "/bin/df")))
                  (("'/bin/ps")
                   (string-append "'" (search-input-file inputs "/bin/ps")))
                  (("'/usr/bin/free")
                   (string-append "'" (search-input-file inputs "/bin/free")))
                  (("'/usr/bin/top")
                   (string-append "'" (search-input-file inputs "/bin/top")))
                  (("'/usr/share/sugar/activities")
                   "'/run/current-system/profile/share/sugar/activities"))))
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gtk+
             sugar-toolkit-gtk3))
      (inputs
       (list coreutils net-tools procps))
      (native-inputs
       (list gettext-minimal))
      (home-page "https://help.sugarlabs.org/log.html")
      (synopsis "Log activity for the Sugar learning environment")
      (description "Log is part of the Sugar desktop.  Log is used when
looking for why an activity or Sugar is not working properly.")
      (license license:gpl2+))))

(define-public sugar-maze-activity
  (package
    (name "sugar-maze-activity")
    (version "31")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/maze-activity")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ai2ws3mqkxi13chy0hidd1gxiv97862r9lg8qgxb7qkxqyh6afr"))))
    (build-system python-build-system)
    (arguments
     (list
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-launcher
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "activity/activity.info"
                (("exec = sugar-activity3")
                 (string-append "exec = "
                                (search-input-file inputs "/bin/sugar-activity3"))))))
          (replace 'install
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "install"
                      (string-append "--prefix=" #$output)))))))
    ;; All these libraries are accessed via gobject introspection.
    (propagated-inputs
     (list gtk+
           telepathy-glib))
    (inputs
     (list sugar-toolkit-gtk3 gettext-minimal))
    (home-page "https://github.com/sugarlabs/maze-activity")
    (synopsis "Simple maze game for the Sugar learning environment")
    (description "Try to make your way through an increasingly difficult path,
or you can also play with a friend!")
    (license license:gpl3+)))

(define-public sugar-read-activity
  (let ((commit "25f69e41a4fa69d93c73c0c9367b4777a014b1cd")
        (revision "1"))
    (package
      (name "sugar-read-activity")
      (version (git-version "123" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/read-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "03piap3r6j58s38cza55bm16g5icrmnhl0s6kpy5hj46vaa5x4fh"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list evince
             gtk+
             sugar-toolkit-gtk3
             webkitgtk-with-libsoup2))
      (inputs
       (list gettext-minimal))
      (home-page "https://help.sugarlabs.org/read.html")
      (synopsis "Read PDF and TXT files in the Sugar learning environment")
      (description "The Read activity allows the laptop to act as a book
reader.  It has a simple interface, and will view many kinds of text and
image-based book-like materials.  It will have particular strengths in
handheld mode, with extremely low power consumption and simple navigation
controls.")
      (license license:gpl2+))))

(define-public sugar-terminal-activity
  (let ((commit "a1f92b9da6121bc9a6fbba2c3f3b885dd26d4617")
        (revision "1"))
    (package
      (name "sugar-terminal-activity")
      (version (git-version "47" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/terminal-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11p7rrnlaw374h3qravhp915vdblvn07i2mnrzn7mhapkwvkg4h5"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gtk+
             vte
             sugar-toolkit-gtk3))
      (inputs
       (list gettext-minimal))
      (home-page "https://help.sugarlabs.org/terminal.html")
      (synopsis "Terminal activity for the Sugar learning environment")
      (description "Terminal is a full-screen text mode program that provides
a Command-Line Interface (CLI) to the system.")
      (license (list license:gpl2+ license:gpl3+)))))

(define-public sugar-typing-turtle-activity
  (package
    (name "sugar-typing-turtle-activity")
    (version "32")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/typing-turtle-activity")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0shadv9wgddjvl97kvsqb8iw1wmmfw5lzcqk78hd70pzvh4c1hmd"))))
    (build-system python-build-system)
    (arguments
     (list
      #:test-target "check"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-reference-to-executables
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "keyboard.py"
                (("setxkbmap")
                 (search-input-file inputs "/bin/setxkbmap")))))
          (add-after 'unpack 'patch-launcher
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "activity/activity.info"
                (("exec = sugar-activity3")
                 (string-append "exec = "
                                (search-input-file inputs "/bin/sugar-activity3"))))))
          (replace 'install
            (lambda _
              (invoke "python" "setup.py" "install"
                      (string-append "--prefix=" #$output)))))))
    (native-inputs
     (list gettext-minimal sugar-toolkit-gtk3))
    (inputs
     (list setxkbmap))
    (home-page "https://help.sugarlabs.org/en/typing_turtle.html")
    (synopsis "Learn typing")
    (description "Need some help typing?  In this activity for the Sugar
environment you will learn the best way to hold your hands in order for you to
become a faster typist.")
    (license license:gpl3+)))

(define-public sugar-write-activity
  (let ((commit "dd854c06378cabf0d064d8dc87b5789d2a1a7746")
        (revision "1"))
    (package
      (name "sugar-write-activity")
      (version (git-version "101" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/sugarlabs/write-activity")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0lw34hf31fyfvqilzlmcz3c7zki0iqkn1zp2sv3dih016gwqg5pw"))))
      (build-system python-build-system)
      (arguments
       (list
        #:test-target "check"
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs "/bin/sugar-activity3"))))))
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list abiword
             gdk-pixbuf
             gtk+
             libgsf
             telepathy-glib))
      (inputs
       (list sugar-toolkit-gtk3
             gettext-minimal))
      (home-page "https://help.sugarlabs.org/write.html")
      (synopsis "Word processor for Sugar desktop")
      (description "Write is a word processor activity for the Sugar desktop.
Write embeds the AbiWord word processor, and can be used to write and edit
text documents.")
      (license license:gpl2+))))
