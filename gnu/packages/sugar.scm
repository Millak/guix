;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023-2025 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages search)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages time)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp))

(define-public sugar
  (package
    (name "sugar")
    (version "0.122")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1baz5w9z2p1m6h842ap2ygl5fb47pp2zshjzm9cmhbwfl9zdw4fa"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%glib-or-gtk-build-system-modules
        ,@%pyproject-build-system-modules)
      #:modules
      `(((guix build pyproject-build-system) #:prefix py:)
        ,@%glib-or-gtk-build-system-default-modules)
      #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-build-system
              (lambda _
                (substitute* "autogen.sh"
                  (("^\"\\$srcdir/configure" m)
                   (string-append "#" m)))
                ;; This .po file does not exist
                (substitute* "po/LINGUAS"
                  (("^ig") ""))))
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
                   (search-input-file inputs "/bin/ssh-keygen"))
                  ;; ssh-keygen no longer supports dsa.
                  (("-t dsa") "-t rsa"))
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
                (substitute* "extensions/cpsection/aboutcomputer/model.py"
                  (("ethtool")
                   (search-input-file inputs "/sbin/ethtool")))
                (substitute* "extensions/cpsection/language/model.py"
                  (("'locale'")
                   (string-append "'"
                                  (search-input-file inputs "/bin/locale")
                                  "'")))
                ;; This is a global location on Guix System.  Ideally we would
                ;; have a search path here.
                (substitute* "extensions/cpsection/background/model.py"
                  (("\\('/usr', 'share', 'backgrounds'\\)")
                   "('/run', 'current-system', 'profile', 'share', 'backgrounds')"))
                (substitute* "src/jarabe/view/viewhelp.py"
                  (("/usr/share/sugar/activities/")
                   "/run/current-system/profile/share/sugar/activities/"))))
            (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (for-each
                 (lambda (executable)
                   (wrap-program executable
                     `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                            ,(py:site-packages inputs outputs)))
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
           libsoup-minimal
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
           webkitgtk-for-gtk3))
    (native-inputs
     (list autoconf-2.72 automake
           gettext-minimal
           intltool
           (list glib "bin")
           libtool
           pkg-config
           python-empy))
    (home-page "https://www.sugarlabs.org/")
    (synopsis "GTK shell")
    (description "Sugar is the desktop environment component of a worldwide
effort to provide every child with an equal opportunity for a quality
education.  Available in more than twenty-five languages, Sugar Activities are
used every school day by children in more than forty countries.")
    (license license:gpl3+)))

(define-public sugar-artwork
  (package
    (name "sugar-artwork")
    (version "0.122")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-artwork")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mqd5kk7x51drd0p8vwb2v85lqq796gwqdq3p9vj2ih5gxvqdxxz"))))
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
     (list autoconf-2.72 automake
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
    (version "0.122")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-datastore")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05p24m4ikk5p7xfb9iq9m0mnqwmji3mlxdj1wk9761c1r97a1345"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%glib-or-gtk-build-system-modules
        ,@%pyproject-build-system-modules)
      #:modules
      `(((guix build pyproject-build-system) #:prefix py:)
        ,@%glib-or-gtk-build-system-default-modules)
      #:phases
        #~(modify-phases %standard-phases
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
                                            ,(py:site-packages inputs outputs)))
                     `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))
                 (list (search-input-file outputs "bin/copy-from-journal")
                       (search-input-file outputs "bin/copy-to-journal")
                       (search-input-file outputs
                                        "bin/datastore-service"))))))))
    (inputs
     (list bash-minimal
           coreutils
           python
           sugar-toolkit-gtk3))
    (propagated-inputs
     (list python-dbus-1.2
           python-pygobject
           python-xapian-bindings))
    (native-inputs
     (list autoconf-2.72
           automake
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
    (version "0.122")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-toolkit-gtk3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "158gcilx25g8pivy42dk8ncc12bmn8g8iibpbik0s2zs1n42ibpy"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules (append %glib-or-gtk-build-system-modules
                                 %pyproject-build-system-modules)
      #:modules `(((guix build pyproject-build-system) #:prefix python:)
                  ,@%glib-or-gtk-build-system-default-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-build-system
            (lambda _
              (substitute* "autogen.sh"
                (("^\"\\$srcdir/configure" m)
                 (string-append "#" m)))))
          (add-after 'unpack 'patch-references
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/sugar3/eggsmclient-xsmp.c"
                (("/bin/rm") (search-input-file inputs "/bin/rm")))
              (substitute* "src/sugar3/mime.py"
                (("'/usr/local/share/'" m)
                 (string-append m ", '/run/current-system/profile/share'")))
              (substitute* "src/sugar3/bundle/activitybundle.py"
                (("'update-mime-database', mime_dir")
                 (string-append "'"
                                (search-input-file inputs "/bin/update-mime-database")
                                "', mime_dir")))
              (substitute* "src/sugar3/bundle/bundle.py"
                (("'unzip', '-o'")
                 (string-append "'"
                                (search-input-file inputs "/bin/unzip")
                                "', '-o'")))
              ;; ssh-keygen no longer supports DSA.
              (substitute* "src/sugar3/profile.py"
                (("(.*)magic = 'ssh-dss '" m indent)
                 (string-append m "\n" indent "magicrsa = 'ssh-rsa '\n"))
                (("if not line.startswith\\(magic\\)")
                 "if not line.startswith(magic) and not line.startswith(magicrsa)"))))
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
           python
           shared-mime-info
           unzip))
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
           python-dbus-1.2
           python-decorator
           python-pygobject
           python-six

           telepathy-glib
           webkitgtk-for-gtk3))
    (native-inputs
     (list autoconf-2.72
           automake
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
  (let ((commit "26a58f14254d6ae38b7bfa3cb2fc63291eefcc97")
        (revision "1"))
    (package
      (name "sugar-block-party-activity")
      (version (git-version "12" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/block-party-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0zinqhwmvyvk1zvs28dr71p68vb6widn4v3zp35zlzj4ayyn5rvx"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
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
    (version "208")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sugarlabs/browse-activity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lxwkwz7bz8vd0jgsgvlwdm6gkrmzcmwlyqvp12j2jk5mpr4fp44"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "check")
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
                                (search-input-file inputs
                                                   "/bin/sugar-activity3"))))))
          (delete 'build)
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
           libsoup-minimal
           python-pygobject
           sugar-toolkit-gtk3
           telepathy-glib
           webkitgtk-for-gtk3))
    (inputs (list (list glib "bin")))
    (native-inputs (list gettext-minimal python-setuptools))
    (home-page "https://help.sugarlabs.org/browse.html")
    (synopsis "Sugar activity to browse the internet")
    (description "Browse is a web browser activity for the Sugar desktop.")
    (license (list license:cc0 ;metadata
                   license:lgpl2.0+
                   license:gpl2+
                   license:gpl3+))))

(define-public sugar-cellgame-activity
  (let ((commit "4a22fd177af224d2df588590eb835affacd5ca72")
        (revision "1"))
    (package
      (name "sugar-cellgame-activity")
      (version (git-version "5" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/cellgame")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09dxq06dr43i3g8im4j1xffl19rzr1pwbixwgb0kipnmbx8pln5c"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (add-after 'unpack 'inject-load-path
              (lambda _
                (substitute* "activity.py"
                  (("^import pygame")
                   (string-append "\
import sys
for directory in \"" (getenv "GUIX_PYTHONPATH") "\".split(\":\"):
    try:
        sys.path.index(directory)
    except ValueError:
        sys.path.insert(1, directory)
import pygame
")))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs (list gtk+ sugar-toolkit-gtk3))
      (inputs (list python-pygame))
      (native-inputs (list gettext-minimal python-setuptools))
      (home-page "https://github.com/sugarlabs/cellgame")
      (synopsis "Cell game for Sugar")
      (description "This game for the Sugar desktop is based on the mechanisms
present in gene regulatory networks.")
      (license license:gpl3+))))

(define-public sugar-chat-activity
  ;; The last release was in 2019 and since then commits have been published
  ;; that include build fixes and translation updates.
  (let ((commit "fbab21ebddd5a11ab37b8a5a622214836ec1ffe7")
        (revision "2"))
    (package
      (name "sugar-chat-activity")
      (version (git-version "86" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/chat")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "190vkrnc3s97gnc69ddr4agh8qf04cbasibf74whn3amnsbch1yn"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gdk-pixbuf
             gobject-introspection
             gtk+
             gstreamer
             gst-plugins-base
             python-pygobject
             sugar-toolkit-gtk3
             telepathy-glib))
      (native-inputs (list gettext-minimal python-setuptools))
      (home-page "https://help.sugarlabs.org/chat.html")
      (synopsis "Sugar activity to chat")
      (description "Chat is an activity used to exchange messages with friends
or classmates.")
      (license license:gpl2+))))

(define-public sugar-classify-cats-activity
  (let ((commit "83aa89788c65bfdd3f77e24ac5a32b37f9518e54")
        (revision "1"))
    (package
      (name "sugar-classify-cats-activity")
      (version (git-version "2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/classify-cats")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "101drh1dqgr9qsz3r1fzkcn5h6z720zskaqnz2aixzp2ybvh17wk"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gdk-pixbuf
             gobject-introspection
             gtk+
             python-pygobject
             sugar-toolkit-gtk3))
      (native-inputs (list gettext-minimal python-setuptools))
      (home-page "https://github.com/sugarlabs/classify-cats")
      (synopsis "Classify cats based on various criteria")
      (description "This is a Sugar activity where players classify cats based
on various criteria.")
      (license license:gpl3+))))

(define-public sugar-commander-activity
  (let ((commit "a018652903e1c52c86ebf23e3250e7b68327427f")
        (revision "1"))
    (package
      (name "sugar-commander-activity")
      (version (git-version "11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/sugar-commander")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02n5wqh9cwr3jnjaxyd9kxcls4h3fdhhxdcyvvxmya08h20idvgd"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (add-after 'unpack 'inject-load-path
              (lambda _
                (substitute* "sugarcommander.py"
                  (("^import logging")
                   (string-append "\
import sys
for directory in \"" (getenv "GUIX_PYTHONPATH") "\".split(\":\"):
    try:
        sys.path.index(directory)
    except ValueError:
        sys.path.insert(1, directory)
import logging
")))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list cairo
             gdk-pixbuf
             gobject-introspection
             gtk+
             python-pygobject
             sugar-toolkit-gtk3))
      (inputs (list python-pygame))
      (native-inputs (list gettext-minimal python-setuptools))
      (home-page "https://github.com/sugarlabs/sugar-commander")
      (synopsis "Manage your Sugar journal")
      (description
       "Sugar-commander lets you import items from removable devices like USB
drives and SD cards using a familiar hierarchical view of files on these
devices, as opposed to the flattened Journal view that the Sugar Journal gives
to these devices.  It also enables you to see how much disk space each Journal
entry uses, generates thumbnails, and does other things to enhance your use of
the Journal.")
      (license license:gpl2+))))

(define-public sugar-help-activity
  (let ((commit "3aa03e300e5bf30afbee529ce5130402a8833640")
        (revision "2"))
    (package
      (name "sugar-help-activity")
      (version (git-version "20" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/help-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "19xs2633341h6xwgzss7pv2i46vxa44qic5bbxqb47yfkdmd4w0w"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'use-newer-webkit
              (lambda _
                (substitute* "helpactivity.py"
                  (("'WebKit2', '4.0'")
                   "'WebKit2', '4.1'"))))
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (replace 'build
              (lambda _
                (invoke "make" "html")))
            (replace 'install
              (lambda _
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs
       (list sugar-toolkit-gtk3 python-sphinx python-setuptools))
      (propagated-inputs (list webkitgtk-for-gtk3))
      (home-page "https://github.com/sugarlabs/help-activity")
      (synopsis "Sugar activity for accessing documentation and manuals")
      (description
       "This is an activity for the Sugar environment which aims to provide
users with easy access to documentation and manuals.")
      (license license:gpl3+))))

(define-public sugar-jukebox-activity
  ;; The last tag release was in 2020 and since then commits have been
  ;; published that include translation updates.
  (let ((commit "787c44927883dccc9bb19324e332d8dd67054387")
        (revision "3"))
    (package
      (name "sugar-jukebox-activity")
      (version (git-version "36" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/jukebox-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0g9naqpkmyla4fpvs9g699yv2nrrr96xld8ljj8ylmmgymj8wi66"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gtk+
             gstreamer
             gst-plugins-base
             sugar-toolkit-gtk3))
      (inputs (list gettext-minimal))
      (home-page "https://help.sugarlabs.org/jukebox.html")
      (synopsis "Media player for the Sugar learning environment")
      (description
       "Jukebox is the media player to play different kinds of audio and video
files including online streams.  It also supports playlists like @file{.m3u}
and @file{.pls}.")
      (license license:gpl2+))))

(define-public sugar-log-activity
  (let ((commit "0c45118958be14a1844456703f0b392de250bc88")
        (revision "1"))
    (package
      (name "sugar-log-activity")
      (version (git-version "42" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/log-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0pacd677gfhyym153x5grwimk8wgm4c9k0a463pq6fdrhm1g5wpc"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-locations
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "logcollect.py"
                  (("'/sbin/ifconfig'")
                   (string-append "'"
                                  (search-input-file inputs "/bin/ifconfig")
                                  "'"))
                  (("'/sbin/route")
                   (string-append "'"
                                  (search-input-file inputs "/bin/route")))
                  (("'/bin/df")
                   (string-append "'"
                                  (search-input-file inputs "/bin/df")))
                  (("'/bin/ps")
                   (string-append "'"
                                  (search-input-file inputs "/bin/ps")))
                  (("'/usr/bin/free")
                   (string-append "'"
                                  (search-input-file inputs "/bin/free")))
                  (("'/usr/bin/top")
                   (string-append "'"
                                  (search-input-file inputs "/bin/top")))
                  (("'/usr/share/sugar/activities")
                   "'/run/current-system/profile/share/sugar/activities"))))
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs (list gtk+ sugar-toolkit-gtk3))
      (inputs (list coreutils net-tools procps))
      (native-inputs (list gettext-minimal python-setuptools))
      (home-page "https://help.sugarlabs.org/log.html")
      (synopsis "Log activity for the Sugar learning environment")
      (description
       "Log is part of the Sugar desktop.  Log is used when
looking for why an activity or Sugar is not working properly.")
      (license license:gpl2+))))

(define-public sugar-maze-activity
  (package
    (name "sugar-maze-activity")
    (version "32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sugarlabs/maze-activity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0506mwxy3agyxlilb5v3pn29pg45lzaxm8rhj9azm58irs3wdmnq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "check")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-launcher
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "activity/activity.info"
                (("exec = sugar-activity3")
                 (string-append "exec = "
                                (search-input-file inputs
                                                   "/bin/sugar-activity3"))))))
          (delete 'build)
          (replace 'install
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "install"
                      (string-append "--prefix=" #$output)))))))
    (native-inputs (list python-setuptools))
    ;; All these libraries are accessed via gobject introspection.
    (propagated-inputs (list gtk+ telepathy-glib))
    (inputs (list sugar-toolkit-gtk3 gettext-minimal))
    (home-page "https://github.com/sugarlabs/maze-activity")
    (synopsis "Simple maze game for the Sugar learning environment")
    (description "Try to make your way through an increasingly difficult path,
or you can also play with a friend!")
    (license license:gpl3+)))

(define-public sugar-physics-activity
  ;; New commits includes fixes and translation updates
  (let ((commit "d3b739667ac94af3f961ced12aed9b20a0129571")
        (revision "2"))
    (package
      (name "sugar-physics-activity")
      (version (git-version "35" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/physics")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "06g61bd1dpqpniv211sbv40v5drp1jg5h2m24yp5pspsydlpb84p"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (add-after 'unpack 'inject-load-path
              (lambda _
                (substitute* "activity.py"
                  (("^import os")
                   (string-append "\
import sys, os
for directory in \"" (getenv "GUIX_PYTHONPATH") "\".split(\":\"):
    try:
        sys.path.index(directory)
    except ValueError:
        sys.path.insert(1, directory)
")))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs (list gtk+ gdk-pixbuf))
      (inputs (list python-pybox2d python-pygame sugar-toolkit-gtk3
                    gettext-minimal))
      (home-page "https://github.com/sugarlabs/physics")
      (synopsis "Physical world simulator and playground")
      (description
       "Physics is a physical world simulator and playground---you can add
squares, circles, triangles, or draw your own shapes, and see them come to
life with forces (think gravity, Newton!), friction (scrrrrape), and inertia
(ahh, slow down!).")
      (license license:gpl3+))))

(define-public sugar-portfolio-activity
  ;; New commits includes translation updates
  (let ((commit "e25eb768e194de4a3197a434d88d31d59f897a3d")
        (revision "2"))
    (package
      (name "sugar-portfolio-activity")
      (version (git-version "52" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/portfolio-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0s3kpl9923g3365p2fqq7m3738rmbzx2n11xs08lnzd3j04qhihh"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f ;there are none
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list cairo
             pango
             gdk-pixbuf
             gobject-introspection
             gtk+
             gstreamer
             gst-plugins-base
             python-dbus-1.2
             python-pygobject
             sugar-datastore
             sugar-toolkit-gtk3
             telepathy-glib))
      (inputs (list gettext-minimal))
      (home-page "https://github.com/sugarlabs/portfolio-activity")
      (synopsis "Portfolio for the Sugar Journal")
      (description "The Portfolio activity creates a slide show from Sugar
Journal entries that have been ‘starred’.")
      (license license:gpl3+))))

(define-public sugar-read-activity
  (package
    (name "sugar-read-activity")
    (version "124")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://download.sugarlabs.org/sources/sucrose/fructose/"
             "Read/Read-" version ".tar.bz2"))
       (sha256
        (base32 "1hla80vclprqzahr8yfnin09spv4mab7il6a00ilz4anyahrzgzy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "check")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-launcher
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "activity/activity.info"
                (("exec = sugar-activity3")
                 (string-append "exec = "
                                (search-input-file inputs
                                                   "/bin/sugar-activity3"))))))
          (delete 'build)
          (replace 'install
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "install"
                      (string-append "--prefix=" #$output)))))))
    (native-inputs (list python-setuptools))
    ;; All these libraries are accessed via gobject introspection.
    (propagated-inputs
     (list evince
           gtk+
           sugar-toolkit-gtk3
           webkitgtk-for-gtk3))
    (inputs (list gettext-minimal))
    (home-page "https://help.sugarlabs.org/read.html")
    (synopsis "Read PDF and TXT files in the Sugar learning environment")
    (description
     "The Read activity allows the laptop to act as a book reader.  It has a
simple interface, and will view many kinds of text and image-based book-like
materials.  It will have particular strengths in handheld mode, with extremely
low power consumption and simple navigation controls.")
    (license license:gpl2+)))

(define-public sugar-river-crossing-activity
  ;; The last tag release was in 2024 and since then commits have been
  ;; published that include fixes.
  (let ((commit "a108365b43c8b6542585e876d260196639beee22")
        (revision "2"))
    (package
      (name "sugar-river-crossing-activity")
      (version (git-version "1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/river-crossing-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0sm389y1qas00pn7jqplgsswd8nwry4nx71nqls589mn0lglrcj5"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (add-after 'unpack 'inject-load-path
              (lambda _
                (substitute* "activity.py"
                  (("^import pygame")
                   (string-append "\
import sys
for directory in \"" (getenv "GUIX_PYTHONPATH") "\".split(\":\"):
    try:
        sys.path.index(directory)
    except ValueError:
        sys.path.insert(1, directory)
import pygame
")))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; These libraries are accessed via gobject introspection.
      (propagated-inputs (list gtk+))
      (inputs (list python-pygame sugar-toolkit-gtk3 gettext-minimal))
      (home-page "https://github.com/sugarlabs/river-crossing-activity")
      (synopsis "Puzzle game for Sugar desktop")
      (description
       "A farmer is to ferry across a river a goat, a cabbage, and a wolf.
The boat allows the farmer to carry only one of the three at a time. Without
supervision, the goat will gobble the cabbage whereas the wolf will not
hesitate to feast on the goat.")
      (license license:gpl3+))))

(define-public sugar-terminal-activity
  ;; New commits includes fixes and translation updates
  (let ((commit "e810f0397b6d105642d8653af41e7dc909b3302e")
        (revision "2"))
    (package
      (name "sugar-terminal-activity")
      (version (git-version "47" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/terminal-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ahmv6ynand6xww4w2p2654hmm4k2kmsdjb73xlwsa4b5ss224av"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs (list gtk+ vte/gtk+-3 sugar-toolkit-gtk3))
      (inputs (list gettext-minimal))
      (home-page "https://help.sugarlabs.org/terminal.html")
      (synopsis "Terminal activity for the Sugar learning environment")
      (description "Terminal is a full-screen text mode program that provides
a Command-Line Interface (CLI) to the system.")
      (license (list license:gpl2+ license:gpl3+)))))

(define-public sugar-turtleart-activity
  ;; The last tag release was in 2020 and since then commits have been
  ;; published that include fixes and translation updates.
  (let ((commit "35541bfa7f3e0ba747901702868c44036c825371")
        (revision "2"))
    (package
      (name "sugar-turtleart-activity")
      (version (git-version "220" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/turtleart-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10214hahq76p187yp5r46hvsfw88blhym2jm89s9v4d0azp4la55"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (add-after 'unpack 'patch-locations
              (lambda _
                (substitute* "setup.py"
                  (("'/usr/share/applications")
                   "'share/applications"))))
            (add-after 'unpack 'patch-tool-references
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* '("TurtleArtActivity.py"
                               "TurtleArt/turtleblocks.py")
                  (("glib-compile-schemas")
                   (search-input-file inputs "/bin/glib-compile-schemas")))
                (substitute* '("plugins/turtle_blocks_extras/turtle_blocks_extras.py"
                               "pysamples/speak.py"
                               "TurtleArt/tacollaboration.py")
                  (("'espeak")
                   (string-append "'"
                                  (search-input-file inputs "/bin/espeak"))))
                (substitute* '("pysamples/csound.py"
                               "plugins/turtle_blocks_extras/turtle_blocks_extras.py")
                  (("'csound '")
                   (string-append "'"
                                  (search-input-file inputs "/bin/csound")
                                  " '")))
                (substitute* '("plugins/turtle_blocks_extras/turtle_blocks_extras.py"
                               "pysamples/speak.py"
                               "TurtleArt/tacollaboration.py")
                  (("\\| aplay")
                   (string-append "| "
                                  (search-input-file inputs "/bin/aplay"))))
                (substitute* "pysamples/sinewave.py"
                  (("'speaker-test")
                   (string-append "'"
                                  (search-input-file inputs
                                                     "/bin/speaker-test"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list gstreamer
             gtk+
             telepathy-glib
             webkitgtk-for-gtk3))
      (inputs
       (list alsa-utils
             csound
             espeak
             (list glib "bin")
             gettext-minimal
             sugar-toolkit-gtk3))
      (home-page "https://help.sugarlabs.org/en/turtleart.html")
      (synopsis "Block-based Logo programming environment")
      (description
       "Turtle Art, also known as Turtle Blocks, is an activity with a
Logo-inspired graphical “turtle” that draws colorful art based on
snap-together visual programming elements.  Its “low floor” provides an easy
entry point for beginners.  It also has “high ceiling” programming,
graphics,mathematics, and Computer Science features which will challenge the
more adventurous student.")
      (license license:expat))))

(define-public sugar-turtlepond-activity
  (let ((commit "e460fc472d2f900c4c71659dbec07a715a3847a7")
        (revision "1"))
    (package
      (name "sugar-turtlepond-activity")
      (version (git-version "10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/turtlepond")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0j7jzbwi2aph312f5dazmwgxqzh458b4yzz8mvrdxpr91ksxd4h4"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            #;
            (add-after 'unpack 'inject-load-path
              (lambda _
                (substitute* "TurtlePondActivity.py"
                  (("^import logging")
                   (string-append "\
import sys
for directory in \"" (getenv "GUIX_PYTHONPATH") "\".split(\":\"):
    try:
        sys.path.index(directory)
    except ValueError:
        sys.path.insert(1, directory)
import logging
")))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list cairo
             gdk-pixbuf
             gobject-introspection
             gtk+
             python-pygobject
             sugar-toolkit-gtk3))
      (native-inputs (list gettext-minimal python-setuptools))
      (home-page "https://github.com/sugarlabs/turtlepond")
      (synopsis "Turtle-based strategy game")
      (description "Turtle in a Pond is a strategy game.  The goal is to
surround the turtle before it runs off the screen.")
      (license license:gpl2+))))

(define-public sugar-typing-turtle-activity
  (package
    (name "sugar-typing-turtle-activity")
    (version "32")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sugarlabs/typing-turtle-activity")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0shadv9wgddjvl97kvsqb8iw1wmmfw5lzcqk78hd70pzvh4c1hmd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "check")
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
                                (search-input-file inputs
                                                   "/bin/sugar-activity3"))))))
          (delete 'build)
          (replace 'install
            (lambda _
              (invoke "python" "setup.py" "install"
                      (string-append "--prefix=" #$output)))))))
    (native-inputs
     (list gettext-minimal sugar-toolkit-gtk3 python-setuptools))
    (inputs (list setxkbmap))
    (home-page "https://help.sugarlabs.org/en/typing_turtle.html")
    (synopsis "Learn typing")
    (description
     "Need some help typing?  In this activity for the Sugar environment you
will learn the best way to hold your hands in order for you to become a faster
typist.")
    (license license:gpl3+)))

(define-public sugar-write-activity
  ;; The last tag release was in 2019 and since then commits have been
  ;; published that include fixes and translation updates.
  (let ((commit "bb4417e3a42c62fab4ff86cf5b12c65267d55912")
        (revision "2"))
    (package
      (name "sugar-write-activity")
      (version (git-version "101" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/sugarlabs/write-activity")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "113l5ava25v36lrxgmc2krhihpna1y2r4wyj8k86a24jr7xv3fji"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:test-flags
        #~(list "check")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-launcher
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "activity/activity.info"
                  (("exec = sugar-activity3")
                   (string-append "exec = "
                                  (search-input-file inputs
                                                     "/bin/sugar-activity3"))))))
            (delete 'build)
            (replace 'install
              (lambda _
                (setenv "HOME" "/tmp")
                (invoke "python" "setup.py" "install"
                        (string-append "--prefix=" #$output)))))))
      (native-inputs (list python-setuptools))
      ;; All these libraries are accessed via gobject introspection.
      (propagated-inputs
       (list abiword
             gdk-pixbuf
             gtk+
             libgsf
             telepathy-glib))
      (inputs (list sugar-toolkit-gtk3 gettext-minimal))
      (home-page "https://help.sugarlabs.org/write.html")
      (synopsis "Word processor for Sugar desktop")
      (description
       "Write is a word processor activity for the Sugar desktop. Write embeds
the AbiWord word processor, and can be used to write and edit text
documents.")
      (license license:gpl2+))))
