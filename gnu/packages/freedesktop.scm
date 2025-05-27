;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2017 Andy Wingo <wingo@pobox.com>
;;; Copyright © 2015-2017, 2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017, 2018, 2019, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 David Hashe <david.hashe@dhashe.com>
;;; Copyright © 2016, 2017, 2019, 2021-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2017, 2018, 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2020, 2021, 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Stefan Stefanović <stefanx2ovic@gmail.com>
;;; Copyright © 2019 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2019, 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020 Rene Saavedra <pacoon@protonmail.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 Anders Thuné <asse.97@gmail.com>
;;; Copyright © 2020 Raghav Gururajan <raghavgururajan@disroot.org>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Robby Zambito <contact@robbyzambito.me>
;;; Copyright © 2021, 2022, 2023 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2022, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2021-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Daniel Meißner <daniel.meissner-i4k@ruhr-uni-bochum.de>
;;; Copyright © 2022 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022 muradm <mail@muradm.net>
;;; Copyright © 2023 Alex Devaure <ajadevaure@gmail.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2022 Samuel Culpepper <sculpepper@newstore.com>
;;; Copyright © 2024 aurtzy <aurtzy@gmail.com>
;;; Copyright © 2024 Dariqq <dariqq@posteo.net>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 dan <i@dan.games>
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

(define-module (gnu packages freedesktop)
  #:use-module (guix bzr-download)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages file)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)                ;intltool
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages rdesktop)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages samba)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages video)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages w3m)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public appstream
  (package
    (name "appstream")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.freedesktop.org/software/"
                       "appstream/releases/"
                       "AppStream-" version ".tar.xz"))
       (sha256
        (base32 "195snvg2jw5ywqxz02xfb570yhxvaqp9d4w5a2lpay2fck7zddjs"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags #~(list "-Dsystemd=false")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-libstemmer
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((libstemmer.h (search-input-file inputs
                                                     "include/libstemmer.h")))
                (substitute* "meson.build"
                  (("/usr/include")
                   (dirname libstemmer.h))))))
          (add-before 'check 'check-setup
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list docbook-xml-4.3
           docbook-xml
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gperf
           gtk-doc/stable
           itstool
           libxslt
           pkg-config
           python-wrapper
           gi-docgen))
    (inputs
     (list curl libsoup-minimal-2 libstemmer libxmlb libxml2 libyaml lmdb))
    (propagated-inputs
     (list glib))
    (synopsis "Tools and libraries to work with AppStream metadata")
    (description "AppStream is a cross-distribution effort for enhancing the way
we interact with the software repositories provided by distributions by
standardizing software component metadata.  It provides the foundation to build
software-center applications, by providing metadata necessary for an
application-centric view on package repositories.  It additionally provides
specifications for things like an unified software metadata database, screenshot
services and various other things needed to create user-friendly
application-centers for distributions.")
    (home-page "https://www.freedesktop.org/wiki/Distributions/AppStream/")
    (license license:lgpl2.1+)))

(define-public appstream-qt
  (package/inherit appstream
    (name "appstream-qt")
    (native-inputs
     (modify-inputs (package-native-inputs appstream)
       (prepend qttools-5)))
    (inputs
     (modify-inputs (package-inputs appstream)
       (prepend qtbase-5)))
    (arguments
     (substitute-keyword-arguments (package-arguments appstream)
       ((#:configure-flags flags #~'())
        #~(append '("-Dqt=true" "-Dqt-versions=5") #$flags))))))

(define-public appstream-qt6
  (package/inherit appstream
    (name "appstream-qt6")
    (native-inputs
     (modify-inputs (package-native-inputs appstream)
       (prepend qttools)))
    (inputs
     (modify-inputs (package-inputs appstream)
       (prepend qtbase)))
    (arguments
     (substitute-keyword-arguments (package-arguments appstream)
       ((#:configure-flags flags #~'())
        #~(append '("-Dqt=true" "-Dqt-versions=6") #$flags))))))

(define-public farstream
  (package
    (name "farstream")
    (version "0.2.9")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/farstream/farstream.git")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sd8syldyq6bphfdm129s3gq554vfv7vh1vcwzk48gjryf101awk"))
       (patches
        (search-patches "farstream-gupnp.patch" ;for test 'transmitter/rawudp'
                        "farstream-make.patch"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-gtk-doc"
              "--enable-glib-asserts"
              (string-append "--with-html-dir=" #$output
                             "/share/gtk-doc/html"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-common
            (lambda _
              (delete-file "autogen.sh")
              (copy-recursively
               #$(this-package-native-input
                  (git-file-name "common" "latest.52adcdb"))
               "common")))
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (substitute* "tests/check/Makefile.am"
                ;; This test fails since updating gstreamer to version 1.22.1
                ;; (see:
                ;; https://gitlab.freedesktop.org/farstream/farstream/-/issues/25).
                (("^\trtp/recvcodecs.*") "")
                ;; This test timeouts despite changing the value of
                ;; 'CK_DEFAULT_TIMEOUT' to 600 (see:
                ;; https://gitlab.freedesktop.org/farstream/farstream/-/issues/20).
                (("^\ttransmitter/nice.*") "")))))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml-4.1.2
           docbook-xsl
           gobject-introspection
           gtk-doc/stable
           libtool
           libxslt
           perl
           pkg-config
           python-wrapper
           (origin
             (method git-fetch)
             (uri
              (git-reference
               (url "https://gitlab.freedesktop.org/gstreamer/common.git")
               (commit "52adcdb89a9eb527df38c569539d95c1c7aeda6e")))
             (file-name (git-file-name "common" "latest.52adcdb"))
             (sha256
              (base32
               "1zlm1q1lgcb76gi82rial5bwy2j9sz1x6x48ijhiz89cml7xxd1r")))))
    (inputs
     (list glib
           gtk+
           gupnp-igd
           libnice))
    (propagated-inputs
     (list gstreamer
           gst-plugins-bad
           gst-plugins-base
           gst-plugins-good))
    (synopsis "The Farstream VVoIP framework")
    (description "Farstream is a collection of GStreamer modules and libraries
for videoconferencing.")
    (home-page "https://www.freedesktop.org/wiki/Software/Farstream/")
    (license license:lgpl2.1+)))

(define-public libglib-testing
  (package
    (name "libglib-testing")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/pwithnall/libglib-testing.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xmycsrlqyji6sc2i4wvp2gxf3897z65a57ygihfnpjpyl7zlwkr"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before
             'check 'pre-check
           (lambda _
             ;; The test suite requires a running dbus-daemon.
             (system "dbus-daemon &")
             ;; Don't fail on missing '/etc/machine-id'.
             (setenv "DBUS_FATAL_WARNINGS" "0")
             #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("gtk-doc" ,gtk-doc/stable)))
    (inputs
     (list dbus glib))
    (synopsis "Glib testing library")
    (description "Libglib-testing is a test library providing test harnesses and
mock classes which complement the classes provided by GLib.  It is intended to
be used by any project which uses GLib and which wants to write internal unit
tests.")
    (home-page "https://gitlab.gnome.org/pwithnall/libglib-testing")
    (license license:lgpl2.1+)))

(define-public libsfdo
  (package
    (name "libsfdo")
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/vyivel/libsfdo")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fi9hlg9ms8sszb0ylx4v0q49265vbsix455x64nkvklh049yc7n"))))
    (build-system meson-build-system)
    (home-page "https://gitlab.freedesktop.org/vyivel/libsfdo")
    (synopsis "Implementation of some of the freedesktop.org specifications")
    (description "libsfdo is a collection of libraries which implement
some of the freedesktop.org specifications.")
    (license license:bsd-2)))

(define-public libliftoff
  (package
    (name "libliftoff")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/emersion/libliftoff")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "163g8ndsbma7acy2k9mrnvlpb7yi4431hgkx1gygkafgwpq1ii1x"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libdrm))
    (home-page "https://gitlab.freedesktop.org/emersion/libliftoff")
    (synopsis "Lightweight KMS plane library for compositors")
    (description "Libliftoff eases the use of
@acronym{KMS, Kernel Mode Setting} planes from userspace.  Users create
\"virtual planes\" called layers, set KMS properties on them, and libliftoff
will pick hardware planes for these layers if possible.")
    (license license:expat)))

(define-public malcontent
  (package
    (name "malcontent")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/pwithnall/malcontent.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g622ig5ffrzk9184xff3lardk7rnmkvj8y5g6h6s41bfh51b71m"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         ;; AppInfo not available inside build environment.
         (add-after 'unpack 'fix-tests
           (lambda _
             (substitute* "libmalcontent/tests/app-filter.c"
               (("g_test_add_func \\(\"/app-filter/appinfo\", test_app_filter_appinfo\\);")
                 "")))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           `(,gtk "bin")
           itstool
           libglib-testing
           libxml2
           pkg-config))
    (inputs
     (list accountsservice
           appstream
           appstream-glib
           dbus
           flatpak
           glib
           gtk
           libadwaita
           libostree
           linux-pam
           polkit))
    (synopsis "Parental controls support")
    (description "MalContent implements parental controls support which can
be used by applications to filter or limit the access of child accounts to
inappropriate content.")
    (home-page "https://gitlab.freedesktop.org/pwithnall/malcontent")
    (license
     (list
      license:gpl2+
      license:lgpl2.1+))))

(define-public maliit-framework
  (package
    (name "maliit-framework")
    (version "2.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/maliit/framework")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dkjxvfxg56hfy70j6ibfklfyv57jiha4vgc3ggl60r5kjx65s5b"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; The Ft_MIMPluginManager::testPluginDescriptions test fails
                ;; with a QFATAL error: received signal 11, while
                ;; ut_mimpluginmanager fails at least on powerpc64le with a
                ;; subprocess aborted error (see:
                ;; https://github.com/maliit/framework/issues/120).
                (invoke "ctest" "-E"
                        "(ft_mimpluginmanager|ut_mimpluginmanager)")))))))
    (native-inputs (list extra-cmake-modules
                         wayland-protocols
                         pkg-config
                         doxygen
                         graphviz
                         `(,glib "bin"))) ;for gdbus-codegen))
    (inputs (list qtbase-5
                  qtdeclarative-5
                  qtwayland-5
                  wayland
                  libxkbcommon
                  dbus
                  eudev
                  glib))
    (home-page "https://github.com/maliit/framework")
    (synopsis "Core libraries of Maliit")
    (description "This package provides Maliit provides a flexible input
method framework.")
    (license license:lgpl2.1+)))

(define-public maliit-keyboard
  (package
    (name "maliit-keyboard")
    (version "2.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/maliit/keyboard")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g89lckl4wzwamc89hs8871fbiyrsjwzk5b6ic4vhc4d1clyqzaw"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f
           #:phases #~(modify-phases %standard-phases
                        (add-after 'install 'install-schemas
                          (lambda* (#:key source outputs #:allow-other-keys)
                            (with-directory-excursion (string-append #$output
                                                       "/share/glib-2.0/schemas")
                              (invoke "glib-compile-schemas" ".")))))))
    (native-inputs (list extra-cmake-modules pkg-config gettext-minimal
                         `(,glib "bin")))
    (inputs (list hunspell
                  glib
                  libchewing
                  libpinyin
                  maliit-framework
                  presage
                  qtbase-5
                  qtdeclarative-5
                  qtmultimedia-5
                  qtquickcontrols2-5))
    (home-page "https://github.com/maliit/keyboard")
    (synopsis "Maliit Keyboard")
    (description
     "This package provides virtual keyboard for Wayland and X11
display servers.  It supports many different languages and emoji.")
    (license license:gpl3+)))

;; Private package used by shared-mime-info.
(define xdgmime
  ;; No public release, match commit to the one used in the
  ;; shared-mime-info release.
  (let ((commit "179296748e92bd91bf531656632a1056307fb7b7")
        (revision "2"))
    (package
      (name "xdgmime")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.freedesktop.org/xdg/xdgmime.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "04bpbqlkmwi2pqx1lj3awa9f9gwp4n91fpnz8hbbd0hl8x41przm"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f  ; no tests
        #:make-flags #~(list (string-append "DESTDIR=" #$output)
                             #$(string-append "CC=" (cc-for-target)))
        #:imported-modules `((guix build copy-build-system)
                             ,@%default-gnu-imported-modules)
        #:modules `((guix build gnu-build-system)
                    ((guix build copy-build-system) #:prefix copy:)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
            ;; Package uses a hand-crafted Makefile.
            (delete 'configure)
            (replace 'install
              (lambda args
                (apply (assoc-ref copy:%standard-phases 'install)
                       #:install-plan
                       '(("src" "bin/" #:include ("print-mime-data"
                                                  "test-mime-data"
                                                  "test-mime")))
                       args))))))
      (home-page "https://gitlab.freedesktop.org/xdg/xdgmime/")
      (synopsis "Module that parses the freedesktop.org MIME spec")
      (description "This module is used for shared-mime-info package tests.")
      (license (list license:lgpl2.1+ license:artistic2.0)))))

;; Note: when updating shared-mime-info, don't forget to update xdgmime's commit
;; to the one used in the release.
(define-public shared-mime-info
  (package
    (name "shared-mime-info")
    (version "2.3")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://gitlab.freedesktop.org/xdg/shared-mime-info.git")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "0w8sbhz00sk6k8pyiykfig4rm22jyibalj7g22j9qf3d2nfy8ivh"))
             (patches (search-patches "shared-mime-info-xdgmime-path.patch"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append
               "-Dxdgmime-path="
               (dirname
                (search-input-file %build-inputs "/bin/test-mime")))
              "-Dupdate-mimedb=true")
      #:phases
      #~(modify-phases %standard-phases
          ;; Don't patch shebangs for the test files.
          (replace 'patch-source-shebangs
            (lambda _
              (let ((pred (lambda (file stat)
                            (and (eq? 'regular (stat:type stat))
                                 (not (string-prefix? "./tests/mime-detection"
                                                      file))))))
                (for-each patch-shebang
                          (find-files "." pred #:stat lstat)))))
          ;; The docs have no install rule.
          (add-after 'install 'install-doc
            (lambda* (#:key source #:allow-other-keys)
              (let ((dest (string-append #$output:doc "/share/doc")))
                (with-directory-excursion "data/shared-mime-info-spec-html"
                  (install-file "shared-mime-info-spec.html"
                                (string-append dest "/html")))
                (install-file (string-append source
                                             "/data/shared-mime-info-spec.xml")
                              dest)))))))
    (inputs
     (list glib libxml2))
    (native-inputs
     (append
       (if (%current-target-system)
           (list libxml2 this-package)
           '())
       (list gettext-minimal pkg-config python xdgmime
             ;; For 'doc' output.
             docbook-xml-4.1.2 docbook-xsl xmlto)))
    (outputs (list "out" "doc"))
    (home-page "https://www.freedesktop.org/wiki/Software/shared-mime-info")
    (synopsis "Database of common MIME types")
    (description
     "The shared-mime-info package contains the core database of common types
and the update-mime-database command used to extend it.  It requires glib2 to
be installed for building the update command.  Additionally, it uses intltool
for translations, though this is only a dependency for the maintainers.  This
database is translated at Transifex.")
    (license license:gpl2+)))

(define-public xdg-utils
  (package
    (name "xdg-utils")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://portland.freedesktop.org/download/xdg-utils-"
             version ".tar.gz"))
       (sha256
        (base32
         "1nai806smz3zcb2l5iny4x7li0fak0rzmjg6vlyhdqm8z25b166p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list docbook-xsl docbook-xml-4.1.2
           libxslt xmlto w3m-for-tests))
    (inputs
     (list bash-minimal                 ;for 'wrap-program'
           coreutils
           file
           gawk
           grep
           inetutils                    ;xdg-screensaver uses `hostname'
           perl-file-mimeinfo           ;for mimeopen fallback
           sed
           xprop                        ;for Xfce detecting
           xset))                       ;for xdg-screensaver
    (arguments
     (list
      #:tests? #f                       ;no check target
      #:modules `((srfi srfi-26)
                  ,@%default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
        (add-after 'unpack 'patch-hardcoded-paths
          (lambda* (#:key inputs #:allow-other-keys)
            (substitute* "scripts/xdg-mime.in"
              (("/usr/bin/file")
               (search-input-file inputs "bin/file")))
            (substitute* "scripts/xdg-open.in"
              (("/usr/bin/printf")
               (search-input-file inputs "bin/printf")))))
        (add-after 'install 'wrap-executables
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let* ((dependencies '("awk" "grep" "hostname" "ls" "mimeopen"
                                   "sed" "xprop" "xset"))
                   (pkgs (map (lambda (cmd)
                                (search-input-file inputs
                                                   (string-append "bin/" cmd)))
                              dependencies))
                   (bindirs (map dirname pkgs)))
              (with-directory-excursion (string-append #$output "/bin")
                (for-each (cute wrap-program <>
                                `("PATH" ":" prefix ,bindirs))
                          (find-files ".")))))))))
    (home-page "https://www.freedesktop.org/wiki/Software/xdg-utils/")
    (synopsis "Freedesktop.org scripts for desktop integration")
    (description "The xdg-utils package is a set of simple scripts that
provide basic desktop integration functions in the framework of the
freedesktop.org project.")
    (license license:expat)))

(define-public libinput
  ;; Updating this will rebuild over 700 packages through libinput-minimal.
  (package
    (name "libinput")
    (version "1.26.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libinput/libinput.git")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zwwq7a0a6yznc6jxhp6gb50yw5vpfkvgbrabrpc5pwldpckfbrg"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Ddocumentation=false")

       ;; XXX: Using 'debug' or 'debugoptimized' pulls in an additional test that
       ;; hangs, and the comments around it suggests that we should be using this
       ;; Meson target anyway.
       #:build-type "release"))
    (native-inputs
     (append (list check pkg-config python-minimal-wrapper python-pytest)
             (if (%current-target-system)
               (list pkg-config-for-build)
               '())))
    (inputs
     (append (list cairo
                   glib
                   gtk+
                   libevdev
                   libwacom
                   mtdev)
             (if (%current-target-system)
               (list check)
               '())))
    (propagated-inputs
     ;; libinput.h requires <libudev.h>, so propagate it.
     (list eudev))
    (home-page "https://www.freedesktop.org/wiki/Software/libinput/")
    (synopsis "Input devices handling library")
    (description
     "Libinput is a library to handle input devices for display servers and
other applications that need to directly deal with input devices.")
    (license license:x11)))

(define-public libinput-minimal
  (package/inherit libinput
    (name "libinput-minimal")
    (inputs
     (fold alist-delete (package-inputs libinput)
           '("cairo" "glib" "gtk+" "libwacom")))
    (arguments
     (substitute-keyword-arguments (package-arguments libinput)
      ((#:configure-flags flags ''())
       `(cons* "-Dlibwacom=false"
               "-Ddebug-gui=false"    ;requires gtk+@3
               ,flags))))))

(define-public libei
  (package
    (name "libei")
    (version "1.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/libinput/libei.git")
                    (commit version)))
              (sha256
               (base32
                "04ll43616pyfm7c835azdggx9x3vfykpcg3pzmsfz4f2vl5whalm"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   ;; Unbundle munit, we provide it as input.
                   (substitute* "test/meson.build"
                     (("subproject\\('munit'")
                      "# subproject('munit'")
                     ((", fallback: \\['munit', 'munit_dep'\\]")
                      ""))
                   (delete-file-recursively "subprojects")))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~'("-Ddocumentation=api" ;protocol requires hugo
                            "-Dsd-bus-provider=libelogind")))
    (inputs
     (list elogind libevdev libxkbcommon))
    (propagated-inputs
     ;; liboeffis-1.0.pc requires.private libelogind
     (list elogind))
    (native-inputs
     (list dbus
           doxygen
           libxml2
           munit
           pkg-config
           python
           python-attrs
           python-dbusmock
           python-jinja2
           python-pytest
           python-structlog
           python-pyaml
           valgrind/interactive))
    (home-page "https://libinput.pages.freedesktop.org/libei/")
    (synopsis "Emulated Input protocol implementation")
    (description
     "Libei provides a client and server implementation of the @acronym{EI,
Emulated Input} protocol for Wayland compositors.")
    (license license:x11)))

(define-public libxdg-basedir
  (package
    (name "libxdg-basedir")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/devnev/libxdg-basedir")
                     (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j8fgp41kxipzdnqsdy83d7w6kadbc45n98qyr84zsj46wl582vv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-autogen
           (lambda _
             ;; Run 'configure' in its own phase, not now.
             (substitute* "autogen.sh"
               (("^.*\\./configure.*") ""))
             #t)))))
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://github.com/devnev/libxdg-basedir")
    (synopsis "Implementation of the XDG Base Directory specification")
    (description
     "libxdg-basedir is a C library providing some functions to use with
the freedesktop.org XDG Base Directory specification.")
    (license license:expat)))

(define-public elogind
  (package
    (name "elogind")
    (version "255.17")
    (replacement elogind/fixed)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elogind/elogind")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cb6p559281dzh24is91v6d4v4kz45yhyizibi4sfql9nign865h"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       ,#~(let* ((out #$output)
                 (sysconf (string-append out "/etc"))
                 (libexec (string-append out "/libexec/elogind"))
                 (dbus-data (string-append out "/share/dbus-1"))
                 (dbuspolicy (string-append dbus-data "/system.d"))
                 (dbussystemservice (string-append dbus-data
                                                   "/system-services"))
                 #$@(if (not (target-riscv64?))
                        #~((kexec-tools #$(this-package-input "kexec-tools")))
                        #~())
                 (shadow #$(this-package-input "shadow"))
                 (shepherd #$(this-package-input "shepherd"))
                 (halt-path (string-append shepherd "/sbin/halt"))
                 #$@(if (not (target-riscv64?))
                        #~((kexec-path (string-append kexec-tools "/sbin/kexec")))
                        #~())
                 (nologin-path (string-append shadow "/sbin/nologin"))
                 (poweroff-path (string-append shepherd "/sbin/shutdown"))
                 (reboot-path (string-append shepherd "/sbin/reboot")))
            (list
             "-Dmode=release"
             (string-append "-Dlibexecdir=" libexec)
             (string-append "-Dsysconfdir=" sysconf)
             (string-append "-Ddbuspolicydir=" dbuspolicy)
             (string-append "-Ddbussystemservicedir=" dbussystemservice)
             (string-append "-Dc_link_args=-Wl,-rpath=" libexec)
             (string-append "-Dcpp_link_args=-Wl,-rpath=" libexec)
             (string-append "-Dhalt-path=" halt-path)
             #$@(if (not (target-riscv64?))
                    #~((string-append "-Dkexec-path=" kexec-path))
                    #~())
             (string-append "-Dpoweroff-path=" poweroff-path)
             (string-append "-Dreboot-path=" reboot-path)
             (string-append "-Dnologin-path=" nologin-path)
             "-Dcgroup-controller=elogind"
             "-Dman=enabled"
             ;; Disable some tests.
             "-Dslow-tests=false"
             ;; Adjust the default user shell to /bin/sh (otherwise it is set
             ;; to /bin/bash).
             "-Ddefault-user-shell=/bin/sh"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tzdata
           (lambda* (#:key native-inputs inputs #:allow-other-keys)
             (substitute* "src/basic/time-util.c"
               (("/usr/share/zoneinfo")
                (search-input-directory (or native-inputs inputs)
                                        "share/zoneinfo")))))
         (add-after 'unpack 'do-not-install-empty-/var/log/elogind-dir
           (lambda _
             ;; This is the elogind state directory, which is not writable in
             ;; the build environment.
             (substitute* "meson.build"
               (("install_emptydir\\(elogindstatedir)") ""))))
         (add-after 'unpack 'fix-pkttyagent-path
           (lambda _
             (substitute* "meson.build"
               (("join_paths\\(bindir, 'pkttyagent'\\)")
                "'\"/run/current-system/profile/bin/pkttyagent\"'"))))
         (add-after 'unpack 'use-global-hook-directory
           ;; XXX There is no run-time setting to set this per-process, only a
           ;; build-time, hard-coded list of global directories.
           (lambda _
             (substitute* (list "src/login/logind-core.c"
                                "src/login/logind-dbus.c"
                                "src/sleep/sleep.c"
                                "src/shared/sleep-config.c")
               (("PKGSYSCONFDIR") "\"/etc/elogind\""))))
         (add-after 'unpack 'adjust-tests
           (lambda _
             ;; A few tests expect /var/tmp to exists, but it doesn't in the
             ;; container.
             (substitute* '("src/test/test-xattr-util.c"
                            "src/test/test-copy.c"
                            "src/test/test-fs-util.c")
               (("/var/tmp/")
                "/tmp/"))
             (substitute* "src/test/test-xattr-util.c"
               ;; The xattr-util test depends on /usr; patch it to use /tmp
               ;; instead.
               (("fd, \"usr\", \"user.idontexist\"")
                "fd, \"/tmp\", \"user.idontexist\""))
             (substitute* '("src/test/test-chase.c"
                            "src/test/test-fd-util.c")
               ;; Many checks use /usr, which doesn't exist in our
               ;; environment.
               (("/usr")
                "/tmp")
               (("\"usr\"")
                "\"tmp\""))
             (substitute* "src/test/meson.build"
               ;; Requires cgroup support.
               ((".*'test-cgroup-util\\.c'.*") "")
               ;; Requires privilege to create mount namespaces.
               ((".*'test-mountpoint-util\\.c'.*") ""))
             (substitute* "src/libelogind/meson.build"
               ;; The bus-creds test fails due to requiring cgroups.
               ((".*'sd-bus/test-bus-creds.c'.*") "")
               ;; The login test fails due to 'sd_pid_get_slice' returning
               ;; NULL.
               ((".*'sd-login/test-login.c'.*") "")
               ;; The sd-device test fails due to 'devname_from_devnum'
               ;; returning NULL.
               ((".*'sd-device/test-sd-device.c'.*") ""))
             ;; This test tries to copy some bytes from /usr/lib/os-release,
             ;; which does not exist in the build container.  Choose something
             ;; more likely to be available.
             (substitute* "src/test/test-copy.c"
               (("/usr/lib/os-release")
                "/etc/passwd"))
             ;; Use a shebang that works in the build container.
             (substitute* "src/test/test-exec-util.c"
               (("#!/bin/sh")
                (string-append "#!" (which "sh"))))
             ;; Do not look for files or directories that do not exist.
             (substitute* "src/test/test-fs-util.c"
               (("usr") "etc")
               (("/etc/machine-id") "/etc/passwd"))
             ;; FIXME: Why is sd_id128_get_machine_app_specific failing.
             ;; Disable for now by hooking into the kernel support check.
             (substitute* "src/test/test-id128.c"
               (("if \\(r == -EOPNOTSUPP\\)")
                "if (1)"))
             ;; This test expects that /sys is available.
             (substitute* "src/test/test-mountpoint-util.c"
               (("assert_se\\(path_is_mount_point\\(\"/sys.*")
                ""))
             ;; /bin/sh does not exist in the build container.
             (substitute* "src/test/test-path-util.c"
               (("/bin/sh") (which "sh")))
             ;; This test uses sd_device_new_from_syspath to allocate a
             ;; loopback device, but that fails because /sys is unavailable.
             (substitute* "src/libelogind/sd-device/test-sd-device-thread.c"
               ((".*sd_device_new_from_syspath.*/sys/class/net/lo.*")
                "return 77;"))))
         (add-after 'unpack 'change-pid-file-path
           (lambda _
             (substitute* "src/login/elogind.c"
               (("\"/run/elogind.pid\"") "\"/run/systemd/elogind.pid\"")))))))
    (native-inputs
     (list docbook-xml-4.5
           docbook-xml-4.2
           docbook-xsl
           gettext-minimal
           gperf
           pkg-config
           python
           python-jinja2
           libxslt
           tzdata))
    (inputs
     (append
      (if (not (target-riscv64?))
          (list kexec-tools)
          '())
      (list linux-pam
            libcap
            libxcrypt
            `(,util-linux "lib")        ;for 'libmount'
            shadow                      ;for 'nologin'
            shepherd                    ;for 'halt' and 'reboot', invoked
                                        ;when pressing the power button
            dbus
            eudev
            acl)))             ; to add individual users to ACLs on /dev nodes
    (home-page "https://github.com/elogind/elogind")
    (synopsis "User, seat, and session management service")
    (description "Elogind is the systemd project's \"logind\" service,
extracted out as a separate project.  Elogind integrates with PAM to provide
the org.freedesktop.login1 interface over the system bus, allowing other parts
of a the system to know what users are logged in, and where.")
    (license license:lgpl2.1+)))

(define-public elogind/fixed
  (hidden-package
   (package
     (inherit elogind)
     (arguments
      (substitute-keyword-arguments (package-arguments elogind)
        ((#:phases phases)
         #~(modify-phases #$phases
             (replace 'fix-pkttyagent-path
               (lambda _
                 (substitute* "meson.build"
                   (("bindir / 'pkttyagent'")
                    "'/run/current-system/profile/bin/pkttyagent'")))))))))))

(define-public basu
  (package
    (name "basu")
    (version "0.2.1")
    (home-page "https://git.sr.ht/~emersion/basu")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url home-page)
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "054mg6f9aqi0i3i3w8fc37qnns1vng3qq5b8nfd9g51wi8h891nc"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config python gperf))
    (propagated-inputs
     ;; Propagated because of pkg-config
     (list libcap))
    (synopsis "The sd-bus library, extracted from systemd")
    (description "Some projects rely on the sd-bus library for DBus support.
However not all systems have systemd or elogind installed.
This library provides just sd-bus (and the busctl utility).")
    (license license:lgpl2.1+)))

(define-public localed
  ;; XXX: This package is extracted from systemd but we retain so little of it
  ;; that it would make more sense to maintain a fork of the bits we need.
  (package
    (name "localed")
    (version "257.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/systemd/systemd")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "06fackmig43p9xx1155vrr5bx8a6a1cfb958x5acxvvahi8lkg7a"))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Don't insist on having systemd as PID 1 (otherwise
                  ;; 'localectl' would exit without doing anything.)
                  (substitute* "src/shared/bus-util.c"
                    (("sd_booted\\(\\)")
                     "(1)"))))
              (patches (search-patches "localed-xorg-keyboard.patch"))))
    (build-system meson-build-system)
    (arguments
     ;; Try to build as little as possible (list of components taken from the
     ;; top-level 'meson.build' file.)
     (let ((components '("utmp"
                         "hibernate"
                         "environment-d"
                         "binfmt"
                         "coredump"
                         "resolve"
                         "logind"
                         "hostnamed"
                         "localed"
                         "machined"
                         "portabled"
                         "networkd"
                         "timedated"
                         "timesyncd"
                         "firstboot"
                         "randomseed"
                         "backlight"
                         "vconsole"
                         "quotacheck"
                         "sysusers"
                         "tmpfiles"
                         "hwdb"
                         "rfkill"
                         "ldconfig"
                         "efi"
                         "tpm"
                         "ima"
                         "smack"
                         "gshadow"
                         "idn"
                         "nss-myhostname"
                         "nss-systemd")))
       (list
        #:configure-flags #~(list
                             #$@(map (lambda (component)
                                       (string-append "-D" component "=false"))
                                     (delete "localed" components)))

        ;; It doesn't make sense to test all of systemd.
        #:tests? #f

        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'set-xkeyboard-config-file-name
              (lambda _
                ;; Set the file name to xkeyboard-config and kbd.
                ;; This is used by 'localectl list-x11-keymap-layouts'
                ;; and similar functions.
                (let ((xkb #$(this-package-input "xkeyboard-config"))
                      (kbd #$(this-package-input "kbd")))
                  (substitute* "src/locale/localectl.c"
                    (("/usr/share/X11/xkb/rules")
                     (string-append xkb "/share/X11/xkb/rules")))
                  (substitute* "src/shared/kbd-util.c"
                    (("/usr/share/keymaps")
                     (string-append kbd "/share/keymaps"))))))
            (replace 'install
              (lambda _
                ;; Install 'localed', the D-Bus and polkit files, and
                ;; 'localectl'.
                (let* ((out #$output)
                       (libexec (string-append out "/libexec/localed"))
                       (bin     (string-append out "/bin"))
                       (lib     (string-append out "/lib"))
                       (dbus    (string-append out
                                               "/share/dbus-1/system-services"))
                       (conf    (string-append out
                                               "/etc/dbus-1/system.d/"))
                       (polkit  (string-append out
                                               "/share/polkit-1/actions"))
                       (data    (string-append out "/share/systemd")))
                  (define (source-file regexp)
                    (car (find-files ".." regexp)))

                  (mkdir-p libexec)
                  (copy-file "systemd-localed"
                             (string-append libexec "/localed"))
                  (install-file "localectl" bin)

                  (let ((service-file (source-file "\\.locale1\\.service$")))
                    (substitute* service-file
                      (("^Exec=.*$")
                       (string-append "Exec=" libexec "/localed\n")))
                    (install-file service-file dbus))
                  (install-file (source-file "\\.locale1\\.policy$") polkit)
                  (install-file (source-file "\\.locale1\\.conf$") conf)
                  (for-each (lambda (file)
                              (install-file file lib))
                            (find-files "src/shared"
                                        "libsystemd-shared.*\\.so"))

                  (for-each
                   (lambda (map)
                     (install-file map data))
                   (find-files
                    ".."
                    "^(kbd-model-map|language-fallback-map)$")))))))))
    (native-inputs
     (modify-inputs (package-native-inputs elogind)
       (append rsync)))
    (inputs
     (modify-inputs (package-inputs elogind)
       (prepend `(,util-linux "lib")
                kbd
                xkeyboard-config)))
    (home-page "https://www.freedesktop.org/wiki/Software/systemd/localed/")
    (synopsis "Control the system locale and keyboard layout")
    (description
     "Localed is a tiny daemon that can be used to control the system locale
and keyboard mapping from user programs.  It is used among other things by the
GNOME Shell.  The @command{localectl} command-line tool allows you to interact
with localed.  This package is extracted from the broader systemd package.")
    (license license:lgpl2.1+)))

(define-public packagekit
  (package
    (name "packagekit")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/"
                                  "PackageKit/releases/" "PackageKit-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "09md23m4fw87x264mls1f5isrswk6iw7y9g4hr1nib008wbbk370"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "contrib/meson.build"
                    (("bash_.*_dep\\.get_.*\\('completionsdir', .*\\)")
                     "join_paths(get_option('prefix'), 'share',
                                 'bash-completion', 'completions')"))))))
    (build-system meson-build-system)
    (arguments
     (list #:tests? #f
           #:configure-flags #~'("-Dsystemd=false" "-Doffline_update=false")))
    (native-inputs
     (list bash-completion
           docbook-xsl
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           libxslt
           pkg-config
           python-wrapper
           vala))
    (inputs
     (list glib
           gstreamer
           gst-plugins-base
           gtk+
           polkit))
    (propagated-inputs (list sqlite))
    (home-page "https://www.freedesktop.org/software/PackageKit/")
    (synopsis "API for package management, through D-Bus")
    (description
     "PackageKit provides a way of performing package management tasks,
e.g. updating, removing and installing software.  Through supporting many
backends, PackageKit can perform these tasks using the appropriate package
manager for the current system.")
    (license license:gpl2+)))

(define-public power-profiles-daemon
  (package
    (name "power-profiles-daemon")
    (version "0.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/upower/power-profiles-daemon")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08xz38r2fv6bpmv5vyjfvizwkbflg6m504fh3qd1jpw6xxv1lzwi"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:configure-flags #~(list "-Dsystemdsystemunitdir="
                                     "-Dpylint=disabled"
                                     "-Dgtk_doc=true"
                                     (string-append "-Dzshcomp=" #$output
                                                    "/share/zsh/site-functions/"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'correct-polkit-dir
                 (lambda _
                   (setenv "PKG_CONFIG_POLKIT_GOBJECT_1_POLICYDIR"
                           (string-append #$output "/share/polkit-1/actions"))))
               (add-after 'install 'wrap-program
                 (lambda _
                   (wrap-program
                       (string-append #$output "/bin/powerprofilesctl")
                     `("GUIX_PYTHONPATH" = (,(string-append
                                              #$(this-package-input "python-pygobject")
                                              "/lib/python"
                                              #$(version-major+minor
                                                 (package-version (this-package-input "python")))
                                              "/site-packages"))))))
               (add-after 'install 'move-docs
                 (lambda _
                   (mkdir-p (string-append #$output:doc "/share"))
                   (rename-file
                    (string-append #$output "/share/gtk-doc")
                    (string-append #$output:doc "/share/gtk-doc")))))))
    (native-inputs
     (list `(,glib "bin")
           gtk-doc/stable
           libxslt
           pkg-config
           python
           python-argparse-manpage
           python-dbusmock
           python-shtab
           umockdev))
    (inputs
     (list bash-minimal                 ;for 'wrap-program'
           bash-completion
           libgudev
           glib
           polkit
           python
           python-pygobject
           upower))
    (home-page "https://gitlab.freedesktop.org/upower/power-profiles-daemon")
    (synopsis "Power profile handling over D-Bus")
    (description
     "power-profiles-daemon offers to modify system behaviour based upon
user-selected power profiles.  There are 3 different power profiles, a
\"balanced\" default mode, a \"power-saver\" mode, as well as a
\"performance\" mode.  The first 2 of those are available on every system.
The \"performance\" mode is only available on select systems and is
implemented by different \"drivers\" based on the system or systems it
targets.  In addition to those 2 or 3 modes (depending on the system),
\"actions\" can be hooked up to change the behaviour of a particular device.
For example, this can be used to disable the fast-charging for some USB
devices when in power-saver mode.")
    (license license:gpl3)))


(define-public python-libevdev
  (package
    (name "python-libevdev")
    (version "0.11")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "libevdev" version))
              (sha256
               (base32
                "03snix86j0angq0lydp29f8833clxq8h0x4spmh8lj7j9mm01jp9"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-dlopen-calls
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "libevdev/_clib.py"
                (("libevdev.so.2")
                 (search-input-file inputs "lib/libevdev.so.2")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv" "test")))))))
    (native-inputs (list python-pytest))
    (inputs (list libevdev))
    (home-page "https://gitlab.freedesktop.org/libevdev/python-libevdev")
    (synopsis "Python wrapper for libevdev")
    (description "This package provides a Python wrapper around
@code{libevdev}, taking advantage of @code{libevdev}'s advanced event
handling.  Documentation is available at
@url{https://python-libevdev.readthedocs.io/en/latest/}.
@code{libevdev} makes it easy to:
@itemize
@item read and parse events from an input device;
@item create a virtual input device and make it send events;
@item duplicate an existing device and modify the event stream.
@end itemize
For information about libevdev, see:
@url{https://freedesktop.org/wiki/Software/libevdev/}.")
    (license license:expat)))

(define-public python-pyxdg
  (package
    (name "python-pyxdg")
    (version "0.27")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyxdg" version))
       (sha256
        (base32
         "19f5j5mxp7ff0vp33s32qbpdi65iiwha0bj641gl70pdwnm97gc0"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "XDG_DATA_DIRS"
                     (string-append (assoc-ref inputs "shared-mime-info")
                                    "/share/"))
             (substitute* "test/test-icon.py"
               (("/usr/share/icons/hicolor/index.theme")
                (string-append (assoc-ref inputs "hicolor-icon-theme")
                               "/share/icons/hicolor/index.theme")))

             ;; These two tests are known to fail in strange ways.
             (substitute* "test/test-mime.py"
               (("def test_get_type\\(self") "def _test_get_type(self")
               (("def test_get_type2\\(self") "def _test_get_type2(self"))

             ;; There are test files not shipped in the release tarball
             (substitute* "test/test-icon.py"
               (("def test_validate_icon_theme") "def _test_validate_icon_theme"))
             (invoke "nosetests" "-v"))))))
    (native-inputs
     ;; For tests.
     (list shared-mime-info hicolor-icon-theme python-nose))
    (home-page "https://www.freedesktop.org/wiki/Software/pyxdg")
    (synopsis "Implementations of freedesktop.org standards in Python")
    (description
     "PyXDG is a collection of implementations of freedesktop.org standards in
Python.")
    (license license:lgpl2.0)))

(define-public hyprland-protocols
  (package
    (name "hyprland-protocols")
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprland-protocols")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0njqyl6vsqlb8dv4wdn5h34dk67yqzc99gvwa13j252cv3n0bpya"))))
    (build-system meson-build-system)
    (home-page "https://github.com/hyprwm/hyprland-protocols")
    (synopsis "Wayland protocol extensions for Hyprland")
    (description
     "This package provides Wayland protocol extensions for Hyprland.")
    (license license:bsd-3)))

(define-public hyprwayland-scanner
  (package
    (name "hyprwayland-scanner")
    (version "0.4.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprwayland-scanner")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1bnckwj7hh4k4knlyprybi1fmy9vda2h492hw6yska2shfzp6jvy"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;No tests.
    (inputs (list pugixml))
    (native-inputs (list gcc-14 pkg-config))
    (home-page "https://github.com/hyprwm/hyprwayland-scanner")
    (synopsis "Hyprland implementation of @code{wayland-scanner}")
    (description
     "This package provides a Hyprland implementation of @code{wayland-scanner},
in and for C++.")
    (license license:bsd-3)))

(define-public wayland
  (package
    (name "wayland")
    (version "1.23.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gitlab.freedesktop.org/" name
                                  "/" name  "/-/releases/" version "/downloads/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1vg5h6d94hglh7724q6wx9dpg4y0afvxksankp1hwbcy76lb4kw6"))))
    (build-system meson-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list #:parallel-tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'move-doc
                 (lambda _
                   (mkdir-p (string-append #$output:doc "/share"))
                   (rename-file
                    (string-append #$output "/share/doc")
                    (string-append #$output:doc "/share/doc")))))))
    (native-inputs
     (append
      (list docbook-xml-4.2
            docbook-xml-4.5
            docbook-xsl
            graphviz
            doxygen
            pkg-config
            python
            xmlto
            libxslt)
      (if (%current-target-system)
          (list pkg-config-for-build
                this-package)           ;for wayland-scanner
          '())))
    (inputs (list expat libxml2))
    (propagated-inputs (list libffi))
    (home-page "https://wayland.freedesktop.org/")
    (synopsis "Core Wayland window system code and protocol")
    (description "Wayland is a project to define a protocol for a compositor to
talk to its clients as well as a library implementation of the protocol.  The
compositor can be a standalone display server running on Linux kernel
modesetting and evdev input devices, an X application, or a wayland client
itself.  The clients can be traditional applications, X servers (rootless or
fullscreen) or other display servers.")
    (license license:expat)))

(define-public wayland-protocols
  (package
    (name "wayland-protocols")
    (version "1.39")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/wayland/wayland-protocols")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dpcwsd2p6sjf5164b674cr7vq24hp3lfdshijj438r4bx8bld28"))))
    (build-system meson-build-system)
    (inputs
     (list wayland))
    (native-inputs (cons* pkg-config python
                          (if (%current-target-system)
                              (list pkg-config-for-build
                                    wayland) ; for wayland-scanner
                              '())))
    (synopsis "Wayland protocols")
    (description "Wayland-Protocols contains Wayland protocols that add
functionality not available in the Wayland core protocol.  Such protocols either
add completely new functionality, or extend the functionality of some other
protocol either in Wayland core, or some other protocol in wayland-protocols.")
    (home-page "https://wayland.freedesktop.org")
    (license license:expat)))

(define-public wayland-protocols-next
  (package
    (inherit wayland-protocols)
    (name "wayland-protocols-next")
    (version "1.43")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/wayland/wayland-protocols")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pgjkc0gw11xb55kn8hf8adnmx3bkpgb4p0haylb2jh7irqhxhqd"))))))

(define-public wayland-utils
  (package
    (name "wayland-utils")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.freedesktop.org/wayland/wayland-utils")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dj9p7vrv3a0fflqkwps8im2hz3ari385a3nqb4ar1ci3crxp204"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list libdrm wayland wayland-protocols))
    (home-page "https://wayland.freedesktop.org/")
    (synopsis "Display information about the Wayland protocols")
    (description "This package provides @code{wayland-info} tool that can be
used to check which Wayland protocols and versions are advertised by the Wayland
compositor.")
    (license license:expat)))

(define-public waylandpp
  (package
    (name "waylandpp")
    (version "0.2.9")
    (home-page "https://github.com/NilsBrause/waylandpp")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0z4m30r609as3kpcgipivddr98y7h529r7ldn9ba4snhk341mfvk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ; no tests
    (native-inputs
     (list pkg-config))
    (inputs
     (list mesa pugixml))
    (propagated-inputs
     (list ;; In Requires of the .pc files.
           wayland))
    (synopsis "Wayland C++ bindings")
    (description
     "This package provides C++ bindings for the Wayland display protocol.")
    (license license:bsd-2)))

(define-public weston
  (package
    (name "weston")
    (version "10.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://gitlab.freedesktop.org/wayland/weston/-/releases/"
                    version "/downloads/weston-" version ".tar.xz"))
              (sha256
               (base32
                "1rs92p7sfkw9lqlkfnqh5af19ym3x8l3hp3yfv117m7qv6h6qr49"))))
    (build-system meson-build-system)
    (native-inputs
     (list mscgen pkg-config python-3 xorg-server))
    (inputs
     (list cairo-xcb
           colord
           dbus
           elogind
           freerdp
           glib
           gstreamer
           gst-plugins-base
           lcms
           libdrm
           libevdev
           libinput-minimal
           libjpeg-turbo
           libpng
           libunwind
           libva
           libwebp
           libx11
           libxcb
           libxcursor
           libxml2
           mesa
           mtdev
           linux-pam
           pango
           pipewire
           wayland-protocols
           xorg-server-xwayland))
    (propagated-inputs
     (list libxkbcommon pixman wayland))
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; Otherwise, the RUNPATH will lack the final path component.
         (string-append "-Dc_link_args=-Wl,-rpath="
                        #$output "/lib:"
                        #$output "/lib/weston:"
                        #$output "/lib/libweston-"
                        #$(version-major (package-version this-package)))
         "-Dbackend-default=auto"
         "-Dsystemd=false"
         (string-append "-Dxwayland-path="
                        #$(this-package-input "xorg-server-xwayland")
                        "/bin/Xwayland"))
      #:parallel-tests? #f              ; Parallel tests cause failures.
      #:phases
      '(modify-phases %standard-phases
         (add-before 'configure 'use-elogind
           (lambda _
             ;; Use elogind instead of systemd
             (substitute* "libweston/meson.build"
               (("libsystemd-login") "libelogind"))
             (substitute* '("libweston/launcher-logind.c"
                            "libweston/weston-launch.c")
               (("#include <systemd/sd-login.h>")
                "#include <elogind/sd-login.h>"))))
         (add-after 'configure 'patch-confdefs.h
           (lambda _
             (system "echo \"#define HAVE_SYSTEMD_LOGIN_209 1\" >> confdefs.h")))
         (add-before 'check 'setup
           (lambda _
             (setenv "HOME" (getcwd))
             (setenv "XDG_RUNTIME_DIR" (getcwd))))
         (add-before 'check 'start-xorg-server
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The test suite requires a running X server.
             (system "Xvfb :1 &")
             (setenv "DISPLAY" ":1"))))))
    (home-page "https://wayland.freedesktop.org")
    (synopsis "Reference implementation of a Wayland compositor")
    (description "Weston is the reference implementation of a Wayland
compositor, and a useful compositor in its own right.

A Wayland compositor allows applications to render to a shared offscreen
buffer using OpenGL ES.  The compositor then culls the hidden parts and
composes the final output.  A Wayland compositor is essentially a
multiplexer to the KMS/DRM Linux kernel devices.")
    (license license:expat)))

(define-public wev
  (package
    (name "wev")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~sircmpwn/wev")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0l71v3fzgiiv6xkk365q1l08qvaymxd4kpaya6r2g8yzkr7i2hms"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags
       (list "CC=gcc" (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     (list pkg-config scdoc))
    (inputs
     (list libxkbcommon wayland wayland-protocols))
    (home-page "https://git.sr.ht/~sircmpwn/wev")
    (synopsis "Wayland event viewer")
    (description "Wev is a tool that opens a window, printing all events
sent to a Wayland window, such as key presses.  It is analogous to the X11 tool
XEv.")
    (license license:expat)))

(define-public wlr-protocols
  (let ((commit "2b8d43325b7012cc3f9b55c08d26e50e42beac7d")
        (revision "0"))
    (package
      (name "wlr-protocols")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/wlroots/wlr-protocols.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "17blwww6rcrahwc6h6j68gh6wjbj14if3mihpxymfdw5pwl72rav"))))
      (build-system gnu-build-system)
      (arguments
       (list #:make-flags
             #~(list (string-append "PREFIX=" #$output))
             #:phases
             #~(modify-phases %standard-phases
                 (delete 'configure))))
      (native-inputs (list wayland))    ;For wayland-scanner.
      (home-page "https://gitlab.freedesktop.org/wlroots/wlr-protocols")
      (synopsis
       "Wayland protocols designed for use in wlroots (and other compositors)")
      (description
       "This package provides Wayland protocols designed for use in wlroots (and
other compositors).")
      (license license:expat))))

(define-public wtype
  (package
    (name "wtype")
    (version "0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/atx/wtype.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bpix92vzip9vlhzihj3k8h9flrlna231x3y8ah7p4965l177yjd"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config
           ;; for wayland-scanner
           wayland))
    (inputs (list wayland libxkbcommon))
    (synopsis "Xdotool type for Wayland")
    (description "Wtype lets you simulate keyboard input and mouse activity,
move and resize windows, etc.")
    (home-page "https://github.com/atx/wtype")
    ;; MIT License
    (license license:expat)))

(define-public exempi
  (package
    (name "exempi")
    (version "2.6.5")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "https://libopenraw.freedesktop.org/download/"
                   name "-" version ".tar.bz2"))
             (sha256
              (base32
               "1zhzwkfna14sy78llhfc94cy5hv3076j5v3p1zmvawzz5gaa7yg9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list (string-append "--with-boost="
                                              (assoc-ref %build-inputs "boost")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'remove-static-library
           (lambda* (#:key outputs #:allow-other-keys)
             ;; XXX: Some tests fail to build with --disable-static due to
             ;; symbols not being visible in the shared library:
             ;; <https://gitlab.freedesktop.org/libopenraw/exempi/-/issues/17>.
             ;; Simply delete the static library instead to save ~4.3 MiB.
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/lib/libexempi.a")))))))
    (native-inputs
     (list boost)) ; tests
    (inputs
     (list expat zlib))
    (home-page "https://libopenraw.freedesktop.org/exempi/")
    (synopsis "XMP metadata handling library")
    (description "Exempi is an implementation of the Extensible Metadata
Platform (@dfn{XMP}), which enables embedding metadata in PDF and image
formats.")
    (license license:bsd-3)))

(define-public libatasmart
  (package
    (name "libatasmart")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://0pointer.de/public/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "138gvgdwk6h4ljrjsr09pxk1nrki4b155hqdzyr8mlk3bwsfmw31"))))
    (build-system gnu-build-system)
    (native-inputs
     (append (if (and (%current-target-system)
                      (target-riscv64?))
                 (list config)
                 '())
             (list pkg-config)))
    (arguments
     (if (and (%current-target-system)
              (target-riscv64?))
         (list #:phases
               #~(modify-phases %standard-phases
                   (add-after 'unpack 'update-config
                     (lambda* (#:key native-inputs inputs #:allow-other-keys)
                       (for-each (lambda (file)
                                   (install-file
                                    (search-input-file
                                     (or native-inputs inputs)
                                     (string-append "/bin/" file)) "build-aux"))
                                 '("config.guess" "config.sub"))))))
         '()))
    (inputs
     (list eudev))
    (home-page "https://0pointer.de/blog/projects/being-smart.html")
    (synopsis "ATA S.M.A.R.T. reading and parsing library")
    (description
     "This library supports a subset of the ATA S.M.A.R.T. (Self-Monitoring,
Analysis and Reporting Technology) functionality.")
    (license license:lgpl2.1+)))

(define-public udisks
  (package
    (name "udisks")
    (version "2.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/storaged-project/" name
                    "/releases/download/" name "-" version "/"
                    name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1klf5pcr9yg8g88mwwh3q2j0idfwd8hfr2q6nknhsm02yv638mxp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list docbook-xml-4.3              ; to build the manpages
           docbook-xsl
           gettext-minimal
           `(,glib "bin")               ; for glib-mkenums
           gobject-introspection
           gtk-doc/stable
           pkg-config
           libxslt))
    (propagated-inputs
     (list glib))                       ; required by udisks2.pc
    (inputs
     (list acl
           bash-minimal
           cryptsetup
           kmod
           libatasmart
           libblockdev
           libgudev
           polkit
           util-linux))
    (outputs '("out"
               "doc"))                  ;5 MiB of gtk-doc HTML
    (arguments
     (list
      #:tests? #f                       ; requiring system message dbus
      #:disallowed-references '("doc")  ;enforce separation of "doc"
      #:configure-flags
      #~(list "--enable-man"
              "--enable-available-modules" ; Such as lvm2, btrfs, etc.
              "--localstatedir=/var"
              (string-append "--with-html-dir=" #$output:doc
                             "/share/doc/udisks/html")
              (string-append "--with-udevdir=" #$output "/lib/udev"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-commands
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/udiskslinuxdrive.c"
                (("\"eject %s\"")
                 (format #f "\"~a %s\""
                         (search-input-file inputs "bin/eject"))))
              (substitute* "src/udisksstate.c"
                (("\"umount -l %s\"")
                 (format #f "\"~a -l %s\""
                         (search-input-file inputs "bin/umount"))))))
          (add-before 'configure 'fix-girdir
            (lambda _
              ;; Install introspection data to its own output.
              (substitute* "udisks/Makefile.in"
                (("girdir = .*")
                 "girdir = $(datadir)/gir-1.0\n")
                (("typelibsdir = .*")
                 "typelibsdir = $(libdir)/girepository-1.0\n")))))))
    (home-page "https://www.freedesktop.org/wiki/Software/udisks/")
    (synopsis "Disk manager service")
    (description
     "UDisks provides interfaces to enumerate and perform operations on disks
and storage devices.  Any application (including unprivileged ones) can access
the udisksd(8) daemon via the name org.freedesktop.UDisks2 on the system
message bus.")
    ;; The dynamic library are under LGPLv2+, others are GPLv2+.
    (license (list license:gpl2+ license:lgpl2.0+))))

(define-public accountsservice
  (package
    (name "accountsservice")
    (version "23.13.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/"
                           "accountsservice/accountsservice-"
                           version ".tar.xz"))
       (sha256
        (base32 "0kwjkff5m7gnzpns6cy27az90w7sxzwzygyzwy90kyi4mvg4rnmd"))
       (patches (search-patches "accountsservice-extensions.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '("--localstatedir=/var"
         "-Delogind=true"
         "-Ddocbook=true"
         "-Dgtk_doc=true"
         "-Dsystemdsystemunitdir=/tmp/empty")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "meson_post_install.py"
               (("in dst_dirs") "in []"))
             (substitute* '("src/user.c" "src/daemon.c")
               (("/bin/cat")
                (search-input-file inputs "bin/cat"))
               (("/usr/sbin/usermod")
                (search-input-file inputs "sbin/usermod"))
               (("/usr/sbin/useradd")
                (search-input-file inputs "sbin/useradd"))
               (("/usr/sbin/userdel")
                (search-input-file inputs "sbin/userdel"))
               (("/usr/bin/passwd")
                (search-input-file inputs "bin/passwd"))
               (("/usr/bin/chage")
                (search-input-file inputs "bin/chage")))))
         (add-after 'install 'wrap-with-xdg-data-dirs
           ;; This is to allow accountsservice finding extensions, which
           ;; should be installed to the system profile.
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (search-input-file outputs "libexec/accounts-daemon")
               '("XDG_DATA_DIRS" prefix
                 ("/run/current-system/profile/share"))))))))
    (native-inputs
     (list docbook-xml-4.1.2
           docbook-xsl
           gettext-minimal
           `(,glib "bin")               ; for gdbus-codegen, etc.
           glibc-locales                    ;for tests
           gobject-introspection
           gtk-doc/stable
           libxslt
           pkg-config
           vala
           xmlto

           ;; For the tests.
           python
           python-dbusmock
           python-pygobject))
    (inputs
     (list bash-minimal
           coreutils-minimal
           dbus
           elogind
           libxcrypt
           shadow))
    (propagated-inputs
     ;; accountsservice.pc 'Requires' these:
     (list glib polkit))
    (home-page "https://www.freedesktop.org/wiki/Software/AccountsService/")
    (synopsis "D-Bus interface for user account query and manipulation")
    (description
     "The AccountService project provides a set of D-Bus interfaces for
querying and manipulating user account information and an implementation of
these interfaces, based on the useradd, usermod and userdel commands.")
    (license license:gpl3+)))

(define-public libmbim
  (package
    (name "libmbim")
    (version "1.30.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mobile-broadband/libmbim")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "00kbjvpka51zrfjigzd3rk6r4x8hkg1xfj7d9zl9lccysnxyjx5h"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin")               ;for glib-mkenums
           gobject-introspection
           help2man
           pkg-config
           python-minimal))
    (propagated-inputs
     (list glib))                       ;required by mbim-glib.pc
    (inputs
     (list
      bash-completion
      libgudev))
    (synopsis "Library to communicate with MBIM-powered modems")
    (home-page "https://www.freedesktop.org/wiki/Software/libmbim/")
    (description
     "Libmbim is a GLib-based library for talking to WWAN modems and devices
which speak the Mobile Interface Broadband Model (MBIM) protocol.")
    (license
     ;; The libmbim-glib library is released under the LGPLv2+ license.
     ;; The mbimcli tool is released under the GPLv2+ license.
     (list license:lgpl2.0+ license:gpl2+))))

(define-public libqrtr-glib
  (package
    (name "libqrtr-glib")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/mobile-broadband/libqrtr-glib")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0bfn5kfscli0rrxvmzdl6ix5ikh0kxia7ad83rmh1hksfcwynwlh"))))
    (build-system meson-build-system)
    (inputs
     (list libgudev libmbim))
    (native-inputs
     (list bash-completion
           `(,glib "bin")
           gtk-doc/stable
           gobject-introspection
           pkg-config))
    (propagated-inputs
     (list glib))                       ;required by mm-glib.pc
    (synopsis "Qualcomm IPC Router protocol helper library")
    (description
     "libqrtr-glib is a glib-based library to use and manage the QRTR (Qualcomm
IPC Router) bus.")
    (home-page "https://gitlab.freedesktop.org/mobile-broadband/libqrtr-glib")
    (license license:lgpl2.1+)))

(define-public libqmi
  (package
    (name "libqmi")
    (version "1.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mobile-broadband/libqmi")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m5y2sf14qd2i9mvbb68wxqlfwvpiprgz8zmcx6wb2cnjgsszmwp"))))
    (build-system meson-build-system)
    (inputs
     (list
      bash-completion
      libgudev))
    (native-inputs
     (list `(,glib "bin")               ;for glib-mkenums
           gobject-introspection
           help2man
           pkg-config
           python-minimal))
    ;; These are required by qmi-glib.pc.
    (propagated-inputs
     (list glib
           libmbim
           libqrtr-glib))
    (synopsis "Library to communicate with QMI-powered modems")
    (home-page "https://www.freedesktop.org/wiki/Software/libqmi/")
    (description
     "Libqmi is a GLib-based library for talking to WWAN modems and devices
which speak the Qualcomm MSM Interface (QMI) protocol.")
    (license
     ;; The libqmi-glib library is released under the LGPLv2+ license.
     ;; The qmicli tool is released under the GPLv2+ license.
     (list license:lgpl2.0+ license:gpl2+))))

(define-public modem-manager
  (package
    (name "modem-manager")
    (version "1.22.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://gitlab.freedesktop.org/mobile-broadband/ModemManager")
         (commit version)))
       (patches (search-patches "modem-manager-fix-test-wrapper.patch"))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fj4ibjfsxal3xfk3hrj4l9vg7zbj42k9lj7151illl2n3d5ngzw"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dudevdir=" #$output "/lib/udev")
              "-Dsystemdsystemunitdir=no"
              "-Dvapi=true")))
    (native-inputs
     (list dbus
           gettext-minimal
           gobject-introspection
           `(,glib "bin")               ;for glib-mkenums
           libxslt                      ;for xsltproc
           pkg-config
           python-minimal
           python-dbus                  ;for test
           python-pygobject             ;for test
           vala))
    (propagated-inputs
     (list glib))                       ;required by mm-glib.pc
    (inputs
     (list bash-completion
           elogind
           libgudev
           libmbim
           libqmi
           libqrtr-glib
           polkit))
    (synopsis "Mobile broadband modems manager")
    (home-page "https://www.freedesktop.org/wiki/Software/ModemManager/")
    (description
     "ModemManager is a DBus-activated daemon which controls mobile
broadband (2G/3G/4G) devices and connections.  Whether built-in devices, USB
dongles, bluetooth-paired telephones, or professional RS232/USB devices with
external power supplies, ModemManager is able to prepare and configure the
modems and setup connections with them.")
    (properties
     '((upstream-name . "ModemManager")))
    (license license:gpl2+)))

(define-public telepathy-gabble
  ;; telepathy-gabble bundles wocky, an unreleased library.  The latest commit
  ;; includes a more recent version.
  (let ((commit "f1c762df6328916b811a834047fedac8529cf157")
        (revision "1"))
    (package
      (name "telepathy-gabble")
      (version (git-version "0.18.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TelepathyIM/telepathy-gabble/")
               (commit commit)
               (recursive? #true)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "00ss14hf1qwb42648cldghmfjfn1nkjvpy508b7vaz322fj37qa4"))))
      (build-system gnu-build-system)
      (arguments
       (list
        ;; Parallel tests freeze.
        #:parallel-tests? #false
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'delete-autogen
             (lambda _ (delete-file "autogen.sh")))
           (add-before 'configure 'configure-wocky
             (lambda* (#:key configure-flags #:allow-other-keys)
               (with-directory-excursion "lib/ext/wocky"
                 (invoke "gtkdocize")
                 (invoke "bash" "autoreconf" "-vif")
                 (substitute* "configure"
                   (("/bin/sh") (which "sh")))
                 (apply invoke "bash" "configure" configure-flags)))))
        #:configure-flags
        #~(list (string-append "--prefix=" #$output)
                "--disable-avahi-tests"
                "--disable-dependency-tracking"
                "--disable-Werror"
                "--without-ca-certificates")))
      (native-inputs
       (list autoconf
             automake
             libtool
             `(,glib "bin")             ;for glib-compile-schemas, etc.
             gtk-doc
             pkg-config))
      (inputs
       (list dbus
             glib
             gnutls
             gobject-introspection
             libnice
             libsoup-minimal-2
             libxslt
             python))
      (propagated-inputs
       (list telepathy-glib))
      (home-page "https://telepathy.freedesktop.org/components/telepathy-gabble/")
      (synopsis "XMPP connection manager for Telepathy")
      (description
       "Gabble is a Jabber/XMPP connection manager for the Telepathy
framework, currently supporting:

@itemize
@item single-user chats
@item multi-user chats
@item voice/video calling
@item file transfer
@end itemize

with Jabber/XMPP interoperability.

Telepathy is a D-Bus framework for unifying real time communication, including
instant messaging, voice calls and video calls.  It abstracts differences
between protocols to provide a unified interface for applications.")
      (license license:lgpl2.1))))

(define-public telepathy-logger
  (package
    (name "telepathy-logger")
    (version "0.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://telepathy.freedesktop.org/releases/"
                                  name "/" name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1bjx85k7jyfi5pvl765fzc7q2iz9va51anrc2djv7caksqsdbjlg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
          (lambda _
            (setenv "HOME" (getenv "TMPDIR"))
            #t)))))
    (native-inputs
     `(("glib:bin" ,glib "bin") ; for glib-genmarshal, etc.
       ("gobject-introspection" ,gobject-introspection)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)
       ("python" ,python-2)
       ("xsltproc" ,libxslt)))
    (propagated-inputs
     ;; telepathy-logger-0.2.pc refers to all these.
     (list libxml2 sqlite telepathy-glib))
    (synopsis "Telepathy logger library")
    (home-page "https://telepathy.freedesktop.org/")
    (description
     "Telepathy logger is a headless observer client that logs information
received by the Telepathy framework.  It features pluggable backends to log
different sorts of messages in different formats.")
    (license license:lgpl2.1+)))

(define-public telepathy-idle
  ;; Use the latest commit, as the latest release does not support Python 3.
  (let ((commit "b516eab0f2b92e078e0f5cab4224214d215b2ea5")
        (revision "0"))
    (package
      (name "telepathy-idle")
      (version (git-version "0.2.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TelepathyIM/telepathy-idle")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02wb61h2k3hhis5y2xi5rhc6pmikd13x722hk620sqb9b3m5pn3s"))))
      (build-system gnu-build-system)
      (native-inputs (list autoconf automake libtool pkg-config))
      (inputs (list libxslt python-wrapper python-dbus))
      (propagated-inputs (list telepathy-glib))
      (home-page "https://telepathy.freedesktop.org/")
      (synopsis "Telepathy IRC connection manager")
      (description
       "Idle is an IRC connection manager for the Telepathy framework.  This
package enables usage of IRC channels and private messages in Telepathy instant
messaging clients such as Empathy, GNOME Shell or KDE Telepathy.")
      (license (list license:lgpl2.1 license:lgpl2.1+)))))

(define-public telepathy-mission-control
  (package
    (name "telepathy-mission-control")
    (version "5.16.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://telepathy.freedesktop.org/releases/"
                           "telepathy-mission-control/"
                           "telepathy-mission-control-" version ".tar.gz"))
       (sha256
        (base32 "0ibs575pfr0wmhfcw6ln6iz7gw2y45l3bah11rksf6g9jlwsxy1d"))))
    (build-system gnu-build-system)
    (native-inputs
     (list `(,glib "bin") ; for glib-compile-schemas, etc.
           gtk-doc/stable
           pkg-config
           python-minimal
           libxslt))
    (inputs (list dconf libgnome-keyring))
    (propagated-inputs (list telepathy-glib))
    (home-page "https://telepathy.freedesktop.org/wiki/Components/Mission_Control/")
    (synopsis "Telepathy real-time communication framework management daemon")
    (description
     "Telepathy Mission Control 5 is an account manager and channel dispatcher
for the Telepathy framework, allowing user interfaces and other clients to
share connections to real-time communication services without conflicting.")
    (license license:lgpl2.1)))

(define-public telepathy-salut
  ;; telepathy-salut bundles wocky, an unreleased library.  The latest commit
  ;; includes a more recent version.
  (let ((commit "90dbe5e74ccdd063cb123212a754f994c9d2019f")
        (revision "1"))
    (package
      (name "telepathy-salut")
      (version (git-version "0.8.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/TelepathyIM/telepathy-salut")
               (commit commit)
               (recursive? #true)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "195pz8dgwhyy1cygd0rlncyr3c4wzhnf99sfjj5qmc8j195j1k7a"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'delete-autogen
             (lambda _ (delete-file "autogen.sh")))
           ;; The twisted tests all fail, but there are no logs, so we can't
           ;; tell what's wrong.
           (add-after 'unpack 'disable-twisted-tests
             (lambda _
               (substitute* "tests/Makefile.am"
                 (("SUBDIRS = twisted") ""))))
           (add-before 'configure 'configure-wocky
             (lambda* (#:key configure-flags #:allow-other-keys)
               (with-directory-excursion "lib/ext/wocky"
                 (invoke "gtkdocize")
                 (invoke "bash" "autoreconf" "-vif")
                 (substitute* "configure"
                   (("/bin/sh") (which "sh")))
                 (apply invoke "bash" "configure" configure-flags)))))
        #:configure-flags
        #~(list (string-append "--prefix=" #$output)
                "--disable-avahi-tests"
                "--without-ca-certificates"
                "--disable-Werror"
                "--enable-olpc")))
      (native-inputs
       (list autoconf
             automake
             libtool
             `(,glib "bin")             ;for glib-compile-schemas, etc.
             gtk-doc
             pkg-config))
      (inputs
       (list avahi
             dbus
             glib
             gnutls
             gobject-introspection
             libxml2
             libxslt
             libsoup-minimal-2
             python
             `(,util-linux "lib")))
      (propagated-inputs
       (list telepathy-glib))
      (home-page "https://telepathy.freedesktop.org/wiki/Components/")
      (synopsis "Link-local XMPP connection manager")
      (description
       "Salut is a link-local XMPP (XEP-0174) connection manager for the
Telepathy framework, currently supporting presence and single-user chats with
iChat interoperability, and multi-user chats and Tubes using the
@url{https://telepathy.freedesktop.org/wiki/Clique,Clique} protocol.")
      (license license:lgpl2.1))))

(define-public colord-gtk
  (package
    (name "colord-gtk")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/colord"
                                  "/releases/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "0b8j7an572ww8n3n0j9kwrl27qd3156g4zix9rzs2c2nny4vhxn1"))))
    (outputs '("out" "doc"))
    (build-system meson-build-system)
    (arguments
     (list
      #:tests? #f            ;require the colord system service
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'split-package
            (lambda _
              (let* ((old (string-append #$output "/share/gtk-doc"))
                     (new (string-append #$output:doc "/share/gtk-doc")))
                (mkdir-p (dirname new))
                (rename-file old new)))))))
    (native-inputs
     (list docbook-xsl
           gettext-minimal
           gobject-introspection
           gtk-doc/stable
           libxslt
           pkg-config
           vala))
    (inputs
     (list gtk+))
    (propagated-inputs
     ;; colord-gtk.pc refers to all these.
     (list colord gtk))
    (synopsis "GTK integration for libcolord")
    (home-page "https://www.freedesktop.org/software/colord/")
    (description
     "This is a GTK convenience library for interacting with colord.  It is
useful for both applications which need colour management and applications
that wish to perform colour calibration.")
    (license license:lgpl2.1+)))

(define-public libfprint
  (package
    (name "libfprint")
    (version "1.94.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libfprint/libfprint")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l1ak7y2kz0nrdkfj41n7h34dyykgzdg50y752ayk3ginp6szr7r"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "-Dudev_hwdb_dir=" #$output
                                  "/lib/udev/hwdb.d")
                   (string-append "-Dc_link_args=-Wl,-rpath="
                                  (search-input-directory %build-inputs
                                                          "lib/nss"))
                   (string-append "-Dudev_rules_dir=" #$output
                                  "/lib/udev/rules.d"))))
    (native-inputs
     (list `(,glib "bin")               ; for {glib-,}mkenums
           gobject-introspection
           gtk-doc/stable               ; for 88 KiB of API documentation
           pkg-config
           ;; For tests
           python-minimal))
    (inputs
     (list gusb
           libgudev
           nss                          ; for the URU4x00 driver
           ;; Replacing this with cairo works but just results in a reference
           ;; (only) to pixman in the end.
           pixman))
    (home-page "https://fprint.freedesktop.org/")
    (synopsis "Library to access fingerprint readers")
    (description
     "libfprint is a library designed to make it easy for application
developers to add support for consumer fingerprint readers to their
software.")
    (license license:lgpl2.1+)))

(define-public fprintd
  (package
    (name "fprintd")
    (version "1.94.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/libfprint/fprintd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "015k3kc4fmas0vc2b21qzq7kvdc9x6lcqvjhbvy6m84pkhhmry3q"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-Dsystemd_system_unit_dir=/tmp"
                   (string-append "-Ddbus_service_dir=" #$output
                                  "/share/dbus-1/system-services")
                   (string-append "-Dpam_modules_dir=" #$output
                                  "/lib/security"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'patch-output-directories
                 ;; Install files to our output, not that of the ‘owner’ package.
                 ;; These are not exposed as Meson options and must be patched.
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "meson.build"
                       (("(dbus_interfaces_dir = ).*" _ set)
                        (string-append set "'" out "/share/dbus-1/interfaces'\n"))
                       (("(polkit_policy_directory = ).*" _ set)
                        (string-append set "'" out "/share/polkit-1/actions/'\n"))
                       (("(dbus_data_dir = ).*" _ set)
                        (string-append set "get_option('prefix')"
                                       " / get_option('datadir')\n"))))))
               (add-before 'configure 'patch-systemd-dependencies
                 (lambda _
                   (substitute* "meson.build"
                     (("(dependency\\(')(libsystemd|systemd)" _ prefix)
                      (string-append prefix "libelogind")))))
               (add-before 'configure 'ignore-test-dependencies
                 (lambda _
                   (substitute* "meson.build"
                     ((".*gi\\.repository\\.FPrint.*") "")
                     (("pam_wrapper_dep .*") "")
                     ((".*'(cairo|dbus|dbusmock|gi|pypamtest)': .*,.*") ""))
                   (substitute* "tests/pam/meson.build"
                     ((".*pam_wrapper.*") "")))))
           #:tests? #f))                    ; XXX depend on unpackaged packages
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")               ; for glib-genmarshal
           perl                         ; for pod2man
           pkg-config
           ;; For tests.
           python))                     ; needed unconditionally
           ;; pam_wrapper
           ;; python-pycairo
           ;; python-dbus
           ;; python-dbusmock
           ;; python-pygobject
           ;; python-pypamtest
    (inputs
     (list dbus-glib
           elogind
           libfprint
           linux-pam
           polkit))
    (home-page "https://fprint.freedesktop.org/")
    (synopsis "D-Bus daemon that exposes fingerprint reader functionality")
    (description
     "fprintd is a D-Bus daemon that offers functionality of libfprint, a
library to access fingerprint readers, over the D-Bus interprocess
communication bus.  This daemon layer above libfprint solves problems related
to applications simultaneously competing for fingerprint readers.")
    (license license:gpl2+)))

(define-public desktop-file-utils
  (package
    (name "desktop-file-utils")
    (version "0.26")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.freedesktop.org/software/"
                                  "desktop-file-utils/releases/"
                                  "desktop-file-utils-" version ".tar.xz"))
              (sha256
               (base32
                "02bkfi6fyk4c0gh2avd897882ww5zl7qg7bzzf28qb57kvkvsvdj"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list glib))
    (home-page "https://www.freedesktop.org/wiki/Software/desktop-file-utils/")
    (synopsis "Utilities for working with desktop entries")
    (description
     "This package contains a few command line utilities for working with
desktop entries:
@table @command
@item desktop-file-validate
Validates a desktop file and prints warnings/errors about desktop entry
specification violations.

@item desktop-file-install
Installs a desktop file to the applications directory, optionally munging it
a bit in transit.

@item update-desktop-database
Updates the database containing a cache of MIME types handled by desktop
files.
@end table")
    (license license:gpl2+)))

(define-public drm-info
  (package
    (name "drm-info")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/emersion/drm_info.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fc1rd3c16ddzbdpcj473ykszipzblj98lk376slk63v7mqvc1qm"))))
    (build-system meson-build-system)
    (arguments (list #:configure-flags
                     #~(list "-Dman-pages=enabled"
                             "-Dlibpci=enabled")))
    (native-inputs
     (append (if (%current-target-system)
                 (list pkg-config-for-build)
                 '())
             (list pkg-config scdoc)))
    (inputs
     (list libdrm json-c pciutils))
    (home-page "https://gitlab.freedesktop.org/emersion/drm_info")
    (synopsis "Dump DRM device info")
    (description "Displaying and dumping information on Direct
Rendering Manager devices.")
    (license license:expat)))

(define-public xdg-user-dirs
  (package
    (name "xdg-user-dirs")
    (version "0.17")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://user-dirs.freedesktop.org/releases/"
                                    name "-" version ".tar.gz"))
              (sha256
               (base32 "13216b8rfkzak5k6bvpx6jvqv3cnbgpijnjwj8a8d3kq4cl0a1ra"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gettext-minimal
           docbook-xsl
           docbook-xml-4.3
           libxslt))
    (home-page "https://www.freedesktop.org/wiki/Software/xdg-user-dirs/")
    (synopsis "Tool to help manage \"well known\" user directories")
    (description "xdg-user-dirs is a tool to help manage \"well known\" user
directories, such as the desktop folder or the music folder.  It also handles
localization (i.e. translation) of the file names.  Designed to be
automatically run when a user logs in, xdg-user-dirs can also be run
manually by a user.")
    (license license:gpl2)))

(define-public perl-file-basedir
  (package
    (name "perl-file-basedir")
    (version "0.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/P/PL/PLICEASE/"
                           "File-BaseDir-" version ".tar.gz"))
       (sha256
        (base32
         "1nb757cyyy80xln147qgns113i2ivfpgcfhsxw8qzb322llgg9kd"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-module-build perl-file-which perl-test-pod
           perl-test-pod-coverage xdg-user-dirs))
    (propagated-inputs
     (list perl-ipc-system-simple))
    (home-page "https://metacpan.org/release/File-BaseDir")
    (synopsis "Use the Freedesktop.org base directory specification")
    (description
     "@code{File::Basedir} can be used to find directories and files as
specified by the Freedesktop.org Base Directory Specification.  This
specifications gives a mechanism to locate directories for configuration,
application data and cache data.")
    (license license:perl-license)))

(define-public perl-file-desktopentry
  (package
    (name "perl-file-desktopentry")
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MICHIELB/"
                           "File-DesktopEntry-" version ".tar.gz"))
       (sha256
        (base32
         "1f1maqix2kbfg2rf008m7mqnvv6nvcf9y6pcgdv2kxp2vbih370n"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-test-pod perl-test-pod-coverage))
    (propagated-inputs
     (list perl-file-basedir perl-uri))
    (home-page "https://metacpan.org/release/File-DesktopEntry")
    (synopsis "Handle @file{.desktop} files")
    (description
     "@code{File::DesktopEntry} parses @file{.desktop} files defined by the
Freedesktop.org @dfn{Desktop Entry} specification.  It can also run the
applications define in those files.")
    (license license:perl-license)))

(define-public perl-file-mimeinfo
  (package
    (name "perl-file-mimeinfo")
    (version "0.33")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/M/MI/MICHIELB/"
                           "File-MimeInfo-" version ".tar.gz"))
       (sha256
        (base32
         "1i5iw6ri0w9clwpqf40xmsh4isc8xvx2lyf2r5g34886i6rsdgpn"))))
    (build-system perl-build-system)
    (inputs (list bash-minimal))        ;for wrap-program
    ;; If the tests are fixed, add perl-test-pod, perl-test-pod-coverage, and
    ;; perl-test-tiny as native-inputs.
    (propagated-inputs
     (list shared-mime-info perl-file-desktopentry))
    (arguments
     ;; Some tests fail due to requiring the mimetype of perl files to be
     ;; text/plain when they are actually application/x-perl.
     (list #:tests? #f
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-programs
                 ;; TODO(staging): Make unconditional.
                 (lambda* (#:key #$@(if (%current-target-system)
                                        #~(inputs)
                                        #~()) outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (for-each
                      (lambda (prog)
                        (wrap-program (string-append out "/bin/" prog)
                          `("PERL5LIB" ":" prefix
                            ;; PERL5LIB looks in 'native-inputs', not 'inputs',
                            ;; whereas the latter is required for
                            ;; cross-compilation.
                            #$(if (%current-target-system)
                                  #~,(search-path-as-list
                                      '("lib/perl5/site_perl")
                                      (map cdr (append inputs outputs)))
                                  #~(,(string-append
                                       (getenv "PERL5LIB")
                                       ":" out "/lib/perl5/site_perl"))))))
                      '("mimeopen" "mimetype"))))))))
    (home-page "https://metacpan.org/release/File-MimeInfo")
    (synopsis "Determine file type from the file name")
    (description
     "@code{File::Mimeinfo} can be used to determine the MIME type of a file.
It tries to implement the Freedesktop specification for a shared MIME
database.

This package also contains two related utilities:

@itemize
@item @command{mimetype} determines a file's MIME type;
@item @command{mimeopen} opens files in an appropriate program according to
their MIME type.
@end itemize")
    (license license:perl-license)))

(define-public uchardet
  (package
    (name "uchardet")
    (version "0.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.freedesktop.org/software/"
                            name "/releases/" name "-" version ".tar.xz"))
        (sha256
          (base32 "1w659aiphbnczpry771diakrzg9a8aqpn2abcxx1870aq37n0yp9"))))
    (build-system cmake-build-system)
    (home-page "https://www.freedesktop.org/wiki/Software/uchardet/")
    (synopsis "Encoding detector library")
    (description "uchardet is an encoding detector library, which takes a
sequence of bytes in an unknown character encoding without any additional
information, and attempts to determine the encoding of the text.  Returned
encoding names are iconv-compatible.")

    ;; This combines code under MPL 1.1, LGPL 2.1+, and GPL 2.0+, so the
    ;; combination is GPL 2.0+.
    (license license:gpl2+)))

(define-public python-cchardet
  (package
  (name "python-cchardet")
  (version "2.2.0a2")
  (source
    (origin
      (method url-fetch)
      (uri (pypi-uri "cchardet" version))
      (sha256
        (base32
          "08wq5yfaafbjipabfc6kpyvivkk2394w7isv0mwx5agcf8cbnwnx"))))
  (build-system pyproject-build-system)
  (inputs
   (list uchardet))
  (native-inputs
   (list python-setuptools python-wheel))
  (home-page "https://github.com/PyYoshi/cChardet")
  (synopsis "High-performance character encoding detection for Python")
  (description "cChardet is a character encoding detector, written in
Python, that binds to the C library @code{uchardet} to increase performance.")
  (license license:gpl2+)))

(define-public udiskie
  (package
    (name "udiskie")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "udiskie" version))
       (sha256
        (base32
         "0z0gk8l6rv4np29kfdalmy4q3900005sxhjg0jz1aa8irdcsp1qz"))))
    (build-system python-build-system)
    (native-inputs
     (list asciidoc
           gettext-minimal
           gobject-introspection))
    (inputs
     (list bash-minimal
           gobject-introspection
           gtk+
           libappindicator
           libnotify
           udisks))
    (propagated-inputs
     (list python-docopt python-pygobject python-keyutils python-pyxdg
           python-pyyaml))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-gi-typelib
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/udiskie")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (home-page "https://github.com/coldfix/udiskie")
    (synopsis "Automounter for removable media")
    (description
     "The @command{udiskie} program is a udisks2 front-end that
manages removable media such as CDs or flash drives from userspace.

Its features include:

@itemize
@item automount removable media,
@item notifications,
@item tray icon,
@item command line tools for manual (un)mounting,
@item LUKS encrypted devices,
@item unlocking with keyfiles,
@item loop devices (mounting ISO archives),
@item password caching.
@end itemize
")
    (license license:expat)))

(define-public plymouth
  (package
    (name "plymouth")
    (version "24.004.60")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/"
                           "plymouth/releases/" name "-" version ".tar.xz"))
       (sha256
        (base32
         "0d0wbfsy70xhgxv4mldv72gzv0k8bvfxpvm90rxmx3y9b09q9xzk"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "-Dlogo=/var/run/plymouth/logo.png"
             "-Dlocalstatedir=/var"
             "-Dboot-tty=/dev/console"
             "-Ddrm=true"
             "-Dsystemd-integration=false"
             ;; Disable GTK to dramatically reduce the closure
             ;; size from ~800 MiB to a little more than 200 MiB
             "-Dgtk=disabled")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-install
            (lambda _
              (substitute* "src/meson.build"
                (("install_emptydir" all) (string-append "# " all)))
              (substitute* "themes/meson.build"
                ;; XXX: meson barfs when installing (temporarily broken)
                ;; symlink to logo.
                (("subdir\\('spinfinity'\\)") ""))))
          (add-after 'unpack 'make-reproducible
            (lambda _
              (substitute* "src/main.c"
                (("__DATE__") "\"guix\"")))))))
    (inputs
     (list eudev
           glib
           libdrm
           libevdev
           libpng
           libxkbcommon
           pango
           xkeyboard-config))
    (native-inputs
     (list gettext-minimal
           pkg-config
           libxslt
           docbook-xsl
           docbook-xml))
    (synopsis "Graphical boot animation (splash) and logger")
    (home-page "https://www.freedesktop.org/wiki/Software/Plymouth/")
    (description
     "Plymouth is an application that runs very early in the boot process and
that provides a graphical boot animation while the boot process happens in the
background.  You are not supposed to install this on your own, it is only
useful with system integration.")
    (license license:gpl2+)))

(define-public libindicator
  (package
    (name "libindicator")
    (version "12.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://launchpad.net/libindicator/"
             (version-major+minor version) "/" version
             "/+download/libindicator-" version ".tar.gz"))
       (sha256
        (base32
         "0zs4z7l9b57jldwz0ban77f3c2zq43ambd0dssf5qg9i216f9lmj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("dbus-test-runner" ,dbus-test-runner)
       ("glib:bin" ,glib "bin")
       ("pkg-config" ,pkg-config)
       ("xvfb" ,xorg-server-for-tests)))
    (inputs
     (list gtk+ glib))
    (arguments
     `(#:make-flags '("CFLAGS=-Wno-error")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-missing-space-for-libm
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("LIBM=\"-lm\"") "LIBM=\" -lm\""))
             #t))
         (add-before 'configure 'fix-test-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "tests/Makefile.in"
               (("/bin/sh") (which "sh"))
               (("#!/bin/bash") (string-append "#!" (which "bash")))
               (("/usr/share")
                (string-append (assoc-ref inputs "dbus-test-runner") "/share")))
             #t)))))
    (home-page "https://launchpad.net/libindicator")
    (synopsis "Ayatana indicators symbols and functions")
    (description "A set of symbols and convenience functions for Ayatana indicators.")
    (license license:gpl3)))

(define-public libappindicator
  ;; Use the latest commit as the latest official release from 2012 uses
  ;; Python 2.
  (let ((revision "0")
        ;; Corresponds to the 12.10.1+20.10.20200706.1-0ubuntu1 tag.
        (bazaar-revision "298"))
    (package
      (name "libappindicator")
      (version (string-append "12.10.1-" revision "-" bazaar-revision))
      (source (origin
                (method bzr-fetch)
                (uri (bzr-reference
                      (url "lp:libappindicator")
                      (revision bazaar-revision)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0jkc1xdsa7r71vrr2l7wgkarvzvwrpwn0m8m4ipaqlzfa5d45n3a"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf
             automake
             at-spi2-core
             dbus-test-runner
             `(,glib "bin")
             gnome-common
             gobject-introspection
             gtk-doc/stable
             libtool
             pkg-config
             vala
             which
             xorg-server-for-tests))
      (inputs
       (list dbus-glib))
      (propagated-inputs
       (list gtk+ libdbusmenu))
      (arguments
       `(#:configure-flags '("--with-gtk=3")
         #:make-flags '("CFLAGS=-Wno-error")
         #:tests? #f ; One test does not pass (it succeeds when it should fail).
         #:phases
         (modify-phases %standard-phases
           (add-before 'configure 'fix-paths
             (lambda* (#:key native-inputs inputs #:allow-other-keys)
               (substitute* "tests/Makefile.in"
                 (("/bin/sh") (which "sh"))
                 (("/bin/bash") (which "bash"))
                 (("/usr/(share/dbus-test-runner/session.conf)" _ tail)
                  (search-input-file (or native-inputs inputs) tail))))))))
      (home-page "https://launchpad.net/libappindicator")
      (synopsis "Allow applications to export a menu into the Unity menu bar")
      (description "A library to allow applications to export a menu, originally
into the Unity menu bar.  Based on KSNI, it also works in KDE and will
fallback to generic Systray support if none of those are available.")
      (license license:lgpl2.1+))))

(define-public snixembed
  (package
    (name "snixembed")
    (version "0.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~steef/snixembed/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14fkgxww4qbsxyqj9h3yqpdqsdz9r6015c9graas50r5b5ggd3bj"))
              (modules '((guix build utils)))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:make-flags #~(list "CC=gcc"
                                (string-append "PREFIX="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)))) ;no configure
    (inputs (list gtk+ libdbusmenu))
    (native-inputs (list pkg-config vala))
    (synopsis "Proxy StatusNotifierItems as XEmbedded systemtray-spec icons")
    (home-page "https://git.sr.ht/~steef/snixembed")
    (description
     "Snixembed is a program to proxy StatusNotifierItems as
XEmbedded systemtray-spec icons.  This allows programs that only support the
newer StatusNotifierItem to have the older XEmbedded systemtray support.
While snixembed works fine with most setups, some bars and DEs provide their
own optional SNI support, which should be preferred when available.

Currently supported:
@itemize
@item icons (by pixmap and by freedesktop name)
@item activation on left mouse button
@item context menu on right mouse button (Menu dbusmenu or ContextMenu)
@item tooltips (on hover, all markup except hyperlinks)
@item limited AppIndicator support as a fallback
@end itemize")
    (license license:isc)))

(define-public flatpak-xdg-utils
  (package
    (name "flatpak-xdg-utils")
    (version "1.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flatpak/flatpak-xdg-utils")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q8wsc46fcjm737hz10jvgci5wl9sz8hj9aix2y2zdj11bqib9af"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "dbus-run-session" "--" "meson" "test"
                                      "--print-errorlogs")))))))
    (inputs (list glib))
    (native-inputs (list dbus pkg-config))
    (synopsis
     "Simple portal-based commandline tools for use inside sandboxes")
    (description
     "This package contains a number of commandline utilities for use inside
Flatpak sandboxes and other containers, like @command{guix shell --container}.
They work by talking to portals.  Currently, there is flatpak-spawn for
running commands in sandboxes as well as xdg-open and xdg-email, which are
compatible with the well-known scripts of the same name.")
    (home-page "https://github.com/flatpak/flatpak-xdg-utils")
    (license (list license:lgpl2.0+ license:lgpl2.1+))))

(define-public libportal
  (package
    (name "libportal")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/flatpak/libportal")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ypl9ds5g5jzyirjg4ic0r7lzv39w67yrh8njz1cw566g4j1kfny"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Ddocs=false")          ; requires unpackaged gi-docgen
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-qt-environment-variables
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Required for tests
              (setenv "QT_QPA_PLATFORM" "offscreen"))))))
    (native-inputs
     (list pkg-config
           docbook-xsl
           docbook-xml
           `(,glib "bin")
           gobject-introspection
           libxml2
           vala))
    (inputs
     (list gtk
           gtk+
           qtbase-5
           qtx11extras))
    (propagated-inputs
     (list glib))
    (home-page "https://github.com/flatpak/libportal")
    (synopsis "Flatpak portal library")
    (description
     "libportal provides GIO-style async APIs for most Flatpak portals.")
    (license license:lgpl2.1+)))

(define-public xdg-desktop-portal
  (package
    (name "xdg-desktop-portal")
    (version "1.18.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/flatpak/xdg-desktop-portal/releases/download/"
             version "/xdg-desktop-portal-" version ".tar.xz"))
       (sha256
        (base32
         "0r8y8qmzcfj7b7brqcxr9lg8pavfds815ffvj0kqc378fhgaln5q"))
       (patches (search-patches
                 ;; Disable portal tests since they try to use fuse.
                 "xdg-desktop-portal-disable-portal-tests.patch"
                 "xdg-desktop-portal-disable-configuration-search-exit.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dsystemd=disabled")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'po-chmod
           (lambda _
             ;; Make sure 'msgmerge' can modify the PO files.
             (for-each (lambda (po)
                         (chmod po #o666))
                       (find-files "po" "\\.po$"))))
         (add-after 'unpack 'set-home-directory
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config
           python
           python-dbusmock
           python-pytest
           python-pytest-xdist))
    (inputs
     (list bubblewrap
           dbus
           flatpak
           fontconfig
           fuse
           gdk-pixbuf
           geoclue
           glib
           json-glib
           libportal
           pipewire))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DESKTOP_PORTAL_DIR")
            (separator #f)
            (files '("share/xdg-desktop-portal/portals")))))
    (home-page "https://github.com/flatpak/xdg-desktop-portal")
    (synopsis "Desktop integration portal for sandboxed apps")
    (description
     "xdg-desktop-portal is a @dfn{portal front-end service} for Flatpak and
possibly other desktop containment frameworks.  It works by exposing a series
of D-Bus interfaces known as portals under a well-known
name (@code{org.freedesktop.portal.Desktop}) and object
path (@code{/org/freedesktop/portal/desktop}).

The portal interfaces include APIs for file access, opening URIs, printing
and others.")
    (license license:lgpl2.1+)))

(define-public xdg-desktop-portal-gtk
  (package
    (name "xdg-desktop-portal-gtk")
    (version "1.14.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/flatpak/xdg-desktop-portal-gtk/releases/download/"
                    version "/xdg-desktop-portal-gtk-" version ".tar.xz"))
              (sha256
               (base32
                "002p19j1q3fc8x338ndzxnicwframpgafw31lwvv5avy329akqiy"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'po-chmod
           (lambda _
             ;; Make sure 'msgmerge' can modify the PO files.
             (for-each (lambda (po)
                         (chmod po #o666))
                       (find-files "po" "\\.po$"))
             #t)))
       ;; Enable Gnome portal backends
       #:configure-flags
       (list
        "--enable-appchooser"
        "--enable-wallpaper"
        "--enable-screenshot"
        "--enable-screencast"
        "--enable-background"
        "--enable-settings")))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("libxml2" ,libxml2)
       ("glib:bin" ,glib "bin")
       ("which" ,which)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("glib" ,glib)
       ("gtk" ,gtk+)
       ("fontconfig" ,fontconfig)
       ("gnome-desktop" ,gnome-desktop)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)))
    (propagated-inputs
     (list xdg-desktop-portal))
    (home-page "https://github.com/flatpak/xdg-desktop-portal-gtk")
    (synopsis "GTK implementation of xdg-desktop-portal")
    (description
     "This package provides a backend implementation for xdg-desktop-portal
which uses GTK+ and various pieces of GNOME infrastructure, such as the
@code{org.gnome.Shell.Screenshot} or @code{org.gnome.SessionManager} D-Bus
interfaces.")
    (license license:lgpl2.1+)))

(define-public xdg-desktop-portal-hyprland
  (package
    (name "xdg-desktop-portal-hyprland")
    (version "1.3.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0k1bgdpg5ixxqg9r4vraszbnl4rl9gh87dhyc7rr332rf0j9n0xh"))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                  ;No tests.
           #:qtbase qtbase
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "." "\\.cp?*$")
                     (("/bin/sh") "sh")
                     (("\\<(sh|grim|hyprctl|slurp)\\>" _ cmd)
                      (search-input-file inputs (string-append "bin/" cmd))))
                   (substitute* "src/shared/ScreencopyShared.cpp"
                     (("\\<(hyprland-share-picker)\\>" _ cmd)
                      (string-append #$output "/bin/" cmd))))))))
    (native-inputs
     (list gcc-14 hyprwayland-scanner pkg-config))
    (inputs
     (list bash-minimal
           grim
           hyprland
           hyprland-protocols
           hyprlang
           hyprutils
           mesa
           pipewire
           qtwayland
           sdbus-c++
           slurp
           wayland
           wayland-protocols))
    (home-page "https://github.com/hyprwm/xdg-desktop-portal-hyprland")
    (synopsis "Hyprland implementation of @code{xdg-desktop-portal} backend")
    (description
     "This package provides an @code{xdg-desktop-portal} backend for Hyprland.")
    (license license:bsd-3)))

(define-public xdg-desktop-portal-kde
  (package
    (name "xdg-desktop-portal-kde")
    (version "6.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/plasma/" version "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "1cm7vh179dvb4jrd70ifsgpkrnfk9imzb65cg76g5znmhvyibjiq"))))
    (build-system qt-build-system)
    (arguments (list
                #:tests? #f ;; colorschemetest test fail, because require dbus.
                #:qtbase qtbase))
    (native-inputs (list extra-cmake-modules pkg-config
                         ;; require by test.
                         python-minimal
                         python-pygobject))
    (inputs (list cups
                  kcoreaddons
                  kconfig
                  ki18n
                  kdeclarative
                  kio
                  kirigami
                  knotifications
                  libplasma
                  plasma-wayland-protocols
                  kstatusnotifieritem
                  kwayland
                  kwidgetsaddons
                  kwindowsystem
                  kiconthemes
                  qtdeclarative
                  qtwayland
                  wayland
                  kglobalaccel
                  kguiaddons
                  libxkbcommon
                  wayland-protocols))
    (propagated-inputs
     (list xdg-desktop-portal))
    (synopsis "Backend implementation for xdg-desktop-portal using Qt/KF5")
    (description "This package provides a backend implementation
for xdg-desktop-portal that is using Qt/KF5.")
    (home-page "https://invent.kde.org/plasma/xdg-desktop-portal-kde")
    (license license:lgpl2.0+)))

(define-public xdg-desktop-portal-wlr
  (package
    (name "xdg-desktop-portal-wlr")
    (version "0.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/emersion/xdg-desktop-portal-wlr")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mbq3czka9swwmfaasnaj89y2m254p3qa522ayclh688jdwh70hq"))
              (patches (search-patches "xdg-desktop-portal-wlr-harcoded-length.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       '("-Dsystemd=disabled"
         "-Dsd-bus-provider=libelogind")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'hardcode-binaries
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((sh (search-input-file inputs "/bin/sh"))
                   (grim (search-input-file inputs "/bin/grim"))
                   (slurp (search-input-file inputs "/bin/slurp")))
               (substitute* "src/screenshot/screenshot.c"
                 (("grim") grim)
                 (("slurp") slurp)
                 (("execl\\(\"/bin/sh\", \"/bin/sh\"")
                  (string-append "execl(\"" sh "\", \"" sh "\"")))
               (substitute* "src/screencast/screencast.c"
                 (("execvp\\(\"sh")
                  (string-append "execvp(\"" sh))))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "../source/README.md"
                           (string-append (assoc-ref outputs "out")
                                          "/share/doc/" ,name)))))))
    (native-inputs
     (list cmake-minimal pkg-config))
    (inputs (list elogind
                  bash-minimal
                  grim
                  iniparser
                  mesa
                  libinih
                  pipewire
                  slurp
                  wayland
                  wayland-protocols))
    (home-page "https://github.com/emersion/xdg-desktop-portal-wlr")
    (synopsis "@code{xdg-desktop-portal} backend for wlroots")
    (description
     "This package provides @code{xdg-desktop-portal-wlr}.  This project
seeks to add support for the screenshot, screencast, and possibly
remote-desktop @code{xdg-desktop-portal} interfaces for wlroots based
compositors.")
    (license license:expat)))

(define-public poweralertd
  (package
    (name "poweralertd")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~kennylevinsen/poweralertd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19rw9q4pcqw56nmzjfglfikzx5wwjl4n08awwdhg0jy1k0bm3dvp"))))
    (build-system meson-build-system)
    (arguments
     '(#:configure-flags '("-Dman-pages=enabled")))
    (native-inputs
     (list scdoc pkg-config bash-minimal))
    (inputs
     (list elogind))
    (home-page "https://sr.ht/~kennylevinsen/poweralertd")
    (synopsis "Power alert daemon")
    (description "poweralertd is a daemon that watches for UPower events and
notifies the user using any notification daemon implementing
@code{org.freedesktop.Notifications}.")
    (license license:gpl3+)))

(define-public waypipe
  (package
    (name "waypipe")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.freedesktop.org/mstoeckl/waypipe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pj7l3ix0pp0sfqxfa2hxql0f30vz6hh01fq5kzhs831b632i3z0"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config scdoc
           ;; For tests
           python))
    (inputs (list lz4 libva mesa libdrm ffmpeg))
    (arguments
     (list #:configure-flags
           #~(list "-Dwith_lz4=enabled" "-Dwith_vaapi=enabled"
                   "-Dwith_dmabuf=enabled" "-Dwith_video=enabled")))
    (home-page "https://gitlab.freedesktop.org/mstoeckl/waypipe")
    (synopsis "Proxy for Wayland protocol applications")
    (description
     "Waypipe is a proxy for Wayland clients, with the aim of
supporting behavior like @samp{ssh -X}.")
    (license license:expat)))

(define-public libdecor
  (package
    (name "libdecor")
    (version "0.2.2")
    (source
     (origin (method git-fetch)
             (uri (git-reference
                   (url "https://gitlab.freedesktop.org/libdecor/libdecor")
                   (commit version)))
             (file-name (git-file-name name version))
             (sha256
              (base32
               "05rxchwzhnkm91kcr30mavizkp25wgjlhb6lcraa456pw7vgb04q"))))
    (build-system meson-build-system)
    (native-inputs (list cmake pkg-config))
    (inputs (list cairo
                  dbus
                  egl-wayland
                  gtk+
                  libglvnd
                  libxkbcommon
                  pango
                  wayland
                  wayland-protocols))
    (home-page "https://gitlab.freedesktop.org/libdecor/libdecor")
    (synopsis "Client-side decorations library for Wayland clients")
    (description "libdecor is a library that can help Wayland clients draw
window decorations for them.  It aims to provide multiple backends that
implements the decoration drawing.")
    (license license:expat)))
