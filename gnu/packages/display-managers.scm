;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Joshua Grant <tadni@riseup.net>
;;; Copyright © 2014, 2018, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 L  p R n  d n <guix@lprndn.info>
;;; Copyright © 2020, 2024 Fredrik Salomonsson <plattfot@posteo.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages display-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system trivial)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages xorg)
  #:export (customize-lightdm-tiny-greeter))

(define-public sddm
  (package
    (name "sddm")
    (version "0.21.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sddm/sddm")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mxrh0z9x4r4bli25g746n66adwnf3r42lzq0yssc50v9y7fc1a1"))))
    (build-system qt-build-system)
    (native-inputs
     (list extra-cmake-modules pkg-config qttools))
    (inputs
     (list elogind
           glib
           libxcb
           libxkbcommon
           linux-pam
           qtdeclarative
           qtsvg
           shadow
           wayland
           qtwayland
           qtbase
           xsetroot))
    (arguments
     (let* ((qtbase (this-package-input "qtbase"))
            (qt6? (string= "6" (version-major (package-version qtbase)))))
       (list
        #:qtbase qtbase
        #:configure-flags
        #~(list
           #$@(if qt6?
                  #~("-DBUILD_WITH_QT6=ON")
                  #~())
           "-DENABLE_WAYLAND=ON"
           "-DENABLE_PAM=ON"
           ;; PAM is configured by pam service.
           "-DINSTALL_PAM_CONFIGURATION=OFF"
           ;; Both flags are required for elogind support.
           "-DNO_SYSTEMD=ON"
           "-DUSE_ELOGIND=ON"
           "-DCONFIG_FILE=/etc/sddm.conf"
           ;; Set path to /etc/login.defs.
           ;; An alternative would be to use -DUID_MIN and -DUID_MAX.
           (string-append "-DLOGIN_DEFS_PATH="
                          #$(this-package-input "shadow")
                          "/etc/login.defs")
           (string-append
            "-DCMAKE_CXX_FLAGS=-I"
            #$(this-package-input "qtdeclarative") "/include/qt" #$(if qt6? "6" "5"))
           (string-append "-DQT_IMPORTS_DIR="
                          #$output "/lib/qt" #$(if qt6? "6" "5") "/qml")
           (string-append "-DCMAKE_INSTALL_SYSCONFDIR="
                          #$output "/etc"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'embed-loginctl-reference
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("/usr/bin/loginctl")
                   (which "loginctl")))))
            (add-after 'unpack 'embed-xsetroot-reference
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* '("src/daemon/XorgDisplayServer.cpp"
                               "src/helper/xorguserhelper.cpp")
                  (("xsetroot")
                   (search-input-file inputs "/bin/xsetroot")))))
            #$@(if qt6?
                   #~((add-after 'unpack 'fix-QML_IMPORT_PATH
                        (lambda _
                          (substitute* "src/daemon/Greeter.cpp"
                            (("QML2_IMPORT_PATH")
                             "QML_IMPORT_PATH")))))
                   #~())))))
    (synopsis "QML based X11 and Wayland display manager")
    (description "SDDM is a display manager for X11 and Wayland aiming to be
fast, simple and beautiful.  SDDM is themeable and puts no restrictions on the
user interface design.  It uses QtQuick which gives the designer the ability
to create smooth, animated user interfaces.")
    (home-page "https://github.com/sddm/sddm")
    ;; QML files are MIT licensed and images are CC BY 3.0.
    (license (list license:gpl2+ license:expat license:cc-by3.0))))

(define-public sddm-qt5
  (package
    (inherit sddm)
    (name "sddm-qt5")
    (native-inputs (modify-inputs (package-native-inputs sddm)
                     (replace "qttools" qttools-5)))
    (inputs (modify-inputs (package-inputs sddm)
              (replace "qtbase" qtbase-5)
              (replace "qtsvg" qtsvg-5)
              (replace "qtdeclarative" qtdeclarative-5)
              (replace "qtwayland" qtwayland-5)
              (append qtgraphicaleffects qtquickcontrols-5 qtquickcontrols2-5)))))

(define-public abstractdark-sddm-theme
  (let ((commit "e817d4b27981080cd3b398fe928619ffa16c52e7")
        (revision "0"))
    (package
      (name "abstractdark-sddm-theme")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/3ximus/abstractdark-sddm-theme")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1si141hnp4lr43q36mbl3anlx0a81r8nqlahz3n3l7zmrxb56s2y"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("." "/share/sddm/themes/abstractdark"))))
      (home-page "https://github.com/3ximus/abstractdark-sddm-theme")
      (synopsis "Abstract Dark theme for SDDM")
      (description
       "This package provides a minimalistic dark theme for SDDM, black
background with abstract shapes.  Inspired by solarized-sddm-theme.")
      (license license:gpl3+))))

(define-public dexy-color-sddm-theme
  (let ((commit "7929384dbb9305e6da53a8942bca3d75593fd99f")
        (revision "0"))
    (package
      (name "dexy-color-sddm-theme")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/L4ki/Dexy-Plasma-Themes")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1dcp3pvs6x63740sz852yr19fjrdnh81dbrq7rssgm6ssi1rqjig"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan '(("Dexy-Color-SDDM"
                           "/share/sddm/themes/dexy-color-sddm"))))
      (home-page "https://github.com/L4ki/Dexy-Plasma-Themes")
      (synopsis "Dexy Color theme for SDDM")
      (description
       "This package provides a minimalistic and modern SDDM theme with blured
background.")
      (license license:gpl3+))))

(define-public guix-simplyblack-sddm-theme
  (package
    (name "guix-simplyblack-sddm-theme")
    (version "0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/plattfot/guix-simplyblack-sddm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09vb9b0pmyhj6fh0b6by59bykszbkdayhz678pnb4pyrdmlvv1am"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (srfi srfi-26))
       #:builder (begin
                   (use-modules (guix build utils)
                                (srfi srfi-26))
                   (let* ((out (assoc-ref %outputs "out"))
                          (themes-dir (string-append out
                                       "/share/sddm/themes/guix-simplyblack-sddm/")))
                     (mkdir-p themes-dir)
                     (copy-recursively (assoc-ref %build-inputs "source")
                                       themes-dir)
                     (substitute* (map (cut string-append themes-dir <>)
                                       '("Main.qml" "theme.conf"))
                       (("file:")
                        themes-dir))))))
    (home-page "https://github.com/plattfot/guix-simplyblack-sddm")
    (synopsis "Guix based theme for SDDM")
    (description
     "This package provides a simple theme for SDDM, black background with
Guix's logo.  Based on Arch linux's archlinux-simplyblack theme.")
    ;; Theme under cc-by-sa3.0, guix logo under license:cc-by-sa4.0
    (license (list license:cc-by-sa3.0 license:cc-by-sa4.0))))

(define-public guix-simplyblack-sddm-theme-qt5
  (package
    (inherit guix-simplyblack-sddm-theme)
    (name "guix-simplyblack-sddm-theme-qt5")
    (source
     (origin
       (inherit (package-source guix-simplyblack-sddm-theme))
       (modules '((guix build utils)))
       (snippet '(begin
                   (substitute* "metadata.desktop"
                     (("QtVersion=6")
                      "QtVersion=5"))))))))

(define-public chili-sddm-theme
  (package
    (name "chili-sddm-theme")
    (version "0.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MarianArlt/sddm-chili")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "036fxsa7m8ymmp3p40z671z163y6fcsa9a641lrxdrw225ssq5f3"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (sddm-themes (string-append out "/share/sddm/themes")))
           (mkdir-p sddm-themes)
           (copy-recursively (assoc-ref %build-inputs "source")
                             (string-append sddm-themes "/chili"))))))
    (home-page "https://github.com/MarianArlt/sddm-chili")
    (synopsis "Chili theme for SDDM")
    (description "Chili reduces all the clutter and leaves you with a clean,
easy to use, login interface with a modern yet classy touch.")
    (license license:gpl3+)))

(define-public sugar-dark-sddm-theme
  (package
    (name "sugar-dark-sddm-theme")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MarianArlt/sddm-sugar-dark")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gx0am7vq1ywaw2rm1p015x90b75ccqxnb1sz3wy8yjl27v82yhb"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (sddm-themes (string-append out "/share/sddm/themes")))
           (mkdir-p sddm-themes)
           (copy-recursively (assoc-ref %build-inputs "source")
                             (string-append sddm-themes "/sugar-dark"))))))
    (home-page "https://github.com/MarianArlt/sddm-sugar-dark")
    (synopsis "Sugar dark theme for SDDM")
    (description "Sugar is extremely customizable and so sweet it will
probably cause you diabetes just from looking at it.  Sweeten the login
experience for your users, your family and yourself")
    (license license:gpl3+)))

(define-public sugar-light-sddm-theme
  (package
    (name "sugar-light-sddm-theme")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MarianArlt/sddm-sugar-light")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sfd5bi5jcfz3hmvvr3smalywixa70g5j96qgx1220mp6rqf886k"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((out (assoc-ref %outputs "out"))
                (sddm-themes (string-append out "/share/sddm/themes")))
           (mkdir-p sddm-themes)
           (copy-recursively (assoc-ref %build-inputs "source")
                             (string-append sddm-themes "/sugar-light"))))))
    (home-page "https://github.com/MarianArlt/sddm-sugar-light")
    (synopsis "Sugar light theme for SDDM")
    (description "Sugar is extremely customizable and so sweet it will
probably cause you diabetes just from looking at it.  Sweeten the login
experience for your users, your family and yourself")
    (license license:gpl3+)))

(define-public lightdm
  (package
    (name "lightdm")
    (version "1.32.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/canonical/lightdm")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wr60c946p8jz9kb8zi4cd8d4mkcy7infbvlfzwajiglc22nblxn"))
              (patches (search-patches "lightdm-arguments-ordering.patch"
                                       "lightdm-vncserver-check.patch"
                                       "lightdm-vnc-color-depth.patch"
                                       "lightdm-vnc-ipv6.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:parallel-tests? #f              ; fails when run in parallel
      #:configure-flags
      #~(list "--localstatedir=/var"
              "--sysconfdir=/etc"
              "--enable-gtk-doc"
              ;; Otherwise the test suite fails on such a warning.
              "CFLAGS=-Wno-error=missing-prototypes")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/shared-data-manager.c"
                (("/bin/rm")
                 (search-input-file inputs "bin/rm")))
              (substitute* '("data/users.conf"
                             "common/user-list.c")
                (("/bin/false")
                 (search-input-file inputs "bin/false"))
                (("/usr/sbin/nologin")
                 (search-input-file inputs "sbin/nologin")))
              (substitute* "src/seat.c"
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))))
          (add-before 'check 'pre-check
            (lambda _
              (wrap-program "tests/src/test-python-greeter"
                #:sh (which "bash")
                `("GUIX_PYTHONPATH"      ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))
              ;; Avoid printing locale warnings, which trip up the text
              ;; matching tests.
              (unsetenv "LC_ALL")))
          (replace 'install
            (lambda* (#:key make-flags #:allow-other-keys #:rest args)
              ;; Override the sysconfdir flag only for the installation phase,
              ;; as it attempts to write the sample config files to /etc and
              ;; fail otherwise.
              (define make-flags*
                (append make-flags (list (string-append "sysconfdir="
                                                        #$output "/etc"))))
              (apply (assoc-ref %standard-phases 'install)
                     (append args (list #:make-flags make-flags*))))))))
    (inputs
     (list audit
           bash-minimal                 ;for cross-compilation
           coreutils-minimal            ;ditto
           linux-pam
           shadow                       ;for sbin/nologin
           libgcrypt
           libxcb
           libxdmcp))
    (native-inputs
     (list accountsservice
           autoconf
           automake
           gobject-introspection
           gtk-doc/stable
           pkg-config
           itstool
           intltool
           libtool
           vala             ;for Vala bindings
           ;; For tests
           ;; All tests fail with dbus >= 1.15.2, see
           ;; https://github.com/canonical/lightdm/issues/346
           dbus-1.15.0
           python-wrapper
           python-pygobject
           which
           yelp-tools))
    ;; Required by liblightdm-gobject-1.pc.
    (propagated-inputs
     (list glib libx11 libxklavier))
    (home-page "https://www.freedesktop.org/wiki/Software/LightDM/")
    (synopsis "Lightweight display manager")
    (description "The Light Display Manager (LightDM) is a cross-desktop
display manager which supports different greeters.")
    (license license:gpl3+)))

(define-public lightdm-gtk-greeter
  (package
    (name "lightdm-gtk-greeter")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/xubuntu/lightdm-gtk-greeter"
                    "/releases/download/lightdm-gtk-greeter-" version "/"
                    "lightdm-gtk-greeter-" version ".tar.gz"))
              (sha256
               (base32
                "04q62mvr97l9gv8h37hfarygqc7p0498ig7xclcg4kxkqw0b7yxy"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-indicator-services-command" ;requires upstart
              ;; Put the binary under /bin rather than /sbin, so that it gets
              ;; wrapped by the glib-or-gtk-wrap phase.
              (string-append "--sbindir=" #$output "/bin")
              (string-append "--with-libxklavier")
              (string-append "--enable-at-spi-command="
                             (search-input-file
                              %build-inputs "libexec/at-spi-bus-launcher")
                             " --launch-immediately"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'customize-default-config-path
            (lambda _
              (substitute* "src/Makefile.in"
                ;; Have the default config directory sourced from
                ;; /etc/lightdm/lightdm-gtk-greeter.conf, which is where the
                ;; lightdm service writes it.
                (("\\$\\(sysconfdir)/lightdm/lightdm-gtk-greeter.conf")
                 "/etc/lightdm/lightdm-gtk-greeter.conf"))))
          (add-after 'install 'fix-.desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (search-input-file
                            outputs
                            "share/xgreeters/lightdm-gtk-greeter.desktop")
                (("Exec=lightdm-gtk-greeter")
                 (string-append "Exec="
                                (search-input-file
                                 outputs "bin/lightdm-gtk-greeter"))))))
          (add-after 'glib-or-gtk-wrap 'custom-wrap
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-script (search-input-file
                            outputs "bin/lightdm-gtk-greeter")
                ;; Wrap GDK_PIXBUF_MODULE_FILE, so that the SVG loader is
                ;; available at all times even outside of profiles, such as
                ;; when used in the lightdm-service-type.  Otherwise, it
                ;; wouldn't be able to display its own icons.
                `("GDK_PIXBUF_MODULE_FILE" =
                  (,(search-input-file
                     outputs
                     "lib/gdk-pixbuf-2.0/2.10.0/loaders.cache")))
                `("XDG_DATA_DIRS" ":" prefix
                  (,(string-append "/run/current-system/profile/share:"
                                   (getenv "XDG_DATA_DIRS"))))
                '("XCURSOR_PATH" ":" prefix
                  ("/run/current-system/profile/share/icons"))))))))
    (native-inputs
     (list exo
           intltool
           pkg-config
           xfce4-dev-tools))
    (inputs
     (list at-spi2-core
           bash-minimal                 ;for wrap-program
           gtk+
           guile-3.0
           (librsvg-for-system)
           libxklavier
           lightdm
           shared-mime-info))
    (synopsis "GTK+ greeter for LightDM")
    (home-page "https://github.com/xubuntu/lightdm-gtk-greeter")
    (description "This package provides a LightDM greeter implementation using
GTK+, lets you select a desktop session and log in to it.")
    (license license:gpl3+)))

(define-public lightdm-mini-greeter
  (let ((commit "ead7936993b4e9e067d73fa49dec7edfb46c73a8")
        (revision "0"))
    (package
      (name "lightdm-mini-greeter")
      ;; Version 0.5.1 release in 2021, so we use a recent commit.
      (version (git-version "0.5.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/prikhi/lightdm-mini-greeter")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "17iy1jkllmi2xc95csb18wcfvbk44gyva2in2k5f29fy362ppz25"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'customize-default-config-path
              (lambda _
                (substitute* "Makefile.am"
                  ;; Have the default config directory sourced from
                  ;; /etc/lightdm/lightdm-mini-greeter.conf, which is where the
                  ;; lightdm service writes it.
                  (("\\$\\(sysconfdir)/lightdm/lightdm-mini-greeter.conf")
                   "/etc/lightdm/lightdm-mini-greeter.conf"))))
            (add-after 'install 'fix-.desktop-file
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* (search-input-file
                              outputs
                              "share/xgreeters/lightdm-mini-greeter.desktop")
                  (("Exec=lightdm-mini-greeter")
                   (string-append "Exec="
                                  (search-input-file
                                   outputs "bin/lightdm-mini-greeter")))))))))
      (native-inputs
       (list autoconf automake pkg-config))
      (inputs
       (list gtk+ lightdm))
      (synopsis "Mini Greeter for LightDM")
      (home-page "https://github.com/prikhi/lightdm-mini-greeter")
      (description "This package provide a minimal but highly configurable
single-user GTK3 greeter for LightDM, this greeter is inspired by the SLiM
Display Manager and LightDM GTK3 Greeter.")
      (license license:gpl3))))

(define-public lightdm-tiny-greeter
  (let ((commit "6717c5853315ebd8164b1ddf85b9483f92cbcae8")
        (revision "0"))
    (package
      (name "lightdm-tiny-greeter")
      ;; Version 1.2 release in 2021, so we use a recent commit.
      (version (git-version "1.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/tobiohlala/lightdm-tiny-greeter")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n970d6525fd918i1j09akxiacqbpxni8apkfi542bq5zg5crjbs"))))
      (build-system glib-or-gtk-build-system)
      (arguments
       (list
        #:tests? #f ; No test target.
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-after 'unpack 'patch-hardcoded-paths
              (lambda _
                (substitute* "Makefile"
                  (("PREFIX = /usr")
                   (string-append "PREFIX = " #$output))
                  (("/usr/share/xgreeters")
                   (string-append #$output "/share/xgreeters"))
                  (("cp lightdm-tiny-greeter")
                   "mkdir -p $(PREFIX)/bin; cp lightdm-tiny-greeter"))))
            (add-after 'glib-or-gtk-wrap 'custom-wrap
              (lambda _
                (wrap-script (string-append #$output "/bin/lightdm-tiny-greeter")
                  ;; Wrap GDK_PIXBUF_MODULE_FILE, so that the SVG loader is
                  ;; available at all times even outside of profiles, such as
                  ;; when used in the lightdm-service-type.  Otherwise, it
                  ;; wouldn't be able to display its own icons.
                  `("GDK_PIXBUF_MODULE_FILE" =
                    (,(string-append
                       #$output
                       "/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache")))
                  `("XDG_DATA_DIRS" ":" prefix
                    (,(string-append "/run/current-system/profile/share:"
                                     (getenv "XDG_DATA_DIRS"))))
                  '("XCURSOR_PATH" ":" prefix
                    ("/run/current-system/profile/share/icons")))))
            (add-after 'install 'fix-.desktop-file
              (lambda _
                (substitute* (string-append
                              #$output "/share/xgreeters/lightdm-tiny-greeter.desktop")
                  (("Exec=lightdm-tiny-greeter")
                   (string-append "Exec="
                                  (string-append
                                   #$output "/bin/lightdm-tiny-greeter")))))))))
      (native-inputs
       (list pkg-config))
      (inputs
       (list gtk+ lightdm))
      (synopsis "Tiny Greeter for LightDM")
      (home-page "https://github.com/tobiohlala/lightdm-tiny-greeter")
      (description "This package provides a tiny yet customizable GTK3 LightDM
Greeter with focus on code and minimalism.")
      (license license:bsd-3))))

(define* (customize-lightdm-tiny-greeter #:key name session
                                         user_text pass_text
                                         fontname fontsize)
  "Make a customized lightdm-tiny-greeter package which name is NAME.

This function will change SESSION, USER_TEXT, PASS_TEXT, FONTNAME and FONTSIZE
in config.h of lightdm-tiny-greeter."
  (package
    (inherit lightdm-tiny-greeter)
    (name (or name (string-append
                    (package-name lightdm-tiny-greeter)
                    "-" (or session "default"))))
    (arguments
     (substitute-keyword-arguments
         (package-arguments lightdm-tiny-greeter)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-config-h
              (lambda _
                (when #$user_text
                  (substitute* "config.h"
                    (("\\*user_text = .*;")
                     (string-append "*user_text = \"" #$user_text "\";"))))
                (when #$pass_text
                  (substitute* "config.h"
                    (("\\*pass_text = .*;")
                     (string-append "*pass_text = \"" #$pass_text "\";"))))
                (when #$session
                  (substitute* "config.h"
                    (("\\*session = .*;")
                     (string-append "*session = \"" #$session "\";"))))
                (when #$fontname
                  (substitute* "config.h"
                    (("font: .*px .*;")
                     (string-append "font: 16px \\\"" #$fontname "\\\";"))))
                (when #$fontsize
                  (substitute* "config.h"
                    (("font: .*px")
                     (string-append "font: " #$fontsize "px"))))))))))))

(define-public slim
  (package
    (name "slim")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/slim-fork/slim-" version
                           ".tar.gz"))
       (sha256
        (base32 "06r47ypf9lsy76jikrvihw8ka9j2wbrnn8g3sbxp819hcbqxg22z"))
       (patches (search-patches "slim-config.patch"
                                "slim-login.patch"
                                "slim-display.patch"))))
    (build-system cmake-build-system)
    (inputs (list fontconfig
                  freeglut
                  freetype
                  libjpeg-turbo
                  libpng
                  libx11
                  libxcrypt
                  libxft
                  libxmu
                  libxrandr
                  libxrender
                  linux-pam
                  xauth))
    (native-inputs (list pkg-config))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'set-new-etc-location
            (lambda _
              (substitute* "CMakeLists.txt"
                (("/etc")
                 (string-append #$output "/etc"))
                (("install.*SYSTEMDDIR.*")
                 ;; The build system's logic here is: if "Linux", then
                 ;; "systemd".  Strip that.
                 "")))))
      #:configure-flags
      #~(list "-DUSE_PAM=yes" "-DUSE_CONSOLEKIT=no")
      #:tests? #f))
    ;; The original project (https://github.com/iwamatsu/slim) has not been
    ;; maintained since 2013, so we use slim-fork instead.
    (home-page "https://slim-fork.sourceforge.io/")
    (synopsis "Desktop-independent graphical login manager for X11")
    (description
     "SLiM is a Desktop-independent graphical login manager for X11, derived
from Login.app.  It aims to be light and simple, although completely
configurable through themes and an option file; is suitable for machines on
which remote login functionalities are not needed.

Features included: PNG and XFT support for alpha transparency and antialiased
fonts, External themes support, Configurable runtime options: X server --
login / shutdown / reboot commands, Single (GDM-like) or double (XDM-like)
input control, Can load predefined user at startup, Configurable welcome /
shutdown messages, Random theme selection.")
    (license license:gpl2)))
