;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Tomáš Čech <sleep_walker@suse.cz>
;;; Copyright © 2015 Daniel Pimentel <d4n1@member.fsf.org>
;;; Copyright © 2015-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Timo Eisenmann <eisenmann@fn.de>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages enlightenment)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bittorrent)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages ibus)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public efl
  (package
    (name "efl")
    (version "1.26.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.enlightenment.org/rel/libs/efl/efl-"
                    version ".tar.xz"))
              (sha256
               (base32
                "05bxc58hj0z6pkp6yy5cmy1lc575q0nrbr5lxr6z8d4kznh3my6r"))))
    (build-system meson-build-system)
    (native-inputs
     (list check
           gettext-minimal
           pkg-config
           python))
    (inputs
     (list curl
           giflib
           gstreamer
           gst-plugins-base
           ibus-minimal
           mesa
           libraw
           (librsvg-for-system)
           libspectre
           libtiff
           libxau
           libxcomposite
           libxcursor
           libxdamage
           libxdmcp
           libxext
           libxi
           libxfixes
           libxinerama
           libxrandr
           libxrender
           libxscrnsaver
           libxtst
           libwebp
           openjpeg
           poppler
           `(,util-linux "lib")
           wayland-protocols))
    (propagated-inputs
     ;; All these inputs are in package config files in section
     ;; Requires.private.
     (append
       (list dbus
             elogind
             eudev
             fontconfig
             freetype
             fribidi
             glib
             harfbuzz
             libinput-minimal
             libjpeg-turbo
             libsndfile
             libpng
             libunwind
             libx11
             libxkbcommon)
       (if (member (%current-system)
                   (package-transitive-supported-systems luajit))
         (list luajit)
         (list lua-5.2))
       (list lz4
             openssl
             pulseaudio
             wayland
             zlib)))
    (arguments
     `(#:configure-flags
       `("-Dembedded-lz4=false"
         "-Dbuild-examples=false"
         "-Decore-imf-loaders-disabler=scim"
         "-Dglib=true"
         "-Dmount-path=/run/privileged/bin/mount"
         "-Dunmount-path=/run/privileged/bin/umount"
         "-Dnetwork-backend=connman"
         ,,@(if (member (%current-system)
                        (package-transitive-supported-systems luajit))
              `("-Dlua-interpreter=luajit")
              `("-Dlua-interpreter=lua"))
         ;; For Wayland.
         "-Dwl=true"
         "-Ddrm=true")
       #:tests? #f     ; Many tests fail due to timeouts and network requests.
       #:phases
       (modify-phases %standard-phases
         ;; If we don't hardcode the location of libcurl.so and others then we
         ;; have to wrap the outputs of efl's dependencies in those libraries.
         (add-after 'unpack 'hardcode-dynamic-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/lib/ecore_con/ecore_con_url_curl.c"
               (("libcurl.so.4")
                (search-input-file inputs "lib/libcurl.so.4")))
             (substitute* "src/lib/ecore_audio/ecore_audio.c"
               (("libpulse.so.0")
                (search-input-file inputs "lib/libpulse.so.0"))
               (("libsndfile.so.1")
                (search-input-file inputs "lib/libsndfile.so.1")))
             (substitute* "src/lib/elput/elput_logind.c"
               (("libelogind.so.0")
                (search-input-file inputs "lib/libelogind.so.0")))))
         (add-after 'unpack 'fix-install-paths
           (lambda _
             (substitute* "dbus-services/meson.build"
               (("install_dir.*")
                "install_dir: join_paths(dir_data, 'dbus-1', 'services'))\n"))
             (substitute* "src/tests/elementary/meson.build"
               (("dir_data") "meson.source_root(), 'test-output'"))
             (substitute* "data/eo/meson.build"
               (("'usr', 'lib'") "'./' + dir_lib"))))
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp"))))))
    (home-page "https://www.enlightenment.org/about-efl")
    (synopsis "Enlightenment Foundation Libraries")
    (description
     "Enlightenment Foundation Libraries is a set of libraries developed
for Enlightenment.  Libraries covers data serialization, wide support for
graphics rendering, UI layout and themes, interaction with OS, access to
removable devices or support for multimedia.")
    ;; Different parts are under different licenses.
    (license (list license:bsd-2 license:lgpl2.1 license:zlib))))

(define-public terminology
  (package
    (name "terminology")
    (version "1.13.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://download.enlightenment.org/rel/apps/"
                              "terminology/terminology-" version ".tar.xz"))
              (sha256
               (base32
                "19ad8ycg5mkk2ldqm0ysvzb7gvhw6fpw1iiyvv4kxmmvszn7z8qn"))
              (modules '((guix build utils)))
              ;; Remove the bundled fonts.
              (snippet
               '(begin
                  (delete-file-recursively "data/fonts")
                  (substitute* "data/meson.build"
                    (("subdir\\('fonts'\\)") ""))))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (let ((efl (assoc-ref %build-inputs "efl")))
         (list "-Dtests=true"
               (string-append "-Dedje-cc=" efl "/bin/edje_cc")
               (string-append "-Deet=" efl "/bin/eet")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp")))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion
                 (string-append "../" ,name "-" ,version "/tests")
                 (invoke "sh" "run_tests.sh" "--verbose"
                         "-t" "../../build/src/bin/tytest")))))
         (add-after 'install 'remove-test-binary
           (lambda* (#:key outputs #:allow-other-keys)
             ;; This file is not meant to be installed.
             (delete-file (string-append (assoc-ref outputs "out")
                                         "/bin/tytest")))))))
    (native-inputs
     (list gettext-minimal
           perl
           pkg-config
           python))
    (inputs
     (list efl))
    (home-page "https://www.enlightenment.org/about-terminology")
    (synopsis "Powerful terminal emulator based on EFL")
    (description
     "Terminology is fast and feature rich terminal emulator.  It is solely
based on Enlightenment Foundation Libraries.  It supports multiple tabs, UTF-8,
URL and local path detection, themes, popup based content viewer for non-text
contents and more.")
    (license license:bsd-2)))

(define-public rage
  (package
    (name "rage")
    (version "0.4.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "https://download.enlightenment.org/rel/apps/rage/rage-"
                version ".tar.xz"))
              (sha256
               (base32
                "03yal7ajh57x2jhmygc6msf3gzvqkpmzkqzj6dnam5sim8cq9rbw"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list efl))
    (home-page "https://www.enlightenment.org/about-rage")
    (synopsis "Video and audio player based on EFL")
    (description
     "Rage is a video and audio player written with Enlightenment Foundation
Libraries with some extra bells and whistles.")
    (license license:bsd-2)))

(define-public enlightenment
  (package
    (name "enlightenment")
    (version "0.25.4")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "https://download.enlightenment.org/rel/apps/"
                              "enlightenment/enlightenment-" version ".tar.xz"))
              (sha256
               (base32
                "18mp4ggfy7n0yz6kzx56ali0piqa84z72vnj6649l6w2dch5vnsn"))
              (patches (search-patches "enlightenment-fix-setuid-path.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list "-Dsystemd=false"
             "-Dpackagekit=false"
             "-Dwl=true")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-system-actions
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" "/tmp")
             (let ((xkeyboard (assoc-ref inputs "xkeyboard-config"))
                   (setxkbmap (assoc-ref inputs "setxkbmap"))
                   (libc      (assoc-ref inputs "libc"))
                   (bc        (assoc-ref inputs "bc"))
                   (ddcutil   (assoc-ref inputs "ddcutil"))
                   (efl       (assoc-ref inputs "efl")))
               ;; We need to patch the path to 'base.lst' to be able
               ;; to switch the keyboard layout in E.
               (substitute* (list "src/modules/xkbswitch/e_mod_parse.c"
                                  "src/modules/wizard/page_011.c")
                 (("/usr/share/X11/xkb/rules/xorg.lst")
                  (string-append xkeyboard
                                 "/share/X11/xkb/rules/base.lst")))
               (substitute* "src/bin/e_xkb.c"
                 (("\"setxkbmap \"")
                  (string-append "\"" setxkbmap "/bin/setxkbmap \"")))
               (substitute* (list "src/bin/e_intl.c"
                                  "src/modules/conf_intl/e_int_config_intl.c"
                                  "src/modules/wizard/page_010.c")
                 (("locale -a") (string-append libc "/bin/locale -a")))
               (substitute* "src/modules/everything/evry_plug_apps.c"
                 (("/usr/bin/") ""))
               (substitute* '("src/bin/e_sys_main.c"
                              "src/bin/e_util_suid.h")
                 (("PATH=/bin:/usr/bin:/sbin:/usr/sbin")
                  (string-append "PATH=/run/privileged/bin:"
                                 "/run/current-system/profile/bin:"
                                 "/run/current-system/profile/sbin")))
               (substitute* "src/modules/everything/evry_plug_calc.c"
                 (("bc -l") (string-append bc "/bin/bc -l")))
               (substitute* "src/bin/system/e_system_ddc.c"
                 (("libddcutil\\.so\\.?" libddcutil)
                  (string-append ddcutil "/lib/" libddcutil)))
               (substitute* "data/etc/meson.build"
                 (("/bin/mount") "/run/privileged/bin/mount")
                 (("/bin/umount") "/run/privileged/bin/umount")
                 (("/usr/bin/eject") "/run/current-system/profile/bin/eject"))
               (substitute* "src/bin/system/e_system_power.c"
                 (("systemctl") "loginctl"))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bc" ,bc)
       ("bluez" ,bluez)
       ("dbus" ,dbus)
       ("ddcutil" ,ddcutil)
       ("freetype" ,freetype)
       ("libdrm" ,libdrm)
       ("libexif" ,libexif)
       ("libxcb" ,libxcb)
       ("libxext" ,libxext)
       ("linux-pam" ,linux-pam)
       ("pulseaudio" ,pulseaudio)
       ("setxkbmap" ,setxkbmap)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xkeyboard-config" ,xkeyboard-config)
       ("xorg-server-xwayland" ,xorg-server-xwayland)))
    (propagated-inputs
     (list efl libxkbcommon wayland-protocols
           ;; Default font that applications such as IceCat require.
           font-dejavu))
    (home-page "https://www.enlightenment.org/about-enlightenment")
    (synopsis "Lightweight desktop environment")
    (description
     "Enlightenment is resource friendly desktop environment with integrated
file manager, wide range of configuration options, plugin system allowing to
unload unused functionality, with support for touchscreen and suitable for
embedded systems.")
    (license license:bsd-2)))

(define-public python-efl
  (package
    (name "python-efl")
    (version "1.26.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/bindings/"
                            "python/python-efl-" version ".tar.xz"))
        (sha256
         (base32
          "0dj6f24n33hkpy0bkdclnzpxhvs8vpaxqaf7hkw0di19pjwrq25h"))
        (modules '((guix build utils)))
        ;; Remove files generated by Cython
        (snippet
         '(begin
            (for-each (lambda (file)
                        (let ((generated-file
                                (string-append (string-drop-right file 3) "c")))
                          (when (file-exists? generated-file)
                            (delete-file generated-file))))
                      (find-files "efl" "\\.pyx$"))
            (delete-file "efl/eo/efl.eo_api.h")))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (setenv "ENABLE_CYTHON" "1")
             (invoke "python" "setup.py" "build")))
        (add-before 'build 'set-flags
          (lambda _
            (setenv "CFLAGS"
                    (string-append "-I" (assoc-ref %build-inputs "python-dbus")
                                   "/include/dbus-1.0"))))
        (add-before 'check 'set-environment
          (lambda _
            ;; Some tests require write access to HOME.
            (setenv "HOME" "/tmp")
            ;; These tests try to connect to the internet.
            (delete-file "tests/ecore/test_09_file_download.py")
            (delete-file "tests/ecore/test_11_con.py"))))))
    (native-inputs
     (list pkg-config python-cython))
    (inputs
     (list efl python-dbus python-packaging))
    (home-page "https://www.enlightenment.org/")
    (synopsis "Python bindings for EFL")
    (description
     "PYTHON-EFL are the python bindings for the whole Enlightenment Foundation
Libraries stack (eo, evas, ecore, edje, emotion, ethumb and elementary).")
    (license license:lgpl3)))

(define-public edi
  (package
    (name "edi")
    (version "0.8.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/Enlightenment/edi/releases/"
                            "download/v" version "/edi-" version ".tar.xz"))
        (sha256
         (base32
          "01k8gp8r2wa6pyg3dkbm35m6hdsbss06hybghg0qjmd4mzswcd3a"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-clang-header
           (lambda _
             (substitute* "scripts/clang_include_dir.sh"
               (("grep clang") "grep clang | head -n1"))
             #t))
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp") #t)))
       #:tests? #f)) ; tests require running dbus service
    (native-inputs
     `(("check" ,check)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list clang efl))
    (home-page "https://www.enlightenment.org/about-edi")
    (synopsis "Development environment for Enlightenment")
    (description "EDI is a development environment designed for and built using
the EFL.  It's aim is to create a new, native development environment for Linux
that tries to lower the barrier to getting involved in Enlightenment development
and in creating applications based on the Enlightenment Foundation Library suite.")
    (license (list license:public-domain ; data/extra/skeleton
                   license:gpl2          ; edi
                   license:gpl3))))      ; data/extra/examples/images/mono-runtime.png

(define-public ephoto
  (package
    (name "ephoto")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.enlightenment.org/rel/"
                           "apps/ephoto/ephoto-" version ".tar.xz"))
       (sha256
        (base32 "1lvhcs4ba8h3z78nyycbww8mj4cscb8k200dcc3cdy8vrvrp7g1n"))))
    (build-system meson-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-home-directory
           ;; FATAL: Cannot create run dir '/homeless-shelter/.run' - errno=2
           (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list efl))
    (home-page "https://smhouston.us/projects/ephoto/")
    (synopsis "EFL image viewer/editor/manipulator/slideshow creator")
    (description "Ephoto is an image viewer and editor written using the
@dfn{Enlightenment Foundation Libraries} (EFL).  It focuses on simplicity and
ease of use, while taking advantage of the speed and small footprint the EFL
provide.

Ephoto’s features include:
@enumerate
@item Browsing the file system and displaying images in an easy-to-use grid view.
@item Browsing images in a single image view format.
@item Viewing images in a slideshow.
@item Editing your image with features such as cropping, auto enhance,
blurring, sharpening, brightness/contrast/gamma adjustments, hue/saturation/value
adjustments, and color level adjustment.
@item Applying artistic filters to your image such as black and white and old
photo.
@item Drag And Drop along with file operations to easily maintain your photo
directories.
@end enumerate\n")
    (license (list
               license:bsd-2 ; Ephoto's thumbnailing code
               license:bsd-3))))

(define-public evisum
  (package
    (name "evisum")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/"
                            "evisum/evisum-" version ".tar.xz"))
        (sha256
         (base32 "1ip3rmp0hcn0pk6lv089cayx18p1b2wycgvwpnf7ghbdxg7n4q15"))))
    (build-system meson-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-homedir
           (lambda _
             (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list gettext-minimal
           pkg-config))
    (inputs
     (list efl))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL process viewer")
    (description
     "This is a process monitor and system monitor using the
@dfn{Enlightenment Foundation Libraries} (EFL).")
    (license license:bsd-2)))

(define-public epour
  (package
    (name "epour")
    (version "0.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://download.enlightenment.org/rel/apps/epour"
                            "/epour-" version ".tar.xz"))
        (sha256
         (base32
          "0g9f9p01hsq6dcf4cs1pwq95g6fpkyjgwqlvdjk1km1i5gj5ygqw"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f      ; no test target
       #:use-setuptools? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-theme-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "epour/gui/__init__.py"
                 (("join\\(data_path")
                  (string-append "join(\"" out "/share/epour\"")))
               #t))))))
    (native-inputs
     (list intltool python-distutils-extra))
    (inputs
     (list libtorrent-rasterbar-1.2 python-dbus python-efl python-pyxdg))
    (home-page "https://www.enlightenment.org")
    (synopsis "EFL Bittorrent client")
    (description "Epour is a BitTorrent client based on the @dfn{Enlightenment
Foundation Libraries} (EFL) and rb-libtorrent.")
    (license license:gpl3+)))
