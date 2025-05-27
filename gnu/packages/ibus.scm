;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017, 2018, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021 Peng Mei Yu <pengmeiyu@riseup.net>
;;; Copyright © 2020 kanichos <kanichos@yandex.ru>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Songlin Jiang <hollowman@hollowman.ml>
;;; Copyright © 2021 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2023 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 Charles <charles@charje.net>
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

(define-module (gnu packages ibus)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages language)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1))

(define-public ibus-minimal
  (package
    (name "ibus")
    (version "1.5.29")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ibus/ibus/"
                                  "releases/download/"
                                  version "/ibus-" version ".tar.gz"))
              (sha256
               (base32
                "0vjybn3xq5sz616fdy21f5c4b4ajrj4wmfnbjqz6584xw887yiaa"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     (list
      #:configure-flags #~(list "--disable-gtk2"
                                "--enable-gtk-doc"
                                "--enable-memconf"
                                (string-append
                                 "--with-unicode-emoji-dir="
                                 (search-input-directory %build-inputs
                                                         "share/unicode/emoji"))
                                (string-append
                                 "--with-emoji-annotation-dir="
                                 (search-input-directory
                                  %build-inputs
                                  "share/unicode/cldr/common/annotations"))
                                (string-append
                                 "--with-ucd-dir="
                                 (search-input-directory %build-inputs
                                                         "share/ucd"))
                                "--enable-wayland"
                                "--disable-systemd-services")
      #:make-flags
      ;; The GUI tests not only require a DISPLAY, but also a window manager
      ;; since IBus needs to receive focus-in/out events to test IBus with GTK
      ;; applications (see: https://github.com/ibus/ibus/issues/2307).
      #~(list (string-append "DISABLE_GUI_TESTS=ibus-compose "
                             "ibus-inputcontext-create "
                             "xkb-latin-layouts "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              ;; These tests require /etc/machine-id.
              (with-directory-excursion "src/tests"
                (substitute* '("ibus-share.c" "ibus-compose.c"
                               "ibus-keypress.c")
                  (("[ \t]*return g_test_run \\(\\);") "")))))
          (add-after 'unpack 'patch-python-target-directories
            (lambda _
              (let ((root (string-append #$output
                                         "/lib/python"
                                         #$(version-major+minor
                                            (package-version python))
                                         "/site-packages")))
                (substitute* "configure"
                  (("(py2?overridesdir)=.*" _ var)
                   (string-append var "=" root "/gi/overrides/"))
                  (("(pkgpython2dir=).*" _ var)
                   (string-append var root "/ibus"))))))
          (add-before 'configure 'disable-dconf-update
            (lambda _
              (substitute* "data/dconf/Makefile.in"
                (("dconf update") "echo dconf update"))))
          (add-after 'unpack 'delete-generated-files
            (lambda _
              (for-each (lambda (file)
                          (let ((c (string-append (string-drop-right file 4) "c")))
                            (when (file-exists? c)
                              (format #t "deleting ~a\n" c)
                              (delete-file c))))
                        (find-files "." "\\.vala"))))
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/ibusenginesimple.c"
                (("/usr/share/X11/locale")
                 (search-input-directory inputs "share/X11/locale")))
              (substitute* "ui/gtk3/xkblayout.vala"
                (("\"(setxkbmap|xmodmap)\"" _ prog)
                 (format #f "~s" (search-input-file
                                  inputs (string-append "bin/" prog)))))))
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests write to $HOME.
              (setenv "HOME" (getcwd))
              ;; Tests look for $XDG_RUNTIME_DIR.
              (setenv "XDG_RUNTIME_DIR" (getcwd))
              ;; For missing '/etc/machine-id'.
              (setenv "DBUS_FATAL_WARNINGS" "0")
              ;; Tests require a running X server.
              (system "Xvfb :1 +extension GLX &")
              (setenv "DISPLAY" ":1")
              ;; Tests require running iBus daemon.
              (system "./bus/ibus-daemon --daemonize")))
          (add-after 'install 'move-doc
            (lambda _
              (mkdir-p (string-append #$output:doc "/share"))
              (rename-file
               (string-append #$output "/share/gtk-doc")
               (string-append #$output:doc "/share/gtk-doc"))))
          (add-after 'glib-or-gtk-wrap 'wrap-with-additional-paths
            (lambda* (#:key outputs #:allow-other-keys)
              ;; Make sure 'ibus-setup' runs with the correct GI_TYPELIB_PATH.
              (wrap-program (search-input-file outputs "bin/ibus-setup")
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")
                   ,(string-append #$output "/lib/girepository-1.0")))))))))
    (inputs
     (list bash-minimal
           dbus
           dconf
           glib
           gtk+
           iso-codes/pinned
           json-glib
           libdbusmenu
           libnotify
           libx11
           libxkbcommon
           libxtst
           setxkbmap
           ucd
           unicode-cldr-common
           unicode-emoji
           wayland
           xmodmap))
    (native-inputs
     (list docbook-xml-4.1.2
           `(,glib "bin")               ;for glib-genmarshal
           gettext-minimal
           gnome-common
           gobject-introspection        ;for g-ir-compiler
           `(,gtk+ "bin")
           gtk-doc/stable
           perl
           pkg-config
           python-wrapper
           vala
           which
           xorg-server-for-tests))
    (native-search-paths
     (list (search-path-specification
            (variable "IBUS_COMPONENT_PATH")
            (files '("share/ibus/component")))))
    (synopsis "Input method framework")
    (description
     "IBus is an input framework providing a full-featured and user-friendly
input method user interface.  It comes with multilingual input support.  It
may also simplify input method development.")
    (home-page "https://github.com/ibus/ibus/wiki")
    (license lgpl2.1+)
    (properties '((hidden? . #t)))))

(define-public ibus
  (package/inherit ibus-minimal
    (arguments
     (substitute-keyword-arguments (package-arguments ibus-minimal)
       ((#:configure-flags flags)
        #~(cons* "--enable-gtk4"
                 "--enable-python-library"
                 #$flags))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'disable-registry-cache
              ;; IBus registry cache depends on mtime, which doesn't work on
              ;; Guix.
              (lambda _
                (substitute* "bus/main.c"
                  (("ibus_init") "g_cache = \"none\"; ibus_init"))))
            (replace 'wrap-with-additional-paths
              (lambda* (#:key outputs #:allow-other-keys)
                ;; Make sure 'ibus-setup' and 'ibus-daemon' runs with the
                ;; correct GUIX_PYTHONPATH and GI_TYPELIB_PATH.  Wrap
                ;; 'ibus-daemon' is needed because engines spawned by
                ;; the daemon need access to those libraries.
                (for-each
                  (lambda (prog)
                    (wrap-program prog
                      `("GUIX_PYTHONPATH" ":" prefix
                        (,(getenv "GUIX_PYTHONPATH")))
                      `("GI_TYPELIB_PATH" ":" prefix
                        (,(getenv "GI_TYPELIB_PATH")
                         ,(string-append #$output "/lib/girepository-1.0")))))
                  (list (search-input-file outputs "bin/ibus-setup")
                        (search-input-file outputs "bin/ibus-daemon")))))))))
    (inputs (modify-inputs (package-inputs ibus-minimal)
              (prepend gtk
                       pango
                       python
                       python-dbus
                       python-pygobject)))
    (native-search-paths
     (cons (search-path-specification
            (variable "GUIX_GTK3_IM_MODULE_FILE")
            (file-type 'regular)
            (separator #f)
            (files '("lib/gtk-3.0/3.0.0/immodules-gtk3.cache")))
           (package-native-search-paths ibus-minimal)))
    (properties (alist-delete 'hidden? (package-properties ibus-minimal)))))

(define-public ibus-libpinyin
  (package
    (name "ibus-libpinyin")
    (version "1.15.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libpinyin/ibus-libpinyin/"
                                  "releases/download/" version
                                  "/ibus-libpinyin-" version ".tar.gz"))
              (sha256
               (base32
                "01zsx3aw9iwjm70mksgpjlqjj5f5wi9l0pdixprw5lj5hxd8siyp"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags
      '(list "--enable-opencc")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'glib-or-gtk-wrap 'wrap-with-additional-paths
            (lambda _
              ;; Make sure 'ibus-setup-libpinyin' runs with the correct
              ;; PYTHONPATH and GI_TYPELIB_PATH.
              (wrap-program (string-append #$output "/libexec/ibus-setup-libpinyin")
                `("GUIX_PYTHONPATH" ":" prefix
                  (,(getenv "GUIX_PYTHONPATH")
                   ,(string-append #$(this-package-input "ibus")
                                   "/lib/girepository-1.0")
                   ,(string-append #$output
                                   "/share/ibus-libpinyin/setup/")))
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(string-append #$(this-package-input "ibus")
                                   "/lib/girepository-1.0")
                   ,(string-append #$(this-package-input "gtk+")
                                   "/lib/girepository-1.0")
                   ,(string-append #$output
                                   "/share/ibus-libpinyin/setup/")
                   ,(getenv "GI_TYPELIB_PATH")))))))))
    (inputs
     (list bash-minimal
           ibus
           libpinyin
           bdb
           sqlite
           opencc
           python
           python-pygobject
           gtk+))
    (native-inputs
     (list pkg-config intltool
           `(,glib "bin")))
    (synopsis "Chinese pinyin and ZhuYin input methods for IBus")
    (description
     "This package includes a Chinese pinyin input method and a Chinese
ZhuYin (Bopomofo) input method based on libpinyin for IBus.")
    (home-page "https://github.com/libpinyin/ibus-libpinyin")
    (license gpl3+)))

(define-public libpinyin
  (package
    (name "libpinyin")
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/libpinyin/libpinyin/"
                                  "releases/download/" version
                                  "/libpinyin-" version ".tar.gz"))
              (sha256
               (base32
                "0l4h1q2l5fql0fy9bmncyw0dpbfwn1yb5p3xnwvhgpbidpq58c9m"))))
    (build-system gnu-build-system)
    (inputs
     (list glib bdb))
    (native-inputs
     (list pkg-config))
    (synopsis "Library to handle Chinese pinyin")
    (description
     "The libpinyin C++ library provides algorithms needed for sentence-based
Chinese pinyin input methods.")
    (home-page "https://github.com/libpinyin/libpinyin")
    (license gpl2+)))

(define-public ibus-chewing
  (package
    (name "ibus-chewing")
    (version "2.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/chewing/ibus-chewing")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fgscxb8nhli4g8d3yy0wxzbk9bcyj6bvmqrzbddkvgikmanj36b"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Settings schema 'org.freedesktop.IBus.Chewing' is not installed
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-for-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; IBus requires write access to the HOME directory.
                (setenv "HOME" (getcwd))
                ;; MESA: error: ZINK: failed to choose pdev
                (setenv "GALLIUM_DRIVER" "llvmpipe")
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1")))))))
    (inputs
     (list glib
           gtk
           ibus
           libadwaita
           libchewing))
    (native-inputs
     (list dbus
           gettext-minimal
           gobject-introspection
           `(,glib "bin")
           pkg-config
           xorg-server-for-tests))
    (home-page "https://chewing.im/")
    (synopsis "Chewing engine for IBus")
    (description "IBus-Chewing is an IBus front-end of Chewing, an intelligent
Chinese input method for Zhuyin (BoPoMoFo) users.")
    (license gpl2)))

(define-public ibus-anthy
  (package
    (name "ibus-anthy")
    (version "1.5.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ibus/ibus-anthy/releases/download/"
                    version "/ibus-anthy-" version ".tar.gz"))
              (sha256
               (base32
                "12yrgqiq6mqc8jr49dgkk3d7mdnyqic4xs597biwjjkahgaydi2q"))
              (patches (search-patches "ibus-anthy-fix-tests.patch"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      ;; The test suite hangs (see:
      ;; https://github.com/ibus/ibus-anthy/pull/35).
      #:tests? #f
      #:configure-flags
      ;; Use absolute exec path in the anthy.xml.
      #~(list (string-append "--libexecdir=" #$output "/libexec")
              (string-append
               "--with-anthy-zipcode="
               (assoc-ref %build-inputs "anthy") "/share/anthy/zipcode.t"))
      ;; The test suite fails (see:
      ;; https://github.com/ibus/ibus-anthy/issues/28).
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-check
            (lambda _
              (substitute* "data/Makefile.in"
                ;; Use a year current at the time the release was made, to
                ;; avoid the "This year ２０２３ is not included in era.y"
                ;; error.
                (("`date '\\+%Y'`")
                 "2021"))))
          (add-after 'unpack 'do-not-override-GI_TYPELIB_PATH
            ;; Do not override the GI_TYPELIB_PATH to avoid the pygobject
            ;; error: "ValueError: Namespace Gdk not available".
            (lambda _
              (substitute* "tests/test-build.sh"
                (("GI_TYPELIB_PATH=\\$BUILDDIR/../gir" all)
                 (string-append all ":$GI_TYPELIB_PATH")))))
          (add-before 'configure 'pre-configure
            (lambda _
              ;; We need generate new _config.py with correct PKGDATADIR.
              (delete-file "setup/python3/_config.py")
              (delete-file "engine/python3/_config.py")))
          (add-before 'check 'prepare-for-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; IBus requires write access to the HOME directory.
                (setenv "HOME" "/tmp")
                ;; The single test is skipped if no actual display is found.
                (system "Xvfb :1 &")
                (setenv "DISPLAY" ":1"))))
          (add-after 'install 'wrap-programs
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each
               (lambda (prog)
                 (wrap-program (string-append #$output "/libexec/" prog)
                   `("GUIX_PYTHONPATH" ":" prefix
                     (,(getenv "GUIX_PYTHONPATH")))
                   `("GI_TYPELIB_PATH" ":" prefix
                     (,(getenv "GI_TYPELIB_PATH")
                      ,(string-append #$output "/lib/girepository-1.0")))))
               '("ibus-engine-anthy" "ibus-setup-anthy")))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           intltool
           pkg-config
           procps                       ;for ps
           python
           python-pycotap
           util-linux                   ;for getopt
           xorg-server-for-tests))
    (inputs
     (list bash-minimal
           anthy
           gtk+
           ibus
           gobject-introspection
           python-pygobject))
    (synopsis "Anthy Japanese language input method for IBus")
    (description "IBus-Anthy is an engine for the input bus \"IBus\").  It
adds the Anthy Japanese language input method to IBus.  Because most graphical
applications allow text input via IBus, installing this package will enable
Japanese language input in most graphical applications.")
    (home-page "https://github.com/fujiwarat/ibus-anthy")
    (license gpl2+)))

(define-public librime
  (package
    (name "librime")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/librime")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0p4ybmn2syhf30vwzrd6ms77xadhl2lh7d2apq2m1yzmy42mdydm"))))
    (build-system cmake-build-system)
    (inputs
     (list boost
           capnproto
           glog
           leveldb
           marisa
           opencc
           yaml-cpp))
    (native-inputs
     (list googletest pkg-config xorgproto)) ; keysym.h
    (home-page "https://rime.im/")
    (synopsis "The core library of Rime Input Method Engine")
    (description "@dfn{librime} is the core library of Rime Input Method
Engine, which is a lightweight, extensible input method engine supporting
various input schemas including glyph-based input methods, romanization-based
input methods as well as those for Chinese dialects.  It has the ability to
compose phrases and sentences intelligently and provide very accurate
traditional Chinese output.")
    (license bsd-3)))

(define-public rime-data
  (package
    (name "rime-data")
    (version "0.38.20210802")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/plum")
             (commit "0b835e347cad9c2d7038cfe82df5b5d1fe1c0327")))
       (file-name "plum-checkout")
       (sha256
        (base32 "0mja4wyazxdc6fr7pzij5ah4rzwxv4s12s64vfn5ikx1ias1f8ib"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((ice-9 match)
                  ,@%default-gnu-modules)
       #:tests? #f                  ; no tests
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             ;; Disable git operations.
             (substitute* "scripts/install-packages.sh"
               (("^\\s*fetch_or_update_package\\s$") ""))
             #t))
         ;; Copy Rime schemas into the "package/rime" directory.
         (add-after 'unpack 'copy-rime-schemas
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dest-dir "package/rime"))
               (mkdir-p dest-dir)
               (for-each
                (match-lambda
                  ((name . path)
                   (if (string-prefix? "rime-" name)
                       (let ((schema (substring name (string-length "rime-"))))
                         (symlink path (string-append dest-dir "/" schema))))))
                inputs))
             #t))
         (replace 'build
           ;; NOTE: Don't build binary Rime schema.  Binary Rime schema files
           ;; are platform dependent and contain timestamp information.
           ;; Therefore they are not reproducible.
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke "make" "all" make-flags)))
         (delete 'configure))))
    (inputs
     `(("rime-array"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-array")
                 (commit "7a7bfafae966e5f949a23a82ee8594cacf492593")))
           (file-name "rime-array-checkout")
           (sha256
            (base32
             "0kw0wyc5f77bv06fixkfvqnibmm80pyifvrhz8f1h411926ny37r"))))
       ("rime-bopomofo"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-bopomofo")
                 (commit "c7618f4f5728e1634417e9d02ea50d82b71956ab")))
           (file-name "rime-bopomofo-checkout")
           (sha256
            (base32
             "0g77nv0jrwqnbqqna0ib0kqcy6l5zl62kh49ny67d6bjwnwz9186"))))
       ("rime-cangjie"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-cangjie")
                 (commit "8dfad9e537f18821b71ba28773315d9c670ae245")))
           (file-name "rime-cangjie-checkout")
           (sha256
            (base32
             "029kw9nx6x0acg4f0m8wj1ziqffffhy9yyj51nlx17cnia0qcrby"))))
       ("rime-cantonese"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-cantonese")
                 (commit "fa7c8ad19d51143c1a470295d56feeb63e92113f")))
           (file-name "rime-cantonese-checkout")
           (sha256
            (base32
             "0vy5vv6h4r4b2msdvdrsj0zr6wmrv0fxm5zyyvxq8f1ix7ignm4c"))))
       ("rime-combo-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-combo-pinyin")
                 (commit "a84065a86b272c76215215bd6f03c506b6e7097c")))
           (file-name "rime-combo-pinyin-checkout")
           (sha256
            (base32
             "1f0b4kakw0x26gmx7xi4f94nbjlb8lvi9bks4f92jswa045vnd87"))))
       ("rime-double-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-double-pinyin")
                 (commit "69bf85d4dfe8bac139c36abbd68d530b8b6622ea")))
           (file-name "rime-double-pinyin-checkout")
           (sha256
            (base32
             "093wif5avvvw45fqbwj5wkbxrychy4pagl4mwsmbrayc8jkp69ak"))))
       ("rime-emoji"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-emoji")
                 (commit "4c8c51f4a3bc7298c99376eda9bbd86070fc4fa1")))
           (file-name "rime-emoji-checkout")
           (sha256
            (base32
             "0175jqh210fncafqckr9zzaw55qpswmqjrykwms1apmc68l43122"))))
       ("rime-essay"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-essay")
                 (commit "9db2e77305e75798baf3ec8dcf1f82785b5e1be9")))
           (file-name "rime-essay-checkout")
           (sha256
            (base32
             "03ypkkaadd5qmyg26n24a66cll90xvcimgbmiyv4d33jradiqg22"))))
       ("rime-ipa"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-ipa")
                 (commit "22b71710e029bcb412e9197192a638ab11bc2abf")))
           (file-name "rime-ipa-checkout")
           (sha256
            (base32
             "0zdk4f9qkfj3q5hmjnairj1lv6f6y27mic12k886n6sxywwbwr2k"))))
       ("rime-jyutping"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-jyutping")
                 (commit "1e24baa6998815c716c581effe8ec65ee87c4e8c")))
           (file-name "rime-jyutping-checkout")
           (sha256
            (base32
             "0s2rckpwlrm3n7w1csnqyi5p9mkpp3z87s7mrm2vc9sv06rpv7zl"))))
       ("rime-luna-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-luna-pinyin")
                 (commit "623adb022b094d540218b287c2e601509eee3347")))
           (file-name "rime-luna-pinyin-checkout")
           (sha256
            (base32
             "06pcwp09l5wkqv7792gbsl31xnlb3gr9q6bgbp94vvq6m2ycahqz"))))
       ("rime-middle-chinese"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-middle-chinese")
                 (commit "9fad7a7c0c26167d5e6e85db8df48a15c7f7d4f0")))
           (file-name "rime-middle-chinese-checkout")
           (sha256
            (base32
             "0a0bqrlzg0k692xblqnh1rh1fwwqqb205xwxlihgji85n8ibcgph"))))
       ("rime-pinyin-simp"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-pinyin-simp")
                 (commit "b0e84cda02c613ebdedc127a26131b3800f45a8e")))
           (file-name "rime-pinyin-simp-checkout")
           (sha256
            (base32
             "05v804qr3a9xvjzp9yid7231fi2l2yrl47ybbvql61z9k36ab094"))))
       ("rime-prelude"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-prelude")
                 (commit "3de303ffaa731dba07b0462ce59f4767e1219ad2")))
           (file-name "rime-prelude-checkout")
           (sha256
            (base32
             "0g7a0bla58rh1v3md59k6adk185pilb4z8i2i0pqdl4nwqp40n2p"))))
       ("rime-quick"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-quick")
                 (commit "3fe5911ba608cb2df1b6301b76ad1573bd482a76")))
           (file-name "rime-quick-checkout")
           (sha256
            (base32
             "08bh87ym5qvw55lyw20l3m7jd4c2z5rvil8h5q8790r7z6j6ijy9"))))
       ("rime-scj"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-scj")
                 (commit "cab5a0858765eff0553dd685a2d61d5536e9149c")))
           (file-name "rime-scj-checkout")
           (sha256
            (base32
             "0ard2bjp4896a8dimmcwyjwgmp9kl4rz92yc92jnd3y4rgwl6fvk"))))
       ("rime-soutzoe"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-soutzoe")
                 (commit "beeaeca72d8e17dfd1e9af58680439e9012987dc")))
           (file-name "rime-soutzoe-checkout")
           (sha256
            (base32
             "0jyqx0q9s0qxn168l5n8zav8jcl2g5ppr7pa8jm1vwrllf20slcc"))))
       ("rime-stenotype"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stenotype")
                 (commit "f3e9189d5ce33c55d3936cc58e39d0c88b3f0c88")))
           (file-name "rime-stenotype-checkout")
           (sha256
            (base32
             "0dl6px7lrh3xa87knjzwzdcwjj1k1dg4l72q7lb48an4s9f1cy5d"))))
       ("rime-stroke"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-stroke")
                 (commit "ea8576d1accd6fda339e96b415caadb56e2a07d1")))
           (file-name "rime-stroke-checkout")
           (sha256
            (base32
             "07h6nq9867hjrd2v3h1pnr940sdrw4mqrzj43siz1rzjxz3s904r"))))
       ("rime-terra-pinyin"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-terra-pinyin")
                 (commit "ce7b9249612f575d2f43d51fcacd31d1b4e0ef1b")))
           (file-name "rime-terra-pinyin-checkout")
           (sha256
            (base32
             "0vm303f4lrdmdmif5klrp6w29vn9z2vzw33cw0y83pcnz39wiads"))))
       ("rime-wubi"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wubi")
                 (commit "f1876f08f1d4a9696395be0070c0e8e4353c44cb")))
           (file-name "rime-wubi-checkout")
           (sha256
            (base32
             "1d9y9rqssacria9d0hla96czsqv2wkfm6z926m1x269ryv96zxvk"))))
       ("rime-wugniu"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/rime/rime-wugniu")
                 (commit "abd1ee98efbf170258fcf43875c21a4259e00b61")))
           (file-name "rime-wugniu-checkout")
           (sha256
            (base32
             "0qn54d3cclny106ixdw08r5n6wn52ffs1hgrma3k0j4pv0kr9nlq"))))))
    (home-page "https://rime.im/")
    (synopsis "Schema data of Rime Input Method Engine")
    (description "@dfn{rime-data} provides the schema data of Rime Input
Method Engine.")
    (license (list
              ;; rime-array
              ;; rime-combo-pinyin
              ;; rime-double-pinyin
              ;; rime-middle-chinese
              ;; rime-scj
              ;; rime-soutzoe
              ;; rime-stenotype
              ;; rime-wugniu
              gpl3

              ;; plum
              ;; rime-bopomofo
              ;; rime-cangjie
              ;; rime-emoji
              ;; rime-essay
              ;; rime-ipa
              ;; rime-jyutping
              ;; rime-luna-pinyin
              ;; rime-prelude
              ;; rime-quick
              ;; rime-stroke
              ;; rime-terra-pinyin
              ;; rime-wubi
              lgpl3

              ;; rime-pinyin-simp
              asl2.0

              ;; rime-cantonese
              cc-by4.0))))

(define-public ibus-rime
  (package
    (name "ibus-rime")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rime/ibus-rime")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vl3m6ydf7mvmalpdqqmrnnmqdi6l8yyac3bv19pp8a5q3qhkwlg"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:configure-flags
       (list (string-append "-DRIME_DATA_DIR="
                            (assoc-ref %build-inputs "rime-data")
                            "/share/rime-data"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "CMakeLists.txt"
               (("DESTINATION....RIME_DATA_DIR..")
                "DESTINATION \"${CMAKE_INSTALL_DATADIR}/rime-data\""))
             #t)))))
    (inputs
     (list gdk-pixbuf
           glib
           ibus
           libnotify
           librime
           rime-data))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://rime.im/")
    (synopsis "Rime Input Method Engine for IBus")
    (description "@dfn{ibus-rime} provides the Rime input method engine for
IBus.  Rime is a lightweight, extensible input method engine supporting
various input schemas including glyph-based input methods, romanization-based
input methods as well as those for Chinese dialects.  It has the ability to
compose phrases and sentences intelligently and provide very accurate
traditional Chinese output.")
    (license gpl3)))

(define-public libhangul
  (package
    (name "libhangul")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://kldp.net/hangul/release/"
                           "3442-libhangul-" version ".tar.gz"))
       (sha256
        (base32
         "0ni9b0v70wkm0116na7ghv03pgxsfpfszhgyj3hld3bxamfal1ar"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/libhangul/libhangul")
    (synopsis "Library to support hangul input method logic")
    (description
     "This package provides a library to support hangul input method logic,
hanja dictionary and small hangul character classification.")
    (license lgpl2.1+)))

(define-public ibus-libhangul
  (package
    (name "ibus-libhangul")
    (version "1.5.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libhangul/ibus-hangul/"
                           "releases/download/" version
                           "/ibus-hangul-" version ".tar.gz"))
       (sha256
        (base32
         "1400ba2p34vr9q285lqvjm73f6m677cgfdymmjpiwyrjgbbiqrjy"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/libexec/ibus-setup-hangul")
               `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
               `("LD_LIBRARY_PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "libhangul") "/lib")))
               `("GI_TYPELIB_PATH" ":" prefix
                 (,(getenv "GI_TYPELIB_PATH"))))
             #t)))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")))
    (inputs
     (list bash-minimal
           ibus
           glib
           python-pygobject
           gtk+
           libhangul
           python))
    (home-page "https://github.com/libhangul/ibus-hangul")
    (synopsis "Hangul engine for IBus")
    (description
     "ibus-hangul is a Korean input method engine for IBus.")
    (license gpl2+)))

(define-public ibus-table
  (package
    (name "ibus-table")
    (version "1.17.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/mike-fabian/ibus-table/releases/download/"
             version "/ibus-table-" version ".tar.gz"))
       (sha256
        (base32 "063ba4fwk04lh0naj8z9r9x15ikckp94pd3f8xn40z3lnwsjx2sj"))
       (patches (search-patches "ibus-table-paths.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-paths
                 (lambda _
                   (substitute* "engine/tabcreatedb.py"
                     (("/usr/share/ibus-table")
                      (string-append #$output "/share/ibus-table")))
                   (substitute* "engine/ibus_table_location.py"
                     (("/usr/share/ibus-table")
                      (string-append #$output "/share/ibus-table"))
                     (("/usr/libexec")
                      (string-append #$output "/libexec")))))
               (add-before 'check 'pre-check
                 (lambda _
                   (setenv "HOME" (getcwd))))))) ; tests write to $HOME
    (native-inputs (list (list glib "bin") pkg-config))
    (inputs (list glib gtk+ ibus python python-pygobject))
    (native-search-paths
     (list (search-path-specification
            (variable "IBUS_TABLE_LOCATION")
            (files '("share/ibus-table"))
            (separator #f))))
    (home-page "https://mike-fabian.github.io/ibus-table")
    (synopsis "Table based input framework for IBus")
    (description
     "@code{ibus-table} is a framework for table based input methods using
IBus.")
    (license lgpl2.1+)))

(define-public ibus-table-others
  (package
    (name "ibus-table-others")
    (version "1.3.16")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/moebiuscurve/ibus-table-others/releases/"
             "download/" version "/ibus-table-others-" version ".tar.gz"))
       (sha256
        (base32 "0vllwrjlgcvdjhs7nrg45hfvnivnfhrc05r6rhw8m0c41layl9jg"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'pre-build
                 (lambda _
                   (setenv "HOME" (getcwd))))))) ; db written in $HOME
    (native-inputs (list pkg-config python))
    (inputs (list ibus ibus-table))
    (home-page "https://github.com/moebiuscurve/ibus-table-others")
    (synopsis "Various table-based input methods for IBus")
    (description
     "@code{ibus-table-others} provides the following input methods on
IBus-Table on IBus framework:

@itemize
@item CNS11643
@item Compose
@item Emoji
@item IPA-X-SAMPA
@item LaTex
@item Mathwriter
@item Mongol bichig
@item RussianTraditional
@item Telex
@item Thai
@item Translit
@item Ua-Translit
@item Viqr
@item VNI
@item Yawerty
@end itemize")
    ;; GPL-3.0-or-later: vni, ipa-x-sampa, telex
    ;; WTFPL: mongol_bichig
    ;; LGPL-2.1-or-later: others
    (license (list lgpl2.1+ gpl3+ wtfpl2))))

(define-public ibus-speech-to-text
  (package
    (name "ibus-speech-to-text")
    (version "0.4.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/PhilippeRo/IBus-Speech-To-Text")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00p7jfv815dblg20hahch6151rdbxhkdhfj51i0yvvmg3irvf7nm"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #true
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'skip-update-desktop-database
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "meson.build"
               (("update_desktop_database: true")
                "update_desktop_database: false"))))
         (add-after 'set-paths 'add-install-to-pythonpath
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (ibus-stt-dir (string-append out "/share/ibus-stt")))
               (setenv "GUIX_PYTHONPATH"
                       (string-append ibus-stt-dir ":"
                                      (getenv "GUIX_PYTHONPATH"))))))
         (add-after 'install 'wrap-with-additional-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Make sure 'ibus-{setup,engine}-stt' find the gst-vosk plugin
             ;; and run with the correct GUIX_PYTHONPATH and GI_TYPELIB_PATH.
             (let ((out (assoc-ref outputs "out")))
               (for-each (lambda (prog)
                           (wrap-program prog
                             `("GST_PLUGIN_PATH" ":" prefix
                               (,(string-append (assoc-ref inputs "gst-vosk")
                                                "/lib/gstreamer-1.0")
                                ,(getenv "GST_PLUGIN_SYSTEM_PATH")))
                             `("GUIX_PYTHONPATH" =
                               (,(getenv "GUIX_PYTHONPATH")))
                             `("GI_TYPELIB_PATH" =
                               (,(getenv "GI_TYPELIB_PATH")))))
                         (list (string-append out "/libexec/ibus-engine-stt")
                               (string-append out "/libexec/ibus-setup-stt")))))))))
    (inputs
     (list bash-minimal
           gst-vosk
           gstreamer
           gtk
           ibus
           libadwaita
           python
           python-babel
           python-pygobject))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           (list glib "bin")
           gobject-introspection
           libxml2 pkg-config))
    (home-page "https://github.com/PhilippeRo/IBus-Speech-To-Text")
    (synopsis "Speech to text IBus engine using VOSK")
    (description "This Input Method uses VOSK for voice recognition and allows
to dictate text in several languages in any application that supports IBus.
One of the main advantages is that VOSK performs voice recognition locally
and does not rely on an online service.")
    (license gpl3+)))

(define-public ibus-theme-tools
  (package
    (name "ibus-theme-tools")
    (version "4.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/IBus-Theme-Tools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0i8vwnikwd1bfpv4xlgzc51gn6s18q58nqhvcdiyjzcmy3z344c2"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ; No tests
    (propagated-inputs
     (list python-tinycss2 python-pygobject))
    (native-inputs
     `(("gettext" ,gettext-minimal)))
    (home-page "https://github.com/openSUSE/IBus-Theme-Tools")
    (synopsis "Tool for IBus Themes")
    (description "IBus Theme Tools can extract IBus-specific settings from
GTK themes to apply both within and without GNOME Shell.")
    (license gpl3+)))
