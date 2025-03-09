;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2022 Luis Felipe López Acevedo <luis.felipe.la@protonmail.com>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Luis Guilherme Coelho <lgcoelho@disroot.org>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages syndication)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages crates-crypto)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages crates-web)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (srfi srfi-1))

(define-public cawbird
  (package
    (name "cawbird")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IBBoard/cawbird")
             (commit (string-append "v"version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17575cp5qcgsqf37y3xqg3vr6l2j8bbbkmy2c1l185rxghfacida"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:configure-flags
       ;; Cawbirds's default key and secret for OAuth process with twitter.
       (list
        "-Dconsumer_key_base64=VmY5dG9yRFcyWk93MzJEZmhVdEk5Y3NMOA=="
        "-Dconsumer_secret_base64=MThCRXIxbWRESDQ2Y0podzVtVU13SGUyVGlCRXhPb3BFRHhGYlB6ZkpybG5GdXZaSjI=")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; These tests require networking.
             (substitute* "tests/meson.build"
               (("[ \t]*.*avatardownload.*$") "")
               (("[ \t]*.*filters.*$") "")
               (("[ \t]*.*friends.*$") "")
               (("[ \t]*.*inlinemediadownloader.*$") "")
               (("[ \t]*.*tweetparsing.*$") "")
               (("[ \t]*.*usercounter.*$") ""))))
         (delete 'check)
         (add-after 'install 'custom-check
           (lambda* (#:key outputs tests? #:allow-other-keys)
             (when tests?
               ;; Tests require a running X server.
               (system "Xvfb :1 +extension GLX &")
               (setenv "DISPLAY" ":1")
               ;; Tests write to $HOME.
               (setenv "HOME" (getcwd))
               ;; Tests look for gsettings-schemas installed by the package.
               (setenv "XDG_DATA_DIRS"
                       (string-append (getenv "XDG_DATA_DIRS")
                                      ":" (assoc-ref outputs "out") "/share"))
               (invoke "meson" "test"))))
         (add-after 'glib-or-gtk-wrap 'wrap-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (gst-plugin-path (getenv "GST_PLUGIN_SYSTEM_PATH"))
                    (gi-typelib-path (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append bin "cawbird")
                 `("GST_PLUGIN_SYSTEM_PATH" ":" prefix (,gst-plugin-path))
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("gtk+:bin" ,gtk+ "bin")
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)
       ("xmllint" ,libxml2)
       ("xorg-server" ,xorg-server-for-tests)))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("glib" ,glib)
       ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
       ("gspell" ,gspell)
       ("gstreamer" ,gstreamer)
       ("gst-libav" ,gst-libav)
       ("gst-plugins-bad" ,gst-plugins-bad)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gtk+" ,gtk+)
       ("json-glib" ,json-glib)
       ("liboauth" ,liboauth)
       ("libsoup" ,libsoup)
       ("rest" ,rest)
       ("sqlite" ,sqlite)
       ("x11" ,libx11)))
    (propagated-inputs
     (list dconf))
    (synopsis "Client for Twitter")
    (description "Cawbird is a Twitter client built with GTK and Vala.
It supports all features except non-mention notifications, polls, threads and
cards.")
    (home-page "https://ibboard.co.uk/cawbird/")
    (license license:gpl3+)))

(define-public giara
  (package
    (name "giara")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/giara")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2lr7s2sqzvphl84zcf68l6lzhp5faycz75yp36ak18aw9b8g0m"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-icon-cache
           (lambda _
             (substitute* "meson_post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'unpack 'skip-validate-metainfo-file-test
           (lambda _
             (substitute* "data/meson.build"
               (("if ascli_exe\\.found\\(\\)")
                "if false"))))
         (add-after 'glib-or-gtk-wrap 'wrap-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/"))
                    (lib (string-append out "/lib/python"
                                        ,(version-major+minor
                                          (package-version python))
                                        "/site-packages")))
               (wrap-program (string-append bin "giara")
                 `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH") ,lib))
                 `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))))))))
    (native-inputs
     (list gettext-minimal
           (list glib "bin")
           gobject-introspection
           (list gtk "bin")
           pkg-config))
    (inputs
     (list bash-minimal
           blueprint-compiler-0.4
           glib
           gtk
           gtksourceview
           libadwaita
           python
           python-beautifulsoup4
           python-dateutil
           python-mistune
           python-pillow
           python-praw
           python-pygobject
           python-requests))
    (propagated-inputs
     (list dconf))
    (synopsis "Client for Reddit")
    (description "Giara is a reddit app, built with Python, GTK4 and libadwaita.")
    (home-page "https://giara.gabmus.org/")
    (license license:gpl3+)))

(define-public newsboat
  (package
    (name "newsboat")
    (version "2.38")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://newsboat.org/releases/" version
                           "/newsboat-" version ".tar.xz"))
       (sha256
        (base32 "11fv2klyc16sfma0zy8phmp4x61w0hswxfwdds10gwa8i7qgdznn"))))
    (build-system cargo-build-system)
    (native-inputs
     (append
       (list gettext-minimal
             openssl
             pkg-config)
       ;; For building documentation.
       (if (supported-package? ruby-asciidoctor)
           (list ruby-asciidoctor)
           '())))
    (inputs
     (list curl
           json-c
           libxml2
           ncurses
           stfl
           sqlite))
    (arguments
     (list
       #:modules '((guix build cargo-build-system)
                   (guix build utils)
                   ((guix build gnu-build-system) #:prefix gnu:))
       #:install-source? #f
       #:cargo-inputs
       (list rust-backtrace-0.3
             rust-bitflags-2
             rust-chrono-0.4
             rust-curl-sys-0.4
             rust-cxx-1
             rust-cxx-build-1
             rust-fastrand-2
             rust-gettext-rs-0.7
             rust-httpmock-0.7
             rust-lexopt-0.3
             rust-libc-0.2
             rust-md5-0.7
             rust-natord-1
             rust-nom-7
             rust-percent-encoding-2
             rust-url-2
             rust-unicode-width-0.1
             rust-unicode-segmentation-1
             rust-xdg-2)
       #:cargo-development-inputs
       (list rust-tempfile-3
             rust-proptest-1
             rust-section-testing-0.0.5)
       #:phases
       #~(modify-phases %standard-phases
           #$@(if (not (this-package-native-input "asciidoctor"))
                  #~((add-after 'unpack 'dont-use-asciidoctor
                       (lambda _
                         (substitute* "config.sh"
                           ((".*asciidoctor.*") ""))
                         (substitute* "Makefile"
                           (("^doc:.*") "doc:\n")
                           (("install-podboat install-docs") "install-podboat")))))
                   #~())
           (add-after 'unpack 'pre-build
             (lambda* (#:key inputs #:allow-other-keys)
               (setenv "CXX" #$(cxx-for-target))
               (setenv "CXX_FOR_BUILD" (which "g++"))
               (substitute* "config.sh"
                 (("if curl-config")
                  (string-append
                    "if " (search-input-file inputs "/bin/curl-config"))))))
           (add-after 'configure 'dont-vendor-self
             (lambda* (#:key vendor-dir #:allow-other-keys)
               ;; Don't keep the whole tarball in the vendor directory
               (delete-file-recursively
                 (string-append vendor-dir "/" #$name "-" #$version ".tar.xz"))))
           (add-after 'unpack 'patch-source
             (lambda* (#:key outputs #:allow-other-keys)
               (substitute* "Makefile"
                 (("Cargo.lock") "")
                 ;; Replace the prefix in the Makefile.
                 (("/usr/local") (assoc-ref outputs "out")))))
           (replace 'build
             (assoc-ref gnu:%standard-phases 'build))
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys #:rest args)
               (when tests?
                 ((assoc-ref gnu:%standard-phases 'check)
                  #:test-target "test"))))
           (replace 'install
             (assoc-ref gnu:%standard-phases 'install)))))
    (native-search-paths
     ;; Newsboat respects CURL_CA_BUNDLE.
     (list (search-path-specification
            (variable "CURL_CA_BUNDLE")
            (file-type 'regular)
            (separator #f)                        ;single entry
            (files '("etc/ssl/certs/ca-certificates.crt")))))
    (home-page "https://newsboat.org/")
    (synopsis "Text-mode RSS and Atom feed reader with podcast support")
    (description "Newsboat is a feed reader for @dfn{RSS} and @dfn{Atom}, XML
formats widely used to transmit, publish, and syndicate news or blog articles.
It's designed for use on text terminals, and to have a coherent and easy-to-use
interface that might look familiar to @command{mutt} or @command{slrn} users.

Newsboat supports OPML import/exports, HTML rendering, podcasts (with
@command{podboat}), off-line reading, searching and storing articles to your
file system, and many more features.")
    (properties '((release-monitoring-url . "https://newsboat.org/news.atom")))
    (license (list license:gpl2+        ; filter/*
                   license:expat))))    ; everything else

(define-public newsraft
  (package
    (name "newsraft")
    (version "0.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/newsraft/newsraft")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10i5khna9wpaisarmzym9dvfaq91mnf1wvwsymnzl052d4n106l9"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output)
              "CFLAGS=-DCURL_WRITEFUNC_ERROR=0xFFFFFFFF")
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)))) ; no configure
    (native-inputs
     (list pkg-config scdoc))
    (inputs
     (list curl expat gumbo-parser ncurses sqlite yajl))
    (home-page "https://codeberg.org/grisha/newsraft")
    (synopsis "Feed reader for terminal")
    (description
     "Newsraft is a feed reader with ncurses user interface.  It is greatly
inspired by Newsboat and tries to be its lightweight counterpart.

Features:
@itemize
@item parallel downloads
@item section-based feeds grouping
@item opening links in any program
@item viewing news from all feeds with explore mode
@item automatic updates for feeds and sections
@item per-feed settings and key bindings
@item assigning multiple actions to key bindings
@item processing feeds from command output
@item text searching by news titles and content
@item interactive news content viewing
@end itemize")
    (license license:isc)))

(define-public liferea
  (package
    (name "liferea")
    (version "1.13.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lwindolf/liferea/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g9463bvswsm899j6dfhslcg6np70m5wq143mjicr24zy8d17bm7"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     `(#:configure-flags
       (list
         "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'prepare-build-environment
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Workaround for https://github.com/lwindolf/liferea/issues/767.
             (setenv "WEBKIT_DISABLE_COMPOSITING_MODE" "1")))
         (add-after 'install 'wrap-gi-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/liferea")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                 `("GUIX_PYTHONPATH" ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           intltool
           libtool
           pkg-config
           which))
    (inputs
     (list bash-minimal
           glib
           glib-networking
           gnome-keyring
           gsettings-desktop-schemas
           gstreamer
           json-glib
           libnotify
           libpeas
           libsecret
           libsoup-minimal-2
           libxml2
           libxslt
           pango
           python
           python-pycairo
           python-pygobject
           sqlite
           webkitgtk-with-libsoup2))
    (home-page "https://lzone.de/liferea/")
    (synopsis "News reader for GTK/GNOME")
    (description "Liferea is a desktop feed reader/news aggregator that
brings together all of the content from your favorite subscriptions into
a simple interface that makes it easy to organize and browse feeds.")
    (license license:gpl2+)))

(define-public rtv
  (package
    (name "rtv")
    (version "1.27.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "rtv" version))
        (sha256
         (base32 "0hvw426y09l3yzwv2zkb9hifpfbg9wd1gg0y3z3pxcli6n3ii2wl"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-environment-variables
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "HOME" (getcwd))
             (setenv "TERM" "linux")
             (setenv "TERMINFO"
                     (search-input-directory inputs "share/terminfo"))))
         ;; Loading this as a library requires a controlling terminal, etc.
         (delete 'sanity-check))
       #:tests? #f)) ; tests fail: _curses.error: nocbreak() returned ERR
    (propagated-inputs
     (list python-beautifulsoup4 python-decorator python-kitchen
           python-requests python-six))
    (native-inputs
     (list ncurses
           python-coveralls
           python-coverage
           python-mock
           python-pylint
           python-pytest
           python-vcrpy))
    (home-page "https://github.com/michael-lazar/rtv")
    (synopsis "Terminal viewer for Reddit (Reddit Terminal Viewer)")
    (description
     "RTV provides a text-based interface to view and interact with Reddit.")
    (license (list license:expat
                   license:gpl3+)))) ; rtv/packages/praw

(define-public tuir
  (package
    (name "tuir")
    (version "1.29.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "tuir" version))
        (sha256
         (base32
          "06xb030ibphbrz4nsxm8mh3g60ld8xfp6kc3j6vi1k4ls5s4h79i"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags
           ;; Some tests require "/etc/ssl/certs/ca-certificates.crt" which is not
           ;; available during the tests; also likely those tests require networking.
           #~(list "-k" (string-append "not test_content"
                                       " and not test_inbox"
                                       " and not test_mime_parsers"
                                       " and not test_oauth"
                                       " and not test_page"
                                       " and not test_submission"
                                       " and not test_subreddit"
                                       " and not test_subscription"))
           #:phases
           #~(modify-phases %standard-phases
               (delete 'sanity-check))))  ; Reads environment variables.
    (inputs
     (list python-beautifulsoup4 python-decorator python-kitchen
           python-requests python-six))
    (native-inputs
     (list python-coverage
           python-coveralls
           python-mock
           python-pylint
           python-pytest
           python-setuptools
           python-vcrpy
           python-wheel))
    (home-page "https://gitlab.com/ajak/tuir")
    (synopsis "Terminal viewer for Reddit (Terminal UI for Reddit)")
    (description
     "Tuir provides a simple terminal viewer for Reddit (Terminal UI for Reddit).")
    (license (list license:expat
                   license:gpl3+))))    ; tuir/packages/praw

(define-public morss
  (package
    (name "morss")
    (version "20221213.2216")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "morss" version))
              (sha256
               (base32
                "1mvxxhzmraxjnw0vz60zkl4d8xp7lw0fs0z537zfhmj1lj9ap4cp"))))
    (build-system python-build-system)
    (arguments
     ;; Tests are not available in the PyPI release and the Git release
     ;; is lagging behind.  Additionally, tests use the network.
     (list #:tests? #f
           ;; Sanity check fails to find the module 'bs4', but it's available
           ;; in the python-beautifulsoup4 dependency.
           #:phases #~(modify-phases %standard-phases
                        (delete 'sanity-check))))
    (propagated-inputs (list python-beautifulsoup4 python-chardet
                             python-dateutil python-lxml))
    (home-page "https://morss.it/")
    (synopsis "Get full-text RSS feeds")
    (description "Morss' goal is to get full-text RSS feeds out of striped
RSS feeds, commonly available on the internet.  It also makes it possible
to create RSS feeds for websites that don't provide any.")
    (license license:agpl3+)))

(define-public syndication-domination
  (let ((revision "1")
        (commit "75920321062d682437f3fb0319dad227d8b18f6c"))
    (package
      (name "syndication-domination")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.com/gabmus/syndication-domination")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1fl362920n6nz4x9wihyzbr82d9cy60sknhmajj62whd5gs49sbw"))))
      (build-system meson-build-system)
      (inputs (list fmt tidy-html pybind11 python pugixml))
      (native-inputs (list cmake pkg-config)) ; need cmake to find pybind11
      (home-page "https://gitlab.com/gabmus/syndication-domination")
      (synopsis "RSS/Atom feed parser")
      (description "This package provides an experimental RSS/Atom feed
parser.  It is \"not fit for use at this point\", but gfeeds uses it anyway.")
      (license license:agpl3))))

(define-public gfeeds
  (package
    (name "gfeeds")
    (version "2.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/gfeeds")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p2hyjif9yhpc6r3ig7fdxpb2q8s9g42mz38svsc38gq7hb13b2w"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-mpv-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "gfeeds/confManager.py"
                (("mpv") (search-input-file inputs "/bin/mpv")))))
          (add-after 'unpack 'skip-icon-cache
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false"))))
          (add-after 'install 'wrap-gfeeds
            (lambda* (#:key outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "/bin/gfeeds")
                `("PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH")))
                `("XDG_DATA_DIRS" ":" prefix (,(getenv "XDG_DATA_DIRS")))))))))
    (native-inputs
     (list `(,glib "bin")
           blueprint-compiler-0.4
           gobject-introspection
           gettext-minimal
           pkg-config))
    (inputs
     (list bash-minimal
           glib
           gsettings-desktop-schemas
           gtk
           hicolor-icon-theme
           libadwaita
           mpv
           python
           python-beautifulsoup4
           python-dateutil
           python-feedparser
           python-html5lib
           python-humanize
           python-listparser
           python-lxml
           python-magic
           python-pillow
           python-pygments
           python-pygobject
           python-pytz
           python-readability-lxml
           python-requests
           syndication-domination
           webkitgtk))
    (home-page "https://gfeeds.gabmus.org/")
    (synopsis "Easy-to-use GTK+ RSS/Atom feed reader")
    (description "Feeds is an RSS/Atom feed reader made with GTK+
and it has an easy-to-use graphical user interface.")
    (license license:gpl3+)))
