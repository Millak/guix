;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Taylan Ulrich Bayirli/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2014, 2015, 2016 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016-2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2018, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2021 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Josselin Poiret <josselin.poiret@protonmail.ch>
;;; Copyright © 2022 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Jacob Hart <hartja1@yahoo.com>
;;; Copyright © 2022 Simon Streit <simon@netpanic.org>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2024 Noisytoot <ron@noisytoot.org>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
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

(define-module (gnu packages bittorrent)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE))
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public transmission
  (package
    (name "transmission")
    (version "4.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/transmission/transmission"
                                  "/releases/download/" version "/transmission-"
                                  version ".tar.xz"))
              (patches (search-patches "transmission-4.0.6-fix-build.patch"))
              (sha256
               (base32
                "0py4n33wk2srdfrcd5rbrbg5p1zq6nipghlins01d693i9nzwf1a"))))
    (build-system cmake-build-system)
    (outputs '("out"                      ; library and command-line interface
               "gui"))                    ; graphical user interface
    (arguments
      (list
        #:imported-modules `((guix build glib-or-gtk-build-system)
                             ,@%cmake-build-system-modules)
        #:modules '(((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                    (guix build cmake-build-system)
                    (guix build utils))
        #:phases
        #~(modify-phases %standard-phases
           ;; Avoid embedding kernel version for reproducible build
           (add-after 'unpack 'remove-kernel-version
             (lambda _
               (substitute* "third-party/miniupnp/miniupnpc/updateminiupnpcstrings.sh"
                 (("OS_VERSION=`uname -r`") "OS_VERSION=Guix"))))
           (replace 'check
             (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
               (if tests?
                   (invoke "ctest"
                           ;; XXX this test fails...
                           "-E" "usesBootstrapFile"
                           "-j" (if parallel-tests?
                                    (number->string (parallel-job-count))
                                    "1"))
                   (format #t "test suite not run~%"))))
           (add-after 'install 'move-gui
             (lambda* (#:key outputs #:allow-other-keys)
               (mkdir-p (string-append #$output:gui "/bin"))
               (mkdir-p (string-append #$output:gui "/share/man/man1"))
               (rename-file (string-append #$output "/bin/transmission-gtk")
                            (string-append #$output:gui "/bin/transmission-gtk"))
               (for-each
                (lambda (dir)
                  (rename-file (string-append #$output "/share/" dir)
                               (string-append #$output:gui "/share/" dir)))
                '("applications" "icons" "metainfo"))
              (rename-file
               (string-append #$output "/share/man/man1/transmission-gtk.1")
               (string-append #$output:gui "/share/man/man1/transmission-gtk.1"))))
           (add-after 'move-gui 'glib-or-gtk-wrap
             (lambda* (#:key outputs #:allow-other-keys #:rest args)
               (apply (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)
                      #:glib-or-gtk-wrap-excluded-outputs (list "out")
                      args)))
           (add-after 'glib-or-gtk-wrap 'wrap-program
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (glib-or-gtk:generate-gdk-pixbuf-loaders-cache
                (map cdr inputs) (list (assoc-ref outputs "gui")))
               (wrap-program (string-append #$output:gui "/bin/transmission-gtk")
                 ;; Wrapping GDK_PIXBUF_MODULE_FILE allows Transmission to load
                 ;; its own icons in pure environments.
                 `("GDK_PIXBUF_MODULE_FILE" =
                   (,(getenv "GDK_PIXBUF_MODULE_FILE")))))))))
    (inputs (list bash-minimal
                  curl
                  (list glib "bin")
                  gtkmm
                  libappindicator
                  libevent
                  openssl
                  python
                  zlib))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://transmissionbt.com/")
    (synopsis "BitTorrent client")
    (description
     "Transmission is a BitTorrent client that comes with graphical,
textual, and Web user interfaces.  Transmission also has a daemon for
unattended operations.  It supports local peer discovery, full encryption,
DHT, µTP, PEX and Magnet Links.")

    ;; COPYING reads:
    ;;
    ;;     Transmission can be redistributed and/or modified under the terms of
    ;; the GNU GPLv2 (http://www.gnu.org/licenses/license-list.html#GPLv2),
    ;; the GNU GPLv3 (http://www.gnu.org/licenses/license-list.html#GNUGPLv3),
    ;; or any future license endorsed by Mnemosyne LLC.
    ;;
    ;; A few files files carry an MIT/X11 license header.
    (license (list l:gpl2 l:gpl3))))

(define-public transmission-remote-gtk
  (package
    (name "transmission-remote-gtk")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/transmission-remote-gtk/"
                           "transmission-remote-gtk/releases/download/"
                           version "/transmission-remote-gtk-" version
                           ".tar.gz"))
       (sha256
        (base32 "0qz9wi70qc6vgnaymivc3xz6y86c9hglk6wjv3snnqxpxmp9saay"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnu-gettext pkg-config))
    (inputs
     (list appstream-glib curl gtk+ json-glib))
    (synopsis "Gtk frontend to the Transmission daemon")
    (description "transmission-remote-gtk is a GTK client for remote management
of the Transmission BitTorrent client, using its HTTP RPC protocol.")
    (home-page "https://github.com/transmission-remote-gtk/transmission-remote-gtk")
    (license l:gpl2+)))

(define-public libtorrent
  (package
    (name "libtorrent")
    (version "0.13.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://rtorrent.net/downloads/libtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "10z9i1rc41cmmi7nx8k7k1agsx6afv09g9cl7g9zr35fyhl5l4gd"))))
    (build-system gnu-build-system)
    (inputs (list openssl zlib))
    (native-inputs (list pkg-config cppunit))
    (synopsis "BitTorrent library of rtorrent")
    (description
     "LibTorrent is a BitTorrent library used by and developed in parallel
with the BitTorrent client rtorrent.  It is written in C++ with emphasis on
speed and efficiency.")
    (home-page "https://github.com/rakshasa/libtorrent")
    (license l:gpl2+)))

(define-public rtorrent
  (package
    (name "rtorrent")
    (version "0.9.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://rtorrent.net/downloads/rtorrent-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1bs2fnf4q7mlhkhzp3i1v052v9xn8qa7g845pk9ia8hlpw207pwy"))))
    (build-system gnu-build-system)
    (inputs (list libtorrent
                  ncurses
                  curl
                  cyrus-sasl
                  openssl
                  zlib))
    (native-inputs (list pkg-config cppunit))
    (synopsis "BitTorrent client with ncurses interface")
    (description
     "rTorrent is a BitTorrent client with an ncurses interface.  It supports
full encryption, DHT, PEX, and Magnet Links.  It can also be controlled via
XML-RPC over SCGI.")
    (home-page "https://github.com/rakshasa/rtorrent")
    (license l:gpl2+)))

(define-public tremc
  (let ((commit "d8deaa5ac25bb45a2ca3a930309d6ecc74836a54")
        (revision "1"))
  (package
    (name "tremc")
    (version (git-version "0.9.3" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tremc/tremc")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "08kpqmgisja98918f2hlmdrld5662dqlkssp0pqlki38l6fvbj7r"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no test suite
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         ;; The software is just a Python script that must be copied into place.
         (delete 'configure)
         (delete 'build))))
    (inputs
     (list python))
    (synopsis "Console client for the Transmission BitTorrent daemon")
    (description "Tremc is a console client, with a curses interface, for the
Transmission BitTorrent daemon.")
    (home-page "https://github.com/tremc/tremc")
    (license l:gpl3+))))

(define-public aria2
  (package
    (name "aria2")
    (version "1.37.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aria2/aria2/releases/"
                                  "download/release-" version
                                  "/aria2-" version ".tar.xz"))
              (sha256
               (base32
                "0sxng4pynhj2qinranpv6wyzys3d42kz1gg2nrn63sw5f2nj1930"))
              (patches (search-patches "aria2-unbundle-wslay.patch"))
              (snippet
               #~(begin (use-modules (guix build utils))
                        (delete-file-recursively "deps")
                        (delete-file "configure")))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-libaria2"
               (string-append "--with-bashcompletiondir="
                              #$output "/etc/bash_completion.d/"))
       #:phases
       #~(modify-phases %standard-phases
           (add-after 'unpack 'delete-socket-tests
             (lambda _
               (substitute* "test/LpdMessageDispatcherTest.cc"
                 (("CPPUNIT_TEST_SUITE_REGISTRATION\\(LpdMessageDispatcherTest\\);" text)
                  (string-append "// " text)))
               (substitute* "test/LpdMessageReceiverTest.cc"
                 (("CPPUNIT_TEST_SUITE_REGISTRATION\\(LpdMessageReceiverTest\\);" text)
                  (string-append "// " text))))))))
    (native-inputs
     (list autoconf ; since we adjusted configure.ac
           automake
           gettext-minimal
           libtool
           cppunit ; for the tests
           pkg-config))
    (inputs
     (list c-ares
           gnutls
           gmp
           libssh2
           libxml2
           nettle
           sqlite
           wslay
           zlib))
    (home-page "https://aria2.github.io/")
    (synopsis "Utility for parallel downloading files")
    (description
      "Aria2 is a lightweight, multi-protocol & multi-source command-line
download utility.  It supports HTTP/HTTPS, FTP, SFTP, BitTorrent and Metalink.
Aria2 can be manipulated via built-in JSON-RPC and XML-RPC interfaces.")
    (properties
     '((release-monitoring-url . "https://github.com/aria2/aria2/releases")))
    (license l:gpl2+)))

(define-public uget
  (package
    (name "uget")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/urlget/"
                           "uget%20%28stable%29/" version "/uget-"
                           version ".tar.gz"))
       (sha256
        (base32 "0dlrjhnm1pg2vwmp7nl2xv1aia5hyirb3021rl46x859k63zap24"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("CFLAGS=-fcommon")))
    (inputs
     (list curl
           gtk+
           glib
           gnutls
           gstreamer
           libgcrypt
           libnotify
           openssl))
    (native-inputs
     (list intltool pkg-config))
    (home-page "https://ugetdm.com/")
    (synopsis "Universal download manager with GTK+ interface")
    (description
     "uGet is portable download manager with GTK+ interface supporting
HTTP, HTTPS, BitTorrent and Metalink, supporting multi-connection
downloads, download scheduling, download rate limiting.")
    (license l:lgpl2.1+)))

(define-public mktorrent
  (package
    (name "mktorrent")
    (version "1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Rudde/mktorrent")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17pdc5mandl739f8q26n5is8ga56s83aqcrwhlnnplbxwx2inidr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (delete 'configure))          ; no configure script
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "NO_HASH_CHECK=1"
                          "USE_LARGE_FILES=1"
                          "USE_LONG_OPTIONS=1"
                          "USE_PTHREADS=1")
       #:tests? #f))                            ; no tests
    (home-page "https://github.com/Rudde/mktorrent")
    (synopsis "Utility to create BitTorrent metainfo files")
    (description
     "mktorrent is a simple command-line utility to create BitTorrent
@dfn{metainfo} files, often known simply as @dfn{torrents}, from both single
files and whole directories.  It can add multiple trackers and web seed URLs,
and set the @code{private} flag to disallow advertisement through the
distributed hash table (@dfn{DHT}) and Peer Exchange.  Hashing is multi-threaded
and will take advantage of multiple processor cores where possible.")
    (license (list l:public-domain      ; sha1.*, used to build without OpenSSL
                   l:gpl2+))))          ; with permission to link with OpenSSL

(define-public libtorrent-rasterbar
  (package
    (name "libtorrent-rasterbar")
    (version "2.0.11")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/arvidn/libtorrent/"
                       "releases/download/v" version "/"
                       "libtorrent-rasterbar-" version ".tar.gz"))
       (sha256
        (base32 "0v8yrxzc7piw5lrpgkb50b4p16ic1sl4pyj0rkkasaag1xc5inzh"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Dpython-bindings=ON"
              "-Dbuild_tests=ON")
      ;; Tests do not reliably work when executed in parallel.
      #:parallel-tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (let* ((disabled-tests
                      '(;; Requires a non-localhost IPv4 interface.
                        "test_upnp"))
                     (exclude-regex (string-append "^("
                                                   (string-join disabled-tests "|")
                                                   ")$"))
                     (jobs (if parallel-tests?
                               (number->string (parallel-job-count))
                               "1")))
                (when tests?
                  (invoke "ctest"
                          "-E" exclude-regex
                          "-j" jobs
                          "--output-on-failure"))))))))
    (inputs (list boost openssl))
    (native-inputs
     (list libfaketime
           python-wrapper
           pkg-config))
    (home-page "https://www.libtorrent.org/")
    (synopsis "Feature-complete BitTorrent implementation")
    (description
     "libtorrent-rasterbar is a feature-complete C++ BitTorrent implementation
focusing on efficiency and scalability.  It runs on embedded devices as well as
desktops.")
    (license l:bsd-2)))

(define-public libtorrent-rasterbar-1.2
  (package
    (inherit libtorrent-rasterbar)
    (version "1.2.20")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/arvidn/libtorrent/"
                       "releases/download/v" version "/"
                       "libtorrent-rasterbar-" version ".tar.gz"))
       (sha256
        (base32 "1z5rdynzxcm6wb7v48ssfbwjairbjacb8rjix5fn70fw4668xgyc"))))))

(define-public qbittorrent
  (package
    (name "qbittorrent")
    (version "5.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qbittorrent/qBittorrent")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0myab81g3qvldfxl1ijbc5qz9nk74xcr173ndy929l1i0r99417j"))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags #~(list "-DTESTING=ON")
           #:test-target "check"))
    (native-inputs
     (list qttools))
    (inputs
     (list boost
           libtorrent-rasterbar
           openssl
           python-wrapper
           qtsvg
           zlib))
    (home-page "https://www.qbittorrent.org/")
    (synopsis "Graphical BitTorrent client")
    (description
     "qBittorrent is a BitTorrent client programmed in C++/Qt that uses
libtorrent (sometimes called libtorrent-rasterbar) by Arvid Norberg.

It aims to be a good alternative to all other BitTorrent clients out there.
qBittorrent is fast, stable and provides unicode support as well as many
features.")
    (license l:gpl2+)))

(define-public qbittorrent-no-x
  (let ((base qbittorrent))
    (package
      (inherit base)
      (name "qbittorrent-no-x")
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:configure-flags configure-flags)
          #~(cons "-DGUI=OFF" #$configure-flags))))
      (inputs
       (modify-inputs (package-inputs base)
         (delete "qtsvg"))))))

(define-public qbittorrent-nox
  (deprecated-package "qbittorrent-nox" qbittorrent-no-x))

(define-public qbittorrent-enhanced
  (package
    (inherit qbittorrent)
    (name "qbittorrent-enhanced")
    (version "5.0.3.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/c0re100/qBittorrent-Enhanced-Edition")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04plcbmqbjjg7wkp7vww6ain3bkgaj5njf94pk7wlm2ysa6hbx3r"))))
    (home-page "https://github.com/c0re100/qBittorrent-Enhanced-Edition")
    (description
     "qBittorrent Enhanced is a bittorrent client based on qBittorrent with
the following features:

@itemize
@item Auto Ban Xunlei, QQ, Baidu, Xfplay, DLBT and Offline downloader
@item Auto Ban Unknown Peer from China Option (Default: OFF)
@item Auto Update Public Trackers List (Default: OFF)
@item Auto Ban BitTorrent Media Player Peer Option (Default: OFF)
@item Peer whitelist/blacklist
@end itemize")))

(define-public qbittorrent-enhanced-no-x
  (package
    (inherit qbittorrent-enhanced)
    (name "qbittorrent-enhanced-no-x")
    (arguments (package-arguments qbittorrent-no-x))
    (inputs (package-inputs qbittorrent-no-x))))

(define-public qbittorrent-enhanced-nox
  (deprecated-package "qbittorrent-enhanced-nox" qbittorrent-enhanced-no-x))

(define-public deluge
  (package
    (name "deluge")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.osuosl.org/pub/deluge/source/"
             (version-major+minor version) "/deluge-" version ".tar.xz"))
       (sha256
        (base32
         "1xyz8bscwqmd7d8b43svxl42w54pnisvwkkrndx46hifh0cx73bn"))))
    (build-system python-build-system)
    (inputs (list bash-minimal))
    (propagated-inputs
     (list gtk+
           libtorrent-rasterbar
           nss-certs
           python-pycairo
           python-chardet
           python-dbus
           python-mako
           python-pygobject
           python-pillow
           python-pyopenssl
           python-pyxdg
           python-rencode
           python-service-identity
           python-setproctitle
           python-six
           python-twisted
           python-zope-interface))
    (native-inputs
     (list intltool python-wheel
           (librsvg-for-system)))
    (native-search-paths
     (list $SSL_CERT_DIR
           $SSL_CERT_FILE))
    ;; TODO: Enable tests.
    ;; After "pytest-twisted" is packaged, HOME is set, and an X server is
    ;; started, some of the tests still fail.  There are likely some tests
    ;; that require a network connection.
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-deluge-console
           (lambda _
             ;; Backport patch for: https://dev.deluge-torrent.org/ticket/3582
             ;; Should be removed for release 2.1.1.
             (substitute* "deluge/ui/console/__init__.py"
               (("    return Console\\(\\).start\\(\\)")
                "    Console().start()"))))
         (add-after 'install 'wrap
           (lambda* (#:key native-inputs inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   ;; "librsvg" input is only needed at build time and it
                   ;; conflit with the "librsvg" propageted by "gtk+", so we
                   ;; make sure there is no reference to it in the wrapper.
                   (gi-typelib-path
                    (string-join (filter
                                  (lambda (x) (not (string-prefix?
                                                    (assoc-ref
                                                     (or native-inputs inputs)
                                                     "librsvg")
                                                    x)))
                                  (string-split
                                   (getenv "GI_TYPELIB_PATH")
                                   #\:))
                                 ":")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))))
                (map (lambda (name)
                       (string-append out "/bin/" name))
                     '("deluge" "deluge-gtk"))))
             #t)))))
    (home-page "https://www.deluge-torrent.org/")
    (synopsis  "Fully-featured cross-platform ​BitTorrent client")
    (description
     "Deluge contains the common features to BitTorrent clients such as
Protocol Encryption, DHT, Local Peer Discovery (LSD), Peer Exchange
(PEX), UPnP, NAT-PMP, Proxy support, Web seeds, global and per-torrent
speed limits.  Deluge heavily utilises the ​libtorrent library.  It is
designed to run as both a normal standalone desktop application and as a
​client-server.")
    (license l:gpl3+)))
