;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2020, 2022-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018, 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
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

(define-module (gnu packages sync)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages dlang)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public nextcloud-client
  (package
    (name "nextcloud-client")
    (version "3.8.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/nextcloud/desktop")
         (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "0gmj217jmmx13wwb096prwzn3njv616njk1id97g6lrbn969fcnn"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        '(begin
           ;; Not available in Guix.
           (let* ((keep '("QProgressIndicator" "qtokenizer" "kirigami")))
             (with-directory-excursion "src/3rdparty"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep)))))
           (with-directory-excursion "src/gui"
             (substitute* "CMakeLists.txt"
               ;; Remove references of deleted 3rdparties.
               (("[ \t]*\\.\\./3rdparty/qtlockedfile/?.*\\.(cpp|h)")
                "")
               (("[ \t]*\\.\\./3rdparty/qtsingleapplication/?.*\\.(cpp|h)")
                "")
               (("[ \t]*\\.\\./3rdparty/kmessagewidget/?.*\\.(cpp|h)")
                "")
               (("[ \t]*list\\(APPEND 3rdparty_SRC \\.\\./3rdparty/?.*\\)")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/qtlockedfile")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/qtsingleapplication")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/kmessagewidget")
                ;; For this, we rely on build inputs, so let's just replace
                ;; them by an autoconf-style variable.
                "@kwidgetsaddons@")
               ;; Expand libraries, that used to be statically linked, but
               ;; no longer are post-vendoring.
               (("KF5::Archive")
                (string-append "KF5::Archive "
                               "QtSolutions_LockedFile "
                               "QtSolutions_SingleApplication "
                               "KF5WidgetsAddons")))
             ;; Fix compatibility with QtSingleApplication from QtSolutions.
             (substitute* '("application.h" "application.cpp")
               (("SharedTools::QtSingleApplication")
                "QtSingleApplication")
               (("slotParseMessage\\(const QString &(msg)?.*\\)")
                "slotParseMessage(const QString &msg)")))
           #t))))
    (build-system qt-build-system)
    (arguments
     `(#:configure-flags
       (list
        "-DUNIT_TESTING=ON" "-DBUILD_UPDATER=OFF")
       #:imported-modules
       ((guix build glib-or-gtk-build-system)
        ,@%qt-build-system-modules)
       #:modules
       (((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
        (guix build qt-build-system)
        (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-cmake
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Patch install directory for dbus service files.
             (substitute* "shell_integration/libcloudproviders/CMakeLists.txt"
               (("pkg_get_variable\\(_install_dir dbus-1 .*\\)")
                (string-append "set(_install_dir \"${CMAKE_INSTALL_PREFIX}"
                               "/share/dbus-1/services\")")))
             (substitute* "shell_integration/dolphin/CMakeLists.txt"
               ;; Make sure, that Qt modules are installed under $prefix.
               (("ON CACHE") "OFF CACHE"))
             (substitute* "src/gui/CMakeLists.txt"
               (("@kwidgetsaddons@")
                (search-input-directory inputs
                                        "/include/KF5/KWidgetsAddons/")))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (setenv "QT_QPA_PLATFORM" "offscreen")
               (invoke "ctest" "-E" "SyncXAttrTest"))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests write to $HOME.
             (setenv "HOME" (getcwd))
             #t))
         (add-after 'install 'glib-or-gtk-compile-schemas
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
         (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("dot" ,graphviz)
       ("doxygen" ,doxygen)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("glib:bin" ,glib "bin")
       ("librsvg" ,(librsvg-for-system))
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("python" ,python-wrapper)
       ("qttools-5" ,qttools-5)
       ("ruby" ,ruby)))
    (inputs
     (list appstream
           dbus
           desktop-file-utils
           glib
           karchive-5
           kconfig-5
           kcoreaddons-5
           kio-5
           kjs
           kwidgetsaddons-5
           libcloudproviders
           libzip
           openssl
           qtbase-5
           qtdeclarative-5
           qtgraphicaleffects
           qtkeychain
           qtquickcontrols2-5
           qtsolutions
           qtsvg-5
           qtwebchannel-5
           qtwebsockets-5
           sqlite
           xdg-utils
           zlib))
    (propagated-inputs
     (list qtwebengine-5))
    (synopsis "Desktop sync client for Nextcloud")
    (description "Nextcloud-Desktop is a tool to synchronize files from
Nextcloud Server with your computer.")
    (home-page "https://nextcloud.com")
    (license (list license:expat     ; QProgressIndicator
                   license:lgpl2.1+  ; qtokenizer
                   license:gpl2+))))

(define-public megacmd
  (package
    (name "megacmd")
    (version "1.5.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/meganz/MEGAcmd")
              (commit (string-append version "_Linux"))
              (recursive? #t)))
        (sha256
         (base32
          "12v46jyxdgp2qqdpmd084d60hd5srjbgwpk082b3rp5dl7yg1rd8"))
        (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    ;; XXX: Disabling tests because they depend on libgtest.la from googletest,
    ;; which is not installed for unclear reasons.
    (arguments
     (list #:tests? #f
           #:configure-flags #~'("--with-pcre")))
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list c-ares
           crypto++
           curl
           freeimage
           googletest
           libuv
           openssl
           pcre
           readline
           libsodium
           sqlite
           zlib))
    (home-page "https://mega.nz/cmd")
    (synopsis
     "Command Line Interactive and Scriptable Application to access mega.nz")
    (description "MEGAcmd provides non UI access to MEGA services.  It intends
to offer all the functionality of a MEGA account via commands.  It features
synchronization, backup of local folders into a MEGA account and a
webdav/streaming server.

See also: megatools, a third-party alternative more commonly packaged in other
distributions.")
    (license (list license:bsd-2 license:gpl3+))))

(define-public megatools
  (package
    (name "megatools")
    (version "1.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://megatools.megous.com/builds/megatools-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1nwbalc54iz6616liyxfalf5yafwx0iv6cmqgvg4kz9khqscmhcd"))))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-completions
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "contrib/bash-completion/megatools"
                           (string-append (assoc-ref outputs "out")
                                          "/etc/bash_completion.d"))
             #t)))))
    (native-inputs
     (list pkg-config
           ;; For documentation
           asciidoc))
    (inputs
     (list curl glib openssl))
    (home-page "https://megatools.megous.com/")
    (synopsis "Command line client application for mega.nz")
    (description "Megatools is a collection of programs for accessing the mega.nz service
from the command line.

Megatools allow you to copy individual files as well as entire directory trees to and from
the cloud.  You can also perform streaming downloads for example to preview videos and
audio files, without needing to download the entire file first.

Megatools are robust and optimized for fast operation - as fast as Mega servers allow.
Memory requirements and CPU utilization are kept at minimum.

See also: megacmd, the official tool set by MEGA.")
    (license license:gpl2)))

(define-public owncloud-client
  (package
    (name "owncloud-client")
    (version "2.9.0.5150")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.owncloud.com/desktop/ownCloud/stable/"
                           version "/source/ownCloud-" version ".tar.xz"))
       (sha256
        (base32 "0nf68x840p30yng4fh1nlyiqg40z0rkcv0lskpz8dd4pj1iw5jjs"))
       (patches (search-patches "owncloud-disable-updatecheck.patch"))))
    ;; TODO: unbundle qprogessindicator, qlockedfile, qtokenizer and
    ;; qtsingleapplication which have not yet been packaged, but all are
    ;; explicitly used from the 3rdparty folder during build.
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-failing-tests
           ;; "Could not create autostart folder"
           (lambda _
             (substitute* "test/CMakeLists.txt"
                          (("owncloud_add_test\\(Utility\\)" test)
                           (string-append "#" test)))
             #t))
         (add-after 'unpack 'dont-embed-store-path
           (lambda _
             (substitute* "src/common/utility_unix.cpp"
               (("QCoreApplication::applicationFilePath\\()") "\"owncloud\""))
             #t))
         (delete 'patch-dot-desktop-files))
       #:configure-flags `("-DUNIT_TESTING=ON"
                           ;; build without qtwebkit, which causes the
                           ;; package to FTBFS while looking for QWebView.
                           "-DNO_SHIBBOLETH=1"
                           ;; Fix sync-exclude.list problem, see
                           ;; <https://github.com/owncloud/client/issues/8373>
                           ;; <https://issues.guix.gnu.org/47672>
                           ,(string-append "-DSYSCONF_INSTALL_DIR="
                                           (assoc-ref %outputs "out")
                                           "/etc"))))
    (native-inputs
     `(("cmocka" ,cmocka)
       ("extra-cmake-modules" ,extra-cmake-modules)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("qtlinguist" ,qttools-5)))
    (inputs
     (list qtbase-5 qtkeychain sqlite zlib))
    (home-page "https://owncloud.org")
    (synopsis "Folder synchronization with an ownCloud server")
    (description "The ownCloudSync system lets you always have your latest
files wherever you are.  Just specify one or more folders on the local machine
to and a server to synchronize to.  You can configure more computers to
synchronize to the same server and any change to the files on one computer will
silently and reliably flow across to every other.")
    (license license:gpl2+)))

(define-public onedrive
  (package
    (name "onedrive")
    ;; Move ahead of 2.5.3 for OpenSSL version check error and other fixes
    (version "2.5.3-1.71a71da")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/abraunegg/onedrive")
               (commit "71a71da1e0c981969900fa690c93905e0cc4b9b5")))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1rr3aw363gln4i8j3b43mih1acqj5way7ybs0mndxkhsayw67vb0"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-completions"
               "--enable-notifications"
               (string-append "--with-zsh-completion-dir="
                              #$output "/share/zsh/site-functions")
               (string-append "--with-fish-completion-dir="
                              #$output "/share/fish/vendor_completions.d"))
       #:make-flags
       #~(list (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'configure 'adjust-makefile
           (lambda _
             (substitute* "Makefile"
               (("-O ") "-O2 "))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./onedrive" "--version")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bash-minimal
           curl
           ldc
           libnotify
           sqlite))
    (home-page "https://abraunegg.github.io")
    (synopsis "Client for OneDrive")
    (description "OneDrive Client which supports OneDrive Personal, OneDrive for
Business, OneDrive for Office365 and SharePoint and fully supports Azure
National Cloud Deployments.  It supports one-way and two-way sync capabilities
and securely connects to Microsoft OneDrive services.")
    (license license:gpl3)))

(define-public lsyncd
  (package
    (name "lsyncd")
    (version "2.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/axkibe/lsyncd")
             (commit (string-append "release-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q2ixp52r96ckghgmxdbms6xrq8dbziimp8gmgzqfq4lk1v1w80y"))))
    (build-system cmake-build-system)
    (arguments
     `(;; The "tests" target is broken and assumes that tests are run in the
       ;; root directory.
       #:tests? #f
       #:test-target "tests"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'search-$PATH-for-binaries
           ;; lsyncd requires and hard-codes absolute file names to binaries.
           ;; Make it fall back to searching $PATH for relative file names.
           (lambda _
             (substitute* "lsyncd.c"
               (("execv\\(") "execvp("))
             (substitute* (list "lsyncd.lua"
                                "default-direct.lua"
                                "default-rsync.lua"
                                "default-rsyncssh.lua")
               (("(|/usr)/bin/") ""))
             #t))
         (replace 'install
           ;; No install target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (man (string-append out "/share/man/man1")))
               (install-file "lsyncd" bin)
               (install-file "../source/doc/manpage/lsyncd.1" man)
               #t))))))
    (native-inputs
     (list lua-5.2))
    (home-page "https://github.com/axkibe/lsyncd")
    (synopsis "Synchronize local directories with remote targets")
    (description "Lsyncd watches a local directory trees event monitor
interface (inotify or fsevents).  It aggregates and combines events for a few
seconds and then spawns one (or more) process(es) to synchronize the changes.
By default this is rsync, which must be installed on all source and target
machines.  Lsyncd is thus a light-weight live mirror solution that is
comparatively easy to install not requiring new file systems or block devices
and does not hamper local file system performance.")
    (license license:gpl2+)))

(define-public usync
  (let ((revision "1")
        (commit "09a8059a1adc22666d3ecf7872e22e6846c3ac9e"))
    (package
      (name "usync")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ebzzry/usync")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16i1q8f0jmfd43rb8d70l2b383vr5ib4kh7iq3yd345q7xjz9c2j"))))
      (build-system copy-build-system)
      (inputs
       (list scsh))
      (propagated-inputs
       (list rsync unison))
      (arguments
       `(#:install-plan '(("usync" "bin/usync"))
         #:phases (modify-phases %standard-phases
                    (add-before 'install 'patch-usync-shebang
                      (lambda _
                        (substitute* "usync"
                          (("/usr/bin/env scsh")
                           (which "scsh"))))))))
      (home-page "https://github.com/ebzzry/usync")
      (synopsis "Command line site-to-site synchronization tool")
      (description
       "@command{usync} is a simple site-to-site synchronization program
written in @command{scsh}.  It makes use of @command{unison} and
@command{rsync} for bi- and uni-directional synchronizations.")
      (license license:expat))))

(define-public casync
  (let ((commit "99559cd1d8cea69b30022261b5ed0b8021415654")
        (revision "0"))
    (package
      (name "casync")
      (version (git-version "2" revision commit))
      (home-page "https://github.com/systemd/casync/")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "139g82rkwv1kzss6crfmw3p01xnyjzz66b1ckprpbfncxb24047w"))
                (file-name (string-append name "-" version "-checkout"))))
      (build-system meson-build-system)
      (arguments
       `(#:configure-flags
         (let ((out (assoc-ref %outputs "out")))
           (list (string-append "-Dudevrulesdir="
                                out "/lib/udev/rules.d")))))
      (native-inputs
       (list pkg-config python python-sphinx rsync))                  ;for tests
      (inputs
       (list xz ;for liblzma
             `(,zstd "lib")
             curl
             acl
             libselinux
             eudev
             fuse-2
             openssl
             zlib))
      (synopsis "File synchronization and backup system")
      (description
       "casync is a @dfn{content-addressable data synchronizer} that can be used
as the basis of a backup system.  It is:

@itemize
@item A combination of the rsync algorithm and content-addressable storage;
@item An efficient way to store and retrieve multiple related versions of
large file systems or directory trees;
@item An efficient way to deliver and update OS, VM, IoT and container images
over the Internet in an HTTP and CDN friendly way;
@item An efficient backup system.
@end itemize\n")
      (license license:lgpl2.1+))))

(define-public rclone
  (package
    (name "rclone")
    (version "1.52.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/rclone/rclone/releases/download/"
                           "v" version "/rclone-v" version ".tar.gz"))
       (sha256
        (base32 "1pdhsxzc5ch2brfylghc602h9ba3x5dshxm3vcaldrgfac0rx0zl"))))
    ;; FIXME: Rclone bundles some libraries Guix already provides.  Need to
    ;; un-bundle them.
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rclone/rclone"
      #:install-source? #f
      #:test-subdirs #~(list ".")))
    (synopsis "@code{rsync} for cloud storage")
    (description "@code{Rclone} is a command line program to sync files and
directories to and from different cloud storage providers.

Features include:
@itemize
@item MD5/SHA1 hashes checked at all times for file integrity
@item Timestamps preserved on files
@item Partial syncs supported on a whole file basis
@item Copy mode to just copy new/changed files
@item Sync (one way) mode to make a directory identical
@item Check mode to check for file hash equality
@item Can sync to and from network, e.g., two different cloud accounts
@item Optional encryption (Crypt)
@item Optional cache (Cache)
@item Optional FUSE mount (rclone mount)
@end itemize")
    (home-page "https://rclone.org/")
    (license license:expat)))
