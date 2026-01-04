;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2020, 2022-2023, 2026 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018, 2019, 2020 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2025 Patrick Norton <patrick.147.norton@gmail.com>
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
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
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
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages selinux)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls))

(define-public nextcloud-client
  (package
    (name "nextcloud-client")
    (version "3.17.2")
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
        (base32 "0y03yldgyazqds691dzgaginnpys6alnlc9j4aimmi3zcnk93hk3"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        '(begin
           ;; QProgressIndicator is not available in Guix.
           ;; FIXME: Fix building with the system kirigami and qtsolutions.
           (let* ((keep '("QProgressIndicator"
                          "kirigami"
                          "qtlockedfile"
                          "qtsingleapplication")))
             (with-directory-excursion "src/3rdparty"
               (for-each delete-file-recursively
                         (lset-difference string=?
                                          (scandir ".")
                                          (cons* "." ".." keep)))))
           (with-directory-excursion "src/gui"
             (substitute* "CMakeLists.txt"
               ;; Remove references of deleted 3rdparties.
               (("[ \t]*\\.\\./3rdparty/kmessagewidget/?.*\\.(cpp|h)")
                "")
               (("\\$\\{CMAKE_SOURCE_DIR\\}/src/3rdparty/kmessagewidget")
                ;; For this, we rely on build inputs, so let's just replace
                ;; them by an autoconf-style variable.
                "@kwidgetsaddons@")
               ;; Expand libraries, that used to be statically linked, but
               ;; no longer are post-vendoring.
               (("KF6::Archive")
                (string-append "KF6::Archive "
                               "KF6WidgetsAddons"))))
           #t))))
    (build-system qt-build-system)
    (arguments
     (list #:qtbase qtbase
           #:configure-flags #~(list "-DUNIT_TESTING=ON" "-DBUILD_UPDATER=OFF")
           #:imported-modules
           `((guix build glib-or-gtk-build-system)
             ,@%qt-build-system-modules)
           #:modules
           '(((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
             (guix build qt-build-system)
             (guix build utils))
           ;; 72% tests passed, 17 tests failed out of 61 due to SEGFAULT.
           #:test-exclude
           (string-append "("
                          (string-join '("OwnSqlTest"
                                         "SyncJournalDBTest"
                                         "SyncFileItemTest"
                                         "ConcatUrlTest"
                                         "CookiesTest"
                                         "XmlParseTest"
                                         "ChecksumValidatorTest"
                                         "ClientSideEncryptionTest"
                                         "ExcludedFilesTest"
                                         "UtilityTest"
                                         "CapabilitiesTest"
                                         "ThemeTest"
                                         "IconUtilsTest"
                                         "FileSystemTest"
                                         "LongPathTest"
                                         "AccountTest"
                                         "FolderTest")
                                       "|")
                          ")")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-cmake
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/gui/CMakeLists.txt"
                     (("@kwidgetsaddons@")
                      (search-input-directory inputs
                                              "/include/KF6/KWidgetsAddons/")))
                   (with-directory-excursion "shell_integration"
                     ;; Patch install directory for dbus service files.
                     (substitute* "libcloudproviders/CMakeLists.txt"
                       (("pkg_get_variable\\(_install_dir dbus-1 .*\\)")
                        (string-append "set(_install_dir \""
                                       "${CMAKE_INSTALL_PREFIX}"
                                       "/share/dbus-1/services\")")))
                     (substitute* "dolphin/CMakeLists.txt"
                       ;; Make sure, that Qt modules are installed under
                       ;; $prefix.
                       (("ON CACHE") "OFF CACHE")))))
               (add-after 'unpack 'unpack-3rd_party-sources
                 (lambda* (#:key inputs #:allow-other-keys)
                   (copy-recursively (assoc-ref inputs "libcrashreporter-qt")
                                     "src/3rdparty/libcrashreporter-qt")))
               ;; Tests write to $HOME.
               (add-before 'check 'set-home-directory
                 (lambda _
                   (setenv "HOME" (getcwd))))
               (add-after 'install 'glib-or-gtk-compile-schemas
                 (assoc-ref glib-or-gtk:%standard-phases
                            'glib-or-gtk-compile-schemas))
               (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list cmocka
           desktop-file-utils
           graphviz
           doxygen
           extra-cmake-modules
           `(,glib "bin")
           (origin
             (method git-fetch)
             (uri
              (git-reference
                (url "https://github.com/dschmidt/libcrashreporter-qt")
                (commit "96da2900d590218b745ea79cd7aa794856e1d7ba")))
             (file-name "libcrashreporter-qt")
             (sha256
              (base32 "0r14abym96fk9zwalyvyzl5igwsrpgmk2hsfxshzh2av4riq0lmk")))
           (librsvg-for-system)
           perl
           pkg-config
           python-wrapper
           python-sphinx
           qttools
           ruby))
    (inputs
     (list appstream
           dbus
           glib
           karchive
           kconfig
           kcoreaddons
           kguiaddons
           kio
           kjs
           kwidgetsaddons
           libcloudproviders
           libp11
           libzip
           openssl
           qt5compat
           qtdeclarative
           qtkeychain-qt6
           qtsvg
           qtwayland
           qtwebchannel
           qtwebengine
           qtwebsockets
           sqlite
           xdg-utils
           zlib))
    (synopsis "Desktop sync client for Nextcloud")
    (description "Nextcloud-Desktop is a tool to synchronize files from
Nextcloud Server with your computer.")
    (home-page "https://nextcloud.com")
    (license (list license:expat     ; QProgressIndicator
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

(define-public onedrive
  (package
    (name "onedrive")
    (version "2.5.9")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/abraunegg/onedrive")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "05ygicsfw7g5jmlymiwpv097223qglx5324ycy47yc1j3qlzpfjn"))))
    (build-system gnu-build-system)
    (arguments
     (list
       #:configure-flags
       #~(list "--enable-completions"
               "--enable-notifications"
               (string-append "--with-bash-completion-dir="
                              #$output "/share/bash-completion/completions")
               (string-append "--with-zsh-completion-dir="
                              #$output "/share/zsh/site-functions")
               (string-append "--with-fish-completion-dir="
                              #$output "/share/fish/vendor_completions.d"))
       #:make-flags
       #~(list (string-append "CC=" #$(cc-for-target)))
       #:phases
       #~(modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./onedrive" "--version")))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bash-minimal
           curl
           dbus
           ldc
           libnotify
           sqlite))
    (home-page "https://abraunegg.github.io")
    (synopsis "Client for OneDrive")
    (description "OneDrive Client which supports OneDrive Personal, OneDrive for
Business, OneDrive for Office365 and SharePoint and fully supports Azure
National Cloud Deployments.  It supports one-way and two-way sync capabilities
and securely connects to Microsoft OneDrive services.")
    (properties '((lint-hidden-cpe-vendors . ("microsoft"))))
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

(define-public croc
  (package
    (name "croc")
    (version "10.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/schollz/croc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jf8rfviy2vy9656ghss00fpvsx0iy079x906png39a52ryqjdzb"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/schollz/croc"
      #:test-flags
      #~(list "-skip" "TestPublicIP|TestLocalIP|TestTCP")))
    (native-inputs
     (list go-github-com-cespare-xxhash-v2
           go-github-com-chzyer-readline
           go-github-com-denisbrodbeck-machineid
           go-github-com-kalafut-imohash
           go-github-com-magisterquis-connectproxy
           go-github-com-minio-highwayhash
           go-github-com-sabhiram-go-gitignore
           go-github-com-schollz-cli-v2
           go-github-com-schollz-logger
           go-github-com-schollz-pake-v3
           go-github-com-schollz-peerdiscovery
           go-github-com-schollz-progressbar-v3
           go-github-com-skip2-go-qrcode
           go-github-com-stretchr-testify
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-time))
    (home-page "https://github.com/schollz/croc")
    (synopsis "Send things from one computer to another easily and securely")
    (description
     "This package provides @code{croc} - a tool that allows any two
computers to simply and securely transfer files and folders.

Feature:
@itemize
@item allows any two computers to transfer data (using a relay)
@item provides end-to-end encryption (using PAKE)
@item enables easy cross-platform transfers (Windows, Linux, Mac)
@item allows multiple file transfers
@item allows resuming transfers that are interrupted
@item no need for local server or port-forwarding
@item IPv6-first with IPv4 fallback
@item can use a proxy, like Tor
@end itemize")
    (license license:expat)))

(define-public rclone
  (package
    (name "rclone")
    (version "1.71.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/rclone/rclone")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kplhspd2jjb4hfm928ybxl8l8blgnh3zkhgfcp3ynm10n7qhsky"))
       (snippet
        #~(begin
            ;; XXX: This test fails to compile: `undefined: testscript.Main'
            (delete-file "fs/logger/logger_test.go")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/rclone/rclone"
      #:embed-files
      #~(list "Linux.gitignore"
              "Windows.gitignore"
              "macOS.gitignore"
              "common.gitignore"
              "children"
              "nodes"
              "text")
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Requires docker-compose
                       (list "TestIntegration"
                             "TestDockerPluginMountTCP"
                             "TestDockerPluginMountUnix"
                             ;; Requires install perms
                             "TestInstallOnLinux"
                             ;; Requires external storage cache
                             "TestInternalObjNotFound"
                             "TestInternalCachedWrittenContentMatches"
                             "TestInternalCachedUpdatedContentMatches"
                             "TestInternalDoubleWrittenContentMatches"
                             "TestInternalWrappedFsChangeNotSeen"
                             "TestInternalNotifyCreatesEmptyParts"
                             "TestInternalChangeSeenAfterDirCacheFlush"
                             "TestInternalCacheWrites"
                             "TestInternalMaxChunkSizeRespected"
                             "TestInternalBug2117"
                             "TestInternalUploadTempDirCreated"
                             "TestInternalMoveWithNotify"
                             "TestSftp"
                             "TestRc"
                             "TestCache*"
                             "TestListPlugins"
                             ;; Requires network
                             "TestZenodoRemote"
                             "TestMetadata"
                             "TestFTP"
                             ;; Requires executable to be visible
                             "TestEndToEnd"
                             "TestEndToEndMigration"
                             "TestEndToEndRepoLayoutCompat"
                             ;; Requires mount perms
                             "TestMount"
                             ;; Requires network access
                             "TestGetVersion"
                             "TestMetadataMapper"
                             "TestStatsGroupOperations"
                             "TestSorterExt"
                             ;; Bad interface conversion
                             "TestRcDu"
                             ;; Requires write access
                             "TestAddPlugin"
                             "TestRemovePlugin"
                             "TestKvConcurrency"
                             "TestKvExit"
                             "TestFileSetModTime"
                             "TestFileRename"
                             "TestItem*"
                             "TestRWFileHandle*"
                             "TestRWFileModTimeWithOpenWriters"
                             "TestRWCacheRename"
                             "TestFunctional"
                             "invalid_UTF-8")
                       "|"))))
    (native-inputs
     (list go-bazil-org-fuse
           go-github-com-a8m-tree
           go-github-com-aalpar-deheap
           go-github-com-abbot-go-http-auth
           go-github-com-anacrolix-dms
           go-github-com-atotto-clipboard
           go-github-com-aws-aws-sdk-go-v2
           go-github-com-aws-aws-sdk-go-v2-config
           go-github-com-aws-aws-sdk-go-v2-feature-s3-manager
           go-github-com-aws-aws-sdk-go-v2-service-s3
           go-github-com-aws-smithy-go
           go-github-com-azure-azure-sdk-for-go-sdk-azcore
           go-github-com-azure-azure-sdk-for-go-sdk-azidentity
           go-github-com-azure-azure-sdk-for-go-sdk-storage-azblob
           go-github-com-azure-azure-sdk-for-go-sdk-storage-azfile
           go-github-com-azure-go-ntlmssp
           go-github-com-buengese-sgzip
           go-github-com-cloudinary-cloudinary-go-v2
           go-github-com-cloudsoda-go-smb2
           go-github-com-colinmarc-hdfs-v2
           go-github-com-coreos-go-semver
           go-github-com-coreos-go-systemd-v22
           go-github-com-dop251-scsu
           go-github-com-dropbox-dropbox-sdk-go-unofficial-v6
           go-github-com-files-com-files-sdk-go-v3
           go-github-com-gabriel-vasile-mimetype
           go-github-com-gdamore-tcell-v2
           go-github-com-go-chi-chi-v5
           go-github-com-go-chi-chi-v5
           go-github-com-go-git-go-billy-v5
           go-github-com-golang-jwt-jwt-v4
           go-github-com-google-uuid
           go-github-com-googleapis-enterprise-certificate-proxy
           go-github-com-hanwen-go-fuse-v2
           go-github-com-henrybear327-go-proton-api
           go-github-com-henrybear327-proton-api-bridge
           go-github-com-ibm-go-sdk-core-v5
           go-github-com-jcmturner-gokrb5-v8
           go-github-com-jlaffaye-ftp
           go-github-com-jzelinskie-whirlpool
           go-github-com-koofr-go-httpclient
           go-github-com-koofr-go-koofrclient
           go-github-com-lanrat-extsort
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-runewidth
           go-github-com-max-sum-base32768
           go-github-com-mitchellh-go-homedir
           go-github-com-moby-sys-mountinfo
           go-github-com-ncw-swift-v2
           go-github-com-oracle-oci-go-sdk-v65
           go-github-com-patrickmn-go-cache
           go-github-com-peterh-liner
           go-github-com-pkg-sftp
           go-github-com-pkg-xattr
           go-github-com-prometheus-client-golang
           go-github-com-protonmail-go-crypto
           go-github-com-putdotio-go-putio-for-rclone
           go-github-com-rclone-gofakes3
           go-github-com-rfjakob-eme
           go-github-com-rivo-uniseg
           go-github-com-shirou-gopsutil-v4
           go-github-com-skratchdot-open-golang
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-github-com-t3rm1n4l-go-mega
           go-github-com-unknwon-goconfig
           go-github-com-willscott-go-nfs
           go-github-com-xanzy-ssh-agent
           go-github-com-youmark-pkcs8
           go-github-com-yunify-qingstor-sdk-go-v3
           go-github-com-zeebo-blake3
           go-github-com-zeebo-xxh3
           go-go-etcd-io-bbolt
           go-goftp-io-server-v2
           go-golang-org-x-crypto
           go-golang-org-x-mobile
           go-golang-org-x-net
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-text
           go-golang-org-x-time
           go-google-golang-org-api
           go-gopkg-in-natefinch-lumberjack-v2
           go-gopkg-in-validator-v2
           go-storj-io-uplink))
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
