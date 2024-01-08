;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2019, 2020 Jan Wielkiewicz <tona_kosmicznego_smiecia@interia.pl>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages jami)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages check)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vulkan)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define %jami-version "20230323.0")

(define %jami-sources
  ;; Return an origin object of the tarball release sources archive of the
  ;; Jami project.
  (origin
    (method url-fetch)
    (uri (string-append "https://dl.jami.net/release/tarballs/jami-"
                        %jami-version ".tar.gz"))
    (modules '((guix build utils)))
    (snippet
     ;; Delete multiple MiBs of bundled tarballs.  The daemon/contrib
     ;; directory contains the custom patches for pjproject and other
     ;; libraries used by Jami.
     '(delete-file-recursively "daemon/contrib/tarballs"))
    (sha256
     (base32
      "0vjsjr37cb87j9hqbmipyxn4877k1wn3l0vzca3l3ldgknglz7v2"))
    (patches (search-patches "jami-disable-integration-tests.patch"
                             "jami-libjami-headers-search.patch"))))

(define-public libjami
  (package
    (name "libjami")
    (version %jami-version)
    (source %jami-sources)
    (outputs '("out" "bin" "debug"))    ;"bin' contains jamid
    (build-system gnu-build-system)
    (arguments
     (list
      ;; The agent links the daemon binary with libguile, which enables the
      ;; execution of test plans described in Scheme.  It may be useful in
      ;; user scripts too, until more general purpose Scheme bindings are made
      ;; available (see: test/agent/README.md).
      #:configure-flags #~(list "--enable-agent" "--enable-debug")
      #:make-flags #~(list "V=1")       ;build verbosely
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory/maybe
            (lambda _
              ;; Allow building from the tarball or a git checkout.
              (false-if-exception (chdir "daemon"))))
          (add-after 'install 'delete-static-libraries
            ;; Remove 100+ MiB of static libraries.  "--disable-static" cannot
            ;; be used as the test suite requires access to private symbols
            ;; not included in the shared library.
            (lambda _
              (for-each delete-file
                        (find-files (string-append #$output "/lib")
                                    "\\.a$"))))
          (add-after 'install 'move-jamid
            ;; This nearly halves the size of the main output (from 1566.2 MiB
            ;; to 833.6 MiB), due to not depending on dbus-c++ and its large
            ;; dependencies.
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((libexec (string-append #$output:bin "/libexec"))
                    (share (string-append #$output:bin "/share")))
                (mkdir-p libexec)
                (rename-file (search-input-file outputs "libexec/jamid")
                             (string-append libexec "/jamid"))
                (mkdir-p share)
                (rename-file (search-input-directory outputs "share/dbus-1")
                             (string-append share "/dbus-1"))))))))
    (inputs
     (list alsa-lib
           asio
           dbus-c++
           eudev
           ffmpeg-jami
           guile-3.0
           jack-1
           jsoncpp
           libarchive
           libgit2
           libnatpmp
           libsecp256k1
           libupnp
           opendht
           openssl
           pjproject-jami
           pulseaudio
           speex
           speexdsp
           webrtc-audio-processing
           yaml-cpp))
    (native-inputs
     (list autoconf
           automake
           cppunit
           libtool
           perl                         ;to generate manpages with pod2man
           pkg-config
           which))
    (synopsis "Jami core library and daemon")
    (description "This package provides a library and daemon implementing the
Jami core functionality.  Jami is a secure and distributed voice, video and
chat communication platform that requires no centralized server and leaves the
power of privacy in the hands of the user.  It supports the SIP and IAX
protocols, as well as decentralized calling using P2P-DHT.  The @samp{\"bin\"}
output contains the D-Bus daemon (@command{jamid}) as well as the Jami D-Bus
service definitions.")
    (home-page "https://jami.net/")
    (license license:gpl3+)))

;;; Private package; this is used in source form: the project build system has
;;; no install target.
(define sortfilterproxymodel
  ;; Use the latest commit available from the 'qt-6' branch.
  (let ((commit "6cc21205dbf36640613f0e6e67b2b13b1855c377")
        (revision "0"))
    (package
      (name "sortfilterproxymodel")
      ;; There are no recent release tag; the module version defined in the
      ;; source is used (see:
      ;; https://github.com/oKcerG/SortFilterProxyModel/blob/
      ;; 5a930885b7ea99f7f41c25fce08bf8006ee54e3f/
      ;; qqmlsortfilterproxymodel.cpp#L574C15-L574C15).
      (version (git-version "0.2" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      ;; The upstream is
                      ;; https://github.com/oKcerG/SortFilterProxyModel, but
                      ;; it lacks Qt 6 support, so use this fork, which is the
                      ;; one used by Jami.
                      (url "https://github.com/atraczyk/SortFilterProxyModel")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n54jkimr3a818i3w7w3lnbqn47x72nnr5xi9vk0mdnbwri3viwy"))))
      (build-system qt-build-system)
      (arguments
       (list #:qtbase qtbase            ;use Qt 6
             #:tests? #f                ;no test suite
             #:configure-flags #~(list "BUILD_SFPM_PIC=ON")))
      (inputs (list qtdeclarative))
      (home-page "https://github.com/oKcerG/SortFilterProxyModel")
      (synopsis "Improved QSortFilterProxyModel implementation for QML")
      (description "SortFilterProxyModel is an implementation of
QSortFilterProxyModel conveniently exposed for QML.")
      (license license:expat))))

(define-public jami
  (package
    (name "jami")
    (version %jami-version)
    (source %jami-sources)
    (build-system qt-build-system)
    (outputs '("out" "debug"))
    (arguments
     (list
      #:qtbase qtbase
      #:configure-flags
      #~(list "-DENABLE_TESTS=ON"
              ;; Disable the webengine since it grows the closure size by
              ;; about 450 MiB and requires more resources.
              "-DWITH_WEBENGINE=OFF"
              ;; Use libwrap to link directly to libjami instead of
              ;; communicating via D-Bus to jamid, the Jami daemon.
              "-DENABLE_LIBWRAP=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'change-directory/maybe
            (lambda _
              ;; Allow building from the tarball or a git checkout.
              (false-if-exception (chdir "client-qt"))))
          (add-after 'change-directory/maybe 'fix-version-string
            (lambda _
              (substitute* "src/app/version.h"
                (("VERSION_STRING")
                 "BUILD_DATE")          ;to avoid a redefinition error
                (("// clang-format on.*" anchor)
                 (string-append "const char VERSION_STRING[] = \""
                                #$version "\";\n"
                                anchor)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "QT_QPA_PLATFORM" "offscreen")
                (setenv "QT_QUICK_BACKEND" "software")
                ;; The tests require a writable HOME.
                (setenv "HOME" "/tmp")

                (display "Running unittests...\n")
                (invoke "tests/unittests" "-mutejamid")

                ;; XXX: There are currently multiple failures with the
                ;; functional tests (see:
                ;; https://git.jami.net/savoirfairelinux/jami-client-qt/-/issues/883),
                ;; so the code below is disabled for now.
                ;;
                ;; (display "Running functional tests...\n")
                ;; ;; This is to allow building from the source tarball or
                ;; ;; directly from the git repository.
                ;; (let  ((tests-qml (if (file-exists? "../client-qt/tests")
                ;;                       "../client-qt/tests/qml"
                ;;                       "../tests/qml")))
                ;;   (invoke "tests/qml_tests" "-mutejamid"
                ;;           "-input" tests-qml))
                ))))))
    (native-inputs
     (list googletest
           pkg-config
           python
           qttools
           vulkan-headers))
    (inputs
     (list ffmpeg-jami
           glib                         ;for integration with GNOME
           libjami
           libnotify
           libxcb
           libxkbcommon
           network-manager
           qrencode
           qt5compat
           qtdeclarative
           qtmultimedia
           qtnetworkauth
           qtpositioning
           qtsvg
           vulkan-loader))
    (home-page "https://jami.net")
    (synopsis "Qt Jami client")
    (description "This package provides the Jami Qt client.  Jami is a secure
and distributed voice, video and chat communication platform that requires no
centralized server and leaves the power of privacy in the hands of the user.
It supports the SIP and IAX protocols, as well as decentralized calling using
P2P-DHT.")
    (license license:gpl3+)))

(define-public jami-docs
  ;; There aren't any tags, so use the latest commit.
  (let ((revision "1")
        (commit "ff466ebadb9b99a1672a814126793de670c3099b"))
    (package
      (name "jami-docs")
      (version (git-version "0.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.jami.net/savoirfairelinux/jami-docs")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n8a9dk8mi617rk3ycz5jrzbwv9ybfynlci5faz1klckx0aqdf6q"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'build
              (lambda _
                (invoke "make" "info" "html" "man" "LANGS="
                        "-j" (number->string
                              (parallel-job-count))))))
        #:install-plan
        ;; TODO: Install localized info manuals and HTML.
        ''(("_build/out/texinfo/jami.info" "share/info/")
           ("_build/out/html" "share/doc/jami/")
           ("_build/out/man/jami.1" "share/man/man1/"))))
      (native-inputs
       (list python
             python-myst-parser
             python-sphinx
             python-sphinx-rtd-theme
             texinfo))
      (home-page "https://git.jami.net/")
      (synopsis "Documentation for Jami")
      (description "This package contains the documentation of Jami.  Jami is
a secure and distributed voice, video and chat communication platform that
requires no centralized server and leaves the power of privacy in the hands of
the user.  It supports the SIP and IAX protocols, as well as decentralized
calling using P2P-DHT.")
      (license license:fdl1.3+))))
