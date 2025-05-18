;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Saku Laesvuori <saku@laesvuori.fi>
;;; Copyright © 2023 Lu Hui <luhux76@gmail.com>
;;; Copyright © 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages telegram)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages animation)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages fcitx)
  #:use-module (gnu packages fcitx5)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages telephony)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define %telegram-version "5.12.3")

(define libyuv-for-telegram-desktop
  (let ((commit "04821d1e7d60845525e8db55c7bcd41ef5be9406")
        (revision "2440"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://chromium.googlesource.com/libyuv/libyuv")
            (commit commit)))
      (file-name (git-file-name
                  "libyuv-for-telegram-desktop"
                  (git-version "0" revision commit)))
      (sha256
       (base32
        "1fsvc0f8mckrdzys8lnlnbw6676mjamm6p3ghr2h9liqfa83s6wg")))))

(define cmake-helpers-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/cmake_helpers.git")
          (commit "90e6d73100a9fd2dc4c30a270c3bbc1d35924f32")))
    (file-name
     (git-file-name "cmake-helpers-for-telegram-desktop" %telegram-version))
    (patches
     ;; https://github.com/desktop-app/cmake_helpers/pull/305
     (search-patches "telegram-desktop-unbundle-cppgir.patch"))
    (sha256
     (base32
      "0mpz0adsyzsr5crxcjfr96x133yl4j55nm5f3gv5w1q1g1vk283r"))))

(define codegen-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/codegen.git")
          (commit "4155b9ae2d4c5a37b9738afa8ef9fa20d8fdcb44")))
    (file-name
     (git-file-name "codegen-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1h45rsi4nrkr3j312ji8qlkbzsb948nszmnylwimh5v65n90p21a"))))

(define lib-base-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_base.git")
          (commit "b28088164b7a46c70ae2cfd9daf865f6425610b2")))
    (file-name
     (git-file-name "lib-base-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1dwqdnasn3igr7i14hkx1glxj0gn6rd852bj0w3k1ai9j295wnfz"))))

(define lib-crl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_crl.git")
          (commit "c1d6b0273653095b10b4d0f4f7c30b614b690fd5")))
    (file-name
     (git-file-name "lib-crl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1sxn3qccsfbx1289z0fdrb4cggs16a8r75ic6wi81c6lnkrdi3wl"))))

(define lib-lottie-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_lottie.git")
          (commit "3eb4a97f1dd038bc4b6bd2884262242382a37e79")))
    (file-name
     (git-file-name "lib-lottie-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "04sgbspp0wngpr5w2wjfl1hwk1kiy8kwk2sz841f1yj231s7v6xw"))))

(define lib-qr-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_qr.git")
          (commit "6fdf60461444ba150e13ac36009c0ffce72c4c83")))
    (file-name
     (git-file-name "lib-qr-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1i5n3qvjkf4nv8k75cc0a71xyvpklc4nzg4k2zsfr2pxk0cy7hkw"))))

(define lib-rpl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_rpl.git")
          (commit "9a3ce435f4054e6cbd45e1c6e3e27cfff515c829")))
    (file-name
     (git-file-name "lib-rpl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "02g84i1d1hb5kqnhfr90fnw8nq1khqky95x52v2kx8zz05i1r8vs"))))

(define lib-spellcheck-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_spellcheck.git")
          (commit "8809cc72d07087ec61a1e8569de4da95aac45474")))
    (file-name
     (git-file-name "lib-spellcheck-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0zxk7vxr29f8scdi2ymvvz4zh9zkln8r57y1n65x0vfi8vdihn1a"))))

(define lib-storage-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_storage.git")
          (commit "ccdc72548a5065b5991b4e06e610d76bc4f6023e")))
    (file-name
     (git-file-name "lib-storage-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0b11ix95dzpkz335q0a6b5yg8qhj33s4fgj9ppl37pszcqq1j3wi"))))

(define lib-tl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_tl.git")
          (commit "237cbeb9d1c637759f89a508c1d854caf16e1984")))
    (file-name
     (git-file-name "lib-tl-for-telegram-desktop" %telegram-version))
    (patches
     (search-patches "lib-tl-for-telegram-memcpy.patch"))
    (sha256
     (base32
      "1ji3gypy4yf9knsgylnyz5gc2kii7lls5ymj1rkf0daixdz931cm"))))

(define lib-ui-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_ui.git")
          (commit "ba969667301ae4d8da2c2f6c4528bea63443f607")))
    (file-name
     (git-file-name "lib-ui-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "17h6awwna9qn98a0zk85xhh8ibgh3g7665khpgd752pya4jg27jw"))))

(define lib-webrtc-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webrtc.git")
          (commit "169ba6b1d5e58e9d1cfa7b7d5c85c119e6c6e2db")))
    (file-name
     (git-file-name "lib-webrtc-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0xh24qdy82j9mricja4ahzrsw9bgiklqy2mc0r891cblmmm2d90j"))))

(define lib-webview-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webview.git")
          (commit "f546969919a5946d49a504f8159041fa5b55c3df")))
    (file-name
     (git-file-name "lib-webview-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "178mf6lvgj4y5lscb68pc0yn3jcn66g04zszj74hpya18zjbmavw"))))

(define tgcalls-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TelegramMessenger/tgcalls.git")
          (commit "9bf4065ea00cbed5e63cec348457ed13143459d0")))
    (file-name
     (git-file-name "tgcalls-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1p563a11w8jrid96xf03dg6j39ciz28n5f4r6g28lxhiphbqzfym"))))

(define-public webrtc-for-telegram-desktop
  (let ((commit "8c233a4c12d6ec1f2aa87991564ac28cc996c57a")
        (revision "489"))
    (hidden-package
     (package
       (name "webrtc-for-telegram-desktop")
       (version
        (git-version "0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/tg_owt.git")
            (commit commit)))
          (file-name
           (git-file-name name version))
          (sha256
           (base32 "0d1nglf8irxgavw0p1d23wffmzyzbwapnli45ssgmrn1czzri4gw"))
          (modules '((guix build utils)
                     (ice-9 ftw)
                     (srfi srfi-1)))
          (snippet
           #~(begin
               (let ((keep
                      '("rnnoise"
                        ;; Not available in Guix.
                        "pffft")))
                 (with-directory-excursion "src/third_party"
                   (for-each delete-file-recursively
                             (lset-difference string=?
                                              (scandir ".")
                                              (cons* "." ".." keep)))))
               ;; Unbundle abseil-cpp, crc32c and openh264.
               (substitute* "CMakeLists.txt"
                 (("\\include\\(cmake\\/libopenh264\\.cmake\\)")"")
                 (("\\include\\(cmake\\/libabsl\\.cmake\\)")"")
                 (("\\include\\(cmake\\/libcrc32c\\.cmake\\)")""))))))
       (build-system cmake-build-system)
       (arguments
        (list
         #:tests? #f                    ; No target
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'unpack-additional-sources
               (lambda _
                 (let* ((third-party (string-append (getcwd) "/src/third_party"))
                        (libyuv-to (string-append third-party "/libyuv")))
                   (copy-recursively #$libyuv-for-telegram-desktop
                                     libyuv-to)))))))
       (native-inputs (list pkg-config python-wrapper yasm))
       (inputs
        (list abseil-cpp-cxxstd17
              crc32c
              ffmpeg
              glib
              glibmm
              libdrm
              libglvnd
              libjpeg-turbo
              libsrtp
              libvpx
              libxcomposite
              libxdamage
              libxext
              libxfixes
              libxrandr
              libxrender
              libxtst
              mesa
              openh264
              openssl
              opus
              pipewire
              protobuf))
       (synopsis "WebRTC support for Telegram Desktop")
       (description "WebRTC-for-Telegram-Desktop is a custom WebRTC fork by
Telegram project, for its use in telegram desktop client.")
       (home-page "https://github.com/desktop-app/tg_owt")
       (license
        (list
         ;; Abseil-CPP
         license:asl2.0
         ;; LibYuv
         (license:non-copyleft "file:///src/third_party/libyuv/LICENSE")
         ;; PFFFT
         (license:non-copyleft "file:///src/third_party/pffft/LICENSE")
         ;; RnNoise
         license:gpl3
         ;; LibSRTP, Crc32c and Others
         license:bsd-3))))))

(define-public rlottie-for-telegram-desktop
  (let ((commit "8c69fc20cf2e150db304311f1233a4b55a8892d7")
        (revision "678"))
    (hidden-package
     (package
       (name "rlottie-for-telegram-desktop")
       (version (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/desktop-app/rlottie.git")
                (commit commit)))
          (file-name
           (git-file-name name version))
          (sha256
           (base32 "14gwg3sn6xdx9ymnx5r0vfm4pk8dwk92s10a1wdvfbjpyrxll64i"))
          (modules '((guix build utils)))
          (snippet
           #~(begin
               (substitute* "meson.build"
                 (("werror=true") "werror=false"))))))
       (build-system meson-build-system)
       (arguments
        (list #:configure-flags #~(list
                                   "-Dlog=true"
                                   "-Dtest=true"
                                   "-Dcpp_std=gnu++17")))
       (native-inputs
        (list googletest
              pkg-config))
       (synopsis "Rlottie for Telegram desktop")
       (home-page "https://github.com/desktop-app/rlottie")
       (description
        "This package is an alternative fork of
https://github.com/Samsung/rlottie with changes adopted for Telegram desktop
and not propagated to upstream.")
       ;; All Licenses are listed in README and provided in licenses
       ;; directory.
       (license (list license:bsd-3
                      license:expat
                      license:freetype
                      license:lgpl2.1+))))))

(define cld3-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/google/cld3.git")
         (commit "b48dc46512566f5a2d41118c8c1116c4f96dc661")))
   (file-name
    (git-file-name "cld3-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0ayrrhfdwrf4260h9fsirkhhfrcvc3qqnh6h9wj3ixij2lq0wwqb"))))

(define libprisma-for-telegram-desktop
  (origin
   (method git-fetch)
   (uri (git-reference
         (url "https://github.com/desktop-app/libprisma")
         (commit "23b0d70f9709da9b38561d5706891a134d18df76")))
   (file-name
    (git-file-name "libprisma-for-telegram-desktop" %telegram-version))
   (sha256
    (base32
     "0fg4x4ikj7f3706bmfvkwq4smxc98qr3cgpm25w48n4ys6wfgadg"))))

(define-public telegram-desktop
  (package
    (name "telegram-desktop")
    (version %telegram-version)
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/telegramdesktop/tdesktop.git")
         (commit
          (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "16dfk36xfsizrxmxcid9kwj2dvxfp42382hqcan9rsrgjlqm6ymy"))
       (patches
        (search-patches
         ;; https://github.com/telegramdesktop/tdesktop/pull/24126
         "telegram-desktop-allow-disable-libtgvoip.patch"
         ;; Make it compatible with GCC 11.
         "telegram-desktop-qguiapp.patch"
         "telegram-desktop-hashmap-incomplete-value.patch"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-1)))
       (snippet
        #~(begin
            (let ((keep
                   '(;; Not available in Guix.
                     "tgcalls" "cld3")))
              (with-directory-excursion "Telegram/ThirdParty"
                (for-each delete-file-recursively
                          (lset-difference string=?
                                           (scandir ".")
                                           (cons* "." ".." keep)))))))))
    (build-system qt-build-system)
    (arguments
     (list #:tests? #f                      ; No target
           #:imported-modules
           `(,@%qt-build-system-modules
             (guix build glib-or-gtk-build-system))
           #:modules
           '((guix build qt-build-system)
             ((guix build glib-or-gtk-build-system)
              #:prefix glib-or-gtk:)
             (guix build utils)
             (ice-9 match))
           #:configure-flags
           #~(list
              ;; Do not generate the debug symbols to reduce link time memory
              ;; requirements from 25 GiB to 1.3 GiB.  This also nearly halves
              ;; the build time.
              "-DCMAKE_BUILD_TYPE=Release"
              ;; Client applications must provide their own API-ID and API-HASH,
              ;; see also <https://core.telegram.org/api/obtaining_api_id>.
              ;; Here, we snarf the keys from the official Snaps, which are
              ;; also stored in <#$source/snap/snapcraft.yaml>.
              "-DTDESKTOP_API_ID=611335"
              "-DTDESKTOP_API_HASH=d524b414d21f4d37f08684c1df41ac9c"
              "-DTDESKTOP_DISABLE_LEGACY_TGVOIP=ON"
              "-DDESKTOP_APP_DISABLE_CRASH_REPORTS=ON"
              "-DDESKTOP_APP_DISABLE_AUTOUPDATE=ON"
              "-DDESKTOP_APP_USE_PACKAGED_RLOTTIE=ON"
              ;; Enabling jemalloc causes SIGSEGV.  This probably happened
              ;; after upgrading to glibc 2.39.
              "-DDESKTOP_APP_DISABLE_JEMALLOC=ON")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-additional-sources
                 (lambda _
                   (for-each make-file-writable (find-files "."))
                   (for-each
                    (match-lambda
                      ((dst src)
                       (copy-recursively src dst)
                       (for-each make-file-writable (find-files dst))))
                    '(("cmake" #$cmake-helpers-for-telegram-desktop)
                      ("Telegram/codegen" #$codegen-for-telegram-desktop)
                      ("Telegram/lib_base" #$lib-base-for-telegram-desktop)
                      ("Telegram/lib_crl" #$lib-crl-for-telegram-desktop)
                      ("Telegram/lib_lottie" #$lib-lottie-for-telegram-desktop)
                      ("Telegram/lib_qr" #$lib-qr-for-telegram-desktop)
                      ("Telegram/lib_rpl" #$lib-rpl-for-telegram-desktop)
                      ("Telegram/lib_spellcheck" #$lib-spellcheck-for-telegram-desktop)
                      ("Telegram/lib_storage" #$lib-storage-for-telegram-desktop)
                      ("Telegram/lib_tl" #$lib-tl-for-telegram-desktop)
                      ("Telegram/lib_ui" #$lib-ui-for-telegram-desktop)
                      ("Telegram/lib_webrtc" #$lib-webrtc-for-telegram-desktop)
                      ("Telegram/lib_webview" #$lib-webview-for-telegram-desktop)
                      ("Telegram/ThirdParty/cld3" #$cld3-for-telegram-desktop)
                      ("Telegram/ThirdParty/libprisma" #$libprisma-for-telegram-desktop)
                      ("Telegram/ThirdParty/tgcalls" #$tgcalls-for-telegram-desktop)))))
               (add-after 'unpack-additional-sources 'patch-gir-ignore-paths
                 (lambda _
                   (substitute* "cmake/external/glib/generate_cppgir.cmake"
                     (("\\$\\{cmake_helpers_loc\\}/external/glib/cppgir/data")
                      (string-append #$(this-package-input "cppgir") "/share/cppgir")))))
               (add-after 'unpack-additional-sources 'use-system-xdg-desktop-portal
                 (lambda _
                   (substitute* (list "Telegram/CMakeLists.txt"
                                      "Telegram/lib_base/CMakeLists.txt")
                     (("\\$\\{third_party_loc\\}/xdg-desktop-portal/data")
                      (string-append #$(this-package-native-input "xdg-desktop-portal")
                                     "/share/dbus-1/interfaces")))))
               ;; Remove a problematic 'constexpr' keyword, otherwise
               ;; compilation fails with GCC 11.
               (add-after 'use-system-xdg-desktop-portal 'patch-libwebview
                 (lambda _
                   (substitute* "Telegram/lib_webview/webview/webview_interface.h"
                     (("constexpr ") ""))))
               (add-after 'install 'glib-or-gtk-compile-schemas
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
               (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list cpp-ada-url-parser
           `(,glib "bin")
           gobject-introspection
           `(,gtk+ "bin")
           pkg-config
           python-wrapper
           xdg-desktop-portal))
    (inputs
     (list abseil-cpp-cxxstd17
           alsa-lib
           boost
           c++-gsl
           cppgir-for-telegram-desktop
           crc32c
           expected-lite
           fcitx-qt5
           fcitx5-qt
           ffmpeg
           glib
           glibmm-2.76
           gtk+
           hime
           hunspell
           kcoreaddons-5
           kimageformats-5
           libdispatch
           libexpected
           libjpeg-turbo
           libvpx
           libxcb
           lz4
           minizip
           nimf
           openal
           openssl
           opus
           plasma-wayland-protocols
           pulseaudio
           protobuf
           qrcodegen-cpp
           qtbase-5
           qtdeclarative-5
           qtimageformats-5
           qtsvg-5
           qtwayland-5
           range-v3
           rlottie-for-telegram-desktop
           rnnoise
           wayland
           wayland-protocols
           webkitgtk-for-gtk3
           webrtc-for-telegram-desktop
           xcb-util-keysyms
           xxhash
           zlib))
    (synopsis "Telegram Desktop")
    (description "Telegram desktop is the official desktop version of the
Telegram instant messenger.")
    (home-page "https://desktop.telegram.org/")
    (license
     (list
      ;; ThirdParty
      license:lgpl3
      ;; Others
      license:gpl3+))))

(define-public tl-parser
  (let ((commit "1933e76f8f4fb74311be723b432e4c56e3a5ec06")
        (revision "21"))
    (package
      (name "tl-parser")
      (version
       (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tl-parser.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "13cwi247kajzpkbl86hnwmn1sn2h6rqndz6khajbqj0mlw9mv4hq"))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%cmake-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build cmake-build-system)
          (guix build utils))
         #:phases
         (modify-phases %standard-phases
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("." "bin"
                         #:include ("tl-parser"))
                        ("../source" "include/tl-parser"
                         #:include-regexp ("\\.h$")))
                      args))))))
      (synopsis "Parse tl scheme to tlo")
      (description "TL-Parser is a tl scheme to tlo file parser.  It was
formerly a part of telegram-cli, but now being maintained separately.")
      (home-page "https://github.com/vysheng/tl-parser")
      (license license:gpl2+))))

(define-public tgl
  (let ((commit "ffb04caca71de0cddf28cd33a4575922900a59ed")
        (revision "181"))
    (package
      (name "tgl")
      (version
       (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tgl.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "0cf5s7ygslb5klg1qv9qdc3hivhspmvh3zkacyyhd2yyikb5p0f9"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                    ; No target
         #:imported-modules
         `((guix build copy-build-system)
           ,@%default-gnu-imported-modules)
         #:modules
         '(((guix build copy-build-system)
            #:prefix copy:)
           (guix build gnu-build-system)
           (guix build utils))
         #:configure-flags
         '(list
           ;; Use gcrypt instead of openssl.
           "--disable-openssl"
           ;; Enable extended queries system.
           "--enable-extf"
           ;; Include libevent-based net and timers.
           "--enable-libevent")
         #:phases
         '(modify-phases %standard-phases
            (add-after 'unpack 'trigger-bootstrap
              (lambda _
                (delete-file "configure")))
            (add-after 'trigger-bootstrap 'patch-tl-parser
              (lambda _
                (delete-file "Makefile.tl-parser")
                (substitute* "Makefile.in"
                  (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                   "")
                  (("\\$\\{EXE\\}/tl-parser")
                   "tl-parser"))))
            (add-after 'unpack 'remove-Werror
              (lambda _
                (substitute* "Makefile.in"
                  (("-Werror") ""))))
            (replace 'install
              (lambda args
                (apply (assoc-ref copy:%standard-phases 'install)
                       #:install-plan
                       '(("bin" "bin")
                         ("." "include/tgl"
                          #:include-regexp ("\\.h$"))
                         ("libs" "lib/tgl"))
                       args))))))
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (inputs
       (list libevent libgcrypt tl-parser zlib))
      (synopsis "Telegram Library")
      (description "TGL is the telegram library for telegram-cli.")
      (home-page "https://github.com/vysheng/tgl")
      (license license:lgpl2.1+))))

(define-public telegram-cli
  (let ((commit "6547c0b21b977b327b3c5e8142963f4bc246187a")
        (revision "324"))
    (package
      (name "telegram-cli")
      (version
       (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/vysheng/tg.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "0c1w7jgska71jjbvg1y09v52549pwa4zkdjly18yxywn7gayd2p6"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                     ; No target
        #:imported-modules
        `((guix build copy-build-system)
          ,@%default-gnu-imported-modules)
        #:modules
        '(((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
        #:configure-flags
        '(list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl")
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'remove-Werror
             (lambda _
               (substitute* "Makefile.in"
                 (("-Werror") "-fcommon"))))
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")))
           (add-after 'trigger-bootstrap 'patch-tgl-and-tlparser
             (lambda* (#:key inputs #:allow-other-keys)
               (for-each delete-file
                         (list
                          "Makefile.tgl"
                          "Makefile.tl-parser"))
               (substitute* "Makefile.in"
                 (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                  "")
                 (("include \\$\\{srcdir\\}/Makefile\\.tgl")
                  "")
                 (("-I\\$\\{srcdir\\}/tgl")
                  (string-append "-I" (assoc-ref inputs "tgl")
                                 "/include/tgl"))
                 (("AUTO=auto")
                  (string-append "AUTO=" (assoc-ref inputs "tgl")
                                 "/include/tgl/auto"))
                 (("LIB=libs")
                  (string-append "LIB=" (assoc-ref inputs "tgl")
                                 "/lib/tgl")))))
           (replace 'install
             (lambda args
               (apply (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      '(("bin" "bin")
                        ("." "etc/telegram-cli"
                         #:include-regexp ("\\.pub$")
                         #:exclude ("tg-server.pub")))
                      args))))))
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (inputs
       (list jansson
             libconfig
             libevent
             libgcrypt
             lua
             openssl
             perl
             python
             readline
             tgl
             tl-parser
             zlib))
      (synopsis "Telegram Messenger CLI")
      (description "TG is the command-line interface for Telegram Messenger.")
      (home-page "https://github.com/vysheng/tg")
      (license license:gpl2+))))

(define-public tgcli
  (package
    (name "tgcli")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/erayerdin/tgcli")
         (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "082zim7rh4r8qyscqimjh2sz7998vv9j1i2y2wwz2rgrlhkhly5r"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Test requirements referes to specific versions of packages,
         ;; which are too old. So we patch them to refer to any later versions.
         (add-after 'unpack 'patch-test-requirements
           (lambda _
             (substitute* "dev.requirements.txt"
               (("==") ">="))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "pytest" "tests")))))))
    (native-inputs
     `(("coveralls" ,python-coveralls)
       ("pytest" ,python-pytest)
       ("pytest-click" ,python-pytest-click)
       ("pytest-cov" ,python-pytest-cov)
       ("mkdocs" ,python-mkdocs)
       ("mkdocs-material" ,python-mkdocs-material)
       ("requests-mock" ,python-requests-mock)))
    (propagated-inputs
     `(("click" ,python-click)
       ("colorful" ,python-colorful)
       ("requests" ,python-requests)
       ("yaspin" ,python-yaspin)))
    (home-page "https://tgcli.readthedocs.io")
    (synopsis "Telegram Terminal Application")
    (description "TgCli is a telegram client to automate repetitive tasks.")
    (license license:asl2.0)))

(define-public tgs2png
  (let ((commit "25c15b7c2ca3b1a580a383d9d3cb13bf8531d04a")
        (revision "0"))
    (package
      (name "tgs2png")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/zevlg/tgs2png")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0camvzapkfvr9v0nkk96n26rdmw0g8wbpv41i5l03j6bzdgm4myl"))))
      (build-system cmake-build-system)
      (native-inputs (list pkg-config))
      (inputs (list libpng rlottie))
      (arguments
       `(#:tests? #f))                            ;no tests
      (home-page "https://github.com/zevlg/tgs2png")
      (synopsis "Convert Telegram's TGS format into PNG images")
      (description
       "This program converts Telegram's animated stickers in TGS format into
a series of PNG images.")
      (license license:gpl3+))))
