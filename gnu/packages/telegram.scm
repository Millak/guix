;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022, 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2023 Saku Laesvuori <saku@laesvuori.fi>
;;; Copyright © 2023 Lu Hui <luhux76@gmail.com>
;;; Copyright © 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
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
  #:use-module (gnu packages jemalloc)
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
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt))

(define %telegram-version "4.8.1")

(define libyuv-for-telegram-desktop
  (let ((commit "77c2121f7e6b8e694d6e908bbbe9be24214097da")
        (revision "2439"))
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
        "1b4k8yskr9ffl5k8s9i0af1gn1pavsfixla26vh8bij69rdr7f9c")))))

(define cmake-helpers-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/cmake_helpers.git")
          (commit "6ab5543b3dd1e40979d258e46d03376931b6c37b")))
    (file-name
     (git-file-name "cmake-helpers-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0y96mvzs113zh8bdw1h3i6l0pgwg93rigrday8kfdg4magz686k6"))))

(define codegen-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/codegen.git")
          (commit "1a969faa0afb29d53af03e530775eccdfb8433f1")))
    (file-name
     (git-file-name "codegen-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1xmw8dfm51p5g20rlmzqnr72a14ngyxwq09an8clf1v5s6mmwvak"))))

(define lib-base-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_base.git")
          (commit "fd9adb30ee906ea02c125eaa58fcfae773fdc677")))
    (file-name
     (git-file-name "lib-base-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1m760mcfvgzia53nrs6wvjn353jvzlzln7c9fkx2dhpkigiynz83"))))

(define lib-crl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_crl.git")
          (commit "3d7e1e1f1321c3defd21c01882d674e485ecd8df")))
    (file-name
     (git-file-name "lib-crl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "06mzafnjpfr5ih297dh7bxm6bgpg0wy0gv2r2732n5szyrg9sdl6"))))

(define lib-lottie-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_lottie.git")
          (commit "3e9c2f1026e4b5aa3202fca4cc67ece36c7cebb2")))
    (file-name
     (git-file-name "lib-lottie-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0l57ibfij9xm4ww4s9cc63q1x8xzpc6ablwaji1krrn3xxksqdd4"))))

(define lib-qr-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_qr.git")
          (commit "501f4c3502fd872ab4d777df8911bdac32de7c48")))
    (file-name
     (git-file-name "lib-qr-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0hmwqj7a9vcy8wq7pd1qprl68im3zl5f1wzcn2zzk2wvi0389k9f"))))

(define lib-rpl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_rpl.git")
          (commit "8b1015d1bd57ef03fcd07a3eeddd3f5a9b688ade")))
    (file-name
     (git-file-name "lib-rpl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "12sdhaqby5vlvd5jsj12b3xsqiaknqvijv9ydlyxclx8zail64lv"))))

(define lib-spellcheck-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_spellcheck.git")
          (commit "ae89fefd239ecc47d4dab7ba29f9e230376a57d3")))
    (file-name
     (git-file-name "lib-spellcheck-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "147xbbcza5q4wcdipk5jplajzkc48971kg2s7qv5jlz33sxkw1lq"))))

(define lib-storage-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_storage.git")
          (commit "839609369d04615475cb1518636de3619106a917")))
    (file-name
     (git-file-name "lib-storage-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1l26h2fmqp9dcpr6pfvdd5sjb68j7yh0ms2lnr8na7jf5xqmkwwm"))))

(define lib-tl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_tl.git")
          (commit "36fb95c4de1339d2c8921ad6b2911858c3d0e0fa")))
    (file-name
     (git-file-name "lib-tl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "03rngnssnqwr7ad05qn64mwgji5fb0r3fp5ybkf951p8phr1jvzk"))))

(define lib-ui-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_ui.git")
          (commit "37531086ec21a8569deddedb11b402f8a3157b90")))
    (file-name
     (git-file-name "lib-ui-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0l4baalwdiwcwzn3wgrbyiaryi70lswillbpkzcjpavaa2pjg6b0"))))

(define lib-webrtc-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webrtc.git")
          (commit "b68a95ad4d1ae9a1827671100a7fd76cbe448c3f")))
    (file-name
     (git-file-name "lib-webrtc-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1c8jwdnn26n13yp0rp0l71q6xlxa6wp3cspbm3pnghw964jwgp3z"))))

(define lib-webview-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webview.git")
          (commit "f632fc84cbc62ae8abbbd05f81d472757a337c11")))
    (file-name
     (git-file-name "lib-webview-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0idsfkxq7l9kgyrhifys5l4jkhvbyxkgkp0qdq9218h7g0ldw84i"))))

(define tgcalls-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TelegramMessenger/tgcalls.git")
          (commit "2e2797648aac2588e7fe479c2e8b4455ec65c5e6")))
    (file-name
     (git-file-name "tgcalls-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "193m2gkvipijqbfd6a8mhg9nd63wlnshzgspk3pip57vk21l709z"))))

(define-public webrtc-for-telegram-desktop
  (let ((commit "0532942ac6176a66ef184fb728a4cbb02958fc0b")
        (revision "389"))
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
           (base32 "0fary99yl1ddk5zjpfy0pyb5brd268j41plcnvv9qjyf0wj9hf2k"))
          (patches
           (search-patches
            ;; https://github.com/desktop-app/tg_owt/pull/123
            "webrtc-for-telegram-desktop-unbundle-libsrtp.patch"))
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
       (inherit rlottie)
       (version
        (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri
           (git-reference
            (url "https://github.com/desktop-app/rlottie.git")
            (commit commit)))
          (file-name
           (git-file-name "rlottie-for-telegram-desktop" version))
          (sha256
           (base32 "14gwg3sn6xdx9ymnx5r0vfm4pk8dwk92s10a1wdvfbjpyrxll64i"))
          (modules '((guix build utils)))
          (snippet
           #~(begin
               (substitute* "meson.build"
                 (("werror=true") "werror=false"))))))))))

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
        (base32 "0g47ffamh1csp79yzkv28v3qjkhjacj0c7pjf53n1ks80j5hc2j0"))
       (patches
        (search-patches
         ;; https://github.com/telegramdesktop/tdesktop/pull/24126
         "telegram-desktop-allow-disable-libtgvoip.patch"))
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
              ;; Client applications must provide their own API-ID and API-HASH,
              ;; see also <https://core.telegram.org/api/obtaining_api_id>.
              ;; Here, we snarf the keys from the official Snaps, which are
              ;; also stored in <#$source/snap/snapcraft.yaml>.
              "-DTDESKTOP_API_ID=611335"
              "-DTDESKTOP_API_HASH=d524b414d21f4d37f08684c1df41ac9c"
              "-DTDESKTOP_DISABLE_LEGACY_TGVOIP=ON"
              "-DDESKTOP_APP_DISABLE_CRASH_REPORTS=ON"
              "-DDESKTOP_APP_DISABLE_AUTOUPDATE=ON"
              "-DDESKTOP_APP_USE_PACKAGED_RLOTTIE=ON")
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
                      ("Telegram/ThirdParty/tgcalls" #$tgcalls-for-telegram-desktop)))))
               (add-after 'install 'glib-or-gtk-compile-schemas
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-compile-schemas))
               (add-after 'glib-or-gtk-compile-schemas 'glib-or-gtk-wrap
                 (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list `(,glib "bin")
           `(,gtk+ "bin")
           pkg-config
           python-wrapper))
    (inputs
     (list abseil-cpp-cxxstd17
           alsa-lib
           c++-gsl
           crc32c
           fcitx-qt5
           fcitx5-qt
           ffmpeg
           glib
           glibmm-2.76
           gtk+
           hime
           hunspell
           jemalloc
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
           ,@%gnu-build-system-modules)
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
          ,@%gnu-build-system-modules)
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
