;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022 Hilton Chain <hako@ultrarare.space>
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
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages language)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)
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

(define %telegram-version "4.2.2")

(define libyuv-for-telegram-desktop
  (let ((commit "ad890067f661dc747a975bc55ba3767fe30d4452")
        (revision "2211"))
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
        "01knnk4h247rq536097n9n3s3brxlbby3nv3ppdgsqfda3k159ll")))))

(define cmake-helpers-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/cmake_helpers.git")
          (commit "f49e254d8c5287752b5ed7b86bd84073f584768e")))
    (file-name
     (git-file-name "cmake-helpers-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0yqib2ndhpaj69z603knpcfga0ni978janb5i8rvhslqddvbzfjv"))))

(define codegen-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/codegen.git")
          (commit "8815d7aec9b901191d08445f29e2edd0aeba7b2c")))
    (file-name
     (git-file-name "codegen-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1ly958mjk25kfcpa174kvg134p9r751ixi275afj5kr57by9mbq2"))))

(define lib-base-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_base.git")
          (commit "d932f5048317b05dd414116741d995c82a528542")))
    (file-name
     (git-file-name "lib-base-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1fnirqxj4qq1gzx52rydrc5r6clw3316bh51dfg652jr8hj6wkp2"))))

(define lib-crl-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_crl.git")
          (commit "4e620bc383d032aadea8e6af02661f8c76695cec")))
    (file-name
     (git-file-name "lib-crl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "17bngj247qwq0dg4h37xdi5v1mk22y4yp7sp6ph3irmnz4awah0x"))))

(define lib-lottie-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_lottie.git")
          (commit "6ed1c22ed60692d2f288c9222fafd7d5cd62f904")))
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
          (commit "fd31f5bf382d2679eccdb1abaf8240a56c6a7abe")))
    (file-name
     (git-file-name "lib-rpl-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1fpq8nyh8wx6zkb3sjjrx9ydqzyhw4483bdk1i9blp0ijn9xxdxf"))))

(define lib-spellcheck-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_spellcheck.git")
          (commit "0e386e22cb6ba8a114b569840a635e096dcb645e")))
    (file-name
     (git-file-name "lib-spellcheck-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "06js7ccv6z3nbd4v2p4hp2prrlmz5ww46y3yb922pp7gm317dwyw"))))

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
          (commit "1ceaa0bbcfccb83dbf7f51d7f80a733ff2aa8c22")))
    (file-name
     (git-file-name "lib-ui-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0kyrgxi202xwy14mnx62h1kny0434f5fxqns1ydp24q2c2cr1cxn"))))

(define lib-webrtc-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webrtc.git")
          (commit "9b11599c3c56047cfa4c68b69f1fcc93b602c33a")))
    (file-name
     (git-file-name "lib-webrtc-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0158jx8hj4fv6qpw5pgcr2mdlihj0dxs060dg3iy61zz6q68z5dq"))))

(define lib-webview-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/desktop-app/lib_webview.git")
          (commit "546df65eb2424550ed84ce14fc9c5d1bb3586f35")))
    (file-name
     (git-file-name "lib-webview-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "0zzjwyw82bggncmmsw969lnjl04pklmqjqm77jjzadinivl52z0l"))))

(define tgcalls-for-telegram-desktop
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/TelegramMessenger/tgcalls.git")
          (commit "82c4921045c440b727c38e464f3a0539708423ff")))
    (file-name
     (git-file-name "tgcalls-for-telegram-desktop" %telegram-version))
    (sha256
     (base32
      "1109r17abh66yz91b65fn2g4ryfybnsr5g6075sjhbah1gccv9mk"))))

(define-public webrtc-for-telegram-desktop
  (let ((commit "621f3da55331733bf0d1b223786b96b68c03dca1")
        (revision "327"))
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
           (base32 "1ks1572k1jj7pmzwm79p2gdgi31dd4bs761bphnx32zyq4c6skxk"))
          (patches
           (search-patches
            ;; https://github.com/desktop-app/tg_owt/pull/101
            "webrtc-for-telegram-desktop-fix-gcc12-cstdint.patch"))
          (modules '((guix build utils)
                     (ice-9 ftw)
                     (srfi srfi-1)))
          (snippet
           #~(begin
               (let ((keep
                      '("abseil-cpp" "libsrtp" "rnnoise"
                        ;; Not available in Guix.
                        "pffft")))
                 (with-directory-excursion "src/third_party"
                   (for-each delete-file-recursively
                             (lset-difference string=?
                                              (scandir ".")
                                              (cons* "." ".." keep)))))
               ;; Unbundle openh264.
               (substitute* "CMakeLists.txt"
                 (("\\include\\(cmake\\/libopenh264\\.cmake\\)")""))))))
       (build-system cmake-build-system)
       (arguments
        (list
         #:tests? #f                    ; No target
         #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'unpack-additional-sources
               (lambda _
                 (let* ((third-party (string-append (getcwd) "/src/third_party"))
                        (crc32c-to (string-append third-party "/crc32c/src"))
                        (libyuv-to (string-append third-party "/libyuv")))
                   (copy-recursively #$(package-source crc32c) crc32c-to)
                   (copy-recursively #$libyuv-for-telegram-desktop
                                     libyuv-to)))))))
       (native-inputs (list pkg-config python-wrapper yasm))
       (inputs
        (list abseil-cpp-cxxstd17
              ffmpeg
              glib
              libdrm
              libglvnd
              libjpeg-turbo
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
              pipewire-0.3
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
        (base32 "16mcx4gwkl8s70a8gppxczmjsww1a3vmdrz3snfh986nvid64mq7"))
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
                     "tgcalls")))
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
           fcitx-qt5
           fcitx5-qt
           ffmpeg
           glib
           glibmm-2.64
           gtk+
           hime
           hunspell
           jemalloc
           kcoreaddons
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
           pulseaudio
           qrcodegen-cpp
           qtbase-5
           qtimageformats
           qtsvg-5
           qtwayland
           range-v3
           rlottie-for-telegram-desktop
           rnnoise
           wayland
           webkitgtk
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
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:configure-flags
         (list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl"
          ;; Enable extended queries system.
          "--enable-extf"
          ;; Include libevent-based net and timers.
          "--enable-libevent")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")
               #t))
           (add-after 'trigger-bootstrap 'patch-tl-parser
             (lambda _
               (delete-file "Makefile.tl-parser")
               (substitute* "Makefile.in"
                 (("include \\$\\{srcdir\\}/Makefile\\.tl-parser")
                  "")
                 (("\\$\\{EXE\\}/tl-parser")
                  "tl-parser"))
               #t))
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
       `(#:tests? #f                    ; No target
         #:imported-modules
         ((guix build copy-build-system)
          ,@%gnu-build-system-modules)
         #:modules
         (((guix build copy-build-system)
           #:prefix copy:)
          (guix build gnu-build-system)
          (guix build utils))
         #:configure-flags
         (list
          ;; Use gcrypt instead of openssl.
          "--disable-openssl")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'trigger-bootstrap
             (lambda _
               (delete-file "configure")
               #t))
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
                                 "/lib/tgl")))
               #t))
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
