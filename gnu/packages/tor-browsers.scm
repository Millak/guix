;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017, 2023, 2024 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020-2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Baptiste Strazzul <bstrazzull@hotmail.fr>
;;; Copyright © 2022 SeerLite <seerlite@disroot.org>
;;; Copyright © 2024 Aleksandr Vityazev <avityazew@gmail.com>
;;; Copyright © 2020, 2021 André Batista <nandre@riseup.net>
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

(define-module (gnu packages tor-browsers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages browser-extensions)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages node)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system mozilla)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module (guix packages)
  #:use-module (ice-9 regex)
  #:use-module (guix utils))

(define (mozilla-locale locale changeset hash-string)
  (origin
    (method hg-fetch)
    (uri (hg-reference
          (url (string-append "https://hg.mozilla.org/l10n-central/"
                              locale))
          (changeset changeset)))
    (file-name (string-append "mozilla-locale-" locale))
    (sha256 (base32 hash-string))))

(define-syntax-rule (mozilla-locales (hash-string changeset locale) ...)
  #~(list (cons #$locale #$(mozilla-locale locale changeset hash-string))
          ...))

;; See tor-browser-build/rbm.conf for the list.
;; See browser/locales/l10n-changesets.json for the changeset.
;; See update-mozilla-locales in gnuzilla.scm to automate updating changeset.
(define %torbrowser-locales
  (mozilla-locales
   ;;                      sha256                            changeset    locale
   ;;---------------------------------------------------------------------------
   ("1218mldjxybhgzdi0myzkwjr2fgnysl71pl847kr7wyn1j8wk3a5" "c25d00080479" "ar")
   ("11c96jhfzd3h46qhblhvn2acsn895ykynarai8r5pf0655nfjs0j" "2de60e3d6d0c" "ca")
   ("0yhycgb3s3kydbzy6f2q7f7g2lp975spr092prf9xp8ha62ghby7" "609edd15f9a9" "cs")
   ("1kzx94n36c5vv954j7w65djvb37c178zazy25b35l71q2rvhmlhj" "2197a99c9a08" "da")
   ("13h7hk11bbd0yq8gqdv7ndbizkgwlm3ybz225l3x2b5cnyjxyg14" "b7a533e5edc9" "de")
   ("13ay27vdrqfv2ysyi7c2jmz50lps7rff9rmnws1z7jkj0a5chwrn" "20baf15379d8" "el")
   ("0mdr5b6pqxjmg9c8064x3hpf53h6w9j8ghl32655sx9jh4v3ykza" "beff1baac7c5" "es-ES")
   ("1pnyg09j6r15w8m62lwj89x6rz4br877z60p8s1hlrb9hj2s3vdx" "ebe0b60b0b36" "fa")
   ("067r505626cvlrsalnndf2ykz3nnkiy0b8yaxzf1rracpzmp0hni" "d5ae6a933d71" "fi")
   ("0026zzjv2bqc8sg06yvyd0mhny6mwwvhpvzjrhv2fi5v4wkxapdj" "496c2eb73b82" "fr")
   ("1dxcp26y8siap4k54zsw7mqa7k0l4f1505rdf4hnnxrzf9a643g5" "2fcccb5b19b3" "ga-IE")
   ("14v6xnlyj65hzaz2rmzxcl4skjgm48426jgr9mwkwiqis587lp4a" "c53cea027f8f" "he")
   ("04fdw2gzb64fb51bvs0bwsidzlvkdahmcy76vdg3gfcxslnlpi3y" "5a76dd3b5d5c" "hu")
   ("0bpyxpclfy74bcsjrs1ajh2am4zv6j6j9q4gc4vz8pgvzy9354zp" "6e6de17dcac4" "id")
   ("131ph8n235kr6nj1pszk0m00nh6kl360r4qvx4hjm8s22mw0k8qd" "536265635dfe" "is")
   ("03fbp4vgkwyimfmbm4n8blx1m16yhms2wm8j4wlx2h3cpxp5r71k" "91951e37e2b8" "it")
   ("0ncm531d7ih7phcn9d83zwq0dfphvmzg3gmhqmrrkkbydi1g3pbb" "895dcf8bb524" "ja")
   ("1x3110v730ak522zfm8j3r3v1x5lq3ig82kcgyxkc49xywajy0ni" "d0819a64fc40" "ka")
   ("14rc9mr4ngxdzwpjagzhz47jazgp1a6vwb0vbwj31yxv9iwkrgzi" "6ef881aff44b" "ko")
   ("1gl85z550amhbaxp39zdj6yyvashj9xd4ampfhm9jdpbf6n5j2l8" "afcbc29a15e5" "lt")
   ("1hz5g3iprfkbd88ncppyksbhlws73lhs75nf62hangw8l73wdn69" "84f3d6c7e2da" "mk")
   ("14aq37ngnav5m2kcb4wavxwhp28ad4jzdkzc7i64h0qvvxq5n3hf" "c9ec27a5db3d" "ms")
   ("0h7dlnawm5mbcx4qdlz5c7n4axz2dpa677v13ljdgm2b5w76msmq" "5c1480ccc040" "my")
   ("1b12azc1n8j1i2l20v66r74q79zqjvc5sf9pd8rmj3xd0fkxzdp2" "fc1896a0a24d" "nb-NO")
   ("1fh4dhlb6hynlpb2997gssv9v8zk5b7qrw0sclggczb5pcpjk6wc" "7e6da4f01bdb" "nl")
   ("1w8x3jjrd28f6g6ywwxldizpiipfkr63dzqd74kjpg24s2lqzp80" "e86a451a9cb5" "pl")
   ("1v3v4n82sn7a4h2d9n653fmgc31mikacf59lvdj6gbwvzpjb5yfa" "94c3dbb67a5d" "pt-BR")
   ("061a4z0lffgks3wlr6yh5z7x9arcn804mjwvffcmibs106vzamyq" "470b13b5805b" "ro")
   ("1fxgh7nfxpg2zknvfff8igq9q1vm5n4q033v7lm2c0xn3dbl8m28" "402b2ecbf04d" "ru")
   ("1i119g6dnhzxmpaz5r2jr9yzm1v24v2q6m3z6bfz2yihj0w7m133" "f637484e72b6" "sq")
   ("1nllh3ax323sxwhj7xvwvbfnh4179332pcmpfyybw1vaid3nr39k" "bb2d5d96d69e" "sv-SE")
   ("136m68fd0641k3qqmsw6zp016cvvd0sipsyv6rx2b9nli56agz57" "0e6c56bf2ac9" "th")
   ("0q8p8bwq8an65yfdwzm4dhl6km68r83bv5i17kay2gak8msxxhsb" "91e611ae3f19" "tr")
   ("1f2g7rnxpr2gjzngfsv19g11vk9zqpyrv01pz07mw2z3ffbkxf0j" "99d5ffa0b81e" "uk")
   ("1rizwsfgr7vxm31bin3i7bwhcqa67wcylak3xa387dvgf1y9057i" "5fd44724e22d" "vi")
   ("02ifa94jfii5f166rwdvv8si3bazm4bcf4qhi59c8f1hxbavb52h" "081aeb1aa308" "zh-CN")
   ("0qx9sh56pqc2x5qrh386cp1fi1gidhcmxxpvqkg9nh2jbizahznr" "9015a180602e" "zh-TW")))

;; We copy the official build id, which is defined at
;; tor-browser-build/rbm.conf (browser_release_date).
(define %torbrowser-build-date "20240510190000")

;; To find the last version, look at https://www.torproject.org/download/.
(define %torbrowser-version "13.0.16")

;; To find the last Firefox version, browse
;; https://archive.torproject.org/tor-package-archive/torbrowser/<%torbrowser-version>
;; There should be only one archive that starts with
;; "src-firefox-tor-browser-".
(define %torbrowser-firefox-version "115.12.0esr-13.0-1-build1")

;; See tor-browser-build/projects/translation/config.
(define torbrowser-translation-base
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "f28525699864f4e3d764c354130bd898ce5b20aa")))
    (file-name "translation-base-browser")
    (sha256
     (base32
      "1vf6nl7fdmlmg2gskf3w1xlsgcm0pxi54z2daz5nwr6q9gyi0lkf"))))

;; See tor-browser-build/projects/translation/config.
(define torbrowser-translation-specific
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "b5d79336411e5a59c4861341ef9aa7353e0bcad9")))
    (file-name "translation-tor-browser")
    (sha256
     (base32
      "0ahz69pxhgik7ynmdkbnx7v5l2v392i6dswjz057g4hwnd7d34fb"))))

(define torbrowser-assets
  ;; This is a prebuilt Torbrowser from which we take the assets we need.
  (package
    (name "torbrowser-assets")
    (version %torbrowser-version)
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://archive.torproject.org/tor-package-archive/torbrowser/"
         version "/tor-browser-linux-x86_64-" version ".tar.xz"))
       (sha256
        (base32
         "1kffam66bsaahzx212hw9lb03jwfr24hivzg067iyzilsldpc9c1"))))
    (arguments
     (list
      #:install-plan
      ''(("Browser" "." #:include-regexp
          ("^\\./TorBrowser/Data/Tor/torrc-defaults"
           "^\\./fonts/"
           "^\\./fontconfig/fonts.conf")))))
    (build-system copy-build-system)
    (home-page "https://www.torproject.org")
    (synopsis "Tor Browser assets")
    (description "This package contains fonts and configuration files for Tor
Browser.")
    (license license:silofl1.1)))

(define* (make-torbrowser #:key
                          moz-app-name
                          moz-app-remotingname
                          branding-directory
                          translation-base
                          translation-specific
                          assets
                          locales
                          build-date
                          base-browser-version)
  (package
    (name "torbrowser")
    (version %torbrowser-version)
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://archive.torproject.org/tor-package-archive/torbrowser/"
         version "/src-firefox-tor-browser-" %torbrowser-firefox-version
         ".tar.xz"))
       (sha256
        (base32
         "1b70zyjyai6kk4y1kkl8jvrs56gg7z31kkad6bmdpd8jw4n71grx"))))
    (build-system mozilla-build-system)
    (inputs
     (list go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird
           tor-client
           alsa-lib
           bash-minimal                 ;for wrap-program
           bzip2
           cups
           dbus-glib
           gdk-pixbuf
           glib
           gtk+
           ;; UNBUNDLE-ME! graphite2
           cairo
           pango
           freetype
           ;; UNBUNDLE-ME! harfbuzz
           libcanberra
           libgnome
           libjpeg-turbo
           libpng-apng
           ;; UNBUNDLE-ME! libogg
           ;; UNBUNDLE-ME! libtheora ; wants theora-1.2, not yet released
           ;; UNBUNDLE-ME! libvorbis
           libxft
           libevent
           libxinerama
           libxscrnsaver
           libxcomposite
           libxt
           libffi
           ffmpeg
           libvpx
           icu4c-73
           pixman
           pulseaudio
           mesa
           pciutils
           mit-krb5
           hunspell
           libnotify
           nspr
           ;; UNBUNDLE-ME! nss  (pending upgrade of 'nss' to 3.90 or later)
           shared-mime-info
           sqlite
           eudev
           unzip
           zip
           zlib))
    (native-inputs
     (list
      rust
      `(,rust "cargo")
      rust-cbindgen
      llvm-15
      clang-15
      perl
      node-lts
      python-wrapper
      yasm
      nasm                         ; XXX FIXME: only needed on x86_64 and i686
      pkg-config
      m4
      which))
    (arguments
     (list
      #:tests? #f                       ;not worth the cost

      ;; Some dynamic lib was determined at runtime, so rpath check may fail.
      #:validate-runpath? #f

      #:configure-flags
      #~(list
         "--without-relative-data-dir" ;store is read-only
         "--disable-base-browser-update"
         ;; Default is "default", which is the same as "nightly".
         "--enable-update-channel=release"
         ;; This is useless right now but it might be used in the future.
         ;; (See nsAppFileLocationProvider.cpp.)
         (string-append "--with-user-appdir=." #$moz-app-name)
         (string-append "--with-branding=" #$branding-directory)
         (string-append "--prefix=" #$output)
         (string-append "--with-base-browser-version="
                        #$base-browser-version)

         "--enable-application=browser"
         "--with-distribution-id=org.gnu"
         "--enable-geckodriver"
         ;; Do not require addons in the global app or system directories to
         ;; be signed by Mozilla.
         "--with-unsigned-addon-scopes=app,system"
         "--allow-addon-sideload"

         "--enable-pulseaudio"

         "--disable-tests"
         "--disable-updater"
         "--disable-crashreporter"
         ;; The --disable-eme option is not available on aarch64.
         #$(if (target-aarch64?) "" "--disable-eme")

         ;; Building with debugging symbols takes ~5GiB, so disable it.
         "--disable-debug"
         "--disable-debug-symbols"

         "--enable-rust-simd"
         "--enable-release"
         "--enable-optimize"
         "--enable-strip"
         "--disable-elf-hack"

         ;; Clang is needed to build Stylo, Mozilla's new CSS engine.  We must
         ;; specify the clang paths manually, because otherwise the Mozilla
         ;; build system looks in the directories returned by llvm-config
         ;; --bindir and llvm-config --libdir, which return paths in the llvm
         ;; package where clang is not found.
         (string-append "--with-clang-path="
                        (search-input-file %build-inputs "bin/clang"))
         (string-append "--with-libclang-path="
                        (dirname (search-input-file %build-inputs
                                                    "lib/libclang.so")))

         ;; Hack to work around missing "unofficial" branding in icecat.
         "--enable-official-branding"

         ;; TODO: Add support for wasm sandboxed libraries.
         "--without-wasm-sandboxed-libraries"

         ;; Avoid bundled libraries.
         "--with-system-jpeg"           ;must be libjpeg-turbo
         "--with-system-png"            ;must be libpng-apng
         "--with-system-zlib"
         ;; UNBUNDLE-ME! "--with-system-bz2"
         ;; UNBUNDLE-ME! "--with-system-libevent"
         ;; UNBUNDLE-ME! "--with-system-ogg"
         ;; UNBUNDLE-ME! "--with-system-vorbis"
         ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
         ;; UNBUNDLE-ME! "--with-system-libvpx"
         "--with-system-icu"
         "--with-system-nspr"
         ;; UNBUNDLE-ME! "--with-system-nss" ; pending upgrade of 'nss' to 3.90

         ;; UNBUNDLE-ME! "--with-system-harfbuzz"
         ;; UNBUNDLE-ME! "--with-system-graphite2"
         "--enable-system-pixman"
         "--enable-system-ffi"
         ;; UNBUNDLE-ME! "--enable-system-sqlite"
         )

      #:imported-modules %cargo-utils-modules ;for `generate-all-checksums'

      #:modules `((ice-9 ftw)
                  (ice-9 match)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%default-gnu-imported-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'apply-guix-specific-patches
            (lambda _
              (for-each
               (lambda (file) (invoke "patch" "--force" "-p1" "-i" file))
               '(#$(local-file
                    (search-patch "icecat-compare-paths.patch"))
                 #$(local-file
                    (search-patch "icecat-use-system-wide-dir.patch"))))))
          (add-after 'apply-guix-specific-patches 'remove-bundled-libraries
            (lambda _
              ;; Remove bundled libraries that we don't use, since they may
              ;; contain unpatched security flaws, they waste disk space and
              ;; memory, and may cause confusion.
              (for-each (lambda (file)
                          (format #t "deleting '~a'...~%" file)
                          (delete-file-recursively file))
                        '( ;; FIXME: Removing the bundled icu breaks configure.
                          ;;   * The bundled icu headers are used in some places.
                          ;;   * The version number is taken from the bundled copy.
                          ;;"intl/icu"
                          ;;
                          ;; FIXME: A script from the bundled nspr is used.
                          ;;"nsprpub"
                          ;;
                          ;; FIXME: Some of the bundled NSS sources are used
                          ;; to build third_party/prio.
                          ;;"security/nss"
                          ;;
                          ;; TODO: Use more system media libraries.  See:
                          ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                          ;;   * libtheora: esr60 wants v1.2, not yet released.
                          ;;   * soundtouch: avoiding the bundled library would
                          ;;     result in some loss of functionality.  There's
                          ;;     also an issue with exception handling
                          ;;     configuration.  It seems that this is needed in
                          ;;     some moz.build:
                          ;;       DEFINES['ST_NO_EXCEPTION_HANDLING'] = 1
                          ;;   * libopus
                          ;;   * speex
                          ;;
                          "modules/freetype2"
                          ;; "media/libjpeg"  ; needed for now, because media/libjpeg/moz.build is referenced from config/external/moz.build
                          ;; UNBUNDLE-ME! "modules/zlib"
                          ;; UNBUNDLE-ME! "ipc/chromium/src/third_party/libevent"
                          ;; UNBUNDLE-ME! "media/libvpx"
                          ;; UNBUNDLE-ME! "media/libogg"
                          ;; UNBUNDLE-ME! "media/libvorbis"
                          ;; UNBUNDLE-ME! "media/libtheora" ; wants theora-1.2, not yet released
                          ;; UNBUNDLE-ME! "media/libtremor"
                          ;; UNBUNDLE-ME! "gfx/harfbuzz"
                          ;; UNBUNDLE-ME! "gfx/graphite2"
                          "js/src/ctypes/libffi"
                          ;; UNBUNDLE-ME! "db/sqlite3"
                          ))))
          (add-after 'remove-bundled-libraries 'fix-ffmpeg-runtime-linker
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Arrange to load libavcodec.so by its absolute file name.
              (substitute* "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                (("libavcodec\\.so")
                 (search-input-file inputs "lib/libavcodec.so")))))
          (add-after 'fix-ffmpeg-runtime-linker 'build-sandbox-whitelist
            (lambda* (#:key inputs #:allow-other-keys)
              (define (runpath-of lib)
                (call-with-input-file lib
                  (compose elf-dynamic-info-runpath
                           elf-dynamic-info
                           parse-elf
                           get-bytevector-all)))
              (define (runpaths-of-input label)
                (let* ((dir (string-append (assoc-ref inputs label) "/lib"))
                       (libs (find-files dir "\\.so$")))
                  (append-map runpath-of libs)))
              ;; Populate the sandbox read-path whitelist as needed by ffmpeg.
              (let* ((whitelist
                      (map (cut string-append <> "/")
                           (delete-duplicates
                            `(,(string-append (assoc-ref inputs "shared-mime-info")
                                              "/share/mime")
                              ,@(append-map runpaths-of-input
                                            '("mesa" "ffmpeg"))))))
                     (whitelist-string (string-join whitelist ",")))
                (with-output-to-file "whitelist.txt"
                  (lambda ()
                    (display whitelist-string))))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"))
                (for-each (lambda (file)
                            (format #t "patching checksums in ~a~%" file)
                            (substitute* file
                              (("^checksum = \".*\"")
                               (string-append "checksum = \"" null-hash "\""))))
                          (find-files "." "Cargo.lock$"))
                (for-each generate-all-checksums
                          '("services"
                            "js"
                            "third_party/rust"
                            "dom/media"
                            "dom/webauthn"
                            "toolkit"
                            "gfx"
                            "storage"
                            "modules"
                            "xpcom/rust"
                            "media"
                            "mozglue/static/rust"
                            "netwerk"
                            "remote"
                            "intl"
                            "servo"
                            "security/manager/ssl"
                            "build")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invokation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("\"--frozen\",") ""))))
          (delete 'bootstrap)
          (add-before 'configure 'setenv
            (lambda _
              (setenv "CONFIG_SHELL" (which "bash"))
              ;; Install location is prefix/lib/$MOZ_APP_NAME.  Also
              ;; $MOZ_APP_NAME is the executable name.  Default is
              ;; "firefox".
              (setenv "MOZ_APP_NAME" #$moz-app-name)
              ;; Profile location (relative to "~/.").  Default is
              ;; lower($MOZ_APP_VENDOR/$MOZ_APP_BASENAME), which is:
              ;; ~/.tor project/firefox.
              (setenv "MOZ_APP_PROFILE" #$(in-vicinity
                                           moz-app-name "browser"))
              ;; WM_CLASS (default is "$MOZ_APP_NAME-$MOZ_UPDATE_CHANNEL").
              (setenv "MOZ_APP_REMOTINGNAME" #$moz-app-remotingname)
              ;; Persistent state directory for the build system (default is
              ;; $HOME/.mozbuild).
              (setenv "MOZBUILD_STATE_PATH"
                      (in-vicinity (getcwd) ".mozbuild"))
              (setenv "MOZ_CHROME_MULTILOCALE"
                      (string-join (map car #$locales)))
              ;; Make build reproducible.
              (setenv "MOZ_BUILD_DATE" #$build-date)))
          (add-before 'configure 'mozconfig
            (lambda* (#:key configure-flags #:allow-other-keys)
              (with-output-to-file "mozconfig"
                (lambda ()
                  (format #t ". $topsrcdir/mozconfig-linux-x86_64~%")
                  (for-each (lambda (flag)
                              (format #t "ac_add_options ~a~%" flag))
                            configure-flags)))))
          ;; See tor-browser-build/projects/firefox/build.
          (add-before 'configure 'copy-firefox-locales
            (lambda _
              (let ((l10ncentral ".mozbuild/l10n-central"))
                (mkdir-p l10ncentral)
                (for-each
                 (lambda (lang)
                   (copy-recursively (cdr lang)
                                     (in-vicinity l10ncentral
                                                  (car lang))))
                 #$locales))))
          (add-after 'copy-firefox-locales 'copy-basebrowser-locales
            (lambda _
              (let ((l10ncentral ".mozbuild/l10n-central"))
                ;; Temporary copy so that we can use ‘mv’ to mimic
                ;; tor-browser-build/projects/firefox/build.
                (copy-recursively #$translation-base
                                  "translation-base-browser")
                (for-each
                 (lambda (lang)
                   (system
                    (format
                     #f (string-join
                         '("mv"
                           "translation-base-browser/~a/base-browser.ftl"
                           "~a/~a/browser/browser/"))
                     lang l10ncentral lang))
                   (system
                    (format
                     #f (string-join
                         '("mv"
                           "translation-base-browser/~a/*"
                           "~a/~a/browser/chrome/browser/"))
                     lang l10ncentral lang)))
                 (map car #$locales)))))
          (add-after 'copy-basebrowser-locales 'copy-torbrowser-locales
            (lambda _
              (let ((l10ncentral ".mozbuild/l10n-central"))
                ;; Temporary copy so that we can use ‘mv’ to mimic
                ;; tor-browser-build/projects/firefox/build.
                (copy-recursively #$translation-specific
                                  "translation-tor-browser")
                (for-each
                 (lambda (lang)
                   (system
                    (format
                     #f (string-join
                         '("mv"
                           "translation-tor-browser/~a/tor-browser.ftl"
                           "~a/~a/browser/browser/"))
                     lang l10ncentral lang))
                   (system
                    (format
                     #f (string-join
                         '("mv"
                           "translation-tor-browser/~a/cryptoSafetyPrompt.properties"
                           "~a/~a/browser/chrome/browser/"))
                     lang l10ncentral lang))
                   (system
                    (format
                     #f (string-join
                         '("mv"
                           "translation-tor-browser/~a"
                           "toolkit/torbutton/chrome/locale/"))
                     lang))
                   (let ((port (open-file "toolkit/torbutton/jar.mn" "a")))
                     (format port "% locale torbutton ~a %locale/~a/~%"
                             lang lang)
                     (format port "  locale/~a/ (chrome/locale/~a/*)~%"
                             lang lang)
                     (close port)))
                 (map car #$locales)))))
          (replace 'configure
            (lambda _
              (invoke "./mach" "configure")))
          (add-before 'build 'fix-addons-placeholder
            (lambda _
              (substitute*
                  "toolkit/locales/en-US/toolkit/about/aboutAddons.ftl"
                (("addons.mozilla.org") "gnuzilla.gnu.org"))))
          (add-before 'build 'add-bridges ;see deploy.sh
            (lambda _
              (let ((port (open-file
                           "browser/app/profile/000-tor-browser.js" "a")))
                (display
                 "#include ../../../tools/torbrowser/bridges.js" port)
                (newline port)
                (close port))))
          (replace 'build
            (lambda* (#:key (make-flags '()) (parallel-build? #t)
                      #:allow-other-keys)
              (apply invoke "./mach" "build"
                     ;; mach will use a wide parallel build if possible by
                     ;; default, so reign it in if requested.
                     `(,(string-append
                         "-j" (number->string (if parallel-build?
                                                  (parallel-job-count)
                                                  1)))
                       ,@make-flags))))
          ;; See tor-browser-build/projects/firefox/build.
          (add-after 'build 'build-locales
            (lambda _
              (system (string-join '("./mach package-multi-locale --locales"
                                     "en-US $MOZ_CHROME_MULTILOCALE")))))
          (add-after 'build-locales 'neutralise-store-references
            (lambda _
              ;; Mangle the store references to compilers & other build tools in
              ;; about:buildconfig, reducing IceCat's closure by 1 GiB on x86-64.
              (let* ((obj-dir (match (scandir "." (cut string-prefix? "obj-" <>))
                                ((dir) dir)))
                     (file (string-append
                            obj-dir
                            "/dist/bin/chrome/toolkit/content/global/buildconfig.html")))
                (substitute* file
                  (("[0-9a-df-np-sv-z]{32}" hash)
                   (string-append (string-take hash 8)
                                  "<!-- Guix: not a runtime dependency -->"
                                  (string-drop hash 8)))))))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "./mach" "install")
              ;; The geckodriver binary is not installed by the above, for some
              ;; reason.  Use 'find-files' to avoid having to deal with the
              ;; system/architecture-specific file name.
              (install-file (first (find-files "." "geckodriver"))
                            (string-append #$output "/bin"))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((gtk #$(this-package-input "gtk+"))
                     (gtk-share (string-append gtk "/share"))
                     (fonts.conf (format #f "~a/lib/~a/fontconfig/fonts.conf"
                                         #$output #$moz-app-name))
                     (ld-libs '#$(cons
                                  (file-append
                                   (this-package-input "libcanberra")
                                   "/lib/gtk-3.0/modules")
                                  (map (lambda (label)
                                         (file-append (this-package-input label) "/lib"))
                                       '("libpng-apng"
                                         "libxscrnsaver"
                                         "mesa"
                                         "pciutils"
                                         "mit-krb5"
                                         "eudev"
                                         "pulseaudio"
                                         ;; For the integration of native notifications
                                         ;; (same reason as icedove)
                                         "libnotify")))))
                (wrap-program (format #f "~a/lib/~a/~a"
                                      #$output #$moz-app-name #$moz-app-name)
                  `("XDG_DATA_DIRS" prefix (,gtk-share))
                  ;; The following line is commented out because the icecat
                  ;; package on guix has been observed to be unstable when
                  ;; using wayland, and the bundled extensions stop working.
                  ;;   `("MOZ_ENABLE_WAYLAND" = ("1"))
                  `("LD_LIBRARY_PATH" prefix ,ld-libs)
                  `("FONTCONFIG_FILE" prefix (,fonts.conf))))))
          (add-after 'wrap-program 'install-desktop-entry
            (lambda _
              (let ((apps (in-vicinity #$output "share/applications")))
                (mkdir-p apps)
                (make-desktop-entry-file
                 (string-append apps "/" #$moz-app-name ".desktop")
                 #:name #$moz-app-remotingname
                 #:exec (format #f "~a/bin/~a %u" #$output #$moz-app-name)
                 #:categories '("Network" "WebBrowser" "Security")
                 #:startup-w-m-class #$moz-app-remotingname
                 #:icon #$moz-app-name))))
          (add-after 'install-desktop-entry 'install-icons
            (lambda* (#:key inputs #:allow-other-keys)
              (for-each
               (lambda (size)
                 (let ((oldpath (string-append
                                 #$branding-directory "/default" size ".png"))
                       (newpath (string-append
                                 #$output "/share/icons/hicolor/" size "x"
                                 size "/apps/" #$moz-app-name ".png")))
                   (mkdir-p (dirname newpath))
                   (copy-file oldpath newpath)))
               '("16" "22" "24" "32" "48" "64" "128" "256"))))
          (add-after 'install 'deploy-fonts
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((lib (string-append #$output "/lib/" #$moz-app-name)))
                ;; Fonts
                (copy-recursively (in-vicinity #$assets "fontconfig")
                                  (in-vicinity lib "fontconfig"))
                (substitute* (in-vicinity lib "fontconfig/fonts.conf")
                  (("<dir>fonts</dir>")
                   (format #f "<dir>~a</dir>" (in-vicinity lib "fonts"))))
                (delete-file-recursively (in-vicinity lib "fonts"))
                (copy-recursively (in-vicinity #$assets "fonts")
                                  (in-vicinity lib "fonts")))))
          (add-after 'deploy-fonts 'deploy-tor-assets
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((lib (in-vicinity #$output "lib/torbrowser"))
                    (tor #$(this-package-input "tor-client")))
                ;; TorBrowser/Data/Tor/torrc-defaults
                (copy-recursively (in-vicinity #$assets "TorBrowser")
                                  (in-vicinity lib "TorBrowser"))
                (substitute*
                    (in-vicinity lib "TorBrowser/Data/Tor/torrc-defaults")
                  (("exec ./TorBrowser/Tor/PluggableTransports/lyrebird")
                   (string-append
                    "exec " (search-input-file inputs "bin/lyrebird"))))
                ;; The geoip and geoip6 files are in the same directory as
                ;; torrc-defaults.  (See TorProcess.sys.mjs.)
                (mkdir-p (in-vicinity lib "TorBrowser/Data/Tor"))
                (copy-file (in-vicinity tor "share/tor/geoip6")
                           (in-vicinity lib "TorBrowser/Data/Tor/geoip6"))
                (copy-file (in-vicinity tor "share/tor/geoip")
                           (in-vicinity lib "TorBrowser/Data/Tor/geoip")))))
          (add-after 'install 'autoconfig
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((lib (string-append #$output "/lib/" #$moz-app-name))
                    (config-file (string-append #$moz-app-name ".cfg")))
                (with-output-to-file (in-vicinity
                                      lib "defaults/pref/autoconfig.js")
                  (lambda ()
                    (format #t "// first line must be a comment~%")
                    (format #t "pref(~s, ~s);~%"
                            "general.config.filename" config-file)
                    (format #t "pref(~s, ~a);~%"
                            "general.config.obscure_value" "0")))
                (with-output-to-file (in-vicinity lib config-file)
                  (lambda ()
                    (format #t "// first line must be a comment~%")
                    ;; Required for Guix packaged extensions
                    ;; SCOPE_PROFILE=1, SCOPE_APPLICATION=4, SCOPE_SYSTEM=8
                    ;; Default is 5.
                    (format #t "pref(~s, ~a);~%"
                            "extensions.enabledScopes" "13")
                    (format #t "pref(~s, ~s);~%"
                            "security.sandbox.content.read_path_whitelist"
                            (call-with-input-file "whitelist.txt"
                              get-string-all))
                    ;; Add-ons pannel (see settings.js in Icecat source).
                    (format #t "pref(~s, ~s);~%"
                            "extensions.getAddons.search.browseURL"
                            "https://gnuzilla.gnu.org/mozzarella")
                    (format #t "pref(~s, ~s);~%"
                            "extensions.getAddons.get.url"
                            "https://gnuzilla.gnu.org/mozzarella")
                    (format #t "pref(~s, ~s);~%"
                            "extensions.getAddons.link.url"
                            "https://gnuzilla.gnu.org/mozzarella")
                    (format #t "pref(~s, ~s);~%"
                            "extensions.getAddons.discovery.api_url"
                            "https://gnuzilla.gnu.org/mozzarella")
                    (format #t "pref(~s, ~s);~%"
                            "extensions.getAddons.langpacks.url"
                            "https://gnuzilla.gnu.org/mozzarella")
                    (format #t "pref(~s, ~s);~%"
                            "lightweightThemes.getMoreURL"
                            "https://gnuzilla.gnu.org/mozzarella")
                    ;; FIXME: https://github.com/NixOS/nixpkgs/issues/307095
                    (format #t "pref(~s, ~a);~%"
                            "widget.use-xdg-desktop-portal.file-picker"
                            "1"))))))
          (add-after 'autoconfig 'autoconfig-tor
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((lib (in-vicinity #$output "lib/torbrowser"))
                    (config-file (string-append #$moz-app-name ".cfg")))
                (let ((port (open-file (in-vicinity lib config-file) "a")))
                  (format port "pref(~s, ~s);~%"
                          "extensions.torlauncher.torrc-defaults_path"
                          (in-vicinity
                           lib "TorBrowser/Data/Tor/torrc-defaults"))
                  (format port "pref(~s, ~s);~%"
                          "extensions.torlauncher.tor_path"
                          (search-input-file inputs "bin/tor"))
                  (close port))))))))
    (propagated-inputs
     (list noscript/icecat))
    (native-search-paths
     (list (search-path-specification
            (variable "ICECAT_SYSTEM_DIR")
            (separator #f)              ;single entry
            (files '("lib/icecat")))))
    (home-page "https://www.torproject.org")
    (synopsis "Anonymous browser derived from Mozilla Firefox")
    (description
     "Tor Browser is the Tor Project version of Firefox browser.  It is the
only recommended way to anonymously browse the web that is supported by the
project.  It modifies Firefox in order to avoid many known application level
attacks on the privacy of Tor users.")
    (license license:mpl2.0)))       ;And others, see
                                     ;toolkit/content/license.html

(define-public torbrowser
  (make-torbrowser #:moz-app-name "torbrowser"
                   #:moz-app-remotingname "Tor Browser"
                   #:branding-directory "browser/branding/tb-release"
                   #:translation-base torbrowser-translation-base
                   #:translation-specific torbrowser-translation-specific
                   #:assets torbrowser-assets
                   #:locales %torbrowser-locales
                   #:build-date %torbrowser-build-date
                   #:base-browser-version %torbrowser-version))


;; See tor-browser-build/rbm.conf for the list.
;; See browser/locales/l10n-changesets.json for the changeset.
;; See update-mozilla-locales in gnuzilla.scm to automate updating changeset.
(define %mullvadbrowser-locales
  (mozilla-locales
   ;;                      sha256                            changeset    locale
   ;;---------------------------------------------------------------------------
   ("1218mldjxybhgzdi0myzkwjr2fgnysl71pl847kr7wyn1j8wk3a5" "c25d00080479" "ar")
   ("1kzx94n36c5vv954j7w65djvb37c178zazy25b35l71q2rvhmlhj" "2197a99c9a08" "da")
   ("13h7hk11bbd0yq8gqdv7ndbizkgwlm3ybz225l3x2b5cnyjxyg14" "b7a533e5edc9" "de")
   ("0mdr5b6pqxjmg9c8064x3hpf53h6w9j8ghl32655sx9jh4v3ykza" "beff1baac7c5" "es-ES")
   ("1pnyg09j6r15w8m62lwj89x6rz4br877z60p8s1hlrb9hj2s3vdx" "ebe0b60b0b36" "fa")
   ("067r505626cvlrsalnndf2ykz3nnkiy0b8yaxzf1rracpzmp0hni" "d5ae6a933d71" "fi")
   ("0026zzjv2bqc8sg06yvyd0mhny6mwwvhpvzjrhv2fi5v4wkxapdj" "496c2eb73b82" "fr")
   ("03fbp4vgkwyimfmbm4n8blx1m16yhms2wm8j4wlx2h3cpxp5r71k" "91951e37e2b8" "it")
   ("0ncm531d7ih7phcn9d83zwq0dfphvmzg3gmhqmrrkkbydi1g3pbb" "895dcf8bb524" "ja")
   ("14rc9mr4ngxdzwpjagzhz47jazgp1a6vwb0vbwj31yxv9iwkrgzi" "6ef881aff44b" "ko")
   ("0h7dlnawm5mbcx4qdlz5c7n4axz2dpa677v13ljdgm2b5w76msmq" "5c1480ccc040" "my")
   ("1b12azc1n8j1i2l20v66r74q79zqjvc5sf9pd8rmj3xd0fkxzdp2" "fc1896a0a24d" "nb-NO")
   ("1fh4dhlb6hynlpb2997gssv9v8zk5b7qrw0sclggczb5pcpjk6wc" "7e6da4f01bdb" "nl")
   ("1w8x3jjrd28f6g6ywwxldizpiipfkr63dzqd74kjpg24s2lqzp80" "e86a451a9cb5" "pl")
   ("1v3v4n82sn7a4h2d9n653fmgc31mikacf59lvdj6gbwvzpjb5yfa" "94c3dbb67a5d" "pt-BR")
   ("1fxgh7nfxpg2zknvfff8igq9q1vm5n4q033v7lm2c0xn3dbl8m28" "402b2ecbf04d" "ru")
   ("1nllh3ax323sxwhj7xvwvbfnh4179332pcmpfyybw1vaid3nr39k" "bb2d5d96d69e" "sv-SE")
   ("136m68fd0641k3qqmsw6zp016cvvd0sipsyv6rx2b9nli56agz57" "0e6c56bf2ac9" "th")
   ("0q8p8bwq8an65yfdwzm4dhl6km68r83bv5i17kay2gak8msxxhsb" "91e611ae3f19" "tr")
   ("02ifa94jfii5f166rwdvv8si3bazm4bcf4qhi59c8f1hxbavb52h" "081aeb1aa308" "zh-CN")
   ("0qx9sh56pqc2x5qrh386cp1fi1gidhcmxxpvqkg9nh2jbizahznr" "9015a180602e" "zh-TW")))

;; We copy the official build id, which can be found there:
;; https://cdn.mullvad.net/browser/update_responses/update_1/release.
(define %mullvadbrowser-build-date "20240510190000")

;; To find the last version, look at
;; https://mullvad.net/en/download/browser/linux.
(define %mullvadbrowser-version "13.0.16")

;; To find the last Firefox version, browse
;; https://archive.torproject.org/tor-package-archive/mullvadbrowser/<%mullvadbrowser-version>
;; There should be only one archive that starts with
;; "src-firefox-mullvad-browser-".
(define %mullvadbrowser-firefox-version "115.12.0esr-13.0-1-build1")

;; See tor-browser-build/projects/translation/config.
(define mullvadbrowser-translation-base
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "f28525699864f4e3d764c354130bd898ce5b20aa")))
    (file-name "translation-base-browser")
    (sha256
     (base32
      "1vf6nl7fdmlmg2gskf3w1xlsgcm0pxi54z2daz5nwr6q9gyi0lkf"))))

;; See tor-browser-build/projects/translation/config.
(define mullvadbrowser-translation-specific
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "bff8092bbe5ae93b2c162ade300d739b2cd9e92d")))
    (file-name "translation-mullvad-browser")
    (sha256
     (base32
      "0742ylhz80445a28ssp2hpshy0dvr12h2c1mcv5pjdipzcwhgil8"))))

(define mullvadbrowser-assets
  ;; This is a prebuilt Mullvad Browser from which we take the assets we need.
  (package
    (name "mullvadbrowser-assets")
    (version %mullvadbrowser-version)
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://archive.torproject.org/tor-package-archive/mullvadbrowser/"
         version "/mullvad-browser-linux-x86_64-" version ".tar.xz"))
       (sha256
        (base32
         "1bpchiz12zjyrzpgyk71naf1jdf3msjcjwggb1mziyawc6pyxj7v"))))
    (arguments
     (list
      #:install-plan
      ''(("Browser" "." #:include-regexp
          ("^\\./fonts/"
           "^\\./fontconfig/fonts.conf"
           ;; Mullvad Browser Extension
           "^\\./distribution/extensions/\\{d19a89b9-76c1-4a61-bcd4-49e8de916403\\}.xpi"
           )))))
    (build-system copy-build-system)
    (home-page "https://www.torproject.org")
    (synopsis "Mullvad Browser assets")
    (description "This package contains fonts and configuration files for
Mullvad Browser.")
    (license license:silofl1.1)))

(define mullvadbrowser-base
  (make-torbrowser #:moz-app-name "mullvadbrowser"
                   #:moz-app-remotingname "Mullvad Browser"
                   #:branding-directory "browser/branding/mb-release"
                   #:translation-base mullvadbrowser-translation-base
                   #:translation-specific mullvadbrowser-translation-specific
                   #:assets mullvadbrowser-assets
                   #:locales %mullvadbrowser-locales
                   #:build-date %mullvadbrowser-build-date
                   #:base-browser-version %mullvadbrowser-version))

(define-public mullvadbrowser
  (package
    (inherit mullvadbrowser-base)
    (name "mullvadbrowser")
    (version %mullvadbrowser-version)
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://archive.torproject.org/tor-package-archive/mullvadbrowser/"
         version "/src-firefox-mullvad-browser-"
         %mullvadbrowser-firefox-version ".tar.xz"))
       (sha256
        (base32
         "1xs4qwa3c6nfq6cj5q6asfrzki4brafg65g6hbn0fc9qqcmrhkv5"))))
    (arguments
     (substitute-keyword-arguments (package-arguments mullvadbrowser-base)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'ublock-private-allowed
              (lambda _
                (substitute* "toolkit/components/extensions/Extension.sys.mjs"
                  ;; The code that gives the correct permission only applies
                  ;; to distribution add-ons (see installDistributionAddon()
                  ;; in XPIProvider.jsm).
                  (("this.isNoScript")
                   (format #f "this.isNoScript || this.id === ~s"
                           "uBlock0@raymondhill.net")))))
            ;; See tor-browser-build/projects/firefox/build.
            (replace 'copy-torbrowser-locales
              (lambda _
                (for-each
                 (lambda (lang)
                   (system
                    (format #f "cp -Lr ~a/~a .mozbuild/l10n-central/"
                            #$mullvadbrowser-translation-specific lang)))
                 (map car #$%mullvadbrowser-locales))))
            (add-before 'build 'fix-profiles
              ;; Otherwise the profile would change every time the install
              ;; location changes, that is: at every package update.  These
              ;; values are already the default values for Icecat and Tor
              ;; Browser.
              (lambda _
                (substitute* "browser/moz.configure"
                  (("\"MOZ_DEDICATED_PROFILES\", True")
                   "\"MOZ_DEDICATED_PROFILES\", False")
                  (("\"MOZ_BLOCK_PROFILE_DOWNGRADE\", True")
                   "\"MOZ_BLOCK_PROFILE_DOWNGRADE\", False"))))
            (add-after 'deploy-fonts 'deploy-extension
              (lambda _
                (let ((lib (in-vicinity #$output "lib/mullvadbrowser")))
                  ;; Mullvad Browser Extension (FIXME: package it)
                  (copy-recursively
                   (in-vicinity #$mullvadbrowser-assets "distribution")
                   (in-vicinity lib "distribution")))))
            (delete 'deploy-tor-assets)
            (delete 'autoconfig-tor)))))
    (inputs
     (modify-inputs (package-inputs torbrowser)
       (delete go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs torbrowser)
       (append ublock-origin/icecat)))
    (home-page "https://mullvad.net/en/browser")
    (synopsis "Privacy-focused web browser")
    (description "Mullvad Browser is a privacy-focused web browser developed
in collaboration between Mullvad VPN and the Tor Project.  It’s produced to
minimize tracking and fingerprinting.")))
