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
   ("14wnjv13alaj04pd8i8ysillbr3ic2jqa867rbj5ncz8h4hxxfxc" "4c7e24ef78bd" "ar")
   ("0mcc15n3p7yk4zdbr3na2fm7wq2184mbcrkk3cvppkl6p4k8654d" "24d50653ab5c" "ca")
   ("0ray22hdb3nrv2yi5z98cvbmpk9kpsv96a8wzad5dr4sxy44ii0d" "0d96b6b04bfb" "cs")
   ("0is7qbykv2pj0z9ll9r35vwjp0x29vmfr10yjl3s0amfaqzjqpqc" "0a0b774407cc" "da")
   ("0yq7m4v7d7ayg90m66j73mflrnp709qw9n7skhpsl9h1wbhrd7q7" "633986260777" "de")
   ("19g2ha32syq6rjcyl4ypmy7sc9w7xkvrpkic5lfc2yja6ll9116p" "e2f2d1541e38" "el")
   ("018qi9zn24kzfcidsj9lbqfg5n97r295yr8fs953nyfdbim9jsfv" "accf5e4506c0" "es-ES")
   ("11prhmh2cp95dpv6z0k479mb11zbfm541bvigs3gnkh3nazjvc8q" "37aa71d77cb6" "fa")
   ("1lv9l98q88ixb0ph970yzphahgzbl97x0w069bkxa54kblkv1ch1" "dc40a4fd5d0e" "fi")
   ("0wx4k7mwhvpv5w0wa4y5pca2q3jac62jv804nxqnfwh1bvi90wv0" "415c1f0e84bd" "fr")
   ("17j68a6rbaphfcq38mgz6s1076fyy92fk0ldw8igql6gd85qjlaa" "d271f275cf48" "ga-IE")
   ("0b7qdayljb4ryyqgalvi626lzg238gyn03m3a2f7afs9zi6px526" "46f8d7c031a6" "he")
   ("14xbrzvc09fcp7qzllb65nis27hkg9pg5615y29xzwiz4g090my1" "086ac0260d6b" "hu")
   ("0q5s4iz02xgmbw6nnpg6xg4pwz7n55nvxb9mj8vqdakq3faybbd5" "f03a6b3069a5" "id")
   ("1lwklx3nkm56420xc3kbg892jm2b6202sjw33nvv766sm9hbvcap" "5c4b61165e1d" "is")
   ("1n7l5idw9399n8ih1r1d6m8vzpzhwmnxmr9i7jvygkdc8d6adp1k" "07d5e1ff5f9b" "it")
   ("1w6nw9cd92p1ndy82wwlq9xizyq3i8rq0nj7118gbxbx368mk2kj" "e6f9db9ce3e6" "ja")
   ("1js99gbyc1dj33xc425wb08s1aw3bfznaacrqhw3l42yw1g1ghy4" "a15eb9feea2c" "ka")
   ("116a8s0k2yvijy7qf0xpqm5w66gdzs32jhc06364sdar5v34lyhh" "805b85981696" "ko")
   ("1yrjrhmmd0b810kxryja1j1md3rr2zpn1j9cbg05dgp5s8i89psk" "943a26276832" "lt")
   ("08zccz7gflzpr20y0hvhmdsiz6ncags39kh83cay5ivchyib5qbi" "fbef80de5499" "mk")
   ("100k4ibpwys9i4ghi5xvmgwr9api67ngav2hvb613rj6hdfd57f7" "20ec0915ec35" "ms")
   ("0kk3cjlpghbi7j3ndb2s0c7g838fzd2mpzg01bp0cra8lzd0n2ac" "4ab6f0d05aa6" "my")
   ("1i3r2ici95mazw07m2mrf192fc6bfa3x6j3c2pcc1zg7z9srihgh" "561b0cd86ec1" "nb-NO")
   ("1c0m8jhn52h1dif5bswrdwrlzppgga01y61wlii4aaaw15imd6yd" "2a55df0cc389" "nl")
   ("1gssvg306b80drp7kvc35kvcxwldb5sga0bapaxhv362irq1nya8" "a64a7dab01c4" "pl")
   ("1dzh13x85a7src8szbrq5pjmrbak4isln9xdwjk7a1yq4g9h7jgs" "33bf2a9f4c49" "pt-BR")
   ("0jx9y7fv44wxqapmcgr924wgb1l5cm95bgpmnhnjchp1zpmyfdl5" "a367feeadd33" "ro")
   ("09x2jirf04kgc118a70z0xrb3msbm7vr4f41ig4xrwf2s5b816r3" "528b76d6aaca" "ru")
   ("02y898f0ncjwka474r9lw361b0kywx1w56hj09i7im4j5jrsjnh1" "fa28d9d79cd3" "sq")
   ("1cyimbd42aaq2amyhdbbx26jwsns77lsfl8g9a70bsjlpwzwzryg" "cc8e8962e59c" "sv-SE")
   ("03mqrvcal7i172gf9239q9fnynfp5kg9b3r1w8gr9iz7rkr22gw5" "d361502c559e" "th")
   ("12srgqkqwaidcwbz0y7zr59165f7aq5k5s3b81ql7ixdbwia91pm" "f6173aca4762" "tr")
   ("1d91gfx5p6wyb455syw0b57wxl1sd4b4kcdvfk92pb050rqaqfgv" "c5ad4d4f70eb" "uk")
   ("1dj8q2jw60a184f018jyldl51rfmvz1cndz3kbw0cc5l5sli7hwr" "0e75c226763d" "vi")
   ("1dl2dpif4wwrlpx7zkz5qf8kk4vhxyf63016xcfpbhxizqqwc1ki" "df2d025ed631" "zh-CN")
   ("1c63ngff9lsc1x3pi6lnkyxw19gdc65yc67p7alzvrka3cv292ia" "11f8d68148a4" "zh-TW")))

;; We copy the official build id, which can be found there:
;; https://aus1.torproject.org/torbrowser/update_3/release/.
(define %torbrowser-build-date "20240115174022")

;; To find the last version, look at https://www.torproject.org/download/.
(define %torbrowser-version "13.0.9")

;; To find the last Firefox version, browse
;; https://archive.torproject.org/tor-package-archive/torbrowser/<%torbrowser-version>
;; There should be only one archive that starts with
;; "src-firefox-tor-browser-".
(define %torbrowser-firefox-version "115.7.0esr-13.0-1-build1")

;; See tor-browser-build/projects/translation/config.
;; If Tor Browser and Mullvad Browser updates are not synchronized, maybe this
;; will have to be duplicated.
(define translation-base-browser
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "cbd9b6c415ec2edb99237ef67ccd4f033a7b9c2a")))
    (file-name "translation-base-browser")
    (sha256
     (base32
      "103dj1zzc68gxzjxwcpc4sbc6qca4zg8kkhdivzpq37ma07sp9sf"))))

;; See tor-browser-build/projects/translation/config.
(define translation-tor-browser
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "767ab5111f065b82151275775af5ecf7a529ef48")))
    (file-name "translation-tor-browser")
    (sha256
     (base32
      "034s0ivbama497xq0904q8p6d7n2f2aa2vn2jcs9g4bvmhgwicw4"))))

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
         "0j143r24xzmq38nd5z1xqsa9zp35lws9rvlj6hb9xn3dnl67gh59"))))
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
         "0h05js9j1drzw5q98nlphsmvlp1k2a71z5jd06xk6pz29w6322pw"))))
    (build-system mozilla-build-system)
    (inputs
     (list go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-lyrebird
           tor-client
           alsa-lib
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
           ;; Support for FFmpeg 6 was only added in version 112 (see:
           ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1819374).
           ffmpeg-5
           libvpx
           (force (@@ (gnu packages gnuzilla) icu4c-73-promise))
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
      rust-cbindgen-0.24
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
                  ,@%gnu-build-system-modules)
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
                (copy-recursively #$translation-base-browser
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
                (copy-recursively #$translation-tor-browser
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
                            "https://gnuzilla.gnu.org/mozzarella"))))))
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
project.  It modifies Firefox in order to avoid many know application level
attacks on the privacy of Tor users.")
    (license license:mpl2.0)))       ;And others, see
                                     ;toolkit/content/license.html

(define-public torbrowser
  (make-torbrowser #:moz-app-name "torbrowser"
                   #:moz-app-remotingname "Tor Browser"
                   #:branding-directory "browser/branding/tb-release"
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
   ("14wnjv13alaj04pd8i8ysillbr3ic2jqa867rbj5ncz8h4hxxfxc" "4c7e24ef78bd" "ar")
   ("0is7qbykv2pj0z9ll9r35vwjp0x29vmfr10yjl3s0amfaqzjqpqc" "0a0b774407cc" "da")
   ("0yq7m4v7d7ayg90m66j73mflrnp709qw9n7skhpsl9h1wbhrd7q7" "633986260777" "de")
   ("018qi9zn24kzfcidsj9lbqfg5n97r295yr8fs953nyfdbim9jsfv" "accf5e4506c0" "es-ES")
   ("11prhmh2cp95dpv6z0k479mb11zbfm541bvigs3gnkh3nazjvc8q" "37aa71d77cb6" "fa")
   ("1lv9l98q88ixb0ph970yzphahgzbl97x0w069bkxa54kblkv1ch1" "dc40a4fd5d0e" "fi")
   ("0wx4k7mwhvpv5w0wa4y5pca2q3jac62jv804nxqnfwh1bvi90wv0" "415c1f0e84bd" "fr")
   ("1n7l5idw9399n8ih1r1d6m8vzpzhwmnxmr9i7jvygkdc8d6adp1k" "07d5e1ff5f9b" "it")
   ("1w6nw9cd92p1ndy82wwlq9xizyq3i8rq0nj7118gbxbx368mk2kj" "e6f9db9ce3e6" "ja")
   ("116a8s0k2yvijy7qf0xpqm5w66gdzs32jhc06364sdar5v34lyhh" "805b85981696" "ko")
   ("0kk3cjlpghbi7j3ndb2s0c7g838fzd2mpzg01bp0cra8lzd0n2ac" "4ab6f0d05aa6" "my")
   ("1i3r2ici95mazw07m2mrf192fc6bfa3x6j3c2pcc1zg7z9srihgh" "561b0cd86ec1" "nb-NO")
   ("1c0m8jhn52h1dif5bswrdwrlzppgga01y61wlii4aaaw15imd6yd" "2a55df0cc389" "nl")
   ("1gssvg306b80drp7kvc35kvcxwldb5sga0bapaxhv362irq1nya8" "a64a7dab01c4" "pl")
   ("1dzh13x85a7src8szbrq5pjmrbak4isln9xdwjk7a1yq4g9h7jgs" "33bf2a9f4c49" "pt-BR")
   ("09x2jirf04kgc118a70z0xrb3msbm7vr4f41ig4xrwf2s5b816r3" "528b76d6aaca" "ru")
   ("1cyimbd42aaq2amyhdbbx26jwsns77lsfl8g9a70bsjlpwzwzryg" "cc8e8962e59c" "sv-SE")
   ("03mqrvcal7i172gf9239q9fnynfp5kg9b3r1w8gr9iz7rkr22gw5" "d361502c559e" "th")
   ("12srgqkqwaidcwbz0y7zr59165f7aq5k5s3b81ql7ixdbwia91pm" "f6173aca4762" "tr")
   ("1dl2dpif4wwrlpx7zkz5qf8kk4vhxyf63016xcfpbhxizqqwc1ki" "df2d025ed631" "zh-CN")
   ("1c63ngff9lsc1x3pi6lnkyxw19gdc65yc67p7alzvrka3cv292ia" "11f8d68148a4" "zh-TW")))

;; We copy the official build id, which can be found there:
;; https://cdn.mullvad.net/browser/update_responses/update_1/release.
(define %mullvadbrowser-build-date "20240115174108")

;; To find the last version, look at
;; https://mullvad.net/en/download/browser/linux.
(define %mullvadbrowser-version "13.0.9")

;; To find the last Firefox version, browse
;; https://archive.torproject.org/tor-package-archive/mullvadbrowser/<%mullvadbrowser-version>
;; There should be only one archive that starts with
;; "src-firefox-mullvad-browser-".
(define %mullvadbrowser-firefox-version "115.7.0esr-13.0-1-build1")

;; See tor-browser-build/projects/translation/config.
(define translation-mullvad-browser
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "57de1569da0e2c48fd999a13e555f6b522041993")))
    (file-name "translation-mullvad-browser")
    (sha256
     (base32
      "1q3979ac92c5mib573hx9w06x3hrfw7r52wzmj9r75sz2hhsmrq3"))))

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
         "1f930j3c1xq88cqlqmnj0m00k0hd63cmgnxd788sp9hz56al22sc"))))
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
         "16chkc07pqr4ypmmgy4z2grvlpvbyr161gpzy72w35dgzzff46f9"))))
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
                            #$translation-mullvad-browser lang)))
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
