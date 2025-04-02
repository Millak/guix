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
;;; Copyright © 2020, 2021, 2024 André Batista <nandre@riseup.net>
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

;; See browser/locales/l10n-changesets.json for the commit.
(define firefox-locales
  (let ((commit "fcd0300e8478d1ec4d1c097a073ddb8e1e0351e3")
        (revision "0"))
    (package
      (name "firefox-locales")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/mozilla-l10n/firefox-l10n")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1pzw65852ix6a6qb3wwhg5vrkz8337cs6lznk2vj0md5cvf2rrc4"))))
      (build-system copy-build-system)
      (home-page "https://github.com/mozilla-l10n/firefox-l10n")
      (synopsis "Firefox Locales")
      (description "This package contains localized messages for all
Firefox locales.")
      (license license:mpl2.0))))

;; We copy the official build id, which is defined at
;; tor-browser-build/rbm.conf (browser_release_date).
(define %torbrowser-build-date "20250331180000")

;; To find the last version, look at https://www.torproject.org/download/.
(define %torbrowser-version "14.0.9")

;; To find the last Firefox version, browse
;; https://archive.torproject.org/tor-package-archive/torbrowser/<%torbrowser-version>
;; There should be only one archive that starts with
;; "src-firefox-tor-browser-".
(define %torbrowser-firefox-version "128.9.0esr-14.0-2-build2")

;; See tor-browser-build/rbm.conf for the list.
(define %torbrowser-locales (list "ar" "ca" "cs" "da" "de" "el" "es-ES" "fa" "fi" "fr"
                                  "ga-IE" "he" "hu" "id" "is" "it" "ja" "ka" "ko" "lt"
                                  "mk" "ms" "my" "nb-NO" "nl" "pl" "pt-BR" "ro" "ru"
                                  "sq" "sv-SE" "th" "tr" "uk" "vi" "zh-CN" "zh-TW"))

;; See tor-browser-build/projects/translation/config.
(define torbrowser-translation-base
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "d687be19490caa48a46a3e2193bda95d57cbb96d")))
    (file-name "translation-base-browser")
    (sha256
     (base32
      "0hb4v0d898h7zxg9iwjvxjzh776wa65inysh4wdla51r0ib91w1b"))))

;; See tor-browser-build/projects/translation/config.
(define torbrowser-translation-specific
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "95b60f2679611d39b035f2e45555c2c3d64d991f")))
    (file-name "translation-tor-browser")
    (sha256
     (base32
      "1lvdy586v0p84lilvx1z0x5y6ng1yy6aw12lg3xphfh0kr1ygi16"))))

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
         "1s65fr8crhlmgx449686f0s5k28gjia9wdq6d5rhif3d3r696cx1"))))
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

;;; A LLD wrapper that can be used as a (near) drop-in replacement to GNU ld.
(define lld-as-ld-wrapper-16
  (make-lld-wrapper lld-16 #:lld-as-ld? #t))

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
         "1jq6jaiwr2jk7ayylnaha7rqk9g14ryybld817zhcqpldr0xmvyr"))))
    (build-system mozilla-build-system)
    (inputs
     (list lyrebird
           firefox-locales
           tor-client
           alsa-lib
           bash-minimal                 ;for wrap-program
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
           libwebp
           libxft
           libevent
           libxinerama
           libxscrnsaver
           libxcomposite
           libxt
           libffi
           ffmpeg-7
           libvpx
           icu4c
           pixman
           pulseaudio
           mesa
           pciutils
           mit-krb5
           hunspell
           libnotify
           nspr
           nss-rapid  ; requires v. 3.101, so nss won't cut it for now.
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
      rust-cbindgen-0.26
      lld-as-ld-wrapper-16  ; for cargo rustc
      llvm-16
      clang-16
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
         "--with-system-webp"
         "--with-system-zlib"
         "--with-system-libevent"
         ;; UNBUNDLE-ME! "--with-system-ogg"
         ;; UNBUNDLE-ME! "--with-system-vorbis"
         ;; UNBUNDLE-ME! "--with-system-theora" ; wants theora-1.2, not yet released
         "--with-system-libvpx"
         "--with-system-icu"
         "--with-system-nspr"
         "--with-system-nss"

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
                    (search-patch "torbrowser-compare-paths.patch"))
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
                          ;; to build netwerk/socket/neqo_glue.
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
                          "modules/zlib"
                          "ipc/chromium/src/third_party/libevent"
                          "media/libvpx"
                          ;; UNBUNDLE-ME! "media/libogg"
                          ;; UNBUNDLE-ME! "media/libvorbis"
                          ;; UNBUNDLE-ME! "media/libtheora" ; wants theora-1.2, not yet released
                          ;; UNBUNDLE-ME! "media/libtremor"
                          "media/libwebp"
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
              ;; Remove --frozen flag from cargo invocation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("args.append\\(\"--frozen\"\\)") "pass"))))
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
                      (string-join (list #$@locales)))
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
              (let ((l10ncentral ".mozbuild/l10n-central")
                    (ff-locales #$(this-package-input "firefox-locales")))
                (mkdir-p l10ncentral)
                (for-each
                 (lambda (lang)
                   (copy-recursively (string-append ff-locales "/" lang)
                                     (in-vicinity l10ncentral lang)))
                 (list #$@locales)))))
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
                           "~a/~a/toolkit/toolkit/global/"))
                     lang l10ncentral lang))
                   (system
                    (format
                     #f (string-join
                         '("mv"
                           "translation-base-browser/~a/*"
                           "~a/~a/browser/chrome/browser/"))
                     lang l10ncentral lang)))
                 (list #$@locales)))))
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
                           "~a/~a/toolkit/toolkit/global/"))
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
                 (list #$@locales)))))
          (replace 'configure
            (lambda _
              (invoke "./mach" "configure")))
          (add-before 'build 'fix-addons-placeholder
            (lambda _
              (substitute*
                  "toolkit/locales/en-US/toolkit/about/aboutAddons.ftl"
                (("addons.mozilla.org") "gnuzilla.gnu.org"))))
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
                  (("<dir prefix=\"cwd\">fonts</dir>")
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
(define %mullvadbrowser-locales (list "ar" "da" "de" "es-ES" "fa" "fi" "fr" "it"
                                      "ja" "ko" "my" "nb-NO" "nl" "pl" "pt-BR"
                                      "ru" "sv-SE" "th" "tr" "zh-CN" "zh-TW"))

;; We copy the official build id, which can be found there:
;; https://cdn.mullvad.net/browser/update_responses/update_1/release.
(define %mullvadbrowser-build-date "20250303093702")

;; To find the last version, look at
;; https://mullvad.net/en/download/browser/linux.
(define %mullvadbrowser-version "14.0.7")

;; To find the last Firefox version, browse
;; https://archive.torproject.org/tor-package-archive/mullvadbrowser/<%mullvadbrowser-version>
;; There should be only one archive that starts with
;; "src-firefox-mullvad-browser-".
(define %mullvadbrowser-firefox-version "128.8.0esr-14.0-1-build2")

;; See tor-browser-build/projects/translation/config.
(define mullvadbrowser-translation-base
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "21fed48fc58df9e6c4d9f67b048fcae831df50c9")))
    (file-name "translation-base-browser")
    (sha256
     (base32
      "1gs2b9bak7rglpbswkm47jwj3yd76361xblvyxjfsbji9igjj7za"))))

;; See tor-browser-build/projects/translation/config.
(define mullvadbrowser-translation-specific
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://gitlab.torproject.org/tpo/translation.git")
          (commit "d279ce2add9a5bb3fc71f24b55679e3e0706f0eb")))
    (file-name "translation-mullvad-browser")
    (sha256
     (base32
      "1bnclgln8ibxslnqs2lmmlww7q0qgar7lkhc69dkr1w1cpxjcrr9"))))

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
         "0z9wvhprn8w6lfi3vsd9qk8fdl6gxq9z07awx7r5vrk1813mj0ry"))))
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
         "0w6r13wli2z4hpww4gb8hy99grvmcffpp4nkn1jljcxk316jcrcy"))))
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
                 (list #$@%mullvadbrowser-locales))))
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
       (delete lyrebird)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs torbrowser)
       (append ublock-origin/icecat)))
    (home-page "https://mullvad.net/en/browser")
    (synopsis "Privacy-focused web browser")
    (description "Mullvad Browser is a privacy-focused web browser developed
in collaboration between Mullvad VPN and the Tor Project.  It’s produced to
minimize tracking and fingerprinting.")))
