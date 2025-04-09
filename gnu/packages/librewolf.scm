;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015, 2024 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017, 2018, 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017, 2018 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018 ng0 <gillmann@infotropique.org>
;;; Copyright © 2017, 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Ivan Petkov <ivanppetkov@gmail.com>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2019, 2020 Adrian Malacoda <malacoda@monarch-pass.net>
;;; Copyright © 2020-2023 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2021 pineapples <guixuser6392@protonmail.com>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021, 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2023 Tomas Volf <wolf@wolfsden.cz>
;;; Copyright © 2023, 2024, 2025 Ian Eure <ian@retrospec.tv>
;;; Copyright © 2024 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2024 Ashvith Shetty <ashvithshetty10@gmail.com>
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

(define-module (gnu packages librewolf)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system trivial)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))

  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crates-io)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages hunspell)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages jemalloc)
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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg))

(define (firefox-source-origin version hash)
  (origin
    (method url-fetch)
    (uri (string-append
          "https://ftp.mozilla.org/pub/firefox/releases/"
          version "/source/" "firefox-" version
          ".source.tar.xz"))
    (sha256 (base32 hash))))

(define (librewolf-source-origin version hash)
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://codeberg.org/librewolf/source.git")
          (commit version)
          (recursive? #t)))
    (file-name (git-file-name "librewolf-source" version))
    (patches (search-patches "librewolf-neuter-locale-download.patch"))
    (sha256 (base32 hash))))

(define computed-origin-method (@@ (guix packages) computed-origin-method))

(define firefox-l10n
  (let ((commit "24e2602d2221646fbbe92e908bed0d605acd2e8a"))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/mozilla-l10n/firefox-l10n.git")
            (commit commit)))
      (file-name (git-file-name "firefox-l10n" commit))
      (sha256 (base32 "1xnldwgldls07m5hmm9wnln6g2vcar5w4k4918qkmakldaw6ang0")))))

(define* (make-librewolf-source #:key version firefox-hash librewolf-hash l10n)
  (let* ((ff-src (firefox-source-origin
                  (car (string-split version #\-))
                  firefox-hash))
         (lw-src (librewolf-source-origin
                  version
                  librewolf-hash)))

    (origin
      (method computed-origin-method)
      (file-name (string-append "librewolf-" version ".source.tar.gz"))
      (sha256 #f)
      (uri
       (delay
         (with-imported-modules '((guix build utils))
           #~(begin
               (use-modules (guix build utils))
               (set-path-environment-variable
                "PATH" '("bin")
                (list #+python
                      #+(canonical-package bash)
                      #+(canonical-package gnu-make)
                      #+(canonical-package coreutils)
                      #+(canonical-package findutils)
                      #+(canonical-package patch)
                      #+(canonical-package xz)
                      #+(canonical-package sed)
                      #+(canonical-package grep)
                      #+(canonical-package pigz)
                      #+(canonical-package tar)))
               (set-path-environment-variable
                "PYTHONPATH"
                (list #+(format #f "lib/python~a/site-packages"
                                (version-major+minor
                                 (package-version python))))
                '#+(cons python-jsonschema
                         (map second
                              (package-transitive-propagated-inputs
                               python-jsonschema))))

               ;; Copy LibreWolf source into the build directory and make
               ;; everything writable.
               (copy-recursively #+lw-src ".")
               (for-each make-file-writable (find-files "."))

               ;; Patch Makefile to use the upstream source instead of
               ;; downloading.
               (substitute* '("Makefile")
                 (("^ff_source_tarball:=.*")
                  (string-append "ff_source_tarball:=" #+ff-src)))

               ;; Stage locales.
               (begin
                 (substitute* "scripts/librewolf-patches.py"
                   (("l10n_dir = Path(\"..\", \"l10n\")")
                    (string-append
                     "l10n_dir = \"" #+l10n "\""))))

               ;; Run the build script
               (invoke "make" "all")
               (copy-file (string-append "librewolf-" #$version
                                         ".source.tar.gz")
                          #$output)))))
      (patches
       (search-patches
        "torbrowser-compare-paths.patch"
        "librewolf-use-system-wide-dir.patch")))))

;;; Define the versions of rust needed to build firefox, trying to match
;;; upstream.  See table at [0], `Uses' column for the specific version.
;;; Using `rust' will likely lead to a newer version then listed in the table,
;;; but since in Guix only the latest packaged Rust is officially supported,
;;; it is a tradeoff worth making.
;;; 0: https://firefox-source-docs.mozilla.org/writing-rust-code/update-policy.html
;; 136.0.1 wants 1.84, but it's not available in Guix yet.
(define rust-librewolf rust-1.82)

;; Update this id with every update to its release date.
;; It's used for cache validation and therefore can lead to strange bugs.
;; ex: date '+%Y%m%d%H%M%S'
(define %librewolf-build-id "20250405165830")

(define-public librewolf
  (package
    (name "librewolf")
    (version "137.0.1-1")
    (source
     (make-librewolf-source
      #:version version
      #:firefox-hash "1r0b5vfhqkw5vgf7bb0ylcw4vlg9mpfj96n8whfppj8r5rhah788"
      #:librewolf-hash "0yry4k44wifi9h25h49krm05jbrs6bg9pa4vszv8af8dv5qm2bz0"
      #:l10n firefox-l10n))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(let ((clang #$(this-package-native-input "clang")))
          ;; Configuration.
          `("--enable-jemalloc"
            "--enable-system-pixman"
            "--with-system-ffi"
            "--with-system-icu"
            "--with-system-jpeg"
            "--with-system-libevent"
            "--with-system-libvpx"
            "--with-system-nspr"
            "--with-system-nss"
            "--with-system-png"
            "--with-system-webp"
            "--with-system-zlib"
            "--without-wasm-sandboxed-libraries"

            ,(string-append "--with-clang-path=" clang
                            "/bin/clang")
            ,(string-append "--with-libclang-path=" clang
                            "/lib")

            ;; Distribution
            "--with-distribution-id=org.guix"

            ;; Features
            "--enable-pulseaudio"

            ;; switch only available on x86, whereas EME is not supported on
            ;; other targets
            ,@(if #$(target-x86?) '("--disable-eme") '())

            ;; Build details
            "--disable-elf-hack"
            ;; Don't attempt to download toolchains.  Upstream enables this,
            ;; adding the flag ensures we don't use network in the build
            ;; environment.
            "--disable-bootstrap"
            "--enable-strip"))
      #:imported-modules %cargo-utils-modules
      #:modules `((ice-9 regex)
                  (ice-9 string-fun)
                  (ice-9 ftw)
                  (srfi srfi-1)
                  (srfi srfi-26)
                  (rnrs bytevectors)
                  (rnrs io ports)
                  (guix elf)
                  (guix build gremlin)
                  ,@%default-gnu-imported-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-preferences
            (lambda* (#:key inputs #:allow-other-keys)
              (call-with-port (open-file "browser/app/profile/firefox.js" "a")
                (lambda (port)
                  (define (write-setting key value)
                    (format port "~%pref(\"~a\", ~a);~%" key value)
                    (format #t
                            "fix-preferences: setting value of ~a to ~a~%" key
                            value))

                  ;; We should allow the sandbox to read the store directory,
                  ;; because the sandbox has access to /usr on FHS distros.
                  (write-setting
                   "security.sandbox.content.read_path_whitelist"
                   (string-append "\""
                                  (%store-directory) "/\""))

                  ;; XDG settings should be managed by Guix.
                  (write-setting "browser.shell.checkDefaultBrowser"
                                 "false")))))
          (add-after 'unpack 'fix-ffmpeg-runtime-linker
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((ffmpeg (assoc-ref inputs "ffmpeg"))
                     (libavcodec (string-append ffmpeg
                                                "/lib/libavcodec.so")))
                ;; Arrange to load libavcodec.so by its absolute file name.
                (substitute* 
                    "dom/media/platforms/ffmpeg/FFmpegRuntimeLinker.cpp"
                  (("libavcodec\\.so")
                   libavcodec)))))
          (add-after 'unpack 'neuter-genai
            (lambda* _
              ;; Lock the preferences so they can't be enabled.
              (substitute* "lw/librewolf.cfg"
                (("defaultPref\\(\"browser\\.ml\\.")
                 "lockPref(\"browser.ml."))))
          (add-after 'unpack 'expand-extension-scope
            (lambda* _
                ;; Required for Guix packaged extensions
                ;; SCOPE_PROFILE=1, SCOPE_APPLICATION=4, SCOPE_SYSTEM=8
                ;; Default is 5.
                (substitute* "lw/librewolf.cfg"
                  (("defaultPref\\(\"extensions.enabledScopes\", 5\\)")
                   "defaultPref(\"extensions.enabledScopes\", 13)"))))
          (add-after 'unpack 'use-mozzarella
            (lambda _
              (substitute*
                  "toolkit/locales/en-US/toolkit/about/aboutAddons.ftl"
                (("addons.mozilla.org")
                 "gnuzilla.gnu.org"))
              (call-with-port (open-file "lw/librewolf.cfg" "a")
                (lambda (port)
                  ;; Add-ons panel (see settings.js in Icecat source).
                  (for-each
                   (lambda (pref)
                     (format port
                             "defaultPref(~s, ~s);~%"
                             (car pref)
                             (cdr pref)))
                   `(("extensions.getAddons.search.browseURL"
                      ,(string-append
                        "https://gnuzilla.gnu.org/mozzarella/"
                        "search.php?q=%TERMS%"))
                     ("extensions.getAddons.get.url" .
                      "https://gnuzilla.gnu.org/mozzarella")
                     ("extensions.getAddons.link.url" .
                      "https://gnuzilla.gnu.org/mozzarella")
                     ("extensions.getAddons.discovery.api_url" .
                      "https://gnuzilla.gnu.org/mozzarella")
                     ("extensions.getAddons.langpacks.url" .
                      "https://gnuzilla.gnu.org/mozzarella")
                     ("lightweightThemes.getMoreURL" .
                      "https://gnuzilla.gnu.org/mozzarella")))))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules (guix build cargo-utils))
              (let ((null-hash
                     ;; This is the SHA256 output of an empty string.
                     (string-append
                      "e3b0c44298fc1c149afbf4c8996fb924"
                      "27ae41e4649b934ca495991b7852b855")))
                (for-each
                 (lambda (file)
                   (format #t
                           "patch-cargo-checksums: patching checksums in ~a~%"
                           file)
                   (substitute* file
                     (("(checksum = )\".*\"" all name)
                      (string-append name "\"" null-hash
                                     "\""))))
                 (find-files "." "Cargo\\.lock$"))
                (for-each generate-all-checksums
                          '("build"
                            "dom/media"
                            "dom/webauthn"
                            "gfx"
                            "intl"
                            "js"
                            "media"
                            "modules"
                            "mozglue/static/rust"
                            "netwerk"
                            "remote"
                            "security/manager/ssl"
                            "servo"
                            "storage"
                            "third_party/rust"
                            "toolkit"
                            "xpcom/rust"
                            "services")))))
          (add-after 'patch-cargo-checksums 'remove-cargo-frozen-flag
            (lambda _
              ;; Remove --frozen flag from cargo invocation, otherwise it'll
              ;; complain that it's not able to change Cargo.lock.
              ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1726373
              (substitute* "build/RunCbindgen.py"
                (("args.append\\(\"--frozen\"\\)") "pass"))))
          (delete 'bootstrap)
          (add-before 'configure 'patch-SpeechDispatcherService.cpp
            (lambda _
              (let* ((lib "libspeechd.so.2")
                     (file (string-append
                            "dom/media/webspeech/synth/"
                            "speechd/SpeechDispatcherService.cpp"))
                     (old-content (call-with-input-file file
                                    get-string-all)))
                (substitute
                 file
                 `((,(format #f "~s" lib) unquote
                    (lambda (line _)
                      (string-replace-substring
                       line lib
                       (string-append #$speech-dispatcher
                                      "/lib/" lib))))))
                (if (string=? old-content
                              (call-with-input-file file
                                get-string-all))
                    (error
                     "substitute did nothing, phase requires an update")))))
          (add-before 'configure 'set-build-id
            ;; Build will write the timestamp to output, which is harmful
            ;; for reproducibility, so change it to a fixed date.  Use a
            ;; separate phase for easier modification with inherit.
            (lambda _
              (setenv "MOZ_BUILD_DATE"
                      #$%librewolf-build-id)))
          ;; https://bugzilla.mozilla.org/show_bug.cgi?id=1927380
          (add-before 'configure 'patch-icu-lookup
            (lambda _
              (let* ((file "js/moz.configure")
                     (old-content (call-with-input-file file get-string-all)))
                (substitute* file
                  (("icu-i18n >= 76.1" all)
                   (string-append all ", icu-uc >= 76.1")))
                (if (string=? old-content
                              (pk (call-with-input-file file get-string-all)))
                    (error 
                     "substitute did nothing, phase requires an update")))))
          (replace 'configure
            (lambda* (#:key inputs outputs configure-flags
                      #:allow-other-keys)
              (setenv "AUTOCONF"
                      (string-append (assoc-ref inputs "autoconf")
                                     "/bin/autoconf"))
              (setenv "SHELL"
                      (which "bash"))
              (setenv "CONFIG_SHELL"
                      (which "bash"))
              (setenv "MACH_BUILD_PYTHON_NATIVE_PACKAGE_SOURCE"
                      "system")
              (setenv "LANG" "en_US.utf8")
              ;; This should use the host info probably (does it
              ;; build on non-x86_64 though?)
              (setenv "GUIX_PYTHONPATH"
                      (string-append
                       (getcwd)
                       "/obj-x86_64-pc-linux-gnu/_virtualenvs/build"))

              ;; Use Clang, Clang is 2x faster than GCC
              (setenv "AR" "llvm-ar")
              (setenv "NM" "llvm-nm")
              (setenv "CC" "clang")
              (setenv "CXX" "clang++")
              (setenv "MOZ_NOSPAM" "1")
              (setenv "MOZ_APP_REMOTINGNAME" "LibreWolf")
              (setenv "MOZ_APP_NAME" "librewolf")

              (setenv "MOZBUILD_STATE_PATH"
                      (getcwd))

              (let* ((mozconfig (string-append (getcwd) "/mozconfig"))
                     (out (assoc-ref outputs "out"))
                     (flags (cons (string-append "--prefix=" out)
                                  configure-flags)))
                (format #t "build directory: ~s~%"
                        (getcwd))
                (format #t "configure flags: ~s~%" flags)

                (call-with-port (open-file mozconfig "a")
                  (lambda (port)
                    (format port "~%## Guix-specific options ##~%~%")
                    (for-each
                     (lambda (flag) (format port "ac_add_options ~a\n" flag))
                     flags)
                    (display "\n" port)))
                (setenv "MOZCONFIG" mozconfig))
              (invoke "./mach" "configure")))
          (replace 'build
            (lambda* (#:key (make-flags '())
                      (parallel-build? #t) #:allow-other-keys)
              (apply invoke "./mach" "build"
                     ;; mach will use parallel build if possible by default
                     `(,@(if parallel-build?
                             `(,(string-append
                                 "-j" (number->string (parallel-job-count))))
                             '("-j1"))
                       ,@make-flags))))
          (add-after 'build 'neutralise-store-references
            (lambda _
              ;; Mangle the store references to compilers &
              ;; other build tools in about:buildconfig,
              ;; reducing the package's closure by 1 GiB on
              ;; x86-64.
              (let* ((build-dir (car (scandir "."
                                              (cut string-prefix?
                                                   "obj-" <>))))
                     (file (string-append build-dir
                                          "/dist/bin/chrome/toolkit/"
                                          "content/global/buildconfig.html")))
                (substitute* file
                  (((format #f "(~a/)([0-9a-df-np-sv-z]{32})"
                            (regexp-quote (%store-directory)))
                    _ store hash)
                   (string-append store
                                  (string-take hash 8)
                                  "<!-- Guix: not a runtime dependency -->"
                                  (string-drop hash 8)))))))
          (replace 'install
            (lambda _
              (invoke "./mach" "install")))
          (add-after 'install 'remove-duplicate-bin
            (lambda* (#:key outputs #:allow-other-keys)
              (delete-file (string-append #$output
                                          "/lib/librewolf/librewolf-bin"))))
          (add-after 'install 'wrap-glxtest
            ;; glxtest uses dlopen() to load mesa and pci
            ;; libs, wrap it to set LD_LIBRARY_PATH.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib"))
                     (libs (map
                            (lambda (lib-name)
                              (string-append (assoc-ref inputs
                                                        lib-name)
                                             "/lib"))
                            '("mesa" "pciutils"))))
                (wrap-program (car (find-files lib "^glxtest$"))
                  `("LD_LIBRARY_PATH" prefix ,libs)))))
          (add-after 'install 'wrap-program
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; The following two functions are from Guix's icecat package in
              ;; (gnu packages gnuzilla).  See commit
              ;; b7a0935420ee630a29b7e5ac73a32ba1eb24f00b.
              (define (runpath-of lib)
                (call-with-input-file lib
                  (compose elf-dynamic-info-runpath elf-dynamic-info
                           parse-elf get-bytevector-all)))
              (define (runpaths-of-input label)
                (let* ((dir (string-append (assoc-ref inputs label)
                                           "/lib"))
                       (libs (find-files dir "\\.so$")))
                  (append-map runpath-of libs)))
              (let* ((out (assoc-ref outputs "out"))
                     (lib (string-append out "/lib"))
                     (libs (map
                            (lambda (lib-name)
                              (string-append (assoc-ref inputs
                                                        lib-name)
                                             "/lib"))
                            '("eudev" ; For U2F and WebAuthn
                              "libnotify"
                              "libpng-apng"
                              "libva"
                              "mesa"
                              "pipewire" ; For sharing on Wayland
                              "pulseaudio")))

                     ;; VA-API is run in the RDD (Remote Data Decoder) sandbox
                     ;; and must be explicitly given access to files it needs.
                     ;; Rather than adding the whole store (as Nix had
                     ;; upstream do, see
                     ;; <https://github.com/NixOS/nixpkgs/pull/165964> and
                     ;; linked upstream patches), we can just follow the
                     ;; runpaths of the needed libraries to add everything to
                     ;; LD_LIBRARY_PATH.  These will then be accessible in the
                     ;; RDD sandbox.
                     (rdd-whitelist
                      (map (cut string-append <> "/")
                           (delete-duplicates
                            (append-map runpaths-of-input
                                        '("mesa"
                                          "ffmpeg")))))
                     (gtk-share (string-append (assoc-ref inputs
                                                          "gtk+")
                                               "/share")))
                (wrap-program (car (find-files lib "^librewolf$"))
                  `("LD_LIBRARY_PATH" prefix
                    (,@libs ,@rdd-whitelist))
                  `("XDG_DATA_DIRS" prefix
                    (,gtk-share))
                  `("MOZ_LEGACY_PROFILES" =
                    ("1"))
                  `("MOZ_ALLOW_DOWNGRADE" =
                    ("1"))))))
          (add-after 'wrap-program 'install-desktop-entry
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((desktop-file
                      "toolkit/mozapps/installer/linux/rpm/mozilla.desktop")
                     (applications (string-append #$output
                                                  "/share/applications")))
                (substitute* desktop-file
                  (("^Exec=@MOZ_APP_NAME@")
                   (string-append "Exec="
                                  #$output "/bin/librewolf %u"))
                  (("@MOZ_APP_DISPLAYNAME@")
                   "LibreWolf")
                  (("@MOZ_APP_REMOTINGNAME@")
                   "LibreWolf")
                  (("^Icon=@MOZ_APP_NAME@")
                   (string-append "Icon="
                                  #$output
                                  "/share/icons/hicolor/128x128/apps/librewolf.png")))

                (copy-file desktop-file "librewolf.desktop")
                (install-file "librewolf.desktop" applications))))
          (add-after 'install-desktop-entry 'install-icons
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((icon-source-dir (string-append #$output
                                                    "/lib/librewolf/browser/"
                                                    "chrome/icons/default")))
                (for-each (lambda (size)
                            (let ((dest (string-append #$output
                                                       "/share/icons/hicolor/"
                                                       size
                                                       "x"
                                                       size
                                                       "/apps")))
                              (mkdir-p dest)
                              (symlink (string-append icon-source-dir
                                                      "/default" size ".png")
                                       (string-append dest
                                                      "/librewolf.png"))))
                          '("16" "32" "48" "64" "128"))))))

      ;; Test will significantly increase build time but with little rewards.
      #:tests? #f

      ;; WARNING: Parallel build will consume lots of memory!
      ;; If you have encountered OOM issue in build phase, try disable it.
      #:parallel-build? #t

      ;; Some dynamic lib was determined at runtime, so rpath check may fail.
      #:validate-runpath? #f))
    (inputs (list bash-minimal
                  bzip2
                  cairo
                  cups
                  dbus-glib
                  freetype
                  ffmpeg
                  gdk-pixbuf
                  glib
                  gtk+
                  gtk+-2
                  hunspell
                  icu4c-76
                  jemalloc
                  libcanberra
                  libevent
                  libffi
                  libgnome
                  libjpeg-turbo
                  libnotify
                  libpng-apng-for-librewolf
                  libva
                  libvpx
                  libwebp
                  libxcomposite
                  libxft
                  libxinerama
                  libxscrnsaver
                  libxt
                  mesa
                  mit-krb5
                  nspr
                  nss-rapid
                  pango
                  pciutils
                  pipewire
                  pixman
                  pulseaudio
                  speech-dispatcher
                  sqlite
                  startup-notification
                  eudev
                  unzip
                  zip
                  zlib))
    (native-inputs (list alsa-lib
                         autoconf-2.13
                         `(,rust-librewolf "cargo")
                         clang-18
                         llvm-18
                         m4
                         nasm
                         node-lts
                         perl
                         pkg-config
                         python
                         rust-librewolf
                         rust-cbindgen-0.26
                         which
                         yasm))
    (native-search-paths
     (list (search-path-specification
            (variable "ICECAT_SYSTEM_DIR")
            (separator #f)              ;single entry
            (files '("lib/icecat")))))
    (home-page "https://librewolf.net/")
    (synopsis
     "Custom version of Firefox, focused on privacy, security and freedom")
    (description
     "LibreWolf is designed to increase protection against tracking and
fingerprinting techniques, while also including a few security improvements.
This is achieved through our privacy and security oriented settings and
patches.  LibreWolf also aims to remove all the telemetry, data collection and
annoyances, as well as disabling anti-freedom features like DRM.")
    (license license:mpl2.0)))
