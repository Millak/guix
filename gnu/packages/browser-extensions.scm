;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023, 2024 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2025 Robin Templeton <robin@guixotic.coop>
;;; Copyright © 2025 André Batista <nandre@riseup.net>
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

(define-module (gnu packages browser-extensions)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu build chromium-extension)
  #:use-module (gnu build icecat-extension)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls))

(define adaptive-tab-bar-colour
  (package
    (name "adaptive-tab-bar-colour")
    (version "2.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/easonwong-de/Adaptive-Tab-Bar-Colour")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03w06qrfg3bra6z7bmksj7mj27xnznn0ddm8gsr8z8b32hiifg4r"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." #$(assq-ref (package-properties this-package) 'addon-id)))))
    (home-page "https://github.com/easonwong-de/Adaptive-Tab-Bar-Colour")
    (synopsis "Adaptive tab bar colour")
    (description
     "This package provides a browser extension for changing tab bar colour to
match website theme.")
    (license license:expat)
    (properties '((addon-id . "ATBC@EasonWong")))))

(define-public adaptive-tab-bar-colour/icecat
  (make-icecat-extension adaptive-tab-bar-colour))

(define play-to-kodi
  (package
    (name "play-to-kodi")
    (version "1.9.1")
    (home-page "https://github.com/khloke/play-to-xbmc-chrome")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01rmcpbkn9vhcd8mrah2jmd2801k2r5fz7aqvp22hbwmh2z5f1ch"))))
    (build-system copy-build-system)
    (synopsis "Send website contents to Kodi")
    (description
     "Play to Kodi is a browser add-on that can send video, audio, and other
supported content to the Kodi media center.")
    (license license:expat)))

(define-public play-to-kodi/chromium
  (make-chromium-extension play-to-kodi))

(define ublock-main-assets
  ;; Arbitrary commit of branch master,
  ;; Update when updating uBlockOrigin.
  (let* ((name "ublock-main-assets")
         (commit "4696d6a593755d34b28c073b0150f4b4bb000387")
         (revision "4")
         (version (git-version "0" revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/uBlockOrigin/uAssets")
            (commit commit)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "02cixd497fhhxk2xg74ys20d3h0l2s5rpvvqxdbvlpjvliinsg50")))))

(define ublock-prod-assets
  ;; Arbitrary commit of branch gh-pages,
  ;; Update when updating uBlockOrigin.
  (let* ((name "ublock-prod-assets")
         (commit "bfeb222d20279afcc997d4b804fc4862f1364f95")
         (revision "4")
         (version (git-version "0" revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/uBlockOrigin/uAssets")
            (commit commit)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "161ilyilvpixgxx09qjcyiavjwc120hh012jfiq4r6lk4dvm8ij0")))))

(define ublock-origin
  (package
    (name "ublock-origin")
    (version "1.65.0")
    (home-page "https://github.com/gorhill/uBlock")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mmgacpp5g6ypfjp4niyyvhhc2linr752gr274ssqirzhbwdygpw"))))
    (build-system gnu-build-system)
    (outputs '("xpi" "firefox" "chromium"))
    (properties '((addon-id . "uBlock0@raymondhill.net")))
    (arguments
     (list
      #:tests? #f                      ;no tests
      #:allowed-references '()
      #:phases
      #~(modify-phases (map (lambda (phase)
                              (assq phase %standard-phases))
                            '(set-paths unpack patch-source-shebangs))
          (add-after 'unpack 'do-not-depend-on-git
            (lambda _
              (mkdir-p "dist/build/uAssets/main")
              (copy-recursively #$ublock-main-assets "dist/build/uAssets/main")
              (mkdir-p "dist/build/uAssets/prod")
              (copy-recursively #$ublock-prod-assets "dist/build/uAssets/prod")))
          (add-after 'unpack 'make-files-writable
            (lambda _
              ;; The build system copies some files and later tries
              ;; modifying them.
              (for-each make-file-writable (find-files "."))))
          (add-after 'patch-source-shebangs 'build-xpi
            (lambda _
              (invoke "./tools/make-firefox.sh" "all")))
          (add-after 'build-xpi 'build-chromium
            (lambda _
              (invoke "./tools/make-chromium.sh")))
          (add-after 'build-chromium 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((addon-id #$(assq-ref properties 'addon-id))
                     (firefox (in-vicinity
                               (assoc-ref outputs "firefox") addon-id))
                     (xpi (assoc-ref outputs "xpi"))
                     (chromium (assoc-ref outputs "chromium")))
                (install-file "dist/build/uBlock0.firefox.xpi"
                              (string-append xpi "/lib/mozilla/extensions"))
                (copy-recursively "dist/build/uBlock0.firefox" firefox)
                (copy-recursively "dist/build/uBlock0.chromium" chromium)))))))
    (native-inputs
     (list python-wrapper zip))
    (synopsis "Block unwanted content from web sites")
    (description
     "uBlock Origin is a @dfn{wide spectrum blocker} for IceCat and
ungoogled-chromium.")
    (license license:gpl3+)))

(define-public ublock-origin/chromium
  (make-chromium-extension ublock-origin "chromium"))

(define-public ublock-origin/icecat
  (make-icecat-extension ublock-origin "firefox"))

(define-public passff-host
  (package
    (name "passff-host")
    (version "1.2.4")
    (home-page "https://github.com/passff/passff-host")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lcwa1qzfxlifmj33qndp1wgi6yx6vj21ir0az79vhm5k03p961z"))))
    (build-system copy-build-system)
    (arguments
     (let ((native-manifests "lib/icecat/native-messaging-hosts"))
       (list
        #:install-plan
        `'(("src" ,native-manifests #:include ("passff.json" "passff.py")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'substitute
              (lambda _
                (substitute* "src/passff.json"
                  (("PLACEHOLDER")
                   (format #f "~a/~a/passff.py" #$output #$native-manifests)))
                (substitute* "src/passff.py"
                  (("_VERSIONHOLDER_") #$version)
                  (("^COMMAND = .*")
                   (format #f "COMMAND = \"~a/bin/pass\"~%"
                           #$(this-package-input "password-store"))))
                (patch-shebang "src/passff.py")))))))
    (inputs (list password-store python))
    (synopsis "Host app for the WebExtension PassFF")
    (description "This piece of software wraps around the zx2c4 pass shell
command.  It has to be installed for the PassFF browser extension to work
properly.")
    (license license:gpl2+)))

(define passff
  (package
    (name "passff")
    (version "1.16")
    (home-page "https://github.com/passff/passff")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0y3cbgy89lgvq6lfabp7mi1zhphdvihcccn3yw5mmaql9yrdm5kc"))))
    (propagated-inputs (list passff-host))
    (build-system copy-build-system)
    (properties '((addon-id . "passff@invicem.pro")))
    (arguments
     `(#:install-plan '(("src" ,(assq-ref properties 'addon-id)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'substitute-placeholder
           (lambda _
             (substitute* "src/manifest.json"
               (("_VERSIONHOLDER_") ,version)))))))
    (synopsis "Pass management extension for Mozilla Firefox")
    (description "This extension will allow you to access your zx2c4 pass
repository directly from your web browser.  You can choose to automatically
fill and submit login forms if a matching password entry is found.")
    (license license:gpl2+)))

(define-public passff/icecat
  (make-icecat-extension passff))

(define keepassxc-browser
  (package
    (name "keepassxc-browser")
    (version "1.9.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/keepassxreboot/keepassxc-browser")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12m7j7gz5gdhlv3paj9mmv9nb94cf80lridipmbdvk9shr43d0ag"))
	      ;;  Default 'manifest.json' targets chromium based browsers and
	      ;;  depends on background.service_worker support.
	      ;;  See: <https://bugzilla.mozilla.org/show_bug.cgi?id=1573659>
	      (snippet
	       #~(begin
		   (delete-file "keepassxc-browser/manifest.json")
		   (copy-file "dist/manifest_firefox.json"
			      "keepassxc-browser/manifest.json")))))
    (build-system copy-build-system)
    (properties
     '((addon-id . "keepassxc-browser@keepassxc.org")))
    (arguments
     `(#:install-plan
       '(("keepassxc-browser" ,(assq-ref properties 'addon-id)))))
    (synopsis "Browser extension for the KeePassXC password manager")
    (description
     "This package provides an extension allow the browser to work together
with the @uref{https://keepassxc.org, KeePassXC} password manager.")
    (home-page "https://keepassxc.org")
    (license license:gpl3+)))

(define-public keepassxc-browser/icecat
  (make-icecat-extension keepassxc-browser))

(define livemarks
  (package
    (name "livemarks")
    (version "3.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nt1m/livemarks")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02p2080lgnb2xy4n781ydjdywkg7g7hmz6cpnbh6icldbjy5xa5i"))))
    (build-system copy-build-system)
    (arguments
     (list #:install-plan
           #~'(("." #$(assq-ref (package-properties this-package) 'addon-id)))))
    (home-page "https://github.com/nt1m/livemarks")
    (synopsis "RSS feed bookmark folders")
    (description
     "This browser extension provides auto-updated RSS feed bookmark folders.")
    (license license:expat)
    (properties '((addon-id . "{c5867acc-54c9-4074-9574-04d8818d53e8}")))))

(define-public livemarks/icecat
  (make-icecat-extension livemarks))

(define noscript
  (package
    (name "noscript")
    (version "13.0.9")
    (source (origin
              (method url-fetch/zipbomb)
              (uri (string-append
                    "https://noscript.net/download/releases/noscript-" version
                    ".xpi"))
              (sha256
               (base32
                "1xbisx3xqak9aj7nb2lh94an6yfldsl6a2g2qc87vxi1zwdbcnjj"))))
    (build-system copy-build-system)
    (properties '((addon-id . "{73a6fe31-595d-460b-a920-fcc0f8843232}")))
    (arguments
     `(#:install-plan '(("." ,(assq-ref properties 'addon-id)))))
    (home-page "https://noscript.net")
    (synopsis "Software providing extra protection for various browsers")
    (description "The NoScript Security Suite is a software providing extra
protection for web browsers.")
    (license license:gpl3+)))

(define-public noscript/icecat
  (make-icecat-extension noscript))

(define privacy-redirect
  (package
    (name "privacy-redirect")
    (version "1.1.49")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SimonBrazell/privacy-redirect")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "13j5i6vh4lq9hcqmqbmn8fnymnplwra5rm696h6magbjxnj3nkyz"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("src" #$(assq-ref (package-properties this-package) 'addon-id)))))
    (home-page "https://github.com/SimonBrazell/privacy-redirect")
    (synopsis "Redirect to privacy friendly alternative frontends")
    (description
     "This package provides a browser extension that redirects sites to their
privacy friendly alternative frontends.  It's possible to toggle all redirects
on and off and the extension will default to using random instances if none are
selected.")
    (license license:gpl3)
    (properties '((addon-id . "{b7f9d2cd-d772-4302-8c3f-eb941af36f76}")))))

(define-public privacy-redirect/icecat
  (make-icecat-extension privacy-redirect))

(define-public web-eid-host
  (package
    (name "web-eid-host")
    (version "2.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/web-eid/web-eid-app")
             (commit (string-append "v" version))
             ;; This package contains two git modules:
             ;; <https://github.com/web-eid/libelectronic-id>, a library with
             ;; no support for standalone installation; and a copy of the
             ;; Web-eID WebExtension, deleted below.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02kryhzy4zizly8pj0bldcfh1hn93ls83033n98i6zaylhf3sn5f"))
       (modules '((guix build utils)))
       ;; Delete bundled copy of the corresponding browser extension.
       (snippet '(delete-file-recursively "src/mac/js"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-icecat-nmh
            (lambda _
              (let ((icecat-lib (format #f "~A/lib/icecat" #$output))
                    (mozilla-lib (format #f "~A/lib/mozilla" #$output)))
                (mkdir-p icecat-lib)
                (copy-recursively mozilla-lib icecat-lib)))))))
    (native-inputs (list googletest pkg-config qttools))
    (inputs (list openssl pcsc-lite qtsvg))
    (home-page "https://github.com/web-eid/web-eid-app")
    (synopsis "Web eID native messaging host for Estonian ID cards")
    (description
     "Native messaging host for the Web eID browser extension that performs
signing and authentication operations with Estonian ID cards.  The
@command{web-eid} program can also be used directly in command-line mode.

It requires a running pcscd service and a compatible card reader.")
    (license license:expat)))
