;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
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
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu build chromium-extension)
  #:use-module (gnu build icecat-extension)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages python))

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
         (commit "d93605b8584df8cd47bcc91b3d932feecd9e3a2a")
         (revision "1")
         (version (git-version "0" revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/uBlockOrigin/uAssets")
            (commit commit)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1bbwxmb5rb1afh6i5a7m1ysaw0022wi5g091vpahi4h805p1s7a2")))))

(define ublock-prod-assets
  ;; Arbitrary commit of branch gh-pages,
  ;; Update when updating uBlockOrigin.
  (let* ((name "ublock-prod-assets")
         (commit "1d3df32ef6672763f44b27a95fd5cb3b5770d5e2")
         (revision "1")
         (version (git-version "0" revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/uBlockOrigin/uAssets")
            (commit commit)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1cbx7w1nzdcjq0z4z7j9nr8922i27nslprrw5dy03xcdqwc3x4l6")))))

(define ublock-origin
  (package
    (name "ublock-origin")
    (version "1.53.2")
    (home-page "https://github.com/gorhill/uBlock")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mz1k5ghyc25v51md02qx7chrbg4cxagvqi18bcbs4agq8ix6sp7"))))
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
    (version "1.2.3")
    (home-page "https://github.com/passff/passff-host")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p18l1jh20x4v8dj64z9qjlp96fxsl5h069iynxfpbkzj6hd74yl"))))
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
    (version "1.15")
    (home-page "https://github.com/passff/passff")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1gymqyqppr8k9fqv5js7f6pk6hcc47qpf51x5cy6aahsk2v1qssj"))))
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
    (synopsis "zx2c4 pass management extension for Mozilla Firefox")
    (description "This extension will allow you to access your zx2c4 pass
repository directly from your web browser.  You can choose to automatically
fill and submit login forms if a matching password entry is found.")
    (license license:gpl2+)))

(define-public passff/icecat
  (make-icecat-extension passff))

(define keepassxc-browser
  (package
    (name "keepassxc-browser")
    (version "1.8.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url
                     "https://github.com/keepassxreboot/keepassxc-browser")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1059kcb95ig18izbchwlb7pz41l4l3vjwzlmhz3w8zw2qxm6hrvx"))))
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
