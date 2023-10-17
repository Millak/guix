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
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu build chromium-extension)
  #:use-module (gnu build icecat-extension)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
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
         (commit "c8783488f377723165e3661062bd124ae6d57165")
         (revision "0")
         (version (git-version "0" revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/uBlockOrigin/uAssets")
            (commit commit)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "1b6a1m6s060r49vg563f32rsy057af6i4jcyprym4sdci3z90nls")))))

(define ublock-prod-assets
  ;; Arbitrary commit of branch gh-pages,
  ;; Update when updating uBlockOrigin.
  (let* ((name "ublock-prod-assets")
         (commit "fbcfe9229ab6b865ef349c01a4eac73943be8418")
         (revision "0")
         (version (git-version "0" revision commit)))
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/uBlockOrigin/uAssets")
            (commit commit)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0s5rvaz8lc9lk44yfc8463vah8yppy1ybmag0dpd4m1hyj6165h0")))))

(define ublock-origin
  (package
    (name "ublock-origin")
    (version "1.51.0")
    (home-page "https://github.com/gorhill/uBlock")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i8rnij3sbwg6vj6znprrsca0n5xjzhmhppaa8v6jyxg6wrrfch1"))))
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
    (build-system trivial-build-system)
    (arguments
     (list
      #:modules '((guix build utils))
      #:builder
      #~(begin
          (use-modules (guix build utils))
          (setenv "PATH" (string-join
                          (list #$(file-append coreutils "/bin")
                                #$(file-append grep "/bin")
                                #$(file-append password-store "/bin")
                                #$(file-append python "/bin")
                                #$(file-append sed "/bin")
                                #$(file-append which "/bin")) ":"))
          (copy-recursively #$source ".")
          (substitute* "src/install_host_app.sh"
            (("#!/usr/bin/env sh") #$(file-append bash-minimal "/bin/sh"))
            (("(TARGET_DIR_FIREFOX=).*" all var)
             (string-append var #$output
                            "/lib/icecat/native-messaging-hosts")))
          (invoke #$(file-append gnu-make "/bin/make")
                  (string-append "VERSION=" #$version) "install-unix"))))
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
