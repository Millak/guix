;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2019-2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2019, 2024, 2025 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020, 2021, 2022, 2023 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2025 Noé Lopez <noelopez@free.fr>
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

;;; Comment:

;;; This module is for packages that are part of GNOME Circle
;;; <https://circle.gnome.org/>.

;;; Code:

(define-module (gnu packages gnome-circle)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public apostrophe
  (package
    (name "apostrophe")
    (version "2.6.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.gnome.org/World/apostrophe")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wsvq2434p650cf3vq5w7a6czbk8in0ra7nji45mvwyfahdyn6j4"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:imported-modules (append %meson-build-system-modules
                                 %pyproject-build-system-modules)
      #:modules '((guix build meson-build-system)
                  ((guix build pyproject-build-system) #:prefix py:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-meson
            (lambda _
              (substitute* "build-aux/meson_post_install.py"
                (("gtk-update-icon-cache") "true"))))
          (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (wrap-program (search-input-file outputs "bin/apostrophe")
                `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                       ,(py:site-packages inputs outputs)))
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))
                `("PATH" prefix (,(dirname
                                   (search-input-file inputs
                                                      "/bin/pandoc"))))))))))
    (inputs
     (list bash-minimal
           glib
           gobject-introspection
           gspell
           gtk+
           libhandy
           pandoc
           python
           python-chardet
           python-levenshtein
           python-regex
           python-pycairo
           python-pygobject
           python-pyenchant
           python-pypandoc
           webkitgtk-with-libsoup2))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           pkg-config
           sassc))
    (home-page "https://gitlab.gnome.org/World/apostrophe")
    (synopsis "Markdown editor written in Python with GTK+")
    (description "Apostrophe is a GTK+ based distraction-free Markdown editor.
It uses pandoc as back-end for parsing Markdown.")
    (license license:gpl3)))

(define-public gnome-authenticator
  (package
    (name "gnome-authenticator")
    (version "4.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.gnome.org/World/Authenticator.git/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zavax35n048spx097ymiq31s8b879qwbg8xmcxcx73r6m823mic"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:imported-modules `(,@%meson-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules `(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build meson-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-for-build
            (lambda _
              (substitute* "meson.build"
                (("gtk_update_icon_cache: true")
                 "gtk_update_icon_cache: false")
                (("update_desktop_database: true")
                 "update_desktop_database: false"))
              ;; Help the tests find the Cargo.toml in the sources.
              (substitute* "src/meson.build"
                (("'test'") "'test', cargo_options"))
              (delete-file "Cargo.lock")))
          ;; The meson 'configure phase changes to a different directory and
          ;; we need it created before unpacking the crates.
          (add-after 'configure 'prepare-cargo-build-system
            (lambda args
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        #:vendor-dir "vendor"
                        #:cargo-target #$(cargo-triplet)
                        args))
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums)))))))
    (native-inputs
     (append
      (list gettext-minimal
            `(,glib "bin") ; for glib-compile-schemas
            pkg-config
            rust
            `(,rust "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs (cons* bash-minimal
                   glib
                   gstreamer
                   gst-plugins-base
                   gst-plugins-bad
                   gtk
                   libadwaita
                   openssl
                   pipewire             ; Needed but not listed
                   sqlite
                   zbar
                   (cargo-inputs 'gnome-authenticator)))
    (home-page "https://apps.gnome.org/Authenticator")
    (synopsis "Generate two-factor codes")
    (description "Simple application for generating Two-Factor Authentication
Codes:

It features:

@itemize
@item Time-based/Counter-based/Steam methods support
@item SHA-1/SHA-256/SHA-512 algorithms support
@item QR code scanner using a camera or from a screenshot
@item Lock the application with a password
@item Beautiful UI
@item GNOME Shell search provider
@item Backup/Restore from/into known applications like FreeOTP+,
Aegis (encrypted / plain-text), andOTP, Google Authenticator
@end itemize")
    (license license:gpl3+)))
