;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Ryan Prior <rprior@protonmail.com>
;;; Copyright © 2023 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2023, 2024 altadil <Altadil@protonmail.com>
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

(define-module (gnu packages pantheon)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public granite
  (package
    (name "granite")
    (version "7.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/granite")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pyvkif2kin5dskh7adadsh4r96mvx12y7cs6gnm0ml733q548dj"))))
    (build-system meson-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-icon-cache
                    (lambda _
                      (setenv "DESTDIR" "/"))))))
    (inputs (list sassc))
    (propagated-inputs (list glib libgee gtk))      ;required in .pc file
    (native-inputs (list gettext-minimal
                         `(,glib "bin")
                         gobject-introspection
                         pkg-config
                         python
                         vala))
    (home-page "https://github.com/elementary/granite")
    (synopsis "Library that extends GTK with common widgets and utilities")
    (description "Granite is a companion library for GTK+ and GLib.  Among other
things, it provides complex widgets and convenience functions designed for use
in apps built for the Pantheon desktop.")
    (license license:lgpl3+)))

;; This is required for pantheon apps that have not been ported to GTK4 yet.
(define-public granite-6
  (package
    (inherit granite)
    (version "6.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/granite")
                    (commit version)))
              (file-name (git-file-name "granite" version))
              (sha256
               (base32
                "0ilslmg63hh2x7h5rvs3mhzw1y9ixhhkqnn1j1lzwm12v2iidkaq"))))
    (propagated-inputs (list glib libgee gtk+))))

(define-public pantheon-calculator
  (package
    (name "pantheon-calculator")
    (version "8.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/calculator")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1as5rxd0b6z3lnh8my36szr056rxxqwkjzvaiylspx5g2kg3qjs0"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'disable-schema-cache-generation
                     (lambda _
                       (setenv "DESTDIR" "/"))))))
    (inputs
      (list granite
            glib
            gtk
            libgee
            libhandy))
    (native-inputs
      (list cmake
            `(,glib "bin") ; for glib-compile-schemas
            gettext-minimal
            pkg-config
            vala))
    (home-page "https://github.com/elementary/calculator")
    (synopsis "Desktop calculator")
    (description "Calculator is an application for performing simple
arithmetic.  It is the default calculator application in the Pantheon
desktop.")
    (license license:gpl3)))

(define-public pantheon-terminal
  (package
    (name "pantheon-terminal")
    (version "6.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/terminal")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0x3gzghnfx4a1q2zhra4dysc0pm1zvlfdxj96qhfb627pz16iv4k"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f      ; Tests invole launching the terminal.
       #:glib-or-gtk? #t
       #:phases (modify-phases %standard-phases
                  (add-before 'install 'set-environment-variables
                    (lambda _
                      ;; Disable compiling schemas and updating desktop databases
                      (setenv "DESTDIR" "/")))
                  (add-after 'install 'install-symlinks
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out
                                   "/bin/io.elementary.terminal"))
                             (link (string-append out "/bin/pantheon-terminal")))
                        (symlink bin link)))))))
    (native-inputs (list appstream
                         desktop-file-utils     ;required for tests
                         gettext-minimal        ;for msgfmt
                         `(,glib "bin")         ;for glib-compile-resources
                         gobject-introspection
                         pkg-config
                         vala
                         xvfb-run))
    (inputs (list granite-6
                  gtk+
                  libgee
                  libhandy
                  pcre2
                  vte))
    (synopsis "Terminal emulator from elementaryOS")
    (description "pantheon-terminal is a lightweight, beautiful and simple
terminal.  It comes with sane defaults, browser-class tabs, sudo paste
protection, smart copy/paste, and little to no configuration.  It is the default
terminal in the Pantheon desktop.")
    (home-page "https://elementary.io/open-source")
    (license license:lgpl3)))

(define-public sideload
  (package
    (name "sideload")
    (version "6.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elementary/sideload")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vrj91899f13cvzpycqy3y74hmixsffjbzsj29da7n370fa3ci86"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-environment-variables
           (lambda _
             ;; Disable compiling schemas and updating desktop databases
             (setenv "DESTDIR" "/")))
         (add-after 'install 'install-symlinks
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/io.elementary.sideload"))
                    (link (string-append out "/bin/sideload")))
               (symlink bin link)))))))
    (inputs
     `(("flatpak" ,flatpak)
       ("glib" ,glib)
       ("granite" ,granite)
       ("gtk" ,gtk+)
       ("hicolor-icon-theme" ,hicolor-icon-theme)
       ("libgee" ,libgee)
       ("libhandy" ,libhandy)
       ("libostree" ,libostree)
       ("libxml2" ,libxml2)))
    (propagated-inputs
     ;; Sideload needs these in the environment to fetch data securely from
     ;; Flatpak remotes.
     (list gnupg gpgme))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("glib:bin" ,glib "bin")
       ("gobject-introspection" ,gobject-introspection)
       ("pkg-config" ,pkg-config)
       ("vala" ,vala)))
    (home-page "https://github.com/elementary/sideload")
    (synopsis "Graphical application to side-load Flatpaks")
    (description "Sideload handles flatpakref files, like those you might find
on Flathub or another third-party website providing a Flatpak app for
download.")
    (license license:gpl3+)))

(define-public pantheon-wallpapers
  (package
    (name "pantheon-wallpapers")
    (version "7.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/elementary/wallpapers/")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256 (base32
                       "0km3h52kapbm8ymwxdxasz80qbgzkfni7981pdyf740wjp7linwb"))))
    (build-system meson-build-system)
    (native-inputs (list gettext-minimal))  ; for msgfmt
    (inputs (list libexif))
    (synopsis "Wallpapers for the Pantheon desktop")
    (description "This package provides wallpapers for the Pantheon desktop.")
    (home-page "https://github.com/elementary/wallpapers")
    (license (list license:cc-by-sa4.0
                   license:cc0
                   (license:non-copyleft "https://unsplash.com/license")
                   (license:non-copyleft "https://www.pexels.com/license/")))))
