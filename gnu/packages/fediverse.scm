;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Taiju HIGASHI <higashi@taiju.info>
;;; Copyright © 2024 Sergio Durigan Junior <sergiodj@sergiodj.net>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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

(define-module (gnu packages fediverse)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml))

(define-public toot
  (package
    (name "toot")
    (version "0.42.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "toot" version))
        (sha256
         (base32 "1vw3j504dxmq22s40kysps3d09hl7l48cwznwrfr9zqif67i4v3g"))))
    (build-system pyproject-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "py.test")))))))
    (native-inputs
     (list python-psycopg2-binary
           python-pytest
           python-pyyaml
           python-setuptools
           python-typing-extensions
           python-wheel))
    (inputs
     (list python-beautifulsoup4
           python-click
           python-requests
           python-tomlkit
           python-urwid
           python-urwidgets
           python-wcwidth))
    (home-page "https://github.com/ihabunek/toot/")
    (synopsis "Mastodon CLI client")
    (description "Interact with Mastodon social network from the command line.
Features include:
@itemize
@item Posting, replying, deleting statuses
@item Support for media uploads, spoiler text, sensitive content
@item Search by account or hash tag
@item Following, muting and blocking accounts
@item Simple switching between authenticated in Mastodon accounts
@end itemize")
    (license license:gpl3)))

(define-public tuba
  (package
    (name "tuba")
    (version "0.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GeopJr/Tuba")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a66kgsa7blsfk4fp62m76jmvqqlmilih1rs48wb41q69ldm0953"))))
    (build-system meson-build-system)
    (arguments
      (list
        #:glib-or-gtk? #t
        #:configure-flags #~(list "-Ddistro=true")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'glib-or-gtk-wrap 'lib-vars-wrap
              (lambda _
                (let ((gstvar "GST_PLUGIN_SYSTEM_PATH")
                      (pixvar "GDK_PIXBUF_MODULE_FILE"))
                  (wrap-program (string-append #$output "/bin/dev.geopjr.Tuba")
                    `(,gstvar ":" suffix (,(getenv gstvar)))
                    `(,pixvar ":" = (,(getenv pixvar)))))))
            (add-after 'lib-vars-wrap 'symlink-package
              (lambda _
                (with-directory-excursion (string-append #$output "/bin")
                  (symlink "dev.geopjr.Tuba" "tuba")))))))
    (native-inputs
     (list gdk-pixbuf ; so pixbuf loader cache (for webp) is generated
           gettext-minimal
           `(,glib "bin") ; for glib-compile-resources
           gsettings-desktop-schemas    ; for the org.gnome.system.proxy schema
           pkg-config))
    (inputs
     (list bash-minimal     ; for wrap-program
           gst-plugins-bad
           gst-plugins-base
           gst-plugins-good
           gstreamer
           gtk
           gtksourceview
           json-glib
           libadwaita
           libgee
           libsoup-minimal
           libsecret
           libspelling
           libwebp
           libxml2
           vala
           webp-pixbuf-loader))
    (home-page "https://tuba.geopjr.dev/")
    (synopsis "GTK client for Mastodon")
    (description "Tuba is a GTK client for Mastodon.  It provides a clean,
native interface that allows you to integrate Mastodon's social experience
seamlessly with your desktop environment.")
    (license license:gpl3)))

(define-public tootle
  (deprecated-package "tootle" tuba))

(define-public python-mastodon-py
  (package
    (name "python-mastodon-py")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Mastodon.py" version))
        (sha256
         (base32
          "1vikvkzcij2gd730cssigxi38vlmzqmwdy58r3y2cwsxifnxpz9a"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-blurhash
           python-dateutil
           python-decorator
           python-magic
           python-pytz
           python-requests
           python-six))
    (native-inputs
     (list python-blurhash
           python-cryptography
           python-http-ece
           python-pytest
           python-pytest-cov
           python-pytest-mock
           python-pytest-runner
           python-pytest-vcr
           python-requests-mock
           python-vcrpy))
    (home-page "https://github.com/halcy/Mastodon.py")
    (synopsis "Python wrapper for the Mastodon API")
    (description
     "This package provides a python wrapper for the Mastodon API.")
    (license license:expat)))

(define-public snac2
  (package
    (name "snac")
    (version "2.77")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/grunfink/snac2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mibrqjdv68lxgvhly46v6i04bv05bm06qp7qp1kjr3mph7wahs1"))))
    (build-system gnu-build-system)
    (inputs (list curl openssl))
    (arguments
     (list
       #:phases #~(modify-phases %standard-phases
                    (delete 'configure))
       #:tests? #f ; no test target
       #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                            (string-append "CFLAGS=-O2 -g -Wall -Wextra")
                            (string-append "PREFIX=" #$output)
                            (string-append "PREFIX_MAN=" #$output "/share/man"))))
    (home-page "https://codeberg.org/grunfink/snac2")
    (synopsis
     "Simple, minimalistic ActivityPub instance written in portable C")
    (description
     "Snac is a simple, minimalistic ActivityPub instance written in
portable C.

It features:

@itemize
@item Lightweight, minimal dependencies
@item Extensive support of ActivityPub operations, e.g. write public notes,
follow users, be followed, reply to the notes of others, admire wonderful
content (like or boost), write private messages, etc.
@item Multiuser support
@item Mastodon API support, so Mastodon-compatible apps can be used
@item Simple but effective web interface
@item Easily-accessed MUTE button to silence users
@item Tested interoperability with related software
@item No database needed
@item Totally JavaScript-free; no cookies either
@end itemize")
    (license license:expat)))
