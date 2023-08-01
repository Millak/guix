;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Taiju HIGASHI <higashi@taiju.info>
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

(define-module (gnu packages mastodon)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages time)
  #:use-module (gnu packages xml))

(define-public toot
  (package
    (name "toot")
    (version "0.38.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "toot" version))
        (sha256
         (base32 "1cn646jzys9vjaw20sxmgzc7zq5a5ma8vabvrw9zpa0yl9wm97my"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "py.test")))))))
    (native-inputs
     (list python-psycopg2 python-pytest))
    (inputs
     (list python-beautifulsoup4
           python-tomlkit
           python-requests
           python-urwid
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
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GeopJr/Tuba")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xhyz6wi17g4m76lr6qc75q4xnnw7c3dh3d04dg8m5gzk6j0y89x"))))
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
           pkg-config))
    (inputs
     (list gst-plugins-bad
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
  (package
    (name "tootle")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bleakgrey/tootle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1nm57239mhdq462an6bnhdlijpijxmjs9mqbyirwxwa048d3n4rm"))
       (patches
        (search-patches
         ;; https://github.com/bleakgrey/tootle/pull/339
         "tootle-glib-object-naming.patch"
         ;; https://github.com/bleakgrey/tootle/pull/322
         "tootle-reason-phrase.patch"))))
    (build-system meson-build-system)
    (arguments
     `(#:glib-or-gtk? #t
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'skip-gtk-update-icon-cache
           ;; Don't create 'icon-theme.cache'.
           (lambda _
             (substitute* "meson/post_install.py"
               (("gtk-update-icon-cache") "true"))))
         (add-after 'unpack 'patch-source
           (lambda _
             (substitute* "src/Dialogs/NewAccount.vala"
               (("xdg-mime") (which "xdg-mime")))
             ;; Patch for building on glib < 2.64
             (substitute* "src/Build.vala"
               (("(os_name = ).*" _ first) (string-append first "\"GNU\";\n"))
               (("(os_ver = ).*" _ first) (string-append first "\"Guix\";\n"))
               (("GLib.Environment.get_os_info.*") "\"unknown\";\n"))))
         (add-after 'install 'symlink-package
           (lambda* (#:key outputs #:allow-other-keys)
             (symlink "com.github.bleakgrey.tootle"
                      (string-append (assoc-ref outputs "out")
                                     "/bin/tootle")))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin") ; for glib-compile-resources
           gsettings-desktop-schemas pkg-config))
    (inputs
     (list glib-networking
           gtk+
           json-glib
           libgee
           libhandy
           libsoup-minimal-2
           vala
           xdg-utils))
    (home-page "https://github.com/bleakgrey/tootle")
    (synopsis "GTK3 client for Mastodon")
    (description "Tootle is a GTK client for Mastodon.  It provides a clean,
native interface that allows you to integrate Mastodon's social experience
seamlessly with your desktop environment.")
    (license license:gpl3+)))

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
