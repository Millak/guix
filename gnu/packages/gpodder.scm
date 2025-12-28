;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2022 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages gpodder)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages music)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml))

(define-public gpodder
  (package
    (name "gpodder")
    (version "3.11.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gpodder/gpodder")
             (commit version)))
       (sha256
        (base32 "1zmp7kkldb59fx1y6k4mkff8ngmyb9pflcd3yqb28m9wb9bp4j4h"))
       (file-name (git-file-name name version))
       (patches (search-patches "gpodder-disable-updater.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Avoid needing xdg-utils as a propagated input.
          (add-after 'unpack 'patch-xdg-open
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/gpodder/util.py"
                (("xdg-open") (search-input-file inputs "bin/xdg-open")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "unittest"))))
          ;; 'msgmerge' introduces non-determinism by resetting the
          ;; POT-Creation-Date in .po files.
          (add-before 'install 'do-not-run-msgmerge
            (lambda _
              (substitute* "makefile"
                (("msgmerge") "true"))))
          (add-before 'install 'make-po-files-writable
            (lambda _
              (for-each
               (lambda (f)
                 (chmod f #o664))
               (find-files "po"))))
          (replace 'install
            (lambda _
              (setenv "PREFIX" #$output)
              (invoke "make" "install")))
          (add-after 'install 'wrap-gpodder
            (lambda _
              (let ((gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (wrap-program (string-append #$output "/bin/gpodder")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))))
    (native-inputs
     (list intltool
           python-minimock
           python-pytest
           python-pytest-cov
           python-pytest-httpserver
           python-setuptools
           which))
    (inputs
     (list bash-minimal
           gtk+
           python-pygobject
           python-pycairo
           python-requests
           python-dbus
           python-html5lib
           python-mutagen
           python-mygpoclient
           python-podcastparser
           yt-dlp
           xdg-utils))
    (home-page "https://gpodder.github.io")
    (synopsis "Simple podcast client")
    (description "gPodder is a podcatcher, i.e. an application that allows
podcast feeds (RSS, Atom, Youtube, Soundcloud, Vimeo and XSPF) to be
subscribed to, checks for new episodes and allows the podcast to be saved
locally for later listening.")
    (license license:gpl3+)))

(define-public libmygpo-qt
  (package
    (name "libmygpo-qt")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://stefan.derkits.at/files/"
                                  "libmygpo-qt/libmygpo-qt." version ".tar.gz"))
              (sha256
               (base32
                "1rpallrgfdpvdw2npjizw0gj7lidb8hxs7ak16jkryq2yijpzkjh"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list qtbase))
    (arguments
     (list #:configure-flags
           #~(list "-DBUILD_WITH_QT6=ON")
           #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     ;; TODO: Enable JsonCreatorTest-test when
                     ;; https://github.com/gpodder/gpodder/issues/446 is fixed.
                     (invoke "ctest" "-E" "JsonCreatorTest-test")))))))
    (home-page "https://gpodder.github.io")
    (synopsis "Qt/C++ library wrapping the gpodder web service")
    (description "@code{libmygpo-qt} is a Qt/C++ library wrapping the
@url{https://gpodder.net} APIs.  It allows applications to discover, manage
and track podcasts.")
    (license license:lgpl2.1+)))

(define-public libmygpo-qt5
  (package/inherit libmygpo-qt
    (name "libmygpo-qt5")
    (inputs
     (modify-inputs (package-inputs libmygpo-qt)
       (replace "qtbase" qtbase-5)))
    (arguments
     (substitute-keyword-arguments (package-arguments libmygpo-qt)
       ((#:configure-flags flags)
        #~(delete "-DBUILD_WITH_QT6=ON" #$flags))))))

(define-public python-mygpoclient
  (package
    (name "python-mygpoclient")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mygpoclient" version))
       (sha256
        (base32 "1w28ij4ar42725d7np6f3rxfvm4w2ms5j06ny70qmap26ijsaj52"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "test")))))))
    (native-inputs (list python-minimock python-pytest python-setuptools))
    (home-page "https://mygpoclient.readthedocs.io")
    (synopsis "Python library for the gPodder web service")
    (description
     "@code{mygpoclient} provides an easy and structured way to access the
@url{https://gpodder.net} web services.  In addition to subscription list
synchronization and storage, the API supports uploading and downloading
episode status changes.")
    (license license:gpl3)))

(define-public python-podcastparser
  (package
    (name "python-podcastparser")
    (version "0.6.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "podcastparser" version))
       (sha256
        (base32 "1mqkkxz928y430xx3mgw9dj78ilkgv9hjdha1hizbks6mmhcp6ib"))))
    (native-inputs
     (list python-pytest))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _ (invoke "pytest"))))))
    (build-system python-build-system)
    (home-page "http://gpodder.org/podcastparser")
    (synopsis "Simplified and fast RSS parser Python library")
    (description "@code{podcastparser} is a library for the gPodder project to
provide an easy and reliable way of parsing RSS and Atom-based podcast feeds
in Python.")
    (license license:isc)))

(define-public castget
  ;; Since ronn-ng uses a newer ruby-nokogiri, the test suite would fail on a
  ;; free call with the error: "free(): invalid pointer".  Use the latest
  ;; commit, which is immune to that problem.
  (let ((revision "1")
        (commit "e97b179227b4fc7e2e2bc5a373933624c0467daa"))
    (package
      (name "castget")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mlj/castget")
               (commit commit)))
         (sha256
          (base32 "1dsbmfkchza9bwcsks2iyq5n2sj55pdmh79jpg3hisxpjgqcvpyy"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake libtool pkg-config ronn-ng))
      (inputs (list curl glib taglib libxml2))
      (synopsis "Command line podcast downloader")
      (description
       "castget is a simple, command-line based RSS enclosure downloader.  It is
primarily intended for automatic, unattended downloading of podcasts.  It uses
libcurl for the download process.")
      (license license:lgpl2.1+)
      (home-page "https://castget.johndal.com"))))
