;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2019-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Mark Meyer <mark@ofosos.org>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2018, 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020 Prafulla Giri <pratheblackdiamond@gmail.com>
;;; Copyright © 2020-2025 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021 Alexandros Theodotou <alex@zrythm.org>
;;; Copyright © 2021 la snesne <lasnesne@lagunposprasihopre.org>
;;; Copyright © 2021, 2022, 2023, 2024, 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2022 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Mehmet Tekman <mtekman89@gmail.com>
;;; Copyright © 2024 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
;;; Copyright © 2025 Junker <dk@junkeria.club>
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

(define-module (gnu packages kde)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages image)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-graphics)
  #:use-module (gnu packages kde-multimedia)
  #:use-module (gnu packages kde-pim)
  #:use-module (gnu packages kde-plasma)
  ;; Including this module breaks the build.
  ;#:use-module ((gnu packages kde-systemtools) #:select (dolphin))
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public libqaccessibilityclient
  (package
    (name "libqaccessibilityclient")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://kde/stable/" name
                                  "/libqaccessibilityclient-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "0csxbwy4479196l32j4xnk672kiyggcaf3fi3q2cbj9dc94c8l2c"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f ;TODO: Failing tests
           #:configure-flags
           #~(list (string-append
                    "-DQT_MAJOR_VERSION="
                    #$(version-major
                       (package-version (this-package-input "qtbase")))))
           #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              ;; make Qt render "offscreen", required for tests
                              (setenv "QT_QPA_PLATFORM" "offscreen")
                              ;; For missing '/etc/machine-id'
                              (setenv "DBUS_FATAL_WARNINGS" "0")
                              (setenv "HOME"
                                      (getcwd))
                              (invoke "dbus-launch" "ctest")))))))
    (native-inputs (list dbus extra-cmake-modules))
    (inputs (list qtbase))
    (home-page "https://invent.kde.org/libraries/libqaccessibilityclient")
    (synopsis "Helper library to make writing accessibility tools easier")
    (description "This package provides library that is used when writing
accessibility clients such as screen readers.")
    (license license:lgpl2.1+)))

(define-public libqaccessibilityclient-qt5
  (package
    (inherit libqaccessibilityclient)
    (name "libqaccessibilityclient-qt5")
    (inputs (modify-inputs (package-inputs libqaccessibilityclient)
              (replace "qtbase" qtbase-5)))))

(define-public qca
  (package
    (name "qca")
    (version "2.3.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://kde/stable/qca/" version
                            "/qca-" version ".tar.xz"))
        (sha256
         (base32 "0kkf8wyc7slii86danfl4cx59yhcyc363ydiwapnnyyxihlxamf5"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list openssl qtbase-5))
    (home-page "https://userbase.kde.org/QCA")
    (synopsis "Libraries for the Qt Cryptographic Architecture")
    (description "The Qt Cryptographic Architecture (QCA) provides a
straightforward and cross-platform API for a range of cryptographic features,
including SSL/TLS, X.509 certificates, SASL, OpenPGP, S/MIME CMS, and smart
cards.")
    (license license:lgpl2.1+)))

(define-public qca-qt6
  (package
    (inherit qca)
    (name "qca-qt6")
    (arguments (list #:configure-flags #~(list "-DBUILD_WITH_QT6=ON")))
    (inputs
     (list openssl qtbase qt5compat))))
