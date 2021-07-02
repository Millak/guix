;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016-2023 Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Marius Bakke <marius@gnu.org>
;;; Copyright © 2021 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2024 Andy Tai <atai@atai.org>
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

(define-module (gnu packages gps)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system scons)
  #:use-module (guix build-system qt)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml))

(define-public gpsbabel
  (package
    (name "gpsbabel")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gpsbabel/gpsbabel")
                    (commit (string-append
                             "gpsbabel_"
                             (string-replace-substring version "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pyrbykf15znn63y9j1npdv148i9b8sgc947xq6dqw8gwx023ji5"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "zlib")
                  (delete-file-recursively "shapelib")
                  (delete-file-recursively "mac")
                  #t))))
    ;; TODO: "make doc" requires Docbook & co.
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-up-test-environment
                 (lambda* (#:key inputs #:allow-other-keys)
                   (setenv "TZDIR" (search-input-directory inputs "share/zoneinfo"))))

               (replace 'install
                 (lambda _
                   (install-file "gpsbabel" (string-append #$output "/bin")))))

           #:configure-flags
           #~(list
              "-DGPSBABEL_MAPPREVIEW=OFF"
              "-DGPSBABEL_WITH_LIBUSB=pkgconfig"
              "-DGPSBABEL_WITH_SHAPELIB=pkgconfig"
              "-DGPSBABEL_WITH_ZLIB=pkgconfig")))
    (inputs
     (list expat libusb libxkbcommon shapelib qt5compat qtbase qtserialport qttranslations zlib))
    (native-inputs
     (list which pkg-config qttools libxml2 tzdata-for-tests)) ;'xmllint' needed for the KML tests
    (home-page "https://www.gpsbabel.org/")
    (synopsis "Convert and exchange data with GPS and map programs")
    (description
     "GPSBabel converts waypoints, tracks, and routes between hundreds of
popular GPS receivers and mapping programs.  It contains extensive data
manipulation abilities making it a convenient for server-side processing or as
the back-end for other tools.  It does not convert, transfer, send, or
manipulate maps.")
    (license (list license:gpl2+))))

(define-public gpscorrelate
  (package
    (name "gpscorrelate")
    (version "2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dfandrich/gpscorrelate")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kvybhfnygz79q3pgwc1q2x4ccmnsfscx2hzxnmzjbnc6arnqari"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; This is a rudimentary build system.
             (substitute* "Makefile"
               (("prefix[[:blank:]]*=.*$")
                (string-append "prefix = " (assoc-ref outputs "out")
                               "\n")))
             #t)))))
    (inputs
     (list gtk+ libxml2 exiv2))
    (native-inputs
     (list pkg-config docbook-xml docbook-xsl libxslt))
    (home-page "https://dfandrich.github.io/gpscorrelate/")
    (synopsis "GPS photo correlation tool to geo-localize images")
    (description
     "GPS Correlate is a program to match a recorded GPS track with the EXIF
tags in digital camera photos, and update the EXIF tags with the location that
the photo was taken.  It does this by using the timestamp in the photo and
finding a data point in the GPS track that matches, or interpolating a point
between two other data points.")
    (license license:gpl2+)))

(define-public gama
  (package
    (name "gama")
    (version "2.30")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://gnu/gama/gama-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0yph6q7a0dy2r2vsrkjg26q8v988pcvnaay5lk6q7k06plpr2x1m"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (delete-file-recursively "lib/expat")
            (for-each delete-file (find-files "doc/fig" "\\.pdf$"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list
                           ;; force not to use of bundled copy of expat
                           "--enable-expat_1_1=no")
      #:parallel-tests? #f)) ; race condition
    (native-inputs
     (list libxml2 yaml-cpp))
    (inputs
     (list expat sqlite))
    (home-page "https://www.gnu.org/software/gama/")
    (synopsis "Adjustment of geodetic networks")
    (description
     "GNU Gama is a program for the adjustment of geodetic networks.  It is
useful in measurements where Global Positioning System (GPS) is not available,
such as underground.  It features the ability to adjust in local Cartesian
coordinates as well as partial support for adjustments in global coordinate systems.")
    (license license:gpl3+)))

(define-public gpxsee
  (package
    (name "gpxsee")
    (version "13.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tumic0/GPXSee")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15m5rqky2fwsq5n6x531350x39wwhpv1hz56m1yfaxj7acxhb2p5"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase
      #:tests? #f                     ; no tests
      #:phases #~(modify-phases %standard-phases
                   (replace 'configure
                     ;; Use lrelease to convert TS translation files into QM files.
                     (lambda _
                       (apply invoke "lrelease"
                              (find-files "lang" "\\.ts"))
                       (invoke "qmake"
                               (string-append "PREFIX=" #$output)))))))
    (inputs (list libxkbcommon qtbase qtpositioning qtserialport qtsvg))
    (native-inputs (list qttools))
    (home-page "https://www.gpxsee.org")
    (synopsis "GPS log file viewer and analyzer")
    (description
     "GPXSee is a Qt-based GPS log file viewer and analyzer that supports all
common GPS log file formats.  It can display multiple tracks on various on-
and off-line maps.  You can easily add more maps and graph other captured data
such as elevation, speed, heart rate, power, temperature, and gear shifts.")
    (license license:gpl3)))

(define-public gpsd
  (package
    (name "gpsd")
    (version "3.23.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/gpsd/gpsd-" version ".tar.xz"))
       (sha256
        (base32 "1hd8b09is4gd73lpsdywxxdx11iijikmqgxd0y57pic3yxnlcb6a"))))
    (build-system scons-build-system)
    (native-inputs
     (list bc pkg-config))
    (inputs
     (list bash-minimal
           bluez
           dbus
           gtk+
           libcap
           libusb
           ncurses
           python
           python-pycairo
           python-pygobject
           python-pyserial
           python-wrapper
           qtbase-5))
    (arguments
     `(#:scons-flags
       (list (string-append "prefix=" %output)
             (let ((version ,(version-major+minor (package-version python))))
               (string-append "python_libdir=" %output
                              "/lib/python" version
                              "/site-packages"))
             "qt_versioned=5")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "TAR" "noop")
             (substitute* "SConscript"
               (("envs = \\{\\}")
                "envs = os.environ"))))
         (add-after 'install 'wrap-python-scripts
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (pycairo (assoc-ref inputs "python-pycairo"))
                    (pygobject (assoc-ref inputs "python-pygobject"))
                    (pyserial (assoc-ref inputs "python-pyserial"))
                    (sitedir (lambda (package)
                               (string-append package
                                              "/lib/python"
                                              ,(version-major+minor
                                                (package-version python))
                                              "/site-packages")))
                    (pythonpath (string-join (map sitedir
                                                  (list out pycairo pygobject
                                                        pyserial))
                                             ":")))
               (for-each (lambda (script)
                           (wrap-program (string-append out "/bin/" script)
                             `("GUIX_PYTHONPATH" ":" prefix (,pythonpath))))
                         '("gegps" "gpscat" "gpsfake" "gpsprof"
                           "ubxtool" "xgps" "xgpsspeed" "zerk"))))))))
    (synopsis "GPS service daemon")
    (description
     "@code{gpsd} is a service daemon that monitors one or more GPSes or AIS
receivers attached to a host computer through serial or USB ports, making all
data on the location/course/velocity of the sensors available to be queried on
TCP port 2947 of the host computer.")
    (home-page "https://gpsd.gitlab.io/gpsd/")
    (license license:bsd-2)))
