;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2017, 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2023-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018, 2019, 2020, 2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2019-2023 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2019-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019, 2021 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2019, 2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2020–2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021, 2023-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2023, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021, 2022 Nikolay Korotkiy <sikmir@disroot.org>
;;; Copyright © 2022 Patrick Noll <patrick@patricknoll.com>
;;; Copyright © 2022 Roman Scherer <roman.scherer@burningswell.com>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Denis 'GNUtoo' Carikli <GNUtoo@cyberdimension.org>
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
;;; Copyright © 2024 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2025 Mattia Bunel <mattia.bunel@ehess.fr>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2025 Lars Bilke <lars.bilke@ufz.de>
;;; Copyright © 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Nguyễn Gia Phong <mcsinyx@disroot.org>
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

(define-module (gnu packages geo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system r)
  #:use-module (guix build-system zig)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages busybox)
  #:use-module (gnu packages c)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cran)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages speech)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public gmt
  (package
    (name "gmt")
    (version "6.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/GenericMappingTools/gmt/"
                           "releases/download/"
                           version "/gmt-" version "-src.tar.xz"))
       (sha256
        (base32 "07hlqg3adxrz7wqih8pydr44v7j40savcxfjlkaw3y9k82sas8j0"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #false)) ;tests need costline data and caches
    (inputs
     (list curl ffmpeg fftw gdal geos ghostscript netcdf openblas pcre2))
    (native-inputs
     (list graphicsmagick pkg-config))
    (home-page "https://www.generic-mapping-tools.org/")
    (synopsis "Generic mapping tools")
    (description "GMT is a collection of about 100 command-line tools for
manipulating geographic and Cartesian data sets (including filtering, trend
fitting, gridding, projecting, etc.) and producing high-quality illustrations
ranging from simple x-y plots via contour maps to artificially illuminated
surfaces, 3D perspective views and animations.  The GMT supplements add
another 50 more specialized and discipline-specific tools.  GMT supports over
30 map projections and transformations and requires support data such as GSHHG
coastlines, rivers, and political boundaries and optionally DCW country
polygons.")
    (license license:lgpl3+)))

(define-public libaec
  (package
    (name "libaec")
    (version "1.0.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.dkrz.de/k202009/libaec")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14myrmmiz9z6wgxqywf3a63cq514vrzsd6z4zvpwigvawlk30iip"))))
    (build-system cmake-build-system)
    (home-page "https://gitlab.dkrz.de/k202009/libaec")
    (synopsis "Adaptive Entropy Coding library")
    (description "Libaec provides fast lossless compression of 1 up to 32 bit
wide signed or unsigned integers (samples).  The library achieves best results
for low entropy data as often encountered in space imaging instrument data or
numerical model output from weather or climate simulations.  While floating
point representations are not directly supported, they can also be efficiently
coded by grouping exponents and mantissa.")
    (license license:bsd-2)))

(define-public eccodes
  (package
    (name "eccodes")
    (version "2.27.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://confluence.ecmwf.int/download/attachments/45757960/"
             "eccodes-" version "-Source.tar.gz"))
       (sha256
        (base32 "16cw4v2d0kjq6gq04paqny0sh5jymn70w449mig7m5h3spzv7rgd"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DENABLE_MEMFS=ON" "-DENABLE_PNG=ON")
       #:validate-runpath? #f))
    (inputs
     (list jasper libaec libjpeg-turbo libpng netcdf openjpeg))
    (native-inputs
     (list gfortran perl pkg-config python))
    (home-page "https://confluence.ecmwf.int/display/ECC")
    (synopsis "Library for handling the GRIB, BUFR and GTS file formats")
    (description "ecCodes is a package developed by @acronym{ECMWF, European
Centre for Medium-Range Weather Forecasts} which provides an application
programming interface and a set of tools for decoding and encoding messages in
the @acronym{WMO, World Meteorological Organization} FM-92 GRIB, WMO FM-94
BUFR and WMO GTS abbreviated header formats.")
    (license license:asl2.0)))

(define-public cdo
  (package
    (name "cdo")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://code.mpimet.mpg.de/attachments/download/29313/cdo-"
             version ".tar.gz"))
       (sha256
        (base32 "0b2d1d8r1lxs422dxajnmvjyhjwfichlkglv3yqm7wq7rjw0yyd4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-curl="
                                  #$(this-package-input "curl"))
                   (string-append "--with-eccodes="
                                  #$(this-package-input "eccodes"))
                   (string-append "--with-fftw3="
                                  #$(this-package-input "fftw"))
                   (string-append "--with-hdf5="
                                  #$(this-package-input "hdf5"))
                   (string-append "--with-netcdf="
                                  #$(this-package-input "netcdf"))
                   (string-append "--with-proj="
                                  #$(this-package-input "proj"))
                   (string-append "--with-udunits2="
                                  #$(this-package-input "udunits"))
                   (string-append "--with-libxml2="
                                  #$(this-package-input "libxml2")))
           ;; Some tests can fail on machines with many threads.
           #:parallel-tests? #f))
    (inputs
     (list curl eccodes fftw hdf5 libxml2 netcdf proj udunits))
    (native-inputs
     (list pkg-config python-wrapper))
    (home-page "https://code.mpimet.mpg.de/projects/cdo")
    (synopsis "Climate data operators")
    (description "@acronym{CDO, Climate Data Operators} is a collection of command-line
operators to manipulate and analyse climate and NWP model data.  Supported
data formats are GRIB 1/2, netCDF 3/4, SERVICE, EXTRA and IEG.  There are more
than 600 operators available.")
    (license license:bsd-3)))

(define-public h3
  (package
    (name "h3")
    (version "4.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber/h3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x764xzna8ka6yhgv2y4hb158a61y3g9a6835qckqp7wfkpqvb7f"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON")))
    (home-page "https://h3geo.org/")
    (synopsis "Hexagonal hierarchical geospatial indexing system")
    (description "H3 is a geospatial indexing system using a hexagonal grid
that can be (approximately) subdivided into finer and finer hexagonal grids,
combining the benefits of a hexagonal grid with S2's hierarchical
subdivisions.")
    (license license:asl2.0)))

;; For python-timezonefinder, remove it when it starts supporting newer
;; version.
(define-public h3-3
  (package
    (inherit h3)
    (name "h3")
    (version "3.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber/h3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bvsljfxmjvl23v9gxykc4aynjzh5xfy3wg02bxad7cknr1amx9j"))))))

(define-public python-h3
  (package
    (name "python-h3")
    (version "4.0.0b2")
    (source
     (origin
       (method git-fetch) ; no tests data in PyPi package
       (uri (git-reference
             (url "https://github.com/uber/h3-py")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k1n256hhlh05gjcj64pqh08zlaz6962jkb6nk1aazsgg8p41zs0"))
       (modules '((guix build utils)))
       ;; Remove bundeled H3 lib.
       (snippet #~(begin (delete-file-recursively "src/h3lib")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: Check why these tests are failing.
      ;; test_versions - assert (4, 1) == (4, 0)
      ;; test_resolution - h3._cy.error_system.H3Failed
      #:test-flags #~(list "-k" (string-append
                                 "not test_versions"
                                 " and not test_resolution"))
      #:phases
      #~(modify-phases %standard-phases
          ;; Use packaged in Guix h3 source.
          (add-after 'unpack 'patch-cmakelists
            (lambda _
              (substitute* "CMakeLists.txt"
                (("add_subdirectory\\(src/h3lib\\)")
                 (string-append
                  "include_directories(" #$(this-package-input "h3")
                  "/include/h3)\n"
                  "link_directories(" #$(this-package-input "h3")
                  "/lib)\n"))
                ((".*CMAKE_CURRENT_BINARY_DIR.*")
                 (string-append #$(this-package-input "h3")
                                "/include/h3/h3api.h\n"))))))))
    (native-inputs
     (list cmake-minimal
           python-cython
           python-numpy
           python-pytest
           python-scikit-build
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (inputs (list h3))
    (home-page "https://uber.github.io/h3-py")
    (synopsis "Python bindings for H3")
    (description "This package provides a Python bindings for H3, a
hierarchical hexagonal geospatial indexing system")
    (license license:asl2.0)))

;; For python-timezonefinder, remove it when it starts supporting newer
;; version.
(define-public python-h3-3
  (package
    (inherit python-h3)
    (name "python-h3")
    (version "3.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uber/h3-py")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16gxa1sivghxw179rik87r918mjasars2qkzidlwq83qfa4axn20"))))
    (inputs
     (modify-inputs (package-inputs python-h3)
       (replace "h3" h3-3)))))

(define-public memphis
  (package
    (name "memphis")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/jiuka/memphis")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "068c3943pgbpfjq44pmvn5fmkh005ak5aa67vvrq3fn487c6w54q"))))
    (build-system glib-or-gtk-build-system)
    (outputs '("out" "doc"))
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             "--enable-gtk-doc"
             "--enable-vala"
             (string-append "--with-html-dir=" #$output "/share/gtk-doc/html"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-autogen
           (lambda _
             (substitute* "autogen.sh"
               (("\\./configure \"\\$@\"")
                "")))))))
    (native-inputs
     (list autoconf
           automake
           docbook-xml-4.3
           gobject-introspection
           gtk-doc/stable
           libtool
           libxml2                      ;for XML_CATALOG_FILES
           pkg-config
           python-wrapper
           seed
           vala))
    (inputs (list expat glib))
    (propagated-inputs (list cairo))
    (synopsis "Map-rendering for OpenSteetMap")
    (description "Memphis is a map-rendering application and a library for
OpenStreetMap written in C using eXpat, Cairo and GLib.")
    (home-page "http://trac.openstreetmap.ch/trac/memphis/")
    (license license:lgpl2.1+)))

(define-public geos
  (package
    (name "geos")
    (version "3.12.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.osgeo.org/geos/geos-"
                                  version
                                  ".tar.bz2"))
              (sha256
               (base32
                "00qdk9a4048pzfj2rhzkfw3lvm642znf6kr4x29i3d94494pxsnn"))))
    (build-system cmake-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (add-after
                    'unpack 'patch-test-shebangs
                    (lambda _
                      (substitute* '("tests/xmltester/testrunner.sh"
                                     "tests/xmltester/safe_to_xml.sh")
                        (("/bin/sh") (which "sh"))))))))
    (inputs
     (list glib))
    (home-page "https://libgeos.org/")
    (synopsis "Geometry Engine for Geographic Information Systems")
    (description
     "GEOS provides a spatial object model and fundamental geometric
functions.  It is a C++ port of the Java Topology Suite (JTS).  As such,
it aims to contain the complete functionality of JTS in C++.  This
includes all the OpenGIS Simple Features for SQL spatial predicate
functions and spatial operators, as well as specific JTS enhanced
topology functions.")
    (license (list license:lgpl2.1+          ; Main distribution.
                   license:zlib              ; tests/xmltester/tinyxml/*
                   license:public-domain)))) ; include/geos/timeval.h

(define-public gnome-maps
  (package
    (name "gnome-maps")
    (version "46.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/" name "/"
                                  (version-major version) "/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "09af1kk63h4ks6kv3sixfmjxkfy0qbi2iym6q5ahcsfjp12d3qc4"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'skip-cache-and-database-updates
            (lambda _
              (substitute* "meson.build"
                (("([a-z_]*): true" all option)
                 (cond                ; cond rather than match saves an import
                  ((member option '("gtk_update_icon_cache"
                                    "update_desktop_database"))
                   (string-append option ": false"))
                  (else all))))))
          (add-before 'check 'check-setup
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              ;; for timeTest
              (setenv "TZDIR"
                      (search-input-directory (or native-inputs inputs)
                                              "share/zoneinfo"))))
          (add-after 'install 'wrap
            (lambda _
              (let ((gi-typelib-path (getenv "GI_TYPELIB_PATH")))
                (substitute* (string-append #$output "/share/gnome-maps/"
                                            "org.gnome.Maps")
                  (("imports\\.package\\.init" all)
                   (string-append "'" gi-typelib-path "'.split(':').forEach("
                                  "path => imports.gi.GIRepository.Repository."
                                  "prepend_search_path(path));\n"
                                  all)))))))))
    (native-inputs
     (list gettext-minimal
           `(,glib "bin")
           gobject-introspection
           libportal
           pkg-config
           tzdata-for-tests))
    (inputs
     (list folks
           evolution-data-server
           geoclue
           geocode-glib
           gfbgraph
           gjs
           glib
           glib-networking
           gnome-online-accounts
           gsettings-desktop-schemas
           gtk
           libadwaita
           libgee
           libgweather4
           librsvg
           libsecret
           libshumate
           libsoup
           libxml2
           rest-next
           webkitgtk-for-gtk3))
    (synopsis "Graphical map viewer and wayfinding program")
    (description "GNOME Maps is a graphical map viewer.  It uses map data from
the OpenStreetMap project.  It can provide directions for walking, bicycling,
and driving.")
    (home-page "https://wiki.gnome.org/Apps/Maps")
    (license license:gpl2+)))

(define-public libgeotiff
  (package
    (name "libgeotiff")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/geotiff/libgeotiff/libgeotiff-"
                           version ".tar.gz"))
       (patches
         (search-patches "libgeotiff-fix-tests-with-proj-9.1.1.patch"
                         "libgeotiff-fix-tests-with-proj-9.3.0.patch"
                         "libgeotiff-fix-tests-with-proj-9.3.1.patch"
                         "libgeotiff-fix-tests-on-i386.patch"))
       (sha256
        (base32 "1mjmgv48x51ppax5dnb6lq7z600czxll53bx6jbzqwd4m93i7aq5"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove .csv files, distributed from EPSG under a restricted
           ;; license. See LICENSE for full license text.
           (for-each delete-file (find-files "." "\\.csv$"))
           #t))))
    (build-system gnu-build-system)
    (inputs
     (list libjpeg-turbo libtiff zlib))
    (propagated-inputs
     (list ;; libgeotiff headers include proj headers, so ensure those are available.
           proj))
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             "--with-zlib" "--with-jpeg"
             (string-append "--with-libtiff=" (assoc-ref %build-inputs "libtiff")))))
    (synopsis "Library for handling GeoTIFF (geographic enabled TIFF)")
    (description "libgeotiff is a library on top of libtiff for reading and
writing GeoTIFF information tags.")
    (home-page "https://trac.osgeo.org/geotiff/")
    ;; This is a mixture of various contributions under different licenses.
    ;; Note that the EPSG database is NOT "free to use" as the LICENSE file
    ;; states, as its commercial redistribution is restricted. Hence, we have
    ;; removed it from the package.
    (license (list license:public-domain
                   license:x11
                   license:bsd-3
                   (license:non-copyleft "file://LICENSE"
                                         "See LICENSE in the distribution.")))))

(define-public mepo
  (package
    (name "mepo")
    (version "1.3.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~mil/mepo")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "025nxkilar3gdif2f1zsiy27614x2hbpcmh38sl61ng37aji0jw4"))))
    (build-system zig-build-system)
    (arguments
     (list #:install-source? #f
           ;; Work around https://github.com/ziglang/zig/issues/17384
           #:zig-build-flags #~(list "--search-prefix" #$curl)
           #:zig-release-type "safe"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'wrap-scripts
                 (lambda* (#:key inputs #:allow-other-keys)
                   (let ((bin-dirs
                          (map (lambda (bin)
                                 (dirname (search-input-file inputs bin)))
                               '("bin/column" ;util-linux
                                 "bin/gpspipe" ;gpsd
                                 "bin/jq"
                                 "bin/xargs" ;busybox
                                 "bin/xwininfo"
                                 "bin/zenity"))))
                     (for-each (lambda (script)
                                 (wrap-program
                                   (string-append #$output "/bin/" script)
                                   `("PATH" ":" prefix ,bin-dirs)))
                       '("mepo_dl.sh"
                         "mepo_generated_osmtags.sh"
                         "mepo_geojson_import.sh"
                         "mepo_ui_central_menu.sh"
                         "mepo_ui_helper_menu.sh"
                         "mepo_ui_helper_pref_pan.sh"
                         "mepo_ui_menu_dbg_queueclear.sh"
                         "mepo_ui_menu_dbg_queuedownloadinteractive.sh"
                         "mepo_ui_menu_dbg_queuedownloadnoninteractive.sh"
                         "mepo_ui_menu_pin_drop.sh"
                         "mepo_ui_menu_pref_fontsize.sh"
                         "mepo_ui_menu_pref_network.sh"
                         "mepo_ui_menu_pref_stateload.sh"
                         "mepo_ui_menu_pref_statesave.sh"
                         "mepo_ui_menu_pref_url.sh"
                         "mepo_ui_menu_pref_zoom.sh"
                         "mepo_ui_menu_reposition_nominatim.sh"
                         "mepo_ui_menu_route_graphhopper.sh"
                         "mepo_ui_menu_route_mobroute.sh"
                         "mepo_ui_menu_search_nominatim.sh"
                         "mepo_ui_menu_search_overpass.sh"
                         "mepo_ui_menu_user_pin_updater.sh"))))))))
    (native-inputs (list pkg-config))
    ;; TODO: package Mobroute
    (inputs (list bash-minimal busybox curl gpsd jq ncurses
                  sdl2 sdl2-gfx sdl2-image sdl2-ttf
                  util-linux xwininfo zenity))
    (home-page "https://mepo.lrdu.org")
    (synopsis "OpenStreetMap map viewer")
    (description
     "Mepo is a fast, simple, and hackable OSM map viewer for desktop and
mobile Linux devices.  It supports Wayland and X Windows.

Mepo works both offline and online, features a minimalist both touch/mouse
and keyboard compatible interface, and offers a simple and powerful JSON API
to allow the user to change and add functionality such as adding their own
search and routing scripts, adding arbitrary buttons/keybindings to the UI,
and more.")
    (license license:gpl3+)))

(define-public librasterlite2
  (package
    (name "librasterlite2")
    (version "1.1.0-beta1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.gaia-gis.it/gaia-sins/librasterlite2-sources/"
             "librasterlite2-" version ".tar.gz"))
       (sha256
        (base32
         "1x24gqp4hsq97c31ncwxblab0x0863q8v1z42jil7lvsq3glqa7p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo
           curl
           freetype
           freexl
           giflib
           libgeotiff
           libjpeg-turbo
           libpng
           librttopo
           libspatialite
           libtiff
           libwebp
           libxml2
           lz4
           minizip
           openjpeg
           proj
           sqlite
           `(,zstd "lib")))
    (synopsis "Library to work with huge raster coverages using a SpatiaLite")
    (description
     "librasterlite2 is a library that stores and retrieves huge raster
coverages using a SpatiaLite DBMS.")
    (home-page "https://www.gaia-gis.it/fossil/librasterlite2/index")
    ;; For the genuine librasterlite-sources holds:
    ;; Any of the licenses MPL1.1, GPL2+ or LGPL2.1+  may be picked.
    ;; Files under src/control_points are from GRASS
    ;; and are licensed under GPL2+ only.
    ;; src/md5.[ch]: Placed into the public domain by Alexander Peslyak.
    ;; The tools supporting the library (both rl2tool and wmslite) are
    ;; licensed under the GPL v3 (or any subsequent version) terms.
    ;; The test/*.svg files are placed in the public domain, except for
    ;; test/Car_Yellow.svg which is licensed under the Free Art License 1.3.
    (license (list license:gpl2+
                   license:gpl3+
                   license:lal1.3
                   license:lgpl2.1+
                   license:mpl1.1
                   license:public-domain))))

(define-public librttopo
  (package
    (name "librttopo")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.osgeo.org/gitea/rttopo/librttopo")
             (commit (string-append "librttopo-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h7lzlkn9g4xky6h81ndy0aa6dxz8wb6rnl8v3987jy1i6pr072p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-autogen
           (lambda _
             (let ((autoconf (which "autoconf"))
                   (autoheader (which "autoheader"))
                   (aclocal (which "aclocal"))
                   (automake (which "automake"))
                   (libtoolize (which "libtoolize")))
               (substitute* "autogen.sh"
                            (("`which autoconf 2>/dev/null`") autoconf)
                            (("`which autoheader 2>/dev/null`") autoheader)
                            (("ACLOCAL=.*$")
                             (string-append "ACLOCAL=" aclocal "\n"))
                            (("AUTOMAKE=.*$")
                             (string-append "AUTOMAKE=" automake "\n"))
                            (("LIBTOOLIZE=.*$")
                             (string-append "LIBTOOLIZE=" libtoolize "\n"))))
             #t)))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list geos))
    (synopsis "Library to handle SQL/MM topologies")
    (description
     "The RT Topology Library exposes an API to create and manage standard
(ISO 13249 aka SQL/MM) topologies using user-provided data stores.")
    (home-page "https://git.osgeo.org/gitea/rttopo/librttopo")
    (license license:gpl2+)))

(define-public libspatialite
  (package
    (name "libspatialite")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                           "libspatialite-sources/libspatialite-"
                           version ".tar.gz"))
       (sha256
        (base32
         "102hc18fvwr1kw8aap53zqi8r0l52b8wa00lvlbf1zys979jvgj3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list freexl
           geos
           librttopo
           libxml2
           minizip
           proj
           sqlite))
    (arguments
     `(#:configure-flags
       '("--enable-rttopo=yes")
       ;; FIXME: Several tests fail with Proj 9.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; 1 test is failing, ignore it:
         (add-after 'unpack 'ignore-broken-tests
           (lambda _
             (substitute* '("test/Makefile.in")
               (("check_wms\\$\\(EXEEXT\\) check_drop_rename\\$\\(EXEEXT\\) ")
                "check_wms$(EXEEXT) ")))))))
    (synopsis "Extend SQLite to support Spatial SQL capabilities")
    (description
     "SpatiaLite is a library intended to extend the SQLite core to support
fully fledged Spatial SQL capabilities.")
    (home-page "https://www.gaia-gis.it/fossil/libspatialite/index")
    ;; For the genuine libspatialite-sources holds:
    ;; Any of the licenses MPL1.1, GPL2+ or LGPL2.1+  may be picked.
    ;; Files under src/control_points are from GRASS
    ;; and are licensed under GPL2+ only.
    ;; src/md5.[ch]: Placed into the public domain by Alexander Peslyak.
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1
                   license:public-domain))))

(define-public proj
  (package
    (name "proj")
    (version "9.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/proj/proj-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1g0hkpiablvhsmw0kn5frwgdir3q7avc45p6lc1zhhhzkv5ikydh"))))
    (build-system cmake-build-system)
    (native-inputs (list googletest pkg-config))
    (propagated-inputs (list curl libtiff sqlite)) ;required by proj.pc
    (home-page "https://proj.org/")
    (synopsis "Coordinate transformation software")
    (description
     "Proj is a generic coordinate transformation software that transforms
geospatial coordinates from one @acronym{CRS, coordinate reference system}
to another.  This includes cartographic projections as well as geodetic
transformations.  Proj includes command line applications for easy
conversion of coordinates from text files or directly from user input.
In addition, Proj also exposes an application programming interface that
lets developers use the functionality of Proj in their own software.")
    (license (list license:expat
                   ;; src/projections/patterson.cpp
                   license:asl2.0
                   ;; src/geodesic.*, src/tests/geodtest.cpp
                   license:x11))))

; This is the last version of proj that provides the old proj.4 API.
(define-public proj-7
  (package (inherit proj)
    (version "7.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://download.osgeo.org/proj/proj-"
                           version ".tar.gz"))
       (sha256
        (base32
         "050apzdn0isxpsblys1shrl9ccli5vd32kgswlgx1imrbwpg915k"))
       (patches
        (search-patches "proj-7-initialize-memory.patch"))))
    (arguments
     `(#:configure-flags '("-DUSE_EXTERNAL_GTEST=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-version
           (lambda _
             (substitute* "CMakeLists.txt"
               (("MAJOR 7 MINOR 2 PATCH 0") "MAJOR 7 MINOR 2 PATCH 1")))))))))

(define-public proj.4
  (package
    (name "proj.4")
    (version "4.9.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.osgeo.org/proj/proj-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xw5f427xk9p2nbsj04j6m5zyjlyd66sbvl2bkg8hd1kx8pm9139"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-test-paths
           (lambda _
             (substitute* '("nad/test27"
                            "nad/test83"
                            "nad/testvarious"
                            "nad/testdatumfile"
                            "nad/testflaky"
                            "nad/testIGNF")
               (("/bin/rm") (which "rm")))
             #t))
         ;; Precision problems on i686 and other platforms. See:
         ;; https://web.archive.org/web/20151006134301/http://trac.osgeo.org/proj/ticket/255
         ;; Disable failing test.
         (add-after 'patch-test-paths 'ignore-failing-tests
           (lambda _
             (substitute* '("nad/Makefile.in")
               (("\tPROJ_LIB.*" all) (string-append  "#" all)))
             #t)))))
    (inputs
     (list glib))
    (home-page "https://proj.org/")
    (synopsis "Cartographic Projections Library")
    (description
     "Proj.4 is a library for converting coordinates between cartographic
projections.")
    (license (list license:expat
                   ;; src/PJ_patterson.c
                   license:asl2.0
                   ;; src/geodesic.c/h
                   license:x11
                   ;; Embedded EPSG database.
                   (license:non-copyleft "http://www.epsg.org/TermsOfUse")
                   ;; cmake/*
                   license:boost1.0))))

(define-public python-pyogrio
  (package
    (name "python-pyogrio")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyogrio" version))
       (sha256
        (base32 "0g5j3a2n5hdnmi45261y84rqk1bikcvrdblgh9wfhk9jd2siq1gc"))))
    (properties
     `((updater-extra-inputs . ("gdal"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests need Internet access.
      '(list "-k" (string-append "not test_url"
                                 " and not test_url_with_zip"
                                 " and not test_uri_s3"))
      #:phases
      '(modify-phases %standard-phases
         (add-before 'check 'build-extensions
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs (list python-certifi python-numpy python-packaging))
    (inputs (list gdal))
    (native-inputs (list python-cython-3
                         python-pytest
                         python-pytest-cov
                         python-setuptools
                         python-tomli
                         python-versioneer
                         python-wheel))
    (home-page "https://pypi.org/project/pyogrio/")
    (synopsis "Vectorized spatial vector file format I/O using GDAL/OGR")
    (description "Pyogrio provides a GeoPandas-oriented API to OGR vector data
sources, such as ESRI Shapefile, GeoPackage, and GeoJSON.  Vector data sources
have geometries, such as points, lines, or polygons, and associated records
with potentially many columns worth of data.  Pyogrio uses a vectorized
approach for reading and writing GeoDataFrames to and from OGR vector data
sources in order to give you faster interoperability.  It uses pre-compiled
bindings for GDAL/OGR so that the performance is primarily limited by the
underlying I/O speed of data source drivers in GDAL/OGR rather than multiple
steps of converting to and from Python data types within Python.")
    (license license:expat)))

(define-public python-pyproj
  (package
    (name "python-pyproj")
    (version "3.6.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyproj" version))
        (sha256
          (base32
            "1gq1spm5zdq9k8kl9cb31b9m08ybyrdggfw3sjrqyz9b9iq7raj4"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-proj-path
            (lambda* (#:key #:allow-other-keys)
              (let ((proj #$(this-package-input "proj")))
                (setenv "PROJ_DIR" proj)
                (substitute* "pyproj/datadir.py"
                  (("(internal_datadir = ).*$" all var)
                   (string-append var "Path(\"" proj
                                  "/share/proj\")\n")))))))))
    (inputs
      (list proj))
    (propagated-inputs
      (list python-certifi))
    (native-inputs
      (list python-cython
            python-numpy
            python-pandas
            python-pytest
            python-xarray))
    (home-page "https://github.com/pyproj4/pyproj")
    (synopsis
      "Python interface to PROJ")
    (description
      "This package provides a Python interface to PROJ, a cartographic
projections and coordinate transformations library.")
    (license license:expat)))

(define-public python-fiona
  (package
    (name "python-fiona")
    (version "1.9.4.post1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Fiona" version))
        (sha256
          (base32
            "083120rqc4rrqzgmams0yjd8b1h4p5xm4n9fnxg064ymw3vx6yan"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-local-fiona
           (lambda _
             ; This would otherwise interfere with finding the installed
             ; fiona when running tests.
             (delete-file-recursively "fiona")))
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (add-installed-pythonpath inputs outputs)
             (setenv "GDAL_ENABLE_DEPRECATED_DRIVER_GTM" "YES")
             (when tests?
               (invoke "pytest"
                       "-m" "not network and not wheel"
                       ;; FIXME: Find why the
                       ;;   test_no_append_driver_cannot_append[PCIDSK]
                       ;; test is failing.
                       "-k" "not test_no_append_driver_cannot_append")))))))
    (inputs
      (list gdal))
    (propagated-inputs
      (list python-attrs
            python-certifi
            python-click
            python-click-plugins
            python-cligj
            python-importlib-metadata
            python-six))
    (native-inputs
      (list gdal ; for gdal-config
            python-boto3
            python-cython
            python-pytest python-pytest-cov python-pytz))
    (home-page "https://github.com/Toblerity/Fiona")
    (synopsis
      "Fiona reads and writes spatial data files")
    (description
      "Fiona is GDAL’s neat and nimble vector API for Python programmers.
Fiona is designed to be simple and dependable.  It focuses on reading
and writing data in standard Python IO style and relies upon familiar
Python types and protocols such as files, dictionaries, mappings, and
iterators instead of classes specific to OGR.  Fiona can read and write
real-world data using multi-layered GIS formats and zipped virtual file
systems and integrates readily with other Python GIS packages such as
pyproj, Rtree, and Shapely.")
    (license license:bsd-3)))

(define-public python-geopack
  (package
    (name "python-geopack")
    (version "1.0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "geopack" version))
       (sha256
        (base32 "0mryjp7m4h99qlpvnn40s81sygr73qcv8rkmjp9pcli1gz829kjf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX Reported upstream <https://github.com/tsssss/geopack/issues/21>.
      #:tests? #f))
    (native-inputs
     (list python-pytest python-setuptools python-wheel))
    (propagated-inputs
     (list python-numpy python-scipy))
    (home-page "https://github.com/tsssss/geopack")
    (synopsis "Python version of geopack and Tsyganenko models")
    (description
     "Python version of geopack and Tsyganenko models, compatible with
@code{geopack05} and @code{geopack08}.")
    (license license:expat)))

(define-public python-geopandas
  (package
    (name "python-geopandas")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "geopandas" version))
        (sha256
          (base32
            "1aq8rb1a97n9h0yinrcr6nhfj7gvh8h6wr2ng9dj1225afjp1gxq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:test-flags
       '(list
         ;; Test files are missing
         "--ignore=geopandas/tests/test_overlay.py"
         "--ignore=geopandas/io/tests/test_file.py"
         ;; Number of open figures changed during test
         "-k" "not test_pandas_kind"
         ;; Disable tests that require internet access.
         "-m" "not web")))
    (propagated-inputs
      (list python-numpy
            python-packaging
            python-pandas
            python-pyogrio
            python-pyproj
            python-shapely))
    (native-inputs
      (list python-codecov
            python-pytest
            python-pytest-cov
            python-pytest-xdist
            python-setuptools
            python-wheel))
    (home-page "https://geopandas.org")
    (synopsis "Geographic pandas extensions")
    (description "The goal of GeoPandas is to make working with
geospatial data in Python easier.  It combines the capabilities of
Pandas and Shapely, providing geospatial operations in Pandas and a
high-level interface to multiple geometries to Shapely.  GeoPandas
enables you to easily do operations in Python that would otherwise
require a spatial database such as PostGIS.")
    (license license:bsd-3)))

(define-public python-overpass
  (package
    (name "python-overpass")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "overpass" version))
       (sha256
        (base32 "0l2n01j0vslag8cf3sp7jif0d4ql6i99fvfv2mgc3ajws69aqzr6"))))
    (build-system pyproject-build-system)
    (arguments
     ;; tests disabled, as they require network
     (list #:tests? #f))
    (propagated-inputs (list python-geojson
                             python-requests
                             python-shapely))
    (native-inputs (list python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/mvexel/overpass-api-python-wrapper")
    (synopsis "Python wrapper for the OpenStreetMap Overpass API")
    (description "This package provides python-overpass, a Python wrapper
for the @code{OpenStreetMap} Overpass API.")
    (license license:asl2.0)))

(define-public python-ogr2osm
  (package
    (name "python-ogr2osm")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch) ; no tests data in PyPi package
       (uri
        (git-reference
         (url "https://github.com/roelderickx/ogr2osm/")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hwqnx3cdqxmniydpj1v31kglq1xjsx41d8p10c9j4hg8kb43j80"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; There are tests in git checkout but non of the examples taken from
      ;; GitHub Actions worked for me. Disabling them to be checked later
      ;; <https://github.com/roelderickx/ogr2osm/blob/main/.github/workflows/test.yml>.
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; TODO: Fix provided tests.
                ;; (invoke "cram" "test/basic_usage.t")
                ;; (invoke "cram" "test/osm_output.t")
                ;; (invoke "cram" "test/pbf_output.t")

                ;; Run simple tests to ensure that the command is working.
                (invoke "ogr2osm" "--help")
                (invoke
                 "ogr2osm" "-f" "test/shapefiles/basic_geometries.kml")))))))
    (inputs
     (list gdal))
    (native-inputs
     (list coreutils
           diffutils
           libxml2
           python-cram
           python-setuptools
           python-wheel
           which))
    (propagated-inputs
     (list python-lxml
           python-protobuf))
    (home-page "https://github.com/roelderickx/ogr2osm")
    (synopsis "Convert ogr-readable files like shapefiles into OSM or PDF formats")
    (description
     "@code{ogr2osm} is a tool for converting ogr-readable files into
@acronym{OSM, OpenStreetMap} format.  It supports reading from OGR files like
shapefiles or PostgreSQL database and converts data into @code{osm} or
@code{osm.pbf} formats.  A translation file can be used to manipulate the data
during conversion.")
    (license license:expat)))

(define-public python-osmnx
  (package
    (name "python-osmnx")
    (version "1.9.3")
    (source
     (origin
       ;; Fetch from github as the pypi package is missing the tests dir.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gboeing/osmnx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yi9al6rrc584y24vigi7w52dq9k2l2zgblrj5ajwgk8079k8zsf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags '(list "-k"
                          (string-append
                           ;; The following tests require network access.
                           "not test_stats"
                           " and not test_geocoder"
                           " and not test_osm_xml"
                           " and not test_elevation"
                           " and not test_routing"
                           " and not test_plots"
                           " and not test_find_nearest"
                           " and not test_api_endpoints"
                           " and not test_graph_save_load"
                           " and not test_graph_from_functions"
                           " and not test_features"))))
    (propagated-inputs (list python-folium
                             python-geopandas
                             python-matplotlib
                             python-networkx
                             python-numpy
                             python-pandas
                             python-requests
                             python-shapely))
    (native-inputs (list python-hatchling python-pytest python-setuptools python-wheel))
    (home-page "https://github.com/gboeing/osmnx")
    (synopsis
     "Retrieve, model, analyze, and visualize OpenStreetMap street networks")
    (description
     "OSMnx is a Python library that lets you download geospatial data
from OpenStreetMap and model, project, visualize, and analyze real-world
street networks and any other geospatial geometries.  You can download
and model walkable, drivable, or bikeable urban networks with a single
line of Python code then easily analyze and visualize them.  You can
just as easily download and work with other infrastructure types,
amenities/points of interest, building footprints, elevation data,
street bearings/orientations, and speed/travel time.")
    (license license:expat)))

(define-public mapnik
  ;; There hasn't been a release since early 2021, and it fails to build with
  ;; Boost 1.77+.
  (let ((commit "81103491b467e17218140f50bc0bb9dc8c1f0317")
        (revision "0"))
    (package
      (name "mapnik")
      (version (git-version "3.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/mapnik/mapnik")
               (commit commit)
               ;; TODO: Un-bundle mapbox dependencies (not yet packaged).
               (recursive? #t)))        ;for mapbox dependencies and test data
         (file-name (git-file-name name version))
         (sha256
          (base32 "094nam57bdd5nak88qy33z2p3kjahk3vk2nk56m5jkcr5d3hlnx2"))))
      (build-system qt-build-system)
      (arguments
       (list
        #:cmake cmake                   ;for FIND_PACKAGE_ARGS
        #:configure-flags
        #~(list (string-append "-DCMAKE_CXX_FLAGS=-I"
                               #$(this-package-native-input "catch2")
                               "/include/catch2"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'use-system-catch2
              (lambda _
                (substitute* "test/CMakeLists.txt"
                  (("GIT_TAG.*v2.13.7" all)
                   (string-append all "\n"
                                  "  FIND_PACKAGE_ARGS NAMES Catch2"))
                  (("^include.*Catch2_SOURCE_DIR.*contrib/Catch.cmake.*")
                   "include(Catch)\n"))))
            (add-after 'unpack 'disable-problematic-tests
              (lambda _
                ;; The 'ogr' test fails for unknown reasons.  Mark it as
                ;; expected to fail (see:
                ;; https://github.com/mapnik/mapnik/issues/4329).
                (substitute* "test/unit/datasource/ogr.cpp"
                  (("TEST_CASE\\(\"ogr\"" all)
                   (string-append all ", \"[!shouldfail]\""))))))))
      (native-inputs
       (list catch2
             pkg-config
             postgresql))
      (inputs
       (list boost
             cairo
             freetype
             gdal
             harfbuzz
             icu4c
             libjpeg-turbo
             libpng
             libtiff
             libwebp
             libxml2
             proj
             sqlite
             zlib))
      (home-page "https://mapnik.org/")
      (synopsis "Toolkit for developing mapping applications")
      (description "Mapnik is a toolkit for developing mapping applications.  It
is basically a collection of geographic objects like maps, layers,
datasources, features, and geometries.  At its core is a C++ shared library
providing algorithms and patterns for spatial data access and visualization.
The library does not rely on any specific windowing system and can be deployed
to any server environment.  It is intended to play fair in a multi-threaded
environment and is aimed primarily, but not exclusively, at web-based
development.")
      (license (list license:lgpl2.1+
                     ;; demo/viewer, demo/python/rundemo.py
                     license:gpl2+
                     ;; deps/boost, deps/mapbox, deps/agg/include/agg_conv_offset.h
                     license:boost1.0
                     ;; deps/mapnik/sparsehash
                     license:bsd-3
                     ;; deps/agg
                     (license:non-copyleft "file://deps/agg/copying"))))))

(define-public spatialite-gui
  (package
    (name "spatialite-gui")
    (version "2.1.0-beta1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.gaia-gis.it/gaia-sins/spatialite-gui-sources/"
             "spatialite_gui-" version ".tar.gz"))
       (sha256
        (base32 "0cyv4cycl073p9lnnnglcb72qn71g8h9g5zn4gzw7swcy5nxjj5s"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl
           freexl
           geos
           giflib
           libjpeg-turbo
           librasterlite2
           librttopo
           libspatialite
           libwebp
           libxlsxwriter
           libxml2
           lz4
           minizip
           openjpeg
           postgresql
           proj
           sqlite
           virtualpg
           wxwidgets
           `(,zstd "lib")))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-gui
                    (lambda _
                      ;; Fix for the GUI not showing up.
                      (substitute* "Main.cpp"
                        (("Hide\\(\\);") ""))
                      #t)))))
    (synopsis "Graphical user interface for SpatiaLite")
    (description "Spatialite-gui provides a visual interface for viewing and
 maintaining a spatialite database.  You can easily see the structure of the
 tables and data contents using point and click functions, many of which
 construct common SQL queries, or craft your own SQL queries.")
    (home-page "https://www.gaia-gis.it/fossil/spatialite_gui/index")
    (license license:gpl3+)))

(define-public pdal
  (package
    (name "pdal")
    (version "2.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PDAL/PDAL")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gg5lcshlmn3wwak42xr0b8a8gdr4572d7hrcvxn2291kp2c3096"))))
    (build-system cmake-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "ctest" "-E" ;; This test hangs.
                             "pdal_io_stac_reader_test")))))))
    (native-inputs (list python))
    (inputs (list gdal
                  h3
                  libgeotiff
                  libunwind
                  libxml2
                  nlohmann-json
                  openssl
                  proj
                  utfcpp
                  xz
                  `(,zstd "lib")))
    (home-page "https://pdal.io/")
    (synopsis "Point Data Abstraction Library")
    (description "PDAL is a C++ library for translating and manipulating point
cloud data.  It is very much like the GDAL library which handles raster and
vector data.")
    (license license:bsd-3)))

(define-public gdal
  (package
    (name "gdal")
    (version "3.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/OSGeo/gdal/releases/download/v"
                     version "/gdal-" version ".tar.gz"))
              (sha256
               (base32
                "1ml9l1c4psb1nc760nvs4vbibwf288d4anxip9sisrwl4q6nj579"))
              (modules '((guix build utils)))
              (snippet
               `(begin
                  ;; TODO: frmts contains a lot more bundled code.
                  (for-each delete-file-recursively
                            ;; bundled code
                            '("frmts/png/libpng"
                              "frmts/gif/giflib"
                              "frmts/jpeg/libjpeg"
                              "frmts/jpeg/libjpeg12"
                              "frmts/gtiff/libtiff"
                              "frmts/gtiff/libgeotiff"
                              ;; We need to keep frmts/zlib/contrib/infback9
                              ;; because the cmake file unconditionally
                              ;; depends on it for deflate64.  The infback9.h
                              ;; header file is also referenced in parts of
                              ;; the code.
                              ;;"frmts/zlib"
                              "ogr/ogrsf_frmts/geojson/libjson"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:modules '((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:imported-modules `(,@%cmake-build-system-modules
                           (guix build python-build-system))
      #:configure-flags
      #~(list "-DGDAL_USE_INTERNAL_LIBS=WHEN_NO_EXTERNAL"
              "-DGDAL_USE_JPEG12_INTERNAL=OFF"
              "-DGDAL_USE_LERC_INTERNAL=ON"
              (string-append "-DCMAKE_INSTALL_RPATH=" #$output "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'swap-files
            (lambda _
              ;; The RPATH of the binaries in build/swig/python/build/ is
              ;; rewritten in build/swig/python/for_install/build/.  For
              ;; unknown reasons the files in the former directory are
              ;; installed when it should be those in the latter directory.
              ;; So we copy them ourselves.
              (with-directory-excursion "../build/swig/python/for_install/build"
                (for-each (lambda (file)
                            (install-file file
                                          (string-append
                                           #$output
                                           "/lib/python"
                                           (python:python-version #$(this-package-native-input "python"))
                                           "/site-packages/osgeo")))
                          (find-files "." "\\.so"))))))))
    (inputs
     (list curl
           expat
           freexl
           geos
           giflib
           json-c
           libgeotiff
           libjpeg-turbo
           libjxl
           libpng
           libtiff
           libwebp
           lz4
           netcdf
           openssl
           openjpeg
           pcre2
           postgresql ; libpq
           proj
           qhull
           shapelib
           sqlite
           swig
           zlib
           zstd))
    (native-inputs
     (list pkg-config
           python))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://gdal.org/")
    (synopsis "Raster and vector geospatial data format library")
    (description "GDAL is a translator library for raster and vector geospatial
data formats.  As a library, it presents a single raster abstract data model
and single vector abstract data model to the calling application for all
supported formats.  It also comes with a variety of useful command line
utilities for data translation and processing.")
    (license (list
               ;; general license
               license:expat
               ;; frmts/gtiff/tif_float.c, frmts/pcraster/libcsf,
               ;; ogr/ogrsf_frmts/dxf/intronurbs.cpp, frmts/pdf/pdfdataset.cpp
               ;; frmts/mrf/
               license:bsd-3
               ;; frmts/hdf4/hdf-eos/*
               ;; similar to the expat license, but without guarantee exclusion
               (license:non-copyleft "file://frmts/hdf4/hdf-eos/README")
               ;; frmts/grib/degrib/
               license:public-domain ; with guarantee exclusion
               ;; port/cpl_minizip*
               ;; Some bsd-inspired license
               (license:non-copyleft "file://port/LICENCE_minizip")
               ;; alg/internal_libqhull
               ;; Some 5-clause license
               (license:non-copyleft "file://alg/internal_libqhull/COPYING.txt")
               ;; frmts/mrf/libLERC
               license:asl2.0))))

(define-public python-pyshp
  (package
    (name "python-pyshp")
    (version "2.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GeospatialPython/pyshp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02pbr091p8v4kfv1p6p2aa4asgm9r74dc12r35lvgmhs9y163z69"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This test requires internet access.
      '(list "--deselect" "test_shapefile.py::test_reader_url")))
    (native-inputs
     (list python-pytest
           python-pytest-runner
           python-setuptools
           python-wheel))
    (home-page "https://github.com/GeospatialPython/pyshp")
    (synopsis "Read/write support for ESRI Shapefile format")
    (description
      "The Python Shapefile Library (PyShp) reads and writes ESRI Shapefiles.")
    (license license:expat)))

(define-public python-verde
  (package
    (name "python-verde")
    (version "1.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "verde" version))
       (sha256
        (base32 "1hnh91dsk2dxfbk7p2hv3hajaa396139pd6apabgdrp5b7s54k97"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Tests below fetch data remotely.
     (list #:test-flags #~(list "-k" (string-append
                                      "not test_minimal_integration_2d_gps"
                                      " and not test_datasets_locate"
                                      " and not test_fetch_texas_wind"
                                      " and not test_fetch_baja_bathymetry"
                                      " and not test_fetch_rio_magnetic"
                                      " and not test_fetch_california_gps"))))
    (native-inputs (list python-cartopy python-distributed python-pytest
                         python-setuptools python-wheel))
    (propagated-inputs (list python-dask
                             python-numpy
                             python-pandas
                             python-pooch
                             python-scikit-learn
                             python-scipy
                             python-xarray))
    (home-page "https://github.com/fatiando/verde")
    (synopsis "Processing and gridding spatial data, machine-learning style")
    (description
     "Verde is a Python library for processing spatial data (topography, point
clouds, bathymetry, geophysics surveys, etc) and interpolating them on a 2D
surface (i.e., gridding) with a hint of machine learning.")
    (license license:bsd-3)))

(define-public python-cartopy
  (package
    (name "python-cartopy")
    (version "0.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cartopy" version))
       (sha256
        (base32 "1gf8hpjlhjsw1gfd80ghcy3k5lkkshbhlvn4vvpsfsaccgai1j81"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         "--pyargs" "cartopy"
         "--numprocesses" (number->string (parallel-job-count))
         ;; These tests require online data.
         "-m" "not natural_earth and not network"
         ;; Failed: Error: Image files did not match.
         "-k" "not test_gridliner_constrained_adjust_datalim")
      #:phases
      #~(modify-phases %standard-phases
          ;; We don't want to create an entrypoint for
          ;; tools/cartopy_feature_download.py, because that file is not
          ;; installed.
          (add-after 'unpack 'remove-endpoint
            (lambda _
              (substitute* "pyproject.toml"
                (("^feature_download = .*") "")))))))
    (propagated-inputs
     (list python-matplotlib
           python-fiona     ; optional [speedups]
           python-numpy
           python-owslib    ; optional [ows]
           python-packaging
           python-pillow    ; optional [ows, plotting]
           python-pykdtree  ; optional [speedups]
           python-pyproj
           python-pyshp
           python-scipy     ; optional [plotting]
           python-shapely))
    (inputs
     (list geos))
    (native-inputs
     (list python-cython
           python-pytest
           python-pytest-mpl
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (home-page "https://scitools.org.uk/cartopy/docs/latest/")
    (synopsis "Cartographic library for visualisation")
    (description
     "Cartopy is a Python package designed to make drawing maps for data
analysis and visualisation easy.

It features:

@itemize
@item object oriented projection definitions
@item point, line, polygon and image transformations between projections
@item integration to expose advanced mapping in Matplotlib with a simple and
intuitive interface
@item powerful vector data handling by integrating shapefile reading with
Shapely capabilities
@end itemize")
    (license license:lgpl3+)))

(define-public postgis
  (package
    (name "postgis")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.osgeo.org/postgis/source/postgis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gl9d6xy2an82ldb9sixz5blyngjryq8m3509fr38ffawvfniazv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "datadir=" (assoc-ref %outputs "out") "/share")
             (string-append "docdir="(assoc-ref %outputs "out") "/share/doc")
             (string-append "pkglibdir="(assoc-ref %outputs "out") "/lib")
             (string-append "bindir=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("raster/loader/Makefile" "raster/scripts/python/Makefile")
               (("\\$\\(DESTDIR\\)\\$\\(PGSQL_BINDIR\\)")
                (string-append (assoc-ref outputs "out") "/bin"))))))))
    (inputs
     (list gdal
           geos
           giflib
           json-c
           libjpeg-turbo
           libxml2
           openssl
           pcre
           postgresql
           protobuf-c
           proj))
    (native-inputs
     (list perl pkg-config))
    (home-page "https://postgis.net")
    (synopsis "Spatial database extender for PostgreSQL")
    (description "PostGIS is a spatial database extender for PostgreSQL
object-relational database.  It adds support for geographic objects allowing
location queries to be run in SQL.  This package provides a PostgreSQL
extension.")
    (license (list
               ;; General license
               license:gpl2+
               ;; loader/dbfopen, safileio.*, shapefil.h, shpopen.c
               license:expat
               ;; loader/getopt.*
               license:public-domain
               ;; doc/xsl
               license:bsd-3 ; files only say "BSD"
               ;; doc
               license:cc-by-sa3.0))))

(define-public python-cf-units
  (package
    (name "python-cf-units")
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cf_units" version))
       (sha256
        (base32 "1nqzlrzxwhvm7z2pl70bwlr37fz95hcm0n8v7y503krh5x4xl9r3"))))
    (build-system pyproject-build-system)
    (arguments
     (list 
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'udunits-path
            (lambda _
              (setenv "UDUNITS2_XML_PATH"
                      (format #f "~a/share/udunits/udunits2.xml"
                              #$(this-package-input "udunits")))))
          (replace 'check
          ;; To load built module and bypath error: ImportError: cannot import
          ;; name '_udunits2' from partially initialized module 'cf_units'.
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (with-directory-excursion #$output
                (apply invoke "pytest" "-vv" test-flags)))))))
    (native-inputs
     (list python-cython-3
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (inputs
     (list udunits))
    (propagated-inputs
     (list java-antlr4-runtime-python
           python-cftime
           python-jinja2
           python-numpy))
    (home-page "https://github.com/SciTools/cf-units")
    (synopsis "Units of measure as required by the CF metadata conventions")
    (description
     "This package provides units of measure as required by the Climate and
Forecast (CF) metadata conventions.  Provision of a wrapper class to support
Unidata/UCAR UDUNITS-2 library, and the cftime calendar functionality.")
    (license license:lgpl3+)))

(define-public tegola
  (package
    (name "tegola")
    (version "0.21.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/go-spatial/tegola")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1f5l7b372dfyibawhcnlz700z11a0dxqd7kr731nwfmhwz2v3438"))
              (modules '((guix build utils)))
              ;; TODO: Unbundle more when missing packages are available.
              (snippet #~(with-directory-excursion "vendor"
                           (for-each delete-file-recursively
                                     '("github.com/aws"
                                       "github.com/beorn7"
                                       "github.com/BurntSushi"
                                       "github.com/gofrs"
                                       "github.com/golang/protobuf"
                                       "github.com/google"
                                       "github.com/go-test"
                                       "github.com/jmespath"
                                       "github.com/mattn/go-sqlite3"
                                       "github.com/spf13"
                                       "golang.org/x/crypto"
                                       "golang.org/x/sys"
                                       "golang.org/x/text"
                                       "golang.org/x/tools"
                                       "google.golang.org/protobuf"
                                       "go.uber.org"))))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-spatial/tegola/cmd/tegola"
       #:unpack-path "github.com/go-spatial/tegola"
       #:build-flags '(,(string-append "\
-ldflags=-X github.com/go-spatial/tegola/internal/build.Version=" version))
       #:install-source? #f))
    (inputs
     (list go-github-com-aws-aws-lambda-go
           go-github-com-aws-aws-sdk-go
           go-github-com-beorn7-perks
           go-github-com-burntsushi-toml
           go-github-com-gofrs-uuid
           go-github-com-golang-protobuf
           go-github-com-google-uuid
           go-github-com-go-test-deep
           go-github-com-jmespath-go-jmespath
           go-github-com-mattn-go-sqlite3
           go-github-com-spf13-pflag
           go-golang-org-x-crypto
           go-golang-org-x-sys
           go-golang-org-x-text
           go-golang-org-x-tools
           go-google-golang-org-protobuf
           go-go-uber-org-atomic
           go-go-uber-org-multierr
           go-go-uber-org-zap))
    (home-page "https://tegola.io")
    (synopsis "Vector tile server for maps")
    (description "Tegola is a free vector tile server written in Go.  Tegola
takes geospatial data and slices it into vector tiles that can be efficiently
delivered to any client.")
    (license (list
               license:expat
               ;; Some packages in vendor have other licenses
               license:asl2.0
               license:bsd-2
               license:bsd-3
               license:wtfpl2))))

(define-public imposm3
  (package
    (name "imposm3")
    (version "0.11.1")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/omniscale/imposm3")
              (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1ifniw57l3s0sl7nb3zwxxm86i46451yrhfqnnkxr46cnpbzmwxr"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/omniscale/imposm3/cmd/imposm"
       #:unpack-path "github.com/omniscale/imposm3"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-version
           (lambda _
             (substitute* "src/github.com/omniscale/imposm3/version.go"
               (("0.0.0-dev") ,version))
             #t)))))
    (inputs
     (list geos leveldb))
    (home-page "https://imposm.org/")
    (synopsis "OpenStreetMap importer for PostGIS")
    (description "Imposm is an importer for OpenStreetMap data.  It reads PBF
files and imports the data into PostgreSQL/PostGIS databases.  It is designed
to create databases that are optimized for rendering/tile/map-services.")
    (license (list
               license:asl2.0
               ;; Some dependencies in vendor have different licenses
               license:expat
               license:bsd-2
               license:bsd-3))))

(define-public python-metpy
  (package
    (name "python-metpy")
    (version "1.6.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "metpy" version))
              (sha256
               (base32
                "0rh7lslwf79sgbf0933pz6mxchbrb0434pbdzqgzs1kjlsli9pr3"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Too many of the tests in the files below require online data.
     (list
      #:test-flags
      #~(list "--ignore" "tests/io/test_nexrad.py"
              "--ignore" "tests/plots/test_declarative.py"
              "--ignore" "tests/io/test_gempak.py"
              "--ignore" "tests/io/test_gini.py"
              "--ignore" "tests/io/test_metar.py"
              "--ignore" "tests/io/test_station_data.py"
              "--ignore" "tests/interpolate/test_grid.py"
              "--ignore" "tests/interpolate/test_points.py"
              "--ignore" "tests/test_xarray.py"
              "--ignore" "tests/calc/test_indices.py"
              "--ignore" "tests/calc/test_kinematics.py"
              "-k" (string-append       ; more tests that require online data
                    "not test_parse_grid_arguments_xarray"
                    " and not test_absolute_momentum_xarray_units_attr"
                    " and not test_zoom_xarray"
                    " and not test_parse_wpc_surface_bulletin"
                    " and not test_add_timestamp_xarray"
                    " and not test_parse_wpc_surface_bulletin_highres"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'fix-version-check
           (lambda _
             (substitute* "src/metpy/testing.py"
               (("^( +)match = pattern.*" m indent)
                (string-append indent "\
version_spec = re.sub('[()]', '', version_spec)\n" m)))))
         ;; The deprecation warning from python-future's use of imp breaks the
         ;; tests.
         (add-after 'unpack 'hide-imp-deprecation-warnings
           (lambda _
             (substitute* "pyproject.toml"
               (("\"ignore:numpy.ndarray size changed:RuntimeWarning\"," m)
                (string-append m "
\"ignore:the imp module is deprecated\","))))))))
    (propagated-inputs (list python-importlib-resources
                             python-matplotlib
                             python-numpy
                             python-pandas
                             python-pint
                             python-pooch
                             python-pyproj
                             python-scipy
                             python-traitlets
                             python-xarray))
    (native-inputs
     (list python-netcdf4
           python-packaging
           python-pytest
           python-pytest-mpl
           python-setuptools
           python-wheel))
    (home-page "https://github.com/Unidata/MetPy")
    (synopsis "Collection of tools to deal with weather data")
    (description "MetPy is a collection of tools in Python for reading,
visualizing and performing calculations with weather data.")
    (license license:bsd-3)))

(define-public libosmium
  (package
    (name "libosmium")
    (version "2.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/osmcode/libosmium")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d69xzd29hk846g049y2g668mr8kaf05f6a26s3qn6az062hxfa7"))))
    (build-system cmake-build-system)
    (propagated-inputs (list boost
                             bzip2
                             expat
                             gdal
                             geos
                             lz4
                             proj-7
                             protozero
                             zlib))
    (native-inputs (list doxygen graphviz-minimal))
    (home-page "https://osmcode.org/libosmium/")
    (synopsis "C++ library for working with OpenStreetMap data")
    (description
     "Libosmium is a fast and flexible C++ library for working with
OpenStreetMap data.")
    (license license:boost1.0)))

(define-public osmium-tool
  (package
    (name "osmium-tool")
    (version "1.15.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/osmcode/osmium-tool")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d90vz316xdl3c416nicgdw7ybw17l2125wgxglbzl7jaqngapy5"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled libraries.
               '(delete-file-recursively "include/rapidjson"))))
    (build-system cmake-build-system)
    (inputs (list libosmium rapidjson))
    (native-inputs (list pandoc))
    (home-page "https://osmcode.org/osmium-tool/")
    (synopsis "Osmium command-line tool")
    (description "Command line tool for working with OpenStreetMap data
based on the Osmium library.")
    (license license:gpl3+)))

(define-public osm2pgsql
  (package
    (name "osm2pgsql")
    (version "1.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstreetmap/osm2pgsql")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "135vqahlcrhwa3b9hfgbiqkzbbsjd4i79fp41cd0rp4zarcpi47p"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled libraries.
               '(delete-file-recursively "contrib"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f ;tests fail because we need to setup a database
           #:configure-flags #~(list "-DUSE_PROJ_LIB=6" ;use API version 6
                                     "-DWITH_LUAJIT=ON"
                                     "-DEXTERNAL_LIBOSMIUM=ON"
                                     "-DEXTERNAL_PROTOZERO=ON"
                                     "-DEXTERNAL_FMT=ON")))
    (inputs (list boost
                  bzip2
                  cli11
                  expat
                  fmt-8
                  libosmium
                  luajit
                  nlohmann-json
                  postgresql
                  proj
                  protozero
                  zlib))
    (native-inputs (list pandoc python python-argparse-manpage))
    (home-page "https://osm2pgsql.org/")
    (synopsis "OSM data importer to PostgreSQL")
    (description
     "Osm2pgsql is a tool for loading OpenStreetMap data into a
PostgreSQL / PostGIS database suitable for applications like rendering into a
map, geocoding with Nominatim, or general analysis.")
    (license license:gpl2+)))

(define-public tippecanoe
  (package
    (name "tippecanoe")
    (version "2.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/felt/tippecanoe")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q2snvsbs10l9pjydid3zxkidlha5hav8gvb0p731m2pwg3xw0qr"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:test-target "test"
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (inputs
     (list perl sqlite zlib))
    (home-page "https://github.com/mapbox/tippecanoe")
    (synopsis "Vector tile server for maps")
    (description "Tippecanoe creates scale-independent view of data, so that
the texture and density of features is visible at every zoom level, instead of
dropping features at lower levels.")
    (license license:bsd-2)))

(define-public osmctools
  (package
    (name "osmctools")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/osm-c-tools/osmctools")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1m8d3r1q1v05pkr8k9czrmb4xjszw6hvgsf3kn9pf0v14gpn4r8f"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (inputs
     (list zlib))
    (home-page "https://gitlab.com/osm-c-tools/osmctools")
    (synopsis "Tools to convert, filter and update OpenStreetMap data files")
    (description "This project contains a few tools which are used in the
OpenStreetMap project.  They can be used to convert, filter and update
OpenStreetMap data files.")
    (license license:agpl3)))

(define-public osm-gps-map
  (package
    (name "osm-gps-map")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/nzjrs/osm-gps-map/releases/download/"
             version "/osm-gps-map-" version ".tar.gz"))
       (sha256
        (base32
         "11imsf4cz1dpxdjh178k2s29axmq86rkfg1pqmn7incyxmjzhbwg"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnome-common gtk-doc/stable pkg-config))
    (inputs
     (list cairo glib gobject-introspection gtk+ libsoup-minimal-2))
    (home-page "https://nzjrs.github.io/osm-gps-map/")
    (synopsis "GTK+ widget for displaying OpenStreetMap tiles")
    (description
     "This package provides a GTK+ widget (and Python bindings) that when
given GPS coordinates,draws a GPS track, and points of interest on a moving
map display.  Downloads map data from a number of websites, including
@url{https://www.openstreetmap.org}.")
    (license license:gpl2+)))

(define-public xygrib
  (package
    (name "xygrib")
    (version "1.2.6.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/opengribs/XyGrib")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xzsm8pr0zjk3f8j880fg5n82jyxn8xf1330qmmq1fqv7rsrg9ia"))
              (patches (search-patches "xygrib-fix-finding-data.patch"
                                       "xygrib-newer-proj.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "data/fonts")
                  ;; Fixes compilation, can be removed with the next release.
                  ;; Upstream link: https://github.com/opengribs/XyGrib/pull/255
                  (substitute* "src/SkewT.h"
                    (("QMessageBox>") "QMessageBox>\n#include <QPainterPath>"))
                  ;; Accept newer versions of openjpeg
                  ;; https://github.com/opengribs/XyGrib/pull/298
                  (substitute* "CMakeLists.txt"
                    (("openjpeg-2.4") "openjpeg-2.5 openjpeg-2.4"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DGNU_PACKAGE=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-directories
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((jpeg (assoc-ref inputs "openjpeg"))
                   (font (assoc-ref inputs "font-liberation")))
               (substitute* "CMakeLists.txt"
                 ;; Skip looking for the static library.
                 (("\"libnova.a\"") ""))
               ;; Don't use the bundled font-liberation.
               (substitute* "src/util/Font.cpp"
                 (("Util::pathFonts\\(\\)\\+\"liberation-fonts/\"")
                  (string-append "\"" font "/share/fonts/truetype/\"")))
               (substitute* "src/util/Util.h"
                 (("pathData\\(\\)\\+\"data/fonts/\"")
                  (string-append "\"" font "/share/fonts/\"")))))))
       #:tests? #f)) ; no tests
    (native-inputs
     (list qttools-5))
    (inputs
     (list bzip2
           font-liberation
           libnova
           libpng
           openjpeg
           proj
           qtbase-5
           zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (synopsis "Weather Forecast Visualization")
    (description
     "XyGrib is a Grib file reader and visualizes meteorological data providing
an off-line capability to analyse weather forecasts or hindcasts.  It is
intended to be used as a capable weather work station for anyone with a serious
interest in examining weather.  This would include members of the sailing
community, private and sport aviators, farmers, weather buffs and many more.
XyGrib is the continuation of the zyGrib software package with a new team of
volunteers.")
    (home-page "https://opengribs.org")
    (license license:gpl3+)))

(define-public libspatialindex
  (package
    (name "libspatialindex")
    (version "1.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libspatialindex/libspatialindex/"
                           "releases/download/" version "/spatialindex-src-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "02n5vjcyk04w0djidyp21hfbxfpbbara8ifd9nml6158rwqr8lja"))))
    (build-system cmake-build-system)
    (home-page "https://libspatialindex.org")
    (synopsis "Spatial indexing library")
    (description "The purpose of this library is to provide:

@itemize
 @item An extensible framework that will support robust spatial indexing
methods.
 @item Support for sophisticated spatial queries.  Range, point location,
 nearest neighbor and k-nearest neighbor as well as parametric queries (defined
by spatial constraints) should be easy to deploy and run.
 @item Easy to use interfaces for inserting, deleting and updating information.
 @item Wide variety of customization capabilities.  Basic index and storage
characteristics like the page size, node capacity, minimum fan-out, splitting
algorithm, etc. should be easy to customize.
 @item Index persistence.  Internal memory and external memory structures
should be supported.  Clustered and non-clustered indices should be easy to be
persisted.
@end itemize
")
    (license license:expat)))

(define-public libmseed
  (package
    (name "libmseed")
    (version "3.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/EarthScope/libmseed")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05sk2h19c7ja98s75b7hbn2cwnjc5l6dr59c23fgnaimmad2rfn7"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (home-page "https://earthscope.github.io/libmseed/")
    (synopsis "Library for the miniSEED data format")
    (description "The miniSEED library provides a framework for manipulation
of SEED data records, a format for commonly used for seismological time
series and related data.  The library includes the functionality to read
and write data records, in addition to reconstructing time series
from multiple records.")
    (license license:asl2.0)))

(define-public python-rtree
  (package
    (name "python-rtree")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Rtree" version))
       (sha256
        (base32 "0aalh07fyf6vpr0a6zswnqvvrjhyic1zg6w4bl368fihkilj2892"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-libspatialindex
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((libspatialindex (assoc-ref inputs "libspatialindex")))
               (substitute* "rtree/finder.py"
                 (("find_library\\(\"spatialindex_c\"\\)")
                  (string-append  "\"" libspatialindex
                                  "/lib/libspatialindex_c.so\"")))))))))
    (native-inputs
     (list python-numpy python-pytest python-setuptools python-wheel))
    (inputs
     (list libspatialindex))
    (home-page "https://github.com/Toblerity/rtree")
    (synopsis "R-Tree spatial index for Python GIS")
    (description
     "RTree is a Python package with bindings for @code{libspatialindex}.")
    (license license:expat)))

(define-public python-scitools-iris
  (package
    (name "python-scitools-iris")
    (version "3.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scitools_iris" version))
       (sha256
        (base32 "19qwmi7amr8q2dd17scch4zvdly3vqc23v5zwpq8qw45vmldxfrq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "--pyargs" "iris"
              "-k" (string-join
                    ;; Tests requiring additional data files distributed
                    ;; separately from this project from
                    ;; <https://github.com/SciTools/iris-test-data> or network
                    ;; access.
                    ;;
                    ;; XXX: Review this list and try to ignore by files
                    ;; instead in individual tests.
                    (list "not test_both_transposed"
                          "test_2d_coords_contour"
                          "test_2d_plain_latlon"
                          "test_2d_plain_latlon_on_polar_map"
                          "test_2d_rotated_latlon"
                          "test_2d_rotated_latlon"
                          "test_ancillary_variables_pass_0"
                          "test_auxiliary_coordinates_pass_0"
                          "test_bounds"
                          "test_bounds_pass_0"
                          "test_broadcast_cubes"
                          "test_broadcast_cubes_weighted"
                          "test_broadcast_transpose_cubes_weighted"
                          "test_cell_measures_pass_0"
                          "test_common_mask_broadcast"
                          "test_common_mask_simple"
                          "test_compatible_cubes"
                          "test_compatible_cubes_weighted"
                          "test_coord_transposed"
                          "test_coordinates_pass_0"
                          "test_cube_transposed"
                          "test_data_pass_0"
                          "test_destructor"
                          "test_formula_terms_pass_0"
                          "test_global_attributes_pass_0"
                          "test_grid_mapping_pass_0"
                          "test_grouped_dim"
                          "test_incompatible_cubes"
                          "test_izip_different_shaped_coords"
                          "test_izip_different_valued_coords"
                          "test_izip_extra_coords_both_slice_dims"
                          "test_izip_extra_coords_slice_dim"
                          "test_izip_extra_coords_step_dim"
                          "test_izip_extra_dim"
                          "test_izip_input_collections"
                          "test_izip_missing_slice_coords"
                          "test_izip_nd_non_ortho"
                          "test_izip_nd_ortho"
                          "test_izip_no_args"
                          "test_izip_no_common_coords_on_step_dim"
                          "test_izip_onecube_height_lat_long"
                          "test_izip_onecube_lat"
                          "test_izip_onecube_lat_lon"
                          "test_izip_onecube_no_coords"
                          "test_izip_ordered"
                          "test_izip_returns_iterable"
                          "test_izip_same_cube_lat"
                          "test_izip_same_cube_lat_lon"
                          "test_izip_same_cube_no_coords"
                          "test_izip_same_dims_single_coord"
                          "test_izip_same_dims_two_coords"
                          "test_izip_subcube_of_same"
                          "test_izip_unequal_slice_coords"
                          "test_izip_use_in_analysis"
                          "test_label_dim_end"
                          "test_label_dim_start"
                          "test_lenient_handling"
                          "test_load_pp_timevarying_orography"
                          "test_mdtol"
                          "test_netcdf_v3"
                          "test_no_delayed_writes"
                          "test_no_transpose"
                          "test_non_existent_coord"
                          "test_perfect_corr"
                          "test_perfect_corr_all_dims"
                          "test_python_versions"
                          "test_realfile_loadsave_equivalence"
                          "test_realise_data"
                          "test_realise_data_multisource"
                          "test_scheduler_types"
                          "test_single_coord"
                          "test_stream"
                          "test_stream_multisource"
                          "test_stream_multisource__manychunks"
                          "test_time_of_writing"
                          "test_ungrouped_dim"
                          "test_variable_attribute_touch_pass_0"
                          "test_variable_cf_group_pass_0"
                          "test_weight_error")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; GeoVista is not packaged yet, "--ignore" option did not work to
          ;; skip test files.
          (add-after 'unpack 'delete-failing-test-files
            (lambda _
              (with-directory-excursion "lib/iris/tests"
                (for-each delete-file-recursively
                          (list "integration/experimental/geovista"
                                "unit/experimental/geovista")))))
          (add-after 'unpack 'fix-paths
            (lambda _
              (let ((netcdf #$(this-package-native-input "netcdf")))
                (substitute* (list "lib/iris/tests/__init__.py"
                                   "lib/iris/tests/stock/netcdf.py")
                  (("env_bin_path\\(\"ncgen\"\\)")
                   (format #f "'~a/bin/ncgen'" netcdf))
                  (("env_bin_path\\(\"ncdump\"\\)")
                   (format #f "'~a/bin/ncdump'" netcdf))))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list netcdf ; for ncdump and ncgen
           nss-certs-for-test
           python-distributed
           python-filelock
           python-imagehash
           python-pytest-xdist
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-cartopy
           python-cf-units
           python-cftime
           python-dask
           ;; python-geovista ; optinoal, not packaged yet
           python-matplotlib
           python-netcdf4
           python-numpy
           python-pyproj
           python-scipy
           python-shapely
           python-xxhash))
    (home-page "https://github.com/SciTools/iris")
    (synopsis "Earth science data analysing and visualising library")
    (description
     "Iris is a Python library for analysing and visualising Earth science
data.  It excels when working with multi-dimensional Earth Science data, where
tabular representations become unwieldy and inefficient.  Iris implements a
data model based on the CF conventions.")
    (license license:lgpl3+)))

(define-public java-jmapviewer
  (package
    (name "java-jmapviewer")
    (version "2.22")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://josm.openstreetmap.de/osmsvn/"
                                  "/applications/viewer/jmapviewer/releases/"
                                  version "/JMapViewer-" version "-Source.zip"))
              (sha256
               (base32
                "0lq82yyancaswjb9fammjl1jicvdbijjz86bz94dy2v9zm62zq5b"))))
    (build-system ant-build-system)
    (native-inputs
     (list unzip))
    (arguments
     `(#:build-target "pack"
       #:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'clean
           (lambda* _
             (invoke "ant" "clean")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java/")))
               (mkdir-p dir)
               (copy-file "JMapViewer.jar" (string-append dir "JMapViewer.jar"))
               #t))))))
    (home-page "https://wiki.openstreetmap.org/wiki/JMapViewer")
    (synopsis "OSM map integration in Java")
    (description "JMapViewer is a Java component which easily
integrates an OSM map view into your Java application.  It is maintained as
an independent project by the JOSM team.")
    (license license:gpl2)))

(define-public java-opening-hours-parser
  (package
    (name "java-opening-hours-parser")
    (version "0.27.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/simonpoole/OpeningHoursParser")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sw5ccxqw4ly5hzxnnljjqx4876gyvagi10sg8r9w25n211lq0x4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-opening-hours-parser.jar"
       #:source-dir "src/main/java"
       #:test-exclude (list "**/IndividualTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'build 'generate-parser
           (lambda* _
             (let* ((dir "src/main/java/ch/poole/openinghoursparser")
                    (file (string-append dir "/OpeningHoursParser.jj")))
               (invoke "javacc" "-DEBUG_PARSER=false"
                       "-DEBUG_TOKEN_MANAGER=false" "-JDK_VERSION=1.8"
                       "-GRAMMAR_ENCODING=UTF-8"
                       (string-append "-OUTPUT_DIRECTORY=" dir)
                       file)))))))
    (inputs
     (list java-jetbrains-annotations))
    (native-inputs
     (list javacc java-junit java-hamcrest-core))
    (home-page "https://github.com/simonpoole/OpeningHoursParser")
    (synopsis "Java parser for the OpenStreetMap opening hour format")
    (description "This is a very simplistic parser for string values according
to the OSM opening hours specification.")
    (license license:expat)))

(define-public josm
  (package
    (name "josm")
    (version "19253")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                     (url "https://josm.openstreetmap.de/svn/trunk")
                     (revision (string->number version))
                     (recursive? #f)))
              (sha256
               (base32
                "1k5v591mkh0xkyfj66qmv1mamqsqw347nhax5hlwyg8hgfk7a6xr"))
              (file-name (string-append name "-" version "-checkout"))
              (modules '((guix build utils)))
            (snippet
             '(begin
                (for-each delete-file (find-files "." ".*.jar$"))
                #t))))
    (build-system ant-build-system)
    (native-inputs
     (list javacc))
    (inputs
     (list java-commons-jcs
           java-commons-compress
           java-jmapviewer
           java-jakarta-annotations-api
           java-jakarta-json
           java-jsr305
           java-metadata-extractor
           java-opening-hours-parser
           java-openjfx-media
           java-parsson ; runtime dependency
           java-signpost-core
           java-svg-salamander
           openjdk11))
    (arguments
     `(#:tests? #f
       #:jar-name "josm.jar"
       #:jdk ,openjdk11
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'rm-build.xml
           (lambda* _
             (delete-file "build.xml")
             #t))
         (add-before 'build 'fix-revision
           (lambda* _
             (with-output-to-file "REVISION.XML"
               (lambda _
                 (display
                   (string-append "<info><entry><commit revision=\"" ,version "\">"
                                  "<date>1970-01-01 00:00:00 +0000</date>"
                                  "</commit></entry></info>"))))
             #t))
         (add-before 'build 'fix-classpath
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH"
                     (string-join
                       (filter
                         (lambda (s)
                           (let ((source (assoc-ref inputs "source")))
                             (not (equal? (substring s 0 (string-length source))
                                          source))))
                         (string-split (getenv "CLASSPATH") #\:))
                       ":"))
             #t))
         (add-before 'build 'generate-parser
           (lambda* _
             (let* ((dir "src/org/openstreetmap/josm/gui/mappaint/mapcss")
                    (out (string-append dir "/parsergen"))
                    (file (string-append dir "/MapCSSParser.jj")))
               (mkdir-p "src/org/openstreetmap/josm/gui/mappaint/mapcss/parsergen")
               (invoke "javacc" "-DEBUG_PARSER=false"
                       "-DEBUG_TOKEN_MANAGER=false" "-JDK_VERSION=1.8"
                       "-GRAMMAR_ENCODING=UTF-8"
                       (string-append "-OUTPUT_DIRECTORY=" out)
                       file))
             #t))
         (add-after 'build 'generate-epsg
           (lambda _
             (system* "javac" "scripts/BuildProjectionDefinitions.java"
                      "-cp" "build/classes")
             (mkdir-p "resources/data/projection")
             (with-output-to-file "resources/data/projection/custom-epsg"
               (lambda _ (display "")))
             (mkdir-p "data/projection")
             (with-output-to-file "data/projection/custom-epsg"
               (lambda _ (display "")))
             (invoke "java" "-cp" "build/classes:scripts:."
                     "BuildProjectionDefinitions" ".")
             #t))
         (add-after 'generate-epsg 'copy-resources
           (lambda _
             (copy-recursively "resources" "build/classes")
             #t))
         (add-before 'install 'regenerate-jar
           (lambda _
             ;; We need to regenerate the jar file to add data.
             (delete-file "build/jar/josm.jar")
             (invoke "jar" "-cf" "build/jar/josm.jar" "-C"
                     "build/classes" ".")
             #t))
         (add-before 'build 'copy-revision
           (lambda _
             (mkdir-p "build/classes")
             (with-output-to-file "build/classes/REVISION"
               (lambda _
                 (display
                   (string-append "Revision: " ,version "\n"
                                  "Is-Local-Build: true\n"
                                  "Build-Date: 1970-01-01 00:00:00 +0000\n"))))
             #t))
         (add-after 'install 'install-share-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (share-directories '("applications" "icons" "man" "menu"
                                        "metainfo" "mime" "pixmaps"))
                   (desktop "org.openstreetmap.josm.desktop"))
               (for-each (lambda (directory)
                           (copy-recursively (string-append
                                              "native/linux/tested/usr/share/"
                                              directory)
                                             (string-append
                                              out "/share/" directory)))
                         share-directories)
               (substitute* (string-append out "/share/applications/" desktop)
                 (("josm-MainApplication") "josm-gui-MainApplication")))
             #t))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/josm")
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    (assoc-ref inputs "openjdk") "/bin/java"
                                    " -cp " out "/share/java/josm.jar:"
                                    ;; CLASSPATH, but remove native inputs
                                    (string-join
                                      (filter
                                        (lambda (jar)
                                          (and (not (string-contains jar "-jdk/"))
                                               (not (string-contains jar "-javacc-"))))
                                        (string-split (getenv "CLASSPATH") #\:))
                                      ":")
                                    " --add-exports=java.base/sun.security.action=ALL-UNNAMED"
                                    " --add-exports=java.desktop/com.sun.imageio.plugins.jpeg=ALL-UNNAMED"
                                    " --add-exports=java.desktop/com.sun.imageio.spi=ALL-UNNAMED"
                                    " org.openstreetmap.josm.gui.MainApplication"))))
               (chmod (string-append bin "/josm") #o755))
             #t)))))
    (home-page "https://josm.openstreetmap.de")
    (synopsis "OSM editor")
    (description "JOSM is an extensible editor for OpenStreetMap (OSM).  It
supports loading GPX tracks, background imagery and OSM data from local
sources as well as from online sources and allows editing the OSM data (nodes,
ways, and relations) and their metadata tags.")
    (license license:gpl2+)))

(define-public libmaxminddb
  (package
    (name "libmaxminddb")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/maxmind/libmaxminddb"
                           "/releases/download/" version "/"
                           "/libmaxminddb-" version ".tar.gz"))
       (sha256
        (base32 "0rw2z7rx8jzgdcgqlmc4wqrsjmiwd8vm5wvvrldy472rghcaq83n"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))))
    (native-inputs
     (list perl))
    (home-page "https://maxmind.github.io/libmaxminddb/")
    (synopsis "C library for the MaxMind DB file format")
    (description "The libmaxminddb library provides a C library for reading
MaxMind DB files, including the GeoIP2 databases from MaxMind.  The MaxMind DB
format is a custom, but open, binary format designed to facilitate fast
lookups of IP addresses while allowing flexibility in the type of data
associated with an address.")
    (license license:asl2.0)))

(define-public python-maxminddb
  (package
    (name "python-maxminddb")
    (version "2.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "maxminddb" version))
       (sha256
        (base32 "0m6j8pvarnw4d88537ghi1gl7nskwgkijx5c3fm4g83sm9mq1hyj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (inputs
     (list libmaxminddb))
    (home-page "https://www.maxmind.com/")
    (synopsis "Reader for the MaxMind DB format")
    (description "MaxMind DB is a binary file format that stores data indexed
by IP address subnets (IPv4 or IPv6).  This is a Python module for reading
MaxMind DB files.")
    (license license:asl2.0)))

(define-public python-geoip2
  (package
    (name "python-geoip2")
    (version "4.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "geoip2" version))
       (sha256
        (base32
         "0ddcm6a0f5xr66r84hpn6jr6b7hl77axb0d41qj285ylny0c376x"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:tests? #f)) ;; Tests require a copy of the maxmind database
    (inputs
     (list python-maxminddb
           python-requests
           python-aiohttp))
    (native-inputs
     (list python-setuptools python-wheel))
    (home-page "https://www.maxmind.com/")
    (synopsis "MaxMind GeoIP2 API")
    (description "Provides an API for the GeoIP2 web services and databases.
The API also works with MaxMind’s free GeoLite2 databases.")
    (license license:asl2.0)))

(define-public routino
  (package
    (name "routino")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.routino.org/download/routino-"
                                  version ".tgz"))
              (sha256
               (base32
                "0aw5idqz7nv458llgwp5wcgikf34xcblpq46mq7msxfib0m8vahb"))))
    (build-system gnu-build-system)
    (native-inputs (list perl))
    (inputs (list bzip2 xz zlib))
    (arguments
     (list #:test-target "test"
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          (lambda* (#:key outputs #:allow-other-keys)
                            (substitute* "Makefile.conf"
                              (("prefix=/usr/local")
                               (string-append "prefix="
                                              (assoc-ref outputs "out")))
                              (("LDFLAGS_LDSO=-Wl,-R\\.")
                               "LDFLAGS_LDSO=-Wl,-R$(libdir)")
                              (("#CFLAGS\\+=-DUSE_XZ")
                               "CFLAGS+=-DUSE_XZ")
                              (("#LDFLAGS\\+=-llzma")
                               "LDFLAGS+=-llzma")))))))
    (synopsis "Routing application for OpenStreetMap data")
    (description
     "Routino is an application for finding a route between two points
using the dataset of topographical information collected by
@url{https://www.OpenStreetMap.org}.")
    (home-page "https://www.routino.org/")
    (license license:agpl3+)))

(define-public r-rnaturalearthhires
  (let ((commit "c3785a8c44738de6ae8f797080c0a337ebed929d")
        (revision "1"))
    (package
      (name "r-rnaturalearthhires")
      (version (git-version "0.2.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ropensci/rnaturalearthhires")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1fr0yb2fbr9zbk7gqr3rnzz2w4ijjpl6hlzdrh4n27lf0ip3h0cx"))))
      (properties `((upstream-name . "rnaturalearthhires")))
      (build-system r-build-system)
      (propagated-inputs (list r-sp))
      (native-inputs (list r-knitr))
      (home-page "https://github.com/ropensci/rnaturalearthhires")
      (synopsis
       "High Resolution World Vector Map Data from Natural Earth used in rnaturalearth")
      (description
       "Facilitates mapping by making natural earth map data from http://
www.naturalearthdata.com/ more easily available to R users.  Focuses on vector
data.")
      (license license:cc0))))

(define-public qmapshack
  (package
    (name "qmapshack")
    (version "1.17.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Maproom/qmapshack")
             (commit (string-append "V_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ckadklk67dp1pvkacfkr8379g2pwk73q85jfzm8viclcqmfvb62"))))
    (build-system qt-build-system)
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list curl
           gdal
           libjpeg-turbo
           proj
           qtbase-5
           qtdeclarative-5
           qtlocation-5
           qtwebchannel-5
           qtwebengine-5
           quazip
           routino
           sqlite ; See wrap phase
           zlib))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-cmake-modules
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("find_package\\(Qt5PrintSupport        REQUIRED\\)" all)
                (string-append all "\nfind_package(Qt5Positioning REQUIRED)")))
             (substitute* "cmake/Modules/FindROUTINO.cmake"
               (("/usr/local")
                (assoc-ref inputs "routino"))))))))
    (synopsis "GPS mapping application")
    (description
     "QMapShack can be used to plan your next outdoor trip or to visualize and
archive all the GPS recordings of your past trips.  It is the successor of the
QLandkarte GT application.")
    (home-page "https://github.com/Maproom/qmapshack/wiki")
    (license license:gpl3+)))

(define-public readosm
  (package
    (name "readosm")
    (version "1.1.0a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                           "readosm-" version ".tar.gz"))
       (sha256
        (base32 "0igif2bxf4dr82glxz9gyx5mmni0r2dsnx9p9k6pxv3c4lfhaz6v"))))
    (build-system gnu-build-system)
    (inputs
     (list expat zlib))
    (synopsis "Data extractor for OpenStreetMap files")
    (description
     "ReadOSM is a library to extract valid data from within an OpenStreetMap
input file (in @code{.osm} or @code{.osm.pbf} format).")
    (home-page "https://www.gaia-gis.it/fossil/readosm/index")
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1))))

(define-public shapelib
  (package
    (name "shapelib")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OSGeo/shapelib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l67gp1618lcw7fg2iclbh016cqyw85s3cmd3qzx6aw0jq19hj8n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (home-page "http://shapelib.maptools.org/")
    (synopsis "Provides C library to write and update ESRI Shapefiles")
    (description
     "The Shapefile C Library provides the ability to write simple C programs
for reading, writing and updating (to a limited extent) ESRI Shapefiles, and
the associated attribute file (@file{.dbf}).")
    (license license:gpl2+)))

(define-public spatialite-tools
  (package
    (name "spatialite-tools")
    (version "5.1.0a")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                           "spatialite-tools-sources/"
                           "spatialite-tools-" version ".tar.gz"))
       (sha256
        (base32 "1kh1amab452m3801knmpn1jcg27axakb90gd8fxwv240irsk97hi"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list expat
           freexl
           geos
           librttopo
           libspatialite
           libxml2
           minizip
           proj
           readosm
           sqlite))
    (synopsis "Collection of command line tools for SpatiaLite")
    (description
     "@code{spatialite-tools} is a collection of Command Line Interface (CLI)
tools supporting SpatiaLite.")
    (home-page "https://www.gaia-gis.it/fossil/spatialite-tools/index")
    (license license:gpl3+)))

(define-public virtualpg
  (package
    (name "virtualpg")
    (version "2.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.gaia-gis.it/gaia-sins/"
                           "virtualpg-" version ".tar.gz"))
       (sha256
        (base32 "12z0l7368r4116ljzg7nljy5hf425r11vxc540w79wlzikmynamy"))))
    (build-system gnu-build-system)
    (inputs
     (list postgresql sqlite))
    (synopsis "Allow SQLite/SpatiaLite to access PostgreSQL/PostGIS tables")
    (description
     "VirtualPG is a dynamic extension for the SQLite DBMS.  It implements
the VirtualPostgres driver, allowing to directly exchange data between SQLite
and PostgreSQL; if SpatiaLite is available even PostGIS geometries can be
exchanged form one Spatial DBMS and the other.")
    (home-page "https://www.gaia-gis.it/fossil/virtualpg/index")
    (license (list license:gpl2+
                   license:lgpl2.1+
                   license:mpl1.1))))

(define-public opencpn
  (package
    (name "opencpn")
    (version "5.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenCPN/OpenCPN")
             (commit (string-append "Release_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16hb0ycp0kbx2h8fx08rqkgrlz48kaym0d6wqvpjrcfa2r4myss8"))))
    (build-system cmake-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list alsa-utils
           bzip2
           cairo
           curl
           eudev
           glu
           gtk+
           jasper
           libarchive
           libelf
           libexif
           libjpeg-turbo
           libsndfile
           libusb
           lz4
           mesa
           pango
           portaudio
           sqlite
           tinyxml
           wxsvg
           wxwidgets-3.0
           xz
           zlib))
    (arguments
     `(#:configure-flags '("-DOCPN_USE_BUNDLED_LIBS=OFF"
                           "-DOCPN_ENABLE_PORTAUDIO=ON"
                           "-DOCPN_ENABLE_SNDFILE=ON"
                           "-DOCPN_BUNDLE_TCDATA=ON"
                           "-DOCPN_BUNDLE_GSHHS=ON")
       #:tests? #f ; No tests defined
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-build
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "CMakeLists.txt"
               (("wx-32.c; cc")
                 "wx-32.c; gcc")
               (("\"/bin/sh\" \"-c\"")
                (string-append "\"" (which "bash") "\" \"-c\""))
               (("include\\(TargetSetup\\)")
                "set(PKG_TARGET \"guix\")\nset(PKG_TARGET_VERSION 1)")))))))
    (synopsis "Chart plotter and marine GPS navigation software")
    (description
     "OpenCPN is a chart plotter and marine navigation software designed to be
used at the helm station of your boat while underway.  Chart a course and
track your position right from your laptop.")
    (home-page "https://opencpn.org/")
    (license (list license:asl2.0
                   license:cc0
                   license:bsd-2
                   license:bsd-3
                   license:expat
                   license:gpl3+
                   license:lgpl2.1+
                   license:lgpl3+
                   license:sgifreeb2.0
                   license:zlib))))

(define-public openorienteering-mapper
  (package
    (name "openorienteering-mapper")
    (version "0.9.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenOrienteering/mapper")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11b578h8f3q9yvphbjhqmy2w1cfc9skslzawypqmc3k44v24aj0s"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list
        "-DLICENSING_PROVIDER:BOOL=OFF"
        "-DMapper_MANUAL_QTHELP:BOOL=OFF")))
    (inputs
     (list clipper
           cups
           curl
           gdal
           proj
           qtbase-5
           qtimageformats-5
           qtlocation-5
           qtsensors-5
           zlib))
    (native-inputs
     (list doxygen
           qttools-5))
    (home-page "https://www.openorienteering.org/apps/mapper/")
    (synopsis "OpenOrienteering Mapper (OOM)")
    (description
     "OpenOrienteering Mapper is a software for creating maps for the
orienteering sport.")
    (license license:gpl3+)))

(define-public grass
  (let* ((version "7.8.8")
         (majorminor (string-join (list-head (string-split version #\.) 2) ""))
         (grassxx (string-append "grass" majorminor)))
    (package
      (name "grass")
      (version version)
      (source
       (origin
         (method url-fetch)
         (uri (string-append "https://grass.osgeo.org/" grassxx
                             "/source/grass-" version ".tar.gz"))
         (sha256
          (base32 "1gpfbppfajc8d6b9alw9fdzgaa83w26kl6fff1395bc9gal215ms"))))
      (build-system gnu-build-system)
      (inputs
       (list bash-minimal
             bzip2
             cairo
             fftw
             freetype
             gdal
             geos
             glu
             libpng
             libtiff
             mesa
             `(,mariadb "dev")
             `(,mariadb "lib")
             netcdf
             openblas
             perl
             postgresql
             proj
             python
             python-dateutil
             python-numpy
             python-wxpython
             readline
             sqlite
             wxwidgets
             zlib
             `(,zstd "lib")))
      (native-inputs
       (list bash-minimal
             bison
             flex
             pkg-config))
      (arguments
       `(#:tests? #f                    ; No tests
         #:modules ((guix build gnu-build-system)
                    ((guix build python-build-system) #:prefix python:)
                    (guix build utils))
         #:imported-modules (,@%default-gnu-imported-modules
                             (guix build python-build-system))
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'fix-lapack
             (lambda _
               (substitute* "./configure"
                 (("-lblas") "-lopenblas")
                 (("-llapack") "-lopenblas"))))
           (replace 'configure
             (lambda* (#:key inputs outputs #:allow-other-keys)
               (let ((shell (search-input-file inputs "/bin/bash")))
                 (setenv "SHELL" shell)
                 (setenv "CONFIG_SHELL" shell)
                 (setenv "LDFLAGS" (string-append "-Wl,-rpath -Wl,"
                                                  (assoc-ref outputs "out")
                                                  "/" ,grassxx "/lib")))
               (invoke "./configure"
                       (string-append "--prefix="
                                      (assoc-ref outputs "out"))
                       "--with-blas"
                       "--with-bzlib"
                       (string-append
                        "--with-freetype-includes="
                        (search-input-directory inputs "/include/freetype2"))
                       (string-append
                        "--with-freetype-libs="
                        (dirname
                         (search-input-file inputs "/lib/libfreetype.so")))
                       "--with-geos"
                       "--with-lapack"
                       "--with-mysql"
                       (string-append
                        "--with-mysql-includes="
                        (search-input-directory inputs "/include/mysql"))
                       (string-append
                        "--with-mysql-libs="
                        (dirname
                         (search-input-file inputs "/lib/libmariadb.so")))
                       "--with-netcdf"
                       "--with-postgres"
                       (string-append
                        "--with-proj-share="
                        (search-input-directory inputs "/share/proj"))
                       "--with-pthread"
                       "--with-readline"
                       "--with-sqlite"
                       "--with-wxwidgets")))
           (add-after 'install 'install-links
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Put links for includes and libraries in the standard places.
               (let* ((out (assoc-ref outputs "out"))
                      (dir (string-append out "/" ,grassxx)))
                 (symlink (string-append dir "/include")
                          (string-append out "/include"))
                 (symlink (string-append dir "/lib")
                          (string-append out "/lib")))))
           (add-after 'install-links 'python:wrap
             (assoc-ref python:%standard-phases 'wrap))
           (add-after 'python:wrap 'wrap-with-python-interpreter
             (lambda* (#:key outputs #:allow-other-keys)
               (let ((out (assoc-ref outputs "out")))
                 (wrap-program (string-append out "/bin/" ,grassxx)
                   `("GRASS_PYTHON" = (,(which "python3"))))))))))
      (synopsis "GRASS Geographic Information System")
      (description
       "GRASS (Geographic Resources Analysis Support System), is a Geographic
Information System (GIS) software suite used for geospatial data management and
analysis, image processing, graphics and maps production, spatial modeling, and
visualization.")
      (home-page "https://grass.osgeo.org/")
      (license license:gpl2+))))

(define-public saga
  (package
    (name "saga")
    (version "8.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/saga-gis/SAGA%20-%20"
                           (version-major version) "/SAGA%20-%20" version
                           "/saga-" version ".tar.gz"))
       (sha256
        (base32 "008izjs6gvj09abxf16ssl1xy0ay3ljq4jswbggp6wiiq459minv"))
       (modules '((guix build utils)))
       (snippet
        '(substitute* "saga-gis/src/tools/docs/docs_pdf/doc_pdf.cpp"
           (("^#include <hpdf\\.h>\n" all)
            (string-append all "#include <hpdf_version.h>\n"))
           (("\\bHPDF_PROJECTING_SCUARE_END\\b")
            "HPDF_PROJECTING_SQUARE_END")))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config swig))
    (inputs
     (list curl
           fftw
           gdal
           hdf5
           jasper
           libharu
           libtiff
           opencv
           postgresql
           proj
           python
           qhull
           unixodbc
           vigra
           wxwidgets))
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'cd-to-source-dir
           (lambda _
             (chdir "saga-gis"))))))
    (synopsis "System for Automated Geoscientific Analyses")
    (description
     "SAGA (System for Automated Geoscientific Analyses) is a Geographic
Information System (GIS) software.  It has been designed for an easy and
effective implementation of spatial algorithms and it offers a comprehensive,
growing set of geoscientific methods.")
    (home-page "https://www.saga-gis.org")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public qgis
  (package
    (name "qgis")
    (version "3.42.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://qgis.org/downloads/qgis-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1s04wy3iy7amm4jn5y5j9pw54isizhqq2mr10ld5ldz2gqwl7dq4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build qt-utils)
                  (guix build utils))
      #:imported-modules `(,@%cmake-build-system-modules
                           (guix build python-build-system)
                           (guix build qt-utils))
      #:configure-flags
      #~(list "-DWITH_QTWEBKIT=NO")
      #:phases
      #~(let* ((ignored-tests (list
                               ;; Disable tests that require network access
                               "PyQgsExternalStorageAwsS3"
                               "PyQgsExternalStorageWebDav"
                               "test_core_networkaccessmanager"
                               "test_core_tiledownloadmanager"
                               "test_gui_filedownloader"
                               "test_provider_wcsprovider"
                               ;; Disable tests that need OGR built with
                               ;; libspatialite support
                               "PyQgsAttributeTableModel"
                               "PyQgsOGRProviderSqlite"
                               "PyQgsWFSProvider"
                               "PyQgsOapifProvider"
                               ;; Disable tests that need Python compiled
                               ;; with loadable SQLite extensions.
                               "PyQgsFieldFormattersTest"
                               "PyQgsSpatialiteProvider"
                               "PyQgsLayerDependencies"
                               "PyQgsDBManagerGpkg"
                               "PyQgsDBManagerSpatialite"
                               ;; Disable tests that need poppler (with Cairo)
                               "PyQgsLayoutExporter"
                               "PyQgsPalLabelingLayout"
                               ;; Disable tests that need Orfeo ToolBox
                               "ProcessingOtbAlgorithmsTest"
                               ;; TODO: Find why the following tests fail
                               "ProcessingQgisAlgorithmsTestPt1"
                               "ProcessingQgisAlgorithmsTestPt2"
                               "ProcessingQgisAlgorithmsTestPt3"
                               "ProcessingQgisAlgorithmsTestPt4"
                               "ProcessingGdalAlgorithmsVectorTest"
                               "ProcessingGrassAlgorithmsImageryTest"
                               "ProcessingGrassAlgorithmsRasterTestPt1"
                               "ProcessingGrassAlgorithmsRasterTestPt2"
                               "ProcessingGrassAlgorithmsVectorTest"
                               "ProcessingGrassAlgsPostgreRasterProvider"
                               "test_core_authconfig"
                               "test_core_authmanager"
                               "test_core_compositionconverter"
                               "test_core_expression"
                               "test_core_gdalprovider"
                               "test_core_gdalutils"
                               "test_core_labelingengine"
                               "test_core_layoutpicture"
                               "test_core_ogrprovider"
                               "test_core_project"
                               "test_core_coordinatereferencesystem"
                               "test_core_overlayexpression"
                               "test_gui_queryresultwidget"
                               "test_provider_eptprovider"
                               "test_analysis_processingalgspt2"
                               "test_analysis_processingpdalalgs"
                               "test_analysis_processing"
                               "test_app_gpsintegration"
                               "test_3d_mesh3drendering"
                               "PyQgsAnnotation"
                               "PyQgsAnnotationLayer"
                               "PyQgsAnnotationLineItem"
                               "PyQgsAnnotationLineTextItem"
                               "PyQgsAnnotationPolygonItem"
                               "PyQgsAuthenticationSystem"
                               "PyQgsDatumTransform"
                               "PyQgsFileUtils"
                               "PyQgsGeometryTest"
                               "PyQgsGoogleMapsGeocoder"
                               "PyQgsHashLineSymbolLayer"
                               "PyQgsLayoutHtml"
                               "PyQgsLineSymbolLayers"
                               "PyQgsMapLayer"
                               "PyQgsOGRProviderGpkg"
                               "PyQgsProcessExecutablePt1"
                               "PyQgsProcessExecutablePt2"
                               "PyQgsProjectionSelectionWidgets"
                               "PyQgsProviderConnectionGpkg"
                               "PyQgsProviderConnectionSpatialite"
                               "PyQgsOGRProvider"
                               "PyQgsTextRenderer"
                               "PyQgsVectorFileWriter"
                               "PyQgsVirtualLayerProvider"
                               "PyQgsAuxiliaryStorage"
                               "PyQgsSelectiveMasking"
                               "PyPythonRepr"
                               "PyQgsAnimatedMarkerSymbolLayer"
                               "PyQgsPythonProvider"
                               "PyQgsCategorizedSymbolRenderer"
                               "PyQgsColorRampLegendNode"
                               "PyQgsEmbeddedSymbolRenderer"
                               "PyQgsFillSymbolLayers"
                               "PyQgsGeometryGeneratorSymbolLayer"
                               "PyQgsGpsLogger"
                               "PyQgsGraduatedSymbolRenderer"
                               "PyQgsHighlight"
                               "PyQgsInterpolatedLineSymbolLayer"
                               "PyQgsJsonUtils"
                               "PyQgsLayerTreeView"
                               "PyQgsLayoutAtlas"
                               "PyQgsLayoutElevationProfile"
                               "PyQgsLayoutPageCollection"
                               "PyQgsLayoutItem"
                               "PyQgsLayoutLegend"
                               "PyQgsLayoutMap"
                               "PyQgsLayoutPage"
                               "PyQgsLineburstSymbolLayer"
                               "PyQgsMapCanvas"
                               "PyQgsMapCanvasAnnotationItem"
                               "PyQgsMapHitTest"
                               "PyQgsMarkerLineSymbolLayer"
                               "PyQgsMergedFeatureRenderer"
                               "PyQgsMeshLayerProfileGenerator"
                               "PyQgsPalLabelingPlacement"
                               "PyQgsPointCloudAttributeByRampRenderer"
                               "PyQgsPointCloudExtentRenderer"
                               "PyQgsPointCloudLayerProfileGenerator"
                               "PyQgsPointClusterRenderer"
                               "PyQgsPointDisplacementRenderer"
                               "PyQgsProfileExporter"
                               "PyQgsProfileRequest"
                               "TestQgsRandomMarkerSymbolLayer"
                               "PyQgsRasterAttributeTable"
                               "PyQgsRasterFileWriterTask"
                               "PyQgsRasterLayer"
                               "PyQgsRasterLayerProfileGenerator"
                               "PyQgsRasterColorRampShader"
                               "PyQgsRasterLineSymbolLayer"
                               "PyQgsRasterPipe"
                               "PyQgsSingleSymbolRenderer"
                               "PyQgsSimpleFillSymbolLayer"
                               "PyQgsSimpleLineSymbolLayer"
                               "PyQgsSymbolLayer"
                               "PyQgsRasterRendererCreateSld"
                               "PyQgsSymbolLayerCreateSld"
                               "PyQgsArrowSymbolLayer"
                               "PyQgsSymbolExpressionVariables"
                               "PyQgsStyleModel"
                               "PyQgsSymbol"
                               "PyQgsSymbolLayerUtils"
                               "PyQgsTextFormatWidget"
                               "PyQgsVectorFieldMarkerSymbolLayer"
                               "PyQgsVectorLayer"
                               "PyQgsVectorLayerProfileGenerator"
                               "PyQgsVectorLayerRenderer"
                               "qgis_sipify"
                               "qgis_sip_include"
                               "qgis_sip_uptodate"))
               (grass-version #$(package-version (this-package-input "grass")))
               (grass-majorminor (string-join
                                  (list-head
                                   (string-split grass-version #\.) 2)
                                  ""))
               (grass-dir (string-append #$(this-package-input "grass")
                                         "/grass" grass-majorminor)))
          (modify-phases %standard-phases
            ;; Configure correct path to PyQt5 SIP directory
            (add-after 'unpack 'configure-pyqt5-sip-path
              (lambda* (#:key inputs #:allow-other-keys)
                (let ((sip-dir (string-append
                                (assoc-ref inputs "python-pyqt+qscintilla")
                                "/lib/python"
                                (python:python-version (assoc-ref inputs "python"))
                                "/site-packages/PyQt5/bindings")))
                  (substitute* "cmake/FindPyQt5.py"
                    (("sip_dir = cfg.default_sip_dir")
                     (string-append "sip_dir = \"" sip-dir "\"")))
                  (substitute* "cmake/FindPyQt5.cmake"
                    (("\
SET\\(PYQT5_SIP_DIR \"\\$\\{Python_SITEARCH\\}/PyQt5/bindings\"\\)")
                     (string-append "SET(PYQT5_SIP_DIR \"" sip-dir "\")"))))
                (substitute* (list "tests/code_layout/test_qt_imports.sh"
                                   "tests/code_layout/test_qgsscrollarea.sh")
                  (("\\$\\(git rev-parse --show-toplevel\\)")
                   (getcwd)))))
            (replace 'check
              (lambda* (#:key inputs outputs tests? parallel-tests?
                        #:allow-other-keys)
                (if tests?
                    (begin
                      (setenv "HOME" "/tmp")
                      (system "Xvfb :1 &")
                      (setenv "DISPLAY" ":1")
                      (setenv "TRAVIS" "true")
                      (setenv "CTEST_OUTPUT_ON_FAILURE" "1")
                      (setenv "GISBASE" grass-dir)
                      (invoke "ctest"
                              "-j" (if parallel-tests?
                                       (number->string (parallel-job-count))
                                       "1")
                              "-E" (string-join ignored-tests "|")))
                    (format #t "test suite not run~%"))))
            (add-after 'install 'wrap-python
              (assoc-ref python:%standard-phases 'wrap))
            (add-after 'wrap-python 'wrap-qt
              (lambda* (#:key inputs #:allow-other-keys)
                (wrap-qt-program "qgis" #:output #$output #:inputs inputs)))
            (add-after 'wrap-qt 'wrap-gis
              (lambda* (#:key inputs #:allow-other-keys)
                ;; TODO: Find if there is a way to get SAGA to work.
                ;; Currently QGIS says "version of SAGA not supported".
                ;; Disable it for now.
                (wrap-program (string-append #$output "/bin/qgis")
                  ;; `("PATH" ":" prefix
                  ;; (,(dirname (search-input-file inputs "/bin/saga_cmd"))))
                  `("QGIS_PREFIX_PATH" = (,#$output))
                  `("GISBASE" = (,grass-dir)))))))))
    (inputs
     (list bash-minimal
           draco
           exiv2
           expat
           freexl
           gdal
           geos
           gpsbabel
           grass
           gsl
           hdf5
           librttopo
           libspatialindex
           libspatialite
           libxml2
           libzip
           minizip
           netcdf
           pdal
           postgresql
           proj
           protobuf
           python
           python-chardet
           python-dateutil
           python-future
           python-jinja2
           python-numpy
           python-owslib
           python-psycopg2
           python-pygments
           python-pyqt+qscintilla
           python-pytz
           python-pyyaml
           python-requests
           python-sip
           python-six
           python-urllib3
           qca
           qscintilla
           qt3d-5
           qtbase-5
           qtdeclarative-5
           qtkeychain
           qtlocation-5
           qtmultimedia-5
           qtserialport-5
           qtsvg-5
           qwt
           ;; saga
           sqlite
           (list zstd "lib")))
    (native-inputs
     (append
      (list bison
            flex
            perl
            perl-yaml-tiny
            pkg-config
            python-mock
            python-nose2
            python-pyqt-builder
            qttools-5)
      (if (supported-package? shellcheck)
          (list shellcheck)
          '())
      (list xorg-server-for-tests)))
    (home-page "https://qgis.org")
    (synopsis "Geographical information system")
    (description "QGIS is an easy to use Geographical Information
System (GIS).  It is a GIS data viewer and editor.  QGIS supports a number of
raster and vector data formats, with new support easily added using the plugin
architecture.")
    (license
     (list
      license:asl1.1
      license:asl2.0
      license:bsd-2
      license:bsd-3
      license:boost1.0
      license:cc-by3.0
      license:cc-by4.0
      license:cc-by-sa3.0
      license:cc-by-sa4.0
      (license:fsdg-compatible "https://www.deviantart.com/elvensword")
      (license:fsf-free "file://debian/copyright" "Go Squared")
      license:expat
      license:fdl1.2+
      (license:fsf-free
       "https://www.deviantart.com/webgoddess/art/Reddish-Inspired-Gradients-42208824")
      (license:fsf-free
       "file://debian/copyright"
       "QT-Commercial or LGPL-2.1 with Digia Qt LGPL Exception 1.1 or GPL-3")
      license:gpl2
      license:gpl2+
      license:gpl3
      license:gpl3+
      license:isc
      license:lgpl2.0+
      license:lgpl2.1
      license:lgpl2.1+
      license:lgpl3
      (license:non-copyleft "file://debian/copyright" "BSD-like-gist")
      (license:non-copyleft "file://debian/copyright" "Jim Mossman Attribution")
      (license:non-copyleft
       "https://www.ncl.ucar.edu/Download/NCL_source_license.shtml"
       "NCL Source Code License")
      license:ogl-psi1.0
      license:opl1.0+
      license:public-domain
      license:qwt1.0))))

(define-public splat
  (package
    (name "splat")
    (version "1.5.0b3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hoche/splat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10djwjwb1pvznr0fjwnxdm5d961f3yngispb4zj9hyzdgq1xh217"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Delete pre-compiled libraries.
           (delete-file-recursively "vstudio")))))
    (build-system gnu-build-system)
    (inputs
     (list bzip2 libjpeg-turbo libpng zlib))
    (arguments
     (list #:tests? #f ; No test suite.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-installation-scripts
                 (lambda _
                   (substitute* (list "install" "utils/install")
                     (("/usr/local")
                      #$output)
                     (("whoami=`whoami`")
                      "whoami=root"))))
               (delete 'configure)
               (add-before 'install 'create-bin-directory
                 (lambda _
                   (mkdir-p (string-append #$output "/bin")))))))
    (synopsis "Signal propagation and coverage analysis tool")
    (description
     "The SPLAT (Signal Propagation, Loss, And Terrain) program can use the
Longley-Rice path loss and coverage prediction using the Irregular Terrain
Model to predict the behaviour and reliability of radio links, and to predict
path loss.")
    (home-page "https://www.qsl.net/kd2bd/splat.html")
    (license license:gpl2+)))

(define-public python-geographiclib
  (package
    (name "python-geographiclib")
    (version "2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "geographiclib" version))
        (sha256
         (base32
          "0naql53537dsa6g9lzz1hf688b1vvih6dj2yjcyjs71yvj2irx7p"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://geographiclib.sourceforge.io/2.0/python/")
    (synopsis "Python geodesic routines from GeographicLib")
    (description
     "This is a python implementation of the geodesic routines in GeographicLib.")
    (license license:expat)))

(define-public python-geoip2fast
  (package
    (name "python-geoip2fast")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "geoip2fast" version))
       (sha256
        (base32 "1cmdjlwjd4pg3qvsb8d4vghqj1im58npxb6dmrd5q90wjga4rfvm"))))
    (build-system pyproject-build-system)
    ;; The tests are speed tests or development tests to compare results with
    ;; a different library.
    (arguments (list #:tests? #false))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/rabuchaim/geoip2fast")
    (synopsis
     "Fast GeoIP2 country/city/asn lookup library")
    (description
     "@code{GeoIP2Fast} is a fast @code{GeoIP2} country/city/asn lookup
library that supports IPv4 and IPv6.  A search takes less than 0.00003
seconds.  It has its own data file updated twice a week with
Maxmind-Geolite2-CSV, supports IPv4/IPv6 and is pure Python.")
    (license license:expat)))

(define-public python-geopy
  (package
    (name "python-geopy")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "geopy" version))
        (sha256
         (base32
          "1lfhnd04hbzmsdm5bqisvx2218v5cf6369xhbjz8jzfhga73sa2h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--skip-tests-requiring-internet")))
    (propagated-inputs
     (list python-geographiclib))
    (native-inputs
     (list python-docutils
           python-pytest
           python-pytest-asyncio
           python-pytz
           python-setuptools
           python-wheel))
    (home-page "https://github.com/geopy/geopy")
    (synopsis "Geocoding library for Python")
    (description "@code{geopy} is a Python client for several popular geocoding
web services.  @code{geopy} makes it easy for Python developers to locate the
coordinates of addresses, cities, countries, and landmarks across the globe
using third-party geocoders and other data sources.")
    (license license:expat)))

(define-public python-haversine
  (package
    (name "python-haversine")
    (version "2.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    ;; There are no tests in the PyPi archive.
                    (url "https://github.com/mapado/haversine")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0inxyj5n4jzgg5xiadqx9sk83gdx5ff989l9s04smdzbd3b8c0c8"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-numpy
           python-setuptools
           python-wheel))
    (home-page "https://github.com/mapado/haversine")
    (synopsis "Calculate the distance between 2 points on Earth")
    (description "This package provides functions to calculate the
distance in various units between two points on Earth using their
latitude and longitude.")
    (license license:expat)))

(define-public gplates
  (package
    (name "gplates")
    (version "2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/GPlates/GPlates")
                    (commit (string-append "GPlates-" version))))
              (sha256
               (base32
                "1qrislbgrsn6l1ikd3mffsqxvy61w3l53wmr8mfd8aqaj6dk1wfx"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f))                    ;no test target
    (inputs
     (list boost
           cgal
           curl
           gdal
           glew
           glu
           gmp
           mesa
           mpfr
           proj
           python-3
           python-numpy
           qtbase-5
           qtsvg-5
           qtxmlpatterns
           qwt
           zlib))
    (home-page "https://www.gplates.org")
    (synopsis "Plate tectonics simulation program")
    (description "GPlates is a plate tectonics program.  Manipulate
reconstructions of geological and paleogeographic features through geological
time.  Interactively visualize vector, raster and volume data.")
    (license license:gpl2+)))

(define-public navit
  (package
    (name "navit")
    (version "0.5.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/navit-gps/navit")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1jhlif0sc5m8wqb5j985g1xba2ki7b7mm14pkvzdghjd0q0gf15s"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; There are no tests
      #:tests? #f
      ;; With -DSAMPLE_MAP=TRUE (the default), it tries to download a
      ;; map during the build process.
      #:configure-flags #~(list "-DSAMPLE_MAP=FALSE")
      #:phases
      #~(modify-phases %standard-phases
          (add-after
              'unpack 'patch-navit-config
            (lambda _
              ;; For now this package only supports SDL, so if we keep
              ;; the configuration as-is, Navit doesn't start.
              (substitute*
                  "navit/navit_shipped.xml"
                (("<graphics type=\"gtk_drawing_area\"/>")
                 "<graphics type=\"sdl\"/>"))
              ;; Users are expected to be able to add XML files inside
              ;; $NAVIT_SHAREDIR, however that directory is in the store.
              (substitute*
                  "navit/navit_shipped.xml"
                (("<xi:include href=\"\\$NAVIT_SHAREDIR/maps/\\*\\.xml\"/>")
                 "<xi:include href=\"$NAVIT_USER_DATADIR/maps/*.xml\"/>"))
              ;; Navit also works without GPS but in that case there is
              ;; no automatic zooming, so we need zoom buttons to be able
              ;; to manually zoom in or out.
              (substitute*
                  "navit/navit_shipped.xml"
                (((string-append
                   "<osd enabled=\"no\" type=\"button\" x=\"-96\" y=\"-96\" "
                   "command=\"zoom_in()"))
                 (string-append
                  "<osd enabled=\"yes\" type=\"button\" x=\"-96\" y=\"-96\" "
                  "command=\"zoom_in()"))
                (((string-append
                   "<osd enabled=\"no\" type=\"button\" x=\"0\" y=\"-96\" "
                   "command=\"zoom_out()"))
                 (string-append
                  "<osd enabled=\"yes\" type=\"button\" x=\"0\" y=\"-96\" "
                  "command=\"zoom_out()\" src=\"zoom_out.png\"/>")))))
          (add-before
              'build 'set-cache
            ;; During the build, svg icons are converted in different
            ;; formats, and this needs XDG_CACHE_HOME to work.
            (lambda _
              (setenv "XDG_CACHE_HOME" "/tmp/xdg-cache"))))))
    (inputs (list dbus-glib
                  espeak
                  freeglut
                  freeimage
                  freetype
                  glib
                  gettext-minimal
                  gpsd
                  gdk-pixbuf
                  imlib2
                  python
                  sdl
                  sdl-image))
    (native-inputs (list fontconfig
                         (librsvg-for-system)
                         pkg-config))
    (home-page "https://www.navit-project.org")
    (synopsis "Car navigation system with routing engine that uses vector maps data")
    (description "Navit is a car navigation system with a routing engine.

It is meant to work with touchscreen devices, but it also works
without a touchscreen.  It also supports text to speech.

It can be configured extensively through its own configuration file
format.  For instance we can configure the graphical interface, and
which map data is to be displayed at which zoom level.

It supports different routing profiles: bike, car, car_avoid_toll,
car_pedantic, car_shortest, horse, pedestrian, truck.

It can use gpsd or NMEA GPS directly to get position data.  It also
works without GPS: in this case users can also enter position data
directly.

It can also be used to log GPS data to files using the GPX or NMEA
formats, or to replay NMEA data.

For maps, it can uses its own \"binfile\" map format, or Garmin map
file format, and data from OpenStreetMap, Garmin maps, Marco Polo
Grosser Reiseplaner, Routeplaner Europa 2007, Map + Route.")
    (license license:gpl2)))

(define-public laszip
  (package
    (name "laszip")
    (version "3.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LASzip/LASzip")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a0ass8wz1cd42mvdmmgp4lpaf0qpfn9blsilfskba3kmx9hpymz"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DLASZIP_BUILD_STATIC=NO")
      #:build-type "Release"
      ;; No tests.
      #:tests? #f))
    (home-page "https://laszip.org/")
    (synopsis "Compression library for LAS files")
    (description
     "LASzip is a library for compressing @code{LAS} files and uncompressing
@code{LAZ} files.  The @code{LAS} format is a file format designed for the
interchange and archiving of lidar point cloud data.")
    (license license:asl2.0)))

(define-public tetgen
  (package
    (name "tetgen")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri
         "https://wias-berlin.de/software/tetgen/1.5/src/tetgen1.6.0.tar.gz")
       (sha256
        (base32 "0fff0l6i3xfjlm0zkcgyyhwndp8i5d615mydyb21yirsplgfddc7"))))
    (build-system cmake-build-system)
    (arguments
      (list
        #:tests? #f ;; no test suite
        #:configure-flags #~(list "-DCMAKE_POSITION_INDEPENDENT_CODE=ON")
        #:phases
        #~(modify-phases %standard-phases
         (replace 'install ;; no install target
           (lambda _
             (install-file "tetgen"
                           (string-append #$output "/bin"))))
         ;; Do not create etc/ld.so.cache. It is a bit mysterious why
         ;; we have this phase in the first place.
         (delete 'make-dynamic-linker-cache))))
    (home-page "https://wias-berlin.de/software/tetgen/")
    (synopsis
     "Quality Tetrahedral Mesh Generator and 3D Delaunay Triangulator")
    (description
     "TetGen is a program to generate tetrahedral meshes of any 3D
polyhedral domains.  TetGen generates exact constrained Delaunay
tetrahedralizations, boundary conforming Delaunay meshes, and Voronoi
partitions.")
    (license license:agpl3+)))

(define-public libe57format
  (package
    (name "libe57format")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/asmaloney/libE57Format")
             (commit "v3.2.0")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00sj0splv4apv3kfjfwgsrizhimav3hxw51q1qz4g2fgncn092a9"))))
    (inputs (list xerces-c))
    (build-system cmake-build-system)
    (arguments
      (list
       ;; Tests use external data from
       ;; https://github.com/asmaloney/libE57Format-test-data
       ;; Even after downloading it and copying it to the
       ;; test/libE57Format-test-data subdirectory, the configure phase
       ;; fails with the following message:
       ;; [E57 Test] The GoogleTest submodule was not downloaded.
       ;; E57_GIT_SUBMODULE_UPDATE was turned off or failed.  Please update
       ;; submodules and try again.
       ;; Adding googletest as a native-input does not solve the problem.
       #:configure-flags #~(list "-DE57_BUILD_TEST=NO")
       #:tests? #f
       #:build-type "Release"))
    (home-page "https://github.com/asmaloney/libE57Format")
    (synopsis "Library for reading and writing E57 files")
    (description
     "The libE57Format package provides a C++ library for reading and
writing files in the ASTM-standard E57 format. E57 files store 3D point
cloud data (produced by 3D imaging systems such as laser scanners),
attributes associated with 3D point data (color and intensity),
and 2D images (photos taken using a 3D imaging system).")
    (license license:boost1.0)))

(define-public cloudcompare
  (package
    (name "cloudcompare")
    (version "2.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/CloudCompare/CloudCompare")
             (commit (string-append "v" version))
             ;; TODO: External source code could be unbundled.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wck05zbfkw7cg8h6fjiinjzrsk55858qg0k2m5rmr9dfdzjbzbb"))))
    (inputs (list qtbase-5
                  qtsvg-5
                  qtlocation-5
                  qttools-5
                  gdal
                  laszip
                  xerces-c
                  libe57format
                  zlib))
    (native-inputs (list xvfb-run))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list
                           ;; Options
                           "-DOPTION_BUILD_CCVIEWER=NO"
                           "-DBUILD_TESTING=ON"
                           "-DOPTION_USE_SHAPE_LIB=YES"
                           "-DOPTION_USE_DXF_LIB=YES"
                           "-DOPTION_USE_GDAL=YES"
                           ;; Shaders
                           ;; At least one shader is needed
                           "-DPLUGIN_GL_QEDL=YES"
                           "-DPLUGIN_GL_QSSAO=YES"
                           ;; IO
                           "-DPLUGIN_IO_QCORE=YES"
                           "-DPLUGIN_IO_QADDITIONAL=NO"
                           "-DPLUGIN_IO_QCSV_MATRIX=NO"
                           ;; No guix package for DRACO
                           "-DPLUGIN_IO_QDRACO=NO"
                           "-DPLUGIN_IO_QE57=YES"
                           ;; No guix package for FBX
                           "-DPLUGIN_IO_QFBX=NO"
                           ;; laszip replaces PDAL in CloudCompare 2.13
                           "-DPLUGIN_IO_QLAS=YES"
                           "-DPLUGIN_IO_QPDAL=NO"
                           "-DPLUGIN_IO_QPHOTOSCAN=YES"
                           ;; No guix package for Riegl RDBlib
                           "-DPLUGIN_IO_QRDB=NO"
                           "-DPLUGIN_IO_QSTEP=NO"
                           ;; Plugins
                           "-DPLUGIN_STANDARD_QANIMATION=YES"
                           "-DQANIMATION_WITH_FFMPEG_SUPPORT=NO"
                           "-DPLUGIN_STANDARD_QBROOM=YES"
                           ;; Compilation error
                           "-DPLUGIN_STANDARD_QCANUPO=YES"
                           "-DPLUGIN_STANDARD_QCLOUDLAYERS=YES"
                           "-DPLUGIN_STANDARD_QCOLORIMETRIC_SEGMENTER=YES"
                           "-DPLUGIN_STANDARD_QCOMPASS=YES"
                           ;; Only for Windows at the moment
                           "-DPLUGIN_STANDARD_QCORK=NO"
                           "-DPLUGIN_STANDARD_QCSF=YES"
                           "-DPLUGIN_STANDARD_QFACETS=YES"
                           ;; Error with eigen
                           "-DPLUGIN_STANDARD_QHOUGH_NORMALS=NO"
                           "-DPLUGIN_STANDARD_QHPR=YES"
                           ;; Need qtWebSocket engine
                           "-DPLUGIN_STANDARD_QJSONRPC=NO"
                           "-DPLUGIN_STANDARD_QM3C2=YES"
                           ;; Need PCL lib
                           "-DPLUGIN_STANDARD_MASONRY_QAUTO_SEG=NO"
                           "-DPLUGIN_STANDARD_MASONRY_QMANUAL_SEG=NO"
                           "-DPLUGIN_STANDARD_QPCL=NO"
                           ;; Need CGAL
                           "-DPLUGIN_STANDARD_QMESH_BOOLEAN=NO"
                           "-DPLUGIN_STANDARD_QMPLANE=YES"
                           "-DPLUGIN_STANDARD_QPCV=NO"
                           "-DPLUGIN_STANDARD_QPOISSON_RECON=YES"
                           "-DPLUGIN_STANDARD_QRANSAC_SD=YES"
                           "-DPLUGIN_STANDARD_QSRA=YES")
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda _
              (invoke "xvfb-run" "make" "test"))))))
    (home-page "https://cloudcompare.org/")
    (synopsis "Point cloud processing software")
    (description
     "CloudCompare is a 3D point cloud (and triangular mesh) processing
software.  It was originally designed to perform comparison between two
3D point clouds (such as the ones obtained with a laser scanner) or between
a point cloud and a triangular mesh.  It relies on an octree structure that
is highly optimized for this particular use-case.  It is also meant to deal
with huge point clouds.")
    (license license:gpl2+)))
