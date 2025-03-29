;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2017, 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015, 2017 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016-2019, 2021, 2023, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Roel Janssen <roel@gnu.org>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Leo Famulari <leo@famulari.name>
;;; Copyright © 2020 Sebastian Schott <sschott@mailbox.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2024. 2021, 2022, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022, 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages photo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages file)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (srfi srfi-26))

(define-public rapid-photo-downloader
  (package
    (name "rapid-photo-downloader")
    (version "0.9.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://launchpad.net/rapid/pyqt/"
                                  version "/+download/" name "-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15p7sssg6vmqbm5xnc4j5dr89d7gl7y5qyq44a240yl5aqkjnybw"))))
    (build-system python-build-system)
    (native-inputs
     (list file intltool gobject-introspection))
    (inputs
     `(("bash" ,bash-minimal) ; for wrap-program
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("gexiv2" ,gexiv2)
       ("gst-libav" ,gst-libav)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gst-plugins-good" ,gst-plugins-good)
       ("gstreamer" ,gstreamer)
       ("libgudev" ,libgudev)
       ("libnotify" ,libnotify)
       ("libmediainfo" ,libmediainfo)
       ("usdisks" ,udisks)
       ("python-pyqt" ,python-pyqt)
       ("python-pygobject" ,python-pygobject)
       ("python-gphoto2" ,python-gphoto2)
       ("python-pyzmq" ,python-pyzmq)
       ("python-tornado" ,python-tornado)
       ("python-psutil" ,python-psutil)
       ("python-pyxdg" ,python-pyxdg)
       ("python-arrow" ,python-arrow)
       ("python-dateutil" ,python-dateutil)
       ("python-easygui" ,python-easygui)
       ("python-colour" ,python-colour)
       ("python-pymediainfo" ,python-pymediainfo)
       ("python-sortedcontainers" ,python-sortedcontainers)
       ("python-rawkit" ,python-rawkit)
       ("python-requests" ,python-requests)
       ("python-colorlog" ,python-colorlog)
       ("python-pyprind" ,python-pyprind)
       ("python-tenacity" ,python-tenacity)
       ("perl-image-exiftool" ,perl-image-exiftool)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-libmediainfo
           (lambda _
             (substitute* "raphodo/metadatavideo.py"
               (("pymedia_library_file = 'libmediainfo.so.0'")
                (string-append "pymedia_library_file = '"
                               (assoc-ref %build-inputs "libmediainfo")
                               "/lib/libmediainfo.so.0'")))
             #t))
         (add-after 'install 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (path              (string-join
                                       (list (string-append
                                              (assoc-ref inputs "perl-image-exiftool")
                                              "/bin"))
                                       ":"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "GUIX_PYTHONPATH")))
               (for-each
                (lambda (program)
                  (wrap-program program
                    `("PATH" ":" prefix (,path))
                    `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                    `("GUIX_PYTHONPATH"             ":" prefix (,python-path))))
                (map (lambda (name)
                       (string-append out "/bin/" name))
                     '("analyze-pv-structure"
                       "rapid-photo-downloader"))))
             #t)))))
    (home-page "https://www.damonlynch.net/rapid/")
    (synopsis "Import photos and videos from cameras, phones and memory cards")
    (description "Import photos and videos from cameras, phones and memory
cards and generate meaningful file and folder names.")
    (license license:gpl2+)))

(define-public libraw
  (package
    (name "libraw")
    (version "0.21.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.libraw.org/data/LibRaw-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00sbscniqrwj341gyvzkgcidfkmscgxx05s4dsplp186680qhwpy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libjpeg-turbo))     ;for lossy DNGs and old Kodak cameras
    (propagated-inputs
     (list lcms))                 ;for color profiles
    (home-page "https://www.libraw.org")
    (synopsis "Raw image decoder")
    (description
     "LibRaw is a library for reading RAW files obtained from digital photo
cameras (CRW/CR2, NEF, RAF, DNG, and others).")
    ;; LibRaw is distributed under both LGPL2.1 and CDDL 1.0.  From the README:
    ;; "You may use one of these licensing modes and switch between them.  If
    ;; you modify LibRaw source and made your changes public, you should accept
    ;; both two licensing modes for your changes/additions."
    (license (list license:lgpl2.1 license:cddl1.0))))

(define-public libexif
  (package
    (name "libexif")
    (version "0.6.24")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libexif/libexif.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zi5vvb0khlzc6xyfayk6mjx5lgkrj8r7s8lfv4j7wkcgndjga0j"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake gettext-minimal libtool))
    (home-page "https://github.com/libexif/libexif")
    (synopsis "Read and manipulate EXIF data in digital photographs")
    (description
     "The libexif C library allows applications to read, edit, and save EXIF
data as produced by digital cameras.")
    (license license:lgpl2.1+)))

(define-public libgphoto2
  (package
    (name "libgphoto2")
    (version "2.5.30")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gphoto/libgphoto/"
                                  version "/libgphoto2-" version ".tar.bz2"))
              (sha256
               (base32
                "1d0g3ixxfz3sfm5rzibydqd9ccflls86pq0ls48zfp5dqvda2qgf"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs
     (list libjpeg-turbo libltdl libusb libxml2))
    (propagated-inputs
     (list ;; The .pc refers to libexif.
           libexif))
    (home-page "http://www.gphoto.org/proj/libgphoto2/")
    (synopsis "Accessing digital cameras")
    (description
     "This is the library backend for gphoto2.  It contains the code for PTP,
MTP, and other vendor specific protocols for controlling and transferring data
from digital cameras.")

    ;; 'COPYING' says LGPLv2.1+, but in practices files are under LGPLv2+.
    (license license:lgpl2.1+)))

(define-public gphoto2
  (package
    (name "gphoto2")
    (version "2.5.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gphoto/gphoto/" version
                                  "/gphoto2-" version ".tar.bz2"))
              (sha256
               (base32
                "0xbki37q9ja34igidr2vj0ps1lp7sfz4xpsmh8h9x89dy76qsr1a"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list readline libjpeg-turbo popt libexif libgphoto2))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'pre-check
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "tests/data" "\\.param$")
               (("/usr/bin/env")
                (which "env"))))))
       ;; FIXME: There is 1 test failure, most likely related to the build
       ;; environment.
       #:tests? #f))
    (home-page "http://www.gphoto.org/")
    (synopsis "Command-line tools to access digital cameras")
    (description
     "Gphoto2 is a set of command line utilities for manipulating a large
number of different digital cameras.  Through libgphoto2, it supports PTP,
MTP, and much more.")

    ;; Files are typically under LGPLv2+, but 'COPYING' says GPLv2+.
    (license license:gpl2+)))

;; Note: See <https://metacpan.org/pod/Image::ExifTool> for the latest
;; release.  The versions at <https://www.sno.phy.queensu.ca/~phil/exiftool/>
;; are not meant for production use according to the Changes file.
(define-public perl-image-exiftool
  (package
    (name "perl-image-exiftool")
    (version "12.70")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "mirror://cpan/authors/id/E/EX/EXIFTOOL/"
                            "Image-ExifTool-" version ".tar.gz")
             ;; New releases may take a while to hit CPAN.
             (string-append "https://www.sno.phy.queensu.ca/~phil/exiftool/"
                            "Image-ExifTool-" version ".tar.gz")))
       (sha256
        (base32
         "1zmg5jsdqmr9mnmxg614brdgr9ddmspcc11rs4xkygnc8lj55cjc"))))
    (build-system perl-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'post-install
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Make sure the 'exiftool' commands finds the library.
                   ;; XXX: Shouldn't it be handled by PERL-BUILD-SYSTEM?
                   (let* ((pm  (find-files #$output "^ExifTool\\.pm$"))
                          (lib (dirname (dirname (car pm)))))
                     (wrap-program (string-append #$output "/bin/exiftool")
                       `("PERL5LIB" prefix (,lib)))))))))
    (inputs (list bash-minimal))
    (home-page "https://metacpan.org/release/Image-ExifTool")
    (synopsis "Program and Perl library to manipulate EXIF and other metadata")
    (description "This package provides the @code{exiftool} command and the
@code{Image::ExifTool} Perl library to manipulate EXIF tags of digital images
and a wide variety of other metadata.")
    (license license:perl-license)))

(define-public libpano13
  (package
    (name "libpano13")
    (version "2.9.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/panotools/libpano13/"
                                  "libpano13-" (first
                                                (string-split version #\_))
                                  "/libpano13-" version ".tar.gz"))
              (sha256
               (base32
                "141mccp4klj0qdpvki97q5wjf5a1b7pj09s6c4lmwc4r452s3rbr"))))
    (build-system cmake-build-system)
    (native-inputs
     (list perl))                       ; for pod2man
    (inputs
     (list libjpeg-turbo libpng libtiff zlib))
    (home-page "https://panotools.sourceforge.net/")
    (synopsis "Library for panoramic images")
    (description
     "The libpano13 package contains the backend library written by the
Panorama Tools project for building panoramic images from a set of
overlapping images, as well as some command line tools.")
    (license license:gpl2+)))

(define-public enblend-enfuse
  (package
    (name "enblend-enfuse")
    (version "4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/enblend/"
                                  name "/"
                                  name "-" (version-major+minor version) "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "0j5x011ilalb47ssah50ag0a4phgh1b0wdgxdbbp1gcyjcjf60w7"))
              (patches
               ;; TODO: Remove when updating.
               ;; Fixed upstream with a98e00eed893f62dd8349fc2894abca3aff4b33a.
               (search-patches "enblend-enfuse-reproducible.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; TODO: Remove when updating.
               ;; Fixed upstream with 81e25afe71146aaaf5058c604034f35d57e3be9d.
               #~(substitute* "src/minimizer.cc"
                   (("^#include <gsl/gsl_errno\\.h>" all)
                    (string-append all "\n#include <limits>"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config
           perl
           perl-timedate
           help2man
           ;; For building the documentation.
           gnuplot
           graphviz-minimal  ; for 'dot'
           font-ghostscript
           imagemagick/stable
           (librsvg-for-system)
           m4
           perl-readonly
           texlive-texloganalyser
           (texlive-updmap.cfg
            (list texlive-bold-extra
                  texlive-cm-mf-extra-bold
                  texlive-comment
                  texlive-float
                  texlive-enumitem
                  texlive-mdwtools
                  texlive-hyphenat
                  texlive-index
                  texlive-listings
                  texlive-microtype
                  texlive-etoolbox  ;used but not propagated by microtype
                  texlive-nag
                  texlive-ragged2e
                  texlive-shorttoc
                  texlive-bigfoot
                  texlive-xstring))
           hevea))
    (inputs
     (list boost
           gsl
           lcms
           libjpeg-turbo
           libpng
           libtiff
           openexr
           vigra
           zlib))
    (arguments
     (list
      #:configure-flags #~(list "--enable-openmp")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'fontconfig-cache
            (lambda _
              (setenv "XDG_CACHE_HOME" (mkdtemp "/tmp/cache-XXXXXX"))))
          ;; XXX: There's some extreme sillyness when building the
          ;; documentation. It gets rebuilt thrice, during build, check and
          ;; install, possibly due to the effects of the invocation of
          ;; UPDATED_ON in doc/Makefile.
          ;; Reported: <URL:https://bugs.launchpad.net/enblend/+bug/2036319>
          (add-after 'configure 'exclude-doc-from-check
            (lambda _
              (substitute* "doc/Makefile"
                (("^(check:).+$" _ rule)
                 (string-append rule "\n")))))
          ;; XXX: Skip building the docs since they're rebuilt again
          ;; during install.
          (replace 'build
            (lambda args
              (with-directory-excursion "src"
                (apply (assoc-ref %standard-phases 'build) args))))
          ;; XXX: Save another doc rebuild when installing.
          (replace 'install
            ;; Intercept and insert a make-flag for this phase only.
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "install"
                     (cons "MAYBE_DOC=" make-flags))))
          ;; XXX: 'make install' doesn't install the docs.
          (add-after 'install 'install-doc
            (lambda* (#:key make-flags #:allow-other-keys)
              ;; Install examples first, for which the 'install' rule works.
              (with-directory-excursion "doc/examples"
                (apply invoke "make" "install" make-flags))
              ;; The docs have to be installed with specific rules.
              (with-directory-excursion "doc"
                (apply invoke "make"
                       "install-ps-local"
                       "install-html-local"
                       "install-dvi-local"
                       ;; Do not overwhelm the console by printing the source
                       ;; to stdout.
                       (cons "V=0" make-flags))))))))
    (outputs '("out" "doc"))
    (home-page "https://enblend.sourceforge.net/")
    (synopsis "Tools for combining and blending images")
    (description
     "Enblend blends away the seams in a panoramic image mosaic using a
multi-resolution spline.  Enfuse merges different exposures of the same
scene to produce an image that looks much like a tone-mapped image.")
    (license license:gpl2+)))

(define-public lensfun
  (package
    (name "lensfun")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lensfun/lensfun")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lwf3cwldvh9qfmh3w7nqqildfmxx2i5f5bn0vr8y6qc5kh7a1s9"))))
    (build-system cmake-build-system)
    (arguments
     `(,@(if (any (cute string-prefix? <> (or (%current-system)
                                              (%current-target-system)))
                  '("x86_64" "i686"))
        ;; SSE and SSE2 are supported only on Intel processors.
        '()
        '(#:configure-flags '("-DBUILD_FOR_SSE=OFF" "-DBUILD_FOR_SSE2=OFF")))
       #:tests? #f)) ; There are no tests to run.
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib))
    (home-page "https://lensfun.github.io/")
    (synopsis "Library to correct optical lens defects with a lens database")
    (description "Digital photographs are not ideal.  Of course, the better is
your camera, the better the results will be, but in any case if you look
carefully at shots taken even by the most expensive cameras equipped with the
most expensive lenses you will see various artifacts.  It is very hard to make
ideal cameras, because there are a lot of factors that affect the final image
quality, and at some point camera and lens designers have to trade one factor
for another to achieve the optimal image quality, within the given design
restrictions and budget.  But we all want ideal shots, don't we?  So that's
what's Lensfun is all about: rectifying the defects introduced by your
photographic equipment.")
    ;; The libraries are licensed under the LGPL3, the programs are
    ;; licensed GPL3, and the database is license CC-BY-SA 3.0.  See the
    ;; README.md file for this clarification.
    (license (list license:lgpl3 license:gpl3 license:cc-by-sa3.0))))

(define-public darktable
  (package
    (name "darktable")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/darktable-org/darktable/releases/"
             "download/release-" version "/darktable-" version ".tar.xz"))
       (sha256
        (base32 "116rdmxl2csxysghm4h9h1rwms6pqcawf351czpq7adv9q4qv4aa"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBINARY_PACKAGE_BUILD=On"
              "-DBUILD_TESTING=On"
              "-DDONT_USE_INTERNAL_LIBRAW=On")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'libOpenCL-path
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Statically link to libOpenCL.
              (substitute* "./src/common/dlopencl.c"
                (("\"libOpenCL\"")
                 (string-append "\""
                                (search-input-file inputs "/lib/libOpenCL.so")
                                "\"")))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/darktable")
                ;; For GtkFileChooserDialog.
                `("GSETTINGS_SCHEMA_DIR" =
                  (,(string-append #$(this-package-input "gtk+")
                                   "/share/glib-2.0/schemas")))))))))
    (native-inputs
     (list cmocka
           desktop-file-utils
           gcc-13             ; gcc-11 too old for darktable, 12+ required
           `(,glib "bin")
           gobject-introspection
           intltool
           llvm
           opencl-headers
           perl
           pkg-config
           po4a
           python-wrapper
           ruby))
    (inputs
     (list bash-minimal
           cairo
           colord-gtk                   ;optional, for color profile support
           cups                         ;optional, for printing support
           curl
           dbus-glib
           exiv2
           gmic                         ;optional, for HaldcLUT support
           graphicsmagick
           gsettings-desktop-schemas
           gtk+
           imath
           iso-codes/pinned          ;optional, for language names in the preferences
           json-glib
           lcms
           lensfun                   ;optional, for the lens distortion plugin
           libgphoto2                ;optional, for camera tethering
           libavif                   ;optional, for AVIF support
           libjpeg-turbo
           libjxl                    ;optional, for JPEG-XL support
           libomp
           libpng
           libraw
           (librsvg-for-system)
           libsecret                    ;optional, for storing passwords
           libsoup-minimal-2            ;optional, for osm-gps-map
           libtiff
           libwebp                      ;optional, for WebP support
           libxml2
           libxslt
           libheif
           lua-5.4                      ;optional, for plugins
           opencl-icd-loader            ;optional, for OpenCL support
           openexr                      ;optional, for EXR import/export
           openjpeg                     ;optional, for JPEG2000 export
           osm-gps-map                  ;optional, for geotagging view
           portmidi                 ;optional, for hardware MIDI input devices
           pugixml
           python-jsonschema
           sdl2
           sqlite))
    (home-page "https://www.darktable.org")
    (synopsis "Virtual lighttable and darkroom for photographers")
    (description "Darktable is a photography workflow application and RAW
developer.  It manages your digital negatives in a database, lets you view
them through a zoomable lighttable and enables you to develop raw images
and enhance them.")
    ;; See src/is_supported_platform.h for supported platforms.
    (supported-systems '("x86_64-linux" "aarch64-linux" "powerpc64le-linux"
                         "riscv64-linux"))
    (properties
     '((release-monitoring-url . "https://github.com/darktable-org/darktable/releases")))
    (license (list license:gpl3+        ;Darktable itself
                   license:lgpl2.1+)))) ;Rawspeed library

(define-public photoflare
  (package
    (name "photoflare")
    (version "1.6.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/photoflare/photoflare")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sm4m9nga1lyqycgqbh08cib5dg4fnrz9qkrliycr3dbisy360lm"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((magickpp (assoc-ref inputs "graphicsmagick"))
                   (out (assoc-ref outputs "out")))
               (invoke "qmake"
                       (string-append "INCLUDEPATH += " magickpp
                                      "/include/GraphicsMagick")
                       (string-append "PREFIX=" out)
                       "Photoflare.pro")))))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list graphicsmagick libomp qtbase-5))
    (home-page "https://photoflare.io")
    (synopsis "Quick, simple but powerful image editor")
    (description "Photoflare is a cross-platform image editor with an aim
to balance between powerful features and a very friendly graphical user
interface.  It suits a wide variety of different tasks and users who value a
more nimble workflow.  Features include basic image editing capabilities,
paint brushes, image filters, colour adjustments and more advanced features
such as Batch image processing.")
    (license license:gpl3+)))

(define-public entangle
  (package
    (name "entangle")
    (version "3.0")    ; delete the 'build-with-meson-0.60 phase when updating
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/entangle/entangle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pdmgxjdb3xlcqsaz7l8qzj5f7g7nwzhsrgid8929bm36d49cgc7"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'build-with-meson-0.60
            ;; Work around ‘ERROR: Function does not take positional arguments.’.
            (lambda _
              (substitute* "src/meson.build"
                (("^i18n\\.merge_file.*" match)
                 (string-append match "  data_dirs:")))))
          (add-after 'unpack 'skip-gtk-update-icon-cache
            ;; Don't create 'icon-theme.cache'.
            (lambda _
              (substitute* "meson_post_install.py"
                (("gtk-update-icon-cache") "true"))))
          (add-after 'install 'wrap-gi-python
            ;; Make GTK find files needed by plugins.
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((gi-typelib-path (getenv "GI_TYPELIB_PATH"))
                    (python-path     (getenv "GUIX_PYTHONPATH")))
                (wrap-program (string-append #$output "/bin/entangle")
                  `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path))
                  `("GUIX_PYTHONPATH" ":" prefix (,python-path)))))))))
    (native-inputs
     (list cmake
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           gtk-doc/stable
           itstool
           libxml2
           perl
           pkg-config))
    (inputs
     (list bash-minimal
           gdk-pixbuf
           gexiv2
           gst-plugins-base
           gstreamer
           gtk+
           lcms
           libgphoto2
           libgudev
           libpeas
           libraw
           python
           python-pygobject))
    (home-page "https://entangle-photo.org/")
    (synopsis "Camera control and capture")
    (description
     "Entangle is an application which uses GTK and libgphoto2 to provide a
graphical interface for tethered photography with digital cameras.  It
includes control over camera shooting and configuration settings and 'hands
off' shooting directly from the controlling computer.")
    (license license:gpl3+)))

(define-public hugin
  (package
    (name "hugin")
    (version "2024.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/hugin/hugin/hugin-"
                                  (version-major+minor version)
                                  "/hugin-" version ".tar.bz2"))
              (sha256
               (base32
                "1r57bgq9dr2wi182vl6qm2kgaz2f6wz8sxikr14k3djfxgg0rv0k"))))
    (build-system cmake-build-system)
    (native-inputs
     (list gettext-minimal pkg-config))
    (inputs
     (list boost
           enblend-enfuse
           exiv2
           fftw
           flann
           freeglut
           glew
           lcms
           libjpeg-turbo
           libpano13
           libpng
           libtiff
           libxi
           libxmu
           mesa
           openexr
           sqlite
           vigra
           wxwidgets
           zlib))
    (arguments
     (list
      #:tests? #f                      ; no check target
      #:configure-flags
      #~(list
         ;; Disable installation of the Python scripting interface.
         ;; It would require the additional inputs python and swig.
         ;; Installation would need to be tweaked, as it tries to install
         ;; into the python directory.
         "-DBUILD_HSI=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'substitute
            (lambda _
              (substitute* "src/hugin1/base_wx/StitchingExecutor.cpp"
                (("wxT\\(\"enblend\"\\)")
                 (string-append "wxT(\"" (which "enblend") "\")"))
                (("wxT\\(\"enfuse\"\\)")
                 (string-append "wxT(\"" (which "enfuse") "\")"))))))))
    (home-page "https://hugin.sourceforge.net/")
    (synopsis "Panorama photo stitcher")
    (description
     "Hugin is an easy to use panoramic imaging toolchain with a graphical
user interface.  It can be used to assemble a mosaic of photographs into
a complete panorama and stitch any series of overlapping pictures.")
    (license license:gpl2+)))

(define-public rawtherapee
  (package
    (name "rawtherapee")
    (version "5.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://rawtherapee.com/shared/source/"
                                  "rawtherapee-" version ".tar.xz"))
              (sha256
               (base32
                "0977dnik78szwznl4knabigah0m394a4gdmjajcy4b8ixj6w3175"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                      ; no test suite
      #:build-type "release"
      #:configure-flags
      #~(list (string-append "-DLENSFUNDBDIR="
                             #$(this-package-input "lensfun")
                             "/share/lensfun")
              ;; Don't optimize the build for the host machine. See the file
              ;; 'ProcessorTargets.cmake' in the source distribution for more
              ;; information.
              "-DPROC_TARGET_NUMBER=1"
              ;; These flags are recommended by upstream for distributed packages.
              ;; See the file 'RELEASE_NOTES.txt' in the source distribution.
              "-DCMAKE_CXX_FLAGS=-O3 -fPIC"
              "-DCMAKE_C_FLAGS=-O3 -fPIC"
              "-DCACHE_NAME_SUFFIX=\"\""
              "-DWITH_JXL=ON"
              "-DWITH_SYSTEM_LIBRAW=ON")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list expat
           exiv2
           fftwf
           glib
           glibmm
           gtk+
           gtkmm-3
           lcms
           lensfun
           libcanberra
           libiptcdata
           libjpeg-turbo
           libjxl
           libpng
           (librsvg-for-system)
           libraw
           libsigc++
           libtiff
           zlib))
    (home-page "https://rawtherapee.com")
    (synopsis "Raw image developing and processing")
    (description "RawTherapee is a raw image processing suite.  It comprises a
subset of image editing operations specifically aimed at non-destructive raw
photo post-production and is primarily focused on improving a photographer's
workflow by facilitating the handling of large numbers of images.  Most raw
formats are supported, including Pentax Pixel Shift, Canon Dual-Pixel, and those
from Foveon and X-Trans sensors.")
    (license license:gpl3+)))

(define-public librtprocess
  (package
    (name "librtprocess")
    (version "0.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CarVac/librtprocess")
                    (commit version)))
              (sha256
               (base32
                "0v0zwbdbc1fn7iy6wi0m6zgb86qdx1ijnv548d0ydbr8cm4klnpz"))
              (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     ;; No tests
     (list #:tests? #f))
    (home-page "https://github.com/CarVac/librtprocess")
    (synopsis "Highly optimized library for processing RAW images")
    (description
     "This package provides RawTherapee's highly optimized RAW processing routines.")
    (license license:gpl3+)))
