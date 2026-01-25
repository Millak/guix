;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darringon <jmd@gnu.org>
;;; Copyright © 2016, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018, 2019, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2024, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages aidc)
  #:use-module (gnu packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages video)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system glib-or-gtk))

(define-public python-pyzbar
  (package
    (name "python-pyzbar")
    (version "0.1.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/NaturalHistoryMuseum/pyzbar")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1df1dvr8i2wyr2vw5pq8rlz2wm4xqda0wbgja19bvql1m9im11ph"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; This tests if find_library was called once, but we remove the call in
      ;; the stage below to make the library find libzbar.
      #~(list #$@(map (lambda (test) (string-append "--deselect="
                                                    "pyzbar/tests/"
                                                    "test_zbar_library.py::"
                                                    "TestLoad::"
                                                    test))
                      (list "test_found_non_windows"
                            "test_not_found_non_windows")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-library-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "pyzbar/zbar_library.py"
                (("find_library\\('zbar'\\)")
                 (format #f "~s"
                         (search-input-file inputs "/lib/libzbar.so.0")))))))))
    (native-inputs
     (list pkg-config
           python-numpy
           python-pillow
           python-pytest
           python-setuptools))
    (inputs
     (list zbar))
    (home-page "https://github.com/NaturalHistoryMuseum/pyzbar/")
    (synopsis "Read one-dimensional barcodes and QR codes")
    (description
     "Read one-dimensional barcodes and QR codes using the zbar library.

Features:

@itemize
@item Pure python
@item Works with PIL / Pillow images, OpenCV / numpy ndarrays, and raw bytes
@item Decodes locations of barcodes
@item No dependencies, other than the zbar library itself
@end itemize")
    (license license:expat)))

(define-public python-zbarlight
  (package
    (name "python-zbarlight")
    (version "3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Polyconseil/zbarlight")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1radfpfhfhkx3xnz01bqr5l9pl2zv70zis6l2kw1gwqbfw65r6w6"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-pillow))
    (inputs (list zbar))
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://github.com/Polyconseil/zbarlight")
    (synopsis "Simple Python wrapper for the zbar barcode library")
    (description "Zbarlight is a simple wrapper for the zbar library.  It can
read all zbar supported codes.")
    (license license:bsd-3)))

(define-public zint
  (package
    (name "zint")
    (version "2.16.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/zint/zint")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zpv9ar1kfjl3bb27jc8hd8601zcda9yv4dz1jhxfhdsg6v4cdg7"))))
    (build-system cmake-build-system)
    (arguments
     (list #:disallowed-references (list qtbase)
           #:parallel-tests? #f         ;test suite fails otherwise
           #:configure-flags #~(list "-DZINT_TEST=ON"
                                     "-DZINT_UNINSTALL=OFF")))
    (native-inputs (list pkg-config))
    (inputs (list libpng))
    (synopsis "Barcode encoding library")
    (description "Zint is a suite of programs to allow easy encoding of data in
any of the wide range of public domain barcode standards and to allow
integration of this capability into your own programs.")
    (home-page "https://www.zint.org.uk/")
    (license (list license:bsd-3 license:gpl3+))))

(define-public zint-with-qt
  (package/inherit zint
    (name "zint-with-qt")
    (build-system qt-build-system)
    (arguments (substitute-keyword-arguments (package-arguments zint)
                 ((#:disallowed-references references)
                  (delete qtbase references))
                 ((#:qtbase _ #f)
                  qtbase)
                 ((#:configure-flags flags ''())
                  #~(cons "-DZINT_QT6=ON" #$flags))))
    (inputs (modify-inputs (package-inputs zint)
              ;; The UI library of qttools is linked to; hence it must be used
              ;; as an input rather than a native input.
              (append qtsvg qttools)))
    (synopsis "Barcode encoding library (with graphical user interface)")))

(define-public zxing-cpp
  (package
    (name "zxing-cpp")
    (version "2.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zxing-cpp/zxing-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hdr73fllnsp3zpmrhw6cjla39lihwg1khgvddsf4v57a0lmiy3f"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DZXING_READERS=ON"
              "-DZXING_WRITERS=BOTH"
              "-DZXING_DEPENDENCIES=LOCAL"
              "-DZXING_EXAMPLES=OFF" ;requires stb.pc
              "-DZXING_USE_BUNDLED_ZINT=OFF"
              "-DZXING_UNIT_TESTS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-headers
            (lambda* (#:key source #:allow-other-keys)
              (invoke "sh" "-c"
                           (string-append "cp " source "/core/src/*.h "
                                                #$output "/include/ZXing/")))))))
    (native-inputs (list fmt-8 googletest pkg-config))
    (inputs (list libpng zint zlib))
    (synopsis "C++ port of ZXing")
    (description "ZXing-CPP is a barcode scanning library.")
    (home-page "https://github.com/zxing-cpp/zxing-cpp")
    (license license:asl2.0)))

;;; This older variant is kept for gst-plugins-bad (see:
;;; https://gitlab.freedesktop.org/gstreamer/gst-plugins-bad/-/issues/1684).
(define-public zxing-cpp-1.2
  (package/inherit zxing-cpp
    (name "zxing-cpp")
    (version "1.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zxing-cpp/zxing-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "zxing-cpp-1.2.0-gcc-14.patch"))
              (sha256
               (base32
                "1gjj9c7h634rrmmgzbc7cxjqsxdq0paj6113k02ncjm1s9abk7ik"))))
    ;; Disable tests to avoid bundled dependencies.
    (arguments '(#:tests? #f
                 #:configure-flags '("-DBUILD_BLACKBOX_TESTS=OFF")))
    (native-inputs '())))

(define-public barcode
  (package
    (name "barcode")
    (version "0.99")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/barcode/barcode-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1indapql5fjz0bysyc88cmc54y8phqrbi7c76p71fgjp45jcyzp8"))))
    (build-system gnu-build-system)
    (arguments
      ;; Fix build with GCC 10.
     '(#:configure-flags '("CFLAGS=-fcommon")))
    (synopsis "Convert text strings to printed bars in various standards")
    (description "GNU Barcode is a flexible tool to produce printed barcodes
from text strings.  It supports a variety of encoding standards and sizing
measurements.  Barcodes can be output in PostScript or Encapsulated PostScript
formats.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/barcode/")))

(define-public qrencode
  (package
    (name "qrencode")
    (version "4.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://fukuchi.org/works/qrencode/"
                                  "qrencode-" version ".tar.bz2"))
              (sha256
               (base32
                "08v9d8jn26bva2a8x4hghq3mgl8zcid393iqkidwyhc05xrxjmg4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--with-tests")
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "tests"
                 (invoke "./test_basic.sh")))
             #t)))))
    (inputs (list libpng))
    (native-inputs (list pkg-config))
    (synopsis "Encode data into a QR Code symbol")
    (description "Libqrencode is a C library for encoding data in a QR Code
symbol, a kind of 2D symbology that can be scanned by handy terminals such as
a mobile phone with CCD.  The capacity of QR Code is up to 7000 digits or 4000
characters, and is highly robust.")
    (license license:lgpl2.1+)
    (home-page "https://fukuchi.org/works/qrencode")))

(define-public libdmtx
  (package
    (name "libdmtx")
    (version "0.7.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmtx/libdmtx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s95gplvb6x7gnl48yn7ywa9r15lfm8k2m60wm9i7w75ay4bq32i"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-static")))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://github.com/dmtx")
    (synopsis "Library for reading and writing Data Matrix 2D barcodes")
    (description "libdmtx is software for reading and writing Data Matrix 2D
barcodes of the modern ECC200 variety.  libdmtx is a shared library, allowing
C/C++ programs to use its capabilities without restrictions or overhead.")
    (license license:bsd-3)))

(define-public dmtx-utils
  (package
    (name "dmtx-utils")
    (version "0.7.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dmtx/dmtx-utils/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        "06m3qncqdlcnmw83n95yrx2alaq6bld320ax26z4ndnla41yk0p4")))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool pkg-config))
    (inputs (list libdmtx imagemagick libjpeg-turbo))
    (home-page "https://github.com/dmtx/dmtx-utils/")
    (synopsis "Utilities for working with Data Matrix barcodes")
    (description
     "libdmtx is a software library that enables programs to read and write Data
Matrix barcodes of the modern ECC200 variety.  This package, dmtx-utils,
provides command line utilities that allow scripts and command line users to
use libdmtx functionality.

This package includes the following programs:
@enumerate
@item @code{dmtxquery}: Extract information from dmtxread output.
@item @code{dmtxread}: Scan Data Matrix barcodes.
@item @code{dmtxwrite}: Create Data Matrix barcodes.
@end enumerate")
    (license (list license:lgpl2.1 license:gpl2))))

;; XXX: qt variant utils are broken: zbarcam-qt fails with segmentation fault.
(define-public zbar
  (package
    (name "zbar")
    (version "0.23.93")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/mchehab/zbar")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x71clkkm4w765c2d5h3svr29w08dj03r6785f9jwqx5r4ral0za"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:configure-flags '(list "--disable-static"
                               "--with-gtk=auto"
                               "--with-python=auto"
                               (string-append "--with-dbusconfdir="
                                              (assoc-ref %outputs "out")
                                              "/etc"))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           libtool
           patchelf
           pkg-config
           python-wrapper))
    (inputs
     (list dbus
           imagemagick
           libjpeg-turbo
           perl
           python
           v4l-utils-minimal))
    (propagated-inputs
     ;; These are in 'requires' field of .pc files.
     (list glib gtk+))
    (synopsis "Bar code reader")
    (description
     "ZBar can read barcodes from various sources, such as video streams,
image files, and raw intensity sensors.  It supports EAN-13/UPC-A, UPC-E,
EAN-8, Code 128, Code 93, Code 39, Codabar, Interleaved 2 of 5, QR Code and SQ
Code.  Included with the library are basic applications for decoding captured
bar code images and using a video device (e.g. webcam) as a bar code scanner.
For application developers, language bindings are included for C, C++ and
Python as well as GUI widgets for GTK and Qt.")
    (home-page "https://github.com/mchehab/zbar")
    (license license:lgpl2.1+)))

(define-public zbar-minimal
  (package/inherit zbar
    (name "zbar-minimal")
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments zbar)
       ((#:configure-flags flags)
        #~(cons* "--with-gtk=no" (delete "--with-gtk=auto" #$flags)))
       ((#:disallowed-references _ '())
        (list qtbase gtk+))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs zbar)
       (delete "gtk+")))))

(define-public qrcodegen-cpp
  (package
    (name "qrcodegen-cpp")
    (version "1.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/nayuki/QR-Code-generator")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "qrcodegen-cpp-cmake.patch"))
              (sha256
               (base32
                "0dk9ci5gchxa8gh0hyhlj3d5jwxqlnfm85xyp791ldaia14bkj39"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~'("-DBUILD_TESTS=ON"
               "-DBUILD_SHARED_LIBS=ON")))
    (synopsis "QR Code generator library")
    (description "qrcodegen-cpp is a QR code generator library in C++.  The
project also offers Java, Javascript, Python, C, and Rust implementations.")
    (home-page "https://www.nayuki.io/page/qr-code-generator-library")
    (license license:expat)))
