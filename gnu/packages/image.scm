;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2017, 2019, 2021-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013, 2015, 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Alex Kost <alezost@gmail.com>
;;; Copyright © 2014, 2016, 2017, 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;; Copyright © 2015 Amirouche Boubekki <amirouche@hypermove.net>
;;; Copyright © 2014, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2016, 2017, 2018, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016, 2017, 2020, 2021, 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2016, 2017 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017,2019,2020 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018-2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2020 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2020, 2021, 2022, 2023, 2024 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Alexandr Vityazev <avityazev@posteo.org>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2022-2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2023, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 chris <chris@bumblehead.com>
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

(define-module (gnu packages image)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mcrypt)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages ragel)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages fonts)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system scons)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1))

(define-public converseen
  (package
    (name "converseen")
    (version "0.12.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Faster3ck/Converseen")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gjg2ma8v8pwldny4j2ag92g5zrv5cz511mq44qr7akjsddq6q6p"))
              (patches
               (search-patches "converseen-hide-updates-checks.patch"
                               ;; Remove links to sites relying on non-free
                               ;; Javascript.
                               "converseen-hide-non-free-pointers.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #false                   ;no tests
      #:configure-flags #~(list "-DUSE_QT6=yes")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-translations-location
            ;; Fix translations location.  Without this, only English is
            ;; offered.
            (lambda _
              (substitute* "src/translator.cpp"
                (("QString\\(\"%1/share/converseen/loc\"\\).arg\\(rootPath\\)")
                 (string-append "QString(\""
                                #$output
                                "/share/converseen/loc\")"))))))))
    (native-inputs
     (list pkg-config qttools))
    (inputs
     (list imagemagick qtbase))
    (home-page "https://converseen.fasterland.net/")
    (synopsis "Batch image converter and resizer")
    (description
     "Converseen is an image batch conversion tool.  You can resize and
convert images in more than 100 different formats.")
    (license license:gpl3+)))

(define-public iqa
  (package
    (name "iqa")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "mirror://sourceforge/iqa/"
                       version " Release" "/iqa_" version "_src.tar.gz"))
       (sha256
        (base32 "00mgwy031ammab6bwmd1whhvqv3fxy1cs1igabq0n3ag12zhjs77"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (install-file "build/debug/libiqa.a" lib)
               #t))))))
    (synopsis "Image Quality Assessment")
    (description "IQA is a C library for objectively measuring image/video
quality.  It implements many popular algorithms, such as MS-SSIM, MS-SSIM*,
SIMM, MSE, and PSNR.  It is designed to be fast, accurate, and reliable.  All
code is Valgrind-clean and unit tested.")
    (home-page "https://sourceforge.net/projects/iqa/")
    (license license:bsd-4)))

(define-public libpng
  (package
   (name "libpng")
   (version "1.6.39")  ; Remember to also update libpng-apng if possible!
   (source (origin
            (method url-fetch)
            (uri (list (string-append "mirror://sourceforge/libpng/libpng16/"
                                      version "/libpng-" version ".tar.xz")
                       (string-append
                        "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                        "/libpng16/libpng-" version ".tar.xz")
                       (string-append
                        "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                        "/libpng16/libpng-" version ".tar.xz")))
            (sha256
             (base32
              "0dv90dxvmqpk7mbywyjbz8lh08cv4b0ksqp1y62mzvmlf379cihz"))))
   (build-system gnu-build-system)
   (arguments
    `(#:configure-flags '("--disable-static")))

   ;; libpng.la says "-lz", so propagate it.
   (propagated-inputs (list zlib))

   (synopsis "Library for handling PNG files")
   (description
    "Libpng is the official PNG (Portable Network Graphics) reference
library.  It supports almost all PNG features and is extensible.")
   (license license:zlib)
   (home-page "http://www.libpng.org/pub/png/libpng.html")))

(define-public libpng-apng
  ;; The APNG patch is maintained separately and may lag behind upstream libpng.
  (package
    (name "libpng-apng")
    (version "1.6.39")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/libpng/libpng16/"
                                 version "/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                   "/libpng16/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                   "/libpng16/libpng-" version ".tar.xz")))
       (sha256
        (base32 "0dv90dxvmqpk7mbywyjbz8lh08cv4b0ksqp1y62mzvmlf379cihz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (srfi srfi-1))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-apng
           (lambda* (#:key inputs #:allow-other-keys)
             (define (apply-patch file)
               (invoke "patch" "-p1" "--force"
                       "--input" file))
             (let ((apng.gz (assoc-ref inputs "apng")))
               (format #t "Applying APNG patch '~a'...~%"
                       apng.gz)
               (invoke "sh" "-c"
                       (string-append "gunzip < " apng.gz " > the-patch"))
               (apply-patch "the-patch"))))
         (add-before 'configure 'no-checks
           (lambda _
             (substitute* "Makefile.in"
               (("^scripts/symbols.chk") "")
               (("check: scripts/symbols.chk") "")))))))
    (inputs
     `(("apng" ,(origin
                  (method url-fetch)
                  (uri
                   (string-append "mirror://sourceforge/libpng-apng/libpng16/"
                                  version "/libpng-" version "-apng.patch.gz"))
                  (sha256
                   (base32
                    "1z8cx011a2c7vagwgi92rbmky1wi8awmrdldqh9f5k80pbmbdi2a"))))))
    (native-inputs
     (list libtool))
    ;; libpng.la says "-lz", so propagate it.
    (propagated-inputs
     (list zlib))
    (synopsis "APNG patch for libpng")
    (description
     "APNG (Animated Portable Network Graphics) is an unofficial
extension of the APNG (Portable Network Graphics) format.
APNG patch provides APNG support to libpng.")
    (home-page "https://sourceforge.net/projects/libpng-apng/")
    (license license:zlib)))

;; Temporary, until 76798 merges into core-packages-team, and that merges into
;; master.
(define-public libpng-apng-for-librewolf
  (hidden-package
   (package
     (inherit libpng-apng)
     (version "1.6.46")
     (source
      (origin
        (method url-fetch)
        (uri (list (string-append "mirror://sourceforge/libpng/libpng16/"
                                  version "/libpng-" version ".tar.xz")
                   (string-append
                    "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                    "/libpng16/libpng-" version ".tar.xz")
                   (string-append
                    "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                    "/libpng16/libpng-" version ".tar.xz")))
        (sha256
         (base32
          "1cbwf20zlm4gcv8rpjivkngrjgl5366w21lr9qmbk2lr0dq8papk"))))
     (inputs
      (modify-inputs (package-inputs libpng-apng)
        (replace "apng"
          (origin
            (method url-fetch)
            (uri
             (string-append "mirror://sourceforge/libpng-apng/libpng16/"
                            version "/libpng-" version "-apng.patch.gz"))
            (sha256
             (base32
              "00ykl1bzb79xsjwrq7dl0yz9dz5g3zwj0lry5zam3vs6s3gw5gi9")))))))))

(define-public pngcrush
  (package
   (name "pngcrush")
   (version "1.8.13")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://sourceforge/pmt/pngcrush/"
                                version "/pngcrush-" version "-nolib.tar.xz"))
            (sha256 (base32
                     "0l43c59d6v9l0g07z3q3ywhb8xb3vz74llv3mna0izk9bj6aqkiv"))))
   (build-system gnu-build-system)
   (arguments
    '(#:tests? #f ; no check target
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (substitute* "Makefile"
              (("^(PNG(INC|LIB) = )/usr/local/" line vardef)
               (string-append vardef (assoc-ref inputs "libpng") "/"))
              (("^(Z(INC|LIB) = )/usr/local/" line vardef)
               (string-append vardef (assoc-ref inputs "zlib") "/"))
              ;; The Makefile is written by hand and not using $PREFIX
              (("\\$\\(DESTDIR\\)/usr/")
               (string-append (assoc-ref outputs "out") "/")))
            #t)))))
   (inputs
    (list libpng zlib))
   (home-page "https://pmt.sourceforge.io/pngcrush")
   (synopsis "Utility to compress PNG files")
   (description "Pngcrush optimizes @acronym{PNG, Portable Network Graphics}
images.  It can further losslessly compress them by as much as 40%.")
   (license license:zlib)))

(define-public pngcheck
  (package
    (name "pngcheck")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.libpng.org/pub/png/src/pngcheck-" version
                    ".tar.gz"))
              (sha256
               (base32
                "1rny14v57d2zvnqcqbh3m87mkya22qr2394fg7vm3xsacf8l8sn3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ;no check target
       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "make" "-f" "Makefile.unx")))
                  (add-after 'build 'compress-man-pages
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (invoke "gzip" "pngcheck.1")
                      (invoke "gzip" "gpl/pngsplit.1")
                      (invoke "gzip" "gpl/png-fix-IDAT-windowsize.1")))
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin/"))
                             (man (string-append out "/share/man/man1/")))
                        (install-file "pngcheck" bin)
                        (install-file "pngcheck.1.gz" man)
                        (install-file "pngsplit" bin)
                        (install-file "gpl/pngsplit.1.gz" man)
                        (install-file "png-fix-IDAT-windowsize" bin)
                        (install-file "gpl/png-fix-IDAT-windowsize.1.gz" man)))))))
    (inputs (list zlib))
    (home-page "http://www.libpng.org/pub/png/apps/pngcheck.html")
    (synopsis "Print info and check PNG, JNG and MNG files")
    (description
     "@code{pngcheck} verifies the integrity of PNG, JNG and MNG files (by
checking the internal 32-bit CRCs, a.k.a. checksums, and decompressing the image
data); it can optionally dump almost all of the chunk-level information in the image
in human-readable form.  For example, it can be used to print the basic statistics
about an image (dimensions, bit depth, etc.); to list the color and transparency info
in its palette (assuming it has one); or to extract the embedded text annotations.
This is a command-line program with batch capabilities (e.g. @code{pngcheck
*.png}.)

Also includes @code{pngsplit} which can split a PNG, MNG or JNG file into individual,
numbered chunks, and @code{png-fix-IDAT-windowsize} that allows resetting first IDAT's
zlib window-size bytes and fix up CRC to match.")
    ;; "pngsplit" and "png-fix-IDAT-windowsize" are licensed under the terms of
    ;; GNU GPL2+.  See "gpl/COPYING" in the repository."
    (license (list license:x11 license:gpl2+))))

(define-public pnglite
  (let ((commit "11695c56f7d7db806920bd9229b69f230e6ffb38")
        (revision "1"))
    (package
      (name "pnglite")
      ;; The project was moved from sourceforge to github.
      ;; The latest version in sourceforge was 0.1.17:
      ;; https://sourceforge.net/projects/pnglite/files/pnglite/
      ;; No releases are made in github.
      (version (git-version "0.1.17" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dankar/pnglite")
                      (commit commit)))
                (sha256
                 (base32
                  "1lmmkdxby5b8z9kx3zrpgpk33njpcf2xx8z9bgqag855sjsqbbby"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               ;; common build flags for building shared libraries
               (let ((cflags '("-O2" "-g" "-fPIC"))
                     (ldflags '("-shared")))
                 (apply invoke
                        `("gcc"
                          "-o" "libpnglite.so"
                          ,@cflags
                          ,@ldflags
                          "pnglite.c"))
                 #t)))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib/"))
                      (include (string-append out "/include/"))
                      (doc (string-append out "/share/doc/"
                                          ,name "-" ,version "/")))
                 (install-file "libpnglite.so" lib)
                 (install-file "pnglite.h" include)
                 (install-file "README.md" doc)
                 #t))))))
      (inputs (list zlib))
      (home-page "https://github.com/dankar/pnglite")
      (synopsis "Pretty small png library")
      (description "A pretty small png library.
Currently all documentation resides in @file{pnglite.h}.")
      (license license:zlib))))

(define-public libimagequant
  (package
    (name "libimagequant")
    (version "2.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ImageOptim/libimagequant")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00w7fny3xf14cfyhbdnmqyh9ddqdh1irvgzxd35a2z65kp7vnvj0"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f))                    ; no check target
    (home-page "https://pngquant.org/lib/")
    (synopsis "Image palette quantization library")
    (description "libimagequant is a small, portable C library for
high-quality conversion of RGBA images to 8-bit indexed-color (palette)
images.  This library can significantly reduces file sizes and powers pngquant
and other PNG optimizers.")
    (license license:gpl3+)))

(define-public pngquant
  (package
    (name "pngquant")
    (version "2.12.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kornelski/pngquant")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15hanshahxqs6s9fyc3aym02251dcys7bf78g3inp0y233amdbl3"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       '("--with-openmp" "--with-lcms2")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libpng zlib lcms libimagequant))
    (home-page "https://pngquant.org/")
    (synopsis "Utility and library for lossy compressing PNG images")
    (description "pngquant is a PNG compressor that significantly reduces file
sizes by converting images to a more efficient 8-bit PNG format with alpha
channel (often 60-80% smaller than 24/32-bit PNG files).  Compressed images
are fully standards-compliant and are supported by all web browsers and
operating systems.

Features:
@enumerate
@item High-quality palette generation using a combination of vector
      quantization algorithms.
@item Unique adaptive dithering algorithm that adds less noise to images
      than the standard Floyd-Steinberg.
@item Easy to integrate with shell scripts, GUIs and server-side software.
@item Fast mode for real-time processing/large numbers of images.
@end enumerate")
    (license license:gpl3+)))

(define-public ijg-libjpeg
  (package
   (name "libjpeg")
   (version "9d")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.ijg.org/files/jpegsrc.v"
                   version ".tar.gz"))
            (sha256 (base32
                     "0spshb0126m70xpgkqwxic8rw1z6ywlrv2kfx3h37ibczfnac0r3"))))
   (build-system gnu-build-system)
   (synopsis "Library for handling JPEG files")
   (description
    "Libjpeg implements JPEG image encoding, decoding, and transcoding.
JPEG is a standardized compression method for full-color and gray-scale
images.
It also includes programs that provide conversion between the JPEG format and
image files in PBMPLUS PPM/PGM, GIF, BMP, and Targa file formats, as well as
lossless JPEG manipulations such as rotation, scaling or cropping:
@enumerate
@item cjpeg
@item djpeg
@item jpegtran
@item rdjpgcom
@item wrjpgcom
@end enumerate")
   (license license:ijg)
   (home-page "https://www.ijg.org/")))

(define-public libjxr
  (package
    (name "libjxr")
    (version "1.1")
    (source (origin
              ;; We are using the Debian source because CodePlex does not
              ;; deliver an easily downloadable tarball.
              (method url-fetch)
              (uri (string-append "mirror://debian/pool/main/j/jxrlib/jxrlib_"
                                  version ".orig.tar.gz"))
              (sha256
               (base32
                "00w3f3cmjsm3fiaxq5mxskmp5rl3mki8psrf9y8s1vqbg237na67"))
              (patch-flags '("-p1" "--binary"))
              (patches (search-patches "libjxr-fix-function-signature.patch"
                                       "libjxr-fix-typos.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target))
             ;; A substitute* procedure call would be enough to add the -fPIC
             ;; flag if there was no file decoding error.
             ;; The makefile is a "Non-ISO extended-ASCII text, with CRLF line
             ;; terminators" according to the file(1) utility.
             (string-append "CFLAGS=-I. -Icommon/include -Iimage/sys -fPIC "
                            "-D__ANSI__ -DDISABLE_PERF_MEASUREMENT -w -O "))
       #:tests? #f ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ; no configure script
         (add-after 'build 'build-shared-library
           (lambda _
             ;; The Makefile uses optimization level 1, so the same
             ;; level is used here for consistency.
             (invoke ,(cc-for-target) "-shared" "-fPIC" "-O"
                     ;; Common files.
                     "adapthuff.o" "image.o" "strcodec.o" "strPredQuant.o"
                     "strTransform.o" "perfTimerANSI.o"
                     ;; Decoding files.
                     "decode.o" "postprocess.o" "segdec.o" "strdec.o"
                     "strInvTransform.o" "strPredQuantDec.o" "JXRTranscode.o"
                     ;; Encoding files.
                     "encode.o" "segenc.o" "strenc.o" "strFwdTransform.o"
                     "strPredQuantEnc.o"
                     "-o" "libjpegxr.so")
             (invoke ,(cc-for-target) "-shared" "-fPIC" "-O"
                     ;; Glue files.
                     "JXRGlue.o" "JXRMeta.o" "JXRGluePFC.o" "JXRGlueJxr.o"
                     ;; Test files.
                     "JXRTest.o" "JXRTestBmp.o" "JXRTestHdr.o" "JXRTestPnm.o"
                     "JXRTestTif.o" "JXRTestYUV.o"
                     "-o" "libjxrglue.so")))
         ;; The upstream makefile does not include an install phase.
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include/jxrlib")))
               (for-each (lambda (file)
                           (install-file file include)
                           (delete-file file))
                         (append
                          '("jxrgluelib/JXRGlue.h"
                            "jxrgluelib/JXRMeta.h"
                            "jxrtestlib/JXRTest.h"
                            "image/sys/windowsmediaphoto.h")
                          (find-files "common/include" "\\.h$")))
               (for-each (lambda (file)
                           (install-file file lib)
                           (delete-file file))
                         (find-files "." "\\.(a|so)$"))
               (for-each (lambda (file)
                           (install-file file bin)
                           (delete-file file))
                         '("JxrDecApp" "JxrEncApp")))
             #t)))))
    (synopsis "Implementation of the JPEG XR standard")
    (description "JPEG XR is an approved ISO/IEC International standard (its
official designation is ISO/IEC 29199-2). This library is an implementation of that standard.")
    (license
     (license:non-copyleft
      "file://Makefile"
      "See the header of the Makefile in the distribution."))
    (home-page "https://jxrlib.codeplex.com/")))

(define-public jpegoptim
  (package
   (name "jpegoptim")
   (version "1.5.5")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/tjko/jpegoptim")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32 "18zq7ada7n17vgkkcixpisxsbs7i8xp5qjp78hyyvmmb9dqy97fy"))))
   (build-system gnu-build-system)
   (arguments
    (list #:tests? #f))
   (inputs (list libjpeg-turbo))
   (synopsis "Optimize JPEG images")
   (description
    "jpegoptim provides lossless optimization (based on optimizing
the Huffman tables) and \"lossy\" optimization based on setting
maximum quality factor.")
   (license license:gpl3+)
   (home-page "https://www.kokkonen.net/tjko/projects.html#jpegoptim")))

(define-public tgif
  (package
    (name "tgif")
    (version "4.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://sourceforge/tgif/tgif/"
               version "/tgif-QPL-" version ".tar.gz"))
        (sha256
          (base32 "1fk7qnqjmrr390bclwqrvlmh77bcl28hdn4vfdqydrpsrbzfj91g"))))
    (build-system gnu-build-system)
    (inputs
      (list libx11
            libxext
            libxt
            libxmu
            zlib))
    (home-page "http://bourbon.usc.edu/tgif/")
    (synopsis "Xlib based interactive 2-D drawing tool")
    (description
      "Tgif (pronounced t-g-i-f) is an Xlib based interactive 2-D drawing tool
(using vector graphics) under X11.")
    (license license:qpl)))

(define-public libicns
  (package
    (name "libicns")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/icns/"
                    "libicns-" version ".tar.gz"))
              (sha256
               (base32
                "1hjm8lwap7bjyyxsyi94fh5817xzqhk4kb5y0b7mb6675xw10prk"))))
    (build-system gnu-build-system)
    (arguments
     (if (and (target-riscv64?)
              (%current-target-system))
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'update-config-scripts
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (for-each (lambda (file)
                                 (install-file
                                   (search-input-file
                                     (or native-inputs inputs)
                                     (string-append "/bin/" file)) "."))
                               '("config.guess" "config.sub"))))) )
       '()))
    (native-inputs
     (if (and (target-riscv64?)
              (%current-target-system))
       (list config)
       '()))
    (inputs
     (list libpng jasper))
    (home-page "https://icns.sourceforge.io/")
    (synopsis "Library for handling Mac OS icns resource files")
    (description
     "Libicns is a library for the manipulation of Mac OS IconFamily resource
type files (ICNS).  @command{icns2png} and @command{png2icns} are provided to
convert between PNG and ICNS. @command{icns2png} will extract image files from
ICNS files under names like \"Foo_48x48x32.png\" useful for installing for use
with .desktop files.  Additionally, @command{icontainer2png} is provided for
extracting icontainer icon files.")
    (license (list license:lgpl2.1+     ; libicns
                   license:lgpl2.0+     ; src/apidocs.*
                   license:gpl2+))))    ; icns2png, png2icns, icontainer2png

(define-public libtiff
  (package
   (name "libtiff")
   (version "4.4.0")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.osgeo.org/libtiff/tiff-"
                           version ".tar.xz"))
       (patches (search-patches "libtiff-CVE-2022-34526.patch"))
       (sha256
        (base32
         "1h8xrcpbyf9id2hw2ms0cmpgx0li8gladjzj82ycgk28018pnc29"))))
   (build-system gnu-build-system)
   (outputs '("out"
              "doc"))                           ;1.8 MiB of HTML documentation
   (arguments
    (list #:configure-flags
          ;; Instead of using --docdir, this package has its own --with-docdir.
          #~(list (string-append "--with-docdir=" #$output:doc "/share/doc/"
                                 #$name "-" #$(package-version this-package))
                "--disable-static")))
   (inputs
    (list libjpeg-turbo xz zlib))
   (synopsis "Library for handling TIFF files")
   (description
    "Libtiff provides support for the Tag Image File Format (TIFF), a format
used for storing image data.
Included are a library, libtiff, for reading and writing TIFF and a small
collection of tools for doing simple manipulations of TIFF images.")
   (license (license:non-copyleft "file://COPYRIGHT"
                                  "See COPYRIGHT in the distribution."))
   (properties
    '((upstream-name . "tiff")))
   (home-page "http://www.simplesystems.org/libtiff/")))

(define-public leptonica
  (package
    (name "leptonica")
    (version "1.84.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DanBloomberg/leptonica")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b4ikf1p2ll4310n4dg5lg0b79wys71fb6nj22i7pz17wjdma0j8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gnuplot ;needed for test suite
           autoconf
           automake
           libtool
           pkg-config))
    (inputs
     (list giflib
           libjpeg-turbo
           libpng
           libtiff
           libwebp
           openjpeg
           zlib))
    (arguments
     (list
      ;; Parallel tests cause some tests to fail randomly.
      ;; Same thing observed on Debian.
      #:parallel-tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-reg-wrapper
            (lambda _
              (substitute* "prog/reg_wrapper.sh"
                ((" /bin/sh ")
                 (string-append " " (which "sh") " "))
                (("which gnuplot")
                 "true"))))
          (add-after 'install 'provide-absolute-giflib-reference
            (lambda _
              (let ((giflib #$(this-package-input "giflib")))
                ;; Add an absolute reference to giflib to avoid propagation.
                (with-directory-excursion (string-append #$output "/lib")
                  (substitute* '("libleptonica.la" "pkgconfig/lept.pc")
                    (("-lgif") (string-append "-L" giflib "/lib -lgif"))))))))))
    (home-page "http://www.leptonica.com/")
    (synopsis "Library and tools for image processing and analysis")
    (description
     "Leptonica is a C library and set of command-line tools for efficient
image processing and image analysis operations.  It supports rasterop, affine
transformations, binary and grayscale morphology, rank order, and convolution,
seedfill and connected components, image transformations combining changes in
scale and pixel depth, and pixelwise masking, blending, enhancement, and
arithmetic ops.")
    (license license:bsd-2)))

(define-public leptonica-1.80
  (package
    (inherit leptonica)
    (name "leptonica")
    (version "1.80.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DanBloomberg/leptonica")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12ddln72z5l3icz0i9rpsfkg5xik8fcwcn8lb0cp3jigjxi8gvkg"))))
    (arguments
     (substitute-keyword-arguments (package-arguments leptonica)
       ((#:tests? _ #t)
        ;; The pngio_reg test fails, probably because the libpng used is
        ;; newer.
        #f)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'provide-absolute-giflib-reference
              (lambda _
                (let ((giflib #$(this-package-input "giflib")))
                  ;; Add an absolute reference to giflib to avoid propagation.
                  ;; This is the same as for the parent package, but at that
                  ;; time the file name was 'liblept.la, not libleptonica.la.
                  (with-directory-excursion (string-append #$output "/lib")
                    (substitute* '("liblept.la" "pkgconfig/lept.pc")
                      (("-lgif")
                       (string-append "-L" giflib "/lib -lgif")))))))))))))

(define-public jbig2dec
  (package
    (name "jbig2dec")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ArtifexSoftware"
                                  "/ghostpdl-downloads/releases/download"
                                  "/gs9533/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dwa24kjqyg9hmm40fh048sdxfpnasz43l2rm8wlkw1qbdlpd517"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--disable-static")))
    (native-inputs
     (list python-minimal-wrapper))     ;for tests
    (synopsis "Decoder of the JBIG2 image compression format")
    (description
     "JBIG2 is designed for lossy or lossless encoding of @code{bilevel} (1-bit
monochrome) images at moderately high resolution, and in particular scanned
paper documents.  In this domain it is very efficient, offering compression
ratios on the order of 100:1.

This is a decoder only implementation, and currently is in the alpha
stage, meaning it doesn't completely work yet.  However, it is
maintaining parity with available encoders, so it is useful for real
work.")
    (home-page "https://jbig2dec.com")
    (license license:gpl2+)))

(define-public jbigkit
  (package
    (name "jbigkit")
    (version "2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.cl.cam.ac.uk/~mgk25/jbigkit/"
                           "download/jbigkit-" version ".tar.gz"))
       (sha256
        (base32 "0cnrcdr1dwp7h7m0a56qw09bv08krb37mpf7cml5sjdgpyv0cwfy"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove files without clear licence information.
           (for-each delete-file-recursively
                     (list "contrib" "examples"))
           #t))))
    (build-system gnu-build-system)
    (outputs (list "out" "pbmtools"))
    (arguments
     `(#:modules ((srfi srfi-26)
                  ,@%default-gnu-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'install              ; no ‘make install’ target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (with-directory-excursion "libjbig"
                 (for-each (cut install-file <> include)
                           (find-files "." "\\.h$"))
                 (for-each (cut install-file <> lib)
                           (find-files "." "\\.a$")))
               #t)))
         (add-after 'install 'install-pbmtools
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "pbmtools"))
                    (bin (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1"))
                    (man5 (string-append out "/share/man/man5")))
               (with-directory-excursion "pbmtools"
                 (for-each (cut install-file <> bin)
                           (list "jbgtopbm" "jbgtopbm85"
                                 "pbmtojbg" "pbmtojbg85"))

                 (for-each (cut install-file <> man1)
                           (find-files "." "\\.1$"))
                 (for-each (cut install-file <> man5)
                           (find-files "." "\\.5$"))
                 #t)))))
       #:test-target "test"
       #:tests? #f))                    ; tests depend on examples/
    (home-page "https://www.cl.cam.ac.uk/~mgk25/jbigkit/")
    (synopsis "Lossless compression for bi-level high-resolution images")
    (description
     "JBIG-KIT implements the JBIG1 data compression standard (ITU-T T.82 and
ISO/IEC 11544:1993), designed for bi-level (one bit per pixel) images such as
black-and-white scanned documents.  It is widely used in fax products, printer
firmware and drivers, document management systems, and imaging software.

This package provides a static C library of (de)compression functions and some
simple command-line converters similar to those provided by netpbm.

Two JBIG1 variants are available.  One (@file{jbig.c}) implements nearly all
options of the standard but has to keep the full uncompressed image in memory.
The other (@file{jbig85.c}) implements just the ITU-T T.85 profile, with
memory management optimized for embedded and fax applications.  It buffers
only a few lines of the uncompressed image in memory and is able to stream
images of initially unknown height.")
    (license (list license:isc          ; pbmtools/p?m.5
                   license:gpl2+))))    ; the rest

(define-public openjpeg-data
  (package
    (name "openjpeg-data")
    (version "2021.09.26")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/uclouvain/openjpeg-data")
         (commit "1f3d093030f9a0b43353ec6b48500f65786ff57a")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13jpdaqk7mngk0xw6xbh9zgipip6n25spvqd97hwpfna1zyh8lzh"))))
    (build-system copy-build-system)
    (synopsis "Test files for OpenJPEG")
    (description "OpenJPEG-Data contains all files required to run the openjpeg
test suite, including conformance tests (following Rec. ITU-T T.803 | ISO/IEC
15444-4 procedures), non-regression tests and unit tests.")
    (home-page "https://github.com/uclouvain/openjpeg-data")
    (license license:bsd-2)))

(define-public openjpeg
  (package
    (name "openjpeg")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/uclouvain/openjpeg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xvcxlzqlylnjsyl1j64d7rgzv68ihm4qy6fpdrix3xzbqb3fjpz"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list
        "-DBUILD_STATIC_LIBS=OFF"
        "-DBUILD_UNIT_TESTS=ON"
        "-DBUILD_TESTING=ON"
        (string-append "-DOPJ_DATA_ROOT="
                       (assoc-ref %build-inputs "openjpeg-data")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             ;; To be re-enabled after upstream fixes the bug,
             ;; https://github.com/uclouvain/openjpeg/issues/1264
             (substitute* "tests/CMakeLists.txt"
               (("add_subdirectory\\(nonregression\\)")
                ""))
             ;; These tests fail on all architectures except x86_64
             (substitute* "tests/conformance/CMakeLists.txt"
               ;; 4, 5, 6 fail
               (("numFileC1P0 RANGE 1 16") "numFileC1P0 RANGE 7 16")
               ;; 2, 3, 4, 5 fail
               (("numFileC1P1 RANGE 1 7") "numFileC1P1 1 6 7")
               ;; 2, 3 fail
               (("numFileJP2 RANGE 1 9") "numFileJP2 RANGE 4 9")
               ;; All fail
               (("subsampling.*") "")
               (("zoo.*") "")))))))
    (native-inputs
     (list openjpeg-data)) ; Files for test-suite
    (inputs
     (list lcms libpng libtiff zlib))
    (synopsis "OPENJPEG Library and Applications")
    (description "OpenJPEG is an implementation of JPEG 2000 codec written in C
language.  It has been developed in order to promote the use of JPEG 2000, a
still-image compression standard from the Joint Photographic Experts Group
(JPEG).  Since April 2015, it is officially recognized by ISO/IEC and ITU-T as a
JPEG 2000 Reference Software.")
    (home-page "https://github.com/uclouvain/openjpeg")
    (license license:bsd-2)))

(define-public giflib
  (package
    (name "giflib")
    (version "5.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/giflib/giflib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gbrg03z1b6rlrvjyc6d41bc8j1bsr7rm8206gb1apscyii5bnii"))))
    (build-system gnu-build-system)
    (outputs '("bin"                    ; utility programs
               "out"))                  ; library
    (arguments
     `(#:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX="
                                         (assoc-ref %outputs "out"))
                          (string-append "BINDIR="
                                         (assoc-ref %outputs "bin") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-html-doc-gen
           (lambda _
             (substitute* "doc/Makefile"
               (("^all: allhtml manpages") ""))
             #t))
         (delete 'configure)
         (add-after 'install 'install-manpages
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((bin (assoc-ref outputs "bin"))
                    (man1dir (string-append bin "/share/man/man1")))
               (mkdir-p man1dir)
               (for-each (lambda (file)
                           (let ((base (basename file)))
                             (format #t "installing `~a' to `~a'~%"
                                     base man1dir)
                             (copy-file file
                                        (string-append
                                         man1dir "/" base))))
                         (find-files "doc" "\\.1"))
               #t))))))
    (synopsis "Tools and library for working with GIF images")
    (description
     "GIFLIB is a library for reading and writing GIF images.  It is API and
ABI compatible with libungif which was in wide use while the LZW compression
algorithm was patented.  Tools are also included to convert, manipulate,
compose, and analyze GIF images.")
    (home-page "http://giflib.sourceforge.net/")
    (license license:x11)))

(define-public libuemf
  (package
    (name "libuemf")
    (version "0.2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libuemf/libUEMF-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05djs99vqf067x81xfpskh7a66y5x7b4mmjavybcy7swnm0swg7v"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; Overriding CMAKE_INSTALL_PREFIX is not a good idea.
         (add-after 'unpack 'fix-CMakeLists.txt
           (lambda _
             (substitute* "CMakeLists.txt"
               ((".*SET\\(CMAKE_INSTALL_PREFIX.*") ""))
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (sources (string-append "../libUEMF-" ,version))
                    (drm-tools (assoc-ref inputs "drm-tools"))
                    (extract (string-append drm-tools "/bin/extract"))
                    (execinput (string-append drm-tools "/bin/execinput")))
               (with-directory-excursion sources
                 (substitute* "testit.sh"
                   (("^EPATH=.*")
                    (format #f "EPATH=~a~%" bin))
                   (("`which diff`")
                    "diff")
                   (("^EXTRACT=.*")
                    (format #f "EXTRACT=~a~%" extract))
                   (("^EXECINPUT=.*")
                    (format #f "EXECINPUT=~a~%" execinput)))
                 (invoke "sh" "testit.sh"))))))))
    (native-inputs (list drm-tools)) ;for tests
    (home-page "https://libuemf.sourceforge.net/")
    (synopsis "Library for working with WFM, EMF and EMF+ images")
    (description "The libUEMF library is a portable C99 implementation for
reading and writing @acronym{WFM, Windows Metafile}, @acronym{EMF, Enhanced
Metafile}, and @acronym{EMF+, Enhanced Metafile Plus} files.")
    (license license:gpl2+)))

(define-public libungif
  (package
    (name "libungif")
    (version "4.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/giflib/libungif-4.x/"
                                  "libungif-" version "/libungif-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "0cnksimmmjngdrys302ik1385sg1sj4i0gxivzldhgwd46n7x2kh"))))
    (build-system gnu-build-system)
    (inputs (list perl))          ;package ships some perl tools
    (home-page "http://giflib.sourceforge.net/")
    (synopsis "GIF decompression library")
    (description
     "libungif is the old GIF decompression library by the GIFLIB project.")
    (license license:expat)))

(define-public imlib2
  (package
    (name "imlib2")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/enlightenment/imlib2-src/" version
                    "/imlib2-" version ".tar.xz"))
              (sha256
               (base32
                "1fnbh6vj0d9l1c2bzw9psxh3wnlbr1nlfzi16w60hp48gj9ilz3j"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "--disable-static")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list bzip2
           freetype
           giflib
           libid3tag
           libjpeg-turbo
           libpng
           libtiff
           libx11
           libxext
           libwebp))
    (home-page "https://sourceforge.net/projects/enlightenment/")
    (synopsis
     "Loading, saving, rendering and manipulating image files")
    (description
     "Imlib2 is a library that does image file loading and saving as well as
rendering, manipulation, arbitrary polygon support, etc.

It does ALL of these operations FAST.  Imlib2 also tries to be highly
intelligent about doing them, so writing naive programs can be done easily,
without sacrificing speed.

This is a complete rewrite over the Imlib 1.x series.  The architecture is
more modular, simple, and flexible.")
    (license license:imlib2)))

(define-public imlib2-1.7
  (package
    (inherit imlib2)
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/enlightenment/imlib2-src/" version
                    "/imlib2-" version ".tar.bz2"))
              (sha256
               (base32
                "01y45cdml2dr9cqgybrgxr86sd77d1qfa1gzclzy1j6bkminlfh3"))))))

(define-public giblib
  (package
    (name "giblib")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (list
                     (string-append
                       "http://linuxbrit.co.uk/downloads/giblib-"
                       version ".tar.gz")
                     (string-append
                       "https://src.fedoraproject.org/repo/pkgs/giblib/giblib-"
                       version ".tar.gz/c810ef5389baf24882a1caca2954385e/giblib-"
                       version ".tar.gz")
                     (string-append
                       "https://sourceforge.net/projects/slackbuildsdirectlinks/"
                       "files/giblib/giblib-" version ".tar.gz")))
              (sha256
               (base32
                "1b4bmbmj52glq0s898lppkpzxlprq9aav49r06j2wx4dv3212rhp"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11
           ;; Needs an old imlib2 with the 'imlib2-config' program.
           imlib2-1.7))
    (home-page
     ;; This vanished page is universally accepted as giblib's home despite not
     ;; mentioning the package once.
     (string-append "https://web.archive.org/web/20140907071208/"
                    "https://linuxbrit.co.uk/software/"))
    (synopsis "Wrapper library for imlib2")
    (description
     "Giblib is a simple library which wraps imlib2's context API, avoiding
all the context_get/set calls, adds fontstyles to the truetype renderer and
supplies a generic doubly-linked list and some string functions.")
    ;; This license removes a clause about X Consortium from the original
    ;; X11 license.
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public freeimage
  (package
    (name "freeimage")
    (version "3.18.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/freeimage/Source%20Distribution/"
                    version "/FreeImage"
                    (string-concatenate (string-split version #\.))
                    ".zip"))
              (sha256
               (base32
                "1z9qwi9mlq69d5jipr3v2jika2g0kszqdzilggm99nls5xl7j4zl"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each
                   (lambda (dir)
                     (delete-file-recursively (string-append "Source/" dir)))
                   '("LibJPEG" "LibOpenJPEG" "LibPNG" "LibRawLite"
                     "LibJXR" "LibWebP" "OpenEXR" "ZLib"))))
              (patches
               (append
                (search-patches "freeimage-unbundle.patch"
                                "freeimage-libtiff-compat.patch"
                                "freeimage-CVE-2020-21428.patch"
                                "freeimage-CVE-2020-22524.patch"
                                "freeimage-libraw-0.21-compat.patch")
                ;; Take one patch from Arch Linux that adds LibRaw 0.20 compatibility.
                (list (origin
                        (method url-fetch)
                        (uri "https://raw.githubusercontent.com/archlinux\
/svntogit-community/ca3e6a52f5a46dec87cbf85e9d84fe370e282c8c/trunk\
/freeimage-libraw-0.20.patch")
                        (file-name "freeimage-libraw-compat.patch")
                        (sha256
                         (base32
                          "0cwjxjz0f4gs6igvwqg0p99mnrsrwzkal1l2n08yvz2xq9s5khki"))))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; According to Fedora these files depend on private headers, but their
          ;; presence is required for building, so we replace them with empty files.
          (add-after 'unpack 'delete-unbuildable-files
            (lambda _
              (for-each (lambda (file)
                          (delete-file file)
                          (close (open file O_CREAT)))
                        '("Source/FreeImage/PluginG3.cpp"
                          "Source/FreeImageToolkit/JPEGTransform.cpp"))))
          ;; These scripts generate the Makefiles.
          (replace 'configure
            (lambda _
              (invoke "sh" "gensrclist.sh")
              (invoke "sh" "genfipsrclist.sh")))
          (add-before 'build 'patch-makefile
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "Makefile.gnu"
                (("/usr") (assoc-ref outputs "out"))
                (("-o root -g root") "")))))
      #:make-flags
      #~(let ((jxrlib (search-input-directory %build-inputs "include/jxrlib")))
          (list (string-append "CC=" #$(cc-for-target))
                ;; We need '-fpermissive' for Source/FreeImage.h.
                ;; libjxr doesn't have a pkg-config file.
                (string-append "CFLAGS+=-O2 -fPIC -fvisibility=hidden "
                               "-fpermissive -I" jxrlib)))
      #:tests? #f))                     ; no check target
    (native-inputs
     (list pkg-config unzip))
    (inputs
     (list libjpeg-turbo
           libjxr
           libpng
           libraw
           libtiff
           libwebp
           openexr-2
           openjpeg
           zlib))
    (synopsis "Library for handling popular graphics image formats")
    (description
     "FreeImage is a library for developers who would like to support popular
graphics image formats like PNG, BMP, JPEG, TIFF and others.")
    (license license:gpl2+)
    (home-page "https://freeimage.sourceforge.io/")))

(define-public vigra
    (let ((commit "9b514fa00a136f5fd81bb57ee9f6293c333ffc1f")
          (revision "0"))
    (package
     (name "vigra")
     (version (git-version "1.11.1" revision commit))
     (source
      (origin
        ;; The last release is 1.11.1, from 2017. It's becoming more and more
        ;; difficult to build this old release, and the upstream developers
        ;; suggest on their home page to build from the Git repo, saying "It is
        ;; generally safe to use the 'master' branch of the development snapshot,
        ;; as we avoid uploading untested or incompatible changes to this branch."
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/ukoethe/vigra")
               (commit commit)))
        (file-name (git-file-name name version))
        (sha256 (base32
                  "1vzlypviala109imwxkp46lqhhxszf79ypfb8wxg6z7g02j7mm73"))))
     (build-system cmake-build-system)
     (inputs
      `(("boost" ,boost)
        ("fftw" ,fftw)
        ("fftwf" ,fftwf)
        ("hdf5" ,hdf5-1.10)
        ("ilmbase" ,ilmbase) ; propagated by openexr, but needed explicitly
                             ; to create a configure-flag
        ("libjpeg" ,libjpeg-turbo)
        ("libpng" ,libpng)
        ("libtiff" ,libtiff)
        ("openexr" ,openexr-2)
        ("python" ,python-wrapper)
        ;("python-numpy" ,python-numpy)
        ("zlib" ,zlib)))
     (native-inputs
      `(("doxygen" ,doxygen)
        ("python-nose" ,python-nose)
        ("sphinx" ,python-sphinx)))
     (arguments
      `(#:test-target "check"
        #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'disable-broken-tests
            (lambda _
              ;; See https://github.com/ukoethe/vigra/issues/432
              (substitute* "test/fourier/CMakeLists.txt"
                (("VIGRA_ADD_TEST.*") ""))
              ;; This test fails with Numpy 1.15:
              ;; <https://github.com/ukoethe/vigra/issues/436>.
              (substitute* "vigranumpy/test/CMakeLists.txt"
                (("test1\\.py") ""))
              #t)))
        #:configure-flags
          (list "-Wno-dev" ; suppress developer mode with lots of warnings
                (string-append "-DVIGRANUMPY_INSTALL_DIR="
                               (assoc-ref %outputs "out")
                               "/lib/python"
                               ,(version-major+minor (package-version python))
                               "/site-packages")
                ;; Vigranumpy isn't compatible with numpy >= 1.20.
                "-DWITH_VIGRANUMPY=0"
                ;; OpenEXR is not enabled by default.
                "-DWITH_OPENEXR=1"
                ;; Fix rounding error on 32-bit machines
                "-DCMAKE_C_FLAGS=-ffloat-store"
                ;; The header files of ilmbase are not found when included
                ;; by the header files of openexr, and an explicit flag
                ;; needs to be set.
                (string-append "-DCMAKE_CXX_FLAGS=-I"
                               (assoc-ref %build-inputs "ilmbase")
                               "/include/OpenEXR"
                               " -ffloat-store"))))
     (synopsis "Computer vision library")
     (description
      "VIGRA stands for Vision with Generic Algorithms.  It is an image
  processing and analysis library that puts its main emphasis on customizable
  algorithms and data structures.  It is particularly strong for
  multi-dimensional image processing.")
     (license license:expat)
     (home-page "https://ukoethe.github.io/vigra/")
     (properties '((max-silent-time . 7200)))))) ;2 hours, to avoid timing out

(define-public vigra-c
  (let* ((commit "49f53191a12fe91d4e2fd177d22af167571c71d8")
         (revision "2"))
    (package
      (name "vigra-c")
      (version (git-version "0.0.0" revision commit))
      (home-page "https://github.com/BSeppke/vigra_c")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url home-page)
                      (commit commit)))
                (sha256
                 (base32
                  "04ch1jhk4zjf1fpsyp8ldzjp8l9bx025zq0vskjx1clb3ncn305x"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f))                  ; No test target.
      (native-inputs
       (list doxygen))
      (inputs
       (list fftw fftwf hdf5 vigra))
      (synopsis "C interface to the VIGRA computer vision library")
      (description
       "This package provides a C interface to the VIGRA C++ computer vision
library.  It is designed primarily to ease the implementation of higher-level
language bindings to VIGRA.")
      (license license:expat))))

(define-public libwebp
  (package
    (name "libwebp")
    (version "1.3.2")
    (source
     (origin
       ;; No tarballs are provided for >0.6.1.
       (method git-fetch)
       (uri (git-reference
             (url "https://chromium.googlesource.com/webm/libwebp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1x37795gpc63g1ma9kqw4q3dikwhrjklixqzjjsj6viqksa19z41"))))
    (build-system gnu-build-system)
    (inputs
     (list freeglut
           giflib
           libjpeg-turbo
           libpng
           libtiff))
    (native-inputs
     (list autoconf automake libtool))
    (arguments
     '(#:configure-flags '("--enable-libwebpmux"
                           "--enable-libwebpdemux"
                           "--enable-libwebpdecoder"
                           "--disable-static")))
    (home-page "https://developers.google.com/speed/webp/")
    (synopsis "Lossless and lossy image compression")
    (description
     "WebP is a new image format that provides lossless and lossy compression
for images.  WebP lossless images are 26% smaller in size compared to
PNGs.  WebP lossy images are 25-34% smaller in size compared to JPEG images at
equivalent SSIM index.  WebP supports lossless transparency (also known as
alpha channel) with just 22% additional bytes.  Transparency is also supported
with lossy compression and typically provides 3x smaller file sizes compared
to PNG when lossy compression is acceptable for the red/green/blue color
channels.")
    (license license:bsd-3)))

(define-public libmng
  (package
    (name "libmng")
    (version "2.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libmng/libmng-devel/"
                                  version "/" name "-" version ".tar.xz"))
              (sha256
               (base32
                "1lvxnpds0vcf0lil6ia2036ghqlbl740c4d2sz0q5g6l93fjyija"))))
    (build-system gnu-build-system)
    (arguments
     (if (and (target-riscv64?)
              (%current-target-system))
         (list #:phases
               #~(modify-phases %standard-phases
                   (add-after 'unpack 'update-config-scripts
                     (lambda* (#:key inputs native-inputs #:allow-other-keys)
                       ;; Replace outdated config.guess and config.sub.
                       (for-each (lambda (file)
                                   (install-file
                                    (search-input-file
                                     (or native-inputs inputs)
                                     (string-append "/bin/" file)) "."))
                                 '("config.guess" "config.sub"))))))
         '()))
    (native-inputs
     (if (and (target-riscv64?)
              (%current-target-system))
         (list config)
         '()))
    (propagated-inputs
     ;; These are all in the 'Libs.private' field of libmng.pc.
     (list lcms libjpeg-turbo zlib))
    (home-page "https://www.libmng.com/")
    (synopsis "Library for handling MNG files")
    (description
     "Libmng is the MNG (Multiple-image Network Graphics) reference library.")
    (license license:bsd-3)))

(define-public exiv2
  (package
    (name "exiv2")
    (version "0.27.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.exiv2.org/builds/exiv2-" version
                           "-Source.tar.gz"))
       (sha256
        (base32 "1qm6bvj28l42km009nc60gffn1qhngc0m2wjlhf90si3mcc8d99m"))))
    (build-system cmake-build-system)
    (arguments
     `(#:test-target "tests"
       #:configure-flags (list "-DEXIV2_BUILD_UNIT_TESTS=ON"
                               ;; darktable needs BMFF to support
                               ;; CR3 files.
                               "-DEXIV2_ENABLE_BMFF=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-gcc-reference
           (lambda _
             ;; _GLIBCXX_ASSERTIONS brings reference to GCC.
             (substitute* "cmake/compilerFlags.cmake"
               (("add_compile_options[(]-Wp,-D_GLIBCXX_ASSERTIONS[)]")
                ""))))
         (add-after 'install 'delete-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$")))))

         ,@(if (or (target-ppc64le?)
                   (target-aarch64?)
                   (target-riscv64?))
               '((add-after 'unpack 'adjust-tests
                   (lambda _
                     ;; Adjust test on ppc64 and aarch64, where no exception
                     ;; is raised and thus the return value is different.  See
                     ;; <https://github.com/Exiv2/exiv2/issues/365> and
                     ;; <https://github.com/Exiv2/exiv2/issues/933>.
                     (substitute* "tests/bugfixes/github/test_CVE_2018_12265.py"
                       (("\\$uncaught_exception \\$addition_overflow_message\n") "")
                       (("retval = \\[1\\]") "retval = [0]")))))
               '()))))
    (propagated-inputs
     (list expat zlib))
    (native-inputs
     (list googletest python))
    (home-page "https://www.exiv2.org/")
    (synopsis "Library and command-line utility to manage image metadata")
    (description
     "Exiv2 is a C++ library and a command line utility to manage image
metadata.  It provides fast and easy read and write access to the Exif, IPTC
and XMP metadata of images in various formats.")

    ;; Files under `xmpsdk' are a copy of Adobe's XMP SDK, licensed under the
    ;; 3-clause BSD license: <http://www.adobe.com/devnet/xmp/sdk/eula.html>.
    ;; The core is GPLv2+:
    ;;   <https://launchpad.net/ubuntu/precise/+source/exiv2/+copyright>.
    (license license:gpl2+)))

(define-public devil
  (package
    (name "devil")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/openil/"
                                  "DevIL-" version ".tar.gz"))
              (sha256
               (base32
                "02dpzvi493r09c9hfjnk54nladl3qw55iqkkg18g12fxwwz9fx80"))))
    (build-system cmake-build-system)
    (arguments
     '(;; XXX: Not supported in the released CMakeLists.txt.
       ;; Enable this for > 1.8.0.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'change-directory
           (lambda _ (chdir "DevIL") #t)))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("lcms" ,lcms)
       ("libjpeg" ,libjpeg-turbo)
       ("libmng" ,libmng)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("openexr" ,openexr-2)
       ("zlib" ,zlib)))
    (synopsis "Library for manipulating many image formats")
    (description "Developer's Image Library (DevIL) is a library to develop
applications with support for many types of images.  DevIL can load, save,
convert, manipulate, filter and display a wide variety of image formats.")
    (home-page "https://openil.sourceforge.net")
    (license license:lgpl2.1+)))

(define-public jasper
  (package
    (name "jasper")
    (version "2.0.33")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mdadams/jasper")
                    (commit (string-append "version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0p3fj89gkhd2ys5ci75cwb6p7rvb2pf52jd8c9d4g76qp846njnx"))))
    (build-system cmake-build-system)
    (inputs
     (list libjpeg-turbo))
    (synopsis "JPEG-2000 library")
    (description "The JasPer Project is an initiative to provide a reference
implementation of the codec specified in the JPEG-2000 Part-1 standard (i.e.,
ISO/IEC 15444-1).")
    (home-page "https://www.ece.uvic.ca/~frodo/jasper/")
    (license (license:x11-style "file://LICENSE"))))

(define-public zimg
  (package
    (name "zimg")
    (version "3.0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/sekrit-twc/zimg")
              (commit (string-append "release-" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "0pwgf1mybpa3fs13p6jryzm32vfldyql9biwaypqdcimlnlmyk20"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool))
    (synopsis "Scaling, colorspace conversion, and dithering library")
    (description "Zimg implements the commonly required image processing basics
of scaling, colorspace conversion, and depth conversion.  A simple API enables
conversion between any supported formats to operate with minimal knowledge from
the programmer.")
    (home-page "https://github.com/sekrit-twc/zimg")
    ;; test/extra/ contains musl-libm,
    ;; which is MIT/expat licensed, but only used for tests
    (license license:wtfpl2)))

(define-public perceptualdiff
  (package
    (name "perceptualdiff")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/myint/perceptualdiff")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yys55f9i9g3wjjg0j2m0p0k21zwnid8520a8lrr30khm4k5gibp"))))
    (build-system cmake-build-system)
    (inputs (list freeimage))
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'fix-tests
                    ;; cmake-build-system uses a build/ directory outside
                    ;; of the source tree, one level higher than expected
                    (lambda _
                      (substitute* "test/run_tests.bash"
                        (("../build") "../../build"))
                      #t)))))
    (home-page "https://github.com/myint/perceptualdiff")
    (synopsis "Perceptual image comparison utility")
    (description "PerceptualDiff visually compares two images to determine
whether they look alike.  It uses a computational model of the human visual
system to detect similarities.  This allows it too see beyond irrelevant
differences in file encoding, image quality, and other small variations.")
    (license license:gpl2+)))

(define-public steghide
  (package
    (name "steghide")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/steghide/steghide/"
                                  version "/steghide-" version ".tar.bz2"))
              (sha256
               (base32
                "18bxlhbdc3zsmxj84i417xjh0q28kv26q449k23n0a72ldwziix2"))
              (patches (list (search-patch "steghide-fixes.patch")))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list "CXXFLAGS=-fpermissive")  ; required for MHashPP.cc
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'set-perl-search-path
                 (lambda _
                   ;; Work around "dotless @INC" build failure.
                   (setenv "PERL5LIB"
                           (string-append (getcwd) "/tests:"
                                          (getenv "PERL5LIB"))))))))
    (native-inputs
     (list gettext-minimal libtool perl))
    (inputs
     (list libjpeg-turbo libmhash libmcrypt zlib))
    (home-page "https://steghide.sourceforge.net")
    (synopsis "`Hide' (nonconfidential) data in image or audio files")
    (description
     "Steghide is a program to `hide' data in various kinds of image and audio
files.  This practice is known as @dfn{steganography}, but the method used by
steghide is not very secure and should not be used where security is at stake.
Even if a password is used, steghide offers little plausible deniability.

Nonetheless, neither color nor sample frequencies are changed, making the
embedding resistant against first-order statistical tests not aimed
specifically at this tool.")
    (license license:gpl2+)))

(define-public optipng
  (package
    (name "optipng")
    (version "0.7.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://prdownloads.sourceforge.net/optipng/optipng-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0lj4clb851fzpaq446wgj0sfy922zs5l5misbpwv6w7qrqrz4cjg"))
       (modules '((guix build utils)))
       (snippet
         '(begin
            (delete-file-recursively "src/libpng")
            (delete-file-recursively "src/zlib")
            #t))))
    (build-system gnu-build-system)
    (inputs
     (list libpng zlib))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             ;; configure script doesn't accept arguments CONFIG_SHELL and SHELL
             (invoke "sh" "configure"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "-with-system-libs")
             #t)))))
    (synopsis "Optimizer that recompresses PNG image files to a smaller size")
    (description "OptiPNG is a PNG optimizer that recompresses image
files to a smaller size, without losing any information.  This program
also converts external formats (BMP, GIF, PNM and TIFF) to optimized
PNG, and performs PNG integrity checks and corrections.")
    (home-page "https://optipng.sourceforge.net/")
    (license license:zlib)))

(define-public imgp
  (package
    (name "imgp")
    (version "2.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "imgp" version))
       (sha256
        (base32 "0avdgr4fx643jg9wzwm65y14s56bnrn3hmkw7v0mcyvxn88vxwiq"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f                      ;there are no tests
       #:phases
       (modify-phases %standard-phases
         ;; setup.py expects the file to be named 'imgp'.
         (add-after 'unpack 'rename-imgp
           (lambda _
             (rename-file "imgp.py" "imgp"))))))
    (inputs
     (list python-pillow))
    (home-page "https://github.com/jarun/imgp")
    (synopsis "High-performance CLI batch image resizer & rotator")
    (description
     "@code{imgp} is a command line image resizer and rotator for JPEG and PNG
images.  It can resize (or thumbnail) and rotate thousands of images in a go
while saving significantly on storage.

This package may optionally be built with @code{python-pillow-simd} in place
of @{python-pillow} for SIMD parallelism.")
    (license license:gpl3+)))

(define-public pngsuite
  (package
    (name "pngsuite")
    (version "2017jul19")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append "http://www.schaik.com/pngsuite2011/PngSuite-"
                           version ".tgz"))
       (sha256
        (base32
         "1j7xgd9iffcnpphhzz9ld9ybrjmx9brhq0803g0450ssr52b5502"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; there is no test target
       #:license-file-regexp "PngSuite.LICENSE"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." (string-append out "/"))
             #t)))
         (delete 'build)
         (delete 'configure))))
    (home-page "http://www.schaik.com/pngsuite2011/pngsuite.html")
    (synopsis "Example PNGs for use in test suites")
    (description "Collection of graphics images created to test PNG
applications like viewers, converters and editors.  As far as that is
possible, all formats supported by the PNG standard are represented.")
    (license (license:fsdg-compatible "file://PngSuite.LICENSE" "Permission to
use, copy, modify and distribute these images for any purpose and without fee
is hereby granted."))))

(define-public libjpeg-turbo
  (package
    (name "libjpeg-turbo")
    (version "2.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/libjpeg-turbo/"
                                  version "/libjpeg-turbo-" version ".tar.gz"))
              (sha256
               (base32
                "0arl61ici38ann5xjidwdzkhmjdp1r95x4x4zinnh4qs2fhjdvfk"))))
    (build-system cmake-build-system)
    (native-inputs
     (list nasm))
    (arguments
     (list #:configure-flags
           #~'("-DCMAKE_INSTALL_LIBDIR:PATH=lib"
               "-DENABLE_STATIC=0"
               ;; djpeg-shared-3x2-float-prog-cmp fails on some systems.
               #$@(if (or (target-ppc32?)
                          (target-riscv64?))
                      '("-DFLOATTEST=NO")
                      '())
               ;; The build system probes for the current CPU, but
               ;; that fails when cross-compiling.
               #$@(let ((target (%current-target-system)))
                    (if target
                        (cond ((string-prefix? "arm" target)
                               '("-DCMAKE_SYSTEM_PROCESSOR=arm"))
                              ((string-prefix? "aarch64" target)
                               '("-DCMAKE_SYSTEM_PROCESSOR=aarch64"))
                              ((string-prefix? "i686" target)
                               '("-DCMAKE_SYSTEM_PROCESSOR=x86"))
                              ((string-prefix? "x86_64" target)
                               '("-DCMAKE_SYSTEM_PROCESSOR=x86_64"))
                              ;; 32-bit and 64-bit
                              ((string-prefix? "powerpc" target)
                               '("-DCMAKE_SYSTEM_PROCESSOR=powerpc"))
                              ((string-prefix? "riscv64" target)
                               '("-DCMAKE_SYSTEM_PROCESSOR=riscv64"))
                              (else '()))
                        '())))))
    (home-page "https://libjpeg-turbo.org/")
    (synopsis "SIMD-accelerated JPEG image handling library")
    (description "libjpeg-turbo is a JPEG image codec that accelerates baseline
JPEG compression and decompression using SIMD instructions: MMX on x86, SSE2 on
x86-64, NEON on ARM, and AltiVec on PowerPC processors.  Even on other systems,
its highly-optimized Huffman coding routines allow it to outperform libjpeg by
a significant amount.
libjpeg-turbo implements both the traditional libjpeg API and the less powerful
but more straightforward TurboJPEG API, and provides a full-featured Java
interface.  It supports color space extensions that allow it to compress from
and decompress to 32-bit and big-endian pixel buffers (RGBX, XBGR, etc.).")
    ;; libjpeg-turbo is covered by three different licenses; see LICENSE.md.
    (license (list license:bsd-3        ;the TurboJPEG API library and programs
                   license:ijg          ;the libjpeg library and associated tools
                   license:zlib))))     ;the libjpeg-turbo SIMD extensions

(define-public niftilib
  (package
    (name "niftilib")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "mirror://sourceforge/niftilib/"
                                        "nifticlib/nifticlib_"
                                        (string-join (string-split version #\.) "_")
                                        "/nifticlib-" version ".tar.gz")))
              (sha256
               (base32 "123z9bwzgin5y8gi5ni8j217k7n683whjsvg0lrpii9flgk8isd3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; there is no test target
       #:parallel-build? #f             ; not supported
       #:make-flags
       (list "SHELL=bash"
             (string-append "ZLIB_INC="
                            (assoc-ref %build-inputs "zlib") "/include")
             ;; Append "-fPIC" to CFLAGS.
             (string-append "CFLAGS="
                            "-Wall -ansi -pedantic -fPIC"))
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (for-each
                (lambda (dir)
                  (copy-recursively dir (string-append out "/" dir)))
                '("bin" "lib" "include")))
             #t))
         (delete 'configure))))
    (inputs
     (list zlib))
    (synopsis "Library for reading and writing files in the nifti-1 format")
    (description "Niftilib is a set of i/o libraries for reading and writing
files in the nifti-1 data format - a binary file format for storing
medical image data, e.g. magnetic resonance image (MRI) and functional MRI
(fMRI) brain images.")
    (home-page "https://niftilib.sourceforge.net")
    (license license:public-domain)))

(define-public mini
  (package
    (name "mini")
    (version "0.9.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/pulzed/mINI")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n4c4dsmycp9pgaykc7rpgrgs23nb5gc9fb4agq95fdfpphdarm4"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (delete 'build)
               (delete 'configure)
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (with-directory-excursion "tests"
                       (for-each (lambda (test)
                                   (let ((test-name (basename test
                                                              ".cpp")))
                                     (invoke "./build.sh" test-name)
                                     (invoke "./run.sh" test-name)))
                                 (find-files "." ".cpp"))))))
               (replace 'install
                 (lambda _
                   (install-file "src/mini/ini.h"
                                 (string-append #$output "/include/mini")))))))
    (home-page "https://github.com/pulzed/mINI")
    (synopsis "INI file reader and writer header library")
    (description
     "This is a tiny, header-only C++ library for manipulating INI files.")
    (license license:expat)))

(define-public picket
  (package
    (name "picket")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rajter/picket")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1zhpynyakjx9nc51b1j80b4y3138p3l380kp1cqmmjx2n9430144"))
              (snippet '(begin
                          ;; bundled mINI header library.
                          (delete-file "src/cfg/ini.h")))))
    (native-inputs (list pkg-config))
    (inputs (list gtkmm-3 mini))
    (arguments
     (list #:tests? #f
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-mini-includes
                          (lambda _
                            (substitute* '("src/cfg/config.h"
                                           "src/cfg/config.cpp")
                              (("#include \"ini.h\"")
                               "#include \"mini/ini.h\""))
                            (substitute* "src/main.cpp"
                              (("/usr")
                               #$output))))
                        (add-after 'unpack 'fix-cmake-paths
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "CMakeLists.txt"
                              (("src/cfg/ini.h")
                               (search-input-file inputs
                                                  "/include/mini/ini.h"))
                              (("/usr/")
                               #$output)))))))
    (build-system cmake-build-system)
    (home-page "https://github.com/rajter/picket")
    (synopsis "Screen color picker with custom format output")
    (description
     "Picket is a screen color picker that includes a magnifier and supports
custom formats for representing color values..")
    (license license:gpl3+)))

(define-public gpick
  (package
    (name "gpick")
    (version "0.2.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/thezbyg/gpick")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0nl89gca5nmbyycv5rl5bm6k7facapdk4pab9pl949aa3cjw9bk7"))))
    (build-system scons-build-system)
    (native-inputs
     `(("boost" ,boost)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("ragel" ,ragel)))
    (inputs
     `(("expat" ,expat)
       ("gtk2" ,gtk+-2)
       ("lua" ,lua-5.2)))
    (arguments
     `(#:tests? #f
       #:scons ,scons-python2
       #:scons-flags (list (string-append "DESTDIR=" %output))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-lua-reference
           (lambda _
             (substitute* "SConscript"
               (("lua5.2") "lua-5.2"))
             #t)))))
    (home-page "http://www.gpick.org/")
    (synopsis "Color picker")
    (description "Gpick is an advanced color picker and palette editing tool.")
    (license license:bsd-3)))

(define-public libiptcdata
  (package
    (name "libiptcdata")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ianw/libiptcdata"
                                  "/releases/download/release_"
                                  (string-join (string-split version #\.) "_")
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "17m2bscc76r1bymjgb44fbbfrdsjfqyb2ivg9wchyllm8pgx1560"))))
    (build-system gnu-build-system)
    (home-page "https://github.com/ianw/libiptcdata")
    (synopsis "IPTC metadata manipulation library")
    (description
     "Libiptcdata is a C library for manipulating the International Press
Telecommunications Council (@dfn{IPTC}) metadata stored within multimedia files
such as images.  This metadata can include captions and keywords, often used by
popular photo management applications.  The library provides routines for
parsing, viewing, modifying, and saving this metadata.")
    (license license:lgpl2.0+)))

(define-public flameshot
  (package
    (name "flameshot")
    (version "12.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flameshot-org/flameshot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1p7gqs5vqzbddlgl38lbanchwb14m6lx8f2cn2c5p0vyqwvqqv52"))))
    (build-system qt-build-system)
    (native-inputs
     (list qttools-5))
    (inputs
     (list qtbase-5 qtsvg-5))
    (arguments
     `(#:tests? #f))                    ;no tests
    (home-page "https://github.com/flameshot-org/flameshot")
    (synopsis "Powerful yet simple to use screenshot software")
    (description "Flameshot is a screenshot program.
Features:

@itemize
@item Customizable appearance.
@item Easy to use.
@item In-app screenshot edition.
@item DBus interface.
@item Upload to Imgur.
@end itemize\n")
    (license license:gpl3+)))

(define-public swappy
  (package
    (name "swappy")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jtheoof/swappy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s2lp3bz30svqdg6467jvncim0qgl0q1b1nqxnnci6kljbp5g0xh"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config scdoc
           `(,glib "bin"))) ; for 'glib-compile-resources'
    (inputs
     (list gtk+ libnotify gettext-minimal))
    (propagated-inputs
     ;; Needed to properly render the icons.
     (list font-awesome))
    (home-page "https://github.com/jtheoof/swappy")
    (synopsis "Grab and edit on the fly snapshots of a Wayland compositor")
    (description
     "@command{swappy} is a command-line utility to take and edit screenshots
of Wayland desktops.  Works great with grim, slurp and sway.  But can easily
work with other screen copy tools that can output a final PNG image to
stdout.")
    (license license:expat)))

(define-public gifsicle
  (package
    (name "gifsicle")
    (version "1.95")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kohler/gifsicle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wvsf2kv90bqpyxcjilir4zgmaga0xjg96vnn7rzq4fkjx8pb3yg"))
       (modules '((guix build utils)))
       (snippet '(begin (substitute* "configure.ac"
                          (("2.72") "2.69"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'patch-tests
            (lambda _
              (substitute* "test/testie"
                (("/usr/bin/perl")
                 (which "perl"))
                (("/bin/sh")
                 (which "sh"))
                (("/bin/rm")
                 (which "rm"))))))))
    (native-inputs
     (list
      autoconf automake
      perl))    ; only for tests
    (inputs (list libx11))
    (home-page "https://www.lcdf.org/gifsicle/")
    (synopsis "Edit GIF images and animations")
    (description "Gifsicle is a command-line GIF image manipulation tool that:

@itemize
@item Provides a batch mode for changing GIFs in place.
@item Prints detailed information about GIFs, including comments.
@item Control over interlacing, comments, looping, transparency, etc.
@item Creates well-behaved GIFs: removes redundant colors, only uses local color
tables, etc.
@item Shrinks colormaps and change images to use the Web-safe palette.
@item Optimizes GIF animations, or unoptimizes them for easier editing.
@end itemize

Two other programs are included with Gifsicle: @command{gifview} is a
lightweight animated-GIF viewer, and @command{gifdiff} compares two GIFs for
identical visual appearance.")
    (license license:gpl2+)))

(define-public jp2a
  (package
    (name "jp2a")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/Talinx/jp2a/releases/download/v"
                           version "/jp2a-" version ".tar.gz"))
        (sha256
         (base32
          "10kwhh1a0ivrzagl2vcxrbqmlr2q8x29ymqwzchpiriy6xqxck8l"))))
    (build-system gnu-build-system)
    (inputs
     (list curl libpng libjpeg-turbo ncurses))
    (native-inputs
     (list doxygen))
    (home-page "https://csl.name/jp2a/")
    (synopsis "Convert JPEG images to ASCII")
    (description
     "Jp2a is a small utility that converts JPEG images to ASCII.")
    (license license:gpl2)))

(define-public grim
  (package
   (name "grim")
   (version "1.4.1")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://git.sr.ht/~emersion/grim")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1snp4qlj05d0nx4f0qr8kywv0i1xcw5i278ybng1rand2alhkjz5"))))
   (build-system meson-build-system)
   (native-inputs (append (if (%current-target-system)
                              ;; For wayland-scanner.
                              (list pkg-config-for-build wayland)
                              '())
                          (list pkg-config scdoc)))
   (inputs (list pixman libpng libjpeg-turbo wayland wayland-protocols))
   (home-page "https://wayland.emersion.fr/grim/")
   (synopsis "Create screenshots from a Wayland compositor")
   (description "grim can create screenshots from a Wayland compositor.")
   (license license:expat)))

(define-public slurp
  (package
   (name "slurp")
   (version "1.5.0")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/emersion/slurp")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "0wlml42c3shma50bsvqzll7p3zn251jaf0jm59q2idks8gg1zkyq"))))
   (build-system meson-build-system)
   (native-inputs
    (append (if (%current-target-system)
                ;; for wayland-scanner
                (list wayland pkg-config-for-build)
                '())
            (list pkg-config scdoc)))
   (inputs
    (list cairo libxkbcommon wayland wayland-protocols))
   (home-page "https://github.com/emersion/slurp")
   (synopsis "Select a region in a Wayland compositor")
   (description "Slurp can select a region in a Wayland compositor and print it
to the standard output.  It works well together with grim.")
   (license license:expat)))

(define-public sng
  (package
    (name "sng")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/sng/sng-"
                           version ".tar.gz"))
       (sha256
        (base32 "06a6ydvx9xb3vxvrzdrg3hq0rjwwj9ibr7fyyxjxq6qx1j3mb70i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'link-pngsuite
           ;; tests expect pngsuite in source dir
           (lambda* (#:key inputs #:allow-other-keys)
             (symlink (assoc-ref inputs "pngsuite") "pngsuite")
             #t)))
       #:configure-flags
       (list (string-append "--with-rgbtxt="
                            (assoc-ref %build-inputs "xorg-rgb")
                            "/share/X11/rgb.txt"))))
    (inputs (list xorg-rgb libpng))
    (native-inputs (list pngsuite))
    (home-page "https://sng.sourceforge.net")
    (synopsis "Markup language for representing PNG contents")
    (description "SNG (Scriptable Network Graphics) is a minilanguage designed
specifically to represent the entire contents of a PNG (Portable Network
Graphics) file in an editable form.  Thus, SNGs representing elaborate
graphics images and ancillary chunk data can be readily generated or modified
using only text tools.

SNG is implemented by a compiler/decompiler called sng that
losslessly translates between SNG and PNG.")
    (license license:zlib)))

(define-public blurhash
  (package
    (name "blurhash")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nheko-Reborn/blurhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hx15fspava43z47kv17ivxv56g03fb2zf45dl07v3shickqxw0x"))))
    (build-system meson-build-system)
    (native-inputs
     (list doctest pkg-config))
    (home-page "https://github.com/Nheko-Reborn/blurhash")
    (synopsis "C++ blurhash encoder/decoder")
    (description "Simple encoder and decoder for blurhashes.  Contains a
command line program as well as a shared library.")
    (license license:boost1.0)))

(define-public lodepng
  ;; There are no tags in the repository, so we take the version as defined in
  ;; lodepng.cpp.
  (let ((commit "48e5364ef48ec2408f44c727657ac1b6703185f8")
        (revision "1")
        (version "20200215"))
    (package
      (name "lodepng")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/lvandeve/lodepng")
                      (commit commit)))
                (sha256
                 (base32
                  "1a1x8ag2scanzb2066jm9hg2y9kaa3wmpgmz10l1x9bkpik612lw"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases
         (modify-phases %standard-phases
           (delete 'configure)
           (replace 'build
             (lambda _
               (setenv "CXXFLAGS" "-fPIC")
               (invoke "make" "lodepng.o")
               (invoke "make" "lodepng_util.o")
               (invoke "g++" "-fPIC" "-O3"
                       "-o" "liblodepng.so"
                       "-shared" "lodepng.o" "lodepng_util.o")
               #t))
           (replace 'check
             (lambda _
               (invoke "make" "unittest")
               (invoke "./unittest")
               #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (doc (string-append out "/share/doc"))
                      (lib (string-append out "/lib"))
                      (include (string-append out "/include")))
                 (install-file "lodepng.h" include)
                 (install-file "lodepng_util.h" include)
                 (install-file "liblodepng.so" lib)
                 (install-file "README.md" doc)
                 #t))))))
      (home-page "https://lodev.org/lodepng/")
      (synopsis "PNG encoder and decoder in C and C++, without dependencies")
      (description "LodePNG is a PNG image decoder and encoder, all in one,
no dependency or linkage required.  It's made for C (ISO C90), and has a C++
wrapper with a more convenient interface on top.")
      (license license:zlib))))

(define-public icoutils
  (package
    (name "icoutils")
    (version "0.32.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://savannah/icoutils/icoutils-" version ".tar.bz2"))
       (sha256
        (base32 "1q66cksms4l62y0wizb8vfavhmf7kyfgcfkynil3n99s0hny1aqp"))))
    (build-system gnu-build-system)
    (inputs
     (list libpng perl))
    (propagated-inputs
     (list perl-libwww))
    (home-page "https://www.nongnu.org/icoutils/")
    (synopsis "Extract and convert bitmaps from Windows icon and cursor files")
    (description "Icoutils are a set of program for extracting and converting
bitmaps from Microsoft Windows icon and cursor files.  These files usually
have the extension @code{.ico} or @code{.cur}, but they can also be embedded
in executables and libraries (@code{.dll}-files).  (Such embedded files are
referred to as resources.)

Conversion of these files to and from PNG images is done @command{icotool}.
@command{extresso} automates these tasks with the help of special resource
scripts.  Resources such can be extracted from MS Windows executable and
library files with @command{wrestool}.

This package can be used to create @code{favicon.ico} files for web sites.")
     (license license:gpl3+)))

(define-public libavif
  (package
    (name "libavif")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/AOMediaCodec/libavif")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k72q7yvfdn92wkslyifw14319nm981a8r3kd84i4ylxmrkgi0zm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  (srfi srfi-26))
      #:configure-flags
      #~(list "-DAVIF_CODEC_AOM=ON" "-DAVIF_CODEC_DAV1D=ON"
              #$@(if (this-package-input "rav1e")
                   '("-DAVIF_CODEC_RAV1E=ON")
                   '())
              "-DAVIF_BUILD_TESTS=ON" "-DAVIF_ENABLE_GTEST=ON"
              "-DAVIF_BUILD_APPS=ON" "-DAVIF_BUILD_GDK_PIXBUF=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-thumbnailer
            (lambda _
              (substitute* "contrib/gdk-pixbuf/avif.thumbnailer.in"
                (("@CMAKE_INSTALL_FULL_BINDIR@/gdk-pixbuf-thumbnailer")
                 (string-append #$gdk-pixbuf "/bin/gdk-pixbuf-thumbnailer")))))
          (add-after 'install 'install-readme
            (lambda _
              (let ((doc (string-append #$output "/share/doc/libavif-"
                                        #$(package-version this-package))))
                (install-file "../source/README.md" doc))))
          (add-after 'install 'split
            (lambda _
              (let* ((avifenc  (string-append #$output       "/bin/avifenc"))
                     (avifenc* (string-append #$output:tools "/bin/avifenc"))
                     (avifdec  (string-append #$output       "/bin/avifdec"))
                     (avifdec* (string-append #$output:tools "/bin/avifdec"))

                     (thumbnailer    (string-append
                                      #$output
                                      "/share/thumbnailers/avif.thumbnailer"))
                     (thumbnailer*   (string-append
                                      #$output:pixbuf-loader
                                      "/share/thumbnailers/avif.thumbnailer"))
                     (pixbuf-loader  (string-append
                                      #$output
                                      "/lib/gdk-pixbuf-2.0/2.10.0/loaders/"
                                      "libpixbufloader-avif.so"))
                     (pixbuf-loader* (string-append
                                      #$output:pixbuf-loader
                                      "/lib/gdk-pixbuf-2.0/2.10.0/loaders/"
                                      "libpixbufloader-avif.so")))
                (mkdir-p (string-append #$output:tools "/bin"))
                (for-each (compose mkdir-p
                                   (cut string-append
                                        #$output:pixbuf-loader <>))
                          '("/share/thumbnailers"
                            "/lib/gdk-pixbuf-2.0/2.10.0/loaders/"))

                (for-each (lambda (old new)
                            (copy-file old new)
                            (delete-file old)
                            (chmod new #o555))
                          (list avifenc avifdec
                                thumbnailer pixbuf-loader)
                          (list avifenc* avifdec*
                                thumbnailer* pixbuf-loader*))))))))
    (native-inputs (list googletest pkg-config))
    (inputs
     (append
      (if (member (%current-system) (package-transitive-supported-systems rav1e))
        (list rav1e) '())
      (list dav1d libaom zlib libpng libjpeg-turbo gdk-pixbuf)))
    (outputs (list "out"
                   "tools"  ; avifenc & avifdec
                   "pixbuf-loader"))
    (synopsis "Encode and decode AVIF files")
    (description "Libavif is a C implementation of @acronym{AVIF, the AV1 Image
File Format}.  It can encode and decode all YUV formats and bit depths supported
by AOM, including with alpha.")
    (home-page "https://github.com/AOMediaCodec/libavif")
    (license (list license:bsd-2))))

(define-public libheif
  (package
    (name "libheif")
    (version "1.17.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/strukturag/libheif")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00rc8ffc2s9dz9szhy0f0raas8wnn5cyni1imd5lqz79by6qz7x6"))))
    (build-system cmake-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
      (list gdk-pixbuf ; optional
            libjpeg-turbo
            libpng))
     ;; Propagated to satisfy 'libheif.pc'.
     (propagated-inputs
      (list dav1d libaom libde265 x265))
    (home-page "https://github.com/strukturag/libheif")
    (synopsis "HEIF and AVIF file format decoder and encoder")
    (description
     "@code{libheif} is an ISO/IEC 23008-12:2017 HEIF and AVIF (AV1 Image File
Format) file format decoder and encoder.")
    (license license:lgpl3+)))

(define-public libjxl
  (package
    (name "libjxl")
    (version "0.11.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libjxl/libjxl")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wfxzrhj8a19z6x47ib1qbmgyg56jsxjs955xcvqhdkrx8l2271r"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete the bundles that will not be used.
        '(begin
           (for-each (lambda (directory)
                       (delete-file-recursively
                        (string-append "third_party/" directory)))
                     '("brotli" "googletest" "highway" "lcms" "libjpeg-turbo"
                       "libpng" "zlib"))))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DJPEGXL_FORCE_SYSTEM_GTEST=true"
             "-DJPEGXL_FORCE_SYSTEM_BROTLI=true"
             "-DJPEGXL_FORCE_SYSTEM_LCMS2=true"
             "-DJPEGXL_FORCE_SYSTEM_HWY=true"
             "-DJPEGXL_BUNDLE_LIBPNG=false")
       ,@(cond
           ((target-riscv64?)
            '(#:phases
              (modify-phases %standard-phases
                (add-after 'unpack 'fix-atomic
                  (lambda _
                    (substitute* "lib/jxl/enc_xyb.cc"
                      (("#include \"lib/jxl/enc_xyb.h\"" a)
                       (string-append a "\n#include <atomic>"))))))))
           ((target-x86-32?)
            '(#:phases
              (modify-phases %standard-phases
                (add-after 'unpack 'loosen-test-parameter
                  (lambda _
                    ;; This test fails likely due to a floating point
                    ;; rounding difference.
                    (substitute* "lib/jxl/color_management_test.cc"
                      (("8\\.7e-4") "8.7e-3")))))))
           (#t '()))))
    (native-inputs
     (list asciidoc doxygen googletest pkg-config python))
    (inputs
     (list freeglut
           gflags
           giflib
           imath
           lcms
           libavif
           libjpeg-turbo
           libpng
           libwebp
           openexr
           zlib))
    ;; These are in Requires.private of libjxl.pc.
    (propagated-inputs
     (list brotli google-highway))
    (home-page "https://github.com/libjxl/libjxl")
    (synopsis "JPEG XL image format reference implementation")
    (description "This package contains a reference implementation of JPEG XL
(encoder and decoder).")
    (license license:bsd-3)))

(define-public libjxl-0.10
  (package
    (inherit libjxl)
    (name "libjxl")
    (version "0.10.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libjxl/libjxl")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0la5xkb3zsz8df1x2phld275w2j847hwpy4vlb249g2cqaqnvg9f"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete the bundles that will not be used.
        '(begin
           (for-each (lambda (directory)
                       (delete-file-recursively
                        (string-append "third_party/" directory)))
                     '("brotli" "googletest" "highway" "lcms" "libpng"
                       "zlib"))))))
    (arguments
     `(;; Otherwise gcc segfaults after using up all memory available.
       #:parallel-build? #f
       #:configure-flags
       (list "-DJPEGXL_FORCE_SYSTEM_GTEST=true"
             "-DJPEGXL_FORCE_SYSTEM_BROTLI=true"
             "-DJPEGXL_FORCE_SYSTEM_LCMS2=true"
             "-DJPEGXL_FORCE_SYSTEM_HWY=true"
             "-DJPEGXL_BUNDLE_LIBPNG=false")
       ,@(if (target-riscv64?)
             '(#:phases
               (modify-phases %standard-phases
                 (add-after 'unpack 'fix-atomic
                   (lambda _
                     (substitute* "lib/jxl/enc_xyb.cc"
                       (("#include \"lib/jxl/enc_xyb.h\"" a)
                        (string-append a "\n#include <atomic>")))))))
             '())))
    (native-inputs
     (list asciidoc doxygen googletest pkg-config python))))

(define-public mtpaint
  (package
    (name "mtpaint")
    ;; The author neither releases tarballs nor uses git version tags.
    ;; Instead, author puts version in git commit title.
    (version "3.49.33")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wjaguar/mtPaint")
             (commit "5272e2b1e773c8e02ac3506b2d3bde82ad946b21")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bmq4m0dxczl18n1yiqb75g05a4c3pal1vdcyypkilx7ijsr0cmc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("imlib2" ,imlib2)
       ("libtiff" ,libtiff)
       ("libpng" ,libpng)
       ("libungif" ,libungif)
       ("libjpeg" ,libjpeg-turbo)
       ("libwebp" ,libwebp)
       ("openjpeg" ,openjpeg)
       ("lcms" ,lcms)
       ("zlib" ,zlib)
       ("glib" ,glib)
       ;; Support for gtk3 is in the testing stage.
       ("gtk+" ,gtk+-2)))
    (arguments
     `(#:configure-flags
       (list "intl"                     ; build internationalized version
             "man")                     ; build the man page
       #:tests? #f))                    ; no test suite
    (home-page "https://mtpaint.sourceforge.net/")
    (synopsis "Create pixel art and manipulate digital images")
    (description
     "Mtpaint is a graphic editing program which uses the GTK+ toolkit.
It can create and edit indexed palette or 24bit RGB images, offers basic
painting and palette manipulation tools.  It also handles JPEG, JPEG2000,
GIF, TIFF, WEBP, BMP, PNG, XPM formats.")
    (license license:gpl3+)))

(define-public mypaint
  (package
    (name "mypaint")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mypaint/mypaint/"
                                  "releases/download/v" version "/mypaint-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "05mvay73vb9d2sh1ckv4vny45n059dmsps1jcppjizfmrpbkgr7k"))))
    (build-system python-build-system)
    (arguments
     `(#:imported-modules ((guix build glib-or-gtk-build-system)
                           ,@%python-build-system-modules)
       #:modules ((guix build python-build-system)
                  ((guix build glib-or-gtk-build-system) #:prefix glib-or-gtk:)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'glib-or-gtk-wrap
           (assoc-ref glib-or-gtk:%standard-phases 'glib-or-gtk-wrap))
         (add-after 'install 'wrap-program
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (gdk-pixbuf (assoc-ref inputs "gdk-pixbuf"))
                    (gtk+ (assoc-ref inputs "gtk+")))
               (wrap-program (string-append out "/bin/mypaint")
                 `("GI_TYPELIB_PATH" ":" prefix
                   (,(getenv "GI_TYPELIB_PATH")))))))
         (add-before 'check 'pre-check
           (lambda _
             ;; Tests need writing access
             (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list pkg-config
           gobject-introspection
           swig
           gettext-minimal))
    (inputs
     (list bash-minimal
           gtk+
           (librsvg-for-system)
           hicolor-icon-theme
           libmypaint
           mypaint-brushes
           json-c
           lcms
           python-numpy
           python-pycairo
           python-pygobject))
    (home-page "http://mypaint.org/")
    (synopsis "Fast and simple painting app for artists")
    (description
     "MyPaint is a simple drawing and painting program that works well with
Wacom-style graphics tablets.")
    (license license:gpl2+)))

(define-public phockup
  (package
    (name "phockup")
    (version "1.9.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ivandokov/phockup")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j4mnsy12bhsmd80vgqknv004xbqd165y8gpalw87gp8i8xv172r"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("src" "share/phockup/")
                        ("phockup.py" "share/phockup/"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (list "src/dependency.py"
                                "src/exif.py")
               (("'exiftool")
                (string-append "'" (search-input-file inputs "bin/exiftool"))))))
         (add-before 'install 'check
           (lambda _
             ;; Test without PATH to make sure ‘exiftool’ is properly found.
             (let ((path (getenv "PATH"))
                   (pytest (which "pytest")))
               (setenv "PATH" "")
               (invoke pytest)
               (setenv "PATH" path))))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir (string-append out "/bin"))
               (symlink (string-append out "/share/phockup/phockup.py")
                        (string-append out "/bin/phockup")))))
         (add-after 'install-bin 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/phockup")
                 `("GUIX_PYTHONPATH" prefix
                   ,(search-path-as-string->list
                     (getenv "GUIX_PYTHONPATH"))))))))))
    (inputs
     (list bash-minimal perl-image-exiftool python python-tqdm))
    (native-inputs
     (list python-pytest python-pytest-mock))
    (home-page "https://github.com/ivandokov/phockup")
    (synopsis "Organize photos and videos in folders")
    (description "Phockup is a media sorting tool that uses creation date and
time information in photos and videos to organize them into folders by year,
month and day.  All files which are not images or videos or those which do not
have creation date information will be placed in a folder called
@file{unknown}.")
    (license license:expat)))

(define-public spng
  (package
   (name "spng")
   (version "0.7.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/randy408/libspng")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0src9ii9w9afz2vgridn9r38pa6888myk28x2bjw0ynw5xcd62hs"))))
   (build-system meson-build-system)
   (inputs (list zlib))
   (native-inputs (list libpng))
   (home-page "https://libspng.org")
   (synopsis "Simple PNG loading library")
   (description
    "@code{libspng} is a simple C library for loading Portable Network
Graphics (PNGs), intended as an easy-to-use replacement for @code{libpng}.")
   (license license:bsd-2)
   ;; Supports SSE on x86-64 and NEON on AArch64.
   (properties '((tunable? . #t)))))

(define-public libsixel
  (package
    (name "libsixel")
    (version "1.10.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libsixel/libsixel")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j85p8s5cy7vz1ph9vsvcmxdrr8yi57pgv8vz1xp6dr715jc3g51"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:build-type "release"
      #:configure-flags #~(list "--buildtype=plain"
                                "-Dtests=enabled"
                                "-Dlibcurl=disabled"
                                "-Dgdk-pixbuf2=enabled"
                                "-Dbashcompletiondir=etc/bash_completion.d")))
    (native-inputs (list pkg-config))
    (inputs (list libjpeg-turbo libpng python))
    ;; pkg-config's "Requires.private" need gdk-pixbuf. TODO: Remove it when
    ;; we use pkgconf.
    (propagated-inputs (list gdk-pixbuf))
    (home-page "https://github.com/libsixel/libsixel")
    (synopsis
     "Encoder and decoder implementation for DEC SIXEL graphics")
    (description
     "LibSIXEL is a an encoder/decoder implementation for DEC SIXEL graphics,
and some converter programs.  SIXEL is one of image formats for printer and
terminal imaging introduced by @acronym{DEC, Digital Equipment Corp.}.  Its
data scheme is represented as a terminal-friendly escape sequence.  So if you
want to view a SIXEL image file, all you have to do is @command{cat} it to
your terminal.")
    (license license:expat)))
