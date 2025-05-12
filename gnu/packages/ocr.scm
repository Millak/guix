;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2021, 2024 Andy Tai <atai@atai.org>
;;; Copyright © 2021, 2022, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages ocr)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages djvu)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages image))

(define-public ocrad
  (package
    (name "ocrad")
    (version "0.29")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/ocrad/ocrad-"
                                 version ".tar.lz"))
             (sha256
              (base32
               "1sfrs9jg102malg09cnjdji7lkv9xxccpp3j9a41dfmpn330q80i"))))
    (build-system gnu-build-system)
    (native-inputs (list libpng lzip))
    (home-page "https://www.gnu.org/software/ocrad/")
    (synopsis "Optical character recognition based on feature extraction")
    (description
     "GNU Ocrad is an optical character recognition program based on a
feature extraction method.  It can read images in PBM, PGM or PPM formats and
it produces text in 8-bit or UTF-8 formats.")
    (license license:gpl3+)))

(define-public tesseract-ocr-tessdata-fast
  (package
    (name "tesseract-ocr-tessdata-fast")
    (version "4.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/tesseract-ocr/tessdata_fast")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m310cpb87xx8l8q7jy9fvzf6a0m8rm0dmjpbiwhc2mi6w4gn084"))))
    (build-system copy-build-system)
    (arguments (list #:install-plan #~'(("." "share/tessdata"))
                     #:phases #~(modify-phases %standard-phases
                                  (add-after 'unpack 'delete-broken-links
                                    (lambda _
                                      (delete-file "configs")
                                      (delete-file "pdf.ttf"))))))
    (home-page "https://github.com/tesseract-ocr/tessdata_fast")
    (synopsis "Fast integer versions of trained LSTM models")
    (description "This repository contains fast integer versions of trained
models for the Tesseract OCR Engine.")
    (license license:asl2.0)))

(define-public tesseract-ocr
  (package
    (name "tesseract-ocr")
    (version "5.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tesseract-ocr/tesseract")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0vylcba8w5ljnr6pf3ilc2wjq72k660h71b7mlbjz3a2f0x5kr33"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "LIBLEPT_HEADERSDIR="
                             #$(this-package-input "leptonica") "/include")
              "--disable-static")       ;avoid 6 MiB static archive
      ;; The unit tests are disabled because they require building bundled
      ;; third party libraries.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-override-xml-catalog-files
            (lambda _
              (substitute* "configure.ac"
                (("AC_SUBST\\(\\[XML_CATALOG_FILES])")
                 ""))))
          (add-after 'build 'build-training
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (define n (if parallel-build? (number->string
                                             (parallel-job-count))
                            "1"))
              (invoke "make" "-j" n "training")))
          (add-after 'install 'install-training
            (lambda _
              (invoke "make" "training-install")))
          (add-after 'install 'install-minimal-tessdata
            ;; tesseract-ocr cannot be used without its trained models data;
            ;; install the English language as a minimal base which can be
            ;; extended via TESSDATA_PREFIX.
            (lambda* (#:key native-inputs inputs #:allow-other-keys)
              (define eng.traineddata
                "/share/tessdata/eng.traineddata")
              (install-file (search-input-file (or native-inputs inputs)
                                               eng.traineddata)
                            (dirname (string-append #$output
                                                    eng.traineddata))))))))
    (native-inputs
     (list asciidoc
           autoconf
           automake
           curl
           docbook-xsl
           libarchive
           libtiff
           libtool
           libxslt
           pkg-config
           tesseract-ocr-tessdata-fast))
    (inputs
     (list cairo
           icu4c
           pango
           python-wrapper))
    (propagated-inputs
     ;; Required by tesseract.pc.
     (list leptonica))
    (native-search-paths (list (search-path-specification
                                (variable "TESSDATA_PREFIX")
                                (files (list "share/tessdata"))
                                (separator #f)))) ;single value
    (home-page "https://github.com/tesseract-ocr/tesseract")
    (synopsis "Optical character recognition engine")
    (description
     "Tesseract is an optical character recognition (OCR) engine with very
high accuracy.  It supports many languages, output text formatting, hOCR
positional information and page layout analysis.  Several image formats are
supported through the Leptonica library.  It can also detect whether text is
monospaced or proportional.  Support for the English language is included by
default.  To add support for more languages, the
@code{tesseract-ocr-tessdata-fast} package should be installed.")
    (license license:asl2.0)))

(define-public tesseract-ocr-4
  (package
    (inherit tesseract-ocr)
    (name "tesseract-ocr")
    (version "4.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tesseract-ocr/tesseract")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ksds8n26kq0acprjn4ycdr163hr9kmkwij7fc973mrjg7kz0pdi"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs tesseract-ocr)
       (replace "leptonica" leptonica-1.80)))))

(define-public gimagereader
  (package
    (name "gimagereader")
    (version "3.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/manisandro/gImageReader/releases"
             "/download/v" version "/"
             "gimagereader-" version ".tar.xz"))
       (sha256
        (base32 "0r52j5q2d0mvzw6qkds6vrapcqhdgyj2jvp7bh8w9zw6gpjsyk0q"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test
      #:configure-flags #~(list "-DENABLE_VERSIONCHECK=0")))
    (native-inputs
     (list gettext-minimal intltool pkg-config))
    (inputs
     (list enchant
           djvulibre
           leptonica
           podofo
           poppler-qt5
           sane-backends
           qtbase-5
           qtspell
           quazip-5
           tesseract-ocr))
    (home-page "https://github.com/manisandro/gImageReader")
    (synopsis "Qt front-end to tesseract-ocr")
    (description
     "gImageReader is a Qt front-end to Tesseract optical character
recognition (OCR) software.

gImageReader supports automatic page layout detection but the user can
also manually define and adjust the recognition regions.  It is
possible to import images from disk, scanning devices, clipboard and
screenshots.  gImageReader also supports multipage PDF documents.
Recognized text is displayed directly next to the image and basic text
editing including search/replace and removing of line breaks is
possible.  Spellchecking for the output text is also supported if the
corresponding dictionaries are installed.")
    (license license:gpl3+)))

(define-public zinnia
  (let* ((commit "581faa8f6f15e4a7b21964be3a5ec36265c80e5b")
         (revision "1")
         ;; version copied from 'configure.in'
         (version (git-version "0.07" revision commit)))
    (package
      (name "zinnia")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/taku910/zinnia")
               (commit commit)))
         (sha256
          (base32
           "1izjy5qw6swg0rs2ym2i72zndb90mwrfbd1iv8xbpwckbm4899lg"))
         (file-name (git-file-name name version))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-26)))
         (snippet ; remove unnecessary files with potentially different license
          '(begin
             (for-each delete-file-recursively
                       (scandir "."
                                (negate (cut member <> '("zinnia"
                                                         "." "..")))))
             #t))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (chdir "zinnia")
               (for-each make-file-writable
                         '("config.log" "config.status"))
               #t)))))
      (home-page "https://taku910.github.io/zinnia/")
      (synopsis "Online hand recognition system with machine learning")
      (description
       "Zinnia is a simple, customizable and portable online hand recognition
system based on Support Vector Machines.  Zinnia simply receives user pen
strokes as a sequence of coordinate data and outputs n-best characters sorted
by SVM confidence.  To keep portability, Zinnia doesn't have any rendering
functionality.  In addition to recognition, Zinnia provides training module
that allows us to create any hand-written recognition systems with low-cost.")
      (license (list license:bsd-3 ; all files except...
                     (license:non-copyleft ; some autotools related files
                      "file://zinnia/aclocal.m4")
                     license:x11 ; 'install-sh'
                     license:public-domain))))) ; 'install-sh'


