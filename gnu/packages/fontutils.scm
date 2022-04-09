;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019, 2020, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020, 2021 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Felipe Balbi <balbi@kernel.org>
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

(define-module (gnu packages fontutils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages tex)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system meson)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public freetype
  (package
   (name "freetype")
   (version "2.10.4")
   (source
    (origin
      (method url-fetch)
      (uri (string-append "mirror://savannah/freetype/freetype-"
                          version ".tar.xz"))
      (sha256
       (base32 "112pyy215chg7f7fmp2l9374chhhpihbh8wgpj5nj6avj3c59a46"))))
   (build-system gnu-build-system)
   (arguments
    ;; The use of "freetype-config" is deprecated, but other packages still
    ;; depend on it.
    `(#:configure-flags (list "--enable-freetype-config")))
   (native-inputs
    (list pkg-config))
   (propagated-inputs
    ;; These are all in the Requires.private field of freetype2.pc.
    ;; XXX: add harfbuzz.
    (list libpng zlib))
   (synopsis "Font rendering library")
   (description
    "Freetype is a library that can be used by applications to access the
contents of font files.  It provides a uniform interface to access font files.
It supports both bitmap and scalable formats, including TrueType, OpenType,
Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others.  It supports high-speed
anti-aliased glyph bitmap generation with 256 gray levels.")
   (license license:freetype)           ; some files have other licenses
   (home-page "https://www.freetype.org/")))

(define-public opentype-sanitizer
  (package
    (name "opentype-sanitizer")
    (version "8.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/khaledhosny/ots"
                                  "/releases/download/v" version
                                  "/ots-" version ".tar.xz"))
              (sha256
               (base32
                "17z8cxv48rfig5k7j3xk3bmbf7rm3kxsc3bazix96l0wws58r569"))))
    (build-system meson-build-system)
    (native-inputs (list googletest pkg-config))
    (inputs (list freetype lz4 woff2 zlib))
    (home-page "https://github.com/khaledhosny/ots")
    (synopsis "Sanitizer for OpenType fonts")
    (description "The OpenType Sanitizer (OTS) parses and serializes OpenType
files (OTF, TTF) and WOFF and WOFF2 font files, validating them and sanitizing
them as it goes.")
    (license license:bsd-3)))

(define-public python-afdko
  (package
    (name "python-afdko")
    (version "3.8.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "afdko" version))
       (sha256
        (base32 "171r9f7n8fgz37dkcgpzj508lxfafcyzzx43ps12j1z2nk1sk905"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-problematic-requirements
            (lambda _
              (substitute* "requirements.txt"
                ;; Remove lxml because the version requested here is different
                ;; than the one propagated by the python-fonttools package.
                (("^lxml==.*") ""))))
          (add-after 'unpack 'patch-setup.py
            (lambda _
              ;; There is no use for Python-provided CMake nor Ninja binaries.
              (substitute* '("pyproject.toml" "setup.py")
                ((".*cmake.*") "")
                ((".*ninja.*") ""))))
          (add-after 'unpack 'unbundle-antlr4-cpp
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "CMakeLists.txt"
                (("^include\\(ExternalAntlr4Cpp).*")
                 (format #f "include_directories(SYSTEM ~a)"
                         (search-input-directory inputs
                                                 "include/antlr4-runtime"))))
              (substitute* "c/makeotf/lib/hotconv/CMakeLists.txt"
                (("antlr4_static")
                 "antlr4-runtime"))))
          ;; The test suite expects the commands to be Python rather than
          ;; shell scripts, so move the wrap phase after the tests.
          (delete 'wrap)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "pytest" "-vv"))))
          (add-after 'check 'wrap
            (assoc-ref %standard-phases 'wrap))
          (add-before 'wrap 'wrap-PATH
            (lambda _
              ;; The commands execute other commands from this package from
              ;; PATH; by wrapping them with bindir, they can be found even
              ;; when the command is run from its store location.
              (let* ((bindir (string-append #$output "/bin"))
                     (commands (find-files bindir)))
                (for-each (lambda (c)
                            (wrap-program c
                              `("PATH" prefix (,bindir))))
                          commands)))))))
    (native-inputs
     (list ninja python-pytest python-scikit-build python-wheel))
    (inputs (list java-antlr4-runtime-cpp `(,util-linux "lib")))
    (propagated-inputs
     (list psautohint
           python-booleanoperations
           python-defcon
           python-fontmath
           python-fonttools-next
           python-lxml
           python-tqdm
           python-ufonormalizer
           python-ufoprocessor))
    (home-page "https://github.com/adobe-type-tools/afdko")
    (synopsis "Adobe Font Development Kit for OpenType")
    (description "The Adobe Font Development Kit for OpenType (AFDKO) is a set
of tools for building OpenType font (OTF) files from PostScript and TrueType
font data.  It includes the following commands:
@table @command
@item buildcff2vf
Assemble a CFF2 variable font from a .designspace file.
@item buildmasterotfs
Build master source OpenType/CFF fonts from a @file{.designspace} file
and UFO master source fonts.
@item charplot
@itemx digiplot
@itemx fontplot
@itemx fontsetplot
@itemx hintplot
@itemx waterfallplot
Aliases for the corresponding options of the @command{proofpdf} command.
@item checkoutlinesufo
Perform outline quality checks.  It can also remove path overlaps.
@item comparefamily
Look in a specific directory, examine and report on all the OpenType fonts found.
@item type1
@itemx detype1
Compile and decompile, respectively, a Type 1 font to and from a plain-text
representation.
@item makeinstancesufo
Generate UFO font instances from a set of master UFO fonts.
@item makeotfexe
Read all the font data and build the final OpenType font.
@item makeotf
This command can be used to prepare the input files needed by
@command{makeotfexe}.
@item mergefonts
Merge one or more fonts into a parent font.
@item otc2otf
Extract all OpenType fonts from the parent OpenType Collection font.
@item otf2otc
Build an OpenType Collection font file from two or more OpenType font
files.
@item otf2ttf
Converts OpenType-CFF fonts to TrueType.
@item rotatefont
Apply a Postscript transform matrix to the source font files.
@item sfntdiff
Low-level comparison of two OpenType font files.
@item sfntedit
Support table-editing, listing, and checksumming options on
sfnt-formatted files such as OpenType Format (OTF) or TrueType.
@item spot
Dump sfnt data from plain files or Macintosh resource files.
@item ttfcomponentizer
Take in a TrueType font and look for a UFO font stored in the same directory.
Use the UFO's components data to compose matching TrueType glyphs.
@item ttfdecomponentizer
Take in a TrueType font and decompose any composite glyphs into simple glyphs.
@item ttxn
Make a normalized dump of the font, or of selected tables.
@item tx
The @command{tx} (Type eXchange) is a test harness for the CoreType libraries
but also provides many useful font conversion and analysis facilities.
@end table")
    (license license:asl2.0)))

(define-public python-cffsubr
  (package
    (name "python-cffsubr")
    (version "0.2.9.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cffsubr" version))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "external")) ;unbundle ADFKO
       (sha256
        (base32 "0p7wyagkmwf4agr6ysgswrpmpifx5rz8dnjbcs2gmj29rwnl2cbb"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-setup.py
            (lambda _
              (substitute* '("pyproject.toml"
                             "setup.py")
                ;; This is not needed when building the package.
                (("setuptools-git-ls-files") "")
                ;; Do not attempt to build the unbundled ADFKO.
                (("cmdclass\\[\"build_ext\"] = ExecutableBuildExt.*")
                 ""))))
          (add-after 'unpack 'patch-tx-path
            (lambda* (#:key inputs #:allow-other-keys)
              (define tx (search-input-file inputs "bin/tx"))
              (substitute* "src/cffsubr/__init__.py"
                (("TX_EXE = \"tx\"")
                 (format #f "TX_EXE = ~s" tx))
                ;; Use the full 'tx' file name directly.
                (("with path\\(__name__, TX_EXE) as tx_cli:")
                 "")
                (("    (return subprocess.run\\(\\[)str\\(tx_cli)(].*)" _ h t)
                 (format #f "~a~s~a" h tx t)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest python-setuptools-scm python-wheel))
    (inputs (list python-afdko))
    (propagated-inputs (list python-fonttools))
    (home-page "https://github.com/adobe-type-tools/cffsubr")
    (synopsis "Compact Font Format (CFF) subroutinizer")
    (description "This package provides the @command{cffsubr} command, a
Compact Font Format (CFF) subroutinizer based on the Adobe Font Development
Kit for OpenType (AFDKO) @command{tx} tool.")
    (license license:asl2.0)))

(define-public python-cu2qu
  (package
    (name "python-cu2qu")
    (version "1.6.7.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cu2qu" version ".zip"))
       (sha256
        (base32 "1x762r7bf39g6aivfvrmq00h6f07abvs9x1xm0fz8l81vq8jz64c"))))
    (build-system python-build-system)
    (propagated-inputs (list python-fonttools-full))
    (native-inputs
     (list python-cython
           python-defcon
           python-pytest
           python-pytest-runner
           python-setuptools-scm
           unzip))
    (home-page "https://github.com/googlefonts/cu2qu")
    (synopsis "Cubic-to-quadratic bezier curve conversion")
    (description "This library provides functions which take in @acronym{UFO,
Unified Font Object} objects (such as Defcon Fonts or Robofab RFonts) and
converts any cubic curves to quadratic.  The most useful function is probably
@code{fonts_to_quadratic}.")
    (license license:asl2.0)))

(define-public python-fontmath
  (package
    (name "python-fontmath")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fontMath" version ".zip"))
       (sha256
        (base32 "001756zxn2386dm4svgqjgw5026hvyacxl09b2qlk7s06phpcphw"))))
    (build-system python-build-system)
    (propagated-inputs (list python-fonttools))
    (native-inputs
     (list python-setuptools-scm
           python-pytest
           python-pytest-runner
           python-wheel
           unzip))
    (home-page "https://github.com/robotools/fontMath")
    (synopsis "Fast font mathematical operations library")
    (description "This package provides a set of objects for performing fast
font, glyph, etc. mathematical operations on font data.")
    (license license:expat)))

;;; An untested variant used to break a cycle with python-booleanoperations.
(define-public python-fontpens-bootstrap
  (package
    (name "python-fontpens-bootstrap")
    (version "0.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fontPens" version ".zip"))
       (sha256
        (base32 "1za15dzsnymq6d9x7xdfqwgw4a3003wj75fn2crhyidkfd2s3nd6"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f))
    (propagated-inputs (list python-fonttools))
    (native-inputs (list unzip))
    (home-page "https://github.com/robofab-developers/fontPens")
    (synopsis "Python classes implementing the pen protocol")
    (description "This package provides a collection of Python classes
implementing the pen protocol for manipulating glyphs.")
    (license license:bsd-3)))

(define-public python-fontpens
  (hidden-package
   (package/inherit python-fontpens-bootstrap
     (name "python-fontpens")
     (arguments
      (substitute-keyword-arguments (package-arguments python-fontpens-bootstrap)
        ((#:tests? _ #f)
         #t)))
     (native-inputs
      (modify-inputs (package-native-inputs python-fontpens-bootstrap)
        (append python-fontparts-bootstrap
                python-fontpens-bootstrap
                python-pytest
                python-pytest-runner))))))

;;; A variant used to break a cycle with python-fontpens.
(define-public python-fontparts-bootstrap
  (hidden-package
   (package
     (name "python-fontparts-bootstrap")
     (version "0.10.4")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fontParts" version ".zip"))
        (sha256
         (base32 "1ic453q86s5hsw8mxnclk1vr4qp69fd67gywhv23zqwz9a7kb7lh"))))
     (build-system python-build-system)
     (propagated-inputs
      (list python-booleanoperations
            python-defcon-bootstrap
            python-fontmath
            python-fonttools))
     (native-inputs (list unzip))
     (home-page "https://github.com/robotools/fontParts")
     (synopsis "Library for interacting with font parts")
     (description "FontParts is an @acronym{API, Application Programming
Interface} for interacting with the parts of fonts during the font development
process.  FontParts is the successor of RoboFab.")
     (license license:expat))))

(define-public python-fontparts
  (package/inherit python-fontparts-bootstrap
    (name "python-fontparts")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-fontparts-bootstrap)
       (replace "python-defcon-bootstrap" python-defcon)))
    (properties
     (alist-delete 'hidden?
                   (package-properties python-fontparts-bootstrap)))))

(define-public python-opentype-sanitizer
  (package
    (name "python-opentype-sanitizer")
    (version "8.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "opentype-sanitizer" version))
       (sha256
        (base32 "1wjy6chbnj9ic5yjxal6spln5jfzr8cigqs6ab0gj7q60dndrl5k"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle-opentype-sanitizer
            (lambda* (#:key inputs #:allow-other-keys)
              (delete-file-recursively "src/c")
              (substitute* "setup.py"
                (("^cmdclass\\[\"download\"].*") "")
                (("^cmdclass\\[\"build_ext\"].*") "")
                (("^cmdclass\\[\"egg_info\"].*") ""))
              (substitute* "src/python/ots/__init__.py"
                (("^OTS_SANITIZE = .*")
                 (format #f "OTS_SANITIZE = ~s~%"
                         (search-input-file inputs "bin/ots-sanitize"))))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest python-setuptools-scm))
    (inputs (list opentype-sanitizer))
    (home-page "https://github.com/googlefonts/ots-python")
    (synopsis "Python wrapper for OpenType Sanitizer")
    (description "Python wrapper for the OpenType Sanitizer library.")
    (license license:bsd-3)))

(define-public python-mutatormath
  (package
    (name "python-mutatormath")
    (version "3.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "MutatorMath" version ".zip"))
       (sha256
        (base32 "0r1qq45np49x14zz1zwkaayqrn7m8dn2jlipjldg2ihnmpzw29w1"))))
    (build-system python-build-system)
    (propagated-inputs (list python-defcon python-fontmath python-fonttools))
    (native-inputs (list unzip))
    (home-page "https://github.com/LettError/MutatorMath")
    (synopsis "Piecewise linear interpolation Python library")
    (description "MutatorMath is a Python library for the calculation of
piecewise linear interpolations in n-dimensions with any number of masters. It
was developed for interpolating data related to fonts, but if can handle any
arithmetic object.")
    (license license:bsd-3)))

(define-public psautohint-font-data
  ;; There is no release tag, so use the latest commit.
  (let ((revision "0")
        (commit "1e4c5061d328105c4dcfcb6fdbc27ec49b3e9d23"))
    (hidden-package
     (package
       (name "psautohint-font-data")
       (version (git-version "0.0.0" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/adobe-type-tools/psautohint-testdata")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0p7g8mnndzp8zpbj9h6lkvfdpvd74fy10q8wmkagbg2ahbdi1zva"))))
       (build-system copy-build-system)
       (home-page "https://github.com/adobe-type-tools/psautohint-testdata")
       (synopsis "Test font data psautohint")
       (description "This package contains the font data used by the test
suite of the @code{psautohint} package.")
       ;; The bundle contains font data from the Cantarell, Libertinus, Source
       ;; Code Pro, Source Serif Pro, all available under the same license.
       (license license:silofl1.1)))))

(define-public psautohint
  (package
    (name "psautohint")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "psautohint" version))
       (sha256
        (base32 "0zzz7hy1kkkjfrrm9ly2di3xv2x1ywdqhbyqy21k670jysldw3nm"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-font-data
            ;; The data is copied as it needs to be writable for the tests.
            (lambda _
              (copy-recursively
               #$(this-package-native-input "psautohint-font-data")
               "tests/integration/data")
              (for-each make-file-writable
                        (find-files "tests/integration/data"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (propagated-inputs (list python-fonttools))
    (native-inputs
     (list psautohint-font-data
           python-fs
           python-pytest
           python-pytest-cov
           python-pytest-randomly
           python-pytest-xdist
           python-setuptools-scm
           python-wheel))
    (home-page "https://github.com/adobe-type-tools/psautohint")
    (synopsis "Adobe's PostScript autohinter")
    (description "This package provides the @command{autohinter} command that
can be used to hint PostScript fonts.  A Python wrapper is also included.")
    (license license:asl2.0)))

(define-public python-skia-pathops
  (package
    (name "python-skia-pathops")
    (version "0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skia-pathops" version ".zip"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/cpp")) ;140+ MiB of stuff
       (sha256
        (base32 "1456rclfn6a01c2cchlgyn166zppcjcqij0k5gwmm8gvzsd5rn0r"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'configure-env
            (lambda _
              (setenv "BUILD_SKIA_FROM_SOURCE" "0")))
          (add-after 'unpack 'adjust-c++-language
            (lambda _
              ;; Our version of Skia requires c++17.
              (substitute* "setup.py"
                (("-std=c\\+\\+14")
                 "-std=c++17"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "pytest" "-vv")))))))
    (native-inputs
     (list pkg-config
           python-cython
           python-pytest
           python-setuptools-scm
           unzip))
    (inputs (list skia))
    (home-page "https://github.com/fonttools/skia-pathops")
    (synopsis "Python bindings for the Skia library's Path Ops module")
    (description "This package provides Python bindings for the Path Ops
module of the Skia library, performing boolean operations on
paths (intersection, union, difference, xor).")
    (license license:bsd-3)))

(define-public python-ufoprocessor
  (package
    (name "python-ufoprocessor")
    (version "1.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ufoProcessor" version ".zip"))
       (sha256
        (base32 "0ns11aamgavgsfj8qf5kq7dvzmgl0mhr1cbych2f075ipfdvva5s"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              ;; Most of the tests appear to be a work in
                              ;; progress; run only a subset.
                              (invoke "python" "Tests/tests.py")))))))
    (propagated-inputs
     (list python-defcon
           python-fontmath
           python-fontparts
           python-fonttools
           python-mutatormath))
    (native-inputs (list unzip))
    (home-page "https://github.com/LettError/ufoProcessor")
    (synopsis "Process and generate @acronym{UFO, Unified Font Object} files")
    (description "This Python package processes and generates instances for
@acronym{UFO, Unified Font Object} files, glyphs and other data.  It can,
among other things:
@itemize
@item Collect source materials.
@item Provide mutators for specific glyphs, font info, kerning so that other
tools can generate partial instances.
@item Support designspace format 4 with layers.
@item Apply avar-like designspace bending.
@item Apply rules.
@item Generate actual UFO instances in formats 2 and 3.
@item Round geometry as requested.
@end itemize")
    (license license:expat)))

(define-public python-ufonormalizer
  (package
    (name "python-ufonormalizer")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ufonormalizer" version ".zip"))
       (sha256
        (base32 "0v5awian2alap7nvxfz38aahyqbqnma16nrqcpr8602hbbki04g6"))))
    (build-system python-build-system)
    (native-inputs (list python-setuptools-scm unzip))
    (home-page "https://github.com/unified-font-object/ufoNormalizer")
    (synopsis "Script to normalize @acronym{UFO, Unified Font Object} data")
    (description "The purpose of the @command{ufonormalizer} command is to
provide a standard formatting so that updates to @acronym{UFO, Unified Font
Object} data can be usefully versioned.  Examples of formatting applied by
ufoNormalizer include:
@itemize
@item Changing floating-point numbers to integers where it doesn't alter the
value (e.g. @samp{x=\"95.0\"} becomes @samp{x=\"95\"})
@item Rounding floating-point numbers to 10 digits
@item Formatting XML with tabs rather than spaces.
@end itemize")
    (license license:bsd-3)))

(define-public ttfautohint
  (package
    (name "ttfautohint")
    (version "1.8.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/freetype/ttfautohint-"
                           version ".tar.gz"))
       (sha256
        (base32
         "0zpqgihn3yh3v51ynxwr8asqrijvs4gv686clwv7bm8sawr4kfw7"))))
    (build-system gnu-build-system)
    (native-inputs
     (list flex bison pkg-config))
    (inputs
     (list freetype harfbuzz))
    (arguments
     `(#:configure-flags '("--disable-static"
                           "--with-qt=no"))) ;no gui
    (synopsis "Automated font hinting")
    (description
     "ttfautohint provides a 99% automated hinting process and a platform for
finely hand-hinting the last 1%.  It is ideal for web fonts and supports many
scripts.")
    (license (list license:gpl2+ license:freetype)) ;choose one or the other
    (home-page "https://www.freetype.org/ttfautohint/")))

(define-public woff-tools
  (package
    (name "woff-tools")
    (version "2009.10.04")
    (source
     (origin
       (method url-fetch)
       ;; Upstream source is unversioned, so use Debian's versioned tarball
       (uri (string-append "mirror://debian/pool/main/w/woff-tools/"
                           "woff-tools_" version ".orig.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1i97gkqa6jfzlslsngqf556kx60knlgf7yc9pzsq2pizc6f0d4zl"))))
    (build-system gnu-build-system)
    (inputs
     (list zlib))
    (arguments
     `(#:make-flags '(,(string-append "CC=" (cc-for-target)))
       #:tests? #f                      ;no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ;no configuration
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "sfnt2woff" bin)
               (install-file "woff2sfnt" bin))
             #t)))))
    (synopsis "Convert between OpenType and WOFF fonts")
    (description
     "This package provides two tools:
@table @code
@item sfnt2woff
Converts OpenType fonts to WOFF fonts
@item woff2sfnt
Converts WOFF fonts to OpenType fonts
@end table")
    (license (list license:mpl1.1 license:gpl2+ license:lgpl2.1+))
    (home-page "https://people.mozilla.com/~jkew/woff/")))

(define-public ttf2eot
  (package
    (name "ttf2eot")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wget/ttf2eot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0l2yh2ialx7135pjzhjs204kk3br7zxjr09zwaia493by2adzigr"))
       (patches (list (search-patch "ttf2eot-cstddef.patch")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configuration
         (replace 'install              ; no install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "ttf2eot" bin)
               #t))))))
    (synopsis "Convert from TrueType to Embeddable Open Type")
    (description
     "This package contains a commandline wrapper around OpenTypeUtilities.cpp
from Chromium, used to make EOT (Embeddable Open Type) files from
TTF (TrueType/OpenType Font) files.")
    ;; While the README states "License: Derived from WebKit, so BSD/LGPL
    ;; 2/LGPL 2.1", the single derived source file includes only BSD in its
    ;; license header, and the wrapper source contains no license header.
    (license license:bsd-2)
    (home-page "https://github.com/wget/ttf2eot")))

(define-public ttf2pt1
  (package
    (name "ttf2pt1")
    (version "3.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/ttf2pt1/ttf2pt1/"
                                  version "/ttf2pt1-" version ".tgz"))
              (sha256
               (base32
                "1l718n4k4widx49xz7qrj4mybzb8q67kp2jw7f47604ips4654mf"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove trailing backslashes in the sed expression of the
                  ;; 'install' rule since sed would otherwise fail.
                  (substitute* "Makefile"
                    (("\\|;\\\\[[:space:]]*$") "|; "))
                  #t))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                                ;no tests
       #:phases (modify-phases %standard-phases
                  (replace 'configure
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* "Makefile"
                          (("INSTDIR =.*")
                           (string-append "INSTDIR = " out "\n"))
                          (("OWNER = .*")
                           "OWNER = `id -un`\n")
                          (("GROUP = .*")
                           "GROUP = `id -g`\n"))
                        #t)))
                  (replace 'build
                    (lambda _
                      (invoke "make" "-j"
                              (number->string (parallel-job-count))
                              "all" "CC=gcc"))))))
    (inputs (list perl))
    (synopsis "Convert TrueType fonts to Postscript Type 1")
    (description
     "TTF2PT1 provides tools to convert most TrueType fonts (or other formats
supported by the FreeType library) to an Adobe Type 1 @file{.pfa} or
@file{.pfb} file.  Another use is as a hinting engine: feed it an unhinted or
poorly hinted Adobe Type 1 font through the FreeType library and get it back
with freshly generated hints.  The files produced by default are in
human-readable form, which further needs to be encoded with t1utilities to
work with most software requiring Type 1 fonts.")
    (home-page "http://ttf2pt1.sourceforge.net/")
    (license license:bsd-3)))

(define-public woff2
  (package
    (name "woff2")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/google/woff2")
         (commit (string-append "v" version))))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "13l4g536h0pr84ww4wxs2za439s0xp1va55g6l478rfbb1spp44y"))))
    (build-system cmake-build-system)
    (outputs '("out" "bin"))
    (arguments
     `(#:tests? #f                      ;no test suite
       #:configure-flags
       (list
        (string-append "-DCMAKE_INSTALL_BINDIR="
                       (assoc-ref %outputs "bin")
                       "/bin")
        (string-append "-DCMAKE_INSTALL_INCLUDEDIR="
                       (assoc-ref %outputs "out")
                       "/include")
        (string-append "-DCMAKE_INSTALL_LIBDIR="
                       (assoc-ref %outputs "out")
                       "/lib"))
       #:phases
       (modify-phases %standard-phases
         ;; To install both binaries and libraries.
         (add-after 'unpack 'patch-installation
           (lambda _
             (substitute* "CMakeLists.txt"
               (("NOT BUILD_SHARED_LIBS")
                "BUILD_SHARED_LIBS")))))))
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list brotli))                     ;libwoff2dec.pc requires libbrotlidec
    (synopsis "Libraries and tools for WOFF2 font format")
    (description "WOFF2 provides libraries and tools to handle the Web Open
Font Format (WOFF).")
    (home-page "https://w3c.github.io/woff/woff2/")
    (license license:expat)))

(define-public fontconfig
  (hidden-package
   (package
     (name "fontconfig-minimal")
     (version "2.13.94")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "https://www.freedesktop.org/software/"
                     "fontconfig/release/fontconfig-" version ".tar.xz"))
               (sha256 (base32
                        "0g004r0bkkqz00mpm3svnnxn7d83158q0yb9ggxryizxfg5m5w55"))
               (patches (search-patches "fontconfig-cache-ignore-mtime.patch"))))
     (build-system gnu-build-system)
     ;; In Requires or Requires.private of fontconfig.pc.
     (propagated-inputs `(("expat" ,expat)
                          ("freetype" ,freetype)
                          ("libuuid" ,util-linux "lib")))
     (inputs
      ;; We use to use 'font-ghostscript' but they are not recognized by newer
      ;; versions of Pango, causing many applications to fail to find fonts
      ;; otherwise.
      (list font-dejavu))
     (native-inputs
      `(("gperf" ,gperf)
        ("pkg-config" ,pkg-config)
        ("python" ,python-minimal)))    ;to avoid a cycle through tk
     (arguments
      `(#:configure-flags
        (list "--disable-docs"
              "--with-cache-dir=/var/cache/fontconfig"
              ;; register the default fonts
              (string-append "--with-default-fonts="
                             (assoc-ref %build-inputs "font-dejavu")
                             "/share/fonts"))
        #:phases
        (modify-phases %standard-phases
          (add-before 'check 'skip-problematic-tests
            (lambda _
              ;; SOURCE_DATE_EPOCH doesn't make sense when ignoring mtime
              (unsetenv "SOURCE_DATE_EPOCH")

              (substitute* "test/run-test.sh"
                ;; The crbug1004254 test attempts to fetch fonts from the
                ;; network.
                (("\\[ -x \"\\$BUILDTESTDIR\"/test-crbug1004254 \\]")
                 "false"))))
          (replace 'install
            (lambda _
              ;; Don't try to create /var/cache/fontconfig.
              (invoke "make" "install"
                      "fc_cachedir=$(TMPDIR)"
                      "RUN_FC_CACHE_TEST=false"))))))
     (synopsis "Library for configuring and customizing font access")
     (description
      "Fontconfig can discover new fonts when installed automatically;
perform font name substitution, so that appropriate alternative fonts can
be selected if fonts are missing;
identify the set of fonts required to completely cover a set of languages;
have GUI configuration tools built as it uses an XML-based configuration file;
efficiently and quickly find needed fonts among the set of installed fonts;
be used in concert with the X Render Extension and FreeType to implement
high quality, anti-aliased and subpixel rendered text on a display.")
                                        ; The exact license is more X11-style than BSD-style.
     (license (license:non-copyleft "file://COPYING"
                                    "See COPYING in the distribution."))
     (native-search-paths
      ;; Since version 2.13.94, fontconfig knows to find fonts from
      ;; XDG_DATA_DIRS.
      (list (search-path-specification
             (variable "XDG_DATA_DIRS")
             (files '("share")))))
     (home-page "https://www.freedesktop.org/wiki/Software/fontconfig"))))

;;; The documentation of fontconfig is built in a separate package, as it
;;; causes a dramatic increase in the size of the closure of fontconfig.  This
;;; is intentionally named 'fontconfig', as it's intended as the user-facing
;;; fontconfig package.
(define-public fontconfig-with-documentation
  (package
    (inherit fontconfig)
    (name "fontconfig")
    (outputs (cons "doc" (package-outputs fontconfig)))
    (arguments
     (substitute-keyword-arguments (package-arguments fontconfig)
       ((#:configure-flags configure-flags)
        `(delete "--disable-docs" ,configure-flags))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-after 'unpack 'no-pdf-doc
             (lambda _
               ;; Don't build documentation as PDF.
               (substitute* "doc/Makefile.in"
                 (("^PDF_FILES = .*")
                  "PDF_FILES =\n"))))
           (add-after 'install 'move-man-sections
             (lambda* (#:key outputs #:allow-other-keys)
               ;; Move share/man/man{3,5} to the "doc" output.  Leave "man1" in
               ;; "out" for convenience.
               (let ((out (assoc-ref outputs "out"))
                     (doc (assoc-ref outputs "doc")))
                 (for-each (lambda (section)
                             (let ((source (string-append out "/share/man/"
                                                          section))
                                   (target (string-append doc "/share/man/"
                                                          section)))
                               (copy-recursively source target)
                               (delete-file-recursively source)))
                           '("man3" "man5")))))))))
    (native-inputs
     (append (package-native-inputs fontconfig)
             `(("docbook-utils" ,docbook-utils))))
    (properties (alist-delete 'hidden? (package-properties fontconfig)))))

(define-public t1lib
  (package
   (name "t1lib")
   (version "5.1.2")
   (source (origin
            (method url-fetch)
            (uri (list (string-append "ftp://sunsite.unc.edu/pub/Linux/libs/"
                                      "graphics/" name "-" version ".tar.gz")
                       (string-append "https://fossies.org/linux/misc/old/"
                                      name "-" version ".tar.gz")))
            (sha256 (base32
                     "0nbvjpnmcznib1nlgg8xckrmsw3haa154byds2h90y2g0nsjh4w2"))
            (patches (search-patches
                       "t1lib-CVE-2010-2642.patch" ; 2011-0443, 2011-5244
                       "t1lib-CVE-2011-0764.patch"
                       "t1lib-CVE-2011-1552+.patch")))) ; 2011-1553, 2011-1554
   (properties `((lint-hidden-cve . ("CVE-2011-0433"
                                     "CVE-2011-1553"
                                     "CVE-2011-1554"
                                     "CVE-2011-5244"))))
   (build-system gnu-build-system)
   (arguments
    ;; Making the documentation requires latex, but t1lib is also an input
    ;; for building texlive.
    `(#:tests? #f ; no test target
      #:make-flags
      '("without_doc")))
   (synopsis "Library for generating bitmaps from Type 1 fonts")
   (description
    "T1lib is a library for generating/rasterising bitmaps from Type 1 fonts.
It is based on the code of the X11 rasteriser of the X11 project.

The bitmaps created by t1lib are returned in a data structure with type
GLYPH.  This special GLYPH-type is also used in the X11 window system to
describe character bitmaps.  It contains the bitmap data as well as some
metric information.  But t1lib is in itself entirely independent of the
X11-system or any other graphical user interface.")
   (license license:gpl2)
   (home-page "https://www.t1lib.org/")))

(define-public teckit
  (package
    (name "teckit")
    (version "2.5.10")                  ; signed by key 0xC9183BEA0288CDEE
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/silnrsi/teckit/releases/"
                           "download/v" version "/teckit-" version ".tar.gz"))
       (sha256
        (base32 "12qnf8nhxyr4d5pc01s3vc6h726506957an4vvmmfz633cqi5796"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
     (list zlib expat))
    (native-inputs
     (list perl))                 ;for the tests
    (synopsis "Toolkit for encoding conversions")
    (description
     "TECkit is a low-level toolkit intended to be used by other applications
that need to perform encoding conversions (e.g., when importing legacy data
into a Unicode-based application).  The primary component of the TECkit
package is therefore a library that performs conversions; this is the
\"TECkit engine\".  The engine relies on mapping tables in a specific binary
format (for which documentation is available); there is a compiler that
creates such tables from a human-readable mapping description (a simple
text file).

To facilitate the development and testing of mapping tables for TECkit,
several applications are also included in the current package; these
include simple tools for applying conversions to plain-text and Standard
Format files, as well as both command-line and simple GUI versions of the
TECkit compiler.  However, it is not intended that these tools will be the
primary means by which end users perform conversions, and they have not
been designed, tested, and debugged to the extent that general-purpose
applications should be.")
    (license license:lgpl2.1+)
    (home-page "https://scripts.sil.org/cms/scripts/page.php?cat_id=teckit")))

(define-public graphite2
  (package
   (name "graphite2")
   (version "1.3.13")
   (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/silnrsi/graphite/releases/"
                           "download/" version "/" name "-" version ".tgz"))
       (sha256
        (base32
         "01jzhwnj1c3d68dmw15jdxly0hwkmd8ja4kw755rbkykn1ly2qyx"))))
   (build-system cmake-build-system)
   (native-inputs
    (list python python-fonttools))
   (inputs
    (list freetype))
   (synopsis "Reimplementation of the SIL Graphite text processing engine")
   (description
    "Graphite2 is a reimplementation of the SIL Graphite text processing
engine.  Graphite is a smart font technology designed to facilitate the
process known as shaping.  This process takes an input Unicode text string
and returns a sequence of positioned glyphids from the font.")
   (license license:lgpl2.1+)
   (home-page "https://github.com/silnrsi/graphite")))

(define-public potrace
  (package
    (name "potrace")
    (version "1.16")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/potrace/" version
                          "/potrace-" version ".tar.gz"))
      (sha256
       (base32
        "1k3sxgjqq0jnpk9xxys05q32sl5hbf1lbk1gmfxcrmpdgnhli0my"))))
    (build-system gnu-build-system)
    (native-inputs (list ghostscript)) ;for tests
    (inputs (list zlib))
    (arguments
     `(#:configure-flags
      `("--with-libpotrace"))) ; install library and headers
    (synopsis "Transform bitmaps into vector graphics")
    (description
     "Potrace is a tool for tracing a bitmap, which means, transforming a
bitmap into a smooth, scalable image.  The input is a bitmap (PBM, PGM, PPM,
or BMP format), and the default output is an encapsulated PostScript
file (EPS).  A typical use is to create EPS files from scanned data, such as
company or university logos, handwritten notes, etc.  The resulting image is
not \"jaggy\" like a bitmap, but smooth.  It can then be rendered at any
resolution.")
    (license license:gpl2+)
    (home-page "http://potrace.sourceforge.net/")))

(define-public libotf
  (package
    (name "libotf")
    (version "0.9.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/m17n/libotf-"
                                  version ".tar.gz"))
              (sha256
               (base32 "0sq6g3xaxw388akws6qrllp3kp2sxgk2dv4j79k6mm52rnihrnv8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (propagated-inputs
     (list freetype))
    (home-page "https://www.nongnu.org/m17n/")
    (synopsis "Library for handling OpenType Font")
    (description "This library can read Open Type Layout Tables from an OTF
file.  Currently these tables are supported; head, name, cmap, GDEF, GSUB, and
GPOS.  It can convert a Unicode character sequence to a glyph code sequence by
using the above tables.")
    (license license:lgpl2.0+)))

(define-public libspiro
  (package
    (name "libspiro")
    (version "20200505")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/fontforge/libspiro/releases"
                          "/download/" version "/libspiro-dist-" version ".tar.gz"))
      (sha256
       (base32
        "0j8fmyj4wz6mqk17dqs6f8jx0i52n68gv5px17qbrjnbilg9mih6"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (synopsis "Clothoid to bezier conversion library")
    (description
     "Raph Levien's Spiro package as a library.  A mechanism for drawing
smooth contours with constant curvature at the spline joins.")
    (license license:gpl2+)
    (home-page "http://libspiro.sourceforge.net/")))

(define-public libuninameslist
  (package
    (name "libuninameslist")
    (version "20200313")
    (home-page "https://github.com/fontforge/libuninameslist")
    (source
     (origin
       (method url-fetch)
       (uri (string-append home-page "/releases/download/" version
                           "/libuninameslist-dist-" version ".tar.gz"))
       (sha256
        (base32
         "10ri80c64xb4rhbif3sr87y5vhi3m702zb0m02imvj1jib9rq0m8"))))
    (build-system gnu-build-system)
    (synopsis "Unicode names and annotation list")
    (description
     "LibUniNamesList holds www.unicode.org Nameslist.txt data which can be
useful for programs that need Unicode \"Names\", \"Annotations\", and block
definitions.")
    ;; COPYING specifies GPL2, but according to LICENSE it only covers the
    ;; configure script.  The actual code is BSD-3, and the Unicode data
    ;; is governed by an X11-style license only found on the web.
    (license (list license:bsd-3
                   (license:x11-style
                    "https://www.unicode.org/copyright.html#License")))))

(define-public fontforge
  (package
   (name "fontforge")
   (version "20201107")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/fontforge/fontforge/releases/download/"
                  version "/fontforge-" version ".tar.xz"))
            (sha256
             (base32 "0y3c8x1i6yf6ak9m5dhr1nldgfmg7zhnwdfd57ffs698c27vmg38"))))
   (build-system cmake-build-system)
   (native-inputs
    (list pkg-config))
   (inputs `(("cairo"           ,cairo)
             ("fontconfig"      ,fontconfig) ;dlopen'd
             ("freetype"        ,freetype)
             ("gettext"         ,gettext-minimal)
             ("libICE"          ,libice)
             ("libSM"           ,libsm)
             ("libX11"          ,libx11)
             ("libXi"           ,libxi)
             ("libjpeg"         ,libjpeg-turbo)
             ("libltdl"         ,libltdl)
             ("libpng"          ,libpng)
             ("libspiro"        ,libspiro)
             ("libtiff"         ,libtiff)
             ("libungif"        ,libungif)
             ("libuninameslist" ,libuninameslist)
             ("libxft"          ,libxft)
             ("libxml2"         ,libxml2)
             ("pango"           ,pango)
             ("potrace"         ,potrace)
             ("python"          ,python)
             ("zlib"            ,zlib)))
   (arguments
    '(#:configure-flags '(;; TODO: Provide GTK+ for the Wayland-friendly GDK
                          ;; backend, instead of the legacy X11 backend.
                          ;; Currently it introduces a circular dependency.
                          "-DENABLE_X11=ON")
      #:phases
      (modify-phases %standard-phases
        (add-after 'unpack 'do-not-override-RPATH
          (lambda _
            ;; Do not attempt to set a default RPATH, as our ld-wrapper
            ;; already does the right thing.
            (substitute* "CMakeLists.txt"
              (("^set_default_rpath\\(\\)")
               ""))
            #t))
        (add-after 'install 'set-library-path
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out"))
                  (potrace (dirname
                            (search-input-file inputs "bin/potrace"))))
              (wrap-program (string-append out "/bin/fontforge")
                ;; Fontforge dynamically opens libraries.
                `("LD_LIBRARY_PATH" ":" prefix
                  ,(map (lambda (input)
                          (string-append (assoc-ref inputs input)
                                         "/lib"))
                        '("libtiff" "libjpeg" "libpng" "libungif"
                          "libxml2" "zlib" "libspiro" "freetype"
                          "pango" "cairo" "fontconfig")))
                ;; Checks for potrace program at runtime
                `("PATH" ":" prefix (,potrace)))
              #t))))))
   (synopsis "Outline font editor")
   (description
    "FontForge allows you to create and modify postscript, truetype and
opentype fonts.  You can save fonts in many different outline formats, and
generate bitmaps.")
   (license license:gpl3+)
   (home-page "https://fontforge.github.io")))

;; This is the last version that supports Python 2, which is needed for
;; GNU FreeFont.  Remove once no longer required.
(define-public fontforge-20190801
  (package
    (inherit fontforge)
    (version "20190801")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/fontforge/fontforge/releases/download/"
                    version "/fontforge-" version ".tar.gz"))
              (sha256
               (base32 "0lh8yx01asbzxm6car5cfi64njh5p4lxc7iv8dldr5rwg357a86r"))))
    (build-system gnu-build-system)
    (arguments
     (substitute-keyword-arguments (package-arguments fontforge)
       ((#:configure-flags _)
        ''())
       ((#:phases phases)
        `(modify-phases ,phases
           (delete 'do-not-override-RPATH)))))
    (inputs
     `(("python" ,python-2)
       ,@(alist-delete "python" (package-inputs fontforge))))))

(define-public python2-ufolib
  (package
    (name "python2-ufolib")
    (version "2.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ufoLib" version ".zip"))
       (sha256
        (base32 "07qy6mx7z0wi9a30lc2hj5i9q1gnz1n8l40dmjz2c19mj9s6mz9l"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     (list python2-fonttools))
    (native-inputs
     (list unzip python2-pytest python2-pytest-runner))
    (home-page "https://github.com/unified-font-object/ufoLib")
    (synopsis "Low-level UFO reader and writer")
    (description
     "UfoLib reads and writes Unified Font Object (UFO)
files.  UFO is a file format that stores fonts source files.")
    (license license:bsd-3)))

;;; A variant used to break a cycle between python-fontpens and
;;; python-fontparts.
(define-public python-defcon-bootstrap
  (package
    (name "python-defcon-bootstrap")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defcon" version ".zip"))
       (sha256
        (base32 "0g0bjwzdj6sskyh8snbxsxza3czdmvb807qv38mizx631cm8c2d0"))))
    (build-system python-build-system)
    (propagated-inputs (list python-fontpens-bootstrap python-fonttools-full))
    (native-inputs
     (list python-pytest
           python-pytest-runner
           python-setuptools-scm
           unzip))
    (home-page "https://github.com/robotools/defcon")
    (synopsis "Flexible objects for representing @acronym{UFO, unified font object} data")
    (description "Defcon is a set of @acronym{UFO, unified font object} based
objects optimized for use in font editing applications.  The objects are built
to be lightweight, fast and flexible.  The objects are very bare-bones and
they are not meant to be end-all, be-all objects.  Rather, they are meant to
provide base functionality so that you can focus on your application’s
behavior, not object observing or maintaining cached data.  Defcon implements
UFO3 as described by the UFO font format.")
    (license license:expat)))

(define-public python-defcon
  (hidden-package
   (package/inherit python-defcon-bootstrap
     (name "python-defcon")
     (propagated-inputs
      (modify-inputs (package-propagated-inputs python-defcon-bootstrap)
        (replace "python-fontpens-bootstrap" python-fontpens))))))

(define-public python2-defcon
  (package
    (inherit python-defcon)
    (name "python2-defcon")
    (version "0.3.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "defcon" version ".zip"))
              (sha256
               (base32
                "03jlm2gy9lvbwj68kfdm43yaddwd634jwkdg4wf0jxx2s8mwbg22"))))
    (arguments
     `(#:python ,python-2))
    (native-inputs
     (list unzip python2-pytest python2-pytest-runner))
    (propagated-inputs
     (list python2-fonttools python2-ufolib))))

(define-public nototools
  (package
    (name "nototools")
    (version "20170925")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/googlei18n/nototools")
              (commit "v2017-09-25-tooling-for-phase3-update")))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "03nzvcvwmrhfrcjhg218q2f3hfrm3vlivp4rk19sc397kh3hisiz"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     (list python2-booleanoperations
           python2-defcon
           python2-fonttools
           python2-pillow
           python2-pyclipper
           python2-ufolib))
    (home-page "https://github.com/googlei18n/nototools")
    (synopsis "Noto fonts support tools and scripts")
    (description
     "Nototools is a Python package containing Python scripts used to
maintain the Noto Fonts project.")
    (license (list license:asl2.0
                   ;; Sample texts are attributed to UN and OHCHR.
                   ;; The permissions on the UDHR are pretty lax:
                   ;; http://www.ohchr.org/EN/UDHR/Pages/Introduction.aspx
                   ;; "If UDHR translations or materials are reproduced, users
                   ;; should make reference to this website as a source by
                   ;; providing a link."
                   license:public-domain
                   (license:non-copyleft
                    "file://sample_texts/attributions.txt"
                    "See sample_texts/attributions.txt in the distribution.")))))

(define-public fcft
  (package
    (name "fcft")
    (version "3.0.1")
    (home-page "https://codeberg.org/dnkl/fcft")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0jxy92ny8b7s7yvz1mr8zpf7l2zsn506fi9f98pvh9k25jprg0cx"))))
    (build-system meson-build-system)
    (native-inputs
     (list check pkg-config scdoc))
    (propagated-inputs
     (list ;; Required by fcft.pc.
           fontconfig
           freetype
           harfbuzz
           pixman
           tllist))
    (synopsis "Font loading and glyph rasterization library")
    (description
     "@code{fcft} is a small font loading and glyph rasterization library
built on-top of FontConfig, FreeType2 and pixman.

It can load and cache fonts from a fontconfig-formatted name string, e.g.
@code{Monospace:size=12}, optionally with user configured fallback fonts.

After a font has been loaded, you can rasterize glyphs.  When doing so, the
primary font is first considered.  If it does not have the requested glyph,
the user configured fallback fonts (if any) are considered.  If none of the
user configured fallback fonts has the requested glyph, the FontConfig
generated list of fallback fonts are checked.")
    ;; The code is distributed under the Expat license, but embeds Unicode
    ;; data files carrying the Unicode license.
    (license (list license:expat license:unicode))))

(define-public fontmanager
  (package
   (name "fontmanager")
   (version "0.8.7")
   (source
    (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/FontManager/font-manager")
            (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32 "0nyda2a6vbzyz4sn9mmrr8bkifzxmmjp7x9a3c4s6n925ccy79cn"))))
   (build-system meson-build-system)
   (arguments
    `(#:glib-or-gtk? #t
      #:build-type "release"
      #:configure-flags
      (list (string-append "-Dc_link_args=-Wl,-rpath="
                           (assoc-ref %outputs "out")
                           "/lib/font-manager"))))
   (native-inputs
    `(("desktop-file-utils" ,desktop-file-utils)
      ("gettext" ,gettext-minimal)
      ("glib" ,glib "bin")
      ("gobject-introspection" ,gobject-introspection)
      ("pkg-config" ,pkg-config)
      ("python-wrapper" ,python-wrapper)
      ("vala" ,vala-0.52)
      ("yelp-tools" ,yelp-tools)))
   (inputs
    `(("fonconfig" ,fontconfig)
      ("freetype" ,freetype)
      ("gsettings-desktop-schemas" ,gsettings-desktop-schemas)
      ("gtk+" ,gtk+)
      ("json-glib" ,json-glib)
      ("libsoup" ,libsoup-minimal-2)
      ("sqlite" ,sqlite)
      ("webkitgtk" ,webkitgtk-with-libsoup2)))
   (home-page "https://fontmanager.github.io/")
   (synopsis "Simple font management for GTK+ desktop environments")
   (description "Font Manager is intended to provide a way for users to
easily manage desktop fonts, without having to resort to command-line
tools or editing configuration files by hand.
While designed primarily with the GNOME Desktop Environment in mind, it should
work well with other GTK+ desktop environments.")
   (license license:gpl3+)))

(define-public fntsample
  (package
    (name "fntsample")
    (version "5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/eugmes/fntsample")
                     (commit (string-append "release/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pcqqdriv6hq64zrqd9vhdd9p2vhimjnajcxdz10qnqgrkmm751v"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f ; There are no tests.
       #:configure-flags
       (list (string-append
              "-DUNICODE_BLOCKS=" (assoc-ref %build-inputs "unicode-blocks")))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'set-library-path
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out      (assoc-ref outputs "out"))
                    (pdf-api2 (assoc-ref inputs "perl-pdf-api2"))
                    (intl     (assoc-ref inputs "perl-libintl-perl"))
                    (perllib  (string-append pdf-api2
                                             "/lib/perl5/site_perl/"
                                             ,(package-version perl)
                                             ":" intl
                                             "/lib/perl5/site_perl/"
                                             ,(package-version perl))))
               (wrap-program (string-append out "/bin/pdfoutline")
                 `("PERL5LIB" ":" prefix (,perllib)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("cairo" ,cairo)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glib" ,glib)
       ("pango" ,pango)
       ("perl-pdf-api2" ,perl-pdf-api2)
       ("perl-libintl-perl" ,perl-libintl-perl)
       ("unicode-blocks"
        ,(let ((version "14.0.0"))
           (origin
             (method url-fetch)
             (uri (string-append "https://unicode.org/Public/"
                                 version "/ucd/Blocks.txt"))
             (file-name (string-append "unicode-blocks-" version ".txt"))
             (sha256
              (base32
               "05vzgrvfp35mgxjgkm4wnxjjgzva8n6545i9jxd4pczpvvfp122r")))))))
    (home-page "https://github.com/eugmes/fntsample")
    (synopsis "PDF and PostScript font samples generator")
    (description "This package provides a tool that can be used to make font
samples that show coverage of the font and are similar in appearance to
Unicode Charts.  It was developed for use with DejaVu Fonts project.")
    (license license:gpl3+)))

(define-public libraqm
  (package
    (name "libraqm")
    (version "0.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HOST-Oman/libraqm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qrdw67n14n0km2f8l0gk8scgj3ybz662s9x8jwj3rrj33nl2d1a"))))
    (build-system meson-build-system)
    (native-inputs
     (list gtk-doc/stable pkg-config python-wrapper))
    (inputs
     (list freetype fribidi harfbuzz))
    (home-page "https://github.com/HOST-Oman/libraqm")
    (synopsis "Library for complex text layout")
    (description
     "Raqm is a small library that encapsulates the logic for complex text
layout and provides a convenient API.

It currently provides bidirectional text support (using FriBiDi),
shaping (using HarfBuzz), and proper script itemization.  As a result, Raqm
can support most writing systems covered by Unicode.")
    (license license:expat)))

(define-public lcdf-typetools
  (package
    (name "lcdf-typetools")
    (version "2.108")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/kohler/lcdf-typetools")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0a6jqaqwq43ldjjjlnsh6mczs2la9363qav7v9fyrfzkfj8kw9ad"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       ;; This is only provided by the monolithic texlive distribution.
       ;; FIXME: texlive-kpathsea doesn't come with the library and headers
       (list "--without-kpathsea")))
    (native-inputs
     (list autoconf automake))
    (home-page "https://lcdf.org/type/")
    (synopsis "Multiple font manipulation tools")
    (description "LCDF Typetools comprises several programs for manipulating
PostScript Type 1, Type 1 Multiple Master, OpenType, and TrueType fonts.
These tools are cfftot1, mmafm, mmpfb, otfinfo, otftotfm, t1dotlessj, t1lint,
t1rawfm, t1reencode, t1testpage and ttftotype42.")
    (license license:gpl2+)))
