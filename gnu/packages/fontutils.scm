;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2020, 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Rene Saavedra <rennes@openmailbox.org>
;;; Copyright © 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2018, 2020–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018, 2019, 2020, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019, 2020, 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2020, 2021, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021-2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2022 Felipe Balbi <balbi@kernel.org>
;;; Copyright © 2023 gemmaro <gemmaro.dev@gmail.com>
;;; Copyright © 2023 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 pinoaffe <pinoaffe@gmail.com>
;;; Copyright © 2024 Sören Tempel <soeren@soeren-tempel.net>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Ashish SHUKLA <ashish.is@lostca.se>
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
  #:use-module (gnu packages bash)
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
  #:use-module (gnu packages mc)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages textutils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix utils)
  #:use-module (srfi srfi-1))

(define-public freetype
  (package
    (name "freetype")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/freetype/freetype-"
                           version ".tar.xz"))
       (sha256
        (base32 "0k32jaaz4pfhw34xwr6a38fncrpwr9fn5ij35m5w4dkn0jykmqjy"))))
    (build-system gnu-build-system)
    (arguments
     ;; The use of "freetype-config" is deprecated, but other packages still
     ;; depend on it.
     (list
      #:configure-flags #~(list "--enable-freetype-config")
      #:disallowed-references (list pkg-config)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'remove-reference-to-pkg-config
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* (search-input-file outputs "bin/freetype-config")
                (("/([a-zA-Z0-9/\\._-]+)/bin/([a-zA-Z0-9_-]+)?pkg-config"
                  _ store target)
                 "pkg-config")))))))
    (native-inputs (list pkg-config))
    ;; XXX: Not adding harfbuzz here, as it would introduce a dependency
    ;; cycle.
    (propagated-inputs (list libpng zlib))
    (synopsis "Font rendering library")
    (description
     "Freetype is a library that can be used by applications to access the
contents of font files.  It provides a uniform interface to access font files.
It supports both bitmap and scalable formats, including TrueType, OpenType,
Type1, CID, CFF, Windows FON/FNT, X11 PCF, and others.  It supports high-speed
anti-aliased glyph bitmap generation with 256 gray levels.")
    (license license:freetype)          ; some files have other licenses
    (home-page "https://freetype.org/")))

;; TODO: Make this change directly in freetype in the next large rebuild cycle
;; and remove this package.
(define-public freetype-with-brotli
  (package
    (inherit freetype)
    (name "freetype-with-brotli")
    (propagated-inputs
     (modify-inputs (package-propagated-inputs freetype)
       (prepend brotli)))))

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
    (version "3.9.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "afdko" version))
       (sha256
        (base32 "02c1rjx7ggbd1m9vqgsc2r28yiw66cjgvs5cq1a2fz0lkadbvrnb"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (with-directory-excursion "c/makeotf/lib/hotconv"
              ;; Delete ANTLR-generated code.
              (for-each delete-file
                        (find-files
                         "." "Feat(Parser|Lexer).*\\.(h|cpp|interp|tokens)$")))))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'use-c++17
            (lambda _
              ;; ANTLR4 4.10 and later require C++ 17.
              (substitute* "CMakeLists.txt"
                (("CMAKE_CXX_STANDARD 11")
                 "CMAKE_CXX_STANDARD 17"))))
          (add-after 'unpack 'patch-problematic-requirements
            (lambda _
              (substitute* "requirements.txt"
                ;; Remove lxml because the version requested here is different
                ;; than the one propagated by the python-fonttools package.
                (("^lxml==.*") "")
                (("<=4.38.0") ">=4.38.0"))))
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
              (substitute* '("c/makeotf/lib/hotconv/CMakeLists.txt"
                             "c/makeotf/lib/cffread/CMakeLists.txt")
                (("antlr4_static")
                 "antlr4-runtime"))))
          (add-after 'unpack 'regenerate-hotconv-grammar
            (lambda _
              (let ((antlr-version #$(package-version
                                      (this-package-native-input "antlr4"))))
                (with-directory-excursion "c/makeotf/lib/hotconv"
                  (substitute* "BuildGrammar.py"
                    (("antlr_version = .*")
                     (string-append "antlr_version = \""
                                    antlr-version
                                    "\"")))
                  (invoke "python" "BuildGrammar.py")))))
          ;; The test suite expects the commands to be Python rather than
          ;; shell scripts, so move the wrap phase after the tests.
          (delete 'wrap)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (invoke "pytest" "-vv" "--dist" "loadfile" "-n"
                        (number->string (parallel-job-count))
                        ;; This test fails because of a different date in the
                        ;; copyright header of an expected file since an
                        ;; update to ffmpeg.
                        "-k" "not test_alt_missing_glyph"))))
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
     (list antlr4
           openjdk                      ;required by antlr4
           ninja
           pkg-config
           python-pytest
           python-pytest-xdist
           python-scikit-build
           python-setuptools-scm
           python-wheel))
    (inputs
     (list bash-minimal
           java-antlr4-runtime-cpp
           libxml2
           `(,util-linux "lib")))
    (propagated-inputs
     (list psautohint
           python-booleanoperations
           python-defcon
           python-fontmath
           python-fonttools
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

(define-public python-afdko-3.6.1
  ;; This older version does not depend on Java and Antlr4.
  (package
    (inherit python-afdko)
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "afdko" version))
       (sha256
        (base32 "0187xhgw6spzaji93fs1mnhqnq30pxhdj1p2m88673szvzpf10av"))))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'set-CC
                 (lambda _
                   (setenv "CC" "gcc"))))))
    (native-inputs
     (list pkg-config
           python-pytest
           python-setuptools-scm
           python-wheel))
    (inputs
     (list bash-minimal
           libxml2))
    (propagated-inputs
     (list psautohint
           python-booleanoperations
           python-defcon
           python-fontmath
           python-fonttools
           python-lxml
           python-tqdm
           python-ufonormalizer
           python-ufoprocessor))))

(define-public python-beziers
  (package
    (name "python-beziers")
    (version "0.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/simoncozens/beziers.py")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dyr45m15sclbgaz1mrcnw8kny50h09gd45dlpfkgv9qpfxphkg3"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "pytest" "-vv")))))))
    (native-inputs (list python-pytest python-dotmap python-matplotlib))
    (propagated-inputs (list python-pyclipper))
    (home-page "https://simoncozens.github.io/beziers.py/index.html")
    (synopsis "Python bezier manipulation library")
    (description
     "Beziers provides a variety of classes for constructing,
manipulating and drawing Bezier curves and paths.  Principally designed for
font design software, it allows you to join, split, offset, and perform many
other operations on paths.")
    (license license:expat)))

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

    ;; Use version 3.6.1, which matches the bundled version and does not
    ;; depend on Java.
    (inputs (list python-afdko-3.6.1))

    (propagated-inputs (list python-fonttools-minimal))
    (home-page "https://github.com/adobe-type-tools/cffsubr")
    (synopsis "Compact Font Format (CFF) subroutinizer")
    (description "This package provides the @command{cffsubr} command, a
Compact Font Format (CFF) subroutinizer based on the Adobe Font Development
Kit for OpenType (AFDKO) @command{tx} tool.")
    (license license:asl2.0)))

(define-public python-compreffor
  (package
    (name "python-compreffor")
    (version "0.5.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "compreffor" version))
       (sha256
        (base32 "05gpszc8xh6wn3mdra05d6yz6ns624y67m9xs4vv8gh68m0aasrh"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-setup.py
            (lambda _
              (substitute* "setup.py"
                ;; Not actually needed.
                ((", \"setuptools_git_ls_files\"") "")))))))
    (native-inputs (list python-pytest python-pytest-runner
                         python-setuptools-scm))
    (propagated-inputs (list python-fonttools-minimal))
    (home-page "https://github.com/googlefonts/compreffor")
    (synopsis "@acronym{CFF, Compact Font Format} subroutinizer for fontTools")
    (description "This package provides a @acronym{CFF, Compact Font Format}
subroutinizer for fontTools.")
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
    (build-system pyproject-build-system)
    (arguments
     ;; XXX: Try to remove it when updating python-fonttools.
     (list #:test-flags #~(list "-k" "not test_ignore_single_points")))
    (propagated-inputs (list python-fonttools))
    (native-inputs
     (list python-cython
           python-defcon
           python-pytest
           python-pytest-runner
           python-setuptools
           python-setuptools-scm
           python-wheel
           unzip))
    (home-page "https://github.com/googlefonts/cu2qu")
    (synopsis "Cubic-to-quadratic bezier curve conversion")
    (description "This library provides functions which take in @acronym{UFO,
Unified Font Object} objects (such as Defcon Fonts or Robofab RFonts) and
converts any cubic curves to quadratic.  The most useful function is probably
@code{fonts_to_quadratic}.")
    (license license:asl2.0)))

(define-public python-ufo2ft
  (package
    (name "python-ufo2ft")
    (version "2.31.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ufo2ft" version))
       (sha256
        (base32 "1rg2997af8blvswlwif0kpz2vxrlh555gzqslz6yv9y7i7v8lphl"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-booleanoperations
           python-cffsubr
           python-compreffor
           python-cu2qu
           python-defcon
           python-fonttools
           python-skia-pathops
           python-ufolib2))
    (home-page "https://github.com/googlefonts/ufo2ft")
    (synopsis "Generate OpenType fonts from Unified Font Objects (UFOs)")
    (description "@code{ufo2ft} (UFO to FontTools) is a fork of @code{ufo2fdk}
intended to leverage FontTools (a Python library) rather than the Adobe Font
Development Kit for OpenType (AFDKO), a set of C libraries/utilities so that
it can be more easily extended.  Like @code{ufo2fdk}, its primary purpose is
to generate OpenType font binaries from Unified Font Objects (UFOs).")
    (license license:expat)))

(define-public python-fontmath
  (package
    (name "python-fontmath")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fontMath" version ".zip"))
       (sha256
        (base32 "070v1jz5f18g15if459ppwswq4w5hzffwp1gvdc5j47bgz5qflva"))))
    (build-system python-build-system)
    (propagated-inputs (list python-fonttools-minimal))
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
    (propagated-inputs (list python-fonttools-minimal))
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
         #t)
        ((#:phases phases #~%standard-phases)
         #~(modify-phases #$phases
             (add-after 'unpack 'drop-flaky-docstring
               ;; XXX This assertion fails on certain (Intel?) machines, but not
               ;; others (AMD?), so we can't patch in a ‘correct’ value.  Just
               ;; drop it until the proper fix lands upstream.  Reported there
               ;; as <https://github.com/robotools/fontPens/issues/41>.
               (lambda _
                 (substitute* "Lib/fontPens/penTools.py"
                   ((".*\\(\\(0, 0), \\(50, 20), \\(100, 40)).*") "")
                   ((".*107\\.70329614269009.*") ""))))))))
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
     (version "0.11.0")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "fontParts" version ".zip"))
        (sha256
         (base32 "0j4h8hszky639gmfy1avmw670y80ya49kca8yc635h5ihl0c3v8x"))))
     (build-system python-build-system)
     (propagated-inputs
      (list python-booleanoperations
            python-defcon-bootstrap
            python-fontmath
            python-fonttools-minimal))
     (native-inputs (list python-setuptools-scm unzip))
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

(define-public python-glyphslib
  (package
    (name "python-glyphslib")
    (version "6.0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "glyphsLib" version))
              (sha256
               (base32
                "0mkkwd09g76hvif603ij5aqicxh47zvhgyyd0pjcjmpdy6dr70yw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~'(;; These fail because the test data has not yet been
                       ;; updated for newer FontTools:
                       ;;   https://github.com/googlefonts/glyphsLib/issues/787
                       ;; Re-enable for versions > 6.0.7.
                       "--ignore=tests/builder/designspace_gen_test.py"
                       "--ignore=tests/builder/interpolation_test.py")))
    (native-inputs
     (list python-setuptools-scm
           python-setuptools
           python-wheel
           ;; For tests.
           python-pytest
           python-xmldiff))
    (propagated-inputs
     (list python-defcon
           python-fonttools
           python-openstep-plist
           python-ufolib2
           python-ufo2ft
           python-ufonormalizer))
    (home-page "https://github.com/googlefonts/glyphsLib")
    (synopsis "Bridge Glyphs source files to UFOs")
    (description
     "This package provides a bridge from Glyphs source files (@file{.glyphs})
to UFOs and DesignSpace files via @code{defcon} and @code{designspaceLib}.")
    (license license:asl2.0)))

(define-public python-glyphsets
 (package
  (name "python-glyphsets")
  (version "0.5.2")
  (source (origin
            (method url-fetch)
            (uri (pypi-uri "glyphsets" version))
            (sha256
             (base32
              "1dc24i0hkd85gkkg3bqjhagjyw3xsqxazd86yh2l60c1wr5n9y6g"))))
  (build-system python-build-system)
  (arguments
   (list #:phases
         #~(modify-phases %standard-phases
             (add-after 'unpack 'loosen-version-constraints
               (lambda _
                 (substitute* "setup.py"
                   (("setuptools_scm>=4,<6\\.1")
                    "setuptools_scm>=4"))))
             (replace 'check
               (lambda* (#:key tests? #:allow-other-keys)
                 (when tests?
                   (invoke "pytest" "-vv" "tests/testglyphdata.py")
                   (invoke "pytest" "-vv" "tests/testusage.py")))))))
  (native-inputs (list python-pytest python-setuptools-scm))
  (propagated-inputs
   (list python-defcon python-fonttools-minimal python-glyphslib))
  (home-page "https://github.com/googlefonts/glyphsets/")
  (synopsis "Evaluate coverage of glyph sets")
  (description
   "This package provides an API with data about glyph sets for many
different scripts and languages.")
  (license license:asl2.0)))

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
    (propagated-inputs (list python-defcon python-fontmath
                             python-fonttools-minimal))
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
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              (string-join
               '(;; The CJKSparseVar.subset.hinted.otf test fails with slightly
                 ;; different output caused by the newer fonttools version used
                 ;; in Guix.
                 "not CJKSparseVar.subset.hinted.otf"
                 ;; These tests fails underministically, See also:
                 ;; https://github.com/adobe-type-tools/afdko/issues/1678
                 "not test_hashmap_no_version"
                 "not test_hashmap_old_version")
               " and "))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-font-data
            ;; The data is copied as it needs to be writable for the tests.
            (lambda _
              (copy-recursively
               #$(this-package-native-input "psautohint-font-data")
               "tests/integration/data")
              (for-each make-file-writable
                        (find-files "tests/integration/data")))))))
    (inputs (list python-fonttools))
    (native-inputs
     (list psautohint-font-data
           python-pytest
           python-pytest-cov
           python-pytest-xdist
           python-setuptools-scm
           python-setuptools
           python-wheel))
    (home-page "https://github.com/adobe-type-tools/psautohint")
    (synopsis "Adobe's PostScript autohinter")
    (description "This package provides the @command{autohinter} command that
can be used to hint PostScript fonts.  A Python wrapper is also included.")
    (license license:asl2.0)))

(define-public python-sfdlib
  (package
    (name "python-sfdlib")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aliftype/sfdLib")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q61km32i1h3cmn8nazcgsbzpm8q2nxp3kq3glqgfgvlxr1s3brm"))))
    (build-system python-build-system)
    (propagated-inputs (list python-ufolib2))
    (home-page "https://github.com/aliftype/sfdLib")
    (synopsis "Simple SFD to UFO converter")
    (description "This package provides the @command{sfd2ufo} command, a
converter from FontForge’s @acronym{SFD, Spline Font Database} fonts to
@acronym{UFO, Unified Font Object} fonts.")
    (license license:bsd-3)))

(define-public python-skia-pathops
  (package
    (name "python-skia-pathops")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skia-pathops" version ".zip"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "src/cpp")) ;140+ MiB of stuff
       (sha256
        (base32 "1vlwl1w6sn8c78fsh1w549n3lk9v3v9hcp866vrsdr4byb7g2ani"))))
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
           python-fonttools-minimal
           python-mutatormath))
    (native-inputs (list python-setuptools-scm unzip))
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

(define-public fontobene-qt
  (package
    (name "fontobene-qt")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fontobene/fontobene-qt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "098ys33l563hjyzm6azzw8kmlybja374vacakczwhh2k3ifn37r9"))))
    (inputs (list qtbase))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "./tests/fontobene-qt-tests")))))))
    (home-page "https://github.com/fontobene/fontobene-qt5")
    (synopsis "Parser for FontoBene stroke fonts")
    (description "FontoBene-Qt is a header-only library to parse FontoBene
stroke fonts with C++11/Qt.")
    ;; Dual-licensed, either license applies.
    (license (list license:asl2.0 license:expat))))

(define-public fontobene-qt5
  (deprecated-package "fontobene-qt5" fontobene-qt))

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
    (home-page "https://ttf2pt1.sourceforge.net/")
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
     (version "2.14.0")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "https://www.freedesktop.org/software/"
                     "fontconfig/release/fontconfig-" version ".tar.xz"))
               (sha256 (base32
                        "1b4v1r94ri44p4a3kbwd38ig5jgdgcfgwdfm6fqzvfvlki6bignw"))
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
    (list python python-fonttools-minimal))
   (inputs
    (list freetype))
   (arguments
    (if (system-hurd?)
        (list
         #:phases
         #~(modify-phases %standard-phases
             (replace 'check
               ;; cmake-build-system ignores #:make-flags for make check
               (lambda* (#:key test-target tests? parallel-tests?
                         #:allow-other-keys)
                 (if tests?
                     (let ((jobs (if parallel-tests?
                                     (number->string (parallel-job-count))
                                     "1")))
                       (invoke "make"
                               (string-append
                                "ARGS=-j " jobs " --exclude-regex ^awamicmp3$")
                               test-target))
                     (format #t "test suite not run~%"))))))
        '()))
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
    (home-page "https://potrace.sourceforge.net/")))

(define-public psftools
  (package
    (name "psftools")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.seasip.info/Unix/PSF/"
                           "psftools-" version ".tar.gz"))
       (sha256
        (base32 "1lv6kvrcbspyh7a0hlji84wbmw6xh87r3iaafq3khp88kgh1irri"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--disable-static")))
    (home-page "https://www.seasip.info/Unix/PSF/")
    (synopsis
     "Convert PSF fixed-width bitmap (console) fonts from/to other formats")
    (description
     "@acronym{PSF, PC Screen Font} is the simple monospaced bitmap font format
used by the Linux kernel for console fonts.  The PSF Tools convert between PSF
and many other font formats, similar to what the NetPBM package does for images.

It includes converters for a good number of common bitmap font formats such as
@file{.BDF}, @file{.FNT}, and @file{.FON} files, Berkeley vfonts, classic
Amstrad/Sinclair/Hercules/BBC Micro soft fonts, and raw (DOS-style) fonts.

It also supports less traditional formats such as PBM/XBM images, plain text
(for rudimentary editing), and C header files.")
    (license license:gpl2+)))

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
    (home-page "https://libspiro.sourceforge.net/")))

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
    (version "20220308")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/fontforge/fontforge/releases/download/"
                    version "/fontforge-" version ".tar.xz"))
              (sha256
               (base32 "0ncfc4ajwy4ng6b6h79w52jh9z3lngvf3f3ldi1wzkhcg9zh3r01"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list cairo
           bash-minimal
           fontconfig                   ;dlopen'd
           freetype
           gettext-minimal
           libice
           libsm
           libx11
           libxi
           libjpeg-turbo
           libltdl
           libpng
           libspiro
           libtiff
           libungif
           libxft
           libxml2
           pango
           potrace
           python
           zlib))
    (arguments
     (list
      #:configure-flags #~'( ;; TODO: Provide GTK+ for the Wayland-friendly GDK
                            ;; backend, instead of the legacy X11 backend.
                            ;; Currently it introduces a circular dependency.
                            "-DENABLE_X11=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'do-not-override-RPATH
            (lambda _
              ;; Do not attempt to set a default RPATH, as our ld-wrapper
              ;; already does the right thing.
              (substitute* "CMakeLists.txt"
                (("^set_default_rpath\\(\\)")
                 ""))))
          #$@(if (target-hurd?)
                 #~((add-after 'unpack 'apply-hurd-patch
                      (lambda _
                        (let ((patch-file
                               #$(local-file
                                  (search-patch "fontforge-hurd.patch"))))
                          (invoke "patch" "--force" "-p1" "-i" patch-file)))))
                 #~())
          #$@(if (system-hurd?)
                 #~((replace 'check
                      ;; cmake-build-system ignores #:make-flags for make check
                      (lambda* (#:key test-target tests? parallel-tests?
                                #:allow-other-keys)
                        (let ((skip '("test0001_py" "test0001_pyhook")))
                          (if tests?
                              (let ((jobs
                                     (if parallel-tests?
                                         (number->string (parallel-job-count))
                                         "1")))
                                (invoke "make"
                                        (string-append "ARGS=-j " jobs
                                                       " --exclude-regex ^"
                                                       (string-join skip "\\|")
                                                       "$")
                                        test-target))
                              (format #t "test suite not run~%"))))))
                 #~())
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
                          '("libtiff" "libjpeg-turbo" "libpng" "libungif"
                            "libxml2" "zlib" "libspiro" "freetype"
                            "pango" "cairo" "fontconfig-minimal")))
                  ;; Checks for potrace program at runtime
                  `("PATH" ":" prefix (,potrace)))))))))
    (synopsis "Outline font editor")
    (description
     "FontForge allows you to create and modify postscript, truetype and
opentype fonts.  You can save fonts in many different outline formats, and
generate bitmaps.")
    (license license:gpl3+)
    (home-page "https://fontforge.github.io")))

(define-public python-statmake
  (package
    (name "python-statmake")
    (version "0.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/daltonmaag/statmake")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1k6fkzyhsfkgi599sb017wzf4jzbnp5wjg1kla1b33vgjpa7n5nw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; The code no longer raises <class 'ValueError'>
      '(list "-k" "not test_load_stylespace_broken_range")))
    (native-inputs
     (list python-poetry-core
           python-pytest
           python-ufo2ft))
    (propagated-inputs
     (list python-attrs
           python-cattrs
           python-fonttools))
    (home-page "https://github.com/daltonmaag/statmake")
    (synopsis "Apply OpenType STAT information to a variable font")
    (description
     "@command{statmake} takes a user-written Stylespace that defines
@url{https://docs.microsoft.com/en-us/typography/opentype/spec/stat, OpenType
STAT information} for an entire font family and then (potentially subsets and)
applies it to a specific variable font.  This spares users from having to deal
with @url{https://github.com/fonttools/fonttools/, raw TTX dumps} and juggling
with @samp{nameIDs}.")
    (license license:expat)))

(define-public python-ufolib2
  (package
    (name "python-ufolib2")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ufoLib2" version))
       (sha256
        (base32 "0yx4i8q5rfyqhr2fj70a7z1bp1jv7bdlr64ww9z4nv9ycbda4x9j"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-attrs
           python-fonttools))
    (home-page "https://github.com/fonttools/ufoLib2")
    (synopsis "Unified Font Object (UFO) font processing library")
    (description "The ufoLib2 Python library is meant to be a thin
representation of the Unified Font Object (UFO) version 3 data model, intended
for programmatic manipulation and fast batch processing of UFOs.  It resembles
the defcon library, but does without notifications, the layout engine and
other support classes.  Where useful and possible, ufoLib2 tries to be
API-compatible with defcon.")
    (license license:asl2.0)))

;;; A variant used to break a cycle between python-fontpens and
;;; python-fontparts.
(define-public python-defcon-bootstrap
  (package
    (name "python-defcon-bootstrap")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "defcon" version ".zip"))
       (sha256
        (base32 "036clkwjfv5mmvy6s67s0pbni73n9hdw32z20gm4w5jzqzbjdpjn"))))
    (build-system python-build-system)
    (propagated-inputs (list python-fontpens-bootstrap python-fonttools))
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

(define-public nototools
  (package
    (name "nototools")
    (version "0.2.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/googlefonts/nototools")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "14rrdamkmhrykff8ln07fq9cm8zwj3k113lzwjcy0lgz23g51jyl"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'pretend-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (with-directory-excursion "tests"
                (invoke "./run_tests")))))))
    (native-inputs (list python-setuptools-scm))
    (propagated-inputs (list python-afdko))
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
    (version "3.3.1")
    (home-page "https://codeberg.org/dnkl/fcft")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08fr6zcqk4qp1k3r0di6v60qfyd3q5k9jnxzlsx2p1lh0nils0xa"))))
    (build-system meson-build-system)
    (native-inputs
     (list check pkg-config scdoc))
    (propagated-inputs
     (list ;; Required by fcft.pc.
           utf8proc
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
    (version "0.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FontManager/font-manager")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pxdwpjzsmld4j2m4q423vdrkx23bb6jqszjgk5wqbr2ln772hcx"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t
           #:build-type "release"
           #:configure-flags
           #~(list (string-append "-Dc_link_args=-Wl,-rpath=" #$output
                                  "/lib/font-manager"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'skip-gtk-update-icon-cache
                 (lambda _
                   ;; Remove dependency on needless desktop cache stuff.
                   (substitute* "meson.build"
                     (("gtk_update_icon_cache: true")
                      "gtk_update_icon_cache: false")
                     (("update_desktop_database: true")
                      "update_desktop_database: false")))))))
    (native-inputs
     (list desktop-file-utils
           gettext-minimal
           `(,glib "bin")
           gobject-introspection
           pkg-config
           python-wrapper
           vala
           yelp-tools))
    (inputs
     (list fontconfig
           freetype
           gsettings-desktop-schemas
           gtk
           json-glib
           libsoup
           sqlite
           webkitgtk))
    (home-page "https://fontmanager.github.io/")
    (synopsis "Simple font management for GTK desktop environments")
    (description "Font Manager is intended to provide a way for users to
easily manage desktop fonts, without having to resort to command-line
tools or editing configuration files by hand.
While designed primarily with the GNOME Desktop Environment in mind, it should
work well with other GTK desktop environments.")
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
       ("bash-minimal", bash-minimal)
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
    (version "0.10.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HOST-Oman/libraqm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bzdrvacgj9629r4mgmag9sm5ay5914fbs8pnxf8xphvrbnbxm8z"))))
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


(define-public fontopia
  (package
    (name "fontopia")
    (version "2.0")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/fontopia/fontopia-"
                                 version ".tar.gz"))
             (sha256
              (base32 "0wv7bd7gdm1ma4xgq9av73ic3xhpwyszj6g6c6311xjk26xm9ahd"))))
    (build-system gnu-build-system)
    (inputs
     (list gnudos))
    (home-page "https://www.gnu.org/software/fontopia/")
    (synopsis "Text-based, console font editor")
    (description
     "GNU fontopia is an easy-to-use, text-based, console font editor.  You can
edit the fonts that your GNU/Linux kernel is using to display your text on text-
based (vs graphical) terminals.")
    (license license:gpl3+)))


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

(define-public bdf2sfd
  (package
    (name "bdf2sfd")
    (version "1.1.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fcambus/bdf2sfd")
                    (commit "1.1.8")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pa92gjiijp9xqnw9dcvz24s6qk11a4lp5q6s5psd6mpkhpd88zq"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/fcambus/bdf2sfd")
    (synopsis "BDF to SFD converter, allowing to vectorize bitmap fonts")
    (description
     "bdf2sfd is a
@uref{https://en.wikipedia.org/wiki/Glyph_Bitmap_Distribution_Format,
BDF} to @uref{https://fontforge.org/docs/techref/sfdformat.html, SFD}
converter, allowing to vectorize bitmap fonts.  It works by converting
each pixel of a glyph to a polygon, which produces large and
unoptimized SFD files that should be post-processed using
@uref{https://fontforge.org, FontForge}.")
    (license license:bsd-2)))
