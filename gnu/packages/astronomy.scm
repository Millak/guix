;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 by Amar Singh <nly@disroot.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2023-2024 Iliya Tikhonenko <tikhonenko@mpe.mpg.de>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2023, 2025 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2024-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Andy Tai <lichengtai@gmail.com>
;;; Copyright © 2024-2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 Vasilii Smirnov <vasilii.smirnov@mailbox.org>
;;; Copyright © 2025 Daniel Ziltener <dziltener@lyrion.ch>
;;; Copyright © 2025 Hugo Buddelmeijer <hugo@buddelmeijer.nl>
;;; Copyright © 2025 Brendan Tildesley <mail@brendan.scot>
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

(define-module (gnu packages astronomy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix build-system python) #:select (pypi-uri)) ;to be removed soon
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fortran-xyz)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages markup)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-graphics)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages specifications)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1))

(define-public alfa
  (package
    (name "alfa")
    (version "2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rwesson/ALFA")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "1wi7pjra49qyyz8qz2qcgjcjck16ld0zczavxqn2xksdx1p99ajs"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:parallel-build? #f
           #:make-flags #~(list (string-append "PREFIX="
                                               #$output)
                                (string-append "VERSION="
                                               #$version))
           #:tests? (not (or (%current-target-system)
                             ;; The test suite consumes all disk space
                             (target-riscv64?)))
           #:test-target "fittest"
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (delete 'check)
                        (add-after 'install 'post-install-check
                          (assoc-ref %standard-phases 'check)))))
    (inputs (list cfitsio gfortran))
    (home-page "https://nebulousresearch.org/codes/alfa/")
    (synopsis "Automated line fitting algorithm")
    (description
     "This package provides @acronym{ALFA, Automatic line fitting algorithm},
which can identify and fit hundreds of lines in emission line spectra in just a
few seconds with following features:
@itemize

@item A population of synthetic spectra is generated using a reference line
catalogue.

@item The goodness of fit for each synthetic spectrum is calculated.  The best
sets of parameters are retained and the rest discarded.

@item A new population of synthetic spectra is obtained by averaging pairs of
the best performers.

@item A small fraction of the parameters of the lines in the new generation are
randomly altered.

@item The process repeats until a good fit is obtained.
@end itemize")
    (license license:gpl3)))

(define-public aocommon
  (let ((commit "1444d66a59e757e7a0c74447e9f8d7a69c5e102d")
        (revision "3"))
    (package
      (name "aocommon")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://gitlab.com/aroffringa/aocommon")
                (commit commit)))
         (sha256
          (base32 "06kdkkarx4bmp9808bwlx35vnp6g0vfzaxx1ijvzwgsjgg8yi5cp"))
         (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             #~'(("include/aocommon" "include/aocommon")
                 ("CMake" "CMake"))))
      (home-page "https://gitlab.com/aroffringa/aocommon")
      (synopsis "Collection of functionality that is reused in astronomical applications")
      (description
       "This package provides source-only AOCommon collection of functionality that is
reused in several astronomical applications, such as @code{wsclean},
@code{aoflagger}, @code{DP3} and @code{everybeam}.")
      (license license:gpl3+))))

(define-public aoflagger
  ;; 3.4.0 was released in 2023, there are a lot of changes and compatibility
  ;; for EveryBeam.
  (let ((commit "b3a459df54b35ec18821ae0a392eeef1ca92cdba")
        (revision "0"))
    (package
      (name "aoflagger")
      (version (git-version "3.4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://gitlab.com/aroffringa/aoflagger")
                (commit commit)))
         (sha256
          (base32 "0fgm2svdw52m348hi28pnknxsdy54dkfd7y388b14hwf9z5ransa"))
         (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; Tests require external files download from
        ;; https://www.astron.nl/citt/ci_data/aoflagger/
        #:tests? #f
        #:configure-flags
        #~(list (string-append "-DCASACORE_ROOT_DIR="
                               #$(this-package-input "casacore")))
        #:phases
        #~(modify-phases %standard-phases
            ;; aocommon and pybind11 are expected to be found as git submodules,
            ;; link them before build.
            (add-after 'unpack 'link-submodule-package
              (lambda _
                (rmdir "external/aocommon")
                (symlink #$(this-package-native-input "aocommon")
                         (string-append (getcwd) "/external/aocommon")))))))
      (native-inputs
       (list aocommon
             boost-1.83
             pkg-config
             python
             pybind11))
      (inputs
       (list casacore
             cfitsio
             fftw
             gsl
             gtkmm-3
             hdf5
             libpng
             libsigc++
             libxml2
             lua
             openblas
             zlib))
      (home-page "https://gitlab.com/aroffringa/aoflagger")
      (synopsis "Astronomical tool that can find and remove radio-frequency interference")
      (description
       "AOFlagger is a tool that can find and remove radio-frequency
interference (RFI) in radio astronomical observations.  It can make use of Lua
scripts to make flagging strategies flexible, and the tools are applicable to a
wide set of telescopes.")
      (license license:gpl3+))))

(define-public astroterm
  (package
    (name "astroterm")
    (version "1.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/da-luce/astroterm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03vrprrahhfp7jbl1apmpxmv05fb8lw469fnsnq7sajdhc3waifx"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'copy-bsc5
            (lambda _
              (let* ((ybsc-input #+(this-package-native-input "specification-ybsc"))
                     (ybsc-file (string-append ybsc-input "/share/ybsc5")))
                (copy-file ybsc-file "./data/ybsc5"))))
          (add-before 'configure 'fix-embed-sh-ref
            (lambda _
              (substitute* "meson.build"
                (("'../scripts/embed.sh'")
                 "meson.source_root() / 'scripts/embed.sh'")))))))
    (native-inputs
     (list pkg-config python-wrapper specification-ybsc xxd))
    (inputs
     (list ncurses argtable3))
    (home-page "https://github.com/da-luce/astroterm")
    (synopsis "Planetarium for your terminal")
    (description
     "@code{astroterm} is a terminal-based star map written in C.  It displays
the real-time positions of stars, planets, constellations, and more, all
within your terminal - no telescope required!")
    (license license:expat)))

(define-public calceph
  (package
    (name "calceph")
    (version  "4.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.obspm.fr/imcce_calceph/calceph")
             (commit (string-append name "_"
                                    (string-replace-substring version "." "_")))))
       (sha256
        (base32 "0a01hglcafgvvi3gfkfh7mz417j4vv9sikgac7rzs1idabff30ap"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://www.imcce.fr/inpop/calceph")
    (synopsis "Astronomical library to access the binary planetary ephemeris files")
    (description
     "The CALCEPH Library is designed to access the binary planetary ephemeris files,
such INPOPxx and JPL DExxx ephemeris files, (called @code{original JPL binary} or
@code{INPOP 2.0 or 3.0 binary} ephemeris files in the next sections) and the SPICE
kernel files (called @code{SPICE} ephemeris files in the next sections).  At the
moment, supported SPICE files are:

@itemize
@item text Planetary Constants Kernel (KPL/PCK) files;
@item binary PCK (DAF/PCK) files;
@item binary SPK (DAF/SPK) files containing segments of type 1, 2, 3, 5, 8, 9,
12, 13, 17, 18, 19, 20, 21, 102, 103 and 120;
@item meta kernel (KPL/MK) files;
@item frame kernel (KPL/FK) files (only basic support).
@end itemize\n")
    (license license:cecill)))

(define-public calcmysky
  (package
    (name "calcmysky")
    (version "0.3.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/10110111/CalcMySky")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03h93jxpxfa7gy4z3pvh8xy60z2f5n5id9k2wkzmf81zrvakbvgv"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DQT_VERSION=6"
                   "-DCMAKE_CXX_FLAGS=-fPIC")))
    (inputs
     (list eigen glm qtbase))
    (home-page "https://10110111.github.io/CalcMySky/")
    (synopsis "Simulator of light scattering by planetary atmospheres")
    (description
     "CalcMySky is a software package that simulates scattering of light by the
atmosphere to render daytime and twilight skies (without stars).  Its primary
purpose is to enable realistic view of the sky in applications such as
planetaria.  Secondary objective is to make it possible to explore atmospheric
effects such as glories, fogbows etc., as well as simulate unusual environments
such as on Mars or an exoplanet orbiting a star with a non-solar spectrum of
radiation.

This package consists of three parts:

@itemize
@item @code{calcmysky} utility that does the precomputation of the atmosphere
model to enable rendering.

@item @code{libShowMySky} library that lets the applications render the
atmosphere model.

@item @code{ShowMySky} preview GUI that makes it possible to preview the
rendering of the atmosphere model and examine its properties.
@end itemize")
    (license license:gpl3+)))

(define-public calcmysky-qt5
  (package/inherit calcmysky
    (name "calcmysky-qt5")
    (arguments
     (list #:configure-flags
           #~(list "-DQT_VERSION=5"
                   "-DCMAKE_CXX_FLAGS=-fPIC")))
    (inputs
     (modify-inputs (package-inputs calcmysky)
       (replace "qtbase" qtbase-5)))
    (synopsis "Qt5 build for the CalcMySky library")))

(define-public casacore
  (package
    (name "casacore")
    (version "3.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/casacore/casacore")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13n39zchslgkh76f2hil68a0xa9p2c2375z5bqynj75fq0f6xv1l"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Note: There are multiple failures in
      ;; tests which require additional measures data. They are
      ;; distributed via FTP without any license:
      ;; ftp://ftp.astron.nl/outgoing/Measures/
      ;; TODO: Check how to fix tests.
      #:tests? #f
      #:parallel-build? #t
      #:configure-flags
      #~(list "-DBUILD_PYTHON3=ON"
              "-DBUILD_PYTHON=OFF"
              "-DUSE_HDF5=ON"
              "-DUSE_OPENMP=OFF"
              "-DUSE_THREADS=ON"
              (string-append "-DDATA_DIR=" #$output "/data")
              (string-append "-DPYTHON3_EXECUTABLE="
                             #$(this-package-input "python") "/bin")
              (string-append "-DPYTHON3_INCLUDE_DIR="
                             #$(this-package-input "python") "/include")
              (string-append "-DPYTHON3_LIBRARY="
                             #$(this-package-input "python") "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-env
            (lambda _
              (setenv "HOME" "/tmp")))
          (add-after 'unpack 'use-absolute-rm
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "casa/OS/test/tFile.run"
                (("/bin/rm")
                 (search-input-file inputs "/bin/rm")))))
          (add-after 'unpack 'use-absolute-python3
            (lambda _
              (substitute* "build-tools/casacore_floatcheck"
                (("#!/usr/bin/env python")
                 (string-append "#!" (which "python3"))))))
          ;; NOTE: (Sharlatan-20220611T200837+0100): Workaround for casacore
          ;; tests stuck with missing "qsub" issue.
          ;; https://github.com/casacore/casacore/issues/1122
          (add-after 'unpack 'patch-pre-test-checks
            (lambda _
              (substitute* "build-tools/casacore_assay"
                (("QSUBP=.*$") "QSUBP=\n")
                (("YODP=.*$") "YODP=\n"))))
          ;; XXX: It fails to find the stdlib types when the gfortran header
          ;; is used.  Remove gfortran from CPLUS_INCLUDE_PATH as a
          ;; workaround.  Taken from <https://issues.guix.gnu.org/73439#45>.
          (add-after 'set-paths 'hide-gfortran
            (lambda _
              (let ((gfortran #$(this-package-input "gfortran")))
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join
                         (delete (string-append gfortran "/include/c++")
                                 (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                         ":"))))))))
    (native-inputs
     (list bison
           boost
           flex
           readline))
    (inputs
     (list cfitsio
           fftw
           fftwf
           gfortran
           gsl
           hdf5
           ncurses
           openblas
           python
           python-numpy
           wcslib))
    (home-page "https://casacore.github.io/casacore/")
    (synopsis "Suite of C++ libraries for radio astronomy data processing")
    (description
     "The casacore package contains the core libraries of the old
AIPS++/CASA (Common Astronomy Software Application) package.  This split was
made to get a better separation of core libraries and applications.
@url{https://casa.nrao.edu/, CASA} is now built on top of Casacore.")
    (license license:gpl2+)))

(define-public ccfits
  (package
    (name "ccfits")
    (version "2.7")
    ;; Updater struggles to determine the version, check for the latest one on
    ;; <https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/ccfits/> or on the
    ;; home page.
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/ccfits/v"
             version "/CCfits-" version ".tar.gz"))
       (sha256
        (base32 "06mhvvdsaqvk3cc309gv6zd4lcxm5q5aialaq0n77gzczv94cdgn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS=ON"
              "-DTESTS=ON")))
    (inputs (list cfitsio zlib))
    (home-page "https://heasarc.gsfc.nasa.gov/docs/software/fitsio/ccfits/")
    (synopsis "C++ interface to the CFITSIO")
    (description
     "CCfits is an object oriented interface to the cfitsio library.  It is
designed to make the capabilities of cfitsio available to programmers working
in C++.  It is written in ANSI C++ and implemented using the C++ Standard
Library with namespaces, exception handling, and member template functions.")
    (license (license:non-copyleft "file://License.txt"
                                   "See License.txt in the distribution."))))

(define-public celestia
  ;; 1.6.4 was placed in 2023 while master migrated to Qt6, use the lates
  ;; commit for now.
  (let ((commit "d3f4040401f5f71bcca79e55d53be75c05b867ef")
        (revision "0"))
    (package
      (name "celestia")
      (version (git-version "1.6.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/celestiaproject/celestia")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0rqkzxyf8gfjprhj1c19d7chhc3b94wlq2119wz0c344rx7hnh9l"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f     ;no tests were found
        #:configure-flags
        #~(list "-DENABLE_FFMPEG=ON"
                "-DENABLE_GLES=ON"
                "-DENABLE_LIBAVIF=ON"
                "-DENABLE_QT6=ON"
                "-DENABLE_TOOLS=ON"
                "-DUSE_QT6=ON")
        #:phases
        #~(modify-phases %standard-phases
            ;; TODO: Wrap celestia-content instead of copping it, if posible.
            (add-after 'install 'install-content
              (lambda _
                (copy-recursively
                 (string-append #$(this-package-native-input "celestia-content")
                                "/share")
                 (string-append #$output "/share")))))))
      (native-inputs
       (list boost
             celestia-content
             gettext-minimal
             pkg-config))
      (inputs
       (list eigen
             ffmpeg
             fmt
             freetype
             glu
             gperf
             libavif
             libepoxy
             libjpeg-turbo
             libpng
             mesa
             qtbase
             qtwayland))
      (propagated-inputs
       (list lua
             perl))
      (home-page "https://celestia.space/")
      (synopsis "Real-time 3D visualization of space")
      (description
       "This simulation program lets you explore our universe in three
dimensions.  Celestia simulates many different types of celestial objects.
From planets and moons to star clusters and galaxies, you can visit every
object in the expandable database and view it from any point in space and
time.  The position and movement of solar system objects is calculated
accurately in real time at any rate desired.")
      (license license:gpl2+))))

(define-public celestia-content
  ;; No rleases or version tags.
  (let ((commit "b6d3daa2260cdbd251336264eb62334a4d6e62c6")
        (revision "3"))
    (package
      (name "celestia-content")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/CelestiaProject/CelestiaContent")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "11xgsl4bln1lid6cb0a2s4d5h7zb0y8c9dm61vpss8f5dlvdp8n5"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f))   ;no tests were found
      (native-inputs
       (list gettext-minimal
             imagemagick))
      (home-page "https://celestia.space/")
      (synopsis "Data files for Celestia space simulator")
      (description
       "This package provides data content for Celestia.
@itemize
@item Scientific Data Base
@item Texture maps
@item 3D Models
@end itemize")
      (license license:gpl2+))))

(define-deprecated-package celestia-gtk
  celestia)

(define-public cfitsio
  (package
    (name "cfitsio")
    (version "4.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/"
             "cfitsio-" version ".tar.gz"))
       (sha256
        (base32 "001nh63i3fadjk42yadr48kkd5n5qcxzy3qcrkzsbpag4zzlzm7s"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-reentrant"
              (string-append "--with-bzip2=" #$(this-package-input "bzip2")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda _
              (substitute* "Makefile.in" (("/bin/") ""))))
          (delete 'check)
          ;; TODO: Testing steps are sourced from docs/fitsio.pdf, implement
          ;; the logic in Guile in the future.
          (add-after 'install 'post-install-check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "testprog")
                (with-output-to-file "testprog.lis" (lambda _(invoke "./testprog")))
                (invoke "diff" "-r" "testprog.lis" "testprog.out")
                (invoke "cmp" "-l" "testprog.fit" "testprog.std")))))))
    (native-inputs (list gfortran))
    (inputs (list bzip2 curl zlib))
    (home-page "https://heasarc.gsfc.nasa.gov/fitsio/fitsio.html")
    (synopsis "Library for reading and writing FITS files")
    (description
     "CFITSIO provides simple high-level routines for reading and writing
@acronym{Flexible Image Transport System,FITS} files that insulate the
programmer from the internal complexities of the FITS format.  CFITSIO also
provides many advanced features for manipulating and filtering the information
in FITS files.")
    (properties
     '((release-monitoring-url .
        "https://heasarc.gsfc.nasa.gov/docs/software/fitsio/fitsio.html")))
    (license (license:non-copyleft "file://License.txt"
                                   "See License.txt in the distribution."))))

;;; The version is required for gnuastro.  It fails on check phase with a
;;; newer version.
(define-public cfitsio-4.4
  (package
    (inherit cfitsio)
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/"
             "cfitsio-" version ".tar.gz"))
       (sha256
        (base32 "098x1l8ijwsjp2ivp3v7pamrmpgwj5xmgb4yppm9w3w044zxr8b6"))))))

(define-public cianna
  (package
    (name "cianna")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Deyht/CIANNA")
              (commit (string-append "V-" version ".0"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0i7czicyiy9lldsrarsh9lpjm4znx3gnsi1kqqyhiafxjxsji35k"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure
          (delete 'check)     ; no tests
          (add-after 'unpack 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((blas #$(this-package-input "openblas")))
                (substitute* "compile.cp"
                  (("/usr/bin/gcc")
                   #$(cc-for-target))
                  (("/opt/OpenBLAS/include/")
                   (string-append blas "/include/"))
                  (("/opt/OpenBLAS/lib")
                   (string-append blas "/lib")))
                (substitute* "src/python_module_setup.py"
                  (("/opt/OpenBLAS/include")
                   (string-append blas "/include"))
                  (("/opt/OpenBLAS/lib")
                   (string-append blas "/lib"))))))
          (replace 'build
            (lambda* _
                (substitute* "compile.cp"
                  (("-Wno-unknown-pragmas")
                   "-Wno-unknown-pragmas -Wno-error=maybe-uninitialized"))
              (invoke "./compile.cp" "BLAS" "OPEN_MP" "LPTHREAD" "PY_INTERF")))
          (replace 'install
            (lambda _
              (rename-file "main" "cianna-cpu")
              (install-file "cianna-cpu" (string-append #$output "/bin"))))
          (add-after 'install 'install-python
            (lambda _
              (with-directory-excursion "src"
                (invoke "python" "python_module_setup.py" "install"
                        "--root=/"
                        (string-append "--prefix=" #$output))))))))
    (native-inputs
     (list python-wrapper
           python-numpy
           python-setuptools))
    (inputs (list openblas))
    (home-page "https://github.com/Deyht/CIANNA")
    (synopsis "Deep learning framework for astronomical data analysis")
    (description
     "This package provides a @acronym{CIANNA, Convolutional Interactive
Artificial Neural Networks by/for Astrophysicists} - a general-purpose deep
learning framework primarily developed and used for astronomical data
analysis.")
    (license license:asl2.0)))

(define-public cpl
  (package
    (name "cpl")
    (version "7.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.eso.org/pub/dfs/pipelines/libraries/cpl/cpl-"
             version ".tar.gz"))
       (sha256
        (base32 "083ppsa6ifc52m0s4ww4l9cajnh2f0y3s5bxaq31drihhrd2c355"))))
    (build-system gnu-build-system)
    (arguments
     ;; pycpl expects to find a lib/esopipes-plugins directory.  This is
     ;; overruled by the PYESOREX_PLUGIN_DIR search path, but the default
     ;; directory is still consulted and therefor needs to exist.
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'create-plugins-directory
            (lambda _
              (mkdir-p (string-append #$output "/lib/esopipes-plugins")))))))
    (native-inputs
     (list automake
           autoconf
           libtool
           pkg-config
           perl))
    (inputs
     (list cfitsio
           wcslib
           fftw
           fftwf))
    (home-page "https://www.eso.org/sci/software/cpl/")
    (synopsis "Common Pipeline Library for astronomical data reduction")
    (description
     "The @acronym{CPL, Common Pipeline Library} comprises a set of ISO-C
libraries that provide a comprehensive, efficient and robust software toolkit
to develop astronomical data-reduction tasks (known as recipes).  These
data-reduction tasks can then be executed manually by a user, or can be
triggered in an automated data-reduction framework (known as pipelines) which
are used at @acronym{ESO, European Southern Observatory} to monitor the health
status of @acronym{VLT, Very Large Telescope} instruments, for quick-look data
processing at the observatory, and the creation of data products available
from the ESO archive facility.")
    (license license:gpl2+)))

(define-public erfa
  (package
    (name "erfa")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liberfa/erfa")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hxjbcvdlq4871r17fphbaf3bd8dsjagp1rdb3j8v6kr4f1dil9n"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf libtool pkg-config))
    (home-page "https://github.com/liberfa/erfa")
    (synopsis "Essential Routines for Fundamental Astronomy")
    (description
     "The @acronym{ERFA, Essential Routines for Fundamental Astronomy} C library
contains key algorithms for astronomy, and is based on the @acronym{SOFA,
Standards of Fundamental Astronomy} library published by the @acronym{IAU,
International Astronomical Union}.")
    (license license:bsd-3)))

(define-public esorex
  (package
    (name "esorex")
    (version "3.13.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.eso.org/pub/dfs/pipelines/libraries/esorex/esorex-"
             version ".tar.gz"))
       (sha256
        (base32 "1mkxjm2rnmviqfblnr4wwb3simvs7f5dly66qylvdfynvg3gk2d9"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake
           autoconf
           libtool
           pkg-config
           perl))
    (inputs
     (list cfitsio
           wcslib
           cpl))
    (native-search-paths
     (list (search-path-specification
             (variable "ESOREX_PLUGIN_DIR")
             (files '("lib/esopipes-plugins")))))
    (home-page "https://www.eso.org/sci/software/cpl/esorex.html")
    (synopsis "The European Southern Observatory Recipe Execution Tool")
    (description
     "EsoRex is the @acronym{European Southern Observatory,ESO} Recipe
Execution Tool.  It can list, configure and execute @acronym{Common Pipeline
Library,CPL}-based recipes from the command line.")
    ;; Set a rerlease-monitoring-url because the ftp directory includes
    ;; prereleases that should not be refreshed to.
    (properties `((release-monitoring-url
                   . "https://www.eso.org/sci/software/cpl/download.html")))
    (license license:gpl2+)))

(define-public eye
  (package
    (name "eye")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/eye")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j8rpgz3fjp6fw0qmxgfqycf3n01fzxds4w12vgyrhbnk658ia41"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "CPPFLAGS=-fcommon")))    ; fix build with GCC 10
    (home-page "https://www.astromatic.net/software/eye")
    (synopsis "Small image feature detector using machine learning")
    (description
     "In @acronym{EyE, Enhance Your Extraction} an artificial neural network
connected to pixels of a moving window (@dfn{retina}) is trained to associate
these input stimuli to the corresponding response in one or several output
image(s).  The resulting filter can be loaded in SExtractor to operate
complex, wildly non-linear filters on astronomical images.  Typical
applications of EyE include adaptive filtering, feature detection and cosmetic
corrections.")
    (license license:cecill)))

(define-public fitsverify
  (package
    (name "fitsverify")
    (version "4.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://heasarc.gsfc.nasa.gov/docs/software/ftools/"
                           name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1d0qvgmrpv09qm4vi4n26frx4qb3mrdn261rs6vvrvg0lw1yhibc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure
          (replace 'build
            (lambda* _
              ;; Build steps are taken from Debian's package definition.
              (invoke #$(cc-for-target) "-o" #$name
                      "ftverify.c"
                      "fvrf_data.c"
                      "fvrf_file.c"
                      "fvrf_head.c"
                      "fvrf_key.c"
                      "fvrf_misc.c"
                      "-DSTANDALONE"
                      "-lcfitsio"
                      "-lm")))
          (replace 'install
            (lambda _
              (install-file #$name (string-append #$output "/bin")))))))
    (inputs
     (list cfitsio))
    (home-page "https://heasarc.gsfc.nasa.gov/docs/software/ftools/fitsverify/index.html")
    (synopsis "FITS File Format-Verification Tool")
    (description
     "Fitsverify is a computer program that rigorously checks whether a
@acronym{FITS, Flexible Image Transport System} data file conforms to the
requirements defined in Version 3.0 of the
@url{http://fits.gsfc.nasa.gov/fits_documentation.html, FITS Standard
document}.")
    (properties
     '((release-monitoring-url .
        "https://heasarc.gsfc.nasa.gov/docs/software/ftools/fitsverify/")))
    ;; The license is the same as for CFITSIO, see
    ;; URL: <https://salsa.debian.org/debian-astro-team/fitsverify>
    ;; File: <debian/copyright>
    (license (license:non-copyleft "file://License.txt"
                                   "See License.txt in the distribution."))))

(define-public genetic
  (package
    (name "genetic")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pynbody/genetic")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pwvr8bvnbp6d16f51hy981pdlrqz9yf9fh2wvxl958ikyjmaaiy"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'link-highfive
            (lambda _
              (rmdir "genetIC/HighFive")
              (symlink #+(package-source (this-package-native-input "highfive"))
                       (string-append (getcwd) "/HighFive"))))
          (add-before 'check 'delete-problematic-tests
            (lambda _
              (with-directory-excursion "../source/genetIC/tests"
                (for-each delete-file-recursively
                          ;; XXX: Some NumPy related tests failed with "Arrays
                          ;; are not almost equal to <...> decimals", it
                          ;; might need attention.
                          (list "test_06c_stray_subsampling"
                                "test_09a_border_safety"
                                "test_10a_gadget"
                                "test_10b_gadget_zoom_varimass"
                                "test_10c_gadget_zoom_ptype"
                                "test_10d_gadget_flagged_particles"
                                "test_10e_gadget_zoom_ptype_resample"
                                "test_10g_gadget_multifile_baryons"
                                "test_10h_gadget_zoom_ptype_gas"
                                "test_10i_gadget_zoom_ptype_autopad"
                                "test_14_velocity_modif"
                                "test_17_reversal"
                                "test_19_rounding_errors"
                                "test_22_angular_momentum_modif"
                                "test_26_gadgethdf"
                                "test_26a_gadgethdf_multifile_baryons"
                                ;; Error "File zoomtest.txt not found" on line
                                ;; 27 ("idfile zoomtest.txt")
                                "test_10f_gadget_multifile")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "../source/genetIC/tests"
                  ;; Matplotlib needs to write access.
                  (setenv "HOME" "/tmp")
                  (invoke "./run_tests.sh"))))))))
    (native-inputs
     (list boost
           git-minimal/pinned
           highfive
           pkg-config
           python-pynbody
           python-wrapper))
    (inputs
     (list fftw
           fftwf
           gsl
           hdf5))
    (home-page "https://github.com/pynbody/genetic")
    (synopsis "Genetically-modified initial conditions generator")
    (description
     "GenetIC is a code to generate initial conditions for cosmological
simulations, especially for zoom simulations of galaxies.  It provides support
for \"genetic modifications\" as described by e.g.
@url{https://arxiv.org/abs/1504.07250, Roth et al 2015},
@url{https://arxiv.org/abs/1706.04615, Rey & Pontzen 2018}.  It also supports
'splicing' as described by @url{https://arxiv.org/abs/2107.03407, Cadiou et al
2021}.")
    (license license:gpl3+)))

(define-public ginga
  (package
    (name "ginga")
    (version "5.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ginga" version))
       (sha256
        (base32 "0wv8fb8p8icsvkh2rn8jcxxx33kgac36gm9xqbgpm2z7z6m4haa7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; AssertionError: Not equal to tolerance rtol=1e-07, atol=0.0001
      #:test-flags #~(list "-k" "not test_fwhm")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; Relax matplotlib warning: ... because the default path
              ;; (/homeless-shelter/.config/matplotlib) is not a writable
              ;; directory ...
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-attrs
           python-pytest-doctestplus
           python-pytest-astropy
           python-pytest-astropy-header
           python-setuptools
           python-setuptools-scm))
    (inputs
     (list python-astropy
           python-numpy
           python-packaging
           python-pillow
           python-puremagic
           python-pyyaml
           python-qtpy
           ;; [recomended]
           opencv
           python-astroquery
           python-dateutil
           python-exif-read
           python-matplotlib
           python-photutils
           python-pillow-heif
           python-scipy
           ;; [qt6]
           python-pyqt-6
           ;; [pyside6]
           python-pyside-6
           ;; [tk,web]
           python-pycairo
           python-tornado))
    (home-page "https://ejeschke.github.io/ginga/")
    (synopsis "Scientific image viewer and toolkit for FITS files")
    (description
     "Ginga is a toolkit designed for building viewers for scientific image
data in Python, visualizing 2D pixel data in numpy arrays.  It can view
astronomical data such as contained in files based on the FITS (Flexible Image
Transport System) file format.  It is written and is maintained by software
engineers at the National Astronomical Observatory of Japan (NAOJ), the Space
Telescope Science Institute (STScI), and other contributing entities.

The Ginga toolkit centers around an image display object which supports
zooming and panning, color and intensity mapping, a choice of several
automatic cut levels algorithms and canvases for plotting scalable geometric
forms.  In addition to this widget, a general purpose \"reference\" FITS
viewer is provided, based on a plugin framework.  A fairly complete set of
standard plugins are provided for features that we expect from a modern FITS
viewer: panning and zooming windows, star catalog access, cuts, star
pick/FWHM, thumbnails, etc.")
(license license:bsd-3)))

(define-deprecated-package ginga-qt5
  ginga)

(define-public glnemo2
  (package
    (name "glnemo2")
    (version "1.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://gitlab.lam.fr/jclamber/glnemo2")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jmmxszh8d2jmfghig36nhykff345mqnpssfa64d0r7l9cnfp3cn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f        ; No test target
      #:configure-flags
      #~(list "CPPFLAGS=-fcommon"
              (string-append "-DCMAKE_C_FLAGS="
                             " -Wno-error=implicit-function-declaration"
                             " -Wno-error=implicit-int"
                             " -Wno-error=int-conversion"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-libraries-paths
            (lambda _
              (substitute* "CMakeLists.txt"
                ;; There is some not straightforward logic on how to set
                ;; the installation prefix for the project; inherit it
                ;; from the build-system default flags.
                (("CMAKE_INSTALL_PREFIX  \"/usr\"")
                 "CMAKE_INSTALL_PREFIX")
                (("/usr/include/CCfits")
                 (string-append
                  #$(this-package-input "ccfits") "/include/CCfits"))
                (("/usr/include/tirpc")
                 (string-append
                  #$(this-package-input "libtirpc") "/include/tirpc"))
                ;; It tries to detect library in two "predictable" paths,
                ;; required during the link phase.
                (("/usr/lib64/libtirpc.so")
                 (string-append
                  #$(this-package-input "libtirpc") "/lib/libtirpc.so"))))))))
    (inputs
     (list ccfits
           cfitsio
           glm
           glu
           hdf5
           libtirpc
           qtbase-5
           zlib))
    (home-page "https://projets.lam.fr/projects/glnemo2/wiki/Wiki")
    (synopsis "3D interactive visualization program for n-body like particles")
    (description
     "GLNEMO2 is an interactive 3D visualization program which displays
particles positions of the different components (gas, stars, disk, dark
matter halo, bulge) of an N-body snapshot.  It is a tool for running
N-body simulations from isolated galaxies to cosmological simulations.
It has a graphical user interface (based on QT 5.X API), uses a fast
3D engine (OPenGL and GLSL), and is generic with the possibility to load
different kinds of input files.")
    (license license:cecill)))

(define-public gnuastro
  (package
    (name "gnuastro")
    (version "0.24")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.lz"))
       (sha256
        (base32
         "0z2i5cx4wx1m90nyg98f653gwdil86mi0w55sg8rick4rg2a9ifz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
     (list cfitsio-4.4
           curl
           gsl
           libgit2
           libjpeg-turbo
           libtiff
           wcslib
           zlib))
    (native-inputs
     (list libtool lzip))
    (home-page "https://www.gnu.org/software/gnuastro/")
    (synopsis "Astronomy utilities")
    (description "The GNU Astronomy Utilities (Gnuastro) is a suite of
programs for the manipulation and analysis of astronomical data.")
    (license license:gpl3+)))

(define-public gpredict
  ;; The latest tag, 2.3, has no major difference with 2.2.1 and is dated for
  ;; 2018. Additionally, there is some activity on the master branch, see
  ;; <https://github.com/csete/gpredict/issues/368>.
  (package
    (name "gpredict")
    (version "2.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/csete/gpredict/releases"
                           "/download/v" version
                           "/gpredict-" version ".tar.bz2"))
       (sha256
        (base32 "0hwf97kng1zy8rxyglw04x89p0bg07zq30hgghm20yxiw2xc8ng7"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "CFLAGS=-O2 -g -fcommon")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              ;; Remove reference to non-existent file.
              (substitute* "po/POTFILES.in"
                (("src/gtk-sat-tree\\.c") "")))))))
    (native-inputs
     (list gettext-minimal intltool pkg-config))
    (inputs
     (list curl glib goocanvas gtk+))
    (home-page "https://oz9aec.dk/gpredict/")
    (synopsis "Satellite tracking and orbit prediction application")
    (description
     "Gpredict is a real-time satellite tracking and orbit prediction
application.  It can track a large number of satellites and display their
position and other data in lists, tables, maps, and polar plots (radar view).
Gpredict can also predict the time of future passes for a satellite, and
provide you with detailed information about each pass.

Some core features of Gpredict include:

@itemize
@item Tracking of a large number of satellites only limited by the physical
memory and processing power of the computer
@item Display the tracking data in lists, maps, polar plots and any
combination of these
@item Have many modules open at the same either in a notebook or in their own
windows.  The modules can also run in full-screen mode
@item You can use many ground stations
@item Predict upcoming passes
@item Gpredict can run in real-time, simulated real-time (fast forward and
backward), and manual time control
@item Detailed information both the real time and non-real time modes
@item Doppler tuning of radios via Hamlib rigctld
@item Antenna rotator control via Hamlib rotctld
@end itemize")
    (license license:gpl2+)))

(define* (healpix-source #:key version sha256-base32-hash)
    ;; The sources of HEALPix contains 6 independent packages (Fortran90, IDL,
    ;; C, C++, Java and Python) and distributed together with libsharp.  There
    ;; is also a fresh C++ tarball which does not follow the naming pattern
    ;; e.g. healpix_cxx-3.82.1.tar.gz.
     (origin
       (method url-fetch)
       (uri
        (let* ((name "Healpix")
               (version-list (string-split version #\.))
               (name+version (format #f "~a_~{~a.~a~a~}" name version-list)))
          (string-append "mirror://sourceforge/healpix/"
                         name+version "/" name+version "_" "2024Nov13.tar.gz")))
       (sha256
        (base32 sha256-base32-hash ))))

(define-public healpix
  (package
    (name "healpix")
    (version "3.8.3")
    (source
     (healpix-source
      #:version version
      #:sha256-base32-hash "1nhxad4zbk6qm6zblvyvyaavfq7vy8asq150n83dfvsrzj7c2xl8"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no tests
      #:make-flags
      #~(list "shared"
              "AR=ar -rsv"
              "OPT=-O2 -Wall"
              "PIC=-fPIC"
              (string-append "CC=" #$(cc-for-target))
              (string-append "CFITSIO_INCDIR="
                             #$(this-package-input "cfitsio") "/include")
              (string-append "CFITSIO_LIBDIR="
                             #$(this-package-input "cfitsio") "/lib")
              (string-append "INCDIR=" #$output "/include")
              (string-append "LIBDIR=" #$output "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure
          (add-after 'unpack 'chdir-c
            (lambda _
              (chdir "src/C/subs")))
          (add-before 'install 'set-output-directories
            (lambda _
              (mkdir-p (string-append #$output "/include"))
              (mkdir-p (string-append #$output "/lib")))))))
    (native-inputs
     (list pkg-config autoconf automake))
    (inputs
     (list cfitsio))
    (home-page "https://healpix.sourceforge.io/")
    (synopsis "Representation of spherical data")
    (description
     "@acronym{HEALPix, Hierarchical Equal Area isoLatitude Pixelation} of a
sphere produces a subdivision of a spherical surface in which each pixel
covers the same surface area as every other pixel.  This package provides the
dynamic library for the C language implementation of HEALPix.")
    (license license:gpl2+)))

(define-public healpix-cxx
  (package
    (inherit healpix)
    (name "healpix-cxx")
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-cxx
            (lambda _
              (chdir "src/cxx")))
          (add-after 'chdir-cxx 'adjust-unit-tests
            (lambda _
              (substitute* "configure.ac"
                ;; Run unit tests using serial harness, taken from
                ;; <https://salsa.debian.org/debian-astro-team/healpix-cxx/>.
                (("foreign subdir-objects -Wall -Werror")
                 "foreign serial-tests subdir-objects -Wall -Werror"))))
           (replace 'bootstrap
             (lambda _
               (invoke "aclocal")
               (invoke "automake" "--add-missing")
               (invoke "autoconf"))))))
    (inputs (modify-inputs (package-inputs healpix)
              (prepend libsharp zlib)))
    (description
     (string-replace-substring (package-description healpix)
                    "C language"
                    "C++ language"))))

(define-public imppg
  (package
    (name "imppg")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/GreatAttractor/imppg")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fcssjpavjyvjwwbbcs2i0b0sk409hl7x8il88d7xviggfiyh1hm"))))
    (build-system cmake-build-system)
    (native-inputs
     (list boost
           pkg-config))
    (inputs
     (list cfitsio
           freeimage
           glew
           lua
           wxwidgets))
    (home-page "https://github.com/GreatAttractor/imppg")
    (synopsis "Astronomical Image Post-Proccessor (ImPPG)")
    (description
     "ImPPG performs Lucy-Richardson deconvolution, unsharp masking,
brightness normalization and tone curve adjustment.  It can also apply
previously specified processing settings to multiple images.  All operations
are performed using 32-bit floating-point arithmetic.

Supported input formats: FITS, BMP, JPEG, PNG, TIFF (most of bit depths and
compression methods), TGA and more.  Images are processed in grayscale and can
be saved as: BMP 8-bit; PNG 8-bit; TIFF 8-bit, 16-bit, 32-bit
floating-point (no compression, LZW- or ZIP-compressed), FITS 8-bit, 16-bit,
32-bit floating-point.")
    (license license:gpl3+)))

(define-public indi
  (package
    (name "indi")
    (version "2.1.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indilib/indi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sa74i7352g567p1i29mr8qdgb2z481zvqa3f8x2xnmim8brirbm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:parallel-tests? #f  ; Socket address collisions between tests
      #:configure-flags
      #~(list "-DINDI_BUILD_UNITTESTS=ON"
              "-DINDI_BUILD_INTEGTESTS=ON"
              "-DCMAKE_INSTALL_LIBDIR=lib"
              (string-append "-DCMAKE_INSTALL_PREFIX=" #$output)
              (string-append "-DUDEVRULES_INSTALL_DIR=" #$output "/lib/udev/rules.d"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-udev-rules
            (lambda _
              (substitute* (list "drivers/auxiliary/99-indi_auxiliary.rules"
                                 "drivers/video/80-dbk21-camera.rules")
                (("/bin/sh") (which "sh"))
                (("/sbin/modprobe")
                 (string-append #$(this-package-input "kmod") "/bin/modprobe")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "integs"
                  (invoke "ctest" "-V" "--output-on-failure"))
                (with-directory-excursion "test"
                  (invoke "ctest" "-V"))))))))
    (native-inputs
     (list googletest))
    (inputs
     (list cfitsio
           curl
           fftw
           gsl
           kmod
           libev
           libjpeg-turbo
           libnova
           libtiff
           libusb
           zlib))
    (home-page "https://www.indilib.org")
    (synopsis "Library for astronimical instrumentation control")
    (description
     "INDI (Instrument-Neutral Device Interface) is a distributed XML-based
control protocol designed to operate astronomical instrumentation.  INDI is
small, flexible, easy to parse, scalable, and stateless.  It supports common
DCS functions such as remote control, data acquisition, monitoring, and a lot
more.")
    (license (list license:bsd-3
                   license:gpl2+
                   license:lgpl2.0+
                   license:lgpl2.1+))))

(define-public iraf-community
  (package
    (name "iraf-community")
    (version "2.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/iraf-community/iraf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bp88lgciibkm83jbmv8nazz8mzcrl1vlmll853fib6pwiikd015"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; XXX: All tests fail, figure out how to fix them.
      #:tests? #f
      ;; It failes with segmentation fault in parallel build
      #:parallel-build? #f
      ;; No such file or directory .../lib/iraf/lib/libmemdbg.a
      #:validate-runpath? #f
      #:make-flags
      #~(list (format #f "IRAFARCH=~a" (cond (#$(target-hurd?) "hurd")
                                             (#$(target-64bit?) "linux64")
                                             (else "linux")))
              "MNAME=linux64"
              (string-append "CC=" #$(cc-for-target))
              (string-append "prefix=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ;; no configure
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (list "test/run_tests"
                                 "unix/boot/spp/xc.c"
                                 "unix/f2c/libf2c/makefile.u"
                                 "unix/f2c/src/changes"
                                 "unix/f2c/src/main.c"
                                 "unix/f2c/src/makefile.u"
                                 "unix/f2c/trim_f2c.sh"
                                 "unix/hlib/f77.sh"
                                 "unix/hlib/irafarch.sh"
                                 "unix/hlib/irafcl.sh"
                                 "unix/hlib/mkfloat"
                                 "unix/hlib/mkiraf.sh"
                                 "unix/hlib/mkmlist.sh"
                                 "unix/os/zoscmd.c")
                (("/bin/sh") (which "sh")))
              (substitute* "unix/hlib/irafarch.sh"
                (("/usr/bin/uname") (search-input-file inputs "/bin/uname"))
                (("/bin/uname") (search-input-file inputs "/bin/uname")))
              (substitute* "Makefile"
                (("DESTDIR./etc/iraf") "prefix)/etc/iraf")))))))
    (native-inputs
     (list bison flex perl))
    (inputs
     (list curl expat ncurses readline zlib))
    (home-page "https://iraf-community.github.io/")
    (synopsis "Image Reduction and Analysis Facility")
    (description
     "IRAF is the Image Reduction and Analysis Facility, a general purpose
software system for the reduction and analysis of astronomical data.  IRAF was
written by the @acronym{NOAO, National Optical Astronomy Observatories} in
Tucson, Arizona.  This package provides a community successor of the last IRAF
release from 2013.")
    ;; It's multi licenses project but primary one is MIT (Expat) it was
    ;; checked with upstream, see
    ;; <https://github.com/iraf-community/iraf/issues/403>.
    (license license:expat)))

(define-public java-cds-healpix
  ;; XXX: Upstream bundles java-commons-math3 available in Guix, find out how
  ;; to use the system package instead of it.
  (package
    (name "java-cds-healpix")
    (version "0.30.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cds-astro/cds-healpix-java")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wi5ni6j0rvjyhz80g1gglxdimf7gnfa1kx8a3c2przzbwya0j8d"))))
    (build-system ant-build-system)
    (arguments
     (list
      #:jar-name "cdshealpix.jar"
      #:source-dir "src/main/java/cds/healpix"
      #:test-dir "src/test"))
    (home-page "https://github.com/cds-astro/cds-healpix-java")
    (synopsis "CDS HEALPix library in Java")
    (description
     "This package provides a @acronym{Centre de Données astronomiques de
Strasbourg, CDS} implementation in Java of the @acronym{Hierarchical Equal
Area isoLatitude Pixelization of a sphere , HEALPix} tesselation.")
    (license license:bsd-3)))

(define-public libnova
  (package
    (name "libnova")
    (version "0.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/libnova/libnova.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0icwylwkixihzni0kgl0j8dx3qhqvym6zv2hkw2dy6v9zvysrb1b"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-git-version
            (lambda _
              (substitute* "./git-version-gen"
                (("/bin/sh") (which "sh"))))))))
    (native-inputs
     (list autoconf automake libtool))
    (home-page "https://libnova.sourceforge.net/")
    (synopsis "Celestial mechanics, astrometry and astrodynamics library")
    (description
     "Libnova is a general purpose, double precision, Celestial Mechanics,
Astrometry and Astrodynamics library.")
    (license (list license:lgpl2.0+
                   license:gpl2+)))) ; examples/transforms.c & lntest/*.c

(define-public libpasastro
  (package
    (name "libpasastro")
    (version "1.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pchev/libpasastro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16d0kc22a1cn9kl8s0mxnaw0ax6qjq3rwhfifgpf1a3f9qp17115"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no tests provided
      #:make-flags
      #~(list
         ;; Keep OS detection for the case when Hurd would be suitable to try.
         #$@(if (target-linux?) '("OS_TARGET=linux") '())
         ;; Enable buildtime CPU detection where supported,
         ;; and set a suitable CPU target variable.
         #$@(match (or (%current-target-system)
                       (%current-system))
              ("i686-linux"
               '("CPU_TARGET=i386"))
              ("x86_64-linux"
               '("CPU_TARGET=x86_64"))
              ;; There is no a case for RISCV in upstream, attempt to treat it
              ;; as ARM.
              ((or "armhf-linux" "aarch64-linux" "riscv64")
               '("CPU_TARGET=armv7l"))
              (_ '()))
         (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://github.com/pchev/libpasastro")
    (synopsis "Interface to astronomy library for use from Pascal program")
    (description
     "This package provides shared libraries to interface Pascal program with
standard astronomy libraries:

@itemize
@item @code{libpasgetdss.so}: Interface with GetDSS to work with DSS images.
@item @code{libpasplan404.so}: Interface with Plan404 to compute planets position.
@item @code{libpaswcs.so}: Interface with libwcs to work with FITS WCS.
@item @code{libpasspice.so}: To work with NAIF/SPICE kernel.
@end itemize")
      (license license:gpl2+)))

(define-public libsep
  (package
    (name "libsep")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kbarbary/sep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ni02sf3pcg438mi26csdcwsbq1v5mnxlna2aiwxj0mhq2psb1rw"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:modules '((guix build cmake-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:rest args)
              (with-directory-excursion "../source"
                (apply (assoc-ref gnu:%standard-phases 'check)
                       #:test-target "test" args)))))))
    (native-inputs
     (list python-wrapper))
    (home-page "https://github.com/kbarbary/sep")
    (synopsis "Astronomical source extraction and photometry library")
    (description
     "SEP makes the core algorithms of
@url{https://www.astromatic.net/software/sextractor/, sextractor} available as
a library of stand-alone functions and classes.  These operate directly on
in-memory arrays (no FITS files or configuration files).  The code is derived
from the Source Extractor code base (written in C) and aims to produce results
compatible with Source Extractor whenever possible.  SEP consists of a C
library with no dependencies outside the standard library, and a Python module
that wraps the C library in a Pythonic API.  The Python wrapper operates on
NumPy arrays with NumPy as its only dependency.")
    (license (list license:expat license:lgpl3+ license:bsd-3))))

(define-public libsharp
  (package
    (name "libsharp")
    (version "3.8.2")
    (source
     (healpix-source
      #:version version
      #:sha256-base32-hash "09x1lafq01gzk16yvmz2pdhrxnqfjp3b2p9hlgy0dbrdg82ryqj7"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir-libsharp
            (lambda _
              (chdir "src/common_libraries/libsharp"))))))
    (home-page "https://healpix.sourceforge.io/")
    (synopsis "Efficient spherical harmonic transforms at arbitrary spins")
    (description
     "This package provides a librari for spherical harmonic
transforms (SHTs), which evolved from the libpsht library, addressing several
of its shortcomings, such as adding MPI support for distributed memory systems
and SHTs of fields with arbitrary spin, but also supporting new developments
in CPU instruction sets like the Advanced Vector Extensions (AVX) or fused
multiply-accumulate (FMA) instructions.  The library is implemented in
portable C99 and provides an interface that can be easily accessed from other
programming languages such as C++, Fortran, Python etc.  Generally, libsharp's
performance is at least on par with that of its predecessor; however,
significant improvements were made to the algorithms for scalar SHTs, which
are roughly twice as fast when using the same CPU capabilities.

Supporting paper is available at https://arxiv.org/abs/1303.4945")
    (license license:gpl2+)))

(define-public libskry
  (package
    (name "libskry")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreatAttractor/libskry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14kwng0j8wqzlb0gqg3ayq36l15dpz7kvxc56fa47j55b376bwh6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list
        (string-append
         "LIBAV_INCLUDE_PATH=" (assoc-ref %build-inputs "ffmpeg") "/include"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure) ;; no configure provided
         (delete 'check) ;; no tests provided
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (include (string-append out "/include")))
               (copy-recursively "bin" lib)
               (copy-recursively "include" include))
             #t)))))
    (inputs
     (list ffmpeg-4))
    (home-page "https://github.com/GreatAttractor/libskry")
    (synopsis "Astronimical lucky imaging library")
    (description
     "@code{libskry} implements the lucky imaging principle of astronomical
imaging: creating a high-quality still image out of a series of many
thousands) low quality ones")
    (license license:gpl3+)))

(define-public libxisf
  (package
    (name "libxisf")
    (version "0.2.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.nouspiro.space/nou/libXISF")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zhj4xijr374c9raddxw8ibawx0n66lqvypgpccb81g41qkkdkmx"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DUSE_BUNDLED_LIBS=OFF")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list lz4 pugixml zlib))
    (home-page "https://nouspiro.space/?page_id=306")
    (synopsis "Astronomical library to load and write XISF file format")
    (description
     "LibXISF is C++ library that can read and write @acronym{XISF,Extensible
Image Serialization Format} files produced by @url{https://pixinsight.com/,
PixInsight}.  It implements
@url{https://pixinsight.com/doc/docs/XISF-1.0-spec/XISF-1.0-spec.html, XISF
1.0 specification}.")
    (license license:gpl3+)))

(define-public libxisf-for-tenmon
  ;; This is an exact commit required for tenmon git submodule.
  (let ((commit "556bb22d2675ee6072c6224fef3da0fb5d93db41")
        (revision "0"))
    (hidden-package
     (package
       (inherit libxisf)
       (name "libxisf")
       (version (git-version "0.2.13" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gitea.nouspiro.space/nou/libXISF")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "037dijy0ql1mwm8nddwawlf3ms6w30kxdlkrjjprfsss80ssn30k"))))))))

(define-public missfits
  (package
    (name "missfits")
    (version "2.8.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/missfits")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12ndvrr3l5j7ph2i5f3qf0wqmv5ymsyjzxnnypqajsvliw72iprh"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; Address this link error:
         ;; ld: ... multiple definition of ... first defined here
         "CPPFLAGS=-fcommon")))
    (home-page "https://www.astromatic.net/software/missfits")
    (synopsis "FITS files Maintenance program")
    (description
     "MissFITS is a program that performs basic maintenance and packaging tasks
on FITS files:

@itemize
@item add/edit FITS header keywords
@item split/join @acronym{MEF, Multi-Extension-FITS} files
@item unpack/pack FITS data-cubes
@item create/check/update FITS checksums, using
@uref{http://www.adass.org/adass/proceedings/adass94/seamanr.html,
R. Seaman's protocol}
@end itemize\n")
    (license license:gpl3+)))

(define-public phd2
    (package
      (name "phd2")
      (version "2.6.14")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/OpenPHDGuiding/phd2")
                (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0n2jzlpwvjgj8ydg7hsqxnvy8s9pdi5z96id0231kgi87pyfkh7s"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-26)))
         (snippet
          #~(begin
              ;; XXX: 'delete-all-but' is copied from the turbovnc package.
              (define (delete-all-but directory . preserve)
                (with-directory-excursion directory
                  (let* ((pred (negate (cut member <>
                                            (cons* "." ".." preserve))))
                         (items (scandir "." pred)))
                    (for-each (cut delete-file-recursively <>) items))))
              (delete-all-but "thirdparty" "thirdparty.cmake")))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DOPENSOURCE_ONLY=yes"
                "-DUSE_SYSTEM_CFITSIO=yes"
                "-DUSE_SYSTEM_EIGEN3=yes"
                "-DUSE_SYSTEM_GTEST=yes"
                "-DUSE_SYSTEM_LIBINDI=yes"
                "-DUSE_SYSTEM_LIBUSB=yes")))
      (native-inputs
       (list gettext-minimal
             googletest
             perl
             pkg-config
             python))
      (inputs
       (list cfitsio
             curl
             eigen
             gtk+
             indi
             libnova
             libusb
             opencv
             wxwidgets
             zlib))
      (home-page "https://openphdguiding.org")
      (synopsis "Teleskope guiding software")
      (description
       "PHD2 is the enhanced,second generation version of the PHD guiding
software from Stark Labs.")
      (license license:bsd-3)))

(define-public psfex
  (package
    (name "psfex")
    (version "3.24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/psfex")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ykgzyxnxjxqk6b8jng006wjilg4fqaxclpfn8plg6brk1qf39sn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "CPPFLAGS=-fcommon"
              "--enable-openblas"
              "--enable-plplot"
              (string-append "--with-fftw-libdir="
                             #$(this-package-input "fftw") "/lib")
              (string-append "--with-fftw-incdir="
                             #$(this-package-input "fftw") "/include")
              (string-append "--with-openblas-libdir="
                             #$(this-package-input "openblas") "/lib")
              (string-append "--with-openblas-incdir="
                             #$(this-package-input "openblas") "/include")
              (string-append "--with-plplot-libdir="
                             #$(this-package-input "plplot") "/lib")
              (string-append "--with-plplot-incdir="
                             #$(this-package-input "plplot") "/include"))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list openblas fftw fftwf plplot))
    (home-page "https://www.astromatic.net/software/psfex/")
    (synopsis "Astronomical PSF modelling and quality assessment")
    (description
     "@acronym{PSFEx, PSF Extractor} extracts models of the @acronym{PSF,
Point Spread Function} from FITS images processed with SExtractor, and
measures the quality of images.  The generated PSF models can be used for
model-fitting photometry or morphological analyses.")
    (license license:gpl3+)))

(define-public python-acstools
  (package
    (name "python-acstools")
    (version "3.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "acstools" version))
       (sha256
        (base32 "0kc1lwr160awk3rq44iav2bk8b9w7vw4q6dd1s035yb442cqz0qh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-ci-watson
           python-pytest
           python-pytest-astropy-header
           python-pytest-remotedata
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-pyyaml
           python-requests))
    (home-page "https://github.com/spacetelescope/acstools")
    (synopsis "Hubble Space Telescope Advanced Camera for Surveys Tools")
    (description
     "Python Tools for HST ACS (Advanced Camera for Surveys) Data.")
    (license license:bsd-3)))

(define-public python-ads
  (package
    (name "python-ads")
    (version "0.12.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ads" version))
       (sha256
        (base32 "156y3zp0nm77976rwsxjjdh7yvggas8s6m0nm523khvn4lq8f9n0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list nss-certs-for-test
           python-httpretty
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-mock
           python-requests
           python-six
           python-werkzeug))
    (home-page "http://www.github.com/andycasey/ads/")
    (synopsis "Python client to NASA's Astrophysics Data System")
    (description
     "This package provides a Python Module to Interact with NASA's
@acronym{Astrophysics Data System,ADS}.")
    (license license:expat)))

(define-public python-aiapy
  (package
    (name "python-aiapy")
    (version "0.10.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aiapy" version))
       (sha256
        (base32 "0lr8v2bakqxqn516fr45905lcql72kac3q6rdzn24rn95f812n27"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          ;; It fails to check SunPy's optional inputs versions.
          (delete 'sanity-check)
          (add-before 'check 'set-home
            (lambda _
              ;; E PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-pytest-astropy
           python-pytest-cov
           python-pytest-doctestplus
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-sunpy))
    (home-page "https://aia.lmsal.com/")
    (synopsis "Library for AIA data analysis")
    (description
     "aiapy is a Python package for analyzing data from the @acronym{AIA,
Atmospheric Imaging Assembly} instrument onboard NASA's @acronym{SDO, Solar
Dynamics Observatory} spacecraft.")
    (license license:bsd-3)))

(define-public python-aplpy
  (package
    (name "python-aplpy")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aplpy" version))
       (sha256
        (base32 "03c8k7y75f5bwm8d08fr5xfaay4d9jzr5sas4j2frs7zrr8aak51"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "aplpy")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-env
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest-astropy
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-pillow
           python-pyavm
           python-pyregion
           python-reproject
           python-scikit-image
           python-shapely))
    (home-page "http://aplpy.github.io")
    (synopsis "Astronomical Plotting Library in Python")
    (description
     "@acronym{APLpy, the Astronomical Plotting Library in Python} is a Python
module aimed at producing publication-quality plots of astronomical imaging data
in FITS format.  The module uses @code{matplotlib}, a powerful and interactive
plotting package.  It is capable of creating output files in several graphical
formats, including EPS, PDF, PS, PNG, and SVG.

Main features:
@itemize
@item Make plots interactively or using scripts
@item Show grayscale, colorscale, and 3-color RGB images of FITS files
@item Generate co-aligned FITS cubes to make 3-color RGB images
@item Make plots from FITS files with arbitrary WCS (e.g. position-velocity)
@item Slice multi-dimensional FITS cubes
@item Overlay any number of contour sets
@item Overlay markers with fully customizable symbols
@item Plot customizable shapes like circles, ellipses, and rectangles
@item Overlay ds9 region files
@item Overlay coordinate grids
@item Show colorbars, scalebars, and beams
@item Customize the appearance of labels and ticks
@item Hide, show, and remove different contour and marker layers
@item Pan, zoom, and save any view as a full publication-quality plot
@item Save plots as EPS, PDF, PS, PNG, and SVG
@end itemize")
    (license license:expat)))

(define-public python-asdf
  (package
    (name "python-asdf")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "1m0j8lbyhblpj8sjnc6ffb8lplffnj17pwyw8i08kp2rfbaarg10"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1986 passed, 13 skipped, 2 xfailed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))))
    (native-inputs
     (list python-psutil
           python-pytest
           python-pytest-remotedata
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf-standard
           python-asdf-transform-schemas
           python-attrs ;; for vendorized jsonschema
           python-importlib-metadata
           python-jmespath
           python-numpy
           python-packaging
           python-pyyaml
           python-semantic-version
           ;; [optional]
           python-fsspec
           python-lz4))
    (home-page "https://github.com/asdf-format/asdf")
    (synopsis "Python tools to handle ASDF files")
    (description
     "The Advanced Scientific Data Format (ASDF) is a next-generation
interchange format for scientific data.  This package contains the Python
implementation of the ASDF Standard.")
    (license license:bsd-3)))

(define-public python-asdf-3
  (package
    (inherit python-asdf)
    (version "3.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "0scnw5y4x5k3vyfylq0w612b662xlccx3gsscaw082zlv2yxfyh4"))))))

(define-public python-asdf-4
  (package
    (inherit python-asdf)
    (version "4.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "1h9dvnxdcd7cmjddpfnjsn6a1acav8gm5307gf7kpifacf99fdqz"))))))

(define-public python-asdf-astropy
  (package
    (name "python-asdf-astropy")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_astropy" version))
       (sha256
        (base32 "0hsm9brcq92x0mhayc067skzxm0gvkl9m04af98fac3jhn08favv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 4419 passed, 1 skipped
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; AttributeError: 'AsdfFile' object has no attribute
                    ;; '_open_impl'
                    (list "not test_legacy_icrs_deseialize"
                          "test_read_examples[example0]"
                          "test_read_examples[example1]"
                          "test_read_examples[example2]"
                          "test_read_examples[example3]"
                          "test_read_examples[example4]"
                          "test_read_examples[example5]"
                          "test_read_examples[example6]")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-asdf-plugin
           python-pytest-astropy
           python-pytest-xdist
           python-scipy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-asdf-coordinates-schemas
           python-asdf-standard
           python-asdf-transform-schemas
           python-astropy-minimal
           python-numpy
           python-packaging))
    (home-page "https://github.com/astropy/asdf-astropy")
    (synopsis "ASDF serialization support for astropy")
    (description
     "This package includes plugins that provide ASDF serialization support for
Astropy objects.")
    (license license:bsd-3)))

(define-public python-asdf-bootstrap
  (hidden-package
   (package/inherit python-asdf
     (arguments
      (list #:tests? #f
            #:phases #~(modify-phases %standard-phases
                         (delete 'sanity-check))))
     (native-inputs
      (list python-setuptools
            python-setuptools-scm))
    (propagated-inputs
     (list python-importlib-metadata
           python-numpy
           python-pyyaml
           python-semantic-version)))))

(define-public python-asdf-compression
  (package
    (name "python-asdf-compression")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/asdf-format/asdf-compression")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fd2d5raglp1nwjy0hr3kckk518xrph451zymlw58jgwg5l1vq3m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-numpy
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-blosc
           python-lz4
           python-zstandard))
    (home-page "https://github.com/asdf-format/asdf-compression")
    (synopsis "ASDF extension to support various compression algorithms")
    (description
     "This package includes a plugin for the Python library ASDF to add support for
reading and writing various compression algorithms including:
@url{https://www.blosc.org/python-blosc/reference.html,
Blosc},@url{https://python-lz4.readthedocs.io/en/stable/lz4.frame.html, LZ4
Frame},@url{http://facebook.github.io/zstd/, Zstandard}.")
    (license license:bsd-3)))

(define-public python-asdf-coordinates-schemas
  (hidden-package
   (package
     (name "python-asdf-coordinates-schemas")
     (version "0.4.0")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "asdf_coordinates_schemas" version))
        (sha256
         (base32 "1i5w1v74b84ygbd9pw9cs35bjaqs35f5iavazhjpbycqr1pf8rvh"))))
     (build-system pyproject-build-system)
     (arguments
      (list
       #:test-flags
       #~(list "--numprocesses" (number->string (parallel-job-count)))))
     (native-inputs
      (list python-pytest
            python-pytest-xdist
            python-setuptools
            python-setuptools-scm
            python-wheel))
     (propagated-inputs
      (list python-asdf))
     (home-page "https://github.com/asdf-format/asdf-coordinates-schemas")
     (synopsis "ASDF coordinates schemas")
     (description
      "This package provides ASDF schemas for validating coordinates tags.
Users should not need to install this directly; instead, install an
implementation package such as asdf-astropy.")
     (license license:bsd-3))))

(define-public python-asdf-fits-schemas
  (hidden-package
   ;; This package was never released and has been archived. The schemas in
   ;; this package were never removed from and will continue to be maintained
   ;; in <https://github.com/asdf-format/asdf-standard>.
   (let ((commit "6321c0ae4e44c9a59ccf81a446f9d9e22fd42b55")
         (revision "2"))
     (package
       (name "python-asdf-fits-schemas")
       (version (git-version "0.0.1" revision commit))
       (source
        (origin
          (method git-fetch)
          (uri (git-reference
                (url "https://github.com/asdf-format/asdf-fits-schemas")
                (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32 "0p0m1sgnv9yqk0l0w15skvfshl47x0gc7lg6p2x83158hjyix5q6"))))
       (build-system pyproject-build-system)
       (arguments
        (list
         #:tests? #f ; cycle with python-asdf
         #:phases
         #~(modify-phases %standard-phases
             (add-before 'build 'set-version
               (lambda _
                 (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" "0.0.1"))))))
       (native-inputs
        (list python-setuptools
              python-setuptools-scm
              python-wheel))
       (propagated-inputs
        (list python-asdf-standard
              python-importlib-resources))
       (home-page "https://github.com/asdf-format/asdf-fits-schemas")
       (synopsis "ASDF schemas to support the FITS format")
       (description
        "This package provides ASDF schemas for validating FITS tags.")
       (license license:bsd-3)))))

(define-public python-asdf-standard
  (package
    (name "python-asdf-standard")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_standard" version))
       (sha256
        (base32 "1p7x5j8ym70c2cmgnv0113i4q465jbrqg8311mwbfz5q1lfi4pqc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-asdf-bootstrap
           python-asdf-transform-schemas-bootstrap
           python-packaging
           python-pytest
           python-pytest-asdf-plugin
           python-pyyaml
           python-setuptools
           python-setuptools-scm))
    (home-page "https://asdf-standard.readthedocs.io/")
    (synopsis "ASDF standard schemas")
    (description
     "This package provides Python implementation of @acronym{ASDF, Advanced
Scientific Data Format} - a proposed next generation interchange format for
scientific data.  ASDF aims to exist in the same middle ground that made FITS
so successful, by being a hybrid text and binary format: containing human
editable metadata for interchange, and raw binary data that is fast to load
and use.  Unlike FITS, the metadata is highly structured and is designed
up-front for extensibility.")
    (license license:bsd-3)))

(define-public python-asdf-transform-schemas
  (hidden-package
   (package
     (name "python-asdf-transform-schemas")
     (version "0.6.0")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "asdf_transform_schemas" version))
        (sha256
         (base32 "0clp3a2ldfhvsh5c7zqd7nr2bvv62a89aaf8p4a2vzgzjvhghl0g"))))
     (build-system pyproject-build-system)
     (arguments
      (list
       ;; XXX: Check why all tests fail in this file.
       #:test-flags #~(list "--deselect=tests/test_invalid.py")))
     (native-inputs
      (list python-asdf-bootstrap
            python-pytest
            python-setuptools
            python-setuptools-scm
            python-wheel))
     (propagated-inputs
      (list python-asdf-standard))
     (home-page "https://github.com/asdf-format/asdf-transform-schemas")
     (synopsis "ASDF schemas for transforms")
     (description
      "This package provides ASDF schemas for validating transform tags.
Users should not need to install this directly; instead, install an
implementation package such as asdf-astropy.")
     (license license:bsd-3))))

(define-public python-asdf-transform-schemas-bootstrap
  (hidden-package
   (package/inherit python-asdf-transform-schemas
     (arguments
      (list #:tests? #f
            #:phases #~(modify-phases %standard-phases
                         (delete 'sanity-check))))
     (native-inputs
      (list python-setuptools
            python-wheel))
     (propagated-inputs '()))))

(define-public python-asdf-wcs-schemas
  (hidden-package
   (package
     (name "python-asdf-wcs-schemas")
     (version "0.5.0")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "asdf_wcs_schemas" version))
        (sha256
         (base32 "1dar2pzf2plwyl1rbmnv8fqvx1ljgpf3z39d4ybmn690djjdsyxg"))))
     (build-system pyproject-build-system)
     (arguments
      (list
       #:test-flags #~(list "tests")))
     (native-inputs
      (list python-asdf
            python-pytest
            python-pytest-openfiles
            python-setuptools
            python-setuptools-scm
            python-wheel))
     (propagated-inputs
      (list python-asdf-coordinates-schemas
            python-asdf-standard
            python-asdf-transform-schemas))
     (home-page "https://github.com/asdf-format/asdf-wcs-schemas")
     (synopsis "ASDF WCS Schemas")
     (description
      "This package provides ASDF schemas for validating World Coordinate
System (WCS) tags.  Users should not need to install this directly; instead,
install an implementation package such as gwcs.")
     (license license:bsd-3))))

(define-public python-asdf-zarr
  (package
    (name "python-asdf-zarr")
    (version "0.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_zarr" version))
       (sha256
        (base32 "0xddz4hnsypyvqxhi43alaqh2vl1ripcl4p63qn6dk2v90lra8c0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-asdf-3
           python-fsspec
           python-zarr))
    (home-page "https://github.com/asdf-format/asdf-zarr")
    (synopsis "Asdf extension to support Zarr arrays")
    (description
     "This package includes an extension for the Python library asdf to add
support for reading and writing chunked
@url{https://zarr.readthedocs.io/en/stable/,Zarr} arrays, a file storage
format for chunked, compressed, N-dimensional arrays based on an open-source
specification.")
    (license license:bsd-3)))

(define-public python-astral
  (package
    (name "python-astral")
    (version "3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astral" version))
       (sha256
        (base32 "121xag65rmv6pszbi3d206yz3jfwmpkf0jxjrxrd2scy5r0knz4v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; XXX: Disable tests which require newer version of python-pytz.
      ;; No time zone found with key Pacific/Auckland
      #~(list "-k" (string-append
                    "not test_TimezoneLookup"
                    " and not test_Sun"
                    " and not test_Dawn"
                    " and not test_Sunrise"
                    " and not test_SolarNoon"
                    " and not test_Dusk"
                    " and not test_Sunset"
                    " and not test_SolarElevation"
                    " and not test_SolarAzimuth"
                    " and not test_TimeAtAltitude"
                    " and not test_MoonNoDate"
                    " and not test_lookup"
                    " and not test_tzinfo"
                    " and not test_australia"
                    " and not test_adak"
                    " and not test_australia"
                    " and not test_Elevation_NonNaive"
                    " and not test_Wellington"
                    " and not test_Sun_Local_tzinfo"
                    " and not test_Sun_Local_str"
                    " and not test_SolarZenith_London"
                    " and not test_SolarZenith_Riyadh"
                    " and not test_moonrise_utc"
                    " and not test_moonrise_wellington"
                    " and not test_moonset_wellington"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-freezegun
           python-poetry-core
           python-pytest
           python-setuptools-scm))
    (propagated-inputs
     (list python-pytest python-pytz))
    (home-page "https://github.com/sffjunkie/astral")
    (synopsis "Calculations for the position of the sun and moon")
    (description "Astral is a Python module that calculates times for various
positions of the sun: dawn, sunrise, solar noon, sunset, dusk, solar
elevation, solar azimuth, rahukaalam, and the phases of the moon.")
    (license license:asl2.0)))

(define-public python-astroalign
  (package
    (name "python-astroalign")
    (version "2.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quatrope/astroalign")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jcnv0gll0p7aqykmn9qffyzfq4gm3py5g20v6xxwf91vr85xrl3"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-astropy-minimal
           python-ccdproc
           python-pillow
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-numpy
           python-scikit-image
           python-scipy
           python-sep))
    (home-page "https://astroalign.readthedocs.io/")
    (synopsis "Astrometric Alignment of Images")
    (description
     "ASTROALIGN is a python module that will try to align two stellar
astronomical images, especially when there is no WCS information available.")
    (license license:expat)))

(define-public python-astrocut
  (package
    (name "python-astrocut")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astrocut" version))
       (sha256
        (base32 "1i8cpghk31cds9ipgap2ffws7jqy0smgk6w6kihxwpcw34jkr8h4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; Tests requiring network access.
                    (list "not index.rst"
                          "test_cube_cut_from_footprint"
                          "test_fits_cut[SPOC]"
                          "test_fits_cut[TICA]"
                          "test_fits_cutout_cloud"
                          "test_multithreading"
                          "test_s3_cube_cut"
                          "test_tess_cube_cutout_s3"
                          "test_tess_cube_cutout_threads"
                          "test_tess_footprint_cutout[SPOC]"
                          "test_tess_footprint_cutout[TICA]"
                          "test_tess_footprint_cutout_all_sequences"
                          "test_tess_footprint_cutout_invalid_sequence"
                          "test_tess_footprint_cutout_multi_sequence"
                          "test_tess_footprint_cutout_outside_coords"
                          "test_tess_footprint_cutout_write_as_tpf"
                          ;; Some NumPy compatability errors during tests.
                          "test_get_cutout_limits"
                          "test_get_cutout_wcs"
                          ;; botocore.exceptions.EndpointConnectionError:
                          ;; Could not connect to the endpoint URL:
                          ;; "https://stpubdata.s3.amazonaws.com/tess/public/\
                          ;; footprints/tess_ffi_footprint_cache.json"
                          "test_tess_footprint_cutout")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; TODO: Report upstream: ModuleNotFoundError: No module named
          ;; 'packagename'.
          (add-after 'unpack 'fix-setup.cfg
            (lambda _
              (substitute* "setup.cfg"
                (("console_scripts =") "")
                (("astropy-package-template-example.*") "")))))))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-astroquery
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-cachetools
           python-fsspec
           python-gwcs
           python-pillow
           python-requests
           python-roman-datamodels
           python-s3fs
           python-s3path
           python-scipy
           python-spherical-geometry))
    (home-page "https://astrocut.readthedocs.io")
    (synopsis "Cutout tools for astronomical images")
    (description
     "Astrocut provides tools for making cutouts from sets of astronomical
images with shared footprints.  It is under active development.

Three main areas of functionality are included:
@itemize
@item solving the specific problem of creating image cutouts from sectors of
@acronym{Transiting Exoplanet Survey Satellite, TESS} full-frame images
@item general fits file cutouts including from single images and sets of
images with the shared WCS/pixel scale
@item cutout post-processing functionality, including centering cutouts along
a path (for moving targets) and combining cutouts
@end itemize")
    (license license:bsd-3)))

(define-public python-astrodata
  (package
    (name "python-astrodata")
    (version "2.10.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/GeminiDRSoftware/astrodata")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "141qymnirjlrbjzl26wx2dvrr0glva8q74cwxi575a29aa0byrhv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 985 passed, 7 skipped, 58 deselected
      #:test-flags
      ;; Tests depend on unpackaged DRAGON's module "gemini_instruments".
      #~(list "--ignore=tests/integration/"
              ;; Access to https://archive.gemini.edu/file is required to
              ;; download test data.
              "--deselect=tests/unit/test_provenance.py::test_convert_provhistory"
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/unit/"
                                                    "test_fits.py::"
                                                    test))
                      (list "test_attributes"
                            "test_can_add_and_del_extension"
                            "test_copy"
                            "test_crop"
                            "test_crop_ext"
                            "test_do_arith_and_retain_features"
                            "test_extver3"
                            "test_from_hdulist"
                            "test_header"
                            "test_header_collection"
                            "test_paths"
                            "test_phu"
                            "test_read_no_extensions"
                            "test_set_a_keyword_on_phu_deprecated"
                            "test_slice"
                            "test_slice_data"
                            "test_slice_multiple"
                            "test_slice_single_element"))
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/unit/"
                                                    "test_object_construction.py::"
                                                    test))
                      (list "test_append_array_to_extension_with_arbitrary_name"
                            "test_append_array_to_extension_with_name_sci"
                            "test_append_array_to_root_no_name"
                            "test_append_array_to_root_with_arbitrary_name"
                            "test_append_array_to_root_with_name_sci"
                            "test_append_dq_var"
                            "test_append_nddata_to_root_no_name"
                            "test_append_nddata_to_root_with_arbitrary_name"
                            "test_append_non_single_slice"
                            "test_append_single_slice"
                            "test_append_slice_to_extension"
                            "test_append_whole_instance"
                            "test_can_read_data"
                            "test_delete_arbitrary_attribute_from_ad"
                            "test_delete_named_associated_extension"))
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/unit/"
                                                    "test_testing.py::"
                                                    test))
                      (list "test_ADCompare_header_matching"
                            "test_ADCompare_header_matching_flip"
                            "test_ADCompare_init"
                            "test_ADCompare_missing_refcat"
                            "test_ADCompare_numext"
                            "test_ADCompare_run_comparison"
                            "test_ADCompare_wcs"
                            "test_ad_compare"
                            "test_download_file_path_subpath"
                            "test_download_from_archive_None_sub_path"
                            "test_download_memory_leaks"
                            "test_warning_if_no_cache_path"))

              #$@(map (lambda (test) (string-append "--deselect="
                                                    "tests/unit/"
                                                    "test_wcs.py::"
                                                    test))
                      (list "test_gwcs_creation"
                            "test_loglinear_axis"
                            "test_reading_and_writing_sliced_image"
                            "test_remove_unused_world_axis"))
              ;; assert 1 == 0
              "-k" (string-append "not test_script_executes[script0-]"
                                  " and not test_script_executes[script1-]"))))
    (native-inputs
     (list python-poetry-core
           python-objgraph
           python-pytest
           python-pytest-doctestplus))
    (propagated-inputs
     (list python-asdf-3
           python-astropy-6
           python-gwcs-0.21
           python-jsonschema
           python-numpy-1))
    (home-page "https://github.com/GeminiDRSoftware/astrodata")
    (synopsis "Managing astronomical data through a uniform interface")
    (description
     "@code{astrodata} is a package for managing astronomical data through a
uniform interface.  It is designed to be used with the
@url{https://www.astropy.org/, Astropy package}.  @code{astrodata} was created
for use as part of the @url{https://github.com/GeminiDRSoftware/DRAGONS,
DRAGONS} data reduction pipeline, but it is now implemented to be useful for
any astronomical data reduction or analysis project.

Unlike managing files using the @code{astropy.io.fits} package alone,
@code{astrodata} is designed to be extendible to any data format, and to
parse, respond to, and store metadata in a consistent, intentional way.  This
makes it especially useful for managing data from multiple instruments,
telescopes, and data generation utilities.")
    (license license:bsd-3)))

(define-public python-astrodendro
  (package
    (name "python-astrodendro")
    (version "0.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astrodendro" version))
       (sha256
        (base32 "0pav2rq5q0wyr38g6z8ai4z2iqqc9x04iwll158yvkvgnv352m0i"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-matplotlib
           python-numpy))
    (home-page "https://dendrograms.readthedocs.io/")
    (synopsis "Astronomical dendrograms computation")
    (description
     "This package provides an way to compute dendrograms of observed or
simulated Astronomical data in Python.")
    (license license:expat)))

(define-public python-astromartini
  (package
    (name "python-astromartini")
    (version "2.1.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/kyleaoman/martini")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y36jblj2ibr91v967nfzczbm7mlvhjpgbd519s5ybsxay27qj4g"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2529 passed, 11 skipped, 4 xfailed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-scipy
           python-tqdm
           ;; [optional]
           ;; python-eaglesqltools
           python-gizmo-analysis
           python-h5py
           ;; python-halo-analysis
           ;; python-hdecompose
           ;; python-pyread-eagle
           python-requests
           python-swiftgalaxy
           python-swiftsimio
           python-utilities-awetzel
           #;python-velociraptor))
    (home-page "https://github.com/kyleaoman/martini")
    (synopsis "Synthetic datacube creation from simulations")
    (description
     "MARTINI is a modular package for the creation of synthetic resolved HI
line observations (data cubes) of smoothed-particle hydrodynamics simulations
of galaxies.  The various aspects of the mock-observing process are divided
logically into sub-modules handling the data cube, source, beam,
noise,spectral model and SPH kernel.  MARTINI is object-oriented: each
sub-module provides a class (or classes) which can be configured as desired.
For most sub-modules, base classes are provided to allow for straightforward
customization.  Instances of each sub-module class are given as parameters to
the Martini class; a mock observation is then constructed by calling a handful
of functions to execute the desired steps in the mock-observing process.")
    (license license:gpl3)))

(define-public python-astroml
  (package
    (name "python-astroml")
    (version "1.0.2.post1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "astroML" version))
              (sha256
               (base32
                "14g2mcd5qdr3nn7icvjs84bjvx17l9glx81sbbna6v53i1x8l625"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      '(list "--ignore-glob=examples/*"
        ;; Disable tests which are not compatible with Astropy 6.0.0, see
        ;; <https://github.com/astroML/astroML/issues/273>.
        "--ignore=astroML/density_estimation/tests/test_bayesian_blocks.py"
        "--ignore=astroML/density_estimation/tests/test_bayesian_blocks.py"
        "--ignore=astroML/density_estimation/tests/test_hist_binwidth.py"
        "--ignore=astroML/density_estimation/tests/test_hist_binwidth.py"
        ;; Disalbe tests with NumPy, see
        ;; <https://github.com/astroML/astroML/issues/281>.
        "--ignore=astroML/tests/test_resample.py"
        "-k" (string-append "not astroML.density_estimation.empirical.EmpiricalDistribution"
                            " and not astroML.utils.utils.log_multivariate_gaussian"))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             (substitute* "setup.cfg"
               ;; Do not error out on deprecations
               (("	error::DeprecationWarning.*") "")
               ;; Do not test examples
               (("testspaths = astroML doc examples")
                "testspaths = astroML"))))
         (add-after 'unpack 'matplotlib-compatibility
           (lambda _
             (substitute* "astroML/plotting/tools.py"
               (("^( *)ax.(lines|patches|tables|artists|images).clear.*" _ indent type)
                (string-append indent "for art in ax." type ":\n"
                               indent "  art.remove()\n")))))
         ;; See commit e1c779de1f0ce4cb499dbda6c23d14f76b98e430
         (add-after 'unpack 'scipy-compatibility
           (lambda _
             (substitute* "astroML/dimensionality/iterative_pca.py"
               (("sym_pos=True") "assume_a=\"pos\""))))
         (add-before 'check 'pre-check
           ;; Some tests need this
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pytest-astropy-header
           python-pytest-doctestplus
           python-pytest-remotedata
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scikit-learn
           python-scipy))
    (home-page "https://astroml.org")
    (synopsis "Tools for machine learning and data mining in astronomy")
    (description "This package provides tools for machine learning and data
mining in astronomy.")
    (license license:bsd-2)))

(define-public python-astroplan
  (package
    (name "python-astroplan")
    (version "0.10.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroplan" version))
       (sha256
        (base32 "0nb97fz0mlypdlvs09wyh0z7mxw0d6aqqkd9yfzhlqz1fwrprn9r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 44 passed, 31 deselected, 135 warnings
      #:test-flags
      #~(list "--ignore=doc/"
              "--ignore=astroplan/constraints.py"
              "--ignore=astroplan/target.py"
              ;; Network access is required to download test data.
              "--ignore=astroplan/tests/test_scheduling.py"
              "--deselect=astroplan/plots/tests/test_sky.py::test_timezone"
              "-k" (string-join
                    (list "not test_at_night_basic"
                          "test_FixedTarget_from_name"
                          "test_altitude_constraint"
                          "test_caches_shapes"
                          "test_compare_airmass_constraint_and_observer"
                          "test_compare_altitude_constraint_and_observer"
                          "test_docs_example"
                          "test_eclipses"
                          "test_eq_observer"
                          "test_event_observable"
                          "test_galactic_plane_separation"
                          "test_get_skycoord"
                          "test_hash_observer"
                          "test_is_night"
                          "test_local_time_constraint_hawaii_tz"
                          "test_local_time_constraint_utc"
                          "test_moon_illumination"
                          "test_moon_separation"
                          "test_observability_table"
                          "test_observer_lon_lat_el"
                          "test_regression_airmass_141"
                          "test_regression_shapes"
                          "test_sun_separation"
                          "test_tonight")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-ephem
           python-pytest-astropy
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm
           tzdata-for-tests))
    (propagated-inputs
     (list python-astropy
           python-astroquery
           python-matplotlib
           python-numpy
           python-pytz))
    (home-page "https://github.com/astropy/astroplan")
    (synopsis "Observation planning package for astronomers")
    (description
     "This package provides a flexible toolbox for observation planning and
scheduling.  When complete, the goal is to be easy for Python beginners and new
observers to to pick up, but powerful enough for observatories preparing nightly
and long-term schedules.

Features:
@itemize
@item calculate rise/set/meridian transit times, alt/az positions for targets at
observatories anywhere on Earth
@item built-in plotting convenience functions for standard observation planning
plots (airmass, parallactic angle, sky maps)
@item determining observability of sets of targets given an arbitrary set of
constraints (i.e., altitude, airmass, moon separation/illumination, etc.)
@end itemize")
      (license license:bsd-3)))

(define-public python-astropy
  (package
    (name "python-astropy")
    (version "7.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy" version))
       (sha256
        (base32 "1y5hip9pkndx13yrq9ssw4gcmr6hz65ld11l25q4zhz20l08y4kd"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove Python bundles.
           (with-directory-excursion "astropy/extern"
             (for-each delete-file-recursively '("ply" "configobj")))
           ;; Remove cextern bundles. Check bundled versions against available
           ;; in Guix in the future update of astropy.
           ;; Linking against an external cfitsio version has been removed,
           ;; see https://github.com/astropy/astropy/pull/14311
           (with-directory-excursion "cextern"
             (for-each delete-file-recursively '("expat" "wcslib")))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 29347 passed, 373 skipped, 233 xfailed, 32 warnings
      #:test-flags
      #~(list "--pyargs" "astropy"
              ;; XXX: Tests are not thread save when they are more than 8.
              "--numprocesses" (number->string (min 8 (parallel-job-count)))
              "-k" (string-append
                    ;; Fails with  assert 13 == 1.
                    "not test_skip_meta"
                    ;; Failed: DID NOT WARN. No warnings of type (<class
                    ;; 'ResourceWarning'>,) were emitted.
                    " and not test_ephemeris_local_file_not_ephemeris"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'install-coordinates/sites.json
            (lambda _
              (copy-file
               (string-append #$(this-package-native-input
                                 "specification-astropy-data")
                              "/share/coordinates/sites.json")
               "astropy/coordinates/data/sites.json")))
          (add-after 'unpack 'preparations
            (lambda _
              ;; Use our own libraries in place of bundles.
              (setenv "ASTROPY_USE_SYSTEM_ALL" "1")
              ;; Relax xfail tests.
              (substitute* "pyproject.toml"
                (("xfail_strict = true") "xfail_strict = false"))
              ;; Replace reference to external ply.
              (substitute* (find-files "." "\\.py$")
                (("astropy.extern.ply") "ply"))
              ;; Replace reference to external configobj.
              (substitute* "astropy/config/configuration.py"
                (("from astropy.extern.configobj ") ""))))
          ;; This file is opened in both install and check phases.
          (add-before 'install 'writable-compiler
            (lambda _
              (make-file-writable "astropy/_compiler.c")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (setenv "OMP_NUM_THREADS" "1")
                ;; Step out of the source directory to avoid interference; we
                ;; want to run the installed code with extensions etc.
                (with-directory-excursion #$output
                  (apply invoke "pytest" "-vv" test-flags))))))))
    (native-inputs
     (list nss-certs-for-test
           pkg-config
           python-cython
           python-extension-helpers
           python-objgraph
           python-pytest
           python-pytest-astropy
           python-pytest-astropy-header
           python-pytest-doctestplus
           python-pytest-xdist
           python-setuptools-scm
           python-sgp4
           python-skyfield
           python-threadpoolctl
           specification-astropy-data))
    (inputs
     (list expat
           wcslib))
    (propagated-inputs
     (list python-astropy-iers-data
           python-configobj      ;to replace custom module
           python-numpy
           python-packaging
           python-ply            ;to replace custom module
           python-pyerfa
           python-pyyaml
           ;; [optional]
           python-matplotlib
           python-scipy
           ;; python-ipydatagrid         ;no packaged
           python-ipykernel
           python-ipython
           python-ipywidgets
           python-jupyter-core
           python-pandas
           python-asdf
           python-asdf-astropy
           python-beautifulsoup4
           python-bleach
           python-bottleneck
           python-certifi
           python-dask
           python-fsspec
           python-h5py
           python-html5lib
           python-jplephem
           python-mpmath
           python-pandas
           python-pyarrow
           python-pytz
           python-s3fs
           python-sortedcontainers
           python-uncompresspy))
    (home-page "https://www.astropy.org/")
    (synopsis "Core package for Astronomy in Python")
    (description
     "Astropy is a single core package for Astronomy in Python.  It contains
much of the core functionality and some common tools needed for performing
astronomy and astrophysics.")
    (license license:bsd-3)))

(define-public python-astropy-6
  (package
    (inherit python-astropy)
    (name "python-astropy")
    (version "6.1.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy" version))
       (sha256
        (base32 "1vspagb4vbmkl6fm3mr78577dgdq992ggwkd5qawpdh6cccaq1d4"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (with-directory-excursion "astropy/extern"
              (for-each delete-file-recursively '("ply" "configobj")))
            (with-directory-excursion "cextern"
              (for-each delete-file-recursively '("expat" "wcslib")))))))
    ;; tests: 28419 passed, 316 skipped, 233 xfailed, 25 warnings
    (arguments
     (substitute-keyword-arguments
         (package-arguments python-astropy)
       ((#:test-flags test-flags #~(list))
        #~(list "--pyargs" "astropy"
                "--numprocesses" (number->string (min 8 (parallel-job-count)))
                "-k" (string-join
                      (list "not test_skip_meta"
                            "test_all_included"
                            "test_basic_testing_completeness"
                            "test_ephemeris_local_file_not_ephemeris"
                            "test_non_linear_lsq_fitter_with_weights[LMLSQFitter]"
                            "test_testing_completeness")
                      " and not ")))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-astropy)
       (replace "python-jplephem" python-jplephem-2.22)))))

(define-public python-astropy-healpix
  (package
    (name "python-astropy-healpix")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy_healpix" version))
       (sha256
        (base32 "1r362081aj5jqxshcxw0bpzn4qvqnra52k94ghskpv1n5bqisrq3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion #$output
                  (apply invoke "pytest" "-vv" test-flags))))))))
    (native-inputs
     (list python-extension-helpers
           python-hypothesis
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy python-numpy))
    (home-page "https://github.com/astropy/astropy-healpix")
    (synopsis "HEALPix for Astropy")
    (description "This package provides HEALPix to the Astropy project.")
    (license license:bsd-3)))

(define-public python-astropy-iers-data
  (package
    (name "python-astropy-iers-data")
    (version "0.2025.12.15.0.40.51")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy_iers_data" version))
       (sha256
        (base32 "14d5g6ffni65y7n3r1xjddglrgb0ffz55wf9x0mdxlkhqvzygwyr"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests
    (native-inputs
     (list python-hatch-vcs
           python-hatchling))
    (home-page "https://docs.astropy.org/en/latest/utils/iers.html")
    (synopsis "IERS Earth Rotation and Leap Second tables for Astropy core")
    (description
     "The @code{iers} package provides access to the tables provided by the
@acronym{International Earth Rotation and Reference Systems, IERS} service, in
particular the
@url{https://www.iers.org/IERS/EN/DataProducts/EarthOrientationData/eop.html,
Earth Orientation data} allowing interpolation of published UT1-UTC and polar
motion values for given times.  The UT1-UTC values are used in
@url{https://docs.astropy.org/en/latest/time/index.html#astropy-time, Time and
Dates (astropy.time)} to provide UT1 values, and the polar motions are used in
@code{astropy.coordinates} to determine Earth orientation for
celestial-to-terrestrial coordinate transformations.")
    (license license:bsd-3)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-astropy-minimal
  (package/inherit python-astropy
    (name "python-astropy-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments python-astropy)
       ((#:tests? _ #t) #f)))
    (native-inputs
     (list nss-certs-for-test
           pkg-config
           python-cython
           python-extension-helpers
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy-iers-data
           python-configobj      ;to replace custom module
           python-numpy
           python-packaging
           python-ply            ;to replace custom module
           python-pyerfa
           python-pyyaml))))

(define-public python-astroquery
  (package
    (name "python-astroquery")
    (version "0.4.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroquery" version))
       (sha256
        (base32 "0crb0h39bs389y8x3vbl6kczbqwklnn9pkfmfgkhgyn7vndm4dsm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "astroquery"
              "-m" "not remote_data"
              ;; Some tests failed with parallel run, see
              ;; <https://github.com/astropy/astroquery/issues/2968>.
              ;; "-n" "auto"
              "-k" (string-join
                    (list
                     ;; Failed: DID NOT RAISE <class
                     ;; 'astropy.utils.exceptions.AstropyDeprecationWarning'>
                     "not test_raises_deprecation_warning"
                     ;; E       fixture 'tmp_cwd' not found
                     "test_download_cache"
                     "test_download_local"
                     "test_download_table"
                     "test_read_uncompressed")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                ;; Some tests require write access to $HOME.
                (setenv "HOME" "/tmp")
                ;; Step out of the source directory to avoid interference;
                ;; we want to run the installed code with extensions etc.
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-v" test-flags))))))))
    (native-inputs
     (list nss-certs-for-test
           python-matplotlib
           python-pytest-astropy
           python-pytest-dependency
           python-pytest-doctestplus
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-astropy-healpix
           python-beautifulsoup4
           python-boto3
           python-html5lib
           python-keyring
           ;; python-mocpy : Not packed yet, optional and Rust is required
           python-numpy
           python-pyvo
           python-regions
           python-requests))
    (home-page "https://astroquery.readthedocs.io/en/latest/index.html")
    (synopsis "Access online astronomical data resources")
    (description
     "Astroquery is a package that contains a collection of tools to access
online Astronomical data.  Each web service has its own sub-package.")
    (license license:bsd-3)))

(define-public python-astroscrappy
  (package
    (name "python-astroscrappy")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroscrappy" version))
       (sha256
        (base32 "1jpj7df26w31brbkhc1xrqaz2ayz9w3s88ia5vq86als7sfhfs5q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (with-directory-excursion #$output
                (apply invoke "pytest" "-vv" test-flags))))
          (add-before 'check 'post-check
            (lambda _
              (for-each delete-file-recursively
                        (find-files #$output "__pycache__" #:directories? #t)))))))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-astropy
           python-scipy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy))
    (home-page "https://github.com/astropy/astroscrappy")
    (synopsis "Speedy Cosmic Ray Annihilation Package in Python")
    (description
     "Astro-SCRAPPY is designed to detect cosmic rays in images (numpy
arrays), based on Pieter van Dokkum's L.A.Cosmic algorithm.  Much of this was
originally adapted from cosmics.py written by Malte Tewes.  This is designed to
be as fast as possible so some of the readability has been sacrificed,
specifically in the C code.")
    (license license:bsd-3)))

(define-public python-baseband
  (package
    (name "python-baseband")
    (version "4.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "baseband" version))
       (sha256
        (base32 "1yzgzdf8ri4blcpmnz5f3dv6in076vgbhbcqp37kjidlp4f4w05r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: Tests are broken during collection phase, see
      ;; <https://github.com/mhvk/baseband/issues/539>.
      #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy))
    (home-page "https://github.com/mhvk/baseband")
    (synopsis "Radio baseband I/O")
    (description
     "Baseband is a package for reading and writing @acronym{VLBI,
Very-long-baseline interferometry} and other radio baseband files, with the
aim of simplifying and streamlining data conversion and standardization.")
    (license license:gpl3+)))

(define-public python-bayesicfitting
  (package
    (name "python-bayesicfitting")
    (version "3.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dokester/BayesicFitting")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l4yq907a61js7pmi4zi2gg3nq479n0f21ycmjgk3rkc4ixrislz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: To run full tests suite is quite expansive, select the one which
      ;; is mentioned in <BayesicFitting/test/testall.csh>.
      #:test-flags #~(list "-m" "unittest"
                           "discover"
                           "-p" "TestUserModel.py"
                           "--top-level-directory" ".")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion "BayesicFitting/test"
                  (apply invoke "python" test-flags))))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-future
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://dokester.github.io/BayesicFitting/")
    (synopsis "Astronimical Bayesian fitting")
    (description
     "@code{BayesicFitting} is a package for model fitting and Bayesian
evidence calculation, it is a Python version of the the fitter classes in
@acronym{HCSS, Herschel Common Science System}.  HCSS was the all encompassing
software system for the operations and analysis of the ESA satelite Herschel.")
    (license license:gpl3+)))

(define-public python-calcos
  (package
    (name "python-calcos")
    (version "3.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "calcos" version))
       (sha256
        (base32 "0mfyinbbrps1ryjnqkjj7h7117clx762q7jvl7raycf1wj0g2zs0"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-ci-watson
           python-numpy
           python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-scipy
           python-stsci-tools))
    (home-page "https://hst-docs.stsci.edu/cosdhb/chapter-3-cos-calibration")
    (synopsis "Calibration software for Cosmic Origins Spectrograph")
    (description
     "This packages provides a calibration software for @acronym{COS, Cosmic
Origins Spectrograph}.")
    (license license:bsd-3)))

(define-public python-camb
  (package
    (name "python-camb")
    (version "1.6.5")
    (source
     (origin
       (method git-fetch)       ;XXX: PyPI archive bundles forutils.
       (uri (git-reference
              (url "https://github.com/cmbant/CAMB")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1axqgxvp6id9d600zn6529ypi0ygf6qvd2p0z99dr90prcrr3qva"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-backend #~'unittest
      #:test-flags #~(list "camb.tests.camb_test")
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: It may be set via FORUTILSPATH environemnt variable but
          ;; build fails with error:
          ;; mkdir: cannot create directory ‘Releaselib’: Read-only file system
          (add-after 'unpack 'copy-forutils
            (lambda _
              ;; It's a git sumbodule to <https://github.com/cmbant/forutils>.
              (copy-recursively
               #+(package-source (this-package-native-input "fortran-forutils"))
               "forutils"))))))
    (native-inputs
     (list fortran-forutils
           gfortran
           python-packaging
           python-setuptools
           which)) ; for fortran/Makefile
    (propagated-inputs
     (list python-numpy
           python-packaging
           python-scipy
           python-sympy))
    (home-page "https://camb.info/")
    (synopsis "Code for Anisotropies in the Microwave Background")
    (description
     "CAMB is a cosmology code for calculating cosmological observables,
including @acronym{CMB, Cosmic microwave background}, lensing, source count
and 21cm angular power spectra, matter power spectra, transfer functions and
background evolution.  The code is in Python, with numerical code implemented
in fast modern Fortran.")
    (license license:gpl3+)))

(define-public python-casa-formats-io
  (package
    (name "python-casa-formats-io")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "casa-formats-io" version))
              (sha256
               (base32
                "16qwr6yq86qgdb0lvnmfm5mn6g2d29b1vrmfv26v77kxm6szxr8h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 17 passed, 58 skipped, 1 deselected, 1 xfailed, 25 warnings
      #:test-flags
      #~(list "--pyargs" "casa_formats_io"
              ;; AttributeError: module 'numpy' has no attribute 'product'
              "-k" "not test_ms_tables[.]")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'remove-local-source
            (lambda _
              (delete-file-recursively "casa_formats_io"))))))
    (native-inputs
     (list python-pytest
           python-pytest-openfiles
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-click
           python-dask
           python-numpy))
    (home-page "https://casa-formats-io.readthedocs.io/")
    (synopsis "Dask-based reader for CASA data")
    (description
     "The @code{casa-formats-io} package is a small package which implements
functionality to read data stored in @acronym{CASA, Common Astronomy Software
Applications} formats (such as @file{.image} datasets).  This implementation
is independent of and does not use @code{casacore}.")
    (license license:lgpl2.0)))

(define-public python-casacore
  (package
    (name "python-casacore")
    (version "3.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "python_casacore" version))
       (sha256
        (base32 "1hvmlzimkz1v65zmhwg6c6vi437jjymbdd2fjjfsph3kp860ckkc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list boost
           cmake-minimal
           python-pytest
           python-scikit-build-core
           python-setuptools-scm))
    (inputs
     (list casacore
           cfitsio
           hdf5
           openblas
           wcslib))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://casacore.github.io/python-casacore")
    (synopsis "Python wrapper for Casacore")
    (description
     "This package provides a wrapper around @code{casacore}, the radio
astronomy library.")
    (license license:gpl3+)))

(define-public python-ccdproc
  (package
    (name "python-ccdproc")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ccdproc" version))
       (sha256
        (base32 "1ymx9ssapv52m6flfvlhgjnvwmqdgv1qimyjvci8w66k9j4446q5"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-memory-profiler
           python-pytest-astropy))
    (propagated-inputs
     (list python-astropy
           python-astroscrappy
           python-numpy
           python-reproject
           python-scikit-image
           python-scipy))
    (home-page "http://ccdproc.readthedocs.io/")
    (synopsis "Basic data reductions of CCD images")
    (description "The ccdproc package provides many of the necessary tools for
processing of CCD images built on a framework to provide error propagation and
bad pixel tracking throughout the reduction process.")
    (license license:bsd-3)))

(define-public python-cdflib
  (package
    (name "python-cdflib")
    (version "1.3.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lasp/cdflib")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ky54sg21g9mcgg8cimkb5bpmz4kl3yfgd1gk99s687lj1qm9v31"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 71 passed, 26 skipped, 7 warnings
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-pytest-config
            (lambda _
              (substitute* "pyproject.toml"
                ((" --cov=cdflib --cov-report=xml") ""))))
          (add-before 'build 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'set-home-env
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (native-inputs
     (list python-astropy-minimal
           python-pytest
           python-pytest-remotedata
           python-setuptools
           python-setuptools-scm
           python-xarray))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/lasp/cdflib")
    (synopsis "Python library to deal with NASA's CDF astronmical data format")
    (description
     "This package provides a Python @acronym{CDF, Computable Document Format}
reader toolkit.

It provides the following functionality:
@itemize
@item Ability to read variables and attributes from CDF files
@item Writes CDF version 3 files
@item Can convert between CDF time types (EPOCH/EPOCH16/TT2000) to other common
time formats
@item Can convert CDF files into XArray Dataset objects and vice versa,
attempting to maintain ISTP compliance
@end itemize")
    (license license:expat)))

(define-public python-cesium
  (package
    (name "python-cesium")
    (version "0.12.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cesium" version))
              (sha256
               (base32
                "0kfz2fgi2hnl1a9pz8nk0cr1vyk4c7cg1vmkpnwpgy7n44fkbxwa"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; The installed test files contain the /gnu/store location, not the
          ;; location of the discovered test files from the build directory.
          ;; The test framework doesn't like this.  The easiest way around
          ;; this mismatch is to jump to the output directory.
          (add-before 'check 'check-chdir
            (lambda _ (chdir #$output))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-cloudpickle
           python-dask
           python-gatspy
           python-joblib
           python-numpy
           python-pandas
           python-scikit-learn
           python-scipy
           python-toolz))
    (home-page "https://cesium-ml.org/")
    (synopsis "Library for time-series feature extraction and processing")
    (description
     "Cesium is an end-to-end machine learning platform for time-series, from
calculation of features to model-building to predictions.  Cesium has two main
components - a Python library, and a web application platform that allows
interactive exploration of machine learning pipelines.  Take control over the
workflow in a Python terminal or Jupyter notebook with the Cesium library, or
upload your time-series files, select your machine learning model, and watch
Cesium do feature extraction and evaluation right in your browser with the web
application.")
    (license license:bsd-3)))

(define-public python-ci-watson
  (package
    (name "python-ci-watson")
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ci_watson" version))
       (sha256
        (base32 "0dlys7yr11c59zl0smy2hb3bw5r6vyrmx8s97f1942i7zjnyb1zx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-pypojrect-toml
            (lambda _
              (substitute* "setup.cfg"
                ;; ImportError: Error importing plugin " no:legacypath": No
                ;; module named ' no:legacypath'
                (("-p no:legacypath") "")))))))
    (native-inputs
     (list python-pytest-astropy-header
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-colorama
           python-crds
           python-readchar
           python-requests))
    (home-page "https://github.com/spacetelescope/ci_watson")
    (synopsis "Helper functions for STScI software")
    (description
     "This package contains a helper functionality to test ROMAN and JWST.")
    (license license:bsd-3)))

(define-public python-cmyt
  (package
    (name "python-cmyt")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch) 
       (uri (git-reference
              (url "https://github.com/yt-project/cmyt")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d257xlgxc06x47k07xn5ml2kjqzc7dgjal4bl9x2w6b90xn0pm1"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-colorspacious
           python-pytest
           python-pytest-mpl
           python-hatchling))
    (propagated-inputs
     (list python-matplotlib
           python-numpy))
    (home-page "https://yt-project.org/")
    (synopsis "Matplotlib colormaps from the yt project")
    (description
     "This package provides a range of colormaps designed for scientific use
with Matplotlib.  It includes perceptually uniform sequential colormaps such
as @code{abre}, @code{dusk}, @code{kepl}, and @code{octarine}, as well as
monochromatic sequential colormaps like @code{blue}, @code{green}, and
@code{red}, and others (@code{algae}, @code{pastel}, and @code{xray}).")
    (license license:bsd-3)))

(define-public python-cobaya
  (package
    (name "python-cobaya")
    (version "3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/CobayaSampler/cobaya")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jws7kb3578mgbimxxanbhzkrxpmcby0jqxq9zlf04i5ilqn4s6m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 76 passed, 3 skipped, 93 xfailed, 2 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              "--skip-not-installed"
              "-k" "not test_planck_NPIPE_p_CamSpec_camb and not test_grid")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (setenv "COBAYA_PACKAGES_PATH" "/tmp"))))))
    (native-inputs
     (list python-camb
           ;; python-classy
           python-flaky
           python-pytest
           python-pytest-xdist
           python-setuptools))
    (propagated-inputs
     (list python-dill
           python-fuzzywuzzy
           python-getdist
           python-numpy
           python-packaging
           python-pandas
           python-portalocker
           python-py-bobyqa
           python-pyyaml
           python-requests
           python-scipy
           python-tqdm
           python-typing-extensions))
    (home-page "https://github.com/CobayaSampler/cobaya")
    (synopsis "Code for Bayesian analysis in Cosmology")
    (description
     "@acronym{Cobaya, COde for BAYesian Analysis}, and Spanish for Guinea
Pig) is a framework for sampling and statistical modelling: it allows you to
explore an arbitrary prior or posterior using a range of Monte Carlo
samplers (including the advanced MCMC sampler from CosmoMC, and the advanced
nested sampler PolyChord). The results of the sampling can be analysed with
GetDist. It supports MPI parallelization (and very soon HPC containerization
with Docker/Shifter and Singularity).")
    (license license:lgpl3+)))

(define-public python-colossus
  ;; There is no source distribution in PyPI and no version tags, use the
  ;; latest commit from the master HEAD.
  (let ((commit "4c9d0e54969e531c1b9a7120b0e752c77b1f404a")
        (revision "0"))
    (package
      (name "python-colossus")
      (version (git-version "1.3.10" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://bitbucket.org/bdiemer/colossus")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1zvdpxf82fp3r8nfh65gzlfkacpagfdhpbac65p4bjfnpvm1m3cj"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        ;; tests: 105 passed, 7 warnings
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'check 'pre-check
              (lambda _
                (setenv "HOME" "/tmp"))))))
      (native-inputs
       (list python-astropy-minimal
             python-jsonpickle
             python-lenstronomy-minimal
             python-pytest
             python-scikit-image
             python-setuptools))
      (propagated-inputs
       (list python-numpy
             python-scipy))
      (home-page "https://bitbucket.org/bdiemer/colossus")
      (synopsis "Cosmology, halo, and large-scale structure tools")
      (description
       "@acronym{Colossus, COsmology haLO and large-Scale StrUcture toolS} is a
Python toolkit for calculations pertaining to cosmology, the large-scale
structure of the universe, and the properties of dark matter halos.")
      (license license:expat))))

(define-public python-coolest
  (package
    (name "python-coolest")
    (version "0.1.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/aymgal/COOLEST")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mdj5c91w1p2ch1y80y0x1y3ky30b95lzlscz2b6hl7jcf3n6jfb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 120 passed, 81 warnings
      #:test-flags
      ;; Cycle with python-lenstronomy.
      #~(list "--ignore=test/api/profiles/light_test.py"
              "--ignore=test/api/profiles/mass_test.py")))
    (native-inputs
     (list python-pytest
           python-scikit-image
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-getdist
           python-jsonpickle
           python-numpy
           python-pandas
           python-scipy))
    (home-page "https://github.com/aymgal/COOLEST")
    (synopsis "Strong Gravitational Lensing Analyses")
    (description
     "The COde-independent Organized LEns STandard (COOLEST) defines a set of
conventions to be shared across the strong lensing community, in order to
consistently store, share and improve lens modeling analyses.  In short, this
project provides tools to manipulate lens models as a single, human-readable
JSON template file alongside Python routines for visualizing and comparing
lens models possibly obtained from different modeling codes.")
    (license  license:gpl3)))

(define-public python-corsikaio
  (package
    (name "python-corsikaio")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cta-observatory/pycorsikaio")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rnp1myhilkcr7mnv6x3cmxqjn0adgmb89crszn536qphsisyc35"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 51 passed
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-pytest
           python-scipy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-zstandard))
    (home-page "https://github.com/cta-observatory/pycorsikaio")
    (synopsis "Reader for CORSIKA binary output files")
    (description
     "This package implements a reader for
@url{https://www.iap.kit.edu/corsika/, @acronym{CORSIKA, COsmic Ray
SImulations for KAscade}} binary output files using NumPy.")
    (license license:expat)))

(define-public python-cosmopy
  (package
    (name "python-cosmopy")
    (version "3.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cosmopy" version))
       (sha256
        (base32 "16xlg0vyzdx1h5h28y8az48nhiqvx43yd9s30g3rk9ywl4r1j6h8"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-click
           python-future
           python-numpy
           python-scipy))
    (home-page "https://github.com/lzkelley/cosmopy")
    (synopsis "Cosmological calculator in Python")
    (description
     "This package provides a CLI and Python module to quickly calculate
cosmological parameters e.g. redshift or luminosity-distance.")
    (license license:expat)))

(define-public python-costools
  (package
    (name "python-costools")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "costools" version))
       (sha256
        (base32 "0w0hgm847qsq5q30j5nshxwf7bdj503h6r1y3b6hvlf55gzlscnd"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-calcos
           python-numpy
           python-stsci-tools))
    (home-page "https://github.com/spacetelescope/costools")
    (synopsis "Tools for Cosmic Origins Spectrograph")
    (description
     "This package provides tools for @acronym{COS, Cosmic Origins
Spectrograph}.")
    (license license:bsd-3)))

(define-public python-crds
  (package
    (name "python-crds")
    (version "13.0.6")
    ;; Git: <https://github.com/spacetelescope/crds>
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "crds" version))
       (sha256
        (base32 "1i0si0z400grdwrkni8fcp58m8f8n2k18n319cwfdlasbw6r2pr5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 26 passed, 3 skipped
      #:test-flags
      ;; XXX: Tests require a complex set up and test data, try to run some
      ;; minimal portion of unit tests to persist package comparability during
      ;; updates, see:
      ;; <https://github.com/spacetelescope/crds/blob/13.0.6/TESTING>.
      #~(list #$@(map (lambda (file) (string-append "--ignore=" file))
                      ;; Network acces to <https://hst-crds.stsci.edu> or
                      ;; additional test data is required.
                      (list "test/bestrefs/test_bestrefs.py"
                            "test/bestrefs/test_special.py"
                            "test/bestrefs/test_table_effects.py"
                            "test/certify/test_certify.py"
                            "test/core/test_cmdline.py"
                            "test/core/test_heavy_client.py"
                            "test/core/test_reftypes.py"
                            "test/core/test_rmap.py"
                            "test/core/test_substitutions.py"
                            "test/misc/test_check_archive.py"
                            "test/misc/test_synphot.py"
                            "test/misc/test_uniqname.py"
                            "test/refactoring/test_checksum.py"
                            "test/refactoring/test_newcontext.py"
                            "test/refactoring/test_refactor.py"
                            "test/roman/test_roman.py"
                            "test/submit/test_submit.py"
                            "test/test_bad_files.py"
                            "test/test_build6.py"
                            "test/test_diff.py"
                            "test/test_list.py"
                            "test/test_matches.py"
                            "test/test_rowdiff.py"
                            "test/test_sync.py")))))
    (native-inputs
     (list python-mock
           python-pytest
           python-pytest-astropy
           python-pytest-doctestplus
           python-setuptools
           python-setuptools-scm
           python-stdatamodels-minimal
           python-stsynphot))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-filelock
           python-asdf
           python-requests
           python-parsley
           ;; [optional]
           python-beautifulsoup4
           ;; python-crds -> python-ci-watson -> python-jwst -> python-crds
           ;; python-jwst
           python-roman-datamodels
           python-stsynphot))
    (home-page "https://hst-crds.stsci.edu")
    (synopsis "Calibration Reference Data System for HST and JWST")
    (description
     "CRDS is a package used for working with astronomical reference files for
the HST and JWST telescopes.  CRDS is useful for performing various operations
on reference files or reference file assignment rules.  CRDS is used to
assign, check, and compare reference files and rules, and also to predict
those datasets which should potentially be reprocessed due to changes in
reference files or assignment rules.  CRDS has versioned rules which define
the assignment of references for each type and instrument configuration.  CRDS
has web sites corresponding to each project @url{http://hst-crds.stsci.edu} or
@url{https://jwst-crds.stsci.edu/} which record information about reference
files and provide related services.")
    (license license:bsd-3)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-crds-minimal
  (package/inherit python-crds
    (name "python-crds-minimal")
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-filelock
           python-asdf
           python-requests
           python-parsley))))

(define-public python-ctapipe
  (package
    (name "python-ctapipe")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ctapipe" version))
       (sha256
        (base32 "1gkkqbar3r816s8qks9r8ps710xny8v50iyp596j02f40nw6zknf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: The most of the tests require external data, check if it may be
      ;; packaged of some portion of unit test may be run without it.
      #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-docutils
           python-joblib
           python-numba
           python-numpy
           python-packaging
           python-psutil
           python-pyyaml
           python-requests
           python-scikit-learn
           python-scipy
           python-tables
           python-tqdm
           python-traitlets
           ;; [poptional]
           python-bokeh
           python-eventio
           python-iminuit
           python-matplotlib
           python-pyirf))
    (home-page "https://github.com/cta-observatory/ctapipe/")
    (synopsis "Low-level data processing pipeline software for CTAO")
    (description
     "Event reconstruction framework for Imaging Atmospheric Cherenkov
Telescopes developed for @acronym{CTAO, Cherenkov Telescope Array
Observatory}.")
    (license license:bsd-3)))

(define-public python-czml3
  (package
    (name "python-czml3")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              ;; Maintainer of the project has been changed, see
              ;; <https://github.com/poliastro/czml3/issues/112>.
              (url "https://github.com/Stoops-ML/czml3")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ra0m0c4y6k2v0lzi73xfvq26pcvsspimzz237fcs4mbg35hjd4z"))))
    (build-system pyproject-build-system)
    ;; tests: 229 passed, 3 xfailed, 1 warning
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-dateutil
           python-numpy
           python-pydantic
           python-strenum
           python-typing-extensions
           python-w3lib))
    (home-page "https://github.com/Stoops-ML/czml3")
    (synopsis "Python library to write CZML")
    (description
     "CZML3 is a Python library to write CZML, a JSON format for describing
a time-dynamic graphical scene, primarily for display in a web browser running
Cesium.")
    (license license:bsd-3)))

(define-public python-dkist
  (package
    (name "python-dkist")
    (version "1.16.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dkist" version))
       (sha256
        (base32 "1n0qr28linf72xri5l26m0dgciczk5nc60yysg19iijwfh7y0m1x"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 443 passed, 7 skipped
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; When python-pytest-benchmark is added: Benchmarks are
              ;; automatically disabled because xdist plugin is
              ;; active.Benchmarks cannot be performed reliably in a
              ;; parallelized environment.
              "--ignore=dkist/tests/test_benchmarks.py"
              ;; Network access is required.
              "--deselect=dkist/net/tests/test_client.py::test_fetch_with_headers")
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: It fails to check SunPy's optional inputs versions.
          (delete 'sanity-check)
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pydot
           python-pytest
           python-pytest-cov
           python-pytest-asdf-plugin
           python-pytest-doctestplus
           python-pytest-filter-subpackage
           python-pytest-httpserver
           python-pytest-lazy-fixtures
           python-pytest-xdist
           python-pytest-mock
           python-pytest-mpl
           python-pytest-remotedata
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-aiohttp
           python-asdf
           python-asdf-astropy
           python-asdf-coordinates-schemas
           python-asdf-standard
           python-asdf-transform-schemas
           python-asdf-wcs-schemas
           python-astropy
           python-dask
           python-globus-sdk
           python-gwcs
           python-matplotlib
           python-ndcube
           python-numpy
           python-packaging
           python-parfive
           python-platformdirs
           python-sunpy
           python-tqdm))
    (home-page "https://github.com/DKISTDC/dkist")
    (synopsis "Obtaining, processing and interacting with calibrated DKIST data")
    (description
     "The @acronym{DKIST, Daniel K. Inouye Solar Telescope} package aims to
help you search, obtain and use DKIST data as part of your Python software.")
    (license license:bsd-3)))

(define-public python-drizzle
  (package
    (name "python-drizzle")
    (version "2.1.1")
    (source
     (origin
       (method git-fetch)       ;no test data in PyPI archive
       (uri (git-reference
             (url "https://github.com/spacetelescope/drizzle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s2vydy3fp6hvlzxdhx6my4js3vc7vpvy3hpgj4kjkl0r47s9vpx"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-astropy-minimal
           python-gwcs
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/spacetelescope/drizzle")
    (synopsis "Combining dithered images into a single image")
    (description
     "The drizzle library is a Python package for combining dithered images
into a single image.  This library is derived from code used in DrizzlePac.
Like DrizzlePac, most of the code is implemented in the C language.  The
biggest change from DrizzlePac is that this code passes an array that maps the
input to output image into the C code, while the DrizzlePac code computes the
mapping by using a Python callback.  Switching to using an array allowed the
code to be greatly simplified.")
    (license license:bsd-3)))

(define-public python-drizzlepac
  (package
    (name "python-drizzlepac")
    ;; 3.10.0 requires astrocut<0.9.
    (properties '((commit . "e01563c48206f02f4cd4c525732764b9b827b3c4")
                  (revision . "0")))
    (version (git-version "3.10.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spacetelescope/drizzlepac")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zcny9mkm7w4mjnrglhq28i59x7a2yj38x773qcl7qsvxd0cjs5b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: Tests fail to load with error: E ModuleNotFoundError: No module
      ;; named 'stsci.tools'
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; TODO: Sanity check phase fails a lot on mismatched versions or
          ;; failed to load "stsci.tools" module.
          (delete 'sanity-check)
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(version-major+minor+point version)))))))
    (native-inputs
     (list python-ci-watson
           python-crds-minimal
           python-markupsafe
           python-pytest
           python-pytest-remotedata
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astrocut
           python-astropy
           python-astroquery
           python-bokeh
           python-fitsblender
           python-lxml
           python-matplotlib
           python-numpy
           python-pandas
           python-photutils
           python-pypdf
           python-stregion
           python-requests
           python-scikit-image
           python-scikit-learn
           python-scipy
           python-simplify-polyline
           python-spherical-geometry
           python-stsci-image
           python-stsci-imagestats
           python-stsci-skypac
           python-stsci-stimage
           python-stsci-tools
           python-stwcs
           python-tweakwcs))
    (home-page "https://drizzlepac.readthedocs.io/")
    (synopsis "AstroDrizzle for HST images")
    (description
     "@acronym{Hubble Space Telescope , HST} image combination using the
drizzle algorithm to combine astronomical images, to model image distortion,
to remove cosmic rays, and generally to improve the fidelity of data in the
final image.")
    (license license:bsd-3)))

(define-public python-drms
  (package
    (name "python-drms")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "drms" version))
       (sha256
        (base32 "0nnyqzy9dblis3q1xhx77z8ys1k969fh9qqdvmapn46v91299gml"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-astropy-minimal
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-packaging
           python-pandas))
    (home-page "https://sunpy.org")
    (synopsis "Access astronomical HMI, AIA and MDI data with Python")
    (description
     "DRMS module provides an easy-to-use interface for accessing HMI, AIA and
MDI data with Python.  It uses the publicly accessible
JSOC (@url{http://jsoc.stanford.edu/}) DRMS server by default, but can also be
used with local NetDRMS sites.")
    (license license:bsd-2)))

(define-public python-dust-extinction
  (package
    (name "python-dust-extinction")
    (version "1.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dust_extinction" version))
       (sha256
        (base32 "01iap2k49izn53v23kwkkpr5j5xhgk79xlwx6cb6h5ng29274nq5"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-doctestplus
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-scipy))
    (home-page "http://dust-extinction.readthedocs.io/")
    (synopsis "Interstellar Dust Extinction Models")
    (description
     "This package provides astronomical interstellar dust extinction curves
implemented using the astropy.modeling framework.")
    (license license:bsd-3)))

(define-public python-edps
  (package
    (name "python-edps")
    (version "1.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.eso.org/pub/dfs/pipelines/libraries/edps/edps-"
             version ".tar.gz"))
       (sha256
        (base32 "0r8afkpviihy9pd7hvgnc3amblzmbz21kbrdvynmz25w7fj7q9jk"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f)) ;no tests in tarball
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-fastapi
           python-networkx
           python-pyyaml
           python-requests
           python-uvicorn
           python-tinydb
           python-frozendict
           python-jinja2
           python-pydantic
           python-psutil))
    (home-page "https://www.eso.org/sci/software/edps.html")
    (synopsis "ESO's Data Processing System")
    (description
     "@acronym{European Southern Observatory Data Processing System EDPS} is a
system to automatically organise data from ESO instruments for pipeline
processing and running the pipeline on these data.  It is used for quality
control at ESO.  The current public release is a beta version without a GUI.
A GUI is being developed and the system is meant to eventually replace the
older EsoReflex environment.")
    (properties '((upstream-name . "edps")))
    (license license:bsd-3)))

(define-public python-ephem
  (package
    (name "python-ephem")
    (version "4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ephem" version))
       (sha256
        (base32 "0rb0vc3kgmw5rzhfhxffg94bcwasm46sf814hv7l13ry8m7xckrw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--deselect=ephem/tests/test_jpl.py::JPLTest::runTest")
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: See https://codeberg.org/guix/guix/issues/2108
          (add-after 'install 'remove-installed-tests
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (delete-file-recursively ".")
              (mkdir-p "ephem")
              (let* ((site (site-packages inputs outputs))
                     (tests (string-append site "/ephem/tests")))
                (copy-recursively tests "ephem/tests")
                (delete-file-recursively tests)))))))
    (native-inputs
     (list python-pytest python-setuptools tzdata))
    (home-page "https://rhodesmill.org/pyephem/")
    (synopsis "Compute positions of the planets and stars")
    (description
     "PyEphem provides an @code{ephem} Python package for performing
high-precision astronomy computations.

The name ephem is short for the word ephemeris, which is the traditional term
for a table giving the position of a planet, asteroid, or comet for a series
of dates.")
    (license license:expat)))

(define-public python-esutil
  (package
    (name "python-esutil")
    (version "0.6.16")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/esheldon/esutil")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05csk5asq3si7gdq8mpfh288z10rs45ylpcrrcjx0009q52l95xq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'relax-gcc-14-strictness
            (lambda _
              (setenv "CFLAGS" (string-join
                                (list "-g" "-O2"
                                      "-Wno-error=incompatible-pointer-types")
                                " ")))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://github.com/esheldon/esutil")
    (synopsis "Numerical, scientific, and astrophysical computing utilities")
    (description
     "This package provides a wide variety of utilities, focused primarily on
numerical python, statistics, and file input/output.  Includes specialized
tools for astronomers.")
    (license license:gpl2+)))

(define-public python-eventio
  (package
    (name "python-eventio")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch) 
       (uri (git-reference
              (url "https://github.com/cta-observatory/pyeventio")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g548pca43iwpq1641w3jzrb3rk2kqjf5gcjbcymfpdif1982zv2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 163 passed, 3 skipped, 4 xfailed
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-cython
           python-numpy
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-corsikaio
           python-matplotlib
           python-numpy
           python-zstandard))
    (home-page "https://github.com/cta-observatory/pyeventio")
    (synopsis "Python read-only implementation of the EventIO file format")
    (description
     "Python read-only implementation of the @code{EventIO} file format.")
    (license license:expat)))

(define-public python-extinction
  (package
    (name "python-extinction")
    (version "0.4.8")
    (source
     (origin
       (method git-fetch) ; No tests in PyPI
       (uri (git-reference
             (url "https://github.com/sncosmo/extinction")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dcb4c1rjv0msb3kagpgrj2xlb5spv8j76giy14vxkvz33lm4pz7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "test.py")))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/sncosmo/extinction")
    (synopsis "Fast interstellar dust extinction laws")
    (description
     "This package provides a cython-optimized implementations of empirical dust
exitinction laws found in the literature.")
    (license license:expat)))

(define-public python-fits-schema
  (package
    (name "python-fits-schema")
    ;; 0.5.6 was released in 2022, there are NumPy 2 comparability changes
    ;; which are available on master HEAD, use the latest commit for now.
    (properties '((commit . "ccffe04a8a47f2bd0f69014caaf73d9679b89a87")
                  (revision . "0")))
    (version (git-version "0.5.6"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/VODF/fits_schema")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lj8vb3b2s7m56bs4am6856w8vdlyi4p86gj7hlkncfngsgx1f8v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(version-major+minor+point version)))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy))
    (home-page "https://github.com/VODF/fits_schema")
    (synopsis "Schemata for FITS files")
    (description
     "This package provides a Python definition and validation schemata for FITS
files.")
    (license license:expat)))

(define-public python-fitsblender
  (package
    (name "python-fitsblender")
    (version "0.4.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsblender" version))
       (sha256
        (base32 "1ba1374axaxh3dzzpii6q05z4jcrfp7yjk1wsfm2gzmxalnp0f6r"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-stsci-tools))
    (home-page "https://github.com/spacetelescope/fitsblender")
    (synopsis "Aggregate values in FITS headers")
    (description
     "This package supports the creation of a combined header for a FITS file
based on the contents of the headers of a set of input FITS images.  A rules
file defines what keywords will be present in the combined output header as
well as how the output value will be determined from the set of values from
all the input image headers.")
    (license license:bsd-3)))

(define-public python-fitsio
  (package
    (name "python-fitsio")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsio" version))
       (sha256
        (base32 "0q4j9c49ladlpy7ag5jsi91d5mpsi6nlh6pnmbz4cbx6vjq4yfg2"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove the bundled cfitsio. When update the package check the
        ;; current bundled version.
        #~(begin
            (delete-file-recursively "cfitsio-4.6.3.tar.gz")
            (substitute* "MANIFEST.in"
              (("recursive-include cfitsio-4.6.3.*$\n") ""))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1860 passed, 904 skipped, 113 xfailed
      #:test-flags
      #~(list "--pyargs" "fitsio")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env
            (lambda _
              (setenv "FITSIO_USE_SYSTEM_FITSIO" "True")
              (setenv "FITSIO_SYSTEM_FITSIO_INCLUDEDIR"
                      (string-append
                       #$(this-package-input "cfitsio") "/include"))
              (setenv "FITSIO_SYSTEM_FITSIO_LIBDIR"
                      (string-append
                       #$(this-package-input "cfitsio") "/lib")))))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (inputs
     (list curl
           cfitsio
           zlib))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/esheldon/fitsio")
    (synopsis "Python library to read from and write to FITS files")
    (description
     "This package provides a Python library for reading from and writing
to @acronym{FITS, Flexible Image Transport System} files using the
CFITSIO library.  Among other things, it can

@itemize
@item read and write image, binary, and ascii table extensions;

@item read arbitrary subsets of tables in a lazy manner;

@item query the rows and columns of a table;

@item read and write header keywords;

@item read and write Gzip files.
@end itemize")
    (license license:gpl2+)))

(define-public python-galsim
  (package
    (name "python-galsim")
    (version "2.7.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/GalSim-developers/GalSim")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1phrxmxwwr72hs4yq6c5yjf3gcpqz8psah74vij5l8zlv4d71qc3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 232 passed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              "--ignore=devel/"
              ;; XXX: Tests need data from
              ;; <http://www.sas.upenn.edu/~mjarvis/des_data.tar.gz> 637MiB,
              ;; see how it's implemented in CI
              ;; (.github/workflows/ci.yml). Try to activate more tests: 141
              ;; failed, 608 passed, 1 skipped, 1 warning
              #$@(map (lambda (file) (string-append "--ignore="
                                                    "tests/"
                                                    file))
                      (list "test_bandpass.py"
                            "test_catalog.py"
                            "test_cdmodel.py"
                            "test_chromatic.py"
                            "test_config_gsobject.py"
                            "test_config_image.py"
                            "test_config_input.py"
                            "test_config_noise.py"
                            "test_config_output.py"
                            "test_config_value.py"
                            "test_convolve.py"
                            "test_correlatednoise.py"
                            "test_deprecated.py"
                            "test_des.py"
                            "test_download.py"
                            "test_draw.py"
                            "test_fitsheader.py"
                            "test_galaxy_sample.py"
                            "test_hsm.py"
                            "test_image.py"
                            "test_inclined.py"
                            "test_interpolatedimage.py"
                            "test_knots.py"
                            "test_lensing.py"
                            "test_main.py"
                            "test_optics.py"
                            "test_phase_psf.py"
                            "test_photon_array.py"
                            "test_random.py"
                            "test_real.py"
                            "test_roman.py"
                            "test_sensor.py"
                            "test_sum.py"
                            "test_table.py"
                            "test_transforms.py"
                            "test_wcs.py")))
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* (list "conda_requirements.txt"
                                "pyproject.toml"
                                "requirements.txt"
                                "setup.py")
               (("setuptools>=38,<72") "setuptools")
               (("pybind11>=2.2") "pybind11"))))
          (add-before 'check 'remove-local-galsim
            (lambda _
              ;; This would otherwise interfere with finding the installed
              ;; galsim when running tests.
              (delete-file-recursively "galsim"))))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools))
    (inputs
     (list eigen
           fftw))
    (propagated-inputs
     (list pybind11     ;XXX: Double check if it needs be here.
           python-astropy
           python-lsstdesc-coord
           python-numpy))
    (home-page "https://github.com/GalSim-developers/GalSim")
    (synopsis "Modular galaxy image simulation toolkit")
    (description
     "GalSim is software for simulating images of astronomical objects (stars,
 galaxies) in a variety of ways.")
    (license license:bsd-3)))

(define-public python-gatspy
  (package
    (name "python-gatspy")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gatspy" version))
       (sha256
        (base32 "1gw2z6x8nikvnw2gkdl70gr81cwczd1pd7v8ry2kjn6k4kssrfav"))))
    (build-system pyproject-build-system)
    ;;TODO: Tests depends on Nose, report upstream.
    (arguments (list #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astroml
           python-numpy
           python-scipy
           python-supersmoother))
    (home-page "https://github.com/astroml/gatspy")
    (synopsis "General tools for astronomical time series in Python")
    (description
     "This package provides general tools for astronomical time series in
Python.")
    (license license:bsd-2)))

(define-public python-ginga
  (package/inherit ginga
    (name "python-ginga")
    (inputs '())
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-packaging
           python-pillow
           python-puremagic
           python-pyyaml
           python-qtpy))))

(define-public python-gizmo-analysis
  (package
    (name "python-gizmo-analysis")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gizmo_analysis" version))
       (sha256
        (base32 "199wvy9jnb76dp1xvh8r3gyjrxipvh51fpvhqgc6gq0frkjxxnf9"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;no tests
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-h5py
           python-matplotlib
           python-numpy
           python-scipy
           python-utilities-awetzel))
    (home-page "https://bitbucket.org/awetzel/gizmo_analysis")
    (synopsis "Read and analyze Gizmo simulations")
    (description
     "Python package for reading and analyzing simulations generated using
the Gizmo code, in particular, the FIRE cosmological simulations.")
    (license license:expat)))

(define-public python-glue-astronomy
  (package
    (name "python-glue-astronomy")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glue_astronomy" version))
       (sha256
        (base32 "0f2849jyhyxldrxlls00gpq9qckrfvxrkn796j5zb8j92l2qjgnp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 77 passed, 2 skipped, 3 deselected
      #:test-flags
      #~(list "--pyargs" "glue_astronomy"
              ;; XXX: See: <https://github.com/glue-viz/glue-astronomy/issues/117>.
              "-k" (string-join
                    ;; Not equal to tolerance rtol=1e-07, atol=0
                    (list "not test_from_spectrum1d[wcs3d]"
                          ;; ValueError: WCS is 1D but flux is
                          ;; multi-dimensional. Please specify
                          ;; spectral_axis_index.
                          "test_spectrum1d_2d_data[2]"
                          "test_spectrum1d_2d_data[3]"
                          ;; AssertionError: assert False
                          "test_identifier_casa"
                          ;; AttributeError: module 'numpy' has no attribute
                          ;; 'product'
                          "test_reader_casa"
                          "test_spectral_cube_io")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-mock
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-glue-core
           python-pyqt  ;PyQt5
           python-regions
           python-reproject
           python-specreduce
           python-spectral-cube
           python-specutils))
    (home-page "https://github.com/glue-viz/glue-astronomy")
    (synopsis "Astronomy-specific plugins for glue")
    (description
     "The glue-astronomy plugin for glue provides a collection of
astronomy-specific functionality")
    (license license:bsd-3)))

(define-public python-glue-core
  (package
    (name "python-glue-core")
    (version "1.24.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glue_core" version))
       (sha256
        (base32 "062l4bhh1dvsazrlnjc4kr3qj9g3s8qmcs0kqb1libiimy6rdbkk"))))
    (build-system pyproject-build-system)
    (arguments
     ;; XXX: This test is flaky, it can fail if the CI system is slow.
     (list #:test-flags #~(list "-k" "not test_combine_slices_hypot")))
    (native-inputs
     (list python-pytest
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astrodendro
           python-astropy
           python-dill
           python-echo
           python-fast-histogram
           python-ipython
           python-matplotlib
           python-mpl-scatter-density
           python-numpy
           python-openpyxl
           python-pandas
           python-pyavm
           python-pillow
           python-scikit-image
           python-scipy
           python-setuptools ; to load extenral plugins, glue/main.py.
           python-shapely
           python-spectral-cube
           python-xlrd))
    (home-page "http://glueviz.org")
    (synopsis "Multidimensional data visualization project")
    (description
     "Glue is a python project to link visualizations of scientific datasets
across many files.")
    (license license:bsd-3)))

(define-public python-glue-geospatial
  (package
    (name "python-glue-geospatial")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glue-geospatial" version))
       (sha256
        (base32 "1mn71zqsrxlg9gp5czmvw5k523ddvwp42mlx6fqvm5ahw42yhqaj"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-mock
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-affine
           python-glue-core
           python-pyproj
           python-rasterio))
    (home-page "https://github.com/glue-viz/glue-geospatial")
    (synopsis "Glue plugin to support satellite imagery")
    (description
     "This package provides a Glue plugin for geospatial imagery.")
    (license license:bsd-3)))

(define-public python-glue-qt
  (package
    (name "python-glue-qt")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/glue-viz/glue-qt")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hs1njs25mzmkxr84mrmw4p3pl1fkdlagpnflcd0cvdn9g2mbdsq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 618 passed, 4 skipped, 2 xfailed, 436 warnings
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               ;; ipython>=4.0,<9.0
               ((">=4.0,<9\\.0") ">=4.0"))))
          (add-before 'build 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'prepare-x
            (lambda _
              (system "Xvfb &")
              (setenv "DISPLAY" ":0")
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-objgraph
           python-pytest
           python-setuptools
           python-setuptools-scm
           xorg-server-for-tests))
    (propagated-inputs
     (list python-astropy
           python-echo
           python-glue-core
           python-ipykernel
           python-ipython
           python-matplotlib
           python-numpy
           python-pvextractor
           python-qtconsole
           python-qtpy
           python-scipy))
    (home-page "http://glueviz.org")
    (synopsis "Multidimensional data visualization across files")
    (description "Multidimensional data visualization across files.")
    (license license:bsd-3)))

(define-public python-glue-vispy-viewers
  (package
    (name "python-glue-vispy-viewers")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glue_vispy_viewers" version))
       (sha256
        (base32 "0pnlkrkc6vn6mrqlw8xknb01rp7dy3qng4vaaiz9fry50nkf4gsd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "glue_vispy_viewers"
              ;; XXX: Fatal Python error: Segmentation fault.
              "-k" "not test_vispy_widget")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-x
            (lambda _
              (system "Xvfb :99 -screen 0 1024x768x24 &")
              (setenv "DISPLAY" ":99.0")
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-mock
           python-objgraph
           python-pytest
           python-pytest-cov
           ;; python-pytest-faulthandler
           python-setuptools
           python-wheel
           xorg-server-for-tests))
    (propagated-inputs
     (list python-echo
           python-glfw
           python-glue-core
           python-imageio
           python-matplotlib
           python-numpy
           python-pyopengl
           python-scipy
           python-vispy))
    (home-page "https://github.com/glue-viz/glue-vispy-viewers")
    (synopsis "Vispy-based viewers for Glue")
    (description
     "This package provides a Glue plugin which adds a 3D scatter plot viewer
and a 3D volume rendering viewer.")
    (license license:bsd-2)))

(define-public python-gw-sky
  (package
    (name "python-gw-sky")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gw_sky" version))
       (sha256
        (base32 "0k2qgy6d443lgxb140w70q078hclaf2c1jl85czkzkmz77yiiblc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list jupyter
           python-pytest
           python-healpy
           python-nbconvert
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://github.com/Hazboun6/gw_sky")
    (synopsis "Gravitational wave signals visualization tool")
    (description
     "Python package for making visuals of gravitational wave signals,
specifically pulsar timing array signals.")
    (license license:expat)))

(define-public python-gwcs
  (package
    (name "python-gwcs")
    (version "0.26.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gwcs" version))
       (sha256
        (base32 "0gkxgwzlazg0xihhgg9vlss6jj7lg6i6r2z6igpxcnyc5s147d0p"))))
    (build-system pyproject-build-system)
    ;; tests: 1023 passed, 10 skipped, 10 warnings
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pyyaml
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy
           python-asdf-wcs-schemas
           python-astropy
           python-numpy
           python-scipy))
    (home-page "https://gwcs.readthedocs.io/en/latest/")
    (synopsis "Generalized World Coordinate System")
    (description "Generalized World Coordinate System (GWCS) is an Astropy
affiliated package providing tools for managing the World Coordinate System of
astronomical data.

GWCS takes a general approach to the problem of expressing transformations
between pixel and world coordinates.  It supports a data model which includes
the entire transformation pipeline from input coordinates (detector by
default) to world coordinates.")
    (license license:bsd-3)))

(define-public python-gwcs-0.21
  (package
    (inherit python-gwcs)
    (name "python-gwcs")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gwcs" version))
       (sha256
        (base32 "1fn5l4v236bl7xqi1is40c2q57dji8by98iwqcndfnmjwqf7zllc"))))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs python-gwcs)
       (replace "python-astropy" python-astropy-6)
       (replace "python-asdf" python-asdf-3)))))

(define-public python-halotools
  (package
    (name "python-halotools")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "halotools" version))
       (sha256
        (base32 "004nqlyiv6gyzmjk840a1hl3j4sgi5xwbfibankwi7281gq4hx3d"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Use built library for tests.
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion #$output
                  (setenv "HOME" "/tmp")
                  (apply invoke "pytest" "-vv" test-flags))))))))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-beautifulsoup4
           python-h5py
           python-numpy
           python-requests
           python-scipy))
    (home-page "http://halotools.rtfd.org/")
    (synopsis "N-body simulations and constrain models of cosmology evolution")
    (description
     "Halotools is a specialized python package for building and testing
models of the galaxy-halo connection, and analyzing catalogs of dark matter
halos.  The core feature of Halotools is a modular platform for creating mock
universes of galaxies starting from a catalog of dark matter halos obtained
from a cosmological simulation.")
    (license license:bsd-3)))

(define-public python-hasasia
  (package
    (name "python-hasasia")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hasasia" version))
       (sha256
        (base32 "0vs3935c9cqwp44dycxfsv4p9q4zbw8i5z3946928yy0nqmj1aw7"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-scipy))
    (home-page "https://github.com/Hazboun6/hasasia")
    (synopsis "Pulsar timing array sensitivity curves calculation in Python")
    (description
     "This package provides a Python package to calculate gravitational-wave
sensitivity curves for pulsar timing arrays.

Features:
@itemize
@item pulsar transmission functions
@item inverse-noise-weighted transmission functions
@item individual pulsar sensitivity curves
@item pulsar timing array sensitivity curves as characteristic strain, strain
sensitivity or energy density
@item power-law integrated sensitivity curves
@item sensitivity sky maps for pulsar timing arrays
@end itemize")
    (license license:expat)))

(define-public python-healpy
  (package
    (name "python-healpy")
    (version "1.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "healpy" version))
       (sha256
        (base32 "1i8f2d0zjaf35z8mn24lv9zs0wcjrfij6d7wfxnd68ssi35kks18"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 11 passed
      #:test-flags
      #~(list "--pyargs" "healpy")))
    (native-inputs
     (list nss-certs-for-test
           pkg-config
           python-cython
           python-pytest
           python-pytest-astropy-header
           python-pytest-cython
           python-pytest-doctestplus
           python-setuptools
           python-setuptools-scm))
    (inputs
     (list cfitsio
           healpix-cxx
           libsharp))
    (propagated-inputs
     (list python-astropy
           python-numpy
           ;; [optional]
           python-matplotlib
           python-scipy))
    (home-page "http://healpy.readthedocs.org/")
    (synopsis "Healpix tools package for Python")
    (description
     "healpy is a Python package to handle pixelated data on the sphere.  It
is based on the Hierarchical Equal Area isoLatitude Pixelization (HEALPix)
scheme and builds with the HEALPix C++ library.")
    (license license:gpl2+)))

(define-public python-hierarc
  (package
    (name "python-hierarc")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)       ;no tests data in the PyPI archive
       (uri (git-reference
              (url "https://github.com/sibirrer/hierArc")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02078a745hrb3m8fj739rwzk4wwxrfk40sr4yvs722aj5xk8j00w"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Some tests are computation intensive and not thread safe, failing
      ;; during parallel invocation:
      ;;   109 passed, 32 deselected, 43 warnings in 634.59s
      #~(list "--durations" "5"
              ;; AttributeError: 'LambdaCDM' object has no attribute '_Ok0'.
              "-k" (string-join
                    (list "not test_cosmo_instance"
                          "test_kde_likelihood_integration"
                          "test_log_likelihood"
                          "test_mcmc_emcee"
                          "test_sne_likelihood_integration")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;;  RuntimeError: cannot cache function 'rotate': no locator
              ;;  available for file '<...>/lenstronomy/Util/util.py'.
              (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy-6
           python-emcee
           python-h5py
           python-lenstronomy
           python-matplotlib
           python-mpmath
           python-numpy
           python-pandas
           python-scikit-learn
           python-scipy))
    (home-page "https://github.com/sibirrer/hierarc")
    (synopsis "Hierarchical analysis of strong lensing systems")
    (description
     "This package implements a funtionality for hierarchical analysis of
strong lensing systems to infer lens properties and cosmological parameters
simultaneously.  It allows to fit lenses with measured time delays, imaging
information, kinematics constraints and standardizable magnifications with
parameters described on the ensemble level.")
    (license license:bsd-3)))

(define-public python-holodeck
  (package
    (name "python-holodeck")
    (version "1.5.2")
    (source
     (origin
       (method git-fetch) ; no tests in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/nanograv/holodeck")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jz54fb6yyling2a756qqahixpn1wgxmhhqmv6pf0iqds019v9k7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count)))))
    (native-inputs
     (list python-cython-0
           python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-cosmopy
           python-emcee
           python-george
           python-h5py
           python-hasasia
           python-healpy
           python-ipywidgets
           python-kalepy
           python-matplotlib
           python-numpy
           python-psutil
           python-schwimmbad
           python-scipy
           python-sympy
           python-tqdm))
    (home-page "https://github.com/byu-pccl/holodeck")
    (synopsis "MBH Binary Population Synthesis for Gravitational Wave Calculations")
    (description
     "This package provides a comprehensive framework for @acronym{Massive
Black Hole,MBH} binary population synthesis.  The framework includes modules
to perform population synthesis using a variety of methodologies from
semi-analytic models, to cosmological hydrodynamic simulations, and even
observationally-derived galaxy merger catalogs.")
    (license license:expat)))

(define-public python-hvpy
  (package
    (name "python-hvpy")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hvpy" version))
       (sha256
        (base32 "0bly1bgp0axxhzzf5imqsgmms41z8cxbjahxsibvb55dk94gwig6"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; Requires HTTP(S) access to api.beta.helioviewer.org
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-pydantic
           python-pydantic-settings
           python-requests))
    (home-page "https://helioviewer.org/")
    (synopsis "Helioviewer Python API Wrapper")
    (description "@code{hvpy} is a Python API wrapper around the formal
@url{Helioviewer API, https://api.helioviewer.org/docs/v2/}.")
    (license license:bsd-2)))

(define-public python-irispy-lmsal
  (package
    (name "python-irispy-lmsal")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "irispy_lmsal" version))
       (sha256
        (base32 "131n2wvb5pdg0qx5chfwkcvcy025840cs01pvcg5i4nlqv995h1q"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 65 passed, 1 skipped, 9 deselected
      #:test-flags
      ;; See: <https://github.com/LM-SAL/irispy-lmsal/issues/83>.
      ;; Expected:
      ;;     np.float64(0.33)
      ;; Got:
      ;;     0.33
      #~(list "--deselect=irispy/obsid.py::irispy.obsid.ObsID"
              ;; TODO: NumPy 2+ is required to fully resolve this.
              ;; Arrays are not almost equal to 4 decimals
              ;; Mismatched elements: 42 / 3601 (1.17%)
              ;; Max absolute difference: 0.34272391
              ;; Max relative difference: 0.00491848
              ;;  x: array([0., 0., 0., ..., 0., 0., 0.])
              ;;  y: array([0., 0., 0., ..., 0., 0., 0.], dtype='>f4')
              "-k" "not test_get_latest_response_to_idl")
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: It fails to check SunPy's optional inputs versions.
          (delete 'sanity-check)
          (add-before 'check 'set-home
            (lambda _
              ;; E PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list ffmpeg
           python-pooch
           python-pytest-astropy
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-dkist
           python-mpl-animators
           python-ndcube
           python-pandas
           python-scipy
           python-sunpy
           python-sunraster))
    (home-page "https://iris.lmsal.com/")
    (synopsis "Tools to read and analyze data from the IRIS solar-observing satellite")
    (description
     "This package provides tools to read and analyze data from the
@acronym{IRIS, Interface Region Imaging Spectrograph} solar-observing
satellite.")
    (license license:bsd-3)))

(define-public python-jplephem
  (package
    (name "python-jplephem")
    (version "2.23")
    (source
     (origin
       (method git-fetch)       ;no tests data in the PyPI tarball
       (uri (git-reference
              (url "https://github.com/brandon-rhodes/python-jplephem")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mmd30cymb9f259c657d7jd65plirdsngnk14fbyjxd9vbryn2qa"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "ci"
                  (invoke "python" "-m" "unittest" "test"))))))))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/brandon-rhodes/python-jplephem")
    (synopsis "Python version of NASA DE4xx ephemerides")
    (description
     "@code{skyfield} computes positions for the stars, planets, and
satellites in orbit around the Earth.  Its results should agree with the
positions generated by the United States Naval Observatory and their
Astronomical Almanac to within 0.0005 arcseconds (half a @emph{mas} or
milliarcsecond).")
    (license license:expat)))

;; The last compatible version for python-astropy-6.
(define-public python-jplephem-2.22
  (hidden-package
   (package
     (inherit python-jplephem)
     (version "2.22")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "jplephem" version))
        (sha256
         (base32 "0b2rgb7pvwnl72pqjryf9c812mmdxr69fwiym7mnz05l2xrcr6hd"))))
    (arguments
     (list
      ;; FIXME: OSError: [Errno 30] Read-only file system:
      ;; '/gnu/store/<hash>-python-3.11.11/lib/python3.11/@test_43_tmpæ.pyc'
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "-m" "unittest" "discover" "-s" "test")))))))
    (native-inputs
     (list python-setuptools
           python-wheel)))))

(define-public python-jwst
  (package
    (name "python-jwst")
    (version "1.20.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jwst" version))
       (sha256
        (base32 "1kpv46fl3x3ygq3vzprv0c30x6m95ka4fp5c9cd9p0rhydm6apm3"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Replace reference to external configobj.
            (substitute* (find-files "." "\\.py$")
              (("from astropy.extern import configobj")
               "import configobj")
              (("from astropy.extern.configobj import validate")
               "import validate")
              (("from astropy.extern.configobj.configobj import ")
               "from configobj import ")
              (("from astropy.extern.configobj.validate import ")
               "from validate import "))))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests require access to https://jwst-crds-pub.stsci.edu server
      ;; for getting data sets.
      #:tests? #f))
    (native-inputs
     (list python-ci-watson
           python-pysiaf
           python-pytest
           python-pytest-doctestplus
           python-requests-mock
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-bayesicfitting
           python-crds-minimal
           python-drizzle
           python-gwcs
           python-importlib-metadata
           python-jsonschema
           python-numpy
           python-packaging
           python-photutils
           python-pyparsing
           python-requests
           python-scikit-image
           python-scipy
           python-spherical-geometry
           python-stcal
           python-stdatamodels
           python-stpipe
           python-stsci-imagestats
           python-synphot
           python-tweakwcs
           python-wiimatch
           ;; [optional]
           python-jplephem
           python-pysiaf))
    (home-page "https://jwst-pipeline.readthedocs.io/en/latest/")
    (synopsis "Science observations from the James Webb Space Telescope")
    (description
     "This package provides an access to the JWST Science Calibration Pipeline
processes data from all JWST instruments and observing modes by applying
various science corrections sequentially, producing both fully-calibrated
individual exposures and high-level data products (mosaics, extracted spectra,
etc.).")
    (license license:bsd-3)))

(define-public python-jwst-backgrounds
  (package
    (name "python-jwst-backgrounds")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jwst_backgrounds" version))
       (sha256
        (base32 "14m1a6z884vg2n5ndwwhpnzpb5h28hh58a53dfjwwbjakwmixb0p"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests in PyPI tarball, tests requir networking
    (native-inputs
     (list python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-healpy
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://github.com/spacetelescope/jwst_backgrounds")
    (synopsis "Retrieve and plot JWST background information")
    (description
     "This package provides a a simple program to predict the levels of
background emission in JWST observations, for use in proposal planning.

It accesses a precompiled background cache prepared by Space Telescope Science
Institute.  The background cache is hosted by the Mikulski Archive for Space
Telescopes (MAST), so you need internet access to run the tool with the remote
cache.  It is possible to download the full background cache to your local
machine.")
    (license license:bsd-3)))

(define-public python-jwst-reffiles
  (package
    (name "python-jwst-reffiles")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jwst_reffiles" version))
       (sha256
        (base32 "1dlw955cw49qczdmimglmlcbal8vd3wbv5j48ckllvjgd59pwr3s"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; One test fails with error: FileNotFoundError: [Errno 2] No such file
      ;; or directory.
      #:test-flags #~(list "-k" "not test_calib_prep_steps")))
    (native-inputs
     (list python-pytest))
    (propagated-inputs
     (list python-astropy
           python-jwst
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://github.com/spacetelescope/jwst_reffiles")
    (synopsis "Tool for JWST's CRDS-formatted reference files creation")
    (description
     "This package provides a tool to create @acronym{Calibration References
Data System,CRDS}-formatted reference files for @acronym{James Webb Space
Telescope,JWST} from a set of input dark current files and a set of flat field
files.")
    (license license:bsd-3)))

(define-public python-kanon
  (package
    (name "python-kanon")
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ALFA-project-erc/kanon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sg9yrsas5xmhbw6mhfyxsxh9i060af6v02whr9fqgv687fiyrhc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 150 passed, 18 deselected, 246 warnings
      #:test-flags
      ;; XXX: This tests failing a lot.
      #~(list "-k" (string-append "not test_attribute_forwardin"
                                  " and not test_declination"
                                  " and not test_init_basedquantity"
                                  " and not test_ptolemy_viz"
                                  " and not test_ptolemy_viz"
                                  " and not test_quantity"
                                  " and not test_read"
                                  " and not test_shifting"
                                  " and not test_sun_true_position"
                                  " and not test_sun_true_position")
              "--ignore=kanon/tables/__init__.py")
      #:phases
      #~(modify-phases %standard-phases
          ;; See <https://github.com/ALFA-project-erc/kanon/issues/149>.
          (delete 'sanity-check)
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                (("version = \"0.0.0\"") (string-append "version = \"" #$version "\""))
                ;; RuntimeError: Unable to detect version control
                ;; system. Checked: Git. Not installed: Mercurial, Darcs,
                ;; Subversion, Bazaar, Fossil, Pijul.  See
                ;; <https://github.com/blacklanternsecurity/bbot/issues/1257>.
                (("enable = true") "enable = false"))))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list git-minimal/pinned
           python-poetry-core
           python-poetry-dynamic-versioning
           python-pytest-astropy
           python-pytest-xdist
           python-requests-mock))
    (propagated-inputs
     (list python-astropy-6
           python-matplotlib
           python-numpy-1
           python-pandas
           python-requests
           python-scipy
           ;; [optional]
           python-ipykernel
           python-papermill))
    (home-page "https://dishas.obspm.fr")
    (synopsis "History of astronomy")
    (description "This package provides a history of astronomy library.
Current Features:
@itemize
@item define standard positional numeral systems with standard arithmetics
(BasedReal)
@item set your own precision contexts and algorithms on arithmetical
operations (PrecisionContext)
@item keep track of all operations
@item build or import ancient astronomical tables
@item perform arithmetical and statistical operations
@item support for BasedReal values
@item define new calendar types
@item date conversions
@item collection of mathematical models used for all kinds of geocentric
astronomical tables
@end itemize")
    (license license:bsd-3)))

(define-public python-lenstronomy
  (package
    (name "python-lenstronomy")
    (version "1.13.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/lenstronomy/lenstronomy")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "046695i2qapjz1r8z87kqsp2x0klsd157zhsncplrkpb3kapk7zj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1483 passed, 2189 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; TypeError: SparseSolverBase.__init__() got an unexpected
              ;; keyword argument 'num_iter_lens'
              "--ignore=test/test_ImSim/test_image_model_pixelbased.py"
              "-k" (string-join
                    ;; _flapack.error: (liwork>=max(1,10*n)||liwork==-1)
                    ;; failed for 10th keyword liwork: dsyevr:liwork=1
                    (list "not test_sampler"
                          ;; TypeError: SLIT_Starlets.function_2d() got an
                          ;; unexpected keyword argument 'n_pix_x'
                          "test_pixelbased_modelling"
                          ;; This might be NumPy issue:
                          ;; Arrays are not almost equal to 3 decimals
                          ;;  ACTUAL: -8669.992995432973
                          ;;  DESIRED: -9108.078115857057
                          "test_multiplane")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; Tests try to search for JSON in "path = os.getcwd()" and fail
              ;; with error:
              ;;
              ;; FileNotFoundError: [Errno 2] No such file or directory:
              ;; '/tmp/<...>/source/coolest_template.json'
              (for-each (lambda (file) (copy-file file (basename file)))
                        (find-files "test/test_Util/test_COOLEST"
                                    "(\\.json|\\.fits)$")))))))
    (native-inputs
     (list python-cobaya
           python-colossus
           python-coolest
           python-emcee
           python-nestcheck
           ;; python-pymultinest   ;depends on MultiNest
           python-pytest
           ;; python-starred-astro ;depends on python-jax
           python-nautilus-sampler
           python-dynesty
           python-pytest-xdist
           python-setuptools
           python-slitronomy
           python-zeus-mcmc))
    (propagated-inputs
     (list python-astropy-6
           python-configparser
           python-corner
           python-h5py
           python-matplotlib
           python-mpmath
           python-multiprocess
           python-numba
           python-numpy
           python-pyxdg
           python-pyyaml
           python-schwimmbad
           python-scikit-image
           python-scikit-learn
           python-scipy))
    (home-page "https://github.com/lenstronomy/lenstronomy")
    (synopsis "Multi-purpose lens modeling software")
    (description
     "@code{lenstronomy} is a multi-purpose software package to model strong
gravitational lenses.  @code{lenstronomy} finds application for time-delay
cosmography and measuring the expansion rate of the Universe, for quantifying
lensing substructure to infer dark matter properties, morphological
quantification of galaxies, quasar-host galaxy decomposition and much more.")
    (license license:bsd-3)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-lenstronomy-minimal
  (package/inherit python-lenstronomy
    (name "python-lenstronomy-minimal")
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools))))

(define-public python-libstempo
  (package
    (name "python-libstempo")
    (version "2.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libstempo" version))
       (sha256
        (base32 "0408g761w9i0kg69b72y4lnpz5sa8bzd0zayi73q6wkry8lh7ymq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-tempo2-search-paths
            (lambda _
              (let* ((tempo2 #$(this-package-input "tempo2"))
                     (tempo2-runtime (string-append tempo2 "/share/runtime")))
                (setenv "TEMPO2_PREFIX" tempo2)
                (setenv "TEMPO2" tempo2-runtime)))))))
    (native-inputs
     (list python-cython
           python-numpy
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (inputs
     (list tempo2))
    (propagated-inputs
     (list python-astropy
           python-ephem
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://github.com/vallis/libstempo")
    (synopsis "Python wrapper for tempo2")
    (description
     "This package provides a Python wrapper for tempo2 - a high precision
pulsar timing tool.")
    (license license:expat)))

(define-public python-linetools
  (package
    (name "python-linetools")
    (version "0.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "linetools" version))
       (sha256
        (base32 "1nljcaz0r60wvsy5dwb040wll2dd764dvib5xzkpc59cbw5lz85h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; AttributeError: `np.string_` was removed in the NumPy 2.0
      ;; release. Use `np.bytes_` instead.
      #~(list "--deselect=linetools/tests/test_init_absline.py::test_dicts"
              "--deselect=linetools/tests/test_init_emissline.py::test_dicts")))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-ipython
           python-matplotlib
           python-numpy
           python-pyyaml
           python-qtpy
           python-scipy))
    (home-page "http://linetools.readthedocs.org/")
    (synopsis "Quasar and galaxy spectra 1-D analysis tool")
    (description
     "This package implements a functionality for analysing absorption and
emission lines in 1-D spectra, especially galaxy and quasar spectra.")
    (license license:bsd-3)))

(define-public python-lofar-h5plot
  (package
    (name "python-lofar-h5plot")
    (version "2.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lofar_h5plot" version))
       (sha256
        (base32 "0gxri18iqsw1l5zxn40475zj34h5nn1bscfrhkc739vpqjmj60hz"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests provided
    (native-inputs
     (list python-cython
           python-setuptools))
    (propagated-inputs
     (list python-losoto
           python-matplotlib
           python-numpy-1
           python-pyqt
           python-pyqt5-sip))
    (home-page "https://github.com/tikk3r/lofar-h5plot")
    (synopsis "GUI for plotting H5Parms produced during LOFAR calibration")
    (description
     "H5plot is a small GUI to view the solutions in an H5parm interactively.
It is a spiritual successor to @code{ParmDBplot} for quickly reviewing gain
solutions generated by NDPPP.")
    (license license:gpl3)))

(define-public python-lofar-parameterset
  (package
    (name "python-lofar-parameterset")
    (version "1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "lofar_parameterset" version))
       (sha256
        (base32 "059k9kvv0rfg1npc75mrr8qa3k8x7h0572jvfzbm5w66qx5jy0nh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (home-page "https://git.astron.nl/lofar2.0/lofar_pyparameterset")
    (synopsis "Minimal Python implementation of the LOFAR ParameterSet")
    (description
     "This package contains a pure-python drop-in replacement for the Python
wrapper around the original LOFAR ParameterSet that is written in C++.  It
supports only the basic features of the original.")
    (license license:gpl3+)))

(define-public python-losoto
  (package
    (name "python-losoto")
    (version "2.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/revoltek/losoto")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wqqjc7iaphiq517n939s3wmsypyxwjaqrr51q21343m2blrl40m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 19 failed, 23 passed, 53 warnings
      ;; XXX: See: <https://github.com/revoltek/losoto/issues/186>.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               ;; XXX: See: <https://github.com/revoltek/losoto/issues/187>.
               (("progressbar") "progressbar2")))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-casacore
           python-lofar-parameterset
           python-matplotlib
           python-numpy
           python-progressbar2
           python-scipy
           python-tables))
    (home-page "https://github.com/revoltek/losoto")
    (synopsis "LOFAR Solution Tool")
    (description
     "This package provides a @acronym{Low-Frequency Array,
@url{http://www.lofar.org/, LOFAR}} a large radio telescope Solution Tool.")
    (license license:gpl3)))

(define-public python-lsstdesc-coord
  (package
    (name "python-lsstdesc-coord")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "LSSTDESC.Coord" version))
       (sha256
        (base32 "1gk6vx8j0sx15b41l3sci62ckgmq7y5qsbn97v3dwwllw7qqznjh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-astropy-minimal
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-future
           python-numpy))
    (home-page "https://github.com/LSSTDESC/Coord")
    (synopsis "Angles and celestial coordinates handling in Python")
    (description
     "This package provides a similar in functionality to the
@code{astropy.coordinates} module, but with more of an emphasis on efficiency.
Some functions are more than 100 times faster than the corresponding
functionality in astropy.  On the other hand, the API is somewhat more
restrictive than the API used by astropy, so the appropriate module to use
will depend on your needs.")
    (license license:expat)))

(define-public python-mapsims
  (package
    (name "python-mapsims")
    (version "2.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mapsims" version))
       (sha256
        (base32 "15mkdbmcys42vh46wzvaw8avx54dicav3dazflpfr634jw9bd8hs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Tests requiring additional FITS files.
      ;; <https://portal.nersc.gov/project/cmb>
      ;; <https://portal.nersc.gov/project/sobs>
      #~(list "-k" (string-join
                    (list "not test_noise_simulator[ST0]"
                          "test_noise_simulator[ST3]"
                          "test_homogeneous_noise"
                          "test_from_classes_car_healpix"
                          "test_from_config_v02"
                          "test_from_classes"
                          "test_s4sim_202222_ame_high")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               ;; pixell==0.17.3
               (("==0.17.3") ">=0.17.3")))))))
    (native-inputs
     (list nss-certs-for-test
           python-flit-core
           python-nbformat
           python-nbval
           python-pytest
           python-pytest-astropy))
    (propagated-inputs
     (list python-astropy
           python-healpy
           python-numpy
           python-pixell
           python-pysm3
           python-pyyaml
           python-so-noise-models
           python-toml))
    (home-page "https://github.com/galsci/mapsims")
    (synopsis "Map based simulations software for CMB Experiments")
    (description
     "This package implements a functionality to produce map based simulations
for the @url{https://simonsobservatory.org/, Simons Observatory} or other CMB
experiments.  It creates simulated maps in HEALPix and CAR pixelization based
on:
@itemize
@item foreground models included in PySM
@item custom foregrounds models from the so_pysm_models package
@item precomputed Cosmic Microwave Background simulations
@item noise simulations based on expected performance and simulated hitmaps
@item effect of gaussian beam convolution
@end itemize")
    (license license:bsd-2)))

(define-public python-mpl-animators
  (package
    (name "python-mpl-animators")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpl_animators" version))
       (sha256
        (base32 "1lwh16f5dilnxjnchw2l2pz7iz3dby7ij6z0l7v3fm6aqhk6fwrg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; Relax matplotlib warning: ... because the default path
              ;; (/homeless-shelter/.config/matplotlib) is not a writable
              ;; directory ...
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-doctestplus
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy))
    (home-page "https://sunpy.org")
    (synopsis "Interactive animations with matplotlib")
    (description
     "The @code{mpl_animators} package provides a set of classes which allow
the easy construction of interactive matplotlib widget based animations.")
    (license license:bsd-3)))

(define-public python-naima
  (package
    (name "python-naima")
    (version "0.10.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "naima" version))
       (sha256
        (base32 "16z3qzdd31is2dxkp4jgazcykrhx3m2i3qrs7dl9rbhlj5nxx1iy"))))
    (build-system pyproject-build-system)
    ;; tests: 213 passed, 2 skipped, 1 xpassed, 99 warnings
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest))
    (propagated-inputs
     (list python-astropy
           python-corner
           python-emcee
           python-h5py
           python-matplotlib
           python-numpy
           python-pyyaml
           python-scipy))
    (home-page "http://github.com/zblz/naima")
    (synopsis "Derivation of non-thermal particle distributions through MCMC spectral fitting")
    (description
     "This package implement functionality for computation of non-thermal
radiation from relativistic particle populations.  It includes tools to
perform MCMC fitting of radiative models to X-ray, GeV, and TeV spectra using
@code{emcee}, an affine-invariant ensemble sampler for Markov Chain Monte
Carlo.")
    (license license:bsd-3)))

(define-public python-ndcube
  (package
    (name "python-ndcube")
    (version "2.3.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ndcube" version))
       (sha256
        (base32 "0nwy0h1xvs6gw13nygfqx34nqkiixj40pf913n6h7bjfvqkyg2f1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 397 passed, 7 skipped, 10 xfailed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "ndcube/wcs/wrappers/tests/"
                                                    "test_resampled_wcs.py::"
                                                    test))
                      ;; ResampledLowLevelWCS Transformation
                      ;; This transformation has 2 pixel and 2 world dimensions
                      ;;
                      ;; - Array shape (Numpy order): (2.3333333333333335, 15.0)
                      ;; + Array shape (Numpy order): (2, 15)
                      (list "test_2d[celestial_2d_ape14_wcs]"
                            "test_2d[celestial_2d_fitswcs]")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-dask
           python-pytest
           python-pytest-astropy
           python-pytest-mpl
           ;; python-pytest-memray ; not packaged yet
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-specutils
           python-sunpy-minimal))
    (propagated-inputs
     (list python-astropy
           python-gwcs
           python-numpy
           python-scipy
           ;; [optional]
           python-matplotlib
           python-mpl-animators
           python-reproject))
    (home-page "https://docs.sunpy.org/projects/ndcube/")
    (synopsis "Multi-dimensional contiguous and non-contiguous coordinate aware arrays")
    (description
     "@code{ndcube} is a package for manipulating, inspecting and visualizing
multi-dimensional contiguous and non-contiguous coordinate-aware data arrays.

It combines data, uncertainties, units, metadata, masking, and coordinate
transformations into classes with unified slicing and generic coordinate
transformations and plotting/animation capabilities.  It is designed to handle
data of any number of dimensions and axis types (e.g. spatial, temporal,
spectral, etc.) whose relationship between the array elements and the real
world can be described by @acronym{WCS, World Coordinate System}
translations.")
    (license license:bsd-2)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-ndcube-minimal
  (package/inherit python-ndcube
    (name "python-ndcube-minimal")
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))))

(define-public python-ogadf-schema
  (package
    (name "python-ogadf-schema")
    (version "0.2.4.post1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ogadf_schema" version))
       (sha256
        (base32 "02v5r5vnr40ggp2mccxnx600gkl3mvv77zslpf9dj96jcd8pk117"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f))        ;no tests
    (native-inputs
     (list python-poetry-core
           python-setuptools))
    (propagated-inputs
     (list python-fits-schema))
    (home-page "https://github.com/open-gamma-ray-astro/ogadf-schema")
    (synopsis
     "Definition of the open gamma ray astronomy data formats using fits-schema")
    (description
     "This package provides schema definitions for the Data Formats For Gamma-Ray
Astronomy.")
    (license license:expat)))

(define-public python-orbitalpy
  (package
    (name "python-orbitalpy")
    ;; 0.7.0 was placed in 2015, project does not tag new releases any
    ;; longer. It's not the latest commit, which needs SciPy 1.13+.
    (properties '((commit . "d6d3fa4f0b372a1d0ed64c52765c22b74fbefca6")
                  (revision . "0")))
    (version (git-version "0.7.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/RazerM/orbital")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dpahlqy7xmdqbzs9hbz1x5sm8sv7g2krccaj3r4ykfarv8lbah7"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-represent
           python-scipy
           python-sgp4))
    (home-page "https://github.com/RazerM/orbital")
    (synopsis "High level orbital mechanics")
    (description
     "Orbital is a high level orbital mechanics package for Python.")
    (license license:expat)))

(define-public python-petrofit
  (package
    (name "python-petrofit")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch) ; no tests data in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/PetroFit/petrofit")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07dri6r6ws82nn379gqvg899g576n8skhgp5fjg3qq38rp8dgl0k"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-hatch-vcs
           python-hatchling
           python-pytest
           python-pytest-randomly
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list jupyter
           python-astropy
           python-cython
           python-extension-helpers
           python-ipywidgets
           python-matplotlib
           python-notebook
           python-numpy
           python-photutils
           python-pytest-astropy-header
           python-pyyaml
           python-regions
           python-scikit-image
           python-scikit-learn
           python-scipy))
    (home-page "https://github.com/PetroFit/petrofit")
    (synopsis "Petrosian properties and fitting galaxy light profiles calculation")
    (description
     "PetroFit is a package for calculating Petrosian properties,
such as radii and concentration indices, as well as fitting galaxy light
profiles.  In particular, PetroFit includes tools for performing accurate
photometry, segmentations, Petrosian profiling, and Sérsic fitting.")
    (license license:bsd-3)))

(define-public python-photutils
  (package
    (name "python-photutils")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "photutils" version))
       (sha256
        (base32 "1xyrnf0ynh8l3dad8s93dyqk3m9gvmxgvrji1nb9yillfzvjfxcl"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1843 passed, 22 skipped
      #:test-flags
      #~(list "--pyargs" "photutils"
              "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-vv" test-flags))))))))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-astropy
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-bottleneck
           python-gwcs
           python-matplotlib
           python-numpy
           python-rasterio
           python-regions
           python-scikit-image
           python-scipy
           python-shapely
           python-tqdm))
    (home-page "https://github.com/astropy/photutils")
    (synopsis "Source detection and photometry")
    (description
     "Photutils is an Astropy package for detection and photometry of
astronomical sources.")
    (license license:bsd-3)))

(define-public python-pint-pulsar
  (package
    (name "python-pint-pulsar")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch) ; no tests data in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/nanograv/PINT")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0522p1gns52vzgj1l89s1s3idi40910hv4dpbhy4r1ijmwfb3kdg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; Tests failing with assertion on not correct precision
                    ;; or missing data files.
                    (list "not test_astropy_observatory"
                          "test_copy_wideband_fitter_object"
                          "test_IERS_B_builtin_agree_with_IERS_Auto_dX"
                          "test_astropy_observatory"
                          "test_time_construction_jds_exact[tdb]"
                          "test_copy_toa_object"
                          "test_copy_residuals"
                          "test_copy_fitter_object")
                    " and not ")
              ;; XXX: The most of the tests require additional data, select
              ;; files where they may run without it and check how to enable
              ;; more.
              "tests/test_Galactic.py"
              "tests/test_all_component_and_model_builder.py"
              "tests/test_astrometry.py"
              "tests/test_astropy_observatory.py"
              "tests/test_astropy_times.py"
              "tests/test_astropy_version.py"
              "tests/test_binary_generic.py"
              "tests/test_binconvert.py"
              "tests/test_compare.py"
              "tests/test_compare_model.py"
              "tests/test_compare_model_ecl_vs_icrs.py"
              "tests/test_convert_parfile.py"
              "tests/test_copy.py"
              "tests/test_covariance_matrix.py"
              "tests/test_datafiles.py"
              "tests/test_derived_quantities.py"
              "tests/test_derivedparams.py"
              "tests/test_dmxrange_add_sub.py"
              "tests/test_erfautils.py"
              "tests/test_eventstats.py"
              "tests/test_funcpar.py"
              "tests/test_kepler.py"
              "tests/test_leapsec.py"
              "tests/test_model_manual.py"
              "tests/test_numpy.py"
              "tests/test_observatory_envar.py"
              "tests/test_observatory_metadata.py"
              "tests/test_parfile.py"
              "tests/test_parunits.py"
              "tests/test_pb.py"
              "tests/test_phase.py"
              "tests/test_pickle.py"
              "tests/test_plk_widget.py"
              "tests/test_plot_utils.py"
              "tests/test_plrednoise.py"
              "tests/test_pmtransform_units.py"
              "tests/test_precision.py"
              "tests/test_prefix_param_inheritance.py"
              "tests/test_priors.py"
              "tests/test_process_parfile.py"
              "tests/test_pulsar_mjd.py"
              "tests/test_pulsar_position.py"
              "tests/test_reduced_precision.py"
              "tests/test_satobs.py"
              "tests/test_t2binary2pint.py"
              "tests/test_tcb2tdb.py"
              "tests/test_templates.py"
              "tests/test_variety_parfiles.py"
              "tests/test_version.py")
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'remove-deprecated-scripts
           ;; ImportError: cannot import name 'CompositeMCMCFitter' from
           ;; 'pint.mcmc_fitter'.
           ;;
           ;; Removed in 1.1.3: Broken fitter class `CompositeMCMCFitter`
           ;; (this fitter was added seemingly to deal with combined radio and
           ;; high-energy datasets, but has since been broken for a while.)
           (lambda _
             (substitute* "pyproject.toml"
               (("event_optimize_multiple.*") "")))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-versioneer
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-corner
           python-emcee
           python-jplephem
           python-loguru
           python-matplotlib
           python-nestle
           python-numdifftools
           python-numpy
           python-pyerfa
           python-scipy
           python-uncertainties))
    (home-page "https://github.com/nanograv/PINT")
    (synopsis "Software for high-precision pulsar timing")
    (description
     "PINT is not TEMPO3 - package providing a Pulsar Timing, written in
Python from scratch.
Features:
@itemize
@item a robust system to produce high-precision timing results that is
completely independent of TEMPO and Tempo2
@item a system that is easy to extend and modify due to a good design
and the use of a modern programming language, techniques, and libraries
@end itemize")
    (license license:bsd-3)))

(define-public python-pixell
  (package
    (name "python-pixell")
    (version "0.31.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pixell" version))
       (sha256
        (base32 "0i09gv19mpw0mr3l9yyvmq7x2ln57fmc87qz863d9blw3g1rj5za"))))
    (build-system pyproject-build-system)
    ;; tests: 41 passed, 2 warnings
    (native-inputs
     (list gfortran
           meson-python
           perl ; fortran/Makefile
           pkg-config
           python-cython
           python-h5py
           python-pytest
           python-pyyaml))
    (propagated-inputs
     (list python-astropy
           python-dateutil
           python-ducc0
           python-ephem
           python-healpy
           python-matplotlib
           python-numba
           python-numpy
           python-pillow
           python-scipy))
    (home-page "https://pixell.readthedocs.io/")
    (synopsis "Tectangular pixel map manipulation and harmonic analysis")
    (description
     "pixell is a library for loading, manipulating and analyzing maps stored
in rectangular pixelization.  It is mainly intended for use with maps of the
sky (e.g. CMB intensity and polarization maps, stacks of 21 cm intensity maps,
binned galaxy positions or shear) in cylindrical projection, but its core
functionality is more general.")
    (license license:bsd-3)))

;; XXX: The project is archived, maintained fork is available see
;; <https://github.com/poliastro/poliastro/issues/1640>.
;; Maintained fork <https://github.com/pleiszenburg/hapsira>.
(define-public python-poliastro
  (package
    (name "python-poliastro")
    (version "0.17.0")
    (source
     (origin
       ;; PyPi tarball lacks tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/poliastro/poliastro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iclyjp0cvm6hp5qf4fzklszxvhj3idkxgb6a9h7xzg9bf5j5gi2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-m" "not remote_data"
              ;; TODO: Review failing tests later when any upstream
              ;; suggestions are provided:
              ;; https://github.com/poliastro/poliastro/issues/1618
              "--ignore=tests/test_czml.py"
              "-k" (string-append
                    ;; This fails with "ufunc 'isfinite' not
                    ;; supported for the input types"
                    "not test_porkchop_plotting"
                    " and not test_maneuver_constructor_raises_error_if_invalid_delta_v"))
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               (("5.0,<6") "5.0,<7"))))
          (add-after 'unpack 'matplotlib-compatibility
            (lambda _
              (substitute* "src/poliastro/plotting/static.py"
                (("import numpy as np.*" m)
                 (string-append m "\
import itertools\n"))
                (("( +)self._ax = ax.*" m indent)
                 (string-append m indent
                                "\
self.colors = itertools.cycle(plt.rcParams[\"axes.prop_cycle\"].by_key()[\"color\"])\n"))
                (("color = next\\(self._ax._get_lines.prop_cycler\\)\\[\"color\"\\]")
                 "color = next(self.colors)"))))
          ;; NOTE: Tests take about 7-10 minutes to pass.
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-coverage
           python-hypothesis
           python-mypy
           python-flit-core
           python-pytest
           python-pytest-cov
           python-pytest-doctestplus
           python-pytest-mpl
           python-pytest-mypy))
    (propagated-inputs
     (list python-astropy-6
           python-astroquery
           python-czml3
           python-jplephem
           python-matplotlib
           python-numba
           python-numpy
           python-pandas
           python-plotly
           python-pyerfa
           python-scipy))
    (home-page "https://www.poliastro.space/")
    (synopsis "Astrodynamics in Python")
    (description
     "POLIASTRO is a Python library for interactive Astrodynamics and Orbital
Mechanics, with a focus on ease of use, speed, and quick visualization.  It
provides a simple and intuitive API, and handles physical quantities with
units.

Some features include orbit propagation, solution of the Lambert's problem,
conversion between position and velocity vectors and classical orbital
elements and orbit plotting, among others.  It focuses on interplanetary
applications, but can also be used to analyze artificial satellites in
Low-Earth Orbit (LEO).")
  (license license:expat)))

(define-public python-poppy
  (package
    (name "python-poppy")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "poppy" version))
       (sha256
        (base32 "0mvnd9rlglb1cqhaavd2lyxnvi4xmc133x50rzzlh00xn0gyxgfq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "--ignore=docs/exts/numfig.py")))
    (native-inputs
     (list python-docutils
           python-pytest
           python-pytest-astropy
           python-pytest-xdist
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     ;; XXX: With python-synphot (marked as optional) package added to the list
     ;; it tries to download from remote host during tests and fails. Overall
     ;; tests take up to 5-8min to pass.
     (list python-astropy
           python-matplotlib
           python-numexpr
           python-numpy
           python-scipy))
    (home-page "https://poppy-optics.readthedocs.io/")
    (synopsis "Physical Optics Propagation in Python")
    (description
     "@acronym{POPPY, Physical Optics Propagation in Python} is a Python package that
simulates physical optical propagation including diffraction.  It implements a
flexible framework for modeling Fraunhofer and Fresnel diffraction and point
spread function formation, particularly in the context of astronomical
telescopes.

POPPY was developed as part of a simulation package for the James Webb Space
Telescope, but is more broadly applicable to many kinds of imaging simulations.
It is not, however, a substitute for high fidelity optical design software such
as Zemax or Code V, but rather is intended as a lightweight alternative for
cases for which diffractive rather than geometric optics is the topic of
interest, and which require portability between platforms or ease of scripting.")
    (license license:bsd-3)))

(define-public python-pvextractor
  (package
    (name "python-pvextractor")
    (version "0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pvextractor" version))
       (sha256
        (base32 "1kl33vg5rxmdwlk36pn8zqa7k7f43fb7w417fym6ygp86mci2spd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "pvextractor")
      #:phases
      #~(modify-phases %standard-phases
          ;; See <https://github.com/radio-astro-tools/pvextractor/issues/124>.
         (add-after 'unpack 'patch-regexp
           (lambda _
             (substitute* "pvextractor/pvregions.py"
               (("coordre = re.compile.*")
                "coordre = re.compile(\"^[a-z]*\\\\((.*)\\\\)\")\n"))))
           (add-before 'check 'prepare-x
             (lambda _
               (system "Xvfb &")
               (setenv "DISPLAY" ":0")
               (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-pyqt-6
           python-qtpy
           python-scipy
           python-spectral-cube))
    (native-inputs
     (list python-pytest-astropy
           python-setuptools-scm
           xorg-server-for-tests))
    (home-page "http://pvextractor.readthedocs.io")
    (synopsis "Position-Velocity diagram extractor")
    (description
     "The concept of the pvextractor package is simple - given a path defined
in sky coordinates, and a spectral cube, extract a slice of the cube along
that path, and along the spectral axis, producing a position-velocity or
position-frequency slice.")
    (license license:bsd-3)))

(define-public python-pvextractor-bootstrap
  (hidden-package
   (package
     (inherit python-pvextractor)
     (arguments
      (list #:tests? #f
            #:phases #~(modify-phases %standard-phases
                         (delete 'sanity-check))))
     (propagated-inputs '())
     (native-inputs
      (list python-setuptools
            python-wheel)))))

(define-public python-pyavm
  (package
    (name "python-pyavm")
    (version "0.9.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyAVM" version))
       (sha256
        (base32 "0vgjqvddq4a5lnmg8msm7fwqs3r6fc748xzvnhyvc387h0z8pdxk"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pillow
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy))
    (home-page "https://astrofrog.github.io/pyavm/")
    (synopsis "Simple pure-python AVM meta-data handling")
    (description
     "PyAVM is a module to represent, read, and write metadata following the
@acronym{AVM, Astronomy Visualization Metadata} standard provided by
@url{https://www.virtualastronomy.org/avm_metadata.php, vamp} project.")
    (license license:expat)))

(define-public python-pycpl
  (package
    (name "python-pycpl")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.eso.org/pub/dfs/pipelines/libraries/pycpl/pycpl-"
             version ".tar.gz"))
       (sha256
        (base32 "0kfzx4k8z2k0ms6q8f16wqr120drd8fqrw9qpnks419pyc8cr5xp"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list cmake-minimal
           pybind11
           python-pandas
           python-pytest
           python-scipy
           python-setuptools))
    (inputs
     (list cpl))
    (propagated-inputs
     (list python-astropy))
    (home-page "https://www.eso.org/sci/software/pycpl/pycpl-site/")
    (synopsis "Python bindings for ESO's CPL")
    (description
     "PyCPL provides Python3 language bindings for the complete programming
API of the @acronym{European Southern Observatory, ESO} @acronym{Common
Pipeline Library, CPL} toolkit, including the CPL plugin interface.")
    (properties '((upstream-name . "pycpl")))
    (license license:bsd-3)))

(define-public python-pydl
  (package
    (name "python-pydl")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pydl" version))
       (sha256
        (base32 "0z08c05qf0ix4h348n0hhxixn1wipdm55g9z6qys24z41ywf69jh"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-astropy
           python-pytest-mock
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-scipy))
    (home-page "https://github.com/weaverba137/pydl")
    (synopsis "IDL astronomy routines converted to Python")
    (description
     "This package consists of Python replacements for functions that are part
of the @url{https://www.nv5geospatialsoftware.com/Products/IDL, IDL} built-in
library or part of astronomical IDL libraries.  The emphasis is on reproducing
results of the astronomical library functions.  Only the bare minimum of IDL
built-in functions are implemented to support this.")
    (license license:bsd-3)))

(define-public python-pyerfa
  (package
    (name "python-pyerfa")
    (version "2.0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyerfa" version))
       (sha256
        (base32 "1h7nw61wqx9qsznnl8qandixr6c1n3f65hyqwzanav44wi7v5mhp"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Remove bundled submodule library.
            (delete-file-recursively "liberfa")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable only one failing test:
      ;; AttributeError: __warningregistry__
      ;; See https://github.com/liberfa/pyerfa/issues/126
      #:test-flags #~(list "-k" "not test_errwarn_reporting")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'use-system-liberfa
            (lambda _
              (setenv "PYERFA_USE_SYSTEM_LIBERFA" "1")))
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-pytest-doctestplus
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (inputs
     (list erfa))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/liberfa/pyerfa")
    (synopsis "Python bindings for ERFA")
    (description
     "PyERFA is the Python wrapper for the ERFA library (Essential
Routines for Fundamental Astronomy), a C library containing key algorithms for
astronomy, which is based on the SOFA library published by the International
Astronomical Union (IAU).  All C routines are wrapped as Numpy universal
functions, so that they can be called with scalar or array inputs.")
    (license license:bsd-3)))

(define-public python-pyesorex
  (package
    (name "python-pyesorex")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://ftp.eso.org/pub/dfs/pipelines/libraries/pyesorex/pyesorex-"
             version ".tar.gz"))
       (sha256
        (base32 "1ynb9q9aj51mdva1b76fkz7mlw5q8nlfs8f5f70bhila8iincjca"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-pycpl))
    (native-search-paths
     (list (search-path-specification
            (variable "PYESOREX_PLUGIN_DIR")
            (files '("lib/esopipes-plugins")))))
    (home-page "https://www.eso.org/sci/software/pycpl/pycpl-site/")
    (synopsis "Alternative Python CLI for ESO's Rex")
    (description
     "PyEsoRex is a command line tool which can serve as a drop-in replacement
of EsoRex, which can execute both, existing pipeline recipes implemented using
the @acronym{Common Pipeline Library, CPL} C API, and recipes implemented
using the PyCPL Python API.")
    (properties '((upstream-name . "pyesorex")))
    (license license:bsd-3)))

(define-public python-pyhalo
  (package
    (name "python-pyhalo")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyhalo" version))
       (sha256
        (base32 "1yh5acjiwkfm4sjiydksg9187k3lwsads1p9zy0ck7wb6jwxrj81"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 194 passed, 3 deselected, 119 warnings
      ;; ValueError: The truth value of an array with more than one element
      ;; is ambiguous. Use a.any() or a.all()
      #:test-flags #~(list "-k" "not test_vmax")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-setupup.py
            ;; XXX: ModuleNotFoundError: No module named 'pyHalo.cli'.
            (lambda _
              (substitute* "setup.py"
                ((".pyHalo=pyHalo.cli:main.*") ""))))
          (add-before 'check 'pre-check
            (lambda _
              ;; RuntimeError: cannot cache function 'rotate': no locator
              ;; available for file '<...>/lenstronomy/Util/util.py'.
              (setenv "NUMBA_CACHE_DIR" "/tmp")
              ;; PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-click
           python-colossus
           python-h5py
           python-lenstronomy
           python-mcfit
           python-numpy
           python-scipy))
    (home-page "https://github.com/dangilman/pyHalo")
    (synopsis "Render populations of dark matter halos")
    (description
     "pyHalo renders full mass distributions for substructure lensing
simulations with gravitational lensing software package lenstronomy.  The main
purpose of the code is to quickly render full populations of dark matter
subhalos and line of sight halos for gravitational lensing simulations.  It
also transltes halo properties (mass, concentration, redshift, etc) into
angular units for lensing computations with lenstronomy.")
    (license license:expat)))

(define-public python-pyirf
  (package
    (name "python-pyirf")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyirf" version))
       (sha256
        (base32 "17z8czdqk6c742ww9274x9inx0q201908la0i27gv7r1496l0l0b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; It fails to load optional Gammapy, remove when it's packaged.
      #:test-flags
      #~(list "--ignore=pyirf/io/tests/test_gadf.py"
              ;; fixture 'prod5_irfs' not found
              "-k" (string-append "not test_EnergyDispersionEstimator"
                                  " and not test_EffectiveAreaEstimator_prod5"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (delete-file "pyirf/conftest.py"))))))
    (native-inputs
     (list python-ogadf-schema
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           ;; python-gammapy    ;not packaged yet in Guix
           python-numpy
           python-packaging
           python-scipy
           python-tqdm))
    (home-page "https://github.com/cta-observatory/pyirf")
    (synopsis "Python IRF builder")
    (description
     "This package provides a python library to calculate IACT IRFs
and Sensitivities.")
    (license license:expat)))

(define-public python-pynbody
  (package
    (name "python-pynbody")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pynbody/pynbody")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a0m42mvdnmlkm8q6704xamnim1q5wkw29p89ikj1axpl2mmq0d3"))
       (modules '((guix build utils)))
       (snippet
        ;; Symlink goes to not existing directory.
        #~(for-each delete-file '("docs/testdata"
                                  "docs/tutorials/example_code/testdata")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 92 passed, 6 skipped, 10 deselected
      #:test-flags
      ;; XXX: testdata is distributed via Zenodo
      ;; <https://zenodo.org/records/17084976> with total size more than 2GiB,
      ;; refer to the helper script obtaining it
      ;; <pynbody/test_utils/__init__.py>.
      #~(list #$@(map (lambda (file) (string-append "--ignore=tests/" file))
                      (list "adaptahop_test.py"
                            "ahf_halos_test.py"
                            "array_test.py"
                            "bridge_test.py"
                            "copy_on_access_test.py"
                            "family_test.py"
                            "gadget_test.py"
                            "gadgethdf_test.py"
                            "gizmo_test.py"
                            "grafic_test.py"
                            "gravity_test.py"
                            "halos_test.py"
                            "halotools_test.py"
                            "hbtplus_test.py"
                            "hmf_test.py"
                            "hop_test.py"
                            "ionfrac_test.py"
                            "kdtree_test.py"
                            "luminosity_test.py"
                            "morphology_test.py"
                            "nchilada_test.py"
                            "partial_tipsy_test.py"
                            "pkdgravhdf_test.py"
                            "plot_stars_test.py"
                            "profile_test.py"
                            "ramses_new_ptcl_format_test.py"
                            "ramses_test.py"
                            "rockstar_test.py"
                            "schmidtlaw_test.py"
                            "snapshot_test.py"
                            "sph_image_test.py"
                            "subfind_test.py"
                            "subfindhdf_gadget4_test.py"
                            "subfindhdf_test.py"
                            "swift_test.py"
                            "tipsy_test.py"
                            "velociraptor_test.py"))
              "--deselect=tests/shape_test.py::test_2D_shape"
              "--deselect=tests/shape_test.py::test_3D_shape"
              "--deselect=tests/shape_test.py::test_halo_shape_wrapper"
              "--deselect=tests/test_profile.py::test_unique_hash_generation"
              "--deselect=tests/test_profile.py::test_write_profile"
              ;; DeprecationWarning: backend2gui is deprecated since IPython
              ;; 8.24, backends are managed in matplotlib and can be
              ;; externally registered.
              "--deselect=tests/plot_hist2d_test.py::test_hist2d"
              "--deselect=tests/plot_hist2d_test.py::test_hist2d_massweight"
              "--deselect=tests/schmidtlaw_test.py::test_full_disc"
              "--deselect=tests/schmidtlaw_test.py::test_truncated_disc")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (mkdir-p "testdata/empty_folder_0001")
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-cython
           python-ipython-minimal
           python-pandas
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-certifi
           python-h5py
           python-matplotlib
           python-numpy
           python-osfclient
           python-scipy))
    (home-page "https://pynbody.readthedocs.io/latest/")
    (synopsis "Light-weight astronomical N-body/SPH analysis for python")
    (description
     "@code{Pynbody} is an analysis framework for N-body and hydrodynamic
astrophysical simulations supporting PKDGRAV/Gasoline, Gadget, Gadget4/Arepo,
N-Chilada and RAMSES AMR outputs.")
    (license license:gpl3+)))

(define-public python-pynbody-1
  (package
    (inherit python-pynbody)
    (name "python-pynbody")
    (version "1.6.0.post0")
    (source
     (origin
       (inherit (package-source python-pynbody))
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pynbody/pynbody")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rl9ais4yc6kdijq1il4vi3mplp7z6bcih4x55axhan5n3n7riwi"))))))

(define-public python-pypeit
  (package
    (name "python-pypeit")
    (version "1.17.4") ;XXX: Newer versions need to be built with NumPy 2+
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pypeit/PypeIt")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16y4vkjmd29zrjhhv2fyv9rksjblri3zf6m81mhmhys5sy09xmfz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 242 passed, 10 skipped, 5 deselected, 120 warnings
      #:test-flags
      ;; Tests still try to download test data even it's available in
      ;; "pypeit/data".
      #~(list "--deselect=pypeit/tests/test_install.py::test_install_telluric"
              #$@(map (lambda (test) (string-append "--deselect=pypeit/"
                                                    "tests/test_pkgdata.py::"
                                                    test))
                      (list "test_cloud_url"
                            "test_fetch_github_files"
                            "test_github_contents"
                            "test_cache_to_pkg")))
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: See: <https://github.com/pypeit/PypeIt/issues/1786>.
          (add-after 'unpack 'remove-missing-scripts-entry-points
            (lambda _
              (substitute* "setup.cfg"
                ((".*pypeit_install_ql_calibs.*") "")
                ((".*pypeit_ql_multislit.*") ""))))
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-after 'install 'include-package-data
            ;; XXX: PyPI archive provides pypeit/data but during build from
            ;; Git it's ignored for some reason, add it manually.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (copy-recursively "pypeit/data"
                                (string-append (site-packages inputs outputs)
                                               "/pypeit/data"))))
          (add-before 'check 'pre-check
            (lambda _
              (system "Xvfb &")
              (setenv "DISPLAY" ":0")
              (setenv "HOME" "/tmp")
              (setenv "MPLBACKEND" "agg")))
          (add-before 'check 'post-check
            (lambda _
              (for-each delete-file-recursively
                        (find-files #$output "__pycache__" #:directories? #t)))))))
    (native-inputs
     (list nss-certs-for-test
           python-cython
           python-pygit2
           python-pytest
           python-scikit-image
           python-setuptools
           python-setuptools-scm
           python-shapely
           python-specutils
           xorg-server-for-tests))
    (propagated-inputs
     (list python-astropy-6
           python-bottleneck
           python-configobj
           python-extension-helpers
           python-fast-histogram
           python-ginga
           python-ipython
           python-linetools
           python-matplotlib
           python-numpy
           python-packaging
           python-pyerfa
           python-pygithub
           python-pyqt-6
           python-pyyaml
           python-qtpy
           python-scikit-learn
           python-scipy))
    (home-page "https://github.com/pypeit/PypeIt")
    (synopsis "Spectroscopic Data Reduction Pipeline")
    (description
     "PypeIt is a Python package for semi-automated reduction of astronomical
spectroscopic data.  Its algorithms build on decades-long development of
previous data reduction pipelines by the developers.

It is designed to be used by both advanced spectroscopists with prior data
reduction expertise and astronomers with no prior experience of data
reduction.  It is highly configurable and designed to be applied to any
standard slit-imaging spectrograph, including long-slit, multi-slit, as well
as cross-dispersed echelle spectra.")
    (license license:bsd-3)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-pypeit-minimal
  (package/inherit python-pypeit
    (name "python-pypeit-minimal")
    (arguments
     (substitute-keyword-arguments
         (package-arguments python-pypeit)
       ((#:tests? _ #t) #f)
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (delete 'pre-check)))))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))))

(define-public python-pyregion
  (package
    (name "python-pyregion")
    (version "2.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyregion" version))
       (sha256
        (base32 "09a98v3zk1vdjns1q64al58mapr4cns3nlnyi6b26wqi888qfjg8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (with-directory-excursion #$output
                (apply invoke "pytest" "-vv" test-flags))))
          (add-before 'check 'post-check
            (lambda _
              (for-each delete-file-recursively
                        (find-files #$output "__pycache__" #:directories? #t)))))))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-pyparsing))
    (home-page "https://github.com/astropy/pyregion")
    (synopsis "Python parser for ds9 region files")
    (description
     "@code{pyregion} is a python module to parse ds9 region files.  It also
supports ciao region files.
Features:
@itemize
@item ds9 and ciao region files.
@item (physical, WCS) coordinate conversion to the image coordinate.
@item convert regions to matplotlib patches.
@item convert regions to spatial filter (i.e., generate mask images)
@end itemize")
    (license license:expat)))

(define-public python-pysat
  (package
    (name "python-pysat")
    (version "3.2.2")
    (source
     (origin
       (method git-fetch) ; no tests data in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/pysat/pysat")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gm71zafigwc94s37wqyl86yjabpq6wx9izwxag74wg1ynhqyvf0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--durations=10" ; report 10 slowest tests
              ;; Tests require pysatSpaceWeather which is not packed yet.
              "--ignore=pysat/tests/test_utils_files.py"
              "-k" "not test_from_os")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; Do not run test coverage.
              (substitute* "pyproject.toml"
                ((".*addopts.*cov.*") ""))
              ;; No such file or directory: '/homeless-shelter/.pysat'
              (setenv "HOME" "/tmp")
              (mkdir "pysatData")
              (invoke "python" "-c"
                      "import pysat; pysat.params['data_dirs'] = 'pysatData'"))))))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-dask
           python-netcdf4
           python-numpy
           python-pandas
           python-portalocker
           python-scipy
           python-toolz
           python-xarray))
    (home-page "https://github.com/pysat/pysat")
    (synopsis "Supports science analysis across disparate data platforms")
    (description
     "The Python Satellite Data Analysis Toolkit (pysat) provides a simple and
flexible interface for robust data analysis from beginning to end - including
downloading, loading, cleaning, managing, processing, and analyzing
data.  Pysat's plug-in design allows analysis support for any data, including
user provided data sets.")
    (license license:bsd-3)))

(define-public python-pysiaf
  (package
    (name "python-pysiaf")
    (version "0.25.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pysiaf" version))
       (sha256
        (base32 "17r06dzx8r12llrhjlp23mmdb5bw4ydmnswdzyincvsz1gc55f9r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 32 passed, 2 deselected
      #:test-flags
      ;; Disable 2 failing tests, see
      ;; <https://github.com/spacetelescope/pysiaf/issues/338>
      #~(list "-k" (string-append "not test_write_jwst_siaf_xlsx"
                                  " and not test_write_jwst_siaf_xml" ))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-lxml
           python-matplotlib
           python-numpy
           python-openpyxl
           python-requests
           python-scipy))
    (home-page "https://pysiaf.readthedocs.io/")
    (synopsis "Handling SIAF for space telescopes")
    (description
     "@code{pysiaf} is a python package to access, interpret, maintain, and
generate @acronym{Handling of Science Instrument Aperture Files, SIAF}, in
particular for JWST.  Tools for applying the frame transformations, plotting,
comparison, and validation are provided.")
    (license license:bsd-3)))

(define-public python-pysiril
  (package
    (name "python-pysiril")
    (version "0.0.17")
    (source
     (origin
       (method git-fetch) ; not published on PyPI
       (uri (git-reference
             (url "https://gitlab.com/free-astro/pysiril")
             (commit (string-append
                      "V" (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w8zc6gm8a18l4rvjv22a0l6m2c45bzj1f2m6sbshaq9z30bm0vv"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests
    (native-inputs
     (list python-setuptools
           python-wheel))
    (home-page "https://siril.org/tutorials/pysiril/")
    (synopsis "Python interface to SiriL")
    (description
     "This package provides an extending scripting capabilities, present
natively in Siril.")
    (license license:gpl3)))

(define-public python-pysm3
  (package
    (name "python-pysm3")
    (version "3.4.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/galsci/pysm")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qi01g71m0biqchdy1v7sk54kg1w3s75qfbf2s50ifsmprajjs5r"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 44 passed, 2 skipped, 97 deselected, 3 warnings
      #:test-flags
      ;; XXX: Tests requiring additional FITS files, check if they may be
      ;; packages:
      ;; - <http://www.astropy.org/astropy-data>
      ;; - <https://github.com/healpy/healpy-data>
      ;; - <https://healpy.github.io/healpy-data>
      ;; - <https://portal.nersc.gov/project/cmb/pysm-data>
      #~(list "-k" (string-join
                    (list "not test_bandpass_unit_conversion"
                          "test_bandpass_unit_conversion_CMB2MJysr"
                          "test_bandpass_unit_conversion_MJysr2KRJ"
                          "test_cmb_lensed"
                          "test_cmb_lensed_delens"
                          "test_cmb_lensed_l01"
                          "test_cmb_lensed_no_delens"
                          "test_cmb_lensed_with_patch_object"
                          "test_cmb_map"
                          "test_cmb_map_bandpass"
                          "test_co"
                          "test_co_model"
                          "test_d10_vs_d11"
                          "test_dust_model"
                          "test_dust_model_353"
                          "test_gnilc_857"
                          "test_healpix_output_nside"
                          "test_highfreq_dust_model"
                          "test_model"
                          "test_model_d12"
                          "test_no_quadrupole"
                          "test_presmoothed"
                          "test_print_quadrupole_amplitudes"
                          "test_quadrupole_corrected_freqs"
                          "test_read_map_unit"
                          "test_read_map_unit_dimensionless"
                          "test_s6_vs_s5"
                          "test_sky_max_nside"
                          "test_sky_max_nside_highres"
                          "test_smoothing_healpix"
                          "test_smoothing_healpix_beamwindow"
                          "test_synch_44"
                          "test_synch_model_noscaling"
                          "test_synch_model_s7_44"
                          "test_synch_model_s7_noscaling"
                          "test_synchrotron_model"
                          ;; RuntimeError: This function has been removed. Use
                          ;; reproject.healpix2map(...method='harm').
                          "test_car_nosmoothing")
                    " and not ")
              "tests")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list nss-certs-for-test
           python-hatch-vcs
           python-hatchling
           python-nbval
           python-netcdf4
           python-pixell
           python-psutil
           python-pytest
           python-pytest-astropy
           python-setuptools-scm
           python-xarray))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-healpy
           python-numba
           python-numpy
           python-scipy
           python-toml))
    (home-page "https://pysm3.readthedocs.io/")
    (synopsis "Sky emission simulations for Cosmic Microwave Background experiments")
    (description
     "PySM generates full-sky simulations of Galactic emissions in intensity
and polarization relevant to @acronym{Cosmic Microwave Background, CMB}
experiments.  It is a large refactor of
@url{https://github.com/bthorne93/PySM_public, PySM 2} focused on reducing
memory usage, improving performance and run in parallel with MPI.")
    (license license:bsd-3)))

(define-public python-pysynphot
  ;; XXX: 2.0.0 was released in 2021 there are a lot of changes since that
  ;; time and it failed to build with python-astropy 6.0.0, use the latest
  ;; upstream commit for now.
  (let ((commit "54e9e2a624910c4d177ca70f8e9fb8110c8fae5b")
        (revision "0"))
    (package
      (name "python-pysynphot")
      (version (git-version "2.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/spacetelescope/pysynphot")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "09sivpfqilk86zp8k5wmrs4g48m4kypn34jcy95y5h4ygbn5zbzy"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-version
              (lambda _
                (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" "2.0.0")))
            (add-before 'check 'set-env-data-path
              (lambda _
                (setenv "PYSYN_CDBS" (string-append #$output "/crds")))))))
      (native-inputs
       (list python-pytest
             python-pytest-remotedata
             python-setuptools
             python-setuptools-scm
             python-wheel))
      (propagated-inputs
       (list python-astropy
             python-beautifulsoup4
             python-numpy
             python-pytest-astropy-header
             python-six))
      (home-page "https://github.com/spacetelescope/pysynphot")
      (synopsis "Python Synthetic Photometry Utilities")
      (description
       "Astrolib PySynphot (hereafter referred to only as pysynphot) is an
object-oriented replacement for STSDAS SYNPHOT synthetic photometry package in
IRAF.  @code{pysynphot} simulates photometric data and spectra as they are
observed with the Hubble Space Telescope (HST).  Passbands for standard
photometric systems are available, and users can incorporate their own filters,
spectra, and data.")
      (license license:bsd-3))))

(define-public python-pyvo
  (package
    (name "python-pyvo")
    (version "1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/astropy/pyvo")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ks1542i85y042yny3ka6gsrgy7pyv0xdaclirmvqrkakikhv70b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 430 passed, 56 skipped, 1 xfailed
      #:test-flags
      #~(list "--pyargs" "pyvo")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-after 'install 'include-package-data
            ;; FIXME: Check why pyproject-build-system ignores coping some
            ;; package data files during build/install phases.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (for-each
               (lambda (file)
                 (install-file file
                            (string-append (site-packages inputs outputs)
                                           "/pyvo/mivot/writer/")))
               (find-files "pyvo/mivot/writer" "\\.xsd$"))))
          (add-before 'check 'post-check
            (lambda _
              (for-each delete-file-recursively
                        (find-files #$output "__pycache__" #:directories? #t)))))))
    (native-inputs
     (list python-pytest-astropy
           python-pytest-doctestplus
           python-requests-mock
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-defusedxml ; extra requirements
           python-pillow     ; extra requirements
           python-requests))
    (home-page "https://github.com/astropy/pyvo")
    (synopsis "Access Virtual Observatory data and services")
    (description
     "PyVO is a package providing access to remote data and services of the
Virtual observatory (VO) using Python.")
    (license license:bsd-3)))

(define-public python-pyxsim
  (package
    (name "python-pyxsim")
    (version "4.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyxsim" version))
       (sha256
        (base32 "04wpy3gvwcw09grdqad4w4pziqhfczizcdmgsqdd52xfalb92765"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests require additional data, check if it may be packaged
      ;; separately, see tests/ci_install.sh.
      ;; - http://yt-project.org/data
      ;;   - enzo_tiny_cosmology 419MiB
      ;;   - GasSloshingLowRes 1GiB
      ;;   - FIRE_M12i_ref11 209MiB
      ;; - http://hea-www.cfa.harvard.edu/~jzuhone/
      ;;   - pyxsim22.tar.gz 12MiB
      #:tests? #f))
    (native-inputs
     (list python-cython
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-numpy
           python-scipy
           python-soxs
           python-tqdm
           python-unyt
           python-yt))
    (home-page "https://hea-www.cfa.harvard.edu/~jzuhone/pyxsim/")
    (synopsis "Simulating X-ray observations from astrophysical sources")
    (description
     "This package implements functionality for simulating X-ray emission from
astrophysical sources.

X-rays probe the high-energy universe, from hot galaxy clusters to compact
objects such as neutron stars and black holes and many interesting sources in
between.  pyXSIM makes it possible to generate synthetic X-ray observations of
these sources from a wide variety of models, whether from grid-based
simulation codes such as FLASH, Enzo, and Athena, to particle-based codes such
as Gadget and AREPO, and even from datasets that have been created 'by hand',
such as from NumPy arrays.  pyXSIM also provides facilities for manipulating
the synthetic observations it produces in various ways, as well as ways to
export the simulated X-ray events to other software packages to simulate the
end products of specific X-ray observatories.")
    (license license:bsd-3)))

(define-public python-raccoon
  (package
    (name "python-raccoon")
    (version "1.0.0")
    (source
     (origin
       ;; PyPi tarball lacks tests.
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ajshajib/raccoon")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hn8g7zxga47yhk4y9nrbnz0n7apflczlaszr79h4lg6b4v4h9f4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; Three tests fail with error: AttributeError: module
                    ;; 'numpy' has no attribute 'trapezoid'.
                    (list "not test_clean_cube"
                          "test_clean_cube_with_min_n_amplitude_and_min_n_f"
                          "test_clean_cube_with_wiggle_detection_thresholds")
                    " and not "))))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scipy
           python-statsmodels
           python-tqdm))
    (home-page "https://github.com/ajshajib/raccoon")
    (synopsis "Clean modulation due to resampling noise in the JWST/NIRSpec IFS spectra")
    (description
     "Package Raccoon cleans the \"wiggles\" (i.e., low-frequency sinusoidal
artifacts) in the JWST-NIRSpec IFS (integral field spectroscopy) data.  These
wiggles are caused by resampling noise or aliasing artifacts.")
    (license license:bsd-3)))

(define-public python-rad
  (package
    (name "python-rad")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rad" version))
       (sha256
        (base32 "0va7yfy3pcd6fp49ni7388lic3fv77n3qhvaxk6jw535v41by381"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1798 passed, 1 skipped, 17 xfailed
      #:test-flags
      ;; Ignore tests requiring python-crds to break cycle:
      ;; python-rad -> python-roman-datamodels -> python-crds -> python-rad
      #~(list "--ignore=tests/test_schemas.py"
              ;; E   git.exc.InvalidGitRepositoryError
              "--ignore=tests/test_versioning.py"
              ;; E TypeError: the JSON object must be str, bytes or bytearray,
              ;; not NoneType
              "--ignore=tests/test_latest.py")))
    (native-inputs
     (list python-pytest
           python-pytest-asdf-plugin
           python-pytest-doctestplus
           python-semantic-version
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy))
    (home-page "https://github.com/spacetelescope/rad")
    (synopsis "Roman Attribute Dictionary")
    (description
     "@acronym{RAD, The Roman Attribute Dictionary} is package which defines
schemas for the Nancy Grace Roman Space Telescope shared attributes for
processing and archive.  These schemas are schemas for the ASDF file file
format, which are used by ASDF to serialize and deserialize data for the Nancy
Grace Roman Space Telescope.")
    (license license:bsd-3)))

(define-public python-radio-beam
  (package
    (name "python-radio-beam")
    (version "0.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "radio_beam" version))
       (sha256
        (base32 "0i76hj6wyijbpxx7n1sm12f0qqw15srk6ikq2cr589lvrixylpwv"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scipy))
   (home-page "https://radio-beam.readthedocs.io/en/latest/")
    (synopsis "Operations for radio astronomy beams with Astropy")
    (description
     "Radio Beam is a simple toolkit for reading beam information from FITS
headers and manipulating beams.
Some example applications include:
@itemize
@item Convolution and deconvolution
@item Unit conversion (Jy to/from K)
@item Handle sets of beams for spectral cubes with varying resolution between
channels
@item Find the smallest common beam from a set of beams
@item Add the beam shape to a matplotlib plot
@end itemize")
    (license license:bsd-3)))

(define-public python-radiospectra
  (package
    (name "python-radiospectra")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "radiospectra" version))
       (sha256
        (base32 "14p4hp9yncyjsrbys0yjq7jbj0n9wf0x5sy67kilqrw14d1xvzch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count)))
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: It fails to check SunPy's optional inputs versions.
          (delete 'sanity-check)
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pytest-doctestplus
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-sunpy-soar
           python-wheel))
    (propagated-inputs
     (list python-cdflib
           python-matplotlib
           python-numpy
           python-scipy
           python-sunpy))
    (home-page "https://docs.sunpy.org/projects/radiospectra")
    (synopsis "Support for radio spectra on solar physics")
    (description
     "@code{radiospectra} provides support for some type of radio spectra in
solar physics.")
    (license license:bsd-2)))

(define-public python-regions
  (package
    (name "python-regions")
    (version "0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "regions" version))
       (sha256
        (base32 "0dmfvk33ywlmxr3f6p9vhr7aq4mf57l1y7ls12d6ds9dibbxl2h0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1112 passed, 8 skipped, 13 xfailed
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count)))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (with-directory-excursion #$output
                (apply invoke "pytest" "-vv" test-flags)))))))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-arraydiff
           python-pytest-astropy
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy
           ;; [optional]
           python-matplotlib
           python-shapely))
    (home-page "https://github.com/astropy/regions")
    (synopsis "Package for region handling")
    (description "Regions is an Astropy package for region handling.")
    (license license:bsd-3)))

(define-public python-regularizepsf
  (package
    (name "python-regularizepsf")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "regularizepsf" version))
       (sha256
        (base32 "1dd50dam227ifhf4d81nr0fgb9gwxrihs80svbwd86f18x5jlnwi"))))
    (build-system pyproject-build-system)
    ;; tests: 61 passed, 26 warnings
    (native-inputs
     (list python-pytest
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-matplotlib
           python-numpy
           python-scikit-image
           python-scipy
           python-sep))
    (home-page "https://github.com/punch-mission/regularizepsf")
    (synopsis "Point spread function modeling and regularization")
    (description
     "This package implements functionality of @acronym{Point Spread Function,
PSF} describing how the optical system spreads light from sources.")
    (license license:expat)))

(define-public python-reproject
  (package
    (name "python-reproject")
    (version "0.19.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reproject" version))
       (sha256
        (base32 "1jbjq54kp438zbjpmd3kxm9r4h6p7dgq11ywf3iajn0j6k4nlpds"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2288 passed, 566 skipped, 99 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; AssertionError: assert 'Broadcasting is being used' in ''
              "-k" (string-append
                    "not test_reproject_parallel_broadcasting[none]"
                    " and not test_reproject_parallel_broadcasting[None]"
                    " and not test_reproject_parallel_broadcasting[memmap]"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion #$output
                  (setenv "HOME" "/tmp")
                  (apply invoke "pytest" "-vv" test-flags)))))
          (add-before 'check 'post-check
            (lambda _
              (for-each delete-file-recursively
                        (find-files #$output "__pycache__" #:directories? #t)))))))
    (native-inputs
     (list python-asdf
           python-cython
           python-extension-helpers
           python-gwcs
           python-pytest-astropy
           python-pytest-xdist
           python-pyvo
           python-setuptools-scm
           python-shapely
           python-sunpy-minimal))
    (propagated-inputs
     (list python-astropy
           python-astropy-healpix
           python-dask
           python-dask-image
           python-fsspec
           python-numpy
           python-pillow
           python-pyavm
           python-scipy
           python-zarr))
    (home-page "https://reproject.readthedocs.io")
    (synopsis "Astronomical image reprojection in Python")
    (description
     "This package provides a functionality to reproject astronomical images
using various techniques via a uniform interface, where reprojection is the
re-gridding of images from one world coordinate system to another e.g.
changing the pixel resolution, orientation, coordinate system.")
    (license license:bsd-3)))

(define-public python-roman-datamodels
  (package
    (name "python-roman-datamodels")
    (version "0.28.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "roman_datamodels" version))
       (sha256
        (base32 "0j5y6d4zz1zyfwp05iwlvn0708hsh7phvp2j3zpi9pczljp78aw0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 4618 passed, 1 skipped, 11 xfailed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-env
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pandas
           python-pytest
           python-pytest-doctestplus
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy
           python-asdf-standard
           python-astropy
           python-gwcs
           python-lz4
           python-numpy
           python-pyarrow
           python-rad))
    (home-page "https://github.com/spacetelescope/roman_datamodels")
    (synopsis "Roman Datamodels Support")
    (description
     "This package provides a Python package of Roman Datamodels for the
calibration pipelines started with the @acronym{JWST, James Webb Space
Telescope} calibration pipelines.  The goal for the JWST pipelines was motivated
primarily by the need to support FITS data files, specifically with isolating
the details of where metadata and data were located in the FITS file from the
representation of the same items within the Python code.  That is not a concern
for Roman since FITS format data files will not be used by the Roman calibration
pipelines.")
    (license license:bsd-3)))

(define-public python-romancal
  (package
    (name "python-romancal")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spacetelescope/romancal")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gjg7hws03rxa7mlapn4pxyvxckrhlqxj9r5q6qg03bx4d0w1gd3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 294 passed, 298 skipped, 2 deselected, 2 warnings
      #:test-flags
      #~(list "--color=no"
              ;; Tests requiring calibration data.
              #$@(map (lambda (file) (string-append "--ignore=" file))
                      (list "romancal/tweakreg/tests/test_tweakreg.py"
                            "romancal/assign_wcs/tests/test_wcs.py"
                            "romancal/dark_current/tests/test_dark.py"
                            "romancal/dq_init/tests/test_dq_init.py"
                            "romancal/flatfield/tests/test_flatfield.py"
                            "romancal/flux/tests/test_flux_step.py"
                            "romancal/linearity/tests/test_linearity.py"
                            "romancal/multiband_catalog/tests/test_multiband_catalog.py"
                            "romancal/photom/tests/test_photom.py"
                            "romancal/pipeline/tests/test_exposure_pipeline.py"
                            "romancal/ramp_fitting/tests/test_ramp_fit_cas22.py"
                            "romancal/ramp_fitting/tests/test_ramp_fit_likelihood.py"
                            "romancal/refpix/tests/test_step.py"
                            "romancal/resample/tests/test_resample.py"
                            "romancal/saturation/tests/test_saturation.py"
                            "romancal/skycell/tests/test_skycell.py"
                            "romancal/skycell/tests/test_skycell_match.py"
                            "romancal/skymatch/tests/test_skymatch.py"
                            "romancal/source_catalog/tests/test_psf.py"
                            "romancal/source_catalog/tests/test_source_catalog.py"
                            "romancal/stpipe/tests/test_core.py"))
              ;;  SystemExit: -1
              "-k" "not test_inject_sources and not test_grid_injection")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(version-major+minor+point version)))))))
    (native-inputs
     (list nss-certs-for-test
           python-ci-watson
           python-deepdiff
           python-pytest
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-stpreview))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy
           python-astropy
           python-crds
           python-drizzle
           python-gwcs
           python-jsonschema
           python-numpy
           python-pandas
           python-photutils
           python-pyarrow
           python-requests
           python-roman-datamodels
           python-romanisim
           python-scipy
           python-spherical-geometry
           python-stcal
           python-stpipe
           python-stsci-imagestats
           python-tweakwcs
           ;; [sdp]
           python-pysiaf
           ;; python-roman-photoz     ;not packaged
           python-stpreview))
    (home-page "https://github.com/spacetelescope/romancal")
    (synopsis "Nancy Grace Roman Space Telescope observations processing library")
    (description
     "This package implements a functionality for calibration of science
observations from the Nancy Grace Roman Space Telescope.")
    (license license:bsd-3)))

(define-public python-romanisim
  (package
    (name "python-romanisim")
    (version "0.11.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "romanisim" version))
       (sha256
        (base32 "10rrr4mkpxz9wrhxlzxhn5mrswnlz7fqln7zkfims22a95kl4ldz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 87 passed, 2 skipped, 8 deselected
      #:test-flags
      #~(list "--pyargs" "romanisim"
              ;; TODO: python-stpsf needs to be packaged with test data:
              ;; OSError: Couldn't read the version number from
              ;; /tmp/data/stpsf-data/version.txt. (Do you need to update the
              ;; STPSF data? See
              ;; https://stpsf.readthedocs.io/en/stable/installation.html#data-install
              ;; for a link to the latest version.)
              "--ignore-glob=*/tests/test_psf.py"
              "--ignore-glob=*/tests/test_image.py"
              ;; Network access is required.
              "-k" (string-join
                    (list "not test_exptime_array"
                          "test_inverse_then_linearity"
                          "test_make_dummy_catalog"
                          "test_make_gaia_stars"
                          "test_scaling"
                          "test_simulate_cps"
                          "test_simulate_vs_cps"
                          "test_wcs_crds_match")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          ;; See: <https://github.com/spacetelescope/romanisim/pull/279>.
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                ((".*Cython >=0.29.21.*") ""))))
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-cython
           python-ci-watson
           python-pytest
           python-pytest-doctestplus
           python-pytest-openfiles
           python-setuptools
           python-setuptools-scm
           python-stpsf))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-astropy-healpix
           python-astroquery
           python-crds
           python-defusedxml
           python-galsim
           python-gwcs
           python-numpy
           python-photutils
           python-roman-datamodels))
    (home-page "https://github.com/spacetelescope/romanisim")
    (synopsis "Nancy Grace Roman Space Telescope WFI Simulator")
    (description
     "@code{romanisim} is a Galsim-based simulator of imaging data from the
@acronym{WFI,Wide Field Instrument} on the @url{https://roman.gsfc.nasa.gov/,
Nancy Grace Roman Space Telescope} (pronounced roman-eye-sim, stylized Roman
I-Sim).  It uses Galsim to render astronomical scenes, WebbPSF to model the
point spread function, and CRDS to access the calibration information needed
to produce realistic WFI images.")
    ;; LICENSE.rst Association of Universities for Research in Astronomy (AURA)
    (license license:bsd-3)))

(define-public python-sbpy
  (package
    (name "python-sbpy")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sbpy" version))
       (sha256
        (base32 "1dvc9ihzziyw69a0h1f3wr7jzzp4v8zm9x71fasw4sx2i0vab21v"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 591 passed, 108 skipped, 309 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pytest-doctestplus
           python-pytest-remotedata
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy
           ;; [optional]
           python-ads
           python-astroquery
           python-ginga
           python-photutils
           ;; python-pyoorb     ;not packed yet in Guix
           python-pyyaml
           python-scipy
           python-synphot))
    (home-page "https://sbpy.org")
    (synopsis "Python module for small-body planetary astronomy")
    (description
     "@code{sbpy} is a package for small-body planetary astronomy.  It is
meant to supplement functionality provided by @code{astropy} with functions
and methods that are frequently used in the context of planetary astronomy
with a clear focus on asteroids and comets.
Features:
@itemize
@item observation planning tools tailored to moving objects
@item photometry models for resolved and unresolved observations
@item wrappers and tools for astrometry and orbit fitting
@item spectroscopy analysis tools and models for reflected solar light and
emission from gas
@item cometary gas and dust coma simulation and analysis tools
@item asteroid thermal models for flux estimation and size/albedo estimation
@item image enhancement tools for comet comae and PSF subtraction tools
@item lightcurve and shape analysis tools
@item access tools for various databases for orbital and physical data, as
well as ephemerides services
@end itemize")
    (license license:bsd-3)))

(define-public python-sep
  (package/inherit libsep
    (name "python-sep")
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "test.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(package-version this-package)))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy))
    (synopsis "Python library for Source Extraction and Photometry")))

(define-public python-sgp4
  (package
    (name "python-sgp4")
    (version "2.25")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sgp4" version))
       (sha256
        (base32 "0x06mxdmk2rsbp7ymjvvbj8pwkf4y2w6g8p0znw9zmi5rinxr7p1"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-backend #~'custom
           #:test-flags #~(list "-m" "sgp4.tests")))
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/brandon-rhodes/python-sgp4")
    (synopsis "Track earth satellite TLE orbits using SGP4")
    (description
     "This package provides a Python implementation for computations of the
position and velocity of an earth-orbiting satellite, given the satellite’s
@acronym{TLE, Two-line element set} orbital elements from a source like
@url{CelesTrak, https://celestrak.org/}.

It implements the most recent version of @acronym{SGP4, Simplified General
Perturbation models}, and is regularly run against the SGP4 test suite to make
sure that its satellite position predictions agree to within 0.1 mm with the
predictions of the standard distribution of the algorithm.  This error is far
less than the 1–3 km/day by which satellites themselves deviate from the ideal
orbits described in TLE files.")
    (license license:expat)))

(define-public python-sirilic
  (package
    (name "python-sirilic")
    (version "1.15.12")
    (source
     (origin
       (method git-fetch) ; not published on PyPI
       (uri (git-reference
             (url "https://gitlab.com/free-astro/sirilic")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08j7gkyc5jd3kavffdxcr9b1ckmsswsvm61f301kvdqk2xcsh0gb"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests
    (native-inputs
     (list nss-certs-for-test
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-requests
           python-wxpython))
    (home-page "https://siril.org/tutorials/pysiril/")
    (synopsis "Acquisition files preparation software to process with SiriL")
    (description
     "SiriLic (SiriL's Interactif Companion) is a software for preparing
acquisition files (raw, Biases, Flat and Dark) for processing with SiriL
software.

Features:
@itemize
@item structuring the SiriL working directory into sub-folders
@item convert Raw, Biases , Dark or Flat files into SiriL sequence
@item automatically generate the SiriL script according to the files present
and the options
@item batch process multiple channel and sessions
@end itemize")
    (license license:gpl3)))

(define-public python-skyfield
  (package
    (name "python-skyfield")
    (version "1.53")
    (source
     (origin
       (method git-fetch) ; PyPI tarball lacks test data
       (uri (git-reference
             (url "https://github.com/skyfielders/python-skyfield")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jj0bjvzlfxr4qaq6mnybhwabhz9n70afi8sd6a26wl79s5bw1q9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-m" "assay" "--batch" "skyfield.tests")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion "ci"
                  (apply invoke "python" test-flags))))))))
    (native-inputs
     (list nss-certs-for-test
           python-assay
           python-pandas
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-certifi
           python-jplephem
           python-numpy
           python-sgp4))
    (home-page "https://rhodesmill.org/skyfield/")
    (synopsis "Astronomy for Python")
    (description
     "Skyfield computes positions for the stars, planets, and satellites in
orbit around the Earth.")
    (license license:expat)))

(define-public python-slitronomy
  (package
    (name "python-slitronomy")
    (version "0.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "slitronomy" version))
       (sha256
        (base32 "0hpffl0icf63ski1f0j06a0wnaawh5gy24w5a3h4rzgkp8rrmhck"))))
    (build-system pyproject-build-system)
    (arguments
     ;; TODO: Tests depend on lenstronomy, not packaged yet.
     (list #:tests? #f))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-configparser
           python-numpy))
    (home-page "https://github.com/aymgal/SLITronomy")
    (synopsis "Sparse Linear Inversion Technique for lenstronomy")
    (description
     "This package provides an Updated and improved version of the
@acronym{Sparse Lens Inversion Technique, SLIT}, developed within the
framework of lens modelling software lenstronomy.")
    (license license:expat)))

(define-public python-sncosmo
  (package
    (name "python-sncosmo")
    (version "2.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sncosmo" version))
       (sha256
        (base32 "1jvrrrlb37pgsa0zd6519r07hih41nswa7ym3sh49i6bx0pmh4n5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 83 passed, 4 skipped, 19 deselected
      #:test-flags
      ;; Network access is required.
      #~(list "--ignore=tests/test_download_builtins.py"
              "--ignore=tests/test_builtins.py"
              "-k" (string-join
                    (list "not test_C11"
                          "test_G10"
                          "test_bandflux"
                          "test_bandflux_multi"
                          "test_bandflux_zpsys"
                          "test_bandfluxcov"
                          "test_bandmag"
                          "test_compositemagsystem_band_error"
                          "test_csp_magsystem"
                          "test_fit_lc_vs_snfit"
                          "test_megacampsf_bandpass"
                          "test_salt2source_rcov_vs_snfit"
                          "test_salt2source_timeseries_vs_snfit"
                          "test_sugarsource"
                          "test_ztf_bandpass")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp")
              (copy-recursively "sncosmo/tests" "tests")
              (delete-file-recursively "sncosmo" ))))))
    (native-inputs
     (list python-iminuit
           python-cython
           python-pytest
           python-pytest-xdist
           python-pytest-astropy
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-extinction
           python-h5py
           python-looseversion
           python-numpy
           python-pyyaml
           python-scipy))
    (home-page "https://sncosmo.readthedocs.org")
    (synopsis "Package for supernova cosmology based on astropy")
    (description
     "SNCosmo is a Python library for supernova cosmology analysis.  It aims
to make such analysis both as flexible and clear as possible.")
    (license license:bsd-3)))

(define-public python-sndata
  (package
    (name "python-sndata")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch) ; no tests data in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/sncosmo/SNData")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jci6ry2b8jylda4v7zhl857pifslpnslrbj2207nz4hw43nz3mp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; Network access is required for these tests.
                    (list "not test_duplicate_obj_id_strings"
                          "test_obj_id_as_str"
                          "test_coordinates_0_0"
                          "test_bad_table_id_err"
                          "test_cache_not_mutated"
                          "test_comments_not_in_metadata"
                          "test_get_zp"
                          "test_id_joining"
                          "test_ids_are_sorted"
                          "test_jd_time_format"
                          "test_join_id_string_error"
                          "test_minimal_metadata_keys"
                          "test_no_duplicate_aliases"
                          "test_no_empty_data_tables"
                          "test_no_empty_ids"
                          "test_paper_tables_are_parsed"
                          "test_sncosmo_registered_band_names"
                          "test_standard_column_names"
                          "test_unique_ids")
                    " and not "))))
    (native-inputs
     (list nss-certs-for-test
           python-cython
           python-poetry-core
           python-pytest))
    (propagated-inputs
     (list python-astropy
           python-beautifulsoup4
           python-numpy
           python-pandas
           python-pytz
           python-pyyaml
           python-requests
           python-sncosmo
           python-tqdm))
    (home-page "https://sndata.readthedocs.io/en/latest/")
    (synopsis "Interface for data published by various supernova surveys")
    (description
     "SNData provides an access to data releases published by a variety of
supernova (SN) surveys.  It is designed to support the development of scalable
analysis pipelines that translate with minimal effort between and across data
sets.  A summary of accessible data is provided below.  Access to additional
surveys is added upon request or as needed for individual research projects
undertaken by the developers.")
    (license license:gpl3+)))

(define-public python-so-noise-models
  (let ((commit "fac881eb5ee012673d8994443caa3c6ad7fac2b6")
        (revision "0"))
    (package
      (name "python-so-noise-models")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch) ; no release on PyPI
         (uri (git-reference
               (url "https://github.com/simonsobs/so_noise_models")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02ridxr3a8dx2nwsv386fxin4qs5f7h6q5sk28n9jprgnmgzzsfa"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:tests? #f)) ; no tests
      (native-inputs
       (list python-setuptools
             python-wheel))
      (propagated-inputs
       (list python-matplotlib
             python-numpy))
      (home-page "https://github.com/simonsobs/so_noise_models")
      (synopsis "Simons Observatory N(ell) noise models")
      (description
       "This package provides N(ell) noise curve projection code for the
Simons Observatory.  The intention is that the full history of noise models
will be provided to supplement published projections and simulations.")
      (license license:bsd-2))))

(define-public python-soxs
  (package
    (name "python-soxs")
    (version "5.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "soxs" version))
       (sha256
        (base32 "1q485ygalnnmx2imzm5qlfvblki63bhgmwzxv8gqfqraxk0midv5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests require additional data, check if it may be packed
      ;; separately, see tests/ci_install.sh.
      ;; - http://hea-www.cfa.harvard.edu/~jzuhone
      ;;   - soxs_test_data_16.tar.gz 235MiB
      #:tests? #f))
    (native-inputs
     (list python-cython
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-appdirs
           python-astropy
           python-h5py
           python-numpy
           python-pooch
           python-pyyaml
           python-regions
           python-scipy
           python-tqdm))
    (home-page "https://hea-www.cfa.harvard.edu/soxs/")
    (synopsis "Simulated Observations of X-ray Sources")
    (description
     "SOXS is a software suite which can create simulated X-ray observations
of astrophysical sources with almost any existing or planned X-ray
observatory.  The goal of SOXS is to provide a comprehensive set of tools to
design source models and convolve them with simulated models of X-ray
instruments.  This package was originally developed to support the
@url{https://www.lynxobservatory.org/,Lynx X-ray Observatory} mission concept,
but has evolved to support other missions as well.")
    ;; SOXS is licensed under the terms of the Modified BSD License (also
    ;; known as New or Revised BSD).
    (license license:bsd-3)))

(define-public python-space-dolphin
  (package
    (name "python-space-dolphin")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "space_dolphin" version))
       (sha256
        (base32 "162899av6mp0wkjbas07xkqjr70qbvirgnnch7hb501gz0rb50bh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f ;TODO: Enable when tensorflow is fixed.
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;;  RuntimeError: cannot cache function 'rotate': no locator
              ;;  available for file '<...>/lenstronomy/Util/util.py'.
              (setenv "NUMBA_CACHE_DIR" "/tmp"))))))
    (native-inputs
     (list ;; python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-corner
           python-emcee
           python-gdown
           python-h5py
           python-lenstronomy
           python-matplotlib
           python-numpy
           python-pyyaml
           python-schwimmbad
           python-scipy
           python-tqdm
           #;tensorflow)) ;XXX: currenlty broken on master, see guix/guix#1436.
    (home-page "https://github.com/ajshajib/dolphin")
    (synopsis "Automated pipeline for lens modeling based on lenstronomy")
    (description
     "This package implements a functionality of AI-powered automated pipeline
for lens modeling, with lenstronomy as the modeling engine.

Features:
@itemize
@item AI-automated forward modeling for large samples of galaxy-scale lenses
@item flexible: supports both fully automated and semi-automated (with user
tweaks) modes
@item multi-band lens modeling made simple
@item supports both galaxy–galaxy and galaxy–quasar systems
@item effortless syncing between local machines and @acronym{High-Performance
Computing Cluster, HPCC}
@end itemize")
    (license license:bsd-3)))

(define-public python-spacetrack
  (package
    (name "python-spacetrack")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spacetrack" version))
       (sha256
        (base32 "0x2c8gag0h8435a5xzqfpm7sw98s908r40y4xjh9c16yvv1m8jzy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 75 passed, 1 deselected, 19 warnings
      #:test-flags
      #~(list "--asyncio-mode=auto"
              ;; One test fails with assertion not equal.
              "--deselect=tests/test_aio.py::test_modeldef_not_used_trio")))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-pytest-asyncio
           python-respx
           python-setuptools))
    (propagated-inputs
     (list python-filelock-next
           python-httpx
           python-logbook
           python-outcome
           python-platformdirs
           python-dateutil
           python-represent
           python-rush
           python-sniffio))
    (home-page "https://github.com/python-astrodynamics/spacetrack")
    (synopsis "Python client for space-track.org")
    (description
     "@code{spacetrack} is a python module for @url{https://www.space-track.org/,
Space-Track} which promotes space flight safety, protection of the space
environment and the peaceful use of space worldwide by sharing space
situational awareness services and information with international satellite
owners/operators, academia and other entities.")
    (license license:expat)))

(define-public python-specreduce
  (package
    (name "python-specreduce")
    (version "1.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/astropy/specreduce")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ldl8g182jdjbb8inah2h7f5i8n2frh4sllqd9l6zp09ylq4s9rs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 104 passed, 24 skipped, 1 deselected
      #:test-flags
      #~(list "--pyargs" "specreduce"
              ;; See: <https://github.com/astropy/specreduce/issues/292>
              "-k" "not test_init_line_list")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)))
          (add-before 'check 'set-home
            (lambda _
              ;; Relax matplotlib warning: ... because the default path
              ;; (/homeless-shelter/.config/matplotlib) is not a writable
              ;; directory ...
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-photutils
           python-pypeit-minimal
           python-pytest
           python-pytest-astropy
           python-pytest-mock
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-gwcs
           python-matplotlib
           python-numpy
           python-scipy
           python-specutils-1.9))
    (home-page "https://specreduce.readthedocs.io/")
    (synopsis "Spectroscopic Reductions")
    (description
     "This package implements functionality of spectroscopic reduction in
observations from Optical and @acronym{Near-infrared spectroscopy,NIR}
instruments.")
    (license (list license:bsd-3     ; licenses/LICENSE.rst, same as python-astropy
                   license:expat)))) ; licenses/KOSMOS_LICENSE

(define-public python-spectral-cube
  (package
    (name "python-spectral-cube")
    (version "0.6.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spectral_cube" version))
       (sha256
        (base32 "1r3mwrnjw2yfjsd3bskl013c3yk0ad3ky1yhxlvyiz4phrahhb3f"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 1520 passed, 185 skipped, 17 xfailed, 6048 warnings
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; AttributeError: module 'numpy' has no attribute 'product'
              #$@(map (lambda (test) (string-append "--deselect="
                                                    "spectral_cube/tests/"
                                                    "test_casafuncs.py::"
                                                    "test"))
                      (list "test_casa_read_basic[False-False]"
                            "test_casa_read_basic[False-True]"
                            "test_casa_read_basic[True-False]"
                            "test_casa_read_basic[True-True]"
                            "test_casa_read_basic_nomask")))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pytest-xdist
           python-regions
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-casa-formats-io
           python-dask
           python-joblib
           python-numpy
           python-packaging
           python-radio-beam
           python-tqdm
           ;; [optional]
           python-scipy
           python-zarr))
    (home-page "https://spectral-cube.readthedocs.io/en/latest/")
    (synopsis "Library for reading and analyzing astrophysical spectral data cubes")
    (description
     "The spectral-cube package provides an easy way to read, manipulate,
analyze, and write data cubes with two positional dimensions and one spectral
dimension, optionally with Stokes parameters.

It provides the following main features:
@itemize
@item A uniform interface to spectral cubes, robust to the wide range of conventions
of axis order, spatial projections, and spectral units that exist in the wild.
@item Easy extraction of cube sub-regions using physical coordinates.
@item Ability to easily create, combine, and apply masks to datasets.
@item Basic summary statistic methods like moments and array aggregates.
@item Designed to work with datasets too large to load into memory.
@end itemize")
    (license license:bsd-3)))

(define-public python-specutils
  (package
    (name "python-specutils")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "specutils" version))
       (sha256
        (base32 "1zcyv8aqmj86nxk2qrvci4wvdylmv5ql16vv1wjq8s6akdsxpmzb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; 572 passed, 131 skipped, 3 xfailed
      #:test-flags
      ;; Disabling test requiring access to download
      ;; <https://datacenter.iers.org/data/9/finals2000A.all>.
      ;; XXX: Check if test data may be packed as standalone package.
      #~(list "-k" "not test_create_spectral_axis")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-matplotlib
           python-pytest-asdf-plugin
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm
           python-spectral-cube))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy
           python-astropy
           python-gwcs
           python-ndcube-minimal
           python-numpy
           python-scipy))
    (home-page "https://specutils.readthedocs.io/")
    (synopsis "Package for spectroscopic astronomical data")
    (description
     "@code{specutils} is a Python package for representing, loading,
manipulating,and analyzing astronomical spectroscopic data.  The generic data
containers and accompanying modules provide a toolbox that the astronomical
community can use to build more domain-specific packages.  For more details
about the underlying principles, see
@url{https://github.com/astropy/astropy-APEs/blob/main/APE13.rst, APE13}.")
    (license license:bsd-3)))

;; For python-specreduce@1.7.0, remove when no longer required.
(define-public python-specutils-1.9
  (hidden-package
   (package
     (inherit python-specutils)
     (name "python-specutils")
     (version "1.9.1")
     (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "specutils" version))
        (sha256
         (base32 "0ar4h7pwm1zygcpfjz78878wd8bjgrzsbcnkpwqy26kvqzn928ih"))))
     (arguments
      (substitute-keyword-arguments
          (package-arguments python-specutils)
        ((#:tests? _ #t) #f))))))

(define-public python-spherical-geometry
  (package
    (name "python-spherical-geometry")
    (version "1.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/spacetelescope/spherical_geometry")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09f5j8xapvb88bcn48qc4438zb1k0c3lbkb8immfzkwwkvx0jpi7"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Remove bundled library.
            (delete-file-recursively "libqd")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "spherical_geometry")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preparations
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)
              ;; Use our own libraries in place of bundles.
              (setenv "USE_SYSTEM_QD" "1"))))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy-header
           python-setuptools
           python-setuptools-scm))
    (inputs
     (list qd))
    (propagated-inputs
     (list python-astropy
           python-numpy))
    (home-page "https://github.com/spacetelescope/spherical_geometry")
    (synopsis "Python astronimical package for handling spherical polygons")
    (description
     "The @code{spherical_geometry} library is a Python package for handling
spherical polygons that represent arbitrary regions of the sky.")
    ;; LICENSE.rst Association of Universities for Research in Astronomy (AURA)
    ;; QD_LIBRARY_LICENSE.rst for bandeled QD source
    (license license:bsd-3)))

(define-public python-spisea
  (package
    (name "python-spisea")
    (version "2.1.15")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/astropy/SPISEA")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18zzyply7mxf3ahlfy33i7hbwrz4abq676gjq66w7wbh7g3b6yy6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests require additional data, see
      ;; <https://spisea.readthedocs.io/en/latest/getting_started.html>.
      ;; - https://archive.stsci.edu/hlsps/reference-atlases/
      ;;   - hlsp_reference-atlases_hst_multi_everything_multi_v10_sed.tar 741MiB
      ;; - https://archive.stsci.edu/hlsps/reference-atlases/
      ;;   - hlsp_reference-atlases_hst_multi_star-galaxy-models_multi_v3_synphot2.tar 84MiB
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-setup.cfg
            (lambda _
              (substitute* "setup.cfg"
                ;; XXX: Report upstream, the template was not adjusted.
                (("astropy-package-template-example = .*") ""))))
          (add-after 'unpack 'set-version
            (lambda _
              (with-output-to-file "spisea/__init__.py"
                (lambda _
                  (display
                   (string-append "__version__ = \""
                                  #$(package-version this-package)
                                  "\""))))
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-pysynphot
           python-scipy))
    (home-page "https://spisea.readthedocs.io/en/stable/index.html")
    (synopsis "Stellar Population Synthesis Modeling")
    (description
     "SPISEA is an python package that generates single-age,
single-metallicity populations (i.e. star clusters). It gives the user control
over many parameters:

@itemize
@item cluster characteristics (age, metallicity, mass, distance)
@item total extinction, differential extinction, and extinction law
@item stellar evolution and atmosphere models
@item stellar multiplicity and Initial Mass Function
@item initial-Final Mass Relation
@item photometric filters
@end itemize")
    ;; Licensed under a 3-clause BSD style license - see LICENSE.rst
    ;; spisea/_astropy_init.py:
    (license (list license:gpl3+ license:bsd-3))))

(define-public python-statmorph
  (package
    (name "python-statmorph")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statmorph" version))
       (sha256
        (base32 "1c4srrmfzx8iszcc140ylljs0dzqs6d9ya2z7wic96p6y6gixb9h"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-photutils
           python-scikit-image
           python-scipy
           ;; [optional]
           python-matplotlib))
    (home-page "https://github.com/vrodgom/statmorph")
    (synopsis "Non-parametric morphological diagnostics of galaxy images")
    (description
     "The package @code{statmorph} implements functionality of calculating
non-parametric morphological diagnostics of galaxy images (e.g., Gini-M_{20}
and CAS statistics), as well as fitting 2D Sérsic profiles.")
    (license license:bsd-3)))

(define-public python-stcal
  (package
    (name "python-stcal")
    (version "1.15.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stcal" version))
       (sha256
        (base32 "0lzwp06399fwxi8m82cmj5009s2xyvmzmvxw64qfbm39mxkhbw51"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "stcal"
              "--numprocesses" (number->string (parallel-job-count))
              "-k" (string-join
                    ;; Tests requiring network access.
                    (list "not test_absolute_align"
                          "test_create_catalog"
                          "test_create_catalog_graceful_failure"
                          "test_get_catalog"
                          "test_parse_refcat"
                          "test_parse_sky_centroid"
                          "test_relative_align[False]"
                          "test_relative_align[True]")
                    " and not ")
              "tests")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'relax-requirements
            (lambda _
              (substitute* "pyproject.toml"
                ;; XXX: Can't detect opencv-python version. The input opencv
                ;; might not set the version correctly.
                ((".*opencv-python-headless.*") "")))))))
    (native-inputs
     (list python-cython
           python-psutil
           python-pytest
           python-pytest-xdist
           python-pytest-doctestplus
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list opencv        ;Provides OpenCV-Python
           python-asdf
           python-astropy
           python-drizzle
           python-gwcs
           python-numpy
           python-requests
           python-scikit-image
           python-scipy
           python-tweakwcs))
    (home-page "https://github.com/spacetelescope/stcal")
    (synopsis "STScI tools and algorithms used in calibration pipelines")
    (description "STScI tools and algorithms used in calibration pipelines.")
    (license license:bsd-3)))

(define-public python-stdatamodels
  (package
    (name "python-stdatamodels")
    (version "4.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stdatamodels" version))
       (sha256
        (base32 "1gwa7gh2hyv3770sv514w51c5s7c8zwjr3scx2b240z4c9pbgmq0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; 1571 passed
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; Disable tests requiring access to CRDS servers to download
              ;; ~500MiB of data.
              "-k" "not test_crds_selectors_vs_datamodel and not test_report")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-crds-minimal
           python-psutil
           python-pytest
           python-pytest-asdf-plugin
           python-pytest-doctestplus
           python-pytest-xdist
           python-scipy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf-4
           python-asdf-transform-schemas
           python-asdf-astropy
           python-astropy
           python-numpy))
    (home-page "https://github.com/spacetelescope/stdatamodels")
    (synopsis "STScI DataModel classes used in calibration pipelines")
    (description
     "Provides @code{DataModel}, which is the base class for data models
implemented in the @acronym{JWST, James Webb Space Telescope} and
@acronym{Roman, Nancy Grace Roman Space Telescope} calibration software.")
    (license license:bsd-3)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-stdatamodels-minimal
  (package/inherit python-stdatamodels
    (name "python-stdatamodels-minimal")
    (arguments
     (list #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))))

(define-public python-stpipe
  (package
    (name "python-stpipe")
    (version "0.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stpipe" version))
       (sha256
        (base32 "1and1hjlwbmqb2nkw04gmp7q0nrnc0s22vgkq6bfs78c77z7cswf"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Replace reference to external configobj.
           (substitute* (find-files "." "\\.py$")
             (("from astropy.extern import configobj") "import configobj")
             (("from astropy.extern.configobj import validate") "import validate")
             (("from astropy.extern.configobj.configobj import ") "from configobj import ")
             (("from astropy.extern.configobj.validate import ") "from validate import "))))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-doctestplus
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-crds
           python-importlib-metadata
           python-pyyaml))
    (home-page "https://github.com/spacetelescope/stpipe")
    (synopsis "STScI framework for calibration pipeline software")
    (description
     "This package provides base classes and command-line tools for
implementing calibration pipeline software.")
    ;; LICENSE Association of Universities for Research in Astronomy (AURA)
    (license license:bsd-3)))

(define-public python-stpreview
  (package
    (name "python-stpreview")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stpreview" version))
       (sha256
        (base32 "1z7zpqyg7rl2sf311idzwckdshm86mkbbpknvq90gkxmqdaqas9q"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-roman-datamodels
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-gwcs
           python-matplotlib
           python-numpy
           python-scikit-image))
    (home-page "https://github.com/spacetelescope/stpreview")
    (synopsis "Build downsampled previews of STScI")
    (description
     "This package provides build downsampled previews of Space Telescope products.")
    (license license:bsd-3)))

(define-public python-stpsf
  (package
    (name "python-stpsf")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stpsf" version))
       (sha256
        (base32 "191bwwv0ji7m74cglk5jvfgx6c4yfxhx6fd0w5n9k363cxqqp0x6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       ;; XXX: Tests and installation require 100MiB archive, upstream does
       ;; not provide any version to pin particular data file per release,
       ;; check if they can apply any tag.
       ;;
       ;; See: <https://stpsf.readthedocs.io/en/stable/installation.html>,
       ;; <https://github.com/spacetelescope/stpsf/issues/76>.
      #:tests? #f))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-astroquery
           python-matplotlib
           python-numpy
           python-photutils
           python-poppy
           python-pysiaf
           python-scipy
           python-synphot))
    (home-page "https://stpsf.readthedocs.io")
    (synopsis "Creates simulated point spread functions for Space Telescopes)")
    (description
     "STPSF produces simulated PSFs for the James Webb Space Telescope, NASA's
flagship infrared space telescope.  STPSF can simulate images for any of the four
science instruments plus the fine guidance sensor, including both direct
imaging, coronagraphic, and spectroscopic modes.")
    (license license:bsd-3)))

(define-public python-stregion
  (package
    (name "python-stregion")
    (version "1.1.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stregion" version))
       (sha256
        (base32 "0clcv76x8pj61dvyh9pa6gp3kskc9g11i1fi559ax2qa2pgqhh1n"))))
    (build-system pyproject-build-system)
    (arguments
     ;; AttributeError: module 'astropy.io.fits' has no attribute 'CardList'
     ;; AttributeError: 'WCS' object has no attribute 'wcs_pix2sky'
     (list #:tests? #f)) ;depends on deprecated Astropy functions
    (native-inputs
     (list python-cython
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy
           python-pyparsing))
    (home-page "https://github.com/spacetelescope/stregion")
    (synopsis "Python parser for DS9 region files")
    (description
     "This package implements a functionality to parse
@url{https://github.com/SAOImageDS9/SAOImageDS9, DS9} region files.")
    (license license:expat)))

(define-public python-stsci-image
  (package
    (name "python-stsci-image")
    (version "2.3.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsci_image" version))
       (sha256
        (base32 "0wvqvpq9hfmpjf0w6kbgk6rc3jmshdnxk8s8q61rkfwjl11vi90b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-scipy))
    (home-page "https://github.com/spacetelescope/stsci.image")
    (synopsis "Image array manipulation functions")
    (description
     "This package provides a Python module to various @acronym{STScI, Space
Telescope Science Institute} image array manipulation functions.")
    (license license:bsd-3)))

(define-public python-stsci-imagestats
  (package
    (name "python-stsci-imagestats")
    (version "1.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsci_imagestats" version))
       (sha256
        (base32 "0gn0qqpv2qwl4c9sf8783xkwawk1xjhv3dk28gx82fp0bqalz4kg"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "stsci.imagestats")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(version-major+minor+point version)))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://stsciimagestats.readthedocs.io/en/latest/")
    (synopsis "Compute sigma-clipped statistics on data arrays")
    (description
     "@code{stsci.imagestats} is a package designed to compute various
statistics on image data using sigma-clipping iterations.  It is designed to
replicate core behaviour of the IRAF's
@url{http://stsdas.stsci.edu/cgi-bin/gethelp.cgi?imstatistics, imstatistics
task}.")
    (license license:bsd-3)))

(define-public python-stsci-skypac
  (package
    (name "python-stsci-skypac")
    (version "1.0.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsci_skypac" version))
       (sha256
        (base32 "1pfgcgcgwvlil4m0v7d6raya70s74knimfh8yn634kapf24xxy1k"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; no tests in PyPI or git
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-spherical-geometry
           python-stsci-imagestats
           python-stsci-tools
           python-stwcs))
    (home-page "https://stsci-skypac.readthedocs.io/en/latest")
    (synopsis "Sky matching on image mosaic")
    (description
     "This package implements a functionality to match sky on image mosaic.")
    (license license:bsd-3)))

(define-public python-stsci-stimage
  (package
    (name "python-stsci-stimage")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsci_stimage" version))
       (sha256
        (base32 "10vhc2kfryis37k5jkg357z7lhlmyci4makzy50xgh08648ak7cd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://stscistimage.readthedocs.io/en/latest/")
    (synopsis "STScI image processing")
    (description
     "This package provides an astronomical Python package with image
processing functions: @code{xyxymatch}, @code{geomap}.")
    (license license:bsd-3)))

(define-public python-stsci-tools
  (package
    (name "python-stsci-tools")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsci_tools" version))
       (sha256
        (base32 "180x3fmp020p4imc39ajs5qs6iimd8ld5bly3g9mm4psqbp8nyw9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k"
              ;; Two tests fail with error: RuntimeError: Problem during:
              ;; takes_time(), exitcode: 1. Check log.
              (string-append "not test_launch_and_wait[None-spawn]"
                             " and not test_launch_and_wait[None-forkserver]"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (delete-file "conftest.py"))))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy-header
           python-pytest-doctestplus
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-packaging))
    (home-page "https://stscitools.readthedocs.io/en/latest")
    (synopsis "Collection of STScI utility functions")
    (description
     "This package provides a collection of @acronym{Space Telescope Science
Institute, STScI} utility functions.")
    (license license:bsd-3)))

(define-public python-stsynphot
  (package
    (name "python-stsynphot")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsynphot" version))
       (sha256
        (base32 "1hkpk27wrkhfxngzq1qdpza0i95lcs9bvxsyyf1bn45nw85ljajy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests fails on missing file, it might need to be downloaded,
      ;; disable them for now.  astropy.utils.exceptions.AstropyUserWarning:
      ;; Failed to load Vega spectrum from
      ;; /grp/redcat/trds/calspec/alpha_lyr_stis_010.fits;
      ;;
      ;; Data is availalbe by the lnks: <https://ssb.stsci.edu/trds/calspec/>,
      ;; <https://archive.stsci.edu/hlsp/reference-atlases>.
      #:tests? #f))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pytest-astropy-header
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-beautifulsoup4
           python-matplotlib
           python-numpy
           python-scipy
           python-synphot))
    (home-page "https://github.com/spacetelescope/stsynphot_refactor")
    (synopsis "Synthetic photometry using Astropy for HST and JWST")
    (description
     "This package provides a replacement for IRAF STSDAS SYNPHOT and ASTROLIB
PYSYNPHOT, utilizing Astropy covering instrument specific portions of the old
packages for HST.")
    (license license:bsd-3)))

(define-public python-stwcs
  (package
    (name "python-stwcs")
    (version "1.7.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stwcs" version))
       (sha256
        (base32 "04s5wlw1yvgyrz9ax0akfvikkpfyvfy598rap37jqjy35lgpqmlb"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 18 passed, 34 deselected, 1 warning
      #:test-flags
      #~(list "-k" (string-join
                    ;; XXX: Test requiring network access to download data
                    ;; from <mast.stsci.edu>, check if the test data may be
                    ;; obtained programmatically.
                    (list "not test1SciExt"
                          "testAllExt"
                          "testHletFromSimpleFITS"
                          "testHletFromString"
                          "testSciExtList"
                          "testSciExtNumList"
                          "testWrongD2IMModel"
                          "testWrongNPOLModel"
                          "testWrongSIPModel"
                          "test_add_radesys"
                          "test_apply_as_alternate_method"
                          "test_apply_as_primary_method"
                          "test_apply_d2im"
                          "test_db_connection"
                          "test_default"
                          "test_new_obs"
                          "test_no_HDRNAME_no_WCSNAME"
                          "test_outwcs"
                          "test_pipeline_sci1"
                          "test_pipeline_sci2"
                          "test_remove_d2im_distortion"
                          "test_remove_npol_distortion"
                          "test_remove_npol_distortion_hdulist"
                          "test_repeate"
                          "test_repeated_updatewcs_use_db"
                          "test_restore_headerlet"
                          "test_simple_sci1"
                          "test_simple_sci2"
                          "test_update_d2im_distortion"
                          "test_update_legacy_file"
                          "test_update_stis_asn"
                          "test_update_waiver_wfpc2")
                    " and not "))))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-lxml
           python-numpy
           python-requests
           python-stsci-tools))
    (home-page "https://stwcs.readthedocs.io/en/latest/")
    (synopsis "WCS based distortion models and coordinate transformation")
    (description
     "STWCS provides support for WCS distortion models and coordinate
transformation for the imaging instruments on the @acronym{Hubble Space
Telescope, HST}).")
    (license license:bsd-3)))

(define-public python-sunkit-image
  (package
    (name "python-sunkit-image")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunkit_image" version))
       (sha256
        (base32 "0x8w87vwdr78vcq0zha2y7xyfy16amc5aym4v8127xvphq1ff4v3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              ;; One test fails with assertion, probably in NumPy array
              ;; precision calculation: np.allclose and
              ;; sunpy.map.mapbase.GenericMap are not matched.
              "--deselect=sunkit_image/tests/test_radial.py::test_fnrgf")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              ;; For tests: Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-dask
           python-pytest-astropy
           python-pytest-mpl
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scikit-image
           python-scipy
           python-sunpy))
    (home-page "https://github.com/sunpy/sunkit-image/")
    (synopsis "Solar Physics image processing toolbox")
    (description
     "This package provides an image processing toolbox for Solar Physics.")
    (license license:bsd-2)))

(define-public python-sunkit-magex
  (package
    (name "python-sunkit-magex")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunkit_magex" version))
       (sha256
        (base32 "16wraf2yrqslik4h3mhj5gcwx7fdg3x9v38swlhcgcyvkcjxbqc2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            ;; FileNotFoundError: [Errno 2] No such file or directory:
            ;; '/homeless-shelter/.config'
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-pytest-arraydiff
           python-pytest-doctestplus
           python-pytest-xdist
           python-reproject
           python-setuptools
           python-setuptools-scm
           python-streamtracer
           python-sympy))
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-scikit-image
           python-scipy
           python-sunpy))
    (home-page "https://docs.sunpy.org/projects/sunkit-magex")
    (synopsis "Solar Magnetic field Extrapolation")
    (description
     "This package provides a set of tools for the modelling of magnetic field
data.  It is a SunPy affiliated package and is built on top of @code{sunpy}
and @code{astropy}.")
    ;; This project is Copyright (c) The SunPy Community and licensed under
    ;; the terms of the GNU GPL v3+ license.  This package is based upon the
    ;; Openastronomy packaging guide which is licensed under the BSD 3-clause
    ;; license. See the licenses folder for more information.
    (license (list license:gpl3+ license:bsd-3))))

(define-public python-sunkit-spex
  (package
    (name "python-sunkit-spex")
    (properties '((commit . "bb544c44573d6de44e3d5ba08d06cab0059a4db7")
                  (revision . "0")))
    (version (git-version "0.4.0"
                          (assoc-ref properties 'revision)
                          (assoc-ref properties 'commit)))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/sunpy/sunkit-spex")
              (commit (assoc-ref properties 'commit))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n6if5zpbblp5bjy93bzh5bwrmbjfn2lkc850j4d9n5cw4jia3gy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; TODO: tests require some remove data, findout how to run bare minmal
      ;; unit tests withou it.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                      #$(version-major+minor+point version)))))))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-corner
           python-emcee
           python-gwcs
           python-matplotlib
           python-ndcube
           python-nestle
           python-numdifftools
           python-numpy
           python-parfive
           python-scipy
           python-sunpy
           python-xarray))
    (home-page "https://github.com/sunpy/sunkit-spex")
    (synopsis "Solar X-ray spectroscopy tool")
    (description
     "This package provides a tooling for solar X-ray spectroscopy based on
SunPy.")
    (license license:bsd-3)))

(define-public python-sunpy
  (package
    (name "python-sunpy")
    (version "7.0.4")   ; higher versions require NumPy 2+
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunpy" version))
       (sha256
        (base32 "0ylzxb52ii8s7i9imc0mpj3pgsa1lz4b8dmjk3kwnf8hrdzzd1ry"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 2465 passed, 1 skipped, 3 xfailed, 36 warnings
      #:test-flags
      #~(list "-m" "not remote_data"
              "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; Test introduces a time bomb and fails with error: ValueError:
              ;; interpolating from IERS_Auto using predictive values that are
              ;; more than 30.0 days old.
              "-k" "not test_print_params")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            ;; It requires during sanity check as well to prevent error like:
            ;; PermissionError: [Errno 13] Permission denied:
            ;; '/homeless-shelter'
            (lambda _
              (setenv "HOME" "/tmp")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion #$output
                  (apply invoke "pytest" "-vv" test-flags)))))
          (add-before 'check 'post-check
            (lambda _
              (for-each delete-file-recursively
                        (find-files #$output "__pycache__" #:directories? #t)))))))
    (native-inputs
     (list nss-certs-for-test
           python-aiohttp
           python-extension-helpers
           python-hvpy
           python-jplephem
           python-pytest-asdf-plugin
           python-pytest-astropy
           python-pytest-mock
           python-pytest-mpl
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-fsspec
           python-numpy
           python-packaging
           python-parfive
           python-pyerfa
           python-requests
           ;; [optional]
           opencv
           python-aiobotocore
           python-asdf
           python-asdf-astropy
           python-beautifulsoup4
           python-cdflib
           python-contourpy
           python-dask
           python-drms
           python-fsspec
           python-glymur
           python-h5netcdf
           python-h5py
           python-lxml
           python-matplotlib
           python-mpl-animators
           python-pandas
           python-beautifulsoup4
           python-reproject
           python-scikit-image
           python-scipy
           ;; python-spiceypy ; Not packed yet in Guix, long journey.
           python-tqdm
           python-zeep))
    (home-page "https://sunpy.org")
    (synopsis "Python library for Solar Physics")
    (description
     "SunPy is package for solar physics and is meant to be a free alternative
to the @url{https://sohowww.nascom.nasa.gov/solarsoft/, SolarSoft} data
analysis environment.

It includes an interface for searching and downloading data from multiple data
providers, data containers for image and time series data, commonly used solar
coordinate frames and associated transformations, as well as other
functionality needed for solar data analysis.")
    (license license:bsd-2)))

;; A bare minimal package, mainly to use in tests and reduce closure
;; size. Tests are left out in the main package to slim down native-inputs.
(define-public python-sunpy-minimal
  (package/inherit python-sunpy
    (name "python-sunpy-minimal")
    (arguments
     (substitute-keyword-arguments (package-arguments python-sunpy)
       ((#:tests? _ #t) #f)))
    (native-inputs
     (list python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-astropy-minimal
           python-fsspec
           python-mpl-animators
           python-parfive
           python-pyerfa
           python-requests))))

(define-public python-sunpy-soar
  (package
    (name "python-sunpy-soar")
    (version "1.12.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunpy_soar" version))
       (sha256
        (base32 "0ma4j8wy3hbshzafq1xvfifvinx9ahr4r6gvzyqahf2wp5y9rr8l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 10 passed, 1 skipped, 21 deselected, 1 xfailed
      #:test-flags
      ;; Disable tests requiring network access to download test data from
      ;; <http://soar.esac.esa.int> and <http://docs.virtualsolar.org>.
      #~(list "-k" (string-join
                    (list "not test_distance_out_of_bounds_warning"
                          "test_distance_search_insitu"
                          "test_distance_search_remote_sensing"
                          "test_distance_time_search"
                          "test_download_path"
                          "test_insitu_search"
                          "test_invalid_detector"
                          "test_no_instrument"
                          "test_no_results"
                          "test_search"
                          "test_search_detector_instrument_dimension_2"
                          "test_search_detector_instrument_dimension_4"
                          "test_search_low_latency"
                          "test_search_soop"
                          "test_search_wavelength_detector_column"
                          "test_wavelength_column_wavelength_exists"
                          "test_wavelength_range"
                          "test_wavelength_single"
                          "test_when_soar_provider_passed"
                          "test_when_wrong_provider_passed")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-matplotlib
           python-pytest
           python-pytest-doctestplus
           python-responses
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-requests
           python-sunpy))
    (home-page "https://docs.sunpy.org/projects/soar")
    (synopsis "Solar Orbiter Archive plugin for SunPy")
    (description
     "This package provides a @code{sunpy} FIDO plugin for accessing data in the
@acronym{Solar Orbiter Archive, SOAR}.")
    (license license:bsd-2)))

(define-public python-sunraster
  (package
    (name "python-sunraster")
    (version "0.7.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunraster" version))
       (sha256
        (base32 "11i4kssm9r2364s35jjy29y42qlcdxaf8h64l9ggrqgbg02anckz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "--ignore=docs/data_types/raster.rst"
              "--ignore=docs/data_types/spectrogram.rst")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              ;; PermissionError: [Errno 13] Permission denied:
              ;; '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list nss-certs-for-test
           python-pytest
           python-pytest-astropy
           python-pytest-doctestplus
           python-pytest-xdist
           python-setuptools
           python-setuptools-scm
           python-sunpy-minimal))
    (propagated-inputs
     (list python-astropy
           python-ndcube
           python-numpy))
    (home-page "https://docs.sunpy.org/projects/sunraster/en/stable/")
    (synopsis "Solar mission spectral data tool")
    (description
     "sunraster is an Python library that provides the tools to read in and
analyze spectrogram data.")
    (license license:bsd-2)))

(define-public python-suntime
  (package
    (name "python-suntime")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch) ; no tests data in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/SatAgro/suntime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "145jqwnl8frg49981xcli1l13h73m40gkbnpwa88lhva03j7y1k2"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "tests.py")))
    (native-inputs
     (list python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-dateutil))
    (home-page "https://github.com/SatAgro/suntime")
    (synopsis "Sunset and sunrise time calculation python library")
    (description
     "Python library doing sunrise and sunset time calculation.  Takes a
WGS84 (GPS) latitude/longitude as input as well as an UTC or local datetime
object.")
    (license license:lgpl3+)))

(define-public python-swiftgalaxy
  (package
    (name "python-swiftgalaxy")
    (version "1.2.1") ;the latest compatible with python-swiftsimio@9.0.1
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SWIFTSIM/swiftgalaxy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ywgjhv6h9cm052gjfhsiavy9s2czv1m9y8fnrfcidh72fks81d4"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;tests need to download remote datasets
    (native-inputs
     (list python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-h5py
           python-numpy
           python-scipy
           python-swiftsimio
           python-unyt))
    (home-page "https://github.com/SWIFTSIM/swiftgalaxy")
    (synopsis "Code abstraction of objects (galaxies) in simulations")
    (description
     "SWIFTGalaxy is an astrophysics module that extends
@url{http://swiftsimio.readthedocs.org/, SWIFTSimIO} tailored to analyses of
particles belonging to individual galaxies simulated with
@url{https://swift.strw.leidenuniv.nl/, SWIFT}.  It inherits from and extends
the functionality of the @code{SWIFTDataset}.  It understands the content of
halo catalogues (supported:
@url{https://ui.adsabs.harvard.edu/abs/2019PASA...36...21E/abstract,
Velociraptor}, @url{https://caesar.readthedocs.io/en/latest/, Caesar},
@url{https://github.com/SWIFTSIM/SOAP, SOAP}) and therefore which particles
belong to a galaxy or other group of particles, and its integrated properties.
The particles occupy a coordinate frame that is enforced to be consistent,
such that particles loaded on-the-fly will match e.g.  rotations and
translations of particles already in memory.  Intuitive masking of particle
datasets is also enabled.  Utilities to make working in cylindrical and
spherical coordinate systems more convenient are also provided.  Finally,
tools to iterate efficiently over multiple galaxies are also provided.")
    (license license:gpl3+)))

(define-public python-swiftsimio
  (package
    (name "python-swiftsimio")
    (version "9.0.1") ; higher versions require NumPy 2+
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/SWIFTSIM/swiftsimio")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16f4vfdjfhhm4mriwm3x32mn6f85bksaymzq1n5abn807zmgjyz2"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;tests need to download remote datasets
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-numba
           python-numpy
           python-unyt))
    (home-page "https://github.com/SWIFTSIM/swiftsimio")
    (synopsis "SWIFTsim I/O routines for Python")
    (description
     "@code{swiftsimio} is a toolkit for reading data produced by the
@url{https://swift.strw.leidenuniv.nl/,SWIFT} astrophysics simulation code.
It is used to ensure that all data have a symbolic unit attached, and can be
used for visualisation.  Another key feature is the use of the cell metadata
in @code{SWIFT} snapshots to enable efficient reading of sub-regions.")
    (license (list license:lgpl3+
                   license:gpl3+))))

(define-public python-synphot
  (package
    (name "python-synphot")
    (version "1.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "synphot" version))
       (sha256
        (base32 "12rmm1hr1f0bsgqsqsa7bz4lfijsv4fd978cvgw5xk66a3266z7b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 406 passed, 29 skipped, 4 xfailed
      #:test-flags
      #~(list "--pyargs" "synphot"
              ;; Not harmful deprecation warning as synphot is compatible with
              ;; specutils now, remove or silent error in the next refresh
              ;; cycle.
              ;;
              ;; See: <https://github.com/spacetelescope/synphot_refactor/issues/411>.
              "-W" "ignore::astropy.utils.exceptions.AstropyDeprecationWarning")))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-dust-extinction
           python-numpy
           python-scipy
           python-specutils))
    (home-page "https://github.com/spacetelescope/synphot_refactor")
    (synopsis "Synthetic photometry using Astropy")
    (description
     "This package provides a replacement for IRAF STSDAS SYNPHOT and ASTROLIB
PYSYNPHOT, utilizing Astropy and covering the non-instrument specific portions
of the old packages.")
    (license license:bsd-3)))

(define-public python-tweakwcs
  (package
    (name "python-tweakwcs")
    (version "0.8.12")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tweakwcs" version))
       (sha256
        (base32 "1p7kbx7ih4ch04gxzv3pm8zkrv45my0c43k42iar5k5jihjhqhx6"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-scipy
           python-setuptools
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy
           python-gwcs
           python-numpy
           python-packaging
           python-spherical-geometry
           python-stsci-imagestats
           python-stsci-stimage))
    (home-page "https://tweakwcs.readthedocs.io/en/latest/")
    (synopsis
     "Algorithms for matching and aligning catalogs and for tweaking the WCS")
    (description
     "@code{tweakwcs} is a package that provides core algorithms for computing
and applying corrections to @code{WCS} objects such as to minimize mismatch
between image and reference catalogs.  Currently only aligning images with
@code{FITS WCS} and @code{JWST gWCS} are supported.")
    (license license:bsd-3)))

(define-public python-utilities-awetzel
  (package
    (name "python-utilities-awetzel")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "utilities_awetzel" version))
       (sha256
        (base32 "049caadnxny330jday9hbg643b1fxpmkccjbxn1w9n6pmsx0ypql"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ;no tests
    (native-inputs
     (list python-setuptools))
    (propagated-inputs
     (list python-h5py
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://bitbucket.org/awetzel/utilities")
    (synopsis "General routines for analyzing astrophysical data sets")
    (description
     "Python package of utility functions that are useful in analyzing various
datasets, in particular, catalogs of particles or galaxies/halos from
cosmological simulations.")
    (license license:expat)))

(define-public python-viresclient
  (package
    (name "python-viresclient")
    (version "0.14.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "viresclient" version))
       (sha256
        (base32 "1li42f7rfhj07kbihlk0d5qwhf1wzsypyjskq0rxrwwvsxd8d8lp"))))
    (build-system pyproject-build-system)
    ;; tests: 9 passed
    (native-inputs
     (list python-flit-core
           python-pytest))
    (propagated-inputs
     (list python-cdflib
           python-h5py
           python-jinja2
           python-netcdf4
           python-pandas
           python-requests
           python-tables
           python-tqdm
           python-xarray))
    (home-page "https://viresclient.readthedocs.io/en/latest/")
    (synopsis "Python client for interacting with a VirES server")
    (description
     "This package provides a Python client for interacting with a
@code{VirES} server, of which there are two: VirES for
@url{https://vires.services, Swarm} and VirES for
@url{https://aeolus.services, Aeolus}")
    (license license:expat)))

;; See <https://github.com/spacetelescope/webbpsf/pull/951>
(define-deprecated-package python-webbpsf
  python-stpsf)

(define-public python-wiimatch
  (package
    (name "python-wiimatch")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "wiimatch" version))
              (sha256
               (base32
                "15kq7z30m9i286ncs9xvpaq3dq1p5fa47jz21prq146qwr7j6dm8"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-scipy))
    (native-inputs (list python-codecov python-pytest python-pytest-cov
                         python-pytest-doctestplus python-setuptools-scm
                         python-wheel))
    (home-page "https://github.com/spacetelescope/wiimatch")
    (synopsis
     "Optimal matching of weighted N-dimensional image intensity data")
    (description
     "@code{wiimatch} is a package that provides core computational algorithms
for optimal @code{matching} of weighted N-dimensional image intensity data
using (multivariate) polynomials.")
    (license license:bsd-3)))

(define-public python-yt
  (package
    (name "python-yt")
    (version "4.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yt" version))
       (sha256
        (base32 "02rvs1555y482vf54ivl5wjiziskybnmbxbq6xn46fslqa4km270"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:build-backend "setuptools.build_meta"
      #:test-flags
      #~(list "--numprocesses" (number->string (min 8 (parallel-job-count)))
              ;; Failed: 'yield' keyword is allowed in fixtures, but not in
              ;; tests (test_recipe)
              "--ignore=doc/source/cookbook/tests/test_cookbook.py"
              "-k" (string-join
                    ;; Tests requiring networking.
                    (list "not test_load_sample_timeout"
                          "test_unknown_filename"
                          "test_typo_filename"
                          "test_data_dir_broken"
                          "test_registry_byte_size_dtype"
                          "test_registry_byte_size_sign"
                          ;; AssertionError: Arrays are not equal
                          "test_field_cut_off_axis_octree"
                          ;; UserWarning: pkg_resources is deprecated as an
                          ;; API
                          "test_glue_data_object")
                    " and not "))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list nss-certs-for-test
           python-cython
           python-pyaml
           python-pytest
           python-pytest-mpl
           python-pytest-xdist
           python-setuptools
           python-sympy))
    (propagated-inputs
     (list python-cmyt
           python-ewah-bool-utils
           python-ipywidgets
           python-matplotlib
           python-more-itertools
           python-numpy
           python-packaging
           python-pillow
           python-tomli-w
           python-tqdm
           python-unyt
           ;; [optional]
           ;; python-arm-pyart ; not packed yet
           ;; python-f90nml
           ;; python-firefly
           ;; python-miniball
           ;; python-pyx
           ;; python-ratarmount
           python-astropy
           python-cartopy
           python-glue-core
           python-h5py
           python-netcdf4
           python-pandas
           python-pooch
           python-regions
           python-requests
           python-xarray))
    (home-page "http://yt-project.org/")
    (synopsis "Analyzing and visualizing volumetric data framework")
    (description
     "This package provides a structured, variable-resolution meshes,
 unstructured meshes, and discrete or sampled data such as particles.  Focused on
 driving physically-meaningful inquiry, it has been applied in domains such as
 astrophysics, seismology, nuclear engineering, molecular dynamics, and
 oceanography.")
    (license (list
              ;; COPYING.txt: for Python code.
              ;;
              ;; yt uses a shared copyright model. Each contributor maintains
              ;; copyright over their contributions to yt. But, it is important
              ;; to note that these contributions are typically only changes to
              ;; the repositories. Thus, the yt source code, in its entirety is
              ;; not the copyright of any single person or institution. Instead,
              ;; it is the collective copyright of the entire yt Development
              ;; Team. If individual contributors want to maintain a record of
              ;; what changes/contributions they have specific copyright on,
              ;; they should indicate their copyright in the commit message of
              ;; the change, when they commit the change to one of the yt
              ;; repositories.
              license:bsd-3
              ;; yt/frontends/artio/artio_headers/LICENSE: for C code.
              license:lgpl3))))

(define-public python-yt-astro-analysis
  (package
    (name "python-yt-astro-analysis")
    (version "1.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yt_astro_analysis" version))
       (sha256
        (base32 "0dy96084wvb7ccnnp22b6wacyjzn1n62i4mwq0q7nkp90bzx44a2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable test which requires MPI setup and failed to run, check why.
      #:test-flags
      #~(list "--ignore=yt_astro_analysis/halo_analysis/tests/test_halo_finders_ts.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools))
    (propagated-inputs
     (list python-h5py
           python-numpy
           python-packaging
           python-yt))
    (home-page "https://github.com/yt-project/yt_astro_analysis")
    (synopsis "YT astrophysical analysis modules")
    (description
     "This package provides an @code{yt} extension package for astrophysical
analysis.  This package contains functionality for:

@itemize
@item halo finding and analysis
@item lightcones
@item planning cosmological simulations for making lightcones and lightrays
@item exporting to the RADMC-3D radiation transport code
@item creating PPV FITS cubes
@end itemize")
    (license (list license:bsd-3 license:lgpl3))))

(define-public python-zodipy
  (package
    (name "python-zodipy")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; XXX: Upstream is not stable with version style
             ;; <https://github.com/Cosmoglobe/zodipy/issues/48>
             (url "https://github.com/Cosmoglobe/zodipy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n51bism8irj2afj4xjyx438ylcc8f1dw2x0jy8xg90x7wdh30cm"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-poetry-core
           python-pytest))
    (propagated-inputs
     (list python-astropy
           python-jplephem
           python-numpy
           python-scipy))
    (home-page "https://github.com/Cosmoglobe/zodipy")
    (synopsis "Zodiacal emission simulations")
    (description
     "ZodiPy is an package for simulating zodiacal light in intensity for
arbitrary solar system observers.")
    (license license:gpl3+)))

(define-public qfits
  (package
    (name "qfits")
    (version "6.2.0")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "ftp://ftp.eso.org/pub/qfits/qfits-" version ".tar.gz"))
       (sha256
        (base32 "0m2b21mim3a7wgfg3ph2w5hv7mdvr03jmmhzipc0wcahijglcw9j"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags
      #~(list (string-append "CFLAGS=-g -O2"
                             " -Wno-error=implicit-function-declaration"))))
    (home-page "https://www.eso.org/sci/software/eclipse/qfits/")
    (synopsis "C library offering access to astronomical FITS files")
    (description
     "@code{qfits} is a C library giving access to FITS file internals, both
for reading and writing.")
    (license license:gpl2+)))

(define-public scamp
  (package
    (name "scamp")
    (version "2.14.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/scamp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wva4c0gz7qrphq713hcnjv84hbwqix05fqmqjq5vypnkdg67p31"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "CPPFLAGS=-fcommon"
              "--enable-openblas"
              "--enable-plplot"
              (string-append "--with-curl-incdir="
                             #$(this-package-input "curl") "/include")
              (string-append "--with-curl-libdir="
                             #$(this-package-input "curl") "/lib")
              (string-append "--with-fftw-incdir="
                             #$(this-package-input "fftwf") "/include")
              (string-append "--with-fftw-libdir="
                             #$(this-package-input "fftwf") "/lib")
              (string-append "--with-openblas-incdir="
                             #$(this-package-input "openblas") "/include")
              (string-append "--with-openblas-libdir="
                             #$(this-package-input "openblas") "/lib")
              (string-append "--with-plplot-incdir="
                             #$(this-package-input "plplot") "/include")
              (string-append "--with-plplot-libdir="
                             #$(this-package-input "plplot") "/lib"))))
    (native-inputs
     (list autoconf
           automake
           libtool
           pkg-config
           python-astropy
           python-numpy
           python-wrapper))
    (inputs
     (list curl fftwf openblas plplot))
    (home-page "https://www.astromatic.net/software/scamp/")
    (synopsis "Compute astrometric solutions")
    (description
     "@acronym{Software for Calibrating AstroMetry and Photometry,SCAMP} is a
software that computes astrometric projection parameters from source
catalogues derived from @url{http://fits.gsfc.nasa.gov/,FITS} images.  The
computed solution is expressed according to the
@url{http://www.atnf.csiro.au/people/mcalabre/WCS/index.html,WCS} standard.
The main features of SCAMP are:

@itemize
@item compatibility with @code{SExtractor} FITS or Multi-Extension FITS
catalogue format in input
@item generation of WCS-compliant and @code{SWarp}-compatible FITS image
headers in output
@item automatic grouping of catalogues on the sky
@item selectable on-line astrometric reference catalogue
@item automatic determination of scale, position angle, flipping and
coordinate shift using fast pattern-matching
@item various astrometric calibration modes for single detectors and detector
arrays
@item combined astrometric solutions for multi-channel/instrument surveys
@item highly configurable astrometric distortion polynomials
@item correction for differential chromatic refraction
@item proper motion measurements
@item multi-threaded code that takes advantage of multiple processors
@item @url{http://www.ivoa.net/documents/VOTable,VOTable}-compliant XML output
of meta-data
@item @url{http://en.wikipedia.org/wiki/XSLT,XSLT} filter sheet provided for
convenient access to metadata from a regular web browser
@end itemize")
    (license license:gpl3+)))

(define-public sextractor
  (package
    (name "sextractor")
    (version "2.28.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/sextractor")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05i2q1665y4hv0dymgcwraq7ixwr706gwfb88vyvi8hazgym2iaq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "--enable-openblas"
         (string-append "--with-cfitsio-incdir="
                        #$(this-package-input "cfitsio") "/include")
         (string-append "--with-cfitsio-libdir="
                        #$(this-package-input "cfitsio") "/lib")
         (string-append "--with-fftw-incdir="
                        #$(this-package-input "fftwf") "/include")
         (string-append "--with-fftw-libdir="
                        #$(this-package-input "fftwf") "/lib")
         (string-append "--with-openblas-incdir="
                        #$(this-package-input "openblas") "/include")
         (string-append "--with-openblas-libdir="
                        #$(this-package-input "openblas") "/lib"))))
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     (list cfitsio fftwf openblas))
    (home-page "https://www.astromatic.net/software/sextractor")
    (synopsis "Extract catalogs of sources from astronomical images")
    (description
     "SExtractor is a program that builds a catalogue of objects from an
astronomical image.  Although it is particularly oriented towards reduction of
large scale galaxy-survey data, it can perform reasonably well on moderately
crowded star fields.")
    (license license:gpl3+)))

(define-public sgp4
  ;; Version tag v1.0 is dated to <2021-01-11>, use the lates commit instead.
  (let ((commit "6a448b4850e5fbf8c1ca03bb5f6013a9fdc1fd91")
        (revision "2"))
    (package
      (name "sgp4")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/dnwrnr/sgp4")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "15q8sain87cbppmzq66y6gp6bvm3kdd1bcchrl59rcvjp0w51wl1"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; FIXME: Tests evaluated via runtest binary, but it's failing even
        ;; when SGP4-VER.TLE file was copied next to it during install phase.
        ;;
        ;; There are 2 more binaries are created after build phase -
        ;; passpredict and sattrack which are failing to execute after
        ;; install, strace output:
        ;;
        ;; strace: exec: Exec format error
        ;;
        #:tests? #f))
      (home-page "https://github.com/dnwrnr/sgp4")
      (synopsis "Simplified perturbations models library")
      (description
       "This is a library implementing the simplified perturbations model.
It can be used to calculate the trajectory of satellites.")
      (license license:asl2.0))))

(define-public siril
  (package
    (name "siril")
    (version "1.4.0-beta4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/free-astro/siril")
             (commit version)))
       (sha256
        (base32 "104wb5m2bb51mi3yqndp0s69281i6px5dxcwlldhjp85cwp3qv51"))
       (file-name (git-file-name name version))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Remove bundled libraries.
            (for-each delete-file-recursively
                      (list "subprojects/healpix_cxx"
                            ;; TODO: Package htmesh, it's included in KSarts
                            ;; - URL: <https://invent.kde.org/education/kstars>
                            ;; - File: <kstars/htmesh>
                            ;; "subprojects/htmesh"
                            ;;
                            ;; TODO: Package kplot - Cairo plotting library
                            ;; <https://github.com/kristapsdz/kplot>.
                            ;; "subprojects/kplot"
                            "subprojects/wcslib"
                            "subprojects/librtprocess"
                            "subprojects/yyjson"))))))
    (build-system meson-build-system)
    (arguments
     (list
      #:glib-or-gtk? #t
      #:configure-flags
      #~(list "--wrap-mode=nodownload")
      #:imported-modules `(,@%meson-build-system-modules
                           (guix build glib-or-gtk-build-system))
      #:modules '((guix build meson-build-system)
                  ((guix build glib-or-gtk-build-system)
                   #:prefix glib-or-gtk:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'generate-gdk-pixbuf-loaders-cache-file
            (assoc-ref glib-or-gtk:%standard-phases
                       'generate-gdk-pixbuf-loaders-cache-file))
          (add-after 'install 'wrap-program
            (lambda* _
              (wrap-program (string-append #$output "/bin/siril")
                ;; Wrapping GDK_PIXBUF_MODULE_FILE to load icons in pure
                ;; environments.
                `("GDK_PIXBUF_MODULE_FILE" =
                  (,(getenv "GDK_PIXBUF_MODULE_FILE")))))))))
    (native-inputs
     (list cmake-minimal
           git
           (list glib "bin") ; for glib-compile-resources
           libconfig
           pkg-config))
    (inputs
     (list bash-minimal ;for wrap-program
           cfitsio
           curl
           exiv2
           ffmpeg
           ffms2
           fftwf
           gdk-pixbuf
           gsl
           gtk+
           gtksourceview-4
           healpix-cxx
           json-glib
           lcms-next

           ;; XXX: It downloads script and data during start up, check if they
           ;; may be presented as packages with wrapped environment variables
           ;; src/io/siril_git.c
           ;; <https://gitlab.com/free-astro/siril-scripts>
           ;; <https://gitlab.com/free-astro/siril-spcc-database>.
           libgit2

           libheif
           libjxl
           libraw
           (librsvg-for-system)
           librtprocess
           libxisf
           lz4
           opencv
           pugixml
           wcslib
           yyjson))
    (home-page "https://siril.org/")
    (synopsis "Image processing software for amateur astronomy")
    (description
     "This package provides an astronomical image processing tool - SIRIL.  It
is specially tailored for noise reduction and improving the signal/noise ratio
of an image from multiple captures, as required in astronomy.  SIRIL can align
automatically or manually, stack and enhance pictures from various file
formats, even image sequence files (films and SER files).  It works well with
limited system resources, like in embedded platforms, but is also very fast
when run on more powerful computers and provides conversion to FITS from a
large number of image formats.")
    (license license:gpl3+)))

(define-public ska-sdp-func
  (package
    (name "ska-sdp-func")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/ska-telescope/sdp/ska-sdp-func")
             (commit version)))
       (sha256
        (base32 "0blk4sfy2kl544d7iahcd9awvlg3xvwcm5qmis6h4xiw7xgj7psf"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_INFO=ON"
              "-DFIND_CUDA=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda _
              (substitute* "tests/CMakeLists.txt"
                ;; Two tests fail with SegFAilt: "Cannot allocate GPU memory:
                ;; The processing function library was compiled without CUDA
                ;; support".
                (("clean") "# clean")
                ;; test_weighting fails with error: sdp_weighting_uniform:
                ;; Expected 'output_weight' not to be read-only.
                (("visibility") "# visibility")))))))
    (home-page "https://developer.skao.int/projects/ska-sdp-func/en/latest/")
    (synopsis "SDP Processing Function Library")
    (description
     "This package provides a Square Kilometre Array (SKA) Science Data
Processor (SDP) function library for radio astronomy.")
    (license license:bsd-3)))

(define-public python-ska-sdp-func
  (package/inherit ska-sdp-func
    (name "python-ska-sdp-func")
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; Ignore tests requiring CUDA.
         "--ignore=tests/visibility/test_tiled_functions.py"
         "--ignore=tests/visibility/test_opt_weighting.py"
         ;; We don't want benchmark tests.
         "--ignore=tests/fourier_transforms/test_swiftly_bench.py"
         ;; Skip tests failing with errors:
         ;; UnboundLocalError: local variable 'psf' referenced before assignment
         ;; AttributeError: 'NoneType' object has no attribute 'asnumpy'
         "-k" "not test_hogbom_clean and not test_ms_clean_cornwell")))
    (native-inputs
     (list cmake
           python-pytest
           python-scipy
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy))))

(define-public skymaker
  (package
    (name "skymaker")
    (version "3.10.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/skymaker")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kcy0lfm25gihahpr41ghhw3b8znzczzg9rrlkfih043h2pfsm5l"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "CPPFLAGS=-fcommon"
         (string-append
          "--with-fftw-libdir=" #$(this-package-input "fftwf") "/lib")
         (string-append
          "--with-fftw-incdir=" #$(this-package-input "fftwf") "/include"))))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (inputs
     (list fftwf))
    (home-page "https://www.astromatic.net/software/skymaker")
    (synopsis "Astronomical image simulator")
    (description
     "SkyMaker is a program that simulates astronomical images.  It accepts
object lists in ASCII generated by the Stuff program to produce realistic
astronomical fields.  SkyMaker is part of the
@uref{https://www.astromatic.net/projects/efigi, EFIGI} development project.")
    (license license:gpl3+)))

(define-public splash
  (package
    (name "splash")
    (version "3.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danieljprice/splash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0jd8pcy9gj8xw515x4rd7pdxz02z24y5nb0gqfapncr9j8qpd6jp"))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: Tests failed
     ;; Issue submitted upstream https://github.com/danieljprice/splash/issues/67
     ;;
     ;; make: *** No rule to make target 'test_interpolate3D.o', needed by 'test1'.  Stop.
     ;;
     (list #:tests? #f
           #:parallel-build? #f ;parallel build fails
           #:make-flags #~(list "SYSTEM=gfortran" "PREFIX="
                                (string-append "GIZA_DIR="
                                               #$(this-package-input "giza"))
                                (string-append "DESTDIR="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-before 'install 'create-install-dirrectories
                          (lambda _
                            (mkdir-p (string-append #$output "/bin")))))))
    (native-inputs
     (list gfortran
           pkg-config
           perl
           python-wrapper))
    (inputs
     (list cairo
           cfitsio
           giza))
    (home-page "https://users.monash.edu.au/~dprice/splash/")
    (synopsis "Astrophysical visualisation tool for smoothed particle hydrodynamics")
    (description
     "SPLASH is visualisation tool for Smoothed Particle Hydrodynamics (SPH)
simulations in one, two and three dimensions, developed mainly for
astrophysics.  It uses a command-line menu but data can be manipulated
interactively in the plotting window.")
    (license license:gpl2+)))

(define-public stackistry
  (package
    (name "stackistry")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreatAttractor/stackistry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rz29v33n0x0k40hv3v79ym5ylch1v0pbph4i21809gz2al5p7dq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; No test target
      #:make-flags
      #~(list
         (string-append
          "SKRY_INCLUDE_PATH=" #$(this-package-input "libskry") "/include")
         (string-append
          "SKRY_LIB_PATH=-L" #$(this-package-input "libskry") "/lib")
         (string-append
          "LIBAV_INCLUDE_PATH=" #$(this-package-input "ffmpeg") "/include"))
      #:phases
      #~(modify-phases %standard-phases
          ;; no configure and tests are provided
          (delete 'configure)
          (add-after 'unpack 'fix-paths
            (lambda _
              (substitute* "src/main.cpp"
                (("\"\\.\\.\", \"lang\"")
                 "\"../share/stackistry\", \"lang\""))
              (substitute* "src/utils.cpp"
                (("\"\\.\\.\", \"icons\"")
                 "\"../share/stackistry\", \"icons\""))))
          (replace 'install
            ;; The Makefile lacks an ‘install’ target.
            (lambda _
              (let* ((out #$output)
                     (bin (string-append out "/bin"))
                     (icons (string-append out "/share/stackistry/icons"))
                     (lang (string-append out "/share/stackistry/lang")))
                (copy-recursively "bin" bin)
                (copy-recursively "icons" icons)
                (copy-recursively "lang" lang)))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtkmm-3 libskry ffmpeg-4))
    (home-page "https://github.com/GreatAttractor/stackistry")
    (synopsis "Astronomical lucky imaging/image stacking tool")
    (description
     "Stackistry implements the lucky imaging principle of astronomical
imaging: creating a high-quality still image out of a series of many (possibly
thousands) low quality ones (blurred, deformed, noisy).  The resulting image
stack typically requires post-processing, including sharpening (e.g. via
deconvolution).  Such post-processing is not performed by Stackistry.")
    (license license:gpl3+)))

(define-public stellarium
  (package
    (name "stellarium")
    (version "25.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/Stellarium/stellarium")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05r3faqarhi4jipaphs6yh4aq6jy2aln67kg53kcqjlxbyxkmr7n"))))
    (build-system qt-build-system)
    ;; TODO: Complete documentation build and split into dedicated outputs.
    (arguments
     (list
      ;; FIXME: Tests keep failing on 100% when preparing test-suit for INDI.
      #:tests? #f
      #:configure-flags
      #~(list "-DENABLE_GPS=1"
              "-DENABLE_TESTING=0"
              (string-append "-DCMAKE_CXX_FLAGS=-isystem "
                             #$(this-package-input "qtpositioning") "/include/qt6"
                             " -isystem "
                             #$(this-package-input "qtserialport") "/include/qt6"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-offscreen-display
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (setenv "HOME" "/tmp"))))))
    (inputs
     (list calcmysky
           eigen
           glm
           gpsd
           indi
           libnova
           md4c
           nlopt
           openssl
           qtbase
           qtcharts
           qtlocation
           qtmultimedia
           qtpositioning
           qtserialport
           qttranslations
           qtwayland
           qtwebengine
           qxlsx
           zlib))
    (native-inputs
     (list doxygen
           gettext-minimal
           graphviz
           mesa
           perl
           python-wrapper
           qttools))
    (home-page "https://stellarium.org/")
    (synopsis "3D sky viewer")
    (description
     "Stellarium is a planetarium.  It shows a realistic sky in
3D, just like what you see with the naked eye, binoculars, or a telescope.  It
can be used to control telescopes over a serial port for tracking celestial
objects.")
    (license license:gpl2+)))

(define-public stuff
  ;; XXX: No version tag available in GitHub.
  ;; See: https://github.com/astromatic/stuff/issues/6
  (let ((commit "9cf363216a0d5c3d41b3dc759994feee3be0fc4e")
        (revision "0"))
    (package
      (name "stuff")
      (version (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/astromatic/stuff")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n7f2f7llwf1fbliypblia3xyrh69yy1rqbwwhz897qd92fxnl5i"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake libtool pkg-config))
      (home-page "https://www.astromatic.net/software/stuff")
      (synopsis "Astronomical catalogue simulation")
      (description
       "Stuff is a program that simulates \"perfect\" astronomical
catalogues. It generates object lists in ASCII which can read by the SkyMaker
program to produce realistic astronomical fields.  Stuff is part of the
@uref{https://www.astromatic.net/projects/efigi, EFIGI} development project.")
      (license license:gpl3+))))

;; TODO: This is not an original source, and currently fails to build:
;; <https://directory.fsf.org/wiki/Sunclock>
;;  -> <https://github.com/mdoege/Sunclock> dead link
;;   -> <http://www.arvernes.com/wiki/index.php/Sunclock> dead link
;; <https://github.com/nongiach/Sunclock> is unmaintained.
;;
;; Maybe use Debian's fork instead
;; <https://salsa.debian.org/debian/sunclock>, it would need to apply 12
;; patches from <debian/patches>?
;; Mark as not public for now.
(define sunclock
  (let ((commit "f4106eb0a81f7594726d6b2859efd8fc64cc1225")
        (revision "1"))
    (package
      (name "sunclock")
      (version (git-version "3.57" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/nongiach/Sunclock")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1rczdpmhvfw57b9r793vq8vqlbdhlkgj52fxwrdfl6cwj95a9kv2"))))
      (build-system gnu-build-system)
      (arguments
       `(#:make-flags
         (list (string-append "DESTDIR=" %output)
               ;; Fix incorrect argument given to gcc. Error message:
               ;; "gcc: error: DefaultGcc2AMD64Opt: No such file or directory"
               "CDEBUGFLAGS=")
         #:phases
         (modify-phases %standard-phases
           (replace 'configure
             (lambda _
               (chdir "sunclock-3.57")
               (substitute* "Imakefile"
                 (("^MANDIR=/X11R6/man/man1")
                  "MANDIR=/share/man/man1")
                 (("^BINDIR=/X11R6/bin")
                  "BINDIR=/bin")
                 ;; Disable ZLIB support for vmf files because zlib implements
                 ;; `gzgetc` as a macro instead of a function, which results in
                 ;; a compilation error.
                 ((" -DZLIB") "")
                 ((" -lz") "")
                 (("cd \\$\\(DESTDIR\\)\\$\\(SHAREDIR\\)/earthmaps/vmf ; \
gzip -f \\*.vmf")
                  ""))
               ;; Generate Makefile.
               (invoke "xmkmf"))))
         #:tests? #f))  ; No check target.
      (inputs
       (list libjpeg-turbo libpng libx11 libxpm))
      (native-inputs
       (list imake))
      (home-page "https://github.com/nongiach/Sunclock")
      (synopsis
       "Map of the Earth that shows which portion is illuminated by the Sun")
      (description
       "Sunclock displays a map of the Earth and shows which portion is
illuminated by the Sun.  It can commute between two states, the \"clock window\"
and the \"map window\".  The clock window displays a small map of the Earth and
therefore occupies little space on the screen, while the \"map window\" displays
a large map and offers more advanced functions: local time of cities, Sun and
Moon position, etc.")
      (license license:gpl2+))))

(define-public sunwait
  (package
    (name "sunwait")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/risacher/sunwait")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mzc8bb7zzl1ch3v7w08vw2v50yjxvr7phyb78njpq89wy6hsrxz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no tests provided
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target)))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure script is provided
          (replace 'install
            (lambda _
              (install-file "sunwait"
                            (string-append #$output "/bin")))))))
    (home-page "https://github.com/risacher/sunwait")
    (synopsis "Sunrise and sunset times calculation")
    (description
     "Sunwait calculates sunrise or sunset times with civil, nautical,
astronomical and custom twilights.  The sun's position is calculated using
time, and position - latitude and longitude should be specified on the command
line.

Features:

@itemize
@item calculates sunrise and sunset for given coordinates
@item can wait for sunrise/sunset, or return DAY or NIGHT codes
@item works with Windows Task Scheduler (or cron)
@item supports custom twilight angles
@item used to automate domestic lighting with Arduino transmitter and radio
controlled sockets
@end itemize")
    (license license:gpl3)))

(define-public swarp
  (package
    (name "swarp")
    (version "2.41.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/swarp")
             (commit (string-append version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00463r5rd4xl74xs4h1n4gl2qk7v9p5nw9x05pbzgh8jm77q90qq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list automake autoconf libtool pkg-config))
    (inputs
     (list cfitsio))
    (home-page "https://www.astromatic.net/software/swarp")
    (synopsis "FITS image resampling and co-addition")
    (description
     "SWarp is a program that resamples and co-adds together FITS images using
any arbitrary astrometric projection defined in the WCS standard.")
    (license license:gpl3+)))

(define-public tangos
  (package
    (name "tangos")
    (version "1.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/pynbody/tangos")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mvg6qvalm4rf8gwi0d8lbljww51ym2lmc044i28fr1815prdhng"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; tests: 320 passed, 1 skipped, 1 deselected, 120 warnings
      #:test-flags
      ;; OSError: File PosixPath('test_simulations/test_tipsy/tiny.000640'):
      ;; format not understood or does not exist
      #~(list "--deselect=tests/test_bh_reader.py::test_bhlog" )
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-broken-scripts
            (lambda _
              ;; TODO: Report upstram.
              ;; I could not find "Simpy" module, see:
              ;; - URL: <https://github.com/pynbody/tangos/commit>
              ;; - commit: 7de987e27d756278c4571dc65f36d377027fa29d
              ;; - comment: #commitcomment-167855052
              (delete-file "tangos/scripts/preprocess_bh.py")
              (substitute* "setup.py"
                ;; ModuleNotFoundError: No module named
                ;; 'tangos.scripts.bh_timelink'
                ((".*tangos_bh_timelink =.*") "")
                ;; It looks this script uses not a publicly available module:
                ;; import Simpy.BlackHoles.orbit.
                ((".*tangos_preprocess_bh =.*") ""))))
         (add-before 'check 'pre-check
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "TANGOS_TESTING_DB_BACKEND" "sqlite"))))))
    (native-inputs
     (list python-pymysql
           python-pynbody
           python-pyquery
           python-pytest
           python-webtest
           python-yt))
    (inputs
     (list python-hupper
           python-matplotlib
           python-more-itertools
           python-numpy
           python-packaging
           python-pastedeploy
           python-plaster
           python-plaster-pastedeploy
           python-psycopg2
           python-pymysql
           python-pyparsing
           python-pyramid
           python-pyramid-debugtoolbar
           python-pyramid-jinja2
           python-pyramid-retry
           python-pyramid-tm
           python-repoze-lru
           python-scipy
           python-setuptools
           python-sqlalchemy-2
           python-tblib
           python-tqdm
           python-translationstring
           python-venusian
           python-waitress
           python-webob
           python-zope-deprecation
           python-zope-interface
           python-zope-sqlalchemy))
    (home-page "https://github.com/pynbody/tangos")
    (synopsis "Agile numerical galaxy organisation system")
    (description
     "TANGOS is a tool to build a database (along the lines of
@url{http://icc.dur.ac.uk/Eagle/database.php, Eagle} or
@url{https://www.cosmosim.org/cms/documentation/projects/multidark-bolshoi-project/,
MultiDark}) for cosmological and zoom simulations.

Features:
@itemize
@item designed to store and manage results from custom analysis code
@item provides web and Python interfaces
@item science-focussed queries across entire merger trees, without requiring
any knowledge of SQL
@item manages the process of populating the database with science data,
including auto-parallelising custom analysis
@item customization with multiple Python modules such as @code{pynbody} or
@code{yt} to process raw simulation data
@item suports file-based database SQLite, server-based MySQL and PostgreSQL
@end itemize")
    (license license:bsd-3)))

(define-public tempo
  (package
    (name "tempo")
    (version "13.103")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/tempo/tempo-"
                           version ".tar.gz"))
       (sha256
        (base32 "1ghml56sd7rhwymrlqlvj3g12hjyqvk2sjl08rqlf5y7dwdp0nvl"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://github.com/nanograv/tempo")
    (synopsis "Pulsar timing data analysis program")
    (description
     "Tempo analyzes pulsar timing data.  Pulse times of arrival (TOAs),
pulsar model parameters, and coded instructions are read from one or more
input files.  The TOAs are fitted by a pulse timing model incorporating
transformation to the solar-system barycenter, pulsar rotation and spin-down
and, where necessary, one of several binary models.  Program output includes
parameter values and uncertainties, residual pulse arrival times, chi-squared
statistics, and the covariance matrix of the model.  In prediction
mode,ephemerides of pulse phase behavior (in the form of polynomial
expansions) are calculated from input timing models.")
    (license license:gpl2+)))

(define-public tempo2
  (package
    (name "tempo2")
    (version "2025.02.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://bitbucket.org/psrsoft/tempo2")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06494q0zff1qj813y70r014ifm60cminhk7lisy4by022mr6wd3k"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-pathes
            (lambda _
              (substitute* (list "bootstrap" "plugin/make_build_settings.sh")
                (("/bin/bash") (which "bash"))
                (("/bin/echo") "echo"))))
          (add-before 'configure 'set-t2runtime-path
            (lambda _
              (setenv "TEMPO2" (string-append #$output "/share/runtime"))))
          (add-after 'install 'install-t2runtime-files
            (lambda _
              (copy-recursively "T2runtime"
                                (string-append #$output "/share/runtime")))))))
    (native-inputs
     (list autoconf
           automake
           gfortran
           libtool
           pkg-config))
    (home-page "https://bitbucket.org/psrsoft/tempo2")
    (synopsis "High precision pulsar timing tool")
    (description
     "Tempo2 is a pulsar timing package, based on the old FORTRAN TEMPO code
to address some shortcomings in that code for high precision pulsar timing.
See related paper
@url{https://ui.adsabs.harvard.edu/abs/2006MNRAS.369..655H/abstract}.")
    (license license:gpl3+)))

(define-public tenmon
  (package
    (name "tenmon")
    (version "20251101")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.nouspiro.space/nou/tenmon")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0m6hhyd64lichxr06b93bl6vfacfya3xwq47m5m4d6y5awkqc4ij"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;no test target
      #:configure-flags
      #~(list "-DCMAKE_C_FLAGS=-Wno-error=implicit-function-declaration")
      #:phases
      #~(modify-phases %standard-phases
          ;; libxisf is expected to be found as git submodule, link it before
          ;; build.
          (add-after 'unpack 'link-libxisf
            (lambda _
              (rmdir "libXISF")
              (symlink #+(package-source (this-package-native-input "libxisf"))
                       (string-append (getcwd) "/libXISF")))))))
    (native-inputs
     (list git-minimal/pinned
           libxisf-for-tenmon
           pkg-config ))
    (inputs
     (list cfitsio
           glu
           gsl
           libexif
           libraw
           qtbase
           qtcharts
           qtdeclarative
           wcslib
           zstd
           (list zstd "lib")))
    (home-page "https://nouspiro.space/?page_id=206")
    (synopsis "FITS and XISF image viewer, converter and indexer")
    (description
     "FITS/XISF image viewer with multithreaded image loading.  It is intended
primarily for viewing astro photos and images with support of following
formats:

@itemize
@item FITS 8, 16 bit integer and 32 bit float
@item XISF 8, 16 bit integer and 32 bit float
@item RAW CR2, DNG, NEF
@item JPEG, PNG, BMP, GIF, PBM, PGM, PPM and SVG images
@end itemize

Features:
@itemize
@item using same stretch function as PixInsight
@item OpenGL accelerated drawing
@item index and search FITS XISF header data
@item quick mark images and then copy/move marked files
@item convert FITS <-> XISF
@item convert FITS/XISF -> JPEG/PNG
@item image statistics mean, media, min, max
@item support for WCS
@item thumbnails
@item convert CFA images to colour - debayer
@item color space aware
@end itemize")
    (license license:gpl3+)))

(define-public unsio
  ;; There is no versioned tag, use the latest commit.
  (let ((commit "ac48210ec24432ec3ad330c4203e7eb21876a921")
        (revision "1"))
    (package
      (name "unsio")
      (version (git-version "1.3.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://gitlab.lam.fr/infrastructure/unsio")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0diiiflwz5yw2hpk3xj5x5iviyfibim4lhs02qn07hfw86qi9vn2"))
                (modules '((guix build utils)))
                (snippet
                  ;; force installation into lib/ instead of lib64/
                  #~(substitute* "cmake/DetectCpackGenerator.cmake"
                      (("lib64") "lib")))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f ; no tests
             #:build-type "Release" ; to improve performance
             #:configure-flags #~(list "-DCMAKE_CXX_STANDARD=14")))
      (inputs (list gfortran hdf5 perl sqlite zlib))
      (home-page "https://projets.lam.fr/projects/unsio/wiki")
      (synopsis "Input and output routines for n-body file formats")
      (description
       "@acronym{UNSIO, Universal Nbody Snapshot Input Output} provides
an API for performing input and output operations on different kinds of
n-body file formats (nemo, Gadget binaries 1 and 2, Gadget hdf5, Ramses).")
      (license license:cecill))))

(define-public uraniborg
  (package
    (name "uraniborg")
    (version "0.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://codeberg.org/astronexus/uraniborg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08pyb2dw7162673526iwr2s24a4fvssaq72ivi92mq41qm5nkp6p"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "bitbucket.org/dpnash/uraniborg"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'patch-config
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "consts.go"
                  (("DEFAULT_BASE_DIR = \".\"")  (format #f "DEFAULT_BASE_DIR = ~s" #$output))
                  (("\"config\"") (format #f "~s" "etc/uraniborg"))
                  (("\"data\"") (format #f "~s" "share/uraniborg/data"))
                  (("\"charts\"") (format #f "~s" "share/uraniborg/charts"))
                  (("\"fonts\"") (format #f "~s" "share/uraniborg/fonts")))
                (substitute* (find-files "config" ".*\\.yaml$")
                  (("fonts/") (string-append #$output "/share/uraniborg/fonts/"))))))
          (add-after 'install 'install-runtime-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (let ((etc (string-append #$output "/etc/uraniborg"))
                      (data (string-append #$output "/share/uraniborg/data"))
                      (charts (string-append #$output "/share/uraniborg/charts"))
                      (fonts (string-append #$output "/share/uraniborg/fonts")))
                  (copy-recursively "config" etc)
                  (copy-recursively "charts" charts)
                  (copy-recursively "fonts" fonts)
                  (system* "gunzip" "data/athyg_33_subset.csv.gz")
                  (install-file "data/athyg_33_subset.csv" data)))))
          (delete 'check)
          (add-after 'install-runtime-files 'post-install-check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (native-inputs
     (list go-codeberg-org-astronexus-brahe
           go-dario-cat-mergo
           go-github-com-fogleman-gg
           go-github-com-fsnotify-fsnotify
           go-gopkg-in-yaml-v3))
    (home-page "https://codeberg.org/astronexus/uraniborg")
    (synopsis "CLI star chart application for the AT-HYG catalog")
    (description
     "@code{uraniborg} is a CLI visualization tool and star chart \"engine\"
for the Augmented Tycho + HYG (AT-HYG) star catalog.  The
@url{https://codeberg.org/astronexus/athyg, AT-HYG} catalog consists of stars
from the Tycho-2 star catalog, augmented with additional distance and velocity
information from Gaia DR3, as well as the \"classic\" / historical information
from the HYG catalog.

@code{uraniborg} lets you view the sky from both the solar system and from any
star in the AT-HYG catalog with a known distance (over 2.5 million stars
currently).

Base directory containing custom config, data, charts and fonts may be
adjusted with command line option @code{-b}, by default set to store path.")
    (license (list license:asl2.0    ;; Roboto fonts
                   license:silofl1.1 ;; Noto Sans fonts
                   license:gpl3+))))

(define-public uranimator
  ;; No release or version tags.
  (let ((commit "d96f0084c029667f313ec72aade9968fa5bb506e")
        (revision "1"))
    (package
      (name "uranimator")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/astronexus/uranimator.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yf1ghzzpnh0v1ra6dv4l135jgbs28pzadqig8cy8bwi9jq5wlrn"))))
      (build-system go-build-system)
      (arguments
       (list
        #:install-source? #f
        #:import-path "codeberg.org/astronexus/uranimator"))
      (native-inputs
       (list go-codeberg-org-astronexus-brahe
             go-github-com-jessevdk-go-flags
             go-gonum-org-v1-gonum
             go-gopkg-in-yaml-v3))
      (home-page "https://codeberg.org/astronexus/uranimator")
      (synopsis "Create sets of astro .png files that can be turned into animations")
      (description
       "@code{uranimator} is a CLI tool that works with your existing
@url{https://codeberg.org/astronexus/uraniborg,(code uraniborg)} install to
create animations.  See how the sky evolves over a million years or what
traveling to a star 100 light years away looks like.")
      (license license:agpl3))))

(define-public wcslib
  (package
    (name "wcslib")
    (version "8.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.atnf.csiro.au/computing/software/wcs/"
                           "wcslib-releases/"
                           "wcslib-" version ".tar.bz2"))
       (sha256
        (base32 "1kpacbh6zrci44qab0jllkn3a9z3b465w17qcfivvfpxzdw1pzgi"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "C/flexed")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-cfitsiolib="
                             #$(this-package-input "cfitsio") "/lib")
              (string-append "--with-cfitsioinc="
                             #$(this-package-input "cfitsio") "/include"))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'install-license-files) ; installed by ‘make install’
          (add-before 'configure 'patch-/bin/sh
            (lambda _
              (substitute* "makedefs.in"
                (("/bin/sh") "sh")))))))
    ;; TODO: Fix build with gfortran and pack missing optional pgplot.
    ;; (inputs (list gfortran pgplot))
    (inputs
     (list cfitsio))
    (native-inputs
     (list flex))
    (home-page "https://www.atnf.csiro.au/computing/software/wcs/")
    (synopsis "Library which implements the FITS WCS standard")
    (description "The FITS \"World Coordinate System\" (@dfn{WCS}) standard
defines keywords and usage that provide for the description of astronomical
coordinate systems in a @dfn{FITS} (Flexible Image Transport System) image
header.")
    (license license:lgpl3+)))

;;; The version is required for julia-wcs-jll and julia-wcs.  They do not
;;; support version higher than 7.x.
(define-public wcslib-7.12
  (package
    (inherit wcslib)
    (version "7.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.atnf.csiro.au/computing/software/wcs/"
                           "wcslib-" version ".tar.bz2"))
       (sha256
        (base32 "1m3bx6gh5w3c7vvsqcki0x20mg8lilg13m0i8nh7za89w58dxy4w"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "C/flexed")))))
    (properties '((hidden? . #t)))))

(define-public wcstools
  (package
    (name "wcstools")
    (version "3.9.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://tdc-www.harvard.edu/software/wcstools/wcstools-"
             version ".tar.gz"))
       (sha256
        (base32 "125hqzspvqrx6372smzsmxwg06ib2arjc5awnwnq53w1xdq6jpsj"))
       (patches (search-patches "wcstools-extend-makefiles.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ;No tests provided.
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "http://tdc-www.harvard.edu/software/wcstools/")
    (synopsis "Handle the WCS of a FITS image")
    (description
     "WCSTools is a set of software utilities, written in C, which create,
display and manipulate the world coordinate system of a FITS or IRAF image,
using specific keywords in the image header which relate pixel position within
the image to position on the sky.  Auxiliary programs search star catalogs and
manipulate images.")
    (license license:gpl2+)))

(define-public webbpsf-data
  (package
    (name "webbpsf-data")
    (version "1.5.0")
    (source
     (origin
       (method url-fetch)
       ;; 73.9MiB archive
       (uri "https://stsci.box.com/shared/static/qxpiaxsjwo15ml6m4pkhtk36c9jgj70k.gz")
       (file-name (string-append "webbpsf-data-" version ".tar.gz"))
       (sha256
        (base32 "0lnzjvn9276v00p5g4894jgdb9mw4mr89q64l7ywikz9khk3j5gc"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("." "share/webbpsf-data"))))
    (home-page "https://webbpsf.readthedocs.io/en/stable/installation.html")
    (synopsis "JWST pupil shape, instrument throughputs, and aperture positions data files")
    (description
     "This package contains FIT and CSV files required for WebbPSF
installation and distributed separately from it.")
    (license license:bsd-3)))

(define-public weightwatcher
  (package
    (name "weightwatcher")
    (version "1.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/weightwatcher")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0701z6bdqq32jv7ga3n6jh27q684ni0hbfjm1mak7rh0qqx089gi"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "CFLAGS=-fcommon")))      ; fix build with GCC 10
    (home-page "https://www.astromatic.net/software/weightwatcher")
    (synopsis "Weight-map/flag-map multiplexer and rasteriser")
    (description
     "Weightwatcher is a program hat combines weight-maps, flag-maps and
polygon data in order to produce control maps which can directly be used in
astronomical image-processing packages like Drizzle, Swarp or SExtractor.")
    (license license:gpl3+)))

(define-public xplanet
  (package
    (name "xplanet")
    (version "1.3.1")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "mirror://sourceforge/xplanet/xplanet/"
         version "/xplanet-" version ".tar.gz"))
       (sha256
        (base32 "1rzc1alph03j67lrr66499zl0wqndiipmj99nqgvh9xzm1qdb023"))
       (patches
        (search-patches
         "xplanet-1.3.1-cxx11-eof.patch"
         "xplanet-1.3.1-libdisplay_DisplayOutput.cpp.patch"
         "xplanet-1.3.1-libimage_gif.c.patch"
         "xplanet-1.3.1-xpUtil-Add2017LeapSecond.cpp.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; No NASA JPL cspice support.
         "--without-cspice"
         (string-append "CPPFLAGS=-I" #$(this-package-input "netpbm")
                        "/include/netpbm"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list freetype
           giflib
           libice
           libjpeg-turbo
           libpng
           libtiff
           libx11
           libxscrnsaver
           netpbm
           pango
           zlib))
    (home-page "https://xplanet.sourceforge.net/")
    (synopsis "Planetary body renderer")
    (description
     "Xplanet renders an image of a planet into an X window or file.
All of the major planets and most satellites can be drawn and different map
projections are also supported, including azimuthal, hemisphere, Lambert,
Mercator, Mollweide, Peters, polyconic, orthographic and rectangular.")
    (license license:gpl2+)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetical order.
;;;
