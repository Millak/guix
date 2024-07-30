;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2023 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018-2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 by Amar Singh <nly@disroot.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021-2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2023 Iliya Tikhonenko <tikhonenko@mpe.mpg.de>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2023 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Andy Tai <lichengtai@gmail.com>
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
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gps)
  #:use-module (gnu packages graph)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages jupyter)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages plotutils)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages web)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module ((guix build-system python) #:select (pypi-uri))
  #:use-module (guix build-system pyproject)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public alfa
  (package
    (name "alfa")
    (version "2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rwesson/ALFA")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0aqxqar36822mh373awsl79j7zn8vik4yddyydsxv0c76gn4i2k3"))
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
  (let ((commit "9272ea36693a7ce7aa3524a9e212a60a509c3b8a")
        (revision "2"))
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
          (base32 "0klcrrlkc4sjpr83m2gnwb65lg798rydyvrlpangf7np1qg4zbk5"))
         (file-name (git-file-name name version))))
      (build-system copy-build-system)
      (arguments
       (list #:install-plan
             #~'(("include/aocommon" "include/aocommon"))))
      (home-page "https://gitlab.com/aroffringa/aocommon")
      (synopsis "Collection of functionality that is reused in astronomical applications")
      (description
       "This package provides source-only AOCommon collection of functionality that is
reused in several astronomical applications, such as @code{wsclean},
@code{aoflagger}, @code{DP3} and @code{everybeam}.")
      (license license:gpl3+))))

(define-public aoflagger
  (package
    (name "aoflagger")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/aroffringa/aoflagger")
             (commit (string-append "v" version))))
       (sha256
        (base32 "0dxmcy04cayhs4s2z41wls1dnmg9hkffvlqcmc660idqziffvv1g"))
       (patches
        (search-patches "aoflagger-use-system-provided-pybind11.patch"))
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
           boost
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
    (license license:gpl3+)))

(define-public calceph
  (package
    (name "calceph")
    (version  "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.obspm.fr/imcce_calceph/calceph")
             (commit (string-append name "_"
                                    (string-replace-substring version "." "_")))))
       (sha256
        (base32 "1yabdq51plg3dijp68xajhsz395gi2fyp5qkvrj3dgv8d4qw52nw"))
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
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/10110111/CalcMySky")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kybjlddrm8x4x5y82qczi6z1d2riv6zcfjzrh7pzg2vwj89izh0"))))
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
    (synopsis "Qt5 build for the CalcMySky library.")))

(define-public casacore
  (package
    (name "casacore")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/casacore/casacore")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "05ar5gykgh4dm826xplj5ri5rw7znhxrvin2l67a3mjwfys7r2a0"))
       (file-name (git-file-name name version))))
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
              "-DBUILD_TESTING=TRUE"
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
                (("YODP=.*$") "YODP=\n")))))))
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
    (version "2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://heasarc.gsfc.nasa.gov/docs/software/fitsio/ccfits/"
             "CCfits-" version ".tar.gz"))
       (sha256
        (base32 "04l6na8vr5xadz3rbx62as79x1ch4994vbb625kx0dz5czdkkd1b"))))
    (build-system cmake-build-system)
    (inputs (list cfitsio))
    (home-page "https://heasarc.gsfc.nasa.gov/docs/software/fitsio/ccfits/")
    (synopsis "C++ interface to the CFITSIO")
    (description
     "CCfits is an object oriented interface to the cfitsio library.  It is
designed to make the capabilities of cfitsio available to programmers working in
C++.  It is written in ANSI C++ and implemented using the C++ Standard Library
with namespaces, exception handling, and member template functions.")
    (license (license:non-copyleft "file://License.txt"
                                   "See License.txt in the distribution."))))

(define-public celestia
  (package
    (name "celestia")
    (version "1.6.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/celestiaproject/celestia")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nz9k5nd2zmrbwj1qhsfwmvqymqk8c4yjxpybck44isrild2ah9j"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      `((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1)
        (srfi srfi-71))
      #:configure-flags
      #~(list "--with-glut"
              (string-append "--with-lua=" #$(this-package-input "lua")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-lua-version
            (lambda _
              (let* ((_ version (package-name->name+version
                                 #$(this-package-input "lua")))
                     (components (string-split version #\.))
                     (major+minor (string-join (take components 2) ".")))
                (substitute* "configure.ac"
                  (("lua5.3")
                   (string-append "lua-" major+minor)))))))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           libgit2
           libtool
           perl
           pkg-config))
    (inputs
     (list freeglut
           glu
           libjpeg-turbo
           libpng
           libtheora
           mesa))
    (propagated-inputs
     (list lua))
    (home-page "https://celestia.space/")
    (synopsis "Real-time 3D visualization of space")
    (description
     "This simulation program lets you explore our universe in three
dimensions.  Celestia simulates many different types of celestial objects.
From planets and moons to star clusters and galaxies, you can visit every
object in the expandable database and view it from any point in space and
time.  The position and movement of solar system objects is calculated
accurately in real time at any rate desired.")
    (license license:gpl2+)))

(define-public celestia-gtk
  (package/inherit celestia
    (name "celestia-gtk")
    (inputs
     (modify-inputs (package-inputs celestia)
       (replace "freeglut" gtk+-2)
       (prepend cairo gtkglext libxmu libtheora pango-1.42)))
    (arguments
     (substitute-keyword-arguments (package-arguments celestia)
       ((#:configure-flags flags '())
        #~(append #$flags
                  (list "--enable-cairo"
                        "--enable-theora"
                        "--without-glut"
                        "--with-gtk")))))
    (synopsis "Real-time 3D visualization of space (using GTK+)")))

(define-public cfitsio
  (package
    (name "cfitsio")
    (version "4.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/"
             "cfitsio-" version ".tar.gz"))
       (sha256
        (base32 "098x1l8ijwsjp2ivp3v7pamrmpgwj5xmgb4yppm9w3w044zxr8b6"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-bzip2=" #$(this-package-input "bzip2")))
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
    (description "CFITSIO provides simple high-level routines for reading and
writing @dfn{FITS} (Flexible Image Transport System) files that insulate the
programmer from the internal complexities of the FITS format. CFITSIO also
provides many advanced features for manipulating and filtering the information
in FITS files.")
    (license (license:non-copyleft "file://License.txt"
                                   "See License.txt in the distribution."))))

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

(define-public python-ads
  (package
    (name "python-ads")
    (version "0.12.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ads" version))
       (sha256
        (base32 "18aizbsmhwz99flz8n101mi0n0lk3m3qqzfvmxrmjwqvydfypjml"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-httpretty))
    (propagated-inputs
     (list python-mock
           python-requests
           python-six
           python-werkzeug))
    (home-page "http://www.github.com/andycasey/ads/")
    (synopsis "Python client to NASA's Astrophysics Data System")
    (description
     "This package provdies a Python Module to Interact with NASA's
@acronym{Astrophysics Data System,ADS} that Doesn't Suck™.")
    (license license:expat)))

(define-public python-aplpy
  (package
    (name "python-aplpy")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "aplpy" version))
       (sha256
        (base32 "0ph9jhv4q4i4z6nkqr6hjw9148kdlnayxsn83qgv5dqn0h3nc9r8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: https://github.com/aplpy/aplpy/issues/492
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check))))
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
    (native-inputs
     (list python-pytest-astropy
           python-pytest-mpl))
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
    (version "3.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "0pwy8p3idz40a1z07d0lvvd0mwwya8g52wrp72frgpagjkvj3ska"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-p" "no:legacypath")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-pypojrect-toml
            (lambda _
              (substitute* "pyproject.toml"
                ;; ImportError: Error importing plugin " no:legacypath": No
                ;; module named ' no:legacypath'
                ((".*:legacypath.*") "")
                ;; TypeError: Configuration.__init__() got an unexpected
                ;; keyword argument 'version_file'
                (("version_file = \"asdf/_version.py\"") "")))))))
    (native-inputs
     (list python-fsspec
           python-packaging
           python-psutil
           ;; 3.3.0+ requries newer version of pytest, see
           ;; <https://github.com/asdf-format/asdf/issues/1804>.
           python-pytest-8
           python-pytest-doctestplus
           python-pytest-remotedata
           python-pytest-xdist
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf-standard
           python-asdf-transform-schemas
           python-attrs ;; for vendorized jsonschema
           python-importlib-metadata
           python-jmespath
           python-lz4
           python-numpy
           python-pyyaml
           python-semantic-version))
    (home-page "https://github.com/asdf-format/asdf")
    (synopsis "Python tools to handle ASDF files")
    (description
     "The Advanced Scientific Data Format (ASDF) is a next-generation
interchange format for scientific data.  This package contains the Python
implementation of the ASDF Standard.")
    (license license:bsd-3)))

(define-public python-asdf-astropy
  (package
    (name "python-asdf-astropy")
    (version "0.6.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf-astropy" version))
       (sha256
        (base32 "1ipjpjiirycj2npicbp39ka7db61vx45j8dm2iis71g5l1rzkblp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")
      #:phases #~(modify-phases %standard-phases
                   (add-before 'check 'set-home-env
                     (lambda _ (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list python-coverage
           python-h5py
           python-pandas
           python-pytest
           python-pytest-astropy
           python-pytest-xdist
           python-scipy
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf
           python-asdf-coordinates-schemas
           python-asdf-transform-schemas
           python-astropy
           python-numpy
           python-packaging))
    (home-page "https://github.com/astropy/asdf-astropy")
    (synopsis "ASDF serialization support for astropy")
    (description
     "This package includes plugins that provide ASDF serialization support for
Astropy objects.")
    (license license:bsd-3)))

(define-public python-astroalign
  (package
    (name "python-astroalign")
    (version "2.5.1")
    (source
     (origin
       ;; There are no tests in the PyPI tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quatrope/astroalign")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kr5cszcxvrdbksy7mvv3ps1h1jzrn4yamfr6x7whkbi6bpqf7xp"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-astropy
           python-ccdproc
           python-pillow
           python-pytest))
    (propagated-inputs
     (list python-bottleneck
           python-numpy
           python-scikit-image
           python-scipy
           python-sep))
    (home-page "https://astroalign.readthedocs.io/")
    (synopsis "Astrometric Alignment of Images")
    (description
     "ASTROALIGN is a python module that will try to align two stellar
astronomical images, especially when there is no WCS information available.")
    (license license:expat)))

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
        "--ignore=astroML/density_estimation/tests/test_hist_binwidth.py")
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
    (propagated-inputs (list python-astropy python-matplotlib python-numpy
                             python-scikit-learn python-scipy))
    (native-inputs (list python-pytest-astropy-header python-pytest-cov
                         python-pytest-doctestplus python-pytest-remotedata))
    (home-page "https://astroml.org")
    (synopsis "Tools for machine learning and data mining in astronomy")
    (description "This package provides tools for machine learning and data
mining in astronomy.")
    (license license:bsd-2)))

(define-public python-extinction
  (package
    (name "python-extinction")
    (version "0.4.6")
    (source
     (origin
       (method git-fetch) ; No tests in PyPI
       (uri (git-reference
             (url "https://github.com/kbarbary/extinction")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1adxq926jd469mxm6llvsljgf2jqb06905h61i9qzc7m2yrm4wga"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "test.py")))
    (native-inputs (list python-cython python-pytest))
    (propagated-inputs (list python-numpy))
    (home-page "http://github.com/kbarbary/extinction")
    (synopsis "Fast interstellar dust extinction laws")
    (description
     "This package provides a cython-optimized implementations of empirical dust
exitinction laws found in the literature.")
    (license license:expat)))

(define-public python-fitsio
  (package
    (name "python-fitsio")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsio" version))
       (sha256
        (base32 "0y7n7wh73ha6439djrhwmqbvgpagrdgaasbrikvw2zb5qx3y6zym"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove the bundled cfitsio. When update the package check the
        ;; current bundled version.
        #~(begin
            (delete-file-recursively "cfitsio-4.4.1-20240617")
            (substitute* "MANIFEST.in"
              (("recursive-include cfitsio-4.4.0.*$\n") ""))))))
    (build-system pyproject-build-system)
    (arguments
     (list
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
                       #$(this-package-input "cfitsio") "/lib"))))
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-pytest))
    (inputs
     (list curl cfitsio zlib))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://github.com/esheldon/fitsio")
    (synopsis
     "Python library to read from and write to FITS files")
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
    (arguments
     (list
      #:test-flags
      ;; These tests require internet access
      #~(list "-k" "not test_download_data.py")
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests need this
          (add-before 'check 'set-HOME
            (lambda _ (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-astroml
           python-numpy
           python-scipy
           python-supersmoother))
    (native-inputs (list python-pytest python-nose python-setuptools-scm))
    (home-page "https://github.com/astroml/gatspy")
    (synopsis "General tools for astronomical time series in Python")
    (description
     "This package provides general tools for astronomical time series in
Python.")
    (license license:bsd-2)))

(define-public python-ginga
  (package
    (name "python-ginga")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ginga" version))
       (sha256
        (base32 "0c6vpcz59x889z8ggq6r5ci9cf10m7r9h262r6chx31mzdr010wf"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.cfg"
               ;; packaging>=23.1
               ((">=23.1") ">=21.3"))))
         (add-before 'check 'set-home
           (lambda _
             ;; Relax matplotlib warning: ... because the default path
             ;; (/homeless-shelter/.config/matplotlib) is not a writable
             ;; directory ...
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list opencv
           python-astropy
           python-astroquery
           python-dateutil
           python-exif-read
           python-fitsio
           python-magic
           python-matplotlib
           python-numpy
           python-packaging
           python-photutils
           python-pillow
           python-pyyaml
           python-qtpy
           python-scipy
           python-tomli))
    (native-inputs
     (list python-attrs
           python-docutils
           python-pytest-astropy
           python-pytest-astropy-header
           python-tornado))
    (home-page "https://ejeschke.github.io/ginga/")
    (synopsis "Scientific image viewer and toolkit for FITS files")
    (description "Ginga is a toolkit designed for building viewers for
scientific image data in Python, visualizing 2D pixel data in numpy arrays.  It
can view astronomical data such as contained in files based on the
FITS (Flexible Image Transport System) file format.  It is written and is
maintained by software engineers at the National Astronomical Observatory of
Japan (NAOJ), the Space Telescope Science Institute (STScI), and other
contributing entities.

The Ginga toolkit centers around an image display object which supports zooming
and panning, color and intensity mapping, a choice of several automatic cut
levels algorithms and canvases for plotting scalable geometric forms.  In
addition to this widget, a general purpose \"reference\" FITS viewer is
provided, based on a plugin framework.  A fairly complete set of standard plugins
are provided for features that we expect from a modern FITS viewer: panning and
zooming windows, star catalog access, cuts, star pick/FWHM, thumbnails, etc.")
(license license:bsd-3)))

(define-public ginga-qt5
  (package/inherit python-ginga
    (name "ginga-qt5")
    (inputs
     (modify-inputs (package-inputs python-ginga)
       (prepend python-pyqt)))
    (synopsis "Qt5 image viewer build based on python-ginga library")))

(define-public python-glue-core
  (package
    (name "python-glue-core")
    (version "1.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "glue_core" version))
       (sha256
        (base32 "1xms896ds70sbym1cr5lrbjl4i8vixy3mcc6qsg1b3ppvky76vmn"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list ;; python-astrodendro ; optional, not packed
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
           ;; python-pyavm ; optional, not packed
           python-scikit-image
           python-scipy
           python-setuptools
           python-shapely
           python-spectral-cube
           python-xlrd))
    (native-inputs
     (list python-h5py
           python-objgraph
           python-pytest
           python-pytest-cov
           python-pytest-mpl
           python-setuptools
           python-setuptools-scm))
    (home-page "http://glueviz.org")
    (synopsis "Multidimensional data visualization project")
    (description
     "Glue is a python project to link visualizations of scientific datasets
across many files.")
    (license license:bsd-3)))

(define-public python-sncosmo
  (package
    (name "python-sncosmo")
    (version "2.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sncosmo" version))
       (sha256
        (base32 "0nmhrvaw22zxpmykl70a91mc88pxmw0x5fdxjiz3hdzkdbqrg0x9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Disable tests requireing remote access to download test data.
      #~(list
         "-k" (string-append "not test_megacampsf_bandpass"
                             " and not test_builtins_remote_aa"
                             " and not test_builtins_remote_nm"
                             " and not test_builtins_remote_um"
                             " and not test_builtins_remote_wfc3"
                             " and not test_builtins_megacampsf"
                             " and not test_builtins_timeseries_ascii"
                             " and not test_builtins_timeseries_fits"
                             " and not test_builtins_salt2model"
                             " and not test_builtins_salt3model"
                             " and not test_builtins_2011fe"
                             " and not test_builtins_mlcs2k2"
                             " and not test_builtins_snemo"
                             " and not test_builtins_sugar"
                             " and not test_builtins_magsys_fits"
                             " and not test_builtins_magsys_csp"
                             " and not test_builtins_magsys_ab_b12"
                             " and not test_builtins_magsys_jla"
                             " and not test_csp_magsystem"
                             " and not test_compositemagsystem_band_error"
                             " and not test_G10"
                             " and not test_C11"
                             " and not test_salt2source_timeseries_vs_snfit"
                             " and not test_salt2source_rcov_vs_snfit"
                             " and not test_bandflux"
                             " and not test_bandflux_multi"
                             " and not test_bandflux_zpsys"
                             " and not test_bandfluxcov"
                             " and not test_bandmag"
                             " and not test_sugarsource"))
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.cfg"
               ;; pyyaml>=6.0.1
               (("6.0.1") "6.0"))))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-astropy
           python-extinction
           python-looseversion
           python-numpy
           python-pyyaml
           python-scipy))
    (native-inputs
     (list ;; python-iminuit ; not packed, optional
           python-cython
           python-pytest
           python-pytest-astropy
           python-pytest-cov))
    (home-page "https://sncosmo.readthedocs.org")
    (synopsis "Package for supernova cosmology based on astropy")
    (description
     "SNCosmo is a Python library for supernova cosmology analysis.  It aims
to make such analysis both as flexible and clear as possible.")
    (license license:bsd-3)))

(define-public wcslib
  (package
    (name "wcslib")
    (version "8.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.atnf.csiro.au/people/mcalabre/WCS/"
                           "wcslib-" version ".tar.bz2"))
       (sha256
        (base32 "0cvqppjf7gk0f3rs9cc46h5fffv2l8ylrb234r9fbx0px0525632"))
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
    (home-page "https://www.atnf.csiro.au/people/mcalabre/WCS")
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
       (uri (string-append "https://www.atnf.csiro.au/people/mcalabre/WCS/"
                           "wcslib-" version ".tar.bz2"))
       (sha256
        (base32 "1m3bx6gh5w3c7vvsqcki0x20mg8lilg13m0i8nh7za89w58dxy4w"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "C/flexed")))))
    (properties '((hidden? . #t)))))

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
      #:configure-flags #~(list "CPPFLAGS=-fcommon")
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
    (version "0.22")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.lz"))
       (sha256
        (base32
         "15rljx1mx9dyvni17qpj7y9gv086cvmjf9f5j34m1pbiyn989fqz"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
     (list cfitsio
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

(define-public phd2
  (package
    (name "phd2")
    (version "2.6.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenPHDGuiding/phd2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0shr50ngi7dliijy8fxrd1c4hzkm4vp4m0a4m0d9gvrx56vzyx0s"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            (define (delete-all-but directory . preserve)
              (define (directory? x)
                (and=> (stat x #f)
                       (compose (cut eq? 'directory <>) stat:type)))
              (with-directory-excursion directory
                (let* ((pred
                        (negate (cut member <> (append '("." "..") preserve))))
                       (items (scandir "." pred)))
                  (for-each (lambda (item)
                              (if (directory? item)
                                  (delete-file-recursively item)
                                  (delete-file item)))
                            items))))
            (delete-all-but "thirdparty" "thirdparty.cmake")))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DOPENSOURCE_ONLY=yes"
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
           wxwidgets
           zlib))
    (home-page "https://openphdguiding.org")
    (synopsis "Teleskope guiding software")
    (description
     "PHD2 is the enhanced, second generation version of the PHD guiding software
from Stark Labs.")
    (license license:bsd-3)))

(define-public sextractor
  (package
    (name "sextractor")
    (version "2.28.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/sextractor")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15v7brjiraj2rdyxiidcgb58b3dqzdd363j31cjrfqhd1wc8ii5j"))))
    (build-system gnu-build-system)
    ;; NOTE: (Sharlatan-20210124T103117+0000): Building with `atlas' is failing
    ;; due to missing shared library which required on configure phase. Switch
    ;; build to use `openblas' instead. It requires FFTW with single precision
    ;; `fftwf'.
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

(define-public siril
  (package
    (name "siril")
    (version "1.2.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/free-astro/siril")
                    (commit version)))
              (sha256
               (base32
                "0gkd8w2bpwq4ibl3vawx008yrm5k6zlj77lp98fflffcf7cj8hr5"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs (list cmake git glib libconfig pkg-config))
    (inputs (list cfitsio
                  exiv2
                  ffms2
                  fftwf
                  gsl
                  gtk+
                  json-glib
                  libheif
                  libraw
                  librtprocess
                  opencv))
    (home-page "https://siril.org/")
    (synopsis "Image processing software for amateur astronomy")
    (description
     "This package provides an astronomical image processing tool - SIRIL.  It is
specially tailored for noise reduction and improving the signal/noise ratio of
an image from multiple captures, as required in astronomy.  SIRIL can align
automatically or manually, stack and enhance pictures from various file formats,
even image sequence files (films and SER files).  It works well with limited
system resources, like in embedded platforms, but is also very fast when run on
more powerful computers and provides conversion to FITS from a large number of
image formats.")
    (license license:gpl3+)))

(define-public splash
  (package
    (name "splash")
    (version "3.10.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/danieljprice/splash")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "077s9if7fmccvhsbp0dhvyqcil46vpbgdm1y6qn6h34r8lfqj9z6"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     ;; FIXME: Tests failed
     ;; Issue submited upstream https://github.com/danieljprice/splash/issues/67
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
    (native-inputs (list gfortran pkg-config perl python-wrapper))
    (inputs (list cairo cfitsio giza))
    (home-page "https://users.monash.edu.au/~dprice/splash/")
    (synopsis
     "Astrophysical visualisation tool for smoothed particle hydrodynamics")
    (description
     "SPLASH is visualisation tool for Smoothed Particle Hydrodynamics (SPH)
simulations in one, two and three dimensions, developed mainly for
astrophysics.  It uses a command-line menu but data can be manipulated
interactively in the plotting window.")
    (license license:gpl2+)))

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
    (version "24.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Stellarium/stellarium")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01iq1cxbhr3xknzpk4spflxj3msf9wk19b4lqlifi1pwaz18pb5n"))))
    (build-system cmake-build-system)
    ;; TODO: Complete documentation build and split into dedicated outputs.
    (arguments
     (list
      ;; FIXME: Tests keep failing on 100% when preparing test-suit for INDI.
      #:tests? #f
      #:test-target "test"
      #:configure-flags
      #~(list "-DENABLE_GPS=1"
              ;; TODO: Enable when all of the dependencies are availalbe for Qt6.
              "-DENABLE_QT6=0"
              "-DENABLE_TESTING=0"
              (string-append "-DCMAKE_CXX_FLAGS=-isystem "
                             #$(this-package-input "qtserialport") "/include/qt5"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-offscreen-display
            (lambda _
              (setenv "QT_QPA_PLATFORM" "offscreen")
              (setenv "HOME" "/tmp"))))))
    (inputs
     (list calcmysky-qt5
           gpsd
           indi
           libnova
           nlopt
           openssl
           qtbase-5
           qtcharts
           qtlocation-5
           qtmultimedia-5
           qtpositioning
           qtscript
           qtserialport-5
           qttranslations
           qtwebengine-5
           qxlsx-qt5
           zlib))
    (native-inputs
     (list doxygen
           gettext-minimal
           graphviz
           mesa
           perl
           python-wrapper
           qttools-5))
    (home-page "https://stellarium.org/")
    (synopsis "3D sky viewer")
    (description
     "Stellarium is a planetarium.  It shows a realistic sky in
3D, just like what you see with the naked eye, binoculars, or a telescope.  It
can be used to control telescopes over a serial port for tracking celestial
objects.")
    (license license:gpl2+)))

(define-public python-astropy
  (package
    (name "python-astropy")
    (version "6.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy" version))
       (sha256
        (base32 "0f31wgjg22phcx8fw7p612qp3v2ak7nziisnsnwaqc0wj5fz9ip5"))
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
      #:test-flags
      #~(list "--pyargs" "astropy"
              "-n" "auto"
              "-k" (string-append
                    ;; Skip tests that need remote data.
                    "not remote_data"
                    ;; ValueError: The truth value of an array with more than
                    ;; one element is ambiguous. Use a.any() or a.all()
                    " and not test_table_comp[t16-t26]"
                    ;; E Unreliable test timings! <...>
                    " and not test_datetime_timedelta_roundtrip"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preparations
            (lambda _
              ;; Use our own libraries in place of bundles.
              (setenv "ASTROPY_USE_SYSTEM_ALL" "1")
              ;; Relax xfail tests.
              (substitute* "pyproject.toml"
                (("xfail_strict = true") "xfail_strict = false"))
              ;; Replace reference to external ply.
              (substitute* "astropy/utils/parsing.py"
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
                (make-file-writable "astropy/_compiler.c")
                ;; Extensions have to be rebuilt before running the tests.
                (invoke "python" "setup.py" "build_ext" "--inplace"
                        "-j" (number->string (parallel-job-count)))
                ;; Step out of the source directory to avoid interference; we
                ;; want to run the installed code with extensions etc.
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-v" test-flags))))))))
    (native-inputs
     (list pkg-config
           python-colorlog
           python-coverage
           python-cython-3
           python-extension-helpers
           python-h5py
           python-ipython
           python-jplephem
           python-objgraph
           python-pandas
           python-pyarrow
           python-pytest
           python-pytest-astropy
           python-pytest-astropy-header
           python-pytest-mpl
           python-pytest-xdist
           python-scikit-image
           python-setuptools-scm
           python-sgp4
           python-skyfield
           python-threadpoolctl
           python-timezonefinder))
    (inputs
     (list expat wcslib))
    (propagated-inputs
     (list python-astropy-iers-data
           python-configobj
           python-matplotlib
           python-numpy
           python-packaging
           python-ply
           python-pyerfa
           python-pyyaml
           python-scipy))
    (home-page "https://www.astropy.org/")
    (synopsis "Core package for Astronomy in Python")
    (description
     "Astropy is a single core package for Astronomy in Python.  It contains
much of the core functionality and some common tools needed for performing
astronomy and astrophysics.")
    (license license:bsd-3)))

(define-public python-astropy-healpix
  (package
    (name "python-astropy-healpix")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy_healpix" version))
       (sha256
        (base32 "0ilpwwvdnd4nchivwnbiy1hl07hd2mdg4wb90r2p05kvr5z2lpfy"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; This file is opened in both install and check phases.
          (add-before 'install 'writable-compiler
            (lambda _ (make-file-writable "astropy_healpix/_compiler.c")))
          (add-before 'check 'prepare-test-environment
            (lambda _
              ;; Extensions have to be rebuilt before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace")
              (make-file-writable "astropy_healpix/_compiler.c"))))))
    (native-inputs
     (list python-extension-helpers
           python-hypothesis
           python-pytest-astropy
           python-setuptools-scm))
    (propagated-inputs
     (list python-astropy python-numpy))
    (home-page "https://github.com/astropy/astropy-healpix")
    (synopsis "HEALPix for Astropy")
    (description "This package provides HEALPix to the Astropy project.")
    (license license:bsd-3)))

(define-public python-astropy-iers-data
  (package
    (name "python-astropy-iers-data")
    ;; In case of changing the source method git-fetch, consider to check the
    ;; tag as it's not following the PyPI version, see
    ;; <https://github.com/astropy/astropy-iers-data/issues/17>.
    (version "0.2024.7.15.0.31.42")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy_iers_data" version))
       (sha256
        (base32 "18wpqjyjqpxhp1x9rh1dxqmmif2amsbwwbw0zwfjh0dyrbb28c3h"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Dependencies cycle wit python-astropy, see
      ;; <https://github.com/astropy/astropy-iers-data/issues/21>.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env-version
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-hypothesis
           python-pytest
           python-pytest-remotedata
           python-setuptools-scm))
    (home-page "https://docs.astropy.org/en/latest/utils/iers.html")
    (synopsis "IERS Earth Rotation and Leap Second tables for the astropy core package")
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

(define-public python-astroplan
  (package
    (name "python-astroplan")
    (version "0.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroplan" version))
       (sha256
        (base32 "0nl7yi0h5nn4id1jifd8hpwzqdarq9z2iq56x0j2kmj3472cjw0n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "astroplan/tests"
              "-k" (string-append
                    ;; Test requiring newer python-pytz
                    "not test_timezone"
                    ;; Disable tests requiring remote data.
                    " and not test_FixedTarget_from_name"
                    " and not test_altitude_constraint"
                    " and not test_at_night_basic"
                    " and not test_caches_shapes"
                    " and not test_compare_airmass_constraint_and_observer"
                    " and not test_compare_altitude_constraint_and_observer"
                    " and not test_docs_example"
                    " and not test_eclipses"
                    " and not test_eq_observer"
                    " and not test_event_observable"
                    " and not test_galactic_plane_separation"
                    " and not test_get_skycoord"
                    " and not test_hash_observer"
                    " and not test_is_night"
                    " and not test_local_time_constraint_hawaii_tz"
                    " and not test_local_time_constraint_utc"
                    " and not test_moon_illumination"
                    " and not test_moon_separation"
                    " and not test_observability_table"
                    " and not test_observer_lon_lat_el"
                    " and not test_regression_airmass_141"
                    " and not test_regression_shapes"
                    " and not test_sun_separation"
                    " and not test_tonight")
              "--ignore=astroplan/tests/test_scheduling.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-astropy
           python-astroquery
           python-matplotlib
           python-numpy
           python-pytz
           python-six))
    (native-inputs
     (list python-pytest-astropy
           python-pytest-mpl
           python-setuptools-scm))
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

(define-public python-astroquery
  (package
    (name "python-astroquery")
    (version "0.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroquery" version))
       (sha256
        (base32 "1jbyfhqk74wsdjxzqi0hcrgc7ha4q8cyjx96nv6w9bjg1b5vlzq4"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--pyargs" "astroquery"
              "-m" "not remote_data"
              ;; Some tests failed with parallel run, see
              ;; <https://github.com/astropy/astroquery/issues/2968>.
              ;; "-n" "auto"
              "-k" (string-append
                    ;; Failed: DID NOT RAISE <class
                    ;; 'astropy.utils.exceptions.AstropyDeprecationWarning'>
                    "not test_raises_deprecation_warning"))
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
    (propagated-inputs
     (list python-astropy
           python-beautifulsoup4
           python-html5lib
           python-keyring
           python-numpy
           python-pyvo
           python-requests))
    (native-inputs
     (list python-astropy-healpix
           python-matplotlib
           ;; python-mocpy : Not packed yet, optional
           python-pytest-astropy
           python-pytest-dependency
           python-regions))
    (home-page "https://astroquery.readthedocs.io/en/latest/index.html")
    (synopsis "Access online astronomical data resources")
    (description "Astroquery is a package that contains a collection of tools
to access online Astronomical data.  Each web service has its own sub-package.")
    (license license:bsd-3)))

(define-public python-astroscrappy
  (package
    (name "python-astroscrappy")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroscrappy" version))
       (sha256
        (base32 "0r2alg8imr201ykjsvr6y43bzw8mwbc4ddprn8f6qfw9k4hsx8ff"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "--pyargs" "astroscrappy")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preparations
            (lambda _ (setenv "HOME" "/tmp")))
          (add-before 'install 'writable-compiler
            (lambda _ (make-file-writable "astroscrappy/_compiler.c")))
          (add-before 'check 'tests-preparation
            (lambda _
              (make-file-writable "astroscrappy/_compiler.c")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-cython-3
           python-extension-helpers
           python-pytest-astropy
           python-scipy
           python-setuptools-scm))
    (propagated-inputs (list python-astropy python-numpy))
    (home-page "https://github.com/astropy/astroscrappy")
    (synopsis "Speedy Cosmic Ray Annihilation Package in Python")
    (description
     "Astro-SCRAPPY is designed to detect cosmic rays in images (numpy
arrays), based on Pieter van Dokkum's L.A.Cosmic algorithm.  Much of this was
originally adapted from cosmics.py written by Malte Tewes.  This is designed to
be as fast as possible so some of the readability has been sacrificed,
specifically in the C code.")
    (license license:bsd-3)))

(define-public python-bayesicfitting
  (package
    (name "python-bayesicfitting")
    (version "3.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dokester/BayesicFitting")
             ;; Tag style has been changed, see
             ;; <https://github.com/dokester/BayesicFitting/issues/23>.
             (commit (string-append "v." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0brqvy4r75vh93lj4jwv8wcrc96ka1v44f5ckjvr65y30plnfwg2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-m" "unittest" "discover" "test")
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (apply invoke "python" test-flags)))))))
    (propagated-inputs
     (list python-astropy
           python-future
           python-matplotlib
           python-numpy
           python-scipy))
    (home-page "https://www.bayesicfitting.nl")
    (synopsis "Python Toolbox for Astronimical Bayesian fitting")
    (description
     "The BayesicFitting package is a python version of the the fitter classes
in @acronym{HCSS, Herschel Common Science System}.  HCSS was the all
encompassing software system for the operations and analysis of the ESA satelite
Herschel.")
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
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (native-inputs
     (list python-pytest python-pytest-cov python-pytest-openfiles))
    (propagated-inputs
     (list python-astropy python-click python-dask python-numpy))
    (home-page "https://casa-formats-io.readthedocs.io/")
    (synopsis "Dask-based reader for CASA data")
    (description
     "The @code{casa-formats-io} package is a small package which implements
functionality to read data stored in @acronym{CASA, Common Astronomy Software
Applications} formats (such as @file{.image} datasets).  This implementation
is independent of and does not use @code{casacore}.")
    (license license:lgpl2.0)))

(define-public python-ccdproc
  (package
    (name "python-ccdproc")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ccdproc" version))
       (sha256
        (base32 "14faivm9nihpdwzg0jx1c9zr7jk22gjfjw78raq6h63ypl10i6yx"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-memory-profiler
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
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)               ; no tests in PyPI release
       (uri (git-reference
             (url "https://github.com/MAVENSDC/cdflib")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zmz9wjhlq43lqy5k4fld9cj5k39s1hkkaligrn3kpf9hcbd79qn"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'set-env-version
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION"
                           #$version)))
               (add-before 'check 'set-home-env
                 (lambda _
                   (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-numpy))
    (native-inputs
     (list python-astropy
           python-hypothesis
           python-pytest
           python-pytest-cov
           python-pytest-remotedata
           python-setuptools-scm
           python-xarray))
    (home-page "https://github.com/MAVENSDC/cdflib")
    (synopsis "Python library to deal with NASA's CDF astronmical data format")
    (description "This package provides a Python @acronym{CDF, Computable
Document Format} reader toolkit.
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

(define-public python-cmyt
  (package
    (name "python-cmyt")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cmyt" version))
       (sha256
        (base32 "1zabmckr1z637pfqqvlkj0asfqqvx2x92163dby8x0c8yiqgdvjb"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-colorspacious python-pytest python-pytest-mpl))
    (propagated-inputs
     (list python-matplotlib python-numpy))
    (home-page "https://yt-project.org/")
    (synopsis "Matplotlib colormaps from the yt project")
    (description
     "This package provides a range of colormaps designed for scientific
use with Matplotlib.  It includes perceptually uniform sequential colormaps such
as @code{abre}, @code{dusk}, @code{kepl}, and @code{octarine}, as well as
monochromatic sequential colormaps like @code{blue}, @code{green}, and
@code{red}, and others (@code{algae}, @code{pastel}, and @code{xray}).")
    (license license:bsd-3)))

(define-public python-crds
  (package
    (name "python-crds")
    (version "11.17.26")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "crds" version))
       (sha256
        (base32 "1qw1j3gz8l2z4ra33hl4zblc90kvwjf2ajb343n0xmf7lzs1gwc7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests require Internet access to https://hst-crds.stsci.edu and
      ;; additional test data. See:
      ;; https://github.com/spacetelescope/crds/blob/master/setup_test_cache
      #:tests? #f))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-boto3
           python-filelock
           python-numpy
           python-parsley
           python-pysynphot
           python-roman-datamodels
           python-stsynphot
           python-requests))
    (native-inputs
     (list python-setuptools-scm))
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

(define-public python-czml3
  (package
    (name "python-czml3")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/poliastro/czml3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vm9ajpnwxncvsl1ix3jarqyi14b0703g12wsr8b0agkrjlpmvmx"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-attrs
           python-dateutil
           python-w3lib))
    (native-inputs
     (list python-astropy
           python-pytest
           python-pytest-cov
           python-pytest-mypy))
    (home-page "https://github.com/poliastro/czml3")
    (synopsis "Python library to write CZML")
    (description
     "CZML3 is a Python library to write CZML, a JSON format for describing
a time-dynamic graphical scene, primarily for display in a web browser running
Cesium.")
    (license license:expat)))

(define-public python-drms
  (package
    (name "python-drms")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "drms" version))
       (sha256
        (base32 "01q00zwpbhik9lgbc42z6q791ybxc41rxgrj04fpcfkl3hcl0nyr"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-astropy
           python-pytest-astropy
           python-pytest
           python-setuptools-scm))
    (propagated-inputs (list python-numpy python-pandas))
    (home-page "https://sunpy.org")
    (synopsis "Access astronomical HMI, AIA and MDI data with Python")
    (description
     "DRMS module provides an easy-to-use interface for accessing HMI, AIA and
MDI data with Python.  It uses the publicly accessible
JSOC (@url{http://jsoc.stanford.edu/}) DRMS server by default, but can also be
used with local NetDRMS sites.")
    (license license:bsd-2)))

(define-public python-drizzle
  (package
    (name "python-drizzle")
    (version "1.15.2")
    (source
     (origin
       (method git-fetch) ;PyPi doesn't have the test data sets
       (uri (git-reference
             (url "https://github.com/spacetelescope/drizzle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fp6gcvp6nz4a2mmy9vjn5wwywldhkg8bjjgb4ldn0vpv9k4nv8q"))))
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
    (propagated-inputs
     (list python-astropy python-numpy))
    (native-inputs
     (list python-pytest python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/drizzle")
    (synopsis
     "Astronomical tool for combining dithered images into a single image")
    (description
     "The drizzle library is a Python package for combining dithered images
into a single image.  This library is derived from code used in DrizzlePac.
Like DrizzlePac, most of the code is implemented in the C language.  The
biggest change from DrizzlePac is that this code passes an array that maps the
input to output image into the C code, while the DrizzlePac code computes the
mapping by using a Python callback.  Switching to using an array allowed the
code to be greatly simplified.")
    (license license:bsd-3)))

(define-public python-dust-extinction
  (package
    (name "python-dust-extinction")
    (version "1.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "dust_extinction" version))
       (sha256
        (base32 "00vyglyq2209y0mp0d5vipqrfjxp4qb8x8nx8ic2x4s19xq8ds88"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-astropy python-scipy))
    (native-inputs
     (list python-pytest-astropy))
    (home-page "http://dust-extinction.readthedocs.io/")
    (synopsis "Interstellar Dust Extinction Models")
    (description
     "This package provides astronomical interstellar dust extinction curves
implemented using the astropy.modeling framework.")
    (license license:bsd-3)))

(define-public python-ephem
  (package
    (name "python-ephem")
    (version "4.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ephem" version))
       (sha256
        (base32 "0ainqbnvw320pc61q5b6ad6f2mhn1pvrlnq489cwfx0m82mahr0c"))))
    (build-system pyproject-build-system)
    (native-inputs (list tzdata))
    (home-page "https://rhodesmill.org/pyephem/")
    (synopsis "Compute positions of the planets and stars")
    (description
     "PyEphem provides an @code{ephem} Python package for performing
high-precision astronomy computations.

The name ephem is short for the word ephemeris, which is the traditional term
for a table giving the position of a planet, asteroid, or comet for a series
of dates.")
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
    (propagated-inputs
     (list python-pydantic-2 python-pydantic-settings python-requests))
    (native-inputs
     (list python-pytest python-pytest-astropy))
    (home-page "https://helioviewer.org/")
    (synopsis "Helioviewer Python API Wrapper")
    (description "@code{hvpy} is a Python API wrapper around the formal
@url{Helioviewer API, https://api.helioviewer.org/docs/v2/}.")
    (license license:bsd-2)))

(define-public python-kanon
  (package
    (name "python-kanon")
    (version "0.6.6")
    (source
     (origin
       (method git-fetch)               ; no release in PyPI
       (uri (git-reference
             (url "https://github.com/ALFA-project-erc/kanon")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sg9yrsas5xmhbw6mhfyxsxh9i060af6v02whr9fqgv687fiyrhc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list ;"--numprocesses" "auto"
              ;; XXX: This tests failing a lot.
              "-k" (string-append "not test_attribute_forwardin"
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
     (list git-minimal
           python-poetry-core
           python-poetry-dynamic-versioning
           python-pytest-astropy
           python-pytest-xdist
           python-requests-mock))
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-pandas-1
           python-requests
           python-scipy
           ;; Optional
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

(define-public python-mpl-animators
  (package
    (name "python-mpl-animators")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpl_animators" version))
       (sha256
        (base32 "078dshs383ny182dac0spg7z0iilcwa0fnwv1vizsr6p1d3ar98b"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-pytest-mpl
           python-setuptools-scm))
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
    (version "0.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "naima" version))
       (sha256
        (base32 "1lng2prl2kzzpgrkj11hl53cvqdh0gpk8cdqkvcg08k3bivzk8q8"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-astropy
           python-corner
           python-emcee
           python-h5py
           python-matplotlib
           python-pyyaml
           python-scipy))
    (native-inputs
     (list python-pytest python-setuptools-scm))
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
    (version "2.2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "ndcube" version))
       (sha256
        (base32 "0d82xldinvjw4csql4w3k44ibprbz0b0g5ixq9a5f6c7zbvfc24l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Break cycle: python-ndcube -> python-specutils -> python-ndcube, see
      ;; <https://github.com/sunpy/ndcube/issues/733>.
      #:test-flags #~(list "-k" "not test_rebin_specutils")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'break-cycle
            (lambda _
              (substitute* "ndcube/tests/test_ndcube.py"
                (("from specutils import Spectrum1D") ""))))
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-astropy
           python-gwcs
           python-matplotlib
           python-mpl-animators
           python-numpy
           python-reproject))
    (native-inputs
     (list python-dask
           python-pytest
           python-pytest-astropy
           python-pytest-mpl
           python-scipy
           python-setuptools-scm
           python-sunpy))
    (home-page "https://docs.sunpy.org/projects/ndcube/")
    (synopsis "Multi-dimensional contiguous and non-contiguous coordinate aware arrays")
    (description
     "@code{ndcube} is a package for manipulating, inspecting and visualizing
multi-dimensional contiguous and non-contiguous coordinate-aware data arrays.

It combines data, uncertainties, units, metadata, masking, and coordinate
transformations into classes with unified slicing and generic coordinate
transformations and plotting/animation capabilities.  It is designed to handle
data of any number of dimensions and axis types (e.g. spatial, temporal,
spectral, etc.) whose relationship between the array elements and the real world
can be described by @acronym{WCS, World Coordinate System} translations.")
    (license license:bsd-2)))

(define-public python-photutils
  (package
    (name "python-photutils")
    ;; PyPI version for source archive is missing minor 0, See
    ;; <https://github.com/astropy/photutils/issues/1727>
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "photutils" version))
       (sha256
        (base32 "1lhpcxh2adknzlmrddqd712yzpwdlqlw9jn49ajj4kz5z7822dns"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")
      #:phases
      #~(modify-phases %standard-phases
          ;; setup.py was removed in 36c3231ce5b80ad470fa78be2e96df859d2daf41
          ;; for some unknown reason, which caused the package to fail to
          ;; build. It is being recreated based on that commit.
          (add-after 'unpack 'create-setup.py
            (lambda _
              (call-with-output-file "setup.py"
                (lambda (port)
                  (format port "from setuptools import setup
from extension_helpers import get_extensions
setup(ext_modules=get_extensions())")))))
          ;; This file is opened in both install and check phases.
          (add-before 'install 'writable-compiler
            (lambda _ (make-file-writable "photutils/_compiler.c")))
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running
              ;; the tests. If it's not build it fails with error:
              ;;
              ;; ModuleNotFoundError: No module named
              ;; 'photutils.geometry.circular_overlap'
              (make-file-writable "photutils/_compiler.c")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-astropy
           python-bottleneck
           python-gwcs
           python-matplotlib
           python-numpy
           python-rasterio
           python-scikit-image
           python-scikit-learn
           python-scipy
           python-shapely
           python-tqdm))
    (native-inputs
     (list python-cython-3
           python-extension-helpers
           python-pytest-astropy
           python-pytest-xdist
           python-setuptools-scm))
    (home-page "https://github.com/astropy/photutils")
    (synopsis "Source detection and photometry")
    (description "Photutils is an Astropy package for detection and photometry
of astronomical sources.")
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
              "-k" (string-append "not test_czml_add_trajectory"
                                  " and not test_czml_custom_packet"
                                  " and not test_czml_ground_station"
                                  " and not test_czml_groundtrack"
                                  " and not test_czml_preamble"
                                  ;; This fails with "ufunc 'isfinite' not
                                  ;; supported for the input types"
                                  " and not test_porkchop_plotting"))
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
     (list python-astropy
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
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "poppy" version))
              (sha256
               (base32
                "0s8rb61q8dz66s8d3qg44kb6bb5gi40zl41ik9wyccgb4kyf3brp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")))
    (propagated-inputs
     ;; XXX: With python-synphot (marked as optional) package added to the list
     ;; it tries to download from remote host during tests and fails. Overall
     ;; tests take up to 5-8min to pass.
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scipy))
    (native-inputs
     (list python-docutils
           python-h5py
           python-pandas
           python-pytest
           python-pytest-astropy
           python-pytest-xdist
           python-scikit-image
           python-setuptools-scm
           python-sphinx))
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

(define-public python-pyavm
  (package
    (name "python-pyavm")
    (version "0.9.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "PyAVM" version))
       (sha256
        (base32 "0vgjqvddq4a5lnmg8msm7fwqs3r6fc748xzvnhyvc387h0z8pdxk"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-astropy python-numpy))
    (native-inputs
     (list python-pillow python-pytest python-setuptools-scm))
    (home-page "https://astrofrog.github.io/pyavm/")
    (synopsis "Simple pure-python AVM meta-data handling")
    (description
     "PyAVM is a module to represent, read, and write metadata following the
@acronym{AVM, Astronomy Visualization Metadata} standard provided by
@url{https://www.virtualastronomy.org/avm_metadata.php, vamp} project.")
    (license license:expat)))

(define-public python-pyvo
  (package
    (name "python-pyvo")
    (version "1.5.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvo" version))
       (sha256
        (base32 "0wcg3jhfwjd9gqs74mw63sgi1yhmgljx1bwk3mxn0p6fv924r8mq"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest-astropy python-requests-mock python-setuptools-scm))
    (propagated-inputs
     (list python-astropy python-pillow python-requests))
    (home-page "https://github.com/astropy/pyvo")
    (synopsis "Access Virtual Observatory data and services")
    (description
     "PyVO is a package providing access to remote data and services of the
Virtual observatory (VO) using Python.")
    (license license:bsd-3)))

(define-public python-regions
  (package
    (name "python-regions")
    (version "0.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "regions" version))
       (sha256
        (base32 "0kvfdzqry3vcvphd7ldmppbgn3ab97hbfzwxfrlxls92yi41h3i6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")
      #:phases
      #~(modify-phases %standard-phases
          ;; setup.py was removed in 84c80a280431adda00641cda5264c7de18b43b2f
          ;; for some unknown reason, which caused the package to fail to
          ;; build. It is being recreated based on that commit.
          (add-after 'unpack 'create-setup.py
            (lambda _
              (call-with-output-file "setup.py"
                (lambda (port)
                  (format port "from setuptools import setup
from extension_helpers import get_extensions
setup(ext_modules=get_extensions())")))))
          ;; This file is opened in both install and check phases.
          (add-before 'install 'writable-compiler
            (lambda _ (make-file-writable "regions/_compiler.c")))
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-astropy
           python-h5py
           python-matplotlib
           python-numpy
           python-scipy
           python-shapely))
    (native-inputs
     (list python-cython-3
           python-extension-helpers
           python-pytest-arraydiff
           python-pytest-astropy
           python-pytest-runner
           python-pytest-xdist
           python-setuptools-scm))
    (home-page "https://github.com/astropy/regions")
    (synopsis "Package for region handling")
    (description "Regions is an Astropy package for region handling.")
    (license license:bsd-3)))

(define-public python-regularizepsf
  (package
    (name "python-regularizepsf")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch) ; no tests data in the PyPI tarball
       (uri (git-reference
             (url "https://github.com/punch-mission/regularizepsf")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "078nklks6hjq0hgv6wpbh2x1m2yh6kmzyfgdzd9q82lxpjy1vq0i"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               ;; numpy==1.26.4
               (("==1.26.4") ">=1.23"))))
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-astropy
           python-dill
           python-h5py
           python-lmfit
           python-matplotlib
           python-numpy
           python-scikit-image
           python-scipy
           python-sep))
    (native-inputs
     (list python-cython
           python-pytest
           python-pytest-mpl
           python-pytest-runner))
    (home-page "https://github.com/punch-mission/regularizepsf")
    (synopsis "Point spread function modeling and regularization")
    (description
     "This package inplements functionality of @acronym{Point Spread Function,
PSF} describing how the optical system spreads light from sources.")
    (license license:expat)))

(define-public python-reproject
  (package
    (name "python-reproject")
    (version "0.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reproject" version))
       (sha256
        (base32 "1xk809h2j3zj37l5lx4l87zanf9zxbxajcrff2b2f2i0jpnmxqv3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--arraydiff"
              "--arraydiff-default-format=fits"
              "--numprocesses" "auto"
              "--pyargs" "reproject")
      #:phases
      #~(modify-phases %standard-phases
          ;; setup.py was removed in a659a260bdd7635cddc8f33c4ea04a3b6d8c1f84
          ;; for some unknown reason, which caused the package to fail to
          ;; build. It is being recreated based on that commit.
          ;; TODO: Check how to implement it in python-build-system.
          (add-after 'unpack 'create-setup.py
            (lambda _
              (call-with-output-file "setup.py"
                (lambda (port)
                  (format port "from setuptools import setup
from extension_helpers import get_extensions
setup(ext_modules=get_extensions())")))))
          (add-before 'install 'writable-compiler
            (lambda _
              (make-file-writable "reproject/_compiler.c")))
          (add-before 'check 'writable-compiler
            (lambda _
              (make-file-writable "reproject/_compiler.c")))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-astropy-healpix
           python-click
           python-cloudpickle
           python-dask
           python-fsspec
           python-gwcs
           python-numpy
           python-pyvo
           python-scipy
           python-shapely
           python-zarr))
    (native-inputs
     (list python-cython-3
           python-extension-helpers
           python-pytest
           python-pytest-astropy
           python-pytest-xdist
           ;; python-sunpy ; circular dependencies, test optional
           python-setuptools-scm))
    (home-page "https://reproject.readthedocs.io")
    (synopsis "Astronomical image reprojection in Python")
    (description
     "This package provides a functionality to reproject astronomical images using
various techniques via a uniform interface, where reprojection is the
re-gridding of images from one world coordinate system to another e.g.
changing the pixel resolution, orientation, coordinate system.")
    (license license:bsd-3)))

(define-public python-sgp4
  (package
    (name "python-sgp4")
    (version "2.23")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sgp4" version))
       (sha256
        (base32 "0aalbmldks6ykgkcxwkvnp04q0avhv903m5zwvg8i7zvl99xrbfq"))))
    (build-system pyproject-build-system)
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

(define-public python-sunpy
  (package
    (name "python-sunpy")
    (version "5.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunpy" version))
       (sha256
        (base32 "1jdkkcv247chsj08wrxxv0m577ji5cg7mxx5pw7q0ahmnq93xk2p"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         "--numprocesses" "auto"
         "-k" (string-append
               ;; XXX: Failed: DID NOT RAISE <class 'ModuleNotFoundError'>
               ;; It struggles to find python-opencsv package info with
               ;; 'importlib.metadata'
               "not test_main_nonexisting_module"
               " and not test_main_stdlib_module")
         ;; Requries SpicePy not packed in Guix yet.
         "--ignore=sunpy/coordinates/tests/test_spice.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "setup.cfg"
                ;; It's already updated in master branch, but not released yet.
                ;; drms>=0.6.1,<0.7.0
                (("0.7.0") "0.7.2"))))
          (add-before 'install 'writable-compiler
            (lambda _
              (make-file-writable "sunpy/_compiler.c")))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (call-with-output-file "pytest.ini"
                (lambda (port)
                  (format port "[pytest]
python_files = test_*.py"))))))))
    (native-inputs
     (list opencv ; For tests, includes OpenCV-Python
           python-aiohttp
           python-extension-helpers
           python-hvpy
           python-packaging
           python-pytest
           python-pytest-astropy
           python-pytest-doctestplus
           python-pytest-mock
           python-pytest-mpl
           python-pytest-xdist
           python-setuptools-scm))
    (propagated-inputs
     (list parfive
           python-asdf
           python-asdf-astropy
           python-astropy
           python-beautifulsoup4
           python-cdflib
           python-dask
           python-dateutil
           python-drms
           python-glymur
           python-h5netcdf
           python-h5py
           python-hypothesis
           python-jplephem
           python-matplotlib
           python-mpl-animators
           python-numpy
           python-pandas
           python-reproject
           python-scikit-image
           python-scipy
           ;; python-spiceypy ; Not packed yet in Guix, long jorney.
           python-sqlalchemy
           python-tqdm
           python-zeep))
    (home-page "https://sunpy.org")
    (synopsis "Python library for Solar Physics")
    (description
     "SunPy is package for solar physics and is meant to be a free alternative to the
SolarSoft data analysis environment.")
    (license license:bsd-2)))

(define-public python-sunpy-soar
  (package
    (name "python-sunpy-soar")
    (version "1.10")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunpy-soar" version))
       (sha256
        (base32 "0pb7dr06n20hdhlqf8npb4j1qb5034cgwqi3iciqdi1wxyy5pjc6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Disabe tests requireing network access.
      #~(list "-k" (string-append
                    "not test_search"
                    " and not test_search_low_latency"
                    " and not test_insitu_search"
                    " and not test_no_results"
                    " and not test_no_instrument"
                    " and not test_download_path"
                    " and not test_search_soop"
                    " and not test_when_soar_provider_passed"
                    " and not test_when_sdac_provider_passed"
                    " and not test_when_wrong_provider_passed"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-sunpy))
    (native-inputs
     (list python-pytest))
    (home-page "https://docs.sunpy.org/projects/soar")
    (synopsis "Solar Orbiter Archive plugin for SunPy")
    (description
     "This package provides a @code{sunpy} FIDO plugin for accessing data in the
@acronym{Solar Orbiter Archive, SOAR}.")
    (license license:bsd-2)))

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
     (list python-dataclasses python-pytest python-pytz))
    (home-page "https://github.com/sffjunkie/astral")
    (synopsis "Calculations for the position of the sun and moon")
    (description "Astral is a Python module that calculates times for various
positions of the sun: dawn, sunrise, solar noon, sunset, dusk, solar
elevation, solar azimuth, rahukaalam, and the phases of the moon.")
    (license license:asl2.0)))

(define-public python-spectral-cube
  (package
    (name "python-spectral-cube")
    (version "0.6.5")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spectral-cube" version))
       (sha256
        (base32 "0cqxgmk46gv2qa2kls8fph105lgwbwf13hvizh6w85mzgypyp740"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")))
    (propagated-inputs
     (list python-astropy
           ;; XXX: Currently failing in upstream as it's optional silent
           ;; until a new version is released, see:
           ;; <https://github.com/aplpy/aplpy/issues/492>.
           ;;
           ;; python-aplpy
           python-casa-formats-io
           python-dask
           python-distributed
           python-fsspec
           ;; python-glue-core ; Not packed http://glueviz.org/, optional.
           python-joblib
           python-matplotlib
           python-numpy
           python-radio-beam
           python-reproject
           python-scipy
           python-six
           ;; python-yt ; Not packed https://yt-project.org/, optional.
           python-zarr))
    (native-inputs
     (list ;; XXX: Introduce cycle with pvextractor, listed as extra requiremnts
           ;; in [noviz] option.
           ;; python-pvextractor
           python-pytest-astropy
           python-pytest-xdist
           python-regions
           python-setuptools-scm))
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
    (version "1.15.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "specutils" version))
       (sha256
        (base32 "0gx90dn9vmbvd7a53xb7a51jabskrad52g7imgy0ih1jchdls2pj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disabling test requiring access to download
      ;; <https://datacenter.iers.org/data/9/finals2000A.all>.
      ;; XXX: Check if test data may be packed as standalone package.
      #:test-flags #~(list "-k" "not test_create_spectral_axis")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list ;; python-stdatamodels ; cycle with python-synphot, optional.
           python-asdf
           python-asdf-astropy
           python-gwcs
           python-ndcube
           python-numpy
           python-scipy))
    (native-inputs
     (list python-matplotlib
           python-pytest-astropy
           python-setuptools-scm
           python-spectral-cube))
    (home-page "https://specutils.readthedocs.io/")
    (synopsis "Package for spectroscopic astronomical data")
    (description
     "@code{specutils} is a Python package for representing, loading, manipulating,
and analyzing astronomical spectroscopic data.  The generic data containers and
accompanying modules provide a toolbox that the astronomical community can use
to build more domain-specific packages.  For more details about the underlying
principles,
see @url{https://github.com/astropy/astropy-APEs/blob/main/APE13.rst, APE13}.")
    (license license:bsd-3)))

(define-public python-spherical-geometry
  (package
    (name "python-spherical-geometry")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spacetelescope/spherical_geometry")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0znzfy3bmnsncvahf8qdav0c9403fn99d1gp25lainhv7kxfk44c"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Disable one failing test
      ;; See https://github.com/spacetelescope/spherical_geometry/issues/252
      #:test-flags #~(list "-k" "not test_overlap")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preparations
            (lambda _
              (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version)
              ;; Use our own libraries in place of bundles.
              (setenv "USE_SYSTEM_QD" "1")))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace")
              (call-with-output-file "pytest.ini"
                (lambda (port)
                  (format port "[pytest]
python_files = test_*.py"))))))))
    (native-inputs
     (list python-pytest
           python-pytest-astropy-header
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

(define-public python-statmorph
  (package
    (name "python-statmorph")
    (version "0.5.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "statmorph" version))
       (sha256
        (base32 "06rbwrqlvdzn9innx05kjjmm4mmacd8kwwsfdf5idbwzby5ny5lw"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-astropy
           python-numpy
           python-photutils
           python-scikit-image
           python-scipy))
    (native-inputs
     (list python-pytest))
    (home-page "https://github.com/vrodgom/statmorph")
    (synopsis "Non-parametric morphological diagnostics of galaxy images")
    (description
     "The package @code{statmorph} implements functionality of calculating
non-parametric morphological diagnostics of galaxy images (e.g., Gini-M_{20}
and CAS statistics), as well as fitting 2D Sérsic profiles.")
    (license license:bsd-3)))

(define-public python-stsci-image
  (package
    (name "python-stsci-image")
    (version "2.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stsci_image" version))
       (sha256
        (base32 "0w7s93jsz61ccbhj7irl28q4jgiwa7y9k8pfj24q8vc9zvs530pj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs (list python-numpy python-scipy))
    (native-inputs (list python-pytest python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/stsci.image")
    (synopsis "Image array manipulation functions")
    (description
     "This package provides a Python module to various @acronym{STScI, Space
Telescope Science Institute} image array manipulation functions.")
    (license license:bsd-3)))

(define-public python-stsci-imagestats
  (package
    (name "python-stsci-imagestats")
    (version "1.8.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "stsci.imagestats" version))
              (sha256
               (base32
                "1nmya85bf2747c9ggya6my5b1slk6g2a7bk16rdv8r5a4ah9hda5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs (list python-numpy))
    (native-inputs (list python-pytest python-setuptools-scm))
    (home-page "https://stsciimagestats.readthedocs.io/en/latest/")
    (synopsis "Compute sigma-clipped statistics on data arrays")
    (description
     "@code{stsci.imagestats} is a package designed to compute various
statistics on image data using sigma-clipping iterations.  It is designed to
replicate core behaviour of the IRAF's
@url{http://stsdas.stsci.edu/cgi-bin/gethelp.cgi?imstatistics, imstatistics
task}.")
    (license license:bsd-3)))

(define-public python-stsci-stimage
  (package
    (name "python-stsci-stimage")
    ;; PyPI version was 0.2.8 but the latest version tag on GiHub was 0.2.7,
    ;; see <https://github.com/spacetelescope/stsci.stimage/issues/38>
    (version "0.2.9")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "stsci_stimage" version))
              (sha256
               (base32
                "18sqmjiyn76hzkmv3g8549vfygi136gnar6pb0s7wb44j7cvc0in"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:build-backend "setuptools.build_meta"
      #:test-flags #~(list "test_c")
      #:phases
      #~(modify-phases %standard-phases
          ;; Test steps are taken from GitHub Actions, see
          ;; <https://github.com/spacetelescope/stsci.stimage/issues/27>
          (add-before 'check 'waf-configure-build
            (lambda _
              (copy-file (string-append
                          #$(this-package-native-input "python-waf") "/bin/waf")
                         "waf")
              (invoke "python" "waf" "configure" "build"))))))
    (propagated-inputs
     (list python-numpy))
    (native-inputs
     (list python-pytest
           python-wheel
           python-setuptools-scm
           python-waf))
    (home-page "https://stscistimage.readthedocs.io/en/latest/")
    (synopsis "STScI image processing")
    (description
     "This package provides an astronomical Python package with image
processing functions: @code{xyxymatch}, @code{geomap}.")
    (license license:bsd-3)))

(define-public python-stcal
  (package
    (name "python-stcal")
    (version "1.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stcal" version))
       (sha256
        (base32 "003ygbfa25awvy2zjfxd1k4f1aklsvd53sdk7qa0w42v6ys2kabs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'silent-check-for-opencv
            (lambda _
              ;; XXX: Can't detect opencv-python version. The input opencv
              ;; might not set the version correctly.
              (substitute* "pyproject.toml"
                ((".*opencv-python-headless.*") ""))))
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list opencv ;Provides OpenCV-Python
           python-asdf
           python-astropy
           python-gwcs
           python-numpy
           python-scipy))
    (native-inputs
     (list python-cython-3
           python-psutil
           python-pytest
           python-pytest-cov
           python-pytest-doctestplus
           python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/stcal")
    (synopsis "STScI tools and algorithms used in calibration pipelines")
    (description "STScI tools and algorithms used in calibration pipelines.")
    (license license:bsd-3)))

(define-public python-stdatamodels
  (package
    (name "python-stdatamodels")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stdatamodels" version))
       (sha256
        (base32 "0a47xf1zv71kv166z6rd9v75bw0jjmg70180af4yi4v4y7gnxvmm"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto"
              ;; Disable tests requiring access to CRDS servers to download
              ;; ~500MiB of data.
              "-k" "not test_crds_selectors_vs_datamodel")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy
           python-astropy
           python-numpy
           python-psutil))
    (native-inputs
     (list python-crds
           python-pytest
           python-pytest-doctestplus
           python-pytest-xdist
           python-scipy
           python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/stdatamodels")
    (synopsis
     "Core support for DataModel classes used in calibration pipelines")
    (description
     "Provides @code{DataModel}, which is the base class for data models
implemented in the @acronym{JWST, James Webb Space Telescope} and
@acronym{Roman, Nancy Grace Roman Space Telescope} calibration software.")
    (license license:bsd-3)))

(define-public python-stpipe
  (package
    (name "python-stpipe")
    (version "0.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stpipe" version))
       (sha256
        (base32 "0iipbz5ydxxxk44q8ab0ylk7jpxjfhag4vgkhvpj67zs4s45sd8a"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Replace reference to external configobj.
           (substitute* (find-files "." "\\.py$")
             (("from astropy.extern import configobj") "import configobj")
             (("from astropy.extern.configobj import validate") "import validate")
             (("from astropy.extern.configobj.configobj import ") "from configobj import ")
             (("from astropy.extern.configobj.validate import ") "from validate import "))))))
    (arguments
     (list
      ;; See https://github.com/spacetelescope/stpipe/issues/114
      #:test-flags #~(list "-k" "not test_roman_datamodel")))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-asdf
           python-astropy
           python-crds
           python-stdatamodels))
    (native-inputs
     (list python-pytest
           python-pytest-doctestplus
           python-pytest-openfiles
           python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/stpipe")
    (synopsis "Framework for calibration pipeline software")
    (description
     "This package provides base classes and command-line tools for
implementing calibration pipeline software.")
    ;; LICENSE Association of Universities for Research in Astronomy (AURA)
    (license license:bsd-3)))

(define-public python-stsynphot
  (package
    (name "python-stsynphot")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "stsynphot" version))
              (sha256
               (base32
                "15m3vy39mc7rh1mrxs8d2bs0bx5vv632m6wg8n15fqjzhadqy7j5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; XXX: Tests fails on missing file, it might need to be downloaded,
      ;; disable them for now.  astropy.utils.exceptions.AstropyUserWarning:
      ;; Failed to load Vega spectrum from
      ;; /grp/redcat/trds/calspec/alpha_lyr_stis_010.fits;
      #:tests? #f))
    (propagated-inputs (list python-astropy
                             python-beautifulsoup4
                             python-matplotlib
                             python-numpy
                             python-scipy
                             python-synphot))
    (native-inputs (list python-pytest
                         python-pytest-astropy
                         python-pytest-astropy-header
                         python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/stsynphot_refactor")
    (synopsis "Synthetic photometry using Astropy for HST and JWST")
    (description
     "This package provides a replacement for IRAF STSDAS SYNPHOT and ASTROLIB
PYSYNPHOT, utilizing Astropy covering instrument specific portions of the old
packages for HST.")
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
    (synopsis "Celestial mechanics, astrometry and astrodynamics library")
    (description "Libnova is a general purpose, double precision, Celestial
Mechanics, Astrometry and Astrodynamics library.")
    (home-page "https://libnova.sourceforge.net/")
    (license (list license:lgpl2.0+
                   license:gpl2+)))) ; examples/transforms.c & lntest/*.c

(define-public libsep
  (package
    (name "libsep")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/kbarbary/sep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sag96am6r1ffh9860yq40js874362v3132ahlm6sq7padczkicf"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                           (string-append "PREFIX=" #$output))
      #:phases #~(modify-phases %standard-phases
                   (replace 'check
                     (lambda* (#:key tests? #:allow-other-keys)
                       (when tests?
                         (chdir "../source")
                         (invoke "make"
                                 (string-append "CC=" #$(cc-for-target))
                                 "test")))))))
    (native-inputs
     (list python-wrapper))
    (home-page "https://github.com/kbarbary/sep")
    (synopsis "Astronomical source extraction and photometry library")
    (description
     "SEP makes the core algorithms of
@url{https://www.astromatic.net/software/sextractor/, sextractor} available as a
library of stand-alone functions and classes.  These operate directly on
in-memory arrays (no FITS files or configuration files).  The code is derived
from the Source Extractor code base (written in C) and aims to produce results
compatible with Source Extractor whenever possible.  SEP consists of a C library
with no dependencies outside the standard library, and a Python module that
wraps the C library in a Pythonic API.  The Python wrapper operates on NumPy
arrays with NumPy as its only dependency.")
    (license (list license:expat license:lgpl3+ license:bsd-3))))

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
imaging: creating a high-quality still image out of a series of many thousands)
low quality ones")
    (license license:gpl3+)))

(define-public libpasastro
  (package
    (name "libpasastro")
    (version "1.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pchev/libpasastro")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1na3gyb3nzb5gdgccs1653j2gnz6w3v1mqzhyhkx3yqw8bs3q5x0"))))
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

(define-public libxisf
  (package
    (name "libxisf")
    (version "0.2.12")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.nouspiro.space/nou/libXISF")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1bvf3x0xdipkg28c75j6jav3b2llbqvfa6lkwiacxxlzmj0226s2"))))
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

(define-public gpredict
  ;; The latest tag, 2.3, has no major difference with 2.2.1 and is dated for
  ;; 2018. Additionally, there is some activity on the master branch.
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

(define-public scamp
  (package
    (name "scamp")
    (version "2.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/scamp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qic52mzw9avf1a1fsr85mlh63b7hq6d4wj2d00zgdllmclj5l9q"))))
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
software that computes astrometric projection parameters from source catalogues
derived from @url{http://fits.gsfc.nasa.gov/,FITS} images.  The computed solution
is expressed according to the
@url{http://www.atnf.csiro.au/people/mcalabre/WCS/index.html,WCS} standard.  The
main features of SCAMP are:

@itemize
@item compatibility with @code{SExtractor} FITS or Multi-Extension FITS
catalogue format in input
@item generation of WCS-compliant and @code{SWarp}-compatible FITS image headers
in output
@item automatic grouping of catalogues on the sky
@item selectable on-line astrometric reference catalogue
@item automatic determination of scale, position angle, flipping and coordinate
shift using fast pattern-matching
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

(define-public imppg
  (package
    (name "imppg")
    (version "0.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreatAttractor/imppg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a6wb1a9adwd01dmy0r03xxp8iz9y7mvh30088ajilhj4lf90vxa"))))
    (build-system cmake-build-system)
    (arguments
     (list ;; No test provided
      #:tests? #f))
    (native-inputs
     (list boost pkg-config))
    (inputs
     (list cfitsio freeimage glew wxwidgets-3.0))
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

(define-public indi-2.0
  (package
    (name "indi")
    (version "2.0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indilib/indi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rsy6ihwl3fnv502pmycx0xry9qn1qfz13kwdifcf2075wqd1lx9"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: fix failing tests on aarch64-system.
     `(#:tests? ,(not (or (%current-target-system) (target-aarch64?)))
       #:configure-flags
       (let ((out (assoc-ref %outputs "out")))
         (list
          "-DINDI_BUILD_UNITTESTS=ON"
          "-DCMAKE_BUILD_TYPE=Release"
          (string-append "-DCMAKE_INSTALL_PREFIX=" out)
          (string-append "-DUDEVRULES_INSTALL_DIR=" out "/lib/udev/rules.d")))
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (with-directory-excursion "test"
                 (invoke "ctest")))))
         (add-before 'install 'set-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib/udev/rules.d"))))))))
    (native-inputs
     (list googletest))
    (inputs
     (list cfitsio
           curl
           fftw
           gsl
           libev
           libjpeg-turbo
           libnova
           libtiff
           libusb
           zlib))
    (home-page "https://www.indilib.org")
    (synopsis "Library for astronimical intrumentation control")
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

(define-public indi-1.9
  (package
    (inherit indi-2.0)
    (version "1.9.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indilib/indi")
             (commit (string-append "v" version))))
       (file-name (git-file-name "indi" version))
       (sha256
        (base32 "1vfcas59nlw8v7n6qhxhcm4isf5wk0crip5rmsallq3bsv3zznfr"))))))

(define-public indi
  ;; Default version of INDI..
  indi-1.9)

(define-public python-jplephem
  (package
    (name "python-jplephem")
    (version "2.22")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jplephem" version))
       (sha256
        (base32 "0b2rgb7pvwnl72pqjryf9c812mmdxr69fwiym7mnz05l2xrcr6hd"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "-m" "unittest" "discover" "-s" "test")))))))
    (inputs
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

(define-public python-jwst
  (package
    (name "python-jwst")
    (version "1.15.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jwst" version))
              (sha256
               (base32
                "1nl5fixakqvjhg9q5biivwaqpi6lzx9w4fq0n6imwccag2gv1va3"))
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
    (arguments
     (list
      ;; XXX: Tests require access to https://jwst-crds-pub.stsci.edu server for
      ;; getting data sets.
      #:tests? #f
      #:phases #~(modify-phases %standard-phases
                   ;; NOTE: (Sharlatan-20230529T113448+0100): opencv-python's
                   ;; version can't be detected, it could the way it's packed in
                   ;; Guix. Review failing sanity check with more efforts,
                   ;; disable for now to make package buildable.
                   (delete 'sanity-check))))
    ;; opencv provides OpenCV-Python which is Listed as install requirement.
    (propagated-inputs (list opencv
                             python-asdf
                             python-asdf-astropy
                             python-astropy
                             python-bayesicfitting
                             python-crds
                             python-drizzle
                             python-gwcs
                             python-importlib-metadata
                             python-jplephem
                             python-jsonschema
                             python-numpy
                             python-packaging
                             python-photutils
                             python-poppy
                             python-psutil
                             python-pyparsing
                             python-pysiaf
                             python-requests
                             python-scikit-image
                             python-scipy
                             python-spherical-geometry
                             python-stcal
                             python-stdatamodels
                             python-stpipe
                             python-stsci-image
                             python-stsci-imagestats
                             python-synphot
                             python-tweakwcs
                             python-wiimatch))
    (native-inputs (list python-colorama
                         python-pytest
                         python-pytest-cov
                         python-pytest-doctestplus
                         python-pytest-openfiles
                         python-requests-mock
                         ;; python-ruff ; not packed yet in Guix
                         python-setuptools-scm))
    (home-page "https://jwst-pipeline.readthedocs.io/en/latest/")
    (synopsis
     "Python library for science observations from the James Webb Space Telescope")
    (description
     "This package provides an access to the JWST Science Calibration Pipeline
processes data from all JWST instruments and observing modes by applying various
science corrections sequentially, producing both fully-calibrated individual
exposures and high-level data products (mosaics, extracted spectra, etc.).")
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
      ;; FIXME: Invistigate why it failes on python-jwst side where the
      ;; python-tweakwcs is built just fine:
      ;;
      ;; <...>/tweakwcs/matchutils.py:18: in <module>
      ;; from stsci.stimage import xyxymatch
      ;; E   ModuleNotFoundError: No module named 'stsci.stimage'
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'sanity-check))))
    (propagated-inputs
     (list python-astropy
           python-jwst
           python-matplotlib
           python-numpy
           python-scipy))
    (native-inputs
     (list python-pytest
           python-stsci-stimage))
    (home-page "https://github.com/spacetelescope/jwst_reffiles")
    (synopsis "Tool for JWST's CRDS-formatted reference files creation")
    (description
     "This package provides a tool to create @acronym{Calibration References
Data System,CRDS}-formatted reference files for @acronym{James Webb Space
Telescope,JWST} from a set of input dark current files and a set of flat field
files.")
    (license license:bsd-3)))

(define-public python-pyerfa
  (package
    (name "python-pyerfa")
    (version "2.0.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyerfa" version))
       (sha256
        (base32 "1lwx4hril705r2iz4pirgn0wrpv1wk20mr669g03bsij69qsdf5c"))
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
     (list python-pytest-doctestplus python-pytest python-setuptools-scm))
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

(define-public python-pynbody
  (package
    (name "python-pynbody")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch) ;PyPi doesn't have not prebuit version.
       (uri (git-reference
             (url "https://github.com/pynbody/pynbody")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00isg6nsqzgjqpkczwvrcmj3ndzav3bfzla0a72b44cgdj20wyv8"))
       (modules '((guix build utils)))
       (snippet
        ;; Symlink goes to not existing directory.
        #~(for-each delete-file '("docs/testdata"
                                  "docs/tutorials/example_code/testdata")))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list
                           ;; Disable tests which need to download additional
                           ;; 1.0GiB+ of test data archive from
                           ;; http://star.ucl.ac.uk/~app/testdata.tar.gz
                           ;;    https://github.com/pynbody/pynbody/blob/ \
                           ;;    f4bd482dc47532831b3ec115c7cb07149d61bfc5/ \
                           ;;    .github/workflows/build-test.yaml#L41
                           ;; See <https://github.com/pynbody/pynbody/issues/778>
                           "--ignore=tests/copy_on_access_test.py"
                           "--ignore=tests/gravity_test.py"
                           "--ignore=tests/adaptahop_test.py"
                           "--ignore=tests/ahf_halos_test.py"
                           "--ignore=tests/array_test.py"
                           "--ignore=tests/bridge_test.py"
                           "--ignore=tests/family_test.py"
                           "--ignore=tests/partial_tipsy_test.py"
                           "--ignore=tests/snapshot_test.py"
                           "--ignore=tests/test_profile.py"
                           "--ignore=tests/gadget_test.py"
                           "--ignore=tests/gadgethdf_test.py"
                           "--ignore=tests/grafic_test.py"
                           "--ignore=tests/halotools_test.py"
                           "--ignore=tests/nchilada_test.py"
                           "--ignore=tests/ramses_new_ptcl_format_test.py"
                           "--ignore=tests/ramses_test.py"
                           "--ignore=tests/rockstar_test.py"
                           "--ignore=tests/sph_image_test.py"
                           "--ignore=tests/sph_smooth_test.py"
                           "--ignore=tests/subfind_test.py"
                           "--ignore=tests/subfindhdf_gadget4_test.py"
                           "--ignore=tests/tipsy_test.py"
                           "-k"
                           (string-append
                            "not test_div_curl_smoothing"
                            " and not test_float_kd"
                            " and not test_kd_delete"
                            " and not test_kd_issue_88"
                            " and not test_kdtree_from_existing_kdtree"
                            " and not test_kdtree_shared_mem"
                            " and not test_neighbour_list"
                            " and not test_particles_in_sphere"
                            " and not test_periodic_smoothing"
                            " and not test_smooth"
                            " and not test_smooth_WendlandC2"))
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'set-compiler
                 (lambda _
                   (setenv "CC" #$(cc-for-target)))))))
    (native-inputs
     (list python-cython
           python-pandas
           python-pytest))
    (propagated-inputs
     (list python-h5py
           python-matplotlib
           python-numpy
           python-posix-ipc
           python-scipy))
    (home-page "https://pynbody.github.io/pynbody/index.html")
    (synopsis "Light-weight astronomical N-body/SPH analysis for python")
    (description "@code{Pynbody} is an analysis framework for N-body and hydrodynamic
astrophysical simulations supporting PKDGRAV/Gasoline, Gadget, Gadget4/Arepo,
N-Chilada and RAMSES AMR outputs.")
    (license license:gpl3+)))

(define-public python-pyregion
  (package
    (name "python-pyregion")
    (version "2.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyregion" version))
       (sha256
        (base32 "0l7qb7r8fnv46mdih4m5b8jaxixgpw6m7v37dpikjkblgh0vigaw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              ;; Cython extensions have to be built before running the tests.
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-astropy python-numpy python-pyparsing))
    (native-inputs
     (list python-cython
           python-pytest
           python-pytest-astropy-header
           python-setuptools-scm))
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

(define-public python-pysiaf
  (package
    (name "python-pysiaf")
    (version "0.22.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pysiaf" version))
       (sha256
        (base32 "08wb98k9k4f04455da5ns9rif8pl9r3ih537w1yj393hkjjiyzfz"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto"
              ;; Disable 2 failing tests, see
              ;; <https://github.com/spacetelescope/pysiaf/issues/338>
              "-k" (string-append "not test_write_jwst_siaf_xlsx"
                                  " and not test_write_jwst_siaf_xml" ))))
    (propagated-inputs
     (list python-astropy
           python-lxml
           python-matplotlib
           python-numpy
           python-numpydoc
           python-openpyxl
           python-requests
           python-scipy))
    (native-inputs
     (list python-pytest python-pytest-xdist))
    (home-page "https://pysiaf.readthedocs.io/")
    (synopsis "Handling SIAF for space telescopes")
    (description
     "@code{pysiaf} is a python package to access, interpret, maintain, and
generate @acronym{Handling of Science Instrument Aperture Files, SIAF}, in
particular for JWST.  Tools for applying the frame transformations, plotting,
comparison, and validation are provided.")
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
             python-setuptools-scm))
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

(define-public python-sbpy
  (package
    (name "python-sbpy")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sbpy" version))
       (sha256
        (base32 "18f3056fgzpvjj43m845wl9znl4dqxs8f8qv3gpay7kik4l8a1fc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; See <https://github.com/NASA-Planetary-Science/sbpy/issues/397>.
      #~(list "--ignore=sbpy/spectroscopy/tests/test_specgrad.py"
              ;; See <https://github.com/NASA-Planetary-Science/sbpy/issues/398>
              "-k" (string-append "not test_from_fluxd"
                                  " and not test_bandpass"
                                  " and not test_spectral_density_vega_bp"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  No such file or directory: '/homeless-shelter/.astropy'
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-ads
           python-astropy
           python-astroquery
           python-ginga
           python-numpy
           python-photutils
           python-pyyaml
           python-scipy
           python-synphot))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-pytest-doctestplus
           python-pytest-remotedata
           python-pytest-xdist
           python-setuptools-scm))
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
     (list #:test-flags #~(list "test.py")))
    (native-inputs
     (list python-cython python-pytest))
    (propagated-inputs
     (list  python-numpy))
    (synopsis "Python library for Source Extraction and Photometry")))

(define-public python-suntime
  (package
    (name "python-suntime")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "suntime" version))
       (sha256
        (base32 "1kyd1r6zcs0jmh5gq74adrnb1h7dfr1mzjq4k4vbngfiga8gfd28"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-dateutil))
    (home-page "https://github.com/SatAgro/suntime")
    (synopsis "Sunset and sunrise time calculation python library")
    (description
     "Python library doing sunrise and sunset time calculation.  Takes a
WGS84 (GPS) latitude/longitude as input as well as an UTC or local datetime
object.")
    (license license:lgpl3+)))

(define-public python-synphot
  (package
    (name "python-synphot")
    (version "1.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "synphot" version))
       (sha256
        (base32 "0fk860bxaqbamrq15pkqlbvhbf70y50cra2mgvv9r0cxq37isbi6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (invoke "python" "setup.py" "build_ext" "--inplace")
                ;; Step out of the source directory to avoid interference; we
                ;; want to run the installed code with extensions etc.
                (with-directory-excursion "/tmp"
                  (apply invoke "pytest" "-v" test-flags))))))))
    (propagated-inputs
     (list python-astropy
           python-dust-extinction
           python-numpy
           python-scipy
           python-specutils ))
    (native-inputs
     (list python-pytest
           python-pytest-astropy
           python-setuptools-scm))
    (home-page "https://github.com/spacetelescope/synphot_refactor")
    (synopsis "Synthetic photometry using Astropy")
    (description
     "This package provides a replacement for IRAF STSDAS SYNPHOT and ASTROLIB
PYSYNPHOT, utilizing Astropy and covering the non-instrument specific portions
of the old packages.")
    (license license:bsd-3)))

(define-public python-asdf-compression
  ;; TODO: No release, change to tag when it's ready.
  (let ((commit "57cc7e76fb4163be3e99fb740b36b5ec5ae96e49")
        (revision "0"))
    (package
      (name "python-asdf-compression")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/asdf-format/asdf-compression")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1g6iiyls950k66dmd0pbqqdvz74kksc4j191n0ik6fhjnkiwifgs"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-version
              (lambda _
                (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" "0.0.1"))))))
      (native-inputs
       (list python-numpy
             python-pytest
             python-setuptools-scm))
      (propagated-inputs
       (list python-asdf
             python-blosc
             python-lz4
             python-zstandard))
      (home-page "https://github.com/asdf-format/asdf-fits-schemas")
      (synopsis "ASDF extension to support various compression algorithms")
      (description
       "This package includes a plugin for the Python library ASDF to add
support for reading and writing various compression algorithms including:
@url{https://www.blosc.org/python-blosc/reference.html,Blosc},
@url{https://python-lz4.readthedocs.io/en/stable/lz4.frame.html,LZ4 Frame},
@url{http://facebook.github.io/zstd/,Zstandard}.")
      (license license:bsd-3))))

(define-public python-asdf-standard
  (package
    (name "python-asdf-standard")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_standard" version))
       (sha256
        (base32
         "00k1fzc8y8j0ar1chq0nqyfw8bgkkjgrkm32ibn0kz2vn715nlq1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Remove tests requiring python-asdf where python-asdf requires
      ;; python-asdf-standard, break circular dependencies.
      #~(list "--ignore=tests/test_asdf_schema.py"
              "--ignore=tests/test_integration.py"
              "--ignore=tests/test_manifests.py"
              "--ignore=tests/test_yaml_schema.py")))
    (native-inputs (list python-astropy
                         python-jsonschema
                         python-pypa-build
                         python-pytest
                         python-packaging
                         python-setuptools-scm))
    (propagated-inputs (list python-importlib-resources))
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

(define python-asdf-transform-schemas
  (package
    (name "python-asdf-transform-schemas")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_transform_schemas" version))
       (sha256
        (base32 "0as6dcf9dmxjh24gwdmqwbbrk56fhgsmzwi7af4llwvm4mw4rkw2"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Dependency cycle with python-asdf
     (list #:tests? #f))
    (native-inputs (list python-setuptools-scm))
    (propagated-inputs (list python-asdf-standard python-importlib-resources))
    (home-page "https://github.com/asdf-format/asdf-transform-schemas")
    (synopsis "ASDF schemas for transforms")
    (description
     "This package provides ASDF schemas for validating transform tags.  Users
should not need to install this directly; instead, install an implementation
package such as asdf-astropy.")
    (license license:bsd-3)))

(define python-asdf-coordinates-schemas
  (package
    (name "python-asdf-coordinates-schemas")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_coordinates_schemas" version))
       (sha256
        (base32 "0kk0rlzv0kkcw6fiwvps4n0x05867xc9hxyyzica31zcvhan12y9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")))
    (native-inputs
     (list python-pytest
           python-pytest-xdist
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf))
    (home-page "https://github.com/asdf-format/asdf-coordinates-schemas")
    (synopsis "ASDF coordinates schemas")
    (description "This package provides ASDF schemas for validating
coordinates tags.  Users should not need to install this directly; instead,
install an implementation package such as asdf-astropy.")
    (license license:bsd-3)))

(define python-asdf-fits-schemas
  ;; TODO: No release, change to tag when it's ready.
  (let ((commit "d1b5e7a12a49fe61d43855045bab106be34cd252")
        (revision "1"))
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
          (base32
           "1h2xbk9c9l2959i3sdnwviv3z0hp1f0lba2vz9hpzgcm46qadqp8"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        ;; Dependency cycle with python-asdf
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-version
              (lambda _
                (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" "0.0.1"))))))
      (native-inputs (list python-setuptools-scm))
      (propagated-inputs (list python-asdf-standard python-importlib-resources))
      (home-page "https://github.com/asdf-format/asdf-fits-schemas")
      (synopsis "ASDF schemas to support the FITS format")
      (description
       "This package provides ASDF schemas for validating FITS tags.")
      (license license:bsd-3))))

(define python-asdf-time-schemas
  ;; TODO: No release, change to tag when it's ready.
  (let ((commit "a3062066ee70f1b934f7339d1ce96a5c5f61f055")
        (revision "3"))
    (package
      (name "python-asdf-time-schemas")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/asdf-format/asdf-time-schemas")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1i8lm2d18r6fadsch52dxc2zp1swkfa8w40s03albn7p290n4a97"))))
      (build-system pyproject-build-system)
      (arguments
       (list
        ;; Dependency cycle with python-asdf
        #:tests? #f
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'build 'set-version
              (lambda _
                (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" "0.0.1"))))))
      (native-inputs (list python-setuptools-scm))
      (propagated-inputs (list python-asdf-standard
                               python-asdf-unit-schemas
                               python-importlib-resources))
      (home-page "https://github.com/asdf-format/asdf-fits-schemas")
      (synopsis "Schemas for storing time in ASDF")
      (description
       "This package provides ASDF schemas for validating time tags.")
      (license license:bsd-3))))

(define python-asdf-unit-schemas
  (package
    (name "python-asdf-unit-schemas")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_unit_schemas" version))
       (sha256
        (base32
         "0h24gvm1wip4gh3fw5ki8kwnvcy8gw6b6pajw3i1zvriamdw95fr"))))
    (build-system pyproject-build-system)
    (arguments
     ;; Dependency cycle with python-asdf
     (list #:tests? #f))
    (native-inputs (list python-setuptools-scm))
    (propagated-inputs (list python-asdf-standard python-importlib-resources))
    (home-page "https://asdf-unit-schemas.readthedocs.io/")
    (synopsis "ASDF serialization schemas for the units defined by @code{astropy.units}")
    (description "This package provides ASDF schemas for validating unit tags.")
    (license license:bsd-3)))

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
    (propagated-inputs
     (list python-asdf python-fsspec python-zarr))
    (native-inputs
     (list python-pytest python-setuptools-scm))
    (home-page "https://github.com/asdf-format/asdf-zarr")
    (synopsis "Asdf extension to support Zarr arrays")
    (description
     "This package includes an extension for the Python library asdf to add
support for reading and writing chunked
@url{https://zarr.readthedocs.io/en/stable/,Zarr} arrays, a file storage
format for chunked, compressed, N-dimensional arrays based on an open-source
specification.")
    (license license:bsd-3)))

(define python-asdf-wcs-schemas
  (package
    (name "python-asdf-wcs-schemas")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_wcs_schemas" version))
       (sha256
        (base32 "1d08ng890xc1ilf3y1hrimx09x990wvg9g18n0ijwvpilnw49fy0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "tests")))
    (native-inputs
     (list python-asdf
           python-pytest
           python-pytest-openfiles
           python-setuptools-scm))
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
    (license license:bsd-3)))

(define-public python-gwcs
  (package
    (name "python-gwcs")
    (version "0.21.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gwcs" version))
       (sha256
        (base32 "1fn5l4v236bl7xqi1is40c2q57dji8by98iwqcndfnmjwqf7zllc"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-jsonschema
           python-jmespath
           python-pytest
           python-pytest-doctestplus
           python-pyyaml
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

(define-public python-rad
  (package
    (name "python-rad")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rad" version))
       (sha256
        (base32 "05b7qjhahzfjdp820m3qm69wrzb73njjqrzkk7hxkd8gbrbp0mj1"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Ignore tests requiring python-crds to break cycle:
      ;; python-rad -> python-roman-datamodels -> python-crds -> python-rad
      #:test-flags #~(list "--ignore=tests/test_schemas.py")))
    (native-inputs
     (list python-pytest python-setuptools-scm))
    (propagated-inputs
     (list python-asdf python-asdf-astropy))
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
    (version "0.3.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "radio-beam" version))
       (sha256
        (base32 "0dg6vqdhmzh47awdkkcbf455gw6if2qwxyhcqbq2dkhbwsx680gc"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-astropy
           python-matplotlib
           python-numpy
           python-scipy
           python-six))
    (native-inputs (list python-pytest-astropy python-setuptools-scm))
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
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "radiospectra" version))
       (sha256
        (base32 "0nw71a7i875lfwbwhpc80nvwalb7iq3fynfkbngx6f6jb3v10dgs"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'set-home-env
            (lambda _
              ;; Tests require HOME to be set.
              ;;  Permission denied: '/homeless-shelter'
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-cdflib
           python-matplotlib
           python-numpy
           python-scipy
           python-sunpy))
    (native-inputs
     (list python-pytest-astropy
           python-setuptools-scm
           python-sunpy-soar))
    (home-page "https://docs.sunpy.org/projects/radiospectra")
    (synopsis "Support for radio spectra on solar physics")
    (description
     "@code{radiospectra} provides support for some type of radio spectra in
solar physics.")
    (license license:bsd-2)))

(define-public python-roman-datamodels
  (package
    (name "python-roman-datamodels")
    (version "0.20.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "roman_datamodels" version))
       (sha256
        (base32 "1918wnssf478w168mhv009jkirmny8hyfxrkwvl8iish36dcqagh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-n" "auto")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-env
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (propagated-inputs
     (list python-asdf
           python-asdf-astropy
           python-astropy
           python-gwcs
           python-numpy
           python-psutil
           python-rad))
    (native-inputs
     (list python-pytest
           python-pytest-doctestplus
           python-pytest-env
           python-pytest-xdist
           python-setuptools-scm))
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

(define-public python-skyfield
  (package
    (name "python-skyfield")
    (version "1.49")
    (source
     (origin
       (method git-fetch) ; PyPI tarball lacks test data
       (uri (git-reference
             (url "https://github.com/skyfielders/python-skyfield")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rk71lzy8w28f2kzhxb3pyndncrj91jay43nvqlrlzjxi2rbg7ix"))))
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
     (list python-assay python-pandas))
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

(define-public python-tweakwcs
  (package
    (name "python-tweakwcs")
    (version "0.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "tweakwcs" version))
       (sha256
        (base32 "148as0x1szk1wv13bwk947qs3rw5xj2286ccdbhzfzarng58a386"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-astropy
           python-gwcs
           python-numpy
           python-packaging
           python-spherical-geometry
           python-stsci-imagestats
           python-stsci-stimage))
    (native-inputs
     (list python-codecov
           python-pytest
           python-pytest-cov
           python-scipy
           python-setuptools-scm))
    (home-page "https://tweakwcs.readthedocs.io/en/latest/")
    (synopsis
     "Algorithms for matching and aligning catalogs and for tweaking the WCS")
    (description
     "@code{tweakwcs} is a package that provides core algorithms for computing
and applying corrections to @code{WCS} objects such as to minimize mismatch
between image and reference catalogs. Currently only aligning images with
@code{FITS WCS} and @code{JWST gWCS} are supported.")
    (license license:bsd-3)))

(define-public python-viresclient
  (package
    (name "python-viresclient")
    (version "0.11.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "viresclient" version))
       (sha256
        (base32 "1npn5ka0cflvl6ngf5b08z59dh79hnyh5v2z4sf0872q9zkwmjjw"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-flit-core python-pytest))
    (propagated-inputs
     (list python-cdflib
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
                         python-pytest-doctestplus python-setuptools-scm))
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
    (version "4.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yt" version))
       (sha256
        (base32 "03jy35vyniyd1pd3sv0zpd2f3ks2iyqw65xv28ids8nw6v1vavbv"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:build-backend "setuptools.build_meta"
      #:test-flags
      #~(list "-n" "auto")
      #:phases
      #~(modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "pyproject.toml"
               ;; XXX: Updating ipywidgets requires long chain of rebuilds,
               ;; maybe for python-team.
               ;;
               ;; ipywidgets>=8.0.0
               ((">=8.0.0") ">=7.6.3"))))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-cmyt
           python-ewah-bool-utils
           python-ipywidgets
           python-matplotlib
           python-more-itertools-next
           python-numpy
           python-packaging
           python-pillow
           python-tomli-w
           python-tqdm
           python-unyt))
    (native-inputs
     (list python-cython-3
           python-nose
           python-nose-exclude
           python-nose-timer
           python-pyaml
           python-pytest
           python-pytest-mpl
           python-pytest-xdist
           python-setuptools
           python-sympy))
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
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "yt_astro_analysis" version))
       (sha256
        (base32 "1fb3sdp6hq2c4c28pd33v9yj14x9l7qizf3y3qpl594qdq1ffmpi"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Disable test which require MPI setup and failed to run, check why.
      #:test-flags #~(list "--ignore=test_halo_finders_ts.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'build-extensions
            (lambda _
              (invoke "python" "setup.py" "build_ext" "--inplace"))))))
    (propagated-inputs
     (list python-h5py
           python-numpy
           python-packaging
           python-yt))
    (native-inputs
     (list python-cython
           python-nose
           python-tomli))
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
    (home-page "https://www.eso.org/sci/software/eclipse/qfits/")
    (synopsis "C library offering access to astronomical FITS files")
    (description
     "@code{qfits} is a C library giving access to FITS file internals, both
for reading and writing.")
    (license license:gpl2+)))

(define-public stuff
  (package
    (name "stuff")
    (version "2.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/stuff")
             ;; XXX: No version tag available in GitHub.
             ;; See: https://github.com/astromatic/stuff/issues/6
             (commit "9008dc022ef53331092da248cf0a794abd6783bf")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "004sry5lqqm7s9x4l3agysp3n63y3ga35x1rwwda4m6dc6zvla6b"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page "https://www.astromatic.net/software/stuff")
    (synopsis "Astronomical catalogue simulation")
    (description
     "Stuff is a program that simulates \"perfect\" astronomical catalogues.
It generates object lists in ASCII which can read by the SkyMaker program to
produce realistic astronomical fields.  Stuff is part of the
@uref{https://www.astromatic.net/projects/efigi, EFIGI} development project.")
    (license license:gpl3+)))

(define-public sunclock
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

(define-public unsio
  ;; There is no versioned tag, use the latest commit.
  (let ((commit "25e52468298e1194c9726ef5dba9d5fbb46870f5")
        (revision "0"))
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
                  "110i2p5608zhh5w3pf3b5r2651hykw2ayspgq6vpqsffhya1p170"))
                (modules '((guix build utils)))
                (snippet
                  ;; force installation into lib/ instead of lib64/
                  #~(substitute* "cmake/DetectCpackGenerator.cmake"
                      (("lib64") "lib")))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f ; no tests
             #:build-type "Release" ; to improve performace
             #:configure-flags #~(list "-DCMAKE_CXX_STANDARD=14")))
      (inputs (list gfortran hdf5 perl sqlite zlib))
      (home-page "https://projets.lam.fr/projects/unsio/wiki")
      (synopsis "Input and output routines for n-body file formats")
      (description
       "@acronym{UNSIO, Universal Nbody Snapshot Input Output} provides
an API for performing input and output operations on different kinds of
n-body file formats (nemo, Gadget binaries 1 and 2, Gadget hdf5, Ramses).")
      (license license:cecill))))

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
the image to position on the sky.  Auxillary programs search star catalogs and
manipulate images.")
    (license license:gpl2+)))

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
