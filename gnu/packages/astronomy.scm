;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 by Amar Singh <nly@disroot.org>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021, 2022, 2023 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2021, 2022 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
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
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
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
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
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
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (delete 'check)
                        (add-after 'install 'post-install-check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "make" "fittest")))))))
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
  (let ((commit "7329a075271edab8f6264db649e81e62b2b6ae5e")
        (revision "1"))
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
          (base32 "0qcfax6pbzs0yigy0x8xibrkk539wm2pbvjsb4lh50fybir02nix"))
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

(define-public calceph
  (package
    (name "calceph")
    (version  "3.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://www.imcce.fr/content/medias/recherche/equipes/asd/calceph/calceph-"
             version ".tar.gz"))
       (sha256
        (base32 "078wn773pwf4pg9m0h0l00g4aq744pq1rb6kz6plgdpzp3hhpk1k"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://www.imcce.fr/inpop/calceph")
    (properties `((release-monitoring-url . ,home-page)))
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
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/10110111/CalcMySky")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgn937px4am0lggniwby1f1dl3v4l3iigq72rl9rlhx58zv4lj1"))))
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

(define-public aoflagger
  (package
    (name "aoflagger")
    (version "3.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/aroffringa/aoflagger")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1dcbfrbiybhpbypna2xhddx1wk7yifh38ha2r6p5rzsikzwlsin1"))
       (patches
        (search-patches "aoflagger-use-system-provided-pybind11.patch"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; XXX: Tests require external files download from
      ;; https://www.astron.nl/citt/ci_data/aoflagger/
      ;; FIXME: runtest is not found
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
           lapack
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
           lapack
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

(define-public cfitsio
  (package
    (name "cfitsio")
    (version "4.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://heasarc.gsfc.nasa.gov/FTP/software/fitsio/c/"
             "cfitsio-" version ".tar.gz"))
       (sha256
        (base32 "128qsv2q0f0g714ahlsixiikvvbwxi9bg9q9pcr5cd3f7wdkv9gb"))))
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

(define-public python-fitsio
  (package
    (name "python-fitsio")
    (version "1.1.8")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fitsio" version))
       (sha256
        (base32 "1y80hgvlkjz1bijfyb2j03853yc1kc63yrf9ab7as31ad2r6kxb1"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove the bundled cfitsio
        `(begin
           (delete-file-recursively "cfitsio3490")
           (substitute* "MANIFEST.in"
             (("recursive-include cfitsio3490.*$\n") ""))))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unbundle-cfitsio
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((cfitsio (assoc-ref inputs "cfitsio"))
                    (includedir (string-append "\"" cfitsio "/include\""))
                    (libdir (string-append "\"" cfitsio "/lib\"")))
               ;; Use Guix' cfitsio instead of the bundled one
               (substitute* "setup.py"
                 (("self.use_system_fitsio = False") "pass")
                 (("self.system_fitsio_includedir = None") "pass")
                 (("self.system_fitsio_libdir = None") "pass")
                 (("self.use_system_fitsio") "True")
                 (("self.system_fitsio_includedir") includedir)
                 (("self.system_fitsio_libdir") libdir))))))))
    (inputs (list curl))
    (propagated-inputs
     (list python-numpy cfitsio))
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

(define-public erfa
  (package
    (name "erfa")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/liberfa/erfa")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s9dpj0jdkqcg552f00jhd722czji4pffabmpys5pgi6djckq4f4"))))
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
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/eye/"
                           "eye-" version ".tar.gz"))
       (sha256
        (base32 "092qhzcbrkcfidbx4bv9wz42w297n80jk7a6kwyi9a3fjfz81d7k"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/eye")
    (synopsis "Small image feature detector using machine learning")
    (description
     "In EyE (Enhance Your Extraction) an artificial neural network connected to
pixels of a moving window (retina) is trained to associate these input stimuli
to the corresponding response in one or several output image(s).  The resulting
filter can be loaded in SExtractor to operate complex, wildly non-linear filters
on astronomical images.  Typical applications of EyE include adaptive filtering,
feature detection and cosmetic corrections.")
    (license license:cecill)))

(define-public wcslib
  (package
    (name "wcslib")
    (version "7.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "ftp://ftp.atnf.csiro.au/pub/software/wcslib/wcslib-" version
             ".tar.bz2"))
       (sha256
        (base32 "1m3bx6gh5w3c7vvsqcki0x20mg8lilg13m0i8nh7za89w58dxy4w"))))
    (inputs
     (list cfitsio))
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
    (home-page "https://www.atnf.csiro.au/people/mcalabre/WCS")
    (synopsis "Library which implements the FITS WCS standard")
    (description "The FITS \"World Coordinate System\" (@dfn{WCS}) standard
defines keywords and usage that provide for the description of astronomical
coordinate systems in a @dfn{FITS} (Flexible Image Transport System) image
header.")
    (license license:lgpl3+)))

(define-public weightwatcher
  (package
    (name "weightwatcher")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/weightwatcher/"
                           "weightwatcher-" version ".tar.gz"))
       (sha256
        (base32 "1zaqd8d9rpgcwjsp92q3lkfaa22i20gppb91dz34ym54swisjc2p"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/weightwatcher")
    (synopsis "Weight-map/flag-map multiplexer and rasteriser")
    (description
     "Weightwatcher is a program hat combines weight-maps, flag-maps and
polygon data in order to produce control maps which can directly be used in
astronomical image-processing packages like Drizzle, Swarp or SExtractor.")
    (license license:gpl3+)))

(define-public gnuastro
  (package
    (name "gnuastro")
    (version "0.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnuastro/gnuastro-"
                           version ".tar.lz"))
       (sha256
        (base32
         "192q3i4zlfmwfcchlf9lnpfgys9dssh7npr5qx0gzn3d2fri0xbz"))))
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
    (version "2.6.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenPHDGuiding/phd2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n87xsv9gzrwk1ygws4vw397ffq40xybp5b3c3bd5kcmff0avaw9"))
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
    (version "2.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astromatic/sextractor")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0q69n3nyal57h3ik2xirwzrxzljrwy9ivwraxzv9566vi3n4z5mw"))))
    (build-system gnu-build-system)
    ;; NOTE: (Sharlatan-20210124T103117+0000): Building with `atlas' is failing
    ;; due to missing shared library which required on configure phase. Switch
    ;; build to use `openblas' instead. It requires FFTW with single precision
    ;; `fftwf'.
    (arguments
     `(#:configure-flags
       (list
        "--enable-openblas"
        (string-append
         "--with-openblas-libdir=" (assoc-ref %build-inputs "openblas") "/lib")
        (string-append
         "--with-openblas-incdir=" (assoc-ref %build-inputs "openblas") "/include")
        (string-append
         "--with-fftw-libdir=" (assoc-ref %build-inputs "fftw") "/lib")
        (string-append
         "--with-fftw-incdir=" (assoc-ref %build-inputs "fftw") "/include"))))
    (native-inputs
     (list autoconf automake libtool))
    (inputs
     `(("openblas" ,openblas)
       ("fftw" ,fftwf)))
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
    (version "1.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/free-astro/siril")
                    (commit version)))
              (sha256
               (base32
                "0iqxb5zmjyygg4b6lwlq8z82mngxg7kjjpahhzk52m0cypfq0l18"))
              (file-name (git-file-name name version))))
    (build-system meson-build-system)
    (native-inputs (list cmake git glib libconfig pkg-config))
    (inputs (list cfitsio
                  exiv2
                  fftwf
                  gsl
                  gtk+
                  json-glib
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
    (version "3.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/danieljprice/splash")
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "0nsm6rk0bi99xz7wclk0zy4bpqf0qcsdln5cdjb30lhpf37i2fpa"))
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
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/skymaker/"
                           "skymaker-" version ".tar.gz"))
       (sha256
        (base32 "03zvx7c89plp9559niqv5532r233kza3ir992rg3nxjksqmrqvx1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append
         "--with-fftw-libdir=" (assoc-ref %build-inputs "fftw") "/lib")
        (string-append
         "--with-fftw-incdir=" (assoc-ref %build-inputs "fftw") "/include"))))
    (inputs
     `(("fftw" ,fftwf)))
    (home-page "https://www.astromatic.net/software/skymaker")
    (synopsis "Astronomical image simulator")
    (description
     "SkyMaker is a program that simulates astronomical images.  It accepts
object lists in ASCII generated by the Stuff program to produce realistic
astronomical fields.  SkyMaker is part of the EFIGI
(@url{https://www.astromatic.net/projects/efigi}) development project.")
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
     `(#:make-flags
       (list
        (string-append
         "SKRY_INCLUDE_PATH=" (assoc-ref %build-inputs "libskry") "/include")
        (string-append
         "SKRY_LIB_PATH=-L" (assoc-ref %build-inputs "libskry") "/lib")
        (string-append
         "LIBAV_INCLUDE_PATH=" (assoc-ref %build-inputs "ffmpeg") "/include"))
       #:phases
       (modify-phases %standard-phases
         ;; no configure and tests are provided
         (delete 'configure)
         (delete 'check)
         (add-after 'unpack 'fix-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/main.cpp"
               (("\"\\.\\.\", \"lang\"")
                "\"../share/stackistry\", \"lang\""))
             (substitute* "src/utils.cpp"
               (("\"\\.\\.\", \"icons\"")
                "\"../share/stackistry\", \"icons\""))
             #t))
         (replace 'install
           ;; The Makefile lacks an ‘install’ target.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (icons (string-append out "/share/stackistry/icons"))
                    (lang (string-append out "/share/stackistry/lang")))
               (copy-recursively "bin" bin)
               (copy-recursively "icons" icons)
               (copy-recursively "lang" lang))
             #t)))))
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
    (version "23.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Stellarium/stellarium")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03gq7n15dsvb32pfq7j3a0vc5wf1y9xdxaq09q1gp534jkgd4g7f"))))
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
           openssl
           qtbase-5
           qtcharts
           qtlocation
           qtmultimedia-5
           qtpositioning
           qtscript
           qtserialport
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

(define-public stuff
  (package
    (name "stuff")
    (version "1.26.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/stuff/"
                           "stuff-" version ".tar.gz"))
       (sha256
        (base32 "1syibi3b86z9pikhicvkkmgxm916j732fdiw0agw0lq6z13fdcjm"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/stuff")
    (synopsis "Astronomical catalogue simulation")
    (description
     "Stuff is a program that simulates \"perfect\" astronomical catalogues.
It generates object lists in ASCII which can read by the SkyMaker program to
produce realistic astronomical fields.  Stuff is part of the EFIGI development
project.")
    (license license:gpl3+)))

(define-public swarp
  (package
    (name "swarp")
    (version "2.38.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/swarp/"
                           "swarp-" version ".tar.gz"))
       (sha256
        (base32 "1i670waqp54vin1cn08mqckcggm9zqd69nk7yya2vvqpdizn6jpm"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/swarp")
    (synopsis "FITS image resampling and co-addition")
    (description
     "SWarp is a program that resamples and co-adds together FITS images using
any arbitrary astrometric projection defined in the WCS standard.")
    (license license:gpl3+)))

(define-public celestia
  (let ((commit "9dbdf29c4ac3d20afb2d9a80d3dff241ecf81dce"))
    (package
      (name "celestia")
      (version (git-version "1.6.1" "815" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/celestiaproject/celestia")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "00xibg87l1arzifakgj7s828x9pszcgx7x7ij88a561ig49ryh78"))))
      (build-system cmake-build-system)
      (native-inputs
       `(("perl" ,perl)
         ("libgit2" ,libgit2)
         ("pkg-config" ,pkg-config)
         ("libtool" ,libtool)
         ("gettext" ,gettext-minimal)))
      (inputs
       `(("glu" ,glu)
         ("glew" ,glew)
         ("libtheora" ,libtheora)
         ("libjpeg" ,libjpeg-turbo)
         ("libpng" ,libpng)
         ;; maybe required?
         ("mesa" ,mesa)
         ;; optional: fmtlib, Eigen3;
         ("fmt" ,fmt-7)
         ("eigen" ,eigen)
         ;; glut: for glut interface
         ("freeglut" ,freeglut)))
      (propagated-inputs
       (list lua))
      (arguments
       `(#:configure-flags '("-DENABLE_GLUT=ON" "-DENABLE_QT=OFF")
         #:tests? #f))                            ;no tests
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

(define-public celestia-gtk
  (package
    (inherit celestia)
    (name "celestia-gtk")
    (inputs
     (append (alist-delete "freeglut" (package-inputs celestia))
             `(("gtk2" ,gtk+-2)
               ("gtkglext" ,gtkglext))))
    (arguments
     `(#:configure-flags '("-DENABLE_GTK=ON" "-DENABLE_QT=OFF")
       #:tests? #f))))

(define-public python-astropy
  (package
    (name "python-astropy")
    (version "5.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy" version))
       (sha256
        (base32 "08xc6brs7xwiqchhsjq8l10p6qc5p68cfxps7s889spqfyh2gbpn"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove Python bundles.
           (with-directory-excursion "astropy/extern"
             (for-each delete-file-recursively '("ply" "configobj")))
           ;; Remove cextern bundles. Check bundled versions against available
           ;; in Guix in the future update of astropy.
           (with-directory-excursion "cextern"
             (for-each delete-file-recursively '("cfitsio" "expat" "wcslib")))
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'preparations
           (lambda _
             ;; Use our own libraries in place of bundles.
             (setenv "ASTROPY_USE_SYSTEM_ALL" "1")
             ;; Some tests require a writable home.
             (setenv "HOME" "/tmp")
             ;; Relax xfail tests.
             (substitute* "setup.cfg"
               (("xfail_strict = true") "xfail_strict = false"))
             ;; Replace all references to external ply.
             (let ((ply-files '("coordinates/angle_formats.py"
                                "utils/parsing.py")))
               (with-directory-excursion "astropy"
                 (map (lambda (file)
                        (substitute* file (("astropy.extern.ply")
                                           "ply")))
                      ply-files)))
             ;; Replace reference to external configobj.
             (with-directory-excursion "astropy/config"
               (substitute* "configuration.py"
                 (("from astropy.extern.configobj ") "")))))
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "astropy/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "astropy/_compiler.c")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Extensions have to be rebuilt before running the tests.
               (invoke "python" "setup.py" "build_ext" "--inplace")
               (invoke "python" "-m" "pytest" "--pyargs" "astropy"
                       ;; Skip tests that need remote data.
                       "-m" "not remote_data")))))))
    (native-inputs
     (list pkg-config
           python-coverage
           python-cython
           python-extension-helpers
           python-ipython
           python-jplephem
           python-objgraph
           python-pytest
           python-pytest-astropy
           python-pytest-xdist
           python-setuptools-scm
           python-sgp4
           python-skyfield))
    (inputs
     (list cfitsio expat wcslib))
    (propagated-inputs
     (list python-configobj
           python-numpy
           python-packaging
           python-ply
           python-pyerfa
           python-pyyaml))
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
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astropy_healpix" version))
       (sha256
        (base32 "1n1svmd41iv944zf4anbnsigd47zr4dfjf49vrc7m6928gmq9hw8"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "astropy_healpix/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "astropy_healpix/_compiler.c")))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               ;; Extensions have to be rebuilt before running the tests.
               (invoke "python" "setup.py" "build_ext" "--inplace")
               (invoke "python" "-m" "pytest"
                       "--pyargs" "astropy_healpix")))))))
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

(define-public python-astroquery
  (package
    (name "python-astroquery")
    (version "0.4.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroquery" version))
       (sha256
        (base32 "1vhkzsqlgn3ji5by2rdf2gwklhbyzvpzb1iglalhqjkkrdaaaz1h"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'writable-home
           (lambda _                    ; some tests need a writable home
             (setenv "HOME" (getcwd))))
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest" "--pyargs" "astroquery"
                       ;; Skip tests that require online data.
                       "-m" "not remote_data")))))))
    (propagated-inputs
     (list python-astropy
           python-beautifulsoup4
           python-html5lib
           python-keyring
           python-numpy
           python-pyvo
           python-requests))
    (native-inputs
     (list python-flask
           python-jinja2
           python-matplotlib
           python-pytest-astropy
           python-pytest-dependency))
    (home-page "https://www.astropy.org/astroquery/")
    (synopsis "Access online astronomical data resources")
    (description "Astroquery is a package that contains a collection of tools
to access online Astronomical data.  Each web service has its own sub-package.")
    (license license:bsd-3)))

(define-public python-cdflib
  (package
    (name "python-cdflib")
    (version "0.4.9")
    (source
     (origin
       (method git-fetch)   ; no tests in pypi archive
       (uri (git-reference
             (url "https://github.com/MAVENSDC/cdflib")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1k557najk7ln293zwyghnhw48ays3nqf9s94kibsc7r70c2q7p08"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'check 'set-home-env
                 (lambda _
                   (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-attrs python-numpy))
    (native-inputs
     (list python-astropy
           python-hypothesis
           python-pytest
           python-pytest-cov
           python-pytest-remotedata
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

(define-public python-czml3
  (package
    (name "python-czml3")
    (version "0.7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/poliastro/czml3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0pbwcsmc9nw591rck586ca9hwwhmm54rjjmp8gflhzq8b7f48lkc"))))
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
    (version "0.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "drms" version))
       (sha256
        (base32 "1b0w350y4wbgyy19zcf28xbb85mqq6gnhb6ppibbc4hbn2ixbcvj"))))
    (build-system python-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs outputs tests?
                      #:allow-other-keys)
              (when tests?
                (add-installed-pythonpath inputs outputs)
                (setenv "JSOC_EMAIL" "jsoc@sunpy.org")
                (invoke "python" "-m" "pytest" "-vv")))))))
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

(define-public python-ephem
  (package
    (name "python-ephem")
    (version "4.1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ephem" version))
              (sha256
               (base32
                "0smmm3l8csnw9rrimh8mpyjrm80jaafjl184spnji98vk22qd8bz"))))
    (build-system python-build-system)
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
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "hvpy" version))
       (sha256
        (base32 "0r0asyflz2sw9zn5vgs138nh81m0rbwbakmrncbc1ghdr3g6jahv"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; Requires HTTP(S) access to api.beta.helioviewer.org
    (propagated-inputs (list python-pydantic python-requests))
    (native-inputs (list python-pytest python-pytest-astropy))
    (home-page "https://helioviewer.org/")
    (synopsis "Helioviewer Python API Wrapper")
    (description "@code{hvpy} is a Python API wrapper around the formal
@url{Helioviewer API, https://api.helioviewer.org/docs/v2/}.")
    (license license:bsd-2)))

(define-public python-mpl-animators
  (package
    (name "python-mpl-animators")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpl_animators" version))
       (sha256
        (base32 "12kjmj7rn3pk9ly82h5s5hn0kl3kxkr7bgkz9zr9k59pir8z1r8b"))))
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

(define-public python-photutils
  (package
    (name "python-photutils")
    (version "1.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "photutils" version))
       (sha256
        (base32 "0w4kxl6aqjp2wv396krw30kwg6cmmska8gvgpihm2i2zxyzz39vd"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "photutils/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "photutils/_compiler.c"))))))
    (propagated-inputs
     (list python-astropy python-numpy))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-astropy
           python-pytest-runner
           python-setuptools-scm))
    (home-page "https://github.com/astropy/photutils")
    (synopsis "Source detection and photometry")
    (description "Photutils is an Astropy package for detection and photometry
of astronomical sources.")
    (license license:bsd-3)))

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
      #:phases
      #~(modify-phases %standard-phases
          ;; NOTE: Tests take about 7-10 minutes to pass.
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              ;; TODO: Review failing tests later when any upstream
              ;; suggestions are provided:
              ;; https://github.com/poliastro/poliastro/issues/1618
              (substitute* "tests/test_czml.py"
              (("def test_czml_add_trajectory") "def __off_test_czml_add_trajectory")
              (("def test_czml_custom_packet") "def __off_test_czml_custom_packet")
              (("def test_czml_ground_station") "def __off_test_czml_ground_station")
              (("def test_czml_groundtrack") "def __off_test_czml_groundtrack")
              (("def test_czml_preamble") "def __off_test_czml_preamble"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "python" "-m" "pytest"
                        ;; Skip tests that need remote data.
                        "-m" "not remote_data")))))))
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
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "poppy" version))
              (sha256
               (base32
                "050cn6aabd1dxbi7zihbqnkl79hz6q6d5n6g25zmrpvc4sii171m"))))
    (build-system pyproject-build-system)
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
           python-sphinx
           python-setuptools-scm))
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

(define-public python-pyvo
  (package
    (name "python-pyvo")
    (version "1.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyvo" version))
       (sha256
        (base32 "1ri5yp6903386lkn79mdcmlax7zsfrrrjbcvb91wxydcc9yasc1n"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "--pyargs" "pyvo" "-k"
                       (string-append   ; these tests use the network
                        "not test_access_with_string"
                        " and not test_access_with_list"
                        " and not test_access_with_expansion"))))))))
    (native-inputs
     (list python-pytest-astropy python-requests-mock))
    (propagated-inputs
     (list python-astropy python-mimeparse python-pillow python-requests))
    (home-page "https://github.com/astropy/pyvo")
    (synopsis "Access Virtual Observatory data and services")
    (description
     "PyVO is a package providing access to remote data and services of the
Virtual observatory (VO) using Python.")
    (license license:bsd-3)))

(define-public python-regions
  (package
    (name "python-regions")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "regions" version))
       (sha256
        (base32 "08wsg9kxnki5pz8gdp81bq06jdmsip382fl02rvnr96cvzr7wf3m"))))
    (build-system python-build-system)
    (arguments
     `(#:test-target "pytest"
       #:phases
       (modify-phases %standard-phases
         ;; This doctest requires online data.
         (add-after 'unpack 'delete-doctest
           (lambda _ (delete-file "docs/masks.rst")))
         ;; This file is opened in both install and check phases.
         (add-before 'install 'writable-compiler
           (lambda _ (make-file-writable "regions/_compiler.c")))
         (add-before 'check 'writable-compiler
           (lambda _ (make-file-writable "regions/_compiler.c")))
         (add-before 'check 'writable-home
           (lambda _  (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-astropy python-numpy))
    (native-inputs
     (list python-cython
           python-extension-helpers
           python-pytest-arraydiff
           python-pytest-astropy
           python-pytest-runner
           python-setuptools-scm))
    (home-page "https://github.com/astropy/regions")
    (synopsis "Package for region handling")
    (description "Regions is an Astropy package for region handling.")
    (license license:bsd-3)))

(define-public python-reproject
  (package
    (name "python-reproject")
    (version "0.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "reproject" version))
       (sha256
        (base32 "1msysqbhkfi3bmw29wipk250a008bnng7din56md9ipbwiar8x55"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; FIXME: Failing tests
      ;;
      ;; reproject/adaptive/core.py:7: in <module>
      ;; from .deforest import map_coordinates
      ;; E   ModuleNotFoundError: No module named 'reproject.adaptive.deforest'
      ;;
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'writable-compiler
            (lambda _
              (make-file-writable "reproject/_compiler.c")))
          (add-before 'check 'writable-compiler
            (lambda _
              (make-file-writable "reproject/_compiler.c")))
          (add-before 'check 'writable-home
            (lambda _
              (setenv "HOME" (getcwd)))))))
    (propagated-inputs
     (list python-astropy
           python-astropy-healpix
           python-numpy
           python-scipy))
    (native-inputs
     (list python-asdf
           python-cython
           python-extension-helpers
           python-gwcs
           python-pytest-astropy
           python-pyvo
           python-semantic-version
           python-pytest
           python-setuptools-scm
           python-shapely))
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
    (version "2.21")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sgp4" version))
       (sha256
        (base32 "1vzcrlrlzmhbycdz16m8v241l8zx49vsy81wcd0yjxs80isvhyb1"))))
    (build-system python-build-system)
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
    (version "4.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sunpy" version))
       (sha256
        (base32 "1h8dnsic96bxm5l278vk6jj5h4bh1b143fghsvv5rhigk137vysp"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'writable-compiler
            (lambda _
              (make-file-writable "sunpy/_compiler.c")))
          (add-before 'check 'prepare-test-environment
            (lambda _
              (setenv "HOME" "/tmp")
              (make-file-writable "sunpy/_compiler.c")
              ;; TODO: (Sharlatan-20221106T115800+0000): Review failing tests
              (substitute* "sunpy/image/tests/test_transform.py"
                (("def test_clipping") "def __off_test_clipping")
                (("def test_nans") "def __off_test_nans")
                (("def test_endian") "def __off_test_endian"))
              (substitute* "sunpy/map/tests/test_mapbase.py"
                (("def test_derotating_nonpurerotation_pcij")
                 "def __off_test_derotating_nonpurerotation_pcij"))
              (substitute* "sunpy/map/sources/tests/test_mdi_source.py"
                (("def test_synoptic_source")
                 "def __off_test_synoptic_source"))
              (substitute* "sunpy/tests/tests/test_self_test.py"
                (("def test_main_nonexisting_module")
                 "def __off_test_main_nonexisting_module")
                (("def test_main_stdlib_module")
                 "def __off_test_main_stdlib_module")))))))
    (native-inputs
     (list python-aiohttp
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
           ;; python-opencv-python ; not packed yet
           python-pandas
           python-reproject
           python-scikit-image
           python-scipy
           python-semantic-version
           python-sqlalchemy
           python-tqdm
           python-zeep))
    (home-page "https://sunpy.org")
    (synopsis "Python library for Solar Physics")
    (description
     "SunPy is package for solar physics and is meant to be a free alternative to the
SolarSoft data analysis environment.")
    (license license:bsd-2)))

(define-public python-astral
  (package
    (name "python-astral")
    (version "2.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astral" version))
       (sha256
        (base32 "1gkggdibccmdy9glymw3kbrkzm6svvsg0lk56hhy92y4smkrj7g4"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs
     (list python-freezegun python-setuptools-scm))
    (propagated-inputs
     (list python-dataclasses python-pytest python-pytz))
    (home-page "https://github.com/sffjunkie/astral")
    (synopsis "Calculations for the position of the sun and moon")
    (description "Astral is a Python module that calculates times for various
positions of the sun: dawn, sunrise, solar noon, sunset, dusk, solar
elevation, solar azimuth, rahukaalam, and the phases of the moon.")
    (license license:asl2.0)))

(define-public python-spherical-geometry
  (package
    (name "python-spherical-geometry")
    (version "1.2.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spacetelescope/spherical_geometry")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kzcncqir4v7nhk9lxj9gxr32p3krkaqa58y2i4kksgxxy24qw4z"))))
    (build-system python-build-system)
    (arguments
     (list
      ;; NOTE: (Sharlatan-20220523T231348+0100): Tests depends on old Python2
      ;; libarry `sphere'
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'preparations
            (lambda _
              ;; Fixing: setuptools-scm was unable to detect version for ...
              (substitute* "setup.py"
                (("use_scm_version=True")
                 (format #f "version=~s" #$version))
                (("setup_requires=\\['setuptools_scm'\\],.*")
                 ""))
              ;; Use our own libraries in place of bundles.
              (setenv "USE_SYSTEM_QD" "1"))))))
    (native-inputs
     (list python-pytest
           python-setuptools-scm))
    (inputs
     (list qd))
    (propagated-inputs
     (list python-astropy
           python-numpy))
    (home-page "https://github.com/spacetelescope/tweakwcs")
    (synopsis "Python astronimical package for handling spherical polygons")
    (description
     "The @code{spherical_geometry} library is a Python package for handling
spherical polygons that represent arbitrary regions of the sky.")
    ;; LICENSE.rst Association of Universities for Research in Astronomy (AURA)
    ;; QD_LIBRARY_LICENSE.rst for bandeled QD source
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
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-git-version
           (lambda _
             (substitute* "./git-version-gen"
               (("/bin/sh") (which "sh")))
             #t)))))
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
  ;; NOTE: (Sharlatan-20210122T215921+0000): the version tag has a build
  ;; error on spice which is resolved with the latest commit.
  (let ((commit "e3c218d1502a18cae858c83a9a8812ab197fcb60")
        (revision "1"))
    (package
      (name "libpasastro")
      (version (git-version "1.4.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/pchev/libpasastro")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0asp2sn34nds5va2ghppwc41vb6j3d1mf049j949rgrll817kx47"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f
         #:make-flags
         (list
          ,(match (or (%current-target-system) (%current-system))
             ((or "aarch64-linux" "armhf-linux" "i686-linux" "x86_64-linux")
              "OS_TARGET=linux")
             (_ #f))
          ,(match (or (%current-target-system) (%current-system))
             ("i686-linux" "CPU_TARGET=i386")
             ("x86_64-linux" "CPU_TARGET=x86_64")
             ((or "armhf-linux" "aarch64-linux") "CPU_TARGET=armv7l")
             (_ #f))
          (string-append "PREFIX=" (assoc-ref %outputs "out")))
         #:phases
         (modify-phases %standard-phases
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
@end itemize\n")
      (license license:gpl2+))))

(define-public libxisf
  (package
    (name "libxisf")
    ;; TODO: v0.2.2 (current latest) failed to build on configure phase, issue
    ;; was open directly with author as he hosts source on seflhosted gitea.
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitea.nouspiro.space/nou/libXISF")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fz9mmj1nz5v7hlr53q8na7khadfn1hm0d1gfpzzw3167bqpy2xv"))))
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
     "LibXISF is C++ library that can read and write XISF files produced by
PixInsight.  It implements XISF 1.0 specification.")
    (license license:gpl3+)))

(define-public missfits
  (package
    (name "missfits")
    (version "2.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/missfits/"
                           "missfits-" version ".tar.gz"))
       (sha256
        (base32 "04jrd7fsvzr14vdmwgj2f6v97gdcfyjyz6jppml3ghr9xh12jxv5"))))
    (build-system gnu-build-system)
    (home-page "https://www.astromatic.net/software/missfits")
    (synopsis "FITS files Maintenance program")
    (description
     "MissFITS is a program that performs basic maintenance and packaging tasks
on FITS files:

@itemize
@item add/edit FITS header keywords
@item split/join Multi-Extension-FITS (MEF) files
@item unpack/pack FITS data-cubes
@item create/check/update FITS checksums, using R. Seaman's protocol
      (see http://www.adass.org/adass/proceedings/adass94/seamanr.html)
@end itemize\n")
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
    (native-inputs
     (list pkg-config))
    (inputs
     `(("libx11" ,libx11)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libice" ,libice)
       ("freetype" ,freetype)
       ("pango" ,pango)
       ("giflib" ,giflib)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("netpbm" ,netpbm)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags
       (let ((netpbm (assoc-ref %build-inputs "netpbm")))
         (append (list
                  ;; Give correct path for pnm.h header to configure script
                  (string-append "CPPFLAGS=-I" netpbm "/include/netpbm")
                  ;; no nasa jpl cspice support
                  "--without-cspice" )))))
    (home-page "https://xplanet.sourceforge.net/")
    (synopsis "Planetary body renderer")
    (description
     "Xplanet renders an image of a planet into an X window or file.
All of the major planets and most satellites can be drawn and different map
projections are also supported, including azimuthal, hemisphere, Lambert,
Mercator, Mollweide, Peters, polyconic, orthographic and rectangular.")
    (license license:gpl2+)))

(define-public gpredict
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
    (native-inputs
     `(("intltool" ,intltool)
       ("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (inputs
     (list curl glib goocanvas gtk+))
    (arguments
     `(#:configure-flags '("CFLAGS=-O2 -g -fcommon")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Remove reference to non-existent file.
             (substitute* "po/POTFILES.in"
               (("src/gtk-sat-tree\\.c")
                ""))
             #t)))))
    (synopsis "Satellite tracking and orbit prediction application")
    (description
     "Gpredict is a real-time satellite tracking and orbit prediction
application.  It can track a large number of satellites and display their
position and other data in lists, tables, maps, and polar plots (radar view).
Gpredict can also predict the time of future passes for a satellite, and
provide you with detailed information about each pass.")
    (home-page "http://gpredict.oz9aec.net/index.php")
    (license license:gpl2+)))

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
     `(#:tests? #f                      ;no test provided
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p "build")
             (chdir "build")
             (invoke
              "cmake"
              "-G" "Unix Makefiles"
              "-DCMAKE_BUILD_TYPE=Release"
              (string-append "-DCMAKE_INSTALL_PREFIX=" (assoc-ref outputs "out"))
              ".."))))))
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

(define-public indi
  (package
    (name "indi")
    (version "1.9.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/indilib/indi")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vfcas59nlw8v7n6qhxhcm4isf5wk0crip5rmsallq3bsv3zznfr"))))
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

(define-public python-jplephem
  (package
    (name "python-jplephem")
    (version "2.17")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "jplephem" version))
       (sha256
        (base32 "09xaibxnwbzzs3x9g3ibqa2la17z3r6in93321glh02dbibfbip1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key tests? inputs outputs #:allow-other-keys)
             (when tests?
               (let ((out (assoc-ref outputs "out")))
                 (add-installed-pythonpath inputs outputs)
                 (setenv "PATH" (string-append out "/bin:" (getenv "PATH")))
                 (invoke "python" "-m" "unittest" "discover" "-s" "test"))))))))
    (inputs
     (list python-numpy))
    (home-page "https://github.com/brandon-rhodes/python-jplephem")
    (synopsis "Python version of NASA DE4xx ephemerides")
    (description
     "The package is a Python implementation of the mathematics that standard
JPL ephemerides use to predict raw (x,y,z) planetary positions.")
    (license license:expat)))

(define-public python-pyerfa
  (package
    (name "python-pyerfa")
    (version "2.0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pyerfa" version))
       (sha256
        (base32 "0c6y1rm51kj8ahbr1vwbswck3ix77dc3zhc2fkg6w7iczrzn7m1g"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Remove bundled submodule library.
           (delete-file-recursively "liberfa")
           #t))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-liberfa
           (lambda _
             (setenv "PYERFA_USE_SYSTEM_LIBERFA" "1"))))))
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
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pynbody" version))
       (sha256
        (base32 "1jxwk2s4qz1znvyak2lj7ld01kl1jh87xp81ki7a8dz1gcy93fkx"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-tests-require-testdata
                 (lambda _
                   ;; Disable tests which need to download additional 1.0GiB+
                   ;; of test data archive from
                   ;; http://star.ucl.ac.uk/~app/testdata.tar.gz
                   ;;    https://github.com/pynbody/pynbody/blob/ \
                   ;;    f4bd482dc47532831b3ec115c7cb07149d61bfc5/ \
                   ;;    .github/workflows/build-test.yaml#L41
                   (with-directory-excursion "tests"
                     (for-each delete-file
                               '("gravity_test.py"
                                 "adaptahop_test.py"
                                 "ahf_halos_test.py"
                                 "array_test.py"
                                 "bridge_test.py"
                                 "family_test.py"
                                 "partial_tipsy_test.py"
                                 "snapshot_test.py"
                                 "test_profile.py"
                                 "gadget_test.py"
                                 "gadgethdf_test.py"
                                 "grafic_test.py"
                                 "halotools_test.py"
                                 "nchilada_test.py"
                                 "ramses_new_ptcl_format_test.py"
                                 "ramses_test.py"
                                 "rockstar_test.py"
                                 "sph_image_test.py"
                                 "sph_smooth_test.py"
                                 "subfind_test.py"
                                 "subfindhdf_gadget4_test.py"
                                 "tipsy_test.py")))))
               (replace 'check
                 (lambda* (#:key tests? inputs outputs #:allow-other-keys)
                   (when tests?
                     (add-installed-pythonpath inputs outputs)
                     (setenv "HOME" "/tmp")
                     (invoke "pytest" "-vv")))))))
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

(define-public python-sep
  (package
    (inherit libsep)
    (name "python-sep")
    (build-system python-build-system)
    (arguments
     (strip-keyword-arguments
      '(#:make-flags) (package-arguments libsep)))
    (native-inputs
     (modify-inputs (package-inputs libsep)
       (prepend python-cython)))
    (propagated-inputs
     (modify-inputs (package-inputs libsep)
       (prepend python-numpy)))))

(define-public python-suntime
  (package
    (name "python-suntime")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "suntime" version))
              (sha256
               (base32
                "0ja64w293wjx76q3z18ikx4rdihwkbj6pnjpbs82ycyyzhfnbpz4"))))
    (build-system python-build-system)
    (propagated-inputs (list python-dateutil))
    (home-page "https://github.com/SatAgro/suntime")
    (synopsis "Sunset and sunrise time calculation python library")
    (description
     "Python library doing sunrise and sunset time calculation.

Takes a WGS84 (GPS) latitude/longitude as input as well as an UTC or local
datetime object.")
    (license license:lgpl3+)))

(define-public python-asdf
  (package
    (name "python-asdf")
    (version "2.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf" version))
       (sha256
        (base32 "1zixzv4n2fryaszsfchqh2nvp0gzvarhz03fc721yw6iafdadqij"))))
    (build-system pyproject-build-system)
    (arguments
     ;; FIXME: Tests fail a lot with
     ;;
     ;; ERROR  - _pytest.pathlib.ImportPathMismatchError:
     ;; ('asdf.conftest', '/gnu/sto...
     ;;
     `(#:tests? #f))
    (native-inputs
     (list python-astropy
           python-packaging
           python-psutil
           python-pytest
           python-pytest-doctestplus
           python-pytest-openfiles
           python-pytest-remotedata
           python-semantic-version
           python-setuptools-scm))
    (propagated-inputs
     (list python-asdf-standard
           python-asdf-transform-schemas
           python-asdf-unit-schemas
           python-importlib-metadata
           python-importlib-resources
           python-jmespath
           python-jsonschema
           python-lz4
           python-numpy
           python-pyyaml))
    (home-page "https://github.com/asdf-format/asdf")
    (synopsis "Python tools to handle ASDF files")
    (description
     "The Advanced Scientific Data Format (ASDF) is a next-generation
interchange format for scientific data.  This package contains the Python
implementation of the ASDF Standard.")
    (license license:bsd-3)))

(define-public python-asdf-standard
  (package
    (name "python-asdf-standard")
    (version "1.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_standard" version))
       (sha256
        (base32
         "0i7xdjwn5prg2hcnf1zhw57mszc68jjr5sv4rimpzcg7f2dgzn5g"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'check 'remove-blocking-tests
                          (lambda _
                            ;; Remove tests require python-asdf where
                            ;; python-asdf require python-asdf-standard,
                            ;; break circular dependencies.
                            (for-each delete-file
                                      (list "tests/test_manifests.py"
                                            "tests/test_integration.py")))))))
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
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_transform_schemas" version))
       (sha256
        (base32 "1midgn575970p5cnsh9y6bz77fjr392b5nfxb3z0id6c49xzzwhc"))))
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
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_coordinates_schemas" version))
       (sha256
        (base32 "0ahwhsz5jzljnpkfd2kvspirg823lnj5ip9sfkd9cx09z1nlz8jg"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs
     (list python-pytest
           python-semantic-version
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
  (let ((commit "572bb370d777f3a325b25c1af9d76e1b7d27dcea")
        (revision "0"))
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
           "1yqnzd0gcrdfl0jqm8m8kz5fd36i8lgh7xkglmp1chsi1cc6mkz2"))))
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
  (let ((commit "e9174083d9cfd3c6f7ded9eeb360d99ccb8d9d18")
        (revision "2"))
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
           "1i40hcxp8sds2zq939fwczjlshfqb9r9pnzy3a44c3wqdbwhcbdb"))))
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
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_unit_schemas" version))
       (sha256
        (base32
         "16grpx3a9h0v1wirp0zqrfsxm867v5c0xyr98pylzziy45kqvds2"))))
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

(define-public python-asdf-astropy
  (package
    (name "python-asdf-astropy")
    (version "0.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_astropy" version))
       (sha256
        (base32 "1gp5iav0a9g9q0zb22vhzi3v9vwk5wn2nxvr3mvi3bsdcdj3h23v"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-coverage
           python-h5py
           python-matplotlib
           python-pandas
           python-pytest-astropy
           python-scipy
           python-semantic-version
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

(define python-asdf-wcs-schemas
  (package
    (name "python-asdf-wcs-schemas")
    (version "0.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "asdf_wcs_schemas" version))
       (sha256
        (base32 "0khyab9mnf2lv755as8kwhk3lqqpd3f4291ny3b9yp3ik86fzhz1"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda* (#:key inputs outputs tests? #:allow-other-keys)
             (when tests?
               (add-installed-pythonpath inputs outputs)
               (invoke "python" "-m" "pytest")))))))
    (native-inputs
     (list python-pytest
           python-setuptools-scm
           python-semantic-version))
    (propagated-inputs
     (list python-asdf))
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
    (version "0.18.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "gwcs" version))
       (sha256
        (base32 "0v9qcq6zl74d6s882s6xmas144jfalvll6va8rvrxmvpx4vqjzhg"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-jsonschema
           python-jmespath
           python-pytest
           python-pytest-doctestplus
           python-pyyaml
           python-semantic-version
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

(define-public python-astroalign
  (package
    (name "python-astroalign")
    (version "2.3.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "astroalign" version))
       (sha256
        (base32 "19qzv3552lgrd9qmj0rxs51wmx485hw04cbf76ds5pin85kfaiy1"))))
    (build-system python-build-system)
    (arguments
     ;; TODO: (Sharlatan-20210213T162940+0000): I could not make tests run
     `(#:tests? #f))
    (inputs
     `(("numpy" ,python-numpy)
       ("scikit-image" ,python-scikit-image)
       ("scipy" ,python-scipy)
       ("sep" ,python-sep)))
    (home-page "https://astroalign.readthedocs.io/")
    (synopsis "Astrometric Alignment of Images")
    (description
     "ASTROALIGN is a python module that will try to align two stellar
astronomical images, especially when there is no WCS information available.")
    (license license:expat)))

(define-public python-skyfield
  (package
    (name "python-skyfield")
    (version "1.39")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "skyfield" version))
       (sha256
        (base32 "1qh3k7g9dm6idppk87hnwxpx9a22xx98vav0zk31p6291drak3as"))))
    (build-system python-build-system)
    (arguments
     ;; NOTE: (Sharlatan-20210207T163305+0000): tests depend on custom test
     ;; framework https://github.com/brandon-rhodes/assay
     `(#:tests? #f))
    (inputs
     (list python-certifi python-jplephem python-numpy python-sgp4))
    (home-page "https://rhodesmill.org/skyfield/")
    (synopsis "Astronomy for Python")
    (description
     "Skyfield computes positions for the stars, planets, and satellites in
orbit around the Earth.")
    (license license:expat)))
