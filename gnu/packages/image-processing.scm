;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017, 2019, 2022, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2014, 2021-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2018 Lprndn <guix@lprndn.info>
;;; Copyright © 2019, 2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021, 2024, 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Brendan Tildesley <mail@brendan.scot>
;;; Copyright © 2021 Oleh Malyi <astroclubzp@gmail.com>
;;; Copyright © 2021, 2022, 2024 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Andy Tai <atai@atai.org>
;;; Copyright © 2021 Ekaitz Zarraga <ekaitz@elenq.tech>
;;; Copyright © 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2022, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022 Tomasz Jeneralczyk <tj@schwi.pl>
;;; Copyright © 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2023 Cairn <cairn@pm.me>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Jake Forster <jakecameron.forster@gmail.com>
;;; Copyright © 2025 Anderson Torres <anderson.torres.8519@gmail.com>
;;; Copyright © 2025 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu packages image-processing)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system qt)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages game-development)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gimp)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public bart
  (package
    (name "bart")
    (version "0.9.00")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mrirecon/bart")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mj6jmw31rsnvqmpfqahhj4cy9iv5xgrhzmcsrikdz5dgd45lmjz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "utest"
      #:make-flags #~(list
                      (string-append "PREFIX=" #$output)
                      "PARALLEL=1"
                      "PARALLEL_NJOBS=1"
                      "OPENBLAS=1"
                      "SCALAPACK=1"
                      (string-append "BLAS_BASE=" #$(this-package-input "openblas"))
                      (string-append "CC=" #$(cc-for-target))
                      (string-append "FFTW_BASE=" #$(this-package-input "fftw")))
      #:parallel-build? #false ;leads to non-deterministic output
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-/bin/bash
            (lambda _
              (substitute* "tests/pics.mk"
                (("/bin/bash") (which "bash"))))))))
    (inputs
     (list fftw
           fftwf
           libpng
           openblas
           python
           scalapack))
    (native-inputs
     (list doxygen
           util-linux)) ;for flock
    (home-page "https://mrirecon.github.io/bart/")
    (synopsis "Toolbox for computational magnetic resonance imaging")
    (description "The Berkeley Advanced Reconstruction Toolbox (BART) is an
image-reconstruction framework for Computational Magnetic Resonance Imaging.
The tools in this software implement various reconstruction algorithms for
Magnetic Resonance Imaging.")
    (license license:bsd-3)))

(define-public cimg
  (package
    (name "cimg")
    (version "3.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/GreycLab/CImg")
             (commit (string-append "v." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01myf3bjbc46f977r7lvr9g1hcnpfygcv2xnqvhrl2nj7955sm5x"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "examples"
                  ;; This is from upstream CI but parallelized.
                  (invoke "make" "-j"
                          (number->string (parallel-job-count)) "mlinux")
                  (invoke "./image2ascii")
                  (invoke "./generate_loop_macros")
                  ;; Build an example that requires an external library.
                  ;; Running it requires a display.
                  (invoke "make" "use_jpeg_buffer"
                          (string-append "CONF_CFLAGS="
                                         (string-join '("-Dcimg_display=0"
                                                        "-O3"
                                                        "-mtune=generic"
                                                        "-Dcimg_use_jpeg")
                                                      " "))
                          "CONF_LIBS=-ljpeg"))))))
      #:install-plan
      #~'(("CImg.h" "include/")
          ("plugins" "include/CImg/plugins"))))
    (native-inputs (list libjpeg-turbo)) ;for 'check
    (home-page "https://cimg.eu")
    (synopsis "Small C++ image processing library")
    (description
     "The @acronym{CImg, Cool Image} Library is a small C++ toolkit for
image processing.  It is made of a single header file @code{CImg.h}
that can be compiled using a minimal set of standard C++ and system
libraries.  It includes a plugin mechanism to extend its functionality
with external tools and libraries.")
    ;; Dual-licensed, either license applies.
    (license (list license:cecill-c license:cecill))))

(define-public dcmtk
  (package
    (name "dcmtk")
    (version "3.6.8")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "ftp://dicom.offis.de/pub/dicom/offis/software/dcmtk/"
                       "dcmtk" (string-join (string-split version #\.) "")
                       "/dcmtk-" version ".tar.gz"))
       (sha256
        (base32 "03vjv2lq5kr79ghf8v0q9wskkrcr2ygi097nybmqs4q3amjpc813"))))
    (build-system cmake-build-system)
    (arguments
     ;; By default, only static archives are built.
     (list #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON")))
    (inputs
     (list icu4c
           libjpeg-turbo
           libpng
           libtiff
           libxml2
           openssl
           zlib))
    (native-inputs
     (list doxygen))           ; for HTML documentation
    (home-page "https://dcmtk.org")
    (synopsis "Libraries and programs implementing parts of the DICOM standard")
    (description "DCMTK is a collection of libraries and applications
implementing large parts the DICOM standard.  It includes software for
examining, constructing and converting DICOM image files, handling offline
media, sending and receiving images over a network connection, as well as
demonstrative image storage and worklist servers.")
    (license (license:fsf-free
              "file://COPYRIGHT"
              "A union of the Apache 2.0 licence and various non-copyleft
licences similar to the Modified BSD licence."))))

(define-public opencolorio
  (package
    (name "opencolorio")
    (version "2.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/AcademySoftwareFoundation/OpenColorIO")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1h5n1adm7hlblq4z9qdbnh5jy3f6aimlk96cn1k4ah3f8vqx7zpq"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     ;; XXX: GPU tests are failing.
     (list #:configure-flags #~(list "-DOCIO_BUILD_GPU_TESTS=false")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'disable-test
                 ;; The approach is somewhat desperate and removes not only
                 ;; line 1436 of the failing
                 ;; FileFormatCTF/difficult_xml_unknown_elements test,
                 ;; but also the identical line 1524 of the succeeding
                 ;; FileFormatCTF/unknown_elements test.
                 (lambda _
                   (substitute* "tests/cpu/fileformats/FileFormatCTF_tests.cpp"
                     (("OCIO_CHECK_NE.*ErrorOutputs.*")
                      ""))))
               (add-after 'unpack 'fix-broken-test
                 (lambda _
                   ;; Replace the invalid multi-line list. It is unclear why
                   ;; this is now failing after only build system updates.
                   ;; (see https://github.com/AcademySoftwareFoundation/OpenColorIO/blob/v2.4.2/tests/cpu/Config_tests.cpp#L6227)
                   (substitute* "tests/cpu/Config_tests.cpp"
                     (("cs1\\\\t\\\\n   \\\\n,   \\\\ncs2")
                      "cs1, cs2")))))))
    (native-inputs
     ;; XXX: OCIO has unit tests for OpenShadingLanguage, but they fail.
     ;; They also require OIIO, but OCIO is an optional dependency to it.
     (list pybind11-2.10 python-wrapper))
    (inputs
     (list expat
           freeglut
           glew
           imath
           lcms
           libglvnd
           ;; opencolorio@2.4.2 fails to build with minizip-ng@4.0.10
           minizip-ng-4.0.9
           openexr
           pystring
           yaml-cpp
           zlib))
    (home-page "https://opencolorio.org")
    (synopsis "Color management for visual effects and animation")
    (description
     "OpenColorIO, or OCIO, is a complete color management solution geared
towards motion picture production, with an emphasis on visual effects and
computer animation.  It provides a straightforward and consistent user
experience across all supporting applications while allowing for sophisticated
back-end configuration options suitable for high-end production usage.

OCIO is compatible with the @acronym{ACES, Academy Color Encoding
Specification} and is @acronym{LUT, look-up table}-format agnostic, supporting
many popular formats.")
    (license license:bsd-3)))

(define-public vtk
  (package
    (name "vtk")
    (version "9.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://vtk.org/files/release/"
                                  (version-major+minor version)
                                  "/VTK-" version ".tar.gz"))
              (sha256
               (base32
                "1f6ac40db7wkb3bfh31d71qc5gy44cw29r6v4cyxrlm09q4fqm43"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each
                   (lambda (dir)
                     (delete-file-recursively
                      (string-append "ThirdParty/" dir "/vtk" dir)))
                   ;; pugixml depended upon unconditionally
                   '("doubleconversion" "eigen" "expat" "freetype" "gl2ps"
                     "glew" "hdf5" "jpeg" "jsoncpp" "libharu" "libproj"
                     "libxml2" "lz4" "netcdf" "ogg" "png" "sqlite" "theora"
                     "tiff" "zlib"))
                  (substitute* "IO/ExportPDF/vtkPDFContextDevice2D.cxx"
                    (("\\bHPDF_UINT16 (noPen|dash|dot|denseDot|dashDot|dashDotDot)\\b"
                      _ var)
                     (string-append "HPDF_REAL " var)))))))
    (properties `((release-monitoring-url . "https://vtk.org/download/")))
    (build-system cmake-build-system)
    (arguments
     (list #:build-type "Release"           ;Build without '-g' to save space.
           #:configure-flags
           #~'("-DVTK_USE_EXTERNAL=OFF"           ;default
               "-DVTK_MODULE_USE_EXTERNAL_VTK_doubleconversion=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_eigen=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_expat=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_freetype=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_gl2ps=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_glew=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_hdf5=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_jpeg=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_jsoncpp=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_libharu=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_libproj=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_libxml2=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_lz4=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_netcdf=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_ogg=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_png=ON"
               ;;"-DVTK_MODULE_USE_EXTERNAL_VTK_pugixml=ON" ;breaks IO/CityGML
               "-DVTK_MODULE_USE_EXTERNAL_VTK_sqlite=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_theora=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_tiff=ON"
               "-DVTK_MODULE_USE_EXTERNAL_VTK_zlib=ON"
               "-DVTK_MODULE_ENABLE_VTK_RenderingExternal=YES" ;for F3D
               "-DVTK_MODULE_ENABLE_VTK_GUISupportQt=YES"      ;for itk-snap
               "-DVTK_WRAP_PYTHON=ON"
               "-DVTK_PYTHON_VERSION:STRING=3"

               "-DVTK_SMP_ENABLE_OPENNMP=ON"
               "-DVTK_SMP_ENABLE_TBB=ON"
               "-DVTK_USE_MPI=ON"
               #$@(if (target-riscv64?)
                    '("-DCMAKE_SHARED_LINKER_FLAGS=-latomic"
                      "-DCMAKE_EXE_LINKER_FLAGS=-latomic")
                    '()))

           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'clear-reference-to-compiler
                 (lambda _
                   (define (choose . files)
                     (let loop ((files files))
                       (if (null? files)
                           #f
                           (if (file-exists? (car files))
                               (car files)
                               (loop (cdr files))))))

                   ;; Do not retain a reference to GCC.
                   (substitute* (choose
                                 "Common/Core/vtkBuild.h.in" ;dummy >=v9.3
                                 "Common/Core/vtkConfigureDeprecated.h.in" ;v9.x
                                 "Common/Core/vtkConfigure.h.in") ;v7.x
                     (("@CMAKE_CXX_COMPILER@") "c++")))))

           #:tests? #f))                          ;XXX: test data not included
    (inputs
     (list double-conversion
           eigen
           expat
           freetype
           gl2ps
           glew
           glu
           hdf5
           libharu
           libjpeg-turbo
           jsoncpp
           libtheora
           libx11
           libxml2
           libxt
           lz4
           mesa
           netcdf
           libpng
           libtiff
           openmpi
           proj
           python
           ;("pugixml" ,pugixml)
           sqlite
           xorgproto
           qtbase
           zlib))
    (propagated-inputs
     ;; VTK's 'VTK-vtk-module-find-packages.cmake' calls
     ;; 'find_package(THEORA)', which in turns looks for libogg.  Likewise for
     ;; TBB.
     (list libogg
           tbb))
    (home-page "https://vtk.org/")
    (synopsis "Libraries for 3D computer graphics")
    (description
     "The Visualization Toolkit (VTK) is a C++ library for 3D computer graphics,
image processing and visualization.  It supports a wide variety of
visualization algorithms including: scalar, vector, tensor, texture, and
volumetric methods; and advanced modeling techniques such as: implicit
modeling, polygon reduction, mesh smoothing, cutting, contouring, and Delaunay
triangulation.  VTK has an extensive information visualization framework, has
a suite of 3D interaction widgets, supports parallel processing, and
integrates with various databases on GUI toolkits such as Qt and Tk.")
    (license license:bsd-3)))

(define-public vktdiff
  (package
    (name "vtkdiff")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ufz/vtkdiff")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15nwzfhgyjfvd083kw1s19xkxcds9h31hx9cr339x3bkllbj609j"))))
    (build-system cmake-build-system)
    (inputs (list tclap vtk))
    (arguments
     (list
      #:tests? #f ;There are no tests.
      #:configure-flags #~(list "-DGUIX_BUILD=ON")))
    (home-page "https://github.com/ufz/vtkdiff")
    (synopsis "Numerical difference of data arrays in vtk files")
    (description
     "The vtkdiff tool shall provide means of numerical comparison of
different data arrays similar to those available in the numdiff software.")
    (license license:bsd-3)))

(define-public opencv
  (package
    (name "opencv")
    (version "4.12.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/opencv/opencv")
                    (commit version)))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove external libraries.  Almost all of them are
                  ;; available in Guix.
                  (with-directory-excursion "3rdparty"
                    (for-each delete-file-recursively
                              '("cpufeatures"
                                "flatbuffers"
                                "ffmpeg"
                                "include"
                                "ippicv"
                                "ittnotify"
                                "libjasper"
                                "libjpeg"
                                "libjpeg-turbo"
                                "libpng"
                                "libtiff"
                                "libwebp"
                                "openexr"
                                "openjpeg"
                                "protobuf"
                                ;;"quirc"
                                "tbb"
                                "zlib")))

                  ;; Delete any bundled .jar files.
                  (for-each delete-file (find-files "." "\\.jar$"))))
              (sha256
               (base32
                "1n5qif1ispmdby1cmwf9f8isdx07phyhb19jsmgrw5bk3k23dcyy"))))
    (build-system cmake-build-system)
    (arguments
     `(#:parallel-tests? #f
       #:configure-flags
       (list "-DWITH_ADE=OFF"           ;we don't have a package for ade yet
             "-DWITH_IPP=OFF"
             "-DWITH_ITT=OFF"
             "-DWITH_CAROTENE=OFF"      ; only visible on arm/aarch64
             "-DENABLE_PRECOMPILED_HEADERS=OFF"
             "-DOPENCV_GENERATE_PKGCONFIG=ON"

             ;; CPU-Features:
             ;; See cmake/OpenCVCompilerOptimizations.cmake
             ;; (CPU_ALL_OPTIMIZATIONS) for a list of all optimizations
             ;; BASELINE is the minimum optimization all CPUs must support
             ;;
             ;; DISPATCH is the list of optional dispatches.
             "-DCPU_BASELINE=SSE2"

             ;; Build Python bindings.
             "-DBUILD_opencv_python3=ON"

             ,@(match (%current-system)
                 ("x86_64-linux"
                  '("-DCPU_DISPATCH=NEON;VFPV3;FP16;SSE;SSE2;SSE3;SSSE3;SSE4_1;SSE4_2;POPCNT;AVX;FP16;AVX2;FMA3;AVX_512F;AVX512_SKX"
                    "-DCPU_DISPATCH_REQUIRE=SSE3,SSSE3,SSE4_1,SSE4_2,AVX,AVX2"))
                 ("armhf-linux"
                  '("-DCPU_BASELINE_DISABLE=NEON")) ; causes build failures
                 ("aarch64-linux"
                  '("-DCPU_BASELINE=NEON"
                    "-DCPU_DISPATCH=NEON;VFPV3;FP16"))
                 (_ '()))

             "-DBUILD_PERF_TESTS=OFF"
             "-DBUILD_TESTS=ON"

             (string-append "-DOPENCV_EXTRA_MODULES_PATH=" (getcwd)
                            "/opencv-contrib/modules")

             ;;Define test data:
             (string-append "-DOPENCV_TEST_DATA_PATH=" (getcwd)
                            "/opencv-extra/testdata")

             ;; Is ON by default and would try to rebuild 3rd-party protobuf,
             ;; which we had removed, which would lead to an error:
             "-DBUILD_PROTOBUF=OFF"

             ;; OpenCV tries to use flatbuffers in 3rdparty which we removed
             ;; so for now we don't buildfor  flatbuffer support
             ;; TODO: make OpenCV use system flatbuffers which involves
             ;; modifying CMake files
             "-DWITH_FLATBUFFERS=OFF"

             ;; Rebuild protobuf files, because we have a slightly different
             ;; version than the included one. If we would not update, we
             ;; would get a compile error later:
             "-DPROTOBUF_UPDATE_FILES=ON"

             ;; For xfeatures2d.
             "-DOPENCV_SKIP_FEATURES2D_DOWNLOADING=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             ;; This test fails with "unknown file: Failure"
             ;; But I couldn't figure out which file was missing:
             (substitute* "../opencv-contrib/modules/face/test/test_face_align.cpp"
               (("\\bcan_detect_landmarks\\b" all)
                (string-append "DISABLED_" all)))

             ;; This all fails with a comparison between the expected 396 and
             ;; the actual 440 in file size.
             (substitute* "modules/imgcodecs/test/test_exr.impl.hpp"
               (("\\breadWrite_32FC1\\b" all)
                (string-append "DISABLED_" all)))

             ;; These fail with protobuf parse errors that come from
             ;; opencv-extra/alldata.
             (substitute* "modules/dnn/test/test_layers.cpp"
               (("\\b(Accum|DataAugmentation|Resample|Correlation|Interp)\\b" all)
                (string-append "DISABLED_" all)))

             ;; This test fails on x86-64, loosen the bounds.
             ;; Expected: (max) < (0.1), actual: 0.2 vs 0.1
             (substitute* "modules/photo/test/test_hdr.cpp"
                (("0\\.1\\)") "0.222)"))

             ,@(if (target-aarch64?)
                 `(;; This test fails on aarch64, loosen the bounds.
                   ;; Expected: (max) < (0.131), actual: 0.207148 vs 0.131
                   (substitute* "modules/photo/test/test_hdr.cpp"
                     (("0\\.131") "0.222"))
                   ;; These tests hang forever on aarch64.
                   (delete-file-recursively "modules/videoio/test/"))
                 '())

             ,@(if (target-riscv64?)
                 `(;; This test fails on riscv64, loosen the bounds.
                   ;; Expected: (max) < (0.1), actual: 0.220829 vs 0.1
                   (substitute* "modules/photo/test/test_hdr.cpp"
                     (("0\\.1") "0.240"))
                   ;; Expected equality of these values:
                   ;;   ellipses.size()
                   ;;     Which is: 668
                   ;;   ellipses_size
                   ;;     Which is: 2449
                   (substitute* "../opencv-contrib/modules/ximgproc/test/test_fld.cpp"
                     (("\\bManySmallCircles\\b" all)
                      (string-append "DISABLED_" all))))
                 '())))
         (add-after 'unpack 'unpack-submodule-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir "../opencv-extra")
             (mkdir "../opencv-contrib")
             (copy-recursively (assoc-ref inputs "opencv-extra")
                               "../opencv-extra")
             (copy-recursively (assoc-ref inputs "opencv-contrib")
                               "../opencv-contrib")

             ;; Disable downloads of Boost and VGG descriptors as we include
             ;; them in native inputs.
             (substitute* "../opencv-contrib/modules/xfeatures2d/CMakeLists.txt"
               (("download_(boost|vgg)_descriptors") "#"))
             (copy-recursively (assoc-ref inputs "opencv-3rdparty-boost")
                               "../downloads/xfeatures2d")
             (for-each make-file-writable
                       (find-files "../downloads/xfeatures2d" "."))
             (copy-recursively (assoc-ref inputs "opencv-3rdparty-vgg")
                               "../downloads/xfeatures2d")))
         (add-after 'build 'do-not-install-3rdparty-file
           (lambda _
             (substitute* "cmake_install.cmake"
               (("file\\(INSTALL .*3rdparty/include/opencl/LICENSE.txt.*")
                ""))))
         (add-before 'check 'start-xserver
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((xorg-server (assoc-ref inputs "xorg-server"))
                   (disp ":1"))
               (setenv "HOME" (getcwd))
               (setenv "DISPLAY" disp)
               ;; There must be a running X server and make check doesn't start one.
               ;; Therefore we must do it.
               (zero? (system (format #f "~a/bin/Xvfb ~a &" xorg-server disp)))))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("xorg-server" ,xorg-server-for-tests) ;For running the tests
       ;; These are files that are derived from the binary descriptors that
       ;; are part of the BinBoost package.  The BinBoost package is released
       ;; under GPLv2+.  See
       ;; https://www.epfl.ch/labs/cvlab/research/descriptors-and-keypoints/research-detect-binboost/
       ;; See xfeatures2d/cmake/download_boostdesc.cmake for commit hash.
       ("opencv-3rdparty-boost"
        ,(let ((commit "34e4206aef44d50e6bbcd0ab06354b52e7466d26"))
           (origin
             (method git-fetch)
             (uri (git-reference (url "https://github.com/opencv/opencv_3rdparty")
                                 (commit commit)))
             (file-name (git-file-name "opencv_3rdparty" commit))
             (sha256
              (base32
               "13yig1xhvgghvxspxmdidss5lqiikpjr0ddm83jsi0k85j92sn62")))))
       ;; These are the Visual Geometry Group descriptors, released under
       ;; BSD-3.  They are generated files produced by the DLCO framework.
       ;; See xfeatures2d/cmake/download_vgg.cmake for commit hash.
       ("opencv-3rdparty-vgg"
        ,(let ((commit "fccf7cd6a4b12079f73bbfb21745f9babcd4eb1d"))
           (origin
             (method git-fetch)
             (uri (git-reference (url "https://github.com/opencv/opencv_3rdparty")
                                 (commit commit)))
             (file-name (git-file-name "opencv_3rdparty" commit))
             (sha256
              (base32
               "0r9fam8dplyqqsd3qgpnnfgf9l7lj44di19rxwbm8mxiw0rlcdvy")))))
       ("opencv-extra"
        ,(origin
           (method git-fetch)
           (uri (git-reference
                 (url "https://github.com/opencv/opencv_extra")
                 (commit version)))
           (file-name (git-file-name "opencv_extra" version))
           (sha256
            (base32
             "0awang2hl7z194rlnic38ry6fgxy8b7k2qqasinjqznxab4dkhvz"))))
       ("opencv-contrib"
        ,(origin
           (method git-fetch)
           (uri (git-reference (url "https://github.com/opencv/opencv_contrib")
                               (commit version)))
           (file-name (git-file-name "opencv_contrib" version))
           (sha256
            (base32
             "1ai1kanlq1wr3173q86yfj6r9h3kkvjx18cakjr31jkb25qyrmny"))))))
    (inputs
     (list eigen
           ffmpeg-4
           ;; TODO: add gstreamer
           gtk+
           hdf5
           imath                        ;should be propagated by openexr
           jasper
           libgphoto2
           libjpeg-turbo
           libpng
           libtiff
           libwebp
           openblas
           opencl-headers
           openexr
           openmpi
           openjpeg
           protobuf
           python
           python-numpy
           vtk
           zlib))
    ;; These three CVEs are not a problem of OpenCV, see:
    ;; https://github.com/opencv/opencv/issues/10998
    (properties '((lint-hidden-cve . ("CVE-2018-7712"
                                      "CVE-2018-7713"
                                      "CVE-2018-7714"))))
    (synopsis "Computer vision library")
    (description "OpenCV is a library aimed at real-time computer vision,
including several hundred computer vision algorithms.  It can be used to do
things like:

@itemize
@item image and video input and output
@item image and video processing
@item displaying
@item feature recognition
@item segmentation
@item facial recognition
@item stereo vision
@item structure from motion
@item augmented reality
@item machine learning
@end itemize\n

This package includes the Python bindings for OpenCV, which are also known as
the OpenCV-Python library.")
    (home-page "https://opencv.org/")
    (license license:bsd-3)))

(define-public vips
  (package
    (name "vips")
    (version "8.17.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libvips/libvips")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rdz2mmivr02s7cksqgbramv37yw95w6s8r20hgjqx36in2ndnbl"))))
    (build-system meson-build-system)
    (native-inputs
     (list `(,glib "bin")
           gobject-introspection
           pkg-config))
    (inputs
     (list glib
           hdf5
           imagemagick
           poppler))
    ;; Propagated to satisfy vips.pc.
    (propagated-inputs
     (list expat
           fftw
           lcms
           libexif
           libgsf
           libjpeg-turbo
           libpng
           (librsvg-for-system)
           libtiff
           libxml2-next
           libwebp
           matio
           openexr
           orc
           pango))
    (home-page "https://www.libvips.org/")
    (synopsis "Multithreaded image processing system with low memory needs")
    (description
     "VIPS is a demand-driven, horizontally threaded image processing library.
It's particularly good at processing large images, working with colour,
scientific analysis, and general research & development.

Compared to most image processing libraries VIPS needs little RAM and runs
quickly, especially on machines with more than one CPU core.  This is primarily
due to its architecture which automatically parallelises the image workflows.")
    (license license:lgpl2.1+)))

(define-public gmic
  (package
    (name "gmic")
    (version "3.5.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://gmic.eu/files/source/gmic_"
                           version ".tar.gz"))
       (sha256
        (base32 "1wyzq5g8kfnglb5l8ipzh6lhf4667vmcd88gd9vf55fdnvdrjygp"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;there are no tests
      #:configure-flags #~(list "-DBUILD_LIB_STATIC=OFF"
                                "-DENABLE_DYNAMIC_LINKING=ON"
                                (string-append "-DCMAKE_EXE_LINKER_FLAGS="
                                               "-Wl,-rpath=" #$output "/lib")
                                "-DENABLE_LTO=ON")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list curl
           fftw
           graphicsmagick
           imath
           libjpeg-turbo
           libpng
           libtiff
           libx11
           openexr
           zlib))
    (home-page "https://gmic.eu/")
    (synopsis "Full-featured framework for digital image processing")
    (description "G'MIC is a full-featured framework for digital image
processing.  It provides several user interfaces to convert / manipulate
/ filter / visualize generic image datasets, ranging from 1D scalar
signals to 3D+t sequences of multi-spectral volumetric images, hence
including 2D color images.")
    ;; Dual-licensed, either license applies.
    (license (list license:cecill license:cecill-c))))

(define-public gmic-qt
  (package
    (inherit gmic)
    (name "gmic-qt")
    (arguments
     (substitute-keyword-arguments (package-arguments gmic)
       ((#:configure-flags _)
        #~(list "-DGMIC_QT_HOST=none"
                "-DENABLE_DYNAMIC_LINKING=ON"))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'qt-chdir
              (lambda _ (chdir "gmic-qt")))))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (modify-inputs (package-inputs gmic)
       (prepend gmic qtbase-5)))
    (synopsis "Qt frontend for the G'MIC image processing framework")
    (license license:gpl3+)))

(define-public gmic-qt-gimp
  (package
    (inherit gmic-qt)
    (name "gmic-qt-gimp")
    (inputs
     ;; GIMP and its dependencies.
     (modify-inputs (package-inputs gmic-qt)
       (prepend gexiv2 gdk-pixbuf gegl gimp pango)))
    (arguments
     (substitute-keyword-arguments (package-arguments gmic-qt)
       ((#:configure-flags flags)
        #~(list "-DGMIC_QT_HOST=gimp3"
                "-DENABLE_DYNAMIC_LINKING=ON"))))
    (synopsis "GIMP plugin for the G'MIC image processing framework")))

(define-public nip2
  (package
    (name "nip2")
    (version "8.9.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/libvips/nip2/releases/download/v"
                           version "/nip2-" version ".tar.gz" ))
       (sha256
        (base32 "1kbndn37m0cfjvrvw25b5if9d962r8v79q78i2lzm55r7zmjcpmp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'set-home
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (inputs (list gsl gtk+-2 vips))
    (native-inputs (list bison flex pkg-config))
    (home-page "https://github.com/libvips/nip2")
    (synopsis "Spreadsheet-like GUI for libvips")
    (description "This package provide a graphical user interface (GUI) for
the VIPS image processing library.  It's a little like a spreadsheet: you
create a set of formula connecting your objects together, and on a change nip2
recalculates.")
    (license license:gpl2+)))

(define-public paraview
  (package
    (name "paraview")
    (version "6.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.kitware.com/paraview/paraview")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m1c7vngrpaqdqvnjx4wj0va20hih5rb7rf0a44mp3wqgp4wgy0f"))
       (modules '((guix build utils)))
       (snippet
        ;; TODO: Also remove unused bundled libraries and plugins?
        #~(begin
            ;; Remove bundled ParaView libraries which are available in Guix
            ;; or undesired.
            (for-each (lambda (dir)
                        (delete-file-recursively
                         (string-append "ThirdParty/" dir "/vtk"
                                        (string-downcase dir))))
                      '(;;"CosmoHaloFinder"
                        ;;"IceT"
                        "NvPipe"        ; Don't want NvPipe support
                        ;;"QtTesting"
                        ;;"cinema"
                        ;;"cinemasci"
                        "protobuf"))
            ;; Remove undesired ParaView plugins.
            (delete-file-recursively "Plugins/pvNVIDIAIndeX")
            ;; Remove bundled VTK libraries which are available in Guix.
            (for-each (lambda (dir)
                        (delete-file-recursively
                         (string-append "VTK/ThirdParty/" dir "/vtk" dir)))
                      '("cgns"
                        "cli11"
                        ;;"diy2"
                        "doubleconversion"
                        "eigen"
                        ;;"exodusII"
                        "expat"
                        ;;"exprtk"
                        ;;"fast-float"
                        ;;"fides"
                        "fmt"
                        "freetype"
                        "gl2ps"
                        ;;"glad"
                        ;;"h5part"
                        "hdf5"
                        ;;"ioss"
                        "jpeg"
                        "jsoncpp"
                        ;;"kissfft"
                        ;;"libharu" ; Requires some PRs applied to 2.3.0
                        "libproj"
                        "libxml2"
                        ;;"loguru"
                        "lz4"
                        "lzma"
                        "mpi4py"
                        "netcdf"
                        "nlohmannjson"
                        "ogg"
                        ;;"pegtl"
                        "png"
                        "pugixml"
                        "sqlite"
                        "theora"
                        "tiff"
                        ;;"token"
                        "utf8"
                        ;;"verdict"
                        ;;"viskores"
                        ;;"vpic"
                        ;;"vtkm"
                        ;;"xdmf2"
                        ;;"xdmf3"
                        "zlib"))))))
    (build-system qt-build-system)
    (arguments
     (list
      #:build-type "Release"            ; 542 MiB in release mode
      #:tests? #f                       ; Downloads test data
      #:configure-flags
      #~(let ((doc (string-append "share/doc/" #$name "-" #$version)))
          (list
           (string-append "-DCMAKE_INSTALL_DOCDIR=" doc) ; For paraview.qch

           ;; ParaView build options
           "-DPARAVIEW_BUILD_DEVELOPER_DOCUMENTATION=ON"
           (string-append "-DPARAVIEW_GENERATED_DOCUMENTATION_OUTPUT_DIRECTORY=" doc)

           ;; ParaView capability options
           ;;"-DPARAVIEW_USE_EXTERNAL_VTK=ON" ; Unsupported by ParaView
           "-DPARAVIEW_USE_MPI=ON"
           "-DPARAVIEW_USE_PYTHON=ON"
           "-DPARAVIEW_USE_QTWEBENGINE=ON"

           ;; ParaView features
           ;;
           ;; Enable those that are disabled by default.
           ;; Commented means the dependencies are missing from Guix
           ;; (or are otherwise described).
           ;;"-DPARAVIEW_ENABLE_ADIOS2=ON"
           ;;"-DPARAVIEW_ENABLE_COSMOTOOLS=ON"
           ;;"-DPARAVIEW_ENABLE_CATALYST=ON"
           "-DPARAVIEW_ENABLE_FFMPEG=ON"
           ;;"-DPARAVIEW_ENABLE_FIDES=ON"
           "-DPARAVIEW_ENABLE_GDAL=ON"
           ;;"-DPARAVIEW_ENABLE_LAS=ON"
           ;;"-DPARAVIEW_ENABLE_LOOKINGGLASS=ON" ; Downloads dependency
           ;;"-DPARAVIEW_ENABLE_MOMENTINVARIANTS=ON" ; Downloads dependency
           "-DPARAVIEW_ENABLE_MOTIONFX=ON"
           ;;"-DPARAVIEW_ENABLE_OPENTURNS=ON"
           ;;"-DPARAVIEW_ENABLE_OPENVDB=ON" ; Dependency not found
           ;;"-DPARAVIEW_ENABLE_PDAL=ON"
           ;;"-DPARAVIEW_ENABLE_RAYTRACING=ON"
           "-DPARAVIEW_ENABLE_VISITBRIDGE=ON"
           "-DPARAVIEW_ENABLE_XDMF3=ON"

           ;; ParaView miscellaneous options
           ;;
           ;; Without -DPARAVIEW_DATA_EXCLUDE_FROM_ALL=OFF, test data is
           ;; downloaded even with tests disabled.
           "-DPARAVIEW_VERSIONED_INSTALL=OFF"
           "-DPARAVIEW_DATA_EXCLUDE_FROM_ALL=OFF"

           ;; ParaView plugins
           ;;
           ;; Enable those that are disabled by default.
           ;; Commented means the dependencies are missing from Guix
           ;; (or are otherwise described).
           ;;"-DPARAVIEW_PLUGIN_ENABLE_AdiosReaderPixie=ON"
           ;;"-DPARAVIEW_PLUGIN_ENABLE_AdiosReaderStaging=ON"
           "-DPARAVIEW_PLUGIN_ENABLE_CAVEInteraction=ON"
           ;;"-DPARAVIEW_PLUGIN_ENABLE_CDIReader=ON"
           "-DPARAVIEW_PLUGIN_ENABLE_GeographicalMap=ON"
           "-DPARAVIEW_PLUGIN_ENABLE_GmshIO=ON"
           "-DPARAVIEW_PLUGIN_ENABLE_InSituExodus=ON"
           ;;"-DPARAVIEW_PLUGIN_ENABLE_LookingGlass=ON"
           "-DPARAVIEW_PLUGIN_ENABLE_NetCDFTimeAnnotationPlugin=ON"
           ;;"-DPARAVIEW_PLUGIN_ENABLE_ParFlow=ON" ; Build fails
           ;;"-DPARAVIEW_PLUGIN_ENABLE_PythonQtPlugin=ON"
           "-DPARAVIEW_PLUGIN_ENABLE_SpaceMouseInteractor=ON"
           ;;"-DPARAVIEW_PLUGIN_ENABLE_VDFReaderPlugin=ON"
           ;;"-DPARAVIEW_PLUGIN_ENABLE_XRInterface=ON" ; Build fails
           ;;"-DPARAVIEW_PLUGIN_ENABLE_zSpace=ON"

           ;; VTK options
           "-DVTK_SMP_IMPLEMENTATION_TYPE=TBB"
           "-DVTKm_ENABLE_MPI=ON"

           ;; External libraries for ParaView and VTK
           "-DVTK_MODULE_USE_EXTERNAL_ParaView_protobuf=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_cgns=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_cli11=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_doubleconversion=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_eigen=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_expat=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_fmt=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_freetype=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_gl2ps=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_hdf5=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_jpeg=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_jsoncpp=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_libproj=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_libxml2=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_lz4=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_lzma=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_mpi4py=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_netcdf=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_nlohmannjson=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_ogg=ON"
           ;;"-DVTK_MODULE_USE_EXTERNAL_VTK_pegtl=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_png=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_pugixml=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_sqlite=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_theora=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_tiff=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_utf8=ON"
           "-DVTK_MODULE_USE_EXTERNAL_VTK_zlib=ON"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'set-paths 'hide-gfortran
            (lambda _
              (setenv "CPLUS_INCLUDE_PATH"
                      (string-join
                       (delete (string-append #$(this-package-native-input "gfortran")
                                              "/include/c++")
                               (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                       ":"))))
          (replace 'install-license-files
            (lambda _
              (let ((src (string-append #$output "/share/licenses/ParaView"))
                    (dst (string-append #$output "/share/doc/"
                                        #$name "-" #$version "/licenses")))
                (copy-recursively src dst)
                (delete-file-recursively (dirname src))))))))
    (native-inputs
     (list gfortran
           ;; For the documentation
           doxygen
           graphviz
           perl
           python-sphinx))
    (inputs
     (list boost
           cgns
           cli11
           curl
           double-conversion
           eigen
           expat
           ffmpeg
           fmt-11
           freetype
           gdal
           gl2ps
           gmsh
           hdf5
           nlohmann-json                ;For ParFlow; build fails
           jsoncpp
           libjpeg-turbo
           libogg
           libpng
           libtheora
           libtiff
           libxcursor
           libxml2
           libxt
           lz4
           lzip
           mesa
           netcdf
           openmpi
           ;;openvdb                      ;For OpenVDB; dependency not found
           ;;openvr                       ;For XRInterface; build fails
           ;;pegtl                        ;For VTK; build fails
           proj
           protobuf
           pugixml
           python-cftime
           python-matplotlib
           python-mpi4py
           python-numpy
           python-wrapper
           qtbase-5
           qtdeclarative-5
           qtmultimedia-5
           qtsvg-5
           qttools-5
           qtwebchannel-5
           qtwebengine-5
           qtx11extras
           qtxmlpatterns-5
           sdl2
           sqlite
           tbb
           utfcpp
           zlib))
    (home-page "https://www.paraview.org/")
    (synopsis "VTK-based, parallel data analyzer and visualizer")
    (description "ParaView is a VTK-based, parallel data analyzer and
visualizer which allows exploring data interactively in 3D or
programmatically.")
    (license license:bsd-3)))

(define-public vxl
  (package
    (name "vxl")
    (version "3.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vxl/vxl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0iqq4lm51l5gvkax6r79ypifqmgir3p3vman9gsc2085d2agjvbs"))
       (modules '((guix build utils)))
       ;; TODO: vxl includes an old version of dcmtk.  It won't build with
       ;; version 3.6.x.
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("v3p/bzlib/"
                       "v3p/geotiff/"
                       "v3p/jpeg/"
                       "v3p/png/"
                       "v3p/tiff/"
                       "v3p/zlib/"))
           (substitute* "v3p/CMakeLists.txt"
             (("add_subdirectory\\((tiff|png|jpeg|zlib|bzlib|geotiff)\\)")
              ""))
           #t))))
    (build-system cmake-build-system)
    (arguments
     ;; Not an actual test, but is incorrectly generated by the build system.
     (list #:test-exclude "vul_vul_test_driver"))
    (inputs
     (list libgeotiff libtiff libjpeg-turbo libpng zlib))
    (home-page "https://github.com/vxl/vxl/")
    (synopsis "Collection of C++ libraries for computer vision")
    (description "VXL (the Vision-something-Libraries) is a collection of C++
libraries designed for computer vision research and implementation.")
    (license license:bsd-3)))

(define-public vxl-1
  (package (inherit vxl)
    (name "vxl")
    (version "1.18.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/vxl/vxl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g4mr2cc58jwm0vasscbd4y5380wj3ahkvq121z4gs83fhavvxgz"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file-recursively
                     '("v3p/bzlib/"
                       "v3p/geotiff/"
                       "v3p/png/"
                       "v3p/tiff/"
                       "v3p/zlib/"))
           (substitute* "v3p/CMakeLists.txt"
             (("add_subdirectory\\((tiff|png|jpeg|zlib|bzlib|geotiff)\\)")
              ""))))))
    (arguments
     (substitute-keyword-arguments (package-arguments vxl)
      ((#:configure-flags flags)
       #~(cons* ;; Needed for itk-snap
                "-DVNL_CONFIG_LEGACY_METHODS=ON"
                "-DCMAKE_CXX_STANDARD=14"
                #$flags))))))

(define-public insight-toolkit
  ;; For information about ITK remote modules, see:
  ;; https://insightsoftwareconsortium.github.io/ITKWikiArchive/Wiki/ITK/Policy_and_Procedures_for_Adding_Remote_Modules
  ;; For a remote MODULE, use the commit in
  ;; 'Modules/Remote/MODULE.remote.cmake'.
  ;; MorphologicalContourInterpolation is required by itk-snap.
  ;; SimpleITKFilters and GenericLabelInterpolator are required by simpleitk.
  ;; GenericLabelInterpolator and AdaptiveDenoising are required by ants.
  (let* ((module-mci-commit "821bf9b3ef8eaaab10391ed060dc9ca5e4d37b39")
         (module-mci-file (git-file-name "ITKMorphologicalContourInterpolation"
                                         module-mci-commit))
         (module-sitkf-commit "bb896868fc6480835495d0da4356d5db009592a6")
         (module-sitkf-file (git-file-name "ITKSimpleITKFilters"
                                           module-sitkf-commit))
         (module-gli-commit "ebf2436469ccf82c08fab54b7446f699ad0eae01")
         (module-gli-file (git-file-name "ITKGenericLabelInterpolator"
                                         module-gli-commit))
         (module-ad-commit "853934c352f83cb1e8f87e3051e1b8e75dbb41fe")
         (module-ad-file (git-file-name "ITKAdaptiveDenoising"
                                        module-ad-commit)))
    (package
      (name "insight-toolkit")
      (version "5.4.4")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/InsightSoftwareConsortium/ITK")
               (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1l5rby8jj8726k380aivycmhn56cz56mr9k3r56c8hkkrfwwng50"))
         ;; This patch is required to build with both ITK_USE_GPU=ON and
         ;; ITK_WRAP_PYTHON=ON.
         ;; <https://github.com/InsightSoftwareConsortium/ITK/pull/4842>
         (patches (search-patches "insight-toolkit-fix-build.patch"))))
      (build-system cmake-build-system)
      (outputs '("out" "python"))
      (arguments
       (list
        #:imported-modules (append %cmake-build-system-modules
                                   %pyproject-build-system-modules)
        #:modules '(((guix build pyproject-build-system) #:prefix py:)
                    (guix build cmake-build-system)
                    (guix build utils))
        #:configure-flags
        #~(list "-DITK_USE_GPU=ON"
                "-DITK_USE_SYSTEM_LIBRARIES=ON"
                "-DITK_USE_SYSTEM_CASTXML=ON"
                "-DITK_USE_SYSTEM_SWIG=ON"
                (string-append "-DHDF5_DIR="
                               #$(this-package-input "hdf5") "/lib/cmake")
                "-DBUILD_SHARED_LIBS=ON"
                ;; Without this flag, there are shared libraries installed
                ;; in PY_SITE_PACKAGES_PATH/itk instead of #$output/lib and
                ;; RUNPATHs contain the *build directory* of
                ;; PY_SITE_PACKAGES_PATH/itk.
                "-DCMAKE_BUILD_WITH_INSTALL_RPATH=ON"
                "-DITK_WRAPPING=ON"
                "-DITK_WRAP_PYTHON=ON"
                "-DITK_DYNAMIC_LOADING=ON"
                (let* ((python-version
                         #$(version-major+minor
                             (package-version
                               (this-package-input "python"))))
                       (python-lib-path (string-append #$output:python
                                                       "/lib/python"
                                                       python-version
                                                       "/site-packages")))
                  (string-append "-DPY_SITE_PACKAGES_PATH="
                                 python-lib-path))
                ;; Python is not built with Py_LIMITED_API.
                "-DITK_USE_PYTHON_LIMITED_API=OFF"
                "-DModule_MorphologicalContourInterpolation=ON"
                "-DModule_SimpleITKFilters=ON"
                "-DModule_GenericLabelInterpolator=ON"
                "-DModule_AdaptiveDenoising:BOOL=ON"
                "-DModule_ITKReview:BOOL=ON"
                "-DCMAKE_CXX_STANDARD=17"
                "-DBUILD_TESTING=OFF")

        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'do-not-tune
              (lambda _
                (substitute* "CMake/ITKSetStandardCompilerFlags.cmake"
                  (("-mtune=native")
                   ""))))
            (add-after 'unpack 'ignore-warnings
              (lambda _
                (substitute* "Wrapping/Generators/Python/CMakeLists.txt"
                  (("-Werror")
                   ""))))
            (add-after 'unpack 'exclude-gtest-target
              (lambda _
                ;; Prevent ITKGoogleTest from being added to
                ;; ITK_MODULES_ENABLED in the installed
                ;; ITKConfig.cmake, which in turn prevents
                ;; 'GTest::GTest' from being added to the
                ;; ITK_LIBRARIES variable.  This is necessary
                ;; because projects that use ITK fail to configure
                ;; otherwise.  Fixes
                ;; <https://codeberg.org/guix/guix/issues/776>.
                ;; <https://github.com/microsoft/vcpkg/pull/27187>
                (substitute* "Modules/ThirdParty/GoogleTest/itk-module.cmake"
                  (("DEPENDS")
                   "DEPENDS\n  EXCLUDE_FROM_DEFAULT"))))
            (add-after 'unpack 'prepare-remote-modules
              (lambda _
                ;; ITK module MorphologicalContourInterpolation
                ;; is for ITK-SNAP.
                (symlink #$(this-package-native-input module-mci-file)
                         "Modules/Remote/MorphologicalContourInterpolation")
                (delete-file
                  (string-append
                    "Modules/Remote/"
                    "MorphologicalContourInterpolation.remote.cmake"))
                ;; ITK modules SimpleITKFilters and GenericLabelInterpolator
                ;; are for SimpleITK.
                (symlink #$(this-package-native-input module-sitkf-file)
                         "Modules/Remote/SimpleITKFilters")
                (delete-file "Modules/Remote/SimpleITKFilters.remote.cmake")
                (symlink #$(this-package-native-input module-gli-file)
                         "Modules/Remote/GenericLabelInterpolator")
                (delete-file
                 "Modules/Remote/GenericLabelInterpolator.remote.cmake")
                (symlink #$(this-package-native-input module-ad-file)
                         "Modules/Remote/AdaptiveDenoising")
                (delete-file
                 "Modules/Remote/AdaptiveDenoising.remote.cmake")))
            (add-after 'unpack 'fix-numpy-bool
              (lambda _
                ;; <https://github.com/InsightSoftwareConsortium/ITK/pull/5402>
                (substitute* (string-append "Wrapping/Generators/"
                                            "Python/itk/support/types.py")
                  (("np\\.bool") "np.bool_"))))
            (delete 'check)    ;tests require network access and external data
            (add-after 'install 'python-sanity-check
              (lambda* (#:key inputs outputs tests? #:allow-other-keys)
                (when tests?
                  (py:add-installed-pythonpath inputs outputs)
                  (invoke "python3" "-c" "import itk")))))))
      (inputs (list eigen
                    expat
                    fftw
                    fftwf
                    hdf5
                    libjpeg-turbo
                    libpng
                    libtiff
                    mesa-opencl
                    perl
                    python
                    tbb
                    vxl-1
                    zlib))
      (native-inputs
       (list castxml
             gcc-13
             git-minimal
             pkg-config
             python-numpy               ;for phase 'python-sanity-check
             swig-next
             which
             (origin
               (method git-fetch)
               (uri
                 (git-reference
                   (url (string-append
                          "https://github.com/KitwareMedical/"
                          "ITKMorphologicalContourInterpolation"))
                   (commit module-mci-commit)))
               (file-name module-mci-file)
               (sha256
                 (base32
                   "00myhgvlk3n062i8bnknz1d10zkv3jlvs7f4jnk24727gd4v2n4i")))
             (origin
               (method git-fetch)
               (uri (git-reference
                      (url (string-append
                            "https://github.com/InsightSoftwareConsortium/"
                            "ITKSimpleITKFilters"))
                      (commit module-sitkf-commit)))
               (file-name module-sitkf-file)
               (sha256
                (base32
                 "13nys94wl4k77f89i8y1dm3y4pmgmw3rrc0la1rzl0vi9h1qixii")))
             (origin
               (method git-fetch)
               (uri (git-reference
                      (url (string-append
                            "https://github.com/InsightSoftwareConsortium/"
                            "ITKGenericLabelInterpolator"))
                      (commit module-gli-commit)))
               (file-name module-gli-file)
               (sha256
                (base32
                 "1khakqh6pzdg6csli8jypzrhcdr9xmhnzgwz265krv8r5mbnndrg")))
             (origin
               (method git-fetch)
               (uri (git-reference
                      (url "https://github.com/ntustison/ITKAdaptiveDenoising")
                      (commit module-ad-commit)))
               (file-name module-ad-file)
               (sha256
                (base32
                 "0aqvhb4byg0shgbrwmb41nq2h8sbg45xn0gsdy94iiq0v72bw307")))))

      ;; The 'CMake/ITKSetStandardCompilerFlags.cmake' file normally sets
      ;; '-mtune=native -march=corei7', suggesting there's something to be
      ;; gained from CPU-specific optimizations.
      (properties '((tunable? . #t)))

      (home-page "https://github.com/InsightSoftwareConsortium/ITK/")
      (synopsis "Scientific image processing, segmentation and registration")
      (description
       "The Insight Toolkit (ITK) is a toolkit for N-dimensional
scientific image processing, segmentation, and registration.  Segmentation is
the process of identifying and classifying data found in a digitally sampled
representation.  Typically the sampled representation is an image acquired
from such medical instrumentation as CT or MRI scanners.  Registration is the
task of aligning or developing correspondences between data.  For example, in
the medical environment, a CT scan may be aligned with a MRI scan in order to
combine the information contained in both.")
      (license license:asl2.0))))

;; Provide variant of insight-toolkit (ITK) built with ITK_LEGACY_REMOVE=OFF.
;; ITK-SNAP version 4.2.2 and 4.4.0-alpha3 require ITK >= 5.4 and for ITK to
;; be built this way.  Note that enabling Python wrapping forces this option
;; to ON, so Python wrapping is not enabled for this build.
(define insight-toolkit-legacy
  (hidden-package
   (package/inherit insight-toolkit
     ;; Unfortunately we cannot remove the 'python output because it is
     ;; referenced in #:configure-flags below.
     (name "insight-toolkit-legacy")
     (arguments
      (substitute-keyword-arguments (package-arguments insight-toolkit)
        ((#:configure-flags cf '())
         #~(filter (lambda (flag)
                     (not (or
                           ;; Remove these flags to restore the default
                           ;; ITK_LEGACY_REMOVE=OFF.
                           (string=? "-DITK_WRAPPING=ON" flag)
                           (string=? "-DITK_WRAP_PYTHON=ON" flag)
                           ;; These flags are now unused.
                           (string-prefix? "-DPY_SITE_PACKAGES_PATH=" flag)
                           (string-prefix? "-DITK_USE_PYTHON_LIMITED_API=" flag)
                           (string-prefix? "-DITK_USE_SYSTEM_CASTXML=" flag)
                           (string-prefix? "-DITK_USE_SYSTEM_SWIG=" flag))))
                   #$cf))
         ((#:phases phases #~%standard-phases)
          #~(modify-phases #$phases
              (delete 'python-sanity-check)))))
     (inputs (modify-inputs (package-inputs insight-toolkit)
               (delete "python")))
     (native-inputs (modify-inputs (package-native-inputs insight-toolkit)
                      (delete "castxml")
                      (delete "python-numpy")
                      (delete "swig"))))))

(define-public itk-snap
  ;; The latest release, 4.2.2, segmentation faults on startup.
  ;; The commit is version 4.4.0-alpha3.
  (let ((commit "65251254d44d68a6c0530984169784e35de020dd")
        (revision "0"))
  (package
    (name "itk-snap")
    (version (git-version "4.2.2" revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pyushkevich/itksnap")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "07dgcfklc55yj3ldcq6fc5fil8qfrv7z6c3xhbd293kz7kpjr4yc"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DSNAP_VERSION_GIT_BRANCH=release"
              "-DSNAP_PACKAGE_QT_PLUGINS=OFF")
      #:phases
      #~(modify-phases %standard-phases
          ;; During the installation phase all libraries provided by all
          ;; dependencies will be copied to the lib directory.  That's insane,
          ;; so we disable this.
          (add-after 'unpack 'do-not-copy-dependencies
            (lambda _
              (substitute* "CMakeLists.txt"
                (("install_qt5_executable\
\\(\\$\\{SNAP_MAIN_INSTALL_DIR\\}/\\$\\{SNAP_EXE\\}\\)")
                 ""))))
          (add-after 'unpack 'disable-gui-tests
            (lambda _
              ;; The GUI tests just segmentation fault.
              (substitute* "CMakeLists.txt"
                (("  (Workspace|DiffSpace|ProbeIntensity|RegionCompetition\
|RandomForest|RandomForestBailOut|NaNs|4DContinuousRenderingD|EdgeAttraction\
|EchoCartesianDicomLoading|LabelSmoothing|PreferencesDialog|MeshImport\
|MeshWorkspace|SegmentationMesh|VolumeRendering|Reloading|4DToMC|MCTo4D\
|DeformationGrid)")
                 ""))))
          (add-after 'unpack 'make-reproducible
            (lambda _
              (substitute* "CMakeLists.txt"
                (("TODAY\\(SNAP_VERSION_COMPILE_DATE\\)")
                 "SET(SNAP_VERSION_COMPILE_DATE \"(removed for reproducibility)\")"))))
          (add-after 'unpack 'prepare-submodules
            (lambda _
              (rmdir "Submodules/c3d")
              (symlink #$(this-package-native-input "c3d-checkout")
                       "Submodules/c3d")
              (rmdir "Submodules/digestible")
              (symlink #$(this-package-native-input "digestible-checkout")
                       "Submodules/digestible")
              (rmdir "Submodules/greedy")
              (symlink #$(this-package-native-input "greedy-checkout")
                       "Submodules/greedy")))
          (add-after 'unpack 'remove-bundled-jsoncpp
            (lambda _
              (substitute* "CMakeLists.txt"
                (("  Common/JSon/jsoncpp\\.cpp") ""))))
          (add-before 'check 'prepare-tests
            (lambda _
              ;; Needed by at least one test.
              (setenv "HOME" "/tmp")))
          (add-after 'install 'wrap-executable
            (lambda _
              (wrap-program (string-append #$output "/bin/itksnap")
                (list
                 "QT_PLUGIN_PATH"
                 'suffix
                 (list (string-append #$(this-package-input "qtbase")
                                      "/lib/qt6/plugins")
                       (string-append #$(this-package-input "qtdeclarative")
                                      "/lib/qt6/plugins")))))))))
    (inputs
     (list curl
           freetype
           glew
           glu
           insight-toolkit-legacy
           jsoncpp
           libssh
           mesa-opencl
           qtbase
           qtdeclarative
           vtk))
    (native-inputs
     (list
      doxygen
      ;; Use the submodule commits in this version of ITK-SNAP.
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pyushkevich/c3d")
              (commit "a86a2a32db8635c1535522332fee68bc56eacaa2")))
        (file-name "c3d-checkout")
        (sha256
         (base32 "0da3ikx7pqlrmvhkmzil269j6kyd84pphy1mls8v69gmzl89piis")))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pyushkevich/digestible")
              (commit "1b66709e99c43d280bb472e1a0e36185ef2ea412")))
        (file-name "digestible-checkout")
        (sha256
         (base32 "1m1b954prq6l3byfdgxw2y17xsg81agd516g5n6ps60dqnxw8hqs")))
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/pyushkevich/greedy")
              (commit "f10152c5374da08ee024c4c60ef8882876bd0808")))
        (file-name "greedy-checkout")
        (sha256
         (base32 "0xk1l0h4wis4nkfwjnvh624bdlhy7l26djibk4l00wzv0vvq21qv")))))
    (home-page "https://sourceforge.net/p/itk-snap/")
    (synopsis "Medical image segmentation")
    (description "ITK-SNAP is a tool for segmenting anatomical structures in
medical images.  It provides an automatic active contour segmentation
pipeline, along with supporting a manual segmentation toolbox.  ITK-SNAP has a
full-featured UI aimed at clinical researchers.")
    ;; This includes the submodules greedy, c3d and digestible.
    (license license:gpl3+))))

(define-public metapixel
  ;; Follow stable branch.
  (let ((commit "98ee9daa093b6c334941242e63f90b1c2876eb4f"))
    (package
      (name "metapixel")
      (version (git-version "1.0.2" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/schani/metapixel")
               (commit commit)
               ;; TODO: Package rwimg and lispreader?
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0r7n3a6bvcxkbpda4mwmrpicii09iql5z69nkjqygkwxw7ny3309"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f                    ; No tests.
        #:make-flags
        #~(list
           (string-append "PREFIX=" #$output)
           (format #f "MANPAGE_XSL=~a/xml/xsl/~a-~a/manpages/docbook.xsl"
                   #$(this-package-native-input "docbook-xsl")
                   #$(package-name
                      (this-package-native-input "docbook-xsl"))
                   #$(package-version
                      (this-package-native-input "docbook-xsl"))))
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (add-before 'install 'fix-directory-creation
              (lambda _
                (mkdir-p (string-append #$output "/share/man/man1")))))))
      (inputs
       (list giflib libjpeg-turbo libpng
             perl))
      (native-inputs
       (list docbook-xml-4.2 docbook-xsl
             libxslt pkg-config))
      (home-page "https://www.complang.tuwien.ac.at/schani/metapixel/")
      (synopsis "Photomosaics generator")
      (description "Metapixel is a program for generating photomosaics.  It can
generate classical photomosaics, in which the source image is viewed as a
matrix of equally sized rectangles for each of which a matching image is
substituted, as well as collage-style photomosaics, in which rectangular parts
of the source image at arbitrary positions (i.e. not aligned to a matrix) are
substituted by matching images.")
      (license license:gpl2))))

(define-public scantailor-advanced
  (let ((commit "3d1e74e6ace413733511086934a66f4e3f7a6027"))
    (package
      (name "scantailor-advanced")
      (version (string-append "1.0.16-" (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/4lex4/scantailor-advanced")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0kixwjb2x457dq7927hkh34c803p7yh1pmn6n61rk9shqrcg492h"))))
      (build-system qt-build-system)
      (native-inputs
       (list qttools-5))
      (inputs
       `(("boost" ,boost)
         ("libjpeg" ,libjpeg-turbo)
         ("libpng" ,libpng)
         ("libtiff" ,libtiff)
         ("qtbase" ,qtbase-5)
         ("qtsvg-5" ,qtsvg-5)
         ("zlib" ,zlib)))
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           ;; Some tests require a display and fail with offscreen mode.
           (add-after 'unpack 'disable-failing-tests
             (lambda _
               (setenv "ARGS" "--exclude-regex \"imageproc_.*\"")
               #t)))))
      (home-page "https://github.com/4lex4/scantailor-advanced")
      (synopsis "Clean up scanned pages")
      (description "Scan Tailor is an interactive post-processing tool for
scanned pages.  It performs operations such as page splitting, deskewing,
adding/removing borders, and others.  You give it raw scans, and you get pages
ready to be printed or assembled into a PDF or DJVU file.  Scanning, optical
character recognition, and assembling multi-page documents are out of scope of
this project.

Scan Tailer Advanced is a fork of Scan Tailer that merges Scan Tailor Featured
and Scan Tailor Enhanced versions as well as including many more bug fixes.")
      (license license:gpl3+))))

(define-public simpleitk
  (package
    (name "simpleitk")
    (version "2.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SimpleITK/SimpleITK")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ykrfrfh2012cg1122689w23pan2y731sszfpb701zhsb6fwv7j7"))))
    (build-system cmake-build-system)
    (outputs '("out" "python" "r"))
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS=ON"
              "-DWRAP_LUA=OFF"
              "-DWRAP_PYTHON=ON"
              "-DWRAP_R=ON"
              "-DSimpleITK_PYTHON_USE_VIRTUALENV=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'early-GTest-discovery
            (lambda _
              ;; Find GTest before targets link to 'GTest::GTest'.
              (substitute* "Testing/Unit/CMakeLists.txt"
                (("add_subdirectory\\(TestBase\\)" anchor)
                 (string-append "find_package(GTest REQUIRED)\n" anchor)))))
          (add-after 'unpack 'extract-test-data
            (lambda _
              (invoke "tar" "xvf"
                      #$(this-package-native-input
                         (string-append "SimpleITKData-" version ".tar.gz"))
                      "--strip-components=1")))
          (add-after 'unpack 'fix-runpath
            (lambda _
              ;; The SWIG-generated shared libraries do not have SimpleITK's
              ;; libraries in their RUNPATH.
              (define (cmake-snippet start)
                (string-append
                 start
                 "set_target_properties(${SWIG_MODULE_SimpleITK_TARGET_NAME} "
                 "PROPERTIES BUILD_WITH_INSTALL_RPATH TRUE "
                 "INSTALL_RPATH \"" #$output "/lib\")\n"))
              (substitute* "Wrapping/Python/CMakeLists.txt"
                (("^set\\(SWIG_MODULE_SimpleITKPython.*" anchor)
                 (cmake-snippet anchor)))
              (substitute* "Wrapping/R/CMakeLists.txt"
                (("^set\\(SWIG_MODULE_SimpleITKR.*" anchor)
                 (cmake-snippet anchor)))))
          (add-after 'unpack 'patch-cmake-config-itk-path
            (lambda _
              ;; Allow building SimpleITK C++ projects without ITK installed
              ;; in the profile.
              #$(let*
                  ((itk (this-package-input "insight-toolkit"))
                   (itk-version (package-version itk))
                   (itk-version-major+minor
                         (version-major+minor itk-version)))
                  #~(substitute* "SimpleITKConfig.cmake.in"
                      (((string-append
                          "find_package\\(ITK \"@ITK_VERSION@\" "
                          "EXACT REQUIRED\\)"))
                       (string-append
                         "find_package(ITK \"" #$itk-version
                         "\" EXACT REQUIRED PATHS " #$itk
                         "/lib/cmake/ITK-"
                         #$itk-version-major+minor ")"))))))
          (add-after 'install 'install-language-extension-modules
            (lambda _
              (with-directory-excursion "Wrapping/Python"
                (invoke "python3" "setup.py" "bdist_wheel")
                (apply invoke "pip" "--no-cache-dir" "--no-input" "install"
                       "--no-deps" "--prefix" #$output:python
                       (find-files "dist" "\\.whl$")))
              (let ((r-package-path
                     (string-append #$output:r "/site-library/SimpleITK")))
                (mkdir-p r-package-path)
                (copy-recursively "Wrapping/R/R_libs/SimpleITK"
                                  r-package-path))))
          (delete 'check)
          (add-after 'install 'check-after-install
            ;; Run the tests when the SimpleITK libraries are where the
            ;; language extension modules expect them to be.
            (assoc-ref %standard-phases 'check)))))
    (home-page "https://simpleitk.org")
    (inputs (list insight-toolkit python r-minimal))
    (native-inputs
     (list googletest
           lua
           python-numpy                 ;for tests
           python-pip
           python-setuptools
           python-wheel
           swig-next
           (origin
             (method url-fetch)
             (uri (string-append
                   "https://github.com/SimpleITK/SimpleITK/releases/download/v"
                   version "/SimpleITKData-" version ".tar.gz"))
             (sha256
              (base32
               "13y44qqsgsvbrm84073i8clhggdgk8f36i6102sjg4j3fq790gal")))))
    (synopsis "Simplified interface to @acronym{ITK, Insight Toolkit}")
    (description
     "SimpleITK is an image analysis toolkit built on top of @acronym{ITK,
Insight Toolkit}.  It provides a simplified interface to most of the
image filters and the input/output and registration frameworks in
@acronym{ITK, Insight Toolkit}.  It is written in C++ and provides
bindings for interpreted languages.  This package includes the C++,
Python and R interfaces.")
    (license license:asl2.0)))

(define-public stiff
  (package
    (name "stiff")
    (version "2.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.astromatic.net/download/stiff/stiff-"
                           version ".tar.gz"))
       (sha256
        (base32 "14m92dskzw7bwsr64ha4p0mj3ndv13gwcbfic3qxrs3zq5353s7l"))))
    (build-system gnu-build-system)
    (inputs
     (list libtiff zlib libjpeg-turbo))
    (home-page "https://www.astromatic.net/software/stiff")
    (synopsis "Convert scientific FITS images to TIFF format")
    (description
     "STIFF is a program that converts scientific @acronym{FITS, Flexible Image
Transport System} images to the more popular TIFF format for illustration
purposes.")
    (license license:gpl3+)))

(define-public python-imgviz
  (package
    (name "python-imgviz")
    (version "1.7.6")
    (source
     (origin
       ;; PyPi tarball lacks tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wkentaro/imgviz")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z7nwnvqh3hbbccf7v56398aiiwqs68kyrgc5vsmmh1cp4pwrgnb"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; LookupError: Error getting the version from source `vcs`:
               ;; setuptools-scm was unable to detect version for <...>
               (add-after 'unpack 'pretend-version
                 (lambda _
                   (setenv "SETUPTOOLS_SCM_PRETEND_VERSION" #$version))))))
    (native-inputs
     (list python-pytest
           python-hatchling
           python-hatch-vcs
           python-hatch-fancy-pypi-readme))
    (propagated-inputs
     (list python-matplotlib
           python-numpy
           python-pillow
           python-pyyaml))
    (home-page "http://github.com/wkentaro/imgviz")
    (synopsis "Image Visualization Tools")
    (description "Python library for object detection, semantic and instance
segmentation.")
    (license license:expat)))

(define-public python-pims
  (package
    (name "python-pims")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pims" version))
       (sha256
        (base32 "0swlh8g4kf8p24g0ghkmwcj9y45rc59lmqx459nhhmhj6167m42m"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; We don't have all the (sometimes very large) data files, so we skip
      ;; these tests.
      '(list "--ignore=pims/tests/test_common.py"
             "--ignore=pims/tests/test_imseq.py"
             "--ignore=pims/tests/test_norpix.py")))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-imageio python-numpy python-slicerator))
    (home-page "https://github.com/soft-matter/pims")
    (synopsis "Python Image Sequence")
    (description "Scientific video can be packaged in various ways: familiar
video formats like @file{.AVI} and @file{.MOV}, folders full of numbered
images, or \"stacks\" of TIFF images.  Each of these requires a separate
Python module.  And, once loaded, they have different methods for accessing
individual images, looping through the images in bulk, accessing a specific
range, or dealing with multidimensional files.  PIMS can do all of these using
a consistent interface, handling the differences between different inputs
invisibly.")
    (license license:bsd-3)))

(define-public python-spatial-image
  (package
    (name "python-spatial-image")
    (version "1.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "spatial_image" version))
       (sha256
        (base32 "0blyjwgv6bqpg7r3f4dcs7x27ysfm8n9y0zk5j7dxj74lma3wdsm"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-numpy python-pytest python-pytest-mypy
                             python-xarray python-xarray-dataclasses))
    (native-inputs (list python-flit-core python-pytest))
    (home-page "https://github.com/spatial-image/spatial-image")
    (synopsis "Multi-dimensional spatial image data structure")
    (description "This package implements a multi-dimensional spatial image
data structure for scientific Python.

To facilitate:

@itemize
@item Multi-scale processing and analysis
@item Registration
@item Resampling
@item Subregion parallel processing
@item Coupling with meshes, point sets, and annotations
@end itemize

with scientific images, which are typically multi-dimensional with anisotropic
sampling, this package provides a spatial-image data structure.  In addition
to an N-dimensional array of pixel values, spatial metadata defines the
location of the pixel sampling grid in space time.  It also labels the array
dimensions.  This metadata is easily utilized and carried through image
processing pipelines.")
    (license license:expat)))

(define-public labelme
  (package
    (name "labelme")
    ;; It's the latest available version which does not require not packaged
    ;; <https://github.com/wkentaro/osam>.
    (version "5.2.1")
    (source
     (origin
       ;; PyPi tarball lacks tests.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/wkentaro/labelme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xpyad6rlkxyx51jaai4xhdy15k1gvm62xnkjn152hc1vj1c77sr"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-m" "not gpu"
              ;; Fails when invoking help2man for unknown reason.
              "--ignore=tests/docs_tests/man_tests/test_labelme_1.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xserver
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((xorg-server #$(this-package-native-input "xorg-server")))
                ;; Options taken from CI workflow.
                (system (string-append xorg-server "/bin/Xvfb :99 -screen 0 "
                                       "1920x1200x24 -ac +extension GLX +render "
                                       "-noreset &"))
                (setenv "DISPLAY" ":99.0")))))))
    (native-inputs
     (list python-pytest
           python-pytest-qt
           xorg-server-for-tests
           python-wheel))
    (propagated-inputs
     (list python-imgviz
           python-matplotlib
           python-natsort
           python-numpy
           python-pillow
           python-pyyaml
           python-qtpy
           python-termcolor))
    (home-page "https://github.com/wkentaro/labelme")
    (synopsis "Image Polygonal Annotation")
    (description
     "Image and video labeling tool supporting different shapes like polygons,
rectangles, circles, lines, points and VOC/COCO export.")
    (license license:gpl3+)))

(define-public charls
  (package
    (name "charls")
    (version "2.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/team-charls/charls/")
                    (commit (string-append version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vp6mjjm6hi2r1swwvbg86asvn604w58nam7yvqk1qr13aizjl71"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCMAKE_BUILD_TYPE:STRING=Release"
                           "-DBUILD_SHARED_LIBS=On")))
    (native-inputs (list git pkg-config))
    (home-page "https://github.com/team-charls/charls")
    (synopsis "Library for using JPEG-LS compliant images")
    (description
     "CharLS is a codec library that can be used to build applications that
can handle JPEG-LS compliant images.  In the application you are writing you
can call the CharLS codec and pass it images (sometimes called raster bitmaps),
 to have them encoded to JPEG-LS, or JPEG-LS streams, which CharLS will decode
to images.")
    (license license:bsd-3)))

(define-public libansilove
  (package
    (name "libansilove")
    (version "1.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ansilove/libansilove")
                    (commit (string-append version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "169njlck4a2bmf1kmjas1w594hyda543ykdnwg7fwkviij39l9z6"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f)) ; No tests included
    (native-inputs
     (list gd))
    (home-page "https://www.ansilove.org/")
    (synopsis "Library for converting ANSI, ASCII, and other formats to PNG")
    (description
     "libansilove is a library for converting artscene file types to PNG images,
including ANSI (.ANS) and many others.  The library primarily serves to support
the ansilove tool.")
    (license license:bsd-2)))

(define-public ansilove
  (package
    (name "ansilove")
    (version "4.2.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/ansilove/ansilove")
                    (commit (string-append version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h9r759krjl8wi68yxs1d46qfrx6v89a8vmmv3aqym8vn9x430kh"))))
    (build-system cmake-build-system)
    (native-inputs
     (list libansilove))
    (home-page "https://www.ansilove.org/")
    (synopsis "ANSI and ASCII art to PNG converter")
    (description
     "AnsiLove is an ANSI and ASCII art to PNG converter, allowing to convert
ANSI and artscene-related file formats into PNG images, supporting ANSI (.ANS),
PCBoard (.PCB), Binary (.BIN), Artworx (.ADF), iCE Draw (.IDF), Tundra (.TND)
and XBin (.XB) formats.")
    (license license:bsd-2)))
