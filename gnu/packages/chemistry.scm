;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Konrad Hinsen <konrad.hinsen@fastmail.net>
;;; Copyright © 2018, 2021 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022, 2023, 2024 David Elsing <david.elsing@posteo.net>
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

(define-module (gnu packages chemistry)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gv)
  #:use-module (gnu packages image)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python))

(define-public avogadrolibs
  (package
    (name "avogadrolibs")
    (version "1.93.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenChemistry/avogadrolibs")
             (commit version)))
       (sha256
        (base32 "1xivga626n5acnmwmym8svl0pdri8hkp59czf04ri2zflnviyh39"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list eigen
           mmtf-cpp
           googletest
           pkg-config
           pybind11))
    (inputs
     (list glew
           libarchive
           libmsym
           molequeue
           python
           spglib
           qtbase-5))
    (arguments
     '(#:configure-flags (list "-DENABLE_TESTING=ON"
                               (string-append "-DSPGLIB_INCLUDE_DIR="
                                              (assoc-ref %build-inputs "spglib")
                                              "/include"))))
    (home-page "https://www.openchemistry.org/projects/avogadro2/")
    (synopsis "Libraries for chemistry, bioinformatics, and related areas")
    (description
     "Avogadro libraries provide 3D rendering, visualization, analysis and data
processing useful in computational chemistry, molecular modeling,
bioinformatics, materials science, and related areas.")
    (license license:bsd-3)))

(define-public avogadro2
  (package
    (name "avogadro2")
    (version "1.93.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenChemistry/avogadroapp")
             (commit version)))
       (sha256
        (base32
         "1z3pjlwja778a1dmvx9aqz2hlw5q9g3kqxhm9slz08452600jsv7"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list eigen pkg-config))
    (inputs
     (list avogadrolibs hdf5 molequeue qtbase-5))
    ;; TODO: Enable tests with "-DENABLE_TESTING" configure flag.
    (arguments
     '(#:tests? #f))
    (home-page "https://www.openchemistry.org/projects/avogadro2/")
    (synopsis "Advanced molecule editor")
    (description
     "Avogadro 2 is an advanced molecule editor and visualizer designed for use
in computational chemistry, molecular modeling, bioinformatics, materials
science, and related areas.  It offers flexible high quality rendering and a
powerful plugin architecture.")
    (license license:bsd-3)))

(define-public inchi
  (package
    (name "inchi")
    ;; Update the inchi-doc native input when updating inchi.
    (version "1.07.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/IUPAC-InChI/InChI")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0r32f6i5d8ir96ic3nvqb5lywxrznwrkk6hnz1q0a4bgsw5pmk0n"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "INCHI-1-BIN"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; no check target
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure) ; no configure script
          (add-after 'unpack 'chdir-to-build-directory
            (lambda _ (chdir "INCHI-1-SRC/INCHI_EXE/inchi-1/gcc")))
          (add-after 'build 'build-library
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (chdir "../../../INCHI_API/libinchi/gcc")
              (invoke "make" "-j" (if parallel-build?
                                      (number->string (parallel-job-count))
                                      "1"))))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (doc (string-append #$output "/share/doc/inchi"))
                     (include-dir (string-append #$output "/include/inchi"))
                     (lib (string-append #$output "/lib/inchi")))
                (chdir "../../..")
                ;; Install binary.
                (with-directory-excursion "INCHI_EXE/bin/Linux"
                  (rename-file "inchi-1" "inchi")
                  (install-file "inchi" bin))
                ;; Install library.
                (with-directory-excursion "INCHI_API/bin/Linux"
                  (let ((libname (basename
                                  (car
                                   (find-files "." "libinchi\\.so\\.1\\.*")))))
                    (install-file libname lib)
                    (with-directory-excursion lib
                      (symlink libname "libinchi.so.1")
                      (symlink "libinchi.so.1" "libinchi.so"))))
                ;; Install header files.
                (with-directory-excursion "INCHI_BASE/src"
                  (for-each (lambda (file)
                              (install-file file include-dir))
                            (find-files "." "\\.h$")))
                ;; Install documentation.
                (with-directory-excursion "../INCHI-1-DOC"
                  (for-each
                   (lambda (file)
                     (install-file file doc))
                   (find-files "." "\\.pdf$")))))))))
    (native-inputs (list unzip))
    (home-page "https://www.inchi-trust.org")
    (synopsis "Utility for manipulating machine-readable chemical structures")
    (description
     "The @dfn{InChI} (IUPAC International Chemical Identifier) algorithm turns
chemical structures into machine-readable strings of information.  InChIs are
unique to the compound they describe and can encode absolute stereochemistry
making chemicals and chemistry machine-readable and discoverable.  A simple
analogy is that InChI is the bar-code for chemistry and chemical structures.")
    (license license:expat)))

(define-public libmsym
  (package
    (name "libmsym")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mcodev31/libmsym")
             (commit (string-append "v" version))))
       (sha256
        (base32
         "0a9j28irdsr461qpzlc9z1yjyb9kp64fh5zw7ylspc9zn3189qwk"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")
       #:tests? #f))                    ; no check target
    (home-page "https://github.com/mcodev31/libmsym")
    (synopsis "C library dealing with point group symmetry in molecules")
    (description "libmsym is a C library dealing with point group symmetry in
molecules.")
    (license license:expat)))

(define-public mmtf-cpp
  (package
    (name "mmtf-cpp")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rcsb/mmtf-cpp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0rs2f1ppgqz663c3m22p8wsq6z839bj59zy29chci46ypfhwv6ph"))))
    (build-system cmake-build-system)
    ;; Tests require the soon-to-be-deprecated version 1 of the catch-framework.
    (arguments '(#:tests? #f))
    ;; There is no support for modern msgpack versions yet (see:
    ;; https://github.com/rcsb/mmtf-cpp/issues/44).
    (propagated-inputs (list msgpack-3)) ;included by mmtf/structure_data.hpp
    (home-page "https://mmtf.rcsb.org/")
    (synopsis "C++ API for the Macromolecular Transmission Format")
    (description "This package is a library for the
@acronym{MMTF,macromolecular transmission format}, a binary encoding of
biological structures.")
    (license license:expat)))

(define-public molequeue
  (package
    (name "molequeue")
    (version "0.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/OpenChemistry/molequeue/"
                           "releases/download/" version "/molequeue-"
                           version ".tar.bz2"))
       (sha256
        (base32
         "1w1fgxzqrb5yxvpmnc3c9ymnvixy0z1nfafkd9whg9zw8nbgl998"))))
    (build-system cmake-build-system)
    (inputs
     (list qtbase-5))
    (arguments
     '(#:configure-flags '("-DENABLE_TESTING=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-tests
           (lambda _
             ;; TODO: Fix/enable the failing message and clientserver tests.
             ;; In the message test, the floating-point value "5.36893473232" on
             ;; line 165 of molequeue/app/testing/messagetest.cpp should
             ;; (apparently) be truncated, but it is not.
             (substitute* "molequeue/app/testing/messagetest.cpp"
               (("5\\.36893473232") "5.36893"))
             ;; It is unclear why the clientserver test fails, so it is
             ;; completely disabled.
             (substitute* "molequeue/app/testing/CMakeLists.txt"
               ((".*clientserver.*") ""))
             #t))
         (add-before 'check 'set-display
           (lambda _
             ;; Make Qt render "offscreen" for the sake of tests.
             (setenv "QT_QPA_PLATFORM" "offscreen")
             #t)))))
    (home-page "https://www.openchemistry.org/projects/molequeue/")
    (synopsis "Application for coordinating computational jobs")
    (description "MoleQueue is a system-tray resident desktop application for
abstracting, managing, and coordinating the execution of tasks both locally and
 on remote computational resources.  Users can set up local and remote queues
that describe where the task will be executed.  Each queue can have programs,
with templates to facilitate the execution of the program.  Input files can be
staged, and output files collected using a standard interface.")
    (license license:bsd-3)))

(define-public tng
  (package
    (name "tng")
    (version "1.8.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gromacs/tng")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1apf2n8nb34z09xarj7k4jgriq283l769sakjmj5aalpbilvai4q"))))
    (build-system cmake-build-system)
    (inputs
     (list zlib))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-bundled-zlib
           (lambda _
             (delete-file-recursively "external")
             #t))
         (replace 'check
           (lambda _
             (invoke "../build/bin/tests/tng_testing")
             #t)))))
    (home-page "https://github.com/gromacs/tng")
    (synopsis "Trajectory Next Generation binary format manipulation library")
    (description "TRAJNG (Trajectory next generation) is a program library for
handling molecular dynamics (MD) trajectories.  It can store coordinates, and
optionally velocities and the H-matrix.  Coordinates and velocities are
stored with user-specified precision.")
    (license license:bsd-3)))

(define-public gromacs
  (package
    (name "gromacs")
    (version "2022.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ftp.gromacs.org/pub/gromacs/gromacs-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15vjwasxjq0h18dmzacjkdim51zrvr0ni42hbc30557j5xhbw4f5"))
              ;; Our version of tinyxml2 is far newer than the bundled one and
              ;; require fixing `testutils' code. See patch header for more info
              (patches (search-patches "gromacs-tinyxml2.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags
           #~(list "-DGMX_DEVELOPER_BUILD=on"     ; Needed to run tests
                   ;; Unbundling
                   "-DGMX_USE_LMFIT=EXTERNAL"
                   "-DGMX_BUILD_OWN_FFTW=off"
                   "-DGMX_EXTERNAL_BLAS=on"
                   "-DGMX_EXTERNAL_LAPACK=on"
                   "-DGMX_EXTERNAL_TNG=on"
                   "-DGMX_EXTERNAL_ZLIB=on"
                   "-DGMX_EXTERNAL_TINYXML2=on"
                   (string-append "-DTinyXML2_DIR="
                                  #$(this-package-input "tinyxml2"))
                   ;; Workaround for cmake/FindSphinx.cmake version parsing that does
                   ;; not understand the guix-wrapped `sphinx-build --version' answer
                   (string-append "-DSPHINX_EXECUTABLE_VERSION="
                                  #$(package-version python-sphinx)))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fixes
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Still bundled: part of gromacs, source behind registration
                   ;; but free software anyways
                   ;;(delete-file-recursively "src/external/vmd_molfile")
                   ;; Still bundled: threads-based OpenMPI-compatible fallback
                   ;; designed to be bundled like that
                   ;;(delete-file-recursively "src/external/thread_mpi")
                   ;; Unbundling
                   (delete-file-recursively "src/external/lmfit")
                   (delete-file-recursively "src/external/clFFT")
                   (delete-file-recursively "src/external/fftpack")
                   (delete-file-recursively "src/external/build-fftw")
                   (delete-file-recursively "src/external/tng_io")
                   (delete-file-recursively "src/external/tinyxml2")
                   (delete-file-recursively "src/external/googletest")
                   (copy-recursively #$(package-source googletest)
                                     "src/external/googletest")
                   ;; This test warns about the build host hardware, disable
                   (substitute* "src/gromacs/hardware/tests/hardwaretopology.cpp"
                     (("TEST\\(HardwareTopologyTest, HwlocExecute\\)")
                      "void __guix_disabled()")))))))
    (native-inputs
     (list doxygen
           graphviz
           pkg-config
           python
           python-pygments
           python-sphinx))
    (inputs
     (list fftwf
           `(,hwloc-2 "lib")
           lmfit
           openblas
           perl
           tinyxml2
           tng))
    (home-page "https://www.gromacs.org/")
    (synopsis "Molecular dynamics software package")
    (description "GROMACS is a versatile package to perform molecular dynamics,
i.e. simulate the Newtonian equations of motion for systems with hundreds to
millions of particles.  It is primarily designed for biochemical molecules like
proteins, lipids and nucleic acids that have a lot of complicated bonded
interactions, but since GROMACS is extremely fast at calculating the nonbonded
interactions (that usually dominate simulations) many groups are also using it
for research on non-biological systems, e.g. polymers.  GROMACS supports all the
usual algorithms you expect from a modern molecular dynamics implementation.")
    (license license:lgpl2.1+)
    (properties '((tunable? . #t)))))

(define-public openbabel
  (package
    (name "openbabel")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/openbabel/openbabel/"
                                  "releases/download/openbabel-"
                                  (string-replace-substring version "." "-")
                                  "/openbabel-" version "-source.tar.bz2"))
              (sha256
               (base32
                "0s0f4zib8vshfaywsr5bjjz55jwsg6yiz2qw4i5jm8wysn0q7v56"))))
    (build-system cmake-build-system)
    (arguments
     `(;; FIXME: Disable tests on i686 to work around
       ;; https://github.com/openbabel/openbabel/issues/2041.
       #:tests? ,(or (%current-target-system)
                     (not (string=? "i686-linux" (%current-system))))
       #:configure-flags
       (list "-DOPENBABEL_USE_SYSTEM_INCHI=ON"
             (string-append "-DINCHI_LIBRARY="
                            (assoc-ref %build-inputs "inchi")
                            "/lib/inchi/libinchi.so.1")
             (string-append "-DINCHI_INCLUDE_DIR="
                            (assoc-ref %build-inputs "inchi") "/include/inchi"))
       #:test-target "test"))
    (native-inputs
     (list pkg-config))
    (inputs
     (list eigen inchi libxml2 zlib))
    (home-page "http://openbabel.org/wiki/Main_Page")
    (synopsis "Chemistry data manipulation toolbox")
    (description
     "Open Babel is a chemical toolbox designed to speak the many languages of
chemical data.  It's a collaborative project allowing anyone to search, convert,
analyze, or store data from molecular modeling, chemistry, solid-state
materials, biochemistry, or related areas.")
    (license license:gpl2)))

(define-public spglib
  (package
    (name "spglib")
    (version "1.16.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spglib/spglib")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1kzc956m1pnazhz52vspqridlw72wd8x5l3dsilpdxl491aa2nws"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "check"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-header-install-dir
           (lambda _
             ;; As of the writing of this package, CMake and GNU build systems
             ;; install the header to two different location.  This patch makes
             ;; the CMake build system's choice of header directory compatible
             ;; with the GNU build system's choice and with what avogadrolibs
             ;; expects.
             ;; See https://github.com/spglib/spglib/issues/75 and the relevant
             ;; part of https://github.com/OpenChemistry/avogadroapp/issues/97.
             (substitute* "CMakeLists.txt"
               (("\\$\\{CMAKE_INSTALL_INCLUDEDIR\\}" include-dir)
                (string-append include-dir "/spglib")))
             #t)))))
    (home-page "https://spglib.github.io/spglib/index.html")
    (synopsis "Library for crystal symmetry search")
    (description "Spglib is a library for finding and handling crystal
symmetries written in C.  Spglib can be used to:

@enumerate
@item Find symmetry operations
@item Identify space-group type
@item Wyckoff position assignment
@item Refine crystal structure
@item Find a primitive cell
@item Search irreducible k-points
@end enumerate")
    (license license:bsd-3)))

(define-public python-pymol
  (package
    (name "python-pymol")
    (version "2.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schrodinger/pymol-open-source")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08zmfgclkbjkqjpq8xs1mphs1i8rpqj76mcw7m2mrhvma5qj1nr5"))))
    (build-system python-build-system)
    (arguments
     '(#:configure-flags
       (list "--glut" "--testing")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'make-reproducible
           (lambda _
             (substitute* "create_shadertext.py"
               (("time\\.time\\(\\)") "0"))))
         (add-after 'unpack 'add-include-directories
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "freetype")
                                    "/include/freetype2:"
                                    (assoc-ref inputs "libxml2")
                                    "/include/libxml2:"
                                    (getenv "CPLUS_INCLUDE_PATH")))))
         ;; Prevent deleting the leading / in the __init__.py path in the
         ;; launch script.
         (add-after 'unpack 'disable-unchroot
           (lambda _
             (substitute* "setup.py"
               (("self\\.unchroot") ""))))
         ;; The setup.py script does not support one of the Python build
         ;; system's default flags, "--single-version-externally-managed".
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "python" "setup.py" "install"
                     (string-append "--prefix=" (assoc-ref outputs "out"))
                     "--root=/"))))))
    (inputs
     (list freetype
           libpng
           freeglut
           glew
           libxml2
           mmtf-cpp
           python-pyqt
           glm
           netcdf))
    (native-inputs (list catch2))
    (home-page "https://pymol.org")
    (synopsis "Molecular visualization system")
    (description "PyMOL is a capable molecular viewer and renderer.  It can be
used to prepare publication-quality figures, to share interactive results with
your colleagues, or to generate pre-rendered animations.")
    (license license:bsd-3)))

(define-public gemmi
  (package
    (name "gemmi")
    (version "0.6.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/project-gemmi/gemmi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wciqqswc4p4v4kglfv36gnvyyimqn4lnywdzd0pgrjn443i860y"))
              (patches
               (search-patches "gemmi-fix-sajson-types.patch"
                               "gemmi-fix-pegtl-usage.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file-recursively "include/gemmi/third_party")
                  (delete-file-recursively "third_party")))))
    (outputs '("out" "bin" "python"))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build python-build-system)
                   #:select (site-packages)))
      #:imported-modules (append %cmake-build-system-modules
                                 '((guix build python-build-system)))
      #:configure-flags
      #~(list "-DUSE_PYTHON=ON"
              (string-append "-DPYTHON_INSTALL_DIR="
                             (site-packages %build-inputs %outputs)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-includes
            (lambda _
              (substitute* (list "src/sprintf.cpp"
                                 "include/gemmi/dirwalk.hpp"
                                 "include/gemmi/cif.hpp"
                                 "include/gemmi/json.hpp"
                                 "python/gemmi.cpp"
                                 "include/gemmi/atof.hpp"
                                 "include/gemmi/numb.hpp"
                                 "include/gemmi/fourier.hpp")
                (("<stb/stb_sprintf.h>") "<stb_sprintf.h>")
                (("\"third_party/tinydir.h\"") "<tinydir.h>")
                (("\"third_party/tao/pegtl.hpp\"") "<tao/pegtl.hpp>")
                (("\"third_party/sajson.h\"") "<sajson.h>")
                (("\"gemmi/third_party/tao/pegtl/parse_error.hpp\"")
                 "<tao/pegtl/parse_error.hpp>")
                (("\"third_party/fast_float.h\"")
                 "<fast_float/fast_float.h>")
                (("\"third_party/pocketfft_hdronly.h\"")
                 "<pocketfft_hdronly.h>"))))
          (add-after 'unpack 'change-bin-prefix
            (lambda _
              (substitute* "CMakeLists.txt"
                (("install\\(TARGETS program DESTINATION bin\\)")
                 (string-append
                  "install(TARGETS program DESTINATION "
                  #$output:bin "/bin)")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (with-directory-excursion "../source"
                  (setenv "PYTHONPATH" "../build")
                  (invoke "python3" "-m" "unittest" "discover" "-v"
                          "-s" "tests"))))))))
    (inputs (list python zlib))
    (native-inputs
     (list fast-float
           optionparser
           pegtl
           pocketfft-cpp
           pybind11
           sajson-for-gemmi
           stb-sprintf
           tinydir))
    (home-page "https://gemmi.readthedocs.io/en/latest/")
    (synopsis "Macromolecular crystallography library and utilities")
    (description "GEMMI is a C++ library for macromolecular crystallography.
It can be used for working with
@enumerate
@item macromolecular models (content of PDB, PDBx/mmCIF and mmJSON files),
@item refinement restraints (CIF files),
@item reflection data (MTZ and mmCIF formats),
@item data on a 3D grid (electron density maps, masks, MRC/CCP4 format)
@item crystallographic symmetry.
@end enumerate")
    (license license:mpl2.0)))

(define-public freesasa
  (package
    (name "freesasa")
    (version "2.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/mittinatten/freesasa")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "07wdnahf3g355ryaiqvfxd5f4rl54wv8jwxcbn0nia89fqysbv0f"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Remove C files generated by Flex and Bison
                  (for-each delete-file
                            '("src/parser.c" "src/parser.h"
                              "src/lexer.c" "src/lexer.h"))))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-check"
              "--enable-parser-generator"
              "CXXFLAGS=-std=c++17"
              "--enable-doxygen"
              ;; Some tests rely on replacing malloc with a wrapper which
              ;; fails in a controlled way, but this does not work if the call
              ;; is replaced. This was fixed upstream, remove once there is a
              ;; new release.
              "CFLAGS=-fno-builtin-malloc")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-libc++-linking
            (lambda _
              (substitute* "src/Makefile.am"
                (("-lc\\+\\+") ""))))
          (add-after 'unpack 'build-shared-library
            (lambda _
              (substitute* "src/Makefile.am"
                (("lib_LIBRARIES") "lib_LTLIBRARIES")
                (("libfreesasa\\.a") "libfreesasa.la")
                (("freesasa_LDADD \\+= libfreesasa\\.la" prev)
                 (string-append prev "\nlibfreesasa_la_LIBADD"
                                " = -ljson-c -lgemmi_cpp ${libxml2_LIBS}\n"))
                (("_a_SOURCES") "_la_SOURCES"))
              (substitute* "configure.ac"
                (("AC_PROG_INSTALL" inst)
                 (string-append "AM_PROG_LIBTOOL\n" inst)))
              (substitute* "tests/Makefile.am"
                (("libfreesasa\\.a") "libfreesasa.la"))))
          (add-before 'build 'build-lexer-and-parser
            (lambda _
              (with-directory-excursion "src"
                (invoke "make" "lexer.h" "parser.h"))))
          (add-after 'install 'install-doc
            (lambda _
              (copy-recursively
               "doc/html"
               (string-append #$output:doc "/share/doc/"
                              #$name "-" #$version)))))))
    (inputs (list gemmi json-c libxml2))
    (native-inputs
     (list autoconf
           automake
           bison
           check
           doxygen
           fast-float
           flex
           libtool
           pegtl
           perl
           pkg-config))
    (home-page "https://freesasa.github.io/")
    (synopsis "Calculate the solvent accessible surface area (SASA) of
molecules")
    (description "FreeSASA is a command line tool and C-library for
calculating @acronym{SASAs, solvent accessible surface areas}.  By default Lee
& Richards' algorithm is used, but Shrake & Rupley's is also available.  Both
can be parameterized to arbitrary precision, and for high resolution versions
of the algorithms, the calculations give identical results.")
    (license license:expat)))

(define-public maeparser
  (package
    (name "maeparser")
    (version "1.3.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/schrodinger/maeparser")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0mr5glg4br04ql5grby8yqni9fqq1l1cc75wyc159a1b9lwr7q7r"))))
    (build-system cmake-build-system)
    (inputs (list boost zlib))
    (home-page "https://github.com/schrodinger/maeparser")
    (synopsis "Maestro file parser")
    (description "maeparser is a parser for Schrodinger Maestro files.")
    (license license:expat)))

(define-public coordgenlibs
  (package
    (name "coordgenlibs")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/schrodinger/coordgenlibs/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wjaxwaihjy9xm5ys23f5abl50zcar1h9pww5ajdkgygsqy0bavi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DCOORDGEN_RIGOROUS_BUILD=OFF"
              "-DCOORDGEN_USE_MAEPARSER=ON")))
    (inputs (list boost maeparser))
    (home-page "https://github.com/schrodinger/coordgenlibs/")
    (synopsis "2D molecule coordinate generation")
    (description "@code{coordgenlibs} contains algorithms to generate 2D
coordinates of molecules including macrocycles and metal complexes.  It has an
emphasis on quality rather than speed.")
    (license license:bsd-3)))

(define-public yaehmop
  (package
    (name "yaehmop")
    (version "2024.03.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/greglandrum/yaehmop")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wy38cfqfs203p1k3qqsizzlpvasldjcfxmlng54y5mxzw97n55f"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Separate program
                  (delete-file-recursively "viewkel")
                  ;; Remove example output (some are corrupted)
                  (for-each delete-file (find-files "examples" "\\.Z$"))
                  ;; Documentation outputs
                  (for-each delete-file (find-files "docs" "\\.(ps|pdf)$"))
                  ;; These are transpiled from Fortran to C, but we build the
                  ;; Fortran code instead
                  (delete-file-recursively "tightbind/f2c_files")
                  (with-directory-excursion "tightbind"
                    (for-each delete-file '("abfns.c"
                                            "cboris.c"
                                            "diag.c"
                                            "lovlap.c")))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-DUSE_BLAS_LAPACK=ON"
         (string-append "-DPARM_FILE_LOC=" #$output
                        "/share/" #$name "-" #$version "/eht_parms.dat")
         "-DBIND_EXE_NAME=yaehmop-bind")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "tightbind")))
          (add-after 'chdir 'patch-fortran-functions
            (lambda _
              (substitute* '("mov.c" "prototypes.h")
                (("lovlap\\(") "lovlap_(")
                (("abfns\\(") "abfns_("))))
          (add-after 'chdir 'patch-cmake
            (lambda _
              (substitute* "CMakeLists.txt"
                (("project\\(yaehmop C\\)") "project(yaehmop C Fortran)")
                (("abfns.c") "fortran77/abfns.f")
                (("lovlap.c") "fortran77/lovlap.f")
                (("(set\\(PARM_FILE_LOC.*)\\)" all init)
                 (string-append init " CACHE STRING \"\")"))
                (("add_library\\(yaehmop_eht" lib)
                 (string-append lib " SHARED "))
                (("target_link_libraries\\(test_eht \\$\\{LAPACK_LIBRARIES\\}.*"
                  all)
                 (string-append all "\ntarget_link_libraries(yaehmop_eht "
                                "${LAPACK_LIBRARIES})\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../docs"
                (substitute* "bind_manual.tex"
                  (("\\\\usepackage\\{bindpage\\}")
                   (string-append
                    "\\usepackage[left=2cm,right=2cm,top=4cm,bottom=2cm]"
                    "{geometry}\n"
                    "\\pdfsuppressptexinfo=-1\n")))
                (substitute* "Zmat_appendix.tex"
                  (("file=dihedral\\.eps")
                   "file=figs/dihedral.eps"))
                (setenv "FORCE_SOURCE_DATE" "1")
                (invoke "latexmk" "-pdf" "bind_manual.tex"))))
          (add-after 'install 'install-eht-parms
            (lambda _
              (install-file "../tightbind/eht_parms.dat"
                            (string-append #$output "/share/"
                                           #$name "-" #$version))))
          (add-after 'install-eht-parms 'install-doc
            (lambda _
              (install-file "../docs/bind_manual.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (delete 'check)
          (add-after 'install-doc 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "./test_eht")))))))
    (inputs (list openblas))
    (native-inputs
     (list gfortran
           (texlive-updmap.cfg
            (list texlive-epstopdf
                  texlive-latexmk))))
    (home-page "https://github.com/greglandrum/yaehmop")
    (synopsis "Perform extended Hückel calculations")
    (description "@acronym{YAeHMOP, Yet Another extended Hueckel Molecular
Orbital Package} contains a program and library for performing extended Hückel
calculations and analyzing the results.")
    (license license:bsd-2)))

(define-public avalon-toolkit
  (package
    (name "avalon-toolkit")
    (version "2.0.5a")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rohdebe1/ava-formake")
                    (commit (string-append "AvalonToolkit_" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mfg40y5xc17sm59zdfc5sk22n9zm5zk0z1aw47chvl6hp465szk"))
              (patches
               (search-patches "avalon-toolkit-rdkit-fixes.patch"))
              (modules '((guix build utils) (ice-9 ftw)))
              (snippet
               #~(begin
                   (delete-file-recursively "src/main/java")
                   (delete-file-recursively "src/test/target")))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; There is only one test, which is missing a file
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (replace 'build
            (lambda* (#:key parallel-build? #:allow-other-keys)
              (mkdir "build")
              (mkdir-p "target/executables")
              (mkdir-p "target/libraries")
              (invoke "make" "programs" "-j"
                      (if parallel-build?
                          (number->string (parallel-job-count))
                          "1"))))
          (replace 'install
            (lambda _
              ;; Executables
              (let ((programs '("canonizer" "matchtest" "smi2mol" "struchk")))
                (for-each
                 (lambda (program)
                   (install-file (string-append "target/executables/" program)
                                 (string-append #$output "/bin")))
                 programs))
              (for-each
               (lambda (name)
                 (symlink (string-append #$output "/bin/smi2mol")
                          (string-append #$output "/bin/" name)))
               '("mol2smi" "rdf2smi" "mol2tbl" "mol2sma" "smi2rdf"))
              ;; Library
              (install-file "target/libraries/libavalon_tools.a"
                            (string-append #$output "/lib"))
              (install-file "target/libraries/libavalon4rdkit.a"
                            (string-append #$output "/lib"))
              (for-each
               (lambda (file)
                 (install-file file (string-append #$output
                                                   "/include/avalontoolkit")))
               (find-files "src/main/C/include" "\\.h$"))
              (install-file "license.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version "/")))))))
    (home-page "https://sourceforge.net/projects/avalontoolkit/")
    (synopsis "Tools for SMILES and MOL files and for structure fingerprinting")
    (description "This package contains a library and programs for
canonicalization of SMILES and MOL files, molecular structure fingerprinting
and rendering molecules.")
    (license license:bsd-3)))

(define-public ringdecomposerlib
  (package
    (name "ringdecomposerlib")
    (version "1.1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rareylab/RingDecomposerLib")
                    (commit (string-append "v" version "_rdkit"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1rxzs2wpkkdi40wdzxc4sn0brk7dm7ivgqyfh38gf2f5c7pbg0wi"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_PYTHON_WRAPPER=ON"
              "-DPYTHON_EXECUTABLE=python3"
              (string-append "-DPYTHON_FLAGS=;--prefix=" #$output ";--root=/"))
      #:imported-modules (append %cmake-build-system-modules
                                 '((guix build python-build-system)))
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build python-build-system)
                   #:select (add-installed-pythonpath)))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'patch-cmake
            (lambda _
              (substitute* (list "src/Test/CMakeLists.txt"
                                 "src/RingDecomposerLib/CMakeLists.txt")
                (("build_.*STATIC") "#"))
              (substitute* "test/CMakeLists.txt"
                (("STATIC_TEST") "SHARED_TEST"))
              ;; Link Python library against shared library
              (substitute* "src/python/CMakeLists.txt"
                (("RingDecomposerLibStatic") "RingDecomposerLib"))
              (substitute* "src/python/setup.py.in"
                (("static_libs =.*") "static_libs = []\n")
                (("shared_libs\\s*=.*")
                 (string-append
                  "shared_libs = ['RingDecomposerLib']"))
                (("library_dirs\\s*=\\s*\\[\\]")
                 "library_dirs = ['${CMAKE_BINARY_DIR}/src/RingDecomposerLib']")
                (("extra_objects=.*")
                 (string-append
                  "extra_link_args=['-Wl,-rpath=" #$output "/lib'],\n")))))
          (add-after 'build 'build-doc
            (lambda _
              ;; Disable redundant LaTeX documentation
              (substitute* "../source/documentation/sphinx/conf.py"
                (("^(subprocess.*latex|shutil).*") ""))
              (substitute* "../source/documentation/doxygen.cfg"
                (("GENERATE_LATEX.*YES") "GENERATE_LATEX = NO"))
              ;; Build HTML documentation
              (invoke "sphinx-build" "-b" "html"
                      "../source/documentation/sphinx" "html")))
          (add-after 'install 'install-doc
            (lambda _
              ;; Not reproducible
              (delete-file-recursively "html/.doctrees")
              (copy-recursively "html"
                                (string-append #$output "/share/doc/"
                                               #$name "-" #$version "/html"))))
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))
          (add-before 'check 'set-pythonpath
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (add-installed-pythonpath inputs outputs))))))
    (inputs (list python))
    (native-inputs (list doxygen python python-cython python-sphinx))
    (home-page "https://github.com/rareylab/RingDecomposerLib")
    (synopsis "Calculate ring topology descriptions")
    (description "RingDecomposerLib is a library for the calculation of
unique ring families, relevant cycles, the smallest set of smallest rings and
other ring topology descriptions.")
    (license license:bsd-3)))

(define-public pubchem-align3d
  (let ((commit "daefab3dd0c90ca56da9d3d5e375fe4d651e6be3")
        (revision "0"))
    (package
      (name "pubchem-align3d")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ncbi/pubchem-align3d")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1nj1zz5nvn5h3dyj66zi11mmvmzpq3b8y51fld9bkxnsmk17h05m"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f ; circular dependency with rdkit
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'build
              (lambda _
                (invoke "g++"
                        "-o" "libpubchem-align3d.so"
                        "-O2" "-g" "-fPIC" "-shared"
                        "shape_functions1.cpp" "shape_functions2.cpp"
                        "shape_neighbor.cpp")))
            (replace 'install
              (lambda _
                (for-each
                 (lambda (file)
                   (install-file
                    file
                    (string-append #$output "/include/pubchem-align3d")))
                 (find-files "." "\\.hpp"))
                (install-file "libpubchem-align3d.so"
                              (string-append #$output "/lib")))))))
      (home-page "https://github.com/ncbi/pubchem-align3d")
      (synopsis "C++ library for aligning small molecules")
      (description "This is a generic C++ library that can be used to rapidly
align two small molecules in 3D space, with shape - and optionally color -
Tanimoto scoring.")
      (license license:public-domain))))

(define-public rdkit
  (package
    (name "rdkit")
    (version "2023.09.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rdkit/rdkit")
                    (commit
                     (string-append
                      "Release_" (string-replace-substring version "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lgcgijlzzwpfxndsdlx13npdfk7hcii11zg25cvpmzhbpn6vyn8"))
              (patches
               (search-patches "rdkit-unbundle-external-dependencies.patch"))
              (modules '((guix build utils)))
              (snippet
               #~(begin
                   ;; Remove pickle files (only used in tests),
                   ;; as they are compiled programs
                   (for-each
                    (lambda (name)
                      (display (string-append name "\n"))
                      (delete-file name))
                    (find-files "." "\\.pkl(\\.gz)?$"))
                   ;; Remove SQLite data files (can be generated)
                   (delete-file "Data/RDData.sqlt")
                   (delete-file "Data/RDTests.sqlt")))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules (append %cmake-build-system-modules
                                 '((guix build python-build-system)))
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build python-build-system)
                   #:select (add-installed-pythonpath)))
      #:configure-flags
      #~(list "-DRDK_BUILD_AVALON_SUPPORT=ON"
              "-DRDK_BUILD_CAIRO_SUPPORT=ON"
              "-DRDK_BUILD_FREESASA_SUPPORT=ON"
              "-DRDK_BUILD_INCHI_SUPPORT=ON"
              "-DRDK_BUILD_YAEHMOP_SUPPORT=ON"
              (string-append "-DCATCH_DIR="
                             (search-input-directory %build-inputs
                                                     "/include/catch2"))
              "-DRDK_INSTALL_INTREE=OFF"
              "-DRDK_INSTALL_STATIC_LIBS=OFF"
              (string-append
               "-DRDK_OPTIMIZE_POPCNT="
               #$(let ((system (or (%current-target-system)
                                   (%current-system))))
                   (cond
                    ((string-prefix? "x86_64" system) "ON")
                    ((string-prefix? "i686" system) "ON")
                    (else "OFF"))))
              "-DRDK_USE_FLEXBISON=ON"
              (string-append
               "-DCMAKE_INCLUDE_PATH="
               (search-input-directory %build-inputs "/include/avalontoolkit")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-external-dependencies
            (lambda _
              (symlink
               (string-append
                (search-input-file
                 %build-inputs "/share/fonts/truetype/ComicNeue-Regular.ttf"))
               "Data/Fonts/ComicNeue-Regular.ttf")))
          (add-after 'unpack 'fix-inchi-include
            (lambda _
              (substitute* "Code/cmake/Modules/FindInchi.cmake"
                (("inchi_api.h.*\\)") "inchi/inchi_api.h)")
                (("INCHI_LIBRARY NAMES.*\\)")
                 "INCHI_LIBRARY NAMES inchi PATH_SUFFIXES inchi)")
                (("find_library" prev)
                 (string-append
                  "list(APPEND CMAKE_FIND_LIBRARY_SUFFIXES .so.1)\n"
                  prev)))
              (substitute* "External/INCHI-API/inchi.cpp"
                (("<inchi_api.h>") "<inchi/inchi_api.h>"))))
          (add-before 'build 'enable-bytecode-determinism
              (lambda _
                (setenv "PYTHONHASHSEED" "0")
                (setenv "PYTHONDONTWRITEBYTECODE" "1")))
          (add-after 'install 'pre-check
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (with-directory-excursion "../source"
                (invoke "sqlite3" "Data/RDData.sqlt"
                        ".read rdkit/Dbase/test_data/RDData.sqlite")
                (invoke "sqlite3" "Data/RDTests.sqlt"
                        ".read rdkit/Dbase/test_data/RDTests.sqlite")
                (setenv "RDBASE" (canonicalize-path ".")))
              (add-installed-pythonpath inputs outputs)))
          (delete 'check)
          (add-after 'pre-check 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (when tests?
                (let ((job-count (number->string
                                  (if parallel-tests? (parallel-job-count) 1))))
                  (invoke
                   "ctest" "-j" job-count
                   "-E" (string-append
                         "("
                         (string-join
                          '(;; need pickled data
                            "pyDiscreteValueVect" "pySparseIntVect"
                            "graphmoltestPickler" "pyPartialCharges"
                            "substructLibraryTest" "pyFeatures"
                            "pythonTestDirML" "pythonTestDirChem"
                            ;; Catching Python exception fails
                            "pyRanker"
                            ;; Flaky test depending on floating point rounding
                            "testConrec"
                            ) "|")
                         ")")))))))))
    (inputs
     (list avalon-toolkit
           cairo
           coordgenlibs
           font-comic-neue
           freetype
           inchi
           maeparser
           python
           ringdecomposerlib
           sqlite
           yaehmop))
    (native-inputs
     (list bison
           boost
           catch2-3
           eigen
           flex
           freesasa
           pkg-config
           rapidjson
           tar))
    (propagated-inputs
     (list python-numpy python-cairocffi python-pillow))
    (home-page "https://rdkit.org/")
    (synopsis "Collection of cheminformatics software")
    (description "RDKit is a C++ and Python library for cheminformatics, which
includes (among other things) the analysis and modification of molecules in 2D
and 3D and descriptor generation for machine learning.")
    ;; For 32 bit systems, there is a bug in Boost.Python:
    ;; https://github.com/boostorg/python/issues/312. Additionally, several
    ;; other test fail.
    (supported-systems %64bit-supported-systems)
    (license license:bsd-3)))
