;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2022 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2023 Reza Housseini <reza@housseini.me>
;;; Copyright © 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages simulation)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages iso-codes)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1))

(define-public openfoam-org
  (package
    (name "openfoam-org")
    (version "13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenFOAM/OpenFOAM-dev")
             (commit (string-append "version-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10s1x82znfnwspq5iif928j1ks4v0slmmycg6py8xw1vvhjp7arh"))))
    (build-system gnu-build-system)
    (native-inputs (list bison flex git))
    (inputs (list bash-completion
                  boost
                  cgal
                  cgns
                  cli11
                  cmake-minimal
                  coreutils
                  curl
                  double-conversion
                  eigen
                  expat
                  ffmpeg
                  findutils
                  fmt-11
                  freetype
                  gdal
                  gl2ps
                  glew
                  gmp
                  gmsh
                  gnuplot
                  hdf5
                  jsoncpp
                  libjpeg-turbo
                  libogg
                  libpng
                  libharu
                  libtheora
                  libtiff
                  libx11
                  libxml2
                  libxslt
                  libxt
                  lz4
                  metis
                  mpfr
                  ncurses
                  netcdf
                  nlohmann-json
                  openmpi
                  paraview
                  proj
                  protobuf
                  pt-scotch32
                  pugixml
                  python
                  python-mpi4py
                  qt5compat
                  qtbase
                  qtsvg
                  qttools
                  qtwebengine
                  readline
                  trilinos-zoltan
                  utfcpp
                  vtk
                  xz
                  zlib))
    (outputs '("debug" ;~60MB
               "out"))
    (arguments
     (list
      ;; Executable files and shared libraries are located in the 'platforms'
      ;; subdirectory.
      #:strip-directories
      #~(list (string-append "OpenFOAM-"
                             #$(package-version this-package)
                             "/platforms/linux64GccDPInt32Opt/bin")
              (string-append "OpenFOAM-"
                             #$(package-version this-package)
                             "/platforms/linux64GccDPInt32Opt/lib"))

      #:modules '((ice-9 ftw)
                  (ice-9 regex)
                  (ice-9 string-fun)
                  (srfi srfi-1)
                  (guix build gnu-build-system)
                  (guix build utils))

      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'rename-self
            (lambda _
              (let* ((old-build-path (getcwd))
                     (root-path (canonicalize-path (string-append
                                                    old-build-path "/..")))
                     (new-build-path (string-append root-path "/OpenFOAM-"
                                                    #$(package-version
                                                       this-package))))
                (chdir root-path)
                (rename-file old-build-path new-build-path)
                (chdir new-build-path))))
          (add-before 'configure 'patch-version
            (lambda _
              (substitute* "etc/bashrc"
                (("export WM_PROJECT_VERSION=.*$")
                 (string-append "export WM_PROJECT_VERSION="
                                #$(package-version this-package) "\n")))))
          (add-before 'configure 'patch-foamcleanpath
            (lambda _
              (substitute* "bin/foamCleanPath"
                (("getconf")
                 (which "getconf")))))
          (add-before 'configure 'patch-shell-path
            (lambda _
              (substitute* "wmake/makefiles/general"
                (("/bin/sh")
                 (which "sh")))))
          ;; needed for tests
          (add-before 'configure 'patch-HOME-path
            (lambda _
              (setenv "HOME" "/tmp")))
          (add-before 'configure 'patch-bash-completion
            (lambda _
              (substitute* "etc/bashrc"
                ;; do not source bash_completion (gives error)
                (("^.*bash_completion.*$" all)
                 (string-append "#" all)))))
          (add-before 'configure 'patch-rpaths
            (lambda _
              (letrec* ((libraries '(#$(this-package-input "boost")
                                     #$(this-package-input "cgal")
                                     #$(this-package-input "gmp")
                                     #$(this-package-input "metis")
                                     #$(this-package-input "mpfr")
                                     #$(this-package-input "pt-scotch32")
                                     #$(this-package-input "openmpi")
                                     #$(this-package-input "zlib")
                                     #$(this-package-input "paraview")))
                        (rpaths (fold-right (lambda (lib rpaths)
                                              (string-append rpaths "-rpath="
                                                             lib "/lib,")) ""
                                            libraries))
                        (openfoam-lib (string-append #$output
                                       "/share/OpenFOAM-"
                                       #$(package-version this-package)
                                       "/platforms/linux64GccDPInt32Opt/lib"))
                        (ldflags (string-append "-Wl,"
                                                rpaths
                                                "-rpath="
                                                openfoam-lib
                                                ","
                                                "-rpath="
                                                openfoam-lib
                                                "/dummy,"
                                                "-rpath="
                                                openfoam-lib
                                                "/paraview-"
                                                #$(version-major+minor (package-version
                                                                        (this-package-input
                                                                         "paraview"))))))
                (substitute* "wmake/rules/linux64Gcc/c++"
                  (("-fPIC" all)
                   (string-append all " " ldflags))))))
          (add-before 'configure 'patch-paraview
            (lambda _
              (substitute* "etc/config.sh/paraview"
                (("ldd")
                 (which "ldd")))
              (substitute* "bin/paraFoam"
                (("paraview \"\\$@\"")
                 (string-append #$paraview "/bin/paraview \"$@\"")))))
          (add-before 'configure 'patch-mpi
            (lambda _
              (substitute* "etc/config.sh/mpi"
                (("mpicc")
                 (string-append #$(this-package-input "openmpi") "/bin/mpicc")))))
          (add-before 'configure 'patch-paraview
            (lambda _
              (substitute* "etc/config.sh/paraview"
                (("\\$paraviewBinDir/paraview")
                 "$paraviewBinDir/.paraview-real")
                (("libpqCore-pv")
                 "libpqCore"))))
          (add-before 'configure 'patch-vtk-include-path
            (lambda _
              (let* ((vtk-version #$(version-major+minor (package-version (this-package-input
                                                                           "vtk"))))
                     (vtk-inc (string-append #$(this-package-input "vtk")
                                             "/include/vtk-" vtk-version))
                     (vtk-inc-flag (string-append "-I" vtk-inc)))
                (substitute* "wmake/makefiles/general"
                  (("SYS_INC         =" all)
                   (string-append all " " vtk-inc-flag))))))
          (replace 'configure
            (lambda _
              (substitute* "etc/bashrc"
                (("export ParaView_TYPE=.*$")
                 "export ParaView_TYPE=system\n")
                (("export SCOTCH_TYPE=.*$")
                 "export SCOTCH_TYPE=system\n")
                (("export METIS_TYPE=.*$")
                 "export METIS_TYPE=system\n")
                (("export ZOLTAN_TYPE=.*$")
                 "export ZOLTAN_TYPE=system\n"))
              (substitute* "etc/config.sh/scotch"
                (("export SCOTCH_ARCH_PATH=.*$")
                 (string-append "export SCOTCH_ARCH_PATH="
                                #$(this-package-input "pt-scotch32") "\n")))
              (substitute* "etc/config.sh/metis"
                (("export METIS_ARCH_PATH=.*$")
                 (string-append "export METIS_ARCH_PATH="
                                #$(this-package-input "metis") "\n")))
              (substitute* "etc/config.sh/zoltan"
                (("export ZOLTAN_ARCH_PATH=.*$")
                 (string-append "export ZOLTAN_ARCH_PATH="
                                #$(this-package-input "trilinos-zoltan") "\n")))))
          (replace 'build
            (lambda _
              ;; compile OpenFOAM libraries and applications
              (invoke "bash" "-c"
                      (format #f "source ./etc/bashrc && ./Allwmake -j~a"
                              (parallel-job-count)))))
          (add-after 'build 'cleanup
            ;; Avoid unnecessary, voluminous object and dep files.
            (lambda _
              (when (file-exists? "platforms/linux64GccDPInt32Opt/src")
                (delete-file-recursively "platforms/linux64GccDPInt32Opt/src"))
              (when (file-exists?
                     "platforms/linux64GccDPInt32OptSYSTEMOPENMPI")
                (delete-file-recursively
                 "platforms/linux64GccDPInt32OptSYSTEMOPENMPI"))
              (for-each delete-file
                        (find-files "." "\\.o$"))
              (for-each delete-file
                        (find-files "." "\\.dep$"))
              ;; Remove spurious files in src tree
              ;; (invoke "bash" "-c" "source ./etc/bashrc && wclean all")
              ))
          (add-before 'check 'disable-failing-tests
            (lambda _
              ;; disable failing test
              (substitute* "test/postProcessing/channel/Allrun"
                (("^.*foam.*$" all)
                 (string-append "#" all "\n")))
              (substitute* "test/Lagrangian/boundaries/system/decomposeParDict"
                (("numberOfSubdomains  6")
                 "numberOfSubdomains  4")
                (("n               \\(3 2 1\\)")
                 "n               (2 2 1)"))
              (substitute* "test/Lagrangian/parabolic/system/decomposeParDict"
                (("numberOfSubdomains  6")
                 "numberOfSubdomains  4")
                (("n               \\(3 2 1\\)")
                 "n               (2 2 1)"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (when (file-exists? "test")
                  (with-directory-excursion "test"
                    (invoke "bash" "-c"
                            (format #f "source ../etc/bashrc && ./Allrun -j~a"
                                    (parallel-job-count)))))
                ;; too many tutorials are failing
                ;; (with-directory-excursion "tutorials"
                ;; (invoke "bash" "-c" "source ../etc/bashrc && ./Alltest"))
                )))
          (replace 'install
            (lambda _
              (let ((install-path (string-append #$output "/share/OpenFOAM-"
                                                 #$(package-version
                                                    this-package))))
                (mkdir-p install-path) ;create install directory
                ;; move contents of build directory to install directory
                (copy-recursively "." install-path))))
          (add-after 'install 'add-symbolic-link
            (lambda _
              (let* ((bin (string-append #$output "/bin"))
                     (lib (string-append #$output "/lib"))
                     (etc (string-append #$output "/etc"))
                     (openfoam (string-append #$output "/share/OpenFOAM-"
                                              #$(package-version this-package)))
                     (build-bin (string-append openfoam
                                 "/platforms/linux64GccDPInt32Opt/bin"))
                     (build-lib (string-append openfoam
                                 "/platforms/linux64GccDPInt32Opt/lib"))
                     (foam-bin (string-append openfoam "/bin")))
                ;; add symbolic links in standard 'bin' directory
                (mkdir-p bin)
                (for-each (lambda (file)
                            (symlink file
                                     (string-append bin "/"
                                                    (basename file))))
                          (find-files build-bin))
                (for-each (lambda (file)
                            (symlink file
                                     (string-append bin "/"
                                                    (basename file))))
                          (find-files foam-bin))
                ;; add symbolic link for standard 'lib' directory
                (symlink build-lib lib)
                ;; add symbolic link for bashrc file
                (mkdir-p etc)
                (symlink (string-append openfoam "/etc/bashrc")
                         (string-append etc "/bashrc"))) #t))
          (add-after 'add-symbolic-link 'wrap
            (lambda* (#:key python inputs outputs #:allow-other-keys)
              (let* ((bin (string-append #$output "/bin"))
                     (openfoam-version #$(package-version this-package))
                     (openfoam-root (string-append #$output "/share/OpenFOAM-"
                                                   openfoam-version))
                     (openmpi-version #$(package-version (this-package-input
                                                          "openmpi")))
                     (paraview-version #$(package-version (this-package-input
                                                           "paraview")))
                     (paraview-version-major+minor #$(version-major+minor (package-version
                                                                           (this-package-input
                                                                            "paraview")))))
                (for-each (lambda (program)
                            ;; wrap the programs with all the variables created by sourcing
                            ;; etc/bashrc
                            (wrap-program program
                              `("WM_PROJECT_INST_DIR" =
                                (,openfoam-root))
                              `("WM_PROJECT_DIR" =
                                (,openfoam-root))
                              '("WM_PROJECT" =
                                ("OpenFOAM"))
                              `("WM_PROJECT_VERSION" =
                                (,openfoam-version))
                              '("WM_COMPILER_TYPE" =
                                ("system"))
                              '("WM_COMPILER" =
                                ("Gcc"))
                              '("WM_ARCH_OPTION" =
                                ("64"))
                              '("WM_PRECISION_OPTION" =
                                ("DP"))
                              '("WM_LABEL_SIZE" =
                                ("32"))
                              '("WM_COMPILE_OPTION" =
                                ("Opt"))
                              '("WM_MPLIB" =
                                ("SYSTEMOPENMPI"))
                              '("WM_OSTYPE" =
                                ("POSIX"))
                              '("WM_OPTIONS" =
                                ("linux64GccDPInt32Opt"))
                              `("WM_PROJECT_USER_DIR" =
                                (,(string-append "$HOME/OpenFOAM/$USER-"
                                                 openfoam-version)))
                              `("WM_THIRD_PARTY_DIR" =
                                (,(string-append openfoam-root "/ThirdParty-"
                                                 openfoam-version)))
                              '("WM_LABEL_OPTION" =
                                ("Int32"))
                              '("WM_LINK_LANGUAGE" =
                                ("c++"))
                              '("WM_COMPILER_LIB_ARCH" =
                                ("64"))
                              `("WM_DIR" =
                                (,(string-append openfoam-root "/wmake")))
                              '("WM_LDFLAGS" =
                                ("-m64"))
                              '("WM_CC" =
                                ("gcc"))
                              '("WM_CFLAGS" =
                                ("-m64 -fPIC"))
                              '("WM_CXX" =
                                ("g++"))
                              '("WM_CXXFLAGS" =
                                ("-m64 -fPIC -std=c++0x"))

                              `("FOAM_INST_DIR" =
                                (,openfoam-root))
                              `("FOAM_APP" =
                                (,(string-append openfoam-root "/applications")))
                              `("FOAM_SRC" =
                                (,(string-append openfoam-root "/src")))
                              `("FOAM_ETC" =
                                (,(string-append openfoam-root "/etc")))
                              `("FOAM_TUTORIALS" =
                                (,(string-append openfoam-root "/tutorials")))
                              `("FOAM_UTILITIES" =
                                (,(string-append openfoam-root
                                                 "/applications/utilities")))
                              `("FOAM_SOLVERS" =
                                (,(string-append openfoam-root
                                                 "/applications/solvers")))
                              `("FOAM_MPI" =
                                (,(string-append "openmpi-" openmpi-version)))
                              `("FOAM_RUN" =
                                (,(string-append "$HOME/OpenFOAM/$USER-"
                                                 openfoam-version "/run")))
                              `("FOAM_EXT_LIBBIN" =
                                (,(string-append openfoam-root "/ThirdParty-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32/lib")))
                              `("FOAM_APPBIN" =
                                (,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/bin")))
                              `("FOAM_JOB_DIR" =
                                (,(string-append openfoam-root "/jobControl")))
                              `("FOAM_LIBBIN" =
                                (,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/lib")))
                              `("FOAM_SITE_LIBBIN" =
                                (,(string-append openfoam-root "/site/"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/lib")))
                              `("FOAM_SITE_APPBIN" =
                                (,(string-append openfoam-root "/site/"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/bin")))
                              `("FOAM_USER_LIBBIN" =
                                (,(string-append "$HOME/OpenFOAM/$USER-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/lib")))
                              `("FOAM_USER_APPBIN" =
                                (,(string-append "$HOME/OpenFOAM/$USER-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/bin")))
                              `("FOAM_MODULES" =
                                (,(string-append openfoam-root
                                                 "/applications/modules")))
                              '("FOAM_SIGFPE" =
                                (""))
                              '("FOAM_SETTINGS" =
                                (""))

                              `("ParaView_INCLUDE_DIR" =
                                (,(string-append #$(this-package-input
                                                    "paraview")
                                                 "/include/paraview-"
                                                 paraview-version-major+minor)))
                              '("ParaView_GL" =
                                ("mesa"))
                              `("ParaView_VERSION" =
                                (,paraview-version))
                              `("ParaView_MAJOR" =
                                (,paraview-version-major+minor))
                              `("ParaView_DIR" =
                                (,#$(this-package-input "paraview")))
                              `("PV_PLUGIN_PATH" =
                                (,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/lib/paraview-"
                                   paraview-version-major+minor)))

                              `("MPI_ARCH_PATH" =
                                (,#$(this-package-input "openmpi")))
                              `("OPAL_PREFIX" =
                                (,#$(this-package-input "openmpi")))
                              '("MPI_BUFFER_SIZE" =
                                ("20000000"))

                              `("LD_LIBRARY_PATH" prefix
                                (,(string-append openfoam-root "/ThirdParty-"
                                   openfoam-version
                                   "/platforms/linux64Gcc/gperftools-svn/lib") ,
                                 (string-append #$(this-package-input
                                                   "paraview") "/lib")
                                 ,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/lib/openmpi-"
                                   openmpi-version)
                                 ,(string-append openfoam-root "/ThirdParty-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32/lib/openmpi-"
                                   openmpi-version)
                                 ,(string-append #$(this-package-input
                                                    "openmpi") "/lib")
                                 ,(string-append #$(this-package-input
                                                    "openmpi") "/lib64")
                                 ,(string-append "$HOME/OpenFOAM/$USER-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/lib")
                                 ,(string-append openfoam-root "/site/"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/lib")
                                 ,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/lib")
                                 ,(string-append openfoam-root "/ThirdParty-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32/lib")
                                 ,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/lib/openmpi-system")))
                              `("PATH" prefix
                                (,(string-append openfoam-root "/ThirdParty-"
                                   openfoam-version
                                   "/platforms/linux64Gcc/gperftools-svn/bin") ,
                                 (string-append #$(this-package-input
                                                   "paraview") "/bin")
                                 ,(string-append openfoam-root "/ThirdParty-"
                                   openfoam-version
                                   "/platforms/linux64Gcc/cmake-*/bin")
                                 ,(string-append #$(this-package-input
                                                    "openmpi") "/bin")
                                 ,(string-append openfoam-root "/bin")
                                 ,(string-append openfoam-root "/wmake")
                                 ,(string-append "$HOME/OpenFOAM/$USER-"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/bin")
                                 ,(string-append openfoam-root "/site/"
                                   openfoam-version
                                   "/platforms/linux64GccDPInt32Opt/bin")
                                 ,(string-append openfoam-root
                                   "/platforms/linux64GccDPInt32Opt/bin")))))
                          (find-files bin))))))))
    ;; Note:
    ;; Tutorial files are installed read-only in /gnu/store.
    ;; To allow write permissions on files copied from the store a
    ;; 'chmod' step is needed before running the applications.  For
    ;; example, from a user's login:
    ;; $ source $(foamEtcFile bashrc)
    ;; $ mkdir -p $FOAM_RUN
    ;; $ cd $FOAM_RUN
    ;; $ cp -r $FOAM_TUTORIALS/incompressible/simpleFoam/pitzDaily .
    ;; $ cd pitzDaily
    ;; $ chmod -R u+w .
    ;; $ blockMesh
    (home-page "https://openfoam.org")
    (synopsis "Framework for numerical simulation of fluid flow")
    (description
     "OpenFOAM provides a set of solvers and methods for tackling
problems in the field of Computational Fluid Dynamics (CFD).  It is written in
C++.  Governing equations such as the Navier-Stokes equations can be solved in
integral form.  Physical processes such as phase change, droplet transport and
chemical reaction can be modelled.  Numerical methods are included to deal with
sharp gradients, such as those encountered in flows with shock waves and flows
with gas/liquid interfaces.  Large problems may be split into smaller, connected
problems for efficient solution on parallel systems.")
    (license license:gpl3+)))

(define-public openfoam-com
  ;; This is a fork of 'openfoam-org', maintained separately.
  (package
    (inherit openfoam-org)
    (name "openfoam-com")
    (version "2506")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/openfoam/core/openfoam")
             (commit (string-append "OpenFOAM-v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g8cjiqkk4yp5kmxyv8z43aaalr5459i54yaqmkn6qr38cjrhnd2"))))
    (arguments
     ;; Since 'arguments' is a function of the package's version, define
     ;; 'parent' such that the 'arguments' thunk gets to see the right
     ;; version.
     (let ((parent (package
                     (inherit openfoam-org)
                     (version (package-version this-package)))))
       (substitute-keyword-arguments (package-arguments parent)
         ((#:phases phases)
          #~(modify-phases #$phases
              (delete 'disable-failing-tests))))))
    (synopsis
     "Framework for numerical simulation of fluid flow (from openfoam.com)")
    (home-page "https://www.openfoam.com")))

(define-public open-simulation-interface
  (package
    (name "open-simulation-interface")
    (version "3.5.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/"
                                        "OpenSimulationInterface/"
                                        "open-simulation-interface"))
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09vclrvsawx608kk0vnzywr71xn11qzwxzh2j508zjfn0kvhyx7q"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f         ; tests are for the python package
                     #:phases
                     #~(modify-phases %standard-phases
                         (add-after 'unpack 'fix-cmake
                           (lambda _
                             (substitute* "CMakeLists.txt"
                               (("-targets\\.cmake") "_targets.cmake")))))))
    (native-inputs (list protobuf))
    (home-page
     "https://github.com/OpenSimulationInterface/open-simulation-interface")
    (synopsis "Generic interface for environmental perception")
    (description "The Open Simulation Interface is a generic interface based on
Google's protocol buffers for the environmental perception of automated driving
functions in virtual scenarios.")
    (license license:mpl2.0)))

(define-public python-open-simulation-interface
  (package/inherit open-simulation-interface
    (name "python-open-simulation-interface")
    (build-system pyproject-build-system)
    (arguments '())
    (native-inputs
     (modify-inputs (package-native-inputs open-simulation-interface)
       (append python-pytest python-setuptools)))
    (propagated-inputs
     (list python-pyyaml
           python-protobuf))))

(define-public esmini
  (package
    (name "esmini")
    (version "2.37.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/esmini/esmini")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "esmini-use-pkgconfig.patch"))
              (modules '((guix build utils) (ice-9 ftw)))
              (snippet
               #~(with-directory-excursion "externals"
                   (for-each
                    (lambda (dir) (unless (member dir '("." ".." "expr"))
                               (delete-file-recursively dir)))
                    (scandir "."))))
              (sha256
               (base32
                "07pwa34nf0b4ihb9fn1pvfi0b39hd8r630nfa6v3a17dsy66a730"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DDYN_PROTOBUF=TRUE"
              ;; Missing implot package
              "-DUSE_IMPLOT=FALSE")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (substitute* "support/cmake/common/locations.cmake"
                (("\\$\\{CMAKE_SOURCE_DIR\\}/bin")
                 (string-append (assoc-ref outputs "out") "/bin")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (with-directory-excursion "EnvironmentSimulator/Unittest/"
                (for-each invoke (find-files "_test$")))))
          (add-after 'install 'move-libraries
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p (string-append out "/lib"))
                (with-directory-excursion (string-append out "/bin")
                  (for-each
                   (lambda (f)
                     (rename-file f (string-append out "/lib/"
                                                   (basename f))))
                   (find-files "." "\\.(a|so)$")))))))))
    (inputs (list mesa
                  openscenegraph `(,openscenegraph "pluginlib")
                  open-simulation-interface
                  protobuf pugixml sumo))
    (native-inputs (list googletest pkg-config))
    (home-page "https://github.com/esmini/esmini")
    (synopsis "Basic OpenSCENARIO player")
    (description "@command{esmini} is a tool to play OpenSCENARIO files.
It is provided as both a standalone application and a shared library and has
some support for generating and analysing traffic scenarios..")
    (license license:mpl2.0)))

(define-public python-fenics-dijitso
  (package
    (name "python-fenics-dijitso")
    (version "2019.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fenics-dijitso" version))
       (sha256
        (base32 "0lhqsq8ypdak0ahr2jnyvg07yrqp6wicjxi6k56zx24wp3qg60sc"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'mpi-setup
            #$%openmpi-setup)
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (setenv "HOME" "/tmp")
                (with-directory-excursion "test"
                  ;; Disable parallel tests to avoid race condition.  See
                  ;; https://github.com/pytest-dev/pytest-cov/issues/237.
                  (substitute* "runtests.sh"
                    (("for p in 1 4 8 16; do") "for p in 1; do"))
                  (invoke "./runtests.sh"))))))))
    (native-inputs
     (list python-pytest
           python-pytest-cov
           python-setuptools
           python-wheel))
    (inputs
     (list openmpi python-numpy))
    (propagated-inputs
     (list python-mpi4py))
    (home-page "https://bitbucket.org/fenics-project/dijitso/")
    (synopsis "Distributed just-in-time building of shared libraries")
    (description
     "Dijitso provides a core component of the @code{FEniCS} framework,
namely the just-in-time compilation of C++ code that is generated from
Python modules.  It is called from within a C++ library, using ctypes
to import the dynamic shared library directly.

As long as the compiled code can provide a simple factory function to
a class implementing a predefined C++ interface, there is no limit to
the complexity of that interface.  Parallel support depends on the
@code{mpi4py} interface.")
    (license license:lgpl3+)))

(define-public python-fenics-ufl
  (package
    (name "python-fenics-ufl")
    (version "2025.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fenics_ufl" version))
       (sha256
        (base32 "1k2r7s1c7dqlgkah4jr08lqy4z17h49r94yxjm7ca16vgvqvq1xg"))))
    (build-system pyproject-build-system)
    (inputs (list python-numpy))
    (native-inputs (list python-pytest python-setuptools))
    (home-page "https://bitbucket.org/fenics-project/ufl/")
    (synopsis "Unified language for form-compilers")
    (description
     "The Unified Form Language (UFL) is a domain specific language for
declaration of finite element discretizations of variational forms.  More
precisely, it defines a flexible interface for choosing finite element spaces
and defining expressions for weak forms in a notation close to mathematical
notation.

UFL is part of the FEniCS Project.")
    (license license:lgpl3+)))

(define-public python-fenics-ufl-2019
  (package/inherit python-fenics-ufl
    (name "python-fenics-ufl")
    (version "2019.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fenics-ufl" version))
       (sha256
        (base32 "10dz8x3lm68x2w3kkqcjask38h0zkhhak26jdbkppr8g9y8wny7p"))))))

;; XXX: This package is quite dated and upstream no longer maintains it: "This
;; repository was archived by the owner on Feb 21, 2022. It is now read-only."
;; <https://bitbucket.org/fenics-project/fiat> ->
;; <https://github.com/FEniCS/fiat> while providing a new refactored fork
;; <https://github.com/firedrakeproject/fiat>.
(define-public python-fenics-fiat
  (package
    (name "python-fenics-fiat")
    (version "2019.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fenics-fiat" version))
       (sha256
        (base32 "13sc7lma3d2mh43an7i4kkdbbk4cmvxjk45wi43xnjd7qc38zg4b"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; FIXME: three FIAT test modules are known to fail with recent
         ;; versions of pytest (>= 4).  These are skipped for FIAT version
         ;; 2019.1.0 pending an upstream pull request. For details see request
         ;; #59 at https://bitbucket.org/fenics-project/fiat/.
         "--ignore=test/regression/"
         "--ignore=test/unit/test_quadrature.py"
         "--ignore=test/unit/test_reference_element.py"
         "-k" (string-join
               (list "not test_nodality"
                     "test_basis_values")
               " and not "))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-numpy
           python-sympy))
    (home-page "https://bitbucket.org/fenics-project/fiat/")
    (synopsis "Tabulation of finite element function spaces")
    (description
     "The FInite element Automatic Tabulator (FIAT) supports generation of
arbitrary order instances of the Lagrange elements on lines, triangles, and
tetrahedra.  It is also capable of generating arbitrary order instances of
Jacobi-type quadrature rules on the same element shapes.  Further, H(div) and
H(curl) conforming finite element spaces such as the families of
Raviart-Thomas, Brezzi-Douglas-Marini and Nedelec are supported on triangles
and tetrahedra.  Upcoming versions will also support Hermite and nonconforming
elements.

FIAT is part of the FEniCS Project.")
    (license license:lgpl3+)))

(define-public python-fenics-ffc
  (package
    (name "python-fenics-ffc")
    (version "2019.1.0.post0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "fenics-ffc" version))
       (sha256
        (base32 "1f2a44ha65fg3a1prrbrsz4dgvibsv0j5c3pi2m52zi93bhwwgg9"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda _
              (setenv "HOME" (getcwd))
              (with-directory-excursion "test"
                ;; FIXME: the tests in subdirectory
                ;; 'unit/ufc/finite_element' require the ffc_factory
                ;; extension module.  This module, located in the 'libs'
                ;; subdirectory, needs to be built and made accessible
                ;; prior to running the tests.
                (invoke "pytest" "unit/" "--ignore=unit/ufc/")
                (with-directory-excursion "uflacs"
                  (invoke "pytest" "unit/"))))))))
    (native-inputs (list python-pytest python-setuptools))
    (propagated-inputs (list python-fenics-dijitso python-fenics-fiat
                             python-fenics-ufl-2019))
    (home-page "https://bitbucket.org/fenics-project/ffc/")
    (synopsis "Compiler for finite element variational forms")
    (description
     "The FEniCS Form Compiler (FFC) is a compiler for finite element
variational forms.  From a high-level description of the form, it generates
efficient low-level C++ code that can be used to assemble the corresponding
discrete operator (tensor).  In particular, a bilinear form may be assembled
into a matrix and a linear form may be assembled into a vector.  FFC may be
used either from the command line (by invoking the @code{ffc} command) or as a
Python module (@code{import ffc}).

FFC is part of the FEniCS Project.")
    ;; There are two files released with a public domain licence;
    ;; ufc.h and ufc_geometry.h, in subdirectory 'ffc/backends/ufc'.
    (license (list license:public-domain license:lgpl3+))))

(define-public fenics-dolfin
  (package
    (name "fenics-dolfin")
    (version "2019.1.0.post0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://bitbucket.org/fenics-project/dolfin/get/"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32 "1m91hwcq5gfj4qqswp8l8kj58nia48f0n4kq13w0xqj4biq7rla0"))
       (patches (search-patches "fenics-dolfin-algorithm.patch"
                                "fenics-dolfin-demo-init.patch"
                                "fenics-dolfin-boost.patch"
                                "fenics-dolfin-config-slepc.patch"
                                "fenics-dolfin-hdf5-version-check.patch"
                                "fenics-dolfin-integer-types.patch"))
       (modules '((guix build utils)))
       (snippet '(begin
                   ;; Make sure we don't use the bundled test framework.
                   (delete-file-recursively "test/unit/cpp/catch")
                   (substitute* "test/unit/cpp/main.cpp"
                     ;; Use standard search paths for 'catch' header file.
                     (("#include.*")
                      "#include <catch.hpp>\n"))
                   (substitute* "test/unit/cpp/CMakeLists.txt"
                     ;; Specify directory to find the header file.
                     (("(^set\\(CATCH_INCLUDE_DIR ).*(/catch\\))" _ front back)
                      (string-append front "$ENV{CATCH_DIR}/include" back "\n")))))))
    (build-system cmake-build-system)
    (inputs (list openblas
                  boost-1.83
                  eigen
                  hdf5-parallel-openmpi
                  libxml2
                  openmpi
                  python-3
                  pt-scotch32
                  suitesparse
                  zlib))
    (native-inputs (list catch-framework pkg-config))
    (propagated-inputs (list python-fenics-ffc petsc-openmpi slepc-openmpi))
    (arguments
     (list
      #:configure-flags
      #~`("-DDOLFIN_ENABLE_DOCS:BOOL=OFF" "-DDOLFIN_ENABLE_HDF5:BOOL=ON"
          "-DDOLFIN_ENABLE_MPI:BOOL=ON"
          "-DDOLFIN_ENABLE_PARMETIS:BOOL=OFF"
          "-DDOLFIN_ENABLE_SCOTCH:BOOL=ON"
          "-DDOLFIN_ENABLE_SUNDIALS:BOOL=OFF"
          "-DDOLFIN_ENABLE_TRILINOS:BOOL=OFF")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'patch-usr-bin-file 'mpi-setup
            #$%openmpi-setup)
          (add-after 'patch-source-shebangs 'set-paths
            (lambda _
              ;; Define paths to store locations.
              (setenv "BLAS_DIR"
                      #$(this-package-input "openblas"))
              (setenv "CATCH_DIR"
                      #$(this-package-input "catch"))
              (setenv "LAPACK_DIR"
                      #$(this-package-input "openblas"))
              (setenv "PETSC_DIR"
                      #$(this-package-input "petsc"))
              (setenv "SLEPC_DIR"
                      #$(this-package-input "slepc"))
              (setenv "SCOTCH_DIR"
                      #$(this-package-input "scotch"))
              (setenv "UMFPACK_DIR"
                      #$(this-package-input "suitesparse"))))
          (add-before 'check 'pre-check
            (lambda _
              ;; The Dolfin repository uses git-lfs, whereby web links are
              ;; substituted for large files.  Only the links are downloaded.
              ;; The tests that require the absent meshes cannot run and are
              ;; skipped.
              ;;
              ;; One serial test fails and is skipped.
              ;; i) demo_multimesh-stokes_serial:
              ;; Warning: Found no facets matching domain for boundary
              ;; condition.
              ;;
              ;; One mpi test fails and is skipped.
              ;; i) demo_stokes-iterative_mpi:
              ;; The MPI_Comm_rank() function was called before MPI_INIT was
              ;; invoked.
              (call-with-output-file "CTestCustom.cmake"
                (lambda (port)
                  (display (string-append "set(CTEST_CUSTOM_TESTS_IGNORE "
                            "demo_bcs_serial "
                            "demo_bcs_mpi "
                            "demo_eigenvalue_serial "
                            "demo_eigenvalue_mpi "
                            "demo_navier-stokes_serial "
                            "demo_navier-stokes_mpi "
                            "demo_stokes-taylor-hood_serial "
                            "demo_stokes-taylor-hood_mpi "
                            "demo_subdomains_serial "
                            "demo_advection-diffusion_serial "
                            "demo_advection-diffusion_mpi "
                            "demo_auto-adaptive-navier-stokes_serial "
                            "demo_contact-vi-snes_serial "
                            "demo_contact-vi-snes_mpi "
                            "demo_contact-vi-tao_serial "
                            "demo_contact-vi-tao_mpi "
                            "demo_curl-curl_serial "
                            "demo_curl-curl_mpi "
                            "demo_dg-advection-diffusion_serial "
                            "demo_dg-advection-diffusion_mpi "
                            "demo_elasticity_serial "
                            "demo_elasticity_mpi "
                            "demo_elastodynamics_serial "
                            "demo_elastodynamics_mpi "
                            "demo_lift-drag_serial "
                            "demo_lift-drag_mpi "
                            "demo_mesh-quality_serial "
                            "demo_mesh-quality_mpi "
                            "demo_multimesh-stokes_serial "
                            "demo_stokes-iterative_mpi"
                            ")\n") port)))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "unittests")
                (invoke "make" "demos")
                (invoke "ctest" "-R" "unittests")
                (invoke "ctest" "-R" "demo" "-R" "serial")
                (invoke "ctest" "-R" "demo" "-R" "mpi")))))))
    (home-page "https://bitbucket.org/fenics-project/dolfin/")
    (synopsis "Problem solving environment for differential equations")
    (description
     "DOLFIN is a computational framework for finding numerical
solutions to problems described by differential equations.  Numerical
models in DOLFIN are constructed using general families of finite
elements.  Data structures are provided for discretizing the governing
system on a computational mesh.  A compact syntax, similar to
mathematical notation, is made available for defining function spaces
and expressing variational forms.  Interfaces to specialized matrix
solvers are provided for solving the resultant linear systems.

@code{fenics-dolfin} is part of the FEniCS project.  It is the C++
user interface to the FEniCS core components and external libraries.")
    ;; The source code for the DOLFIN C++ library is licensed under the
    ;; GNU Lesser General Public License, version 3 or later, with the
    ;; following exceptions:
    ;;
    ;; public-domain: dolfin/geometry/predicates.cpp
    ;; dolfin/geometry/predicates.h
    ;;
    ;; zlib:          dolfin/io/base64.cpp
    ;; dolfin/io/base64.h
    ;;
    ;; expat:         dolfin/io/pugiconfig.hpp
    ;; dolfin/io/pugixml.cpp
    ;; dolfin/io/pugixml.hpp
    (license (list license:public-domain license:zlib license:expat
                   license:lgpl3+))))

(define-public fenics
  (package/inherit fenics-dolfin
    (name "fenics")
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         "unit"
         ;; This test fails and is ignored.
         "--deselect"
         "unit/nls/test_PETScSNES_solver.py::test_snes_set_from_options"
         ;; These tests are flaky.
         "--deselect=unit/common/test_timer.py::test_context_manager_named"
         "--deselect=unit/common/test_timer.py::test_context_manager_anonymous"
         ;; FIXME: Tests with binary encoded hdf5 files fail with a
         ;; segfault.  See fenics-project DOLFIN commit 6fbc9fb.
         "--ignore=unit/io/test_XDMF.py")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-requirements
            (lambda _
              (substitute* "python/setup.py"
                (("pybind11==") "pybind11>="))))
          (add-after 'unpack 'set-dolfin-pc-file-name
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Instead of cluttering the user's PKG_CONFIG_PATH environment
              ;; variable, hard-code the 'dolfin.pc' absolute file name.
              (let ((pc-file (search-input-file inputs
                                                "/lib/pkgconfig/dolfin.pc")))
                (substitute* "python/dolfin/jit/jit.py"
                  (("pkgconfig\\.parse\\(\"dolfin\"\\)")
                   (string-append "pkgconfig.parse(\"" pc-file "\")"))))))
          (add-after 'patch-source-shebangs 'set-paths
            (lambda _
              ;; Define paths to store locations.
              (setenv "PYBIND11_DIR" #$(this-package-input "pybind11"))
              ;; Move to python sub-directory.
              (chdir "python")))
          (add-after 'build 'mpi-setup #$%openmpi-setup)
          (add-before 'check 'pre-check
            (lambda _
              ;; Exclude three tests that generate 'NotImplementedError' in
              ;; matplotlib v3.1.2. See
              ;; <https://github.com/matplotlib/matplotlib/issues/15382>.
              ;; Also exclude tests that require meshes supplied by git-lfs.
              (substitute* "demo/test.py"
                (("(.*stem !.*)" line)
                 (format #f "~a
excludeList = [~{ '~a',~% ~}]
demos = [d for d in demos if d[0].stem not in excludeList]~%"
                         line
                         (list "built-in-meshes"
                               "hyperelasticity"
                               "elasticity"
                               "multimesh-quadrature"
                               "multimesh-marking"
                               "mixed-poisson-sphere"
                               "mesh-quality"
                               "lift-drag"
                               "elastodynamics"
                               "dg-advection-diffusion"
                               "curl-curl"
                               "contact-vi-tao"
                               "contact-vi-snes"
                               "collision-detection"
                               "buckling-tao"
                               "auto-adaptive-navier-stokes"
                               "advection-diffusion"
                               "subdomains"
                               "stokes-taylor-hood"
                               "stokes-mini"
                               "navier-stokes"
                               "eigenvalue"))))
              ;; Do not test for expired numpy aliases.
              (substitute* "test/unit/la/test_vector.py"
                ((" numpy.float\\(42.0\\),") "")
                (("^.*numpy.int\\(42.0\\).*$") ""))
              (substitute* "test/unit/la/test_la_basic.py"
                ((" int,") " ")
                (("\\[int,") "["))
              (setenv "HOME" (getcwd))
              ;; Restrict OpenBLAS to MPI-only in preference to MPI+OpenMP.
              (setenv "OPENBLAS_NUM_THREADS" "1")))
          (replace 'check
            (lambda* (#:key tests? test-flags #:allow-other-keys)
              (when tests?
                (with-directory-excursion "test"
                  (apply invoke "pytest" test-flags)))))
          (add-after 'install 'install-demo-files
            (lambda _
              (let* ((demos (string-append #$output
                                           "/share/python-dolfin/demo")))
                (mkdir-p demos)
                (with-directory-excursion "demo"
                  (for-each (lambda (file)
                              (let* ((dir (dirname file))
                                     (tgt-dir (string-append demos "/" dir)))
                                (unless (equal? "." dir)
                                  (mkdir-p tgt-dir)
                                  (install-file file tgt-dir))))
                            (find-files "." ".*\\.(py|gz|xdmf)$")))))))))
    (inputs
     (modify-inputs (package-inputs fenics-dolfin)
       (delete "python")
       (prepend pybind11 python-matplotlib)))
    (native-inputs
     (modify-inputs (package-native-inputs fenics-dolfin)
       (prepend cmake-minimal python-ply python-pytest python-decorator
                python-setuptools)))
    (propagated-inputs
     (list fenics-dolfin
           python-petsc4py
           python-slepc4py
           ;; 'dolfin/jit/jit.py' parses 'dolfin.pc' at runtime.
           python-pkgconfig))
    (home-page "https://fenicsproject.org/")
    (synopsis "High-level environment for solving differential equations")
    (description
     "@code{fenics} is a computing platform for solving general classes of
problems that involve differential equations.  @code{fenics} facilitates
access to efficient methods for dealing with ordinary differential
equations (ODEs) and partial differential equations (PDEs).  Systems of
equations such as these are commonly encountered in areas of engineering,
mathematics and the physical sciences.  It is particularly well-suited to
problems that can be solved using the Finite Element Method (FEM).

@code{fenics} is the top level of the set of packages that are developed
within the FEniCS project.  It provides the python user interface to the
FEniCS core components and external libraries.")
    (license license:lgpl3+)))

(define-public fullswof-2d
  (let ((revision 505)
        (release "1.09.01"))
    (package
      (name "fullswof-2d")
      (version release)
      (source (origin
               (method svn-fetch)
               (uri (svn-reference
                     (url (string-append "https://subversion.renater.fr/"
                                         "anonscm/svn/fullswof-2d/tags/"
                                         "release-" version))
                     (revision revision)))
               (file-name (string-append "fullswof-2d-" version "-checkout"))
               (sha256
                (base32
                 "16v08dx7h7n4wyddzbwimazwyj74ynis12mpjfkay4243npy44b8"))))
      (build-system gnu-build-system)
      (native-inputs
       (list inetutils)) ; for 'hostname', used in the check phase
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (delete 'configure)     ; no configure script
           (add-after 'build 'build-tools
             (lambda _
               (with-directory-excursion "Tools/ConvertFormat"
                 (invoke "make" "../../bin/asc2xyz")
                 (invoke "make" "../../bin/xyz2asc"))
               (with-directory-excursion "Tools/ExtractWindow"
                 (invoke "make" "../../bin/cropxyz"))
               #t))
           (replace 'check         ; no check target
             (lambda _
               (invoke "make" "benchref")))
           (replace 'install       ; no install target
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin"))
                      (doc (string-append
                            out "/share/doc/" ,name "-" ,version))
                      (examples (string-append doc "/Examples")))
                 (with-directory-excursion "bin"
                   (for-each (lambda (binary) (install-file binary bin))
                             (list "asc2xyz" "xyz2asc" "cropxyz"
                                   "FullSWOF_2D")))
                 (with-directory-excursion "doc"
                   (for-each (lambda (pdf) (install-file pdf doc))
                             (list "Documentation.pdf" "refman.pdf")))
                 (with-directory-excursion "Tools"
                   (for-each (lambda (dir)
                               (copy-file
                                (string-append dir "/README.txt")
                                (string-append doc "/README_" dir ".txt")))
                             (list "ConvertFormat" "ExtractWindow")))
                 (copy-recursively "Examples" examples)
                 #t))))))
      (home-page "https://www.idpoisson.fr/fullswof/")
      (synopsis "Two dimensional flow solver for flood modelling")
      (description "@code{FullSWOF_2d} is a numerical tool for solving
the shallow water equations on structured grids.  The name FullSWOF
refers to the Full form of the Shallow Water equations for Overland
Flow.  The discretized system of equations is solved using the finite
volume method.  A choice of shock-capturing methods is available to
locate the transition boundaries between the wet areas and the dry
areas in the model.  A semi-implicit method is used to advance the
solution in time.  The tool is typically applied to the modelling of
river flooding.")
      (license license:cecill))))

(define-public python-meshio
  (package
    (name "python-meshio")
    (version "5.3.5")
    (source
     (origin
       ;; Using PyPI rather than git because some test files must be
       ;; downloaded through git-fls.
       (method url-fetch)
       (uri (pypi-uri "meshio" version))
       (sha256
        (base32 "0vaqhygr2d186qixvfckzqf4447674ylnc0rl5pa16zjv6mh27zj"))))
    (build-system pyproject-build-system)
    (inputs (list python-h5py python-netcdf4))
    (native-inputs (list python-pytest python-setuptools))
    (propagated-inputs (list python-numpy python-rich))
    (home-page "https://github.com/nschloe/meshio")
    (synopsis "I/O for mesh files")
    (description
     "There are various file formats available for representing unstructured
meshes and mesh data.  The @code{meshio} package is able to read and write
mesh files in many formats and to convert files from one format to another.
Formats such as cgns, h5m, gmsh, xdmf and vtk are supported.  The package
provides command-line tools and a collection of Python modules for
programmatic use.")
    (license license:expat)))

(define-public python-pygmsh
  (package
    (name "python-pygmsh")
    (version "7.1.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nschloe/pygmsh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "11flp2c4ynk1fhanf4mqyzrpd0gjbnv6afrwwc7xi3mb6ms69lr0"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'workaround-gmsh-detection-failure
            (lambda _
              ;; Due to lack of metadata, the gmsh Python package is not
              ;; detected although importable.
              (substitute* "pyproject.toml"
                (("\"gmsh\",") "")))))))
    (native-inputs (list python-flit-core python-pytest))
    (propagated-inputs (list gmsh python-meshio python-numpy))
    (home-page "https://github.com/nschloe/pygmsh")
    (synopsis "Python frontend for Gmsh")
    (description "The goal of @code{pygmsh} is to combine the power of
Gmsh with the versatility of Python.  The package generalises many of
the methods and functions that comprise the Gmsh Python API.  In this
way the meshing of complex geometries using high-level abstractions is
made possible.  The package provides a Python library together with a
command-line utility for mesh optimisation.")
    (license license:gpl3+)))

(define-public python-dolfin-adjoint
  (package
    (name "python-dolfin-adjoint")
    (version "2019.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dolfin-adjoint/pyadjoint")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xhy76a5f33hz94wc9g2mc5qmwkxfccbbc6yxl7psm130afp8lhn"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; One of the migration tests attempts to call openmpi
           ;; recursively and fails.  See
           ;; https://bitbucket.org/mpi4py/mpi4py/issues/95.  Run the
           ;; test sequentially instead.
           (with-directory-excursion "tests/migration/optimal_control_mms"
             (substitute* "test_optimal_control_mms.py"
               (("\\\"mpirun\\\", \\\"-n\\\", \\\"2\\\", ") "")))
           ;; Result files are regenerated in the check phase.
           (delete-file-recursively
            "tests/migration/viscoelasticity/test-results")))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list
         ;; Ignore tests which require missing packages and/or failed during
         ;; tests collection.
         "--import-mode=importlib"
         "--ignore=tests/firedrake_adjoint"
         "--ignore=tests/fenics_adjoint/test_dynamic_meshes.py"
         "-k" (string-append "not test_sin_weak_spatial and "
                             "not test_shape_hessian and "
                             "not test_read_checkpoint and "
                             "not test_lu_solver and "
                             "not test_lu_solver_function_ctrl and "
                             "not test_dirichletbc_subspace"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'build 'mpi-setup #$%openmpi-setup)
          (add-before 'check 'set-environment-variables
            (lambda _
              (setenv "HOME" (getcwd))))
          (add-after 'install 'install-doc
            (lambda _
              (let* ((doc (string-append #$output "/share/doc/" #$name "-" #$version))
                     (examples (string-append doc "/examples")))
                (mkdir-p examples)
                (copy-recursively "examples" examples))))
          ;; Remove 'sanity-check, because it tries to import
          ;; firedrake_adjoint after importing fenics_adjoint.
          ;; Both load a module named 'backend' and firedrake_adjoint
          ;; fails with an ImportError if it sees that the backend module
          ;; has already been loaded.
          (delete 'sanity-check))))
    (inputs
     (list fenics openmpi pybind11))
    (native-inputs
     (list pkg-config
           python-coverage
           python-decorator
           python-flake8
           python-pkgconfig
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-scipy))
    (home-page "https://www.dolfin-adjoint.org")
    (synopsis "Automatic differentiation library")
    (description
     "@code{python-dolfin-adjoint} is a solver of differential equations
associated with a governing system and a functional of interest.  Working from
the forward model the solver automatically derives the discrete adjoint and
tangent linear models.  These additional models are key ingredients in many
algorithms such as data assimilation, optimal control, sensitivity analysis,
design optimisation and error estimation.  The dolfin-adjoint project provides
the necessary tools and data structures for cases where the forward model is
implemented in @code{fenics} or
@url{https://firedrakeproject.org,firedrake}.")
    (license license:lgpl3)))

(define %commonroad-dont-install-license-at-root
  #~(substitute* "setup.py"
      (("data_files=\\[\\('.', \\['LICENSE.txt'\\]\\)\\],")
       "")))

(define-public python-commonroad-vehicle-models
  (package
    (name "python-commonroad-vehicle-models")
    (version "3.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "commonroad-vehicle-models" version))
              (sha256
               (base32
                "13jg0cys7y4n7rg548w6mxk9g10gd5qxmj4ynrlriczpffqy6kc7"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--ignore=scripts/test_vehicle.py" ; import error
              "-k" "not test_std")               ; flaky
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-setup.py
            (lambda _
              #$%commonroad-dont-install-license-at-root)))))
    (native-inputs
     (list python-matplotlib
           python-pytest
           python-scipy
           python-setuptools))
    (propagated-inputs (list python-numpy python-omegaconf))
    (home-page "https://commonroad.in.tum.de/")
    (synopsis "CommonRoad vehicle models")
    (description "This package provides vehicle models used in CommonRoad
benchmarks.  Varying abstraction levels are used ranging from kinematic single
track models to multi-body models.")
    (license license:bsd-3)))

(define-public python-commonroad-io
  (package
    (name "python-commonroad-io")
    (version "2022.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "commonroad-io" version))
              (sha256
               (base32
                "1cj9zj567mca8xb8sx9h3nnl2cccv6vh8h73imgpq61cimk9mvas"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-setup.py
            (lambda _
              (substitute* "setup.py"
                (("protobuf==3.20.1") "protobuf >= 3.20.1"))
              #$%commonroad-dont-install-license-at-root)))))
    (propagated-inputs (list python-commonroad-vehicle-models
                             python-iso3166
                             python-lxml
                             python-matplotlib
                             python-networkx
                             python-numpy
                             python-omegaconf
                             python-pillow
                             python-protobuf
                             python-rtree
                             python-scipy
                             python-shapely
                             python-tqdm))
    (native-inputs (list python-lxml python-setuptools))
    (home-page "https://commonroad.in.tum.de/")
    (synopsis "Read, write, and visualize CommonRoad scenarios")
    (description "This package provides methods to read, write, and visualize
CommonRoad scenarios and planning problems.  It can be used as a framework for
implementing motion planning algorithms to solve CommonRoad Benchmarks
and is the basis for other tools of the CommonRoad Framework.")
    (license license:bsd-3)))

(define-public python-commonroad-route-planner
  (package
    (name "python-commonroad-route-planner")
    (version "2022.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.lrz.de/tum-cps/commonroad-route-planner")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xn0l7bzmj56d4mlqacvbl8mdvsffkg2fn2lzfmis5jl4vp99ipf"))))
    (arguments
     (list
      #:tests? #f                       ; No tests.
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-setup.py
            (lambda _
              #$%commonroad-dont-install-license-at-root)))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools))
    (propagated-inputs (list python-commonroad-io
                             python-matplotlib
                             python-networkx
                             python-numpy
                             python-setuptools
                             python-shapely))
    (home-page "https://gitlab.lrz.de/tum-cps/commonroad-route-planner")
    (synopsis "Route planner for CommonRoad scenarios")
    (description
     "This package provides functions for route planning, that is finding
sequences that lead from a given start lanelet to some goal lanelet(s).")
    (license license:bsd-3)))

(define-public sumo
  (package
    (name "sumo")
    (version "1.14.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/eclipse/sumo")
                    (commit (string-append "v"
                                           (string-replace-substring version
                                                                     "." "_")))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1965vrsff0k14z3y3b1c460zdwp9nx6q6plrdyxn496vg6846k1y"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-problematic-tests
            (lambda _
              (substitute* "CMakeLists.txt"
                ;; docs/example appears to be missing
                (("add_test\\(exampletest .*") ""))
              (substitute* "src/traci_testclient/CMakeLists.txt"
                ;; requires network connection (at least to localhost)
                (("add_test\\(NAME libtracitest .*") "")))))))
    (inputs (list eigen
                  freetype
                  fontconfig
                  ffmpeg
                  fox
                  gdal
                  glu
                  gperftools ; tcmalloc
                  libjpeg-turbo
                  libtiff
                  libx11
                  libxcursor
                  libxft
                  libxi
                  libxrandr
                  libxrender
                  openscenegraph
                  proj
                  python
                  xerces-c
                  zlib))
    (native-inputs (list googletest-1.8 python))
    (home-page "https://eclipse.org/sumo")
    (synopsis "Traffic simulator")
    (description "@acronym{SUMO, Simulation of Urban MObility} is a traffic
simulation package designed to handle large road networks and different modes
of transportation -- including road vehicles, public transport and pedestrians.
Included with SUMO is a wealth of supporting tools which automate core tasks
for the creation, the execution and evaluation of traffic simulations,
such as network import, route calculations, visualization and emission
calculation.  SUMO can be enhanced with custom models and provides various
APIs to remotely control the simulation.")
    (license (list license:epl2.0 license:gpl2+))))
