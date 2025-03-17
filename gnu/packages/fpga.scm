;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2022 Christian Gelinek <cgelinek@radlogic.com.au>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Simon South <simon@simonsouth.net>
;;; Copyright © 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 Jakob Kirsch <jakob.kirsch@web.de>
;;; Copyright © 2025 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu packages fpga)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages version-control))

(define-public abc
 (let ((commit "d5e1a5d445f68bdb4895bb735b9568e5f4738c13")
       (revision "4"))
  (package
    (name "abc")
    (version (git-version "0.0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/berkeley-abc/abc")
                    (commit commit)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0b3qdljcr7dznqr3zxihx9vp6ng6a6pnaqhasblc03rnpp83y1w4"))))
    (build-system gnu-build-system)
    (inputs
     (list readline))
    (arguments
     (list #:license-file-regexp "copyright.txt"
           #:tests? #f ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (replace 'install
                 (lambda _
                   (install-file "abc" (string-append #$output "/bin")))))))
    (home-page "https://people.eecs.berkeley.edu/~alanmi/abc/")
    (synopsis "Sequential logic synthesis and formal verification")
    (description "ABC is a program for sequential logic synthesis and
formal verification.")
    (license
     (license:non-copyleft
      "https://people.eecs.berkeley.edu/~alanmi/abc/copyright.htm")))))

(define-public iverilog
  (package
    (name "iverilog")
    (version "12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/steveicarus/iverilog")
             (commit
              (string-append "v" (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cm3ksxyyp8ihs0as5c2nk3a0y2db8dmrrw0f9an3sl255smxn17"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:bootstrap-scripts #~(list "autoconf.sh")))
    (native-inputs (list autoconf bison flex gperf))
    (inputs (list zlib))
    (home-page "https://steveicarus.github.io/iverilog/")
    (synopsis "FPGA Verilog simulation and synthesis tool")
    (description
     "Icarus Verilog is a Verilog simulation and synthesis tool.
It operates as a compiler, compiling source code written in Verilog
(IEEE-1364) into some target format.
For batch simulation, the compiler can generate an intermediate form
called vvp assembly.
This intermediate form is executed by @command{vvp}.
For synthesis, the compiler generates netlists in the desired format.")
    ;; GPL2 only because of:
    ;; - ./driver/iverilog.man.in
    ;; - ./iverilog-vpi.man.in
    ;; - ./tgt-fpga/iverilog-fpga.man
    ;; - ./vvp/vvp.man.in
    ;; Otherwise would be GPL2+.
    ;; You have to accept both GPL2 and LGPL2.1+.
    (license (list license:gpl2 license:lgpl2.1+))))

(define-public yosys
  (package
    (name "yosys")
    (version "0.51")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/YosysHQ/yosys")
             (commit (string-append "v" version))))
       (sha256
        (base32 "091acyz3bs20shsbavqnd11n0jcx3fqal4rfg6gdf314bx6nrydm"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags
      #~(list (string-append "CC="
                             #$(cc-for-target))
              (string-append "CXX="
                             #$(cxx-for-target))
              (string-append "PREFIX="
                             #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "backends/smt2/smtio.py"
                (("\\['z3")
                 (string-append "['"
                                (search-input-file inputs "bin/z3"))))
              (substitute* "kernel/fstdata.cc"
                (("vcd2fst")
                 (search-input-file inputs "bin/vcd2fst")))
              (substitute* "kernel/driver.cc"
                (("^#include \"libs/cxxopts/include/cxxopts.hpp\"")
                 "#include <cxxopts.hpp>"))
              (substitute* '("passes/cmds/show.cc" "passes/cmds/viz.cc")
                (("exec xdot")
                 (string-append "exec "
                                (search-input-file inputs "bin/xdot")))
                (("dot -")
                 (string-append (search-input-file inputs "bin/dot") " -"))
                (("fuser")
                 (search-input-file inputs "bin/fuser")))))
          (replace 'configure
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "config-gcc" make-flags)))
          (add-after 'configure 'use-external-abc
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("Makefile")
                (("ABCEXTERNAL \\?=")
                 (string-append "ABCEXTERNAL = "
                                (search-input-file inputs "/bin/abc"))))))
          (add-after 'install 'add-symbolic-link
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Previously this package provided a copy of the "abc"
              ;; executable in its output, named "yosys-abc".  Create a
              ;; symbolic link so any external uses of that name continue to
              ;; work.
              (symlink (search-input-file inputs "/bin/abc")
                       (string-append #$output "/bin/yosys-abc"))))
          (add-after 'install 'wrap
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/yosys-witness")
                `("GUIX_PYTHONPATH" ":" prefix
                  (,(getenv "GUIX_PYTHONPATH")))))))))
    (native-inputs (list bison
                         cxxopts ;header-only library
                         flex
                         gawk ;for the tests and "make" progress pretty-printing
                         iverilog ;for the tests
                         pkg-config
                         python
                         tcl)) ;tclsh for the tests
    (inputs (list abc
                  bash-minimal
                  graphviz
                  gtkwave
                  libffi
                  psmisc
                  python
                  python-click
                  readline
                  tcl
                  xdot
                  z3
                  zlib))
    (home-page "https://yosyshq.net/yosys/")
    (synopsis "FPGA Verilog RTL synthesizer")
    (description "Yosys synthesizes Verilog-2005.")
    (license license:isc)))

(define-public yosys-clang
  (package
    (inherit yosys)
    (name "yosys-clang")
    (arguments
     (substitute-keyword-arguments (package-arguments yosys)
       ((#:make-flags _ #f)
        #~(list "CC=clang"
                "CXX=clang++"
                (string-append "PREFIX=" #$output)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (replace 'configure
              (lambda* (#:key make-flags #:allow-other-keys)
                (apply invoke "make" "config-clang" make-flags)))))))
    (inputs
     (modify-inputs (package-inputs yosys)
       (append clang)))
    (synopsis "FPGA Verilog RTL synthesizer (Clang variant)")))

(define-public icestorm
  (let ((commit "2bc541743ada3542c6da36a50e66303b9cbd2059")
        (revision "4"))
   (package
    (name "icestorm")
    (version (git-version "0.0" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cliffordwolf/icestorm")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                 "0d0ibdq3rzmjcbv97h4b9wgq8ikvgwlfw60spi2w81mis317lis8"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no unit tests that don't need an FPGA exist.
       #:make-flags (list "CC=gcc" "CXX=g++"
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'remove-usr-local
            (lambda _
              (substitute* "iceprog/Makefile"
                (("-I/usr/local/include") "")
                (("-L/usr/local/lib") ""))
              #t))
          (add-after 'remove-usr-local 'fix-usr-local
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "icebox/icebox_vlog.py"
                (("/usr/local/share") (string-append (assoc-ref outputs "out") "/share")))
              #t))
          (delete 'configure))))
    (inputs
     (list libftdi))
    (native-inputs
     `(("python-3" ,python)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.clifford.at/icestorm/")
    (synopsis "Project IceStorm - Lattice iCE40 FPGAs bitstream tools")
    (description "Project IceStorm - Lattice iCE40 FPGAs Bitstream Tools.
Includes the actual FTDI connector.")
    (license license:isc))))

(define-public nextpnr-ice40
  (let* ((version "0.7")
         (tag (string-append "nextpnr-" version)))
    (package
      (name "nextpnr-ice40")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/nextpnr")
               (commit tag)
               (recursive? #t)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0sbhqscgmlk4q2207rsqsw99qx4fyrxx1hsd669lrk42gmk3s9lm"))
         (modules '((guix build utils)))
         (snippet
          #~(begin
              ;; Remove bundled source code for which Guix has packages.
              ;; Note the bundled copies of json11 and python-console contain
              ;; modifications, while QtPropertyBrowser appears to be
              ;; abandoned and without an official source.
              ;; fpga-interchange-schema is used only by the
              ;; "fpga_interchange" architecture target, which this package
              ;; doesn't build.
              (with-directory-excursion "3rdparty"
                (for-each delete-file-recursively
                          '("googletest" "imgui" "pybind11" "qtimgui"
                            "sanitizers-cmake")))

              ;; Remove references to unbundled code and link against external
              ;; libraries instead.
              (substitute* "CMakeLists.txt"
                (("^\\s+add_subdirectory\\(3rdparty/googletest.*") "")
                (("^(\\s+target_link_libraries.*)( gtest_main\\))"
                  _ prefix suffix)
                 (string-append prefix " gtest" suffix)))
              (substitute* "gui/CMakeLists.txt"
                (("^\\s+../3rdparty/(qt)?imgui.*") "")
                (("^(target_link_libraries.*)\\)" _ prefix)
                 (string-append prefix " imgui qt_imgui_widgets)")))))))
      (native-inputs
       (list googletest sanitizers-cmake))
      (inputs
       (list boost
             eigen
             icestorm
             imgui-1.86
             pybind11
             python
             qtbase-5
             qtwayland-5
             qtimgui
             yosys))
      (build-system qt-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DARCH=ice40"
                "-DBUILD_GUI=ON"
                "-DBUILD_TESTS=ON"
                (string-append "-DCURRENT_GIT_VERSION=" #$tag)
                (string-append "-DICESTORM_INSTALL_PREFIX="
                               #$(this-package-input "icestorm"))
                "-DUSE_IPO=OFF")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'patch-source
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "CMakeLists.txt"
                  ;; Use the system sanitizers-cmake module.
                  (("\\$\\{CMAKE_SOURCE_DIR\\}/3rdparty/sanitizers-cmake/cmake")
                   (string-append
                    #$(this-package-native-input "sanitizers-cmake")
                    "/share/sanitizers-cmake/cmake")))
                (substitute* "gui/CMakeLists.txt"
                  ;; Compile with system imgui and qtimgui headers.
                  (("^(target_include_directories.*)../3rdparty/imgui(.*)$"
                    _ prefix suffix)
                   (string-append prefix
                                  (search-input-directory inputs
                                                          "include/imgui")
                                  suffix))
                  (("^(target_include_directories.*)../3rdparty/qtimgui/(.*)$"
                    _ prefix suffix)
                   (string-append prefix
                                  (search-input-directory inputs
                                                          "include/qtimgui")
                                  suffix))))))))
      (synopsis "Place-and-Route tool for FPGAs")
      (description "Nextpnr aims to be a vendor neutral, timing driven, FOSS
FPGA place and route tool.")
      (home-page "https://github.com/YosysHQ/nextpnr")
      (license license:expat))))

(define-public gtkwave
  (package
    (name "gtkwave")
    (version "3.3.121")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/gtkwave/"
                                 "gtkwave-gtk3-" version "/"
                                 "gtkwave-gtk3-" version ".tar.gz")
                  (string-append "https://gtkwave.sourceforge.net/"
                                 "gtkwave-" version ".tar.gz")))
       (sha256
        (base32 "0ikk49zyar5aiq7pg9whi4nfzq7xm8sz7bn3b6vaylkdimw4bajl"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list gperf pkg-config))
    (inputs
     (list gtk+ tcl tk))
    (arguments
     (list #:configure-flags
           #~(list "--enable-gtk3"
                   (string-append "--with-tcl="
                    (assoc-ref %build-inputs "tcl")
                    "/lib")
                   (string-append "--with-tk="
                                  (assoc-ref %build-inputs "tk")
                                  "/lib"))))
    (synopsis "Waveform viewer for FPGA simulator trace files")
    (description "This package is a waveform viewer for FPGA
simulator trace files (@dfn{FST}).")
    (home-page "https://gtkwave.sourceforge.net/")
    ;; Exception against free government use in tcl_np.c and tcl_np.h.
    (license (list license:gpl2+ license:expat license:tcl/tk))))

(define-public python-migen
  (package
    (name "python-migen")
    (version "0.9.2")
    (source
     (origin
       ;; Tests fail in the PyPI tarball due to missing files.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/m-labs/migen")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kq11if64zj84gv4w1q7l16fp17xjxl2wv5hc9dibr1z3m1gy67l"))))
    (build-system python-build-system)
    (propagated-inputs
     (list python-colorama))
    (home-page "https://m-labs.hk/gateware/migen/")
    (synopsis "Python toolbox for building complex digital hardware")
    (description
     "Migen FHDL is a Python library that replaces the event-driven
paradigm of Verilog and VHDL with the notions of combinatorial and
synchronous statements, has arithmetic rules that make integers always
behave like mathematical integers, and allows the design's logic to be
constructed by a Python program.")
    (license license:bsd-2)))

(define-public python-myhdl
  (package
    (name "python-myhdl")
    (version "0.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "myhdl" version))
        (sha256
          (base32
            "04fi59cyn5dsci0ai7djg74ybkqfcjzhj1jfmac2xanbcrw9j3yk"))))
    (build-system python-build-system)
    (home-page "https://www.myhdl.org/")
    (synopsis "Python as a Hardware Description Language")
    (description "This package provides a library to turn Python into
a hardware description and verification language.")
    (license license:lgpl2.1+)))

(define-public python-vunit
  (package
    (name "python-vunit")
    (version "4.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VUnit/vunit")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s7j5bykbv34wgnxy5cl4zp6g0caidvzs8pd9yxjq341543xkjwm"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f))                ;XXX: requires setuptools_scm >= 2.0.0, <3
    (propagated-inputs (list python python-colorama))
    (home-page "https://vunit.github.io")
    (synopsis "Unit testing framework for VHDL/SystemVerilog")
    (description
     "VUnit features the functionality needed to realize continuous and
automated testing of HDL code.")

    ;; According to 'LICENSE.rst', VUnit itself is under MPL but two
    ;; subdirectories are under ASL.
    (license (list license:mpl2.0 license:asl2.0))))

(define-public nvc
  (package
    (name "nvc")
    (version "1.15.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nickg/nvc")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hqkgwkvflha1fpch13byb8clwa97n6z1d9a2d34cqzsjrzkdx0k"))))
    (build-system gnu-build-system)
    (arguments
     (list #:out-of-source? #t
           #:configure-flags #~(list "--enable-vhpi")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'clean-up
                          (lambda _
                            (delete-file "autogen.sh"))))))
    (native-inputs
     (list automake
           autoconf
           flex
           gettext-minimal
           libtool
           pkg-config
           which
           check)) ; for the tests
    (inputs
     (list elfutils
           llvm-9
           libffi
           `(,zstd "lib")))
    (synopsis "VHDL compiler and simulator")
    (description "This package provides a VHDL compiler and simulator.")
    (home-page "https://www.nickg.me.uk/nvc/")
    (license license:gpl3+)))

(define-public systemc
  (package
    (name "systemc")
    (version "3.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/accellera-official/systemc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v5fg3h9ffdzq9f6zplvr9all00ssc1gpdvbg129xahkrbl53kvw"))))
    (native-inputs (list perl))
    (build-system cmake-build-system)
    (arguments '(#:test-target "check"))
    (home-page "https://accellera.org/community/systemc")
    (synopsis "Library for event-driven simulation")
    (description
     "SystemC is a C++ library for modeling concurrent systems, and the
reference implementation of IEEE 1666-2011.  It provides a notion of timing as
well as an event-driven simulations environment.  Due to its concurrent and
sequential nature, SystemC allows the description and integration of complex
hardware and software components.  To some extent, SystemC can be seen as
a Hardware Description Language.  However, unlike VHDL or Verilog, SystemC
provides sophisticated mechanisms that offer high abstraction levels on
components interfaces.  This, in turn, facilitates the integration of systems
using different abstraction levels.")
    ;; homepages.cae.wisc.edu/~ece734/SystemC/Esperan_SystemC_tutorial.pdf
    (license license:asl2.0)))

(define-public verilator
  (package
    (name "verilator")
    (version "5.028")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/verilator/verilator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q9facgfdwwmf2ax65aznhqmk8qfisq9k5p8wrxrw6qqy38vl0k2"))))
    (native-inputs
     (list autoconf
           automake
           bison
           flex
           gettext-minimal
           python
           ;; And a couple of extras for the test suite:
           cmake-minimal
           gdb/pinned
           which))
    (inputs
     (list help2man perl python systemc))
    (build-system gnu-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "autoconf")))
         (add-after 'unpack 'adjust-source
           (lambda _
             (substitute* "bin/verilator"
               (("/bin/echo") "echo"))))
         (add-before 'check 'disable-gdb-safe-path
           (lambda _
             (setenv "HOME" (getcwd))
             (mkdir-p (string-append (getcwd) "/.config/gdb"))
             (with-output-to-file (string-append (getcwd) "/.config/gdb/gdbinit")
               (lambda ()
                 (display "set auto-load safe-path /"))))))
       #:test-target "test"))
    ;; #error "Something failed during ./configure as config_build.h is incomplete.
    ;; Perhaps you used autoreconf, don't." -- so we won't. ^^
    (home-page "https://www.veripool.org/projects/verilator/")
    (synopsis "Fast Verilog/SystemVerilog simulator")
    (description
     "Verilator is invoked with parameters similar to GCC or Synopsys’s VCS.
It ``Verilates'' the specified Verilog or SystemVerilog code by reading it,
performing lint checks, and optionally inserting assertion checks and
coverage-analysis points.  It outputs single- or multi-threaded @file{.cpp}
and @file{.h} files, the ``Verilated'' code.

The user writes a little C++/SystemC wrapper file, which instantiates the
Verilated model of the user’s top level module.  These C++/SystemC files are
then compiled by a C++ compiler (GCC/Clang/etc.).  The resulting executable
performs the design simulation.  Verilator also supports linking its generated
libraries, optionally encrypted, into other simulators.")
    (license license:lgpl3)))

(define-public fftgen
  (let ((commit "1d75a992efd0528edea128a903aafdabe133cb08") ;no releases
        (revision "0"))
    (package
      (name "fftgen")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ZipCPU/dblclockfft")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0qq874yalzpjdwnxhc5df8a0ifywv29wcncb09945x56xplvkcmd"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                              ;no tests
         #:make-flags '("CFLAGS=-g -O2")          ;default flags lack -O2
         #:phases (modify-phases %standard-phases
                    (delete 'configure)
                    (replace 'install
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let ((bin (string-append (assoc-ref outputs "out")
                                                  "/bin")))
                          (install-file "sw/fftgen" bin)))))))
      (synopsis "Generic pipelined FFT core generator")
      (description "fftgen produces @acronym{FFT, fast-Fourier transforms}
hardware designs in Verilog.")
      (home-page "https://zipcpu.com/")
      (license license:lgpl3+))))

(define-public openfpgaloader
  (package
    (name "openfpgaloader")
    (version "0.13.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/trabucayre/openfpgaloader")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p5qvr0bq27rp7f20ysjml7zy4bbwjx3s4yd5qjsg4b01mw4hbiq"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs (list eudev
                  hidapi
                  libftdi
                  libgpiod
                  libusb
                  zlib))
    (arguments
     `(#:tests? #f)) ; No tests exist
    (synopsis "Utility for programming FPGA")
    (description "This package provides a program to transfer a bitstream
to an FPGA.")
    (home-page "https://trabucayre.github.io/openFPGALoader")
    (license license:asl2.0)))

(define-public python-hdlmake
  (let ((commit "3cb248fdad601c579b59fd7c194402871209bc54")
        (revision "0"))
    (package
      (name "python-hdlmake")
      (version (git-version "3.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://ohwr.org/project/hdl-make")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "08ivnhxyp44agmifqb4pjbxj23p43qqcg73s2y2z1hqk2six3fdx"))))
      (build-system pyproject-build-system)
      (arguments
       `(#:tests? #f))
      (native-inputs (list python-setuptools python-wheel))
      (propagated-inputs (list python-six))
      (home-page "https://ohwr.org/projects/hdl-make")
      (synopsis "Generate multi-purpose makefiles for HDL projects")
      (description
       "Hdlmake helps manage and share @acronym{HDL, hardware description
language} code by automatically finding file dependencies, writing synthesis
and simulation Makefiles.")
      (license license:gpl3+))))
