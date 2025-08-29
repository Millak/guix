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
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system meson)
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
  #:use-module (gnu packages electronics)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
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
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web))

(define-public abc
  (let ((commit "e29dcd9f3275874c8d31a2f781487efac1dabb7b")
        (revision "6"))
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
                  "18g4i1kdsxvp25p5z3wja4jkxppgrp6ybxal9y2p2d2qvlafiw5z"))))
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

(define-public abc-yosyshq
  (package
    (inherit abc)
    (name "abc-yosyshq")
    (version "0.56")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/YosysHQ/abc/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0wy42qd0dl58icw3nklgns5zrr1inj8br40vwcpwiz1pkfg3gl0j"))))
    (home-page "https://github.com/YosysHQ/abc/")
    (description "ABC is a program for sequential logic synthesis and
formal verification.  This is the Yosyshq fork of ABC.")
    (license (license:non-copyleft "file:///copyright.txt"))))

(define-public apycula
  (package
    (name "apycula")
    (version "0.23")
    ;; The pypi tar.gz file includes the necessary .pickle files, not available
    ;; in the home-page repository.
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "apycula" version))
       (sha256
        (base32 "1kk9hi8zhdp1am5vj716lwlmrs31lxrwhdbbc4qsad470dcjqs57"))))
    (build-system pyproject-build-system)
    (arguments (list #:tests? #f))      ;requires Gowin EDA tools
    (inputs (list python-crc))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "https://github.com/YosysHQ/apicula/")
    (synopsis "Gowin FPGA bitstream format")
    (description
     "The project Apycula provides tools to support development and
generating bitstreams with Gowin FPGAs.")
    (license license:expat)))

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
    (version "0.56")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/YosysHQ/yosys")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1q74hm1z0m08r9amz982a9ylcwz2mbg3hqarprwj775wkrbv81h7"))
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
          (add-after 'install 'keep-pmgen-py
            (lambda* (#:key inputs #:allow-other-keys)
              ;; pmgen.py is required by some yosys plugins.
              (install-file (search-input-file inputs
                                               "/passes/pmgen/pmgen.py")
                            (string-append #$output "/bin"))))
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
    (inputs (list abc-yosyshq
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
  (let ((commit "3cdcf4b009bb8681ab7e2e09d65043f04334b60e")
        (revision "5"))
    (package
      (name "icestorm")
      (version (git-version "0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/icestorm/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0ygp6cj7grlnyji572kx215p2mw4crllskif9g795f390bp38g68"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:tests? #f               ;avoid a cyclic dependency with nextpr-ice40
        #:make-flags
        #~(list (string-append "CC="
                               #$(cc-for-target))
                (string-append "CXX="
                               #$(cxx-for-target))
                (string-append "PREFIX="
                               #$output)
                "ICEPROG=1")
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'fix-usr-local
              (lambda* (#:key outputs #:allow-other-keys)
                (substitute* "icepack/Makefile"
                  (("/usr/local")
                   #$output))
                (substitute* "icebox/Makefile"
                  (("/usr/local")
                   #$output))
                (substitute* "icebox/icebox_vlog.py"
                  (("/usr/local")
                   #$output))))
            (add-after 'build 'make-info
              (lambda* (#:key outputs #:allow-other-keys)
                (with-directory-excursion "docs"
                  (invoke "make" "info")
                  (install-file "build/texinfo/projecticestorm.info"
                                (string-append #$output "/share/info"))
                  (copy-recursively "build/texinfo/projecticestorm-figures"
                                    (string-append #$output
                                     "/share/info/projecticestorm-figures")))))
            (delete 'configure))))
      (inputs (list libftdi))
      (native-inputs (list pkg-config
                           python
                           python-sphinx
                           python-sphinx-rtd-theme
                           texinfo))
      (home-page "https://prjicestorm.readthedocs.io/")
      (synopsis "Bitstream tools for Lattice iCE40 FPGAs")
      (description
       "Project IceStorm aims at documenting the bitstream format of
Lattice iCE40 FPGAs and providing simple tools for analyzing and creating bitstream
files.")
      (license license:isc))))

(define-public libfst
  ;; There are no release nor tags.
  (let ((commit "6a52070cd62ec65c29832bc95e7db493504aa7ac")
        (revision "0"))
    (package
      (name "libfst")
      (version (git-version "1.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gtkwave/libfst/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0b1r660m5aib316jjl4nhs10y7vhhqy2mvxjip3ynahig3hpi46z"))))
      (build-system meson-build-system)
      (native-inputs (list gobject-introspection pkg-config))
      (inputs (list bzip2))
      (propagated-inputs (list zlib))  ;in Requires.private of libfst.pc
      (synopsis "Fast Signal Trace (FST) format waveforms library")
      (description "Libfst is a small library used to read and write
@acronym{FST, Fast Signal Trace} format waveforms.")
      (home-page "https://github.com/gtkwave/libfst/")
      (license (list license:expat      ;libfst and fastlz-derived sources
                     license:bsd-2))))) ;for lz4-derived sources

(define-public nextpnr
  ;; Necessary for compatibility with latest apycula.
  ;; TODO: Remove with release 0.9.
  (let ((commit "d796cc720b60ccc18580c686d93c8751fe461532")
        (revision "0"))
    (package
      (name "nextpnr")
      (version (git-version "0.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/YosysHQ/nextpnr/")
                (commit commit)
                ;; XXX: Fetch some bundled libraries such as QtPropertyBrowser,
                ;; json11 and python-console, which have custom modifications or
                ;; no longer have their original upstream.
                (recursive? #t)))
         (file-name (git-file-name name version))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-26)))
         (snippet
          '(begin
             ;; XXX: 'delete-all-but' is copied from the turbovnc package.
             (define (delete-all-but directory . preserve)
                (with-directory-excursion directory
                  (let* ((pred (negate (cut member <>
                                            (cons* "." ".." preserve))))
                         (items (scandir "." pred)))
                    (for-each (cut delete-file-recursively <>) items))))
             (delete-all-but "3rdparty"
                             ;; The following sources have all been patched, so
                             ;; cannot easily be unbundled.
                             "QtPropertyBrowser"
                             "json11"
                             "python-console"
                             "oourafft")))
         (patches (search-patches "nextpnr-gtest.patch"
                                  "nextpnr-imgui.patch"))
         (sha256
          (base32 "1arj25vad76wg6b5yaaky4cby5zp9v92pdd4y3l0kxi7wvxhmmya"))))
      (build-system qt-build-system)
      (arguments
       (list
        #:cmake cmake                     ;CMake 3.25 or higher is required.
        #:configure-flags
        ;; TODO: enable more architectures?
        #~(list "-DARCH=generic;ice40;ecp5;himbaechel"
                "-DBUILD_GUI=ON"
                "-DUSE_OPENMP=ON"
                "-DBUILD_TESTS=ON"
                "-DHIMBAECHEL_UARCH=ng-ultra;gowin"
                "-DHIMBAECHEL_NGULTRA_DEVICES=ng-ultra"
                "-DHIMBAECHEL_SPLIT=ON"
                "-DHIMBAECHEL_PRJBEYOND_DB=/tmp/prjbeyond-db"
                (string-append "-DCURRENT_GIT_VERSION=nextpnr-" #$version)
                (string-append "-DICESTORM_INSTALL_PREFIX="
                               #$(this-package-input "icestorm"))
                (string-append "-DTRELLIS_INSTALL_PREFIX="
                               #$(this-package-input "prjtrellis"))
                "-DUSE_IPO=OFF")
        #:phases
        #~(modify-phases %standard-phases
            ;; Required by himbaechel architecture, ng-ultra support.
            (add-after 'unpack 'get-prjbeyond-db
              (lambda _
                (copy-recursively
                 #$(origin
                     (method git-fetch)
                     (uri (git-reference
                            (url "https://github.com/yosyshq-GmbH/prjbeyond-db/")
                            ;; We take latest commit, as indicated in nextpnr’s
                            ;; README.md file
                            (commit "06d3b424dd0e52d678087c891c022544238fb9e3")))
                     (sha256
                      (base32
                       "17dd3cgms2fy6xvz7magdmvv92km4cqh2kz9dyjrvz5y8caqav4y")))
                 "/tmp/prjbeyond-db")))
            (add-after 'unpack 'unbundle-sanitizers-cmake
              (lambda _
                (substitute* "CMakeLists.txt"
                  ;; Use the system sanitizers-cmake module.  This is made
                  ;; necessary 'sanitizers-cmake' installing a FindPackage
                  ;; module but no CMake config file.
                  (("\\$\\{CMAKE_SOURCE_DIR}/3rdparty/sanitizers-cmake/cmake")
                   (string-append
                    #$(this-package-native-input "sanitizers-cmake")
                    "/share/sanitizers-cmake/cmake"))))))))
      (native-inputs
       (list googletest
             sanitizers-cmake))
      (inputs
       (list apycula
             boost
             corrosion
             eigen
             icestorm
             prjtrellis
             pybind11
             python
             qtbase-5
             qtwayland-5
             qtimgui
             yosys))
      (synopsis "Place-and-Route tool for FPGAs")
      (description "Nextpnr is a portable FPGA place and route tool.")
      (home-page "https://github.com/YosysHQ/nextpnr/")
      (license license:isc))))

(define-public nextpnr-ice40
  (deprecated-package "nextpnr-ice40" nextpnr))

(define-public gtkwave
  ;; The last release is more than 2 years old, and there are improvements in
  ;; the master branch, such as GTK 4 support: pick the latest commit that
  ;; passes their CI.
  (let ((commit "bb978d9d667d569b9153ffa34007e300302907dc")
        (revision "0"))
    (package
      (name "gtkwave")
      ;; The version string can be found in meson.build.
      (version (git-version "3.4.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gtkwave/gtkwave")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1nv27cpz5937cb6bkhpw8w0ji6hm9xr8f0znvfwzfl1fwwypf23y"))))
      (build-system meson-build-system)
      (arguments (list #:glib-or-gtk? #t))
      (native-inputs (list desktop-file-utils
                           flex
                           `(,glib "bin") ;for glib-mkenums
                           gobject-introspection
                           gperf
                           `(,gtk "bin")
                           pkg-config))
      (inputs (list gtk gtk+ libfst))
      (synopsis "Waveform viewer for FPGA simulator trace files")
      (description "This package is a waveform viewer for @acronym{FST, FPGA
Simulator Trace} files.")
      (home-page "https://github.com/gtkwave/gtkwave")
      ;; Exception against free government use in tcl_np.c and tcl_np.h.
      (license (list license:gpl2+ license:expat license:tcl/tk)))))

(define-public python-migen
  ;; XXX: The latest version tag (0.9.2) was placed in 2019, there are latest
  ;; changes supporting Python 3.11 on master branch, see
  ;; <https://github.com/m-labs/migen/issues/259>.
  (let ((commit "6e3a9e150fb006dabc4b55043d3af18dbfecd7e8")
        (revision "1"))
    (package
      (name "python-migen")
      (version (git-version "0.9.2" revision commit))
      (source
       (origin
         ;; Tests fail in the PyPI tarball due to missing files.
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/m-labs/migen")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1hibhjnj5b3ka5y4nnqx9zp5w730gfqfa3r5injpa14i4cz6bj1n"))))
      (build-system pyproject-build-system)
      (native-inputs
       (list python-pytest
             python-setuptools-next))
      (propagated-inputs
       (list python-colorama))
      (home-page "https://m-labs.hk/gateware/migen/")
      (synopsis "Python toolbox for building complex digital hardware")
      (description
       "Migen FHDL is a Python library that replaces the event-driven paradigm
of Verilog and VHDL with the notions of combinatorial and synchronous
statements, has arithmetic rules that make integers always behave like
mathematical integers, and allows the design's logic to be constructed by a
Python program.")
      (license license:bsd-2))))

(define-public python-myhdl
  (package
    (name "python-myhdl")
    (version "0.11.51")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "myhdl" version))
        (sha256
          (base32
            "0b360smk2m60vhxdi837hz75m0pnms477wkn9gh6m4v3nih1v4cx"))))
    (build-system python-build-system)
    (home-page "http://www.myhdl.org/")
    (synopsis "Python as a Hardware Description Language")
    (description "This package provides a library to turn Python into
a hardware description and verification language.")
    (license license:lgpl2.1+)))

(define-public python-vunit
  (package
    (name "python-vunit")
    (version "5.0.0-dev.6") ;v4.7.0 dates back from 2 years ago.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VUnit/vunit")
             (commit (string-append "v" version))
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zm7733g7ivcx6y00bigvqzkxa2i46sw4pb5k1n3lfbqvsjymshh"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-ghdl-jit
            (lambda _
              ;; TODO: Remove when fixed upstream (see:
              ;; https://github.com/VUnit/vunit/pull/1121).
              (substitute* "vunit/sim_if/ghdl.py"
                ((": \"llvm\",")
                 (string-append
                  ": \"llvm\",\n\tr\"static elaboration, LLVM JIT code "
                  "generator\": \"llvm-jit\","))))))
      #:test-flags
      ;; Skip lint tests which require python-pycodestyle, python-pylint and
      ;; python-mypy to reduce closoure size; some lint test fails, see
      ;; <https://github.com/VUnit/vunit/issues/1111>.
      ;;
      ;; XXX: Acceptance tests take 10+ minutes to complete, hang on
      ;; "test_external_run_scripts.py" and fail eventually, consider to
      ;; improve them; ignore for now.
      #~(list "tests/unit")))
    (native-inputs
     (list nvc
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-colorama))
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
    (version "1.17.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nickg/nvc")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hr5y9ys5kf096x18mh10wwqa0hbzlmdj7pyayc6szsjla1d3mk0"))))
    (build-system gnu-build-system)
    (arguments
     (list #:out-of-source? #t
           #:configure-flags
           #~(list "--enable-tcl"
                   "--enable-llvm"
                   "--enable-verilog"
                   "--enable-vital"
                   "--enable-server"
                   "--with-ncurses"
                   "--enable-parallel-make"
                   "--enable-vital"
                   (string-append "--with-bash-completion=" #$output
                                  "/share/bash-completion/completions"))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'clean-up
                          (lambda _
                            (delete-file "autogen.sh"))))))
    (native-inputs
     (list automake
           autoconf
           check ; for the tests
           flex
           gettext-minimal
           libtool
           pkg-config
           python-minimal
           which))
    (inputs
     (list jansson
           libffi
           llvm
           readline
           tcl
           `(,zstd "lib")))
    (synopsis "VHDL compiler and simulator")
    (description "This package provides a VHDL compiler and simulator.")
    (home-page "https://www.nickg.me.uk/nvc/")
    (license license:gpl3+)))

(define-public systemc
  (package
    (name "systemc")
    (version "3.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/accellera-official/systemc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c8brlv3702p2ivifai9929bg20y30jb301ap0gdmz305q8mcb33"))))
    (native-inputs (list perl))
    (build-system cmake-build-system)
    (arguments
     (list
      #:modules '((guix build cmake-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check (assoc-ref gnu:%standard-phases 'check)))))
    (home-page "https://systemc.org/")
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
    (version "5.034")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/verilator/verilator/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14alpa2z4fqbbsyx67dz50nqcvfis8pha84545h28xmglrzm13yn"))))
    (native-inputs
     (list autoconf
           automake
           bison
           flex
           help2man
           gettext-minimal
           python
           ;; And a couple of extras for the test suite:
           cmake-minimal
           gdb/pinned
           which))
    (inputs
     (list perl python systemc))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
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
              (with-output-to-file
                  (string-append (getcwd) "/.config/gdb/gdbinit")
                (lambda ()
                  (display "set auto-load safe-path /"))))))
      #:test-target "test"))
    (home-page "https://www.veripool.org/verilator/")
    (synopsis "Verilog/SystemVerilog simulator")
    (description
     "Verilator transforms the specified Verilog or SystemVerilog code by
reading it, performing lint checks, and optionally inserting assertion checks
and coverage-analysis points.  It outputs single- or multi-threaded
@file{.cpp} and @file{.h} files.")
    (license license:lgpl3)))

(define-public fftgen
  (let ((commit "3378b77d83a98b06184656a5cb9b54e50dfe4485") ;no releases
        (revision "1"))
    (package
      (name "fftgen")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/ZipCPU/dblclockfft")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1rvln871wjkbbqnv88jnx328xlhn5sgbr8fglk3ajnd9rwgiq3jg"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:test-target "bench-test"
        #:make-flags #~(list "CFLAGS=-g -O2") ;default flags lack -O2
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (replace 'install
              (lambda _
                (install-file "sw/fftgen"
                              (string-append #$output "/bin")))))))
      (native-inputs (list bc fftw python-minimal verilator which))
      (synopsis "Generic pipelined FFT core generator")
      (description "fftgen produces @acronym{FFT, fast-Fourier transforms}
hardware designs in Verilog.")
      (home-page "https://github.com/ZipCPU/zipcpu/")
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
     (list #:tests? #f                  ;no test suite
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-rules
                 (lambda _
                   (install-file
                    "../source/99-openfpgaloader.rules"
                    (string-append #$output "/lib/udev/rules.d/")))))))
    (synopsis "Utility for programming FPGA")
    (description "This package provides a program to transfer a bitstream
to an FPGA.  To use @code{openfpgaloader} without root privileges it is
necessary to install the necessary udev rules.  This can be done by extending
@code{udev-service-type} in the @code{operating-system} configuration file with
this package, as in:
@lisp
(udev-rules-service 'openfpgaloader openfpgaloader #:groups '(\"plugdev\")
@end lisp
Additionally, the @samp{plugdev} group should be registered in the
@code{supplementary-groups} field of your @code{user-account} declaration. Refer
to @samp{info \"(guix) Base Services\"} for examples.")
    (home-page "https://trabucayre.github.io/openFPGALoader/")
    (license license:asl2.0)))

(define-public python-hdlmake
  (let ((commit "48260fb0d7ace3ff2ee124121a5780a226513077")
        (revision "2"))
    (package
      (name "python-hdlmake")
      (version (git-version "3.3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/ohwr/project/hdl-make/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1s36gc5g3v20x2v26d45hfw2x9r7k54lj7sggz94qi7ydbi1ng41"))))
      (build-system pyproject-build-system)
      (arguments (list #:phases #~(modify-phases %standard-phases
                                    (add-before 'check 'chdir
                                      (lambda _
                                        (chdir "testsuite"))))
                       #:test-flags #~(list "test_all.py")))
      (native-inputs (list python-pytest python-setuptools python-wheel))
      (propagated-inputs (list python-networkx python-six))
      (home-page "https://ohwr.gitlab.io/project/hdl-make/")
      (synopsis "Generate multi-purpose makefiles for HDL projects")
      (description
       "Hdlmake helps manage and share @acronym{HDL, hardware description
language} code by automatically finding file dependencies, writing synthesis
and simulation Makefiles.")
      (license license:gpl3+))))
