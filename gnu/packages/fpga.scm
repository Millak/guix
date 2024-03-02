;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2016, 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2022 Christian Gelinek <cgelinek@radlogic.com.au>
;;; Copyright © 2022 jgart <jgart@dismail.de>
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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages libftdi)
  #:use-module (gnu packages libusb))

(define-public abc
 (let ((commit "70cb339f869e")
       (revision "2"))
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
                "1ngxg4jvz8vwm74sbidysgz3v5lrzjcabkqj4nhcksi6hnhyc9m8"))))
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
    (version "11.0")
    (source (origin
              (method url-fetch)
              (uri
               (string-append "ftp://ftp.icarus.com/pub/eda/verilog/v11/"
                              "verilog-" version ".tar.gz"))
              (sha256
               (base32
                "1mamlrkpb2gb00g7xdddaknrvwi4jr4ng6cfjhwngzk3ddhqaiym"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))))
    (native-inputs
     (list flex bison ghostscript zlib))   ; ps2pdf
    (home-page "http://iverilog.icarus.com/")
    (synopsis "FPGA Verilog simulation and synthesis tool")
    (description "Icarus Verilog is a Verilog simulation and synthesis tool.
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
    (version "0.26")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/YosysHQ/yosys")
                    (commit (string-append "yosys-" version))))
              (sha256
               (base32
                "0s79ljgbcfkm7l9km7dcvlz4mnx38nbyxppscvh5il5lw07n45gx"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:make-flags #~(list "CC=gcc"
                           "CXX=g++"
                           (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "./backends/smt2/smtio.py"
                (("\\['z3")
                 (string-append "['" (search-input-file inputs "/bin/z3"))))
              (substitute* "./kernel/fstdata.cc"
                (("vcd2fst")
                 (search-input-file inputs "/bin/vcd2fst")))
              (substitute* '("./passes/cmds/show.cc"
                             "./passes/cmds/viz.cc")
                (("exec xdot")
                 (string-append "exec " (search-input-file inputs
                                                           "/bin/xdot")))
                (("dot -")
                 (string-append (search-input-file inputs "/bin/dot") " -"))
                (("fuser")
                 (search-input-file inputs "/bin/fuser")))))
          (replace 'configure
            (lambda* (#:key make-flags #:allow-other-keys)
              (apply invoke "make" "config-gcc" make-flags)))
          (add-after 'configure 'use-external-abc
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* '("./Makefile")
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
                `("GUIX_PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))))))))
    (native-inputs
     (list bison
           flex
           gawk ; for the tests and "make" progress pretty-printing
           iverilog ; for the tests
           pkg-config
           python
           tcl)) ; tclsh for the tests
    (inputs
     (list abc
           graphviz
           gtkwave
           libffi
           psmisc
           readline
           tcl
           xdot
           z3
           zlib
           python
           python-click))
    (home-page "https://yosyshq.net/yosys/")
    (synopsis "FPGA Verilog RTL synthesizer")
    (description "Yosys synthesizes Verilog-2005.")
    (license license:isc)))

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
  (let [(commit "fbe486df459909065d6852a7495a212dfd2accef")
        (revision "1")]
    (package
      (name "nextpnr-ice40")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "git://github.com/YosysHQ/nextpnr")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1fmxsywgs45g88ra7ips5s2niiiwrkyxdcy742ws18dfk2y4vi9c"))))
      (inputs
       (list boost
             eigen
             icestorm
             python
             qtbase-5
             yosys))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags `("-DARCH=ice40"
                             ,(string-append "-DICEBOX_ROOT="
                                             (assoc-ref %build-inputs "icestorm")
                                             "/share/icebox"))
         #:tests? #f))
      (synopsis "Place-and-Route tool for FPGAs")
      (description "Nextpnr aims to be a vendor neutral, timing driven,
FOSS FPGA place and route tool.")
      (home-page "https://github.com/YosysHQ/nextpnr")
      (license license:expat))))

(define-public arachne-pnr
  (let ((commit "840bdfdeb38809f9f6af4d89dd7b22959b176fdd")
        (revision "2"))
   (package
    (name "arachne-pnr")
    (version (string-append "0.0-" revision "-" (string-take commit 9)))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/YosysHQ/arachne-pnr")
                     (commit commit)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                   "1dqvjvgvsridybishv4pnigw9gypxh7r7nrqp9z9qq92v7c5rxzl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out"))
             (string-append "ICEBOX=" (string-append
                                       (assoc-ref %build-inputs "icestorm")
                                       "/share/icebox")))
       #:phases (modify-phases %standard-phases
       (replace 'configure
         (lambda* (#:key outputs inputs #:allow-other-keys)
           (substitute* '("./tests/fsm/generate.py"
                          "./tests/combinatorial/generate.py")
             (("#!/usr/bin/python") "#!/usr/bin/python2"))
           #t)))))
    (inputs
     (list icestorm))
    (native-inputs
     `(("git" ,git)  ; for determining its own version string
       ("yosys" ,yosys) ; for tests
       ("perl" ,perl) ; for shasum
       ("python-2" ,python-2))) ; for tests
    (home-page "https://github.com/YosysHQ/arachne-pnr")
    (synopsis "Place-and-Route tool for FPGAs")
    (description "Arachne-PNR is a Place-and-Route Tool For FPGAs.")
    (license license:gpl2))))

(define-public gtkwave
  (package
    (name "gtkwave")
    (version "3.3.113")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/gtkwave/"
                                 "gtkwave-" version "/"
                                 "gtkwave-" version ".tar.gz")
                  (string-append "https://gtkwave.sourceforge.net/"
                                 "gtkwave-" version ".tar.gz")))
       (sha256
        (base32 "1zqkfchmns5x90qxa8kg39bfhax3vxf1mrdz3lhyb9fz1gp4difn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gperf pkg-config))
    (inputs
     (list tcl tk gtk+-2))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-tcl="
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

(define-public nvc
  (package
    (name "nvc")
    (version "1.11.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/nickg/nvc.git")
                     (commit (string-append "r" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0rh6xwzr1drgwa04gx6w4r968yxlvfkvzg92950akf7wyxf331k7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:out-of-source? #t
       #:configure-flags
       '("--enable-vhpi")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'clean-up
           (lambda _
             (delete-file "autogen.sh"))))))
    (native-inputs
     (list automake
           autoconf
           flex
           gnu-gettext
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
    (version "2.3.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://accellera.org/images/downloads/standards/"
             "systemc/systemc-" version ".tar.gz"))
       (sha256
        (base32 "0gvv3xmhiwx1izmzy06yslzqzh6ygrgmw53xqfmyvbz5a6ivk0ap"))))
    (native-inputs (list perl))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-debug")))
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
    (version "4.204")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/verilator/verilator")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0cji5c8870h895l2vxnz8g6z7msv23dzbjaf98va7kva0qlfy2fz"))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gettext" ,gettext-minimal)
       ("python" ,python)))
    (inputs
     (list perl systemc))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "LDFLAGS=-L"
                            (assoc-ref %build-inputs "systemc")
                            "/lib-linux64"))
       #:make-flags
       (list (string-append "LDFLAGS=-L"
                            (assoc-ref %build-inputs "systemc")
                            "/lib-linux64"))
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           (lambda _ (invoke "autoconf"))))
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
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/trabucayre/openFPGALoader.git")
                     (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "1v3bwzhsrnsn304cqhd5azn68cl847qv8w8cb8bl7372jiqz5wqq"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libftdi libusb hidapi zlib))
    (arguments
     `(#:tests? #f)) ; No tests exist
    (synopsis "Utility for programming FPGA")
    (description "This package provides a program to transfer a bitstream
to an FPGA.")
    (home-page "https://f4pga.org/")
    (license license:asl2.0)))
