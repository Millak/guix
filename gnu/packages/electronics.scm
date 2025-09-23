;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2018–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2022, 2023, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2024 Juliana Sims <juli@incana.org>
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 Konstantinos Agiannis <agiannis.kon@gmail.com>
;;; Copyright © 2015-2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022, 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2016 Danny Milosavljevic <dannym@scratchpost.org>
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2021 Andrew Miloradovsky <andrew@interpretmath.pw>
;;; Copyright © 2022 Christian Gelinek <cgelinek@radlogic.com.au>
;;; Copyright © 2022 jgart <jgart@dismail.de>
;;; Copyright © 2023 Simon South <simon@simonsouth.net>
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

(define-module (gnu packages electronics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system qt)
  #:use-module (guix deprecation)
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
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages embedded)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
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
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages stb)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages textutils)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages toolkits)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

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
    (version "0.57")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/YosysHQ/abc/")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "191hsznsmsjn8100n50qsh3ng8wgrnyfhr7qcnb8yskiwqp37pjh"))))
    (home-page "https://github.com/YosysHQ/abc/")
    (description "ABC is a program for sequential logic synthesis and
formal verification.  This is the Yosyshq fork of ABC.")
    (license (license:non-copyleft "file:///copyright.txt"))))

(define-public apycula
  (package
    (name "apycula")
    (version "0.25")
    ;; The pypi tar.gz file includes the necessary .pickle files, not available
    ;; in the home-page repository.
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "apycula" version))
       (sha256
        (base32 "0pdq6mi8f2ablc1m85ip6sr6ih5ysm2d1k6kcsh6r62vwrxdrfh8"))))
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

(define-public camv-rnd
  (package
    (name "camv-rnd")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.hu/projects/camv-rnd/"
                           "releases/camv-rnd-" version ".tar.gz"))
       (sha256
        (base32
         "1dp1vj5rpxlddx40paa9i727c92is3bz6z6pa0y6dy2nsjcm86fs"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/route-rnd/")
    (synopsis "Viewer for electronic boards in CAM file formats")
    (description
     "@code{Camv-rnd} is a viewer for @acronym{PCB, Printed Circuit Board}
supporting gerber, excellon and g-code.  It is part of the RiNgDove EDA
suite.")
    (license license:gpl2+)))

(define-public comedilib
  (package
    (name "comedilib")
    (version "0.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.comedi.org/download/comedilib-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0jdw5gp02d8q3p4ldjrc3zaw0v435kmn3c95pv094gyxj3pwhacm"))))
    (build-system gnu-build-system)
    (synopsis "Library for Comedi")
    (description "Comedilib is a user-space library that provides a
developer-friendly interface to Comedi devices.  Comedi is a collection of
drivers for a variety of common data acquisition plug-in boards.  The drivers
are implemented as a core Linux kernel module providing common functionality and
individual low-level driver modules.")
    (home-page "https://www.comedi.org/")
    (license license:lgpl2.1)))

(define-public ieee-p1076
  (package
    (name "ieee-p1076")
    (version "2019")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://opensource.ieee.org/vasg/Packages/")
             (commit (string-append "1076-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1va626i5ww2ziw3dghw0d2mq7mrj5dwcn0h019h77866yw2pq9xn"))))
    (build-system copy-build-system)
    (native-inputs (list python-minimal-wrapper nvc python-vunit))
    (arguments
     (list
      ;; Not all 2019 features are supported by nvc compiler.
      ;; pass 1055 of 1648
      #:tests? #f
      #:install-plan
      #~'(("ieee" "share/ieee/p1076/ieee" #:include ("vhdl"))
          ("std" "share/ieee/p1076/std" #:include ("vhdl")))))
    (native-search-paths
     (list (search-path-specification
             (variable "IEEE-1076")
             (separator #f)
             (files (list "share/ieee/p1076")))))
    (home-page "https://IEEE-P1076.gitlab.io")
    (synopsis "VHDL libraries corresponding to the IEEE 1076 standard")
    (description
     "Open source materials intended for reference by the IEEE standard 1076,
as approved and published by the @acronym{VHDL, Very High Speed Hardware
Description Language} Analysis and Standardization Group.")
    (license license:asl2.0)))

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
      #:bootstrap-scripts #~(list "autoconf.sh")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'ensure-native-baked-CC/CXX
                     (lambda _
                       ;; The compilers used to build are retained in
                       ;; bin/iverilog-vpi, which is a Makefile
                       ;; script. Normalize these to just 'gcc' and 'g++' to
                       ;; avoid having these set to cross compilers.
                       (substitute* "Makefile.in"
                         (("s;@IVCC@;\\$\\(CC);")
                          "s;@IVCC@;gcc;")
                         (("s;@IVCXX@;\\$\\(CXX);")
                          "s;@IVCXX@;g++;")))))))
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

(define-public icestorm
  (package
    (name "icestorm")
    (version "1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/YosysHQ/icestorm/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yh36kd23y4sk65g34r1h244ax9fj5c668y6pwqwaq3c0nmb3d28"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f               ;no tests
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
              (substitute* "config.mk"
                (("/usr/local")
                 #$output))
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
                (copy-recursively
                 "build/texinfo/projecticestorm-figures"
                 (string-append #$output
                                "/share/info/projecticestorm-figures")))))
          (delete 'configure))))
    (inputs (list libftdi))
    (native-inputs (list pkg-config
                         python-minimal
                         python-sphinx-rtd-theme
                         python-sphinxcontrib-svg2pdfconverter
                         texinfo))
    (home-page "https://prjicestorm.readthedocs.io/")
    (synopsis "Bitstream tools for Lattice iCE40 FPGAs")
    (description
     "Project IceStorm aims at documenting the bitstream format of Lattice
iCE40 FPGAs and providing simple tools for analyzing and creating bitstream
files.")
    (license license:isc)))

(define-public json-for-vhdl
  ;; No tagged releases.
  (let ((commit "0dc9e317440263cd4941f157f5e5668baa858ec2")
        (revision "0"))
    (package
      (name "json-for-vhdl")
      (version (git-version "20220905" revision commit)) ;last revision
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/Paebbels/JSON-for-VHDL/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1062g2c3dpsb67zhqrn1j04p7jl28g4mcxd6nhrqqfffjsvxkpw9"))))
      (build-system copy-build-system)
      (arguments
       (list
        #:install-plan
        #~'(("src" "share/json-for-vhdl"
             #:include ("vhdl")))
        #:phases
        #~(modify-phases %standard-phases
            ;; The examples/Encodings_VUnit test requires vhdl builtins.
            (add-after 'unpack 'fix-check
              (lambda _
                (substitute* "tests/VUnit/run.py"
                  (("from_argv\\(\\)")
                   "from_argv()\nvu.add_vhdl_builtins()"))))
            (add-after 'install 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "python3" "tests/VUnit/run.py" "-v")))))))
      (native-inputs
       (list nvc python-minimal python-vunit))
      (home-page "https://github.com/Paebbels/JSON-for-VHDL/")
      (synopsis "Parse and query JSON data structures in VHDL")
      (description
       "The JSON-for-VHDL library provides a parser to query JSON data
structures from external files on disk.  It provides a context to be
used in the declarative section of design units.")
      (license license:asl2.0))))

;;; Required by python-vunit.
(define json-for-vhdl-for-vunit
  (let ((commit "95e848b8902c6b4275d715462e1a2cc60706917c") ;sync with vunit
        (revision "0"))
    (package
      (inherit json-for-vhdl)
      (name "json-for-vhdl-for-vunit")
      (version (git-version "20220106" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/Paebbels/JSON-for-VHDL/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1c106hm0sfnzdi5j9vaacjlz7i5m1dm75j7lrgcdsa4siw5ac7k3"))))
      (arguments
       (list
        #:install-plan
        #~'(("src" "share/json-for-vhdl" #:include ("vhdl")))))
      (native-inputs
       '()))))

(define librnd
  (package
    (name "librnd")
    (version "4.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.repo.hu/projects/librnd/"
                                  "releases/librnd-" version ".tar.bz2"))
              (sha256
               (base32
                "1qjv6gg9fb3rpvr1y9l5nbzz2xk2sa4nqz0dgwvds5hc1bmd97mf"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:tests? #false                   ;no check target
      #:phases
      #~(modify-phases %standard-phases
          ;; The configure script doesn't tolerate most of our configure
          ;; flags.
          (replace 'configure
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs
     (list gd glib glu gtk gtkglext libepoxy))
    (native-inputs
     (list pkg-config))
    (home-page "http://repo.hu/projects/librnd/")
    (synopsis "Two-dimensional CAD engine")
    (description "This is a flexible, modular two-dimensional CAD engine
@itemize
@item with transparent multiple GUI toolkit support;
@item a flexible, dynamic menu system;
@item a flexible, dynamic configuration system; and
@item support for user scripting in a dozen languages.
@end itemize")
    (license license:gpl2+)))

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

(define-public libserialport
  (package
    (name "libserialport")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "git://sigrok.org/libserialport")
                    (commit (string-append name "-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0dn10gmm3rwdsiw1psaczb9m52x6cfkfrbywm4f5y8fsmghh7dsy"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool))
    (home-page "https://sigrok.org/wiki/Libserialport")
    (synopsis "Library for using serial ports")
    (description "Libserialport is a minimal shared library written in C that is intended
to take care of the OS-specific details when writing software that uses serial ports.")
    (license license:lgpl3+)))

(define-public libsigrok
  (let ((commit "f06f788118191d19fdbbb37046d3bd5cec91adb1")
        (revision "2"))
    (package
      (name "libsigrok")
      (version (git-version "0.5.2" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "git://sigrok.org/libsigrok")
               (commit commit)))
         (sha256
          (base32 "1ahgpa0gaa4fl8c6frpgamvgxg0fisfwlqddr5x25456vkk2i9zi"))
         (file-name (git-file-name name version))))
      (outputs '("out" "doc"))
      (arguments
       (list
        #:tests? #f                      ; tests need USB access
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'change-udev-group
              (lambda _
                (substitute* (find-files "contrib" "\\.rules$")
                  (("plugdev") "dialout"))))
            (add-after 'build 'build-doc
              (lambda _
                (invoke "doxygen")))
            (add-after 'install 'install-doc
              (lambda _
                (copy-recursively
                 "doxy/html-api"
                 (string-append #$output:doc "/share/doc/libsigrok"))))
            (add-after 'install-doc 'install-udev-rules
              (lambda _
                (for-each
                 (lambda (file)
                   (install-file
                    file
                    (string-append #$output "/lib/udev/rules.d/")))
                          (find-files "contrib" "\\.rules$"))))
            (add-after 'install-udev-rules 'install-fw
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((fx2lafw (assoc-ref inputs "sigrok-firmware-fx2lafw"))
                       (dir-suffix "/share/sigrok-firmware/")
                       (input-dir (string-append fx2lafw dir-suffix))
                       (output-dir (string-append #$output dir-suffix)))
                  (for-each
                   (lambda (file)
                     (install-file file output-dir))
                   (find-files input-dir "."))))))))
      (native-inputs
       (list autoconf automake doxygen graphviz libtool
             sigrok-firmware-fx2lafw pkg-config))
      (inputs
       (list python zlib))
      ;; libsigrokcxx.pc lists "glibmm" in Requires libsigrok.pc lists
      ;; "libserialport", "libusb", "libftdi" and "libzip" in Requires.private
      ;; and "glib" in Requires
      (propagated-inputs
       (list glib
             glibmm-2.66
             libserialport
             libusb
             libftdi
             libzip))
      (build-system gnu-build-system)
      (home-page "https://www.sigrok.org/wiki/Libsigrok")
      (synopsis "Basic hardware access drivers for logic analyzers")
      (description "@code{libsigrok} is a shared library written in C which
provides the basic hardware access drivers for logic analyzers and other
supported devices, as well as input/output file format support.")
      (license license:gpl3+))))

(define-public libsigrokdecode
  (let ((commit "71f451443029322d57376214c330b518efd84f88")
        (revision "1"))
    (package
      (name "libsigrokdecode")
      (version (git-version "0.5.3" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/libsigrokdecode")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "11l8vnf2khqbaqas7cfnq3f8q5w7am6nbkkd5mqj5kpb3ya2avb9"))))
      (outputs '("out" "doc"))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'build 'build-doc
              (lambda _
                (invoke "doxygen")))
            (add-after 'install 'install-doc
              (lambda _
                (copy-recursively
                 "doxy/html-api"
                 (string-append #$output:doc
                                "/share/doc/libsigrokdecode")))))))
      (native-inputs
       (list check doxygen graphviz pkg-config automake autoconf libtool))
      ;; libsigrokdecode.pc lists "python" in Requires.private, and "glib" in
      ;; Requires.
      (propagated-inputs
       (list glib python))
      (build-system gnu-build-system)
      (home-page "https://www.sigrok.org/wiki/Libsigrokdecode")
      (synopsis
       "Library providing (streaming) protocol decoding functionality")
      (description
       "Libsigrokdecode is a shared library written in C, which provides
(streaming) protocol decoding functionality.")
      (license license:gpl3+))))

(define-public m8c
  (package
    (name "m8c")
    (version "2.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/laamaa/m8c")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vv0m4ry23nns5a47m2n9k6i3wly2jjc5n1j3l7sh1m480ga3d42"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:configure-flags
      #~(list "-DUSE_LIBSERIALPORT=ON")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libdecor libserialport sdl3))
    (home-page "https://github.com/laamaa/m8c")
    (synopsis "Cross-platform M8 tracker headless client")
    (description
     "The @url{https://dirtywave.com/products/m8-tracker,Dirtywave M8 Tracker}
is a portable sequencer and synthesizer, featuring 8 tracks of assignable
instruments such as FM, waveform synthesis, virtual analog, sample playback, and
MIDI output.  It is powered by a @url{https://www.pjrc.com/teensy/,Teensy}
micro-controller and inspired by the Gameboy tracker
@url{https://www.littlesounddj.com/lsd/index.php,Little Sound DJ}.  m8c is a
client for @url{https://github.com/Dirtywave/M8HeadlessFirmware,M8 Headless}
which allows one to install the M8 firmware on any Teensy.")
    (license (list license:cc-by-sa3.0
                   license:expat
                   license:public-domain
                   license:zlib))))

(define-public nextpnr
  (package
    (name "nextpnr")
    (version "0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/YosysHQ/nextpnr/")
              (commit (string-append "nextpnr-" version))
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
        (base32 "1wrlk0f4y29znd1zgl531lw4s0rfm5w8kx4hlwwdaj7b9vv3v65f"))))
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
    (license license:isc)))

(define-public nextpnr-ice40
  (deprecated-package "nextpnr-ice40" nextpnr))

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

(define-public openboardview
  (package
    (name "openboardview")
    (version "9.95.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/OpenBoardView/OpenBoardView")
                    (commit version)))
              (file-name (git-file-name name version))
              (modules '((ice-9 ftw)
                         (srfi srfi-26)
                         (guix build utils)))
              (snippet
               '(with-directory-excursion "src"
                  (define keep (list "." ".." "openboardview"))
                  (for-each (lambda (f)
                              (when (eq? 'directory (stat:type (lstat f)))
                                (delete-file-recursively f)))
                            (scandir "." (negate (cut member <> keep))))))
              (patches
               (search-patches "openboardview-use-system-imgui.patch"
                               "openboardview-use-system-mpc.patch"))
              (sha256
               (base32
                "1safjd729a7591rigkiy3c678bivrj5q1qwg1f18sijhlsfkf5b3"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:imported-modules `((guix build glib-or-gtk-build-system)
                           ,@%cmake-build-system-modules)
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build glib-or-gtk-build-system) #:prefix gtk:))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'configure-glad
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/CMakeLists.txt"
                (("add_subdirectory\\(glad\\)")
                 (string-append
                  ;; Configure Glad to use static Khronos XML specifications
                  ;; instead of attempting to fetch them from the Internet.
                  "option(GLAD_REPRODUCIBLE \"Reproducible build\" ON)\n"
                  ;; Use the CMake files from our glad package.
                  "add_subdirectory("
                  (search-input-directory inputs "share/glad") ;source_dir
                  " src/glad)\n")))))                          ;binary dir
          (add-before 'configure 'dynamically-load-gtk-via-absolute-path
            ;; The GTK library is not linked thus not present in the RUNPATH of
            ;; the produced binary; the absolute path of the libraries must to
            ;; the dynamic loader otherwise they aren't found.
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/openboardview/unix.cpp"
                (("libgtk-3.so")
                 (search-input-file inputs "lib/libgtk-3.so")))))
          ;; Add the two extra phases from `glib-or-gtk-build-system'.
          (add-after 'install 'glib-or-gtk-compile-schemas
            (assoc-ref gtk:%standard-phases 'glib-or-gtk-compile-schemas))
          (add-after 'install 'glib-or-gtk-wrap
            (assoc-ref gtk:%standard-phases 'glib-or-gtk-wrap)))))
    (native-inputs
     (list pkg-config
           python
           glad-0.1
           stb-image
           utf8-h))
    (inputs
     (list fontconfig
           gtk+
           ;; OpenBoardView can build with Dear ImGui 1.88, but there are some
           ;; usability problems such as the difficulty to register clicks.
           imgui-1.87
           orangeduck-mpc
           sdl2
           sqlite
           zlib))
    (home-page "https://github.com/OpenBoardView/OpenBoardView")
    (synopsis "Viewer for BoardView files")
    (description "OpenBoardView is a viewer for BoardView files, which present
the details of a printed circuit board (PCB).  It comes with features
such as:
@itemize
@item Dynamic part outline rendering, including complex connectors
@item Annotations, for leaving notes about parts, nets, pins or location
@item Configurable colour themes
@item Configurable DPI to facilitate usage on 4K monitors
@item Configurable for running on slower systems
@item Reads FZ (with key), BRD, BRD2, BDV and BV* formats.
@end itemize")
    (license license:expat)))

(define-public pcb-rnd
  (package
    (name "pcb-rnd")
    (version "3.1.7b")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://repo.hu/projects/pcb-rnd/"
                                  "releases/pcb-rnd-" version ".tar.gz"))
              (sha256
               (base32
                "1djsa0w53l6nvhwv28rlhpva55ir9n3xdvjgnjj8fgvcmrqlzrsl"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/pcb-rnd/")
    (synopsis "Modular layout editor")
    (description "@code{Pcb-rnd} is a @acronym{Printed Circuit Board} layout
editor, part of the RiNgDove EDA suite.")
    (license license:gpl2+)))

(define-public prjtrellis
  ;; The last release is 2 years old; use the latest commit for now.
  (let ((commit "898329dddf6ce6463299973081f109d645b9c55f")
        (revision "0"))
    (package
      (name "prjtrellis")
      (version (git-version "1.4" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/YosysHQ/prjtrellis/")
               (commit commit)
               ;; Pull the bitstream database for ECP5 devices; this is useful
               ;; only by prjtrellis: there is no need to package it separately.
               (recursive? #t)))
         (file-name (git-file-name name version))
         (modules '((guix build utils)))
         (snippet
          ;; Remove bundled source code for which Guix has packages.
          '(with-directory-excursion "libtrellis/3rdparty"
             (for-each delete-file-recursively
                       '("pybind11"))))
         (sha256
          (base32 "1qljgn7rxz114vki21rms70zi9rgr4gw7crdfihxx1n68zgv60gg"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:configure-flags
        #~(list (string-append "-DPYBIND11_INCLUDE_DIR="
                               (search-input-directory %build-inputs
                                                       "include/pybind11")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _
                (chdir "libtrellis"))))))
      (native-inputs (list python))
      (inputs (list openocd boost pybind11))
      (synopsis "Placement and routing for ECP5 FPGAs")
      (description
       "Project Trellis is a Nextpnr backend compatible with ECP5 FPGAs.
The following features are currently available:
@itemize
@item logic slice functionality, including carries
@item distributed RAM inside logic slices
@item all internal interconnect
@item basic IO, including tristate
@item block RAM, using inference or manual instantiation
@item multipliers using manual instantiation
@item global networks and PLLs
@item transcievers (DCUs.)
@end itemize")
      (home-page "https://github.com/YosysHQ/prjtrellis/")
      (license license:expat))))

(define-public opensta
  ;; There are no releases, we use last commit.
  (let ((commit "12f03395ec80d3593f4796b2a3cf5480e75735bd")
        (revision "0"))
    (package
      (name "opensta")
      ;; The version string is taken from the CMakeLists.txt.
      (version (git-version "2.7.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/parallaxsw/OpenSTA/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1gka50p4wv2b49d8jbw5fs3qg7cppa8ynl3diqgdf8mqgskwapzf"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; Tests expect output sta binary inside source tree.
        #:out-of-source? #f
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "../test/regression"))))
            (add-before 'build 'create-build-dir
              (lambda _
                (mkdir-p "./build")
                (chdir "./build"))))
        #:configure-flags
        #~(list
           (string-append "-DCUDD_DIR=" #$(this-package-input "cudd"))
           (string-append "-DBUILD_SHARED_LIBS=YES")
           "-B./build")))
      (native-inputs (list bison flex swig))
      (inputs (list cudd eigen tcl tcllib zlib))
      (synopsis "Parallax Static Timing Analyzer")
      (description
       "OpenSTA is a gate level static timing verifier.  As a stand-alone
executable it can be used to verify the timing of a design using standard file
formats.")
      (home-page "https://github.com/parallaxsw/OpenSTA/")
      (license license:gpl3+))))

(define-public pulseview
  (package
    (name "pulseview")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://sigrok.org/download/source/pulseview/pulseview-"
             version ".tar.gz"))
       (sha256
        (base32
         "1jxbpz1h3m1mgrxw74rnihj8vawgqdpf6c33cqqbyd8v7rxgfhph"))
       (patches (search-patches "pulseview-qt515-compat.patch"
                                "pulseview-glib-2.68.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ;format_time_minutes_test is failing
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'remove-empty-doc-directory
            (lambda _
              (with-directory-excursion (string-append #$output "/share")
                ;; Use RMDIR to never risk silently deleting files.
                (rmdir "doc/pulseview")
                (rmdir "doc")))))))
    (native-inputs
     (list pkg-config qttools-5))
    (inputs
     (list boost
           glib
           glibmm
           libsigrok
           libsigrokdecode
           qtbase-5
           qtsvg-5))
    (home-page "https://www.sigrok.org/wiki/PulseView")
    (synopsis "Qt based logic analyzer, oscilloscope and MSO GUI for sigrok")
    (description "PulseView is a Qt based logic analyzer, oscilloscope and MSO
GUI for sigrok.")
    (license license:gpl3+)))

(define-public osvvm
  (package
    (name "osvvm")
    (version "2025.06")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/osvvm/OsvvmLibraries/")
              (commit version)
              ;; OsvvmLibraries repository gathers all osvvm libraries as
              ;; submodules.
              (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08mfh7pyrb26mp8wx3xjns79slb3yf1c78nf8y1awvxc1p8q1wq4"))))
    (outputs
     '("out" "common" "scripts" "uart" "axi4"))
    (properties
     `((output-synopsis "out" "Verification Utility Library")
       (output-synopsis "common" "Common library")
       (output-synopsis "scripts" "Simulator script library")
       (output-synopsis "uart" "UART Verification Component Library")
       (output-synopsis "axi4" "AXI4 Verification Component Library")))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("osvvm" "share/osvvm/osvvm/"
           #:include ("vhd" "pro" "md")
           #:output "out")
          ("Common" "share/osvvm/Common/"
           #:include ("vhd" "pro" "md")
           #:output "common")
          ("Scripts" "share/osvvm/Scripts/"
           #:include ("tcl" "md")
           #:output "scripts")
          ("UART" "share/osvvm/UART"
           #:include ("vhd" "pro" "md")
           #:exclude-regexp ("GHDL_Debug")
           #:output "uart")
          ("AXI4" "share/osvvm/AXI4"
           #:include ("vhd" "pro" "md")
           #:output "axi4"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'fix-scripts
            (lambda _
              ;; Default conflicts with read-only /gnu/store.
              (substitute* "osvvm/OsvvmVhdlSettings.pro"
                (("\\[FindOsvvmSettingsDirectory\\]")
                 " \"\" ")))))))
    (native-search-paths
     (list (search-path-specification
             (variable "OSVVM")
             (separator #f)
             (files (list "share/osvvm")))))
    (home-page "https://osvvm.github.io/Overview/Osvvm1About.html/")
    (synopsis "The OSVVM VHDL Verification Libraries and Scripts")
    (description "OSVVM is a verification methodology that defines a VHDL
verification framework, verification utility library, verification component
library, scripting API, and co-simulation capability for FPGA or ASIC
verification.")
    (license license:asl2.0)))

;;; Required by python-vunit.
(define osvvm-2023.04
  (package
    (inherit osvvm)
    (name "osvvm")
    (version "2023.04")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/osvvm/OsvvmLibraries/")
              (commit version)
              ;; OsvvmLibraries repository gathers all osvvm libraries as
              ;; submodules.
              (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kn18ibvm7bzdyw2d914284wriravyh5qwfarj06pb052x1yblyx"))))
    (arguments
     (substitute-keyword-arguments (package-arguments osvvm)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (delete 'fix-scripts)))))))

(define-public python-cocotb
  (package
    (name "python-cocotb")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/cocotb/cocotb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b9wc14df11wkwa20wkij4iip07841qsr0yir9g7dww069rj36q6"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" "not test_toplevel_library")));requires questasim simulator
    (native-inputs
     (list iverilog
           nvc
           python-pytest
           python-setuptools-next))
    (propagated-inputs
     (list python-find-libpython))
    (home-page "https://github.com/cocotb/cocotb")
    (synopsis "Library for writing HDL test benches in Python")
    (description
     "Coroutine based cosimulation test bench environment for verifying VHDL
and Verilog RTL using Python.")
    (license license:bsd-3)))

(define-public python-cocotb-bus
  ;; XXX: The latest tagged release (2.6.1) was placed on <2023-07-01>, switch
  ;; to tag when the fresh release is available.
  (let ((commit "c3541f15c43c914d8cf3e57ecf92c5d256c97e6c")
        (revision "1"))
    (package
      (name "python-cocotb-bus")
      ;; Version from src/cocotb_bus/_version.py
      (version (git-version "0.2.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cocotb/cocotb-bus/")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1xx0w9g8psn4f8qlv7jzdwwr2ivvlaxi3flgp1v0qybzzhz6l1k3"))))
      (build-system pyproject-build-system)
      ;; TODO: Build documentation from <docs>.
      (arguments
       (list
        #:tests? #f                     ;not compatible with cocotb 2.0
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "make" "-k" "-C" "tests")
                  (invoke "make" "-k" "-C" "examples")))))))
      (native-inputs
       (list iverilog
             nvc
             python-pytest
             python-setuptools-next))
      (propagated-inputs
       (list python-cocotb
             python-packaging
             python-scapy))
      (home-page "https://github.com/cocotb/cocotb-bus/")
      (synopsis "Cocotb reusable tools")
      (description "@code{Cocotb-bus} provides a set of utilities, test benches
and reusable bus interfaces to be used with @code{cocotb}.")
      (license license:bsd-3))))

(define-public python-edalize
  (package
    (name "python-edalize")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/olofk/edalize/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03mkzkmi96jkrpgcnawixvy832p3b8li8lrirdjhfp9dmp7d5kg5"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "-k" (string-join
                    ;; XXX: Tests failing with assertion not equal, find out
                    ;; why.
                    (list "not test_gatemate"
                          "test_vcs_tool_options"
                          "test_vcs_no_tool_options"
                          "test_vcs_minimal"
                          "test_xcelium")
                    " and not "))))
    (native-inputs
     (list python-pytest python-setuptools-next))
    (propagated-inputs
     (list python-jinja2))
    (home-page "https://github.com/olofk/edalize/")
    (synopsis "Python Library for interacting with EDA tools")
    (description
     "This package implements a functionality to create project files for
supported tools and run them in batch or GUI mode.  All EDA tools such as
Icarus, Yosys, ModelSim, Vivado, Verilator, GHDL, Quartus etc get input HDL
files (Verilog and VHDL) and some tool-specific files (constraint files,memory
initialization files, IP description files etc).  Together with the files,
perhaps a couple of Verilog `defines, some top-level parameters/generics or
some tool-specific options are set.")
    (license license:bsd-2)))

(define-public python-hdlmake
  (let ((commit "c56cb8efa2000d06cec698f0149bc4ca4ef4e5bc")
        (revision "3"))
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
          (base32 "1w4b0g015rzfypr8sjyr8xqij6s2p5qxhxraswrhjvg1w86b6s36"))))
      (build-system pyproject-build-system)
      (arguments (list #:phases #~(modify-phases %standard-phases
                                    (add-before 'check 'chdir
                                      (lambda _
                                        (chdir "testsuite"))))
                       #:test-flags #~(list "test_all.py")))
      (native-inputs (list python-pytest python-setuptools-next))
      (propagated-inputs (list python-networkx python-six))
      (home-page "https://ohwr.gitlab.io/project/hdl-make/")
      (synopsis "Generate multi-purpose makefiles for HDL projects")
      (description
       "Hdlmake helps manage and share @acronym{HDL, hardware description
language} code by automatically finding file dependencies, writing synthesis
and simulation Makefiles.")
      (license license:gpl3+))))

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
  (let ((commit "7dc29c242cd33cb835c336a81ffc3a461eaa92f4")
        (revision "0"))
    (package
      (name "python-myhdl")
      (version (git-version "0.11" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/myhdl/myhdl/")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1b91yvr0ksrw3bx61i7914caf8pyks9c242kwmj4l12zjd06mp56"))))
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda* (#:key tests? #:allow-other-keys)
                (when tests?
                  (invoke "make" "iverilog" "core")))))))
      (build-system pyproject-build-system)
      (native-inputs
       (list iverilog python-setuptools-next python-pytest))
      (home-page "http://www.myhdl.org/")
      (synopsis "Python as a Hardware Description Language")
      (description "This package provides a library to turn Python into
a hardware description and verification language.")
      (license license:lgpl2.1+))))

(define-public python-pydigitalwavetools
  (package
    (name "python-pydigitalwavetools")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Nic30/pyDigitalWaveTools/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fll8anz3i1j1nngsij1psp8766kvdfpls655lbxn2ykypv3633m"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/Nic30/pyDigitalWaveTools/")
    (synopsis "Library to manipulate digital wave files")
    (description
     "Pydigitalwavetools is a Python library to parse, write and format digital
wave files in @acronym{VCD, Value Change Dump} format, a standardized ASCII
format used to store simulation data from Verilog and other hardware description
languages.")
    (license license:expat)))

(define-public python-surf
  (package
    (name "python-surf")
    (version "2.57.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/slaclab/surf/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ncb34mdxaw0m6cnk7kvl7mkhwa6hpcxkc2lgarwcmmnfydr8kg3"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-deps
            (lambda _
              (invoke "git" "init") ;expects a git repo
              ;; fix version
              (substitute* "setup.py"
                (("rawVer .*")
                 (string-append "rawVer = \"v"
                                #$version "\""))))))))
    (native-inputs (list python-setuptools python-wheel python-gitpython
                         git-minimal/pinned))
    (home-page "https://slaclab.github.io/surf/")
    (synopsis "SLAC Ultimate RTL Framework")
    (description
     "Surf is a python library with support functions for VHDL gateware
digital design.  It provides implementation modules compatible with FPGA and ASIC
design.")
    (license (license:non-copyleft "file://LICENSE.txt"
                                   "See LICENSE.txt in the distribution."))))

(define-public python-vsg
  (package
    (name "python-vsg")
    (version "3.34.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jeremiah-c-leary/vhdl-style-guide/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sryf1wv4r5maxj4di5rpsmzcxins3gq8aksv7cpw6ywvdk1nj5l"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; Tests are expensive and may introduce race condition on systems with
      ;; high (more than 16) threads count; limit parallel jobs to 8x.
      #~(list
         "--numprocesses" (number->string (min 8 (parallel-job-count))))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'pathch-pytest-options
            (lambda _
              (substitute* "pyproject.toml"
                ((".*--cov=.*") "")
                ((".*--self-contained-html.*") "")
                ((".*-n.*auto.*") "")))))))
    (native-inputs
     (list python-pytest-cov
           python-pytest-html
           python-pytest-xdist
           python-setuptools-next))
    (propagated-inputs
     (list python-pyyaml))
    (home-page "https://github.com/jeremiah-c-leary/vhdl-style-guide/")
    (synopsis "Coding style enforcement for VHDL")
    (description
     "VSG lets you define a VHDL coding style and provides a command-line tool
to enforce it.")
    (license license:gpl3+)))

(define-public python-vunit
  (package
    (name "python-vunit")
    (version "5.0.0-dev.6") ;v4.7.0 dates back from 2 years ago.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/VUnit/vunit")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1si542jrrvibiigaridg2vds5smbiass7g5pdfk5z26xqgbh0fxc"))))
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
                  "generator\": \"llvm-jit\",")))))
          (add-after 'ensure-no-mtimes-pre-1980 'dosymlink
            (lambda* (#:key inputs #:allow-other-keys)
              (with-directory-excursion "vunit/vhdl/JSON-for-VHDL"
                (symlink
                 (search-input-directory inputs "/share/json-for-vhdl")
                 "src"))
              (with-directory-excursion "vunit/vhdl"
                (delete-file-recursively "osvvm")
                (symlink
                 (search-input-directory inputs "/share/osvvm/osvvm")
                 "osvvm")))))
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
    (inputs
     (list json-for-vhdl-for-vunit osvvm-2023.04))
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

(define-public qucsator-rf
  (package
    (name "qucsator-rf")
    (version "1.0.7")                   ;required by qucs-s, keep in sync
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ra3xdh/qucsator_rf/")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1qyih418r0jcrpk1ja4p7v9v5iqvri8iszg7s3vaf1d2agwblzb4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'run-tests
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; Qucs-test is a collection of python scripts and data test
                ;; cases. Its purpose is to test Qucs (GUI) and Qucsator;
                ;; tests are under `testsuite` directory.
                (copy-recursively
                 #$(origin
                     (method git-fetch)
                     (uri
                      ;; Using latest revision; refer to
                      ;; .github/workflows/cmake.yml to keep up to date.
                      (git-reference
                        (url "https://github.com/ra3xdh/qucs-test/")
                        (commit "ce69e05ceecab910175e6ea36b6e021a6d279947")))
                     (sha256
                      (base32
                       (string-append "1r3hx43wvd0s11mzsvj1chylzv"
                                      "0lk9qhaw7205j9x316ly03bl08"))))
                 "qucs-test")
                (with-directory-excursion "qucs-test"
                  (invoke "python3" "run.py" "--qucsator"
                          (format #f "--prefix=~a/bin" #$output)
                          "--exclude=skip.txt"))))))
      #:configure-flags
      #~(list (format #f "-DBISON_DIR=~a/bin"
                      #$(this-package-native-input "bison"))
              (format #f "-DADMSXML_DIR=~a/bin"
                      #$(this-package-native-input "adms")))))
    (native-inputs
     (list adms bison dos2unix flex gperf python python-looseversion
           python-numpy python-matplotlib))
    (synopsis "RF and microwave circuits simulator")
    (description
     "@code{Qucsator-rf} is a command line driven circuit simulator targeted
for RF and microwave circuits.  It takes a network list in a certain format as
input and outputs an XML dataset.")
    (home-page "https://ra3xdh.github.io//")
    (license license:gpl2+)))

(define-public qucs-s
  (package
    (name "qucs-s")
    (version "25.2.0")                  ;update qucsator-rf accordingly
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ra3xdh/qucs_s")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0328irynm8vy4xjdip5286fd8nag1zdp0p6rcbhdhp4fca6wp5ak"))))
    (build-system qt-build-system)
    (arguments
     (list
      #:qtbase qtbase                   ;for Qt 6
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'adjust-default-settings
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "qucs/settings.cpp"
                (("\"ngspice\"")
                 (format #f "~s" (search-input-file inputs "bin/ngspice")))
                (("\"octave\"")
                 (format #f "~s" (search-input-file inputs "bin/octave"))))))
          (add-after 'install 'wrap-program
            (lambda _
              (wrap-program (string-append #$output "/bin/qucs-s")
                `("PATH" ":" prefix
                  (,(string-append #$(this-package-input "ngspice") "/bin")
                   ,(string-append
                     #$(this-package-input "qucsator-rf") "/bin")))))))))
    (native-inputs (list qttools))
    (inputs
     ;; TODO Add xyce-serial to the list.
     (list bash-minimal octave qtbase qtcharts qtsvg qtwayland qucsator-rf ngspice))
    (synopsis "GUI for different circuit simulation kernels")
    (description
     "@acronym{Qucs-S, Quite universal circuit simulator with SPICE} provides
a fancy graphical user interface for a number of popular circuit simulation
engines.  The package contains libraries for schematic capture, visualization
and components.  The following simulation kernels are supported:
@itemize
@item Ngspice (recommended)
@item Xyce
@item SpiceOpus
@item Qucsator (non-SPICE)
@end itemize\n")
    (home-page "https://ra3xdh.github.io/")
    (license license:gpl2+)))

(define-public xschem
  (package
    (name "xschem")
    (version "3.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/StefanSchippers/xschem")
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0g9qrzm2mjd7nfg8iyc5az2bs8n5gjv1mrjjdja5vn1yjia7pvy9"))))
    (native-inputs (list flex bison pkg-config))
    (inputs (list gawk
                  tcl
                  tk
                  libxpm
                  cairo
                  libxrender
                  libxcb)) ; Last 3 are optional, but good to have.
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'setenv
            (lambda* (#:key outputs #:allow-other-keys)
              (setenv "CC" #$(cc-for-target))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (synopsis "Hierarchical schematic editor")
    (description
     "Xschem is an X11 schematic editor written in C and focused on
hierarchical and parametric design.  It can generate VHDL, Verilog or Spice
netlists from the drawn schematic, allowing the simulation of the circuit.")
    (home-page "https://xschem.sourceforge.io/stefan/index.html")
    (license license:gpl2+)))

(define-public route-rnd
  (package
    (name "route-rnd")
    (version "0.9.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.hu/projects/route-rnd/"
                           "releases/route-rnd-" version ".tar.gz"))
       (sha256
        (base32
         "0fy3b48s72lpicyap3y6jr9fyvb2ri42jb0gqxk6s927a278bfhc"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/route-rnd/")
    (synopsis "Automatic routing for electronics boards")
    (description
     "@code{Route-rnd} is a generic external autorouter for @acronym{PCB,
Printed Circuit Board} using tEDAx file format, part of the RiNgDove EDA
suite.")
    (license license:gpl2+)))

(define-public sch-rnd
  (package
    (name "sch-rnd")
    (version "1.0.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://repo.hu/projects/sch-rnd/"
                           "releases/sch-rnd-" version ".tar.gz"))
       (sha256
        (base32
         "07a1ik0rpsa5cscg9l7i5rnipx76543s7cdnkg802747rral7yj5"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; The configure script doesn't tolerate most of our configure
            ;; flags.
            (lambda _
              (setenv "CC" #$(cc-for-target))
              (setenv "LIBRND_PREFIX" #$(this-package-input "librnd"))
              (invoke "./configure" (string-append "--prefix=" #$output)))))))
    (inputs (list librnd))
    (home-page "http://repo.hu/projects/sch-rnd/")
    (synopsis "Scriptable editor of schematics for electronics boards")
    (description
     "@code{Sch-rnd} is a standalone and workflow agnostic schematics capture
tool for @acronym{PCB, Printed Circuit Board}, part of the RiNgDove EDA
suite.")
    (license license:gpl2+)))

(define-public sigrok-cli
  (package
    (name "sigrok-cli")
    (version "0.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://sigrok.org/download/source/sigrok-cli/sigrok-cli-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1f0a2k8qdcin0pqiqq5ni4khzsnv61l21v1dfdjzayw96qzl9l3i"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libsigrok libsigrokdecode))
    (build-system gnu-build-system)
    (home-page "https://sigrok.org/wiki/Sigrok-cli")
    (synopsis "Command-line frontend for sigrok")
    (description "Sigrok-cli is a command-line frontend for sigrok.")
    (license license:gpl3+)))

(define-public sigrok-firmware-fx2lafw
  ;; The project's last formal release was in 2019.
  ;;
  ;; The changes since then allow it to build with the latest version of SDCC,
  ;; 4.3.0.
  (let ((commit "96b0b476522c3f93a47ff8f479ec08105ba6a2a5")
        (revision "1"))
    (package
      (name "sigrok-firmware-fx2lafw")
      (version (git-version "0.1.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "git://sigrok.org/sigrok-firmware-fx2lafw")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1n5nj2g2m5ih59591ny2drrv25zviqcwyx1cfdhy8ijl82yxjkmb"))))
      (build-system gnu-build-system)
      (arguments
       (list #:tests? #f))              ; no test suite
      (native-inputs
       (list autoconf automake sdcc))
      (home-page "https://www.sigrok.org/wiki/Fx2lafw")
      (synopsis "Firmware for Cypress FX2 chips")
      (description "Fx2lafw is free firmware for Cypress FX2 chips which makes
them usable as simple logic analyzer and/or oscilloscope hardware.")
      (license license:gpl2+))))

(define-public symbiyosys
  (package
    (name "symbiyosys")
    (version "0.57")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/YosysHQ/sby/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w95svb9vgvfqbbvqaq6jkia5jldbai9l7r8nrx93wcfrm82r36x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:modules `((guix build gnu-build-system)
                  ((guix build python-build-system) #:prefix python:)
                  (guix build utils))
      #:imported-modules `(,@%default-gnu-imported-modules
                           (guix build python-build-system))
      #:make-flags #~(list (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          ;; TODO: build docs, after furo-ys is packaged.
          ;; (add-after 'install 'build-info
          ;; (lambda _
          ;; (invoke "make" "-C" "docs" "info")))
          (add-before 'check 'git-init
            (lambda _
              (invoke "git" "init")))   ;check expects a git repo
          (add-after 'git-init 'patch-/usr/bin/env
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "sbysrc/sby_core.py"
                (("\"/usr/bin/env\", ")
                 ""))
              (substitute* "sbysrc/sby.py"
                (("/usr/bin/env python")
                 (search-input-file inputs "bin/python3")))))
          (add-after 'install 'python:wrap
            (assoc-ref python:%standard-phases 'wrap)))))
    (inputs (list abc-yosyshq
                  boolector
                  git-minimal/pinned
                  python
                  python-click
                  python-xmlschema
                  z3
                  yices
                  yosys))
    ;; TODO: see above build-info phase comment.
    ;; (native-inputs (list
    ;;                 python-sphinx python-sphinx-argparse texinfo))
    (home-page "https://github.com/YosysHQ/sby/")
    (synopsis "Formal hardware verification with yosys")
    (description
     "SimbyYosys is a front-end program for yosys-based formal hardware
verification flows.")
    (license license:isc)))

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

(define-public uhdm
  (package
    (name "uhdm")
    (version "1.86")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chipsalliance/UHDM/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nsy385frxz5v7i757h1x59xkl21asz3h2fk1nyvx37z8cj0kd3z"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; there is no configure stage, as for INSTALL.md
          (delete 'configure))
      #:test-target "test"
      #:make-flags
      #~(list
         (string-append
          "ADDITIONAL_CMAKE_OPTIONS=-DUHDM_USE_HOST_CAPNP=On"
          ;; " -DUHDM_WITH_PYTHON=On"      ;FIXME
          " -DUHDM_USE_HOST_GTEST=On")
         (string-append "PREFIX=" #$output))))
    (native-inputs
     (list cmake-minimal googletest pkg-config python-wrapper swig))
    (inputs
     (list capnproto openssl python-orderedmultidict zlib))
    (home-page "https://github.com/chipsalliance/UHDM/")
    (synopsis "Universal Hardware Data Model")
    (description
     "UHDM is a complete modeling of the IEEE SystemVerilog Object Model with
VPI Interface, Elaborator, Serialization, Visitor and Listener.")
    (license license:asl2.0)))

(define-public verilator
  (package
    (name "verilator")
    (version "5.040")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/verilator/verilator/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xw2w7fikli3jffwd819rx8bwbh3zsymhrn3zbq34glklff07rsb"))))
    (native-inputs
     (list autoconf
           automake
           bison
           cmake-minimal
           flex
           gdb/pinned
           gettext-minimal
           help2man
           python-distro
           python-minimal
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
          (add-before 'check 'set-SYSTEMC_ROOT
            (lambda _
              (setenv "SYSTEMC_ROOT" #$(this-package-input systemc))))
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

(define-public xoscope
  (package
    (name "xoscope")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/xoscope/xoscope/"
                                  version "/xoscope-" version ".tar.gz"))
              (sha256
               (base32
                "0a5ycfc1qdmibvagc82r2mhv2i99m6pndy5i6ixas3j2297g6pgq"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 pkg-config))
    (inputs
     (list alsa-lib comedilib fftw gtk+ gtkdatabox))
    (synopsis "Digital oscilloscope")
    (description "Xoscope is a digital oscilloscope that can acquire signals
from ALSA, ESD, and COMEDI sources.  This package currently does not include
support for ESD sources.")
    (home-page "https://xoscope.sourceforge.net/")
    (license license:gpl2+)))

(define-public yosys
  (package
    (name "yosys")
    (version "0.57")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/YosysHQ/yosys")
              (commit (string-append "v" version))))
       (sha256
        (base32 "0bix5zlv9zp9fxqpn9l9bdw65xrgih5w0csq1xkkhm2c7p3vqjbb"))
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
