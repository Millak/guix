;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2019, 2020, 2023, 2024, 2025 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013 Nikita Karetnikov <nikita@karetnikov.org>
;;; Copyright © 2014, 2016, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2014-2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2014 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2014 Mathieu Lirzin <mathieu.lirzin@openmailbox.org>
;;; Copyright © 2015–2025 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2015, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2015 Fabian Harfert <fhmgufs@web.de>
;;; Copyright © 2016 Roel Janssen <roel@gnu.org>
;;; Copyright © 2016, 2018, 2020, 2021 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2016-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018, 2019, 2020, 2021 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2017, 2019, 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Dave Love <me@fx@gnu.org>
;;; Copyright © 2018, 2019, 2020, 2021, 2022, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2018 Joshua Sierles, Nextjournal <joshua@nextjournal.com>
;;; Copyright © 2018 Nadya Voronova <voronovank@gmail.com>
;;; Copyright © 2018 Adam Massmann <massmannak@gmail.com>
;;; Copyright © 2018, 2020-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Eric Brown <brown@fastmail.com>
;;; Copyright © 2018, 2021, 2024 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2019, 2021-2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2019 Steve Sprang <scs@stevesprang.com>
;;; Copyright © 2019 Robert Smith <robertsmith@posteo.net>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020–2023 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2020 R Veera Kumar <vkor@vkten.in>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020, 2021, 2025 Vinicius Monego <monego@posteo.net>
;;; Copyright © 2020 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2021 Gerd Heber <gerd.heber@gmail.com>
;;; Copyright © 2021 Franck Pérignon <franck.perignon@univ-grenoble-alpes.fr>
;;; Copyright © 2021 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2021, 2022 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021 Ivan Gankevich <i.gankevich@spbu.ru>
;;; Copyright © 2021 Jean-Baptiste Volatier <jbv@pm.me>
;;; Copyright © 2021, 2023, 2024 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Pierre-Antoine Bouttier <pierre-antoine.bouttier@univ-grenoble-alpes.fr>
;;; Copyright © 2022 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2022 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2022 Marek Felšöci <marek@felsoci.sk>
;;; Copyright © 2022 vicvbcun <guix@ikherbers.com>
;;; Copyright © 2022, 2023 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2022 Maximilian Heisinger <mail@maxheisinger.at>
;;; Copyright © 2022 Akira Kyle <akira@akirakyle.com>
;;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2022 Roman Scherer <roman.scherer@burningswell.com>
;;; Copyright © 2023 Jake Leporte <jakeleporte@outlook.com>
;;; Copyright © 2023 Camilo Q.S. (Distopico) <distopico@riseup.net>
;;; Copyright © 2023, 2025 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2025 Luca Cirrottola <luca.cirrottola@inria.fr>
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

(define-module (gnu packages maths)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 match)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module ((guix build utils) #:select (alist-replace))
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system dune)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system ocaml)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages astronomy)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages calendar)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages coq)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages datamash)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages django)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fltk)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages ghostscript)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages image-processing)
  #:use-module (gnu packages java)
  #:use-module (gnu packages less)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages netpbm)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages prolog)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages scheme)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages simulation)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xml)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26))

(define-public aris
  (package
    (name "aris")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/" name "/" name "-" version ".tar.gz"))
              (sha256 (base32
                       "1q1887ryqdr9sn0522hc7p16kqwlxxyz5dkmma8ar2nxplhgll7q"))))
    (build-system gnu-build-system)
    (inputs (list gtk+ libxml2))
    (native-inputs (list pkg-config))
    (arguments
     `(#:configure-flags '("CFLAGS=-O2 -g -fcommon")))
    (synopsis "Natural deduction first-order logic interface")
    (description "Aris is a program for performing logical proofs.  It supports
propositional and predicate logic, as well as Boolean algebra and
arithmetical logic.  In addition to its predefined inference and equivalence
rules, Aris also supports references to older proofs.  Its use of standard
logical symbols and its natural deduction interface make it easy to use for
beginners.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/aris/")))

(define-public bitwise
  (package
    (name "bitwise")
    (version "0.50")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mellowcandle/bitwise"
                                  "/releases/download/v" version
                                  "/bitwise-v" version ".tar.gz"))
              (sha256
               (base32 "0zp9rb0qv1m9hk593sc08jajkxd80h075s0m0dhf07gkbgx72ql0"))))
    (build-system gnu-build-system)
    (inputs
     (list ncurses readline))
    (native-inputs
     (list cunit pkg-config))
    (synopsis "Terminal based bit manipulator in ncurses")
    (description "Bitwise is a multi base interactive calculator supporting
dynamic base conversion and bit manipulation.  It's a handy tool for low level
hackers, kernel developers and device drivers developers.

Some of the features include:
@itemize
@item Interactive ncurses interface.
@item Command line calculator supporting all bitwise operations.
@item Individual bit manipulator.
@item Bitwise operations such as NOT, OR, AND, XOR, and shifts.
@end itemize")
    (license license:gpl3+)
    (home-page "https://github.com/mellowcandle/bitwise/")))

(define-public c-graph
  (package
   (name "c-graph")
   (version "2.0.1")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/c-graph/c-graph-" version
                                ".tar.gz"))
            (sha256 (base32
                     "092412jzxy6wdvpk96pfj499hpmaww8xllavbvlqspfpr7ips9id"))))
   (build-system gnu-build-system)
   (inputs
    (list gfortran))
   (synopsis "Visualizing and demonstrating convolution")
   (description
    "GNU C-Graph is a tool for demonstrating the theory of convolution.
Thus, it can serve as an excellent aid to students of signal and systems
theory in visualizing the convolution process.  Rather than forcing the
student to write code, the program offers an intuitive interface with
interactive dialogs to guide them.")
   (license license:gpl3+)
   (home-page "https://www.gnu.org/software/c-graph/")))

(define-public calc
  (package
    (name "calc")
    (version "2.14.2.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.isthe.com/chongo/src/calc/calc-"
                           version ".tar.bz2"))
       (sha256
        (base32 "1swalx3cxjcx4aprnchb2jf0wig89ggvxjzzzx488r115w58lxnr"))))
    (build-system gnu-build-system)
    (inputs (list readline))
    (native-inputs (list util-linux)) ; for col
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-before 'build 'patch-makefile
                          (lambda _
                            (substitute* "Makefile"
                              (("^PREFIX= /usr/local")
                               (string-append "PREFIX=" #$output))
                              (("=\\s?/usr")
                               "= ${PREFIX}")))))))
    (synopsis "Arbitrary precision console calculator")
    (description
     "Calc is an arbitrary precision arithmetic system that uses a C-like
language.  It can be used as a calculator, an algorithm prototyper and as
a mathematical research tool, and it comes with built in mathematical and
programmatic functions.")
    (home-page "http://www.isthe.com/chongo/tech/comp/calc/")
    (license license:lgpl2.1)))

(define-public chuffed
  (package
    (name "chuffed")
    (version "0.13.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chuffed/chuffed")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "164brmwn71p9gb2441kh7b1gzmy2sg7bjv5z00wjs9nw41qc908g"))))
    (build-system cmake-build-system)
    (arguments
      (list #:tests? #f ;no 'test' target
            #:phases
            #~(modify-phases %standard-phases
                (add-before 'build 'patch-msc
                  (lambda* (#:key outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out")))
                      (substitute* "chuffed.msc"
                        (("\\.\\./../..") out)
                        (("\\.\\.")
                         (string-append out "/share/minizinc")))))))))
    (synopsis "Lazy clause generation solver")
    (description
     "Chuffed is a state of the art lazy clause solver designed from the
ground up with lazy clause generation in mind.  Lazy clause generation
is a hybrid approach to constraint solving that combines features of
finite domain propagation and Boolean satisfiability.")
    (home-page "https://github.com/chuffed/chuffed")
    (license license:expat)))

(define-public coda
  (package
    (name "coda")
    (version "2.19")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/stcorp/coda/releases/download/"
                           version "/coda-" version ".tar.gz"))
       (sha256
        (base32 "1fbxd2afm7dshd92p10yy8dwbr9gc1h1fmnnnmr7d0c5lnw80245"))
       (patches (search-patches "coda-use-system-libs.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure we don't use the bundled software.
        '(begin
           (for-each (lambda (d)
                       (delete-file-recursively (string-append "libcoda/" d)))
                     '("zlib" "pcre" "expat"))
           #t))))
    (native-inputs
     `(("fortran" ,gfortran)
       ("python" ,python)
       ("python-numpy" ,python-numpy)))
    (inputs
     `(("zlib" ,zlib)
       ("pcre" ,pcre)
       ("expat" ,expat)
       ("hdf4" ,hdf4-alt)
       ("hdf5" ,hdf5)))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--with-hdf4" "--with-hdf5" "--enable-python"
                           "LIBS= -lz -lpcre -lexpat")))
    (synopsis "Common interface to various earth observation data formats")
    (description
     "The Common Data Access toolbox (CODA) provides a set of interfaces for
reading remote sensing data from earth observation data files.  It consists of
command line applications and interfaces to the C, Fortran, Python, and Java
programming languages.")
    (home-page "https://stcorp.nl/coda")
    (license license:gpl2+)))

(define-public qhull
  (package
    (name "qhull")
    (version "2020.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.qhull.org/download/qhull-"
                                  (car (string-split version #\.))
                                  "-src-8.0.2.tgz"))
              (sha256
               (base32
                "0zlbhg0lb6j60188c2xhcrvviskr079552icjldqhy1jhgmxghmm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DLINK_APPS_SHARED=ON"
                           "-DCMAKE_POSITION_INDEPENDENT_CODE=ON")))
    (synopsis "Calculate convex hulls and related structures")
    (description
     "@code{Qhull} computes the convex hull, Delaunay triangulation, Voronoi
diagram, halfspace intersection about a point, furthest-site Delaunay
triangulation, and furthest-site Voronoi diagram.  The source code runs in 2-d,
3-d, 4-d, and higher dimensions.  @code{Qhull} implements the Quickhull
algorithm for computing the convex hull.  It handles roundoff errors from
floating point arithmetic.  It computes volumes, surface areas, and
approximations to the convex hull.

@code{Qhull} does not support triangulation of non-convex surfaces, mesh
generation of non-convex objects, medium-sized inputs in 9-D and higher, alpha
shapes, weighted Voronoi diagrams, Voronoi volumes, or constrained Delaunay
triangulations.")
    (home-page "http://qhull.org")
    (license (license:non-copyleft "file://COPYING.txt"
                                   "See COPYING in the distribution."))))

(define-public units
  (package
   (name "units")
   (version "2.24")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/units/units-" version
                                ".tar.gz"))
            (sha256 (base32
                     "00n9l329814nww1pnp9nlaas8lfvblpcf5j750lhpwmcvx72ql0y"))))
   (build-system gnu-build-system)
   (inputs
     (list bash-minimal       ;for wrap-program
           readline
           python-wrapper   ;for 'units_cur' script
           python-requests))
   (arguments
    `(#:phases (modify-phases %standard-phases
                 (add-after 'install 'wrap-units_cur
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let* ((out (assoc-ref outputs "out"))
                            (bin (string-append out "/bin")))
                       (wrap-program (string-append bin "/units_cur")
                         `("GUIX_PYTHONPATH" ":" prefix
                           ,(search-path-as-string->list
                             (getenv "GUIX_PYTHONPATH"))))))))))
   (synopsis "Conversion between thousands of scales")
   (description
    "GNU Units converts numeric quantities between units of measure.  It
can handle scale changes through adaptive usage of standard scale
prefixes (micro-, kilo-, etc.).  It can also handle nonlinear
conversions such as Fahrenheit to Celsius.  Its interpreter is powerful
enough to be used effectively as a scientific calculator.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/units/")))

(define-public double-conversion
  (package
    (name "double-conversion")
    (version "3.1.5")
    (home-page "https://github.com/google/double-conversion")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0csy4pjw1p8rp6g5qxi2h0ychhhp1fldv7gb761627fs2mclw9gv"))))
    (build-system cmake-build-system)
    (arguments
     '(#:test-target "test"
       #:configure-flags '("-DBUILD_SHARED_LIBS=ON"
                           "-DBUILD_TESTING=ON")))
    (synopsis "Conversion routines for IEEE doubles")
    (description
     "The double-conversion library provides binary-decimal and decimal-binary
routines for IEEE doubles.  The library consists of efficient conversion
routines that have been extracted from the V8 JavaScript engine.")
    (license license:bsd-3)))

(define-public dionysus
  (package
    (name "dionysus")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/dionysus/dionysus-" version
                                  ".tar.xz"))
              (sha256
               (base32
                "194pzs1mlsj4ww6v37qq3961h5hckm5h805cv0r14xj3g9wfx2sk"))))
    (build-system gnu-build-system)
    (inputs (list tcl))                      ;for 'tclsh'
    (synopsis "Local search for universal constants and scientific values")
    (description
     "GNU Dionysus is a convenient system for quickly retrieving the values of
mathematical constants used in science and engineering.  Values can be
searched using a simple command-line tool, choosing from three databases:
universal constants, atomic numbers, and constants related to
semiconductors.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/dionysus/")))

(define-public dozenal
  ;; There is no recent release, so use the latest commit.
  (let ((revision "1")
        (commit "328bc03ad544179f2cccda36763358c4216f188e"))
    (package
      (name "dozenal")
      (version (git-version "12010904-3" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://codeberg.org/dgoodmaniii/dozenal")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0knwfwjqdv854l5ny7csdpvp7r0md6a2k43a1l2lkyw9k3cglpph"))))
      (build-system gnu-build-system)
      (arguments
       (list
        ;; Some test scripts are included, but no makefile-driven
        ;; tests, and they are all quite manual to run and check.
        #:tests? #f
        ;; Running with `make -j' causes the build to fail.  This is likely
        ;; because this project uses the "recursive make" structure, where
        ;; each subdirectory contains its own make file, which is called by
        ;; the top-level makefile.
        #:parallel-build? #f
        #:make-flags
        #~(list (string-append "prefix=" #$output))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'chdir
              (lambda _
                (chdir "dozenal")))
            (add-after 'chdir 'patch-lua-references
              (lambda _
                (let ((lua-name (strip-store-file-name
                                 #$(this-package-input "lua"))))
                  (substitute* '("dozcal/Makefile"
                                 "dozlua/Makefile")
                    (("lua52")
                     (string-take lua-name
                                  (string-rindex lua-name #\.)))))))
            (delete 'configure)
            (add-before 'install 'make-bin-dir
              (lambda _
                (mkdir-p (string-append #$output "/bin"))))
            (add-after 'install 'install-html-docs
              (lambda _
                (invoke "make"
                        (string-append "prefix=" #$output)
                        "installhtml")))
            (add-after 'install-html-docs 'split-outputs
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (for-each
                 (lambda (prog)
                   (let ((orig (string-append #$output "/bin/" prog))
                         (dst (string-append #$output:gui "/bin/" prog))
                         (man-orig (string-append #$output
                                                  "/share/man/man1/"
                                                  prog ".1"))
                         (man-dst (string-append #$output:gui
                                                 "/share/man/man1/"
                                                 prog ".1")))
                     (mkdir-p (dirname dst))
                     (rename-file orig dst)
                     (mkdir-p (dirname man-dst))
                     (rename-file man-orig man-dst)))
                 '("xdozdc" "gdozdc"))
                (wrap-program (string-append #$output:gui "/bin/" "gdozdc")
                  `("PATH" = (,(string-append #$output "/bin")))
                  `("PERL5LIB" = (,(getenv "PERL5LIB")))))))))
      (outputs '("out" "gui"))
      (native-inputs (list groff pkg-config))
      (inputs (list bash-minimal        ;for wrap-program
                    libhdate
                    lua
                    ncurses
                    perl
                    perl-tk
                    perl-par
                    xforms))
      (synopsis "Suite of dozenal programs")
      (description
       "The dozenal suite is a set of programs designed to assist with working
in the dozenal (also called \"duodecimal\" or \"base twelve\") system.  It
includes number converters (dozenal-to-decimal and decimal-to-dozenal), an RPN
calculator, a graphical calculator, a metric system converter (works with
imperial, U.S. customary, SI metric, and the dozenal TGM), a pretty-printer
for dozenal numbers, a date-and-time program, and a dozenal calendar programs,
complete with events and to-dos.")
      (home-page "https://codeberg.org/dgoodmaniii/dozenal")
      (license license:gpl3+))))

(define-public dsfmt
  (package
    (name "dsfmt")
    (version "2.2.3")
    (source
     (origin
       (method url-fetch)
       (uri
         (string-append
           "http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/"
           "dSFMT-src-" version ".tar.gz"))
       (sha256
        (base32
         "03kaqbjbi6viz0n33dk5jlf6ayxqlsq4804n7kwkndiga9s4hd42"))
       (modules '((guix build utils)))
       ;; Don't distribute html documentation with bundled jquery.
       (snippet
        '(begin
           (delete-file-recursively "html") #t))
       ;; Add patches borrowed from Julia.
       (patches
         (list
           (origin
             (method url-fetch)
             (uri (string-append
                    "https://raw.githubusercontent.com/JuliaLang/julia/"
                    "v1.3.0/deps/patches/dSFMT.c.patch"))
             (sha256 (base32
                      "09mhv11bms8jsmkmdqvlcgljwhzw3b6n9nncpi2b6dla9798hw2y"))
             (file-name "dSFMT.c.patch"))
           (origin
             (method url-fetch)
             (uri (string-append
                    "https://raw.githubusercontent.com/JuliaLang/julia/"
                    "v1.3.0/deps/patches/dSFMT.h.patch"))
             (sha256 (base32
                      "1py5rd0yxic335lzka23f6x2dhncrpizpyrk57gi2f28c0p98y5n"))
             (file-name "dSFMT.h.patch"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           ;; Upstream Makefile does not build a shared library. Borrow from Julia
           ;; https://github.com/JuliaLang/julia/blob/v1.3.0/deps/dsfmt.mk
           (lambda _
             (invoke
               "gcc" "-DNDEBUG" "-DDSFMT_MEXP=19937"
               "-fPIC" "-DDSFMT_DO_NOT_USE_OLD_NAMES"
               "-O3" "-finline-functions" "-fomit-frame-pointer"
               "-fno-strict-aliasing" "--param" "max-inline-insns-single=1800"
               "-Wmissing-prototypes" "-Wall" "-std=c99" "-shared" "dSFMT.c"
               "-o" "libdSFMT.so")))
         (replace 'install              ; no "install" target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include"))
                    (doc (string-append out "/share/doc/" ,name "-" ,version)))
               (install-file "libdSFMT.so" lib)
               (install-file "dSFMT.h" inc)
               (install-file "LICENSE.txt" doc)
               #t))))))
    (synopsis "Double precision SIMD-oriented Fast Mersenne Twister")
    (description
     "The dSMFT package speeds up Fast Mersenne Twister generation by avoiding
the expensive conversion of integer to double (floating point).  dSFMT directly
generates double precision floating point pseudorandom numbers which have the
IEEE Standard for Binary Floating-Point Arithmetic (ANSI/IEEE Std 754-1985)
format.  dSFMT is only available on the CPUs which use IEEE 754 format double
precision floating point numbers.")
    (home-page "http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/")
    (license license:bsd-3)))

(define-public gsl
  (package
    (name "gsl")
    (version "2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/gsl/gsl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "141i8ag2qazyqqk17bfr2l7mr1asxm1da7avi5a66b332pnyx6ba"))))
    (outputs '("out" "static" "debug"))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               #$@(cond
                   ((and (target-riscv64?)
                         (%current-target-system))
                    #~((add-after 'unpack 'force-bootstrap
                         (lambda _
                           ;; gsl ships with an old configure script that does not
                           ;; support riscv64. Regenerate it.
                           (delete-file "configure")))))

                   ((or (string-prefix? "aarch64" (%current-system))
                        (string-prefix? "powerpc" (%current-system)))
                    ;; Some sparse matrix tests are failing on AArch64 and PowerPC:
                    ;; https://lists.gnu.org/archive/html/bug-gsl/2020-04/msg00001.html
                    #~((add-before 'check 'disable-failing-tests
                         (lambda _
                           (substitute* "spmatrix/test.c"
                             ((".*test_complex.*")
                              "\n"))))))

                   ((string-prefix? "i686" (%current-system))
                    ;; There are rounding issues with these tests on i686:
                    ;; https://lists.gnu.org/archive/html/bug-gsl/2016-10/msg00000.html
                    ;; https://lists.gnu.org/archive/html/bug-gsl/2020-04/msg00000.html
                    #~((add-before 'check 'disable-failing-tests
                         (lambda _
                           (substitute* "spmatrix/test.c"
                             ((".*test_all.*") "\n")
                             ((".*test_float.*") "\n")
                             ((".*test_complex.*") "\n"))

                           ;; XXX: These tests abort with:
                           ;; gsl: cholesky.c:645: ERROR: matrix is not positive definite
                           (substitute* '("multifit_nlinear/test.c"
                                          "multilarge_nlinear/test.c")
                             (("gsl_ieee_env_setup.*" all)
                              (string-append "exit (77);\n" all)))))))

                   (else #~()))
               (add-after 'install 'move-static-library
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((static (string-append (assoc-ref outputs
                                                           "static")
                                                "/lib/"))
                         (out (string-append (assoc-ref outputs "out")
                                             "/lib/")))
                     (mkdir-p static)
                     (rename-file (string-append out "libgsl.a")
                                  (string-append static "libgsl.a"))
                     (rename-file (string-append out "libgslcblas.a")
                                  (string-append static "libgslcblas.a"))))))))
    (native-inputs
     (if (and (target-riscv64?)
              (%current-target-system))
         (list autoconf automake libtool)
         '()))
    (home-page "https://www.gnu.org/software/gsl/")
    (synopsis "Numerical library for C and C++")
    (description
     "The GNU Scientific Library is a library for numerical analysis in C
and C++.  It includes a wide range of mathematical routines, with over 1000
functions in total.  Subject areas covered by the library include:
differential equations, linear algebra, Fast Fourier Transforms and random
numbers.")

    ;; Linear algebra routines should benefit from SIMD optimizations.
    (properties `((tunable? . #t)))

    (license license:gpl3+)))

(define-public sleef
  (package
    (name "sleef")
    (version "3.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shibatch/sleef")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14y1zf621zp0333vs29pc7pcc31gsrrs3q49b6qmd1kz6c7a7fp2"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:configure-flags
      #~(list "-DSLEEF_BUILD_SHARED_LIBS=ON")))
    (inputs
     (list fftw gmp mpfr openssl))
    (home-page "https://sleef.org/")
    (synopsis "SIMD library for evaluating elementary functions and DFT")
    (description
     "SLEEF (SIMD Library for Evaluating Elementary Functions) is a library that
implements vectorized versions of all C99 real floating point math functions.
It can utilize SIMD instructions that are available on modern processors.")
    (license (list license:boost1.0       ;sleef
                   license:cc-by4.0))))   ;simplex algorithm

(define-public glpk
  (package
    (name "glpk")
    (version "5.0")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "05bgxidxj8d9xdp82niy7cy36w181cxq7p8vc3y2ixshpgp1642a"))))
    (build-system gnu-build-system)
    (inputs
     (list gmp))
    (arguments
     `(#:configure-flags '("--with-gmp"
                           "--disable-static")))
    (home-page "https://www.gnu.org/software/glpk/")
    (synopsis "GNU Linear Programming Kit, supporting the MathProg language")
    (description
     "GLPK is a C library for solving large-scale linear programming (LP),
mixed integer programming (MIP), and other related problems.  It supports the
GNU MathProg modeling language, a subset of the AMPL language, and features a
translator for the language.  In addition to the C library, a stand-alone
LP/MIP solver is included in the package.")
    (license license:gpl3+)))

(define-public glpk-4
  (package
    (inherit glpk)
    (name "glpk")
    (version "4.65")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/glpk/glpk-"
                          version ".tar.gz"))
      (sha256
       (base32
        "040sfaa9jclg2nqdh83w71sv9rc1sznpnfiripjdyr48cady50a2"))))))

(define-public linasm
  (package
    (name "linasm")
    (version "1.13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.code.sf.net/p/linasm/linasm")
                    (commit (string-append "v" version "(stable)"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11095bxjxsq3a9apvyi1kpddwkg9b2hc5ga65qhrdxzvdsrjhaaq"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #false                   ;there are none
      #:make-flags
      #~(list (string-append "prefix=" #$output))
      #:phases
      '(modify-phases %standard-phases
         (delete 'configure))))
    (supported-systems '("x86_64-linux"))
    (home-page "https://sourceforge.net/projects/linasm/")
    (synopsis "Collection of fast and optimized assembly libraries for x86-64")
    (description
     "LinAsm is collection of very fast and SIMD optimized assembly written
libraries for x86-64.

It implements many common and widely used algorithms for array manipulations:
searching, sorting, arithmetic and vector operations, unit conversions; fast
mathematical and statistic functions; numbers and time converting algorithms;
@dfn{finite impulse response} (FIR) digital filters; spectrum analysis
algorithms, Fast Hartley transformation; CPU cache friendly functions and
extremely fast @dfn{abstract data types} (ADT) such as hash tables b-trees,
and much more.")
    (license license:lgpl3+)))

(define-public gfan
  (package
    (name "gfan")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://users-math.au.dk/jensen/software"
                           "/gfan/gfan" version ".tar.gz"))
       (sha256
        (base32 "17lqripnsdb5hn7nnhgn4siajgh1jh9nkaplca3akm74w5bkg0xb"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:make-flags #~(list (string-append "PREFIX=" #$output)
                           (string-append "CC=" #$(cc-for-target))
                           (string-append "CXX=" #$(cxx-for-target)))
      #:phases #~(modify-phases %standard-phases
                   (delete 'configure)
                   ;; cddlib is distributed with the 'cddlib' header name,
                   ;; but gfan expects it to be named 'cdd'.  Substitute
                   ;; the include headers to make gfan find it.
                   (add-after 'unpack 'fix-cdd-reference
                     (lambda _
                       (substitute* '("src/lp_cdd.cpp"
                                      "src/gfanlib_zcone.cpp"
                                      "src/app_librarytest.cpp")
                         (("#include \"cdd") "#include \"cddlib")))))))
    (inputs (list cddlib gmp))
    (home-page "https://users-math.au.dk/jensen/software/gfan/gfan.html")
    (synopsis "Compute Gröbner fans and tropical varieties")
    (description "Gfan is a software package for computing Gröbner fans and
tropical varieties.")
    ;; homepage/gfan.html: "Gfan is distributed under the terms of the GPL
    ;; license version 2 or 3 as desired"
    (license license:gpl2)))

(define-public 4ti2
  (package
    (name "4ti2")
    (version "1.6.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/4ti2/4ti2/releases/download/"
                           "Release_"
                           (string-map (lambda (c) (if (char=? c #\.) #\_ c))
                                       version)
                           "/4ti2-" version ".tar.gz"))
       (sha256
        (base32 "0sx8n4acmqx086a5cfkdkqxnjrlr7nsihnzxwi1vcij2n6z93hgp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list (@ (gnu packages base) which))) ; for the tests
    (inputs
     (list glpk gmp))
    (home-page "https://4ti2.github.io")
    (synopsis "Mathematical tool suite for problems on linear spaces")
    (description
     "4ti2 implements algorithms for solving algebraic, geometric and
combinatorial problems on linear spaces.  Among others, it solves systems
of linear equations, computes extreme rays of polyhedral cones, solves
integer programming problems and computes Markov bases for statistics.")
    (license license:gpl2+)))

(define-public sympow
  (package
    (name "sympow")
    (version "2.023.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
         (url "https://gitlab.com/rezozer/forks/sympow")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ilnxygkj4g5arjiyd16k00cvnjlqs0cpc8hk64kbqhl877mm5i9"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ; no tests
           #:phases #~(modify-phases %standard-phases
                        (replace 'configure
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "Configure"
                              (("/bin/sh") (search-input-file inputs "/bin/bash")))
                            (setenv "PREFIX" #$output)
                            (setenv "VARPREFIX" #$output)
                            (invoke "bash" "./Configure"))))))
    (native-inputs (list bash-minimal coreutils help2man pari-gp which))
    (home-page "https://gitlab.com/rezozer/forks/sympow")
    (synopsis "Symmetric power elliptic curve L-functions")
    (description "SYMPOW is a mathematical program to compute special values
of symmetric power elliptic curve L-functions; it can compute up to about 64
digits of precision.")
    ;; bsd-2 with extra stipulation that users be informed of sympow's
    ;; "less restrictive license" if it's included in a program with a more
    ;; restrictive license.  However, since sympow includes fpu.c which is
    ;; gpl2+ the whole package can only be distributed via GPL anyway.
    ;; See also <https://gitlab.com/rezozer/forks/sympow/-/issues/7>.
    (license (license:non-copyleft "file:///COPYING"
                                   "See COPYING in the distribution."))))

(define-public cddlib
  (package
    (name "cddlib")
    (version "0.94m")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cddlib/cddlib")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "09s8323h5w9j6mpl1yc6lm770dkskfxd2ayyafkcjllmnncxzfa0"))))
    (build-system gnu-build-system)
    (inputs
     (list gmp))
    (native-inputs
     (list autoconf
           automake
           libtool
           (texlive-updmap.cfg
            (list texlive-amsfonts
                  texlive-graphics
                  texlive-l3backend
                  texlive-l3backend))))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--docdir=" #$output
                                  "/share/doc/" #$name "-" #$version))))
    (home-page "https://www.inf.ethz.ch/personal/fukudak/cdd_home/index.html")
    (synopsis "Library for convex hulls and extreme rays of polyhedra")
    (description
     "The C-library cddlib implements the Double Description Method of
Motzkin et al. for generating all vertices (i.e. extreme points) and extreme
rays of a general convex polyhedron given by a system of linear inequalities
in arbitrary dimension.  It can also be used for the converse operation of
computing convex hulls.")
    (license license:gpl2+)))

(define-public lrslib
  (package
    (name "lrslib")
    (version "7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://cgm.cs.mcgill.ca/~avis/C/lrslib/archive/"
                           "lrslib-0"
                           (string-delete #\. version) ".tar.gz"))
       (sha256
        (base32
         "1w1jsnfgny8cihndr5gfm99pvwp48qsvxkqfsi2q87gd3m57aj7w"))))
    (build-system gnu-build-system)
    (inputs
     (list gmp))
    (arguments
     `(#:tests? #f                      ; no check phase
       #:make-flags `("CC=gcc"
                      ,(string-append "prefix=" (assoc-ref %outputs "out"))
                      "all-shared")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (substitute* "makefile"
               (("-L \\.") "-L . -Wl,-rpath='$$ORIGIN/../lib'"))
             #t)))))
    (home-page "http://cgm.cs.mcgill.ca/~avis/C/lrs.html")
    (synopsis "Convex hulls of polyhedra with exact arithmetic")
    (description
     "The C code of lrslib implements the reverse search algorithm for
vertex enumeration and convex hull problems.  Its input file format is
compatible with cddlib.  All computations are done exactly in either
multiple precision or fixed integer arithmetic.  Output is not stored
in memory, so even problems with very large output sizes can sometimes
be solved.")
    (license license:gpl2+)))

(define-public libcerf
  (package
    (name "libcerf")
    (version "1.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://jugit.fz-juelich.de/mlz/libcerf")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ic2q7kvxpqmgxlishygvx8d00i4wn51vkq4fyac44ahhf6c3kwd"))))
    (build-system cmake-build-system)
    (native-inputs
     (list perl))
    (home-page "https://jugit.fz-juelich.de/mlz/libcerf")
    (synopsis "Library for complex error functions")
    (description
     "@code{libcerf} is a self-contained numeric library that provides an
efficient and accurate implementation of complex error functions, along with
Dawson, Faddeeva, and Voigt functions.")
    (license license:expat)))

(define-public vinci
  (package
    (name "vinci")
    (version "1.0.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.math.u-bordeaux.fr/~aenge/software/"
                           "vinci/vinci-" version ".tar.gz"))
       (sha256
        (base32
         "1aq0qc1y27iw9grhgnyji3290wwfznsrk3sg6ynqpxwjdda53h4m"))))
    (build-system gnu-build-system)
    (inputs
     (list lrslib))
    (arguments
     `(#:tests? #f                      ; no check phase
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           ;; register the lrs location in the config file
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((lrs (assoc-ref inputs "lrslib"))
                    (lrsexec (string-append lrs "/bin/lrs")))
               (substitute* "vinci.h"
                 (("#define LRS_EXEC      \"lrs\"")
                  (string-append "#define LRS_EXEC \"" lrsexec "\""))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (install-file "vinci" bin))
             #t)))))
    (home-page
     "https://www.math.u-bordeaux.fr/~aenge/?category=software&page=vinci")
    (synopsis "Volume computation for polytopes")
    (description
     "Vinci implements a number of volume computation algorithms for convex
polytopes in arbitrary dimension.  The polytopes can be given by their
V-representation (as the convex hull of a finite number of vertices), by
their H-representation (as the bounded intersection of a finite number of
halfspaces) or by their double description with both representations.")
    (license license:gpl2+)))

(define-public arpack-ng
  (package
    (name "arpack-ng")
    (version "3.9.1")
    (home-page "https://github.com/opencollab/arpack-ng")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bbw6a48py9fjlif2n4x75skyjskq2hghffjqzm85wnsnsjdlaqw"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DICB=ON")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list eigen openblas gfortran))
    (synopsis "Fortran subroutines for solving eigenvalue problems")
    (description
     "ARPACK-NG is a collection of Fortran77 subroutines designed to solve
large scale eigenvalue problems.")
    (license (license:non-copyleft "file://COPYING"
                                "See COPYING in the distribution."))))

(define-public arpack-ng-openmpi
  (package (inherit arpack-ng)
    (name "arpack-ng-openmpi")
    (inputs
     (modify-inputs (package-inputs arpack-ng)
       (prepend openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments arpack-ng)
       ((#:configure-flags _ #~())
        #~'("-DMPI=ON"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'mpi-setup
              #$%openmpi-setup)))))
    (synopsis "Fortran subroutines for solving eigenvalue problems with MPI")))

(define-public lapack
  (package
    (name "lapack")
    (version "3.12.1")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
            (url "https://github.com/Reference-LAPACK/lapack/")
            (commit (string-append "v" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "19387ifv5kplnggw5fgz4nzspmqi11wnwzap2knwxgrvknysrwj9"))))
    (build-system cmake-build-system)
    (home-page "https://www.netlib.org/lapack/")
    (inputs (list gfortran python-wrapper))
    (arguments
     `(#:configure-flags (list
                          "-DBUILD_SHARED_LIBS:BOOL=YES"
                          "-DCBLAS=ON"
                          "-DLAPACKE=ON"
                          ;; Build the 'LAPACKE_clatms' functions.
                          "-DLAPACKE_WITH_TMG=ON"
                          "-DBUILD_TESTING=ON")))
    (synopsis "Library for numerical linear algebra")
    (description
     "LAPACK is a Fortran 90 library for solving the most commonly occurring
problems in numerical linear algebra.")
    (license (license:non-copyleft "file://LICENSE"
                                "See LICENSE in the distribution."))
    (properties '((tunable? . #t)))))

(define-public lis
  (package
   (name "lis")
   (version "2.1.8")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://www.ssisc.org/lis/dl/lis-"
                                version ".zip"))
            (sha256
             (base32 "0nh2593xkcdv1c3gmj7i64ca393nn0ngqfl522yiwbidh9dvd1nl"))))
   (build-system gnu-build-system)
   (arguments
    (list
     #:configure-flags #~(list "--enable-fortran"
                               "--enable-f90"
                               "--enable-openmp"
                               "--enable-complex"
                               "--disable-sse2" ;; XXX: tuning
                               "--enable-shared")
     #:phases
     #~(modify-phases %standard-phases
         (add-after 'install 'install-doc
           (lambda _
             (let* ((share (string-append #$output "/share"))
                    (docdir (string-append share "/doc/lis-" #$version))
                    (mandir (string-append share "/man")))
               (copy-recursively "doc/man" mandir)
               ;; TODO: Build the manuals ourselves
               (install-file "doc/lis-ug-en.pdf" docdir)
               (install-file "doc/lis-ug-ja.pdf" docdir)))))))
   (inputs (list openmpi))
   (native-inputs (list gfortran unzip))
   (home-page "https://www.ssisc.org/lis")
   (synopsis "Solve discretized linear equations and eigenvalue problems")
   (description "Lis is a parallel software library for solving discretized
linear equations and eigenvalue problems that arise in the numerical solution
of partial differential equations using iterative methods.")
   (license license:bsd-3)))

(define-public clapack
  (package
    (name "clapack")
    (version "3.2.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "http://www.netlib.org/clapack/clapack-"
                          version "-CMAKE.tgz"))
      (sha256
       (base32
        "0nnap9q1mv14g57dl3vkvxrdr10k5w7zzyxs6rgxhia8q8mphgqb"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DCMAKE_C_FLAGS=-fcommon -O2")
       #:phases
       (modify-phases %standard-phases
         ;; These tests use a lot of stack variables and segfault without
         ;; lifting resource limits.
         (add-after 'unpack 'disable-broken-tests
           (lambda _
             (substitute* "TESTING/CMakeLists.txt"
               (("add_lapack_test.* xeigtstz\\)") ""))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib"))
                    (f2cinc (string-append out "/include/libf2c")))
               (mkdir-p f2cinc)
               (display (getcwd))
               (for-each (lambda (file)
                           (install-file file libdir))
                         '("SRC/liblapack.a"
                           "F2CLIBS/libf2c/libf2c.a"
                           "TESTING/MATGEN/libtmglib.a"
                           "BLAS/SRC/libblas.a"))
               (for-each (lambda (file)
                           (install-file file f2cinc))
                         (cons "F2CLIBS/libf2c/arith.h"
                               (find-files (string-append "../clapack-"
                                                          ,version "-CMAKE/F2CLIBS/libf2c")
                                           "\\.h$")))
               (copy-recursively (string-append "../clapack-"
                                                ,version "-CMAKE/INCLUDE")
                                 (string-append out "/include"))))))))
    (home-page "https://www.netlib.org/clapack/")
    (synopsis "Numerical linear algebra library for C")
    (description
     "The CLAPACK library was built using a Fortran to C conversion utility
called f2c.  The entire Fortran 77 LAPACK library is run through f2c to obtain
C code, and then modified to improve readability.  CLAPACK's goal is to
provide LAPACK for someone who does not have access to a Fortran compiler.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public scalapack
  (package
    (name "scalapack")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://www.netlib.org/scalapack/scalapack-"
                           version ".tgz"))
       (sha256
        (base32
         "19i0h9vdc3zsy58r6fy1vs2kz2l7amifkz0cf926j90xz1n23nb1"))
       (patches (search-patches "scalapack-gcc-10-compilation.patch"))))
    (build-system cmake-build-system)
    (inputs
     `(("mpi" ,openmpi)
       ("fortran" ,gfortran)
       ("lapack" ,openblas)))             ;for testing only
    (arguments
     `(#:configure-flags `("-DBUILD_SHARED_LIBS:BOOL=YES")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'mpi-setup
		    ,%openmpi-setup)
                  (add-after 'unpack 'skip-faulty-test
                    (lambda _
                      ;; FIXME: Skip these two tests that fail to complete for
                      ;; unknown reasons:
                      ;; <https://github.com/Reference-ScaLAPACK/scalapack/issues/43>.
                      (substitute* "TESTING/CMakeLists.txt"
                        (("^add_test\\(x[sd]hseqr.*" all)
                         (string-append "# " all "\n"))))))))
    (home-page "https://www.netlib.org/scalapack/")
    (synopsis "Library for scalable numerical linear algebra")
    (description
     "ScaLAPACK is a Fortran 90 library of high-performance linear algebra
routines on parallel distributed memory machines.  ScaLAPACK solves dense and
banded linear systems, least squares problems, eigenvalue problems, and
singular value problems.")
    (license (license:non-copyleft "file://LICENSE"
                                   "See LICENSE in the distribution."))))

(define-public feedgnuplot
  (package
    (name "feedgnuplot")
    (version "1.60")
    (home-page "https://github.com/dkogan/feedgnuplot")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0403hwlian2s431m36qdzcczhvfjvh7128m64hmmwbbrgh0n7md7"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'adjust-tests
           (lambda _
             (substitute* "t/plots.t"
               ;; XXX: The vnlog tests uses 'echo' with escaped strings,
               ;; but does not enable escape interpretation.
               (("echo -n ")
                "echo -ne "))))
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/feedgnuplot")))
               (mkdir-p doc)
               (invoke "pod2html" "--title=feedgnuplot" "bin/feedgnuplot"
                       "--outfile" (string-append doc "/feedgnuplot.html")))))
       (add-after 'install 'wrap
         (lambda* (#:key inputs outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (gnuplot (search-input-file inputs "/bin/gnuplot"))
                  ;; XXX: We need List::MoreUtils as well as its supporting
                  ;; (propagated) modules; for now just refer to labels.
                  (modules '("perl-list-moreutils" "perl-exporter-tiny"))
                  (PERL5LIB (string-join
                             (map (lambda (input)
                                    (string-append (assoc-ref inputs input)
                                                   "/lib/perl5/site_perl"))
                                  modules)
                             ":")))
             (wrap-program (string-append out "/bin/feedgnuplot")
               `("PERL5LIB" ":" suffix (,PERL5LIB))
               `("PATH" ":" suffix (,(dirname gnuplot))))))))))
    (inputs
     (list bash-minimal gnuplot perl-list-moreutils vnlog))
    (native-inputs
     ;; For tests.
     (list perl-ipc-run perl-string-shellquote))
    (synopsis "Pipe-oriented plotting tool")
    (description
     "@command{feedgnuplot} is a tool to plot realtime and stored data
from the command line, using @command{gnuplot}.  It can read data from
a pipe or file, make a variety of transformations, and render the result
in the terminal or with an external viewer.")
    (license license:gpl1+))) ;any version

(define-public giza
  (package
    (name "giza")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/danieljprice/giza")
             (commit (string-append "v" version))))
       (sha256
        (base32 "17h8hkhcqlvgryyp5n206fbqpals2vbnjy4f6f1zwj9jiblgi5mj"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl pkg-config))
    (inputs
     (list cairo freetype gfortran))
    (home-page "https://danieljprice.github.io/giza/")
    (synopsis "Scientific plotting library for C/Fortran")
    (description
     "Giza is a lightweight scientific plotting library built on top of
@code{cairo} that provides uniform output to multiple devices.")
    (license license:gpl2+)))

(define-public perl-pgplot
  (package
    (name "perl-pgplot")
    (version "2.34")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://cpan/authors/id/E/ET/ETJ/PGPLOT-" version
                           ".tar.gz"))
       (sha256
        (base32 "1j0hjnhi0rkihviab2s6ninwfm71s73zh89pds1mpg9kf3c1w97z"))))
    (build-system perl-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'unpack 'setenv
         (lambda* (#:key inputs #:allow-other-keys)
           (setenv "PGPLOT_DIR" (string-append (assoc-ref inputs "giza") "/lib")))))))
    (inputs (list giza libx11))
    (native-inputs (list perl-devel-checklib perl-extutils-f77 gfortran))
    (home-page "https://metacpan.org/release/PGPLOT")
    (synopsis "Scientific plotting library (using giza)")
    (description "This package provides PGPLOT bindings for Perl.  It uses
giza instead of PGPLOT for the implementation, though.")
    ;; Since giza is GPL2+, so is this.
    (license license:gpl2+)))

(define-public gnuplot
  (package
    (name "gnuplot")
    (version "6.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/gnuplot/gnuplot/"
                                  version "/gnuplot-"
                                  version ".tar.gz"))
       (sha256
        (base32 "16ipf7m2c2v1cldp3kwxbjj6db6bzy0rkrpp4kzhh61a3866cnp8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config (texlive-updmap.cfg)))
    (inputs
     (list cairo gd libcerf lua pango readline))
    (arguments
     (list #:configure-flags
           #~(list "--with-qt=no"
                   (string-append "--with-texdir=" #$output
                                  "/texmf-local/tex/latex/gnuplot"))
           ;; Plot on a dumb terminal during tests.
           #:make-flags #~'("GNUTERM=dumb")))
    (home-page "http://www.gnuplot.info")
    (synopsis "Command-line driven graphing utility")
    (description "Gnuplot is a portable command-line driven graphing
utility.  It was originally created to allow scientists and students to
visualize mathematical functions and data interactively, but has grown to
support many non-interactive uses such as web scripting.  It is also used as a
plotting engine by third-party applications like Octave.")
    ;;  X11 Style with the additional restriction that derived works may only be
    ;;  distributed as patches to the original.
    (license (license:fsf-free
              "http://gnuplot.cvs.sourceforge.net/gnuplot/gnuplot/Copyright"))))

(define-public hmat
  (package
    (name "hmat")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/jeromerobert/hmat-oss")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0ssjzf3sdhn80w03bhp694s413222cl0100bf36mx70q3a1b6vi5"))))
    (build-system cmake-build-system)
    (arguments
     ;; Examples are the tests.
     (list #:configure-flags #~(list "-DBUILD_EXAMPLES=ON")))
    (inputs (list openblas))
    (home-page "https://github.com/jeromerobert/hmat-oss")
    (synopsis "Hierarchical matrix library")
    (description "@code{hmat-oss} is hierarchical matrix library written in
C++ with a C API.  It contains a LU and LLt solver, and a few other things.")
    (license license:gpl2+)))

(define-public cminpack
  (package
    (name "cminpack")
    (version "1.3.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/devernay/cminpack")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05cjb54in7kks70rrnmvczwkg4nsxhwyf23abxqdj143zwbz4yyr"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_SHARED_LIBS=ON")))
    (home-page "https://github.com/devernay/cminpack")
    (synopsis "C/C++ rewrite of the MINPACK software")
    (description
     "This is a C version of the minpack minimization package.  It has been
derived from the fortran code using f2c and some limited manual editing.
Extern C linkage permits the package routines to be called from C++.")
    (license (license:non-copyleft ; original minpack license
              "https://github.com/certik/minpack/blob/master/LICENSE"))))

(define-public bonmin
  (package
    (name "bonmin")
    (version "1.8.9")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coin-or/Bonmin")
                    (commit (string-append "releases/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "153kj4wx386609g21hw3cv5yxps62qqrc64zwb9ryd2xad1w1a4y"))))
    (build-system gnu-build-system)
    (native-inputs (list gfortran pkg-config))
    (inputs (list cbc ipopt openblas))
    (home-page "https://coin-or.github.io/Bonmin/")
    (synopsis "Basic Open-source Nonlinear Mixed INteger programming")
    (description "Bonmin is a code for solving general MINLP (Mixed Integer
NonLinear Programming) problems.  It builds on top of Cbc and Ipopt.")
    (license license:epl1.0)))

(define-public pagmo
  (package
    (name "pagmo")
    (version "2.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/esa/pagmo2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0g0j0k0cwp8kyyggj80s5cd24bl6gqmf6f5g7j2axswr2bdj16fg"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DPAGMO_BUILD_TESTS=ON"
                                     "-DPAGMO_WITH_EIGEN3=ON")))
    ;; Eigen is optional, enables some extra features.
    (inputs (list boost eigen tbb))
    (home-page "https://esa.github.io/pagmo2/")
    (synopsis
     "Platform to perform parallel computations of optimisation tasks")
    (description "@code{pagmo} is a C++ scientific library for massively
parallel optimization.  It is built around the idea of providing a unified
interface to optimization algorithms and to optimization problems and to make
their deployment in massively parallel environments easy.")
    ;; Pagmo only supports 64-bit x86, ARM and PowerPC:
    ;; https://esa.github.io/pagmo2/install.html
    (supported-systems '("x86_64-linux" "aarch64-linux" "armhf-linux"
                         "powerpc64le-linux"))
    ;; Dual licensed, user choice.
    (license (list license:lgpl3+ license:gpl3+))))

(define-public gctp
  (package
    (name "gctp")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OkoSanto/GCTP")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11wqmd443b4nksdbzp1msdws3av948nmwq1xz80w6hka3ss2aigd"))))
    (native-inputs
     (list gfortran))
    (build-system gnu-build-system)
    (synopsis "General Cartographic Transformation Package (GCTP)")
    (description
     "The General Cartographic Transformation Package (GCTP) is a system of
software routines designed to permit the transformation of coordinate pairs
from one map projection to another.  The GCTP is the standard computer
software used by the National Mapping Division for map projection
computations.")
    (home-page "https://github.com/OkoSanto/GCTP")
    (license license:public-domain))) ;https://www2.usgs.gov/laws/info_policies.html

(define-public hdf4
  (package
    (name "hdf4")
    (version "4.2.16-2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://support.hdfgroup.org/ftp/HDF/releases/HDF"
                           version "/src/hdf-" version ".tar.bz2"))
       (sha256
        (base32 "0b395czhqr43mmbiifmg2mhb488wnd4zccj45vpql98ja15j7hy5"))
       (patches (search-patches "hdf4-reproducibility.patch"
                                "hdf4-shared-fortran.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gfortran bison flex))
    (inputs
     `(("zlib" ,zlib)
       ("libjpeg" ,libjpeg-turbo)
       ("libtirpc" ,libtirpc)))
    (arguments
     `(#:parallel-tests? #f
       #:configure-flags (list "--enable-shared"
                               "FCFLAGS=-fallow-argument-mismatch"
                               "FFLAGS=-fallow-argument-mismatch"
                               "--enable-hdf4-xdr")
       #:phases
       (modify-phases %standard-phases
         ;; This is inspired by two of Debian's patches.
         (add-before 'configure 'add-more-architecture-support
           (lambda _
             (substitute* '("mfhdf/ncgen/ncgen.l"
                            "mfhdf/ncgen/ncgenyy.c"
                            "mfhdf/libsrc/netcdf.h.in")
               (("AIX5L64") "__aarch64__ || ( __riscv && __riscv_xlen == 64)"))))
         (add-before 'configure 'patchbuild
           (lambda _
             (substitute*
                 '("mfhdf/hdfimport/testutil.sh.in" "hdf/util/testutil.sh.in")
               (("/bin/rm") "rm")
               (("/bin/mkdir") "mkdir"))))
         (add-after 'configure 'patch-settings
           (lambda _
             ;; libhdf4.settings contains the full path of the
             ;; compilers used, and its contents are included in
             ;; .so-files.  We truncate the hashes to avoid
             ;; unnecessary store references to those compilers:
             (substitute* "libhdf4.settings"
               (("(/gnu/store/)([0-9A-Za-z]*)" all prefix hash)
                (string-append prefix (string-take hash 10) "...")))
             #t))
         (add-after 'install 'provide-absolute-libjpeg-reference
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (libjpeg (assoc-ref inputs "libjpeg")))
               ;; libjpeg-turbo does not provide a .la file, so libtool is
               ;; unable to add an absolute reference for -ljpeg in the .la
               ;; files.  Fix it manually to avoid having to propagate it.
               (substitute* (find-files (string-append out "/lib") "\\.la$")
                 (("-ljpeg")
                  (string-append "-L" libjpeg "/lib -ljpeg")))))))))
    (home-page "https://www.hdfgroup.org/products/hdf4/")
    (synopsis
     "Library and multi-object file format for storing and managing data")
    (description "HDF4 is a library and multi-object file format for storing
and managing data between machines.  HDF4 is an older hierarchical data format,
incompatible with HDF5.")
    (license
     (license:non-copyleft
      "https://www.hdfgroup.org/ftp/HDF/HDF_Current/src/unpacked/COPYING"))))

(define-public hdf4-alt
  (package
    (inherit hdf4)
    (name "hdf4-alt")
    (arguments
     (substitute-keyword-arguments (package-arguments hdf4)
       ((#:configure-flags flags) `(cons* "--disable-netcdf" ,flags))))
    (synopsis
     "HDF4 without netCDF API, can be combined with the regular netCDF library")))

(define-public hdf5-1.8
  (package
    (name "hdf5")
    (version "1.8.23")
    (source
     (origin
      (method url-fetch)
      (uri (list (string-append "https://support.hdfgroup.org/ftp/HDF5/releases/"
                                "hdf5-" (version-major+minor version)
                                "/hdf5-" version "/src/hdf5-"
                                version ".tar.bz2")
                 (string-append "https://support.hdfgroup.org/ftp/HDF5/"
                                "current"
                                (match (string-split version #\.)
                                  ((major minor _ ...)
                                   (string-append major minor)))
                                "/src/hdf5-" version ".tar.bz2")))
      (sha256
       (base32 "0km65mr6dgk4ia2dqr1b9dzw9qg15j5z35ymbys9cnny51z1zb39"))
      (patches (search-patches "hdf5-config-date.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list zlib))
    (native-inputs
     (list gfortran perl))                 ;part of the test machinery needs Perl
    (outputs '("out"       ; core library
               "fortran")) ; fortran interface
    (arguments
     `(;; Some of the users, notably Flann, need the C++ interface.
       #:configure-flags '("--enable-cxx"
                           "--enable-fortran"
                           "--enable-fortran2003"

                           ;; Build a thread-safe library.  Unfortunately,
                           ;; 'configure' invites you to either turn off C++,
                           ;; Fortran, and the high-level interface (HL), or
                           ;; to pass '--enable-unsupported'.  Debian
                           ;; packagers chose to pass '--enable-unsupported'
                           ;; and we follow their lead here.
                           "--enable-threadsafe"
                           "--with-pthread"
                           "--enable-unsupported")
       ;; Use -fPIC to allow the R bindings to link with the static libraries
       #:make-flags (list "CFLAGS=-fPIC"
                          "CXXFLAGS=-fPIC")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "configure"
               (("/bin/mv") "mv"))
             (substitute* "fortran/src/Makefile.in"
               (("libhdf5_fortran_la_LDFLAGS =")
                (string-append "libhdf5_fortran_la_LDFLAGS = -Wl,-rpath="
                               (assoc-ref outputs "fortran") "/lib")))
             (substitute* "hl/fortran/src/Makefile.in"
               (("libhdf5hl_fortran_la_LDFLAGS =")
                (string-append "libhdf5hl_fortran_la_LDFLAGS = -Wl,-rpath="
                               (assoc-ref outputs "fortran") "/lib")))))
         (add-after 'configure 'patch-settings
           (lambda _
             ;; libhdf5.settings contains the full path of the
             ;; compilers used, and its contents are included in
             ;; libhdf5.so.  We truncate the hashes to avoid
             ;; unnecessary store references to those compilers:
             (substitute* "src/libhdf5.settings"
              (("(/gnu/store/)([a-zA-Z0-9]*)" all prefix hash)
               (string-append prefix (string-take hash 10) "..."))
              ;; Don't record the build-time kernel version to make the
              ;; settings file reproducible.
              (("Uname information:.*")
               "Uname information: Linux\n"))))
         (add-after 'install 'patch-references
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin"))
                   (zlib (assoc-ref inputs "zlib")))
               (substitute* (find-files bin "h5p?cc")
                 (("-lz" lib)
                  (string-append "-L" zlib "/lib " lib))))))
         (add-after 'install 'split
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Move all fortran-related files
              (let* ((out (assoc-ref outputs "out"))
                     (bin (string-append out "/bin"))
                     (lib (string-append out "/lib"))
                     (inc (string-append out "/include"))
                     (ex (string-append out "/share/hdf5_examples/fortran"))
                     (fort (assoc-ref outputs "fortran"))
                     (fbin (string-append fort "/bin"))
                     (flib (string-append fort "/lib"))
                     (finc (string-append fort "/include"))
                     (fex (string-append fort "/share/hdf5_examples/fortran")))
                (mkdir-p fbin)
                (mkdir-p flib)
                (mkdir-p finc)
                (mkdir-p fex)
                ;; Note: When built with --enable-parallel, the 'h5fc' file
                ;; doesn't exist, hence this condition.
                (when (file-exists? (string-append bin "/h5fc"))
                  (rename-file (string-append bin "/h5fc")
                               (string-append fbin "/h5fc")))
                (for-each (lambda (file)
                            (rename-file file
                                         (string-append flib "/" (basename file))))
                          (find-files lib ".*fortran.*"))
                (for-each (lambda (file)
                            (rename-file file
                                         (string-append finc "/" (basename file))))
                          (find-files inc ".*mod"))
                (for-each (lambda (file)
                            (rename-file file
                                         (string-append fex "/" (basename file))))
                          (find-files ex ".*"))
                (delete-file-recursively ex)))))))
    (home-page "https://www.hdfgroup.org")
    (synopsis "Management suite for extremely large and complex data")
    (description "HDF5 is a suite that makes possible the management of
extremely large and complex data collections.")
    (license (license:x11-style
              "https://www.hdfgroup.org/ftp/HDF5/current/src/unpacked/COPYING"))))

(define-public hdf5-1.10
  (package
    (inherit hdf5-1.8)
    (version "1.10.9")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "https://support.hdfgroup.org/ftp/HDF5/releases/"
                                 "hdf5-" (version-major+minor version)
                                 "/hdf5-" version "/src/hdf5-"
                                 version ".tar.bz2")
                  (string-append "https://support.hdfgroup.org/ftp/HDF5/"
                                 "current"
                                 (apply string-append
                                        (take (string-split version #\.) 2))
                                 "/src/hdf5-" version ".tar.bz2")))
       (sha256
        (base32 "14gih7kmjx4h3lc7pg4fwcl28hf1qqkf2x7rljpxqvzkjrqbxi00"))
       (patches (search-patches "hdf5-config-date.patch"))))))

(define-public hdf5
  (package
    (name "hdf5")
    (version "1.14.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HDFGroup/hdf5")
             (commit (string-append "hdf5_" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1f7yv0xra465c3qy8c79fzddib653wzj5dsakb0bs02nwp3xm54q"))
       (modules '((guix build utils)))
       (snippet
        '(for-each
          delete-file
          (append
           (find-files "." "Makefile\\.in$")
           (find-files "java/lib" "\\.jar$")
           (list "aclocal.m4"
                 "bin/compile"
                 "bin/config.guess"
                 "bin/config.sub"
                 "bin/depcomp"
                 "bin/install-sh"
                 "bin/ltmain.sh"
                 "bin/missing"
                 "bin/test-driver"
                 "configure"
                 "HDF5Examples/aclocal.m4"
                 "HDF5Examples/compile"
                 "HDF5Examples/configure"
                 "HDF5Examples/depcomp"
                 "HDF5Examples/missing"
                 "HDF5Examples/test-driver"
                 "hl/src/H5LTanalyze.c"
                 "hl/src/H5LTparse.c"
                 "hl/src/H5LTparse.h"
                 "hl/tools/gif2h5/testfiles/ex_image2.h5"
                 "hl/tools/gif2h5/testfiles/h52giftst.h5"
                 "m4/ax_prog_doxygen.m4"
                 "m4/libtool.m4"
                 "m4/lt~obsolete.m4"
                 "m4/ltoptions.m4"
                 "m4/ltsugar.m4"
                 "m4/ltversion.m4"
                 "src/H5config.h.in"
                 "src/H5Edefin.h"
                 "src/H5Einit.h"
                 "src/H5Epubgen.h"
                 "src/H5Eterm.h"
                 "src/H5overflow.h"
                 "src/H5version.h"))))
       (patches (search-patches "hdf5-config-dependencies.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; Some of the users, notably Flann, need the C++ interface.
      #:configure-flags
      #~(list
         (string-append "-DHDF5_INSTALL_CMAKE_DIR=" #$output "/lib/cmake")
         "-DHDF5_BUILD_CPP_LIB=ON"
         "-DHDF5_BUILD_FORTRAN=ON"
         ;; Build a thread-safe library.  Unfortunately, CMakeLists.txt
         ;; invites you to either turn off C++, Fortran, and the high-level
         ;; interface (HL), or to enable 'ALLOW_UNSUPPORTED'.  Debian
         ;; packagers chose to pass '--enable-unsupported' to the 'configure'
         ;; script and we follow their lead here.
         "-DHDF5_ENABLE_THREADSAFE=ON"
         "-DALLOW_UNSUPPORTED=ON"
         "-DHDF5_BUILD_DOC=ON")
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: src/H5private.h includes <fenv.h> and fails to find the
          ;; stdlib types when the gfortran header is used.  Remove gfortran
          ;; from CPLUS_INCLUDE_PATH as a workaround.
          (add-after 'set-paths 'hide-gfortran
            (lambda _
              (let ((gfortran #$(this-package-native-input "gfortran")))
                (setenv "CPLUS_INCLUDE_PATH"
                        (string-join
                         (delete (string-append gfortran "/include/c++")
                                 (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                         ":")))))
          (add-after 'unpack 'make-gen-deterministic
            (lambda _
              (substitute* "bin/make_err"
                (("keys %major" all)
                 (string-append "sort " all))
                (("while.*each \\(%section\\).*")
                 (string-append
                  "foreach $sect_name (sort keys %section) {\n"
                  "        $sect_desc = $section{$sect_name};\n")))))
          (add-after 'unpack 'generate-flexbison
            (lambda _
              (invoke "bash" "bin/genparser" "hl/src")))
          (add-after 'unpack 'patch-trace-shebang
            (lambda _
              (for-each patch-shebang
                        (find-files "bin" (lambda (file stat)
                                            (executable-file? file))))))
          ;; This is a known issue on i686, see
          ;; https://github.com/HDFGroup/hdf5/issues/4926
          (add-after 'unpack 'fix-fortran-i686-test
            (lambda _
              (substitute* "fortran/test/tH5R.F90"
                (((string-append
                   "CALL h5rget_obj_name_f\\(C_LOC\\(ref_ptr\\(2\\)\\),"
                   " \"\", error, name_len=buf_size \\)"))
                 (string-append
                  "CALL h5rget_obj_name_f(C_LOC(ref_ptr(2)),"
                  " \"xxxxxxxxxxxxxx\", error, name_len=buf_size)")))))
          (add-after 'unpack 'generate-headers
            (lambda _
              (invoke "perl" "bin/make_err" "src/H5err.txt")
              (invoke "perl" "bin/make_vers" "src/H5vers.txt")
              (invoke "perl" "bin/make_overflow" "src/H5overflow.txt"))))))
    (inputs (list libaec zlib))
    (native-inputs
     (list bison
           doxygen
           flex
           gfortran
           graphviz
           ;; Needed to generate some headers and for tests
           perl
           which))
    (home-page "https://www.hdfgroup.org")
    (synopsis "Management suite for extremely large and complex data")
    (description "HDF5 is a suite that makes possible the management of
extremely large and complex data collections.")
    (license (license:x11-style
              "https://support.hdfgroup.org/ftp/HDF5/releases/COPYING.html"))))

;; Keep this in sync with the current hdf5 package.
(define-public hdf-java
  (package
    (name "hdf-java")
    (version "1.14.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/HDFGroup/hdf5")
             (commit (string-append "hdf5-"
                                    (string-map
                                     (lambda (c) (if (char=? c #\.) #\_ c))
                                     version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lw9f62zxyjiv7vx9nvnashjj39i44j8d626i7b788zkxw58csvs"))
       (modules '((guix build utils)))
       (snippet     ; Make sure we don't use the bundled sources and binaries.
        '(for-each delete-file
                   (find-files "java/lib" "\\.jar$")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-java"
              "--disable-tools")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'unbundle
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((simple
                     (search-input-file
                      inputs "/lib/m2/org/slf4j/slf4j-simple/1.7.25/slf4j-simple-1.7.25.jar"))
                    (api
                     (search-input-file
                      inputs "/lib/m2/org/slf4j/slf4j-api/1.7.25/slf4j-api-1.7.25.jar"))
                    (junit
                     (search-input-file
                      inputs "/lib/m2/junit/junit/4.12/junit-4.12.jar"))
                    (hamcrest
                     (search-input-file
                      inputs "/lib/m2/org/hamcrest/hamcrest-core/1.3/hamcrest-core-1.3.jar")))
                (substitute* (append (find-files "java" "Makefile.am")
                                     (find-files "java" "Makefile.in"))
                  (("\\$\\(top_srcdir\\)/java/lib/ext/slf4j-simple-2.0.6.jar")
                   simple)
                  (("\\$\\(top_srcdir\\)/java/lib/slf4j-api-2.0.6.jar")
                   api)
                  (("\\$\\(top_srcdir\\)/java/lib/junit.jar")
                   junit)
                  (("\\$\\(top_srcdir\\)/java/lib/hamcrest-core.jar")
                   hamcrest))
                (substitute* '("java/test/junit.sh.in"
                               "java/examples/datatypes/JavaDatatypeExample.sh.in"
                               "java/examples/datasets/JavaDatasetExample.sh.in"
                               "java/examples/intro/JavaIntroExample.sh.in"
                               "java/examples/groups/JavaGroupExample.sh.in")
                  (("^LIST_JAR_TESTFILES=\"" m)
                   (string-append m hamcrest "\n"
                                  junit "\n"
                                  api "\n"
                                  simple "\n"))
                  (("^\\$HDFLIB_HOME/.*") "")
                  (("\"\\$BLDLIBDIR\"/junit.jar")
                   junit)
                  (("\"\\$BLDLIBDIR\"/hamcrest-core.jar")
                   hamcrest)
                  (("\"\\$BLDLIBDIR\"/slf4j-api-2.0.6.jar")
                   api)
                  (("\"\\$BLDLIBDIR\"/slf4j-simple-2.0.6.jar")
                   simple)
                  (("/usr/bin/test")
                   (search-input-file inputs "/bin/test"))
                  (("/usr/bin/uname")
                   (search-input-file inputs "/bin/uname")))
                (substitute* (find-files "java/test/testfiles/" ".*\\.txt$")
                  (("JUnit version 4.11")
                   "JUnit version 4.12-SNAPSHOT"))))))))
    (native-inputs
     (list `(,icedtea "jdk")
           ;; For tests:
           java-hamcrest-core
           java-junit
           java-slf4j-simple))
    (inputs
     (list hdf4
           hdf5
           java-slf4j-api
           libjpeg-turbo
           zlib))
    (home-page "https://www.hdfgroup.org")
    (synopsis "Java interface for the HDF4 and HDF5 libraries")
    (description "Java HDF Interface (JHI) and Java HDF5 Interface (JHI5) use
the Java Native Interface to wrap the HDF4 and HDF5 libraries, which are
implemented in C.")

    ;; BSD-style license:
    (license (license:x11-style
              "https://support.hdfgroup.org/ftp/HDF5/hdf-java\
/current/src/unpacked/COPYING.html"))))

(define-public hdf-eos2
  (package
    (name "hdf-eos2")
    (version "19.1.0")
    (source
     (origin
       (method url-fetch)
       (uri "ftp://edhs1.gsfc.nasa.gov\
/edhs/hdfeos/latest_release/HDF-EOS2.19v1.00.tar.Z")
       (sha256
        (base32 "0c9fcz25s292ldap12wxmlrvnyz99z24p63d8fwx51bf8s0s1zrz"))
       (patches (search-patches "hdf-eos2-remove-gctp.patch"
                                "hdf-eos2-build-shared.patch"
                                "hdf-eos2-fortrantests.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gfortran))
    (inputs
     `(("hdf4" ,hdf4-alt) ; assume most HDF-EOS2 users won't use the HDF4 netCDF API
       ;; XXX: These inputs are really dependencies of hdf4.
       ("zlib" ,zlib)
       ("libjpeg" ,libjpeg-turbo)
       ("libtirpc" ,libtirpc)

       ("gctp" ,gctp)))
    (arguments
     `( #:configure-flags '("--enable-install-include" "--enable-shared"
                            "CC=h4cc -Df2cFortran" "LIBS=-lgctp")
        #:parallel-tests? #f))
    (home-page "https://hdfeos.org/software/library.php#HDF-EOS2")
    (synopsis "HDF4-based data format for NASA's Earth Observing System")
    (description "HDF-EOS2 is a software library built on HDF4 which supports
the construction of data structures used in NASA's Earth Observing
System (Grid, Point and Swath).")

    ;; Source files carry a permissive license header.
    (license (license:non-copyleft home-page))))

(define-public hdf-eos5
  (package
    (name "hdf-eos5")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://git.earthdata.nasa.gov/projects/DAS/repos/hdfeos5/raw/"
             "hdf-eos5-" version "-src.tar.gz?at=refs/heads/HDFEOS5_" version))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0by82zznms00b0d5v4iv8a7jff6xm9hzswsx4mfzw2gyy1q4ghyp"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each delete-file (find-files "." "Makefile\\.in$"))
            (for-each delete-file (find-files "m4" "^l.*\\.m4$"))
            (delete-file "configure")
            (delete-file "aclocal.m4")))))
    (native-inputs
     (list autoconf automake gfortran libtool))
    (build-system gnu-build-system)
    (inputs
     (list hdf5 zlib gctp))
    (arguments
     (list
      #:configure-flags ''("--enable-install-include" "--enable-shared"
                           "CC=h5cc -Df2cFortran" "LIBS=-lgctp")
      #:parallel-tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-single_module-flag
            (lambda _
              (substitute* "src/Makefile.am"
                ((",-single_module") ""))))
          (add-after 'unpack 'fix-parallel-tests
            (lambda _
              (substitute* (find-files "testdrivers" "\\.c$")
                (("#include <HE5_HdfEosDef.h>" orig)
                 (string-append "#include <HE5_config.h>\n" orig)))
              ;; pthread is already linked.
              (substitute* "testdrivers/threads/Makefile.am"
                (("(LDADD=\\$\\(LIBHDFEOS5\\) \\$\\(LIBGCTP\\)) pthread" _ rest)
                 rest))
              ;; This file is missing in the testdrivers/threads directory.
              (copy-file "testdrivers/point/simple.txt"
                         "testdrivers/threads/simple.txt"))))))
    (synopsis "HDF5-based data format for NASA's Earth Observing System")
    (description
     "HDF-EOS5 is a software library built on HDF5 to support the construction
of data structures used in NASA's Earth Observing System (Grid, Point and
Swath).")
    (home-page "https://www.hdfeos.org/software/library.php#HDF-EOS5")

    ;; Source files carry a permissive license header.
    (license (license:non-copyleft home-page))))

(define-public hdf5-parallel-openmpi
  (package/inherit hdf5
    (name "hdf5-parallel-openmpi")
    (inputs (modify-inputs (package-inputs hdf5)
              (prepend openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments hdf5)
       ((#:configure-flags _ #f)
        ''("-DHDF5_ENABLE_THREADSAFE=OFF"
           "-DHDF5_ENABLE_PARALLEL=ON"
           "-DHDF5_BUILD_CPP_LIB=OFF"
           "-DHDF5_BUILD_DOC=ON"))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-after 'build 'mpi-setup
              #$%openmpi-setup)))))
    (synopsis "Management suite for data with parallel IO support")))

(define-public hdf5-blosc
  (package
    (name "hdf5-blosc")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Blosc/hdf5-blosc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kq09w2mz8i735p3zfnsdqdvay0086ls0cb621ckfxq42pr3gkm4"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-DBLOSC_INSTALL_DIR="
                            (assoc-ref %build-inputs "c-blosc"))
             (string-append "-DPLUGIN_INSTALL_PATH="
                            (assoc-ref %outputs "out")
                            "/hdf5/lib/plugin"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-build-blosc
           (lambda _
             (substitute* "CMakeLists.txt"
               (("set\\(BLOSC_INSTALL_DIR.*") "")
               (("ExternalProject_Add\\(project_blosc") "message("))
             #t)))))
    (inputs
     (list c-blosc hdf5))
    (home-page "https://github.com/Blosc/hdf5-blosc")
    (synopsis "Filter for HDF5 using the Blosc compressor")
    (description "This is a filter for HDF5 that uses the Blosc compressor; by
installing this filter, you can read and write HDF5 files with
Blosc-compressed datasets.")
    (license license:expat)))

(define-public itex2mml
  (package
    (name "itex2mml")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://golem.ph.utexas.edu"
                                  "/~distler/blog/files/itexToMML-"
                                  version
                                  ".tar.gz"))
              (sha256
               (base32
                "0pz51c0hfh2mg8xli0wj7hf92s3b7yf5r4114g8z8722lcm5gwiy"))
              (snippet
               #~(begin
                   (use-modules (guix build utils))
                   (delete-file-recursively "itex-binaries")))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison
           flex))
    (arguments
     (list
      #:make-flags #~(list (string-append "BINDIR=" #$output "/bin/")
                           (string-append "CC=" #$(cc-for-target)))
      #:tests? #f ;; there are none
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'chdir
            (lambda args
              (chdir "itex-src")))
          (add-before 'install 'make-bindir
            (lambda args
              (mkdir-p (string-append #$output "/bin"))))
          (add-after 'install 'install-doc
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((doc-prefix (or (assoc-ref outputs "doc")
                                     #$output))
                     (itex2mml+version (strip-store-file-name #$output))
                     (doc-dir (string-append doc-prefix
                                             "/share/doc/"
                                             itex2mml+version)))
                (install-file "../README" doc-dir)))))))
    (home-page "https://golem.ph.utexas.edu/~distler/blog/itex2MML.html")
    (synopsis "LaTeX to XHTML/MathML converter")
    (description
     "The @command{itex2MML} utility is a stream filter.  It takes text with
embedded itex equations, converts the itex equations to MathML, and outputs
the resulting text.")
    (license (list license:lgpl2.0+ license:gpl2+ license:mpl1.1))))

(define-public itpp
  (package
    (name "itpp")
    (version "4.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/itpp/itpp/"
                                  version "/itpp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "14ddy2xnb6sgp4hiax9v5sv4pr4l4dd4ps76nfha3nrpr1ikhcqm"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; Tests require googletest *sources*
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'delete-formulas-log
            ;; Contains date and timing information which is unreproducible,
            ;; and should not be needed when using the package
            (lambda _
              (delete-file
               (string-append #$output "/share/doc/itpp/html/_formulas.log"))))
          (add-after 'unpack 'set-man-page-date
            (lambda _
              (substitute* "itpp-config.1.cmake.in"
                ((".PACKAGE_DATE.") "2012-04-18"))))
          (add-before 'build 'set-force-source-date
            ;; for reproducible dates, texlive needs this to respect respect
            ;; SOURCE_DATE_EPOCH
            (lambda _
              (setenv "FORCE_SOURCE_DATE" "1"))))))
    (inputs (list fftw lapack openblas))
    ;; FIXME: Even though the fonts are available dvips complains:
    ;; "Font cmmi10 not found; characters will be left blank."
    (native-inputs (list (texlive-updmap.cfg) ghostscript doxygen))
    (home-page "https://itpp.sourceforge.net")
    (synopsis "C++ library of maths, signal processing and communication classes")
    (description "IT++ is a C++ library of mathematical, signal processing and
communication classes and functions.  Its main use is in simulation of
communication systems and for performing research in the area of
communications.  The kernel of the library consists of generic vector and
matrix classes, and a set of accompanying routines.  Such a kernel makes IT++
similar to MATLAB, GNU Octave or SciPy.")
    (license license:gpl3+)))

(define-public netcdf
  (package
    (name "netcdf")
    (version "4.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://downloads.unidata.ucar.edu/netcdf-c/" version
             "/netcdf-c-" version ".tar.gz"))
       (sha256
        (base32
         "0j8b814mjdqvqanzmrxpq8hn33n22cdzb3gf9vhya24wnwi615ac"))
       (modules '((guix build utils)))
       (snippet
        ;; Make sure this variable is defined only once.  Failing to do so
        ;; would break builds of 'netcdf-parallel-openmpi' with a
        ;; multiple-definition link error with GCC 10.
        '(substitute* "ncdump/ocprint.c"
           (("^int ocdebug") "static int ocdebug")))
       (patches (search-patches "netcdf-date-time.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4 doxygen graphviz))
    (inputs
     (list curl
           hdf4-alt
           hdf5
           libjpeg-turbo
           libxml2
           unzip
           zlib))
    (arguments
     (list #:configure-flags
           #~'("--enable-doxygen" "--enable-dot"
               "--enable-hdf4" "--disable-dap-remote-tests")

           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'fix-source-date
                 (lambda _
                   ;; As we ${SOURCE_DATE_EPOCH} evaluates to "1" in the build
                   ;; environment, `date -u -d ${SOURCE_DATE_EPOCH}` will evaluate
                   ;; to '1st hour of the current day', and therefore makes the
                   ;; package not reproducible.
                   (substitute* "./configure"
                     (("date -u -d \"\\$\\{SOURCE_DATE_EPOCH\\}\"")
                      "date --date='@0'"))))
               (add-after 'configure 'patch-settings
                 (lambda _
                   ;; libnetcdf.settings contains the full filename of the compilers
                   ;; used to build the library.  We truncate the hashes of those
                   ;; filenames to avoid unnecessary references to the corresponding
                   ;; store items.
                   (substitute* "libnetcdf.settings"
                     (("(/gnu/store/)([0-9A-Za-z]*)" all prefix hash)
                      (string-append prefix
                                     (string-take hash 10) "...")))))
               (add-before 'check 'fix-test-rcmerge
                 (lambda _
                   ;; Set HOME, to fix the test-rcmerge test.
                   (setenv "HOME" "/tmp"))))

           #:parallel-tests? #f))           ;various race conditions
    (home-page "https://www.unidata.ucar.edu/software/netcdf/")
    (synopsis "Library for scientific data")
    (description "NetCDF is an interface for scientific data access and a
software library that provides an implementation of the interface.  The netCDF
library defines a machine-independent format for representing scientific data.
Together, the interface, library, and format support the creation, access, and
sharing of scientific data.")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public netcdf-parallel-openmpi
  (package/inherit netcdf
    (name "netcdf-parallel-openmpi")
    (inputs (modify-inputs (package-inputs netcdf)
              (append openmpi pnetcdf)
              (replace "hdf5" hdf5-parallel-openmpi)))
    ;; TODO: Replace pkg-config references in nc-config with absolute references
    (arguments
     (substitute-keyword-arguments (package-arguments netcdf)
       ((#:configure-flags flags)
        #~(cons* "CC=mpicc"
                 "CXX=mpicxx"
                 "--enable-parallel-tests"
                 ;; Enable support of CDF-1, 2 and 5 formats.
                 "--enable-pnetcdf"
                 ;; NetCDF supports both parallel and shared library building
                 ;; See https://docs.unidata.ucar.edu/nug/current/getting_and_building_netcdf.html#build_parallel
                 "--enable-shared"
                 "--with-pic"
                 #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'build 'mpi-setup
              #$%openmpi-setup)))))))

(define-public netcdf-fortran
  (package
    (name "netcdf-fortran")
    (version "4.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.unidata.ucar.edu/pub/netcdf/netcdf-fortran-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0x4acvfhbsx1q79dkkwrwbgfhm0w5ngnp4zj5kk92s1khihmqfhj"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("FCFLAGS=-fallow-argument-mismatch"
                           "FFLAGS=-fallow-argument-mismatch")
       #:parallel-tests? #f))
    (inputs
     (list netcdf))
    (native-inputs
     (list gfortran))
    (synopsis "Fortran interface for the netCDF library")
    (description (package-description netcdf))
    (home-page (package-home-page netcdf))
    (license (package-license netcdf))))

(define-public netcdf-cxx4
  (package
    (name "netcdf-cxx4")
    (version "4.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Unidata/netcdf-cxx4")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05kydd5z9iil5iv4fp7l11cicda5n5lsg5sdmsmc55xpspnsg7hr"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-before 'configure 'patch-configure
                    (lambda _
                      (substitute* "libnetcdf-cxx.settings.in"
                        ;; Don't record the build-time host, time and gcc path to make the
                        ;; settings file reproducible.
                        (("@CONFIG_DATE@")
                         "Not set (Guix build)")
                        (("@host_cpu@-@host_vendor@-@host_os@")
                         "Linux")
                        (("@CC_VERSION@")
                         "gcc"))
                      ;; The filter tests fail with 'Caught unexpected exception'.
                      (substitute* "cxx4/CMakeLists.txt"
                        (("add_bin_test\\(cxx4 test_filter\\)")
                         ""))
                      (substitute* "examples/CMakeLists.txt"
                        (("add_sh_test\\(examples tst_filter\\)")
                         ""))))
                  (add-after 'install 'clear-reference-to-compiler
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; Do not retain a reference to GCC and other build only inputs.
                      (let ((out (assoc-ref outputs "out")))
                        (substitute* (string-append out "/bin/ncxx4-config")
                          (("cc=([[:graph:]]+)/bin/gcc")
                           "cc=\"gcc")
                          (("cxx=([[:graph:]]+)/bin/c\\+\\+")
                           "cxx=\"c++"))))))
       #:configure-flags (list (string-append "-DHDF5_C_LIBRARY_hdf5="
                                              (search-input-file
                                               %build-inputs
                                               "/lib/libhdf5.so")))))
    (inputs (list hdf5))
    (propagated-inputs (list netcdf)) ;include/netcdf includes netcdf.h
    (home-page "https://github.com/Unidata/netcdf-cxx4")
    (synopsis "NetCDF C++ interface")
    (description
     "This package provides a C++ interface to the NetCDF library for
scientific data storage.")
    (license license:bsd-3)))

(define-public netcdf-cxx4-parallel-openmpi
  (package
    (inherit netcdf-cxx4)
    (name "netcdf-cxx4-parallel-openmpi")
    (synopsis "NetCDF C++ interface (with MPI support)")
    (inputs (modify-inputs (package-inputs netcdf-cxx4)
              (prepend openmpi)
              (replace "hdf5" hdf5-parallel-openmpi)))
    (propagated-inputs (modify-inputs (package-propagated-inputs netcdf-cxx4)
              (replace "netcdf" netcdf-parallel-openmpi)))))

(define-public n2p2
  (package
    (name "n2p2")
    (version "2.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/CompPhysVienna/n2p2")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lw195ihpxwh08387i4gamk1glhalpq888q6nj8l5vswbgnrv1pq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags '("MODE=shared" "-C" "src")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'post-unpack
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/makefile.gnu"
               (("PROJECT_EIGEN=/usr/include/eigen3")
                (string-append "PROJECT_EIGEN="
                               (assoc-ref inputs "eigen") "/include/eigen3")))
             (substitute* "src/makefile.gnu"
               (("-lblas")
                (string-append "-L" (assoc-ref inputs "openblas")
                               "/lib -lopenblas"))
               (("-march=native")
                ""))
             (substitute* "src/application/makefile"
               (("LDFLAGS=")
                "LDFLAGS=-Wl,-rpath='$$ORIGIN/../lib' "))))
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bindir (string-append out "/bin"))
                    (libdir (string-append out "/lib"))
                    (incdir (string-append out "/include")))
               (for-each (lambda (f) (install-file f bindir))
                         (find-files "bin" "^nnp-"))
               (for-each (lambda (f) (install-file f libdir))
                         (find-files "lib" "\\.so$"))
               (for-each (lambda (f) (install-file f incdir))
                         (find-files "include" "\\.h$"))))))))
    (inputs
     (list openmpi gsl openblas eigen))
    (synopsis "Neural network potentials for chemistry and physics")
    (description "This package contains software that will allow you to use
existing neural network potential parameterizations to predict energies and
forces (with standalone tools but also in conjunction with the MD software
LAMMPS).  In addition it is possible to train new neural network potentials
with the provided training tools.")
    (home-page "https://compphysvienna.github.io/n2p2/")
    (properties '((tunable? . #t)))        ;to benefit from SIMD code in Eigen
    (license license:gpl3+)))

(define-public nlopt
  (package
    (name "nlopt")
    (version "2.7.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/stevengj/nlopt/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1xpdza28i8w441fwv6a5f3qk4zi7ys6ws9fx6kr5ny27dfdz6rr1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-libnlopt-file-name
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Make sure the Scheme module refers to the library by its
             ;; absolute file name.
             (let ((out (assoc-ref outputs "out")))
               (substitute* "src/swig/nlopt-guile.i"
                 (("\"nlopt_guile\"")
                  (format #f "~s"
                          `(format #f "~anlopt_guile"
                                   (if (getenv "NLOPT_UNINSTALLED")
                                       ""
                                       ,(format #f "~a/lib/guile/3.0/extensions/" out))))))
               (setenv "NLOPT_UNINSTALLED" "1")))))))
    (inputs (list guile-3.0 octave python))
    (native-inputs (list pkg-config swig))
    (home-page "http://ab-initio.mit.edu/wiki/")
    (synopsis "Library for nonlinear optimization")
    (description "NLopt is a library for nonlinear optimization, providing a
common interface for a number of different free optimization routines available
online as well as original implementations of various other algorithms.")
    (license license:lgpl2.1+)))

(define-public ipopt
  (package
    (name "ipopt")
    (version "3.13.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/coin-or/Ipopt")
                    (commit (string-append "releases/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "08gznhwhqv1x4baksz350ih8q16r5rd0k8vals6078m3h94khr4b"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list "--with-lapack=-lopenblas")
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'add--L-flags-in-ipopt.pc
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      ;; The '.pc' file lists '-llapack -lblas' in "Libs";
                      ;; move it to "Libs.private" where it belongs, and add a
                      ;; '-L' flag for LAPACK.
                      (let ((out    (assoc-ref outputs "out"))
                            (lapack (assoc-ref inputs "lapack")))
                        (substitute* (string-append out "/lib/pkgconfig/"
                                                    "ipopt.pc")
                          (("Libs: (.*)-llapack -lblas(.*)$" _ before after)
                           (string-append "Libs: " before " " after "\n"
                                          "Libs.private: " before
                                          "-L" openblas "/lib -lopenblas"
                                          after "\n")))
                        #t))))))
    (native-inputs
     (list gfortran pkg-config))
    (inputs
     ;; TODO: Maybe add dependency on COIN-MUMPS, ASL, and HSL.
     (list openblas))                    ;for both libblas and liblapack
    (home-page "https://www.coin-or.org")
    (synopsis "Large-scale nonlinear optimizer")
    (description
     "The Interior Point Optimizer (IPOPT) is a software package for
large-scale nonlinear optimization.  It provides C++, C, and Fortran
interfaces.")
    (license license:epl2.0)))

(define-public nomad-optimizer
  (package
    (name "nomad-optimizer")
    (version "4.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bbopt/nomad/")
             (commit (string-append "v." version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08bxdvx8p5qzdw331xa5irc1896as4q5hajsid7f3qcxjm4nq4v3"))))
    (build-system cmake-build-system)
    (native-inputs
     (list openmpi))
    (arguments
     (list
      ;; Cannot build Python interface because it is incompatible with OpenMP
      ;; support, which is enabled by default.
      #:configure-flags
      #~(list "-DBUILD_TESTS=ON"
              "-DBUILD_INTERFACE_C=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda _
              (substitute* "examples/CMakeLists.txt"
                ;; This test passes only sometimes.
                ;; See https://github.com/bbopt/nomad/issues/72.
                (("^ +add_subdirectory\\(\\$\\{CMAKE_CURRENT_SOURCE_DIR\\}/advanced/library/PSDMads\\)\n")
                 ""))
              (make-file-writable
               "examples/advanced/library/exampleSuggestAndObserve/cache0.txt")
              ;; Fix the tests so they run in out-of-source builds.
              (substitute*
                  '("examples/basic/library/COCO/CMakeLists.txt"
                    "examples/basic/library/example1/CMakeLists.txt"
                    "examples/basic/library/example2/CMakeLists.txt"
                    "examples/basic/library/example3/CMakeLists.txt"
                    "examples/basic/library/example4/CMakeLists.txt"
                    "examples/basic/library/single_obj_parallel/CMakeLists.txt"
                    "examples/advanced/library/FixedVariable/CMakeLists.txt"
                    "examples/advanced/library/NMonly/CMakeLists.txt"
                    "examples/advanced/library/PSDMads/CMakeLists.txt"
                    "examples/advanced/library/Restart/CMakeLists.txt"
                    "examples/advanced/library/Restart_VNS/CMakeLists.txt"
                    "examples/advanced/library/c_api/example1/CMakeLists.txt"
                    "examples/advanced/library/c_api/example2/CMakeLists.txt"
                    "examples/advanced/library/exampleSuggestAndObserve/CMakeLists.txt")
                ;; The runExampleTest.sh script is run with WORKING_DIRECTORY
                ;; set to CMAKE_CURRENT_SOURCE_DIR.
                ;; Other scripts invoked by that script (for example
                ;; examples/advanced/batch/SuggestAndObserve/loopSuggestAndObserve.sh)
                ;; are in that same directory, but compiled examples are
                ;; located in CMAKE_CURRENT_BINARY_DIR.
                (("(COMMAND \\$\\{CMAKE_BINARY_DIR\\}/examples/runExampleTest\\.sh )\\.(/.*)"
                  _ command test)
                 (string-append command "${CMAKE_CURRENT_BINARY_DIR}" test)))
              (let ((builddir (string-append (getcwd) "/../build")))
                (let ((dir "examples/advanced/library/FixedVariable"))
                  (substitute* (string-append dir "/fixedVariable.cpp")
                    (("^( +std::string sExe = ).*" _ prefix)
                     (string-append prefix "\"" builddir "/" dir "/ufl.exe" "\";\n"))))
                ;; The BB_EXE and SURROGATE_EXE paths are interpreted relative
                ;; to the configuration file provided to NOMAD.
                ;; However, the configuration files are all in the source tree
                ;; rather than in the build tree (unlike the compiled
                ;; executables).
                (let ((fix-exe-path (lambda* (dir #:optional
                                                  (file "param.txt")
                                                  (exe-opt "BB_EXE"))
                                      (substitute* (string-append dir "/" file)
                                        (((string-append "^" exe-opt " +"))
                                         ;; The $ prevents NOMAD from prefixing
                                         ;; the executable with the path of the
                                         ;; parent directory of the configuration
                                         ;; file NOMAD was provided with as
                                         ;; argument (param.txt or some such).
                                         (string-append exe-opt " $"
                                                        builddir "/" dir "/"))))))
                  (for-each
                   (lambda (dir)
                     (substitute* (string-append dir "/CMakeLists.txt")
                       ;; The install phase has not yet run.
                       (("(COMMAND.*)\\$\\{CMAKE_INSTALL_PREFIX\\}/bin/nomad\\b"
                         _ prefix)
                        (string-append prefix "${CMAKE_BINARY_DIR}/src/nomad")))
                     (if (equal? dir "examples/basic/batch/single_obj_MPIparallel")
                         (substitute* (string-append dir "/param.txt")
                           (("^BB_EXE +'\\$.*mpirun \\$-np \\$4 ")
                            (string-append "BB_EXE '$" (which "mpirun") " $"
                                           builddir "/" dir "/")))
                         (fix-exe-path dir))
                     (when (equal? dir "examples/basic/batch/surrogate_sort")
                       (fix-exe-path dir "param.txt" "SURROGATE_EXE"))
                     (when (equal? dir "examples/advanced/batch/FixedVariable")
                       (fix-exe-path dir "param1.txt")
                       (fix-exe-path dir "param2.txt")
                       (fix-exe-path dir "param3.txt")
                       (fix-exe-path dir "param10.txt")))
                   '("examples/basic/batch/coco_bbob-constrained"
                     "examples/basic/batch/example1"
                     "examples/basic/batch/example2"
                     "examples/basic/batch/example3"
                     "examples/basic/batch/multi_obj"
                     "examples/basic/batch/multi_obj2"
                     "examples/basic/batch/single_obj"
                     "examples/basic/batch/single_obj_MPIparallel"
                     "examples/basic/batch/single_obj_parallel"
                     "examples/basic/batch/surrogate_sort"
                     "examples/advanced/batch/FixedVariable"
                     "examples/advanced/batch/LHonly"))))))
          (add-before 'configure 'mpi-setup
            #$%openmpi-setup))))
    (home-page "https://www.gerad.ca/nomad/")
    (synopsis "Nonlinear optimization by mesh-adaptive direct search")
    (description
     "NOMAD is a C++ implementation of the mesh-adaptive direct search (MADS)
algorithm, designed for difficult blackbox optimization problems.  These
problems occur when the functions defining the objective and constraints are
the result of costly computer simulations.")
    (license license:lgpl3+)))

(define-public cbc
  (package
    (name "cbc")
    (version "2.10.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.coin-or.org/download/source/"
                                  "Cbc/Cbc-" version ".tgz"))
              (sha256
               (base32
                "0wk9vr6zc62gw71v7gnra5wxqlcljcgbhm5lasx236v791b986ns"))
              (modules '((guix build utils)))
              (snippet
               ;; Make sure we don't use the bundled software.
               '(delete-file-recursively "ThirdParty"))))
    (build-system gnu-build-system)
    (native-inputs
     (list gfortran pkg-config))
    (inputs
     (list openblas))
    (home-page "https://www.coin-or.org")
    (synopsis "Branch-and-cut solver")
    (description
     "Cbc (Coin-or branch and cut) is a mixed integer linear programming
solver written in C++.  It can be used as a library or as a standalone
executable.")
    (license license:epl1.0)))

(define-public clp
  (package
    (name "clp")
    (version "1.17.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.coin-or.org/download/source/"
                                  "Clp/Clp-" version ".tgz"))
              (sha256
               (base32
                "0ap1f0lxppa6pnbc4bg7ih7a96avwaki482nig8w5fr3vg9wvkzr"))
              (modules '((guix build utils)))
              (snippet
               ;; Make sure we don't use the bundled software.
               '(begin
                  (delete-file-recursively "ThirdParty")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list gfortran pkg-config))
    (inputs
     (list openblas))
    (home-page "https://www.coin-or.org")
    (synopsis "Linear programming solver")
    (description
     "CLP is a high quality linear programming solver.  Its main strengths are
its dual and primal Simplex algorithms.  It also has a barrier algorithm for
linear and quadratic objectives.  There are limited facilities for nonlinear
and quadratic objectives using the Simplex algorithm.")
    (license license:epl1.0)))

(define-public python-cylp
  (package
    (name "python-cylp")
    (version "0.92.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cylp" version))
       (sha256
        (base32 "1mhvjrhvpgnpw4zwri92dj168qvyclcpsqvzbj5maxx5cilnhkww"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags #~(list "-k" (string-append
                                 "not " (string-join
                                         (list
                                          "test_removeVar2" ; AssertionError
                                          ;; Tests below segfault
                                          "test_dantzig"
                                          "test_lifo"
                                          "test_mf"
                                          "test_pe")
                                         " and not ")))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key tests? #:allow-other-keys)
              (if tests? ; rebuild extensions
                  (invoke "python" "setup.py" "build_ext" "--inplace")))))))
    (propagated-inputs (list python-numpy python-pytest python-scipy))
    (inputs (list cbc))
    (native-inputs (list pkg-config
                         python-cython-3
                         python-hypothesis
                         python-numpy
                         python-pytest
                         python-setuptools
                         python-wheel))
    (home-page "https://github.com/coin-or/cylp")
    (synopsis "Python interface for CLP, CBC, and CGL")
    (description
     "CyLP is a Python interface to COIN-OR’s Linear and mixed-integer program
solvers (CLP, CBC, and CGL).  CyLP’s unique feature is that you can use it to
alter the solution process of the solvers from within Python.  For example,
you may define cut generators, branch-and-bound strategies, and primal/dual
Simplex pivot rules completely in Python.")
    (license license:epl2.0)))

(define-public gecode
  (let* ((commit "f7f0d7c273d6844698f01cec8229ebe0b66a016a")
         (version (git-version "6.2.0" "1" commit)))
    (package
      (name "gecode")
      (version version)
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/Gecode/gecode")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
            "16gzwa64w90vifaflmii515rsrqclf2y7nziq621m4ad9cjgcixj"))
         (modules '((guix build utils)))
         ;; delete generated sources
         (snippet '(for-each delete-file
                             '("gecode/kernel/var-imp.hpp"
                               "gecode/kernel/var-type.hpp")))))
      (outputs '("out" "examples"))
      (build-system gnu-build-system)
      (arguments
       (list
        #:configure-flags #~(list (string-append "GLDFLAGS=-Wl,-rpath="
                                                 #$output "/lib")
                                  "--enable-examples=no")
        #:modules '((guix build gnu-build-system)
                    (guix build utils)
                    (ice-9 rdelim)
                    (ice-9 popen))
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'configure 'patch-msc-and-version
              (lambda* (#:key outputs #:allow-other-keys)
                (let ((out (assoc-ref outputs "out")))
                  (substitute* "tools/flatzinc/gecode.msc.in"
                    (("\\.\\./../..") out)
                    (("\\.\\.")
                     (string-append out "/share/minizinc")))
                  (substitute* "configure"
                    (("(PACKAGE_[^0-9]*)[0-9\\.]+" all match)
                     (string-append match #$version))))))
            (add-after 'build 'build-examples
              (lambda _
                (invoke "make" "compileexamples")))
            ;; The Makefile disrespects GLDFLAGS for some reason,
            ;; so we have to patch it ourselves.
            (add-after 'install 'fix-rpath
              (lambda _
                (let ((libdir (string-append #$output "/lib")))
                  (for-each
                    (lambda (file)
                      (let* ((pipe (open-pipe* OPEN_READ "patchelf"
                                               "--print-rpath" file))
                             (line (read-line pipe)))
                        (and (zero? (close-pipe pipe))
                             (invoke "patchelf" "--set-rpath"
                                     (string-append libdir ":" line)
                                     file))))
                    (find-files libdir ".*\\.so$")))))
            (add-after 'install 'install-examples
              (lambda* (#:key outputs #:allow-other-keys)
                (let* ((examples (assoc-ref outputs "examples"))
                       (bindir (format #f "bindir=~a/bin" examples)))
                  (invoke "make" "installexamples" bindir)))))))
      (native-inputs (list patchelf perl sed))
      (home-page "https://www.gecode.org")
      (synopsis "Toolkit for developing constraint-based systems")
      (description
        "Gecode is a C++ toolkit for developing constraint-based systems
and applications.  It provides a modular and extensible solver.")
      (license license:expat))))

(define-public libfixmath
  (let ((commit "1416c9979635c69f344d3c1de84b3246001a6540")
        (revision "1"))
    (package
      (name "libfixmath")
      (version (git-version "0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PetteriAimonen/libfixmath")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1vnpycw30rq3xwqyvj20l7pnw74dc4f27304i0918igsrdsjw501"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'install
              (lambda _
                (let ((includes (string-append #$output "/include/libfixmath"))
                      (lib (string-append #$output "/lib")))
                  (mkdir-p includes)
                  (for-each (lambda (file)
                              (install-file file includes))
                            (find-files "../source" "\\.h(pp)?$"))
                  (for-each (lambda (file)
                              (install-file file lib))
                            (find-files "." "\\.a$"))))))))
      (home-page "https://code.google.com/archive/p/libfixmath/")
      (synopsis "Cross platform fixed point maths library")
      (description "This library implements the @file{math.h} functions in
fixed point (16.16) format.")
      (license license:expat))))

(define-public glucose
  (package
    (name "glucose")
    (version "4.2.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/audemard/glucose")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0zrn4hnkf8k95dc3s3acydl1bqkr8a0axw56g7n562lx7zj7sd62"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f ; there are no tests
      #:configure-flags
      #~(list "-DBUILD_SHARED_LIBS=ON"
              (string-append "-DCMAKE_BUILD_RPATH=" #$output "/lib"))
      #:phases #~(modify-phases %standard-phases
                   (replace 'install
                     (lambda _
                       (for-each
                        (lambda (bin)
                          (install-file bin (string-append #$output "/bin")))
                        '("glucose-simp" "glucose-syrup"))
                       (for-each
                        (lambda (lib)
                          (install-file lib (string-append #$output "/lib")))
                        '("libglucose.so" "libglucosep.so")))))))
    (inputs (list zlib))
    (home-page "https://www.labri.fr/perso/lsimon/research/glucose/")
    (synopsis "SAT Solver")
    (description "Glucose is a SAT solver based on a scoring scheme introduced
in 2009 for the clause learning mechanism of so called “Modern” SAT solvers.
It is designed to be parallel.")
    (license license:expat)))

(define-public libflame
  ;; The latest release (5.2.0) dates back to 2019.  Use a newer one, which
  ;; among other things provides extra LAPACK symbols, such as 'dgemlq_'
  ;; (needed by LAPACKe).
  (let ((commit "70c19e770ead0ae846c59b59216deb16d236b40c")
        (revision "0"))
    (package
      (name "libflame")
      (version (git-version "5.2.0" revision commit))
      (outputs '("out" "static"))
      (home-page "https://github.com/flame/libflame")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0rk8ln5p4yybsws6p60w0vkxbqp53jddv90brlgf60mk6lv51sxl"))))
      (build-system gnu-build-system)
      (arguments
       (list #:configure-flags
             ;; Sensible defaults: https://github.com/flame/libflame/issues/28
             #~(list "--enable-dynamic-build"
                     "--enable-max-arg-list-hack"
                     "--enable-lapack2flame"
                     "--enable-verbose-make-output"
                     "--enable-multithreading=pthreads" ; Openblas isn't built with openmp.
                     #$@(if (target-x86?)
                            #~("--enable-vector-intrinsics=sse")
                            #~())
                     "--enable-supermatrix"
                     "--enable-memory-alignment=16"
                     "--enable-ldim-alignment")
             #:make-flags #~(list "FC=gfortran -fPIC")
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-/usr/bin/env-bash
                   (lambda _
                     (substitute* "build/config.mk.in"
                       (("/usr/bin/env bash")
                        (which "bash")))))
                 (replace 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (substitute* "test/Makefile"
                       (("LIBBLAS .*")
                        "LIBBLAS = -lopenblas\n")
                       (("LIBLAPACK .*")
                        "LIBLAPACK = -lopenblas\n"))
                     (when tests?
                       (with-directory-excursion "test"
                         (mkdir "obj")
                         (invoke "make")
                         (invoke "./test_libflame.x")))))
                 (add-after 'install 'install-static
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out"))
                           (static (assoc-ref outputs "static")))
                       (mkdir-p (string-append static "/lib"))
                       (rename-file (string-append out
                                                   "/lib/libflame.a")
                                    (string-append static
                                                   "/lib/libflame.a"))
                       (install-file (string-append out
                                                    "/include/FLAME.h")
                                     (string-append static "/include"))))))))
      (inputs (list gfortran))
      (native-inputs (list openblas perl python-wrapper))
      (synopsis "High-performance library for @acronym{DLA, dense linear algebra} computations")
      (description "@code{libflame} is a portable library for dense matrix
computations, providing much of the functionality present in LAPACK, developed
by current and former members of the @acronym{SHPC, Science of High-Performance
Computing} group in the @url{https://www.ices.utexas.edu/, Institute for
Computational Engineering and Sciences} at The University of Texas at Austin.
@code{libflame} includes a compatibility layer, @code{lapack2flame}, which
includes a complete LAPACK implementation.")
      (license license:bsd-3))))

(define-public hpcombi
  (package
    (name "hpcombi")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/libsemigroups/hpcombi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "00mbxw5x6m61n0x68dsiyq97i7b08h3hkbj9is2w6gcg571jy319"))))
    (arguments
     (list #:configure-flags #~(list "-DBUILD_TESTING=ON")))
    (native-inputs
     (list catch2-3))
    (build-system cmake-build-system)
    (home-page "https://libsemigroups.github.io/HPCombi/")
    (synopsis "Fast combinatorics in C++ using SSE/AVX instruction sets")
    (description "HPCombi is a C++17 header-only library using the SSE and AVX
instruction sets, and some equivalents, for very fast manipulation of
combinatorial objects such as transformations, permutations, and boolean
matrices of small size.")
    (license license:gpl3+)))

(define-public scasp
  (package
    (name "scasp")
    (version "1.1.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SWI-Prolog/sCASP")
                    (commit (string-append "V" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m4fs1ywich9cwj55miqp5zxs7c1fw9wvy7lcj5rkrgcanks5qk4"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~`(("scasp" "bin/")
                         ("prolog" "lib/swipl/library"))
      #:modules `((guix build copy-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:)
                  (guix build utils)
                  (ice-9 regex))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'build (assoc-ref gnu:%standard-phases 'build))
          (add-after 'build 'check (assoc-ref gnu:%standard-phases 'check)))))
    (native-inputs (list swi-prolog))
    (home-page "https://github.com/SWI-Prolog/sCASP")
    (synopsis "Interpreter for ASP programs with constraints")
    (description "@code{s(CASP)} is a top-down interpreter for ASP programs
with constraints.")
    (license license:asl2.0)))

(define-public ceres
  (package
    (name "ceres-solver")
    (version "2.0.0")
    (home-page "http://ceres-solver.org/")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "ceres-solver-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "00vng9vnmdb1qga01m0why90m0041w7bn6kxa2h4m26aflfqla8h"))))
    (build-system cmake-build-system)
    (arguments
     ;; TODO: Build HTML user documentation and install separately.
     '(#:configure-flags '("-DBUILD_EXAMPLES=OFF"
                           "-DBUILD_SHARED_LIBS=ON")

       #:phases (modify-phases %standard-phases
                  (add-before 'configure 'set-library-directory
                    (lambda _
                      ;; Install libraries to lib/, not lib64/.
                      (substitute* "CMakeLists.txt"
                        (("set\\(LIB_SUFFIX \"64\"\\)")
                         "set(LIB_SUFFIX \"\")")))))))
    (native-inputs (list pkg-config))
    ;; These inputs need to be propagated to satisfy dependent packages.
    (propagated-inputs (list eigen gflags glog))
    (inputs (list openblas suitesparse))
    (synopsis "C++ library for solving large optimization problems")
    (description
     "Ceres Solver is a C++ library for modeling and solving large,
complicated optimization problems.  It is a feature rich, mature and
performant library which has been used in production since 2010.  Ceres Solver
can solve two kinds of problems:
@enumerate
@item non-linear least squares problems with bounds constraints;
@item general unconstrained optimization problems.
@end enumerate\n")
    (license license:bsd-3)

    ;; Mark as tunable to take advantage of SIMD code in Eigen.
    (properties `((tunable? . #t)))))

(define-public ceres-solver-benchmarks
  (package
    (inherit ceres)
    (name "ceres-solver-benchmarks")
    (arguments
     '(#:modules ((ice-9 popen)
                  (ice-9 rdelim)
                  (guix build utils)
                  (guix build cmake-build-system))

       #:phases (modify-phases %standard-phases
                  (delete 'configure)
                  (replace 'build
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (bin (string-append out "/bin")))
                        (define flags
                          (string-tokenize
                           (read-line (open-pipe* OPEN_READ
                                                  "pkg-config" "eigen3"
                                                  "--cflags"))))

                        (define (compile-file top-dir)
                          (lambda (file)
                            (let ((source (string-append file ".cc")))
                              (format #t "building '~a'...~%" file)
                              (apply invoke "c++" "-fopenmp" "-O2" "-g" "-DNDEBUG"
                                     source "-lceres" "-lbenchmark" "-lglog"
                                     "-pthread"
                                     "-o" (string-append bin "/" file)
                                     "-I" top-dir flags))))

                        (mkdir-p bin)
                        (with-directory-excursion "internal/ceres"
                          (for-each (compile-file "..")
                                    '("schur_eliminator_benchmark"
                                      "small_blas_gemm_benchmark"
                                      "small_blas_gemv_benchmark"))
                          (with-directory-excursion "autodiff_benchmarks"
                            ((compile-file "../..") "autodiff_benchmarks"))))))
                  (delete 'check)
                  (delete 'install))))
    (inputs (modify-inputs (package-inputs ceres)
              (prepend googlebenchmark ceres)))
    (synopsis "Benchmarks of the Ceres optimization problem solver")))

;; For a fully featured Octave, users are strongly recommended also to install
;; the following packages: less, ghostscript, gnuplot.
(define-public octave-cli
  (package
    (name "octave-cli")
    (version "9.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/octave/octave-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0gbvrcblz6akpgm1vls7qjk97imq3j65pasd4jx9b7zpks813ygz"))))
    (build-system gnu-build-system)
    (inputs
     (list alsa-lib
           arpack-ng
           bdb
           curl
           fftw
           fftwf
           fltk
           fontconfig
           freetype
           gl2ps
           glpk
           glu
           graphicsmagick

           ;; TODO: libjpeg-turbo is indirectly required through libtiff.  In
           ;; the next rebuild cycle, add an absolute reference for -ljpeg in
           ;; libtiff.la instead of having to provide it here.
           libjpeg-turbo

           hdf5
           libsndfile
           libxft
           mesa
           openblas
           pcre
           portaudio
           qhull
           readline
           suitesparse
           zlib))
    (native-inputs
     (list gfortran
           pkg-config
           perl
           ;; The following inputs are not actually used in the build process.
           ;; However, the ./configure gratuitously tests for their existence and
           ;; assumes that programs not present at build time are also not, and
           ;; can never be, available at run time!  If these inputs are therefore
           ;; not present, support for them will be built out.  However, Octave
           ;; will still run without them, albeit without the features they
           ;; provide.
           less
           ghostscript
           gnuplot
           texinfo))
    ;; Octave code uses this variable to detect directories holding multiple CA
    ;; certificates to verify peers with.  This is required for the networking
    ;; functions that require encryption to work properly.
    (native-search-paths
     (list (search-path-specification
            (variable "CURLOPT_CAPATH")
            (files '("etc/ssl/certs")))))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-shell="
                            (assoc-ref %build-inputs "bash")
                            "/bin/sh")

             ;; XXX: Without this flag, linking octave-cli fails with
             ;; undefined references to 'logf@GLIBCXX_3.4' et.al. due to
             ;; not pulling in liboctinterp.la for -lstdc++.
             "--enable-link-all-dependencies")
       #:phases
       (modify-phases %standard-phases
         (add-after 'configure 'configure-makeinfo
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "libinterp/corefcn/help.h"
               (("\"makeinfo\"")
                (string-append
                 "\"" (assoc-ref inputs "texinfo") "/bin/makeinfo\""))))))))
    (home-page "https://www.gnu.org/software/octave/")
    (synopsis "High-level language for numerical computation (no GUI)")
    (description "GNU Octave is a high-level interpreted language that is
specialized for numerical computations.  It can be used for both linear and
non-linear applications and it provides great support for visualizing results.
Work may be performed both at the interactive command-line as well as via
script files.")
    (license license:gpl3+)))

(define-public octave
  (package (inherit octave-cli)
    (name "octave")
    (inputs
     `(("qscintilla" ,qscintilla)
       ("qt" ,qtbase-5)
       ,@(package-inputs octave-cli)))
    (native-inputs
     `(("qttools-5" , qttools-5) ;for lrelease
       ("texlive" ,(texlive-updmap.cfg (list texlive-epsf))) ; for texi2dvi
       ,@(package-native-inputs octave-cli)))
    (arguments
     (substitute-keyword-arguments (package-arguments octave-cli)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'patch-qscintilla-library-name
             (lambda* (#:key inputs #:allow-other-keys)
               ;; The QScintilla library that the Octave configure script tries
               ;; to link with should be named libqscintilla-qt5.so, but the
               ;; QScintilla input provides the shared library as
               ;; libqscintilla2_qt5.so.
               (substitute* "configure"
                 (("qscintilla2-qt5")
                  "qscintilla2_qt5"))
               #t))))))
    (synopsis "High-level language for numerical computation (with GUI)")))

(define-public opencascade-occt
  (package
    (name "opencascade-occt")
    (version "7.6.2")
    (properties
      '((release-tag-prefix . "^V")
        (release-tag-version-delimiter . "_")))
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://git.dev.opencascade.org/repos/occt.git")
              (commit
               (string-append "V"
                              (string-map (lambda (x) (if (eq? x #\.) #\_ x))
                                          version)))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "07z5d83vm9f50an7vhimzl7gbmri1dn6p2g999l5fgyaj5sg5f02"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            ;; Remove files specific to non-free operating systems.
            (delete-file-recursively "samples/ios")
            (delete-file-recursively "samples/mfc")
            (delete-file-recursively "samples/qt/FuncDemo")
            (delete-file "genconf.bat")
            (delete-file "gendoc.bat")
            (delete-file "genproj.bat")
            (delete-file "upgrade.bat")
            ;; Remove references to deleted files.
            (substitute* "dox/FILES_HTML.txt"
              ((".*standard.*") "" )
              ((".*UIKitSample.*") ""))
            #t))))
    (build-system cmake-build-system)
    (arguments
     '(;; There is no test target for make.  OCCT provides an
       ;; 'Automated Testing System', which may be accessed after
       ;; installation via the draw.sh script.  draw.sh is located in
       ;; the bin directory. For details see:
       ;; https://www.opencascade.com/doc/occt-7.3.0/overview/html/\
       ;; occt_dev_guides__tests.html
       #:tests? #f
       ;; Configure without freeimage: attempting to link against the
       ;; freeimage version 3.17 library leads to 'undefined
       ;; reference' errors.
       #:configure-flags
        (list "-DUSE_FREEIMAGE:BOOL=OFF"
              "-DUSE_TBB:BOOL=ON"
              "-DUSE_VTK:BOOL=OFF"
              "-DBUILD_DOC_Overview:BOOL=OFF"
              "-DCMAKE_EXPORT_NO_PACKAGE_REGISTRY=ON"
              "-DCMAKE_FIND_PACKAGE_NO_PACKAGE_REGISTRY=ON"
              "-UCMAKE_INSTALL_LIBDIR")))
    (native-inputs (list doxygen fontconfig))
    (inputs
     (list freetype
           ;("freeimage" ,freeimage)
           glu
           libxext
           libxi
           libxmu
           mesa
           tbb-2020
           tcl
           tk))
    ;; TODO: build Overview documentation and add 'doc' output.
    (home-page "https://www.opencascade.com")
    (synopsis "Libraries for 3D modeling and numerical simulation")
    (description
     "Open CASCADE is a set of libraries for the development of applications
dealing with 3D CAD data or requiring industrial 3D capabilities.  It includes
C++ class libraries providing services for 3D surface and solid modeling, CAD
data exchange, and visualization.  It is used for development of specialized
software dealing with 3D models in design (CAD), manufacturing (CAM),
numerical simulation (CAE), measurement equipment (CMM), and quality
control (CAQ) domains.

This is the certified version of the Open Cascade Technology (OCCT) library.")
    (license (list ;; OCCT library:
                   license:lgpl2.1; with an exception for the use of header
                                  ; files, see OCCT_LGPL_EXCEPTION.txt.
                   ;; Files src/OpenGl/glext.h, adm/cmake/cotire.cmake and
                   ;; src/OpenGl/OpenGl_HaltonSampler.hxx:
                   license:expat
                   ;; Files src/ExprIntrp/ExprIntrp.tab.* and
                   ;; src/StepFile/step.tab.*:
                   license:gpl3+  ; with Bison 2.2 exception.
                   ;; File src/NCollection/NCollection_UtfIterator.lxx:
                   (license:non-copyleft
                    "https://www.unicode.org/license.html")
                   ;; File src/NCollection/NCollection_StdAllocator.hxx:
                   license:public-domain))))

(define-public fast-downward
  (package
    (name "fast-downward")
    (version "23.06.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/aibasel/downward")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1xrgnvbkzkdf6srbrlsnf4qrgp0f1lkk7yxf34ynna0w49l468d4"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f        ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-driver
            (lambda* (#:key outputs #:allow-other-keys)
              (substitute* "driver/run_components.py"
                ;; strip gratuitous "bin"
                (("os\\.path\\.join\\((.*), \"bin\"\\)" all keep)
                 (string-append "os.path.join(" keep ")")))))
          (add-before 'configure 'chdir
            (lambda _ (chdir "src")))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (dest (string-append out "/libexec/fast-downward"
                                          "/builds/release")))
                (mkdir-p dest)
                (with-directory-excursion "bin"
                  (install-file "downward" dest)
                  (copy-recursively "translate"
                                    (string-append dest "/translate"))))))
          (add-after 'install 'install-driver
            (lambda* (#:key outputs #:allow-other-keys)
              (with-directory-excursion ".."
                (let* ((out (assoc-ref outputs "out"))
                       (bin (string-append out "/bin/fast-downward"))
                       (dest (string-append out "/libexec/fast-downward")))
                  (copy-recursively "driver"
                                    (string-append dest "/driver"))
                  (mkdir-p (dirname bin))
                  (copy-file "fast-downward.py" bin)
                  (wrap-program bin
                    `("PYTHONPATH" prefix (,dest))))))))))
    (inputs (list bash-minimal python))
    (home-page "https://www.fast-downward.org/")
    (synopsis "Domain-independent classical planning system")
    (description "Fast Downward is a portfolio-based planning system that
supports the propositional fragment of PDDL2.2.")
    (license license:gpl3+)))

(define-public gmsh
  (package
    (name "gmsh")
    (version "4.13.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.onelab.info/gmsh/gmsh.git")
             (commit
              (string-append "gmsh_"
                             (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16abxhadyyj7890lv6cdfxskg25w105pcpqvb5iwf6a59py8na8y"))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "contrib/metis"))))
    (build-system cmake-build-system)
    (propagated-inputs
     (list fltk
           gfortran
           glu
           gmp
           hdf5
           libx11
           libxext
           mesa
           metis
           openblas
           opencascade-occt))
    (inputs
     (list fontconfig
           libxft
           python))
    (arguments
     `(#:configure-flags `("-DENABLE_SYSTEM_CONTRIB:BOOL=ON"
                           "-DENABLE_BUILD_SHARED:BOOL=ON"
                           "-DENABLE_BUILD_DYNAMIC:BOOL=ON")
       #:imported-modules (,@%cmake-build-system-modules
                           (guix build python-build-system))
       #:modules (((guix build python-build-system) #:select (site-packages))
                  (guix build cmake-build-system)
                  (guix build utils))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-paths
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;; Use the standard Guix site-package path for
             ;; installation of the Python API.
             (substitute* "CMakeLists.txt"
               (("include\\(GNUInstallDirs\\)\n")
                (string-append "include(GNUInstallDirs)\n"
                               "  set(GMSH_PY_LIB "
                               (site-packages inputs outputs) ")\n"))
               (("\\$\\{GMSH\\_PY\\} DESTINATION \\$\\{GMSH\\_LIB\\}")
                "${GMSH_PY} DESTINATION ${GMSH_PY_LIB}"))
             ;; Find the shared library.
             (let ((libgmsh (string-append (assoc-ref outputs "out")
                                           "/lib/libgmsh.so")))
               (substitute* "api/gmsh.py"
                 (("find_library\\(\"gmsh\"\\)")
                  (simple-format #f "\"~a\"" libgmsh)))))))))
    (home-page "https://gmsh.info/")
    (synopsis "3D finite element grid generator")
    (description "Gmsh is a 3D finite element grid generator with a built-in
CAD engine and post-processor.  Its design goal is to provide a fast, light
and user-friendly meshing tool with parametric input and advanced
visualization capabilities.  Gmsh is built around four modules: geometry,
mesh, solver and post-processing.  The specification of any input to these
modules is done either interactively using the graphical user interface or in
ASCII text files using Gmsh's own scripting language.")
    (license license:gpl2+)))

(define-public veusz
  (package
    (name "veusz")
    (version "3.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "veusz" version))
       (sha256
        (base32 "1lcmcfr0dcam8g1fp5qip8jnxglxx7i62ln3ix6l4c2bbv21l5y2"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      ;; Tests currently fail with exception TypeError:
      ;; calling <function version ...> returned 3.6.2, not a test
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Veusz uses python's site-packages to look for pyqt5_include_dir.
          (add-after 'unpack 'fix-pyqt5-include-dir
            (lambda _
              (substitute* "pyqt_setuptools.py"
                (("get_path\\('platlib'\\)")
                 (format #f "~s"
                         (string-append
                          #$(this-package-input "python-pyqt")
                          "/lib/python"
                          #$(version-major+minor
                             (package-version python-wrapper))
                          "/site-packages"))))))
          ;; Ensure that icons are found at runtime.
          (add-after 'wrap 'wrap-executable
            (lambda* (#:key inputs #:allow-other-keys)
              (wrap-program (string-append #$output "/bin/veusz")
                `("QT_PLUGIN_PATH" prefix
                  ,(list (string-append
                          (string-join
                           (list #$(this-package-input "qtbase")
                                 #$(this-package-input "qtsvg")
                                 #$(this-package-input "qtwayland"))
                           "/lib/qt5/plugins:")
                          "/lib/qt5/plugins")))))))))
    (native-inputs
     (list pkg-config
           python-astropy
           python-setuptools
           python-wheel
           qttools-5))
    (inputs
     (list bash-minimal
           ghostscript ;optional, for EPS/PS output
           python-dbus
           python-h5py ;optional, for HDF5 data
           python-pyqt
           qtbase-5
           qtsvg-5
           qtwayland-5))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://veusz.github.io/")
    (synopsis "Scientific plotting package")
    (description
     "Veusz is a scientific plotting and graphing program with a graphical
user interface, designed to produce publication-ready 2D and 3D plots.  In
addition it can be used as a module in Python for plotting.  It supports
vector and bitmap output, including PDF, Postscript, SVG and EMF.")
    (license license:gpl2+)))

(define-public maxflow
  (package
    (name "maxflow")
    ;; Versioning is ambiguous: the git tag matching this commit is ‘3.0.5’,
    ;; which matches CMakeLists.txt, but README.md and CHANGES say ‘3.04’.
    (version "3.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/gerddie/maxflow")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0rll38whw55h0vcjrrwdnh9ascvxby0ph7n1l0d12z17cg215kkb"))))
    (build-system cmake-build-system)
    (home-page "https://pub.ist.ac.at/~vnk/software.html")
    (synopsis "Library implementing Maxflow algorithm")
    (description "An implementation of the maxflow algorithm described in
@cite{An Experimental Comparison of Min-Cut/Max-Flow Algorithms for
Energy Minimization in Computer Vision.\n
Yuri Boykov and Vladimir Kolmogorov.\n
In IEEE Transactions on Pattern Analysis and Machine Intelligence,\n
September 2004}")
    (license license:gpl3+)))

(define-public petsc
  (package
    (name "petsc")
    (version "3.21.4")
    (source
     (origin
      (method url-fetch)
      ;; The *-lite-* tarball does not contain the *large* documentation
      (uri (string-append "https://web.cels.anl.gov/projects/petsc/download/release-snapshots/"
                          "petsc-lite-" version ".tar.gz"))
      (sha256
       (base32 "1394ybnchawb2kghx4xk36gw26930aa73lxyw96diiqp8rnhgbm9"))))
    (outputs '("out"                    ; libraries and headers
               "examples"))             ; ~30MiB of examples
    (build-system gnu-build-system)
    (native-inputs
     (list python which))
    (inputs
     (list gfortran openblas superlu
           ;; leaving out hdf5 and fftw, as petsc expects them to be built with mpi
           ;; leaving out opengl, as configuration seems to only be for mac
           ))
    (arguments
     (list
      #:test-target "test"
      #:parallel-build? #f             ; build is parallel by default
      #:configure-flags
      #~(list "COPTFLAGS=-g -O3"
              "CXXOPTFLAGS=-g -O3"
              "FOPTFLAGS=-g -O3"
              "--with-mpi=0"
              "--with-openmp=1"
              "--with-openblas=1"
              (string-append "--with-openblas-dir="
                             #$(this-package-input "openblas"))
              "--with-superlu=1"
              "--with-debugging=0")
      #:make-flags
      ;; Honor (parallel-job-count) for build.  Do not use --with-make-np,
      ;; whose value is dumped to $out/lib/petsc/conf/petscvariables.
      #~(list (format #f "MAKE_NP=~a" (parallel-job-count)))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            ;; PETSc's configure script is actually a python script, so we can't
            ;; run it with bash.
            (lambda* (#:key outputs (configure-flags '())
                      #:allow-other-keys)
              (let* ((prefix (assoc-ref outputs "out"))
                     (flags `(,(string-append "--prefix=" prefix)
                              ,@configure-flags)))
                ;; Sort source files in configure (for reproducibility).
                (substitute* "config/gmakegen.py"
                  (("join\\(srcs\\[lang\\]\\)")
                   "join(sorted(srcs[lang]))"))

                (format #t "build directory: ~s~%" (getcwd))
                (format #t "configure flags: ~s~%" flags)
                (apply invoke "./configure" flags)

                ;; Generate test scripts with the right shebang.
                (substitute* "config/example_template.py"
                  (("#!/usr/bin/env bash")
                   (string-append "#!" (which "bash")))))))
          (add-after 'configure 'clean-local-references
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (substitute* (find-files "." "^petsc(conf|machineinfo).h$")
                  ;; Prevent build directory from leaking into compiled code
                  (((getcwd)) out)
                  (("Machine characteristics: Linux-[0-9]+\\.[0-9]+\\.[0-9]+-arch[0-9]+-[0-9]+")
                    "Machine characteristics: Linux-x.x.x-archx-x")
                  (("([[:graph:]]+)/bin/make") "\"make"))
                (substitute* (find-files "." "petscvariables")
                  ;; Do not expose build machine characteristics, set to defaults.
                  (("MAKE_NP = [[:digit:]]+") "MAKE_NP = 2")
                  (("MAKE_TEST_NP = [[:digit:]]+") "MAKE_TEST_NP = 2")
                  (("MAKE_LOAD = .*") "MAKE_LOAD = 256.0\n")
                  (("NPMAX = [[:digit:]]+") "NPMAX = 2")))))
          (add-after 'install 'clean-install
            ;; Try to keep installed files from leaking build directory names.
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (substitute* (map (lambda (file)
                                    (string-append out "/lib/petsc/conf/" file))
                                  '("petscvariables"))
                  (((getcwd)) out))
                ;; Make compiler references point to the store
                (substitute* (string-append out "/lib/petsc/conf/petscvariables")
                  (("= (gcc|g\\+\\+|gfortran)" _ compiler)
                   (string-append "= " (which compiler))))
                ;; PETSc installs some build logs, which aren't necessary.
                (for-each (lambda (file)
                            (let ((f (string-append out "/lib/petsc/conf/" file)))
                              (when (file-exists? f)
                                (delete-file f))))
                          '("configure.log" "make.log" "gmake.log"
                            "test.log" "error.log" "RDict.db"
                            "PETScBuildInternal.cmake"
                            "configure-hash"
                            ;; Once installed, should uninstall with Guix
                            "uninstall.py")))))
          (add-after 'clean-install 'clear-reference-to-compiler
            (lambda* (#:key inputs outputs #:allow-other-keys)
              ;; Do not retain a reference to GCC and other build only inputs.
              (let ((out (assoc-ref outputs "out")))
              (substitute* (string-append out "/lib/petsc/conf/petscvariables")
                (("([[:graph:]]+)/bin/gcc") "gcc")
                (("([[:graph:]]+)/bin/g\\+\\+") "g++")
                (("([[:graph:]]+)/bin/make") "make")
                (("([[:graph:]]+)/bin/diff") "diff")
                (("([[:graph:]]+)/bin/sed") "sed")
                (("([[:graph:]]+)/bin/gfortran") "gfortran")
                (("([[:graph:]]+)/bin/gcov") "gcov")))))

          ;; Some of the tests get linked with '-L$prefix/lib -lpetsc' (even
          ;; though that's unnecessary because they also explicitly link
          ;; against 'libpetsc.so' from the build directory).  To work around
          ;; it, run tests after installation.  See
          ;; <https://gitlab.com/petsc/petsc/-/issues/1634>.
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check))

          (add-after 'install 'move-examples
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (examples (assoc-ref outputs "examples"))
                     (exdir (string-append out "/share/petsc/examples"))
                     (exdir' (string-append examples "/share/petsc/examples")))
                (copy-recursively exdir exdir')
                (delete-file-recursively exdir)))))))
    (home-page "https://petsc.org")
    (synopsis "Library to solve PDEs")
    (description "PETSc, pronounced PET-see (the S is silent), is a suite of
data structures and routines for the scalable (parallel) solution of
scientific applications modeled by partial differential equations.")
    (license (license:non-copyleft
              "https://www.mcs.anl.gov/petsc/documentation/copyright.html"))
    (properties '((tunable? . #t)))))

(define-public petsc-complex
  (package
    (inherit petsc)
    (name "petsc-complex")
    (arguments
     (substitute-keyword-arguments (package-arguments petsc)
       ((#:configure-flags cf)
        #~(cons "--with-scalar-type=complex" #$cf))))
    (synopsis "Library to solve PDEs (with complex scalars)")))

(define-public petsc-openmpi
  (package
    (inherit petsc)
    (name "petsc-openmpi")
    (inputs
     (modify-inputs (package-inputs petsc)
       (prepend hdf5-parallel-openmpi
                hypre-openmpi
                metis
                mumps-openmpi
                openmpi
                scalapack
                pt-scotch32
                `(,pt-scotch32 "metis"))))
    (arguments
     (substitute-keyword-arguments (package-arguments petsc)
       ((#:configure-flags cf)
        #~`("--with-hypre=1"
            "--with-mpiexec=mpirun"
            "--with-metis=1"
            "--with-mumps=1"
            "--with-scalapack=1"
            "--with-ptscotch=1"
            ,(string-append "--with-mpi-dir="
                            #$(this-package-input "openmpi"))
            ,(string-append "--with-hdf5-include="
                            #$(this-package-input "hdf5-parallel-openmpi")
                            "/include")
            ,(string-append "--with-hdf5-lib="
                            #$(this-package-input "hdf5-parallel-openmpi")
                            "/lib/libhdf5.a")
            ,@(delete "--with-mpi=0" #$cf)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'adjust-pt-scotch-library-names
              (lambda _
                ;; Adjust to the library name changes in Scotch 7.0.
                (substitute* "config/BuildSystem/config/packages/PTScotch.py"
                  (("libptesmumps") "libesmumps")
                  (("libptscotchparmetis") "libptscotchparmetisv3"))))
            (add-before 'configure 'mpi-setup
              #$%openmpi-setup)
            (add-after 'install 'patch-header-inclusions
              ;; TODO: Replace with ‘patch-header-inclusions’ when (some form
              ;; of) https://issues.guix.gnu.org/54780#19 is merged.
              (lambda _
                (substitute* (string-append #$output "/include/petsclayouthdf5.h")
                  (("<(H5Ipublic.h)>" _ header)
                   (format #f "<~a/include/~a>"
                           #$(this-package-input "hdf5-parallel-openmpi")
                           header)))))))))
    (synopsis "Library to solve PDEs (with MUMPS and MPI support)")))

(define-public petsc-complex-openmpi
  (package
    (inherit petsc-complex)
    (name "petsc-complex-openmpi")
    (inputs
     (modify-inputs (package-inputs petsc-complex)
       (prepend openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments petsc-complex)
       ((#:configure-flags cf)
        #~`("--with-mpiexec=mpirun"
            ,(string-append "--with-mpi-dir="
                            #$(this-package-input "openmpi"))
            ,@(delete "--with-mpi=0" #$cf)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'configure 'mpi-setup
              #$%openmpi-setup)))))
    (synopsis "Library to solve PDEs (with complex scalars and MPI support)")))

(define-public pnetcdf
  (package
    (name "pnetcdf")
    (version "1.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://parallel-netcdf.github.io/Release/pnetcdf-"
             version ".tar.gz"))
       (sha256
        (base32
         "14f4nbcnw80y59cl0kjpxqqfaxzzd62kixnhb6ihp6aigb3z385b"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-shared"
                   (string-append "--with-mpi=" #$(this-package-input "openmpi")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'build 'mpi-setup
                 #$%openmpi-setup))))
    (inputs (list openmpi zlib))
    (native-inputs (list m4))
    (home-page "https://parallel-netcdf.github.io/")
    (synopsis "Parallel I/O Library for NetCDF File Access")
    (description "PnetCDF is a high-performance parallel I/O library for accessing
Unidata's NetCDF, files in classic formats, specifically the formats of CDF-1, 2, and
5.")
    (license (license:x11-style "file://COPYRIGHT"))))

(define-public popf
  (package
    (name "popf")
    (version "0.0.15")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/fmrico/popf")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i1am3h6japn8fgapi5s5mnyrm31a05jkjhzgk48cd2n42c5060v"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f                       ; no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-cmake
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* (find-files "." "CMakeLists\\.txt")
                (("/usr/local/opt/flex/include")
                 (dirname (search-input-file inputs "include/FlexLexer.h"))))
              (substitute* "CMakeLists.txt"
                (("find_package\\(ament_cmake REQUIRED\\)") "")
                (("ament_.*") "")
                (("(RUNTIME DESTINATION) .*" all dst)
                 (string-append dst " libexec/${PROJECT_NAME}")))))
          (add-after 'install 'symlink
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (mkdir-p (string-append out "/bin"))
                (for-each (lambda (link)
                            (symlink
                             (string-append out "/libexec/popf/" (cdr link))
                             (string-append out "/bin/" (car link))))
                          '(("popf" . "popf") ("VAL" . "validate")))))))))
    (inputs (list cbc flex))
    (native-inputs (list flex bison perl))
    (home-page "https://github.com/fmrico/popf")
    (synopsis "Forward-chaining temporal planner")
    (description "This package contains an implementation of the @acronym{POPF,
Partial Order Planning Forwards} planner described in @cite{Forward-Chaining
Partial Order Planning}, that has been updated to compile with newer C++
compilers.")
    (license license:gpl2+)))

(define-public ppl
  (package
    (name "ppl")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.bugseng.com/products/ppl/download/"
                           "ftp/releases/" version
                           "/ppl-" version ".tar.gz"))
       (sha256
        (base32
         "1j5aji1g2vmdvc0gqz45n2ll2l2f6czca04wiyfl5g3sm3a6vhvb"))))
    (build-system gnu-build-system)
    (native-inputs
     (list m4))
    (inputs
     (list glpk gmp))
    (home-page "https://www.bugseng.com/parma-polyhedra-library")
    (synopsis
     "Parma Polyhedra Library for computations with polyhedra")
    (description
     "The Parma Polyhedra Library (PPL) provides numerical abstractions
especially targeted at applications in the field of analysis and
verification of complex systems.  These abstractions include convex
polyhedra, defined as the intersection of a finite number of (open or
closed) halfspaces, each described by a linear inequality (strict or
non-strict) with rational coefficients; some special classes of polyhedra
shapes that offer interesting complexity/precision tradeoffs; and grids
which represent regularly spaced points that satisfy a set of linear
congruence relations.  The library also supports finite powersets and
products of (any kind of) polyhedra and grids, a mixed integer linear
programming problem solver using an exact-arithmetic version of the simplex
algorithm, a parametric integer programming solver, and primitives for
termination analysis via the automatic synthesis of linear ranking
functions.")
    (license license:gpl3+)))

(define-public primecount
  (package
    (name "primecount")
    (version "7.14")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kimwalisch/primecount")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "097p3wfq6ds56275cra678hzg8cp2vd1ccllsi8wczrf0qvq91rp"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_LIBPRIMESIEVE=OFF"
                                     "-DBUILD_MANPAGE=ON"
                                     "-DBUILD_SHARED_LIBS=ON"
                                     "-DBUILD_STATIC_LIBS=OFF"
                                     "-DBUILD_TESTS=ON")))
    (native-inputs
     (list asciidoc))
    (inputs
     (list primesieve))
    (home-page "https://github.com/kimwalisch/primecount")
    (synopsis "Fast prime counting function implementations")
    (description "@code{primecount} is a command-line program and C/C++
library that counts the number of primes ≤ x (maximum 1031) using highly
optimized implementations of the combinatorial prime counting algorithms.")
    (license license:bsd-2)))

(define-public primesieve
  (package
    (name "primesieve")
    (version "12.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kimwalisch/primesieve")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lxvs1jgch0zgpa5axx6zlvgab4rmm3lqpbah75072xpj8ndhhld"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_STATIC_LIBS=off"
                                     "-DBUILD_TESTS=ON")))
    (home-page "https://github.com/kimwalisch/primesieve")
    (synopsis "Prime number generator")
    (description "@code{primesieve} is a command-line program and C/C++
 library for quickly generating prime numbers.  It is very cache efficient,
 it detects your CPU's L1 & L2 cache sizes and allocates its main data
 structures accordingly.  It is also multi-threaded by default, it uses all
 available CPU cores whenever possible i.e. if sequential ordering is not
 required. primesieve can generate primes and prime k-tuplets up to 264.")
    (license license:bsd-2)))

(define-public python-accupy
  (package
    (name "python-accupy")
    (version "0.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diego-hayashi/accupy")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sxkwpp2xy2jgakhdxr4nh1cspqv8l89kz6s832h05pbpyc0n767"))
       (patches (search-patches "python-accupy-use-matplotx.patch"
                                "python-accupy-fix-use-of-perfplot.patch"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-eigen-include-dir
            (lambda _
              (substitute* "setup.py"
                (("include_dirs=\\[\"\\/usr\\/include\\/eigen3\\/\"\\]," _)
                 (string-append "include_dirs=[\""
                                #$(file-append (this-package-input "eigen")
                                               "/include/eigen3/")
                                "\"],"))))))))
    (native-inputs
     (list pybind11
           python-matplotx
           python-perfplot
           python-pytest
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list eigen
           python-mpmath
           python-pyfma))
    (home-page "https://github.com/diego-hayashi/accupy")
    (synopsis "Accurate calculation of sums and dot products")
    (description
     "@code{accupy} is a Python library for accurately computing sums
and (dot) products.  It implements Kahan summation, Shewchuck's algorithm and
summation in K-fold precision.")
    (license license:gpl3+)))

;; It is unfortunate that we cannot just link with the existing blis package.
(define-public python-blis
  (package
    (name "python-blis")
    (version "0.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "blis" version))
              (sha256
               (base32
                "0vrnzk9jx7fcl56q6zpa4w4mxkr4iknxs42fngn9g78zh1kc9skw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      '(modify-phases %standard-phases
         (add-after 'build 'build-ext
           (lambda _
             (invoke "python" "setup.py" "build_ext" "--inplace"
                     "-j" (number->string (parallel-job-count))))))))
    (propagated-inputs
     (list python-numpy))
    (native-inputs
     (list python-cython
           python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/explosion/cython-blis")
    (synopsis "Blis as a self-contained C-extension for Python")
    (description
     "This package provides the Blis BLAS-like linear algebra library, as a
self-contained C-extension for Python.")
    (license license:bsd-3)))

(define-public python-blis-for-thinc
  (package
    (inherit python-blis)
    (name "python-blis")
    (version "0.7.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "blis" version))
              (sha256
               (base32
                "0mvcif9g69424bk8xiflacxzpvz802ns791v2r8a6fij0sxl3mgp"))))))

(define-public python-cvxopt
  (package
    (name "python-cvxopt")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/cvxopt/cvxopt")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vdfag3rr906w0gk7vxm2yxfy8y92i4wmqxi82cbykpfp5r82i36"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'find-libraries
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CVXOPT_BLAS_LIB" "openblas")
             (setenv "CVXOPT_LAPACK_LIB" "openblas")
             (setenv "CVXOPT_BUILD_FFTW" "1")
             (setenv "CVXOPT_BUILD_GLPK" "1")
             (setenv "CVXOPT_BUILD_GSL" "1")
             #t)))))
    (inputs
     (list fftw
           glpk
           gsl
           openblas
           suitesparse))
    (home-page "https://www.cvxopt.org")
    (synopsis "Python library for convex optimization")
    (description
     "CVXOPT is a package for convex optimization based on the Python
programming language.  Its main purpose is to make the development of software
for convex optimization applications straightforward by building on Python’s
extensive standard library and on the strengths of Python as a high-level
programming language.")
    (license license:gpl3+)))

(define-public python-ducc0
  (package
    (name "python-ducc0")
    (version "0.37.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.mpcdf.mpg.de/mtr/ducc")
             (commit (string-append
                      "ducc0_" (string-replace-substring version "." "_")))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pckbip2ffmiwm73wrpvif3gy0a09v9b9kbyallp520l6l69n4k8"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      #~(list "--numprocesses" (number->string (parallel-job-count))
              "python/test")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'set-env
            (lambda _
              (setenv "DUCC0_OPTIMIZATION" "portable-strip"))))))
    (native-inputs
     (list cmake-minimal
           pybind11
           python-nanobind
           python-pytest
           python-pytest-xdist
           python-scikit-build-core
           python-setuptools))
    (propagated-inputs
     (list python-numpy))
    (home-page "https://gitlab.mpcdf.mpg.de/mtr/ducc")
    (synopsis "Distinctly Useful Code Collection")
    (description
     "This package provides a collection of basic programming tools for
numerical computation, including Fast Fourier Transforms, Spherical Harmonic
Transforms, non-equispaced Fourier transforms, as well as some concrete
applications like 4pi convolution on the sphere and gridding/degridding of
radio interferometry data.")
    (license license:gpl2+)))

(define-public python-kiwisolver
  (package
    (name "python-kiwisolver")
    (version "1.4.6")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "kiwisolver" version))
              (sha256
               (base32
                "1q77r6cl9m4vh8lsvmsm1ijh3r9aijdhmnc03yhnlpj407b2kniw"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-cppy
           python-pytest
           python-setuptools
           python-setuptools-scm
           python-wheel))
    (propagated-inputs
     (list python-typing-extensions))
    (home-page "https://github.com/nucleic/kiwi")
    (synopsis "Fast implementation of the Cassowary constraint solver")
    (description
     "Kiwi is an efficient C++ implementation of the Cassowary constraint
solving algorithm.  Kiwi has been designed from the ground up to be
lightweight and fast.  Kiwi ranges from 10x to 500x faster than the original
Cassowary solver with typical use cases gaining a 40x improvement.  Memory
savings are consistently > 5x.")
    (license license:bsd-3)))

(define-public python-libensemble
  (package
    (name "python-libensemble")
    (version "1.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "libensemble" version))
       (sha256
        (base32 "0qxb0sn624jaxjxg2ayd65zaiq1p043w3kk55w8r6drkjiar70yj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'relax-psutil
            (lambda _
              (substitute* "setup.py"
                (("psutil>=5.9.4") "psutil>=5.9.2"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; These files require MPI and call subprocesses.
                (delete-file
                 "libensemble/tests/unit_tests/test_executor.py")
                (delete-file
                 "libensemble/tests/unit_tests/test_executor_gpus.py")
                ;; This file has one failing MPI test but since tests run from
                ;; a shell script, they can't be disabled individually.
                ;; Failing test: 'test_ensemble_prevent_comms_overwrite'
                (delete-file "libensemble/tests/unit_tests/test_ensemble.py")
                (setenv "TERM" "xterm")
                ;; A very bad way to skip another MPI test.
                (substitute* "libensemble/tests/run-tests.sh"
                  (("export UNIT_TEST_MPI_SUBDIR=.*")
                   "export UNIT_TEST_MPI_SUBDIR=''"))
                ;; Run only unit tests, regression tests require MPI.
                (invoke "bash" "libensemble/tests/run-tests.sh" "-u")))))))
    (native-inputs (list ncurses
                         python-mock
                         python-mpi4py
                         python-pytest
                         python-pytest-cov
                         python-pytest-timeout
                         python-setuptools
                         python-wheel))
    (propagated-inputs (list python-mpmath
                             python-numpy
                             python-psutil
                             python-pydantic-2
                             python-pyyaml
                             python-tomli))
    (home-page "https://github.com/Libensemble/libensemble")
    (synopsis "Toolkit for dynamic ensembles of calculations")
    (description "@code{libensemble} is a complete toolkit for dynamic
ensembles of calculations.  It connects @code{deciders} to experiments or
simulations.")
    (license license:bsd-3)))

(define-public python-ndim
  (package
    (name "python-ndim")
    (version "0.1.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diego-hayashi/ndim")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hri82k7pcpw9dns8l1f2asa3dm7hjv71wnxi3752258ia2qa44v"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-sympy))
    (native-inputs (list python-flit-core python-pytest))
    (home-page "https://github.com/diego-hayashi/ndim")
    (synopsis "Multidimensional volumes and monomial integrals")
    (description
      "@code{ndim} computes all kinds of volumes and integrals of
monomials over such volumes in a fast, numerically stable way, using
recurrence relations.")
    (license license:gpl3+)))

(define-public python-orthopy
  (package
    (name "python-orthopy")
    (version "0.9.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/diego-hayashi/orthopy")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00s2rwjdlq38zkf7wl1gvm2aw057r30266lkzfxkrfzr4i705xnq"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; These tests fails with unexpected keyword arguments
      ;; in calls to cplot.
      #~(list "--deselect" "tests/test_u3.py::test_write_single"
              "--deselect" "tests/test_u3.py::test_write_tree"
              "-k" (string-join
                    ;; Tests fail in arrays comprising.
                    (list "not test_chebyshev1_p11[2-y2]"
                          "test_chebyshev1_p11[4-y4]"
                          "test_eval[1-ref1]"
                          "test_eval[t2-ref2]"
                          "test_eval[t3-ref3]")
                    " and not "))))
    (native-inputs
     (list python-matplotx
           python-meshio
           python-meshzoo
           python-pytest
           python-scipy
           python-setuptools
           python-wheel))
    (propagated-inputs
     (list python-importlib-metadata
           python-ndim
           python-numpy
           python-sympy))
    (home-page "https://github.com/diego-hayashi/orthopy")
    (synopsis "Tools for orthogonal polynomials, Gaussian quadrature")
    (description "@code{orthopy} provides various orthogonal polynomial
classes for lines, triangles, quadrilaterals, disks, spheres, hexahedra,
and n-cubes.  All computations are done using numerically stable
recurrence schemes.  Furthermore, all functions are fully vectorized and
can return results in exact arithmetic.")
    (license license:gpl3+)))

(define-public python-petsc4py
  (package
    (name "python-petsc4py")
    (version "3.21.4")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "petsc4py" version))
        (sha256
          (base32
           "1kffxhcwkx6283n2p83ymanz6m8j2xmz5kpa5s8qc4f9iiah59sb"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'set-PETSC_DIR
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Define path to PETSc installation.
                   (setenv "PETSC_DIR"
                           (assoc-ref inputs "petsc-openmpi"))))
               (add-before 'check 'mpi-setup
                 #$%openmpi-setup)
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python" "test/runtests.py")))))))
    (native-inputs (list python-cython-3))
    (inputs (list petsc-openmpi python-numpy))
    (home-page "https://bitbucket.org/petsc/petsc4py/")
    (synopsis "Python bindings for PETSc")
    (description "PETSc, the Portable, Extensible Toolkit for
Scientific Computation, is a suite of data structures and routines for
the scalable (parallel) solution of scientific applications modeled by
partial differential equations.  It employs the MPI standard for all
message-passing communication.  @code{petsc4py} provides Python
bindings to almost all functions of PETSc.")
    (license license:bsd-3)))

(define-public python-primecountpy
  (package
    (name "python-primecountpy")
    (version "0.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "primecountpy" version))
       (sha256
        (base32 "0xh6zx5zw5scy7jygqirks9y6z4zyfm0zjfp8nd6dw0m471przkq"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:tests? #f)) ; there are no tests
    (native-inputs
     (list python-cysignals
           python-cython
           python-setuptools
           python-wheel))
    (inputs
     (list pari-gp
           primecount
           primesieve))
    (home-page "https://github.com/dimpase/primecountpy")
    (synopsis "Cython interface for C++ primecount library")
    (description "This package provides a Cython interface to the C++ library
@code{primecount}.")
    ;; pyproject.toml says gpl3 but file headers say gpl2+, see
    ;; <https://github.com/dimpase/primecountpy/issues/16>.
    (license license:gpl2+)))

(define-public python-pyglm
  (package
    (name "python-pyglm")
    (version "2.8.1")
    (source
     (origin
       ;; Test files are not included in the archive in pypi.
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Zuzu-Typ/PyGLM")
             (commit version)
             ;; XXX: Attempt to use Guix's glm@1.0.1 failed, try to figure out
             ;; how to fix it.
             (recursive? #t)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1ra54m0pb5aca7q6ymappjsyxdzdy17yz8rrhlql04k0p9lnf1v8"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel))
    (home-page "https://github.com/Zuzu-Typ/PyGLM")
    (synopsis "OpenGL Mathematics library for Python")
    (description "PyGLM is a Python extension library which brings the OpenGL
Mathematics (GLM) library to Python.")
    (license license:zlib)))

(define-public python-quadpy
  (package
    (name "python-quadpy")
    (version "0.16.10")
    (source
      (origin
        (method url-fetch)
        ; Download zipfile from zenodo, because git checkout is missing
        ; some data files that are stored via git-lfs.
        (uri (string-append
               "https://zenodo.org/records/5541216/files/nschloe/quadpy-v"
               version
               ".zip"))
        (sha256
          (base32
            "1f989dipv7lqxvalfrvvlmhlxyl67a87lavyyqrr1mh88glhl592"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:test-flags
      ;; AssertionError: Lebedev(047) -- observed: 41, expected: 47 (max err:
      ;; 4.910e-15).
      #~(list "--deselect=tests/test_u3.py::test_scheme_spherical[lebedev_047]")))
    (native-inputs
     (list python-accupy
           python-pytest
           python-setuptools
           python-wheel
           unzip
           vtk))
    (propagated-inputs
      (list python-importlib-metadata
            python-numpy
            python-orthopy
            python-scipy
            python-sympy))
    (home-page "https://github.com/diego-hayashi/quadpy")
    (synopsis "Numerical integration, quadrature for various domains")
    (description
      "More than 1500 numerical integration schemes for line segments, circles,
disks, triangles, quadrilaterals, spheres, balls, tetrahedra, hexahedra,
wedges, pyramids, n-spheres, n-balls, n-cubes, n-simplices, and the
1D/2D/3D/nD spaces with weight functions exp(-r) and exp(-r2) for fast
integration of real-, complex-, and vector-valued functions.")
    (license license:gpl3+)))

(define-public slepc
  (package
    (name "slepc")
    (version "3.21.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://slepc.upv.es/download/distrib/slepc-"
                           version ".tar.gz"))
       (sha256
        (base32
         "12kdgnw9lm5q6bq5wp27ygdp1bjdz3fhkb8m9ds83kn32l53zcxy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python)
       ("which" ,which)
       ("petsc:examples" ,petsc "examples"))) ;for gmakegen.py script
    (inputs
     `(("arpack" ,arpack-ng)
       ("gfortran" ,gfortran)))
    (propagated-inputs
     (list petsc))
    (arguments
     `(#:parallel-build? #f             ;build is parallel by default
       #:configure-flags
       `(,(string-append "--with-arpack-dir="
                         (assoc-ref %build-inputs "arpack") "/lib"))
       #:make-flags                     ;honor (parallel-job-count)
       `(,(format #f "MAKE_NP=~a" (parallel-job-count))
         ,(string-append "PETSCCONFIGDIR="
                         (assoc-ref %build-inputs "petsc:examples")
                         "/share/petsc/examples/config"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
          ;; configure is a python script, so we can't run it with bash.
          (lambda* (#:key inputs outputs (configure-flags '())
                    #:allow-other-keys)
            (let* ((prefix (assoc-ref outputs "out"))
                   (flags `(,(string-append "--prefix=" prefix)
                            ,@configure-flags)))
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (setenv "SLEPC_DIR" (getcwd))
              (setenv "PETSC_DIR" (assoc-ref inputs "petsc"))
              (apply invoke "./configure" flags))))
         (add-after 'install 'delete-doc
          ;; TODO: SLEPc installs HTML documentation alongside headers in
          ;; $out/include.  We'd like to move them to share/doc, but delete
          ;; them for now, as they are incomplete and installing the complete
          ;; documentation is difficult.
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out")))
              (for-each delete-file (find-files out "\\.html$"))
              #t)))
         (add-after 'install 'clean-install
          ;; Clean up unnecessary build logs from installation.
          (lambda* (#:key outputs #:allow-other-keys)
            (let ((out (assoc-ref outputs "out")))
              (for-each (lambda (file)
                          (let ((f (string-append out "/lib/slepc/conf/" file)))
                            (when (file-exists? f)
                              (delete-file f))))
                        '("configure.log" "make.log" "gmake.log"
                          "test.log" "error.log" "RDict.db"
                          "uninstall.py"))
              #t))))))
    (home-page "https://slepc.upv.es")
    (synopsis "Scalable library for eigenproblems")
    (description "SLEPc is a software library for the solution of large sparse
eigenproblems on parallel computers.  It can be used for the solution of
linear eigenvalue problems formulated in either standard or generalized form,
as well as other related problems such as the singular value decomposition.
The emphasis of the software is on methods and techniques appropriate for
problems in which the associated matrices are sparse, for example, those
arising after the discretization of partial differential equations.")
    (license license:bsd-2)
    (properties
     `((release-monitoring-url . "http://slepc.upv.es/download/")))))

(define-public slepc-complex
  (package (inherit slepc)
    (name "slepc-complex")
    (propagated-inputs
     `(("petsc" ,petsc-complex)
       ,@(alist-delete "petsc" (package-propagated-inputs slepc))))
    (synopsis "Scalable library for eigenproblems (with complex scalars)")))

(define-public slepc-openmpi
  (package (inherit slepc)
    (name "slepc-openmpi")
    (arguments
     (substitute-keyword-arguments (package-arguments slepc)
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)))))
    (inputs
     `(("mpi" ,openmpi)
       ,@(alist-delete "arpack" (package-inputs slepc))))
    (propagated-inputs
     `(("petsc" ,petsc-openmpi)
       ("arpack" ,arpack-ng-openmpi)
       ,@(alist-delete "petsc" (package-propagated-inputs slepc))))
    (synopsis "Scalable library for eigenproblems (with MPI support)")))

(define-public slepc-complex-openmpi
  (package (inherit slepc-openmpi)
    (name "slepc-complex-openmpi")
    (propagated-inputs
     `(("petsc" ,petsc-complex-openmpi)
       ,@(alist-delete "petsc" (package-propagated-inputs slepc-openmpi))))
    (synopsis "Scalable library for eigenproblems (with complex scalars and MPI support)")))

(define-public python-slepc4py
  (package
    (name "python-slepc4py")
    (version "3.21.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "slepc4py" version))
        (sha256
          (base32
            "01vvpl8g73knkwnh6mbxd45vwcs4zsw814147fvgkvj30qkhx3mw"))))
    (build-system python-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'pre-build
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; Define path to PETSc installation.
                   (setenv "PETSC_DIR" (assoc-ref inputs "petsc-openmpi"))
                   ;; Define path to SLEPc installation.
                   (setenv "SLEPC_DIR" (assoc-ref inputs "slepc-openmpi"))))
               (add-before 'check 'mpi-setup
                 #$%openmpi-setup)
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "python" "test/runtests.py")))))))
    (native-inputs (list python-cython-3))
    (inputs (list python-numpy python-petsc4py petsc-openmpi slepc-openmpi))
    (home-page "https://bitbucket.org/slepc/slepc4py/")
    (synopsis "Python bindings for SLEPc")
    (description "SLEPc, the Scalable Library for Eigenvalue Problem
Computations, is based on PETSc, the Portable, Extensible Toolkit for
Scientific Computation.  It employs the MPI standard for all
message-passing communication.  @code{slepc4py} provides Python
bindings to almost all functions of SLEPc.")
    (license license:bsd-3)))

(define-public metamath
  (package
    (name "metamath")
    (version "0.193")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/metamath/metamath-exe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1s9hyknfvhj86g3giayyf3dxzg23iij0rs7bdvj075v9qbyhqn9b"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake))
    (home-page "https://us.metamath.org/")
    (synopsis "Proof verifier based on a minimalistic formalism")
    (description
     "Metamath is a tiny formal language and that can express theorems in
abstract mathematics, with an accompyaning @command{metamath} executable that
verifies databases of these proofs.  There is a public database,
@url{https://github.com/metamath/set.mm, set.mm}, implementing first-order
logic and Zermelo-Frenkel set theory with Choice, along with a large swath of
associated, high-level theorems, e.g.@: the fundamental theorem of arithmetic,
the Cauchy-Schwarz inequality, Stirling's formula, etc.  See the Metamath
book.")
    (license license:gpl2+)))

(define-public minizinc
  (package
    (name "minizinc")
    (version "2.8.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/MiniZinc/libminizinc")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "03iliizyadd0wvx6a63rg22lb6p4m6krhlpfm2hfzwj66y3a76j6"))
              (modules '((guix build utils)
                         (ice-9 ftw)
                         (srfi srfi-1)))
              (snippet
               '(begin
                  ;; Do not advertise proprietary solvers
                  (with-directory-excursion "cmake/targets"
                    (let ((targets '("libminizinc_fzn.cmake"
                                     "libminizinc_gecode.cmake"
                                     "libminizinc_mip.cmake"
                                     "libminizinc_nl.cmake"
                                     "libminizinc_osicbc.cmake"
                                     "libminizinc_parser.cmake"
                                     "libmzn.cmake"
                                     "minizinc.cmake"
                                     "mzn2doc.cmake")))
                     (for-each delete-file
                              (remove
                               (lambda (file)
                                 (member file (cons* "." ".." targets)))
                               (scandir ".")))
                     (substitute* "libmzn.cmake"
                       (("include\\(cmake/targets/(.*)\\)" all target)
                        (if (member target targets) all "")))))
                  (with-directory-excursion "include/minizinc/solvers/MIP"
                    (for-each delete-file
                              (remove
                               (lambda (file)
                                 (member file '("." ".."
                                                "MIP_osicbc_solverfactory.hh"
                                                "MIP_osicbc_wrap.hh"
                                                "MIP_solverinstance.hh"
                                                "MIP_solverinstance.hpp"
                                                "MIP_wrap.hh")))
                               (scandir "."))))
                  (with-directory-excursion "solvers/MIP"
                    (for-each delete-file
                              (remove
                               (lambda (file)
                                 (member file '("." ".."
                                                "MIP_osicbc_solverfactory.cpp"
                                                "MIP_osicbc_wrap.cpp"
                                                "MIP_solverinstance.cpp"
                                                "MIP_wrap.cpp")))
                               (scandir "."))))
                  (substitute* "CMakeLists.txt"
                    (("find_package\\(([^ ]*).*\\)" all pkg)
                     (if (member pkg '("Gecode" "OsiCBC" "Threads"))
                         all
                         "")))
                  ;; TODO: swap out miniz for zlib
                  #t))))
    (build-system cmake-build-system)
    (arguments
     (list
       #:tests? #f ; no ‘check’ target
       #:modules '((guix build cmake-build-system)
                   (guix build utils)
                   (srfi srfi-1))
       #:phases
       #~(modify-phases %standard-phases
         (add-after 'install 'install-solver-configs
           (lambda _
             (let ((chuffed #$(this-package-input "chuffed"))
                   (gecode #$(this-package-input "gecode"))
                   (pkgdatadir (string-append #$output
                                              "/share/minizinc")))
               (call-with-output-file (string-append pkgdatadir
                                                     "/Preferences.json")
                 (lambda (port)
                   (display "\
{
  \"tagDefaults\": [
    [\"\", \"org.gecode.gecode\"],
    [\"gecode\", \"org.gecode.gecode\"]
  ],
  \"solverDefaults\": []
}"
                            port)
                   (newline port)))
               (for-each
                 (lambda (solver)
                   (copy-recursively
                     (string-append solver "/share/minizinc/solvers")
                     (string-append pkgdatadir "/solvers")))
                 (list gecode chuffed))))))))
    (native-inputs
     (list bison flex))
    (inputs
     (list cbc chuffed gecode zlib))
    (home-page "https://www.minizinc.org")
    (synopsis "High-level constraint modeling language")
    (description "MiniZinc is a high-level modeling language for constraint
satisfaction and optimization problems.  Models are compiled to FlatZinc, a
language understood by many solvers.")
    (license license:mpl2.0)))

(define-public mumps
  (package
    (name "mumps")
    (version "5.5.1")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "http://mumps.enseeiht.fr/MUMPS_"
                                 version ".tar.gz")
                  (string-append
                   "https://ftp.mcs.anl.gov/pub/petsc/externalpackages"
                   "/MUMPS_" version ".tar.gz")))
       (sha256
        (base32
         "05gs2i8b76m9flm1826fxpyfnwibjjawbmfza3ylrvj7zaag5gqs"))))
    (build-system gnu-build-system)
    (inputs
     (list gfortran
           ;; These are required for linking against mumps, but we let the user
           ;; declare the dependency.
           openblas
           metis
           scotch))
    (arguments
     `(#:modules ((ice-9 match)
                  (ice-9 popen)
                  (srfi srfi-1)
                  ,@%default-gnu-modules)
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
          (lambda* (#:key inputs outputs #:allow-other-keys)
            (call-with-output-file "Makefile.inc"
              (lambda (port)
                (format port "
PLAT          =
LIBEXT        = .a
LIBEXT_SHARED = .so
OUTC          = -o
OUTF          = -o
BLASDIR       = ~a
LIBBLAS       = -Wl,-rpath=$(BLASDIR)/lib -Wl,-rpath='$$ORIGIN'
LIBBLAS      += -L$(BLASDIR)/lib
LIBBLAS      += -lopenblas~@[
SCALAPDIR     = ~a
SCALAP        = -Wl,-rpath=$(SCALAPDIR)/lib -Wl,-rpath='$$ORIGIN'
SCALAP       += -L$(SCALAPDIR)/lib -lscalapack~]
RM            = rm -f~:[
CC            = gcc
FC            = gfortran
FL            = gfortran
INCSEQ        = -I$(topdir)/libseq
LIBSEQ        = $(LAPACK) -L$(topdir)/libseq -lmpiseq
LIBSEQNEEDED  = libseqneeded
INCS          = $(INCSEQ)
LIBS          = $(LIBSEQ)~;
CC            = mpicc
FC            = mpifort
FL            = mpifort
INCPAR        =
LIBPAR        = $(SCALAP) $(LAPACK)
LIBSEQNEEDED  =
INCS          = $(INCPAR)
LIBS          = $(LIBPAR)~]
AR            = ar vr # rules require trailing space, ugh...
RANLIB        = ranlib
LIBOTHERS     = -pthread
CDEFS         = -DAdd_
PIC           = -fPIC
FPIC_OPT      = $(PIC)
RPATH_OPT     = -Wl,-rpath,~a/lib
OPTF          = -O2 -fopenmp -DALLOW_NON_INIT -DBLR_MT
OPTF         += -fallow-argument-mismatch $(PIC)
OPTL          = -O2 -fopenmp $(PIC)
OPTC          = -O2 -fopenmp $(PIC)
LPORDDIR      = $(topdir)/PORD/lib
IPORD         = -I$(topdir)/PORD/include
LPORD         = $(LPORDDIR)/libpord.a
ORDERINGSF    = -Dpord~@[
METISDIR      = ~a
IMETIS        = -I$(METISDIR)/include
LMETIS        = -Wl,-rpath $(METISDIR)/lib -L$(METISDIR)/lib -lmetis
ORDERINGSF   += -Dmetis~]~@[~:{
SCOTCHDIR     = ~a
ISCOTCH       = -I$(SCOTCHDIR)/include
LSCOTCH       = -Wl,-rpath $(SCOTCHDIR)/lib -L$(SCOTCHDIR)/lib ~a -lesmumps
LSCOTCH      += -lscotch -lscotcherr
ORDERINGSF   += ~a~}~]
ORDERINGSC    = $(ORDERINGSF)
LORDERINGS    = $(LPORD) $(LMETIS) $(LSCOTCH)
IORDERINGSF   = $(ISCOTCH)
IORDERINGSC   = $(IPORD) $(IMETIS) $(ISCOTCH)"
                        (assoc-ref inputs "openblas")
                        (assoc-ref inputs "scalapack")
                        (->bool (which "mpicc"))  ;; MPI support enabled?
                        (assoc-ref outputs "out")
                        (assoc-ref inputs "metis")
                        (match (list (assoc-ref inputs "pt-scotch")
                                     (assoc-ref inputs "scotch"))
                          ((#f #f)
                           #f)
                          ((#f scotch)
                           `((,scotch "" "-Dscotch")))
                          ((ptscotch _)
                           `((,ptscotch
                              "-lesmumps -lptscotch -lptscotcherr "
                              "-Dptscotch")))))))))
         (replace 'build
           ;; By default only the d-precision library is built. Make with "all"
           ;; target so that all precision libraries and examples are built.
           ;; Then, "make allshared" builts equivalent shared libraries as well.
           (lambda _
             (invoke "make" "all"
                     (format #f "-j~a" (parallel-job-count)))
             (invoke "make" "allshared"
                     (format #f "-j~a" (parallel-job-count)))))
         (replace 'check
          ;; Run the simple test drivers, which read test input from stdin:
          ;; from the "real" input for the single- and double-precision
          ;; testers, and from the "cmplx" input for complex-precision
          ;; testers.  The EXEC-PREFIX key is used by the mumps-openmpi
          ;; package to prefix execution with "mpirun".
          (lambda* (#:key (exec-prefix '()) #:allow-other-keys)
            (with-directory-excursion "examples"
              (every
               (lambda (prec type)
                 (let ((tester (apply open-pipe*
                                      `(,OPEN_WRITE
                                        ,@exec-prefix
                                        ,(string-append "./" prec
                                                        "simpletest"))))
                       (input  (open-input-file
                                (string-append "input_simpletest_" type))))
                   (begin
                     (dump-port input tester)
                     (close-port input)
                     (zero? (close-pipe tester)))))
               '("s" "d" "c" "z")
               '("real" "real" "cmplx" "cmplx")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (libdir (string-append out "/lib")))
               (copy-recursively "lib" libdir)
               (copy-recursively "include" (string-append out "/include"))
               (when (file-exists? "libseq/libmpiseq.a")
                 (install-file "libseq/libmpiseq.a" libdir))
               (when (file-exists? "libseq/libmpiseq.so")
                 (install-file "libseq/libmpiseq.so" libdir))
               #t))))))
    (home-page "http://mumps.enseeiht.fr")
    (synopsis "Multifrontal sparse direct solver")
    (description
     "MUMPS (MUltifrontal Massively Parallel sparse direct Solver) solves a
sparse system of linear equations A x = b using Gaussian elimination.")
    (license license:cecill-c)))

(define-public mumps-metis
  (package
    (inherit mumps)
    (name "mumps-metis")
    (inputs (modify-inputs (package-inputs mumps)
              (delete "scotch")))))

(define-public mumps-openmpi
  (package
    (inherit mumps)
    (name "mumps-openmpi")
    (inputs
     (modify-inputs (package-inputs mumps)
       (delete "scotch")
       (prepend openmpi scalapack pt-scotch)))
    (arguments
     (substitute-keyword-arguments (package-arguments mumps)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
	     ,%openmpi-setup)
           (replace 'check
             (lambda _
               ((assoc-ref ,phases 'check)
                #:exec-prefix '("mpirun" "-n" "2"))))))))
    (synopsis "Multifrontal sparse direct solver (with MPI)")))

(define-public mumps-metis-openmpi
  (package
    (inherit mumps-openmpi)
    (name "mumps-metis-openmpi")
    (inputs (modify-inputs (package-inputs mumps-openmpi)
              (delete "pt-scotch")))))

(define-public ruby-asciimath
  (package
    (name "ruby-asciimath")
    (version "2.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (rubygems-uri "asciimath" version))
       (sha256
        (base32
         "1fy2jrn3gr7cl33qydp3pwyfilcmb4m4z6hfhnvydzg8r3srp36j"))))
    (build-system ruby-build-system)
    (native-inputs
     (list ruby-nokogiri ruby-rspec))
    (synopsis "AsciiMath parsing and conversion library")
    (description
     "A pure Ruby AsciiMath parsing and conversion library.  AsciiMath is an
easy-to-write markup language for mathematics.")
    (home-page "https://github.com/asciidoctor/asciimath")
    (license license:expat)))

(define-public superlu
  (package
    (name "superlu")
    (version "5.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://portal.nersc.gov/project/sparse/superlu/"
                           "superlu-" version ".tar.gz"))
       (sha256
        (base32 "0xvib7nk2rlbsiv1iwkwl9kxppkalkciv628bsyiiv0pv754n48q"))
       (modules '((guix build utils)))
       (snippet
        ;; Replace the non-free implementation of MC64 with a stub adapted
        ;; from Debian
        '(begin
           (use-modules (ice-9 regex)
                        (ice-9 rdelim))
           (call-with-output-file "SRC/mc64ad.c"
             (lambda (port)
               (display "
#include <stdio.h>
#include <stdlib.h>
void mc64id_(int *a) {
  fprintf (stderr, \"SuperLU: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}
void mc64ad_ (int *a, int *b, int *c, int *d, int *e, double *f, int *g,
              int *h, int *i, int *j, int *k, double *l, int *m, int *n) {
  fprintf (stderr, \"SuperLU: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}\n" port)))
           ;; Remove the corresponding license verbiage.  MC64 license follows
           ;; a "------" line separator.
           (with-atomic-file-replacement "License.txt"
             (let ((rx (make-regexp "-{8}")))
               (lambda (in out)
                 (let loop ()
                   (let ((line (read-line in 'concat)))
                    (unless (regexp-exec rx line)
                      (display line out)
                      (loop))))
                 #t)))))))
    (build-system cmake-build-system)
    (native-inputs
     (list tcsh))
    (inputs
     `(("blas" ,openblas)
       ("gfortran" ,gfortran)))
    (arguments
     `(#:configure-flags '("-Denable_blaslib:BOOL=NO" ;do not use internal cblas
                           "-DTPL_BLAS_LIBRARIES=openblas"
                           "-DBUILD_SHARED_LIBS:BOOL=YES")))
    (home-page "https://portal.nersc.gov/project/sparse/superlu/")
    (synopsis "Supernodal direct solver for sparse linear systems")
    (description
     "SuperLU is a general purpose library for the direct solution of large,
sparse, nonsymmetric systems of linear equations on high performance machines.
The library is written in C and is callable from either C or Fortran.  The
library routines perform an LU decomposition with partial pivoting and
triangular system solves through forward and back substitution.  The library
also provides threshold-based ILU factorization preconditioners.")
    (license (list license:bsd-3
                   license:gpl2+        ;EXAMPLE/*fgmr.c
                   (license:fsf-free "file://SRC/colamd.h")))))

(define-public superlu-dist
  (package
    (name "superlu-dist")
    (version "6.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xiaoyeli/superlu_dist")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fa29yr72p4yq5ln4rgfsawmi5935n4qcr5niz6864bjladz4lql"))
       (modules '((guix build utils)))
       (snippet
        ;; Replace the non-free implementation of MC64 with a stub
        '(begin
           (make-file-writable "SRC/mc64ad_dist.c")
           (call-with-output-file "SRC/mc64ad_dist.c"
             (lambda (port)
               (display "
#include <stdio.h>
#include <stdlib.h>
void mc64id_dist(int *a) {
  fprintf (stderr, \"SuperLU_DIST: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}
void mc64ad_dist (int *a, int *b, int *c, int *d, int *e, double *f, int *g,
              int *h, int *i, int *j, int *k, double *l, int *m, int *n) {
  fprintf (stderr, \"SuperLU_DIST: non-free MC64 not available.  Aborting.\\n\");
  abort ();
}\n" port)))
           (substitute* "SRC/util.c"    ;adjust default algorithm
             (("RowPerm[[:blank:]]*=[[:blank:]]*LargeDiag_MC64;")
              ;; TODO: set to "LargeDiag_AWPM" once combinatorial-blas has
              ;; general (i.e. non-square) processor-grid support.
              "RowPerm = NOROWPERM;"))
           #t))
       (patches (search-patches "superlu-dist-scotchmetis.patch"
                                "superlu-dist-awpm-grid.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list tcsh))
    (inputs
     `(("gfortran" ,gfortran)
       ("blas" ,openblas)
       ("combblas" ,combinatorial-blas)))
    (propagated-inputs
     `(("mpi" ,openmpi)                 ;headers include MPI heades
       ("parmetis" ,pt-scotch32 "metis")
       ("pt-scotch" ,pt-scotch32)))
    (arguments
     `(#:parallel-tests? #f             ;tests use MPI and OpenMP
       #:configure-flags (list "-DBUILD_SHARED_LIBS:BOOL=YES"
                               "-DTPL_ENABLE_COMBBLASLIB=YES"
                               "-DTPL_BLAS_LIBRARIES=-lopenblas"
                               "-DTPL_LAPACK_LIBRARIES=-lopenblas"
                               (string-append "-DTPL_PARMETIS_LIBRARIES="
                                              (string-join
                                               '("ptscotchparmetisv3" "ptscotcherr"
                                                 "scotchmetisv3" "scotcherr")
                                               ";"))
                               (string-append "-DTPL_PARMETIS_INCLUDE_DIRS="
                                              (assoc-ref %build-inputs "parmetis")
                                              "/include")
                               "-DTPL_ENABLE_COMBBLASLIB=ON"
                               (string-append "-DTPL_COMBBLAS_INCLUDE_DIRS="
                                              (assoc-ref %build-inputs "combblas")
                                              "/include/CombBLAS;"
                                              (assoc-ref %build-inputs "combblas")
                                              "/include/BipartiteMatchings")
                               "-DTPL_COMBBLAS_LIBRARIES=CombBLAS")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-c++-standard
           (lambda _
             (substitute* "CMakeLists.txt"
               ;; AWPM headers require C++14
               (("CMAKE_CXX_STANDARD 11") "CMAKE_CXX_STANDARD 14"))))
	 (add-before 'check 'mpi-setup
	   ,%openmpi-setup)
         (add-before 'check 'omp-setup
           (lambda _ (setenv "OMP_NUM_THREADS" "1") #t)))))
    (home-page (package-home-page superlu))
    (synopsis "Parallel supernodal direct solver")
    (description
     "SuperLU_DIST is a parallel extension to the serial SuperLU library.
It is targeted for distributed memory parallel machines.  SuperLU_DIST is
implemented in ANSI C, and MPI for communications.")
    (license license:bsd-3)))

(define-public scotch
  (package
    (name "scotch")
    (version "7.0.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.inria.fr/scotch/scotch")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0r46bmnz9xjlgcb3vvlx3sg2qh4gfgga89vs4vlbzz3s4lj48g46"))
       (patches (search-patches "scotch-cmake-remove-metis.patch"))
       (modules '((guix build utils)))
       (snippet
        #~(substitute* "src/libscotchmetis/library_parmetis.h"
            (("typedef DUMMYINT SCOTCH_Num" all)
             ;; 'DUMMYINT' is typically replaced by 'int32_t'.  Include
             ;; <stdint.h> to get that type definition.
             (string-append "#include <stdint.h>\n" all "\n"))))))
    (build-system cmake-build-system)
    (inputs
     (list zlib))
    (native-inputs
     (list flex bison gfortran))
    (outputs '("out" "metis"))
    (arguments
     (list #:configure-flags #~'("-DBUILD_SHARED_LIBS=YES" "-DINTSIZE=64"
                                 "-DBUILD_PTSCOTCH=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-metis
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Move the METIS compatibility library to a separate output to
                   ;; avoid a name clash on <metis.h>.
                   (let* ((out (assoc-ref outputs "out"))
                          (metis (assoc-ref outputs "metis"))
                          (prefix (string-length out)))
                     (for-each (lambda (file)
                                 (let ((target
                                        (string-append metis
                                                       (string-drop file
                                                                    prefix))))
                                   (mkdir-p (dirname target))
                                   (rename-file file target)))
                               (find-files out "metis"))))))))
    (home-page "https://www.labri.fr/perso/pelegrin/scotch/")
    (properties
     `((release-monitoring-url
        . "https://gitlab.inria.fr/scotch/scotch/-/releases")))
    (synopsis "Programs and libraries for graph algorithms")
    (description "SCOTCH is a set of programs and libraries which implement
the static mapping and sparse matrix reordering algorithms developed within
the SCOTCH project.  Its purpose is to apply graph theory, with a divide and
conquer approach, to scientific computing problems such as graph and mesh
partitioning, static mapping, and sparse matrix ordering, in application
domains ranging from structural mechanics to operating systems or
bio-chemistry.")
    ;; See LICENSE_en.txt
    (license license:cecill-c)))

(define-public scotch32
  ;; This is the 'INTSIZE32' variant, which uses 32-bit integers, as needed by
  ;; some applications.
  (package
    (inherit scotch)
    (name "scotch32")
    (arguments
     (substitute-keyword-arguments (package-arguments scotch)
       ((#:configure-flags flags #~'())
        #~'("-DBUILD_SHARED_LIBS=YES" "-DBUILD_PTSCOTCH=OFF" "-DINTSIZE=32"))))
    (synopsis
     "Programs and libraries for graph algorithms (32-bit integers)")))

(define-public pt-scotch
  (package
    (inherit scotch)
    (name "pt-scotch")
    (propagated-inputs
     (list openmpi))                              ;headers include MPI headers
    (arguments
     (substitute-keyword-arguments (package-arguments scotch)
       ((#:configure-flags flags #~'())
        #~'("-DBUILD_SHARED_LIBS=YES" "-DBUILD_PTSCOTCH=ON" "-DINTSIZE=64"))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'mpi-setup
              #$%openmpi-setup)))))
    (synopsis "Programs and libraries for graph algorithms (with MPI)")))

(define-public pt-scotch32
  (package
    (inherit pt-scotch)
    (name "pt-scotch32")
    (propagated-inputs
     (list openmpi))                     ;headers include MPI headers
    (arguments
     (substitute-keyword-arguments (package-arguments pt-scotch)
       ((#:configure-flags flags #~'())
        #~'("-DBUILD_SHARED_LIBS=YES" "-DBUILD_PTSCOTCH=ON" "-DINTSIZE=32"))))
    (synopsis
     "Programs and libraries for graph algorithms (with MPI and 32-bit integers)")))

(define-public gklib
  (let ((commit "8bd6bad750b2b0d90800c632cf18e8ee93ad72d7")
        (revision "1"))
    (package
      (name "gklib")
      (version (git-version "5.1.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/KarypisLab/GKlib")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08k4zzyd7zsisdhfmnwz7zb9w3pzpgagyjq52mwk8i6sqajdxsdn"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DBUILD_SHARED_LIBS=ON"
                #$@(if (target-x86?)
                       '()
                       '("-DNO_X86=1")))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'remove-march=native
                (lambda _
                  (substitute* "GKlibSystem.cmake"
                    (("-march=native") "")))))))
      (home-page "https://github.com/KarypisLab/GKlib")
      (synopsis "Helper library for METIS")
      (description
       "GKlib is a library of various helper routines and frameworks used by
software from KarypisLab, such as METIS.")
      (license license:asl2.0))))

;; XXX: Remove once the full SuiteSparse package is replaced.
(define-public metis
  (package
    (name "metis")
    (version "5.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://glaros.dtc.umn.edu/gkhome/fetch/sw/metis/"
                           "metis-" version ".tar.gz"))
       (sha256
        (base32
         "1cjxgh41r8k6j029yxs8msp3z6lcnpm16g5pvckk35kc7zhfpykn"))))
    (properties
     `((release-monitoring-url
        . "http://glaros.dtc.umn.edu/gkhome/metis/metis/download")))
    (build-system cmake-build-system)
    (inputs
     `(("blas" ,openblas)))
    (arguments
     `(#:tests? #f                      ;no tests
       #:configure-flags `("-DSHARED=ON"
                           ,(string-append "-DGKLIB_PATH=" (getcwd)
                                           "/metis-" ,version "/GKlib"))))
    (home-page "http://glaros.dtc.umn.edu/gkhome/metis/metis/overview")
    (synopsis "Graph partitioning and fill-reducing matrix ordering library")
    (description
     "METIS is a set of serial programs for partitioning graphs, partitioning
finite element meshes, and producing fill-reducing orderings for sparse
matrices.  The algorithms implemented in METIS are based on the multilevel
recursive-bisection, multilevel k-way, and multi-constraint partitioning
schemes.")
    (license license:asl2.0)))          ;As of version 5.0.3

(define-public metis-5.2
  (package
    (name "metis")
    (version "5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/KarypisLab/METIS")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "19vi1wsi2gp2m5vb715yfnzd2g7brm4r40qxg65ysrzgl13lpmvr"))
       (snippet
        #~(delete-file "manual/manual.pdf"))))
    (build-system cmake-build-system)
    (inputs (list gklib openblas))
    (arguments
     (list
      #:tests? #f ; Tests are not automatic
      #:configure-flags
      #~(list "-DSHARED=ON"
              (string-append "-DGKLIB_PATH=" #$gklib))
      #:phases
      #~(modify-phases %standard-phases
          ;; The original Makefile copies some files and invokes CMake.
          (add-before 'configure 'prepare-cmake
              (lambda _
                (substitute* "Makefile"
                  (("config: distclean") "config:")
                  (("BUILDDIR =.*")
                   "BUILDDIR = .\n")
                  ((".*cmake.*") ""))
                (substitute* "CMakeLists.txt"
                  (("build/") "../source/"))
                (invoke "make" "config"))))))
    (home-page "http://glaros.dtc.umn.edu/gkhome/metis/metis/overview")
    (synopsis "Graph partitioning and fill-reducing matrix ordering library")
    (description
     "METIS is a set of serial programs for partitioning graphs, partitioning
finite element meshes, and producing fill-reducing orderings for sparse
matrices.  The algorithms implemented in METIS are based on the multilevel
recursive-bisection, multilevel k-way, and multi-constraint partitioning
schemes.")
    (license license:asl2.0)))

(define-public p4est
  (package
    (name "p4est")
    (version "2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://p4est.github.io/release/p4est-"
                           version ".tar.gz"))
       (sha256
        (base32
         "16h267z256kxcxfjs390qqzv19hr58vrj4x8lndb7alnk2vca8n5"))))
    (build-system gnu-build-system)
    (inputs
     `(("fortran" ,gfortran)
       ("blas" ,openblas)
       ("zlib" ,zlib)))
    (arguments
     `(#:configure-flags `(,(string-append "BLAS_LIBS=-L"
                                           (assoc-ref %build-inputs "blas")
                                           " -lopenblas")
                           ,(string-append "LAPACK_LIBS=-L"
                                           (assoc-ref %build-inputs "blas")
                                           " -lopenblas"))
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'mpi-setup
		    ,%openmpi-setup))))
    (home-page "https://www.p4est.org")
    (synopsis "Adaptive mesh refinement on forests of octrees")
    (description
     "The p4est software library enables the dynamic management of a
collection of adaptive octrees, conveniently called a forest of octrees.
p4est is designed to work in parallel and scales to hundreds of thousands of
processor cores.")
    (license license:gpl2+)))

(define-public p4est-openmpi
  (package (inherit p4est)
    (name "p4est-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ,@(package-inputs p4est)))
    (arguments
     (substitute-keyword-arguments (package-arguments p4est)
       ((#:configure-flags cf)
        ``("--enable-mpi" ,@,cf))))
    (synopsis "Parallel adaptive mesh refinement on forests of octrees")))

(define-public gsegrafix
  ;; This is an old and equally dead "experimental fork" of the longer-dead
  ;; original. At least it no longer requires the even-deader libgnomeprint{,ui}
  ;; libraries, instead rendering plots with Pango.
  (package
    (name "gsegrafix")
    (version "1.0.7.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/gsegrafix-experimental/"
                           "gsegrafix-experimental-" version ".tar.gz"))
       (sha256
        (base32 "0fwh6719xy2zasmqlp0vdx6kzm45hn37ga88xmw5cz0yx7xw4j6f"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static")))
    (inputs
     (list glib gtk+))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.gnu.org/software/gsegrafix/")
    (synopsis "GNOME application to create scientific and engineering plots")
    (description
     "GSEGrafix is an application which produces high-quality graphical
plots for science and engineering.  Plots are specified via simple ASCII
parameter files and data files and are presented in an anti-aliased GNOME
canvas.  The program supports rectangular two-dimensional plots, histograms,
polar-axis plots and three-dimensional plots.  Plots can be printed or saved
to BMP, JPEG or PNG image formats.")
    (license license:gpl3+)))

(define-public maxima
  (package
    (name "maxima")
    (version "5.47.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/maxima/Maxima-source/"
                           version "-source/" name "-" version ".tar.gz"))
       (sha256
        (base32 "0yhgsi7s22bpblrmrj60x0jsjdz98b5hjdcq7b0fhlzx4hdh414i"))
       (patches (search-patches "maxima-defsystem-mkdir.patch"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal
           gnuplot                       ;for plots
           sbcl
           sed
           tk))                          ;Tcl/Tk is used by 'xmaxima'
    (native-inputs
     (list texinfo perl python))
    (arguments
     (list
      #:configure-flags
      #~(list "--enable-sbcl"
              (string-append "--with-sbcl=" #$sbcl "/bin/sbcl")
              (string-append "--with-posix-shell=" #$bash-minimal "/bin/sh")
              (string-append "--with-wish=" #$tk "/bin/wish"
                             #$(version-major+minor (package-version tk))))
      ;; By default Maxima attempts to write temporary files to
      ;; '/tmp/nix-build-maxima-*', which won't exist at run time.
      ;; Work around that.
      #:make-flags #~(list "TMPDIR=/tmp")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (let* ((sed (search-input-file inputs "/bin/sed"))
                     (coreutils (assoc-ref inputs "coreutils"))
                     (dirname (string-append coreutils "/bin/dirname"))
                     (head (string-append coreutils "/bin/head"))
                     (perl (search-input-file inputs "/bin/perl"))
                     (python (search-input-file inputs "/bin/python3")))
                (substitute* "src/maxima.in"
                  (("sed ") (string-append sed " "))
                  (("dirname") dirname)
                  (("head") head))
                (substitute* "doc/info/Makefile.in"
                  (("/usr/bin/env perl") perl))
                (substitute* "doc/info/build_html.sh.in"
                  (("python") python)))))
          (add-before 'check 'pre-check
            (lambda _
              (chmod "src/maxima" #o555)))
          (replace 'check
            (lambda _
              ;; This is derived from the testing code in the "debian/rules" file
              ;; of Debian's Maxima package.
              ;; If Maxima can successfully run this, the binary to be installed
              ;; should be fine.
              (invoke "sh" "-c"
                      (string-append
                       "./maxima-local "
                       "--lisp=sbcl "
                       "--batch-string=\"run_testsuite();\" "
                       "| grep -q \"No unexpected errors found\""))))
          ;; Make sure the doc and emacs files are found in the
          ;; standard location.  Also configure maxima to find gnuplot
          ;; without having it on the PATH.
          (add-after 'install 'post-install
            (lambda* (#:key outputs inputs #:allow-other-keys)
              (let* ((gnuplot (assoc-ref inputs "gnuplot"))
                     (out (assoc-ref outputs "out"))
                     (datadir (string-append out "/share/maxima/" #$version))
                     (binutils (dirname (search-input-file inputs "/bin/as"))))
                (with-directory-excursion out
                  (mkdir-p "share/emacs")
                  (mkdir-p "share/doc")
                  (symlink
                   (string-append datadir "/doc/")
                   (string-append out "/share/doc/maxima"))
                  (with-atomic-file-replacement
                   (string-append datadir "/share/maxima-init.lisp")
                   (lambda (in out)
                     (format out "~a ~s~a~%"
                             "(setf $gnuplot_command "
                             (string-append gnuplot "/bin/gnuplot") ")")
                     (dump-port in out))))
                ;; Ensure that Maxima will have access to the GNU binutils
                ;; components at runtime.
                (wrap-program (string-append out "/bin/maxima")
                  `("PATH" prefix (#$binutils))))))
          ;; The Maxima command ‘describe’ allows picking the relevant portions
          ;; from Maxima’s Texinfo docs.  However it does not support reading
          ;; gzipped info files.
          (delete 'compress-documentation))))
    (home-page "https://maxima.sourceforge.io")
    (synopsis "Numeric and symbolic expression manipulation")
    (description "Maxima is a system for the manipulation of symbolic and
numerical expressions.  It yields high precision numeric results by using
exact fractions, arbitrary precision integers, and variable precision floating
point numbers.")
    ;; Some files are lgpl2.1+. Some are gpl2+.  Some explicitly state gpl1+.
    ;; Others simply say "GNU General Public License" without stating a
    ;; version (which implicitly means gpl1+).
    ;; At least one file (src/maxima.asd) says "version 2."
    ;; GPLv2 only is therefore the smallest subset.
    (license license:gpl2)))

(define-public maxima-ecl
  (package/inherit maxima
    (name "maxima-ecl")
    (properties '((hidden? . #t)))
    (inputs
      (modify-inputs (package-inputs maxima)
        (delete "sbcl")
        (prepend ecl)))
    (arguments
     (substitute-keyword-arguments (package-arguments maxima)
       ((#:configure-flags flags)
         #~(list "--enable-ecl"))
        ((#:phases phases)
          #~(modify-phases #$phases
            (add-after 'install 'install-lib
             (lambda _
               (let ((lib (string-append
                            #$output "/lib/maxima/"
                            #$(package-version this-package)
                            "/binary-ecl")))
                    (install-file "src/binary-ecl/maxima.fas" lib))))
            (replace 'check
              (lambda _
                (invoke "sh" "-c"
                        (string-append
                          "./maxima-local "
                          "--lisp=ecl "
                          "--batch-string=\"run_testsuite();\" "
                          "| grep -q \"No unexpected errors found\""))))))))
    (description
      (string-append
        (package-description maxima)
        "  This package variant uses ECL as the underlying Lisp
implementation."))))

(define-public wxmaxima
  (package
    (name "wxmaxima")
    (version "24.11.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/wxMaxima-developers/wxmaxima")
                    (commit (string-append "Version-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1q66qv7m7dky9h7m6dzvlw6pkzixna4bhrdkz11sg7bv3a9qrlfy"))))
    (build-system cmake-build-system)
    (native-inputs (list gettext-minimal))
    (inputs (list bash-minimal
                  wxwidgets
                  maxima
                  ;; Runtime support.
                  adwaita-icon-theme
                  gtk+
                  shared-mime-info))
    (arguments
     `(#:tests? #f ; tests fail non-deterministically
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-doc-path
           (lambda _
             ;; Don't look in share/doc/wxmaxima-xx.xx.x for the
             ;; documentation.  Only licensing information is placed there by
             ;; Guix.
             (substitute* "src/Dirstructure.cpp"
               (("/doc/wxmaxima-\\%s")
                "/doc/wxmaxima"))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/wxmaxima")
               `("PATH" ":" prefix
                 (,(string-append (assoc-ref inputs "maxima")
                                  "/bin")))
               ;; For GtkFileChooserDialog.
               `("GSETTINGS_SCHEMA_DIR" =
                 (,(string-append (assoc-ref inputs "gtk+")
                                  "/share/glib-2.0/schemas")))
               `("XDG_DATA_DIRS" ":" prefix
                 ( ;; Needed by gdk-pixbuf to know supported icon formats.
                  ,(string-append (assoc-ref inputs "shared-mime-info")
                                  "/share")
                  ;; The default icon theme of GTK+.
                  ,(string-append (assoc-ref inputs "adwaita-icon-theme")
                                  "/share")))))))))
    (home-page "https://wxmaxima-developers.github.io/wxmaxima/")
    (synopsis
     "Graphical user interface for the Maxima computer algebra system")
    (description
     "wxMaxima is a graphical user interface for the Maxima computer algebra
system.  It eases the use of Maxima by making most of its commands available
through a menu system and by providing input dialogs for commands that require
more than one argument.  It also implements its own display engine that
outputs mathematical symbols directly instead of depicting them with ASCII
characters.

wxMaxima also features 2D and 3D inline plots, simple animations, mixing of
text and mathematical calculations to create documents, exporting of input and
output to TeX, and a browser for Maxima's manual including command index and
full text searching.")
    (license license:gpl2+)))

(define-public armadillo
  (package
    (name "armadillo")
    (version "12.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/arma/armadillo-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "150fcl2cca3ks91ahgr32jw0ww8lc8ng84mczfab7clvigqpdnpv"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f  ; no test target
       #:configure-flags '("-DOPENBLAS_PROVIDES_LAPACK=ON")))
    (inputs
     (list openblas arpack-ng))
    (home-page "https://arma.sourceforge.net/")
    (synopsis "C++ linear algebra library")
    (description
     "Armadillo is a C++ linear algebra library, aiming towards a good balance
between speed and ease of use.  It is useful for algorithm development
directly in C++, or quick conversion of research code into production
environments.  It can be used for machine learning, pattern recognition,
signal processing, bioinformatics, statistics, econometrics, etc.  The library
provides efficient classes for vectors, matrices and cubes, as well as 150+
associated functions (e.g., contiguous and non-contiguous submatrix views).")
    (license license:asl2.0)))

(define-public muparser
  ;; When switching download sites, muparser re-issued a 2.2.5 release with a
  ;; different hash. In order to make `guix package --upgrade` work correctly,
  ;; we set a Guix packaging revision.
  ;; When the next version of muparser is released, we can remove
  ;; UPSTREAM-VERSION and REVISION and use the plain VERSION.
  (let ((upstream-version "2.2.5")
        (revision "2"))
    (package
      (name "muparser")
      (version (string-append upstream-version "-" revision))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/beltoforion/muparser")
               (commit (string-append "v" upstream-version))))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0f0g4995xngf1pp3zr4p6ai2f8v6f8bxwa0k8ayjjiv1l8h44m24"))))
      (build-system gnu-build-system)
      (arguments
       `(#:configure-flags '("--enable-samples=no")
         #:tests? #f)) ;no "check" target
      (home-page "http://muparser.beltoforion.de/")
      (synopsis "Fast parser library for mathematical expressions")
      (description
       "muParser is an extensible high performance math parser library.  It is
based on transforming an expression into a bytecode and precalculating constant
parts of it.")
      (license license:expat))))

(define-public openblas
  (package
    (name "openblas")
    (version "0.3.29")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xianyi/OpenBLAS")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1wm00hn0vzb45nqg0q3ka15wjqjzma5lh1x6227di73icqdcbzcz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      ;; No default baseline is supplied for powerpc-linux.
      #:substitutable? (not (target-ppc32?))
      #:make-flags
      #~(list (string-append "PREFIX=" #$output)
              "SHELL=bash"
              "DEBUG=1"                 ; enable debug symbols
              "MAKE_NB_JOBS=0"          ;use jobserver for submakes
              "NO_STATIC=1"             ;avoid a 67 MiB static archive

              ;; This is the maximum number of threads OpenBLAS will ever use (that
              ;; is, if $OPENBLAS_NUM_THREADS is greater than that, then NUM_THREADS
              ;; is used.)  If we don't set it, the makefile sets it to the number
              ;; of cores of the build machine, which is obviously wrong.
              "NUM_THREADS=128"

              ;; DYNAMIC_ARCH is only supported on some architectures.
              ;; DYNAMIC_ARCH combined with TARGET=GENERIC provides a library
              ;; which uses the optimizations for the detected CPU.  This can
              ;; be overridden at runtime with the environment variable
              ;; OPENBLAS_CORETYPE=<type>, where "type" is a supported CPU
              ;; type.  On other architectures we target only the baseline CPU
              ;; supported by Guix.
              #$@(cond
                    ((or (target-x86-64?)
                         (target-x86-32?)
                         (target-ppc64le?)
                         (target-aarch64?))
                     ;; Dynamic older enables a few extra CPU architectures
                     ;; on x86_64 that were released before 2010.
                     '("DYNAMIC_ARCH=1" "DYNAMIC_OLDER=1" "TARGET=GENERIC"))
                    ;; On some of these architectures the CPU type can't be detected.
                    ;; We list the oldest CPU core we want to have support for.
                    ;; On MIPS we force the "SICORTEX" TARGET, as for the other
                    ;; two available MIPS targets special extended instructions
                    ;; for Loongson cores are used.
                    ((target-mips64el?)
                     '("TARGET=SICORTEX"))
                    ((target-arm32?)
                     '("TARGET=ARMV7"))
                    ((target-riscv64?)
                     '("TARGET=RISCV64_GENERIC"))
                    (else '())))
      ;; no configure script
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (add-before 'build 'set-extralib
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Get libgfortran found when building in utest.
              (setenv "FEXTRALIB"
                      (string-append
                       "-L"
                       (dirname
                        (search-input-file inputs "/lib/libgfortran.so")))))))))
    (inputs
     (list `(,gfortran "lib")))
    (native-inputs
     (list cunit gfortran perl))
    (outputs '("out" "debug"))
    (home-page "https://www.openblas.net/")
    (synopsis "Optimized BLAS library based on GotoBLAS")
    (description
     "OpenBLAS is a Basic Linear Algebra Subprograms (BLAS) library forked
from the GotoBLAS2-1.13 BSD version.")
    (license license:bsd-3)))

(define-public openblas-ilp64
  (package/inherit openblas
    (name "openblas-ilp64")
    (supported-systems %64bit-supported-systems)
    (arguments
     (substitute-keyword-arguments (package-arguments openblas)
       ((#:make-flags flags #~'())
        ;; These should be '64' but julia hardcodes '64_'.
        #~(append (list "INTERFACE64=1"
                        "SYMBOLSUFFIX=64_"
                        "LIBPREFIX=libopenblas64_")
                 #$flags))))
    (synopsis "Optimized BLAS library based on GotoBLAS (ILP64 version)")
    (license license:bsd-3)))

(define-public openblas-0.3.29 openblas)

(define-public libblastrampoline
  (package
    (name "libblastrampoline")
    (version "5.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLinearAlgebra/libblastrampoline")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0mf79zw11kxyil72y2ly5x8bbz3ng3nsqmp0zcps16b69wvfs19c"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "-C" "src"
                          (string-append "prefix=" (assoc-ref %outputs "out"))
                          (string-append "CC=" ,(cc-for-target)))
       #:tests? #f      ; No check target.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "https://github.com/JuliaLinearAlgebra/libblastrampoline")
    (synopsis "PLT trampolines to provide a BLAS and LAPACK demuxing library")
    (description
     "This package uses PLT trampolines to provide a BLAS and LAPACK demuxing
library.")
    (license license:expat)))

(define-public palp
  (package
    (name "palp")
    (version "2.21")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://hep.itp.tuwien.ac.at/~kreuzer/CY/palp/palp-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1myxjv0jxgr9acchwnjh9g5l61wxwiva3q6c1d6892lr37r7njky"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "CC=" #$(cc-for-target)))
           #:tests? #f ; no tests
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'install
                          (lambda _
                            (for-each
                             (lambda (name)
                               (install-file name (string-append #$output "/bin")))
                             '("class.x" "cws.x" "mori.x" "nef.x" "poly.x")))))))
    (home-page "http://hep.itp.tuwien.ac.at/~kreuzer/CY/CYpalp.html")
    (synopsis "Package for Analyzing Lattice Polytopes")
    (description
     "PALP is a set of programs for calculations with lattice polytopes and
applications to toric geometry.")
    (license license:gpl3)))

(define-public blis
  (package
    (name "blis")
    (version "0.9.0")
    (home-page "https://github.com/flame/blis")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (sha256
               (base32
                "14v2awhxma6nzas42hq97702672f2njrskqhsv9kl23hvrvci8fm"))
              (file-name (git-file-name "blis" version))))
    (native-inputs
     (list python perl))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~'("--enable-cblas")
      #:modules
      '((guix build gnu-build-system)
        (guix build utils)
        (srfi srfi-1))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs target system (configure-flags '())
                      #:allow-other-keys)
              ;; This is a home-made 'configure' script.
              (let* (;; Guix-specific support for choosing the configuration
                     ;; via #:configure-flags: see below for details.
                     (config-flag-prefix "--blis-config=")
                     (maybe-config-flag (find
                                         (lambda (s)
                                           (string-prefix? config-flag-prefix s))
                                         configure-flags))
                     (configure-flags (if maybe-config-flag
                                          (delete maybe-config-flag
                                                  configure-flags)
                                          configure-flags))
                     ;; Select the "configuration" to build.
                     ;; The "generic" configuration is non-optimized but
                     ;; portable (no assembly).
                     ;; The "x86_64" configuration family includes
                     ;; sub-configurations for all supported
                     ;; x86_64 microarchitectures.
                     ;; BLIS currently lacks runtime hardware detection
                     ;; for other architectures: see
                     ;; <https://github.com/flame/blis/commit/c534da6>.
                     ;; Conservatively, we stick to "generic" on armhf,
                     ;; aarch64, and ppc64le for now. (But perhaps
                     ;; "power9", "cortexa9", and "cortexa57" might be
                     ;; general enough to use?)
                     ;; Another approach would be to use the "auto"
                     ;; configuration and make this package
                     ;; non-substitutable.
                     ;; The build is fairly intensive, though.
                     (blis-config
                      (cond
                       (maybe-config-flag
                        (substring maybe-config-flag
                                   (string-length config-flag-prefix)))
                       ((string-prefix? "x86_64" (or target system))
                        "x86_64")
                       (else
                        "generic")))
                     (configure-args
                      `("-p" ,#$output
                        "-d" "opt"
                        "--disable-static"
                        "--enable-shared"
                        "--enable-threading=openmp"
                        "--enable-verbose-make"
                        ,@configure-flags
                        ,blis-config)))
                (format #t "configure args: ~s~%" configure-args)
                (apply invoke
                       "./configure"
                       configure-args))))
          (add-before 'check 'show-test-output
            (lambda _
              ;; By default "make check" is silent.  Make it verbose.
              (system "tail -F output.testsuite &"))))))
    (synopsis "High-performance basic linear algebra (BLAS) routines")
    (description
     "BLIS is a portable software framework for instantiating high-performance
BLAS-like dense linear algebra libraries.  The framework was designed to
isolate essential kernels of computation that, when optimized, immediately
enable optimized implementations of most of its commonly used and
computationally intensive operations.  While BLIS exports a new BLAS-like API,
it also includes a BLAS compatibility layer which gives application developers
access to BLIS implementations via traditional BLAS routine calls.")
    (license license:bsd-3)))

(define ignorance blis)

(define-public openlibm
  (package
    (name "openlibm")
    (version "0.8.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/openlibm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xsrcr49z0wdqpwd98jmw2xh18myzsa9xman0kp1h2i89x8mic5b"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out"))
             ,(string-append "CC=" (cc-for-target)))
       #:phases
       ;; no configure script
       (modify-phases %standard-phases (delete 'configure))
       #:tests? #f)) ;the tests are part of the default target
    (home-page "https://openlibm.org/")
    (synopsis "Portable C mathematical library (libm)")
    (description
     "OpenLibm is an effort to have a high quality, portable, standalone C
mathematical library (libm).  It can be used standalone in applications and
programming language implementations.  The project was born out of a need to
have a good libm for the Julia programming language that worked consistently
across compilers and operating systems, and in 32-bit and 64-bit
environments.")
    ;; Each architecture has its own make target, and there is none for mips.
    (supported-systems (delete "mips64el-linux" %supported-systems))
    ;; See LICENSE.md for details.
    (license (list license:expat
                   license:isc
                   license:bsd-2
                   license:public-domain
                   license:lgpl2.1+))))

(define-public openspecfun
  (package
    (name "openspecfun")
    (version "0.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/JuliaLang/openspecfun")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pfw6l3ch7isz403llx7inxlvavqh01jh1hb9dpidi86sjjx9kfh"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no "check" target
       #:make-flags
       (list (string-append "prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))         ; no configure script
    (inputs
     `(("fortran" ,gfortran)))
    (home-page "https://github.com/JuliaLang/openspecfun")
    (synopsis "Collection of special mathematical functions")
    (description
     "Openspecfun provides AMOS and Faddeeva.  AMOS (from Netlib) is a
portable package for Bessel Functions of a Complex Argument and Nonnegative
Order; it contains subroutines for computing Bessel functions and Airy
functions.  Faddeeva allows computing the various error functions of arbitrary
complex arguments (Faddeeva function, error function, complementary error
function, scaled complementary error function, imaginary error function, and
Dawson function); given these, one can also easily compute Voigt functions,
Fresnel integrals, and similar related functions as well.")
    ;; Faddeeva is released under the Expat license; AMOS is included as
    ;; public domain software.
    (license (list license:expat license:public-domain))))

;; Source for the modular SuiteSparse packages. When updating, also update the
;; (different) versions of the subpackages.
(define suitesparse-version "7.2.0")
(define suitesparse-source
  (origin
    (method git-fetch)
    (uri (git-reference
          (url "https://github.com/DrTimothyAldenDavis/SuiteSparse")
          (commit (string-append "v" suitesparse-version))))
    (file-name (git-file-name "suitesparse" suitesparse-version))
    (sha256
     (base32
      "1draljn8i46862drc6008cnb2zjpklf74j8c34jirjazzpf53kaa"))
    (modules '((guix build utils)))
    (snippet
     #~(begin
         ;; Delete autogenerated and bundled files
         (for-each delete-file (find-files "." "\\.pdf$"))
         ;; ssget
         (delete-file-recursively "ssget")
         ;; SuiteSparse_config
         (delete-file "SuiteSparse_config/SuiteSparse_config.h")
         ;; CHOLMOD
         (delete-file-recursively "CHOLMOD/SuiteSparse_metis")
         ; GraphBLAS
         (delete-file "GraphBLAS/README.md")
         (delete-file "GraphBLAS/Config/GB_config.h")
         (delete-file "GraphBLAS/Config/GB_prejit.c")
         (delete-file-recursively "GraphBLAS/cpu_features")
         (delete-file "GraphBLAS/CUDA/GB_cuda_common_jitFactory.hpp")
         (delete-file "GraphBLAS/JITpackage/GB_JITpackage.c")
         (delete-file-recursively "GraphBLAS/lz4/lz4.c")
         (delete-file-recursively "GraphBLAS/lz4/lz4.h")
         (delete-file-recursively "GraphBLAS/lz4/lz4hc.c")
         (delete-file-recursively "GraphBLAS/lz4/lz4hc.h")
         (delete-file "GraphBLAS/GraphBLAS/Config/GB_config.h")
         (delete-file "GraphBLAS/Tcov/PreJIT/GB_prejit.c")
         (delete-file-recursively "GraphBLAS/Source/FactoryKernels")
         (delete-file "GraphBLAS/Source/GB_AxB__include1.h")
         (delete-file "GraphBLAS/xxHash/xxhash.h")
         (delete-file-recursively "GraphBLAS/zstd/zstd_subset")
         ;; KLU
         (delete-file "KLU/Include/klu.h")
         (delete-file "KLU/Doc/klu_version.tex")
         ;; LDL
         (delete-file "LDL/Include/ldl.h")
         (delete-file "LDL/Doc/ldl_version.tex")
         ;; RBio
         (delete-file "RBio/Include/RBio.h")
         ;; SPEX
         (delete-file "SPEX/Include/SPEX.h")
         (delete-file "SPEX/Doc/SPEX_version.tex")
         ;; SPQR
         (delete-file "SPQR/Include/SuiteSparseQR_definitions.h")
         (delete-file "SPQR/Doc/spqr_version.tex")
         ;; UMFPACK
         (delete-file "UMFPACK/Include/umfpack.h")
         (delete-file "UMFPACK/Doc/umfpack_version.tex")))))

(define-public suitesparse-config
  (package
    (name "suitesparse-config")
    (version suitesparse-version)
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "SuiteSparse_config"))))))
    (inputs (list openblas))
    (native-inputs (list pkg-config))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Configuration for all SuiteSparse packages")
    (description "SuiteSparse is a suite of sparse matrix algorithms.  This
package contains a library with common configuration options.")
    (license license:bsd-3)))

(define-public suitesparse-amd
  (package
    (name "suitesparse-amd")
    (version "3.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "AMD")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../AMD/Doc"
                (invoke "make"))))
          ;; Required for suitesparse-umfpack
          (add-after 'install 'install-internal-header
              (lambda _
                (install-file "../AMD/Include/amd_internal.h"
                              (string-append #$output "/include"))))
          (add-after 'install-internal-header 'install-doc
            (lambda _
              (install-file "../AMD/Doc/AMD_UserGuide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (install-file "../AMD/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (propagated-inputs (list suitesparse-config))
    (native-inputs (list gfortran (texlive-updmap.cfg '())))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Sparse matrix ordering for Cholesky factorization")
    (description "AMD is a set of routines for ordering a sparse matrix prior
to Cholesky factorization (or for LU factorization with diagonal pivoting).")
    (license license:bsd-3)))

(define-public suitesparse-btf
  (package
    (name "suitesparse-btf")
    (version "2.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "BTF")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (replace 'install-license-files
            (lambda _
              (install-file "../BTF/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (propagated-inputs (list suitesparse-config))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Library for permuting matrices into block upper triangular form")
    (description "BTF (Block Triangular Form) is a C library for permuting a
matrix into block upper triangular form.")
    (license license:lgpl2.1+)))

(define-public suitesparse-camd
  (package
    (name "suitesparse-camd")
    (version "3.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "CAMD")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../CAMD/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../CAMD/Doc/CAMD_UserGuide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (install-file "../CAMD/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (propagated-inputs (list suitesparse-config))
    (native-inputs (list (texlive-updmap.cfg '())))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Sparse matrix ordering for Cholesky factorization with constraints")
    (description "CAMD is a set of routines for ordering a sparse matrix prior
to Cholesky factorization (or for LU factorization with diagonal pivoting).
It is a variant of AMD which has the the option to apply constraints to the
ordering.")
    (license license:bsd-3)))

(define-public suitesparse-colamd
  (package
    (name "suitesparse-colamd")
    (version "3.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "COLAMD")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (replace 'install-license-files
            (lambda _
              (install-file "../COLAMD/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (propagated-inputs (list suitesparse-config))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Column Approximate Minimum Degree Ordering")
    (description "COLAMD is library for computing a permutation vector for a
matrix with which the LU factorization becomes sparser.")
    (license license:bsd-3)))

(define-public suitesparse-ccolamd
  (package
    (name "suitesparse-ccolamd")
    (version "3.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "CCOLAMD")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (replace 'install-license-files
            (lambda _
              (install-file "../CCOLAMD/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (propagated-inputs (list suitesparse-config))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Column Approximate Minimum Degree Ordering with constraints")
    (description "CCOLAMD is library for computing a permutation vector for a
matrix with which the LU factorization becomes sparser.  It is a variant of
COLAMD which has the the option to apply constraints to the ordering.")
    (license license:bsd-3)))

(define-public gklib-suitesparse
  (package/inherit gklib
    (name "gklib-suitesparse")
    (source (origin
              (inherit (package-source gklib))
              (patches (cons
                        (search-patch
                         "gklib-suitesparse.patch")
                        (origin-patches (package-source gklib))))))
    (arguments
     (substitute-keyword-arguments (package-arguments gklib)
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-after 'unpack 'patch-cmake
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("add_library\\(GKlib.*" all)
                   (string-append
                    all
                    "target_link_libraries(GKlib PUBLIC"
                    " ${SUITESPARSE_CONFIG_LIBRARIES} m)\n")))))))))
    (propagated-inputs (list suitesparse-config))))

(define-public metis-suitesparse
  (package/inherit metis-5.2
    (name "metis-suitesparse")
    (arguments
     (substitute-keyword-arguments (package-arguments metis-5.2)
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'prepare-cmake 'set-idxwidth
              (lambda _
                (substitute* "Makefile"
                  (("IDXWIDTH.*=.*")
                   "IDXWIDTH = \"\\#define IDXTYPEWIDTH 64\"\n"))))
            (add-before 'prepare-cmake 'link-suitesparse-config
              (lambda _
                (substitute* "programs/CMakeLists.txt"
                  (("include_directories.*" all)
                   (string-append
                    all "find_package(SuiteSparse_config REQUIRED)\n"))
                  (("(target_link_libraries.*)GKlib(.*)" _ start end)
                   (string-append
                    start "GKlib ${SUITESPARSE_CONFIG_LIBRARIES}" end)))))))
       ((#:configure-flags _)
        #~(list "-DSHARED=ON"
                (string-append "-DGKLIB_PATH=" #$gklib-suitesparse)))))
    (inputs (list suitesparse-config gklib-suitesparse))))

(define-public suitesparse-cholmod
  (package
    (name "suitesparse-cholmod")
    (version "4.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "CHOLMOD")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-amd
                              #$suitesparse-camd
                              #$suitesparse-ccolamd
                              #$suitesparse-colamd
                              #$suitesparse-config)))
                  ")\nset(DUMMY\n"))
                (("add_subdirectory.*GPU.*") "\n")
                ((".*cmake_modules/FindCHOLMOD_CUDA.cmake.*") "\n"))))
          (add-after 'chdir 'use-external-metis
            (lambda _
              (let ((port (open-file "CMakeLists.txt" "a")))
                (display
                 (string-append
                  "find_library(METIS_LIBRARY NAME metis PATHS ENV LIBRARY_PATH)
get_filename_component(METIS_LIBRARY ${METIS_LIBRARY} REALPATH)
find_library(GKLIB_LIBRARY NAME GKlib PATHS ENV LIBRARY_PATH)
get_filename_component(GKLIB_LIBRARY ${GKLIB_LIBRARY} REALPATH)
target_link_libraries(CHOLMOD PRIVATE ${METIS_LIBRARY} ${GKLIB_LIBRARY})
target_link_libraries(CHOLMOD_static PRIVATE ${METIS_LIBRARY} ${GKLIB_LIBRARY})")
                 port)
                (close-port port))
              (delete-file "Partition/cholmod_metis_wrapper.c")
              (delete-file "Partition/cholmod_metis_wrapper.h")
              (substitute* "Partition/cholmod_metis.c"
                (("#include \"cholmod_metis_wrapper\\.h\"") "")
                (("#include \"SuiteSparse_metis/include/metis.h\"")
                 "#include <metis.h>")
                (("SuiteSparse_metis_METIS") "METIS"))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../CHOLMOD/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../CHOLMOD/Doc/CHOLMOD_UserGuide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (let ((out (string-append #$output
                                        "/share/doc/" #$name "-" #$version)))
                (install-file "../CHOLMOD/Doc/License.txt" out)
                (install-file "../CHOLMOD/Core/lesser.txt" out)
                (install-file "../CHOLMOD/MatrixOps/gpl.txt" out)))))))
    (inputs
     (list gklib-suitesparse
           metis-suitesparse
           openblas
           suitesparse-amd
           suitesparse-camd
           suitesparse-ccolamd
           suitesparse-colamd))
    (propagated-inputs (list suitesparse-config))
    (native-inputs (list (texlive-updmap.cfg '())))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Library for solving sparse symmetric positive definite linear
equations")
    (description "CHOLMOD is a set of routines for factorizing sparse symmetrix
positive definite matrices, updating/downdating sparse Cholesky factorizations
and other related operations.")
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public suitesparse-cxsparse
  (package
    (name "suitesparse-cxsparse")
    (version "4.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "CXSparse")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (replace 'install-license-files
            (lambda _
              (let ((out (string-append #$output
                                        "/share/doc/" #$name "-" #$version)))
                (install-file "../CXSparse/Doc/License.txt" out)
                (install-file "../CXSparse/Doc/lesser.txt" out)))))))
    (propagated-inputs (list suitesparse-config))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Concise eXtended Sparse Matrix Package")
    (description "CXSparse is a collection of sparse matrix algorithms for
direct methods on both real and complex matrices.")
    (license license:lgpl2.1+)))

(define-public suitesparse-klu
  (package
    (name "suitesparse-klu")
    (version "2.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "KLU")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-amd
                              #$suitesparse-btf
                              #$suitesparse-camd
                              #$suitesparse-ccolamd
                              #$suitesparse-cholmod
                              #$suitesparse-colamd
                              #$suitesparse-config)))
                  ")\nset(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (substitute* "../KLU/Doc/Makefile"
                (("\\.\\./\\.\\./BTF/Include/btf.h")
                 (string-append #$suitesparse-btf "/include/btf.h")))
              (with-directory-excursion "../KLU/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../KLU/Doc/KLU_UserGuide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (install-file "../KLU/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (inputs
     (list suitesparse-camd
           suitesparse-ccolamd
           suitesparse-colamd
           suitesparse-config))
    (propagated-inputs
     (list suitesparse-amd
           suitesparse-btf
           suitesparse-cholmod))
    (native-inputs (list (texlive-updmap.cfg '())))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Routines for solving sparse linear problems with a LU factorization")
    (description "KLU is a method for computing the LU factorization of sparse
for real and complex matrices.")
    (license license:lgpl2.1+)))

(define-public suitesparse-ldl
  (package
    (name "suitesparse-ldl")
    (version "3.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "LDL")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-amd
                              #$suitesparse-config)))
                  ")\nset(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (substitute* "../LDL/Doc/Makefile"
                (("\\.\\./\\.\\./BTF/Include/btf.h")
                 (string-append #$suitesparse-btf "/include/btf.h")))
              (with-directory-excursion "../LDL/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../LDL/Doc/ldl_userguide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (install-file "../LDL/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (inputs (list suitesparse-amd))
    (propagated-inputs (list suitesparse-config))
    (native-inputs (list (texlive-updmap.cfg '())))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "LDL' factorization method for sparse, symmetric matrices")
    (description "This package contains a set of routines for computing the
LDL' factorization of sparse, symmetric matrices.  Its focus lies on concise
code.")
    (license license:lgpl2.1+)))

(define-public suitesparse-rbio
  (package
    (name "suitesparse-rbio")
    (version "4.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "RBio")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-config)))
                  ")\nset(DUMMY\n")))))
          (replace 'install-license-files
            (lambda _
              (let ((out (string-append #$output
                                        "/share/doc/" #$name "-" #$version)))
                (install-file "../RBio/Doc/License.txt" out)
                (install-file "../RBio/Doc/gpl.txt" out)))))))
    (propagated-inputs (list suitesparse-config))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Library for the Rutherford/Boeing sparse matrix format")
    (description "This package provides the C library of RBio.  It can be used
for reading and writing sparse matrices in the Rutherford/Boeing format.")
    (license license:gpl2+)))

(define-public suitesparse-mongoose
  (package
    (name "suitesparse-mongoose")
    (version "3.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "Mongoose")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append "set(CMAKE_MODULE_PATH "
                                #$suitesparse-config "/lib/cmake/SuiteSparse)\n"
                                "set(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              ;; XeLaTeX fails with .eps graphics
              (with-directory-excursion "../Mongoose/Doc"
                (for-each
                 (lambda (name)
                   (invoke "epstopdf" name))
                 (find-files "Figures" "\\.eps$"))
                (substitute* "Mongoose_UserGuide.tex"
                  (("\\.eps") ".pdf"))
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../Mongoose/Doc/Mongoose_UserGuide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (install-file "../Mongoose/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (propagated-inputs (list suitesparse-config))
    (native-inputs
     (list texlive-epstopdf
           (texlive-updmap.cfg
            (list texlive-algorithmicx
                  texlive-booktabs
                  texlive-lastpage
                  texlive-multirow
                  texlive-pgf
                  texlive-caption
                  texlive-etoolbox
                  texlive-csquotes
                  texlive-fancybox
                  texlive-enumitem
                  texlive-microtype
                  texlive-cancel
                  texlive-sourcecodepro
                  texlive-xkeyval
                  texlive-fontspec
                  texlive-wasy
                  texlive-wasysym
                  texlive-float
                  texlive-tcolorbox
                  texlive-environ
                  texlive-xcolor
                  texlive-xetex
                  texlive-listings))))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Graph partitioning library")
    (description "Mongoose is a library for graph partitioning by computing
edge cuts using a coarsening and refinement framework.")
    (license license:gpl3)))

(define-public suitesparse-spex
  (package
    (name "suitesparse-spex")
    (version "2.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "SPEX")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-amd
                              #$suitesparse-colamd
                              #$suitesparse-config)))
                  ")\nset(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../SPEX/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../SPEX/Doc/SPEX_UserGuide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (inputs (list suitesparse-amd suitesparse-colamd))
    (propagated-inputs (list gmp mpfr suitesparse-config))
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-paralist
                  texlive-comment
                  texlive-psfrag
                  texlive-soul
                  texlive-multirow
                  texlive-algorithms
                  texlive-float
                  texlive-algorithmicx
                  texlive-cprotect
                  texlive-bigfoot
                  texlive-caption
                  texlive-listings
                  texlive-xcolor
                  texlive-framed
                  texlive-mdframed
                  texlive-etoolbox
                  texlive-zref
                  texlive-needspace))))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Package for SParse EXact algebra")
    (description "SPEX is a set of routines for sparse exact linear algebra.
It contains the SPEX Left LU library for computing a sparse exact left-looking
LU factorization for solving unsymmetric sparse linear systems.")
    ;; Dual licensed.
    (license (list license:lgpl3+ license:gpl2+))))

(define-public suitesparse-spqr
  (package
    (name "suitesparse-spqr")
    (version "4.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "SPQR")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-amd
                              #$suitesparse-camd
                              #$suitesparse-ccolamd
                              #$suitesparse-cholmod
                              #$suitesparse-colamd
                              #$suitesparse-config)))
                  ")\nset(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../SPQR/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (install-file "../SPQR/Doc/spqr_user_guide.pdf"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version))))
          (replace 'install-license-files
            (lambda _
              (install-file "../SPQR/Doc/License.txt"
                            (string-append #$output "/share/doc/"
                                           #$name "-" #$version)))))))
    (inputs (list openblas
                  suitesparse-amd
                  suitesparse-camd
                  suitesparse-ccolamd
                  suitesparse-colamd
                  suitesparse-config))
    (propagated-inputs (list suitesparse-cholmod))
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-epsf))))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Sparse QR factorization method")
    (description "The SPQR (SuiteSparseQR) package provides sparse QR
factorization based on the multifrontal method, using BLAS for the frontal
matrices.")
    (license license:gpl2+)))

(define-public suitesparse-umfpack
  (package
    (name "suitesparse-umfpack")
    (version "6.2.0")
    (source suitesparse-source)
    (build-system cmake-build-system)
    (arguments
     (list
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'chdir
            (lambda _
              (chdir "UMFPACK")))
          (add-after 'chdir 'set-cmake-module-path
            (lambda _
              (substitute* "CMakeLists.txt"
                (("set.*CMAKE_MODULE_PATH.*")
                 (string-append
                  "set(CMAKE_MODULE_PATH "
                  (string-join
                   (map (lambda (path)
                          (string-append path "/lib/cmake/SuiteSparse"))
                        (list #$suitesparse-amd
                              #$suitesparse-camd
                              #$suitesparse-ccolamd
                              #$suitesparse-cholmod
                              #$suitesparse-colamd
                              #$suitesparse-config)))
                  ")\nset(DUMMY\n")))))
          (add-after 'build 'build-doc
            (lambda _
              (with-directory-excursion "../UMFPACK/Doc"
                (invoke "make"))))
          (add-after 'install 'install-doc
            (lambda _
              (let ((outdir
                     (string-append #$output "/share/doc/"
                                    #$name "-" #$version)))
                (install-file "../UMFPACK/Doc/UMFPACK_UserGuide.pdf" outdir)
                (install-file "../UMFPACK/Doc/UMFPACK_QuickStart.pdf" outdir))))
          (replace 'install-license-files
            (lambda _
              (let ((outdir
                     (string-append #$output "/share/doc/"
                                    #$name "-" #$version)))
                (install-file "../UMFPACK/Doc/License.txt" outdir)
                (install-file "../UMFPACK/Doc/gpl.txt" outdir)))))))
    (inputs (list openblas
                  suitesparse-camd
                  suitesparse-ccolamd
                  suitesparse-cholmod
                  suitesparse-colamd))
    (propagated-inputs
     (list suitesparse-amd
           suitesparse-config))
    (native-inputs
     (list (texlive-updmap.cfg
            (list texlive-etoolbox
                  texlive-framed
                  texlive-mdframed
                  texlive-needspace
                  texlive-xcolor
                  texlive-zref))))
    (home-page "https://people.engr.tamu.edu/davis/suitesparse.html")
    (synopsis "Routines for solving sparse linear problems via LU factorization")
    (description "UMFPACK is a set of routines for solving unsymmetric sparse
linear systems using the Unsymmetric MultiFrontal method and direct sparse LU
factorization.")
    (license license:gpl2+)))

(define-public suitesparse
  (package
    (name "suitesparse")
    (version "5.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/DrTimothyAldenDavis/SuiteSparse")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zwri246yr39p9ymjp18dzv36ch0dn107sf0jghj7capigasfxq2"))
       (patches (search-patches "suitesparse-mongoose-cmake.patch"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled metis source
        '(begin
           (delete-file-recursively "metis-5.1.0")
           #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ;no "check" target
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             "TBB=-ltbb"
             "MY_METIS_LIB=-lmetis"

             ;; The default is to link against netlib lapack.  Use OpenBLAS
             ;; instead.
             "BLAS=-lopenblas" "LAPACK=-lopenblas"

             ;; Flags for cmake (required to build GraphBLAS and Mongoose)
             (string-append "CMAKE_OPTIONS=-DCMAKE_INSTALL_PREFIX="
                            (assoc-ref %outputs "out")
                            " -DCMAKE_VERBOSE_MAKEFILE=ON"
                            " -DCMAKE_C_FLAGS_RELEASE=\"$(CFLAGS) $(CPPFLAGS)\""
                            " -DCMAKE_CXX_FLAGS_RELEASE=\"$(CXXFLAGS) $(CPPFLAGS)\""
                            " -DCMAKE_SKIP_RPATH=TRUE"
                            " -DCMAKE_BUILD_TYPE=Release"
                            " -DCMAKE_INSTALL_LIBDIR=lib")
             (string-append "INSTALL_LIB="
                            (assoc-ref %outputs "out") "/lib")
             (string-append "INSTALL_INCLUDE="
                            (assoc-ref %outputs "out") "/include")
             "library")
       #:phases
       (modify-phases %standard-phases
         ,@(if (target-riscv64?)
             ;; GraphBLAS FTBFS on riscv64-linux
             `((add-after 'unpack 'skip-graphblas
                 (lambda _
                   (substitute* "Makefile"
                     ((".*cd GraphBLAS.*") "")
                     (("metisinstall gbinstall moninstall")
                     "metisinstall moninstall")))))
             '())
         (delete 'configure))))         ;no configure script
    (inputs
     (list tbb openblas gmp mpfr metis))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("m4" ,m4)))
    (home-page "https://faculty.cse.tamu.edu/davis/suitesparse.html")
    (synopsis "Suite of sparse matrix software")
    (description
     "SuiteSparse is a suite of sparse matrix algorithms, including: UMFPACK,
multifrontal LU factorization; CHOLMOD, supernodal Cholesky; SPQR,
multifrontal QR; KLU and BTF, sparse LU factorization, well-suited for circuit
simulation; ordering methods (AMD, CAMD, COLAMD, and CCOLAMD); CSparse and
CXSparse, a concise sparse Cholesky factorization package; and many other
packages.

This package contains all of the above-mentioned parts.
")
    ;; LGPLv2.1+:
    ;;   AMD, CAMD, BTF, COLAMD, CCOLAMD, CSparse, CXSparse, KLU, LDL
    ;; GPLv2+:
    ;;  GPUQREngine, RBio, SuiteSparse_GPURuntime, SuiteSparseQR, UMFPACK
    (license (list license:gpl2+ license:lgpl2.1+))))

(define-public cglm
  (package
    (name "cglm")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/recp/cglm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zgckh56vcdar3a4n51r84wrizyd2ssqal4nsvxd4qdjm0rvb4h0"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dbuild_tests=true")))
    (home-page "https://github.com/recp/cglm")
    (synopsis "Mathematics C library for graphics programming")
    (description
     "@acronym{CGLM, C OpenGL Mathematics} is an optimised 3D maths library
for graphics software based on the @acronym{GLSL, OpenGL Shading Language}
specifications.

It's similar to the original C++ GLM library but written in C99 and compatible
with C89.")
    (license license:expat)))

(define-public glm
  (package
    (name "glm")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/g-truc/glm")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0890rvv3czi3nqj11dc2m3wcdfv0dm0nr63wfcpfikk9sk6b4w8s"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'post-install
            (lambda _
              (let* ((doc (string-append #$output "/share/doc/glm"))
                     (pkgconfig (string-append #$output "/lib/pkgconfig")))
                (mkdir-p doc)
                (mkdir-p pkgconfig)
                (copy-recursively "../source/doc/api" (string-append doc "/html"))
                (install-file "../source/doc/manual.pdf" doc)
                (call-with-output-file (string-append pkgconfig "/glm.pc")
                  (lambda (port)
                    (format port
                            "prefix=~a
includedir=${prefix}/include

Name: GLM
Description: OpenGL Mathematics
Version: ~a
Cflags: -I${includedir}~%" #$output #$version)))))))))
    (native-inputs
     (list unzip))
    (home-page "https://glm.g-truc.net/")
    (synopsis "OpenGL Mathematics library")
    (description "OpenGL Mathematics (GLM) is a header-only C++ mathematics
library for graphics software based on the OpenGL Shading Language (GLSL)
specifications.")
    (license license:expat)))

(define-public lpsolve
  (package
    (name "lpsolve")
    (version "5.5.2.5")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/lpsolve/lpsolve/" version
                          "/lp_solve_" version "_source.tar.gz"))
      (sha256
       (base32
        "12pj1idjz31r7c2mb5w03vy1cmvycvbkx9z29s40qdmkp1i7q6i0"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (substitute* (list "lp_solve/ccc" "lpsolve55/ccc")
            (("^c=cc") "c=gcc")
            ;; Pretend to be on a 64 bit platform to obtain a common directory
            ;; name for the build results on all architectures; nothing else
            ;; seems to depend on it.
            (("^PLATFORM=.*$") "PLATFORM=ux64\n")

            ;; The check for 'isnan' as it is written fails with
            ;; "non-floating-point argument in call to function
            ;; ‘__builtin_isnan’", which leads to the 'NOISNAN' cpp macro
            ;; definition, which in turn leads to bad things.  Fix the feature
            ;; test.
            (("isnan\\(0\\)") "isnan(0.)"))
          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)            ; no configure script
         (replace 'build
           (lambda _
             (with-directory-excursion "lpsolve55"
               (invoke "bash" "ccc"))
             (with-directory-excursion "lp_solve"
               (invoke "bash" "ccc"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (lib (string-append out "/lib"))
                    ;; This is where LibreOffice expects to find the header
                    ;; files, and where they are installed by Debian.
                    (include (string-append out "/include/lpsolve")))
               (install-file "lpsolve55/bin/ux64/liblpsolve55.a" lib)
               (install-file "lpsolve55/bin/ux64/liblpsolve55.so" lib)
               (install-file "lp_solve/bin/ux64/lp_solve" bin)

               ;; Install a subset of the header files as on Debian
               ;; (plus lp_bit.h, which matches the regular expression).
               (for-each (lambda (name)
                           (install-file name include))
                         (find-files "." "lp_[HMSa-z].*\\.h$"))
               (with-directory-excursion "shared"
                 (for-each (lambda (name)
                             (install-file name include))
                           (find-files "." "\\.h$")))
               #t))))))
    (home-page "https://lpsolve.sourceforge.net/")
    (synopsis "Mixed integer linear programming (MILP) solver")
    (description
     "lp_solve is a mixed integer linear programming solver based on the
revised simplex and the branch-and-bound methods.")
    (license license:lgpl2.1+)))

;; Private Trilinos package for dealii-openmpi (similar to
;; trilinos-serial-xyce and trilinos-parallel-xyce).
;; This version is the latest known to be compatible with deal.II [1].
;; Since the latest version of Trilinos is not necessarily supported by
;; deal.II, it may be worth keeping this package even if and when Trilinos
;; gets packaged separately for Guix (unless various versions of Trilinos are
;; packaged).
;;
;; An insightful source of information for building Trilinos for deal.II lies
;; in the Trilinos package for candi [2], which is a source-based installer
;; for deal.II and its dependencies.
;;
;; [1]: https://www.dealii.org/current/external-libs/trilinos.html
;; [2]: https://github.com/dealii/candi/blob/master/deal.II-toolchain/packages/trilinos.package
(define trilinos-for-dealii-openmpi
  (package
    (name "trilinos-for-dealii-openmpi")
    (version "12.18.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/trilinos/Trilinos/")
             (commit
              (string-append "trilinos-release-"
                             (string-replace-substring version "." "-")))))
       (file-name (git-file-name "trilinos" version))
       (sha256
        (base32 "0fnwlhzsh85qj38cq3igbs8nm1b2jdgr2z734sapmyyzsy21mkgp"))
       (patches
        (search-patches "teuchos-remove-duplicate-using.patch"
                        "tpetra-remove-duplicate-using.patch"))))
    (build-system cmake-build-system)
    (native-inputs
     (list
      gfortran
      ;; Trilinos's repository contains several C-shell scripts, but adding
      ;; tcsh to the native inputs does not result in the check phase running
      ;; any more tests than without it (nor is tcsh required to build
      ;; Trilinos).
      ;; It seems that Trilinos has replaced its use of C-shell test scripts
      ;; with CMake's testing facilities.
      ;; For example,
      ;; packages/zoltan/doc/Zoltan_html/dev_html/dev_test_script.html [1]
      ;; states that Zoltan's C-shell test script
      ;; packages/zoltan/test/test_zoltan has been obsoleted by the tests now
      ;; performed through CMake.
      ;;
      ;; Perl is required for some Zoltan tests and Python 2 for one ML test.
      ;;
      ;; [1]: https://cs.sandia.gov/zoltan/dev_html/dev_test_script.html
      perl
      python-2))
    (inputs
     (list openblas
           lapack
           mumps-openmpi
           scalapack))
    (propagated-inputs
     (list openmpi))
    (arguments
     (list #:build-type "Release"
           #:configure-flags
           #~(list "-DBUILD_SHARED_LIBS=ON"
                   ;; Obtain the equivalent of RelWithDebInfo but with -O3
                   ;; (the Release default) rather than -O2 (the
                   ;; RelWithDebInfo default), to conform to candi's
                   ;; trilinos.package's compilation flags, which are -g -O3.
                   "-DCMAKE_C_FLAGS=-g"
                   "-DCMAKE_CXX_FLAGS=-g"
                   "-DCMAKE_Fortran_FLAGS=-g"

                   ;; Trilinos libraries that deal.II can interface with.
                   "-DTrilinos_ENABLE_Amesos=ON"
                   "-DTrilinos_ENABLE_AztecOO=ON"
                   "-DTrilinos_ENABLE_Epetra=ON"
                   "-DTrilinos_ENABLE_EpetraExt=ON"
                   "-DTrilinos_ENABLE_Ifpack=ON"
                   "-DTrilinos_ENABLE_ML=ON"
                   "-DTrilinos_ENABLE_MueLu=ON"
                   "-DTrilinos_ENABLE_ROL=ON"
                   ;; Optional; required for deal.II's GridIn::read_exodusii,
                   ;; but depends on netcdf.
                   ;; Enable if and when someone needs it.
                   ;;"-DTrilinos_ENABLE_SEACAS=ON"
                   "-DTrilinos_ENABLE_Sacado=ON"
                   "-DTrilinos_ENABLE_Teuchos=ON"
                   "-DTrilinos_ENABLE_Tpetra=ON"
                   "-DTrilinos_ENABLE_Zoltan=ON"

                   ;; Third-party libraries (TPLs) that Trilinos can interface
                   ;; with.
                   "-DBLAS_LIBRARY_NAMES=openblas"
                   "-DTPL_ENABLE_MPI=ON"
                   "-DTPL_ENABLE_MUMPS=ON"
                   "-DTPL_ENABLE_SCALAPACK=ON"

                   ;; Enable the tests but not the examples (which are enabled
                   ;; by default when enabling tests).
                   ;; Although some examples are run as tests, they are
                   ;; otherwise unnecessary since this is a private package
                   ;; meant for dealii-openmpi.
                   ;; Besides, some MueLu and ROL examples require a lot of
                   ;; memory to compile.
                   ;;
                   ;; (For future reference, note that some ROL and SEACAS
                   ;; examples require removing gfortran from
                   ;; CPLUS_INCLUDE_PATH as in the dune-istl,
                   ;; dune-localfunctions and dune-alugrid packages.)
                   "-DTrilinos_ENABLE_TESTS=ON"
                   "-DTrilinos_ENABLE_EXAMPLES=OFF"
                   ;; MueLu tests require considerably more time and memory to
                   ;; compile than the rest of the tests.
                   "-DMueLu_ENABLE_TESTS=OFF"

                   ;; The following options were gleaned from candi's
                   ;; trilinos.package.
                   ;; (We do not enable the complex instantiations, which are
                   ;; anyway provided only as an option in trilinos.package,
                   ;; because they are costly in compilation time and memory
                   ;; usage, and disk space [1].)
                   ;;
                   ;; [1]: https://www.docs.trilinos.org/files/TrilinosBuildReference.html#enabling-float-and-complex-scalar-types
                   "-DTrilinos_ENABLE_Ifpack2=OFF"
                   "-DTeuchos_ENABLE_FLOAT=ON"
                   "-DTpetra_INST_INT_LONG=ON"
                   "-DTPL_ENABLE_Boost=OFF")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'configure 'fix-kokkos-config
                 (lambda _
                   ;; GNU Make 4.3 accidentally leaves the backslash preceding
                   ;; the number sign in strings containing a literal
                   ;; backslash–number sign (\#) [1, 2].
                   ;; This is still an issue in Trilinos 13.0.1, but should be
                   ;; fixed in the following version.
                   ;; (The latest versions of Kokkos incorporate the fix [2].)
                   ;;
                   ;; [1]: https://github.com/GEOSX/thirdPartyLibs/issues/136
                   ;; [2]: https://github.com/kokkos/kokkos/blob/3.4.00/Makefile.kokkos#L441
                   (substitute* "KokkosCore_config.h"
                     (("\\\\#") "#"))))
               (add-before 'check 'mpi-setup
                 #$%openmpi-setup))))
    (home-page "https://trilinos.github.io/")
    (synopsis "Algorithms for engineering and scientific problems")
    (description
     "The Trilinos Project is an effort to develop algorithms and enabling
technologies within an object-oriented software framework for the solution of
large-scale, complex multi-physics engineering and scientific problems.
A unique design feature of Trilinos is its focus on packages.")
    ;; The packages are variously licensed under more than just BSD-3 and
    ;; LGPL-2.1+, but all the licenses are either BSD- or LGPL-compatible.
    ;; See https://trilinos.github.io/license.html.
    (license (list license:bsd-3 license:lgpl2.1+))))

(define-public dealii
  (package
    (name "dealii")
    (version "9.6.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/dealii/dealii/releases/"
                           "download/v" version "/dealii-" version ".tar.gz"))
       (sha256
        (base32 "1vbvw76xv8h1diwfgybgarm7qwn51rxd1kp2jgy2rvcfxgq26lv7"))
       (modules '((guix build utils)))
       (snippet
        ;; Remove bundled boost, Kokkos, muparser, TBB and UMFPACK.
        #~(delete-file-recursively "bundled"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc"))
    (native-inputs
     ;; Required to build the documentation.
     (list graphviz doxygen perl))
    (inputs
     (list arpack-ng
           openblas
           gfortran
           muparser
           zlib))
    (propagated-inputs
     ;; Some scripts are installed into share/deal.II/scripts that require
     ;; perl and python, but they are not executable (and some are missing the
     ;; shebang line) and therefore must be explicitly passed to the
     ;; interpreter.
     ;; Anyway, they are meant to be used at build time, so rather than adding
     ;; the interpreters here, any package depending on them should just add
     ;; the requisite interpreter to its native inputs.
     (list boost
           hdf5
           kokkos
           suitesparse                  ; For UMFPACK.
           sundials
           tbb))
    (arguments
     (list #:build-type "DebugRelease"  ; Only Debug, Release or DebugRelease.
           ;; The tests take too long and must be explicitly enabled with
           ;; "make setup_tests".
           ;; See https://www.dealii.org/developer/developers/testsuite.html.
           ;; (They can also be run for an already installed deal.II.)
           #:tests? #f
           #:configure-flags
           #~(let ((doc (string-append #$output:doc "/share/doc/"
                                       #$name "-" #$version)))
               (list "-DDEAL_II_COMPONENT_DOCUMENTATION=ON"
                     (string-append "-DDEAL_II_DOCREADME_RELDIR=" doc)
                     (string-append "-DDEAL_II_DOCHTML_RELDIR=" doc "/html")
                     ;; Don't compile the examples because the source and
                     ;; CMakeLists.txt are installed anyway, allowing users to
                     ;; do so for themselves.
                     "-DDEAL_II_COMPILE_EXAMPLES=OFF"
                     (string-append "-DDEAL_II_EXAMPLES_RELDIR=" doc
                                    "/examples")))
           #:phases
           #~(modify-phases %standard-phases
               ;; Without unsetting CPATH, the build fails with the following
               ;; error (similar to <https://bugs.gnu.org/30756>):
               ;;
               ;;   /gnu/store/…-gcc-11.3.0/include/c++/math.h:30:16: fatal error: math.h: No such file or directory
               ;;      30 | # include_next <math.h>
               ;;         |                ^~~~~~~~
               ;;
               ;; Why does unsetting CPATH magically fix the error?
               ;; TODO: Properly fix this issue.
               (add-after 'set-paths 'unset-cpath
                 (lambda _
                   (unsetenv "CPATH")))
               (add-after 'install 'remove-build-logs
                 ;; These build logs leak the name of the build directory by
                 ;; storing the values of CMAKE_SOURCE_DIR and
                 ;; CMAKE_BINARY_DIR.
                 (lambda _
                   (let ((doc (string-append #$output:doc "/share/doc/"
                                             #$name "-" #$version)))
                     (for-each delete-file
                               (map (lambda (f) (string-append doc "/" f))
                                    '("detailed.log" "summary.log")))))))))
    (home-page "https://www.dealii.org/")
    (synopsis "Finite element library")
    (description
     "Deal.II is a C++ program library targeted at the computational solution
of partial differential equations using adaptive finite elements.  The main
aim of deal.II is to enable rapid development of modern finite element codes,
using among other aspects adaptive meshes and a wide array of tools often used
in finite element programs.")
    (license license:lgpl2.1+)))

(define-public dealii-openmpi
  (package/inherit dealii
    (name "dealii-openmpi")
    (inputs
     (modify-inputs (package-inputs dealii)
       (delete "arpack")
       (prepend arpack-ng-openmpi
                metis
                scalapack)))
    (propagated-inputs
     (modify-inputs (package-propagated-inputs dealii)
       (delete "hdf5" "kokkos" "sundials")
       (prepend hdf5-parallel-openmpi
                openmpi
                p4est-openmpi
                petsc-openmpi
                slepc-openmpi
                sundials-openmpi
                trilinos-for-dealii-openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments dealii)
       ((#:configure-flags flags)
        #~(cons "-DDEAL_II_WITH_MPI=ON" #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            ;; The build failure fixed by this phase does not manifest when
            ;; Kokkos is included via Trilinos.
            (delete 'unset-cpath)))))
    (synopsis "Finite element library (with MPI support)")))

(define-public flann
  (package
    (name "flann")
    (version "1.9.1")
    (home-page "https://github.com/mariusmuja/flann/")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference (url home-page) (commit version)))
        (file-name (git-file-name name version))
        (sha256
          (base32
           "0p56fl2yx1r86ds1mgjq40926jdcgq3hka7p3l1hv2acv9jxp15x"))
        (patches (search-patches "flann-cmake-3.11.patch"))))
    (build-system cmake-build-system)
    (outputs '("out"))
    (native-inputs
     (list unzip))
    (inputs
     `(("hdf5" ,hdf5)
       ;; FIXME: 'mkoctfile' fails with a linker error:
       ;;  ld: cannot find -loctinterp
       ;;  ld: cannot find -loctave
       ;; Disable it for now.
       ;;("octave" ,octave-cli)
       ("python" ,python-2) ; print syntax
       ("zlib" ,zlib)))
    (arguments
     `(;; The 'share/flann/octave' contains a .mex file, which is an ELF file
       ;; taken 46 MiB unstripped, and 6 MiB stripped.
       #:strip-directories '("lib" "lib64" "libexec"
                             "bin" "sbin" "share/flann/octave")

       ;; Save 12 MiB by not installing .a files.  Passing
       ;; '-DBUILD_STATIC_LIBS=OFF' has no effect.
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'remove-static-libraries
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out"))
                             (lib (string-append out "/lib")))
                        (for-each delete-file
                                  (find-files lib "\\.a$"))
                        #t))))

       #:tests? #f)) ; The test data are downloaded from the Internet.
    (synopsis "Library for approximate nearest neighbors computation")
    (description "FLANN is a library for performing fast approximate
nearest neighbor searches in high dimensional spaces.  It implements a
collection of algorithms and a system for automatically choosing the best
algorithm and optimum parameters depending on the dataset.

FLANN is written in C++ and contains bindings for C, Octave and Python.")
    (license (license:non-copyleft "file://COPYING"
                                "See COPYING in the distribution."))))

(define-public wcalc
  (package
    (name "wcalc")
    (version "2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://sourceforge/w-calc/Wcalc/" version "/"
                            "wcalc-" version ".tar.bz2"))
        (sha256
         (base32
          "1vi8dl6rccqiq1apmpwawyg2ywx6a1ic1d3cvkf2hlwk1z11fb0f"))
        (snippet
         #~(begin
             (for-each delete-file
                       (list "src/common/scanner.c"
                             "src/common/parser.c"
                             "src/common/parser.h"))))))
    (build-system gnu-build-system)
    (inputs
     (list mpfr readline))
    (native-inputs
     (list bison flex))
    (home-page "https://w-calc.sourceforge.net/index.php")
    (synopsis "Flexible command-line scientific calculator")
    (description "Wcalc is a very capable calculator.  It has standard functions
(sin, asin, and sinh for example, in either radians or degrees), many
pre-defined constants (pi, e, c, etc.), support for using variables, \"active\"
variables, a command history, hex/octal/binary input and output, unit
conversions, embedded comments, and an expandable expression entry field.  It
evaluates expressions using the standard order of operations.")
    (license license:gpl2+)))

(define-public xaos
  (package
    (name "xaos")
    (version "4.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xaos-project/XaoS")
                    (commit (string-append "release-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0imq6rvvjwjgmrfr25yr5lmhmqr4s6a5174jhah90mhf7pb62j0i"))))
    (build-system gnu-build-system)
    (native-inputs `(("gettext" ,gettext-minimal)
                     ("qtbase" ,qtbase)
                     ("qttools" ,qttools)))
    (inputs (list libx11 zlib libpng gsl))
    ;; The upstream project file ("XaoS.pro") and the Makefile it generates are
    ;; not enough for this package to install properly.  These phases fix that.
    (arguments
     (list #:tests? #f ;no "check" target
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'make-qt-deterministic
                 (lambda _
                   ;; Make Qt deterministic.
                   (setenv "QT_RCC_SOURCE_DATE_OVERRIDE" "1")))
               (replace 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out")))
                     (substitute* "XaoS.pro"
                       ;; The DESTDIR is originally set to install the xaos binary to
                       ;; the "bin" folder inside the build directory.  Setting make
                       ;; flags doesn't seem to change this.
                       (("DESTDIR.*$")
                        (string-append "DESTDIR=" out "/bin"))
                       (("/usr/local")
                        out)
                       ;; Set the correct path to the lrelease binary.
                       (("lrelease-qt6") "lrelease"))
                     (substitute* "src/include/config.h"
                       (("/usr/share/XaoS")
                        (string-append out "/share/XaoS")))
                     (invoke "qmake"))))
               (add-after 'install 'install-data
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out"))
                          (share (string-append out "/share")))
                     (mkdir-p share)
                     (for-each
                      (lambda (folder)
                        (copy-recursively folder
                                          (string-append share "/XaoS/" folder)))
                      '("catalogs" "examples" "tutorial"))
                     (install-file "xdg/xaos.png"
                                   (string-append share "/pixmaps"))
                     (install-file "xdg/io.github.xaos_project.XaoS.desktop"
                                   (string-append share "/applications"))))))))
    (synopsis "Real-time fractal zoomer")
    (description "GNU XaoS is a graphical program that generates fractal
patterns and allows you to zoom in and out of them infinitely in a fluid,
continuous manner.  It also includes tutorials that help to explain how fractals
are built.  It can generate many different fractal types such as the Mandelbrot
set.")
    (home-page "https://xaos-project.github.io/")
    (license license:gpl2+)))

(define-public hypre
  (package
    (name "hypre")
    (version "2.32.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hypre-space/hypre")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hlydh15wz0yv5bgry7yyx4pvrl656mifhqjgifzf6dyksfvwpl7"))))
    (build-system gnu-build-system)
    (outputs '("out"                    ;5.3 MiB of headers and libraries
               "doc"))                  ;12 MiB of documentation
    (native-inputs
     (list doc++
           doxygen
           ghostscript
           python
           python-breathe
           python-sphinx
           python-sphinx-rtd-theme
           (texlive-updmap.cfg
            (list texlive-adjustbox
                  texlive-alphalph
                  texlive-capt-of
                  texlive-caption
                  texlive-changepage
                  texlive-cmap
                  texlive-courier
                  texlive-enumitem
                  texlive-etoc
                  texlive-etoolbox
                  texlive-fancyvrb
                  texlive-float
                  texlive-fncychap
                  texlive-framed
                  texlive-hanging
                  texlive-helvetic
                  texlive-jknapltx
                  texlive-latexmk
                  texlive-listofitems
                  texlive-multirow
                  texlive-natbib
                  texlive-needspace
                  texlive-newunicodechar
                  texlive-parskip
                  texlive-sectsty
                  texlive-stackengine
                  texlive-tabulary
                  texlive-tex-gyre
                  texlive-titlesec
                  texlive-tocloft
                  texlive-ulem
                  texlive-upquote
                  texlive-varwidth
                  texlive-wasy
                  texlive-wasysym
                  texlive-wrapfig
                  texlive-xcolor
                  texlive-xypic))))
    (inputs
     (list openblas))
    (arguments
     (list #:modules `((srfi srfi-1)
                       ,@%default-gnu-modules)
           #:configure-flags #~'("--enable-shared"
                                 "--disable-fortran"
                                 "--without-MPI"
                                 "--with-openmp"
                                 "--with-fei"
                                 "--with-lapack"
                                 "--with-blas")
           #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'chdir-src
                 (lambda _
                   (chdir "src")))
               (replace 'configure
                 (lambda* (#:key build target configure-flags
                           #:allow-other-keys #:rest args)
                   (let* ((configure (assoc-ref %standard-phases 'configure)))
                     (apply configure
                            (append args
                                    (list #:configure-flags
                                          (cons (string-append "--host="
                                                               (or target build))
                                                configure-flags)))))))
               (add-after 'build 'build-docs
                 (lambda _
                   (setenv "HOME" (getcwd))
                   (invoke "make" "-C" "docs")))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (setenv "LD_LIBRARY_PATH"
                             (string-append (getcwd) "/hypre/lib"))
                     (setenv "PATH"
                             (string-append "." ":" (getenv "PATH")))
                     (invoke "make" "check" "CHECKRUN=")
                     (for-each (lambda (filename)
                                 (let ((size (stat:size (stat filename))))
                                   (when (positive? size)
                                     (error (format #f
                                                    "~a size ~d; error indication~%"
                                                    filename size)))))
                               (find-files "test" ".*\\.err$")))))
               (add-after 'install 'install-docs
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Custom install because docs/Makefile doesn't honor ${docdir}.
                   (let* ((doc (assoc-ref outputs "doc"))
                          (docdir (string-append doc "/share/doc/hypre-"
                                                 #$version)))
                     (with-directory-excursion "docs"
                       (for-each (lambda (base)
                                   (install-file (string-append base
                                                                ".pdf") docdir)
                                   (copy-recursively (string-append base
                                                                    "-html")
                                                     (string-append docdir
                                                                    "/" base)))
                                 '("usr-manual" "ref-manual")))))))))
    (home-page "https://computing.llnl.gov/projects\
/hypre-scalable-linear-solvers-multigrid-methods")
    (synopsis "Library of solvers and preconditioners for linear equations")
    (description
     "HYPRE is a software library of high performance preconditioners and
solvers for the solution of large, sparse linear systems of equations.  It
features multigrid solvers for both structured and unstructured grid
problems.")
    (properties '((tunable? . #t)))
    (license license:lgpl2.1)))

(define-public hypre-openmpi
  (package
    (inherit hypre)
    (name "hypre-openmpi")
    (inputs (modify-inputs (package-inputs hypre)
              (prepend openmpi)))
    (arguments
     (substitute-keyword-arguments (package-arguments hypre)
       ((#:configure-flags flags)
        #~`("--with-MPI" ,@(delete "--without-MPI" #$flags)))
       ((#:phases phases)
        #~(modify-phases #$phases
            (add-before 'check 'mpi-setup
              #$%openmpi-setup)))))
    (synopsis "Parallel solvers and preconditioners for linear equations")
    (description
     "HYPRE is a software library of high performance preconditioners and
solvers for the solution of large, sparse linear systems of equations on
parallel computers.  It features parallel multigrid solvers for both
structured and unstructured grid problems.")))

(define-public matio
  (package
    (name "matio")
    (version "1.5.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/matio/matio/" version "/"
                           "matio-" version ".tar.gz"))
       (sha256
        (base32
         "0vjdkxn402gwrgbi5ii3n2ai01bjzzfb588iqd9ylinzc7kfm4cz"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'install 'install-matioConfig.h
            (lambda _
              (install-file "src/matioConfig.h"
                            (string-append #$output "/include")))))))
    (inputs
     (list zlib hdf5))
    (home-page "http://matio.sourceforge.net/")
    (synopsis "Library for reading and writing MAT files")
    (description "Matio is a library for reading and writing MAT files.  It
supports compressed MAT files, as well as newer (version 7.3) MAT files.")
    (license license:bsd-2)))

(define-public vc
  (package
    (name "vc")
    (version "1.4.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/VcDevel/Vc/releases/"
                           "download/" version "/Vc-" version ".tar.gz"))
       (sha256
        (base32 "0zq37r8yisd4dwlb024l10wk2yq9kisa4xm79ia1ggrz7w2s13lq"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DBUILD_TESTING=ON"
              ;; By default, Vc will optimize for the CPU of the build machine.
              ;; Setting this to "none" makes it create portable binaries.  See
              ;; "cmake/OptimizeForArchitecture.cmake".
              "-DTARGET_ARCHITECTURE=none")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-testdata
            (lambda _
              (copy-recursively #$(this-package-native-input "testdata")
                                "tests/testdata"))))))
    (native-inputs
     `(("virtest" ,virtest)

       ;; This is a submodule in the git project, but not part of the
       ;; released sources.  See the git branch for the commit to take.
       ("testdata"
        ,(let ((commit "9ada1f34d6a41f1b5553d6223f277eae72c039d3"))
           (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/VcDevel/vc-testdata")
                   (commit "9ada1f34d6a41f1b5553d6223f277eae72c039d3")))
             (file-name (git-file-name "vc-testdata" (string-take commit 7)))
             (sha256
              (base32
               "1hkhqib03qlcq412ym2dciynfxcdr2ygqhnplz4l1vissr1wnqn2")))))))
    (synopsis "SIMD vector classes for C++")
    (description "Vc provides portable, zero-overhead C++ types for explicitly
data-parallel programming.  It is a library designed to ease explicit
vectorization of C++ code.  Its types enable explicitly stating data-parallel
operations on multiple values.  The parallelism is therefore added via the type
system.  Vc has an intuitive API and provides portability between different
compilers and compiler versions as well as portability between different vector
instruction sets.  Thus, an application written with Vc can be compiled for:
@enumerate
@item AVX and AVX2
@item SSE2 up to SSE4.2 or SSE4a
@item Scalar
@item MIC
@item NEON (in development)
@item NVIDIA GPUs / CUDA (in development)
@end enumerate\n")
    (home-page "https://github.com/VcDevel/Vc")
    ;; "No support_???.cpp file exists for this architecture."
    (supported-systems '("x86_64-linux" "i686-linux"))
    (license license:bsd-3)))

(define-public reducelcs
  ;; This is the last commit which is available upstream, no
  ;; release happened since 2010.
  (let ((commit "474f88deb968061abe8cf11c959e02319b8ae5c0")
        (revision "1"))
    (package
      (name "reducelcs")
      (version (string-append "1.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gdv/Reduce-Expand-for-LCS")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "1rllzcfwc042c336mhq262a8ha90x6afq30kvk60r7i4761j4yjm"))))
      (build-system gnu-build-system)
      (inputs
       (list openlibm))
      (arguments
       `(#:tests? #f ; no tests
         #:phases
         (modify-phases %standard-phases
           (delete 'configure) ; No configure script exists.
           (replace 'install ; No install phase exists.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (bin (string-append out "/bin")))
                 (install-file "Approximation" bin)
                 (install-file "CollectResults" bin)
                 (install-file "GenerateInstances" bin)
                 #t))))))
      (synopsis "Approximate Longest Commons Subsequence computation tool")
      (description
       "@code{reduceLCS} is an implementation of the Reduce-Expand
algorithm for LCS.  It is a fast program to compute the approximate
Longest Commons Subsequence of a set of strings.")
      (home-page "https://github.com/gdv/Reduce-Expand-for-LCS")
      (license license:gpl3+))))

(define-public jacal
  (package
    (name "jacal")
    (version "1c8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://groups.csail.mit.edu/mac/ftpdir/scm/jacal-"
                    version ".zip"))
              (sha256
               (base32 "0dn706gl5nd36177m7rkx9sdzpxy116jy2mdmc0dcb758r64qvmw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'pre-build
                     ;; Don't use upstream's script - it really doesn't fit into
                     ;; Guix's functional paradigm.
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (substitute* "Makefile"
                         (("^install: install-script") "install: "))))
         (add-after 'install 'post-install
                    ;; Instead, we provide our own simplified script.
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((wrapper (string-append (assoc-ref outputs "out")
                                                    "/bin/jacal")))
                        (format (open wrapper (logior O_WRONLY O_CREAT))
                                (string-append "#!~a\nexec ~a/bin/scm -ip1 "
                                "-e '(slib:load \"~a/lib/jacal/math\") "
                                "(math)' \"$@\"\n")
                                (which  "bash")
                                (assoc-ref inputs "scm")
                                (assoc-ref outputs "out"))
                        (chmod wrapper #o555))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "sh" "configure"
                     (string-append "--prefix="
                                    (assoc-ref outputs "out"))))))))
    (inputs (list scm))
    (native-inputs (list time unzip texinfo))
    (synopsis "Symbolic mathematics system")
    (description "GNU JACAL is an interactive symbolic mathematics program based on
Scheme.  It manipulate and simplify a range of mathematical expressions such
as equations, scalars, vectors, and matrices.")
    (home-page "https://www.gnu.org/software/jacal/")
    (license license:gpl3+)))

(define-public boolector
  (package
   (name "boolector")
   (version "3.2.2")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Boolector/boolector")
                  (commit version)))
            (file-name (git-file-name name version))
            (patches (search-patches "boolector-find-googletest.patch"))
            (sha256
             (base32
              "07rvp3iry7a7ixwl0q7nc47fwky1s1cyia7gqrjsg46syqlxbz2c"))))
   (build-system cmake-build-system)
   (arguments
    (list #:configure-flags
          #~(list "-DBUILD_SHARED_LIBS=on"
                  (string-append
                   "-DBtor2Tools_INCLUDE_DIR="
                   (dirname (search-input-file %build-inputs
                                               "include/btor2parser.h")))
                  (string-append
                   "-DBtor2Tools_LIBRARIES="
                   (search-input-file %build-inputs
                                      "lib/libbtor2parser.so")))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-cmake
                (lambda _
                  (delete-file "cmake/FindCryptoMiniSat.cmake")
                  (substitute* (list "CMakeLists.txt" "src/CMakeLists.txt")
                    (("find_package\\(CryptoMiniSat\\)")
                     "find_package(cryptominisat5 CONFIG)
find_package(louvain_communities)")
                    (("CryptoMiniSat_FOUND") "cryptominisat5_FOUND")
                    (("CryptoMiniSat_INCLUDE_DIR")
                     "CRYPTOMINISAT5_INCLUDE_DIRS")
                    (("CryptoMiniSat_LIBRARIES")
                     "CRYPTOMINISAT5_LIBRARIES"))))
              (add-after 'unpack 'fix-sources
                (lambda _
                  (substitute* (find-files "." "\\.c$")
                    (("\"btor2parser/btor2parser\\.h\"") "<btor2parser.h>")))))))
   (inputs (list btor2tools
                 boost cryptominisat louvain-community sqlite
                 gmp))
   (native-inputs (list googletest pkg-config python-wrapper))
   (home-page "https://boolector.github.io")
   (synopsis "Bitvector-based theory solver")
   (description "Boolector is a @acronym{SMT, satisfiability modulo theories}
solver for the theories of fixed-size bit-vectors, arrays and uninterpreted
functions.")
   (license license:lgpl3+)))

(define-public java-smtinterpol
  (package
   (name "java-smtinterpol")
   (version "2.5")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ultimate-pa/smtinterpol")
                  (commit version)))
            (file-name (git-file-name name version))
            (modules '((guix build utils)))
            (snippet #~(begin
                         (delete-file-recursively "jacoco")
                         (delete-file-recursively "libs")
                         (delete-file-recursively "sonar")))
            (sha256
             (base32
              "0bq5l7g830a8hxw1xyyfp2ph6jqk8ak0ichlymdglpnpngf6322f"))))
   (build-system ant-build-system)
   (arguments
    (list #:build-target "dist"
          #:test-target "runtests"
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-build.xml
                (lambda _
                  (substitute* "build.xml"
                    (("<tstamp>") "<!--")
                    (("</tstamp>") "-->")
                    (("executable=\"git\"")
                     (string-append "executable=\""
                                    (which "sh")
                                    "\""))
                    (("<property file=.*/>" all)
                     (string-append all
                                    "<property environment=\"env\" />"))
                    (("<classpath>" all)
                     (string-append
                      all
                      "<pathelement path=\"${env.CLASSPATH}\" />"))
                    (("<fileset file=\".*/libs/.*/>") "")
                    (("<junit")
                     "<junit haltonfailure=\"yes\""))
                  (call-with-output-file "describe"
                    (lambda (port)
                      (format port "echo ~a" #$version)))))
              (add-before 'check 'delete-failing-tests
                (lambda _
                  (delete-file
                   (string-append "SMTInterpolTest/src/de/uni_freiburg"
                                  "/informatik/ultimate/smtinterpol/convert/"
                                  "EqualityDestructorTest.java"))))
              (replace 'install
                (lambda* (#:key outputs #:allow-other-keys)
                  (let* ((out (assoc-ref outputs "out"))
                         (java (string-append out "/share/java")))
                    (for-each (lambda (f) (install-file f java))
                              (find-files "dist" "\\.jar$"))))))))
   (native-inputs (list java-junit))
   (home-page "http://ultimate.informatik.uni-freiburg.de/smtinterpol/")
   (synopsis "Interpolating SMT solver")
   (description "SMTInterpol is an @acronym{SMT, Satisfiability Modulo Theories}
solver, that can compute Craig interpolants for various theories.")
   (license license:lgpl3+)))

(define-public yices
  (package
   (name "yices")
   (version "2.6.4")
   (source (origin
            (method url-fetch)
            (uri (string-append "https://yices.csl.sri.com/releases/"
                                version "/yices-" version "-src.tar.gz"))
            (sha256
             (base32
              "1jvqvf35gv2dj936yzl8w98kc68d8fcdard90d6dddzc43h28fjk"))))
   (build-system gnu-build-system)
   (arguments
    (list #:configure-flags
          #~(list #$@(if (%current-target-system)
                         '()
                         (list (string-append "--build="
                                              (%current-system))))
                  "--enable-mcsat"
                  ;; XXX: Ewww, static linkage
                  (string-append
                   "--with-static-libpoly="
                   (search-input-file %build-inputs "lib/libpoly.a"))
                  (string-append
                   "--with-static-gmp="
                   (search-input-file %build-inputs "lib/libgmp.a"))
                  (string-append
                   "--with-pic-libpoly="
                   (search-input-file %build-inputs "lib/libpicpoly.a")))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'fix-build-files
                (lambda _
                  (substitute* "Makefile.build"
                    (("SHELL=.*") "")
                    (("/sbin/ldconfig") (which "ldconfig")))
                  (substitute* (find-files "etc" "install-yices.*")
                    (("/usr/bin/install") (which "install"))
                    (("/bin/ln") (which "ln"))
                    (("/sbin/ldconfig") (which "ldconfig"))
                    (("install_dir=.*")
                     (string-append "install_dir=" #$output))))))))
   (inputs (list cudd gmp gperf libpoly))
   (native-inputs (list autoconf automake bash-minimal))
   (home-page "https://yices.csl.sri.com/")
   (synopsis "Satisfiability modulo theories solver")
   (description "Yices is a solver for @acronym{SMT, satisfiability modulo
theories} problems.  It can process input in SMT-LIB format or its own
s-expression-based format.")
   (license license:gpl3+)))

(define-public z3
  (package
    (name "z3")
    (version "4.13.0")
    (home-page "https://github.com/Z3Prover/z3")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "z3-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0j46lckf3zgx2xjay7z6nvlgh47gisbbl4s3m5zn280a13fwz1ih"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:imported-modules `((guix build python-build-system)
                           ,@%cmake-build-system-modules)
      #:modules '((guix build cmake-build-system)
                  (guix build utils)
                  ((guix build python-build-system) #:select (site-packages)))
      #:configure-flags
      #~(list "-DZ3_BUILD_PYTHON_BINDINGS=ON"
              "-DZ3_LINK_TIME_OPTIMIZATION=ON"
              (string-append
               "-DCMAKE_INSTALL_PYTHON_PKG_DIR="
               #$output "/lib/python"
               #$(version-major+minor (package-version python-wrapper))
               "/site-packages"))
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key parallel-build? tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "test-z3"
                        (format #f "-j~a"
                                (if parallel-build?
                                    (parallel-job-count)
                                    1)))
                (invoke "./test-z3" "/a"))))
          (add-after 'install 'compile-python-modules
            (lambda _
              (setenv "PYTHONHASHSEED" "0")

              (invoke "python" "-m" "compileall"
                      "--invalidation-mode=unchecked-hash"
                      #$output)))
          ;; This step is missing in the CMake build system, do it here.
          (add-after 'compile-python-modules 'fix-z3-library-path
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let* ((dest (string-append (site-packages inputs outputs)
                                          "/z3/lib/libz3.so"))
                     (z3-lib (string-append #$output "/lib/libz3.so")))
                (mkdir-p (dirname dest))
                (symlink z3-lib dest)))))))
    (native-inputs
     (list which python-wrapper))
    (synopsis "Theorem prover")
    (description "Z3 is a theorem prover and @dfn{satisfiability modulo
theories} (SMT) solver.  It provides a C/C++ API, as well as Python bindings.")
    (license license:expat)))

(define-public ocaml-z3
  (package
    (inherit z3)
    (name "ocaml-z3")
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%default-gnu-imported-modules)
       #:modules (((guix build python-build-system) #:select (site-packages))
                  (guix build gnu-build-system)
                  (guix build utils))
       #:tests? #f; no ml tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'bootstrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "OCAMLFIND_LDCONF" "ignore")
               (setenv "OCAMLFIND_DESTDIR" (string-append out "/lib/ocaml/site-lib"))
               (mkdir-p (string-append out "/lib/ocaml/site-lib"))
               (substitute* "scripts/mk_util.py"
                 (("LIBZ3 = LIBZ3")
                  (string-append "LIBZ3 = LIBZ3 + ' -dllpath " out "/lib'"))
                 ;; Do not build z3 again, use the library passed as input
                 ;; instead
                 (("z3linkdep,") "\"\",")
                 (("z3linkdep)") "\"\")"))
               (invoke "python" "scripts/mk_make.py"))))
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "./configure"
                     "--ml"
                     (string-append "--prefix=" (assoc-ref outputs "out")))))
         (add-after 'configure 'change-directory
           (lambda _
             (chdir "build")
             #t))
         (replace 'build
           (lambda _
             (invoke "make" "ml")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (invoke "ocamlfind" "install" "-destdir"
                     (string-append (assoc-ref outputs "out") "/lib/ocaml/site-lib")
                     "z3" "api/ml/META" "api/ml/z3enums.mli" "api/ml/z3enums.cmi"
                     "api/ml/z3enums.cmx" "api/ml/z3native.mli"
                     "api/ml/z3native.cmi" "api/ml/z3native.cmx"
                     "../src/api/ml/z3.mli" "api/ml/z3.cmi" "api/ml/z3.cmx"
                     "api/ml/libz3ml.a" "api/ml/z3ml.a" "api/ml/z3ml.cma"
                     "api/ml/z3ml.cmxa" "api/ml/z3ml.cmxs" "api/ml/dllz3ml.so"))))))
    (native-inputs
     `(("which" ,which)
       ("python" ,python-wrapper)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)))
    (propagated-inputs
     (list ocaml-zarith))
    (inputs
     (list z3))))

(define-public elpa
  (package
    (name "elpa")
    (version "2018.11.001")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://elpa.mpcdf.mpg.de/software/"
                                  "tarball-archive/Releases/"
                                  version "/elpa-" version ".tar.gz"))
              (sha256
               (base32
                "05hv3v5i6xmziaizw350ff72y1c3k662r85fm3xfdrkclj5zw9yc"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("fortran" ,gfortran)
       ("perl" ,perl)))                 ;for configure and deps
    (inputs
     `(("blas" ,openblas)))
    (arguments
     `(#:configure-flags
       `("--enable-openmp"
         "--with-mpi=no"
         ;; ELPA unfortunately does not support runtime dispatch, so we can
         ;; only enable the "generic" kernels.  See the "Cross compilation"
         ;; section of INSTALL.md.
         "--enable-generic"
         "--disable-sse" "--disable-sse-assembly" ;Require SSE3
         "--disable-avx" "--disable-avx2" "--disable-avx512"
         ,(string-append "CFLAGS=-O3 "
                         "-funsafe-loop-optimizations -funsafe-math-optimizations "
                         "-ftree-vect-loop-version -ftree-vectorize "
                         ,(let ((system (or (%current-target-system)
                                            (%current-system))))
                            (cond
                             ((or (string-prefix? "x86_64" system)
                                  (string-prefix? "i686" system))
                              "-msse2")
                             (else "")))))
       #:parallel-tests? #f             ;tests are multi-threaded, via BLAS
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'patch-header-generation
           (lambda _
             (substitute* "configure"
               (("^  *make.*top_srcdir=\"\\$srcdir\"" &)
                (string-append & " CPP=\"$CPP\"")))
             #t))
         (add-before 'check 'setup-tests
           (lambda _
             ;; Decrease test time and RAM use by computing fewer eigenvalues.
             ;; The flags are (MATRIX-SIZE, EIGENVALUES, BLOCK-SIZE), where
             ;; the default is (500, 250, 16) for C tests and (5000, 150, 16)
             ;; for Fortran.  This also causes several tests to pass that
             ;; otherwise would otherwise fail with matrix size 5000; possibly
             ;; due to floating point tolerances that are too tight.
             (setenv "TEST_FLAGS" "1500 50 16") ;from elpa.spec
             (setenv "OMP_NUM_THREADS" (number->string (parallel-job-count)))
             (substitute* "Makefile"
               ;; Test scripts are generated, patch the shebang
               (("#!/bin/bash") (string-append "#!" (which "sh"))))
             #t)))))
    (home-page "https://elpa.mpcdf.mpg.de")
    (synopsis "Eigenvalue solvers for symmetric matrices")
    (description
     "The ELPA library provides efficient and scalable direct eigensolvers for
symmetric matrices.")
    (license license:lgpl3)))

(define-public elpa-openmpi
  (package (inherit elpa)
    (name "elpa-openmpi")
    (inputs
     `(("mpi" ,openmpi)
       ("scalapack" ,scalapack)
       ,@(package-inputs elpa)))
    (arguments
     (substitute-keyword-arguments (package-arguments elpa)
       ((#:configure-flags cf '())
        `(cons "--with-mpi=yes" (delete "--with-mpi=no" ,cf)))
       ((#:phases phases '%standard-phases)
        `(modify-phases ,phases
           (add-before 'check 'mpi-setup
             (lambda _
               ;; Tests use 2 mpi tasks by default, use our remaining build
               ;; cores as OpenMP threads.
               (setenv "OMP_NUM_THREADS" (number->string
                                          (max (quotient (parallel-job-count) 2)
                                               1)))
               (,%openmpi-setup)))))))
    (synopsis "Eigenvalue solvers for symmetric matrices (with MPI support)")))

(define-public elemental
  ;; The build of 0.87.7 is failed for a long time due to new version of GCC. The
  ;; latest commit has fixes.
  ;; See https://github.com/elemental/Elemental/issues/254
  (let ((commit "6eb15a0da2a4998bf1cf971ae231b78e06d989d9")
        (revision "0"))
    (package
      (name "elemental")
      (version (git-version "0.87.7" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/elemental/Elemental")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "06xcs4ic60ndcf2hq19gr8yjwdsnphpcyhapab41rkw726z4lm7p"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:build-type "Release" ;default RelWithDebInfo not supported
        #:configure-flags
        #~(list "-DEL_DISABLE_PARMETIS:BOOL=YES"
                "-DEL_AVOID_COMPLEX_MPI:BOOL=NO"
                "-DEL_CACHE_WARNINGS:BOOL=YES"
                "-DEL_TESTS:BOOL=YES"
                "-DCMAKE_INSTALL_LIBDIR=lib"
                "-DGFORTRAN_LIB=gfortran")
        #:phases
        #~(modify-phases %standard-phases
            (add-before 'check 'mpi-setup
                 #$%openmpi-setup)
            (add-before 'check 'setup-tests
              (lambda _                ;; Parallelism is done at the MPI layer.
                (setenv "OMP_NUM_THREADS" "1")))
            (add-after 'install 'remove-tests
              (lambda _
                ;; Tests are installed, with no easy configuration
                ;; switch to prevent this, so delete them.
                (delete-file-recursively
                 (string-append  #$output "/bin/test")))))))
      (native-inputs
       (list gfortran))
      (inputs
       (list `(,gfortran "lib")
             gmp
             metis
             mpc
             mpfr
             openmpi
             qd
             openblas))
      (home-page "https://github.com/elemental/Elemental")
      (synopsis "Dense and sparse-direct linear algebra and optimization")
      (description "Elemental is a modern C++ library for distributed-memory
dense and sparse-direct linear algebra, conic optimization, and lattice
reduction.")
      (license license:bsd-2))))

(define-public mcrl2
  (package
    (name "mcrl2")
    (version "202307.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.mcrl2.org/download/release/mcrl2-"
                    version ".tar.gz"))
              (sha256
               (base32
                "147002dl3gq2jsk5gcmvw7hj3mfxrpgxwcfda5mfrj26rcxw48fc"))))
    (inputs
     (list boost glu mesa qtbase-5))
    (build-system cmake-build-system)
    (synopsis "Toolset for the mCRL2 formal specification language")
    (description
     "@dfn{mCRL2} (micro Common Representation Language 2) is a formal
specification language for describing concurrent discrete event systems.  Its
toolset supports analysis and automatic verification, linearisation, simulation,
state-space exploration and generation, and tools to optimise and analyse
specifications.  Also, state spaces can be manipulated, visualised and
analysed.")
    (home-page "https://mcrl2.org")
    (license license:boost1.0)))

(define-public mcrl2-minimal
  (package
    (inherit mcrl2)
    (name "mcrl2-minimal")
    (inputs
     (list boost))
    (arguments
     '(#:configure-flags '("-DMCRL2_ENABLE_GUI_TOOLS=OFF")))))

(define-public tcalc
  (package
  (name "tcalc")
  (version "2.0")
  (source
    (origin
      (method url-fetch)
      (uri (string-append "https://sites.google.com/site/mohammedisam2000/tcalc/tcalc-"
                            version ".tar.gz"))
      (sha256
        (base32
          "0jq806m4dqfia85nppfm75mml9w57g0cgv4cdw9bp3zymda83s0m"))))
  (build-system gnu-build-system)
  (synopsis "The terminal calculator")
  (description
    "The terminal calculator is a small program to help users of the GNU/Linux
terminal do calculations simply and quickly.  The formula to be calculated can
be fed to @command{tcalc} through the command line.")
  (home-page "https://sites.google.com/site/mohammedisam2000/tcalc")
  (license license:gpl3+)))

(define-public tiny-bignum
  (let ((commit "1d7a1f9b8e77316187a6b3eae8e68d60a6f9a4d4"))
    (package
     (name "tiny-bignum")
     (version (git-version "0" "0" commit))
     (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://github.com/kokke/tiny-bignum-c")
              (commit commit)))
        (file-name (git-file-name "tiny-bignum" commit))
        (sha256
         (base32 "0vj71qlhlaa7d92bfar1kwqv6582dqrby8x3kdw0yzh82k2023g6"))))
     (build-system gnu-build-system)
     (arguments
      `(#:phases
        (modify-phases %standard-phases
          (delete 'configure)
          (add-after 'unpack 'patch-tests
            (lambda _
              (substitute* "scripts/test_rand.py"
                (("\t") "  ")
                (("\" % (\\w+)" _ symbol) (string-append "\" % int(" symbol ")")))
              #t))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "make" "test"))
              #t))
          (replace 'install
            (lambda* (#:key outputs #:allow-other-keys)
              (let ((share (string-append (assoc-ref outputs "out") "/share"))
                    (doc (string-append (assoc-ref outputs "out") "/doc")))
                (mkdir-p share)
                (install-file "bn.c" share)
                (install-file "bn.h" share)
                (mkdir-p doc)
                (install-file "LICENSE" doc)
                (install-file "README.md" doc))
              #t)))))
     (native-inputs
      `(("python" ,python-wrapper)))
     (home-page "https://github.com/kokke/tiny-bignum-c")
     (synopsis "Small portable multiple-precision unsigned integer arithmetic in C")
     (description
      "This library provides portable Arbitrary-precision unsigned integer
arithmetic in C, for calculating with large numbers.  Basic arithmetic (+, -,
*, /, %) and bitwise operations (&, |, ^. <<, >>) plus increments, decrements
and comparisons are supported.")
     (license license:unlicense))))

(define-public sundials
  (package
    (name "sundials")
    (version "7.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/LLNL/sundials/releases/download/v"
                           version "/sundials-" version ".tar.gz"))
       (sha256
        (base32
         "1fwlgbcdxz30dzsqw3y1b6ic1rhzfadghj7cq7yxsj14npgnwzga"))))
    (build-system cmake-build-system)
    (native-inputs
     (list pkg-config python-2)) ;for tests; syntax incompatible with Python 3
    (inputs
     (list openblas suitesparse))
    (arguments
     (list #:configure-flags
           #~(list "-DCMAKE_C_FLAGS=-O2 -g -fcommon"

                   "-DSUNDIALS_INDEX_SIZE=32"
                   ;; Incompatible with 32-bit indices.
                   ;;"-DBUILD_FORTRAN_MODULE_INTERFACE:BOOL=ON"

                   "-DEXAMPLES_ENABLE_C:BOOL=ON"
                   "-DEXAMPLES_ENABLE_CXX:BOOL=ON"
                   ;; Requires -DBUILD_FORTRAN_MODULE_INTERFACE:BOOL=ON.
                   ;;"-DEXAMPLES_ENABLE_F2003:BOOL=ON"
                   "-DEXAMPLES_INSTALL:BOOL=OFF"

                   "-DENABLE_KLU:BOOL=ON"
                   (string-append "-DKLU_INCLUDE_DIR="
                                  (dirname
                                   (search-input-file %build-inputs
                                                      "include/klu.h")))
                   (string-append "-DKLU_LIBRARY_DIR="
                                  (dirname
                                   (search-input-file %build-inputs
                                                      "lib/libklu.so"))))))
    (home-page "https://computation.llnl.gov/projects/sundials")
    (synopsis "Suite of nonlinear and differential/algebraic equation solvers")
    (description "SUNDIALS is a family of software packages implemented with
the goal of providing robust time integrators and nonlinear solvers that can
easily be incorporated into existing simulation codes.")
    (properties
     '((release-monitoring-url
        . "https://computing.llnl.gov/projects/sundials/sundials-software")))
    (license license:bsd-3)))

(define-public sundials-openmpi
  (package/inherit sundials
    (name "sundials-openmpi")
    (propagated-inputs
     (list openmpi
           ;; Support for the below requires MPI.
           hypre-openmpi
           petsc-openmpi))
    (arguments
     (substitute-keyword-arguments (package-arguments sundials)
       ((#:configure-flags flags #~())
        #~(cons* "-DENABLE_MPI:BOOL=ON"
                 "-DENABLE_HYPRE:BOOL=ON"
                 (string-append "-DHYPRE_INCLUDE_DIR="
                                (dirname
                                 (search-input-file %build-inputs
                                                    "include/HYPRE.h")))
                 (string-append "-DHYPRE_LIBRARY_DIR="
                                (dirname
                                 (search-input-file %build-inputs
                                                    "lib/libHYPRE.so")))
                 "-DENABLE_PETSC:BOOL=ON"
                 (string-append "-DPETSC_INCLUDE_DIRS="
                                (dirname
                                 (search-input-file %build-inputs
                                                    "include/petsc.h")))
                 #$flags))
       ((#:phases phases #~%standard-phases)
        #~(modify-phases #$phases
            (add-before 'check 'mpi-setup
              #$%openmpi-setup)))))
    (synopsis "SUNDIALS with MPI support")))

(define-public sundials-5
  (package
    (inherit sundials)
    (name "sundials")
    (version "5.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/LLNL/sundials/releases/download/v"
                           version "/sundials-" version ".tar.gz"))
       (sha256
        (base32
         "04x2x0jchf9kbcw2a1c6f4h4as8sr6k2snfz8z9k897pa4rl1vfl"))))))

(define-public sundials-openmpi-5
  (package/inherit sundials-5
    (name "sundials-openmpi")
    (propagated-inputs
     (package-propagated-inputs sundials-openmpi))
    (arguments
     (package-arguments sundials-openmpi))
    (synopsis (package-synopsis sundials-openmpi))))

(define-public sundials-julia
  (package
    (inherit sundials)
    (name "sundials-julia")
    (version "5.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/LLNL/sundials")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0nx4sqhmi126m14myzm7syv2053harav9snl0a247wnkcgs5rxrv"))))
    (inputs
     (modify-inputs (package-inputs sundials)
       (prepend gfortran openblas)))
    (arguments
     '(#:configure-flags `("-DCMAKE_C_FLAGS=-O2 -g -fcommon"
                           "-DSUNDIALS_INDEX_SIZE=32"
                           "-DKLU_ENABLE:BOOL=ON"
                           ,(string-append "-DKLU_INCLUDE_DIR="
                                           (assoc-ref %build-inputs "suitesparse")
                                           "/include")
                           ,(string-append "-DKLU_LIBRARY_DIR="
                                           (assoc-ref %build-inputs "suitesparse")
                                           "/lib")
                           "-DLAPACK_ENABLE:BOOL=ON")))
    (synopsis "SUNDIALS with LAPACK support as required by julia-sundials-jll")))

(define-public combinatorial-blas
  (package
    (name "combinatorial-blas")
    (version "1.6.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://eecs.berkeley.edu/~aydin/CombBLAS_FILES/"
                           "CombBLAS_beta_"
                           (match (string-split version #\.)
                            ((major minor patch)
                             (string-append major minor "_" patch))) ;e.g. "16_2"
                           ".tgz"))
       (sha256
        (base32
         "0gzxgd2ybnh49h57rh47vrqnsyk11jn206j5kf9y7p5vksc79ffz"))
       (patches (search-patches "combinatorial-blas-awpm.patch"
                                "combinatorial-blas-io-fix.patch"))))
    (build-system cmake-build-system)
    (inputs
     `(("mpi" ,openmpi)
       ("test-data" ,(origin
                       (method url-fetch)
                       (uri (string-append "https://people.eecs.berkeley.edu/~aydin/"
                                           "CombBLAS_FILES/testdata_combblas1.6.1.tgz"))
                       (sha256
                        (base32
                         "01y2781cy3fww7znmidrp85mf8zx0c905w5vzvk1mgrmhhynim87"))))))
    (arguments
     `(#:configure-flags '("-DBUILD_SHARED_LIBS:BOOL=YES"
                           "-DCMAKE_CXX_FLAGS=-DUSE_FUNNEL")
       #:parallel-tests? #f             ;tests use 'mpiexec -n4'
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-tests
           (lambda _
             ;; Skip failing tests (SIGFPE and SIGSEGV).
             (substitute* "ReleaseTests/CMakeLists.txt"
               (("^.*SpAsgnTest.*$") "")
               (("^.*IndexingTest.*$") ""))))
         (add-before 'check 'mpi-setup
           ,%openmpi-setup)
         (add-before 'check 'test-setup
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "OMP_NUM_THREADS" "2")
             (invoke "tar" "xf" (assoc-ref inputs "test-data")))))))
    (home-page "https://people.eecs.berkeley.edu/~aydin/CombBLAS/html/")
    (synopsis "Linear algebra primitives for graph analytics")
    (description "The Combinatorial BLAS (CombBLAS) is an extensible
distributed-memory parallel graph library offering a small but powerful set of
linear algebra primitives specifically targeting graph analytics.")
    (license (list
              license:gpl2+             ;include/psort/(funnel|sort)*.h
              license:x11               ;usort and psort
              license:bsd-3))))         ;CombBLAS and MersenneTwister.h

(define-public dune-common
  (package
    (name "dune-common")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-common-" version ".tar.gz"))
       (sha256
        (base32
         "1pdgxlxb570fm7smk1zv9b3iq1wzjg6g0s4361xs2w5qrf6drh4l"))
       (patches (search-patches "dune-common-skip-failing-tests.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list gmp metis openblas python superlu))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).")
    ;; GPL version 2 with "runtime exception" to make it behave like LGPLv2.
    (license license:gpl2)))

(define-public dune-geometry
  (package
    (name "dune-geometry")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-geometry-" version ".tar.gz"))
       (sha256
        (base32
         "00vkidb931zvpq3nmw8ikyg8pr3jqisfq2qxwj9hqzj7634qms98"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           ;; Optional
           openblas gmp python))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).

This package contains the basic DUNE geometry classes.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-uggrid
  (package
    (name "dune-uggrid")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-uggrid-" version ".tar.gz"))
       (sha256
        (base32
         "1wm1jy8ssfzpskhk7z34ahmw0q0iyna0dgph8kskv6j2i8v7skip"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).

This package contains the DUNE UG grid classes.")
    (license license:lgpl2.1)))

(define-public dune-grid
  (package
    (name "dune-grid")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-grid-" version ".tar.gz"))
       (sha256
        (base32
         "0mh06g3sryx3s0d7zgzsz6j18vbzb0f46wq7aw6ahj2hswb7rsrg"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           dune-geometry
           gmp
           metis
           openblas
           python))
    (propagated-inputs
     (list dune-uggrid))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.  It supports the easy implementation of methods like
@dfn{Finite Elements} (FE), @dfn{Finite Volumes} (FV), and also @dfn{Finite
Differences} (FD).

This package contains the basic DUNE grid classes.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-istl
  (package
    (name "dune-istl")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-istl-" version ".tar.gz"))
       (sha256
        (base32
         "0rk95rkj87gpb3gn40jl532rybs2lxkhn7g6b30m9kbzz7yfjfbc"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; XXX: istl/test/matrixtest.cc includes <fenv.h> and fails to find
         ;; the stdlib types when the gfortran header is used.  Remove gfortran
         ;; from CPLUS_INCLUDE_PATH as a workaround.
         (add-after 'set-paths 'hide-gfortran
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gfortran (assoc-ref inputs "gfortran")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gfortran "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t)))
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           ;; Optional
           metis
           suitesparse
           superlu
           openblas
           gmp
           python))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "DUNE, the Distributed and Unified Numerics Environment is a
modular toolbox for solving @dfn{partial differential equations} (PDEs) with
grid-based methods.

This is the iterative solver template library which provides generic sparse
matrix/vector classes and a variety of solvers based on these classes.  A
special feature is the use of templates to exploit the recursive block
structure of finite element matrices at compile time.  Available solvers
include Krylov methods, (block-) incomplete decompositions and
aggregation-based algebraic multigrid.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-localfunctions
  (package
    (name "dune-localfunctions")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-localfunctions-" version ".tar.gz"))
       (sha256
        (base32
         "0a5hyd7fps18178dq41nxa21h0i9ah6sw7di8qkc4i1rh052rzc1"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; XXX: localfunctions/test/lagrangeshapefunctiontest.cc includes <fenv.h>
         ;; and fails to find the stdlib types when the gfortran header is used.
         ;; Hide gfortran from CPLUS_INCLUDE_PATH to ensure we get the GCC header.
         (add-after 'set-paths 'hide-gfortran
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gfortran (assoc-ref inputs "gfortran")))
               (setenv "CPLUS_INCLUDE_PATH"
                       (string-join
                        (delete (string-append gfortran "/include/c++")
                                (string-split (getenv "CPLUS_INCLUDE_PATH") #\:))
                        ":"))
               #t)))
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           dune-geometry
           ;; Optional
           metis
           superlu
           gmp))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment") ; TODO
    (description "This DUNE module provides interface and implementation for
shape functions defined on the DUNE reference elements.  In addition to the
shape function, interpolation operators and special keys are provided which
can be used to assemble global function spaces on finite-element grids.

This package provides an interface and implementation for shape functions
defined on the DUNE reference elements.  In addition to the shape function,
interpolation operators and special keys are provided which can be used to
assemble global function spaces on finite-element grids.")
    ;; GPL version 2 with "runtime exception"
    (license license:gpl2)))

(define-public dune-alugrid
  (package
    (name "dune-alugrid")
    (version "2.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.dune-project.org/extensions/dune-alugrid.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0289vqf9azhgqda04qa5prn201xnsd9i0r8gy6jn0g6wfy9bcpav"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-include
           (lambda _
             (substitute* "dune/alugrid/test/test-alugrid.cc"
               (("doc/grids/gridfactory/testgrids")
                "doc/dune-grid/grids/gridfactory/testgrids"))
             #t))
         (add-after 'build 'build-tests
           (lambda* (#:key inputs make-flags parallel-build? #:allow-other-keys)
             (setenv "CPLUS_INCLUDE_PATH"
                     (string-append (assoc-ref inputs "dune-grid") "/share"))
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           dune-geometry
           dune-grid
           ;; Optional
           metis
           openblas
           python
           superlu
           gmp
           zlib))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "ALUGrid is an adaptive, loadbalancing, unstructured
implementation of the DUNE grid interface supporting either simplices or
cubes.")
    (license license:gpl2+)))

(define-public dune-subgrid
  ; dune-subgrid does not tag its releases.
  ; The following commit is the last commit on the releases/2.10 branch
  ; as of 2024-11-14.
  (let ((commit "e71cc9c2514356c7cd989f0c13661f10c37c58df"))
    (package
      (name "dune-subgrid")
      (version (git-version "2.10.0" "1" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://gitlab.dune-project.org/extensions/dune-subgrid")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
            "1vahmj2r9r684n8rgnqqb8zhi3wibkxjsv1kql804azx71dslx0d"))))
      (build-system cmake-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'build 'build-tests
             (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
               (apply invoke "make" "build_tests"
                      `(,@(if parallel-build?
                              `("-j" ,(number->string (parallel-job-count)))
                              '())
                        ,@make-flags)))))))
      (inputs
       (list dune-common
             dune-geometry
             dune-grid
             ;; Optional
             metis
             openblas
             gmp))
      (native-inputs
       (list gfortran pkg-config))
      (home-page "http://numerik.mi.fu-berlin.de/dune-subgrid/index.php")
      (synopsis "Distributed and Unified Numerics Environment")
      (description "The dune-subgrid module marks elements of
another hierarchical dune grid.  The set of marked elements can then be
accessed as a hierarchical dune grid in its own right.  Dune-Subgrid
provides the full grid interface including adaptive mesh refinement.")
      (license license:gpl2+))))

(define-public dune-typetree
  (package
    (name "dune-typetree")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-typetree-" version ".tar.gz"))
       (sha256
        (base32
         "0k756c543r79jz51jfnvi6knnxv7y19xg69yb15b0hrv4gq015pf"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           ;; Optional
           openblas
           python
           metis
           superlu
           gmp))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "TypeTree is a template library for constructing and
operating on statically typed trees of objects.")
    ;; Either GPL version 2 with "runtime exception" or LGPLv3+.
    (license (list license:lgpl3+ license:gpl2))))

(define-public dune-functions
  (package
    (name "dune-functions")
    (version "2.10.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://dune-project.org/download/"
                           version "/dune-functions-" version ".tar.gz"))
       (sha256
        (base32
         "1v2yyiqacspa7fkz5pbhd9hcz8rk5bhyhlhwvr3jjgmniiy0x2hp"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'disable-failing-tests
           (lambda _
             (setenv "ARGS"
                     ;; unable to load GMSH file in this test
                     "--exclude-regex gridviewfunctionspacebasistest")
            #t))
         (add-after 'build 'build-tests
           (lambda* (#:key make-flags parallel-build? #:allow-other-keys)
             (apply invoke "make" "build_tests"
                    `(,@(if parallel-build?
                            `("-j" ,(number->string (parallel-job-count)))
                            '())
                      ,@make-flags)))))))
    (inputs
     (list dune-common
           dune-istl
           dune-localfunctions
           dune-grid
           dune-geometry
           dune-typetree
           openblas
           metis
           python
           superlu
           gmp))
    (native-inputs
     (list gfortran pkg-config))
    (home-page "https://dune-project.org/")
    (synopsis "Distributed and Unified Numerics Environment")
    (description "The dune-functions module provides an abstraction layer for
global finite element functions.  Its two main concepts are functions
implemented as callable objects, and bases of finite element spaces.")
    ;; Either GPL version 2 with "runtime exception" or LGPLv3+.
    (license (list license:lgpl3+ license:gpl2))))

(define-public dune-pdelab
  ;; This was the last commit on the releases/2.8 branch as of 2023-04-12,
  ;; unfortunately there was no tag for any 2.8 release.
  (let ((commit "d5dddb6b1c21b95e867ff53cca159ad676689f5c"))
    (package
      (name "dune-pdelab")
      (version (git-version "2.8.0" "0" commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.dune-project.org/pdelab/dune-pdelab")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0l2idjp59a6x46vdd30xhhsnv7chql0z7msdsyra2h6pqm6xiqxk"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f)) ; XXX: the tests cannot be compiled
      (inputs
       (list dune-common
             dune-istl
             dune-localfunctions
             dune-geometry
             dune-grid
             dune-typetree
             dune-functions
             ;; Optional
             openblas
             eigen
             metis
             python
             superlu
             gmp))
      (native-inputs
       (list gfortran pkg-config))
      (home-page "https://dune-project.org/")
      (synopsis "Differential equations solver toolbox")
      (description "PDELab is a partial differential equations solver toolbox
built on top of DUNE, the Distributed and Unified Numerics Environment.")
      ;; Either GPL version 2 with "runtime exception" or LGPLv3+.
      (license (list license:lgpl3+ license:gpl2)))))

(define add-openmpi-to-dune-package
  (let ((dune-package?
          (lambda (p) (string-prefix? "dune-" (package-name p)))))
    (package-mapping
      (lambda (p)
        (if (dune-package? p)
            (package (inherit p)
              (name (string-append (package-name p) "-openmpi"))
              (inputs (modify-inputs (package-inputs p)
                        (append openmpi)))
              (arguments
               (substitute-keyword-arguments (package-arguments p)
                 ((#:phases phases '%standard-phases)
                  `(modify-phases ,phases
                     (add-before 'check 'mpi-setup
                       ,%openmpi-setup)))))
              (synopsis (string-append (package-synopsis p) " (with MPI support)")))
            p))
      (negate dune-package?))))

(define-public dune-common-openmpi
  (add-openmpi-to-dune-package dune-common))

(define-public dune-geometry-openmpi
  (add-openmpi-to-dune-package dune-geometry))

(define-public dune-istl-openmpi
  (add-openmpi-to-dune-package dune-istl))

(define-public dune-typetree-openmpi
  (add-openmpi-to-dune-package dune-typetree))

(define-public dune-uggrid-openmpi
  (add-openmpi-to-dune-package dune-uggrid))

(define-public dune-grid-openmpi
  (add-openmpi-to-dune-package dune-grid))

(define-public dune-alugrid-openmpi
  (add-openmpi-to-dune-package dune-alugrid))

(define-public dune-subgrid-openmpi
  (add-openmpi-to-dune-package dune-subgrid))

(define-public dune-localfunctions-openmpi
  (add-openmpi-to-dune-package dune-localfunctions))

(define-public dune-functions-openmpi
  (add-openmpi-to-dune-package dune-functions))

(define-public dune-pdelab-openmpi
  (add-openmpi-to-dune-package dune-pdelab))

(define-public mlucas
  (package
    (name "mlucas")
    (version "18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://mersenneforum.org/mayer/src/C/mlucas_v" version ".txz"))
       (sha256
        (base32 "0h4xj6pyyac79ka5ibqjilfa3s9j3yxnzgpwc57b54kfh2bj3447"))))
    (build-system gnu-build-system)
    (inputs
     (list python-2))
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (chdir "src")
             (call-with-output-file "Makefile"
               (lambda (port)
                 (format port "CC = gcc
CFLAGS = -O3 ~a -DUSE_THREADS
LDLIBS = -lm -lpthread -lrt
Mlucas: $(addsuffix .o,$(basename $(wildcard *.c)))
"
                         ,(let ((system (or (%current-target-system)
                                            (%current-system))))
                            (cond
                             ((string-prefix? "x86_64" system) "-DUSE_SSE2")
                             (else ""))))))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (install-file "Mlucas" bin)
               (install-file "primenet.py" bin))
             #t)))))
    (home-page "https://www.mersenne.org")
    (synopsis "Great Internet Mersenne Prime Search (GIMPS) distributed computing client")
    (description "Mlucas performs Lucas-Lehmer primality testing of Mersenne
numbers in search of a world-record prime.  You may use it to test any
suitable number as you wish, but it is preferable that you do so in a
coordinated fashion, as part of the Great Internet Mersenne Prime
Search (GIMPS).  Mlucas also includes a simple Python script for assignment
management via the GIMPS project's Primenet server.")
    (license license:gpl2+)))

(define-public nauty
  (package
    (name "nauty")
    (version "2.8.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://pallini.di.uniroma1.it/"
             "nauty" (string-join (string-split version #\.) "_") ".tar.gz"))
       (sha256
        (base32 "1vn4abz498h8fbh27z0l5jrs4z04d693xklbb5mai5l7yhmv8yn9"))))
    (build-system gnu-build-system)
    (outputs '("out" "lib"))
    (arguments
     (list
      #:test-target "checks"
      #:configure-flags #~(list "--enable-generic") ;prevent -march-native
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'normalize-pkgconfig-files-location
            (lambda _
              (substitute* "makefile.in"
                (("^(pkgconfigexecdir=).*" _ prefix)
                 (string-append prefix "${libdir}/pkgconfig\n")))))
          (add-after 'unpack 'fix-failing-test
            (lambda _
              (substitute* "runalltests.in"
                ((" uniqg") " ./uniqg")))))))
    (inputs (list gmp))                 ;for sumlines
    (home-page "https://pallini.di.uniroma1.it/")
    (synopsis "Library for graph automorphisms")
    (description "@code{nauty} (No AUTomorphisms, Yes?) is a set of
procedures for computing automorphism groups of graphs and digraphs.

@code{nauty} computes graph information in the form of a set of
generators, the size of the group, and the orbits of the group; it can
also produce a canonical label.  The @code{nauty} suite is written in
C and comes with a command-line interface, a collection of
command-line tools, and an Application Programming Interface (API).

This package provides the static libraries required to run programs
compiled against the nauty library.")
    (license license:asl2.0)))

(define-public speedcrunch
  (package
    (name "speedcrunch")
    (version "0.12.0")
    (source
     (origin
      (method git-fetch)
      (uri (git-reference
        (url "https://bitbucket.org/heldercorreia/speedcrunch.git")
        (commit (string-append "release-" version))))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "0vh7cd1915bjqzkdp3sk25ngy8cq624mkh8c53c5bnzk357kb0fk"))))
    (build-system cmake-build-system)
    (inputs (list qtbase-5))
    (native-inputs (list qttools-5))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-to-src
           (lambda _ (chdir "src") #t)))))
    (synopsis "High-precision scientific calculator")
    (description
     "SpeedCrunch is a high-precision scientific calculator.  It features a
syntax-highlighted scrollable display and is designed to be fully used via
keyboard.  Some distinctive features are auto-completion of functions and
variables, a formula book, and quick insertion of constants from various
fields of knowledge.")
    (home-page "https://speedcrunch.org/")
    (license license:gpl2+)))

(define-public minisat
  ;; This is the last commit which is available upstream, no
  ;; release happened since 2010.
  (let ((commit "37dc6c67e2af26379d88ce349eb9c4c6160e8543")
        (revision "1"))
    (package
      (name "minisat")
      (version (string-append "2.2.0-" revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/niklasso/minisat")
               (commit commit)))
         (file-name (string-append name "-" version "-checkout"))
         (sha256
          (base32
           "091hf3qkm197s5r7xcr3m07xsdwyz2rqk1hc9kj0hn13imz09irq"))
         (patches
          (search-patches "minisat-friend-declaration.patch"
                          "minisat-install.patch"))))
      (build-system gnu-build-system)
      (arguments
       '(#:make-flags (list (string-append "prefix=" %output))
         #:tests? #f ;no check target
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))))
      (inputs
       `(("zlib:static" ,zlib "static")
         ("zlib" ,zlib)))
      (synopsis
       "Small, yet efficient, SAT solver")
      (description
       "MiniSat is a minimalistic, open-source SAT solver, developed to help
researchers and developers alike to get started on SAT.")
      (home-page
       "http://minisat.se/MiniSat.html")
      (license license:expat))))

(define-public kissat
  (package
    (name "kissat")
    (version "4.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/arminbiere/kissat")
             (commit (string-append "rel-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0acg61cfcjg13if2i375cyl4xvwmabhfhi9z8pnw971046am6bzv"))))
    (build-system gnu-build-system)
    (inputs (list xz gzip lzip bzip2 p7zip))
    (arguments
     (list
      #:test-target "test"
      #:configure-flags #~(list "-shared")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/file.c"
                (("(bzip2|gzip|lzma|xz) -c" all cmd)
                 (string-append (search-input-file inputs
                                                   (string-append "bin/" cmd))
                                " -c"))
                (("7z ([ax])" all mode)
                 (string-append (search-input-file inputs "bin/7z")
                                " " mode))
                ;; Since we hard-coded the paths, we no longer need to find
                ;; them.
                (("bool found = kissat_find_executable \\(name\\);")
                 "bool found = true;"))
              (substitute* "test/testmain.c"
                ;; SIGINT is ignored inside invoke.
                (("^[ \t]*SIGNAL[ \t]*\\(SIGINT\\)") ""))))
          (replace 'configure
            (lambda* (#:key configure-flags #:allow-other-keys)
              ;; The configure script does not support standard GNU options.
              (apply invoke "./configure" configure-flags)))
          (replace 'install
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((out (assoc-ref outputs "out")))
                (install-file "build/kissat" (string-append out "/bin"))
                (install-file "build/libkissat.so" (string-append out "/lib"))
                (install-file "src/kissat.h"
                              (string-append out "/include"))))))))
    (home-page "https://github.com/arminbiere/kissat")
    (synopsis "Bare-metal SAT solver")
    (description
     "Kissat is a bare-metal SAT-solver written in C.  It is a port of CaDiCaL
back to C with improved data structures, better scheduling of inprocessing and
optimized algorithms and implementation.")
    (license license:expat)))

(define-public aiger
  (package
    (name "aiger")
    (version "1.9.9")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://fmv.jku.at/aiger/aiger-"
                                 version ".tar.gz"))
             (sha256
               (base32
                "1ish0dw0nf9gyghxsdhpy1jjiy5wp54c993swp85xp7m6vdx6l0y"))))
    (outputs (list "out" "static"))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                  ; no check target
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-source
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "aiger.c"
                     (("\"(gzip|gunzip)" all cmd)
                      (string-append
                       "\""
                       (search-input-file inputs (string-append "bin/" cmd)))))))
               (add-after 'unpack 'patch-build-files
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* "makefile.in"
                     (("test -d .*") "true")
                     (("/usr/local") (assoc-ref outputs "out")))))
               (replace 'configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (apply invoke "./configure.sh" configure-flags)))
               (add-after 'install 'install-static
                 (lambda* (#:key outputs #:allow-other-keys)
                   (apply invoke #$(ar-for-target) "rcs" "libaiger.a"
                          (find-files "." "\\.o$"))
                   (let* ((static (assoc-ref outputs "static"))
                          (lib (string-append static "/lib"))
                          (incl (string-append static "/include/aiger")))
                     (mkdir-p lib)
                     (mkdir-p incl)
                     (install-file "libaiger.a" lib)
                     (for-each (lambda (f) (install-file f incl))
                               (find-files "." "\\.h$"))))))))
    (inputs (list gzip))
    (home-page "https://fmv.jku.at/aiger")
    (synopsis "Utilities for And-Inverter Graphs")
    (description "AIGER is a format, library and set of utilities for
@acronym{AIG, And-Inverter Graphs}s.  The focus is on conversion utilities and a
generic reader and writer API.")
    (license (list license:expat
                   license:bsd-3))))    ; blif2aig

(define-public btor2tools
  (let ((commit "b8456dda4780789e882f5791eb486f295ade4da4")
        (revision "1"))
   (package
   (name "btor2tools")
   (version (git-version "1.0.0-pre" revision commit))
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/Boolector/btor2tools")
                  (commit commit)))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0r3cm69q5xhnbxa74yvdfrsf349s4cxmiqlb4aq8appi7yg3qhww"))))
   (build-system cmake-build-system)
   (arguments
    (list #:out-of-source? #f
          #:phases
          #~(modify-phases %standard-phases
              (replace 'check
                (lambda* (#:key tests? #:allow-other-keys)
                  (when tests?
                    (invoke "sh" "test/runtests.sh")))))))
   (home-page "https://boolector.github.io")
   (synopsis "Parser for BTOR2 format")
   (description "This package provides a parser for the BTOR2 format used by
Boolector.")
   (license license:lgpl3+))))

(define-public cudd
  (package
   (name "cudd")
   (version "3.0.0")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/ivmai/cudd")
                  (commit (string-append "cudd-" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0hyw9q42ir92vcaa7bwv6f631n85rfsxp463rnmklniq1wf6dyn9"))))
   (build-system gnu-build-system)
   (arguments (list #:configure-flags #~(list "--enable-shared")))
   ;; The original home-page was lost to time, so we reference the "unofficial"
   ;; Github mirror.  For what it's worth, the author of the library appears to
   ;; have been involved with this mirror at some point in time.
   (home-page "https://github.com/ivmai/cudd")
   (synopsis "Manipulate decision diagrams")
   (description "@acronym{CUDD, Colorado University Decision Diagrams} is a
library for manipulating decision diagrams.  It supports binary decision
diagrams, algebraic decision diagrams, and zero-suppressed binary decision
diagrams.")
   (license license:bsd-3)))

(define-public libpoly
  (package
   (name "libpoly")
   (version "0.1.12")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/SRI-CSL/libpoly")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "1cgdj7mxjyq4r2n852nxqacml90jm9irbvv27an0fmg7q4v1p2kb"))))
   (build-system cmake-build-system)
   (arguments
    (list #:configure-flags #~(list "-DLIBPOLY_BUILD_PYTHON_API=off")))
   (inputs (list gmp))
   (home-page "https://github.com/SRI-CSL/libpoly")
   (synopsis "Manipulate polynomials")
   (description "LibPoly is a C library for manipulating polynomials to support
symbolic reasoning engines that need to reason about polynomial constraints.")
   (license license:lgpl3+)))

(define-public lingeling
  (package
    (name "lingeling")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arminbiere/lingeling")
                    (commit (string-append "rel-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hszkhyni7jcw580f41rrrnwz42x56sqvd8zpcjdagvdiag76lc1"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"
           #:modules `((ice-9 match)
                       ,@%default-gnu-modules)
           #:configure-flags #~(list "--aiger=.")
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'unpack-aiger
                 (lambda* (#:key inputs #:allow-other-keys)
                   (invoke #$(ar-for-target) "x"
                           (search-input-file inputs "lib/libaiger.a")
                           "aiger.o")
                   (copy-file
                    (search-input-file inputs "include/aiger/aiger.h")
                    "aiger.h")))
               (add-after 'unpack 'hard-code-commit
                 (lambda _
                   (substitute* "mkconfig.sh"
                     (("`\\./getgitid`") ""))))
               (add-after 'unpack 'patch-source
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (list "treengeling.c" "lgldimacs.c")
                     (("\"(gunzip|xz|bzcat|7z)" all cmd)
                      (string-append
                       "\""
                       (search-input-file inputs (string-append "bin/" cmd)))))))
               (replace 'configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (apply invoke "./configure.sh" configure-flags)))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let ((bin (string-append (assoc-ref outputs "out")
                                             "/bin")))
                     (mkdir-p bin)
                     (for-each
                      (lambda (file)
                        (install-file file bin))
                      '("blimc" "ilingeling" "lglddtrace" "lglmbt"
                        "lgluntrace" "lingeling" "plingeling"
                        "treengeling")))))
               (add-after 'install 'wrap-path
                 (lambda* (#:key outputs #:allow-other-keys)
                   (with-directory-excursion (string-append
                                              (assoc-ref outputs "out")
                                              "/bin")
                     (for-each
                      (lambda (file)
                        (wrap-program
                            file
                          '("PATH" suffix
                            #$(map (lambda (input)
                                     (file-append (this-package-input input) "/bin"))
                                   '("gzip" "bzip2" "xz" "p7zip")))))
                      ;; These programs use sprintf on buffers with magic
                      ;; values to construct commands (yes, eww), so we
                      ;; can't easily substitute* them.
                      '("lglddtrace" "lgluntrace" "lingeling" "plingeling"))))))))
    (inputs (list `(,aiger "static") bash-minimal gzip bzip2 xz p7zip))
    (home-page "http://fmv.jku.at/lingeling")
    (synopsis "SAT solver")
    (description "This package provides a range of SAT solvers, including
the sequential @command{lingeling} and its parallel variants
@command{plingeling} and @command{treengeling}.  A bounded model checker is
also included.")
    (license license:expat)))

(define-public cadical
  (package
    (name "cadical")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/arminbiere/cadical")
                    (commit (string-append "rel-" version))))
              (file-name (git-file-name name version))
              (patches (search-patches "cadical-add-shared-library.patch"))
              (sha256
               (base32 "1dzjah3z34v89ka48hncwqkxrwl4xqn9947p0ipf39lxshrq91xa"))))
    (build-system gnu-build-system)
    (arguments
     (list #:test-target "test"
           #:modules `(((guix build copy-build-system) #:prefix copy:)
                       (guix build gnu-build-system)
                       (guix build utils)
                       (ice-9 regex))
           #:imported-modules %copy-build-system-modules
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda* (#:key configure-flags #:allow-other-keys)
                   (setenv "CXXFLAGS" "-DPIC -fPIC")
                   (apply invoke "./configure" configure-flags)))
               (replace 'check
                 (lambda args
                   ;; Tests are incorrectly linked upstream.
                   ;; Since we don't install them, just work around this in the
                   ;; check phase.
                   (setenv "LD_LIBRARY_PATH" (string-append (getcwd) "/build"))
                   (apply (assoc-ref %standard-phases 'check) args)
                   (unsetenv "LD_LIBRARY_PATH")))
               (replace 'install
                 (lambda args
                   (apply
                    (assoc-ref copy:%standard-phases 'install)
                    #:install-plan
                    `(("build" "bin" #:include ("cadical" "mobical"))
                      ("build" "lib" #:include-regexp ("libcadical\\.(a|so)$"))
                      ("src" "include" #:include ("cadical.h"))
                      ;; Internal headers used by cadiback.
                      ("src" "include/cadical" #:include-regexp ("\\.hpp$")))
                    args))))))
    (home-page "https://github.com/arminbiere/cadical")
    (synopsis "SAT solver")
    (description "This package provides a SAT solver based on conflict-driven
clause learning.")
    (license license:expat)))

(define-public cadiback
  (let ((commit "789329d8fcda851085ed72f1b07d8c3f46243b8a")
        (revision "1"))
    (package
      (name "cadiback")
      ;; Note: version taken from VERSION file
      (version (git-version "0.2.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arminbiere/cadiback")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "137jxf9g7c1979pcgcqgfff1mqk5hs41a84780px8gpcrh469cks"))))
      (build-system gnu-build-system)
      (arguments
       (list #:test-target "test"
             #:modules `(((guix build copy-build-system) #:prefix copy:)
                         (guix build gnu-build-system)
                         (guix build utils)
                         (ice-9 regex))
             #:imported-modules %copy-build-system-modules
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-build-files
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "configure"
                       (("\\$CADICAL/src") "$CADICAL/include/cadical")
                       (("\\$CADICAL/build") "$CADICAL/lib"))
                     (substitute* "generate"
                       (("\\[ -d .git \\]" all) (string-append ": " all))
                       (("GITID=.*") (string-append "GITID=\"" #$commit "\"")))
                     (substitute* "makefile.in"
                       (("\\.\\./cadical/build")
                        (dirname
                         (search-input-file inputs "lib/libcadical.a")))
                       (("\\.\\./cadical/src")
                        (search-input-directory inputs "include/cadical")))))
                 (replace 'configure
                   (lambda* (#:key configure-flags #:allow-other-keys)
                     (setenv "CADICAL" #$(this-package-input "cadical"))
                     (apply invoke "./configure" configure-flags)))
                 (replace 'install
                   (lambda args
                     (apply
                      (assoc-ref copy:%standard-phases 'install)
                      #:install-plan
                      `(("." "bin" #:include ("cadiback")))
                      args))))))
      (inputs (list cadical))
      (home-page "https://github.com/arminbiere/cadiback")
      (synopsis "Backbone extractor for cadical")
      (description "This package provides a tool to determine the backbone of
a satisfiable formula.  The backbone is the set of literals that are set to
true in all models.")
      (license license:expat))))

(define cadiback-for-cryptominisat
  (let ((commit "ea65a9442fc2604ee5f4ffd0f0fdd0bf481d5b42")
        (revision "1"))
    (package
      (inherit cadiback)
      (name "cadiback-for-cryptominisat")
      (version (git-version "0.2.1" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/meelgroup/cadiback")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1zznrlj4zp1mc7s4pfw11aq773q2lr9yl6pph630zg5mqijaim5g"))))
      (arguments
       (substitute-keyword-arguments (package-arguments cadiback)
         ((#:phases phases)
          #~(modify-phases #$phases
              (add-after 'patch-build-files 'fix-prefix
                (lambda _
                  (substitute* "makefile.in"
                    (("/usr") #$output))))
              (replace 'install
                (lambda args
                  (mkdir-p (string-append #$output "/include"))
                  (mkdir-p (string-append #$output "/lib"))
                  (apply (assoc-ref %standard-phases 'install) args))))))))))

(define-public louvain-community
  (let ((commit "8cc5382d4844af127b1c1257373740d7e6b76f1e")
        (revision "1"))
    (package
      (name "louvain-community")
      (version (git-version "1.0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/meelgroup/louvain-community")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1ss00hkdvr9bdkd355hxf8zd7xycb3nm8qpy7s75gjjf6yng0bfj"))))
      (build-system cmake-build-system)
      (arguments
       (list #:tests? #f                ; tests appear to require missing files
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'encode-git-hash
                   (lambda _
                     (substitute* "CMakeLists.txt"
                       (("GIT-hash-notfound") #$commit))))
                 (add-after 'unpack 'no-tune-native
                   (lambda _
                     (substitute* "CMakeLists.txt"
                       (("-mtune=native") "")))))))
      (native-inputs (list python))
      (home-page "https://github.com/meelgroup/louvain-community")
      (synopsis "Multi-criteria community detection")
      (description "This package provides a C++ implementation of the Louvain
community detection algorithm.")
      (properties '((tunable? . #t)))
      (license license:lgpl3+))))

(define-public cryptominisat
  (package
    (name "cryptominisat")
    (version "5.11.22")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/msoos/cryptominisat")
             (commit version)))
      (file-name (git-file-name name version))
      (sha256
       (base32
        "1c85gfqvy90yhh9jwmiiz2bz4i86prgpfyx1gbzl42hn2ixkcjgm"))
      (modules '((guix build utils)))
      (snippet
       #~(begin
           (substitute* "src/backbone.cpp"
             (("\"\\.\\./cadiback/cadiback\\.h\"") "<cadiback.h>"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:build-type "Release"
      #:test-target "test"
      #:configure-flags #~(list "-DENABLE_TESTING=ON" "-DSTATS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-source
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "CMakeLists.txt"
                (("add_subdirectory\\(utils/lingeling-ala\\)") ""))
              ;; Transitively included in vendored gtest.h. Fixed in
              ;; upstream:
              ;; https://github.com/msoos/cryptominisat/pull/686
              (substitute* "tests/assump_test.cpp"
                (("#include <vector>")
                 "#include <vector>\n#include <algorithm>"))
              (substitute* "tests/CMakeLists.txt"
                (("add_subdirectory\\(\\$\\{GTEST_PREFIX\\} gtest\\)")
                 "find_package(GTest REQUIRED)")
                (("add_subdirectory\\(\\$\\{PROJECT_SOURCE_DIR\\}/utils/.*\\)")
                 "")))))))
    (inputs (list boost
                  cadical
                  cadiback-for-cryptominisat
                  gmp
                  louvain-community
                  python
                  python-numpy
                  sqlite
                  zlib))
    (native-inputs (list googletest lingeling python python-wrapper python-lit))
    (synopsis "Incremental SAT solver")
    (description
     "CryptoMiniSat is an incremental SAT solver with both command line and
library (C++, C, Python) interfaces.  The command-line interface takes a
@acronym{CNF, Conjunctive Normal Form} as an input in the DIMACS format with
the extension of XOR clauses.  The library interfaces mimic this and also
allow incremental solving, including assumptions.")
    (home-page "https://github.com/msoos/cryptominisat")
    (license license:expat)))

(define-public libqalculate
  (package
    (name "libqalculate")
    (version "3.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/libqalculate/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w44407wb552q21dz4m2nwwdi8b9hzjb2w1l3ffsikzqckc7wbyj"))
       (patches
        (search-patches "libqalculate-3.8.0-libcurl-ssl-fix.patch"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("gettext" ,gettext-minimal)
       ("intltool" ,intltool)
       ("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("doxygen" ,doxygen)
       ("file" ,file)))
    (inputs
     (list gmp
           mpfr
           libxml2
           curl
           icu4c
           gnuplot
           readline))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'setenv
           ;; Prevent the autogen.sh script to carry out the configure
           ;; script, which has not yet been patched to replace /bin/sh.
           (lambda _
             (setenv "NOCONFIGURE" "TRUE")
             #t)))))
    (home-page "https://qalculate.github.io/")
    (synopsis "Multi-purpose cli desktop calculator and library")
    (description
     "Libqalculate is a multi-purpose cli desktop calculator and library.
It provides basic and advanced functionality.  Features include customizable
functions, unit calculations, and conversions, physical constants, symbolic
calculations (including integrals and equations), arbitrary precision,
uncertainty propagation, interval arithmetic, plotting and a user-friendly
cli.")
    (license license:gpl2+)))

(define-public qalculate-gtk
  (package
    (name "qalculate-gtk")
    (version "3.19.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Qalculate/qalculate-gtk/")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1nrx7gp6f1yalbdda1gb97azhbr4xclq2xf08vvbvsk8jfd6fd2v"))))
    (build-system glib-or-gtk-build-system)
    (native-inputs
     (list pkg-config
           intltool
           automake
           autoconf
           libtool
           file))
    (inputs
     (list gmp
           mpfr
           libqalculate
           libxml2
           glib
           gtk+))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'bootstrap 'setenv
           ;; Prevent the autogen.sh script to carry out the configure
           ;; script, which has not yet been patched to replace /bin/sh.
           (lambda _
             (setenv "NOCONFIGURE" "TRUE")
             #t)))))
    (home-page "https://qalculate.github.io/")
    (synopsis "Multi-purpose graphical desktop calculator")
    (description
     "Qalculate-gtk is the GTK frontend for libqalculate.  It is a
multi-purpose GUI desktop calculator.  It provides basic and advanced
functionality.  Features include customizable functions, unit calculations,
and conversions, physical constants, symbolic calculations (including
integrals and equations), arbitrary precision, uncertainty propagation,
interval arithmetic, plotting.")
    (license license:gpl2+)))

(define-public numdiff
  (package
    (name "numdiff")
    (version "5.9.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://savannah/numdiff/numdiff-"
                           version ".tar.gz"))
       (sha256
        (base32
         "1vzmjh8mhwwysn4x4m2vif7q2k8i19x8azq7pzmkwwj4g48lla47"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; There are no tests.
       #:phases
       (modify-phases %standard-phases
         (add-before 'compress-documentation 'delete-precompressed-info-file
           (lambda _
             (delete-file (string-append (assoc-ref %outputs "out")
                                         "/share/info/numdiff.info.gz"))
             #t)))))
    (home-page "https://nongnu.org/numdiff/")
    (synopsis "Compare files with numeric fields")
    (description
     "Numdiff compares files line by line and field by field, ignoring small
numeric differences and differences in numeric formats.")
    (license license:gpl3+)))

(define-public why3
  (package
    (name "why3")
    (version "1.7.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.inria.fr/why3/why3")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fq8wg8ji2v2ssz1d681glmk8glps1irnmdlhqfklaggx01hlf4p"))))
    (build-system ocaml-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-before 'configure 'bootstrap
                 (lambda _
                   (invoke "./autogen.sh")
                   (setenv "CONFIG_SHELL" (which "sh"))
                   (substitute* "configure"
                     (("#! /bin/sh") (string-append "#!" (which "sh")))
                     ;; find ocaml-num in the correct directory
                     (("\\$DIR/nums.cma") "$DIR/num.cma")
                     (("\\$DIR/num.cmi") "$DIR/core/num.cmi"))))
               (add-after 'configure 'fix-makefile
                 (lambda _
                   (substitute* "Makefile"
                     ;; find ocaml-num in the correct directory
                     (("site-lib/num") "site-lib"))))
            (add-after 'install 'install-lib
              (lambda _
                (invoke "make" "byte")
                (invoke "make" "install-lib"))))))
    (native-inputs (list autoconf
                         automake
                         coq
                         ocaml
                         ocaml-findlib
                         which))
    (propagated-inputs (list camlzip
                             lablgtk3
                             ocaml-graph
                             ocaml-lablgtk3-sourceview3
                             ocaml-menhir
                             ocaml-ppx-deriving
                             ocaml-ppx-sexp-conv
                             ocaml-num
                             ocaml-re
                             ocaml-sexplib
                             ocaml-zarith))
    (inputs (list coq-flocq
                  emacs-minimal
                  zlib))
    (home-page "https://why3.lri.fr")
    (synopsis "Deductive program verification")
    (description "Why3 provides a language for specification and programming,
called WhyML, and relies on external theorem provers, both automated and
interactive, to discharge verification conditions.  Why3 comes with a standard
library of logical theories (integer and real arithmetic, Boolean operations,
sets and maps, etc.) and basic programming data structures (arrays, queues,
hash tables, etc.).  A user can write WhyML programs directly and get
correct-by-construction OCaml programs through an automated extraction
mechanism.  WhyML is also used as an intermediate language for the verification
of C, Java, or Ada programs.")
    (license license:lgpl2.1)))

(define-public frama-c
  (package
    (name "frama-c")
    (version "29.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://frama-c.com/download/frama-c-"
                                  version "-Copper.tar.gz"))
              (sha256
               (base32
                "14vlvynp3yfmnkixm676c1ip0jlkiqjzmrp9f9c990zzs2wb7yyj"))))
    (build-system dune-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
             (add-before 'build 'set-env
               (lambda _
                 (setenv "CC" "gcc")))
             (add-after 'install 'wrap-programs
               (lambda _
                 (let ((ocamlpath
                         `(,(string-append #$output "/lib/ocaml/site-lib")
                           ,@(search-path-as-string->list
                               (getenv "OCAMLPATH")))))
                   (for-each
                     (lambda (program)
                       (wrap-program (string-append #$output "/bin/" program)
                         `("OCAMLPATH" ":" prefix ,ocamlpath)))
                     '("frama-c" "frama-c-gui"))))))))
    (inputs
     (list bash-minimal gmp zlib))
    (propagated-inputs (list
                         graphviz
                         lablgtk3
                         ocaml-graph
                         ocaml-odoc
                         ocaml-lablgtk3-sourceview3
                         ocaml-yaml
                         ocaml-yojson
                         ocaml-zarith
                         ocaml-ppx-deriving
                         ocaml-ppx-deriving-yojson
                         ocaml-ppx-deriving-yaml
                         ocaml-ppx-import
                         ocaml-unionfind
                         why3))
    (native-inputs (list dune-site time ocaml-menhir ocaml-graph))
    (native-search-paths
     (list (search-path-specification
            (variable "FRAMAC_SHARE")
            (files '("share/frama-c"))
            (separator #f))
           (search-path-specification
            (variable "FRAMAC_LIB")
            (files '("lib/frama-c"))
            (separator #f))))
    (home-page "https://frama-c.com")
    (synopsis "C source code analysis platform")
    (description "Frama-C is an extensible and collaborative platform dedicated
to source-code analysis of C software.  The Frama-C analyzers assist you in
various source-code-related activities, from the navigation through unfamiliar
projects up to the certification of critical software.")
    (license license:lgpl2.1+)))

(define-public blitz
  (package
    (name "blitz")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/blitzpp/blitz")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0c88gc72j3zggyk4yrrip6i0v7xkx97l140vpy3xhxs2i7xy1461"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags '("-DBUILD_DOC=ON"
                           "-DBUILD_TESTING=ON")
       ;; The default "check" target also includes examples and benchmarks.
       #:test-target "check-testsuite"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda _
             (invoke "make" "-j" (number->string (parallel-job-count))
                     "blitz-doc"))))))
    (native-inputs
     (list python texinfo))
    (synopsis "C++ template class library for multidimensional arrays")
    (description "Blitz++ is a C++ template class library that provides
high-performance multidimensional array containers for scientific computing.")
    (home-page "https://github.com/blitzpp/blitz")
    (license (list license:artistic2.0
                   license:bsd-3
                   license:lgpl3+))))

(define-public fxdiv
  ;; There is currently no tag in this repo.
  (let ((commit "63058eff77e11aa15bf531df5dd34395ec3017c8")
        (version "0.0")
        (revision "1"))
    (package
      (name "fxdiv")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/FXdiv")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0zwzh8gmbx4m6b18s5nf13b0dk5yjkd1fs8f421bl7fz5f9gjd9f"))
                (patches (search-patches "fxdiv-system-libraries.patch"))))
      (build-system cmake-build-system)
      (inputs
       (list googletest googlebenchmark))
      (synopsis
       "C++ library for division via fixed-point multiplication by inverse")
      (description
       "On modern CPUs and GPUs, integer division is several times slower than
multiplication.  FXdiv implements an algorithm to replace an integer division
with a multiplication and two shifts.  This algorithm improves performance
when an application performs repeated divisions by the same divisor.")
      (license license:expat))))

(define-public fp16
  ;; There is currently no tag in this repo.
  (let ((commit "0a92994d729ff76a58f692d3028ca1b64b145d91")
        (version "0.0")
        (revision "1"))
    (package
      (name "fp16")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/FP16")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "05mm4vrxsac35hjf5djif9r6rdxj9ippg97ia3p6q6b8lrp7srwv"))
                (patches (search-patches "fp16-implicit-double.patch"
                                         "fp16-system-libraries.patch"))))
      (build-system cmake-build-system)
      (arguments
       `(#:imported-modules ((guix build python-build-system)
                             ,@%cmake-build-system-modules)
         #:modules (((guix build python-build-system)
                     #:select (site-packages))
                    (guix build cmake-build-system)
                    (guix build utils))
         #:phases (modify-phases %standard-phases
                    (add-after 'install 'move-python-files
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        ;; Python files get installed to $includedir (!).
                        ;; Move them to the usual Python site directory.
                        (let* ((out     (assoc-ref outputs "out"))
                               (include (string-append out "/include"))
                               (site    (site-packages inputs outputs))
                               (target  (string-append site "/fp16")))
                          (mkdir-p target)
                          (for-each (lambda (file)
                                      (rename-file file
                                                   (string-append target "/"
                                                                  (basename
                                                                   file))))
                                    (find-files include "\\.py$"))))))))
      (native-inputs
       (list python-wrapper))
      (inputs
       (list psimd googletest googlebenchmark))
      (synopsis "C++ library for half-precision floating point formats")
      (description
       "This header-only C++ library implements conversion to and from
half-precision floating point formats.")
      (license license:expat))))

(define-public optizelle
  (let ((commit "ed4160b5287518448caeb34789d92dc6a0b7e2cc"))
   (package
    (name "optizelle")
    (version (git-version "1.3.0" "0" commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OptimoJoe/Optizelle")
             (commit commit)))
       (file-name (git-file-name "optizelle" commit))
       (sha256
        (base32
         "0rjrs5sdmd33a9f4xm8an7p0953aa0bxsmr4hs3ss1aad9k181vq"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; Reduce the stopping tolerance in one test so that the
           ;; convergence check returns the correct stopping
           ;; condition.
           (substitute*
            "src/unit/linear_algebra/tcg_loss_of_orthogonality.cpp"
            (("1e-13") "5e-14"))
           ;; Skip one set of python tests.  See
           ;; https://github.com/OptimoJoe/Optizelle/issues/2.
           (substitute*
            "src/examples/inequality_scaling/CMakeLists.txt"
            (("add_unit(.*)\\$\\{interfaces\\}(.*)$" all middle end)
             (string-append "add_unit" middle "\"cpp\"" end)))
           ;; Install the licence for Optizelle, without also
           ;; including the licences for the dependencies.
           (substitute* "licenses/CMakeLists.txt"
                        (("file.*package.*$" all)
                         (string-append "# " all))
                        ((".*[^l].[.]txt\\)\n") "")
                        (("add_license.*\"\n") ""))
           #t))))
    (build-system cmake-build-system)
    (arguments
     `(#:imported-modules ((guix build python-build-system)
                           ,@%cmake-build-system-modules)
       #:modules (((guix build python-build-system) #:select
                   (python-version))
                  (guix build cmake-build-system)
                  (guix build utils))
       #:configure-flags `("-DCMAKE_CXX_FLAGS:STRING=-pthread"
                           "-DENABLE_CPP_UNIT:BOOL=ON"
                           "-DENABLE_CPP_EXAMPLES:BOOL=ON"
                           "-DENABLE_PYTHON:BOOL=ON"
                           "-DENABLE_PYTHON_UNIT:BOOL=ON"
                           "-DENABLE_PYTHON_EXAMPLES:BOOL=ON"
                           ,(string-append "-DBLAS_LIBRARY:FILEPATH="
                                           (assoc-ref %build-inputs
                                                      "blas/lapack")
                                           "/lib/libopenblas.so")
                           ,(string-append "-DLAPACK_LIBRARY:FILEPATH="
                                           (assoc-ref %build-inputs
                                                      "fortran:lib")
                                           "/lib/libgfortran.so;"
                                           (assoc-ref %build-inputs
                                                      "fortran:lib")
                                           "/lib/libquadmath.so"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'set-numpy-path ; Needed for the unit tests.
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((pyver (python-version (assoc-ref inputs "python")))
                    (npdir (string-append (assoc-ref inputs "numpy")
                                          "/lib/python" pyver
                                          "/site-packages")))
                (substitute* "src/cmake/Modules/Optizelle.cmake"
                  (("PYTHONPATH=")
                   (string-append "LD_LIBRARY_PATH=$ENV{LIBRARY_PATH};"
                                  "PYTHONPATH=" npdir ":"))))))
         (delete 'install-license-files)))) ; LICENSE.txt is installed.
    (inputs
     `(("blas/lapack" ,openblas)
       ("fortran:lib" ,gfortran "lib")
       ("jsoncpp" ,jsoncpp)
       ("numpy" ,python-numpy)
       ("python" ,python)))
    (native-inputs
     `(("fortran" ,gfortran)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.optimojoe.com/products/optizelle/")
    (synopsis "Mathematical optimization library")
    (description "@code{optizelle} is a software library designed to
solve nonlinear optimization problems.  Four types of problem are
considered: unconstrained, equality constrained, inequality
constrained and constrained.  Constraints may be applied as values of
functions or sets of partial differential equations (PDEs).

Solution algorithms such as the preconditioned nonlinear conjugate
gradient method, sequential quadratic programming (SQP) and the
primal-dual interior-point method are made available.  Interfaces are
provided for applications written in C++ and Python.  Parallel
computation is supported via MPI.")
    (license license:bsd-2))))

(define-public scilab
  (package
    (name "scilab")
    (version "2025.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/scilab/scilab")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1zn31mby7rwxsnkg91rp92ixhj785hw459krw9k2prs2cxqpn6j5"))
       (patches (search-patches "scilab-better-compiler-detection.patch"
                                "scilab-tbx_build_help.patch"))
       (modules '((guix build utils)
                  (ice-9 ftw)))
       (snippet
        #~(begin
            ;; Delete everything except for scilab itself:
            (for-each (lambda (file)
                        (unless (member file '("." ".." "scilab"))
                          (delete-file-recursively file)))
                      (scandir "."))
            (for-each (lambda (file)
                        (unless (member file '("." ".."))
                          (rename-file (string-append "scilab/" file) file)))
                      (scandir "scilab"))

            ;; Some clean-up in scilab:
            (for-each delete-file-recursively
                      '("scilab"
                        "config"
                        "libs/GetWindowsVersion"
                        "Visual-Studio-settings"))
            (for-each delete-file
                      (append
                       (list "aclocal.m4"
                             "configure"
                             "m4/ax_cxx_compile_stdcxx.m4"
                             "m4/lib-ld.m4"
                             "m4/libtool.m4"
                             "m4/ltoptions.m4"
                             "m4/ltsugar.m4"
                             "m4/ltversion.m4"
                             "m4/lt~obsolete.m4"
                             "m4/pkg.m4"
                             "Scilab.sln")
                       (find-files "." "^Makefile\\.in$")
                       (find-files "." "\\.bat$")))

            ;; And finally some files in the modules directory:
            (for-each
             (lambda (file)
               (delete-file
                (string-append "modules/dynamic_link/src/scripts/" file)))
             '("aclocal.m4"
               "configure"
               "compile"
               "config.guess"
               "config.sub"
               "depcomp"
               "install-sh"
               "ltmain.sh"
               "missing"))
            (delete-file-recursively "modules/dynamic_link/src/scripts/m4")
            (for-each delete-file
                      '("modules/ast/src/cpp/parse/scanscilab.cpp"
                        "modules/ast/src/cpp/parse/bison/parsescilab.output"
                        "modules/ast/includes/parse/parsescilab.hxx"
                        "modules/ast/src/cpp/parse/parsescilab.cpp"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf-2.71
           autoconf-archive
           automake
           bison
           coreutils-minimal
           eigen
           flex
           gfortran
           libtool
           ocaml
           ocaml-findlib
           pkg-config))
    (inputs (list `(,pcre "bin")
                  `(,pcre "out")
                  arpack-ng
                  bash-minimal
                  curl
                  fftw
                  gettext-minimal
                  hdf5
                  libarchive
                  libx11
                  libxml2
                  matio
                  ocaml-num
                  openblas
                  readline
                  suitesparse
                  tcl
                  tk))
    (arguments
     (let* ((tcl (this-package-input "tcl"))
            (tk (this-package-input "tk")))
       (list
        #:configure-flags
        #~(list
           "--enable-relocatable"
           "--disable-static-system-lib"
           "--enable-build-parser"
           ;; Disable all java code.
           "--without-gui"
           "--without-javasci"
           "--disable-build-help"
           "--with-external-scirenderer"
           ;; Tcl and Tk library locations.
           (string-append "--with-tcl-include=" #$tcl "/include")
           (string-append "--with-tcl-library=" #$tcl "/lib")
           (string-append "--with-tk-include=" #$tk "/include")
           (string-append "--with-tk-library=" #$tk "/lib")
           (string-append "--with-eigen-include="
                          (search-input-directory %build-inputs "include/eigen3"))
           ;; Find and link to the OCaml Num package
           "OCAMLC=ocamlfind ocamlc -package num"
           "OCAMLOPT=ocamlfind ocamlopt -package num -linkpkg")
        #:phases
        #~(modify-phases %standard-phases
            ;; The Num library is specified with the OCAMLC and
            ;; OCAMLOPT variables above.
            (add-after 'unpack 'fix-ocaml-num
              (lambda _
                (substitute*
                    '("modules/scicos/Makefile.modelica.am"
                      "modules/scicos/src/translator/makefile.mak"
                      "modules/scicos/src/modelica_compiler/makefile.mak")
                  (("nums\\.cmx?a") ""))))
            (add-after 'unpack 'fix-linking
              (lambda _
                (substitute* "modules/Makefile.am"
                  (("libscilab_cli_la_LDFLAGS = .*\\)" all)
                   (string-append all " -lcurl")))))
            (add-after 'unpack 'set-version
              (lambda _
                (substitute* "modules/core/includes/version.h.in"
                  (("branch-[a-z0-9\\.]*")  ; version
                   #$(version-major+minor (package-version this-package))))))
            (add-after 'unpack 'restrain-to-scilab-cli
              (lambda _
                ;; Install only scilab-cli.desktop
                (substitute* "desktop/Makefile.am"
                  (("desktop_DATA =")
                   "desktop_DATA = scilab-cli.desktop\nDUMMY ="))
                ;; Replace scilab with scilab-cli for tests.
                (substitute* "Makefile.incl.am"
                  (("scilab-bin") "scilab-cli-bin")
                  (("scilab -nwni") "scilab-cli")
                  ;; Do not install tests, demos and examples.
                  ;; This saves up to 140 Mo in the final output.
                  (("(TESTS|DEMOS|EXAMPLES)_DIR=.*" all kind)
                   (string-append kind "_DIR=")))))
            (add-before 'check 'disable-failing-tests
              (lambda _
                (substitute* "Makefile"
                  (("TESTS = .*")
                   "TESTS =\n"))
                (substitute* "modules/functions_manager/Makefile"
                  (("check:.*")
                   "check:\n"))
                (substitute* "modules/types/Makefile"
                  (("\\$\\(MAKE\\) \\$\\(AM_MAKEFLAGS\\) check-am")
                   ""))))
            ;; These generated files are assumed to be present during
            ;; the build.
            (add-after 'bootstrap 'bootstrap-dynamic_link-scripts
              (lambda _
                (with-directory-excursion "modules/dynamic_link/src/scripts"
                  ((assoc-ref %standard-phases 'bootstrap))
                  (substitute* "compilerDetection.sh"
                    (("PATHTOCONFIGURE=.*")
                     "PATHTOCONFIGURE=${BASH_SOURCE[0]%/*}/\n")
                    (("PROGNAME.*") "\n")))))
            (add-before 'build 'pre-build
              (lambda* (#:key inputs #:allow-other-keys)
                ;; Fix scilab script.
                (substitute* "bin/scilab"
                  (("/bin/ls")
                   (search-input-file inputs "bin/ls")))
                ;; Fix core.start.
                (substitute* "modules/core/etc/core.start"
                  (("'SCI/modules")
                   "SCI+'/modules"))))
            ;; Prevent race condition
            (add-after 'pre-build 'build-parsers
              (lambda* (#:key (make-flags #~'()) #:allow-other-keys)
                (with-directory-excursion "modules/ast"
                  (apply invoke "make"
                         "src/cpp/parse/parsescilab.cpp"
                         "src/cpp/parse/scanscilab.cpp"
                         make-flags))))
            ;; The startup script is mostly there to define the following env
            ;; variables properly. We can do this with guix directly.
            (add-after 'install 'rewrap-scilab-cli
              (lambda _
                (define (bin path) (string-append #$output "/bin/" path))
                (delete-file (bin "scilab-cli"))
                (wrap-program (bin "scilab-cli-bin")
                  `("SCI" = (,(string-append #$output "/share/scilab")))
                  `("LD_LIBRARY_PATH" ":" prefix
                    (,(string-append #$output "/lib/scilab")))
                  `("TCL_LIBRARY" = (,(string-append #$tcl "/lib")))
                  `("TK_LIBRARY" = (,(string-append #$tk "/lib"))))
                (copy-file (bin "scilab-cli-bin") (bin "scilab-cli"))
                (copy-file (bin ".scilab-cli-bin-real") (bin "scilab-cli-bin"))
                (delete-file (bin ".scilab-cli-bin-real"))
                (substitute* (bin "scilab-cli")
                  ;; Also set SCIHOME to sensible XDG base dirs value.
                  (("\\.scilab-cli-bin-real\"")
                   (string-append
                    "scilab-cli-bin\" -scihome "
                    "\"${XDG_STATE_HOME:-$HOME/.local/state}/scilab/"
                    #$(package-version this-package) "\""))
                  (("export SCI=")
                   "unset LANGUAGE\nexport SCI="))))
            (add-after 'rewrap-scilab-cli 'sanity-check
              (lambda _
                (setenv "HOME" (getcwd))
                (invoke (string-append #$output "/bin/scilab-cli")
                        "-e" "\"quit;\"")))))))
    (home-page "https://www.scilab.org/")
    (synopsis "Software for engineers and scientists")
    (description "This package provides the non-graphical version of the Scilab
software for engineers and scientists.  Scilab is used for signal processing,
statistical analysis, image enhancement, fluid dynamics simulations, numerical
optimization, and modeling, simulation of explicit and implicit dynamical
systems and symbolic manipulations.")
    (license (list license:gpl2 license:bsd-3))))

(define-public ruy
  (let ((commit "caa244343de289f913c505100e6a463d46c174de")
        (version "0")
        (revision "1"))
    (package
      (name "ruy")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/google/ruy")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0j2g90nzam4h52zwx2vpanj8m17068cfb1zi4hcy0pyk52kb11dy"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list "-DRUY_FIND_CPUINFO=ON"
                ;; Needed to make sure code is relocatable for use in
                ;; tensorflow.
                "-DCMAKE_CXX_FLAGS=-fPIC ")))
      (inputs (list cpuinfo))
      (native-inputs (list googletest))
      (home-page "https://github.com/google/ruy")
      (synopsis "Matrix multiplication library")
      (description
       "Ruy is a matrix multiplication library.  Its focus is to cover the
matrix multiplication needs of neural network inference engines.  Its initial
user has been TensorFlow Lite, where it is used by default on the ARM CPU
architecture.")
      (license license:asl2.0))))

(define-public bliss
  (package
    (name "bliss")
    (version "0.77")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://users.aalto.fi/~tjunttil/bliss/downloads/bliss-"
                    version ".zip"))
              (sha256
               (base32
                "193jb63kdwfas2cf61xj3fgkvhb6v2mnbwwpr0jas3zk6j0bkj5c"))))
    (build-system cmake-build-system)
    (arguments
     (list
      ;; There are no tests
      #:tests? #f
      #:configure-flags #~(list "-DUSE_GMP=ON") ; Used by igraph
      #:phases
      #~(modify-phases %standard-phases
          ;; Ensure that GMP is used, otherwise the BigNum type changes.
          (add-after 'unpack 'define-use-gmp
            (lambda _
              (substitute* "src/bignum.hh"
                (("#pragma once.*" all)
                 (string-append all "#define BLISS_USE_GMP")))))
          ;; Move headers under the bliss/ prefix. This is a Guix choice,
          ;; since there are no upstream installation instructions and the
          ;; header names are sufficiently generic to cause confusions with
          ;; other packages (e.g. "heap.hh").
          (add-after 'define-use-gmp 'move-headers
            (lambda _
              (substitute* (find-files "src")
                (("#include \"(.*)\"" _ path)
                 (string-append "#include <bliss/" path ">")))
              (mkdir-p "include/bliss")
              (for-each
               (lambda (file)
                 (rename-file file
                              (string-append "include/bliss/" (basename file))))
               (find-files "src" "\\.(h|hh)$"))
              (substitute* "Doxyfile"
                (("INPUT *=.*") "INPUT = bliss"))))
          (add-after 'move-headers 'patch-cmake
            (lambda _
              (let ((port (open-file "CMakeLists.txt" "a")))
                (display
                 (apply
                  string-append
                  ;; Install the executable and the shared library.
                  "install(TARGETS bliss)\n"
                  "install(TARGETS bliss-executable)\n"
                  "install(DIRECTORY include/bliss DESTINATION include)\n"
                  "target_link_libraries(bliss PUBLIC ${GMP_LIBRARIES})\n"
                  ;; Missing include directories.
                  (map
                   (lambda (name)
                     (string-append
                      "target_include_directories(" name " PUBLIC\n"
                      "${CMAKE_CURRENT_SOURCE_DIR}/include"
                      " ${GMP_INCLUDE_DIR})\n"))
                   '("bliss" "bliss_static" "bliss-executable")))
                 port)
                (close-port port))))
          (add-after 'build 'build-doc
            (lambda _
              (mkdir "doc")
              (with-directory-excursion "doc"
                (let ((srcdir (string-append "../../bliss-" #$version)))
                  (copy-recursively (string-append srcdir "/include/bliss")
                                    "bliss")
                  (invoke "doxygen" (string-append srcdir "/Doxyfile"))))))
          (add-after 'install 'install-doc
            (lambda _
              (copy-recursively
               "doc/html" (string-append #$output "/share/doc/"
                                     #$name "-" #$version "/html")))))))
    (native-inputs (list doxygen graphviz unzip))
    (propagated-inputs (list gmp))
    (home-page "https://users.aalto.fi/~tjunttil/bliss/index.html")
    (synopsis "Tool for computing automorphism groups and canonical labelings of graphs")
    (description "@code{bliss} is a library for computing automorphism groups
+and canonical forms of graphs.  It has both a command line user interface as
+well as C++ and C programming language APIs.")
    (license license:lgpl3)))

(define-public python-mathics-scanner
  (package
    (name "python-mathics-scanner")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Mathics3/mathics-scanner.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0y34kzqha5wp6n8cyvhhz47mq33x9kwi8ibj67q6pf08qslg154n"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare
            (lambda _
              ;; They forgot to update the version number.
              (substitute* "mathics_scanner/version.py"
               (("__version__=\"[^\"]*\"")
                (string-append "__version__=\"" #$version "\"")))
              (invoke "bash" "./admin-tools/make-JSON-tables.sh")
              ;; Missing installation of "operators.yml".
              (substitute* "pyproject.toml"
               (("\"data/named-characters.yml\",")
                "\"data/named-characters.yml\", \"data/operators.yml\","))
              ;; Would cause a crash at runtime every time you select
              ;; anything when running build_tables.py .
              (substitute* "mathics_scanner/generate/build_tables.py"
               (("\"operator-to-amslatex\",") "")))))))
    (propagated-inputs (list python-chardet python-click python-pyyaml))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://mathics.org/")
    (synopsis
     "Character tables and tokenizer for Mathics and the Wolfram language")
    (description
     "This package provides character tables and a tokenizer for Mathics and
the Wolfram language.")
    (license license:gpl3+)))

(define-public python-mathics-pygments
  (package
    (name "python-mathics-pygments")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mathics_pygments" version))
       (sha256
        (base32 "1iagdic8f0yjx01kdds40jfcxcpdbrd3i0ywydl01dhyyvd2yjk9"))))
    (build-system pyproject-build-system)
    (propagated-inputs (list python-mathics-scanner python-pygments))
    (native-inputs (list python-setuptools python-wheel))
    (home-page "http://github.com/Mathics3/mathics-pygments/")
    (synopsis "Wolfram language lexer for Pygments")
    (description "This package provides a Wolfram language lexer for Pygments.")
    (license license:expat)))

(define-public python-mathics-core
  (package
    (name "python-mathics-core")
    (version "8.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Mathics3/mathics-core.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ikgw3w3silxisih92g1wgcwb37k7qkwfmdv5r6yy4ki74qvyk5q"))))
    (arguments
     `(;; <https://github.com/pytest-dev/pytest/pull/10173> is missing .closed
       #:test-flags '("-s")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bugs
           (lambda _
             (substitute* "pyproject.toml"
              (("\"data/*.json\",")
              "\"data/*.json\", \"data/operator-tables.json\",")
              (("\"autoload/\\*.m\",")
               ;; They forgot to install autoload/rules/*.m
               "\"autoload/*.m\", \"autoload/rules/*.m\","))
             ;; Prevent internet access by tests.
             (substitute* "mathics/builtin/files_io/files.py"
              (("https://raw.githubusercontent.com/Mathics3/mathics-core/master/README.rst")
               (string-append (getcwd) "/README.rst")))
             ;; setup.py has some weird acrobatics that cannot work right.
             (invoke "mathics3-generate-operator-json-table" "-o"
                     "mathics/data/operator-tables.json")))
         (add-before 'check 'prepare-check
           (lambda* (#:key inputs outputs #:allow-other-keys)
             ;(copy-file "operator-tables.json" "mathics/data/operator-tables.json")
             ; Doesn't work: (add-installed-pythonpath inputs outputs)
             (setenv "PYTHONPATH" (getcwd))))
         (add-before 'check 'prepare-locales
           (lambda _
             ;; Otherwise 210 tests fail because the real output would use
             ;; unicode arrow characters.  With this, only 18 (symbolic) tests fail.
             (setenv "MATHICS_CHARACTER_ENCODING" "ASCII"))))))
    (build-system pyproject-build-system)
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (inputs (list llvm))
    (propagated-inputs (list python-mpmath
                             python-pint
                             python-palettable
                             python-pympler
                             python-stopit
                             python-sympy
                             python-numpy
                             python-mathics-scanner
                             python-pillow
                             python-dateutil
                             python-requests
                             python-llvmlite
                             python-scipy))
    (synopsis "Computer algebra system")
    (description "This package provides a computer algebra system--an alternative
to Wolfram.")
    (home-page "https://mathics.org/")
    (license license:gpl3)))

(define-public python-mathicsscript
  (package
    (name "python-mathicsscript")
    (version "8.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mathicsscript" version))
       (sha256
        (base32 "12si397b9ap5ibvbap72bvkmssh8hdap8jbmdmhsj1qdb1axrac4"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'check 'setenv
           (lambda _
             (setenv "HOME" "/tmp"))))))
    (propagated-inputs (list python-click
                             python-colorama
                             python-columnize
                             python-mathics-pygments
                             python-mathics-scanner
                             python-mathics-core
                             python-networkx
                             python-prompt-toolkit
                             python-pygments
                             python-term-background))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://mathics.org/")
    (synopsis "Command-line interface to Mathics3")
    (description "This package provides a command-line interface to
Mathics3.")
    (license license:gpl3)))

(define-public python-mathics-django
  (package
    (name "python-mathics-django")
    (version "8.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "Mathics_Django" version))
       (sha256
        (base32 "1cwvm72w6c3vz49gj09ihiwicj291ia51xhkxgi9z88a9vszx5pq"))))
    (build-system pyproject-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'check)
         (add-after 'build 'check
           (lambda _
             (setenv "PYTHONPATH" (getcwd))
             (setenv "DJANGO_SETTINGS_MODULE" "mathics_django.settings")
             (invoke "django-admin" "test"))))))
    (propagated-inputs (list python-django-4.2
                             python-mathics-scanner
                             python-mathics-core
                             python-networkx
                             python-pygments
                             python-requests))
    (native-inputs (list python-pytest python-setuptools python-wheel))
    (home-page "https://mathics.org/")
    (synopsis "A Django front end for Mathics3.")
    (description "This package provides a Django front end for Mathics3.")
    (license license:gpl3)))

(define-public lie
  (package
    (name "lie")
    (version "2.2.2")
    ;; Original: <http://www-math.univ-poitiers.fr/~maavl/LiE/conLiE.tar.gz>
    ;; This source has the license file added as allowed on
    ;; <http://www-math.univ-poitiers.fr/~maavl/LiE/>.
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/daym/LiE")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qb9y08mdl2wr77lf61wv4xks429sw99xacb9gh91w242z0nbcqn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:parallel-build? #f
       ;; There are test input files under progs/--but no expected results.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "make_lie"
              (("`/bin/pwd`")
               (string-append (assoc-ref outputs "out")
                              "/bin")))))
         (add-after 'install 'install-lie-program
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
               (lambda (name)
                 (install-file name
                               (string-append (assoc-ref outputs "out")
                                              "/bin")))
                  '("lie" "Lie.exe")))))))
    (native-inputs
     (list bison))
    (inputs
     (list readline))
    (synopsis "Lie group computer algebra module")
    (description "This package provides a computer algebra module for Lie
groups.  Documentation is available on
@url{http://www-math.univ-poitiers.fr/~maavl/pdf/LiE-manual.pdf}.")
    (home-page "http://www-math.univ-poitiers.fr/~maavl/LiE/")
    ;; <http://www-math.univ-poitiers.fr/~maavl/LiE/> says LGPL.
    (license license:lgpl3+)))

(define-public exprtk
  (package
    (name "exprtk")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ArashPartow/exprtk")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pszh11w29nc256qhil51g3635n06ncf0ihg7g4h86jrhqsk7183"))))
    (build-system copy-build-system)
    (arguments
     '(#:install-plan '(("exprtk.hpp" "include/"))))
    (synopsis "C++ Mathematical Expression Parsing And Evaluation Library")
    (description "ExprTk is a C++ headers only library for mathematical
expression parsing and evaluation.")
    (home-page "https://www.partow.net/programming/exprtk/index.html")
    (license license:expat)))

(define-public highs
  (package
    (name "highs")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ERGO-Code/HiGHS")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19q99nkk6r6k91gxp8a5rjil1399qyfgfc6jqlg2gd82vpcw8c8b"))))
    (build-system cmake-build-system)
    (home-page "https://highs.dev")
    (synopsis "High performance software for linear optimization")
    (description
     "HiGHS provides serial and parallel solvers for large-scale sparse
linear programming (LP), mixed-integer programming (MIP), and quadratic
programming (QP) models")
    (license license:expat)))
