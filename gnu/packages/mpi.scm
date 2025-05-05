;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2018, 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2017, 2022, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Paul Garlick <pgarlick@tourbillion-technology.com>
;;; Copyright © 2019, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Romain Garbage <romain.garbage@inria.fr>
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

(define-module (gnu packages mpi)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix deprecation)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fabric-management)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages profiling)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages valgrind)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match))

(define-public hwloc-1
  (package
    (name "hwloc")
    (version "1.11.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.open-mpi.org/software/hwloc/v"
                                  (version-major+minor version)
                                  "/downloads/hwloc-" version ".tar.bz2"))
              (sha256
               (base32
                "1j69p8a1pjpbpwn4w7l4dfxmaxxqikchjzqw1ncw05zmcmvlnjd4"))
              (patches (search-patches "hwloc-1-test-btrfs.patch"))))

    (properties
     ;; Tell the 'generic-html' updater to monitor this URL for updates.
     `((release-monitoring-url
        . "https://www-lb.open-mpi.org/software/hwloc/current")))

    (build-system gnu-build-system)
    (outputs '("out"           ;'lstopo' & co., depends on Cairo, libx11, etc.
               "lib"           ;small closure
               "doc"           ;400+ section 3 man pages
               "debug"))
    (inputs
     (append (if (%current-target-system)
                 '()                  ;fewer dependencies when cross-compiling
                 (list libx11 cairo ncurses expat))
             (if (target-arm32?) '() (list numactl))))
    (propagated-inputs
     ;; hwloc.pc lists it in 'Requires.private'.
     (list libpciaccess))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:configure-flags '("--localstatedir=/var")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'skip-linux-libnuma-test
           (lambda _
             ;; Arrange to skip 'tests/linux-libnuma', which fails on some
             ;; machines: <https://github.com/open-mpi/hwloc/issues/213>.
             (substitute* "tests/linux-libnuma.c"
               (("numa_available\\(\\)")
                "-1"))))
         (add-after 'install 'refine-libnuma
           ;; Give -L arguments for libraries to avoid propagation
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out  (assoc-ref outputs "lib"))
                   (numa (assoc-ref inputs "numactl")))
               (substitute* (map (lambda (f) (string-append out "/" f))
                                 '("lib/pkgconfig/hwloc.pc" "lib/libhwloc.la"))
                 (("-lnuma" lib)
                  (string-append "-L" numa "/lib " lib))))))
         (add-after 'install 'avoid-circular-references
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((lib (assoc-ref outputs "lib")))
               ;; Suppress the 'prefix=' and 'exec_prefix=' lines so that the
               ;; "lib" output doesn't refer to "out".
               (substitute* (string-append lib "/lib/pkgconfig/hwloc.pc")
                 (("^.*prefix=.*$")
                  "")))))
         (add-after 'install 'move-man3-pages
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Move section 3 man pages to the "doc" output.
             (let ((out (assoc-ref outputs "out"))
                   (doc (assoc-ref outputs "doc")))
               (copy-recursively (string-append out "/share/man/man3")
                                 (string-append doc "/share/man/man3"))
               (delete-file-recursively
                (string-append out "/share/man/man3"))))))))
    (home-page "https://www.open-mpi.org/projects/hwloc/")
    (synopsis "Abstraction of hardware architectures")
    (description
     "hwloc provides a portable abstraction (across OS,
versions, architectures, ...) of the hierarchical topology of modern
architectures, including NUMA memory nodes, sockets, shared caches, cores and
simultaneous multithreading.  It also gathers various attributes such as cache
and memory information.  It primarily aims at helping high-performance
computing applications with gathering information about the hardware so as to
exploit it accordingly and efficiently.

hwloc may display the topology in multiple convenient formats.  It also offers
a powerful programming interface to gather information about the hardware,
bind processes, and much more.")
    (license license:bsd-3)))

(define-public hwloc-2
  (package
    (inherit hwloc-1)
    (version "2.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.open-mpi.org/release/hwloc/v"
                                  (version-major+minor version)
                                  "/hwloc-" version ".tar.bz2"))
              (sha256
               (base32
                "1m2qkjw35qj9gv9yjn1p46bahdp9l133hs34j61lwwd5q2ys5806"))))

    (native-inputs (modify-inputs (package-native-inputs hwloc-1)
                     (append bash)))              ;for completion tests
    (inputs (modify-inputs (package-inputs hwloc-1)
              (delete "numactl")))               ;libnuma is no longer needed.
    (arguments
     (substitute-keyword-arguments (package-arguments hwloc-1)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'skip-linux-libnuma-test
             (lambda _
               ;; Arrange to skip 'tests/hwloc/linux-libnuma', which fails on
               ;; some machines: <https://github.com/open-mpi/hwloc/issues/213>.
               (substitute* "tests/hwloc/linux-libnuma.c"
                 (("numa_available\\(\\)")
                  "-1"))))
           (add-before 'check 'skip-tests-that-require-/sys
             (lambda _
               ;; 'test-gather-topology.sh' requires /sys as of 2.9.0; skip it.
               (setenv "HWLOC_TEST_GATHER_TOPOLOGY" "0")))
           (add-before 'check 'skip-test-that-fails-on-qemu
             (lambda _
               ;; Skip test that fails on emulated hardware due to QEMU bug:
               ;; <https://bugs.gnu.org/40342>.
               (substitute* "tests/hwloc/hwloc_get_last_cpu_location.c"
                 (("hwloc_topology_init" all)
                  (string-append "exit (77);\n" all)))))))))))

(define-public hwloc
  ;; The latest stable series of hwloc.
  hwloc-2)

(define-public openmpi-4
  (package
    (name "openmpi")
    (version "4.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.open-mpi.org/software/ompi/v"
                           (version-major+minor version)
                           "/downloads/openmpi-" version ".tar.bz2"))
       (sha256
        (base32 "05g4245v0pdqcyjmgn50519ry5v5q8ig26iinmiynvaihm29jh7p"))
       (patches (search-patches "openmpi-mtl-priorities.patch"))))

    (properties
     ;; Tell the 'generic-html' updater to monitor this URL for updates.
     `((release-monitoring-url
        . "https://www.open-mpi.org/software/ompi/current")))

    (build-system gnu-build-system)
    (inputs
     (let ((if-supported
            (lambda (package)
              (if (and (not (%current-target-system))
                       (member (%current-system)
                               (package-supported-systems package)))
                  (list package)
                  '()))))
       (append (list `(,hwloc-2 "lib")
                     gfortran
                     libfabric
                     libevent
                     opensm
                     openssh-sans-x)
               (if-supported psm)
               (if-supported psm2)
               (if-supported ucx)
               (if-supported valgrind/pinned)
               (list rdma-core
                     slurm))))         ;for PMI support (launching via "srun")
    (native-inputs
     (list pkg-config perl))
    (outputs '("out" "debug"))
    (arguments
     (list
      #:configure-flags #~`("--enable-mpi-ext=affinity" ;cr doesn't work
                            "--with-sge"
                            "--disable-static"

                            #$@(if (package? (this-package-input "valgrind"))
                                   #~("--enable-memchecker"
                                      "--with-valgrind")
                                   #~("--without-valgrind"))

                            "--with-hwloc=external"
                            "--with-libevent"

                            ;; Help 'orterun' and 'mpirun' find their tools
                            ;; under $prefix by default.
                            "--enable-mpirun-prefix-by-default"

                            ;; InfiniBand support
                            "--enable-openib-control-hdr-padding"
                            "--enable-openib-dynamic-sl"
                            "--enable-openib-udcm"
                            "--enable-openib-rdmacm"
                            "--enable-openib-rdmacm-ibaddr"

                            ;; Enable support for the 'Process Management
                            ;; Interface for Exascale' (PMIx) used e.g. by
                            ;; Slurm for the management communication and
                            ;; coordination of MPI processes.
                            "--with-pmix=internal"

                            ;; Enable support for SLURM's Process Manager
                            ;; Interface (PMI).
                            ,(string-append "--with-pmi="
                                            #$(this-package-input "slurm")))
      #:phases #~(modify-phases %standard-phases
                   ;; opensm is needed for InfiniBand support.
                   (add-after 'unpack 'find-opensm-headers
                     (lambda* (#:key inputs #:allow-other-keys)
                       (setenv "C_INCLUDE_PATH"
                               (search-input-directory inputs
                                                       "/include/infiniband"))
                       (setenv "CPLUS_INCLUDE_PATH"
                               (search-input-directory inputs
                                                       "/include/infiniband"))))
                   (add-before 'build 'remove-absolute
                     (lambda _
                       ;; Remove compiler absolute file names (OPAL_FC_ABSOLUTE
                       ;; etc.) to reduce the closure size.  See
                       ;; <https://lists.gnu.org/archive/html/guix-devel/2017-07/msg00388.html>
                       ;; and
                       ;; <https://www.mail-archive.com/users@lists.open-mpi.org//msg31397.html>.
                       (substitute* '("orte/tools/orte-info/param.c"
                                      "oshmem/tools/oshmem_info/param.c"
                                      "ompi/tools/ompi_info/param.c")
                         (("_ABSOLUTE") ""))
                       ;; Avoid valgrind (which pulls in gdb etc.).
                       (substitute*
                           '("./ompi/mca/io/romio321/src/io_romio321_component.c")
                         (("MCA_io_romio321_COMPLETE_CONFIGURE_FLAGS")
                          "\"[elided to reduce closure]\""))))
                   (add-before 'build 'ssh-absolute-path
                     (lambda* (#:key inputs #:allow-other-keys)
                       ;; Avoid error at run time about 'ssh' not being found.
                       (substitute* "orte/mca/plm/rsh/plm_rsh_component.c"
                         (("mca_plm_rsh_component.agent = \"ssh : rsh\";")
                          (format #f "mca_plm_rsh_component.agent = \"~a : rsh\";"
                                  (search-input-file inputs
                                                     "bin/ssh"))))))
                   (add-before 'build 'scrub-timestamps ;reproducibility
                     (lambda _
                       (substitute* '("ompi/tools/ompi_info/param.c"
                                      "orte/tools/orte-info/param.c"
                                      "oshmem/tools/oshmem_info/param.c")
                         ((".*(Built|Configured) on.*") ""))))
                   (add-after 'install 'remove-logs ;reproducibility
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (for-each delete-file (find-files out "config.log"))))))))
    (home-page "https://www.open-mpi.org")
    (synopsis "MPI-3 implementation")
    (description
     "The Open MPI Project is an MPI-3 implementation that is developed and
maintained by a consortium of academic, research, and industry partners.  Open
MPI is therefore able to combine the expertise, technologies, and resources
from all across the High Performance Computing community in order to build the
best MPI library available.  Open MPI offers advantages for system and
software vendors, application developers and computer science researchers.")
    ;; See file://LICENSE
    (license license:bsd-2)))

(define-public openmpi openmpi-4)

(define-public openmpi-5
  (package
    (inherit openmpi)
    (version "5.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.open-mpi.org/software/ompi/v"
                           (version-major+minor version)
                           "/downloads/openmpi-" version ".tar.bz2"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        '(begin
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
           ;; Delete as many bundled libraries as permitted by the build
           ;; system.
           (delete-all-but "3rd-party" "treematch" "Makefile.in" "Makefile.am")
           ;; Do not install 64 MiB worth fo pre-generated HTML
           ;; documentation.
           (delete-file-recursively "docs/html")))
       (sha256
        (base32 "1pf25zp9y0ch3vab3ycpjkck4njrsms0sg6zs0s36h3ajc4j17qi"))))

    (inputs (modify-inputs (package-inputs openmpi)
              ;; As of Open MPI 5.0.X, PMIx is used to communicate
              ;; with SLURM, so SLURM'S PMI is no longer needed.
              (delete "slurm")
              (append openpmix)         ;for PMI support (launching via "srun")
              (append prrte)))          ;for PMI support (launching via "srun")
    (native-inputs (modify-inputs (package-native-inputs openmpi)
                     (append python)))

    (outputs '("out" "debug"))
    (arguments
     (list #:configure-flags
           #~(list "--enable-mpi-ext=affinity"         ;cr doesn't work
                   "--with-sge"
                   "--disable-static"

                   #$@(if (package? (this-package-input "valgrind"))
                          #~("--enable-memchecker"
                             "--with-valgrind")
                          #~("--without-valgrind"))

                   "--with-hwloc=external"
                   "--with-libevent"

                   ;; This replaces --enable-mpirun-prefix-by-default wich is deprecated
                   ;; since 5.x.
                   "--enable-prte-prefix-by-default"

                   ;; Enable support for the 'Process Management Interface for Exascale'
                   ;; (PMIx) used e.g. by Slurm for the management communication and
                   ;; coordination of MPI processes.
                   (string-append "--with-pmix=" #$(this-package-input "openpmix"))
                   (string-append "--with-prrte=" #$(this-package-input "prrte"))

                   ;; Since 5.x, Infiniband support is provided by ucx.
                   ;; See https://docs.open-mpi.org/en/main/release-notes/networks.html#miscellaneous-network-notes
                   #$@(if (package? (this-package-input "ucx"))
                          #~((string-append "--with-ucx=" #$(this-package-input "ucx")))
                          #~()))

           #:phases
           #~(modify-phases %standard-phases
               (add-before 'build 'remove-absolute
                 (lambda _
                   ;; Remove compiler absolute file names (OPAL_FC_ABSOLUTE
                   ;; etc.) to reduce the closure size.  See
                   ;; <https://lists.gnu.org/archive/html/guix-devel/2017-07/msg00388.html>
                   ;; and
                   ;; <https://www.mail-archive.com/users@lists.open-mpi.org//msg31397.html>.
                   (substitute* '("oshmem/tools/oshmem_info/param.c"
                                  "ompi/tools/ompi_info/param.c")
                     (("_ABSOLUTE") "")))))

           #:disallowed-references (list (canonical-package gcc))))))

(define-public openmpi-c++
  (package/inherit openmpi
    (name "openmpi-c++")
    (outputs '("out"))
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags)
        #~(cons "--enable-mpi-cxx" #$flags))))
    (synopsis "C++ bindings for MPI")))

;; TODO: javadoc files contain timestamps.
(define-public java-openmpi
  (package/inherit openmpi
    (name "java-openmpi")
    (inputs
     `(("openmpi" ,openmpi)
       ,@(package-inputs openmpi)))
    (native-inputs
     `(("jdk" ,openjdk11 "jdk")
       ("zip" ,(@ (gnu packages compression) zip))
       ,@(package-native-inputs openmpi)))
    (outputs '("out"))
    (arguments
     (cons*
      #:modules '((guix build gnu-build-system)
                  ((guix build ant-build-system) #:prefix ant:)
                  (guix build utils))
      #:imported-modules `((guix build ant-build-system)
                           ,@%default-gnu-imported-modules)
      (substitute-keyword-arguments (package-arguments openmpi)
        ((#:configure-flags flags)
         #~(cons "--enable-mpi-java" #$flags))
        ((#:make-flags flags ''())
         #~(append '("-C" "ompi/mpi/java")
                   #$flags))
        ((#:phases phases)
         #~(modify-phases #$phases
             ;; We could provide the location of the JDK in the configure
             ;; flags, but since the configure flags are embedded in the
             ;; info binaries that would leave a reference to the JDK in
             ;; the "out" output.  To avoid this we set JAVA_HOME.
             (add-after 'unpack 'set-JAVA_HOME
               (lambda* (#:key inputs #:allow-other-keys)
                 (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
                 #t))
             (add-after 'unpack 'link-with-existing-mpi-libraries
               (lambda* (#:key inputs #:allow-other-keys)
                 (substitute* "ompi/mpi/java/c/Makefile.in"
                   (("\\$\\(top_builddir\\)/ompi/lib@OMPI_LIBMPI_NAME@.la")
                    (search-input-file inputs "/lib/libmpi.la")))))
             (add-after 'install 'strip-jar-timestamps
               (assoc-ref ant:%standard-phases 'strip-jar-timestamps)))))))
    (synopsis "Java bindings for MPI")))

(define-public openmpi-thread-multiple
  (package/inherit openmpi
    (name "openmpi-thread-multiple")
    (arguments
     (substitute-keyword-arguments (package-arguments openmpi)
       ((#:configure-flags flags)
        #~(cons "--enable-mpi-thread-multiple" #$flags))))
    (description "This version of Open@tie{}MPI has an implementation of
@code{MPI_Init_thread} that provides @code{MPI_THREAD_MULTIPLE}.  This won't
work correctly with all transports (such as @code{openib}), and the
performance is generally worse than the vanilla @code{openmpi} package, which
only provides @code{MPI_THREAD_FUNNELED}.")))

;;; Build phase to be used for packages that execute MPI code.
(define-public %openmpi-setup
  '(lambda _
     ;; By default, running the test suite would fail because 'ssh' could not
     ;; be found in $PATH.  Define this variable to placate Open MPI without
     ;; adding a dependency on OpenSSH (the agent isn't used anyway.)
     (setenv "OMPI_MCA_plm_rsh_agent" (which "false"))
     ;; Allow oversubscription in case there are less physical cores available
     ;; in the build environment than the package wants while testing.
     (setenv "OMPI_MCA_rmaps_base_mapping_policy" "core:OVERSUBSCRIBE")

     ;; UCX sometimes outputs uninteresting warnings such as:
     ;;
     ;;   mpool.c:38   UCX  WARN  object 0x7ffff44fffc0 was not returned to mpool ucp_am_bufs
     ;;
     ;; These in turn leads to failures of test suites that capture and
     ;; compare stdout, such as that of 'hdf5-parallel-openmpi'.  Thus, tell
     ;; UCX to not emit those warnings.
     (setenv "UCX_LOG_LEVEL" "error")

     ;; Starting from 2.9.0, hwloc fails when /sys is unavailable:
     ;;
     ;;  [hwloc/linux] failed to find sysfs cpu topology directory, aborting linux discovery.
     ;;
     ;; This in turn breaks Open MPI users.  To work around it, define a fake
     ;; topology with 4 cores.  That silently disables CPU binding, though
     ;; 'get_cpubind' will report there's no binding.
     (setenv "HWLOC_SYNTHETIC" "4")
     #t))

(define-public python-mpi4py
  (package
    (name "python-mpi4py")
    (version "3.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mpi4py" version))
       (sha256
        (base32 "101lz7bnm9l17nrkbg6497kxscyh53aah7qd2b820ck2php8z18p"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'build 'mpi-setup
           ,%openmpi-setup)
         (add-before 'check 'pre-check
           (lambda _
             ;; Skip BaseTestSpawn class (causes error 'ompi_dpm_dyn_init()
             ;; failed --> Returned "Unreachable"' in chroot environment).
             (substitute* "test/test_spawn.py"
               (("unittest.skipMPI\\('openmpi\\(<3.0.0\\)'\\)")
                "unittest.skipMPI('openmpi')"))
             #t)))))
    (inputs
     (list openmpi))
    (properties
     '((updater-extra-inputs . ("openmpi"))))
    (home-page "https://github.com/mpi4py/mpi4py")
    (synopsis "Python bindings for the Message Passing Interface standard")
    (description "MPI for Python (mpi4py) provides bindings of the Message
Passing Interface (MPI) standard for the Python programming language, allowing
any Python program to exploit multiple processors.

mpi4py is constructed on top of the MPI-1/MPI-2 specification and provides an
object oriented interface which closely follows MPI-2 C++ bindings.  It
supports point-to-point and collective communications of any picklable Python
object as well as optimized communications of Python objects (such as NumPy
arrays) that expose a buffer interface.")
    (license license:bsd-3)))

(define-public mpich
  (package
    (name "mpich")
    (version "4.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.mpich.org/static/downloads/"
                                  version "/mpich-" version ".tar.gz"))
              (sha256
               (base32
                "0qyc3c99dzx88y8gzadh4inmmvxms9r61xskrjwwm0xdhhli612y"))))
    (build-system gnu-build-system)
    (inputs
     `(,zlib
       (,hwloc-2 "lib")
       ,slurm
       ,@(if (and (not (%current-target-system))
                  (member (%current-system) (package-supported-systems ucx)))
             (list ucx)
             '())))
    (native-inputs
     (list perl which gfortran python-minimal))
    (outputs '("out" "debug"))
    (arguments
     `(#:configure-flags
       (list "--disable-silent-rules"             ;let's see what's happening
             "--enable-debuginfo"

             (string-append "--with-hwloc-prefix="
                            (assoc-ref %build-inputs "hwloc"))

             ,@(if (assoc "ucx" (package-inputs this-package))
                   `((string-append "--with-ucx="
                                    (assoc-ref %build-inputs "ucx"))

                     ;; Default to using ucx when built with ucx as an input.
                     "--with-device=ch4:ucx")
                   '(;; Fallback to ch4 with autodetect at configure time.
                     "--with-device=ch4")))

       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-sources
                    (lambda _
                      (substitute* "./maint/gen_subcfg_m4"
                        (("/usr/bin/env") (which "env")))
                      (substitute* "src/glue/romio/all_romio_symbols"
                        (("/usr/bin/env") (which "env")))
                      (substitute* (find-files "." "buildiface")
                        (("/usr/bin/env") (which "env")))
                      (substitute* "maint/extracterrmsgs"
                        (("/usr/bin/env") (which "env")))
                      (substitute* (find-files "." "f77tof90")
                        (("/usr/bin/env") (which "env")))
                      (substitute* (find-files "." "\\.sh$")
                        (("/bin/sh") (which "sh")))))
                  ;; Move 'check after 'install.  Some tests try to call
                  ;; #$output/bin/mpicc.
                  (delete 'check)
                  (add-after 'install 'post-install-check
                    (assoc-ref %standard-phases 'check))
                  (add-before 'configure 'fix-makefile
                    (lambda _
                      ;; Remove "@hwloclib@" from 'pmpi_convenience_libs'.
                      ;; This fixes "No rule to make target '-lhwloc', needed
                      ;; by 'lib/libmpi.la'".
                      (substitute* "Makefile.in"
                        (("^pmpi_convenience_libs = (.*) @hwloclib@ (.*)$" _
                          before after)
                         (string-append "pmpi_convenience_libs = "
                                        before " " after)))))
                  (add-before 'configure 'define-gfortran-wrapper
                    (lambda* (#:key inputs #:allow-other-keys)
                      ;; 'configure' checks whether the Fortran compiler
                      ;; allows argument type mismatch.  Since gfortran >= 10
                      ;; does not, provide a wrapper that passes
                      ;; '-fallow-argument-mismatch' to get the desired
                      ;; behavior.
                      (mkdir-p ".gfortran-wrapper/bin")
                      (call-with-output-file ".gfortran-wrapper/bin/gfortran"
                        (lambda (port)
                          (display (string-append "#!" (which "sh") "\n")
                                   port)
                          (display
                           (string-append "exec \"" (which "gfortran")
                                          "\" -fallow-argument-mismatch"
                                          " \"$@\"\n")
                           port)
                          (chmod port #o755)))

                      (setenv "PATH"
                              (string-append (getcwd) "/"
                                             ".gfortran-wrapper/bin:"
                                             (getenv "PATH"))))))))
    (home-page "https://www.mpich.org/")
    (synopsis "Implementation of the Message Passing Interface (MPI)")
    (description
     "MPICH is a high-performance and portable implementation of the Message
Passing Interface (MPI) standard (MPI-1, MPI-2 and MPI-3).  MPICH provides an
MPI implementation that efficiently supports different computation and
communication platforms including commodity clusters, high-speed networks (10
Gigabit Ethernet, InfiniBand, Myrinet, Quadrics), and proprietary high-end
computing systems (Blue Gene, Cray).  It enables research in MPI through a
modular framework for other derived implementations.")
    (license license:bsd-2)))

(define-public mpich-ofi
  (package/inherit mpich
    (name "mpich-ofi")
    (inputs (modify-inputs (package-inputs mpich)
              (delete ucx)
              (append libfabric)
              (append rdma-core)
              (append psm2)))
    (arguments
      (substitute-keyword-arguments (package-arguments mpich)
        ((#:configure-flags flags)
         #~(list "--disable-silent-rules" ;let's see what's happening
                 "--enable-debuginfo"
                 "--with-device=ch4:ofi"

                 (string-append "--with-hwloc-prefix="
                                #$(this-package-input "hwloc"))

                 (string-append "--with-libfabric="
                                #$(this-package-input "libfabric"))))
        ((#:phases phases
          '%standard-phases)
         phases)))
    (synopsis "Implementation of the Message Passing Interface (MPI) for OmniPath")))

(define (make-scorep mpi)
  (package
    (name (string-append "scorep-" (package-name mpi)))
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.vi-hps.org/upload/packages/scorep/scorep-"
                    version ".tar.gz"))
              (sha256
               (base32
                "0h45357djna4dn9jyxx0n36fhhms3jrf22988m9agz1aw2jfivs9"))
              (modules '((guix build utils)))
              (snippet
               ;; Remove bundled software.
               '(begin
                  (for-each delete-file-recursively
                            '("vendor/opari2" "vendor/cube"))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     `(("mpi" ,mpi)
       ("papi" ,papi)
       ("opari2" ,opari2)
       ("libunwind" ,libunwind)
       ("otf2" ,otf2)
       ("cubelib" ,cube "lib")                    ;for lib, include
       ("openmpi" ,openmpi)
       ("zlib" ,zlib)))
    (native-inputs
     (list gfortran
           flex
           cube ;for cube-config
           bison
           python
           doxygen
           which))
    (arguments
     `(#:configure-flags
       (list "--enable-shared" "--disable-static"
             (string-append "--with-opari2="
                            (assoc-ref %build-inputs "opari2"))
             (string-append "--with-cube="
                            (assoc-ref %build-inputs "cube")))
       #:parallel-tests? #f
       #:make-flags '("V=1")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'licence
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((doc (string-append (assoc-ref outputs "out")
                                       "/share/doc/scorep")))
               (install-file "COPYING" doc)
               #t))))))
    (home-page "https://www.vi-hps.org/projects/score-p/")
    (synopsis "Performance measurement infrastructure for parallel code")
    (description
     "The Score-P (Scalable Performance Measurement Infrastructure for
Parallel Codes) measurement infrastructure is a scalable and easy-to-use tool
suite for profiling, event trace recording, and online analysis of
high-performance computing (HPC) applications.")
    (license license:cpl1.0)))

(define-public scorep-openmpi (make-scorep openmpi))
