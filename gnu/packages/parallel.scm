;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015-2018, 2020-2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Pjotr Prins <pjotr.guix@thebird.nl>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016, 2020, 2021, 2022, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Ben Woodcroft <donttrustben@gmail.com>
;;; Copyright © 2017, 2018 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Roel Janssen <roel@gnu.org>
;;; Copyright © 2021 Stefan Reichör <stefan@xsteve.at>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024, 2025 David Elsing <david.elsing@posteo.net>
;;; Copyright © 2024 Romain Garbage <romain.garbage@inria.fr>
;;; Copyright © 2024 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages parallel)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module ((guix utils) #:select (target-64bit?))
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freeipmi)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public parallel
  (package
    (name "parallel")
    (version "20250422")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/parallel/parallel-"
                          version ".tar.bz2"))
      (sha256
       (base32 "1pa8yapvkd3267rhadhs4ingi8g11vf3yh1ssv5yv1zdzfvsgw0h"))
      (snippet
       '(begin
          (use-modules (guix build utils))
          ;; Delete pre-generated manpages and documents.
          ;; TODO: generate rst files.
          ;; parallel_cheat_bw.pdf uses libreoffice to be generated.
          (rename-file "src/parallel_cheat_bw.pdf"
                       "src/parallel_cheat_bw.pdf-keep")
          (for-each delete-file (find-files "src" "\\.(1|7|html|pdf)$"))
          (rename-file "src/parallel_cheat_bw.pdf-keep"
                       "src/parallel_cheat_bw.pdf")))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-bin-sh
           (lambda _
             (for-each
              (lambda (file)
                (substitute* file
                  ;; Patch hard coded '/bin/sh' in the line ending in:
                  ;; $Global::shell = $ENV{'PARALLEL_SHELL'} ||
                  ;;  parent_shell($$) || $ENV{'SHELL'} || "/bin/sh";
                  (("/bin/sh\\\";\n$") (string-append (which "sh") "\";\n"))))
              (list "src/parallel" "src/sem"))))
         (add-before 'install 'add-install-to-path
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PATH" (string-append (getenv "PATH") ":"
                                           (assoc-ref outputs "out") "/bin"))))
         (add-after 'install 'wrap-program
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (wrap-program (string-append out "/bin/parallel")
                 `("PATH" ":" prefix
                   ,(map (lambda (input)
                           (string-append (assoc-ref inputs input) "/bin"))
                         '("perl"
                           "procps")))))))
         (add-after 'wrap-program 'post-install-test
           (lambda* (#:key tests? outputs #:allow-other-keys)
             (when tests?
               (invoke (string-append
                        (assoc-ref outputs "out") "/bin/parallel")
                       "echo"
                       ":::" "1" "2" "3")))))))
    (native-inputs
     (list perl pod2pdf))
    (inputs
     (list bash-minimal perl procps))
    (home-page "https://www.gnu.org/software/parallel/")
    (synopsis "Build and execute command lines in parallel")
    (description
     "GNU Parallel is a tool for executing shell jobs in parallel using one
or more computers.  Jobs can consist of single commands or of scripts
and they are executed on lists of files, hosts, users or other items.")
    (license license:gpl3+)))

(define-public xe
  (package
    (name "xe")
    (version "1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/leahneukirchen/xe")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ijvf7q5pxk4rlj7p9q6fmpdqiwmc28gffkk6yg390k1a1z3msf9"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f
           #:make-flags #~(list (string-append "CC=" #$(cc-for-target))
                                (string-append "PREFIX=" #$output))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure))))
    (synopsis "Execute a command for every argument")
    (description
     "The xe utility constructs command lines from specified arguments,
combining some of the best features of xargs(1) and apply(1).  Parallel
execution is also possible.")
    (home-page "https://github.com/leahneukirchen/xe")
    (license license:public-domain)))

(define-public xjobs
  (package
    (name "xjobs")
    (version "20200726")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.maier-komor.de/xjobs/xjobs-"
                    version ".tgz"))
              (sha256
               (base32
                "0ay6gn43pnm7r1jamwgpycl67bjg5n87ncl27jb01w2x6x70z0i3"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ;; No tests
    (native-inputs
     (list flex which))
    (home-page "https://www.maier-komor.de/xjobs.html")
    (properties `((release-monitoring-url . ,home-page)))
    (synopsis
     "Parallel execution of jobs with several useful options")
    (description
     "xjobs reads job descriptions line by line and executes them in
parallel.  It limits the number of parallel executing jobs and starts new jobs
when jobs finish.")
    (license license:gpl2+)))

(define-public slurm-minimal
  (package
    (name "slurm-minimal")
    (version "23.11.10")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (sha256
               (base32
                "0gf7x85bzpkrx87mb16wyiyvkjxqq01sbajsjxwrspyi2v675hgr"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; According to
                  ;; <https://lists.gnu.org/archive/html/guix-devel/2016-02/msg00534.html>
                  ;; there are non-free bits under contribs/, though it's not
                  ;; clear which ones.  libpmi is clearly free (it used to be
                  ;; under src/api/) and so is pmi2 (lax non-copyleft
                  ;; license), so remove all of contribs/ except pmi and pmi2.
                  (substitute* "configure.ac"
                    (("^[[:space:]]+contribs/(.*)$" all directory)
                     (if (string-prefix? "pmi" directory)
                         all
                         "")))

                  (rename-file "contribs/pmi" "tmp-pmi")
                  (rename-file "contribs/pmi2" "tmp-pmi2")
                  (delete-file-recursively "contribs")
                  (mkdir "contribs")
                  (rename-file "tmp-pmi" "contribs/pmi")
                  (rename-file "tmp-pmi2" "contribs/pmi2")))))
    (inputs
     (append
       (list freeipmi
             `(,hwloc-2 "lib")
             json-c
             linux-pam)
       (if (supported-package? openpmix)
           (list openpmix)
           '())
       (list munge
             numactl
             readline)))
    (native-inputs
     (list autoconf expect perl pkg-config python-wrapper))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--enable-pam" "--sysconfdir=/etc/slurm"
                   "--disable-static"
                   (string-append "--with-freeipmi=" #$(this-package-input "freeipmi"))
                   (string-append "--with-hwloc="
                                  (ungexp (this-package-input "hwloc") "lib"))
                   (string-append "--with-json=" #$(this-package-input "json-c"))
                   (string-append "--with-munge=" #$(this-package-input "munge"))

                   ;; Use PMIx bundled with Open MPI (this is required for Open MPI 5.x).
                   ;; Note: Older versions that inherit from this package lack the
                   ;; 'openpmix' dependency.
                   #$(let ((openmpix (this-package-input "openpmix")))
                       (if openmpix
                           #~(string-append "--with-pmix=" #$openmpix)
                           "--without-pmix"))

                   ;; 32-bit support is marked as deprecated and needs to be
                   ;; explicitly enabled.
                   #$@(if (target-64bit?) '() '("--enable-deprecated")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'patch-plugin-linker-flags
                 (lambda _
                   (substitute* (find-files "src/plugins/" "Makefile.in")
                     (("_la_LDFLAGS = ")
                      "_la_LDFLAGS = ../../../api/libslurm.la "))))
               (add-after 'patch-plugin-linker-flags 'autoconf
                 (lambda _ (invoke "autoconf")))  ;configure.ac was patched
               (add-after 'install 'install-libpmi
                 (lambda _
                   ;; Open MPI expects libpmi to be provided by Slurm so install it.
                   (invoke "make" "install" "-C" "contribs/pmi")

                   ;; Others expect pmi2.
                   (invoke "make" "install" "-C" "contribs/pmi2"))))))
    (home-page "https://slurm.schedmd.com/")
    (synopsis "Workload manager for cluster computing")
    (description
     "SLURM is a fault-tolerant and highly scalable cluster management and job
scheduling system for large and small clusters.  It allocates access to
resources (computer nodes) to users for some duration of time, provides a
framework for starting, executing, and monitoring work (typically a parallel
job) on a set of allocated nodes, and arbitrates contention for resources
by managing a queue of pending work.")
    (license (list license:bsd-2      ; src/common/log.[ch], src/common/uthash
                   license:expat      ; slurm/pmi.h
                   license:isc        ; src/common/strlcpy.c
                   license:lgpl2.1+   ; hilbert.[ch], src/common/slurm_time.h
                   license:zlib       ; src/common/strnatcmp.c
                   (license:non-copyleft    ;contribs/pmi2, Argonne Natl. Lab.
                    "https://github.com/SchedMD/slurm/blob/master/contribs/pmi2/COPYRIGHT")
                   license:gpl2+))))   ; the rest, often with OpenSSL exception

(define (make-slurm base-slurm)
  "Make a slurm package with all optional features enabled. Base it off of the
minimal slurm package BASE-SLURM."
  (package
    (inherit base-slurm)
    (name "slurm")
    (arguments
     (substitute-keyword-arguments (package-arguments base-slurm)
       ((#:configure-flags flags #~'())
        #~(cons* "--enable-slurmrestd"
                 (string-append "--with-bpf="
                                (dirname
                                 (dirname (search-input-directory
                                           %build-inputs "include/linux"))))
                 (string-append "--with-http-parser="
                                #$(this-package-input "http-parser"))
                 (string-append "--with-rdkafka="
                                #$(this-package-input "librdkafka"))
                 (string-append "--with-yaml="
                                #$(this-package-input "libyaml"))
                 #$flags))))
    ;; FIXME: More optional inputs could be added.
    (inputs
     (modify-inputs (package-inputs base-slurm)
       ;; Add dependencies required by the slurm REST API.
       (prepend dbus freeipmi http-parser
                libjwt librdkafka libyaml (list mariadb "dev"))))))

(define-public slurm (make-slurm slurm-minimal))

;; The SLURM client/daemon protocol and file format changes from time to time
;; in incompatible ways, as noted in
;; <https://slurm.schedmd.com/troubleshoot.html#network>.  Thus, keep older
;; releases here.  See also <https://issues.guix.gnu.org/44387>.
;; As noted in the link, YY.MM is the release scheme, and the 'maintenance'
;; digit does not introduce incompatibilities.

(define-public slurm-minimal-23.02
  (package
   (inherit slurm-minimal)
   (version "23.02.6")
    (source (origin
             (inherit (package-source slurm))
             (method url-fetch)
             (uri (string-append
                   "https://download.schedmd.com/slurm/slurm-"
                   version ".tar.bz2"))
             (patches
              (search-patches "slurm-23-salloc-fallback-shell.patch"))
             (sha256
              (base32
               "08rz3r1rlnb3pmfdnbh542gm44ja0fdy8rkj4vm4lclc48cvqp2a"))))))

(define-public slurm-23.02 (make-slurm slurm-minimal-23.02))

(define-public slurm-minimal-22.05
  (package
    (inherit slurm-minimal-23.02)
    (version "22.05.1")
    (source (origin
              (inherit (package-source slurm-minimal-23.02))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (patches '())                       ;drop 'salloc' patch
              (sha256
               (base32
                "0f3hhlki8g7slllsnyj1qikbsvr62i0hig85lcdcfnmsagzlhbyi"))))))

(define-public slurm-22.05 (make-slurm slurm-minimal-22.05))

(define-public slurm-minimal-21.08
  (package
    (inherit slurm-minimal-22.05)
    (version "21.08.8")
    (source (origin
              (inherit (package-source slurm-minimal-22.05))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (patches '())                       ;drop 'salloc' patch
              (sha256
               (base32
                "1sjln54idc9rhg8f2nvm38sgs6fncncyzslas8ixy65pqz2hphbf"))))

    ;; This and older versions of slurm have PMIx support but they seem to
    ;; require an older version of openpmix.  Disable PMIx support.
    (inputs (modify-inputs (package-inputs slurm-minimal-22.05)
              (delete "openpmix")))))

(define-public slurm-21.08 (make-slurm slurm-minimal-21.08))

(define-public slurm-minimal-20.11
  (package
    (inherit slurm-minimal-21.08)
    (version "20.11.9")
    (source (origin
              (inherit (package-source slurm-minimal-21.08))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (patches '())                       ;drop 'salloc' patch
              (sha256
               (base32
                "0xq2d6dm285y541dyg1h66z7svsisrq8c81ag0f601xz1cn3mq9m"))))))

(define-public slurm-20.11 (make-slurm slurm-minimal-20.11))

(define-public slurm-minimal-20.02
  (package
    (inherit slurm-minimal-20.11)
    (version "20.02.6-1")
    (source (origin
              (inherit (package-source slurm-minimal-20.11))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (patches '())                       ;drop 'salloc' patch
              (sha256
               (base32
                "0qj4blfymrd2ry2qmb58l3jbr4jwygc3adcfw7my27rippcijlyc"))))
    (arguments
     (substitute-keyword-arguments (package-arguments slurm-minimal-20.11)
       ((#:configure-flags flags ''())
        #~(append '("CFLAGS=-O2 -g -fcommon" "LDFLAGS=-fcommon")
                  #$flags))))))

(define-public slurm-20.02 (make-slurm slurm-minimal-20.02))

(define-public slurm-minimal-19.05
  (package
    (inherit slurm-minimal-20.02)
    (version "19.05.8")
    (source (origin
              (inherit (package-source slurm-minimal-20.02))
              (method url-fetch)
              (uri (string-append
                    "https://download.schedmd.com/slurm/slurm-"
                    version ".tar.bz2"))
              (patches '())                       ;drop 'salloc' patch
              (sha256
               (base32
                "10c9j4a9a6d4ibpf75006mn03p8xgpaprc247x2idakysjf2fw43"))))))

(define-public slurm-19.05 (make-slurm slurm-minimal-19.05))

;; Same as Debian 10
(define-public slurm-minimal-18.08
  (package
    (inherit slurm-minimal-19.05)
    (version "18.08.9")
    (source
      (origin
        (inherit (package-source slurm-minimal-20.02))
        (uri (string-append
               "https://download.schedmd.com/slurm/slurm-"
               version ".tar.bz2"))
        (patches '())                             ;drop 'salloc' patch
        (sha256
         (base32
          "1bgrpz75m7l4xhirsd0fvnkzlkrl8v2qpmjcz60barc5qm2kn457"))))))

(define-public slurm-18.08 (make-slurm slurm-minimal-18.08))

(define-public slurm-drmaa
  (package
    (name "slurm-drmaa")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/natefoo/slurm-drmaa/releases/download/"
                    version "/slurm-drmaa-" version ".tar.gz"))
              (sha256
               (base32
                "1fn3p4wjj0sgvx0isy3hiwi35vhxa2n2ksq5cn9sq2hg7yyb2phl"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f)) ; The tests require "bats".
    (inputs
     (list slurm))
    (native-inputs
     (list which))
    (home-page "https://github.com/natefoo/slurm-drmaa")
    (synopsis "Distributed resource management application API for SLURM")
    (description
     "PSNC DRMAA for Simple Linux Utility for Resource Management (SLURM) is
an implementation of Open Grid Forum DRMAA 1.0 (Distributed Resource
Management Application API) specification for submission and control of jobs
to SLURM.  Using DRMAA, grid applications builders, portal developers and ISVs
can use the same high-level API to link their software with different
cluster/resource management systems.")
    (license license:gpl3+)))

(define-public python-schwimmbad
  (package
    (name "python-schwimmbad")
    (version "0.4.2")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "schwimmbad" version))
       (sha256
        (base32 "1aac1rswb0r0vzbxvjj2jyx5j0vqyjj7mygc71n9zbkpmr8m1rpg"))))
    (build-system pyproject-build-system)
    (propagated-inputs
     (list python-dill
           python-joblib
           python-mpi4py
           python-multiprocess))
    (native-inputs
     (list python-hatch-vcs python-hatchling python-pytest))
    (home-page "https://github.com/adrn/schwimmbad")
    (synopsis "Common interface for parallel processing pools")
    (description
     "@code{schwimmbad} provides a uniform interface to parallel processing
pools and enables switching easily between local development (e.g., serial
processing or with @code{multiprocessing}) and deployment on a cluster or
supercomputer (via, e.g., MPI or JobLib).")
    (license license:expat)))

(define-public python-slurm-magic
  (let ((commit "73dd1a2b85799f7dae4b3f1cd9027536eff0c4d7")
        (revision "0"))
    (package
      (name "python-slurm-magic")
      (version (git-version "0.0" revision commit))
      (home-page "https://github.com/NERSC/slurm-magic")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page)
                                    (commit commit)))
                (sha256
                 (base32
                  "19pp2vs0wm8mx0arz9n6lw9wgyv70w9wyi4y6b91qc5j3bz5igfs"))
                (file-name (git-file-name name version))))
      (build-system python-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-before 'build 'set-slurm-path
                      (lambda* (#:key inputs #:allow-other-keys)
                        ;; The '_execute' method tries to exec 'salloc'
                        ;; etc. from $PATH.  Record the absolute file name
                        ;; instead.
                        (let ((slurm (assoc-ref inputs "slurm")))
                          (substitute* "slurm_magic.py"
                            (("name = (.*)$" _ value)
                             (string-append "name = \""
                                            slurm "/bin/\" + "
                                            value "\n")))
                          #t))))))
      (inputs
       (list slurm))
      (propagated-inputs
       (list python-ipython python-pandas))
      (synopsis "Control the SLURM batch scheduler from Jupyter Notebook")
      (description
       "This package implements Jupyter/IPython
@uref{http://ipython.readthedocs.io/en/stable/interactive/magics.html, magic
commands} for interacting with the SLURM workload manager.  SLURM magic simply
wraps command-line executables and the commands themselves should look like
their command-line counterparts.  Commands are spawned via @code{subprocess}
and output captured in the notebook.  Whatever arguments are accepted by a
SLURM command line executable are also accepted by the corresponding magic
command---e.g., @code{%salloc}, @code{%sbatch}, etc.")
      (license license:bsd-3))))

(define-public pthreadpool
  ;; This repository has only one tag, 0.1, which is older than what users
  ;; such as XNNPACK expect.
  (let ((commit "560c60d342a76076f0557a3946924c6478470044")
        (version "0.1")
        (revision "3"))
    (package
      (name "pthreadpool")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/pthreadpool")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0l82ix1h8pmjikf15bvmlap355gmyfjpahmgz4lbd6g40ab3ls5c"))
                (patches (search-patches "pthreadpool-system-libraries.patch"))))
      (build-system cmake-build-system)
      (arguments '(#:configure-flags '("-DBUILD_SHARED_LIBS=ON")))
      (inputs
       (list googletest googlebenchmark fxdiv))
      (synopsis "Efficient thread pool implementation")
      (description
       "The pthreadpool library implements an efficient and portable thread
pool, similar to those implemented by OpenMP run-time support libraries for
constructs such as @code{#pragma omp parallel for}, with additional
features.")
      (license license:bsd-2))))

(define-public cpuinfo
  ;; There's currently no tag on this repo.
  (let ((version "0.0")
        (revision "5")
        (commit "b73ae6ce38d5dd0b7fe46dbe0a4b5f4bab91c7ea"))
    (package
      (name "cpuinfo")
      (version (git-version version revision commit))
      (home-page "https://github.com/pytorch/cpuinfo")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0ibba4jssvjyd147dyj5lzijgxhmjxf0ishl1wykka1rblmxmli4"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; cpuinfo does not work in the build container for aarch64:
        ;; https://github.com/pytorch/cpuinfo/issues/143
        #:tests? (not (or (target-aarch64?)
                          (target-riscv64?)))
        #:configure-flags
        '(list "-DBUILD_SHARED_LIBS=ON"
               "-DUSE_SYSTEM_LIBS=ON")
        #:phases
        '(modify-phases %standard-phases
           (add-after 'unpack 'skip-bad-test
             (lambda _
               (substitute* "test/init.cc"
                 (("TEST\\(CORE, known_uarch\\) \\{" m)
                  (string-append m "\
GTEST_SKIP() << \"See https://github.com/pytorch/cpuinfo/issues/132\";"))))))))
      (inputs
       (list googletest googlebenchmark))
      (synopsis "C/C++ library to obtain information about the CPU")
      (description
       "The cpuinfo library provides a C/C++ and a command-line interface to
obtain information about the CPU being used: supported instruction set,
processor name, cache information, and topology information.")
      (license license:bsd-2))))

(define-public clog
  (package
    (inherit cpuinfo) ;distributed with cpuinfo but not built by it
    (name "clog")
    (source (origin
              (inherit (package-source cpuinfo))
              (patches (search-patches "clog-fix-shared-build.patch"))))
    (arguments
     (list
      #:configure-flags
      ''("-DBUILD_SHARED_LIBS=ON"
         "-DUSE_SYSTEM_LIBS=ON")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'chdir
                     (lambda _
                       (chdir "deps/clog"))))))
    (native-inputs (list googletest))
    (inputs '())
    (synopsis "C-style logging library based on printf")
    (description
     "This package provides a C-style library for logging errors,
warnings, information notes, and debug information.  Its features are:
@itemize
@item printf-style interface for formatting variadic parameters.
@item Separate functions for logging errors, warnings, information notes, and
debug information.
@item Independent logging settings for different modules.
@item Logging to logcat on Android and stderr/stdout on other platforms.
@item Compatible with C99 and C++.
@item Covered with unit tests.
@end itemize")))

(define-public psimd
  ;; There is currently no tag in this repo.
  (let ((commit "072586a71b55b7f8c584153d223e95687148a900")
        (version "0.0")
        (revision "1"))
    (package
      (name "psimd")
      (version (git-version version revision commit))
      (home-page "https://github.com/Maratyszcza/Psimd")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "16mslhvqs0gpqbg7kkq566a8gkn58cgjpqca8ljj9qcv5mk9apwm"))))
      (build-system cmake-build-system)
      (arguments '(#:tests? #f))                  ;there are no tests
      (synopsis "Portable 128-bit SIMD intrinsics")
      (description
       "This header-only C++ library provides a portable interface to
single-instruction multiple-data (SIMD) intrinsics.")
      (license license:expat))))

(define-public openpmix
  (package
    (name "openpmix")
    (version "4.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/openpmix/openpmix/releases/download/v"
                    version "/pmix-" version ".tar.bz2"))
              (sha256
               (base32
                "1j9xlhqrrmgjdkwakamn78y5gj756adi53hn25zksgr3is3l5d09"))
              (snippet
               '(begin (use-modules (guix build utils))
                       ;; Remove ~5 MiB of pre-built HTML doc.
                       (delete-file-recursively "docs/_build/html")))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-hwloc="
                             (ungexp (this-package-input "hwloc") "lib"))
              "--enable-python-bindings") ;disabled by default

      ;; Don't keep a reference to GCC.
      #:disallowed-references (and (not (%current-target-system))
                                   (list (canonical-package gcc)))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'set-LDFLAGS
            (lambda _
              ;; The Cython-compiled shared library would fail the
              ;; validate-runpath phase otherwise.
              (setenv "LDFLAGS"
                      (string-append "-Wl,-rpath=" #$output "/lib"))))
          (add-before 'configure 'strip-pmix-cc-absolute
            (lambda _
              ;; The 'pmix_info' program prints the 'configure' command line,
              ;; compiler absolute file name, etc., which causes it to keep
              ;; references to many build-time packages.  Scrub these.
              (substitute* "configure"
                (("PMIX_CC_ABSOLUTE=\"(.*)\"" _ cc)
                 (string-append "PMIX_CC_ABSOLUTE=\"$(basename \""
                                cc "\")\"\n")))))
          (add-after 'configure 'strip-pmix-config-header
            (lambda _
              (substitute* "src/include/pmix_config.h"
                (("#define PMIX_CONFIGURE_CLI .*")
                 "#define PMIX_CONFIGURE_CLI \"[scrubbed]\"\n")))))))
    (inputs (list libevent `(,hwloc "lib") zlib))
    (native-inputs (list perl python python-cython))
    (synopsis "PMIx library")
    (description
     "PMIx is an application programming interface standard that provides
libraries and programming models with portable and well-defined access to
commonly needed services in distributed and parallel computing systems.")
    (home-page "https://pmix.org/")
    ;; configure: WARNING: PMIx does not support 32 bit builds.
    (supported-systems %64bit-supported-systems)
    ;; The provided license is kind of BSD-style but specific.
    (license (license:fsf-free "https://github.com/openpmix/openpmix?tab=License-1-ov-file#License-1-ov-file"))))

(define-public prrte
  (package
   (name "prrte")
   (version "3.0.6")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://github.com/openpmix/prrte/releases/download/v"
                  version "/prrte-" version ".tar.bz2"))
            (sha256
             (base32
              "0wiy0vk37v4db1jgxza8bci0cczcvj34dalzsrlz05dk45zb7dl3"))))
   (build-system gnu-build-system)
   (arguments
    (list #:configure-flags
          #~(list (string-append "--with-hwloc="
                                 (assoc-ref %build-inputs "hwloc"))
                  (string-append "--with-pmix="
                                 #$(this-package-input "openpmix")))

          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'remove-absolute-references
                (lambda _
                  ;; Remove references to GCC, the shell, etc. (shown by
                  ;; 'prte_info') to reduce the closure size.
                  (substitute* "src/tools/prte_info/param.c"
                    (("_ABSOLUTE")
                     "")
                    (("PRTE_CONFIGURE_CLI")
                     "\"[elided to reduce closure]\""))))
              (add-after 'unpack 'patch-prted-reference
                (lambda _
                  ;; Record the absolute file name of 'prted' instead of
                  ;; assuming it will be found in $PATH at run time.
                  (substitute* "src/runtime/prte_mca_params.c"
                    (("prte_launch_agent =.*")
                     (string-append "prte_launch_agent = \""
                                    #$output "/bin/prted\";\n"))))))

          #:disallowed-references (list (canonical-package gcc))))
   (inputs (list libevent
                 `(,hwloc "lib")
                 openpmix))
   (native-inputs (list perl))
   (synopsis "PMIx Reference RunTime Environment (PRRTE)")
   (description
    "The PMIx Reference RunTime Environment is a runtime environment
containing the reference implementation and capable of operating
within a host SMS. The reference RTE therefore provides an easy way of
exploring PMIx capabilities and testing PMIx-based applications
outside of a PMIx-enabled environment.")
   (home-page "https://openpmix.github.io/")
   ;; The provided license is kind of BSD-style but specific.
   (license (license:fsf-free "https://github.com/openpmix/prrte?tab=License-1-ov-file#License-1-ov-file"))))
