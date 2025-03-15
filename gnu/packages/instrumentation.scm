;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Olivier Dion <olivier.dion@polymtl.ca>
;;; Copyright © 2023 Andy Tai <atai@atai.org>
;;; Copyright © 2023 Marius Bakke <marius@gnu.org>
;;; Copyright © 2024 Nguyễn Gia Phong <mcsinyx@disroot.org>
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

(define-module (gnu packages instrumentation)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpio)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages engineering)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages libunwind)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages man)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xml)
  #:use-module (guix platform)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (srfi srfi-26)
  #:use-module (guix utils))

(define-public babeltrace
  (package
    (name "babeltrace")
    (version "2.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.efficios.com/files/babeltrace/babeltrace2-"
                                  version ".tar.bz2"))
              (sha256
               (base32 "1d7jxljbfb4y8jmxm7744ndhh9k9rw8qhmnljb19wz7flzr9x3vv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f  ; FIXME - When Python's bindings are enabled, tests do not
                    ; pass.
       #:make-flags
       ,#~(list
           (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
           "DISTSETUPOPTS=--single-version-externally-managed") ;no .egg files
       #:configure-flags
       '("--enable-debug-info"
         "--enable-man-pages"
         "--enable-python-bindings"
         "--enable-python-plugins")
       #:phases
       (modify-phases %standard-phases
         ;; These are recommended in the project's README for a development
         ;; build configuration.
         (add-after 'unpack 'reconfigure
           (lambda _
             (delete-file "configure"))))))
    (inputs
     (list glib))
    ;; NOTE - elfutils is used for the LTTng debug information filter
    ;; component class.  This can be moved to `native-inputs` if
    ;; `--enable-debug-info` is replaced by `--disable-debug-info` in
    ;; `#:configure-flags`.
    (propagated-inputs
     (list elfutils))
    ;; NOTE - python-3 is set here for generating the bindings.  Users need to
    ;; install python-3 in their profile in order to use these bindings.
    ;;
    ;; NOTE - Babeltrace 2.0.4 is distributed with a aclocal.m4 that does not
    ;; support Python3.10.  We can either disable Python's bindings or we can
    ;; reconfigure the project with our autoconf.  This is because the
    ;; distribution tarballs are generated on Ubuntu LTS 18.04.
    ;;
    ;; `paredit-raise-sexp' on the first list of native inputs and remove the
    ;; 'autoreconf invocation whenever we bump to the next version that is
    ;; goind to be generated on Ubuntu LTS 22.04.
    (native-inputs
     (append
      (list asciidoc
            bison
            flex
            pkg-config
            python-3
            python-sphinx
            swig
            xmltoman)
      (list autoconf
            automake
            libtool)))
    (home-page "https://babeltrace.org/")
    (synopsis "Trace manipulation toolkit")
    (description "Babeltrace 2 is a framework for viewing, converting,
transforming, and analyzing traces.  It is also the reference parser
implementation of the Common Trace Format (CTF), produced by tools such as
LTTng and barectf.  This package provides a library with a C API, Python 3
bindings, and the command-line tool @command{babeltrace2}.")
    (license license:expat)))

(define-public babeltrace/dev
  (package/inherit babeltrace
    ;; This dev variant of the package babeltrace is slower but allows better
    ;; development of plugins as recommended by the authors.
    (name "babeltrace-dev")
    (synopsis "Trace manipulation toolkit (variant for plugin developers)")
    (arguments
     (substitute-keyword-arguments (package-arguments babeltrace)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-before 'configure 'set-environment-variables
             (lambda _
               (setenv "BABELTRACE_DEV_MODE" "1")
               (setenv "BABELTRACE_MINIMAL_LOG_LEVEL" "TRACE")))))))))

(define-public barectf
  (package
    (name "barectf")
    (version "3.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/efficios/barectf")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0v7w830dqi46bq753x84d0z75dw4cf4r93gpfrv4sjynvmylbs95"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'relax-requirements
                 (lambda _
                   ;; Remove version constraints as the program appears
                   ;; to work (tests pass!) with later versions.
                   ;; Try removing these when updating barectf.
                   (substitute* "pyproject.toml"
                     (("pyyaml = '\\^5")
                      "pyyaml = '>=5")
                     (("jsonschema = '\\^3")
                      "jsonschema = '>=3"))))
               (add-before 'check 'set-CC
                 (lambda _
                   ;; Some tests invoke a compiler.
                   (setenv "CC" "gcc"))))))
    (native-inputs
     (list python-poetry-core python-pytest))
    (propagated-inputs
     (list python-jinja2 python-jsonschema python-pyyaml python-termcolor-1))
    (home-page "https://barectf.org")
    (synopsis "CTF tracer generator")
    (description
     "@command{barectf} is a generator of tracer which produces CTF data
streams.  The generated C source code has no other dependencies than a few C
standard library headers.")
    (license license:expat)))

(define-public dyninst
  (package
    (name "dyninst")
    (version "13.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dyninst/dyninst")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0vkd9z6zwvn13ynfys2fg5yanv7n9pl7x5z8m1lcnmnb0kwgi035"))))

    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f
       ;; STERILE_BUILD: Do not download/build third-party dependencies from
       ;; source.
       #:configure-flags
       (list "-DSTERILE_BUILD=ON")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'adjust-supported-platform-name
           (lambda _
             ;; That file checks for "i386" but
             ;; 'cmake_host_system_information' returns "i686" when targeting
             ;; i686-linux.  Adjust accordingly.
             (substitute* "cmake/DyninstPlatform.cmake"
               (("\"i386\"") "\"i686\"")))))))
    (propagated-inputs
     (list elfutils libiberty boost tbb))

    ;; Supported systems according to 'cmake/DyninstPlatform.cmake'.
    (supported-systems '("x86_64-linux" "i686-linux"
                         "aarch64-linux" "powerpc64le-linux"))

    (home-page "https://dyninst.org/")
    (synopsis "Dynamic instrumentation")
    (description "Dyninst is a collection of libraries for instrumenting,
analyzing and editing binaries.  It can attach to an existing program or
create a new one out of an ELF file for analysis or modification.  It come
with a handful of C++ libraries.")
    (license license:lgpl2.1+)))

(define-public flamegraph
  ;; No new version since 2019, but there's still some new important commits.
  (let ((commit "810687f180f3c4929b5d965f54817a5218c9d89b")
        (revision "1"))
    (package
      (name "flamegraph")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/brendangregg/FlameGraph")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1lg02mxzdsm9szn4vcmx76c1bw9gqmxqk8n6v63v03036sc83s22"))))
      (build-system copy-build-system)
      (arguments
       `(#:install-plan
         ',(map (cut list <> "bin/")
                '("flamegraph.pl"
                  "stackcollapse.pl"
                  "stackcollapse-perf.pl"
                  "stackcollapse-pmc.pl"
                  "stackcollapse-stap.pl"
                  "stackcollapse-instruments.pl"
                  "stackcollapse-vtune.pl"
                  "stackcollapse-jstack.pl"
                  "stackcollapse-gdb.pl"
                  "stackcollapse-go.pl"
                  "stackcollapse-vsprof.pl"
                  "stackcollapse-wcp.pl"))))
      (inputs (list perl))
      (home-page "http://www.brendangregg.com/flamegraphs.html")
      (synopsis "Stack trace visualizer")
      (description "Flamegraph is a collection of scripts that generate
interactive SVGs out of traces genated from various tracing tools.  It comes
with the script @command{flamegraph.pl} and many stackcollapse scripts.")
      (license license:cddl1.0))))

(define-public libpatch
  (package
    (name "libpatch")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~old/libpatch")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1sx1sichnnqfi84z37gd04h41vpr8i2vg6yg0jkqxlrv3dys489a"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        (string-append
         "--target="
         ,(platform-linux-architecture
           (lookup-platform-by-target-or-system
            (or
             (%current-target-system)
             (%current-system))))))))
    ;;; Add lttng-ust to the inputs if you want tracepoints within libpatch
    ;;; for debugging.
    (inputs
     (list capstone
           elfutils
           libunwind
           liburcu))
    (native-inputs
     (list coreutils
           ;; test-ftrace.scm
           (list coreutils "debug")
           (list guile-3.0 "debug")
           (list gnu-make "debug")

           ;; For eu-nm in test-ftrace.scm.
           (list elfutils "bin")

           guile-3.0
           gnu-make
           pkg-config
           ;; zlib is required by libdw.  This can be removed if zlib is put
           ;; as a propagated-input of elfutils.
           zlib))
    (synopsis "Dynamic binary patcher")
    (description
     "libpatch is a lightweight C library that can be used by tracers,
debuggers and other tools for insertion of probes in a program at runtime.  It
has many strategies to minimize probe overhead and maximize possible
coverage.")
    (home-page "https://git.sr.ht/~old/libpatch")
    (license (list license:lgpl2.1 license:expat license:gpl3+))
    ;; Libpatch only supports instrumentation for x86_64 right now.  Augment
    ;; that list in further version.
    (supported-systems (list "x86_64-linux"))))

(define-public lttng-modules
  (package
    (name "lttng-modules")
    (version "2.13.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-modules/"
                                  "lttng-modules-" version ".tar.bz2"))
              (sha256
               (base32
                "19jx1yz1hsbr4vwfp2nr6z52a5g50iimaak962k49diay5zryi66"))))
    (build-system linux-module-build-system)
    (arguments
     `(#:tests? #f ; no tests
       #:make-flags (list "CONFIG_LTTNG=m"
                          "CONFIG_LTTNG_CLOCK_PLUGIN_TEST=m")))
    (home-page "https://lttng.org/")
    (synopsis "LTTng kernel modules for the LTTng tracer toolset")
    (description
     "LTTng kernel modules are Linux kernel modules which make
LTTng kernel tracing possible.  They include essential control modules and
many probes which instrument numerous interesting parts of Linux.")
    (license (list license:lgpl2.1 license:gpl2 license:expat))))

(define-public lttng-ust
  (package
    (name "lttng-ust")
    (version "2.13.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-ust/"
                                  "lttng-ust-" version ".tar.bz2"))
              (sha256
               (base32
                "0dyr4j8f8y6fmfpxb3ajkc1ndwpl1baxzbyc4ksx8ym3p7d9ivyl"))))
    (build-system gnu-build-system)
    (inputs
     (list numactl))
    (propagated-inputs
     (list liburcu))          ;headers of liburcu are used by headers of lttng
    (native-inputs
     (list python-3 pkg-config))
    (home-page "https://lttng.org/")
    (synopsis "LTTng userspace tracer libraries")
    (description "The user space tracing library, liblttng-ust, is the LTTng
user space tracer.  It receives commands from a session daemon, for example to
enable and disable specific instrumentation points, and writes event records
to ring buffers shared with a consumer daemon.")
    (license (list license:lgpl2.1 license:expat))))

(define-public lttng-tools
  (package
    (name "lttng-tools")
    (version "2.13.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://lttng.org/files/lttng-tools/"
                                  "lttng-tools-" version ".tar.bz2"))
              (sha256
               (base32
                "1jxdwm0a3z70jrx6yg57x5cswi7ycgyazng41rkgxnq7mp2gggak"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-python-bindings")
       ;; FIXME - Tests are disabled for now because one test hangs
       ;; indefinetely.  Also, parallel testing is not possible because of how
       ;; the lttng-daemon handles sessions.  Thus, keep parallel testing
       ;; disabled even after tests are enabled!
       #:tests? #f
       #:parallel-tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-environment-variables
           (lambda _
             (setenv "HOME" "/tmp")
             (setenv "LTTNG_HOME" "/tmp")))
         ;; We don't put (which "man") here because LTTng uses execlp.
         (add-after 'unpack 'patch-default-man-path
           (lambda _
             (substitute* "src/common/defaults.h"
               (("/usr/bin/man") "man"))))
         (add-before 'configure 'autoreconf
           (lambda _
             (invoke "autoreconf" "-vfi"))))))
    ;; NOTE - Users have to install python-3 in their profile to use the
    ;; bindings.  We don't put it in the inputs, because the rest of the tools
    ;; can work without it.
    (inputs
     (list liburcu popt numactl))
    (propagated-inputs
     (list kmod module-init-tools))
    ;; NOTE - LTTng 2.13.9 is distributed with a aclocal.m4 that does not
    ;; support Python3.10.  We can either disable Python's bindings or we can
    ;; reconfigure the project with our autoconf.  This is because the
    ;; distribution tarballs are generated on Ubuntu LTS 18.04.
    ;;
    ;; `paredit-raise-sexp' on the first list of native inputs and remove the
    ;; 'autoreconf build step whenever we bump to the next version that is
    ;; goind to be generated on Ubuntu LTS 22.04.
    (native-inputs
     (append
      (list pkg-config
            perl
            libpfm4
            python-3
            swig
            procps
            which
            flex
            bison
            asciidoc
            libxml2
            lttng-ust)
      (list autoconf
            automake
            libtool)))
    (home-page "https://lttng.org/")
    (synopsis "LTTng userspace tracer libraries")
    (description "The lttng-tools project provides a session
daemon @code{lttng-sessiond} that acts as a tracing registry, the @command{lttng} command
line for tracing control, a @code{lttng-ctl} library for tracing control and a
@code{lttng-relayd} for network streaming.")
    (license (list  license:gpl2 license:lgpl2.1))))

(define-public perf-tools
  (package
    (name "perf-tools")
    (version "1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/brendangregg/perf-tools")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1ab735idi0h62yvhzd7822jj3555vygixv4xjrfrdvi8d2hhz6qn"))))
    (build-system copy-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-file-names
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* '("execsnoop" "killsnoop" "kernel/funcslower")
               (("/usr/bin/gawk")
                (search-input-file inputs "/bin/awk")))
             (substitute* "execsnoop"
               (("/usr/bin/getconf")
                (search-input-file inputs "/bin/getconf"))))))
       #:install-plan
       ',(append
          (map (cut list <> "bin/")
               '("disk/bitesize"
                 "fs/cachestat"
                 "execsnoop"
                 "kernel/funccount"
                 "kernel/funcgraph"
                 "kernel/funcslower"
                 "kernel/functrace"
                 "iolatency"
                 "iosnoop"
                 "killsnoop"
                 "kernel/kprobe"
                 "opensnoop"
                 "misc/perf-stat-hist"
                 "tools/reset-ftrace"
                 "syscount"
                 "net/tcpretrans"
                 "system/tpoint"
                 "user/uprobe"))
          '(("man/man8/" "share/man/man8")))))
    (propagated-inputs
     (list
      bash
      coreutils                         ; cat + rm
      gawk
      file
      perf
      perl
      procps                            ; sysctl
      which))
    (home-page "https://github.com/brendangregg/perf-tools")
    (synopsis "Performance analysis tools")
    (description "Perf-tools is a collection of scripts for performance
analysis and instrumentation based on Linux perf_events (aka perf) and
ftrace.")
    (license (list license:gpl2))))

(define-public systemtap
  (package
    (name "systemtap")
    (version "4.9")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sourceware.org/ftp/systemtap/releases/" name "-"
                    version ".tar.gz"))
              (sha256
               (base32
                "161smpv4ajqfncmgylvs89bbix1id60nf0g7clmga2lxxax3646h"))))
    (build-system gnu-build-system)
    (native-inputs (list cpio python))
    (inputs (list elfutils))

    (home-page "https://sourceware.org/systemtap/")
    (synopsis "GNU/Linux trace/probe tool")
    (description
     "SystemTap provides infrastructure to simplify the
gathering of information about the running Linux system.  This assists
diagnosis of a performance or functional problem.  SystemTap eliminates the
need for the developer to go through the tedious and disruptive
instrument, recompile, install, and reboot sequence that may be otherwise
required to collect data.

SystemTap provides a simple command line interface and scripting language for
writing instrumentation for a live running kernel plus user-space applications.
We are publishing samples, as well as enlarging the internal \"tapset\" script
library to aid reuse and abstraction.  SystemTap is a tool for complex
tasks that may require live analysis, programmable on-line response, and
whole-system symbolic access, and can also handle simple tracing jobs.")
    (license license:gpl2+)))

(define-public uftrace
  (package
    (name "uftrace")
    (version "0.17")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/namhyung/uftrace")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "0p1iy70hc4vl3j16j9vvlh5amvk06l3m35iic2crpavm240dw7y7"))
              (patches (search-patches "uftrace-fix-tests.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:modules
      `((ice-9 match)
        ,@%default-gnu-modules)
      #:make-flags
      #~(list
         (string-append "CC=" #$(cc-for-target)))
      #:test-target "test"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda* (#:key outputs target #:allow-other-keys)
              (let ((arch #$(platform-linux-architecture
                             (lookup-platform-by-target-or-system
                              (or (%current-target-system)
                                  (%current-system))))))
                (setenv "ARCH"
                        (match arch
                          ("arm64" "aarch64")
                          (_ arch)))
                (when target
                  (setenv "CROSS_COMPILE" (string-append target "-"))))
              (setenv "SHELL" (which "sh"))
              (let ((python #$(this-package-input "python"))
                    (luajit #$(this-package-input "luajit")))
                (setenv "LDFLAGS" (string-append "-Wl," "-rpath=" python "/lib"
                                                 ":" luajit "/lib")))
              (invoke "./configure"
                      (string-append "--prefix="
                                     #$output))))
          (add-before 'check 'fix-shebang
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "tests/t220_trace_script.py"
                (("/bin/sh")
                 (search-input-file inputs "bin/sh")))))
          (add-after 'unpack 'delete-network-tests
            (lambda _
              ;; These tests require network capability (localhost)
              (for-each delete-file
                        '("tests/t141_recv_basic.py"
                          "tests/t142_recv_multi.py"
                          "tests/t143_recv_kernel.py"
                          "tests/t150_recv_event.py"
                          "tests/t151_recv_runcmd.py"
                          "tests/t167_recv_sched.py")))))))
    (inputs
     (list capstone
           elfutils
           libunwind
           python ;; libpython3.10.so for python scripting
           luajit ;; libluajit-5.1.so for lua scripting
           ncurses))
    (native-inputs
     (list luajit
           pandoc
           pkg-config
           python-wrapper))
    (home-page "https://github.com/namhyung/uftrace")
    (synopsis "Function graph tracer for C/C++/Rust")
    (description "uftrace is a tool for tracing and analyzing the execution of
programs written in C/C++.  It is heavily inspired by the ftrace framework of
the Linux kernel, while supporting userspace programs.  It supports various
kind of commands and filters to help analysis of the program execution and
performance.  It provides the command @command{uftrace}.")
    (license license:gpl2)))
