;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017 Dave Love <fx@gnu.org>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2019, 2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020 malte Frank Gerdes <malte.f.gerdes@gmail.com>
;;; Copyright © 2020, 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Greg Hogan <code@greghogan.com>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages benchmark)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages php)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-science)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match))

;; Lazily resolve the gcc-toolchain to avoid a circular dependency.  Always
;; use the latest available toolchain to avoid conflicts in user profiles.
(define gcc-toolchain*
  (delay (module-ref (resolve-interface '(gnu packages commencement))
                     'gcc-toolchain-12)))

(define-public fio
  (package
    (name "fio")
    (version "3.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://brick.kernel.dk/snaps/"
                                  "fio-" version ".tar.bz2"))
              (sha256
               (base32
                "03x0n18f2wsyjh6qv57kvgqcwga54rzngwzr6fzlrjsalqw7mxlp"))))
    (build-system gnu-build-system)
    (arguments
     (list #:modules
           `(,@%gnu-build-system-modules
             (ice-9 textual-ports))
           #:test-target "test"
           #:configure-flags
           #~(list "--disable-native")  ;don't generate code for the build CPU
           #:phases
           #~(modify-phases %standard-phases
               (replace 'configure
                 (lambda* (#:key (configure-flags ''()) #:allow-other-keys)
                   ;; The configure script doesn't understand some of the
                   ;; GNU options, so we can't use the stock phase.
                   (apply invoke "./configure"
                          (string-append "--prefix=" #$output)
                          configure-flags)))
               ;; The main `fio` executable is fairly small and self contained.
               ;; Moving the auxiliary scripts to a separate output saves ~100 MiB
               ;; on the closure.
               (add-after 'install 'move-outputs
                 (lambda _
                   (let ((oldbin (string-append #$output "/bin"))
                         (newbin (string-append #$output:utils "/bin"))
                         (script? (lambda* (file #:rest _)
                                    (call-with-input-file file
                                      (lambda (port)
                                        (char=? #\# (peek-char port)))))))
                     (mkdir-p newbin)
                     (for-each (lambda (file)
                                 (link file (string-append newbin "/" (basename file)))
                                 (delete-file file))
                               (find-files oldbin script?))))))))
    (outputs '("out" "utils"))
    (inputs
     (list libaio python zlib))
    (home-page "https://github.com/axboe/fio")
    (synopsis "Flexible I/O tester")
    (description
     "fio is a tool that will spawn a number of threads or processes doing a
particular type of I/O action as specified by the user.  The typical use of fio
is to write a job file matching the I/O load one wants to simulate.")
    ;; The software is distributed under the GPL2, but a handful of components
    ;; are covered by other licenses.
    (license (list license:gpl2 license:gpl2+ license:bsd-2
                   license:public-domain))))

(define-public intel-mpi-benchmarks/openmpi
  (package
    (name "intel-mpi-benchmarks")
    (version "2019.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/intel/mpi-benchmarks")
                    (commit (string-append "IMB-v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02hxbk9g9nl59bk5qcfl3djj7b58vsqys340m1xdbyqwcrbnahh9"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Some source configuration files in the original tarball
                  ;; have inappropriate execute permissions, which interferes
                  ;; with the install phase below.
                  (for-each (lambda (file) (chmod file #o444))
                            (find-files "WINDOWS" "."))
                  #t))))
    (build-system gnu-build-system)
    (inputs
     (list openmpi))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (define (benchmark? file stat)
               (and (string-prefix? "IMB-" (basename file))
                    (executable-file? file)))

             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "." benchmark?))
               #t))))

       ;; The makefile doesn't express all the dependencies, it seems.
       #:parallel-build? #t

       #:make-flags '("CC=mpicc" "CXX=mpicxx")))
    (home-page "https://software.intel.com/en-us/articles/intel-mpi-benchmarks")
    (synopsis "Benchmarks for the Message Passing Interface (MPI)")
    (description
     "This package provides benchmarks for implementations of the @dfn{Message
Passing Interface} (MPI).  It contains MPI performance measurements for
point-to-point and global communication, and file, operations for a range of
message sizes.  The generated benchmark data fully characterize:

@itemize
@item
Performance of a cluster system, including node performance, network latency,
and throughput;
@item
Efficiency of the MPI implementation.
@end itemize")
    (license license:cpl1.0)))

(define-public multitime
  (package
    (name "multitime")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tratt.net/laurie/src/"
                                  "multitime/releases/"
                                  "multitime-" version ".tar.gz"))
              (sha256
               (base32
                "0iyfsdrbyqa7a4ifrh19l9a48hgv7ld6m0d8yf9bkl12q0qw91fx"))))
    (build-system gnu-build-system)
    (arguments '(#:tests? #f)) ; there are no tests
    (home-page "https://tratt.net/laurie/src/multitime/")
    (synopsis "Time command execution over multiple executions")
    (description
     "The @code{time} utility is a simple and often effective way of measuring
how long a command takes to run (wall time).  Unfortunately, running a command
once can give misleading timings.  @code{multitime} is, in essence, a simple
extension to @code{time} which runs a command multiple times and prints the
timing means, standard deviations, mins, medians, and maxes having done so.
This can give a much better understanding of the command's performance.")
    (license license:expat)))

(define-public benchmark
  (package
    (name "benchmark")
    (version "1.5.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/google/benchmark")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "030g4d8vpn2442dsap0qw86lsw7xfl36k0x0x9bn0vvm11qvjn8c"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("googletest-source" ,(package-source googletest))
       ("googletest" ,googletest)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'unpack-googletest
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "googletest-source")
                               "googletest"))))))
    (home-page "https://github.com/google/benchmark")
    (synopsis "Microbenchmark support library")
    (description
     "Benchmark is a library to benchmark code snippets, similar to unit
tests.")
    (license license:asl2.0)))

(define-public bonnie++
  (package
    (name "bonnie++")
    (version "1.98")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.coker.com.au/bonnie++/bonnie++-"
                                  version ".tgz"))
              (sha256
               (base32
                "010bmlmi0nrlp3aq7p624sfaj5a65lswnyyxk3cnz1bqig0cn2vf"))))
    (build-system gnu-build-system)
    (native-inputs
     (list perl))
    (arguments '(#:tests? #f)) ; there are no tests
    (home-page "https://doc.coker.com.au/projects/bonnie/")
    (synopsis "Hard drive and file system benchmark suite")
    (description
     "Bonnie++ is a benchmark suite that is aimed at performing a number of
simple tests of hard drive and file system performance.  Bonnie++ allows you to
benchmark how your file systems perform with respect to data read and write
speed, the number of seeks that can be performed per second, and the number of
file metadata operations that can be performed per second.")
    (license license:gpl2)))   ;GPL 2 only, see copyright.txt

(define-public phoronix-test-suite
  (package
    (name "phoronix-test-suite")
    (version "10.8.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://phoronix-test-suite.com/releases/"
                           name "-" version ".tar.gz"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (ice-9 regex)
                  (srfi srfi-26)))
       (snippet
        '(begin
           ;; Many test profiles have their license identified as "Free",
           ;; while they are in fact non-free (see:
           ;; https://github.com/phoronix-test-suite/phoronix-test-suite/issues/667).
           (define problems             ;see:
             '("bioshock-infinite-1"    ;mis-licensed as free
               "firefox"                ;not FSDG-compliant
               "dirt-rally"             ;mis-licensed as free
               "dirt-showdown"          ;mis-licensed as free
               "dota2"                  ;mis-licensed as free
               "dow3"                   ;mis-licensed as free
               "etqw-demo"              ;mis-licensed as free
               "f12015"                 ;mis-licensed as free
               "f12017"                 ;mis-licensed as free
               "geexlab"                ;mis-licensed as free
               "gfxbench"               ;mis-licensed as free
               "gnupg"                  ;downloads ubuntu image
               "hitman-1"               ;mis-licensed as free
               "hl2lostcoast"           ;mis-licensed as free
               "linux"                  ;contains blobs
               "madmax"                 ;mis-licensed as free
               "metro"                  ;mis-licensed as free
               "minion"                 ;mis-licensed as free
               "sam2017"                ;mis-licensed as free
               "talos-principle"        ;mis-licensed as free
               "tomb-raider"            ;mis-licensed as free
               "tf2"                    ;mis-licensed as free
               "ue4"                    ;mis-licensed as free
               "unigine"                ;mis-licensed as free
               "ut2004"))               ;mis-licensed as free

           (define rx (format #f "(~a)" (string-join problems "|")))

           (define (mark-as-non-free directory)
             (format #t "Marking ~s as non-free...~%" directory)
             (substitute* (find-files directory "^(test|suite)-definition.xml$")
               (("Free")
                "Non-free")))

           (with-directory-excursion "ob-cache/test-profiles/pts"
             (for-each (cut mark-as-non-free <>)
                       (scandir "." (cut string-match rx <>))))))
       (sha256
        (base32
         "1x5pyzzn7ipi0ia1xlvq3bpw0rgf7h7sbr2kzhz1k8y06var480z"))
       (patches (search-patches "phoronix-test-suite-fsdg.patch"))))
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure)
          (delete 'build)
          (replace 'install
            (lambda _
              (invoke "./install-sh" #$output "--free-software-only")))
          (add-after 'install 'wrap-binary
            (lambda* (#:key inputs #:allow-other-keys)
              (let ((pts (string-append #$output "/bin/phoronix-test-suite")))
                (wrap-program pts
                  (list "PATH" 'prefix
                        (map (lambda (binary)
                               (dirname (search-input-file
                                         inputs (string-append "bin/" binary))))
                             '("bash" "cat" ;coreutils
                               "gzip" "make" "php" "sed" "tar" "which"))))))))))
    (build-system gnu-build-system)
    (native-inputs (list python which))
    ;; Wrap the most basic build tools needed by Phoronix Test Suite to build
    ;; simple tests such as 'fio'.
    (inputs (list bash coreutils gnu-make gzip php sed tar which))
    ;; Phoronix Test Suite builds and caches the benchmarking tools itself;
    ;; the user is required to manually install extra libraries depending on
    ;; the selected test; but at least a working C/C++ toolchain is assumed to
    ;; be available.
    (propagated-inputs (list (force gcc-toolchain*)))
    (home-page "https://www.phoronix-test-suite.com/")
    (synopsis "Automated testing/benchmarking software")
    (description
     "The Phoronix Test Suite is a comprehensive testing and benchmarking platform
that provides an extensible framework for which new tests can be easily added.
It can carry out both qualitative and quantitative benchmarks in a clean,
reproducible, and easy-to-use manner, making it easy to compare one particular
setup against another one.")
    (license license:gpl3+)))

(define-public python-locust
  (package
    (name "python-locust")
    (version "2.8.6")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "locust" version))
       (sha256
        (base32
         "1gn13j758j36knlcdyyyggn60rpw98iqdkvl3kjsz34brysic6q1"))))
    (build-system python-build-system)
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'relax-requirements
           (lambda _
             (substitute* "setup.py"
               (("setuptools_scm<=6.0.1")
                "setuptools_scm")
               (("Jinja2<3.1.0")
                "Jinja2"))))
         (replace 'check
           (lambda* (#:key tests? #:allow-other-keys)
             (when tests?
               (invoke "python" "-m" "pytest" "locust"
                       "-k" (string-join
                             '(;; These tests return "non-zero exit status 1".
                               "not test_default_headless_spawn_options"
                               "not test_default_headless_spawn_options_with_shape"
                               "not test_headless_spawn_options_wo_run_time"
                               ;; These tests depend on networking.
                               "not test_html_report_option"
                               "not test_web_options"
                               ;; This test fails because of the warning "System open
                               ;; file limit '1024' is below minimum setting '10000'".
                               "not test_skip_logging"
                               ;; On some (slow?) machines, the following tests
                               ;; fail, with the processes returning exit code
                               ;; -15 instead of the expected 42 and 0,
                               ;; respectively (see:
                               ;; https://github.com/locustio/locust/issues/1708).
                               "not test_custom_exit_code"
                               "not test_webserver"
                               ;; This test fails with "AssertionError:
                               ;; 'stopped' != 'stopping'".
                               "not test_distributed_shape") " and "))))))))
    (propagated-inputs
     (list python-configargparse
           python-flask
           python-flask-basicauth
           python-flask-cors
           python-gevent
           python-geventhttpclient
           python-jinja2
           python-msgpack
           python-psutil
           python-pyzmq
           python-requests
           python-roundrobin
           python-typing-extensions
           python-werkzeug))
    (native-inputs
     (list python-mock
           python-pyquery
           python-pytest
           python-retry
           python-setuptools-scm))
    (home-page "https://locust.io/")
    (synopsis "Distributed load testing framework")
    (description "Locust is a performance testing tool that aims to be easy to
use, scriptable and scalable.  The test scenarios are described in plain
Python.  It provides a web-based user interface to visualize the results in
real-time, but can also be run non-interactively.  Locust is primarily geared
toward testing HTTP-based applications or services, but it can be customized to
test any system or protocol.

Note: Locust will complain if the available open file descriptors limit for
the user is too low.  To raise such limit on a Guix System, refer to
@samp{info guix --index-search=pam-limits-service}.")
    (license license:expat)))

(define-public interbench
  (package
    (name "interbench")
    (version "0.31")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ckolivas/interbench")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ifnw8vnkcgrksx7g5d9ii4kjppqnk32lvrybdybmibyvag6zfdc"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-broken-makefile
           (lambda _
             ;; Remove erroneous "-lm" target
             (substitute* "Makefile"
               (("hackbench.o -lm") "hackbench.o"))))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "interbench" (string-append out "/bin"))
               (install-file "interbench.8" (string-append out "/share/man/man8"))))))))
    (home-page "http://users.on.net/~ckolivas/interbench/")
    (synopsis "Interactivity benchmark")
    (description "interbench is designed to benchmark interactivity on Linux.
It is designed to measure the effect of changes in Linux kernel design or
system configuration changes such as CPU, I/O scheduler and filesystem changes
and options.  With careful benchmarking, different hardware can be compared.")
    (license license:gpl2+)))

(define-public clpeak
  ;; Release 1.1.0 is too old for our opencl-clhpp. This commit supports
  ;; cl2.hpp.
  (let ((commit "6d59cb64997a53c35207b77a63d2e9f0e84de5fd"))
    (package
      (name "clpeak")
      (version (git-version "1.1.0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/krrishnarraj/clpeak.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                  (base32
                    "0qmhdjyhwl7gfgyqxsddqn6zpp3b57503m16h7jv6illy3lfvji1"))))
      (build-system cmake-build-system)
      (home-page "https://github.com/krrishnarraj/clpeak")
      (inputs
        (list opencl-clhpp opencl-icd-loader))
      (synopsis "OpenCL benchmark tool")
      (description
        "A synthetic benchmarking tool to measure peak capabilities of OpenCL
        devices.  It only measures the peak metrics that can be achieved using
        vector operations and does not represent a real-world use case.")
        (license license:unlicense))))

(define-public kdiskmark
  (package
    (name "kdiskmark")
    (version "2.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/JonMagon/KDiskMark")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1l4sw05yx70pcnaa64arjc414mgvyz05pn3gz9nc9hga8v2d3rzn"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      ;; Drop runtime dependency on KDE's KFAuth.
      #~(list "-DPERFORM_PAGECACHE_CLEARING_USING_KF5AUTH=no")
      #:tests? #f                       ;no test suite
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/benchmark.cpp"
                (("\"fio\"")
                 (format #f "~s" (search-input-file inputs "bin/fio")))))))))
    (native-inputs (list extra-cmake-modules qttools-5))
    (inputs (list fio qtbase-5))
    (home-page "https://github.com/JonMagon/KDiskMark")
    (synopsis "Simple disk benchmark tool")
    (description "KDiskMark is an HDD and SSD benchmark tool.  KDiskMark
abstracts away the complexity of the Flexible I/O Tester (@command{fio})
command via a convenient graphical user interface (GUI) and handles its output
to provide an easy to view and interpret benchmark result.  The application is
written in C++ with Qt and doesn't have any runtime KDE dependencies.  Among
its features are:
@itemize
@item Configurable block size, queues, and threads count for each test
@item Many languages support
@item Report generation.
@end itemize")
    (license license:gpl3+)))

(define-public sysbench
  (package
    (name "sysbench")
    (version "1.0.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/akopytov/sysbench")
                    (commit version)))
              (file-name (git-file-name name version))
              (modules '((guix build utils)))
              (snippet '(begin
                          ;; Ensure no bundled libraries get used.
                          (delete-file-recursively "third_party")
                          (substitute* "configure.ac"
                            (("^third_party/.*")
                             ""))
                          (substitute* "Makefile.am"
                            ((".*(LUAJIT|CK)_DIR =.*")
                             ""))))
              (sha256
               (base32
                "1sanvl2a52ff4shj62nw395zzgdgywplqvwip74ky8q7s6qjf5qy"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--with-pgsql"
                                ;; Explicitly specify the library directory of
                                ;; MySQL, otherwise `mysql_config` gets
                                ;; consulted and adds unnecessary link
                                ;; directives.
                                (string-append "--with-mysql-libs="
                                               #$(this-package-input "mysql")
                                               "/lib")
                                "--with-system-luajit"
                                "--with-system-ck"
                                ;; If we let the build tool select the most
                                ;; optimal compiler architecture flag, the
                                ;; build is not reproducible.
                                "--without-gcc-arch")
      #:phases #~(modify-phases %standard-phases
                   (add-after 'unpack 'patch-test-runner
                     (lambda _
                       (substitute* "tests/test_run.sh"
                         (("/bin/bash")
                          (which "bash"))
                         ;; Do not attempt to invoke the cram command via
                         ;; Python, as on Guix it is a shell script (wrapper).
                         (("\\$\\(command -v cram\\)")
                          "-m cram"))))
                   (add-after 'unpack 'disable-test-installation
                     (lambda _
                       (substitute* "tests/Makefile.am"
                         (("install-data-local")
                          "do-not-install-data-local")
                         (("^test_SCRIPTS.*")
                          ""))))
                   (add-after 'unpack 'fix-docbook
                     (lambda* (#:key native-inputs inputs #:allow-other-keys)
                       (substitute* "m4/ax_check_docbook.m4"
                         (("DOCBOOK_ROOT=.*" all)
                          (string-append
                           all "XML_CATALOG="
                           (search-input-file (or native-inputs inputs)
                                              "xml/dtd/docbook/catalog.xml")
                           "\n")))
                       (substitute* "doc/xsl/xhtml.xsl"
                         (("http://docbook.sourceforge.net/release/xsl\
/current/xhtml/docbook.xsl")
                          (search-input-file
                           (or native-inputs inputs)
                           (string-append "xml/xsl/docbook-xsl-"
                                          #$(package-version docbook-xsl)
                                          "/xhtml/docbook.xsl"))))
                       (substitute* "doc/xsl/xhtml-chunk.xsl"
                         (("http://docbook.sourceforge.net/release/xsl\
/current/xhtml/chunk.xsl")
                          (search-input-file
                           (or native-inputs inputs)
                           (string-append "xml/xsl/docbook-xsl-"
                                          #$(package-version docbook-xsl)
                                          "/xhtml/chunk.xsl")))))))))
    (native-inputs (list autoconf
                         automake
                         libtool
                         pkg-config
                         python-cram
                         python-wrapper
                         which
                         ;; For documentation
                         libxml2        ;for XML_CATALOG_FILES
                         libxslt
                         docbook-xml
                         docbook-xsl))
    (inputs (list ck libaio luajit mysql postgresql))
    (home-page "https://github.com/akopytov/sysbench/")
    (synopsis "Scriptable database and system performance benchmark")
    (description "@command{sysbench} is a scriptable multi-threaded benchmark
tool based on LuaJIT.  It is most frequently used for database benchmarks, but
can also be used to create arbitrarily complex workloads that do not involve a
database server.  @command{sysbench} comes with the following bundled
benchmarks:
@table @file
@item oltp_*.lua
A collection of OLTP-like database benchmarks.
@item fileio
A filesystem-level benchmark.
@item cpu
A simple CPU benchmark.
@item memory
A memory access benchmark.
@item threads
A thread-based scheduler benchmark.
@item mutex
A POSIX mutex benchmark.
@end table
It includes features such as:
@itemize
@item
Extensive statistics about rate and latency is available, including latency
percentiles and histograms.
@item
Low overhead even with thousands of concurrent threads.  @command{sysbench} is
capable of generating and tracking hundreds of millions of events per second.
@item
New benchmarks can be easily created by implementing pre-defined hooks in
user-provided Lua scripts.
@item
@end itemize")
    (license license:gpl2+)))
