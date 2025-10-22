;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2015, 2016, 2017, 2019-2022 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2016-2018, 2020, 2021, 2025 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2019 Pkill -9 <pkill9@runbox.com>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2020, 2021 Morgan Smith <Morgan.J.Smith@outlook.com>
;;; Copyright © 2020, 2025 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 Foo Chuan Wei <chuanwei.foo@hotmail.com>
;;; Copyright © 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2023 Andy Tai <atai@atai.org>
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
;;; Copyright © 2025 Nguyễn Gia Phong <mcsinyx@disroot.org>
;;; Copyright © 2025 Robin Templeton <robin@guixotic.coop>
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

(define-module (gnu packages debug)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system go)
  #:use-module (guix build-system linux-module)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system qt)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages attr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages c)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gdb)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lesstif)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public delta
  (package
    (name "delta")
    (version "2006.08.03")
    (source
     (origin
      (method url-fetch)
      (uri (list
            (string-append "mirror://debian/pool/main/d/delta/"
                           "delta_" version ".orig.tar.gz")))
      (sha256
       (base32
        "184wh35pf2ddx97319s6sgkzpz48xxkbwzcjpycv009bm53lh61q"))))
    (build-system gnu-build-system)
    (inputs                             ;Installed programs are perl scripts
     (list perl))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Makefile contains no install target
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc/delta-" ,version)))
               (begin
                 (for-each (lambda (h)
                             (install-file h doc))
                           `("License.txt" ,@(find-files "www" ".*\\.html")))
                 (for-each (lambda (b)
                             (install-file b bin))
                           `("delta" "multidelta" "topformflat"))))
             #t))
         (delete 'configure))))         ; no configure script
    (home-page
     "https://web.archive.org/web/20200701152100/http://delta.tigris.org/")
    (synopsis "Heuristical file minimizer")
    (description
     "Delta assists you in minimizing \"interesting\" files subject to a test
of their interestingness.  A common such situation is when attempting to
isolate a small failure-inducing substring of a large input that causes your
program to exhibit a bug.")
    ;; See License.txt, which is a bsd-3 license, despite the project's
    ;; home-page pointing to a bsd-2 license.
    (license license:bsd-3)))

(define-public c-reduce
  (let ((commit "31e855e290970cba0286e5032971509c0e7c0a80")
        (revision "0"))
    (package
      (name "c-reduce")
      (version (git-version "2.10.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://github.com/csmith-project/creduce")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1yd3wvnkj7qgn2jj5byp58p3qq04w5d6agagrkgi5z5gb6z4qyqk"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (replace 'check
              (lambda _
                (with-directory-excursion "tests"
                  ;; Running all tests can take a looong time, and tests 4 and 5
                  ;; require frama-c or kcc.  So run just one for sanity.
                  (invoke "./run_tests" "1"))))
            (add-after 'install 'set-load-paths
              (lambda _
                ;; Tell creduce where to find the perl modules it needs.
                (wrap-program (string-append #$output "/bin/creduce")
                  `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB")))))))))
      (native-inputs (list flex))
      (inputs (list astyle
                    bash-minimal ;for wrap-program
                    llvm-18
                    clang-18
                    indent
                    perl
                    perl-exporter-lite
                    perl-file-which
                    perl-getopt-tabular
                    perl-regexp-common
                    perl-term-readkey))
      (home-page "https://embed.cs.utah.edu/creduce")
      (synopsis "Reducer for interesting code")
      (description
       "C-Reduce is a tool that takes a large C or C++ program that has a
property of interest (such as triggering a compiler bug) and automatically
produces a much smaller C/C++ program that has the same property.  It is
intended for use by people who discover and report bugs in compilers and other
tools that process C/C++ code.")
      (license license:ncsa))))

(define-public cppdap
  (package
    (name "cppdap")
    (version "1.58.0-a")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/google/cppdap")
         (commit (string-append "dap-" version))))
       (modules '((guix build utils)))
       (snippet
        '(delete-file-recursively "third_party"))
       (patches (search-patches
                 "cppdap-add-CPPDAP_USE_EXTERNAL_GTEST_PACKAGE.patch"))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fq2w35fw1lb5wya1nny45pk3a13b689k48calk1cmqmqpbcjn2b"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-DCPPDAP_BUILD_TESTS=ON"
         "-DCPPDAP_USE_EXTERNAL_GTEST_PACKAGE=ON"
         ;; Only one of the following three can be enabled at the same time
         ;; "-DCPPDAP_USE_EXTERNAL_RAPIDJSON_PACKAGE=ON"
         ;; "-DCPPDAP_USE_EXTERNAL_JSONCPP_PACKAGE=ON"
         "-DCPPDAP_USE_EXTERNAL_NLOHMANN_JSON_PACKAGE=ON")
      #:phases
      (if (target-riscv64?)
          #~(modify-phases %standard-phases
              ;; We need to unconditionally link with libatomic on some
              ;; architectures to successfully build cmake.
              (add-after 'unpack 'link-with-libatomic
                (lambda _
                  (substitute* "CMakeLists.txt"
                    (("cppdap_set_target_options\\(cppdap\\)" all)
                     (string-append
                       all "\n\n"
                       "target_link_libraries(cppdap PRIVATE atomic)"))))))
          #~%standard-phases)))
    (native-inputs
     (list googletest))
    ;; see lib/cmake/cppdap/cppdapConfig.cmake
    ;; cmake file require propagate this.
    (propagated-inputs
     (list
      ;; Only one of the following three can be enabled at the same time
      ;; rapidjson
      ;; jsoncpp
      nlohmann-json))
    (home-page "https://github.com/google/cppdap")
    (synopsis "C++ library for the Debug Adapter Protocol")
    (description
     "cppdap is a C++11 library (\"SDK\") implementation of the Debug Adapter
Protocol, providing an API for implementing a DAP client or server.  cppdap
provides C++ type-safe structures for the full DAP specification, and provides a
simple way to add custom protocol messages.")
    (license license:expat)))

(define-public c-vise
  (package
    (name "c-vise")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marxin/cvise")
             (commit (string-append "v" version))))
       (sha256
        (base32 "1i2z5q2pcwh1gpdqc24x1a2q5vzwhblzzq021nzwf304di7m18vl"))
       (file-name (git-file-name name version))))
    (build-system cmake-build-system)
    (native-inputs
     (list flex python-pytest python-pytest-flake8))
    (inputs
     (list bash-minimal clang llvm unifdef
           python python-pebble python-psutil python-chardet))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-linkage
           (lambda _
            (substitute* "clang_delta/CMakeLists.txt"
              (("\\$\\{LLVM_LINK_LLVM_DYLIB\\}") "True")
              (("  LLVM") "  LLVMSupport"))))
         (add-before 'build 'hardcode-paths
           (lambda _
            (substitute* "cvise.py"
              (("/bin/bash") (which "bash"))
              (("(.*)# Special case for clang-format" & >)
               (string-append > "# Special case for unifdef\n"
                              > "programs['unifdef'] = '" (which "unifdef") "'\n"
                              &)))))
         (add-after 'install 'wrap
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (python-path (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/bin/cvise")
                 `("PYTHONPATH" ":" prefix (,python-path)))
               #t))))))
    (home-page "https://github.com/marxin/cvise")
    (synopsis "Reducer for interesting code")
    (description
     "C-Vise is a Python port of the C-Reduce tool that is fully compatible
and uses the same efficient LLVM-based C/C++ @code{clang_delta} reduction
tool.")
    (license license:ncsa)))

(define-public american-fuzzy-lop
  (let ((machine (match (or (%current-target-system)
                            (%current-system))
                   ("x86_64-linux"   "x86_64")
                   ("i686-linux"     "i386")
                   ("aarch64-linux"  "aarch64")
                   ("armhf-linux"    "arm")
                   ("mips64el-linux" "mips64el")
                   ("powerpc-linux"  "ppc")
                   ;; Prevent errors when querying this package on unsupported
                   ;; platforms, e.g. when running "guix package --search="
                   (_                "UNSUPPORTED"))))
    (package
      (name "american-fuzzy-lop")
      (version "2.57b")             ;It seems all releases have the 'b' suffix
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/google/AFL")
               (commit (string-append "v" version))))
         (sha256
          (base32 "0ks0s8iizp7mpc9mlpv126rsny0dkljfsw68689g9jiisjz2z530"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (inputs
       (list qemu-for-american-fuzzy-lop))
      (arguments
       `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                            (string-append "DOC_PATH=$(PREFIX)/share/doc/"
                                           ,name "-" ,version)
                            "CC=gcc")
         #:phases (modify-phases %standard-phases
                    (add-after 'unpack 'make-git-checkout-writable
                      (lambda _
                        (for-each make-file-writable (find-files "."))
                        #t))
                    (delete 'configure)
                    ,@(if (string=? (%current-system) (or "x86_64-linux"
                                                          "i686-linux"))
                        '()
                        '((add-before 'build 'set-afl-flag
                            (lambda _ (setenv "AFL_NO_X86" "1") #t))
                          (add-after 'install 'remove-x86-programs
                            (lambda* (#:key outputs #:allow-other-keys)
                              (let* ((out (assoc-ref outputs "out"))
                                     (bin (string-append out "/bin/")))
                                (delete-file (string-append bin "afl-gcc"))
                                (delete-file (string-append bin "afl-g++"))
                                (delete-file (string-append bin "afl-clang"))
                                (delete-file (string-append bin "afl-clang++")))
                              #t))))
                    (add-after
                     ;; TODO: Build and install the afl-llvm tool.
                     'install 'install-qemu
                     (lambda* (#:key inputs outputs #:allow-other-keys)
                       (let ((qemu (assoc-ref inputs "qemu"))
                             (out  (assoc-ref outputs "out")))
                         (symlink (string-append qemu "/bin/qemu-" ,machine)
                                  (string-append out "/bin/afl-qemu-trace"))
                         #t)))
                    (delete 'check)))) ; tests are run during 'install phase
      (home-page "https://lcamtuf.coredump.cx/afl/")
      (synopsis "Security-oriented fuzzer")
      (description
       "American fuzzy lop is a security-oriented fuzzer that employs a novel
type of compile-time instrumentation and genetic algorithms to automatically
discover clean, interesting test cases that trigger new internal states in the
targeted binary.  This substantially improves the functional coverage for the
fuzzed code.  The compact synthesized corpora produced by the tool are also
useful for seeding other, more labor- or resource-intensive testing regimes
down the road.")
      (license license:asl2.0))))

(define-public qemu-for-american-fuzzy-lop
  ;; afl only supports using a single afl-qemu-trace executable, so
  ;; we only build qemu for the native target.
  (let ((machine (match (or (%current-target-system)
                            (%current-system))
                   ("x86_64-linux"   "x86_64")
                   ("i686-linux"     "i386")
                   ("aarch64-linux"  "aarch64")
                   ("armhf-linux"    "arm")
                   ("mips64el-linux" "mips64el")
                   ("powerpc-linux"  "ppc")
                   ;; Prevent errors when querying this package on unsupported
                   ;; platforms, e.g. when running "guix package --search="
                   (_                "UNSUPPORTED"))))
  (hidden-package
   (package
    (name "qemu")
    (version "2.10.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "https://download.qemu.org/qemu-"
                                 version ".tar.xz"))
             (sha256
              (base32
               "17w21spvaxaidi2am5lpsln8yjpyp2zi3s3gc6nsxj5arlgamzgw"))
             (patches
              (search-patches "qemu-glibc-2.27.patch"
                              "qemu-glibc-2.30.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(;; Running tests in parallel can occasionally lead to failures, like:
       ;; boot_sector_test: assertion failed (signature == SIGNATURE): (0x00000000 == 0x0000dead)
       #:parallel-tests? #f
       #:configure-flags
       (list (string-append "--target-list=" ,machine "-linux-user"))
       #:make-flags '("V=1")
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs (configure-flags '())
                           #:allow-other-keys)
             ;; The `configure' script doesn't understand some of the
             ;; GNU options.  Thus, add a new phase that's compatible.
             (let ((out (assoc-ref outputs "out")))
               (setenv "SHELL" (which "bash"))

               ;; While we're at it, patch for tests.
               (substitute* "tests/libqtest.c"
                 (("/bin/sh") (which "sh")))

               ;; The binaries need to be linked against -lrt.
               (setenv "LDFLAGS" "-lrt")
               (apply invoke
                      `("./configure"
                        ,(string-append "--cc=" (which "gcc"))
                        ;; Some architectures insist on using HOST_CC
                        ,(string-append "--host-cc=" (which "gcc"))
                        "--disable-debug-info" ; save build space
                        "--enable-virtfs"      ; just to be sure
                        ,(string-append "--prefix=" out)
                        ,(string-append "--sysconfdir=/etc")
                        ,@configure-flags)))))
         (add-after
          'unpack 'apply-afl-patches
          (lambda* (#:key inputs #:allow-other-keys)
            (let* ((afl-src (assoc-ref inputs "afl-source"))
                   (patch-dir "qemu_mode/patches"))
              (copy-recursively (string-append afl-src "/"
                                               patch-dir)
                                patch-dir)
              (install-file
               (string-append patch-dir
                              "/afl-qemu-cpu-inl.h")
               ".")
              (copy-file (string-append afl-src "/config.h")
                         "./afl-config.h")
              (install-file (string-append afl-src "/types.h")
                            ".")
              (substitute* "afl-qemu-cpu-inl.h"
                (("\\.\\./\\.\\./config.h") "afl-config.h"))
              (substitute* (string-append patch-dir
                                          "/cpu-exec.diff")
                (("\\.\\./patches/") ""))

              ;; These were already applied to qemu-minimal-2.10.
              (for-each (lambda (obsolete-patch)
                          (delete-file (string-append
                                        patch-dir "/"
                                        obsolete-patch)))
                        (list "configure.diff"
                              "memfd.diff"))

              (for-each (lambda (patch-file)
                          (invoke "patch" "--force" "-p1"
                                  "--input" patch-file))
                        (find-files patch-dir
                                    "\\.diff$"))
              #t)))
         (add-before 'check 'disable-unusable-tests
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "tests/Makefile.include"
               ;; Comment out the test-qga test, which needs /sys and
               ;; fails within the build environment.
               (("check-unit-.* tests/test-qga" all)
                (string-append "# " all)))
             (substitute* "tests/Makefile.include"
               ;; Comment out the test-char test, which needs networking and
               ;; fails within the build environment.
               (("check-unit-.* tests/test-char" all)
                (string-append "# " all)))
             #t)))))
    (native-inputs
     `(("python-2" ,python-2) ; QEMU 2 needs Python 2
       ("glib:bin" ,glib "bin")
       ("perl" ,perl)
       ("flex" ,flex)
       ("bison" ,bison)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("afl-source" ,(package-source american-fuzzy-lop))
       ("alsa-lib" ,alsa-lib)
       ("attr" ,attr)
       ("glib" ,glib)
       ("libaio" ,libaio)
       ("libattr" ,attr)
       ("libcap" ,libcap)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("ncurses" ,ncurses)
       ("pixman" ,pixman)
       ("util-linux" ,util-linux)
       ("zlib" ,zlib)))
    (home-page "https://www.qemu.org")
    (synopsis "Machine emulator and virtualizer (without GUI) for american fuzzy lop")
    (description
     "QEMU is a generic machine emulator and virtualizer.  This package
of QEMU is used only by the american fuzzy lop package.

When used as a machine emulator, QEMU can run OSes and programs made for one
machine (e.g. an ARM board) on a different machine---e.g., your own PC.  By
using dynamic translation, it achieves very good performance.

When used as a virtualizer, QEMU achieves near native performances by
executing the guest code directly on the host CPU.  QEMU supports
virtualization when executing under the Xen hypervisor or using
the KVM kernel module in Linux.  When using KVM, QEMU can virtualize x86,
server and embedded PowerPC, and S390 guests.")
    ;; Many files are GPLv2+, but some are GPLv2-only---e.g., `memory.c'.
    (license license:gpl2)
    ;; Several tests fail on MIPS.
    (supported-systems (delete "mips64el-linux" %supported-systems))))))

(define-public aflplusplus
  (package
    (inherit american-fuzzy-lop)
    (name "aflplusplus")
    (version "4.33c")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/AFLplusplus/AFLplusplus")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h4ya82s1wd11c04r0g0wsq56asl1np3nmsf6yk4vyic9nm0hsq0"))))
    (arguments
     (substitute-keyword-arguments (package-arguments american-fuzzy-lop)
       ((#:make-flags _ ''())
        #~(list (string-append "PREFIX=" #$output)
                (string-append "DOC_PATH=" #$output "/share/doc/"
                               #$(package-name this-package) "-"
                               #$(package-version this-package))
                (string-append "CC=" #$(cc-for-target))))
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            ;; For GCC plugins.
            (add-after 'unpack 'patch-gcc-path
              (lambda* (#:key inputs #:allow-other-keys)
                (substitute* "src/afl-cc.c"
                  (("alt_cc = \"gcc\";")
                   (format #f "alt_cc = \"~a\";"
                           (search-input-file inputs "bin/gcc")))
                  (("alt_cxx = \"g\\+\\+\";")
                   (format #f "alt_cxx = \"~a\";"
                           (search-input-file inputs "bin/g++"))))))))))
    ;; According to the Dockerfile, GCC 12 is producing compile errors for some
    ;; targets, so explicitly use GCC 11 here.
    (inputs (list gcc-11 gmp python qemu))
    (native-inputs (list gcc-11))
    (home-page "https://aflplus.plus/")
    (description
     "AFLplusplus is a security-oriented fuzzer that employs a novel type of
compile-time instrumentation and genetic algorithms to automatically discover
clean, interesting test cases that trigger new internal states in the targeted
binary.  This substantially improves the functional coverage for the fuzzed
code.  The compact synthesized corpora produced by the tool are also useful for
seeding other, more labor- or resource-intensive testing regimes down the road.
It is a fork of American Fuzzy Lop fuzzer and features:
@itemize
@item A more recent qemu version.
@item More algorithms like collision-free coverage, enhanced laf-intel &
redqueen, AFLfast++ power schedules, MOpt mutators, unicorn_mode, etc.
@end itemize")))

(define-public backward-cpp
  (package
    (name "backward-cpp")
    (version "1.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/bombela/backward-cpp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b2h03iwfhcsg8i4f125mlrjf8l1y7qsr2gsbkv0z03i067lykns"))))
    (arguments
     (list
      #:configure-flags
      #~(list "-DBACKWARD_SHARED=ON")))
    (build-system cmake-build-system)
    (inputs
     (list libiberty zlib))
    (synopsis "Stack trace pretty printer for C++")
    (description
     "Backward-cpp is a stack trace pretty printer for C++.
It can print annotated stack traces using debug info in the executable.")
    (home-page "https://github.com/bombela/backward-cpp")
    (license license:expat)))

(define-public stress-make
  (let ((commit "97815bed8060de33952475b3498767c91f59ffd9")
        (revision "2"))                 ;No official source distribution
    (package
      (name "stress-make")
      (version (git-version "1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/lanl/stress-make")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0k55cy7x0hlc6rgpascl6ibhcfxaash3p9r9r8kwvbm3zag1rmac"))))
      (build-system gnu-build-system)
      (native-inputs
       (list autoconf automake go))
      (inputs
       `(("make-source" ,(package-source gnu-make))))
      (arguments
       ;; stress-make's configure script insists on having a tarball and does
       ;; not accept a directory name instead.  To let the gnu-build-system's
       ;; patch-* phases work properly, we unpack the source first, then
       ;; repack before the configure phase.
       (let ((make-dir (string-append "make-" (package-version gnu-make))))
         `(#:configure-flags '("--with-make-tar=./make.tar.xz"
                               "make_cv_sys_gnu_glob=yes")
           #:phases
           (modify-phases %standard-phases
             (add-after 'unpack 'unpack-make
               (lambda* (#:key inputs #:allow-other-keys)
                 (invoke "tar" "xf" (assoc-ref inputs "make-source"))))
             (add-after 'unpack-make 'set-default-shell
               (lambda _
                 ;; Taken mostly directly from (@ (gnu packages base) gnu-make)
                 (substitute* (string-append ,make-dir "/src/job.c")
                   (("default_shell = .*$")
                    (format #f "default_shell = \"~a\";\n"
                            (which "sh"))))))
             (add-before 'configure 'repack-make
               (lambda _
                 (invoke "tar" "cJf" "./make.tar.xz" ,make-dir)))
             (add-before 'build 'setup-go
               ;; The Go cache is required starting in Go 1.12, and it needs
               ;; to be writable.
               (lambda _ (setenv "GOCACHE" "/tmp/go-cache") #t))))))
      (home-page "https://github.com/lanl/stress-make")
      (synopsis "Expose race conditions in Makefiles")
      (description
       "Stress Make is a customized GNU Make that explicitly manages the order
in which concurrent jobs are run to provoke erroneous behavior into becoming
manifest.  It can run jobs in the order in which they're launched, in backwards
order, or in random order.  The thought is that if code builds correctly with
Stress Make, then it is likely that the @code{Makefile} contains no race
conditions.")
      ;; stress-make wrapper is under BSD-3-modifications-must-be-indicated,
      ;; and patched GNU Make is under its own license.
      (license (list (license:non-copyleft "LICENSE.md")
                     license:gpl3+)))))

(define-public zzuf
  (package
    (name "zzuf")
    (version "0.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/samhocevar/zzuf/releases/download/v"
             version "/" name "-" version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1mpzjaksc2qg2hzqflf39pl06p53qam2dn3hkhkcv6p00d2n4kx3"))))
    (build-system gnu-build-system)
    (native-inputs (list gcc-13))
    (home-page "https://github.com/samhocevar/zzuf")
    (synopsis "Transparent application input fuzzer")
    (description "Zzuf is a transparent application input fuzzer.  It works by
intercepting file operations and changing random bits in the program's
input.  Zzuf's behaviour is deterministic, making it easy to reproduce bugs.")
    (license license:wtfpl2)))

(define-public scanmem
  (package
    (name "scanmem")
    (version "0.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/scanmem/scanmem")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "17p8sh0rj8yqz36ria5bp48c8523zzw3y9g8sbm2jwq7sc27i7s9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--enable-gui")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'hardcode-python
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "gui/GameConqueror.py"
               (("/usr/bin/env python")
                (search-input-file inputs "/bin/python")))))
         (add-after 'install 'wrap-gameconqueror
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH"))
                   (python-path       (getenv "GUIX_PYTHONPATH")))
               (wrap-program (string-append out "/share/gameconqueror/GameConqueror.py")
                 `("GI_TYPELIB_PATH"        ":" prefix (,gi-typelib-path))
                 `("GUIX_PYTHONPATH"             ":" prefix (,python-path))))
             #t)))))
    (native-inputs
     (list libtool
           python-wrapper
           gobject-introspection
           gtk+
           intltool
           automake
           autoconf))
    (inputs
     (list bash-minimal
           readline))
    (propagated-inputs
     (list python-pygobject))
    (home-page "https://github.com/scanmem/scanmem")
    (synopsis "Memory scanner")
    (description "Scanmem is a debugging utility designed to isolate the
address of an arbitrary variable in an executing process.  Scanmem simply
needs to be told the pid of the process and the value of the variable at
several different times.  After several scans of the process, scanmem isolates
the position of the variable and allows you to modify its value.")
    ;; The library is covered by LGPLv3 or later; the application is covered
    ;; by GPLv3 or later.
    (license (list license:lgpl3+ license:gpl3+))))

(define-public remake
  (package (inherit gnu-make)
    (name "remake")
    (version "4.3-1.6")
    (source (origin
              (method url-fetch)
              (uri (let ((upstream-version
                          (match (string-split version #\-)
                            ((ver sub) (string-append ver "%2Bdbg-" sub)))))
                     (string-append "mirror://sourceforge/bashdb/"
                                    "remake/" upstream-version "/"
                                    "remake-" upstream-version ".tar.gz")))
              (file-name (string-append "remake-" version ".tar.gz"))
              (sha256
               (base32
                "11vvch8bi0yhjfz7gn92b3xmmm0cgi3qfiyhbnnj89frkhbwd87n"))
              (patches (search-patches "remake-impure-dirs.patch"))))
    (inputs
     (modify-inputs (package-inputs gnu-make)
       (prepend readline)))
    (home-page "https://bashdb.sourceforge.net/remake/")
    (description "Remake is an enhanced version of GNU Make that adds improved
error reporting, better tracing, profiling, and a debugger.")
    (license license:gpl3+)))

(define-public rr
  ;; Use a newer commit, which contains unreleased fixes to the
  ;; zen-pmu-workaround.c kernel module, among others.
  (let ((commit "7fe1e367c2b4e0df7647e020c20d66827badfad3")
        (revision "0"))
    (package
      (name "rr")
      (version (git-version "5.9.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/mozilla/rr")
                       (commit commit)))
                (sha256
                 (base32
                  "1zgxk4blwwq0bigi0g8dafgl330aid3kw9ym426825flc3pp1c3m"))
                (file-name (git-file-name name version))))
      (build-system cmake-build-system)
      (arguments
       `(#:configure-flags
         ;; The 'rr_exec_stub' is a static binary, which leads CMake to fail
         ;; with ‘file RPATH_CHANGE could not write new RPATH: ...’.
         ;; Clear CMAKE_INSTALL_RPATH to avoid that problem.
         (list "-DCMAKE_INSTALL_RPATH="
               ;; Satisfy the ‘validate-runpath’ phase.  This isn't a direct
               ;; consequence of clearing CMAKE_INSTALL_RPATH.
               (string-append "-DCMAKE_EXE_LINKER_FLAGS=-Wl,-rpath="
                              (assoc-ref %build-inputs "capnproto")
                              "/lib,-rpath=" (assoc-ref %build-inputs "zlib")
                              "/lib,-rpath=" (assoc-ref %build-inputs "zstd")
                              "/lib")
               ,@(if (and (not (%current-target-system))
                          (member (%current-system)
                                  '("x86_64-linux" "aarch64-linux")))
                     ;; The toolchain doesn't support '-m32'.
                     '("-Ddisable32bit=ON")
                     '()))

         ;; XXX: Most tests fail with:
         ;;
         ;;  rr needs /proc/sys/kernel/perf_event_paranoid <= 1, but it is 2.
         ;;
         ;; This setting cannot be changed from the build environment, so skip
         ;; the tests.
         #:tests? #f

         #:phases (modify-phases %standard-phases
                    (add-before 'check 'set-home
                      (lambda _
                        ;; Some tests expect 'HOME' to be set.
                        (setenv "HOME" (getcwd)))))))
      (native-inputs
       (list lldb pkg-config which))
      (inputs
       (list gdb
             capnproto
             python
             python-pexpect
             zlib
             `(,zstd "lib")))

      ;; List of supported systems according to 'src/preload/raw_syscall.S'.
      (supported-systems '("x86_64-linux" "i686-linux" "aarch64-linux"))

      (home-page "https://rr-project.org/")
      (synopsis "Record and replay debugging framework")
      (description
       "rr is a lightweight tool for recording, replaying and debugging
execution of applications (trees of processes and threads).  Debugging extends
GDB with very efficient reverse-execution, which in combination with standard
GDB/x86 features like hardware data watchpoints, makes debugging much more
fun.")
      (license license:expat))))

(define-public rr-zen-pmu-workaround
  (package
    (name "rr-zen-pmu-workaround")
    (version (package-version rr))
    (source (package-source rr))
    (build-system linux-module-build-system)
    (arguments
     (list
      #:tests? #f                       ;no tests
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'cd
            (lambda _
              (chdir "third-party/zen-pmu-workaround"))))))
    (home-page "https://github.com/rr-debugger/rr/wiki/Zen")
    (synopsis "AMD Zen workaround kernel module for rr")
    (description "This is a Linux kernel module that implements
 workarounds needed for using the @command{rr} debugger with AMD Zen
@acronym{CPU, Central Processing Unit}.")
    (license license:gpl2)))            ;GPLv2 only, like the kernel

(define-public libbacktrace
  ;; There are no releases nor tags.
  (let ((revision "2")
        (commit "cdb64b688dda93bbbacbc2b1ccf50ce9329d4748"))
    (package
      (name "libbacktrace")
      (version (git-version "1.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/ianlancetaylor/libbacktrace")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0iwd41pgr2nxlmghqdfwfwxac27jbqxwxp07jihhq85a8s3igjgr"))))
      (build-system gnu-build-system)
      (arguments
       `(#:parallel-tests? #f ;spurious failures when testing in parallel
         #:make-flags '("CFLAGS=-fPIC")))
      (home-page "https://github.com/ianlancetaylor/libbacktrace")
      (synopsis "C library for producing symbolic backtraces")
      (description "The @code{libbacktrace} library can be linked into a C/C++
program to produce symbolic backtraces.")
      (license license:bsd-3))))

(define-public libleak
  (package
    (name "libleak")
    (version "0.3.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/WuBingzheng/libleak")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1p6x20mm0dym2qn10d6cvwmh71m93xwcd319g94zkv88hj5q17n6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test suite
       #:parallel-build? #f             ;jobserver unavailable
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'unbundle-libwuya
                    (lambda _
                      (substitute* "Makefile"
                        ((".*make -C libwuya.*") ""))
                      #t))
                  (add-before 'build 'set-CC
                    (lambda _
                      (setenv "CC" "gcc")
                      #t))
                  (delete 'configure)   ;no configure script
                  (replace 'install
                    (lambda* (#:key outputs #:allow-other-keys)
                      (let* ((out (assoc-ref outputs "out")))
                        (install-file "libleak.so" (string-append out "/lib"))
                        #t))))))
    (inputs (list libbacktrace libwuya))
    (home-page "https://github.com/WuBingzheng/libleak")
    (synopsis "Memory leaks detection tool")
    (description "The libleak tool detects memory leaks by hooking memory
functions such as @code{malloc}.  It comes as a shared object to be pre-loaded
via @code{LD_PRELOAD} when launching the application.  It prints the full call
stack at suspicious memory leak points.  Modifying or recompiling the target
program is not required, and the detection can be enabled or disabled while
the target application is running.  The overhead incurred by libleak is
smaller than that of other tools such as Valgrind, and it aims to be easier to
use than similar tools like @command{mtrace}.")
    (license license:gpl2+)))

(define-public cgdb
  (package
    (name "cgdb")
    (version "0.8.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://cgdb.me/files/cgdb-" version ".tar.gz"))
       (sha256
        (base32 "1w8ib2vg3pg68d9hh97fw5042c73i9nqavdddc87n9bpscjbaf0d"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal ncurses readline gdb))
    (native-inputs
     (list flex texinfo))
    (arguments
     `(#:configure-flags
        (list
          (string-append "ac_cv_rl_version=" ,(package-version readline))
          "ac_cv_file__dev_ptmx=no"
          "ac_cv_file__proc_self_status=no"
          "ac_cv_func_setpgrp_void=no")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((gdb (search-input-file inputs "bin/gdb"))
                   (sh (search-input-file inputs "bin/sh")))
               (substitute* "lib/util/fork_util.cpp"
                 (("GDB = \"gdb\"") (string-append "GDB = \"" gdb "\"")))
               (substitute* "cgdb/cgdb.cpp" (("/bin/sh") sh))))))))
    (home-page "https://cgdb.github.io")
    (synopsis "Console front-end to the GNU debugger")
    (description
     "@code{cgdb} is a lightweight curses (terminal-based) interface to the
GNU Debugger (GDB).  In addition to the standard gdb console, cgdb provides
a split screen view that displays the source code as it executes.  The
keyboard interface is modeled after vim, so vim users should feel at home
using cgdb.")
    (license license:gpl2+)))

(define-public mspdebug
  ;; Last official release was 24 July 2017
  (let ((commit "4c4d94e43bc4a18ecf82070ff81cd38dd5641e3b")
        (revision "0"))
    (package
      (name "mspdebug")
      (version (git-version "0.25" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/dlbeer/mspdebug")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "1lgw1dsc1aglyja610ichadvgs5b0df3wlarinczb0ykf431gjln"))))
      (build-system gnu-build-system)
      (arguments
       `(#:tests? #f                    ; no test suite
         #:phases
         (modify-phases %standard-phases
           (delete 'configure))         ; no configure script
         #:make-flags
         (list (string-append "CC=" ,(cc-for-target))
               "INSTALL=install"
               (string-append "PREFIX=" %output))))
      (inputs
       (list libusb-compat readline))
      (synopsis "Debugging tool for MSP430 MCUs")
      (description "MspDebug supports FET430UIF, eZ430, RF2500 and Olimex
MSP430-JTAG-TINY programmers, as well as many other compatible
devices.  It can be used as a proxy for gdb or as an independent
debugger with support for programming, disassembly and reverse
engineering.")
      (home-page "https://github.com/dlbeer/mspdebug")
      (license license:gpl2+))))

(define-public seer-gdb
  (package
    (name "seer-gdb")
    (version "1.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/epasveer/seer.git")
                     (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "0jdvyg2jab1pvf36pvkyrfsg2wyy8zp1qx0v2ksclgrnr1hja6k6"))))
    (build-system qt-build-system)
    (arguments
     `(#:tests? #f ; Those are strangely manual
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "src"))))))
    (inputs
     (list qtbase-5 qtcharts-5 qtwayland-5))
    (synopsis "GUI frontend for GDB")
    (description "This package provides a frontend to GDB, the GNU debugger.")
    (home-page "https://github.com/epasveer/seer")
    ;; Note: Some icons in src/resources are creative commons 3.0 and/or 4.0.
    (license license:gpl3+)))

(define-public ddd
  (package
    (name "ddd")
    (version "3.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/ddd/ddd-" version ".tar.gz"))
              (patches (search-patches "ddd-build.patch"))
              (sha256
               (base32
                "12gfyh139rim49m56lxm36ckdyiiz4n3la3y6ik1aqgrqfk1fxdq"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f                 ;tests require manual intervention
           #:configure-flags
           #~(list (string-append "--with-freetype-includes="
                   #$(this-package-input "freetype") "/include/freetype2"))))
    (native-inputs
     (list pkg-config bison flex perl))
    (inputs
     (list libxaw libxft freetype motif ncurses gdb))
    (synopsis "Graphical front-end for GDB and other debuggers")
    (description "GNU DDD, the Data Display Debugger, is a graphical front-end
for command-line debuggers.  Many back-end debuggers are supported, notably
the GNU debugger, GDB.  In addition to usual debugging features such as
viewing the source files, DDD has additional graphical, interactive features
to aid in debugging.")
    (home-page "https://www.gnu.org/software/ddd/")
    (license license:gpl3+)))

(define-public delve
  (package
    (name "delve")
    (version "1.25.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/go-delve/delve")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1b6vp6m4vjx4wfs1djhpizvz40s563jwld9lgjq82svaiilrmlqa"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/go-delve/delve/cmd/dlv"
      #:unpack-path "github.com/go-delve/delve"
      ;; [1] Want mod github.com/go-delve/delve and dep
      ;; github.com/google/go-dap in the output.
      ;; [2] Output of go tool nm contains MethodByName.
      #:test-flags #~(list "-skip" "TestVersion|TestDeadcodeEliminated")))
    (native-inputs
     (list go-github-com-cilium-ebpf
           go-github-com-cosiner-argv
           go-github-com-creack-pty
           go-github-com-derekparker-trie
           go-github-com-go-delve-liner
           go-github-com-google-go-dap
           go-github-com-hashicorp-golang-lru
           go-github-com-mattn-go-colorable
           go-github-com-mattn-go-isatty
           go-github-com-spf13-cobra
           go-github-com-spf13-pflag
           go-go-starlark-net
           go-golang-org-x-arch
           go-golang-org-x-sys
           go-golang-org-x-telemetry
           go-golang-org-x-tools
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/go-delve/delve")
    (synopsis "Debugger for the Go programming language")
    (description "Delve is a debugger for the Go programming language.")
    (license license:expat)))

(define-public fiu
  (package
    (name "fiu")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://blitiri.com.ar/p/libfiu/files/"
                           version "/libfiu-" version ".tar.gz"))
       (sha256
        (base32 "0x4ncvi6sv22rqi9x61byybpmch0z1zvpr6p48axkk890ysv6fim"))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags #~(list (string-append "PREFIX=" #$output)
                                (string-append "CC=" #$(cc-for-target))
                                (string-append "LDFLAGS=-Wl,-rpath="
                                               #$output "/lib"))
           #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (add-before 'check 'set-env
                          ;; Shorten paths to sockets in tests.
                          (lambda _ (setenv "TMPDIR" "/tmp"))))
           #:test-target "test"))
    (native-inputs (list python))      ; for tests
    (synopsis "Fault injector in userspace")
    (description "Fiu provides CLI utilities and a C library
to mark points of failure inside your code
and to enable/disable the failure of those points.")
    (home-page "https://blitiri.com.ar/p/libfiu")
    (license license:public-domain)))

(define-public python-fiu
  (package
    (name "python-fiu")
    (version "1.2")
    (source (package-source fiu))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'enter-python-dir
                          (lambda _ (chdir "bindings/python")))
                        (add-before 'build 'set-env
                          (lambda* (#:key inputs #:allow-other-keys)
                            (setenv "PLIBPATH"
                                    (string-append (assoc-ref inputs "fiu")
                                                   "/lib")))))
           #:tests? #f))               ; tests run in fiu
    (native-inputs (list python-setuptools python-wheel))
    (inputs (list fiu))
    (synopsis "Python binding for fiu (fault injection in userspace)")
    (description "This package includes two Python modules:
@enumerate
@item @code{fiu} is a wrapper for @code{libfiu}, the fault injection C library.
@item @code{fiu_ctrl} provide an easy way run a command
with @code{libfiu} enabled, and controlling the failure points dynamically.
@end enumerate")
    (home-page (package-home-page fiu))
    (license (package-license fiu))))
