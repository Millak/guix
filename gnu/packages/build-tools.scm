;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017 Corentin Bocquillon <corentin@nybble.fr>
;;; Copyright © 2017–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Fis Trivial <ybbs.daans@hotmail.com>
;;; Copyright © 2018 Tomáš Čech <sleep_walker@gnu.org>
;;; Copyright © 2018, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019, 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2019 Jonathan Brielmaier <jonathan.brielmaier@web.de>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Yuval Kogman <nothingmuch@woobling.org>
;;; Copyright © 2020 Jakub Kądziołka <kuba@kadziolka.net>
;;; Copyright © 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021, 2023, 2024, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2022, 2023 Juliana Sims <juli@incana.org>
;;; Copyright © 2024 Evgeny Pisemsky <mail@pisemsky.site>
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

(define-module (gnu packages build-tools)
  #:use-module (ice-9 optargs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system guile)
  #:use-module (guix modules)
  #:use-module (guix search-paths)
  #:use-module (gnu packages)
  #:use-module (gnu packages adns)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages code)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages cppi)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages lisp)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages regex)
  #:use-module (gnu packages rpc)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages unicode)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system python))

(define-public bam
  (package
    (name "bam")
    (version "0.5.1")
    (source (origin
              ;; do not use auto-generated tarballs
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/matricks/bam")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "13br735ig7lygvzyfd15fc2rdygrqm503j6xj5xkrl1r7w2wipq6"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags `(,(string-append "CC=" ,(cc-for-target))
                      ,(string-append "INSTALL_PREFIX="
                                      (assoc-ref %outputs "out")))
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("python" ,python-2)))
    (inputs
     (list lua))
    (home-page "https://matricks.github.io/bam/")
    (synopsis "Fast and flexible build system")
    (description "Bam is a fast and flexible build system.  Bam uses Lua to
describe the build process.  It takes its inspiration for the script files
from scons.  While scons focuses on being 100% correct when building, bam
makes a few sacrifices to acquire fast full and incremental build times.")
    (license license:bsd-3)))

(define-public bear
  (package
    (name "bear")
    (version "3.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rizsotto/Bear")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1x99d2cycgxay62cz2ypjjkmjgrbdvz5d3pg4fyv0gnq2srnlcnm"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after 'unpack 'disable-TEST_BEFORE_INSTALL
                    (lambda _
                      (substitute* "CMakeLists.txt"
                        ;; Delete the matching line—and comment out the next.
                        ((".*TEST_(BEFORE_INSTALL|COMMAND).*") "#"))))
                  (add-before 'check 'set-build-environment
                    (lambda _
                      (setenv "CC" "gcc")))
                  (replace 'check
                    ;; TODO: Test configuration is incomplete.
                    (lambda* (#:key tests? #:allow-other-keys)
                      (when tests?
                        (invoke "ctest")))))))
    (inputs
     `(("c-ares" ,c-ares)
       ("fmt" ,fmt-8)
       ("grpc" ,grpc)
       ("nlohmann-json" ,nlohmann-json)
       ("protobuf" ,protobuf)
       ("python" ,python-wrapper)
       ("re2" ,re2)
       ("spdlog" ,spdlog-1.10)))
    (native-inputs
     `(("abseil-cpp" ,abseil-cpp-cxxstd11)
       ("googletest" ,googletest)
       ("openssl" ,openssl)
       ("pkg-config" ,pkg-config)
       ("python-lit" ,python-lit)
       ("zlib" ,zlib)))
    (home-page "https://github.com/rizsotto/Bear")
    (synopsis "Tool for generating a compilation database")
    (description "A JSON compilation database is used in the Clang project to
provide information on how a given compilation unit is processed.  With this,
it is easy to re-run the compilation with alternate programs.  Bear is used to
generate such a compilation database.")
    (license license:gpl3+)))

(define-public bmake
  (package
    (name "bmake")
    (version "20230723")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://www.crufty.net/ftp/pub/sjg/bmake-" version ".tar.gz"))
       (sha256
        (base32 "012rzgjmncdla1l43f9wl8v13h7d46zgn28k6djpcgx23fahsan4"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal))
    (native-inputs
     (list coreutils))
    (arguments
     (list
      #:tests? #f                       ; test during build
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'configure 'fix-test ; fix from nixpkgs
            (lambda* (#:key inputs native-inputs #:allow-other-keys)
              (substitute* "unit-tests/unexport-env.mk"
                (("PATH=\t/bin:/usr/bin:/sbin:/usr/sbin")
                 "PATH := ${PATH}"))
              (substitute* '("unit-tests/opt-keep-going-indirect.mk"
                             "unit-tests/opt-keep-going-indirect.exp")
                (("false")
                 (search-input-file (or native-inputs inputs) "/bin/false")))))
          (add-after 'configure 'remove-fail-tests
            (lambda _
              (substitute* "unit-tests/Makefile"
                (("cmd-interrupt") "")
                (("deptgt-interrupt") "")
                (("varmod-localtime") "")))))
      #:configure-flags
      #~(list
         (string-append
          "--with-defshell=" #$(this-package-input "bash-minimal") "/bin/bash")
         (string-append
          "--with-default-sys-path=" #$output "/share/mk"))
      #:make-flags
      #~(list "INSTALL=install")))      ; use coreutils' install
    (home-page "http://www.crufty.net/help/sjg/bmake.htm")
    (synopsis "BSD's make")
    (description
     "bmake is a program designed to simplify the maintenance of other
programs.  Its input is a list of specifications as to the files upon which
programs and other files depend.")
    (license license:bsd-3)))

(define-public gn
  (let ((commit "1c4151ff5c1d6fbf7fa800b8d4bb34d3abc03a41")
        (revision "2072"))            ;as returned by `git describe`, used below
    (package
      (name "gn")
      (version (git-version "0.0" revision commit))
      (home-page "https://gn.googlesource.com/gn")
      (source (origin
                (method git-fetch)
                (uri (git-reference (url home-page) (commit commit)))
                (sha256
                 (base32
                  "02621c9nqpr4pwcapy31x36l5kbyd0vdgd0wdaxj5p8hrxk67d6b"))
                (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (arguments
       (list #:phases
             #~(modify-phases %standard-phases
                 (add-before 'configure 'set-build-environment
                   (lambda _
                     (setenv "CC" "gcc")
                     (setenv "CXX" "g++")
                     (setenv "AR" "ar")))
                 (replace 'configure
                   (lambda _
                     (invoke "python" "build/gen.py"
                             "--no-last-commit-position")))
                 (add-after 'configure 'create-last-commit-position
                   (lambda _
                     ;; Mimic GenerateLastCommitPosition from gen.py.
                     (call-with-output-file "out/last_commit_position.h"
                       (lambda (port)
                         (format port
                                 "// Generated by Guix.

#ifndef OUT_LAST_COMMIT_POSITION_H_
#define OUT_LAST_COMMIT_POSITION_H_

#define LAST_COMMIT_POSITION_NUM ~a
#define LAST_COMMIT_POSITION \"~a (~a)\"

#endif  // OUT_LAST_COMMIT_POSITION_H_
"
                                 #$revision #$revision
                                 #$(string-take commit 12))))))
                 (replace 'build
                   (lambda _
                     (invoke "ninja" "-C" "out" "gn"
                             "-j" (number->string (parallel-job-count)))))
                 (replace 'check
                   (lambda* (#:key tests? #:allow-other-keys)
                     (if tests?
                         (begin
                           (invoke "ninja" "-C" "out" "gn_unittests"
                                   "-j" (number->string (parallel-job-count)))
                           (invoke "./out/gn_unittests"))
                         (format #t "test suite not run~%"))))
                 (replace 'install
                   (lambda _
                     (install-file "out/gn" (string-append #$output "/bin")))))))
      (native-inputs
       (list ninja python-wrapper))
      (synopsis "Generate Ninja build files")
      (description
       "GN is a tool that collects information about a project from @file{.gn}
files and generates build instructions for the Ninja build system.")
      ;; GN is distributed as BSD-3, but bundles some files from ICU using the
      ;; X11 license.
      (license (list license:bsd-3 license:x11)))))

(define-public meson
  (package
    (name "meson")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mesonbuild/meson/"
                                  "releases/download/" version  "/meson-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02wi62k9w7716xxdgrrx68q89vaq3ncnbpw5ms0g27npn2df0mgr"))))
    (build-system python-build-system)
    (arguments
     (list #:tests? #f                  ;disabled to avoid extra dependencies
           #:phases
           #~(modify-phases %standard-phases
               ;; Meson calls the various executables in out/bin through the
               ;; Python interpreter, so we cannot use the shell wrapper.
               (replace 'wrap
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (substitute* (search-input-file outputs "bin/meson")
                     (("# EASY-INSTALL-ENTRY-SCRIPT")
                      (format #f "\
import sys
sys.path.insert(0, '~a')
# EASY-INSTALL-ENTRY-SCRIPT" (site-packages inputs outputs)))))))))
    (inputs (list python ninja))
    (home-page "https://mesonbuild.com/")
    (synopsis "Build system designed to be fast and user-friendly")
    (description
     "The Meson build system is focused on user-friendliness and speed.
It can compile code written in C, C++, Fortran, Java, Rust, and other
languages.  Meson provides features comparable to those of the
Autoconf/Automake/make combo.  Build specifications, also known as @dfn{Meson
files}, are written in a custom domain-specific language (@dfn{DSL}) that
resembles Python.")
    (license license:asl2.0)))

(define-public meson-python
  (package
    (name "meson-python")
    (version "0.17.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "meson_python" version))
              (sha256
               (base32
                "10szxcqgki4zwkrwmsirdg68h03k9qmfswd4r5xyz7p1y9lizfgg"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:test-flags #~(list "tests"
                                ;; The test_pep518 tries to install
                                ;; dependencies from the network using pip.
                                "-k" "not test_pep518")))
    (propagated-inputs
     (list meson
           ninja
           python-colorama
           python-cython-3
           python-pyproject-metadata
           python-tomli
           python-typing-extensions
           python-wheel))
    (native-inputs
     (list ;; For tests.
           git-minimal/pinned
           patchelf
           pkg-config
           python-cython-3
           python-gitpython
           python-pytest
           python-pytest-cov
           python-pytest-mock))
    (home-page "https://github.com/mesonbuild/meson-python")
    (synopsis "Meson-based build backend for Python")
    (description "Meson-python is a PEP 517 build backend for Meson projects.")
    (license license:expat)))

(define-public muon
  ;; Use the latest commit, as there hasn't yet been a new release including
  ;; recent changes (see: https://github.com/muon-build/muon/issues/146).
  (let ((commit "55b7285a92779bd8b8870482e5535ce878f3e09f")
        (revision "0"))
    (package
      (name "muon")
      (version (git-version "0.4.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/muon-build/muon")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0kpk1h82djb0brxkwy5ylpvdpp2l1489bq822dmryhmsd573ii48"))))
      (build-system meson-build-system)
      (arguments
       (list #:meson (computed-file "null-package" #~(mkdir #$output))
             #:ninja samu-as-ninja-wrapper
             #:configure-flags #~(list "-Dsamurai=disabled")
             #:tests? #f                  ;to avoid extra dependencies
             #:phases
             #~(modify-phases %standard-phases
                 (add-after 'unpack 'patch-/bin/sh
                   (lambda* (#:key inputs #:allow-other-keys)
                     (substitute* "tools/generate_test_check_script.py"
                       (("#!/bin/sh")
                        (string-append "#!" (search-input-file inputs
                                                               "bin/sh"))))))
                 (add-after 'patch-source-shebangs 'build-muon-bootstrap
                   (lambda _
                     (setenv "CC" #$(cc-for-target))
                     (setenv "CFLAGS" "-DBOOTSTRAP_NO_SAMU")
                     (invoke "./bootstrap.sh" "build")))
                 (add-after 'build-muon-bootstrap 'setup-muon-bootstrap-as-meson
                   (lambda _
                     (mkdir "bin")
                     (symlink "../build/muon-bootstrap" "bin/meson")
                     (setenv  "PATH" (string-append (getcwd) "/bin:"
                                                    (getenv "PATH"))))))))
      (native-inputs (list samurai))
      (inputs (list bash-minimal pkgconf))
      (native-search-paths (list $PKG_CONFIG_PATH))
      (home-page "https://muon.build/")
      (synopsis "Meson build system alternative implementation in C99")
      (description "Muon is an implementation of the meson build system in c99
with minimal dependencies.")
      (license license:gpl3))))            ;for the combined work

(define-public premake4
  (package
    (name "premake")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/premake/Premake/"
                                  version "/premake-" version "-src.zip"))
              (sha256
               (base32
                "1017rd0wsjfyq2jvpjjhpszaa7kmig6q1nimw76qx3cjz2868lrn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list unzip)) ; for unpacking the source
    (arguments
     `(#:make-flags (list (string-append "CC=" ,(cc-for-target)))
       #:tests? #f ; No test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'enter-source
           (lambda _ (chdir "build/gmake.unix") #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "../../bin/release/premake4"
                           (string-append (assoc-ref outputs "out") "/bin"))
             #t)))))
    (synopsis "Portable software build tool")
    (description "@code{premake4} is a command line utility that reads a
scripted definition of a software project and outputs @file{Makefile}s or
other lower-level build files.")
    (home-page "https://premake.github.io")
    (license license:bsd-3)))

(define-public premake5
  (package
    (inherit premake4)
    (version "5.0.0-alpha15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/premake/premake-core/"
                                  "releases/download/v" version
                                  "/premake-" version "-src.zip"))
              (sha256
               (base32
                "0lyxfyqxyhjqsb3kmx1fyrxinb26i68hb7w7rg8lajczrgkmc3w8"))))
    (arguments
     (substitute-keyword-arguments (package-arguments premake4)
       ((#:phases phases)
        `(modify-phases ,phases
           (replace 'enter-source
             (lambda _ (chdir "build/gmake2.unix") #t))
           (replace 'install
             (lambda* (#:key outputs #:allow-other-keys)
               (install-file "../../bin/release/premake5"
                             (string-append (assoc-ref outputs "out") "/bin"))
               #t))))))
    (description "@code{premake5} is a command line utility that reads a
scripted definition of a software project and outputs @file{Makefile}s or
other lower-level build files.")))

(define-public scons
  (package
    (name "scons")
    (version "4.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/SCons/scons")
                    (commit version)))
              (file-name (git-file-name name version))
              (patches (search-patches "scons-test-environment.patch"))
              (sha256
               (base32
                "1h9653965bqf8zab4gbsilsmnhp6nxn5b5b9yvm6pf401qjx8n4x"))))
    (build-system python-build-system)
    (arguments
     (list
      #:modules (append %python-build-system-modules
                        '((ice-9 ftw) (srfi srfi-26)))
      #:phases
      #~(modify-phases (@ (guix build python-build-system) %standard-phases)
          (add-after 'unpack 'adjust-hard-coded-paths
            (lambda _
              (substitute* "SCons/Script/Main.py"
                (("/usr/share/scons")
                 (string-append #$output "/share/scons")))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                ;; remove these tests as they require a read/write filesystem
                (delete-file "SCons/Variables/PathVariableTests.py")
                (invoke "python" "runtest.py" "--all" "--unit-only" ))))
          (add-after 'install 'move-manuals
            (lambda _
              ;; XXX: For some reason manuals get installed to the top-level
              ;; #$output directory.
              (with-directory-excursion #$output
                (let ((man1 (string-append #$output "/share/man/man1"))
                      (stray-manuals (scandir "."
                                              (cut string-suffix? ".1" <>))))
                  (mkdir-p man1)
                  (for-each (lambda (manual)
                              (link manual (string-append man1 "/" manual))
                              (delete-file manual))
                            stray-manuals))))))))
    (native-inputs
     ;; TODO: Add 'fop' when available in Guix to generate manuals.
     (list python-setuptools python-lxml python-wheel
           ;;For tests.
           python-psutil))
    (home-page "https://scons.org/")
    (synopsis "Software construction tool written in Python")
    (description
     "SCons is a software construction tool.  Think of SCons as an improved,
cross-platform substitute for the classic Make utility with integrated
functionality similar to autoconf/automake and compiler caches such as ccache.
In short, SCons is an easier, more reliable and faster way to build
software.")
    (license license:x11)))

(define-public scons-3
  (package
    (inherit scons)
    (version "3.0.4")
    (source (origin
             (method git-fetch)
             (uri (git-reference
                   (url "https://github.com/SCons/scons")
                   (commit version)))
             (file-name (git-file-name "scons" version))
             (sha256
              (base32
               "1xy8jrwz87y589ihcld4hv7wn122sjbz914xn8h50ww77wbhk8hn"))))
    (arguments
     `(#:use-setuptools? #f                ; still relies on distutils
       #:tests? #f                         ; no 'python setup.py test' command
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'bootstrap
           (lambda _
             (substitute* "src/engine/SCons/compat/__init__.py"
               (("sys.modules\\[new\\] = imp.load_module\\(old, \\*imp.find_module\\(old\\)\\)")
                "sys.modules[new] = __import__(old)"))
             (substitute* "src/engine/SCons/Platform/__init__.py"
               (("mod = imp.load_module\\(full_name, file, path, desc\\)")
                "mod = __import__(full_name)"))
             (invoke "python" "bootstrap.py" "build/scons" "DEVELOPER=guix")
             (chdir "build/scons")
             #t)))))
    (native-inputs '())))

(define-public scons-python2
  (package
    (inherit (package-with-python2 scons-3))
    (name "scons-python2")))

(define-public tup
  (package
    (name "tup")
    (version "0.7.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://gittup.org/tup/releases/tup-v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1157qfnhjakm3h07y7h38lrjw5650gkif34k30bnrsypmwl5xyzb"))
              (patches (search-patches "tup-unbundle-dependencies.patch"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; NOTE: Tup uses a slightly modified Lua, so it cannot be
                  ;; unbundled.  See: src/lula/tup-lua.patch
                  (delete-file-recursively "src/pcre")
                  (delete-file-recursively "src/sqlite3")
                  #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; There is a bootstrap script, but it doesn't do what you think - it
         ;; builds tup.
         (delete 'bootstrap)
         (replace 'configure
           (lambda _
             (substitute* "src/tup/link.sh"
               (("`git describe`") ,version))
             (with-output-to-file "tup.config"
               (lambda _
                 (format #t "CONFIG_TUP_USE_SYSTEM_SQLITE=y~%")))
             #t))
         (delete 'check)
         (replace 'build
           (lambda _
             ;; Based on bootstrap-nofuse.sh, but with a detour to patch-shebang.
             (invoke "./build.sh")
             (invoke "./build/tup" "init")
             (invoke "./build/tup" "generate" "--verbose" "build-nofuse.sh")
             (patch-shebang "build-nofuse.sh")
             (invoke "./build-nofuse.sh")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((outdir (assoc-ref outputs "out"))
                    (ftdetect (string-append outdir
                                             "/share/vim/vimfiles/ftdetect")))
               (install-file "tup" (string-append outdir "/bin"))
               (install-file "tup.1" (string-append outdir "/share/man/man1"))
               (install-file "contrib/syntax/tup.vim"
                             (string-append outdir "/share/vim/vimfiles/syntax"))
               (mkdir-p ftdetect)
               (with-output-to-file (string-append ftdetect "/tup.vim")
                 (lambda _
                   (display "au BufNewFile,BufRead Tupfile,*.tup setf tup")))
               #t))))))
    (inputs
     (list fuse pcre
           `(,pcre "bin") ; pcre-config
           sqlite))
    (native-inputs
     (list pkg-config))
    (home-page "https://gittup.org/tup/")
    (synopsis "Fast build system that's hard to get wrong")
    (description "Tup is a generic build system based on a directed acyclic
graphs of commands to be executed.  Tup instruments your build to detect the
exact dependencies of the commands, allowing you to take advantage of ideal
parallelism during incremental builds, and detecting any situations where
a build worked by accident.")
    (license license:gpl2)))

(define-public osc
  (package
    (name "osc")
    (version "0.172.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/openSUSE/osc")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sqdnkka3c6b6hwnrmlwrgy7w62cp8raq8mph9pgd2lydzzbvwlp"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'fix-filename
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               ;; Main osc tool is renamed in spec file, not setup.py, let's
               ;; do that too.
               (rename-file
                (string-append bin "osc-wrapper.py")
                (string-append bin "osc"))
               #t))))))
    (native-inputs
     (list python-chardet))
    (inputs
     (list python-m2crypto python-pycurl rpm))                   ; for python-rpm
    (home-page "https://github.com/openSUSE/osc")
    (synopsis "Open Build Service command line tool")
    (description "@command{osc} is a command line interface to the Open Build
Service.  It allows you to checkout, commit, perform reviews etc.  The vast
majority of the OBS functionality is available via commands and the rest can
be reached via direct API calls.")
    (license license:gpl2+)))

(define-public compdb
  (package
    (name "compdb")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Sarcasm/compdb")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f4x0gm5n1mr87dx3gzn5da16a1qhd2y3kz22dl5xsd9pd720l4w"))))
    (build-system pyproject-build-system)
    (native-inputs (list python-setuptools python-wheel))
    (propagated-inputs (list python-configparser))
    (home-page "https://github.com/Sarcasm/compdb")
    (synopsis "Compilation database Swiss army knife")
    (description
     "@command{compdb} is a command line tool to manipulate compilation
databases.  It eases the usage of tooling in a codebase by spoon-feeding the
right compilation options.")
    (license license:expat)))

(define-public compiledb
  (package
    (name "compiledb")
    (version "0.10.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "compiledb" version))
        (sha256
          (base32 "0vlngsdxfakyl8b7rnvn8h3l216lhbrrydr04yhy6kd03zflgfq6"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'no-compat-shim-dependency
           ;; shutilwhich is only needed for python 3.3 and earlier
           (lambda _
             (substitute* "setup.py" (("^ *'shutilwhich'\n") ""))
             (substitute* "compiledb/compiler.py" (("shutilwhich") "shutil")))))))
    (propagated-inputs
      (list python-bashlex python-click))
    (native-inputs
      (list python-pytest))
    (home-page
      "https://github.com/nickdiego/compiledb")
    (synopsis
      "Generate Clang JSON Compilation Database files for make-based build systems")
    (description
     "@code{compiledb} provides a @code{make} python wrapper script which,
besides executing the make build command, updates the JSON compilation
database file corresponding to that build, resulting in a command-line
interface similar to Bear.")
    (license license:gpl3)))

(define-public build
  (package
    (name "build")
    (version "0.3.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.codesynthesis.com/download/"
                           "build/" (version-major+minor version)
                           "/build-" version ".tar.bz2"))
       (sha256
        (base32 "1lx5rpnmsbip43zpp0a57sl5rm7pjb0y6i2si6rfglfp4p9d3z76"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "install_prefix=" %output))
       #:tests? #f
       #:phases (modify-phases %standard-phases
                  (delete 'build)
                  (delete 'configure))))
    (home-page "https://www.codesynthesis.com/projects/build/")
    (synopsis "Massively-parallel build system implemented on top of GNU make")
    (description "Build is a massively-parallel software build system
implemented on top of GNU Make, designed with the following tasks in mind:
@itemize
@item configuration
@item building
@item testing
@item installation
@end itemize
Build has features such as:
@itemize
@item Position-independent makefiles.
@item Non-recursive multi-makefile include-based structure.
@item Leaf makefiles are full-fledged GNU makefiles, not just variable definitions.
@item Complete dependency graph.
@item Inter-project dependency tracking.
@item Extensible language/compiler framework.
@end itemize")
    (license license:gpl2+)))

(define-public genie
  (let ((commit "22cc907a4351db46c55f73e6aa901f1b2f0c52ad")
        (revision "0"))
    (package
      (name "genie")
      (version (git-version "1170" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/bkaradzic/genie")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1wxhbdnr52qa2xr1i83577mwr25fxr5vby4r7m5brp9z5a08fwry"))))
      (build-system gnu-build-system)
      (arguments
       (list #:phases #~(modify-phases %standard-phases
                          (delete 'configure)
                          (replace 'install
                            (lambda _
                              (install-file "bin/linux/genie"
                                            (string-append #$output "/bin")))))
             #:tests? #f)) ;no tests
      (home-page "https://github.com/bkaradzic/genie")
      (synopsis "Project generator")
      (description
       "GENie generates projects from Lua scripts, making it easy to apply the
same settings to multiple projects.  It supports generating projects using GNU
Makefiles, JSON Compilation Database, and experimentally Ninja.")
      (license license:bsd-3))))

(define*-public (gnulib-checkout #:key
                                 version
                                 (revision "1")
                                 commit
                                 hash)
  "Return as a package the exact gnulib checkout."
  (package
    (name "gnulib")
    (version (git-version version revision commit))
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.savannah.gnu.org/git/gnulib.git/")
             (commit commit)))
       (file-name (git-file-name name version))
       (sha256 hash)
       (snippet
        (with-imported-modules (source-module-closure '((guix build utils)))
          #~(begin
              (use-modules (guix build utils)
                           (ice-9 ftw)
                           (ice-9 rdelim))
              ;; .c, .h and .gperf files whose first line is /* DO NOT EDIT!
              ;; GENERATED AUTOMATICALLY! */ are generated automatically based
              ;; on the unicode database. Since we replace the unicode
              ;; database with our own, we need to regenerate them. So, they
              ;; are removed from the source. They are sprinkled all over the
              ;; place unfortunately, so we can’t exclude whole directories.
              (let ((generated-automatically?
                     (lambda (filename . unused)
                       (and (or (string-suffix? ".c" filename)
                                (string-suffix? ".h" filename)
                                (string-suffix? ".gperf" filename))
                            (call-with-input-file filename
                              (lambda (port)
                                (let ((first-line (read-line port)))
                                  (equal?
                                   first-line
                                   "/* DO NOT EDIT! GENERATED AUTOMATICALLY! */"))))))))
                (for-each delete-file (find-files (getcwd) generated-automatically?)))
              ;; Other files are copied from UCD.
              (for-each delete-file
                        '("tests/unigbrk/GraphemeBreakTest.txt"
                          "tests/uninorm/NormalizationTest.txt"
                          "tests/uniname/UnicodeData.txt"
                          "tests/uniname/NameAliases.txt"
                          ;; FIXME: tests/uniname/HangulSyllableNames.txt
                          ;; seems like a UCD file but it is not distributed
                          ;; with UCD.
                          "tests/uniwbrk/WordBreakTest.txt")))))
       (patches (search-patches "gnulib-bootstrap.patch"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan
      #~'(("./gnulib-tool" "bin/")
          ("./gnulib-tool.py" "bin/")
          ("./gnulib-tool.sh" "bin/")
          ("." "src/gnulib" #:exclude-regexp ("\\.git.*")))
      #:modules '((ice-9 match)
                  (guix build utils)
                  (guix build copy-build-system)
                  ((guix build gnu-build-system) #:prefix gnu:))
      #:phases
      #~(modify-phases %standard-phases
          ;; Since this package is intended to be used in source form, it
          ;; should not retain references to tools (with the exception for the
          ;; commands we install, which should be wrapper for proper
          ;; execution).
          (delete 'patch-source-shebangs)
          (delete 'patch-generated-file-shebangs)
          (delete 'patch-usr-bin-file)
          (add-before 'install 'check
            (assoc-ref gnu:%standard-phases 'check))
          (add-before 'check 'fix-tests
            (lambda _
              (substitute* "Makefile"
                (("-f maint.mk syntax-check")
                 "_gl-Makefile=yes -f maint.mk syntax-check"))
              (invoke "git" "init")
              (invoke "git" "config" "user.name" "Guix")
              (invoke "git" "config" "user.email" "guix@localhost")
              (invoke "git" "add" ".")
              ;; Syntax checks are only run against committed files.
              (invoke "git" "commit" "-m" "Prepare for tests.")))
          (add-before 'check 'disable-failing-tests
            (lambda _
              (substitute* "cfg.mk"
                (("local-checks-to-skip =")
                 ;; sc_copyright_check fails because the fake commit date may
                 ;; be later than the copyright year.
                 "local-checks-to-skip = \\
  sc_Wundef_boolean \\
  sc_copyright_check \\
  sc_file_system \\
  sc_error_message_warn_fatal \\
  sc_indent \\
  sc_keep_gnulib_texi_files_mostly_ascii \\
  sc_prefer_angle_bracket_headers \\
  sc_prohibit_assert_without_use \\
  sc_prohibit_close_stream_without_use \\
  sc_prohibit_defined_have_decl_tests \\
  sc_prohibit_doubled_word \\
  sc_prohibit_empty_lines_at_EOF \\
  sc_prohibit_intprops_without_use \\
  sc_prohibit_openat_without_use \\
  sc_prohibit_test_minus_ao \\
  sc_readme_link_copying \\
  sc_readme_link_install \\
  sc_unportable_grep_q \\
  sc_unsigned_char \\
  sc_unsigned_int \\
  sc_unsigned_long \\
  sc_unsigned_short"))
              (substitute* "Makefile"
                (("sc_check_(sym_list|copyright|config_h_reminder)" rule)
                 (string-append "disabled_check_" rule))
                (("sc_cpp_indent_check")
                 "disabled_cpp_indent_check")
                (("sc_prefer_ac_check_funcs_once")
                 "disabled_prefer_ac_check_funcs_once")
                (("sc_prohibit_(AC_LIBOBJ_in_m4|leading_TABs\
|sc_omitted_at)" rule)
                 (string-append "disabled_prohibit_" rule)))))
          (add-before 'check 'regenerate-unicode
            (lambda* (#:key inputs #:allow-other-keys)
              (define (find-ucd-file name)
                (search-input-file inputs (string-append "share/ucd/" name)))
              (define (find-ucd-files . names)
                (map find-ucd-file names))
              (with-directory-excursion "lib"
                ;; See the compile-command buffer-local variable in
                ;; lib/gen-uni-tables.c
                (invoke "gcc" "-O" "-Wall" "gen-uni-tables.c"
                        "-Iunictype" "-o" "gen-uni-tables")
                (apply invoke
                       "./gen-uni-tables"
                       (append
                        (find-ucd-files "UnicodeData.txt"
                                        "PropList.txt"
                                        "DerivedCoreProperties.txt"
                                        "emoji/emoji-data.txt"
                                        "ArabicShaping.txt"
                                        "Scripts.txt"
                                        "Blocks.txt")
                        (list
                         #$(this-package-native-input "PropList.txt"))
                        (find-ucd-files "BidiMirroring.txt"
                                        "EastAsianWidth.txt"
                                        "LineBreak.txt"
                                        "auxiliary/WordBreakProperty.txt"
                                        "auxiliary/GraphemeBreakProperty.txt"
                                        "CompositionExclusions.txt"
                                        "SpecialCasing.txt"
                                        "CaseFolding.txt")
                         (list #$(package-version (this-package-native-input "ucd")))))
                (invoke "clisp" "-C" "uniname/gen-uninames.lisp"
                        (find-ucd-file "UnicodeData.txt")
                        "uniname/uninames.h"
                        (find-ucd-file "NameAliases.txt"))
                (for-each
                 (match-lambda
                  ((ucd-file . directory)
                   (copy-file (find-ucd-file ucd-file)
                              (string-append "../tests/" directory "/"
                                             (basename ucd-file)))))
                 '(("NameAliases.txt" . "uniname")
                   ("UnicodeData.txt" . "uniname")
                   ("NormalizationTest.txt" . "uninorm")
                   ("auxiliary/GraphemeBreakTest.txt" . "unigbrk")
                   ("auxiliary/WordBreakTest.txt" . "uniwbrk")))
                (delete-file "gen-uni-tables")))))))
    (inputs
     (list bash-minimal)) ;shebang for gnulib-tool
    (native-inputs
     (list
      bash-minimal python perl clisp
      ;; Unicode data:
      ucd
      (origin
        (method url-fetch)
        (uri (string-append
              "https://www.unicode.org/Public/"
              "3.0-Update1/PropList-3.0.1.txt"))
        (file-name "PropList.txt")
        (sha256
         (base32
          "0k6wyijyzdl5g3nibcwfm898kfydx1pqaz28v7fdvnzdvd5fz7lh")))
      ;; Programs for the tests:
      cppi indent git-minimal/pinned autoconf))
    (home-page "https://www.gnu.org/software/gnulib/")
    (synopsis "Source files to share among distributions")
    (description
     "Gnulib is a central location for common infrastructure needed by GNU
packages.  It provides a wide variety of functionality, e.g., portability
across many systems, working with Unicode strings, cryptographic computation,
and much more.  The code is intended to be shared at the level of source
files, rather than being a standalone library that is distributed, built, and
installed.  The included @command{gnulib-tool} script helps with using Gnulib
code in other packages.  Gnulib also includes copies of licensing and
maintenance-related files, for convenience.")
    (native-search-paths
     (list (search-path-specification
            (variable "GNULIB_SRCDIR")
            (files (list "src/gnulib"))
            (separator #f))))
    (license (list license:lgpl2.0+ license:gpl3+))))

(define-public gnulib
  (gnulib-checkout
   #:version "2024-05-30"
   #:commit "ac4b301ae15223c98b51cd5a0eda2e2cf57c817b"
   #:hash (base32 "0f4w56fc97clg13mmdghx84dh9xqmaqr3j672ppfh3h66gmmmvzs")))

(define-public pdpmake
  (package
    (name "pdpmake")
    (version "1.4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/rmyorston/pdpmake")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fjx5imd7s0h0yy8h2qc4vkdq7kxqcljnrw6h8n88720xha5z3cb"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:parallel-tests? #f
      #:make-flags
      #~(list "DESTDIR=\"\""
              (string-append "CC=" #$(cc-for-target))
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))
    (home-page "https://frippery.org/make/")
    (synopsis "POSIX make")
    (description
     "This package contains an implementation of POSIX make.  The default
configuration enables extensions.  Generally these extensions are compatible
with GNU make.")
    ;; pdpmake is distributed under the public domain, but the sources include
    ;; tests under the GPL license version 2.
    (license (list license:gpl2 license:public-domain))))

(define-public potato-make
  ;; No releases.
  (let ((commit "e8c09ce1f6a33c013b27961b0a07f991db33e6fb")
        (revision "0"))
    (package
      (name "potato-make")
      (version (git-version "0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/spk121/potato-make")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0axgrkqdfip5f2bp7d2dprd74g58lyc0c7lgg2m93k5jqn1lpbmj"))
         ;; Delete files of the seemingly unfinished pmake program.
         (snippet '(begin
                     (delete-file "pmake")
                     (delete-file "documents/pmake.org")
                     (delete-file "make/main.scm")
                     (rmdir "documents")
                     (rmdir "make")))))
      (build-system guile-build-system)
      (arguments
       (list
        #:phases #~(modify-phases %standard-phases
                     (add-after 'build 'check
                       (lambda _
                         ;; Delete test referencing nonexistent file.
                         (delete-file "tests/parse.sh")
                         (for-each (lambda (f)
                                     (invoke "guile" "-L" "." "-s" f))
                                   (find-files "tests")))))))
      (native-inputs (list guile-3.0))
      (home-page "https://github.com/spk121/potato-make")
      (synopsis "Library to write makefiles in Guile Scheme")
      (description
       "Potato Make is a Scheme library that aims to simplify the task of
maintaining, updating, and regenerating programs.  It is inspired by
the POSIX make utility and allows writing a build script in Guile
Scheme.")
      (license license:expat))))
