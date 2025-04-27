;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Cyril Roelandt <tipecaml@gmail.com>
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014 Ian Denhardt <ian@zenhack.net>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017, 2018, 2020, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019-2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Pierre-Moana Levesque <pierre.moana.levesque@gmail.com>
;;; Copyright © 2020, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Simon South <simon@simonsouth.net>
;;; Copyright © 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2024 dan <i@dan.games>
;;; Copyright © 2024 Charles <charles@charje.net>
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

(define-module (gnu packages cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix deprecation)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system emacs)
  #:use-module (guix build-system python)
  #:use-module ((guix search-paths) #:select ($SSL_CERT_DIR $SSL_CERT_FILE))
  #:use-module (gnu packages)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages file)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xml)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1))

(define-public cmake-shared
  (let ((commit "8122f2b96c8da38ea41b653cf69958e75fe2129d")
        (revision "32"))
    (package
      (name "cmake-shared")
      (version
       (git-version "1.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri
          (git-reference
           (url "https://github.com/lirios/cmake-shared.git")
           (commit commit)))
         (file-name
          (git-file-name name version))
         (sha256
          (base32 "05avwzqcnliwx9h7qi1kl0iz4smqmxc4vkavyjbmlc6h2b97i58g"))
         (modules '((guix build utils)
                    (ice-9 ftw)
                    (srfi srfi-1)))
         (snippet
          `(begin
             (delete-file-recursively "3rdparty")))))
      (build-system cmake-build-system)
      (arguments
       `(#:tests? #f))                  ; No target
      (native-inputs
       (list extra-cmake-modules))
      (synopsis "Shared CMake functions and macros")
      (description "CMake-Shared are shared functions and macros for projects
using the CMake build system.")
      (home-page "https://github.com/lirios/cmake-shared/")
      (license license:bsd-3))))

;;; Build phases shared between 'cmake-bootstrap' and the later variants
;;; that use cmake-build-system.
(define (%common-build-phases)
  `((add-after 'unpack 'split-package
      ;; Remove files that have been packaged in other package recipes.
      (lambda _
        (delete-file "Auxiliary/cmake-mode.el")
        (substitute* "Auxiliary/CMakeLists.txt"
          ((".*cmake-mode.el.*") ""))))
    ,@(if (target-x86-32?)
          '((add-after 'unpack 'skip-cpack-txz-test
              (lambda _
                ;; In 'RunCMake.CPack_TXZ', the 'TXZ/THREADED_ALL' test
                ;; would occasionally fail on i686 with "Internal error
                ;; initializing compression library: Cannot allocate
                ;; memory": <https://issues.guix.gnu.org/50617>.  Skip it.
                (substitute* "Tests/RunCMake/CPack/RunCMakeTest.cmake"
                  (("THREADED_ALL \"TXZ;DEB\"")
                   "THREADED_ALL \"DEB\"")))))
          '())
    (add-before 'configure 'patch-bin-sh
      (lambda _
        ;; Replace "/bin/sh" by the right path in... a lot of
        ;; files.
        (substitute*
            '("Modules/CompilerId/Xcode-3.pbxproj.in"
              "Modules/Internal/CPack/CPack.RuntimeScript.in"
              "Source/cmGlobalXCodeGenerator.cxx"
              "Source/cmLocalUnixMakefileGenerator3.cxx"
              "Source/cmExecProgramCommand.cxx"
              "Tests/CMakeLists.txt"
              "Tests/RunCMake/File_Generate/RunCMakeTest.cmake")
          (("/bin/sh") (which "sh")))))))

(define %common-disabled-tests
  '(;; This test copies libgcc_s.so.1 from GCC and tries to modify its RPATH,
    ;; but does not cope with the file being read-only.
    "BundleUtilities"
    ;; These tests require network access.
    "CTestTestUpload" "CMake.FileDownload"
    ;; This test requires 'ldconfig' which is not available in Guix.
    "RunCMake.install"
    ;; This test fails for unknown reason.
    "RunCMake.file-GET_RUNTIME_DEPENDENCIES"))

(define %common-disabled-tests/hurd
  '("CTestTestTimeout"
    "CTestTestRerunFailed"
    "RunCMake.CompilerChange"
    "RunCMake.ctest_test"
    "RunCMake.file"
    "RunCMake.BundleUtilities"
    "RunCMake.configure_file"
    "RunCMake.CTestTimeout"
    "RunCMake.CTestTimeoutAfterMatch"
    "RunCMake.CommandLine"
    "RunCMake.CTestCommandLine"))

(define %preserved-third-party-files
  '(;; 'Source/cm_getdate.c' includes archive_getdate.c wholesale, so it must
    ;; be available along with the required headers.
    "Utilities/cmlibarchive/libarchive/archive_getdate.c"
    "Utilities/cmlibarchive/libarchive/archive_getdate.h"
    ;; ELF headers.
    "Utilities/cmelf"
    ;; CMake header wrappers.
    "Utilities/cm3p"))

;;; The "bootstrap" CMake.  It is used to build 'cmake-minimal' below, as well
;;; as any dependencies that need cmake-build-system.
(define-public cmake-bootstrap
  (package
    (name "cmake-bootstrap")
    (version "3.24.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "1ny8y2dzc6fww9gzb1ml0vjpx4kclphjihkxagxigprxdzq2140d"))
              (patches (search-patches "cmake-curl-certificates-3.24.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:test-target "test"
      #:configure-flags
      #~(let ((parallel-job-count (number->string (parallel-job-count))))
          (list "--verbose"
                (string-append "--parallel=" parallel-job-count)
                (string-append "--prefix=" #$output)
                "--system-libs"
                ;; By default, the man pages and other docs land
                ;; in PREFIX/man and PREFIX/doc, but we want them
                ;; in share/{man,doc}.  Note that unlike
                ;; autoconf-generated configure scripts, cmake's
                ;; configure prepends "PREFIX/" to what we pass
                ;; to --mandir and --docdir.
                "--mandir=share/man"
                (string-append "--docdir=share/doc/cmake-"
                               #$(version-major+minor version))

                ;; By default CMake is built without any optimizations.  Use
                ;; the recommended Release target for a ~2.5x speedup.
                "--" "-DCMAKE_BUILD_TYPE=Release"))
      #:make-flags
      #~(let ((skipped-tests
               (list #$@%common-disabled-tests
                     "CTestTestSubdir" ; This test fails to build 2 of the 3 tests.
                     ;; This test fails when ARGS (below) is in use, see
                     ;; <https://gitlab.kitware.com/cmake/cmake/issues/17165>.
                     "CTestCoverageCollectGCOV"
                     #$@(if (target-hurd?)
                            %common-disabled-tests/hurd
                            #~()))))
          (list
           (string-append
            ;; These arguments apply for the tests only.
            "ARGS=-j " (number->string (parallel-job-count))
            " --output-on-failure"
            " --exclude-regex ^\\(" (string-join skipped-tests "\\|") "\\)$")))
      #:phases
      #~(modify-phases %standard-phases
          #$@(%common-build-phases)
          (add-before 'configure 'set-paths
            (lambda _
              ;; Help cmake's bootstrap process to find system libraries
              (begin
                (setenv "CMAKE_LIBRARY_PATH" (getenv "LIBRARY_PATH"))
                (setenv "CMAKE_INCLUDE_PATH" (or (getenv "CPATH")
                                                 (getenv "C_INCLUDE_PATH"))))))
          ;; CMake uses its own configure script.
          (replace 'configure
            (lambda* (#:key (configure-flags '()) #:allow-other-keys)
              (apply invoke "./configure" configure-flags)))
          #$@(if (target-hurd?)
                 #~((add-after 'unpack 'patch-hurd
                      (lambda _
                        ;; Version 3.25.0 has a similar fix.
                        (substitute* "Utilities/cmlibuv/src/unix/udp.c"
                          (("!defined\\(__QNX__\\)")
                           "!defined(__GNU__)")))))
                 #~()))))
    (inputs
     (list libuv bzip2 curl expat file jsoncpp libarchive rhash zlib))
    (native-search-paths
     (list (search-path-specification
            (variable "CMAKE_PREFIX_PATH")
            (files '("")))
           ;; "cmake-curl-certificates.patch" changes CMake to honor 'SSL_CERT_DIR'
           ;; and 'SSL_CERT_FILE', hence these search path entries.
           $SSL_CERT_DIR
           $SSL_CERT_FILE))
    (home-page "https://cmake.org/")
    (synopsis "Cross-platform build system")
    (description
     "CMake is a family of tools designed to build, test and package software.
CMake is used to control the software compilation process using simple platform
and compiler independent configuration files.  CMake generates native makefiles
and workspaces that can be used in the compiler environment of your choice.")
    (properties '((hidden? . #t)))
    (license (list license:bsd-3        ; cmake
                   license:expat        ; cmjsoncpp is dual MIT/public domain
                   license:public-domain)))) ; cmlibarchive/archive_getdate.c

;;; This minimal variant of CMake does not include the documentation.  It is
;;; used by the cmake-build-system.
(define-public cmake-minimal
  (package
    (inherit cmake-bootstrap)
    (name "cmake-minimal")
    (properties (alist-delete 'hidden? (package-properties cmake-bootstrap)))
    (source (origin
              (inherit (package-source cmake-bootstrap))
              ;; Purge CMakes bundled dependencies as they are no longer needed.
              (modules '((ice-9 ftw)))
              (snippet
               `(begin
                  (define preserved-files ',%preserved-third-party-files)

                  (file-system-fold (lambda (dir stat result)         ;enter?
                                      (or (string=? "Utilities" dir)  ;init
                                          ;; The bundled dependencies are
                                          ;; distinguished by having a "cm"
                                          ;; prefix to their upstream names.
                                          (and (string-prefix? "Utilities/cm" dir)
                                               (not (member dir preserved-files)))))
                                    (lambda (file stat result)        ;leaf
                                      (unless (or (member file preserved-files)
                                                  ;; Preserve top-level files.
                                                  (string=? "Utilities"
                                                            (dirname file)))
                                        (delete-file file)))
                                    (const #t)                        ;down
                                    (lambda (dir stat result)         ;up
                                      (when (equal? (scandir dir) '("." ".."))
                                        (rmdir dir)))
                                    (const #t)                        ;skip
                                    (lambda (file stat errno result)
                                      (format (current-error-port)
                                              "warning: failed to delete ~a: ~a~%"
                                              file (strerror errno)))
                                    #t
                                    "Utilities"
                                    lstat)
                  #t))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DCMAKE_USE_SYSTEM_LIBRARIES=ON"
              (string-append "-DCMAKE_DOC_DIR=share/doc/cmake-"
                             #$(version-major+minor (package-version
                                                     cmake-bootstrap))))

      ;; This is the CMake used in cmake-build-system.  Ensure compiler
      ;; optimizations are enabled to save size and CPU cycles.
      #:build-type "Release"
      #:phases
      #~(modify-phases %standard-phases
          #$@(%common-build-phases)
          (add-after 'install 'delete-help-documentation
            (lambda _
              (delete-file-recursively
               (string-append #$output
                              "/share/cmake-"
                              #$(version-major+minor
                                 (package-version cmake-bootstrap))
                              "/Help"))))
          (replace 'check
            (lambda* (#:key tests? parallel-tests? #:allow-other-keys)
              (let ((skipped-tests (list #$@%common-disabled-tests
                                         ;; This test requires the bundled libuv.
                                         "BootstrapTest"
                                         #$@(if (system-hurd?)
                                                %common-disabled-tests/hurd
                                                #~()))))
                (if tests?
                    (begin
                      (invoke "ctest" "-j" (if parallel-tests?
                                               (number->string (parallel-job-count))
                                               "1")
                              "--exclude-regex"
                              (string-append "^(" (string-join skipped-tests "|") ")$")))
                    (format #t "test suite not run~%"))))))
      #:cmake (if (%current-target-system)
                  cmake-minimal-cross
                  cmake-bootstrap)))))

;;; The "user-facing" CMake, now with manuals and HTML documentation.
(define-public cmake
  (package
    (inherit cmake-minimal)
    (name "cmake")
    (version "3.25.1")
    (source (origin
              (inherit (package-source cmake-minimal))
              (method url-fetch)
              (uri (string-append "https://cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (snippet (match (origin-snippet (package-source cmake-minimal))
                         (('begin ('define 'preserved-files ('quote x))
                                  rest ...)
                          `(begin (define preserved-files
                                    ',(cons "Utilities/cmelf" x))
                                  ,@rest))))
              (sha256
               (base32
                "1n4inb3fvk70sni5gmkljqw3cyllalyg3fnr9rlr7x3aa44isl8w"))
              (patches (search-patches "cmake-curl-certificates-3.24.patch"))))
    (outputs '("out" "doc"))
    (arguments
     (substitute-keyword-arguments (package-arguments cmake-minimal)
       ;; Use cmake-minimal this time.
       ((#:cmake _ #f)
        (if (%current-target-system)
            cmake-minimal-cross
            cmake-minimal))

       ;; Enable debugging information for convenience.
       ((#:build-type _ #f) "RelWithDebInfo")

       ((#:configure-flags flags ''())
        #~(append (list "-DSPHINX_INFO=ON" "-DSPHINX_MAN=ON" "-DSPHINX_HTML=ON"
                        (string-append "-DCMAKE_DOC_DIR=share/doc/cmake-"
                                       #$(version-major+minor (package-version
                                                               cmake-minimal)))
                        (string-append "-DCMake_INSTALL_VIMFILES_DIR=" #$output
                                       "/share/vim/vimfiles/pack/guix/start/cmake")
                        "-DCMAKE_INFO_DIR=share/info"
                        "-DCMAKE_MAN_DIR=share/man")
                  #$flags))
       ((#:phases phases)
        #~(modify-phases #$phases
            (delete 'delete-help-documentation)
            (add-after 'install 'move-html-doc
              (lambda _
                (let ((html (string-append "/share/doc/cmake-"
                                           #$(version-major+minor
                                              (package-version cmake-minimal))
                                           "/html")))
                  (copy-recursively (string-append #$output html)
                                    (string-append #$output:doc html))
                  (delete-file-recursively (string-append #$output html)))))))))
    (inputs
     (modify-inputs (package-inputs cmake-minimal)
       (prepend ncurses)))              ;required for ccmake
    ;; Extra inputs required to build the documentation.
    (native-inputs
     (modify-inputs (package-native-inputs cmake-minimal)
       (append python-sphinx
               texinfo)))
    (properties (alist-delete 'hidden? (package-properties cmake-minimal)))))

(define-public cmake-next
  (package
    (inherit cmake)
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://cmake.org/files/v"
                                  (version-major+minor version)
                                  "/cmake-" version ".tar.gz"))
              (sha256
               (base32
                "1q9119wg68vz3ki4g3yw3dkb90zpbr13vy2raar21rb31vhafc6n"))))
    (native-inputs
     (modify-inputs (package-native-inputs cmake)
       ;; Avoid circular dependency with (gnu packages debug).  Note: cppdap
       ;; is built with cmake, so when the default cmake is updated to this
       ;; version this circular dependency will need to be worked around.
       (prepend (module-ref (resolve-interface '(gnu packages debug))
                            'cppdap))))))

(define-public cmake-minimal-cross
  (package
    (inherit cmake-minimal)
    (name "cmake-minimal-cross")
    (native-search-paths '())
    (search-paths
     (package-native-search-paths cmake-minimal))))

(define-public cmakelang
  (package
    (name "cmakelang")
    (version "0.6.13")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "cmakelang" version))
       (sha256
        (base32 "0zz6g1ignqanl4ja9f5nrlk5f3mvv7cp5y9yswjd0m06n23jx603"))))
    (build-system python-build-system)
    (arguments (list #:tests? #f        ;no test data in pypi archive
                     #:phases #~(modify-phases %standard-phases
                                  (add-after 'unpack 'adjust-setup.py
                                    (lambda _
                                      (substitute* "setup.py"
                                        (("cmakelang/doc/README.rst")
                                         "README.rst")))))))
    (inputs (list python-jinja2 python-pyyaml python-six))
    (home-page "https://github.com/cheshirekow/cmake_format/")
    (synopsis "Language tools for CMake (format, lint, etc.)")
    (description "The cmakelang project provides quality assurance (QA) tools
for CMake:
@table @command
@item cmake-annotate
generate pretty HTML from your listfiles
@item cmake-format
format your listfiles nicely
@item cmake-lint
check your listfiles for problems
@item ctest-to
parse a ctest output tree and translate it into a more structured
format (either JSON or XML).
@end table")
    (license license:gpl3+)))

(define-public corrosion
  (package
    (name "corrosion")
    (version "0.5.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/corrosion-rs/corrosion")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32 "1bylfjji4yw88r00hgb69nfl9lz73bhc7q3n64myif4alr4b8ypx"))))
    (build-system cmake-build-system)
    (arguments
     (list
       #:phases
       #~(modify-phases %standard-phases
           (replace 'check
             (lambda* (#:key tests? #:allow-other-keys)
               (when tests?
                 (invoke "ctest" "-E"
                         (string-append
                           "(" (string-join
                                 (list "cbindgen_rust2cpp"
                                       "rustup_proxy"
                                       "hostbuild"
                                       "parse_target_triple")
                                 "|")
                           ")"))))))))
    (native-inputs
     (list rust
           `(,rust "cargo")))
    (home-page "https://corrosion-rs.github.io/corrosion/")
    (synopsis "Tool for integrating Rust into an existing CMake project")
    (description "Corrosion, formerly known as cmake-cargo, is a tool for
integrating Rust into an existing CMake project.  Corrosion can automatically
import executables, static libraries, and dynamic libraries from a workspace
or package manifest (Cargo.toml file).")
    (license license:expat)))

(define-public emacs-cmake-mode
  (package
    (inherit cmake)
    (name "emacs-cmake-mode")
    (native-inputs '())
    (inputs '())
    (outputs '("out"))
    (build-system emacs-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'chdir-elisp
                 ;; Elisp directory is not in root of the source.
                 (lambda _
                   (chdir "Auxiliary"))))))
    (synopsis "Emacs major mode for editing Cmake expressions")
    (description "@code{cmakeos-mode} provides an Emacs major mode for editing
Cmake files.  It supports syntax highlighting, indenting and refilling of
comments.")))

(define-public qmsetup
  (let ((commit "89fa57046241c26dfcfd97ceba174728b24bdd27")
        (revision "0"))
    (package
      (name "qmsetup")
      ;; The base version string is retrieved from the CMakeLists.txt file.
      (version (git-version "0.0.1.5" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/stdware/qmsetup")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0nqpblv08yqv97vjv7cxkpf160s3877gnd7jjqxnfrrknm2396r1"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:tests? #f                     ;no test suite
        #:phases #~(modify-phases %standard-phases
                     (add-after 'unpack 'patch-paths
                       (lambda* (#:key inputs #:allow-other-keys)
                         (substitute* "src/corecmd/utils_unix.cpp"
                           (("\"patchelf\"")
                            (format #f "~s" (search-input-file
                                             inputs "bin/patchelf")))))))))
      (inputs (list patchelf syscmdline))
      (home-page "https://github.com/stdware/qmsetup")
      (synopsis "CMake modules and basic libraries for C/C++ projects")
      (description "QMSetup is a set of CMake Modules and Basic Libraries for
C/C++ projects.  It features:
@itemize
@item Helpful CMake utilities
@item Generate configuration header files
@item Reorganize header files
@item Deploy project dependencies and fix rpaths
@item Support calling Doxygen via CMake conveniently
@item Support calling Qt Linguist Tools via CMake conveniently
@end itemize")
      (license license:expat))))

(define-public tinycmmc
  (package
    (name "tinycmmc")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Grumbel/tinycmmc")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0chv7h6vnd8zpa6b69krxwdgvp3n5fd37355wa1zwi14x4nyyay5"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;no test suite
    (home-page "https://github.com/Grumbel/tinycmmc")
    (synopsis "Tiny CMake Module Collections")
    (description "The tinycmmc package contains a small collection of reusable
CMake modules.")
    (license license:zlib)))

(define-public cpm-cmake
  (package
    (name "cpm-cmake")
    (version "0.38.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cpm-cmake/CPM.cmake")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qbbhdq6cz2y7qfyy1k11i98d13s229r3phd5y3n5maq51ky8bgb"))))
    (build-system copy-build-system)
    (arguments
     (list
      #:install-plan #~'(("cmake/CPM.cmake" "lib/cmake/CPM.cmake"))))
    (home-page "https://github.com/cpm-cmake/CPM.cmake")
    (synopsis "Package manager for CMake")
    (description "CPM.cmake is a cross-platform CMake script that adds
dependency management capabilities to CMake.")
    (license license:expat)))

(define-public sanitizers-cmake
  (let ((commit "0573e2ea8651b9bb3083f193c41eb086497cc80a")
        (revision "0"))
    (package
      (name "sanitizers-cmake")
      (version (git-version "0.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/arsenm/sanitizers-cmake")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "1sqjmx65iif67k10jwlf1j8p279rsniy1i3ff660hfrynr0knlry"))))
      (build-system cmake-build-system)
      (arguments
       (list
        #:configure-flags
        #~(list
           ;; Otherwise optimizer will optimize away our faulty thing.
           "-DCMAKE_BUILD_TYPE=Debug")
        #:phases
        #~(modify-phases %standard-phases
            ;; No install target provided; manually copy files to a suitable
            ;; folder in the output.
            (replace 'install
              (lambda* (#:key source #:allow-other-keys)
                (copy-recursively
                 (string-append source "/cmake")
                 (string-append #$output "/share/" #$name "/cmake")))))))
      (synopsis "CMake module to enable sanitizers for binary targets")
      (description "@code{sanitizers-cmake} provides a module for the CMake
build system that can enable address, memory, thread and undefined-behavior
sanitizers for binary targets using flags appropriate for the compiler in
use.")
      (home-page "https://github.com/arsenm/sanitizers-cmake")
      (license license:expat))))
