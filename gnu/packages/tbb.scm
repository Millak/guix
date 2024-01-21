;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015, 2016, 2022, 2024 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022, 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages tbb)
  #:use-module (guix packages)
  #:use-module (guix licenses)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig))

(define-public tbb
  (package
    (name "tbb")
    (version "2021.6.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oneapi-src/oneTBB")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "039v4jmnkkxs7haxrfmk9j57vfbrwlhjynlm5byfaqddv4cbsy0p"))
              (patches (search-patches "tbb-other-arches.patch"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       '(,@(if (or (target-riscv64?)
                   (target-ppc32?))
               '("-DTBB_TEST_LINK_FLAGS=-latomic")
               `())
         ,@(if (or (target-arm32?)
                   (target-ppc32?))
               '("-DTBB_TEST_COMPILE_FLAGS=-DTBB_TEST_LOW_WORKLOAD")
               `())
         "-DTBB_STRICT=OFF") ;; Don't fail on warnings
       #:phases
       (modify-phases %standard-phases
         ,@(cond
            ((target-arm32?)
             `((add-after 'unpack 'adjust-test-suite
                 (lambda _
                   (substitute* "test/CMakeLists.txt"
                     ;; Bus error, skipped on mips.
                     ((".*test_malloc_pools.*") ""))))))
            ((target-ppc32?)
             `((add-after 'unpack 'adjust-test-suite
                 (lambda _
                   (substitute* "test/CMakeLists.txt"
                     ;; These tests hang forever.
                     ((".*test_function_node.*") "")
                     ((".*test_multifunction_node.*") "")
                     ((".*test_async_node.*") ""))))))
            (else '())))))
    (home-page "https://www.threadingbuildingblocks.org")
    (synopsis "C++ library for parallel programming")
    (description
     "Threading Building Blocks (TBB) is a C++ runtime library that abstracts
the low-level threading details necessary for optimal multi-core performance.
It uses common C++ templates and coding style to eliminate tedious threading
implementation work.  It provides parallel loop constructs, asynchronous
tasks, synchronization primitives, atomic operations, and more.")
    (license asl2.0)))

(define-public python-tbb
  (package
    (inherit tbb)
    (name "python-tbb")
    (arguments
     (list
      #:configure-flags
      #~(list "-DTBB_STRICT=OFF"
              "-DTBB4PY_BUILD=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-python-install-directory
            (lambda _
              (substitute* "python/CMakeLists.txt"
                (("\\$\\{PYTHON_BUILD_WORK_DIR\\}/build")
                 #$output)
                (("install --prefix.*-f" m)
                 (string-append m " --root=/")))
              (substitute* "python/setup.py"
                (("extra_link_args=tbb_flag,")
                 (string-append "extra_link_args=['-Wl,-rpath="
                                #$(this-package-input "tbb") "/lib"
                                "', '-Wl,-rpath=" #$output "/lib'] + tbb_flag,")))))
          (replace 'build
            (lambda _
              (setenv "PYTHONHASHSEED" "0")
              (setenv "PYTHONDONTWRITEBYTECODE" "1")
              (invoke "make" "python_build")))
          ;; The 'build phase already installs the modules
          (replace 'install
            (lambda _
              (with-directory-excursion "python/rml"
                (invoke "make" "install"))))
          (replace 'check
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (invoke "ctest" "-R" "python_test" "--output-on-failure")))))))
    (inputs (list python tbb))
    (native-inputs (list swig))))

(define-public tbb-2020
  (package
    (name "tbb")
    (version "2020.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/01org/tbb")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r9axsdlmacjlcnax4vkzg86nwf8lsx7wbqdi3wnryaxk0xvdcx6"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "build/common.inc"
                    (("export tbb_build_prefix.+$")
                     "export tbb_build_prefix?=guix\n"))

                  ;; Don't capture the build time and kernel version.
                  (substitute* "build/version_info_linux.sh"
                    (("uname -srv") "uname -s")
                    (("`date -u`") "01 Jan 1970"))

                  (substitute* "build/linux.inc"
                    (("os_kernel_version:=.*")
                     "os_kernel_version:=5\n")
                    (("os_version:=.*")
                     "os_version:=1\n"))))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib")
                          "CFLAGS=-fuse-ld=gold")
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fail-on-test-errors
           (lambda _
             (substitute* "Makefile"
               (("-\\$\\(MAKE") "$(MAKE"))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "build/linux.gcc.inc"
               (("LIB_LINK_FLAGS =")
                (string-append "LIB_LINK_FLAGS = -Wl,-rpath="
                               (assoc-ref outputs "out") "/lib")))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((doc      (string-append
                               (assoc-ref outputs "doc") "/doc"))
                    (examples (string-append doc "/examples"))
                    (lib      (string-append
                               (assoc-ref outputs "out") "/lib"))
                    (include  (string-append
                               (assoc-ref outputs "out") "/include")))
               (mkdir-p lib)
               (for-each
                (lambda (f)
                  (copy-file f
                             (string-append lib "/"
                                            (basename f))))
                (find-files "build/guix_release" "\\.so"))
               (copy-recursively "doc" doc)
               (copy-recursively "examples" examples)
               (copy-recursively "include" include)))))))
    (native-inputs
     ;; XXX: For some reason, since commit "gnu: binutils: Absorb
     ;; binutils-next", the build of just this version of TBB crashes during
     ;; tests.  Workaround it by linking the binaries with ld.gold.
     (list (module-ref (resolve-interface
                        '(gnu packages commencement))
                       'ld-gold-wrapper)))
    (home-page "https://www.threadingbuildingblocks.org")
    (synopsis "C++ library for parallel programming")
    (description
     "Threading Building Blocks (TBB) is a C++ runtime library that abstracts
the low-level threading details necessary for optimal multi-core performance.
It uses common C++ templates and coding style to eliminate tedious threading
implementation work.  It provides parallel loop constructs, asynchronous
tasks, synchronization primitives, atomic operations, and more.")
    (license asl2.0)))
