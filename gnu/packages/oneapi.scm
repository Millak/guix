;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Cayetano Santos <csantosb@inventati.org>
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

(define-module (gnu packages oneapi)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (guix build-system cmake)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils))

;;; Updates and replaces tbb in (gnu packages tbb)
(define-public onetbb
  (package
    (name "onetbb")
    (version "2022.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/uxlfoundation/oneTBB/")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "04hjgc0yg0kdwr5qssl4y7hqv4wgcrlmblvbiaxqlyxrd400y901"))
       (patches (search-patches "onetbb-other-arches.patch"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list #$@(if (or (target-riscv64?)
                         (target-ppc32?))
                     '(list "-DTBB_TEST_LINK_FLAGS=-latomic")
                     '())
              #$@(if (or (target-arm32?)
                         (target-ppc32?))
                     '("-DTBB_TEST_COMPILE_FLAGS=-DTBB_TEST_LOW_WORKLOAD")
                     '())
              ;; Don't fail on warnings.
              "-DTBB_STRICT=OFF")
      #:phases
      #~(modify-phases %standard-phases
          #$@(cond
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
              ((target-riscv64?)
               `((add-after 'unpack 'adjust-test-suite
                   (lambda _
                     (substitute* "test/CMakeLists.txt"
                       ;; This tests hangs forever.
                       ((".*test_task_group.*") ""))))))
              (else '())))))
    (home-page "https://uxlfoundation.github.io/oneTBB/")
    (synopsis "C++ library for parallel programming")
    (description
     "@acronym{OneTBB, OneAPI Threading Building Blocks} is a C++ runtime
library that abstracts the low-level threading details necessary for optimal
multi-core performance.  It uses common C++ templates and coding style to
eliminate tedious threading implementation work.  It provides parallel loop
constructs, asynchronous tasks, synchronization primitives, atomic operations,
and more.")
    (license license:asl2.0)))

(define-public python-onetbb
  (package
    (inherit onetbb)
    (name "python-onetbb")
    (arguments
     (list
      #:configure-flags
      #~(list "-DTBB4PY_BUILD=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-python-install-directory
            (lambda _
              (substitute* "python/setup.py"
                (("extra_link_args=tbb_flag,")
                 (string-append
                  "extra_link_args=['-Wl,-rpath="
                  #$(this-package-input "onetbb") "/lib"
                  "', '-Wl,-rpath=" #$output "/lib'] + tbb_flag,")))))
          (replace 'build
            (lambda _
              (setenv "PYTHONHASHSEED" "0")
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
    (native-inputs (list swig python-minimal))
    (inputs (list onetbb))
    (synopsis "Python bindings for the oneTBB parallel library")
    (description
     "@acronym{OneTBB, OneAPI Threading Building Blocks} is a C++ runtime
library that abstracts the low-level threading details necessary for optimal
multi-core performance.  It uses common C++ templates and coding style to
eliminate tedious threading implementation work.  It provides parallel loop
constructs, asynchronous tasks, synchronization primitives, atomic operations,
and more.  @code{python-onetbb} enables threading composability between two or
more thread-enabled Python libraries.")))
