;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Zhu Zihao <all_but_last@163.com>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages mold)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages compression)
  #:use-module ((guix licenses) #:prefix license:))

(define-public mold
  (package
    (name "mold")
    (version "2.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rui314/mold")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rqw7p61qijxhbfm887xbh8idbp5w30axvwgmm68s03xirnr7ymr"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            (for-each
             (lambda (x)
               (delete-file-recursively (string-append "third-party/" x)))
             '("mimalloc" "tbb" "xxhash" "zlib" "zstd"))))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DMOLD_USE_SYSTEM_MIMALLOC=ON"
                                "-DMOLD_USE_SYSTEM_TBB=ON"
                                "-DBUILD_TESTING=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'force-system-xxhash
            (lambda _
              (substitute* "common/common.h"
                (("#include \"../third-party/xxhash/xxhash.h\"")
                 "#include <xxhash.h>"))))
          (add-before 'configure 'fix-compiler-name-in-test
            (lambda _
              (substitute* "test/elf/common.inc"
                (("CC=\"\\$\\{TEST_CC:-cc\\}\"") "CC=gcc")
                (("CXX=\"\\$\\{TEST_CXX:-c\\+\\+\\}\"")
                 "CXX=g++"))))
          (add-before 'configure 'skip-tbb-lto-test
            (lambda _
              ;; This test needs tbb 2021.9.0 or newer
              (delete-file "test/elf/lto-version-script.sh")))
          (add-before 'configure 'disable-rpath-test
            (lambda _
              ;; This test fails because mold expect the RUNPATH as-is,
              ;; but compiler in Guix will insert the path of gcc-lib and
              ;; glibc into the output binary.
              (delete-file "test/elf/rpath.sh"))))))
    (inputs (list mimalloc tbb xxhash zlib `(,zstd "lib")))
    (home-page "https://github.com/rui314/mold")
    (synopsis "Fast linker")
    (description
     "Mold is a faster drop-in replacement for existing linkers.
It is designed to increase developer productivity by reducing build time,
especially in rapid debug-edit-rebuild cycles.")
    (license license:agpl3)))

(define* (make-mold-wrapper mold #:key mold-as-ld?)
  "Return a MOLD wrapper.  When MOLD-AS-LD? is true, create a 'ld' symlink that
points to 'mold'."
  (package
    (inherit mold)
    (name (if mold-as-ld? "mold-as-ld-wrapper" "mold-wrapper"))
    (source #f)
    (native-inputs '())
    (inputs (list (make-ld-wrapper "ld.mold-wrapper" #:binutils mold
                                   #:linker "ld.mold")
                  (make-ld-wrapper "mold-wrapper" #:binutils mold #:linker
                                   "mold")))
    (propagated-inputs '())
    (build-system trivial-build-system)
    (arguments
     (list #:builder
           #~(let ((ld.mold (string-append #$(this-package-input
                                              "ld.mold-wrapper")
                                           "/bin/ld.mold"))
                   (mold (string-append #$(this-package-input "mold-wrapper")
                                        "/bin/mold")))
               (mkdir #$output)
               (mkdir (string-append #$output "/bin"))
               (symlink ld.mold (string-append #$output "/bin/ld.mold"))
               (symlink mold (string-append #$output "/bin/mold"))
               (when #$mold-as-ld?
                 (symlink ld.mold (string-append #$output "/bin/ld"))))))
    (synopsis "Mold linker wrapper")
    (description "This is a linker wrapper for Mold; like @code{ld-wrapper}, it
wraps the linker to add any missing @code{-rpath} flags, and to detect any
misuse of libraries outside of the store.")))

(define-public mold-wrapper
  (make-mold-wrapper mold))

(define-public mold-as-ld-wrapper
  (make-mold-wrapper mold #:mold-as-ld? #t))
