;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2024 Li-cheng (Andy) Tai <atai@atai.org>
;;
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

(define-module (gnu packages squirrel)
  #:use-module (gnu packages)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages sphinx)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build utils)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public squirrel
  (package
    (name "squirrel")
    (version "3.2")
    (source (origin
              (method git-fetch)
               (uri (git-reference
                     (url "https://github.com/albertodemichelis/squirrel.git")
                     (commit (string-append "v" version))))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "028v90k5bbcb8qwysgv6r0ycy6g920ns32i2sdq0i8hqib90ac5z"))))
    (build-system cmake-build-system)
    (arguments
     '(#:configure-flags '("-DDISABLE_STATIC=ON")
       #:tests? #f ; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-documentation
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((source (assoc-ref %build-inputs "source"))
                    (out (assoc-ref outputs "out"))
                    (doc-dir (string-append out "/share/doc/squirrel")))
               (for-each
                (lambda (file)
                  (install-file (string-append source "/" file) doc-dir))
                '("COPYRIGHT" "HISTORY" "README"
                  ;"doc/sqstdlib3.pdf" "doc/squirrel3.pdf"  ;; pdf not build out of git; TODO

                  )))
             #t)))))
    (native-inputs
     `(("cmake" ,cmake-minimal)
       ("python-sphinx" ,python-sphinx)))
    (home-page "https://squirrel-lang.org/")
    (synopsis "High level imperative, object-oriented programming language")
    (description
     "Squirrel is a high level imperative, object-oriented programming
language, designed to be a light-weight scripting language that fits in the
size, memory bandwidth, and real-time requirements of applications like video
games.")
    (license license:expat)))

(define-public squirrel-for-supertux
  (let ((commit "abab32a2077397c3739c70fc8b5b22a18bc2b5de")
        (revision "0"))
  (package
    (inherit squirrel)
    (name "squirrel-for-supertux")
    (version (git-version "2025-12-21" revision commit))
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/SuperTux/squirrel.git")
                     (commit commit)))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0dsg9iz0hsypa8020vl35fqzzichrrq09f1ns2fnmi3x1ggw0c0f")))))))

(define-public simplesquirrel-for-supertux
  (let ((commit "37c2410b5f31d3aea3431d83b9b4398883224f23")
        (revision "0"))
    (package
      (name "simplesquirrel-for-supertux")
      (version (git-version "2026-01-06" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/SuperTux/simplesquirrel.git")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32 "0d41vmjzdx1l4xfzn4sywy6bhspv89ih589s77b5gbllylgrakxp"))))
      (build-system cmake-build-system)
      (arguments
       (list
        ;; No tests by default.  Tests that can be enabled with
        ;; -DSSQ_BUILD_TESTS configure options do not seem to be
        ;; in working order.
        #:tests? #f
        #:configure-flags
        #~(list
           ;; "-DSSQ_BUILD_TESTS=ON"
           "-DSSQ_USE_SQ_SUBMODULE=OFF"
           (string-append "-DSQUIRREL_INCLUDE_DIR="
                          #$(this-package-input "squirrel-for-supertux")
                          "/include")
           (string-append "-DSQUIRREL_LIBRARIES="
                          #$(this-package-input "squirrel-for-supertux")
                          "/include/lib/libsquirrel.so")
           (string-append "-DSQSTDLIB_LIBRARIES="
                          #$(this-package-input "squirrel-for-supertux")
                          "/include/lib/libsqstdlib.so"))
        #:phases
        #~(modify-phases %standard-phases
            (add-after 'unpack 'do-not-install-static-lib
              (lambda _
                (substitute* "CMakeLists.txt"
                  (("INSTALL\\(TARGETS \\$\\{PROJECT_NAME\\}_static .*$")
                   "")))))))
      (inputs (list squirrel-for-supertux))
      (home-page "https://matusnovak.github.io/simplesquirrel")
      (synopsis "C++ API bindings for Squirrel language")
      (description "Simplesquirrel is a C++ 11 library providing
bindings for Squirrel language.

Allows the following:

@itemize
@item Binding C++ function and calling it from Squirrel
@item Looking up Squirrel function and calling it from C++
@item Looking up Squirrel classes and creating instances on C++ side
@item Binding C++ classes including methods and member variables
@item Passing any C++ pointer as either instance or userpointer
(depends if your C++ has been exposed to VM)
@item Creating enumerations
@item Creating and passing tables
@item Creating and passing arrays
@end itemize")
      (license license:expat))))

