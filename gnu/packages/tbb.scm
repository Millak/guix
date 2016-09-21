;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
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
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages))

(define-public tbb
  (package
    (name "tbb")
    (version "2017")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.threadingbuildingblocks.org/sites/default"
                    "/files/software_releases/source/"
                    "tbb" version "_20160722oss_src.tgz"))
              (sha256
               (base32
                "038rmv3s8si51bjrzwyv8ldqw742fjjdfayi8pmjaq5zw32b8pzx"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "build/common.inc"
                  (("export tbb_build_prefix.+$")
                   "export tbb_build_prefix?=guix\n")))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags (list (string-append "LDFLAGS=-Wl,-rpath="
                                         (assoc-ref %outputs "out") "/lib"))
       #:phases (alist-replace
                 'configure
                 (lambda* (#:key outputs #:allow-other-keys)
                   (substitute* "build/linux.gcc.inc"
                     (("LIB_LINK_FLAGS =")
                      (string-append "LIB_LINK_FLAGS = -Wl,-rpath="
                                     (assoc-ref outputs "out") "/lib"))))
                 (alist-replace
                  'install
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
                      (copy-recursively "include" include)
                      #t))
                  %standard-phases))))
    (home-page "https://www.threadingbuildingblocks.org")
    (synopsis "C++ library for parallel programming")
    (description
     "Threading Building Blocks (TBB) is a C++ runtime library that abstracts
the low-level threading details necessary for optimal multi-core performance.
It uses common C++ templates and coding style to eliminate tedious threading
implementation work.  It provides parallel loop constructs, asynchronous
tasks, synchronization primitives, atomic operations, and more.")
    (license asl2.0)))
