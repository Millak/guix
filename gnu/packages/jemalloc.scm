;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages jemalloc)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module ((guix licenses) #:select (bsd-2))
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gcc)
  #:use-module (guix build-system gnu))

(define-public jemalloc
  (package
    (name "jemalloc")
    (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jemalloc/jemalloc/releases/download/"
                    version "/jemalloc-" version ".tar.bz2"))
              (sha256
               (base32
                "10373xhpc10pgmai9fkc1z0rs029qlcb3c0qfnvkbwdlcibdh2cl"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-thp-test
           ;; This test does not check if transparent huge pages are supported
           ;; on the system before running the test.
           (lambda _
             (substitute* "Makefile.in"
               (("\\$\\(srcroot\\)test/unit/pages.c \\\\") "\\"))
             #t)))
       ,@(if (any (cute string-prefix? <> (or (%current-target-system)
                                              (%current-system)))
                 '("x64_64" "i686"))
           ;; Transparent huge pages are only enabled by default on Intel processors
           '()
           '(#:configure-flags (list "--disable-thp")))))
    (home-page "http://jemalloc.net/")
    (synopsis "General-purpose scalable concurrent malloc implementation")
    (description
     "This library providing a malloc(3) implementation that emphasizes
fragmentation avoidance and scalable concurrency support.")
    (license bsd-2)))
