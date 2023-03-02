;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017, 2020, 2021 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2021 Ryan Sundberg <ryan@arctype.co>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
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
  #:use-module (ice-9 match)
  #:use-module ((guix licenses) #:select (bsd-2))
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages perl)
  #:use-module (guix build-system gnu))

(define-public jemalloc
  (package
    (name "jemalloc")
    (version "5.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/jemalloc/jemalloc/releases/download/"
                    version "/jemalloc-" version ".tar.bz2"))
              (sha256
               (base32
                "1apyxjd1ixy4g8xkr61p0ny8jiz8vyv1j0k4nxqkxpqrf4g2vf1d"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-thp-test
            ;; This test does not check if transparent huge pages are supported
            ;; on the system before running the test.
            (lambda _
              (substitute* "Makefile.in"
                (("\\$\\(srcroot\\)test/unit/pages.c \\\\") "\\")))))
      #:configure-flags
      ;; Disable the thread local storage model in jemalloc 5 to prevent
      ;; shared libraries linked to libjemalloc from crashing on dlopen()
      ;; https://github.com/jemalloc/jemalloc/issues/937
      #~'("--disable-initial-exec-tls"
          #$@(match (%current-system)
               ("powerpc-linux"
                (list "CPPFLAGS=-maltivec"))
               (_
                '())))))
    (inputs (list perl))
    ;; Install the scripts to a separate output to avoid referencing Perl and
    ;; Bash in the default output, saving ~75 MiB on the closure.
    (outputs '("out" "bin"))
    (home-page "https://jemalloc.net/")
    (synopsis "General-purpose scalable concurrent malloc implementation")
    (description
     "This library providing a malloc(3) implementation that emphasizes
fragmentation avoidance and scalable concurrency support.")
    (license bsd-2)))
