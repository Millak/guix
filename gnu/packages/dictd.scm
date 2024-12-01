;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Runciter <runciter@whispers-vpn.org>
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

(define-module (gnu packages dictd)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools))

(define-public libmaa
  (package
    (name "libmaa")
    (version "1.3.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dict/libmaa/"
                           "libmaa-"
                           version
                           "/libmaa-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "1idi4c30pi79g5qfl7rr9s17krbjbg93bi8f2qrbsdlh78ga19ar"))))
    (native-inputs (list libtool))
    (arguments
     (list
      ;; Change -Werror to -Wno-error, reproduce other default flags
      ;; Do not error out on warnings related to snprintf function
      #:make-flags #~'("CFLAGS=-DHAVE_CONFIG_H  -Wall -Wno-error -g -O2 $(VERCFLAGS) -I. -I${srcdir}")
      #:test-target "test"))
    (build-system gnu-build-system)
    (synopsis "Low-level data structures used by the dictd program")
    (description
     "The libmaa library provides many low-level
data structures which are helpful for writing compilers, including hash
tables, sets, lists, debugging support, and memory management.  Although
libmaa was designed and implemented as a foundation for the Khepara
transformation system, the data structures are generally applicable to a
wide range of programming problems.

The memory management routines are especially helpful for improving the
performance of memory-intensive applications.")
    (home-page "https://sourceforge.net/projects/dict/")
    (license gpl2+)))
