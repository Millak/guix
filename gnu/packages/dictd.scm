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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages compression))

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

(define-public dictd
  (package
    (name "dictd")
    (version "1.13.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/dict/dictd/"
                           "dictd-"
                           version
                           "/dictd-"
                           version
                           ".tar.gz"))
       (sha256
        (base32 "06racmv25ihwgwf67fgj2703ik0m5i2cjzcxasa88kc92rysdwg4"))))
    (inputs (list libmaa zlib))
    (native-inputs (list libtool bison flex))
    (arguments
     (list
      #:test-target "test"))
    (build-system gnu-build-system)
    (synopsis "@command{dict}, @command{dictd} and @command{dictfmt} programs")
    (description
     "The DICT Interchange Format (DICF) is a human-readable
 format for the interchange of dictionary databases for the use with
DICT protocol client/server software.

This package provides a client @command{dict} and a server program
@command{dictd} for the DICT protocol, as well as a utility
@command{dictfmt} to convert various dictionary formats into
dictionaries that can be served by @command{dictd} or Dico.")
    (home-page "https://sourceforge.net/projects/dict/")
    (license gpl2+)))
