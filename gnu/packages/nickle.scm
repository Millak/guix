;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024,2026 Eric Bavier <bavier@posteo.net>
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

(define-module (gnu packages nickle)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages ruby-xyz)
  #:use-module (guix build-system meson)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public nickle
  (package
    (name "nickle")
    (version "2.107")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://nickle.org/release/nickle-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1wli8p5fv2ldy51ycc8pcx8rl3ph2wbyiqs21qpjadlvildaaqj0"))))
    (build-system meson-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-before 'configure 'disable-benchmarks
                          ;; Benchmarks would require `gmp` and don't
                          ;; test anything not already covered by other
                          ;; tests, so disable them here.
                          (lambda _
                            (substitute* "meson.build"
                              (("^subdir\\('bench'\\)" &)
                               (string-append "# " &))))))))
    (native-inputs (list bc bison flex pkg-config ruby-asciidoctor-pdf))
    (inputs (list readline))
    (synopsis "Numeric oriented programming language")
    (description
     "Nickle is a programming language based prototyping environment with
powerful programming and scripting capabilities.  Nickle supports a variety of
datatypes, especially arbitrary precision numbers.  The programming language
vaguely resembles C.  Some things in C which do not translate easily are
different, some design choices have been made differently, and a very few
features are simply missing.  Nickle provides the functionality of Unix bc, dc
and expr in a different form.  It is also an ideal environment for prototyping
complex algorithms.  Nickle's scripting capabilities make it a replacement for
spreadsheets in some applications, and its numeric features complement the
limited numeric functionality of text-oriented languages such as AWK and Perl.")
    (home-page "https://nickle.org/")
    (license license:expat)))
