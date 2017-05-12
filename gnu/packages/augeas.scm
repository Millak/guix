;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages augeas)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public augeas
  (package
    (name "augeas")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.augeas.net/augeas-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1iac5lwi1q10r343ii9v5p2fdplvh06yv9svsi8zz6cd2c2fjp2i"))))
    (build-system gnu-build-system)
    ;; Marked as "required" in augeas.pc
    (propagated-inputs
     `(("libxml2" ,libxml2)))
    (inputs
     `(("readline" ,readline)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://augeas.net/")
    (synopsis "Edit configuration files programmatically")
    (description
     "Augeas is a library and command line tool for programmatically editing
configuration files in a controlled manner.  Augeas exposes a tree of all
configuration settings and a simple local API for manipulating the tree.
Augeas then modifies underlying configuration files according to the changes
that have been made to the tree; it does as little modeling of configurations
as possible, and focuses exclusivley on transforming the tree-oriented syntax
of its public API to the myriad syntaxes of individual configuration files.")
    (license license:lgpl2.1+)))
