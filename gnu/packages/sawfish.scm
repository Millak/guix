;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
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

(define-module (gnu packages sawfish)
  #:use-module ((guix licenses) #:select (gpl2+))
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages base)
  #:use-module (gnu packages xorg))

(define-public librep
  (package
    (name "librep")
    (version "0.92.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.tuxfamily.org/librep/"
                                  "librep_" version ".tar.xz"))
              (sha256
               (base32
                "1bmcjl1x1rdh514q9z3hzyjmjmwwwkziipjpjsl301bwmiwrd8a8"))))
    (build-system gnu-build-system)
    (native-inputs
     (list texinfo pkg-config autoconf automake libtool))
    (inputs
     (list gdbm gmp libffi readline))
    (native-search-paths
     (list (search-path-specification
            (variable "REP_DL_LOAD_PATH")
            (files '("lib/rep")))))
    (home-page "https://sawfish.fandom.com/wiki/Librep")
    (synopsis "Lisp system for sawfish")
    (description
     "Librep is a dialect of Lisp, designed to be used both as an extension
language for applications and as a general purpose programming language.  It
was originally written to be mostly-compatible with Emacs Lisp, but has
subsequently diverged markedly.  Its aim is to combine the best features of
Scheme and Common Lisp and provide an environment that is comfortable for
implementing both small and large scale systems.")
    (license gpl2+)))
