;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2017 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages xml))

(define-public augeas
  (package
    (name "augeas")
    (version "1.14.1")
    (source (origin
              ;; XXX: Project release has moved to GitHub which has
              ;;      pre-generated "configure" script that allows to simplify
              ;;      the package definition. Try to completely build from
              ;;      source, glibc comes as git submodule.
              (method url-fetch)
              (uri
               (string-append
                "https://github.com/hercules-team/augeas/releases/download/"
                "release-" version
                "/augeas-" version ".tar.gz"))
              (sha256
               (base32
                "1zzdp5bwnszza5q6cjw66hkicay8b49n5pda7cbcgfg4hbbzv2rn"))))
    (build-system gnu-build-system)
    (propagated-inputs
     (list libxml2))
    (native-inputs
     (list readline pkg-config))
    (home-page "https://augeas.net")
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

(define-public python-augeas
  (package
    (name "python-augeas")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hercules-team/python-augeas")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1l17gl23f5naram1jaab7gjr9bhjdj97fd9sydvs7cmpns91rbrf"))))
    (build-system pyproject-build-system)
    (native-inputs
     (list python-pytest pkg-config))
    (propagated-inputs
     (list python-cffi))
    (inputs
     (list augeas libxml2))
    (home-page "https://github.com/hercules-team/python-augeas")
    (synopsis "Python bindings for Augeas")
    (description "Pure Python bindings for @url{https://augeas.net, Augeas}.")
    (license license:lgpl2.1+)))
