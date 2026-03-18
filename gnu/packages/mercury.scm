;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2022 jgart <jgart@dismail.de>
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

(define-module (gnu packages mercury)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bdw-gc)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages pkg-config)
  #:use-module ((ice-9 match) #:select (match-lambda)))

;; NOTE: Mercury uses a tightly coupled fork of BDWGC and
;; libatomic-ops. When updating the package, please check the GitHub
;; repository to ensure that the submodule commit matches what is
;; provided.
(define (gc-fork package-name package-url
                 package-commit package-hash)
  (let ((commit package-commit))
    (package (inherit package-name)
             (source
              (origin
                (method git-fetch)
                (uri (git-reference
                      (url package-url)
                      (commit commit)))
                (sha256 (base32 package-hash)))))))

