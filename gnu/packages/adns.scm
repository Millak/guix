;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Anderson Torres <anderson.torres.8519@gmail.com>
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

(define-module (gnu packages adns)
  #:use-module (gnu packages dns)
  #:use-module (guix deprecation))

(define-deprecated/public-alias adns
  (@ (gnu packages dns) adns))

(define-deprecated/public-alias c-ares
  (@ (gnu packages dns) c-ares))

(define-deprecated/public-alias c-ares-for-node-lts
  (@ (gnu packages dns) c-ares-for-node-lts))

(define-deprecated/public-alias c-ares-for-node-bootstrap
  (@ (gnu packages dns) c-ares-for-node-bootstrap))

(define-deprecated/public-alias c-ares/cmake
  (@ (gnu packages dns) c-ares/cmake))
