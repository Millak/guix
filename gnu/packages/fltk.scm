;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu packages fltk)
  #:use-module (guix deprecation))

;;; The whole file was deprecated on 2026-08-26.

(define-deprecated/public-alias fltk
  (@ (gnu packages toolkits) fltk))

(define-deprecated/public-alias fltk-1.3
  (@ (gnu packages toolkits) fltk-1.3))

(define-deprecated/public-alias ntk
  (@ (gnu packages toolkits) ntk))
