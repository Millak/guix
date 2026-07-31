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

;;; INFO: the whole file was deprecated at 2026-07-31.

(define-module (gnu packages piet)
  #:use-module (guix deprecation))

(define-deprecated/public-alias npiet
  (@ (gnu packages esolangs) npiet))

(define-deprecated/public-alias piet-toolchain
  (@ (gnu packages esolangs) piet-toolchain))

;;; piet.scm ends here
