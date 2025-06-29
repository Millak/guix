;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2025 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (guix build-system minetest)
  #:use-module (guix build-system luanti)
  #:use-module (guix deprecation)
  #:export (minetest-mod-build-system))

(define-deprecated/alias minetest-mod-build-system luanti-mod-build-system)
