;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Hartmut Goebel <h.goebel@crazy-compilers.com>
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

(define-module (guix import minetest)
  #:use-module (guix deprecation)
  #:use-module (guix import luanti)
  #:re-export (%default-sort-key
               %contentdb-api
               json->package
               contentdb-fetch
               elaborate-contentdb-name
               sort-packages)
  #:export    (minetest-package?
               latest-minetest-release
               minetest->guix-package
               minetest-recursive-import
               %minetest-updater))

(define-deprecated/alias minetest-package? luanti-package?)
(define-deprecated/alias latest-minetest-release latest-luanti-release)
(define-deprecated/alias minetest->guix-package luanti->guix-package)
(define-deprecated/alias minetest-recursive-import luanti-recursive-import)
(define-deprecated/alias %minetest-updater %luanti-updater)
