;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-home-services)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (guix diagnostics)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

(test-begin "home-services")

(test-assert "fold-home-service-types"
  (match (fold-home-service-types cons '())
    (() #f)
    (lst (and (every service-type? lst)
              (every (lambda (type)
                       (let ((location (service-type-location type)))
                         (string-contains (location-file location)
                                          "gnu/home")))
                     lst)))))

(test-eq "lookup-service-types"
  home-files-service-type
  (and (null? (lookup-home-service-types 'does-not-exist-at-all))
       (match (lookup-home-service-types 'home-files)
         ((one) one)
         (x x))))

(test-end)
