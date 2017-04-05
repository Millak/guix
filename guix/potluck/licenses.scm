;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Andy Wingo <wingo@pobox.com>
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

(define-module (guix potluck licenses)
  #:use-module ((guix licenses) #:select (license?))
  #:use-module (ice-9 match)
  #:export (license-by-name all-potluck-license-names))

(define all-licenses
  (delay
    (let ((iface (resolve-interface '(guix licenses)))
          (by-name (make-hash-table)))
      (module-for-each (lambda (k var)
                         (let ((val (variable-ref var)))
                           (when (license? val)
                             (hashq-set! by-name k val))))
                       (resolve-interface '(guix licenses)))
      by-name)))

(define (all-potluck-license-names)
  (sort
   (hash-map->list (lambda (k v) k) (force all-licenses))
   (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define (license-by-name name)
  (hashq-ref (force all-licenses) name))
