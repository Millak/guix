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

(define-module (guix potluck build-systems)
  #:use-module ((guix build-system) #:select (build-system?))
  #:use-module ((gnu packages) #:select (scheme-modules))
  #:use-module (ice-9 match)
  #:export (build-system-by-name all-potluck-build-system-names))

(define all-build-systems
  (delay
    (let* ((gbs (resolve-module '(guix build-system)))
           (root (dirname (dirname (module-filename gbs))))
           (by-name (make-hash-table)))
      (for-each (lambda (iface)
                  (module-for-each
                   (lambda (k var)
                     (let* ((str (symbol->string k))
                            (pos (string-contains str "-build-system"))
                            (val (variable-ref var)))
                       (when (and pos (build-system? val))
                         (let* ((head (substring str 0 pos))
                                (tail (substring str
                                                 (+ pos (string-length
                                                         "-build-system"))))
                                (name (string->symbol
                                       (string-append head tail))))
                           (hashq-set! by-name name val)))))
                   iface))
                (scheme-modules root "guix/build-system"))
      by-name)))

(define (all-potluck-build-system-names)
  (sort
   (hash-map->list (lambda (k v) k) (force all-build-systems))
   (lambda (a b) (string<? (symbol->string a) (symbol->string b)))))

(define (build-system-by-name name)
  (hashq-ref (force all-build-systems) name))
