;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>
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

;;; This file returns a manifest of packages built using the rebar-build-system.
;;; It is used to assist continuous integration of the beam-team branch.
;;;
;;;
;;; You can use it to build all covered packages with:
;;;
;;;  ./pre-inst-env guix build -m etc/teams/beam/erlang-manifest.scm
;;;
;;; or to update them with:
;;;
;;;  ./pre-inst-env guix refresh -u -m etc/teams/beam/erlang-manifest.scm

(use-modules (guix packages)
             (guix profiles)
             (guix gexp)
             (guix build-system)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

(define* (has-rebar-build-system? build-system #:key modules #:allow-other-keys)
  (or (eq? (build-system-name build-system) 'rebar)
      (any
       (match-lambda
         (('guix 'build 'rebar-build-system) #t)
         ('((guix build rebar-build-system) . _) #t)
         (_ #f))
       (cond
        ((gexp? modules) (gexp->approximate-sexp modules))
        ((pair? modules) modules)
        (else '())))))

(manifest
  (map package->manifest-entry
       (fold-packages
        (lambda (package lst)
          (if (apply has-rebar-build-system?
                     (package-build-system package)
                     (package-arguments package))
              (cons package lst)
              lst))
        (list))))
