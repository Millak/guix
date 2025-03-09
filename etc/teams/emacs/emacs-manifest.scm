;;; GNU Guix --- Functional package management for GNU
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

;;; This file returns a manifest of packages built using the emacs-build-system.
;;; It is used to assist continuous integration of the emacs-team branch.

(use-modules (guix packages)
             (guix profiles)
             (guix gexp)
             (guix build-system)
             (ice-9 match)
             (srfi srfi-1)
             (srfi srfi-26))

(define* (has-emacs-build-system? build-system #:key modules #:allow-other-keys)
  (or (eq? (build-system-name build-system) 'emacs)
      (any
       (match-lambda
         (('guix 'build 'emacs-build-system) #t)
         ('((guix build emacs-build-system) . _) #t)
         (_ #f))
       (cond
        ((gexp? modules) (gexp->approximate-sexp modules))
        ((pair? modules) modules)
        (else '())))))

(manifest
  (map package->manifest-entry
       (fold-packages
        (lambda (package lst)
          (if (apply has-emacs-build-system?
                     (package-build-system package)
                     (package-arguments package))
              (cons package lst)
              lst))
        (list))))
