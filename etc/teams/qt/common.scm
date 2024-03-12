;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(use-modules (guix packages)
             (guix profiles)
             (guix utils)
             (gnu packages))

(define* (qt-packages-manifest #:key major-version negate-version-test?)
  "Return a manifest of Qt packages for MAJOR-VERSION, or any version if left
unspecified.  If NEGATE-VERSION-TEST? is true, select all the Qt packages
*not* matching MAJOR-VERSION."
  (manifest
   (map package->manifest-entry
        (fold-packages
         (lambda (package lst)
           (let ((uri (and=> (package-source package)
                             (lambda (x)
                               (and (origin? x)
                                    (origin-uri x))))))
             (if (and uri
                      (string? uri)
                      (string-prefix? "mirror://qt/" uri)
                      (if major-version
                          ((if negate-version-test? not identity)
                           (string=? major-version (version-major
                                                    (package-version package))))
                          #t))
                 (cons package lst)
                 lst)))
         '()))))
