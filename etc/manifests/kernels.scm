;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Leo Famulari <leo@famulari.name>
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

;;; This file returns a manifest of packages related to linux-libre.
;;; Simplistically, it selects packages whose names begin with "linux-libre".
;;; It is used to assist continuous integration of the kernel packages.

(use-modules (guix packages)
             (guix profiles)
             (gnu packages))

(manifest
  (map package->manifest-entry
       (fold-packages
         (lambda (package lst)
           (if (string-prefix? "linux-libre"
                               (package-name package))
             (cons package lst)
             lst))
         '())))
