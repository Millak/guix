;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Ludovic Courtès <ludo@gnu.org>
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

;; This manifest "ungrafts" all the currently grafted packages and returns
;; said packages and all their dependents.

(use-modules (guix diagnostics)
             (guix i18n)
             (guix packages)
             (guix profiles)
             (guix store)
             ((guix scripts build) #:select (dependents))
             ((gnu packages) #:select (all-packages))
             (srfi srfi-1))

(define (grafted-packages)
  (info (G_ "enumerating grafted packages...~%"))
  (let ((result (filter package-replacement (all-packages))))
    (info (G_ "found ~d grafted packages:~{ ~a~}~%")
          (length result) (map package-full-name result))
    result))

(manifest
 (with-store store
   (let* ((grafted (grafted-packages))
          (ungraft-all (package-input-rewriting
                        (map (lambda (package)
                               `(,package . ,(package-replacement package)))
                             grafted))))
     (map (lambda (package)
            (manifest-entry
              (inherit (package->manifest-entry (ungraft-all package)))
              (name (string-append (package-name package) "-ungrafted"))))
          (dependents store grafted)))))
