;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Sharlatan Hellseher <sharlatanus@gmail.com>
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

;;; This file returns a manifest of packages under ai in etc/teams.scm.
;;; It is used to assist testing of the ai-team.
;;;
;;;
;;; You can use it to build all covered packages with:
;;;
;;;  ./pre-inst-env guix build -m etc/teams/science/science-manifest.scm
;;;
;;; or to update them with:
;;;
;;;  ./pre-inst-env guix build -m etc/teams/science/science-manifest.scm
;;;
;;; or to lint them with:
;;;
;;;  ./pre-inst-env guix build -m etc/teams/science/science-manifest.scm


(use-modules (guix diagnostics)
             (guix packages)
             (ice-9 regex))

;; The list is mirrored with etc/teams.scm for the science team.
(define %science-package-modules
  '("algebra" "astronomy" "chemistry" "geo" "graph" "lean" "maths" "medical"
    "physics" "sagemath" "statistics"))

(define (package-in-module? package modules)
  (let ((modules-re (string-append "^gnu/packages/("
                                   (string-join modules "|") ")\\.scm$")))
    (and (package-definition-location package)
         (string-match modules-re
                       (location-file (package-definition-location package))))))

(manifest
 (map package->manifest-entry
      (fold-packages
       (lambda (package lst)
         (if (package-in-module? package %science-package-modules)
             (cons package lst)
             lst))
       '())))
