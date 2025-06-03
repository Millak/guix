;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 David Elsing <david.elsing@posteo.net>
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

(define-module (gnu packages fortran-check)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages gcc))

;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public fortran-test-drive
  (package
    (name "fortran-test-drive")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/fortran-lang/test-drive")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0zfkih7mq9cgjkfl5f3d34ahchj2933i5ryzcvqiixqdn897q765"))))
    (build-system meson-build-system)
    (native-inputs
     (list gfortran))
    (home-page "https://github.com/fortran-lang/test-drive")
    (synopsis "Fortran testing framework")
    (description
     "@code{test-drive} is a procedural unit testing framework for
Fortran.")
    ;; Dual license
    (license (list license:expat license:asl2.0))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetical order.
;;;
