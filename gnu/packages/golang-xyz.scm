;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Thomas Ieong <th.ieong@free.fr>
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

(define-module (gnu packages golang-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang-check))

;;; Commentary:
;;;
;;; Nomad Golang modules (libraries) are welcome here.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public go-github-com-djherbis-atime
  (package
    (name "go-github-com-djherbis-atime")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/djherbis/atime")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xsz55zpihd9wyrj6qvm3miqzb6x3mnp5apzs0dx1byndhb8adpq"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/djherbis/atime"))
    (home-page "https://github.com/djherbis/atime")
    (synopsis "Access Times for files")
    (description "Package atime provides a platform-independent way to get
atimes for files.")
    (license license:expat)))

(define-public go-github-com-matryer-try
  (package
    (name "go-github-com-matryer-try")
    (version "1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/matryer/try")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15f0m5ywihivnvwzcw0mh0sg27aky9rkywvxqszxka9q051qvsmy"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/matryer/try"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-tests
            (lambda* (#:key import-path #:allow-other-keys)
              (substitute* (string-append "src/" import-path
                                          "/try_test.go")
                (("var value string")
                 "")
                (("value, err = SomeFunction\\(\\)")
                 "_, err = SomeFunction()")))))))
    (native-inputs
     (list go-github-com-cheekybits-is))
    (home-page "https://github.com/matryer/try")
    (synopsis "Simple idiomatic retry package for Go")
    (description "This package provides an idiomatic Go retry module.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
