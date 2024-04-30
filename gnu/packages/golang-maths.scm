;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Rodion Goritskov <rodion.goritskov@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages golang-maths)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages))

;;; Commentary:
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-github-com-montanaflynn-stats
  (package
    (name "go-github-com-montanaflynn-stats")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/montanaflynn/stats")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y38cvp7r6fb6291k82j781dbykx00mxw8ca0v9d0fijzc1x81fi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/montanaflynn/stats"))
    (home-page "https://github.com/montanaflynn/stats")
    (synopsis "Statistics library for Golang")
    (description
     "This package provides a statistical library for Golang.")
    (license license:expat)))

(define-public go-github-com-shopspring-decimal
  (package
    (name "go-github-com-shopspring-decimal")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shopspring/decimal")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p1iz0ybsjvc8k3w6lf92drs51fgrcbz0ib1p4ihp3gmdq5rnzjk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shopspring/decimal"))
    (home-page "https://github.com/shopspring/decimal")
    (synopsis "Arbitrary-precision fixed-point decimal numbers in Golang")
    (description
     "Package decimal implements an arbitrary precision fixed-point decimal.
Features:
@itemize
@item the zero-value is 0, and is safe to use without initialization
@item addition, subtraction, multiplication with no loss of precision
@item division with specified precision
@item database/sql serialization/deserialization
@item JSON and XML serialization/deserialization
@end itemize")
    (license license:expat)))

(define-public go-github-com-x448-float16
  (package
    (name "go-github-com-x448-float16")
    (version "0.8.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/x448/float16")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qg6ya30fra20hpa2qzqqzs8l95lvw9yzd87fdzq195xqi6crb2l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/x448/float16"))
    (home-page "https://github.com/x448/float16")
    (synopsis "Float16 (Binary16) in Golang")
    (description
     "This package provides a
@url{https://en.wikipedia.org/wiki/Half-precision_floating-point_format,IEEE
754 half-precision floating-point format (binary16)} with IEEE 754 default
rounding for conversions.  IEEE 754-2008 refers to this 16-bit floating-point
format as binary16.")
    (license license:expat)))

;;;
;;; Executables:
;;;

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
