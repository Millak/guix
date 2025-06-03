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

(define-module (gnu packages fortran-xyz)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages fortran-check)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

(define-public fortran-dftd4
  (package
    (name "fortran-dftd4")
    (version "3.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dftd4/dftd4")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0alsvzgdkmw80wfpsds31pzgcr962xhq9q7yvc26jxgrn444yb3n"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dfortran_link_args=-Wl,-rpath="
                             #$output "/lib"))))
    (native-inputs
     (list gfortran
           pkg-config
           python-minimal))
    (inputs
     (list fortran-mctc-lib
           fortran-mstore
           fortran-multicharge
           lapack))
    (home-page "https://github.com/dftd4/dftd4")
    (synopsis "Implementation of the DFT-D4 dispersion correction")
    (description
     "This library provides an implementation of the DFT-D4 dispersion
correction with both a Fortran and a C interface.")
    (license license:lgpl3+)))

(define-public fortran-mctc-lib
  (package
    (name "fortran-mctc-lib")
    (version "0.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/grimme-lab/mctc-lib")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1gabdxllx2pcw1mbv4gw9zpn6817ikz9ql8xs9w86wswd6f0m5kl"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dfortran_link_args=-Wl,-rpath="
                             #$output "/lib"))))
    (native-inputs
     (list gfortran
           python-minimal))
    (home-page "https://github.com/grimme-lab/mctc-lib")
    (synopsis "Fortran library for working with molecular structure data")
    (description
     "@code{mctc-lib} (modular computation tool chain library) is a Fortran
library for operating on molecular structures and reading and writing common
geometry file formats.")
    (license license:asl2.0)))

(define-public fortran-mstore
  (package
    (name "fortran-mstore")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/grimme-lab/mstore")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g3jxc96hd7r57kczi5p28g2xm345ad1hk9lm5v6wlkmnrvg3ynd"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dfortran_link_args=-Wl,-rpath="
                             #$output "/lib"))))
    (native-inputs
     (list gfortran
           pkg-config
           python-minimal))
    (inputs
     (list fortran-mctc-lib))
    (home-page "https://github.com/grimme-lab/mstore")
    (synopsis "Molecular structure data for testing")
    (description
     "This package contains a Fortran interface to obtain molecular geometries
used for testing.")
    (license license:asl2.0)))

(define-public fortran-multicharge
  (package
    (name "fortran-multicharge")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/grimme-lab/multicharge")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19460xclxnlmyzcxg392kmm532ydg3yka5mkbbv845kgvbfhrb7j"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         (string-append "-Dfortran_link_args=-Wl,-rpath="
                        #$output "/lib"))))
    (native-inputs
     (list gfortran
           pkg-config
           python-minimal))
    (inputs
     (list fortran-mctc-lib
           fortran-mstore
           fortran-toml-f
           lapack))
    (home-page "https://github.com/grimme-lab/multicharge")
    (synopsis "Electronegativity equilibration model for atomic partial charges")
    (description
     "This library implements an electronegativity equilibration model to
calculate partial charges used in the DFT-D4 model.")
    (license license:asl2.0)))

(define-public fortran-simple-dftd3
  (package
    (name "fortran-simple-dftd3")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/dftd3/simple-dftd3")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0aygmnax3vwz2x3ad7syksfjca4zc85nyslsibs0wg8wqfsmr33k"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         "-Dpython=true"
         (string-append "-Dfortran_link_args=-Wl,-rpath="
                        #$output "/lib"))))
    (native-inputs
     (list gfortran
           pkg-config
           python-minimal
           python-cffi))
    (inputs
     (list fortran-mctc-lib
           fortran-mstore
           fortran-toml-f))
    (home-page "https://github.com/dftd3/simple-dftd3")
    (synopsis "Implementation of the DFT-D3 dispersion correction")
    (description
     "This library provides an implementation of the @url{DFT-D3,
https://www.chemie.uni-bonn.de/grimme/de/software/dft-d3/} dispersion
correction.")
    (license (list license:lgpl3+ license:gpl3+))))

(define-public fortran-toml-f
  (package
    (name "fortran-toml-f")
    (version "0.4.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/toml-f/toml-f")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0lfhk0qqf00gcsl9h78fi3s4k9q8x1aiyp90vyqdv98dnpi9ripr"))))
    (build-system meson-build-system)
    (native-inputs
     (list fortran-test-drive
           gfortran
           pkg-config))
    (home-page "https://github.com/toml-f/toml-f")
    (synopsis "Fortran TOML parser")
    (description
     "This library provides an implementation of TOML data serialization and
deserialization in Fortran.")
    ;; Dual license
    (license (list license:expat license:asl2.0))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetical order.
;;;
