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
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

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
