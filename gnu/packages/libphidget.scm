;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2026 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu packages libphidget)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system pyproject)
  #:use-module (guix licenses)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages python-build))

(define-public libphidget22
  (package
   (name "libphidget22")
   ;; See file 'library_version' in the tarball for the version string.
   (version "1.27.20261112")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "https://cdn.phidgets.com/downloads/phidget22/libraries/"
                  "linux/libphidget22.tar.gz"))
            (sha256
             (base32
              "1w6h5h5x1b9fy8dzbc3h04m9v3h9v6a426iqarmgliz4rvza9jki"))))
   (build-system gnu-build-system)
   (arguments
    (list #:configure-flags #~(list "--disable-static")))
   (inputs (list libusb))
   (outputs '("out" "debug"))
   (home-page "https://www.phidgets.com/")
   (license lgpl3+)
   (synopsis "C library to manipulate Phidgets")
   (description synopsis)))

(define-deprecated-package libphidget libphidget22)

(define-public python-phidget22
  (package
    (name "python-phidget22")
    (version "1.27.20261112")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "phidget22" version))
       (sha256
        (base32 "0xgg5gpwzx944gq2ahp7vagx920fn05hp59053qpahanbvlqcp2b"))))
    (build-system pyproject-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'record-libphidget-file-name
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "Phidget22/_phidget_support.py"
                     (("libs_path = .*")
                      (let ((libphidget (search-input-file
                                         inputs
                                         "lib/libphidget22.so")))
                        (string-append "libs_path = '"
                                       (dirname libphidget)
                                       "'\n")))))))
       #:tests? #f))                              ;no tests
    (inputs (list libphidget22))
    (native-inputs (list python-setuptools))
    (home-page "https://www.phidgets.com")
    (synopsis "Python bindings for the Phidget library")
    (description
     "This package provides Python bindings to the libphidget library, which
allows you to control Phidget devices.")
    (license bsd-3)))
