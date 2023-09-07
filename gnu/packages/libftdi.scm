;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Manolis Fragkiskos Ragkousis <manolis837@gmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023 Simon South <simon@simonsouth.net>
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

(define-module (gnu packages libftdi)
  #:use-module (guix build utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages textutils))

(define-public libftdi
  (package
    (name "libftdi")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.intra2net.com/en/developer/libftdi/download/"
                    "libftdi1-" version ".tar.bz2"))
              (sha256
               (base32
                "0jdh5r499wbz83vmpskczq5m3cfc1mcv8xqisj5i95k1r3lr2w3w"))
              (patches
               (search-patches "libftdi-fix-paths-when-FTDIPP-set.patch"))))
    (build-system cmake-build-system)
    (outputs '("out" "doc" "python"))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-DCMAKE_INSTALL_DOCDIR="
                             #$output "/share/doc/" #$name "-" #$version)
              "-DDOCUMENTATION=ON"
              "-DEXAMPLES=OFF"
              "-DFTDIPP=ON"
              "-DLIB_SUFFIX=''"         ; place libraries in /lib, not /lib64
              "-DPYTHON_BINDINGS=ON")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-python-binding
            (lambda _
              (let* ((python-version
                      #$(version-major+minor (package-version python)))
                     (python-lib-path
                      (string-append "/lib/python" python-version)))
                (mkdir-p (string-append #$output:python "/lib"))
                (mkdir-p (string-append #$output:python "/share/libftdi"))
                (rename-file (string-append #$output python-lib-path)
                             (string-append #$output:python python-lib-path))
                (rename-file (string-append #$output
                                            "/share/libftdi/examples")
                             (string-append #$output:python
                                            "/share/libftdi/examples")))))
          (add-after 'install-python-binding 'install-documentation
            (lambda _
              (let ((share (string-append #$output:doc "/share")))
                (copy-recursively "doc/man"
                                  (string-append share "/man"))
                (copy-recursively "doc/html"
                                  (string-append share "/doc/"
                                                 #$name "-" #$version
                                                 "/html"))))))
      #:test-target "check"
      #:tests? #f))                     ; tests fail without access to USB
    (native-inputs
     (list doxygen graphviz pkg-config python swig))
    (inputs
     (list boost libconfuse))
    (propagated-inputs
     (list libusb))                     ; required by libftdi1.pc
    (home-page "https://www.intra2net.com/en/developer/libftdi/")
    (synopsis "FTDI USB driver with bitbang mode")
    (description
     "libFTDI is a library to talk to FTDI chips: FT232BM, FT245BM, FT2232C,
FT2232D, FT245R and FT232H including the popular bitbangmode.")
    (license (list license:gpl2         ; ftdi_eeprom, C++ bindings
                   license:lgpl2.1))))  ; main library
