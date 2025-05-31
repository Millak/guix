;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Sughosha <sughosha@proton.me>
;;; Copyright © 2025 Herman Rimm <herman@rimm.ee>
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

(define-module (gnu packages postmarketos)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages android)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (guix build-system pyproject)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (ice-9 match))

(define-public pmbootstrap
  (package
    (name "pmbootstrap")
    (version "3.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url (string-append "https://gitlab.postmarketos.org/"
                                  "postmarketOS/pmbootstrap.git"))
              (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16naj8ykipbrs2a93zaxci1wl045cc9jn7rq8sy76d1rqlqb9mmw"))))
    (build-system pyproject-build-system)
    (arguments
     (list
       #:modules '((guix build pyproject-build-system)
                   (guix build utils)
                   (ice-9 match))
       #:phases #~(modify-phases %standard-phases
                    (add-after 'unpack 'set-sudo
                      (lambda _
                        (substitute* "pmb/config/sudo.py"
                          (("sudo\"") "/run/privileged/bin/sudo\""))))
                    (add-after 'wrap 'wrap-required-programs
                      (lambda* (#:key inputs outputs #:allow-other-keys)
                        (wrap-program (string-append #$output
                                                     "/bin/pmbootstrap")
                          '("PATH" ":" prefix
                            #$(map (match-lambda
                                     ((input directory)
                                      (file-append (this-package-input input)
                                                   "/" directory)))
                                   '(("bash-minimal"      "bin")
                                     ("coreutils-minimal" "bin")
                                     ("git-minimal"       "bin")
                                     ("multipath-tools"   "sbin")
                                     ("openssl"           "bin")
                                     ("procps"            "bin")
                                     ("tar"               "bin")
                                     ("util-linux"        "bin")
                                     ("util-linux"        "sbin"))))))))
       ;; The first two tests require a pmaports git repository in the workdir.
       #:test-flags #~(list (string-append "--deselect=test/core/test_pkgrepo"
                                           ".py::test_pkgrepo_pmaports")
                            (string-append "--deselect=test/parse/test_bootimg"
                                           ".py::test_bootimg")
                            ;; RuntimeError: No package repositories specified?
                            (string-append "--deselect=test/parse/"
                                           "test_deviceinfo.py::"
                                           "test_random_valid_deviceinfos"))))
    (native-inputs (list mkbootimg
                         python-pytest
                         python-setuptools
                         python-wheel
                         util-linux)) ; for losetup
    (inputs (list bash-minimal
                  coreutils-minimal
                  git-minimal
                  multipath-tools
                  openssl
                  procps
                  tar
                  util-linux))
    (home-page "https://gitlab.com/postmarketOS/pmbootstrap")
    (synopsis "Bootstrap a postmarketOS system")
    (description
     "This package provides a sophisticated chroot, build and flash tool
to develop and install postmarketOS.")
    (license license:gpl3+)))
