;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
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

(define-module (gnu packages prometheus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang-build))

;;; Commentary:
;;;
;;; Libraries and commands related to, or provided by Prometheus project
;;; <https://prometheus.io>.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-github-com-prometheus-client-model
  (package
    (name "go-github-com-prometheus-client-model")
    (version "0.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/prometheus/client_model")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g1q2szzwp4rwkvayi2mnq2nwj6hj4ja7j43vwyi1iaz6d9z505c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/prometheus/client_model"
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
        ;; Source-only package
        (delete 'build))))
    (propagated-inputs
     (list go-github-com-golang-protobuf))
    (home-page "https://github.com/prometheus/client_model")
    (synopsis "Data model artifacts for Prometheus")
    (description
     "This package provides data model artifacts for Prometheus.")
    (license license:asl2.0)))

;;;
;;; Executables:
;;;

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above in alphabetic order.
;;;
