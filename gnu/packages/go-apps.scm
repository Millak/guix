;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages go-apps)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-xyz))

(define-public godef
  (package
    (name "godef")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rogpeppe/godef")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rhhg73kzai6qzhw31yxw3nhpsijn849qai2v9am955svmnckvf4"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "vendor"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/rogpeppe/godef"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'delete-problematic-tests
            (lambda _
              ;; The TestGoDef/Modules test fails, because of the lack of Go
              ;; modules support.
              (delete-file "src/github.com/rogpeppe/godef/godef_test.go"))))))
    (inputs (list go-golang-org-x-tools go-ninefans-net-go))
    (home-page "https://github.com/rogpeppe/godef")
    (synopsis "Print where symbols are defined in Go source code")
    (description "The @command{godef} command prints the source location of
definitions in Go programs.")
    (license license:bsd-3)))
