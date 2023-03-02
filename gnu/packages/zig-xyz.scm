;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
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

(define-module (gnu packages zig-xyz)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages python))

(define-public zig-zls
  (package
    (name "zig-zls")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zigtools/zls")
                    (commit version)
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hhs7dz9rpshfd1a7x5swmix2rmh53vsqskh3mzqlrj2lgb3cnii"))))
    (build-system gnu-build-system)
    (inputs (list zig-0.9 python))
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (delete 'configure)
                        (replace 'build
                          (lambda* (#:key outputs #:allow-other-keys)
                            (let ((out (assoc-ref outputs "out")))
                              (setenv "ZIG_GLOBAL_CACHE_DIR"
                                      (string-append (getcwd) "/zig-cache"))
                              (invoke "zig" "build" "install"
                                      "-Drelease-safe" "--prefix" out))))
                        (delete 'install)
                        (replace 'check
                          (lambda* (#:key tests? #:allow-other-keys)
                            (when tests?
                              (invoke "zig" "build" "test")))))))
    (synopsis "Zig language server")
    (description
     "Zig Language Server is a language server implementing the @acronym{LSP,
Language Server Protocol} for the Zig programming language.")
    (home-page "https://github.com/zigtools/zls")
    (license license:expat)))
