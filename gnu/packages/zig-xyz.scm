;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maya Tomasek <maya.tomasek@disroot.org>
;;; Copyright © 2023 Ekaitz Zarraga <ekaitz@elenq.tech>
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
  #:use-module (guix build-system zig)
  #:use-module (guix gexp)
  #:use-module (gnu packages)
  #:use-module (gnu packages zig)
  #:use-module (gnu packages python))

(define-public tigerbeetle
  (package
    (name "tigerbeetle")
    (version "0.13.35")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tigerbeetledb/tigerbeetle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x8msknvq8s6vnlczq5fxmaiqvig2sbcv60c3x8zbgr28dsqpmll"))))
    (build-system zig-build-system)
    (arguments
     (list
      #:zig zig-0.9
      #:zig-release-type "safe"))
    (synopsis "Distributed financial accounting database")
    (description "TigerBeetle is a financial accounting database designed for
mission-critical safety and performance for financial services.")
    (home-page "https://github.com/tigerbeetledb/tigerbeetle")
    (license license:asl2.0)))

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
