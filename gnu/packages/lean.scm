;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Amin Bandali <bandali@gnu.org>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022 Pradana Aumars <paumars@courrier.dev>
;;; Copyright © 2023 Zhu Zihao <all_but_last@163.com>
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

(define-module (gnu packages lean)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages multiprecision)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (gnu packages graphviz)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz))

(define-public lean
  (package
    (name "lean")
    (version "3.51.1")
    (home-page "https://lean-lang.org" )
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/leanprover-community/lean")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "17g4d3lqnbl1yfy2pjannf73v8qhc5003d2jkmrqiy05zkqs8d9n"))))
    (build-system cmake-build-system)
    (inputs
     (list gmp))
    (arguments
     (list
      #:build-type "Release"            ; default upstream build type
      ;; XXX: Test phases currently fail on 32-bit sytems.
      ;; Tests for those architectures have been temporarily
      ;; disabled, pending further investigation.
      #:tests? (and (not (%current-target-system))
                    (let ((arch (%current-system)))
                      (not (or (string-prefix? "i686" arch)
                               (string-prefix? "armhf" arch)))))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'chdir-to-src
            (lambda _ (chdir "src"))))))
    (synopsis "Theorem prover and programming language")
    (description
     "Lean is a theorem prover and programming language with a small trusted
core based on dependent typed theory, aiming to bridge the gap between
interactive and automated theorem proving.")
    (license license:asl2.0)))

(define-public python-mathlibtools
  (package
    (name "python-mathlibtools")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mathlibtools" version))
              (sha256
               (base32
                "089pql105imx8z7ar1wiz9fn000jp6xqdfixw4jf2vric94vn9fj"))))
    (build-system python-build-system)
    (arguments
     '(#:phases (modify-phases %standard-phases
                  (add-before 'check 'fix-home-directory
                    (lambda _
                      (setenv "HOME" "/tmp"))))))
    (inputs (list python-toml
                  python-pygithub
                  python-certifi
                  python-gitpython
                  python-requests
                  python-click
                  python-tqdm
                  python-networkx
                  python-pydot
                  python-pyyaml
                  python-atomicwrites))
    (home-page "https://github.com/leanprover-community/mathlib-tools")
    (synopsis "Development tools for Lean mathlib")
    (description
     "This package contains @command{leanproject}, a supporting tool for Lean
mathlib, a mathematical library for the Lean theorem prover.")
    (license license:asl2.0)))
