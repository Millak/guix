;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018-2021 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2018, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 Dan Frumin <dfrumin@cs.ru.nl>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Björn Höfling <bjoern.hoefling@bjoernhoefling.de>
;;; Copyright © 2020 raingloom <raingloom@riseup.net>
;;; Copyright © 2020 Robin Green <greenrd@greenrd.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Simon Tournier <zimon.toutoune@gmail.com>
;;; Copyright © 2022 Garek Dyszel <garekdyszel@disroot.org>
;;; Copyright © 2024 Foundation Devices, Inc. <hello@foundation.xyz>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2026 Dan Rostovtsev <dan@rostovtsev.org>
;;; Copyright © 2026 Jason Conroy <jconroy@tscripta.net>
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

(define-module (gnu packages rocq)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml)
  #:use-module (guix build-system dune)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define (rocq-arguments opam-package-name)
  ;; Dune parallel build is not reproducible (in rocq and camlp-streams,
  ;; at least).
  ;; First observed in Rocq in 2019 by bmwiedemann at OpenSUSE.
  ;; - https://github.com/rocq-prover/rocq/issues/11229
  ;; Was escalated to a specific package by Rocq team in 2023.
  ;; - https://github.com/ocaml/camlp-streams/issues/9
  ;; Was escalated to Dune by Xavier Leroy in 2023.
  ;; - https://github.com/ocaml/dune/issues/9152
  ;; AFAIK, no patches addressing this in any project circa 2026.
  (list #:package opam-package-name
        #:build-flags ''("-j1")
        ;; The tests must be serial as well for reproducible builds.
        #:test-flags ''("-j1")))

(define-public rocq-runtime
  (package
    (name "rocq-runtime") ;see rocq-runtime.opam in rocq's git
    (version "9.2.0")
    (source
     (origin
       (method git-fetch)
       ;; Use github uri as recommended by "Contributing" docs.
       ;; Signed by Nicolas Tabareau (GPG: 5F6E82ADF36B53F6)
       (uri (git-reference
             (url "https://github.com/rocq-prover/rocq")
             (commit (string-append "V" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rxq2cfqlbp8vzygmk6cwszbsn5l2vka0syhrafrnpn2rpyivya9"))))
    (build-system dune-build-system)
    (arguments
     (rocq-arguments "rocq-runtime"))
    (propagated-inputs (list ocaml-zarith camlzip ocaml-yojson))
    (inputs (list gmp))
    (native-inputs (list ocaml-ounit2 which))
    (native-search-paths
     (list
       (search-path-specification
         (variable "ROCQLIB")
         (files (list "lib/ocaml/site-lib/coq")))
       (search-path-specification
         (variable "ROCQRUNTIMELIB")
         (files (list "lib/ocaml/site-lib/rocq-runtime")))))
    (home-page "https://rocq-prover.org/")
    (synopsis "Core Binaries and Tools for the Rocq Prover")
    (description
     "Rocq is an interactive theorem prover, or proof assistant.  It provides
a formal language to write mathematical definitions, executable algorithms
and theorems together with an environment for semi-interactive development
of machine-checked proofs.

This package includes the Rocq Prover core binaries, plugins, and tools, but
not the language's standard library implementation.")
    ;; The source code is distributed under lgpl2.1.
    ;; Some of the documentation is distributed under opl1.0+.
    (license (list lgpl2.1 opl1.0+))))

(define-public rocq-core
  (package
    (inherit rocq-runtime)
    (name "rocq-core") ;see rocq-core.opam in rocq's git
    (arguments
     (append (rocq-arguments "rocq-core")
             (list #:phases
                   ;; this is the only package in rocq that /needs/ a "dunestrap"
                   #~(modify-phases %standard-phases
                       (add-before 'build 'make-dunestrap
                         (lambda _
                           (invoke "make" "DUNEOPT=-j1" "COQ_SPLIT=1"
                                   "dunestrap")))))))
    (propagated-inputs (modify-inputs (package-propagated-inputs rocq-runtime)
                         (append rocq-runtime)))
    (synopsis "Standard library modules for Rocq Prover")
    (description
     "Rocq is an interactive theorem prover, or proof assistant.  It provides
 a formal language to write mathematical definitions, executable algorithms
and theorems together with an environment for semi-interactive development
of machine-checked proofs.

This package includes the Rocq standard library some other core language
libraries.")))

; The Rocq project refers to the pacakge as the "RocqIDE server," but has not
; yet changed the name of the opam package.
; https://rocq-prover.org/install
(define-public rocqide-server
  (package
    (inherit rocq-runtime)
    (name "rocqide-server") ;see coqide-server.opam in rocq's git
    (propagated-inputs (modify-inputs (package-propagated-inputs rocq-runtime)
                         (append rocq-runtime)))
    (arguments
     (rocq-arguments "coqide-server"))
    (synopsis "Rocq's XML protocol server")
    (description
     "Rocq is an interactive theorem prover, or proof assistant.  It provides
 a formal language to write mathematical definitions, executable algorithms
and theorems together with an environment for semi-interactive development
of machine-checked proofs.

This package provides the @code{coqidetop} language server, an implementation of
Rocq's XML protocol which allows clients, such as RocqIDE, to interact with
the Rocq Prover in a structured way.")))

(define-public rocqide
  (package
    (inherit rocq-runtime)
    (name "rocqide") ;see rocqide.opam in rocq's git
    (propagated-inputs (modify-inputs (package-propagated-inputs
                                       rocqide-server)
                         (append rocqide-server ocaml-lablgtk3-sourceview3)))
    (arguments
     (rocq-arguments "rocqide"))
    (synopsis "Rocq's IDE")
    (description
     "Rocq is an interactive theorem prover, or proof assistant.  It provides
 a formal language to write mathematical definitions, executable algorithms
and theorems together with an environment for semi-interactive development
of machine-checked proofs.

This package provides the RocqIDE, a graphical user interface for the
development of interactive proofs.")))
