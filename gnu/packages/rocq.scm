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

(define-module (gnu packages rocq))

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
