;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Nicolas Goaziou <mail@nicolasgoaziou.fr>
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

(define-module (gnu packages weather)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages golang))

(define-public wego
  (package
    (name "wego")
    (version "2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/schachmat/wego")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bji9fywa0kg29zj1vrwq7l5f18hh0lcz6rl6sppi5id0qbjpiwl"))))
    (build-system go-build-system)
    (arguments '(#:import-path "github.com/schachmat/wego"))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-github-com-rivo-uniseg
           go-github-com-mattn-go-isatty
           go-github-com-schachmat-ingo
           go-github-com-mattn-go-runewidth
           go-github-com-mattn-go-colorable))
    (home-page "https://github.com/schachmat/wego")
    (synopsis "Weather client for the terminal")
    (description "Wego is a weather client for the terminal.  It shows
forecast for one or seven days.  Displayed information includes temperature
range---felt and measured---, wind speed and direction, viewing distance,
precipitation amount and probability.")
    (license license:isc)))
