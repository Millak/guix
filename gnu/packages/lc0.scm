;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 zamfofex <zamfofex@twdb.moe>
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

(define-module (gnu packages lc0)
  #:use-module (guix build-system meson)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages c)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python))

(define-public lc0
  (package
    (name "lc0")
    (version "0.30.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/LeelaChessZero/lc0")
                    (commit (string-append "v" version))
                    ;; Only a few source files are in one Git submodules
                    ;; (rather than there being bundled projects).  These
                    ;; files are in a different repository just because they
                    ;; are used across multiple repositories of the Leela
                    ;; Chess Zero project.
                    (recursive? #t)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1m7k8m8iz4pxv3h9g2j1dkgryi4k9c1bcg3fx5j7ii4ysif63kj3"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-Ddnnl=true"
              (string-append "-Ddnnl_dir="
                             #$(this-package-input "oneapi-dnnl")))))
    (native-search-paths
     (list (search-path-specification
            (variable "XDG_DATA_DIRS")
            (files '("share")))))
    (inputs (list eigen oneapi-dnnl zlib))
    (native-inputs (list googletest ispc pkg-config python))
    (synopsis "Chess engine based on neural networks")
    (description
     "Leela Chess Zero is a UCI-compliant chess engine designed to play chess
using neural networks.  This package does not provide a neural network, which
is necessary to use Leela Chess Zero and should be installed separately.")
    (home-page "https://lczero.org")
    (license license:gpl3+)))
