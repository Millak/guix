;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 jgart <jgart@dismail.de>
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

;;; This file returns a manifest of packages built using the texlive-build-system.
;;; It is used to assist continuous integration of the tex-team branch.

(use-modules (guix packages)
             (guix profiles)
             (guix build-system)
             (gnu packages tex))

(define %sundry-texlive-packages
  (list bibtool
        rubber
        texmaker
        texstudio
        lyx))

(packages->manifest
  (cons texlive-scheme-full
        %sundry-texlive-packages))
