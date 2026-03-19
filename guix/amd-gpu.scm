;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2026 David Elsing <david.elsing@posteo.net>
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

(define-module (guix amd-gpu)
  #:autoload (guix packages) (this-package package-properties)
  #:export (%default-amd-gpu-targets
            current-amd-gpu-targets
            current-amd-gpu-targets-string))

;;; Commentary:
;;;
;;; This module provides helpers for packages targeting AMD GPUs.
;;;
;;; Code:

(define %default-amd-gpu-targets
  '("gfx908" "gfx90a" "gfx942" "gfx1030" "gfx1100" "gfx1101" "gfx1200" "gfx1201"))

(define-syntax-rule (current-amd-gpu-targets)
  "Return the list of AMD GPU targets for this package, as a list of strings."
  (or (assoc-ref (package-properties this-package) 'amd-gpu-targets)
      %default-amd-gpu-targets))

(define-syntax-rule (current-amd-gpu-targets-string)
  "Return the list of AMD GPU targets for this package, as a string separated
by \";\".  This is useful for build options like 'AMDGPU_TARGETS' in many
CMake-based packages."
  (string-join (current-amd-gpu-targets) ";"))
