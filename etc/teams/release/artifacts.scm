;;; GNU Guix --- Functional package management for GNU
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

;;; Produce a single directory with all the artifacts inside of it, with proper
;;; names. They can then be easily copied to releasedir in Makefile. The files are
;;; symlinked to save space, but they should be copied out of the store as regular
;;; files.

(load "artifacts-manifest.scm")

(manifest->union
 supported-systems-union-manifest)
