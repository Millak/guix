;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu services configuration utils)
  #:use-module (ice-9 string-fun)
  #:export (uglify-snake-case))

(define (uglify-snake-case field-name)
  "Serializes FIELD-NAME, a field name from @code{(gnu services configuration)},
to a downcased, snake case string representation of the field name.  Trailing
@code{?} in the name are dropped and dashes are replaced with underscores.

For example the procedure would convert @code{'A-Field?} to @code{\"a_field\"}."
  (define str (symbol->string field-name))
  (string-downcase
   (string-replace-substring
    (if (string-suffix? "?" str)
        (string-drop-right str 1)
        str)
    "-" "_")))
