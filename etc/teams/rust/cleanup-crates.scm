#!/usr/bin/env -S guix repl --
!#
;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025-2026 Hilton Chain <hako@ultrarare.space>
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

(use-modules (ice-9 regex)
             (srfi srfi-26)
             (system base compile)
             (guix diagnostics)
             (guix i18n)
             (guix utils))

(define file "gnu/packages/rust-crates.scm")

(define %unused
  (let ((warnings (open-output-string)))
    (parameterize ((current-warning-port warnings))
      (compile-file file
                    #:optimization-level 0
                    #:warning-level 0
                    #:opts '(#:warnings (unused-toplevel))))
    (map (cut match:substring <> 1)
         (list-matches "`(rust-[^']*)'" (get-output-string warnings)))))

(info (G_ "removing ~a unused definitions~%") (length %unused))

(for-each (lambda (name)
            (info (G_ "removing '~a'...~%") name)
            (and=> (find-definition-location file (string->symbol name) #:define-prefix 'define)
                   delete-expression))
          %unused)

(info (G_ "collapsing newlines~%"))

(system* "sed" "--in-place" ":a;N;$!ba;s/\\n\\n\\+/\\n\\n/g" file)
