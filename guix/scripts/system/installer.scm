;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (guix scripts system installer)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-37)
  #:use-module (gnu installer)
  #:use-module (guix scripts)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:export (guix-system-installer))

;;; Commentary:
;;;
;;; Implement the 'guix system installer' command, which runs the installer,
;;; directly as a Guix command, also in dry-run mode.
;;;
;;; Code:

(define %options
  (list (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))
        (option '(#\h "help") #f #f
                (lambda args
                  (show-help)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix system installer")))))

(define (show-help)
  (display (G_ "Usage: guix system installer [OPTION]...
Run the system installer.\n"))
  (display (G_ "
  -n, --dry-run          skip network setup, partitioning, and actual install"))
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))


;;;
;;; Entry Point.
;;;
(define-command (guix-system-installer . args)
  (synopsis "run the graphical installer program")

  (with-error-handling
    (let* ((opts     (parse-command-line args %options '((dry-run? . #f))
                                         #:build-options? #f))
           (dry-run? (assoc-ref opts 'dry-run?)))
      (run-installer #:dry-run? dry-run?))))
