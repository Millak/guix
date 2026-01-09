;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024, 2026 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu installer kernel)
  #:use-module (gnu system hurd)
  #:use-module (guix read-print)
  #:export (kernel->configuration))

(define-syntax-rule (G_ str)
  ;; In this file, translatable strings are annotated with 'G_' so xgettext
  ;; catches them, but translation happens later on at run time.
  str)

(define (kernel->configuration kernel dry-run?)
  (if (string-prefix? "Hurd" kernel)
      `((kernel %hurd-default-operating-system-kernel)
        ,(comment (G_ ";; \"noide\" disables the gnumach IDE driver, enabling rumpdisk.\n"))
        (kernel-arguments '("noide"))
        (firmware '())
        (hurd hurd)
        (locale-libcs (list glibc/hurd))
        (name-service-switch #f)
        (essential-services (hurd-default-essential-services this-operating-system))
        (privileged-programs '())
        (setuid-programs %setuid-programs/hurd))
      '()))
