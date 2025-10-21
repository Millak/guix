;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (guix build io)
  #:use-module (guix build syscalls)
  #:use-module (ice-9 format)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (system foreign)
  #:export (file->bytevector)
  ;; For convenience.
  #:re-export (PROT_READ
               PROT_NONE
               PROT_READ
               PROT_WRITE
               PROT_EXEC
               PROT_SEM
               MAP_SHARED
               MAP_PRIVATE
               MAP_FAILED
               munmap))

;;;
;;; Memory mapped files.
;;;

(define* (file->bytevector file #:key
                           (protection PROT_READ)
                           (flags (if (logtest PROT_WRITE protection)
                                      MAP_SHARED
                                      MAP_PRIVATE))
                           (offset 0))
  "Return a bytevector object that is backed by a memory mapped FILE.  This
avoids eagerly copying the full file contents into memory, instead letting the
kernel lazily page it in on demand.  The underlying memory map is
automatically unmapped when the bytevector is no longer referenced."
  (let* ((mode (format #f "rb~:[~;+~]" (and (logtest PROT_WRITE protection)
                                            (logtest MAP_SHARED flags))))
         (port (open-file file mode)))
    (call-with-port port
      (lambda (port)
        (mmap (fileno port) (- (stat:size (stat file)) offset)
              #:protection protection #:flags flags #:offset offset)))))
