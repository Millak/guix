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

(define-module (gnu installer newt kernel)
  #:use-module (gnu installer newt page)
  #:use-module (guix i18n)
  #:use-module (guix utils)
  #:export (run-kernel-page))

(define (run-kernel-page)
  ;; TRANSLATORS: "Hurd" is a proper noun and must not be translated.
  (let* ((hurd-x86 (G_ "Hurd 32-bit (experimental)"))
         (hurd-x86_64 (G_ "Hurd 64-bit (highly experimental!)"))
         (linux-libre "Linux Libre")
         (kernels (parameterize ((%current-target-system #f))
                    `(,linux-libre
                      ,@(cond ((target-x86-64?)
                               (list hurd-x86 hurd-x86_64))
                              ((target-x86?)
                               (list hurd-x86))
                              (else
                               '())))))
         (default (cond ((equal? (%current-target-system) "i586-pc-gnu")
                         hurd-x86)
                        ((equal? (%current-target-system) "x86_64-pc-gnu")
                         hurd-x86_64)
                        (else
                         linux-libre)))
         (result
          (run-listbox-selection-page
           #:title (G_ "Kernel")
           #:info-text
           ;; TRANSLATORS: "Hurd" is a proper noun and must not be translated.
           ;; TRANSLATORS: "Linux Libre" is a literal and must not be translated.
           (G_ "Please select a kernel.  When in doubt, choose \"Linux Libre\".
The Hurd is offered as a technology preview and development aid; many packages \
are not yet available in Guix, such as a desktop environment or even a \
windowing system (X, Wayland).")
           #:listbox-items kernels
           #:listbox-item->text identity
           #:listbox-default-item default
           #:sort-listbox-items? #f               ;keep Linux first
           #:button-text (G_ "Back")
           #:button-callback-procedure
           (lambda _
             (abort-to-prompt 'installer-step 'abort)))))
    (let ((target (cond ((equal? result hurd-x86)
                         "i586-pc-gnu")
                        ((equal? result hurd-x86_64)
                         "x86_64-pc-gnu")
                        (else
                         #f))))
      (%current-target-system target))
    result))
