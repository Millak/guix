;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Lilah Tascheter <lilah@lunabee.space>
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

(define-module (guix build hare-build-system)
  #:use-module ((guix build gnu-build-system) #:prefix gnu:)
  #:use-module (guix build utils)
  #:export (%standard-phases
            hare-build))

(define (invoke-gnu-phase name args)
  (apply (assoc-ref gnu:%standard-phases name) args))

(define* (set-paths #:key inputs #:allow-other-keys #:rest args)
  "Sets proper PATH, HAREPATH, and pulled-in search paths."
  (invoke-gnu-phase 'set-paths args)
  ;; XXX: bag->derivation doesn't consider non-native search paths
  (set-path-environment-variable "HAREPATH" '("share/hare")
    (map cdr (alist-delete "source" inputs))))

(define* (build #:key hare-arch (make-flags '()) #:allow-other-keys #:rest args)
  "Builds Hare binaries through a provided Makefile."
  ;; HAREFLAGS seems to be typically supported, but we have to just guess.
  ;; Later args override earlier ones, so packages can override HAREFLAGS.
  (let* ((hareflags (format #f "HAREFLAGS=-a ~a" hare-arch))
         (flags (cons* hareflags make-flags)))
    (invoke-gnu-phase 'build (append args (list #:make-flags flags)))))

(define* (install #:key (make-flags '()) binary-output module-output
                  #:allow-other-keys #:rest args)
  "Install Hare modules and binaries through a provided Makefile."
  ;; Same deal here as in build. These seem to be the most common relevant
  ;; variables. Packages can override as needed.
  (let* ((prefix (format #f "PREFIX=~a" binary-output)) ; for manpages mostly
         (bindir (format #f "BINDIR=~a/bin" binary-output))
         (srcdir (format #f "SRCDIR=~a/share/hare" module-output))
         (moddir (format #f "THIRDPARTYDIR=~a/share/hare" module-output))
         (flags (cons* prefix bindir srcdir moddir make-flags)))
    (invoke-gnu-phase 'install (append args (list #:make-flags flags)))))

(define %standard-phases
  (modify-phases gnu:%standard-phases
    (replace 'set-paths set-paths)
    (delete 'bootstrap)
    (delete 'configure)
    (replace 'build build)
    (replace 'install install)))

(define hare-build gnu:gnu-build)
