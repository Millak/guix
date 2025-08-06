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

(define-module (gnu packages hare)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages c)
  #:use-module (gnu packages commencement)
  #:use-module (gnu packages cross-base)
  #:use-module (gnu packages less)
  #:use-module (gnu packages man)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix search-paths)
  #:use-module (guix utils)
  #:export (hare-supported-systems
            target->hare-arch))

(define hare-supported-systems
  '("x86_64-linux" "aarch64-linux" "riscv64-linux"))

(define* (target->hare-arch #:optional (target (or (%current-target-system)
                                                   (%current-system))))
  (cond ((target-x86-64? target) "x86_64")
        ((target-aarch64? target) "aarch64")
        ((target-riscv64? target) "riscv64")
        (else (error "unsupported hare target" target))))

(define (cross-target? target) ; only has to work for supported arches
  (and target (not (if (%current-target-system)
                     (string=? (%current-target-system) target)
                     (string-prefix? (%current-system) target)))))

(define (tools-for-target target)
  (let* ((cross? (and=> target cross-target?))
         (with-target (lambda (f) (if cross? (f target) (f))))
         (prefix (string-upcase (with-target target->hare-arch)))
         (prefix-for (lambda (v) (if target (string-append prefix "_" v) v)))
         (toolchain (if cross? (cross-gcc-toolchain target) gcc-toolchain))
         (bin-for (lambda (t) (file-append toolchain "/bin/" (with-target t)))))
    #~((#$(prefix-for "CC") #$(bin-for cc-for-target))
       (#$(prefix-for "AS") #$(bin-for as-for-target))
       (#$(prefix-for "LD") #$(bin-for ld-for-target)))))

(define hare-config ; unified build config for hare packages
  (computed-file "hare-config.mk"
    #~(begin (use-modules (ice-9 format))
        (call-with-output-file #$output
          (lambda (port)
            (format port "include configs/linux.mk~%~:{~a=~a~%~}"
              `(("ARCH" ,#$(target->hare-arch))
                ("DEFAULT_TARGET" "$(ARCH)")
                ("STDLIB" "$(PREFIX)/share/hare")
                ("HAREPATH" "$(PREFIX)/share/hare")
                #$@(tools-for-target #f)
                #$@(tools-for-target "aarch64-linux-gnu")
                #$@(tools-for-target "riscv64-linux-gnu")
                #$@(tools-for-target "x86_64-linux-gnu"))))))))

(define-public harec
  (package
    (name "harec")
    (version "0.25.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.sr.ht/~sircmpwn/harec")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
                (base32
                  "0qyhni011116wc194kkybmiphmi1cak0n8kxgiq7v174xsh9irp7"))))
    (build-system gnu-build-system)
    (arguments
      (list #:modules `((ice-9 format) ,@%default-gnu-modules)
            #:make-flags #~(list (string-append "PREFIX=" #$output)
                                 (string-append "VERSION=" #$version))
            #:phases
            #~(modify-phases %standard-phases
                (replace 'configure
                  (lambda _ (copy-file #$hare-config "config.mk"))))))
    (native-inputs (list qbe))
    (supported-systems hare-supported-systems)
    (home-page "https://harelang.org")
    (synopsis "Harelang bootstrap compiler")
    (description "@code{harec} is a bootstrap compiler written in C for the Hare
programming language. For general Hare programming, see the @code{hare}
package.")
    (license license:gpl3)))
