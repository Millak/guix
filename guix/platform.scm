;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Mathieu Othacehe <othacehe@gnu.org>
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

(define-module (guix platform)
  #:use-module (guix discovery)
  #:use-module (guix memoization)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (srfi srfi-1)
  #:export (platform
            platform?
            platform-target
            platform-system
            platform-linux-architecture
            platform-glibc-dynamic-linker

            platform-modules
            platforms
            lookup-platform-by-system
            lookup-platform-by-target
            lookup-platform-by-target-or-system
            platform-system->target
            platform-target->system

            systems
            targets))


;;;
;;; Platform record.
;;;

;; Description of a platform supported by GNU Guix.
;;
;; The 'target' field must be a valid GNU triplet as defined here:
;; https://www.gnu.org/software/autoconf/manual/autoconf-2.68/html_node/Specifying-Target-Triplets.html.
;; It is used for cross-compilation purposes.
;;
;; The 'system' field is the name of the corresponding system as defined in
;; the (gnu packages bootstrap) module.  It can be for instance
;; "aarch64-linux" or "armhf-linux".  It is used to emulate a different host
;; architecture, for instance i686-linux on x86_64-linux-gnu, or armhf-linux
;; on x86_64-linux, using the QEMU binfmt transparent emulation mechanism.
;;
;; The 'linux-architecture' is only relevant if the kernel is Linux.  In that
;; case, it corresponds to the ARCH variable used when building Linux.
;;
;; The 'glibc-dynamic-linker' field is the name of Glibc's dynamic linker for
;; the corresponding system.
(define-record-type* <platform> platform make-platform
  platform?
  (target               platform-target)
  (system               platform-system)
  (linux-architecture   platform-linux-architecture
                        (default #false))
  (glibc-dynamic-linker platform-glibc-dynamic-linker))


;;;
;;; Platforms.
;;;

(define (platform-modules)
  "Return the list of platform modules."
  (all-modules (map (lambda (entry)
                      `(,entry . "guix/platforms"))
                    %load-path)
               #:warn warn-about-load-error))

(define platforms
  ;; The list of publically-known platforms.
  (memoize
   (lambda ()
     (fold-module-public-variables (lambda (obj result)
                                     (if (platform? obj)
                                         (cons obj result)
                                         result))
                                   '()
                                   (platform-modules)))))

(define (lookup-platform-by-system system)
  "Return the platform corresponding to the given SYSTEM."
  (find (lambda (platform)
          (let ((s (platform-system platform)))
            (and (string? s) (string=? s system))))
        (platforms)))

(define (lookup-platform-by-target target)
  "Return the platform corresponding to the given TARGET."
  (find (lambda (platform)
          (let ((t (platform-target platform)))
            (and (string? t) (string=? t target))))
        (platforms)))

(define (lookup-platform-by-target-or-system target-or-system)
  "Return the platform corresponding to the given TARGET or SYSTEM."
  (or (lookup-platform-by-target target-or-system)
      (lookup-platform-by-system target-or-system)))

(define (platform-system->target system)
  "Return the target matching the given SYSTEM if it exists or false
otherwise."
  (let ((platform (lookup-platform-by-system system)))
    (and=> platform platform-target)))

(define (platform-target->system target)
  "Return the system matching the given TARGET if it exists or false
otherwise."
  (let ((platform (lookup-platform-by-target target)))
    (and=> platform platform-system)))


;;;
;;; Systems & Targets.
;;;

(define (systems)
  "Return the list of supported systems."
  (delete-duplicates
   (filter-map platform-system (platforms))))

(define (targets)
  "Return the list of supported targets."
  (map platform-target (platforms)))
