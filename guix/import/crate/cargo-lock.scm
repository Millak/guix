;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Murilo <murilo@disroot.org>
;;; Copyright © 2024 Luis Guilherme Coelho <lgcoelho@disroot.org>
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

(define-module (guix import crate cargo-lock)
  #:use-module (ice-9 peg)
  #:export (cargo-lock-string->scm

            crate-name
            crate-version
            crate-source
            crate-checksum
            crate-dependencies
            cargo-lock))

;;;
;;; PEG parser for ‘Cargo.lock’.
;;;

(define (cargo-lock-string->scm str)
  (peg:tree (search-for-pattern cargo-lock str)))

;; Auxiliar peg patterns
(define-peg-pattern numeric-char body
  (range #\0 #\9))

(define-peg-pattern lowercase-char body
  (range #\a #\z))

(define-peg-pattern uppercase-char body
  (range #\A #\Z))

(define-peg-pattern alphabetic-char body
  (or lowercase-char uppercase-char))

(define-peg-pattern alphanumeric-char body
  (or alphabetic-char numeric-char))

;; name
(define-peg-pattern crate-name all
  (+ (or "-" alphabetic-char
         "_" numeric-char)))

;; version
(define-peg-pattern non-negative-integer body
  (+ numeric-char))

(define-peg-pattern crate-version all
  (and non-negative-integer "."
       non-negative-integer "."
       non-negative-integer
       (? (+ (or "-" lowercase-char
                 "." uppercase-char
                 "+" numeric-char "_")))))

;; source
(define-peg-pattern crate-source all
  (and (or "registry" "git")
       "+https://"
       (+ (or "/" "." "?" "=" "-" "#" "_"
              alphanumeric-char))))

;; checksum
(define-peg-pattern crate-checksum all
  (+ (or lowercase-char numeric-char)))

;; dependency specification
(define-peg-pattern dependency-specification all
  (and crate-name (? (and (ignore " ") crate-version))))

;; dependencies
(define-peg-pattern crate-dependencies all
  (and (ignore "[\n")
       (+ (and (ignore " \"")
               (capture dependency-specification)
               (ignore "\",\n")))
       (ignore "]")))

;; crates
(define-peg-pattern crate all
  (and (ignore "[[package]]\n")
       (ignore "name = \"") (capture crate-name) (ignore "\"\n")
       (ignore "version = \"") (capture crate-version) (ignore "\"\n")
       (? (and (ignore "source = \"") (capture crate-source) (ignore "\"\n")))
       (? (and (ignore "checksum = \"") (capture crate-checksum) (ignore "\"\n")))
       (? (ignore (and "dependencies = " crate-dependencies "\n")))))

;; Cargo.lock
(define-peg-pattern cargo-lock all
  (+ (and (ignore "\n") crate)))
