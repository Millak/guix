;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2025 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (guix build-system luanti)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix utils)
  #:export (luanti-mod-build-system))

;;
;; Build procedure for luanti mods.  This is implemented as an extension
;; of ‘copy-build-system’.
;;
;; Code:

(define %luanti-build-system-modules
  ;; Build-side modules imported by default.
  `((guix build luanti-build-system)
    ,@%copy-build-system-modules))

(define %default-modules
  ;; Modules in scope in the build-side environment.
  '((guix build gnu-build-system)
    (guix build luanti-build-system)
    (guix build utils)))

(define (standard-luanti-packages)
  "Return the list of (NAME PACKAGE OUTPUT) or (NAME PACKAGE) tuples of
standard packages used as implicit inputs of the Luanti build system,
resolved lazily."
  `(("xvfb-run" ,(@* (gnu packages xorg) xvfb-run))
    ("optipng" ,(@* (gnu packages image) optipng))
    ("luanti" ,(@* (gnu packages luanti) luanti))
    ("luanti-game" ,(@* (gnu packages luanti) minetest-game))
    ,@(filter (lambda (input)
                (member (car input)
                        '("libc" "tar" "gzip" "bzip2" "xz" "locales")))
              (standard-packages))))

(define* (lower-mod name #:key (implicit-inputs? #t) #:allow-other-keys
                    #:rest arguments)
  (define lower (build-system-lower gnu-build-system))
  (apply lower
         name
         (substitute-keyword-arguments arguments
           ;; luanti-mod-build-system adds implicit inputs by itself,
           ;; so don't let gnu-build-system add its own implicit inputs
           ;; as well.
           ((#:implicit-inputs? implicit-inputs? #t)
            #f)
           ((#:implicit-cross-inputs? implicit-cross-inputs? #t)
            #f)
           ((#:imported-modules imported-modules %luanti-build-system-modules)
            imported-modules)
           ((#:modules modules %default-modules)
            modules)
           ((#:phases phases '%standard-phases)
            phases)
           ;; Ensure nothing sneaks into the closure.
           ((#:allowed-references allowed-references '())
            allowed-references)
           ;; Add the implicit inputs.
           ((#:native-inputs native-inputs '())
            (if implicit-inputs?
                (append native-inputs (standard-luanti-packages))
                native-inputs)))))

(define luanti-mod-build-system
  (build-system
    (name 'luanti-mod)
    (description "The build system for Luanti mods")
    (lower lower-mod)))

;;; luanti.scm ends here
