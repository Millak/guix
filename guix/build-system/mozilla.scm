;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (guix build-system mozilla)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system)
  #:use-module (guix utils)
  #:export (mozilla-build-system))

;;
;; Build procedure for packages using Autotools with the Mozillian conventions
;; for --target, --host and --build, which are different from the GNU
;; conventions.
;;
;; Code:

(define* (lower-mozilla name #:key system target #:allow-other-keys
                        #:rest arguments)
  (define lower (build-system-lower gnu-build-system))
  (if target
      (apply lower name
             (substitute-keyword-arguments arguments
               ;; Override --target and --host to what Mozillian configure
               ;; scripts expect.
               ((#:configure-flags configure-flags ''())
                `(cons* ,(string-append "--target=" target)
                        ,(string-append "--host=" (nix-system->gnu-triplet system))
                        ,configure-flags))))
      (apply lower name arguments))) ; not cross-compiling

(define mozilla-build-system
  (build-system
    (name 'mozilla)
    (description "The build system for Mozilla software using the Autotools")
    (lower lower-mozilla)))

;;; mozilla.scm ends here
