;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Fredrik Salomonsson <plattfot@posteo.net>
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


(define-module (test build-emacs-utils)
  #:use-module (guix tests)
  #:use-module (guix build emacs-utils)
  #:use-module (guix build utils)
  #:use-module ((guix utils)
                #:select (call-with-temporary-directory))
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "build-emacs-utils")
;; Only run the following tests if emacs is present.
(test-skip (if (which "emacs") 0 2))

(test-equal "emacs-batch-script: print foo from emacs"
  "foo"
  (emacs-batch-script '(princ "foo")))

(test-assert "emacs-batch-script: raise &emacs-batch-error on failure"
  (guard (c ((emacs-batch-error? c)
             (string-contains (emacs-batch-error-message c)
                              "Lisp error: (wrong-type-argument numberp \"three\")")))
    (emacs-batch-script '(mapcar 'number-to-string (list 1 2 "three")))))

(test-end "build-emacs-utils")
