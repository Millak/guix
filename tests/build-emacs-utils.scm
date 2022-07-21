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
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64))

(test-begin "build-emacs-utils")
;; Only run the following tests if emacs is present.
(test-skip (if (which "emacs") 0 5))

(test-equal "emacs-batch-script: print foo from emacs"
  "foo"
  (emacs-batch-script '(princ "foo")))

(test-assert "emacs-batch-script: raise &emacs-batch-error on failure"
  (guard (c ((emacs-batch-error? c)
             ;; The error message format changed between Emacs 27 and Emacs
             ;; 28.
             (string-match "[Ww]rong.*argument.*numberp.*\"three\""
                           (emacs-batch-error-message c))))
    (emacs-batch-script '(mapcar 'number-to-string (list 1 2 "three")))))

(call-with-temporary-directory
 (lambda (directory)
   (let ((mock-elisp-file (string-append directory "/foo.el")))
     (call-with-output-file mock-elisp-file
       (lambda (port)
         (display ";;; foo --- mock emacs package -*- lexical-binding: t -*-

;; Created: 4 Jun 2022
;; Keywords: lisp test
;; Version: 1.0.0
;;; Commentary:
;;; Code:
;;; foo.el ends here
"
                  port)))
     (test-equal "emacs-header-parse: fetch version"
       "1.0.0"
       (emacs-header-parse "version" mock-elisp-file))
     (test-equal "emacs-header-parse: fetch keywords"
       "lisp test"
       (emacs-header-parse "keywords" mock-elisp-file))
     (test-equal "emacs-header-parse: fetch nonexistent author"
       "nil"
       (emacs-header-parse "author" mock-elisp-file)))))

(test-end "build-emacs-utils")
