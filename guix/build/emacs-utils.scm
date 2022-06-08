;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014, 2018 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Alex Kost <alezost@gmail.com>
;;; Copyright © 2018, 2020, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Liliana Marie Prikler <liliana.prikler@gmail.com>
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

(define-module (guix build emacs-utils)
  #:use-module (guix build utils)
  #:use-module (ice-9 format)
  #:export (%emacs
            emacs-batch-eval
            emacs-batch-edit-file
            emacs-batch-disable-compilation
            emacs-generate-autoloads
            emacs-byte-compile-directory

            as-display
            emacs-substitute-sexps
            emacs-substitute-variables))

;;; Commentary:
;;;
;;; Tools to programmatically edit files using Emacs,
;;; e.g. to replace entire s-expressions in elisp files.
;;;
;;; Code:

(define %emacs
  ;; The `emacs' command.
  (make-parameter "emacs"))

(define (expr->string expr)
  "Converts EXPR, an expression, into a string."
  (if (string? expr)
      expr
      (format #f "~s" expr)))

(define* (emacs-batch-eval expr #:key dynamic?)
  "Run Emacs in batch mode, and execute the Elisp code EXPR.  If DYNAMIC? is
true, evaluate using dynamic scoping."
  (invoke (%emacs) "--quick" "--batch"
          (format #f "--eval=(eval '~a ~:[t~;nil~])"
                  (expr->string expr) dynamic?)))

(define (emacs-batch-edit-file file expr)
  "Load FILE in Emacs using batch mode, and execute the elisp code EXPR."
  (invoke (%emacs) "--quick" "--batch"
          (string-append "--visit=" file)
          (string-append "--eval=" (expr->string expr))))

(define (emacs-batch-disable-compilation file)
  (emacs-batch-edit-file file
    '(progn
      (add-file-local-variable 'no-byte-compile t)
      (basic-save-buffer))))

(define (emacs-generate-autoloads name directory)
  "Generate autoloads for Emacs package NAME placed in DIRECTORY."
  (let* ((file (string-append directory "/" name "-autoloads.el"))
         (expr `(let ((backup-inhibited t)
                      (generated-autoload-file ,file))
                  (update-directory-autoloads ,directory))))
    (emacs-batch-eval expr #:dynamic? #t)))

(define* (emacs-byte-compile-directory dir)
  "Byte compile all files in DIR and its sub-directories."
  (let ((expr `(progn
                (setq byte-compile-debug t) ; for proper exit status
                (byte-recompile-directory (file-name-as-directory ,dir) 0 1))))
    (emacs-batch-eval expr)))

(define as-display         ;syntactic keyword for 'emacs-substitute-sexps'
  '(as display))

(define-syntax replacement-helper
  (syntax-rules (as-display)
    ((_ (leading-regexp replacement (as-display)))
     `(progn (goto-char (point-min))
             (re-search-forward ,leading-regexp)
             (kill-sexp)
             (insert " ")
             (insert ,(format #f "~a" replacement))))
    ((_ (leading-regexp replacement))
     `(progn (goto-char (point-min))
             (re-search-forward ,leading-regexp)
             (kill-sexp)
             (insert " ")
             (insert ,(format #f "~s" replacement))))))

(define-syntax emacs-substitute-sexps
  (syntax-rules ()
    "Substitute the S-expression immediately following the first occurrence of
LEADING-REGEXP by the string returned by REPLACEMENT in FILE.  For example:

  (emacs-substitute-sexps \"w3m.el\"
    (\"defcustom w3m-command\"
     (string-append w3m \"/bin/w3m\"))
    (\"defvar w3m-image-viewer\"
     (string-append imagemagick \"/bin/display\")))

This replaces the default values of the `w3m-command' and `w3m-image-viewer'
variables declared in `w3m.el' with the results of the `string-append' calls
above.  Note that LEADING-REGEXP uses Emacs regexp syntax.

Here is another example that uses the '(as-display)' subform to avoid having
the Elisp procedure symbol from being double quoted:
  (emacs-substitute-sexps \"gnugo.el\"
    (\"defvar gnugo-xpms\" \"#'gnugo-imgen-create-xpms\" (as-display))"
    ((_ file replacement-spec ...)
     (emacs-batch-edit-file file
       `(progn ,(replacement-helper replacement-spec)
               ...
               (basic-save-buffer))))))

(define-syntax emacs-substitute-variables
  (syntax-rules ()
    "Substitute the default value of VARIABLE by the string returned by
REPLACEMENT in FILE.  For example:

  (emacs-substitute-variables \"w3m.el\"
    (\"w3m-command\" (string-append w3m \"/bin/w3m\"))
    (\"w3m-image-viewer\" (string-append imagemagick \"/bin/display\")))

This replaces the default values of the `w3m-command' and `w3m-image-viewer'
variables declared in `w3m.el' with the results of the `string-append' calls
above.  Similarly to `emacs-substitute-sexps', the '(as-display)' subform can
be used to have the replacement formatted like `display' would, which can be
useful to avoid double quotes being added when the replacement is provided as
a string."
    ((_ file (variable replacement modifier ...) ...)
     (emacs-substitute-sexps file
       ((string-append "(def[a-z]+[[:space:]\n]+" variable "\\>")
        replacement
        modifier ...)
       ...))))

;;; emacs-utils.scm ends here
