;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Florian Pelz <pelzflorian@pelzflorian.de>
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

;; Minify Gettext PO files when synced from
;; <https://codeberg.org/guix/translations/>,
;; keeping only actually translated messages.

;; Note: This does not work for PO files of the guix domain, which needed
;; support for plural forms in (@ (guix build po) read-po-file).  The guix
;; domain's files are comparatively small and the read-po-file API would
;; have to be expanded to use records or such; it is not worth it.

(use-modules (guix build po)
             (ice-9 match)
             (ice-9 textual-ports))

(define (escape str)
  "Escape msgid or msgstr.  Replace by C-style escape sequences."
  (let* ((in (open-input-string str))
         (text (get-string-all in))
         (escaped-text-as-list
          (string-fold-right
           (lambda (char result)
             (cons (case char
                     ((#\") "\\\"")
                     ((#\\) "\\\\")
                     ((#\linefeed) "\\n")
                     ((#\return) "\\r")
                     ((#\tab) "\\t")
                     (else (string char)))
                   result))
           '()
           text))
         (escaped-text (apply string-append escaped-text-as-list)))
    (display escaped-text)))

(match (command-line)
  ((program pofile)
   (let ((input (open-input-file pofile)))
     ;; Just copy until an empty line.
     (letrec ((copy
               (lambda ()
                 (let ((next-line (get-line input)))
                   (display next-line)
                   (newline)
                   (when (> (string-length next-line) 0)
                     (copy))))))
       (copy))
     ;; Then print only translated messages.
     (for-each
      (lambda (msg)
        (match msg
          ((msgid . msgstr)
           (display "msgid \"")
           (escape msgid)
           (display "\"")
           (newline)
           (display "msgstr \"")
           (escape msgstr)
           (display "\"")
           (newline)
           (newline))))
      (read-po-file input)))))
