;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2020 Jelle Licht <jlicht@fsfe.org>
;;; Copyright © 2019, 2021 Timothy Sample <samplet@ngyro.com>
;;; Copyright © 2021, 2022 Philip McGrath <philip@philipmcgrath.com>
;;; Copyright © 2024 Daniel Khodabakhsh <d.khodabakhsh@gmail.com>
;;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (guix build json-utils)
  #:use-module (guix build utils)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (ice-9 format)
  #:use-module (ice-9 ftw)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (with-atomic-json-file-replacement
            modify-json
            modify-json-fields
            delete-fields
            delete-json-fields
            replace-fields
            replace-json-fields
            add-fields
            add-json-fields

            &modify-json-invalid-field-value-error
            &modify-json-missing-key-error))

;;;
;;; JSON modification procedures
;;;

;; This is the function we eventually want to migrate to.
(define (with-atomic-json-file-replacement* file proc)
  "Like 'with-atomic-file-replacement', but PROC is called with a single
argument---the result of parsing FILE's contents as JSON---and should produce
a value to be written as JSON to the replacement FILE."
  (with-atomic-file-replacement file
    (lambda (in out)
      (scm->json (proc (json->scm in #:ordered #t)) out #:pretty #t))))

;; This is a deprecated version of the function that ought to be
;; removed in favor of with-atomic-json-file-replacement*'s content eventually.
;; On removal, also remove the (guix deprecation) modules and their closures
;; from node-build-system imported-modules.
(define* (with-atomic-json-file-replacement proc
                                            #:optional (file "package.json"))
  (with-atomic-json-file-replacement* file proc))

(define-deprecated/public-alias with-atomic-json-file-replacement
  with-atomic-json-file-replacement*)

;; This is the function we eventually want to migrate to.
(define* (modify-json* file #:rest modifications)
  "Modify JSON FILE with successive callbacks."
  (with-atomic-json-file-replacement file
    (apply compose modifications)))

;; Copied and adapted from (guix utils).
;; To be removed after modify-json deprecation period.
(define (delkw kw lst)
  "Remove KW and its associated value from LST, a keyword/value list such
as '(#:foo 1 #:bar 2)."
  (let loop ((lst    lst)
             (result '()))
    (match lst
      (()
       (reverse result))
      (((? (cute eq? <> kw)) value . rest)
       (append (reverse result) rest))
      ((head . tail)
       (loop tail (cons* head result))))))

;; This is a deprecated version of the function that ought to be
;; removed in favor of modify-json*'s content eventually.
;; On removal, also remove the (guix deprecation) modules and their closures
;; from node-build-system imported-modules.
(define modify-json
  (lambda* args
    (cond
     ;; Syntax from modify-json*
     ((and (pair? args) (string? (car args)) (file-exists? (car args)))
      (apply modify-json* args))
     ;; Former syntax, #:file set.
     ((memq '#:file args)
      => (lambda (file-args)
           (warning (G_ "'modify-json' 'file' keyword argument is deprecated,\
 pass the file as the first argument instead~%"))
           (let-keywords (take file-args 2) #f ((file "unreached-default"))
             (apply modify-json* file (delkw #:file args)))))
     ;; Former syntax, #:file unset.
     (else
      (warning (G_ "'modify-json' requires a file as the first argument~%"))
      (apply modify-json* "package.json" args)))))

(define-condition-type &modify-json-error &error
  modify-json-error?)

(define-condition-type &modify-json-invalid-field-value-error &modify-json-error
  modify-json-invalid-field-value-error?
  (field-path modify-json-invalid-field-value-error-field-path))

(define-condition-type &modify-json-missing-key-error &modify-json-error
  modify-json-missing-key-error?
  (key modify-json-missing-key-error-key)
  (data modify-json-missing-key-error-data))

(define* (modify-json-fields fields field-modifier
                             #:key
                             (field-path-mapper identity)
                             (insert? #f)
                             (strict? #t))
  "Return a procedure to supply to `modify-json' which modifies the specified
JSON file.  FIELDS is a list procedure-specific data structures which should
include the definition of a ``field-path'' in one of two syntaxes: dot-syntax
string such as @code{\"devDependencies.esbuild\"}, or a list of strings such
as @code{(list \"devDependencies\" \"esbuild\")}.

FIELD-MODIFIER is a procedure called with three arguments: 1) the original
field-path, e.g. \"dependencies.typescript\", 2) the field's
surrounding (parent) JSON data, as an association list, and 3) the field
name (key), e.g. \"typescript\".  The value it returns should be the modified
JSON data associated with the field; in other words, returning the second
argument without changing it is a no-op.

FIELD-PATH-MAPPER is a procedure which instructs where the field-path is
located within the field structure.  INSERT? allows the creation of the field
and any missing intermediate fields, while STRICT? causes an error to be
thrown if the exact field-path is not found in the data."
  (lambda (package)
    (fold
     (lambda (field package)
       (let* ((field-path (field-path-mapper field))
              (field-path (cond
                           ((string? field-path)
                            (string-split field-path #\.))
                           ((and (list? field-path) (every string? field-path))
                            field-path)
                           (else
                            (raise (make-compound-condition
                                    (condition (&modify-json-invalid-field-value-error
                                                (field-path field-path)))
                                    (formatted-message (G_ "\
invalid field value provided, expected string or list of strings, got ~s~%")
                                                       field-path)))))))
         (let loop ((data package)
                    (field-path field-path))
           (let* ((key (car field-path))
                  (field-missing? (not (assoc key data)))
                  (data (if (and field-missing? insert?)
                            (acons key '() data)
                            data)))
             (if (and field-missing? (not insert?))
                 (if strict?
                     (raise (make-compound-condition
                             (condition (&modify-json-missing-key-error
                                         (key key)
                                         (data data)))
                             (formatted-message
                              (G_ "key ~s was not found in data: ~y~%")
                              key data)))
                     data)
                 (if (= (length field-path) 1)
                     (field-modifier field data key)
                     (assoc-set! data key
                                 (loop (assoc-ref data key)
                                       (cdr field-path)))))))))
     package
     fields)))

(define* (delete-json-fields fields #:key (strict? #t))
  "Provides a lambda to supply to modify-json which deletes the specified
 `fields` which is a list of field-paths as mentioned in `modify-json-fields`.
 Examples:
  (delete-json-fields
   '((\"path\" \"to\" \"field\")
     \"path.to.other.field\"))"
  (modify-json-fields
   fields
   (lambda (_ data key)
     (format #t "deleting field ~s, of value: ~y~%"
             key (assoc-ref data key))
     (assoc-remove! data key))
   #:strict? strict?))

(define-deprecated/alias delete-fields delete-json-fields)

(define* (replace-json-fields fields #:key (strict? #t) insert?)
  "Provides a lambda to supply to modify-json which replaces the value of the
 supplied field. `fields` is a list of pairs, where the first element is the
 field-path and the second element is the value to replace the target with.
 Examples:
  (replace-json-fields
   '(((\"path\" \"to\" \"field\") \"new field value\")
     (\"path.to.other.field\" \"new field value\")))"
  (modify-json-fields
   fields
   (lambda (field data key)
     (let ((value (cdr field)))
       (format #t "setting field ~s to value: ~y~%" key value)
       (assoc-set! data key value)))
   #:field-path-mapper (lambda (field) (car field))
   #:insert? insert?
   #:strict? strict?))

(define-deprecated/alias replace-fields replace-json-fields)

(define* (add-json-fields fields)
  "Like `replace-json-fields', but can insert new fields as well."
  (replace-json-fields fields #:insert? #t))

(define-deprecated/alias add-fields add-json-fields)

;;; Local Variables:
;;; eval: (put 'with-atomic-json-file-replacement 'scheme-indent-function 1)
;;; eval: (put 'modify-json* 'scheme-indent-function 1)
;;; End:
