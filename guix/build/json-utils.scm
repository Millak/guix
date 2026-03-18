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
  #:export (with-atomic-json-file-replacement
            modify-json
            modify-json-fields
            delete-fields
            replace-fields
            add-fields))

;;;
;;; JSON modification procedures
;;;

(define* (with-atomic-json-file-replacement proc
                                            #:optional (file "package.json"))
  "Like 'with-atomic-file-replacement', but PROC is called with a single
argument---the result of parsing FILE's contents as JSON---and should produce
a value to be written as JSON to the replacement FILE."
  (with-atomic-file-replacement file
    (lambda (in out)
      (scm->json (proc (json->scm in #:ordered #t)) out #:pretty #t))))

(define* (modify-json #:key (file "package.json") #:rest all-arguments)
  "Provide package.json modifying callbacks such as (delete-dependencies ...)"
  (let ((modifications
         (let loop ((arguments all-arguments))
           (cond
            ((null? arguments) '())
            ((keyword? (car arguments)) (loop (cddr arguments)))
            (else (cons (car arguments) (loop (cdr arguments))))))))
    (with-atomic-json-file-replacement
     (lambda (package)
       (fold (lambda (modification package)
               (modification package))
             package
             modifications))
     file)))

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
                           (else (error (format #f "\
invalid field value provided, expected string or list of strings, got ~s~%"
                                                field-path))))))
         (let loop ((data package)
                    (field-path field-path))
           (let* ((key (car field-path))
                  (field-missing? (not (assoc key data)))
                  (data (if (and field-missing? insert?)
                            (acons key '() data)
                            data)))
             (if field-missing?
                 (if strict?
                     (error (format #f "key ~s was not found in data: ~y~%"
                                    key data))
                     data)
                 (if (= (length field-path) 1)
                     (field-modifier field data key)
                     (assoc-set! data key
                                 (loop (assoc-ref data key)
                                       (cdr field-path)))))))))
     package
     fields)))

(define* (delete-fields fields #:key (strict? #t))
  "Provides a lambda to supply to modify-json which deletes the specified
 `fields` which is a list of field-paths as mentioned in `modify-json-fields`.
 Examples:
  (delete-fields '(
    (\"path\" \"to\" \"field\")
    \"path.to.other.field\"))"
  (modify-json-fields
   fields
   (lambda (_ data key)
     (format #t "deleting field ~s, of value: ~y~%"
             key (assoc-ref data key))
     (assoc-remove! data key))
   #:strict? strict?))

(define* (replace-fields fields #:key (strict? #t) insert?)
  "Provides a lambda to supply to modify-json which replaces the value of the
 supplied field. `fields` is a list of pairs, where the first element is the
 field-path and the second element is the value to replace the target with.
 Examples:
  (replace-fields '(
    ((\"path\" \"to\" \"field\") \"new field value\")
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

(define* (add-fields fields)
  "Like `replace-fields', but can insert new fields as well."
  (replace-fields fields #:insert? #t))

;;; Local Variables:
;;; eval: (put 'with-atomic-json-file-replacement 'scheme-indent-function 1)
;;; End:
