;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (test-json-utils)
  #:use-module (guix build json-utils)
  #:use-module (guix tests)
  #:use-module (json)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-64))

(define sample-json
  '(("name" . "my-package")
    ("version" . "1.0.0")
    ("dependencies" . (("foo" . "^1.0.0")
                       ("bar" . "^2.0.0")))
    ("devDependencies" . (("baz" . "^3.0.0")
                          ("qux" . "^4.0.0")))
    ("scripts" . (("build" . "make build")
                  ("test" . "make test")))))

;; Many json-utils procedures use assoc-set! and assoc-remove! which
;; mutate alist structure in place.  Use copy-tree to obtain a fresh
;; deep copy for each test.
(define (fresh-sample-json)
  (copy-tree sample-json))

(define (package.json)
  "Sample package.json file as a JSON string."
  (call-with-output-string
   (cute scm->json sample-json <> #:pretty #t)))

(define (with-atomic-file-replacement/mock _ proc)
  "Mock for with-atomic-file-replacement: reads from package.json procedure,
captures the written output as a string."
  (call-with-input-string (package.json)
    (lambda (in)
      (call-with-output-string
       (cute proc in <>)))))

(define* (modify-json* #:rest all-arguments)
  "Mock modify-json* with input from the package.json procedure."
  (mock ((guix build utils) with-atomic-file-replacement
         with-atomic-file-replacement/mock)
        (json-string->scm (apply modify-json all-arguments)
                          #:ordered #t)))

(test-begin "json-utils")

;;;
;;; with-atomic-json-file-replacement
;;;

(test-equal "with-atomic-json-file-replacement, modify top-level field"
  "2.0.0"
  (assoc-ref
   (modify-json* (cut assoc-set! <> "version" "2.0.0"))
   "version"))

;;;
;;; delete-json-fields
;;;

(test-assert "delete-json-fields, single top-level field"
  (let ((result ((delete-json-fields '("version")) (fresh-sample-json))))
    (and (not (assoc-ref result "version"))
         (assoc-ref result "name"))))

(test-assert "delete-json-fields, multiple top-level fields"
  (let ((result ((delete-json-fields '("version" "name")) (fresh-sample-json))))
    (and (not (assoc-ref result "version"))
         (not (assoc-ref result "name"))
         (assoc-ref result "dependencies"))))

(test-assert "delete-json-fields, nested field with dot syntax"
  (let ((result ((delete-json-fields '("dependencies.foo")) (fresh-sample-json))))
    (and (not (assoc-ref (assoc-ref result "dependencies") "foo"))
         (assoc-ref (assoc-ref result "dependencies") "bar"))))

(test-assert "delete-json-fields, nested field with list syntax"
  (let ((result ((delete-json-fields '(("dependencies" "bar")))
                 (fresh-sample-json))))
    (and (not (assoc-ref (assoc-ref result "dependencies") "bar"))
         (assoc-ref (assoc-ref result "dependencies") "foo"))))

(test-assert "delete-json-fields, missing field with strict? #f"
  (let ((result ((delete-json-fields '("nonexistent") #:strict? #f)
                 (fresh-sample-json))))
    (assoc-ref result "name")))

(test-error "delete-json-fields, missing field with strict? #t raises error"
  &modify-json-missing-key-error
  ((delete-json-fields '("nonexistent") #:strict? #t) (fresh-sample-json)))

(test-assert "delete-json-fields, missing nested field with strict? #f"
  (let ((result ((delete-json-fields '("dependencies.nonexistent")
                                     #:strict? #f)
                 (fresh-sample-json))))
    (assoc-ref (assoc-ref result "dependencies") "foo")))

;;;
;;; replace-json-fields
;;;

(test-equal "replace-json-fields, single top-level field"
  "new-name"
  (assoc-ref
   ((replace-json-fields '(("name" . "new-name"))) (fresh-sample-json))
   "name"))

(test-equal "replace-json-fields, nested field with dot syntax"
  "^5.0.0"
  (assoc-ref
   (assoc-ref
    ((replace-json-fields '(("dependencies.foo" . "^5.0.0")))
     (fresh-sample-json))
    "dependencies")
   "foo"))

(test-equal "replace-json-fields, nested field with list syntax"
  "^6.0.0"
  (assoc-ref
   (assoc-ref
    ((replace-json-fields '((("dependencies" "bar") . "^6.0.0")))
     (fresh-sample-json))
    "dependencies")
   "bar"))

(test-equal "replace-json-fields, multiple replacements"
  '("new-name" . "2.0.0")
  (let ((result ((replace-json-fields
                  '(("name" . "new-name")
                    ("version" . "2.0.0")))
                 (fresh-sample-json))))
    (cons (assoc-ref result "name")
          (assoc-ref result "version"))))

(test-error "replace-json-fields, missing field with strict? #t raises error"
  &modify-json-missing-key-error
  ((replace-json-fields '(("nonexistent" . "value")) #:strict? #t)
   (fresh-sample-json)))

(test-assert "replace-json-fields, missing field with strict? #f"
  (let ((result ((replace-json-fields '(("nonexistent" . "value"))
                                      #:strict? #f)
                 (fresh-sample-json))))
    (equal? (assoc-ref result "name") "my-package")))

;;;
;;; add-json-fields
;;;
(test-equal "add-json-fields"
  #("test")
  (assoc-ref (modify-json*
                 (add-json-fields '(("exclude" . #("test")))))
             "exclude"))

(test-equal "add-json-fields, recursive"
  "^25.9.2"
  (assoc-ref (assoc-ref (modify-json*
                            (add-json-fields '(("devDependencies2.@types/node"
                                                . "^25.9.2"))))
                        "devDependencies2")
             "@types/node"))

(test-equal "add-json-fields, recursive, partially preexisting"
  "^9.9.9"
  (assoc-ref (assoc-ref (modify-json*
                            (add-json-fields '(("devDependencies.@types/dummy"
                                                . "^9.9.9"))))
                        "devDependencies")
             "@types/dummy"))

;;;
;;; modify-json-fields
;;;

(test-equal "modify-json-fields, custom modifier on top-level field"
  "MY-PACKAGE"
  (assoc-ref
   ((modify-json-fields '("name")
      (lambda (field data key)
        (assoc-set! data key (string-upcase (assoc-ref data key)))))
    (fresh-sample-json))
   "name"))

(test-equal "modify-json-fields, custom modifier on nested field"
  "^1.0.0-patched"
  (assoc-ref
   (assoc-ref
    ((modify-json-fields '("dependencies.foo")
       (lambda (field data key)
         (assoc-set! data key
                     (string-append (assoc-ref data key) "-patched"))))
     (fresh-sample-json))
    "dependencies")
   "foo"))

(test-assert "modify-json-fields, insert? creates missing field"
  (let ((result ((modify-json-fields '("newField")
                   (lambda (field data key)
                     (assoc-set! data key "inserted"))
                   #:insert? #t)
                 (fresh-sample-json))))
    (equal? (assoc-ref result "newField") "inserted")))

(test-assert "modify-json-fields, insert? creates nested missing fields"
  (let ((result ((modify-json-fields '("newSection.newKey")
                   (lambda (field data key)
                     (assoc-set! data key "deep-insert"))
                   #:insert? #t)
                 (fresh-sample-json))))
    (equal? (assoc-ref (assoc-ref result "newSection") "newKey")
            "deep-insert")))

(test-assert "modify-json-fields, field-path-mapper"
  (let ((result ((modify-json-fields '(("name" . "REPLACED"))
                   (lambda (field data key)
                     (assoc-set! data key (cdr field)))
                   #:field-path-mapper car)
                 (fresh-sample-json))))
    (equal? (assoc-ref result "name") "REPLACED")))

(test-error "modify-json-fields, invalid field-path raises error"
  &modify-json-invalid-field-value-error
  ((modify-json-fields '(42) (lambda (field data key) data))
   (fresh-sample-json)))

(test-equal "modify-json, ordering"
  '(("baz" . "^3.0.0")
    ("qux" . "^4.0.0"))
  (assoc-ref (modify-json* (modify-json-fields
                            '("devDependencies.qux")
                            (lambda (field-path data key)
                              data)))   ;no-op
             "devDependencies"))

;;;
;;; modify-json (deprecated wrapper)
;;;

(test-equal "modify-json, single modification"
  "modified"
  (assoc-ref
   (modify-json* (cut assoc-set! <> "name" "modified"))
   "name"))

(test-equal "modify-json, chained modifications"
  '("chain-name" . "3.0.0")
  (let ((result (modify-json*
                 (cut assoc-set! <> "name" "chain-name")
                 (cut assoc-set! <> "version" "3.0.0"))))
    (cons (assoc-ref result "name")
          (assoc-ref result "version"))))

;;;
;;; Integration: delete-json-fields and replace-json-fields with modify-json
;;;

(test-equal "modify-json + delete-json-fields integration"
  #f
  (assoc-ref
   (modify-json* (delete-json-fields '("devDependencies")))
   "devDependencies"))

(test-equal "modify-json + replace-json-fields integration"
  "^9.0.0"
  (assoc-ref
   (assoc-ref
    (modify-json* (replace-json-fields '(("dependencies.foo" . "^9.0.0"))))
    "dependencies")
   "foo"))

(test-equal "modify-json + chained delete and replace integration"
  '(#f . "replaced-name")
  (let ((result (modify-json*
                  (delete-json-fields '("devDependencies"))
                  (replace-json-fields '(("name" . "replaced-name"))))))
    (cons (assoc-ref result "devDependencies")
          (assoc-ref result "name"))))

(test-end "json-utils")

;; Local Variables:
;; eval: (put 'modify-json-fields 'scheme-indent-function 1)
;; End:
