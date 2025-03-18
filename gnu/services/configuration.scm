;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
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

(define-module (gnu services configuration)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module ((guix utils) #:select (source-properties->location))
  #:use-module ((guix diagnostics)
                #:select (formatted-message location-file &error-location
                          warning))
  #:use-module ((guix modules) #:select (file-name->module-name))
  #:use-module (guix i18n)
  #:autoload   (texinfo) (texi-fragment->stexi)
  #:autoload   (texinfo serialize) (stexi->texi)
  #:use-module (ice-9 curried-definitions)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-171)
  #:export (configuration-field
            configuration-field-name
            configuration-field-type
            configuration-missing-field
            configuration-field-error
            configuration-field-sanitizer
            configuration-field-serializer
            configuration-field-getter
            configuration-field-default-value-thunk
            configuration-field-documentation

            configuration-error?

            define-configuration
            define-configuration/no-serialization
            no-serialization

            empty-serializer?
            tfilter-maybe-value
            base-transducer

            serialize-configuration
            define-maybe
            define-maybe/no-serialization
            %unset-value
            maybe-value
            maybe-value-set?
            generate-documentation
            configuration->documentation
            empty-serializer
            serialize-package

            filter-configuration-fields

            interpose
            list-of

            list-of-packages?
            list-of-strings?
            list-of-symbols?
            alist?
            serialize-file-like
            text-config?
            serialize-text-config
            generic-serialize-alist-entry
            generic-serialize-alist))

;;; Commentary:
;;;
;;; Syntax for creating Scheme bindings to complex configuration files.
;;;
;;; Code:

(define-condition-type &configuration-error &error
  configuration-error?)

(define (configuration-error message)
  (raise (condition (&message (message message))
                    (&configuration-error))))
(define (configuration-field-error loc field value)
  (raise (apply
          make-compound-condition
          (formatted-message (G_ "invalid value ~s for field '~a'")
                             value field)
          (condition (&configuration-error))
          (if loc
              (list (condition
                     (&error-location (location loc))))
              '()))))

(define (configuration-missing-field kind field)
  (configuration-error
   (format #f "~a configuration missing required field ~a" kind field)))
(define (configuration-missing-default-value kind field)
  (configuration-error
   (format #f "The field `~a' of the `~a' configuration record \
does not have a default value" field kind)))

(define-record-type* <configuration-field>
  configuration-field make-configuration-field configuration-field?
  (name configuration-field-name)
  (type configuration-field-type)
  (getter configuration-field-getter)
  (predicate configuration-field-predicate)
  (sanitizer configuration-field-sanitizer)
  (serializer configuration-field-serializer)
  (default-value-thunk configuration-field-default-value-thunk)
  (documentation configuration-field-documentation))

(define (empty-serializer? field)
  "Predicate that checks whether FIELD is exempt from serialization."
  (eq? empty-serializer
       (configuration-field-serializer field)))

(define (tfilter-maybe-value config)
  "Return a transducer for CONFIG that removes all maybe-type fields whose
value is '%unset-marker."
  (tfilter (lambda (field)
             (let ((field-value ((configuration-field-getter field) config)))
               (maybe-value-set? field-value)))))

(define (base-transducer config)
  "Return a transducer for CONFIG that calls the serializing procedures only
for fields marked for serialization and whose values are not '%unset-marker."
  (compose (tremove empty-serializer?)
           ;; Only serialize fields whose value isn't '%unset-marker%.
           (tfilter-maybe-value config)
           (tmap (lambda (field)
                   ((configuration-field-serializer field)
                    (configuration-field-name field)
                    ((configuration-field-getter field) config))))))

(define (serialize-configuration config fields)
  "Return a G-expression that contains the values corresponding to the
FIELDS of CONFIG, a record that has been generated by `define-configuration'.
The G-expression can then be serialized to disk by using something like
`mixed-text-file'."
  #~(string-append
     #$@(list-transduce (base-transducer config) rcons fields)))

(define-syntax-rule (id ctx parts ...)
  "Assemble PARTS into a raw (unhygienic) identifier."
  (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))

(define (define-maybe-helper serialize? prefix syn)
  (syntax-case syn ()
    ((_ stem)
     (with-syntax
         ((stem?            (id #'stem #'stem #'?))
          (maybe-stem?      (id #'stem #'maybe- #'stem #'?))
          (serialize-stem   (if prefix
                                (id #'stem prefix #'serialize- #'stem)
                                (id #'stem #'serialize- #'stem)))
          (serialize-maybe-stem (if prefix
                                    (id #'stem prefix #'serialize-maybe- #'stem)
                                    (id #'stem #'serialize-maybe- #'stem))))
       #`(begin
           (define (maybe-stem? val)
             (or (not (maybe-value-set? val))
                 (stem? val)))
           #,@(if serialize?
                  (list #'(define (serialize-maybe-stem field-name val)
                            (if (stem? val)
                                (serialize-stem field-name val)
                                "")))
                  '()))))))

(define-syntax define-maybe
  (lambda (x)
    (syntax-case x (no-serialization prefix)
      ((_ stem (no-serialization))
       (define-maybe-helper #f #f #'(_ stem)))
      ((_ stem (prefix serializer-prefix))
       (define-maybe-helper #t #'serializer-prefix #'(_ stem)))
      ((_ stem)
       (define-maybe-helper #t #f #'(_ stem))))))

(define-syntax-rule (define-maybe/no-serialization stem)
  (define-maybe stem (no-serialization)))

(define (normalize-field-type+def s)
  (syntax-case s ()
    ((field-type def)
     (identifier? #'field-type)
     (values #'(field-type def)))
    ((field-type)
     (identifier? #'field-type)
     (values #'(field-type %unset-value)))
    (field-type
     (identifier? #'field-type)
     (values #'(field-type %unset-value)))))

(define (define-configuration-helper serialize? serializer-prefix syn)

  (define (normalize-extra-args s)
    "Extract and normalize arguments following @var{doc}."
    (let loop ((s s)
               (sanitizer* #f)
               (serializer* #f))
      (syntax-case s (sanitizer serializer empty-serializer)
        (((sanitizer proc) tail ...)
         (if sanitizer*
             (syntax-violation 'sanitizer
                               "duplicate entry" #'proc)
             (loop #'(tail ...) #'proc serializer*)))
        (((serializer proc) tail ...)
         (if serializer*
             (syntax-violation 'serializer
                               "duplicate or conflicting entry" #'proc)
             (loop #'(tail ...) sanitizer* #'proc)))
        ((empty-serializer tail ...)
         (if serializer*
             (syntax-violation 'empty-serializer
                               "duplicate or conflicting entry" #f)
             (loop #'(tail ...) sanitizer* #'empty-serializer)))
        (()  ; stop condition
         (values (list sanitizer* serializer*)))
        ((proc)  ; TODO: deprecated, to be removed.
         (not (or sanitizer* serializer*))
         (begin
           (warning #f (G_ "specifying serializers after documentation is \
deprecated, use (serializer ~a) instead~%") (syntax->datum #'proc))
           (values (list #f #'proc)))))))

  (syntax-case syn ()
    ((_ stem (field field-type+def doc extra-args ...) ...)
     (with-syntax
         ((((field-type def) ...)
           (map normalize-field-type+def #'(field-type+def ...)))
          (((sanitizer* serializer*) ...)
           (map normalize-extra-args #'((extra-args ...) ...))))
       (with-syntax
           (((field-getter ...)
             (map (lambda (field)
                    (id #'stem #'stem #'- field))
                  #'(field ...)))
            ((field-predicate ...)
             (map (lambda (type)
                    (id #'stem type #'?))
                  #'(field-type ...)))
            ((field-default ...)
             (map (match-lambda
                    ((field-type default-value)
                     default-value))
                  #'((field-type def) ...)))
            ((field-sanitizer ...)
             #'(sanitizer* ...))
            ((field-serializer ...)
             (map (lambda (type proc)
                    (and serialize?
                         (or proc
                             (if serializer-prefix
                                 (id #'stem serializer-prefix #'serialize- type)
                                 (id #'stem #'serialize- type)))))
                  #'(field-type ...)
                  #'(serializer* ...))))
         (define (default-field-sanitizer name pred)
           ;; Define a macro for use as a record field sanitizer, where NAME
           ;; is the name of the field and PRED is the predicate that tells
           ;; whether a value is valid for this field.
           #`(define-syntax #,(id #'stem #'validate- #'stem #'- name)
               (lambda (s)
                 ;; Make sure the given VALUE, for field NAME, passes PRED.
                 (syntax-case s ()
                   ((_ value)
                    (with-syntax ((name #'#,name)
                                  (pred #'#,pred)
                                  (loc (datum->syntax #'value
                                                      (syntax-source #'value))))
                      #'(if (pred value)
                            value
                            (configuration-field-error
                             (and=> 'loc source-properties->location)
                             'name value))))))))

         #`(begin
             ;; Define field validation macros.
             #,@(filter-map (lambda (name pred sanitizer)
                              (if sanitizer
                                  #f
                                  (default-field-sanitizer name pred)))
                            #'(field ...)
                            #'(field-predicate ...)
                            #'(field-sanitizer ...))

             (define-record-type* #,(id #'stem #'< #'stem #'>)
               stem
               #,(id #'stem #'make- #'stem)
               #,(id #'stem #'stem #'?)
               #,@(map (lambda (name getter def sanitizer)
                         #`(#,name #,getter
                                   (default #,def)
                                   (sanitize
                                    #,(or sanitizer
                                          (id #'stem
                                              #'validate- #'stem #'- name)))))
                       #'(field ...)
                       #'(field-getter ...)
                       #'(field-default ...)
                       #'(field-sanitizer ...))
               (%location #,(id #'stem #'stem #'-source-location)
                          (default (and=> (current-source-location)
                                          source-properties->location))
                          (innate)))

             (define #,(id #'stem #'stem #'-fields)
               (list (configuration-field
                      (name 'field)
                      (type 'field-type)
                      (getter field-getter)
                      (predicate field-predicate)
                      (sanitizer
                       (or field-sanitizer
                           (id #'stem #'validate- #'stem #'- #'field)))
                      (serializer field-serializer)
                      (default-value-thunk
                        (lambda ()
                          (if (maybe-value-set? (syntax->datum field-default))
                              field-default
                              (configuration-missing-default-value
                               '#,(id #'stem #'% #'stem) 'field))))
                      (documentation doc))
                     ...))))))))

(define no-serialization         ;syntactic keyword for 'define-configuration'
  '(no serialization))

(define-syntax define-configuration
  (lambda (s)
    (syntax-case s (no-serialization prefix)
      ((_ stem (field field-type+def doc custom-serializer ...) ...
          (no-serialization))
       (define-configuration-helper
         #f #f #'(_ stem (field field-type+def doc custom-serializer ...)
                 ...)))
      ((_ stem  (field field-type+def doc custom-serializer ...) ...
          (prefix serializer-prefix))
       (define-configuration-helper
         #t #'serializer-prefix #'(_ stem (field field-type+def
                                                 doc custom-serializer ...)
                 ...)))
      ((_ stem (field field-type+def doc custom-serializer ...) ...)
       (define-configuration-helper
         #t #f #'(_ stem (field field-type+def doc custom-serializer ...)
                 ...))))))

(define-syntax-rule (define-configuration/no-serialization
                      stem (field field-type+def
                                  doc custom-serializer ...) ...)
  (define-configuration stem (field field-type+def
                                    doc custom-serializer ...) ...
    (no-serialization)))

(define (empty-serializer field-name val) "")
(define serialize-package empty-serializer)

;; Ideally this should be an implementation detail, but we export it
;; to provide a simpler API that enables unsetting a configuration
;; field that has a maybe type, but also a default value.  We give it
;; a value that sticks out to the reader when something goes wrong.
;;
;; An example use-case would be something like a network application
;; that uses a default port, but the field can explicitly be unset to
;; request a random port at startup.
(define %unset-value '%unset-marker%)

(define (maybe-value-set? value)
  "Predicate to check whether a 'maybe' value was explicitly provided."
  (not (eq? %unset-value value)))

;; Ideally there should be a compiler macro for this predicate, that expands
;; to a conditional that only instantiates the default value when needed.
(define* (maybe-value value #:optional (default #f))
  "Returns VALUE, unless it is the unset value, in which case it returns
DEFAULT."
  (if (maybe-value-set? value)
      value
      default))

;; A little helper to make it easier to document all those fields.
(define (generate-documentation documentation documentation-name)
  (define (str x) (object->string x))

  (define (package->symbol package)
    "Return the first symbol name of a package that matches PACKAGE, else #f."
    (let* ((module (file-name->module-name
                    (location-file (package-location package))))
           (symbols (filter-map
                     identity
                     (module-map (lambda (symbol var)
                                   (and (equal? package (variable-ref var))
                                        symbol))
                                 (resolve-module module)))))
      (if (null? symbols)
          #f
          (first symbols))))

  (define (generate configuration-name)
    (match (assq-ref documentation configuration-name)
      ((fields . sub-documentation)
       `((deftp (% (category "Data Type") (name ,(str configuration-name)))
           (para "Available " (code ,(str configuration-name)) " fields are:")
           (table
            (% (formatter (asis)))
            ,@(map
               (lambda (f)
                 (let ((field-name (configuration-field-name f))
                       (field-type (configuration-field-type f))
                       (field-docs (cdr (texi-fragment->stexi
                                         (configuration-field-documentation f))))
                       (default (catch #t
                                  (configuration-field-default-value-thunk f)
                                  (lambda _ '%invalid))))
                   (define (show-default? val)
                     (or (string? val) (number? val) (boolean? val)
                         (package? val)
                         (and (symbol? val) (not (eq? val '%invalid)))
                         (and (list? val) (and-map show-default? val))))

                   (define (show-default val)
                     (cond
                      ((package? val)
                       (symbol->string (package->symbol val)))
                      (((list-of package?) val)
                       (format #f "(~{~a~^ ~})" (map package->symbol val)))
                      (else (str val))))

                   `(entry (% (heading
                               (code ,(str field-name))
                               ,@(if (show-default? default)
                                     `(" (default: "
                                       (code ,(show-default default)) ")")
                                     '())
                               " (type: " ,(str field-type) ")"))
                           (para ,@field-docs)
                           ,@(append-map
                              generate
                              (filter-map
                               (match-lambda
                                 ((name config)
                                  (and (eq? name field-name)
                                       config)))
                               (or (assq-ref sub-documentation field-name)
                                   '()))))))
               fields)))))))
  (stexi->texi `(*fragment* . ,(generate documentation-name))))

(define (configuration->documentation configuration-symbol)
  "Take CONFIGURATION-SYMBOL, the symbol corresponding to the name used when
defining a configuration record with DEFINE-CONFIGURATION, and output the
Texinfo documentation of its fields."
  ;; This is helper for a simple, straight-forward application of
  ;; GENERATE-DOCUMENTATION.
  (let ((fields-getter (module-ref (current-module)
                                   (symbol-append configuration-symbol
                                                  '-fields))))
    (display (generate-documentation `((,configuration-symbol ,fields-getter))
                                     configuration-symbol))))

(define* (filter-configuration-fields configuration-fields fields
                                      #:optional negate?)
  "Retrieve the fields listed in FIELDS from CONFIGURATION-FIELDS.
If NEGATE? is @code{#t}, retrieve all fields except FIELDS."
  (filter (lambda (field)
            (let ((member? (member (configuration-field-name field) fields)))
              (if (not negate?) member? (not member?))))
          configuration-fields))


(define* (interpose ls  #:optional (delimiter "\n") (grammar 'infix))
  "Same as @code{string-join}, but without join and string, returns a
DELIMITER interposed LS.  Support 'infix and 'suffix GRAMMAR values."
  (when (not (member grammar '(infix suffix)))
    (raise
     (formatted-message
      (G_ "The GRAMMAR value must be 'infix or 'suffix, but ~a provided.")
      grammar)))
  (fold-right (lambda (e acc)
                (cons e
                      (if (and (null? acc) (eq? grammar 'infix))
                          acc
                          (cons delimiter acc))))
              '() ls))


;;;
;;; Commonly used predicates
;;;

(define (list-of pred?)
  "Return a procedure that takes a list and check if all the elements of
the list result in @code{#t} when applying PRED? on them."
    (lambda (x)
      (if (list? x)
          (every pred? x)
          #f)))

(define list-of-packages?
  (list-of package?))

(define list-of-strings?
  (list-of string?))

(define list-of-symbols?
  (list-of symbol?))


;;;
;;; Special serializers
;;;

(define alist?
  (list-of pair?))

(define serialize-file-like empty-serializer)

(define (text-config? config)
  (list-of file-like?))

(define (serialize-text-config field-name val)
  #~(string-append
     #$@(interpose
         (map
          (lambda (e)
            #~(begin
                (use-modules (ice-9 rdelim))
                (with-fluids ((%default-port-encoding "UTF-8"))
                  (with-input-from-file #$e read-string))))
          val)
         "\n" 'suffix)))

(define ((generic-serialize-alist-entry serialize-field) entry)
  "Apply the SERIALIZE-FIELD procedure on the field and value of ENTRY."
  (match entry
    ((field . val) (serialize-field field val))))

(define (generic-serialize-alist combine serialize-field fields)
  "Generate a configuration from an association list FIELDS.

SERIALIZE-FIELD is a procedure that takes two arguments, it will be
applied on the fields and values of FIELDS using the
@code{generic-serialize-alist-entry} procedure.

COMBINE is a procedure that takes one or more arguments and combines
all the alist entries into one value, @code{string-append} or
@code{append} are usually good candidates for this."
  (apply combine
         (map (generic-serialize-alist-entry serialize-field) fields)))
