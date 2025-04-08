;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021-2025 Ludovic Courtès <ludo@gnu.org>
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

;;; Commentary:
;;;
;;; This script updates package definitions so they use the "simplified" style
;;; for input lists, as in:
;;;
;;;  (package
;;;    ;; ...
;;;    (inputs (list foo bar baz)))
;;;
;;; Code:

(define-module (guix scripts style)
  #:autoload   (gnu packages) (specification->package fold-packages)
  #:use-module (guix combinators)
  #:use-module (guix scripts)
  #:use-module ((guix scripts build) #:select (%standard-build-options))
  #:use-module (guix ui)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (guix read-print)
  #:use-module (ice-9 control)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-37)
  #:export (guix-style))


;;;
;;; Simplifying input expressions.
;;;

(define (label-matches? label name)
  "Return true if LABEL matches NAME, a package name."
  (or (string=? label name)
      (and (string-prefix? "python-" label)
           (string-prefix? "python2-" name)
           (string=? (string-drop label (string-length "python-"))
                     (string-drop name (string-length "python2-"))))))

(define* (simplify-inputs location package str inputs
                          #:key (label-matches? label-matches?))
  "Simplify the inputs field of PACKAGE (a string) at LOCATION; its current
value is INPUTS the corresponding source code is STR.  Return a string to
replace STR."
  (define (simplify-input-expression return)
    (match-lambda
      ((label ('unquote symbol)) symbol)
      ((label ('unquote symbol) output)
       (list 'quasiquote
             (list (list 'unquote symbol) output)))
      (_
       ;; Expression doesn't look like a simple input.
       (warning location (G_ "~a: complex expression, \
bailing out~%")
                package)
       (return str))))

  (define (simplify-input exp input return)
    (define package* package)

    (match input
      ((or ((? string? label) (? package? package))
           ((? string? label) (? package? package)
            (? string?)))
       ;; If LABEL doesn't match PACKAGE's name, then simplifying would incur
       ;; a rebuild, and perhaps it would break build-side code relying on
       ;; this specific label.
       (if (label-matches? label (package-name package))
           ((simplify-input-expression return) exp)
           (begin
             (warning location (G_ "~a: input label \
'~a' does not match package name, bailing out~%")
                      package* label)
             (return str))))
      (_
       (warning location (G_ "~a: non-trivial input, \
bailing out~%")
                package*)
       (return str))))

  (define (simplify-expressions exp inputs return)
    ;; Simplify the expressions in EXP, which correspond to INPUTS, and return
    ;; a list of expressions.  Call RETURN with a string when bailing out.
    (let loop ((result '())
               (exp exp)
               (inputs inputs))
      (match exp
        (((? blank? head) . rest)
         (loop (cons head result) rest inputs))
        ((head . rest)
         (match inputs
           ((input . inputs)
            ;; HEAD (an sexp) and INPUT (an input tuple) are correlated.
            (loop (cons (simplify-input head input return) result)
                  rest inputs))
           (()
            ;; If EXP and INPUTS have a different length, that
            ;; means EXP is a non-trivial input list, for example
            ;; with input-splicing, conditionals, etc.
            (warning location (G_ "~a: input expression is too short~%")
                     package)
            (return str))))
        (()
         ;; It's possible for EXP to contain fewer elements than INPUTS, for
         ;; example in the case of input splicing.  No bailout here.  (XXX)
         (reverse result)))))

  (define inputs-exp
    (call-with-input-string str read-with-comments))

  (match inputs-exp
    (('list _ ...)                                ;already done
     str)
    (('modify-inputs _ ...)                       ;already done
     str)
    (('quasiquote                                 ;prepending inputs
      (exp ...
           ('unquote-splicing
            ((and symbol (or 'package-inputs 'package-native-inputs
                             'package-propagated-inputs))
             arg))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions exp inputs return)))
          `(modify-inputs (,symbol ,arg)
                          (prepend ,@things)))
        (location-column location))))
    (('quasiquote                                 ;replacing an input
      ((and exp ((? string? to-delete) ('unquote replacement)))
       ('unquote-splicing
        ('alist-delete (? string? to-delete)
                       ((and symbol
                             (or 'package-inputs 'package-native-inputs
                                 'package-propagated-inputs))
                        arg)))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions (list exp)
                                            (list (car inputs))
                                            return)))
          `(modify-inputs (,symbol ,arg)
                          (replace ,to-delete ,replacement)))
        (location-column location))))

    (('quasiquote                                 ;removing an input
      (exp ...
           ('unquote-splicing
            ('alist-delete (? string? to-delete)
                           ((and symbol
                                 (or 'package-inputs 'package-native-inputs
                                     'package-propagated-inputs))
                            arg)))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions exp inputs return)))
          `(modify-inputs (,symbol ,arg)
                          (delete ,to-delete)
                          (prepend ,@things)))
        (location-column location))))
    (('fold 'alist-delete                         ;removing several inputs
            ((and symbol
                  (or 'package-inputs 'package-native-inputs
                      'package-propagated-inputs))
             arg)
            ('quote ((? string? to-delete) ...)))
     (object->string*
      `(modify-inputs (,symbol ,arg)
                      (delete ,@to-delete))
      (location-column location)))
    (('quasiquote                    ;removing several inputs and adding others
      (exp ...
           ('unquote-splicing
            ('fold 'alist-delete
                   ((and symbol
                         (or 'package-inputs 'package-native-inputs
                             'package-propagated-inputs))
                    arg)
                   ('quote ((? string? to-delete) ...))))))
     (let/ec return
       (object->string*
        (let ((things (simplify-expressions exp inputs return)))
          `(modify-inputs (,symbol ,arg)
                          (delete ,@to-delete)
                          (prepend ,@things)))
        (location-column location))))
    (('quasiquote (exp ...))
     (let/ec return
       (object->string*
        `(list ,@(simplify-expressions exp inputs return))
        (location-column location))))
    (_
     (warning location (G_ "~a: unsupported input style, \
bailing out~%")
              package)
     str)))

(define (edit-expression/dry-run properties rewrite-string)
  "Like 'edit-expression' but display what would be edited without actually
doing it."
  (edit-expression properties
                   (lambda (str)
                     (unless (string=? (rewrite-string str) str)
                       (info (source-properties->location properties)
                             (G_ "would be edited~%")))
                     str)))

(define (trivial-package-arguments? package)
  "Return true if PACKAGE has zero arguments or only \"trivial\" arguments
guaranteed not to refer to input labels."
  (let loop ((arguments (package-arguments package)))
    (match arguments
      (()
       #t)
      (((? keyword?) value rest ...)
       (and (or (boolean? value) (number? value) (string? value))
            (loop rest))))))

(define* (simplify-package-inputs package
                                  #:key (policy 'silent)
                                  (edit-expression edit-expression))
  "Edit the source code of PACKAGE to simplify its inputs field if needed.
POLICY is a symbol that defines whether to simplify inputs; it can one of
'silent (change only if the resulting derivation is the same), 'safe (change
only if semantics are known to be unaffected), and 'always (fearlessly
simplify inputs!).  Call EDIT-EXPRESSION to actually edit the source of
PACKAGE."
  (for-each (lambda (field-name field)
              (match (field package)
                (()
                 #f)
                (inputs
                 (match (package-field-location package field-name)
                   (#f
                    ;; If the location of FIELD-NAME is not found, it may be
                    ;; that PACKAGE inherits from another package.
                    #f)
                   (location
                    (edit-expression
                     (location->source-properties (absolute-location location))
                     (lambda (str)
                       (define matches?
                         (match policy
                           ('silent
                            ;; Simplify inputs only when the label matches
                            ;; perfectly, such that the resulting derivation
                            ;; is unchanged.
                            label-matches?)
                           ('safe
                            ;; If PACKAGE has no arguments, labels are known
                            ;; to have no effect: this is a "safe" change, but
                            ;; it may change the derivation.
                            (if (trivial-package-arguments? package)
                                (const #t)
                                label-matches?))
                           ('always
                            ;; Assume it's gonna be alright.
                            (const #t))))

                       (simplify-inputs location
                                        (package-name package)
                                        str inputs
                                        #:label-matches? matches?))))))))
            '(inputs native-inputs propagated-inputs)
            (list package-inputs package-native-inputs
                  package-propagated-inputs)))


;;;
;;; Gexpifying package arguments.
;;;

(define (unquote->ungexp value)
  "Replace 'unquote' and 'unquote-splicing' in VALUE with their gexp
counterpart."
  ;; Replace 'unquote only on the first quasiquotation level.
  (let loop ((value value)
             (quotation 1))
    (match value
      (('unquote x)
       (if (= quotation 1)
           `(ungexp ,x)
           value))
      (('unquote-splicing x)
       (if (= quotation 1)
           `(ungexp-splicing ,x)
           value))
      (('quasiquote x)
       (list 'quasiquote (loop x (+ quotation 1))))
      (('quote x)
       (list 'quote (loop x (+ quotation 1))))
      ((lst ...)
       (map (cut loop <> quotation) lst))
      (x x))))

(define (gexpify-argument-value value quotation)
  "Turn VALUE, an sexp, into its gexp equivalent.  QUOTATION is a symbol that
indicates in what quotation context VALUE is to be interpreted: 'quasiquote,
'quote, or 'none."
  (match quotation
    ('none
     (match value
       (('quasiquote value)
        (gexpify-argument-value value 'quasiquote))
       (('quote value)
        (gexpify-argument-value value 'quote))
       (value value)))
    ('quote
     `(gexp ,value))
    ('quasiquote
     `(gexp ,(unquote->ungexp value)))))

(define (quote-argument-value value quotation)
  "Quote VALUE, an sexp.  QUOTATION is a symbol that indicates in what
quotation context VALUE is to be interpreted: 'quasiquote, 'quote, or 'none."
  (define (self-quoting? x)
    (or (boolean? x) (number? x) (string? x) (char? x)
        (keyword? x)))

  (match quotation
    ('none
     (match value
       (('quasiquote value)
        (quote-argument-value value 'quasiquote))
       (('quote value)
        (quote-argument-value value 'quote))
       (value value)))
    ('quote
     (if (self-quoting? value)
         value
         (list 'quote value)))
    ('quasiquote
     (match value
       (('unquote x) x)
       ((? self-quoting? x) x)
       (_ (list 'quasiquote value))))))

(define %gexp-keywords
  ;; Package argument keywords that must be followed by a gexp.
  '(#:phases #:configure-flags #:make-flags #:strip-flags))

(define (gexpify-argument-tail sexp)
  "Gexpify SEXP, an unquoted argument tail."
  (match sexp
    (('substitute-keyword-arguments lst clauses ...)
     `(substitute-keyword-arguments ,lst
        ,@(map (match-lambda
                 ((((? keyword? keyword) identifier) body)
                  `((,keyword ,identifier)
                    ,(if (memq keyword %gexp-keywords)
                         (gexpify-argument-value body 'none)
                         (quote-argument-value body 'none))))
                 ((((? keyword? keyword) identifier default) body)
                  `((,keyword ,identifier
                              ,(if (memq keyword %gexp-keywords)
                                   (gexpify-argument-value default 'none)
                                   (quote-argument-value default 'none)))
                    ,(if (memq keyword %gexp-keywords)
                         (gexpify-argument-value body 'none)
                         (quote-argument-value body 'none))))
                 (clause clause))
               clauses)))
    (_ sexp)))

(define* (gexpify-package-arguments package
                                    #:key
                                    (policy 'none)
                                    (edit-expression edit-expression))
  "Rewrite the 'arguments' field of PACKAGE to use gexps where applicable."
  (define (gexpify location str)
    (match (call-with-input-string str read-with-comments)
      ((rest ...)
       (let ((blanks (take-while blank? rest))
             (value  (drop-while blank? rest)))
         (define-values (quotation arguments tail)
           (match value
             (('quote (arguments ...)) (values 'quote arguments '()))
             (('quasiquote (arguments ... ('unquote-splicing tail)))
              (values 'quasiquote arguments tail))
             (('quasiquote (arguments ...)) (values 'quasiquote arguments '()))
             (('list arguments ...) (values 'none arguments '()))
             (arguments (values 'none '()  arguments))))

         (define (append-tail sexp)
           (if (null? tail)
               sexp
               (let ((tail (gexpify-argument-tail tail)))
                 (if (null? arguments)
                     tail
                     `(append ,sexp ,tail)))))

         (let/ec return
           (object->string*
            (append-tail
             `(list ,@(let loop ((arguments arguments)
                                 (result '()))
                        (match arguments
                          (() (reverse result))
                          (((? keyword? keyword) value rest ...)
                           (when (eq? quotation 'none)
                             (match value
                               (('gexp _)         ;already gexpified
                                (return str))
                               (_ #f)))

                           (loop rest
                                 (cons* (if (memq keyword %gexp-keywords)
                                            (gexpify-argument-value value
                                                                    quotation)
                                            (quote-argument-value value quotation))
                                        keyword result)))
                          (((? blank? blank) rest ...)
                           (loop rest (cons blank result)))
                          (_
                           ;; Something like: ,@(package-arguments xyz).
                           (warning location
                                    (G_ "unsupported argument style; \
bailing out~%"))
                           (return str))))))
            (location-column location)))))
      (_
       (warning location
                (G_ "unsupported argument field; bailing out~%"))
       str)))

  (unless (null? (package-arguments package))
    (match (package-field-location package 'arguments)
      (#f
       #f)
      (location
       (edit-expression
        (location->source-properties (absolute-location location))
        (lambda (str)
          (gexpify location str)))))))


;;;
;;; Formatting package definitions.
;;;

(define* (format-package-definition package
                                    #:key policy
                                    (edit-expression edit-expression))
  "Reformat the definition of PACKAGE."
  (unless (package-definition-location package)
    (leave (package-location package)
           (G_ "no definition location for package ~a~%")
           (package-full-name package)))

  (edit-expression
   (location->source-properties
    (absolute-location (package-definition-location package)))
   (lambda (str)
     (let ((exp (call-with-input-string str
                  read-with-comments)))
       (object->string* exp
                        (location-column
                         (package-definition-location package))
                        #:format-comment canonicalize-comment
                        #:format-vertical-space canonicalize-vertical-space)))))

(define (package-location<? p1 p2)
  "Return true if P1's location is \"before\" P2's."
  (let ((loc1 (package-location p1))
        (loc2 (package-location p2)))
    (and loc1 loc2
         (if (string=? (location-file loc1) (location-file loc2))
             (< (location-line loc1) (location-line loc2))
             (string<? (location-file loc1) (location-file loc2))))))


;;;
;;; Whole-file formatting.
;;;

(define (order-packages lst)
  "Return LST, a list of top-level expressions and blanks, with
top-level package definitions in alphabetical order.  Packages which
share a name are placed with versions in descending order."
  (define (package-fields pkg)
    (match pkg
      ((('define-public pkg _ ... (or ('let _ expr) expr)) _ ...)
       (match expr
         (((or 'package 'package/inherit) fields ...)
          (let ((name (and=> (assoc-ref fields 'name) first))
                (version (and=> (assoc-ref fields 'version) first)))
            (values name version)))
         (_ (and (values #f #f)))))
      (_ (and (values #f #f)))))

  (define (package>? lst1 lst2)
    (let-values (((name1 version1) (package-fields lst1))
                 ((name2 version2) (package-fields lst2)))
      (and (string? name1)
           (string? name2)
           (or (string>? name1 name2)
               (and (string=? name1 name2)
                    (string? version1)
                    (string? version2)
                    (version>? version2 version1))))))

        ;; Group define-public with preceding blanks and defines.
  (let ((lst (fold2 (lambda (expr tail head)
                      (let ((head (cons expr head)))
                        (match expr
                          ((? blank?)
                           (values tail head))
                          (('define _ ...)
                           (values tail head))
                          (_ (values (cons head tail) '())))))
                    '() '() lst)))
    (reverse (concatenate (sort! lst package>?)))))

(define* (format-whole-file file order? #:rest rest)
  "Reformat all of FILE. When ORDER? is true, top-level package definitions
are put in alphabetical order."
  (with-fluids ((%default-port-encoding "UTF-8"))
    (let* ((lst (call-with-input-file file read-with-comments/sequence
                                      #:guess-encoding #t))
           (lst (if order?
                    (let loop ((lst lst))
                      (match lst
                        (((? blank? blank) rest ...)
                         (cons blank (loop rest)))
                        ((module rest ...)
                         (cons module (order-packages rest)))))
                    lst)))
      (with-atomic-file-output file
        (lambda (port)
          (apply pretty-print-with-comments/splice port lst
                 #:format-comment canonicalize-comment
                 #:format-vertical-space canonicalize-vertical-space
                 rest))
        #:sync? #f))))


;;;
;;; Options.
;;;

(define %options
  ;; Specification of the command-line options.
  (list (find (lambda (option)
                (member "load-path" (option-names option)))
              %standard-build-options)

        (option '(#\n "dry-run") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'dry-run? #t result)))
        (option '(#\e "expression") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'expression arg result)))
        (option '(#\f "whole-file") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'whole-file? #t result)))
        (option '(#\A "alphabetical-sort") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'order? #t result)))
        (option '(#\S "styling") #t #f
                (lambda (opt name arg result)
                  (alist-cons 'styling-procedure
                              (match arg
                                ("inputs" simplify-package-inputs)
                                ("arguments" gexpify-package-arguments)
                                ("format" format-package-definition)
                                (_ (leave (G_ "~a: unknown styling~%")
                                          arg)))
                              result)))
        (option '("input-simplification") #t #f
                (lambda (opt name arg result)
                  (let ((symbol (string->symbol arg)))
                    (unless (memq symbol '(silent safe always))
                      (leave (G_ "~a: invalid input simplification policy~%")
                             arg))
                    (alist-cons 'input-simplification-policy symbol
                                result))))

        (option '(#\h "help") #f #f
                (lambda args
                  (leave-on-EPIPE (show-help))
                  (exit 0)))
        (option '(#\l "list-stylings") #f #f
                (lambda args
                  (show-stylings)
                  (exit 0)))
        (option '(#\V "version") #f #f
                (lambda args
                  (show-version-and-exit "guix style")))))

(define (show-stylings)
  (display (G_ "Available styling rules:\n"))
  (display (G_ "- format: Format the given package definition(s)\n"))
  (display (G_ "- inputs: Rewrite package inputs to the “new style”\n"))
  (display (G_ "- arguments: Rewrite package arguments to G-expressions\n")))

(define (show-help)
  (display (G_ "Usage: guix style [OPTION]... [PACKAGE]...
Update package definitions to the latest style.\n"))
  (display (G_ "
  -S, --styling=RULE     apply RULE, a styling rule"))
  (display (G_ "
  -l, --list-stylings    display the list of available style rules"))
  (newline)
  (display (G_ "
  -n, --dry-run          display files that would be edited but do nothing"))
  (display (G_ "
  -L, --load-path=DIR    prepend DIR to the package module search path"))
  (display (G_ "
  -e, --expression=EXPR  consider the package EXPR evaluates to"))
  (display (G_ "
      --input-simplification=POLICY
                         follow POLICY for package input simplification, one
                         of 'silent', 'safe', or 'always'"))
  (newline)
  (display (G_ "
  -f, --whole-file       format the entire contents of the given file(s)"))
  (display (G_ "
  -A, --alphabetical-sort
                         place the contents in alphabetical order as well"))
  (newline)
  (display (G_ "
  -h, --help             display this help and exit"))
  (display (G_ "
  -V, --version          display version information and exit"))
  (newline)
  (show-bug-report-information))

(define %default-options
  ;; Alist of default option values.
  `((input-simplification-policy . silent)
    (styling-procedure . ,format-package-definition)))


;;;
;;; Entry point.
;;;

(define-command (guix-style . args)
  (category packaging)
  (synopsis "update the style of package definitions")

  (define (parse-options)
    ;; Return the alist of option values.
    (parse-command-line args %options (list %default-options)
                        #:build-options? #f))

  (let* ((opts     (parse-options))
         (edit     (if (assoc-ref opts 'dry-run?)
                       edit-expression/dry-run
                       edit-expression))
         (style    (assoc-ref opts 'styling-procedure))
         (policy   (assoc-ref opts 'input-simplification-policy)))
    (with-error-handling
      (if (assoc-ref opts 'whole-file?)
          (let ((files (filter-map (match-lambda
                                     (('argument . file) file)
                                     (_ #f))
                                   opts)))
            (unless (eq? format-package-definition style)
              (warning (G_ "'--styling' option has no effect in whole-file mode~%")))
            (when (null? files)
              (warning (G_ "no files specified, nothing to do~%")))
            (for-each
              (cute format-whole-file <> (assoc-ref opts 'order?))
              files))
          (let ((packages (filter-map (match-lambda
                                        (('argument . spec)
                                         (specification->package spec))
                                        (('expression . str)
                                         (read/eval str))
                                        (_ #f))
                                      opts)))
            (for-each (lambda (package)
                        (style package #:policy policy
                               #:edit-expression edit))
                      ;; Sort package by source code location so that we start
                      ;; editing files from the bottom and going upward.  That
                      ;; way, the 'location' field of <package> records is not
                      ;; invalidated as we modify files.
                      (sort (if (null? packages)
                                (fold-packages cons '() #:select? (const #t))
                                packages)
                            (negate package-location<?))))))))
