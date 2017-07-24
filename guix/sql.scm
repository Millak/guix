;;; Copyright © 2017 Caleb Ristvedt <caleb.ristvedt@cune.org>
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


(define-module (guix sql)
  #:use-module (sqlite3)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (sqlite-last-insert-rowid
            sql-parameters
            with-sql-statement
            with-sql-database
            run-sql
            run-statement
            single-result
            with-sql-statements)
  #:re-export (sqlite-step
               sqlite-fold
               sqlite-fold-right
               sqlite-map
               sqlite-prepare
               sqlite-reset
               sqlite-finalize))

;; Miscellaneous SQL stuff. Mostly macros.


;; This structure and the procedures that operate on it make it possible to
;; open an sqlite database and compile sql statements only when
;; necessary. Whichever macro opens the database is also responsible for
;; closing it, and any macros using that database or statements compiled for
;; it within the scope of that macro will use what is already there.
(define-record-type <guix-database>
  (make-guix-database sql-database statement-cache)
  guix-database?
  (sql-database guix-sql-database)
  (statement-cache db-statement-cache))

(define (open-guix-database location)
  (make-guix-database (sqlite-open location)
                      (make-hash-table)))

(define (close-guix-database db)
  (hash-for-each (lambda (key value)
                   (sqlite-finalize value))
                 (db-statement-cache db))
  (sqlite-close (guix-sql-database db)))

(define (maybe-compile-statement db sql)
  (let ((statement (hash-ref (db-statement-cache db) sql)))
    (or statement
        (let ((new-statement (sqlite-prepare (guix-sql-database db)
                                             sql)))
          (hash-set! (db-statement-cache db)
                     sql
                     new-statement)
          new-statement))))


;; This really belongs in guile-sqlite3, as can be seen from the @@s.
(define sqlite-last-insert-rowid
  (let ((last-rowid (pointer->procedure
                     int
                     (dynamic-func "sqlite3_last_insert_rowid"
                                   (@@ (sqlite3) libsqlite3))
                     (list '*))))
    (lambda (db)
      "Gives the row id of the last inserted row in DB."
      (last-rowid ((@@ (sqlite3) db-pointer) (guix-sql-database db))))))

(define sqlite-parameter-index
  (let ((param-index (pointer->procedure
                      int
                      (dynamic-func "sqlite3_bind_parameter_index"
                                    (@@ (sqlite3) libsqlite3))
                      (list '* '*))))
    (lambda (statement key)
      "Gives the index of an sqlite parameter for a certain statement with a
certain (string) name."
      (param-index ((@@ (sqlite3) stmt-pointer) statement)
                   (string->pointer key "utf-8")))))


(define-syntax sql-parameters
  (syntax-rules ()
    "Converts key-value pairs into sqlite bindings for a specific statement."
    ((sql-parameters statement (name1 val1) (name2 val2) (name3 val3) ...)
     (begin (sqlite-bind statement
                         (sqlite-parameter-index statement name1)
                         val1)
            (sql-parameters statement (name2 val2) (name3 val3) ...)))
    ((sql-parameters statement (name value))
     (sqlite-bind statement
                  (sqlite-parameter-index statement name)
                  value))))

(define* (step-all statement #:optional (callback noop))
  "Step until statement is completed. Return number of rows."
  ;; Where "number of rows" is assumed to be number of steps taken, excluding
  ;; the last one.
  (let maybe-step ((ret (sqlite-step statement))
                   (count 0))
    (if ret
        (maybe-step (sqlite-step statement) (+ count 1))
        count)))

;; I get the feeling schemers have probably already got this "with" business
;; much more automated than this...
(define-syntax with-sql-statement
  (syntax-rules ()
    "Automatically prepares statements and then finalizes statements once the
scope of this macro is left. Also with built-in sqlite parameter binding via
key-value pairs."
    ((with-sql-statement db sql statement-var
                         ((name1 val1) (name2 val2) ...)
                         exps ...)
     (let ((statement-var (maybe-compile-statement db sql)))
       (dynamic-wind noop
                     (lambda ()
                       (sql-parameters statement-var
                                       (name1 val1)
                                       (name2 val2) ...)
                       exps ...)
                     (lambda ()
                       (sqlite-reset statement-var)))))
    ((with-sql-statement db sql statement-var () exps ...)
     (let ((statement-var (maybe-compile-statement db sql)))
       (dynamic-wind noop
                     (lambda ()
                       exps ...)
                     (lambda ()
                       (sqlite-reset statement-var)))))))

(define-syntax with-sql-statements
  (syntax-rules ()
    "Like with-sql-statement, but with multiple statements."
    ((with-sql-statements db ((sql statement-var bindings))
                          exps ...)
     (with-sql-statement db sql statement-var bindings
                         exps ...))
    ((with-sql-statements db ((sql statement-var bindings) stmt-clause-rest ...)
                          exps ...)
     (with-sql-statements db (stmt-clause-rest ...)
                          (with-sql-statement db sql statement-var bindings
                                              exps ...)))))


(define-syntax with-sql-database
  (syntax-rules ()
    "Automatically closes the database once the scope of this macro is left
unless the database was already open - that is, LOCATION wasn't a string but a
<sqlite-db>"
    ((with-sql-database location db-var exps ...)
     (let* ((already-open? (guix-database? location))
            (db-var (if already-open?
                        location
                        (open-guix-database location))))
       (dynamic-wind noop
                     (lambda ()
                       exps ...)
                     (lambda ()
                       (unless already-open?
                         (close-guix-database db-var))))))))

(define-syntax run-sql
  (syntax-rules ()
    "For one-off queries that don't get repeated on the same
database. Everything after database and sql source should be 2-element lists
containing the sql placeholder name and the value to use. Returns the number
of rows."
    ((run-sql db sql (name1 val1) (name2 val2) ...)
     (let ((statement (maybe-compile-statement db sql)))
       (dynamic-wind noop
                     (lambda ()
                       (sql-parameters statement
                                            (name1 val1)
                                            (name2 val2) ...)
                       (step-all statement))
                     (lambda ()
                       (sqlite-reset statement)))))
    ((run-sql db sql)
     (let ((statement (maybe-compile-statement db sql)))
       (dynamic-wind noop
                     (lambda ()
                       (step-all statement))
                     (lambda ()
                       (sqlite-reset statement)))))))

(define-syntax run-statement
  (syntax-rules ()
    "For compiled statements that may be run multiple times. Everything after
database and sql source should be 2-element lists containing the sql
placeholder name and the value to use. Returns the number of rows."
    ((run-sql db statement (name1 val1) (name2 val2) ...)
     (dynamic-wind noop
                   (lambda ()
                     (sql-parameters statement
                                     (name1 val1)
                                     (name2 val2) ...)
                     (step-all statement))
                   (lambda ()
                     (sqlite-reset statement))))
    ((run-sql db statement)
     (dynamic-wind noop
                   (lambda ()
                     (step-all statement))
                   (lambda ()
                     (sqlite-reset statement))))))



(define (single-result statement)
  "Gives the first element of the first row returned by statement."
  (let ((row (sqlite-step statement)))
    (if row
        (vector-ref row 0)
        #f)))
