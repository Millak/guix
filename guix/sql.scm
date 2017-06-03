(define-module (guix sql)
  #:use-module (sqlite3)
  #:use-module (system foreign)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-9)
  #:export (sqlite-register))

;; Miscellaneous SQL stuff, currently just setup for sqlite-register. Mostly
;; macros.

;; This really belongs in guile-sqlite3, as can be seen from the @@s.
(define sqlite-last-insert-rowid
  (let ((last-rowid (pointer->procedure
                     int
                     (dynamic-func "sqlite3_last_insert_rowid"
                                   (@@ (sqlite3) libsqlite3))
                     (list '*))))
    (lambda (db)
      "Gives the row id of the last inserted row in DB."
      (last-rowid ((@@ (sqlite3) db-pointer) db)))))


;; Should I go from key->index here or try to change that in guile-sqlite3?
(define-syntax sql-parameters
  (syntax-rules ()
    "Converts key-value pairs into sqlite bindings for a specific statement."
    ((sql-parameters statement (name1 val1) (name2 val2) (name3 val3) ...)
     (begin (sqlite-bind statement name1 val1)
            (sql-parameters statement (name2 val2) (name3 val3) ...)))
    ((sql-parameters statement (name value))
     (sqlite-bind statement name value))))

(define* (step-all statement #:optional (callback noop))
  "Step until statement is completed. Return number of rows."
  ;; Where "number of rows" is assumed to be number of steps taken, excluding
  ;; the last one.
  (let maybe-step ((ret (sqlite-step statement))
                   (count 0))
    (if ret
        (maybe-step ret (+ count 1))
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
     (let ((statement-var (sqlite-prepare db sql)))
       (dynamic-wind noop
                     (lambda ()
                       (sql-parameters statement-var
                                            (name1 val1)
                                            (name2 val2) ...)
                       exps ...)
                     (lambda ()
                       (sqlite-finalize statement-var)))))
    ((with-sql-statement db sql statement-var () exps ...)
     (let ((statement-var (sqlite-prepare db sql)))
       (dynamic-wind noop
                     (lambda ()
                       exps ...)
                     (lambda ()
                       (sqlite-finalize statement-var)))))))

(define-syntax with-sql-database
  (syntax-rules ()
    "Automatically closes the database once the scope of this macro is left."
    ((with-sql-database location db-var exps ...)
     (let ((db-var (sqlite-open location)))
       (dynamic-wind noop
                     (lambda ()
                       exps ...)
                     (lambda ()
                       (sqlite-close db-var)))))))

(define-syntax run-sql
  (syntax-rules ()
    "For one-off queries that don't get repeated on the same
database. Everything after database and sql source should be 2-element lists
containing the sql placeholder name and the value to use. Returns the number
of rows."
    ((run-sql db sql (name1 val1) (name2 val2) ...)
     (let ((statement (sqlite-prepare db sql)))
       (dynamic-wind noop
                     (lambda ()
                       (sql-parameters statement
                                            (name1 val1)
                                            (name2 val2) ...)
                       (step-all statement))
                     (lambda ()
                       (sqlite-finalize statement)))))
    ((run-sql db sql)
     (let ((statement (sqlite-prepare db sql)))
       (dynamic-wind noop
                     (lambda ()
                       (step-all statement))
                     (lambda ()
                       (sqlite-finalize statement)))))))

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

(define path-id-sql
  "SELECT id FROM ValidPaths WHERE path = $path")

(define (single-result statement)
  "Gives the first element of the first row returned by statement."
  (let ((row (sqlite-step statement)))
    (if row
        (vector-ref row 0)
        #f)))

(define* (path-id db path)
  "If the path \"path\" exists in the ValidPaths table, return its
id. Otherwise, return #f. If you already have a compiled statement for this
purpose, you can give it as statement."
  (with-sql-statement db path-id-sql statement
                      (;("$path" path)
                       (1 path))
                      (single-result statement)))


(define update-sql
  "UPDATE ValidPaths SET hash = $hash, registrationTime = $time, deriver =
$deriver, narSize = $size WHERE id = $id")

(define insert-sql
  "INSERT INTO ValidPaths (path, hash, registrationTime, deriver, narSize)
VALUES ($path, $hash, $time, $deriver, $size)")

(define* (update-or-insert #:key db path deriver hash nar-size time)
  "The classic update-if-exists and insert-if-doesn't feature that sqlite
doesn't exactly have... they've got something close, but it involves deleting
and re-inserting instead of updating, which causes problems with foreign keys,
of course. Returns the row id of the row that was modified or inserted."
  (let ((id (path-id db path)))
    (if id
        (begin
          (run-sql db update-sql
                   ;; As you may have noticed, sqlite-bind doesn't behave
                   ;; exactly how I was expecting...
                   ;; ("$id" id)
                   ;; ("$deriver" deriver)
                   ;; ("$hash" hash)
                   ;; ("$size" nar-size)
                   ;; ("$time" time)
                   (5 id)
                   (3 deriver)
                   (1 hash)
                   (4 nar-size)
                   (2 time))
          id)
        (begin
          (run-sql db insert-sql
                   ;; ("$path" path)
                   ;; ("$deriver" deriver)
                   ;; ("$hash" hash)
                   ;; ("$size" nar-size)
                   ;; ("$time" time)
                   (1 path)
                   (4 deriver)
                   (2 hash)
                   (5 nar-size)
                   (3 time))
          (sqlite-last-insert-rowid db)))))

(define add-reference-sql
  "INSERT OR IGNORE INTO Refs (referrer, reference) SELECT $referrer, id
FROM ValidPaths WHERE path = $reference")

(define (add-references db referrer references)
  "referrer is the id of the referring store item, references is a list
containing store item paths being referred to. Note that all of the store
items in \"references\" should already be registered."
  (with-sql-statement db add-reference-sql add-reference-statement ()
                      (for-each (lambda (reference)
                                  (run-statement db
                                                 add-reference-statement
                                                 ;("$referrer" referrer)
                                                 ;("$reference" reference)
                                                 (1 referrer)
                                                 (2 reference)))
                                references)))

;; XXX figure out caching of statement and database objects... later
(define* (sqlite-register #:key dbpath path references deriver hash nar-size)
  "Registers this stuff in a database specified by DBPATH. PATH is the string
path of some store item, REFERENCES is a list of string paths which the store
item PATH refers to (they need to be already registered!), DERIVER is a string
path of the derivation that created the store item PATH, HASH is the
base16-encoded sha256 hash of the store item denoted by PATH (prefixed with
\"sha256:\") after being converted to nar form, and nar-size is the size in
bytes of the store item denoted by PATH after being converted to nar form."
  (with-sql-database dbpath db
                     (let ((id (update-or-insert #:db db
                                                 #:path path
                                                 #:deriver deriver
                                                 #:hash hash
                                                 #:nar-size nar-size
                                                 #:time (current-time))))
     (add-references db id references))))
