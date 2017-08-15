;;; GNU Guix --- Functional package management for GNU
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

(define-module (guix store database)
  #:use-module (sqlite3)
  #:use-module (guix sql)
  #:use-module (guix config)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 vlist)
  #:export (sqlite-register
            outputs-exist?
            file-closure
            register-derivation-output))

;;; Code for working with the store database directly.

(define path-id-sql
  "SELECT id FROM ValidPaths WHERE path = $path")

(define* (path-id db path)
  "If the path \"path\" exists in the ValidPaths table, return its
id. Otherwise, return #f."
  (with-sql-statement
      db path-id-sql statement
      (("$path" path))
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
          (run-sql
              db update-sql
              ("$id" id)
              ("$deriver" deriver)
              ("$hash" hash)
              ("$size" nar-size)
              ("$time" time))
          id)
        (begin
          (run-sql
              db insert-sql
              ("$path" path)
              ("$deriver" deriver)
              ("$hash" hash)
              ("$size" nar-size)
              ("$time" time))
          (sqlite-last-insert-rowid db)))))

(define add-reference-sql
  "INSERT OR IGNORE INTO Refs (referrer, reference) SELECT $referrer, id
FROM ValidPaths WHERE path = $reference")

(define (add-references db referrer references)
  "referrer is the id of the referring store item, references is a list
containing store item paths being referred to. Note that all of the store
items in \"references\" should already be registered."
  (with-sql-statement
      db add-reference-sql add-reference-statement ()
      (for-each (lambda (reference)
                  (run-statement
                      db add-reference-statement
                      ("$referrer" referrer)
                      ("$reference" reference)))
                references)))

;; XXX figure out caching of statement and database objects... later
(define* (sqlite-register #:key dbpath path references deriver hash nar-size)
  "Registers this stuff in a database specified by DBPATH. PATH is the string
path of some store item, REFERENCES is a list of string paths which the store
item PATH refers to (they need to be already registered!), DERIVER is a string
path of the derivation that created the store item PATH, HASH is the
base16-encoded sha256 hash of the store item denoted by PATH (prefixed with
\"sha256:\") after being converted to nar form, and nar-size is the size in
bytes of the store item denoted by PATH after being converted to nar
form. Returns the id of the registered path."
  (with-sql-database
      dbpath db
      (let ((id (update-or-insert #:db db
                                  #:path path
                                  #:deriver deriver
                                  #:hash hash
                                  #:nar-size nar-size
                                  #:time (current-time))))
        (add-references db id references)
        id)))

(define get-outputs-sql
  "SELECT path FROM DerivationOutputs WHERE $drvpath IN (SELECT path FROM
ValidPaths WHERE ValidPaths.id = DerivationOutputs.drv) AND id = $id")

(define output-path-id-sql
  "SELECT id FROM ValidPaths WHERE path IN (SELECT path FROM DerivationOutputs
WHERE DerivationOutputs.id = $id AND drv IN (SELECT id FROM ValidPaths WHERE
path = $drvpath))")
;; "SELECT id FROM ValidPaths WHERE ValidPaths.path IN (SELECT path FROM
;; DerivationOutputs WHERE $drvpath IN (SELECT path FROM ValidPaths WHERE
;; ValidPaths.id = DerivationOutputs.drv) AND id = $id)"


(define* (outputs-exist? drv-path outputs
                         #:optional (database %store-database))
  "Determines whether all output labels in OUTPUTS exist as built outputs of
drv-path."
  (with-sql-database
      database db
      (with-sql-statement db output-path-id-sql output-path-id
                          (("$drvpath" drv-path))
                          (fold
                           (lambda (output-label prev)
                             (and prev
                                  (begin
                                    (sqlite-reset output-path-id)
                                    (sql-parameters output-path-id
                                                    ("$id" output-label))
                                    (single-result output-path-id))))
                           #t
                           outputs))))

(define referrers-sql
  "SELECT path FROM ValidPaths WHERE id IN (SELECT referrer FROM Refs WHERE
reference IN (SELECT id FROM ValidPaths WHERE path = $path))")

(define references-sql
  "SELECT path FROM ValidPaths WHERE id IN (SELECT reference FROM Refs WHERE
referrer IN (SELECT id FROM ValidPaths WHERE path = $path))")

(define* (file-closure path #:key
                       (database %store-database)
                       (list-so-far vlist-null))
  "Returns a vlist containing the store paths referenced by PATH, the store
paths referenced by those paths, and so on."
  (with-sql-database
      database db
      (with-sql-statement
          db references-sql get-references ()

          ;; to make it possible to go depth-first we need to get all the
          ;; references of an item first or we'll have re-entrancy issues with
          ;; the get-references statement.
          (define (references-of path)
            ;; There are no problems with resetting an already-reset
            ;; statement.
            (sqlite-reset get-references)
            (sql-parameters get-references ("$path" path))
            (sqlite-fold (lambda (row prev)
                           (cons (vector-ref row 0) prev))
                         '()
                         get-references))

          (let %file-closure ((references-vlist (vhash-cons path
                                                            #t
                                                            list-so-far))
                              (path path))
            (fold (lambda (ref prev)
                    (if (vhash-assoc ref prev)
                        prev
                        (%file-closure (vhash-cons ref #t prev)
                                       ref)))
                  references-vlist
                  (references-of path))))))

(define register-output-sql
  "INSERT OR REPLACE INTO DerivationOutputs (drv, id, path) SELECT id, $outid,
$outpath FROM ValidPaths WHERE path = $drvpath")

(define (register-derivation-output database drv-path outid output-path
                                    references nar-size hash)
  (with-sql-database
      database db
      (with-sql-statement db
        register-output-sql register-output
        (("$drvpath" drv-path)
         ("$outid" outid)
         ("$outpath" output-path))
        (let ((id (sqlite-register #:dbpath db
                                   #:path output-path
                                   #:references references
                                   #:deriver drv-path
                                   #:hash hash
                                   #:nar-size nar-size)))
          (run-statement db register-output)))))
