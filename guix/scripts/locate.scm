;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Antoine R. Dumont <antoine.romain.dumont@gmail.com>
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

(define-module (guix scripts locate)
  #:use-module ((guix config) #:select (%localstatedir))
  #:use-module (guix i18n)
  #:use-module ((guix ui)
                #:select (show-version-and-exit
                          show-bug-report-information
                          with-error-handling
                          string->number*
                          display-hint
                          leave-on-EPIPE))
  #:use-module (guix diagnostics)
  #:use-module (guix scripts)
  #:use-module (sqlite3)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:autoload   (guix combinators) (fold2)
  #:autoload   (guix grafts) (%graft?)
  #:autoload   (guix store roots) (gc-roots)
  #:use-module (guix derivations)
  #:use-module (guix packages)
  #:use-module (guix profiles)
  #:autoload   (guix progress) (progress-reporter/bar
                                call-with-progress-reporter)
  #:use-module (guix sets)
  #:use-module ((guix utils) #:select (cache-directory))
  #:autoload   (guix build utils) (find-files mkdir-p)
  #:autoload   (gnu packages) (fold-packages)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-37)
  #:use-module (srfi srfi-71)
  #:export     (guix-locate))

(define %db-schema-version
  ;; Current database schema version.
  3)

;; The following schema is the full schema at the `%db-schema-version`.  It
;; should be modified according to the development required and
;; `%db-schema-version` should be bumped. If the schema needs modification
;; across time, those should be changed directly in the full-schema and the
;; incremental changes should be referenced as migration step below for the
;; new `%db-schema-version` (for the existing dbs to know what to migrate).
(define %db-schema
  "
create table if not exists SchemaVersion (
  version integer primary key not null,
  date    integer,
  store   text not null,    -- value of (%store-prefix)
  unique (version)
);

create table if not exists Packages (
  id        integer primary key autoincrement not null,
  name      text not null,
  version   text not null,
  output    text,
  unique    (name, version) -- add uniqueness constraint
);

create table if not exists Directories (
  id        integer primary key autoincrement not null,
  name      text not null,
  package   integer not null,
  foreign key (package) references Packages(id) on delete cascade,
  unique (name, package) -- add uniqueness constraint
);

create table if not exists Files (
  name      text not null,
  basename  text not null,
  directory integer not null,
  foreign key (directory) references Directories(id) on delete cascade
  unique (name, basename, directory) -- add uniqueness constraint
);

create index if not exists IndexFiles on Files(basename);")

;; List of tuple ((version . sqlite schema migration script)). There should be
;; as much version increments as step needed to migrate the db.
(define schema-to-migrate '((1 . "
create table if not exists SchemaVersion (
  version integer primary key not null,
  unique (version)
);
")
                            (2 . "
alter table SchemaVersion
add column date date;
")
                            (3 . "
alter table Packages
add column output text;
")))

;; XXX: missing in guile-sqlite3@0.1.3
(define SQLITE_BUSY 5)

(define (call-with-database file proc)
  (catch 'sqlite-error
    (lambda ()
      (let ((db (sqlite-open file)))
        (dynamic-wind
          (lambda () #t)
          (lambda ()
            (ensure-latest-database-schema db)
            (proc db))
          (lambda () (sqlite-close db)))))
    (lambda (key who code errmsg)
      (if (= code SQLITE_BUSY)
          (leave (G_ "~a: database is locked by another process~%")
                 file)
          (throw key who code errmsg)))))

(define (ensure-latest-database-schema db)
  "Ensure DB follows the latest known version of the schema."
  (define (initialize)
    (sqlite-exec db %db-schema)
    (insert-version db %db-schema-version))

  (let ((version (false-if-exception (read-version db))))
    (cond ((not version)
           (initialize))
          ((> version %db-schema-version)
           (initialize))
          (else
           (catch #t
             (lambda ()
               ;; Migrate from the current version to the full migrated schema.
               ;; This can raise sqlite-error if the db is not properly configured yet
               (let loop ((current version))
                 (when (< current %db-schema-version)
                   ;; when the current db version is older than the current application
                   (let* ((next (+ current 1))
                          (migration (assoc-ref schema-to-migrate next)))
                     (when migration
                       (sqlite-exec db migration)
                       (insert-version db next))
                     (loop next)))))
             (lambda _
               ;; Exception handler in case failure to read an inexisting db:
               ;; fallback to bootstrap the schema.
               (initialize)))))))

(define (last-insert-row-id db)        ;XXX: copied from (guix store database)
  ;; XXX: (sqlite3) currently lacks bindings for 'sqlite3_last_insert_rowid'.
  ;; Work around that.
  (define stmt
    (sqlite-prepare db "SELECT last_insert_rowid();"
                    #:cache? #t))
  (match (sqlite-fold cons '() stmt)
    ((#(id)) id)
    (_ #f)))

(define (insert-version db version)
  "Insert application VERSION into the DB."
  (define stmt-insert-version
    (sqlite-prepare db "\
INSERT OR IGNORE INTO SchemaVersion(version, date, store)
VALUES (:version, CURRENT_TIMESTAMP, :store);"
                    #:cache? #t))
  (sqlite-exec db "begin immediate;")
  (sqlite-bind-arguments stmt-insert-version
                         #:version version
                         #:store (%store-prefix))
  (sqlite-fold (const #t) #t stmt-insert-version)
  (sqlite-exec db "commit;"))

(define (read-version db)
  "Read the current application version from the DB."

  (define stmt-select-version (sqlite-prepare db "\
SELECT version FROM SchemaVersion ORDER BY version DESC LIMIT 1;"
                                              #:cache? #f))
  (match (sqlite-fold cons '() stmt-select-version)
    ((#(version))
     version)))

(define user-database-file
  ;; Default user database file name.
  (string-append (cache-directory #:ensure? #f)
                 "/locate/db.sqlite"))

(define system-database-file
  ;; System-wide database file name.
  (string-append %localstatedir "/cache/guix/locate/db.sqlite"))

(define (file-age stat)
  "Return the age of the file denoted by STAT in seconds."
  (- (current-time) (stat:mtime stat)))

(define (suitable-database create? age-update-threshold)
  "Return a suitable database file.  When CREATE? is true, the returned
database will be opened for writing; otherwise, return the most recent one,
user or system.  Do not return the system database if it is older than
AGE-UPDATE-THRESHOLD seconds."
  (if (zero? (getuid))
      system-database-file
      (if create?
          user-database-file
          (let ((system (stat system-database-file #f))
                (user   (stat user-database-file #f)))
            (if user
                (if (and system
                         (> (stat:mtime system) (stat:mtime user))
                         (< (file-age system) age-update-threshold))
                    system-database-file
                    user-database-file)
                (if (and system
                         (< (file-age system) age-update-threshold))
                    system-database-file
                    user-database-file))))))

(define (clear-database db)
  "Drop packages and files from DB."
  (sqlite-exec db "BEGIN IMMEDIATE;")
  (sqlite-exec db "DELETE FROM Files;")
  (sqlite-exec db "DELETE FROM Directories;")
  (sqlite-exec db "DELETE FROM Packages;")
  (sqlite-exec db "COMMIT;")
  (sqlite-exec db "VACUUM;"))

(define (print-statistics file)
  "Print statistics about the database in FILE."
  (define (count db table)
    (define stmt
      (sqlite-prepare
       db (string-append "SELECT COUNT(*) FROM " table ";")))

    (match (sqlite-fold cons '() stmt)
      ((#(number)) number)))

  (call-with-database file
    (lambda (db)
      (format #t (G_ "schema version:\t~a~%")
              (read-version db))
      (format #t (G_ "number of packages:\t~9h~%")
              (count db "Packages"))
      (format #t (G_ "number of files:\t~9h~%")
              (count db "Files"))
      (format #t (G_ "database size:\t~9h MiB~%")
              (inexact->exact
               (round (/ (stat:size (stat file))
                         (expt 2 20))))))))


;;;
;;; Indexing from local packages.
;;;

(define (insert-files db package version outputs directories)
  "Insert DIRECTORIES files belonging to VERSION PACKAGE (with OUTPUTS)."
  (define stmt-select-package
    (sqlite-prepare db "\
SELECT id FROM Packages WHERE name = :name AND version = :version LIMIT 1;"
                    #:cache? #t))

  (define stmt-insert-package
    (sqlite-prepare db "\
INSERT OR IGNORE INTO Packages(name, version, output)
VALUES (:name, :version, :output);"
                    #:cache? #t))

  (define stmt-select-directory
    (sqlite-prepare db "\
SELECT id FROM Directories WHERE package = :package;"
                    #:cache? #t))

  (define stmt-insert-directory
    (sqlite-prepare db "\
INSERT OR IGNORE INTO Directories(name, package) -- to avoid spurious writes
VALUES (:name, :package);"
                    #:cache? #t))

  (define stmt-insert-file
    (sqlite-prepare db "\
INSERT OR IGNORE INTO Files(name, basename, directory)
VALUES (:name, :basename, :directory);"
                    #:cache? #t))

  (sqlite-exec db "begin immediate;")
  ;; 1 record per output
  (for-each (lambda (output)
              (sqlite-reset stmt-insert-package)
              (sqlite-bind-arguments stmt-insert-package
                                     #:name package
                                     #:version version
                                     #:output output)
              (sqlite-fold (const #t) #t stmt-insert-package))
            outputs)
  (sqlite-bind-arguments stmt-select-package
                         #:name package
                         #:version version)
  (match (sqlite-fold cons '() stmt-select-package)
    ((#(package-id))
     (for-each (lambda (directory)
                 (define (strip file)
                   (string-drop file (+ (string-length directory) 1)))

                 ;; If there's already a directory associated with PACKAGE-ID,
                 ;; not necessarily the same directory, skip it.  That keeps
                 ;; the database slimmer at the expense of not recording
                 ;; variants of the same package; it also makes indexing
                 ;; faster.
                 (sqlite-reset stmt-select-directory)
                 (sqlite-bind-arguments stmt-select-directory
                                        #:package package-id)
                 (when (null? (sqlite-fold cons '() stmt-select-directory))
                   ;; DIRECTORY is missing so insert it and traverse it.
                   (sqlite-reset stmt-insert-directory)
                   (sqlite-bind-arguments stmt-insert-directory
                                          #:name (store-path-base directory)
                                          #:package package-id)
                   (sqlite-fold (const #t) #t stmt-insert-directory)

                   (let ((directory-id (last-insert-row-id db)))
                     (for-each (lambda (file)
                                 ;; If DIRECTORY is a symlink, (find-files
                                 ;; DIRECTORY) returns the DIRECTORY singleton.
                                 (unless (string=? file directory)
                                   (sqlite-reset stmt-insert-file)
                                   (sqlite-bind-arguments stmt-insert-file
                                                          #:name (strip file)
                                                          #:basename
                                                          (basename file)
                                                          #:directory
                                                          directory-id)
                                   (sqlite-fold (const #t) #t stmt-insert-file)))
                               (find-files directory)))))
               directories)))
  (sqlite-exec db "commit;"))

(define (insert-package db package)
  "Insert all the files of PACKAGE into DB."
  (define stmt-select-package-output
    (sqlite-prepare db "\
SELECT output FROM Packages WHERE name = :name AND version = :version"
                    #:cache? #t))

  (define (known-outputs package)
    ;; Return the list of outputs of PACKAGE already in DB.
    (sqlite-bind-arguments stmt-select-package-output
                           #:name (package-name package)
                           #:version (package-version package))
    (match (sqlite-fold cons '() stmt-select-package-output)
      ((#(outputs ...)) outputs)
      (() '())))

  (with-monad %store-monad
    ;; Since calling 'package->derivation' is expensive, do not call it if the
    ;; outputs of PACKAGE at VERSION are already in DB.
    (munless (lset= string=?
                    (known-outputs package)
                    (package-outputs package))
      (mlet %store-monad ((drv (package->derivation package #:graft? #f)))
        (match (derivation->output-paths drv)
          (((labels . directories) ...)
           (when (every file-exists? directories)
             (insert-files
              db (package-name package) (package-version package) (package-outputs package)
              directories))
           (return #t)))))))

(define (insert-packages-with-progress db packages insert-package)
  "Insert PACKAGES into DB with progress bar reporting, calling INSERT-PACKAGE
for each package to insert."
  (let* ((count    (length packages))
         (prefix   (format #f (G_ "indexing ~h packages") count))
         (progress (progress-reporter/bar count prefix)))
    (call-with-progress-reporter progress
      (lambda (report)
        (for-each (lambda (package)
                    (insert-package db package)
                    (report))
                  packages)))))

(define (index-packages-from-store-with-db db)
  "Index local store packages using DB."
  (with-store store
    (parameterize ((%graft? #f))
      (define (insert-package-from-store db package)
        (run-with-store store (insert-package db package)))
      (let ((packages (fold-packages
                       cons
                       '()
                       #:select? (lambda (package)
                                   (and (not (hidden-package? package))
                                        (not (package-superseded package))
                                        (supported-package? package))))))
        (insert-packages-with-progress
         db packages insert-package-from-store)))))


;;;
;;; Indexing from local profiles.
;;;

(define (all-profiles)
  "Return the list of system profiles."
  (delete-duplicates
   (filter-map (lambda (root)
                 (if (file-exists? (string-append root "/manifest"))
                     root
                     (let ((root (string-append root "/profile")))
                       (and (file-exists? (string-append root "/manifest"))
                            root))))
               (gc-roots))))

(define (profiles->manifest-entries profiles)
  "Return deduplicated manifest entries across all PROFILES."
  (let loop ((visited (set))
             (profiles profiles)
             (entries '()))
    (match profiles
      (()
       entries)
      ((profile . rest)
       (match (false-if-exception (profile-manifest profile))
         (#f
          ;; PROFILE's manifest is unreadable for some reason such as an
          ;; unsupported version.
          (loop visited rest entries))
         (manifest
          (let ((entries visited
                         (fold2 (lambda (entry lst visited)
                                  (let ((item (manifest-entry-item entry)))
                                    (if (set-contains? visited item)
                                        (values lst visited)
                                        (values (cons entry lst)
                                                (set-insert item
                                                            visited)))))
                                entries
                                visited
                                (manifest-transitive-entries manifest))))
            (loop visited rest entries))))))))

(define (insert-manifest-entry db entry)
  "Insert a manifest ENTRY into DB."
  (insert-files db (manifest-entry-name entry)
                (manifest-entry-version entry)
                (list (manifest-entry-output entry))
                (list (manifest-entry-item entry)))) ;FIXME: outputs?

(define (index-packages-from-manifests-with-db db)
  "Index packages entries into DB from the system manifests."
  (info (G_ "traversing local profile manifests...~%"))
  (let ((entries (profiles->manifest-entries (all-profiles))))
    (insert-packages-with-progress db entries insert-manifest-entry)))



;;;
;;; Search.
;;;

(define-record-type <package-match>
  (package-match name version output file)
  package-match?
  (name    package-match-name)
  (version package-match-version)
  (output  package-match-output)
  (file    package-match-file))

(define* (matching-packages db file #:key glob?)
  "Return a list of <package-match> records, one for each package containing
FILE.  When GLOB? is true, interpret FILE as a glob pattern."
  (define match-stmt
    (if glob?
        "f.basename GLOB :file"
        "f.basename = :file"))

  (define lookup-stmt
    (sqlite-prepare db (string-append "\
SELECT p.name, p.version, p.output, d.name, f.name
FROM Packages p
INNER JOIN Files f, Directories d
ON " match-stmt "
  AND d.id = f.directory
  AND p.id = d.package;")))

  (define prefix
    (match (sqlite-fold (lambda (value _) value)
                        #f
                        (sqlite-prepare db "SELECT store FROM SchemaVersion;"))
      (#(prefix) prefix)))

  (sqlite-bind-arguments lookup-stmt #:file file)
  (sqlite-fold (lambda (result lst)
                 (match result
                   (#(package version output directory file)
                    (cons (package-match package version output
                                         (string-append prefix "/"
                                                        directory "/" file))
                          lst))))
               '() lookup-stmt))

(define (print-matching-results matches)
  "Print the MATCHES matching results."
  (for-each (lambda (result)
              (let ((name    (package-match-name result))
                    (version (package-match-version result))
                    (output  (package-match-output result))
                    (file    (package-match-file result)))
                (format #t "~20a ~a~%"
                        (string-append name "@" version
                                       (match output
                                         ("out" "")
                                         (_ (string-append ":" output))))
                        file)))
            matches))


;;;
;;; Options.
;;;

(define (show-help)
  (display (G_ "Usage: guix locate [OPTIONS...] FILE...
Locate FILE and return the list of packages that contain it.\n"))
  (display (G_ "
  -g, --glob          interpret FILE as a glob pattern"))
  (display (G_ "
      --stats         display database statistics"))
  (display (G_ "
  -u, --update        force a database update"))
  (display (G_ "
      --clear         clear the database"))
  (display (G_ "
      --database=FILE store the database in FILE"))
  (newline)
  (display (G_ "
      --method=METHOD use METHOD to select packages to index; METHOD can
                      be 'manifests' (fast) or 'store' (slower)"))
  (newline)
  (display (G_ "
  -h, --help          display this help and exit"))
  (display (G_ "
  -V, --version       display version information and exit"))
  (show-bug-report-information))

(define %options
  (list (option '(#\h "help") #f #f
                (lambda args (leave-on-EPIPE (show-help)) (exit 0)))
        (option '(#\V "version") #f #f
                (lambda (opt name arg result)
                  (show-version-and-exit "guix locate")))
        (option '(#\g "glob") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'glob? #t result)))
        (option '("stats") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'stats? #t result)))
        (option '("database") #f #t
                (lambda (opt name arg result)
                  (alist-cons 'database (const arg)
                              (alist-delete 'database result))))
        (option '(#\u "update") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'update? #t result)))
        (option '("clear") #f #f
                (lambda (opt name arg result)
                  (alist-cons 'clear? #t result)))
        (option '(#\m "method") #f #t
                (lambda (opt name arg result)
                  (match arg
                    ((or "manifests" "store")
                     (alist-cons 'method (string->symbol arg)
                                 (alist-delete 'method result)))
                    (_
                     (leave (G_ "~a: unknown indexing method~%"))))))))

(define %default-options
  `((database . ,suitable-database)
    (method . manifests)))


;;;
;;; Entry point.
;;;

(define-command (guix-locate . args)
  (category main)
  (synopsis "search for packages providing a given file")

  (define age-update-threshold
    ;; Time since database modification after which an update is triggered.
    (* 2 30 (* 24 60 60)))

  (define age-cleanup-threshold
    ;; Time since database modification after which it is cleared.  This is to
    ;; avoid having stale info in the database and an endlessly growing
    ;; database.
    (* 9 30 (* 24 60 60)))

  (with-error-handling
    (let* ((opts     (parse-command-line args %options
                                         (list %default-options)
                                         #:build-options? #f
                                         #:argument-handler
                                         (lambda (arg result)
                                           (alist-cons 'argument arg
                                                       result))))
           (clear?   (assoc-ref opts 'clear?))
           (update?  (assoc-ref opts 'update?))
           (glob?    (assoc-ref opts 'glob?))
           (database ((assoc-ref opts 'database)
                      (or clear? update?) age-update-threshold))
           (method   (assoc-ref opts 'method))
           (files    (reverse (filter-map (match-lambda
                                            (('argument . arg) arg)
                                            (_ #f))
                                          opts))))
      (define* (populate-database database clear?)
        (mkdir-p (dirname database))
        (call-with-database database
          (lambda (db)
            (when clear?
              (clear-database db))
            (match method
              ('manifests
               (index-packages-from-manifests-with-db db))
              ('store
               (index-packages-from-store-with-db db))
              (_
               (leave (G_ "~a: unknown indexing method~%") method))))))

      ;; Populate the database if needed.
      (let* ((stat   (stat database #f))
             (age    (and stat (file-age stat)))
             (clear? (or clear?
                         (and age (>= age age-cleanup-threshold)))))
        (when (or update? clear?
                  (not stat)
                  (>= age age-update-threshold))
          (when clear?
            (info (G_ "clearing database...~%")))
          (info (G_ "indexing files from ~a...~%") (%store-prefix))
          (populate-database database clear?)))

      (if (assoc-ref opts 'stats?)
          (print-statistics database)
          (match (call-with-database database
                   (lambda (db)
                     (append-map (lambda (file)
                                   (matching-packages db file
                                                      #:glob? glob?))
                                 files)))
            (()
             (if (null? files)
                 (unless (or update? (assoc-ref opts 'clear?))
                   (leave (G_ "no files to search for~%")))
                 (leave (N_ "file~{ '~a'~} not found in database '~a'~%"
                            "files~{ '~a'~} not found in database '~a'~%"
                            (length files))
                        files database)))
            (matches
             (leave-on-EPIPE
              (print-matching-results matches))))))))
