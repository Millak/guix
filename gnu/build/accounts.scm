;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu build accounts)
  #:use-module (guix records)
  #:use-module (guix combinators)
  #:use-module (gnu system accounts)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 receive)
  #:use-module (ice-9 rdelim)
  #:export (password-entry
            password-entry?
            password-entry-name
            password-entry-uid
            password-entry-gid
            password-entry-real-name
            password-entry-directory
            password-entry-shell

            shadow-entry
            shadow-entry?
            shadow-entry-name
            shadow-entry-minimum-change-period
            shadow-entry-maximum-change-period
            shadow-entry-change-warning-time
            shadow-entry-maximum-inactivity
            shadow-entry-expiration

            group-entry
            group-entry?
            group-entry-name
            group-entry-gid
            group-entry-members

            subid-entry
            subid-entry?
            subid-entry-name
            subid-entry-start
            subid-entry-count

            %password-lock-file
            write-group
            write-passwd
            write-shadow
            write-subgid
            write-subuid
            read-group
            read-passwd
            read-shadow
            read-subgid
            read-subuid

            %id-min
            %id-max
            %system-id-min
            %system-id-max
            %subordinate-id-min
            %subordinate-id-max
            %subordinate-id-count

            &subordinate-id-error
            subordinate-id-error?
            &subordinate-id-overflow-error
            subordinate-id-overflow-error?
            subordinate-id-overflow-error-range
            &invalid-subid-range-error
            invalid-subid-range-error?
            invalid-subid-range-error-range
            &specific-subid-range-expected-error
            specific-subid-range-expected-error?
            specific-subid-range-expected-error-range
            &generic-subid-range-expected-error
            generic-subid-range-expected-error?
            generic-subid-range-expected-error-range

            user+group-databases
            subuid+subgid-databases))

;;; Commentary:
;;;
;;; This module provides functionality equivalent to the C library's
;;; <shadow.h>, <pwd.h>, and <grp.h> routines, as well as a subset of the
;;; functionality of the Shadow command-line tools.  It can parse and write
;;; /etc/passwd, /etc/shadow, /etc/group, /etc/subuid and /etc/subgid.  It can
;;; also take care of UID and GID allocation in a way similar to what 'useradd'
;;; does.  The same goes for sub UID and sub GID allocation.
;;;
;;; The benefit is twofold: less code is involved, and the ID allocation
;;; strategy and state preservation is made explicit.
;;;
;;; Code:


;;;
;;; Machinery to define user and group databases.
;;;

(define-syntax serialize-field
  (syntax-rules (serialization)
    ((_ entry (field get (serialization ->string string->) _ ...))
     (->string (get entry)))
    ((_ entry (field get _ ...))
     (get entry))))

(define-syntax deserialize-field
  (syntax-rules (serialization)
    ((_ str (field get (serialization ->string string->) _ ...))
     (string-> str))
    ((_ str (field get _ ...))
     str)))

(define-syntax let/fields
  (syntax-rules ()
    ((_ (((name get attributes ...) rest ...) lst) body ...)
     (let ((l lst))
       (let ((name (deserialize-field (car l)
                                      (name get attributes ...))))
         (let/fields ((rest ...) (cdr l)) body ...))))
    ((_ (() lst) body ...)
     (begin body ...))))

(define-syntax define-database-entry
  (syntax-rules (serialization)
    "Define a record data type, as per 'define-record-type*', with additional
information on how to serialize and deserialize the whole database as well as
each field."
    ((_ <record> record make-record record?
        (serialization separator entry->string string->entry)
        fields ...)
     (let-syntax ((field-name
                   (syntax-rules ()
                     ((_ (name _ (... ...))) name))))
       (define-record-type* <record> record make-record
         record?
         fields ...)

       (define (entry->string entry)
         (string-join (list (serialize-field entry fields) ...)
                      (string separator)))

       (define (string->entry str)
         (let/fields ((fields ...) (string-split str #\:))
                     (make-record (field-name fields) ...)))))))


(define number->string*
  (match-lambda
    ((? number? number) (number->string number))
    (_ "")))

(define (false-if-string=? false-string)
  (lambda (str)
    (if (string=? str false-string)
        #f
        str)))

(define (string-if-false str)
  (lambda (obj)
    (if (not obj) str obj)))

(define (comma-separated->list str)
  (string-tokenize str (char-set-complement (char-set #\,))))

(define (list->comma-separated lst)
  (string-join lst ","))


;;;
;;; Database definitions.
;;;

(define-database-entry <password-entry>           ;<pwd.h>
  password-entry make-password-entry
  password-entry?
  (serialization #\: password-entry->string string->password-entry)

  (name       password-entry-name)
  (password   password-entry-password
              (serialization (const "x") (const #f))
              (default "x"))
  (uid        password-entry-uid
              (serialization number->string string->number))
  (gid        password-entry-gid
              (serialization number->string string->number))
  (real-name  password-entry-real-name
              (default ""))
  (directory  password-entry-directory)
  (shell      password-entry-shell
              (default "/bin/sh")))

(define-database-entry <shadow-entry>             ;<shadow.h>
  shadow-entry make-shadow-entry
  shadow-entry?
  (serialization #\: shadow-entry->string string->shadow-entry)

  (name                  shadow-entry-name)       ;string
  (password              shadow-entry-password    ;string | #f
                         (serialization (string-if-false "!")
                                        (false-if-string=? "!"))
                         (default #f))
  (last-change           shadow-entry-last-change ;days since 1970-01-01
                         (serialization number->string* string->number)
                         (default 0))
  (minimum-change-period shadow-entry-minimum-change-period
                         (serialization number->string* string->number)
                         (default #f))            ;days | #f
  (maximum-change-period shadow-entry-maximum-change-period
                         (serialization number->string* string->number)
                         (default #f))            ;days | #f
  (change-warning-time   shadow-entry-change-warning-time
                         (serialization number->string* string->number)
                         (default #f))            ;days | #f
  (maximum-inactivity    shadow-entry-maximum-inactivity
                         (serialization number->string* string->number)
                         (default #f))             ;days | #f
  (expiration            shadow-entry-expiration
                         (serialization number->string* string->number)
                         (default #f))            ;days since 1970-01-01 | #f
  (flags                 shadow-entry-flags       ;"reserved"
                         (serialization number->string* string->number)
                         (default #f)))

(define-database-entry <group-entry>              ;<grp.h>
  group-entry make-group-entry
  group-entry?
  (serialization #\: group-entry->string string->group-entry)

  (name            group-entry-name)
  (password        group-entry-password
                   (serialization (string-if-false "x")
                                  (false-if-string=? "x"))
                   (default #f))
  (gid             group-entry-gid
                   (serialization number->string string->number))
  (members         group-entry-members
                   (serialization list->comma-separated comma-separated->list)
                   (default '())))

(define-database-entry <subid-entry>              ;<subid.h>
  subid-entry make-subid-entry
  subid-entry?
  (serialization #\: subid-entry->string string->subid-entry)

  (name            subid-entry-name)
  (start           subid-entry-start
                   (serialization number->string string->number))
  (count           subid-entry-count
                   (serialization number->string string->number)))

(define %password-lock-file
  ;; The password database lock file used by libc's 'lckpwdf'.  Users should
  ;; grab this lock with 'with-file-lock' when they access the databases.
  "/etc/.pwd.lock")

(define (database-writer file mode entry->string)
  (lambda* (entries #:optional (file-or-port file))
    "Write ENTRIES to FILE-OR-PORT.  When FILE-OR-PORT is a file name, write
to it atomically and set the appropriate permissions."
    (define (write-entries port)
      (for-each (lambda (entry)
                  (display (entry->string entry) port)
                  (newline port))
                (delete-duplicates entries)))

    (if (port? file-or-port)
        (write-entries file-or-port)
        (let* ((template (string-append file-or-port ".XXXXXX"))
               (port     (mkstemp! template)))
          (dynamic-wind
            (const #t)
            (lambda ()
              (chmod port mode)
              (write-entries port)

              (fsync port)
              (close-port port)
              (rename-file template file-or-port))
            (lambda ()
              (unless (port-closed? port)
                (close-port port))
              (when (file-exists? template)
                (delete-file template))))))))

(define write-passwd
  (database-writer "/etc/passwd" #o644 password-entry->string))
(define write-shadow
  (database-writer "/etc/shadow" #o600 shadow-entry->string))
(define write-group
  (database-writer "/etc/group" #o644 group-entry->string))
(define write-subuid
  (database-writer "/etc/subuid" #o644 subid-entry->string))
(define write-subgid
  (database-writer "/etc/subgid" #o644 subid-entry->string))

(define (database-reader file string->entry)
  (lambda* (#:optional (file-or-port file))
    (define (read-entries port)
      (let loop ((entries '()))
        (match (read-line port)
          ((? eof-object?)
           (reverse entries))
          (line
           (loop (cons (string->entry line) entries))))))

    (if (port? file-or-port)
        (read-entries file-or-port)
        (call-with-input-file file-or-port
          read-entries))))

(define read-passwd
  (database-reader "/etc/passwd" string->password-entry))
(define read-shadow
  (database-reader "/etc/shadow" string->shadow-entry))
(define read-group
  (database-reader "/etc/group" string->group-entry))
(define read-subuid
  (database-reader "/etc/subuid" string->subid-entry))
(define read-subgid
  (database-reader "/etc/subgid" string->subid-entry))


;;;
;;; Building databases.
;;;

(define-record-type* <allocation>
  allocation make-allocation
  allocation?
  (ids            allocation-ids (default vlist-null))
  (next-id        allocation-next-id (default %id-min))
  (next-system-id allocation-next-system-id (default %system-id-max)))

(define-record-type* <unused-subid-range>
  unused-subid-range make-unused-subid-range
  unused-subid-range?
  (left    unused-subid-range-left   ;previous unused subuid range or #f
           (default #f))
  (min     unused-subid-range-min    ;lower bound of this unused subuid range
           (default %subordinate-id-min))
  (max     unused-subid-range-max    ;upper bound
           (default %subordinate-id-max))
  (right   unused-subid-range-right  ;next unused subuid range or #f
           (default #f)))

;; Trick to avoid name clashes...
(define-syntax %allocation (identifier-syntax allocation))

;; Minimum and maximum UIDs and GIDs (from find_new_uid.c and find_new_gid.c
;; in Shadow.)
(define %id-min 1000)
(define %id-max 60000)

(define %system-id-min 100)
(define %system-id-max 999)

;; According to Shadow's libmisc/find_new_sub_uids.c and
;; libmisc/find_new_sub_gids.c.
(define %subordinate-id-min 100000)
(define %subordinate-id-max 600100000)
(define %subordinate-id-count 65536)

(define-condition-type &subordinate-id-error &error
  subordinate-id-error?)
(define-condition-type &subordinate-id-overflow-error &subordinate-id-error
  subordinate-id-overflow-error?
  (range subordinate-id-overflow-error))
(define-condition-type &invalid-subid-range-error &subordinate-id-error
  invalid-subid-range-error?
  (range invalid-subid-range-error-range))
(define-condition-type &specific-subid-range-expected-error &subordinate-id-error
  specific-subid-range-expected-error?
  (range specific-subid-range-expected-error-range))
(define-condition-type &generic-subid-range-expected-error &subordinate-id-error
  generic-subid-range-expected-error?
  (range generic-subid-range-expected-error-range))

(define (system-id? id)
  (and (> id %system-id-min)
       (<= id %system-id-max)))

(define (user-id? id)
  (and (>= id %id-min)
       (< id %id-max)))

(define (subordinate-id? id)
  (and (>= id %subordinate-id-min)
       (< id %subordinate-id-max)))

(define* (allocate-id assignment #:key system?)
  "Return two values: a newly allocated ID, and an updated <allocation> record
based on ASSIGNMENT.  If SYSTEM? is true, return a system ID."
  (define next
    ;; Return the next available ID, looping if necessary.
    (if system?
        (lambda (id)
          (let ((next-id (- id 1)))
            (if (< next-id %system-id-min)
                %system-id-max
                next-id)))
        (lambda (id)
          (let ((next-id (+ id 1)))
            (if (>= next-id %id-max)
                %id-min
                next-id)))))

  (let loop ((id (if system?
                     (allocation-next-system-id assignment)
                     (allocation-next-id assignment))))
    (if (vhash-assv id (allocation-ids assignment))
        (loop (next id))
        (let ((taken (vhash-consv id #t (allocation-ids assignment))))
          (values (if system?
                      (allocation (inherit assignment)
                                  (next-system-id (next id))
                                  (ids taken))
                      (allocation (inherit assignment)
                                  (next-id (next id))
                                  (ids taken)))
                  id)))))

(define* (reserve-ids allocation ids #:key (skip? #t))
  "Mark the numbers listed in IDS as reserved in ALLOCATION.  When SKIP? is
true, start allocation after the highest (or lowest, depending on whether it's
a system ID allocation) number among IDS."
  (%allocation
   (inherit allocation)
   (next-id (if skip?
                (+ (reduce max
                           (- (allocation-next-id allocation) 1)
                           (filter user-id? ids))
                   1)
                (allocation-next-id allocation)))
   (next-system-id
    (if skip?
        (- (reduce min
                   (+ 1 (allocation-next-system-id allocation))
                   (filter system-id? ids))
           1)
        (allocation-next-system-id allocation)))
   (ids (fold (cut vhash-consv <> #t <>)
              (allocation-ids allocation)
              ids))))

(define (within-interval? allocation range)
  "Returns #t when RANGE is included in the ALLOCATION.
Both ends of the ALLOCATION are included in the comparison."
  (define allocation-start
    (unused-subid-range-min allocation))
  (define allocation-end
    (unused-subid-range-max allocation))
  (unless (subid-range-has-start? range)
    (raise
     (condition
      (&specific-subid-range-expected-error
       (range range)))))
  (and (<= allocation-start
           (subid-range-start range))
       (<= (subid-range-end range)
           allocation-end)))

(define (allocate-unused-range allocation actual-range)
  "Allocates RANGE inside ALLOCATION.  RANGE is assumed to be
@code{within-interval?} of ALLOCATION, a new @code{unused-subid-range} record is
returned with all the subids contained in RANGE marked as used."
  (define allocation-start
    (unused-subid-range-min allocation))
  (define allocation-end
    (unused-subid-range-max allocation))
  (define allocation-left
    (unused-subid-range-left allocation))
  (define allocation-right
    (unused-subid-range-left allocation))
  (define range-start
    (subid-range-start actual-range))
  (define range-end
    (subid-range-end actual-range))
  (define new-start
    (+ 1 range-end))
  (define new-end
    (- range-start 1))
  (if (or (= allocation-start range-start)
          (= allocation-end range-end))
      (unused-subid-range
       (inherit allocation)
       (min (if (= allocation-start range-start)
                new-start
                allocation-start))
       (max (if (= allocation-end range-end)
                new-end
                allocation-end)))
      (let* ((left-child?
              (<= (- range-start allocation-start)
                  (- allocation-end range-end)))
             (child
              (unused-subid-range
               (min allocation-start)
               (max new-end)
               (left
                (and left-child?
                     allocation-left))
               (right
                (and (not left-child?)
                     allocation-right)))))
        (unused-subid-range
         (inherit allocation)
         (min new-start)
         (max allocation-end)
         (left
          (if left-child?
              child
              allocation-left))
         (right
          (if left-child?
              allocation-right
              child))))))

(define (allocate-generic-range allocation range)
  "Allocates a range of subids in ALLOCATION, based on RANGE.  RANGE is expected
to be a generic range i.e. to not have an explicit start subordinate id.  All
nodes in ALLOCATION are visited and the first where RANGE is
@code{within-interval?} will be selected, the subordinate ids contained in RANGE
will be marked as used in it."
  (when (subid-range-has-start? range)
    (raise
     (condition
      (&generic-subid-range-expected-error
       (ranges range)))))
  (define left (unused-subid-range-left allocation))
  (define right (unused-subid-range-right allocation))
  (define allocation-start
    (unused-subid-range-min allocation))
  (define actual-range
    (subid-range
     (inherit range)
     (start allocation-start)))

  (if (within-interval? allocation actual-range)
      (values
       (allocate-unused-range allocation actual-range)
       actual-range)
      (if left
          (let-values (((new-left new-range)
                        (allocate-generic-range left range)))
            (values (unused-subid-range
                     (inherit allocation)
                     (left new-left))
                    new-range))
          (if right
              (let-values (((new-right new-range)
                            (allocate-generic-range right range)))
                (values (unused-subid-range
                         (inherit allocation)
                         (left new-right))
                        new-range))
              (raise
               (condition
                (&subordinate-id-overflow-error
                 (range range))))))))

(define (allocate-specific-range allocation range)
  "Allocates a range of subids in ALLOCATION, based on RANGE.  RANGE is expected
to be a specific range i.e. to have an explicit start subordinate id.  ALLOCATION
is visited to find the best unused range that can hold RANGE."
  (unless (subid-range-has-start? range)
    (raise
     (condition
      (&specific-subid-range-expected-error
       (range range)))))
  (define allocation-left
    (unused-subid-range-left allocation))
  (define allocation-right
    (unused-subid-range-right allocation))
  (define allocation-start
    (unused-subid-range-min allocation))
  (define allocation-end
    (unused-subid-range-max allocation))

  (define range-start
    (subid-range-start range))
  (define range-end
    (subid-range-end range))

  (unless (and (subordinate-id? range-start)
               (subordinate-id? range-end))
    (raise
     (condition
      (&invalid-subid-range-error
       (range range)))))

  (define less?
    (< range-end allocation-start))
  (define more?
    (> range-start allocation-end))

  (cond ((within-interval? allocation range)
         (values (allocate-unused-range allocation range)
                 range))
        ((and allocation-left less?)
         (let-values (((new-left _)
                       (allocate-specific-range allocation-left range)))
           (values (unused-subid-range
                    (inherit allocation)
                    (left new-left))
                   range)))
        ((and allocation-right more?)
         (let-values (((new-right _)
                       (allocate-specific-range allocation-right range)))
           (values (unused-subid-range
                    (inherit allocation)
                    (right new-right))
                   range)))
        (else
         (raise
          (condition
           (&subordinate-id-overflow-error
            (range range)))))))

(define* (reserve-subids allocation ranges)
  "Mark the subid ranges listed in RANGES as reserved in ALLOCATION."
  (fold (lambda (range state)
          (define-values (new-allocation actual-range)
            ((if (subid-range-has-start? range)
                 allocate-specific-range
                 allocate-generic-range)
             (first state)
             range))
          (list new-allocation
                (cons actual-range
                      (second state))))
        (list allocation '()) ranges))

(define (allocated? allocation id)
  "Return true if ID is already allocated as part of ALLOCATION."
  (->bool (vhash-assv id (allocation-ids allocation))))

(define (lookup-procedure lst key)
  "Return a lookup procedure for the elements of LST, calling KEY to obtain
the key of each element."
  (let ((table (fold (lambda (obj table)
                       (vhash-cons (key obj) obj table))
                     vlist-null
                     lst)))
    (lambda (key)
      (match (vhash-assoc key table)
        (#f #f)
        ((_ . value) value)))))

(define* (allocate-groups groups members
                          #:optional (current-groups '()))
  "Return a list of group entries for GROUPS, a list of <user-group>.  Members
for each group are taken from MEMBERS, a vhash that maps group names to member
names.  GIDs and passwords found in CURRENT-GROUPS, a list of group entries,
are reused."
  (define gids
    ;; Mark all the currently-used GIDs and the explicitly requested GIDs as
    ;; reserved.
    (reserve-ids (reserve-ids (allocation)
                              (map group-entry-gid current-groups))
                 (filter-map user-group-id groups)
                 #:skip? #f))

  (define previous-entry
    (lookup-procedure current-groups group-entry-name))

  (reverse
   (fold2 (lambda (group result allocation)
            (let ((name         (user-group-name group))
                  (password     (user-group-password group))
                  (requested-id (user-group-id group))
                  (system?      (user-group-system? group)))
              (let*-values (((previous)
                             (previous-entry name))
                            ((allocation id)
                             (cond
                              ((number? requested-id)
                               (values (reserve-ids allocation
                                                    (list requested-id))
                                       requested-id))
                              (previous
                               (values allocation
                                       (group-entry-gid previous)))
                              (else
                               (allocate-id allocation
                                            #:system? system?)))))
                (values (cons (group-entry
                               (name name)
                               (password
                                (if previous
                                    (group-entry-password previous)
                                    password))
                               (gid id)
                               (members (vhash-fold* cons '() name members)))
                              result)
                        allocation))))
          '()
          gids
          groups)))

(define* (allocate-passwd users groups #:optional (current-passwd '()))
  "Return a list of password entries for USERS, a list of <user-account>.
Take GIDs from GROUPS, a list of group entries.  Reuse UIDs from
CURRENT-PASSWD, a list of password entries, when possible; otherwise allocate
new UIDs."
  (define uids
    (reserve-ids (reserve-ids (allocation)
                              (map password-entry-uid current-passwd))
                 (filter-map user-account-uid users)
                 #:skip? #f))

  (define previous-entry
    (lookup-procedure current-passwd password-entry-name))

  (define (group-id name)
    (or (any (lambda (entry)
               (and (string=? (group-entry-name entry) name)
                    (group-entry-gid entry)))
             groups)
        (error "group not found" name)))

  (reverse
   (fold2 (lambda (user result allocation)
            (let ((name         (user-account-name user))
                  (requested-id (user-account-uid user))
                  (group        (user-account-group user))
                  (real-name    (user-account-comment user))
                  (directory    (user-account-home-directory user))
                  (shell        (user-account-shell user))
                  (system?      (user-account-system? user)))
              (let*-values (((previous)
                             (previous-entry name))
                            ((allocation id)
                             (cond
                              ((number? requested-id)
                               (values (reserve-ids allocation
                                                    (list requested-id))
                                       requested-id))
                              (previous
                               (values allocation
                                       (password-entry-uid previous)))
                              (else
                               (allocate-id allocation
                                            #:system? system?)))))
                (values (cons (password-entry
                               (name name)
                               (uid id)
                               (directory directory)
                               (gid (if (number? group) group (group-id group)))

                               ;; Users might change their name to something
                               ;; other than what the sysadmin chose, with
                               ;; 'chfn'.  Thus consider it "stateful".
                               (real-name (if (and previous (not system?))
                                              (password-entry-real-name previous)
                                              real-name))

                               ;; Do not reuse the shell of PREVIOUS since (1)
                               ;; that could lead to confusion, and (2) the
                               ;; shell might have been GC'd.  See
                               ;; <https://lists.gnu.org/archive/html/guix-devel/2019-04/msg00478.html>.
                               (shell shell))
                              result)
                        allocation))))
          '()
          uids
          users)))

(define (range->entry range)
  (subid-entry
   (name (subid-range-name range))
   (start (subid-range-start range))
   (count (subid-range-count range))))

(define (entry->range entry)
  (subid-range
   (name (subid-entry-name entry))
   (start (subid-entry-start entry))
   (count (subid-entry-count entry))))

(define* (allocate-subids ranges #:optional (current-ranges '()))
  "Return a list of subids entries for RANGES, a list of <subid-range>.  IDs
found in CURRENT-RANGES, a list of subid entries, are reused."
  (let ((generic (any (compose not subid-range-has-start?) current-ranges)))
    (when generic
      (raise
       (condition
        (&specific-subid-range-expected-error
         (range generic))))))
  (define sorted-ranges
    (stable-sort ranges
                 subid-range-less))
  (define current-allocation+subids
    (reserve-subids (unused-subid-range)
                    current-ranges))
  (define subids
    ;; Reserve first specific subid-ranges
    ;; and later generic ones.
    (second
     (reserve-subids (first
                      current-allocation+subids)
                     sorted-ranges)))

  (map range->entry
       ;; Produce deterministic subid collections.
       (stable-sort
        (append (second current-allocation+subids)
                subids)
        subid-range-less)))

(define* (days-since-epoch #:optional (current-time current-time))
  "Return the number of days elapsed since the 1st of January, 1970."
  (let* ((now   (current-time time-utc))
         (epoch (make-time time-utc 0 0))
         (diff  (time-difference now epoch)))
    (quotient (time-second diff) (* 24 3600))))

(define* (passwd->shadow users passwd #:optional (current-shadow '())
                         #:key (current-time current-time))
  "Return a list of shadow entries for the password entries listed in PASSWD.
Reuse shadow entries from CURRENT-SHADOW when they exist, and take the initial
password from USERS."
  (define previous-entry
    (lookup-procedure current-shadow shadow-entry-name))

  (define now
    ;; On machines without a real-time clock (typically Arm SBCs), the system
    ;; clock may be at 1970-01-01 while booting, which would lead us to define
    ;; NOW as zero.
    ;;
    ;; However, the 'isexpired' function in Shadow interprets the combination
    ;; uninitialized password + last-change = 0 as "The password has expired,
    ;; it must be changed", which prevents logins altogether.  To avoid that,
    ;; never set 'last-change' to zero.
    (max (days-since-epoch current-time) 1))

  (map (lambda (user passwd)
         (or (previous-entry (password-entry-name passwd))
             (shadow-entry (name (password-entry-name passwd))
                           (password (user-account-password user))
                           (last-change now))))
       users passwd))

(define (empty-if-not-found thunk)
  "Call THUNK and return the empty list if that throws to ENOENT."
  (catch 'system-error
    thunk
    (lambda args
      (if (= ENOENT (system-error-errno args))
          '()
          (apply throw args)))))

(define* (user+group-databases users groups
                               #:key
                               (current-passwd
                                (empty-if-not-found read-passwd))
                               (current-groups
                                (empty-if-not-found read-group))
                               (current-shadow
                                (empty-if-not-found read-shadow))
                               (current-time current-time))
  "Return three values: the list of group entries, the list of password
entries, and the list of shadow entries corresponding to USERS and GROUPS.
Preserve stateful bits from CURRENT-PASSWD, CURRENT-GROUPS, and
CURRENT-SHADOW: UIDs, GIDs, passwords, user shells, etc."
  (define members
    ;; Map group name to user names.
    (fold (lambda (user members)
            (fold (cute vhash-cons <> (user-account-name user) <>)
                  members
                  (user-account-supplementary-groups user)))
          vlist-null
          users))

  (define group-entries
    (allocate-groups groups members current-groups))

  (define passwd-entries
    (allocate-passwd users group-entries current-passwd))

  (define shadow-entries
    (passwd->shadow users passwd-entries current-shadow
                    #:current-time current-time))

  (values group-entries passwd-entries shadow-entries))

(define* (subuid+subgid-databases subuids subgids
                                  #:key
                                  (current-subuids
                                   (map entry->range
                                        (empty-if-not-found read-subuid)))
                                  (current-subgids
                                   (map entry->range
                                        (empty-if-not-found read-subgid))))
  "Return two values: the list of subgid entries, and the list of subuid entries
corresponding to SUBUIDS and SUBGIDS.
Preserve stateful bits from CURRENT-SUBUIDS and CURRENT-SUBGIDS."

  (define (range-eqv? a b)
    (string=? (subid-range-name a)
              (subid-range-name b)))

  (define subuid-entries
    (allocate-subids
     (lset-difference range-eqv? subuids current-subuids) current-subuids))

  (define subgid-entries
    (allocate-subids
     (lset-difference range-eqv? subgids current-subgids) current-subgids))

  (values subuid-entries subgid-entries))
