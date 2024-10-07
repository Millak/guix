;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-accounts)
  #:use-module (gnu build accounts)
  #:use-module (gnu system accounts)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match))

(define %passwd-sample
  "\
root:x:0:0:Admin:/root:/bin/sh
charlie:x:1000:998:Charlie:/home/charlie:/bin/sh\n")

(define %group-sample
  "\
root:x:0:
wheel:x:999:alice,bob
hackers:x:65000:alice,charlie\n")

(define %shadow-sample
  (string-append "\
root:" (crypt "secret" "$6$abc") ":17169::::::
charlie:" (crypt "hey!" "$6$abc") ":17169::::::
nobody:!:0::::::\n"))

(define %subuid-sample
  "\
root:100000:300
ada:100300:300\n")

(define %subgid-sample
  "\
root:100000:600
ada:100600:300\n")


(test-begin "accounts")

(test-equal "write-passwd"
  %passwd-sample
  (call-with-output-string
    (lambda (port)
      (write-passwd (list (password-entry
                           (name "root")
                           (uid 0) (gid 0)
                           (real-name "Admin")
                           (directory "/root")
                           (shell "/bin/sh"))
                          (password-entry
                           (name "charlie")
                           (uid 1000) (gid 998)
                           (real-name "Charlie")
                           (directory "/home/charlie")
                           (shell "/bin/sh")))
                    port))))

(test-equal "write-passwd with duplicate entry"
  %passwd-sample
  (call-with-output-string
    (lambda (port)
      (let ((charlie (password-entry
                      (name "charlie")
                      (uid 1000) (gid 998)
                      (real-name "Charlie")
                      (directory "/home/charlie")
                      (shell "/bin/sh"))))
        (write-passwd (list (password-entry
                             (name "root")
                             (uid 0) (gid 0)
                             (real-name "Admin")
                             (directory "/root")
                             (shell "/bin/sh"))
                            charlie charlie)
                      port)))))

(test-equal "read-passwd + write-passwd"
  %passwd-sample
  (call-with-output-string
    (lambda (port)
      (write-passwd (call-with-input-string %passwd-sample
                      read-passwd)
                    port))))

(test-equal "write-group"
  %group-sample
  (call-with-output-string
    (lambda (port)
      (write-group (list (group-entry
                          (name "root") (gid 0))
                         (group-entry
                          (name "wheel") (gid 999)
                          (members '("alice" "bob")))
                         (group-entry
                          (name "hackers") (gid 65000)
                          (members '("alice" "charlie"))))
                   port))))

(test-equal "read-group + write-group"
  %group-sample
  (call-with-output-string
    (lambda (port)
      (write-group (call-with-input-string %group-sample
                     read-group)
                   port))))

(test-equal "write-shadow"
  %shadow-sample
  (call-with-output-string
    (lambda (port)
      (write-shadow (list (shadow-entry
                           (name "root")
                           (password (crypt "secret" "$6$abc"))
                           (last-change 17169))
                          (shadow-entry
                           (name "charlie")
                           (password (crypt "hey!" "$6$abc"))
                           (last-change 17169))
                          (shadow-entry
                           (name "nobody")))
                    port))))

(test-equal "read-shadow + write-shadow"
  %shadow-sample
  (call-with-output-string
    (lambda (port)
      (write-shadow (call-with-input-string %shadow-sample
                      read-shadow)
                    port))))

(test-equal "write-subuid"
  %subuid-sample
  (call-with-output-string
    (lambda (port)
      (write-subuid (list (subid-entry
                           (name "root")
                           (start 100000)
                           (count 300))
                          (subid-entry
                           (name "ada")
                           (start 100300)
                           (count 300)))
                    port))))

(test-equal "read-subuid + write-subuid"
  %subuid-sample
  (call-with-output-string
    (lambda (port)
      (write-subuid (call-with-input-string %subuid-sample
                      read-subuid)
                    port))))

(test-equal "write-subgid"
  %subgid-sample
  (call-with-output-string
    (lambda (port)
      (write-subgid (list (subid-entry
                           (name "root")
                           (start 100000)
                           (count 600))
                          (subid-entry
                           (name "ada")
                           (start 100600)
                           (count 300)))
                    port))))

(test-equal "read-subgid + write-subgid"
  %subgid-sample
  (call-with-output-string
    (lambda (port)
      (write-subgid (call-with-input-string %subgid-sample
                      read-subgid)
                    port))))


(define allocate-groups (@@ (gnu build accounts) allocate-groups))
(define allocate-passwd (@@ (gnu build accounts) allocate-passwd))
(define allocate-subids (@@ (gnu build accounts) allocate-subids))

(test-equal "allocate-groups"
  ;; Allocate GIDs in a stateless fashion.
  (list (group-entry (name "s") (gid %system-id-max))
        (group-entry (name "x") (gid 900))
        (group-entry (name "t") (gid 899))
        (group-entry (name "a") (gid %id-min) (password "foo")
                     (members '("alice" "bob")))
        (group-entry (name "b") (gid (+ %id-min 1))
                     (members '("charlie"))))
  (allocate-groups (list (user-group (name "s") (system? #t))
                         (user-group (name "x") (id 900))
                         (user-group (name "t") (system? #t))
                         (user-group (name "a") (password "foo"))
                         (user-group (name "b")))
                   (alist->vhash `(("a" . "bob")
                                   ("a" . "alice")
                                   ("b" . "charlie")))))

(test-equal "allocate-groups with requested GIDs"
  ;; Make sure the requested GID for "b" is honored.
  (list (group-entry (name "a") (gid (+ 1 %id-min)))
        (group-entry (name "b") (gid %id-min))
        (group-entry (name "c") (gid (+ 2 %id-min))))
  (allocate-groups (list (user-group (name "a"))
                         (user-group (name "b") (id %id-min))
                         (user-group (name "c")))
                   vlist-null))

(test-equal "allocate-groups with previous state"
  ;; Make sure bits of state are preserved: password, GID, no reuse of
  ;; previously-used GIDs.
  (list (group-entry (name "s") (gid (- %system-id-max 1)))
        (group-entry (name "t") (gid (- %system-id-max 2)))
        (group-entry (name "a") (gid 30000) (password #f)
                     (members '("alice" "bob")))
        (group-entry (name "b") (gid 30001) (password "bar")
                     (members '("charlie"))))
  (allocate-groups (list (user-group (name "s") (system? #t))
                         (user-group (name "t") (system? #t))
                         (user-group (name "a") (password "foo"))
                         (user-group (name "b")))
                   (alist->vhash `(("a" . "bob")
                                   ("a" . "alice")
                                   ("b" . "charlie")))
                   (list (group-entry (name "a") (gid 30000))
                         (group-entry (name "b") (gid 30001)
                                      (password "bar"))
                         (group-entry (name "removed")
                                      (gid %system-id-max)))))

(test-equal "allocate-groups with previous state, looping"
  ;; Check that allocation starts after the highest previously-used GID, and
  ;; loops back to the lowest GID.
  (list (group-entry (name "a") (gid (- %id-max 1)))
        (group-entry (name "b") (gid %id-min))
        (group-entry (name "c") (gid (+ 1 %id-min))))
  (allocate-groups (list (user-group (name "a"))
                         (user-group (name "b"))
                         (user-group (name "c")))
                   vlist-null
                   (list (group-entry (name "d")
                                      (gid (- %id-max 2))))))

(test-equal "allocate-subids"
  ;; Allocate sub IDs in a stateless fashion.
  (list (subid-entry (name "root") (start %subordinate-id-min) (count 100))
        (subid-entry (name "t") (start 100100) (count 899))
        (subid-entry (name "x") (start 100999) (count 200)))
  (allocate-subids (list
                    (subid-range (name "x") (count 200))
                    (subid-range (name "t") (count 899)))
                   (list (subid-range (name "root")
                                      (start %subordinate-id-min)
                                      (count 100)))))

(test-equal "allocate-subids with requested IDs ranges"
  ;; Make sure the requested sub ID for "k" and "root" are honored.
  (list (subid-entry (name "x") (start %subordinate-id-min) (count 200))
        (subid-entry (name "k") (start (+ %subordinate-id-min 300)) (count 100))
        (subid-entry (name "t") (start (+ %subordinate-id-min 500)) (count 899))
        (subid-entry (name "root") (start (+ %subordinate-id-min 2500)) (count 100)))

  (allocate-subids (list
                    (subid-range (name "root") (start (+ %subordinate-id-min 2500)) (count 100))
                    (subid-range (name "k") (start (+ %subordinate-id-min 300)) (count 100)))
                   (list
                    (subid-range (name "x") (start %subordinate-id-min) (count 200))
                    (subid-range (name "t") (start (+ %subordinate-id-min 500)) (count 899)))))

(test-assert "allocate-subids, impossible allocations - ranges must have start"
  (guard (c ((specific-subid-range-expected-error? c)
             #t))
    (allocate-subids (list (subid-range (name "m"))) (list (subid-range (name "x"))))
    #f))

(test-assert "allocate-subids, impossible allocations - ranges must fall within allowed max min subids"
  (guard (c ((invalid-subid-range-error? c)
             #t))
    (allocate-subids
     (list (subid-range (name "m")
                        (start (- %subordinate-id-min 1))
                        (count
                         (+ %subordinate-id-max %subordinate-id-min))))
     (list
      (subid-range (name "root") (start %subordinate-id-min))))
    #f))

(test-equal "allocate-subids with interleaving"
  ;; Make sure the requested sub ID for "m" is honored and
  ;; for "l" and "i" are correctly deduced.
  (list (subid-entry (name "x") (start %subordinate-id-min) (count 200))
        (subid-entry (name "m") (start (+ %subordinate-id-min 201)) (count 27))
        (subid-entry (name "root") (start (+ %subordinate-id-min 231)) (count 100))
        (subid-entry (name "i") (start (+ %subordinate-id-min 331)) (count 2))
        (subid-entry (name "l") (start (+ %subordinate-id-min 333)) (count 1)))
  (allocate-subids (list
                    (subid-range (name "m") (start (+ %subordinate-id-min 201)) (count 27))
                    (subid-range (name "l") (count 1))
                    (subid-range (name "i") (count 2)))
                   (list
                    (subid-range (name "x") (start %subordinate-id-min) (count 200))
                    (subid-range (name "root") (start (+ %subordinate-id-min 231)) (count 100)))))

(test-assert "allocate-subids with interleaving, impossible interleaving - before"
  (guard (c ((subordinate-id-overflow-error? c)
             #t))
    (allocate-subids
     (list (subid-range (name "m") (start %subordinate-id-min) (count 16)))
     (list
      (subid-range (name "x") (start (+ 15 %subordinate-id-min)) (count 150))))
    #f))

(test-assert "allocate-subids with interleaving, impossible interleaving - after"
  (guard (c ((subordinate-id-overflow-error? c)
             #t))
    (allocate-subids
     (list (subid-range (name "m") (start %subordinate-id-min) (count 30)))
     (list
      (subid-range (name "x") (start (+ 29 %subordinate-id-min)) (count 150))))
    #f))

(test-assert "allocate-subids with interleaving, impossible interleaving - between"
  (guard (c ((subordinate-id-overflow-error? c)
             #t))
    (allocate-subids
     (list (subid-range (name "m") (start 100200) (count 500)))
     (list
      (subid-range (name "root") (start %subordinate-id-min) (count 100))
      (subid-range (name "x") (start (+ %subordinate-id-min 500)) (count 100))))
    #f))

(test-equal "allocate-passwd"
  ;; Allocate UIDs in a stateless fashion.
  (list (password-entry (name "alice") (uid %id-min) (gid 1000)
                        (real-name "Alice") (shell "/bin/sh")
                        (directory "/home/alice"))
        (password-entry (name "bob") (uid (+ 1 %id-min)) (gid 1001)
                        (real-name "Bob") (shell "/bin/gash")
                        (directory "/home/bob"))
        (password-entry (name "sshd") (uid %system-id-max) (gid 500)
                        (real-name "sshd") (shell "/nologin")
                        (directory "/var/empty"))
        (password-entry (name "guix") (uid 30000) (gid 499)
                        (real-name "Guix") (shell "/nologin")
                        (directory "/var/empty")))
  (allocate-passwd (list (user-account (name "alice")
                                       (comment "Alice")
                                       (shell "/bin/sh")
                                       (group "users"))
                         (user-account (name "bob")
                                       (comment "Bob")
                                       (shell "/bin/gash")
                                       (group "wheel"))
                         (user-account (name "sshd") (system? #t)
                                       (comment "sshd")
                                       (home-directory "/var/empty")
                                       (shell "/nologin")
                                       (group "sshd"))
                         (user-account (name "guix") (system? #t)
                                       (comment "Guix")
                                       (home-directory "/var/empty")
                                       (shell "/nologin")
                                       (group "guix")
                                       (uid 30000)))
                   (list (group-entry (name "users") (gid 1000))
                         (group-entry (name "wheel") (gid 1001))
                         (group-entry (name "sshd") (gid 500))
                         (group-entry (name "guix") (gid 499)))))

(test-equal "allocate-passwd with previous state"
  ;; Make sure bits of state are preserved: UID, no reuse of previously-used
  ;; UIDs, and shell.
  (list (password-entry (name "alice") (uid 1234) (gid 1000)
                        (real-name "Alice Smith") (shell "/bin/sh")
                        (directory "/home/alice"))
        (password-entry (name "charlie") (uid 1236) (gid 1000)
                        (real-name "Charlie") (shell "/bin/sh")
                        (directory "/home/charlie")))
  (allocate-passwd (list (user-account (name "alice")
                                       (comment "Alice")
                                       (shell "/bin/sh") ;honored
                                       (group "users"))
                         (user-account (name "charlie")
                                       (comment "Charlie")
                                       (shell "/bin/sh")
                                       (group "users")))
                   (list (group-entry (name "users") (gid 1000)))
                   (list (password-entry (name "alice") (uid 1234) (gid 9999)
                                         (real-name "Alice Smith")
                                         (shell "/gnu/.../bin/gash") ;ignored
                                         (directory "/home/alice"))
                         (password-entry (name "bob") (uid 1235) (gid 1001)
                                         (real-name "Bob") (shell "/bin/sh")
                                         (directory "/home/bob")))))

(test-equal "user+group-databases"
  ;; The whole shebang.
  (list (list (group-entry (name "a") (gid %id-min)
                           (members '("bob")))
              (group-entry (name "b") (gid (+ 1 %id-min))
                           (members '("alice")))
              (group-entry (name "s") (gid %system-id-max)))
        (list (password-entry (name "alice") (real-name "Alice")
                              (uid %id-min) (gid %id-min)
                              (directory "/a"))
              (password-entry (name "bob") (real-name "Bob")
                              (uid (+ 1 %id-min)) (gid (+ 1 %id-min))
                              (directory "/b"))
              (password-entry (name "nobody")
                              (uid 65534) (gid %system-id-max)
                              (directory "/var/empty")))
        (list (shadow-entry (name "alice") (last-change 100)
                            (password (crypt "initial pass" "$6$")))
              (shadow-entry (name "bob") (last-change 50)
                            (password (crypt "foo" "$6$")))
              (shadow-entry (name "nobody") (last-change 100))))
  (call-with-values
      (lambda ()
        (user+group-databases (list (user-account
                                     (name "alice")
                                     (comment "Alice")
                                     (home-directory "/a")
                                     (group "a")
                                     (supplementary-groups '("b"))
                                     (password (crypt "initial pass" "$6$")))
                                    (user-account
                                     (name "bob")
                                     (comment "Bob")
                                     (home-directory "/b")
                                     (group "b")
                                     (supplementary-groups '("a")))
                                    (user-account
                                     (name "nobody")
                                     (group "s")
                                     (uid 65534)
                                     (home-directory "/var/empty")))
                              (list (user-group (name "a"))
                                    (user-group (name "b"))
                                    (user-group (name "s") (system? #t)))
                              #:current-passwd '()
                              #:current-shadow
                              (list (shadow-entry (name "bob")
                                                  (password (crypt "foo" "$6$"))
                                                  (last-change 50)))
                              #:current-groups '()
                              #:current-time
                              (lambda (type)
                                (make-time type 0 (* 24 3600 100)))))
    list))

(test-equal "subuid+subgid-databases"
  ;; The whole process.
  (list (list (subid-entry (name "root")
                           (start %subordinate-id-min)
                           (count 100))
              (subid-entry (name "alice")
                           (start (+ %subordinate-id-min 100))
                           (count 200))
              (subid-entry (name "bob")
                           (start (+ %subordinate-id-min 100 200))
                           (count 200)))
        (list
         (subid-entry (name "root")
                      (start %subordinate-id-min)
                      (count 200))
         (subid-entry (name "alice")
                      (start (+ %subordinate-id-min 200))
                      (count 400))
         (subid-entry (name "charlie")
                      (start (+ %subordinate-id-min 200 400))
                      (count 300))))
  (call-with-values
      (lambda ()
        (subuid+subgid-databases
         (list (subid-range (name "root")
                            (start %subordinate-id-min)
                            (count 100))
               (subid-range (name "alice")
                            (start (+ %subordinate-id-min 100))
                            (count 200))
               (subid-range (name "bob")
                            (count 200)))
         (list
          (subid-range (name "alice")
                       (count 400))
          (subid-range (name "charlie")
                       (count 300)))
         #:current-subgids
         (list (subid-range (name "root")
                            (start %subordinate-id-min)
                            (count 200)))
         #:current-subuids '()))
    list))

(test-end "accounts")
