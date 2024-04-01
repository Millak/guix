;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019-2021, 2024 Ludovic Courtès <ludo@gnu.org>
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

(define-module (test-swh)
  #:use-module (guix swh)
  #:use-module (guix base32)
  #:use-module (guix tests http)
  #:use-module (web response)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-64)
  #:use-module (ice-9 match))

;; Test the JSON mapping machinery used in (guix swh).

(define %origin
  "{ \"origin_visits_url\": \"/visits/42\",
     \"type\": \"git\",
     \"url\": \"http://example.org/guix.git\" }")

(define %visits
  ;; A single visit where 'snapshot_url' is null.
  ;; See <https://bugs.gnu.org/45615>.
  "[ {
    \"origin\": \"https://github.com/Genivia/ugrep\",
    \"visit\": 1,
    \"date\": \"2020-05-17T21:43:45.422977+00:00\",
    \"status\": \"ongoing\",
    \"snapshot\": null,
    \"metadata\": {},
    \"type\": \"git\",
    \"origin_visit_url\": \"https://archive.softwareheritage.org/api/1/origin/https://github.com/Genivia/ugrep/visit/1/\",
    \"snapshot_url\": null
  } ]")

(define %directory-entries
  "[ { \"name\": \"one\",
       \"type\": \"regular\",
       \"length\": 123,
       \"dir_id\": 1 },
     { \"name\": \"two\",
       \"type\": \"regular\",
       \"length\": 456,
       \"dir_id\": 2 } ]")

(define %external-id
  "{ \"extid_type\": \"nar-sha256\",
     \"extid\":
\"0b56ba94c2b83b8f74e3772887c1109135802eb3e8962b628377987fe97e1e63\",
     \"version\": 0,
     \"target\": \"swh:1:dir:84a8b34591712c0a90bab0af604188bcd1fe3153\",
     \"target_url\":
\"https://archive.softwareheritage.org/swh:1:dir:84a8b34591712c0a90bab0af604188bcd1fe3153\"
   }")

(define-syntax-rule (with-json-result str exp ...)
  (with-http-server `((200 ,str))
    (parameterize ((%swh-base-url (%local-url)))
      exp ...)))

(test-begin "swh")

(test-equal "lookup-origin"
  (list "git" "http://example.org/guix.git")
  (with-json-result %origin
    (let ((origin (lookup-origin "http://example.org/guix.git")))
      (list (origin-type origin)
            (origin-url origin)))))

(test-equal "lookup-origin, not found"
  #f
  (with-http-server `((404 "Nope."))
    (parameterize ((%swh-base-url (%local-url)))
      (lookup-origin "http://example.org/whatever"))))

(test-equal "origin-visit, no snapshots"
  '("https://github.com/Genivia/ugrep"
    "2020-05-17T21:43:45Z"
    #f)                                      ;see <https://bugs.gnu.org/45615>
  (with-http-server `((200 ,%origin)
                      (200 ,%visits))
    (parameterize ((%swh-base-url (%local-url)))
      (let ((origin (lookup-origin "http://example.org/whatever")))
        (match (origin-visits origin)
          ((visit)
           (list (visit-origin visit)
                 (date->string (visit-date visit) "~4")
                 (visit-snapshot-url visit))))))))

(test-equal "lookup-directory"
  '(("one" 123) ("two" 456))
  (with-json-result %directory-entries
    (map (lambda (entry)
           (list (directory-entry-name entry)
                 (directory-entry-length entry)))
         (lookup-directory "123"))))

(test-equal "lookup-origin-revision"
  '("cd86c72084993d9ef26fc9e24b73cea612b8c97b"
    "d173c707ee88e3c89401ad77fafa65fcd9e9f5be")
  (let ()
    ;; Make sure that 'lookup-origin-revision' does the job, and in particular
    ;; that it doesn't stop until it has found an actual revision:
    ;; 'git-checkout visits point to directories instead of revisions.
    ;; See <https://issues.guix.gnu.org/69070>.
    (define visits
      ;; Two visits of differing types: the first visit (type 'git-checkout')
      ;; points to a directory, the second one (type 'git') points to a
      ;; revision.
      "[ {
    \"origin\": \"https://example.org/repo.git\",
    \"visit\": 1,
    \"type\": \"git-checkout\",
    \"date\": \"2020-05-17T21:43:45.422977+00:00\",
    \"status\": \"full\",
    \"metadata\": {},
    \"type\": \"git-checkout\",
    \"origin_visit_url\": \"/visit/42\",
    \"snapshot_url\": \"/snapshot/1\"
  }, {
    \"origin\": \"https://example.org/repo.git\",
    \"visit\": 2,
    \"type\": \"git\",
    \"date\": \"2020-05-17T21:43:49.422977+00:00\",
    \"status\": \"full\",
    \"metadata\": {},
    \"type\": \"git\",
    \"origin_visit_url\": \"/visit/41\",
    \"snapshot_url\": \"/snapshot/2\"
  } ]")
    (define snapshot-for-git-checkout
      "{ \"id\": 42,
         \"branches\": { \"1.3.2\": {
           \"target\": \"e4a4be18fae8d9c6528abff3bc9088feb19a76c7\",
           \"target_type\": \"directory\",
           \"target_url\": \"/directory/e4a4be18fae8d9c6528abff3bc9088feb19a76c7\"
         }}
       }")
    (define snapshot-for-git
      "{ \"id\": 42,
         \"branches\": { \"1.3.2\": {
           \"target\": \"e4a4be18fae8d9c6528abff3bc9088feb19a76c7\",
           \"target_type\": \"revision\",
           \"target_url\": \"/revision/e4a4be18fae8d9c6528abff3bc9088feb19a76c7\"
         }}
       }")
    (define revision
      "{ \"author\": {},
         \"committer\": {},
         \"committer_date\": \"2018-05-17T21:43:49.422977+00:00\",
         \"date\": \"2018-05-17T21:43:49.422977+00:00\",
         \"directory\": \"d173c707ee88e3c89401ad77fafa65fcd9e9f5be\",
         \"directory_url\": \"/directory/d173c707ee88e3c89401ad77fafa65fcd9e9f5be\",
         \"id\": \"cd86c72084993d9ef26fc9e24b73cea612b8c97b\",
         \"merge\": false,
         \"message\": \"Fix.\",
         \"parents\": [],
         \"type\": \"what type?\"
       }")

    (with-http-server `((200 ,%origin)
                        (200 ,visits)
                        (200 ,snapshot-for-git-checkout)
                        (200 ,snapshot-for-git)
                        (200 ,revision))
      (parameterize ((%swh-base-url (%local-url)))
        (let ((revision (lookup-origin-revision "https://example.org/repo.git"
                                                "1.3.2")))
          (list (revision-id revision)
                (revision-directory revision)))))))

(test-equal "lookup-directory-by-nar-hash"
  "swh:1:dir:84a8b34591712c0a90bab0af604188bcd1fe3153"
  (with-json-result %external-id
    (lookup-directory-by-nar-hash
     (nix-base32-string->bytevector
      "0qqygvlpz63phdi2p5p8ncp80dci230qfa3pwds8yfxqqaablmhb")
     'sha256)))

(test-equal "rate limit reached"
  3000000000
  (let ((too-many (build-response
                   #:code 429
                   #:reason-phrase "Too many requests"

                   ;; Pretend we've reached the limit and it'll be reset in
                   ;; June 2065.
                   #:headers '((x-ratelimit-remaining . "0")
                               (x-ratelimit-reset . "3000000000")))))
    (with-http-server `((,too-many "Too bad."))
      (parameterize ((%swh-base-url (%local-url)))
        (catch 'swh-error
          (lambda ()
            (lookup-origin "http://example.org/guix.git"))
          (lambda (key url method response)
            ;; Ensure the reset time was recorded.
            (@@ (guix swh) %general-rate-limit-reset-time)))))))

(test-assert "%allow-request? and request-rate-limit-reached?"
  ;; Here we test two things: that the rate limit set above is in effect and
  ;; that %ALLOW-REQUEST? is called, and that 'request-rate-limit-reached?'
  ;; returns true.
  (let* ((key (gensym "skip-request"))
         (skip-if-limit-reached
          (lambda (url method)
            (or (not (request-rate-limit-reached? url method))
                (throw key #t)))))
    (parameterize ((%allow-request? skip-if-limit-reached))
      (catch key
        (lambda ()
          (lookup-origin "http://example.org/guix.git")
          #f)
        (const #t)))))

(test-end "swh")

;; Local Variables:
;; eval: (put 'with-json-result 'scheme-indent-function 1)
;; eval: (put 'with-http-server 'scheme-indent-function 1)
;; End:

