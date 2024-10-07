;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2016, 2017, 2018, 2019 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu system accounts)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (user-account
            user-account?
            user-account-name
            user-account-password
            user-account-uid
            user-account-group
            user-account-supplementary-groups
            user-account-comment
            user-account-home-directory
            user-account-create-home-directory?
            user-account-shell
            user-account-system?

            user-group
            user-group?
            user-group-name
            user-group-password
            user-group-id
            user-group-system?

            subid-range
            subid-range?
            subid-range-name
            subid-range-start
            subid-range-count
            subid-range-end
            subid-range-has-start?
            subid-range-less

            sexp->user-account
            sexp->user-group
            sexp->subid-range

            default-shell))


;;; Commentary:
;;;
;;; Data structures representing user accounts and user groups.  This is meant
;;; to be used both on the host side and at run time--e.g., in activation
;;; snippets.
;;;
;;; Code:

(define default-shell
  ;; Default shell for user accounts (a string or string-valued gexp).
  (make-parameter "/bin/sh"))

(define-record-type* <user-account>
  user-account make-user-account
  user-account?
  (name           user-account-name)
  (password       user-account-password (default #f))
  (uid            user-account-uid (default #f))
  (group          user-account-group)             ; number | string
  (supplementary-groups user-account-supplementary-groups
                        (default '()))            ; list of strings
  (comment        user-account-comment (default ""))
  (home-directory user-account-home-directory (thunked)
                  (default (default-home-directory this-record)))
  (create-home-directory? user-account-create-home-directory? ;Boolean
                          (default #t))
  (shell          user-account-shell              ; gexp
                  (default (default-shell)))
  (system?        user-account-system?            ; Boolean
                  (default #f)))

(define-record-type* <user-group>
  user-group make-user-group
  user-group?
  (name           user-group-name)
  (password       user-group-password (default #f))
  (id             user-group-id (default #f))
  (system?        user-group-system?              ; Boolean
                  (default #f)))

(define-record-type* <subid-range>
  subid-range make-subid-range
  subid-range?
  (name           subid-range-name)
  (start          subid-range-start (default #f))    ; number
  (count          subid-range-count                  ; number
                  ; from find_new_sub_gids.c and
                  ; find_new_sub_uids.c
                  (default 65536)))

(define (subid-range-end range)
  "Returns the last subid referenced in RANGE."
  (and
   (subid-range-has-start? range)
   (+ (subid-range-start range)
      (subid-range-count range)
      -1)))

(define (subid-range-has-start? range)
  "Returns #t when RANGE's start is a number."
  (number? (subid-range-start range)))

(define (subid-range-less a b)
  "Returns #t when subid range A either starts before, or is more specific
than B.  When it is not possible to determine whether a range is more specific
w.r.t. another range their names are compared alphabetically."
  (define start-a (subid-range-start a))
  (define start-b (subid-range-start b))
  (cond ((and (not start-a) (not start-b))
         (string< (subid-range-name a)
                  (subid-range-name b)))
        ((and start-a start-b)
         (< start-a start-b))
        (else
         (and start-a
              (not start-b)))))

(define (default-home-directory account)
  "Return the default home directory for ACCOUNT."
  (string-append "/home/" (user-account-name account)))

(define (sexp->user-group sexp)
  "Take SEXP, a tuple as returned by 'user-group->gexp', and turn it into a
user-group record."
  (match sexp
    ((name password id system?)
     (user-group (name name)
                 (password password)
                 (id id)
                 (system? system?)))))

(define (sexp->user-account sexp)
  "Take SEXP, a tuple as returned by 'user-account->gexp', and turn it into a
user-account record."
  (match sexp
    ((name uid group supplementary-groups comment home-directory
           create-home-directory? shell password system?)
     (user-account (name name) (uid uid) (group group)
                   (supplementary-groups supplementary-groups)
                   (comment comment)
                   (home-directory home-directory)
                   (create-home-directory? create-home-directory?)
                   (shell shell) (password password)
                   (system? system?)))))

(define (sexp->subid-range sexp)
  "Take SEXP, a tuple as returned by 'subid-range->gexp', and turn it into a
subid-range record."
  (match sexp
    ((name start count)
     (subid-range (name name)
                  (start start)
                  (count count)))))
