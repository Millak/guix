;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
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

(define-module (gnu installer user)
  #:use-module (guix records)
  #:export (<user>
            user
            make-user
            user-name
            user-group
            user-home-directory

            users->configuration))

(define-record-type* <user>
  user make-user
  user?
  (name            user-name)
  (group           user-group
                   (default "users"))
  (home-directory  user-home-directory))

(define (users->configuration users)
  "Return the configuration field for USERS."
  `((users (cons*
             ,@(map (lambda (user)
                      `(user-account
                        (name ,(user-name user))
                        (group ,(user-group user))
                        (home-directory ,(user-home-directory user))
                        (supplementary-groups
                         (quote ("wheel" "netdev"
                                 "audio" "video")))))
                    users)
             %base-user-accounts))))
