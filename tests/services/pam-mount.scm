;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Brian Cully <bjc@spork.org>
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

(define-module (tests services pam-mount)
  #:use-module (gnu services pam-mount)
  #:use-module (gnu system pam)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix grafts)
  #:use-module (guix store)
  #:use-module (guix tests)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-64))

(define pam-mount-volume-fields (@@ (gnu services pam-mount)
                                    pam-mount-volume-fields))
(define field-name->tag (@@ (gnu services pam-mount)
                            field-name->tag))

(define pam-mount-volume->sxml (@@ (gnu services pam-mount)
                                   pam-mount-volume->sxml))

(test-begin "services-pam-mount")

(test-group "field-name->tag"
  (let ((field-map '((user-name user)
                     (user-id uid)
                     (primary-group pgrp)
                     (group-id gid)
                     (secondary-group sgrp)
                     (file-system-type fstype)
                     (no-mount-as-root? noroot)
                     (server server)
                     (file-name path)
                     (mount-point mountpoint)
                     (options options)
                     (ssh? ssh)
                     (cipher cipher)
                     (file-system-key-cipher fskeycipher)
                     (file-system-key-hash fskeyhash)
                     (file-system-key-file-name fskeypath))))

    (test-equal "all fields accounted for"
      (map car field-map)
      (map configuration-field-name pam-mount-volume-fields))

    (for-each (match-lambda
                ((field-name tag-name)
                 (test-eq (format #f "~a -> ~a" field-name tag-name)
                   (field-name->tag field-name) tag-name)))
              field-map)))

(let ((tmpfs-volume (pam-mount-volume
                     (secondary-group "users")
                     (file-system-type "tmpfs")
                     (mount-point "/run/user/%(USERUID)")
                     (options "someoptions"))))
  (test-equal "tmpfs"
    '(volume (@ (sgrp "users")
                (fstype "tmpfs")
                (mountpoint "/run/user/%(USERUID)")
                (options "someoptions")))
    (pam-mount-volume->sxml tmpfs-volume)))

(test-end "services-pam-mount")
