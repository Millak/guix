;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
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

(define-module (gnu system privilege)
  #:use-module (guix records)
  #:export (privileged-program
            privileged-program?
            privileged-program-program
            privileged-program-setuid?
            privileged-program-setgid?
            privileged-program-user
            privileged-program-group
            privileged-program-capabilities))

;;; Commentary:
;;;
;;; Data structures representing privileged programs: binaries with additional
;;; permissions such as setuid/setgid, or POSIX capabilities.  This is meant to
;;; be used both on the host side and at run time--e.g., in activation snippets.
;;;
;;; Code:

(define-record-type* <privileged-program>
  privileged-program make-privileged-program
  privileged-program?
  ;; File name of the program to assign elevated privileges.
  (program       privileged-program-program) ;file-like
  ;; Whether to set the setuid (‘set user ID’) bit.
  (setuid?       privileged-program-setuid? ;boolean
                 (default #f))
  ;; Whether to set the setgid (‘set group ID’) bit.
  (setgid?       privileged-program-setgid? ;boolean
                 (default #f))
  ;; The user name or ID this should be set to (defaults to root's).
  (user          privileged-program-user ;integer or string
                 (default 0))
  ;; The group name or ID we want to set this to (defaults to root's).
  (group         privileged-program-group ;integer or string
                 (default 0))
  ;; POSIX capabilities in cap_from_text(3) form (defaults to #f: none).
  (capabilities  privileged-program-capabilities ;string or #f
                 (default #f)))
