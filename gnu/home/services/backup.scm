;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <goodoldpaul@autistici.org>
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

(define-module (gnu home services backup)
  #:use-module (gnu services)
  #:use-module (gnu services backup)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:export (home-restic-backup-service-type)
  #:re-export (restic-backup-configuration
               restic-backup-job))

(define home-restic-backup-service-type
  (service-type
   (inherit (system->home-service-type restic-backup-service-type))
   (extend
    (lambda (config jobs)
      (for-home
       (restic-backup-configuration
        (inherit config)
        (jobs (append (restic-backup-configuration-jobs config)
                      jobs))))))
   (default-value (for-home (restic-backup-configuration)))))
