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

(define-module (gnu home services containers)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services containers)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (srfi srfi-1)
  #:export (home-oci-service-type))

(define home-oci-service-type
  (service-type
   (inherit (system->home-service-type oci-service-type))
   (extensions
    (list
     (service-extension home-profile-service-type
                        (lambda (config)
                          (let ((runtime-cli
                                 (oci-configuration-runtime-cli config))
                                (runtime
                                 (oci-configuration-runtime config)))
                            (oci-service-profile runtime runtime-cli))))
     (service-extension home-shepherd-service-type
                        oci-configuration->shepherd-services)))
   (extend
    (lambda (config extension)
      (for-home
       (oci-configuration
        (inherit (oci-configuration-extend config extension))))))
   (default-value (for-home (oci-configuration)))))
