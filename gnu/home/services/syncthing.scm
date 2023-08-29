;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu home services syncthing)
  #:use-module (gnu services)
  #:use-module (gnu home services)
  #:use-module (gnu services syncthing)
  ;; For the 'home-shepherd-service-type' mapping.
  #:use-module (gnu home services shepherd)
  #:export (home-syncthing-service-type)
  #:re-export (syncthing-configuration
               syncthing-configuration?))

(define home-syncthing-service-type
  (service-type
   (inherit (system->home-service-type syncthing-service-type))
   (default-value (for-home (syncthing-configuration)))))
