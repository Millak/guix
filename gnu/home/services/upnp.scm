;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Sughosha <sughosha@disroot.org>
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

(define-module (gnu home services upnp)
  #:use-module ((gnu build linux-container) #:select (%namespaces))
  #:use-module (gnu services)
  #:use-module (gnu home services)
  ;; For the 'home-shepherd-service-type' mapping.
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services upnp)
  #:use-module (guix records)
  #:export (home-readymedia-service-type)
  #:re-export (readymedia-configuration
               readymedia-configuration?
               readymedia-configuration-readymedia
               readymedia-configuration-port
               readymedia-configuration-cache-directory
               readymedia-configuration-extra-config
               readymedia-configuration-friendly-name
               readymedia-configuration-log-directory
               readymedia-configuration-media-directories
               readymedia-media-directory
               readymedia-media-directory?
               readymedia-media-directory-path
               readymedia-media-directory-types))

(define home-readymedia-service-type
  (service-type
   (inherit (system->home-service-type readymedia-service-type))
   ;; system->home-service-type does not convert special-files-service-type to
   ;; home-files-service-type, so redefine extensios
   (extensions
    (list (service-extension home-shepherd-service-type
                             (compose list readymedia-shepherd-service))
          (service-extension home-activation-service-type
                             readymedia-activation)))
   (default-value
    (for-home
     (readymedia-configuration
       (media-directories '()))))))
