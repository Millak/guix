;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu home services media)
  #:use-module (srfi srfi-26)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages kodi)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-kodi-configuration
            home-kodi-service-type))


;;;
;;; Kodi.
;;;

(define-record-type* <home-kodi-configuration>
  home-kodi-configuration make-home-kodi-configuration
  home-kodi-configuration?
  (kodi          home-kodi-kodi          ;file-like
                 (default kodi))
  (extra-options home-kodi-extra-options ;list of string
                 (default '())))

(define (home-kodi-services config)
  "Return a <shepherd-service> for kodi with CONFIG."
  (match-record config <home-kodi-configuration>
    (kodi extra-options)
    (let* ((kodi (file-append kodi "/bin/kodi"))
           (command #~'(#$kodi "-fs" #$@extra-options))
           (log-file #~(string-append %user-log-dir "/kodi.log")))
      (list (shepherd-service
             (documentation "Run the kodi media center.")
             (provision '(kodi))
             (modules '((shepherd support)))      ;for '%user-log-dir'
             (start #~(make-forkexec-constructor #$command
                                                 #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define home-kodi-service-type
  (service-type
   (name 'home-kodi)
   (default-value (home-kodi-configuration))
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-kodi-services)))
   (description
    "Install and configure the Kodi media center so that it runs as a Shepherd
service.")))
