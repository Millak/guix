;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2024, 2025 Ian Eure <ian@retrospec.tv>
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
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages kodi)
  #:use-module (gnu packages video)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-kodi-configuration
            home-kodi-service-type

            home-jellyfin-mpv-shim-configuration
            home-jellyfin-mpv-shim-service-type))


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

(define-configuration home-jellyfin-mpv-shim-configuration
  (package
    (file-like jellyfin-mpv-shim)
    "The Jellyfin MPV Shim package to use"))

(define (jellyfin-mpv-shim-shepherd-service config)
  (list (shepherd-service
         (documentation "Jellyfin MPV Shim.")
         (provision '(jellyfin-mpv-shim jellyfin-client))

         ;; Depend on 'x11-display', which sets 'DISPLAY' if an X11 server is
         ;; available, and fails to start otherwise.
         (requirement '(x11-display))

         (modules '((srfi srfi-1)
                    (srfi srfi-26)
                    (srfi srfi-98)))
         (start
          #~(make-forkexec-constructor
             (list
                #$(file-append
                   (home-jellyfin-mpv-shim-configuration-package config)
                   "/bin/jellyfin-mpv-shim"))))
         (stop #~(make-kill-destructor)))))

(define-public home-jellyfin-mpv-shim-service-type
  (service-type
   (name 'home-jellyfin-mpv-shim)
   (extensions (list (service-extension home-shepherd-service-type
                                        jellyfin-mpv-shim-shepherd-service)
                     ;; Ensure 'home-x11-service-type' is instantiated so we
                     ;; can depend on the Shepherd 'x11-display' service.
                     (service-extension home-x11-service-type
                                        (const #t))))
   (default-value (home-jellyfin-mpv-shim-configuration))
   (description "Run Jellyfin MPV Shim.")))
