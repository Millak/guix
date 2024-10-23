;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Wilko Meyer <w@wmeyer.eu>
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

(define-module (gnu services dav)
  #:use-module (gnu packages dav)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (xandikos-configuration
            xandikos-configuration?
            xandikos-service-type))

;;;
;;; Xandikos.
;;;

(define (port? x)
  (and (number? x)
       (and (>= x 0) (<= x 65535))))

(define-configuration/no-serialization xandikos-configuration
  (package
    (file-like xandikos)
    "Xandikos package to use.")
  (directory
   (string "/var/xandikos/dav")
   "Directory to serve from.")
  (listen-address
   (string "127.0.0.1")
   "The address Xandikos listens on.")
  (port
   (port 8080)
   "The port to run Xandikos on.")
  (current-user-principal
   (string "/user/")
   "Path to current user principal.")
  (route-prefix
   (string "/")
   "Path to Xandikos. (Useful when Xandikos is behind a reverse proxy.)")
  (defaults?
    (boolean #t)
    "Create initial calendar and address book.")
  (dump?
   (boolean #f)
   "Print DAV XML request/responses.")
  (avahi?
   (boolean #f)
   "Announce services with avahi.")
  (autocreate?
   (boolean #f)
   "Automatically create necessary directories.")
  (no-strict?
   (boolean #f)
   "Enable workarounds for buggy CalDAV/CardDAV client implementations."))

(define (xandikos-shepherd-service config)
  (match-record config <xandikos-configuration>
                (package directory listen-address port current-user-principal
                         route-prefix defaults? dump? avahi? autocreate?
                         no-strict?)
    (list
     (shepherd-service
       (provision '(xandikos))
       (documentation "Caldav/CardDAV server")
       (requirement '(networking user-processes))
       (start #~(make-forkexec-constructor
                 (list #$(file-append xandikos "/bin/xandikos")
                       "--listen-address" #$listen-address
                       "--port" #$(number->string port)
                       "-d" #$directory
                       "--route-prefix" #$route-prefix
                       "--current-user-principal" #$current-user-principal
                       #$@(if dump? '("--dump-dav-xml") '())
                       #$@(if avahi? '("--avahi") '())
                       #$@(if autocreate? '("--autocreate") '())
                       #$@(if defaults? '("--defaults") '())
                       #$@(if no-strict? '("--no-strict") '()))))
       (stop #~(make-kill-destructor))
       (respawn? #t)))))

(define xandikos-service-type
  (service-type
    (name 'xandikos)
    (extensions
     (list (service-extension shepherd-root-service-type
                              xandikos-shepherd-service)))
    (default-value (xandikos-configuration))
    (description "Service to run the @code{Xandikos} CalDAV/CardDAV server.")))
