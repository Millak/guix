;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Oleg Pykhalov <go.wigust@gmail.com>
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

(define-module (gnu home services niri)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages wm)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (niri-configuration
            home-niri-service-type))

(define-record-type* <niri-configuration>
  niri-configuration make-niri-configuration
  niri-configuration?
  (environment-variables niri-configuration-environment-variables ;list of strings
                         (default '())))

(define (home-niri-shepherd-service config)
  "Return a shepherd service that runs Niri, a scrollable tiling Wayland
compositor.  The service starts Niri in a DBus session with appropriate
environment variables set for a Wayland desktop session."
  (list (shepherd-service
          (documentation "Run Niri scrollable tiling Wayland compositor.")
          (provision '(niri))
          (start #~(make-forkexec-constructor
                    (list #$(file-append bash "/bin/bash") "-l"
                          "-c" "exec dbus-run-session niri --session")
                    #:environment-variables
                    (append (list #$@(niri-configuration-environment-variables config))
                            '("DESKTOP_SESSION=niri"
                              "XDG_CURRENT_DESKTOP=niri"
                              "XDG_SESSION_DESKTOP=niri"
                              "XDG_SESSION_TYPE=wayland")
                            (filter (negate
                                     (lambda (str)
                                       (string-prefix? "WAYLAND_DISPLAY=" str)))
                                    (environ)))))
          (stop #~(make-kill-destructor)))))

(define home-niri-service-type
  (service-type
   (name 'home-niri)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-niri-shepherd-service)
          (service-extension home-profile-service-type
                             (lambda (config)
                               (list dbus
                                     niri
                                     xdg-desktop-portal
                                     xdg-desktop-portal-gnome
                                     xdg-desktop-portal-gtk)))))
   (description "Install and configure Niri, a scrollable tiling Wayland
compositor.  This service starts Niri as a user-level desktop session with
proper environment variables set for Wayland compatibility.  It ensures Niri
and its dependencies are available in the user's profile.")
   (default-value (niri-configuration))))
