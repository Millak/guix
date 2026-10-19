;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Igorj Gorjaĉev <igor@goryachev.org>
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

;;;; Commentary:
;;;
;;; This module implements a home service that to run River, a non-monolithic
;;; Wayland compositor, as a user-level desktop session with proper environment
;;; variables set for Wayland compatibility.  It ensures River and its
;;; dependencies are available in the user's profile.  The module is loosely
;;; based on niri.scm.
;;;
;;;; Code:

(define-module (gnu home services river)
  #:use-module (gnu home services desktop)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages window-management)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zig-xyz)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:export (river-configuration
            home-river-service-type))

(define-record-type* <river-configuration>
  river-configuration make-river-configuration
  river-configuration?
  (init-file river-configuration-init-file
             (default
               (program-file "river-init"
                             #~(system*
                                (string-append #$tinyrwm "/bin/tinyrwm")))))
  (extra-packages river-configuration-extra-packages
                  (default '()))
  (environment-variables river-configuration-environment-variables
                         (default '())))

(define (home-river-shepherd-service config)
  "Return a shepherd service that runs River, a non-monolithic Wayland
compositor.  The service starts River with appropriate environment
variables set for a Wayland desktop session."
  (list (shepherd-service
          (documentation "Run River non-monolithic Wayland compositor.")
          (provision '(river))
          (requirement '(dbus))
          (start #~(make-forkexec-constructor
                    (list #$(file-append bash "/bin/bash") "-l"
                          "-c" "exec river")
                    #:environment-variables
                    (append
                     (list #$@(river-configuration-environment-variables
                               config))
                     '("DESKTOP_SESSION=river"
                       "XDG_CURRENT_DESKTOP=river"
                       "XDG_SESSION_DESKTOP=river"
                       "XDG_SESSION_TYPE=wayland")
                     (filter (negate
                              (lambda (str)
                                (string-prefix? "WAYLAND_DISPLAY=" str)))
                             (default-environment-variables)))))
          (stop #~(make-kill-destructor)))))

(define home-river-service-type
  (service-type
   (name 'home-river)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-river-shepherd-service)
          (service-extension home-dbus-service-type
                             (const '()))
          (service-extension home-xdg-configuration-files-service-type
                             (lambda (config)
                               `(("river/init"
                                  ,(river-configuration-init-file config)))))
          (service-extension home-profile-service-type
                             (lambda (config)
                               (append
                                (list dbus
                                      river
                                      xdg-desktop-portal
                                      xdg-desktop-portal-gnome
                                      xdg-desktop-portal-gtk
                                      xwayland-satellite)
                                (river-configuration-extra-packages
                                 config))))))
   (description "Install and configure River, a non-monolithic Wayland
compositor.  This service starts River as a user-level desktop session
with proper environment variables set for Wayland compatibility.  It
ensures River and its dependencies are available in the user's profile.")
   (default-value (river-configuration))))
