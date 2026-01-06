;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Giacomo Leidi <therewasa@fishinthecalculator.me>
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

(define-module (gnu services firmware)
  #:use-module (gnu packages firmware)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:export (fwupd-configuration
            fwupd-configuration?
            fwupd-configuration-fields
            fwupd-configuration-fwupd

            fwupd-service-type))

(define-configuration/no-serialization fwupd-configuration
  (fwupd
   (package fwupd)
   "The fwupd package that will be installed in the system profile."))

(define fwupd-service-type
  (service-type
    (name 'fwupd)
    (extensions
     (list
      (service-extension profile-service-type
                         (compose list fwupd-configuration-fwupd))
      (service-extension dbus-root-service-type
                         (compose list fwupd-configuration-fwupd))
      (service-extension udev-service-type
                         (compose list fwupd-configuration-fwupd))
      (service-extension polkit-service-type
                         (compose list fwupd-configuration-fwupd))))
    (default-value (fwupd-configuration))
    (description
     "This service runs the fwupd daemon on the Guix System.")))
