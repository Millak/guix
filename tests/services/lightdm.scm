;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (tests services lightdm)
  #:use-module (guix diagnostics)
  #:use-module (gnu services lightdm)
  #:use-module (srfi srfi-64))

;;; Tests for the (gnu services lightdm) module.

;;; Access some internals for whitebox testing.
(define validate-lightdm-configuration (@@ (gnu services lightdm)
                                           validate-lightdm-configuration))

(test-begin "lightdm-service")

(test-equal "error on missing greeter"
  'ok
  (catch 'quit
    (lambda ()
      (validate-lightdm-configuration (lightdm-configuration (greeters '()))))
    (lambda _
      'ok)))

(test-equal "error when a greeter has multiple configurations"
  'ok
  (catch 'quit
    (lambda ()
      (lightdm-configuration
       (greeters (list (lightdm-gtk-greeter-configuration
                        (theme-name "boring"))
                       (lightdm-gtk-greeter-configuration
                        (theme-name "blue"))))))
    (lambda _
      'ok)))

(test-end "lightdm-service")
