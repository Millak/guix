;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Reily Siegel <mail@reilysiegel.com>
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

(define-module (gnu home services guix)
  #:use-module (gnu home services)
  #:use-module (guix channels)
  #:use-module (guix gexp)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:export (home-channels-service-type))

(define (channels-xdg-files channels)
  `(("guix/channels.scm"
     ,(plain-file
       "channels.scm"
       (call-with-output-string
         (lambda (port)
           (pretty-print (cons 'list (map channel->code channels)) port)))))))

(define home-channels-service-type
  (service-type
   (name 'home-channels)
   (default-value %default-channels)
   (compose concatenate)
   (extend append)
   (extensions
    (list (service-extension home-xdg-configuration-files-service-type
                             channels-xdg-files)))
   (description "Manages the per-user Guix channels specification.")))
