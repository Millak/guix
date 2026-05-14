;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu home services jupyter)
  #:use-module (gnu home services)
  #:use-module (gnu services)
  #:use-module (gnu services jupyter)
  #:use-module (gnu services shepherd)
  ;; For the 'home-shepherd-service-type' mapping.
  #:use-module (gnu home services shepherd)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:export (home-jupyter-service-type)
  #:re-export (jupyter-configuration))

(define home-jupyter-service-type
  (let* ((type jupyter-service-type)
         ;; XXX: Find a better way for this #:log-file replacement.
         (log-file #~(string-append %user-log-dir "/jupyter.log"))
         (shepherd-extension
          (service-extension shepherd-root-service-type
                             (lambda (config)
                               (jupyter-shepherd-service config log-file))))
         (base (service-type
                 (inherit type)
                 (extensions
                  (cons* shepherd-extension
                         (remove (lambda (extension)
                                   (memq (service-extension-target extension)
                                         (list shepherd-root-service-type)))
                                 (service-type-extensions type)))))))
    (service-type
      (inherit (system->home-service-type base))
      (default-value (for-home (jupyter-configuration))))))
