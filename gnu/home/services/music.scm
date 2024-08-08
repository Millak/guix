;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Fabio Natali <me@fabionatali.com>
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

(define-module (gnu home services music)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (gnu home services)
  #:use-module (gnu packages music)
  #:use-module (gnu packages video)
  #:export (home-beets-configuration
            home-beets-service-type))

;;; Commentary:
;;;
;;; A Guix Home service to configure Beets, a music file and metadata manager.
;;;
;;; Code:

(define-record-type* <home-beets-configuration>
  home-beets-configuration make-home-beets-configuration
  home-beets-configuration?
  (package home-beets-package (default beets))
  (directory home-beets-directory (default #f))
  (extra-options home-beets-extra-options (default '()))
  (extra-packages home-beets-extra-packages (default (list ffmpeg))))

(define (home-beets-configuration->file config)
  "Return the Beets configuration file corresponding to CONFIG."
  (match-record config <home-beets-configuration>
    (directory extra-options)
    (plain-file "beets.yaml"
                (string-append "directory: " directory "\n"
                               (string-join extra-options "\n" 'suffix)))))

(define home-beets-service-type
  (service-type
   (name 'home-beets)
   (extensions
    (list
     (service-extension home-profile-service-type
                        (lambda (config)
                          (cons* (home-beets-package config)
                                 (home-beets-extra-packages config))))
     (service-extension home-xdg-configuration-files-service-type
                        (lambda (config)
                          (list `("beets/config.yaml"
                                  ,(home-beets-configuration->file config)))))))
   (description "Configure Beets, a music file and metadata manager.")))
