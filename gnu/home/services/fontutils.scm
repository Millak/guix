;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2023 Andrew Patterson <andrewpatt7@gmail.com>
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

(define-module (gnu home services fontutils)
  #:use-module (gnu home services)
  #:use-module (gnu packages fontutils)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (sxml simple)

  #:export (home-fontconfig-service-type))

;;; Commentary:
;;;
;;; Services related to fonts.  home-fontconfig service provides
;;; fontconfig configuration, which allows fc-* utilities to find
;;; fonts in Guix Home's profile and regenerates font cache on
;;; activation.
;;;
;;; Code:

(define (write-fontconfig-doctype)
  "Prints fontconfig's DOCTYPE to current-output-port."
  ;; This is necessary because SXML doesn't seem to have a way to represent a doctype,
  ;; but sxml->xml /does/ currently call any thunks in the SXML with the XML output port
  ;; as current-output-port, allowing the output to include arbitrary text instead of
  ;; just properly quoted XML.
  (format #t "<!DOCTYPE fontconfig SYSTEM 'fonts.dtd'>"))

(define (config->sxml config)
  "Converts a <home-fontconfig-configuration> record into the SXML representation
of fontconfig's fonts.conf file."
  (define (snippets->sxml snippet)
    (match snippet
      ((or (? string? dir)
           (? gexp? dir))
       `(dir ,dir))
      ((? list?)
       snippet)))
  `(*TOP* (*PI* xml "version='1.0'")
          ,write-fontconfig-doctype
          (fontconfig
           ,@(map snippets->sxml config))))

(define (add-fontconfig-config-file config)
  `(("fontconfig/fonts.conf"
     ,(mixed-text-file
       "fonts.conf"
       (call-with-output-string
         (lambda (port)
           (sxml->xml (config->sxml config) port)))))))

(define (regenerate-font-cache-gexp _)
  `(("profile/share/fonts"
     ,#~(system* #$(file-append fontconfig "/bin/fc-cache") "-fv"))))

(define home-fontconfig-service-type
  (service-type (name 'home-fontconfig)
                (extensions
                 (list (service-extension
                        home-xdg-configuration-files-service-type
                        add-fontconfig-config-file)
                       (service-extension
                        home-run-on-change-service-type
                        regenerate-font-cache-gexp)))
                (compose concatenate)
                (extend append)
                (default-value '("~/.guix-home/profile/share/fonts"))
                (description
                 "Provides configuration file for fontconfig and make
fc-* utilities aware of font packages installed in Guix Home's profile.")))
