;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023, 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu home services messaging)
  #:use-module (srfi srfi-26)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages messaging)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services messaging)
  #:use-module ((gnu system shadow) #:select (account-service-type))
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:export (home-snuik-service-type
            home-znc-configuration
            home-znc-service-type))

;;;
;;; Znc.
;;;
(define-record-type* <home-znc-configuration>
  home-znc-configuration make-home-znc-configuration
  home-znc-configuration?
  (znc           home-znc-znc           ;string
                 (default znc))
  (extra-options home-znc-extra-options ;list of string
                 (default '())))

(define (home-znc-services config)
  "Return a <shepherd-service> for znc with CONFIG."
  (match-record config <home-znc-configuration>
    (znc extra-options)
    (let* ((znc (file-append znc "/bin/znc"))
           (command #~'(#$znc "--foreground" #$@extra-options))
           (log-file #~(string-append %user-log-dir "/znc.log")))
      (list (shepherd-service
             (documentation "Run the znc IRC bouncer.")
             (provision '(znc))
             (modules '((shepherd support)))      ;for '%user-log-dir'
             (start #~(make-forkexec-constructor #$command
                                                 #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define home-znc-service-type
  (service-type
   (name 'home-znc)
   (default-value (home-znc-configuration))
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-znc-services)))
   (description
    "Install and configure @command{znc}, an @acronym{IRC, Internet Relay
Chat} bouncer, as a Shepherd service.")))


;;;
;;; Snuik.
;;;
(define home-snuik-service-type
  (service-type
   (inherit (system->home-service-type
             (remove-service-extensions snuik-service-type
                                        (list account-service-type
                                              activation-service-type))))
   (default-value (for-home (snuik-configuration)))))
