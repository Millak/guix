;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2023 Justin Veilleux <terramorpha@cock.li>
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

(define-module (gnu services syncthing)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (syncthing-configuration
            syncthing-configuration?
            syncthing-service-type))

;;; Commentary:
;;;
;;; This module provides a service definition for the syncthing service.
;;;
;;; Code:

(define-record-type* <syncthing-configuration>
  syncthing-configuration make-syncthing-configuration
  syncthing-configuration?
  (syncthing syncthing-configuration-syncthing ;file-like
             (default syncthing))
  (arguments syncthing-configuration-arguments ;list of strings
             (default '()))
  (logflags  syncthing-configuration-logflags  ;number
             (default 0))
  (user      syncthing-configuration-user      ;string
             (default #f))
  (group     syncthing-configuration-group     ;string
             (default "users"))
  (home      syncthing-configuration-home      ;string
             (default #f))
  (home-service? syncthing-configuration-home-service?
                 (default for-home?) (innate)))

(define syncthing-shepherd-service
  (match-record-lambda <syncthing-configuration>
      (syncthing arguments logflags user group home home-service?)
    (list
     (shepherd-service
      (provision (if home-service?
                     '(syncthing)
                     (list (string->symbol
                            (string-append "syncthing-" user)))))
      (documentation "Run syncthing.")
      (requirement (if home-service? '() '(loopback user-processes)))
      (start #~(make-forkexec-constructor
                (append (list (string-append #$syncthing "/bin/syncthing")
                              "--no-browser"
                              "--no-restart"
                              (string-append "--logflags=" (number->string #$logflags)))
                        '#$arguments)
                #:user #$(and (not home-service?) user)
                #:group #$(and (not home-service?) group)
                #:environment-variables
                (append (list (string-append "HOME=" (or #$home (passwd:dir (getpw #$user))))
                              "SSL_CERT_DIR=/etc/ssl/certs"
                              "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                        (filter (negate       ;XXX: 'remove' is not in (guile)
                                 (lambda (str)
                                   (or (string-prefix? "HOME=" str)
                                       (string-prefix? "SSL_CERT_DIR=" str)
                                       (string-prefix? "SSL_CERT_FILE=" str))))
                                (environ)))))
      (respawn? #f)
      (stop #~(make-kill-destructor))))))

(define syncthing-service-type
  (service-type (name 'syncthing)
                (extensions (list (service-extension shepherd-root-service-type
                                                     syncthing-shepherd-service)))
                (description
                 "Run @uref{https://github.com/syncthing/syncthing, Syncthing}
decentralized continuous file system synchronization.")))

;;; syncthing.scm ends here
