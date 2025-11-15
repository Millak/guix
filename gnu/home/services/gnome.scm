;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Giacomo Leidi <therewasa@fishinthecalculator.me>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.


(define-module (gnu home services gnome)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages gnome)
  #:use-module (gnu system accounts)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:export (gcr-ssh-agent-configuration
            gcr-ssh-agent-configuration?
            gcr-ssh-agent-configuration-fields
            gcr-ssh-agent-configuration-package
            gcr-ssh-agent-configuration-log-file

            home-gcr-ssh-agent-log-file
            home-gcr-ssh-agent-shepherd-service

            home-gcr-ssh-agent-service-type))

(define-maybe/no-serialization string)

(define-configuration/no-serialization gcr-ssh-agent-configuration
  (package
   (package gcr)
   "The @code{gcr} package to use.")
  (log-file
   (maybe-string)
   "Where the service will write its logs.  If unset, it defaults to
@file{$HOME/.local/state/shepherd/gcr-ssh-agent.log}."))

(define (home-gcr-ssh-agent-log-file config)
  (define maybe-log-file (gcr-ssh-agent-configuration-log-file config))
  (if (maybe-value-set? maybe-log-file)
      maybe-log-file
      #~(string-append %user-log-dir "/gcr-ssh-agent.log")))

(define (home-gcr-ssh-agent-shepherd-service config)
  (let ((package
          (gcr-ssh-agent-configuration-package config))
        (log-file (home-gcr-ssh-agent-log-file config)))
    (list
     (shepherd-service
       (provision '(gcr-ssh-agent ssh-agent))
       (modules
        ;;for '%user-log-dir' and '%user-runtime-dir'
        '((shepherd support)))
       (start
        #~(let* ((socket-directory
                  (string-append %user-runtime-dir
                                 "/gcr"))
                 (socket-endpoint
                  (endpoint
                   (make-socket-address
                    AF_UNIX
                    (string-append socket-directory "/ssh"))
                   #:name "ssh"
                   #:socket-directory-permissions #o700)))
            (make-systemd-constructor
             (list #$(file-append package
                                  "/libexec/gcr-ssh-agent")
                   "-d" socket-directory)
             (list socket-endpoint)
             ;; With #:lazy-start #t the first ssh connection
             ;; hangs indefinitely.
             #:lazy-start? #f
             #:log-file #$log-file)))
       (stop #~(make-systemd-destructor))))))

(define (home-gcr-ssh-agent-environment-variables config)
  `(("SSH_AUTH_SOCK" . "${XDG_RUNTIME_DIR}/gcr/ssh")))

(define home-gcr-ssh-agent-service-type
  (service-type
   (name 'home-gcr-ssh-agent)
   (extensions
    (list (service-extension
           home-shepherd-service-type
           home-gcr-ssh-agent-shepherd-service)
          (service-extension home-environment-variables-service-type
                             home-gcr-ssh-agent-environment-variables)))
   (default-value (gcr-ssh-agent-configuration))
   (description
    "Provides @code{gcr-ssh-agent} Shepherd service and installs
@code{gcr} in the system profile.")))
