;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2024-2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu home services shepherd)
  #:use-module (gnu home services)
  #:use-module (gnu packages admin)
  #:use-module (gnu services shepherd)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:export (home-shepherd-service-type

            home-shepherd-configuration
            home-shepherd-configuration?
            home-shepherd-configuration-shepherd
            home-shepherd-configuration-auto-start?
            home-shepherd-configuration-daemonize?
            home-shepherd-configuration-silent?
            home-shepherd-configuration-services

            home-shepherd-transient-service-type
            home-shepherd-timer-service-type)
  #:re-export (shepherd-service
               shepherd-service?
               shepherd-service-documentation
               shepherd-service-provision
               shepherd-service-canonical-name
               shepherd-service-requirement
               shepherd-service-one-shot?
               shepherd-service-respawn?
               shepherd-service-start
               shepherd-service-stop
               shepherd-service-auto-start?
               shepherd-service-modules

               shepherd-action
               shepherd-configuration-action
               shepherd-trigger-action
               shepherd-timer))

(define-record-type* <home-shepherd-configuration>
  home-shepherd-configuration make-home-shepherd-configuration
  home-shepherd-configuration?
  (shepherd home-shepherd-configuration-shepherd
            (default shepherd-for-home))               ;package
  (auto-start? home-shepherd-configuration-auto-start?
               (default #t))
  (daemonize? home-shepherd-configuration-daemonize?
              (default #t))
  (silent? home-shepherd-configuration-silent?
            (default #t))
  (services home-shepherd-configuration-services
            (default '())))

(define (home-shepherd-configuration-file config)
  "Return the shepherd configuration file for SERVICES.  SHEPHERD is used
as shepherd package."
  (let* ((daemonize? (home-shepherd-configuration-daemonize? config))
         (services (home-shepherd-configuration-services config))
         (_ (assert-valid-graph services))
         (files (map shepherd-service-file services))
         ;; TODO: Add compilation of services, it can improve start
         ;; time.
         ;; (scm->go (cute scm->go <> shepherd))
         )
    (define config
      #~(begin
          (use-modules (srfi srfi-34)
                       (system repl error-handling))

          (define (make-user-module)
            ;; Copied from (shepherd support), where it's private.
            (let ((m (make-fresh-user-module)))
              (module-use! m (resolve-interface '(shepherd service)))
              m))

          (register-services
           (map (lambda (file)
                  (save-module-excursion
                   (lambda ()
                     (set-current-module (make-user-module))
                     (load file))))
                '#$files))

          #$(and daemonize?
                 #~(perform-service-action root-service 'daemonize))

          (format #t "Starting services...~%")
          (let ((services-to-start
                 '#$(append-map shepherd-service-provision
                                (filter shepherd-service-auto-start?
                                        services))))
            (start-in-the-background services-to-start)

            (redirect-port (open-input-file "/dev/null")
                           (current-input-port)))))

    (scheme-file "shepherd.conf" config)))

(define (launch-shepherd-gexp config)
  (let* ((shepherd (home-shepherd-configuration-shepherd config))
         (silent? (home-shepherd-configuration-silent? config)))
    (if (home-shepherd-configuration-auto-start? config)
        (with-imported-modules '((guix build utils))
          #~(unless (file-exists?
                     (string-append
                      (or (getenv "XDG_RUNTIME_DIR")
                          (format #f "/run/user/~a" (getuid)))
                      "/shepherd/socket"))
              (system*
               #$(file-append shepherd "/bin/shepherd")
               #$@(if silent? '("--silent") '())
               "--config"
               #$(home-shepherd-configuration-file config))))
        #~"")))

(define (reload-configuration-gexp config)
  (let* ((shepherd (home-shepherd-configuration-shepherd config)))
    #~(system*
       #$(file-append shepherd "/bin/herd")
       "load" "root"
       #$(home-shepherd-configuration-file config))))

(define (ensure-shepherd-gexp config)
  #~(if (file-exists?
         (string-append
          (or (getenv "XDG_RUNTIME_DIR")
              (format #f "/run/user/~a" (getuid)))
          "/shepherd/socket"))
        #$(reload-configuration-gexp config)
        ;; Don't attempt to start user shepherd if the system is running the
        ;; activation script. /run/user/<uid> may not have been created
        ;; yet. But do otherwise so if the runtime dir does not exist an error
        ;; is logged.
        (unless (getenv "GUIX_SYSTEM_IS_RUNNING_HOME_ACTIVATE")
          #$(launch-shepherd-gexp config))))

(define (shepherd-xdg-configuration-files config)
  `(("shepherd/init.scm" ,(home-shepherd-configuration-file config))))

(define home-shepherd-service-type
  (service-type (name 'home-shepherd)
                (extensions
                 (list (service-extension
                        home-run-on-first-login-service-type
                        launch-shepherd-gexp)
                       (service-extension
                        home-xdg-configuration-files-service-type
                        shepherd-xdg-configuration-files)
                       (service-extension
                        home-activation-service-type
                        ensure-shepherd-gexp)
                       (service-extension
                        home-profile-service-type
                        (lambda (config)
                          `(,(home-shepherd-configuration-shepherd config))))))
                (compose concatenate)
                (extend
                 (lambda (config extra-services)
                   (home-shepherd-configuration
                    (inherit config)
                    (services
                     (append (home-shepherd-configuration-services config)
                             extra-services)))))
                (default-value (home-shepherd-configuration))
                (description "Configure and install userland Shepherd.")))

(define-service-type-mapping
  shepherd-root-service-type => home-shepherd-service-type)


;;;
;;; Timer and transient service maker.
;;;

(define home-shepherd-timer-service-type
  (service-type
   (inherit (system->home-service-type shepherd-timer-service-type))
   (default-value '())))                          ;requirement

(define home-shepherd-transient-service-type
  (service-type
   (inherit (system->home-service-type shepherd-transient-service-type))
   (default-value '())))                          ;requirement
