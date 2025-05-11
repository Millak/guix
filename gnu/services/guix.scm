;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020, 2021, 2022 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2024 Andrew Tropin <andrew@trop.in>
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

(define-module (gnu services guix)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module ((gnu packages base)
                #:select (libc-utf8-locales-for-target))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages web)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages package-management)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services getmail)
  #:use-module (gnu system shadow)
  #:export (guix-build-coordinator-configuration
            guix-build-coordinator-configuration?
            guix-build-coordinator-configuration-package
            guix-build-coordinator-configuration-user
            guix-build-coordinator-configuration-group
            guix-build-coordinator-configuration-datastore-uri-string
            guix-build-coordinator-configuration-agent-communication-uri-string
            guix-build-coordinator-configuration-client-communication-uri-string
            guix-build-coordinator-configuration-allocation-strategy
            guix-build-coordinator-configuration-hooks
            guix-build-coordinator-configuration-parallel-hooks
            guix-build-coordinator-configuration-listen-repl
            guix-build-coordinator-configuration-guile
            guix-build-coordinator-configuration-extra-environment-variables
            guix-build-coordinator-configuration-extra-build-coordinator-arguments

            guix-build-coordinator-service-type

            guix-build-coordinator-agent-configuration
            guix-build-coordinator-agent-configuration?
            guix-build-coordinator-agent-configuration-package
            guix-build-coordinator-agent-configuration-user
            guix-build-coordinator-agent-configuration-coordinator
            guix-build-coordinator-agent-configuration-authentication
            guix-build-coordinator-agent-configuration-systems
            guix-build-coordinator-agent-configuration-max-parallel-builds
            guix-build-coordinator-agent-configuration-max-parallel-uploads
            guix-build-coordinator-agent-configuration-max-allocated-builds
            guix-build-coordinator-agent-configuration-max-1min-load-average
            guix-build-coordinator-agent-configuration-derivation-substitute-urls
            guix-build-coordinator-agent-configuration-non-derivation-substitute-urls
            guix-build-coordinator-agent-configuration-extra-options

            guix-build-coordinator-agent-password-auth
            guix-build-coordinator-agent-password-auth?
            guix-build-coordinator-agent-password-auth-uuid
            guix-build-coordinator-agent-password-auth-password

            guix-build-coordinator-agent-password-file-auth
            guix-build-coordinator-agent-password-file-auth?
            guix-build-coordinator-agent-password-file-auth-uuid
            guix-build-coordinator-agent-password-file-auth-password-file

            guix-build-coordinator-agent-dynamic-auth
            guix-build-coordinator-agent-dynamic-auth?
            guix-build-coordinator-agent-dynamic-auth-agent-name
            guix-build-coordinator-agent-dynamic-auth-token

            guix-build-coordinator-agent-dynamic-auth-with-file
            guix-build-coordinator-agent-dynamic-auth-with-file?
            guix-build-coordinator-agent-dynamic-auth-with-file-agent-name
            guix-build-coordinator-agent-dynamic-auth-with-file-token-file

            guix-build-coordinator-agent-service-type

            <guix-data-service-configuration>
            guix-data-service-configuration
            guix-data-service-configuration?
            guix-data-service-package
            guix-data-service-user
            guix-data-service-group
            guix-data-service-port
            guix-data-service-host
            guix-data-service-getmail-idle-mailboxes
            guix-data-service-commits-getmail-retriever-configuration
            guix-data-service-configuration-git-repositories
            guix-data-service-configuration-build-servers

            guix-data-service-type

            guix-home-service-type

            nar-herder-service-type
            nar-herder-configuration
            nar-herder-configuration?
            nar-herder-configuration-package
            nar-herder-configuration-user
            nar-herder-configuration-group
            nar-herder-configuration-mirror
            nar-herder-configuration-database
            nar-herder-configuration-database-dump
            nar-herder-configuration-host
            nar-herder-configuration-port
            nar-herder-configuration-storage
            nar-herder-configuration-storage-limit
            nar-herder-configuration-storage-nar-removal-criteria
            nar-herder-configuration-log-level
            nar-herder-configuration-cached-compressions
            nar-herder-configuration-cached-compression-min-uses
            nar-herder-configuration-cached-compression-workers
            nar-herder-configuration-cached-compression-nar-source
            nar-herder-configuration-extra-environment-variables

            nar-herder-cached-compression-configuration
            nar-herder-cached-compression-configuration?
            nar-herder-cached-compression-configuration-type
            nar-herder-cached-compression-configuration-level
            nar-herder-cached-compression-configuration-directory
            nar-herder-cached-compression-configuration-directory-max-size

            bffe-configuration
            bffe-configuration?
            bffe-configuration-package
            bffe-configuration-user
            bffe-configuration-group
            bffe-configuration-arguments
            bffe-configuration-extra-environment-variables

            bffe-service-type))

;;;; Commentary:
;;;
;;; Services specifically related to GNU Guix.
;;;
;;;; Code:

(define-record-type* <guix-build-coordinator-configuration>
  guix-build-coordinator-configuration make-guix-build-coordinator-configuration
  guix-build-coordinator-configuration?
  (package                         guix-build-coordinator-configuration-package
                                   (default guix-build-coordinator))
  (user                            guix-build-coordinator-configuration-user
                                   (default "guix-build-coordinator"))
  (group                           guix-build-coordinator-configuration-group
                                   (default "guix-build-coordinator"))
  (database-uri-string
   guix-build-coordinator-configuration-datastore-uri-string
   (default "sqlite:///var/lib/guix-build-coordinator/guix_build_coordinator.db"))
  (agent-communication-uri-string
   guix-build-coordinator-configuration-agent-communication-uri-string
   (default "http://0.0.0.0:8745"))
  (client-communication-uri-string
   guix-build-coordinator-configuration-client-communication-uri-string
   (default "http://127.0.0.1:8746"))
  (allocation-strategy
   guix-build-coordinator-configuration-allocation-strategy
   (default #~basic-build-allocation-strategy))
  (hooks                           guix-build-coordinator-configuration-hooks
                                   (default '()))
  (parallel-hooks                  guix-build-coordinator-configuration-parallel-hooks
                                   (default '()))
  (listen-repl                     guix-build-coordinator-configuration-listen-repl
                                   (default #f))
  (guile                           guix-build-coordinator-configuration-guile
                                   (default guile-next))
  (extra-environment-variables
   guix-build-coordinator-configuration-extra-environment-variables
   (default '()))
  (extra-build-coordinator-arguments
   guix-build-coordinator-configuration-extra-build-coordinator-arguments
   (default '())))

(define-record-type* <guix-build-coordinator-agent-configuration>
  guix-build-coordinator-agent-configuration
  make-guix-build-coordinator-agent-configuration
  guix-build-coordinator-agent-configuration?
  (package             guix-build-coordinator-agent-configuration-package
                       (default guix-build-coordinator/agent-only))
  (user                guix-build-coordinator-agent-configuration-user
                       (default "guix-build-coordinator-agent"))
  (coordinator         guix-build-coordinator-agent-configuration-coordinator
                       (default "http://localhost:8745"))
  (authentication      guix-build-coordinator-agent-configuration-authentication)
  (systems             guix-build-coordinator-agent-configuration-systems
                       (default #f))
  (max-parallel-builds
   guix-build-coordinator-agent-configuration-max-parallel-builds
   (default #f))
  (max-parallel-uploads
   guix-build-coordinator-agent-configuration-max-parallel-uploads
   (default #f))
  (max-allocated-builds
   guix-build-coordinator-agent-configuration-max-allocated-builds
   (default #f))
  (max-1min-load-average
   guix-build-coordinator-agent-configuration-max-1min-load-average
   (default #f))
  (derivation-substitute-urls
   guix-build-coordinator-agent-configuration-derivation-substitute-urls
   (default #f))
  (non-derivation-substitute-urls
   guix-build-coordinator-agent-configuration-non-derivation-substitute-urls
   (default #f))
  (extra-options
   guix-build-coordinator-agent-configuration-extra-options
   (default '())))

(define-record-type* <guix-build-coordinator-agent-password-auth>
  guix-build-coordinator-agent-password-auth
  make-guix-build-coordinator-agent-password-auth
  guix-build-coordinator-agent-password-auth?
  (uuid                guix-build-coordinator-agent-password-auth-uuid)
  (password            guix-build-coordinator-agent-password-auth-password))

(define-record-type* <guix-build-coordinator-agent-password-file-auth>
  guix-build-coordinator-agent-password-file-auth
  make-guix-build-coordinator-agent-password-file-auth
  guix-build-coordinator-agent-password-file-auth?
  (uuid                guix-build-coordinator-agent-password-file-auth-uuid)
  (password-file
   guix-build-coordinator-agent-password-file-auth-password-file))

(define-record-type* <guix-build-coordinator-agent-dynamic-auth>
  guix-build-coordinator-agent-dynamic-auth
  make-guix-build-coordinator-agent-dynamic-auth
  guix-build-coordinator-agent-dynamic-auth?
  (agent-name          guix-build-coordinator-agent-dynamic-auth-agent-name)
  (token               guix-build-coordinator-agent-dynamic-auth-token))

(define-record-type* <guix-build-coordinator-agent-dynamic-auth-with-file>
  guix-build-coordinator-agent-dynamic-auth-with-file
  make-guix-build-coordinator-agent-dynamic-auth-with-file
  guix-build-coordinator-agent-dynamic-auth-with-file?
  (agent-name      guix-build-coordinator-agent-dynamic-auth-with-file-agent-name)
  (token-file      guix-build-coordinator-agent-dynamic-auth-with-file-token-file))

(define* (make-guix-build-coordinator-start-script database-uri-string
                                                   allocation-strategy
                                                   pid-file
                                                   guix-build-coordinator-package
                                                   #:key
                                                   agent-communication-uri-string
                                                   client-communication-uri-string
                                                   (hooks '())
                                                   (parallel-hooks '())
                                                   (guile guile-next)
                                                   listen-repl
                                                   (extra-build-coordinator-arguments
                                                    '()))
  (program-file
   "start-guix-build-coordinator"
   (with-extensions (cons guix-build-coordinator-package
                          ;; This is a poorly constructed Guile load path,
                          ;; since it contains things that aren't Guile
                          ;; libraries, but it means that the Guile libraries
                          ;; needed for the Guix Build Coordinator don't need
                          ;; to be individually specified here.
                          (append
                           (map second (package-inputs
                                        guix-build-coordinator-package))
                           (map second (package-propagated-inputs
                                        guix-build-coordinator-package))))
     #~(begin
         (use-modules (srfi srfi-1)
                      (ice-9 match)
                      (web uri)
                      (prometheus)
                      (guix-build-coordinator hooks)
                      (guix-build-coordinator datastore)
                      (guix-build-coordinator build-allocator)
                      (guix-build-coordinator coordinator))

         (setvbuf (current-output-port) 'line)
         (setvbuf (current-error-port) 'line)

         (simple-format #t "starting the guix-build-coordinator:\n  ~A\n"
                        (current-filename))
         (let* ((hooks
                 (list #$@(map (match-lambda
                                 ((name . hook-gexp)
                                  #~(cons '#$name #$hook-gexp)))
                               hooks)))
                (hooks-with-defaults
                 `(,@hooks
                   ,@(remove (match-lambda
                               ((name . _) (assq-ref hooks name)))
                             %default-hooks)))
                (build-coordinator (make-build-coordinator
                                    #:database-uri-string #$database-uri-string
                                    #:hooks hooks-with-defaults
                                    #:allocation-strategy #$allocation-strategy
                                    #:timestamp-log-output? #f
                                    #$@extra-build-coordinator-arguments)))

           (run-coordinator-service
            build-coordinator
            #:update-datastore? #t
            #:pid-file #$pid-file
            #:agent-communication-uri (string->uri
                                       #$agent-communication-uri-string)
            #:client-communication-uri (string->uri
                                        #$client-communication-uri-string)
            #:parallel-hooks (list #$@(map (match-lambda
                                             ((name . val)
                                              #~(cons '#$name #$val)))
                                           parallel-hooks))
            #:listen-repl #$listen-repl))))
   #:guile guile))

(define (guix-build-coordinator-shepherd-services config)
  (match-record config <guix-build-coordinator-configuration>
    (package user group database-uri-string
             agent-communication-uri-string
             client-communication-uri-string
             allocation-strategy
             hooks
             parallel-hooks
             listen-repl
             guile
             extra-environment-variables
             extra-build-coordinator-arguments)
    (list
     (shepherd-service
      (documentation "Guix Build Coordinator")
      (provision '(guix-build-coordinator))
      (requirement '(user-processes networking))
      (start #~(lambda args
                 (parameterize ((%current-logfile-date-format ""))
                   (apply
                    (make-forkexec-constructor
                     (list #$(make-guix-build-coordinator-start-script
                              database-uri-string
                              allocation-strategy
                              "/var/run/guix-build-coordinator/pid"
                              package
                              #:agent-communication-uri-string
                              agent-communication-uri-string
                              #:client-communication-uri-string
                              client-communication-uri-string
                              #:hooks hooks
                              #:parallel-hooks parallel-hooks
                              #:listen-repl listen-repl
                              #:guile guile
                              #:extra-build-coordinator-arguments
                              extra-build-coordinator-arguments))
                     #:user #$user
                     #:group #$group
                     #:directory "/var/lib/guix-build-coordinator"
                     #:pid-file "/var/run/guix-build-coordinator/pid"
                     ;; Allow time for migrations to run
                     #:pid-file-timeout 240
                     #:environment-variables
                     `("LC_ALL=en_US.utf8"
                       "PATH=/run/current-system/profile/bin" ; for hooks
                       #$@extra-environment-variables)
                     #:log-file "/var/log/guix-build-coordinator/coordinator.log")
                    args))))
      (stop #~(make-kill-destructor))
      (modules
       `((shepherd comm)
         ,@%default-modules))))))

(define (guix-build-coordinator-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(guix-build-coordinator-configuration-user
                  config)))

      (chmod "/var/lib/guix-build-coordinator" #o755)

      (mkdir-p "/var/log/guix-build-coordinator")

      ;; Allow writing the PID file
      (mkdir-p "/var/run/guix-build-coordinator")
      (chown "/var/run/guix-build-coordinator"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-build-coordinator-account config)
  (match-record config <guix-build-coordinator-configuration>
    (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Guix Build Coordinator user")
           (home-directory "/var/lib/guix-build-coordinator")
           (shell (file-append shadow "/sbin/nologin"))))))

(define guix-build-coordinator-service-type
  (service-type
   (name 'guix-build-coordinator)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        guix-build-coordinator-shepherd-services)
     (service-extension activation-service-type
                        guix-build-coordinator-activation)
     (service-extension account-service-type
                        guix-build-coordinator-account)))
   (default-value
     (guix-build-coordinator-configuration))
   (description
    "Run an instance of the Guix Build Coordinator.")))

(define (guix-build-coordinator-agent-shepherd-services config)
  (match-record config <guix-build-coordinator-agent-configuration>
    (package user coordinator authentication
             max-parallel-builds max-parallel-uploads
             max-allocated-builds max-1min-load-average
             derivation-substitute-urls non-derivation-substitute-urls
             extra-options
             systems)
    (list
     (shepherd-service
      (documentation "Guix Build Coordinator Agent")
      (provision '(guix-build-coordinator-agent))
      (requirement '(networking user-processes))
      (start
       #~(lambda _
           (parameterize ((%current-logfile-date-format ""))
             (fork+exec-command
              (list #$(file-append package "/bin/guix-build-coordinator-agent")
                    #$(string-append "--coordinator=" coordinator)
                    "--timestamp-log-output=false"
                    #$@(match authentication
                         (($ <guix-build-coordinator-agent-password-auth>
                             uuid password)
                          #~(#$(string-append "--uuid=" uuid)
                             #$(string-append "--password=" password)))
                         (($ <guix-build-coordinator-agent-password-file-auth>
                             uuid password-file)
                          #~(#$(string-append "--uuid=" uuid)
                             #$(string-append "--password-file="
                                              password-file)))
                         (($ <guix-build-coordinator-agent-dynamic-auth>
                             agent-name token)
                          #~(#$(string-append "--name=" agent-name)
                             #$(string-append "--dynamic-auth-token=" token)))
                         (($
                           <guix-build-coordinator-agent-dynamic-auth-with-file>
                           agent-name token-file)
                          #~(#$(string-append "--name=" agent-name)
                             #$(string-append "--dynamic-auth-token-file="
                                              token-file))))
                    #$@(if max-parallel-builds
                           #~(#$(simple-format #f "--max-parallel-builds=~A"
                                               max-parallel-builds))
                           #~())
                    #$@(if max-parallel-uploads
                           #~(#$(simple-format #f "--max-parallel-uploads=~A"
                                               max-parallel-uploads))
                           #~())
                    #$@(if max-allocated-builds
                           #~(#$(simple-format #f "--max-allocated-builds=~A"
                                               max-allocated-builds))
                           #~())
                    #$@(if max-1min-load-average
                           #~(#$(simple-format #f "--max-1min-load-average=~A"
                                               max-1min-load-average))
                           #~())
                    #$@(if derivation-substitute-urls
                           #~(#$(string-append
                                 "--derivation-substitute-urls="
                                 (string-join derivation-substitute-urls " ")))
                           #~())
                    #$@(if non-derivation-substitute-urls
                           #~(#$(string-append
                                 "--non-derivation-substitute-urls="
                                 (string-join non-derivation-substitute-urls " ")))
                           #~())
                    #$@extra-options
                    #$@(map (lambda (system)
                              (string-append "--system=" system))
                            (or systems '())))
              #:user #$user
              #:environment-variables
              `(;; XDG_CACHE_HOME is used by Guix when caching narinfo files
                "XDG_CACHE_HOME=/var/cache/guix-build-coordinator-agent"
                "LC_ALL=en_US.utf8")
              #:log-file "/var/log/guix-build-coordinator/agent.log"))))
      (stop #~(make-kill-destructor))
      (modules
       `((shepherd comm)
         ,@%default-modules))))))

(define (guix-build-coordinator-agent-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(guix-build-coordinator-agent-configuration-user
                  config)))

      (mkdir-p "/var/log/guix-build-coordinator")

      ;; Create a cache directory for storing narinfo files if downloaded
      (mkdir-p "/var/cache/guix-build-coordinator-agent")
      (chown "/var/cache/guix-build-coordinator-agent"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-build-coordinator-agent-account config)
  (list (user-account
         (name (guix-build-coordinator-agent-configuration-user config))
         (group "nogroup")
         (system? #t)
         (comment "Guix Build Coordinator agent user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define guix-build-coordinator-agent-service-type
  (service-type
   (name 'guix-build-coordinator-agent)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        guix-build-coordinator-agent-shepherd-services)
     (service-extension activation-service-type
                        guix-build-coordinator-agent-activation)
     (service-extension account-service-type
                        guix-build-coordinator-agent-account)))
   (description
    "Run a Guix Build Coordinator agent.")))


;;;
;;; Guix Data Service
;;;

(define-record-type* <guix-data-service-configuration>
  guix-data-service-configuration make-guix-data-service-configuration
  guix-data-service-configuration?
  (package          guix-data-service-package
                    (default guix-data-service))
  (user             guix-data-service-configuration-user
                    (default "guix-data-service"))
  (group            guix-data-service-configuration-group
                    (default "guix-data-service"))
  (port             guix-data-service-port
                    (default 8765))
  (host             guix-data-service-host
                    (default "127.0.0.1"))
  (getmail-idle-mailboxes
   guix-data-service-getmail-idle-mailboxes
   (default #f))
  (commits-getmail-retriever-configuration
   guix-data-service-commits-getmail-retriever-configuration
   (default #f))
  (extra-options    guix-data-service-extra-options
                    (default '()))
  (extra-process-jobs-options
   guix-data-service-extra-process-jobs-options
   (default '()))
  (git-repositories guix-data-service-configuration-git-repositories
                    (default #f))
  (build-servers    guix-data-service-configuration-build-servers
                    (default #f)))

(define (guix-data-service-profile-packages config)
  "Return the guix-data-service package, this will populate the
ca-certificates.crt file in the system profile."
  (list
   (guix-data-service-package config)))

(define (guix-data-service-shepherd-services config)
  (match-record config <guix-data-service-configuration>
    (package user group port host extra-options extra-process-jobs-options
             git-repositories build-servers)
    (list
     (shepherd-service
      (documentation "Guix Data Service web server")
      (provision '(guix-data-service))
      (requirement '(user-processes postgres networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package
                                     "/bin/guix-data-service")
                      "--pid-file=/var/run/guix-data-service/pid"
                      #$(string-append "--port=" (number->string port))
                      #$(string-append "--host=" host)
                      ;; Perform any database migrations when the
                      ;; service is started
                      "--update-database"
                      #$@extra-options)

                #:user #$user
                #:group #$group
                #:directory "/var/lib/guix-data-service"
                #:pid-file "/var/run/guix-data-service/pid"
                #:environment-variables
                `(,(string-append
                    "GUIX_LOCPATH="
                    #$(libc-utf8-locales-for-target) "/lib/locale")
                  "LC_ALL=en_US.UTF-8")
                #:log-file "/var/log/guix-data-service/web.log"))
      (stop #~(make-kill-destructor)))

     (shepherd-service
      (documentation "Guix Data Service setup database")
      (provision '(guix-data-service-setup-database))
      (requirement '(user-processes postgres))
      (one-shot? #t)
      (start
       #~(make-forkexec-constructor
          (list
           #$(program-file
              "data-service-setup-database"
              (with-extensions
                  (cons package
                        ;; This is a poorly constructed Guile load path, since
                        ;; it contains things that aren't Guile libraries, but
                        ;; it means that the Guile libraries needed for the
                        ;; Guix Data Service don't need to be individually
                        ;; specified here.
                        (append
                         (map second (package-inputs package))
                         (map second (package-propagated-inputs package))))
                #~(lambda _
                    (use-modules (guix-data-service database)
                                 (guix-data-service model git-repository)
                                 (guix-data-service model build-server))

                    (begin
                      ((@ (guix-data-service database) run-sqitch))

                      #$@(if git-repositories
                             #~(((@ (guix-data-service model git-repository)
                                    specify-git-repositories)
                                 '(#$@git-repositories)))
                             '())
                      #$@(if build-servers
                             #~(((@ (guix-data-service model build-server)
                                    specify-build-servers)
                                 '(#$@build-servers)))
                             '()))))))
          #:user #$user
          #:group #$group
          #:directory "/var/lib/guix-data-service"
          #:environment-variables
          `(,(string-append
              "GUIX_LOCPATH="
              #$(libc-utf8-locales-for-target) "/lib/locale")
            "LC_ALL=en_US.utf8")
          #:log-file "/var/log/guix-data-service/setup-database.log"))
      (auto-start? #t))

     (shepherd-service
      (documentation "Guix Data Service process jobs")
      (provision '(guix-data-service-process-jobs))
      (requirement '(user-processes postgres networking))
      (start #~(make-forkexec-constructor
                (list
                 #$(file-append package
                                "/bin/guix-data-service-process-jobs")
                 #$@extra-process-jobs-options)
                #:user #$user
                #:group #$group
                #:directory "/var/lib/guix-data-service"
                #:environment-variables
                `("HOME=/var/lib/guix-data-service"
                  "GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt"
                  ,(string-append
                    "GUIX_LOCPATH="
                    #$(libc-utf8-locales-for-target) "/lib/locale")
                  "LC_ALL=en_US.UTF-8")
                #:log-file "/var/log/guix-data-service/process-jobs.log"))
      (stop #~(make-kill-destructor))))))

(define (guix-data-service-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user (getpw "guix-data-service"))

      (chmod "/var/lib/guix-data-service" #o755)

      (mkdir-p "/var/log/guix-data-service")

      ;; Allow writing the PID file
      (mkdir-p "/var/run/guix-data-service")
      (chown "/var/run/guix-data-service"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (guix-data-service-account config)
  (match-record config <guix-data-service-configuration>
    (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Guix Data Service user")
           (home-directory "/var/lib/guix-data-service")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (guix-data-service-getmail-configuration config)
  (match config
    (($ <guix-data-service-configuration> package user group
                                          port host
                                          #f #f)
     '())
    (($ <guix-data-service-configuration> package user group
                                          port host
                                          getmail-idle-mailboxes
                                          commits-getmail-retriever-configuration)
     (list
      (getmail-configuration
       (name 'guix-data-service)
       (user user)
       (group group)
       (directory "/var/lib/getmail/guix-data-service")
       (rcfile
        (getmail-configuration-file
         (retriever commits-getmail-retriever-configuration)
         (destination
          (getmail-destination-configuration
           (type "MDA_external")
           (path (file-append
                  package
                  "/bin/guix-data-service-process-branch-updated-email"))))
         (options
          (getmail-options-configuration
           (read-all #f)
           (delivered-to #f)
           (received #f)))))
       (idle getmail-idle-mailboxes))))))

(define guix-data-service-type
  (service-type
   (name 'guix-data-service)
   (extensions
    (list
     (service-extension profile-service-type
                        guix-data-service-profile-packages)
     (service-extension shepherd-root-service-type
                        guix-data-service-shepherd-services)
     (service-extension activation-service-type
                        guix-data-service-activation)
     (service-extension account-service-type
                        guix-data-service-account)
     (service-extension getmail-service-type
                        guix-data-service-getmail-configuration)))
   (default-value
     (guix-data-service-configuration))
   (description
    "Run an instance of the Guix Data Service.")))


;;;
;;; Guix Home Service
;;;

(define (guix-home-shepherd-service config)
  (map (match-lambda
         ((user he)
          (shepherd-service
           (documentation "Activate Guix Home.")
           (requirement '(user-processes))
           (provision (list (symbol-append 'guix-home- (string->symbol user))))
           (one-shot? #t)
           (auto-start? #t)
           (start #~(make-forkexec-constructor
                     '(#$(file-append he "/activate"))
                     #:user #$user
                     #:environment-variables
                     (list (string-append "HOME=" (passwd:dir (getpw #$user)))
                           "GUIX_SYSTEM_IS_RUNNING_HOME_ACTIVATE=t")
                     #:group (group:name (getgrgid (passwd:gid (getpw #$user))))))
           (stop #~(make-kill-destructor)))))
       config))

(define guix-home-service-type
  (service-type
   (name 'guix-home)
   (description "Sets up Guix Home for the specified user accounts.")
   (extensions (list (service-extension
                      shepherd-root-service-type
                      guix-home-shepherd-service)))
   (compose concatenate)
   (extend append)
   (default-value '())))


;;;
;;; Nar Herder
;;;

(define-record-type* <nar-herder-configuration>
  nar-herder-configuration make-nar-herder-configuration
  nar-herder-configuration?
  (package       nar-herder-configuration-package
                 (default nar-herder))
  (user          nar-herder-configuration-user
                 (default "nar-herder"))
  (group         nar-herder-configuration-group
                 (default "nar-herder"))
  (mirror        nar-herder-configuration-mirror
                 (default #f))
  (database      nar-herder-configuration-database
                 (default "/var/lib/nar-herder/nar_herder.db"))
  (database-dump nar-herder-configuration-database-dump
                 (default "/var/lib/nar-herder/nar_herder_dump.db"))
  (host          nar-herder-configuration-host
                 (default "127.0.0.1"))
  (port          nar-herder-configuration-port
                 (default 8734))
  (storage       nar-herder-configuration-storage
                 (default #f))
  (storage-limit nar-herder-configuration-storage-limit
                 (default "none"))
  (storage-nar-removal-criteria
   nar-herder-configuration-storage-nar-removal-criteria
   (default '()))
  (ttl           nar-herder-configuration-ttl
                 (default #f))
  (new-ttl       nar-herder-configuration-new-ttl
                 (default #f))
  (negative-ttl  nar-herder-configuration-negative-ttl
                 (default #f))
  (log-level     nar-herder-configuration-log-level
                 (default 'DEBUG))
  (cached-compressions
   nar-herder-configuration-cached-compressions
   (default '()))
  (cached-compression-min-uses
   nar-herder-configuration-cached-compression-min-uses
   (default 3))
  (cached-compression-workers
   nar-herder-configuration-cached-compression-workers
   (default 2))
  (cached-compression-nar-source
   nar-herder-configuration-cached-compression-nar-source
   (default #f))
  (extra-environment-variables
   nar-herder-configuration-extra-environment-variables
   (default '())))

(define-record-type* <nar-herder-cached-compression-configuration>
  nar-herder-cached-compression-configuration
  make-nar-herder-cached-compression-configuration
  nar-herder-cached-compression-configuration?
  (type                nar-herder-cached-compression-configuration-type)
  (level               nar-herder-cached-compression-configuration-level
                       (default #f))
  (directory           nar-herder-cached-compression-configuration-directory
                       (default #f))
  (directory-max-size
   nar-herder-cached-compression-configuration-directory-max-size
   (default #f))
  (unused-removal-duration
   nar-herder-cached-compression-configuration-unused-removal-duration
   (default #f))
  (ttl                 nar-herder-cached-compression-configuration-ttl
                       (default #f))
  (new-ttl             nar-herder-cached-compression-configuration-new-ttl
                       (default #f)))

(define (nar-herder-shepherd-services config)
  (define (cached-compression-configuration->options cached-compression)
    (match-record
        cached-compression
        <nar-herder-cached-compression-configuration>
        (type level directory directory-max-size
              unused-removal-duration ttl new-ttl)

      `(,(simple-format #f "--enable-cached-compression=~A~A"
                        type
                        (if level
                            (simple-format #f ":~A" level)
                            ""))
        ,@(if directory
              (list
               (simple-format #f "--cached-compression-directory=~A=~A"
                              type
                              directory))
              '())
        ,@(if directory-max-size
              (list
               (simple-format #f "--cached-compression-directory-max-size=~A=~A"
                              type
                              directory-max-size))
              '())
        ,@(if unused-removal-duration
              (list
               (simple-format
                #f "--cached-compression-unused-removal-duration=~A=~A"
                type
                unused-removal-duration))
              '())
        ,@(if ttl
              (list
               (simple-format
                #f "--cached-compression-ttl=~A=~A"
                type
                ttl))
              '())
        ,@(if new-ttl
              (list
               (simple-format
                #f "--cached-compression-new-ttl=~A=~A"
                type
                new-ttl))
              '()))))

  (match-record config <nar-herder-configuration>
    (package user group
             mirror
             database database-dump
             host port
             storage storage-limit storage-nar-removal-criteria
             ttl new-ttl negative-ttl log-level
             cached-compressions cached-compression-min-uses
             cached-compression-workers cached-compression-nar-source
             extra-environment-variables)

    (unless (or mirror storage)
      (error "nar-herder: mirror or storage must be set"))

    (list
     (shepherd-service
      (documentation "Nar Herder")
      (provision '(nar-herder))
      (requirement '(user-processes networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package
                                     "/bin/nar-herder")
                      "run-server"
                      "--pid-file=/var/run/nar-herder/pid"
                      #$(string-append "--port=" (number->string port))
                      #$(string-append "--host=" host)
                      #$@(if mirror
                             (list (string-append "--mirror=" mirror))
                             '())
                      #$(string-append "--database=" database)
                      #$(string-append "--database-dump=" database-dump)
                      #$@(if storage
                             (list (string-append "--storage=" storage))
                             '())
                      #$(string-append "--storage-limit="
                                       (if (number? storage-limit)
                                           (number->string storage-limit)
                                           storage-limit))
                      #$@(map (lambda (criteria)
                                (string-append
                                 "--storage-nar-removal-criteria="
                                 (match criteria
                                   ((k . v) (simple-format #f "~A=~A" k v))
                                   (str str))))
                              storage-nar-removal-criteria)
                      #$@(if ttl
                             (list (string-append "--ttl=" ttl))
                             '())
                      #$@(if new-ttl
                             (list (string-append "--new-ttl=" new-ttl))
                             '())
                      #$@(if negative-ttl
                             (list (string-append "--negative-ttl=" negative-ttl))
                             '())
                      #$@(if log-level
                             (list (simple-format #f "--log-level=~A" log-level))
                             '())
                      #$@(append-map
                          cached-compression-configuration->options
                          cached-compressions)
                      #$@(if cached-compression-min-uses
                             (list (simple-format
                                    #f "--cached-compression-min-uses=~A"
                                    cached-compression-min-uses))
                             '())
                      #$@(if cached-compression-workers
                             (list (simple-format
                                    #f "--cached-compression-workers=~A"
                                    cached-compression-workers))
                             '())
                      #$@(if cached-compression-nar-source
                             (list (simple-format
                                    #f "--cached-compression-nar-source=~A"
                                    cached-compression-nar-source))
                             '()))
                #:user #$user
                #:group #$group
                #:directory "/var/lib/nar-herder"
                #:pid-file "/var/run/nar-herder/pid"
                #:environment-variables
                `(,(string-append
                    "GUIX_LOCPATH="
                    #$(libc-utf8-locales-for-target) "/lib/locale")
                  "LC_ALL=en_US.utf8"
                  #$@extra-environment-variables)
                #:log-file "/var/log/nar-herder/server.log"))
      (stop #~(make-kill-destructor))))))

(define (nar-herder-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(nar-herder-configuration-user
                  config)))

      (chmod "/var/lib/nar-herder" #o755)

      (mkdir-p "/var/log/nar-herder")

      ;; Allow writing the PID file
      (mkdir-p "/var/run/nar-herder")
      (chown "/var/run/nar-herder"
             (passwd:uid %user)
             (passwd:gid %user))))

(define (nar-herder-account config)
  (match-record config <nar-herder-configuration>
    (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "Nar Herder user")
           (home-directory "/var/lib/nar-herder")
           (shell (file-append shadow "/sbin/nologin"))))))

(define nar-herder-service-type
  (service-type
   (name 'nar-herder)
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        nar-herder-shepherd-services)
     (service-extension activation-service-type
                        nar-herder-activation)
     (service-extension account-service-type
                        nar-herder-account)))
   (description
    "Run a Nar Herder server.")))


;;;
;;; Build Farm Front-end (BFFE)
;;;

(define-record-type* <bffe-configuration>
  bffe-configuration make-bffe-configuration
  bffe-configuration?
  (package       bffe-configuration-package
                 (default bffe))
  (user          bffe-configuration-user
                 (default "bffe"))
  (group         bffe-configuration-group
                 (default "bffe"))
  (arguments     bffe-configuration-arguments)
  (extra-environment-variables
   bffe-configuration-extra-environment-variables
   (default '())))

(define (bffe-shepherd-services config)
  (define bffe-package
    (bffe-configuration-package config))

  (define start-script
    (program-file
     "run-bffe"
     (with-extensions (cons
                       bffe-package
                       ;; This is a poorly constructed Guile load path,
                       ;; since it contains things that aren't Guile
                       ;; libraries, but it means that the Guile
                       ;; libraries needed for BFFE don't need to be
                       ;; individually specified here.
                       (map second (package-transitive-propagated-inputs
                                    bffe-package)))
       #~(begin
           (use-modules (bffe)
                        (bffe manage-builds))

           (setvbuf (current-output-port) 'line)
           (setvbuf (current-error-port) 'line)

           (simple-format #t "starting the bffe:\n  ~A\n"
                          (current-filename))

           (apply run-bffe-service
                  (append
                   (list #:pid-file "/var/run/bffe/pid")
                   #$(bffe-configuration-arguments config)))))
     #:guile (lookup-package-native-input bffe-package "guile-next")))

  (match-record config <bffe-configuration>
    (package user group arguments extra-environment-variables)

    (list
     (shepherd-service
      (documentation "Build Farm Front-end")
      (provision '(bffe))
      (requirement '(user-processes networking))
      (start #~(make-forkexec-constructor
                (list #$start-script)
                #:user #$user
                #:group #$group
                #:pid-file "/var/run/bffe/pid"
                #:directory "/var/lib/bffe"
                #:environment-variables
                `(,(string-append
                    "GUIX_LOCPATH="
                    #$(libc-utf8-locales-for-target) "/lib/locale")
                  "LC_ALL=en_US.utf8"
                  #$@extra-environment-variables)
                #:log-file "/var/log/bffe/server.log"))
      (stop #~(make-kill-destructor))))))

(define (bffe-activation config)
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw #$(bffe-configuration-user config)))

      (chmod "/var/lib/bffe" #o755)

      (mkdir-p "/var/log/bffe")

      ;; Allow writing the PID file
      (mkdir-p "/var/run/bffe")
      (chown "/var/run/bffe" (passwd:uid %user) (passwd:gid %user))))

(define (bffe-account config)
  (match-record config <bffe-configuration>
    (user group)
    (list (user-group
           (name group)
           (system? #t))
          (user-account
           (name user)
           (group group)
           (system? #t)
           (comment "BFFE user")
           (home-directory "/var/lib/bffe")
           (shell (file-append shadow "/sbin/nologin"))))))

(define bffe-service-type
  (service-type
   (name 'bffe)
   (extensions
    (list (service-extension shepherd-root-service-type
                             bffe-shepherd-services)
          (service-extension activation-service-type
                             bffe-activation)
          (service-extension account-service-type
                             bffe-account)))
   (description
    "Run the Build Farm Front-end.")))
