;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2019, 2020, 2021 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2021, 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2025 David Thompson <davet@gnu.org>
;;; Copyright © 2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu services ci)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:autoload   (guix modules) (source-module-closure)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages ci)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (ice-9 match)
  #:export (laminar-configuration
            laminar-configuration?
            laminar-configuration-home-directory
            laminar-configuration-supplementary-groups
            laminar-configuration-bind-http
            laminar-configuration-bind-rpc
            laminar-configuration-title
            laminar-configuration-keep-rundirs
            laminar-configuration-archive-url
            laminar-configuration-base-url

            laminar-service-type

            forgejo-runner-configuration
            forgejo-runner-configuration?
            forgejo-runner-configuration-package
            forgejo-runner-configuration-data-directory
            forgejo-runner-configuration-run-directory
            forgejo-runner-configuration-name
            forgejo-runner-configuration-labels
            forgejo-runner-configuration-capacity
            forgejo-runner-configuration-timeout
            forgejo-runner-configuration-fetch-timeout
            forgejo-runner-configuration-fetch-interval
            forgejo-runner-configuration-report-interval

            forgejo-runner-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instances of Laminar, a
;;; continuous integration tool.
;;;
;;;; Code:

(define-record-type* <laminar-configuration>
  laminar-configuration make-laminar-configuration
  laminar-configuration?
  (laminar              laminars-configuration-laminar
                        (default laminar))
  (home-directory       laminar-configuration-home-directory
                        (default "/var/lib/laminar"))
  (supplementary-groups laminar-configuration-supplementary-groups
                        (default '()))
  (bind-http            laminar-configuration-bind-http
                        (default "*:8080"))
  (bind-rpc             laminar-configuration-bind-rpc
                        (default "unix-abstract:laminar"))
  (title                laminar-configuration-title
                        (default "Laminar"))
  (keep-rundirs         laminar-keep-rundirs
                        (default 0))
  (archive-url          laminar-archive-url
                        (default #f))
  (base-url             laminar-base-url
                        (default #f)))

(define laminar-shepherd-service
  (match-lambda
    (($ <laminar-configuration> laminar home-directory supplementary-groups
                                bind-http bind-rpc
                                title keep-rundirs archive-url
                                base-url)
     (list (shepherd-service
            (documentation "Run Laminar.")
            (provision '(laminar))
            (requirement '(user-processes networking))
            (start #~(make-forkexec-constructor
                      (list #$(file-append laminar "/sbin/laminard"))
                      #:environment-variables
                      `(,(string-append "LAMINAR_HOME="
                                        #$home-directory)
                        ,(string-append "LAMINAR_BIND_HTTP="
                                        #$bind-http)
                        ,(string-append "LAMINAR_BIND_RPC="
                                        #$bind-rpc)
                        ,(string-append "LAMINAR_TITLE="
                                        #$title)
                        ,(string-append "LAMINAR_KEEP_RUNDIRS="
                                        #$(number->string
                                           keep-rundirs))
                        ,@(if #$archive-url
                              (list
                               (string-append "LAMINAR_ARCHIVE_URL="
                                              #$archive-url))
                              '())
                        ,@(if #$base-url
                              (list
                               (string-append "LAMINAR_BASE_URL="
                                              #$base-url))
                              '()))
                      #:user "laminar"
                      #:group "laminar"
                      #:supplementary-groups '#$supplementary-groups))
            (stop #~(make-kill-destructor)))))))

(define (laminar-account config)
  "Return the user accounts and user groups for CONFIG."
  (list (user-group
         (name "laminar")
         (system? #t))
        (user-account
         (name "laminar")
         (group "laminar")
         (supplementary-groups
          (laminar-configuration-supplementary-groups config))
         (system? #t)
         (comment "Laminar privilege separation user")
         (home-directory (laminar-configuration-home-directory config))
         (shell #~(string-append #$shadow "/sbin/nologin")))))

(define (laminar-activation config)
  (let ((bind-http (laminar-configuration-bind-http config)))
    #~(begin
        ;; If listen is a unix socket, create its parent directory.
        (when (string-prefix? "unix:" #$bind-http)
          (let ((run-directory
                 (dirname (substring #$bind-http (string-length "unix:"))))
                (user (getpw "laminar")))
            (mkdir-p run-directory)
            (chown run-directory (passwd:uid user) (passwd:gid user)))))))

(define laminar-service-type
  (service-type
   (name 'laminar)
   (extensions
    (list
     (service-extension shepherd-root-service-type laminar-shepherd-service)
     (service-extension account-service-type laminar-account)
     (service-extension activation-service-type laminar-activation)))
   (default-value (laminar-configuration))
   (description
    "Run the Laminar continuous integration service.")))


;;;
;;; Forgejo runner.
;;;

(define-record-type* <forgejo-runner-configuration>
  forgejo-runner-configuration
  make-forgejo-runner-configuration
  forgejo-runner-configuration?
  (package         forgejo-runner-configuration-package
                   (default forgejo-runner))
  (data-directory  forgejo-runner-configuration-data-directory
                   (default "/var/lib/forgejo-runner"))
  (run-directory   forgejo-runner-configuration-run-directory
                   (default "/var/run/forgejo-runner"))

  ;; Configuration options for the YAML config file:
  ;; <https://forgejo.org/docs/latest/admin/runner-installation/#configuration>.
  (name            forgejo-runner-configuration-name
                   (default #~(gethostname)))
  (labels          forgejo-runner-configuration-labels
                   (default '("guix")))
  (capacity        forgejo-runner-configuration-job-capacity
                   (default 1))
  (timeout         forgejo-runner-configuration-timeout
                   (default (* 3 3600)))
  (fetch-timeout   forgejo-runner-configuration-fetch-timeout
                   (default 5))
  (fetch-interval  forgejo-runner-configuration-fetch-interval
                   (default 2))
  (report-interval forgejo-runner-configuration-report-interval
                   (default 1)))

(define (create-forgejo-runner-account config)
  (list (user-account
          (name "forgejo-runner")
          (group "forgejo-runner")
          (system? #t)
          (comment "Forgejo Runner user")
          (home-directory
           (forgejo-runner-configuration-data-directory config)))
        (user-group
          (name "forgejo-runner")
          (system? #t))))

(define (forgejo-runner-activation config)
  (match-record config <forgejo-runner-configuration>
    (data-directory run-directory)
    #~(let* ((user (getpwnam "forgejo-runner")))
        (mkdir-p #$run-directory)
        (chown #$run-directory (passwd:uid user) (passwd:gid user)))))

;; Very naive YAML writer that does just enough for our needs.
(define* (write-yaml port exp depth)
  (match exp
    ((? string? str)
     (write str port))
    ((? number? n)
     (display n port))
    (('seconds (? number? n))
     (format port "~as" n))
    (#(values ...)
     (display "[ " port)
     (let ((strings
            (map (lambda (value)
                   (call-with-output-string
                     (lambda (port)
                       (write-yaml port value depth))))
                 values)))
       (display (string-join strings ", ") port))
     (display " ]" port))
    (() (values))
    ((((? symbol? k) . v) . rest)
     (do ((i 0 (1+ i)))
         ((= i depth))
       (display "  " port))
     (display k port)
     (display ": " port)
     (match v
       (((k* . v*) . _)                           ; subtree
        (newline port)
        (write-yaml port v (1+ depth)))
       (_ (write-yaml port v depth)))
     (newline port)
     (write-yaml port rest depth))))

(define (yaml-file name exp)
  (plain-file name
              (call-with-output-string
                (lambda (port)
                  (write-yaml port exp 0)))))

(define (forgejo-runner-shepherd-service config)
  (match-record config <forgejo-runner-configuration>
    (package data-directory run-directory name
             capacity timeout fetch-timeout fetch-interval report-interval
             labels)
    (define runner (file-append package "/bin/forgejo-runner"))
    (define runner-file (string-append data-directory "/runner"))
    (define config
      (yaml-file
       "forgejo-runner-config.yml"
       `((runner . ((file . ,runner-file)
                    (capacity . ,capacity)
                    (timeout . (seconds ,timeout))
                    (fetch_timeout . (seconds ,fetch-timeout))
                    (fetch_interval . (seconds ,fetch-interval))
                    (report_interval . (seconds ,report-interval))
                    (labels . ,(list->vector labels))))
         (cache . ((dir . ,(string-append run-directory "/cache"))))
         (host . ((workdir_parent
                   . ,(string-append run-directory "/act")))))))

    (list (shepherd-service
            (provision '(forgejo-runner))
            (requirement '(user-processes networking))
            (start #~(make-forkexec-constructor
                      (list #$runner "daemon" "--config" #$config)
                      #:user "forgejo-runner"
                      #:group "forgejo-runner"
                      #:directory #$run-directory
                      #:environment-variables
                      ;; Provide access to a fresh Guix obtained via 'guix
                      ;; pull'.
                      (cons* (string-append "PATH="
                                            #$data-directory "/.config/guix/current/bin"
                                            ":/run/current-system/profile/bin")
                             (string-append "HOME=" #$data-directory)
                             "GIT_SSL_CAINFO=/etc/ssl/certs/ca-certificates.crt"
                             (default-environment-variables))))
            (stop #~(make-kill-destructor))
            (actions
             (list
              (shepherd-configuration-action config)
              (shepherd-action
                (procedure
                 #~(lambda (running instance token)
                     (define status
                       (spawn-command (list #$runner "register"
                                            "--no-interactive"
                                            "--config" #$config
                                            "--name" #$name
                                            "--instance" instance
                                            "--token" token)
                                      #:user "forgejo-runner"
                                      #:group "forgejo-runner"))

                     (if (zero? status)
                         (format #t "Successfully registered runner \
'~a' for '~a'.~%"
                                 #$name instance)
                         (format #t "'~a register' failed with status ~a.~%"
                                 #$runner status))
                     (zero? status)))
                (name 'register)
                (documentation "Register this runner with a Forgejo server.
This action takes two arguments: the Forgejo server URL and an access
token."))))
            (documentation "Forgejo task runner")))))

(define forgejo-runner-service-type
  (service-type
   (name 'forgejo-runner)
   (extensions
    (list (service-extension activation-service-type
                             forgejo-runner-activation)
          (service-extension account-service-type
                             create-forgejo-runner-account)
          (service-extension shepherd-root-service-type
                             forgejo-runner-shepherd-service)))
   (default-value (forgejo-runner-configuration))
   (description
    "Run @command{forgejo-runner}, a daemon to run tasks for the Forgejo
source code collaboration service.")))
