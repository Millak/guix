;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2025-2026 Artur Wroblewski <wrobell@riseup.net>
;;; Copyright © 2026 Mathieu Lirzin <mthl@gnu.org>
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

(define-module (gnu services high-availability)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages erlang)
  #:use-module (gnu packages high-availability)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:export (epmd-configuration
            epmd-configuration-fields
            epmd-configuration?
            epmd-configuration-user
            epmd-configuration-group
            epmd-configuration-address
            epmd-configuration-port
            epmd-service-type

            rabbitmq-configuration
            rabbitmq-configuration?
            rabbitmq-configuration-rabbitmq
            rabbitmq-configuration-node-name
            rabbitmq-configuration-config-file
            rabbitmq-configuration-env-config-file
            rabbitmq-configuration-plugins
            rabbitmq-service-type))

(define-configuration/no-serialization epmd-configuration
  (user (string "epmd") "The user running epmd.")
  (group (string "epmd") "The user group running epmd.")
  ;; The address default works for IPv4-only or IPv6-only systems. For example,
  ;; epmd shows warning for ::1 on IPv4-only system, but starts anyway.
  (address (list-of-strings (list "127.0.0.1" "::1"))
           "List of addresses for epmd to listen on.  Set it to false
(@code{#f}) to listen on all available addresses.  Note that epmd cannot start
on @code{\"0.0.0.0\"} address.")
  (port (integer 4369) "Default port for epmd to listen on."))

(define (epmd-accounts config)
  (match-record config <epmd-configuration> (user group)
    (list (user-group (name group)
                      (system? #t))
          (user-account (name user)
                        (group group)
                        (system? #t)
                        (comment "Erlang Port Mapper Daemon user")
                        (home-directory "/var/empty")
                        (shell (file-append shadow "/sbin/nologin"))))))

(define (epmd-shepherd-service config)
  (match-record config <epmd-configuration>
    (address port)
    (let ((address-list (if address (string-join address ",") #f)))
      (with-imported-modules
        (source-module-closure '((gnu build shepherd)))
        (list
          (shepherd-service
            (provision '(epmd))
            (documentation "Run the Erlang Port Mapper Daemon (epmd).")
            (requirement '(user-processes loopback))
            (modules '((gnu build shepherd)))
            (start
             #~(make-forkexec-constructor
                 (append (list #$(file-append erlang "/bin/epmd")
                               "-port"
                               #$(number->string port))
                         (if #$address-list
                           (list "-address" #$address-list)
                           (list)))
                 #:user "epmd"
                 #:group "epmd"))
            (stop #~(make-kill-destructor))))))))

(define epmd-service-type
  (service-type
    (name 'epmd)
    (description "Run the Erlang Port Mapper Daemon (epmd).")
    (extensions (list (service-extension shepherd-root-service-type
                                         epmd-shepherd-service)
                      (service-extension account-service-type epmd-accounts)))
    (default-value (epmd-configuration))))

;; By default, start messaging and inter-node RabbitMQ listeners on local
;; interfaces only, see also:
;;
;;   https://www.rabbitmq.com/docs/networking
;;
;; NOTE: Enabling a RabbitMQ plugin will make it usually listen on a public
;; interface.
(define %default-rabbitmq-config-file
  (plain-file "rabbitmq.conf" "
listeners.tcp.1 = 127.0.0.1:5672
listeners.tcp.2 = ::1:5672

distribution.listener.interface = 127.0.0.1
"))

(define-record-type* <rabbitmq-configuration> rabbitmq-configuration
                     make-rabbitmq-configuration
  rabbitmq-configuration?
  (rabbitmq rabbitmq-configuration-rabbitmq
            (default rabbitmq))
  (node-name rabbitmq-configuration-node-name
             (default "rabbit@localhost"))
  (config-file rabbitmq-configuration-config-file
               (default %default-rabbitmq-config-file))
  (env-config-file rabbitmq-configuration-env-file (default #f))
  ;; It can be a mnesia database or a khepri database, so use "data" instead
  ;; of the traditional "mnesia".
  (data-directory rabbitmq-configuration-data-directory
                  (default "/var/lib/rabbitmq/data"))
  (plugins rabbitmq-configuration-plugins
           (default '())))

(define %rabbitmq-accounts
  (list (user-group
          (name "rabbitmq")
          (system? #t))
        (user-account
          (name "rabbitmq")
          (group "rabbitmq")
          (system? #t)
          (comment "RabbitMQ server user")
          (home-directory "/var/lib/rabbitmq")
          (shell (file-append shadow "/sbin/nologin")))))

(define (rabbitmq-activation config)
  (let* ((data-directory (rabbitmq-configuration-data-directory config))
         (plugins (string-join (rabbitmq-configuration-plugins config) ",")))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (let ((user (getpwnam "rabbitmq"))
                (srv-directories (list
                                  "/var/lib/rabbitmq"
                                  "/var/log/rabbitmq"
                                  "/var/run/rabbitmq"
                                  #$data-directory)))
            (for-each (lambda (directory)
                        (mkdir-p directory)
                        (chown directory
                               (passwd:uid user)
                               (passwd:gid user)))
                      srv-directories)

            ;; Create file with the enabled plugins.
            (with-output-to-file (string-append #$data-directory
                                  "/enabled_plugins")
              (lambda () (display (format #f "[~a]." #$plugins))))
            (chown (string-append #$data-directory "/enabled_plugins")
                   (passwd:uid user)
                   (passwd:gid user)))))))

(define (rabbitmq-shepherd-service config)
  (match-record config <rabbitmq-configuration>
    (rabbitmq node-name data-directory config-file env-config-file plugins)
    (with-imported-modules
      (source-module-closure '((gnu build shepherd)))
      (list
        (shepherd-service
          (provision '(rabbitmq))
          (documentation "Run the RabbitMQ daemon.")
          (requirement '(user-processes loopback))
          (modules '((gnu build shepherd)))
          (start
           #~(make-forkexec-constructor
              `(#$(file-append rabbitmq "/sbin/rabbitmq-server"))
              #:pid-file "/var/run/rabbitmq/pid"
              #:user "rabbitmq"
              #:group "rabbitmq"
              #:environment-variables
              (append
                (if #$env-config-file
                  (list (string-append "RABBITMQ_CONF_ENV_FILE="
                                       #$env-config-file))
                  (list))
                (list
                  (string-append "RABBITMQ_NODENAME=" #$node-name)
                  (string-append "RABBITMQ_CONFIG_FILE=" #$config-file)
                  "RABBITMQ_PID_FILE=/var/run/rabbitmq/pid"
                  (string-append
                    "RABBITMQ_ENABLED_PLUGINS_FILE="
                    #$data-directory
                    "/enabled_plugins")
                  (string-append
                    "RABBITMQ_MNESIA_BASE="
                    #$data-directory)
                  "RABBITMQ_LOG_BASE=/var/log/rabbitmq")
                (environ))))
          (stop #~(make-kill-destructor)))))))

(define rabbitmq-service-type
  (service-type
    (name 'rabbitmq)
    (description "Run the RabbitMQ message broker service.")
    (extensions (list (service-extension
                       shepherd-root-service-type
                       rabbitmq-shepherd-service)
                      (service-extension activation-service-type
                                         rabbitmq-activation)
                      (service-extension account-service-type
                                         (const %rabbitmq-accounts))
                      (service-extension
                       profile-service-type
                       (compose list rabbitmq-configuration-rabbitmq))))
    (default-value (rabbitmq-configuration))))
