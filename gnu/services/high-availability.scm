;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2025 Artur Wroblewski <wrobell@riseup.net>
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
  #:use-module (gnu packages high-availability)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)

  #:export (rabbitmq-configuration rabbitmq-configuration?
                                   rabbitmq-configuration-rabbitmq
                                   rabbitmq-configuration-config-file
                                   rabbitmq-configuration-plugins
                                   rabbitmq-service-type))

;; By default, start on local ipv4 and ipv6 interfaces only, see also:
;;
;;   https://www.rabbitmq.com/docs/networking
;;
;; NOTE: How to enable plugins to listen on localhost only?
(define %default-rabbitmq-config-file
  (plain-file "rabbitmq.conf" "
listeners.tcp.1 = 127.0.0.1:5672
listeners.tcp.2 = ::1:5672
"))

(define-record-type* <rabbitmq-configuration> rabbitmq-configuration
                     make-rabbitmq-configuration
  rabbitmq-configuration?
  (rabbitmq rabbitmq-configuration-rabbitmq
            (default rabbitmq))
  (config-file rabbitmq-configuration-config-file
               (default %default-rabbitmq-config-file))
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
    (rabbitmq data-directory config-file plugins)
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
                (list
                  (string-append "RABBITMQ_CONFIG_FILE=" #$config-file)
                  "RABBITMQ_PID_FILE=/var/run/rabbitmq/pid"
                  "RABBITMQ_CONF_ENV_FILE=/run/current-system/profile/etc/rabbitmq/rabbitmq-env.conf"
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
  (service-type (name 'rabbitmq)
                (description "Run the RabbitMQ message broker service.")
                (extensions (list (service-extension
                                   shepherd-root-service-type
                                   rabbitmq-shepherd-service)
                                  (service-extension activation-service-type
                                                     rabbitmq-activation)
                                  (service-extension account-service-type
                                                     (const %rabbitmq-accounts))))
                (default-value (rabbitmq-configuration))))
