;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 ng0 <ng0@we.make.ritual.n0.is>
;;; Copyright © 2016, 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Christopher Baines <mail@cbaines.net>
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

(define-module (gnu services web)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages web)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (nginx-configuration
            nginx-configuration?
            nginx-server-configuration
            nginx-server-configuration?
            nginx-upstream-configuration
            nginx-upstream-configuration?
            nginx-location-configuration
            nginx-location-configuration?
            nginx-named-location-configuration
            nginx-named-location-configuration?
            nginx-service
            nginx-service-type))

;;; Commentary:
;;;
;;; Web services.
;;;
;;; Code:

(define-record-type* <nginx-server-configuration>
  nginx-server-configuration make-nginx-server-configuration
  nginx-server-configuration?
  (http-port           nginx-server-configuration-http-port
                       (default 80))
  (https-port          nginx-server-configuration-https-port
                       (default 443))
  (server-name         nginx-server-configuration-server-name
                       (default (list 'default)))
  (root                nginx-server-configuration-root
                       (default "/srv/http"))
  (locations           nginx-server-configuration-locations
                       (default '()))
  (index               nginx-server-configuration-index
                       (default (list "index.html")))
  (ssl-certificate     nginx-server-configuration-ssl-certificate
                       (default "/etc/nginx/cert.pem"))
  (ssl-certificate-key nginx-server-configuration-ssl-certificate-key
                       (default "/etc/nginx/key.pem"))
  (server-tokens?      nginx-server-configuration-server-tokens?
                       (default #f)))

(define-record-type* <nginx-upstream-configuration>
  nginx-upstream-configuration make-nginx-upstream-configuration
  nginx-upstream-configuration?
  (name                nginx-upstream-configuration-name)
  (servers             nginx-upstream-configuration-servers))

(define-record-type* <nginx-location-configuration>
  nginx-location-configuration make-nginx-location-configuration
  nginx-location-configuration?
  (uri                 nginx-location-configuration-uri
                       (default #f))
  (body                nginx-location-configuration-body))

(define-record-type* <nginx-named-location-configuration>
  nginx-named-location-configuration make-nginx-named-location-configuration
  nginx-named-location-configuration?
  (name                nginx-named-location-configuration-name
                       (default #f))
  (body                nginx-named-location-configuration-body))

(define-record-type* <nginx-configuration>
  nginx-configuration make-nginx-configuration
  nginx-configuration?
  (nginx         nginx-configuration-nginx          ;<package>
                 (default nginx))
  (log-directory nginx-configuration-log-directory  ;string
                 (default "/var/log/nginx"))
  (run-directory nginx-configuration-run-directory  ;string
                 (default "/var/run/nginx"))
  (server-blocks nginx-configuration-server-blocks
                 (default '()))          ;list of <nginx-server-configuration>
  (upstream-blocks nginx-configuration-upstream-blocks
                   (default '()))      ;list of <nginx-upstream-configuration>
  (file          nginx-configuration-file         ;#f | string | file-like
                 (default #f)))

(define (config-domain-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of domain names."
 (string-join
  (map (match-lambda
        ('default "_ ")
        ((? string? str) (string-append str " ")))
       names)))

(define (config-index-strings names)
 "Return a string denoting the nginx config representation of NAMES, a list
of index files."
 (string-join
  (map (match-lambda
        ((? string? str) (string-append str " ")))
       names)))

(define nginx-location-config
  (match-lambda
    (($ <nginx-location-configuration> uri body)
     (string-append
      "      location " uri " {\n"
      "        " (string-join body "\n    ") "\n"
      "      }\n"))
    (($ <nginx-named-location-configuration> name body)
     (string-append
      "      location @" name " {\n"
      "        " (string-join body "\n    ") "\n"
      "      }\n"))))

(define (default-nginx-server-config server)
  (string-append
   "    server {\n"
   (if (nginx-server-configuration-http-port server)
       (string-append "      listen "
                      (number->string (nginx-server-configuration-http-port server))
                      ";\n")
       "")
   (if (nginx-server-configuration-https-port server)
       (string-append "      listen "
                      (number->string (nginx-server-configuration-https-port server))
                      " ssl;\n")
       "")
   "      server_name " (config-domain-strings
                         (nginx-server-configuration-server-name server))
                        ";\n"
   (if (nginx-server-configuration-ssl-certificate server)
       (let ((certificate (nginx-server-configuration-ssl-certificate server)))
         ;; lstat fails when the certificate file does not exist: it aborts
         ;; and lets the user fix their configuration.
         (lstat certificate)
         (string-append "      ssl_certificate " certificate ";\n"))
       "")
   (if (nginx-server-configuration-ssl-certificate-key server)
       (let ((key (nginx-server-configuration-ssl-certificate-key server)))
         (lstat certificate)
         (string-append "      ssl_certificate_key " key ";\n"))
       "")
   "      root " (nginx-server-configuration-root server) ";\n"
   "      index " (config-index-strings (nginx-server-configuration-index server)) ";\n"
   "      server_tokens " (if (nginx-server-configuration-server-tokens? server)
                              "on" "off") ";\n"
   "\n"
   (string-join
    (map nginx-location-config (nginx-server-configuration-locations server))
    "\n")
   "    }\n"))

(define (nginx-upstream-config upstream)
  (string-append
   "    upstream " (nginx-upstream-configuration-name upstream) " {\n"
   (string-concatenate
    (map (lambda (server)
           (simple-format #f "      server ~A;\n" server))
         (nginx-upstream-configuration-servers upstream)))
   "    }\n"))

(define (default-nginx-config nginx log-directory run-directory server-list upstream-list)
  (mixed-text-file "nginx.conf"
               "user nginx nginx;\n"
               "pid " run-directory "/pid;\n"
               "error_log " log-directory "/error.log info;\n"
               "http {\n"
               "    client_body_temp_path " run-directory "/client_body_temp;\n"
               "    proxy_temp_path " run-directory "/proxy_temp;\n"
               "    fastcgi_temp_path " run-directory "/fastcgi_temp;\n"
               "    uwsgi_temp_path " run-directory "/uwsgi_temp;\n"
               "    scgi_temp_path " run-directory "/scgi_temp;\n"
               "    access_log " log-directory "/access.log;\n"
               "    include " nginx "/share/nginx/conf/mime.types;\n"
               "\n"
               (string-join
                (filter (lambda (section) (not (null? section)))
                        (map nginx-upstream-config upstream-list))
                "\n")
               "\n"
               (let ((http (map default-nginx-server-config server-list)))
                 (do ((http http (cdr http))
                      (block "" (string-append (car http) "\n" block )))
                     ((null? http) block)))
               "}\n"
               "events {}\n"))

(define %nginx-accounts
  (list (user-group (name "nginx") (system? #t))
        (user-account
         (name "nginx")
         (group "nginx")
         (system? #t)
         (comment "nginx server user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define nginx-activation
  (match-lambda
    (($ <nginx-configuration> nginx log-directory run-directory server-blocks
                              upstream-blocks config-file)
     #~(begin
         (use-modules (guix build utils))

         (format #t "creating nginx log directory '~a'~%" #$log-directory)
         (mkdir-p #$log-directory)
         (format #t "creating nginx run directory '~a'~%" #$run-directory)
         (mkdir-p #$run-directory)
         (format #t "creating nginx temp directories '~a/{client_body,proxy,fastcgi,uwsgi,scgi}_temp'~%" #$run-directory)
         (mkdir-p (string-append #$run-directory "/client_body_temp"))
         (mkdir-p (string-append #$run-directory "/proxy_temp"))
         (mkdir-p (string-append #$run-directory "/fastcgi_temp"))
         (mkdir-p (string-append #$run-directory "/uwsgi_temp"))
         (mkdir-p (string-append #$run-directory "/scgi_temp"))
         ;; Start-up logs. Once configuration is loaded, nginx switches to
         ;; log-directory.
         (mkdir-p (string-append #$run-directory "/logs"))
         ;; Check configuration file syntax.
         (system* (string-append #$nginx "/sbin/nginx")
                  "-c" #$(or config-file
                             (default-nginx-config nginx log-directory
                               run-directory server-blocks upstream-blocks))
                  "-t")))))

(define nginx-shepherd-service
  (match-lambda
    (($ <nginx-configuration> nginx log-directory run-directory server-blocks
                              upstream-blocks config-file)
     (let* ((nginx-binary (file-append nginx "/sbin/nginx"))
            (nginx-action
             (lambda args
               #~(lambda _
                   (zero?
                    (system* #$nginx-binary "-c"
                             #$(or config-file
                                   (default-nginx-config nginx log-directory
                                     run-directory server-blocks upstream-blocks))
                             #$@args))))))

       ;; TODO: Add 'reload' action.
       (list (shepherd-service
              (provision '(nginx))
              (documentation "Run the nginx daemon.")
              (requirement '(user-processes loopback))
              (start (nginx-action "-p" run-directory))
              (stop (nginx-action "-s" "stop"))))))))

(define nginx-service-type
  (service-type (name 'nginx)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          nginx-shepherd-service)
                       (service-extension activation-service-type
                                          nginx-activation)
                       (service-extension account-service-type
                                          (const %nginx-accounts))))
                (compose concatenate)
                (extend (lambda (config servers)
                          (nginx-configuration
                            (inherit config)
                            (server-blocks
                              (append (nginx-configuration-server-blocks config)
                              servers)))))))

(define* (nginx-service #:key (nginx nginx)
                        (log-directory "/var/log/nginx")
                        (run-directory "/var/run/nginx")
                        (server-list '())
                        (upstream-list '())
                        (config-file #f))
  "Return a service that runs NGINX, the nginx web server.

The nginx daemon loads its runtime configuration from CONFIG-FILE, stores log
files in LOG-DIRECTORY, and stores temporary runtime files in RUN-DIRECTORY."
  (service nginx-service-type
           (nginx-configuration
            (nginx nginx)
            (log-directory log-directory)
            (run-directory run-directory)
            (server-blocks server-list)
            (upstream-blocks upstream-list)
            (file config-file))))
