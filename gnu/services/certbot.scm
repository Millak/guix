;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2024 Carlo Zancanaro <carlo@zancanaro.id.au>
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

(define-module (gnu services certbot)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages tls)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (certbot-service-type
            certbot-configuration
            certbot-configuration?
            certificate-configuration))

;;; Commentary:
;;;
;;; Automatically obtaining TLS certificates from Let's Encrypt.
;;;
;;; Code:


(define-record-type* <certificate-configuration>
  certificate-configuration make-certificate-configuration
  certificate-configuration?
  (name                certificate-configuration-name
                       (default #f))
  (domains             certificate-configuration-domains
                       (default '()))
  (challenge           certificate-configuration-challenge
                       (default #f))
  (csr                 certificate-configuration-csr
                       (default #f))
  (authentication-hook certificate-authentication-hook
                       (default #f))
  (cleanup-hook        certificate-cleanup-hook
                       (default #f))
  (deploy-hook         certificate-configuration-deploy-hook
                       (default #f))
  (start-self-signed?  certificate-configuration-start-self-signed?
                       (default #t)))

(define-record-type* <certbot-configuration>
  certbot-configuration make-certbot-configuration
  certbot-configuration?
  (package             certbot-configuration-package
                       (default certbot))
  (webroot             certbot-configuration-webroot
                       (default "/var/www"))
  (certificates        certbot-configuration-certificates
                       (default '()))
  (email               certbot-configuration-email
                       (default #f))
  (server              certbot-configuration-server
                       (default #f))
  (rsa-key-size        certbot-configuration-rsa-key-size
                       (default #f))
  (default-location    certbot-configuration-default-location
                       (default
                         (nginx-location-configuration
                          (uri "/")
                          (body
                           (list "return 301 https://$host$request_uri;"))))))

(define (certbot-deploy-hook name deploy-hook-script)
  "Returns a gexp which creates symlinks for privkey.pem and fullchain.pem
from /etc/certs/NAME to /etc/letsenctypt/live/NAME.  If DEPLOY-HOOK-SCRIPT is
not #f then it is run after the symlinks have been created.  This wrapping is
necessary for certificates with start-self-signed? set to #t, as it will
overwrite the initial self-signed certificates upon the first successful
deploy."
  (program-file
   (string-append name "-deploy-hook")
   (with-imported-modules '((gnu services herd)
                            (guix build utils))
     #~(begin
         (use-modules (gnu services herd)
                      (guix build utils))
         (mkdir-p #$(string-append "/etc/certs/" name))
         (chmod #$(string-append "/etc/certs/" name) #o755)

         ;; Create new symlinks
         (symlink #$(string-append
                     "/etc/letsencrypt/live/" name "/privkey.pem")
                  #$(string-append "/etc/certs/" name "/privkey.pem.new"))
         (symlink #$(string-append
                     "/etc/letsencrypt/live/" name "/fullchain.pem")
                  #$(string-append "/etc/certs/" name "/fullchain.pem.new"))

         ;; Rename over the top of the old ones, just in case they were the
         ;; original self-signed certificates.
         (rename-file #$(string-append "/etc/certs/" name "/privkey.pem.new")
                      #$(string-append "/etc/certs/" name "/privkey.pem"))
         (rename-file #$(string-append "/etc/certs/" name "/fullchain.pem.new")
                      #$(string-append "/etc/certs/" name "/fullchain.pem"))

         ;; With the new certificates in place, tell nginx to reload them.
         (with-shepherd-action 'nginx ('reload) result result)

         #$@(if deploy-hook-script
                (list #~(invoke #$deploy-hook-script))
                '())))))

(define certbot-command
  (match-lambda
    (($ <certbot-configuration> package webroot certificates email
                                server rsa-key-size default-location)
     (let* ((certbot (file-append package "/bin/certbot"))
            (rsa-key-size (and rsa-key-size (number->string rsa-key-size)))
            (commands
             (map
              (match-lambda
                (($ <certificate-configuration> custom-name domains challenge
                                                csr authentication-hook
                                                cleanup-hook deploy-hook)
                 (let ((name (or custom-name (car domains))))
                   (if challenge
                     (append
                      (list name certbot "certonly" "-n" "--agree-tos"
                            "--manual"
                            (string-append "--preferred-challenges=" challenge)
                            "--cert-name" name
                            "--manual-public-ip-logging-ok"
                            "-d" (string-join domains ","))
                      (if csr `("--csr" ,csr) '())
                      (if email
                          `("--email" ,email)
                          '("--register-unsafely-without-email"))
                      (if server `("--server" ,server) '())
                      (if rsa-key-size `("--rsa-key-size" ,rsa-key-size) '())
                      (if authentication-hook
                          `("--manual-auth-hook" ,authentication-hook)
                          '())
                      (if cleanup-hook `("--manual-cleanup-hook" ,cleanup-hook) '())
                      (list "--deploy-hook"
                            (certbot-deploy-hook name deploy-hook)))
                     (append
                      (list name certbot "certonly" "-n" "--agree-tos"
                            "--webroot" "-w" webroot
                            "--cert-name" name
                            "-d" (string-join domains ","))
                      (if csr `("--csr" ,csr) '())
                      (if email
                          `("--email" ,email)
                          '("--register-unsafely-without-email"))
                      (if server `("--server" ,server) '())
                      (if rsa-key-size `("--rsa-key-size" ,rsa-key-size) '())
                      (list "--deploy-hook"
                            (certbot-deploy-hook name deploy-hook)))))))
              certificates)))
       (program-file
        "certbot-command"
        #~(begin
            (use-modules (ice-9 match)
                         (ice-9 textual-ports))

            (define (log format-string . args)
              (apply format #t format-string args)
              (force-output))

            (define (file-contains? file string)
              (string-contains (call-with-input-file file
                                 get-string-all)
                               string))

            (define (connection-error?)
              ;; Certbot errors are always exit code 1, so we need to look at
              ;; the log file to see if there was a connection error.
              (file-contains? "/var/log/letsencrypt/letsencrypt.log"
                              "Failed to establish a new connection"))

            (let ((script-code 0))
              (for-each
               (match-lambda
                 ((name . command)
                  (log "Acquiring or renewing certificate: ~a~%" name)
                  (cond
                   ((zero? (status:exit-val (apply system* command)))
                    (log "Certificate successfully acquired: ~a~%" name))
                   ((connection-error?)
                    ;; If we have a connection error, then bail early with
                    ;; exit code 2. We don't expect this to resolve within the
                    ;; timespan of this script.
                    (log "Connection error - bailing out~%")
                    (exit 2))
                   (else
                    ;; If we have any other type of error, then continue but
                    ;; exit with a failing status code in the end.
                    (log "Error: ~a - continuing with other domains~%" name)
                    (set! script-code 1)))))
               '#$commands)
              (exit script-code))))))))

(define (certbot-renewal-shepherd-services config)
  (list (shepherd-service
         (provision '(certbot-certificate-renewal))
         (requirement '(user-processes nginx))
         (modules '((shepherd service timer)))
         (start #~(make-timer-constructor
                   ;; Attempt to renew the certificates twice per day.  See
                   ;; https://eff-certbot.readthedocs.io/.
                   (calendar-event #:minutes '(22) #:hours '(0 12))
                   (command '(#$(certbot-command config)))
                   #:wait-for-termination? #t))
         (stop #~(make-timer-destructor))
         (documentation "Periodically run the 'certbot' command to renew X.509
certificates.")
         (actions
          (list shepherd-trigger-action
                (shepherd-configuration-action (certbot-command config)))))

        ;; Renew certificates when the system first starts. This is a one-shot
        ;; service, because the timer above takes care of running this
        ;; periodically.  This is most useful the very first time the system
        ;; starts, to overwrite our self-signed certificates as soon as
        ;; possible without user intervention.
        (shepherd-service
         (provision '(renew-certbot-certificates))
         (requirement '(user-processes nginx))
         (one-shot? #t)
         (start #~(lambda _
                    ;; This needs the network, but there's no reliable way to know
                    ;; if the network is up other than trying. If we fail due to a
                    ;; connection error we retry a number of times in the hope that
                    ;; the network comes up soon.
                    (let loop ((attempt 0))
                      (let ((code (status:exit-val
                                   (system* #$(certbot-command config)))))
                        (cond
                         ((and (= code 2) ; Exit code 2 means connection error
                               (< attempt 12)) ; Arbitrarily chosen max attempts
                          (sleep 10)          ; Arbitrarily chosen retry delay
                          (loop (1+ attempt)))
                         ((zero? code)
                          ;; Success!
                          #t)
                         (else
                          ;; Failure.
                          #f))))))
         (auto-start? #t)
         (documentation "Run 'certbot' to renew certificates at boot time.")
         (actions
          (list (shepherd-configuration-action (certbot-command config)))))))

(define (generate-certificate-gexp certbot-cert-directory rsa-key-size)
  (match-lambda
    (($ <certificate-configuration> name (primary-domain other-domains ...)
                                    challenge
                                    csr authentication-hook
                                    cleanup-hook deploy-hook)
     (let (;; Arbitrary default subject, with just the
           ;; right domain filled in. These values don't
           ;; have any real significance.
           (subject (string-append
                     "/C=US/ST=Oregon/L=Portland/O=Company Name/OU=Org/CN="
                     primary-domain))
           (alt-names (if (null? other-domains)
                          #f
                          (format #f "subjectAltName=~{DNS:~a~^,~}"
                                  other-domains)))
           (directory (string-append "/etc/certs/" (or name primary-domain))))
       #~(when (not (file-exists? #$directory))
           ;; We generate self-signed certificates in /etc/certs/{domain},
           ;; because certbot is very sensitive to its directory
           ;; structure. It refuses to write over the top of existing files,
           ;; so we need to use a directory outside of its control.
           ;;
           ;; These certificates are overwritten by the certbot deploy hook
           ;; the first time it successfully obtains a letsencrypt-signed
           ;; certificate.
           (mkdir-p #$directory)
           (chmod #$directory #o755)
           (invoke #$(file-append openssl "/bin/openssl")
                   "req" "-x509"
                   "-newkey" #$(string-append "rsa:" (or rsa-key-size "4096"))
                   "-keyout" #$(string-append directory "/privkey.pem")
                   "-out" #$(string-append directory "/fullchain.pem")
                   "-sha256"
                   "-days" "1" ; Only one day, because we expect certbot to run
                   "-nodes"
                   "-subj" #$subject
                   #$@(if alt-names
                          (list "-addext" alt-names)
                          (list))))))))

(define (certbot-activation config)
  (let* ((certbot-directory "/var/lib/certbot")
         (certbot-cert-directory "/etc/letsencrypt/live"))
    (match config
      (($ <certbot-configuration> package webroot certificates email
                                  server rsa-key-size default-location)
       (with-imported-modules '((guix build utils))
         #~(begin
             (use-modules (guix build utils))
             (mkdir-p #$webroot)
             (mkdir-p #$certbot-directory)
             (mkdir-p #$certbot-cert-directory)

             #$@(let ((rsa-key-size (and rsa-key-size
                                         (number->string rsa-key-size))))
                  (map (generate-certificate-gexp certbot-cert-directory
                                                  rsa-key-size)
                       (filter certificate-configuration-start-self-signed?
                               certificates)))))))))

(define certbot-nginx-server-configurations
  (match-lambda
    (($ <certbot-configuration> package webroot certificates email
                                server rsa-key-size default-location)
     (define (certificate->nginx-server certificate-configuration)
       (match-record certificate-configuration <certificate-configuration> 
         (domains challenge)
         (nginx-server-configuration
          (listen '("80" "[::]:80"))
          (ssl-certificate #f)
          (ssl-certificate-key #f)
          (server-name domains)
          (locations
           (filter identity
                   (append
                    (if challenge
                      '()
                      (list (nginx-location-configuration
                             (uri "/.well-known")
                             (body (list (list "root " webroot ";"))))))
                    (list default-location)))))))
     (map certificate->nginx-server certificates))))

(define certbot-service-type
  (service-type (name 'certbot)
                (extensions
                 (list (service-extension nginx-service-type
                                          certbot-nginx-server-configurations)
                       (service-extension profile-service-type
                                          (compose list certbot-configuration-package))
                       (service-extension activation-service-type
                                          certbot-activation)
                       (service-extension shepherd-root-service-type
                                          certbot-renewal-shepherd-services)))
                (compose concatenate)
                (extend (lambda (config additional-certificates)
                          (certbot-configuration
                           (inherit config)
                           (certificates
                            (append
                             (certbot-configuration-certificates config)
                             additional-certificates)))))
                (description
                 "Automatically renew @url{https://letsencrypt.org, Let's
Encrypt} HTTPS certificates by adjusting the nginx web server configuration
and periodically invoking @command{certbot}.")))
