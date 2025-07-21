;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2020-2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <pierre-antoine.rouby@inria.fr>
;;; Copyright © 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2024 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu tests web)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services web)
  #:use-module (gnu services databases)
  #:use-module (gnu services getmail)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services mail)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages patchutils)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (guix packages)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:export (%test-httpd
            %test-nginx
            %test-nginx+anonip
            %test-varnish
            %test-php-fpm
            %test-hpcguix-web
            %test-tailon
            %test-anonip
            %test-patchwork
            %test-agate))

(define %index.html-contents
  ;; Contents of the /index.html file.
  "Hello, guix!")

(define %make-http-root
  ;; Create our server root in /srv.
  #~(begin
      (mkdir "/srv")
      (mkdir "/srv/http")
      (call-with-output-file "/srv/http/index.html"
        (lambda (port)
          (display #$%index.html-contents port)))))

(define retry-on-error
  #~(lambda* (f #:key times delay)
      (let loop ((attempt 1))
        (match (catch
                 #t
                 (lambda ()
                   (cons #t
                         (f)))
                 (lambda args
                   (cons #f
                         args)))
          ((#t . return-value)
           return-value)
          ((#f . error-args)
           (if (>= attempt times)
               (apply throw error-args)
               (begin
                 (sleep delay)
                 (loop (+ 1 attempt)))))))))

(define* (run-webserver-test name test-os #:key (log-file #f) (http-port 8080)
                             extra-tests)
  "Run tests in %NGINX-OS, which has nginx running and listening on HTTP-PORT.
EXTRA-TESTS should be a sexp of gexp containing extra code to run as part of
the tests."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define forwarded-port 8080)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,http-port . ,forwarded-port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin #$name)

          (test-assert #$(string-append name " service running")
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service '#$(string->symbol name))
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) pid)))))
             marionette))

          (test-assert "HTTP port ready"
            (wait-for-tcp-port #$forwarded-port marionette))

          ;; Retrieve the index.html file we put in /srv.
          (test-equal "http-get"
            '(200 #$%index.html-contents)
            (let-values
                (((response text)
                  (http-get #$(simple-format
                               #f "http://localhost:~A/index.html" forwarded-port)
                            #:decode-body? #t)))
              (list (response-code response) text)))

          #$@(if log-file
                 `((test-assert ,(string-append "log file exists " log-file)
                     (marionette-eval
                      '(file-exists? ,log-file)
                      marionette)))
                 '())

          #$extra-tests

          (test-end))))

  (gexp->derivation (string-append name "-test") test))


;;;
;;; HTTPD
;;;

(define %httpd-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service httpd-service-type
            (httpd-configuration
             (config
              (httpd-config-file
               (listen '("8080"))))))
   (simple-service 'make-http-root activation-service-type
                   %make-http-root)))

(define %test-httpd
  (system-test
   (name "httpd")
   (description "Connect to a running HTTPD server.")
   (value (run-webserver-test name %httpd-os
                              #:log-file "/var/log/httpd/error_log"))))


;;;
;;; NGINX
;;;

(define %nginx-servers
  ;; Server blocks.
  (list (nginx-server-configuration
         (listen '("8080")))))

(define %nginx-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcpcd-service-type)
   (service nginx-service-type
            (nginx-configuration
             (log-directory "/var/log/nginx")
             (server-blocks %nginx-servers)))
   (simple-service 'make-http-root activation-service-type
                   %make-http-root)))

(define %test-nginx
  (system-test
   (name "nginx")
   (description "Connect to a running NGINX server.")
   (value (run-webserver-test name %nginx-os
                              #:log-file "/var/log/nginx/access.log"))))

(define %nginx+anonip-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service anonip-service-type
            (anonip-configuration
             (input "/var/run/anonip/access.log")
             (output "/var/log/anonip/access.log")
             (debug? #t)))
   (service nginx-service-type
            (nginx-configuration
             (log-directory "/var/run/anonip/")
             (server-blocks %nginx-servers)
             (shepherd-requirement '(anonip-/var/log/anonip/access.log))))
   (simple-service 'make-http-root activation-service-type
                   %make-http-root)))

(define nginx-anonip-tests
  #~(test-assert "anonip service is running"
      (marionette-eval
       '(begin
          (use-modules (gnu services herd))
          (wait-for-service 'anonip-/var/log/anonip/access.log))
       marionette)))

(define %test-nginx+anonip
  (system-test
   (name "nginx+anonip")
   (description "Run a NGINX server with logs anonymized by Anonip")
   (value (run-webserver-test "nginx" %nginx+anonip-os
                              #:log-file "/var/log/anonip/access.log"
                              #:extra-tests nginx-anonip-tests))))


;;;
;;; Varnish
;;;

(define %varnish-vcl
  (mixed-text-file
   "varnish-test.vcl"
   "vcl 4.0;
backend dummy { .host = \"127.1.1.1\"; }
sub vcl_recv { return(synth(200, \"OK\")); }
sub vcl_synth {
  synthetic(\"" %index.html-contents "\");
  set resp.http.Content-Type = \"text/plain\";
  return(deliver);
}"))

(define %varnish-os
  (simple-operating-system
   (service dhcpcd-service-type)
   ;; Pretend to be a web server that serves %index.html-contents.
   (service varnish-service-type
            (varnish-configuration
             (name "/tmp/server")
             ;; Use a small VSL buffer to fit in the test VM.
             (parameters '(("vsl_space" . "4M")))
             (vcl %varnish-vcl)))
   ;; Proxy the "server" using the builtin configuration.
   (service varnish-service-type
            (varnish-configuration
             (parameters '(("vsl_space" . "4M")))
             (backend "localhost:80")
             (listen '(":8080"))))))

(define %test-varnish
  (system-test
   (name "varnish")
   (description "Test the Varnish Cache server.")
   (value (run-webserver-test "varnish-default" %varnish-os))))


;;;
;;; PHP-FPM
;;;

(define %make-php-fpm-http-root
  ;; Create our server root in /srv.
  #~(begin
      (mkdir "/srv")
      (call-with-output-file "/srv/index.php"
        (lambda (port)
          (display "<?php
phpinfo();
echo(\"Computed by php:\".((string)(2+3)));
?>\n" port)))))

(define %php-fpm-nginx-server-blocks
  (list (nginx-server-configuration
         (root "/srv")
         (locations
          (list (nginx-php-location)))
         (listen '("8042"))
         (ssl-certificate #f)
         (ssl-certificate-key #f))))

(define %php-fpm-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcpcd-service-type)
   (service php-fpm-service-type)
   (service nginx-service-type
            (nginx-configuration
             (server-blocks %php-fpm-nginx-server-blocks)))
   (simple-service 'make-http-root activation-service-type
                   %make-php-fpm-http-root)))

(define* (run-php-fpm-test #:optional (http-port 8042))
  "Run tests in %PHP-FPM-OS, which has nginx running and listening on
HTTP-PORT, along with php-fpm."
  (define os
    (marionette-operating-system
     %php-fpm-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,http-port)))))

  (define test
    (with-imported-modules '((gnu build marionette)
                             (guix build utils))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "php-fpm")

          (test-assert "php-fpm running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'php-fpm)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-assert "nginx running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nginx))
             marionette))

          (test-equal "http-get"
            200
            (let-values (((response text)
                          (http-get "http://localhost:8080/index.php"
                                    #:decode-body? #t)))
              (response-code response)))

          (test-equal "php computed result is sent"
            "Computed by php:5"
            (let-values (((response text)
                          (http-get "http://localhost:8080/index.php"
                                    #:decode-body? #t)))
              (begin
                (use-modules (ice-9 regex))
                (let ((matches (string-match "Computed by php:5" text)))
                  (and matches
                       (match:substring matches 0))))))

          (test-end))))

  (gexp->derivation "php-fpm-test" test))

(define %test-php-fpm
  (system-test
   (name "php-fpm")
   (description "Test PHP-FPM through nginx.")
   (value (run-php-fpm-test))))


;;;
;;; hpcguix-web
;;;

(define* (run-hpcguix-web-server-test name test-os)
  "Run tests in %HPCGUIX-WEB-OS, which has hpcguix-web running."
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings '((8080 . 5000)))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (ice-9 match)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin #$name)

          (test-assert "hpcguix-web running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'hpcguix-web)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((pid) pid)))))
             marionette))

          (test-equal "http-get"
            200
            (begin
              (wait-for-tcp-port 5000 marionette)
              (#$retry-on-error
               (lambda ()
                 (let-values (((response text)
                               (http-get "http://localhost:8080")))
                   (response-code response)))
               #:times 10
               #:delay 5)))

          (test-end))))

  (gexp->derivation (string-append name "-test") test))

(define %hpcguix-web-specs
  ;; Server config gexp.
  #~(hpcweb-configuration
     (title-prefix "[TEST] HPCGUIX-WEB")))

(define %hpcguix-web-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (service hpcguix-web-service-type
            (hpcguix-web-configuration
             (specs %hpcguix-web-specs)
             (address "0.0.0.0")))))

(define %test-hpcguix-web
  (system-test
   (name "hpcguix-web")
   (description "Connect to a running hpcguix-web server.")
   (value (run-hpcguix-web-server-test name %hpcguix-web-os))))


(define %tailon-os
  ;; Operating system under test.
  (simple-operating-system
   (service dhcpcd-service-type)
   (service tailon-service-type
            (tailon-configuration
             (config-file
              (tailon-configuration-file
               (bind "0.0.0.0:8080")))))))

(define* (run-tailon-test #:optional (http-port 8081))
  "Run tests in %TAILON-OS, which has tailon running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     %tailon-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,http-port . 8080)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (ice-9 match)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            ;; Forward the guest's HTTP-PORT, where tailon is listening, to
            ;; port 8080 in the host.
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "tailon")

          (test-assert "service running"
            (wait-for-tcp-port 8080 marionette))

          (test-equal "http-get"
            200
            (#$retry-on-error
             (lambda ()
               (let-values (((response text)
                             (http-get #$(format
                                          #f
                                          "http://localhost:~A/"
                                          http-port)
                                       #:decode-body? #t)))
                 (response-code response)))
             #:times 10
             #:delay 5))

          (test-end))))

  (gexp->derivation "tailon-test" test))

(define %test-tailon
  (system-test
   (name "tailon")
   (description "Connect to a running Tailon server.")
   (value (run-tailon-test))))


;;;
;;; Anonip
;;;
(define %anonip-os
  ;; Operating system under test.
  (simple-operating-system
   (service anonip-service-type
            (anonip-configuration
             (input "/var/run/anonip/access.log")
             (output "/var/log/anonip/access.log")
             (debug? #t)))))

(define (run-anonip-test)
  (define os
    (marionette-operating-system
     %anonip-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define vm
    (virtual-machine
     (operating-system os)
     ;; We are interested in verifying if anonip still launches following a
     ;; reboot; thus make the base image writable.
     (volatile? #f)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "anonip")

          (test-assert "service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'anonip-/var/log/anonip/access.log))
             marionette))

          (test-assert "service can be restarted"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (restart-service 'anonip-/var/log/anonip/access.log)
                (wait-for-service 'anonip-/var/log/anonip/access.log))
             marionette))

          (test-assert "ip addresses are anonymized"
            (marionette-eval
             '(begin
                (use-modules (ice-9 textual-ports))
                (call-with-output-file "/var/run/anonip/access.log"
                  (lambda (port)
                    (display "192.168.100.200 - - \
[30/Oct/2024:14:57:44 +0100] GET /xxx.narinfo HTTP/1.1\" 200 1065 \
\"-\" \"GNU Guile\"\n" port)
                    (display "2001:0db8:85a3:0000:0000:8a2e:0370:7334 - - \
[30/Oct/2024:14:57:44 +0100] \"GET /xxx.narinfo HTTP/1.1\" 200 1065 \
\"-\" \"GNU Guile\"\n" port)))
                (#$retry-on-error
                 (lambda ()
                   (call-with-input-file "/var/log/anonip/access.log"
                     (lambda (port)
                       (let ((content (get-string-all port)))
                         ;; The expected values are taken from anonip's test
                         ;; suite (see its test_module.py file).
                         (or (and (string-contains content "192.168.96.0")
                                  (string-contains content "2001:db8:85a0::"))
                             (error "could not find expected anonymized IPs"
                                    content))))))
                 #:times 20
                 #:delay 1))
             marionette))

          (test-assert "service is running after reboot"
            (begin
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (eval-there '(begin
                                 (use-modules (shepherd system))
                                 (sync) ;ensure the log is fully written
                                 (reboot))))
               marionette)
              ;; Note: a distinct marionette-eval call is needed here; if
              ;; included in the previous one issuing the reboot,
              ;; 'wait-for-service' would apparently run before the system had
              ;; rebooted (and succeed), which would defeat the test.
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (wait-for-service 'anonip-/var/log/anonip/access.log))
               marionette)))

          (test-assert "service can be stopped"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (stop-service 'anonip-/var/log/anonip/access.log))
             marionette))

          (test-end))))

  (gexp->derivation "anonip-test" test))

(define %test-anonip
  (system-test
   (name "anonip")
   (description "Anonymize logs via Anonip")
   (value (run-anonip-test))))


;;;
;;; Patchwork
;;;

(define (patchwork-initial-database-setup-service configuration)
  (define start-gexp
    #~(lambda ()
        (let ((pid (primitive-fork))
              (postgres (getpwnam "postgres")))
          (if (eq? pid 0)
              (dynamic-wind
                (const #t)
                (lambda ()
                  (setgid (passwd:gid postgres))
                  (setuid (passwd:uid postgres))
                  (primitive-exit
                   (if (and
                        (zero?
                         (system* #$(file-append postgresql "/bin/createuser")
                                  #$(patchwork-database-configuration-user
                                      configuration)))
                        (zero?
                         (system* #$(file-append postgresql "/bin/createdb")
                                  "-O"
                                  #$(patchwork-database-configuration-user
                                      configuration)
                                  #$(patchwork-database-configuration-name
                                      configuration))))
                       0
                       1)))
                (lambda ()
                  (primitive-exit 1)))
              (zero? (cdr (waitpid pid)))))))

  (shepherd-service
   (requirement '(postgres))
   (provision '(patchwork-postgresql-user-and-database))
   (start start-gexp)
   (stop #~(const #f))
   (respawn? #f)
   (documentation "Setup patchwork database.")))

(define (patchwork-os patchwork)
  (simple-operating-system
   (service dhcpcd-service-type)
   (service httpd-service-type
            (httpd-configuration
             (config
              (httpd-config-file
               (listen '("8080"))))))
   (service postgresql-service-type
            (postgresql-configuration
             (postgresql postgresql)))
   (service patchwork-service-type
            (patchwork-configuration
             (patchwork patchwork)
             (domain "localhost")
             (settings-module
              (patchwork-settings-module
               (allowed-hosts (list domain))
               (default-from-email "")))
             (getmail-retriever-config
              (getmail-retriever-configuration
               (type "SimpleIMAPSSLRetriever")
               (server "imap.example.com")
               (port 993)
               (username "username")
               (password "password")
               (extra-parameters
                '((mailboxes . ("INBOX"))))))))
   (simple-service 'patchwork-database-setup
                   shepherd-root-service-type
                   (list
                    (patchwork-initial-database-setup-service
                     (patchwork-database-configuration))))))

(define (run-patchwork-test patchwork)
  "Run tests in %NGINX-OS, which has nginx running and listening on
HTTP-PORT."
  (define os
    (marionette-operating-system
     (patchwork-os patchwork)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define forwarded-port 8080)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((8080 . ,forwarded-port)))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-11) (srfi srfi-64)
                       (ice-9 match)
                       (gnu build marionette)
                       (web uri)
                       (web client)
                       (web response))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "patchwork")

          (test-assert "patchwork-postgresql-user-and-service started"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service 'patchwork-postgresql-user-and-database)
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) pid)))))
             marionette))

          (test-assert "httpd running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'httpd))
             marionette))

          (test-equal "http-get"
            200
            (#$retry-on-error
             (lambda ()
               (let-values
                   (((response text)
                     (http-get #$(simple-format
                                  #f "http://localhost:~A/" forwarded-port)
                               #:decode-body? #t)))
                 (response-code response)))
             #:times 10
             #:delay 5))

          (test-end))))

  (gexp->derivation "patchwork-test" test))

(define %test-patchwork
  (system-test
   (name "patchwork")
   (description "Connect to a running Patchwork service.")
   (value (run-patchwork-test patchwork))))


;;;
;;; Agate
;;;

(define %index.gmi-contents
  ;; Contents of the /index.gmi file.
  "Hello, guix!")

(define %make-agate-root
  ;; Create our server root in /srv.
  #~(begin
      (mkdir "/srv")
      (mkdir "/srv/gemini")
      (mkdir "/srv/gemini-certs")
      ;; Directory should be writable for Agate user to generate certificates
      (let ((user (getpw "agate")))
        (chown "/srv/gemini-certs" (passwd:uid user) (passwd:gid user)))
      (call-with-output-file (string-append "/srv/gemini/index.gmi")
        (lambda (port)
          (display #$%index.gmi-contents port)))))

(define %agate-os
  (simple-operating-system
   (service dhcpcd-service-type)
   (simple-service 'make-agate-root activation-service-type
                   %make-agate-root)
   (service agate-service-type
            (agate-configuration
             (hostnames '("localhost"))))))

(define* (run-agate-test name test-os expected-content)
  (define os
    (marionette-operating-system
     test-os
     #:imported-modules '((gnu services herd)
                          (guix combinators))
     #:extensions (list guile-gemini guile-gnutls)))

  (define forwarded-port 1965)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((1965 . ,forwarded-port)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin #$name)

          (test-assert #$(string-append name " service running")
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (match (start-service '#$(string->symbol name))
                  (#f #f)
                  (('service response-parts ...)
                   (match (assq-ref response-parts 'running)
                     ((#t) #t)
                     ((pid) pid)))))
             marionette))

          (test-assert "Agate TCP port ready, IPv4"
            (wait-for-tcp-port #$forwarded-port marionette))

          (test-assert "Agate TCP port ready, IPv6"
            (wait-for-tcp-port #$forwarded-port marionette
                               #:address
                               '(make-socket-address
                                 AF_INET6 (inet-pton AF_INET6 "::1") #$forwarded-port)))

          (test-equal "Agate responses with the specified index.gmi"
            #$expected-content
            (marionette-eval '(begin
                                (use-modules (ice-9 iconv)
                                             (gemini client)
                                             (gemini request)
                                             (gemini response))
                                (bytevector->string (gemini-response-body-bytes
                                                     (send-gemini-request
                                                      (build-gemini-request #:host "localhost" #:port #$forwarded-port)))
                                                    "utf8")) marionette))

          (test-end))))
  (gexp->derivation "agate-test" test))

(define %test-agate
  (system-test
   (name "agate")
   (description "Connect to a running Agate service.")
   (value (run-agate-test name %agate-os %index.gmi-contents))))
