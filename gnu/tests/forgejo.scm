;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2026 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu tests forgejo)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages version-control)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services databases)
  #:use-module (gnu services forgejo)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (guix gexp)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (%test-forgejo
            %test-forgejo-mysql
            %test-forgejo-postgres))

;;; Sample SSH key generated with 'ssh-keygen -t ed25519'.
(define dummy-ssh-private-key "\
-----BEGIN OPENSSH PRIVATE KEY-----
b3BlbnNzaC1rZXktdjEAAAAABG5vbmUAAAAEbm9uZQAAAAAAAAABAAAAMwAAAAtzc2gtZW
QyNTUxOQAAACAsDitIHHy6wlmXz7cJO0UQQbrszdpLvBSiTZk1l08z0AAAAJBl4y2eZeMt
ngAAAAtzc2gtZWQyNTUxOQAAACAsDitIHHy6wlmXz7cJO0UQQbrszdpLvBSiTZk1l08z0A
AAAEDxsrZIsEgI6cehDgFNsP/FQ3aefvb7fUtp/RF6sZf3GiwOK0gcfLrCWZfPtwk7RRBB
uuzN2ku8FKJNmTWXTzPQAAAAC21heGltQHRlcnJhAQI=
-----END OPENSSH PRIVATE KEY-----
")

(define dummy-ssh-public-key "\
ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICwOK0gcfLrCWZfPtwk7RRBBuuzN2ku8FK\
JNmTWXTzPQ dummy")

;;; For a manual test/inspection:
#;(

$(./pre-inst-env guix system vm \
-e '(@@ (gnu tests forgejo) %forgejo-os)' --no-graphic) \
-nic user,model=virtio-net-pci,hostfwd=tcp::3000-:3000 -m 1024

)

(define %forgejo-os
  (let ((base %simple-os))
    (operating-system
      (inherit %simple-os)
      (packages (cons* git-minimal git-lfs
                       ;;sqlite strace    ;for debug
                       %base-packages))
      (services
       (cons* (service dhcpcd-service-type)
              (service openssh-service-type)
              (service forgejo-service-type
                       (forgejo-configuration
                        (application-slogan "In Code We Trust")
                        (run-mode "dev")
                        (log-level "debug")
                        (user-push-to-create? #t)
                        (default-push-to-create-private? #f)
                        (mailer? #t)
                        (mail-from "Forgejo Test <test@example.com>")
                        (mail-protocol "dummy")
                        (mail-notification? #t)
                        (mail-notification-on-new-user-signin? #t)))
              %base-services)))))

(define %forgejo-os/postgres
  (operating-system
    (inherit %forgejo-os)
    (services
     (cons*
      (service postgresql-service-type
               (postgresql-configuration
                 (postgresql postgresql)))
      (service postgresql-role-service-type
               (postgresql-role-configuration
                (roles (list (postgresql-role
                               (name "forgejo")
                               (create-database? #t))))))
      (modify-services (operating-system-user-services %forgejo-os)
        (forgejo-service-type
         config =>
         (forgejo-configuration
          (inherit config)
          (database-type "postgres"))))))))

(define %forgejo-os/mysql
  (operating-system
    (inherit %forgejo-os)
    (services
     (cons
      (service mysql-service-type)
      (modify-services (operating-system-user-services %forgejo-os)
        (forgejo-service-type
         config =>
         (forgejo-configuration
          (inherit config)
          (database-type "mysql")
          (database-host "/run/mysqld/mysqld.sock"))))))))

(define (os->forgejo-configuration os)
  (let ((forgejo-service
         (find (lambda (x)
                 (eq? forgejo-service-type (service-kind x)))
               (operating-system-user-services os))))
    (service-value forgejo-service)))

(define* (make-forgejo-test name #:key (os %forgejo-os))
  "Return a test of an OS running the Forgejo service."

  (define forgejo-config
    (os->forgejo-configuration os))

  (define database-type
    (forgejo-configuration-database-type forgejo-config))

  (define database-name
    (forgejo-configuration-database-name forgejo-config))

  (define database-user
    (forgejo-configuration-database-user forgejo-config))

  (define vm
    (virtual-machine
      (operating-system (marionette-operating-system
                         os
                         #:imported-modules '((gnu services herd))
                         #:requirements (match database-type
                                          ("mysql" '(mysql))
                                          ("postgres" '(postgres))
                                          ("sqlite3" '()))))
      (memory-size 1024)
      (port-forwardings '((3333 . 3000))))) ;3333 on host

  (define test
    (with-extensions (list guile-json-4)
      (with-imported-modules '((gnu build dbus-service)
                               (gnu build marionette)
                               (guix base64))
        #~(begin
            (use-modules (gnu build dbus-service)
                         (gnu build marionette)
                         (guix base64)
                         (json)
                         (rnrs base)
                         (rnrs bytevectors)
                         (srfi srfi-64)
                         (srfi srfi-71)
                         (web client)
                         (web response)
                         (web uri))

            (define marionette
              (make-marionette (list #$vm)))

            (test-runner-current (system-test-runner #$output))
            (test-begin "forgejo")

            (unless (string=? "mysql" #$database-type)
              (test-skip 2))
            (test-assert "wait for mysql socket"
              ;; This can be removed after Shepherd gains systemd notify
              ;; support (mysql uses it to notify when it's ready).
              (wait-for-unix-socket
               #$(forgejo-configuration-database-host forgejo-config)
               marionette))
            (test-assert "mysql setup"
              (marionette-eval
               '(begin
                  (use-modules (guix build utils))
                  (invoke "mysql" "-e" (string-append "\
CREATE USER '" #$database-user "'@'localhost';
CREATE DATABASE " #$database-name ";
GRANT ALL PRIVILEGES ON " #$database-name ".*"
"TO '" #$database-user "'@'localhost';
FLUSH PRIVILEGES;")))
               marionette))

            (test-assert "forgejo can be restarted"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (restart-service 'forgejo))
               marionette))

            (test-assert "forgejo runs"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd))
                  (start-service 'forgejo))
               marionette))

            (test-assert "shepherd listens on tcp port 3000"
              (wait-for-tcp-port 3000 marionette))

            (test-assert "slogan can be retrieved from main page"
              ;; This socket-activates Forgejo, so we must wait a bit while it
              ;; becomes ready.
              (with-retries 20 1
                (let (((values response body)
                       (http-get "http://localhost:3333")))
                  (assert (= 200 (response-code response)))
                  (string-contains body "In Code We Trust"))))

            (test-assert "create test forgejo user"
              (marionette-eval
               '(begin
                  (use-modules (guix build utils))
                  (invoke "su" "-l" "forgejo" "-c"
                          "FORGEJO_WORK_DIR=/var/lib/forgejo forgejo \
admin user create --username dummy --password dummy \
--email dummy@localhost --must-change-password=false"))
               marionette))

            (test-assert "upload ssh key for dummy forgejo user"
              ;; We use the Forgejo API to get a token, then use it to set the
              ;; SSH public key of the user, which is needed for
              ;; non-interactively pushing to a repository.
              (let (((values response body)
                     (http-post "http://localhost:3333/api/v1/user/keys"
                                #:headers
                                `((Accept . "application/json")
                                  (Authorization
                                   . ,(string-append
                                       "Basic " (base64-encode
                                                 (string->utf8 "dummy:dummy"))))
                                  (Content-Type . "application/json"))
                                #:body (string->utf8
                                        (scm->json-string
                                         '(("key" . #$dummy-ssh-public-key)
                                           ("read_only" . #t)
                                           ("title" . "Dummy's SSH key")))))))
                (= 201 (response-code response))))

            (test-assert "configure git client for dummy user"
              (marionette-eval
               '(begin
                  (setenv "HOME" "/root")
                  (chdir "/root")

                  ;; Populate private SSH key.
                  (mkdir ".ssh")
                  (call-with-output-file ".ssh/id_ed25519"
                    (lambda (port)
                      (display #$dummy-ssh-private-key port)))
                  (chmod ".ssh/id_ed25519" #o700)

                  ;; To avoid interactive prompts.
                  (call-with-output-file ".ssh/config"
                    (lambda (port)
                      (display "StrictHostKeyChecking no\n" port)))

                  ;; Init git.
                  (invoke "git" "config" "--global" "user.name" "dummy")
                  (invoke "git" "config" "--global" "user.email"
                          "dummy@localhost"))
               marionette))

            (test-assert "git create dummy local repo"
              (marionette-eval
               '(begin
                  (mkdir "dummy-repo")
                  (chdir "dummy-repo")
                  (invoke "git" "init")
                  (invoke "git" "remote" "add" "origin"
                          "forgejo@localhost:dummy/hello") ;SSH remote
                  (call-with-output-file "hello.txt"
                    (lambda (port)
                      (display "is anybody here?\n" port)))
                  (invoke "git" "add" "hello.txt")
                  (invoke "git" "commit" "-m" "init repo"))
               marionette))

            (test-assert "git push repo to forgejo via ssh"
              (marionette-eval
               '(begin
                  (invoke "git" "push" "origin" "master"))
               marionette))

            (test-assert "git clone repo from forgejo via http"
              (marionette-eval
               '(begin
                  (use-modules (ice-9 textual-ports))
                  ;; FIXME: Switch back 127.0.0.1 to localhost after
                  ;; <https://codeberg.org/shepherd/shepherd/issues/122> is
                  ;; resolved.
                  (invoke "git" "clone" "http://127.0.0.1:3000/dummy/hello"
                          "/tmp/hello-copy")
                  (call-with-input-file "/tmp/hello-copy/hello.txt"
                    (lambda (port)
                      (string-contains (get-string-all port)
                                       "is anybody here?"))))
               marionette))

            (test-assert "setup git lfs"
              (marionette-eval
               '(begin
                  (invoke "git" "lfs" "install")
                  (invoke "git" "lfs" "track" "*.blob")
                  (invoke "git" "add" ".gitattributes"))
               marionette))

            (test-assert "commit blob file with git lfs"
              (marionette-eval
               '(begin
                  (use-modules (ice-9 binary-ports)
                               (rnrs base)
                               ((scheme base) #:select (read-bytevector
                                                        write-bytevector)))

                  (call-with-output-file "one-mebibyte.blob"
                    (lambda (out)
                      (write-bytevector
                       (call-with-input-file "/dev/random"
                         (lambda (in)
                           (read-bytevector (expt 2 20) in))
                         #:binary #t)
                       out))
                    #:binary #t)

                  (invoke "git" "add" "one-mebibyte.blob")
                  (invoke "git" "commit" "-m" "add binary blob")
                  (zero?
                   (system "git lfs ls-files|grep -Fq one-mebibyte.blob")))
               marionette))

            (test-assert "push lfs tracked files to forgejo"
              (marionette-eval
               '(begin
                  (invoke "git" "push" "origin" "master"))
               marionette))

            (test-assert "cloned repo contains lfs tracked files"
              (marionette-eval
               '(begin
                  ;; FIXME: Switch back 127.0.0.1 to localhost after
                  ;; <https://codeberg.org/shepherd/shepherd/issues/122> is
                  ;; resolved.
                  (invoke "git" "clone" "http://127.0.0.1:3000/dummy/hello"
                          "/tmp/hello-with-blob")
                  (= (expt 2 20)
                     (stat:size
                      (stat "/tmp/hello-with-blob/one-mebibyte.blob"))))
               marionette))

            (test-assert "email notification works"
              (begin
                (let (((values response body)
                       (http-post
                        "http://localhost:3333/user/forgot_password"
                        #:headers `((Content-Type
                                     . "application/x-www-form-urlencoded"))
                        #:body (string->utf8
                                (string-append
                                 "email=" (uri-encode "dummy@localhost"))))))
                  (assert (= 200 (response-code response))))

                (with-retries 10 1
                  (marionette-eval
                   '(begin
                      (use-modules (ice-9 textual-ports))
                      (string-contains (call-with-input-file
                                           "/var/log/forgejo.log"
                                         get-string-all)
                                       "Subject: Recover your account"))
                   marionette))))

            (test-end)))))

  (gexp->derivation (string-append "forgejo-test-" name) test))

(define %test-forgejo
  (system-test
   (name "forgejo")
   (description "Basic tests for the Forgejo service.")
   (value (make-forgejo-test "sqlite"))))

(define %test-forgejo-mysql
  (system-test
   (name "forgejo-mysql")
   (description "Basic tests for the Forgejo service, using a MySQL
database.")
   (value (make-forgejo-test "mysql" #:os %forgejo-os/mysql))))

(define %test-forgejo-postgres
  (system-test
   (name "forgejo-postgres")
   (description "Basic tests for the Forgejo service, using a PostgreSQL
database.")
   (value (make-forgejo-test "postgres" #:os %forgejo-os/postgres))))

;; Local Variables:
;; eval: (put 'with-retries 'scheme-indent-function 2)
;; End:
