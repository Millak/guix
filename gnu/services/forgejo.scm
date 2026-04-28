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

(define-module (gnu services forgejo)
  #:use-module (guix gexp)
  #:use-module (guix least-authority)
  #:use-module (guix records)
  #:use-module (gnu build linux-container)
  #:use-module (gnu packages forgejo)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (forgejo-service-type
            forgejo-configuration
            forgejo-configuration-forgejo
            forgejo-configuration-application-name
            forgejo-configuration-application-slogan
            forgejo-configuration-run-mode
            forgejo-configuration-configuration-wizard?
            forgejo-configuration-domain
            forgejo-configuration-port
            forgejo-configuration-protocol
            forgejo-configuration-root-url
            forgejo-configuration-http-address
            forgejo-configuration-acme?
            forgejo-configuration-tls-cert-file
            forgejo-configuration-tls-key-file
            forgejo-configuration-ssh-address
            forgejo-configuration-ssh-port
            forgejo-configuration-actions?
            forgejo-configuration-default-actions-url
            forgejo-configuration-disable-registration?
            forgejo-configuration-confirm-mail-on-registration?
            forgejo-configuration-internal-token-file
            forgejo-configuration-secret-key-file
            forgejo-configuration-lfs?
            forgejo-configuration-lfs-secret-file
            forgejo-configuration-log-level
            forgejo-configuration-oauth2-secret-file
            forgejo-configuration-mail-notification?
            forgejo-configuration-mail-notification-on-new-user-signin?
            forgejo-configuration-mailer?
            forgejo-configuration-mail-subject-prefix
            forgejo-configuration-mail-protocol
            forgejo-configuration-smtp-address
            forgejo-configuration-smtp-port
            forgejo-configuration-smtp-user
            forgejo-configuration-smtp-password-file
            forgejo-configuration-mail-plain-text?
            forgejo-configuration-mail-from
            forgejo-configuration-sendmail-command
            forgejo-configuration-sendmail-options
            forgejo-configuration-sendmail-timeout
            forgejo-configuration-sendmail-convert-crlf?
            forgejo-configuration-openid-signin?
            forgejo-configuration-metrics?
            forgejo-configuration-metrics-token
            forgejo-configuration-database-type
            forgejo-configuration-database-host
            forgejo-configuration-database-name
            forgejo-configuration-database-user
            forgejo-configuration-database-password-file
            forgejo-configuration-database-tls?
            forgejo-configuration-user-push-to-create?
            forgejo-configuration-organization-push-to-create?
            forgejo-configuration-default-push-to-create-private?
            forgejo-configuration-require-signin-to-view?
            forgejo-configuration-default-merge-style
            forgejo-configuration-default-trust-model
            forgejo-configuration-default-keep-email-private?
            forgejo-configuration-offline-mode?
            forgejo-configuration-user
            forgejo-configuration-group
            forgejo-configuration-custom-configuration-file
            forgejo-configuration-shepherd-provision
            forgejo-configuration-work-directory))

;;; Copied from (gnu services base)
(define (ipv6-address? str)
  "Return true if STR denotes an IPv6 address."
  (false-if-exception (->bool (inet-pton AF_INET6 str))))

(define (database-type? x)
  (member x '("sqlite3" "mysql" "postgres")))

(define (port? x)
  (and (number? x)
       (and (>= x 1) (<= x 65535))))

(define (privileged-port? x)
  (and (number? x)
       (and (>= x 1) (<= x 1023))))

(define (unprivileged-port? x)
  (and (number? x)
       (and (>= x 1024) (<= x 65535))))

(define (port-or-false? x)
  (or (not x)
      (port? x)))

(define (log-level? x)
  (member x '("trace" "debug" "info" "warn" "error" "none")))

(define (merge-style? x)
  (member x '("merge" "rebase" "rebase-merge" "squash" "fast-forward-only")))

(define (protocol? x)
  (member x '("http" "https" "fcgi" "http+unix" "fcgi+unix")))

(define (run-mode? x)
  (member x '("dev" "prod")))

(define (trust-model? x)
  (member x '("collaborator" "committer" "collaboratorcommitter")))

(define (ssl-mode? x)
  (member x '(#t #f 'skip-verify)))

(define (serialize-database-tls type value)
  (match type
    ("sqlite3" "disable")               ;does not apply
    ("mysql" (match value
               (#t "true")
               (#f "false")
               ('skip-verify "skip-verify")))
    ("postgres" (match value
                    (#t "verify-full")
                    (#f "disable")
                    ('skip-verify "require")))))

(define-maybe file-like)

(define-maybe/no-serialization string)

(define (mail-protocol? x)
  (member x '("" "dummy" "sendmail"
              "smtp" "smtps" "smtp+starttls" "smtp+unix")))

(define-configuration/no-serialization forgejo-configuration
  (forgejo
   (file-like forgejo)
   "The forgejo package.")

  (work-directory
   (string "/var/lib/forgejo")
   "The directory where Forgejo keeps its state, including its database when
using @code{\"sqlite3\"} as the @code{database-type}.")

  (shepherd-provision
   (list-of-symbols '(forgejo))
   "The name(s) of the service.")

  (configuration-wizard?
   (boolean #f)
   "Whether to use the initial configuration page (wizard) to setup Forgejo.
Setting this to @code{#t} means that none of the declarative configuration
values will be honored.  The Forgejo-generated configuration file will be kept
under @file{custom/conf/app.ini}, relative to the value of the
@code{work-directory}.")

  (user
   (string "forgejo")
   "The name of the user under which Forgejo will be executed.")

  (group
   (string "forgejo")
   "The name of the group under which Forgejo will be executed.")

  (custom-configuration-file
   maybe-file-like
   "File-like object to provide a complete Forgejo configuration file as an
escape hatch, overriding every other fields of this configuration.")

  ;; Global settings (no section).
  (application-name
   (string "Forgejo")
   "The application name that shows in every page title.")

  (application-slogan
   (string "")
   "Slogan to show near the application name in every page title.")

  (run-mode
   (run-mode "prod")
   "The run mode to use; either @code{\"dev\"} or @code{\"prod\"}.")

  ;; [server] settings.
  (domain
   (string "localhost")
   "The domain name or host address of the server.")

  (offline-mode?
   (boolean #f)
   "Set to @code{#t} to run Forgejo in offline mode, meaning it won't reach
out to any external services such as Gravatar, CDNs, etc. and serve everything
locally instead.")

  (port
   (unprivileged-port 3000)
   "The port Forgejo will listen on.  The port should be an unprivileged port;
to serve Forgejo on a privileged port like @code{80}, configure a reverse
proxy.  It is ignored when using a Unix-domain socket.")

  (protocol
   (protocol "http")
   "Listen protocol.  Either @code{http}, @code{https}, @code{fcgi},
@code{\"http+unix\"} or @code{\"fcgi+unix\"}.  @samp{+unix} means @emph{via} a
Unix-domain socket, in which case @code{http-address} must be the file name of
the socket to use.")

  (root-url
   maybe-string
   "Overwrite the automatically generated public URL,
which implicitly defaults to @samp{@var{protocol}://@var{domain}:@var{port}},
which is necessary using a reverse proxy.")

  (http-address
   (string "0.0.0.0")
   "The address to listen to.  It can be an absolute file name when using a
Unix-domain socket, for example @file{/run/forgejo.sock}, or a file name
relative to the @code{work-directory}, for example @file{forgejo.sock}.")

  (ssh-address
   (string "0.0.0.0")
   "The IP address to listen on for the SSH server.")

  (lfs?
   (boolean #t)
   "Whether to enable Git LFS support.")

  (lfs-secret-file
   (string "/etc/forgejo/lfs_jwt_secret")
   "The file name of the LFS JWT (JSON Web Token) secret, necessary for
proper Git LFS operation, which can be generated with @samp{forgejo generate
secret JWT_SECRET}.  Automatically generated the first time Forgejo runs if it
does not yet exist.")

  (mail-notification?
   (boolean #f)
   "Whether to enable email notifications.")

  (acme?
   (boolean #f)
   "Whether to enable @acronym{ACME, Automatic Certificate Management
Environment}, to automatically provision TLS certificates, which can be useful
if you use the @code{https} or @code{https+unix} protocols.")

  (tls-cert-file
   (string "https/cert.pem")
   "File name of a TLS certificate file.  Ignored if @code{acme?} is enabled.
A relative file name is located under @file{@var{work-directory}/custom}.")

  (tls-key-file
   (string "https/key.pem")
   "File name of a TLS key.  Ignored if @code{acme?} is enabled.  A relative
file name is located under @file{@var{work-directory}/custom}.")

  (ssh-port
   (port-or-false 22)
   "The SSH port Forgejo will use; if you already run an OpenSSH as the SSH
server, specify the port used by the SSH daemon.  Otherwise, you need to use
an unprivileged port (greater or equal to 1024) which Forgejo will use with
its built-in SSH server.  Set to @code{#f} to disable SSH support.")

  ;; [actions] settings.
  (actions?
   (boolean #t)
   "Whether to enable actions capabilities.")

  (default-actions-url
    (string "https://code.forgejo.org")
    "The default address to fetch action plugins from.")

  ;; [security] settings.
  (internal-token-file
   (string "/etc/forgejo/internal_token")
   "The file name of the internal token used to validate communication within
Forgejo, which can be generated with @samp{forgejo generate
secret INTERNAL_TOKEN}")

  (secret-key-file
   (string "/etc/forgejo/secret_key")
   "The file name containing the global secret key used for encrypting data
like 2FA secrets; it is thus very important to back it up somewhere safe to
avoid losing access to encrypted data.")

  ;; [oauth2] settings.
  (oauth2-jwt-secret-file
   (string "/etc/forgejo/oauth2_jwt_secret")
   "The file name of the OAuth2 JWT secret, which Forgejo expects to exist.")

  ;; [admin] settings.
  (mail-notification-on-new-user-signin?
   (boolean #f)
   "Whether to notify administrators by email when a new user signs in for the
first time.")

  ;; [mailer] settings.
  (mailer?
   (boolean #f)
   "Whether to enable the mail server integration, used for sending email
notifications for example.")

  (mail-from
   (string "")
   "The FROM email address used for sent emails, per the RFC 5322
specification.  This can be for example just an email address, or something
like @samp{\"Name\" <email@@example.com>}.  This field @emph{must} be specified
if @code{mailer?} is set to @code{#t}.")

  (mail-subject-prefix
   (string "")
   "The prefix displayed before the subject in emails.")

  (mail-protocol
   (mail-protocol "")
   "The mail server protocol.  One of @code{\"smtp\"}, @code{\"smtps\"},
@code{\"smtp+starttls\"}, @code{\"smtp+unix\"}, @code{\"sendmail\"},
@code{\"dummy\"} or @code{\"\"}.  The empty string defaults means the protocol
is inferred from the value of the @code{smtp-port} value, meaning its
effective implicit value is @code{\"smtps\"}.  @code{\"dummy\"} causes emails
to be sent to the log, useful for testing.  When @code{\"sendmail\"} is used,
it is expected to be fully and correctly externally configured, with the other
@samp{smtp-} prefixed options of this configuration ignored.")

  (smtp-address
   (string "")
   "The mail server address, for example @code{\"smtp.example.com\"} or an
absolute file name, if using @code{\"smtp+unix\"} for the
@code{mail-protocol}.")

  (smtp-port
   (port 465)                           ;implies smtps protocol
   "The mail server port.  This field affects the implicit value of the
@code{mail-protocol}.")

  (smtp-user
   (string "")
   "The SMTP server user name, if required ")

  (smtp-password-file
   (string "")
   "An absolute file name containing the password of the SMTP server.")

  (mail-plain-text?
   (boolean #f)
   "Send emails only in plain text, without HTML alternative.")

  (sendmail-command
   (string "sendmail")
   "The file name of the @command{sendmail} command to use, which can be
either an absolute file name or a command name to be looked from
@env{\"PATH\"}.")

  (sendmail-options
   (list-of-strings '())
   "Optional options to pass to the @command{sendmail} command.  If your
@code{sendmail} program, like that of Postfix, interprets options, you should
provide @code{\"--\"} as the first option.")

  (sendmail-timeout
   (string "5m")
   "The timeout for Sendmail, provided as a
@url{https://pkg.go.dev/time#ParseDuration, Go @code{time.Duration} string},
for example @code{\"300ms\"}, @code{\"1.5h\"} or @code{\"2h45m\"}.")

  (sendmail-convert-crlf?
   (boolean #t)
   "Whether to convert @samp{\\r\\n} to @samp{\\n} for Sendmail.")

  ;; [log] settings.
  (log-level
   (log-level "info")
   "The log level to use.  Either @code{\"trace\"}, @code{\"debug\"},
@code{\"info\"}, @code{\"warn\"}, @code{\"error\"} or @code{\"none\"}.")

  ;; [openid] settings.
  (openid-signin?
   (boolean #t)
   "Whether to allow signing in using OpenID.")

  ;; [service] settings.
  (disable-registration?
   (boolean #f)
   "Disallow registration, only allowing administrators to create accounts.")

  (confirm-mail-on-registration?
   (boolean #f)
   "Whether to require email confirmation when new users register.")

  (require-signin-to-view?
   (boolean #f)
   "Whether users must sign in before they can view the explore pages.")

  (default-keep-email-private?
    (boolean #f)
    "Whether by default a user email is displayed on their profile.")

  (user-push-to-create?
   (boolean #f)
   "Allow users to push local repositories to Forgejo and have them
automatically created for a user.")

  (organization-push-to-create?
   (boolean #f)
   "Allow users to push local repositories to Forgejo and have them
automatically created for an organization.")

  (default-push-to-create-private?
    (boolean #t)
    "Whether to make newly created repositories private by default when they
were created via push.")

  ;; [repository.pull-request] settings.
  (default-merge-style
    (merge-style "merge")
    "The default merge style to use.  Either @code{\"merge\"},
@code{\"rebase\"}, @code{\"rebase-merge\"}, @code{\"squash\"} or
@code{\"fast-forward-only\"}.")

  ;; [repository.signing] settings.
  (default-trust-model
    (trust-model "committer")
    "The default trust model for repositories.  Either
@code{\"collaborator\"}, @code{\"committer\"} or
@code{\"collaboratorcommitter\"}")

  ;; [metrics] settings.
  (metrics?
   (boolean #f)
   "Whether to enable the metrics endpoint.")

  (metrics-token
   (string "")
   "A secret to use, if authorization to the metrics endpoint is desired.")

  ;; [database] settings.
  (database-type
   (database-type "sqlite3")
   "The database type to use.  Either @code{\"sqlite3\"}, @code{\"mysql\"} or
@code{\"postgres\"}.  The sqlite3 type is the simplest to use, but a
full-fledged database like PostgreSQL may scale better for multiple users.  If
you change this, make sure to adjust the other @samp{database-*} values
accordingly.  Also note that currently, the PostgreSQL and MySQL databases
must be manually created: refer to the official Forgejo documentation for
@url{https://forgejo.org/docs/latest/admin/installation/database-preparation,
database preparation}.")

  (database-host
   (string "/var/run/postgresql")
   "This is the @samp{@var{host}:@var{port}} tuple for remote database
MySQL/PostgreSQL hosts, or a local Unix-domain socket file.  If using MySQL,
this should be adjusted to @file{/run/mysqld/mysqld.sock}")

  (database-name
   ;; Keep the default database name in sync with the database user name, so
   ;; that it can be easily provisioned via a the
   ;; postgresql-role-service-type, which assumes both match.
   (string "forgejo")
   "The MySQL/PostgreSQL database name to use.")

  (database-user
   (string "forgejo")
   "The MySQL/PostgreSQL database user to use.")

  (database-password-file
   maybe-string
   "A file name containing the password to access the MySQL/PostgreSQL
database, for example @file{\"/etc/forgejo/db_passwd\"}.")

  (database-tls?
   (ssl-mode #f)
   "Whether the connection to a MySQL/PostgreSQL database should use TLS.  Set
to @code{#t} to enable TLS with full verification or to @code{'skip-verify} to
enable TLS without verification."))

;;; TODO: Allow enabling/configuring email integration
;;; See custom/conf/app.example.ini for a documentation of the fields.
(define forgejo-configuration->file
  (match-record-lambda <forgejo-configuration>
      ( application-name application-slogan work-directory
        run-mode domain protocol http-address port root-url
        acme? tls-cert-file tls-key-file
        ssh-address ssh-port
        default-keep-email-private?
        default-merge-style default-trust-model
        actions? default-actions-url
        disable-registration? confirm-mail-on-registration?
        require-signin-to-view? mail-notification?
        internal-token-file secret-key-file
        lfs? lfs-secret-file oauth2-jwt-secret-file
        mail-notification-on-new-user-signin?
        mailer? mail-subject-prefix mail-protocol
        smtp-address smtp-port smtp-user smtp-password-file
        mail-plain-text? mail-from
        sendmail-command sendmail-options sendmail-timeout
        sendmail-convert-crlf?
        log-level offline-mode? openid-signin?
        metrics? metrics-token
        user-push-to-create? organization-push-to-create?
        default-push-to-create-private?
        user group
        custom-configuration-file
        database-type database-host database-name database-user
        database-password-file database-tls?)
    (if (maybe-value-set? custom-configuration-file)
        custom-configuration-file
        (mixed-text-file "forgejo.ini" "
APP_NAME = " application-name "
APP_SLOGAN = " application-slogan "
RUN_USER = " user "
WORK_PATH = " work-directory "
RUN_MODE = " run-mode "

[database]
DB_TYPE = " database-type "
HOST = " database-host "
NAME = " database-name "
USER = " database-user "\n"
(if (maybe-value-set? database-password-file)
    (string-append "PASSWD_URI = file://" database-password-file "\n")
    "")
"SSL_MODE = " (serialize-database-tls database-type database-tls?) "

[repository]
ROOT = " work-directory "/data/forgejo-repositories
DEFAULT_PUSH_CREATE_PRIVATE = "
(if default-push-to-create-private? "true" "false") "
ENABLE_PUSH_CREATE_USER = " (if user-push-to-create? "true" "false") "
ENABLE_PUSH_CREATE_ORG = " (if organization-push-to-create? "true" "false") "

[server]
SSH_DOMAIN = " domain "
SSH_LISTEN_HOST = " ssh-address "
PROTOCOL = " protocol "
DOMAIN = " domain "
HTTP_ADDR = " http-address "
HTTP_PORT = " (number->string port) "\n"
(if (maybe-value-set? root-url)
    (string-append "ROOT_URL = " root-url "\n")
    "")
"ENABLE_ACME = " (if acme? "true" "false") "
CERT_FILE = " tls-cert-file "
KEY_FILE = " tls-key-file "
APP_DATA_PATH = " work-directory "/data
DISABLE_SSH = " (if (not ssh-port) "true" "false") "
SSH_PORT = " (if (not ssh-port) "" (number->string ssh-port)) "
LFS_START_SERVER = " (if lfs? "true" "false") "
LFS_JWT_SECRET_URI = file://" lfs-secret-file "
OFFLINE_MODE = " (if offline-mode? "true" "false") "

[lfs]
PATH = " work-directory "/data/lfs

[service]
REGISTER_EMAIL_CONFIRM = false
ENABLE_NOTIFY_MAIL = " (if mail-notification? "true" "false") "
DISABLE_REGISTRATION = " (if disable-registration? "true" "false") "
REGISTER_EMAIL_CONFIRM = " (if confirm-mail-on-registration? "true" "false") "
ENABLE_CAPTCHA = false
REQUIRE_SIGNIN_VIEW = " (if require-signin-to-view? "true" "false") "
DEFAULT_KEEP_EMAIL_PRIVATE = " (if default-keep-email-private? "true" "false") "

[openid]
ENABLE_OPENID_SIGNIN = " (if openid-signin? "true" "false")  "

[cron.update_checker]
ENABLED = false

[log]
LEVEL = " log-level "
ROOT_PATH = " work-directory "/log

[repository.pull-request]
DEFAULT_MERGE_STYLE = " default-merge-style "

[repository.signing]
DEFAULT_TRUST_MODEL = " default-trust-model "

[security]
INSTALL_LOCK = true
SECRET_KEY = file://" secret-key-file "
INTERNAL_TOKEN_URI = file://" internal-token-file "

[oauth2]
JWT_SECRET_URI = file://" oauth2-jwt-secret-file "

[admin]
SEND_NOTIFICATION_EMAIL_ON_NEW_USER = "
(if mail-notification-on-new-user-signin? "true" "false") "

[mailer]
ENABLED = " (if mailer? "true" "false") "
SUBJECT_PREFIX = " mail-subject-prefix "
PROTOCOL = " mail-protocol "
SMTP_ADDR = " smtp-address "
SMTP_PORT = " (number->string smtp-port) "
USER = " smtp-user "
PASSWD_URI = " (if (string-null? smtp-password-file)
                   ""
                   (string-append "file://" smtp-password-file)) "
SEND_AS_PLAIN_TEXT = " (if mail-plain-text? "true" "false") "
FROM = " mail-from "
SENDMAIL_PATH = " sendmail-command "
SENDMAIL_ARGS = " (string-join sendmail-options " ") "
SENDMAIL_TIMEOUT = " sendmail-timeout "
SENDMAIL_CONVERT_CRLF = " (if sendmail-convert-crlf? "true" "false") "

[metrics]
ENABLED = " (if metrics? "true" "false") "
TOKEN = " metrics-token "

[actions]
ENABLED = " (if actions? "true" "false") "
DEFAULT_ACTIONS_URL = " default-actions-url "
"))))

(define (forgejo-shepherd-service config)
  (match-record config <forgejo-configuration>
                ( forgejo work-directory user group port
                  http-address protocol ssh-port
                  database-type database-host
                  configuration-wizard?
                  internal-token-file lfs-secret-file
                  smtp-password-file
                  tls-cert-file tls-key-file
                  oauth2-jwt-secret-file secret-key-file
                  shepherd-provision)
    (let* ((mappings (append
                      %network-file-mappings
                      (list %store-mapping ;XXX: coarse-grained
                            (file-system-mapping
                              (source "/etc/ssl/certs")
                              (target source))
                            (file-system-mapping
                              (source work-directory)
                              (target source)
                              (writable? #t))
                            (file-system-mapping
                              (source internal-token-file)
                              (target source))
                            (file-system-mapping
                              (source lfs-secret-file)
                              (target source))
                            (file-system-mapping
                              (source oauth2-jwt-secret-file)
                              (target source))
                            (file-system-mapping
                              (source secret-key-file)
                              (target source)))
                      (if (absolute-file-name? smtp-password-file)
                          (list (file-system-mapping
                                  (source smtp-password-file)
                                  (target source)))
                          '())
                      (if (absolute-file-name? tls-cert-file)
                          (list (file-system-mapping
                                  (source tls-cert-file)
                                  (target source)))
                          '())
                      (if (absolute-file-name? tls-key-file)
                          (list (file-system-mapping
                                  (source tls-key-file)
                                  (target source)))
                          '())
                      (if (and (not (string=? "sqlite3" database-type))
                               (absolute-file-name? database-host)) ;socket
                          (list (file-system-mapping
                                  (source database-host)
                                  (target source)
                                  (writable? #t)))
                          '())))
           (forgejo (file-append forgejo "/bin/forgejo"))
           (forgejo-wrapper (least-authority-wrapper
                             forgejo
                             #:name "forgejo-pola-wrapper"
                             #:user user #:group group
                             #:namespaces
                             ;; The wrapper needs access to the forgejo user
                             ;; to be able to mount
                             ;; /etc/forgejo/internal_token for example.
                             (fold delq %namespaces '(net user))
                             #:mappings mappings
                             #:preserved-environment-variables
                             (cons* "FORGEJO_WORK_DIR"
                                    "SSL_CERT_DIR"
                                    %default-preserved-environment-variables)))
           (config (forgejo-configuration->file config))
           (init-provision (list (symbol-append (first shepherd-provision)
                                                '-init))))
      (list (shepherd-service
              (documentation "Run Forgejo pre-start actions.")
              (requirement '(networking user-processes))
              (provision init-provision)
              (one-shot? #t)
              (modules (cons '(srfi srfi-19) %default-modules))
              (start
               #~(lambda _
                   (chdir #$work-directory)

                   (define app.ini "custom/conf/app.ini")

                   (define (symlink? x)
                     (eq? (stat:type (stat x)) 'symlink))

                   (define (delete-file/backup x)
                     (let* ((timestamp (date->string
                                        (current-date) "~4"))
                            (bak (string-append
                                  x "-" timestamp ".bak")))
                       (copy-file x bak)
                       (delete-file x)))

                   (define (fork+exec-forgejo . options)
                     ;; We can't change the environment in a one-shot
                     ;; service/Shepherd (is this a bug?), so use this wrapper
                     ;; of fork+exec-command that passes the needed
                     ;; environment variables and raises on unclean exit.
                     (let ((pid
                            (apply fork+exec-command
                                   (cons #$forgejo-wrapper options)
                                   (list #:environment-variables
                                         (list #$(string-append
                                                  "FORGEJO_WORK_DIR="
                                                  work-directory)
                                               #$(string-append
                                                  "HOME=" work-directory))))))
                       (unless (zero? (cdr (waitpid pid)))
                         (error "failed to run forgejo command"
                                #$forgejo-wrapper options))))

                   (unless #$configuration-wizard?
                     ;; When users transition from configuration-wizard? #t to
                     ;; #f, the stale wizard configuration is backed up.
                     (when (file-exists? app.ini)
                       (if (symlink? app.ini)
                           (delete-file app.ini)
                           (delete-file/backup app.ini)))
                     (symlink #$config app.ini))

                   (when (file-exists? app.ini)
                     ;; Initialize Forgejo's database.
                     (fork+exec-forgejo "migrate")
                     ;; Refresh binary file names captured in git hooks.
                     (fork+exec-forgejo "admin" "regenerate" "hooks")
                     ;; Update command option in 'authorized_keys' file.
                     (fork+exec-forgejo "admin" "regenerate" "keys")
                     ;; Sanity check configuration/environment.
                     (fork+exec-forgejo "doctor" "check" "--all")))))
            (shepherd-service
              (documentation "Run the Forgejo Git forge")
              (requirement (append init-provision
                                   '(networking
                                     user-processes)
                                   (if (privileged-port? ssh-port)
                                       '(ssh-daemon)
                                       '())
                                   (if (string=? "postgres" database-type)
                                       '(postgres)
                                       '())
                                   (if (string=? "mysql" database-type)
                                       '(mysql)
                                       '())))
              (provision shepherd-provision)
              (actions
               (list (shepherd-configuration-action
                      (if configuration-wizard?
                          (string-append work-directory "/custom/conf/app.ini")
                          config))))
              (start
               #~(make-systemd-constructor
                  (list #$forgejo-wrapper "web"
                        #$@(if configuration-wizard?
                               '()
                               #~("--config" #$config)))
                  (cond
                   (#$(string-suffix? "+unix" protocol)
                    (list (endpoint (make-socket-address AF_UNIX
                                                         #$http-address)
                                    #:socket-owner #$user
                                    #:socket-group #$group)))
                   (#$(string=? "0.0.0.0" http-address)
                    (list (endpoint (make-socket-address AF_INET
                                                         INADDR_ANY #$port))
                          (endpoint (make-socket-address AF_INET6
                                                         INADDR_ANY #$port))))
                   (#$(ipv6-address? http-address)
                    (list (endpoint (make-socket-address
                                     AF_INET6
                                     (inet-pton AF_INET6 #$http-address)
                                     #$port))))
                   (else                ;assumed ipv4
                    (list (endpoint (make-socket-address
                                     AF_INET
                                     (inet-pton AF_INET #$http-address)
                                     #$port)))))
                  #:log-file "/var/log/forgejo.log"
                  #:environment-variables
                  (list (string-append "HOME=" #$work-directory)
                        "SSL_CERT_DIR=/etc/ssl/certs")))
              (stop #~(make-systemd-destructor)))))))

(define %forgejo-accounts
  (match-record-lambda <forgejo-configuration>
      (user group work-directory)
    (list (user-group (name user)
                      (system? #t))
          (user-account
            (name user)
            (group group)
            (system? #t)
            (comment "Forgejo User")
            (home-directory work-directory)))))

(define %forgejo-activation
  (match-record-lambda <forgejo-configuration>
      ( forgejo user configuration-wizard? work-directory
        protocol http-address
        secret-key-file internal-token-file lfs-secret-file
        oauth2-jwt-secret-file)
    #~(begin
        (use-modules (gnu build activation)
                     (guix build utils)
                     (ice-9 match))

        (define secret-files `(("SECRET_KEY" . ,#$secret-key-file)
                               ("INTERNAL_TOKEN" . ,#$internal-token-file)
                               ("LFS_JWT_SECRET" . ,#$lfs-secret-file)
                               ("JWT_SECRET" . ,#$oauth2-jwt-secret-file)))

        (define forgejo #$(file-append forgejo "/bin/forgejo"))

        (define (generate-secret type file)
          (with-exception-handler
              (lambda (e)
                (false-if-exception (delete-file file))
                (raise-exception e))
            (lambda ()
              (call-with-output-file file
                (lambda (port)
                  (let ((pid (spawn forgejo
                                    (list forgejo "generate" "secret" type)
                                    #:output port)))
                    (match (waitpid pid)
                      ((_ . status)
                       (unless (zero? status)
                         (error "failed to generate forgejo secret" type))))))))))

        (let* ((owner (getpw #$user))
               (uid (passwd:uid owner))
               (gid (passwd:gid owner)))
          ;; Top-level directories of secrets; needed even when not using the
          ;; wizard.
          (for-each (match-lambda
                      ((_ . file)
                       (mkdir-p/perms (dirname file) owner #o750)))
                    secret-files)
          (when (and (string-suffix? "-unix" #$protocol)
                     (absolute-file-name? http-address))
            (mkdir-p/perms (dirname http-address) owner #o750))
          ;; To please 'forgejo doctor check --all' out of the box.
          (mkdir-p/perms #$work-directory owner #o750)
          (with-directory-excursion #$work-directory
            (mkdir-p/perms ".ssh" owner #o700)
            (mkdir-p/perms "custom" owner #o750)
            (mkdir-p/perms "custom/conf" owner #o750)
            (mkdir-p/perms "data" owner #o750)
            (mkdir-p/perms "data/forgejo-repositories" owner #o750)
            (mkdir-p/perms "log" owner #o750))
          (unless #$configuration-wizard?
            ;; Bootstrap secret files.
            (for-each (match-lambda
                        ((type . file)
                         (unless (file-exists? file)
                           (generate-secret type file))
                         (chown file uid gid)
                         (chmod file #o400)))
                      secret-files))))))

(define forgejo-service-type
  (service-type
    (name 'forgejo)
    (extensions
     (list (service-extension shepherd-root-service-type
                              forgejo-shepherd-service)
           (service-extension account-service-type
                              %forgejo-accounts)
           (service-extension activation-service-type
                              %forgejo-activation)
           (service-extension profile-service-type
                              (compose list forgejo-configuration-forgejo))))
    (default-value (forgejo-configuration))
    (description "Run Forgejo.")))
