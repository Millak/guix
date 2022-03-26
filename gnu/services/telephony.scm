;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 nee  <nee-git@hidamari.blue>
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu services telephony)
  #:use-module ((gnu build jami-service) #:select (account-fingerprint?))
  #:use-module ((gnu services) #:hide (delete))
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages jami)
  #:use-module (gnu packages telephony)
  #:use-module (guix deprecation)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (jami-account
            jami-account-archive
            jami-account-allowed-contacts
            jami-account-moderators
            jami-account-rendezvous-point?
            jami-account-discovery?
            jami-account-bootstrap-uri
            jami-account-name-server-uri

            jami-configuration
            jami-configuration-jamid
            jami-configuration-dbus
            jami-configuration-enable-logging?
            jami-configuration-debug?
            jami-configuration-auto-answer?
            jami-configuration-accounts

            jami-service-type

            mumble-server-configuration
            make-mumble-server-configuration
            mumble-server-configuration?
            mumble-server-configuration-package
            mumble-server-configuration-user
            mumble-server-configuration-group
            mumble-server-configuration-port
            mumble-server-configuration-welcome-text
            mumble-server-configuration-server-password
            mumble-server-configuration-max-users
            mumble-server-configuration-max-user-bandwidth
            mumble-server-configuration-database-file
            mumble-server-configuration-log-file
            mumble-server-configuration-pid-file
            mumble-server-configuration-autoban-attempts
            mumble-server-configuration-autoban-timeframe
            mumble-server-configuration-autoban-time
            mumble-server-configuration-opus-threshold
            mumble-server-configuration-channel-nesting-limit
            mumble-server-configuration-channelname-regex
            mumble-server-configuration-username-regex
            mumble-server-configuration-text-message-length
            mumble-server-configuration-image-message-length
            mumble-server-configuration-cert-required?
            mumble-server-configuration-remember-channel?
            mumble-server-configuration-allow-html?
            mumble-server-configuration-allow-ping?
            mumble-server-configuration-bonjour?
            mumble-server-configuration-send-version?
            mumble-server-configuration-log-days
            mumble-server-configuration-obfuscate-ips?
            mumble-server-configuration-ssl-cert
            mumble-server-configuration-ssl-key
            mumble-server-configuration-ssl-dh-params
            mumble-server-configuration-ssl-ciphers
            mumble-server-configuration-public-registration
            mumble-server-configuration-file

            mumble-server-public-registration-configuration
            make-mumble-server-public-registration-configuration
            mumble-server-public-registration-configuration?
            mumble-server-public-registration-configuration-name
            mumble-server-public-registration-configuration-url
            mumble-server-public-registration-configuration-password
            mumble-server-public-registration-configuration-hostname

            mumble-server-service-type))


;;;
;;; Jami daemon.
;;;

;;; XXX: Passing a computed-file object as the account is used for tests.
(define (string-or-computed-file? val)
  (or (string? val)
      (computed-file? val)))

(define (string-list? val)
  (and (list? val)
       (and-map string? val)))

(define (account-fingerprint-list? val)
  (and (list? val)
       (and-map account-fingerprint? val)))

(define-maybe string-list)

(define-maybe/no-serialization account-fingerprint-list)

(define-maybe boolean)

(define-maybe string)

;;; The following serializers are used to derive an account details alist from
;;; a <jami-account> record.
(define (serialize-string-list _ val)
  (string-join val ";"))

(define (serialize-boolean _ val)
  (format #f "~:[false~;true~]" val))

(define (serialize-string _ val)
  val)

;;; Note: Serialization is used to produce an account details alist that can
;;; be passed to the SET-ACCOUNT-DETAILS procedure.  Fields that do not map to
;;; a Jami account 'detail' should have their serialization disabled via the
;;; 'empty-serializer' procedure.
(define-configuration jami-account
  (archive
   (string-or-computed-file)
   "The account archive (backup) file name of the account.  This is used to
provision the account when the service starts.  The account archive should
@emph{not} be encrypted.  It is highly recommended to make it readable only to
the @samp{root} user (i.e., not in the store), to guard against leaking the
secret key material of the Jami account it contains."
   empty-serializer)
  (allowed-contacts
   (maybe-account-fingerprint-list 'disabled)
   "The list of allowed contacts for the account, entered as their 40
characters long fingerprint.  Messages or calls from accounts not in that list
will be rejected.  When unspecified, the configuration of the account archive
is used as-is with respect to contacts and public inbound calls/messaging
allowance, which typically defaults to allow any contact to communicate with
the account."
   empty-serializer)
  (moderators
   (maybe-account-fingerprint-list 'disabled)
   "The list of contacts that should have moderation privileges (to ban, mute,
etc. other users) in rendezvous conferences, entered as their 40 characters
long fingerprint.  When unspecified, the configuration of the account archive
is used as-is with respect to moderation, which typically defaults to allow
anyone to moderate."
   empty-serializer)
  ;; The serializable fields below are to be set with set-account-details.
  (rendezvous-point?
   (maybe-boolean 'disabled)
   "Whether the account should operate in the rendezvous mode.  In this mode,
all the incoming audio/video calls are mixed into a conference.  When left
unspecified, the value from the account archive prevails.")
  (peer-discovery?
   (maybe-boolean 'disabled)
   "Whether peer discovery should be enabled.  Peer discovery is used to
discover other OpenDHT nodes on the local network, which can be useful to
maintain communication between devices on such network even when the
connection to the the Internet has been lost.  When left unspecified, the
value from the account archive prevails.")
  (bootstrap-hostnames
   (maybe-string-list 'disabled)
   "A list of hostnames or IPs pointing to OpenDHT nodes, that should be used
to initially join the OpenDHT network.  When left unspecified, the value from
the account archive prevails.")
  (name-server-uri
   (maybe-string 'disabled)
   "The URI of the name server to use, that can be used to retrieve the
account fingerprint for a registered username."))

(define (jami-account->alist jami-account-object)
  "Serialize the JAMI-ACCOUNT object as an alist suitable to be passed to
SET-ACCOUNT-DETAILS."
  (define (field-name->account-detail name)
    (match name
      ('rendezvous-point? "Account.rendezVous")
      ('peer-discovery? "Account.peerDiscovery")
      ('bootstrap-hostnames "Account.hostname")
      ('name-server-uri "RingNS.uri")
      (_ #f)))

  (filter-map (lambda (field)
                (and-let* ((name (field-name->account-detail
                                  (configuration-field-name field)))
                           (value ((configuration-field-serializer field)
                                   name ((configuration-field-getter field)
                                         jami-account-object)))
                           ;; The define-maybe default serializer produces an
                           ;; empty string for the 'disabled value.
                           (value* (if (string-null? value)
                                       #f
                                       value)))
                  (cons name value*)))
              jami-account-fields))

(define (jami-account-list? val)
  (and (list? val)
       (and-map jami-account? val)))

(define-maybe/no-serialization jami-account-list)

(define-configuration/no-serialization jami-configuration
  (jamid
   (file-like libjami)
   "The Jami daemon package to use.")
  (dbus
   (file-like dbus)
   "The D-Bus package to use to start the required D-Bus session.")
  (nss-certs
   (file-like nss-certs)
   "The nss-certs package to use to provide TLS certificates.")
  (enable-logging?
   (boolean #t)
   "Whether to enable logging to syslog.")
  (debug?
   (boolean #f)
   "Whether to enable debug level messages.")
  (auto-answer?
   (boolean #f)
   "Whether to force automatic answer to incoming calls.")
  (accounts
   (maybe-jami-account-list 'disabled)
   "A list of Jami accounts to be (re-)provisioned every time the Jami daemon
service starts.  When providing this field, the account directories under
@file{/var/lib/jami/} are recreated every time the service starts, ensuring a
consistent state."))

(define %jami-accounts
  (list (user-group (name "jami") (system? #t))
        (user-account
         (name "jami")
         (group "jami")
         (system? #t)
         (comment "Jami daemon user")
         (home-directory "/var/lib/jami"))))

(define (jami-configuration->command-line-arguments config)
  "Derive the command line arguments to used to launch the Jami daemon from
CONFIG, a <jami-configuration> object."
  (match-record config <jami-configuration>
    (jamid dbus enable-logging? debug? auto-answer?)
    `(,(file-append jamid "/libexec/jamid")
      "--persistent"                    ;stay alive after client quits
      ,@(if enable-logging?
            '()                         ;logs go to syslog by default
            (list "--console"))         ;else stdout/stderr
      ,@(if debug?
            (list "--debug")
            '())
      ,@(if auto-answer?
            (list "--auto-answer")
            '()))))

(define (jami-dbus-session-activation config)
  "Create a directory to hold the Jami D-Bus session socket."
  (with-imported-modules (source-module-closure '((gnu build activation)))
    #~(begin
        (use-modules (gnu build activation))
        (let ((user (getpwnam "jami")))
          (mkdir-p/perms "/var/run/jami" user #o700)))))

(define (jami-shepherd-services config)
  "Return a <shepherd-service> running the Jami daemon."
  (let* ((jamid (jami-configuration-jamid config))
         (nss-certs (jami-configuration-nss-certs config))
         (dbus (jami-configuration-dbus config))
         (dbus-daemon (file-append dbus "/bin/dbus-daemon"))
         (dbus-send (file-append dbus "/bin/dbus-send"))
         (accounts (jami-configuration-accounts config))
         (declarative-mode? (not (eq? 'disabled accounts))))

    (with-imported-modules (source-module-closure
                            '((gnu build jami-service)
                              (gnu build shepherd)
                              (gnu system file-systems)))

      (define list-accounts-action
        (shepherd-action
         (name 'list-accounts)
         (documentation "List the available Jami accounts.  Return the account
details alists keyed by their account username.")
         (procedure
          #~(lambda _
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                ;; Print the accounts summary or long listing, according to
                ;; user-provided option.
                (let* ((usernames (get-usernames))
                       (accounts (map-in-order username->account usernames)))
                  (match accounts
                    (()                 ;empty list
                     (format #t "There is no Jami account available.~%"))
                    ((one two ...)
                     (format #t "The following Jami accounts are available:~%")
                     (for-each
                      (lambda (account)
                        (define fingerprint (assoc-ref account
                                                       "Account.username"))
                        (define human-friendly-name
                          (or (assoc-ref account
                                         "Account.registeredName")
                              (assoc-ref account
                                         "Account.displayName")
                              (assoc-ref account
                                         "Account.alias")))
                        (define disabled?
                          (and=> (assoc-ref account "Account.enable")
                                 (cut string=? "false" <>)))

                        (format #t "  - ~a~@[ (~a)~] ~:[~;[disabled]~]~%"
                                fingerprint human-friendly-name disabled?))
                      accounts)
                     (display "\n")))
                  ;; Return the account-details-list alist.
                  (map cons usernames accounts)))))))

      (define list-account-details-action
        (shepherd-action
         (name 'list-account-details)
         (documentation "Display the account details of the available Jami
accounts in the @code{recutils} format.  Return the account details alists
keyed by their account username.")
         (procedure
          #~(lambda _
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (let* ((usernames (get-usernames))
                       (accounts (map-in-order username->account usernames)))
                  (for-each (lambda (account)
                              (display (account-details->recutil account))
                              (display "\n\n"))
                            accounts)
                  (map cons usernames accounts)))))))

      (define list-contacts-action
        (shepherd-action
         (name 'list-contacts)
         (documentation "Display the contacts for each Jami account.  Return
an alist containing the contacts keyed by the account usernames.")
         (procedure
          #~(lambda _
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (let* ((usernames (get-usernames))
                       (contacts (map-in-order username->contacts usernames)))
                  (for-each (lambda (username contacts)
                              (format #t "Contacts for account ~a:~%"
                                      username)
                              (format #t "~{  - ~a~%~}~%" contacts))
                            usernames contacts)
                  (map cons usernames contacts)))))))

      (define list-moderators-action
        (shepherd-action
         (name 'list-moderators)
         (documentation "Display the moderators for each Jami account.  Return
an alist containing the moderators keyed by the account usernames.")
         (procedure
          #~(lambda _
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (let* ((usernames (get-usernames))
                       (moderators (map-in-order username->moderators
                                                 usernames)))
                  (for-each
                   (lambda (username moderators)
                     (if (username->all-moderators? username)
                         (format #t "Anyone can moderate for account ~a~%"
                                 username)
                         (begin
                           (format #t "Moderators for account ~a:~%" username)
                           (format #t "~{  - ~a~%~}~%" moderators))))
                   usernames moderators)
                  (map cons usernames moderators)))))))

      (define add-moderator-action
        (shepherd-action
         (name 'add-moderator)
         (documentation "Add a moderator for a given Jami account.  The
MODERATOR contact must be given as its 40 characters fingerprint, while the
Jami account can be provided as its registered USERNAME or fingerprint.

@example
herd add-moderator jami 1dbcb0f5f37324228235564b79f2b9737e9a008f username
@end example

Return the moderators for the account known by USERNAME.")
         (procedure
          #~(lambda (_ moderator username)
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (set-all-moderators #f username)
                (add-contact moderator username)
                (set-moderator moderator #t username)
                (username->moderators username))))))

      (define ban-contact-action
        (shepherd-action
         (name 'ban-contact)
         (documentation "Ban a contact for a given or all Jami accounts, and
clear their moderator flag.  The CONTACT must be given as its 40 characters
fingerprint, while the Jami account can be provided as its registered USERNAME
or fingerprint, or omitted.  When the account is omitted, CONTACT is banned
from all accounts.

@example
herd ban-contact jami 1dbcb0f5f37324228235564b79f2b9737e9a008f [username]
@end example")
         (procedure
          #~(lambda* (_ contact #:optional username)
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (let ((usernames (or (and=> username list)
                                     (get-usernames))))
                  (for-each (lambda (username)
                              (set-moderator contact #f username)
                              (remove-contact contact username #:ban? #t))
                            usernames)))))))

      (define list-banned-contacts-action
        (shepherd-action
         (name 'list-banned-contacts)
         (documentation "List the banned contacts for each accounts.  Return
an alist of the banned contacts, keyed by the account usernames.")
         (procedure
          #~(lambda _
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))

                (define banned-contacts
                  (let ((usernames (get-usernames)))
                    (map cons usernames
                         (map-in-order (lambda (x)
                                         (receive (_ banned)
                                             (username->contacts x)
                                           banned))
                                       usernames))))

                (for-each (match-lambda
                            ((username . banned)
                             (unless (null? banned)
                               (format #t "Banned contacts for account ~a:~%"
                                       username)
                               (format #t "~{  - ~a~%~}~%" banned))))
                          banned-contacts)
                banned-contacts)))))

      (define enable-account-action
        (shepherd-action
         (name 'enable-account)
         (documentation "Enable an account.  It takes USERNAME as an argument,
either a registered username or the fingerprint of the account.")
         (procedure
          #~(lambda (_ username)
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (enable-account username))))))

      (define disable-account-action
        (shepherd-action
         (name 'disable-account)
         (documentation "Disable an account.  It takes USERNAME as an
argument, either a registered username or the fingerprint of the account.")
         (procedure
          #~(lambda (_ username)
              (parameterize ((%send-dbus-binary #$dbus-send)
                             (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                             (%send-dbus-user   "jami")
                             (%send-dbus-group  "jami"))
                (disable-account username))))))

      (list (shepherd-service
             (documentation "Run a D-Bus session for the Jami daemon.")
             (provision '(jami-dbus-session))
             (modules `((gnu build shepherd)
                        (gnu build jami-service)
                        (gnu system file-systems)
                        ,@%default-modules))
             ;; The requirement on dbus-system is to ensure other required
             ;; activation for D-Bus, such as a /etc/machine-id file.
             (requirement '(dbus-system syslogd))
             (start
              #~(lambda args
                  (define pid
                    ((make-forkexec-constructor/container
                      (list #$dbus-daemon "--session"
                            "--address=unix:path=/var/run/jami/bus"
                            "--nofork" "--syslog-only" "--nopidfile")
                      #:mappings (list (file-system-mapping
                                        (source "/dev/log") ;for syslog
                                        (target source))
                                       (file-system-mapping
                                        (source "/var/run/jami")
                                        (target source)
                                        (writable? #t)))
                      #:user "jami"
                      #:group "jami"
                      #:environment-variables
                      ;; This is so that the cx.ring.Ring service D-Bus
                      ;; definition is found by dbus-send.
                      (list (string-append "XDG_DATA_DIRS="
                                           #$jamid "/share")))))

                  ;; XXX: This manual synchronization probably wouldn't be
                  ;; needed if we were using a PID file, but providing it via a
                  ;; customized config file with <pidfile> would not override
                  ;; the one inherited from the base config of D-Bus.
                  (let ((sock (socket PF_UNIX SOCK_STREAM 0)))
                    (with-retries 20 1 (catch 'system-error
                                         (lambda ()
                                           (connect sock AF_UNIX
                                                    "/var/run/jami/bus")
                                           (close-port sock)
                                           #t)
                                         (lambda args
                                           #f))))

                  pid))
             (stop #~(make-kill-destructor)))

            (shepherd-service
             (documentation "Run the Jami daemon.")
             (provision '(jami))
             (actions (list list-accounts-action
                            list-account-details-action
                            list-contacts-action
                            list-moderators-action
                            add-moderator-action
                            ban-contact-action
                            list-banned-contacts-action
                            enable-account-action
                            disable-account-action))
             (requirement '(jami-dbus-session))
             (modules `((ice-9 format)
                        (ice-9 ftw)
                        (ice-9 match)
                        (ice-9 receive)
                        (srfi srfi-1)
                        (srfi srfi-26)
                        (gnu build jami-service)
                        (gnu build shepherd)
                        (gnu system file-systems)
                        ,@%default-modules))
             (start
              #~(lambda args
                  (define (delete-file-recursively/safe file)
                    ;; Ensure we're not deleting things outside of
                    ;; /var/lib/jami.  This prevents a possible attack in case
                    ;; the daemon is compromised and an attacker gains write
                    ;; access to /var/lib/jami.
                    (let ((parent-directory (dirname file)))
                      (if (eq? 'symlink (stat:type (stat parent-directory)))
                          (error "abnormality detected; unexpected symlink found at"
                                 parent-directory)
                          (delete-file-recursively file))))

                  (when #$declarative-mode?
                    ;; Clear the Jami configuration and accounts, to enforce the
                    ;; declared state.
                    (catch #t
                      (lambda ()
                        (for-each (cut delete-file-recursively/safe <>)
                                  '("/var/lib/jami/.cache/jami"
                                    "/var/lib/jami/.config/jami"
                                    "/var/lib/jami/.local/share/jami"
                                    "/var/lib/jami/accounts")))
                      (lambda args
                        #t))
                    ;; Copy the Jami account archives from somewhere readable
                    ;; by root to a place only the jami user can read.
                    (let* ((accounts-dir "/var/lib/jami/accounts/")
                           (pwd (getpwnam "jami"))
                           (user (passwd:uid pwd))
                           (group (passwd:gid pwd)))
                      (mkdir-p accounts-dir)
                      (chown accounts-dir user group)
                      (for-each (lambda (f)
                                  (let ((dest (string-append accounts-dir
                                                             (basename f))))
                                    (copy-file f dest)
                                    (chown dest user group)))
                                '#$(and declarative-mode?
                                        (map jami-account-archive accounts)))))

                  ;; Start the daemon.
                  (define daemon-pid
                    ((make-forkexec-constructor/container
                      '#$(jami-configuration->command-line-arguments config)
                      #:mappings
                      (list (file-system-mapping
                             (source "/dev/log") ;for syslog
                             (target source))
                            (file-system-mapping
                             (source "/var/lib/jami")
                             (target source)
                             (writable? #t))
                            (file-system-mapping
                             (source "/var/run/jami")
                             (target source)
                             (writable? #t))
                            ;; Expose TLS certificates for GnuTLS.
                            (file-system-mapping
                             (source #$(file-append nss-certs "/etc/ssl/certs"))
                             (target "/etc/ssl/certs")))
                      #:user "jami"
                      #:group "jami"
                      #:environment-variables
                      (list (string-append "DBUS_SESSION_BUS_ADDRESS="
                                           "unix:path=/var/run/jami/bus")
                            ;; Expose TLS certificates for OpenSSL.
                            "SSL_CERT_DIR=/etc/ssl/certs"))))

                  (parameterize ((%send-dbus-binary #$dbus-send)
                                 (%send-dbus-bus    "unix:path=/var/run/jami/bus")
                                 (%send-dbus-user   "jami")
                                 (%send-dbus-group  "jami"))

                    ;; Wait until the service name has been acquired by D-Bus.
                    (with-retries 20 1
                      (dbus-service-available? "cx.ring.Ring"))

                    (when #$declarative-mode?
                      ;; Provision the accounts via the D-Bus API of the daemon.
                      (let* ((jami-account-archives
                              (map (cut string-append
                                        "/var/lib/jami/accounts/" <>)
                                   (scandir "/var/lib/jami/accounts/"
                                            (lambda (f)
                                              (not (member f '("." "..")))))))
                             (usernames (map-in-order (cut add-account <>)
                                                      jami-account-archives)))

                        (define (archive-name->username archive)
                          (list-ref
                           usernames
                           (list-index (lambda (f)
                                         (string-suffix? (basename archive) f))
                                       jami-account-archives)))

                        (for-each
                         (lambda (archive allowed-contacts moderators
                                          account-details)
                           (let ((username (archive-name->username
                                            archive)))
                             (when (not (eq? 'disabled allowed-contacts))
                               ;; Reject calls from unknown contacts.
                               (set-account-details
                                '(("DHT.PublicInCalls" . "false")) username)
                               ;; Remove all contacts.
                               (for-each (cut remove-contact <> username)
                                         (username->contacts username))
                               ;; Add allowed ones.
                               (for-each (cut add-contact <> username)
                                         allowed-contacts))
                             (when (not (eq? 'disabled moderators))
                               ;; Disable the 'AllModerators' property.
                               (set-all-moderators #f username)
                               ;; Remove all moderators.
                               (for-each (cut set-moderator <> #f username)
                                         (username->moderators username))
                               ;; Add declared moderators.
                               (for-each (cut set-moderator <> #t username)
                                         moderators))
                             ;; Set the various account parameters.
                             (set-account-details account-details username)))
                         '#$(and declarative-mode?
                                 (map-in-order (cut jami-account-archive <>)
                                               accounts))
                         '#$(and declarative-mode?
                                 (map-in-order
                                  (cut jami-account-allowed-contacts <>)
                                  accounts))
                         '#$(and declarative-mode?
                                 (map-in-order (cut jami-account-moderators <>)
                                               accounts))
                         '#$(and declarative-mode?
                                 (map-in-order jami-account->alist accounts))))))

                  ;; Finally, return the PID of the daemon process.
                  daemon-pid))
             (stop
              #~(lambda (pid . args)
                  (kill pid SIGKILL)
                  ;; Wait for the process to exit; this prevents overlapping
                  ;; processes when issuing 'herd restart'.
                  (waitpid pid)
                  #f)))))))

(define jami-service-type
  (service-type
   (name 'jami)
   (default-value (jami-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             jami-shepherd-services)
          (service-extension account-service-type
                             (const %jami-accounts))
          (service-extension activation-service-type
                             jami-dbus-session-activation)))
   (description "Run the Jami daemon (@command{jamid}).  This service is
geared toward the use case of hosting Jami rendezvous points over a headless
server.  If you use Jami on your local machine, you may prefer to setup a user
Shepherd service for it instead; this way, the daemon will be shared via your
normal user D-Bus session bus.")))


;;;
;;; Mumble server.
;;;

;; https://github.com/mumble-voip/mumble/blob/master/scripts/murmur.ini

(define-record-type* <mumble-server-configuration> mumble-server-configuration
  make-mumble-server-configuration
  mumble-server-configuration?
  (package               mumble-server-configuration-package ;file-like
                         (default mumble))
  (user                  mumble-server-configuration-user
                         (default "mumble-server"))
  (group                 mumble-server-configuration-group
                         (default "mumble-server"))
  (port                  mumble-server-configuration-port
                         (default 64738))
  (welcome-text          mumble-server-configuration-welcome-text
                         (default ""))
  (server-password       mumble-server-configuration-server-password
                         (default ""))
  (max-users             mumble-server-configuration-max-users
                         (default 100))
  (max-user-bandwidth    mumble-server-configuration-max-user-bandwidth
                         (default #f))
  (database-file         mumble-server-configuration-database-file
                         (default "/var/lib/mumble-server/db.sqlite"))
  (log-file              mumble-server-configuration-log-file
                         (default "/var/log/mumble-server/mumble-server.log"))
  (pid-file              mumble-server-configuration-pid-file
                         (default "/var/run/mumble-server/mumble-server.pid"))
  (autoban-attempts      mumble-server-configuration-autoban-attempts
                         (default 10))
  (autoban-timeframe     mumble-server-configuration-autoban-timeframe
                         (default 120))
  (autoban-time          mumble-server-configuration-autoban-time
                         (default 300))
  (opus-threshold        mumble-server-configuration-opus-threshold
                         (default 100)) ; integer percent
  (channel-nesting-limit mumble-server-configuration-channel-nesting-limit
                         (default 10))
  (channelname-regex     mumble-server-configuration-channelname-regex
                         (default #f))
  (username-regex        mumble-server-configuration-username-regex
                         (default #f))
  (text-message-length   mumble-server-configuration-text-message-length
                         (default 5000))
  (image-message-length  mumble-server-configuration-image-message-length
                         (default (* 128 1024))) ; 128 Kilobytes
  (cert-required?         mumble-server-configuration-cert-required?
                          (default #f))
  (remember-channel?     mumble-server-configuration-remember-channel?
                         (default #f))
  (allow-html?           mumble-server-configuration-allow-html?
                         (default #f))
  (allow-ping?           mumble-server-configuration-allow-ping?
                         (default #f))
  (bonjour?              mumble-server-configuration-bonjour?
                         (default #f))
  (send-version?         mumble-server-configuration-send-version?
                         (default #f))
  (log-days              mumble-server-configuration-log-days
                         (default 31))
  (obfuscate-ips?        mumble-server-obfuscate-ips?
                         (default #t))
  (ssl-cert              mumble-server-configuration-ssl-cert
                         (default #f))
  (ssl-key               mumble-server-configuration-ssl-key
                         (default #f))
  (ssl-dh-params         mumble-server-configuration-ssl-dh-params
                         (default #f))
  (ssl-ciphers           mumble-server-configuration-ssl-ciphers
                         (default #f))
  (public-registration   mumble-server-configuration-public-registration
                         (default #f))  ; <mumble-server-public-registration-configuration>
  (file                  mumble-server-configuration-file
                         (default #f)))

(define-record-type* <mumble-server-public-registration-configuration>
  mumble-server-public-registration-configuration
  make-mumble-server-public-registration-configuration
  mumble-server-public-registration-configuration?
  (name         mumble-server-public-registration-configuration-name)
  (password     mumble-server-public-registration-configuration-password)
  (url          mumble-server-public-registration-configuration-url)
  (hostname     mumble-server-public-registration-configuration-hostname
                (default #f)))

(define (flatten . lst)
  "Return a list that recursively concatenates all sub-lists of LST."
  (define (flatten1 head out)
    (if (list? head)
        (fold-right flatten1 out head)
        (cons head out)))
  (fold-right flatten1 '() lst))

(define (default-mumble-server-config config)
  (match-record
   config
   <mumble-server-configuration>
   (user port welcome-text server-password max-users max-user-bandwidth
    database-file log-file pid-file autoban-attempts autoban-timeframe
    autoban-time opus-threshold channel-nesting-limit channelname-regex
    username-regex text-message-length image-message-length cert-required?
    remember-channel? allow-html? allow-ping? bonjour? send-version?
    log-days obfuscate-ips? ssl-cert ssl-key ssl-dh-params ssl-ciphers
    public-registration)
   (apply mixed-text-file "mumble-server.ini"
          (flatten
           "welcometext=" welcome-text "\n"
           "port=" (number->string port) "\n"
           (if server-password (list "serverpassword=" server-password "\n") '())
           (if max-user-bandwidth (list "bandwidth="
                                        (number->string max-user-bandwidth) "\n")
               '())
           "users=" (number->string max-users) "\n"
           "uname=" user "\n"
           "database=" database-file "\n"
           "logfile=" log-file "\n"
           "pidfile=" pid-file "\n"
           (if autoban-attempts (list "autobanAttempts=" (number->string autoban-attempts) "\n") '())
           (if autoban-timeframe (list "autobanTimeframe=" (number->string autoban-timeframe) "\n") '())
           (if autoban-time (list "autobanTime=" (number->string autoban-time) "\n") '())
           (if opus-threshold (list "opusthreshold=" (number->string opus-threshold) "\n") '())
           (if channel-nesting-limit (list "channelnestinglimit=" (number->string channel-nesting-limit) "\n") '())
           (if channelname-regex (list "channelname=" channelname-regex "\n") '())
           (if username-regex (list "username=" username-regex "\n") '())
           (if text-message-length (list "textmessagelength=" (number->string text-message-length) "\n") '())
           (if image-message-length (list "imagemessagelength=" (number->string image-message-length) "\n") '())
           (if log-days (list "logdays=" (number->string log-days) "\n") '())
           "obfuscate=" (if obfuscate-ips? "true" "false") "\n"
           "certrequired=" (if cert-required? "true" "false") "\n"
           "rememberchannel=" (if remember-channel? "true" "false") "\n"
           "allowhtml=" (if allow-html? "true" "false") "\n"
           "allowping=" (if allow-ping? "true" "false") "\n"
           "bonjour=" (if bonjour? "true" "false") "\n"
           "sendversion=" (if send-version? "true" "false") "\n"
           (cond ((and ssl-cert ssl-key)
                  (list
                   "sslCert=" ssl-cert "\n"
                   "sslKey=" ssl-key "\n"))
                 ((or ssl-cert ssl-key)
                  (error "ssl-cert and ssl-key must both be set"
                         ssl-cert ssl-key))
                 (else '()))
           (if ssl-dh-params (list "sslDHParams=" ssl-dh-params) '())
           (if ssl-ciphers (list "sslCiphers=" ssl-ciphers) '())

           (match public-registration
             (#f '())
             (($ <mumble-server-public-registration-configuration>
                 name password url hostname)
              (if (and (or (not server-password) (string-null? server-password))
                       allow-ping?)
                  (list
                   "registerName=" name "\n"
                   "registerPassword=" password "\n"
                   "registerUrl=" url "\n"
                   (if hostname
                       (string-append "registerHostname=" hostname "\n")
                       ""))
                  (error "To publicly register your mumble-server server your server must be publicy visible
and users must be able to join without a password. To fix this set:
(allow-ping? #t)
(server-password \"\")
Or set public-registration to #f"))))))))

(define (mumble-server-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let* ((log-dir (dirname #$(mumble-server-configuration-log-file config)))
             (pid-dir (dirname #$(mumble-server-configuration-pid-file config)))
             (db-dir (dirname #$(mumble-server-configuration-database-file config)))
             (user (getpwnam #$(mumble-server-configuration-user config)))
             (init-dir
              (lambda (name dir)
                (format #t "creating mumble-server ~a directory '~a'\n" name dir)
                (mkdir-p dir)
                (chown dir (passwd:uid user) (passwd:gid user))
                (chmod dir #o700)))
             (ini #$(or (mumble-server-configuration-file config)
                        (default-mumble-server-config config))))
        (init-dir "log" log-dir)
        (init-dir "pid" pid-dir)
        (init-dir "database" db-dir)

        (format #t "mumble-server: use config file: ~a~%\n" ini)
        (format #t "mumble-server: to set the SuperUser password run:
    `~a -ini ~a -readsupw`\n"
                #$(file-append (mumble-server-configuration-package config)
                               "/bin/mumble-server") ini)
        #t)))

(define mumble-server-accounts
  (match-lambda
    (($ <mumble-server-configuration> _ user group)
     (list
      (user-group
       (name group)
       (system? #t))
      (user-account
       (name user)
       (group group)
       (system? #t)
       (comment "Mumble server daemon")
       (home-directory "/var/empty")
       (shell (file-append shadow "/sbin/nologin")))))))

(define (mumble-server-shepherd-service config)
  (list (shepherd-service
         (provision '(mumble-server))
         (documentation "Run the Mumble server.")
         (requirement '(networking))
         (start #~(make-forkexec-constructor
                   '(#$(file-append (mumble-server-configuration-package config)
                                    "/bin/mumble-server")
                     "-ini"
                     #$(or (mumble-server-configuration-file config)
                           (default-mumble-server-config config)))
                   #:pid-file #$(mumble-server-configuration-pid-file config)))
         (stop #~(make-kill-destructor)))))

(define mumble-server-service-type
  (service-type (name 'mumble-server)
                (description
                 "Run the Mumble voice-over-IP (VoIP) server.")
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          mumble-server-shepherd-service)
                       (service-extension activation-service-type
                                          mumble-server-activation)
                       (service-extension account-service-type
                                          mumble-server-accounts)))
                (default-value (mumble-server-configuration))))

(define-deprecated/public-alias
  murmur-configuration
  mumble-server-configuration)
(define-deprecated/public-alias
  make-murmur-configuration
  make-mumble-server-configuration)
(define-deprecated/public-alias
  murmur-configuration?
  mumble-server-configuration?)
(define-deprecated/public-alias
  murmur-configuration-package
  mumble-server-configuration-package)
(define-deprecated/public-alias
  murmur-configuration-user
  mumble-server-configuration-user)
(define-deprecated/public-alias
  murmur-configuration-group
  mumble-server-configuration-group)
(define-deprecated/public-alias
  murmur-configuration-port
  mumble-server-configuration-port)
(define-deprecated/public-alias
  murmur-configuration-welcome-text
  mumble-server-configuration-welcome-text)
(define-deprecated/public-alias
  murmur-configuration-server-password
  mumble-server-configuration-server-password)
(define-deprecated/public-alias
  murmur-configuration-max-users
  mumble-server-configuration-max-users)
(define-deprecated/public-alias
  murmur-configuration-max-user-bandwidth
  mumble-server-configuration-max-user-bandwidth)
(define-deprecated/public-alias
  murmur-configuration-database-file
  mumble-server-configuration-database-file)
(define-deprecated/public-alias
  murmur-configuration-log-file
  mumble-server-configuration-log-file)
(define-deprecated/public-alias
  murmur-configuration-pid-file
  mumble-server-configuration-pid-file)
(define-deprecated/public-alias
  murmur-configuration-autoban-attempts
  mumble-server-configuration-autoban-attempts)
(define-deprecated/public-alias
  murmur-configuration-autoban-timeframe
  mumble-server-configuration-autoban-timeframe)
(define-deprecated/public-alias
  murmur-configuration-autoban-time
  mumble-server-configuration-autoban-time)
(define-deprecated/public-alias
  murmur-configuration-opus-threshold
  mumble-server-configuration-opus-threshold)
(define-deprecated/public-alias
  murmur-configuration-channel-nesting-limit
  mumble-server-configuration-channel-nesting-limit)
(define-deprecated/public-alias
  murmur-configuration-channelname-regex
  mumble-server-configuration-channelname-regex)
(define-deprecated/public-alias
  murmur-configuration-username-regex
  mumble-server-configuration-username-regex)
(define-deprecated/public-alias
  murmur-configuration-text-message-length
  mumble-server-configuration-text-message-length)
(define-deprecated/public-alias
  murmur-configuration-image-message-length
  mumble-server-configuration-image-message-length)
(define-deprecated/public-alias
  murmur-configuration-cert-required?
  mumble-server-configuration-cert-required?)
(define-deprecated/public-alias
  murmur-configuration-remember-channel?
  mumble-server-configuration-remember-channel?)
(define-deprecated/public-alias
  murmur-configuration-allow-html?
  mumble-server-configuration-allow-html?)
(define-deprecated/public-alias
  murmur-configuration-allow-ping?
  mumble-server-configuration-allow-ping?)
(define-deprecated/public-alias
  murmur-configuration-bonjour?
  mumble-server-configuration-bonjour?)
(define-deprecated/public-alias
  murmur-configuration-send-version?
  mumble-server-configuration-send-version?)
(define-deprecated/public-alias
  murmur-configuration-log-days
  mumble-server-configuration-log-days)
(define-deprecated/public-alias
  murmur-configuration-obfuscate-ips?
  mumble-server-configuration-obfuscate-ips?)
(define-deprecated/public-alias
  murmur-configuration-ssl-cert
  mumble-server-configuration-ssl-cert)
(define-deprecated/public-alias
  murmur-configuration-ssl-key
  mumble-server-configuration-ssl-key)
(define-deprecated/public-alias
  murmur-configuration-ssl-dh-params
  mumble-server-configuration-ssl-dh-params)
(define-deprecated/public-alias
  murmur-configuration-ssl-ciphers
  mumble-server-configuration-ssl-ciphers)
(define-deprecated/public-alias
  murmur-configuration-public-registration
  mumble-server-configuration-public-registration)
(define-deprecated/public-alias
  murmur-configuration-file
  mumble-server-configuration-file)

(define-deprecated/public-alias
  murmur-public-registration-configuration
  mumble-server-public-registration-configuration)
(define-deprecated/public-alias
  make-murmur-public-registration-configuration
  make-mumble-server-public-registration-configuration)
(define-deprecated/public-alias
  murmur-public-registration-configuration?
  mumble-server-public-registration-configuration?)
(define-deprecated/public-alias
  murmur-public-registration-configuration-name
  mumble-server-public-registration-configuration-name)
(define-deprecated/public-alias
  murmur-public-registration-configuration-url
  mumble-server-public-registration-configuration-url)
(define-deprecated/public-alias
  murmur-public-registration-configuration-password
  mumble-server-public-registration-configuration-password)
(define-deprecated/public-alias
  murmur-public-registration-configuration-hostname
  mumble-server-public-registration-configuration-hostname)

(define-deprecated/public-alias
  murmur-service-type
  mumble-server-service-type)

;; Local Variables:
;; eval: (put 'with-retries 'scheme-indent-function 2)
;; End:
