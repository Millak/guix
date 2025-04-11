;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2015, 2017-2020, 2022-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Pierre-Antoine Rouby <contact@parouby.fr>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu services messaging)
  #:use-module ((gnu home services utils) #:select (object->camel-case-string))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages tls)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu system shadow)
  #:autoload   (gnu build linux-container) (%namespaces)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix least-authority)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (prosody-service-type
            prosody-configuration
            opaque-prosody-configuration

            virtualhost-configuration
            int-component-configuration
            ext-component-configuration

            mod-muc-configuration
            ssl-configuration

            %default-modules-enabled
            prosody-configuration-pidfile

            bitlbee-configuration
            bitlbee-configuration?
            bitlbee-service-type

            ngircd-service-type
            ngircd-configuration
            ngircd-configuration?
            ngircd-configuration-ngircd
            ngircd-configuration-debug?
            ngircd-configuration-shepherd-requirement
            ngircd-configuration-global
            ngircd-configuration-limits
            ngircd-configuration-options
            ngircd-configuration-ssl
            ngircd-configuration-operators
            ngircd-configuration-servers
            ngircd-configuration-channels
            ngircd-global
            ngircd-global?
            ngircd-global-name
            ngircd-global-admin-info-1
            ngircd-global-admin-info-2
            ngircd-global-admin-email
            ngircd-global-help-file
            ngircd-global-info
            ngircd-global-listen
            ngircd-global-motd-file
            ngircd-global-motd-phrase
            ngircd-global-network
            ngircd-global-password
            ngircd-global-pid-file
            ngircd-global-ports
            ngircd-global-server-uid
            ngircd-global-server-gid
            ngircd-limits
            ngircd-limits?
            ngircd-limits-connect-retry
            ngircd-limits-max-connections
            ngircd-limits-max-connections-ip
            ngircd-limits-max-joins
            ngircd-limits-max-list-size
            ngircd-limits-ping-timeout
            ngircd-limits-pong-timeout
            ngircd-options
            ngircd-options?
            ngircd-options-allowed-channel-types
            ngircd-options-allow-remote-oper?
            ngircd-options-connect-ipv4?
            ngircd-options-connect-ipv6?
            ngircd-options-dns?
            ngircd-options-more-privacy?
            ngircd-options-notice-before-registration?
            ngircd-options-oper-can-use-mode?
            ngircd-options-oper-chan-p-auto-op?
            ngircd-options-oper-server-mode?
            ngircd-options-pam?
            ngircd-options-pam-is-optional?
            ngircd-options-require-auth-ping?
            ngircd-ssl
            ngircd-ssl?
            ngircd-ssl-cert-file
            ngircd-ssl-key-file
            ngircd-ssl-ca-file
            ngircd-ssl-ports
            ngircd-ssl-cipher-list
            ngircd-ssl-dh-file
            ngircd-operator
            ngircd-operator?
            ngircd-operator-name
            ngircd-operator-password
            ngircd-operator-mask
            ngircd-server
            ngircd-server?
            ngircd-server-name
            ngircd-server-host
            ngircd-server-my-password
            ngircd-server-peer-password
            ngircd-server-bind
            ngircd-server-port
            ngircd-server-group
            ngircd-server-passive?
            ngircd-server-ssl-connect?
            ngircd-server-ssl-verify?
            ngircd-channel
            ngircd-channel?
            ngircd-channel-name
            ngircd-channel-topic
            ngircd-channel-modes
            ngircd-channel-key-file

            quassel-configuration
            quassel-service-type

            snuik-configuration
            snuik-service-type))

;;; Commentary:
;;;
;;; Messaging services.
;;;
;;; Code:

(define-syntax define-all-configurations
  (lambda (stx)
    (define-syntax-rule (id ctx parts ...)
      "Assemble PARTS into a raw (unhygienic) identifier."
      (datum->syntax ctx (symbol-append (syntax->datum parts) ...)))
    (define (make-pred arg)
      (lambda (field target)
        (and (memq (syntax->datum target) `(common ,arg)) field)))
    (syntax-case stx ()
      ;; TODO Also handle (field-type) form, without a default.
      ((_ stem (field (field-type def) doc target) ...)
       (with-syntax (((new-field-type ...)
                      (map (lambda (field-type target)
                             (if (and (eq? 'common (syntax->datum target))
                                      (not (string-prefix?
                                            "maybe-"
                                            (symbol->string
                                             (syntax->datum field-type)))))
                                 (id #'stem #'maybe- field-type) field-type))
                           #'(field-type ...) #'(target ...)))
                     ((new-def ...)
                      (map (lambda (def target)
                             (if (eq? 'common (syntax->datum target))
                                 ;; TODO Use the %unset-value variable, or
                                 ;; even better just simplify this so that it
                                 ;; doesn't interfere with
                                 ;; define-configuration and define-maybe
                                 ;; internals.
                                 #''%unset-marker% def))
                           #'(def ...) #'(target ...)))
                     ((new-doc ...)
                      (map (lambda (doc target)
                             (if (eq? 'common (syntax->datum target))
                                 "" doc))
                           #'(doc ...) #'(target ...))))
         #`(begin
             (define #,(id #'stem #'common-fields)
               '(#,@(filter-map (make-pred #f) #'(field ...) #'(target ...))))
             (define-configuration #,(id #'stem #'prosody-configuration)
               #,@(filter-map (make-pred 'global)
                              #'((field (field-type def) doc) ...)
                              #'(target ...)))
             (define-configuration #,(id #'stem #'virtualhost-configuration)
               #,@(filter-map (make-pred 'virtualhost)
                              #'((field (new-field-type new-def) new-doc) ...)
                              #'(target ...)))
             (define-configuration #,(id #'stem #'int-component-configuration)
               #,@(filter-map (make-pred 'int-component)
                              #'((field (new-field-type new-def) new-doc) ...)
                              #'(target ...)))
             (define-configuration #,(id #'stem #'ext-component-configuration)
               #,@(filter-map (make-pred 'ext-component)
                              #'((field (new-field-type new-def) new-doc) ...)
                              #'(target ...)))))))))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (substring str 0 (1- (string-length str)))
                                   str)
                               #\-)
                 "_")))

(define (serialize-field field-name val)
  #~(format #f "~a = ~a;\n" #$(uglify-field-name field-name) #$val))
(define (serialize-field-list field-name val)
  (serialize-field field-name #~(format #f "{\n~@{~a;\n~}}" #$@val)))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val "true" "false")))
(define-maybe boolean)

(define (string-or-boolean? val)
  (or (string? val) (boolean? val)))
(define (serialize-string-or-boolean field-name val)
  (if (string? val)
      (serialize-string field-name val)
      (serialize-boolean field-name val)))

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))
(define (serialize-non-negative-integer field-name val)
  (serialize-field field-name (number->string val)))
(define-maybe non-negative-integer)

(define (non-negative-integer-list? val)
  (and (list? val) (and-map non-negative-integer? val)))
(define (serialize-non-negative-integer-list field-name val)
  (serialize-field-list field-name (map number->string val)))
(define-maybe non-negative-integer-list)

(define (enclose-quotes s)
  #~(string-append "\"" #$s "\""))
(define (serialize-string field-name val)
  (serialize-field field-name (enclose-quotes val)))
(define-maybe string)

(define (string-list? val)
  (and (list? val)
       (and-map (lambda (x)
                  (and (string? x) (not (string-index x #\,))))
                val)))
(define (serialize-string-list field-name val)
  (serialize-field-list field-name (map enclose-quotes val)))
(define-maybe string-list)

(define (module-list? val)
  (string-list? val))
(define (serialize-module-list field-name val)
  (serialize-string-list field-name val))
(define-maybe module-list)

(define (file-name? val)
  (and (string? val)
       (string-prefix? "/" val)))
(define (serialize-file-name field-name val)
  (serialize-string field-name val))
(define-maybe file-name)

(define (file-name-list? val)
  (and (list? val) (and-map file-name? val)))
(define (serialize-file-name-list field-name val)
  (serialize-string-list field-name val))
(define-maybe file-name-list)

(define (file-object? val)
  (or (file-like? val) (file-name? val)))
(define (serialize-file-object field-name val)
  (serialize-string field-name val))
(define-maybe file-object)

(define (file-object-list? val)
  (and (list? val) (and-map file-object? val)))
(define (serialize-file-object-list field-name val)
  (serialize-string-list field-name val))
(define-maybe file-object-list)

(define (raw-content? val)
  (maybe-value-set? val))
(define (serialize-raw-content field-name val)
  val)
(define-maybe raw-content)

(define-configuration mod-muc-configuration
  (name
   (string "Prosody Chatrooms")
   "The name to return in service discovery responses.")

  (restrict-room-creation
   (string-or-boolean #f)
   "If @samp{#t}, this will only allow admins to create new chatrooms.
Otherwise anyone can create a room.  The value @samp{\"local\"} restricts room
creation to users on the service's parent domain.  E.g. @samp{user@@example.com}
can create rooms on @samp{rooms.example.com}.  The value @samp{\"admin\"}
restricts to service administrators only.")

  (max-history-messages
   (non-negative-integer 20)
   "Maximum number of history messages that will be sent to the member that has
just joined the room."))
(define (serialize-mod-muc-configuration field-name val)
  (serialize-configuration val mod-muc-configuration-fields))
(define-maybe mod-muc-configuration)

(define-configuration ssl-configuration
  (protocol
   maybe-string
   "This determines what handshake to use.")

  (key
   maybe-file-name
   "Path to your private key file.")

  (certificate
   maybe-file-name
   "Path to your certificate file.")

  (capath
   (file-object "/etc/ssl/certs")
   "Path to directory containing root certificates that you wish Prosody to
trust when verifying the certificates of remote servers.")

  (cafile
   maybe-file-object
   "Path to a file containing root certificates that you wish Prosody to trust.
Similar to @code{capath} but with all certificates concatenated together.")

  (verify
   maybe-string-list
   "A list of verification options (these mostly map to OpenSSL's
@code{set_verify()} flags).")

  (options
   maybe-string-list
   "A list of general options relating to SSL/TLS.  These map to OpenSSL's
@code{set_options()}.  For a full list of options available in LuaSec, see the
LuaSec source.")

  (depth
   maybe-non-negative-integer
   "How long a chain of certificate authorities to check when looking for a
trusted root certificate.")

  (ciphers
   maybe-string
   "An OpenSSL cipher string.  This selects what ciphers Prosody will offer to
clients, and in what order.")

  (dhparam
   maybe-file-name
   "A path to a file containing parameters for Diffie-Hellman key exchange.  You
can create such a file with:
@code{openssl dhparam -out /etc/prosody/certs/dh-2048.pem 2048}")

  (curve
   maybe-string
   "Curve for Elliptic curve Diffie-Hellman. Prosody's default is
@samp{\"secp384r1\"}.")

  (verifyext
   maybe-string-list
   "A list of \"extra\" verification options.")

  (password
   maybe-string
   "Password for encrypted private keys."))
(define (serialize-ssl-configuration field-name val)
  #~(format #f "ssl = {\n~a};\n"
            #$(serialize-configuration val ssl-configuration-fields)))
(define-maybe ssl-configuration)

(define %default-modules-enabled
  '("roster"
    "saslauth"
    "tls"
    "dialback"
    "disco"
    "carbons"
    "private"
    "blocklist"
    "vcard"
    "version"
    "uptime"
    "time"
    "ping"
    "pep"
    "register"
    "admin_adhoc"))

;; Guile bug.  Use begin wrapper, because otherwise virtualhost-configuration
;; is assumed to be a function.  See
;; https://www.gnu.org/software/guile/manual/html_node/R6RS-Incompatibilities.html
(begin
  (define (virtualhost-configuration-list? val)
    (and (list? val) (and-map virtualhost-configuration? val)))
  (define (serialize-virtualhost-configuration-list l)
    #~(string-append
       #$@(map (lambda (val)
                 (serialize-virtualhost-configuration val)) l)))

  (define (int-component-configuration-list? val)
    (and (list? val) (and-map int-component-configuration? val)))
  (define (serialize-int-component-configuration-list l)
    #~(string-append
       #$@(map (lambda (val)
                 (serialize-int-component-configuration val)) l)))

  (define (ext-component-configuration-list? val)
    (and (list? val) (and-map ext-component-configuration? val)))
  (define (serialize-ext-component-configuration-list l)
    #~(string-append
       #$@(map (lambda (val)
                 (serialize-ext-component-configuration val)) l)))

  (define-all-configurations prosody-configuration
    (prosody
     (file-like prosody)
     "The Prosody package."
     global)

    (data-path
     (file-name "/var/lib/prosody")
     "Location of the Prosody data storage directory.  See
@url{https://prosody.im/doc/configure}."
     global)

    (plugin-paths
     (file-object-list '())
     "Additional plugin directories.  They are searched in all the specified
paths in order.  See @url{https://prosody.im/doc/plugins_directory}."
     global)

    (certificates
     (file-name "/etc/prosody/certs")
     "Every virtual host and component needs a certificate so that clients and
servers can securely verify its identity.  Prosody will automatically load
certificates/keys from the directory specified here."
     global)

    (admins
     (string-list '())
     "This is a list of accounts that are admins for the server.  Note that you
must create the accounts separately.  See @url{https://prosody.im/doc/admins} and
@url{https://prosody.im/doc/creating_accounts}.
Example: @code{(admins '(\"user1@@example.com\" \"user2@@example.net\"))}"
     common)

    (use-libevent?
     (boolean #f)
     "Enable use of libevent for better performance under high load.  See
@url{https://prosody.im/doc/libevent}."
     common)

    (modules-enabled
     (module-list %default-modules-enabled)
     "This is the list of modules Prosody will load on startup.  It looks for
@code{mod_modulename.lua} in the plugins folder, so make sure that exists too.
Documentation on modules can be found at:
@url{https://prosody.im/doc/modules}."
     common)

    (modules-disabled
     (string-list '())
     "@samp{\"offline\"}, @samp{\"c2s\"} and @samp{\"s2s\"} are auto-loaded, but
should you want to disable them then add them to this list."
     common)

    (groups-file
     (file-object "/var/lib/prosody/sharedgroups.txt")
     "Path to a text file where the shared groups are defined.  If this path is
empty then @samp{mod_groups} does nothing.  See
@url{https://prosody.im/doc/modules/mod_groups}."
     common)

    (allow-registration?
     (boolean #f)
     "Disable account creation by default, for security.  See
@url{https://prosody.im/doc/creating_accounts}."
     common)

    (ssl
     (maybe-ssl-configuration (ssl-configuration))
     "These are the SSL/TLS-related settings.  Most of them are disabled so to
use Prosody's defaults.  If you do not completely understand these options, do
not add them to your config, it is easy to lower the security of your server
using them.  See @url{https://prosody.im/doc/advanced_ssl_config}."
     common)

    (c2s-require-encryption?
     (boolean #f)
     "Whether to force all client-to-server connections to be encrypted or not.
See @url{https://prosody.im/doc/modules/mod_tls}."
     common)

    (disable-sasl-mechanisms
     (string-list '("DIGEST-MD5"))
     "Set of mechanisms that will never be offered.  See
@url{https://prosody.im/doc/modules/mod_saslauth}."
     common)

    (insecure-sasl-mechanisms
     (string-list '("PLAIN" "LOGIN"))
     "Set of mechanisms that will not be offered on unencrypted connections.
See @url{https://prosody.im/doc/modules/mod_saslauth}."
     common)

    (s2s-require-encryption?
     (boolean #f)
     "Whether to force all server-to-server connections to be encrypted or not.
See @url{https://prosody.im/doc/modules/mod_tls}."
     common)

    (s2s-secure-auth?
     (boolean #f)
     "Whether to require encryption and certificate authentication.  This
provides ideal security, but requires servers you communicate with to support
encryption AND present valid, trusted certificates.  See
@url{https://prosody.im/doc/s2s#security}."
     common)

    (s2s-insecure-domains
     (string-list '())
     "Many servers don't support encryption or have invalid or self-signed
certificates.  You can list domains here that will not be required to
authenticate using certificates.  They will be authenticated using DNS.  See
@url{https://prosody.im/doc/s2s#security}."
     common)

    (s2s-secure-domains
     (string-list '())
     "Even if you leave @code{s2s-secure-auth?} disabled, you can still require
valid certificates for some domains by specifying a list here.  See
@url{https://prosody.im/doc/s2s#security}."
     common)

    (authentication
     (string "internal_plain")
     "Select the authentication backend to use.  The default provider stores
passwords in plaintext and uses Prosody's configured data storage to store the
authentication data.  If you do not trust your server please see
@url{https://prosody.im/doc/modules/mod_auth_internal_hashed} for information
about using the hashed backend.  See also
@url{https://prosody.im/doc/authentication}"
     common)

    ;; TODO: Handle more complicated log structures.
    (log
     (maybe-string "*syslog")
     "Set logging options.  Advanced logging configuration is not yet supported
by the Prosody service.  See @url{https://prosody.im/doc/logging}."
     common)

    (pidfile
     (file-name "/var/run/prosody/prosody.pid")
     "File to write pid in.  See @url{https://prosody.im/doc/modules/mod_posix}."
     global)

    (http-max-content-size
     (maybe-non-negative-integer %unset-value)
     "Maximum allowed size of the HTTP body (in bytes)."
     common)

    (http-external-url
     (maybe-string %unset-value)
     "Some modules expose their own URL in various ways.  This URL is built
from the protocol, host and port used.  If Prosody sits behind a proxy, the
public URL will be @code{http-external-url} instead.  See
@url{https://prosody.im/doc/http#external_url}."
     common)

    (virtualhosts
     (virtualhost-configuration-list
      (list (virtualhost-configuration
             (domain "localhost"))))
     "A host in Prosody is a domain on which user accounts can be created.  For
example if you want your users to have addresses like
@samp{\"john.smith@@example.com\"} then you need to add a host
@samp{\"example.com\"}.  All options in this list will apply only to this host.

Note: the name \"virtual\" host is used in configuration to avoid confusion with
the actual physical host that Prosody is installed on.  A single Prosody
instance can serve many domains, each one defined as a VirtualHost entry in
Prosody's configuration.  Conversely a server that hosts a single domain would
have just one VirtualHost entry.

See @url{https://prosody.im/doc/configure#virtual_host_settings}."
     global)

    (int-components
     (int-component-configuration-list '())
     "Components are extra services on a server which are available to clients,
usually on a subdomain of the main server (such as
@samp{\"mycomponent.example.com\"}).  Example components might be chatroom
servers, user directories, or gateways to other protocols.

Internal components are implemented with Prosody-specific plugins.  To add an
internal component, you simply fill the hostname field, and the plugin you wish
to use for the component.

See @url{https://prosody.im/doc/components}."
     global)

    (ext-components
     (ext-component-configuration-list '())
     "External components use XEP-0114, which most standalone components
support.  To add an external component, you simply fill the hostname field.  See
@url{https://prosody.im/doc/components}."
     global)

    (component-secret
     (string (configuration-missing-field 'ext-component 'component-secret))
     "Password which the component will use to log in."
     ext-component)

    (component-ports
     (non-negative-integer-list '(5347))
     "Port(s) Prosody listens on for component connections."
     global)

    (component-interface
     (string "127.0.0.1")
     "Interface Prosody listens on for component connections."
     global)

    (domain
     (string (configuration-missing-field 'virtualhost 'domain))
     "Domain you wish Prosody to serve."
     virtualhost)

    (hostname
     (string (configuration-missing-field 'int-component 'hostname))
     "Hostname of the component."
     int-component)

    (plugin
     (string (configuration-missing-field 'int-component 'plugin))
     "Plugin you wish to use for the component."
     int-component)

    (mod-muc
     (maybe-mod-muc-configuration %unset-value)
     "Multi-user chat (MUC) is Prosody's module for allowing you to create
hosted chatrooms/conferences for XMPP users.

General information on setting up and using multi-user chatrooms can be found
in the \"Chatrooms\" documentation (@url{https://prosody.im/doc/chatrooms}),
which you should read if you are new to XMPP chatrooms.

See also @url{https://prosody.im/doc/modules/mod_muc}."
     int-component)

    (hostname
     (string (configuration-missing-field 'ext-component 'hostname))
     "Hostname of the component."
     ext-component)

    (raw-content
     (maybe-raw-content %unset-value)
     "Raw content that will be added to the configuration file."
     common)))

;; Serialize Virtualhost line first.
(define (serialize-virtualhost-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(domain))))
  (let ((domain (virtualhost-configuration-domain config))
        (rest (filter rest? virtualhost-configuration-fields)))
    #~(string-append
       #$(format #f "VirtualHost \"~a\"\n" domain)
       #$(serialize-configuration config rest))))

;; Serialize Component line first.
(define (serialize-int-component-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(hostname plugin))))
  (let ((hostname (int-component-configuration-hostname config))
        (plugin (int-component-configuration-plugin config))
        (rest (filter rest? int-component-configuration-fields)))
    #~(string-append
       #$(format #f "Component \"~a\" \"~a\"\n" hostname plugin)
       #$(serialize-configuration config rest))))

;; Serialize Component line first.
(define (serialize-ext-component-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(hostname))))
  (let ((hostname (ext-component-configuration-hostname config))
        (rest (filter rest? ext-component-configuration-fields)))
    #~(string-append
       #$(format #f "Component \"~a\"\n" hostname)
       #$(serialize-configuration config rest))))

;; Serialize virtualhosts and components last.
(define (serialize-prosody-configuration config)
  (define (rest? field)
    (not (memq (configuration-field-name field)
               '(virtualhosts int-components ext-components))))
  #~(string-append
     #$(let ((rest (filter rest? prosody-configuration-fields)))
         (serialize-configuration config rest))
     #$(serialize-virtualhost-configuration-list
        (prosody-configuration-virtualhosts config))
     #$(serialize-int-component-configuration-list
        (prosody-configuration-int-components config))
     #$(serialize-ext-component-configuration-list
        (prosody-configuration-ext-components config))))

(define-configuration opaque-prosody-configuration
  (prosody
   (file-like prosody)
   "The prosody package.")

  (prosody.cfg.lua
   (string (configuration-missing-field 'opaque-prosody-configuration
                                        'prosody.cfg.lua))
   "The contents of the @code{prosody.cfg.lua} to use."))

(define (prosody-shepherd-service config)
  "Return a <shepherd-service> for Prosody with CONFIG."
  (let* ((prosody (if (opaque-prosody-configuration? config)
                      (opaque-prosody-configuration-prosody config)
                      (prosody-configuration-prosody config)))
         (prosodyctl-bin (file-append prosody "/bin/prosodyctl"))
         (pid-file (prosody-configuration-pidfile config))
         (prosodyctl-action (lambda args
                              #~(lambda _
                                  (invoke #$prosodyctl-bin #$@args)
                                  (match '#$args
                                    (("start")
                                     (call-with-input-file #$pid-file read))
                                    (_ #t))))))
    (list (shepherd-service
           (documentation "Run the Prosody XMPP server")
           (provision '(prosody xmpp-daemon))
           (requirement '(networking syslogd user-processes))
           (modules `((ice-9 match)
                      ,@%default-modules))
           (start (prosodyctl-action "start"))
           (stop (prosodyctl-action "stop"))))))

(define %prosody-accounts
  (list (user-group (name "prosody") (system? #t))
        (user-account
         (name "prosody")
         (group "prosody")
         (system? #t)
         (comment "Prosody daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (prosody-activation config)
  "Return the activation gexp for CONFIG."
  (let* ((config-dir "/etc/prosody")
         (default-certs-dir "/etc/prosody/certs")
         (data-path (prosody-configuration-data-path config))
         (pidfile-dir (dirname (prosody-configuration-pidfile config)))
         (config-str (if (opaque-prosody-configuration? config)
                         (opaque-prosody-configuration-prosody.cfg.lua config)
                         #~(begin
                             (use-modules (ice-9 format))
                             #$(serialize-prosody-configuration config))))
         (config-file (mixed-text-file "prosody.cfg.lua" config-str)))
    #~(begin
        (use-modules (guix build utils))
        (define %user (getpw "prosody"))

        (mkdir-p #$config-dir)
        (chown #$config-dir (passwd:uid %user) (passwd:gid %user))
        (copy-file #$config-file (string-append #$config-dir
                                                "/prosody.cfg.lua"))

        (mkdir-p #$default-certs-dir)
        (chown #$default-certs-dir (passwd:uid %user) (passwd:gid %user))
        (chmod #$default-certs-dir #o750)

        (mkdir-p #$data-path)
        (chown #$data-path (passwd:uid %user) (passwd:gid %user))
        (chmod #$data-path #o750)

        (mkdir-p #$pidfile-dir)
        (chown #$pidfile-dir (passwd:uid %user) (passwd:gid %user)))))

(define prosody-service-type
  (service-type (name 'prosody)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          prosody-shepherd-service)
                       (service-extension account-service-type
                                          (const %prosody-accounts))
                       (service-extension activation-service-type
                                          prosody-activation)))
                (default-value (prosody-configuration
                                (virtualhosts
                                 (list
                                  (virtualhost-configuration
                                   (domain "localhost"))))))
                (description
                 "Run Prosody, a modern XMPP communication server.")))

;; A little helper to make it easier to document all those fields.
(define (generate-documentation)
  (define documentation
    `((prosody-configuration
       ,prosody-configuration-fields
       (ssl ssl-configuration)
       (virtualhosts virtualhost-configuration)
       (int-components int-component-configuration)
       (ext-components ext-component-configuration))
      (ssl-configuration ,ssl-configuration-fields)
      (int-component-configuration ,int-component-configuration-fields
                                   (mod-muc mod-muc-configuration))
      (ext-component-configuration ,ext-component-configuration-fields)
      (mod-muc-configuration ,mod-muc-configuration-fields)
      (virtualhost-configuration ,virtualhost-configuration-fields)
      (opaque-prosody-configuration ,opaque-prosody-configuration-fields)))
  (define (generate configuration-name)
    (match (assq-ref documentation configuration-name)
      ((fields . sub-documentation)
       (format #t "\nAvailable @code{~a} fields are:\n\n" configuration-name)
       (when (memq configuration-name
                   '(virtualhost-configuration
                     int-component-configuration
                     ext-component-configuration))
         (format #t "all these @code{prosody-configuration} fields: ~a, plus:\n"
                 (string-join (map (lambda (s)
                                     (format #f "@code{~a}" s)) common-fields)
                              ", ")))
       (for-each
        (lambda (f)
          (let ((field-name (configuration-field-name f))
                (field-type (configuration-field-type f))
                (field-docs (string-trim-both
                             (configuration-field-documentation f)))
                (default (catch #t
                           (configuration-field-default-value-thunk f)
                           (lambda _ 'nope))))
            (define (escape-chars str chars escape)
              (with-output-to-string
                (lambda ()
                  (string-for-each (lambda (c)
                                     (when (char-set-contains? chars c)
                                       (display escape))
                                     (display c))
                                   str))))
            (define (show-default? val)
              (or (string? val) (number? val) (boolean? val)
                  (and (list? val) (and-map show-default? val))))
            (format #t "@deftypevr {@code{~a} parameter} ~a ~a\n~a\n"
                    configuration-name field-type field-name field-docs)
            (when (show-default? default)
              (format #t "Defaults to @samp{~a}.\n"
                      (escape-chars (format #f "~s" default)
                                    (char-set #\@ #\{ #\})
                                    #\@)))
            (for-each generate (or (assq-ref sub-documentation field-name) '()))
            (format #t "@end deftypevr\n\n")))
        (filter (lambda (f)
                  (not (string=? "" (configuration-field-documentation f))))
                fields)))))
  (generate 'prosody-configuration)
  (format #t "It could be that you just want to get a @code{prosody.cfg.lua}
up and running.  In that case, you can pass an
@code{opaque-prosody-configuration} record as the value of
@code{prosody-service-type}.  As its name indicates, an opaque configuration
does not have easy reflective capabilities.")
  (generate 'opaque-prosody-configuration)
  (format #t "For example, if your @code{prosody.cfg.lua} is just the empty
string, you could instantiate a prosody service like this:

@example
(service prosody-service-type
         (opaque-prosody-configuration
          (prosody.cfg.lua \"\")))
@end example"))


;;;
;;; BitlBee.
;;;

(define-record-type* <bitlbee-configuration>
  bitlbee-configuration make-bitlbee-configuration
  bitlbee-configuration?
  (bitlbee bitlbee-configuration-bitlbee
           (default bitlbee))
  (interface bitlbee-configuration-interface
             (default "127.0.0.1"))
  (port bitlbee-configuration-port
        (default 6667))
  (plugins bitlbee-plugins
           (default '()))
  (extra-settings bitlbee-configuration-extra-settings
                  (default "")))

(define bitlbee-shepherd-service
  (match-lambda
    (($ <bitlbee-configuration> bitlbee interface port
                                plugins extra-settings)
     (let* ((plugins (directory-union "bitlbee-plugins" plugins))
            (conf    (mixed-text-file "bitlbee.conf"
                                  "
  [settings]
  User = bitlbee
  ConfigDir = /var/lib/bitlbee
  DaemonInterface = " interface "
  DaemonPort = " (number->string port) "
  PluginDir = " plugins "/lib/bitlbee
" extra-settings))
            (bitlbee* (least-authority-wrapper
                       (file-append bitlbee "/sbin/bitlbee")
                       #:name "bitlbee"
                       #:preserved-environment-variables
                       '("PURPLE_PLUGIN_PATH" "GUIX_LOCPATH" "LC_ALL")
                       #:mappings (list (file-system-mapping
                                         (source "/var/lib/bitlbee")
                                         (target source)
                                         (writable? #t))
                                        (file-system-mapping
                                         (source "/run/current-system/locale")
                                         (target source))
                                        (file-system-mapping
                                         (source conf)
                                         (target conf)))
                       #:namespaces (delq 'net %namespaces))))

       (list (shepherd-service
              (provision '(bitlbee))

              ;; Note: If networking is not up, then /etc/resolv.conf
              ;; doesn't get mapped in the container, hence the dependency
              ;; on 'networking'.
              (requirement '(user-processes networking))

              (start #~(make-inetd-constructor
                        (list #$bitlbee* "-I" "-c" #$conf)
                        (list (endpoint
                               (addrinfo:addr
                                (car (getaddrinfo #$interface
                                                  #$(number->string port)
                                                  (logior AI_NUMERICHOST
                                                          AI_NUMERICSERV))))))
                        #:requirements '#$requirement
                        #:service-name-stem "bitlbee"
                        #:user "bitlbee" #:group "bitlbee"

                        ;; Allow 'bitlbee-purple' to use libpurple plugins.
                        #:environment-variables
                        (list (string-append "PURPLE_PLUGIN_PATH="
                                             #$plugins "/lib/purple-2")
                              "GUIX_LOCPATH=/run/current-system/locale")))
              (stop  #~(make-inetd-destructor))))))))

(define %bitlbee-accounts
  ;; User group and account to run BitlBee.
  (list (user-group (name "bitlbee") (system? #t))
        (user-account
         (name "bitlbee")
         (group "bitlbee")
         (system? #t)
         (comment "BitlBee daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %bitlbee-activation
  ;; Activation gexp for BitlBee.
  #~(begin
      (use-modules (guix build utils))

      ;; This directory is used to store OTR data.
      (mkdir-p "/var/lib/bitlbee")
      (let ((user (getpwnam "bitlbee")))
        (chown "/var/lib/bitlbee"
               (passwd:uid user) (passwd:gid user)))))

(define bitlbee-service-type
  (service-type (name 'bitlbee)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          bitlbee-shepherd-service)
                       (service-extension account-service-type
                                          (const %bitlbee-accounts))
                       (service-extension activation-service-type
                                          (const %bitlbee-activation))))
                (default-value (bitlbee-configuration))
                (description
                 "Run @url{http://bitlbee.org,BitlBee}, a daemon that acts as
a gateway between IRC and chat networks.")))


;;;
;;; ngIRCd.
;;;

(define-maybe string
  (prefix ngircd-))

(define-maybe file-like
  (prefix ngircd-))

(define-maybe list-of-strings
  (prefix ngircd-))

(define (port? x)
  (and (number? x)
       (and (>= x 0) (<= x 65535))))

(define list-of-ports?
  (list-of port?))

(define-maybe port
  (prefix ngircd-))

(define-maybe list-of-ports
  (prefix ngircd-))

(define-maybe number
  (prefix ngircd-))

(define-maybe boolean
  (prefix ngircd-))

(define (pascal-case text)
  (object->camel-case-string text 'upper))

(define (ngircd-serialize-string field value)
  (format #f "~a = ~a~%" (pascal-case field) value))

(define (ngircd-serialize-boolean field value)
  (let* ((field (symbol->string field))
         (name (if (string-suffix? "?" field)
                   (string-drop-right field 1)
                   field)))
    (format #f "~a = ~:[false~;true~]~%" (pascal-case name) value)))

(define (ngircd-serialize-file-like field value)
  #~(format #f "~a = ~a~%" #$(pascal-case field) #$value))

(define (ngircd-serialize-list-of-strings field value)
  (format #f "~a = ~{~a~^,~}~%" (pascal-case field) value))

(define ngircd-serialize-list-of-ports
  ngircd-serialize-list-of-strings)

(define ngircd-serialize-number ngircd-serialize-string)

(define ngircd-serialize-port ngircd-serialize-number)

(define (string-or-number? x)
  (or (string? x) (number? x)))

(define ngircd-serialize-string-or-number ngircd-serialize-string)

(define-configuration ngircd-global     ;[Global]
  (name
   maybe-string
   "Server name in the IRC network.  This is an individual name of the IRC
server, it is not related to the DNS host name.  It must be unique in the IRC
network and must contain at least one dot (@samp{.}) character.  When not set,
ngIRCd tries to deduce a valid IRC server name from the local host name.")
  (admin-info-1
   maybe-string
   "First administrator information.")
  (admin-info-2
   maybe-string
   "Second administrator information.")
  (admin-email
   maybe-string
   "Email to reach administrators.")
  (help-file
   maybe-file-like
   "File-like containing the ngIRCd help text.")
  (info
   maybe-string
   "Info text of the server. This will be shown by WHOIS and LINKS requests
for example.")
  (listen
   (list-of-strings (list "::" "0.0.0.0"))
   "A list of IP address on which the server should listen.  By default it
listens on all configured IP addresses and interfaces.")
  (motd-file
   ;; Provide an empty default file to avoid a warning when running
   ;; --configtest to validate the configuration file.
   (file-like (plain-file "ngircd.motd" ""))
   "Text file with the @i{message of the day} (MOTD).  This message will be
shown to all users connecting to the server.")
  (motd-phrase
   maybe-string
   "A simple phrase (<127 chars) to use if you don't want to use a MOTD
file.")
  (network
   maybe-string
   "The name of the IRC network to which this server belongs.  This name is
optional, should only contain ASCII characters, and can't contain spaces.  It
is only used to inform clients.")
  (password
   maybe-string
   "Global password or all users needed to connect to the server.  By default,
no password is required.  PAM must be disabled for this option to have an
effect.")
  (pid-file
   maybe-string
   "The file name where the PID of ngIRCd should be written after it starts.
By default, no PID file is created.")
  (ports
   (list-of-ports (list 6667))
   "Port number(s) on which the server should listen for @emph{unencrypted}
connections.")
  (server-uid
   (string-or-number "ngircd")
   "The user that the @command{ngircd} command should run as.")
  (server-gid
   (string-or-number "ngircd")
   "The group that the @command{ngircd} command should run as.")
  (prefix ngircd-))

(define (serialize-ngircd-global _ config)
  #~(string-append
     "[Global]\n"
     #$(serialize-configuration config ngircd-global-fields)))

(define-configuration ngircd-limits     ;[Limits]
  (connect-retry
   (maybe-number 60)
   "The number of seconds the server should wait before re-attempting to
establish a link to not yet (or no longer) connected servers.")
  (max-connections
   (maybe-number 0)
   "Maximum number of simultaneous in- and outbound connections the server is
allowed to accept.  There is no limit by default.")
  (max-connections-ip
   (maybe-number 5)
   "Maximum number of simultaneous connections from a single IP address that
the server will accept.  This configuration options lowers the risk of denial
of service attacks (DoS).  Set to 0 to remove the limit.")
  (max-joins
   (maybe-number 10)
   "Maximum number of channels a user can be member of.  Set to 0 to remove
the limit.")
  (max-list-size
   (maybe-number 100)
   "Maximum number of channels returned in response to a LIST command.")
  (ping-timeout
   (maybe-number 120)
   "Number of seconds of inactivity after which the server will send a PING to
the peer to test whether it is alive or not.")
  (pong-timeout
   (maybe-number 20)
   "If a client fails to answer a PING with a PONG within this amount of
seconds, it will be disconnected by the server.")
  (prefix ngircd-))

(define (serialize-ngircd-limits _ config)
  #~(string-append
     "\n[Limits]\n"
     #$(serialize-configuration config ngircd-limits-fields)))

(define-maybe ngircd-limits)

(define-configuration ngircd-options    ;[Options]
  (allowed-channel-types
   (maybe-string "#&+")
   "List of allowed channel types (channel prefixes) for newly created
channels on the local server.  By default, all supported channel types are
allowed.")
  (allow-remote-oper?
   (maybe-boolean #f)
   "If this option is active, IRC operators connected to remote servers are
allowed to control this local server using administrative commands, for
example like CONNECT, DIE, SQUIT, etc.")
  (connect-ipv4?
   (maybe-boolean #t)
   "Set to @code{#f} to prevent ngIRCd from connecting to other IRC servers
using the IPv4 protocol, allowed by default.")
  (connect-ipv6?
   (maybe-boolean #t)
   "Set to @code{#f} to prevent ngIRCd from connecting to other IRC servers
using the IPv6 protocol, allowed by default.")
  (dns?
   (maybe-boolean #t)
   "Set to @code{#f} to disable DNS lookups when clients connect.  If you
configure the daemon to connect to other servers, ngIRCd may still perform a
DNS lookup if required.")
  (more-privacy?
   (maybe-boolean #f)
   "Set this to @code{#t} to have ngIRCd censor user idle time, logon time as
well as the PART/QUIT messages (that sometimes used to inform everyone about
which client software is being used).  WHOWAS requests are also silently
ignored, and NAMES output doesn't list any clients for non-members.  This
option is most useful when ngIRCd is being used together with anonymizing
software such as TOR or I2P and one does not wish to make it too easy to
collect statistics on the users.")
  (notice-before-registration?
   (maybe-boolean #f)
   "Normally ngIRCd doesn't send any messages to a client until it is
registered.  Enable this option to let the daemon send @samp{NOTICE *}
messages to clients while connecting.")
  (oper-can-use-mode?
   (maybe-boolean #f)
   "Should IRC Operators be allowed to use the MODE command even if they are
not(!) channel-operators?")
  (oper-chan-p-auto-op?
   (maybe-boolean #t)
   "Should IRC Operators get AutoOp (+o) in persistent (+P) channels?")
  (oper-server-mode?
   (maybe-boolean #f)
   "If @code{open-can-use-mode?} is @code{#t}, this may lead the compatibility
problems with servers that run the ircd-irc2 software.  This option masks mode
requests by non-chanops as if they were coming from the server.  Only enable
this if you have ircd-irc2 servers in your IRC network.")
  (pam?
   (boolean #f)
   "Set to @code{#t} to enable calls to the PAM library at runtime; all users
connecting without password are allowed to connect, all passwords given will
fail.  Users identified without PAM are registered with a tilde (@samp{~})
prepended to their user name.  This defaults to @code{#f} in Guix because the
service runs as an unprivileged user and thus cannot authenticate other users
via the @code{pam_unix} PAM module.")
  (pam-is-optional?
   (maybe-boolean #f)
   "Set to @code{#t} to make PAM authentication optional, causing clients not
sending a password to still be able to connect, but won't become identified
and keep the tilder (@samp{~}) character prepended to their supplied user
name.")
  (require-auth-ping?
   (maybe-boolean #f)
   "Set to @code{#t} to have ngIRCd send an authentication PING when a new
client connects, and register this client only after receiving the
corresponding PONG reply.")
  (prefix ngircd-))

(define (serialize-ngircd-options _ config)
  #~(string-append
     "\n[Options]\n"
     #$(serialize-configuration config ngircd-options-fields)))

(define-maybe ngircd-options)

(define-configuration ngircd-ssl        ;[SSL]
  (cert-file
   maybe-string
   "SSL certificate file of the private server key.")
  (key-file
   maybe-string
   "File name of the SSL Server Key to be used for SSL connections, which is
required for SSL/TLS support.")
  (ca-file
   (maybe-string "/etc/ssl/certs/ca-certificates.crt")
   "A file listing all the certificates of the trusted Certificate
Authorities.")
  (ports
   maybe-list-of-ports
   "Like the global configuration's @code{port} option, except that ngIRCd
will expect incoming connections to be SSL/TLS encrypted.  Common port numbers
for SSL-encrypted IRC are 6669 and 6697.")
  (cipher-list
   maybe-string
   "The GnuTLS cipher suites allowed for SSL/TLS connections, a value such as
@code{\"SECURE128:-VERS-SSL3.0\"}.  Refer to @samp{man 3 gnutls_priority_init}
for details.")
  (dh-file
   maybe-file-like
   "A file-like containing the Diffie-Hellman parameters, which can be created
with GnuTLS via @samp{certtool --generate-dh-params}.  If this file is not
present, the Diffie-Hellman parameters will be computed on startup, which may
take some time.")
  (prefix ngircd-))

(define (serialize-ngircd-ssl _ config)
  #~(string-append
     "\n[SSL]\n"
     #$(serialize-configuration config ngircd-ssl-fields)))

(define-maybe ngircd-ssl)

(define-configuration ngircd-operator   ;[Operator]
  (name
   string
   "ID of the operator (may be different of the nickname).")
  (password
   string
   "Password of the IRC operator.")
  (mask
   maybe-string
   "Mask that is to be checked before an /OPER for this account is accepted,
for example: @code{\"nick!ident@@*.example.com\"}.")
  (prefix ngircd-))

(define list-of-ngircd-operators?
  (list-of ngircd-operator?))

(define (serialize-ngircd-operator _ operator)
  #~(string-append
     "\n[Operator]\n"
     #$(serialize-configuration operator ngircd-operator-fields)))

(define (serialize-list-of-ngircd-operators _ operators)
  #~(string-append #$@(map (cut serialize-ngircd-operator #f <>) operators)))

(define-maybe list-of-ngircd-operators)

(define-configuration ngircd-server     ;[Server]
  (name
   string
   "IRC name of the remote server.")
  (host
   string
   "Internet host name (or IP address) of the peer.")
  (my-password
   string
   "Own password for this connection.  This password has to be configured as
@code{peer-password} on the other server and must not have @samp{:} as first
character.")
  (peer-password
   string
   "Foreign password for this connection.  This password has to be configured
as @code{my-password} on the other server.")
  (bind
   maybe-string
   "IP address to use as source IP for the outgoing connection.  The default
is to let the operating system decide.")
  (port
   maybe-port
   "Port of the remote server to which ngIRCd should connect (active).  If no
port is assigned to a configured server, the daemon only waits for incoming
connections (passive, which is the default).")
  (group
   maybe-number
   "Group of this server.")
  (passive?
   (maybe-boolean #f)
   "Set to @code{#t} to disable automatic connection even if the port value is
specified.")
  (ssl-connect?
   (maybe-boolean #f)
   "Set to @code{#t} to enforce the use of TLS to connect to the remote
server.")
  (ssl-verify?
   (maybe-boolean #t)
   "Set to @code{#f} to disable the verification of the TLS certificate
presented by the remote server.  This can be useful if the remote server uses
a self-signed certificate.  Beware that this degrades security by enabling
man-in-the-middle attacks, as the @emph{remote site is not verified at all}.")
  (prefix ngircd-))

(define list-of-ngircd-servers?
  (list-of ngircd-server?))

(define (serialize-ngircd-server _ server)
  #~(string-append
     "\n[Server]\n"
     #$(serialize-configuration server ngircd-server-fields)))

(define (serialize-list-of-ngircd-servers _ servers)
  #~(string-append #$@(map (cut serialize-ngircd-server #f <>) servers)))

(define-maybe list-of-ngircd-servers)

(define-configuration ngircd-channel    ;[Channel]
  (name
   string
   "Name of the channel, including channel prefix (\"#\" or \"&\").")
  (topic
   maybe-string
   "Topic for this channel.")
  (modes
   maybe-list-of-strings
   "Initial channel modes, as used in MODE commands.  Modifying lists (ban
list, invite list, exception list) is supported.  If multiple MODE strings are
specified, they are evaluated in the order listed (left to right)."
   (serializer (lambda (_ value)
                 ;; Special case: each mode string gets serialized to a
                 ;; separate option.
                 (format #f "~{Modes = ~a~%~}" value))))
  (key-file
   maybe-file-like
   "Path and file name of a ngIRCd key file containing individual channel keys
for different users.  Refer to @samp{man 5 ngircd.conf} for more details.")
  (prefix ngircd-))

(define list-of-ngircd-channels?
  (list-of ngircd-channel?))

(define (serialize-ngircd-channel _ channel)
  #~(string-append
     "\n[Channel]\n"
     #$(serialize-configuration channel ngircd-channel-fields)))

(define (serialize-list-of-ngircd-channels _ channels)
  #~(string-append #$@(map (cut serialize-ngircd-channel #f <>) channels)))

(define-maybe list-of-ngircd-channels)

(define-configuration ngircd-configuration
  (ngircd
   (file-like ngircd)
   "The @code{ngircd} package to use.")
  (debug?
   (boolean #f)
   "Turn on debugging messages."
   (serializer empty-serializer))
  (shepherd-requirement
   (list-of-symbols '(user-processes))
   "Shepherd requirements the service should depend on."
   (serializer empty-serializer))
  (global
   ;; Always use a ngircd-global default to ensure the default addresses
   ;; listened to are known (used to compute the socket endpoints).
   (ngircd-global (ngircd-global))
   "A ngircd-global record object used to specify global options.")
  (limits
   maybe-ngircd-limits
   "The ngircd-limits record object used to specify limits options.")
  ;; Always use a ngircd-options default to ensure PAM is disabled by default.
  (options
   (ngircd-options (ngircd-options))
   "The ngircd-options record object used to specify optional features and
configuration options.")
  (ssl
   maybe-ngircd-ssl
   "The ngircd-ssl record object used to specify the SSL-related options.")
  (operators
   maybe-list-of-ngircd-operators
   "A list of ngircd-operator record objects used to specify the operators.")
  (servers
   maybe-list-of-ngircd-servers
   "A list of ngircd-server record objects used to specify other remote
servers to connect to.")
  (channels
   maybe-list-of-ngircd-channels
   "A list of ngircd-channels record objects specifying pre-defined channels
to be created by the server when starting up."))

(define (ngircd-generate-documentation)
  (configuration->documentation 'ngircd-configuration)
  (configuration->documentation 'ngircd-global)
  (configuration->documentation 'ngircd-limits)
  (configuration->documentation 'ngircd-options)
  (configuration->documentation 'ngircd-ssl)
  (configuration->documentation 'ngircd-operator)
  (configuration->documentation 'ngircd-server)
  (configuration->documentation 'ngircd-channel))

(define (ngircd-user+group config)
  "Return the Global->ServerUID and Global->ServerGID configuration options as
values."
  (let* ((global (ngircd-configuration-global config))
         (user (ngircd-global-server-uid global))
         (group (ngircd-global-server-gid global)))
    (values user group)))

(define (ngircd-account config)
  (let* ((user group (ngircd-user+group config))
         (group-name (if (string? group)
                         group
                         "ngircd"))
         (user-name (if (string? user)
                        user
                        "ngircd"))
         (gid (if (number? group)
                  group
                  #f))
         (uid (if (number? user)
                  user
                  #f)))
    (list (user-group
           (name group-name)
           (id gid)
           (system? #t))
          (user-account
           (name user-name)
           (uid uid)
           (group group-name)
           (system? #t)
           (comment "Ngircd daemon user")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))))))

(define (serialize-ngircd-configuration config)
  "Return a file-like object corresponding to the serialized
<ngircd-configuration> record."
  (let ((ngircd (file-append (ngircd-configuration-ngircd config)
                             "/sbin/ngircd"))
        (ngircd.conf (mixed-text-file "unvalidated-ngircd.conf"
                                      (serialize-configuration
                                       config ngircd-configuration-fields))))
    (computed-file
     "ngircd.conf"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           ;; Ensure stdin is not connected to a TTY source to avoid ngircd
           ;; configtest blocking with a confirmation prompt.
           (parameterize ((current-input-port (%make-void-port "r")))
             (invoke #+ngircd "--config" #$ngircd.conf "--configtest" ))
           (copy-file #$ngircd.conf #$output))))))

(define (ngircd-wrapper config)
  "Take CONFIG, a <ngircd-configuration> object, and provide a least-authority
wrapper for the 'ngircd' command."
  (let* ((ngircd.conf (serialize-ngircd-configuration config))
         (user group (ngircd-user+group config))
         (global (ngircd-configuration-global config))
         (help-file (ngircd-global-help-file global))
         (motd-file (ngircd-global-motd-file global))
         (ssl (ngircd-configuration-ssl config))
         (channels (ngircd-configuration-channels config)))
    (least-authority-wrapper
     (file-append (ngircd-configuration-ngircd config) "/sbin/ngircd")
     #:name "ngircd-pola-wrapper"
     ;; Expose all needed files, such as all options corresponding to
     ;; file-like objects and string file names.
     #:mappings
     (append
      (list (file-system-mapping
             (source "/var/log/ngircd.log")
             (target source)
             (writable? #t))
            (file-system-mapping
             (source ngircd.conf)
             (target source)))
      (if (maybe-value-set? help-file)
          (list (file-system-mapping
                 (source help-file)
                 (target source)))
          '())
      (if (maybe-value-set? motd-file)
          (list (file-system-mapping
                 (source motd-file)
                 (target source)))
          '())
      (if (maybe-value-set? ssl)
          (let ((ca-file (ngircd-ssl-ca-file ssl))
                (cert-file (ngircd-ssl-cert-file ssl))
                (key-file (ngircd-ssl-key-file ssl))
                (dh-file (ngircd-ssl-dh-file ssl)))
            ;; When SSL is used, expose the specified keys and certificates.
            (append
             (if (maybe-value-set? ca-file)
                 (list (file-system-mapping
                        (source ca-file)
                        (target source)))
                 '())
             (if (maybe-value-set? cert-file)
                 (list (file-system-mapping
                        (source cert-file)
                        (target source)))
                 '())
             (if (maybe-value-set? key-file)
                 (list (file-system-mapping
                        (source key-file)
                        (target source)))
                 '())
             (if (maybe-value-set? dh-file)
                 (list (file-system-mapping
                        (source dh-file)
                        (target source)))
                 '())))
          '())
      (if (maybe-value-set? channels)
          (filter-map (lambda (channel)
                        (let ((key-file (ngircd-channel-key-file channel)))
                          (and (maybe-value-set? key-file)
                               key-file)))
                      channels)
          '()))
     #:user user
     #:group group
     ;; ngircd wants to look up users in /etc/passwd so run in the global user
     ;; namespace.
     #:namespaces (fold delq %namespaces '(net user)))))

(define (ngircd-shepherd-service config)
  (match-record config <ngircd-configuration>
                (ngircd debug? global shepherd-requirement ssl)
    (let* ((ngircd.conf (serialize-ngircd-configuration config))
           (ngircd (file-append ngircd "/sbin/ngircd"))
           (addresses (ngircd-global-listen global))
           (ports* (ngircd-global-ports global))
           (ports (if (and (maybe-value-set? ssl)
                           (maybe-value-set? (ngircd-ssl-ports ssl)))
                      (append ports* (ngircd-ssl-ports ssl))
                      ports*)))
      (list (shepherd-service
             (provision '(ngircd))
             (requirement shepherd-requirement)
             (modules (cons '(srfi srfi-1) %default-modules))
             (actions (list (shepherd-configuration-action ngircd.conf)))
             (start #~(make-systemd-constructor
                       (append (list #$(ngircd-wrapper config)
                                     "--nodaemon"
                                     "--config" #$ngircd.conf)
                               (if #$debug?
                                   '("--debug")
                                   '()))
                       ;; Compute endpoints for each listen addresses/ports
                       ;; combinations.
                       (append-map
                        (lambda (port)
                          (map (lambda (addr)
                                 (endpoint
                                  (addrinfo:addr
                                   (car (getaddrinfo
                                         addr
                                         (number->string port)
                                         (logior AI_NUMERICHOST
                                                 AI_NUMERICSERV))))))
                               (list #$@addresses)))
                        (list #$@ports))
                       #:log-file "/var/log/ngircd.log"))
             (stop  #~(make-systemd-destructor)))))))

(define ngircd-service-type
  (service-type
   (name 'ngircd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             ngircd-shepherd-service)
          (service-extension profile-service-type
                             (compose list ngircd-configuration-ngircd))
          (service-extension account-service-type
                             ngircd-account)))
   (default-value (ngircd-configuration))
   (description
    "Run @url{https://ngircd.barton.de/, ngIRCd}, a lightweight @acronym{IRC,
Internet Relay Chat} daemon.")))


;;;
;;; Quassel.
;;;

(define-record-type* <quassel-configuration>
  quassel-configuration make-quassel-configuration
  quassel-configuration?
  (quassel quassel-configuration-quassel
           (default quassel))
  (interface quassel-configuration-interface
             (default "::,0.0.0.0"))
  (port quassel-configuration-port
        (default 4242))
  (loglevel quassel-configuration-loglevel
            (default "Info")))

(define quassel-shepherd-service
  (match-lambda
    (($ <quassel-configuration> quassel interface port loglevel)
     (let ((quassel (least-authority-wrapper
                     (file-append quassel "/bin/quasselcore")
                     #:name "quasselcore"
                     #:mappings (list (file-system-mapping
                                       (source "/var/lib/quassel")
                                       (target source)
                                       (writable? #t))
                                      (file-system-mapping
                                       (source "/var/log/quassel")
                                       (target source)
                                       (writable? #t)))
                     ;; XXX: The daemon needs to live in the main user
                     ;; namespace, as root, so it can access /var/lib/quassel
                     ;; owned by "quasselcore".
                     #:namespaces (fold delq %namespaces '(net user)))))
       (list (shepherd-service
               (provision '(quassel))
               (requirement '(user-processes networking))
               (start #~(make-forkexec-constructor
                         (list #$quassel
                               "--configdir=/var/lib/quassel"
                               "--logfile=/var/log/quassel/core.log"
                               (string-append "--loglevel=" #$loglevel)
                               (string-append "--port=" (number->string #$port))
                               (string-append "--listen=" #$interface))))
               (stop  #~(make-kill-destructor))))))))

(define %quassel-account
  (list (user-group (name "quassel") (system? #t))
        (user-account
          (name "quasselcore")
          (group "quassel")
          (system? #t)
          (comment "Quassel daemon user")
          (home-directory "/var/lib/quassel")
          (shell (file-append shadow "/sbin/nologin")))))

(define %quassel-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/quassel")
      (mkdir-p "/var/log/quassel")
      (let ((cert "/var/lib/quassel/quasselCert.pem"))
        (unless (file-exists? cert)
          (invoke #$(file-append openssl "/bin/openssl")
                  "req" "-x509" "-nodes" "-batch" "-days" "680" "-newkey"
                  "rsa" "-keyout" cert "-out" cert)))))

(define quassel-service-type
  (service-type (name 'quassel)
                (extensions
                  (list (service-extension shepherd-root-service-type
                                           quassel-shepherd-service)
                        (service-extension profile-service-type
                                           (compose list quassel-configuration-quassel))
                        (service-extension account-service-type
                                           (const %quassel-account))
                        (service-extension activation-service-type
                                           (const %quassel-activation))))
                (default-value (quassel-configuration))
                (description
                 "Run @url{https://quassel-irc.org/,quasselcore}, the backend
for the distributed IRC client quassel, which allows you to connect from
multiple machines simultaneously.")))


;;;
;;; Snuik.
;;;
(define-maybe integer (no-serialization))
(define-configuration/no-serialization snuik-configuration
  (snuik         (package snuik)       "The snuik package to use.")
  (server        maybe-string          "The IRC server to connect to.")
  (port          maybe-integer         "The port used by the IRC server.")
  (nick          maybe-string          "The nickname for snuik to use.")
  (password      maybe-string          "The password to use when logging in.")
  (password-file maybe-string          "The file to read the password from.")
  (channels      (list-of-strings '()) "The channels for snuik to join.")
  (extra-options (list-of-strings '()) "Extra options to be passed to snuik.")
  (home-service? (boolean for-home?)   "Running as home service?"))

(define (snuik-services config)
  "Return a <shepherd-service> for snuik with CONFIG."
  (match-record config
      <snuik-configuration>
      (snuik server port nick password password-file channels extra-options
             home-service?)
    (let* ((password-file (snuik-configuration-password-file config))
           (mappings `(,@(if home-service?
                             '()
                             `(,(file-system-mapping
                                 (source "/var/run/snuik")
                                 (target source)
                                 (writable? #t))
                               ,@(if password-file
                                     (list (file-system-mapping
                                            (source password-file)
                                            (target source)))
                                     '())))))
           (snuik (least-authority-wrapper
                   (file-append snuik "/bin/snuik")
                   #:name "snuik"
                   #:mappings mappings
                   #:namespaces (delq 'net %namespaces)))
           (command
            #~'(#$snuik
                #$@(if (and server (not (eq? server %unset-value)))
                       (list (string-append "--server=" server))
                       #~())
                #$@(if (and port (not (eq? port %unset-value)))
                       (list (string-append "--port=" (number->string port)))
                       #~())
                #$@(if (and nick (not (eq? nick %unset-value)))
                       (list (string-append "--nick=" nick))
                       #~())
                #$@(if (and password (not (eq? password %unset-value)))
                       (list (string-append "--password=" password))
                       #~())
                #$@(if (and password-file (not (eq? password-file %unset-value)))
                       (list (string-append "--password-file=" password-file))
                       #~())
                #$@(if (pair? channels)
                       (list (string-append "--channels=" (string-join channels ",")))
                       #~())
                #$@extra-options))
           (log-file #~(string-append
                        #$(if home-service? #~%user-log-dir "/var/log")
                        "/snuik.log")))
      (list (shepherd-service
             (documentation "Run the snuik IRC bot.")
             (provision '(snuik))
             (requirement (if home-service? '() '(user-processes networking)))
             (modules (if home-service?
                          '((shepherd support)) ;for '%user-log-dir'
                          '()))
             (start #~(make-forkexec-constructor
                       #$command
                       #:log-file #$log-file
                       #:user #$(and (not home-service?) "snuik")
                       #:group #$(and (not home-service?) "snuik")))
             (stop #~(make-kill-destructor)))))))

(define snuik-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (let* ((user (getpw "snuik"))
               (directory "/var/run/snuik"))
          (mkdir-p directory)
          (chown directory (passwd:uid user) (passwd:gid user))))))

(define %snuik-accounts
  (list (user-group (name "snuik") (system? #t))
        (user-account
         (name "snuik")
         (group "snuik")
         (system? #t)
         (comment "Snuik IRC bot user")
         (home-directory "/var/run/snuik")
         (shell (file-append shadow "/sbin/nologin")))))

(define snuik-service-type
  (service-type
   (name 'home-snuik)
   (description "Run the Snuik IRC bot.")
   (default-value (snuik-configuration))
   (extensions
    (list (service-extension activation-service-type
                             (const snuik-activation))
          (service-extension account-service-type
                             (const %snuik-accounts))
          (service-extension shepherd-root-service-type
                             snuik-services)))))
