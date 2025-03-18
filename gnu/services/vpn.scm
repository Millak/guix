;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 Domagoj Stolfa <ds815@gmx.com>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2021 Nathan Dehnel <ncdehnel@gmail.com>
;;; Copyright © 2022 Cameron V Chaparro <cameron@cameronchaparro.com>
;;; Copyright © 2022 Timo Wilken <guix@twilken.net>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2024 Richard Sent <richard@freakingpenguin.com>
;;; Copyright © 2025 Carlo Zancanaro <carlo@zancanaro.id.au>
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

(define-module (gnu services vpn)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages vpn)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix deprecation)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (openvpn-client-service  ; deprecated
            openvpn-server-service  ; deprecated
            openvpn-client-service-type
            openvpn-server-service-type
            openvpn-client-configuration
            openvpn-server-configuration
            openvpn-remote-configuration
            openvpn-ccd-configuration
            generate-openvpn-client-documentation
            generate-openvpn-server-documentation

            strongswan-configuration
            strongswan-service-type

            wireguard-peer
            wireguard-peer?
            wireguard-peer-name
            wireguard-peer-endpoint
            wireguard-peer-allowed-ips
            wireguard-peer-public-key
            wireguard-peer-preshared-key
            wireguard-peer-keep-alive

            wireguard-configuration
            wireguard-configuration?
            wireguard-configuration-wireguard
            wireguard-configuration-interface
            wireguard-configuration-addresses
            wireguard-configuration-port
            wireguard-configuration-dns
            wireguard-configuration-monitor-ips?
            wireguard-configuration-monitor-ips-interval
            wireguard-configuration-private-key
            wireguard-configuration-bootstrap-private-key?
            wireguard-configuration-peers
            wireguard-configuration-pre-up
            wireguard-configuration-post-up
            wireguard-configuration-pre-down
            wireguard-configuration-post-down
            wireguard-configuration-shepherd-requirement
            wireguard-configuration-table
            wireguard-configuration-auto-start?

            wireguard-service-type))

;;;
;;; Bitmask.
;;;

(define-public bitmask-service-type
  (service-type
   (name 'bitmask)
   (description "Setup the @uref{https://bitmask.net, Bitmask} VPN application.")
   (default-value bitmask)
   (extensions
    (list
     ;; Add bitmask to the system profile.
     (service-extension profile-service-type list)
     ;; Configure polkit policy of bitmask.
     (service-extension polkit-service-type list)))))

;;;
;;; OpenVPN.
;;;

(define (uglify-field-name name)
  (match name
    ('verbosity "verb")
    (_ (let ((str (symbol->string name)))
         (if (string-suffix? "?" str)
             (substring str 0 (1- (string-length str)))
             str)))))

(define (serialize-field field-name val)
  (if (eq? field-name 'pid-file)
      (format #t "")
      (format #t "~a ~a\n" (uglify-field-name field-name) val)))
(define serialize-string serialize-field)
(define-maybe string)
(define (serialize-boolean field-name val)
  (if val
      (serialize-field field-name "")
      (format #t "")))

(define (ip-mask? val)
  (and (string? val)
       (if (string-match "^([0-9]+\\.){3}[0-9]+ ([0-9]+\\.){3}[0-9]+$" val)
           (let ((numbers (string-tokenize val char-set:digit)))
             (all-lte numbers (list 255 255 255 255 255 255 255 255)))
           #f)))
(define serialize-ip-mask serialize-string)

(define-syntax define-enumerated-field-type
  (lambda (x)
    (define (id-append ctx . parts)
      (datum->syntax ctx (apply symbol-append (map syntax->datum parts))))
    (syntax-case x ()
      ((_ name (option ...))
       #`(begin
           (define (#,(id-append #'name #'name #'?) x)
             (memq x '(option ...)))
           (define (#,(id-append #'name #'serialize- #'name) field-name val)
             (serialize-field field-name val)))))))

(define-enumerated-field-type proto
  (udp tcp udp6 tcp6))
(define-enumerated-field-type dev
  (tun tap))

(define key-usage? boolean?)
(define (serialize-key-usage field-name value)
  (if value
      (format #t "remote-cert-tls server\n")
      #f))

(define bind? boolean?)
(define (serialize-bind field-name value)
  (if value
      #f
      (format #t "nobind\n")))

(define resolv-retry? boolean?)
(define (serialize-resolv-retry field-name value)
  (if value
      (format #t "resolv-retry infinite\n")
      #f))

(define (serialize-tls-auth role location)
  (if location
      (serialize-field 'tls-auth
                       (string-append location " " (match role
                                                     ('server "0")
                                                     ('client "1"))))
      #f))
(define (tls-auth? val)
  (or (eq? val #f)
      (string? val)))
(define (serialize-tls-auth-server field-name val)
  (serialize-tls-auth 'server val))
(define (serialize-tls-auth-client field-name val)
  (serialize-tls-auth 'client val))
(define tls-auth-server? tls-auth?)
(define tls-auth-client? tls-auth?)

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (all-lte left right)
  (if (eq? left '())
      (eq? right '())
      (and (<= (string->number (car left)) (car right))
           (all-lte (cdr left) (cdr right)))))

(define (cidr4? val)
  (if (string? val)
      (if (string-match "^([0-9]+\\.){3}[0-9]+/[0-9]+$" val)
          (let ((numbers (string-tokenize val char-set:digit)))
            (all-lte numbers (list 255 255 255 255 32)))
          #f)
      (eq? val #f)))

(define (cidr6? val)
  (if (string? val)
      (string-match "^([0-9a-f]{0,4}:){0,8}/[0-9]{1,3}$" val)
      (eq? val #f)))

(define (serialize-cidr4 field-name val)
  (if (eq? val #f) #f (serialize-field field-name val)))

(define (serialize-cidr6 field-name val)
  (if (eq? val #f) #f (serialize-field field-name val)))

(define (ip? val)
  (if (string? val)
      (if (string-match "^([0-9]+\\.){3}[0-9]+$" val)
          (let ((numbers (string-tokenize val char-set:digit)))
            (all-lte numbers (list 255 255 255 255)))
          #f)
      (eq? val #f)))
(define (serialize-ip field-name val)
  (if (eq? val #f) #f (serialize-field field-name val)))

(define (keepalive? val)
  (and (list? val)
       (and (number? (car val))
            (number? (car (cdr val))))))
(define (serialize-keepalive field-name val)
  (format #t "~a ~a ~a\n" (uglify-field-name field-name)
          (number->string (car val)) (number->string (car (cdr val)))))

(define gateway? boolean?)
(define (serialize-gateway field-name val)
  (and val
       (format #t "push \"redirect-gateway\"\n")))


(define-configuration openvpn-remote-configuration
  (name
   (string "my-server")
   "Server name.")
  (port
   (number 1194)
   "Port number the server listens to."))

(define-configuration openvpn-ccd-configuration
  (name
   (string "client")
   "Client name.")
  (iroute
   (ip-mask #f)
   "Client own network")
  (ifconfig-push
   (ip-mask #f)
   "Client VPN IP."))

(define (openvpn-remote-list? val)
  (and (list? val)
       (or (eq? val '())
           (and (openvpn-remote-configuration? (car val))
                (openvpn-remote-list? (cdr val))))))
(define (serialize-openvpn-remote-list field-name val)
  (for-each (lambda (remote)
              (format #t "remote ~a ~a\n" (openvpn-remote-configuration-name remote)
                      (number->string (openvpn-remote-configuration-port remote))))
            val))

(define (openvpn-ccd-list? val)
  (and (list? val)
       (or (eq? val '())
           (and (openvpn-ccd-configuration? (car val))
                (openvpn-ccd-list? (cdr val))))))
(define (serialize-openvpn-ccd-list field-name val)
  #f)

(define (create-ccd-directory val)
  "Create a ccd directory containing files for the ccd configuration option
of OpenVPN.  Each file in this directory represents particular settings for a
client.  Each file is named after the name of the client."
  (let ((files (map (lambda (ccd)
                      (list (openvpn-ccd-configuration-name ccd)
                            (with-output-to-string
                              (lambda ()
                                (serialize-configuration
                                 ccd openvpn-ccd-configuration-fields)))))
                    val)))
    (computed-file "ccd"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (use-modules (guix build utils))
                         (use-modules (ice-9 match))
                         (mkdir-p #$output)
                         (for-each
                          (lambda (ccd)
                            (match ccd
                              ((name config-string)
                               (call-with-output-file
                                   (string-append #$output "/" name)
                                 (lambda (port) (display config-string port))))))
                          '#$files))))))

(define-syntax define-split-configuration
  (lambda (x)
    (syntax-case x ()
      ((_ name1 name2 (common-option ...) (first-option ...) (second-option ...))
       #`(begin
           (define-configuration #,#'name1
             common-option ...
             first-option ...)
           (define-configuration #,#'name2
             common-option ...
             second-option ...))))))

(define-split-configuration openvpn-client-configuration
  openvpn-server-configuration
  ((openvpn
    (file-like openvpn)
    "The OpenVPN package.")

   (pid-file
    (string "/var/run/openvpn/openvpn.pid")
    "The OpenVPN pid file.")

   (proto
    (proto 'udp)
    "The protocol (UDP or TCP) used to open a channel between clients and
servers.")

   (dev
    (dev 'tun)
    "The device type used to represent the VPN connection.")

   (ca
    (maybe-string "/etc/openvpn/ca.crt")
    "The certificate authority to check connections against.")

   (cert
    (maybe-string "/etc/openvpn/client.crt")
    "The certificate of the machine the daemon is running on. It should be signed
by the authority given in @code{ca}.")

   (key
    (maybe-string "/etc/openvpn/client.key")
    "The key of the machine the daemon is running on. It must be the key whose
certificate is @code{cert}.")

   (comp-lzo?
    (boolean #t)
    "Whether to use the lzo compression algorithm.")

   (persist-key?
    (boolean #t)
    "Don't re-read key files across SIGUSR1 or --ping-restart.")

   (persist-tun?
    (boolean #t)
    "Don't close and reopen TUN/TAP device or run up/down scripts across
SIGUSR1 or --ping-restart restarts.")

   (fast-io?
     (boolean #f)
     "(Experimental) Optimize TUN/TAP/UDP I/O writes by avoiding a call to
poll/epoll/select prior to the write operation.")

   (verbosity
    (number 3)
    "Verbosity level."))
  ;; client-specific configuration
  ((tls-auth
    (tls-auth-client #f)
    "Add an additional layer of HMAC authentication on top of the TLS control
channel to protect against DoS attacks.")

   (auth-user-pass
    maybe-string
     "Authenticate with server using username/password.  The option is a file
containing username/password on 2 lines.  Do not use a file-like object as it
would be added to the store and readable by any user.")

   (verify-key-usage?
    (key-usage #t)
    "Whether to check the server certificate has server usage extension.")

   (bind?
    (bind #f)
    "Bind to a specific local port number.")

   (resolv-retry?
    (resolv-retry #t)
    "Retry resolving server address.")

   (remote
    (openvpn-remote-list '())
    "A list of remote servers to connect to."))
  ;; server-specific configuration
  ((tls-auth
    (tls-auth-server #f)
    "Add an additional layer of HMAC authentication on top of the TLS control
channel to protect against DoS attacks.")

   (port
    (number 1194)
    "Specifies the port number on which the server listens.")

   (server
    (ip-mask "10.8.0.0 255.255.255.0")
    "An ip and mask specifying the subnet inside the virtual network.")

   (server-ipv6
    (cidr6 #f)
    "A CIDR notation specifying the IPv6 subnet inside the virtual network.")

   (dh
    (string "/etc/openvpn/dh2048.pem")
    "The Diffie-Hellman parameters file.")

   (ifconfig-pool-persist
    (string "/etc/openvpn/ipp.txt")
    "The file that records client IPs.")

   (redirect-gateway?
    (gateway #f)
    "When true, the server will act as a gateway for its clients.")

   (client-to-client?
    (boolean #f)
    "When true, clients are allowed to talk to each other inside the VPN.")

   (keepalive
    (keepalive '(10 120))
    "Causes ping-like messages to be sent back and forth over the link so that
each side knows when the other side has gone down. @code{keepalive} requires
a pair. The first element is the period of the ping sending, and the second
element is the timeout before considering the other side down.")

   (max-clients
    (number 100)
    "The maximum number of clients.")

   (status
    (string "/var/run/openvpn/status")
    "The status file. This file shows a small report on current connection. It
is truncated and rewritten every minute.")

   (client-config-dir
    (openvpn-ccd-list '())
    "The list of configuration for some clients.")))

(define (openvpn-config-file role config)
  (let ((config-str
         (with-output-to-string
           (lambda ()
             (serialize-configuration config
                                      (match role
                                        ('server
                                         openvpn-server-configuration-fields)
                                        ('client
                                         openvpn-client-configuration-fields))))))
        (ccd-dir (match role
                   ('server (create-ccd-directory
                             (openvpn-server-configuration-client-config-dir
                              config)))
                   ('client #f))))
    (computed-file "openvpn.conf"
                   #~(begin
                       (use-modules (ice-9 match))
                       (call-with-output-file #$output
                         (lambda (port)
                           (match '#$role
                             ('server (display "" port))
                             ('client (display "client\n" port)))
                           (display #$config-str port)
                           (match '#$role
                             ('server (display
                                       (string-append "client-config-dir "
                                                      #$ccd-dir "\n") port))
                             ('client (display "" port)))))))))

(define (openvpn-shepherd-service role)
  (lambda (config)
    (let* ((config-file (openvpn-config-file role config))
           (pid-file ((match role
                        ('server openvpn-server-configuration-pid-file)
                        ('client openvpn-client-configuration-pid-file))
                      config))
           (openvpn ((match role
                       ('server openvpn-server-configuration-openvpn)
                       ('client openvpn-client-configuration-openvpn))
                     config))
           (log-file (match role
                       ('server "/var/log/openvpn-server.log")
                       ('client "/var/log/openvpn-client.log"))))
      (list (shepherd-service
             (documentation (string-append "Run the OpenVPN "
                                           (match role
                                             ('server "server")
                                             ('client "client"))
                                           " daemon."))
             (provision (match role
                          ('server '(vpn-server))
                          ('client '(vpn-client))))
             (requirement '(user-processes networking))
             (start #~(make-forkexec-constructor
                       (list (string-append #$openvpn "/sbin/openvpn")
                             "--writepid" #$pid-file "--config" #$config-file
                             "--daemon")
                       #:pid-file #$pid-file
                       #:log-file #$log-file))
             (stop #~(make-kill-destructor)))))))

(define %openvpn-accounts
  (list (user-group (name "openvpn") (system? #t))
        (user-account
         (name "openvpn")
         (group "openvpn")
         (system? #t)
         (comment "Openvpn daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define %openvpn-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/run/openvpn")))

(define openvpn-server-service-type
  (service-type (name 'openvpn-server)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          (openvpn-shepherd-service 'server))
                       (service-extension account-service-type
                                          (const %openvpn-accounts))
                       (service-extension activation-service-type
                                          (const %openvpn-activation))))
                (description "Run the OpenVPN server, which allows you to
@emph{host} a @acronym{VPN, virtual private network}.")
                (default-value (openvpn-server-configuration))))

(define openvpn-client-service-type
  (service-type (name 'openvpn-client)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          (openvpn-shepherd-service 'client))
                       (service-extension account-service-type
                                          (const %openvpn-accounts))
                       (service-extension activation-service-type
                                          (const %openvpn-activation))))
                (description
                 "Run the OpenVPN client service, which allows you to connect
to an existing @acronym{VPN, virtual private network}.")
                (default-value (openvpn-client-configuration))))

(define-deprecated
  (openvpn-client-service #:key (config (openvpn-client-configuration)))
  openvpn-client-service-type
  (service openvpn-client-service-type config))

(define-deprecated
  (openvpn-server-service #:key (config (openvpn-server-configuration)))
  openvpn-server-service-type
  (service openvpn-server-service-type config))

(define (generate-openvpn-server-documentation)
  (generate-documentation
   `((openvpn-server-configuration
      ,openvpn-server-configuration-fields
      (ccd openvpn-ccd-configuration))
     (openvpn-ccd-configuration ,openvpn-ccd-configuration-fields))
   'openvpn-server-configuration))

(define (generate-openvpn-client-documentation)
  (generate-documentation
   `((openvpn-client-configuration
      ,openvpn-client-configuration-fields
      (remote openvpn-remote-configuration))
     (openvpn-remote-configuration ,openvpn-remote-configuration-fields))
   'openvpn-client-configuration))

;;;
;;; Strongswan.
;;;

(define-record-type* <strongswan-configuration>
  strongswan-configuration make-strongswan-configuration
  strongswan-configuration?
  (strongswan      strongswan-configuration-strongswan ;file-like
                   (default strongswan))
  (ipsec-conf      strongswan-configuration-ipsec-conf ;string|#f
                   (default #f))
  (ipsec-secrets   strongswan-configuration-ipsec-secrets ;string|#f
                   (default #f)))

;; In the future, it might be worth implementing a record type to configure
;; all of the plugins, but for *most* basic use cases, simply creating the
;; files will be sufficient. Same is true of charon-plugins.
(define strongswand-configuration-files
  (list "charon" "charon-logging" "pki" "pool" "scepclient"
        "swanctl" "tnc"))

;; Plugins to load. All of these plugins end up as configuration files in
;; strongswan.d/charon/.
(define charon-plugins
  (list "aes" "aesni" "attr" "attr-sql" "chapoly" "cmac" "constraints"
        "counters" "curl" "curve25519" "dhcp" "dnskey" "drbg" "eap-aka-3gpp"
        "eap-aka" "eap-dynamic" "eap-identity" "eap-md5" "eap-mschapv2"
        "eap-peap" "eap-radius" "eap-simaka-pseudonym" "eap-simaka-reauth"
        "eap-simaka-sql" "eap-sim" "eap-sim-file" "eap-tls" "eap-tnc"
        "eap-ttls" "ext-auth" "farp" "fips-prf" "gmp" "ha" "hmac"
        "kernel-netlink" "led" "md4" "md5" "mgf1" "nonce" "openssl" "pem"
        "pgp" "pkcs12" "pkcs1" "pkcs7" "pkcs8" "pubkey" "random" "rc2"
        "resolve" "revocation" "sha1" "sha2" "socket-default" "soup" "sql"
        "sqlite" "sshkey" "tnc-tnccs" "vici" "x509" "xauth-eap" "xauth-generic"
        "xauth-noauth" "xauth-pam" "xcbc"))

(define (strongswan-configuration-file config)
  (match-record config <strongswan-configuration>
    (strongswan ipsec-conf ipsec-secrets)
    (if (eq? (string? ipsec-conf) (string? ipsec-secrets))
        (let* ((strongswan-dir
                (computed-file
                 "strongswan.d"
                 #~(begin
                     (mkdir #$output)
                     ;; Create all of the configuration files strongswan.d/.
                     (map (lambda (conf-file)
                            (let* ((filename (string-append
                                              #$output "/"
                                              conf-file ".conf")))
                              (call-with-output-file filename
                                (lambda (port)
                                  (display
                                   "# Created by 'strongswan-service'\n"
                                   port)))))
                          (list #$@strongswand-configuration-files))
                     (mkdir (string-append #$output "/charon"))
                     ;; Create all of the plugin configuration files.
                     (map (lambda (plugin)
                            (let* ((filename (string-append
                                              #$output "/charon/"
                                              plugin ".conf")))
                              (call-with-output-file filename
                                (lambda (port)
                                  (format port "~a {
  load = yes
}"
                                          plugin)))))
                          (list #$@charon-plugins))))))
          ;; Generate our strongswan.conf to reflect the user configuration.
          (computed-file
           "strongswan.conf"
           #~(begin
               (call-with-output-file #$output
                 (lambda (port)
                   (display "# Generated by 'strongswan-service'.\n" port)
                   (format port "charon {
  load_modular = yes
  plugins {
    include ~a/charon/*.conf"
                           #$strongswan-dir)
                   (if #$ipsec-conf
                       (format port "
    stroke {
      load = yes
      secrets_file = ~a
    }
  }
}

starter {
  config_file = ~a
}

include ~a/*.conf"
                               #$ipsec-secrets
                               #$ipsec-conf
                               #$strongswan-dir)
                       (format port "
  }
}
include ~a/*.conf"
                               #$strongswan-dir)))))))
        (throw 'error
               (G_ "strongSwan ipsec-conf and ipsec-secrets must both be (un)set")))))

(define (strongswan-shepherd-service config)
  (let* ((ipsec (file-append strongswan "/sbin/ipsec"))
        (strongswan-conf-path (strongswan-configuration-file config)))
    (list (shepherd-service
           (requirement '(user-processes networking))
           (provision '(ipsec))
           (start #~(make-forkexec-constructor
                     (list #$ipsec "start" "--nofork")
                     #:environment-variables
                     (list (string-append "STRONGSWAN_CONF="
                                          #$strongswan-conf-path))))
           (stop #~(make-kill-destructor))
           (documentation
            "strongSwan's charon IKE keying daemon for IPsec VPN.")))))

(define strongswan-service-type
  (service-type
   (name 'strongswan)
   (extensions
    (list (service-extension shepherd-root-service-type
                             strongswan-shepherd-service)))
   (default-value (strongswan-configuration))
   (description
    "Connect to an IPsec @acronym{VPN, Virtual Private Network} with
strongSwan.")))

;;;
;;; Wireguard.
;;;

(define-record-type* <wireguard-peer>
  wireguard-peer make-wireguard-peer
  wireguard-peer?
  (name              wireguard-peer-name)
  (endpoint          wireguard-peer-endpoint
                     (default #f))     ;string
  (public-key        wireguard-peer-public-key)   ;string
  (preshared-key     wireguard-peer-preshared-key
                     (default #f))     ;string
  (allowed-ips       wireguard-peer-allowed-ips) ;list of strings
  (keep-alive        wireguard-peer-keep-alive
                     (default #f)))    ;integer

(define-record-type* <wireguard-configuration>
  wireguard-configuration make-wireguard-configuration
  wireguard-configuration?
  (wireguard              wireguard-configuration-wireguard ;file-like
                          (default wireguard-tools))
  (interface              wireguard-configuration-interface ;string
                          (default "wg0"))
  (addresses              wireguard-configuration-addresses ;string
                          (default '("10.0.0.1/32")))
  (port                   wireguard-configuration-port ;integer
                          (default 51820))
  (private-key            wireguard-configuration-private-key ;maybe-string
                          (default "/etc/wireguard/private.key"))
  (bootstrap-private-key? wireguard-configuration-bootstrap-private-key? ;boolean
                          (default #t))
  (peers                  wireguard-configuration-peers ;list of <wiregard-peer>
                          (default '()))
  (dns                    wireguard-configuration-dns ;list of strings
                          (default '()))
  (monitor-ips?           wireguard-configuration-monitor-ips? ;boolean
                          (default #f))
  (monitor-ips-interval   wireguard-configuration-monitor-ips-interval
                          (default '(next-minute (range 0 60 5)))) ;string | list
  (pre-up                 wireguard-configuration-pre-up ;list of strings
                          (default '()))
  (post-up                wireguard-configuration-post-up ;list of strings
                          (default '()))
  (pre-down               wireguard-configuration-pre-down ;list of strings
                          (default '()))
  (post-down              wireguard-configuration-post-down ;list of strings
                          (default '()))
  (shepherd-requirement   wireguard-configuration-shepherd-requirement ; list of symbols
                          (default '()))
  (table                  wireguard-configuration-table ;string
                          (default "auto"))
  (auto-start?            wireguard-configuration-auto-start? ;boolean
                          (default #t)))

(define (wireguard-configuration-file config)
  (define (peer->config peer)
    (match-record peer <wireguard-peer>
      (name public-key endpoint allowed-ips keep-alive)
      (let ((lines (list
                    (format #f "[Peer]   #~a" name)
                    (format #f "PublicKey = ~a" public-key)
                    (format #f "AllowedIPs = ~{~a~^, ~}" allowed-ips)
                    (format #f "~@[Endpoint = ~a~]" endpoint)
                    (format #f "~@[PersistentKeepalive = ~a~]" keep-alive))))
        (string-join (remove string-null? lines) "\n"))))

  (define (peers->preshared-keys peer keys)
    (let ((public-key (wireguard-peer-public-key peer))
          (preshared-key (wireguard-peer-preshared-key peer)))
      (if preshared-key
          (cons* public-key preshared-key keys)
          keys)))

  (match-record config <wireguard-configuration>
    (wireguard interface addresses port private-key peers dns
               pre-up post-up pre-down post-down table)
    (let* ((config-file (string-append interface ".conf"))
           (peer-keys (fold peers->preshared-keys (list) peers))
           (peers (map peer->config peers))
           (config
            (computed-file
             "wireguard-config"
             #~(begin
                 (use-modules (ice-9 format)
                              (srfi srfi-1))

                 (define lines
                   (list
                    "[Interface]"
                    (if (null? '#$addresses)
                        ""
                        (format #f "Address = ~{~a~^, ~}"
                                (list #$@addresses)))
                    (format #f "~@[Table = ~a~]" #$table)
                    (if (null? '#$pre-up)
                        ""
                        (format #f "~{PreUp = ~a~%~}" (list #$@pre-up)))
                    (if #$private-key
                        (format #f "PostUp = ~a set %i private-key ~a\
~{ peer ~a preshared-key ~a~}"
                                #$(file-append wireguard "/bin/wg")
                                #$private-key (list #$@peer-keys))
                        "")
                    (if (null? '#$post-up)
                        ""
                        (format #f "~{PostUp = ~a~%~}" (list #$@post-up)))
                    (if (null? '#$pre-down)
                        ""
                        (format #f "~{PreDown = ~a~%~}" (list #$@pre-down)))
                    (if (null? '#$post-down)
                        ""
                        (format #f "~{PostDown = ~a~%~}" (list #$@post-down)))
                    (format #f "~@[ListenPort = ~a~]" #$port)
                    (if (null? '#$dns)
                        ""
                        (format #f "DNS = ~{~a~^, ~}" (list #$@dns)))))

                 (mkdir #$output)
                 (chdir #$output)
                 (call-with-output-file #$config-file
                   (lambda (port)
                     (format port "~a~%~%~{~a~%~^~%~}"
                             (string-join (remove string-null? lines) "\n")
                             '#$peers)))))))
      (file-append config "/" config-file))))

(define (wireguard-activation config)
  (match-record config <wireguard-configuration>
    (private-key bootstrap-private-key? wireguard)
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 popen)
                     (ice-9 rdelim))
        (when (and #$private-key
                   #$bootstrap-private-key?)
          (mkdir-p (dirname #$private-key))
          (unless (file-exists? #$private-key)
            (let* ((pipe
                    (open-input-pipe (string-append
                                      #$(file-append wireguard "/bin/wg")
                                      " genkey")))
                   (key (read-line pipe)))
              (call-with-output-file #$private-key
                (lambda (port)
                  (display key port)))
              (chmod #$private-key #o400)
              (close-pipe pipe)))))))

;;; XXX: Copied from (guix scripts pack), changing define to define*.
(define-syntax-rule (define-with-source (variable args ...) body body* ...)
  "Bind VARIABLE to a procedure accepting ARGS defined as BODY, also setting
its source property."
  (begin
    (define* (variable args ...)
      body body* ...)
    (eval-when (load eval)
      (set-procedure-property! variable 'source
                               '(define* (variable args ...) body body* ...)))))

(define (wireguard-service-name interface)
  "Return the WireGuard service name (a symbol) configured to use INTERFACE."
  (symbol-append 'wireguard- (string->symbol interface)))

(define-with-source (strip-port/maybe endpoint #:key ipv6?)
  "Strip the colon and port, if present in ENDPOINT, a string."
  (if ipv6?
      (if (string-prefix? "[" endpoint)
          (first (string-split (string-drop endpoint 1) #\])) ;ipv6
          endpoint)
      (first (string-split endpoint #\:)))) ;ipv4

(define* (ipv4-address? address)
  "Predicate to check whether ADDRESS is a valid IPv4 address."
  (let ((address (strip-port/maybe address)))
    (false-if-exception
     (->bool (getaddrinfo address #f AI_NUMERICHOST AF_INET)))))

(define* (ipv6-address? address)
  "Predicate to check whether ADDRESS is a valid IPv6 address."
  (let ((address (strip-port/maybe address #:ipv6? #t)))
    (false-if-exception
     (->bool (getaddrinfo address #f AI_NUMERICHOST AF_INET6)))))

(define (host-name? name)
  "Predicate to check whether NAME is a host name, i.e. not an IP address."
  (not (or (ipv6-address? name) (ipv4-address? name))))

(define (endpoint-host-names peers)
  "Return an association list of endpoint host names keyed by their peer
public key, if any."
  (reverse
   (fold (lambda (peer host-names)
           (let ((public-key (wireguard-peer-public-key peer))
                 (endpoint (wireguard-peer-endpoint peer)))
             (if (and endpoint (host-name? endpoint))
                 (cons (cons public-key endpoint) host-names)
                 host-names)))
         '()
         peers)))

(define (wireguard-shepherd-service config)
  (match-record config <wireguard-configuration>
    (wireguard interface shepherd-requirement)
    (let ((wg-quick (file-append wireguard "/bin/wg-quick"))
          (auto-start? (wireguard-configuration-auto-start? config))
          (config (wireguard-configuration-file config)))
      (list (shepherd-service
             (requirement `(networking user-processes ,@shepherd-requirement))
             (provision (list (wireguard-service-name interface)))
             (start #~(lambda _
                       (invoke #$wg-quick "up" #$config)))
             (stop #~(lambda _
                       (invoke #$wg-quick "down" #$config)
                       #f))                       ;stopped!
             (actions (list (shepherd-configuration-action config)))
             (auto-start? auto-start?)
             (documentation "Run the Wireguard VPN tunnel"))))))

(define (wireguard-monitoring-jobs config)
  ;; Loosely based on WireGuard's own 'reresolve-dns.sh' shell script (see:
  ;; https://raw.githubusercontent.com/WireGuard/wireguard-tools/
  ;; master/contrib/reresolve-dns/reresolve-dns.sh).
  (match-record config <wireguard-configuration>
    (interface monitor-ips? monitor-ips-interval peers)
    (let ((host-names (endpoint-host-names peers)))
      (if monitor-ips?
          (if (null? host-names)
              (begin
                (warn "monitor-ips? is #t but no host name to monitor")
                '())
              ;; The mcron monitor job may be a string or a list; ungexp strips
              ;; one quote level, which must be added back when a list is
              ;; provided.
              (list
               #~(job
                  (if (string? #$monitor-ips-interval)
                      #$monitor-ips-interval
                      '#$monitor-ips-interval)
                  #$(program-file
                     (format #f "wireguard-~a-monitoring" interface)
                     (with-imported-modules (source-module-closure
                                             '((gnu services herd)
                                               (guix build utils)))
                       #~(begin
                           (use-modules (gnu services herd)
                                        (guix build utils)
                                        (ice-9 popen)
                                        (ice-9 match)
                                        (ice-9 textual-ports)
                                        (srfi srfi-1)
                                        (srfi srfi-26))

                           (define (resolve-host name)
                             "Return the IP address resolved from NAME."
                             (let* ((ai (car (getaddrinfo name)))
                                    (sa (addrinfo:addr ai)))
                               (inet-ntop (sockaddr:fam sa)
                                          (sockaddr:addr sa))))

                           (define wg #$(file-append wireguard-tools "/bin/wg"))

                           #$(procedure-source strip-port/maybe)

                           (define service-name '#$(wireguard-service-name
                                                    interface))

                           (when (live-service-running
                                  (current-service service-name))
                             (let* ((pipe (open-pipe* OPEN_READ wg "show"
                                                      #$interface "endpoints"))
                                    (lines (string-split (get-string-all pipe)
                                                         #\newline))
                                    ;; IPS is an association list mapping
                                    ;; public keys to IP addresses.
                                    (ips (map (match-lambda
                                                ((public-key ip)
                                                 (cons public-key
                                                       (strip-port/maybe ip))))
                                              (map (cut string-split <> #\tab)
                                                   (remove string-null?
                                                           lines)))))
                               (close-pipe pipe)
                               (for-each
                                (match-lambda
                                  ((key . host-name)
                                   (let ((resolved-ip (resolve-host
                                                       (strip-port/maybe
                                                        host-name)))
                                         (current-ip (assoc-ref ips key)))
                                     (unless (string=? resolved-ip current-ip)
                                       (format #t "resetting `~a' peer \
endpoint to `~a' due to stale IP (`~a' instead of `~a')~%"
                                               key host-name
                                               current-ip resolved-ip)
                                       (invoke wg "set" #$interface "peer" key
                                               "endpoint" host-name)))))
                                '#$host-names)))))))))
          '()))))                     ;monitor-ips? is #f

(define wireguard-service-type
  (service-type
   (name 'wireguard)
   (extensions
    (list (service-extension shepherd-root-service-type
                             wireguard-shepherd-service)
          (service-extension activation-service-type
                             wireguard-activation)
          (service-extension profile-service-type
                             (compose list
                                      wireguard-configuration-wireguard))
          (service-extension mcron-service-type
                             wireguard-monitoring-jobs)))
   (description "Set up Wireguard @acronym{VPN, Virtual Private Network}
tunnels.")))
