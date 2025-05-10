;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 John Darrington <jmd@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2017, 2018 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2018 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2019 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2019, 2021, 2024, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2019 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2019 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022, 2023 Andrew Tropin <andrew@trop.in>
;;; Copyright © 2023 Declan Tsien <declantsien@riseup.net>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 muradm <mail@muradm.net>
;;; Copyright © 2024 Nigko Yerden <nigko.yerden@gmail.com>
;;; Copyright © 2025 45mg <45mg.writes@gmail.com>
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

(define-module (gnu services networking)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services linux)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services dbus)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu system pam)
  #:use-module ((gnu system file-systems) #:select (file-system-mapping))
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages cluster)
  #:use-module (gnu packages connman)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages tor)
  #:use-module (gnu packages usb-modeswitch)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages ntp)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages ipfs)
  #:use-module (gnu build linux-container)
  #:autoload   (guix least-authority) (least-authority-wrapper)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:autoload   (guix ui) (display-hint)
  #:use-module (guix i18n)
  #:use-module (rnrs enums)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-43)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 string-fun)
  #:use-module (json)
  #:re-export (static-networking-service
               static-networking-service-type)
  #:export (%facebook-host-aliases ;deprecated
            block-facebook-hosts-service-type

            dhcp-client-service-type
            dhcp-client-configuration
            dhcp-client-configuration?
            dhcp-client-configuration-package
            dhcp-client-configuration-interfaces
            dhcp-client-configuration-config-file
            dhcp-client-configuration-shepherd-provision
            dhcp-client-configuration-shepherd-requirement
            dhcp-client-configuration-version

            dhcpd-service-type
            dhcpd-configuration
            dhcpd-configuration?
            dhcpd-configuration-package
            dhcpd-configuration-config-file
            dhcpd-configuration-version
            dhcpd-configuration-run-directory
            dhcpd-configuration-lease-file
            dhcpd-configuration-pid-file
            dhcpd-configuration-interfaces

            dhcpcd-service-type
            dhcpcd-configuration
            dhcpcd-configuration?
            dhcpcd-configuration-interfaces
            dhcpcd-configuration-command-arguments
            dhcpcd-configuration-host-name
            dhcpcd-configuration-duid
            dhcpcd-configuration-persistent?
            dhcpcd-configuration-option
            dhcpcd-configuration-require
            dhcpcd-configuration-slaac
            dhcpcd-configuration-no-option
            dhcpcd-configuration-no-hook
            dhcpcd-configuration-static
            dhcpcd-configuration-vendor-class-id
            dhcpcd-configuration-client-id
            dhcpcd-configuration-extra-content

            ntp-configuration
            ntp-configuration?
            ntp-configuration-ntp
            ntp-configuration-servers
            ntp-allow-large-adjustment?

            %ntp-servers
            ntp-server
            ntp-server-type
            ntp-server-address
            ntp-server-options

            ntp-service-type

            %openntpd-servers
            openntpd-configuration
            openntpd-configuration?
            openntpd-service-type

            inetd-configuration
            inetd-configuration?
            inetd-configuration-program
            inetd-configuration-entries
            inetd-entry
            inetd-entry?
            inetd-entry-node
            inetd-entry-name
            inetd-entry-socket-type
            inetd-entry-protocol
            inetd-entry-wait?
            inetd-entry-user
            inetd-entry-program
            inetd-entry-arguments
            inetd-service-type

            opendht-configuration
            opendht-configuration-peer-discovery?
            opendht-configuration-verbose?
            opendht-configuration-bootstrap-host
            opendht-configuration-port
            opendht-configuration-proxy-server-port
            opendht-configuration-proxy-server-port-tls
            opendht-configuration->command-line-arguments

            opendht-shepherd-service
            opendht-service-type

            tor-configuration
            tor-configuration?
            tor-configuration-tor
            tor-configuration-config-file
            tor-configuration-hidden-services
            tor-configuration-socks-socket-type
            tor-configuration-control-socket-path
            tor-configuration-transport-plugins
            tor-onion-service-configuration
            tor-onion-service-configuration?
            tor-onion-service-configuration-name
            tor-onion-service-configuration-mapping
	    tor-transport-plugin
	    tor-transport-plugin?
	    tor-plugin-role
	    tor-plugin-protocol
	    tor-plugin-program
            tor-hidden-service  ; deprecated
            tor-service-type

            network-manager-configuration
            network-manager-configuration?
            network-manager-configuration-shepherd-requirement
            network-manager-configuration-dns
            network-manager-configuration-vpn-plugins
            network-manager-service-type

            connman-general-configuration
            connman-general-configuration?
            connman-configuration
            connman-configuration?
            connman-configuration-connman
            connman-configuration-shepherd-requirement
            connman-configuration-disable-vpn?
            connman-configuration-iwd?
            connman-service-type

            modem-manager-configuration
            modem-manager-configuration?
            modem-manager-service-type

            usb-modeswitch-configuration
            usb-modeswitch-configuration?
            usb-modeswitch-configuration-usb-modeswitch
            usb-modeswitch-configuration-usb-modeswitch-data
            usb-modeswitch-service-type

            wpa-supplicant-configuration
            wpa-supplicant-configuration?
            wpa-supplicant-configuration-wpa-supplicant
            wpa-supplicant-configuration-requirement
            wpa-supplicant-configuration-pid-file
            wpa-supplicant-configuration-dbus?
            wpa-supplicant-configuration-interface
            wpa-supplicant-configuration-config-file
            wpa-supplicant-configuration-extra-options
            wpa-supplicant-service-type

            hostapd-configuration
            hostapd-configuration?
            hostapd-configuration-package
            hostapd-configuration-interface
            hostapd-configuration-ssid
            hostapd-configuration-broadcast-ssid?
            hostapd-configuration-channel
            hostapd-configuration-driver
            hostapd-service-type

            simulated-wifi-service-type

            openvswitch-service-type
            openvswitch-configuration

            iptables-configuration
            iptables-configuration?
            iptables-configuration-iptables
            iptables-configuration-ipv4-rules
            iptables-configuration-ipv6-rules
            iptables-service-type

            nftables-service-type
            nftables-configuration
            nftables-configuration?
            nftables-configuration-package
            nftables-configuration-debug-levels
            nftables-configuration-ruleset
            %default-nftables-ruleset

            pagekite-service-type
            pagekite-configuration
            pagekite-configuration?
            pagekite-configuration-package
            pagekite-configuration-kitename
            pagekite-configuration-kitesecret
            pagekite-configuration-frontend
            pagekite-configuration-kites
            pagekite-configuration-extra-file

            yggdrasil-service-type
            yggdrasil-configuration
            yggdrasil-configuration?
            yggdrasil-configuration-autoconf?
            yggdrasil-configuration-config-file
            yggdrasil-configuration-log-level
            yggdrasil-configuration-log-to
            yggdrasil-configuration-json-config
            yggdrasil-configuration-package

            ipfs-service-type
            ipfs-configuration
            ipfs-configuration?
            ipfs-configuration-package
            ipfs-configuration-gateway
            ipfs-configuration-api

            keepalived-configuration
            keepalived-configuration?
            keepalived-service-type))

;;; Commentary:
;;;
;;; Networking services.
;;;
;;; Code:

(define %unroutable-ipv4
  ;; Unroutable address, as per <https://www.rfc-editor.org/rfc/rfc5737>.
  "203.0.113.1")

(define %unroutable-ipv6
  ;; Unroutable address, as per <https://www.rfc-editor.org/rfc/rfc6666>.
  "0100::")

(define facebook-host-aliases
  ;; This is the list of known Facebook hosts to be added to /etc/hosts if you
  ;; are to block it.
  (let ((domains '("facebook.com" "www.facebook.com"
                   "login.facebook.com" "www.login.facebook.com"
                   "fbcdn.net" "www.fbcdn.net" "fbcdn.com" "www.fbcdn.com"
                   "static.ak.fbcdn.net" "static.ak.connect.facebook.com"
                   "connect.facebook.net" "www.connect.facebook.net"
                   "apps.facebook.com")))
    (append-map (lambda (name)
                  (map (lambda (addr)
                         (host addr name))
                       (list %unroutable-ipv4 %unroutable-ipv6)))
                domains)))

(define-deprecated %facebook-host-aliases
  block-facebook-hosts-service-type
  (string-join
   (map (lambda (x)
          (string-append (host-address x) "\t"
                         (host-canonical-name x) "\n"))
        facebook-host-aliases)))

(define block-facebook-hosts-service-type
  (service-type
   (name 'block-facebook-hosts)
   (extensions
    (list (service-extension hosts-service-type
                             (const facebook-host-aliases))))
   (default-value #f)
   (description "Add a list of known Facebook hosts to @file{/etc/hosts}")))

(define-record-type* <dhcp-client-configuration>
  dhcp-client-configuration make-dhcp-client-configuration
  dhcp-client-configuration?
  (package      dhcp-client-configuration-package ;file-like
                (default isc-dhcp))
  (shepherd-requirement dhcp-client-configuration-shepherd-requirement
                        (default '()))
  (shepherd-provision   dhcp-client-configuration-shepherd-provision
                        (default '(networking)))
  (config-file dhcp-client-configuration-config-file
               (default #f))
  (interfaces   dhcp-client-configuration-interfaces
                (default 'all))         ;'all | list of strings
  (version dhcp-client-configuration-version ;"4", "6", or "4o6"
           (default "4")))

(define dhcp-client-shepherd-service
  (match-lambda
    ((? dhcp-client-configuration? config)
     (match-record config <dhcp-client-configuration>
                   (package shepherd-requirement shepherd-provision
                            interfaces config-file version)
       ;; Version the PID file to avoid conflicts in case multiple DHCP
       ;; clients are run concurrently.
       (let ((pid-file (if (string=? "4" version)
                           "/var/run/dhclient.pid"
                           (string-append "/var/run/dhclient-" version ".pid"))))
         (list (shepherd-service
                (documentation "Set up networking via DHCP.")
                (requirement `(user-processes udev ,@shepherd-requirement))
                (provision shepherd-provision)

                ;; XXX: Running with '-nw' ("no wait") avoids blocking for a
                ;; minute when networking is unavailable, but also means that
                ;; the interface is not up yet when 'start' completes.  To
                ;; wait for the interface to be ready, one should instead
                ;; monitor udev events.
                (start #~(lambda _
                           (define dhclient
                             (string-append #$package "/sbin/dhclient"))

                           ;; When invoked without any arguments, 'dhclient'
                           ;; discovers all non-loopback interfaces *that are
                           ;; up*.  However, the relevant interfaces are
                           ;; typically down at this point.  Thus we perform
                           ;; our own interface discovery here.
                           (define valid?
                             (lambda (interface)
                               (and (arp-network-interface? interface)
                                    (not (loopback-network-interface? interface))
                                    ;; XXX: Make sure the interfaces are up so
                                    ;; that 'dhclient' can actually
                                    ;; send/receive over them.  Ignore those
                                    ;; that cannot be activated.
                                    (false-if-exception
                                     (set-network-interface-up interface)))))
                           (define ifaces
                             (filter valid?
                                     #$(match interfaces
                                         ('all
                                          #~(all-network-interface-names))
                                         (_
                                          #~'#$interfaces))))

                           (define config-file-args
                             (if #$config-file
                                 (list "-cf" #$config-file)
                                 '()))

                           (false-if-exception (delete-file #$pid-file))
                           (let ((status (spawn-command
                                          ;; By default dhclient uses a
                                          ;; pre-standardization implementation of
                                          ;; DDNS, which is incompatable with
                                          ;; non-ISC DHCP servers; thus, pass '-I'.
                                          ;; <https://kb.isc.org/docs/aa-01091>.
                                          `(,dhclient "-nw" "-I"
                                                      #$(string-append "-" version)
                                                      "-pf" ,#$pid-file
                                                      ,@config-file-args
                                                      ,@ifaces))))
                             (and (zero? status)
                                  (read-pid-file #$pid-file)))))
                (stop #~(make-kill-destructor)))))))
    (package
     (warning (G_ "'dhcp-client' service now expects a \
'dhcp-client-configuration' record~%"))
     (display-hint (G_ "The value associated with instances of
@code{dhcp-client-service-type} must now be a @code{dhcp-client-configuration}
record instead of a package.  Please adjust your configuration accordingly."))
     (dhcp-client-shepherd-service
      (dhcp-client-configuration
       (package package))))))

(define dhcp-client-service-type
  (service-type (name 'dhcp-client)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          dhcp-client-shepherd-service)))
                (default-value (dhcp-client-configuration))
                (description "Run @command{dhcp}, a Dynamic Host Configuration
Protocol (DHCP) client, on all the non-loopback network interfaces.")))

(define-record-type* <dhcpd-configuration>
  dhcpd-configuration make-dhcpd-configuration
  dhcpd-configuration?
  (package   dhcpd-configuration-package ;file-like
             (default isc-dhcp))
  (config-file   dhcpd-configuration-config-file ;file-like
                 (default #f))
  (version dhcpd-configuration-version ;"4", "6", or "4o6"
              (default "4"))
  (run-directory dhcpd-configuration-run-directory
                 (default "/run/dhcpd"))
  (lease-file dhcpd-configuration-lease-file
              (default "/var/db/dhcpd.leases"))
  (pid-file dhcpd-configuration-pid-file
            (default "/run/dhcpd/dhcpd.pid"))
  ;; list of strings, e.g. (list "enp0s25")
  (interfaces dhcpd-configuration-interfaces
              (default '())))

(define (dhcpd-shepherd-service config)
  (match-record config <dhcpd-configuration>
    (package config-file version run-directory
             lease-file pid-file interfaces)
    (unless config-file
      (error "Must supply a config-file"))
    (list (shepherd-service
           ;; Allow users to easily run multiple versions simultaneously.
           (provision (list (string->symbol
                             (string-append "dhcpv" version "-daemon"))))
           (documentation (string-append "Run the DHCPv" version " daemon"))
           (requirement '(networking))
           (start #~(make-forkexec-constructor
                     '(#$(file-append package "/sbin/dhcpd")
                       #$(string-append "-" version)
                       "-lf" #$lease-file
                       "-pf" #$pid-file
                       "-cf" #$config-file
                       #$@interfaces)
                     #:pid-file #$pid-file))
           (stop #~(make-kill-destructor))))))

(define (dhcpd-activation config)
  (match-record config <dhcpd-configuration>
    (package config-file version run-directory
             lease-file pid-file interfaces)
    (with-imported-modules '((guix build utils))
      #~(begin
          (unless (file-exists? #$run-directory)
            (mkdir #$run-directory))
          ;; According to the DHCP manual (man dhcpd.leases), the lease
          ;; database must be present for dhcpd to start successfully.
          (unless (file-exists? #$lease-file)
            (with-output-to-file #$lease-file
              (lambda _ (display ""))))
          ;; Validate the config.
          (invoke/quiet
           #$(file-append package "/sbin/dhcpd")
           #$(string-append "-" version)
           "-t" "-cf" #$config-file)))))

(define dhcpd-service-type
  (service-type
   (name 'dhcpd)
   (extensions
    (list (service-extension shepherd-root-service-type dhcpd-shepherd-service)
          (service-extension activation-service-type dhcpd-activation)))
   (description "Run a DHCP (Dynamic Host Configuration Protocol) daemon.  The
daemon is responsible for allocating IP addresses to its client.")))


;;
;; DHCPCD.
;;

(define (serialize-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-replace-substring
      (if (string-suffix? "?" str)
        (string-drop-right str 1)
        str)
      "-" "")))

(define (dhcpcd-serialize-string field-name value)
  (if (equal? field-name 'extra-content)
      #~(string-append #$value "\n")
      #~(format #f "~a ~a~%" #$(serialize-field-name field-name) #$value)))

(define (dhcpcd-serialize-boolean field-name value)
  (if value
    #~(format #f "~a~%" #$(serialize-field-name field-name))
    ""))

(define (dhcpcd-serialize-list-of-strings field-name value)
  #~(string-append #$@(map (cut dhcpcd-serialize-string field-name <>) value)))

;; Some fields (e.g. host-name) can be specified with an empty string argument.
;; Therefore, we need a maybe type to differentiate disabled/empty-string.
(define-maybe string (prefix dhcpcd-))

(define-configuration dhcpcd-configuration
  (interfaces
    (list '())
    "List of networking interfaces---e.g., @code{\"eth0\"}---to start a DHCP client
for.  If no interface is specified (i.e., the list is empty) then @command{dhcpcd}
discovers available Ethernet interfaces, that can be configured, automatically."
    empty-serializer)
  (command-arguments
    (list '("-q" "-q"))
    "List of additional command-line options."
    empty-serializer)

  ;; The following defaults replicate the default dhcpcd configuration file.
  ;;
  ;; See https://github.com/NetworkConfiguration/dhcpcd/tree/v10.0.10#configuration
  (host-name
    (maybe-string "")
    "Host name to send via DHCP, defaults to the current system host name.")
  (duid
    (maybe-string "")
    "DHCPv4 clients require a unique client identifier, this option uses the DHCPv6
Unique Identifier as a DHCPv4 client identifier as well.  For more information, refer
to @uref{https://www.rfc-editor.org/rfc/rfc4361, RFC 4361} and @code{dhcpcd.conf(5)}.")
  (persistent?
    (boolean #t)
    "When true, automatically de-configure the interface when @command{dhcpcd} exits.")
  (option
    (list-of-strings
      '("rapid_commit"
        "domain_name_servers"
        "domain_name"
        "domain_search"
        "host_name"
        "classless_static_routes"
        "interface_mtu"))
    "List of options to request from the server.")
  (require
    (list-of-strings '("dhcp_server_identifier"))
    "List of options to require in responses.")
  (slaac
    (maybe-string "private")
    "Interface identifier used for SLAAC generated IPv6 addresses.")

  ;; Common options not set in the default configuration file.
  (no-option
    (list-of-strings '())
    "List of options to remove from the message before it's processed.")
  (no-hook
    (list-of-strings '())
    "List of hook script which should not be invoked.")
  (static
    (list-of-strings '())
    "DHCP client can request different options from a DHCP server, through
@code{static} it is possible to configure static values for selected options.  For
example, @code{\"domain_name_servers=127.0.0.1\"}.")
  (vendor-class-id
    maybe-string
    "Set the DHCP Vendor Class (e.g., @code{MSFT}).  For more information, refer
to @uref{https://www.rfc-editor.org/rfc/rfc2132#section-9.13,RFC 2132}.")
  (client-id
    maybe-string
    "Use the interface hardware address or the given string as a client identifier,
this is matually exclusive with the @code{duid} option.")

  ;; Escape hatch for the generated configuration file.
  (extra-content
    maybe-string
    "Extra content to append to the configuration as-is.")

  (prefix dhcpcd-))

(define (dhcpcd-config-file config)
  (mixed-text-file "dhcpcd.conf"
    (serialize-configuration
      config
      dhcpcd-configuration-fields)))

(define dhcpcd-account-service
  (list (user-group (name "dhcpcd") (system? #t))
        (user-account
          (name "dhcpcd")
          (group "dhcpcd")
          (system? #t)
          (comment "dhcpcd daemon user")
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define (dhcpcd-shepherd-service config)
  (let* ((config-file (dhcpcd-config-file config))
         (command-args (dhcpcd-configuration-command-arguments config))
         (ifaces (dhcpcd-configuration-interfaces config)))
    (list (shepherd-service
            (documentation "dhcpcd daemon.")
            (provision '(networking))
            (requirement '(user-processes udev))
            (actions (list (shepherd-configuration-action config-file)))
            (start
              #~(make-forkexec-constructor
                    (list (string-append #$dhcpcd "/sbin/dhcpcd")
                          #$@command-args "-B" "-f" #$config-file #$@ifaces)))
            (stop #~(make-kill-destructor))))))

(define dhcpcd-service-type
  (service-type (name 'dhcpcd)
                (description "Run the dhcpcd daemon.")
                (extensions
                 (list (service-extension account-service-type
                                          (const dhcpcd-account-service))
                       (service-extension shepherd-root-service-type
                                          dhcpcd-shepherd-service)))
                (compose concatenate)
                (default-value (dhcpcd-configuration))))


;;;
;;; NTP.
;;;

(define ntp-server-types (make-enumeration
                          '(pool
                            server
                            peer
                            broadcast
                            manycastclient)))

(define-record-type* <ntp-server>
  ntp-server make-ntp-server
  ntp-server?
  ;; The type can be one of the symbols of the NTP-SERVER-TYPE? enumeration.
  (type ntp-server-type
        (default 'server))
  (address ntp-server-address)    ; a string
  ;; The list of options can contain single option names or tuples in the form
  ;; '(name value).
  (options ntp-server-options
           (default '())))

(define (ntp-server->string ntp-server)
  ;; Serialize the NTP server object as a string, ready to use in the NTP
  ;; configuration file.
  (define (flatten lst)
    (reverse
     (let loop ((x lst)
                (res '()))
       (if (list? x)
           (fold loop res x)
           (cons (format #f "~a" x) res)))))

  (match-record ntp-server <ntp-server>
    (type address options)
    ;; XXX: It'd be neater if fields were validated at the syntax level (for
    ;; static ones at least).  Perhaps the Guix record type could support a
    ;; predicate property on a field?
    (unless (enum-set-member? type ntp-server-types)
      (error "Invalid NTP server type" type))
    (string-join (cons* (symbol->string type)
                        address
                        (flatten options)))))

(define %ntp-servers
  ;; Default set of NTP servers. These URLs are managed by the NTP Pool project.
  ;; Within Guix, Leo Famulari <leo@famulari.name> is the administrative contact
  ;; for this NTP pool "zone".
  ;; The full list of available URLs are 0.guix.pool.ntp.org,
  ;; 1.guix.pool.ntp.org, 2.guix.pool.ntp.org, and 3.guix.pool.ntp.org.  We
  ;; use 2.guix.pool.ntp.org as default as it supports IPv6, contrary to the
  ;; others.
  (list
   (ntp-server
    (type 'pool)
    (address "2.guix.pool.ntp.org")
    (options '("iburst")))))               ;as recommended in the ntpd manual

(define-record-type* <ntp-configuration>
  ntp-configuration make-ntp-configuration
  ntp-configuration?
  (ntp      ntp-configuration-ntp
            (default ntp))
  (servers  ntp-configuration-servers     ;list of <ntp-server> objects
            (default %ntp-servers))
  (allow-large-adjustment? ntp-allow-large-adjustment?
                           (default #t))) ;as recommended in the ntpd manual

(define (ntp-shepherd-service config)
  (match-record config <ntp-configuration>
    (ntp servers allow-large-adjustment?)
    ;; TODO: Add authentication support.
    (define config
      (string-append "driftfile /var/run/ntpd/ntp.drift\n"
                     (string-join (map ntp-server->string servers) "\n")
                     "
# Disable status queries as a workaround for CVE-2013-5211:
# <http://support.ntp.org/bin/view/Main/SecurityNotice#DRDoS_Amplification_Attack_using>.
restrict default kod nomodify notrap nopeer noquery limited
restrict -6 default kod nomodify notrap nopeer noquery limited

# Yet, allow use of the local 'ntpq'.
restrict 127.0.0.1
restrict -6 ::1

# This is required to use servers from a pool directive when using the 'nopeer'
# option by default, as documented in the 'ntp.conf' manual.
restrict source notrap nomodify noquery\n"))

    (define ntpd.conf
      (plain-file "ntpd.conf" config))

    (list (shepherd-service
           (provision '(ntpd))
           (documentation "Run the Network Time Protocol (NTP) daemon.")
           (requirement '(user-processes networking))
           (actions (list (shepherd-configuration-action ntpd.conf)))
           (start #~(make-forkexec-constructor
                     (list (string-append #$ntp "/bin/ntpd") "-n"
                           "-c" #$ntpd.conf "-u" "ntpd"
                           #$@(if allow-large-adjustment?
                                  '("-g")
                                  '()))
                     #:log-file "/var/log/ntpd.log"))
           (stop #~(make-kill-destructor))))))

(define %ntp-accounts
  (list (user-account
         (name "ntpd")
         (group "nogroup")
         (system? #t)
         (comment "NTP daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))


(define (ntp-service-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define %user
          (getpw "ntpd"))

        (let ((directory "/var/run/ntpd"))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))))))

(define ntp-service-type
  (service-type (name 'ntp)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          ntp-shepherd-service)
                       (service-extension account-service-type
                                          (const %ntp-accounts))
                       (service-extension activation-service-type
                                          ntp-service-activation)))
                (description
                 "Run the @command{ntpd}, the Network Time Protocol (NTP)
daemon of the @uref{http://www.ntp.org, Network Time Foundation}.  The daemon
will keep the system clock synchronized with that of the given servers.")
                (default-value (ntp-configuration))))


;;;
;;; OpenNTPD.
;;;

(define %openntpd-servers
  (map ntp-server-address %ntp-servers))

(define-record-type* <openntpd-configuration>
  openntpd-configuration make-openntpd-configuration
  openntpd-configuration?
  (openntpd                openntpd-configuration-openntpd
                           (default openntpd))
  (listen-on               openntpd-listen-on
                           (default '("127.0.0.1"
                                      "::1")))
  (query-from              openntpd-query-from
                           (default '()))
  (sensor                  openntpd-sensor
                           (default '()))
  (server                  openntpd-server
                           (default '()))
  (servers                 openntpd-servers
                           (default %openntpd-servers))
  (constraint-from         openntpd-constraint-from
                           (default '()))
  (constraints-from        openntpd-constraints-from
                           (default '())))

(define (openntpd-configuration->string config)

  (define (quote-field? name)
    (member name '("constraints from")))

  (match-record config <openntpd-configuration>
    (listen-on query-from sensor server servers constraint-from
               constraints-from)
    (string-append
     (string-join
      (concatenate
       (filter-map (lambda (field values)
                     (match values
                       (() #f)          ;discard entry with filter-map
                       ((val ...)       ;validate value type
                        (map (lambda (value)
                               (if (quote-field? field)
                                   (format #f "~a \"~a\"" field value)
                                   (format #f "~a ~a" field value)))
                             values))))
                   ;; The entry names.
                   '("listen on" "query from" "sensor" "server" "servers"
                     "constraint from" "constraints from")
                   ;; The corresponding entry values.
                   (list listen-on query-from sensor server servers
                         constraint-from constraints-from)))
      "\n")
     "\n")))                              ;add a trailing newline

(define (openntpd-shepherd-service config)
  (let ((openntpd (openntpd-configuration-openntpd config)))

    (define ntpd.conf
      (plain-file "ntpd.conf" (openntpd-configuration->string config)))

    (list (shepherd-service
           (provision '(ntpd))
           (documentation "Run the Network Time Protocol (NTP) daemon.")
           (requirement '(user-processes networking))
           (start #~(make-forkexec-constructor
                     (list (string-append #$openntpd "/sbin/ntpd")
                           "-f" #$ntpd.conf
                           "-d") ;; don't daemonize
                     ;; When ntpd is daemonized it repeatedly tries to respawn
                     ;; while running, leading shepherd to disable it.  To
                     ;; prevent spamming stderr, redirect output to logfile.
                     #:log-file "/var/log/ntpd.log"))
           (stop #~(make-kill-destructor))
           (actions (list (shepherd-configuration-action ntpd.conf)))))))

(define (openntpd-service-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (mkdir-p "/var/db")
        (mkdir-p "/var/run")
        (unless (file-exists? "/var/db/ntpd.drift")
          (with-output-to-file "/var/db/ntpd.drift"
                               (lambda _
                                 (format #t "0.0")))))))

(define openntpd-service-type
  (service-type (name 'openntpd)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          openntpd-shepherd-service)
                       (service-extension account-service-type
                                          (const %ntp-accounts))
                       (service-extension profile-service-type
                                          (compose list openntpd-configuration-openntpd))
                       (service-extension activation-service-type
                                          openntpd-service-activation)))
                (default-value (openntpd-configuration))
                (description
                 "Run the @command{ntpd}, the Network Time Protocol (NTP)
daemon, as implemented by @uref{http://www.openntpd.org, OpenNTPD}.  The
daemon will keep the system clock synchronized with that of the given servers.")))


;;;
;;; Inetd.
;;;

(define-record-type* <inetd-configuration> inetd-configuration
  make-inetd-configuration
  inetd-configuration?
  (program           inetd-configuration-program   ;file-like
                     (default (file-append inetutils "/libexec/inetd")))
  (entries           inetd-configuration-entries   ;list of <inetd-entry>
                     (default '())))

(define-record-type* <inetd-entry> inetd-entry make-inetd-entry
  inetd-entry?
  (node              inetd-entry-node         ;string or #f
                     (default #f))
  (name              inetd-entry-name)        ;string, from /etc/services

  (socket-type       inetd-entry-socket-type) ;stream | dgram | raw |
                                              ;rdm | seqpacket
  (protocol          inetd-entry-protocol)    ;string, from /etc/protocols

  (wait?             inetd-entry-wait?        ;Boolean
                     (default #t))
  (user              inetd-entry-user)        ;string

  (program           inetd-entry-program      ;string or file-like object
                     (default "internal"))
  (arguments         inetd-entry-arguments    ;list of strings or file-like objects
                     (default '())))

(define (inetd-config-file entries)
  (apply mixed-text-file "inetd.conf"
         (map
          (lambda (entry)
            (let* ((node (inetd-entry-node entry))
                   (name (inetd-entry-name entry))
                   (socket
                    (if node (string-append node ":" name) name))
                   (type
                    (match (inetd-entry-socket-type entry)
                      ((or 'stream 'dgram 'raw 'rdm 'seqpacket)
                       (symbol->string (inetd-entry-socket-type entry)))))
                   (protocol (inetd-entry-protocol entry))
                   (wait (if (inetd-entry-wait? entry) "wait" "nowait"))
                   (user (inetd-entry-user entry))
                   (program (inetd-entry-program entry))
                   (args (inetd-entry-arguments entry)))
              #~(string-append
                 (string-join
                  (list #$@(list socket type protocol wait user program) #$@args)
                  " ") "\n")))
          entries)))

(define (inetd-shepherd-service config)
  (let ((entries (inetd-configuration-entries config)))
    (if (null? entries)
        '()                                       ;do nothing
        (let ((program (inetd-configuration-program config)))
          (list (shepherd-service
                 (documentation "Run inetd.")
                 (provision '(inetd))
                 (requirement '(user-processes networking syslogd))
                 (start #~(make-forkexec-constructor
                           (list #$program #$(inetd-config-file entries))
                           #:pid-file "/var/run/inetd.pid"))
                 (stop #~(make-kill-destructor))))))))

(define-public inetd-service-type
  (service-type
   (name 'inetd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             inetd-shepherd-service)))

   ;; The service can be extended with additional lists of entries.
   (compose concatenate)
   (extend (lambda (config entries)
             (inetd-configuration
              (inherit config)
              (entries (append (inetd-configuration-entries config)
                               entries)))))
   (description
    "Start @command{inetd}, the @dfn{Internet superserver}.  It is responsible
for listening on Internet sockets and spawning the corresponding services on
demand.")))


;;;
;;; OpenDHT, the distributed hash table network used by Jami
;;;

(define-maybe/no-serialization number)
(define-maybe/no-serialization string)

;;; To generate the documentation of the following configuration record, you
;;; can evaluate: (configuration->documentation 'opendht-configuration)
(define-configuration/no-serialization opendht-configuration
  (opendht
   (file-like opendht)
   "The @code{opendht} package to use.")
  (peer-discovery?
   (boolean #false)
   "Whether to enable the multicast local peer discovery mechanism.")
  (enable-logging?
   (boolean #false)
   "Whether to enable logging messages to syslog.  It is disabled by default
as it is rather verbose.")
  (debug?
   (boolean #false)
   "Whether to enable debug-level logging messages.  This has no effect if
logging is disabled.")
  (bootstrap-host
   (maybe-string "bootstrap.jami.net:4222")
   "The node host name that is used to make the first connection to the
network.  A specific port value can be provided by appending the @code{:PORT}
suffix.  By default, it uses the Jami bootstrap nodes, but any host can be
specified here.  It's also possible to disable bootstrapping by explicitly
setting this field to @code{%unset-value}.")
  (port
   (maybe-number 4222)
   "The UDP port to bind to.  When left unspecified, an available port is
automatically selected.")
  (proxy-server-port
   maybe-number
   "Spawn a proxy server listening on the specified port.")
  (proxy-server-port-tls
   maybe-number
   "Spawn a proxy server listening to TLS connections on the specified
port."))

(define %opendht-accounts
  ;; User account and groups for Tor.
  (list (user-group (name "opendht") (system? #t))
        (user-account
         (name "opendht")
         (group "opendht")
         (system? #t)
         (comment "OpenDHT daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (opendht-configuration->command-line-arguments config)
  "Derive the command line arguments used to launch the OpenDHT daemon from
CONFIG, an <opendht-configuration> object."
  (match-record config <opendht-configuration>
    (opendht bootstrap-host enable-logging? port debug? peer-discovery?
             proxy-server-port proxy-server-port-tls)
    (let ((dhtnode (least-authority-wrapper
                    ;; XXX: Work around lack of support for multiple outputs
                    ;; in 'file-append'.
                    (computed-file "dhtnode"
                                   #~(symlink
                                      (string-append #$opendht:tools
                                                     "/bin/dhtnode")
                                      #$output))
                    #:name "dhtnode"
                    #:mappings (list (file-system-mapping
                                      (source "/dev/log") ;for syslog
                                      (target source)))
                    #:namespaces (delq 'net %namespaces))))
      `(,dhtnode
        "--service"                     ;non-forking mode
        ,@(if (string? bootstrap-host)
              (list "--bootstrap" bootstrap-host))
        ,@(if enable-logging?
              (list "--syslog")
              '())
        ,@(if (number? port)
              (list "--port" (number->string port))
              '())
        ,@(if debug?
              (list "--verbose")
              '())
        ,@(if peer-discovery?
              (list "--peer-discovery")
              '())
        ,@(if (number? proxy-server-port)
              (list "--proxyserver" (number->string proxy-server-port))
              '())
        ,@(if (number? proxy-server-port-tls)
              (list "--proxyserverssl" (number->string proxy-server-port-tls))
              '())))))

(define (opendht-shepherd-service config)
  "Return a <shepherd-service> running OpenDHT."
  (shepherd-service
   (documentation "Run an OpenDHT node.")
   (provision '(opendht dhtnode dhtproxy))
   (requirement '(user-processes networking syslogd))
   (start #~(make-forkexec-constructor
             (list #$@(opendht-configuration->command-line-arguments config))
             #:user "opendht"
             #:group "opendht"))
   (stop #~(make-kill-destructor))))

(define opendht-service-type
  (service-type
   (name 'opendht)
   (default-value (opendht-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list opendht-shepherd-service))
          (service-extension account-service-type
                             (const %opendht-accounts))))
   (description "Run the OpenDHT @command{dhtnode} command that allows
participating in the distributed hash table based OpenDHT network.  The
service can be configured to act as a proxy to the distributed network, which
can be useful for portable devices where minimizing energy consumption is
paramount.  OpenDHT was originally based on Kademlia and adapted for
applications in communication.  It is used by Jami, for example.")))


;;;
;;; Tor.
;;;

(define-record-type* <tor-configuration>
  tor-configuration make-tor-configuration
  tor-configuration?
  (tor              tor-configuration-tor
                    (default tor))
  (config-file      tor-configuration-config-file
                    (default (plain-file "empty" "")))
  (hidden-services  tor-configuration-hidden-services
                    (default '()))
  (socks-socket-type tor-configuration-socks-socket-type ; 'tcp or 'unix
                     (default 'tcp))
  (control-socket?  tor-configuration-control-socket-path
                    (default #f))
  (transport-plugins tor-configuration-transport-plugins
                    (default '())))

(define %tor-accounts
  ;; User account and groups for Tor.
  (list (user-group (name "tor") (system? #t))
        (user-account
         (name "tor")
         (group "tor")
         (system? #t)
         (comment "Tor daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define-configuration/no-serialization tor-onion-service-configuration
  (name
   string
   "Name for this Onion Service.  This creates a
@file{/var/lib/tor/hidden-services/@var{name}} directory, where the
@file{hostname} file contains the @indicateurl{.onion} host name for this
Onion Service.")

  (mapping
   alist
   "Association list of port to address mappings.  The following example:
@lisp
'((22 . \"127.0.0.1:22\")
  (80 . \"127.0.0.1:8080\"))
@end lisp
maps ports 22 and 80 of the Onion Service to the local ports 22 and 8080."))

(define-record-type* <tor-transport-plugin>
  tor-transport-plugin make-tor-transport-plugin
  tor-transport-plugin?
  (role           tor-plugin-role
		  (default 'client)
		  (sanitize (lambda (value)
			      (if (memq value '(client server))
				  value
				  (configuration-field-error #f 'role value)))))
  (protocol       tor-plugin-protocol
		  (default "obfs4"))
  (program        tor-plugin-program))

(define (tor-configuration->torrc config)
  "Return a 'torrc' file for CONFIG."
  (match-record config <tor-configuration>
    (tor config-file hidden-services socks-socket-type control-socket?
         transport-plugins)
    (computed-file
     "torrc"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils)
                        (ice-9 match))

           (call-with-output-file #$output
             (lambda (port)
               (display "\
### These lines were generated from your system configuration:
DataDirectory /var/lib/tor
Log notice stderr\n" port)
               (when (eq? 'unix '#$socks-socket-type)
                 (display "\
SocksPort unix:/var/run/tor/socks-sock
UnixSocksGroupWritable 1\n" port))
               (when #$control-socket?
                 (display "\
ControlSocket unix:/var/run/tor/control-sock GroupWritable RelaxDirModeCheck
ControlSocketsGroupWritable 1\n"
                          port))

               (for-each (match-lambda
                           ((service (ports hosts) ...)
                            (format port "\
HiddenServiceDir /var/lib/tor/hidden-services/~a~%"
                                    service)
                            (for-each (lambda (tcp-port host)
                                        (format port "\
HiddenServicePort ~a ~a~%"
                                                tcp-port host))
                                      ports hosts)))
                         '#$(map (match-lambda
                                   (($ <tor-onion-service-configuration> name mapping)
                                    (cons name mapping)))
                                 hidden-services))

               (for-each (match-lambda
                           ((role-string protocol program)
                            (format port "\
~aTransportPlugin ~a exec ~a~%"
                                    role-string protocol program)))
                         '#$(map (match-lambda
                                   (($ <tor-transport-plugin> role protocol program)
                                    (list (if (eq? role 'client)
                                              "Client"
                                              "Server")
                                          protocol
                                          program)))
                                 transport-plugins))

               (display "\
### End of automatically generated lines.\n\n" port)

               ;; Append the user's config file.
               (call-with-input-file #$config-file
                 (lambda (input)
                   (dump-port input port)))
               #t)))))))

(define (tor-shepherd-service config)
  "Return a <shepherd-service> running Tor."
  (let* ((torrc (tor-configuration->torrc config))
         (transport-plugins (tor-configuration-transport-plugins config))
         (tor   (least-authority-wrapper
                 (file-append (tor-configuration-tor config) "/bin/tor")
                 #:name "tor"
                 #:mappings (append
                             (list (file-system-mapping
                                    (source "/var/lib/tor")
                                    (target source)
                                    (writable? #t))
                                   (file-system-mapping
                                    (source "/var/run/tor")
                                    (target source)
                                    (writable? #t))
                                   (file-system-mapping
                                    (source torrc)
                                    (target source)))
                             (map (lambda (plugin)
				    (file-system-mapping
				     (source (tor-plugin-program plugin))
				     (target source)))
				  transport-plugins))
                 #:namespaces (delq 'net %namespaces))))
    (list (shepherd-service
           (provision '(tor))

           ;; Tor needs at least one network interface to be up, hence the
           ;; dependency on 'loopback'.
           (requirement '(user-processes loopback syslogd))

           ;; XXX: #:pid-file won't work because the wrapped 'tor'
           ;; program would print its PID within the user namespace
           ;; instead of its actual PID outside.  There's no inetd or
           ;; systemd socket activation support either (there's
           ;; 'sd_notify' though), so we're stuck with that.
           (start #~(make-forkexec-constructor
                     (list #$tor "-f" #$torrc)
                     #:user "tor" #:group "tor"))
           (stop #~(make-kill-destructor))
           (actions (list (shepherd-configuration-action torrc)))
           (documentation "Run the Tor anonymous network overlay.")))))

(define (tor-activation config)
  "Set up directories for Tor and its hidden services, if any."
  #~(begin
      (use-modules (guix build utils))

      (define %user
        (getpw "tor"))

      (define (initialize service)
        (let ((directory (string-append "/var/lib/tor/hidden-services/"
                                        service)))
          (mkdir-p directory)
          (chown directory (passwd:uid %user) (passwd:gid %user))

          ;; The daemon bails out if we give wider permissions.
          (chmod directory #o700)))

      ;; Allow Tor to write its PID file.
      (mkdir-p "/var/run/tor")
      (chown "/var/run/tor" (passwd:uid %user) (passwd:gid %user))
      ;; Set the group permissions to rw so that if the system administrator
      ;; has specified UnixSocksGroupWritable=1 in their torrc file, members
      ;; of the "tor" group will be able to use the SOCKS socket.
      (chmod "/var/run/tor" #o750)

      ;; Allow Tor to access the hidden services' directories.
      (mkdir-p "/var/lib/tor")
      (chown "/var/lib/tor" (passwd:uid %user) (passwd:gid %user))
      (chmod "/var/lib/tor" #o700)

      ;; Make sure /var/lib is accessible to the 'tor' user.
      (chmod "/var/lib" #o755)

      (for-each initialize
                '#$(map tor-onion-service-configuration-name
                        (tor-configuration-hidden-services config)))))

(define tor-service-type
  (service-type (name 'tor)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          tor-shepherd-service)
                       (service-extension account-service-type
                                          (const %tor-accounts))
                       (service-extension activation-service-type
                                          tor-activation)))

                ;; This can be extended with Tor Onion Services.
                (compose concatenate)
                (extend (lambda (config services)
                          (tor-configuration
                           (inherit config)
                           (hidden-services
                            (append (tor-configuration-hidden-services config)
                                    services)))))
                (default-value (tor-configuration))
                (description
                 "Run the @uref{https://torproject.org, Tor} anonymous
networking daemon.")))

(define-deprecated (tor-hidden-service name mapping)
  #f
  "Define a new Tor @dfn{hidden service} called @var{name} and implementing
@var{mapping}.  @var{mapping} is a list of port/host tuples, such as:

@example
 '((22 . \"127.0.0.1:22\")
   (80 . \"127.0.0.1:8080\"))
@end example

In this example, port 22 of the hidden service is mapped to local port 22, and
port 80 is mapped to local port 8080.

This creates a @file{/var/lib/tor/hidden-services/@var{name}} directory, where
the @file{hostname} file contains the @code{.onion} host name for the hidden
service.

See @uref{https://www.torproject.org/docs/tor-hidden-service.html.en, the Tor
project's documentation} for more information."
  (simple-service 'tor-hidden-service
                  tor-service-type
                  (list (tor-onion-service-configuration
                         (name name)
                         (mapping mapping)))))


;;;
;;; ModemManager
;;;

(define-record-type* <modem-manager-configuration>
  modem-manager-configuration make-modem-manager-configuration
  modem-manager-configuration?
  (modem-manager modem-manager-configuration-modem-manager
                   (default modem-manager)))


;;;
;;; NetworkManager
;;;

;; TODO: deprecated field, remove later.
(define-with-syntax-properties (warn-iwd?-field-deprecation
                                (value properties))
  (when value
    (warning (source-properties->location properties)
             (G_ "the 'iwd?' field is deprecated, please use \
'shepherd-requirement' field instead~%")))
  value)

(define-record-type* <network-manager-configuration>
  network-manager-configuration make-network-manager-configuration
  network-manager-configuration?
  (network-manager network-manager-configuration-network-manager
                   (default network-manager))
  (shepherd-requirement network-manager-configuration-shepherd-requirement
                        (default '(wpa-supplicant)))
  (dns network-manager-configuration-dns
       (default "default"))
  (vpn-plugins network-manager-configuration-vpn-plugins ;list of file-like
               (default '()))
  (iwd? network-manager-configuration-iwd?  ; TODO: deprecated field, remove.
        (default #f)
        (sanitize warn-iwd?-field-deprecation))
  (extra-configuration-files
   network-manager-configuration-extra-configuration-files
   (default '())))                 ;'((file-name-string file-like-object) ...)

(define (network-manager-activation config)
  ;; Activation gexp for NetworkManager
  (match-record config <network-manager-configuration>
                (network-manager dns vpn-plugins extra-configuration-files)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p "/etc/NetworkManager/system-connections")
        #$@(if (equal? dns "dnsmasq")
               ;; create directory to store dnsmasq lease file
               '((mkdir-p "/var/lib/misc"))
               '())
        #$@(if (pair? extra-configuration-files)  ;if non-empty
               ;; If /etc/NetworkManager/conf.d is a symlink to a store file,
               ;; delete it.
               `((if (and (file-exists? "/etc/NetworkManager/conf.d")
                          (store-file-name?
                           (canonicalize-path "/etc/NetworkManager/conf.d")))
                     (delete-file-recursively
                      "/etc/NetworkManager/conf.d"))
                 ;; If it exists but is not a symlink to a store file, then
                 ;; this will fail with EEXIST; we leave this for the user to
                 ;; handle, since they probably created the directory
                 ;; themselves.
                 (symlink
                  ,(file-union "network-manager-configuration-directory"
                               extra-configuration-files)
                  "/etc/NetworkManager/conf.d"))
               '()))))

(define (vpn-plugin-directory plugins)
  "Return a directory containing PLUGINS, the NM VPN plugins."
  (directory-union "network-manager-vpn-plugins" plugins))

(define (network-manager-accounts config)
  "Return the list of <user-account> and <user-group> for CONFIG."
  (define nologin
    (file-append shadow "/sbin/nologin"))

  (define accounts
    (append-map (lambda (package)
                  (map (lambda (name)
                         (user-account (system? #t)
                                       (name name)
                                       (group "network-manager")
                                       (comment "NetworkManager helper")
                                       (home-directory "/var/empty")
                                       (create-home-directory? #f)
                                       (shell nologin)))
                       (or (assoc-ref (package-properties package)
                                      'user-accounts)
                           '())))
                (network-manager-configuration-vpn-plugins config)))

  (match accounts
    (()
     '())
    (_
     (cons (user-group (name "network-manager") (system? #t))
           accounts))))

(define (network-manager-environment config)
  (match-record config <network-manager-configuration>
    (network-manager dns vpn-plugins)
    ;; Define this variable in the global environment such that
    ;; "nmcli connection import type openvpn file foo.ovpn" works.
    `(("NM_VPN_PLUGIN_DIR"
       . ,(file-append (vpn-plugin-directory vpn-plugins)
                       "/lib/NetworkManager/VPN")))))

(define (network-manager-shepherd-service config)
  (match-record config <network-manager-configuration>
    (network-manager shepherd-requirement dns vpn-plugins iwd?)
    (let* ((iwd? (or iwd?  ; TODO: deprecated field, remove later.
                     (and shepherd-requirement
                          (memq 'iwd shepherd-requirement))))
           (conf (plain-file "NetworkManager.conf"
                             (string-append
                              "[main]\ndns=" dns "\n"
                              (if iwd? "[device]\nwifi.backend=iwd\n" ""))))
           (vpn  (vpn-plugin-directory vpn-plugins)))
      (list (shepherd-service
             (documentation "Run the NetworkManager.")
             (provision '(NetworkManager networking))
             (requirement `(user-processes dbus-system loopback
                            ,@shepherd-requirement
                            ;; TODO: iwd? is deprecated and should be passed
                            ;; with shepherd-requirement, remove later.
                            ,@(if iwd? '(iwd) '())))
             (actions (list (shepherd-configuration-action conf)))
             (start
              #~(lambda _
                  (let ((pid
                         (fork+exec-command
                          (list #$(file-append network-manager
                                               "/sbin/NetworkManager")
                                (string-append "--config=" #$conf)
                                "--no-daemon")
                          #:environment-variables
                          (list (string-append "NM_VPN_PLUGIN_DIR=" #$vpn
                                               "/lib/NetworkManager/VPN")
                                ;; Override non-existent default users
                                "NM_OPENVPN_USER="
                                "NM_OPENVPN_GROUP="
                                ;; Allow NetworkManager to find the modules.
                                (string-append
                                 "LINUX_MODULE_DIRECTORY="
                                 "/run/booted-system/kernel/lib/modules")))))
                    ;; XXX: Despite the "online" name, this doesn't guarantee
                    ;; WAN connectivity, it merely waits for NetworkManager
                    ;; to finish starting-up. This is required otherwise
                    ;; services will fail since the network interfaces be
                    ;; absent until NetworkManager finishes setting them up.
                    (system* #$(file-append network-manager "/bin/nm-online")
                             "--wait-for-startup" "--quiet")
                    ;; XXX: Finally, return the pid from running
                    ;; fork+exec-command to shepherd.
                    pid)))
             (stop #~(make-kill-destructor)))))))

(define network-manager-service-type
  (let ((config->packages
         (lambda (config)
          (match-record config <network-manager-configuration>
            (network-manager vpn-plugins)
            `(,network-manager ,@vpn-plugins)))))

    (service-type
     (name 'network-manager)
     (extensions
      (list (service-extension shepherd-root-service-type
                               network-manager-shepherd-service)
            (service-extension dbus-root-service-type config->packages)
            (service-extension polkit-service-type
                               (compose
                                list
                                network-manager-configuration-network-manager))
            (service-extension account-service-type
                               network-manager-accounts)
            (service-extension activation-service-type
                               network-manager-activation)
            (service-extension session-environment-service-type
                               network-manager-environment)
            ;; Add network-manager to the system profile.
            (service-extension profile-service-type config->packages)))
     (default-value (network-manager-configuration))
     (description
      "Run @uref{https://wiki.gnome.org/Projects/NetworkManager,
NetworkManager}, a network management daemon that aims to simplify wired and
wireless networking."))))


;;;
;;; Connman
;;;

(define (connman-general-configuration-field-name field-name)
  (let* ((str->camel (lambda (s)
                       (string-concatenate
                        (map string-capitalize (string-split s #\-)))))
         (str (if (symbol? field-name)
                  (str->camel (symbol->string field-name))
                  field-name)))
    (cond
     ((string-suffix? "?" str) (connman-general-configuration-field-name
                                (string-drop-right str 1)))
     ((string-contains str "RegulatoryDomain") (connman-general-configuration-field-name
                                                (string-replace-substring str "RegulatoryDomain" "Regdom")))
     ((string-contains str "Url") (connman-general-configuration-field-name
                                   (string-replace-substring str "Url" "URL")))
     ((string-contains str "Ip") (connman-general-configuration-field-name
                                  (string-replace-substring str "Ip" "IP")))
     ((string-contains str "6To4") (connman-general-configuration-field-name
                                    (string-replace-substring str "6To4" "6to4")))
     (#t str))))

(define (connman-general-configuration-serialize-string field-name value)
  (let ((param (connman-general-configuration-field-name field-name)))
    #~(string-append #$param " = " #$value "\n")))

(define (connman-general-configuration-serialize-number field-name value)
  (connman-general-configuration-serialize-string
   field-name (number->string value)))

(define (connman-general-configuration-serialize-list field-name value)
  (connman-general-configuration-serialize-string
   field-name (string-join value ",")))

(define (connman-general-configuration-serialize-boolean field-name value)
  (connman-general-configuration-serialize-string
   field-name (if value "true" "false")))

(define-maybe boolean (prefix connman-general-configuration-))
(define-maybe number (prefix connman-general-configuration-))
(define-maybe string (prefix connman-general-configuration-))
(define-maybe list (prefix connman-general-configuration-))

(define-configuration connman-general-configuration
  (input-request-timeout
   maybe-number
   "Set input request timeout.  Default is 120 seconds.  The request for inputs
like passphrase will timeout after certain amount of time.  Use this setting to
increase the value in case of different user interface designs.")
  (browser-launch-timeout
   maybe-number
   "Set browser launch timeout.  Default is 300 seconds.  The request for
launching a browser for portal pages will timeout after certain  amount  of
time.  Use this setting to increase the value in case of different user
interface designs.")
  (background-scanning?
   maybe-boolean
   "Enable background scanning.  Default is true.  If wifi is disconnected, the
background scanning will follow a simple back off mechanism from 3s up to 5
minutes.  Then, it will stay in 5 minutes unless user specifically asks for
scanning through a D-Bus call.  If so, the mechanism will start again from
3s.  This feature activates also the background scanning while being connected,
which is required for roaming on wifi.  When @code{background-scanning?} is false,
ConnMan will not perform any scan regardless of wifi is connected or not,
unless it is requested by the user through a D-Bus call.")
  (use-gateways-as-timeservers?
   maybe-boolean
   "Assume that service gateways also function as timeservers.  Default is false.")
  (fallback-timeservers
   maybe-list
   "List of Fallback timeservers.  These timeservers are used for NTP sync
when there are no timeservers set by the user or by the service, and when
@code{use-gateways-as-timeservers?} is @code{#f}.  These can contain a mixed
combination of fully qualified domain names, IPv4 and IPv6 addresses.")
  (fallback-nameservers
   maybe-list
   "List of fallback nameservers appended to the list of nameservers given
by the service.  The nameserver entries must be in numeric format,
host names are ignored.")
  (default-auto-connect-technologies
   maybe-list
   "List of technologies that are marked autoconnectable by default.  The
default value for this entry when empty is @code{\"ethernet\"}, @code{\"wifi\"},
@code{\"cellular\"}.  Services that are automatically connected must have been
set up and saved to storage beforehand.")
  (default-favourite-technologies
   maybe-list
   "List of technologies that are marked favorite by default.  The default
value for this entry when empty is @code{\"ethernet\"}.  Connects to services
from this technology even if not setup and saved to storage.")
  (always-connected-technologies
   maybe-list
   "List of technologies which are always connected regardless of
preferred-technologies setting (@code{auto-connect?} @code{#t}).  The default
value is empty and this feature is disabled unless explicitly enabled.")
  (preferred-technologies
   maybe-list
   "List of preferred technologies from the most preferred one to the least
preferred one.  Services of the listed technology type will be tried one by
one in the order given, until one of them gets connected or they are all
tried.  A service of a preferred technology type in state 'ready' will get
the default route when compared to another preferred type further down the
list with state 'ready' or with a non-preferred type; a service of a
preferred technology type in state 'online' will get the default route when
compared to either a non-preferred type or a preferred type further down
in the list.")
  (network-interface-blacklist
   maybe-list
   "List of blacklisted network interfaces.  Found interfaces will be
compared to the list and will not be handled by ConnMan, if their first
characters match any of the list entries.  Default value is @code{\"vmnet\"},
@code{\"vboxnet\"}, @code{\"virbr\"}, @code{\"ifb\"}.")
  (allow-hostname-updates?
   maybe-boolean
   "Allow ConnMan to change the system hostname.  This can happen for
example if we receive DHCP hostname option.  Default value is @code{#t}.")
  (allow-domainname-updates?
   maybe-boolean
   "Allow connman to change the system domainname.  This can happen for
example if we receive DHCP domainname option.  Default value is @code{#t}.")
  (single-connected-technology?
   maybe-boolean
   "Keep only a single connected technology at any time.  When a new
service is connected by the user or a better one is found according to
preferred-technologies, the new service is kept connected  and all the
other previously connected services are disconnected.  With this setting
it does not matter whether the previously connected services are
in 'online' or 'ready' states, the newly connected service is the only
one that will be kept connected.  A service connected by the user will
be used until going out of network coverage.  With this setting enabled
applications will notice more network breaks than normal.  Note this
options can't be used with VPNs.  Default value is @code{#f}.")
  (tethering-technologies
   maybe-list
   "List of technologies that are allowed to enable tethering.  The
default value is @code{\"wifi\"}, @code{\"bluetooth\"},
@code{\"gadget\"}.  Only those technologies listed here are used for
tethering.  If one wants to tether ethernet, then add @code{\"ethernet\"}
in the list.  Note that if ethernet tethering is enabled, then a DHCP
server is started on all ethernet interfaces.  Tethered ethernet should
never be connected to corporate or home network as it will disrupt normal
operation of these networks.  Due to this ethernet is not tethered by
default.  Do not activate ethernet tethering unless you really know
what you are doing.")
  (persistent-tethering-mode?
   maybe-boolean
   "Restore earlier tethering status when returning from offline mode,
re-enabling a technology, and after restarts and reboots.  Default
value is @code{#f}.")
  (enable-6to4?
   maybe-boolean
   "Automatically enable anycast 6to4 if possible.  This is not
recommended, as the use of 6to4 will generally lead to a severe
degradation of connection quality.  See RFC6343.  Default value
is @code{#f} (as recommended by RFC6343 section 4.1).")
  (vendor-class-id
   maybe-string
   "Set DHCP option 60 (Vendor Class ID) to the given string.  This
option can be used by DHCP servers to identify specific clients
without having to rely on MAC address ranges, etc.")
  (enable-online-check?
   maybe-boolean
   "Enable or disable use of HTTP GET as an online status check.  When
a service is in a READY state, and is selected as default, ConnMan will
issue an HTTP GET request to verify that end-to-end connectivity is
successful.  Only then the service will be transitioned to ONLINE
state.  If this setting is false, the default service will remain
in READY state.  Default value is @code{#t}.")
  (online-check-ipv4-url
   maybe-string
   "IPv4 URL used during the online status check.  Please refer to
the README for more detailed  information.  Default value is
@url{http://ipv4.connman.net/online/status.html}.")
  (online-check-ipv6-url
   maybe-string
   "IPv6 URL used during the online status check.  Please refer to
the README for more detailed  information.  Default value is
@url{http://ipv6.connman.net/online/status.html}.")
  (online-check-initial-interval
   maybe-number
   "Range of intervals between two online check requests.  Please
refer to the README for more detailed information.  Default value
is @samp{1}.")
  (online-check-max-interval
   maybe-number
   "Range of intervals between two online check requests.  Please
refer to the README for more detailed information.  Default value
is @samp{1}.")
  (enable-online-to-ready-transition?
   maybe-boolean
   "WARNING: This is an experimental feature.  In addition to
@code{enable-online-check} setting, enable or disable use of HTTP GET
to detect the loss of end-to-end connectivity.  If this setting is
@code{#f}, when the default service transitions to ONLINE state, the
HTTP GET request is no more called until next cycle, initiated by a
transition of the default service to DISCONNECT state.  If this
setting is @code{#t}, the HTTP GET request keeps being called to
guarantee that end-to-end connectivity is still successful.  If not,
the default service will transition to READY state, enabling another
service to become the default one, in replacement.  Default value
is @code{#f}.")
  (auto-connect-roaming-services?
   maybe-boolean
   "Automatically connect roaming services.  This is not recommended
unless you know you won't have any billing problem.  Default value
is @code{#f}.")
  (address-conflict-detection?
   maybe-boolean
   "Enable or disable the implementation of IPv4 address conflict
detection according to RFC5227.  ConnMan will send probe ARP packets
to see if an IPv4 address is already in use before assigning the
address to an interface.  If an address conflict occurs for a
statically configured address, an IPv4LL address will be chosen
instead (according to RFC3927).  If an address conflict occurs for
an address offered via DHCP, ConnMan sends a DHCP DECLINE once
and for the second conflict resorts to finding an IPv4LL
address.  Default value is @code{#f}.")
  (localtime
   maybe-string
   "Path to localtime file.  Defaults to @file{/etc/localtime}.")
  (regulatory-domain-follows-timezone?
   maybe-boolean
   "Enable regulatory domain to be changed along timezone changes.
With this option set to true each time the timezone changes the first
present ISO3166 country code is read from
@file{/usr/share/zoneinfo/zone1970.tab} and set as regulatory domain
value.  Default value is @code{#f}.")
  (resolv-conf
   maybe-string
   "Path to resolv.conf file.  If the file does not exist, but
intermediate directories exist, it will be created.  If this option
is not set, it tries to write into @file{/var/run/connman/resolv.conf}
if it fails (@file{/var/run/connman} does not exist or is not
writeable).  If you do not want to update resolv.conf, you can
set @file{/dev/null}.")
  (prefix connman-general-configuration-))

(define-record-type* <connman-configuration>
  connman-configuration make-connman-configuration
  connman-configuration?
  (connman      connman-configuration-connman
                (default connman))
  (shepherd-requirement connman-configuration-shepherd-requirement
                        (default '()))
  (disable-vpn? connman-configuration-disable-vpn?
                (default #f))
  (iwd?         connman-configuration-iwd?
                (default #f)
                (sanitize warn-iwd?-field-deprecation))
  (general-configuration connman-configuration-general-configuration
                         (default (connman-general-configuration))))

(define (connman-activation config)
  (let ((disable-vpn? (connman-configuration-disable-vpn? config)))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p "/var/lib/connman/")
          (unless #$disable-vpn?
            (mkdir-p "/var/lib/connman-vpn/"))))))

(define (connman-shepherd-service config)
  (match-record config <connman-configuration> (connman shepherd-requirement
                                                disable-vpn? iwd?
                                                general-configuration)
    (let ((iwd? (or iwd?  ; TODO: deprecated field, remove later.
                    (and shepherd-requirement
                         (memq 'iwd shepherd-requirement))))
          (config (mixed-text-file
                   "main.conf"
                   "[General]\n"
                   (serialize-configuration
                    general-configuration
                    connman-general-configuration-fields))))
      (list (shepherd-service
             (documentation "Run Connman")
             (provision '(connman networking))
             (requirement `(user-processes dbus-system loopback
                                           ,@shepherd-requirement
                                           ;; TODO: iwd? is deprecated and should be passed
                                           ;; with shepherd-requirement, remove later.
                                           ,@(if iwd? '(iwd) '())))
             (start #~(make-forkexec-constructor
                       (list (string-append #$connman
                                            "/sbin/connmand")
                             (string-append "--config=" #$config)
                             "--nodaemon"
                             "--nodnsproxy"
                             #$@(if disable-vpn? '("--noplugin=vpn") '())
                             #$@(if iwd? '("--wifi=iwd_agent") '()))

                       ;; As connman(8) notes, when passing '-n', connman
                       ;; "directs log output to the controlling terminal in
                       ;; addition to syslog."  Redirect stdout and stderr
                       ;; to avoid spamming the console (XXX: for some reason
                       ;; redirecting to /dev/null doesn't work.)
                       #:log-file "/var/log/connman.log"))
             (stop #~(make-kill-destructor)))))))

(define connman-service-type
  (let ((connman-package (compose list connman-configuration-connman)))
    (service-type (name 'connman)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            connman-shepherd-service)
                         (service-extension polkit-service-type
                                            connman-package)
                         (service-extension dbus-root-service-type
                                            connman-package)
                         (service-extension activation-service-type
                                            connman-activation)
                         ;; Add connman to the system profile.
                         (service-extension profile-service-type
                                            connman-package)))
                  (default-value (connman-configuration))
                  (description
                   "Run @url{https://01.org/connman,Connman},
a network connection manager."))))


;;;
;;; Modem manager
;;;

(define modem-manager-service-type
  (let ((config->package
         (lambda (config)
           (list (modem-manager-configuration-modem-manager config)))))
    (service-type (name 'modem-manager)
                  (extensions
                   (list (service-extension dbus-root-service-type
                                            config->package)
                         (service-extension udev-service-type
                                            config->package)
                         (service-extension polkit-service-type
                                            config->package)))
                  (default-value (modem-manager-configuration))
                  (description
                   "Run @uref{https://wiki.gnome.org/Projects/ModemManager,
ModemManager}, a modem management daemon that aims to simplify dialup
networking."))))


;;;
;;; USB_ModeSwitch
;;;

(define-record-type* <usb-modeswitch-configuration>
  usb-modeswitch-configuration make-usb-modeswitch-configuration
  usb-modeswitch-configuration?
  (usb-modeswitch      usb-modeswitch-configuration-usb-modeswitch
                       (default usb-modeswitch))
  (usb-modeswitch-data usb-modeswitch-configuration-usb-modeswitch-data
                       (default usb-modeswitch-data))
  (config-file         usb-modeswitch-configuration-config-file
                       (default #~(string-append #$usb-modeswitch:dispatcher
                                                 "/etc/usb_modeswitch.conf"))))

(define (usb-modeswitch-sh usb-modeswitch config-file)
  "Build a copy of usb_modeswitch.sh located in package USB-MODESWITCH,
modified to pass the CONFIG-FILE in its calls to usb_modeswitch_dispatcher,
and wrap it to actually find the dispatcher in USB-MODESWITCH.  The script
will be run by USB_ModeSwitch’s udev rules file when a modeswitchable USB
device is detected."
  (computed-file
   "usb_modeswitch-sh"
   (with-imported-modules '((guix build utils))
     #~(begin
         (use-modules (guix build utils))
         (let ((cfg-param
                #$(if config-file
                      #~(string-append " --config-file=" #$config-file)
                      "")))
           (mkdir #$output)
           (install-file (string-append #$usb-modeswitch:dispatcher
                                        "/lib/udev/usb_modeswitch")
                         #$output)

           ;; insert CFG-PARAM into usb_modeswitch_dispatcher command-lines
           (substitute* (string-append #$output "/usb_modeswitch")
             (("(exec usb_modeswitch_dispatcher .*)( 2>>)" _ left right)
              (string-append left cfg-param right))
             (("(exec usb_modeswitch_dispatcher .*)( &)" _ left right)
              (string-append left cfg-param right)))

           ;; wrap-program needs bash in PATH:
           (putenv (string-append "PATH=" #$bash "/bin"))
           (wrap-program (string-append #$output "/usb_modeswitch")
             `("PATH" ":" = (,(string-append #$coreutils "/bin")
                             ,(string-append
                               #$usb-modeswitch:dispatcher
                               "/bin")))))))))

(define (usb-modeswitch-configuration->udev-rules config)
  "Build a rules file for extending udev-service-type from the rules in the
usb-modeswitch package specified in CONFIG.  The rules file will invoke
usb_modeswitch.sh from the usb-modeswitch package, modified to pass the right
config file."
  (match-record config <usb-modeswitch-configuration>
    (usb-modeswitch usb-modeswitch-data config-file)
    (computed-file
     "usb_modeswitch.rules"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))
           (let ((in (string-append #$usb-modeswitch-data
                                    "/udev/40-usb_modeswitch.rules"))
                 (out (string-append #$output "/lib/udev/rules.d"))
                 (script #$(usb-modeswitch-sh usb-modeswitch config-file)))
             (mkdir-p out)
             (chdir out)
             (install-file in out)
             (substitute* "40-usb_modeswitch.rules"
               (("PROGRAM=\"usb_modeswitch")
                (string-append "PROGRAM=\"" script "/usb_modeswitch"))
               (("RUN\\+=\"usb_modeswitch")
                (string-append "RUN+=\"" script "/usb_modeswitch")))))))))

(define usb-modeswitch-service-type
  (service-type
   (name 'usb-modeswitch)
   (extensions
    (list
     (service-extension
      udev-service-type
      (lambda (config)
        (let ((rules (usb-modeswitch-configuration->udev-rules config)))
          (list rules))))))
   (default-value (usb-modeswitch-configuration))
   (description "Run @uref{http://www.draisberghof.de/usb_modeswitch/,
USB_ModeSwitch}, a mode switching tool for controlling USB devices with
multiple @dfn{modes}.  When plugged in for the first time many USB
devices (primarily high-speed WAN modems) act like a flash storage containing
installers for Windows drivers.  USB_ModeSwitch replays the sequence the
Windows drivers would send to switch their mode from storage to modem (or
whatever the thing is supposed to do).")))


;;;
;;; WPA supplicant
;;;

(define-record-type* <wpa-supplicant-configuration>
  wpa-supplicant-configuration make-wpa-supplicant-configuration
  wpa-supplicant-configuration?
  (wpa-supplicant     wpa-supplicant-configuration-wpa-supplicant ;file-like
                      (default wpa-supplicant))
  (requirement        wpa-supplicant-configuration-requirement    ;list of symbols
                      (default '(user-processes loopback syslogd)))
  (pid-file           wpa-supplicant-configuration-pid-file       ;string
                      (default "/var/run/wpa_supplicant.pid"))
  (dbus?              wpa-supplicant-configuration-dbus?          ;Boolean
                      (default #t))
  (interface          wpa-supplicant-configuration-interface      ;#f | string
                      (default #f))
  (config-file        wpa-supplicant-configuration-config-file    ;#f | <file-like>
                      (default #f))
  (extra-options      wpa-supplicant-configuration-extra-options  ;list of strings
                      (default '())))

(define (wpa-supplicant-shepherd-service config)
  (match-record config <wpa-supplicant-configuration>
    (wpa-supplicant requirement pid-file dbus?
                    interface config-file extra-options)
    (list (shepherd-service
           (documentation "Run the WPA supplicant daemon")
           (provision '(wpa-supplicant))
           (requirement (if dbus?
                            (cons 'dbus-system requirement)
                            requirement))
           (start #~(make-forkexec-constructor
                     (list (string-append #$wpa-supplicant
                                          "/sbin/wpa_supplicant")
                           (string-append "-P" #$pid-file)
                           "-B"                   ;run in background
                           "-s"                   ;log to syslogd
                           #$@(if dbus?
                                  #~("-u")
                                  #~())
                           #$@(if interface
                                  #~((string-append "-i" #$interface))
                                  #~())
                           #$@(if config-file
                                  #~((string-append "-c" #$config-file))
                                  #~())
                           #$@extra-options)
                     #:pid-file #$pid-file))
           (stop #~(make-kill-destructor))))))

(define wpa-supplicant-service-type
  (let ((config->package
         (lambda (config)
           (list (wpa-supplicant-configuration-wpa-supplicant config)))))
    (service-type (name 'wpa-supplicant)
                  (extensions
                   (list (service-extension shepherd-root-service-type
                                            wpa-supplicant-shepherd-service)
                         (service-extension dbus-root-service-type config->package)
                         (service-extension profile-service-type config->package)))
                  (description "Run the WPA Supplicant daemon, a service that
implements authentication, key negotiation and more for wireless networks.")
                  (default-value (wpa-supplicant-configuration)))))


;;;
;;; Hostapd.
;;;

(define-record-type* <hostapd-configuration>
  hostapd-configuration make-hostapd-configuration
  hostapd-configuration?
  (package           hostapd-configuration-package
                     (default hostapd))
  (interface         hostapd-configuration-interface ;string
                     (default "wlan0"))
  (ssid              hostapd-configuration-ssid)  ;string
  (broadcast-ssid?   hostapd-configuration-broadcast-ssid? ;Boolean
                     (default #t))
  (channel           hostapd-configuration-channel ;integer
                     (default 1))
  (driver            hostapd-configuration-driver ;string
                     (default "nl80211"))
  ;; See <https://w1.fi/cgit/hostap/plain/hostapd/hostapd.conf> for a list of
  ;; additional options we could add.
  (extra-settings    hostapd-configuration-extra-settings ;string
                     (default "")))

(define (hostapd-configuration-file config)
  "Return the configuration file for CONFIG, a <hostapd-configuration>."
  (match-record config <hostapd-configuration>
    (interface ssid broadcast-ssid? channel driver extra-settings)
    (plain-file "hostapd.conf"
                (string-append "\
# Generated from your Guix configuration.

interface=" interface "
ssid=" ssid "
ignore_broadcast_ssid=" (if broadcast-ssid? "0" "1") "
channel=" (number->string channel) "\n"
extra-settings "\n"))))

(define* (hostapd-shepherd-services config #:key (requirement '()))
  "Return Shepherd services for hostapd."
  (list (shepherd-service
         (provision '(hostapd))
         (requirement `(user-processes ,@requirement))
         (documentation "Run the hostapd WiFi access point daemon.")
         (start #~(make-forkexec-constructor
                   (list #$(file-append (hostapd-configuration-package config)
                                        "/sbin/hostapd")
                         #$(hostapd-configuration-file config))
                   #:log-file "/var/log/hostapd.log"))
         (stop #~(make-kill-destructor)))))

(define hostapd-service-type
  (service-type
   (name 'hostapd)
   (extensions
    (list (service-extension shepherd-root-service-type
                             hostapd-shepherd-services)))
   (description
    "Run the @uref{https://w1.fi/hostapd/, hostapd} daemon for Wi-Fi access
points and authentication servers.")))

(define (simulated-wifi-shepherd-services config)
  "Return Shepherd services to run hostapd with CONFIG, a
<hostapd-configuration>, as well as services to set up WiFi hardware
simulation."
  (append (hostapd-shepherd-services config
                                     #:requirement
                                     '(unblocked-wifi
                                       kernel-module-loader))
          (list (shepherd-service
                 (provision '(unblocked-wifi))
                 (requirement '(file-systems kernel-module-loader))
                 (documentation
                  "Unblock WiFi devices for use by mac80211_hwsim.")
                 (start #~(lambda _
                            (invoke #$(file-append util-linux "/sbin/rfkill")
                                    "unblock" "0")
                            (invoke #$(file-append util-linux "/sbin/rfkill")
                                    "unblock" "1")))
                 (one-shot? #t)))))

(define simulated-wifi-service-type
  (service-type
   (name 'simulated-wifi)
   (extensions
    (list (service-extension shepherd-root-service-type
                             simulated-wifi-shepherd-services)
          (service-extension kernel-module-loader-service-type
                             (const '("mac80211_hwsim")))))
   (default-value (hostapd-configuration
                   (interface "wlan1")
                   (ssid "Test Network")))
   (description "Run hostapd to simulate WiFi connectivity.")))


;;;
;;; Open vSwitch
;;;

(define-record-type* <openvswitch-configuration>
  openvswitch-configuration make-openvswitch-configuration
  openvswitch-configuration?
  (package openvswitch-configuration-package
           (default openvswitch)))

(define (openvswitch-activation config)
  (let ((ovsdb-tool (file-append (openvswitch-configuration-package config)
                                 "/bin/ovsdb-tool")))
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p "/var/run/openvswitch")
          (mkdir-p "/var/lib/openvswitch")
          (let ((conf.db "/var/lib/openvswitch/conf.db"))
            (unless (file-exists? conf.db)
              (system* #$ovsdb-tool "create" conf.db)))))))

(define (openvswitch-shepherd-service config)
  (let* ((package      (openvswitch-configuration-package config))
         (ovsdb-server (file-append package "/sbin/ovsdb-server"))
         (ovs-vswitchd (file-append package "/sbin/ovs-vswitchd")))
    (list (shepherd-service
           (provision '(ovsdb))
           (requirement '(user-processes))
           (documentation "Run the Open vSwitch database server.")
           (start #~(make-forkexec-constructor
                     (list #$ovsdb-server "--pidfile"
                           "--remote=punix:/var/run/openvswitch/db.sock")
                     #:pid-file "/var/run/openvswitch/ovsdb-server.pid"))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (provision '(vswitchd))
           (requirement '(user-processes ovsdb))
           (documentation "Run the Open vSwitch daemon.")
           (start #~(make-forkexec-constructor
                     (list #$ovs-vswitchd "--pidfile")
                     #:pid-file "/var/run/openvswitch/ovs-vswitchd.pid"))
           (stop #~(make-kill-destructor))))))

(define openvswitch-service-type
  (service-type
   (name 'openvswitch)
   (extensions
    (list (service-extension activation-service-type
                             openvswitch-activation)
          (service-extension profile-service-type
                             (compose list openvswitch-configuration-package))
          (service-extension shepherd-root-service-type
                             openvswitch-shepherd-service)))
   (description
    "Run @uref{http://www.openvswitch.org, Open vSwitch}, a multilayer virtual
switch designed to enable massive network automation through programmatic
extension.")
   (default-value (openvswitch-configuration))))

;;;
;;; iptables
;;;

(define %iptables-accept-all-rules
  (plain-file "iptables-accept-all.rules"
              "*filter
:INPUT ACCEPT
:FORWARD ACCEPT
:OUTPUT ACCEPT
COMMIT
"))

(define-record-type* <iptables-configuration>
  iptables-configuration make-iptables-configuration iptables-configuration?
  (iptables iptables-configuration-iptables
            (default iptables))
  (ipv4-rules iptables-configuration-ipv4-rules
              (default %iptables-accept-all-rules))
  (ipv6-rules iptables-configuration-ipv6-rules
              (default %iptables-accept-all-rules)))

(define (iptables-shepherd-service config)
  (match-record config <iptables-configuration>
    (iptables ipv4-rules ipv6-rules)
    (let ((iptables-restore (file-append iptables "/sbin/iptables-restore"))
          (ip6tables-restore (file-append iptables "/sbin/ip6tables-restore")))
      (shepherd-service
       (documentation "Packet filtering framework")
       (provision '(iptables))
       (start #~(lambda _
                  (invoke #$iptables-restore #$ipv4-rules)
                  (invoke #$ip6tables-restore #$ipv6-rules)))
       (stop #~(lambda _
                 (invoke #$iptables-restore #$%iptables-accept-all-rules)
                 (invoke #$ip6tables-restore #$%iptables-accept-all-rules)))))))

(define iptables-service-type
  (service-type
   (name 'iptables)
   (default-value (iptables-configuration))
   (description
    "Run @command{iptables-restore}, setting up the specified rules.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list iptables-shepherd-service))))))

;;;
;;; nftables.
;;;

(define %default-nftables-ruleset
  (plain-file "nftables.conf" "\
# A simple and safe firewall
table inet filter {
  chain input {
    type filter hook input priority 0; policy drop;

    # early drop of invalid connections
    ct state invalid drop

    # allow established/related connections
    ct state { established, related } accept

    # allow from loopback
    iif lo accept
    # drop connections to lo not coming from lo
    iif != lo ip daddr 127.0.0.1/8 drop
    iif != lo ip6 daddr ::1/128 drop

    # allow icmp
    ip protocol icmp accept
    ip6 nexthdr icmpv6 accept

    # allow ssh
    tcp dport ssh accept

    # reject everything else
    reject with icmpx type port-unreachable
  }
  chain forward {
    type filter hook forward priority 0; policy drop;
  }
  chain output {
    type filter hook output priority 0; policy accept;
  }
}
"))

(define (debug-level? x)
  (member x '(scanner parser eval netlink mnl proto-ctx segtree all)))

(define list-of-debug-levels?
  (list-of debug-level?))

(define-maybe/no-serialization list-of-debug-levels)

(define-configuration/no-serialization nftables-configuration
  (package
    (file-like nftables)
    "The @code{nftables} package to use.")
  (debug-levels
   maybe-list-of-debug-levels
   "A list of debug levels, for enabling debugging output.  Valid debug level values
are the @samp{scanner}, @samp{parser}, @samp{eval}, @samp{netlink},
@samp{mnl}, @samp{proto-ctx}, @samp{segtree} or @samp{all} symbols.")
  (ruleset
   (file-like %default-nftables-ruleset)
   "A file-like object containing the complete nftables ruleset.  The default
ruleset rejects all incoming connections except those to TCP port 22, with
connections from the loopback interface are allowed."))

(define (nftables-shepherd-service config)
  (match-record config <nftables-configuration>
                (package debug-levels ruleset)
    (let ((nft (file-append package "/sbin/nft")))
      (shepherd-service
       (documentation "Packet filtering and classification")
       (actions (list (shepherd-configuration-action ruleset)))
       (provision '(nftables))
       (start #~(lambda _
                  (invoke #$nft
                          #$@(if (maybe-value-set? debug-levels)
                                 (list (format #f "--debug=~{~a~^,~}"
                                               debug-levels))
                                 #~())
                          "--file" #$ruleset)))
       (stop #~(lambda _
                 (invoke #$nft "flush" "ruleset")))))))

(define nftables-service-type
  (service-type
   (name 'nftables)
   (description
    "Run @command{nft}, setting up the specified ruleset.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list nftables-shepherd-service))
          (service-extension profile-service-type
                             (compose list nftables-configuration-package))))
   (default-value (nftables-configuration))))


;;;
;;; PageKite
;;;

(define-record-type* <pagekite-configuration>
  pagekite-configuration
  make-pagekite-configuration
  pagekite-configuration?
  (package pagekite-configuration-package
           (default pagekite))
  (kitename pagekite-configuration-kitename
            (default #f))
  (kitesecret pagekite-configuration-kitesecret
              (default #f))
  (frontend pagekite-configuration-frontend
            (default #f))
  (kites pagekite-configuration-kites
         (default '("http:@kitename:localhost:80:@kitesecret")))
  (extra-file pagekite-configuration-extra-file
              (default #f)))

(define (pagekite-configuration-file config)
  (match-record config <pagekite-configuration>
    (package kitename kitesecret frontend kites extra-file)
    (mixed-text-file "pagekite.rc"
                     (if extra-file
                         (string-append "optfile = " extra-file "\n")
                         "")
                     (if kitename
                         (string-append "kitename = " kitename "\n")
                         "")
                     (if kitesecret
                         (string-append "kitesecret = " kitesecret "\n")
                         "")
                     (if frontend
                         (string-append "frontend = " frontend "\n")
                         "defaults\n")
                     (string-join (map (lambda (kite)
                                         (string-append "service_on = " kite))
                                       kites)
                                  "\n"
                                  'suffix))))

(define (pagekite-shepherd-service config)
  (match-record config <pagekite-configuration>
    (package kitename kitesecret frontend kites extra-file)
    (let* ((config-file (pagekite-configuration-file config))
           (mappings (cons (file-system-mapping
                            (source config-file)
                            (target source))
                           (if extra-file
                               (list (file-system-mapping
                                      (source extra-file)
                                      (target source)))
                               '())))
           (pagekite (least-authority-wrapper
                      (file-append package "/bin/pagekite")
                      #:name "pagekite"
                      #:mappings mappings
                      ;; 'pagekite' changes user IDs to it needs to run in the
                      ;; global user namespace.
                      #:namespaces (fold delq %namespaces '(net user)))))
      (shepherd-service
       (documentation "Run the PageKite service.")
       (provision '(pagekite))
       (requirement '(user-processes networking))
       (actions (list (shepherd-configuration-action config-file)))
       (start #~(make-forkexec-constructor
                 (list #$pagekite
                       "--clean"
                       "--nullui"
                       "--nocrashreport"
                       "--runas=pagekite:pagekite"
                       (string-append "--optfile=" #$config-file))
                 #:log-file "/var/log/pagekite.log"))
       ;; SIGTERM doesn't always work for some reason.
       (stop #~(make-kill-destructor SIGINT))))))

(define %pagekite-accounts
  (list (user-group (name "pagekite") (system? #t))
        (user-account
         (name "pagekite")
         (group "pagekite")
         (system? #t)
         (comment "PageKite user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define pagekite-service-type
  (service-type
   (name 'pagekite)
   (default-value (pagekite-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list pagekite-shepherd-service))
          (service-extension account-service-type
                             (const %pagekite-accounts))))
   (description
    "Run @url{https://pagekite.net/,PageKite}, a tunneling solution to make
local servers publicly accessible on the web, even behind NATs and firewalls.")))


;;;
;;; Yggdrasil
;;;

(define-record-type* <yggdrasil-configuration>
  yggdrasil-configuration
  make-yggdrasil-configuration
  yggdrasil-configuration?
  (package yggdrasil-configuration-package
           (default yggdrasil))
  (json-config yggdrasil-configuration-json-config
               (default '()))
  (config-file yggdrasil-config-file
               (default "/etc/yggdrasil-private.conf"))
  (autoconf? yggdrasil-configuration-autoconf?
             (default #f))
  (log-level yggdrasil-configuration-log-level
             (default 'info))
  (log-to yggdrasil-configuration-log-to
          (default 'stdout)))

(define (yggdrasil-configuration-file config)
  (define (scm->yggdrasil-json x)
    (define key-value?
      dotted-list?)
    (define (param->camel str)
      (string-concatenate
       (map
	string-capitalize
	(string-split str (cut eqv? <> #\-)))))
    (cond
     ((key-value? x)
      (let ((k (car x))
	    (v (cdr x)))
	(cons
	 (if (symbol? k)
	     (param->camel (symbol->string k))
	     k)
	 v)))
     ((list? x) (map scm->yggdrasil-json x))
     ((vector? x) (vector-map scm->yggdrasil-json x))
     (else x)))
  (computed-file
   "yggdrasil.conf"
   #~(call-with-output-file #$output
       (lambda (port)
         ;; it's HJSON, so comments are a-okay
         (display "# Generated by yggdrasil-service\n" port)
         (display #$(scm->json-string
                     (scm->yggdrasil-json
                      (yggdrasil-configuration-json-config config)))
                  port)))))

(define (yggdrasil-shepherd-service config)
  "Return a <shepherd-service> for yggdrasil with CONFIG."
  (define yggdrasil-command
    #~(append
       (list (string-append
              #$(yggdrasil-configuration-package config)
              "/bin/yggdrasil")
             "-useconffile"
             #$(yggdrasil-configuration-file config))
       (if #$(yggdrasil-configuration-autoconf? config)
           '("-autoconf")
           '())
       (let ((extraconf #$(yggdrasil-config-file config)))
         (if extraconf
             (list "-extraconffile" extraconf)
             '()))
       (list "-loglevel"
             #$(symbol->string
		(yggdrasil-configuration-log-level config))
             "-logto"
             #$(symbol->string
		(yggdrasil-configuration-log-to config)))))
  (list (shepherd-service
         (documentation "Connect to the Yggdrasil mesh network")
         (provision '(yggdrasil))
         (requirement '(networking))
         (start #~(make-forkexec-constructor
                   #$yggdrasil-command
                   #:log-file "/var/log/yggdrasil.log"
                   #:group "yggdrasil"))
         (stop #~(make-kill-destructor)))))

(define %yggdrasil-accounts
  (list (user-group (name "yggdrasil") (system? #t))))

(define yggdrasil-service-type
  (service-type
   (name 'yggdrasil)
   (description
    "Connect to the Yggdrasil mesh network.
See @command{yggdrasil -genconf} for config options.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             yggdrasil-shepherd-service)
          (service-extension account-service-type
                             (const %yggdrasil-accounts))
          (service-extension profile-service-type
                             (compose list yggdrasil-configuration-package))))))


;;;
;;; IPFS
;;;

(define-record-type* <ipfs-configuration>
  ipfs-configuration
  make-ipfs-configuration
  ipfs-configuration?
  (package ipfs-configuration-package
           (default go-ipfs))
  (gateway ipfs-configuration-gateway
           (default "/ip4/127.0.0.1/tcp/8082"))
  (api     ipfs-configuration-api
           (default "/ip4/127.0.0.1/tcp/5001")))

(define %ipfs-home "/var/lib/ipfs")

(define %ipfs-accounts
  (list (user-account
         (name "ipfs")
         (group "ipfs")
         (system? #t)
         (comment "IPFS daemon user")
         (home-directory "/var/lib/ipfs")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "ipfs")
         (system? #t))))

(define (ipfs-binary config)
  (define command
    (file-append (ipfs-configuration-package config) "/bin/ipfs"))

  (least-authority-wrapper
   command
   #:name "ipfs"
   #:mappings (list %ipfs-home-mapping)
   #:namespaces (delq 'net %namespaces)))

(define %ipfs-home-mapping
  (file-system-mapping
   (source %ipfs-home)
   (target %ipfs-home)
   (writable? #t)))

(define %ipfs-environment
  #~(list #$(string-append "HOME=" %ipfs-home)))

(define (ipfs-shepherd-service config)
  "Return a <shepherd-service> for IPFS with CONFIG."
  (define ipfs-daemon-command
    #~(list #$(ipfs-binary config) "daemon"))

  (list (shepherd-service
         (provision '(ipfs))
         ;; While IPFS is most useful when the machine is connected
         ;; to the network, only loopback is required for starting
         ;; the service.
         (requirement '(user-processes loopback))
         (documentation "Connect to the IPFS network")
         (start #~(make-forkexec-constructor
                   #$ipfs-daemon-command
                   #:log-file "/var/log/ipfs.log"
                   #:user "ipfs" #:group "ipfs"
                   #:environment-variables #$%ipfs-environment))
         (stop #~(make-kill-destructor)))))

(define (%ipfs-activation config)
  "Return an activation gexp for IPFS with CONFIG"
  (define (exec-command . args)
    ;; Exec the given ifps command with the right authority.
    #~(let ((pid (primitive-fork)))
        (if (zero? pid)
            (dynamic-wind
              (const #t)
              (lambda ()
                ;; Run ipfs init and ipfs config from a container,
                ;; in case the IPFS daemon was compromised at some point
                ;; and ~/.ipfs is now a symlink to somewhere outside
                ;; %ipfs-home.
                (let ((pw (getpwnam "ipfs")))
                  (setgroups '#())
                  (setgid (passwd:gid pw))
                  (setuid (passwd:uid pw))
                  (environ #$%ipfs-environment)
                  (execl #$(ipfs-binary config) #$@args)))
              (lambda ()
                (primitive-exit 127)))
            (waitpid pid))))

  (define settings
    `(("Addresses.API" ,(ipfs-configuration-api config))
      ("Addresses.Gateway" ,(ipfs-configuration-gateway config))))

  (define inner-gexp
    #~(begin
        (umask #o077)
        ;; Create $HOME/.ipfs structure
        #$(exec-command "ipfs" "init")
        ;; Apply settings
        #$@(map (match-lambda
                  ((setting value)
                   (exec-command "ipfs" "config" setting value)))
                settings)))

  (define inner-script
    (program-file "ipfs-activation-inner" inner-gexp))

  ;; The activation may happen from the initrd, which uses
  ;; a statically-linked guile, while the guix container
  ;; procedures require a working dynamic-link.
  #~(system* #$inner-script))

(define ipfs-service-type
  (service-type
   (name 'ipfs)
   (extensions
    (list (service-extension account-service-type
                             (const %ipfs-accounts))
          (service-extension activation-service-type
                             %ipfs-activation)
          (service-extension shepherd-root-service-type
                             ipfs-shepherd-service)))
   (default-value (ipfs-configuration))
   (description
    "Run @command{ipfs daemon}, the reference implementation
of the IPFS peer-to-peer storage network.")))


;;;
;;; Keepalived
;;;

(define-record-type* <keepalived-configuration>
  keepalived-configuration make-keepalived-configuration
  keepalived-configuration?
  (keepalived  keepalived-configuration-keepalived  ;file-like
               (default keepalived))
  (config-file keepalived-configuration-config-file ;file-like
               (default #f)))

(define (keepalived-shepherd-service config)
  (match-record config <keepalived-configuration>
    (keepalived config-file)
    (list (shepherd-service
           (provision '(keepalived))
           (documentation "Run keepalived.")
           (requirement '(loopback))
           (start #~(make-forkexec-constructor
                     (list (string-append #$keepalived "/sbin/keepalived")
                           "--dont-fork" "--log-console" "--log-detail"
                           "--pid=/var/run/keepalived.pid"
                           (string-append "--use-file=" #$config-file))
                     #:pid-file "/var/run/keepalived.pid"
                     #:log-file "/var/log/keepalived.log"))
           (respawn? #f)
           (stop #~(make-kill-destructor))))))

(define keepalived-service-type
  (service-type (name 'keepalived)
                (extensions (list (service-extension shepherd-root-service-type
                                                     keepalived-shepherd-service)))
                (description
                 "Run @uref{https://www.keepalived.org/, Keepalived}
routing software.")))

;;; networking.scm ends here
