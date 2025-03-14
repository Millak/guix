;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Ryan Moe <ryan.moe@gmail.com>
;;; Copyright © 2018, 2020-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020, 2021, 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Timotej Lazar <timotej.lazar@araneo.si>
;;; Copyright © 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022 Leo Nikkilä <hello@lnikki.la>
;;; Copyright © 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
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

(define-module (gnu services virtualization)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu image)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages gdb)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages ssh)
  #:use-module (gnu packages base)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages virtualization)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system hurd)
  #:use-module (gnu system image)
  #:use-module (gnu system shadow)
  #:autoload   (gnu system vm) (linux-image-startup-command
                                virtualized-operating-system)
  #:autoload   (gnu system locale) (locale-definition)
  #:use-module (gnu system)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix store)
  #:use-module (guix utils)
  #:autoload   (guix self) (make-config.scm)
  #:autoload   (guix platform) (platform-system)

  #:use-module ((srfi srfi-1) #:hide (partition))
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 match)

  #:export (virtual-build-machine
            virtual-build-machine-service-type

            %virtual-build-machine-operating-system
            %virtual-build-machine-default-vm

            %hurd-vm-operating-system
            hurd-vm-configuration
            hurd-vm-configuration?
            hurd-vm-configuration-os
            hurd-vm-configuration-qemu
            hurd-vm-configuration-image
            hurd-vm-configuration-disk-size
            hurd-vm-configuration-memory-size
            hurd-vm-configuration-options
            hurd-vm-configuration-id
            hurd-vm-configuration-net-options
            hurd-vm-configuration-secrets

            hurd-vm-disk-image
            hurd-vm-port
            hurd-vm-net-options
            hurd-vm-service-type

            libvirt-configuration
            libvirt-service-type
            virtlog-configuration
            virtlog-service-type

            %qemu-platforms
            lookup-qemu-platforms
            qemu-platform?
            qemu-platform-name

            qemu-binfmt-configuration
            qemu-binfmt-configuration?
            qemu-binfmt-service-type

            qemu-guest-agent-configuration
            qemu-guest-agent-configuration?
            qemu-guest-agent-service-type

            xe-guest-utilities-configuration
            xe-guest-utilities-service-type
            xen-guest-agent-configuration
            xen-guest-agent-service-type))

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join
     (string-split (string-delete #\? str) #\-)
     "_")))

(define (quote-val val)
  (string-append "\"" val "\""))

(define (serialize-field field-name val)
  (format #t "~a = ~a\n" (uglify-field-name field-name) val))

(define (serialize-string field-name val)
  (serialize-field field-name (quote-val val)))

(define (serialize-boolean field-name val)
  (serialize-field field-name (if val 1 0)))

(define (serialize-integer field-name val)
  (serialize-field field-name val))

(define (build-opt-list val)
  (string-append
   "["
   (string-join (map quote-val val) ",")
   "]"))

(define optional-list? list?)
(define optional-string? string?)

(define (serialize-list field-name val)
  (serialize-field field-name (build-opt-list val)))

(define (serialize-optional-list field-name val)
  (if (null? val)
      (format #t "# ~a = []\n" (uglify-field-name field-name))
      (serialize-list field-name val)))

(define (serialize-optional-string field-name val)
  (if (string-null? val)
      (format #t "# ~a = \"\"\n" (uglify-field-name field-name))
      (serialize-string field-name val)))

(define-configuration libvirt-configuration
  (libvirt
   (file-like libvirt)
   "Libvirt package.")
  (qemu
   (file-like qemu)
   "Qemu package.")

  (listen-tls?
   (boolean #t)
   "Flag listening for secure TLS connections on the public TCP/IP port.
must set @code{listen} for this to have any effect.

It is necessary to setup a CA and issue server certificates before
using this capability.")
  (listen-tcp?
   (boolean #f)
   "Listen for unencrypted TCP connections on the public TCP/IP port.
must set @code{listen} for this to have any effect.

Using the TCP socket requires SASL authentication by default. Only
SASL mechanisms which support data encryption are allowed. This is
DIGEST_MD5 and GSSAPI (Kerberos5)")
  (tls-port
   (string "16514")
   "Port for accepting secure TLS connections This can be a port number,
or service name")
  (tcp-port
   (string "16509")
   "Port for accepting insecure TCP connections This can be a port number,
or service name")
  (listen-addr
   (string "0.0.0.0")
   "IP address or hostname used for client connections.")
  (mdns-adv?
   (boolean #f)
   "Flag toggling mDNS advertisement of the libvirt service.

Alternatively can disable for all services on a host by
stopping the Avahi daemon.")
  (mdns-name
   (string (string-append "Virtualization Host " (gethostname)))
   "Default mDNS advertisement name. This must be unique on the
immediate broadcast network.")
  (unix-sock-group
   (string "libvirt")
   "UNIX domain socket group ownership. This can be used to
allow a 'trusted' set of users access to management capabilities
without becoming root.")
  (unix-sock-ro-perms
   (string "0777")
   "UNIX socket permissions for the R/O socket. This is used
for monitoring VM status only.")
  (unix-sock-rw-perms
   (string "0770")
   "UNIX socket permissions for the R/W socket. Default allows
only root. If PolicyKit is enabled on the socket, the default
will change to allow everyone (eg, 0777)")
  (unix-sock-admin-perms
   (string "0777")
   "UNIX socket permissions for the admin socket. Default allows
only owner (root), do not change it unless you are sure to whom
you are exposing the access to.")
  (unix-sock-dir
   (string "/var/run/libvirt")
   "The directory in which sockets will be found/created.")
  (auth-unix-ro
   (string "polkit")
   "Authentication scheme for UNIX read-only sockets. By default
socket permissions allow anyone to connect")
  (auth-unix-rw
   (string "polkit")
   "Authentication scheme for UNIX read-write sockets. By default
socket permissions only allow root. If PolicyKit support was compiled
into libvirt, the default will be to use 'polkit' auth.")
  (auth-tcp
   (string "sasl")
   "Authentication scheme for TCP sockets. If you don't enable SASL,
then all TCP traffic is cleartext. Don't do this outside of a dev/test
scenario.")
  (auth-tls
   (string "none")
   "Authentication scheme for TLS sockets. TLS sockets already have
encryption provided by the TLS layer, and limited authentication is
done by certificates.

It is possible to make use of any SASL authentication mechanism as
well, by using 'sasl' for this option")
  (access-drivers
   (optional-list '())
   "API access control scheme.

By default an authenticated user is allowed access to all APIs. Access
drivers can place restrictions on this.")
  (key-file
   (string "")
   "Server key file path. If set to an empty string, then no private key
is loaded.")
  (cert-file
   (string "")
   "Server key file path. If set to an empty string, then no certificate
is loaded.")
  (ca-file
   (string "")
   "Server key file path. If set to an empty string, then no CA certificate
is loaded.")
  (crl-file
   (string "")
   "Certificate revocation list path. If set to an empty string, then no
CRL is loaded.")
  (tls-no-sanity-cert
   (boolean #f)
   "Disable verification of our own server certificates.

When libvirtd starts it performs some sanity checks against its own
certificates.")
  (tls-no-verify-cert
   (boolean #f)
   "Disable verification of client certificates.

Client certificate verification is the primary authentication mechanism.
Any client which does not present a certificate signed by the CA
will be rejected.")
  (tls-allowed-dn-list
   (optional-list '())
   "Whitelist of allowed x509 Distinguished Name.")
  (sasl-allowed-usernames
   (optional-list '())
   "Whitelist of allowed SASL usernames. The format for username
depends on the SASL authentication mechanism.")
  (tls-priority
   (string "NORMAL")
   "Override the compile time default TLS priority string. The
default is usually \"NORMAL\" unless overridden at build time.
Only set this is it is desired for libvirt to deviate from
the global default settings.")
  (max-clients
   (integer 5000)
   "Maximum number of concurrent client connections to allow
over all sockets combined.")
  (max-queued-clients
   (integer 1000)
   "Maximum length of queue of connections waiting to be
accepted by the daemon. Note, that some protocols supporting
retransmission may obey this so that a later reattempt at
connection succeeds.")
  (max-anonymous-clients
   (integer 20)
   "Maximum length of queue of accepted but not yet authenticated
clients. Set this to zero to turn this feature off")
  (min-workers
   (integer 5)
   "Number of workers to start up initially.")
  (max-workers
   (integer 20)
   "Maximum number of worker threads.

If the number of active clients exceeds @code{min-workers},
then more threads are spawned, up to max_workers limit.
Typically you'd want max_workers to equal maximum number
of clients allowed.")
  (prio-workers
   (integer 5)
   "Number of priority workers. If all workers from above
pool are stuck, some calls marked as high priority
(notably domainDestroy) can be executed in this pool.")
  (max-requests
    (integer 20)
    "Total global limit on concurrent RPC calls.")
  (max-client-requests
    (integer 5)
    "Limit on concurrent requests from a single client
connection. To avoid one client monopolizing the server
this should be a small fraction of the global max_requests
and max_workers parameter.")
  (admin-min-workers
    (integer 1)
    "Same as @code{min-workers} but for the admin interface.")
  (admin-max-workers
     (integer 5)
    "Same as @code{max-workers} but for the admin interface.")
  (admin-max-clients
    (integer 5)
    "Same as @code{max-clients} but for the admin interface.")
  (admin-max-queued-clients
    (integer 5)
    "Same as @code{max-queued-clients} but for the admin interface.")
  (admin-max-client-requests
    (integer 5)
    "Same as @code{max-client-requests} but for the admin interface.")
  (log-level
    (integer 3)
    "Logging level. 4 errors, 3 warnings, 2 information, 1 debug.")
  (log-filters
    (string "3:remote 4:event")
    "Logging filters.

A filter allows selecting a different logging level for a given category
of logs
The format for a filter is one of:
@itemize
@item x:name

@item x:+name
@end itemize

where @code{name} is a string which is matched against the category
given in the @code{VIR_LOG_INIT()} at the top of each libvirt source
file, e.g., \"remote\", \"qemu\", or \"util.json\" (the name in the
filter can be a substring of the full category name, in order
to match multiple similar categories), the optional \"+\" prefix
tells libvirt to log stack trace for each message matching
name, and @code{x} is the minimal level where matching messages should
be logged:

@itemize
@item 1: DEBUG
@item 2: INFO
@item 3: WARNING
@item 4: ERROR
@end itemize

Multiple filters can be defined in a single filters statement, they just
need to be separated by spaces.")
  (log-outputs
    (string "3:syslog:libvirtd")
    "Logging outputs.

An output is one of the places to save logging information
The format for an output can be:

@table @code
@item x:stderr
output goes to stderr

@item x:syslog:name
use syslog for the output and use the given name as the ident

@item x:file:file_path
output to a file, with the given filepath

@item x:journald
output to journald logging system
@end table

In all case the x prefix is the minimal level, acting as a filter

@itemize
@item 1: DEBUG
@item 2: INFO
@item 3: WARNING
@item 4: ERROR
@end itemize

Multiple outputs can be defined, they just need to be separated by spaces.")
  (audit-level
    (integer 1)
    "Allows usage of the auditing subsystem to be altered

@itemize
@item 0: disable all auditing
@item 1: enable auditing, only if enabled on host
@item 2: enable auditing, and exit if disabled on host.
@end itemize
")
  (audit-logging
    (boolean #f)
    "Send audit messages via libvirt logging infrastructure.")
  (host-uuid
    (optional-string "")
    "Host UUID. UUID must not have all digits be the same.")
  (host-uuid-source
    (string "smbios")
    "Source to read host UUID.

@itemize

@item @code{smbios}: fetch the UUID from @code{dmidecode -s system-uuid}

@item @code{machine-id}: fetch the UUID from @code{/etc/machine-id}

@end itemize

If @code{dmidecode} does not provide a valid UUID a temporary UUID
will be generated.")
  (keepalive-interval
    (integer 5)
    "A keepalive message is sent to a client after
@code{keepalive_interval} seconds of inactivity to check if
the client is still responding. If set to -1, libvirtd will
never send keepalive requests; however clients can still send
them and the daemon will send responses.")
  (keepalive-count
    (integer 5)
    "Maximum number of keepalive messages that are allowed to be sent
to the client without getting any response before the connection is
considered broken.

In other words, the connection is automatically
closed approximately after
@code{keepalive_interval * (keepalive_count + 1)} seconds since the last
message received from the client. When @code{keepalive-count} is
set to 0, connections will be automatically closed after
@code{keepalive-interval} seconds of inactivity without sending any
keepalive messages.")
  (admin-keepalive-interval
    (integer 5)
    "Same as above but for admin interface.")
  (admin-keepalive-count
    (integer 5)
    "Same as above but for admin interface.")
  (ovs-timeout
    (integer 5)
    "Timeout for Open vSwitch calls.

The @code{ovs-vsctl} utility is used for the configuration and
its timeout option is set by default to 5 seconds to avoid
potential infinite waits blocking libvirt."))

(define* (libvirt-conf-file config)
  "Return a libvirtd config file."
  (plain-file "libvirtd.conf"
              (with-output-to-string
                (lambda ()
                  (serialize-configuration config libvirt-configuration-fields)))))

(define %libvirt-accounts
  (list (user-group (name "libvirt") (system? #t))))

(define (%libvirt-activation config)
  (let ((sock-dir (libvirt-configuration-unix-sock-dir config)))
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$sock-dir))))


(define (libvirt-shepherd-service config)
  (let* ((config-file (libvirt-conf-file config))
         (libvirt (libvirt-configuration-libvirt config))
         (listen-tcp? (libvirt-configuration-listen-tcp? config)))
    (list (shepherd-service
           (documentation "Run the libvirt daemon.")
           (provision '(libvirtd))
           (requirement '(dbus-system))
           (start #~(make-forkexec-constructor
                     (list (string-append #$libvirt "/sbin/libvirtd")
                           "-f" #$config-file
                           #$@(if listen-tcp? '("--listen") '()))
                     ;; For finding qemu, ip binaries and kernel modules.
                     #:environment-variables
                     (list
                      (string-append
                       "PATH=/run/current-system/profile/bin:"
                       "/run/current-system/profile/sbin")
                      "LINUX_MODULE_DIRECTORY="
                      "/run/booted-system/kernel/lib/modules")))
           (stop #~(make-kill-destructor))))))

(define libvirt-service-type
  (service-type (name 'libvirt)
		(extensions
                 (list
                  (service-extension polkit-service-type
                                     (compose list libvirt-configuration-libvirt))
                  (service-extension profile-service-type
                                     (lambda (config)
                                       (list
                                        (libvirt-configuration-libvirt config)
                                        (libvirt-configuration-qemu config))))
                  (service-extension activation-service-type
                                     %libvirt-activation)
                  (service-extension shepherd-root-service-type
                                     libvirt-shepherd-service)
                  (service-extension account-service-type
                                     (const %libvirt-accounts))))
                (default-value (libvirt-configuration))
                (description "Run @command{libvirtd}, a daemon of the libvirt
virtualization management system.  This daemon runs on host servers and
performs required management tasks for virtualized guests.")))


(define-record-type* <virtlog-configuration>
  virtlog-configuration make-virtlog-configuration
  virtlog-configuration?
  (libvirt      virtlog-configuration-libvirt
                (default libvirt))
  (log-level    virtlog-configuration-log-level
                (default 3))
  (log-filters  virtlog-configuration-log-filters
                (default "3:remote 4:event"))
  (log-outputs  virtlog-configuration-log-outputs
                (default "3:syslog:virtlogd"))
  (max-clients  virtlog-configuration-max-clients
                (default 1024))
  (max-size     virtlog-configuration-max-size
                (default 2097152)) ;; 2MB
  (max-backups  virtlog-configuration-max-backups
                (default 3)))

(define* (virtlogd-conf-file config)
  "Return a virtlogd config file."
  (plain-file "virtlogd.conf"
              (string-append
               "log_level = " (number->string (virtlog-configuration-log-level config)) "\n"
               "log_filters = \"" (virtlog-configuration-log-filters config) "\"\n"
               "log_outputs = \"" (virtlog-configuration-log-outputs config) "\"\n"
               "max_clients = " (number->string (virtlog-configuration-max-clients config)) "\n"
               "max_size = " (number->string (virtlog-configuration-max-size config)) "\n"
               "max_backups = " (number->string (virtlog-configuration-max-backups config)) "\n")))

(define (virtlogd-shepherd-service config)
  (let* ((config-file (virtlogd-conf-file config))
         (libvirt (virtlog-configuration-libvirt config)))
    (list (shepherd-service
           (documentation "Run the virtlog daemon.")
           (provision '(virtlogd))
           (requirement '(user-processes))
           (start #~(make-forkexec-constructor
                     (list (string-append #$libvirt "/sbin/virtlogd")
                           "-f" #$config-file)))
           (stop #~(make-kill-destructor))))))

(define virtlog-service-type
  (service-type (name 'virtlogd)
		(extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     virtlogd-shepherd-service)))
                (default-value (virtlog-configuration))
                (description "Run @command{virtlogd}, a daemon libvirt that is
used to manage logs from @acronym{VM, virtual machine} consoles.")))

(define (generate-libvirt-documentation)
  (generate-documentation
   `((libvirt-configuration ,libvirt-configuration-fields))
   'libvirt-configuration))


;;;
;;; Transparent QEMU emulation via binfmt_misc.
;;;

;; Platforms that QEMU can emulate.
(define-record-type* <qemu-platform>
  qemu-platform make-qemu-platform
  qemu-platform?
  (name     qemu-platform-name)                   ;string
  (family   qemu-platform-family)                 ;string
  (magic    qemu-platform-magic)                  ;bytevector
  (mask     qemu-platform-mask)                   ;bytevector

  ;; Default flags:
  ;;
  ;;   "F": fix binary.  Open the qemu-user binary (statically linked) as soon
  ;;   as binfmt_misc interpretation is handled.
  ;;
  ;;   "P": preserve argv[0].  QEMU 6.0 detects whether it's started with this
  ;;   flag and automatically does the right thing.  Without this flag,
  ;;   argv[0] is replaced by the absolute file name of the executable, an
  ;;   observable difference that can cause discrepancies.
  (flags    qemu-platform-flags (default "FP")))  ;string

(define-syntax bv
  (lambda (s)
    "Expand the given string into a bytevector."
    (syntax-case s ()
      ((_ str)
       (string? (syntax->datum #'str))
       (let ((bv (u8-list->bytevector
                  (map char->integer
                       (string->list (syntax->datum #'str))))))
         bv)))))

;;; The platform descriptions below are taken from
;;; 'scripts/qemu-binfmt-conf.sh' in QEMU.

(define %i386
  (qemu-platform
   (name "i386")
   (family "i386")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x03\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %i486
  (qemu-platform
   (name "i486")
   (family "i386")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x06\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xfe\xfe\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %x86_64
  (qemu-platform
   (name "x86_64")
   (family "i386")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x3e\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xfe\xfe\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %alpha
  (qemu-platform
   (name "alpha")
   (family "alpha")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x26\x90"))
   (mask (bv "\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %arm
  (qemu-platform
   (name "arm")
   (family "arm")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %armeb
  (qemu-platform
   (name "armeb")
   (family "armeb")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x28"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %sparc
  (qemu-platform
   (name "sparc")
   (family "sparc")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %sparc32plus
  (qemu-platform
   (name "sparc32plus")
   (family "sparc")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x12"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %sparc64
  (qemu-platform
   (name "sparc64")
   (family "sparc")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2b"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %ppc
  (qemu-platform
   (name "ppc")
   (family "ppc")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x14"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %ppc64
  (qemu-platform
   (name "ppc64")
   (family "ppc")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x15"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %ppc64le
  (qemu-platform
   (name "ppc64le")
   (family "ppcle")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x15\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\x00"))))

(define %m68k
  (qemu-platform
   (name "m68k")
   (family "m68k")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x04"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xfe\xfe\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

;; XXX: We could use the other endianness on a MIPS host.
(define %mips
  (qemu-platform
   (name "mips")
   (family "mips")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %mipsel
  (qemu-platform
   (name "mipsel")
   (family "mips")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %mipsn32
  (qemu-platform
   (name "mipsn32")
   (family "mips")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %mipsn32el
  (qemu-platform
   (name "mipsn32el")
   (family "mips")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %mips64
  (qemu-platform
   (name "mips64")
   (family "mips")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %mips64el
  (qemu-platform
   (name "mips64el")
   (family "mips")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x08\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %sh4
  (qemu-platform
   (name "sh4")
   (family "sh4")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2a\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %sh4eb
  (qemu-platform
   (name "sh4eb")
   (family "sh4")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x2a"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %s390x
  (qemu-platform
   (name "s390x")
   (family "s390x")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x16"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %aarch64
  (qemu-platform
   (name "aarch64")
   (family "arm")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %aarch64be
  (qemu-platform
   (name "aarch64be")
   (family "armeb")
   (magic (bv "\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xb7"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %hppa
  (qemu-platform
   (name "hppa")
   (family "hppa")
   (magic (bv "\x7f\x45\x4c\x46\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x0f"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %riscv32
  (qemu-platform
   (name "riscv32")
   (family "riscv")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xf3\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %riscv64
  (qemu-platform
   (name "riscv64")
   (family "riscv")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xf3\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %xtensa
  (qemu-platform
   (name "xtensa")
   (family "xtensa")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x5e\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %xtensaeb
  (qemu-platform
   (name "xtensaeb")
   (family "xtensaeb")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x5e"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %microblaze
  (qemu-platform
   (name "microblaze")
   (family "microblaze")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xba\xab"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %microblazeel
  (qemu-platform
   (name "microblazeel")
   (family "microblazeel")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xab\xba"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %or1k
  (qemu-platform
   (name "or1k")
   (family "or1k")
   (magic (bv "\x7fELF\x01\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x5c"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff"))))

(define %hexagon
  (qemu-platform
   (name "hexagon")
   (family "hexagon")
   (magic (bv "\x7fELF\x01\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\xa4\x00"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

(define %loongarch64
  (qemu-platform
   (name "loongarch64")
   (family "loongarch")
   (magic (bv "\x7fELF\x02\x01\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x02\x01"))
   (mask (bv "\xff\xff\xff\xff\xff\xff\xff\xfc\x00\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff\xff"))))

;; binfmt_misc: register: failed to install interpreter file /gnu/store/...
;; i486 and aarch64be aren't supported by the qemu-binfmt service.
(define %qemu-platforms
  (list %i386 %x86_64 %alpha %arm %sparc32plus %sparc64 %ppc %ppc64
        %ppc64le %m68k %mips %mipsel %mipsn32 %mipsn32el %mips64 %mips64el %sh4
        %sh4eb %s390x %aarch64 %hppa %riscv32 %riscv64 %xtensa
        %xtensaeb %microblaze %microblazeel %or1k %hexagon %loongarch64))

(define (lookup-qemu-platforms . names)
  "Return the list of QEMU platforms that match NAMES--a list of names such as
\"arm\", \"hppa\", etc."
  (filter (lambda (platform)
            (member (qemu-platform-name platform) names))
          %qemu-platforms))

(define-record-type* <qemu-binfmt-configuration>
  qemu-binfmt-configuration make-qemu-binfmt-configuration
  qemu-binfmt-configuration?
  (qemu        qemu-binfmt-configuration-qemu
               (default qemu))
  (platforms   qemu-binfmt-configuration-platforms
               (default '())))          ;safest default

(define (qemu-platform->binfmt qemu platform)
  "Return a gexp that evaluates to a binfmt string for PLATFORM, using the
given QEMU package."
  (define (bytevector->binfmt-string bv)
    ;; Return a binfmt-friendly string representing BV.  Hex-encode every
    ;; character, in particular because the doc notes "that you must escape
    ;; any NUL bytes; parsing halts at the first one".
    (string-concatenate
     (map (lambda (n)
            (string-append "\\x"
                           (string-pad (number->string n 16) 2 #\0)))
          (bytevector->u8-list bv))))

  (match platform
    (($ <qemu-platform> name family magic mask flags)
     ;; See 'Documentation/binfmt_misc.txt' in the kernel.
     #~(string-append ":qemu-" #$name ":M::"
                      #$(bytevector->binfmt-string magic)
                      ":" #$(bytevector->binfmt-string mask)
                      ":" #$qemu:static "/bin/qemu-" #$name
                      ":" #$flags))))

(define %binfmt-mount-point
  (file-system-mount-point %binary-format-file-system))

(define %binfmt-register-file
  (string-append %binfmt-mount-point "/register"))

(define qemu-binfmt-shepherd-services
  (match-lambda
    (($ <qemu-binfmt-configuration> qemu platforms)
     (list (shepherd-service
            (provision '(qemu-binfmt))
            (documentation "Install binfmt_misc handlers for QEMU.")
            (requirement '(file-system-/proc/sys/fs/binfmt_misc))
            (start #~(lambda ()
                       ;; Register the handlers for all of PLATFORMS.
                       (for-each (lambda (str)
                                   (call-with-output-file
                                       #$%binfmt-register-file
                                     (lambda (port)
                                       (display str port))))
                                 (list
                                  #$@(map (cut qemu-platform->binfmt qemu
                                               <>)
                                          platforms)))
                       #t))
            (stop #~(lambda (_)
                      ;; Unregister the handlers.
                      (for-each (lambda (name)
                                  (let ((file (string-append
                                               #$%binfmt-mount-point
                                               "/qemu-" name)))
                                    (call-with-output-file file
                                      (lambda (port)
                                        (display "-1" port)))))
                                '#$(map qemu-platform-name platforms))
                      #f)))))))

(define qemu-binfmt-service-type
  ;; TODO: Make a separate binfmt_misc service out of this?
  (service-type (name 'qemu-binfmt)
                (extensions
                 (list (service-extension file-system-service-type
                                          (const
                                           (list %binary-format-file-system)))
                       (service-extension shepherd-root-service-type
                                          qemu-binfmt-shepherd-services)))
                (default-value (qemu-binfmt-configuration))
                (description
                 "This service supports transparent emulation of binaries
compiled for other architectures using QEMU and the @code{binfmt_misc}
functionality of the kernel Linux.")))


;;;
;;; QEMU guest agent service.
;;;

(define-configuration qemu-guest-agent-configuration
  (qemu
   (file-like qemu-minimal)
   "QEMU package.")
  (device
   (string "")
   "Path to device or socket used to communicate with the host.  If not
specified, the QEMU default path is used."))

(define (qemu-guest-agent-shepherd-service config)
  (let ((qemu   (qemu-guest-agent-configuration-qemu config))
        (device (qemu-guest-agent-configuration-device config)))
    (list
     (shepherd-service
      (provision '(qemu-guest-agent))

      ;; The service needs to depend on udev, which brings up devices like
      ;; those under /dev/virtio-ports.
      (requirement '(user-processes udev))

      (documentation "Run the QEMU guest agent.")
      (start #~(make-forkexec-constructor
                `(,(string-append #$qemu "/bin/qemu-ga")
                  "--statedir" "/var/run"
                  ,@(if (string-null? #$device)
                        '()
                        (list "--path" #$device)))
                #:log-file "/var/log/qemu-ga.log"))
      (stop #~(make-kill-destructor))))))

(define qemu-guest-agent-service-type
  (service-type
   (name 'qemu-guest-agent)
   (extensions
    (list (service-extension shepherd-root-service-type
                             qemu-guest-agent-shepherd-service)))
   (default-value (qemu-guest-agent-configuration))
   (description "Run the QEMU guest agent.")))


;;;
;;; Guest agent for VMs running under Xen
;;;
(define-configuration/no-serialization xe-guest-utilities-configuration
  (package
   (package xe-guest-utilities)
   "Xen guest management utilities package.")
  (pid-file
   (string "/var/run/xe-daemon.pid")
   "Path to the file holding the PID of xe-deamon.")
  (log-file
   (string "/var/log/xe-guest-utilties.log")
   "Path to xe-guest-utilities log file."))

(define (generate-xe-guest-utilities-documentation)
  "Generate documentation for xe-guest-utilities-configuration fields"
  (generate-documentation
   `((xe-guest-utilities-configuration ,xe-guest-utilities-configuration-fields))
   'xe-guest-utilities-configuration))

(define (xe-guest-utilities-shepherd-service config)
  (let ((xe-guest-utils (xe-guest-utilities-configuration-package config))
        (pid-file (xe-guest-utilities-configuration-pid-file config))
        (log-file (xe-guest-utilities-configuration-log-file config)))
    (list
     (shepherd-service
      (provision '(xen-guest-agent))
      (requirement '(networking user-processes udev))
      (documentation "Run the Xen guest management agent.")
      (start
       #~(lambda _
           (let ((pid (make-forkexec-constructor
                       (list
                        #$(file-append xe-guest-utils
                                       "/sbin/xe-daemon")
                        "-p" #$pid-file)
                       #:log-file #$log-file
                       #:pid-file #$pid-file
                       #:environment-variables
                       (list (string-append
                              "PATH="
                              #$(file-append xe-guest-utils "/bin") ":"
                              ;; logger
                              #$(file-append inetutils "/bin"))))))
             ;; Run xe-linux-distribution script before starting the actual
             ;; daemon. The script collects some basic system information that
             ;; is shared back to the Xen host.
             (system* #$(file-append xe-guest-utils "/sbin/xe-linux-distribution")
                      "/var/cache/xe-linux-distribution")
             ;; Finally, start and return the PID made by
             ;; make-forkexec-constructor.
             pid)))
      (stop #~(make-kill-destructor))))))

(define (xe-guest-utilities-udev-rules-service config)
  (let ((guest-utils (xe-guest-utilities-configuration-package config)))
    (list
     (file->udev-rule
      "z10_xen-vcpu-hotplug.rules"
      (file-append guest-utils "/lib/udev/rules.d/z10_xen-vcpu-hotplug.rules")))))

(define xe-guest-utilities-service-type
  (service-type
   (name 'xe-guest-utilities)
   (extensions
    (list (service-extension shepherd-root-service-type
                             xe-guest-utilities-shepherd-service)
          (service-extension udev-service-type
                             xe-guest-utilities-udev-rules-service)))
   (default-value (xe-guest-utilities-configuration))
   (description "Run the Xen guest management utilities.")))

(define-configuration/no-serialization xen-guest-agent-configuration
  (package
    (package xen-guest-agent)
    "Xen guest agent package."))

(define (generate-xen-guest-agent-documentation)
  "Generate documentation for xen-guest-agent-configuration fields"
  (generate-documentation
   `((xen-guest-agent-configuration ,xen-guest-agent-configuration-fields))
   'xen-guest-agent-configuration))

(define (xen-guest-agent-shepherd-service config)
  (list
   (shepherd-service
    (provision '(xen-guest-agent))
    (requirement '(networking user-processes udev))
    (documentation "Run the Xen guest management agent.")
    (start #~(make-forkexec-constructor
              (list #$(file-append xen-guest-agent "/bin/xen-guest-agent"))))
    (stop #~(make-kill-destructor)))))

(define xen-guest-agent-service-type
  (service-type
   (name 'xen-guest-agent)
   (extensions
    (list (service-extension shepherd-root-service-type
                             xen-guest-agent-shepherd-service)))
   (default-value (xen-guest-agent-configuration))
   (description "Run the Xen guest management utilities.")))


;;;
;;; Secrets for guest VMs.
;;;

(define (secret-service-shepherd-services address)
  "Return a Shepherd service that fetches sensitive material at local PORT,
over TCP.  Reboot upon failure."
  ;; This is a Shepherd service, rather than an activation snippet, to make
  ;; sure it is started once 'networking' is up so it can accept incoming
  ;; connections.
  (list
   (shepherd-service
    (documentation "Fetch secrets from the host at startup time.")
    (provision '(secret-service-client))
    (requirement '(loopback networking))
    (modules '((gnu build secret-service)
               (guix build utils)))
    (start (with-imported-modules '((gnu build secret-service)
                                    (guix build utils))
             #~(lambda ()
                 ;; Since shepherd's output port goes to /dev/log, write this
                 ;; message to stderr so it's visible on the Mach console.
                 (format (current-error-port)
                         "receiving secrets from the host...~%")
                 (force-output (current-error-port))

                 (let ((sent (secret-service-receive-secrets #$address)))
                   (unless sent
                     (sleep 3)
                     (reboot))))))
    (stop #~(const #f)))))

(define secret-service-type
  (service-type
   (name 'secret-service)
   (extensions (list (service-extension shepherd-root-service-type
                                        secret-service-shepherd-services)

                     ;; Make every Shepherd service depend on
                     ;; 'secret-service-client'.
                     (service-extension user-processes-service-type
                                        (const '(secret-service-client)))))
   (description
    "This service fetches secret key and other sensitive material over TCP at
boot time.  This service is meant to be used by virtual machines (VMs) that
can only be accessed by their host.")))

(define* (secret-service-operating-system os
                                          #:optional
                                          (address
                                           #~(make-socket-address
                                              AF_INET INADDR_ANY 1004)))
  "Return an operating system based on OS that includes the secret-service,
that will be listening to receive secret keys on ADDRESS."
  (operating-system
    (inherit os)
    (services
     ;; Turn off SSH and Guix key generation that normally happens during
     ;; activation: that requires entropy and thus takes time during boot, and
     ;; those keys are going to be overwritten by secrets received from the
     ;; host anyway.
     (cons (service secret-service-type address)
           (modify-services (operating-system-user-services os)
             (openssh-service-type
              config => (openssh-configuration
                         (inherit config)
                         (generate-host-keys? #f)))
             (guix-service-type
              config => (guix-configuration
                         (inherit config)
                         (generate-substitute-key? #f))))))))


;;;
;;; Offloading-as-a-service.
;;;

(define-record-type* <virtual-build-machine>
  virtual-build-machine make-virtual-build-machine
  virtual-build-machine?
  this-virtual-build-machine
  (name        virtual-build-machine-name
               (default 'build-vm))
  (image       virtual-build-machine-image
               (thunked)
               (default
                 (virtual-build-machine-default-image
                  this-virtual-build-machine)))
  (qemu        virtual-build-machine-qemu
               (default qemu-minimal))
  (cpu         virtual-build-machine-cpu
               (thunked)
               (default
                 (qemu-cpu-model-for-date
                  (virtual-build-machine-systems this-virtual-build-machine)
                  (virtual-build-machine-date this-virtual-build-machine))))
  (cpu-count   virtual-build-machine-cpu-count
               (default 4))
  (memory-size virtual-build-machine-memory-size  ;integer (MiB)
               (default 2048))
  (date        virtual-build-machine-date
               ;; Default to a date "in the past" assuming a common use case
               ;; is to rebuild old packages.
               (default (make-date 0 0 00 00 01 01 2020 0)))
  (port-forwardings virtual-build-machine-port-forwardings
                    (default
                      `((,%build-vm-ssh-port . 22)
                        (,%build-vm-secrets-port . 1004))))
  (systems     virtual-build-machine-systems
               (default (list (%current-system))))
  (auto-start? virtual-build-machine-auto-start?
               (default #f)))

(define %build-vm-ssh-port
  ;; Default host port where the guest's SSH port is forwarded.
  11022)

(define %build-vm-secrets-port
  ;; Host port to communicate secrets to the build VM.
  ;; FIXME: Anyone on the host can talk to it; use virtio ports or AF_VSOCK
  ;; instead.
  11044)

(define %x86-64-intel-cpu-models
  ;; List of release date/CPU model pairs representing Intel's x86_64 models.
  ;; The list is taken from
  ;; <https://en.wikipedia.org/wiki/List_of_Intel_CPU_microarchitectures>.
  ;; CPU model strings are those found in 'qemu-system-x86_64 -cpu help'.
  (letrec-syntax ((cpu-models (syntax-rules ()
                                ((_ (date model) rest ...)
                                 (alist-cons (date->time-utc
                                              (string->date date "~Y-~m-~d"))
                                             model
                                             (cpu-models rest ...)))
                                ((_)
                                 '()))))
    (reverse
     (cpu-models ("2006-01-01" "core2duo")
                 ("2010-01-01" "Westmere")
                 ("2008-01-01" "Nehalem")
                 ("2011-01-01" "SandyBridge")
                 ("2012-01-01" "IvyBridge")
                 ("2013-01-01" "Haswell")
                 ("2014-01-01" "Broadwell")
                 ("2015-01-01" "Skylake-Client")))))

(define (qemu-cpu-model-for-date systems date)
  "Return the QEMU name of a CPU model for SYSTEMS that was current at DATE."
  (if (any (cut string-prefix? "x86_64-" <>) systems)
      (let ((time (date->time-utc date)))
        (any (match-lambda
               ((release-date . model)
                (and (time<? release-date time)
                     model)))
             %x86-64-intel-cpu-models))
      ;; TODO: Add models for other architectures.
      "host"))

(define (virtual-build-machine-ssh-port config)
  "Return the host port where CONFIG has its VM's SSH port forwarded."
  (any (match-lambda
         ((host-port . 22) host-port)
         (_ #f))
       (virtual-build-machine-port-forwardings config)))

(define (virtual-build-machine-secrets-port config)
  "Return the host port where CONFIG has its VM's secrets port forwarded."
  (any (match-lambda
         ((host-port . 1004) host-port)
         (_ #f))
       (virtual-build-machine-port-forwardings config)))

(define %system-log-message-destination
  ;; Shepherd system log message destination procedure.  Log most messages to
  ;; the console, which goes to the serial output, allowing the host to log
  ;; it.
  #~(lambda (message)
      (cond ((= (system-log-priority debug)
                (system-log-message-priority message))
             '("/var/log/debug"))
            ((member (system-log-message-facility message)
                     (list (system-log-facility authorization)
                           (system-log-facility authorization/private)))
             '("/var/log/secure"))
            (else
             '("/dev/console")))))

(define %virtual-build-machine-operating-system
  (operating-system
    (host-name "build-machine")

    (locale "en_US.utf8")
    (locale-definitions
     ;; Save space by providing only one locale.
     (list (locale-definition (name "en_US.utf8")
                              (source "en_US")
                              (charset "UTF-8"))))

    (bootloader (bootloader-configuration         ;unused
                 (bootloader grub-minimal-bootloader)
                 (targets '("/dev/null"))))
    (file-systems (cons (file-system              ;unused
                          (mount-point "/")
                          (device "none")
                          (type "tmpfs"))
                        %base-file-systems))
    (users (cons (user-account
                  (name "offload")
                  (group "users")
                  (supplementary-groups '("kvm"))
                  (comment "Account used for offloading"))
                 %base-user-accounts))
    (services (cons* (service static-networking-service-type
                              (list %qemu-static-networking))
                     (service openssh-service-type
                              (openssh-configuration
                               (openssh openssh-sans-x)))

                     ;; Run GC once per hour.
                     (simple-service 'perdiodic-gc mcron-service-type
                                     (list #~(job "12 * * * *"
                                                  "guix gc -F 2G")))

                     (modify-services %base-services
                       ;; By default, the secret service introduces a
                       ;; pre-initialized /etc/guix/acl file in the VM.  Thus,
                       ;; clear 'authorize-key?' so that it's not overridden
                       ;; at activation time.
                       ;; Since it's used for offloading, disable substitutes
                       ;; (this avoids network issues).
                       (guix-service-type config =>
                                          (guix-configuration
                                           (inherit config)
                                           (authorize-key? #f)
                                           (use-substitutes? #f)))
                       (shepherd-system-log-service-type
                        config =>
                        (system-log-configuration
                         (message-destination %system-log-message-destination)))
                       (delete mingetty-service-type)
                       (delete console-font-service-type))))))

(define %default-virtual-build-machine-image-size
  ;; Size of the default disk image of virtual build machines.  It should be
  ;; large enough to let users build a few things.
  (* 20 (expt 2 30)))

(define (virtual-build-machine-default-image config)
  (let* ((type (lookup-image-type-by-name 'mbr-raw))
         (base (os->image %virtual-build-machine-operating-system
                          #:type type)))
    (image (inherit base)
           (name (symbol-append 'build-vm-
                                (virtual-build-machine-name config)))
           (format 'compressed-qcow2)
           (partition-table-type 'mbr)
           (volatile-root? #f)
           (shared-store? #f)
           (size %default-virtual-build-machine-image-size)
           (partitions (match (image-partitions base)
                         ((root)
                          ;; Increase the size of the root partition to match
                          ;; that of the disk image.
                          (let ((root-size (- size (* 50 (expt 2 20)))))
                            (list (partition
                                   (inherit root)
                                   (size root-size))))))))))

(define (virtual-build-machine-account-name config)
  (string-append "build-vm-"
                 (symbol->string
                  (virtual-build-machine-name config))))

(define (virtual-build-machine-accounts config)
  (let ((name (virtual-build-machine-account-name config)))
    (list (user-group (name name) (system? #t))
          (user-account
           (name name)
           (group name)
           (supplementary-groups '("kvm"))
           (comment "Privilege separation user for the virtual build machine")
           (home-directory "/var/empty")
           (shell (file-append shadow "/sbin/nologin"))
           (system? #t)))))

(define (build-vm-shepherd-services config)
  (define transform
    (compose secret-service-operating-system
             operating-system-with-locked-root-account
             operating-system-with-offloading-account
             (lambda (os)
               (virtualized-operating-system os #:full-boot? #t))))

  (define transformed-image
    (let ((base (virtual-build-machine-image config)))
      (image
       (inherit base)
       (operating-system
         (transform (image-operating-system base))))))

  (define command
    (linux-image-startup-command transformed-image
                                 #:qemu
                                 (virtual-build-machine-qemu config)
                                 #:cpu
                                 (virtual-build-machine-cpu config)
                                 #:cpu-count
                                 (virtual-build-machine-cpu-count config)
                                 #:memory-size
                                 (virtual-build-machine-memory-size config)
                                 #:port-forwardings
                                 (virtual-build-machine-port-forwardings
                                  config)
                                 #:date
                                 (virtual-build-machine-date config)))

  (define user
    (virtual-build-machine-account-name config))

  (list (shepherd-service
         (documentation "Run the build virtual machine service.")
         (provision (list (virtual-build-machine-name config)))
         (requirement '(user-processes))
         (modules `((gnu build secret-service)
                    (guix build utils)
                    ,@%default-modules))
         (start
          (with-imported-modules (source-module-closure
                                  '((gnu build secret-service)
                                    (guix build utils)))
            #~(lambda arguments
                (let* ((pid  (fork+exec-command (append #$command arguments)
                                                #:user #$user
                                                #:group "kvm"
                                                #:environment-variables
                                                ;; QEMU tries to write to /var/tmp
                                                ;; by default.
                                                '("TMPDIR=/tmp")))
                       (port #$(virtual-build-machine-secrets-port config))
                       (root #$(virtual-build-machine-secret-root config))
                       (address (make-socket-address AF_INET INADDR_LOOPBACK
                                                     port)))
                  (catch #t
                    (lambda _
                      (if (secret-service-send-secrets address root)
                          pid
                          (begin
                            (kill (- pid) SIGTERM)
                            #f)))
                    (lambda (key . args)
                      (kill (- pid) SIGTERM)
                      (apply throw key args)))))))
         (stop #~(make-kill-destructor))
         (actions
          (list (shepherd-action
                 (name 'configuration)
                 (documentation
                  "Display the configuration of this virtual build machine.")
                 (procedure
                  #~(lambda (_)
                      (format #t "CPU: ~a~%"
                              #$(virtual-build-machine-cpu config))
                      (format #t "number of CPU cores: ~a~%"
                              #$(virtual-build-machine-cpu-count config))
                      (format #t "memory size: ~a MiB~%"
                              #$(virtual-build-machine-memory-size config))
                      (format #t "initial date: ~a~%"
                              #$(date->string
                                 (virtual-build-machine-date config))))))))
         (auto-start? (virtual-build-machine-auto-start? config)))))

(define (authorize-guest-substitutes-on-host)
  "Return a program that authorizes the guest's archive signing key (passed as
an argument) on the host."
  (define not-config?
    (match-lambda
      ('(guix config) #f)
      (('guix _ ...) #t)
      (('gnu _ ...) #t)
      (_ #f)))

  (define run
    (with-extensions (list guile-gcrypt)
      (with-imported-modules `(((guix config) => ,(make-config.scm))
                               ,@(source-module-closure
                                  '((guix pki)
                                    (guix build utils))
                                  #:select? not-config?))
        #~(begin
            (use-modules (ice-9 match)
                         (ice-9 textual-ports)
                         (gcrypt pk-crypto)
                         (guix pki)
                         (guix build utils))

            (match (command-line)
              ((_ guest-config-directory)
               (let ((guest-key (string-append guest-config-directory
                                               "/signing-key.pub")))
                 (if (file-exists? guest-key)
                     ;; Add guest key to the host's ACL.
                     (let* ((key (string->canonical-sexp
                                  (call-with-input-file guest-key
                                    get-string-all)))
                            (acl (public-keys->acl
                                  (cons key (acl->public-keys (current-acl))))))
                       (with-atomic-file-replacement %acl-file
                         (lambda (_ port)
                           (write-acl acl port))))
                     (format (current-error-port)
                             "warning: guest key missing from '~a'~%"
                             guest-key)))))))))

  (program-file "authorize-guest-substitutes-on-host" run))

(define (initialize-build-vm-substitutes)
  "Initialize the Hurd VM's key pair and ACL and store it on the host."
  (define run
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (define host-key
            "/etc/guix/signing-key.pub")

          (define host-acl
            "/etc/guix/acl")

          (match (command-line)
            ((_ guest-config-directory)
             (setenv "GUIX_CONFIGURATION_DIRECTORY"
                     guest-config-directory)
             (invoke #+(file-append guix "/bin/guix") "archive"
                     "--generate-key")

             (when (file-exists? host-acl)
               ;; Copy the host ACL.
               (copy-file host-acl
                          (string-append guest-config-directory
                                         "/acl")))

             (when (file-exists? host-key)
               ;; Add the host key to the childhurd's ACL.
               (let ((key (open-fdes host-key O_RDONLY)))
                 (close-fdes 0)
                 (dup2 key 0)
                 (execl #+(file-append guix "/bin/guix")
                        "guix" "archive" "--authorize"))))))))

  (program-file "initialize-build-vm-substitutes" run))

(define* (build-vm-activation secret-directory
                              #:key
                              offloading-ssh-key
                              (offloading? #t))
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define secret-directory
          #$secret-directory)

        (define ssh-directory
          (string-append secret-directory "/etc/ssh"))

        (define guix-directory
          (string-append secret-directory "/etc/guix"))

        (define offloading-ssh-key
          #$offloading-ssh-key)

        (unless (file-exists? ssh-directory)
          ;; Generate SSH host keys under SSH-DIRECTORY.
          (mkdir-p ssh-directory)
          (invoke #$(file-append openssh "/bin/ssh-keygen")
                  "-A" "-f" secret-directory))

        (unless (or (not #$offloading?)
                    (file-exists? offloading-ssh-key))
          ;; Generate a user SSH key pair for the host to use when offloading
          ;; to the guest.
          (mkdir-p (dirname offloading-ssh-key))
          (invoke #$(file-append openssh "/bin/ssh-keygen")
                  "-t" "ed25519" "-N" ""
                  "-f" offloading-ssh-key)

          ;; Authorize it in the guest for user 'offloading'.
          (let ((authorizations
                 (string-append ssh-directory
                                "/authorized_keys.d/offloading")))
            (mkdir-p (dirname authorizations))
            (copy-file (string-append offloading-ssh-key ".pub")
                       authorizations)
            (chmod (dirname authorizations) #o555)))

        (unless (file-exists? guix-directory)
          (invoke #$(initialize-build-vm-substitutes)
                  guix-directory))

        (when #$offloading?
          ;; Authorize the archive signing key from GUIX-DIRECTORY in the host.
          (invoke #$(authorize-guest-substitutes-on-host) guix-directory)))))

(define (virtual-build-machine-offloading-ssh-key config)
  "Return the name of the file containing the SSH key of user 'offloading'."
  (string-append "/etc/guix/offload/ssh/virtual-build-machine/"
                 (symbol->string
                  (virtual-build-machine-name config))))

(define (virtual-build-machine-activation config)
  "Return a gexp to activate the build VM according to CONFIG."
  (build-vm-activation (virtual-build-machine-secret-root config)
                       #:offloading? #t
                       #:offloading-ssh-key
                       (virtual-build-machine-offloading-ssh-key config)))

(define (virtual-build-machine-secret-root config)
  (string-append "/etc/guix/virtual-build-machines/"
                 (symbol->string
                  (virtual-build-machine-name config))))

(define (check-vm-availability config)
  "Return a Scheme file that evaluates to true if the service corresponding to
CONFIG, a <virtual-build-machine>, is up and running."
  (define service-name
    (virtual-build-machine-name config))

  (scheme-file "check-build-vm-availability.scm"
               #~(begin
                   (use-modules (gnu services herd)
                                (srfi srfi-34))

                   (guard (c ((service-not-found-error? c) #f))
                     (->bool (live-service-running
                              (current-service '#$service-name)))))))

(define (build-vm-guix-extension config)
  (define vm-ssh-key
    (string-append
     (virtual-build-machine-secret-root config)
     "/etc/ssh/ssh_host_ed25519_key.pub"))

  (define host-ssh-key
    (virtual-build-machine-offloading-ssh-key config))

  (guix-extension
   (build-machines
    (list #~(if (primitive-load #$(check-vm-availability config))
                (list (build-machine
                       (name "localhost")
                       (port #$(virtual-build-machine-ssh-port config))
                       (systems
                        '#$(virtual-build-machine-systems config))
                       (user "offloading")
                       (host-key (call-with-input-file #$vm-ssh-key
                                   (@ (ice-9 textual-ports)
                                      get-string-all)))
                       (private-key #$host-ssh-key)))
                '())))))

(define virtual-build-machine-service-type
  (service-type
   (name 'build-vm)
   (extensions (list (service-extension shepherd-root-service-type
                                        build-vm-shepherd-services)
                     (service-extension guix-service-type
                                        build-vm-guix-extension)
                     (service-extension account-service-type
                                        virtual-build-machine-accounts)
                     (service-extension activation-service-type
                                        virtual-build-machine-activation)))
   (description
    "Create a @dfn{virtual build machine}: a virtual machine (VM) that builds
can be offloaded to.  By default, the virtual machine starts with a clock
running at some point in the past.")
   (default-value (virtual-build-machine))))


;;;
;;; The Hurd in VM service: a Childhurd.
;;;

(define (operating-system-with-offloading-account os)
  (define accounts
    (list (user-group
           (name "offloading")
           (system? #t))
          (user-account
           (name "offloading")
           (group "offloading")
           (system? #t)
           (comment "Offloading privilege separation user")
           (home-directory "/var/run/offloading")
           (shell (file-append bash-minimal "/bin/sh")))))

  (operating-system
    (inherit os)
    (services (cons (simple-service 'offloading-account
                                    account-service-type
                                    accounts)
                    (operating-system-user-services os)))))

(define (operating-system-with-locked-root-account os)
  "Return OS with a 'root' account whose password is uninitialized, thereby
preventing password-based authentication as 'root'."
  (define root
    ;; %ROOT-ACCOUNT has an empty password; change that to an uninitialized
    ;; password.
    (user-account
     (inherit %root-account)
     (password #f)))

  (operating-system
    (inherit os)
    (users (cons root (operating-system-users os)))))

(define %hurd-vm-operating-system
  (operating-system
    (inherit %hurd-default-operating-system)
    (host-name "childhurd")
    (timezone "Europe/Amsterdam")
    (bootloader (bootloader-configuration
                 (bootloader grub-minimal-bootloader)
                 (targets '("/dev/vda"))
                 (timeout 0)))
    (packages (cons* gdb-minimal
                     (operating-system-packages
                      %hurd-default-operating-system)))
    (services (cons*
               (service openssh-service-type
                        (openssh-configuration
                         (openssh openssh-sans-x)
                         (use-pam? #f)
                         (permit-root-login 'prohibit-password)
                         (allow-empty-passwords? #t)
                         (password-authentication? #t)))

               ;; By default, the secret service introduces a pre-initialized
               ;; /etc/guix/acl file in the childhurd.  Thus, clear
               ;; 'authorize-key?' so that it's not overridden at activation
               ;; time.
               (modify-services %base-services+qemu-networking/hurd
                 (guix-service-type config =>
                                    (guix-configuration
                                     (inherit config)
                                     (authorize-key? #f))))))))

(define-record-type* <hurd-vm-configuration>
  hurd-vm-configuration make-hurd-vm-configuration
  hurd-vm-configuration?
  (os          hurd-vm-configuration-os                 ;<operating-system>
               (default %hurd-vm-operating-system))
  (qemu        hurd-vm-configuration-qemu               ;file-like
               (default qemu-minimal))
  (image       hurd-vm-configuration-image              ;<image>
               (thunked)
               (default (hurd-vm-disk-image this-record)))
  (disk-size   hurd-vm-configuration-disk-size          ;number or 'guess
               (default 'guess))
  (memory-size hurd-vm-configuration-memory-size        ;number
               (default 2048))
  (options     hurd-vm-configuration-options            ;list of string
               (default `("--snapshot")))
  (id          hurd-vm-configuration-id                 ;#f or integer [1..]
               (default #f))
  (net-options hurd-vm-configuration-net-options        ;list of string
               (thunked)
               (default (hurd-vm-net-options this-record)))
  (offloading? hurd-vm-configuration-offloading?        ;Boolean
               (default #t))
  (secret-root hurd-vm-configuration-secret-root        ;string
               (default "/etc/childhurd")))

(define (hurd-vm-disk-image config)
  "Return a disk-image for the Hurd according to CONFIG.  The secret-service
is added to the OS specified in CONFIG."
  (define transform
    (compose secret-service-operating-system
             ;; When offloading is enabled, (1) add the 'offloading' account,
             ;; and (2) prevent users from logging in as 'root' without a
             ;; password as this would allow any user on the host to populate
             ;; the host's store indirectly (for example by logging in as root
             ;; in the Hurd VM over VNC).
             (if (hurd-vm-configuration-offloading? config)
                 (compose operating-system-with-locked-root-account
                          operating-system-with-offloading-account)
                 identity)))

  (let* ((os        (transform (hurd-vm-configuration-os config)))
         (disk-size (hurd-vm-configuration-disk-size config))
         (type      (lookup-image-type-by-name 'hurd-qcow2))
         (os->image (image-type-constructor type)))
    (image (inherit (os->image os))
           (size disk-size))))

(define (hurd-vm-port config base)
  "Return the forwarded vm port for this childhurd config."
  (let ((id (or (hurd-vm-configuration-id config) 0)))
    (+ base (* 1000 id))))
(define %hurd-vm-secrets-port 11004)
(define %hurd-vm-ssh-port 10022)
(define %hurd-vm-vnc-port 15900)

(define (hurd-vm-net-options config)
  `("--device" "rtl8139,netdev=net0"
    "--netdev"
    ,(string-append "user,id=net0"
                    ",hostfwd=tcp:127.0.0.1:"
                    (number->string (hurd-vm-port config %hurd-vm-secrets-port))
                    "-:1004"
                    ",hostfwd=tcp:127.0.0.1:"
                    (number->string (hurd-vm-port config %hurd-vm-ssh-port))
                    "-:22"
                    ",hostfwd=tcp:127.0.0.1:"
                    (number->string (hurd-vm-port config %hurd-vm-vnc-port))
                    "-:5900")))

(define (hurd-vm-shepherd-service config)
  "Return a <shepherd-service> for a Hurd in a Virtual Machine with CONFIG."

  (let ((image       (hurd-vm-configuration-image config))
        (qemu        (hurd-vm-configuration-qemu config))
        (memory-size (hurd-vm-configuration-memory-size config))
        (options     (hurd-vm-configuration-options config))
        (id          (hurd-vm-configuration-id config))
        (net-options (hurd-vm-configuration-net-options config))
        (provisions  '(hurd-vm childhurd)))

    (define vm-command
      ;; XXX: Use the x86_64 emulator instead of the i386 one to work around
      ;; "Bad ram pointer" issues: <https://issues.guix.gnu.org/66053>.
      #~(append (list #$(file-append qemu "/bin/qemu-system-x86_64")
                      "-m" (number->string #$memory-size)
                      #$@net-options
                      #$@options
                      "--hda" #+(system-image image)

                      ;; Cause the service to be respawned if the guest
                      ;; reboots (it can reboot for instance if it did not
                      ;; receive valid secrets, or if it crashed.)
                      "--no-reboot")
                (if (file-exists? "/dev/kvm")
                    '("--enable-kvm")
                    '())))

    (list
     (shepherd-service
      (documentation "Run the Hurd in a Virtual Machine: a Childhurd.")
      (provision (if id
                     (map
                      (cute symbol-append <>
                            (string->symbol (number->string id)))
                      provisions)
                     provisions))
      (requirement '(loopback networking user-processes))
      (start
       (with-imported-modules
           (source-module-closure '((gnu build secret-service)
                                    (guix build utils)))
         #~(lambda ()
             (let* ((pid  (fork+exec-command #$vm-command
                                             #:user "childhurd"
                                             ;; XXX TODO: use "childhurd" after
                                             ;; updating Shepherd
                                             #:group "kvm"
                                             #:environment-variables
                                             ;; QEMU tries to write to /var/tmp
                                             ;; by default.
                                             '("TMPDIR=/tmp")))
                    (port #$(hurd-vm-port config %hurd-vm-secrets-port))
                    (root #$(hurd-vm-configuration-secret-root config))
                    (address (make-socket-address AF_INET INADDR_LOOPBACK
                                                  port)))
               (catch #t
                 (lambda _
                   ;; XXX: 'secret-service-send-secrets' won't complete until
                   ;; the guest has booted and its secret service server is
                   ;; running, which could take 20+ seconds during which PID 1
                   ;; is stuck waiting.
                   (if (secret-service-send-secrets address root)
                       pid
                       (begin
                         (kill (- pid) SIGTERM)
                         #f)))
                 (lambda (key . args)
                   (kill (- pid) SIGTERM)
                   (apply throw key args)))))))
      (modules `((gnu build secret-service)
                 (guix build utils)
                 ,@%default-modules))
      (stop  #~(make-kill-destructor))))))

(define %hurd-vm-accounts
  (list (user-group (name "childhurd") (system? #t))
        (user-account
         (name "childhurd")
         (group "childhurd")
         (supplementary-groups '("kvm"))
         (comment "Privilege separation user for the childhurd")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin"))
         (system? #t))))

(define (hurd-vm-activation config)
  "Return a gexp to activate the Hurd VM according to CONFIG."
  (build-vm-activation (hurd-vm-configuration-secret-root config)
                       #:offloading?
                       (hurd-vm-configuration-offloading? config)
                       #:offloading-ssh-key
                       (hurd-vm-configuration-offloading-ssh-key config)))

(define (hurd-vm-configuration-offloading-ssh-key config)
  "Return the name of the file containing the SSH key of user 'offloading'."
  (string-append "/etc/guix/offload/ssh/childhurd"
                 (or (and=> (hurd-vm-configuration-id config)
                            number->string)
                     "")))

(define (hurd-vm-guix-extension config)
  "When offloading is enabled, add this childhurd to the list of offlading
machines in /etc/guix/machines.scm."
  (if (hurd-vm-configuration-offloading? config)
      (let* ((image (hurd-vm-configuration-image config))
             (platform (image-platform image))
             (system (platform-system platform))
             (vm-ssh-key (string-append
                          (hurd-vm-configuration-secret-root config)
                          "/etc/ssh/ssh_host_ed25519_key.pub"))
             (host-ssh-key (hurd-vm-configuration-offloading-ssh-key config)))
        (guix-extension
         (build-machines
          (list #~(build-machine
                   (name "localhost")
                   (port #$(hurd-vm-port config %hurd-vm-ssh-port))
                   (systems '(#$system))
                   (host-key (call-with-input-file #$vm-ssh-key
                               (@ (ice-9 textual-ports)
                                  get-string-all)))
                   (user "offloading")
                   (overload-threshold 1.8) ;current load reporting is off by 1
                   (private-key #$host-ssh-key))))))
      (guix-extension)))

(define hurd-vm-service-type
  (service-type
   (name 'hurd-vm)
   (extensions (list (service-extension shepherd-root-service-type
                                        hurd-vm-shepherd-service)
                     (service-extension account-service-type
                                        (const %hurd-vm-accounts))
                     (service-extension guix-service-type
                                        hurd-vm-guix-extension)
                     (service-extension activation-service-type
                                        hurd-vm-activation)))
   (default-value (hurd-vm-configuration))
   (description
    "Provide a virtual machine (VM) running GNU/Hurd, also known as a
@dfn{childhurd}.")))
