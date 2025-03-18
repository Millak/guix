;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2025 John Darrington <jmd@gnu.org>
;;; Copyright © 2018, 2019, 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020, 2021 Maxim Cournoyer <maxim@guixotic.coop>
;;; Copyright © 2023-2025 Ian Eure <ian@retrospec.tv>
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

(define-module (gnu services nfs)
  #:use-module (gnu build file-systems)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (guix)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 match)
  #:export (rpcbind-service-type
            rpcbind-configuration
            rpcbind-configuration?

            pipefs-service-type
            pipefs-configuration
            pipefs-configuration?

            idmap-service-type
            idmap-configuration
            idmap-configuration?

            gss-service-type
            gss-configuration
            gss-configuration?

            nfs-service-type
            nfs-configuration
            nfs-configuration?

            autofs-service-type
            autofs-configuration
            autofs-configuration?

            autofs-indirect-map
            autofs-indirect-map?

            autofs-map-entry
            autofs-map-entry?))

(define default-pipefs-directory "/var/lib/nfs/rpc_pipefs")



(define-record-type* <rpcbind-configuration>
  rpcbind-configuration make-rpcbind-configuration
  rpcbind-configuration?
  (rpcbind             rpcbind-configuration-rpcbind
                       (default rpcbind))
  (warm-start?         rpcbind-configuration-warm-start?
                       (default #t)))

(define rpcbind-service-type
  (let ((proc
         (lambda (config)
           (define rpcbind
             (rpcbind-configuration-rpcbind config))

           (define rpcbind-command
             #~(list (string-append #$rpcbind "/sbin/rpcbind") "-f"
                     #$@(if (rpcbind-configuration-warm-start? config) '("-w") '())))

           (shepherd-service
            (documentation "Start the RPC bind daemon.")
            (requirement '(user-processes networking))
            (provision '(rpcbind-daemon))

            (start #~(make-forkexec-constructor #$rpcbind-command))
            (stop #~(make-kill-destructor))))))
    (service-type
     (name 'rpcbind)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values)
               (match values
                 ((first . rest) first)
                 (_ config))))
     (default-value (rpcbind-configuration))
     (description "Run the RPC Bind service, which provides a facility to map
ONC RPC program numbers into universal addresses.  Many NFS related services
use this facility."))))



(define-record-type* <pipefs-configuration>
  pipefs-configuration make-pipefs-configuration
  pipefs-configuration?
  (mount-point           pipefs-configuration-mount-point
                         (default default-pipefs-directory)))

(define pipefs-service-type
  (let ((proc
         (lambda (config)
           (define pipefs-directory (pipefs-configuration-mount-point config))

           (shepherd-service
            (documentation "Mount the pipefs pseudo file system.")
            (provision '(rpc-pipefs))

            (start #~(lambda ()
                       (mkdir-p #$pipefs-directory)
                       (mount "rpc_pipefs" #$pipefs-directory "rpc_pipefs")
                       (member #$pipefs-directory (mount-points))))

            (stop #~(lambda (pid . args)
                      (umount #$pipefs-directory MNT_DETACH)
                      (not (member #$pipefs-directory (mount-points)))))))))
    (service-type
     (name 'pipefs)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values)
               (match values
                 ((first . rest) first)
                 (_ config))))
     (default-value (pipefs-configuration))
     (description "Mount the pipefs file system, which is used to transfer
NFS-related data between the kernel and user-space programs."))))



(define-record-type* <gss-configuration>
  gss-configuration make-gss-configuration
  gss-configuration?
  (pipefs-directory      gss-configuration-pipefs-directory
                         (default default-pipefs-directory))
  (nfs-utils             gss-configuration-gss
                         (default nfs-utils)))

(define gss-service-type
  (let ((proc
         (lambda (config)
           (define nfs-utils
             (gss-configuration-gss config))

           (define pipefs-directory
             (gss-configuration-pipefs-directory config))

           (define gss-command
             #~(list (string-append #$nfs-utils "/sbin/rpc.gssd") "-f"
                     "-p" #$pipefs-directory))

           (shepherd-service
            (documentation "Start the RPC GSS daemon.")
            (requirement '(user-processes rpcbind-daemon rpc-pipefs))
            (provision '(gss-daemon))

            (start #~(make-forkexec-constructor #$gss-command))
            (stop #~(make-kill-destructor))))))
    (service-type
     (name 'gss)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values)
               (match values
                 ((first . rest) first)
                 (_ config))))
     (default-value (gss-configuration))
     (description "Run the @dfn{global security system} (GSS) daemon, which
provides strong security for protocols based on remote procedure calls (ONC
RPC)."))))



(define-record-type* <idmap-configuration>
  idmap-configuration make-idmap-configuration
  idmap-configuration?
  (pipefs-directory      idmap-configuration-pipefs-directory
                         (default default-pipefs-directory))
  (domain                idmap-configuration-domain
                         (default #f))
  (nfs-utils             idmap-configuration-nfs-utils
                         (default nfs-utils))
  (verbosity             idmap-configuration-verbosity
                         (default 0)))

(define idmap-service-type
  (let ((proc
         (lambda (config)

           (define nfs-utils
             (idmap-configuration-nfs-utils config))

           (define pipefs-directory
             (idmap-configuration-pipefs-directory config))

           (define domain (idmap-configuration-domain config))

           (define (idmap-config-file config)
             (plain-file "idmapd.conf"
                         (string-append
                          "\n[General]\n"
                          "Verbosity = "
                          (number->string
                           (idmap-configuration-verbosity config))
                          "\n"
                          (if domain
                              (format #f "Domain = ~a\n" domain)
                              "")
                          "\n[Mapping]\n"
                          "Nobody-User = nobody\n"
                          "Nobody-Group = nogroup\n")))

           (define idmap-command
             #~(list (string-append #$nfs-utils "/sbin/rpc.idmapd") "-f"
                     "-p" #$pipefs-directory
                     ;; TODO: this is deprecated
                     "-c" #$(idmap-config-file config)))

           (shepherd-service
            (documentation "Start the RPC IDMAP daemon.")
            (requirement '(user-processes rpcbind-daemon rpc-pipefs))
            (provision '(idmap-daemon))
            (start #~(make-forkexec-constructor #$idmap-command))
            (stop #~(make-kill-destructor))))))
    (service-type
     (name 'idmap)
     (extensions
      (list (service-extension shepherd-root-service-type
                               (compose list proc))))
     ;; We use the extensions feature to allow other services to automatically
     ;; configure and start this service.  Only one value can be provided.  We
     ;; override it with the value returned by the extending service.
     (compose identity)
     (extend (lambda (config values) (first values)))
     (default-value (idmap-configuration))
     (description "Run the idmap daemon, which provides a mapping between user
IDs and user names.  It is typically required to access file systems mounted
via NFSv4."))))

(define-record-type* <nfs-configuration>
  nfs-configuration make-nfs-configuration
  nfs-configuration?
  (nfs-utils           nfs-configuration-nfs-utils
                       (default nfs-utils))
  (nfs-versions        nfs-configuration-nfs-versions
                       (default '("4.2" "4.1" "4.0")))
  (exports             nfs-configuration-exports
                       (default '()))
  (rpcmountd-port      nfs-configuration-rpcmountd-port
                       (default #f))
  (rpcstatd-port       nfs-configuration-rpcstatd-port
                       (default #f))
  (rpcbind             nfs-configuration-rpcbind
                       (default rpcbind))
  (idmap-domain        nfs-configuration-idmap-domain
                       (default "localdomain"))
  (nfsd-port           nfs-configuration-nfsd-port
                       (default 2049))
  (nfsd-threads        nfs-configuration-nfsd-threads
                       (default 8))
  (nfsd-tcp?           nfs-configuration-nfsd-tcp?
                       (default #t))
  (nfsd-udp?           nfs-configuration-nfsd-udp?
                       (default #f))
  (pipefs-directory    nfs-configuration-pipefs-directory
                       (default default-pipefs-directory))
  ;; List of modules to debug; any of nfsd, nfs, rpc, idmap, statd, or mountd.
  (debug               nfs-configuration-debug
                       (default '())))

(define (nfs-shepherd-services config)
  "Return a list of <shepherd-service> for the NFS daemons with CONFIG."
  (match-record config <nfs-configuration>
    (nfs-utils nfs-versions exports
               rpcmountd-port rpcstatd-port nfsd-port nfsd-threads
               nfsd-tcp? nfsd-udp?
               pipefs-directory debug)
    (list (shepherd-service
           (documentation "Mount the nfsd pseudo file system.")
           (provision '(/proc/fs/nfsd))
           (start #~(lambda ()
                      (mount "nfsd" "/proc/fs/nfsd" "nfsd")
                      (member "/proc/fs/nfsd" (mount-points))))

           (stop #~(lambda (pid . args)
                     (umount "/proc/fs/nfsd" MNT_DETACH)
                     (not (member "/proc/fs/nfsd" (mount-points))))))
          (shepherd-service
           (documentation "Run the NFS statd daemon.")
           (provision '(rpc.statd))
           (requirement '(/proc/fs/nfsd rpcbind-daemon))
           (start
            #~(make-forkexec-constructor
               (list #$(file-append nfs-utils "/sbin/rpc.statd")
                     ;; TODO: notification support may require a little more
                     ;; configuration work.
                     "--no-notify"
                     #$@(if (member 'statd debug)
                            '("--no-syslog") ; verbose logging to stderr
                            '())
                     "--foreground"
                     #$@(if rpcstatd-port
                            #~("--port" #$(number->string rpcstatd-port))
                            '()))
               #:pid-file "/var/run/rpc.statd.pid"))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (documentation "Run the NFS mountd daemon.")
           (provision '(rpc.mountd))
           (requirement '(/proc/fs/nfsd rpc.statd))
           (start
            #~(make-forkexec-constructor
               (list #$(file-append nfs-utils "/sbin/rpc.mountd")
                     "--foreground"
                     #$@(if (member 'mountd debug)
                            '("--debug" "all")
                            '())
                     #$@(if rpcmountd-port
                            #~("--port" #$(number->string rpcmountd-port))
                            '()))))
           (stop #~(make-kill-destructor)))
          (shepherd-service
           (documentation "Run the NFS daemon.")
           (provision '(rpc.nfsd))
           (requirement '(/proc/fs/nfsd rpc.statd networking))
           (start
            #~(lambda _
                (zero? (apply system* #$(file-append nfs-utils "/sbin/rpc.nfsd")
                              (list
                               #$@(if (member 'nfsd debug)
                                      '("--debug")
                                      '())
                               "--port" #$(number->string nfsd-port)
                               #$@(map (lambda (version)
                                         (string-append "--nfs-version=" version))
                                       nfs-versions)
                               #$(number->string nfsd-threads)
                               #$(if nfsd-tcp?
                                     "--tcp"
                                     "--no-tcp")
                               #$(if nfsd-udp?
                                     "--udp"
                                     "--no-udp"))))))
           (stop
            #~(lambda _
                (zero?
                 (system* #$(file-append nfs-utils "/sbin/rpc.nfsd") "0")))))
          (shepherd-service
           (documentation "Run the NFS mountd daemon and refresh exports.")
           (provision '(nfs))
           (requirement '(/proc/fs/nfsd rpc.nfsd rpc.mountd rpc.statd rpcbind-daemon))
           (start
            #~(lambda _
                (let ((rpcdebug #$(file-append nfs-utils "/sbin/rpcdebug")))
                  (cond
                   ((member 'nfsd '#$debug)
                    (system* rpcdebug "-m" "nfsd" "-s" "all"))
                   ((member 'nfs '#$debug)
                    (system* rpcdebug "-m" "nfs" "-s" "all"))
                   ((member 'rpc '#$debug)
                    (system* rpcdebug "-m" "rpc" "-s" "all"))))
                (zero? (system*
                        #$(file-append nfs-utils "/sbin/exportfs")
                        "-r"            ; re-export
                        "-a"            ; everthing
                        "-v"            ; be verbose
                        "-d" "all"      ; debug
                        ))))
           (stop
            #~(lambda _
                (let ((rpcdebug #$(file-append nfs-utils "/sbin/rpcdebug")))
                  (cond
                   ((member 'nfsd '#$debug)
                    (system* rpcdebug "-m" "nfsd" "-c" "all"))
                   ((member 'nfs '#$debug)
                    (system* rpcdebug "-m" "nfs" "-c" "all"))
                   ((member 'rpc '#$debug)
                    (system* rpcdebug "-m" "rpc" "-c" "all"))))
                #t))
           (respawn? #f)))))

(define %nfs-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        ;; directory containing monitor list
        (mkdir-p "/var/lib/nfs/sm")
        ;; Needed for client recovery tracking
        (mkdir-p "/var/lib/nfs/v4recovery")
        (let ((user (getpw "nobody")))
          (chown "/var/lib/nfs"
                 (passwd:uid user)
                 (passwd:gid user))
          (chown "/var/lib/nfs/v4recovery"
                 (passwd:uid user)
                 (passwd:gid user)))
        #t)))

(define nfs-service-type
  (service-type
   (name 'nfs)
   (extensions
    (list
     (service-extension shepherd-root-service-type nfs-shepherd-services)
     (service-extension activation-service-type (const %nfs-activation))
     (service-extension etc-service-type
                        (lambda (config)
                          `(("exports"
                             ,(plain-file "exports"
                                          (string-join
                                           (map string-join
                                                (nfs-configuration-exports config))
                                           "\n"))))))
     ;; The NFS service depends on these other services.  They are extended so
     ;; that users don't need to configure them manually.
     (service-extension idmap-service-type
                        (lambda (config)
                          (idmap-configuration
                           (domain (nfs-configuration-idmap-domain config))
                           (verbosity
                            (if (member 'idmap (nfs-configuration-debug config))
                                10 0))
                           (pipefs-directory (nfs-configuration-pipefs-directory config))
                           (nfs-utils (nfs-configuration-nfs-utils config)))))
     (service-extension pipefs-service-type
                        (lambda (config)
                          (pipefs-configuration
                           (mount-point (nfs-configuration-pipefs-directory config)))))
     (service-extension rpcbind-service-type
                        (lambda (config)
                          (rpcbind-configuration
                           (rpcbind (nfs-configuration-rpcbind config)))))))
   (description
    "Run all NFS daemons and refresh the list of exported file systems.")))

 ;; Autofs

(define %autofs-pid-file "/var/run/autofs.pid")

(define (value->string _ value)
  (object->string value display))

(define (option-flag? value)
  "Is @var{value} a mount option flag?
Option flags are value like @var{ro}, @var{noatime}, @var{nosuid}, etc."
  (or (string? value)
      (symbol? value)))

(define (option-value? value)
  (or (option-flag? value)
      (integer? value)))

(define (option-pair? value)
  "Is @var{value} an option pair?
Option pairs are cons cells of (option-flag . option-value), used for
mount options like @{var errors=remount-ro}, @var{timeo=600}, etc."
  (match value
    (((? option-flag? _) . (? option-value? _)) #t)
    (_ #f)))

(define (serialize-option-pair name value)
  (match value
    ((option-flag . option-value)
     (string-append (value->string name option-flag)
                    "="
                    (value->string name option-value)))))

(define (file-system-option? value)
  (or (option-flag? value)
      (option-pair? value)))

(define (serialize-file-system-option name value)
  (cond
   ((option-flag? value) (value->string name value))
   ((option-pair? value) (serialize-option-pair name value))))

(define (file-system-options? value)
  (list-of file-system-option?))

(define (serialize-file-system-options name value)
  (string-join (map (cut serialize-file-system-option name <>) value) ","))

(define-configuration autofs-map-entry
  (type (string "auto")
        "The type of the filesystem."
        (serializer value->string))
  (device string
          "Device or remote host to mount.  May contain special
character @code{&}, which can be referenced in the @var{mount-point}
field."
          (serializer value->string))
  (mount-point string
               "Directory to mount this device on.

Map entries come in two flavors: direct and indirect.  Direct entries
map a single device to a single mountpoint, while indirect entries can
map multiple devices to multiple mountpoints.

A direct entry has a @var{mount-point} beginning with @code{/}, representing
the absolute path of the directory to mount the device on.  For example:

    (autofs-map-entry
     (type \"ext4\")
     (device \"/dev/sdb1\")
     (mount-point \"/mnt/external-disk\"))

An indirect entry has a @var{mount-point} not beginning with @code{/},
representing the subdirectory within the parent indirect map for this
entry.  Indirect maps may also use the special character @code{*},
which will be replaced with the value of special character @code{&} in
the @var{device} field of this entry.  For example:

    (autofs-indirect-map
      (mount-point \"/devices\")
      (entries
        (list
          ;; Automount any block device r/o by ID.
          (autofs-map-entry
            (type \"auto\")
            (mount-point \"ro/uuid/*\")
            (device \"/dev/disk/by-id/&\")
            (options '(ro)))
          ;; Automount any block device by UUID.
          (autofs-map-entry
            (type \"auto\")
            (mount-point \"rw/uuid/*\")
            (device \"/dev/disk/by-uuid/&\")))))"
               (serializer value->string))
  (options (file-system-options '())
           "List of mount options.

Some options are simple flags, such as ro, noexec, nosuid, etc.  These
may be expressed as strings or symbols.

Other options also accept a value.  These are expressed as pairs of
@code{(option . value)}.  @code{option} may be a string or symbol, as
with flags.  @code{value} may be a string, symbol, or number.

Example: @code{(ro (errors . remount-ro) noexec)}"))

(define (serialize-autofs-map-entry _ value)
  (let ((all-options
         (serialize-file-system-options
          #f
          `((fstype . ,(autofs-map-entry-type value))
            ,@(autofs-map-entry-options value)))))
    (string-join (list (autofs-map-entry-mount-point value)
                       (string-append "-" all-options)
                       (value->string #f (autofs-map-entry-device value)))
                 " ")))

(define autofs-map-entries? (list-of autofs-map-entry?))

(define (serialize-autofs-map-entries name value)
  (string-join
   (map (cut serialize-autofs-map-entry name <>) value)
   "\n"
   'suffix))

(define-configuration autofs-indirect-map
  (mount-point string "Where to mount the indirect map."
               (serializer value->string))
  (entries (autofs-map-entries '()) "Entries in this map."))

(define (autofs-serialize-indirect-map name value)
  (serialize-autofs-map-entries name (autofs-indirect-map-entries value)))

(define (autofs-direct-mount-point? mount-point)
  (string= "/" (substring mount-point 0 1)))

(define (autofs-direct-map? value)
  (and (autofs-map-entry? value)
       (autofs-direct-mount-point? (autofs-map-entry-mount-point value))))

(define (mount-map? value)
  (or (autofs-direct-map? value)
      (autofs-indirect-map? value)))

(define (mount-maps? value)
  (list-of mount-map?))

(define (scheme->autofs name-sym)
  (string-replace-substring
   (string-replace-substring (symbol->string name-sym) "-" "_")
   "?" ""))

(define (autofs-serialize-integer name value)
  (format #f "~a = ~a" (scheme->autofs name) value))

(define (autofs-serialize-boolean name value)
  (format #f "~a = ~a" (scheme->autofs name) (if value "yes" "no")))

(define (log-level? value)
  (and (keyword? value)
       (member value '(#:none #:verbose #:debug))))

(define (autofs-serialize-log-level name value)
  (format #f "~a = ~a" (scheme->autofs name)
          (symbol->string (keyword->symbol value))))

(define-configuration autofs-configuration
  (autofs
   (package autofs)
   "The autofs package to use."
   (serializer empty-serializer))

  (mounts
   (mount-maps '())
   "Mount maps to manage.

This is a list of either direct map entries or indirect mount maps."
   (serializer empty-serializer))

  (timeout
   (integer 300)
   "Sets the default mount timeout in seconds.")

  (master-wait
   (integer 10)
   "Sets the default maximum number of retries (actual iterations is half
this, each is delayed by 2 seconds before retrying) waiting for the master map
to become available if it cannot be read at program start.  This can be longer
if the map source itself waits for availability (such as sss).")

  (negative-timeout
   (integer 60)
   "Set the default timeout for caching failed key lookups.")

  (mount-verbose?
   (boolean #f)
   "Use the verbose flag when spawning mount(8), and log some process info
about the requestor and its parent.")

  (mount-wait
   (integer -1)
   "Set the default time to wait for a response from a spawned mount(8) before
sending it a SIGTERM.  Note that we still need to wait for the RPC layer to
timeout before the sub-process exits so this isn't ideal but it is the best we
can do. The default is to wait until mount(8) returns without intervention.")

  (umount-wait
   (integer 12)
   "Set the default time to wait for a response from a spawned umount(8)
before sending it a SIGTERM. Note that we still need to wait for the RPC layer
to timeout before the sub-process exits so this isn't ideal but it is the best
we can do.")

  (browse-mode?
   (boolean #t)
   "Should maps be browsable by default?")

  (mount-nfs-default-protocol
   (integer 3)
   "Set the default protocol that mount.nfs(8) uses when performing a mount.
Autofs needs to know the default NFS protocol that mount.nfs(8) uses so it can
do special case handling for its availability probe for different NFS
protocols.  Since we can't identify this default automatically we need to set
it in the autofs configuration.")

  (append-options?
   (boolean #t)
   "When #t, global options are appended to map entry options.  When #f, map
entry options replace the global options.")

  (logging
   (log-level #:none)
   "Default log level.  May be #:none, #:verbose, or #:debug.")

  (force-standard-program-map-env?
   (boolean #f)
   "Override the use of a prefix with standard environment variables when a
program map is executed. Since program maps are run as the privileged user
setting these standard environment variables opens automount(8) to potential
user privilege escalation when the program map is written in a language that
can load components from, for example, a user home directory.")

  (map-hash-table-size
   (integer 1024)
   "This configuration option may be used to change the number of hash table
slots.

This configuration option affects the overhead of searching the map entry
cache for map entries when there are a large number of entries.  It affects
the number of entries that must be looked at to locate a map entry in the map
entry cache. For example, the default of 1024 and a direct map with 8000
entries would result in each slot containing an average of 8 entries, which
should be acceptable.

However, if excessive CPU usage is observed during automount lookups
increasing this option can reduce the CPU overhead considerably because it
reduces the length of the search chains.

Note that the number of entries in a map doesn't necessarily relate to the
number of entries used in the map entry cache.

There are two distinct cases where the map hash table can make a significant
difference, direct maps and indirect maps that use the \"browse\" option.

For indirect maps that do not use the \"browse\" option entries are added to
the map entry cache at lookup so the number of active cache entries, in this
case, is usually much less than the number of entries in the map.  In this
last case it would be unusual for the map entry cache to grow large enough to
warrant increasing the default before an event that cleans stale entries, a
map re-read for example.")

  (use-hostname-for-mounts?
   (boolean #f)
   "NFS mounts where the host name resolves to more than one IP address are
probed for availability and to establish the order in which mounts to them
should be tried.  To ensure that mount attempts are made only to hosts that
are responding and are tried in the order of hosts with the quickest response
the IP address of the host needs to be used for the mount.

If it is necessary to use the hostname given in the map entry for the mount
regardless, then set this option to #t.

Be aware that if this is done there is no defense against the host name
resolving to one that isn't responding and while the number of attempts at a
successful mount will correspond to the number of addresses the host name
resolves to the order will also not correspond to fastest responding hosts.")

  (disable-not-found-message?
   (boolean #f)
   "The original request to add this log message needed it to be
unconditional.  That produces, IMHO, unnecessary noise in the log so a
configuration option has been added to provide the ability to turn it off.")

  (use-ignore-mount-option?
   (boolean #f)
   "An option to enable the use of autofs pseudo option \"disable\".  This
option is used as a hint to user space that the mount entry should be omitted
from mount table listings. The default is #f to avoid unexpected changes in
behaviour and so is an opt-in setting.")

  (sss-master-map-wait
   (integer 0)
   "Set the time to wait and retry if sssd is unable to read the master map at
program start.  Default is 0 (don't wait) or 10 if sss supports returning
EHSTDOWN when the provider isn't available.

If the sss library upports returning EHOSTDOWN when the provider is down then
this value is how long to wait between re‐ tries reading the master map.  When
reading dependent maps or looking up a map key this value is multiplied by the
number of retries that would be used when reading the master map.")

  (use-mount-request-log-id?
   (boolean #f)
   "Set whether to use a mount request log id so that log entries for specific
mount requests can be easily identified in logs that have multiple concurrent
requests.")
  (prefix autofs-))

(define (indirect-map->file-name indirect-map)
  (string-append
   (string-replace-substring
    (substring (autofs-indirect-map-mount-point indirect-map) 1)
    "/" "-") ".map"))

(define (autofs-build-config config)
  (let* ((mounts (autofs-configuration-mounts config))
         (autofs-conf (list-transduce (base-transducer config) rcons
                                      autofs-configuration-fields))
         ;; List of serialized direct maps.
         (direct-maps
          (serialize-autofs-map-entries
           #f (filter autofs-direct-map? mounts)))

         ;; List of (file-name mount-point serialized-map).
         (indirect-maps
          (map
           (lambda (indirect-map)
             (list (indirect-map->file-name indirect-map)
                   (autofs-indirect-map-mount-point indirect-map)
                   (autofs-serialize-indirect-map #f indirect-map)))
           (filter autofs-indirect-map? mounts))))
    (computed-file
     "autofs-config"
     (with-imported-modules
         (source-module-closure '((guix build utils) (ice-9 match)))
       #~(begin
           (use-modules (guix build utils) (ice-9 match))

           (mkdir-p #$output)

           (call-with-output-file (string-append #$output "/autofs.conf")
             (lambda (autofs-conf)
               ;; Write out the serialized config.
               (display (string-join
                         (cons*
                          "[ autofs ]"
                          (string-append "master_map_name = "
                                         #$output "/auto.master")
                          '#$autofs-conf)
                         "\n" 'suffix)
                        autofs-conf)))

           ;; Write out the master map.
           (call-with-output-file (string-append #$output "/auto.master")
             (lambda (master-map)
               ;; Write the direct entries to the master map.
               (display '#$direct-maps master-map)

               ;; Write indirect maps to their own files.
               (for-each
                (match-lambda
                  ((file-name mount-point content)
                   ;; Write the indirect map.
                   (call-with-output-file
                       (string-append #$output "/" file-name)
                     (lambda (indirect-map)
                       (display content indirect-map)))
                   ;; Reference it in the master map.
                   (format master-map "~a ~a/~a~%"
                           mount-point #$output file-name)))
                '#$indirect-maps))))))))

(define (autofs-activation config)
  (let* ((config-dir (autofs-build-config config))
         (autofs-conf (file-append config-dir "/autofs.conf"))
         (conf-link "/etc/autofs.conf")
         (no-mounts? (null? (autofs-configuration-mounts config)))
         (direct-mount-points
          (map
           autofs-map-entry-mount-point
           (filter autofs-direct-map?
                   (autofs-configuration-mounts config))))
         (indirect-mount-points
          (map
           autofs-indirect-map-mount-point
           (filter
            autofs-indirect-map?
            (autofs-configuration-mounts config))))
         (mount-points (append direct-mount-points indirect-mount-points)))
    #~(begin
        (use-modules (guix build utils))
        (for-each mkdir-p (cons "/var/lib/nfs/sm" '#$mount-points))
        (when (or (false-if-exception (lstat #$conf-link))
                  (stat #$conf-link #f))
          (delete-file #$conf-link))
        (unless #$no-mounts?
          (symlink #$autofs-conf #$conf-link)))))

(define (autofs-configuration->raw-entries config)
  (fold
   (lambda (mount acc)
     (cond
      ((autofs-direct-map? mount)
       (cons mount acc))
      ((autofs-indirect-map? mount)
       (append (autofs-indirect-map-entries mount) acc))))
   '()
   (autofs-configuration-mounts config)))

(define (autofs-configuration->requirements config)
  "Compute Shepherd service requirements for @var{config}.

If @var{config} contains NFS mounts, adds rpc.statd to the service
requirements.

If @var{config} contains SMB mounts, adds samba-nmbd and samba-winbindd to the
service requirements.
"
  (delete-duplicates
   (fold
    (lambda (fs-type acc)
      (cond
       ((string= "nfs" fs-type)
        (append acc '(rpc.statd)))
       ((string= "smb" fs-type)
        (append acc '(samba-nmbd samba-winbindd)))))
    '()
    (map autofs-map-entry-type (autofs-configuration->raw-entries config)))))

(define (autofs-shepherd-service config)
  (match-record config <autofs-configuration> (autofs timeout)
    (begin
      (define autofs-command
        ;; Autofs doesn't let us specify the config file, so we rely on
        ;; autofs-activation linking /etc/autofs.conf to the store.
        #~(list
           #$(file-append autofs "/sbin/automount")
           "-f"
           "-t" (number->string #$timeout)
           "-p" #$%autofs-pid-file))

      (list
       (shepherd-service
        (provision '(autofs automount))
        (documentation "Run the autofs daemon.")
        (requirement (autofs-configuration->requirements config))
        (start
         #~(make-forkexec-constructor
            #$autofs-command
            #:pid-file #$%autofs-pid-file))
        (stop #~(make-kill-destructor)))))))

(define (autofs-configuration-merge a b)
  (autofs-configuration
   (inherit b)
   (mounts (append (autofs-configuration-mounts a)
                   (autofs-configuration-mounts b)))))

(define-public autofs-service-type
  (service-type
   (name 'autofs)
   (description "Run autofs")
   (extensions
    (list
     (service-extension activation-service-type
                        autofs-activation)
     (service-extension shepherd-root-service-type
                        autofs-shepherd-service)))
   (compose
    (cut reduce autofs-configuration-merge (autofs-configuration) <>))
   (default-value (autofs-configuration))))
