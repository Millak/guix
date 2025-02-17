;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2023 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2025 Zacchaeus <eikcaz@zacchae.us>
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

(define-module (gnu services syncthing)
  #:use-module (gnu packages syncthing)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (sxml simple)
  #:export (syncthing-configuration
            syncthing-configuration?
            syncthing-device
            syncthing-device?
            syncthing-config-file
            syncthing-config-file?
            syncthing-folder-device
            syncthing-folder-device?
            syncthing-folder
            syncthing-folder?
            syncthing-service-type
            syncthing-shepherd-service
            syncthing-files-service))

;;; Commentary:
;;;
;;; This module provides a service definition for the syncthing service.
;;;
;;; Code:

(define (bool->xml-string bool)
  ;; add compatibility for the short duration where #f was specified as
  ;; "false"
  (if (string? bool) bool
      ;; the expected case
      (if bool "true" "false")))

(define-record-type* <syncthing-device>
  syncthing-device make-syncthing-device
  syncthing-device?
  (id syncthing-device-id)
  (name syncthing-device-name (default ""))
  (compression syncthing-device-compression (default "metadata"))
  (introducer? syncthing-device-introducer? (default #f))
  (skip-introduction-removals? syncthing-device-skip-introduction-removals? (default #f))
  (introduced-by syncthing-device-introduced-by (default ""))
  (addresses syncthing-device-addresses (default '("dynamic")))
  (paused? syncthing-device-paused? (default #f))
  (auto-accept-folders? syncthing-device-auto-accept-folders? (default #f))
  (max-send-kbps syncthing-device-max-send-kbps (default 0))
  (max-receive-kbps syncthing-device-max-receive-kbps (default 0))
  (max-request-kib syncthing-device-max-request-kib (default 0))
  (untrusted? syncthing-device-untrusted? (default #f))
  (remote-gui-port syncthing-device-remote-gui-port (default 0))
  (num-connections syncthing-device-num-connections (default 0)))

(define syncthing-device->sxml
  (match-record-lambda <syncthing-device>
      (id
       name compression introducer? skip-introduction-removals? introduced-by
       addresses paused? auto-accept-folders? max-send-kbps max-receive-kbps
       max-request-kib untrusted? remote-gui-port num-connections)
    `(device (@ (id ,id)
                (name ,name)
                (compression ,compression)
                (introducer ,(bool->xml-string introducer?))
                (skipIntroductionRemovals ,(bool->xml-string skip-introduction-removals?))
                (introducedBy ,introduced-by))
             ,@(map (lambda (address) `(address ,address)) addresses)
             (paused ,(bool->xml-string paused?))
             (autoAcceptFolders ,(bool->xml-string auto-accept-folders?))
             (maxSendKbps ,max-send-kbps)
             (maxRecvKbps ,max-receive-kbps)
             (maxRequestKiB ,max-request-kib)
             (untrusted ,(bool->xml-string untrusted?))
             (remoteGUIPort ,remote-gui-port)
             (numConnections ,num-connections))))

(define-record-type* <syncthing-folder-device>
  syncthing-folder-device make-syncthing-folder-device
  syncthing-folder-device?
  (device syncthing-folder-device-device)
  (introduced-by syncthing-folder-device-introduced-by (default (syncthing-device (id ""))))
  (encryption-password syncthing-folder-device-encryption-password (default "")))

(define syncthing-folder-device->sxml
  (match-record-lambda <syncthing-folder-device>
      (device introduced-by encryption-password)
    `(device (@ (id ,(syncthing-device-id device))
                (introducedBy ,(syncthing-device-id introduced-by)))
             (encryptionPassword ,encryption-password))))

(define-record-type* <syncthing-folder>
  syncthing-folder make-syncthing-folder
  syncthing-folder?
  (id syncthing-folder-id (default #f))
  (label syncthing-folder-label)
  (path syncthing-folder-path)
  (type syncthing-folder-type (default "sendreceive"))
  (rescan-interval-seconds syncthing-folder-rescan-interval-seconds (default 3600))
  (file-system-watcher-enabled? syncthing-folder-file-system-watcher-enabled? (default #t))
  (file-system-watcher-delay-seconds syncthing-folder-file-system-watcher-delay-seconds (default 10))
  (file-system-watcher-timeout-seconds syncthing-folder-file-system-watcher-timeout-seconds (default 0))
  (ignore-permissions? syncthing-folder-ignore-permissions? (default #f))
  (auto-normalize? syncthing-folder-auto-normalize? (default #t))
  (devices syncthing-folder-devices (default '())
           (sanitize (lambda (folder-device-list)
                       (map (lambda (device)
                              (if (syncthing-folder-device? device)
                                  device
                                  (syncthing-folder-device (device device))))
                            folder-device-list))))
  (filesystem-type syncthing-folder-filesystem-type (default "basic"))
  (min-disk-free-unit syncthing-folder-min-disk-free-unit (default "%"))
  (min-disk-free syncthing-folder-min-disk-free (default 1))
  (versioning-type syncthing-folder-versioning-type (default #f))
  (versioning-file-system-path syncthing-folder-versioning-file-system-path (default ""))
  (versioning-file-system-type syncthing-folder-versioning-file-system-type (default "basic"))
  (versioning-cleanup-interval-seconds syncthing-folder-versioning-cleanup-interval-seconds (default 3600))
  (versioning-cleanout-days syncthing-folder-versioning-cleanout-days (default #f))
  (versioning-keep syncthing-folder-versioning-keep (default #f))
  (versioning-max-age syncthing-folder-versioning-max-age (default #f))
  (versioning-command syncthing-folder-versioning-command (default #f))
  (copiers syncthing-folder-copiers (default 0))
  (puller-max-pending-kib syncthing-folder-puller-max-pending-kib (default 0))
  (hashers syncthing-folder-hashers (default 0))
  (order syncthing-folder-order (default "random"))
  (ignore-delete? syncthing-folder-ignore-delete? (default #f))
  (scan-progress-interval-seconds syncthing-folder-scan-progress-interval-seconds (default 0))
  (puller-pause-seconds syncthing-folder-puller-pause-seconds (default 0))
  (max-conflicts syncthing-folder-max-conflicts (default 10))
  (disable-sparse-files? syncthing-folder-disable-sparse-files? (default #f))
  (disable-temp-indexes? syncthing-folder-disable-temp-indexes? (default #f))
  (paused? syncthing-folder-paused? (default #f))
  (weak-hash-threshold-percentage syncthing-folder-weak-hash-threshold-percentage (default 25))
  (marker-name syncthing-folder-marker-name (default ".stfolder"))
  (copy-ownership-from-parent? syncthing-folder-copy-ownership-from-parent? (default #f))
  (mod-time-window-seconds syncthing-folder-mod-time-window-seconds (default 0))
  (max-concurrent-writes syncthing-folder-max-concurrent-writes (default 2))
  (disable-fsync? syncthing-folder-disable-fsync? (default #f))
  (block-pull-order syncthing-folder-block-pull-order (default "standard"))
  (copy-range-method syncthing-folder-copy-range-method (default "standard"))
  (case-sensitive-file-system? syncthing-folder-case-sensitive-file-system? (default #f))
  (junctions-as-dirs? syncthing-folder-junctions-as-dirs? (default #f))
  (sync-ownership? syncthing-folder-sync-ownership? (default #f))
  (send-ownership? syncthing-folder-send-ownership? (default #f))
  (sync-xattrs? syncthing-folder-sync-xattrs? (default #f))
  (send-xattrs? syncthing-folder-send-xattrs? (default #f))
  (xattr-filter-max-single-entry-size syncthing-folder-xattr-filter-max-single-entry-size (default 1024))
  (xattr-filter-max-total-size syncthing-folder-xattr-filter-max-total-size (default 4096)))

;; Some parameters, when empty, are fully omitted from the config file.  It is
;; unknown if this causes a functional difference, but stick to the normal
;; program's behavior to be safe.
(define (maybe-param symbol value)
  (if value `((param (@ (key ,(symbol->string symbol)) (val ,value)) "")) '()))

(define syncthing-folder->sxml
  (match-record-lambda <syncthing-folder>
      (id
       label path type rescan-interval-seconds file-system-watcher-enabled?
       file-system-watcher-delay-seconds file-system-watcher-timeout-seconds
       ignore-permissions? auto-normalize? devices filesystem-type
       min-disk-free-unit min-disk-free versioning-type
       versioning-file-system-path versioning-file-system-type
       versioning-cleanup-interval-seconds versioning-cleanout-days
       versioning-keep versioning-max-age versioning-command copiers
       puller-max-pending-kib hashers order ignore-delete?
       scan-progress-interval-seconds puller-pause-seconds max-conflicts
       disable-sparse-files? disable-temp-indexes? paused?
       weak-hash-threshold-percentage marker-name copy-ownership-from-parent?
       mod-time-window-seconds max-concurrent-writes disable-fsync?
       block-pull-order copy-range-method case-sensitive-file-system?
       junctions-as-dirs? sync-ownership? send-ownership? sync-xattrs?
       send-xattrs? xattr-filter-max-single-entry-size
       xattr-filter-max-total-size)
    `(folder (@ (id ,(if id id label))
                (label ,label)
                (path ,path)
                (type ,type)
                (rescanIntervalS ,rescan-interval-seconds)
                (fsWatcherEnabled ,(bool->xml-string file-system-watcher-enabled?))
                (fsWatcherDelayS ,file-system-watcher-delay-seconds)
                (fsWatcherTimeoutS ,file-system-watcher-timeout-seconds)
                (ignorePerms ,(bool->xml-string ignore-permissions?))
                (autoNormalize ,(bool->xml-string auto-normalize?)))
             (filesystemType ,filesystem-type)
             ,@(map syncthing-folder-device->sxml
                    devices)
             (minDiskFree (@ (unit ,min-disk-free-unit))
                          ,min-disk-free)
             (versioning ,@(if versioning-type
                               `((@ (type ,versioning-type)))
                               '())
                         ,@(maybe-param 'cleanoutDays versioning-cleanout-days)
                         ,@(maybe-param 'keep versioning-keep)
                         ,@(maybe-param 'maxAge versioning-max-age)
                         ,@(maybe-param 'command versioning-command)
                         (cleanupIntervalS ,versioning-cleanup-interval-seconds)
                         (fsPath ,versioning-file-system-path)
                         (fsType ,versioning-file-system-type))
             (copiers ,copiers)
             (pullerMaxPendingKiB ,puller-max-pending-kib)
             (hashers ,hashers)
             (order ,order)
             (ignoreDelete ,(bool->xml-string ignore-delete?))
             (scanProgressIntervalS ,scan-progress-interval-seconds)
             (pullerPauseS ,puller-pause-seconds)
             (maxConflicts ,max-conflicts)
             (disableSparseFiles ,(bool->xml-string disable-sparse-files?))
             (disableTempIndexes ,(bool->xml-string disable-temp-indexes?))
             (paused ,(bool->xml-string paused?))
             (weakHashThresholdPct ,weak-hash-threshold-percentage)
             (markerName ,marker-name)
             (copyOwnershipFromParent ,(bool->xml-string copy-ownership-from-parent?))
             (modTimeWindowS ,mod-time-window-seconds)
             (maxConcurrentWrites ,max-concurrent-writes)
             (disableFsync ,(bool->xml-string disable-fsync?))
             (blockPullOrder ,block-pull-order)
             (copyRangeMethod ,copy-range-method)
             (caseSensitiveFS ,(bool->xml-string case-sensitive-file-system?))
             (junctionsAsDirs ,(bool->xml-string junctions-as-dirs?))
             (syncOwnership ,(bool->xml-string sync-ownership?))
             (sendOwnership ,(bool->xml-string send-ownership?))
             (syncXattrs ,(bool->xml-string sync-xattrs?))
             (sendXattrs ,(bool->xml-string send-xattrs?))
             (xattrFilter (maxSingleEntrySize ,xattr-filter-max-single-entry-size)
                          (maxTotalSize ,xattr-filter-max-total-size)))))

(define-record-type* <syncthing-config-file>
  syncthing-config-file make-syncthing-config-file
  syncthing-config-file?
  (folders syncthing-config-folders
           ; this matches syncthing's default
           (default (list (syncthing-folder (id "default")
                                            (label "Default Folder")
                                            (path "~/Sync")))))
  (devices syncthing-config-devices
           (default '()))
  (gui-enabled? syncthing-config-gui-enabled? (default #t))
  (gui-tls? syncthing-config-gui-tls? (default #f))
  (gui-debugging? syncthing-config-gui-debugging? (default #f))
  (gui-send-basic-authorization-prompt? syncthing-config-gui-send-basic-authorization-prompt? (default #f))
  (gui-address syncthing-config-gui-address (default "127.0.0.1:8384"))
  (gui-user syncthing-config-gui-user (default #f))
  (gui-password syncthing-config-gui-password (default #f))
  (gui-apikey syncthing-config-gui-apikey (default #f))
  (gui-theme syncthing-config-gui-theme (default "default"))
  (ldap-enabled? syncthing-config-ldap-enabled? (default #f))
  (ldap-address syncthing-config-ldap-address (default ""))
  (ldap-bind-dn syncthing-config-ldap-bind-dn (default ""))
  (ldap-transport syncthing-config-ldap-transport (default ""))
  (ldap-insecure-skip-verify syncthing-config-ldap-insecure-skip-verify (default ""))
  (ldap-search-base-dn syncthing-config-ldap-search-base-dn (default ""))
  (ldap-search-filter syncthing-config-ldap-search-filter (default ""))
  (listen-address syncthing-config-listen-address (default "default"))
  (global-announce-server syncthing-config-global-announce-server (default "default"))
  (global-announce-enabled? syncthing-config-global-announce-enabled? (default #t))
  (local-announce-enabled? syncthing-config-local-announce-enabled? (default #t))
  (local-announce-port syncthing-config-local-announce-port (default 21027))
  (local-announce-mac-address syncthing-config-local-announce-mac-address (default "[ff12::8384]:21027"))
  (max-send-kbps syncthing-config-max-send-kbps (default 0))
  (max-receive-kbps syncthing-config-max-receive-kbps (default 0))
  (reconnection-interval-seconds syncthing-config-reconnection-interval-seconds (default 60))
  (relays-enabled? syncthing-config-relays-enabled? (default #t))
  (relay-reconnect-interval-minutes syncthing-config-relay-reconnect-interval-minutes (default 10))
  (start-browser? syncthing-config-start-browser? (default #t))
  (nat-enabled? syncthing-config-nat-enabled? (default #t))
  (nat-lease-minutes syncthing-config-nat-lease-minutes (default 60))
  (nat-renewal-minutes syncthing-config-nat-renewal-minutes (default 30))
  (nat-timeout-seconds syncthing-config-nat-timeout-seconds (default 10))
  (usage-reporting-accepted syncthing-config-usage-reporting-accepted (default 0))
  (usage-reporting-seen syncthing-config-usage-reporting-seen (default 0))
  (usage-reporting-unique-id syncthing-config-usage-reporting-unique-id (default ""))
  (usage-reporting-url syncthing-config-usage-reporting-url (default "https://data.syncthing.net/newdata"))
  (usage-reporting-post-insecurely? syncthing-config-usage-reporting-post-insecurely? (default #f))
  (usage-reporting-initial-delay-seconds syncthing-config-usage-reporting-initial-delay-seconds (default 1800))
  (auto-upgrade-interval-hours syncthing-config-auto-upgrade-interval-hours (default 12))
  (upgrade-to-pre-releases? syncthing-config-upgrade-to-pre-releases? (default #f))
  (keep-temporaries-hours syncthing-config-keep-temporaries-hours (default 24))
  (cache-ignored-files? syncthing-config-cache-ignored-files? (default #f))
  (progress-update-interval-seconds syncthing-config-progress-update-interval-seconds (default 5))
  (limit-bandwidth-in-lan? syncthing-config-limit-bandwidth-in-lan? (default #f))
  (min-home-disk-free-unit syncthing-config-min-home-disk-free-unit (default "%"))
  (min-home-disk-free syncthing-config-min-home-disk-free (default 1))
  (releases-url syncthing-config-releases-url (default "https://upgrades.syncthing.net/meta.json"))
  (overwrite-remote-device-names-on-connect? syncthing-config-overwrite-remote-device-names-on-connect? (default #f))
  (temp-index-min-blocks syncthing-config-temp-index-min-blocks (default 10))
  (unacked-notification-id syncthing-config-unacked-notification-id (default "authenticationUserAndPassword"))
  (traffic-class syncthing-config-traffic-class (default 0))
  (set-low-priority? syncthing-config-set-low-priority? (default #t))
  (max-folder-concurrency syncthing-config-max-folder-concurrency (default 0))
  (crash-reporting-url syncthing-config-crash-reporting-url (default "https://crash.syncthing.net/newcrash"))
  (crash-reporting-enabled? syncthing-config-crash-reporting-enabled? (default #t))
  (stun-keepalive-start-seconds syncthing-config-stun-keepalive-start-seconds (default 180))
  (stun-keepalive-min-seconds syncthing-config-stun-keepalive-min-seconds (default 20))
  (stun-server syncthing-config-stun-server (default "default"))
  (database-tuning syncthing-config-database-tuning (default "auto"))
  (max-concurrent-incoming-request-kib syncthing-config-max-concurrent-incoming-request-kib (default 0))
  (announce-lan-addresses? syncthing-config-announce-lan-addresses? (default #t))
  (send-full-index-on-upgrade? syncthing-config-send-full-index-on-upgrade? (default #f))
  (connection-limit-enough syncthing-config-connection-limit-enough (default 0))
  (connection-limit-max syncthing-config-connection-limit-max (default 0))
  (insecure-allow-old-tls-versions? syncthing-config-insecure-allow-old-tls-versions? (default #f))
  (connection-priority-tcp-lan syncthing-config-connection-priority-tcp-lan (default 10))
  (connection-priority-quic-lan syncthing-config-connection-priority-quic-lan (default 20))
  (connection-priority-tcp-wan syncthing-config-connection-priority-tcp-wan (default 30))
  (connection-priority-quic-wan syncthing-config-connection-priority-quic-wan (default 40))
  (connection-priority-relay syncthing-config-connection-priority-relay (default 50))
  (connection-priority-upgrade-threshold syncthing-config-connection-priority-upgrade-threshold (default 0))
  (default-folder syncthing-config-default-folder
    (default (syncthing-folder (label "") (path "~"))))
  (default-device syncthing-config-default-device
    (default (syncthing-device (id ""))))
  (default-ignores syncthing-config-default-ignores (default "")))

(define syncthing-config-file->sxml
  (match-record-lambda <syncthing-config-file>
      (folders
       devices gui-enabled? gui-tls? gui-debugging?
       gui-send-basic-authorization-prompt? gui-address gui-user gui-password
       gui-apikey gui-theme ldap-enabled? ldap-address ldap-bind-dn
       ldap-transport ldap-insecure-skip-verify ldap-search-base-dn
       ldap-search-filter listen-address global-announce-server
       global-announce-enabled? local-announce-enabled? local-announce-port
       local-announce-mac-address max-send-kbps max-receive-kbps
       reconnection-interval-seconds relays-enabled?
       relay-reconnect-interval-minutes start-browser? nat-enabled?
       nat-lease-minutes nat-renewal-minutes nat-timeout-seconds
       usage-reporting-accepted usage-reporting-seen usage-reporting-unique-id
       usage-reporting-url usage-reporting-post-insecurely?
       usage-reporting-initial-delay-seconds auto-upgrade-interval-hours
       upgrade-to-pre-releases? keep-temporaries-hours cache-ignored-files?
       progress-update-interval-seconds limit-bandwidth-in-lan?
       min-home-disk-free-unit min-home-disk-free releases-url
       overwrite-remote-device-names-on-connect? temp-index-min-blocks
       unacked-notification-id traffic-class set-low-priority?
       max-folder-concurrency crash-reporting-url crash-reporting-enabled?
       stun-keepalive-start-seconds stun-keepalive-min-seconds stun-server
       database-tuning max-concurrent-incoming-request-kib
       announce-lan-addresses? send-full-index-on-upgrade?
       connection-limit-enough connection-limit-max
       insecure-allow-old-tls-versions? connection-priority-tcp-lan
       connection-priority-quic-lan connection-priority-tcp-wan
       connection-priority-quic-wan connection-priority-relay
       connection-priority-upgrade-threshold default-folder default-device
       default-ignores)
    `(configuration (@ (version "37"))
                    ,@(map syncthing-folder->sxml
                           folders)
                    ;; collect any devices in any folders, as well as any
                    ;; devices explicitly added.
                    ,@(map syncthing-device->sxml
                           (delete-duplicates
                            (append devices
                                    (apply append
                                           (map (lambda (folder)
                                                  (map syncthing-folder-device-device
                                                       (syncthing-folder-devices folder)))
                                                folders)))
                            ;; devices are the same if their id's are equal
                            (lambda (device1 device2)
                              (string= (syncthing-device-id device1)
                                       (syncthing-device-id device2)))))
                    (gui (@ (enabled ,(bool->xml-string gui-enabled?))
                            (tls ,(bool->xml-string gui-tls?))
                            (debugging ,(bool->xml-string gui-debugging?))
                            (sendBasicAuthPrompt ,(bool->xml-string gui-send-basic-authorization-prompt?)))
                         (address ,gui-address)
                         ,@(if gui-user `((user ,gui-user)) '())
                         ,@(if gui-password `((password ,gui-password)) '())
                         ,@(if gui-apikey `((apikey ,gui-apikey)) '())
                         (theme ,gui-theme))
                    (ldap ,(if ldap-enabled?
                               `((address ,ldap-address)
                                 (bindDN ,ldap-bind-dn)
                                 ,@(if ldap-transport
                                       `((transport ,ldap-transport))
                                       '())
                                 ,@(if ldap-insecure-skip-verify
                                       `((insecureSkipVerify ,ldap-insecure-skip-verify))
                                       '())
                                 ,@(if ldap-search-base-dn
                                       `((searchBaseDN ,ldap-search-base-dn))
                                       '())
                                 ,@(if ldap-search-filter
                                       `((searchFilter ,ldap-search-filter))
                                       '()))
                               ""))
                    (options (listenAddress ,listen-address)
                             (globalAnnounceServer ,global-announce-server)
                             (globalAnnounceEnabled ,(bool->xml-string global-announce-enabled?))
                             (localAnnounceEnabled ,(bool->xml-string local-announce-enabled?))
                             (localAnnouncePort ,local-announce-port)
                             (localAnnounceMCAddr ,local-announce-mac-address)
                             (maxSendKbps ,max-send-kbps)
                             (maxRecvKbps ,max-receive-kbps)
                             (reconnectionIntervalS ,reconnection-interval-seconds)
                             (relaysEnabled ,(bool->xml-string relays-enabled?))
                             (relayReconnectIntervalM ,relay-reconnect-interval-minutes)
                             (startBrowser ,(bool->xml-string start-browser?))
                             (natEnabled ,(bool->xml-string nat-enabled?))
                             (natLeaseMinutes ,nat-lease-minutes)
                             (natRenewalMinutes ,nat-renewal-minutes)
                             (natTimeoutSeconds ,nat-timeout-seconds)
                             (urAccepted ,usage-reporting-accepted)
                             (urSeen ,usage-reporting-seen)
                             (urUniqueID ,usage-reporting-unique-id)
                             (urURL ,usage-reporting-url)
                             (urPostInsecurely ,(bool->xml-string usage-reporting-post-insecurely?))
                             (urInitialDelayS ,usage-reporting-initial-delay-seconds)
                             (autoUpgradeIntervalH ,auto-upgrade-interval-hours)
                             (upgradeToPreReleases ,(bool->xml-string upgrade-to-pre-releases?))
                             (keepTemporariesH ,keep-temporaries-hours)
                             (cacheIgnoredFiles ,(bool->xml-string cache-ignored-files?))
                             (progressUpdateIntervalS ,progress-update-interval-seconds)
                             (limitBandwidthInLan ,(bool->xml-string limit-bandwidth-in-lan?))
                             (minHomeDiskFree (@ (unit ,min-home-disk-free-unit))
                                              ,min-home-disk-free)
                             (releasesURL ,releases-url)
                             (overwriteRemoteDeviceNamesOnConnect ,(bool->xml-string overwrite-remote-device-names-on-connect?))
                             (tempIndexMinBlocks ,temp-index-min-blocks)
                             (unackedNotificationID ,unacked-notification-id)
                             (trafficClass ,traffic-class)
                             (setLowPriority ,(bool->xml-string set-low-priority?))
                             (maxFolderConcurrency ,max-folder-concurrency)
                             (crashReportingURL ,crash-reporting-url)
                             (crashReportingEnabled ,(bool->xml-string crash-reporting-enabled?))
                             (stunKeepaliveStartS ,stun-keepalive-start-seconds)
                             (stunKeepaliveMinS ,stun-keepalive-min-seconds)
                             (stunServer ,stun-server)
                             (databaseTuning ,database-tuning)
                             (maxConcurrentIncomingRequestKiB ,max-concurrent-incoming-request-kib)
                             (announceLANAddresses ,(bool->xml-string announce-lan-addresses?))
                             (sendFullIndexOnUpgrade ,(bool->xml-string send-full-index-on-upgrade?))
                             (connectionLimitEnough ,connection-limit-enough)
                             (connectionLimitMax ,connection-limit-max)
                             (insecureAllowOldTLSVersions ,(bool->xml-string insecure-allow-old-tls-versions?))
                             (connectionPriorityTcpLan ,connection-priority-tcp-lan)
                             (connectionPriorityQuicLan ,connection-priority-quic-lan)
                             (connectionPriorityTcpWan ,connection-priority-tcp-wan)
                             (connectionPriorityQuicWan ,connection-priority-quic-wan)
                             (connectionPriorityRelay ,connection-priority-relay)
                             (connectionPriorityUpgradeThreshold ,connection-priority-upgrade-threshold))
                    (defaults
                      ,(syncthing-folder->sxml default-folder)
                      ,(syncthing-device->sxml default-device)
                      (ignores ,default-ignores)))))


(define (serialize-syncthing-config-file config)
  (with-output-to-string
    (lambda ()
      (sxml->xml (cons '*TOP* (list (syncthing-config-file->sxml config)))))))

(define-record-type* <syncthing-configuration>
  syncthing-configuration make-syncthing-configuration
  syncthing-configuration?
  (syncthing syncthing-configuration-syncthing ;file-like
             (default syncthing))
  (arguments syncthing-configuration-arguments ;list of strings
             (default '()))
  (logflags  syncthing-configuration-logflags  ;number
             (default 0))
  (user      syncthing-configuration-user      ;string
             (default #f))
  (group     syncthing-configuration-group     ;string
             (default "users"))
  (home      syncthing-configuration-home      ;string
             (default #f))
  (config-file syncthing-configuration-config-file
                         (default #f))         ; syncthing-config-file or file-like
  (home-service? syncthing-configuration-home-service?
                 (default for-home?) (innate)))

(define syncthing-shepherd-service
  (match-record-lambda <syncthing-configuration>
      (syncthing arguments logflags user group home home-service? config-file)
    (list
     (shepherd-service
      (provision (if home-service?
                     '(syncthing)
                     (list (string->symbol
                            (string-append "syncthing-" user)))))
      (documentation "Run syncthing.")
      (requirement (if home-service? '() '(loopback user-processes)))
      (start #~(lambda _
                 ;; If we are managing the config, and it's not a home
                 ;; service, then exepect the config file at
                 ;; /var/lib/syncthing-<user>.  This makes sure the ownership
                 ;; is correct.
                 (unless (or #$(not config-file) #$home-service?)
                   (let ((user-pw (getpw #$user)))
                     (chown (string-append "/var/lib/syncthing-" #$user)
                            (passwd:uid user-pw)
                            (passwd:gid user-pw)))
                   (chmod (string-append "/var/lib/syncthing-" #$user) #o700))
                 (make-forkexec-constructor
                  (append (list (string-append #$syncthing "/bin/syncthing")
                                ;; Do not try to try to launch a web browser on startup.
                                "--no-browser"
                                ;; If syncthing crashes, let the service fail.
                                "--no-restart"
                                (string-append "--logflags=" (number->string #$logflags)))
                          ;; Optionally move data and configuration home to
                          ;; /var/lib/syncthing-<user>.
                          (if (or #$(not config-file) #$home-service?) '()
                              (list (string-append "--home=/var/lib/syncthing-" #$user)))
                          '#$arguments)
                  #:user #$(and (not home-service?) user)
                  #:group #$(and (not home-service?) group)
                  #:environment-variables
                  (append
                   (list
                    (string-append "HOME="
                                   (or #$home
                                       (passwd:dir
                                        (getpw (if (and #$home-service?
                                                        (not #$user))
                                                   (getuid)
                                                   #$user)))))
                    "SSL_CERT_DIR=/etc/ssl/certs"
                    "SSL_CERT_FILE=/etc/ssl/certs/ca-certificates.crt")
                   (filter (negate       ;XXX: 'remove' is not in (guile)
                            (lambda (str)
                              (or (string-prefix? "HOME=" str)
                                  (string-prefix? "SSL_CERT_DIR=" str)
                                  (string-prefix? "SSL_CERT_FILE=" str))))
                           (environ))))))
      (respawn? #f)
      (stop #~(make-kill-destructor))))))

(define syncthing-files-service
  (match-record-lambda <syncthing-configuration> (config-file user home home-service?)
    (if config-file
        ;; When used as a system service, this service might be executed
        ;; before a user's home even exists, causing it to be owned by root,
        ;; and the skeletons to never be applied to that user's home.  In such
        ;; cases, put the config at /var/lib/syncthing-<user>/config.xml
        `((,(if home-service?
                ".local/state/syncthing/config.xml"
                (string-append "/var/lib/syncthing-" user "/config.xml"))
           ,(if (file-like? config-file)
                config-file
                (plain-file "syncthing-config.xml" (serialize-syncthing-config-file
                                                   config-file)))))
        '())))

(define syncthing-service-type
  (service-type (name 'syncthing)
                (extensions (list (service-extension shepherd-root-service-type
                                                     syncthing-shepherd-service)
                                  (service-extension special-files-service-type
                                                     syncthing-files-service)))
                (description
                 "Run @uref{https://github.com/syncthing/syncthing, Syncthing}
decentralized continuous file system synchronization.")))

;;; syncthing.scm ends here
