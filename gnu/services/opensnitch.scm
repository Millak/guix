;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Danny Milosavljevic <dannym@friendly-machines.com>
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

(define-module (gnu services opensnitch)
  #:use-module (gnu packages networking)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (json)
  #:export (opensnitch-configuration
            opensnitch-configuration?
            opensnitch-service-type))

(define-configuration/no-serialization opensnitch-configuration
  (opensnitch
   (package opensnitch-daemon)
   "The @code{opensnitch-daemon} package to use.")

  ;; Server settings
  (server-address
   (string "unix:///tmp/osui.sock")
   "Address for the UI to connect to the daemon.")
  (server-log-file
   (string "/var/log/opensnitchd.log")
   "Path to the daemon log file.")

  ;; Authentication settings
  (authentication-type
   (string "simple")
   "Authentication type for UI-daemon communication.")
  (tls-ca-cert
   (string "")
   "Path to TLS CA certificate.")
  (tls-server-cert
   (string "")
   "Path to TLS server certificate.")
  (tls-client-cert
   (string "")
   "Path to TLS client certificate.")
  (tls-client-key
   (string "")
   "Path to TLS client key.")
  (tls-skip-verify?
   (boolean #f)
   "Whether to skip TLS verification.")
  (tls-client-auth-type
   (string "no-client-cert")
   "TLS client authentication type.")

  ;; Default behavior
  (default-action
   (string "allow")
   "Default action for connections: @code{\"allow\"} or @code{\"deny\"}.")
  (default-duration
   (string "once")
   "Default duration for rules: @code{\"once\"}, @code{\"until-restart\"},
@code{\"always\"}, etc.")
  (intercept-unknown?
   (boolean #f)
   "Whether to intercept connections from unknown processes.")

  ;; Process monitoring
  (proc-monitor-method
   (string "ebpf")
   "Method for monitoring processes: @code{\"ebpf\"}, @code{\"proc\"}, or
@code{\"audit\"}.")

  ;; Logging
  (log-level
   (integer 2)
   "Log level: 0=silent, 1=error, 2=warning, 3=important, 4=debug.")
  (log-utc?
   (boolean #t)
   "Whether to log timestamps in UTC.")
  (log-micro?
   (boolean #f)
   "Whether to include microseconds in log timestamps.")

  ;; Firewall settings
  (firewall
   (string "nftables")
   "Firewall backend: @code{\"nftables\"} or @code{\"iptables\"}.")
  (fw-config-path
   (string "/etc/opensnitchd/system-fw.json")
   "Path to the system firewall configuration file.")
  (fw-monitor-interval
   (string "15s")
   "Interval for monitoring firewall rules.")
  (fw-queue-bypass?
   (boolean #t)
   "Whether to bypass the queue when the daemon is not running.")

  ;; Rules settings
  (rules-path
   (string "/etc/opensnitchd/rules/")
   "Directory where firewall rules are stored.")
  (rules-enable-checksums?
   (boolean #f)
   "Whether to enable checksums for rules.")

  ;; eBPF settings
  (ebpf-events-workers
   (integer 8)
   "Number of eBPF event worker threads.")
  (ebpf-queue-events-size
   (integer 0)
   "Size of the eBPF events queue (0 = default).")

  ;; Statistics settings
  (stats-max-events
   (integer 250)
   "Maximum number of events to keep in statistics.")
  (stats-max-stats
   (integer 25)
   "Maximum number of statistics entries.")
  (stats-workers
   (integer 6)
   "Number of statistics worker threads.")

  ;; Internal settings
  (internal-gc-percent
   (integer 100)
   "Go garbage collector percentage.")
  (internal-flush-conns-on-start?
   (boolean #t)
   "Whether to flush existing connections on daemon start."))

(define (opensnitch-configuration->json config)
  "Convert CONFIG to a JSON string for the OpenSnitch daemon."
  (match-record config <opensnitch-configuration>
    (server-address server-log-file
     authentication-type tls-ca-cert tls-server-cert tls-client-cert
     tls-client-key tls-skip-verify? tls-client-auth-type
     default-action default-duration intercept-unknown?
     proc-monitor-method log-level log-utc? log-micro?
     firewall fw-config-path fw-monitor-interval fw-queue-bypass?
     rules-path rules-enable-checksums?
     ebpf-events-workers ebpf-queue-events-size
     stats-max-events stats-max-stats stats-workers
     internal-gc-percent internal-flush-conns-on-start?)
    (scm->json-string
     `((Server . ((Address . ,server-address)
                  (Authentication . ((Type . ,authentication-type)
                                     (TLSOptions . ((CACert . ,tls-ca-cert)
                                                    (ServerCert . ,tls-server-cert)
                                                    (ClientCert . ,tls-client-cert)
                                                    (ClientKey . ,tls-client-key)
                                                    (SkipVerify . ,tls-skip-verify?)
                                                    (ClientAuthType . ,tls-client-auth-type)))))
                  (LogFile . ,server-log-file)))
       (DefaultAction . ,default-action)
       (DefaultDuration . ,default-duration)
       (InterceptUnknown . ,intercept-unknown?)
       (ProcMonitorMethod . ,proc-monitor-method)
       (LogLevel . ,log-level)
       (LogUTC . ,log-utc?)
       (LogMicro . ,log-micro?)
       (Firewall . ,firewall)
       (FwOptions . ((ConfigPath . ,fw-config-path)
                     (MonitorInterval . ,fw-monitor-interval)
                     (QueueBypass . ,fw-queue-bypass?)))
       (Rules . ((Path . ,rules-path)
                 (EnableChecksums . ,rules-enable-checksums?)))
       (Ebpf . ((EventsWorkers . ,ebpf-events-workers)
                (QueueEventsSize . ,ebpf-queue-events-size)))
       (Stats . ((MaxEvents . ,stats-max-events)
                 (MaxStats . ,stats-max-stats)
                 (Workers . ,stats-workers)))
       (Internal . ((GCPercent . ,internal-gc-percent)
                    (FlushConnsOnStart . ,internal-flush-conns-on-start?))))
     #:pretty #t)))

(define (opensnitch-config-file config)
  "Return a file-like object for the OpenSnitch configuration."
  (plain-file "opensnitch-config.json"
              (opensnitch-configuration->json config)))

(define (opensnitch-activation config)
  "Return the activation gexp for CONFIG."
  (match-record config <opensnitch-configuration>
    (rules-path)
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils))
          (mkdir-p #$rules-path)))))

(define (opensnitch-shepherd-service config)
  (match-record config <opensnitch-configuration>
    (opensnitch server-log-file)
    (list (shepherd-service
           (documentation "Run the OpenSnitch application firewall daemon.")
           (provision '(opensnitch))
           (requirement '(user-processes networking))
           (start #~(make-forkexec-constructor
                     (list #$(file-append opensnitch "/sbin/opensnitchd")
                           "-config-file" #$(opensnitch-config-file config))
                     #:log-file #$server-log-file))
           (stop #~(make-kill-destructor))))))

(define opensnitch-service-type
  (service-type
   (name 'opensnitch)
   (description "Run the OpenSnitch application firewall daemon.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             opensnitch-shepherd-service)
          (service-extension activation-service-type
                             opensnitch-activation)
          (service-extension profile-service-type
                             (compose list opensnitch-configuration-opensnitch))))
   (default-value (opensnitch-configuration))))
