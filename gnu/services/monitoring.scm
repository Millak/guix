;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Sou Bunnbu <iyzsong@member.fsf.org>
;;; Copyright © 2018, 2019 Gábor Boskovits <boskovits@gmail.com>
;;; Copyright © 2018, 2019, 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu services monitoring)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services web)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages monitoring)
  #:use-module (gnu packages networking)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix utils)
  #:use-module ((guix ui) #:select (display-hint G_))
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-35)
  #:export (darkstat-configuration
            darkstat-service-type

            prometheus-node-exporter-configuration
            prometheus-node-exporter-configuration?
            prometheus-node-exporter-configuration-package
            prometheus-node-exporter-web-listen-address
            prometheus-node-exporter-service-type

            vnstat-configuration
            vnstat-configuration?
            vnstat-service-type
            vnstat-configuration-package
            vnstat-configuration-database-directory
            vnstat-configuration-5-minute-hours
            vnstat-configuration-64bit-interface-counters
            vnstat-configuration-always-add-new-interfaces?
            vnstat-configuration-bandwidth-detection?
            vnstat-configuration-bandwidth-detection-interval
            vnstat-configuration-boot-variation
            vnstat-configuration-check-disk-space?
            vnstat-configuration-create-directories?
            vnstat-configuration-daemon-group
            vnstat-configuration-daemon-user
            vnstat-configuration-daily-days
            vnstat-configuration-database-synchronous
            vnstat-configuration-database-write-ahead-logging?
            vnstat-configuration-hourly-days
            vnstat-configuration-log-file
            vnstat-configuration-max-bandwidth
            vnstat-configuration-max-bw
            vnstat-configuration-monthly-months
            vnstat-configuration-month-rotate
            vnstat-configuration-month-rotate-affects-years?
            vnstat-configuration-offline-save-interval
            vnstat-configuration-pid-file
            vnstat-configuration-poll-interval
            vnstat-configuration-rescan-database-on-save?
            vnstat-configuration-save-interval
            vnstat-configuration-save-on-status-change?
            vnstat-configuration-time-sync-wait
            vnstat-configuration-top-day-entries
            vnstat-configuration-trafficless-entries?
            vnstat-configuration-update-file-owner?
            vnstat-configuration-update-interval
            vnstat-configuration-use-logging
            vnstat-configuration-use-utc?
            vnstat-configuration-yearly-years

            zabbix-server-configuration
            zabbix-server-service-type
            zabbix-agent-configuration
            zabbix-agent-service-type
            zabbix-front-end-configuration
            zabbix-front-end-service-type
            %zabbix-front-end-configuration-nginx))


;;;
;;; darkstat
;;;

(define-record-type* <darkstat-configuration>
  darkstat-configuration make-darkstat-configuration darkstat-configuration?
  (package      darkstat-configuration-package
                (default darkstat))
  (interface    darkstat-configuration-interface)
  (port         darkstat-configuration-port
                (default "667"))
  (bind-address darkstat-configuration-bind-address
                (default "127.0.0.1"))
  (base         darkstat-configuration-base
                (default "/")))

(define %darkstat-accounts
  (list (user-account
         (name "darkstat")
         (group "darkstat")
         (system? #t)
         (comment "darkstat daemon user")
         (home-directory "/var/lib/darkstat")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "darkstat")
         (system? #t))))

(define darkstat-shepherd-service
  (match-lambda
    (($ <darkstat-configuration>
        package interface port bind-address base)
     (shepherd-service
      (documentation "Network statistics gatherer.")
      (provision '(darkstat))
      (requirement '(user-processes networking))
      (start #~(make-forkexec-constructor
                (list #$(file-append package "/sbin/darkstat")
                      "-i" #$interface
                      "-p" #$port
                      "-b" #$bind-address
                      "--base" #$base
                      "--syslog" "--no-daemon"
                      "--chroot" "/var/lib/darkstat"
                      "--user" "darkstat"
                      "--import" "darkstat.db"
                      "--export" "darkstat.db")))
      (stop #~(make-kill-destructor))))))

(define darkstat-service-type
  (service-type
   (name 'darkstat)
   (description
    "Run @command{darkstat} to serve network traffic statistics reports over
HTTP.")
   (extensions
    (list (service-extension account-service-type
                             (const %darkstat-accounts))
          (service-extension shepherd-root-service-type
                             (compose list darkstat-shepherd-service))))))


;;;
;;; Prometheus node exporter
;;;

(define-record-type* <prometheus-node-exporter-configuration>
  prometheus-node-exporter-configuration
  make-prometheus-node-exporter-configuration
  prometheus-node-exporter-configuration?
  (package prometheus-node-exporter-configuration-package
           (default go-github-com-prometheus-node-exporter))
  (web-listen-address prometheus-node-exporter-web-listen-address
                      (default ":9100"))
  (textfile-directory prometheus-node-exporter-textfile-directory
                      (default "/var/lib/prometheus/node-exporter"))
  (extra-options      prometheus-node-exporter-extra-options
                      (default '())))

(define %prometheus-node-exporter-accounts
  (list (user-account
         (name "prometheus-node-exporter")
         (group "prometheus-node-exporter")
         (system? #t)
         (comment "Prometheus node exporter daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "prometheus-node-exporter")
         (system? #t))))

(define prometheus-node-exporter-shepherd-service
  (match-lambda
    (( $ <prometheus-node-exporter-configuration>
         package web-listen-address textfile-directory extra-options)
     (list
      (shepherd-service
       (documentation "Prometheus node exporter.")
       (provision '(prometheus-node-exporter))
       (requirement '(networking))
       (start #~(make-forkexec-constructor
                 (list #$(file-append package "/bin/node_exporter")
                       "--web.listen-address" #$web-listen-address
                       #$@(if textfile-directory
                              (list "--collector.textfile.directory"
                                    textfile-directory)
                              '())
                       #$@extra-options)
                 #:user "prometheus-node-exporter"
                 #:group "prometheus-node-exporter"
                 #:log-file "/var/log/prometheus-node-exporter.log"))
       (stop #~(make-kill-destructor)))))))

(define (prometheus-node-exporter-activation config)
  (with-imported-modules '((guix build utils))
    #~(let ((textfile-directory
             #$(prometheus-node-exporter-textfile-directory config)))
        (use-modules (guix build utils))

        (when textfile-directory
          (let ((user (getpw "prometheus-node-exporter")))
            #t
            (mkdir-p textfile-directory)
            (chown textfile-directory (passwd:uid user) (passwd:gid user))
            (chmod textfile-directory #o775))))))

(define prometheus-node-exporter-service-type
  (service-type
   (name 'prometheus-node-exporter)
   (description
    "Run @command{node_exporter} to serve hardware and OS metrics to
Prometheus.")
   (extensions
    (list
     (service-extension account-service-type
                        (const %prometheus-node-exporter-accounts))
     (service-extension activation-service-type
                        prometheus-node-exporter-activation)
     (service-extension shepherd-root-service-type
                        prometheus-node-exporter-shepherd-service)))
   (default-value (prometheus-node-exporter-configuration))))


;;;
;;; vnstat daemon
;;;

(define* (camelfy-field-name field-name #:key (dromedary? #f))
  (match (string-split (symbol->string field-name) #\-)
    ((head tail ...)
     (string-join (cons (if dromedary? head (string-upcase head 0 1))
                        (map (cut string-upcase <> 0 1) tail)) ""))))

(define (strip-trailing-?-character field-name)
  "Drop rightmost '?' character"
  (let ((str (symbol->string field-name)))
    (if (string-suffix? "?" str)
        (string->symbol (string-drop-right str 1))
        field-name)))

(define (vnstat-serialize-string field-name value)
  #~(format #f "~a ~s~%"
            #$(camelfy-field-name field-name)
            #$value))

(define vnstat-serialize-integer vnstat-serialize-string)

(define (vnstat-serialize-boolean field-name value)
  #~(format #f "~a ~a~%"
            #$(camelfy-field-name (strip-trailing-?-character field-name))
            #$(if value 1 0)))

(define (vnstat-serialize-alist field-name value)
  (generic-serialize-alist string-append
                           (lambda (iface val)
                             (vnstat-serialize-integer
                              (format #f "MaxBW~a" iface) val))
                           value))

(define (vnstat-serialize-user-account field-name value)
  (vnstat-serialize-string field-name (user-account-name value)))

(define (vnstat-serialize-user-group field-name value)
  (vnstat-serialize-string field-name (user-group-name value)))

(define-maybe string  (prefix vnstat-))
(define-maybe integer (prefix vnstat-))
(define-maybe boolean (prefix vnstat-))
(define-maybe alist   (prefix vnstat-))
(define-maybe user-account (prefix vnstat-))
(define-maybe user-group (prefix vnstat-))

(define %vnstat-user
  (user-account
   (name "vnstat")
   (group "vnstat")
   (system? #t)
   (home-directory "/var/empty")
   (shell (file-append shadow "/sbin/nologin"))))

(define %vnstat-group
  (user-group
   (name "vnstat")
   (system? #t)))

;; Documentation strings from vnstat.conf manpage adapted to texinfo.
;; vnstat checkout: v2.10, commit b3408af1c609aa6265d296cab7bfe59a61d7cf70
;; Do not reflow these strings or drop the initial \ escape as it makes it
;; harder to diff against the manpage.
(define-configuration vnstat-configuration
  (package
    (file-like vnstat)
    "The vnstat package."
    empty-serializer)

  (database-directory
   (string "/var/lib/vnstat")
   "\
Specifies the directory where the database is to be stored.
A full path must be given and a leading '/' isn't required."
   (serializer
    (lambda (_ value)
      (vnstat-serialize-string 'database-dir value))))

  (5-minute-hours
   (maybe-integer 48)
   "\
Data retention duration for the 5 minute resolution entries. The configuration
defines for how many past hours entries will be stored. Set to @code{-1} for
unlimited entries or to @code{0} to disable the data collection of this
resolution.")

  (64bit-interface-counters
   (maybe-integer -2)
   "\
Select interface counter handling. Set to @code{1} for defining that all interfaces
use 64-bit counters on the kernel side and @code{0} for defining 32-bit counter. Set
to @code{-1} for using the old style logic used in earlier versions where counter
values within 32-bits are assumed to be 32-bit and anything larger is assumed to
be a 64-bit counter. This may produce false results if a 64-bit counter is
reset within the 32-bits. Set to @code{-2} for using automatic detection based on
available kernel datastructures.")

  (always-add-new-interfaces?
   (maybe-boolean #t)
   "\
Enable or disable automatic creation of new database entries for interfaces not
currently in the database even if the database file already exists when the
daemon is started. New database entries will also get created for new interfaces
seen while the daemon is running. Pseudo interfaces @samp{lo}, @samp{lo0} and @samp{sit0} are always
excluded from getting added.")

  (bandwidth-detection?
   (maybe-boolean #t)
   "\
Try to automatically detect
@var{max-bandwidth}
value for each monitored interface. Mostly only ethernet interfaces support
this feature.
@var{max-bandwidth}
will be used as fallback value if detection fails. Any interface specific
@var{max-BW}
configuration will disable the detection for the specified interface.
In Linux, the detection is disabled for tun interfaces due to the
Linux kernel always reporting 10 Mbit regardless of the used real interface.")

  (bandwidth-detection-interval
   (maybe-integer 5)
   "\
How often in minutes interface specific detection of
@var{max-bandwidth}
is done for detecting possible changes when
@var{bandwidth-detection}
is enabled. Can be disabled by setting to @code{0}. Value range: @samp{0}..@samp{30}")

  (boot-variation
   (maybe-integer 15)
   "\
Time in seconds how much the boot time reported by system kernel can variate
between updates. Value range: @samp{0}..@samp{300}")

  (check-disk-space?
   (maybe-boolean #t)
   "\
Enable or disable the availability check of at least some free disk space before
a database write.")

  (create-directories?
   (maybe-boolean #t)
   "\
Enable or disable the creation of directories when a configured path doesn't
exist. This includes @var{database-directory}."
   (serializer
    (lambda (_ value)
      (if (maybe-value-set? value)
          (vnstat-serialize-boolean 'create-dirs value) ""))))

  ;; Note: Documentation for daemon-group and daemon-user adapted
  ;; for user-group and user-account record-types.
  (daemon-group
   (maybe-user-group %vnstat-group)
   "\
Specify the group to which the daemon process should switch during startup.
Set to @code{%unset-value} to disable group switching.")

  (daemon-user
   (maybe-user-account %vnstat-user)
   "\
Specify the user to which the daemon process should switch during startup.
Set to @code{%unset-value} to disable user switching.")

  (daily-days
   (maybe-integer 62)
   "\
Data retention duration for the one day resolution entries. The configuration
defines for how many past days entries will be stored. Set to @code{-1} for
unlimited entries or to @code{0} to disable the data collection of this
resolution.")

  (database-synchronous
   (maybe-integer -1)
   "\
Change the setting of the SQLite \"synchronous\" flag which controls how much
care is taken to ensure disk writes have fully completed when writing data to
the database before continuing other actions. Higher values take extra steps
to ensure data safety at the cost of slower performance. A value of @code{0} will
result in all handling being left to the filesystem itself. Set to @code{-1} to
select the default value according to database mode controlled by
@var{database-write-ahead-logging}
setting. See SQLite documentation for more details regarding values from @code{1}
to @code{3}. Value range: @samp{-1}..@samp{3}")

  (database-write-ahead-logging?
   (maybe-boolean #f)
   "\
Enable or disable SQLite Write-Ahead Logging mode for the database. See SQLite
documentation for more details and note that support for read-only operations
isn't available in older SQLite versions.")

  (hourly-days
   (maybe-integer 4)
   "\
Data retention duration for the one hour resolution entries. The configuration
defines for how many past days entries will be stored. Set to @code{-1} for
unlimited entries or to @code{0} to disable the data collection of this
resolution.")

  (log-file
   maybe-string
   "\
Specify log file path and name to be used if @var{use-logging} is set to @code{1}.")

  (max-bandwidth
   maybe-integer
   "\
Maximum bandwidth for all interfaces. If the interface specific traffic
exceeds the given value then the data is assumed to be invalid and rejected.
Set to 0 in order to disable the feature. Value range: @samp{0}..@samp{50000}")

  ;; documentation adapted for alist type
  (max-bw
   maybe-alist
   "\
Same as
@var{max-bandwidth}
but can be used for setting individual limits
for selected interfaces. This is an association list of interfaces
as strings to integer values. For example,
@lisp
(max-bw
 `((\"eth0\" . 15000)
   (\"ppp0\" . 10000)))
@end lisp
@var{bandwidth-detection}
is disabled on an interface specific level for each
@var{max-bw}
configuration. Value range: @samp{0}..@samp{50000}"
   (serializer
    (lambda (field-name value)
      (if (maybe-value-set? value)
          (vnstat-serialize-alist field-name value) ""))))

  (monthly-months
   (maybe-integer 25)
   "\
Data retention duration for the one month resolution entries. The configuration
defines for how many past months entries will be stored. Set to @code{-1} for
unlimited entries or to @code{0} to disable the data collection of this
resolution.")

  (month-rotate
   (maybe-integer 1)
   "\
Day of month that months are expected to change. Usually set to
1 but can be set to alternative values for example for tracking
monthly billed traffic where the billing period doesn't start on
the first day. For example, if set to 7, days of February up to and
including the 6th will count for January. Changing this option will
not cause existing data to be recalculated. Value range: @samp{1}..@samp{28}")

  (month-rotate-affects-years?
   (maybe-boolean #f)
   "\
Enable or disable
@var{month-rotate}
also affecting yearly data. Applicable only when
@var{month-rotate}
has a value greater than one.")

  (offline-save-interval
   (maybe-integer 30)
   "\
How often in minutes cached interface data is saved to file when all monitored
interfaces are offline. Value range:
@var{save-interval}..@samp{60}")

  (pid-file
   (maybe-string "/var/run/vnstat/vnstatd.pid")
   "\
Specify pid file path and name to be used.")

  (poll-interval
   (maybe-integer 5)
   "\
How often in seconds interfaces are checked for status changes.
Value range: @samp{2}..@samp{60}")

  (rescan-database-on-save?
   maybe-boolean
   "\
Automatically discover added interfaces from the database and start monitoring.
The rescan is done every
@var{save-interval}
or
@var{offline-save-interval}
minutes depending on the current activity state.")

  (save-interval
   (maybe-integer 5)
   "\
How often in minutes cached interface data is saved to file.
Value range: (
@var{update-interval} / 60 )..@samp{60}")

  (save-on-status-change?
   (maybe-boolean #t)
   "\
Enable or disable the additional saving to file of cached interface data
when the availability of an interface changes, i.e., when an interface goes
offline or comes online.")

  (time-sync-wait
   (maybe-integer 5)
   "\
How many minutes to wait during daemon startup for system clock to sync if
most recent database update appears to be in the future. This may be needed
in systems without a real-time clock (RTC) which require some time after boot
to query and set the correct time. @code{0} = wait disabled.
Value range: @samp{0}..@samp{60}")

  (top-day-entries
   (maybe-integer 20)
   "\
Data retention duration for the top day entries. The configuration
defines how many of the past top day entries will be stored. Set to @code{-1} for
unlimited entries or to @code{0} to disable the data collection of this
resolution.")

  (trafficless-entries?
   (maybe-boolean #t)
   "\
Create database entries even when there is no traffic during the entry's time
period.")

  (update-file-owner?
   (maybe-boolean #t)
   "\
Enable or disable the update of file ownership during daemon process startup.
During daemon startup, only database, log and pid files will be modified if the
user or group change feature (
@var{daemon-user}
or
@var{daemon-group}
) is enabled and the files don't match the requested user or group. During manual
database creation, this option will cause file ownership to be inherited from the
database directory if the directory already exists. This option only has effect
when the process is started as root or via sudo.")

  (update-interval
   (maybe-integer 20)
   "\
How often in seconds the interface data is updated. Value range:
@var{poll-interval}..@samp{300}")

  (use-logging
   (maybe-integer 2)
   "\
Enable or disable logging. Accepted values are:
@code{0} = disabled, @code{1} = logfile and @code{2} = syslog.")

  (use-utc?
   maybe-boolean
   "\
Enable or disable using UTC as timezone in the database for all entries. When
enabled, all entries added to the database will use UTC regardless of the
configured system timezone. When disabled, the configured system timezone
will be used. Changing this setting will not result in already existing
data to be modified."
   (serializer
    (lambda (_ value)
      (if (maybe-value-set? value)
          (vnstat-serialize-boolean 'use-UTC value) ""))))

  (yearly-years
   (maybe-integer -1)
   "\
Data retention duration for the one year resolution entries. The configuration
defines for how many past years entries will be stored. Set to @code{-1} for
unlimited entries or to @code{0} to disable the data collection of this
resolution.")

  (prefix vnstat-))

(define (vnstat-serialize-configuration config)
  (mixed-text-file
   "vnstat.conf"
   (serialize-configuration config vnstat-configuration-fields)))

(define (vnstat-shepherd-service config)
  (let ((config-file (vnstat-serialize-configuration config)))
    (match-record config <vnstat-configuration> (package pid-file)
      (shepherd-service
       (documentation "Run vnstatd.")
       (requirement `(networking file-systems))
       (provision '(vnstatd))
       (start #~(make-forkexec-constructor
                 (list #$(file-append package "/sbin/vnstatd")
                       "--daemon"
                       "--config" #$config-file)
                 #:pid-file #$pid-file))
       (stop #~(make-kill-destructor))
       (actions
        (list (shepherd-configuration-action config-file)
              (shepherd-action
               (name 'reload)
               (documentation "Reload vnstatd.")
               (procedure
                #~(lambda (pid)
                    (if pid
                        (begin
                          (kill pid SIGHUP)
                          (format #t
                                  "Issued SIGHUP to vnstatd (PID ~a)."
                                  pid))
                        (format #t "vnstatd is not running.")))))))))))

(define (vnstat-account-service config)
  (match-record config <vnstat-configuration> (daemon-group daemon-user)
    (filter-map maybe-value (list daemon-group daemon-user))))

(define vnstat-service-type
  (service-type
   (name 'vnstat)
   (description "vnStat network-traffic monitor service.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list vnstat-shepherd-service))
          (service-extension account-service-type
                             vnstat-account-service)))
   (default-value (vnstat-configuration))))


;;;
;;; Zabbix server
;;;

(define (uglify-field-name field-name)
  (apply string-append
         (map (lambda (str)
                (if (member (string->symbol str) '(ca db ssl))
                    (string-upcase str)
                    (string-capitalize str)))
              (string-split (string-delete #\?
                                           (symbol->string field-name))
                            #\-))))

(define (serialize-field field-name val)
  #~(format #f "~a=~a~%" #$(uglify-field-name field-name) #$val))

(define (serialize-number field-name val)
  (serialize-field field-name (number->string val)))

(define (serialize-list field-name val)
  #~(if (null? '#$val)
        ""
        #$(serialize-field field-name (string-join val ","))))


(define (serialize-string field-name val)
  (if (or (eq? 'user field-name)
          (eq? 'group field-name)
          (and (string? val) (string=? val "")))
      ""
      (serialize-field field-name val)))

(define include-files? list?)

(define (serialize-include-files field-name val)
  #~(string-append #$@(map (cut serialize-field 'include <>) val)))

(define extra-options? string?)

(define (serialize-extra-options field-name val)
  #~(if (= 0 (string-length #$val)) "" #$(format #f "~a~%" val)))

(define (nginx-server-configuration-list? val)
  (and (list? val) (and-map nginx-server-configuration? val)))

(define (serialize-nginx-server-configuration-list field-name val)
  "")

(define-configuration zabbix-server-configuration
  (zabbix-server
   (file-like zabbix-server)
   "The zabbix-server package.")
  (user
   (string "zabbix")
   "User who will run the Zabbix server.")
  (group
   (string "zabbix")
   "Group who will run the Zabbix server.")
  (db-host
   (string "127.0.0.1")
   "Database host name.")
  (db-name
   (string "zabbix")
   "Database name.")
  (db-user
   (string "zabbix")
   "Database user.")
  (db-password
   (string "")
   "Database password.  Please, use @code{include-files} with
@code{DBPassword=SECRET} inside a specified file instead.")
  (db-port
   (number 5432)
   "Database port.")
  (log-type
   (string "")
   "Specifies where log messages are written to:
@itemize
@item @code{system} - syslog.
@item @code{file} - file specified with @code{log-file} parameter.
@item @code{console} - standard output.
@end itemize\n")
  (log-file
   (string "/var/log/zabbix/server.log")
   "Log file name for @code{log-type} @code{file} parameter.")
  (pid-file
   (string  "/var/run/zabbix/zabbix_server.pid")
   "Name of PID file.")
  (ssl-ca-location
   (string "/etc/ssl/certs/ca-certificates.crt")
   "The location of certificate authority (CA) files for SSL server
certificate verification.")
  (ssl-cert-location
   (string "/etc/ssl/certs")
   "Location of SSL client certificates.")
  (extra-options
   (extra-options "")
   "Extra options will be appended to Zabbix server configuration file.")
  (include-files
   (include-files '())
   "You may include individual files or all files in a directory in the
configuration file."))

(define (zabbix-server-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user (zabbix-server-configuration-user config))
        (zabbix-group (zabbix-server-configuration-group config)))
    (list (user-group (name zabbix-group) (system? #t))
          (user-account
           (name zabbix-user)
           (system? #t)
           (group zabbix-group)
           (comment "zabbix privilege separation user")
           (home-directory (string-append "/var/run/" zabbix-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (zabbix-server-config-file config)
  "Return the zabbix-server configuration file corresponding to CONFIG."
  (computed-file
   "zabbix_server.conf"
   #~(begin
       (call-with-output-file #$output
         (lambda (port)
           (format port "# Generated by 'zabbix-server-service'.~%")
           (format port #$(serialize-configuration
                           config zabbix-server-configuration-fields)))))))

(define (zabbix-server-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((user (getpw #$(zabbix-server-configuration-user config))))
          (for-each (lambda (file)
                      (let ((directory (dirname file)))
                        (mkdir-p directory)
                        (chown directory (passwd:uid user) (passwd:gid user))
                        (chmod directory #o755)))
                    (list #$(zabbix-server-configuration-log-file config)
                          #$(zabbix-server-configuration-pid-file config)
                          "/etc/zabbix/maintenance.inc.php"))))))

(define (zabbix-server-runtime-control-procedure zabbix-server config command)
  ;; XXX: This is duplicated from mcron; factorize.
  #~(lambda (_ . args)
      ;; Run 'zabbix_server' in a pipe so we can explicitly redirect its output
      ;; to 'current-output-port', which at this stage is bound to the client
      ;; connection.
      (let ((pipe (apply open-pipe* OPEN_READ #$zabbix-server
                         "--config" #$config
                         "-R" #$command args)))
        (let loop ()
          (match (read-line pipe 'concat)
            ((? eof-object?)
             (catch 'system-error
               (lambda ()
                 (zero? (close-pipe pipe)))
               (lambda args
                 ;; There's a race with the SIGCHLD handler, which could
                 ;; call 'waitpid' before 'close-pipe' above does.  If we
                 ;; get ECHILD, that means we lost the race; in that case, we
                 ;; cannot tell what the exit code was (FIXME).
                 (or (= ECHILD (system-error-errno args))
                     (apply throw args)))))
            (line
             (display line)
             (loop)))))))

;; Provide shepherd actions for common "zabbix_server -R" commands
;; mainly for a convenient way to use the correct configuration file.
(define (zabbix-server-actions zabbix-server config)
  (list (shepherd-action
         (name 'reload-config-cache)
         (documentation "Reload the configuration cache.")
         (procedure (zabbix-server-runtime-control-procedure
                     zabbix-server config "config_cache_reload")))
        (shepherd-action
         (name 'reload-snmp-cache)
         (documentation "Reload SNMP cache.")
         (procedure (zabbix-server-runtime-control-procedure
                     zabbix-server config "snmp_cache_reload")))))

(define (zabbix-server-shepherd-service config)
  "Return a <shepherd-service> for Zabbix server with CONFIG."
  (let ((zabbix-server
         (file-append (zabbix-server-configuration-zabbix-server config)
                      "/sbin/zabbix_server"))
        (config-file (zabbix-server-config-file config)))
    (list (shepherd-service
           (provision '(zabbix-server))
           (requirement '(user-processes))
           (documentation "Run the Zabbix server daemon.")
           (actions (zabbix-server-actions zabbix-server config-file))
           (start #~(make-forkexec-constructor
                     (list #$zabbix-server
                           "--config" #$config-file
                           "--foreground")
                     #:user #$(zabbix-server-configuration-user config)
                     #:group #$(zabbix-server-configuration-group config)
                     #:pid-file #$(zabbix-server-configuration-pid-file config)
                     #:environment-variables
                     (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                           "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt")))
           (stop #~(make-kill-destructor
                     ;; The server needs to finish database work on shutdown
                     ;; which can take a while for big or busy databases.
                     #:grace-period 60))))))

(define zabbix-server-service-type
  (service-type
   (name 'zabbix-server)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-server-shepherd-service)
          (service-extension account-service-type
                             zabbix-server-account)
          (service-extension activation-service-type
                             zabbix-server-activation)))
   (default-value (zabbix-server-configuration))
   (description "Run the Zabbix server, a high-performance monitoring system
that can collect data about machines from a variety of sources and provide the
results in a Web interface.")))

(define (generate-zabbix-server-documentation)
  (generate-documentation
   `((zabbix-server-configuration
      ,zabbix-server-configuration-fields))
   'zabbix-server-configuration))

(define-configuration zabbix-agent-configuration
  (zabbix-agent
   (file-like zabbix-agentd)
   "The zabbix-agent package.")
  (user
   (string "zabbix")
   "User who will run the Zabbix agent.")
  (group
   (string "zabbix")
   "Group who will run the Zabbix agent.")
  (hostname
   (string "")
   "Unique, case sensitive hostname which is required for active checks and
must match hostname as configured on the server.")
  (log-type
   (string "")
   "Specifies where log messages are written to:
@itemize
@item @code{system} - syslog.
@item @code{file} - file specified with @code{log-file} parameter.
@item @code{console} - standard output.
@end itemize\n")
  (log-file
   (string "/var/log/zabbix/agent.log")
   "Log file name for @code{log-type} @code{file} parameter.")
  (pid-file
   (string "/var/run/zabbix/zabbix_agent.pid")
   "Name of PID file.")
  (server
   (list '("127.0.0.1"))
   "List of IP addresses, optionally in CIDR notation, or hostnames of Zabbix
servers and Zabbix proxies.  Incoming connections will be accepted only from
the hosts listed here.")
  (server-active
   (list '("127.0.0.1"))
   "List of IP:port (or hostname:port) pairs of Zabbix servers and Zabbix
proxies for active checks.  If port is not specified, default port is used.
If this parameter is not specified, active checks are disabled.")
  (extra-options
   (extra-options "")
   "Extra options will be appended to Zabbix server configuration file.")
  (include-files
   (include-files '())
   "You may include individual files or all files in a directory in the
configuration file."))

(define (zabbix-agent-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((zabbix-user (zabbix-agent-configuration-user config))
        (zabbix-group (zabbix-agent-configuration-group config)))
    (list (user-group (name zabbix-group) (system? #t))
          (user-account
           (name zabbix-user)
           (system? #t)
           (group zabbix-group)
           (comment "zabbix privilege separation user")
           (home-directory (string-append "/var/run/" zabbix-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (zabbix-agent-activation config)
  "Return the activation gexp for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils)
                     (ice-9 rdelim))
        (let ((user
               (getpw #$(zabbix-agent-configuration-user config))))
          (for-each (lambda (file)
                      (let ((directory (dirname file)))
                        (mkdir-p directory)
                        (chown directory (passwd:uid user) (passwd:gid user))
                        (chmod directory #o755)))
                    (list #$(zabbix-agent-configuration-log-file config)
                          #$(zabbix-agent-configuration-pid-file config)))))))

(define (zabbix-agent-config-file config)
  "Return the zabbix-agent configuration file corresponding to CONFIG."
  (computed-file
   "zabbix_agent.conf"
   #~(begin
       (call-with-output-file #$output
         (lambda (port)
           (format port "# Generated by 'zabbix-agent-service'.~%")
           (format port #$(serialize-configuration
                           config zabbix-agent-configuration-fields)))))))

(define (zabbix-agent-arguments config)
  #~(let* ((config-file #$(zabbix-agent-config-file config))
           (agent #$(zabbix-agent-configuration-zabbix-agent config))
           (agent2? (file-exists? (string-append agent "/sbin/zabbix_agent2"))))
      (if agent2?
          (list (string-append agent "/sbin/zabbix_agent2")
                "-config" config-file
                "-foreground")
          (list (string-append agent "/sbin/zabbix_agentd")
                "--config" config-file
                "--foreground"))))

(define (zabbix-agent-shepherd-service config)
  "Return a <shepherd-service> for Zabbix agent with CONFIG."
  (list (shepherd-service
         (provision '(zabbix-agent))
         (requirement '(user-processes))
         (documentation "Run Zabbix agent daemon.")
         (start #~(make-forkexec-constructor
                   #$(zabbix-agent-arguments config)
                   #:user #$(zabbix-agent-configuration-user config)
                   #:group #$(zabbix-agent-configuration-group config)
                   #:pid-file #$(zabbix-agent-configuration-pid-file config)
                   #:environment-variables
                   (list "SSL_CERT_DIR=/run/current-system/profile\
/etc/ssl/certs"
                         "SSL_CERT_FILE=/run/current-system/profile\
/etc/ssl/certs/ca-certificates.crt"
                         "PATH=/run/privileged/bin:\
/run/current-system/profile/bin:/run/current-system/profile/sbin")))
         (stop #~(make-kill-destructor)))))

(define zabbix-agent-service-type
  (service-type
   (name 'zabbix-agent)
   (extensions
    (list (service-extension shepherd-root-service-type
                             zabbix-agent-shepherd-service)
          (service-extension account-service-type
                             zabbix-agent-account)
          (service-extension activation-service-type
                             zabbix-agent-activation)))
   (default-value (zabbix-agent-configuration))
   (description "Run the Zabbix agent, @command{zabbix_agentd}, which gathers
information about the running system for the Zabbix monitoring server.")))

(define (generate-zabbix-agent-documentation)
  (generate-documentation
   `((zabbix-agent-configuration
      ,zabbix-agent-configuration-fields))
   'zabbix-agent-configuration))

(define %zabbix-front-end-configuration-nginx
  (nginx-server-configuration
   (root #~(string-append #$zabbix-server:front-end "/share/zabbix/php"))
   (index '("index.php"))
   (locations
    (let ((php-location (nginx-php-location)))
      (list (nginx-location-configuration
             (inherit php-location)
             (body (append (nginx-location-configuration-body php-location)
                           (list "
fastcgi_param PHP_VALUE \"post_max_size = 16M
                          max_execution_time = 300\";
")))))))
   (listen '("80"))))

(define (zabbix-front-end-nginx-extension config)
  (match config
    (($ <zabbix-front-end-configuration> server nginx)
     (if (null? nginx)
         (list
          (nginx-server-configuration
           (inherit %zabbix-front-end-configuration-nginx)
           (root #~(string-append #$server:front-end "/share/zabbix/php"))))
         nginx))))

(define-configuration zabbix-front-end-configuration
  (zabbix-server
    (file-like zabbix-server)
    "The Zabbix server package to use.")
  (nginx
    (list '())
   "List of @ref{nginx-server-configuration,@code{nginx-server-configuration}}
blocks for the Zabbix front-end.  When empty, a default that listens on port 80
is used.")
  (db-host
   (string "localhost")
   "Database host name.")
  (db-port
   (number 5432)
   "Database port.")
  (db-name
   (string "zabbix")
   "Database name.")
  (db-user
   (string "zabbix")
   "Database user.")
  (db-password
   (string "")
   "Database password.  Please, use @code{db-secret-file} instead.")
  (db-secret-file
   (string "")
   "Secret file which will be appended to @file{zabbix.conf.php} file.  This
file contains credentials for use by Zabbix front-end.  You are expected to
create it manually.")
  (zabbix-host
   (string "localhost")
   "Zabbix server hostname.")
  (zabbix-port
   (number 10051)
   "Zabbix server port."))

(define (zabbix-front-end-config config)
  (match-record config <zabbix-front-end-configuration>
    (db-host db-port db-name db-user db-password db-secret-file
             zabbix-host zabbix-port %location)
    (mixed-text-file "zabbix.conf.php"
                     "\
<?php
// Zabbix GUI configuration file.
global $DB;

$DB['TYPE']     = 'POSTGRESQL';
$DB['SERVER']   = '" db-host "';
$DB['PORT']     = '" (number->string db-port) "';
$DB['DATABASE'] = '" db-name "';
$DB['USER']     = '" db-user "';
$DB['PASSWORD'] = " (let ((file (location-file %location))
                          (line (location-line %location))
                          (column (location-column %location)))
                      (if (string-null? db-password)
                          (if (string-null? db-secret-file)
                              (raise (make-compound-condition
                                      (condition
                                       (&message
                                        (message
                                         (format #f "no '~A' or '~A' field in your '~A' record"
                                                 'db-secret-file 'db-password
                                                 'zabbix-front-end-configuration))))
                                      (condition
                                       (&error-location
                                        (location %location)))))
                              (string-append "trim(file_get_contents('"
                                             db-secret-file "'));\n"))
                          (begin
                            (display-hint (G_ "~a:~a:~a: ~a:
Consider using @code{db-secret-file} instead of @code{db-password} for better
security.")
                                          file line column
                                          'zabbix-front-end-configuration)
                            (format #f "'~a';~%" db-password))))
                     "
// Schema name. Used for IBM DB2 and PostgreSQL.
$DB['SCHEMA'] = '';

// Use IEEE754 compatible value range for 64-bit Numeric (float) history values.
$DB['DOUBLE_IEEE754'] = true;

$ZBX_SERVER      = '" zabbix-host "';
$ZBX_SERVER_PORT = '" (number->string zabbix-port) "';
$ZBX_SERVER_NAME = '';

$IMAGE_FORMAT_DEFAULT = IMAGE_FORMAT_PNG;
")))

(define %maintenance.inc.php
  ;; Empty php file to allow us move zabbix-frontend configs to ‘/etc/zabbix’
  ;; directory.  See ‘install-front-end’ phase in
  ;; (@ (gnu packages monitoring) zabbix-server) package.
    "\
<?php
")

(define (zabbix-front-end-activation config)
  "Return the activation gexp for CONFIG."
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/etc/zabbix")
      (call-with-output-file "/etc/zabbix/maintenance.inc.php"
            (lambda (port)
              (display #$%maintenance.inc.php port)))
      (copy-file #$(zabbix-front-end-config config)
                 "/etc/zabbix/zabbix.conf.php")))

(define zabbix-front-end-service-type
  (service-type
   (name 'zabbix-front-end)
   (extensions
    (list (service-extension activation-service-type
                             zabbix-front-end-activation)
          (service-extension nginx-service-type
                             zabbix-front-end-nginx-extension)
          ;; Make sure php-fpm is instantiated.
          (service-extension php-fpm-service-type
                             (const #t))))
   (default-value (zabbix-front-end-configuration))
   (description
    "Run the zabbix-front-end web interface, which allows users to interact
with Zabbix server.")))

(define (generate-zabbix-front-end-documentation)
  (generate-documentation
   `((zabbix-front-end-configuration
      ,zabbix-front-end-configuration-fields))
   'zabbix-front-end-configuration))
