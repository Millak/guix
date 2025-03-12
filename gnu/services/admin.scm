;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2016-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2023 Giacomo Leidi <goodoldpaul@autistici.org>
;;; Copyright © 2024 Gabriel Wicki <gabriel@erlikon.ch>
;;; Copyright © 2024 Richard Sent <richard@freakingpenguin.com>
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

(define-module (gnu services admin)
  #:use-module (gnu system file-systems)
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages base)
                #:select (canonical-package findutils coreutils sed))
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages certs)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services mcron)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system accounts)
  #:use-module ((gnu system shadow) #:select (account-service-type))
  #:use-module ((guix store) #:select (%store-prefix))
  #:use-module (guix deprecation)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:export (log-rotation-configuration
            log-rotation-configuration?
            log-rotation-configuration-provision
            log-rotation-configuration-requirement
            log-rotation-configuration-calendar-event
            log-rotation-configuration-external-log-files
            log-rotation-configuration-compression
            log-rotation-configuration-expiry
            log-rotation-configuration-size-threshold
            log-rotation-service-type

            %default-rotations
            %rotated-files

            log-rotation
            log-rotation?
            log-rotation-frequency
            log-rotation-files
            log-rotation-options
            log-rotation-post-rotate
            %default-log-rotation-options

            rottlog-configuration
            rottlog-configuration?
            rottlog-configuration-rottlog
            rottlog-configuration-rc-file
            rottlog-configuration-rotations
            rottlog-configuration-jobs
            rottlog-service
            rottlog-service-type

            log-cleanup-service-type
            log-cleanup-configuration
            log-cleanup-configuration?
            log-cleanup-configuration-directory
            log-cleanup-configuration-expiry
            log-cleanup-configuration-schedule

            file-database-service-type
            file-database-configuration
            file-database-configuration?
            file-database-configuration-package
            file-database-configuration-schedule
            file-database-configuration-excluded-directories
            %default-file-database-update-schedule
            %default-file-database-excluded-directories

            package-database-service-type
            package-database-configuration
            package-database-configuration?
            package-database-configuration-package
            package-database-configuration-schedule
            package-database-configuration-method
            package-database-configuration-channels

            unattended-upgrade-service-type
            unattended-upgrade-configuration
            unattended-upgrade-configuration?
            unattended-upgrade-configuration-operating-system-file
            unattended-upgrade-configuration-operating-system-expression
            unattended-upgrade-configuration-channels
            unattended-upgrade-configuration-schedule
            unattended-upgrade-configuration-services-to-restart
            unattended-upgrade-configuration-system-expiration
            unattended-upgrade-configuration-maximum-duration
            unattended-upgrade-configuration-log-file

            resize-file-system-service-type
            resize-file-system-configuration
            resize-file-system-configuration?
            resize-file-system-configuration-file-system
            resize-file-system-configuration-cloud-utils
            resize-file-system-configuration-e2fsprogs
            resize-file-system-configuration-btrfs-progs
            resize-file-system-configuration-bcachefs-tools))

;;; Commentary:
;;;
;;; This module provides basic system administration tools: log rotation,
;;; unattended upgrades, etc.
;;;
;;; Code:


;;;
;;; Shepherd's log rotation service.
;;;

(define %default-log-rotation-calendar-event
  ;; Default calendar event when log rotation is triggered.
  #~(calendar-event #:minutes '(0)
                    #:hours '(22)
                    #:days-of-week '(sunday)))

(define (gexp-or-integer? x)
  (or (gexp? x) (integer? x)))

(define-configuration log-rotation-configuration
  (provision
   (list-of-symbols '(log-rotation))
   "The name(s) of the log rotation Shepherd service."
   empty-serializer)
  (requirement
   (list-of-symbols (if for-home? '() '(user-processes)))
   "Dependencies of the log rotation Shepherd service."
   empty-serializer)
  (calendar-event
   (gexp %default-log-rotation-calendar-event)
   "Gexp containing the @dfn{calendar event} when log rotation occurs.
@xref{Timers,,, shepherd, The GNU Shepherd Manual}, for more information on
calendar events."
   empty-serializer)
  (external-log-files
   (list-of-strings '())
   "List of file names, external log files that should also be
rotated."
   empty-serializer)
  (compression
   (symbol 'zstd)
   "The compression method used for rotated log files, one of
@code{'none}, @code{'gzip}, and @code{'zstd}."
   empty-serializer)
  (expiry
   (gexp-or-integer #~(%default-log-expiry))
   "Age in seconds after which a log file is deleted."
   empty-serializer)
  (size-threshold
   (gexp-or-integer #~(%default-rotation-size-threshold))
   "Size in bytes below which a log file is @emph{not} rotated."
   empty-serializer))

(define (log-rotation-shepherd-services config)
  (list (shepherd-service
         (provision (log-rotation-configuration-provision config))
         (requirement (log-rotation-configuration-requirement config))
         (modules '((shepherd service timer)      ;for 'calendar-event'
                    (shepherd service log-rotation)))
         (free-form #~(log-rotation-service
                       #$(log-rotation-configuration-calendar-event config)
                       #:provision
                       '#$(log-rotation-configuration-provision config)
                       #:requirement
                       '#$(log-rotation-configuration-requirement config)
                       #:external-log-files
                       '#$(log-rotation-configuration-external-log-files
                           config)
                       #:compression
                       '#$(log-rotation-configuration-compression config)
                       #:expiry
                       #$(log-rotation-configuration-expiry config)
                       #:rotation-size-threshold
                       #$(log-rotation-configuration-size-threshold
                          config))))))

(define log-rotation-service-type
  (service-type
   (name 'log-rotation)
   (description
    "Periodically rotate log files using the Shepherd's log rotation service.
Run @command{herd status log-rotation} to view its status, @command{herd files
log-rotation} to list files subject to log rotation.")
   (extensions (list (service-extension shepherd-root-service-type
                                        log-rotation-shepherd-services)))
   (compose concatenate)
   (extend (lambda (config log-files)
             (log-rotation-configuration
              (inherit config)
              (external-log-files
               (append (log-rotation-configuration-external-log-files config)
                       log-files)))))
   (default-value (log-rotation-configuration))))


;;;
;;; Rottlog + mcron.
;;;

(define-record-type* <log-rotation> log-rotation make-log-rotation
  log-rotation?
  (files       log-rotation-files)                ;list of strings
  (frequency   log-rotation-frequency             ;symbol
               (default 'weekly))
  (post-rotate log-rotation-post-rotate           ;#f | gexp
               (default #f))
  (options     log-rotation-options               ;list of strings
               (default %default-log-rotation-options)))

(define %default-log-rotation-options
  ;; Default log rotation options: append ".gz" to file names.
  '("storefile @FILENAME.@COMP_EXT"
    "notifempty"))

(define %rotated-files
  ;; Syslog files subject to rotation.
  '("/var/log/messages" "/var/log/secure" "/var/log/debug"
    "/var/log/maillog" "/var/log/mcron.log"))

(define %default-rotations
  (list (log-rotation                             ;syslog files
         (files %rotated-files)

         (frequency 'weekly)
         (options `(;; These files are worth keeping for a few weeks.
                    "rotate 16"
                    ;; Run post-rotate once per rotation
                    "sharedscripts"

                    ,@%default-log-rotation-options))
         ;; Restart syslogd after rotation.
         (post-rotate #~(let ((pid (call-with-input-file "/var/run/syslog.pid"
                                     read)))
                          (kill pid SIGHUP))))
        (log-rotation
         (files '("/var/log/guix-daemon.log"))
         (options `("rotate 4"                    ;don't keep too many of them
                    ,@%default-log-rotation-options)))))

(define (log-rotation->config rotation)
  "Return a string-valued gexp representing the rottlog configuration snippet
for ROTATION."
  (define post-rotate
    (let ((post (log-rotation-post-rotate rotation)))
      (and post
           (program-file "rottlog-post-rotate.scm" post))))

  #~(let ((post #$post-rotate))
      (string-append (string-join '#$(log-rotation-files rotation) ",")
                     " {"
                     #$(string-join (log-rotation-options rotation)
                                    "\n  " 'prefix)
                     (if post
                         (string-append "\n  postrotate\n    " post
                                        "\n  endscript\n")
                         "")
                     "\n}\n")))

(define (log-rotations->/etc-entries rotations)
  "Return the list of /etc entries for ROTATIONS, a list of <log-rotation>."
  (define (frequency-file frequency rotations)
    (computed-file (string-append "rottlog." (symbol->string frequency))
                   #~(call-with-output-file #$output
                       (lambda (port)
                         (for-each (lambda (str)
                                     (display str port))
                                   (list #$@(map log-rotation->config
                                                 rotations)))))))

  (let* ((frequencies (delete-duplicates
                       (map log-rotation-frequency rotations)))
         (table       (fold (lambda (rotation table)
                              (vhash-consq (log-rotation-frequency rotation)
                                           rotation table))
                            vlist-null
                            rotations)))
    (map (lambda (frequency)
           `(,(symbol->string frequency)
             ,(frequency-file frequency
                              (vhash-foldq* cons '() frequency table))))
         frequencies)))

(define (default-jobs rottlog)
  (list #~(job '(next-hour '(0))                  ;midnight
               #$(file-append rottlog "/sbin/rottlog"))
        #~(job '(next-hour '(12))                 ;noon
               #$(file-append rottlog "/sbin/rottlog"))))

(define-record-type* <rottlog-configuration>
  rottlog-configuration make-rottlog-configuration
  rottlog-configuration?
  (rottlog            rottlog-configuration-rottlog             ;file-like
                      (default rottlog))
  (rc-file            rottlog-configuration-rc-file             ;file-like
                      (default (file-append rottlog "/etc/rc")))
  (rotations          rottlog-configuration-rotations           ;list of <log-rotation>
                      (default %default-rotations))
  (jobs               rottlog-configuration-jobs                ;list of <mcron-job>
                      (default #f)))

(define (rottlog-etc config)
  `(("rottlog"
     ,(file-union "rottlog"
                  (cons `("rc" ,(rottlog-configuration-rc-file config))
                        (log-rotations->/etc-entries
                         (rottlog-configuration-rotations config)))))))

(define (rottlog-jobs-or-default config)
  (or (rottlog-configuration-jobs config)
      (default-jobs (rottlog-configuration-rottlog config))))

;; TODO: Deprecated; remove sometime after 2025-06-15.
(define-deprecated rottlog-service-type
  log-rotation-service-type
  (service-type
   (name 'rottlog)
   (description
    "Periodically rotate log files using GNU@tie{}Rottlog and GNU@tie{}mcron.
Old log files are removed or compressed according to the configuration.

This service is deprecated and slated for removal after 2025-06-15.")
   (extensions (list (service-extension etc-service-type rottlog-etc)
                     (service-extension mcron-service-type
                                        rottlog-jobs-or-default)

                     ;; Add Rottlog to the global profile so users can access
                     ;; the documentation.
                     (service-extension profile-service-type
                                        (compose list rottlog-configuration-rottlog))))
   (compose concatenate)
   (extend (lambda (config rotations)
             (rottlog-configuration
              (inherit config)
              (rotations (append (rottlog-configuration-rotations config)
                                 rotations)))))
   (default-value (rottlog-configuration))))


;;;
;;; Build log removal.
;;;

(define-record-type* <log-cleanup-configuration>
  log-cleanup-configuration make-log-cleanup-configuration
  log-cleanup-configuration?
  (directory log-cleanup-configuration-directory) ;string
  (expiry    log-cleanup-configuration-expiry     ;integer (seconds)
             (default (* 6 30 24 3600)))
  (schedule  log-cleanup-configuration-schedule   ;string or gexp
             (default "30 12 01,08,15,22 * *")))

(define (log-cleanup-program directory expiry)
  (program-file "delete-old-logs"
                (with-imported-modules '((guix build utils))
                  #~(begin
                      (use-modules (guix build utils))

                      (let* ((now  (car (gettimeofday)))
                             (logs (find-files #$directory
					       (lambda (file stat)
					         (> (- now (stat:mtime stat))
						    #$expiry)))))
                        (format #t "deleting ~a log files from '~a'...~%"
                                (length logs) #$directory)
                        (for-each delete-file logs))))))

(define (log-cleanup-shepherd-services configuration)
  (match-record configuration <log-cleanup-configuration>
                (directory expiry schedule)
    (let ((program (log-cleanup-program directory expiry)))
      (list (shepherd-service
             (provision '(log-cleanup))
             (requirement '(user-processes))
             (modules '((shepherd service timer)))
             (start #~(make-timer-constructor
                       #$(if (string? schedule)
                             #~(cron-string->calendar-event #$schedule)
                             schedule)
                       (command '(#$program))))
             (stop #~(make-timer-destructor))
             (actions (list shepherd-trigger-action))
             (documentation "Periodically delete old log files."))))))

(define log-cleanup-service-type
  (service-type
   (name 'log-cleanup)
   (extensions
    (list (service-extension shepherd-root-service-type
                             log-cleanup-shepherd-services)))
   (description
    "Periodically delete old log files.")))


;;;
;;; File databases.
;;;

(define %default-file-database-update-schedule
  ;; Default mcron schedule for the periodic 'updatedb' job: once every
  ;; Sunday.
  "10 23 * * 0")

(define %default-file-database-excluded-directories
  ;; Regexps of directories excluded from the 'locate' database.
  (list (%store-prefix)
        "/tmp" "/var/tmp" "/var/cache" ".*/\\.cache"
        "/run/udev"))

(define (string-or-gexp? obj)
  (or (string? obj) (gexp? obj)))

(define string-list?
  (match-lambda
    (((? string?) ...) #t)
    (_ #f)))

(define-configuration/no-serialization file-database-configuration
  (package
    (file-like (let-system (system target)
                 ;; Unless we're cross-compiling, avoid pulling a second copy
                 ;; of findutils.
                 (if target
                     findutils
                     (canonical-package findutils))))
    "The GNU@tie{}Findutils package from which the @command{updatedb} command
is taken.")
  (schedule
   (string-or-gexp %default-file-database-update-schedule)
   "String or G-exp denoting an mcron schedule for the periodic
@command{updatedb} job (@pxref{Guile Syntax,,, mcron, GNU@tie{}mcron}).")
  (excluded-directories
   (string-list %default-file-database-excluded-directories)
   "List of regular expressions of directories to ignore when building the
file database.  By default, this includes @file{/tmp} and @file{/gnu/store};
the latter should instead be indexed by @command{guix locate} (@pxref{Invoking
guix locate}).  This list is passed to the @option{--prunepaths} option of
@command{updatedb} (@pxref{Invoking updatedb,,, find, GNU@tie{}Findutils})."))

(define (file-database-shepherd-services configuration)
  (match-record configuration <file-database-configuration>
    (package schedule excluded-directories)
    (let ((updatedb (program-file
                     "updatedb"
                     #~(begin
                         ;; 'updatedb' is a shell script that expects various
                         ;; commands in $PATH.
                         (setenv "PATH"
                                 (string-append #$package "/bin:"
                                                #$(canonical-package coreutils)
                                                "/bin:"
                                                #$(canonical-package sed)
                                                "/bin"))
                         (execl #$(file-append package "/bin/updatedb")
                                "updatedb"
                                #$(string-append "--prunepaths="
                                                 (string-join
                                                  excluded-directories)))))))
      (list (shepherd-service
             (provision '(file-database-update))
             (requirement '(user-processes))
             (modules '((shepherd service timer)))
             (start #~(make-timer-constructor
                       #$(if (string? schedule)
                             #~(cron-string->calendar-event #$schedule)
                             schedule)
                       (command '(#$updatedb))
                       #:wait-for-termination? #t))
             (stop #~(make-timer-destructor))
             (documentation
              "Periodically update the system-wide file database that can be
queried by the 'locate' command.")
             (actions (list shepherd-trigger-action)))))))

(define file-database-service-type
  (service-type
   (name 'file-database)
   (extensions (list (service-extension shepherd-root-service-type
                                        file-database-shepherd-services)))
   (description
    "Periodically update the file database used by the @command{locate} command,
which lets you search for files by name.  The database is created by running
the @command{updatedb} command.")
   (default-value (file-database-configuration))))

(define %default-package-database-update-schedule
  ;; Default mcron schedule for the periodic 'guix locate --update' job: once
  ;; every Monday.
  "10 23 * * 1")

(define-configuration/no-serialization package-database-configuration
  (package (file-like guix)
           "The Guix package to use.")
  (schedule (string-or-gexp
             %default-package-database-update-schedule)
            "String or G-exp denoting an mcron schedule for the periodic
@command{guix locate --update} job (@pxref{Guile Syntax,,, mcron,
GNU@tie{}mcron}).")
  (method    (symbol 'store)
             "Indexing method for @command{guix locate}.  The default value,
@code{'store}, yields a more complete database but is relatively expensive in
terms of CPU and input/output.")
  (channels (gexp #~%default-channels)
            "G-exp denoting the channels to use when updating the database
(@pxref{Channels})."))

(define (package-database-mcron-jobs configuration)
  (match-record configuration <package-database-configuration>
    (package schedule method channels)
    (let ((channels (scheme-file "channels.scm" channels)))
      (list #~(job #$schedule
                   ;; XXX: The whole thing's running as "root" just because it
                   ;; needs write access to /var/cache/guix/locate.
                   (string-append #$(file-append package "/bin/guix")
                                  " time-machine -C " #$channels
                                  " -- locate --update --method="
                                  #$(symbol->string method)))))))

(define package-database-service-type
  (service-type
   (name 'package-database)
   (extensions (list (service-extension mcron-service-type
                                        package-database-mcron-jobs)))
   (description
    "Periodically update the package database used by the @code{guix locate} command,
which lets you search for packages that provide a given file.")
   (default-value (package-database-configuration))))


;;;
;;; Unattended upgrade.
;;;

(define-record-type* <unattended-upgrade-configuration>
  unattended-upgrade-configuration make-unattended-upgrade-configuration
  unattended-upgrade-configuration?
  (operating-system-file unattended-upgrade-operating-system-file
                         (default "/run/current-system/configuration.scm"))
  (operating-system-expression unattended-upgrade-operating-system-expression
                               (default #f))
  (schedule             unattended-upgrade-configuration-schedule
                        (default "30 01 * * 0"))
  (channels             unattended-upgrade-configuration-channels
                        (default #~%default-channels))
  (reboot?              unattended-upgrade-configuration-reboot?
                        (default #f))
  (services-to-restart  unattended-upgrade-configuration-services-to-restart
                        (default '(unattended-upgrade)))
  (system-expiration    unattended-upgrade-system-expiration
                        (default (* 3 30 24 3600)))
  (maximum-duration     unattended-upgrade-maximum-duration
                        (default 3600))
  (log-file             unattended-upgrade-configuration-log-file
                        (default %unattended-upgrade-log-file)))

(define %unattended-upgrade-log-file
  "/var/log/unattended-upgrade.log")

(define (unattended-upgrade-shepherd-services config)
  (define channels
    (scheme-file "channels.scm"
                 (unattended-upgrade-configuration-channels config)))

  (define log
    (unattended-upgrade-configuration-log-file config))

  (define schedule
    (unattended-upgrade-configuration-schedule config))

  (define services
    (unattended-upgrade-configuration-services-to-restart config))

  (define reboot?
    (unattended-upgrade-configuration-reboot? config))

  (define expiration
    (unattended-upgrade-system-expiration config))

  (define config-file
    (unattended-upgrade-operating-system-file config))

  (define expression
    (unattended-upgrade-operating-system-expression config))

  (define arguments
    (if expression
        #~(list "-e" (object->string '#$expression))
        #~(list #$config-file)))

  (define code
    (with-imported-modules (source-module-closure '((guix build utils)
                                                    (gnu services herd)))
      #~(begin
          (use-modules (guix build utils)
                       (gnu services herd)
                       (srfi srfi-34))

          (setvbuf (current-output-port) 'line)
          (setvbuf (current-error-port) 'line)

          ;; 'guix time-machine' needs X.509 certificates to authenticate the
          ;; Git host.
          (setenv "SSL_CERT_DIR"
                  #$(file-append nss-certs "/etc/ssl/certs"))

          (format #t "starting upgrade...~%")
          (guard (c ((invoke-error? c)
                     (report-invoke-error c)))
            (apply invoke #$(file-append guix "/bin/guix")
                   "time-machine" "-C" #$channels
                   "--" "system" "reconfigure" #$arguments)

            ;; 'guix system delete-generations' fails when there's no
            ;; matching generation.  Thus, catch 'invoke-error?'.
            (guard (c ((invoke-error? c)
                       (report-invoke-error c)))
              (invoke #$(file-append guix "/bin/guix")
                      "system" "delete-generations"
                      #$(string-append (number->string expiration)
                                       "s")))

            (unless #$reboot?
              ;; Rebooting effectively restarts services anyway and execution
              ;; would be halted here if mcron is restarted.
              (format #t "restarting services...~%")
              (for-each restart-service '#$services))

            ;; XXX: If this service has been restarted, this is not reached.
            (format #t "upgrade complete~%")

            ;; Stopping the root shepherd service triggers a reboot.
            (when #$reboot?
              (format #t "rebooting system~%")
              (force-output) ;ensure the entire log is written.
              (stop-service 'root))))))

  (define upgrade
    (program-file "unattended-upgrade" code))

  (list (shepherd-service
         (provision '(unattended-upgrade))
         (requirement '(user-processes networking))
         (modules '((shepherd service timer)))
         (start #~(make-timer-constructor
                   #$(if (string? schedule)
                         #~(cron-string->calendar-event #$schedule)
                         schedule)
                   (command '(#$upgrade))

                   #:log-file #$log

                   ;; Make sure the upgrade doesn't take too long.
                   #:max-duration
                   #$(unattended-upgrade-maximum-duration config)

                   ;; Wait for the previous attempt to terminate before trying
                   ;; again.
                   #:wait-for-termination? #t))
         (stop #~(make-timer-destructor))
         (actions (list shepherd-trigger-action)))))

(define unattended-upgrade-service-type
  (service-type
   (name 'unattended-upgrade)
   (extensions
    (list (service-extension shepherd-root-service-type
                             unattended-upgrade-shepherd-services)))
   (description
    "Periodically upgrade the system from the current configuration.")
   (default-value (unattended-upgrade-configuration))))

;;;
;;; Resize file system.
;;;

(define-record-type* <resize-file-system-configuration>
  resize-file-system-configuration make-resize-file-system-configuration
  resize-file-system-configuration?
  (file-system    resize-file-system-file-system)
  (cloud-utils    resize-file-system-cloud-utils
                  (default cloud-utils))
  (e2fsprogs      resize-file-system-e2fsprogs
                  (default e2fsprogs))
  (btrfs-progs    resize-file-system-btrfs-progs
                  (default btrfs-progs))
  (bcachefs-tools resize-file-system-bcachefs-tools
                  (default bcachefs-tools)))

(define (resize-file-system-shepherd-service config)
  "Returns a <shepherd-service> for resize-file-system-service for CONFIG."
  (match-record config <resize-file-system-configuration>
                (file-system cloud-utils e2fsprogs btrfs-progs
                             bcachefs-tools)
    (let ((fs-spec (file-system->spec file-system)))
      (shepherd-service
       (documentation "Resize a file system. Intended for Guix Systems that
are booted from a system image flashed onto a larger medium.")
       ;; XXX: This could be extended with file-system info.
       (provision '(resize-file-system))
       (requirement '(user-processes))
       (one-shot? #t)
       (respawn? #f)
       (modules '((guix build utils)
                  (gnu build file-systems)
                  (gnu system file-systems)
                  (ice-9 control)
                  (ice-9 match)
                  (ice-9 ftw)
                  (ice-9 rdelim)
                  (srfi srfi-34)))
       (start (with-imported-modules (source-module-closure
                                      '((guix build utils)
                                        (gnu build file-systems)
                                        (gnu system file-systems)))
                #~(lambda _
                    (use-modules (guix build utils)
                                 (gnu build file-systems)
                                 (gnu system file-systems)
                                 (ice-9 control)
                                 (ice-9 match)
                                 (ice-9 ftw)
                                 (ice-9 rdelim)
                                 (srfi srfi-34))

                    (define file-system
                      (spec->file-system '#$fs-spec))

                    ;; Shepherd recommends the start constructor takes <1
                    ;; minute, canonicalize-device-spec will hang for up to
                    ;; max-trials seconds (20 seconds) if an invalid device is
                    ;; connected. Revisit this if max-trials increases.
                    (define device (canonicalize-device-spec
                                    (file-system-device file-system)))

                    (define grow-partition-command
                      (let* ((sysfs-device
                              (string-append "/sys/class/block/"
                                             (basename device)))
                             (partition-number
                              (with-input-from-file
                                  (string-append sysfs-device
                                                 "/partition")
                                read-line))
                             (parent (string-append
                                      "/dev/"
                                      (basename (dirname (readlink sysfs-device))))))
                        (list #$(file-append cloud-utils "/bin/growpart")
                              parent partition-number)))

                    (define grow-filesystem-command
                      (match (file-system-type file-system)
                        ((or "ext2" "ext3" "ext4")
                         (list #$(file-append e2fsprogs "/sbin/resize2fs") device))
                        ("btrfs"
                         (list #$(file-append btrfs-progs "/bin/btrfs")
                               "filesystem" "resize" device))
                        ("bcachefs"
                         (list #$(file-append bcachefs-tools "/sbin/bcachefs")
                               "device" "resize" device))
                        (e (error "Unsupported filesystem type" e))))

                    (let/ec return
                      (guard (c ((and (invoke-error? c)
                                      ;; growpart NOCHANGE exits with 1. It is
                                      ;; unlikely the partition was resized
                                      ;; while the file system was not. Just
                                      ;; exit.
                                      (equal? (invoke-error-exit-status c) 1))
                                 (format (current-error-port)
                                         "The device ~a is already resized.~%" device)
                                 ;; Must return something or Shepherd considers
                                 ;; the service perpetually starting.
                                 (return 0)))
                        (apply invoke grow-partition-command))
                      (apply invoke grow-filesystem-command)))))))))

(define resize-file-system-service-type
  (service-type
   (name 'resize-file-system)
   (description "Resize a partition and the underlying file system during boot.")
   (extensions
    (list
     (service-extension shepherd-root-service-type
                        (compose list resize-file-system-shepherd-service))))))

;;; admin.scm ends here
