;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
;;; Copyright © 2021 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
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

(define-module (gnu services linux)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix i18n)
  #:use-module (guix ui)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu packages linux)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-171)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (earlyoom-configuration
            earlyoom-configuration?
            earlyoom-configuration-earlyoom
            earlyoom-configuration-minimum-available-memory
            earlyoom-configuration-minimum-free-swap
            earlyoom-configuration-prefer-regexp
            earlyoom-configuration-avoid-regexp
            earlyoom-configuration-memory-report-interval
            earlyoom-configuration-ignore-positive-oom-score-adj?
            earlyoom-configuration-show-debug-messages?
            earlyoom-configuration-send-notification-command
            earlyoom-service-type

            fstrim-configuration
            fstrim-configuration?
            fstrim-configuration-package
            fstrim-configuration-schedule
            fstrim-configuration-listed-in
            fstrim-configuration-verbose?
            fstrim-configuration-quiet-unsupported?
            fstrim-configuration-extra-arguments
            fstrim-service-type

            kernel-module-loader-service-type

            cachefilesd-configuration
            cachefilesd-configuration?
            cachefilesd-configuration-cachefilesd
            cachefilesd-configuration-debug-output?
            cachefilesd-configuration-use-syslog?
            cachefilesd-configuration-scan?
            cachefilesd-configuration-cache-directory
            cachefilesd-configuration-cache-name
            cachefilesd-configuration-security-context
            cachefilesd-configuration-pause-culling-for-block-percentage
            cachefilesd-configuration-pause-culling-for-file-percentage
            cachefilesd-configuration-resume-culling-for-block-percentage
            cachefilesd-configuration-resume-culling-for-file-percentage
            cachefilesd-configuration-pause-caching-for-block-percentage
            cachefilesd-configuration-pause-caching-for-file-percentage
            cachefilesd-configuration-log2-table-size
            cachefilesd-configuration-cull?
            cachefilesd-configuration-trace-function-entry-in-kernel-module
            cachefilesd-configuration-trace-function-exit-in-kernel-module
            cachefilesd-configuration-trace-internal-checkpoints-in-kernel-module
            cachefilesd-service-type

            rasdaemon-configuration
            rasdaemon-configuration?
            rasdaemon-configuration-record?
            rasdaemon-service-type

            zram-device-configuration
            zram-device-configuration?
            zram-device-configuration-size
            zram-device-configuration-compression-algorithm
            zram-device-configuration-memory-limit
            zram-device-configuration-priority
            zram-device-service-type))


;;;
;;; Early OOM daemon.
;;;

(define-record-type* <earlyoom-configuration>
  earlyoom-configuration make-earlyoom-configuration
  earlyoom-configuration?
  (earlyoom earlyoom-configuration-earlyoom
            (default earlyoom))
  (minimum-available-memory earlyoom-configuration-minimum-available-memory
                            (default 10)) ; in percent
  (minimum-free-swap earlyoom-configuration-minimum-free-swap
                     (default 10))      ; in percent
  (prefer-regexp earlyoom-configuration-prefer-regexp ; <string>
                 (default #f))
  (avoid-regexp earlyoom-configuration-avoid-regexp  ; <string>
                (default #f))
  (memory-report-interval earlyoom-configuration-memory-report-interval
                          (default 0)) ; in seconds; 0 means disabled
  (ignore-positive-oom-score-adj?
   earlyoom-configuration-ignore-positive-oom-score-adj? (default #f))
  (run-with-higher-priority? earlyoom-configuration-run-with-higher-priority?
                             (default #f))
  (show-debug-messages? earlyoom-configuration-show-debug-messages?
                        (default #f))
  (send-notification-command
   earlyoom-configuration-send-notification-command  ; <string>
   (default #f)))

(define (earlyoom-configuration->command-line-args config)
  "Translate a <earlyoom-configuration> object to its command line arguments
representation."
  (match config
    (($ <earlyoom-configuration> earlyoom minimum-available-memory
                                 minimum-free-swap prefer-regexp avoid-regexp
                                 memory-report-interval
                                 ignore-positive-oom-score-adj?
                                 run-with-higher-priority? show-debug-messages?
                                 send-notification-command)
     `(,(file-append earlyoom "/bin/earlyoom")
       ,@(if minimum-available-memory
             (list "-m" (format #f "~s" minimum-available-memory))
             '())
       ,@(if minimum-free-swap
             (list "-s" (format #f "~s" minimum-free-swap))
             '())
       ,@(if prefer-regexp
             (list "--prefer" prefer-regexp)
             '())
       ,@(if avoid-regexp
             (list "--avoid" avoid-regexp)
             '())
       "-r" ,(format #f "~s" memory-report-interval)
       ,@(if ignore-positive-oom-score-adj?
             (list "-i")
             '())
       ,@(if run-with-higher-priority?
             (list "-p")
             '())
       ,@(if show-debug-messages?
             (list "-d")
             '())
       ,@(if send-notification-command
             (list "-N" send-notification-command)
             '())))))

(define (earlyoom-shepherd-service config)
  (shepherd-service
   (documentation "Run the Early OOM daemon.")
   (provision '(earlyoom))
   (requirement '(user-processes))
   (start #~(make-forkexec-constructor
             '#$(earlyoom-configuration->command-line-args config)
             #:log-file "/var/log/earlyoom.log"))
   (stop #~(make-kill-destructor))))

(define earlyoom-service-type
  (service-type
   (name 'earlyoom)
   (default-value (earlyoom-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list earlyoom-shepherd-service))))
   (description "Run @command{earlyoom}, a daemon that quickly responds to
@acronym{OOM, out-of-memory} conditions by terminating relevant processes.")))


;;;
;;; fstrim
;;;

(define (shepherd-calendar-event? x)
  (or (string? x) (gexp? x)))

(define-maybe list-of-strings (prefix fstrim-))

(define (fstrim-serialize-boolean field-name value)
  (list (format #f "~:[~;--~a~]" value
                ;; Drop trailing '?' character.
                (string-drop-right (symbol->string field-name) 1))))

(define (fstrim-serialize-list-of-strings field-name value)
  (list (string-append "--" (symbol->string field-name))
        #~(string-join '#$value ":")))

(define-configuration fstrim-configuration
  (package
    (file-like util-linux)
    "The package providing the @command{fstrim} command."
    empty-serializer)
  (schedule
   (shepherd-calendar-event "0 0 * * 0")
   "Schedule for launching @command{fstrim}, expressed as a string in
traditional cron syntax or as a gexp evaluating to a Shepherd calendar
event (@pxref{Timers,,, shepherd, The GNU Shepherd Manual}).  By default this
is set to run weekly on Sunday at 00:00."
   empty-serializer)
  ;; The following are fstrim-related options.
  (listed-in
   (maybe-list-of-strings '("/etc/fstab" "/proc/self/mountinfo"))
   ;; Note: documentation sourced from the fstrim manpage.
   "List of files in fstab or kernel mountinfo format.  All missing or
empty files are silently ignored.  The evaluation of the list @emph{stops}
after the first non-empty file.  File systems with @code{X-fstrim.notrim} mount
option in fstab are skipped.")
  (verbose?
   (boolean #t)
   "Verbose execution.")
  (quiet-unsupported?
   (boolean #t)
   "Suppress error messages if trim operation (ioctl) is unsupported.")
  (extra-arguments
   maybe-list-of-strings
   "Extra options to append to @command{fstrim} (run @samp{man fstrim} for
more information)."
   (serializer
    (lambda (_ value)
      (if (maybe-value-set? value)
          value '()))))
  (prefix fstrim-))

(define (serialize-fstrim-configuration config)
  (list-transduce (compose (base-transducer config) tconcatenate)
                  rcons
                  fstrim-configuration-fields))

(define (fstrim-shepherd-services config)
  (match-record config <fstrim-configuration>
    (package schedule)
    (list (shepherd-service
           (provision '(fstrim))
           (requirement '(user-processes))
           (modules '((shepherd service timer)))
           (start #~(make-timer-constructor
                     #$(if (string? schedule)
                           #~(cron-string->calendar-event #$schedule)
                           schedule)
                     (command
                      (list #$(file-append package "/sbin/fstrim")
                            #$@(serialize-fstrim-configuration config)))
                     #:wait-for-termination? #t))
           (stop #~(make-timer-destructor))
           (documentation "Periodically run the 'fstrim' command.")
           (actions (list shepherd-trigger-action))))))

(define fstrim-service-type
  (service-type
   (name 'fstrim)
   (extensions
    (list (service-extension shepherd-root-service-type
                             fstrim-shepherd-services)))
   (description "Discard unused blocks from file systems.")
   (default-value (fstrim-configuration))))


;;;
;;; Kernel module loader.
;;;

(define kernel-module-loader-shepherd-service
  (match-lambda
    ((and (? list? kernel-modules) ((? string?) ...))
     (shepherd-service
      (documentation "Load kernel modules.")
      (provision '(kernel-module-loader))
      (requirement '(udev))
      (one-shot? #t)
      (modules `((srfi srfi-1)
                 (srfi srfi-34)
                 (srfi srfi-35)
                 (rnrs io ports)
                 ,@%default-modules))
      (start
       #~(lambda _
           (cond
            ((null? '#$kernel-modules) #t)
            ((file-exists? "/proc/sys/kernel/modprobe")
             (let ((modprobe (call-with-input-file
                                 "/proc/sys/kernel/modprobe" get-line)))
               (guard (c ((message-condition? c)
                          (format (current-error-port) "~a~%"
                                  (condition-message c))
                          #f))
                 (every (lambda (module)
                          (invoke/quiet modprobe "--" module))
                        '#$kernel-modules))))
            (else
             (format (current-error-port) "error: ~a~%"
                     "Kernel is missing loadable module support.")
             #f))))))))

(define kernel-module-loader-service-type
  (service-type
   (name 'kernel-module-loader)
   (description "Load kernel modules.")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list kernel-module-loader-shepherd-service))))
   (compose concatenate)
   (extend append)
   (default-value '())))


;;;
;;; Cachefilesd, an FS-Cache daemon
;;;

(define (serialize-string variable-symbol value)
  #~(format #f "~a ~a~%" #$(symbol->string variable-symbol) #$value))

(define-maybe string)

(define (non-negative-integer? val)
  (and (exact-integer? val) (not (negative? val))))

(define (serialize-non-negative-integer variable-symbol value)
  #~(format #f "~a ~d~%" #$(symbol->string variable-symbol) #$value))

(define-maybe non-negative-integer)

(define (make-option-serializer option-symbol)
  (lambda (variable-symbol text)
    (if (maybe-value-set? text)
        #~(format #f "~a ~a~%" #$(symbol->string option-symbol) #$text)
        "")))

(define (make-percentage-threshold-serializer threshold-symbol)
  (lambda (variable-symbol percentage)
    (if (maybe-value-set? percentage)
        #~(format #f "~a ~a%~%" #$(symbol->string threshold-symbol) #$percentage)
        "")))

(define-configuration cachefilesd-configuration
  (cachefilesd
   (file-like cachefilesd)
   "The cachefilesd package to use."
   (serializer empty-serializer))

  ;; command-line options
  (debug-output?
   (boolean #f)
   "Print debugging output to stderr."
   (serializer empty-serializer))

  (use-syslog?
   (boolean #t)
   "Log to syslog facility instead of stdout."
   (serializer empty-serializer))

  ;; culling is part of the configuration file
  ;; despite the name of the command-line option
  (scan?
   (boolean #t)
   "Scan for cacheable objects."
   (serializer empty-serializer))

  ;; sole required field in the configuration file
  (cache-directory
   maybe-string
   "Location of the cache directory."
   (serializer (make-option-serializer 'dir)))

  (cache-name
   (maybe-string "CacheFiles")
   "Name of cache (keep unique)."
   (serializer (make-option-serializer 'tag)))

  (security-context
   maybe-string
   "SELinux security context."
   (serializer (make-option-serializer 'secctx)))

  ;; percentage thresholds in the configuration file
  (pause-culling-for-block-percentage
   (maybe-non-negative-integer 7)
   "Pause culling when available blocks exceed this percentage."
   (serializer (make-percentage-threshold-serializer 'brun)))

  (pause-culling-for-file-percentage
   (maybe-non-negative-integer 7)
   "Pause culling when available files exceed this percentage."
   (serializer (make-percentage-threshold-serializer 'frun)))

  (resume-culling-for-block-percentage
   (maybe-non-negative-integer 5)
   "Start culling when available blocks drop below this percentage."
   (serializer (make-percentage-threshold-serializer 'bcull)))

  (resume-culling-for-file-percentage
   (maybe-non-negative-integer 5)
   "Start culling when available files drop below this percentage."
   (serializer (make-percentage-threshold-serializer 'fcull)))

  (pause-caching-for-block-percentage
   (maybe-non-negative-integer 1)
   "Pause further allocations when available blocks drop below this percentage."
   (serializer (make-percentage-threshold-serializer 'bstop)))

  (pause-caching-for-file-percentage
   (maybe-non-negative-integer 1)
   "Pause further allocations when available files drop below this percentage."
   (serializer (make-percentage-threshold-serializer 'fstop)))

  ;; run time optimizations in the configuration file
  (log2-table-size
   (maybe-non-negative-integer 12)
   "Size of tables holding cullable objects in logarithm of base 2."
   (serializer (make-option-serializer 'culltable)))

  (cull?
   (boolean #t)
   "Create free space by culling (consumes system load)."
   (serializer
    (lambda (variable-symbol value)
      (if value "" "nocull\n"))))

  ;; kernel module debugging in the configuration file
  (trace-function-entry-in-kernel-module?
   (boolean #f)
   "Trace function entry in the kernel module (for debugging)."
   (serializer empty-serializer))

  (trace-function-exit-in-kernel-module?
   (boolean #f)
   "Trace function exit in the kernel module (for debugging)."
   (serializer empty-serializer))

  (trace-internal-checkpoints-in-kernel-module?
   (boolean #f)
   "Trace internal checkpoints in the kernel module (for debugging)."
   (serializer empty-serializer)))

(define (serialize-cachefilesd-configuration configuration)
  (mixed-text-file
   "cachefilesd.conf"
   (serialize-configuration configuration cachefilesd-configuration-fields)))

(define (cachefilesd-shepherd-service config)
  "Return a list of <shepherd-service> for cachefilesd for CONFIG."
  (match-record
      config <cachefilesd-configuration> (cachefilesd
                                          debug-output?
                                          use-syslog?
                                          scan?
                                          cache-directory)
      (let ((configuration-file (serialize-cachefilesd-configuration config)))
        (shepherd-service
         (documentation "Run the cachefilesd daemon for FS-Cache.")
         (provision '(cachefilesd))
         (requirement (append '(user-processes file-systems)
                              (if use-syslog? '(syslogd) '())))
         (start #~(begin
                    (and=> #$(maybe-value cache-directory) mkdir-p)
                    (make-forkexec-constructor
                     `(#$(file-append cachefilesd "/sbin/cachefilesd")
                       ;; do not detach
                       "-n"
                       #$@(if debug-output? '("-d") '())
                       #$@(if use-syslog? '() '("-s"))
                       #$@(if scan? '() '("-N"))
                       "-f" #$configuration-file))))
         (stop #~(make-kill-destructor))))))

(define cachefilesd-service-type
  (service-type
   (name 'cachefilesd)
   (description
    "Run the file system cache daemon @command{cachefilesd}, which relies on
the Linux @code{cachefiles} module.")
   (extensions
    (list (service-extension kernel-module-loader-service-type
                             (const '("cachefiles")))
          (service-extension shepherd-root-service-type
                             (compose list cachefilesd-shepherd-service))))
   (default-value (cachefilesd-configuration))))


;;;
;;; Reliability, Availability, and Serviceability (RAS) daemon
;;;

(define-record-type* <rasdaemon-configuration>
  rasdaemon-configuration make-rasdaemon-configuration
  rasdaemon-configuration?
  (record? rasdaemon-configuration-record? (default #f)))

(define (rasdaemon-configuration->command-line-args config)
  "Translate <rasdaemon-configuration> to its command line arguments
  representation"
  (let ((record? (rasdaemon-configuration-record? config)))
    `(,(file-append rasdaemon "/sbin/rasdaemon")
      "--foreground" ,@(if record? '("--record") '()))))

(define (rasdaemon-activation config)
  (let ((record? (rasdaemon-configuration-record? config))
        (rasdaemon-dir "/var/lib/rasdaemon"))
    (with-imported-modules '((guix build utils))
      #~(if #$record? (mkdir-p #$rasdaemon-dir)))))

(define (rasdaemon-shepherd-service config)
  (shepherd-service
   (documentation "Run rasdaemon")
   (provision '(rasdaemon))
   (requirement '(user-processes syslogd))
   (start #~(make-forkexec-constructor
             '#$(rasdaemon-configuration->command-line-args config)))
   (stop #~(make-kill-destructor))))

(define rasdaemon-service-type
  (service-type
   (name 'rasdaemon)
   (default-value (rasdaemon-configuration))
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list rasdaemon-shepherd-service))
          (service-extension activation-service-type rasdaemon-activation)))
   (compose concatenate)
   (description "Run @command{rasdaemon}, the RAS monitor")))


;;;
;;; Zram device
;;;

(define-record-type* <zram-device-configuration>
  zram-device-configuration make-zram-device-configuration
  zram-device-configuration?
  (size                     zram-device-configuration-size
                            (default "1G"))     ; string or integer
  (compression-algorithm    zram-device-configuration-compression-algorithm
                            (default 'lzo))     ; symbol
  (memory-limit             zram-device-configuration-memory-limit
                            (default 0))        ; string or integer
  (priority                 zram-device-configuration-priority
                            (default #f)        ; integer | #f
                            (delayed) ; to avoid printing the deprecation
                                      ; warning multiple times
                            (sanitize warn-zram-priority-change)))

(define-with-syntax-properties
  (warn-zram-priority-change (priority properties))
  (if (eqv? priority -1)
      (begin
        (warning (source-properties->location properties)
                 (G_ "using -1 for zram priority is deprecated~%"))
        (display-hint (G_ "Use #f or leave as default instead (@pxref{Linux \
Services})."))
        #f)
      priority))

(define (zram-device-configuration->udev-string config)
  "Translate a <zram-device-configuration> into a string which can be
placed in a udev rules file."
  (match config
    (($ <zram-device-configuration> size compression-algorithm memory-limit priority)
     (string-append
       "KERNEL==\"zram0\", "
       "ATTR{comp_algorithm}=\"" (symbol->string compression-algorithm) "\" "
       (if (not (or (equal? "0" size)
                    (equal? 0 size)))
         (string-append "ATTR{disksize}=\"" (if (number? size)
                                              (number->string size)
                                              size)
                        "\" ")
         "")
       (if (not (or (equal? "0" memory-limit)
                    (equal? 0 memory-limit)))
         (string-append "ATTR{mem_limit}=\"" (if (number? memory-limit)
                                               (number->string memory-limit)
                                               memory-limit)
                        "\" ")
         "")
       "RUN+=\"/run/current-system/profile/sbin/mkswap /dev/zram0\" "
       "RUN+=\"/run/current-system/profile/sbin/swapon "
       ;; TODO: Revert to simply use 'priority' after removing the deprecation
       ;; warning and the delayed property of the field.
       (let ((priority* (force priority)))
         (if priority*
             (format #f "--priority ~a " priority*)
             ""))
       "/dev/zram0\"\n"))))

(define %zram-device-config
  `("modprobe.d/zram.conf"
    ,(plain-file "zram.conf"
                 "options zram num_devices=1")))

(define (zram-device-udev-rule config)
  (file->udev-rule "99-zram.rules"
                   (plain-file "99-zram.rules"
                               (zram-device-configuration->udev-string config))))

(define zram-device-service-type
  (service-type
    (name 'zram)
    (default-value (zram-device-configuration))
    (extensions
      (list (service-extension kernel-module-loader-service-type
                               (const (list "zram")))
            (service-extension etc-service-type
                               (const (list %zram-device-config)))
            (service-extension udev-service-type
                               (compose list zram-device-udev-rule))))
    (description "Creates a zram swap device.")))
