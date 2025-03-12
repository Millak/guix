;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2016, 2018-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2018 Carlo Zancanaro <carlo@zancanaro.id.au>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
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

(define-module (gnu services shepherd)
  #:use-module (guix ui)
  #:use-module (guix sets)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module ((guix diagnostics)
                #:select (define-with-syntax-properties formatted-message))
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services herd)
  #:use-module (gnu packages admin)
  #:use-module (ice-9 match)
  #:use-module (ice-9 vlist)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:export (shepherd-configuration
            shepherd-configuration?
            shepherd-configuration-shepherd
            shepherd-configuration-services

            shepherd-root-service-type
            %shepherd-root-service
            shepherd-service-type

            shepherd-service
            shepherd-service?
            shepherd-service-documentation
            shepherd-service-provision
            shepherd-service-canonical-name
            shepherd-service-requirement
            shepherd-service-one-shot?
            shepherd-service-respawn-limit
            shepherd-service-respawn-delay
            shepherd-service-respawn?
            shepherd-service-start
            shepherd-service-stop
            shepherd-service-free-form
            shepherd-service-auto-start?
            shepherd-service-modules

            shepherd-action
            shepherd-action?
            shepherd-action-name
            shepherd-action-documentation
            shepherd-action-procedure

            shepherd-configuration-action
            shepherd-trigger-action

            %default-modules

            shepherd-service-file

            shepherd-service-lookup-procedure
            shepherd-service-back-edges
            shepherd-service-upgrade

            user-processes-service-type
            shepherd-timer-service-type
            shepherd-transient-service-type

            system-log-configuration
            system-log-configuration?
            system-log-configuration-provision
            system-log-configuration-requirement
            system-log-configuration-message-destination
            system-log-configuration-date-format
            system-log-configuration-history-size
            system-log-configuration-max-silent-time

            shepherd-system-log-service-type

            assert-valid-graph))

;;; Commentary:
;;;
;;; Instantiating system services as a shepherd configuration file.
;;;
;;; Code:


(define-record-type* <shepherd-configuration>
  shepherd-configuration make-shepherd-configuration
  shepherd-configuration?
  (shepherd shepherd-configuration-shepherd
            (default shepherd-1.0))               ;file-like
  (services shepherd-configuration-services
            (default '()))) ; list of <shepherd-service>

(define (shepherd-boot-gexp config)
  "Return a gexp starting the shepherd service."
  (let ((shepherd (shepherd-configuration-shepherd config))
        (services (shepherd-configuration-services config)))
  #~(begin
      ;; Keep track of the booted system.
      (false-if-exception (delete-file "/run/booted-system"))

      ;; Make /run/booted-system, an indirect GC root, point to the store item
      ;; /run/current-system points to.  Use 'canonicalize-path' rather than
      ;; 'readlink' to make sure we get the store item.
      (symlink (canonicalize-path "/run/current-system")
               "/run/booted-system")

      ;; Ensure open file descriptors are close-on-exec so shepherd doesn't
      ;; inherit them.
      (let loop ((fd 3))
        (when (< fd 1024)
          (false-if-exception
           (let ((flags (fcntl fd F_GETFD)))
             (when (zero? (logand flags FD_CLOEXEC))
               (fcntl fd F_SETFD (logior FD_CLOEXEC flags)))))
          (loop (+ fd 1))))

      ;; Start shepherd.
      (execl #$(file-append shepherd "/bin/shepherd")
             "shepherd" "--config"
             #$(shepherd-configuration-file services shepherd)))))

(define shepherd-packages
  (compose list shepherd-configuration-shepherd))

(define shepherd-root-service-type
  (service-type
   (name 'shepherd-root)
   ;; Extending the root shepherd service (aka. PID 1) happens by
   ;; concatenating the list of services provided by the extensions.
   (compose concatenate)
   (extend (lambda (config extra-services)
             (shepherd-configuration
               (inherit config)
               (services (append (shepherd-configuration-services config)
                                 extra-services)))))
   (extensions (list (service-extension boot-service-type
                                        shepherd-boot-gexp)
                     (service-extension profile-service-type
                                        shepherd-packages)))
   (default-value (shepherd-configuration))
   (description
    "Run the GNU Shepherd as PID 1---i.e., the operating system's first
process.  The Shepherd takes care of managing services such as daemons by
ensuring they are started and stopped in the right order.")))

(define %shepherd-root-service
  ;; The root shepherd service, aka. PID 1.  Its parameter is a
  ;; <shepherd-configuration>.
  (service shepherd-root-service-type))

(define-syntax shepherd-service-type
  (syntax-rules (description)
    "Return a <service-type> denoting a simple shepherd service--i.e., the type
for a service that extends SHEPHERD-ROOT-SERVICE-TYPE and nothing else.  When
DEFAULT is given, use it as the service's default value."
    ((_ service-name proc default (description text))
     (service-type
      (name service-name)
      (extensions
       (list (service-extension shepherd-root-service-type
                                (compose list proc))))
      (default-value default)
      (description text)))
    ((_ service-name proc (description text))
     (service-type
      (name service-name)
      (extensions
       (list (service-extension shepherd-root-service-type
                                (compose list proc))))
      (description text)))))

(define %default-imported-modules
  ;; Default set of modules imported for a service's consumption.
  '((guix build utils)
    (guix build syscalls)))

(define %default-modules
  ;; Default set of modules visible in a service's file.
  `((shepherd service)
    ((guix build utils) #:hide (delete))
    (guix build syscalls)))

(define-with-syntax-properties (validate-provision (provision properties))
  (match provision
    (((? symbol?) ..1) provision)
    (_
     (raise
      (make-compound-condition
       (condition
        (&error-location
         (location (source-properties->location properties))))
       (formatted-message
        (G_ "'provision' must be a non-empty list of symbols")))))))

(define-record-type* <shepherd-service>
  shepherd-service make-shepherd-service
  shepherd-service?
  (documentation shepherd-service-documentation        ;string
                 (default "[No documentation.]"))
  (provision     shepherd-service-provision            ;list of symbols
                 (sanitize validate-provision))
  (requirement   shepherd-service-requirement          ;list of symbols
                 (default '()))
  (one-shot?     shepherd-service-one-shot?            ;Boolean
                 (default #f))
  (respawn?      shepherd-service-respawn?             ;Boolean
                 (default #t))
  (respawn-limit shepherd-service-respawn-limit
                 (default #f))
  (respawn-delay shepherd-service-respawn-delay
                 (default #f))
  (free-form     shepherd-service-free-form            ;#f | g-expression (service)
                 (default #f))
  (start         shepherd-service-start                ;g-expression (procedure)
                 (default #~(const #t)))
  (stop          shepherd-service-stop                 ;g-expression (procedure)
                 (default #~(const #f)))
  (actions       shepherd-service-actions              ;list of <shepherd-action>
                 (default '()))
  (auto-start?   shepherd-service-auto-start?          ;Boolean
                 (default #t))
  (modules       shepherd-service-modules              ;list of module names
                 (default %default-modules)))

(define-record-type* <shepherd-action>
  shepherd-action make-shepherd-action
  shepherd-action?
  (name          shepherd-action-name)            ;symbol
  (procedure     shepherd-action-procedure)       ;gexp
  (documentation shepherd-action-documentation))  ;string

(define (shepherd-service-canonical-name service)
  "Return the 'canonical name' of SERVICE."
  (first (shepherd-service-provision service)))

(define (assert-valid-graph services)
  "Raise an error if SERVICES does not define a valid shepherd service graph,
for instance if a service requires a nonexistent service, or if more than one
service uses a given name.

These are constraints that shepherd's 'register-service' verifies but we'd
better verify them here statically than wait until PID 1 halts with an
assertion failure."
  (define provisions
    ;; The set of provisions (symbols).  Bail out if a symbol is given more
    ;; than once.
    (fold (lambda (service set)
            (define (assert-unique symbol)
              (when (set-contains? set symbol)
                (raise (condition
                        (&message
                         (message
                          (format #f (G_ "service '~a' provided more than once")
                                  symbol)))))))

            (for-each assert-unique (shepherd-service-provision service))
            (fold set-insert set (shepherd-service-provision service)))
          (setq 'shepherd)
          services))

  (define (assert-satisfied-requirements service)
    ;; Bail out if the requirements of SERVICE aren't satisfied.
    (for-each (lambda (requirement)
                (unless (set-contains? provisions requirement)
                  (raise (condition
                          (&message
                           (message
                            (format #f (G_ "service '~a' requires '~a', \
which is not provided by any service")
                                    (match (shepherd-service-provision service)
                                      ((head . _) head)
                                      (_          service))
                                    requirement)))))))
              (shepherd-service-requirement service)))

  (for-each assert-satisfied-requirements services))

(define %store-characters
  ;; Valid store characters; see 'checkStoreName' in the daemon.
  (string->char-set
   "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz+-._?="))

(define (shepherd-service-file-name service)
  "Return the file name where the initialization code for SERVICE is to be
stored."
  (let ((provisions (string-join (map symbol->string
                                      (shepherd-service-provision service)))))
    (string-append "shepherd-"
                   (string-map (lambda (chr)
                                 (if (char-set-contains? %store-characters chr)
                                     chr
                                     #\-))
                               provisions)
                   ".scm")))

(define (shepherd-service-file/regular service)
  "Return a file defining SERVICE, a service whose 'free-form' field is #f."
  (scheme-file (shepherd-service-file-name service)
               (with-imported-modules %default-imported-modules
                 #~(begin
                     (use-modules #$@(shepherd-service-modules service))

                     (service
                      '#$(shepherd-service-provision service)
                       #:documentation '#$(shepherd-service-documentation service)
                       #:requirement '#$(shepherd-service-requirement service)

                       ;; The 'one-shot?' slot is new in Shepherd 0.6.0.
                       ;; Older versions ignore it.
                       #:one-shot? '#$(shepherd-service-one-shot? service)

                       #:respawn? '#$(shepherd-service-respawn? service)
                       #$@(if (shepherd-service-respawn-limit service)
                              `(#:respawn-limit
                                ,(shepherd-service-respawn-limit service))
                              '())
                       #$@(if (shepherd-service-respawn-delay service)
                              `(#:respawn-delay
                                ,(shepherd-service-respawn-delay service))
                              '())
                       #:start #$(shepherd-service-start service)
                       #:stop #$(shepherd-service-stop service)
                       #:actions
                       (actions
                        #$@(map (match-lambda
                                  (($ <shepherd-action> name proc doc)
                                   #~(#$name #$doc #$proc)))
                                (shepherd-service-actions service))))))))

(define (shepherd-service-file/free-form service)
  "Return a file defining SERVICE, a service whose 'free-form' field is set."
  (scheme-file (shepherd-service-file-name service)
               (with-imported-modules %default-imported-modules
                 #~(begin
                     (use-modules #$@(shepherd-service-modules service))

                     #$(shepherd-service-free-form service)))))

(define (shepherd-service-file service)
  "Return a file defining SERVICE."
  (if (shepherd-service-free-form service)
      (shepherd-service-file/free-form service)
      (shepherd-service-file/regular service)))

(define (scm->go file shepherd)
  "Compile FILE, which contains code to be loaded by shepherd's config file,
and return the resulting '.go' file. SHEPHERD is used as shepherd package."
  (define shepherd&co
    (cons shepherd
          (match (lookup-package-input shepherd "guile-fibers")
            (#f '())
            (fibers (list fibers)))))

  (let-system (system target)
    (with-extensions shepherd&co
      (computed-file (string-append (basename (scheme-file-name file) ".scm")
                                    ".go")
                     #~(begin
                         (use-modules (system base compile)
                                      (system base target))

                         ;; Do the same as the Shepherd's 'load-in-user-module'.
                         (let ((env (make-fresh-user-module)))
                           (module-use! env (resolve-interface '(shepherd service)))
                           (with-target #$(or target #~%host-type)
                             (lambda _
                               (compile-file #$file #:output-file #$output
                                             #:env env)))))

                     ;; It's faster to build locally than to download.
                     #:options '(#:local-build? #t
                                 #:substitutable? #f)))))

(define (shepherd-configuration-action file)
  "Return a 'configuration' action to display FILE, which should be the name
of the service's configuration file."
  (shepherd-action
   (name 'configuration)
   (documentation "Display the name of this service's configuration file.")
   (procedure #~(lambda (_)
                  (format #t "~a~%" #$file)
                  #$file))))

(define shepherd-trigger-action
  ;; Action to trigger a timer.
  (shepherd-action
   (name 'trigger)
   (documentation "Trigger immediate execution of this timer.")
   (procedure #~trigger-timer)))

(define (shepherd-configuration-file services shepherd)
  "Return the shepherd configuration file for SERVICES.  SHEPHERD is used
as shepherd package."
  (assert-valid-graph services)

  (let ((files (map shepherd-service-file services))
        (scm->go (cute scm->go <> shepherd)))
    (define config
      #~(begin
          (use-modules (srfi srfi-1))

          (define (make-user-module)
            ;; Copied from (shepherd support), where it's private.
            (let ((m (make-fresh-user-module)))
              (module-use! m (resolve-interface '(shepherd service)))
              m))

          ;; Specify the default environment visible to all the services.
          ;; Without this statement, all the environment variables of PID 1
          ;; are inherited by child services.
          (default-environment-variables
            '("PATH=/run/current-system/profile/bin"))

          ;; Booting off a DVD, especially on a slow machine, can make
          ;; everything slow.  Thus, increase the timeout compared to the
          ;; default 5s in the Shepherd 0.7.0.  See
          ;; <https://bugs.gnu.org/40572>.
          (default-pid-file-timeout 30)

          ;; Load service files one by one; filter out those that could not be
          ;; loaded--e.g., due to an unbound variable--such that an error in
          ;; one service definition does not prevent the system from booting.
          (register-services
           (parameterize ((current-warning-port (%make-void-port "w")))
             (filter-map (lambda (file)
                           (with-exception-handler
                               (lambda (exception)
                                 (format #t "Exception caught \
while loading '~a': ~s~%"
                                         file exception)
                                 #f)
                             (lambda ()
                               (save-module-excursion
                                (lambda ()
                                  (set-current-module (make-user-module))
                                  (load-compiled file))))
                             #:unwind? #t))
                         '#$(map scm->go files))))

          (format #t "starting services...~%")
          (let ((services-to-start
                 '#$(append-map shepherd-service-provision
                                (filter shepherd-service-auto-start?
                                        services))))
            (start-in-the-background services-to-start)

            ;; Hang up stdin.  At this point, we assume that 'start' methods
            ;; that required user interaction on the console (e.g.,
            ;; 'cryptsetup open' invocations, post-fsck emergency REPL) have
            ;; completed.  User interaction becomes impossible after this
            ;; call; this avoids situations where services wrongfully lead
            ;; PID 1 to read from stdin (the console), which users may not
            ;; have access to (see <https://bugs.gnu.org/23697>).
            (redirect-port (open "/dev/null" (logior O_RDONLY O_CLOEXEC))
                           (current-input-port)))))

    (scheme-file "shepherd.conf" config)))

(define* (shepherd-service-lookup-procedure services
                                            #:optional
                                            (provision
                                             shepherd-service-provision))
  "Return a procedure that, when passed a symbol, return the item among
SERVICES that provides this symbol.  PROVISION must be a one-argument
procedure that takes a service and returns the list of symbols it provides."
  (let ((services (fold (lambda (service result)
                          (fold (cut vhash-consq <> service <>)
                                result
                                (provision service)))
                        vlist-null
                        services)))
    (lambda (name)
      (match (vhash-assq name services)
        ((_ . service) service)
        (#f            #f)))))

(define* (shepherd-service-back-edges services
                                      #:key
                                      (provision shepherd-service-provision)
                                      (requirement shepherd-service-requirement))
  "Return a procedure that, when given a <shepherd-service> from SERVICES,
returns the list of <shepherd-service> that depend on it.

Use PROVISION and REQUIREMENT as one-argument procedures that return the
symbols provided/required by a service."
  (define provision->service
    (shepherd-service-lookup-procedure services provision))

  (define edges
    (fold (lambda (service edges)
            (fold (lambda (requirement edges)
                    (vhash-consq (provision->service requirement) service
                                 edges))
                  edges
                  (requirement service)))
          vlist-null
          services))

  (lambda (service)
    (vhash-foldq* cons '() service edges)))

(define (shepherd-service-upgrade live target)
  "Return two values: the subset of LIVE (a list of <live-service>) that needs
to be unloaded, and the subset of LIVE that needs to be restarted to complete
their upgrade."
  (define (essential? service)
    (memq (first (live-service-provision service))
          '(root shepherd)))

  (define lookup-target
    (shepherd-service-lookup-procedure target
                                       shepherd-service-provision))

  (define lookup-live
    (shepherd-service-lookup-procedure live
                                       live-service-provision))

  (define live-service-dependents
    (shepherd-service-back-edges live
                                 #:provision live-service-provision
                                 #:requirement live-service-requirement))

  (define (obsolete? service)
    (match (lookup-target (first (live-service-provision service)))
      (#f (every obsolete? (live-service-dependents service)))
      (_  #f)))

  (define to-restart
    ;; Restart services that appear in TARGET and are currently running.
    (filter-map (lambda (service)
                  (and=> (any lookup-live
                              (shepherd-service-provision service))
                         (lambda (live)
                           (and (live-service-running live)
                                live))))
                target))

  (define to-unload
    ;; Unload services that are no longer required.  Essential services must
    ;; be kept and transient services such as inetd child services should be
    ;; kept as well--they'll vanish eventually.
    (remove (lambda (live)
              (or (essential? live)
                  (live-service-transient? live)))
            (filter obsolete? live)))

  (values to-unload to-restart))


;;;
;;; User processes.
;;;

(define (user-processes-shepherd-service requirements)
  "Return the 'user-processes' Shepherd service with dependencies on
REQUIREMENTS (a list of service names).

This is a synchronization point used to make sure user processes and daemons
get started only after crucial initial services have been started---file
system mounts, etc.  This is similar to the 'sysvinit' target in systemd."
  (define grace-delay
    ;; Delay after sending SIGTERM and before sending SIGKILL.
    4)

  (list (shepherd-service
         (documentation "When stopped, terminate all user processes.")
         (provision '(user-processes))
         (requirement requirements)
         (start #~(const #t))
         (stop #~(lambda _
                   (display "sending all processes the TERM signal\n")

                   (kill -1 SIGTERM)
                   (sleep #$grace-delay)
                   (kill -1 SIGKILL)

                   (let wait ()
                     (let ((pids (processes)))
                       (unless (equal? '(1) pids)
                         (format #t "waiting for process termination\
 (processes left: ~s)~%"
                                 pids)
                         (sleep 1)
                         (wait))))

                   (display "all processes have been terminated\n")
                   #f))
         (respawn? #f))))

(define user-processes-service-type
  (service-type
   (name 'user-processes)
   (extensions (list (service-extension shepherd-root-service-type
                                        user-processes-shepherd-service)))
   (compose concatenate)
   (extend append)

   ;; The value is the list of Shepherd services 'user-processes' depends on.
   ;; Extensions can add new services to this list.
   (default-value '())

   (description "The @code{user-processes} service is responsible for
terminating all the processes so that the root file system can be re-mounted
read-only, just before rebooting/halting.  Processes still running after a few
seconds after @code{SIGTERM} has been sent are terminated with
@code{SIGKILL}.")))


;;;
;;; Timer and transient service maker.
;;;

(define shepherd-timer-service-type
  (shepherd-service-type
   'shepherd-timer
   (lambda (requirement)
     (shepherd-service
      (provision '(timer))
      (requirement requirement)
      (modules '((shepherd service timer)))
      (free-form #~(timer-service
                    '#$provision
                    #:requirement '#$requirement))))
   '(user-processes)
   (description "The Shepherd @code{timer} service lets you schedule commands
dynamically, similar to the @code{at} command that your grandparents would use
on that Slackware they got on a floppy disk.  For example, consider this
command:

@example
herd schedule timer at 07:00 -- mpg123 Music/alarm.mp3
@end example

It does exactly what you would expect.")))

(define shepherd-transient-service-type
  (shepherd-service-type
   'shepherd-transient
   (lambda (requirement)
     (shepherd-service
      (provision '(transient))
      (requirement requirement)
      (modules '((shepherd service transient)))
      (free-form #~(transient-service
                    '#$provision
                    #:requirement '#$requirement))))
   '(user-processes)
   (description "The Shepherd @code{transient} service lets you run commands
asynchronously, in the background, similar to @command{systemd-run}, as in
this example:

@example
herd spawn transient -E SSH_AUTH_SOCK=$SSH_AUTH_SOCK -- \\
  rsync -e ssh -vur . backup.example.org:
@end example

This runs @command{rsync} in the background, as a service that you can inspect
with @command{herd status} and stop with @command{herd stop}.")))


;;;
;;; System log.
;;;

(define (gexp-or-false? x)
  (or (gexp? x) (not x)))

(define (gexp-or-integer? x)
  (or (gexp? x) (integer? x)))

(define (gexp-or-string? x)
  (or (gexp? x) (string? x)))

(define (gexp-or-string-or-false? x)
  (or (gexp-or-string? x) (not x)))

(define-configuration/no-serialization system-log-configuration
  (provision
   (list-of-symbols '(system-log syslogd))
   "The name(s) of the system log service.")
  (requirement
   (list-of-symbols '(root-file-system))
   "Dependencies of the system log service.")
  (kernel-log-file
   (gexp-or-string-or-false #~(kernel-log-file))
   "File from which kernel messages are read, @file{/dev/kmsg} by default.")
  (message-destination
   (gexp-or-false #f)
   "This gexp must evaluate to a procedure that, when passed a log message,
returns the list of files to write it to; @code{#f} is equivalent to using
@code{(default-message-destination-procedure)}.  @xref{System Log Service,,,
shepherd, The GNU Shepherd Manual}, for information on how to write that
procedure.")
  (date-format
   (gexp-or-string #~default-logfile-date-format)
   "String or string-valued gexp specifying how to format timestamps in log
file.  It must be a valid string for @code{strftime} (@pxref{Time,,, guile,
GNU Guile Reference Manual}), including delimiting space---e.g., @code{\"%c
\"} for a format identical to that of traditional syslogd implementations.")
  (history-size
   (gexp-or-integer #~(default-log-history-size))
   "Number of logging messages kept in memory for the purposes of making them
available to @command{herd status system-log}.")
  (max-silent-time
   (gexp-or-integer #~(default-max-silent-time))
   "Time after which a mark is written to log files if nothing was logged
during that time frame."))

(define shepherd-system-log-service-type
  (shepherd-service-type
   'shepherd-system-log
   (lambda (config)
     (match-record config <system-log-configuration>
       (provision requirement message-destination
        date-format history-size max-silent-time)
       (shepherd-service
        (documentation "Shepherd's built-in system log (syslogd).")
        (provision (system-log-configuration-provision config))
        (requirement (system-log-configuration-requirement config))
        (modules '((shepherd service system-log)
                   ((shepherd support) #:select (default-logfile-date-format))
                   ((shepherd logger) #:select (default-log-history-size))))
        (free-form
         #~(system-log-service #:provision '#$provision
                               #:requirement '#$requirement

                               ;; XXX: As of Shepherd 1.0.1,
                               ;; 'default-message-destination-procedure' is not
                               ;; exported, hence this conditional.
                               #$@(match message-destination
                                    (#f #~())
                                    (value #~(#:message-destination #$value)))

                               #:date-format #$date-format
                               #:history-size #$history-size
                               #:max-silent-time #$max-silent-time)))))
   (system-log-configuration)
   (description
    "The Shepherd's @code{system-log} service plays the role of traditional
@command{syslogd} program, reading data logged by daemons to @file{/dev/log}
and writing it to several files in @file{/var/log} according to user-provided
dispatching rules.")))
