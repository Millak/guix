;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>

;;;; Commentary:

;;; Power-related services.

;;;; Code:

(define-module (gnu services power)
  #:use-module (gnu)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages power)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (apcupsd-service-type

            apcupsd-configuration
            apcupsd-configuration-apcupsd
            apcupsd-configuration-shepherd-service-name
            apcupsd-configuration-auto-start?
            apcupsd-configuration-pid-file
            apcupsd-configuration-debug-level
            apcupsd-configuration-run-dir
            apcupsd-configuration-name
            apcupsd-configuration-cable
            apcupsd-configuration-type
            apcupsd-configuration-device
            apcupsd-configuration-poll-time
            apcupsd-configuration-on-batery-delay
            apcupsd-configuration-battery-level
            apcupsd-configuration-remaining-minutes
            apcupsd-configuration-timeout
            apcupsd-configuration-annoy-interval
            apcupsd-configuration-annoy-delay
            apcupsd-configuration-no-logon
            apcupsd-configuration-kill-delay
            apcupsd-configuration-net-server
            apcupsd-configuration-net-server-ip
            apcupsd-configuration-net-server-port
            apcupsd-configuration-net-server-events-file
            apcupsd-configuration-net-server-events-file-max-size
            apcupsd-configuration-class
            apcupsd-configuration-mode
            apcupsd-configuration-stat-time
            apcupsd-configuration-log-stats
            apcupsd-configuration-data-time
            apcupsd-configuration-facility
            apcupsd-configuration-event-handlers

            apcupsd-event-handlers
            apcupsd-event-handlers-modules
            apcupsd-event-handlers-annoyme
            apcupsd-event-handlers-battattach
            apcupsd-event-handlers-battdetach
            apcupsd-event-handlers-changeme
            apcupsd-event-handlers-commfailure
            apcupsd-event-handlers-commok
            apcupsd-event-handlers-doreboot
            apcupsd-event-handlers-doshutdown
            apcupsd-event-handlers-emergency
            apcupsd-event-handlers-endselftest
            apcupsd-event-handlers-failing
            apcupsd-event-handlers-killpower
            apcupsd-event-handlers-loadlimit
            apcupsd-event-handlers-mainsback
            apcupsd-event-handlers-offbattery
            apcupsd-event-handlers-onbattery
            apcupsd-event-handlers-powerout
            apcupsd-event-handlers-remotedown
            apcupsd-event-handlers-runlimit
            apcupsd-event-handlers-startselftest
            apcupsd-event-handlers-timeout))

(define-configuration/no-serialization apcupsd-event-handlers
  (modules
   (gexp #~())
   "Additional modules to import into the generated handler script.")
  (killpower
   (gexp
    #~((wall "Apccontrol doing: apcupsd --killpower on UPS ~a" name)
       (sleep 10)
       (apcupsd "--killpower")
       (wall "Apccontrol has done: apcupsd --killpower on UPS ~a" name)))
   "The handler for the killpower event.")
  (commfailure
   (gexp
    #~((let ((msg (format #f "~a Communications with UPS ~a lost."
                          (gethostname) name)))
         (mail-to-root msg msg))
       (wall "Warning: communications lost with UPS ~a" name)))
   "The handler for the commfailure event.")
  (commok
   (gexp
    #~((let ((msg (format #f "~a Communications with UPS ~a restored."
                          (gethostname) name)))
         (mail-to-root msg msg))
       (wall "Communications restored with UPS ~a" name)))
   "The handler for the commfailure event.")
  (powerout
   (gexp
    #~(#t))
   "The handler for the powerout event.")
  (onbattery
   (gexp
    #~((let ((msg (format #f "~a UPS ~a Power Failure !!!"
                          (gethostname) name)))
         (mail-to-root msg msg))
       (wall "Power failure on UPS ~a.  Running on batteries." name)))
   "The handler for the onbattery event.")
  (offbattery
   (gexp
    #~((let ((msg (format #f "~a UPS ~a Power has returned."
                          (gethostname) name)))
         (mail-to-root msg msg))
       (wall "Power has returned on UPS ~a..." name)))
   "The handler for the offbattery event.")
  (mainsback
   (gexp
    #~((when (file-exists? powerfail-file)
         (wall "Continuing with shutdown."))))
   "The handler for the mainsback event.")
  (failing
   (gexp
    #~((wall "Battery power exhausted on UPS ~a.  Doing shutdown." name)))
   "The handler for the failing event.")
  (timeout
   (gexp
    #~((wall "Battery time limit exceeded on UPS ~a.  Doing shutdown." name)))
   "The handler for the timeout event.")
  (loadlimit
   (gexp
    #~((wall "Remaining battery charge below limit on UPS ~a.  Doing shutdown." name)))
   "The handler for the loadlimit event.")
  (runlimit
   (gexp
    #~((wall "Remaining battery runtime below limit on UPS ~a.  Doing shutdown." name)))
   "The handler for the runlimit event.")
  (doreboot
   (gexp
    #~((wall "UPS ~a initiating Reboot Sequence" name)
       (system* #$(file-append shepherd "/sbin/reboot"))))
   "The handler for the doreboot event.")
  (doshutdown
   (gexp
    #~((wall "UPS ~a initiated Shutdown Sequence" name)
       (system* #$(file-append shepherd "/sbin/halt"))))
   "The handler for the doshutdown event.")
  (annoyme
   (gexp
    #~((wall "Power problems with UPS ~a.  Please logoff." name)))
   "The handler for the annoyme event.")
  (emergency
   (gexp
    #~((wall "Emergency Shutdown.  Possible battery failure on UPS ~a." name)))
   "The handler for the emergency event.")
  (changeme
   (gexp
    #~((let ((msg (format #f "~a UPS ~a battery needs changing NOW."
                          (gethostname) name)))
         (mail-to-root msg msg))
       (wall "Emergency!  Batteries have failed on UPS ~a.  Change them NOW." name)))
   "The handler for the changeme event.")
  (remotedown
   (gexp
    #~((wall "Remote Shutdown.  Beginning Shutdown Sequence.")))
   "The handler for the remotedown event.")
  (startselftest
   (gexp
    #~(#t))
   "The handler for the startselftest event.")
  (endselftest
   (gexp
    #~(#t))
   "The handler for the endselftest event.")
  (battdetach
   (gexp
    #~(#t))
   "The handler for the battdetach event.")
  (battattach
   (gexp
    #~(#t))
   "The handler for the battattach event."))

(define-syntax define-enum
  (lambda (x)
    (syntax-case x ()
      ((_ name values)
       (let* ((datum/name (syntax->datum #'name))
              (datum/predicate (string->symbol
                                (format #f "enum-~a?" datum/name)))
              (datum/serialize (string->symbol
                                (format #f "serialize-enum-~a" datum/name))))
         (with-syntax
             ((predicate (datum->syntax x datum/predicate))
              (serialize (datum->syntax x datum/serialize)))
           #'(begin
               (define (predicate value)
                 (memq value values))
               (define serialize serialize-symbol))))))))

(define mangle-field-name
  (match-lambda
    ('name                            "UPSNAME")
    ('cable                           "UPSCABLE")
    ('type                            "UPSTYPE")
    ('device                          "DEVICE")
    ('poll-time                       "POLLTIME")
    ('lock-dir                        "LOCKFILE")
    ('power-fail-dir                  "PWRFAILDIR")
    ('no-login-dir                    "NOLOGINDIR")
    ('on-batery-delay                 "ONBATTERYDELAY")
    ('battery-level                   "BATTERYLEVEL")
    ('remaining-minutes               "MINUTES")
    ('timeout                         "TIMEOUT")
    ('annoy-interval                  "ANNOY")
    ('annoy-delay                     "ANNOYDELAY")
    ('no-logon                        "NOLOGON")
    ('kill-delay                      "KILLDELAY")
    ('net-server                      "NETSERVER")
    ('net-server-ip                   "NISIP")
    ('net-server-port                 "NISPORT")
    ('net-server-events-file          "EVENTSFILE")
    ('net-server-events-file-max-size "EVENTSFILEMAX")
    ('class                           "UPSCLASS")
    ('mode                            "UPSMODE")
    ('stat-time                       "STATTIME")
    ('stat-file                       "STATFILE")
    ('log-stats                       "LOGSTATS")
    ('data-time                       "DATATIME")
    ('facility                        "FACILITY")))

(define (serialize-string field-name value)
  #~(format #f "~a ~a\n" #$(mangle-field-name field-name) '#$value))
(define serialize-symbol serialize-string)
(define serialize-integer serialize-string)
(define (serialize-boolean field-name value)
  #~(format #f "~a ~a\n"
            #$(mangle-field-name field-name)
            #$(if value "on" "off")))

(define-maybe string)

(define-enum cable '( simple smart ether usb
                      940-0119A 940-0127A 940-0128A 940-0020B 940-0020C
                      940-0023A 940-0024B 940-0024C 940-1524C 940-0024G
                      940-0095A 940-0095B 940-0095C 940-0625A MAM-04-02-2000))
(define-enum type '(apcsmart usb net snmp netsnmp dumb pcnet modbus test))
(define-enum no-logon '(disable timeout percent minutes always))
(define-enum class '(standalone shareslave sharemaster))
(define-enum mode '(disable share))

(define-configuration apcupsd-configuration
  (apcupsd (package apcupsd) "The @code{apcupsd} package to use.")

  (shepherd-service-name
   (symbol 'apcupsd)
   "The name of the shepherd service.  You can add the service multiple times
with different names to manage multiple UPSes."
   empty-serializer)
  (auto-start?
   (boolean #t)
   "Should the shepherd service auto-start?"
   empty-serializer)
  (pid-file
   (string "/run/apcupsd.pid")
   "The file name of the PID file."
   empty-serializer)
  (debug-level
   (integer 0)
   "The logging verbosity.  Bigger number means more logs.  The source code
uses up to @code{300} as debug level value, so a value of @code{999} seems
reasonable to enable all the logs."
   empty-serializer)

  (run-dir
   (string "/run/apcupsd")
   "The directory containing runtime information.  You need to change this if
you desire to run multiple instances of the daemon."
   empty-serializer)

  ;; General configuration parameters
  (name
   maybe-string
   "Use this to give your UPS a name in log files and such.  This is
particularly useful if you have multiple UPSes.  This does not set the EEPROM.
It should be 8 characters or less.")
  (cable
   (enum-cable 'usb)
   "The type of a cable connecting the UPS to your computer.  Possible generic
choices are @code{'simple}, @code{'smart}, @code{'ether} and
@code{'usb}.

Alternatively, a specific cable model number may be used: @code{'940-0119A},
@code{'940-0127A}, @code{'940-0128A}, @code{'940-0020B}, @code{'940-0020C},
@code{'940-0023A}, @code{'940-0024B}, @code{'940-0024C}, @code{'940-1524C},
@code{'940-0024G}, @code{'940-0095A}, @code{'940-0095B}, @code{'940-0095C},
@code{'940-0625A}, @code{'M-04-02-2000}.")
  (type
   (enum-type 'usb)
   "The type of the UPS you have.

@table @code
@item apcsmart
Newer serial character device, appropriate for SmartUPS models using a serial
cable (not an USB).

@item usb
Most new UPSes are an USB.

@item net
Network link to a master apcupsd through apcupsd's Network Information Server.
This is used if the UPS powering your computer is connected to a different
computer for monitoring.

@item snmp
SNMP network link to an SNMP-enabled UPS device.

@item netsnmp
Same as the SNMP above but requires use of the net-snmp library.  Unless you
have a specific need for this old driver, you should use the @code{'snmp}
instead.

@item dumb
An old serial character device for use with simple-signaling UPSes.

@item pcnet
A PowerChute Network Shutdown protocol which can be used as an alternative to
an SNMP with the AP9617 family of smart slot cards.

@item modbus
A serial device for use with newest SmartUPS models supporting the MODBUS
protocol.

@end table")
  (device
   (string "")
   "For USB UPSes, usually you want to set this to an empty string (the
default).  For other UPS types, you must specify an appropriate port or
address.

@table @code
@item apcsmart
Set to the appropriate @file{/dev/tty**} device.

@item usb
A null string setting enables auto-detection, which is the best choice for
most installations.

@item net
Set to @code{@var{hostname}:@var{port}}.

@item snmp
Set to @code{@var{hostname}:@var{port}:@var{vendor}:@var{community}}.  The
@var{hostname} is the ip address or hostname of the UPS on the network.  The
@var{vendor} can be can be \"APC\" or \"APC_NOTRAP\".  \"APC_NOTRAP\" will
disable SNMP trap catching; you usually want \"APC\".  The @var{port} is
usually 161.  The @var{community} is usually \"private\".

@item netsnmp
Same as the @code{'snmp}.

@item dumb
Set to the appropriate @file{/dev/tty**} device.

@item pcnet
Set to @code{@var{ipaddr}:@var{username}:@var{passphrase}:@var{port}}.  The
@var{ipaddr} is the IP address of the UPS management card.  The @var{username}
and the @var{passphrase} are the credentials for which the card has been
configured.  The @var{port} is the port number on which to listen for messages
from the UPS, normally 3052.  If this parameter is empty or missing, the
default of 3052 will be used.

@item modbus
Set to the appropriate @file{/dev/tty**} device.  You can also leave it empty
for MODBUS over USB or set to the serial number of the UPS.

@end table")
  (poll-time
   (integer 60)
   "The interval (in seconds) at which apcupsd polls the UPS for status.  This
setting applies both to directly-attached UPSes (apcsmart, usb, dumb) and
networked UPSes (net, snmp).  Lowering this setting will improve the apcupsd's
responsiveness to certain events at the cost of higher CPU utilization.")

  ;; Configuration parameters used during power failures
  (on-batery-delay
   (integer 6)
   "The time in seconds from when a power failure is detected until we react
to it with an onbattery event.  The @code{'powerout} event will be triggered
immediately when a power failure is detected.  However, the @code{'onbattery}
event will be trigger only after this delay.")
  (battery-level
   (integer 5)
   "If during a power failure, the remaining battery percentage (as reported
by the UPS) is below or equal to this value, the apcupsd will initiate a
system shutdown.

@quotation Note
@code{battery-level}, @code{remaining-minutes}, and @code{timeout} work
in a conjunction, so the first that occurs will cause the initation of a
shutdown.
@end quotation")
  (remaining-minutes
   (integer 3)
   "If during a power failure, the remaining runtime in minutes (as calculated
internally by the UPS) is below or equal to this value, apcupsd will initiate
a system shutdown.

@quotation Note
@code{battery-level}, @code{remaining-minutes}, and @code{timeout} work
in a conjunction, so the first that occurs will cause the initation of a
shutdown.
@end quotation")
  (timeout
   (integer 0)
   "If during a power failure, the UPS has run on batteries for this many
seconds or longer, apcupsd will initiate a system shutdown.  The value of 0
disables this timer.

@quotation Note
@code{battery-level}, @code{remaining-minutes}, and @code{timeout} work
in a conjunction, so the first that occurs will cause the initation of a
shutdown.
@end quotation")
  (annoy-interval
   (integer 300)
   "The time in seconds between annoying users (via the @code{'annoyme} event)
to sign off prior to system shutdown.  0 disables.")
  (annoy-delay
   (integer 60)
   "The initial delay in seconds after a power failure before warning users to
get off the system.")
  (no-logon
   (enum-no-logon 'disable)
   "The condition which determines when users are prevented from logging in
during a power failure.")
  (kill-delay
   (integer 0)
   "If this is non-zero, the apcupsd will continue running after a shutdown
has been requested, and after the specified time in seconds attempt to kill
the power.  This is for use on systems where apcupsd cannot regain control
after a shutdown.")

  ;; Configuration statements for Network Information Server
  (net-server
   (boolean #f)
   "If enabled, a network information server process will be started.")
  (net-server-ip
   (string "127.0.0.1")
   "An IP address on which the NIS server will listen for incoming
connections.")
  (net-server-port
   (integer 3551)
   "An IP port on which the NIS server will listen for incoming connections.")
  (net-server-events-file
   maybe-string
   "If you want the last few EVENTS to be available over the network by the
network information server, you must set this to a file name.")
  (net-server-events-file-max-size
   (integer 10)
   "The maximum size of the events file in kilobytes.")
  ;; Configuration statements used if sharing a UPS with more than one machine
  (class (enum-class 'standalone)
    "Normally standalone unless you share an UPS using an APC ShareUPS card.")
  (mode (enum-mode 'disable)
    "Normally disable unless you share an UPS using an APC ShareUPS card.")
  ;; Configuration statements to control apcupsd system logging
  (stat-time
   (integer 0)
   "The time interval in seconds between writing the status file, 0
disables.")
  (log-stats
   (boolean #f)
   "Also write the stats as a logs.  This generates a lot of output.")
  (data-time
   (integer 0)
   "The time interval in seconds between writing the data records to the log
file, 0 disables.")
  (facility
   maybe-string
   "The logging facility for the syslog.")

  ;; Event handlers
  (event-handlers
   (apcupsd-event-handlers (apcupsd-event-handlers))
   "Handlers for events produced by apcupsd."
   empty-serializer))

(define (%apccontrol config)
  (program-file
   "apccontrol"
   (match-record (apcupsd-configuration-event-handlers config)
       <apcupsd-event-handlers>
       ( killpower commfailure commok powerout onbattery offbattery mainsback
         failing timeout loadlimit runlimit doreboot doshutdown annoyme
         emergency changeme remotedown startselftest endselftest battdetach
         battattach )
     #~(begin
         (use-modules (ice-9 format)
                      (ice-9 match)
                      (ice-9 popen)
                      (srfi srfi-9)
                      #$@(apcupsd-event-handlers-modules
                          (apcupsd-configuration-event-handlers config)))
         ;; Script dir depends on these, and the configuration depends on the
         ;; script dir.  To sever the cyclic dependency, pass the file names via
         ;; environment variables.
         (define conf (getenv "GUIX_APCUPSD_CONF"))
         (define powerfail-file (getenv "GUIX_APCUPSD_POWERFAIL_FILE"))

         (define (err . args)
           (apply format (current-error-port) args))
         (define (wall . args)
           (system* #$(file-append util-linux "/bin/wall") (apply format #f args)))
         (define (apcupsd . args)
           (apply system* #$(file-append apcupsd "/sbin/apcupsd") "-f" conf args))
         (define (mail-to-root subject body)
           (let ((port (open-pipe* OPEN_WRITE
                                   "/run/privileged/bin/sendmail"
                                   "-F" "apcupsd"
                                   "root")))
             (format port "Subject: ~a~%~%~a~&" subject body)
             (close-pipe port)))
         (match (cdr (command-line))
           (((? string? cmd) name connected powered)
            (let ((connected? (match connected
                                ("1" #t)
                                ("0" #f)))
                  (powered? (match powered
                              ("1" #t)
                              ("0" #f))))
              (match cmd
                ("killpower"     #$killpower)
                ("commfailure"   #$commfailure)
                ("commok"        #$commok)
	        ("powerout"      #$powerout)
	        ("onbattery"     #$onbattery)
	        ("offbattery"    #$offbattery)
	        ("mainsback"     #$mainsback)
	        ("failing"       #$failing)
	        ("timeout"       #$timeout)
	        ("loadlimit"     #$loadlimit)
	        ("runlimit"      #$runlimit)
	        ("doreboot"      #$doreboot)
	        ("doshutdown"    #$doshutdown)
	        ("annoyme"       #$annoyme)
	        ("emergency"     #$emergency)
	        ("changeme"      #$changeme)
	        ("remotedown"    #$remotedown)
	        ("startselftest" #$startselftest)
	        ("endselftest"   #$endselftest)
	        ("battdetach"    #$battdetach)
	        ("battattach"    #$battattach)
                (_
                 (err "Unknown event: ~a~%" cmd)
                 (err "Iff the event was emitted by apcupsd, this is a bug.~%")
                 (err "Please report to bug-guix@gnu.org.~%")
                 (exit #f)))))
           (args
            (err "Unknown arguments: ~a~%" args)
            (err "Iff the arguments were passed by apcupsd, this is a bug.~%")
            (err "Please report to bug-guix@gnu.org.~%")
            (exit #f)))))))

(define (apcupsd-script-dir config)
  (computed-file
   "apcupsd-script-dir"
   #~(begin
       (mkdir #$output)
       (chdir #$output)
       (symlink #$(%apccontrol config) "apccontrol"))))

(define (apcupsd-config-file config)
  (let ((run-dir (apcupsd-configuration-run-dir config)))
    (mixed-text-file
     "apcupsd.conf"
     "\
## apcupsd.conf v1.1 ##
#
#  for apcupsd - GNU Guix
#
# \"apcupsd\" POSIX config file (generated by apcupsd-service-type)
"
     (serialize-configuration config apcupsd-configuration-fields)
     ;; This one is confusing.  The manual page states:
     ;;
     ;; > It must be changed when running more than one copy of apcupsd on the
     ;; > same computer to control multiple UPSes.
     ;;
     ;; However would you not want the lock to be per-device, not per-process?
     ;; I decided to follow the documentation, but I do not understand why it
     ;; should be like this.  I do not have multiple UPSes to try.
     (serialize-string 'lock-dir (string-append run-dir "/lock"))
     (serialize-string 'power-fail-dir run-dir)
     (serialize-string 'no-login-dir run-dir)
     (serialize-string 'stat-file (string-append run-dir "/apcupsd.status"))
     "SCRIPTDIR " (apcupsd-script-dir config) "\n")))

(define (apcupsd-activation config)
  (match-record config <apcupsd-configuration> (run-dir)
    #~(begin
        (use-modules (guix build utils))
        (mkdir-p #$(string-append run-dir "/lock")))))

(define (apcupsd-shepherd-services config)
  (match-record config <apcupsd-configuration>
                ( apcupsd pid-file debug-level run-dir
                  shepherd-service-name auto-start?)
    (let ((config-file (apcupsd-config-file config)))
      (list
       (shepherd-service
        (documentation "Run the apcupsd daemon.")
        (requirement '(user-processes))
        (provision (list shepherd-service-name))
        (auto-start? auto-start?)
        (start #~(make-forkexec-constructor
                  '(#$(file-append apcupsd "/sbin/apcupsd")
                    "-b"                ;do not daemonize
                    "-f" #$config-file
                    "-P" #$pid-file
                    "-d" #$(number->string debug-level))
                  #:log-file
                  #$(format #f "/var/log/~a.log" shepherd-service-name)
                  #:environment-variables
                  (cons* (string-append "GUIX_APCUPSD_CONF="
                                        #$config-file)
                         #$(string-append "GUIX_APCUPSD_POWERFAIL_FILE="
                                          run-dir "/powerfail")
                         (default-environment-variables))))
        (stop #~(make-kill-destructor))
        (actions (list (shepherd-configuration-action config-file))))))))

(define (apcupsd-pam-extensions config)
  ;; The apcupsd can be configured to prevent users from logging in on certain
  ;; conditions.  This is implemented by creation of a "nologin" file, and
  ;; using a pam nologin module to prevent the login (if the file exists).
  (define pam-nologin
    (pam-entry
     (control "required")
     (module "pam_nologin.so")
     (arguments (list (string-append "file="
                                     (apcupsd-configuration-run-dir config)
                                     "/nologin")))))

  (list (pam-extension
         (transformer
          (lambda (pam)
            (pam-service
             (inherit pam)
             (auth (cons pam-nologin (pam-service-auth pam)))))))))

(define apcupsd-service-type
  (service-type
   (name 'apcupsd)
   (description "Configure and optionally start the apcupsd.")
   (extensions (list (service-extension activation-service-type
                                        apcupsd-activation)
                     (service-extension shepherd-root-service-type
                                        apcupsd-shepherd-services)
                     (service-extension pam-root-service-type
                                        apcupsd-pam-extensions)))
   (default-value (apcupsd-configuration))))
