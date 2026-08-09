;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Peter Mikkelsen <petermikkelsen10@gmail.com>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022⁠–⁠2023 Bruno Victal <mirai@makinata.eu>
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

(define-module (gnu services audio)
  #:use-module (guix gexp)
  #:use-module (guix deprecation)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix modules)
  #:use-module ((gnu services) #:hide (delete))
  #:use-module (gnu services admin)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services admin)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages audio) #:select (snapcast))
  #:use-module (gnu packages mpd)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-71)
  #:export (mpd-output
            mpd-output?
            mpd-output-name
            mpd-output-type
            mpd-output-enabled?
            mpd-output-format
            mpd-output-tags?
            mpd-output-always-on?
            mpd-output-mixer-type
            mpd-output-replay-gain-handler
            mpd-output-extra-options

            mpd-plugin
            mpd-plugin?
            mpd-plugin-plugin
            mpd-plugin-name
            mpd-plugin-enabled?
            mpd-plugin-extra-options

            mpd-partition
            mpd-partition?
            mpd-partition-name
            mpd-partition-extra-options

            mpd-configuration
            mpd-configuration?
            mpd-configuration-package
            mpd-configuration-user
            mpd-configuration-group
            mpd-configuration-shepherd-requirement
            mpd-configuration-log-file
            mpd-configuration-log-level
            mpd-configuration-music-directory
            mpd-configuration-music-dir
            mpd-configuration-playlist-directory
            mpd-configuration-playlist-dir
            mpd-configuration-db-file
            mpd-configuration-state-file
            mpd-configuration-sticker-file
            mpd-configuration-default-port
            mpd-configuration-endpoints
            mpd-configuration-address
            mpd-configuration-database
            mpd-configuration-partitions
            mpd-configuration-neighbors
            mpd-configuration-inputs
            mpd-configuration-archive-plugins
            mpd-configuration-input-cache-size
            mpd-configuration-decoders
            mpd-configuration-resampler
            mpd-configuration-filters
            mpd-configuration-outputs
            mpd-configuration-playlist-plugins
            mpd-configuration-extra-options
            mpd-service-type

            mympd-service-type
            mympd-configuration
            mympd-configuration?
            mympd-configuration-package
            mympd-configuration-shepherd-requirement
            mympd-configuration-user
            mympd-configuration-group
            mympd-configuration-work-directory
            mympd-configuration-cache-directory
            mympd-configuration-acl
            mympd-configuration-covercache-ttl
            mympd-configuration-http?
            mympd-configuration-host
            mympd-configuration-port
            mympd-configuration-log-level
            mympd-configuration-log-to
            mympd-configuration-lualibs
            mympd-configuration-uri
            mympd-configuration-script-acl
            mympd-configuration-ssl?
            mympd-configuration-ssl-port
            mympd-configuration-ssl-cert
            mympd-configuration-ssl-key
            mympd-configuration-pin-hash
            mympd-configuration-save-caches?
            mympd-ip-acl
            mympd-ip-acl?
            mympd-ip-acl-allow
            mympd-ip-acl-deny

            snapserver-stream
            snapserver-stream?
            snapserver-stream-name
            snapserver-stream-type
            snapserver-stream-location
            snapserver-stream-codec
            snapserver-stream-sampleformat
            snapserver-stream-chunk-ms
            snapserver-stream-parameters

            snapserver-configuration
            snapserver-configuration?
            snapserver-configuration-package
            snapserver-configuration-user
            snapserver-configuration-group
            snapserver-configuration-shepherd-requirement
            snapserver-configuration-log-file
            snapserver-configuration-log-filter
            snapserver-configuration-data-directory
            snapserver-configuration-mdns?
            snapserver-configuration-http?
            snapserver-configuration-http-port
            snapserver-configuration-http-bind-address
            snapserver-configuration-doc-root
            snapserver-configuration-tcp-control?
            snapserver-configuration-tcp-control-port
            snapserver-configuration-tcp-streaming?
            snapserver-configuration-tcp-streaming-port
            snapserver-configuration-streams
            snapserver-configuration-codec
            snapserver-configuration-sampleformat
            snapserver-configuration-buffer-ms
            snapserver-configuration-chunk-ms
            snapserver-configuration-send-to-muted?
            snapserver-configuration-extra-content
            snapserver-service-type))

;;; Commentary:
;;;
;;; Audio related services
;;;
;;; Code:

(define (uglify-field-name field-name)
  (let ((str (symbol->string field-name)))
    (string-join (string-split (if (string-suffix? "?" str)
                                   (string-drop-right str 1)
                                   str)
                               #\-) "_")))

;; Helpers for deprecated field types, to be removed later.
(define %lazy-group (make-symbol "%lazy-group"))

(define (set-user-group user group)
  (user-account
   (inherit user)
   (group (user-group-name group))))


;;;
;;; MPD
;;;

(define (mpd-serialize-field field-name value)
  (let ((field (if (string? field-name) field-name
                   (uglify-field-name field-name)))
        (value (cond
                ((boolean? value) (if value "yes" "no"))
                ((string? value) value)
                (else (object->string value)))))
    #~(format #f "~a ~s~%" #$field #$value)))

(define (mpd-serialize-alist field-name value)
  #~(string-append #$@(generic-serialize-alist list mpd-serialize-field
                                               value)))

(define mpd-serialize-string mpd-serialize-field)
(define mpd-serialize-boolean mpd-serialize-field)

(define (mpd-serialize-list-of-strings field-name value)
  #~(string-append #$@(map (cut mpd-serialize-string field-name <>) value)))

(define (mpd-serialize-user-account field-name value)
  (mpd-serialize-string field-name (user-account-name value)))

(define (mpd-serialize-user-group field-name value)
  (mpd-serialize-string field-name (user-group-name value)))

(define-maybe string (prefix mpd-))
(define-maybe list-of-strings (prefix mpd-))
(define-maybe boolean (prefix mpd-))

(define %mpd-user
  (user-account
   (name "mpd")
   ;; XXX: This is a place-holder to be lazily substituted in (…-accounts)
   ;; with the value from the 'group' field of <mpd-configuration>.
   (group %lazy-group)
   (system? #t)
   (comment "Music Player Daemon (MPD) user")
   ;; MPD can use $HOME (or $XDG_CONFIG_HOME) to place its data.
   (home-directory "/var/lib/mpd")
   (shell (file-append shadow "/sbin/nologin"))))

(define %mpd-group
  (user-group
   (name "mpd")
   (system? #t)))

;;; TODO: Procedures for deprecated fields, to be removed.

(define mpd-deprecated-fields '((music-dir . music-directory)
                                (playlist-dir . playlist-directory)
                                (address . endpoints)))

(define (port? value) (or (string? value) (integer? value)))

(define (mpd-serialize-deprecated-field field-name value)
  (if (maybe-value-set? value)
      (begin
        (warn-about-deprecation
         field-name #f
         #:replacement (assoc-ref mpd-deprecated-fields field-name))
        (match field-name
          ('playlist-dir (mpd-serialize-string "playlist_directory" value))
          ('music-dir (mpd-serialize-string "music_directory" value))
          ('address (mpd-serialize-string "bind_to_address" value))))
      ""))

(define (mpd-serialize-port field-name value)
  (when (string? value)
    (warning
     (G_ "string value for '~a' is deprecated, use integer instead~%")
     field-name))
  (mpd-serialize-field "port" value))

(define-maybe port (prefix mpd-))

;;; Procedures for unsupported value types, to be removed.

(define (mpd-user-sanitizer value)
  (cond ((user-account? value) value)
        ((string? value)
         (warning (G_ "string value for 'user' is deprecated, use \
user-account instead~%"))
         (user-account
          (inherit %mpd-user)
          (name value)))
        (else
         (configuration-field-error #f 'user value))))

(define (mpd-group-sanitizer value)
  (cond ((user-group? value) value)
        ((string? value)
         (warning (G_ "string value for 'group' is deprecated, use \
user-group instead~%"))
         (user-group
          (inherit %mpd-group)
          (name value)))
        (else
         (configuration-field-error #f 'group value))))

(define (mpd-log-file-sanitizer value)
  ;; XXX: While leaving the 'sys_log' option out of the mpd.conf file is
  ;; supposed to cause logging to happen via systemd (elogind provides a
  ;; compatible interface), this doesn't work (nothing gets logged); use
  ;; syslog instead.
  (let ((value (maybe-value value "syslog")))
    (if (string? value)
        value
        (configuration-field-error #f 'log-file value))))

;;;

;; Generic MPD plugin record, lists only the most prevalent fields.
(define-configuration mpd-plugin
  (plugin
   maybe-string
   "Plugin name.")

  (name
   maybe-string
   "Name.")

  (enabled?
   maybe-boolean
   "Whether the plugin is enabled/disabled.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values
to be appended to the plugin configuration. See
@uref{https://mpd.readthedocs.io/en/latest/plugins.html,MPD plugin reference}
for available options.")

  (prefix mpd-))

(define (mpd-serialize-mpd-plugin field-name value)
  #~(format #f "~a {~%~a}~%"
            '#$field-name
            #$(serialize-configuration value mpd-plugin-fields)))

(define (mpd-serialize-list-of-mpd-plugin field-name value)
  #~(string-append #$@(map (cut mpd-serialize-mpd-plugin field-name <>)
                           value)))

(define list-of-mpd-plugin? (list-of mpd-plugin?))

(define-maybe mpd-plugin (prefix mpd-))

(define-configuration mpd-partition
  (name
   string
   "Partition name.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values
to be appended to the partition configuration. See
@uref{https://mpd.readthedocs.io/en/latest/user.html#configuring-partitions,Configuring Partitions}
for available options.")

  (prefix mpd-))

(define (mpd-serialize-mpd-partition field-name value)
  #~(format #f "partition {~%~a}~%"
            #$(serialize-configuration value mpd-partition-fields)))

(define (mpd-serialize-list-of-mpd-partition field-name value)
  #~(string-append #$@(map (cut mpd-serialize-mpd-partition #f <>) value)))

(define list-of-mpd-partition?
  (list-of mpd-partition?))

(define-configuration mpd-output
  (name
   (string "MPD")
   "The name of the audio output.")

  (type
   (string "pulse")
   "The type of audio output.")

  (enabled?
   (boolean #t)
   "Specifies whether this audio output is enabled when MPD is started. By
default, all audio outputs are enabled. This is just the default
setting when there is no state file; with a state file, the previous
state is restored.")

  (format
   maybe-string
   "Force a specific audio format on output. See
@uref{https://mpd.readthedocs.io/en/latest/user.html#audio-output-format,Global Audio Format}
for a more detailed description.")

  (tags?
   (boolean #t)
   "If set to @code{#f}, then MPD will not send tags to this output. This
is only useful for output plugins that can receive tags, for example the
@code{httpd} output plugin.")

  (always-on?
   (boolean #f)
   "If set to @code{#t}, then MPD attempts to keep this audio output always
open. This may be useful for streaming servers, when you don’t want to
disconnect all listeners even when playback is accidentally stopped.")

  (mixer-type
   maybe-string
   "This field accepts a string that specifies which mixer should be used for
this audio output: the @code{hardware} mixer, the @code{software} mixer, the
@code{null} mixer (allows setting the volume, but with no effect; this can be
used as a trick to implement an external mixer External Mixer) or no
mixer (@code{none}).  When left unspecified, a @code{hardware} mixer is used
for devices that support it."
   (sanitizer
    (lambda (x)  ; TODO: deprecated, remove me later.
      (cond
       ((eq? %unset-value x)
        x)
       ((symbol? x)
        (warning (G_ "symbol value for 'mixer-type' is deprecated, \
use string instead~%"))
        (symbol->string x))
       ((string? x) x)
       (else
        (configuration-field-error #f 'mixer-type x))))))

  (replay-gain-handler
   maybe-string
   "This field accepts a string that specifies how
@uref{https://mpd.readthedocs.io/en/latest/user.html#replay-gain,Replay Gain}
is to be applied. @code{software} uses an internal software volume control,
@code{mixer} uses the configured (hardware) mixer control and @code{none}
disables replay gain on this audio output.")

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values
to be appended to the audio output configuration.")

  (prefix mpd-))

(define (mpd-serialize-mpd-output field-name value)
  #~(format #f "audio_output {~%~a}~%"
            #$(serialize-configuration value mpd-output-fields)))

(define (mpd-serialize-list-of-mpd-plugin-or-output field-name value)
  (let ((plugins outputs (partition mpd-plugin? value)))
    #~(string-append #$@(map (cut mpd-serialize-mpd-plugin "audio_output" <>)
                             plugins)
                     #$@(map (cut mpd-serialize-mpd-output #f <>) outputs))))

(define list-of-mpd-plugin-or-output?
  (list-of (lambda (x)
             (or (mpd-output? x) (mpd-plugin? x)))))

(define-configuration mpd-configuration
  (package
   (file-like mpd)
   "The MPD package."
   empty-serializer)

  (user
   (user-account %mpd-user)
   "The user to run mpd as."
   (sanitizer mpd-user-sanitizer))

  (group
   (user-group %mpd-group)
   "The group to run mpd as."
   (sanitizer mpd-group-sanitizer))

  (shepherd-requirement
   (list-of-symbols '())
   "This is a list of symbols naming Shepherd services that this service
will depend on."
   empty-serializer)

  (environment-variables
   (list-of-strings '("PULSE_CLIENTCONFIG=/etc/pulse/client.conf"
                      "PULSE_CONFIG=/etc/pulse/daemon.conf"))
   "A list of strings specifying environment variables."
   empty-serializer)

  (log-file
   maybe-string
   "The location of the log file.  Unless specified, logs are sent to the
local syslog daemon.  Alternatively, a log file name can be specified, for
example @file{/var/log/mpd.log}."
   (sanitizer mpd-log-file-sanitizer))

  (log-level
   maybe-string
   "Suppress any messages below this threshold.
The available values, in decreasing order of verbosity, are: @code{verbose},
@code{info}, @code{notice}, @code{warning} and @code{error}.")

  (music-directory
   maybe-string
   "The directory to scan for music files.")

  (music-dir ; TODO: deprecated, remove later
   maybe-string
   "The directory to scan for music files."
   (serializer mpd-serialize-deprecated-field))

  (playlist-directory
   maybe-string
   "The directory to store playlists.")

  (playlist-dir ; TODO: deprecated, remove later
   maybe-string
   "The directory to store playlists."
   (serializer mpd-serialize-deprecated-field))

  (db-file
   maybe-string
   "The location of the music database.  When left unspecified,
@file{~/.cache/db} is used.")

  (state-file
   maybe-string
   "The location of the file that stores current MPD's state.")

  (sticker-file
   maybe-string
   "The location of the sticker database.")

  (default-port
   (maybe-port 6600)
   "The default port to run mpd on.")

  (endpoints
   maybe-list-of-strings
   "The addresses that mpd will bind to. A port different from
@var{default-port} may be specified, e.g. @code{localhost:6602} and
IPv6 addresses must be enclosed in square brackets when a different
port is used.
To use a Unix domain socket, an absolute path or a path starting with @code{~}
can be specified here."
   (serializer
    (lambda (_ endpoints)
      (if (maybe-value-set? endpoints)
          (mpd-serialize-list-of-strings "bind_to_address" endpoints)
          ""))))

  (address ; TODO: deprecated, remove later
   maybe-string
   "The address that mpd will bind to.
To use a Unix domain socket, an absolute path can be specified here."
   (serializer mpd-serialize-deprecated-field))

  (database
   maybe-mpd-plugin
   "MPD database plugin configuration.")

  (partitions
   (list-of-mpd-partition '())
   "List of MPD \"partitions\".")

  (neighbors
   (list-of-mpd-plugin '())
   "List of MPD neighbor plugin configurations.")

  (inputs
   (list-of-mpd-plugin '())
   "List of MPD input plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "input" x))))

  (archive-plugins
   (list-of-mpd-plugin '())
   "List of MPD archive plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "archive_plugin" x))))

  (auto-update?
   maybe-boolean
   "Whether to automatically update the music database when files are changed
in the @var{music-directory}.")

  (input-cache-size
   maybe-string
   "MPD input cache size."
   (serializer (lambda (_ x)
                 (if (maybe-value-set? x)
                     #~(string-append "\ninput_cache {\n"
                                      #$(mpd-serialize-string "size" x)
                                      "}\n") ""))))

  (decoders
   (list-of-mpd-plugin '())
   "List of MPD decoder plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "decoder" x))))

  (resampler
   maybe-mpd-plugin
   "MPD resampler plugin configuration.")

  (filters
   (list-of-mpd-plugin '())
   "List of MPD filter plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "filter" x))))

  (outputs
   (list-of-mpd-plugin-or-output (list (mpd-output)))
   "The audio outputs that MPD can use.
By default this is a single output using pulseaudio.")

  (playlist-plugins
   (list-of-mpd-plugin '())
   "List of MPD playlist plugin configurations."
   (serializer (lambda (_ x)
                 (mpd-serialize-list-of-mpd-plugin "playlist_plugin" x))))

  (extra-options
   (alist '())
   "An association list of option symbols/strings to string values to be
appended to the configuration.")

  (prefix mpd-))

(define (mpd-serialize-configuration configuration)
  (mixed-text-file
   "mpd.conf"
   (serialize-configuration configuration mpd-configuration-fields)))

(define (mpd-shepherd-service config)
  (match-record config <mpd-configuration>
    (user package shepherd-requirement
          log-file playlist-directory
          db-file state-file sticker-file
          environment-variables)
    (let ((config-file (mpd-serialize-configuration config))
          (username (user-account-name user)))
      (shepherd-service
       (documentation "Run the MPD (Music Player Daemon)")
       (requirement `(user-processes loopback
                                     ,@(if (string=? "syslog" log-file)
                                           '(syslogd)
                                           '())
                                     ,@shepherd-requirement))
       (provision '(mpd))
       (start
        (with-imported-modules (source-module-closure
                                '((gnu build activation)))
          #~(begin
              (use-modules (gnu build activation))

              (let ((home #$(user-account-home-directory user)))
                (let ((user (getpw #$username))
                      (default-cache-dir (string-append home "/.cache")))

                  (define (init-directory directory)
                    (unless (file-exists? directory)
                      (mkdir-p/perms directory user #o755)))

                  ;; Define a cache location that can be automatically used
                  ;; for the database file, in case it hasn't been explicitly
                  ;; specified.
                  (for-each
                   init-directory
                   (cons default-cache-dir
                         '#$(map dirname
                                 ;; XXX: Delete the potential "syslog"
                                 ;; log-file value, which is not a directory.
                                 (delete "syslog"
                                         (filter-map maybe-value
                                                     (list db-file
                                                           log-file
                                                           state-file
                                                           sticker-file)))))))

                (make-forkexec-constructor
                 (list #$(file-append package "/bin/mpd") "--no-daemon"
                       #$config-file)
                 #:environment-variables
                 ;; Set HOME so MPD can infer default paths, such as
                 ;; for the database file.
                 (cons (string-append "HOME=" home)
                       '#$environment-variables))))))
       (stop  #~(make-kill-destructor))
       (actions
        (list (shepherd-configuration-action config-file)
              (shepherd-signal-action
               'reopen SIGHUP
               #:documentation "Re-open log files and flush caches."
               #:message "Issued SIGHUP to Service MPD.")))))))

(define (mpd-accounts config)
  (match-record config <mpd-configuration> (user group)
    ;; TODO: Deprecation code, to be removed.
    (let ((user (if (eq? (user-account-group user) %lazy-group)
                    (set-user-group user group)
                    user)))
      (list user group))))

(define mpd-service-type
  (service-type
   (name 'mpd)
   (description "Run the Music Player Daemon (MPD).")
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list mpd-shepherd-service))
          (service-extension account-service-type mpd-accounts)))
   (default-value (mpd-configuration))))


;;;
;;; myMPD
;;;

(define (string-or-symbol? x)
  (or (symbol? x) (string? x)))

(define-configuration/no-serialization mympd-ip-acl
  (allow
   (list-of-strings '())
   "Allowed IP addresses.")

  (deny
   (list-of-strings '())
   "Disallowed IP addresses."))

(define-maybe/no-serialization integer)
(define-maybe/no-serialization mympd-ip-acl)

(define %mympd-user
  (user-account
   (name "mympd")
   ;; XXX: This is a place-holder to be lazily substituted in 'mympd-accounts'
   ;; with the value from the 'group' field of <mympd-configuration>.
   (group %lazy-group)
   (system? #t)
   (comment "myMPD user")
   (home-directory "/var/empty")
   (shell (file-append shadow "/sbin/nologin"))))

(define %mympd-group
  (user-group
   (name "mympd")
   (system? #t)))

;;; TODO: Procedures for unsupported value types, to be removed.
(define (mympd-user-sanitizer value)
  (cond ((user-account? value) value)
        ((string? value)
         (warning (G_ "string value for 'user' is not supported, use \
user-account instead~%"))
         (user-account
          (inherit %mympd-user)
          (name value)))
        (else
         (configuration-field-error #f 'user value))))

(define (mympd-group-sanitizer value)
  (cond ((user-group? value) value)
        ((string? value)
         (warning (G_ "string value for 'group' is not supported, use \
user-group instead~%"))
         (user-group
          (inherit %mympd-group)
          (name value)))
        (else
         (configuration-field-error #f 'group value))))

(define (mympd-log-to-sanitizer value)
  (match value
    ('syslog
     (warning (G_ "syslog symbol value for 'log-to' is deprecated~%"))
     %unset-value)
    ((or %unset-value (? string?))
     value)
    (_ (configuration-field-error #f 'log-to value))))

;; XXX: The serialization procedures are insufficient since we require
;; access to multiple fields at once.
;; Fields marked with empty-serializer are never serialized and are
;; used for command-line arguments or by the service definition.
(define-configuration/no-serialization mympd-configuration
  (package
    (file-like mympd)
    "The package object of the myMPD server."
    empty-serializer)

  (shepherd-requirement
   (list-of-symbols '())
   "This is a list of symbols naming Shepherd services that this service
will depend on."
   empty-serializer)

  (user
   (user-account %mympd-user)
   "Owner of the @command{mympd} process."
   (sanitizer mympd-user-sanitizer)
   empty-serializer)

  (group
   (user-group %mympd-group)
   "Owner group of the @command{mympd} process."
   (sanitizer mympd-group-sanitizer)
   empty-serializer)

  (work-directory
   (string "/var/lib/mympd")
   "Where myMPD will store its data."
   empty-serializer)

  (cache-directory
   (string "/var/cache/mympd")
   "Where myMPD will store its cache."
   empty-serializer)

  (acl
   maybe-mympd-ip-acl
   "ACL to access the myMPD webserver.")

  (covercache-ttl
   (maybe-integer 31)
   "How long to keep cached covers, @code{0} disables cover caching.")

  (http?
   (boolean #t)
   "HTTP support.")

  (host
   (string "[::]")
   "Host name to listen on.")

  (port
   (maybe-port 80)
   "HTTP port to listen on.")

  (log-level
   (integer 5)
   "How much detail to include in logs, possible values: @code{0} to @code{7}.")

  (log-to
   maybe-string
   "Where to send logs.  Unless specified, the service logs to the local
syslog service under the @samp{daemon} facility.  Alternatively, a log file
name can be specified, for example @file{/var/log/mympd.log}."
   (sanitizer mympd-log-to-sanitizer)
   empty-serializer)

  (lualibs
   (maybe-string "all")
   "See
@url{https://jcorporation.github.io/myMPD/scripting/#lua-standard-libraries}.")

  (uri
   maybe-string
   "Override URI to myMPD.
See @url{https://github.com/jcorporation/myMPD/issues/950}.")

  (script-acl
   (maybe-mympd-ip-acl (mympd-ip-acl
                        (allow '("127.0.0.1"))))
   "ACL to access the myMPD script backend.")

  (ssl?
   (boolean #f)
   "SSL/TLS support.")

  (ssl-port
   (maybe-port 443)
   "Port to listen for HTTPS.")

  (ssl-cert
   maybe-string
   "Path to PEM encoded X.509 SSL/TLS certificate (public key).")

  (ssl-key
   maybe-string
   "Path to PEM encoded SSL/TLS private key.")

  (pin-hash
   maybe-string
   "SHA-256 hashed pin used by myMPD to control settings access by
prompting a pin from the user.")

  (save-caches?
   maybe-boolean
   "Whether to preserve caches between service restarts."))

(define (mympd-serialize-configuration config)
  (define serialize-value
    (match-lambda
      ((? boolean? val) (if val "true" "false"))
      ((? integer? val) (number->string val))
      ((? mympd-ip-acl? val) (ip-acl-serialize-configuration val))
      ((? string? val) val)))

  (define (ip-acl-serialize-configuration config)
    (define (serialize-list-of-strings prefix lst)
      (map (cut format #f "~a~a" prefix <>) lst))
    (string-join
     (append
      (serialize-list-of-strings "+" (mympd-ip-acl-allow config))
      (serialize-list-of-strings "-" (mympd-ip-acl-deny config))) ","))

  ;; myMPD configuration fields are serialized as individual files under
  ;; <work-directory>/config/.
  (match-record config <mympd-configuration> (work-directory acl
                                              covercache-ttl http? host port
                                              log-level lualibs uri script-acl
                                              ssl? ssl-port ssl-cert ssl-key
                                              pin-hash save-caches?)
    (define (serialize-field filename value)
      (when (maybe-value-set? value)
        (list (format #f "~a/config/~a" work-directory filename)
              (mixed-text-file filename (serialize-value value)))))

    (let ((filename-to-field `(("acl" . ,acl)
                               ("covercache_keep_days" . ,covercache-ttl)
                               ("http"                 . ,http?)
                               ("http_host"            . ,host)
                               ("http_port"            . ,port)
                               ("loglevel"             . ,log-level)
                               ("lualibs"              . ,lualibs)
                               ("mympd_uri"            . ,uri)
                               ("scriptacl"            . ,script-acl)
                               ("ssl"                  . ,ssl?)
                               ("ssl_port"             . ,ssl-port)
                               ("ssl_cert"             . ,ssl-cert)
                               ("ssl_key"              . ,ssl-key)
                               ("pin_hash"             . ,pin-hash)
                               ("save_caches"          . ,save-caches?))))
      (filter list?
              (generic-serialize-alist list serialize-field
                                       filename-to-field)))))

(define (mympd-shepherd-service config)
  (match-record config <mympd-configuration>
    (package shepherd-requirement user work-directory cache-directory
             log-level log-to)
    (shepherd-service
     (documentation "Run the myMPD daemon.")
     (requirement `(loopback user-processes
                             ,@(if (maybe-value-set? log-to)
                                   '()
                                   '(syslogd))
                             ,@shepherd-requirement))
     (provision '(mympd))
     (start
      (let ((username (user-account-name user)))
        (with-imported-modules (source-module-closure
                                '((gnu build activation)))
          #~(begin
              (use-modules (gnu build activation))

              (let ((user (getpw #$username)))

                (define (init-directory directory)
                  (unless (file-exists? directory)
                    (mkdir-p/perms directory user #o755)))

                (for-each init-directory
                          '#$(map dirname (filter-map maybe-value
                                                      (list log-to
                                                            work-directory
                                                            cache-directory)))))

              (make-forkexec-constructor
               `(#$(file-append package "/bin/mympd")
                 "--user" #$username
                 #$@(if (eq? log-to 'syslog) '("--syslog") '())
                 "--workdir" #$work-directory
                 "--cachedir" #$cache-directory)
               #:environment-variables
               (list #$(format #f "MYMPD_LOGLEVEL=~a" log-level))
               #:log-file #$(maybe-value log-to)))))))))

(define (mympd-accounts config)
  (match-record config <mympd-configuration> (user group)
    ;; TODO: Deprecation code, to be removed.
    (let ((user (if (eq? (user-account-group user) %lazy-group)
                    (set-user-group user group)
                    user)))
      (list user group))))

(define mympd-service-type
  (service-type
   (name 'mympd)
   (extensions
    (list  (service-extension shepherd-root-service-type
                              (compose list mympd-shepherd-service))
           (service-extension account-service-type
                              mympd-accounts)
           (service-extension special-files-service-type
                              mympd-serialize-configuration)))
   (description "Run myMPD, a frontend for MPD (Music Player Daemon).")
   (default-value (mympd-configuration))))


;;;
;;; Snapcast
;;;

(define %snapserver-user
  (user-account
   (name "snapserver")
   (group %lazy-group)
   (system? #t)
   (comment "Snapcast server user")
   (home-directory "/var/lib/snapserver")
   (shell (file-append shadow "/sbin/nologin"))))

(define %snapserver-group
  (user-group
   (name "snapserver")
   (system? #t)))

(define (snapserver-stream-type? value)
  (memq value '(pipe file process librespot airplay tcp alsa meta)))

(define-maybe/no-serialization file-like)

(define-configuration/no-serialization snapserver-stream
  (name
   (string "default")
   "Name of the stream, as shown by Snapcast clients.")

  (type
   (snapserver-stream-type 'pipe)
   "Type of the stream source: one of @code{pipe}, @code{file},
@code{process}, @code{librespot}, @code{airplay}, @code{tcp}, @code{alsa} or
@code{meta}.")

  (location
   (string "/run/snapserver/snapfifo")
   "Location of the stream source: a file name for @code{pipe}, @code{file}
and @code{process} streams, a host name for @code{tcp} streams, a device name
for @code{alsa} streams, and a slash-separated list of stream names for
@code{meta} streams.")

  (codec
   maybe-string
   "Codec to use for this stream, overriding the @code{codec} field of
@code{snapserver-configuration}.")

  (sampleformat
   maybe-string
   "Sample format of this stream, given as
@code{@var{rate}:@var{bits}:@var{channels}}, overriding the
@code{sampleformat} field of @code{snapserver-configuration}.")

  (chunk-ms
   maybe-integer
   "Duration, in milliseconds, of the audio chunks read from this source,
overriding the @code{chunk-ms} field of @code{snapserver-configuration}.")

  (parameters
   (alist '())
   "Additional source parameters, given as an association list, for example
@code{'((\"controlscript\" . \"meta_mpd.py\"))}."))

(define (snapserver-stream->uri stream)
  (match-record stream <snapserver-stream>
                (name type location codec sampleformat chunk-ms parameters)
    (let ((query `(("name" . ,name)
                   ,@(if (maybe-value-set? codec)
                         `(("codec" . ,codec))
                         '())
                   ,@(if (maybe-value-set? sampleformat)
                         `(("sampleformat" . ,sampleformat))
                         '())
                   ,@(if (maybe-value-set? chunk-ms)
                         `(("chunk_ms" . ,chunk-ms))
                         '())
                   ,@parameters)))
      (simple-format #f "~A://~A?~A" type location
                     (string-join
                      (map (match-lambda
                             ((key . value)
                              (simple-format #f "~A=~A" key value)))
                           query)
                      "&")))))

(define list-of-snapserver-stream?
  (list-of snapserver-stream?))

(define-configuration/no-serialization snapserver-configuration
  (package
    (file-like snapcast)
    "The Snapcast package to use.")

  (user
   (user-account %snapserver-user)
   "Owner of the @command{snapserver} process.")

  (group
   (user-group %snapserver-group)
   "Owner group of the @command{snapserver} process.")

  (shepherd-requirement
   (list-of-symbols '())
   "This is a list of symbols naming Shepherd services that this service
will depend on.")

  (log-file
   (string "/var/log/snapserver.log")
   "File where @command{snapserver} writes its log.")

  (log-filter
   (string "*:info")
   "Log filter, given as @code{@var{tag}:@var{level}} pairs separated by
commas, where @var{tag} is @code{*} or a log tag and @var{level} is one of
@code{trace}, @code{debug}, @code{info}, @code{notice}, @code{warning},
@code{error} or @code{fatal}.")

  (data-directory
   (string "/var/lib/snapserver")
   "Directory where @command{snapserver} stores persistent data, such as
client settings.")

  (mdns?
   (boolean #t)
   "Whether to publish the Snapcast services over mDNS.  This requires
@code{avahi-service-type} (@pxref{Networking Services}).")

  (http?
   (boolean #t)
   "Whether to enable the HTTP JSON-RPC interface, which also serves the
Snapweb web interface.")

  (http-port
   (integer 1780)
   "Port the HTTP server listens on.")

  (http-bind-address
   (string "::")
   "Address the HTTP server listens on.")

  (doc-root
   maybe-file-like
   "Directory served by the HTTP server.  It defaults to the Snapweb web
interface bundled with the @code{package} field.")

  (tcp-control?
   (boolean #t)
   "Whether to enable the TCP JSON-RPC control interface.")

  (tcp-control-port
   (integer 1705)
   "Port the TCP control interface listens on.")

  (tcp-streaming?
   (boolean #t)
   "Whether to enable TCP streaming, which is what Snapcast clients connect
to.")

  (tcp-streaming-port
   (integer 1704)
   "Port the TCP streaming interface listens on.")

  (streams
   (list-of-snapserver-stream (list (snapserver-stream)))
   "List of @code{snapserver-stream} records describing the audio sources
@command{snapserver} reads from.")

  (codec
   (string "flac")
   "Default codec used to send audio to clients, given as
@code{@var{codec}[:@var{options}]} where @var{codec} is one of @code{flac},
@code{ogg}, @code{opus} or @code{pcm}.")

  (sampleformat
   (string "48000:16:2")
   "Default sample format of the streams, given as
@code{@var{rate}:@var{bits}:@var{channels}}.")

  (buffer-ms
   (integer 1000)
   "End-to-end latency, in milliseconds.")

  (chunk-ms
   (integer 20)
   "Default duration, in milliseconds, of the audio chunks read from the
sources.")

  (send-to-muted?
   (boolean #f)
   "Whether to send audio to muted clients.")

  (extra-content
   maybe-string
   "Content appended verbatim to @file{snapserver.conf}, for options that are
not covered by the fields above, such as the @code{[ssl]} and
@code{[authorization]} sections."))

(define (snapserver-config-file config)
  (define (boolean->string value)
    (if value "1" "0"))

  (match-record config <snapserver-configuration>
    (package log-filter data-directory mdns?
             http? http-port http-bind-address doc-root
             tcp-control? tcp-control-port tcp-streaming? tcp-streaming-port
             streams codec sampleformat buffer-ms chunk-ms send-to-muted?
             extra-content)
    (apply mixed-text-file "snapserver.conf"
           `("[server]\n"
             "datadir = " ,data-directory "\n"
             "mdns_enabled = " ,(boolean->string mdns?) "\n"

             "\n[http]\n"
             "enabled = " ,(boolean->string http?) "\n"
             "port = " ,(number->string http-port) "\n"
             "bind_to_address = " ,http-bind-address "\n"
             "doc_root = " ,(if (maybe-value-set? doc-root)
                                doc-root
                                (file-append package
                                             "/share/snapserver/snapweb"))
             "\n"

             "\n[tcp-control]\n"
             "enabled = " ,(boolean->string tcp-control?) "\n"
             "port = " ,(number->string tcp-control-port) "\n"

             "\n[tcp-streaming]\n"
             "enabled = " ,(boolean->string tcp-streaming?) "\n"
             "port = " ,(number->string tcp-streaming-port) "\n"

             "\n[stream]\n"
             ;; The upstream defaults for these two point into /usr/share.
             "plugin_dir = " ,(file-append package
                                           "/share/snapserver/plug-ins") "\n"
             "sandbox_dir = " ,(file-append package
                                            "/share/snapserver/sandbox") "\n"
             ,@(map (lambda (stream)
                      (string-append "source = "
                                     (snapserver-stream->uri stream) "\n"))
                    streams)
             "codec = " ,codec "\n"
             "sampleformat = " ,sampleformat "\n"
             "buffer = " ,(number->string buffer-ms) "\n"
             "chunk_ms = " ,(number->string chunk-ms) "\n"
             "send_to_muted = " ,(boolean->string send-to-muted?) "\n"

             "\n[logging]\n"
             ;; Log to standard error, which the Shepherd redirects to the
             ;; 'log-file' of the service.
             "sink = stderr\n"
             "filter = " ,log-filter "\n"
             ,@(if (maybe-value-set? extra-content)
                   `("\n" ,extra-content "\n")
                   '())))))

(define (snapserver-shepherd-service config)
  (match-record config <snapserver-configuration>
    (package user group shepherd-requirement log-file data-directory
             mdns? streams)
    (let ((config-file (snapserver-config-file config))
          (username (user-account-name user))
          (groupname (user-group-name group))
          ;; 'snapserver' creates the named pipes of its 'pipe' sources, but
          ;; not the directories holding them.
          (directories
           (delete-duplicates
            (cons* data-directory (dirname log-file)
                   (filter-map (lambda (stream)
                                 (and (eq? 'pipe
                                           (snapserver-stream-type
                                            stream))
                                      (dirname
                                       (snapserver-stream-location
                                        stream))))
                               streams)))))
      (shepherd-service
       (documentation "Run the Snapcast server.")
       (requirement `(user-processes loopback
                                     ,@(if mdns? '(avahi-daemon) '())
                                     ,@shepherd-requirement))
       (provision '(snapserver))
       (start
        (with-imported-modules (source-module-closure
                                '((gnu build activation)))
          #~(begin
              (use-modules (gnu build activation))

              (let ((user (getpw #$username)))
                (for-each (lambda (directory)
                            (unless (file-exists? directory)
                              (mkdir-p/perms directory user #o755)))
                          '#$directories))

              (make-forkexec-constructor
               (list #$(file-append package "/bin/snapserver")
                     "--config" #$config-file)
               #:user #$username
               #:group #$groupname
               #:log-file #$log-file))))
       (stop #~(make-kill-destructor))
       (actions (list (shepherd-configuration-action config-file)))))))

(define (snapserver-accounts config)
  (match-record config <snapserver-configuration> (user group)
    (list (if (eq? (user-account-group user) %lazy-group)
              (set-user-group user group)
              user)
          group)))

(define snapserver-service-type
  (service-type
   (name 'snapserver)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list snapserver-shepherd-service))
          (service-extension account-service-type
                             snapserver-accounts)))
   (description "Run the Snapcast server, @command{snapserver}, which streams
audio to Snapcast clients.")
   (default-value (snapserver-configuration))))
