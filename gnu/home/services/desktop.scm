;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022-2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2023 conses <contact@conses.eu>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu home services desktop)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu services configuration)
  #:use-module (gnu services xorg)
  #:autoload   (gnu packages glib)    (dbus)
  #:autoload   (gnu packages xdisorg) (redshift unclutter)
  #:autoload   (gnu packages xorg) (setxkbmap xmodmap)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 match)
  #:export (home-x11-service-type

            home-redshift-configuration
            home-redshift-configuration?
            home-redshift-service-type

            home-dbus-configuration
            home-dbus-service-type

            home-unclutter-configuration
            home-unclutter-service-type

            home-xmodmap-configuration
            home-xmodmap-service-type

            home-startx-command-service-type))


;;;
;;; Waiting for X11.
;;;

(define (x11-shepherd-service delay)
  (list (shepherd-service
         (provision '(x11-display))
         (modules '((ice-9 ftw)
                    (ice-9 match)
                    (srfi srfi-1)))
         (start
          #~(lambda* (#:optional (display (getenv "DISPLAY")))
              (define x11-directory
                "/tmp/.X11-unix")

              (define (find-display delay)
                ;; Wait for an accessible socket to show up in X11-DIRECTORY,
                ;; up to DELAY seconds.
                (let loop ((attempts delay))
                  (define socket
                    (find (match-lambda
                            ((or "." "..") #f)
                            (name
                             (let ((name (in-vicinity x11-directory
                                                      name)))
                               (access? name O_RDWR))))
                          (or (scandir x11-directory) '())))

                  (if (and socket (string-prefix? "X" socket))
                      (let ((display (string-append
                                      ":" (string-drop socket 1))))
                        (format #t "X11 display server found at ~s.~%"
                                display)
                        display)
                      (if (zero? attempts)
                          (begin
                            (format (current-error-port)
                                    "X11 display server did not show up; \
giving up.\n")
                            #f)
                          (begin
                            (sleep 1)
                            (loop (- attempts 1)))))))

              (let ((display (or display (find-display #$delay))))
                (when display
                  ;; Note: 'make-forkexec-constructor' calls take their
                  ;; default #:environment-variables value before this service
                  ;; is started and are thus unaffected by the 'setenv' call
                  ;; below.  Users of this service have to explicitly query
                  ;; its value.
                  (setenv "DISPLAY" display))
                display)))
         (stop #~(lambda (_)
                   (unsetenv "DISPLAY")
                   #f))
         (respawn? #f))))

(define home-x11-service-type
  (service-type
   (name 'home-x11-display)
   (extensions (list (service-extension home-shepherd-service-type
                                        x11-shepherd-service)))
   (default-value 10)
   (description
    "Create a @code{x11-display} Shepherd service that waits for the X
Window (or ``X11'') graphical display server to be up and running, up to a
configurable delay, and sets the @code{DISPLAY} environment variable of
@command{shepherd} itself accordingly.  If no accessible X11 server shows up
during that time, the @code{x11-display} service is marked as failing to
start.")))


;;;
;;; Redshift.
;;;

(define (serialize-integer field value)
  (string-append (match field
                   ('daytime-temperature "temp-day")
                   ('nighttime-temperature "temp-night")
                   ('daytime-brightness "brightness-day")
                   ('nighttime-brightness "brightness-night")
                   ('latitude "lat")
                   ('longitude "lon")
                   (_ (symbol->string field)))
                 "=" (number->string value) "\n"))

(define (serialize-symbol field value)
  (string-append (symbol->string field)
                 "=" (symbol->string value) "\n"))

(define (serialize-string field value)
  (string-append (symbol->string field)
                 "=" value "\n"))

(define serialize-inexact-number serialize-integer)

(define (inexact-number? n)
  (and (number? n) (inexact? n)))
(define-maybe inexact-number)
(define-maybe string)

(define (serialize-raw-configuration-string field value)
  value)
(define raw-configuration-string? string?)

(define-configuration home-redshift-configuration
  (redshift
   (file-like redshift)
   "Redshift package to use.")

  (location-provider
   (symbol 'geoclue2)
   "Geolocation provider---@code{'manual} or @code{'geoclue2}.

In the former case, you must also specify the @code{latitude} and
@code{longitude} fields so Redshift can determine daytime at your place.  In
the latter case, the Geoclue system service must be running; it will be
queried for location information.")
  (adjustment-method
   (symbol 'randr)
   "Color adjustment method.")

  ;; Default values from redshift(1).
  (daytime-temperature
   (integer 6500)
   "Daytime color temperature (kelvins).")
  (nighttime-temperature
   (integer 4500)
   "Nighttime color temperature (kelvins).")

  (daytime-brightness
   maybe-inexact-number
   "Daytime screen brightness, between 0.1 and 1.0.")
  (nighttime-brightness
   maybe-inexact-number
   "Nighttime screen brightness, between 0.1 and 1.0.")

  (latitude
   maybe-inexact-number
   "Latitude, when @code{location-provider} is @code{'manual}.")
  (longitude
   maybe-inexact-number
   "Longitude, when @code{location-provider} is @code{'manual}.")

  (dawn-time
   maybe-string
   "Custom time for the transition from night to day in the
morning---@code{\"HH:MM\"} format.  When specified, solar elevation is not
used to determine the daytime/nighttime period.")
  (dusk-time
   maybe-string
   "Likewise, custom time for the transition from day to night in the
evening.")

  (extra-content
   (raw-configuration-string "")
   "Extra content appended as-is to the Redshift configuration file.  Run
@command{man redshift} for more information about the configuration file
format."))

(define (serialize-redshift-configuration config)
  (define location-fields
    '(latitude longitude))

  (define (location-field? field)
    (memq (configuration-field-name field) location-fields))

  (define (secondary-field? field)
    (or (location-field? field)
        (memq (configuration-field-name field)
              '(redshift extra-content))))

  #~(string-append
     "[redshift]\n"
     #$(serialize-configuration config
                                (remove secondary-field?
                                        home-redshift-configuration-fields))

     #$(home-redshift-configuration-extra-content config)

     "\n[manual]\n"
     #$(serialize-configuration config
                                (filter location-field?
                                        home-redshift-configuration-fields))))

(define (redshift-shepherd-service config)
  (define config-file
    (computed-file "redshift.conf"
                   #~(call-with-output-file #$output
                       (lambda (port)
                         (display #$(serialize-redshift-configuration config)
                                  port)))))

  (list (shepherd-service
         (documentation "Redshift program.")
         (provision '(redshift))

         ;; Depend on 'x11-display', which sets 'DISPLAY' if an X11 server is
         ;; available, and fails to start otherwise.
         (requirement '(x11-display))

         (modules '((srfi srfi-1)
                    (srfi srfi-26)))
         (start #~(lambda _
                    (fork+exec-command
                     (list #$(file-append
                              (home-redshift-configuration-redshift config)
                              "/bin/redshift")
                           "-c" #$config-file)

                     ;; Inherit the 'DISPLAY' variable set by 'x11-display'.
                     #:environment-variables
                     (cons (string-append "DISPLAY=" (getenv "DISPLAY"))
                           (remove (cut string-prefix? "DISPLAY=" <>)
                                   (default-environment-variables))))))
         (stop #~(make-kill-destructor))
         (actions (list (shepherd-configuration-action config-file))))))

(define home-redshift-service-type
  (service-type
   (name 'home-redshift)
   (extensions (list (service-extension home-shepherd-service-type
                                        redshift-shepherd-service)
                     ;; Ensure 'home-x11-service-type' is instantiated so we
                     ;; can depend on the Shepherd 'x11-display' service.
                     (service-extension home-x11-service-type
                                        (const #t))))
   (default-value (home-redshift-configuration))
   (description
    "Run Redshift, a program that adjusts the color temperature of display
according to time of day.")))


;;;
;;; D-Bus.
;;;

(define-record-type* <home-dbus-configuration>
  home-dbus-configuration make-home-dbus-configuration
  home-dbus-configuration?
  (dbus home-dbus-dbus                  ;file-like
        (default dbus)))

(define (home-dbus-shepherd-services config)
  (list (shepherd-service
         (documentation "Run the D-Bus daemon in session-specific mode.")
         (provision '(dbus))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (home-dbus-dbus config)
                                        "/bin/dbus-daemon")
                         "--nofork" "--session"
                         (format #f "--address=unix:path=~a/bus"
                                 (or (getenv "XDG_RUNTIME_DIR")
                                     (format #f "/run/user/~a"
                                             (getuid)))))
                   #:environment-variables
                   (cons "DBUS_VERBOSE=1"
                         (default-environment-variables))
                   #:log-file
                   (format #f "~a/log/dbus.log"
                           (or (getenv "XDG_STATE_HOME")
                               (format #f "~a/.local/state"
                                       (getenv "HOME"))))))
         (stop #~(make-kill-destructor)))))

(define (home-dbus-environment-variables config)
  '(("DBUS_SESSION_BUS_ADDRESS"
     . "unix:path=${XDG_RUNTIME_DIR:-/run/user/$UID}/bus")))

(define home-dbus-service-type
  (service-type
   (name 'home-dbus)
   (extensions
    (list (service-extension home-shepherd-service-type
                             home-dbus-shepherd-services)
          (service-extension home-environment-variables-service-type
                             home-dbus-environment-variables)))
   (default-value (home-dbus-configuration))
   (description
    "Run the session-specific D-Bus inter-process message bus.")))


;;;
;;; Unclutter.
;;;

(define-configuration/no-serialization home-unclutter-configuration
  (unclutter
   (file-like unclutter)
   "The @code{unclutter} package to use.")
  (idle-timeout
   (integer 5)
   "Timeout in seconds after which to hide the cursor."))

(define (home-unclutter-shepherd-service config)
  (list
   (shepherd-service
    (provision '(unclutter))
    (requirement '())
    (one-shot? #t)
    (start #~(make-forkexec-constructor
              (list
               #$(file-append
                  (home-unclutter-configuration-unclutter config)
                  "/bin/unclutter")
               "-idle"
               (number->string
                #$(home-unclutter-configuration-idle-timeout config)))
              #:log-file (string-append
                          (or (getenv "XDG_STATE_HOME")
                              (format #f "~a/.local/state"
                                      (getenv "HOME")))
                          "/log/unclutter.log"))))))

(define home-unclutter-service-type
  (service-type
   (name 'home-unclutter)
   (extensions
    (list
     (service-extension home-shepherd-service-type
                        home-unclutter-shepherd-service)))
   (default-value (home-unclutter-configuration))
   (description "Run the @code{unclutter} daemon, which, on systems using the
Xorg graphical display server, automatically hides the cursor after a
user-defined timeout has expired.")))


;;;
;;; Xmodmap.
;;;

(define-configuration/no-serialization home-xmodmap-configuration
  (xmodmap
   (file-like xmodmap)
   "The @code{xmodmap} package to use.")
  (key-map
   (list '())
   "List of expressions to be read by @code{xmodmap} on service startup."))

(define (serialize-xmodmap-configuration field-name val)
  (define serialize-field
    (match-lambda
      ((key . value)
       (format #f "~a = ~a" key value))
      (e e)))

  #~(string-append
     #$@(interpose (map serialize-field val) "\n" 'suffix)))

(define (xmodmap-shepherd-service config)
  (define config-file
    (mixed-text-file
     "config"
     (serialize-xmodmap-configuration
      #f (home-xmodmap-configuration-key-map config))))

  (list
   (shepherd-service
    (provision '(xmodmap))
    (start #~(make-system-constructor
              (string-join
               (list #$(file-append
                        (home-xmodmap-configuration-xmodmap config)
                        "/bin/xmodmap")
                     #$config-file))))
    (stop #~(make-system-constructor
             #$(file-append setxkbmap "/bin/setxkbmap")))
    (documentation "On startup, run @code{xmodmap} and read the expressions in
the configuration file.  On stop, reset all the mappings back to the
defaults."))))

(define home-xmodmap-service-type
  (service-type
   (name 'home-xmodmap)
   (extensions
    (list
     (service-extension home-shepherd-service-type
                        xmodmap-shepherd-service)))
   (default-value (home-xmodmap-configuration))
   (description "Run the @code{xmodmap} utility to modify keymaps and pointer
buttons under the Xorg display server via user-defined expressions.")))


(define home-startx-command-service-type
  (service-type
   (inherit (system->home-service-type startx-command-service-type))
   (default-value (for-home (xorg-configuration)))))

(define-service-type-mapping
  startx-command-service-type => home-startx-command-service-type)
