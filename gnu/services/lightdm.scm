;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019, 2020 L  p R n  d n <guix@lprndn.info>
;;; Copyright © 2020 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu services lightdm)
  #:use-module (gnu artwork)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages display-managers)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages vnc)
  #:use-module (gnu packages xorg)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services desktop)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services xorg)
  #:use-module (gnu services)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (lightdm-seat-configuration
            lightdm-seat-configuration?
            lightdm-seat-configuration-name
            lightdm-seat-configuration-type
            lightdm-seat-configuration-user-session
            lightdm-seat-configuration-autologin-user
            lightdm-seat-configuration-greeter-session
            lightdm-seat-configuration-xserver-command
            lightdm-seat-configuration-session-wrapper
            lightdm-seat-configuration-extra-config

            lightdm-gtk-greeter-configuration
            lightdm-gtk-greeter-configuration?
            lightdm-gtk-greeter-configuration-lightdm-gtk-greeter
            lightdm-gtk-greeter-configuration-assets
            lightdm-gtk-greeter-configuration-theme-name
            lightdm-gtk-greeter-configuration-icon-theme-name
            lightdm-gtk-greeter-configuration-cursor-theme-name
            lightdm-gtk-greeter-configuration-allow-debug
            lightdm-gtk-greeter-configuration-background
            lightdm-gtk-greeter-configuration-a11y-states
            lightdm-gtk-greeter-configuration-reader
            lightdm-gtk-greeter-configuration-extra-config

            lightdm-configuration
            lightdm-configuration?
            lightdm-configuration-lightdm
            lightdm-configuration-allow-empty-passwords?
            lightdm-configuration-xorg-configuration
            lightdm-configuration-greeters
            lightdm-configuration-seats
            lightdm-configuration-xdmcp?
            lightdm-configuration-xdmcp-listen-address
            lightdm-configuration-vnc-server?
            lightdm-configuration-vnc-server-command
            lightdm-configuration-vnc-server-listen-address
            lightdm-configuration-vnc-server-port
            lightdm-configuration-extra-config

            lightdm-service-type))

;;;
;;; Greeters.
;;;

(define list-of-file-likes?
  (list-of file-like?))

(define %a11y-states '(contrast font keyboard reader))

(define (a11y-state? value)
  (memq value %a11y-states))

(define list-of-a11y-states?
  (list-of a11y-state?))

(define-maybe boolean)

(define (serialize-boolean name value)
  (define (strip-trailing-? name)
    ;; field? -> field
    (let ((str (symbol->string name)))
      (if (string-suffix? "?" str)
          (string-drop-right str 1)
          str)))
  (format #f "~a=~:[false~;true~]~%" (strip-trailing-? name) value))

(define-maybe file-like)

(define (serialize-file-like name value)
  #~(format #f "~a=~a~%" '#$name #$value))

(define (serialize-list-of-a11y-states name value)
  (format #f "~a=~a~%" name (string-join (map symbol->string value) ";")))

(define (serialize-string name value)
  (format #f "~a=~a~%" name value))

(define (serialize-number name value)
  (format #f "~a=~a~%" name value))

(define (serialize-list-of-strings _ value)
  (string-join value "\n"))

(define-configuration lightdm-gtk-greeter-configuration
  (lightdm-gtk-greeter
   (file-like lightdm-gtk-greeter)
   "The lightdm-gtk-greeter package to use."
   empty-serializer)
  (assets
   (list-of-file-likes (list adwaita-icon-theme
                             gnome-themes-extra
                             ;; FIXME: hicolor-icon-theme should be in the
                             ;; packages of the desktop templates.
                             hicolor-icon-theme))
   "The list of packages complementing the greeter, such as package providing
icon themes."
   empty-serializer)
  (theme-name
   (string "Adwaita")
   "The name of the theme to use.")
  (icon-theme-name
   (string "Adwaita")
   "The name of the icon theme to use.")
  (cursor-theme-name
   (string "Adwaita")
   "The name of the cursor theme to use.")
  (cursor-theme-size
   (number 16)
   "The size to use for the the cursor theme.")
  (allow-debugging?
   maybe-boolean
   "Set to #t to enable debug log level.")
  (background
   (file-like (file-append %artwork-repository
                           "/backgrounds/guix-checkered-16-9.svg"))
   "The background image to use.")
  ;; FIXME: This should be enabled by default, but it currently doesn't work,
  ;; failing to connect to D-Bus, causing the login to fail.
  (at-spi-enabled?
   (boolean #f)
   "Enable accessibility support through the Assistive Technology Service
Provider Interface (AT-SPI).")
  (a11y-states
   (list-of-a11y-states %a11y-states)
   "The accessibility features to enable, given as list of symbols.")
  (reader
   maybe-file-like
   "The command to use to launch a screen reader.")
  (extra-config
   (list-of-strings '())
   "Extra configuration values to append to the LightDM GTK Greeter
configuration file."))

(define (strip-class-name-brackets name)
  "Remove the '<<' and '>>' brackets from NAME, a symbol."
  (let ((name* (symbol->string name)))
    (if (and (string-prefix? "<<" name*)
             (string-suffix? ">>" name*))
        (string->symbol (string-drop (string-drop-right name* 2) 2))
        (error "unexpected class name" name*))))

(define (config->name config)
  "Return the constructor name (a symbol) from CONFIG."
  (strip-class-name-brackets (class-name (class-of config))))

(define (greeter-configuration->greeter-fields config)
  "Return the fields of CONFIG, a greeter configuration."
  (match config
    ;; Note: register any new greeter configuration here.
    ((? lightdm-gtk-greeter-configuration?)
     lightdm-gtk-greeter-configuration-fields)))

(define (greeter-configuration->packages config)
  "Return the list of greeter packages, including assets, used by CONFIG, a
greeter configuration."
  (match config
    ;; Note: register any new greeter configuration here.
    ((? lightdm-gtk-greeter-configuration?)
     (cons (lightdm-gtk-greeter-configuration-lightdm-gtk-greeter config)
           (lightdm-gtk-greeter-configuration-assets config)))))

;;; TODO: Implement directly in (gnu services configuration), perhaps by
;;; making the FIELDS argument optional.
(define (serialize-configuration* config)
  "Like `serialize-configuration', but not requiring to provide a FIELDS
argument."
  (define fields (greeter-configuration->greeter-fields config))
  (serialize-configuration config fields))

(define (greeter-configuration->conf-name config)
  "Return the file name of CONFIG, a greeter configuration."
  (format #f "~a.conf" (greeter-configuration->greeter-session config)))

(define (greeter-configuration->file config)
  "Serialize CONFIG into a file under the output directory, so that it can be
easily added to XDG_CONF_DIRS."
  (computed-file
   (greeter-configuration->conf-name config)
   #~(begin
       (call-with-output-file #$output
         (lambda (port)
           (format port (string-append
                         "[greeter]\n"
                         #$(serialize-configuration* config))))))))


;;;
;;; Seats.
;;;

(define seat-name? string?)

(define (serialize-seat-name _ value)
  (format #f "[Seat:~a]~%" value))

(define (seat-type? type)
  (memq type '(local xremote)))

(define (serialize-seat-type name value)
  (format #f "~a=~a~%" name value))

(define-maybe seat-type)

(define (greeter-session? value)
  (memq value '(lightdm-gtk-greeter)))

(define (serialize-greeter-session name value)
  (format #f "~a=~a~%" name value))

(define-maybe greeter-session)

(define-maybe string)

;;; Note: all the fields except for the seat name should be 'maybe's, since
;;; the real default value is set by the %lightdm-seat-default define later,
;;; and this avoids repeating ourselves in the serialized configuration file.
(define-configuration lightdm-seat-configuration
  (name
   seat-name
   "The name of the seat.  An asterisk (*) can be used in the name
to apply the seat configuration to all the seat names it matches.")
  (user-session
   maybe-string
   "The session to use by default.  The session name must be provided as a
lowercase string, such as @code{\"gnome\"}, @code{\"ratpoison\"}, etc.")
  (type
   (seat-type 'local)
   "The type of the seat, either the @code{local} or @code{xremote} symbol.")
  (autologin-user
   maybe-string
   "The username to automatically log in with by default.")
  (greeter-session
   (greeter-session 'lightdm-gtk-greeter)
   "The greeter session to use, specified as a symbol.  Currently, only
@code{lightdm-gtk-greeter} is supported.")
  ;; Note: xserver-command must be lazily computed, so that it can be
  ;; overridden via 'lightdm-configuration-xorg-configuration'.
  (xserver-command
   maybe-file-like
   "The Xorg server command to run.")
  (session-wrapper
   (file-like (xinitrc))
   "The xinitrc session wrapper to use.")
  (extra-config
   (list-of-strings '())
   "Extra configuration values to append to the seat configuration section."))

(define (greeter-session->greater-configuration-pred identifier)
  "Return the predicate to check if a configuration is of the type specifying
a greeter identified by IDENTIFIER."
  (match identifier
    ;; Note: register any new greeter identifier here.
    ('lightdm-gtk-greeter
     lightdm-gtk-greeter-configuration?)))

(define (greeter-configuration->greeter-session config)
  "Given CONFIG, a greeter configuration object, return its identifier,
a symbol."
  (let ((suffix "-configuration")
        (greeter-conf-name (config->name config)))
    (string->symbol (string-drop-right (symbol->string greeter-conf-name)
                                       (string-length suffix)))))

(define list-of-seat-configurations?
  (list-of lightdm-seat-configuration?))


;;;
;;; LightDM.
;;;

(define (greeter-configuration? config)
  (or (lightdm-gtk-greeter-configuration? config)
      ;; Note: register any new greeter configuration here.
      ))

(define (list-of-greeter-configurations? greeter-configs)
  (and ((list-of greeter-configuration?) greeter-configs)
       ;; Greeter configurations must also not be provided more than once.
       (let* ((types (map (cut (compose class-name class-of) <>)
                          greeter-configs))
              (dupes (filter (lambda (type)
                               (< 1 (count (cut eq? type <>) types)))
                             types)))
         (unless (null? dupes)
           (leave (G_ "duplicate greeter configurations: ~a~%") dupes)))))

(define-configuration/no-serialization lightdm-configuration
  (lightdm
   (file-like lightdm)
   "The lightdm package to use.")
  (allow-empty-passwords?
   (boolean #f)
   "Whether users not having a password set can login.")
  (debug?
   (boolean #f)
   "Enable verbose output.")
  (xorg-configuration
   (xorg-configuration (xorg-configuration))
   "The default Xorg server configuration to use to generate the Xorg server
start script.  It can be refined per seat via the @code{xserver-command} of
the @code{<lightdm-seat-configuration>} record, if desired.")
  (greeters
   (list-of-greeter-configurations (list (lightdm-gtk-greeter-configuration)))
   "The LightDM greeter configurations specifying the greeters to use.")
  (seats
   (list-of-seat-configurations (list (lightdm-seat-configuration
                                       (name "*"))))
   "The seat configurations to use.  A LightDM seat is akin to a user.")
  (xdmcp?
   (boolean #f)
   "Whether a XDMCP server should listen on port UDP 177.")
  (xdmcp-listen-address
   maybe-string
   "The host or IP address the XDMCP server listens for incoming connections.
When unspecified, listen on for any hosts/IP addresses.")
  (vnc-server?
   (boolean #f)
   "Whether a VNC server is started.")
  (vnc-server-command
   (file-like (file-append tigervnc-server "bin/Xvnc"))
   "The Xvnc command to use for the VNC server, it's possible to provide extra
options not otherwise exposed along the command, for example to disable
security:
@lisp
(vnc-server-command
 (file-append tigervnc-server \"/bin/Xvnc\"
             \" -SecurityTypes None\" ))
@end lisp

Or to set a PasswordFile for the classic (unsecure) VncAuth mecanism:
@lisp
(vnc-server-command
 (file-append tigervnc-server \"/bin/Xvnc\"
             \" -PasswordFile /var/lib/lightdm/.vnc/passwd\"))
@end lisp
The password file should be manually created using the @command{vncpasswd}
command.

Note that LightDM will create new sessions for VNC users, which means they
need to authenticate in the same way as local users would.
")
  (vnc-server-listen-address
   maybe-string
   "The host or IP address the VNC server listens for incoming connections.
When unspecified, listen for any hosts/IP addresses.")
  (vnc-server-port
   (number 5900)
   "The TCP port the VNC server should listen to.")
  (extra-config
   (list-of-strings '())
   "Extra configuration values to append to the LightDM configuration file."))

(define (lightdm-configuration->greeters-config-dir config)
  "Return a directory containing all the serialized greeter configurations
from CONFIG, a <lightdm-configuration> object."
  (file-union "etc-lightdm"
              (append-map (lambda (g)
                            `((,(greeter-configuration->conf-name g)
                               ,(greeter-configuration->file g))))
                          (lightdm-configuration-greeters config))))

(define (lightdm-configuration->packages config)
  "Return all the greeter packages and their assets defined in CONFIG, a
<lightdm-configuration> object, as well as the lightdm package itself."
  (cons (lightdm-configuration-lightdm config)
        (append-map greeter-configuration->packages
                    (lightdm-configuration-greeters config))))

(define (validate-lightdm-configuration config)
  "Sanity check CONFIG, a <lightdm-configuration> record instance."
  ;; This is required to make inter-field validations, such as between the
  ;; seats and greeters.
  (let* ((seats (lightdm-configuration-seats config))
         (greeter-sessions (delete-duplicates
                            (map lightdm-seat-configuration-greeter-session
                                 seats)
                            eq?))
         (greeter-configurations (lightdm-configuration-greeters config))
         (missing-greeters
          (filter-map
           (lambda (id)
             (define pred (greeter-session->greater-configuration-pred id))
             (if (find pred greeter-configurations)
                 #f                     ;happy path
                 id))
           greeter-sessions)))
    (unless (null? missing-greeters)
      (leave (G_ "no greeter configured for seat greeter sessions: ~a~%")
             missing-greeters))))

(define (lightdm-configuration-file config)
  (match-record config <lightdm-configuration>
    (xorg-configuration seats
     xdmcp? xdmcp-listen-address
     vnc-server? vnc-server-command vnc-server-listen-address vnc-server-port
     extra-config)
    (apply
     mixed-text-file
     "lightdm.conf" "
#
# General configuration
#
[LightDM]
greeter-user=lightdm
sessions-directory=/run/current-system/profile/share/xsessions\
:/run/current-system/profile/share/wayland-sessions
remote-sessions-directory=/run/current-system/profile/share/remote-sessions
"
     #~(string-join '#$extra-config "\n")
     "
#
# XDMCP Server configuration
#
[XDMCPServer]
enabled=" (if xdmcp? "true" "false") "\n"
(if (maybe-value-set? xdmcp-listen-address)
    (format #f "xdmcp-listen-address=~a" xdmcp-listen-address)
    "") "

#
# VNC Server configuration
#
[VNCServer]
enabled=" (if vnc-server? "true" "false") "
command=" vnc-server-command "
port=" (number->string vnc-server-port) "\n"
(if (maybe-value-set? vnc-server-listen-address)
    (format #f "vnc-server-listen-address=~a" vnc-server-listen-address)
    "") "

#
# Seat configuration.
#
"
     (map (lambda (seat)
            ;; This complication exists to propagate a default value for
            ;; the 'xserver-command' field of the seats.  Having a
            ;; 'xorg-configuration' field at the root of the
            ;; lightdm-configuration enables the use of
            ;; 'set-xorg-configuration' and can be more convenient.
            (let ((seat* (if (maybe-value-set?
                              (lightdm-seat-configuration-xserver-command seat))
                             seat
                             (lightdm-seat-configuration
                              (inherit seat)
                              (xserver-command (xorg-start-command
                                                xorg-configuration))))))
              (serialize-configuration seat*
                                       lightdm-seat-configuration-fields)))
          seats))))

(define %lightdm-accounts
  (list (user-group (name "lightdm") (system? #t))
        (user-account
         (name "lightdm")
         (group "lightdm")
         (system? #t)
         (comment "LightDM user")
         (home-directory "/var/lib/lightdm")
         (shell (file-append shadow "/sbin/nologin")))))

(define %lightdm-activation
  ;; Ensure /var/lib/lightdm is owned by the "lightdm" user.  Adapted from the
  ;; %gdm-activation.
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define (ensure-ownership directory)
          (let* ((lightdm (getpwnam "lightdm"))
                 (uid (passwd:uid lightdm))
                 (gid (passwd:gid lightdm))
                 (st  (stat directory #f)))
            ;; Recurse into directory only if it has wrong ownership.
            (when (and st
                       (or (not (= uid (stat:uid st)))
                           (not (= gid (stat:gid st)))))
              (for-each (lambda (file)
                          (chown file uid gid))
                        (find-files "directory"
                                    #:directories? #t)))))

        (when (not (stat "/var/lib/lightdm-data" #f))
          (mkdir-p "/var/lib/lightdm-data"))
        (for-each ensure-ownership
                  '("/var/lib/lightdm"
                    "/var/lib/lightdm-data")))))

(define (lightdm-pam-service config)
  "Return a PAM service for @command{lightdm}."
  (unix-pam-service "lightdm"
                    #:login-uid? #t
                    #:allow-empty-passwords?
                    (lightdm-configuration-allow-empty-passwords? config)))

(define (lightdm-greeter-pam-service)
  "Return a PAM service for @command{lightdm-greeter}."
  (pam-service
   (name "lightdm-greeter")
   (auth (list
          ;; Load environment from /etc/environment and ~/.pam_environment.
          (pam-entry (control "required") (module "pam_env.so"))
          ;; Always let the greeter start without authentication.
          (pam-entry (control "required") (module "pam_permit.so"))))
   ;; No action required for account management
   (account (list (pam-entry (control "required") (module "pam_permit.so"))))
   ;; Prohibit changing password.
   (password (list (pam-entry (control "required") (module "pam_deny.so"))))
   ;; Setup session.
   (session (list (pam-entry (control "required") (module "pam_unix.so"))))))

(define (lightdm-autologin-pam-service)
  "Return a PAM service for @command{lightdm-autologin}}."
  (pam-service
   (name "lightdm-autologin")
   (auth
    (list
     ;; Block login if user is globally disabled.
     (pam-entry (control "required") (module "pam_nologin.so"))
     (pam-entry (control "required") (module "pam_succeed_if.so")
                (arguments (list "uid >= 1000")))
     ;; Allow access without authentication.
     (pam-entry (control "required") (module "pam_permit.so"))))
   ;; Stop autologin if account requires action.
   (account (list (pam-entry (control "required") (module "pam_unix.so"))))
   ;; Prohibit changing password.
   (password (list (pam-entry (control "required") (module "pam_deny.so"))))
   ;; Setup session.
   (session (list (pam-entry (control "required") (module "pam_unix.so"))))))

(define (lightdm-pam-services config)
  (list (lightdm-pam-service config)
        (lightdm-greeter-pam-service)
        (lightdm-autologin-pam-service)))

(define (lightdm-shepherd-service config)
  "Return a <lightdm-service> for LightDM using CONFIG."

  (validate-lightdm-configuration config)

  (define lightdm-command
    #~(list #$(file-append (lightdm-configuration-lightdm config)
                           "/sbin/lightdm")
            #$@(if (lightdm-configuration-debug? config)
                   #~("--debug")
                   #~())
            "--config"
            #$(lightdm-configuration-file config)))

  (define lightdm-paths
    (let ((lightdm (lightdm-configuration-lightdm config)))
      #~(string-join
         '#$(map (lambda (dir)
                   (file-append lightdm dir))
                 '("/bin" "/sbin" "/libexec"))
         ":")))

  (define greeters-config-dir
    (lightdm-configuration->greeters-config-dir config))

  (define data-dirs
    ;; LightDM itself needs to be in XDG_DATA_DIRS for the accountsservice
    ;; interface it provides to be picked up.  The greeters must also be in
    ;; XDG_DATA_DIRS to be found.
    (let ((packages (lightdm-configuration->packages config)))
      #~(string-join '#$(map (cut file-append <> "/share") packages)
                     ":")))

  (list
   (shepherd-service
    (documentation "LightDM display manager")
    (requirement '(dbus-system user-processes host-name))
    (provision '(lightdm display-manager xorg-server))
    (respawn? #f)
    (start
     #~(lambda ()
         ;; Note: sadly, environment variables defined for 'lightdm' are
         ;; cleared and/or overridden by /etc/profile by its spawned greeters,
         ;; so an out-of-band means such as /etc is required.
         (fork+exec-command #$lightdm-command
                            ;; Lightdm needs itself in its PATH.
                            #:environment-variables
                            (list
                             ;; It knows to look for greeter configurations in
                             ;; XDG_CONFIG_DIRS...
                             (string-append "XDG_CONFIG_DIRS="
                                            #$greeters-config-dir)
                             ;; ... and for greeter .desktop files as well as
                             ;; lightdm accountsservice interface in
                             ;; XDG_DATA_DIRS.
                             (string-append "XDG_DATA_DIRS="
                                            #$data-dirs)
                             (string-append "PATH=" #$lightdm-paths)))))
    (stop #~(make-kill-destructor)))))

(define lightdm-service-type
  (handle-xorg-configuration
   lightdm-configuration
   (service-type
    (name 'lightdm)
    (default-value (lightdm-configuration))
    (extensions
     (list (service-extension pam-root-service-type lightdm-pam-services)
           (service-extension shepherd-root-service-type
                              lightdm-shepherd-service)
           (service-extension activation-service-type
                              (const %lightdm-activation))
           (service-extension dbus-root-service-type
                              (compose list lightdm-configuration-lightdm))
           (service-extension polkit-service-type
                              (compose list lightdm-configuration-lightdm))
           (service-extension account-service-type
                              (const %lightdm-accounts))
           ;; Add 'lightdm' to the system profile, so that its
           ;; 'share/accountsservice' D-Bus service extension directory can be
           ;; found via the 'XDG_DATA_DIRS=/run/current-system/profile/share'
           ;; environment variable set in the wrapper of the
           ;; libexec/accounts-daemon binary of the accountsservice package.
           ;; This daemon is spawned by D-Bus, and there's little we can do to
           ;; affect its environment.  For more reading, see:
           ;; https://github.com/NixOS/nixpkgs/issues/45059.
           (service-extension profile-service-type
                              lightdm-configuration->packages)
           ;; This is needed for the greeter itself to find its configuration,
           ;; because XDG_CONF_DIRS gets overridden by /etc/profile.
           (service-extension
            etc-service-type
            (lambda (config)
              `(("lightdm"
                 ,(lightdm-configuration->greeters-config-dir config)))))))
    (description "Run @code{lightdm}, the LightDM graphical login manager."))))


;;;
;;; Generate documentation.
;;;
(define (generate-doc)
  (configuration->documentation 'lightdm-configuration)
  (configuration->documentation 'lightdm-gtk-greeter-configuration)
  (configuration->documentation 'lightdm-seat-configuration))
