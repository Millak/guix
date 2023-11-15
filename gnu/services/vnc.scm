;;; GNU Guix --- Functional package management for GNU
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

(define-module (gnu services vnc)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages vnc)
  #:use-module ((gnu services) #:hide (delete))
  #:use-module (gnu system shadow)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix records)

  #:export (xvnc-configuration
            xvnc-configuration-xvnc
            xvnc-configuration-display-number
            xvnc-configuration-geometry
            xvnc-configuration-depth
            xvnc-configuration-port
            xvnc-configuration-ipv4?
            xvnc-configuration-ipv6?
            xvnc-configuration-password-file
            xvnc-configuration-xdmcp?
            xvnc-configuration-inetd?
            xvnc-configuration-frame-rate
            xvnc-configuration-security-types
            xvnc-configuration-localhost?
            xvnc-configuration-log-level
            xvnc-configuration-extra-options

            xvnc-service-type))

;;;
;;; Xvnc.
;;;

(define (color-depth? x)
  (member x '(16 24 32)))

(define (port? x)
  (and (number? x)
       (and (>= x 0) (<= x 65535))))

(define-maybe/no-serialization port)

(define-maybe/no-serialization string)

(define %security-types '("None" "VncAuth" "Plain" "TLSNone" "TLSVnc" "TLSPlain"
                          "X509None" "X509Vnc"))

(define (security-type? x)
  (member x %security-types))

(define (security-types? x)
  (and (list? x)
       (and-map security-type? x)))

(define (log-level? x)
  (and (number? x)
       (and (>= x 0) (<= x 100))))

(define (strings? x)
  (and (list? x)
       (and-map string? x)))

(define-configuration/no-serialization xvnc-configuration
  (xvnc
   (file-like tigervnc-server)
   "The package that provides the Xvnc binary.")
  (display-number
   (number 0)
   "The display number used by Xvnc.  You should set this to a number not
already used by a Xorg server.  When remoting a complete desktop session via
XDMCP and using a compatible VNC viewer as provided by the
@code{tigervnc-client} or @code{turbovnc} packages, the geometry is
automatically adjusted.")
  (geometry
   (string "1024x768")
   "The size of the desktop to be created.")
  (depth
   (color-depth 24)
   "The pixel depth in bits of the desktop to be created.  Accepted values are
16, 24 or 32.")
  (port
   maybe-port
   "The port on which to listen for connections from viewers.  When left
unspecified, it defaults to 5900 plus the display number.")
  (ipv4?
   (boolean #t)
   "Use IPv4 for incoming and outgoing connections.")
  (ipv6?
   (boolean #t)
   "Use IPv6 for incoming and outgoing connections.")
  (password-file
   maybe-string
   "The password file to use, if any.  Refer to vncpasswd(1) to learn how to
generate such a file.")
  (xdmcp?
   (boolean #f)
   "Query the XDMCP server for a session.  This enables users to log in a
desktop session from the login manager screen.  For a multiple users scenario,
you'll want to enable the @code{inetd?} option as well, so that each
connection to the VNC server is handled separately rather than shared.")
  (inetd?
   (boolean #f)
   "Use an Inetd-style service, which runs the Xvnc server on demand.")
  (frame-rate
   (number 60)
   "The maximum number of updates per second sent to each client.")
  (security-types
   (security-types (list "None"))
   (format #f "The allowed security schemes to use for incoming connections.
The default is \"None\", which is safe given that Xvnc is configured to
authenticate the user via the display manager, and only for local connections.
Accepted values are any of the following: ~s" %security-types))
  (localhost?
   (boolean #t)
   "Only allow connections from the same machine.  It is set to @code{#true}
by default for security, which means SSH or another secure means should be
used to expose the remote port.")
  (log-level
   (log-level 30)
   "The log level, a number between 0 and 100, 100 meaning most verbose
output.  The log messages are output to syslog.")
  (extra-options
   (strings '())
   "This can be used to provide extra Xvnc options not exposed via this
<xvnc-configuration> record."))

(define (xvnc-configuration->command-line-arguments config)
  "Derive the command line arguments to used to launch the Xvnc daemon from
CONFIG, a <xvnc-configuration> object."
  (match-record config <xvnc-configuration>
    (xvnc display-number geometry depth port ipv4? ipv6? password-file xdmcp?
          inetd? frame-rate security-types localhost? log-level extra-options)
    #~(list #$(file-append xvnc "/bin/Xvnc")
            #$@(if inetd? '() (list (format #f ":~a" display-number)))
            "-geometry" #$geometry
            "-depth" #$(number->string depth)
            #$@(if inetd?
                   (list "-inetd")
                   '())
            #$@(if (not inetd?)
                   (if (maybe-value-set? port)
                       (list "-rfbport" (number->string port))
                       '())
                   '())
            #$@(if (not inetd?)
                   (if ipv4?
                       (list "-UseIPv4")
                       '())
                   '())
            #$@(if (not inetd?)
                   (if ipv6?
                       (list "-UseIPv6")
                       '())
                   '())
            #$@(if (maybe-value-set? password-file)
                   (list "-PasswordFile" password-file)
                   '())
            "-FrameRate" #$(number->string frame-rate)
            "-SecurityTypes" #$(string-join security-types ",")
            #$@(if localhost?
                   (list "-localhost")
                   '())
            "-Log" #$(format #f "*:syslog:~a" log-level)
            #$@(if xdmcp?
                   (list "-query" "localhost" "-once")
                   '())
            #$@extra-options)))

(define %xvnc-accounts
  (list (user-group
         (name "xvnc")
         (system? #t))
        (user-account
         (name "xvnc")
         (group "xvnc")
         (system? #t)
         (comment "User for Xvnc server")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (xvnc-shepherd-service config)
  "Return a <shepherd-service> for Xvnc with CONFIG."
  (let* ((display-number (xvnc-configuration-display-number config))
         (port (if (maybe-value-set? (xvnc-configuration-port config))
                   (xvnc-configuration-port config)
                   #f))
         (port* (or port (+ 5900 display-number))))
    (shepherd-service
     (provision '(xvnc vncserver))
     (documentation "Run the Xvnc server.")
     (requirement '(networking syslogd))
     (start (if (xvnc-configuration-inetd? config)
                #~(let* ((inaddr (if #$(xvnc-configuration-localhost? config)
                                     INADDR_LOOPBACK
                                     INADDR_ANY))
                         (in6addr (if #$(xvnc-configuration-localhost? config)
                                      IN6ADDR_LOOPBACK
                                      IN6ADDR_ANY))
                         (ipv4-socket (and #$(xvnc-configuration-ipv4? config)
                                           (make-socket-address AF_INET inaddr
                                                                #$port*)))
                         (ipv6-socket (and #$(xvnc-configuration-ipv6? config)
                                           (make-socket-address AF_INET6 in6addr
                                                                #$port*))))
                    (make-inetd-constructor
                     #$(xvnc-configuration->command-line-arguments config)
                     `(,@(if ipv4-socket
                             (list (endpoint ipv4-socket))
                             '())
                       ,@(if ipv6-socket
                             (list (endpoint ipv6-socket))
                             '()))
                     #:requirements '#$requirement
                     #:user "xvnc"
                     #:group "xvnc"))
                #~(make-forkexec-constructor
                   #$(xvnc-configuration->command-line-arguments config)
                   #:user "xvnc"
                   #:group "xvnc")))
     (stop #~(make-inetd-destructor)))))

(define xvnc-service-type
  (service-type
   (name 'xvnc)
   (default-value (xvnc-configuration))
   (description "Run the Xvnc server, which creates a virtual X11 session and
allow remote clients connecting to it via the remote framebuffer (RFB)
protocol.")
   (extensions (list (service-extension
                      shepherd-root-service-type
                      (compose list xvnc-shepherd-service))
                     (service-extension account-service-type
                                        (const %xvnc-accounts))))))
