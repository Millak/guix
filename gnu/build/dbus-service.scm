;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

;;; Commentary:
;;;
;;; This module contains procedures to interact with D-Bus via the 'dbus-send'
;;; command line utility.  Before using any public procedure
;;;
;;; Code:

(define-module (gnu build dbus-service)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-19)
  #:use-module (srfi srfi-26)
  #:autoload (d-bus protocol connections) (d-bus-conn?
                                           d-bus-conn-flush
                                           d-bus-connect
                                           d-bus-disconnect
                                           d-bus-session-bus-address
                                           d-bus-system-bus-address)
  #:autoload (d-bus protocol messages) (MESSAGE_TYPE_METHOD_CALL
                                        d-bus-headers-ref
                                        d-bus-message-body
                                        d-bus-message-headers
                                        d-bus-read-message
                                        d-bus-write-message
                                        header-PATH
                                        header-DESTINATION
                                        header-INTERFACE
                                        header-MEMBER
                                        header-SIGNATURE
                                        make-d-bus-message)
  #:export (%dbus-query-timeout

            initialize-dbus-connection!
            %current-dbus-connection
            send-dbus
            call-dbus-method

            dbus-available-services
            dbus-service-available?

            with-retries))

(define %dbus-query-timeout 2)          ;in seconds

;;; Use Fibers' sleep to enable cooperative scheduling in Shepherd >= 0.9.0,
;;; which is required at least for the Jami service.
(define sleep*
  (lambda ()                            ;delay execution
    (if (resolve-module '(fibers) #f #:ensure #f)
        (module-ref (resolve-interface '(fibers)) 'sleep)
        (begin
          (format #t "Fibers not available -- blocking 'sleep' in use~%")
          sleep))))

;;;
;;; Utilities.
;;;

(define-syntax-rule (with-retries n delay body ...)
  "Retry the code in BODY up to N times until it doesn't raise an exception nor
return #f, else raise an error.  A delay of DELAY seconds is inserted before
each retry."
  (let loop ((attempts 0))
    (catch #t
      (lambda ()
        (let ((result (begin body ...)))
          (if (not result)
              (error "failed attempt" attempts)
              result)))
      (lambda args
        (if (< attempts n)
            (begin
              ((sleep*) delay)            ;else wait and retry
              (loop (+ 1 attempts)))
            (error "maximum number of retry attempts reached"
                   (quote body ...) args))))))


;;;
;;; Low level wrappers above AC/D-Bus.
;;;

;; The active D-Bus connection (a parameter) used by the other procedures.
(define %current-dbus-connection (make-parameter #f))

(define* (initialize-dbus-connection!
          #:key (address (or (d-bus-session-bus-address)
                             (d-bus-system-bus-address))))
  "Initialize the D-Bus connection.  ADDRESS should be the address of the D-Bus
session, e.g. \"unix:path=/var/run/dbus/system_bus_socket\", the default value
if ADDRESS is not provided and DBUS_SESSION_BUS_ADDRESS is not set.  Return
the initialized D-Bus connection."
  ;; Clear current correction if already active.
  (when (d-bus-conn? (%current-dbus-connection))
    (d-bus-disconnect (%current-dbus-connection)))

  (let ((connection (d-bus-connect address)))
    (%current-dbus-connection connection) ;update connection parameter
    (call-dbus-method "Hello"))           ;initial handshake

  (%current-dbus-connection))

(define* (send-dbus message #:key
                    (connection (%current-dbus-connection))
                    timeout)
  "Send a D-Bus MESSAGE to CONNECTION and return the body of its reply.  Up to
READ-RETRIES replies are read until a matching reply is found, else an error
is raised.  MESSAGE is to be constructed with `make-d-bus-message'.  When the
body contains a single element, it is returned directly, else the body
elements are returned as a list.  TIMEOUT is a timeout value in seconds."
  (let ((serial     (d-bus-write-message connection message))
        (start-time (current-time time-monotonic))
        (timeout* (or timeout %dbus-query-timeout)))
    (d-bus-conn-flush connection)
    (let retry ()
      (when (> (time-second (time-difference (current-time time-monotonic)
                                             start-time))
               timeout*)
        (error 'dbus "fail to get reply in timeout" timeout*))
      (let* ((reply (d-bus-read-message connection))
             (reply-headers (d-bus-message-headers reply))
             (reply-serial (d-bus-headers-ref reply-headers 'REPLY_SERIAL))
             (error-name (d-bus-headers-ref reply-headers 'ERROR_NAME))
             (body (d-bus-message-body reply)))
        ;; Validate the reply matches the message.
        (when error-name
          (error 'dbus "method failed with error" error-name body))
        ;; Some replies do not include a serial header, such as the for the
        ;; org.freedesktop.DBus NameAcquired one.
        (if (and reply-serial (= serial reply-serial))
            (match body
              ((x x* ..1)               ;contains 2 ore more elements
               body)
              ((x)
               x)                       ;single element; return it directly
              (#f #f))
            (retry))))))

(define (argument->signature-type argument)
  "Infer the D-Bus signature type from ARGUMENT."
  ;; XXX: avoid ..1 when using vectors due to a bug (?) in (ice-9 match).
  (match argument
    ((? boolean?) "b")
    ((? string?) "s")
    (#((? string?) (? string?) ...) "as")
    (#(((? string?) . (? string?))
       ((? string?) . (? string?)) ...) "a{ss}")
    (_ (error 'dbus "no rule to infer type from argument" argument))))

(define* (call-dbus-method method
                           #:key
                           (path "/org/freedesktop/DBus")
                           (destination "org.freedesktop.DBus")
                           (interface "org.freedesktop.DBus")
                           (connection (%current-dbus-connection))
                           arguments
                           timeout)
  "Call the D-Bus method specified by METHOD, PATH, DESTINATION and INTERFACE.
The currently active D-Bus CONNECTION is used unless explicitly provided.
Method arguments may be provided via ARGUMENTS sent as the message body.
TIMEOUT limit the maximum time to allow for the reply.  Return the body of the
reply."
  (let ((message (make-d-bus-message
                  MESSAGE_TYPE_METHOD_CALL 0 #f '()
                  `#(,(header-PATH        path)
                     ,(header-DESTINATION destination)
                     ,(header-INTERFACE   interface)
                     ,(header-MEMBER      method)
                     ,@(if arguments
                           (list (header-SIGNATURE
                                  (string-join
                                   (map argument->signature-type arguments)
                                   "")))
                           '()))
                  arguments)))
    (send-dbus message #:connection connection #:timeout timeout)))


;;;
;;; Higher-level, D-Bus procedures.
;;;

(define (dbus-available-services)
  "Return the list of available (acquired) D-Bus services."
  (let ((names (vector->list (call-dbus-method "ListNames"))))
    ;; Remove entries such as ":1.7".
    (remove (cut string-prefix? ":" <>) names)))

(define (dbus-service-available? service)
  "Predicate to check for the D-Bus SERVICE availability."
  (member service (dbus-available-services)))

;; Local Variables:
;; eval: (put 'with-retries 'scheme-indent-function 2)
;; End:
