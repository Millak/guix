;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016-2020, 2022, 2024-2025 Ludovic Courtès <ludo@gnu.org>
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

(define-module (gnu tests avahi)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system nss)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services avahi)
  #:use-module (gnu services base)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix monads)
  #:export (%test-nss-mdns))

;;;
;;; Avahi and NSS-mDNS.
;;;

(define %avahi-os
  (operating-system
    (inherit %simple-os)
    (name-service-switch %mdns-host-lookup-nss)
    (services (cons* (service avahi-service-type
                              (avahi-configuration (debug? #t)))
                     (service dbus-root-service-type)
                     (service dhcpcd-service-type) ;needed for multicast

                     ;; Enable heavyweight debugging output.
                     (modify-services (operating-system-user-services
                                       %simple-os)
                       (nscd-service-type config
                                          => (nscd-configuration
                                              (inherit config)
                                              (debug-level 3)
                                              (log-file "/dev/console")))
                       (shepherd-system-log-service-type
                        config
                        =>
                        (system-log-configuration
                         (inherit config)
                         (message-destination #~(const '("/dev/console"))))))))))

(define (run-nss-mdns-test)
  ;; Test resolution of '.local' names via libc.  Start the marionette service
  ;; *after* nscd.  Failing to do that, libc will try to connect to nscd,
  ;; fail, then never try again (see '__nss_not_use_nscd_hosts' in libc),
  ;; leading to '.local' resolution failures.
  (define os
    (marionette-operating-system
     %avahi-os
     #:requirements '(nscd)
     #:imported-modules '((gnu services herd)
                          (guix combinators))))

  (define mdns-host-name
    (string-append (operating-system-host-name os)
                   ".local"))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-1)
                       (srfi srfi-64)
                       (ice-9 match))

          (define marionette
            (make-marionette (list #$(virtual-machine os))))

          (mkdir #$output)
          (chdir #$output)

          (test-runner-current (system-test-runner))
          (test-begin "avahi")

          (test-assert "nscd PID file is created"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'nscd))
             marionette))

          (test-assert "nscd is listening on its socket"
            (marionette-eval
             ;; XXX: Work around a race condition in nscd: nscd creates its
             ;; PID file before it is listening on its socket.
             '(let ((sock (socket PF_UNIX SOCK_STREAM 0)))
                (let try ()
                  (catch 'system-error
                    (lambda ()
                      (connect sock AF_UNIX "/var/run/nscd/socket")
                      (close-port sock)
                      (format #t "nscd is ready~%")
                      #t)
                    (lambda args
                      (format #t "waiting for nscd...~%")
                      (usleep 500000)
                      (try)))))
             marionette))

          (test-assert "avahi is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'avahi-daemon))
             marionette))

          (test-assert "network is up"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'networking))
             marionette))

          (test-equal "avahi-resolve-host-name"
            0
            (marionette-eval
             '(system*
               "/run/current-system/profile/bin/avahi-resolve-host-name"
               "-v" #$mdns-host-name)
             marionette))

          (test-equal "avahi-browse"
            0
            (marionette-eval
             '(system* "/run/current-system/profile/bin/avahi-browse" "-avt")
             marionette))

          (test-assert "getaddrinfo .local"
            ;; Wait for the 'avahi-daemon' service and perform a resolution.
            (match (marionette-eval
                    '(getaddrinfo #$mdns-host-name)
                    marionette)
              (((? vector? addrinfos) ..1)
               (pk 'getaddrinfo addrinfos)
               (and (any (lambda (ai)
                           (= AF_INET (addrinfo:fam ai)))
                         addrinfos)
                    (any (lambda (ai)
                           (= AF_INET6 (addrinfo:fam ai)))
                         addrinfos)))))

          (test-assert "gethostbyname .local"
            (match (pk 'gethostbyname
                       (marionette-eval '(gethostbyname #$mdns-host-name)
                                        marionette))
              ((? vector? result)
               (and (string=? (hostent:name result) #$mdns-host-name)
                    (= (hostent:addrtype result) AF_INET)))))


          (test-end))))

  (gexp->derivation "nss-mdns" test))

(define %test-nss-mdns
  (system-test
   (name "nss-mdns")
   (description
    "Test Avahi's multicast-DNS implementation, and in particular, test its
glibc name service switch (NSS) module.")
   (value (run-nss-mdns-test))))
