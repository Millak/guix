;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu tests games)
  #:use-module (gnu packages luanti)
  #:use-module (gnu tests)
  #:use-module (gnu services)
  #:use-module (gnu services games)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (%test-luanti))

(define (run-luanti-test name config)
  "Run a test of an OS running LUANTI-SERVICE."
  (define os
    (marionette-operating-system
     (simple-operating-system
      (service luanti-service-type config))
     #:imported-modules '((gnu build dbus-service)
                          (gnu services herd))))

  (define vm (virtual-machine
               (operating-system os)
               (memory-size 1024)))

  (define test
    (with-imported-modules (source-module-closure
                            '((gnu build marionette)))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "luanti")

          (test-assert "luanti service can be stopped"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (stop-service 'luanti))
             marionette))

          (test-assert "luanti service can be started"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'luanti))
             marionette))

          (test-assert "luanti server is responding on configured port"
            ;; This is based on the Python script example in doc/protocol.txt.
            (marionette-eval
             `(begin
                (use-modules ((gnu build dbus-service) #:select (with-retries))
                             (gnu services herd)
                             (ice-9 match)
                             (rnrs bytevectors)
                             (rnrs bytevectors gnu))

                (define sock (socket PF_INET SOCK_DGRAM 0))
                (define addr (make-socket-address AF_INET INADDR_LOOPBACK
                                                  ,#$(luanti-configuration-port
                                                      config)))
                (define probe #vu8(#x4f #x45 #x74 #x03 #x00 #x00 #x00 #x01))
                (define buf (make-bytevector 1000))

                (with-retries 25 1
                  (sendto sock probe addr)
                  (match (select (list sock) '() '() 2) ;limit time to block
                    (((sock) _ _)
                     (match (recvfrom! sock buf)
                       ((byte-count . _)
                        (and (>= (pk 'byte-count byte-count) 14)
                             (pk 'peer-id (bytevector-slice buf 12 2)))))))))
             marionette))

          (test-end))))

  (gexp->derivation name test))

(define %test-luanti
  (system-test
   (name "luanti")
   (description "Connect to a running Luanti server.")
   (value (run-luanti-test name (luanti-configuration
                                 (game luanti-mineclonia)
                                 ;; To test some extra code paths.
                                 (mods (list luanti-whitelist)))))))
