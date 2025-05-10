;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Fabio Natali <me@fabionatali.com>
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

(define-module (gnu tests upnp)
  #:use-module (gnu services)
  #:use-module (gnu services networking)
  #:use-module (gnu services upnp)
  #:use-module (gnu system vm)
  #:use-module (gnu tests)
  #:use-module (guix gexp)
  #:export (%test-readymedia))

(define %readymedia-cache-file "files.db")
(define %readymedia-cache-path
  (string-append %readymedia-default-cache-directory
                 "/"
                 %readymedia-cache-file))
(define %readymedia-log-path
  (string-append %readymedia-default-log-directory
                 "/"
                 %readymedia-log-file))
(define %readymedia-default-port 8200)
(define %readymedia-media-directory "/media")
(define %readymedia-configuration-test
  (readymedia-configuration
   (port %readymedia-default-port)
   (media-directories
    (list (readymedia-media-directory (path %readymedia-media-directory)
                                      (types '(A V)))))))

(define (run-readymedia-test)
  (define os
    (marionette-operating-system
     (simple-operating-system
      (service dhcp-client-service-type)
      (service readymedia-service-type
               %readymedia-configuration-test))
     #:imported-modules '((gnu services herd)
                          (json parser))
     #:requirements '(readymedia)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette
             (list #$(virtual-machine
                      (operating-system os)
                      (port-forwardings '())))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "readymedia")

          ;; ReadyMedia user
          (test-assert "ReadyMedia user exists"
            (marionette-eval
             '(begin
                (getpwnam #$%readymedia-user-account)
                #t)
             marionette))
          (test-assert "ReadyMedia group exists"
            (marionette-eval
             '(begin
                (getgrnam #$%readymedia-user-group)
                #t)
             marionette))

          ;; Cache directory and file
          (test-assert "cache directory exists"
            (marionette-eval
             '(eq? (stat:type (stat #$%readymedia-default-cache-directory))
                   'directory)
             marionette))
          (test-assert "cache directory has correct ownership"
            (marionette-eval
             '(let ((cache-dir (stat #$%readymedia-default-cache-directory))
                    (user (getpwnam #$%readymedia-user-account)))
                (and (eqv? (stat:uid cache-dir) (passwd:uid user))
                     (eqv? (stat:gid cache-dir) (passwd:gid user))))
             marionette))
          (test-assert "cache directory has expected permissions"
            (marionette-eval
             '(eqv? (stat:perms (stat #$%readymedia-default-cache-directory))
                    #o755)
             marionette))

          ;; Log directory and file
          (test-assert "log directory exists"
            (marionette-eval
             '(eq? (stat:type (stat #$%readymedia-default-log-directory))
                   'directory)
             marionette))
          (test-assert "log directory has correct ownership"
            (marionette-eval
             '(let ((log-dir (stat #$%readymedia-default-log-directory))
                    (user (getpwnam #$%readymedia-user-account)))
                (and (eqv? (stat:uid log-dir) (passwd:uid user))
                     (eqv? (stat:gid log-dir) (passwd:gid user))))
             marionette))
          (test-assert "log directory has expected permissions"
            (marionette-eval
             '(eqv? (stat:perms (stat #$%readymedia-default-log-directory))
                    #o755)
             marionette))
          (test-assert "log file exists"
            (marionette-eval
             '(file-exists? #$%readymedia-log-path)
             marionette))
          (test-assert "log file has expected permissions"
            (marionette-eval
             '(eqv? (stat:perms (stat #$%readymedia-log-path))
                    #o640)
             marionette))

          ;; Service
          (test-assert "ReadyMedia service is running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd)
                             (srfi srfi-1))
                (live-service-running
                 (find (lambda (live-service)
                         (memq 'readymedia
                               (live-service-provision live-service)))
                       (current-services))))
             marionette))
          (test-assert "ReadyMedia service is listening for connections"
            (wait-for-tcp-port #$%readymedia-default-port marionette))

          (test-end))))

  (gexp->derivation "readymedia-test" test))

(define %test-readymedia
  (system-test
   (name "readymedia")
   (description "Test the ReadyMedia service.")
   (value (run-readymedia-test))))
