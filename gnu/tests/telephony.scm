;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>.
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

(define-module (gnu tests telephony)
  #:use-module (gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu tests)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services telephony)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:export (%test-jami
            %test-jami-provisioning
            %test-jami-provisioning-partial))

;;;
;;; Jami daemon.
;;;

(include "data/jami-dummy-account.dat") ;defines %jami-account-content-sexp

(define %dummy-jami-account-archive
  ;; A Jami account archive is a gzipped JSON file.
  (computed-file
   "dummy-jami-account.gz"
   (with-extensions (list guile-json-4 guile-zlib)
     #~(begin
         (use-modules (json) (zlib))
         (let ((port (open-output-file #$output)))
           (call-with-gzip-output-port port
             (lambda (port)
               (scm->json '#$%jami-account-content-sexp port))))))))

(define %allowed-contacts '("1dbcb0f5f37324228235564b79f2b9737e9a008f"
                            "2dbcb0f5f37324228235564b79f2b9737e9a008f"))

(define %moderators '("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
                      "bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb"))

(define %dummy-jami-account (jami-account
                             (archive %dummy-jami-account-archive)
                             (allowed-contacts %allowed-contacts)
                             (moderators %moderators)
                             (rendezvous-point? #t)
                             (peer-discovery? #f)
                             (bootstrap-hostnames '("bootstrap.me"
                                                    "fallback.another.host"))
                             (name-server-uri "https://my.name.server")))

;;; Like %dummy-jami-account, but with allowed-contacts and moderators left
;;; unset (thus taking the value *unspecified*).
(define %dummy-jami-account-partial
  (jami-account
   (archive %dummy-jami-account-archive)
   (rendezvous-point? #t)
   (peer-discovery? #f)
   (bootstrap-hostnames '("bootstrap.me"
                          "fallback.another.host"))
   (name-server-uri "https://my.name.server")))

(define* (make-jami-os #:key provisioning? partial?)
  (operating-system
    (host-name "jami")
    (timezone "America/Montreal")
    (locale "en_US.UTF-8")

    (bootloader (bootloader-configuration
                 (bootloader grub-bootloader)
                 (targets '("/dev/sdX"))))
    (file-systems (cons (file-system
                          (device (file-system-label "my-root"))
                          (mount-point "/")
                          (type "ext4"))
                        %base-file-systems))
    (firmware '())

    (services (cons* (service jami-service-type
                              (if provisioning?
                                  (jami-configuration
                                   (debug? #t)
                                   (accounts
                                    (list (if partial?
                                              %dummy-jami-account-partial
                                              %dummy-jami-account))))
                                  (jami-configuration
                                   (debug? #t))))
                     (service dbus-root-service-type)
                     ;; The following services/packages are added for
                     ;; debugging purposes.
                     (service dhcp-client-service-type)
                     (service openssh-service-type
                              (openssh-configuration
                               (permit-root-login #t)
                               (allow-empty-passwords? #t)))
                     %base-services))
    (packages (cons* (specification->package "recutils")
                     (specification->package "strace")
                     %base-packages))))

(define %jami-os
  (make-jami-os))

(define %jami-os-provisioning
  (make-jami-os #:provisioning? #t))

(define %jami-os-provisioning-partial
  (make-jami-os #:provisioning? #t #:partial? #t))

(define* (run-jami-test #:key provisioning? partial?)
  "Run tests in %JAMI-OS.  When PROVISIONING? is true, test the accounts
provisioning feature of the service.  When PARTIAL? is #t, some fields of the
jami account used as part of the jami configuration are left *unspecified*."
  (define os (marionette-operating-system
              (if provisioning?
                  (if partial?
                      %jami-os-provisioning-partial
                      %jami-os-provisioning)
                  %jami-os)
              #:imported-modules '((gnu services herd)
                                   (guix combinators))))
  (define vm (virtual-machine
              (operating-system os)
              (memory-size 512)))

  (define username (assoc-ref %jami-account-content-sexp
                              "Account.username"))

  (define test
    (with-extensions (list guile-packrat ;used by guile-ac-d-bus
                           guile-ac-d-bus
                           ;; Fibers is needed to provide the non-blocking
                           ;; variant of the 'sleep' procedure.
                           guile-fibers)
      (with-imported-modules (source-module-closure
                              '((gnu build marionette)
                                (gnu build dbus-service)
                                (gnu build jami-service)))
        #~(begin
            (use-modules (rnrs base)
                         (srfi srfi-11)
                         (srfi srfi-64)
                         (gnu build marionette)
                         (gnu build dbus-service)
                         (gnu build jami-service))

            (setenv "DBUS_SESSION_BUS_ADDRESS" "unix:path=/var/run/jami/bus")

            (define marionette
              (make-marionette (list #$vm)))

            (test-runner-current (system-test-runner #$output))
            (test-begin "jami")

            (test-assert "service is running"
              (marionette-eval
               '(begin
                  (use-modules (gnu build jami-service))
                  (jami-service-available?))
               marionette))

            (test-assert "service can be stopped"
              (marionette-eval
               '(begin
                  (use-modules (gnu build jami-service)
                               (gnu services herd)
                               (rnrs base))
                  (assert (jami-service-available?))

                  (stop-service 'jami)

                  (with-retries 20 1 (not (jami-service-available?))))
               marionette))

            (test-assert "service can be restarted"
              (marionette-eval
               '(begin
                  (use-modules (gnu build dbus-service)
                               (gnu build jami-service)
                               (gnu services herd)
                               (rnrs base)                               )
                  ;; Start the service.
                  (start-service 'jami)
                  (with-retries 20 1 (jami-service-available?))
                  ;; Restart the service.
                  (restart-service 'jami)
                  (with-retries 20 1 (jami-service-available?)))
               marionette))

            (unless #$provisioning? (test-skip 1))
            (test-assert "jami accounts provisioning, account present"
              (marionette-eval
               '(begin
                  (use-modules (gnu build dbus-service)
                               (gnu services herd)
                               (rnrs base))
                  ;; Accounts take some time to appear after being added.
                  (with-retries 20 1
                    (with-shepherd-action 'jami ('list-accounts) results
                      (let ((account (assoc-ref (car results) #$username)))
                        (assert (string=? #$username
                                          (assoc-ref account
                                                     "Account.username")))))))
               marionette))

            (unless #$(and provisioning? (not partial?)) (test-skip 1))
            (test-assert "jami accounts provisioning, allowed-contacts"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd)
                               (rnrs base)
                               (srfi srfi-1))

                  ;; Public mode is disabled.
                  (with-shepherd-action 'jami ('list-account-details)
                                        results
                    (let ((account (assoc-ref (car results) #$username)))
                      (assert (string=? "false"
                                        (assoc-ref account
                                                   "DHT.PublicInCalls")))))

                  ;; Allowed contacts match those declared in the configuration.
                  (with-shepherd-action 'jami ('list-contacts) results
                    (let ((contacts (assoc-ref (car results) #$username)))
                      (assert (lset= string-ci=? contacts '#$%allowed-contacts)))))
               marionette))

            (unless #$(and provisioning? (not partial?)) (test-skip 1))
            (test-assert "jami accounts provisioning, moderators"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd)
                               (rnrs base)
                               (srfi srfi-1))

                  ;; Moderators match those declared in the configuration.
                  (with-shepherd-action 'jami ('list-moderators) results
                    (let ((moderators (assoc-ref (car results) #$username)))
                      (assert (lset= string-ci=? moderators '#$%moderators))))

                  ;; Moderators can be added via the Shepherd action.
                  (with-shepherd-action 'jami
                      ('add-moderator "cccccccccccccccccccccccccccccccccccccccc"
                                      #$username) results
                    (let ((moderators (car results)))
                      (assert (lset= string-ci=? moderators
                                     (cons "cccccccccccccccccccccccccccccccccccccccc"
                                           '#$%moderators))))))
               marionette))

            (unless #$provisioning? (test-skip 1))
            (test-assert "jami service actions, ban/unban contacts"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd)
                               (rnrs base)
                               (srfi srfi-1))

                  ;; Globally ban a contact.
                  (with-shepherd-action 'jami
                      ('ban-contact "1dbcb0f5f37324228235564b79f2b9737e9a008f") _
                    (with-shepherd-action 'jami ('list-banned-contacts) results
                      (every (match-lambda
                               ((username . banned-contacts)
                                (member "1dbcb0f5f37324228235564b79f2b9737e9a008f"
                                        banned-contacts)))
                             (car results))))

                  ;; Ban a contact for a single account.
                  (with-shepherd-action 'jami
                      ('ban-contact "dddddddddddddddddddddddddddddddddddddddd"
                                    #$username) _
                    (with-shepherd-action 'jami ('list-banned-contacts) results
                      (every (match-lambda
                               ((username . banned-contacts)
                                (let ((found? (member "dddddddddddddddddddddddddddddddddddddddd"
                                                      banned-contacts)))
                                  (if (string=? #$username username)
                                      found?
                                      (not found?)))))
                             (car results)))))
               marionette))

            (unless #$provisioning? (test-skip 1))
            (test-assert "jami service actions, enable/disable accounts"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd)
                               (rnrs base))

                  (with-shepherd-action 'jami
                      ('disable-account #$username) _
                    (with-shepherd-action 'jami ('list-accounts) results
                      (let ((account (assoc-ref (car results) #$username)))
                        (assert (string= "false"
                                         (assoc-ref account "Account.enable"))))))

                  (with-shepherd-action 'jami
                      ('enable-account #$username) _
                    (with-shepherd-action 'jami ('list-accounts) results
                      (let ((account (assoc-ref (car results) #$username)))
                        (assert (string= "true"
                                         (assoc-ref account "Account.enable")))))))
               marionette))

            (unless #$provisioning? (test-skip 1))
            (test-assert "jami account parameters"
              (marionette-eval
               '(begin
                  (use-modules (gnu services herd)
                               (rnrs base)
                               (srfi srfi-1))

                  (with-shepherd-action 'jami ('list-account-details) results
                    (let ((account-details (assoc-ref (car results)
                                                      #$username)))
                      (assert (lset<=
                               equal?
                               '(("Account.hostname" .
                                  "bootstrap.me;fallback.another.host")
                                 ("Account.peerDiscovery" . "false")
                                 ("Account.rendezVous" . "true")
                                 ("RingNS.uri" . "https://my.name.server"))
                               account-details)))))
               marionette))

            (test-end)))))

  (gexp->derivation (if provisioning?
                        (if partial?
                            "jami-provisioning-partial-test"
                            "jami-provisioning-partial")
                        "jami-test")
    test))

(define %test-jami
  (system-test
   (name "jami")
   (description "Basic tests for the jami service.")
   (value (run-jami-test))))

(define %test-jami-provisioning
  (system-test
   (name "jami-provisioning")
   (description "Provisioning test for the jami service.")
   (value (run-jami-test #:provisioning? #t))))

;;; Thi test verifies that <jami-account> values can be left unspecified
;;; without causing any issue (see: https://issues.guix.gnu.org/56799).
(define %test-jami-provisioning-partial
  (system-test
   (name "jami-provisioning-partial")
   (description "Provisioning test for the jami service, when some of the
'maybe' fields aren't provided (such that their value end up being
*unspecified*.")
   (value (run-jami-test #:provisioning? #t #:partial? #t))))
