;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2018 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2017-2018, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu tests messaging)
  #:use-module (gnu tests)
  #:use-module (gnu system)
  #:use-module (gnu system vm)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services messaging)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages)
  #:use-module (gnu packages irc)
  #:use-module (gnu packages messaging)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages screen)
  #:use-module (gnu packages tls)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:export (%test-prosody
            %test-bitlbee
            %test-ngircd
            %test-pounce
            %test-quassel))

(define (run-xmpp-test name xmpp-service pid-file create-account)
  "Run a test of an OS running XMPP-SERVICE, which writes its PID to PID-FILE."
  (define os
    (marionette-operating-system
     (simple-operating-system (service dhcp-client-service-type)
                              xmpp-service)
     #:imported-modules '((gnu services herd))))

  (define port 15222)

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((,port . 5222)))))

  (define username "alice")
  (define server "localhost")
  (define jid (string-append username "@" server))
  (define password "correct horse battery staple")
  (define message "hello world")
  (define witness "/tmp/freetalk-witness")

  (define script.ft
    (scheme-file
     "script.ft"
     #~(begin
         (define (handle-received-message time from nickname message)
           (define (touch file-name)
             (call-with-output-file file-name (const #t)))
           (when (equal? message #$message)
             (touch #$witness)))
         (add-hook! ft-message-receive-hook handle-received-message)

         (ft-set-jid! #$jid)
         (ft-set-password! #$password)
         (ft-set-server! #$server)
         (ft-set-port! #$port)
         (ft-set-sslconn! #f)
         (ft-connect-blocking)
         (ft-send-message #$jid #$message)

         (ft-set-daemon)
         (ft-main-loop))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (gnu build marionette)
                       (srfi srfi-64))

          (define marionette
            (make-marionette (list #$vm)))

          (define (host-wait-for-file file)
            ;; Wait until FILE exists in the host.
            (let loop ((i 60))
              (cond ((file-exists? file)
                     #t)
                    ((> i 0)
                     (begin
                       (sleep 1))
                     (loop (- i 1)))
                    (else
                     (error "file didn't show up" file)))))

          (test-runner-current (system-test-runner #$output))
          (test-begin "xmpp")

          ;; Wait for XMPP service to be up and running.
          (test-assert "service running"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'xmpp-daemon))
             marionette))

          ;; Check XMPP service's PID.
          (test-assert "service process id"
            (let ((pid (number->string (wait-for-file #$pid-file
                                                      marionette))))
              (marionette-eval `(file-exists? (string-append "/proc/" ,pid))
                               marionette)))

          ;; Alice sends an XMPP message to herself, with Freetalk.
          (test-assert "client-to-server communication"
            (let ((freetalk-bin (string-append #$freetalk "/bin/freetalk")))
              (marionette-eval '(system* #$create-account #$jid #$password)
                               marionette)
              ;; Freetalk requires write access to $HOME.
              (setenv "HOME" "/tmp")
              (system* freetalk-bin "-s" #$script.ft)
              (host-wait-for-file #$witness)))

          (test-end))))

  (gexp->derivation name test))

(define %create-prosody-account
  (program-file
   "create-account"
   #~(begin
       (use-modules (ice-9 match))
       (match (command-line)
         ((command jid password)
          (let ((password-input (format #f "\"~a~%~a\"" password password))
                (prosodyctl #$(file-append prosody "/bin/prosodyctl")))
            (system (string-join
                     `("echo" ,password-input "|" ,prosodyctl "adduser" ,jid)
                     " "))))))))

(define %test-prosody
  (let* ((config (prosody-configuration
                  (insecure-sasl-mechanisms '())
                  (virtualhosts
                   (list
                    (virtualhost-configuration
                     (domain "localhost")))))))
    (system-test
     (name "prosody")
     (description "Connect to a running Prosody daemon.")
     (value (run-xmpp-test name
                           (service prosody-service-type config)
                           (prosody-configuration-pidfile config)
                           %create-prosody-account)))))


;;;
;;; BitlBee.
;;;

(define (run-bitlbee-test)
  (define os
    (marionette-operating-system
     (simple-operating-system (service dhcp-client-service-type)
                              (service bitlbee-service-type
                                       (bitlbee-configuration
                                        (interface "0.0.0.0"))))
     #:imported-modules (source-module-closure
                         '((gnu services herd)))))

  (define vm
    (virtual-machine
     (operating-system os)
     (port-forwardings `((6667 . 6667)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (ice-9 rdelim)
                       (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "bitlbee")

          (test-assert "service started"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (start-service 'bitlbee))
             marionette))

          (test-assert "connect"
            (let* ((address (make-socket-address AF_INET INADDR_LOOPBACK
                                                 6667))
                   (sock    (socket AF_INET SOCK_STREAM 0)))
              (connect sock address)
              ;; See <https://tools.ietf.org/html/rfc1459>.
              (->bool (string-contains (pk 'message (read-line sock))
                                       "BitlBee"))))

          (test-end))))

  (gexp->derivation "bitlbee-test" test))

(define %test-bitlbee
  (system-test
   (name "bitlbee")
   (description "Connect to a BitlBee IRC server.")
   (value (run-bitlbee-test))))


;;;
;;; ngIRCd.
;;;

(define %ngircd-os
  (operating-system
    (inherit %simple-os)
    (packages (cons* ii screen %base-packages))
    (services
     (cons*
      (service dhcp-client-service-type)
      (service ngircd-service-type
               (ngircd-configuration
                (debug? #t)
                (global
                 (ngircd-global
                  (server-uid 990)
                  (server-gid 990)))
                ;; There is no need to serialize the following sections, which
                ;; are all optional, but include them anyway to test the
                ;; serializers.
                (limits (ngircd-limits))
                (options (ngircd-options))
                (ssl (ngircd-ssl))
                (operators (list (ngircd-operator
                                  (name "apteryx")
                                  (password "1234"))))
                (channels
                 (list (ngircd-channel
                        (name "#guix")
                        (topic "GNU Guix | https://guix.gnu.org"))))))
      %base-services))))

(define (run-ngircd-test)
  (define vm
    (virtual-machine
     (operating-system
       (marionette-operating-system
        %ngircd-os
        #:imported-modules (source-module-closure
                            '((gnu build dbus-service)
                              (guix build utils)
                              (gnu services herd)))))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "ngircd")

          (test-assert "ngircd service runs"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'ngircd))
             marionette))

          (test-assert "ngircd listens on TCP port 6667"
            (wait-for-tcp-port 6667 marionette))

          (test-assert "basic irc operations function as expected"
            (marionette-eval
             '(begin
                (use-modules ((gnu build dbus-service) #:select (with-retries))
                             (ice-9 textual-ports))

                (define (write-command command)
                  (call-with-output-file "in"
                    (lambda (port)
                      (display (string-append command "\n") port))))

                (define (grep-output text)
                  (with-retries 5 1     ;retry for 5 seconds
                    (string-contains (call-with-input-file "out" get-string-all)
                                     text)))

                (unless (zero? (system "ii -s localhost -i /tmp &"))
                  (error "error connecting to irc server"))

                (with-retries 5 1
                  (chdir "/tmp/localhost")) ;move to FIFO directory

                (write-command "/join #guix")
                (grep-output "GNU Guix | https://guix.gnu.org")

                (write-command "/oper apteryx 1234")
                (grep-output "+o"))
             marionette))

          (test-end))))

  (gexp->derivation "ngircd-test" test))

(define %test-ngircd
  (system-test
   (name "ngircd")
   (description "Connect to a ngircd IRC server.")
   (value (run-ngircd-test))))


;;;
;;; Pounce.
;;;

;;; Code to generate a self-signed TLS certificate/private key for ngIRCd.
;;; The ngIRCd certificate must be added to pounce's 'trust' file so that it
;;; is trusted.  It is deployed via a one-shot shepherd service required by
;;; ngircd, which avoids having to allow file-like objects in the ngircd-ssl
;;; configuration record (which would be unsafe as the store is public).
(define ngircd-tls-cert-service-type
  (shepherd-service-type
   'ngircd-tls-cert
   (lambda _
     (shepherd-service
      (documentation "Generate TLS certificate/key for ngIRCd")
      (modules (append '((gnu build activation)
                         (srfi srfi-26))
                       %default-modules))
      (provision '(ngircd-tls-cert))
      (start
       (with-imported-modules (source-module-closure
                               '((gnu build activation)))
         #~(lambda _
             (let ((certtool #$(file-append gnutls "/bin/certtool"))
                   (user (getpwnam "ngircd")))
               (mkdir-p/perms "/etc/ngircd" user #o755)
               (call-with-output-file "/tmp/template"
                 (cut format <> "expiration_days = -1~%"))
               ;; XXX: Beware, chdir + invoke do not work together in Shepherd
               ;; services (see bug#77707).
               (invoke certtool "--generate-privkey"
                       "--outfile" "/etc/ngircd/ca-key.pem")
               (invoke certtool "--generate-self-signed"
                       "--load-privkey" "/etc/ngircd/ca-key.pem"
                       "--outfile" "/etc/ngircd/ca-cert.pem"
                       "--template" "/tmp/template")
               (chdir "/etc/ngircd")
               (chown "ca-key.pem" (passwd:uid user) (passwd:gid user))
               (chmod "ca-key.pem" #o400)
               (chown "ca-cert.pem" (passwd:uid user) (passwd:gid user))
               (chmod "ca-cert.pem" #o444)
               (delete-file "/tmp/template")
               #t))))
      (one-shot? #t)))
   #t                                   ;dummy default value
   (description "Generate a self-signed TLS certificate for ngIRCd")))

;;; To generate a VM image to test with, run:
;;; guix system vm -e '(@@ (gnu tests messaging) %pounce-os)' --no-graphic
;;; After login, resize tty to your needs, e.g.: 'stty rows 52 columns 234'
(define %pounce-os
  (operating-system
    (inherit %simple-os)
    (packages
     (append (list ii socat)
             ;; Uncomment for debugging.
             ;; (specifications->packages
             ;;  '("gdb"
             ;;    "gnutls"                ;for gnutls-cli
             ;;    "screen"
             ;;    "strace"
             ;;    "ngircd:debug"
             ;;    "pounce:debug"
             ;;    "libressl:debug"
             ;;    "gnutls:debug"))
             %base-packages))
    (services
     (cons*
      (service dhcp-client-service-type)
      (service ngircd-tls-cert-service-type)
      (service ngircd-service-type
               (ngircd-configuration
                (debug? #t)
                (shepherd-requirement '(user-processes ngircd-tls-cert))
                (ssl (ngircd-ssl
                      (ports (list 6697))
                      (cert-file "/etc/ngircd/ca-cert.pem")
                      (key-file "/etc/ngircd/ca-key.pem")))
                (channels (list (ngircd-channel (name "#irc"))))))
      (service pounce-service-type
               (pounce-configuration
                (host "localhost")      ;connect to ngIRCd server
                ;; Trust the IRC server self-signed certificate.
                (trust "/etc/ngircd/ca-cert.pem")
                (verbose? #t)
                ;; The password below was generated by inputting 1234 at the
                ;; prompt requested by 'pounce -x'.
                (local-pass "\
$6$rviyVy+iFC9vT37o$2RUAhhFzD8gklXRk9X5KuHYtp6APk8nEXf1uroY2/KlgO9nQ0O/Dj05fzJ\
/qNlpJQOijJMOyKm4fXjw.Ck9F91")
                (local-port 7000)       ;listen on port 7000
                (nick "apteryx")
                (join (list "#irc"))))
      %base-services))))

(define (run-pounce-test)
  (define vm
    (virtual-machine
     (operating-system
       (marionette-operating-system
        %pounce-os
        #:imported-modules (source-module-closure
                            '((gnu build dbus-service)
                              (guix build utils)
                              (gnu services herd)))))
     (memory-size 1024)))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "pounce")

          (test-assert "IRC test server listens on TCP port 6697"
            (wait-for-tcp-port 6697 marionette))

          (test-assert "pounce service runs"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'pounce))
             marionette))

          (test-assert "pounce listens on TCP port 7000"
            (wait-for-tcp-port 7000 marionette))

          (test-assert "pounce functions as an irc bouncer"
            (marionette-eval
             '(begin
                (use-modules ((gnu build dbus-service) #:select (with-retries))
                             (guix build utils)
                             (ice-9 textual-ports))

                (define (write-command command)
                  (call-with-output-file "in"
                    (lambda (port)
                      (display (string-append command "\n") port))))

                (define (grep-output text)
                  (with-retries 5 1     ;retry for 5 seconds
                    (string-contains (call-with-input-file "out" get-string-all)
                                     (pk 'output-text: text))))

                (define (connect-to-ngircd)
                  (mkdir-p "/tmp/pounce")
                  (unless (zero? (system "ii -s localhost -i /tmp/ngircd \
-n ayoli &"))
                    (error "error connecting to irc server"))
                  (with-retries 5 1 (file-exists? "/tmp/ngircd/localhost"))
                  (with-directory-excursion "/tmp/ngircd/localhost"
                    (write-command "/join #irc"))
                  (with-retries 5 1
                    (file-exists? "/tmp/ngircd/localhost/#irc")))

                (define (connect-to-pounce)
                  (mkdir-p "/tmp/pounce")
                  ;; Expose a tunnel encrypting communication via TLS to
                  ;; pounce (mandated by pounce but supported by ii).
                  (system "socat UNIX-LISTEN:/tmp/pounce/socket \
OPENSSL:localhost:7000,verify=0 &")
                  (with-retries 5 1 (file-exists? "/tmp/pounce/socket"))
                  (setenv "PASS" "1234")
                  (unless (zero? (system "ii -s localhost -i /tmp/pounce \
-u /tmp/pounce/socket -n apteryx -k PASS &"))
                    (error "error connecting to pounce server"))
                  (with-retries 5 1 (file-exists? "/tmp/pounce/localhost"))
                  (with-directory-excursion "/tmp/pounce/localhost"
                    (write-command "/join #irc"))
                  (with-retries 5 1
                    (file-exists? "/tmp/pounce/localhost/#irc")))

                (connect-to-ngircd)
                (connect-to-pounce)

                ;; Send a message via pounce.
                (with-directory-excursion "/tmp/pounce/localhost/#irc"
                  (write-command "hi!  Does pounce work well as a bouncer?")
                  (write-command "/quit"))

                ;; Someone replied while we were away.
                (with-directory-excursion "/tmp/ngircd/localhost/#irc"
                  (write-command "apteryx: pounce does work well"))

                ;; We reconnect some time later and receive the missed
                ;; message.
                (with-retries 5 1 (not (file-exists? "/tmp/pounce/socket")))
                (connect-to-pounce)
                (with-directory-excursion "/tmp/pounce/localhost/#irc"
                  (grep-output "apteryx: pounce does work well")))
             marionette))
          (test-end))))

  (gexp->derivation "pounce-test" test))

(define %test-pounce
  (system-test
   (name "pounce")
   (description "Connect to a pounce IRC network bouncer.")
   (value (run-pounce-test))))


;;;
;;; Quassel.
;;;

(define (run-quassel-test)
  (define os
    (marionette-operating-system
      (simple-operating-system (service dhcp-client-service-type)
                               (service quassel-service-type))
     #:imported-modules (source-module-closure
                         '((gnu services herd)))))

  (define vm
    (virtual-machine
      (operating-system os)
      (port-forwardings `((4242 . 4242)))))

  (define test
    (with-imported-modules '((gnu build marionette))
      #~(begin
          (use-modules (srfi srfi-64)
                       (gnu build marionette))

          (define marionette
            (make-marionette (list #$vm)))

          (test-runner-current (system-test-runner #$output))
          (test-begin "quassel")

          (test-assert "service started"
            (marionette-eval
             '(begin
                (use-modules (gnu services herd))
                (wait-for-service 'quassel))
             marionette))

          (test-assert "certificate file"
            (marionette-eval
              '(file-exists? "/var/lib/quassel/quasselCert.pem")
              marionette))

          (test-end))))

  (gexp->derivation "quassel-test" test))

(define %test-quassel
  (system-test
   (name "quassel")
   (description "Connect to a quassel IRC server.")
   (value (run-quassel-test))))
