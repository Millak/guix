;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
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

(define-module (tests machine hetzner)
  #:use-module (gnu machine hetzner http)
  #:use-module (gnu machine hetzner)
  #:use-module (gnu machine ssh)
  #:use-module (gnu machine)
  #:use-module (gnu system)
  #:use-module (guix build utils)
  #:use-module (guix records)
  #:use-module (guix ssh)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ssh key)
  #:use-module (ssh session))

;;; Unit and integration tests for the (gnu machine hetzner) module.

;; Integration tests require the GUIX_HETZNER_API_TOKEN environment variable.
;; https://docs.hetzner.com/cloud/api/getting-started/generating-api-token

;; The integration tests sometimes fail due to the Hetzner API not being able
;; to allocate a resource.  Switching to a different location might help.

(define %labels
  '(("guix.gnu.org/test" . "true")))

(define %ssh-key-name
  "guix-hetzner-machine-test-key")

(define %ssh-key-file
  (string-append "/tmp/" %ssh-key-name))

(unless (file-exists? %ssh-key-file)
  (private-key-to-file (make-keypair 'rsa 2048) %ssh-key-file))

(define %when-no-token
  (if (hetzner-api-token (hetzner-api)) 0 1))

(define %arm-machine
  (machine
   (operating-system
     (operating-system
       (inherit %hetzner-os-arm)
       (host-name "guix-deploy-hetzner-test-arm")))
   (environment hetzner-environment-type)
   (configuration (hetzner-configuration
                   (labels %labels)
                   (server-type "cax11")
                   (ssh-key %ssh-key-file)))))

(define %x86-machine
  (machine
   (operating-system
     (operating-system
       (inherit %hetzner-os-x86)
       (host-name "guix-deploy-hetzner-test-x86")))
   (environment hetzner-environment-type)
   (configuration (hetzner-configuration
                   (labels %labels)
                   (server-type "cx22")
                   (ssh-key %ssh-key-file)))))

(define (cleanup machine)
  (let* ((config (machine-configuration machine))
         (api (hetzner-configuration-api config)))
    (for-each (lambda (server)
                (hetzner-api-server-delete api server))
              (hetzner-api-servers
               api #:params `(("label_selector" . "guix.gnu.org/test=true"))))
    (for-each (lambda (ssh-key)
                (hetzner-api-ssh-key-delete api ssh-key))
              (hetzner-api-ssh-keys
               api #:params `(("label_selector" . "guix.gnu.org/test=true"))))
    machine))

(define-syntax-rule (with-cleanup (machine-sym machine-init) body ...)
  (let ((machine-sym (cleanup machine-init)))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (cleanup machine-sym)))))

(define (mock-action command)
  (make-hetzner-action
   command #f
   (localtime (current-time))
   1
   100
   '()
   (localtime (current-time))
   "success"))

(define (mock-location machine)
  (let* ((config (machine-configuration machine))
         (name (hetzner-configuration-location config)))
    (make-hetzner-location
     "Falkenstein" "DE" "Falkenstein DC Park 1"
     1 50.47612 12.370071 name "eu-central")))

(define (mock-server-type machine)
  (let* ((config (machine-configuration machine))
         (name (hetzner-configuration-server-type config)))
    (make-hetzner-server-type
     "x86" 8 "shared" #f  #f (string-upcase name)
     160 106 16 name "local")))

(define (mock-server machine)
  (let* ((config (machine-configuration machine))
         (name (hetzner-configuration-location config)))
    (make-hetzner-server
     1
     (localtime (current-time))
     '()
     (operating-system-host-name (machine-operating-system machine))
     (make-hetzner-public-net
      (make-hetzner-ipv4 #f "server.example.com" 1 "1.2.3.4")
      (make-hetzner-ipv6 #f "server.example.com" 1 "2001:db8::1"))
     #f
     (mock-server-type machine))))

(define (mock-ssh-key machine)
  (let ((config (machine-configuration machine)))
    (hetzner-ssh-key-read-file  (hetzner-configuration-ssh-key config))))

(define (expected-ssh-machine? machine ssh-machine)
  (let ((config (machine-configuration machine))
        (ssh-config (machine-configuration ssh-machine)))
    (and (equal? (hetzner-configuration-authorize? config)
                 (machine-ssh-configuration-authorize? ssh-config))
         (equal? (hetzner-configuration-allow-downgrades? config)
                 (machine-ssh-configuration-allow-downgrades? ssh-config))
         (equal? (hetzner-configuration-build-locally? config)
                 (machine-ssh-configuration-build-locally? ssh-config))
         (equal? (hetzner-server-public-ipv4 (mock-server machine))
                 (machine-ssh-configuration-host-name ssh-config)))))

(define-syntax mock*
  (syntax-rules ()
    ((mock* () body1 body2 ...)
     (let () body1 body2 ...))
    ((mock* ((mod1 sym1 fn1) (mod2 sym2 fn2) ...)
            body1 body2 ...)
     (mock (mod1 sym1 fn1)
           (mock* ((mod2 sym2 fn2) ...)
                  body1) body2 ...))))

(test-begin "machine-hetzner")

;; The following tests deploy real machines using the Hetzner API and shut
;; them down afterwards.

(test-skip %when-no-token)
(test-assert "deploy-arm-machine"
  (with-cleanup (machine %arm-machine)
    (deploy-hetzner machine)))

(test-skip %when-no-token)
(test-assert "deploy-x86-machine"
  (with-cleanup (machine %x86-machine)
    (deploy-hetzner machine)))

;; The following tests simulate a deployment, they mock out the actual calls
;; to the Hetzner API.

;; Note: In order for mocking to work, the Guile compiler should not inline
;; the mocked functions. To prevent this it was necessary to set!
;; hetzner-machine-ssh-run-script in (gnu machine hetzner) like this:

;; (set! hetzner-machine-ssh-run-script hetzner-machine-ssh-run-script)

(test-assert "deploy-machine-mock-with-provisioned-server"
  (let ((machine (machine
                  (operating-system %hetzner-os-x86)
                  (environment hetzner-environment-type)
                  (configuration (hetzner-configuration
                                  (api (hetzner-api (token "mock")))
                                  (ssh-key %ssh-key-file))))))
    (mock* (((gnu machine hetzner http) hetzner-api-locations
             (lambda* (api . options)
               (list (mock-location machine))))
            ((gnu machine hetzner http) hetzner-api-server-types
             (lambda* (api . options)
               (list (mock-server-type machine))))
            ((gnu machine hetzner http) hetzner-api-ssh-keys
             (lambda* (api . options)
               (list (mock-ssh-key machine))))
            ((gnu machine hetzner http) hetzner-api-servers
             (lambda* (api . options)
               (list (mock-server machine))))
            ((gnu machine) deploy-machine
             (lambda* (ssh-machine)
               (expected-ssh-machine? machine ssh-machine))))
           (deploy-hetzner machine))))

(test-assert "deploy-machine-mock-with-unprovisioned-server"
  (let ((machine (machine
                  (operating-system %hetzner-os-x86)
                  (environment hetzner-environment-type)
                  (configuration (hetzner-configuration
                                  (api (hetzner-api (token "mock")))
                                  (ssh-key %ssh-key-file)))))
        (servers '()))
    (mock* (((gnu machine hetzner http) hetzner-api-locations
             (lambda* (api . options)
               (list (mock-location machine))))
            ((gnu machine hetzner http) hetzner-api-server-types
             (lambda* (api . options)
               (list (mock-server-type machine))))
            ((gnu machine hetzner http) hetzner-api-ssh-keys
             (lambda* (api . options)
               (list (mock-ssh-key machine))))
            ((gnu machine hetzner http) hetzner-api-servers
             (lambda* (api . options)
               servers))
            ((gnu machine hetzner http) hetzner-api-server-create
             (lambda* (api name ssh-keys . options)
               (set! servers (list (mock-server machine)))
               (car servers)))
            ((gnu machine hetzner http) hetzner-api-server-enable-rescue-system
             (lambda (api server ssh-keys)
               (mock-action "enable_rescue")))
            ((gnu machine hetzner http) hetzner-api-server-power-on
             (lambda (api server)
               (mock-action "start_server")))
            ((gnu machine hetzner) hetzner-machine-ssh-run-script
             (lambda (ssh-session name content)
               #t))
            ((guix ssh) open-ssh-session
             (lambda* (host . options)
               (make-session #:host host)))
            ((gnu machine hetzner http) hetzner-api-server-reboot
             (lambda (api server)
               (mock-action "reboot_server")))
            ((ssh session) write-known-host!
             (lambda (session)
               #t))
            ((gnu machine) deploy-machine
             (lambda* (ssh-machine)
               (expected-ssh-machine? machine ssh-machine))))
           (deploy-hetzner machine))))

(test-end "machine-hetzner")

;; Local Variables:
;; eval: (put 'with-cleanup 'scheme-indent-function 1)
;; End:
