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

(define-module (tests machine hetzner http)
  #:use-module (debugging assert)
  #:use-module (gnu machine hetzner http)
  #:use-module (guix build utils)
  #:use-module (guix tests)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-64)
  #:use-module (ssh key))

;; Unit and integration tests the (gnu machine hetzner http) module.

;; Integration tests require the GUIX_HETZNER_API_TOKEN environment variable.
;; https://docs.hetzner.com/cloud/api/getting-started/generating-api-token

;; The integration tests sometimes fail due to the Hetzner API not being able
;; to allocate a resource.  Switching to a different location might help.

(define %labels
  '(("guix.gnu.org/test" . "true")))

(define %server-name
  "guix-hetzner-api-test-server")

(define %ssh-key-name
  "guix-hetzner-api-test-key")

(define %ssh-key-file
  (string-append "/tmp/" %ssh-key-name))

(unless (file-exists? %ssh-key-file)
  (private-key-to-file (make-keypair 'rsa 2048) %ssh-key-file))

(define %ssh-key
  (hetzner-ssh-key-read-file %ssh-key-file))

(define %when-no-token
  (if (hetzner-api-token (hetzner-api)) 0 1))

(define action-create-server
  (make-hetzner-action
   "create_server" #f *unspecified* 1896091819 0
   (list (make-hetzner-resource 59570198 "server"))
   #(0 17 11 2 1 125 0 32 -1 0 #f) "running"))

(define action-create-server-alist
  '(("command" . "create_server")
    ("error" . null)
    ("finished" . null)
    ("id" . 1896091819)
    ("progress" . 0)
    ("resources" . #((("type" . "server") ("id" . 59570198))))
    ("started" . "2025-02-02T11:17:00+00:00")
    ("status" . "running")))

(define action-delete-server
  (make-hetzner-action
   "delete_server" #f *unspecified* 1896091928 0
   (list (make-hetzner-resource 59570198 "server"))
   #(10 17 11 2 1 125 0 32 -1 0 #f) "running"))

(define action-delete-server-alist
  '(("command" . "delete_server")
    ("error" . null)
    ("finished" . null)
    ("id" . 1896091928)
    ("progress" . 0)
    ("resources" . #((("type" . "server") ("id" . 59570198))))
    ("started" . "2025-02-02T11:17:10+00:00")
    ("status" . "running")))

(define action-enable-rescue
  (make-hetzner-action
   "enable_rescue" #f  *unspecified* 1896091721 0
   (list (make-hetzner-resource 59570198 "server"))
   #(10 17 11 2 1 125 0 32 -1 0 #f) "success"))

(define action-enable-rescue-alist
  '(("command" . "enable_rescue")
    ("error" . null)
    ("finished" . null)
    ("id" . 1896091721)
    ("progress" . 0)
    ("resources" . #((("type" . "server") ("id" . 59570198))))
    ("started" . "2025-02-02T11:17:10+00:00")
    ("status" . "running")))

(define action-power-off
  (make-hetzner-action
   "stop_server" #f  *unspecified* 1896091721 0
   (list (make-hetzner-resource 59570198 "server"))
   #(10 17 11 2 1 125 0 32 -1 0 #f) "success"))

(define action-power-off-alist
  '(("command" . "stop_server")
    ("error" . null)
    ("finished" . null)
    ("id" . 1896091721)
    ("progress" . 0)
    ("resources" . #((("type" . "server") ("id" . 59570198))))
    ("started" . "2025-02-02T11:17:10+00:00")
    ("status" . "running")))

(define action-power-on
  (make-hetzner-action
   "start_server" #f  *unspecified* 1896091721 0
   (list (make-hetzner-resource 59570198 "server"))
   #(10 17 11 2 1 125 0 32 -1 0 #f) "success"))

(define action-power-on-alist
  '(("command" . "start_server")
    ("error" . null)
    ("finished" . null)
    ("id" . 1896091721)
    ("progress" . 0)
    ("resources" . #((("type" . "server") ("id" . 59570198))))
    ("started" . "2025-02-02T11:17:10+00:00")
    ("status" . "running")))

(define action-reboot
  (make-hetzner-action
   "reboot_server" #f  *unspecified* 1896091721 0
   (list (make-hetzner-resource 59570198 "server"))
   #(10 17 11 2 1 125 0 32 -1 0 #f) "success"))

(define action-reboot-alist
  '(("command" . "reboot_server")
    ("error" . null)
    ("finished" . null)
    ("id" . 1896091721)
    ("progress" . 0)
    ("resources" . #((("type" . "server") ("id" . 59570198))))
    ("started" . "2025-02-02T11:17:10+00:00")
    ("status" . "running")))

(define meta-page-alist
  '("pagination"
    ("last_page" . 1)
    ("next_page" . null)
    ("page" . 1)
    ("per_page" . 25)
    ("previous_page" . null)
    ("total_entries" . 1)))

(define location-falkenstein
  (make-hetzner-location
   "Falkenstein" "DE" "Falkenstein DC Park 1"
   1 50.47612 12.370071 "fsn1" "eu-central"))

(define location-falkenstein-alist
  `(("city" . "Falkenstein")
    ("country" . "DE")
    ("description" . "Falkenstein DC Park 1")
    ("id" . 1)
    ("latitude" . 50.47612)
    ("longitude" . 12.370071)
    ("name" . "fsn1")
    ("network_zone" . "eu-central")))

(define server-type-cpx-11
  (make-hetzner-server-type
   "x86" 2 "shared" #f *unspecified*
   "CPX 11" 40 22 2 "cpx11" "local"))

(define server-type-cpx-11-alist
  `(("architecture" . "x86")
    ("cores" . 2)
    ("cpu_type" . "shared")
    ("deprecated" . #f)
    ("deprecation" . null)
    ("description" . "CPX 11")
    ("disk" . 40)
    ("id" . 22)
    ("memory" . 2)
    ("name" . "cpx11")
    ("storage_type" . "local")))

(define server-x86
  (make-hetzner-server
   "2024-12-30T16:38:11+00:00"
   59570198
   '()
   "guix-x86"
   (make-hetzner-public-net
    (make-hetzner-ipv4 #f "static.218.128.13.49.clients.your-server.de" 78014457 "49.13.128.218")
    (make-hetzner-ipv6 #f '() 78014458 "2a01:4f8:c17:293e::/64"))
   #f
   server-type-cpx-11))

(define server-x86-alist
  `(("backup_window" . null)
    ("created" . "2024-12-30T16:38:11+00:00")
    ("id" . 59570198)
    ("included_traffic" . 21990232555520)
    ("ingoing_traffic" . 124530000)
    ("iso" . null)
    ("labels")
    ("load_balancers" . #())
    ("locked" . #f)
    ("name" . "guix-x86")
    ("outgoing_traffic" . 1391250000)
    ("placement_group" . null)
    ("primary_disk_size" . 320)
    ("private_net" . #())
    ("protection" ("rebuild" . #f) ("delete" . #f))
    ("public_net"
     ("firewalls" . #())
     ("floating_ips" . #())
     ("ipv6"
      ("id" . 78014458)
      ("dns_ptr" . #())
      ("blocked" . #f)
      ("ip" . "2a01:4f8:c17:293e::/64"))
     ("ipv4"
      ("id" . 78014457)
      ("dns_ptr" . "static.218.128.13.49.clients.your-server.de")
      ("blocked" . #f)
      ("ip" . "49.13.128.218")))
    ("rescue_enabled" . #f)
    ("server_type" ,@server-type-cpx-11-alist)
    ("status" . "running")
    ("volumes" . #())))

(define primary-ip
  (make-hetzner-primary-ip
   #(55 2 19 28 9 123 6 300 -1 0 #f)
   42
   "131.232.99.1"
   '()
   "static-ip"
   "ipv4"))

(define primary-ip-alist
  `(("created" . "2023-10-28T19:02:55+00:00")
    ("id" . 42)
    ("labels")
    ("name" . "static-ip")
    ("blocked" . #f)
    ("ip" . "131.232.99.1")
    ("datacenter")
    ("dns_ptr")
    ("protection" . (("delete" . #f)))
    ("type" . "ipv4")
    ("auto_delete" . #t)
    ("assignee_type" . "server")
    ("assignee_id" . 17)))

(define ssh-key-root
  (make-hetzner-ssh-key
   #(55 2 19 28 9 123 6 300 -1 0 #f)
   "8c:25:09:8f:37:0f:d8:f0:99:4e:ab:c7:5c:1b:c6:53"
   16510983 '() "root@example.com"
   "ssh-ed25519 ABCAC3NzaC1lZDI1NTE5AAAAIBT3lLYPfOZV9NNrNk0jGCufWmXbFSz+ORxowJdHoSIM"))

(define ssh-key-root-alist
  `(("created" . "2023-10-28T19:02:55+00:00")
    ("fingerprint" . "8c:25:09:8f:37:0f:d8:f0:99:4e:ab:c7:5c:1b:c6:53")
    ("id" . 16510983)
    ("labels")
    ("name" . "root@example.com")
    ("public_key" . "ssh-ed25519 ABCAC3NzaC1lZDI1NTE5AAAAIBT3lLYPfOZV9NNrNk0jGCufWmXbFSz+ORxowJdHoSIM")))

(define* (create-ssh-key api ssh-key #:key (labels %labels))
  (hetzner-api-ssh-key-create
   api
   (hetzner-ssh-key-name ssh-key)
   (hetzner-ssh-key-public-key ssh-key)
   #:labels labels))

(define* (create-server api ssh-key #:key (labels %labels))
  (hetzner-api-server-create api %server-name (list ssh-key)
                             #:labels labels
                             #:server-type "cpx31"))

(define (cleanup api)
  (for-each (lambda (server)
              (hetzner-api-server-delete api server))
            (hetzner-api-servers
             api #:params `(("label_selector" . "guix.gnu.org/test=true"))))
  (for-each (lambda (ssh-key)
              (hetzner-api-ssh-key-delete api ssh-key))
            (hetzner-api-ssh-keys
             api #:params `(("label_selector" . "guix.gnu.org/test=true"))))
  api)

(define-syntax-rule (with-cleanup-api (api-sym api-init) body ...)
  (let ((api-sym (cleanup api-init)))
    (dynamic-wind
      (const #t)
      (lambda ()
        body ...)
      (lambda ()
        (cleanup api-sym)))))

(test-begin "machine-hetzner-api")

;; Unit Tests

(test-equal "hetzner-api-actions-unit"
  (list action-create-server action-delete-server)
  (let ((actions (list action-create-server-alist action-delete-server-alist)))
    (mock ((gnu machine hetzner http) hetzner-api-request-send
           (lambda* (request #:key expected)
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (assert (equal? "https://api.hetzner.cloud/v1/actions"
                             (hetzner-api-request-url request)))
             (assert (unspecified? (hetzner-api-request-body request)))
             (assert (equal? `(("page" . 1)
                               ("id" . ,(string-join
                                         (map (lambda (action)
                                                (number->string (assoc-ref action "id")))
                                              actions)
                                         ",")))
                             (hetzner-api-request-params request)))
             (hetzner-api-response
              (body `(("meta" . ,meta-page-alist)
                      ("actions" . #(,action-create-server-alist ,action-delete-server-alist)))))))
          (hetzner-api-actions (hetzner-api)
                               (map (lambda (action)
                                      (assoc-ref action "id"))
                                    actions)))))

(test-equal "hetzner-api-locations-unit"
  (list location-falkenstein)
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (assert (equal? 'GET (hetzner-api-request-method request)))
           (assert (equal? "https://api.hetzner.cloud/v1/locations"
                           (hetzner-api-request-url request)))
           (assert (unspecified? (hetzner-api-request-body request)))
           (assert (equal? '(("page" . 1)) (hetzner-api-request-params request)))
           (hetzner-api-response
            (body `(("meta" . ,meta-page-alist)
                    ("locations" . #(,location-falkenstein-alist)))))))
        (hetzner-api-locations (hetzner-api))))

(test-equal "hetzner-api-server-types-unit"
  (list server-type-cpx-11)
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (assert (equal? 'GET (hetzner-api-request-method request)))
           (assert (equal? "https://api.hetzner.cloud/v1/server_types"
                           (hetzner-api-request-url request)))
           (assert (unspecified? (hetzner-api-request-body request)))
           (assert (equal? '(("page" . 1)) (hetzner-api-request-params request)))
           (hetzner-api-response
            (body `(("meta" . ,meta-page-alist)
                    ("server_types" . #(,server-type-cpx-11-alist)))))))
        (hetzner-api-server-types (hetzner-api))))

(test-equal "hetzner-api-server-create-unit"
  server-x86
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (cond
            ((equal? "https://api.hetzner.cloud/v1/servers"
                     (hetzner-api-request-url request))
             (assert (equal? 'POST (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("action" . ,action-create-server-alist)
                      ("server" . ,server-x86-alist)))))
            ((equal? "https://api.hetzner.cloud/v1/actions"
                     (hetzner-api-request-url request))
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("actions" . ,(vector (cons `("status" . "success")
                                                  action-create-server-alist)))
                      ("meta" . ,meta-page-alist))))))))
        (hetzner-api-server-create (hetzner-api) %server-name (list ssh-key-root))))

(test-equal "hetzner-api-server-delete-unit"
  (make-hetzner-action
   "delete_server" #f *unspecified* 1896091928 0
   (list (make-hetzner-resource 59570198 "server"))
   #(10 17 11 2 1 125 0 32 -1 0 #f) "success")
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (cond
            ((equal? "https://api.hetzner.cloud/v1/servers/59570198"
                     (hetzner-api-request-url request))
             (assert (equal? 'DELETE (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("action" . ,action-delete-server-alist)))))
            ((equal? "https://api.hetzner.cloud/v1/actions"
                     (hetzner-api-request-url request))
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("actions" . ,(vector (cons `("status" . "success")
                                                  action-delete-server-alist)))
                      ("meta" . ,meta-page-alist))))))))
        (hetzner-api-server-delete (hetzner-api) server-x86)))

(test-equal "hetzner-api-server-enable-rescue-system-unit"
  action-enable-rescue
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (cond
            ((equal? "https://api.hetzner.cloud/v1/servers/59570198/actions/enable_rescue"
                     (hetzner-api-request-url request))
             (assert (equal? 'POST (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("action" . ,action-enable-rescue-alist)))))
            ((equal? "https://api.hetzner.cloud/v1/actions"
                     (hetzner-api-request-url request))
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("actions" . ,(vector (cons `("status" . "success")
                                                  action-enable-rescue-alist)))
                      ("meta" . ,meta-page-alist))))))))
        (hetzner-api-server-enable-rescue-system (hetzner-api) server-x86 (list ssh-key-root))))

(test-equal "hetzner-api-server-power-on-unit"
  action-power-on
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (cond
            ((equal? "https://api.hetzner.cloud/v1/servers/59570198/actions/poweron"
                     (hetzner-api-request-url request))
             (assert (equal? 'POST (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("action" . ,action-power-on-alist)))))
            ((equal? "https://api.hetzner.cloud/v1/actions"
                     (hetzner-api-request-url request))
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("actions" . ,(vector (cons `("status" . "success")
                                                  action-power-on-alist)))
                      ("meta" . ,meta-page-alist))))))))
        (hetzner-api-server-power-on (hetzner-api) server-x86)))

(test-equal "hetzner-api-server-power-off-unit"
  action-power-off
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (cond
            ((equal? "https://api.hetzner.cloud/v1/servers/59570198/actions/poweroff"
                     (hetzner-api-request-url request))
             (assert (equal? 'POST (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("action" . ,action-power-off-alist)))))
            ((equal? "https://api.hetzner.cloud/v1/actions"
                     (hetzner-api-request-url request))
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("actions" . ,(vector (cons `("status" . "success")
                                                  action-power-off-alist)))
                      ("meta" . ,meta-page-alist))))))))
        (hetzner-api-server-power-off (hetzner-api) server-x86)))

(test-equal "hetzner-api-server-reboot-unit"
  action-reboot
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (cond
            ((equal? "https://api.hetzner.cloud/v1/servers/59570198/actions/reboot"
                     (hetzner-api-request-url request))
             (assert (equal? 'POST (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("action" . ,action-reboot-alist)))))
            ((equal? "https://api.hetzner.cloud/v1/actions"
                     (hetzner-api-request-url request))
             (assert (equal? 'GET (hetzner-api-request-method request)))
             (hetzner-api-response
              (body `(("actions" . ,(vector (cons `("status" . "success")
                                                  action-reboot-alist)))
                      ("meta" . ,meta-page-alist))))))))
        (hetzner-api-server-reboot (hetzner-api) server-x86)))

(test-equal "hetzner-api-servers-unit"
  (list server-x86)
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (hetzner-api-response
            (body `(("meta" . ,meta-page-alist)
                    ("servers" . #(,server-x86-alist)))))))
        (hetzner-api-servers (hetzner-api))))

(test-equal "hetzner-api-ssh-key-create-unit"
  ssh-key-root
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (assert (equal? 'POST (hetzner-api-request-method request)))
           (assert (equal? "https://api.hetzner.cloud/v1/ssh_keys"
                           (hetzner-api-request-url request)))
           (assert (equal? `(("name" . "guix-hetzner-api-test-key")
                             ("public_key" . "ssh-ed25519 ABCAC3NzaC1lZDI1NTE5AAAAIBT3lLYPfOZV9NNrNk0jGCufWmXbFSz+ORxowJdHoSIM")
                             ("labels" . (("a" . "1"))))
                           (hetzner-api-request-body request)))
           (assert (equal? `() (hetzner-api-request-params request)))
           (hetzner-api-response
            (body `(("ssh_key" . ,ssh-key-root-alist))))))
        (hetzner-api-ssh-key-create
         (hetzner-api)
         "guix-hetzner-api-test-key"
         "ssh-ed25519 ABCAC3NzaC1lZDI1NTE5AAAAIBT3lLYPfOZV9NNrNk0jGCufWmXbFSz+ORxowJdHoSIM"
         #:labels '(("a" . "1")))))

(test-assert "hetzner-api-ssh-key-delete-unit"
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (assert (equal? "https://api.hetzner.cloud/v1/ssh_keys/16510983"
                           (hetzner-api-request-url request)))
           (assert (equal? 'DELETE (hetzner-api-request-method request)))
           (hetzner-api-response)))
        (hetzner-api-ssh-key-delete (hetzner-api) ssh-key-root)))

(test-equal "hetzner-api-ssh-keys-unit"
  (list ssh-key-root)
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (assert (equal? 'GET (hetzner-api-request-method request)))
           (assert (equal? "https://api.hetzner.cloud/v1/ssh_keys"
                           (hetzner-api-request-url request)))
           (assert (unspecified? (hetzner-api-request-body request)))
           (assert (equal? '(("page" . 1)) (hetzner-api-request-params request)))
           (hetzner-api-response
            (body `(("meta" . ,meta-page-alist)
                    ("ssh_keys" . #(,ssh-key-root-alist)))))))
        (hetzner-api-ssh-keys (hetzner-api))))

(test-equal "hetzner-api-primary-ips-unit"
  (list primary-ip)
  (mock ((gnu machine hetzner http) hetzner-api-request-send
         (lambda* (request #:key expected)
           (assert (equal? 'GET (hetzner-api-request-method request)))
           (assert (equal? "https://api.hetzner.cloud/v1/primary_ips"
                           (hetzner-api-request-url request)))
           (assert (unspecified? (hetzner-api-request-body request)))
           (assert (equal? '(("page" . 1)) (hetzner-api-request-params request)))
           (hetzner-api-response
            (body `(("meta" . ,meta-page-alist)
                    ("primary_ips" . #(,primary-ip-alist)))))))
        (hetzner-api-primary-ips (hetzner-api))))

;; Integration tests

(test-skip %when-no-token)
(test-assert "hetzner-api-actions-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key))
           (action (hetzner-api-server-enable-rescue-system api server (list ssh-key))))
      (member action (hetzner-api-actions api (list (hetzner-action-id action)))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-locations-integration"
  (let ((locations (hetzner-api-locations (hetzner-api))))
    (and (> (length locations) 0)
         (every hetzner-location? locations))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-types-integration"
  (let ((server-types (hetzner-api-server-types (hetzner-api))))
    (and (> (length server-types) 0)
         (every hetzner-server-type? server-types))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-create-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key)))
      (and (hetzner-server? server)
           (equal? %server-name (hetzner-server-name server))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-delete-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key))
           (action (hetzner-api-server-delete api server)))
      (and (hetzner-action? action)
           (equal? "delete_server"
                   (hetzner-action-command action))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-enable-rescue-system-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key))
           (action (hetzner-api-server-enable-rescue-system api server (list ssh-key))))
      (and (hetzner-action? action)
           (equal? "enable_rescue"
                   (hetzner-action-command action))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-power-on-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key))
           (action (hetzner-api-server-power-on api server)))
      (and (hetzner-action? action)
           (equal? "start_server"
                   (hetzner-action-command action))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-power-off-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key))
           (action (hetzner-api-server-power-off api server)))
      (and (hetzner-action? action)
           (equal? "stop_server"
                   (hetzner-action-command action))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-server-reboot-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key))
           (action (hetzner-api-server-reboot api server)))
      (and (hetzner-action? action)
           (equal? "reboot_server"
                   (hetzner-action-command action))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-servers-integration"
  (with-cleanup-api (api (hetzner-api))
    (let* ((ssh-key (create-ssh-key api %ssh-key))
           (server (create-server api ssh-key)))
      (member server (hetzner-api-servers api)))))

(test-skip %when-no-token)
(test-assert "hetzner-api-ssh-key-create-integration"
  (with-cleanup-api (api (hetzner-api))
    (let ((ssh-key (create-ssh-key api %ssh-key)))
      (and (hetzner-ssh-key? ssh-key)
           (equal? (hetzner-ssh-key-fingerprint %ssh-key)
                   (hetzner-ssh-key-fingerprint ssh-key))
           (equal? (hetzner-ssh-key-name %ssh-key)
                   (hetzner-ssh-key-name ssh-key))
           (equal? (hetzner-ssh-key-public-key %ssh-key)
                   (hetzner-ssh-key-public-key ssh-key))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-ssh-key-delete-integration"
  (with-cleanup-api (api (hetzner-api))
    (let ((ssh-key (create-ssh-key api %ssh-key)))
      (and (equal? #t (hetzner-api-ssh-key-delete api ssh-key))
           (not (member ssh-key (hetzner-api-ssh-keys api)))))))

(test-skip %when-no-token)
(test-assert "hetzner-api-ssh-keys-integration"
  (with-cleanup-api (api (hetzner-api))
    (let ((ssh-key (create-ssh-key api %ssh-key)))
      (member ssh-key (hetzner-api-ssh-keys api)))))

(test-end "machine-hetzner-api")

;; Local Variables:
;; eval: (put 'with-cleanup-api 'scheme-indent-function 1)
;; End:
