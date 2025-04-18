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

(define-module (gnu machine hetzner http)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix records)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (ssh key)
  #:use-module (web client)
  #:use-module (web request)
  #:use-module (web response)
  #:use-module (web uri)
  #:export (%hetzner-default-api-token
            %hetzner-default-server-image
            %hetzner-default-server-location
            %hetzner-default-server-type
            hetzner-action
            hetzner-action-command
            hetzner-action-error
            hetzner-action-finished
            hetzner-action-id
            hetzner-action-progress
            hetzner-action-resources
            hetzner-action-started
            hetzner-action-status
            hetzner-action?
            hetzner-api
            hetzner-api-action-wait
            hetzner-api-actions
            hetzner-api-create-ssh-key
            hetzner-api-locations
            hetzner-api-primary-ips
            hetzner-api-request-body
            hetzner-api-request-headers
            hetzner-api-request-method
            hetzner-api-request-params
            hetzner-api-request-send
            hetzner-api-request-url
            hetzner-api-request?
            hetzner-api-response
            hetzner-api-response-body
            hetzner-api-response-headers
            hetzner-api-response-status
            hetzner-api-response?
            hetzner-api-server-create
            hetzner-api-server-delete
            hetzner-api-server-enable-rescue-system
            hetzner-api-server-power-off
            hetzner-api-server-power-on
            hetzner-api-server-reboot
            hetzner-api-server-types
            hetzner-api-servers
            hetzner-api-ssh-key-create
            hetzner-api-ssh-key-delete
            hetzner-api-ssh-keys
            hetzner-api-token
            hetzner-api?
            hetzner-error-code
            hetzner-error-message
            hetzner-error?
            hetzner-ipv4-blocked?
            hetzner-ipv4-dns-ptr
            hetzner-ipv4-id
            hetzner-ipv4-ip
            hetzner-ipv4?
            hetzner-ipv6-blocked?
            hetzner-ipv6-dns-ptr
            hetzner-ipv6-id
            hetzner-ipv6-ip
            hetzner-ipv6?
            hetzner-location
            hetzner-location-city
            hetzner-location-country
            hetzner-location-description
            hetzner-location-id
            hetzner-location-latitude
            hetzner-location-longitude
            hetzner-location-name
            hetzner-location-network-zone
            hetzner-location?
            hetzner-primary-ip
            hetzner-primary-ip-created
            hetzner-primary-ip-id
            hetzner-primary-ip-ip
            hetzner-primary-ip-labels
            hetzner-primary-ip-name
            hetzner-primary-ip-type
            hetzner-public-net
            hetzner-public-net-ipv4
            hetzner-public-net-ipv6
            hetzner-resource
            hetzner-resource-id
            hetzner-resource-type
            hetzner-resource?
            hetzner-server-architecture
            hetzner-server-created
            hetzner-server-id
            hetzner-server-labels
            hetzner-server-name
            hetzner-server-public-ipv4
            hetzner-server-public-net
            hetzner-server-rescue-enabled?
            hetzner-server-system
            hetzner-server-type
            hetzner-server-type-architecture
            hetzner-server-type-cores
            hetzner-server-type-cpu-type
            hetzner-server-type-deprecated
            hetzner-server-type-deprecation
            hetzner-server-type-description
            hetzner-server-type-disk
            hetzner-server-type-id
            hetzner-server-type-memory
            hetzner-server-type-name
            hetzner-server-type-storage-type
            hetzner-server-type?
            hetzner-server?
            hetzner-ssh-key-created
            hetzner-ssh-key-fingerprint
            hetzner-ssh-key-id
            hetzner-ssh-key-labels
            hetzner-ssh-key-name
            hetzner-ssh-key-public-key
            hetzner-ssh-key-read-file
            hetzner-ssh-key?
            make-hetzner-action
            make-hetzner-error
            make-hetzner-ipv4
            make-hetzner-ipv6
            make-hetzner-location
            make-hetzner-public-net
            make-hetzner-primary-ip
            make-hetzner-resource
            make-hetzner-server
            make-hetzner-server-type
            make-hetzner-ssh-key))

;;; Commentary:
;;;
;;; This module implements a lower-level interface for interacting with the
;;; Hetzner Cloud API https://docs.hetzner.cloud.
;;;

(define %hetzner-default-api-token
  (make-parameter (getenv "GUIX_HETZNER_API_TOKEN")))

;; Ideally this would be a Guix image. Maybe one day.
(define %hetzner-default-server-image "debian-11")

;; Falkenstein, Germany
(define %hetzner-default-server-location "fsn1")

;; x86, 8 VCPUs, 16 GB mem, 160 GB disk
(define %hetzner-default-server-type "cx42")


;;;
;;; Helper functions.
;;;

(define (format-query-param param)
  "Format the query PARAM as a string."
  (string-append (uri-encode (format #f "~a" (car param))) "="
                 (uri-encode (format #f "~a" (cdr param)))))

(define (format-query-params params)
  "Format the query PARAMS as a string."
  (if (> (length params) 0)
      (string-append
       "?"
       (string-join
        (map format-query-param params)
        "&"))
      ""))

(define (json->maybe-hetzner-error json)
  (and (list? json) (json->hetzner-error json)))

(define (string->time s)
  (when (string? s) (car (strptime "%FT%T%z" s))))

(define (json->hetzner-dnses vector)
  (map json->hetzner-dns (vector->list vector)))

(define (json->hetzner-resources vector)
  (map json->hetzner-resource (vector->list vector)))


;;;
;;; Domain models.
;;;

(define-json-mapping <hetzner-action>
  make-hetzner-action hetzner-action? json->hetzner-action
  (command hetzner-action-command) ; string
  (error hetzner-action-error "error"
         json->maybe-hetzner-error) ; <hetzner-error> | #f
  (finished hetzner-action-finished "finished" string->time) ; time
  (id hetzner-action-id) ; integer
  (progress hetzner-action-progress) ; integer
  (resources hetzner-action-resources "resources"
             json->hetzner-resources) ; list of <hetzner-resource>
  (started hetzner-action-started "started" string->time) ; time
  (status hetzner-action-status))

(define-json-mapping <hetzner-deprecation>
  make-hetzner-deprecation hetzner-deprecation? json->hetzner-deprecation
  (announced hetzner-deprecation-announced) ; string
  (unavailable-after hetzner-deprecation-unavailable-after
                     "unavailable_after")) ; string

(define-json-mapping <hetzner-dns>
  make-hetzner-dns hetzner-dns? json->hetzner-dns
  (ip hetzner-dns-ip) ; string
  (ptr hetzner-dns-ptr "dns_ptr")) ; string

(define-json-mapping <hetzner-error>
  make-hetzner-error hetzner-error? json->hetzner-error
  (code hetzner-error-code) ; string
  (message hetzner-error-message)) ; <string>

(define-json-mapping <hetzner-ipv4>
  make-hetzner-ipv4 hetzner-ipv4? json->hetzner-ipv4
  (blocked? hetzner-ipv4-blocked? "blocked") ; boolean
  (dns-ptr hetzner-ipv4-dns-ptr "dns_ptr") ; string
  (id hetzner-ipv4-id) ; integer
  (ip hetzner-ipv4-ip)) ; string

(define-json-mapping <hetzner-ipv6>
  make-hetzner-ipv6 hetzner-ipv6? json->hetzner-ipv6
  (blocked? hetzner-ipv6-blocked? "blocked") ; boolean
  (dns-ptr hetzner-ipv6-dns-ptr "dns_ptr"
           json->hetzner-dnses) ; list of <hetzner-dns>
  (id hetzner-ipv6-id) ; integer
  (ip hetzner-ipv6-ip)) ; string

(define-json-mapping <hetzner-location>
  make-hetzner-location hetzner-location? json->hetzner-location
  (city hetzner-location-city) ; string
  (country hetzner-location-country) ; string
  (description hetzner-location-description) ; string
  (id hetzner-location-id) ; integer
  (latitude hetzner-location-latitude) ; decimal
  (longitude hetzner-location-longitude) ; decimal
  (name hetzner-location-name) ; string
  (network-zone hetzner-location-network-zone "network_zone"))

(define-json-mapping <hetzner-public-net>
  make-hetzner-public-net hetzner-public-net? json->hetzner-public-net
  (ipv4 hetzner-public-net-ipv4 "ipv4" json->hetzner-ipv4) ; <hetzner-ipv4>
  (ipv6 hetzner-public-net-ipv6 "ipv6" json->hetzner-ipv6)) ; <hetzner-ipv6>

(define-json-mapping <hetzner-resource>
  make-hetzner-resource hetzner-resource? json->hetzner-resource
  (id hetzner-resource-id) ; integer
  (type hetzner-resource-type)) ; string

(define-json-mapping <hetzner-server>
  make-hetzner-server hetzner-server? json->hetzner-server
  (created hetzner-server-created) ; time
  (id hetzner-server-id) ; integer
  (labels hetzner-server-labels) ; alist of string/string
  (name hetzner-server-name) ; string
  (public-net hetzner-server-public-net "public_net"
              json->hetzner-public-net) ; <hetzner-public-net>
  (rescue-enabled? hetzner-server-rescue-enabled? "rescue_enabled") ; boolean
  (server-type hetzner-server-type "server_type"
               json->hetzner-server-type)) ; <hetzner-server-type>

(define-json-mapping <hetzner-server-type>
  make-hetzner-server-type hetzner-server-type? json->hetzner-server-type
  (architecture hetzner-server-type-architecture) ; string
  (cores hetzner-server-type-cores) ; integer
  (cpu-type hetzner-server-type-cpu-type "cpu_type") ; string
  (deprecated hetzner-server-type-deprecated) ; boolean
  (deprecation hetzner-server-type-deprecation
               json->hetzner-deprecation) ; <hetzner-deprecation>
  (description hetzner-server-type-description) ; string
  (disk hetzner-server-type-disk) ; integer
  (id hetzner-server-type-id) ; integer
  (memory hetzner-server-type-memory) ; integer
  (name hetzner-server-type-name) ; string
  (storage-type hetzner-server-type-storage-type "storage_type")) ; string

;; Reserved IP address. https://docs.hetzner.cloud/#primary-ips
(define-json-mapping <hetzner-primary-ip>
  make-hetzner-primary-ip hetzner-primary-ip? json->hetzner-primary-ip
  (created hetzner-primary-ip-created "created" string->time) ; time
  (id hetzner-primary-ip-id) ; integer
  (ip hetzner-primary-ip-ip) ; string
  (labels hetzner-primary-ip-labels) ; alist of string/string
  (name hetzner-primary-ip-name) ; string
  (type hetzner-primary-ip-type))  ; string

(define-json-mapping <hetzner-ssh-key>
  make-hetzner-ssh-key hetzner-ssh-key? json->hetzner-ssh-key
  (created hetzner-ssh-key-created "created" string->time) ; time
  (fingerprint hetzner-ssh-key-fingerprint) ; string
  (id hetzner-ssh-key-id) ; integer
  (labels hetzner-ssh-key-labels) ; alist of string/string
  (name hetzner-ssh-key-name) ; string
  (public_key hetzner-ssh-key-public-key "public_key")) ; string

(define (hetzner-server-architecture server)
  "Return the architecture of the Hetzner SERVER."
  (hetzner-server-type-architecture (hetzner-server-type server)))

(define* (hetzner-server-path server #:optional (path ""))
  "Return the PATH of the Hetzner SERVER."
  (format #f "/servers/~a~a" (hetzner-server-id server) path))

(define (hetzner-server-public-ipv4 server)
  "Return the public IPv4 address of the SERVER."
  (and-let* ((public-net (hetzner-server-public-net server))
             (ipv4 (hetzner-public-net-ipv4 public-net)))
    (hetzner-ipv4-ip ipv4)))

(define (hetzner-server-system server)
  "Return the Guix system architecture of the Hetzner SERVER."
  (match (hetzner-server-architecture server)
    ("arm" "aarch64-linux")
    ("x86" "x86_64-linux")))

(define* (hetzner-ssh-key-path ssh-key #:optional (path ""))
  "Return the PATH of the Hetzner SSH-KEY."
  (format #f "/ssh_keys/~a~a" (hetzner-ssh-key-id ssh-key) path))

(define (hetzner-ssh-key-read-file file)
  "Read the SSH private key from FILE and return a Hetzner SSH key."
  (let* ((privkey (private-key-from-file file))
         (pubkey (private-key->public-key privkey))
         (hash (get-public-key-hash pubkey 'md5))
         (fingerprint (bytevector->hex-string hash))
         (public-key (format #f "ssh-~a ~a" (get-key-type pubkey)
                             (public-key->string pubkey))))
    (make-hetzner-ssh-key #f fingerprint #f '() (basename file) public-key)))


;;;
;;; Hetzner API response.
;;;

(define-record-type* <hetzner-api-response>
  hetzner-api-response make-hetzner-api-response hetzner-api-response?
  (body hetzner-api-response-body (default *unspecified*))
  (headers hetzner-api-response-headers (default '()))
  (status hetzner-api-response-status (default 200)))

(define (hetzner-api-response-meta response)
  "Return the meta information of the Hetzner API response."
  (assoc-ref (hetzner-api-response-body response) "meta"))

(define (hetzner-api-response-pagination response)
  "Return the meta information of the Hetzner API response."
  (assoc-ref (hetzner-api-response-meta response) "pagination"))

(define (hetzner-api-response-pagination-combine resource responses)
  "Combine multiple Hetzner API pagination responses into a single response."
  (if (positive? (length responses))
      (let* ((response (car responses))
             (pagination (hetzner-api-response-pagination response))
             (total-entries (assoc-ref pagination "total_entries")))
        (hetzner-api-response
         (inherit response)
         (body `(("meta"
                  ("pagination"
                   ("last_page" . 1)
                   ("next_page" . null)
                   ("page" . 1)
                   ("per_page" . ,total-entries)
                   ("previous_page" . null)
                   ("total_entries" . ,total-entries)))
                 (,resource . ,(append-map
                                (lambda (body)
                                  (vector->list (assoc-ref body resource)))
                                (map hetzner-api-response-body responses)))))))
      (raise-exception
       (formatted-message
        (G_ "expected a list of Hetzner API responses")))))

(define (hetzner-api-body-action body)
  "Return the Hetzner API action from BODY."
  (let ((json (assoc-ref body "action")))
    (and json (json->hetzner-action json))))

(define (hetzner-api-response-read port)
  "Read the Hetzner API response from PORT."
  (let* ((response (read-response port))
         (body (read-response-body response)))
    (hetzner-api-response
     (body (and body (json-string->scm (utf8->string body))))
     (headers (response-headers response))
     (status (response-code response)))))

(define (hetzner-api-response-validate-status response expected)
  "Raise an error if the HTTP status code of RESPONSE is not in EXPECTED."
  (when (not (member (hetzner-api-response-status response) expected))
    (raise-exception
     (formatted-message
      (G_ "unexpected HTTP status code: ~a, expected: ~a~%~a")
      (hetzner-api-response-status response)
      expected
      (with-output-to-string
        (lambda ()
          (pretty-print (hetzner-api-response-body response))))))))


;;;
;;; Hetzner API request.
;;;

(define-record-type* <hetzner-api-request>
  hetzner-api-request make-hetzner-api-request hetzner-api-request?
  (body hetzner-api-request-body (default *unspecified*))
  (headers hetzner-api-request-headers (default '()))
  (method hetzner-api-request-method (default 'GET))
  (params hetzner-api-request-params (default '()))
  (url hetzner-api-request-url))

(define (hetzner-api-request-uri request)
  "Return the URI object of the Hetzner API request."
  (let ((params (hetzner-api-request-params request)))
    (string->uri (string-append (hetzner-api-request-url request)
                                (format-query-params params)))))

(define (hetzner-api-request-body-bytevector request)
  "Return the body of the Hetzner API REQUEST as a bytevector."
  (let ((body (hetzner-api-request-body request)))
    (string->utf8 (if (unspecified? body) "" (scm->json-string body)))))

(define (hetzner-api-request-write port request)
  "Write the Hetzner API REQUEST to PORT."
  (let* ((body (hetzner-api-request-body-bytevector request))
         (request (build-request
                   (hetzner-api-request-uri request)
                   #:method (hetzner-api-request-method request)
                   #:version '(1 . 1)
                   #:headers (cons* `(Content-Length
                                      . ,(number->string
                                          (if (unspecified? body)
                                              0 (bytevector-length body))))
                                    (hetzner-api-request-headers request))
                   #:port port))
         (request (write-request request port)))
    (unless (unspecified? body)
      (write-request-body request body))
    (force-output (request-port request))))

(define* (hetzner-api-request-send request #:key (expected (list 200 201 204)))
  "Send the Hetzner API REQUEST via HTTP."
  (let ((port (open-socket-for-uri (hetzner-api-request-uri request))))
    (hetzner-api-request-write port request)
    (let ((response (hetzner-api-response-read port)))
      (close-port port)
      (hetzner-api-response-validate-status response expected)
      response)))

;; Prevent compiler from inlining this function, so we can mock it in tests.
(set! hetzner-api-request-send hetzner-api-request-send)

(define (hetzner-api-request-next-params request)
  "Return the pagination params for the next page of the REQUEST."
  (let* ((params (hetzner-api-request-params request))
         (page (or (assoc-ref params "page") 1)))
    (map (lambda (param)
           (if (equal? "page" (car param))
               (cons (car param) (+ page 1))
               param))
         params)))

(define (hetzner-api-request-paginate request)
  "Fetch all pages of the REQUEST via pagination and return all responses."
  (let* ((response (hetzner-api-request-send request))
         (pagination (hetzner-api-response-pagination response))
         (next-page (assoc-ref pagination "next_page")))
    (if (number? next-page)
        (cons response
              (hetzner-api-request-paginate
               (hetzner-api-request
                (inherit request)
                (params (hetzner-api-request-next-params request)))))
        (list response))))



;;;
;;; Hetzner API.
;;;

(define-record-type* <hetzner-api>
  hetzner-api make-hetzner-api hetzner-api?
  (base-url hetzner-api-base-url ; string
            (default "https://api.hetzner.cloud/v1"))
  (token hetzner-api-token ; string
         (default (%hetzner-default-api-token))))

(define (hetzner-api-authorization-header api)
  "Return the authorization header for the Hetzner API."
  (format #f "Bearer ~a" (hetzner-api-token api)))

(define (hetzner-api-default-headers api)
  "Returns the default headers of the Hetzner API."
  `((user-agent . "Guix Deploy")
    (Accept . "application/json")
    (Authorization . ,(hetzner-api-authorization-header api))
    (Content-Type . "application/json")))

(define (hetzner-api-url api path)
  "Append PATH to the base url of the Hetzner API."
  (string-append (hetzner-api-base-url api) path))

(define (hetzner-api-delete api path)
  "Delete the resource at PATH with the Hetzner API."
  (hetzner-api-response-body
   (hetzner-api-request-send
    (hetzner-api-request
     (headers (hetzner-api-default-headers api))
     (method 'DELETE)
     (url (hetzner-api-url api path))))))

(define* (hetzner-api-list api path resources json->object #:key (params '()))
  "Fetch all objects of RESOURCE from the Hetzner API."
  (let ((body (hetzner-api-response-body
               (hetzner-api-response-pagination-combine
                resources (hetzner-api-request-paginate
                           (hetzner-api-request
                            (url (hetzner-api-url api path))
                            (headers (hetzner-api-default-headers api))
                            (params (cons '("page" . 1) params))))))))
    (map json->object (assoc-ref body resources))))

(define* (hetzner-api-post api path #:key (body *unspecified*))
  "Send a POST request to the Hetzner API at PATH using BODY."
  (hetzner-api-response-body
   (hetzner-api-request-send
    (hetzner-api-request
     (body body)
     (method 'POST)
     (url (hetzner-api-url api path))
     (headers (hetzner-api-default-headers api))))))

(define (hetzner-api-actions api ids)
  "Get actions from the Hetzner API."
  (if (zero? (length ids))
      (raise-exception
       (formatted-message
        (G_ "expected at least one action id, but got '~a'")
        (length ids)))
      (hetzner-api-list
       api "/actions" "actions" json->hetzner-action
       #:params `(("id" . ,(string-join (map number->string ids) ","))))))

(define* (hetzner-api-action-wait api action #:optional (status "success"))
  "Wait until the ACTION has reached STATUS on the Hetzner API."
  (let ((id (hetzner-action-id action)))
    (let loop ()
      (let ((actions (hetzner-api-actions api (list id))))
        (cond
         ((zero? (length actions))
          (raise-exception
           (formatted-message (G_ "server action '~a' not found") id)))
         ((not (= 1 (length actions)))
          (raise-exception
           (formatted-message
            (G_ "expected one server action, but got '~a'")
            (length actions))))
         ((string= status (hetzner-action-status (car actions)))
          (car actions))
         (else
          (sleep 5)
          (loop)))))))

(define* (hetzner-api-locations api . options)
  "Get deployment locations from the Hetzner API."
  (apply hetzner-api-list api "/locations" "locations" json->hetzner-location options))

(define* (hetzner-api-server-create
          api name ssh-keys
          #:key
          (ipv4 #f)
          (ipv6 #f)
          (image %hetzner-default-server-image)
          (labels '())
          (location %hetzner-default-server-location)
          (server-type %hetzner-default-server-type)
          (start-after-create? #f))
  "Create a server with the Hetzner API."
  (let ((body (hetzner-api-post
               api "/servers"
               #:body `(("image" . ,image)
                        ("labels" . ,labels)
                        ("name" . ,name)
                        ("public_net" .
                         (("enable_ipv4" . ,(and ipv4 #t))
                          ("enable_ipv6" . ,(and ipv6 #t))
                          ,@(if (integer? ipv4) `(("ipv4" . ,ipv4)) '())
                          ,@(if (integer? ipv6) `(("ipv6" . ,ipv6)) '())))
                        ("location" . ,location)
                        ("server_type" . ,server-type)
                        ("ssh_keys" . ,(apply vector (map hetzner-ssh-key-id ssh-keys)))
                        ("start_after_create" . ,start-after-create?)))))
    (hetzner-api-action-wait api (hetzner-api-body-action body))
    (json->hetzner-server (assoc-ref body "server"))))

(define (hetzner-api-server-delete api server)
  "Delete the SERVER with the Hetzner API."
  (let ((body (hetzner-api-delete api (hetzner-server-path server))))
    (hetzner-api-action-wait api (hetzner-api-body-action body))))

(define* (hetzner-api-server-enable-rescue-system
          api server ssh-keys #:key (type "linux64"))
  "Enable the rescue system for SERVER with the Hetzner API."
  (let* ((ssh-keys (apply vector (map hetzner-ssh-key-id ssh-keys)))
         (body (hetzner-api-post
                api (hetzner-server-path server "/actions/enable_rescue")
                #:body `(("ssh_keys" . ,ssh-keys)
                         ("type" . ,type)))))
    (hetzner-api-action-wait api (hetzner-api-body-action body))))

(define* (hetzner-api-servers api . options)
  "Get servers from the Hetzner API."
  (apply hetzner-api-list api "/servers" "servers" json->hetzner-server options))

(define (hetzner-api-server-power-on api server)
  "Send a power on request for SERVER to the Hetzner API."
  (let ((body (hetzner-api-post api (hetzner-server-path server "/actions/poweron"))))
    (hetzner-api-action-wait api (hetzner-api-body-action body))))

(define (hetzner-api-server-power-off api server)
  "Send a power off request for SERVER to the Hetzner API."
  (let ((body (hetzner-api-post api (hetzner-server-path server "/actions/poweroff"))))
    (hetzner-api-action-wait api (hetzner-api-body-action body))))

(define (hetzner-api-server-reboot api server)
  "Send a reboot request for SERVER to the Hetzner API."
  (let ((body (hetzner-api-post api (hetzner-server-path server "/actions/reboot"))))
    (hetzner-api-action-wait api (hetzner-api-body-action body))))

(define* (hetzner-api-ssh-key-create api name public-key #:key (labels '()))
  "Create a SSH key with the Hetzner API."
  (let ((body (hetzner-api-post
               api "/ssh_keys"
               #:body `(("name" . ,name)
                        ("public_key" . ,public-key)
                        ("labels" . ,labels)))))
    (json->hetzner-ssh-key (assoc-ref body "ssh_key"))))

(define (hetzner-api-ssh-key-delete api ssh-key)
  "Delete the SSH key on the Hetzner API."
  (hetzner-api-delete api (hetzner-ssh-key-path ssh-key))
  #t)

(define* (hetzner-api-ssh-keys api . options)
  "Get SSH keys from the Hetzner API."
  (apply hetzner-api-list api "/ssh_keys" "ssh_keys"
         json->hetzner-ssh-key options))

(define* (hetzner-api-primary-ips api . options)
  "Get Primary IPs from the Hetzner API."
  (apply hetzner-api-list api "/primary_ips" "primary_ips"
         json->hetzner-primary-ip options))

(define* (hetzner-api-server-types api . options)
  "Get server types from the Hetzner API."
  (apply hetzner-api-list api "/server_types" "server_types"
         json->hetzner-server-type options))
