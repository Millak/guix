;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;; Copyright © 2020 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2022 Remco van 't Veer <remco@remworks.net>
;;; Copyright © 2024 Sören Tempel <soeren@soeren-tempel.net>
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

(define-module (gnu services dns)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages dns)
  #:use-module (guix deprecation)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 regex)
  #:export (knot-service-type
            knot-acl-configuration
            knot-key-configuration
            knot-keystore-configuration
            knot-zone-configuration
            knot-remote-configuration
            knot-policy-configuration
            knot-configuration
            define-zone-entries
            zone-file
            zone-entry

            knot-resolver-service-type
            knot-resolver-configuration

            dnsmasq-service-type
            dnsmasq-configuration

            unbound-service-type
            unbound-configuration
            unbound-configuration?
            unbound-server
            unbound-server?
            unbound-zone
            unbound-zone?
            unbound-remote
            unbound-remote?))

;;;
;;; Knot DNS.
;;;

(define-record-type* <knot-key-configuration>
  knot-key-configuration make-knot-key-configuration
  knot-key-configuration?
  (id        knot-key-configuration-id
             (default ""))
  (algorithm knot-key-configuration-algorithm
             (default #f)); one of #f, or an algorithm name
  (secret    knot-key-configuration-secret
             (default "")))

(define-record-type* <knot-acl-configuration>
  knot-acl-configuration make-knot-acl-configuration
  knot-acl-configuration?
  (id      knot-acl-configuration-id
           (default ""))
  (address knot-acl-configuration-address
           (default '()))
  (key     knot-acl-configuration-key
           (default '()))
  (action  knot-acl-configuration-action
           (default '()))
  (deny?   knot-acl-configuration-deny?
           (default #f)))

(define-record-type* <zone-entry>
  zone-entry make-zone-entry
  zone-entry?
  (name  zone-entry-name
         (default "@"))
  (ttl   zone-entry-ttl
         (default ""))
  (class zone-entry-class
         (default "IN"))
  (type  zone-entry-type
         (default "A"))
  (data  zone-entry-data
         (default "")))

(define-record-type* <zone-file>
  zone-file make-zone-file
  zone-file?
  (entries zone-file-entries
           (default '()))
  (origin  zone-file-origin
           (default ""))
  (ns      zone-file-ns
           (default "ns"))
  (mail    zone-file-mail
           (default "hostmaster"))
  (serial  zone-file-serial
           (default 1))
  (refresh zone-file-refresh
           (default (* 12 3600)))
  (retry   zone-file-retry
           (default (* 15 60)))
  (expiry  zone-file-expiry
           (default (* 2 7 24 3600)))
  (nx      zone-file-nx
           (default 3600)))
(define-record-type* <knot-keystore-configuration>
  knot-keystore-configuration make-knot-keystore-configuration
  knot-keystore-configuration?
  (id knot-keystore-configuration-id
      (default ""))
  (backend knot-keystore-configuration-backend
           (default 'pem))
  (config  knot-keystore-configuration-config
           (default "/var/lib/knot/keys/keys")))

(define-record-type* <knot-policy-configuration>
  knot-policy-configuration make-knot-policy-configuration
  knot-policy-configuration?
  (id                   knot-policy-configuration-id
                        (default ""))
  (keystore             knot-policy-configuration-keystore
                        (default "default"))
  (manual?              knot-policy-configuration-manual?
                        (default #f))
  (single-type-signing? knot-policy-configuration-single-type-signing?
                        (default #f))
  (algorithm            knot-policy-configuration-algorithm
                        (default "ecdsap256sha256"))
  (ksk-size             knot-policy-configuration-ksk-size
                        (default 256))
  (zsk-size             knot-policy-configuration-zsk-size
                        (default 256))
  (dnskey-ttl           knot-policy-configuration-dnskey-ttl
                        (default 'default))
  (zsk-lifetime         knot-policy-configuration-zsk-lifetime
                        (default (* 30 24 3600)))
  (propagation-delay    knot-policy-configuration-propagation-delay
                        (default (* 24 3600)))
  (rrsig-lifetime       knot-policy-configuration-rrsig-lifetime
                        (default (* 14 24 3600)))
  (rrsig-refresh        knot-policy-configuration-rrsig-refresh
                        (default (* 7 24 3600)))
  (nsec3?               knot-policy-configuration-nsec3?
                        (default #f))
  (nsec3-iterations     knot-policy-configuration-nsec3-iterations
                        (default 5))
  (nsec3-salt-length    knot-policy-configuration-nsec3-salt-length
                        (default 8))
  (nsec3-salt-lifetime  knot-policy-configuration-nsec3-salt-lifetime
                        (default (* 30 24 3600))))

(define-record-type* <knot-zone-configuration>
  knot-zone-configuration make-knot-zone-configuration
  knot-zone-configuration?
  (domain            knot-zone-configuration-domain
                     (default ""))
  (file              knot-zone-configuration-file
                     (default "")) ; the file where this zone is saved.
  (zone              knot-zone-configuration-zone
                     (default (zone-file))) ; initial content of the zone file
  (master            knot-zone-configuration-master
                     (default '()))
  (ddns-master       knot-zone-configuration-ddns-master
                     (default #f))
  (notify            knot-zone-configuration-notify
                     (default '()))
  (acl               knot-zone-configuration-acl
                     (default '()))
  (semantic-checks?  knot-zone-configuration-semantic-checks?
                     (default #f))
  (zonefile-sync     knot-zone-configuration-zonefile-sync
                     (default 0))
  (zonefile-load     knot-zone-configuration-zonefile-load
                     (default #f))
  (journal-content   knot-zone-configuration-journal-content
                     (default #f))
  (max-journal-usage knot-zone-configuration-max-journal-usage
                     (default #f))
  (max-journal-depth knot-zone-configuration-max-journal-depth
                     (default #f))
  (max-zone-size     knot-zone-configuration-max-zone-size
                     (default #f))
  (dnssec-policy     knot-zone-configuration-dnssec-policy
                     (default #f))
  (serial-policy     knot-zone-configuration-serial-policy
                     (default 'increment)))

(define-record-type* <knot-remote-configuration>
  knot-remote-configuration make-knot-remote-configuration
  knot-remote-configuration?
  (id  knot-remote-configuration-id
       (default ""))
  (address knot-remote-configuration-address
           (default '()))
  (via     knot-remote-configuration-via
           (default '()))
  (key     knot-remote-configuration-key
           (default #f)))

(define-record-type* <knot-configuration>
  knot-configuration make-knot-configuration
  knot-configuration?
  (knot          knot-configuration-knot
                 (default knot))
  (run-directory knot-configuration-run-directory
                 (default "/var/run/knot"))
  (includes      knot-configuration-includes
                 (default '()))
  (listen-v4     knot-configuration-listen-v4
                 (default "0.0.0.0"))
  (listen-v6     knot-configuration-listen-v6
                 (default "::"))
  (listen-port   knot-configuration-listen-port
                 (default 53))
  (keys          knot-configuration-keys
                 (default '()))
  (keystores     knot-configuration-keystores
                 (default '()))
  (acls          knot-configuration-acls
                 (default '()))
  (remotes       knot-configuration-remotes
                 (default '()))
  (policies      knot-configuration-policies
                 (default '()))
  (zones         knot-configuration-zones
                 (default '())))

(define-syntax define-zone-entries
  (syntax-rules ()
    ((_ id (name ttl class type data) ...)
     (define id (list (make-zone-entry name ttl class type data) ...)))))

(define (error-out msg)
  (raise (condition (&message (message msg)))))

(define (verify-knot-key-configuration key)
  (unless (knot-key-configuration? key)
    (error-out "keys must be a list of only knot-key-configuration."))
  (let ((id (knot-key-configuration-id key)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "key id must be a non empty string.")))
  (unless (memq (knot-key-configuration-algorithm key)
                '(#f hmac-md5 hmac-sha1 hmac-sha224 hmac-sha256 hmac-sha384 hmac-sha512))
    (error-out "algorithm must be one of: #f, 'hmac-md5, 'hmac-sha1,
'hmac-sha224, 'hmac-sha256, 'hmac-sha384 or 'hmac-sha512")))

(define (verify-knot-keystore-configuration keystore)
  (unless (knot-keystore-configuration? keystore)
    (error-out "keystores must be a list of only knot-keystore-configuration."))
  (let ((id (knot-keystore-configuration-id keystore)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "keystore id must be a non empty string.")))
  (unless (memq (knot-keystore-configuration-backend keystore)
                '(pem pkcs11))
    (error-out "backend must be one of: 'pem or 'pkcs11")))

(define (verify-knot-policy-configuration policy)
  (unless (knot-policy-configuration? policy)
    (error-out "policies must be a list of only knot-policy-configuration."))
  (let ((id (knot-policy-configuration-id policy)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "policy id must be a non empty string."))))

(define (verify-knot-acl-configuration acl)
  (unless (knot-acl-configuration? acl)
    (error-out "acls must be a list of only knot-acl-configuration."))
  (let ((id (knot-acl-configuration-id acl))
        (address (knot-acl-configuration-address acl))
        (key (knot-acl-configuration-key acl))
        (action (knot-acl-configuration-action acl)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "acl id must be a non empty string."))
    (unless (and (list? address)
                 (every string? address))
      (error-out "acl address must be a list of strings.")))
  (unless (boolean? (knot-acl-configuration-deny? acl))
    (error-out "deny? must be #t or #f.")))

(define (verify-knot-zone-configuration zone)
  (unless (knot-zone-configuration? zone)
    (error-out "zones must be a list of only knot-zone-configuration."))
  (let ((domain (knot-zone-configuration-domain zone)))
    (unless (and (string? domain) (not (equal? domain "")))
      (error-out "zone domain must be a non empty string."))))

(define (verify-knot-remote-configuration remote)
  (unless (knot-remote-configuration? remote)
    (error-out "remotes must be a list of only knot-remote-configuration."))
  (let ((id (knot-remote-configuration-id remote)))
    (unless (and (string? id) (not (equal? id "")))
      (error-out "remote id must be a non empty string."))))

(define (verify-knot-configuration config)
  (unless (file-like? (knot-configuration-knot config))
    (error-out "knot configuration field must be a file-like object."))
  (unless (string? (knot-configuration-run-directory config))
    (error-out "run-directory must be a string."))
  (unless (list? (knot-configuration-includes config))
    (error-out "includes must be a list of strings or file-like objects."))
  (unless (list? (knot-configuration-keys config))
    (error-out "keys must be a list of knot-key-configuration."))
  (for-each (lambda (key) (verify-knot-key-configuration key))
            (knot-configuration-keys config))
  (unless (list? (knot-configuration-keystores config))
    (error-out "keystores must be a list of knot-keystore-configuration."))
  (for-each (lambda (keystore) (verify-knot-keystore-configuration keystore))
            (knot-configuration-keystores config))
  (unless (list? (knot-configuration-acls config))
    (error-out "acls must be a list of knot-acl-configuration."))
  (for-each (lambda (acl) (verify-knot-acl-configuration acl))
            (knot-configuration-acls config))
  (unless (list? (knot-configuration-zones config))
    (error-out "zones must be a list of knot-zone-configuration."))
  (for-each (lambda (zone) (verify-knot-zone-configuration zone))
            (knot-configuration-zones config))
  (unless (list? (knot-configuration-policies config))
    (error-out "policies must be a list of knot-policy-configuration."))
  (for-each (lambda (policy) (verify-knot-policy-configuration policy))
            (knot-configuration-policies config))
  (unless (list? (knot-configuration-remotes config))
    (error-out "remotes must be a list of knot-remote-configuration."))
  (for-each (lambda (remote) (verify-knot-remote-configuration remote))
            (knot-configuration-remotes config))
  #t)

(define (format-string-list l)
  "Formats a list of string in YAML"
  (if (eq? l '())
      ""
      (let ((l (reverse l)))
        (string-append
          "["
          (fold (lambda (x1 x2)
                  (string-append (if (symbol? x1) (symbol->string x1) x1) ", "
                                 (if (symbol? x2) (symbol->string x2) x2)))
                (if (symbol? (car l)) (symbol->string (car l)) (car l)) (cdr l))
          "]"))))

(define (knot-acl-config acls)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (acl-config)
          (let ((id (knot-acl-configuration-id acl-config))
                (address (knot-acl-configuration-address acl-config))
                (key (knot-acl-configuration-key acl-config))
                (action (knot-acl-configuration-action acl-config))
                (deny? (knot-acl-configuration-deny? acl-config)))
            (format #t "    - id: ~a\n" id)
            (unless (eq? address '())
              (format #t "      address: ~a\n" (format-string-list address)))
            (unless (eq? key '())
              (format #t "      key: ~a\n" (format-string-list key)))
            (unless (eq? action '())
              (format #t "      action: ~a\n" (format-string-list action)))
            (format #t "      deny: ~a\n" (if deny? "on" "off"))))
        acls))))

(define (knot-key-config keys)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (key-config)
          (let ((id (knot-key-configuration-id key-config))
                (algorithm (knot-key-configuration-algorithm key-config))
                (secret (knot-key-configuration-secret key-config)))
            (format #t     "    - id: ~a\n" id)
            (if algorithm
                (format #t "      algorithm: ~a\n" (symbol->string algorithm)))
            (format #t     "      secret: ~a\n" secret)))
        keys))))

(define (knot-keystore-config keystores)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (keystore-config)
          (let ((id (knot-keystore-configuration-id keystore-config))
                (backend (knot-keystore-configuration-backend keystore-config))
                (config (knot-keystore-configuration-config keystore-config)))
            (format #t "    - id: ~a\n" id)
            (format #t "      backend: ~a\n" (symbol->string backend))
            (format #t "      config: \"~a\"\n" config)))
        keystores))))

(define (knot-policy-config policies)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (policy-config)
          (let ((id (knot-policy-configuration-id policy-config))
                (keystore (knot-policy-configuration-keystore policy-config))
                (manual? (knot-policy-configuration-manual? policy-config))
                (single-type-signing? (knot-policy-configuration-single-type-signing?
                                        policy-config))
                (algorithm (knot-policy-configuration-algorithm policy-config))
                (ksk-size (knot-policy-configuration-ksk-size policy-config))
                (zsk-size (knot-policy-configuration-zsk-size policy-config))
                (dnskey-ttl (knot-policy-configuration-dnskey-ttl policy-config))
                (zsk-lifetime (knot-policy-configuration-zsk-lifetime policy-config))
                (propagation-delay (knot-policy-configuration-propagation-delay
                                     policy-config))
                (rrsig-lifetime (knot-policy-configuration-rrsig-lifetime
                                  policy-config))
                (nsec3? (knot-policy-configuration-nsec3? policy-config))
                (nsec3-iterations (knot-policy-configuration-nsec3-iterations
                                    policy-config))
                (nsec3-salt-length (knot-policy-configuration-nsec3-salt-length
                                     policy-config))
                (nsec3-salt-lifetime (knot-policy-configuration-nsec3-salt-lifetime
                                       policy-config)))
            (format #t "    - id: ~a\n" id)
            (format #t "      keystore: ~a\n" keystore)
            (format #t "      manual: ~a\n" (if manual? "on" "off"))
            (format #t "      single-type-signing: ~a\n" (if single-type-signing?
                                                             "on" "off"))
            (format #t "      algorithm: ~a\n" algorithm)
            (format #t "      ksk-size: ~a\n" (number->string ksk-size))
            (format #t "      zsk-size: ~a\n" (number->string zsk-size))
            (unless (eq? dnskey-ttl 'default)
              (format #t "      dnskey-ttl: ~a\n" dnskey-ttl))
            (format #t "      zsk-lifetime: ~a\n" zsk-lifetime)
            (format #t "      propagation-delay: ~a\n" propagation-delay)
            (format #t "      rrsig-lifetime: ~a\n" rrsig-lifetime)
            (format #t "      nsec3: ~a\n" (if nsec3? "on" "off"))
            (format #t "      nsec3-iterations: ~a\n"
                    (number->string nsec3-iterations))
            (format #t "      nsec3-salt-length: ~a\n"
                    (number->string nsec3-salt-length))
            (format #t "      nsec3-salt-lifetime: ~a\n" nsec3-salt-lifetime)))
        policies))))

(define (knot-remote-config remotes)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (remote-config)
          (let ((id (knot-remote-configuration-id remote-config))
                (address (knot-remote-configuration-address remote-config))
                (via (knot-remote-configuration-via remote-config))
                (key (knot-remote-configuration-key remote-config)))
            (format #t "    - id: ~a\n" id)
            (unless (eq? address '())
              (format #t "      address: ~a\n" (format-string-list address)))
            (unless (eq? via '())
              (format #t "      via: ~a\n" (format-string-list via)))
            (if key
              (format #t "      key: ~a\n" key))))
        remotes))))

(define (serialize-zone-entries entries)
  (with-output-to-string
    (lambda ()
      (for-each
        (lambda (entry)
          (let ((name (zone-entry-name entry))
                (ttl (zone-entry-ttl entry))
                (class (zone-entry-class entry))
                (type (zone-entry-type entry))
                (data (zone-entry-data entry)))
            (format #t "~a ~a ~a ~a ~a\n" name ttl class type data)))
        entries))))

(define (serialize-zone-file zone domain)
  (computed-file (string-append domain ".zone")
    #~(begin
        (call-with-output-file #$output
          (lambda (port)
            (format port "$ORIGIN ~a.\n"
                    #$(zone-file-origin zone))
            (format port "@ IN SOA ~a ~a (~a ~a ~a ~a ~a)\n"
                    #$(zone-file-ns zone)
                    #$(zone-file-mail zone)
                    #$(zone-file-serial zone)
                    #$(zone-file-refresh zone)
                    #$(zone-file-retry zone)
                    #$(zone-file-expiry zone)
                    #$(zone-file-nx zone))
            (format port "~a\n"
                    #$(serialize-zone-entries (zone-file-entries zone))))))))

(define (knot-zone-config zone)
  (let ((content (knot-zone-configuration-zone zone)))
    #~(with-output-to-string
        (lambda ()
          (let ((domain #$(knot-zone-configuration-domain zone))
                (file #$(knot-zone-configuration-file zone))
                (master (list #$@(knot-zone-configuration-master zone)))
                (ddns-master #$(knot-zone-configuration-ddns-master zone))
                (notify (list #$@(knot-zone-configuration-notify zone)))
                (acl (list #$@(knot-zone-configuration-acl zone)))
                (semantic-checks? #$(knot-zone-configuration-semantic-checks? zone))
                (zonefile-sync #$(knot-zone-configuration-zonefile-sync zone))
                (zonefile-load '#$(knot-zone-configuration-zonefile-load zone))
                (journal-content #$(knot-zone-configuration-journal-content zone))
                (max-journal-usage #$(knot-zone-configuration-max-journal-usage zone))
                (max-journal-depth #$(knot-zone-configuration-max-journal-depth zone))
                (max-zone-size #$(knot-zone-configuration-max-zone-size zone))
                (dnssec-policy #$(knot-zone-configuration-dnssec-policy zone))
                (serial-policy '#$(knot-zone-configuration-serial-policy zone)))
            (format #t "    - domain: ~a\n" domain)
            (if (eq? master '())
                ;; This server is a master
                (if (equal? file "")
                  (format #t "      file: ~a\n"
                    #$(serialize-zone-file content
                                           (knot-zone-configuration-domain zone)))
                  (format #t "      file: ~a\n" file))
                ;; This server is a slave (has masters)
                (begin
                  (format #t "      master: ~a\n"
                          #$(format-string-list
                              (knot-zone-configuration-master zone)))
                  (if ddns-master (format #t "      ddns-master ~a\n" ddns-master))))
            (unless (eq? notify '())
              (format #t "      notify: ~a\n"
                      #$(format-string-list
                          (knot-zone-configuration-notify zone))))
            (unless (eq? acl '())
              (format #t "      acl: ~a\n"
                      #$(format-string-list
                          (knot-zone-configuration-acl zone))))
            (format #t "      semantic-checks: ~a\n" (if semantic-checks? "on" "off"))
            (if zonefile-sync
              (format #t "      zonefile-sync: ~a\n" zonefile-sync))
            (if zonefile-load
              (format #t "      zonefile-load: ~a\n"
                      (symbol->string zonefile-load)))
            (if journal-content
              (format #t "      journal-content: ~a\n"
                      (symbol->string journal-content)))
            (if max-journal-usage
              (format #t "      max-journal-usage: ~a\n" max-journal-usage))
            (if max-journal-depth
              (format #t "      max-journal-depth: ~a\n" max-journal-depth))
            (if max-zone-size
              (format #t "      max-zone-size: ~a\n" max-zone-size))
            (if dnssec-policy
                (begin
                  (format #t "      dnssec-signing: on\n")
                  (format #t "      dnssec-policy: ~a\n" dnssec-policy)))
            (format #t "      serial-policy: ~a\n"
                    (symbol->string serial-policy)))))))

(define (knot-config-file config)
  (verify-knot-configuration config)
  (computed-file "knot.conf"
    #~(begin
        (call-with-output-file #$output
          (lambda (port)
            (for-each (lambda (inc)
                        (format port "include: ~a\n" inc))
                      '#$(knot-configuration-includes config))
            (format port "server:\n")
            (format port "    rundir: ~a\n" #$(knot-configuration-run-directory config))
            (format port "    user: knot\n")
            (format port "    listen: ~a@~a\n"
                    #$(knot-configuration-listen-v4 config)
                    #$(knot-configuration-listen-port config))
            (format port "    listen: ~a@~a\n"
                    #$(knot-configuration-listen-v6 config)
                    #$(knot-configuration-listen-port config))
            (format port "\nkey:\n")
            (format port #$(knot-key-config (knot-configuration-keys config)))
            (format port "\nkeystore:\n")
            (format port #$(knot-keystore-config (knot-configuration-keystores config)))
            (format port "\nacl:\n")
            (format port #$(knot-acl-config (knot-configuration-acls config)))
            (format port "\nremote:\n")
            (format port #$(knot-remote-config (knot-configuration-remotes config)))
            (format port "\npolicy:\n")
            (format port #$(knot-policy-config (knot-configuration-policies config)))
            (unless #$(eq? (knot-configuration-zones config) '())
              (format port "\nzone:\n")
              (format port "~a\n"
                      (string-concatenate
                        (list #$@(map knot-zone-config
                                      (knot-configuration-zones config)))))))))))

(define %knot-accounts
  (list (user-group (name "knot") (system? #t))
        (user-account
          (name "knot")
          (group "knot")
          (system? #t)
          (comment "knot dns server user")
          (home-directory "/var/empty")
          (shell (file-append shadow "/sbin/nologin")))))

(define (knot-activation config)
  (with-imported-modules (source-module-closure '((gnu build activation)))
    #~(begin
        (use-modules (gnu build activation))
        (mkdir-p/perms #$(knot-configuration-run-directory config)
                       (getpwnam "knot") #o755)
        (mkdir-p/perms "/var/lib/knot" (getpwnam "knot") #o755)
        (mkdir-p/perms "/var/lib/knot/keys" (getpwnam "knot") #o755)
        (mkdir-p/perms "/var/lib/knot/keys/keys" (getpwnam "knot") #o755))))

(define (knot-shepherd-service config)
  (let* ((config-file (knot-config-file config))
         (knot (knot-configuration-knot config)))
    (list (shepherd-service
            (documentation "Run the Knot DNS daemon.")
            (provision '(knot dns))
            (requirement '(networking))
            (actions (list (shepherd-configuration-action config-file)))
            (start #~(make-forkexec-constructor
                       (list (string-append #$knot "/sbin/knotd")
                             "-c" #$config-file)))
            (stop #~(make-kill-destructor))))))

(define knot-service-type
  (service-type (name 'knot)
                (extensions
                  (list (service-extension shepherd-root-service-type
                                           knot-shepherd-service)
                        (service-extension activation-service-type
                                           knot-activation)
                        (service-extension account-service-type
                                           (const %knot-accounts))))
                (description
                 "Run @uref{https://www.knot-dns.cz/, Knot}, an authoritative
name server for the @acronym{DNS, Domain Name System}.")))


;;;
;;; Knot Resolver.
;;;

(define-record-type* <knot-resolver-configuration>
  knot-resolver-configuration
  make-knot-resolver-configuration
  knot-resolver-configuration?
  (package knot-resolver-configuration-package
           (default knot-resolver))
  (kresd-config-file knot-resolver-kresd-config-file
                     (default %kresd.conf))
  (garbage-collection-interval knot-resolver-garbage-collection-interval
                               (default 1000)))

(define %kresd.conf
  (plain-file "kresd.conf" "-- -*- mode: lua -*-
trust_anchors.add_file('/var/cache/knot-resolver/root.keys')
net = { '127.0.0.1', '::1' }
user('knot-resolver', 'knot-resolver')
modules = { 'hints > iterate', 'stats', 'predict' }
cache.size = 100 * MB
"))

(define %knot-resolver-accounts
  (list (user-group
         (name "knot-resolver")
         (system? #t))
        (user-account
         (name "knot-resolver")
         (group "knot-resolver")
         (system? #t)
         (home-directory "/var/cache/knot-resolver")
         (shell (file-append shadow "/sbin/nologin")))))

(define (knot-resolver-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let ((rundir "/var/cache/knot-resolver")
            (owner (getpwnam "knot-resolver")))
        (mkdir-p rundir)
        (chown rundir (passwd:uid owner) (passwd:gid owner)))))

(define knot-resolver-shepherd-services
  (match-lambda
    (($ <knot-resolver-configuration> package
                                      kresd-config-file
                                      garbage-collection-interval)
     (list
      (shepherd-service
       (provision '(kresd))
       (requirement '(user-processes networking))
       (documentation "Run the Knot Resolver daemon.")
       (start #~(make-forkexec-constructor
                 '(#$(file-append package "/sbin/kresd")
                   "-c" #$kresd-config-file "-n"
                   "/var/cache/knot-resolver")))
       (stop #~(make-kill-destructor)))
      (shepherd-service
       (provision '(kres-cache-gc))
       (requirement '(user-processes))
       (documentation "Run the Knot Resolver Garbage Collector daemon.")
       (start #~(make-forkexec-constructor
                 '(#$(file-append package "/sbin/kres-cache-gc")
                   "-d" #$(number->string garbage-collection-interval)
                   "-c" "/var/cache/knot-resolver")
                 #:user "knot-resolver"
                 #:group "knot-resolver"))
       (stop #~(make-kill-destructor)))))))

(define knot-resolver-service-type
  (service-type
   (name 'knot-resolver)
   (extensions
    (list (service-extension shepherd-root-service-type
                             knot-resolver-shepherd-services)
          (service-extension activation-service-type
                             knot-resolver-activation)
          (service-extension account-service-type
                             (const %knot-resolver-accounts))))
   (default-value (knot-resolver-configuration))
   (description "Run the Knot DNS Resolver.")))


;;;
;;; Dnsmasq.
;;;

(define-record-type* <dnsmasq-configuration>
  dnsmasq-configuration make-dnsmasq-configuration
  dnsmasq-configuration?
  (package          dnsmasq-configuration-package
                    (default dnsmasq))  ;file-like
  (provision        dnsmasq-configuration-provision ; deprecated
                    (default #f)
                    (sanitize warn-deprecated-dnsmasq-configuration-provision))
  (shepherd-provision           dnsmasq-configuration-shepherd-provision
                                (default '(dnsmasq)))
  (shepherd-requirement         dnsmasq-configuration-shepherd-requirement
                                (default '(user-processes networking)))
  (no-hosts?        dnsmasq-configuration-no-hosts?
                    (default #f))       ;boolean
  (port             dnsmasq-configuration-port
                    (default 53))       ;integer
  (local-service?   dnsmasq-configuration-local-service?
                    (default #t))       ;boolean
  (listen-addresses dnsmasq-configuration-listen-address
                    (default '()))      ;list of string
  (extra-options    dnsmasq-configuration-extra-options
                    (default '()))      ;list of string
  (resolv-file      dnsmasq-configuration-resolv-file
                    (default "/etc/resolv.conf")) ;string
  (no-resolv?       dnsmasq-configuration-no-resolv?
                    (default #f))       ;boolean
  (forward-private-reverse-lookup?
                    dnsmasq-configuration-forward-private-reverse-lookup?
                    (default #t))       ;boolean
  (query-servers-in-order?
                    dnsmasq-configuration-query-servers-in-order?
                    (default #f))       ;boolean
  (servers          dnsmasq-configuration-servers
                    (default '()))      ;list of string
  (servers-file     dnsmasq-configuration-servers-file
                    (default #f))       ;string|file-like
  (addresses        dnsmasq-configuration-addresses
                    (default '()))      ;list of string
  (cache-size       dnsmasq-configuration-cache-size
                    (default 150))      ;integer
  (negative-cache?  dnsmasq-configuration-negative-cache?
                    (default #t))       ;boolean
  (cpe-id           dnsmasq-configuration-cpe-id
                    (default #f))       ;string
  (tftp-enable?     dnsmasq-configuration-tftp-enable?
                    (default #f))       ;boolean
  (tftp-no-fail?    dnsmasq-configuration-tftp-no-fail?
                    (default #f))       ;boolean
  (tftp-single-port? dnsmasq-configuration-tftp-single-port?
                    (default #f))       ;boolean
  (tftp-secure?     dnsmasq-tftp-secure?
                    (default #f))       ;boolean
  (tftp-max         dnsmasq-tftp-max
                    (default #f))       ;integer
  (tftp-mtu         dnsmasq-tftp-mtu
                    (default #f))       ;integer
  (tftp-no-blocksize? dnsmasq-tftp-no-blocksize?
                      (default #f))     ;boolean
  (tftp-lowercase?  dnsmasq-tftp-lowercase?
                    (default #f))       ;boolean
  (tftp-port-range  dnsmasq-tftp-port-range
                    (default #f))       ;string
  (tftp-root        dnsmasq-tftp-root
                    (default "/var/empty,lo")) ;string
  (tftp-unique-root dnsmasq-tftp-unique-root
                    (default #f)))      ;"" or "ip" or "mac"

(define (warn-deprecated-dnsmasq-configuration-provision value)
  (when (pair? value)
    (warn-about-deprecation
     'provision #f
     #:replacement 'shepherd-provision))
  value)

(define (dnsmasq-shepherd-service config)
  (match-record config <dnsmasq-configuration>
    (package
     provision
     shepherd-provision
     shepherd-requirement
     no-hosts?
     port local-service? listen-addresses
     resolv-file no-resolv?
     forward-private-reverse-lookup? query-servers-in-order?
     servers addresses servers-file
     cache-size negative-cache?
     cpe-id
     tftp-enable? tftp-no-fail?
     tftp-single-port? tftp-secure?
     tftp-max tftp-mtu tftp-no-blocksize?
     tftp-lowercase? tftp-port-range
     tftp-root tftp-unique-root extra-options)
    (shepherd-service
     (provision (or provision shepherd-provision))
     (requirement shepherd-requirement)
     (documentation "Run the dnsmasq DNS server.")
     (start #~(make-forkexec-constructor
               (list
                #$(file-append package "/sbin/dnsmasq")
                "--keep-in-foreground"
                "--pid-file=/run/dnsmasq.pid"
                #$@(if no-hosts?
                       '("--no-hosts")
                        '())
                #$(format #f "--port=~a" port)
                #$@(if local-service?
                       '("--local-service")
                        '())
                #$@(map (cut format #f "--listen-address=~a" <>)
                        listen-addresses)
                #$(format #f "--resolv-file=~a" resolv-file)
                #$@(if no-resolv?
                       '("--no-resolv")
                        '())
                #$@(if forward-private-reverse-lookup?
                       '()
                        '("--bogus-priv"))
                #$@(if query-servers-in-order?
                       '("--strict-order")
                        '())
                #$@(if servers-file
                       (list #~(string-append "--servers-file=" #$servers-file))
                       '())
                #$@(map (cut format #f "--server=~a" <>)
                        servers)
                #$@(map (cut format #f "--address=~a" <>)
                        addresses)
                #$(format #f "--cache-size=~a" cache-size)
                #$@(if negative-cache?
                       '()
                        '("--no-negcache"))
                #$@(if cpe-id
                       (list (format #f "--add-cpe-id=~a" cpe-id))
                       '())
                #$@(if tftp-enable?
                       '("--enable-tftp")
                        '())
                #$@(if tftp-no-fail?
                       '("--tftp-no-fail")
                        '())
                #$@(if tftp-single-port?
                       '("--tftp-single-port")
                        '())
                #$@(if tftp-secure?
                       '("--tftp-secure")
                        '())
                #$@(if tftp-max
                       (list (format #f "--tftp-max=~a" tftp-max))
                       '())
                #$@(if tftp-mtu
                       (list (format #f "--tftp-mtu=~a" tftp-mtu))
                       '())
                #$@(if tftp-no-blocksize?
                       '("--tftp-no-blocksize")
                        '())
                #$@(if tftp-lowercase?
                       '("--tftp-lowercase")
                        '())
                #$@(if tftp-port-range
                       (list (format #f "--tftp-port-range=~a"
                                     tftp-port-range))
                       '())
                #$@(if tftp-root
                       (list (format #f "--tftp-root=~a" tftp-root))
                       '())
                #$@(if tftp-unique-root
                       (list
                        (if (> (length tftp-unique-root) 0)
                            (format #f "--tftp-unique-root=~a" tftp-unique-root)
                            (format #f "--tftp-unique-root")))
                       '())
                #$@extra-options)
               #:pid-file "/run/dnsmasq.pid"))
     (stop #~(make-kill-destructor)))))

(define (dnsmasq-activation config)
  #~(begin
      (use-modules (guix build utils))
      ;; create directory to store dnsmasq lease file
      (mkdir-p "/var/lib/misc")))

(define dnsmasq-service-type
  (service-type
   (name 'dnsmasq)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list dnsmasq-shepherd-service))
          (service-extension activation-service-type
                             dnsmasq-activation)))
   (default-value (dnsmasq-configuration))
   (description "Run the dnsmasq DNS server.")))


;;;
;;; Unbound.
;;;

(define (unbound-serialize-field field-name value)
  (let ((field (object->string field-name))
        (value (cond
                 ((boolean? value) (if value "yes" "no"))
                 ((string? value) value)
                 (else (object->string value)))))
    (if (string=? field "extra-content")
      #~(string-append #$value "\n")
      #~(format #f "	~a: ~s~%" #$field #$value))))

(define (unbound-serialize-alist field-name value)
  #~(string-append #$@(generic-serialize-alist list
                                               unbound-serialize-field
                                               value)))

(define (unbound-serialize-section section-name value fields)
  #~(format #f "~a:~%~a"
            #$(object->string section-name)
            #$(serialize-configuration value fields)))

(define unbound-serialize-string unbound-serialize-field)
(define unbound-serialize-boolean unbound-serialize-field)

(define-maybe string (prefix unbound-))
(define-maybe list-of-strings (prefix unbound-))
(define-maybe boolean (prefix unbound-))

(define (unbound-serialize-list-of-strings field-name value)
  #~(string-append #$@(map (cut unbound-serialize-string field-name <>) value)))

(define-configuration unbound-zone
  (name
    string
    "Zone name.")

  (forward-addr
    maybe-list-of-strings
    "IP address of server to forward to.")

  (forward-tls-upstream
    maybe-boolean
    "Whether the queries to this forwarder use TLS for transport.")

  (extra-options
   (alist '())
   "An association list of options to append.")

  (prefix unbound-))

(define (unbound-serialize-unbound-zone field-name value)
  (unbound-serialize-section field-name value unbound-zone-fields))

(define (unbound-serialize-list-of-unbound-zone field-name value)
  #~(string-append #$@(map (cut unbound-serialize-unbound-zone field-name <>)
                           value)))

(define list-of-unbound-zone? (list-of unbound-zone?))

(define-configuration unbound-remote
  (control-enable
    maybe-boolean
    "Enable remote control.")

  (control-interface
    maybe-string
    "IP address or local socket path to listen on for remote control.")

  (extra-options
   (alist '())
   "An association list of options to append.")

  (prefix unbound-))

(define (unbound-serialize-unbound-remote field-name value)
  (unbound-serialize-section field-name value unbound-remote-fields))

(define-configuration unbound-server
  (interface
    maybe-list-of-strings
    "Interfaces listened on for queries from clients.")

  (hide-version
    maybe-boolean
    "Refuse the version.server and version.bind queries.")

  (hide-identity
    maybe-boolean
    "Refuse the id.server and hostname.bind queries.")

  (tls-cert-bundle
    maybe-string
    "Certificate bundle file, used for DNS over TLS.")

  (extra-options
   (alist '())
   "An association list of options to append.")

  (prefix unbound-))

(define (unbound-serialize-unbound-server field-name value)
  (unbound-serialize-section field-name value unbound-server-fields))

(define-configuration unbound-configuration
  (server
    (unbound-server
      (unbound-server
        (interface '("127.0.0.1" "::1"))

        (hide-version #t)
        (hide-identity #t)

        (tls-cert-bundle "/etc/ssl/certs/ca-certificates.crt")))
    "General options for the Unbound server.")

  (remote-control
    (unbound-remote
      (unbound-remote
        (control-enable #t)
        (control-interface "/run/unbound.sock")))
    "Remote control options for the daemon.")

  (forward-zone
    (list-of-unbound-zone '())
    "A zone for which queries should be forwarded to another resolver.")

  (extra-content
    maybe-string
    "Raw content to add to the configuration file.")

  (prefix unbound-))

(define (unbound-config-file config)
  (mixed-text-file "unbound.conf"
    (serialize-configuration
      config
      unbound-configuration-fields)))

(define (unbound-shepherd-service config)
  (let ((config-file (unbound-config-file config)))
    (list (shepherd-service
            (documentation "Unbound daemon.")
            (provision '(unbound dns))
            ;; unbound may be bound to a particular IP address, hence
            ;; only start it after the networking service has started.
            (requirement '(user-processes networking))
            (actions (list (shepherd-configuration-action config-file)))
            (start #~(make-forkexec-constructor
                       (list (string-append #$unbound "/sbin/unbound")
                             "-d" "-p" "-c" #$config-file)))
            (stop #~(make-kill-destructor))))))

(define unbound-account-service
  (list (user-group (name "unbound") (system? #t))
        (user-account
         (name "unbound")
         (group "unbound")
         (system? #t)
         (comment "Unbound daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define unbound-service-type
  (service-type (name 'unbound)
                (description "Run the Unbound DNS resolver.")
                (extensions
                  (list (service-extension account-service-type
                                           (const unbound-account-service))
                        (service-extension shepherd-root-service-type
                                           unbound-shepherd-service)))
                (compose concatenate)
                (default-value (unbound-configuration))))
