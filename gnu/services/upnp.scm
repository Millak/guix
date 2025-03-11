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

(define-module (gnu services upnp)
  #:use-module (gnu build linux-container)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages upnp)
  #:use-module (gnu services admin)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system shadow)
  #:use-module (guix gexp)
  #:use-module (guix least-authority)
  #:use-module (guix modules)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:export (%readymedia-default-cache-directory
            %readymedia-default-log-directory
            %readymedia-default-port
            %readymedia-log-file
            %readymedia-user-account
            %readymedia-user-group
            readymedia-configuration
            readymedia-configuration?
            readymedia-configuration-readymedia
            readymedia-configuration-port
            readymedia-configuration-cache-directory
            readymedia-configuration-extra-config
            readymedia-configuration-friendly-name
            readymedia-configuration-log-directory
            readymedia-configuration-media-directories
            readymedia-media-directory
            readymedia-media-directory-path
            readymedia-media-directory-types
            readymedia-media-directory?
            readymedia-service-type))

;;; Commentary:
;;;
;;; UPnP services.
;;;
;;; Code:

(define %readymedia-default-cache-directory "/var/cache/readymedia")
(define %readymedia-default-log-directory "/var/log/readymedia")
(define %readymedia-log-file "minidlna.log")
(define %readymedia-user-group "readymedia")
(define %readymedia-user-account "readymedia")

(define-record-type* <readymedia-configuration>
  readymedia-configuration make-readymedia-configuration
  readymedia-configuration?
  (readymedia readymedia-configuration-readymedia
              (default readymedia))
  (port readymedia-configuration-port
        (default #f))
  (cache-directory readymedia-configuration-cache-directory
                   (default %readymedia-default-cache-directory))
  (log-directory readymedia-configuration-log-directory
                 (default %readymedia-default-log-directory))
  (friendly-name readymedia-configuration-friendly-name
                 (default #f))
  (media-directories readymedia-configuration-media-directories)
  (extra-config readymedia-configuration-extra-config
                (default '())))

;; READYMEDIA-MEDIA-DIR is a record that indicates the path of a media folder
;; and the types of media included within it. Allowed individual types are the
;; symbols 'A' for audio, 'V' for video, and 'P' for pictures. The types field
;; can contain any combination of individual types; an empty list means that
;; no type is specified.
(define-record-type* <readymedia-media-directory>
  readymedia-media-directory make-readymedia-media-directory
  readymedia-media-directory?
  (path readymedia-media-directory-path)
  (types readymedia-media-directory-types
         (default '())))

(define (readymedia-configuration->config-file config)
  "Return the ReadyMedia/MiniDLNA configuration file corresponding to CONFIG."
  (match-record config <readymedia-configuration>
    (port friendly-name cache-directory log-directory media-directories extra-config)
    (apply mixed-text-file
           "minidlna.conf"
           "db_dir=" cache-directory "\n"
           "log_dir=" log-directory "\n"
           (if friendly-name
               (string-append "friendly_name=" friendly-name "\n")
               "")
           (if port
               (string-append "port=" (number->string port) "\n")
               "")
           (append (map (match-record-lambda <readymedia-media-directory>
                            (path types)
                          (apply string-append
                                 "media_dir="
                                 (append (map symbol->string types)
                                         (match types
                                           (() (list))
                                           (_ (list ",")))
                                         (list path "\n"))))
                        media-directories)
                   (map (match-lambda
                          ((key . value)
                           (string-append key "=" value "\n")))
                        extra-config)))))

(define (readymedia-shepherd-service config)
  "Return a least-authority ReadyMedia/MiniDLNA Shepherd service."
  (match-record config <readymedia-configuration>
    (cache-directory log-directory media-directories)
    (let ((minidlna-conf (readymedia-configuration->config-file config)))
      (shepherd-service
       (documentation "Run the ReadyMedia/MiniDLNA daemon.")
       (provision '(readymedia))
       (requirement '(networking user-processes))
       (start
        #~(make-forkexec-constructor
           (list #$(least-authority-wrapper
                    (file-append (readymedia-configuration-readymedia config)
                                 "/sbin/minidlnad")
                    #:name "minidlna"
                    #:mappings
                    (cons* (file-system-mapping
                            (source cache-directory)
                            (target source)
                            (writable? #t))
                           (file-system-mapping
                            (source log-directory)
                            (target source)
                            (writable? #t))
                           (file-system-mapping
                            (source minidlna-conf)
                            (target source))
                           (map (lambda (directory)
                                  (file-system-mapping
                                   (source (readymedia-media-directory-path directory))
                                   (target source)))
                                media-directories))
                    #:namespaces (delq 'net %namespaces))
                 "-f"
                 #$minidlna-conf
                 "-S")
           #:log-file #$(string-append log-directory "/" %readymedia-log-file)
           #:user #$%readymedia-user-account
           #:group #$%readymedia-user-group))
       (stop #~(make-kill-destructor))))))

(define readymedia-accounts
  (list (user-account
         (name "readymedia")
         (group "readymedia")
         (system? #t)
         (comment "ReadyMedia/MiniDLNA daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))
        (user-group
         (name "readymedia")
         (system? #t))))

(define (readymedia-activation config)
  "Set up directories for ReadyMedia/MiniDLNA."
  (match-record config <readymedia-configuration>
    (cache-directory log-directory media-directories)
    (with-imported-modules (source-module-closure '((gnu build activation)))
      #~(begin
          (use-modules (gnu build activation))

          (for-each (lambda (directory)
                      (unless (file-exists? directory)
                        (mkdir-p/perms directory
                                       (getpw #$%readymedia-user-account)
                                       #o775)))
                    (list #$@(map readymedia-media-directory-path
                                  media-directories)))
          (for-each (lambda (directory)
                      (unless (file-exists? directory)
                        (mkdir-p/perms directory
                                       (getpw #$%readymedia-user-account)
                                       #o755)))
                    (list #$cache-directory #$log-directory))))))

(define readymedia-service-type
  (service-type
   (name 'readymedia)
   (extensions
    (list (service-extension shepherd-root-service-type
                             (compose list readymedia-shepherd-service))
          (service-extension account-service-type
                             (const readymedia-accounts))
          (service-extension activation-service-type
                             readymedia-activation)))
   (description
    "Run @command{minidlnad}, the ReadyMedia/MiniDLNA media server.")))
