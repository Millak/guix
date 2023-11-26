;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2021, 2023 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu services rsync)
  #:use-module ((gnu build linux-container) #:select (%namespaces))
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services shepherd)
  #:autoload   (gnu system file-systems) (file-system-mapping)
  #:use-module (gnu system shadow)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages rsync)
  #:use-module (guix records)
  #:use-module (guix gexp)
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:use-module (guix least-authority)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 match)
  #:export (rsync-configuration
            rsync-configuration?
            rsync-configuration-modules

            rsync-module
            rsync-module?
            rsync-module-name
            rsync-module-file-name
            rsync-module-comment
            rsync-module-read-only
            rsync-module-timeout

            rsync-service-type))

;;;; Commentary:
;;;
;;; This module implements a service that to run instance of Rsync,
;;; files synchronization tool.
;;;
;;;; Code:

(define-with-syntax-properties (warn-share-field-deprecation (value properties))
  (unless (unspecified? value)
    (warning (source-properties->location properties)
             (G_ "the 'share-path' and 'share-comment' fields is deprecated, \
please use 'modules' instead~%")))
  value)

(define-record-type* <rsync-configuration>
  rsync-configuration
  make-rsync-configuration
  rsync-configuration?
  (package       rsync-configuration-package              ; file-like
                 (default rsync))
  (address       rsync-configuration-address              ; string | #f
                 (default #f))
  (port-number   rsync-configuration-port-number          ; integer
                 (default 873))
  (pid-file      rsync-configuration-pid-file             ; string
                 (default "/var/run/rsyncd/rsyncd.pid"))
  (lock-file     rsync-configuration-lock-file            ; string
                 (default "/var/run/rsyncd/rsyncd.lock"))
  (log-file      rsync-configuration-log-file             ; string
                 (default "/var/log/rsyncd.log"))
  (use-chroot?   rsync-configuration-use-chroot?          ; boolean
                 (sanitize warn-share-field-deprecation)
                 (default *unspecified*))
  (modules       rsync-configuration-actual-modules ;list of <rsync-module>
                 (default %default-modules))  ;TODO: eventually remove default
  (share-path    rsync-configuration-share-path           ; string
                 (sanitize warn-share-field-deprecation)
                 (default *unspecified*))
  (share-comment rsync-configuration-share-comment        ; string
                 (sanitize warn-share-field-deprecation)
                 (default *unspecified*))
  (read-only?    rsync-configuration-read-only?           ; boolean
                 (sanitize warn-share-field-deprecation)
                 (default *unspecified*))
  (timeout       rsync-configuration-timeout              ; integer
                 (sanitize warn-share-field-deprecation)
                 (default *unspecified*))
  (user          rsync-configuration-user                 ; string
                 (default "root"))
  (group         rsync-configuration-group                ; string
                 (default "root"))
  (uid           rsync-configuration-uid                  ; string
                 (default "rsyncd"))
  (gid           rsync-configuration-gid                  ; string
                 (default "rsyncd")))

;; Rsync "module": a directory exported the rsync protocol.
(define-record-type* <rsync-module>
  rsync-module make-rsync-module
  rsync-module?
  (name          rsync-module-name)               ;string
  (file-name     rsync-module-file-name)          ;string
  (comment       rsync-module-comment             ;string
                 (default ""))
  (read-only?    rsync-module-read-only?          ;boolean
                 (default #t))
  (chroot?       rsync-module-chroot?             ;boolean
                 (default #t))
  (timeout       rsync-module-timeout             ;integer
                 (default 300)))

(define %default-modules
  ;; Default modules, provided for backward compatibility.
  (list (rsync-module (name "files")
                      (file-name "/srv/rsyncd")
                      (comment "Rsync share")
                      (read-only? #f))))          ;yes, that was the default

(define (rsync-configuration-modules config)
  (match-record config <rsync-configuration>
    (modules
     share-path share-comment use-chroot? read-only? timeout) ;deprecated
    (if (unspecified? share-path)
        (rsync-configuration-actual-modules config)
        (list (rsync-module                       ;backward compatibility
               (name "files")
               (file-name share-path)
               (comment "Rsync share")
               (chroot?
                (if (unspecified? use-chroot?) #t use-chroot?))
               (read-only?
                (if (unspecified? read-only?) #f read-only?))
               (timeout
                (if (unspecified? timeout) 300 timeout)))))))

(define (rsync-account config)
  "Return the user accounts and user groups for CONFIG."
  (let ((rsync-user (if (rsync-configuration-uid config)
                        (rsync-configuration-uid config)
                        (rsync-configuration-user config)))
        (rsync-group (if (rsync-configuration-gid config)
                         (rsync-configuration-gid config)
                         (rsync-configuration-group config))))
    (list (user-group (name rsync-group) (system? #t))
          (user-account
           (name rsync-user)
           (system? #t)
           (group rsync-group)
           (comment "rsyncd privilege separation user")
           (home-directory (string-append "/var/run/"
                                          rsync-user))
           (shell (file-append shadow "/sbin/nologin"))))))

(define (rsync-activation config)
  "Return the activation GEXP for CONFIG."
  (with-imported-modules '((guix build utils))
    #~(begin
        (let ((user  (getpw (if #$(rsync-configuration-uid config)
                                #$(rsync-configuration-uid config)
                                #$(rsync-configuration-user config))))
              (group (getpw (if #$(rsync-configuration-gid config)
                                #$(rsync-configuration-gid config)
                                #$(rsync-configuration-group config)))))
          (mkdir-p (dirname #$(rsync-configuration-pid-file config)))
          (for-each (lambda (directory)
                      (mkdir-p directory)
                      (chown directory (passwd:uid user) (group:gid group)))
                    '#$(map rsync-module-file-name
                            (rsync-configuration-modules config)))))))

(define (rsync-config-file config)
  ;; Return the rsync configuration file corresponding to CONFIG.
  (define (module-config module)
    (match-record module <rsync-module>
      (name file-name comment chroot? read-only? timeout)
      (list "[" name "]\n"
            "  path = " file-name "\n"
            "  use chroot = " (if chroot? "true" "false") "\n"
            "  comment = " comment "\n"
            "  read only = " (if read-only? "true" "false") "\n"
            "  timeout = " (number->string timeout) "\n")))

  (define modules
    (rsync-configuration-modules config))

  (match-record config <rsync-configuration>
    (package address port-number pid-file lock-file log-file
             user group uid gid)
    (unless (string=? user "root")
      (cond
       ((<= port-number 1024)
        (error (string-append "rsync-service: to run on port "
                              (number->string port-number)
                              ", user must be root.")))
       ((find rsync-module-chroot? modules)
        (error (string-append "rsync-service: to run in a chroot"
                              ", user must be root.")))
       (uid
        (error "rsync-service: to use uid, user must be root."))
       (gid
        (error "rsync-service: to use gid, user must be root."))))

    (apply mixed-text-file "rsync.conf"
           "# Generated by 'rsync-service'.\n\n"
           "pid file = " pid-file "\n"
           "lock file = " lock-file "\n"
           "log file = " log-file "\n"
           (if address (string-append "address = " address "\n") "")
           "port = " (number->string port-number) "\n"
           (if uid (string-append "uid = " uid "\n") "")
           "gid = " (if gid gid "nogroup") "\n"   ; no group nobody
           "\n\n"
           (append-map module-config modules))))

(define (rsync-shepherd-service config)
  "Return a <shepherd-service> for rsync with CONFIG."

  ;; XXX: Predicates copied from (gnu services ssh).
  (define inetd-style?
    #~(and (defined? 'make-inetd-constructor)
           (not (string=? (@ (shepherd config) Version) "0.9.0"))))

  (define ipv6-support?
    #~(catch 'system-error
        (lambda ()
          (let ((sock (socket AF_INET6 SOCK_STREAM 0)))
            (close-port sock)
            #t))
        (const #f)))

  (define (module->file-system-mapping module)
    "Return the <file-system-mapping> record corresponding to MODULE, an
<rsync-module> object."
    (match-record module <rsync-module>
      (file-name read-only?)
      (file-system-mapping
       (source file-name)
       (target source)
       (writable? (not read-only?)))))

  (match-record config <rsync-configuration>
    (package log-file modules pid-file port-number user group)
    ;; Run the rsync daemon in its own 'mnt' namespace, to guard against
    ;; change to mount points it may be serving.
    (let* ((config-file (rsync-config-file config))
           (rsync-command #~(list #$(least-authority-wrapper
                                     (file-append rsync "/bin/rsync")
                                     #:name "rsync"
                                     #:namespaces (fold delq %namespaces
                                                        '(net user))
                                     #:mappings
                                     (append (list (file-system-mapping
                                                    (source "/var/run/rsyncd")
                                                    (target source)
                                                    (writable? #t))
                                                   (file-system-mapping
                                                    (source (dirname log-file))
                                                    (target source)
                                                    (writable? #t))
                                                   (file-system-mapping
                                                    (source config-file)
                                                    (target source)))
                                             (map module->file-system-mapping
                                                  modules)))
                                  "--config" #$config-file "--daemon")))
      (list (shepherd-service
             (provision '(rsync))
             (documentation "Run rsync daemon.")
             (actions (list (shepherd-configuration-action config-file)))
             (start #~(if #$inetd-style?
                          (make-inetd-constructor
                           #$rsync-command
                           (cons (endpoint
                                  (make-socket-address AF_INET INADDR_ANY
                                                       #$port-number))
                                 (if #$ipv6-support?
                                     (list
                                      (endpoint
                                       (make-socket-address AF_INET6 IN6ADDR_ANY
                                                            #$port-number)))
                                     '()))
                           #:service-name-stem "rsync"
                           #:user #$user
                           #:group #$group)
                          (make-forkexec-constructor #$rsync-command
                                                     #:pid-file #$pid-file
                                                     #:user #$user
                                                     #:group #$group)))
             (stop #~(if #$inetd-style?
                         (make-inetd-destructor)
                         (make-kill-destructor))))))))

(define rsync-service-type
  (service-type
   (name 'rsync)
   (extensions
    (list (service-extension shepherd-root-service-type rsync-shepherd-service)
          (service-extension account-service-type rsync-account)
          (service-extension activation-service-type rsync-activation)))
   (default-value (rsync-configuration))
   (description
    "Run the rsync file copying tool in daemon mode.  This allows remote hosts
to keep synchronized copies of the files exported by rsync.")))
