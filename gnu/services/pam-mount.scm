;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2023 Brian Cully <bjc@spork.org>
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

(define-module (gnu services pam-mount)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu system pam)
  #:use-module (guix gexp)
  #:use-module (guix records)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (pam-mount-configuration
            pam-mount-configuration?
            pam-mount-service-type

            pam-mount-volume
            pam-mount-volume?
            pam-mount-volume-service-type))

(define %pam-mount-default-configuration
  `((debug (@ (enable "0")))
    (mntoptions (@ (allow ,(string-join
                            '("nosuid" "nodev" "loop"
                              "encryption" "fsck" "nonempty"
                              "allow_root" "allow_other")
                            ","))))
    (mntoptions (@ (require "nosuid,nodev")))
    (logout (@ (wait "0")
               (hup "0")
               (term "no")
               (kill "no")))
    (mkmountpoint (@ (enable "1")
                     (remove "true")))))

(define (make-pam-mount-configuration-file config)
  (computed-file
   "pam_mount.conf.xml"
   #~(begin
       (use-modules (sxml simple))
       (call-with-output-file #$output
         (lambda (port)
           (sxml->xml
            '(*TOP*
              (*PI* xml "version='1.0' encoding='utf-8'")
              (pam_mount
               #$@(pam-mount-configuration-rules config)
               (pmvarrun
                #$(file-append pam-mount
                               "/sbin/pmvarrun -u '%(USER)' -o '%(OPERATION)'"))
               (cryptmount
                #$(file-append pam-mount
                               (string-append
                                "/sbin/mount.crypt"
                                " '%(if %(CIPHER),-ocipher=%(CIPHER))'"
                                " '%(if %(FSKEYCIPHER),"
                                "-ofsk_cipher=%(FSKEYCIPHER))'"
                                " '%(if %(FSKEYHASH),-ofsk_hash=%(FSKEYHASH))'"
                                " '%(if %(FSKEYPATH),-okeyfile=%(FSKEYPATH))'"
                                " '%(if %(OPTIONS),-o%(OPTIONS))'"
                                " '%(VOLUME)' '%(MNTPT)'")))
               (cryptumount
                #$(file-append pam-mount "/sbin/umount.crypt '%(MNTPT)'"))))
            port))))))

(define-record-type* <pam-mount-configuration>
  pam-mount-configuration
  make-pam-mount-configuration
  pam-mount-configuration?
  (rules pam-mount-configuration-rules
         (default %pam-mount-default-configuration)))

(define (pam-mount-etc-service config)
  `(("security/pam_mount.conf.xml"
     ,(make-pam-mount-configuration-file config))))

(define (pam-mount-pam-service config)
  (define optional-pam-mount
    (pam-entry
     (control "optional")
     (module (file-append pam-mount "/lib/security/pam_mount.so"))))
  (list
   (pam-extension
    (transformer
     (lambda (pam)
       (if (member (pam-service-name pam)
                   '("login" "greetd" "su" "slim" "gdm-password" "sddm"))
           (pam-service
            (inherit pam)
            (auth (append (pam-service-auth pam)
                          (list optional-pam-mount)))
            (session (append (pam-service-session pam)
                             (list optional-pam-mount))))
           pam))))))

(define (extend-pam-mount-configuration initial extensions)
  "Extends INITIAL with EXTENSIONS."
  (pam-mount-configuration (rules (append (pam-mount-configuration-rules
                                           initial) extensions))))

(define pam-mount-service-type
  (service-type
   (name 'pam-mount)
   (extensions (list (service-extension etc-service-type
                                        pam-mount-etc-service)
                     (service-extension pam-root-service-type
                                        pam-mount-pam-service)))
   (compose concatenate)
   (extend extend-pam-mount-configuration)
   (default-value (pam-mount-configuration))
   (description "Activate PAM-Mount support.  It allows mounting volumes for
specific users when they log in.")))

(define (field-name->tag field-name)
  "Convert FIELD-NAME to its tag used by the configuration XML."
  (match field-name
    ('user-name 'user)
    ('user-id 'uid)
    ('primary-group 'pgrp)
    ('group-id 'gid)
    ('secondary-group 'sgrp)
    ('file-system-type 'fstype)
    ('no-mount-as-root? 'noroot)
    ('file-name 'path)
    ('mount-point 'mountpoint)
    ('ssh? 'ssh)
    ('file-system-key-cipher 'fskeycipher)
    ('file-system-key-hash 'fskeyhash)
    ('file-system-key-file-name 'fskeypath)
    (_ field-name)))

(define-maybe string)

(define (serialize-string field-name value)
  (list (field-name->tag field-name) value))

(define (integer-or-range? value)
  (match value
    ((start . end) (and (integer? start)
                        (integer? end)))
    (_ (number? value))))

(define-maybe integer-or-range)

(define (serialize-integer-or-range field-name value)
  (let ((value-string (match value
                        ((start . end) (format #f "~a-~a" start end))
                        (_ (number->string value)))))
    (list (field-name->tag field-name) value-string)))

(define-maybe boolean)

(define (serialize-boolean field-name value)
  (let ((value-string (if value "1" "0")))
    (list (field-name->tag field-name) value-string)))

(define-configuration pam-mount-volume
  (user-name maybe-string "User name to match.")
  (user-id maybe-integer-or-range
   "User ID, or range of user IDs, in the form of @code{(start . end)} to\nmatch.")
  (primary-group maybe-string "Primary group name to match.")
  (group-id maybe-integer-or-range
   "Group ID, or range of group IDs, in the form of @code{(start . end)} to\nmatch.")
  (secondary-group maybe-string
   "Match users who belong to this group name as either a primary or secondary\ngroup.")
  (file-system-type maybe-string "File system type of volume being mounted.")
  (no-mount-as-root? maybe-boolean
                     "Do not use super user privileges to mount this volume.")
  (server maybe-string "Remote server this volume resides on.")
  (file-name maybe-string "Location of the volume to be mounted.")
  (mount-point maybe-string
               "Where to mount the volume in the local file system.")
  (options maybe-string "Options to pass to the underlying mount program.")
  (ssh? maybe-boolean "Whether to pass the login password to SSH.")
  (cipher maybe-string "Cryptsetup cipher named used by volume.")
  (file-system-key-cipher maybe-string
                          "Cipher name used by the target volume.")
  (file-system-key-hash maybe-string
                        "SSL hash name used by the target volume.")
  (file-system-key-file-name maybe-string
   "File name for the file system key used by the target volume."))

(define (pam-mount-volume->sxml volume)
  ;; Convert a list of configuration fields into an SXML-compatible attribute
  ;; list.
  (define xml-attrs
    (filter-map (lambda (field)
                  (let* ((accessor (configuration-field-getter field))
                         (value (accessor volume)))
                    (and (not (eq? value %unset-value))
                         (list (field-name->tag (configuration-field-name
                                                 field)) value))))
                pam-mount-volume-fields))

  `(volume (@ ,@xml-attrs)))

(define (pam-mount-volume-rules volumes)
  (map pam-mount-volume->sxml volumes))

(define pam-mount-volume-service-type
  (service-type (name 'pam-mount-volume)
                (extensions (list (service-extension pam-mount-service-type
                                                     pam-mount-volume-rules)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description
                 "Mount remote volumes such as CIFS shares @i{via}
@acronym{PAM, Pluggable Authentication Modules} when logging in, using login
credentials.")))
