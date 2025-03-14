;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2017, 2019-2021 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2023 Josselin Poiret <dev@jpoiret.xyz>
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

(define-module (gnu system pam)
  #:use-module (guix records)
  #:use-module (guix derivations)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system setuid)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module ((guix utils) #:select (%current-system))
  #:use-module (gnu packages linux)
  #:export (pam-service
            pam-service?
            pam-service-name
            pam-service-account
            pam-service-auth
            pam-service-password
            pam-service-session

            pam-entry
            pam-entry?
            pam-entry-control
            pam-entry-module
            pam-entry-arguments

            pam-limits-entry
            pam-limits-entry?
            pam-limits-entry-domain
            pam-limits-entry-type
            pam-limits-entry-item
            pam-limits-entry-value
            pam-limits-entry->string

            pam-services->directory
            unix-pam-service
            base-pam-services

            session-environment-service
            session-environment-service-type

            pam-extension
            pam-extension-transformer
            pam-extension-shepherd-requirements

            pam-root-service-type
            pam-root-service))

;;; Commentary:
;;;
;;; Configuration of the pluggable authentication modules (PAM).
;;;
;;; Code:

;; PAM services (see
;; <http://www.linux-pam.org/Linux-PAM-html/sag-configuration-file.html>.)
(define-record-type* <pam-service> pam-service
  make-pam-service
  pam-service?
  (name       pam-service-name)                   ; string

  ;; The four "management groups".
  (account    pam-service-account                 ; list of <pam-entry>
              (default '()))
  (auth       pam-service-auth
              (default '()))
  (password   pam-service-password
              (default '()))
  (session    pam-service-session
              (default '())))

(define-record-type* <pam-entry> pam-entry
  make-pam-entry
  pam-entry?
  (control    pam-entry-control)         ; string
  (module     pam-entry-module)          ; file name
  (arguments  pam-entry-arguments        ; list of string-valued g-expressions
              (default '())))

;; PAM limits entries are used by the pam_limits PAM module to set or override
;; limits on system resources for user sessions.  The format is specified
;; here: http://linux-pam.org/Linux-PAM-html/sag-pam_limits.html
(define-record-type <pam-limits-entry>
  (make-pam-limits-entry domain type item value)
  pam-limits-entry?
  (domain     pam-limits-entry-domain)   ; string
  (type       pam-limits-entry-type)     ; symbol
  (item       pam-limits-entry-item)     ; symbol
  (value      pam-limits-entry-value))   ; symbol or number

(define (pam-limits-entry domain type item value)
  "Construct a pam-limits-entry ensuring that the provided values are valid."
  (define (valid? value)
    (case item
      ((priority) (number? value))
      ((nice)     (and (number? value)
                       (>= value -20)
                       (<= value 19)))
      (else       (or (and (number? value)
                           (>= value -1))
                      (member value '(unlimited infinity))))))
  (define items
    (list 'core      'data       'fsize
          'memlock   'nofile     'rss
          'stack     'cpu        'nproc
          'as        'maxlogins  'maxsyslogins
          'priority  'locks      'sigpending
          'msgqueue  'nice       'rtprio))
  (when (not (member type '(hard soft both)))
    (error "invalid limit type" type))
  (when (not (member item items))
    (error "invalid limit item" item))
  (when (not (valid? value))
    (error "invalid limit value" value))
  (make-pam-limits-entry domain type item value))

(define (pam-limits-entry->string entry)
  "Convert a pam-limits-entry record to a string."
  (match entry
    (($ <pam-limits-entry> domain type item value)
     (string-join (list domain
                        (if (eq? type 'both)
                            "-"
                            (symbol->string type))
                        (symbol->string item)
                        (cond
                         ((symbol? value)
                          (symbol->string value))
                         (else
                          (number->string value))))
                  "	"))))

(define (pam-service->configuration service)
  "Return the derivation building the configuration file for SERVICE, to be
dumped in /etc/pam.d/NAME, where NAME is the name of SERVICE."
  (define (entry->gexp type entry)
    (match entry
      (($ <pam-entry> control module (arguments ...))
       #~(format #t "~a ~a ~a ~a~%"
                 #$type #$control #$module
                 (string-join (list #$@arguments))))))

  (match service
    (($ <pam-service> name account auth password session)
     (define builder
       #~(begin
           (with-output-to-file #$output
             (lambda ()
               #$@(append (map (cut entry->gexp "account" <>) account)
                          (map (cut entry->gexp "auth" <>) auth)
                          (map (cut entry->gexp "password" <>) password)
                          (map (cut entry->gexp "session" <>) session))
               #t))))

     (computed-file name builder))))

(define (pam-services->directory services)
  "Return the derivation to build the configuration directory to be used as
/etc/pam.d for SERVICES."
  (let ((names (map pam-service-name services))
        (files (map pam-service->configuration services)))
    (define builder
      #~(begin
          (use-modules (ice-9 match)
                       (srfi srfi-1))

          (mkdir #$output)
          (for-each (match-lambda
                      ((name file)
                       (symlink file (string-append #$output "/" name))))

                    ;; Since <pam-service> objects cannot be compared with
                    ;; 'equal?' since they contain gexps, which contain
                    ;; closures, use 'delete-duplicates' on the build-side
                    ;; instead.  See <http://bugs.gnu.org/20037>.
                    (delete-duplicates '#$(zip names files)))))

    (computed-file "pam.d" builder)))

(define %pam-other-services
  ;; The "other" PAM configuration, which denies everything (see
  ;; <http://www.linux-pam.org/Linux-PAM-html/sag-configuration-example.html>.)
  (let ((deny (pam-entry
               (control "required")
               (module "pam_deny.so"))))
    (pam-service
     (name "other")
     (account (list deny))
     (auth (list deny))
     (password (list deny))
     (session (list deny)))))

(define unix-pam-service
  (let ((unix (pam-entry
               (control "required")
               (module "pam_unix.so")))
        (env  (pam-entry ; to honor /etc/environment.
               (control "required")
               (module "pam_env.so"))))
    (lambda* (name #:key allow-empty-passwords? allow-root? motd
              login-uid? gnupg?)
      "Return a standard Unix-style PAM service for NAME.  When
ALLOW-EMPTY-PASSWORDS? is true, allow empty passwords.  When ALLOW-ROOT? is
true, allow root to run the command without authentication.  When MOTD is
true, it should be a file-like object used as the message-of-the-day.
When LOGIN-UID? is true, require the 'pam_loginuid' module; that module sets
/proc/self/loginuid, which the libc 'getlogin' function relies on.  When
GNUPG? is true, require the 'pam_gnupg.so' module; that module hands over
the login password to 'gpg-agent'."
      ;; See <http://www.linux-pam.org/Linux-PAM-html/sag-configuration-example.html>.
      (pam-service
       (name name)
       (account (list unix))
       (auth (append (if allow-root?
                         (list (pam-entry
                                (control "sufficient")
                                (module "pam_rootok.so")))
                         '())
                     (list (if allow-empty-passwords?
                               (pam-entry
                                (control "required")
                                (module "pam_unix.so")
                                (arguments '("nullok")))
                               unix))
                     (if gnupg?
                         (list (pam-entry
                                (control "required")
                                (module (file-append pam-gnupg "/lib/security/pam_gnupg.so"))))
                         '())))
       (password (list (pam-entry
                        (control "required")
                        (module "pam_unix.so")
                        ;; Store SHA-512 encrypted passwords in /etc/shadow.
                        (arguments '("sha512" "shadow")))))
       (session `(,@(if motd
                        (list (pam-entry
                               (control "optional")
                               (module "pam_motd.so")
                               (arguments
                                (list #~(string-append "motd=" #$motd)))))
                        '())
                  ,@(if login-uid?
                        (list (pam-entry       ;to fill in /proc/self/loginuid
                               (control "required")
                               (module "pam_loginuid.so")))
                        '())
                  ,@(if gnupg?
                        (list (pam-entry
                               (control "required")
                               (module (file-append pam-gnupg "/lib/security/pam_gnupg.so"))))
                        '())
                  ,env ,unix))))))

(define (rootok-pam-service command)
  "Return a PAM service for COMMAND such that 'root' does not need to
authenticate to run COMMAND."
  (let ((unix (pam-entry
               (control "required")
               (module "pam_unix.so"))))
    (pam-service
     (name command)
     (account (list unix))
     (auth (list (pam-entry
                  (control "sufficient")
                  (module "pam_rootok.so"))))
     (password (list unix))
     (session (list unix)))))

(define* (base-pam-services #:key allow-empty-passwords?)
  "Return the list of basic PAM services everyone would want."
  ;; TODO: Add other Shadow programs?
  (append (list %pam-other-services)

          ;; These programs are setuid-root.
          (map (cut unix-pam-service <>
                    #:allow-empty-passwords? allow-empty-passwords?)
               '("passwd" "chfn" "sudo"))
          ;; This is setuid-root, as well.  Allow root to run "su" without
          ;; authenticating.
          (list (unix-pam-service "su"
                                  #:allow-empty-passwords? allow-empty-passwords?
                                  #:allow-root? #t))

          ;; These programs are not setuid-root, and we want root to be able
          ;; to run them without having to authenticate (notably because
          ;; 'useradd' and 'groupadd' are run during system activation.)
          (map rootok-pam-service
               '("useradd" "userdel" "usermod"
                 "groupadd" "groupdel" "groupmod"))))


;;;
;;; System-wide environment variables.
;;;

(define (environment-variables->environment-file vars)
  "Return a file for pam_env(8) that contains environment variables VARS."
  (apply mixed-text-file "environment"
         (append-map (match-lambda
                       ((key . value)
                        (list key "=" value "\n")))
                     vars)))

(define session-environment-service-type
  (service-type
   (name 'session-environment)
   (extensions
    (list (service-extension
           etc-service-type
           (lambda (vars)
             (list `("environment"
                     ,(environment-variables->environment-file vars)))))))
   (compose concatenate)
   (extend append)
   (description
    "Populate @file{/etc/environment}, which is honored by @code{pam_env},
with the specified environment variables.  The value of this service is a list
of name/value pairs for environments variables, such as:

@example
'((\"TZ\" . \"Canada/Pacific\"))
@end example\n")))

(define (session-environment-service vars)
  "Return a service that builds the @file{/etc/environment}, which can be read
by PAM-aware applications to set environment variables for sessions.

VARS should be an association list in which both the keys and the values are
strings or string-valued gexps."
  (service session-environment-service-type vars))



;;;
;;; PAM root service.
;;;

;; Extension of the PAM configuration.  A PAM transformer consists of a
;; procedure acting on each PAM entry; 'shepherd-requirements' lists services
;; that the meta 'pam' Shepherd service will depend on.
(define-record-type* <pam-extension>
  pam-extension make-pam-extension pam-extension?
  (transformer pam-extension-transformer)
  (shepherd-requirements pam-extension-shepherd-requirements
                         (default '())))

;; Overall PAM configuration: a list of services, plus a procedure that takes
;; one <pam-service> and returns a <pam-service>.  The procedure is used to
;; implement cross-cutting concerns such as the use of the 'elogind.so'
;; session module that keeps track of logged-in users.
(define-record-type* <pam-configuration>
  pam-configuration make-pam-configuration pam-configuration?
  ;list of <pam-service>
  (services  pam-configuration-services)
  ;list of procedures <pam-entry> -> <pam-entry>
  (transformers pam-configuration-transformers)
  ;list of symbols
  (shepherd-requirements pam-configuration-shepherd-requirements))

(define (/etc-entry config)
  "Return the /etc/pam.d entry corresponding to CONFIG."
  (match config
    (($ <pam-configuration> services transformers shepherd-requirements)
     (let ((services (map (apply compose identity transformers)
                          services)))
       `(("pam.d" ,(pam-services->directory services)))))))

(define (pam-shepherd-service config)
  "Return the PAM synchronization shepherd service corresponding to CONFIG."
  (match config
    (($ <pam-configuration> services transformers shepherd-requirements)
     (list (shepherd-service
            (documentation "Synchronization point for services that need to be
started for PAM to work.")
            (provision '(pam))
            (requirement shepherd-requirements)
            (start #~(const #t))
            (stop #~(const #f)))))))

(define (extend-configuration initial extensions)
  "Extend INITIAL with NEW."
  ;; TODO: Remove deprecation shim.
  (define cleaned-extensions
    (map (lambda (ext)
           (if (procedure? ext)
               (begin
                 (warning (G_ "'pam-root-service-type' extensions should \
now use the <pam-extension> record~%"))
                 (pam-extension (transformer ext)))
               ext))
         extensions))

  (let-values (((services pam-extensions)
                (partition pam-service? cleaned-extensions)))
    (pam-configuration
     (services (append (pam-configuration-services initial)
                       services))
     (transformers (append (pam-configuration-transformers initial)
                           (map pam-extension-transformer pam-extensions)))
     (shepherd-requirements
      (append (pam-configuration-shepherd-requirements initial)
              (append-map pam-extension-shepherd-requirements pam-extensions))))))

(define pam-root-service-type
  (service-type (name 'pam)
                (extensions
                 (list (service-extension
                        setuid-program-service-type
                        (lambda (_)
                          (list (file-like->setuid-program
                                 (file-append linux-pam "/sbin/unix_chkpwd")))))
                       (service-extension etc-service-type /etc-entry)
                       (service-extension shepherd-root-service-type
                                          pam-shepherd-service)))

                ;; Arguments include <pam-service> as well as procedures.
                (compose concatenate)
                (extend extend-configuration)
                (description
                 "Configure the Pluggable Authentication Modules (PAM) for all
the specified @dfn{PAM services}.  Each PAM service corresponds to a program,
such as @command{login} or @command{sshd}, and specifies for instance how the
program may authenticate users or what it should do when opening a new
session.")))

(define* (pam-root-service base #:key (transformers '()) (shepherd-requirements '()))
  "The \"root\" PAM service, which collects <pam-service> instance and turns
them into a /etc/pam.d directory, including the <pam-service> listed in BASE.
TRANSFORM is a procedure that takes a <pam-service> and returns a
<pam-service>.  It can be used to implement cross-cutting concerns that affect
all the PAM services."
  (service pam-root-service-type
           (pam-configuration (services base)
                              (transformers transformers)
                              (shepherd-requirements shepherd-requirements))))


