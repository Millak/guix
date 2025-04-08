;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Tanguy Le Carrour <tanguy@bioneland.org>
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

(define-module (gnu home services mail)
  #:use-module (guix gexp)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu home services)
  #:use-module (gnu home services shepherd)
  #:use-module (gnu packages mail)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (home-msmtp-configuration
            home-msmtp-configuration?
            home-msmtp-configuration-defaults
            home-msmtp-configuration-accounts
            home-msmtp-configuration-default-account
            home-msmtp-configuration-extra-content
            home-msmtp-service-type
            msmtp-configuration
            msmtp-configuration-auth?
            msmtp-configuration-tls?
            msmtp-configuration-tls-starttls?
            msmtp-configuration-tls-trust-file
            msmtp-configuration-log-file
            msmtp-configuration-host
            msmtp-configuration-port
            msmtp-configuration-user
            msmtp-configuration-from
            msmtp-configuration-password-eval
            msmtp-configuration-extra-content
            msmtp-account
            msmtp-account-name
            msmtp-account-configuration))

(define (string-or-gexp? obj)
  (or (string? obj)
      (gexp? obj)))

(define-maybe string (prefix msmtp-configuration-))
(define-maybe boolean (prefix msmtp-configuration-))
(define-maybe integer (prefix msmtp-configuration-))
(define-maybe string-or-gexp (prefix msmtp-configuration-))

;; Serialization of 'msmtp'.
(define (uglify-symbol field-name)
  (let* ((name (symbol->string field-name))
         (ugly-name (string-replace-substring name "-" "_")))
    (if (string-suffix? "?" ugly-name)
      (string-drop-right ugly-name 1)
      ugly-name)))

(define (msmtp-configuration-serialize-boolean field-name value)
  #~(string-append #$(uglify-symbol field-name) " "
                   (if #$value "on" "off") "\n"))

(define (msmtp-configuration-serialize-string field-name value)
  #~(string-append #$(uglify-symbol field-name) " " #$value "\n"))

(define msmtp-configuration-serialize-string-or-gexp
  msmtp-configuration-serialize-string)

(define (msmtp-configuration-serialize-maybe-string-no-underscore field-name value)
  #~(if #$(maybe-value-set? value)
      (string-append
        #$(string-replace-substring (uglify-symbol field-name) "_" "") " " #$value "\n")
      ""))

(define (msmtp-configuration-serialize-integer field-name value)
  #~(string-append #$(uglify-symbol field-name) " "
                   (number->string #$value) "\n"))

(define (msmtp-configuration-serialize-extra-content field-name value)
  #~(if (string=? #$value "") "" (string-append #$value "\n")))

(define (msmtp-account-serialize-name field-name value)
  #~(string-append "\naccount " #$value "\n"))

(define (msmtp-account-serialize-msmtp-configuration field-name value)
  #~(string-append #$(serialize-configuration value msmtp-configuration-fields)))

(define (home-msmtp-configuration-serialize-list-of-msmtp-accounts field-name value)
  #~(string-append #$@(map (cut serialize-configuration <> msmtp-account-fields)
                           value)))

(define (home-msmtp-configuration-serialize-msmtp-configuration field-name value)
  #~(string-append "defaults\n"
                   #$(serialize-configuration value msmtp-configuration-fields)))

(define (home-msmtp-configuration-serialize-default-account field-name value)
  #~(if #$(maybe-value-set? value)
      (string-append "\naccount default : " #$value "\n")
      ""))

(define (home-msmtp-configuration-serialize-extra-content field-name value)
  #~(if (string=? #$value "") "" (string-append #$value "\n")))

;; Configuration of 'msmtp'.
;; Source <https://marlam.de/msmtp/msmtp.html#Configuration-files>.
(define-configuration msmtp-configuration
  (auth?
   maybe-boolean
   "Enable or disable authentication.")

  (tls?
   maybe-boolean
   "Enable or disable TLS (also known as SSL) for secured connections.")

  (tls-starttls?
   maybe-boolean
   "Choose the TLS variant: start TLS from within the session (‘on’, default),
or tunnel the session through TLS (‘off’).")

  (tls-trust-file
   maybe-string
   "Activate server certificate verification using a list of
trusted Certification Authorities (CAs).")

  (log-file
   maybe-string
   "Enable logging to the specified file. An empty argument disables logging.
The file name ‘-’ directs the log information to standard output."
   (serializer msmtp-configuration-serialize-maybe-string-no-underscore))

  (host
    maybe-string
    "The SMTP server to send the mail to.")

  (port
    maybe-integer
    "The port that the SMTP server listens on. The default is 25 (\"smtp\"),
unless TLS without STARTTLS is used, in which case it is 465 (\"smtps\").")

  (user
    maybe-string
    "Set the user name for authentication.")

  (from
    maybe-string
    "Set the envelope-from address.")

  (password-eval
    maybe-string-or-gexp
    "Set the password for authentication to the output (stdout) of the command cmd."
    (serializer msmtp-configuration-serialize-maybe-string-no-underscore))

  (extra-content
   (string "")
   "Extra content appended as-is to the configuration block.  Run
@command{man msmtp} for more information about the configuration file
format."
   (serializer msmtp-configuration-serialize-extra-content))

  (prefix msmtp-configuration-))

(define-configuration msmtp-account
  (name
   (string)
   "The unique name of the account."
   (serializer msmtp-account-serialize-name))

  (configuration
   (msmtp-configuration)
   "The configuration for this given account.")

  (prefix msmtp-account-))

(define (list-of-msmtp-accounts? lst)
  (every msmtp-account? lst))

(define-configuration home-msmtp-configuration
  (defaults
   (msmtp-configuration (msmtp-configuration))
   "The configuration that will be set as default for all accounts.")

  (accounts
   (list-of-msmtp-accounts '())
   "A list of @code{msmtp-account} records which contain
information about all your accounts.")

  (default-account
   maybe-string
   "Set the default account."
   (serializer home-msmtp-configuration-serialize-default-account))

  (extra-content
   (string "")
   "Extra content appended as-is to the configuration file.  Run
@command{man msmtp} for more information about the configuration file
format."
   (serializer home-msmtp-configuration-serialize-extra-content))

  (prefix home-msmtp-configuration-))

(define (home-msmtp-files config)
  (list
   `(".config/msmtp/config"
     ,(mixed-text-file "msmtp-config"
                       (serialize-configuration config home-msmtp-configuration-fields)))))

(define (home-msmtp-profile-entries config)
  (list msmtp))

(define home-msmtp-service-type
  (service-type (name 'home-msmtp)
                (extensions
                 (list (service-extension home-profile-service-type
                                          home-msmtp-profile-entries)
                       (service-extension home-files-service-type
                                          home-msmtp-files)))
                (default-value (home-msmtp-configuration))
                (description "Configure msmtp, a simple
@acronym{SMTP, Simple Mail Transfer Protocol} client that can relay email
to SMTP servers.")))
