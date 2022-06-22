;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2021, 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

;;; Commentary:
;;;
;;; This module contains helpers used as part of the jami-service-type
;;; definition.
;;;
;;; Code:

(define-module (gnu build jami-service)
  #:use-module (gnu build dbus-service)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:export (jami-service-available?

            account-fingerprint?
            account-details->recutil
            get-accounts
            get-usernames
            set-account-details
            add-account
            account->username
            username->account
            username->contacts
            enable-account
            disable-account

            add-contact
            remove-contact

            set-all-moderators
            set-moderator
            username->all-moderators?
            username->moderators))

;;;
;;; Utilities.
;;;

(define (alist->list alist)
  "Flatten ALIST into a list."
  (append-map (match-lambda
                (() '())
                ((key . value)
                 (list key value)))
              alist))

(define account-fingerprint-rx (make-regexp "[0-9A-Fa-f]{40}"))

(define (account-fingerprint? val)
  "A Jami account fingerprint is 40 characters long and only contains
hexadecimal characters."
  (and (string? val)
       (regexp-exec account-fingerprint-rx val)))

(define (validate-fingerprint fingerprint)
  "Validate that fingerprint is 40 characters long."
  (unless (account-fingerprint? fingerprint)
    (error "Account fingerprint is not valid:" fingerprint)))

(define (jami-service-available?)
  "Whether the Jami D-Bus service was acquired by the D-Bus daemon."
  (unless (%current-dbus-connection)
    (initialize-dbus-connection!))
  (dbus-service-available? "cx.ring.Ring"))


;;;
;;; Bindings for the Jami D-Bus API.
;;;

(define* (call-configuration-manager-method method #:optional arguments
                                            #:key timeout)
  "Query the Jami D-Bus ConfigurationManager interface with METHOD applied to
ARGUMENTS.  TIMEOUT can optionally be provided as a value in seconds."
  (unless (%current-dbus-connection)
    (initialize-dbus-connection!))
  (call-dbus-method method
                    #:path "/cx/ring/Ring/ConfigurationManager"
                    #:destination "cx.ring.Ring"
                    #:interface "cx.ring.Ring.ConfigurationManager"
                    #:arguments arguments
                    #:timeout timeout))

;;; The following methods are for internal use; they make use of the account
;;; ID, an implementation detail of Jami the user should not need to be
;;; concerned with.
(define (get-account-ids)
  "Return the available Jami account identifiers (IDs).  Account IDs are an
implementation detail used to identify the accounts in Jami."
  (vector->list (call-configuration-manager-method "getAccountList")))

(define (id->account-details id)
  "Retrieve the account data associated with the given account ID."
  (vector->list (call-configuration-manager-method "getAccountDetails"
                                                   (list id))))

(define (id->volatile-account-details id)
  "Retrieve the account data associated with the given account ID."
  (vector->list (call-configuration-manager-method "getVolatileAccountDetails"
                                                   (list id))))

(define (id->account id)
  "Retrieve the complete account data associated with the given account ID."
  (append (id->volatile-account-details id)
          (id->account-details id)))

(define %username-to-id-cache #f)

(define (invalidate-username-to-id-cache!)
  (set! %username-to-id-cache #f))

(define (username->id username)
  "Return the first account ID corresponding to USERNAME."
  (unless (assoc-ref %username-to-id-cache username)
    (set! %username-to-id-cache
          (append-map
           (lambda (id)
             (let* ((account (id->account id))
                    (username (assoc-ref account "Account.username"))
                    (registered-name (assoc-ref account
                                                "Account.registeredName")))
               `(,@(if username
                       (list (cons username id))
                       '())
                 ,@(if registered-name
                       (list (cons registered-name id))
                       '()))))
           (get-account-ids))))
  (or (assoc-ref %username-to-id-cache username)
      (let ((message (format #f "no account ID for ~:[username~;fingerprint~]"
                             (account-fingerprint? username))))
        (error message username))))

(define (account->username account)
  "Return USERNAME, the registered username associated with ACCOUNT, else its
public key fingerprint."
  (or (assoc-ref account "Account.registeredName")
      (assoc-ref account "Account.username")))

(define (id->username id)
  "Return USERNAME, the registered username associated with ID, else its
public key fingerprint, else #f."
  (account->username (id->account id)))

(define (get-accounts)
  "Return the list of all accounts, as a list of alists."
  (map id->account (get-account-ids)))

(define (get-usernames)
  "Return the list of the usernames associated with the present accounts."
  (map account->username (get-accounts)))

(define (username->account username)
  "Return the first account associated with USERNAME, else #f.
USERNAME can be either the account 40 characters public key fingerprint or a
registered username."
  (find (lambda (account)
          (member username
                  (list (assoc-ref account "Account.username")
                        (assoc-ref account "Account.registeredName"))))
        (get-accounts)))

(define (add-account archive)
  "Import the Jami account ARCHIVE and return its account ID.  The archive
should *not* be encrypted with a password.  Return the username associated
with the account."
  (invalidate-username-to-id-cache!)
  (let ((id (call-configuration-manager-method
             "addAccount" (list `#(("Account.archivePath" . ,archive)
                                   ("Account.type" . "RING"))))))
    ;; The account information takes some time to be populated.
    (with-retries 20 1
      (let ((username (id->username id)))
        (if (and=> username (negate string-null?))
            username
            #f)))))

(define (remove-account username)
  "Delete the Jami account associated with USERNAME, the account 40 characters
fingerprint or a registered username."
  (let ((id (username->id username)))
    (call-configuration-manager-method "removeAccount" (list id)))
  (invalidate-username-to-id-cache!))

(define* (username->contacts username)
  "Return the contacts associated with the account of USERNAME as two values;
the first one being the regular contacts and the second one the banned
contacts.  USERNAME can be either the account 40 characters public key
fingerprint or a registered username.  The contacts returned are represented
using their 40 characters fingerprint."
  (let* ((id (username->id username))
         ;; The contacts are returned as "aa{ss}", that is, an array of arrays
         ;; containing (string . string) pairs.
         (contacts (map vector->list
                        (vector->list (call-configuration-manager-method
                                       "getContacts" (list id)))))
         (banned? (lambda (contact)
                    (and=> (assoc-ref contact "banned")
                           (cut string=? "true" <>))))
         (banned (filter banned? contacts))
         (not-banned (filter (negate banned?) contacts))
         (fingerprint (cut assoc-ref <> "id")))
    (values (map fingerprint not-banned)
            (map fingerprint banned))))

(define* (remove-contact contact username #:key ban?)
  "Remove CONTACT, the 40 characters public key fingerprint of a contact, from
the account associated with USERNAME (either a fingerprint or a registered
username).  When BAN? is true, also mark the contact as banned."
  (validate-fingerprint contact)
  (let ((id (username->id username)))
    (call-configuration-manager-method "removeContact" (list id contact ban?))))

(define (add-contact contact username)
  "Add CONTACT, the 40 characters public key fingerprint of a contact, to the
account of USERNAME (either a fingerprint or a registered username)."
  (validate-fingerprint contact)
  (let ((id (username->id username)))
    (call-configuration-manager-method "addContact" (list id contact))))

(define* (set-account-details details username #:key timeout)
  "Set DETAILS, an alist containing the key value pairs to set for the account
of USERNAME, a registered username or account fingerprint.  The value of the
parameters not provided are unchanged.  TIMEOUT is a value in milliseconds to
pass to the `call-configuration-manager-method' procedure."
  (let* ((id (username->id username))
         (current-details (id->account-details id))
         (updated-details (map (match-lambda
                                 ((key . value)
                                  (or (and=> (assoc-ref details key)
                                             (cut cons key <>))
                                      (cons key value))))
                               current-details)))
    (call-configuration-manager-method
     "setAccountDetails" (list id (list->vector updated-details))
     #:timeout timeout)))

(define (set-all-moderators enabled? username)
  "Set the 'AllModerators' property to enabled? for the account of USERNAME, a
registered username or account fingerprint."
  (let ((id (username->id username)))
    (call-configuration-manager-method "setAllModerators" (list id enabled?))))

(define (username->all-moderators? username)
  "Return the 'AllModerators' property for the account of USERNAME, a
registered username or account fingerprint."
  (let ((id (username->id username)))
    (call-configuration-manager-method "isAllModerators" (list id))))

(define (username->moderators username)
  "Return the moderators for the account of USERNAME, a registered username or
account fingerprint."
  (let* ((id (username->id username)))
    (vector->list (call-configuration-manager-method "getDefaultModerators"
                                                     (list id)))))

(define (set-moderator contact enabled? username)
  "Set the moderator flag to ENABLED? for CONTACT, the 40 characters public
key fingerprint of a contact for the account of USERNAME, a registered
username or account fingerprint."
  (validate-fingerprint contact)
  (let* ((id (username->id username)))
    (call-configuration-manager-method "setDefaultModerator"
                                       (list id contact enabled?))))

(define (disable-account username)
  "Disable the account known by USERNAME, a registered username or account
fingerprint."
  (set-account-details '(("Account.enable" . "false")) username
                       ;; Waiting for the reply on this command takes a very
                       ;; long time that trips the default D-Bus timeout value
                       ;; (25 s), for some reason.
                        #:timeout 60))

(define (enable-account username)
  "Enable the account known by USERNAME, a registered username or account
fingerprint."
  (set-account-details '(("Account.enable" . "true")) username))


;;;
;;; Presentation procedures.
;;;

(define (.->_ text)
  "Map each period character to underscore characters."
  (string-map (match-lambda
                (#\. #\_)
                (c c))
              text))

(define (account-details->recutil account-details)
  "Serialize the account-details alist into a recutil string.  Period
characters in the keys are normalized to underscore to meet Recutils' format
requirements."
  (define (pair->recutil-property pair)
    (match pair
      ((key . value)
       (string-append (.->_ key) ": " value))))

  (define sorted-account-details
    ;; Have the account username, display name and alias appear first, for
    ;; convenience.
    (let ((first-items '("Account.username"
                         "Account.displayName"
                         "Account.alias")))
      (append (map (cut assoc <> account-details) first-items)
              (fold alist-delete account-details first-items))))

  (string-join (map pair->recutil-property sorted-account-details) "\n"))
