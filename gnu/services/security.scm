;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 muradm <mail@muradm.net>
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

(define-module (gnu services security)
  #:use-module (gnu packages admin)
  #:use-module (gnu services)
  #:use-module (gnu services configuration)
  #:use-module (gnu services shepherd)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix records)
  #:use-module (guix ui)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (fail2ban-configuration
            fail2ban-ignore-cache-configuration
            fail2ban-jail-action-configuration
            fail2ban-jail-configuration
            fail2ban-jail-filter-configuration
            fail2ban-jail-service
            fail2ban-service-type))

(define-configuration/no-serialization fail2ban-ignore-cache-configuration
  (key string "Cache key.")
  (max-count integer "Cache size.")
  (max-time integer "Cache time."))

(define serialize-fail2ban-ignore-cache-configuration
  (match-lambda
    (($ <fail2ban-ignore-cache-configuration> _ key max-count max-time)
     (format #f "key=\"~a\", max-count=~d, max-time=~d"
             key max-count max-time))))

(define-maybe/no-serialization string)

(define-configuration/no-serialization fail2ban-jail-filter-configuration
  (name string "Filter to use.")
  (mode maybe-string "Mode for filter."))

(define serialize-fail2ban-jail-filter-configuration
  (match-lambda
    (($ <fail2ban-jail-filter-configuration> _ name mode)
     (format #f "~a~@[[mode=~a]~]" name (maybe-value mode)))))

(define (argument? a)
  (and (pair? a)
       (string? (car a))
       (or (string? (cdr a))
           (list-of-strings? (cdr a)))))

(define list-of-arguments? (list-of argument?))

(define-configuration/no-serialization fail2ban-jail-action-configuration
  (name string "Action name.")
  (arguments (list-of-arguments '()) "Action arguments."))

(define list-of-fail2ban-jail-actions?
  (list-of fail2ban-jail-action-configuration?))

(define (serialize-fail2ban-jail-action-configuration-arguments args)
  (let* ((multi-value
          (lambda (v)
            (format #f "~a" (string-join v ","))))
         (any-value
          (lambda (v)
            (if (list? v) (string-append "\"" (multi-value v) "\"") v)))
         (key-value
          (lambda (e)
            (format #f "~a=~a" (car e) (any-value (cdr e))))))
    (format #f "~a" (string-join (map key-value args) ","))))

(define serialize-fail2ban-jail-action-configuration
  (match-lambda
    (($ <fail2ban-jail-action-configuration> _ name arguments)
     (format
      #f "~a~a"
      name
      (if (null? arguments) ""
          (format
           #f "[~a]"
           (serialize-fail2ban-jail-action-configuration-arguments
            arguments)))))))

(define fail2ban-backend->string
  (match-lambda
    ('auto "auto")
    ('pyinotify "pyinotify")
    ('gamin "gamin")
    ('polling "polling")
    ('systemd "systemd")
    (unknown
     (leave (G_ "fail2ban: '~a' is not a supported backend~%") unknown))))

(define fail2ban-log-encoding->string
  (match-lambda
    ('auto "auto")
    ('utf-8 "utf-8")
    ('ascii "ascii")
    (unknown
     (leave (G_ "fail2ban: '~a' is not a supported log encoding~%") unknown))))

(define (fail2ban-jail-configuration-serialize-field-name name)
  (cond ((symbol? name)
         (fail2ban-jail-configuration-serialize-field-name
          (symbol->string name)))
        ((string-suffix? "?" name)
         (fail2ban-jail-configuration-serialize-field-name
          (string-drop-right name 1)))
        ((string-prefix? "ban-time-" name)
         (fail2ban-jail-configuration-serialize-field-name
          (string-append "bantime." (substring name 9))))
        ((string-contains name "-")
         (fail2ban-jail-configuration-serialize-field-name
          (string-filter (lambda (c) (not (equal? c #\-))) name)))
        (else name)))

(define (fail2ban-jail-configuration-serialize-string field-name value)
  #~(string-append
     #$(fail2ban-jail-configuration-serialize-field-name field-name)
     " = " #$value "\n"))

(define (fail2ban-jail-configuration-serialize-integer field-name value)
  (fail2ban-jail-configuration-serialize-string
   field-name (number->string value)))

(define (fail2ban-jail-configuration-serialize-boolean field-name value)
  (fail2ban-jail-configuration-serialize-string
   field-name (if value "true" "false")))

(define (fail2ban-jail-configuration-serialize-backend field-name value)
  (if (maybe-value-set? value)
      (fail2ban-jail-configuration-serialize-string
       field-name (fail2ban-backend->string value))
      ""))

(define (fail2ban-jail-configuration-serialize-fail2ban-ignore-cache-configuration field-name value)
  (fail2ban-jail-configuration-serialize-string
   field-name (serialize-fail2ban-ignore-cache-configuration value)))

(define (fail2ban-jail-configuration-serialize-fail2ban-jail-filter-configuration field-name value)
  (fail2ban-jail-configuration-serialize-string
   field-name (serialize-fail2ban-jail-filter-configuration value)))

(define (fail2ban-jail-configuration-serialize-log-encoding field-name value)
  (if (maybe-value-set? value)
      (fail2ban-jail-configuration-serialize-string
       field-name (fail2ban-log-encoding->string value))
      ""))

(define (fail2ban-jail-configuration-serialize-list-of-strings field-name value)
  (if (null? value)
      ""
      (fail2ban-jail-configuration-serialize-string
       field-name (string-join value " "))))

(define (fail2ban-jail-configuration-serialize-list-of-fail2ban-jail-actions field-name value)
  (if (null? value)
      ""
      (fail2ban-jail-configuration-serialize-string
       field-name (string-join
                   (map serialize-fail2ban-jail-action-configuration value) "\n"))))

(define (fail2ban-jail-configuration-serialize-symbol field-name value)
  (fail2ban-jail-configuration-serialize-string field-name (symbol->string value)))

(define (fail2ban-jail-configuration-serialize-extra-content field-name value)
  (if (maybe-value-set? value)
      (string-append "\n" value "\n")
      ""))

(define-maybe integer (prefix fail2ban-jail-configuration-))
(define-maybe string (prefix fail2ban-jail-configuration-))
(define-maybe boolean (prefix fail2ban-jail-configuration-))
(define-maybe symbol (prefix fail2ban-jail-configuration-))
(define-maybe fail2ban-ignore-cache-configuration (prefix fail2ban-jail-configuration-))
(define-maybe fail2ban-jail-filter-configuration (prefix fail2ban-jail-configuration-))

(define-configuration fail2ban-jail-configuration
  (name
   string
   "Required name of this jail configuration."
   empty-serializer)
  (enabled?
   (boolean #t)
   "Whether this jail is enabled.")
  (backend
   maybe-symbol
   "Backend to use to detect changes in the @code{log-path}.  The default is
'auto.  To consult the defaults of the jail configuration, refer to the
@file{/etc/fail2ban/jail.conf} file of the @code{fail2ban} package."
fail2ban-jail-configuration-serialize-backend)
  (max-retry
   maybe-integer
   "The number of failures before a host get banned
(e.g. @code{(max-retry 5)}).")
  (max-matches
   maybe-integer
   "The number of matches stored in ticket (resolvable via
tag @code{<matches>}) in action.")
  (find-time
   maybe-string
   "The time window during which the maximum retry count must be reached for
an IP address to be banned.  A host is banned if it has generated
@code{max-retry} during the last @code{find-time}
seconds (e.g. @code{(find-time \"10m\")}).  It can be provided in seconds or
using Fail2Ban's \"time abbreviation format\", as described in @command{man 5
jail.conf}.")
  (ban-time
   maybe-string
   "The duration, in seconds or time abbreviated format, that a ban should last.
(e.g. @code{(ban-time \"10m\")}).")
  (ban-time-increment?
   maybe-boolean
   "Whether to consider past bans to compute increases to the default ban time
of a specific IP address.")
  (ban-time-factor
   maybe-string
   "The coefficient to use to compute an exponentially growing ban time.")
  (ban-time-formula
   maybe-string
   "This is the formula used to calculate the next value of a ban time.")
  (ban-time-multipliers
   maybe-string
   "Used to calculate next value of ban time instead of formula.")
  (ban-time-max-time
   maybe-string
   "The maximum number of seconds a ban should last.")
  (ban-time-rnd-time
   maybe-string
   "The maximum number of seconds a randomized ban time should last.  This can
be useful to stop ``clever'' botnets calculating the exact time an IP address
can be unbanned again.")
  (ban-time-overall-jails?
   maybe-boolean
   "When true, it specifies the search of an IP address in the database should
be made across all jails.  Otherwise, only the current jail of the ban IP
address is considered.")
  (ignore-self?
   maybe-boolean
   "Never ban the local machine's own IP address.")
  (ignore-ip
   (list-of-strings '())
   "A list of IP addresses, CIDR masks or DNS hosts to ignore.
@code{fail2ban} will not ban a host which matches an address in this list.")
  (ignore-cache
   maybe-fail2ban-ignore-cache-configuration
   "Provide cache parameters for the ignore failure check.")
  (filter
   maybe-fail2ban-jail-filter-configuration
   "The filter to use by the jail, specified via a
@code{<fail2ban-jail-filter-configuration>} object.  By default, jails have
names matching their filter name.")
  (log-time-zone
   maybe-string
   "The default time zone for log lines that do not have one.")
  (log-encoding
   maybe-symbol
   "The encoding of the log files handled by the jail.
Possible values are: @code{'ascii}, @code{'utf-8} and @code{'auto}."
fail2ban-jail-configuration-serialize-log-encoding)
  (log-path
   (list-of-strings '())
   "The file names of the log files to be monitored.")
  (action
   (list-of-fail2ban-jail-actions '())
   "A list of @code{<fail2ban-jail-action-configuration>}.")
  (extra-content
   maybe-string
   "Extra content for the jail configuration."
   fail2ban-jail-configuration-serialize-extra-content)
  (prefix fail2ban-jail-configuration-))

(define list-of-fail2ban-jail-configurations?
  (list-of fail2ban-jail-configuration?))

(define (serialize-fail2ban-jail-configuration config)
  #~(string-append
     #$(format #f "[~a]\n" (fail2ban-jail-configuration-name config))
     #$(serialize-configuration
      config fail2ban-jail-configuration-fields)))

(define-configuration/no-serialization fail2ban-configuration
  (fail2ban
   (package fail2ban)
   "The @code{fail2ban} package to use.  It is used for both binaries and as
base default configuration that is to be extended with
@code{<fail2ban-jail-configuration>} objects.")
  (run-directory
   (string "/var/run/fail2ban")
   "The state directory for the @code{fail2ban} daemon.")
  (jails
   (list-of-fail2ban-jail-configurations '())
   "Instances of @code{<fail2ban-jail-configuration>} collected from
extensions.")
  (extra-jails
   (list-of-fail2ban-jail-configurations '())
   "Instances of @code{<fail2ban-jail-configuration>} explicitly provided.")
  (extra-content
   maybe-string
   "Extra raw content to add to the end of the @file{jail.local} file."))

(define (serialize-fail2ban-configuration config)
  (let* ((jails (fail2ban-configuration-jails config))
         (extra-jails (fail2ban-configuration-extra-jails config))
         (extra-content (fail2ban-configuration-extra-content config)))
    (interpose
     (append (map serialize-fail2ban-jail-configuration
                  (append jails extra-jails))
             (list (if (maybe-value-set? extra-content)
                       extra-content
                       ""))))))

(define (config->fail2ban-etc-directory config)
  (let* ((fail2ban (fail2ban-configuration-fail2ban config))
         (jail-local (apply mixed-text-file "jail.local"
                            (serialize-fail2ban-configuration config))))
    (directory-union
     "fail2ban-configuration"
     (list (computed-file
            "etc-fail2ban"
            (with-imported-modules '((guix build utils))
              #~(begin
                  (use-modules (guix build utils))
                  (let ((etc (string-append #$output "/etc")))
                    (mkdir-p etc)
                    (symlink #$(file-append fail2ban "/etc/fail2ban")
                             (string-append etc "/fail2ban"))))))
           (computed-file
            "etc-fail2ban-jail.local"
            (with-imported-modules '((guix build utils))
              #~(begin
                  (use-modules (guix build utils))
                  (define etc/fail2ban (string-append #$output
                                                      "/etc/fail2ban"))
                  (mkdir-p etc/fail2ban)
                  (symlink #$jail-local (string-append etc/fail2ban
                                                       "/jail.local")))))))))

(define (fail2ban-shepherd-service config)
  (match-record config <fail2ban-configuration>
    (fail2ban run-directory)
    (let* ((fail2ban-server (file-append fail2ban "/bin/fail2ban-server"))
           (pid-file (in-vicinity run-directory "fail2ban.pid"))
           (socket-file (in-vicinity run-directory "fail2ban.sock"))
           (config-dir (file-append (config->fail2ban-etc-directory config)
                                    "/etc/fail2ban"))
           (fail2ban-action (lambda args
                              #~(lambda _
                                  (invoke #$fail2ban-server
                                          "-c" #$config-dir
                                          "-p" #$pid-file
                                          "-s" #$socket-file
                                          "-b"
                                          #$@args)))))

      ;; TODO: Add 'reload' action.
      (list (shepherd-service
             (provision '(fail2ban))
             (documentation "Run the fail2ban daemon.")
             (requirement '(user-processes))
             (modules `((ice-9 match)
                        ,@%default-modules))
             (start (fail2ban-action "start"))
             (stop (fail2ban-action "stop")))))))

(define fail2ban-service-type
  (service-type (name 'fail2ban)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          fail2ban-shepherd-service)))
                (compose concatenate)
                (extend (lambda (config jails)
                          (fail2ban-configuration
                           (inherit config)
                           (jails (append (fail2ban-configuration-jails config)
                                          jails)))))
                (default-value (fail2ban-configuration))
                (description "Run the fail2ban server.")))

(define (fail2ban-jail-service svc-type jail)
  "Convenience procedure to add a fail2ban service extension to SVC-TYPE, a
<service-type> object.  The fail2ban extension is specified by JAIL, a
<fail2ban-jail-configuration> object."
  (service-type
   (inherit svc-type)
   (extensions
    (append (service-type-extensions svc-type)
            (list (service-extension fail2ban-service-type
                                     (lambda _ (list jail))))))))


;;;
;;; Documentation generation.
;;;
(define (generate-doc)
  (configuration->documentation 'fail2ban-configuration)
  (configuration->documentation 'fail2ban-ignore-cache-configuration)
  (configuration->documentation 'fail2ban-jail-action-configuration)
  (configuration->documentation 'fail2ban-jail-configuration)
  (configuration->documentation 'fail2ban-jail-filter-configuration))
