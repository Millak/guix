;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020, 2021 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2021 raid5atemyhomework <raid5atemyhomework@protonmail.com>
;;; Copyright © 2020 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2023 Brian Cully <bjc@spork.org>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu services)
  #:use-module (guix derivations)
  #:use-module (guix gexp)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module (guix records)
  #:use-module (guix profiles)
  #:use-module (guix discovery)
  #:use-module (guix combinators)
  #:use-module (guix channels)
  #:use-module (guix describe)
  #:use-module (guix sets)
  #:use-module (guix ui)
  #:use-module (guix diagnostics)
  #:autoload   (guix openpgp) (openpgp-format-fingerprint)
  #:use-module (guix modules)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix deprecation)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages hurd)
  #:use-module (gnu packages linux)
  #:use-module (gnu system privilege)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ice-9 vlist)
  #:use-module (ice-9 match)
  #:autoload   (ice-9 pretty-print) (pretty-print)
  #:export (service-extension
            service-extension?
            service-extension-target
            service-extension-compute

            service-type
            service-type?
            service-type-name
            service-type-extensions
            service-type-compose
            service-type-extend
            service-type-default-value
            service-type-description
            service-type-location

            %service-type-path
            fold-service-types
            lookup-service-types

            service
            service?
            service-kind
            service-value
            service-parameters                    ;deprecated

            simple-service
            modify-services
            service-back-edges
            instantiate-missing-services
            fold-services

            remove-service-extensions
            for-home
            for-home?

            validate-service-list

            service-error?
            missing-value-service-error?
            missing-value-service-error-type
            missing-value-service-error-location
            missing-target-service-error?
            missing-target-service-error-service
            missing-target-service-error-target-type
            ambiguous-target-service-error?
            ambiguous-target-service-error-service
            ambiguous-target-service-error-target-type

            system-service-type
            provenance-service-type
            sexp->system-provenance
            system-provenance
            boot-service-type
            cleanup-service-type
            activation-service-type
            activation-service->script
            %linux-bare-metal-service
            %hurd-rc-script
            %hurd-startup-service
            special-files-service-type
            extra-special-file
            etc-service-type
            etc-profile-d-service-type
            etc-bashrc-d-service-type
            etc-directory
            privileged-program-service-type
            setuid-program-service-type ; deprecated
            profile-service-type
            firmware-service-type
            gc-root-service-type
            linux-builder-service-type
            linux-builder-configuration
            linux-builder-configuration?
            linux-builder-configuration-kernel
            linux-builder-configuration-modules
            linux-loadable-module-service-type

            %boot-service
            %activation-service
            etc-service)  ; deprecated
  #:re-export (;; Note: Re-export 'delete' to allow for proper syntax matching
               ;; in 'modify-services' forms.  See
               ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=26805#16>.
               delete))

;;; Comment:
;;;
;;; This module defines a broad notion of "service types" and "services."
;;;
;;; A service type describe how its instances extend instances of other
;;; service types.  For instance, some services extend the instance of
;;; ACCOUNT-SERVICE-TYPE by providing it with accounts and groups to create;
;;; others extend SHEPHERD-ROOT-SERVICE-TYPE by passing it instances of
;;; <shepherd-service>.
;;;
;;; When applicable, the service type defines how it can itself be extended,
;;; by providing one procedure to compose extensions, and one procedure to
;;; extend itself.
;;;
;;; A notable service type is SYSTEM-SERVICE-TYPE, which has a single
;;; instance, which is the root of the service DAG.  Its value is the
;;; derivation that produces the 'system' directory as returned by
;;; 'operating-system-derivation'.
;;;
;;; The 'fold-services' procedure can be passed a list of procedures, which it
;;; "folds" by propagating extensions down the graph; it returns the root
;;; service after the applying all its extensions.
;;;
;;; Code:

(define-record-type <service-extension>
  (service-extension target compute)
  service-extension?
  (target  service-extension-target)              ;<service-type>
  (compute service-extension-compute))            ;params -> params

(define &no-default-value
  ;; Value used to denote service types that have no associated default value.
  '(no default value))

(define-record-type* <service-type> service-type make-service-type
  service-type?
  (name       service-type-name)                  ;symbol (for debugging)

  ;; Things extended by services of this type.
  (extensions service-type-extensions)            ;list of <service-extensions>

  ;; Given a list of extensions, "compose" them.
  (compose    service-type-compose                ;list of Any -> Any
              (default #f))

  ;; Extend the services' own parameters with the extension composition.
  (extend     service-type-extend                 ;list of Any -> parameters
              (default #f))

  ;; Optional default value for instances of this type.
  (default-value service-type-default-value       ;Any
                 (default &no-default-value))

  ;; Meta-data.
  (description  service-type-description)         ;string
  (location     service-type-location             ;<location>
                (default (and=> (current-source-location)
                                source-properties->location))
                (innate)))

(define (write-service-type type port)
  (format port "#<service-type ~a ~a>"
          (service-type-name type)
          (number->string (object-address type) 16)))

(set-record-type-printer! <service-type> write-service-type)

(define %distro-root-directory
  ;; Absolute file name of the module hierarchy.
  (dirname (search-path %load-path "guix.scm")))

(define %service-type-path
  ;; Search path for service types.
  (make-parameter `((,%distro-root-directory . "gnu/services")
                    (,%distro-root-directory . "gnu/system"))))

(define (all-service-modules)
  "Return the default set of service modules."
  (cons (resolve-interface '(gnu services))
        (all-modules (%service-type-path)
                     #:warn warn-about-load-error)))

(define* (fold-service-types proc seed
                             #:optional
                             (modules (all-service-modules)))
  "For each service type exported by one of MODULES, call (PROC RESULT).  SEED
is used as the initial value of RESULT."
  (fold-module-public-variables (lambda (object result)
                                  (if (service-type? object)
                                      (proc object result)
                                      result))
                                seed
                                modules))

(define lookup-service-types
  (let ((table
         (delay (fold-service-types (lambda (type result)
                                      (vhash-consq (service-type-name type)
                                                   type result))
                                    vlist-null))))
    (lambda (name)
      "Return the list of services with the given NAME (a symbol)."
      (vhash-foldq* cons '() name (force table)))))

;; Services of a given type.
(define-record-type <service>
  (make-service type value)
  service?
  (type       service-kind)
  (value      service-value))

(define-syntax service
  (syntax-rules ()
    "Return a service instance of TYPE.  The service value is VALUE or, if
omitted, TYPE's default value."
    ((_ type value)
     (make-service type value))
    ((_ type)
     (%service-with-default-value (current-source-location)
                                  type))))

(define (%service-with-default-value location type)
  "Return a instance of service type TYPE with its default value, if any.  If
TYPE does not have a default value, an error is raised."
  ;; TODO: Currently this is a run-time error but with a little bit macrology
  ;; we could turn it into an expansion-time error.
  (let ((default (service-type-default-value type)))
    (if (eq? default &no-default-value)
        (let ((location (source-properties->location location)))
          (raise
           (make-compound-condition
            (condition
             (&missing-value-service-error (type type) (location location)))
            (formatted-message (G_ "~a: no value specified \
for service of type '~a'")
                               (location->string location)
                               (service-type-name type)))))
        (service type default))))

(define-condition-type &service-error &error
  service-error?)

(define-condition-type &missing-value-service-error &service-error
  missing-value-service-error?
  (type     missing-value-service-error-type)
  (location missing-value-service-error-location))



;;;
;;; Helpers.
;;;

(define service-parameters
  ;; Deprecated alias.
  service-value)

(define (simple-service name target value)
  "Return a service that extends TARGET with VALUE.  This works by creating a
singleton service type NAME, of which the returned service is an instance."
  (let* ((extension (service-extension target identity))
         (type      (service-type (name name)
                                  (extensions (list extension))
                                  (description "This is a simple service."))))
    (service type value)))

(define-syntax clause-alist
  (syntax-rules (=> delete)
    "Build an alist of clauses.  Each element has the form (KIND PROC LOC)
where PROC is the service transformation procedure to apply for KIND, and LOC
is the source location information."
    ((_ (delete kind) rest ...)
     (cons (list kind
                 (lambda (service)
                   #f)
                 (current-source-location))
           (clause-alist rest ...)))
    ((_ (kind param => exp ...) rest ...)
     (cons (list kind
                 (lambda (svc)
                   (let ((param (service-value svc)))
                     (service (service-kind svc)
                              (begin exp ...))))
                 (current-source-location))
           (clause-alist rest ...)))
    ((_)
     '())))

(define (apply-clauses clauses service deleted-services)
  "Apply CLAUSES, an alist as returned by 'clause-alist', to SERVICE.  An
exception is raised if a clause attempts to modify a service
present in DELETED-SERVICES."
  (define (raise-if-deleted kind properties)
    (match (find (match-lambda
                   ((deleted-kind _)
                    (eq? kind deleted-kind)))
                 deleted-services)
      ((_ deleted-properties)
       (raise (make-compound-condition
               (condition
                (&error-location
                 (location (source-properties->location properties))))
               (formatted-message
                (G_ "modify-services: service '~a' was deleted here: ~a")
                (service-type-name kind)
                (source-properties->location deleted-properties)))))
      (_ #t)))

  (match clauses
    (((kind proc properties) . rest)
     (raise-if-deleted kind properties)
     (if (eq? (and service (service-kind service)) kind)
         (let ((new-service (proc service)))
           (apply-clauses rest new-service
                          (if new-service
                              deleted-services
                              (cons (list kind properties)
                                    deleted-services))))
         (apply-clauses rest service deleted-services)))
    (()
     service)))

(define (%modify-services services clauses)
  "Apply CLAUSES, an alist as returned by 'clause-alist', to SERVICES.  An
exception is raised if a clause attempts to modify a missing service."
  (define (raise-if-not-found clause)
    (match clause
      ((kind _ properties)
       (unless (find (lambda (service)
                       (eq? kind (service-kind service)))
                     services)
         (raise (make-compound-condition
                 (condition
                  (&error-location
                   (location (source-properties->location properties))))
                 (formatted-message
                  (G_ "modify-services: service '~a' not found in service list")
                  (service-type-name kind))))))))

  (for-each raise-if-not-found clauses)
  (reverse (filter-map identity
                       (fold (lambda (service services)
                               (cons (apply-clauses clauses service '())
                                     services))
                             '()
                             services))))

(define-syntax modify-services
  (syntax-rules ()
    "Modify the services listed in SERVICES according to CLAUSES and return
the resulting list of services.  Each clause must have the form:

  (TYPE VARIABLE => BODY)

where TYPE is a service type, such as 'guix-service-type', and VARIABLE is an
identifier that is bound within BODY to the value of the service of that
TYPE.

Clauses can also remove services of a given type:

  (delete TYPE)

Consider this example:

  (modify-services %base-services
    (guix-service-type config =>
                       (guix-configuration
                        (inherit config)
                        (use-substitutes? #f)
                        (extra-options '(\"--gc-keep-derivations\"))))
    (mingetty-service-type config =>
                           (mingetty-configuration
                            (inherit config)
                            (motd (plain-file \"motd\" \"Hi there!\"))))
    (delete udev-service-type))

It changes the configuration of the GUIX-SERVICE-TYPE instance, and that of
all the MINGETTY-SERVICE-TYPE instances, and it deletes instances of the
UDEV-SERVICE-TYPE."
    ((_ services clauses ...)
     (%modify-services services (clause-alist clauses ...)))))


;;;
;;; Core services.
;;;

(define (system-derivation entries mextensions)
  "Return as a monadic value the derivation of the 'system' directory
containing the given entries."
  (mlet %store-monad ((extensions (mapm/accumulate-builds identity
                                                          mextensions)))
    (lower-object
     (file-union "system"
                 (append entries (concatenate extensions))))))

(define system-service-type
  ;; This is the ultimate service type, the root of the service DAG.  The
  ;; service of this type is extended by monadic name/item pairs.  These items
  ;; end up in the "system directory" as returned by
  ;; 'operating-system-derivation'.
  (service-type (name 'system)
                (extensions '())
                (compose identity)
                (extend system-derivation)
                (description
                 "Build the operating system top-level directory, which in
turn refers to everything the operating system needs: its kernel, initrd,
system profile, boot script, and so on.")))

(define (compute-boot-script _ gexps)
  ;; Reverse GEXPS so that extensions appear in the boot script in the right
  ;; order.  That is, user extensions would come first, and extensions added
  ;; by 'essential-services' (e.g., running shepherd) are guaranteed to come
  ;; last.
  (gexp->file "boot"
              ;; Clean up and activate the system, then spawn shepherd.
              #~(begin #$@(reverse gexps))))

(define (boot-script-entry mboot)
  "Return, as a monadic value, an entry for the boot script in the system
directory."
  (mlet %store-monad ((boot mboot))
    (return `(("boot" ,boot)))))

(define boot-service-type
  ;; The service of this type is extended by being passed gexps.  It
  ;; aggregates them in a single script, as a monadic value, which becomes its
  ;; value.
  (service-type (name 'boot)
                (extensions
                 (list (service-extension system-service-type
                                          boot-script-entry)))
                (compose identity)
                (extend compute-boot-script)
                (default-value #f)
                (description
                 "Produce the operating system's boot script, which is spawned
by the initrd once the root file system is mounted.")))

(define %boot-service
  ;; The service that produces the boot script.
  (service boot-service-type #t))


;;;
;;; Provenance tracking.
;;;

(define (object->pretty-string obj)
  "Like 'object->string', but using 'pretty-print'."
  (call-with-output-string
    (lambda (port)
      (pretty-print obj port))))

(define (channel->code channel)
  "Return code to build CHANNEL, ready to be dropped in a 'channels.scm'
file."
  ;; Since the 'introduction' field is backward-incompatible, and since it's
  ;; optional when using the "official" 'guix channel, include it if and only
  ;; if we're referring to a different channel.
  (let ((intro (and (not (equal? (list channel) %default-channels))
                    (channel-introduction channel))))
    `(channel (name ',(channel-name channel))
              (url ,(channel-url channel))
              (branch ,(channel-branch channel))
              (commit ,(channel-commit channel))
              ,@(if intro
                    `((introduction
                       (make-channel-introduction
                        ,(channel-introduction-first-signed-commit intro)
                        (openpgp-fingerprint
                         ,(openpgp-format-fingerprint
                           (channel-introduction-first-commit-signer
                            intro))))))
                    '()))))

(define (channel->sexp channel)
  "Return an sexp describing CHANNEL.  The sexp is _not_ code and is meant to
be parsed by tools; it's potentially more future-proof than code."
  ;; TODO: Add CHANNEL's introduction.  Currently we can't do that because
  ;; older 'guix system describe' expect exactly name/url/branch/commit
  ;; without any additional fields.
  `(channel (name ,(channel-name channel))
            (url ,(channel-url channel))
            (branch ,(channel-branch channel))
            (commit ,(channel-commit channel))))

(define (sexp->channel sexp)
  "Return the channel corresponding to SEXP, an sexp as found in the
\"provenance\" file produced by 'provenance-service-type'."
  (match sexp
    (('channel ('name name)
               ('url url)
               ('branch branch)
               ('commit commit)
               rest ...)
     ;; XXX: In the future REST may include a channel introduction.
     (channel (name name) (url url)
              (branch branch) (commit commit)))))

(define (provenance-file channels config-file)
  "Return a 'provenance' file describing CHANNELS, a list of channels, and
CONFIG-FILE, which can be either #f or a <local-file> containing the OS
configuration being used."
  (scheme-file "provenance"
               #~(provenance
                  (version 0)
                  (channels #+@(if channels
                                   (map channel->sexp channels)
                                   '()))
                  (configuration-file #+config-file))))

(define (provenance-entry config-file)
  "Return system entries describing the operating system provenance: the
channels in use and CONFIG-FILE, if it is true."
  (define channels
    (current-channels))

  (mbegin %store-monad
    (let ((config-file (cond ((string? config-file)
                              ;; CONFIG-FILE has been passed typically via
                              ;; 'guix system reconfigure CONFIG-FILE' so we
                              ;; can assume it's valid: tell 'local-file' to
                              ;; not emit a warning.
                              (local-file (assume-valid-file-name config-file)
                                          "configuration.scm"))
                             ((not config-file)
                              #f)
                             (else
                              config-file))))
      (return `(("provenance" ,(provenance-file channels config-file))
                ,@(if channels
                      `(("channels.scm"
                         ,(plain-file "channels.scm"
                                      (object->pretty-string
                                       `(list
                                         ,@(map channel->code channels))))))
                      '())
                ,@(if config-file
                      `(("configuration.scm" ,config-file))
                      '()))))))

(define provenance-service-type
  (service-type (name 'provenance)
                (extensions
                 (list (service-extension system-service-type
                                          provenance-entry)))
                (default-value #f)                ;the OS config file
                (description
                 "Store provenance information about the system in the system
itself: the channels used when building the system, and its configuration
file, when available.")))

(define (sexp->system-provenance sexp)
  "Parse SEXP, an s-expression read from /run/current-system/provenance or
similar, and return two values: the list of channels listed therein, and the
OS configuration file or #f."
  (match sexp
    (('provenance ('version 0)
                  ('channels channels ...)
                  ('configuration-file config-file))
     (values (map sexp->channel channels)
             config-file))
    (_
     (values '() #f))))

(define (system-provenance system)
  "Given SYSTEM, the file name of a system generation, return two values: the
list of channels SYSTEM is built from, and its configuration file.  If that
information is missing, return the empty list (for channels) and possibly
#false (for the configuration file)."
  (catch 'system-error
    (lambda ()
      (sexp->system-provenance
       (call-with-input-file (string-append system "/provenance")
         read)))
    (lambda _
      (values '() #f))))

;;;
;;; Cleanup.
;;;

(define (cleanup-gexp _)
  "Return a gexp to clean up /tmp and similar places upon boot."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        ;; Clean out /tmp, /var/run, and /run.
        ;;
        ;; XXX This needs to happen before service activations, so it
        ;; has to be here, but this also implicitly assumes that /tmp
        ;; and /var/run are on the root partition.
        (letrec-syntax ((fail-safe (syntax-rules ()
                                     ((_ exp rest ...)
                                      (begin
                                        (catch 'system-error
                                          (lambda () exp)
                                          (const #f))
                                        (fail-safe rest ...)))
                                     ((_)
                                      #t))))
          ;; Ignore I/O errors so the system can boot.
          (fail-safe
           ;; Remove stale Shadow lock files as they would lead to
           ;; failures of 'useradd' & co.
           (delete-file "/etc/group.lock")
           (delete-file "/etc/passwd.lock")
           (delete-file "/etc/.pwd.lock")         ;from 'lckpwdf'

           ;; Force file names to be decoded as UTF-8.  See
           ;; <https://bugs.gnu.org/26353>.
           (setenv "GUIX_LOCPATH"
                   #+(file-append
                      (libc-utf8-locales-for-target (%current-system))
                      "/lib/locale"))
           (setlocale LC_CTYPE "en_US.utf8")
           (delete-file-recursively "/tmp")
           (delete-file-recursively "/var/run")
           (delete-file-recursively "/run")

           ;; Note: The second argument to 'mkdir' is and'ed with umask,
           ;; hence the 'chmod' calls.
           (mkdir "/tmp" #o1777)
           (chmod "/tmp" #o1777)
           (mkdir "/var/run" #o755)
           (chmod "/var/run" #o755)
           (mkdir "/run" #o755)
           (chmod "/var/run" #o755))))))

(define cleanup-service-type
  ;; Service that cleans things up in /tmp and similar.
  (service-type (name 'cleanup)
                (extensions
                 (list (service-extension boot-service-type
                                          cleanup-gexp)))
                (description
                 "Delete files from @file{/tmp}, @file{/var/run}, and other
temporary locations at boot time.")))

(define* (activation-service->script service)
  "Return as a monadic value the activation script for SERVICE, a service of
ACTIVATION-SCRIPT-TYPE."
  (activation-script (service-value service)))

(define (activation-script gexps)
  "Return the system's activation script, which evaluates GEXPS."
  (define actions
    ;; TODO: Instead of importing modules here, let users of activation service
    ;; add them explicitly.  See <https://issues.guix.gnu.org/76698>.
    (map (lambda (action)
           (program-file "activate-service.scm"
                         (with-imported-modules (source-module-closure
                                                 '((gnu build activation)
                                                   (guix build utils)))
                           #~(begin
                               (use-modules (gnu build activation)
                                            (guix build utils))
                               #$action))))
         gexps))

  (program-file "activate.scm"
                (with-imported-modules (source-module-closure
                                        '((gnu build activation)
                                          (guix build utils)
                                          (guix diagnostics)
                                          (guix i18n)))
                  #~(begin
                      (use-modules (gnu build activation)
                                   (guix build utils)
                                   (guix diagnostics)
                                   (guix i18n)
                                   (srfi srfi-34))

                      (mkdir-p "/var/run")
                      ;; Make sure the user accounting database exists.  If it
                      ;; does not exist, 'setutxent' does not create it and
                      ;; thus there is no accounting at all.
                      (close-port (open-file "/var/run/utmpx" "a0"))

                      ;; Same for 'wtmp', which is populated by mingetty et
                      ;; al.
                      (mkdir-p "/var/log")
                      (close-port (open-file "/var/log/wtmp" "a0"))

                      ;; Set up /run/current-system.  Among other things this
                      ;; sets up locales, which the activation snippets
                      ;; executed below may expect.
                      (activate-current-system)

                      ;; Run the services' activation snippets.
                      ;; TODO: Use 'load-compiled'.
                      (for-each (lambda (action)
                                  ;; Don't block activation process when one
                                  ;; action fails.
                                  (guard (condition
                                          (else
                                           (format (current-error-port) "~a~%"
                                                   condition)
                                           (warning
                                            (G_ "failed to activate '~a'~%")
                                            action)))
                                    (save-module-excursion
                                     (lambda ()
                                       (set-current-module
                                        (make-fresh-user-module))
                                       (primitive-load action)))))
                                '#$actions)))))

(define (gexps->activation-gexp gexps)
  "Return a gexp that runs the activation script containing GEXPS."
  #~(primitive-load #$(activation-script gexps)))

(define (activation-profile-entry gexps)
  "Return, as a monadic value, an entry for the activation script in the
system directory."
  (mlet %store-monad ((activate (lower-object (activation-script gexps))))
    (return `(("activate" ,activate)))))

(define (second-argument a b) b)

(define activation-service-type
  (service-type (name 'activate)
                (extensions
                 (list (service-extension boot-service-type
                                          gexps->activation-gexp)
                       (service-extension system-service-type
                                          activation-profile-entry)))
                (compose identity)
                (extend second-argument)
                (default-value #f)
                (description
                 "Run @dfn{activation} code at boot time and upon
@command{guix system reconfigure} completion.")))

(define %activation-service
  ;; The activation service produces the activation script from the gexps it
  ;; receives.
  (service activation-service-type #t))

(define %modprobe-wrapper
  ;; Wrapper for the 'modprobe' command that knows where modules live.
  ;;
  ;; This wrapper is typically invoked by the Linux kernel ('call_modprobe',
  ;; in kernel/kmod.c), a situation where the 'LINUX_MODULE_DIRECTORY'
  ;; environment variable is not set---hence the need for this wrapper.
  (let ((modprobe "/run/current-system/profile/bin/modprobe"))
    (program-file "modprobe"
                  #~(begin
                      (setenv "LINUX_MODULE_DIRECTORY"
                              "/run/booted-system/kernel/lib/modules")
                      ;; FIXME: Remove this crutch when the patch #40422,
                      ;; updating to kmod 27 is merged.
                      (setenv "MODPROBE_OPTIONS"
                              "-C /etc/modprobe.d")
                      (apply execl #$modprobe
                             (cons #$modprobe (cdr (command-line))))))))

(define %linux-kernel-activation
  ;; Activation of the Linux kernel running on the bare metal (as opposed to
  ;; running in a container.)
  #~(begin
      ;; Tell the kernel to use our 'modprobe' command.
      (activate-modprobe #$%modprobe-wrapper)

      ;; Let users debug their own processes!
      (activate-ptrace-attach)))

(define %linux-bare-metal-service
  ;; The service that does things that are needed on the "bare metal", but not
  ;; necessary or impossible in a container.
  (simple-service 'linux-bare-metal
                  activation-service-type
                  %linux-kernel-activation))

(define %hurd-rc-script
  ;; The RC script to be started upon boot.
  (program-file "rc"
                (with-imported-modules (source-module-closure
                                        '((guix build utils)
                                          (gnu build hurd-boot)
                                          (guix build syscalls)))
                  #~(begin
                      (use-modules (guix build utils)
                                   (gnu build hurd-boot)
                                   (guix build syscalls)
                                   (ice-9 match)
                                   (system repl repl)
                                   (srfi srfi-1)
                                   (srfi srfi-26))
                      (boot-hurd-system)))))

(define (hurd-rc-entry rc)
  "Return, as a monadic value, an entry for the RC script in the system
directory."
  (mlet %store-monad ((rc (lower-object rc)))
    (return `(("rc" ,rc)))))

(define hurd-startup-service-type
  ;; The service that creates the initial SYSTEM/rc startup file.
  (service-type (name 'startup)
                (extensions
                 (list (service-extension system-service-type hurd-rc-entry)))
                (default-value %hurd-rc-script)
                (description "This service creates an @file{rc} script in the
system; that script is responsible for booting the Hurd.")))

(define %hurd-startup-service
  ;; The service that produces the RC script.
  (service hurd-startup-service-type %hurd-rc-script))

(define special-files-service-type
  ;; Service to install "special files" such as /bin/sh and /usr/bin/env.
  (service-type
   (name 'special-files)
   (extensions
    (list (service-extension activation-service-type
                             (lambda (files)
                               #~(activate-special-files '#$files)))))
   (compose concatenate)
   (extend append)
   (description
    "Add special files to the root file system---e.g.,
@file{/usr/bin/env}.")))

(define (extra-special-file file target)
  "Use TARGET as the \"special file\" FILE.  For example, TARGET might be
  (file-append coreutils \"/bin/env\")
and FILE could be \"/usr/bin/env\"."
  (simple-service (string->symbol (string-append "special-file-" file))
                  special-files-service-type
                  `((,file ,target))))

(define (etc-directory service)
  "Return the directory for SERVICE, a service of type ETC-SERVICE-TYPE."
  (files->etc-directory (service-value service)))

(define (files->etc-directory files)
  (define (assert-no-duplicates files)
    (let loop ((files files)
               (seen (set)))
      (match files
        (() #t)
        (((file _) rest ...)
         (when (set-contains? seen file)
           (raise (formatted-message (G_ "duplicate '~a' entry for /etc")
                                     file)))
         (loop rest (set-insert file seen))))))

  ;; Detect duplicates early instead of letting them through, eventually
  ;; leading to a build failure of "etc.drv".
  (assert-no-duplicates files)

  (file-union "etc" files))

(define (etc-entry files)
  "Return an entry for the /etc directory consisting of FILES in the system
directory."
  (with-monad %store-monad
    (return `(("etc" ,(files->etc-directory files))))))

(define etc-service-type
  (service-type (name 'etc)
                (extensions
                 (list
                  (service-extension activation-service-type
                                     (lambda (files)
                                       (let ((etc
                                              (files->etc-directory files)))
                                         #~(activate-etc #$etc))))
                  (service-extension system-service-type etc-entry)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description "Populate the @file{/etc} directory.")))

(define-deprecated (etc-service files)
  etc-service-type
  "Return a new service of ETC-SERVICE-TYPE that populates /etc with FILES.
FILES must be a list of name/file-like object pairs."
  (service etc-service-type files))

(define (make-files->etc-directory name)
  "Return a procedure that accept a list of FILES and compute a directory named NAME.
The returned procedure FILES argument can be packages containing
@file{etc/@var{name}.d/@var{x}.sh} scripts or single file-like objects of the
@file{.sh} file extension.  The constructed procedure returns a list of
two-elements list suitable for extending `etc-service-type'."
  (lambda (files)
    `((,name
       ,(computed-file name
         ;; This is specialized variant of `file-union'.
         (with-imported-modules '((guix build utils))
           #~(begin
               (use-modules (guix build utils)
                            (ice-9 ftw)
                            (ice-9 match)
                            (srfi srfi-1)
                            (srfi srfi-26))

               (define sh-files
                 (append-map
                  (lambda (f)
                    (let* ((dir (format #f "~a/etc/~a" f #$name)))
                      `(,@(if (file-exists? dir)
                              (map (lambda (x)
                                     (list x (string-append dir "/" x)))
                                   (scandir dir
                                            (cut string-suffix? ".sh" <>)))
                              (if (string-suffix? ".sh" f)
                                  (list (list (basename
                                               (strip-store-file-name f)) f))
                                  '())))))
                  (list #$@files)))

               (mkdir #$output)
               (chdir #$output)

               (map (match-lambda       ;XXX: adapted from file-union
                      ((target source)
                       ;; Stat the source to abort early if it does not exist.
                       (stat source)
                       (mkdir-p (dirname target))
                       (symlink source target)))
                    sh-files))))))))

(define files->profile-d-directory
  (make-files->etc-directory "profile.d"))

(define etc-profile-d-service-type
  (service-type
   (name 'etc-profile-d)
   (extensions (list (service-extension etc-service-type
                                        files->profile-d-directory)))
   (compose concatenate)
   (extend append)
   (default-value '())
   (description "A service for populating @file{/etc/profile.d/} with POSIX
scripts having the @file{.sh} file extension, to be sourced when users
log in.")))

(define files->bashrc-d-directory
  (make-files->etc-directory "bashrc.d"))

(define etc-bashrc-d-service-type
  (service-type
   (inherit etc-profile-d-service-type)
   (name 'etc-bashrc-d)
   (extensions (list (service-extension etc-service-type
                                        files->bashrc-d-directory)))
   (description "A service for populating @file{/etc/bashrc.d/} with Bash
scripts having the @file{.sh} file extension, to be sourced by interactive
Bash shells.")))

(define (privileged-program->activation-gexp programs)
  "Return an activation gexp for privileged-program from PROGRAMS."
  (let ((programs
         (map (lambda (program)
                ;; FIXME This is really ugly, I didn't manage to use "inherit".
                (let ((program-name (privileged-program-program program))
                      (setuid?      (privileged-program-setuid? program))
                      (setgid?      (privileged-program-setgid? program))
                      (user         (privileged-program-user program))
                      (group        (privileged-program-group program))
                      (capabilities (privileged-program-capabilities program)))
                  (unless (or setuid? setgid? capabilities)
                    (warning
                     (G_ "so-called privileged-program ~s lacks any privilege~%")
                     program-name))
                  #~(privileged-program (setuid? #$setuid?)
                                        (setgid? #$setgid?)
                                        (user    #$user)
                                        (group   #$group)
                                        (capabilities #$capabilities)
                                        (program #$program-name))))
              programs)))
    (with-imported-modules (source-module-closure
                            '((gnu system privilege)))
      #~(begin
          (use-modules (gnu system privilege))

          (let ((libcap #$(let-system (system target)
                            ;; When cross-compiling, assume libcap is
                            ;; available on GNU/Linux only.
                            (and (if target
                                     (string-suffix? "linux-gnu" target)
                                     (supported-package? libcap system))
                                 libcap))))
            (activate-privileged-programs (list #$@programs) libcap))))))

(define privileged-program-service-type
  (service-type (name 'privileged-program)
                (extensions
                 (list (service-extension activation-service-type
                                          privileged-program->activation-gexp)))
                (compose concatenate)
                (extend (lambda (config extensions)
                          (append config extensions)))
                (description
                 "Copy the specified executables to @file{/run/privileged/bin}
and apply special privileges like setuid and/or setgid.

The deprecated @file{/run/setuid-programs} directory is also populated with
symbolic links to their @file{/run/privileged/bin} counterpart.  It will be
removed in a future Guix release.")))

(define-deprecated/alias setuid-program-service-type
  ;; Deprecated alias to ease transition.  Will be removed!
  privileged-program-service-type)

(define (packages->profile-entry packages)
  "Return a system entry for the profile containing PACKAGES."
  ;; XXX: 'mlet' is needed here for one reason: to get the proper
  ;; '%current-target' and '%current-target-system' bindings when
  ;; 'packages->manifest' is called, and thus when the 'package-inputs'
  ;; etc. procedures are called on PACKAGES.  That way, conditionals in those
  ;; inputs see the "correct" value of these two parameters.  See
  ;; <https://issues.guix.gnu.org/44952>.
  (mlet %store-monad ((_ (current-target-system)))
    (return `(("profile" ,(profile
                           (content (packages->manifest
                                     (delete-duplicates packages eq?)))))))))

(define profile-service-type
  ;; The service that populates the system's profile---i.e.,
  ;; /run/current-system/profile.  It is extended by package lists.
  (service-type (name 'profile)
                (extensions
                 (list (service-extension system-service-type
                                          packages->profile-entry)))
                (compose concatenate)
                (extend append)
                (default-value '())
                (description
                 "This is the @dfn{system profile}, available as
@file{/run/current-system/profile}.  It contains packages that the sysadmin
wants to be globally available to all the system users.")))

(define (firmware->activation-gexp firmware)
  "Return a gexp to make the packages listed in FIRMWARE loadable by the
kernel."
  (let ((directory (directory-union "firmware" firmware)))
    ;; Tell the kernel where firmware is.
    #~(activate-firmware (string-append #$directory "/lib/firmware"))))

(define firmware-service-type
  ;; The service that collects firmware.
  (service-type (name 'firmware)
                (extensions
                 (list (service-extension activation-service-type
                                          firmware->activation-gexp)))
                (compose concatenate)
                (extend append)
                (description
                 "Make ``firmware'' files loadable by the operating system
kernel.  Firmware may then be uploaded to some of the machine's devices, such
as Wifi cards.")))

(define (gc-roots->system-entry roots)
  "Return an entry in the system's output containing symlinks to ROOTS."
  (mlet %store-monad ((entry (gexp->derivation
                              "gc-roots"
                              #~(let ((roots '#$roots))
                                  (mkdir #$output)
                                  (chdir #$output)
                                  (for-each symlink
                                            roots
                                            (map number->string
                                                 (iota (length roots))))))))
    (return (if (null? roots)
                '()
                `(("gc-roots" ,entry))))))

(define gc-root-service-type
  ;; A service to associate extra garbage-collector roots to the system.  This
  ;; is a simple hack that guarantees that the system retains references to
  ;; the given list of roots.  Roots must be "lowerable" objects like
  ;; packages, or derivations.
  (service-type (name 'gc-roots)
                (extensions
                 (list (service-extension system-service-type
                                          gc-roots->system-entry)))
                (compose concatenate)
                (extend append)
                (description
                 "Register garbage-collector roots---i.e., store items that
will not be reclaimed by the garbage collector.")
                (default-value '())))

;; Configuration for the Linux kernel builder.
(define-record-type* <linux-builder-configuration>
  linux-builder-configuration
  make-linux-builder-configuration
  linux-builder-configuration?
  this-linux-builder-configuration

  (kernel   linux-builder-configuration-kernel)                   ; package
  (modules  linux-builder-configuration-modules  (default '())))  ; list of packages

(define (package-for-kernel target-kernel module-package)
  "Return a package like MODULE-PACKAGE, adapted for TARGET-KERNEL, if
possible (that is if there's a LINUX keyword argument in the build system)."
  (package
    (inherit module-package)
    (arguments
     (substitute-keyword-arguments (package-arguments module-package)
       ((#:linux kernel #f)
        target-kernel)))))

(define (linux-builder-configuration->system-entry config)
  "Return the kernel entry of the 'system' directory."
  (let* ((kernel  (linux-builder-configuration-kernel config))
         (modules (linux-builder-configuration-modules config))
         (kernel  (profile
                    (content (packages->manifest
                              (cons kernel
                                    (map (lambda (module)
                                           (cond
                                             ((package? module)
                                              (package-for-kernel kernel module))
                                             ;; support (,package "kernel-module-output")
                                             ((and (list? module) (package? (car module)))
                                              (cons (package-for-kernel kernel
                                                                        (car module))
                                                    (cdr module)))
                                             (else
                                              module)))
                                         modules))))
                    (hooks (list linux-module-database)))))
    (with-monad %store-monad
      (return `(("kernel" ,kernel))))))

(define linux-builder-service-type
  (service-type (name 'linux-builder)
                (extensions
                  (list (service-extension system-service-type
                                           linux-builder-configuration->system-entry)))
                (default-value '())
                (compose identity)
                (extend (lambda (config modifiers)
                          (if (null? modifiers)
                              config
                              ((apply compose modifiers) config))))
                (description "Builds the linux-libre kernel profile, containing
the kernel itself and any linux-loadable kernel modules.  This can be extended
with a function that accepts the current configuration and returns a new
configuration.")))

(define (linux-loadable-module-builder-modifier modules)
  "Extends linux-builder-service-type by appending the given MODULES to the
configuration of linux-builder-service-type."
  (lambda (config)
    (linux-builder-configuration
      (inherit config)
      (modules (append (linux-builder-configuration-modules config)
                       modules)))))

(define linux-loadable-module-service-type
  (service-type (name 'linux-loadable-modules)
                (extensions
                  (list (service-extension linux-builder-service-type
                                           linux-loadable-module-builder-modifier)))
                (default-value '())
                (compose concatenate)
                (extend append)
                (description "Adds packages and package outputs as modules
included in the booted linux-libre profile.  Other services can extend this
service type to add particular modules to the set of linux-loadable modules.")))



;;;
;;; Service folding.
;;;

(define-condition-type &missing-target-service-error &service-error
  missing-target-service-error?
  (service      missing-target-service-error-service)
  (target-type  missing-target-service-error-target-type))

(define-condition-type &ambiguous-target-service-error &service-error
  ambiguous-target-service-error?
  (service      ambiguous-target-service-error-service)
  (target-type  ambiguous-target-service-error-target-type))

(define (missing-target-error service target-type)
  (raise
   (condition (&missing-target-service-error
               (service service)
               (target-type target-type))
              (&message
               (message
                (format #f (G_ "no target of type '~a' for service '~a'")
                        (service-type-name target-type)
                        (service-type-name
                         (service-kind service))))))))

(define (service-back-edges services)
  "Return a procedure that, when passed a <service>, returns the list of
<service> objects that depend on it."
  (define (add-edges service edges)
    (define (add-edge extension edges)
      (let ((target-type (service-extension-target extension)))
        (match (filter (lambda (service)
                         (eq? (service-kind service) target-type))
                       services)
          ((target)
           (vhash-consq target service edges))
          (()
           (missing-target-error service target-type))
          (x
           (raise
            (condition (&ambiguous-target-service-error
                        (service service)
                        (target-type target-type))
                       (&message
                        (message
                         (format #f
                                 (G_ "more than one target service of type '~a'")
                                 (service-type-name target-type))))))))))

    (fold add-edge edges (service-type-extensions (service-kind service))))

  (let ((edges (fold add-edges vlist-null services)))
    (lambda (node)
      (reverse (vhash-foldq* cons '() node edges)))))

(define (instantiate-missing-services services)
  "Return SERVICES, a list, augmented with any services targeted by extensions
and missing from SERVICES.  Only service types with a default value can be
instantiated; other missing services lead to a
'&missing-target-service-error'."
  (define (adjust-service-list svc result instances)
    (fold2 (lambda (extension result instances)
             (define target-type
               (service-extension-target extension))

             (match (vhash-assq target-type instances)
               (#f
                (let ((default (service-type-default-value target-type)))
                  (if (eq? &no-default-value default)
                      (missing-target-error svc target-type)
                      (let ((new (service target-type)))
                        (values (cons new result)
                                (vhash-consq target-type new instances))))))
               (_
                (values result instances))))
           result
           instances
           (service-type-extensions (service-kind svc))))

  (let loop ((services services))
    (define instances
      (fold (lambda (service result)
              (vhash-consq (service-kind service) service
                           result))
            vlist-null services))

    (define adjusted
      (fold2 adjust-service-list
             services instances
             services))

    ;; If we instantiated services, they might in turn depend on missing
    ;; services.  Loop until we've reached fixed point.
    (if (= (length adjusted) (vlist-length instances))
        adjusted
        (loop adjusted))))

(define* (fold-services services
                        #:key (target-type system-service-type))
  "Fold SERVICES by propagating their extensions down to the root of type
TARGET-TYPE; return the root service adjusted accordingly."
  (define dependents
    (service-back-edges services))

  (define (matching-extension target)
    (let ((target (service-kind target)))
      (match-lambda
        (($ <service-extension> type)
         (eq? type target)))))

  (define (apply-extension target)
    (lambda (service)
      (match (find (matching-extension target)
                   (service-type-extensions (service-kind service)))
        (($ <service-extension> _ compute)
         (compute (service-value service))))))

  (match (filter (lambda (service)
                   (eq? (service-kind service) target-type))
                 services)
    ((sink)
     ;; Use the state monad to keep track of already-visited services in the
     ;; graph and to memoize their value once folded.
     (run-with-state
         (let loop ((sink sink))
           (mlet %state-monad ((visited (current-state)))
             (match (vhash-assq sink visited)
               (#f
                (mlet* %state-monad
                    ((dependents (mapm %state-monad loop (dependents sink)))
                     (visited    (current-state))
                     (extensions -> (map (apply-extension sink) dependents))
                     (extend     -> (service-type-extend (service-kind sink)))
                     (compose    -> (service-type-compose (service-kind sink)))
                     (params     -> (service-value sink))
                     (service
                      ->
                      ;; Distinguish COMPOSE and EXTEND because PARAMS typically
                      ;; has a different type than the elements of EXTENSIONS.
                      (if extend
                          (service (service-kind sink)
                                   (extend params (compose extensions)))
                          sink)))
                  (mbegin %state-monad
                    (set-current-state (vhash-consq sink service visited))
                    (return service))))
               ((_ . service)                     ;SINK was already visited
                (return service)))))
       vlist-null))
    (()
     (raise
      (make-compound-condition
       (condition (&missing-target-service-error
                   (service #f)
                   (target-type target-type)))
       (formatted-message (G_ "service of type '~a' not found")
                          (service-type-name target-type)))))
    (x
     (raise
      (condition (&ambiguous-target-service-error
                  (service #f)
                  (target-type target-type))
                 (&message
                  (message
                   (format #f
                           (G_ "more than one target service of type '~a'")
                           (service-type-name target-type)))))))))

(define (remove-service-extensions type lst)
  "Return TYPE, a service type, without any of the service extensions
targeting one of the types in LST."
  (service-type
   (inherit type)
   (extensions (remove (lambda (extension)
                         (memq (service-extension-target extension) lst))
                       (service-type-extensions type)))))

(define-syntax-parameter for-home?
  ;; Whether the configuration being defined is for a Home service.
  (identifier-syntax #f))

(define-syntax-rule (for-home exp ...)
  "Mark EXP, which typically defines a service configuration, as targeting a
Home service rather than a System service."
  (syntax-parameterize ((for-home? (identifier-syntax #t)))
    exp ...))

(define-with-syntax-properties (validate-service-list (value properties))
  (%validate-service-list value properties))

(define (%validate-service-list value properties)
  (match value
    (((? service?) ...) value)
    (_
     (raise
      (make-compound-condition
       (condition
        (&error-location
         (location (source-properties->location properties))))
       (formatted-message
        (G_ "'services' field must contain a list of services")))))))

;;; services.scm ends here.
