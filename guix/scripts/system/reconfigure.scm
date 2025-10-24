;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2022, 2024-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016, 2017, 2018 Chris Marusich <cmmarusich@gmail.com>
;;; Copyright © 2017 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2022 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (guix scripts system reconfigure)
  #:autoload   (gnu packages gnupg) (guile-gcrypt)
  #:use-module (gnu bootloader)
  #:use-module (gnu services)
  #:use-module (gnu services herd)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:autoload   (gnu system file-systems) (file-system-device)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (guix monads)
  #:use-module (guix store)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (guix channels)
  #:autoload   (guix git) (update-cached-checkout)
  #:use-module (guix i18n)
  #:use-module (guix diagnostics)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module ((guix config) #:select (%guix-package-name))
  #:export (switch-system-program
            switch-to-system

            running-services
            upgrade-services-program
            upgrade-shepherd-services

            kexec-loading-program
            load-system-for-kexec

            install-bootloader-program
            install-bootloader

            check-forward-update
            ensure-forward-reconfigure
            warn-about-backward-reconfigure))

;;; Commentary:
;;;
;;; This module implements the "effectful" parts of system
;;; reconfiguration. Although building a system derivation is a pure
;;; operation, a number of impure operations must be carried out for the
;;; system configuration to be realized -- chiefly, creation of generation
;;; symlinks and invocation of activation scripts.
;;;
;;; Code:


;;;
;;; Profile creation.
;;;

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (_ #f)))

(define* (switch-system-program os #:optional profile)
  "Return an executable store item that, upon being evaluated, will create a
new generation of PROFILE pointing to the directory of OS, switch to it
atomically, and run OS's activation script."
  (program-file
   "switch-to-system.scm"
   (with-extensions (list guile-gcrypt)
     (with-imported-modules `(,@(source-module-closure
                                 '((guix profiles)
                                   (guix utils))
                                 #:select? not-config?)
                              ((guix config) => ,(make-config.scm)))
       #~(begin
           (use-modules (guix build utils)
                        (guix config)
                        (guix profiles)
                        (guix utils))

           (define profile
             (or #$profile (string-append %state-directory "/profiles/system")))

           (let* ((number (1+ (generation-number profile)))
                  (generation (generation-file-name profile number)))
             (switch-symlinks generation #$os)
             (switch-symlinks profile generation)
             (setenv "GUIX_NEW_SYSTEM" generation)
             (primitive-load #$(operating-system-activation-script os))))))))

(define* (switch-to-system eval os #:optional profile)
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
create a new generation of PROFILE pointing to the directory of OS, switch to
it atomically, and run OS's activation script."
  (eval #~(parameterize ((current-warning-port (%make-void-port "w")))
            (primitive-load #$(switch-system-program os profile)))))


;;;
;;; Services.
;;;

(define (running-services eval)
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
return the <live-service> objects that are currently running on MACHINE."
  (define exp
    (with-imported-modules '((gnu services herd))
      #~(begin
          (use-modules (gnu services herd)
                       (ice-9 match))

          (let ((services (current-services)))
            (and services
                 (map (lambda (service)
                        (list (live-service-provision service)
                              (live-service-requirement service)
                              (live-service-transient? service)
                              (match (live-service-running service)
                                (#f #f)
                                (#t #t)
                                ((? number? pid) pid)
                                (_ #t))))         ;not serializable
                      services))))))

  (mlet %store-monad ((services (eval exp)))
    (return (map (match-lambda
                   ((provision requirement transient? running)
                    (live-service provision requirement
                                  transient? running)))
                 services))))

;; XXX: Currently, this does NOT attempt to restart running services. See
;; <https://issues.guix.info/issue/33508> for details.
(define (upgrade-services-program service-files to-start to-unload to-restart)
  "Return an executable store item that, upon being evaluated, will upgrade
the Shepherd (PID 1) by unloading obsolete services and loading new
services. SERVICE-FILES is a list of Shepherd service files to load, and
TO-START, TO-UNLOAD, and TO-RESTART are lists of the Shepherd services'
canonical names (symbols)."
  (program-file
   "upgrade-shepherd-services.scm"
   (with-imported-modules '((gnu services herd))
    #~(begin
        (use-modules (gnu services herd)
                     (srfi srfi-1))

        ;; Load the service files for any new services.
        ;; Silence messages coming from shepherd such as "Evaluating
        ;; expression ..." since they are unhelpful.
        (parameterize ((shepherd-message-port (%make-void-port "w")))
          (load-services '#$service-files))

        ;; Unload obsolete services and start new services.
        (for-each unload-service '#$to-unload)
        (for-each start-service '#$to-start)))))

(define (kexec-loading-program os)
  "Return a program that calls 'kexec_file_load' to allow rebooting into OS
via 'kexec'."
  (let ((root-device (file-system-device
                      (operating-system-root-file-system os))))
    (program-file
     "kexec-load-system.scm"
     (with-imported-modules '((guix build syscalls))
       #~(begin
           (use-modules (guix build syscalls))

           (let ((kernel (open-fdes #$(operating-system-kernel-file os)
                                    O_RDONLY))
                 (initrd (open-fdes #$(operating-system-initrd-file os)
                                    O_RDONLY)))
             (kexec-load-file kernel initrd
                              (string-join
                               (list #$@(operating-system-kernel-arguments
                                         os root-device))))))))))

(define* (upgrade-shepherd-services eval os)
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
upgrade the Shepherd (PID 1) by unloading obsolete services and loading new
services as defined by OS."
  (define target-services
    (shepherd-configuration-services
     (service-value
      (fold-services (operating-system-services os)
                     #:target-type shepherd-root-service-type))))

  (mlet* %store-monad ((live-services (running-services eval)))
    (let* ((to-unload to-restart
                      (shepherd-service-upgrade live-services target-services))
           (to-unload  (map live-service-canonical-name to-unload))
           (to-restart (map live-service-canonical-name to-restart))
           (running    (map live-service-canonical-name
                            (filter live-service-running live-services)))
           (to-start   (lset-difference eqv?
                                        (map shepherd-service-canonical-name
                                             (filter shepherd-service-auto-start?
                                                     target-services))
                                        running))
           (service-files (map shepherd-service-file target-services)))
      (eval #~(parameterize ((current-warning-port (%make-void-port "w")))
                (primitive-load #$(upgrade-services-program service-files
                                                            to-start
                                                            to-unload
                                                            to-restart)))))))

(define (load-system-for-kexec eval os)
  "Load OS so that it can be rebooted into via kexec, if supported.  Print a
warning in case of failure."
  (mlet %store-monad
      ((result (eval
                #~(and (string-contains %host-type "-linux")
                       (with-exception-handler
                           (lambda (c)
                             (define kind-and-args?
                               (exception-predicate &exception-with-kind-and-args))

                             (list 'exception
                                   (if (kind-and-args? c)
                                       (call-with-output-string
                                         (lambda (port)
                                           (print-exception port #f
                                                            (exception-kind c)
                                                            (exception-args c))))
                                       (object->string c))))
                         (lambda ()
                           (primitive-load #$(kexec-loading-program os))
                           'success)
                         #:unwind? #t)))))
    (match result
      ('success
       (return #t))
      (('exception message)
       (warning (G_ "failed to load operating system for kexec: ~a~%")
                message)
       (return #f)))))


;;;
;;; Bootloader configuration.
;;;

(define (install-bootloader-program installer disk-installer
                                    bootloader-package bootcfg
                                    bootcfg-file devices target)
  "Return an executable store item that, upon being evaluated, will install
BOOTCFG to BOOTCFG-FILE, a target file name, on DEVICES, a list of file system
devices, at TARGET, a mount point, and subsequently run INSTALLER from
BOOTLOADER-PACKAGE."
  (program-file
   "install-bootloader.scm"
   (with-extensions (list guile-gcrypt)
     (with-imported-modules `(,@(source-module-closure
                                 '((gnu build bootloader)
                                   (gnu build install)
                                   (guix store)
                                   (guix utils))
                                 #:select? not-config?)
                              ((guix config) => ,(make-config.scm)))
       #~(begin
           (use-modules (gnu build bootloader)
                        (gnu build install)
                        (guix build utils)
                        (guix store)
                        (guix utils)
                        (ice-9 binary-ports)
                        (ice-9 match)
                        (srfi srfi-34)
                        (srfi srfi-35))

           (let* ((gc-root (string-append #$target %gc-roots-directory "/bootcfg"))
                  (new-gc-root (string-append gc-root ".new")))
             ;; #$bootcfg has dependencies.
             ;; The bootloader magically loads the configuration from
             ;; (string-append #$target #$bootcfg-file) (for example
             ;; "/boot/grub/grub.cfg").
             ;; If we didn't do something special, the garbage collector
             ;; would remove the dependencies of #$bootcfg.
             ;; Register #$bootcfg as a GC root.
             ;; Preserve the previous activation's garbage collector root
             ;; until the bootloader installer has run, so that a failure in
             ;; the bootloader's installer script doesn't leave the user with
             ;; a broken installation.
             (switch-symlinks new-gc-root #$bootcfg)
             (install-boot-config #$bootcfg #$bootcfg-file #$target)
             (when (or #$installer #$disk-installer)
               (catch #t
                 (lambda ()
                   ;; The bootloader might not support installation on a
                   ;; mounted directory using the BOOTLOADER-INSTALLER
                   ;; procedure. In that case, fallback to installing the
                   ;; bootloader directly on DEVICES using the
                   ;; BOOTLOADER-DISK-IMAGE-INSTALLER procedure.
                   (if #$installer
                       (for-each (lambda (device)
                                   (#$installer #$bootloader-package device
                                                #$target))
                                 '#$devices)
                       (for-each (lambda (device)
                                   (#$disk-installer #$bootloader-package
                                                     0 device))
                                 '#$devices)))
                 (lambda args
                   (delete-file new-gc-root)
                   (match args
                     (('%exception exception)     ;Guile 3 SRFI-34 or similar
                      (raise-exception exception))
                     ((key . args)
                      (apply throw key args))))))
             ;; We are sure that the installation of the bootloader
             ;; succeeded, so we can replace the old GC root by the new
             ;; GC root now.
             (rename-file new-gc-root gc-root)))))))

(define* (install-bootloader eval configuration bootcfg
                             #:key
                             (run-installer? #t)
                             (target "/"))
  "Using EVAL, a monadic procedure taking a single G-Expression as an argument,
configure the bootloader on TARGET such that OS will be booted by default and
additional configurations specified by MENU-ENTRIES can be selected."
  (let* ((bootloader (bootloader-configuration-bootloader configuration))
         (installer (and run-installer?
                         (bootloader-installer bootloader)))
         (disk-installer (and run-installer?
                              (bootloader-disk-image-installer bootloader)))
         (package (bootloader-package bootloader))
         (devices (bootloader-configuration-targets configuration))
         (bootcfg-file (bootloader-configuration-file bootloader)))
    (eval #~(parameterize ((current-warning-port (%make-void-port "w")))
              (primitive-load #$(install-bootloader-program installer
                                                            disk-installer
                                                            #~#+package
                                                            bootcfg
                                                            bootcfg-file
                                                            devices
                                                            target))))))


;;;
;;; Downgrade detection.
;;;

(define (ensure-forward-reconfigure channel start commit relation)
  "Raise an error if RELATION is not 'ancestor, meaning that START is not an
ancestor of COMMIT, unless CHANNEL specifies a commit."
  (match relation
    ('ancestor #t)
    ('self #t)
    (_
     (raise (make-compound-condition
             (formatted-message (G_ "\
aborting reconfiguration because commit ~a of channel '~a' is not a descendant of ~a")
                                commit (channel-name channel)
                                start)
             (condition
              (&fix-hint
               (hint (G_ "Use @option{--allow-downgrades} to force
this downgrade.")))))))))

(define (warn-about-backward-reconfigure channel start commit relation)
  "Warn about non-forward updates of CHANNEL from START to COMMIT, without
aborting."
  (match relation
    ((or 'ancestor 'self)
     #t)
    ('descendant
     (warning (G_ "rolling back channel '~a' from ~a to ~a~%")
              (channel-name channel) start commit))
    ('unrelated
     (warning (G_ "moving channel '~a' from ~a to unrelated commit ~a~%")
              (channel-name channel) start commit))))

(define (channel-relations old new)
  "Return a list of channel/relation pairs, where each relation is a symbol as
returned by 'commit-relation' denoting how commits of channels in OLD relate
to commits of channels in NEW."
  (filter-map (lambda (old)
                (let ((new (find (lambda (channel)
                                   (eq? (channel-name channel)
                                        (channel-name old)))
                                 new)))
                  (and new
                       (let ((checkout commit relation
                                       (update-cached-checkout
                                        (channel-url new)
                                        #:ref `(commit . ,(channel-commit new))
                                        #:starting-commit (channel-commit old)
                                        #:check-out? #f)))
                         (list new
                               (channel-commit old) (channel-commit new)
                               relation)))))
              old))

(define* (check-forward-update #:optional
                               (validate-reconfigure
                                ensure-forward-reconfigure)
                               #:key
                               (current-channels
                                (system-provenance "/run/current-system")))
  "Call VALIDATE-RECONFIGURE passing it, for each channel, the channel, the
currently-deployed commit (from CURRENT-CHANNELS, which is as returned by
'guix system describe' by default) and the target commit (as returned by 'guix
describe')."
  (define new
    ((@ (guix describe) current-channels)))

  (when (null? current-channels)
    (warning (G_ "cannot determine provenance for current system~%")))
  (when (and (null? new) (not (getenv "GUIX_UNINSTALLED")))
    (warning (G_ "cannot determine provenance of ~a~%") %guix-package-name))

  (for-each (match-lambda
              ((channel old new relation)
               (validate-reconfigure channel old new relation)))
            (channel-relations current-channels new)))
