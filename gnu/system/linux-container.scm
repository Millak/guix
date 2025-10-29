;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 David Thompson <davet@gnu.org>
;;; Copyright © 2016-2017, 2019-2023, 2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2019 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2020 Google LLC
;;; Copyright © 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2023 Pierre Langlois <pierre.langlois@gmx.com>
;;; Copyright © 2024 Leo Nikkilä <hello@lnikki.la>
;;; Copyright © 2024 Andreas Enge <andreas@enge.fr>
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

(define-module (gnu system linux-container)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:use-module (guix config)
  #:use-module (guix store)
  #:use-module (guix gexp)
  #:use-module (guix derivations)
  #:use-module (guix monads)
  #:use-module (guix modules)
  #:use-module (gnu build linux-container)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu system)
  #:use-module (gnu system file-systems)
  #:use-module (gnu packages linux)
  #:export (system-container
            containerized-operating-system
            container-script
            eval/container))

(define* (container-essential-services os #:key shared-network?)
  "Return a list of essential services corresponding to OS, a
non-containerized OS.  This procedure essentially strips essential services
from OS that are needed on the bare metal and not in a container."
  (define base
    (remove (lambda (service)
              (memq (service-kind service)
                    (cons* (service-kind %linux-bare-metal-service)
                           firmware-service-type
                           system-service-type
                           (if shared-network?
                               (list hosts-service-type)
                               '()))))
            (operating-system-essential-services os)))

  (cons (service system-service-type
                 `(("locale" ,(operating-system-locale-directory os))))
        ;; If network is to be shared with the host, remove network
        ;; configuration files from etc-service.
        (if shared-network?
            (modify-services base
              (etc-service-type
               files => (remove
                         (match-lambda
                           ((filename _)
                            (member filename
                                    (map basename %network-configuration-files))))
                         files)))
            base)))

(define dummy-networking-service-type
  (shepherd-service-type
   'dummy-networking
   (lambda (provision)
     (shepherd-service
      (documentation "Provide loopback and networking without actually
doing anything.")
      (provision provision)
      (start #~(const #t))))
   '(loopback networking)
   (description "Provide loopback and networking without actually doing
anything.  This service is used by guest systems running in containers, where
networking support is provided by the host.")))

(define %nscd-container-caches
  ;; Similar to %nscd-default-caches but with smaller cache sizes. This allows
  ;; many containers to coexist on the same machine without exhausting RAM.
  (map (lambda (cache)
         (nscd-cache
          (inherit cache)
          (max-database-size (expt 2 18)))) ;256KiB
       %nscd-default-caches))

(define* (containerized-operating-system os mappings
                                         #:key
                                         shared-network?
                                         (extra-file-systems '()))
  "Return an operating system based on OS for use in a Linux container
environment.  MAPPINGS is a list of <file-system-mapping> to realize in the
containerized OS.  EXTRA-FILE-SYSTEMS is a list of file systems to add to OS."
  (define user-file-systems
    (remove (lambda (fs)
              (let ((target (file-system-mount-point fs))
                    (source (file-system-device fs)))
                (or (string=? target (%store-prefix))
                    (string=? target "/")
                    (and (string? source)
                         (string-prefix? "/dev/" source))
                    (string-prefix? "/dev/" target)
                    (string-prefix? "/sys/" target))))
            (operating-system-file-systems os)))

  (define (mapping->fs fs)
    (file-system (inherit (file-system-mapping->bind-mount fs))
      (needed-for-boot? #t)))

  (define services-to-drop
    ;; Service types to filter from the original operating-system. Some of
    ;; these make no sense in a container (e.g., those that access
    ;; /dev/tty[0-9]), while others just need to be reinstantiated with
    ;; different configs that are better suited to containers.
    (append (list console-font-service-type
                  mingetty-service-type
                  agetty-service-type)
            (if shared-network?
                ;; Replace these with dummy-networking-service-type below.
                (list
                 static-networking-service-type
                 dhcpcd-service-type
                 network-manager-service-type
                 connman-service-type)
                (list static-networking-service-type)))) ;loopback

  (define services-to-add
    ;; Many Guix services depend on a 'networking' shepherd
    ;; service, so make sure to provide a dummy 'networking'
    ;; service when we are sure that networking is already set up
    ;; in the host and can be used.  That prevents double setup.
    (if shared-network?
        (list (service dummy-networking-service-type
                       '(loopback networking)))
        (list (service dummy-networking-service-type
                       '(loopback)))))

  (define os-with-base-essential-services
    (operating-system
      (inherit os)
      (kernel %dummy-linux-kernel-for-container)
      (swap-devices '()) ; disable swap
      (services
       (append services-to-add
               (filter-map
                 (lambda (s)
                   (let ((kind (service-kind s))
                         (value (service-value s)))
                        (cond ((memq kind services-to-drop)
                               #f)
                              ((eq? nscd-service-type kind)
                               (service nscd-service-type
                                        (nscd-configuration
                                         (inherit value)
                                         (caches %nscd-container-caches))))
                              ((and (eq? guix-service-type kind)
                                    (eq? (guix-configuration-chroot? value)
                                         'default))
                               ;; If chroot? is 'default, it should become #f
                               ;; so that guix-daemon can build things even in
                               ;; Docker without '--privileged'.
                               (service guix-service-type
                                        (guix-configuration
                                         (inherit value)
                                         (chroot? #f))))
                              (else s))))
                           (operating-system-user-services os))))
      (file-systems (append (map mapping->fs
                                 (if shared-network?
                                     (append %network-file-mappings mappings)
                                     mappings))
                            extra-file-systems
                            user-file-systems

                            ;; Provide a dummy root file system so we can create
                            ;; a 'boot-parameters' file.
                            (list (file-system
                                    (mount-point "/")
                                    (device "nothing")
                                    (type "dummy")))))))

  ;; `essential-services' is thunked, we need to evaluate it separately.
  (operating-system
    (inherit os-with-base-essential-services)
    (essential-services (container-essential-services
                         os-with-base-essential-services
                         #:shared-network? shared-network?))))

(define* (container-script os #:key (mappings '()) shared-network?)
  "Return a derivation of a script that runs OS as a Linux container.
MAPPINGS is a list of <file-system> objects that specify the files/directories
that will be shared with the host system."
  (define (mountable-file-system? file-system)
    ;; Return #t if FILE-SYSTEM should be mounted in the container.
    (and (not (string=? "/" (file-system-mount-point file-system)))
         (file-system-needed-for-boot? file-system)))

  (define (os-file-system-specs os)
    (map file-system->spec
         (filter mountable-file-system?
                 (operating-system-file-systems os))))

  (let* ((os (containerized-operating-system
              os (cons %store-mapping mappings)
              #:shared-network? shared-network?
              #:extra-file-systems %container-file-systems))
         (specs (os-file-system-specs os)))

    (define script
      (with-imported-modules (source-module-closure
                              '((guix build utils)
                                (gnu build linux-container)
                                (guix i18n)
                                (guix diagnostics)))
        #~(begin
            (use-modules (gnu build linux-container)
                         (gnu system file-systems) ;spec->file-system
                         (guix build utils)
                         (guix i18n)
                         (guix diagnostics)
                         (srfi srfi-1)
                         (srfi srfi-37)
                         (ice-9 match))

            (define (show-help)
              (display (G_ "Usage: run-container [OPTION ...]
Run the container with the given options."))
              (newline)
              (display (G_ "
      --share=SPEC       share host file system with read/write access
                         according to SPEC"))
              (display (G_ "
      --expose=SPEC      expose host file system directory as read-only
                         according to SPEC"))
              (newline)
              (display (G_ "
  -h, --help             display this help and exit"))
              (newline))

            (define %options
              ;; Specifications of the command-line options.
              (list (option '(#\h "help") #f #f
                            (lambda args
                              (show-help)
                              (exit 0)))
                    (option '("share") #t #f
                            (lambda (opt name arg result)
                              (alist-cons 'file-system-mapping
                                          (specification->file-system-mapping arg #t)
                                          result)))
                    (option '("expose") #t #f
                            (lambda (opt name arg result)
                              (alist-cons 'file-system-mapping
                                          (specification->file-system-mapping arg #f)
                                          result)))))

            (define (parse-options args options)
              (args-fold args options
                         (lambda (opt name arg . rest)
                           (report-error (G_ "~A: unrecognized option~%") name)
                           (exit 1))
                         (lambda (op res) (cons op res))
                         '()))

            (define (explain pid)
              ;; XXX: We can't quite call 'bindtextdomain' so there's actually
              ;; no i18n.
              ;; XXX: Should we really give both options? 'guix container exec'
              ;; is a more verbose command.  Hard to fail to enter the container
              ;; when we list two options.
              (info (G_ "system container is running as PID ~a~%") pid)
              (info (G_ "Run 'sudo guix container exec ~a /run/current-system/profile/bin/bash --login'\n")
                    pid)
              (info (G_ "or run 'sudo nsenter -a -t ~a' to get a shell into it.~%") pid)
              (newline (guix-warning-port)))

            (let* ((opts (parse-options (cdr (command-line)) %options))
                   (mappings (filter-map (match-lambda
                                           (('file-system-mapping . mapping) mapping)
                                           (_ #f))
                                         opts))
                   (file-systems
                    (filter-map (lambda (fs)
                                  (let ((flags (file-system-flags fs)))
                                    (and (or (not (memq 'bind-mount flags))
                                             (file-exists? (file-system-device fs)))
                                         fs)))
                                (append (map file-system-mapping->bind-mount mappings)
                                        (map spec->file-system '#$specs)))))
              (call-with-container file-systems
                (lambda ()
                  (setenv "HOME" "/root")
                  (setenv "TMPDIR" "/tmp")
                  (setenv "GUIX_NEW_SYSTEM" #$os)
                  (for-each mkdir-p '("/run" "/bin" "/etc" "/home" "/var"))
                  (primitive-load (string-append #$os "/boot")))
                ;; A range of 65536 uid/gids is used to cover 16 bits worth of
                ;; users and groups, which is sufficient for most cases.
                ;;
                ;; See: http://www.freedesktop.org/software/systemd/man/systemd-nspawn.html#--private-users=
                #:host-uids 65536
                #:namespaces (if #$shared-network?
                                 (delq 'net %namespaces)
                                 %namespaces)

                ;; XXX: Work around <https://issues.guix.gnu.org/78356>.
                #:lock-mounts? #f

                #:writable-root? #t
                #:process-spawned-hook explain)))))

    (gexp->script "run-container" script)))

(define* (eval/container exp
                         #:key
                         (populate-file-system (const #t))
                         writable-root?
                         (mappings '())
                         (mounts '())
                         (namespaces %namespaces)
                         (guest-uid 0) (guest-gid 0))
  "Evaluate EXP, a gexp, in a new process executing in separate namespaces as
listed in NAMESPACES.  Add MOUNTS, a list of <file-system>, and MAPPINGS, a
list of <file-system-mapping>, to the set of directories visible in the
process's mount namespace.  Inside the namespaces, run code as GUEST-UID and
GUEST-GID.  Return the process' exit status as a monadic value.

This is useful to implement processes that, unlike derivations, are not
entirely pure and need to access the outside world or to perform side
effects."
  (mlet %store-monad ((lowered (lower-gexp exp)))
    (define inputs
      (cons (lowered-gexp-guile lowered)
            (lowered-gexp-inputs lowered)))

    (define items
      (append (append-map derivation-input-output-paths inputs)
              (lowered-gexp-sources lowered)))

    (mbegin %store-monad
      (built-derivations inputs)
      (mlet %store-monad ((closure ((store-lift requisites) items)))
        (return (call-with-container (append mounts
                                             (map file-system-mapping->bind-mount
                                                  (append (map (lambda (item)
                                                                 (file-system-mapping
                                                                  (source item)
                                                                  (target source)))
                                                               closure)
                                                          mappings)))
                  (lambda ()
                    (apply execl
                           (string-append (derivation-input-output-path
                                           (lowered-gexp-guile lowered))
                                          "/bin/guile")
                           "guile"
                           (append (append-map (lambda (directory)
                                                 `("-L" ,directory))
                                               (lowered-gexp-load-path lowered))
                                   (append-map (lambda (directory)
                                                 `("-C" ,directory))
                                               (lowered-gexp-load-compiled-path
                                                lowered))
                                   (list "-c"
                                         (object->string
                                          (lowered-gexp-sexp lowered))))))
                  #:writable-root? writable-root?
                  #:populate-file-system populate-file-system
                  #:namespaces namespaces
                  #:guest-uid guest-uid
                  #:guest-gid guest-gid))))))
