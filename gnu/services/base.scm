;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2015, 2016, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016, 2017 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019 John Soo <jsoo1@asu.edu>
;;; Copyright © 2019, 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 Hui Lu <luhuins@163.com>
;;; Copyright © 2021-2023, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2025 muradm <mail@muradm.net>
;;; Copyright © 2022 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2022 Justin Veilleux <terramorpha@cock.li>
;;; Copyright © 2022 ( <paren@disroot.org>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2024 Zheng Junjie <873216071@qq.com>
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

(define-module (gnu services base)
  #:use-module (guix store)
  #:use-module (guix deprecation)
  #:autoload   (guix diagnostics) (warning formatted-message &fix-hint)
  #:autoload   (guix i18n) (G_)
  #:use-module (guix combinators)
  #:use-module (guix utils)
  #:use-module (gnu services)
  #:use-module (gnu services admin)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services sysctl)
  #:use-module (gnu system pam)
  #:use-module (gnu system shadow)                ; 'user-account', etc.
  #:use-module (gnu system uuid)
  #:use-module (gnu system file-systems)          ; 'file-system', etc.
  #:use-module (gnu system keyboard)
  #:use-module (gnu system mapped-devices)
  #:use-module ((gnu system linux-initrd)
                #:select (file-system-packages))
  #:use-module (gnu packages admin)
  #:use-module ((gnu packages linux)
                #:select (alsa-utils btrfs-progs crda eudev
                          e2fsprogs f2fs-tools fuse gpm kbd lvm2 rng-tools
                          util-linux xfsprogs))
  #:use-module (gnu packages bash)
  #:use-module ((gnu packages base)
                #:select (coreutils glibc glibc/hurd
                          glibc-utf8-locales
                          libc-utf8-locales-for-target
                          make-glibc-utf8-locales
                          tar canonical-package))
  #:use-module ((gnu packages cross-base)
                #:select (cross-libc))
  #:use-module ((gnu packages compression) #:select (gzip))
  #:use-module (gnu packages fonts)
  #:autoload   (gnu packages guile-xyz) (guile-netlink)
  #:autoload   (gnu packages hurd) (hurd)
  #:use-module (gnu packages package-management)
  #:use-module ((gnu packages gnupg) #:select (guile-gcrypt))
  #:use-module ((gnu packages disk)
                #:select (dosfstools))
  #:use-module ((gnu packages file-systems)
                #:select (bcachefs-tools exfat-utils jfsutils zfs))
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages terminals)
  #:use-module ((gnu packages wm) #:select (sway))
  #:use-module ((gnu build file-systems)
                #:select (mount-flags->bit-mask
                          swap-space->flags-bit-mask))
  #:autoload   (guix channels) (%default-channels channel->code)
  #:use-module (guix gexp)
  #:use-module ((guix packages) #:select (package-version))
  #:use-module (guix records)
  #:use-module (guix modules)
  #:use-module (guix pki)
  #:use-module ((guix self) #:select (make-config.scm))
  #:use-module (guix diagnostics)
  #:use-module (guix i18n)
  #:autoload   (guix utils) (target-hurd?)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (ice-9 match)
  #:use-module (ice-9 format)
  #:re-export (user-processes-service-type        ;backwards compatibility
               %default-substitute-urls)
  #:export (fstab-service-type
            root-file-system-service
            file-system-service-type
            file-system-utilities
            swap-service
            host-name-service  ; deprecated
            host-name-service-type
            %default-console-font
            console-font-service-type
            console-font-service
            virtual-terminal-service-type

            host
            host?
            host-address
            host-canonical-name
            host-aliases
            hosts-service-type

            static-networking
            static-networking?
            static-networking-addresses
            static-networking-links
            static-networking-routes
            static-networking-requirement

            network-address
            network-address?
            network-address-device
            network-address-value
            network-address-ipv6?

            network-link
            network-link?
            network-link-name
            network-link-type
            network-link-arguments

            network-route
            network-route?
            network-route-destination
            network-route-source
            network-route-device
            network-route-ipv6?
            network-route-gateway

            static-networking-service
            static-networking-service-type

            %loopback-static-networking
            %qemu-static-networking

            udev-configuration
            udev-configuration?
            udev-configuration-udev
            udev-configuration-debug?
            udev-configuration-rules
            udev-configuration-hardware
            udev-service-type
            udev-service  ; deprecated
            udev-rule
            udev-hardware
            file->udev-rule
            file->udev-hardware
            udev-rules-service
            udev-hardware-service

            login-configuration
            login-configuration?
            login-service-type
            login-service  ; deprecated

            agetty-configuration
            agetty-configuration?
            agetty-service  ; deprecated
            agetty-service-type

            mingetty-configuration
            mingetty-configuration-tty
            mingetty-configuration-auto-login
            mingetty-configuration-login-program
            mingetty-configuration-login-pause?
            mingetty-configuration-clear-on-logout?
            mingetty-configuration-mingetty
            mingetty-configuration-delay
            mingetty-configuration-print-issue
            mingetty-configuration-print-hostname
            mingetty-configuration-nice
            mingetty-configuration-working-directory
            mingetty-configuration-root-directory
            mingetty-configuration-shepherd-requirement
            mingetty-configuration?
            mingetty-service  ; deprecated
            mingetty-service-type

            %nscd-default-caches
            %nscd-default-configuration  ; deprecated

            nscd-configuration
            nscd-configuration?

            nscd-cache
            nscd-cache?

            nscd-service-type
            nscd-service  ; deprecated

            syslog-configuration
            syslog-configuration?
            syslog-service  ; deprecated
            syslog-service-type
            %default-syslog.conf

            %default-authorized-guix-keys
            guix-configuration
            guix-configuration?

            guix-configuration-guix
            guix-configuration-build-group
            guix-configuration-build-accounts
            guix-configuration-build-machines
            guix-configuration-chroot?
            guix-configuration-authorize-key?
            guix-configuration-authorized-keys
            guix-configuration-use-substitutes?
            guix-configuration-substitute-urls
            guix-configuration-generate-substitute-key?
            guix-configuration-channels
            guix-configuration-extra-options
            guix-configuration-log-file
            guix-configuration-environment

            guix-extension
            guix-extension?
            guix-extension-authorized-keys
            guix-extension-substitute-urls
            guix-extension-chroot-directories

            guix-service-type
            guix-publish-configuration
            guix-publish-configuration?
            guix-publish-configuration-guix
            guix-publish-configuration-port
            guix-publish-configuration-host
            guix-publish-configuration-compression
            guix-publish-configuration-nar-path
            guix-publish-configuration-cache
            guix-publish-configuration-ttl
            guix-publish-configuration-negative-ttl
            guix-publish-service-type

            gpm-configuration
            gpm-configuration?
            gpm-service-type

            urandom-seed-service-type

            rngd-configuration
            rngd-configuration?
            rngd-service-type
            rngd-service  ; deprecated

            kmscon-configuration
            kmscon-configuration?
            kmscon-service-type

            pam-limits-service-type
            pam-limits-service  ; deprecated

            greetd-service-type
            greetd-configuration
            greetd-terminal-configuration
            greetd-user-session
            greetd-agreety-session
            greetd-wlgreet-session  ; deprecated
            greetd-wlgreet-configuration
            greetd-wlgreet-sway-session
            greetd-gtkgreet-sway-session

            %base-services))

;;; Commentary:
;;;
;;; Base system services---i.e., services that 99% of the users will want to
;;; use.
;;;
;;; Code:



;;;
;;; File systems.
;;;

(define (file-system->fstab-entry file-system)
  "Return a @file{/etc/fstab} entry for @var{file-system}."
  (string-append (match (file-system-device file-system)
                   ((? file-system-label? label)
                    (string-append "LABEL="
                                   (file-system-label->string label)))
                   ((? uuid? uuid)
                    (string-append "UUID=" (uuid->string uuid)))
                   ((? string? device)
                    device))
                 "\t"
                 (file-system-mount-point file-system) "\t"
                 (file-system-type file-system) "\t"
                 (or (file-system-options file-system) "defaults") "\t"

                 ;; XXX: Omit the 'fs_freq' and 'fs_passno' fields because we
                 ;; don't have anything sensible to put in there.
                 ))

(define (file-systems->fstab file-systems)
  "Return a @file{/etc} entry for an @file{fstab} describing
@var{file-systems}."
  `(("fstab" ,(plain-file "fstab"
                          (string-append
                           "\
# This file was generated from your Guix configuration.  Any changes
# will be lost upon reboot or reconfiguration.\n\n"
                           (string-join (map file-system->fstab-entry
                                             file-systems)
                                        "\n")
                           "\n")))))

(define fstab-service-type
  ;; The /etc/fstab service.
  (service-type (name 'fstab)
                (extensions
                 (list (service-extension etc-service-type
                                          file-systems->fstab)))
                (compose concatenate)
                (extend append)
                (description
                 "Populate the @file{/etc/fstab} based on the given file
system objects.")))

(define %root-file-system-shepherd-service
  (shepherd-service
   (documentation "Take care of the root file system.")
   (provision '(root-file-system))
   (start #~(const #t))
   (stop #~(lambda _
             ;; Return #f if successfully stopped.
             (sync)

             (let ((null (%make-void-port "w")))
               ;; Redirect the default output ports.
               (set-current-output-port null)
               (set-current-error-port null)

               ;; Close /dev/console.
               (for-each close-fdes '(0 1 2))

               ;; At this point, there should be no open files left so the
               ;; root file system can be re-mounted read-only.
               (let loop ((n 10))
                 (unless (catch 'system-error
                           (lambda ()
                             (mount #f "/" #f
                                    (logior MS_REMOUNT MS_RDONLY)
                                    #:update-mtab? #f)
                             #t)
                           (const #f))
                   (unless (zero? n)
                     ;; Yield to the other fibers.  That gives logging fibers
                     ;; an opportunity to close log files so the 'mount' call
                     ;; doesn't fail with EBUSY.
                     ((@ (fibers) sleep) 1)
                     (loop (- n 1)))))

               #f)))
   (respawn? #f)))

(define root-file-system-service-type
  (shepherd-service-type 'root-file-system
                         (const %root-file-system-shepherd-service)
                         (description "Take care of syncing the root file
system and of remounting it read-only when the system shuts down.")))

(define (root-file-system-service)
  "Return a service whose sole purpose is to re-mount read-only the root file
system upon shutdown (aka. cleanly \"umounting\" root.)

This service must be the root of the service dependency graph so that its
'stop' action is invoked when shepherd is the only process left."
  (service root-file-system-service-type #f))

(define (file-system->shepherd-service-name file-system)
  "Return the symbol that denotes the service mounting and unmounting
FILE-SYSTEM."
  (symbol-append 'file-system-
                 (string->symbol (file-system-mount-point file-system))))

(define (mapped-device->shepherd-service-name md)
  "Return the symbol that denotes the shepherd service of MD, a <mapped-device>."
  (symbol-append 'device-mapping-
                 (string->symbol (string-join
                                  (mapped-device-targets md) "-"))))

(define dependency->shepherd-service-name
  (match-lambda
    ((? mapped-device? md)
     (mapped-device->shepherd-service-name md))
    ((? file-system? fs)
     (file-system->shepherd-service-name fs))))

(define (file-system-shepherd-service file-system)
  "Return the shepherd service for @var{file-system}, or @code{#f} if
@var{file-system} is not auto-mounted or doesn't have its mount point created
upon boot."
  (let ((target  (file-system-mount-point file-system))
        (create? (file-system-create-mount-point? file-system))
        (mount?  (file-system-mount? file-system))
        (dependencies (file-system-dependencies file-system))
        (requirements (file-system-shepherd-requirements file-system))
        (packages (file-system-packages (list file-system))))
    (and (or mount? create?)
         (with-imported-modules (source-module-closure
                                 '((gnu build file-systems)))
           (shepherd-service
            (provision (list (file-system->shepherd-service-name file-system)))
            (requirement `(root-file-system
                           udev
                           ,@(map dependency->shepherd-service-name dependencies)
                           ,@requirements))
            (documentation "Check, mount, and unmount the given file system.")
            (start #~(lambda args
                       #$(if create?
                             #~(mkdir-p #$target)
                             #t)

                       #$(if mount?
                             #~(let (($PATH (getenv "PATH")))
                                 ;; Make sure fsck.ext2 & co. can be found.
                                 (dynamic-wind
                                   (lambda ()
                                     ;; Don’t display the PATH settings.
                                     (with-output-to-port (%make-void-port "w")
                                       (lambda ()
                                         (set-path-environment-variable "PATH"
                                                                        '("bin" "sbin")
                                                                        '#$packages))))
                                   (lambda ()
                                     (mount-file-system
                                      (spec->file-system
                                       '#$(file-system->spec file-system))
                                      #:root "/"))
                                   (lambda ()
                                     (setenv "PATH" $PATH))))
                             #t)
                       #t))
            (stop #~(lambda args
                      ;; Normally there are no processes left at this point, so
                      ;; TARGET can be safely unmounted.

                      ;; Make sure PID 1 doesn't keep TARGET busy.
                      (chdir "/")

                      #$(if (file-system-mount-may-fail? file-system)
                            #~(catch 'system-error
                                (lambda () (umount #$target))
                                (const #f))
                            #~(umount #$target))
                      #f))

            ;; We need additional modules.
            (modules `(((gnu build file-systems)
                        #:select (mount-file-system))
                       (gnu system file-systems)
                       ,@%default-modules)))))))

(define (file-system-shepherd-services file-systems)
  "Return the list of Shepherd services for FILE-SYSTEMS."
  (let* ((file-systems (filter (lambda (x)
                                 (or (file-system-mount? x)
                                     (file-system-create-mount-point? x)))
                               file-systems)))

    (define sink
      (shepherd-service
       (provision '(file-systems))
       (requirement (cons* 'root-file-system 'user-file-systems
                           (map file-system->shepherd-service-name
                                ;; Do not require file systems with Shepherd
                                ;; requirements to provision
                                ;; 'file-systems. Many Shepherd services
                                ;; require 'file-systems, so we would likely
                                ;; deadlock.
                                (filter (lambda (file-system)
                                          (null? (file-system-shepherd-requirements file-system)))
                                        file-systems))))
       (documentation "Target for all the initially-mounted file systems")
       (start #~(const #t))
       (stop #~(const #f))))

    (define known-mount-points
      (map file-system-mount-point file-systems))

    (define user-unmount
      (shepherd-service
       (documentation "Unmount manually-mounted file systems.")
       (provision '(user-file-systems))
       (start #~(const #t))
       (stop #~(lambda args
                 (define (known? mount-point)
                   (member mount-point
                           ;; Count file systems mounted by the initrd to as
                           ;; "known" and not user-mounted file systems.
                           (cons* "/" "/dev" "/proc" "/sys"
                                  '#$known-mount-points)))

                 ;; Make sure we don't keep the user's mount points busy.
                 (chdir "/")

                 (for-each (lambda (mount-point)
                             (format #t "unmounting '~a'...~%" mount-point)
                             (catch 'system-error
                               (lambda ()
                                 (umount mount-point))
                               (lambda args
                                 (let ((errno (system-error-errno args)))
                                   (format #t "failed to unmount '~a': ~a~%"
                                           mount-point (strerror errno))))))
                           (filter (negate known?) (mount-points)))
                 #f))))

    (cons* sink user-unmount
           (map file-system-shepherd-service file-systems))))

(define (file-system-fstab-entries file-systems)
  "Return the subset of @var{file-systems} that should have an entry in
@file{/etc/fstab}."
  ;; /etc/fstab is about telling fsck(8), mount(8), and umount(8) about
  ;; relevant file systems they'll have to deal with.  That excludes "pseudo"
  ;; file systems.
  ;;
  ;; In particular, things like GIO (part of GLib) use it to determine the set
  ;; of mounts, which is then used by graphical file managers and desktop
  ;; environments to display "volume" icons.  Thus, we really need to exclude
  ;; those pseudo file systems from the list.
  (remove (lambda (file-system)
            (or (member (file-system-type file-system)
                        %pseudo-file-system-types)
                (memq 'bind-mount (file-system-flags file-system))))
          file-systems))

(define (file-system-type->utilities type)
  "Return the package providing the utilities for file system TYPE, #f
otherwise."
  (assoc-ref
   `(("bcachefs" . ,bcachefs-tools)
     ("btrfs" . ,btrfs-progs)
     ("exfat" . ,exfat-utils)
     ("ext2" . ,e2fsprogs)
     ("ext3" . ,e2fsprogs)
     ("ext4" . ,e2fsprogs)
     ("fat" . ,dosfstools)
     ("f2fs" . ,f2fs-tools)
     ("jfs" . ,jfsutils)
     ("vfat" . ,dosfstools)
     ("xfs" . ,xfsprogs)
     ("zfs" . ,zfs))
   type))

(define (file-system-utilities file-systems)
  "Return a list of packages containing file system utilities for
FILE-SYSTEMS."
  (filter-map (lambda (file-system)
                (file-system-type->utilities (file-system-type file-system)))
              file-systems))

(define file-system-service-type
  (service-type (name 'file-systems)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          file-system-shepherd-services)
                       (service-extension fstab-service-type
                                          file-system-fstab-entries)
                       (service-extension profile-service-type
                                          file-system-utilities)

                       ;; Have 'user-processes' depend on 'file-systems'.
                       (service-extension user-processes-service-type
                                          (const '(file-systems)))))
                (compose concatenate)
                (extend append)
                (description
                 "Provide Shepherd services to mount and unmount the given
file systems, as well as corresponding @file{/etc/fstab} entries.")))



;;;
;;; Preserve entropy to seed /dev/urandom on boot.
;;;

(define %random-seed-file
  "/var/lib/random-seed")

(define (urandom-seed-shepherd-service _)
  "Return a shepherd service for the /dev/urandom seed."
  (list (shepherd-service
         (documentation "Preserve entropy across reboots for /dev/urandom.")
         (provision '(urandom-seed))

         ;; Depend on udev so that /dev/hwrng is available.
         (requirement '(file-systems udev))

         (start #~(lambda _
                    ;; On boot, write random seed into /dev/urandom.
                    (when (file-exists? #$%random-seed-file)
                      (call-with-input-file #$%random-seed-file
                        (lambda (seed)
                          (call-with-output-file "/dev/urandom"
                            (lambda (urandom)
                              (dump-port seed urandom)

                              ;; Writing SEED to URANDOM isn't enough: we must
                              ;; also tell the kernel to account for these
                              ;; extra bits of entropy.
                              (let ((bits (* 8 (stat:size (stat seed)))))
                                (add-to-entropy-count urandom bits)))))))

                    ;; Try writing from /dev/hwrng into /dev/urandom.
                    ;; It seems that the file /dev/hwrng always exists, even
                    ;; when there is no hardware random number generator
                    ;; available. So, we handle a failed read or any other error
                    ;; reported by the operating system.
                    (let ((buf (catch 'system-error
                                 (lambda ()
                                   (call-with-input-file "/dev/hwrng"
                                     (lambda (hwrng)
                                       (get-bytevector-n hwrng 512))))
                                 ;; Silence is golden...
                                 (const #f))))
                      (when buf
                        (call-with-output-file "/dev/urandom"
                          (lambda (urandom)
                            (put-bytevector urandom buf)
                            (let ((bits (* 8 (bytevector-length buf))))
                              (add-to-entropy-count urandom bits))))))

                    ;; Immediately refresh the seed in case the system doesn't
                    ;; shut down cleanly.
                    (call-with-input-file "/dev/urandom"
                      (lambda (urandom)
                        (let ((previous-umask (umask #o077))
                              (buf (make-bytevector 512)))
                          (mkdir-p (dirname #$%random-seed-file))
                          (get-bytevector-n! urandom buf 0 512)
                          (call-with-output-file #$%random-seed-file
                            (lambda (seed)
                              (put-bytevector seed buf)))
                          (umask previous-umask))))
                    #t))
         (stop #~(lambda _
                   ;; During shutdown, write from /dev/urandom into random seed.
                   (let ((buf (make-bytevector 512)))
                     (call-with-input-file "/dev/urandom"
                       (lambda (urandom)
                         (let ((previous-umask (umask #o077)))
                           (get-bytevector-n! urandom buf 0 512)
                           (mkdir-p (dirname #$%random-seed-file))
                           (call-with-output-file #$%random-seed-file
                             (lambda (seed)
                               (put-bytevector seed buf)))
                           (umask previous-umask))
                         )))
                   #f))
         (modules `((rnrs bytevectors)
                    (rnrs io ports)
                    ,@%default-modules)))))

(define urandom-seed-service-type
  (service-type (name 'urandom-seed)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          urandom-seed-shepherd-service)

                       ;; Have 'user-processes' depend on 'urandom-seed'.
                       ;; This ensures that user processes and daemons don't
                       ;; start until we have seeded the PRNG.
                       (service-extension user-processes-service-type
                                          (const '(urandom-seed)))))
                (default-value #f)
                (description
                 "Seed the @file{/dev/urandom} pseudo-random number
generator (RNG) with the value recorded when the system was last shut
down.")))


;;;
;;; Add hardware random number generator to entropy pool.
;;;

(define-record-type* <rngd-configuration>
  rngd-configuration make-rngd-configuration
  rngd-configuration?
  (rng-tools rngd-configuration-rng-tools         ;file-like
             (default rng-tools))
  (device    rngd-configuration-device            ;string
             (default "/dev/hwrng")))

(define rngd-service-type
  (shepherd-service-type
    'rngd
    (lambda (config)
      (define rng-tools (rngd-configuration-rng-tools config))
      (define device (rngd-configuration-device config))

      (define rngd-command
        (list (file-append rng-tools "/sbin/rngd")
              "-f" "-r" device))

      (shepherd-service
        (documentation "Add TRNG to entropy pool.")
        (requirement '(user-processes udev))
        (provision '(trng))
        (start #~(make-forkexec-constructor '#$rngd-command))
        (stop #~(make-kill-destructor))))
    (rngd-configuration)
    (description "Run the @command{rngd} random number generation daemon to
supply entropy to the kernel's pool.")))

(define-deprecated (rngd-service #:key (rng-tools rng-tools)
                                 (device "/dev/hwrng"))
  rngd-service-type
  "Return a service that runs the @command{rngd} program from @var{rng-tools}
to add @var{device} to the kernel's entropy pool.  The service will fail if
@var{device} does not exist."
  (service rngd-service-type
           (rngd-configuration
            (rng-tools rng-tools)
            (device device))))

;;;
;;; /etc/hosts
;;;

(eval-when (expand load eval)
  (define (valid-name? name)
    "Return true if @var{name} is likely to be a valid host name."
    (false-if-exception (not (string-any char-set:whitespace name)))))

(define-compile-time-procedure (assert-valid-name (name valid-name?))
  "Ensure @var{name} is likely to be a valid host name."
  ;; TODO: RFC compliant implementation.
  (unless (valid-name? name)
    (raise
     (make-compound-condition
      (formatted-message (G_ "host name '~a' contains invalid characters")
                         name)
      (condition (&error-location
                  (location
                   (source-properties->location procedure-call-location)))))))
  name)

(define-record-type* <host> %host
  ;; XXX: Using the record type constructor becomes tiresome when
  ;; there's multiple records to make.
  make-host host?
  (address        host-address)
  (canonical-name host-canonical-name
                  (sanitize assert-valid-name))
  (aliases        host-aliases
                  (default '())
                  (sanitize (cut map assert-valid-name <>))))

(define* (host address canonical-name #:optional (aliases '()))
  "Return a new record for the host at @var{address} with the given
@var{canonical-name} and possibly @var{aliases}.

@var{address} must be a string denoting a valid IPv4 or IPv6 address, and
@var{canonical-name} and the strings listed in @var{aliases} must be valid
host names."
  (%host
   (address address)
   (canonical-name canonical-name)
   (aliases aliases)))

(define hosts-service-type
  ;; Extend etc-service-type with a entry for @file{/etc/hosts}.
  (let* ((serialize-host-record
          (lambda (record)
            (match-record record <host> (address canonical-name aliases)
              (format #f "~a~/~a~{~^~/~a~}~%" address canonical-name aliases))))
         (host-etc-service
          (lambda (lst)
            `(("hosts" ,(plain-file "hosts"
                                    (format #f "~{~a~}"
                                            (map serialize-host-record
                                                 lst))))))))
    (service-type
     (name 'etc-hosts)
     (extensions
      (list
       (service-extension etc-service-type
                          host-etc-service)))
     (compose concatenate)
     (extend append)
     (description "Populate the @file{/etc/hosts} file."))))


;;;
;;; Console & co.
;;;

(define host-name-service-type
  (shepherd-service-type
   'host-name
   (lambda (name)
     (shepherd-service
      (documentation "Initialize the machine's host name.")
      (provision '(host-name))
      (start #~(lambda _
                 (sethostname #$name)
                 #$name))
      (one-shot? #t)))
   (description "Initialize the machine's host name.")))

(define-deprecated (host-name-service name)
  host-name-service-type
  "Return a service that sets the host name to @var{name}."
  (service host-name-service-type name))

(define virtual-terminal-service-type
  ;; Ensure that virtual terminals run in UTF-8 mode.  This is the case by
  ;; default with recent Linux kernels, but this service allows us to ensure
  ;; this.  This service must start before any 'term-' service so that newly
  ;; created terminals inherit this property.  See
  ;; <https://bugs.gnu.org/30505> for a discussion.
  (shepherd-service-type
   'virtual-terminal
   (lambda (utf8?)
     (let ((knob "/sys/module/vt/parameters/default_utf8"))
       (shepherd-service
        (documentation "Set virtual terminals in UTF-8 module.")
        (provision '(virtual-terminal))
        (requirement '(root-file-system))
        (start #~(lambda _
                   ;; In containers /sys is read-only so don't insist on
                   ;; writing to this file.
                   (unless (= 1 (call-with-input-file #$knob read))
                     (call-with-output-file #$knob
                       (lambda (port)
                         (display 1 port))))
                   #t))
        (stop #~(const #f)))))
   #t                                             ;default to UTF-8
   (description "Ensure the Linux virtual terminals run in UTF-8 mode.")))

(define %default-console-font
  ;; Note: the 'font-gnu-unifont' package cannot be cross-compiled (yet), but
  ;; its "psf" output is the same whether it's built natively or not, hence
  ;; 'ungexp-native'.
  #~(string-append #+font-gnu-unifont:psf
                   "/share/consolefonts/Unifont-APL8x16.psf.gz"))

(define (console-font-shepherd-services tty+font)
  "Return a list of Shepherd services for each pair in TTY+FONT."
  (map (match-lambda
         ((tty . font)
          (let ((device (string-append "/dev/" tty)))
            (shepherd-service
             (documentation "Load a Unicode console font.")
             (provision (list (symbol-append 'console-font-
                                             (string->symbol tty))))

             ;; Start after mingetty has been started on TTY, otherwise the settings
             ;; are ignored.
             (requirement (list (symbol-append 'term-
                                               (string->symbol tty))))

             (start #~(lambda _
                        ;; It could be that mingetty is not fully ready yet,
                        ;; which we check by calling 'ttyname'.
                        (let loop ((i 10))
                          (unless (or (zero? i)
                                      (call-with-input-file #$device
                                        (lambda (port)
                                          (false-if-exception (ttyname port)))))
                            (usleep 500)
                            (loop (- i 1))))

                        ;; Assume the VT is already in UTF-8 mode, thanks to
                        ;; the 'virtual-terminal' service.
                        ;;
                        ;; 'setfont' returns EX_OSERR (71) when an
                        ;; KDFONTOP ioctl fails, for example.  Like
                        ;; systemd's vconsole support, let's not treat
                        ;; this as an error.
                        (case (status:exit-val
                               (system* #$(file-append kbd "/bin/setfont")
                                        "-C" #$device #$font))
                          ((0 71) #t)
                          (else #f))))
             (stop #~(const #f))
             (respawn? #f)))))
       tty+font))

(define console-font-service-type
  (service-type (name 'console-fonts)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          console-font-shepherd-services)))
                (compose concatenate)
                (extend append)
                (description
                 "Install the given fonts on the specified ttys (fonts are per
virtual console on GNU/Linux).  The value of this service is a list of
tty/font pairs.  The font can be the name of a font provided by the @code{kbd}
package or any valid argument to @command{setfont}, as in this example:

@example
`((\"tty1\" . \"LatGrkCyr-8x16\")
  (\"tty2\" . ,(file-append
                 font-tamzen
                 \"/share/kbd/consolefonts/TamzenForPowerline10x20.psf\"))
  (\"tty3\" . ,(file-append
                 font-terminus
                 \"/share/consolefonts/ter-132n\"))) ; for HDPI
@end example\n")))

(define %default-motd
  (plain-file "motd" "This is the GNU operating system, welcome!\n\n"))

(define-record-type* <login-configuration>
  login-configuration make-login-configuration
  login-configuration?
  (motd                   login-configuration-motd     ;file-like
                          (default %default-motd))
  ;; Allow empty passwords by default so that first-time users can log in when
  ;; the 'root' account has just been created.
  (allow-empty-passwords? login-configuration-allow-empty-passwords?
                          (default #t)))               ;Boolean

(define (login-pam-service config)
  "Return the list of PAM service needed for CONF."
  ;; Let 'login' be known to PAM.
  (list (unix-pam-service "login"
                          #:login-uid? #t
                          #:allow-empty-passwords?
                          (login-configuration-allow-empty-passwords? config)
                          #:motd
                          (login-configuration-motd config))))

(define login-service-type
  (service-type (name 'login)
                (extensions (list (service-extension pam-root-service-type
                                                     login-pam-service)))
                (default-value (login-configuration))
                (description
                 "Provide a console log-in service as specified by its
configuration value, a @code{login-configuration} object.")))

(define-deprecated (login-service #:optional (config (login-configuration)))
  login-service-type
  "Return a service configure login according to @var{config}, which specifies
the message of the day, among other things."
  (service login-service-type config))

(define-record-type* <agetty-configuration>
  agetty-configuration make-agetty-configuration
  agetty-configuration?
  (agetty           agetty-configuration-agetty   ;file-like
                    (default util-linux))
  (tty              agetty-configuration-tty)     ;string | #f
  (term             agetty-term                   ;string | #f
                    (default #f))
  (baud-rate        agetty-baud-rate              ;string | #f
                    (default #f))
  (auto-login       agetty-auto-login             ;list of strings | #f
                    (default #f))
  (login-program    agetty-login-program          ;gexp
                    (default (file-append shadow "/bin/login")))
  (login-pause?     agetty-login-pause?           ;Boolean
                    (default #f))
  (eight-bits?      agetty-eight-bits?            ;Boolean
                    (default #f))
  (no-reset?        agetty-no-reset?              ;Boolean
                    (default #f))
  (remote?          agetty-remote?                ;Boolean
                    (default #f))
  (flow-control?    agetty-flow-control?          ;Boolean
                    (default #f))
  (host             agetty-host                   ;string | #f
                    (default #f))
  (no-issue?        agetty-no-issue?              ;Boolean
                    (default #f))
  (init-string      agetty-init-string            ;string | #f
                    (default #f))
  (no-clear?        agetty-no-clear?              ;Boolean
                    (default #f))
  (local-line       agetty-local-line             ;always | never | auto
                    (default #f))
  (extract-baud?    agetty-extract-baud?          ;Boolean
                    (default #f))
  (skip-login?      agetty-skip-login?            ;Boolean
                    (default #f))
  (no-newline?      agetty-no-newline?            ;Boolean
                    (default #f))
  (login-options    agetty-login-options          ;string | #f
                    (default #f))
  (chroot           agetty-chroot                 ;string | #f
                    (default #f))
  (hangup?          agetty-hangup?                ;Boolean
                    (default #f))
  (keep-baud?       agetty-keep-baud?             ;Boolean
                    (default #f))
  (timeout          agetty-timeout                ;integer | #f
                    (default #f))
  (detect-case?     agetty-detect-case?           ;Boolean
                    (default #f))
  (wait-cr?         agetty-wait-cr?               ;Boolean
                    (default #f))
  (no-hints?        agetty-no-hints?              ;Boolean
                    (default #f))
  (no-hostname?     agetty-no-hostname?           ;Boolean
                    (default #f))
  (long-hostname?   agetty-long-hostname?         ;Boolean
                    (default #f))
  (erase-characters agetty-erase-characters       ;string | #f
                    (default #f))
  (kill-characters  agetty-kill-characters        ;string | #f
                    (default #f))
  (chdir            agetty-chdir                  ;string | #f
                    (default #f))
  (delay            agetty-delay                  ;integer | #f
                    (default #f))
  (nice             agetty-nice                   ;integer | #f
                    (default #f))
  ;; "Escape hatch" for passing arbitrary command-line arguments.
  (extra-options    agetty-extra-options          ;list of strings
                    (default '()))
  (shepherd-requirement agetty-shepherd-requirement  ;list of SHEPHERD requirements
                    (default '()))
;;; XXX Unimplemented for now!
;;; (issue-file     agetty-issue-file             ;file-like
;;;                 (default #f))
  )

(define (default-serial-port)
  "Return a gexp that determines a reasonable default serial port
to use as the tty.  This is primarily useful for headless systems."
  (with-imported-modules (source-module-closure
                          '((gnu build linux-boot))) ;for 'find-long-options'
    #~(begin
        ;; console=device,options
        ;; device: can be tty0, ttyS0, lp0, ttyUSB0 (serial).
        ;; options: BBBBPNF. P n|o|e, N number of bits,
        ;; F flow control (r RTS)
        (let* ((not-comma (char-set-complement (char-set #\,)))
               (command (linux-command-line))
               (agetty-specs (find-long-options "agetty.tty" command))
               (console-specs (filter (lambda (spec)
                                        (and (string-prefix? "tty" spec)
                                             (not (or
                                                   (string-prefix? "tty0" spec)
                                                   (string-prefix? "tty1" spec)
                                                   (string-prefix? "tty2" spec)
                                                   (string-prefix? "tty3" spec)
                                                   (string-prefix? "tty4" spec)
                                                   (string-prefix? "tty5" spec)
                                                   (string-prefix? "tty6" spec)
                                                   (string-prefix? "tty7" spec)
                                                   (string-prefix? "tty8" spec)
                                                   (string-prefix? "tty9" spec)))))
                                      (find-long-options "console" command)))
               (specs (append agetty-specs console-specs)))
          (match specs
            (() #f)
            ((spec _ ...)
             ;; Extract device name from first spec.
             (match (string-tokenize spec not-comma)
               ((device-name _ ...)
                device-name))))))))

(define (agetty-shepherd-service config)
  (match-record config <agetty-configuration>
    (agetty tty term baud-rate auto-login
            login-program login-pause? eight-bits? no-reset? remote? flow-control?
            host no-issue? init-string no-clear? local-line extract-baud?
            skip-login? no-newline? login-options chroot hangup? keep-baud? timeout
            detect-case? wait-cr? no-hints? no-hostname? long-hostname?
            erase-characters kill-characters chdir delay nice extra-options
            shepherd-requirement)
    (list
     (shepherd-service
      (documentation "Run agetty on a tty.")
      (provision (list (symbol-append 'term- (string->symbol (or tty "console")))))

      ;; Since the login prompt shows the host name, wait for the 'host-name'
      ;; service to be done.  Also wait for udev essentially so that the tty
      ;; text is not lost in the middle of kernel messages (see also
      ;; mingetty-shepherd-service).
      (requirement (cons* 'user-processes 'host-name 'udev
                          shepherd-requirement))

      (modules '((ice-9 match) (gnu build linux-boot)))
      (start
       (with-imported-modules  (source-module-closure
                                '((gnu build linux-boot)))
         #~(lambda args
             (let ((defaulted-tty #$(or tty (default-serial-port))))
               (apply
                (if defaulted-tty
                    (make-forkexec-constructor
                     (list #$(file-append util-linux "/sbin/agetty")
                           #$@extra-options
                           #$@(if eight-bits?
                                  #~("--8bits")
                                  #~())
                           #$@(if no-reset?
                                  #~("--noreset")
                                  #~())
                           #$@(if remote?
                                  #~("--remote")
                                  #~())
                           #$@(if flow-control?
                                  #~("--flow-control")
                                  #~())
                           #$@(if host
                                  #~("--host" #$host)
                                  #~())
                           #$@(if no-issue?
                                  #~("--noissue")
                                  #~())
                           #$@(if init-string
                                  #~("--init-string" #$init-string)
                                  #~())
                           #$@(if no-clear?
                                  #~("--noclear")
                                  #~())
;;; FIXME This doesn't work as expected. According to agetty(8), if this option
;;; is not passed, then the default is 'auto'. However, in my tests, when that
;;; option is selected, agetty never presents the login prompt, and the
;;; term-ttyS0 service respawns every few seconds.
                           #$@(if local-line
                                  #~(#$(match local-line
                                         ('auto "--local-line=auto")
                                         ('always "--local-line=always")
                                         ('never "-local-line=never")))
                                  #~())
                           #$@(if tty
                                  #~()
                                  #~("--keep-baud"))
                           #$@(if extract-baud?
                                  #~("--extract-baud")
                                  #~())
                           #$@(if skip-login?
                                  #~("--skip-login")
                                  #~())
                           #$@(if no-newline?
                                  #~("--nonewline")
                                  #~())
                           #$@(if login-options
                                  #~("--login-options" #$login-options)
                                  #~())
                           #$@(if chroot
                                  #~("--chroot" #$chroot)
                                  #~())
                           #$@(if hangup?
                                  #~("--hangup")
                                  #~())
                           #$@(if keep-baud?
                                  #~("--keep-baud")
                                  #~())
                           #$@(if timeout
                                  #~("--timeout" #$(number->string timeout))
                                  #~())
                           #$@(if detect-case?
                                  #~("--detect-case")
                                  #~())
                           #$@(if wait-cr?
                                  #~("--wait-cr")
                                  #~())
                           #$@(if no-hints?
                                  #~("--nohints?")
                                  #~())
                           #$@(if no-hostname?
                                  #~("--nohostname")
                                  #~())
                           #$@(if long-hostname?
                                  #~("--long-hostname")
                                  #~())
                           #$@(if erase-characters
                                  #~("--erase-chars" #$erase-characters)
                                  #~())
                           #$@(if kill-characters
                                  #~("--kill-chars" #$kill-characters)
                                  #~())
                           #$@(if chdir
                                  #~("--chdir" #$chdir)
                                  #~())
                           #$@(if delay
                                  #~("--delay" #$(number->string delay))
                                  #~())
                           #$@(if nice
                                  #~("--nice" #$(number->string nice))
                                  #~())
                           #$@(if auto-login
                                  (list "--autologin" auto-login)
                                  '())
                           #$@(if login-program
                                  #~("--login-program" #$login-program)
                                  #~())
                           #$@(if login-pause?
                                  #~("--login-pause")
                                  #~())
                           defaulted-tty
                           #$@(if baud-rate
                                  #~(#$baud-rate)
                                  #~())
                           #$@(if term
                                  #~(#$term)
                                  #~())))
                    #$(if tty
                          #~(const #f)         ;always fail to start
                          #~(lambda _          ;succeed, but don't do anything
                              (format #t "~a: \
no serial port console requested; doing nothing~%"
                                      '#$(car provision))
                              'idle)))
                args)))))
      (stop #~(let ((stop (make-kill-destructor)))
                (lambda (running)
                  (if (eq? 'idle running)
                      #f
                      (stop running)))))))))

(define agetty-service-type
  (service-type (name 'agetty)
                (extensions (list (service-extension shepherd-root-service-type
                                                     agetty-shepherd-service)))
                (description
                 "Provide console login using the @command{agetty}
program.")))

(define-deprecated (agetty-service config)
  agetty-service-type
  "Return a service to run agetty according to @var{config}, which specifies
the tty to run, among other things."
  (service agetty-service-type config))

(define-record-type* <mingetty-configuration>
  mingetty-configuration make-mingetty-configuration
  mingetty-configuration?
  (mingetty             mingetty-configuration-mingetty ;file-like
                        (default mingetty))
  (tty                  mingetty-configuration-tty)       ;string
  (auto-login           mingetty-configuration-auto-login ;string | #f
                        (default #f))
  (login-program        mingetty-configuration-login-program ;gexp
                        (default #f))
  (login-pause?         mingetty-configuration-login-pause? ;boolean
                        (default #f))
  (clear-on-logout?     mingetty-configuration-clear-on-logout? ;boolean
                        (default #t))
  (delay                mingetty-configuration-delay ;integer | #f
                        (default #f))
  (print-issue          mingetty-configuration-print-issue ;boolean | Symbol
                        (default #t))
  (print-hostname       mingetty-configuration-print-hostname ;boolean | Symbol
                        (default #t))
  (nice                 mingetty-configuration-nice ;integer | #f
                        (default #f))
  (working-directory    mingetty-configuration-working-directory ;string | #f
                        (default #f))
  (root-directory       mingetty-configuration-root-directory ;string | #f
                        (default #f))
  (shepherd-requirement mingetty-configuration-shepherd-requirement
                        ;; Since the login prompt shows the host name, wait
                        ;; for the 'host-name' service to be done.  Also wait
                        ;; for udev essentially so that the tty text is not
                        ;; lost in the middle of kernel messages (XXX).
                        (default '(user-processes host-name udev
                                                  virtual-terminal))))

(define (mingetty-shepherd-service config)
  (match-record config <mingetty-configuration>
                ( mingetty tty auto-login login-program
                  login-pause? clear-on-logout? delay
                  print-issue print-hostname nice
                  working-directory root-directory shepherd-requirement)
    (list
     (shepherd-service
      (documentation "Run mingetty on an tty.")
      (provision (list (symbol-append 'term- (string->symbol tty))))

      (requirement shepherd-requirement)

      (start  #~(make-forkexec-constructor
                 (list #$(file-append mingetty "/sbin/mingetty")

                       ;; Avoiding 'vhangup' allows us to avoid 'setfont'
                       ;; errors down the path where various ioctls get
                       ;; EIO--see 'hung_up_tty_ioctl' in driver/tty/tty_io.c
                       ;; in Linux.
                       "--nohangup" #$tty

                       #$@(if clear-on-logout?
                              #~()
                              #~("--noclear"))
                       #$@(if auto-login
                              #~("--autologin" #$auto-login)
                              #~())
                       #$@(if login-program
                              #~("--loginprog" #$login-program)
                              #~())
                       #$@(if login-pause?
                              #~("--loginpause")
                              #~())
                       #$@(if delay
                              #~("--delay" #$(number->string delay))
                              #~())
                       #$@(match print-issue
                            (#t
                             #~())
                            ('no-nl
                             #~("--nonewline"))
                            (#f
                             #~("--noissue")))
                       #$@(match print-hostname
                            (#t
                             #~())
                            ('long
                             #~("--long-hostname"))
                            (#f
                             #~("--nohostname")))
                       #$@(if nice
                              #~("--nice" #$(number->string nice))
                              #~())
                       #$@(if working-directory
                              #~("--chdir" #$working-directory)
                              #~())
                       #$@(if root-directory
                              #~("--chroot" #$root-directory)
                              #~()))))
      (stop   #~(make-kill-destructor))))))

(define mingetty-service-type
  (service-type (name 'mingetty)
                (extensions (list (service-extension shepherd-root-service-type
                                                     mingetty-shepherd-service)))
                (description
                 "Provide console login using the @command{mingetty}
program.")))

(define-deprecated (mingetty-service config)
  mingetty-service-type
  "Return a service to run mingetty according to @var{config}, which specifies
the tty to run, among other things."
  (service mingetty-service-type config))

(define-record-type* <nscd-configuration> nscd-configuration
  make-nscd-configuration
  nscd-configuration?
  (log-file    nscd-configuration-log-file        ;string
               (default #f))
  (debug-level nscd-debug-level                   ;integer
               (default 0))
  ;; TODO: See nscd.conf in glibc for other options to add.
  (caches     nscd-configuration-caches           ;list of <nscd-cache>
              (default %nscd-default-caches))
  (name-services nscd-configuration-name-services ;list of file-like
                 (default '()))
  (glibc      nscd-configuration-glibc            ;file-like
              (default (let-system (system target)
                         ;; Unless we're cross-compiling, arrange to use nscd
                         ;; from 'glibc-final' instead of pulling in a second
                         ;; glibc copy.
                         (if target
                             (cross-libc target)
                             (canonical-package glibc))))))

(define-record-type* <nscd-cache> nscd-cache make-nscd-cache
  nscd-cache?
  (database              nscd-cache-database)              ;symbol
  (positive-time-to-live nscd-cache-positive-time-to-live) ;integer
  (negative-time-to-live nscd-cache-negative-time-to-live
                         (default 20))             ;integer
  (suggested-size        nscd-cache-suggested-size ;integer ("default module
                                                   ;of hash table")
                         (default 211))
  (check-files?          nscd-cache-check-files?  ;Boolean
                         (default #t))
  (persistent?           nscd-cache-persistent?   ;Boolean
                         (default #t))
  (shared?               nscd-cache-shared?       ;Boolean
                         (default #t))
  (max-database-size     nscd-cache-max-database-size ;integer
                         (default (* 32 (expt 2 20))))
  (auto-propagate?       nscd-cache-auto-propagate? ;Boolean
                         (default #t)))

(define %nscd-default-caches
  ;; Caches that we want to enable by default.  Note that when providing an
  ;; empty nscd.conf, all caches are disabled.
  (list (nscd-cache (database 'hosts)

                    ;; Aggressively cache the host name cache to improve
                    ;; privacy and resilience.
                    (positive-time-to-live (* 3600 12))
                    (negative-time-to-live 20)
                    (persistent? #t))

        (nscd-cache (database 'services)

                    ;; Services are unlikely to change, so we can be even more
                    ;; aggressive.
                    (positive-time-to-live (* 3600 24))
                    (negative-time-to-live 3600)
                    (check-files? #t)             ;check /etc/services changes
                    (persistent? #t))

        ;; Enable minimal caching of the user databases, not so much for
        ;; caching but rather to allow that uses of NSS plugins like LDAP
        ;; don't lead user processes to dlopen them (which is likely to fail
        ;; due to them not being found in $LD_LIBRARY_PATH).
        (nscd-cache (database 'passwd)
                    (positive-time-to-live 600)
                    (negative-time-to-live 20)
                    (check-files? #t)             ;check /etc/passwd changes
                    (persistent? #f))
        (nscd-cache (database 'group)
                    (positive-time-to-live 600)
                    (negative-time-to-live 20)
                    (check-files? #t)             ;check /etc/group changes
                    (persistent? #f))))

(define-deprecated %nscd-default-configuration
  #f
  ;; Default nscd configuration.
  (nscd-configuration))

(define (nscd.conf-file config)
  "Return the @file{nscd.conf} configuration file for @var{config}, an
@code{<nscd-configuration>} object."
  (define (cache->config cache)
    (match-record cache <nscd-cache>
      (database positive-time-to-live negative-time-to-live
                suggested-size check-files?
                persistent? shared? max-database-size auto-propagate?)
      (let ((database (symbol->string database)))
        (string-append "\nenable-cache\t" database "\tyes\n"

                       "positive-time-to-live\t" database "\t"
                       (number->string positive-time-to-live) "\n"
                       "negative-time-to-live\t" database "\t"
                       (number->string negative-time-to-live) "\n"
                       "suggested-size\t" database "\t"
                       (number->string suggested-size) "\n"
                       "check-files\t" database "\t"
                       (if check-files? "yes\n" "no\n")
                       "persistent\t" database "\t"
                       (if persistent? "yes\n" "no\n")
                       "shared\t" database "\t"
                       (if shared? "yes\n" "no\n")
                       "max-db-size\t" database "\t"
                       (number->string max-database-size) "\n"
                       "auto-propagate\t" database "\t"
                       (if auto-propagate? "yes\n" "no\n")))))

  (match-record config <nscd-configuration>
    (log-file debug-level caches)
    (plain-file "nscd.conf"
                (string-append "\
# Configuration of libc's name service cache daemon (nscd).\n\n"
                               (if log-file
                                   (string-append "logfile\t" log-file)
                                   "")
                               "\n"
                               (if debug-level
                                   (string-append "debug-level\t"
                                                  (number->string debug-level))
                                   "")
                               "\n"
                               (string-concatenate
                                (map cache->config caches))))))

(define (nscd-action-procedure nscd config option)
  ;; XXX: This is duplicated from mcron; factorize.
  #~(lambda (_ . args)
      ;; Run 'nscd' in a pipe so we can explicitly redirect its output to
      ;; 'current-output-port', which at this stage is bound to the client
      ;; connection.
      (let ((pipe (apply open-pipe* OPEN_READ #$nscd
                         "-f" #$config #$option args)))
        (let loop ()
          (match (read-line pipe 'concat)
            ((? eof-object?)
             (catch 'system-error
               (lambda ()
                 (zero? (close-pipe pipe)))
               (lambda args
                 ;; There's a race with the SIGCHLD handler, which could
                 ;; call 'waitpid' before 'close-pipe' above does.  If we
                 ;; get ECHILD, that means we lost the race; in that case, we
                 ;; cannot tell what the exit code was (FIXME).
                 (or (= ECHILD (system-error-errno args))
                     (apply throw args)))))
            (line
             (display line)
             (loop)))))))

(define (nscd-actions nscd config)
  "Return Shepherd actions for NSCD using CONFIG its config file."
  ;; Make this functionality available as actions because that's a simple way
  ;; to run the right 'nscd' binary with the right config file.
  (list (shepherd-configuration-action config)
        (shepherd-action
         (name 'statistics)
         (documentation "Display statistics about nscd usage.")
         (procedure (nscd-action-procedure nscd config "--statistics")))
        (shepherd-action
         (name 'invalidate)
         (documentation
          "Invalidate the given cache--e.g., 'hosts' for host name lookups.")
         (procedure (nscd-action-procedure nscd config "--invalidate")))))

(define (nscd-shepherd-service config)
  "Return a shepherd service for CONFIG, an <nscd-configuration> object."
  (let ((nscd          (file-append (nscd-configuration-glibc config)
                                    "/sbin/nscd"))
        (nscd.conf     (nscd.conf-file config))
        (name-services (nscd-configuration-name-services config)))
    (list (shepherd-service
           (documentation "Run libc's name service cache daemon (nscd).")
           (provision '(nscd))

           ;; Logs are written with syslog(3), which writes to /dev/console
           ;; when nobody's listening--ugly.  Thus, wait for syslogd.
           (requirement '(user-processes syslogd))

           (start #~(make-forkexec-constructor
                     (list #$nscd "-f" #$nscd.conf "--foreground")

                     ;; Wait for the PID file.  However, the PID file is
                     ;; written before nscd is actually listening on its
                     ;; socket (XXX).
                     #:pid-file "/var/run/nscd/nscd.pid"

                     #:environment-variables
                     (list (string-append "LD_LIBRARY_PATH="
                                          (string-join
                                           (map (lambda (dir)
                                                  (string-append dir "/lib"))
                                                (list #$@name-services))
                                           ":")))))
           (stop #~(make-kill-destructor))
           (modules `((ice-9 popen)               ;for the actions
                      (ice-9 rdelim)
                      (ice-9 match)
                      ,@%default-modules))
           (actions (nscd-actions nscd nscd.conf))))))

(define nscd-activation
  ;; Actions to take before starting nscd.
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/run/nscd")
      (mkdir-p "/var/db/nscd")                    ;for the persistent cache

      ;; In libc 2.25 nscd uses inotify to watch /etc/resolv.conf, but only if
      ;; that file exists when it is started.  Thus create it here.  Note: on
      ;; some systems, such as when NetworkManager is used, /etc/resolv.conf
      ;; is a symlink, hence 'lstat'.
      (unless (false-if-exception (lstat "/etc/resolv.conf"))
        (call-with-output-file "/etc/resolv.conf"
          (lambda (port)
            (display "# This is a placeholder.\n" port))))))

(define nscd-service-type
  (service-type (name 'nscd)
                (extensions
                 (list (service-extension activation-service-type
                                          (const nscd-activation))
                       (service-extension shepherd-root-service-type
                                          nscd-shepherd-service)))

                ;; This can be extended by providing additional name services
                ;; such as nss-mdns.
                (compose concatenate)
                (extend (lambda (config name-services)
                          (nscd-configuration
                           (inherit config)
                           (name-services (append
                                           (nscd-configuration-name-services config)
                                           name-services)))))
                (default-value (nscd-configuration))
                (description
                 "Runs libc's @dfn{name service cache daemon} (nscd) with the
given configuration---an @code{<nscd-configuration>} object.  @xref{Name
Service Switch}, for an example.")))

(define-deprecated (nscd-service #:optional (config (nscd-configuration)))
  nscd-service-type
  "Return a service that runs libc's name service cache daemon (nscd) with the
given @var{config}---an @code{<nscd-configuration>} object.  @xref{Name
Service Switch}, for an example."
  (service nscd-service-type config))

;;; Snippet adapted from the GNU inetutils manual.
(define %default-syslog.conf
  (plain-file "syslog.conf" "\
# See info '(inetutils) syslogd invocation' for the documentation
# of the syslogd configuration syntax.

# Log all error messages, authentication messages of
# level notice or higher and anything of level err or
# higher to the console.
# Don't log private authentication messages!
*.alert;auth.notice;authpriv.none      -/dev/console

# Log anything (except mail) of level info or higher.
# Don't log private authentication messages!
*.info;mail.none;authpriv.none         -/var/log/messages

# Log \"debug\"-level entries and nothing else.
*.=debug                               -/var/log/debug

# Same, in a different place.
*.info;mail.none;authpriv.none         -/dev/tty12

# The authpriv file has restricted access.
# 'fsync' the file after each line (hence the lack of a leading dash).
# Also include unprivileged auth logs of info or higher level
# to conveniently gather the authentication data at the same place.
authpriv.*;auth.info                    /var/log/secure

# Log all the mail messages in one place.
mail.*                                 -/var/log/maillog
"))

(define-record-type* <syslog-configuration>
  syslog-configuration  make-syslog-configuration
  syslog-configuration?
  (syslogd              syslog-configuration-syslogd
                        (default (file-append inetutils "/libexec/syslogd")))
  (config-file          syslog-configuration-config-file
                        (default %default-syslog.conf))
  (extra-options        syslog-configuration-extra-options
                        (default '())))

;;; Note: a static file name is used for syslog.conf so that the reload action
;;; work as intended.
(define syslog.conf "/etc/syslog.conf")

(define (syslog-etc configuration)
  (match-record configuration <syslog-configuration>
    (config-file)
    (list `(,(basename syslog.conf) ,config-file))))

(define (syslog-shepherd-service config)
  (define config-file
    (syslog-configuration-config-file config))

  (shepherd-service
   (documentation "Run the syslog daemon (syslogd).")
   (provision '(syslogd))
   (requirement '(user-processes))
   (actions
    (list (shepherd-configuration-action syslog.conf)
          (shepherd-action
           (name 'reload)
           (documentation "Reload the configuration file from disk.")
           (procedure
            #~(lambda (pid)
                (if pid
                    (begin
                      (kill pid SIGHUP)
                      (display #$(G_ "Service syslog has been asked to \
reload its settings file.")))
                    (display #$(G_ "Service syslog is not running."))))))))
   ;; Note: a static file name is used for syslog.conf so that the reload
   ;; action work as intended.
   (start #~(make-forkexec-constructor
             (list #$(syslog-configuration-syslogd config)
                   ;; the -f option here is compatible with rsyslog
                   "-f" #$syslog.conf
                   #$@(syslog-configuration-extra-options config))
             #:file-creation-mask #o137
             #:pid-file "/var/run/syslog.pid"))
   (stop #~(make-kill-destructor))))

(define %default-syslog-files
  ;; List of files usually produced by syslogd that should be rotated.
  '("/var/log/messages" "/var/log/secure" "/var/log/debug"
    "/var/log/maillog"))

(define syslog-service-type
  (service-type
   (name 'syslog)
   (default-value (syslog-configuration))
   (extensions (list (service-extension shepherd-root-service-type
                                        (compose list
                                                 syslog-shepherd-service))
                     (service-extension log-rotation-service-type
                                        (const %default-syslog-files))
                     (service-extension etc-service-type syslog-etc)))
   (description "Run the syslog daemon, @command{syslogd}, which is
responsible for logging system messages.")))

(define-deprecated (syslog-service #:optional (config (syslog-configuration)))
  syslog-service-type
  "Return a service that runs @command{syslogd} and takes
@var{<syslog-configuration>} as a parameter.

@xref{syslogd invocation,,, inetutils, GNU Inetutils}, for more
information on the configuration file syntax."
  (service syslog-service-type config))


(define pam-limits-service-type
  (let ((pam-extension
         (lambda (limits-file)
           (pam-extension
            (transformer
             (lambda (pam)
               (let ((pam-limits (pam-entry
                                  (control "required")
                                  (module "pam_limits.so")
                                  (arguments
                                   (list #~(string-append "conf=" #$limits-file))))))
                 (if (member (pam-service-name pam)
                             '("login" "greetd" "su" "slim" "gdm-password"
                               "sddm" "lightdm" "sudo" "sshd"))
                     (pam-service
                      (inherit pam)
                      (session (cons pam-limits
                                     (pam-service-session pam))))
                     pam)))))))
        (make-limits-file
         (match-lambda
           ;; XXX: Using file-like objects is deprecated, use lists instead.
           ;;      This is to be reduced into the list? case when the deprecated
           ;;      code gets removed.
           ((? file-like? obj)
            (warning (G_ "Using file-like value for \
'pam-limits-service-type' is deprecated~%"))
            obj)
           ((? list? lst)
            (plain-file "limits.conf"
                        (string-join (map pam-limits-entry->string lst)
                                     "\n" 'suffix)))
           (_ (raise
               (formatted-message
                (G_ "invalid input for 'pam-limits-service-type'~%")))))))

    (service-type
     (name 'limits)
     (compose concatenate)
     (extend append)
     (extensions
      (list (service-extension pam-root-service-type
                               (lambda (config)
                                 (list (pam-extension (make-limits-file config)))))))
     (description
      "Use the @code{pam_limits} authentication module to set the specified
resource usage limits.")
     (default-value '()))))

(define-deprecated (pam-limits-service #:optional (limits '()))
  pam-limits-service-type
  "Return a service that makes selected programs respect the list of
pam-limits-entry specified in LIMITS via pam_limits.so."
  (service pam-limits-service-type
           (plain-file "limits.conf"
                       (string-join (map pam-limits-entry->string limits)
                                    "\n"))))


;;;
;;; Guix services.
;;;

(define* (guix-build-accounts count #:key
                              (group "guixbuild")
                              (shadow shadow))
  "Return a list of COUNT user accounts for Guix build users with the given
GID."
  (unfold (cut > <> count)
          (lambda (n)
            (user-account
             (name (format #f "guixbuilder~2,'0d" n))
             (system? #t)
             (group group)

             ;; guix-daemon expects GROUP to be listed as a
             ;; supplementary group too:
             ;; <http://lists.gnu.org/archive/html/bug-guix/2013-01/msg00239.html>.
             (supplementary-groups (list group "kvm"))

             (comment (format #f "Guix Build User ~2d" n))
             (home-directory "/var/empty")
             (shell (file-append shadow "/sbin/nologin"))))
          1+
          1))

(define not-config?
  ;; Select (guix …) and (gnu …) modules, except (guix config).
  (match-lambda
    (('guix 'config) #f)
    (('guix rest ...) #t)
    (('gnu rest ...) #t)
    (rest #f)))

(define (substitute-key-authorization keys guix)
  "Return a gexp with code to register KEYS, a list of files containing 'guix
archive' public keys, with GUIX."
  (define default-acl
    (with-extensions (list guile-gcrypt)
      (with-imported-modules `(((guix config) => ,(make-config.scm))
                               ,@(source-module-closure '((guix pki))
                                                        #:select? not-config?))
        (computed-file "acl"
                       #~(begin
                           (use-modules (guix pki)
                                        (gcrypt pk-crypto)
                                        (ice-9 rdelim))

                           (define keys
                             (map (lambda (file)
                                    (call-with-input-file file
                                      (compose string->canonical-sexp
                                               read-string)))
                                  '(#$@keys)))

                           (call-with-output-file #$output
                             (lambda (port)
                               (write-acl (public-keys->acl keys)
                                          port))))))))

  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))
        (define acl-file #$%acl-file)
        ;; If the ACL already exists, move it out of the way.  Create a backup
        ;; if it's a regular file: it's likely that the user manually updated
        ;; it with 'guix archive --authorize'.
        (if (file-exists? acl-file)
            (if (and (symbolic-link? acl-file)
                     (store-file-name? (readlink acl-file)))
                (delete-file acl-file)
                (rename-file acl-file (string-append acl-file ".bak")))
            (mkdir-p (dirname acl-file)))

        ;; Installed the declared ACL.
        (symlink #+default-acl acl-file))))

(define (install-channels-file channels)
  "Return a gexp with code to install CHANNELS, a list of channels, in
/etc/guix/channels.scm."
  (define channels-file
    (scheme-file "channels.scm"
                 `(list ,@(map channel->code channels))))

  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        ;; If channels.scm already exists, move it out of the way. Create a
        ;; backup if it's a regular file: it's likely that the user
        ;; manually defined it.
        (if (file-exists? "/etc/guix/channels.scm")
            (if (and (symbolic-link? "/etc/guix/channels.scm")
                     (store-file-name? (readlink "/etc/guix/channels.scm")))
                (delete-file "/etc/guix/channels.scm")
                (rename-file "/etc/guix/channels.scm"
                             "/etc/guix/channels.scm.bak"))
            (mkdir-p "/etc/guix"))

        ;; Installed the declared channels.
        (symlink #+channels-file "/etc/guix/channels.scm"))))

(define %default-authorized-guix-keys
  ;; List of authorized substitute keys.
  (list (file-append guix "/share/guix/berlin.guix.gnu.org.pub")
        (file-append guix "/share/guix/bordeaux.guix.gnu.org.pub")))

(define (guix-machines-files-installation machines)
  "Return a gexp to install MACHINES, a list of gexps, as
/etc/guix/machines.scm, which is used for offloading."
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (define machines-file
          "/etc/guix/machines.scm")

        ;; If MACHINES-FILE already exists, move it out of the way.
        ;; Create a backup if it's a regular file: it's likely that the
        ;; user manually updated it.
        (let ((stat (false-if-exception (lstat machines-file))))
          (if stat
              (if (and (eq? 'symlink (stat:type stat))
                       (store-file-name? (readlink machines-file)))
                  (delete-file machines-file)
                  (rename-file machines-file
                               (string-append machines-file ".bak")))
              (mkdir-p (dirname machines-file))))

        ;; Installed the declared machines file.
        (symlink #+(scheme-file "machines.scm"
                                #~((@ (srfi srfi-1) append-map)
                                   (lambda (entry)
                                     (if (build-machine? entry)
                                         (list entry)
                                         entry))
                                   #$machines))
                 machines-file))))

(define (run-with-writable-store)
  "Return a wrapper that runs the given command under the specified UID and
GID in a context where the store is writable, even if it was bind-mounted
read-only via %IMMUTABLE-STORE (this wrapper must run as root)."
  (program-file "run-with-writable-store"
                (with-imported-modules (source-module-closure
                                        '((guix build syscalls)))
                  #~(begin
                      (use-modules (guix build syscalls)
                                   (ice-9 match))

                      (define (ensure-writable-store store)
                        ;; Create a new mount namespace and remount STORE with
                        ;; write permissions if it's read-only.
                        (unshare CLONE_NEWNS)
                        (let ((fs (statfs store)))
                          (unless (zero? (logand (file-system-mount-flags fs)
                                                 ST_RDONLY))
                            (mount store store "none"
                                   (logior MS_BIND MS_REMOUNT)))))

                      (match (command-line)
                        ((_ user group command args ...)
                         (ensure-writable-store #$(%store-prefix))
                         (let ((uid (or (string->number user)
                                        (passwd:uid (getpwnam user))))
                               (gid (or (string->number group)
                                        (group:gid (getgrnam group)))))
                           (setgroups #())
                           (setgid gid)
                           (setuid uid)
                           (apply execl command command args))))))))

(define (guix-ownership-change-program)
  "Return a program that changes ownership of the store and other data files
of Guix to the given UID and GID."
  (program-file
   "validate-guix-ownership"
   (with-imported-modules (source-module-closure
                           '((guix build utils)))
     #~(begin
         (use-modules (guix build utils)
                      (ice-9 ftw)
                      (ice-9 match))

         (define (lchown file uid gid)
           (let ((parent (open (dirname file) O_DIRECTORY)))
             (chown-at parent (basename file) uid gid
                       AT_SYMLINK_NOFOLLOW)
             (close-port parent)))

         (define (change-ownership directory uid gid)
           ;; chown -R UID:GID DIRECTORY
           (file-system-fold (const #t)                              ;enter?
                             (lambda (file stat result)              ;leaf
                               (if (eq? 'symlink (stat:type stat))
                                   (lchown file uid gid)
                                   (chown file uid gid)))
                             (const #t)           ;down
                             (lambda (directory stat result) ;up
                               (chown directory uid gid))
                             (const #t)           ;skip
                             (lambda (file stat errno result)
                               (format (current-error-port)
                                       "i/o error: ~a: ~a~%"
                                       file (strerror errno))
                               #f)
                             #t                   ;seed
                             directory
                             lstat))

         (define (claim-data-ownership uid gid)
           (format #t "Changing file ownership for /gnu/store \
and data directories to ~a:~a...~%"
                   uid gid)
           (change-ownership #$(%store-prefix) uid gid)
           (let ((excluded '("." ".." "profiles" "userpool")))
             (for-each (lambda (directory)
                         (change-ownership (in-vicinity "/var/guix" directory)
                                           uid gid))
                       (scandir "/var/guix"
                                (lambda (file)
                                  (not (member file
                                               excluded))))))
           (chown "/var/guix" uid gid)
           (change-ownership "/etc/guix" uid gid)
           (mkdir-p "/var/log/guix")
           (change-ownership "/var/log/guix" uid gid))

         (match (command-line)
           ((_ (= string->number (? integer? uid))
               (= string->number (? integer? gid)))
            (setlocale LC_ALL "C.UTF-8")          ;for file name decoding
            (setvbuf (current-output-port) 'line)
            (claim-data-ownership uid gid)))))))

(define-record-type* <guix-configuration>
  guix-configuration make-guix-configuration
  guix-configuration?
  (guix             guix-configuration-guix       ;file-like
                    (default guix))
  (build-group      guix-configuration-build-group ;string
                    (default "guixbuild"))
  (build-accounts   guix-configuration-build-accounts ;integer
                    (default 10))
  (chroot?          guix-configuration-chroot? ;Boolean | 'default
                    (default 'default))
  (authorize-key?   guix-configuration-authorize-key? ;Boolean
                    (default #t))
  (authorized-keys  guix-configuration-authorized-keys ;list of gexps
                    (default %default-authorized-guix-keys))
  (use-substitutes? guix-configuration-use-substitutes? ;Boolean
                    (default #t))
  (substitute-urls  guix-configuration-substitute-urls ;list of strings
                    (default %default-substitute-urls))
  (generate-substitute-key? guix-configuration-generate-substitute-key?
                            (default #t))         ;Boolean
  (channels         guix-configuration-channels ;file-like
                    (default #f))
  (chroot-directories guix-configuration-chroot-directories ;list of file-like/strings
                      (default '()))
  (max-silent-time  guix-configuration-max-silent-time ;integer
                    (default 3600))
  (timeout          guix-configuration-timeout    ;integer
                    (default (* 3600 24)))
  (log-compression  guix-configuration-log-compression
                    (default 'gzip))
  (discover?        guix-configuration-discover?
                    (default #f))
  (extra-options    guix-configuration-extra-options ;list of strings
                    (default '()))
  (log-file         guix-configuration-log-file   ;string
                    (default "/var/log/guix-daemon.log"))
  (http-proxy       guix-http-proxy               ;string | #f
                    (default #f))
  (tmpdir           guix-tmpdir                   ;string | #f
                    (default #f))
  (privileged?      guix-configuration-privileged?
                    (default #t))
  (build-machines   guix-configuration-build-machines ;list of gexps | '()
                    (default '()))
  (environment      guix-configuration-environment  ;list of strings
                    (default '()))
  (socket-directory-permissions
   guix-configuration-socket-directory-permissions
   (default #o755))
  (socket-directory-group guix-configuration-socket-directory-group
                          (default #f))
  (socket-directory-user guix-configuration-socket-directory-user
                         (default #f)))

(define %default-guix-configuration
  (guix-configuration))

(define shepherd-set-http-proxy-action
  ;; Shepherd action to change the HTTP(S) proxy.
  (shepherd-action
   (name 'set-http-proxy)
   (documentation
    "Change the HTTP(S) proxy used by 'guix-daemon' and restart it.")
   (procedure #~(lambda* (_ #:optional proxy)
                  (let ((environment (environ)))
                    ;; A bit of a hack: communicate PROXY to the 'start'
                    ;; method via environment variables.
                    (if proxy
                        (begin
                          (format #t "changing HTTP/HTTPS \
proxy of 'guix-daemon' to ~s...~%"
                                  proxy)
                          (setenv "http_proxy" proxy))
                        (begin
                          (format #t "clearing HTTP/HTTPS \
proxy of 'guix-daemon'...~%")
                          (unsetenv "http_proxy")))
                    (perform-service-action (lookup-service 'guix-daemon)
                                            'restart)
                    (environ environment)
                    #t)))))

(define shepherd-discover-action
  ;; Shepherd action to enable or disable substitute servers discovery.
  (shepherd-action
   (name 'discover)
   (documentation
    "Enable or disable substitute servers discovery and restart the
'guix-daemon'.")
   (procedure #~(lambda* (_ status)
                  (let ((environment (environ)))
                    (if (and status
                             (string=? status "on"))
                        (begin
                          (format #t "enable substitute servers discovery~%")
                          (setenv "discover" "on"))
                        (begin
                          (format #t "disable substitute servers discovery~%")
                          (unsetenv "discover")))
                    (perform-service-action (lookup-service 'guix-daemon)
                                            'restart)
                    (environ environment)
                    #t)))))

(define (guix-shepherd-services config)
  "Return a <shepherd-service> for the Guix daemon service with CONFIG."
  (define locales
    (let-system (system target)
      (if (target-hurd? (or target system))
          (make-glibc-utf8-locales glibc/hurd)
          glibc-utf8-locales)))

  (match-record config <guix-configuration>
    (guix privileged?
          build-group build-accounts chroot? authorize-key? authorized-keys
          use-substitutes? substitute-urls max-silent-time timeout
          log-compression discover? extra-options log-file
          http-proxy tmpdir chroot-directories environment
          socket-directory-permissions socket-directory-group
          socket-directory-user)
    (list (shepherd-service
           (provision '(guix-ownership))
           (requirement '(user-processes user-homes))
           (one-shot? #t)
           (start #~(lambda ()
                      (let* ((store #$(%store-prefix))
                             (stat (lstat store))
                             (privileged? #$(guix-configuration-privileged?
                                             config))
                             (change-ownership #$(guix-ownership-change-program))
                             (with-writable-store #$(run-with-writable-store)))
                        ;; Check whether we're switching from privileged to
                        ;; unprivileged guix-daemon, or vice versa, and adjust
                        ;; file ownership accordingly.  Spawn a child process
                        ;; if and only if something needs to be changed.
                        ;;
                        ;; Note: This service remains in 'starting' state for
                        ;; as long as CHANGE-OWNERSHIP is running.  That way,
                        ;; 'guix-daemon' starts only once we're done.
                        (cond ((and (not privileged?)
                                    (or (zero? (stat:uid stat))
                                        (zero? (stat:gid stat))))
                               (let ((user (getpwnam "guix-daemon")))
                                 (format #t "Changing to unprivileged guix-daemon.~%")
                                 (zero?
                                  (system* with-writable-store "0" "0"
                                           change-ownership
                                           (number->string (passwd:uid user))
                                           (number->string (passwd:gid user))))))
                              ((and privileged?
                                    (and (not (zero? (stat:uid stat)))
                                         (not (zero? (stat:gid stat)))))
                               (format #t "Changing to privileged guix-daemon.~%")
                               (zero? (system* with-writable-store "0" "0"
                                               change-ownership "0" "0")))
                              (else #t)))))
           (documentation "Ensure that the store and other data files used by
guix-daemon have the right ownership."))

          (shepherd-service
           (documentation "Run the Guix daemon.")
           (provision '(guix-daemon))
           (requirement `(user-processes
                          guix-ownership
                          ,@(if discover? '(avahi-daemon) '())))
           (actions (list shepherd-set-http-proxy-action
                          shepherd-discover-action))
           (modules '((srfi srfi-1)
                      (ice-9 match)
                      (gnu build shepherd)
                      (guix build utils)))
           (start
            (with-imported-modules `(((guix config) => ,(make-config.scm))
                                     ,@(source-module-closure
                                        '((gnu build shepherd)
                                          (guix build utils))
                                        #:select? not-config?))
              #~(lambda args
                  (define proxy
                    ;; HTTP/HTTPS proxy.  The 'http_proxy' variable is set by
                    ;; the 'set-http-proxy' action.
                    (or (getenv "http_proxy") #$http-proxy))

                  (define discover?
                    (or (getenv "discover") #$discover?))

                  (define daemon-command
                    (cons* #$@(if privileged?
                                  #~()
                                  #~(#$(run-with-writable-store)
                                     "guix-daemon" "guix-daemon"))

                           #$(file-append guix "/bin/guix-daemon")
                           #$@(if privileged?
                                  #~("--build-users-group" #$build-group)
                                  #~())
                           "--max-silent-time"
                           #$(number->string max-silent-time)
                           "--timeout" #$(number->string timeout)
                           "--log-compression"
                           #$(symbol->string log-compression)
                           #$@(if use-substitutes?
                                  '()
                                  '("--no-substitutes"))
                           (string-append "--discover="
                                          (if discover? "yes" "no"))
                           "--substitute-urls" #$(string-join substitute-urls)
                           #$@extra-options

                           #$@(if chroot?
                                  '()
                                  '("--disable-chroot"))
                           ;; Add CHROOT-DIRECTORIES and all their dependencies
                           ;; (if these are store items) to the chroot.
                           (append-map
                            (lambda (file)
                              (append-map (lambda (directory)
                                            (list "--chroot-directory"
                                                  directory))
                                          (call-with-input-file file
                                            read)))
                            '#$(map references-file
                                    chroot-directories))))

                  (define environment-variables
                    (append (list #$@(if tmpdir
                                         (list (string-append "TMPDIR=" tmpdir))
                                         '())

                                  ;; Make sure we run in a UTF-8 locale so that
                                  ;; 'guix offload' correctly restores nars
                                  ;; that contain UTF-8 file names such as
                                  ;; 'nss-certs'.  See
                                  ;; <https://bugs.gnu.org/32942>.
                                  (string-append "GUIX_LOCPATH="
                                                 #$locales "/lib/locale")
                                  "LC_ALL=en_US.utf8"
                                  ;; Make 'tar' and 'gzip' available so
                                  ;; that 'guix perform-download' can use
                                  ;; them when downloading from Software
                                  ;; Heritage via '(guix swh)'.
                                  (string-append "PATH="
                                                 #$(file-append tar "/bin") ":"
                                                 #$(file-append gzip "/bin")))
                            (if proxy
                                (list (string-append "http_proxy=" proxy)
                                      (string-append "https_proxy=" proxy))
                                '())
                            '#$environment))

                  ;; Ensure that a fresh directory is used, in case the old
                  ;; one was more permissive and processes have a file
                  ;; descriptor referencing it hanging around, ready to use
                  ;; with openat.
                  (false-if-exception
                   (delete-file-recursively "/var/guix/daemon-socket"))

                  (match args
                    (((= string->number (? integer? pid)))
                     ;; Start the guix-daemon in the same mnt namespace as
                     ;; PID.  This is necessary when running the installer.
                     ;; Assume /var/guix/daemon-socket was created by a
                     ;; previous 'start' call without arguments.
                     (fork+exec-command/container
                      daemon-command
                      #:pid pid
                      #:environment-variables environment-variables
                      #:log-file #$log-file))
                    (()
                     ;; Default to socket activation.
                     (let ((socket (endpoint
                                    (make-socket-address
                                     AF_UNIX
                                     "/var/guix/daemon-socket/socket")
                                    #:name "socket"
                                    #:socket-owner
                                    (or #$socket-directory-user
                                        #$(if privileged? 0 "guix-daemon"))
                                    #:socket-group
                                    (or #$socket-directory-group
                                        #$(if privileged? 0 "guix-daemon"))
                                    #:socket-directory-permissions
                                    #$socket-directory-permissions)))
                       ((make-systemd-constructor daemon-command
                                                  (list socket)
                                                  #:environment-variables
                                                  environment-variables
                                                  #:log-file #$log-file))))))))
           (stop #~(lambda (value)
                     (if (or (process? value) (integer? value))
                         ((make-kill-destructor) value)
                         ((make-systemd-destructor) value))))))))

(define (guix-accounts config)
  "Return the user accounts and user groups for CONFIG."
  `(,@(if (guix-configuration-privileged? config)
          '()
          (list (user-group (name "guix-daemon") (system? #t))
                (user-account
                 (name "guix-daemon")
                 (group "guix-daemon")
                 (system? #t)
                 (supplementary-groups '("kvm"))
                 (comment "Guix Daemon User")
                 (home-directory "/var/empty")
                 (shell (file-append shadow "/sbin/nologin")))))

    ;; When reconfiguring from privileged to unprivileged, the running daemon
    ;; (privileged) relies on the availability of the build accounts and build
    ;; group until 'guix system reconfigure' has completed.  The simplest way
    ;; to meet this requirement is to create these accounts unconditionally so
    ;; they are not removed in the middle of the 'reconfigure' process.
    ,(user-group
      (name (guix-configuration-build-group config))
      (system? #t)

      ;; Use a fixed GID so that we can create the store with the right owner.
      (id 30000))
    ,@(guix-build-accounts (guix-configuration-build-accounts config)
                           #:group (guix-configuration-build-group config))))

(define (guix-activation config)
  "Return the activation gexp for CONFIG."
  (match-record config <guix-configuration>
    (guix generate-substitute-key? authorize-key? authorized-keys channels)
    #~(begin
        ;; Assume that the store has BUILD-GROUP as its group.  We could
        ;; otherwise call 'chown' here, but the problem is that on a COW overlayfs,
        ;; chown leads to an entire copy of the tree, which is a bad idea.

        ;; Generate a key pair and optionally authorize substitute server keys.
        (unless (or #$(not generate-substitute-key?)
                    (file-exists? "/etc/guix/signing-key.pub"))
          (system* #$(file-append guix "/bin/guix") "archive"
                   "--generate-key"))

        ;; Optionally install /etc/guix/acl...
        #$(if authorize-key?
              (substitute-key-authorization authorized-keys guix)
              #~#f)

        ;; ... and /etc/guix/channels.scm...
        #$(and channels (install-channels-file channels))

        ;; ... and /etc/guix/machines.scm.
        #$(if (null? (guix-configuration-build-machines config))
              #~#f
              (guix-machines-files-installation
               #~(list #$@(guix-configuration-build-machines config)))))))

(define-record-type* <guix-extension>
  guix-extension make-guix-extension
  guix-extension?
  (authorized-keys guix-extension-authorized-keys ;list of file-like
                    (default '()))
  (substitute-urls guix-extension-substitute-urls ;list of strings
                    (default '()))
  (build-machines  guix-extension-build-machines  ;list of gexps
                   (default '()))
  (chroot-directories guix-extension-chroot-directories ;list of file-like/strings
                      (default '())))

(define (guix-extension-merge a b)
  (guix-extension
   (authorized-keys (append (guix-extension-authorized-keys a)
                            (guix-extension-authorized-keys b)))
   (substitute-urls (append (guix-extension-substitute-urls a)
                            (guix-extension-substitute-urls b)))
   (build-machines (append (guix-extension-build-machines a)
                           (guix-extension-build-machines b)))
   (chroot-directories (append (guix-extension-chroot-directories a)
                               (guix-extension-chroot-directories b)))))

(define guix-service-type
  (service-type
   (name 'guix)
   (extensions
    (list (service-extension shepherd-root-service-type guix-shepherd-services)
          (service-extension account-service-type guix-accounts)
          (service-extension activation-service-type guix-activation)
          (service-extension profile-service-type
                             (compose list guix-configuration-guix))))

   ;; Extensions can specify extra directories to add to the build chroot,
   ;; extra substitute urls and extra authorized keys
   (compose (lambda (args) (fold guix-extension-merge (guix-extension) args)))
   (extend (lambda (config extension)
             (guix-configuration
              (inherit config)
              (authorized-keys (append (guix-extension-authorized-keys extension)
                                       (guix-configuration-authorized-keys config)))
              (substitute-urls (append (guix-extension-substitute-urls extension)
                                       (guix-configuration-substitute-urls config)))
              (build-machines
               (and (or (guix-configuration-build-machines config)
                        (pair? (guix-extension-build-machines extension)))
                    (append (guix-configuration-build-machines config)
                            (guix-extension-build-machines extension))))
              (chroot-directories
               (append (guix-extension-chroot-directories extension)
                       (guix-configuration-chroot-directories config))))))

   (default-value (guix-configuration))
   (description
    "Run the build daemon of GNU@tie{}Guix, aka. @command{guix-daemon}.")))


(define-record-type* <guix-publish-configuration>
  guix-publish-configuration make-guix-publish-configuration
  guix-publish-configuration?
  (guix    guix-publish-configuration-guix        ;file-like
           (default guix))
  (port    guix-publish-configuration-port        ;number
           (default 80))
  (host    guix-publish-configuration-host        ;string
           (default "localhost"))
  (advertise? guix-publish-advertise?             ;boolean
              (default #f))
  (compression       guix-publish-configuration-compression
                     (thunked)
                     (default (default-compression this-record)))
  (nar-path    guix-publish-configuration-nar-path ;string
               (default "nar"))
  (cache       guix-publish-configuration-cache   ;#f | string
               (default #f))
  (cache-bypass-threshold guix-publish-configuration-cache-bypass-threshold
                          (default (* 10 (expt 2 20)))) ;integer
  (workers     guix-publish-configuration-workers ;#f | integer
               (default #f))
  (ttl         guix-publish-configuration-ttl     ;#f | integer
               (default #f))
  (negative-ttl guix-publish-configuration-negative-ttl ;#f | integer
                (default #f)))

(define (default-compression config)
  "Return the default 'guix publish' compression according to CONFIG, and
raise a deprecation warning if the 'compression-level' field was used."
  ;; Default to low compression levels when there's no cache so that users
  ;; get good bandwidth by default.
  (if (guix-publish-configuration-cache config)
      '(("gzip" 5) ("zstd" 19))
      '(("gzip" 3) ("zstd" 3))))               ;zstd compresses faster

(define (guix-publish-shepherd-service config)
  (define (config->compression-options config)
    (match (guix-publish-configuration-compression config)
      (()                                   ;empty list means "no compression"
       '("-C0"))
      (lst
       (append-map (match-lambda
                     ((type level)
                      `("-C" ,(string-append type ":"
                                             (number->string level)))))
                   lst))))

  (match-record config <guix-publish-configuration>
    (guix port host nar-path cache workers ttl negative-ttl
          cache-bypass-threshold advertise?)
    (let ((command #~(list #$(file-append guix "/bin/guix")
                           "publish" "-u" "guix-publish"
                           "-p" #$(number->string port)
                           #$@(config->compression-options config)
                           (string-append "--nar-path=" #$nar-path)
                           (string-append "--listen=" #$host)
                           #$@(if advertise?
                                  #~("--advertise")
                                  #~())
                           #$@(if workers
                                  #~((string-append "--workers="
                                                    #$(number->string
                                                       workers)))
                                  #~())
                           #$@(if ttl
                                  #~((string-append "--ttl="
                                                    #$(number->string ttl)
                                                    "s"))
                                  #~())
                           #$@(if negative-ttl
                                  #~((string-append "--negative-ttl="
                                                    #$(number->string negative-ttl)
                                                    "s"))
                                  #~())
                           #$@(if cache
                                  #~((string-append "--cache=" #$cache)
                                     #$(string-append
                                        "--cache-bypass-threshold="
                                        (number->string
                                         cache-bypass-threshold)))
                                  #~())))
          (options #~(#:environment-variables
                      ;; Make sure we run in a UTF-8 locale so we can produce
                      ;; nars for packages that contain UTF-8 file names such
                      ;; as 'nss-certs'.  See <https://bugs.gnu.org/26948>.
                      (list (string-append "GUIX_LOCPATH="
                                           #$(libc-utf8-locales-for-target)
                                           "/lib/locale")
                            "LC_ALL=en_US.utf8")
                      #:log-file "/var/log/guix-publish.log"))
          (endpoints #~(let ((ai (false-if-exception
                                  (getaddrinfo #$host
                                               #$(number->string port)
                                               AI_NUMERICSERV))))
                         (if (pair? ai)
                             (list (endpoint (addrinfo:addr (car ai))))
                             '()))))
      (list (shepherd-service
             (provision '(guix-publish))
             (requirement `(user-processes
                            guix-daemon
                            ,@(if advertise? '(avahi-daemon) '())))

             ;; Use lazy socket activation unless ADVERTISE? is true: in that
             ;; case the process should start right away to advertise itself.
             (start #~(make-systemd-constructor
                       #$command #$endpoints #$@options
                       #:lazy-start? #$(not advertise?)))
             (stop #~(make-systemd-destructor)))))))

(define %guix-publish-accounts
  (list (user-group (name "guix-publish") (system? #t))
        (user-account
         (name "guix-publish")
         (group "guix-publish")
         (system? #t)
         (comment "guix publish user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define (guix-publish-activation config)
  (let ((cache (guix-publish-configuration-cache config)))
    (if cache
        (with-imported-modules '((guix build utils))
          #~(begin
              (use-modules (guix build utils))

              (mkdir-p #$cache)
              (let* ((pw  (getpw "guix-publish"))
                     (uid (passwd:uid pw))
                     (gid (passwd:gid pw)))
                (chown #$cache uid gid))))
        #t)))

(define guix-publish-service-type
  (service-type (name 'guix-publish)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          guix-publish-shepherd-service)
                       (service-extension account-service-type
                                          (const %guix-publish-accounts))
                       (service-extension activation-service-type
                                          guix-publish-activation)))
                (default-value (guix-publish-configuration))
                (description
                 "Add a Shepherd service running @command{guix publish}, a
command that allows you to share pre-built binaries with others over HTTP.")))


;;;
;;; Udev.
;;;

(define-record-type* <udev-configuration>
  udev-configuration make-udev-configuration
  udev-configuration?
  (udev   udev-configuration-udev                 ;file-like
          (default eudev))
  (debug? udev-configuration-debug?               ;boolean
          (default #f))
  (rules  udev-configuration-rules                ;list of file-like
          (default '()))
  (hardware  udev-configuration-hardware          ;list of file-like
             (default '())))

(define (udev-configurations-union subdirectory packages)
  "Return the union of the lib/udev/SUBDIRECTORY directories found in each
item of PACKAGES."
  (define build
    (with-imported-modules '((guix build union)
                             (guix build utils))
      #~(begin
          (use-modules (guix build union)
                       (guix build utils)
                       (srfi srfi-1)
                       (srfi srfi-26))

          (define %standard-locations
            '(#$(string-append "/lib/udev/" subdirectory)
                #$(string-append "/libexec/udev/" subdirectory)))

          (define (configuration-sub-directory directory)
            ;; Return the sub-directory of DIRECTORY containing udev
            ;; configurations, or #f if none was found.
            (find directory-exists?
                  (map (cut string-append directory <>) %standard-locations)))

          (union-build #$output
                       (filter-map configuration-sub-directory '#$packages)))))

  (computed-file (string-append "udev-" subdirectory) build))

(define (udev-rules-union packages)
  "Return the union of the lib/udev/rules.d directories found in each
item of PACKAGES."
  (udev-configurations-union "rules.d" packages))

(define (udev-configuration-file subdirectory file-name contents)
  "Return a directory with a udev configuration file FILE-NAME containing CONTENTS."
  (file->udev-configuration-file subdirectory file-name (plain-file file-name contents)))

(define (udev-rule file-name contents)
  "Return a directory with a udev rule file FILE-NAME containing CONTENTS."
  (udev-configuration-file "rules.d" file-name contents))

(define (udev-hardware file-name contents)
  "Return a directory with a udev hardware file FILE-NAME containing CONTENTS."
  (udev-configuration-file "hwdb.d" file-name contents))

(define (file->udev-configuration-file subdirectory file-name file)
  "Return a directory with a udev configuration file FILE-NAME which is a copy
 of FILE."
  (computed-file file-name
                 (with-imported-modules '((guix build utils))
                   #~(begin
                       (use-modules (guix build utils))

                       (define configuration-directory
                         (string-append #$output
                                        "/lib/udev/"
                                        #$subdirectory))

                       (define file-copy-dest
                         (string-append configuration-directory "/" #$file-name))

                       (mkdir-p configuration-directory)
                       (copy-file #$file file-copy-dest)))))

(define (file->udev-rule file-name file)
  "Return a directory with a udev rule file FILE-NAME which is a copy of FILE."
  (file->udev-configuration-file "rules.d" file-name file))

(define (file->udev-hardware file-name file)
  "Return a directory with a udev hardware file FILE-NAME which is a copy of FILE."
  (file->udev-configuration-file "hwdb.d" file-name file))

(define kvm-udev-rule
  ;; Return a directory with a udev rule that changes the group of /dev/kvm to
  ;; "kvm" and makes it #o660.  Apparently QEMU-KVM used to ship this rule,
  ;; but now we have to add it by ourselves.

  ;; Build users are part of the "kvm" group, so we can fearlessly make
  ;; /dev/kvm 660 (see <http://bugs.gnu.org/18994>, for background.)
  (udev-rule "90-kvm.rules"
             "KERNEL==\"kvm\", GROUP=\"kvm\", MODE=\"0660\"\n"))

(define (udev-shepherd-service config)
  ;; Return a <shepherd-service> for UDEV with RULES.
  (let ((udev (udev-configuration-udev config)))
    (list
     (shepherd-service
      (provision '(udev))

      ;; Udev needs /dev to be a 'devtmpfs' mount so that new device nodes can
      ;; be added: see
      ;; <http://www.linuxfromscratch.org/lfs/view/development/chapter07/udev.html>.
      (requirement '(root-file-system))

      (documentation "Populate the /dev directory, dynamically.")
      (start
       (with-imported-modules (source-module-closure
                               '((gnu build linux-boot)))
         #~(lambda ()
             (define udevd
               ;; 'udevd' from eudev.
               #$(file-append udev "/sbin/udevd"))

             (define (wait-for-udevd)
               ;; Wait until someone's listening on udevd's control
               ;; socket.
               (let ((sock (socket AF_UNIX SOCK_SEQPACKET 0)))
                 (let try ()
                   (catch 'system-error
                     (lambda ()
                       (connect sock PF_UNIX "/run/udev/control")
                       (close-port sock))
                     (lambda args
                       (format #t "waiting for udevd...~%")
                       (usleep 500000)
                       (try))))))

             ;; Allow udev to find the modules.
             (setenv "LINUX_MODULE_DIRECTORY"
                     "/run/booted-system/kernel/lib/modules")

             (let* ((kernel-release
                     (utsname:release (uname)))
                    (linux-module-directory
                     (getenv "LINUX_MODULE_DIRECTORY"))
                    (directory
                     (string-append linux-module-directory "/"
                                    kernel-release))
                    (old-umask (umask #o022)))
               ;; If we're in a container, DIRECTORY might not exist,
               ;; for instance because the host runs a different
               ;; kernel.  In that case, skip it; we'll just miss a few
               ;; nodes like /dev/fuse.
               (when (file-exists? directory)
                 (make-static-device-nodes directory))
               (umask old-umask))

             (let ((pid (fork+exec-command
                         (list udevd
                               #$@(if (udev-configuration-debug? config)
                                      '("--debug")
                                      '()))
                         #:environment-variables
                         (cons*
                          (string-append "LINUX_MODULE_DIRECTORY="
                                         (getenv "LINUX_MODULE_DIRECTORY"))
                          (default-environment-variables)))))
               ;; Wait until udevd is up and running.  This appears to
               ;; be needed so that the events triggered below are
               ;; actually handled.
               (wait-for-udevd)

               ;; Trigger device node creation.
               (system* #$(file-append udev "/bin/udevadm")
                        "trigger" "--action=add")

               ;; Wait for things to settle down.
               (system* #$(file-append udev "/bin/udevadm")
                        "settle")
               pid))))
      (stop #~(make-kill-destructor))

      ;; When halting the system, 'udev' is actually killed by
      ;; 'user-processes', i.e., before its own 'stop' method was called.
      ;; Thus, make sure it is not respawned.
      (respawn? #f)
      ;; We need additional modules.
      (modules `((gnu build linux-boot)           ;'make-static-device-nodes'
                 ,@%default-modules))))))

(define udev.conf
  (computed-file "udev.conf"
                 #~(call-with-output-file #$output
                     (lambda (port)
                       (format port "udev_rules=\"/etc/udev/rules.d\"~%")))))

(define (udev-etc config)
  (match-record config <udev-configuration>
    (udev rules hardware)
    (let* ((hardware
            (udev-configurations-union "hwdb.d" (cons* udev hardware)))
           (hwdb.bin
            (computed-file
             "hwdb.bin"
             (with-imported-modules '((guix build utils))
               #~(begin
                   (use-modules (guix build utils))
                   (setenv "UDEV_HWDB_PATH" #$hardware)
                   (invoke #+(file-append udev "/bin/udevadm")
                           "hwdb"
                           "--update"
                           "-o" #$output))))))
    `(("udev"
       ,(file-union "udev"
                    `(("udev.conf" ,udev.conf)
                      ("rules.d"
                       ,(udev-rules-union (cons* udev kvm-udev-rule
                                                 rules)))
                      ("hwdb.bin" ,hwdb.bin))))))))

(define udev-service-type
  (service-type (name 'udev)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          udev-shepherd-service)
                       (service-extension etc-service-type udev-etc)))
                (compose concatenate)           ;concatenate the list of rules
                (extend (lambda (config extensions)
                          (let ((initial-rules
                                 (udev-configuration-rules config))
                                (initial-hardware
                                 (udev-configuration-hardware config)))
                            (udev-configuration
                             (inherit config)
                             (rules (append initial-rules extensions))
                             (hardware (append initial-hardware extensions))))))
                (default-value (udev-configuration))
                (description
                 "Run @command{udev}, which populates the @file{/dev}
directory dynamically.  Get extra rules from the packages listed in the
@code{rules} field of its value, @code{udev-configuration} object.")))

(define-deprecated (udev-service #:key (udev eudev) (rules '()))
  udev-service-type
  "Run @var{udev}, which populates the @file{/dev} directory dynamically.  Get
extra rules from the packages listed in @var{rules}."
  (service udev-service-type
           (udev-configuration (udev udev) (rules rules))))

(define* (udev-rules-service name rules #:key (groups '()))
  "Return a service that extends udev-service-type with RULES and
account-service-type with GROUPS as system groups.  This works by creating a
singleton service type NAME-udev-rules, of which the returned service is an
instance."
  (let* ((name (symbol-append name '-udev-rules))
         (account-extension
          (const (map (lambda (group)
                        (user-group (name group) (system? #t)))
                      groups)))
         (udev-extension (const (list rules)))
         (type (service-type
                (name name)
                (extensions (list
                             (service-extension
                              account-service-type account-extension)
                             (service-extension
                              udev-service-type udev-extension)))
                (description "This service adds udev rules."))))
    (service type #f)))

(define (udev-hardware-service name hardware-files)
  "Return a service that extends udev-service-type with HARDWARE-FILES, named
NAME-udev-hardware."
  (let* ((name (symbol-append name '-udev-hardware))
         (udev-extension (const (list hardware-files)))
         (type (service-type
                (name name)
                (extensions (list
                             (service-extension
                              udev-service-type udev-extension)))
                (description "This service adds udev hardware files."))))
    (service type #f)))

(define (swap-space->shepherd-service-name space)
  (let ((target (swap-space-target space)))
    (symbol-append 'swap-
                   (string->symbol
                    (cond ((uuid? target)
                           (uuid->string target))
                          ((file-system-label? target)
                           (file-system-label->string target))
                          (else
                           target))))))

; TODO Remove after deprecation
(define (swap-deprecated->shepherd-service-name sdep)
  (symbol-append 'swap-
                 (string->symbol
                  (cond ((uuid? sdep)
                         (string-take (uuid->string sdep) 6))
                        ((file-system-label? sdep)
                         (file-system-label->string sdep))
                        (else
                         sdep)))))

(define swap->shepherd-service-name
  (match-lambda ((? swap-space? space)
                 (swap-space->shepherd-service-name space))
                (sdep
                 (swap-deprecated->shepherd-service-name sdep))))

(define swap-service-type
  (shepherd-service-type
   'swap
   (lambda (swap)
     (define requirements
       (cond ((swap-space? swap)
              (map dependency->shepherd-service-name
                   (swap-space-dependencies swap)))
             ; TODO Remove after deprecation
             ((and (string? swap) (string-prefix? "/dev/mapper/" swap))
              (list (symbol-append 'device-mapping-
                                   (string->symbol (basename swap)))))
             (else
              '())))

     (define device-lookup
       ;; The generic 'find-partition' procedures could return a partition
       ;; that's not swap space, but that's unlikely.
       (cond ((swap-space? swap)
              (let ((target (swap-space-target swap)))
                (cond ((uuid? target)
                       #~(find-partition-by-uuid #$(uuid-bytevector target)))
                      ((file-system-label? target)
                       #~(find-partition-by-label
                          #$(file-system-label->string target)))
                      (else
                       target))))
             ; TODO Remove after deprecation
             ((uuid? swap)
              #~(find-partition-by-uuid #$(uuid-bytevector swap)))
             ((file-system-label? swap)
              #~(find-partition-by-label
                 #$(file-system-label->string swap)))
             (else
              swap)))

     (with-imported-modules (source-module-closure '((gnu build file-systems)))
       (shepherd-service
        (provision (list (swap->shepherd-service-name swap)))
        (requirement `(,@(if (target-hurd?) '() '(udev)) ,@requirements))
        (documentation "Enable the given swap space.")
        (modules `((gnu build file-systems)
                   ,@%default-modules))
        (start #~(lambda ()
                   (let ((device #$device-lookup))
                     (and device
                          (begin
                            #$(if (target-hurd?)
                                  #~(system* "swapon" device)
                                  #~(restart-on-EINTR
                                     (swapon device
                                             #$(if (swap-space? swap)
                                                   (swap-space->flags-bit-mask
                                                    swap)
                                                   0))))
                            #t)))))
        (stop #~(lambda _
                  (let ((device #$device-lookup))
                    (when device
                      #$(if (target-hurd?)
                            #~(system* "swapoff" device)
                            #~(restart-on-EINTR (swapoff device))))
                    #f)))
        (respawn? #f))))
   (description "Turn on the virtual memory swap area.")))

(define (swap-service swap)
  "Return a service that uses @var{swap} as a swap space."
  (service swap-service-type swap))

(define %default-gpm-options
  ;; Default options for GPM.
  '("-m" "/dev/input/mice" "-t" "ps2"))

(define-record-type* <gpm-configuration>
  gpm-configuration make-gpm-configuration gpm-configuration?
  (gpm      gpm-configuration-gpm                 ;file-like
            (default gpm))
  (options  gpm-configuration-options             ;list of strings
            (default %default-gpm-options)))

(define (gpm-shepherd-service config)
  (match-record config <gpm-configuration>
    (gpm options)
    (list (shepherd-service
           (requirement '(user-processes udev))
           (provision '(gpm))
           ;; 'gpm' runs in the background and sets a PID file.
           ;; Note that it requires running as "root".
           (start #~(make-forkexec-constructor
                     (list #$(file-append gpm "/sbin/gpm")
                           #$@options)
                     #:pid-file "/var/run/gpm.pid"
                     #:pid-file-timeout 3))
           (stop #~(lambda (_)
                     ;; Return #f if successfully stopped.
                     (not (zero? (system* #$(file-append gpm "/sbin/gpm")
                                          "-k")))))))))

(define gpm-service-type
  (service-type (name 'gpm)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          gpm-shepherd-service)))
                (default-value (gpm-configuration))
                (description
                 "Run GPM, the general-purpose mouse daemon, with the given
command-line options.  GPM allows users to use the mouse in the console,
notably to select, copy, and paste text.  The default options use the
@code{ps2} protocol, which works for both USB and PS/2 mice.")))


(define-record-type* <kmscon-configuration>
  kmscon-configuration     make-kmscon-configuration
  kmscon-configuration?
  (kmscon                  kmscon-configuration-kmscon
                           (default kmscon))
  (virtual-terminal        kmscon-configuration-virtual-terminal)
  (login-program           kmscon-configuration-login-program
                           (default (file-append shadow "/bin/login")))
  (login-arguments         kmscon-configuration-login-arguments
                           (default '("-p")))
  (auto-login              kmscon-configuration-auto-login
                           (default #f))
  (hardware-acceleration?  kmscon-configuration-hardware-acceleration?
                           (default #f))  ; #t causes failure
  (font-engine             kmscon-configuration-font-engine
                           (default "pango"))
  (font-size               kmscon-configuration-font-size
                           (default 12))
  (keyboard-layout         kmscon-configuration-keyboard-layout
                           (default #f))) ; #f | <keyboard-layout>

(define kmscon-service-type
  (shepherd-service-type
   'kmscon
   (lambda (config)
     (let ((kmscon (kmscon-configuration-kmscon config))
           (virtual-terminal (kmscon-configuration-virtual-terminal config))
           (login-program (kmscon-configuration-login-program config))
           (login-arguments (kmscon-configuration-login-arguments config))
           (auto-login (kmscon-configuration-auto-login config))
           (hardware-acceleration? (kmscon-configuration-hardware-acceleration? config))
           (font-engine (kmscon-configuration-font-engine config))
           (font-size (kmscon-configuration-font-size config))
           (keyboard-layout (kmscon-configuration-keyboard-layout config)))

       (define kmscon-command
         #~(list
            #$(file-append kmscon "/bin/kmscon") "--login"
            "--vt" #$virtual-terminal
            "--no-switchvt" ;Prevent a switch to the virtual terminal.
            "--font-engine" #$font-engine
            "--font-size" #$(number->string font-size)
            #$@(if keyboard-layout
                   (let* ((layout (keyboard-layout-name keyboard-layout))
                          (variant (keyboard-layout-variant keyboard-layout))
                          (model (keyboard-layout-model keyboard-layout))
                          (options (keyboard-layout-options keyboard-layout)))
                     `("--xkb-layout" ,layout
                       ,@(if variant `("--xkb-variant" ,variant) '())
                       ,@(if model `("--xkb-model" ,model) '())
                       ,@(if (null? options)
                             '()
                             `("--xkb-options" ,(string-join options ",")))))
                   '())
            #$@(if hardware-acceleration? '("--hwaccel") '())
            "--login" "--"
            #$login-program #$@login-arguments
            #$@(if auto-login
                   #~(#$auto-login)
                   #~())))

       (shepherd-service
        (documentation "kmscon virtual terminal")
        (requirement '(user-processes udev dbus-system))
        (provision (list (symbol-append 'term- (string->symbol virtual-terminal))))
        (start #~(make-forkexec-constructor
                  #$kmscon-command

                  ;; The installer needs to be able to display glyphs from
                  ;; various scripts, so give it access to unifont.
                  ;; TODO: Make this configurable.
                  #:environment-variables
                  (list (string-append "XDG_DATA_DIRS="
                                       #+font-gnu-unifont "/share"))))
        (stop #~(make-kill-destructor)))))
   (description "Start the @command{kmscon} virtual terminal emulator for the
Linux @dfn{kernel mode setting} (KMS).")))


;;;
;;; Static networking.
;;;

(define (ipv6-address? str)
  "Return true if STR denotes an IPv6 address."
  (false-if-exception (->bool (inet-pton AF_INET6 str))))

(define-compile-time-procedure (assert-valid-address (address string?))
  "Ensure ADDRESS has a valid netmask."
  (unless (cidr->netmask address)
    (raise
     (make-compound-condition
      (formatted-message (G_ "address '~a' lacks a network mask")
                         address)
      (condition (&error-location
                  (location
                   (source-properties->location procedure-call-location))))
      (condition (&fix-hint
                  (hint (format #f (G_ "\
Write, say, @samp{\"~a/24\"} for a 24-bit network mask.")
                                address)))))))
  address)

(define (mac-address? str)
  "Return true if STR is a valid MAC address."
  (let ((pattern (make-regexp "^([0-9A-Fa-f]{2}:?){6}$")))
    (false-if-exception (vector? (regexp-exec pattern str)))))

(define-compile-time-procedure (assert-network-link-mac-address (value identity))
  (cond
   ((eq? value #f) value)
   ((and (string? value) (mac-address? value)) value)
   (else (raise
          (make-compound-condition
           (formatted-message (G_ "Value (~S) is not a valid mac address.~%")
                              value)
           (condition (&error-location
                       (location (source-properties->location procedure-call-location)))))))))

(define-compile-time-procedure (assert-network-link-type (value identity))
  (match value
    (#f value)
    (('quote _) (datum->syntax #'value value))
    (else
     (raise
      (make-compound-condition
       (formatted-message (G_ "Value (~S) is not a symbol.~%") value)
       (condition (&error-location
                   (location (source-properties->location procedure-call-location)))))))))

(define-record-type* <static-networking>
  static-networking make-static-networking
  static-networking?
  (addresses static-networking-addresses)         ;list of <network-address>
  (links     static-networking-links (default '())) ;list of <network-link>
  (routes    static-networking-routes (default '())) ;list of <network-routes>
  (provision static-networking-provision
             (default '(networking)))
  (requirement static-networking-requirement
               (default '(udev)))
  (name-servers static-networking-name-servers    ;FIXME: doesn't belong here
                (default '())))

(define-record-type* <network-address>
  network-address make-network-address
  network-address?
  (device    network-address-device)              ;string--e.g., "en01"
  (value     network-address-value                ;string--CIDR notation
             (sanitize assert-valid-address))
  (ipv6?     network-address-ipv6?                ;Boolean
             (thunked)
             (default
               (ipv6-address? (cidr->ip (network-address-value this-record))))))

(define-record-type* <network-link>
  network-link make-network-link
  network-link?
  (name      network-link-name
             (default #f))                   ;string or #f --e.g, "v0p0"
  (type      network-link-type
             (sanitize assert-network-link-type)
             (default #f))                   ;symbol or #f--e.g.,'veth, 'bond
  (mac-address network-link-mac-address
               (sanitize assert-network-link-mac-address)
               (default #f))
  (arguments network-link-arguments))             ;list

(define-record-type* <network-route>
  network-route make-network-route
  network-route?
  (destination network-route-destination)
  (source      network-route-source (default #f))
  (device      network-route-device (default #f))
  (ipv6?       network-route-ipv6? (thunked)
               (default
                 (or (ipv6-address? (network-route-destination this-record))
                     (and=> (network-route-gateway this-record)
                            ipv6-address?))))
  (gateway     network-route-gateway (default #f)))

(eval-when (expand load eval)
  (define* (cidr->netmask str #:optional (family AF_INET))
    "Given @var{str}, a string in CIDR notation (e.g., \"1.2.3.4/24\"), return
the netmask as a string like \"255.255.255.0\"."
    (match (string-split str #\/)
      ((ip (= string->number bits))
       (let ((mask (ash (- (expt 2 bits) 1)
                        (- (if (= family AF_INET6) 128 32)
                           bits))))
         (inet-ntop family mask)))
      (_ #f))))

(define (cidr->ip str)
  "Strip the netmask bit of @var{str}, a CIDR-notation IP/netmask address."
  (match (string-split str #\/)
    ((or (ip _) (ip))
     ip)))

(define* (ip+netmask->cidr ip netmask #:optional (family AF_INET))
  "Return the CIDR notation (a string) for @var{ip} and @var{netmask}, two
@var{family} address strings, where @var{family} is @code{AF_INET} or
@code{AF_INET6}."
  (let* ((netmask (inet-pton family netmask))
         (bits    (logcount netmask)))
    (string-append ip "/" (number->string bits))))

(define (static-networking->hurd-pfinet-options config)
  "Return command-line options for the Hurd's pfinet translator corresponding
to CONFIG."
  (unless (null? (static-networking-links config))
    ;; XXX: Presumably this is not supported, or perhaps could be approximated
    ;; by running separate pfinet instances in some cases?
    (warning (G_ "network links are currently ignored on GNU/Hurd~%")))

  (match (static-networking-addresses config)
    ((and addresses (first _ ...))
     `("--ipv6" "/servers/socket/26"
       "--interface" ,(string-append "/dev/" (network-address-device first))
       ,@(append-map (lambda (address)
                       `(,(if (network-address-ipv6? address)
                              "--address6"
                              "--address")
                         ,(cidr->ip (network-address-value address))
                         ,@(match (cidr->netmask (network-address-value address)
                                                 (if (network-address-ipv6? address)
                                                     AF_INET6
                                                     AF_INET))
                             (#f '())
                             (mask (list "--netmask" mask)))))
                     addresses)
       ,@(append-map (lambda (route)
                       (match route
                         (($ <network-route> "default" #f device _ gateway)
                          (if (network-route-ipv6? route)
                              `("--gateway6" ,gateway)
                              `("--gateway" ,gateway)))
                         (($ <network-route> destination)
                          (warning (G_ "ignoring network route for '~a'~%")
                                   destination)
                          '())))
                     (static-networking-routes config))))))

(define (network-set-up/hurd config)
  "Set up networking for the Hurd."
  ;; The Hurd implements SIOCGIFADDR and other old-style ioctls, but the only
  ;; way to set up IPv6 is by starting pfinet with the right options.
  (if (equal? (static-networking-provision config) '(loopback))
      (program-file "set-up-pflocal" #~(begin 'nothing-to-do! #t))
      (program-file "set-up-pfinet"
                    (with-imported-modules '((guix build utils))
                      #~(begin
                          (use-modules (guix build utils)
                                       (ice-9 format))

                          ;; TODO: Do that without forking.
                          (let ((options '#$(static-networking->hurd-pfinet-options
                                             config)))
                            (format #t "starting '~a~{ ~s~}'~%"
                                    #$(file-append hurd "/hurd/pfinet")
                                    options)
                            (apply invoke #$(file-append hurd "/bin/settrans")
                                   "--active"
                                   "--create"
                                   "--keep-active"
                                   "/servers/socket/2"
                                   #$(file-append hurd "/hurd/pfinet")
                                   options)))))))

(define (network-tear-down/hurd config)
  (program-file "tear-down-pfinet"
                (with-imported-modules '((guix build utils))
                  #~(begin
                      (use-modules (guix build utils))

                      ;; Forcefully terminate pfinet.  XXX: In theory this
                      ;; should just undo the addresses and routes of CONFIG;
                      ;; this could be done using ioctls like SIOCDELRT, but
                      ;; these are IPv4-only; another option would be to use
                      ;; fsysopts but that seems to crash pfinet.
                      (invoke #$(file-append hurd "/bin/settrans") "-fg"
                              "/servers/socket/2")
                      #f))))

(define (network-set-up/linux config)
  (define max-set-up-duration
    ;; Maximum waiting time in seconds for devices to be up.
    60)

  (match-record config <static-networking>
    (addresses links routes)
    (program-file "set-up-network"
                  (with-extensions (list guile-netlink)
                    #~(begin
                        (use-modules (ip addr) (ip link) (ip route)
                                     (srfi srfi-1)
                                     (ice-9 format)
                                     (ice-9 match))

                        (define (match-link-by field-accessor value)
                          (fold (lambda (link result)
                                  (if (equal? (field-accessor link) value)
                                      link
                                      result))
                                #f
                                (get-links)))

                        (define (alist->keyword+value alist)
                          (fold (match-lambda*
                                  (((k . v) r)
                                   (cons* (symbol->keyword k) v r))) '() alist))

                        ;; FIXME: It is interesting that "modprobe bonding" creates an
                        ;; interface bond0 straigt away.  If we won't have bonding
                        ;; module, and execute `ip link add name bond0 type bond' we
                        ;; will get
                        ;;
                        ;; RTNETLINK answers: File exists
                        ;;
                        ;; This breaks our configuration if we want to
                        ;; use `bond0' name.  Create (force modprobe
                        ;; bonding) and delete the interface to free up
                        ;; bond0 name.
                        #$(let lp ((links links))
                            (cond
                             ((null? links) #f)
                             ((and (network-link? (car links))
                                   ;; Type is not mandatory
                                   (false-if-exception
                                    (eq? (network-link-type (car links)) 'bond)))
                              #~(begin
                                  (false-if-exception (link-add "bond0" "bond"))
                                  (link-del "bond0")))
                             (else (lp (cdr links)))))

                        #$@(map (match-lambda
                                  (($ <network-link> name type mac-address arguments)
                                   (cond
                                    ;; Create a new interface
                                    ((and (string? name) (symbol? type))
                                     #~(begin
                                         (link-add #$name (symbol->string '#$type) #:type-args '#$arguments)
                                         ;; XXX: If we add routes, addresses must be
                                         ;; already assigned, and interfaces must be
                                         ;; up. It doesn't matter if they won't have
                                         ;; carrier or anything.
                                         (link-set #$name #:up #t)))

                                    ;; Amend an existing interface
                                    ((and (string? name)
                                          (eq? type #f))
                                     #~(let ((link (match-link-by link-name #$name)))
                                         (if link
                                             (apply link-set
                                                    (link-id link)
                                                    (alist->keyword+value '#$arguments))
                                             (format #t (G_ "Interface with name '~a' not found~%") #$name))))
                                    ((string? mac-address)
                                     #~(let ((link (match-link-by link-addr #$mac-address)))
                                         (if link
                                             (apply link-set
                                                    (link-id link)
                                                    (alist->keyword+value '#$arguments))
                                             (format #t (G_ "Interface with mac-address '~a' not found~%") #$mac-address)))))))
                                links)

                        ;; 'wait-for-link' below could wait forever when
                        ;; passed a non-existent device.  To ensure timely
                        ;; completion, install an alarm.
                        (alarm #$max-set-up-duration)

                        #$@(map (lambda (address)
                                  #~(let ((device
                                           #$(network-address-device address)))
                                      ;; Before going any further, wait for the
                                      ;; device to show up.
                                      (format #t "Waiting for network device '~a'...~%"
                                              device)
                                      (wait-for-link device)

                                      (addr-add #$(network-address-device address)
                                                #$(network-address-value address)
                                                #:ipv6?
                                                #$(network-address-ipv6? address))
                                      ;; FIXME: loopback?
                                      (link-set #$(network-address-device address)
                                                #:multicast-on #t
                                                #:up #t)))
                                addresses)

                        #$@(map (lambda (route)
                                  #~(route-add #$(network-route-destination route)
                                               #:device
                                               #$(network-route-device route)
                                               #:ipv6?
                                               #$(network-route-ipv6? route)
                                               #:via
                                               #$(network-route-gateway route)
                                               #:src
                                               #$(network-route-source route)))
                                routes)
                        #t)))))

(define (network-tear-down/linux config)
  (match-record config <static-networking>
    (addresses links routes)
    (program-file "tear-down-network"
                  (with-extensions (list guile-netlink)
                    #~(begin
                        (use-modules (ip addr) (ip link) (ip route)
                                     (netlink error)
                                     (srfi srfi-34))

                        (define-syntax-rule (false-if-netlink-error exp)
                          (guard (c ((netlink-error? c) #f))
                            exp))

                        ;; Wrap calls in 'false-if-netlink-error' so this
                        ;; script goes as far as possible undoing the effects
                        ;; of "set-up-network".

                        #$@(map (lambda (route)
                                  #~(false-if-netlink-error
                                     (route-del #$(network-route-destination route)
                                                #:device
                                                #$(network-route-device route)
                                                #:ipv6?
                                                #$(network-route-ipv6? route)
                                                #:via
                                                #$(network-route-gateway route)
                                                #:src
                                                #$(network-route-source route))))
                                routes)

                        ;; Cleanup addresses first, they might be assigned to
                        ;; created bonds, vlans or bridges.
                        #$@(map (lambda (address)
                                  #~(false-if-netlink-error
                                     (addr-del #$(network-address-device
                                                  address)
                                               #$(network-address-value address)
                                               #:ipv6?
                                               #$(network-address-ipv6? address))))
                                addresses)

                        ;; It is now safe to delete some links
                        #$@(map (match-lambda
                                  (($ <network-link> name type mac-address arguments)
                                   (cond
                                    ;; We delete interfaces that were created
                                    ((and (string? name) (symbol? type))
                                     #~(false-if-netlink-error
                                        (link-del #$name)))
                                    (else #t))))
                                links)
                        #f)))))

(define (static-networking-shepherd-service config)
  (match-record config <static-networking>
    (addresses links routes provision requirement name-servers)
    (let ((loopback? (and provision (memq 'loopback provision))))
      (shepherd-service

       (documentation
        "Bring up the networking interface using a static IP address.")
       (requirement requirement)
       (provision provision)

       (start #~(lambda _
                  ;; Return #t if successfully started.
                  (zero? (system*
                          #$(let-system (system target)
                              (if (string-contains (or target system) "-linux")
                                  (network-set-up/linux config)
                                  (network-set-up/hurd config)))))))
       (stop #~(lambda _
                 ;; Return #f is successfully stopped.
                 (zero? (system*
                         #$(let-system (system target)
                             (if (string-contains (or target system) "-linux")
                                 (network-tear-down/linux config)
                                 (network-tear-down/hurd config)))))))
       (respawn? #f)))))

(define (static-networking-shepherd-services networks)
  (map static-networking-shepherd-service networks))

(define (static-networking-etc-files interfaces)
  "Return a /etc/resolv.conf entry for INTERFACES or the empty list."
  (match (delete-duplicates
          (append-map static-networking-name-servers
                      interfaces))
    (()
     '())
    ((name-servers ...)
     (let ((content (string-join
                     (map (cut string-append "nameserver " <>)
                          name-servers)
                     "\n" 'suffix)))
       `(("resolv.conf"
          ,(plain-file "resolv.conf"
                       (string-append "\
# Generated by 'static-networking-service'.\n"
                                      content))))))))

(define static-networking-service-type
  ;; The service type for statically-defined network interfaces.
  (service-type (name 'static-networking)
                (extensions
                 (list
                  (service-extension shepherd-root-service-type
                                     static-networking-shepherd-services)
                  (service-extension etc-service-type
                                     static-networking-etc-files)))
                (compose concatenate)
                (extend append)
                (description
                 "Turn up the specified network interfaces upon startup,
with the given IP address, gateway, netmask, and so on.  The value for
services of this type is a list of @code{static-networking} objects, one per
network interface.")))

(define-deprecated (static-networking-service interface ip
                                              #:key
                                              netmask gateway provision
                                              ;; Most interfaces require udev to be usable.
                                              (requirement '(udev))
                                              (name-servers '()))
  static-networking-service-type
  "Return a service that starts @var{interface} with address @var{ip}.  If
@var{netmask} is true, use it as the network mask.  If @var{gateway} is true,
it must be a string specifying the default network gateway.

This procedure can be called several times, one for each network
interface of interest.  Behind the scenes what it does is extend
@code{static-networking-service-type} with additional network interfaces
to handle."
  (simple-service 'static-network-interface
                  static-networking-service-type
                  (list (static-networking
                         (addresses
                          (list (network-address
                                 (device interface)
                                 (value (if netmask
                                            (ip+netmask->cidr ip netmask)
                                            ip))
                                 (ipv6? #f))))
                         (routes
                          (if gateway
                              (list (network-route
                                     (destination "default")
                                     (gateway gateway)
                                     (ipv6? #f)))
                              '()))
                         (requirement requirement)
                         (provision (or provision '(networking)))
                         (name-servers name-servers)))))

(define %loopback-static-networking
  ;; The loopback device.
  (static-networking
   (addresses (list (network-address
                     (device "lo")
                     (value "127.0.0.1/8"))))
   (requirement '())
   (provision '(loopback))))

(define %qemu-static-networking
  ;; Networking configuration for QEMU's user-mode network stack (info "(QEMU)
  ;; Using the user mode network stack").
  (static-networking
   (addresses (list (network-address
                     (device "eth0")
                     (value "10.0.2.15/24"))))
   (routes (list (network-route
                  (destination "default")
                  (gateway "10.0.2.2"))))
   (requirement '())
   (provision '(networking))
   (name-servers '("10.0.2.3"))))


;;;
;;; greetd-service-type -- minimal and flexible login manager daemon
;;;

(define-record-type* <greetd-user-session>
  greetd-user-session make-greetd-user-session greetd-user-session?
  (command greetd-user-session-command (default (file-append bash "/bin/bash")))
  (command-args greetd-user-session-command-args (default '("-l")))
  (extra-env greetd-user-session-extra-env (default '()))
  (xdg-session-type greetd-user-session-xdg-session-type (default "tty"))
  (xdg-env? greetd-user-session-xdg-env? (default #t)))

(define (make-greetd-user-session-command config)
  (match-record config <greetd-user-session>
                (command command-args extra-env)
                (program-file
                 "greetd-user-session-command"
                 #~(begin
                     (use-modules (ice-9 match))
                     (for-each (match-lambda ((var . val) (setenv var val)))
                               (quote (#$@extra-env)))
                     (apply execl #$command #$command
                            (list #$@command-args))))))

(define (make-greetd-xdg-user-session-command config)
  (match-record config <greetd-user-session>
                (command command-args extra-env xdg-session-type)
                (program-file
                 "greetd-xdg-user-session-command"
                 #~(begin
                     (use-modules (ice-9 match))
                     (let* ((username (getenv "USER"))
                            (useruid (passwd:uid (getpwuid username)))
                            (useruid (number->string useruid)))
                       (setenv "XDG_SESSION_TYPE" #$xdg-session-type)
                       (setenv "XDG_RUNTIME_DIR"
                               (string-append "/run/user/" useruid)))
                     (for-each (match-lambda ((var . val) (setenv var val)))
                               (quote (#$@extra-env)))
                     (apply execl #$command #$command
                            (list #$@command-args))))))

(define-gexp-compiler (greetd-user-session-compiler
                       (session <greetd-user-session>)
                       system target)
  (lower-object
   ((if (greetd-user-session-xdg-env? session)
        make-greetd-xdg-user-session-command
        make-greetd-user-session-command) session)))

(define-record-type* <greetd-agreety-session>
  greetd-agreety-session make-greetd-agreety-session greetd-agreety-session?
  (agreety greetd-agreety-session-agreety (default greetd))
  (command greetd-agreety-session-command
           (default (greetd-user-session))
           (sanitize warn-greetd-agreety-session-command-type))
  (command-args greetd-agreety-command-args
                (default #nil)
                (sanitize warn-deprecated-greetd-agreety-command-args))
  (extra-env greetd-agreety-extra-env
             (default #nil)
             (sanitize warn-deprecated-greetd-agreety-extra-env))
  (xdg-env? greetd-agreety-xdg-env?
            (default #nil)
            (sanitize warn-deprecated-greetd-agreety-xdg-env?)))

(define (warn-deprecated-greetd-agreety-command-args value)
  (unless (nil? value)
    (warn-about-deprecation
     'command-args #f
     #:replacement '<greetd-user-seesion>))
  value)

(define (warn-deprecated-greetd-agreety-extra-env value)
  (unless (nil? value)
    (warn-about-deprecation
     'extra-env #f
     #:replacement '<greetd-user-seesion>))
  value)

(define (warn-deprecated-greetd-agreety-xdg-env? value)
  (unless (nil? value)
    (warn-about-deprecation
     'xdg-env? #f
     #:replacement '<greetd-user-seesion>))
  value)

(define-deprecated/alias greetd-agreety greetd-agreety-session-agreety)
(define-deprecated/alias greetd-agreety-command greetd-agreety-session-command)

(define (warn-greetd-agreety-session-command-type value)
  (unless (greetd-user-session? value)
    (warn-about-deprecation
     "arbitrary command" #f
     #:replacement '<greetd-user-session>))
  value)

(define (greetd-agreety-session-to-user-session session default-command)
  (let ((command (greetd-agreety-session-command session))
        (command-args (or (greetd-agreety-command-args session)
                          (greetd-user-session-command-args default-command)))
        (extra-env (or (greetd-agreety-extra-env session)
                       (greetd-user-session-extra-env default-command)))
        (xdg-env? (or (greetd-agreety-xdg-env? session)
                      (greetd-user-session-xdg-env? default-command))))
    (greetd-user-session
     (command command)
     (command-args command-args)
     (extra-env extra-env)
     (xdg-env? xdg-env?))))

(define-gexp-compiler (greetd-agreety-session-compiler
                       (session <greetd-agreety-session>)
                       system target)
  (let* ((agreety
          (file-append (greetd-agreety-session-agreety session) "/bin/agreety"))
         (command
          (greetd-agreety-session-command session))
         (command
          (if (greetd-user-session? command)
              command
              (greetd-agreety-session-to-user-session
               session
               (greetd-user-session)))))
    (lower-object
     (program-file
      "agreety-wrapper"
      #~(execl #$agreety #$agreety "-c" #$command)))))

(define (make-greetd-sway-greeter-command sway sway-config)
  (let ((sway-bin (file-append sway "/bin/sway")))
    (program-file
     "greeter-sway-command"
     (with-imported-modules '((guix build utils))
       #~(begin
           (use-modules (guix build utils))

           (let* ((username (getenv "USER"))
                  (user (getpwnam username))
                  (useruid (passwd:uid user))
                  (usergid (passwd:gid user))
                  (useruid-s (number->string useruid))
                  ;; /run/user/<greeter-user-uid> won't exist yet
                  ;; this will contain WAYLAND_DISPLAY socket file
                  ;; and log-file below
                  (user-home-dir "/tmp/.greeter-home")
                  (user-xdg-runtime-dir (string-append user-home-dir "/run"))
                  (user-xdg-cache-dir (string-append user-home-dir "/cache"))
                  (log-file (string-append (number->string (getpid)) ".log"))
                  (log-file (string-append user-home-dir "/" log-file)))
             (for-each (lambda (d)
                         (mkdir-p d)
                         (chown d useruid usergid) (chmod d #o700))
                       (list user-home-dir
                             user-xdg-runtime-dir
                             user-xdg-cache-dir))
             (setenv "HOME" user-home-dir)
             (setenv "XDG_CACHE_DIR" user-xdg-cache-dir)
             (setenv "XDG_RUNTIME_DIR" user-xdg-runtime-dir)
             (dup2 (open-fdes log-file
                              (logior O_CREAT O_WRONLY O_APPEND) #o640) 1)
             (dup2 1 2)
             (execl #$sway-bin #$sway-bin "-d" "-c" #$sway-config)))))))

(define-record-type* <greetd-wlgreet-configuration>
  greetd-wlgreet-configuration make-greetd-wlgreet-configuration
  greetd-wlgreet-configuration?
  (output-mode greetd-wlgreet-configuration-output-mode (default "all"))
  (scale greetd-wlgreet-configuration-scale (default 1))
  (background greetd-wlgreet-configuration-background (default '(0 0 0 0.9)))
  (headline greetd-wlgreet-configuration-headline (default '(1 1 1 1)))
  (prompt greetd-wlgreet-configuration-prompt (default '(1 1 1 1)))
  (prompt-error greetd-wlgreet-configuration-prompt-error (default '(1 1 1 1)))
  (border greetd-wlgreet-configuration-border (default '(1 1 1 1)))
  (wlgreet greetd-wlgreet
           (default #nil)
           (sanitize warn-deprecated-greetd-wlgreet))
  (command greetd-wlgreet-command
           (default #nil)
           (sanitize warn-deprecated-greetd-wlgreet-command))
  (command-args greetd-wlgreet-command-args
                (default #nil)
                (sanitize warn-deprecated-greetd-wlgreet-command-args))
  (extra-env greetd-wlgreet-extra-env
             (default #nil)
             (sanitize warn-deprecated-greetd-wlgreet-extra-env)))

(define-deprecated/alias greetd-wlgreet-session greetd-wlgreet-configuration)

(define (warn-deprecated-greetd-wlgreet value)
  (unless (nil? value)
    (warn-about-deprecation
     'wlgreet #f
     #:replacement '<greetd-wlgreet-sway-session>))
  value)

(define (warn-deprecated-greetd-wlgreet-command value)
  (unless (nil? value)
    (warn-about-deprecation
     'command #f
     #:replacement '<greetd-wlgreet-sway-session>))
  value)

(define (warn-deprecated-greetd-wlgreet-command-args value)
  (unless (nil? value)
    (warn-about-deprecation
     'command-args #f
     #:replacement '<greetd-wlgreet-sway-session>))
  value)

(define (warn-deprecated-greetd-wlgreet-extra-env value)
  (unless (nil? value)
    (warn-about-deprecation
     'extra-env #f
     #:replacement '<greetd-wlgreet-sway-session>))
  value)

(define (make-greetd-wlgreet-config-color section-name color)
  (match color
    ((red green blue opacity)
     (string-append
      "[" section-name "]\n"
      "red = " (number->string red) "\n"
      "green = " (number->string green) "\n"
      "blue = " (number->string blue) "\n"
      "opacity = " (number->string opacity) "\n"))))

(define (make-greetd-wlgreet-config command color)
  (match-record color <greetd-wlgreet-configuration>
    (output-mode scale background headline prompt prompt-error border)
    (mixed-text-file
     "wlgreet.toml"
     "command = \"" command "\"\n"
     "outputMode = \"" output-mode "\"\n"
     "scale = " (number->string scale) "\n"
     (apply string-append
            (map (match-lambda
                   ((section-name . color)
                    (make-greetd-wlgreet-config-color section-name color)))
                 `(("background" . ,background)
                   ("headline" . ,headline)
                   ("prompt" . ,prompt)
                   ("prompt-error" . ,prompt-error)
                   ("border" . ,border)))))))

(define-record-type* <greetd-wlgreet-sway-session>
  greetd-wlgreet-sway-session make-greetd-wlgreet-sway-session
  greetd-wlgreet-sway-session?
  (sway greetd-wlgreet-sway-session-sway (default sway))
  (sway-configuration greetd-wlgreet-sway-session-sway-configuration
                      (default #f))
  (wlgreet greetd-wlgreet-sway-session-wlgreet (default wlgreet))
  (wlgreet-configuration greetd-wlgreet-sway-session-wlgreet-configuration
                         (default (greetd-wlgreet-configuration)))
  (command greetd-wlgreet-sway-session-command (default (greetd-user-session)))
  (wlgreet-session
   greetd-wlgreet-sway-session-wlgreet-session
   (default #nil)
   (sanitize warn-deprecated-greetd-wlgreet-sway-session-wlgreet-session)))

(define (warn-deprecated-greetd-wlgreet-sway-session-wlgreet-session value)
  (unless (nil? value)
    (warn-about-deprecation
     'wlgreet-session #f
     #:replacement 'wlgreet-configuration))
  value)

(define (make-greetd-wlgreet-sway-session-sway-config session)
  (match-record session <greetd-wlgreet-sway-session>
                (sway sway-configuration wlgreet wlgreet-configuration command)
    (let ((wlgreet-bin (file-append wlgreet "/bin/wlgreet"))
          (wlgreet-config-file
           (make-greetd-wlgreet-config command wlgreet-configuration))
          (swaymsg-bin (file-append sway "/bin/swaymsg")))
      (mixed-text-file
       "wlgreet-sway-config"
       (if sway-configuration
           #~(string-append "include " #$sway-configuration "\n")
           "")
       "xwayland disable\n"
       "exec \"" wlgreet-bin " --config " wlgreet-config-file
       "; " swaymsg-bin " exit\"\n"))))

(define (greetd-wlgreet-session-to-config session config)
  (let* ((wlgreet (or (greetd-wlgreet config)
                      (greetd-wlgreet-sway-session-wlgreet session)))
         (default-command (greetd-wlgreet-sway-session-command session))
         (command (or (greetd-wlgreet-command config)
                      (greetd-user-session-command default-command)))
         (command-args (or (greetd-wlgreet-command-args config)
                           (greetd-user-session-command-args default-command)))
         (extra-env (or (greetd-wlgreet-extra-env config)
                        (greetd-user-session-extra-env default-command))))
    (greetd-wlgreet-sway-session
     (sway (greetd-wlgreet-sway-session-sway session))
     (sway-configuration
      (greetd-wlgreet-sway-session-sway-configuration session))
     (wlgreet wlgreet)
     (wlgreet-configuration config)
     (command
      (greetd-user-session
       (command command)
       (command-args command-args)
       (extra-env extra-env))))))

(define-gexp-compiler (greetd-wlgreet-sway-session-compiler
                       (session <greetd-wlgreet-sway-session>)
                       system target)
  (let ((s (if (nil? (greetd-wlgreet-sway-session-wlgreet-session session))
               session
               (greetd-wlgreet-session-to-config
                session
                (greetd-wlgreet-sway-session-wlgreet-session session)))))
    (match-record s <greetd-wlgreet-sway-session> (sway)
      (lower-object
       (make-greetd-sway-greeter-command
        sway
        (make-greetd-wlgreet-sway-session-sway-config s))))))

(define-record-type* <greetd-gtkgreet-sway-session>
  greetd-gtkgreet-sway-session make-greetd-gtkgreet-sway-session
  greetd-gtkgreet-sway-session?
  (sway greetd-gtkgreet-sway-session-sway (default sway))
  (sway-configuration greetd-gtkgreet-sway-session-sway-configuration
                      (default #f))
  (gtkgreet greetd-gtkgreet-sway-session-gtkgreet (default gtkgreet))
  (gtkgreet-style greetd-gtkgreet-sway-session-gtkgreet-style (default #f))
  (command greetd-gtkgreet-sway-session-command
           (default (greetd-user-session))))

(define (make-greetd-gtkgreet-sway-session-sway-config session)
  (match-record session <greetd-gtkgreet-sway-session>
                (sway sway-configuration gtkgreet gtkgreet-style command)
    (let ((gtkgreet-bin (file-append gtkgreet "/bin/gtkgreet"))
          (swaymsg-bin (file-append sway "/bin/swaymsg")))
      (mixed-text-file
       "gtkgreet-sway-config"
       (if sway-configuration
           #~(string-append "include " #$sway-configuration "\n")
           "")
       "xwayland disable\n"
       "exec \"" gtkgreet-bin " -l"
       (if gtkgreet-style #~(string-append " -s " #$gtkgreet-style) "")
       " -c " command "; " swaymsg-bin " exit\"\n"))))

(define-gexp-compiler (greetd-gtkgreet-sway-session-compiler
                       (session <greetd-gtkgreet-sway-session>)
                       system target)
  (match-record session <greetd-gtkgreet-sway-session> (sway)
    (lower-object
     (make-greetd-sway-greeter-command
      sway
      (make-greetd-gtkgreet-sway-session-sway-config session)))))

(define-record-type* <greetd-terminal-configuration>
  greetd-terminal-configuration make-greetd-terminal-configuration
  greetd-terminal-configuration?
  (greetd greetd-package (default greetd))
  (extra-shepherd-requirement greetd-extra-shepherd-requirement (default '()))
  (config-file-name greetd-config-file-name (thunked)
                    (default (default-config-file-name this-record)))
  (log-file-name greetd-log-file-name (thunked)
                 (default (default-log-file-name this-record)))
  (terminal-vt greetd-terminal-vt (default "7"))
  (terminal-switch greetd-terminal-switch (default #f))
  (source-profile? greetd-source-profile? (default #t))
  (default-session-user greetd-default-session-user (default "greeter"))
  (default-session-command greetd-default-session-command
    (default (greetd-agreety-session))))

(define (default-config-file-name config)
  (string-join (list "config-" (greetd-terminal-vt config) ".toml") ""))

(define (default-log-file-name config)
  (string-join (list "/var/log/greetd-" (greetd-terminal-vt config) ".log") ""))

(define (make-greetd-terminal-configuration-file config)
  (let*
      ((config-file-name (greetd-config-file-name config))
       (source-profile? (greetd-source-profile? config))
       (terminal-vt (greetd-terminal-vt config))
       (terminal-switch (greetd-terminal-switch config))
       (default-session-user (greetd-default-session-user config))
       (default-session-command (greetd-default-session-command config)))
    (mixed-text-file
     config-file-name
     "[general]\n"
     "source_profile = " (if source-profile? "true" "false") "\n"
     "[terminal]\n"
     "vt = " terminal-vt "\n"
     "switch = " (if terminal-switch "true" "false") "\n"
     "[default_session]\n"
     "user = " default-session-user "\n"
     "command = " default-session-command "\n")))

(define %greetd-file-systems
  (list (file-system
          (device "none")
          (mount-point "/run/greetd/pam_mount")
          (type "tmpfs")
          (check? #f)
          (flags '(no-suid no-dev no-exec))
          (options "mode=0755")
          (create-mount-point? #t))))

(define %greetd-pam-mount-rules
  `((debug (@ (enable "0")))
    (volume (@ (sgrp "users")
               (fstype "tmpfs")
               (mountpoint "/run/user/%(USERUID)")
               (options "noexec,nosuid,nodev,size=1g,mode=0700,uid=%(USERUID),gid=%(USERGID)")))
    (logout (@ (wait "0")
               (hup "0")
               (term "yes")
               (kill "no")))
    (mkmountpoint (@ (enable "1") (remove "true")))))

(define-record-type* <greetd-configuration>
  greetd-configuration make-greetd-configuration
  greetd-configuration?
  (motd greetd-motd (default %default-motd))
  (allow-empty-passwords? greetd-allow-empty-passwords? (default #t))
  (terminals greetd-terminals (default '()))
  (greeter-supplementary-groups greetd-greeter-supplementary-groups (default '())))

(define (greetd-accounts config)
  (list (user-group (name "greeter") (system? #t))
        (user-account
         (name "greeter")
         (group "greeter")
         (supplementary-groups (greetd-greeter-supplementary-groups config))
         (system? #t)
         (create-home-directory? #f))))

(define (make-greetd-pam-mount-conf-file config)
  (computed-file
   "greetd_pam_mount.conf.xml"
   #~(begin
       (use-modules (sxml simple))
       (call-with-output-file #$output
         (lambda (port)
           (sxml->xml
            '(*TOP*
              (*PI* xml "version='1.0' encoding='utf-8'")
              (pam_mount
               #$@%greetd-pam-mount-rules
               (pmvarrun
                #$(file-append greetd-pam-mount
                               "/sbin/pmvarrun -u '%(USER)' -o '%(OPERATION)'"))))
            port))))))

(define (greetd-etc-service config)
  `(("security/greetd_pam_mount.conf.xml"
     ,(make-greetd-pam-mount-conf-file config))))

(define (greetd-pam-service config)
  (define optional-pam-mount
    (pam-entry
     (control "optional")
     (module (file-append greetd-pam-mount "/lib/security/pam_mount.so"))
     (arguments '("disable_interactive"))))

  (list
   (unix-pam-service "greetd"
                     #:login-uid? #t
                     #:allow-empty-passwords?
                     (greetd-allow-empty-passwords? config)
                     #:motd
                     (greetd-motd config))
   (pam-extension
    (transformer
     (lambda (pam)
       (if (member (pam-service-name pam)
                   '("login" "greetd" "su" "slim" "gdm-password"))
           (pam-service
            (inherit pam)
            (auth (append (pam-service-auth pam)
                          (list optional-pam-mount)))
            (session (append (pam-service-session pam)
                             (list optional-pam-mount))))
           pam))))))

(define (greetd-run-user-activation config)
  #~(begin
      (use-modules (guix build utils))
      (let ((d "/run/user"))
        (mkdir-p d)
        (chmod d #o755))))

(define (greetd-shepherd-services config)
  (map
   (lambda (tc)
     (let*
         ((greetd-bin (file-append (greetd-package tc) "/sbin/greetd"))
          (greetd-conf (make-greetd-terminal-configuration-file tc))
          (greetd-log (greetd-log-file-name tc))
          (greetd-vt (greetd-terminal-vt tc))
          (greetd-requirement (greetd-extra-shepherd-requirement tc)))
       (shepherd-service
        (documentation "Minimal and flexible login manager daemon")
        (requirement `(pam user-processes host-name udev virtual-terminal
                           ,@greetd-requirement))
        (provision (list (symbol-append
                          'term-tty
                          (string->symbol (greetd-terminal-vt tc)))))
        (start #~(make-forkexec-constructor
                  (list #$greetd-bin "-c" #$greetd-conf)
                  #:log-file #$greetd-log))
        (stop #~(make-kill-destructor)))))
   (greetd-terminals config)))

(define greetd-service-type
  (service-type
   (name 'greetd)
   (description "Provides necessary infrastructure for logging into the
system including @code{greetd} PAM service, @code{pam-mount} module to
mount/unmount /run/user/<uid> directory for user and @code{greetd}
login manager daemon.")
   (extensions
    (list
     (service-extension account-service-type greetd-accounts)
     (service-extension file-system-service-type (const %greetd-file-systems))
     (service-extension activation-service-type greetd-run-user-activation)
     (service-extension etc-service-type greetd-etc-service)
     (service-extension pam-root-service-type greetd-pam-service)
     (service-extension shepherd-root-service-type greetd-shepherd-services)))
   (default-value (greetd-configuration))))


(define %base-services
  ;; Convenience variable holding the basic services.
  (list (service login-service-type)

        (service virtual-terminal-service-type)
        (service console-font-service-type
                 (map (lambda (tty)
                        (cons tty %default-console-font))
                      '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

        (service shepherd-system-log-service-type)
        (service agetty-service-type (agetty-configuration
                                       (extra-options '("-L")) ; no carrier detect
                                       (term "vt100")
                                       (tty #f) ; automatic
                                       (shepherd-requirement '(syslogd))))

        (service mingetty-service-type (mingetty-configuration
                                         (tty "tty1")))
        (service mingetty-service-type (mingetty-configuration
                                         (tty "tty2")))
        (service mingetty-service-type (mingetty-configuration
                                         (tty "tty3")))
        (service mingetty-service-type (mingetty-configuration
                                         (tty "tty4")))
        (service mingetty-service-type (mingetty-configuration
                                         (tty "tty5")))
        (service mingetty-service-type (mingetty-configuration
                                         (tty "tty6")))

        (service static-networking-service-type
                 (list %loopback-static-networking))
        (service urandom-seed-service-type)
        (service guix-service-type)
        (service nscd-service-type)

        (service log-rotation-service-type)

        ;; Convenient services brought by the Shepherd.
        (service shepherd-timer-service-type)
        (service shepherd-transient-service-type)

        ;; Periodically delete old build logs.
        (service log-cleanup-service-type
                 (log-cleanup-configuration
                  (directory "/var/log/guix/drvs")))

        ;; The LVM2 rules are needed as soon as LVM2 or the device-mapper is
        ;; used, so enable them by default.  The FUSE and ALSA rules are
        ;; less critical, but handy.
        (service udev-service-type
                 (udev-configuration
                   (rules (list lvm2 fuse alsa-utils crda))))

        (service sysctl-service-type)

        (service special-files-service-type
                 `(("/bin/sh" ,(file-append bash "/bin/sh"))
                   ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))

;;; base.scm ends here
