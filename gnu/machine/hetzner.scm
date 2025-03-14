;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Roman Scherer <roman@burningswell.com>
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

(define-module (gnu machine hetzner)
  #:use-module (gnu bootloader grub)
  #:use-module (gnu bootloader)
  #:use-module (gnu machine hetzner http)
  #:use-module (gnu machine ssh)
  #:use-module (gnu machine)
  #:use-module (gnu packages ssh)
  #:use-module (gnu services base)
  #:use-module (gnu services networking)
  #:use-module (gnu services ssh)
  #:use-module (gnu services)
  #:use-module (gnu system file-systems)
  #:use-module (gnu system image)
  #:use-module (gnu system linux-initrd)
  #:use-module (gnu system pam)
  #:use-module (gnu system)
  #:use-module (guix base32)
  #:use-module (guix colors)
  #:use-module (guix derivations)
  #:use-module (guix diagnostics)
  #:use-module (guix gexp)
  #:use-module (guix i18n)
  #:use-module (guix import json)
  #:use-module (guix monads)
  #:use-module (guix packages)
  #:use-module (guix pki)
  #:use-module (guix records)
  #:use-module (guix ssh)
  #:use-module (guix store)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 match)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (json)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-34)
  #:use-module (srfi srfi-35)
  #:use-module (srfi srfi-71)
  #:use-module (ssh channel)
  #:use-module (ssh key)
  #:use-module (ssh popen)
  #:use-module (ssh session)
  #:use-module (ssh sftp)
  #:use-module (ssh shell)
  #:export (%hetzner-os-arm
            %hetzner-os-x86
            deploy-hetzner
            hetzner-configuration
            hetzner-configuration-allow-downgrades?
            hetzner-configuration-api
            hetzner-configuration-authorize?
            hetzner-configuration-build-locally?
            hetzner-configuration-delete?
            hetzner-configuration-labels
            hetzner-configuration-location
            hetzner-configuration-server-type
            hetzner-configuration-ssh-key
            hetzner-configuration-ssh-public-key
            hetzner-configuration?
            hetzner-environment-type))

;;; Commentary:
;;;
;;; This module implements a high-level interface for provisioning machines on
;;; the Hetzner Cloud service https://docs.hetzner.cloud.
;;;


;;;
;;; Hetzner operating systems.
;;;

;; Operating system for arm servers using UEFI boot mode.

(define %hetzner-os-arm
  (operating-system
    (host-name "guix-arm")
    (bootloader
     (bootloader-configuration
      (bootloader grub-efi-bootloader)
      (targets (list "/boot/efi"))
      (terminal-outputs '(console))))
    (file-systems
     (cons* (file-system
              (mount-point "/")
              (device "/dev/sda1")
              (type "ext4"))
            (file-system
              (mount-point "/boot/efi")
              (device "/dev/sda15")
              (type "vfat"))
            %base-file-systems))
    (initrd-modules
     (cons* "sd_mod" "virtio_scsi" %base-initrd-modules))
    (services
     (cons* (service dhcp-client-service-type)
            (service openssh-service-type
                     (openssh-configuration
                      (openssh openssh-sans-x)
                      (permit-root-login 'prohibit-password)))
            %base-services))))

;; Operating system for x86 servers using BIOS boot mode.

(define %hetzner-os-x86
  (operating-system
    (inherit %hetzner-os-arm)
    (host-name "guix-x86")
    (bootloader
     (bootloader-configuration
      (bootloader grub-bootloader)
      (targets (list "/dev/sda"))
      (terminal-outputs '(console))))
    (initrd-modules
     (cons "virtio_scsi" %base-initrd-modules))
    (file-systems
     (cons (file-system
             (mount-point "/")
             (device "/dev/sda1")
             (type "ext4"))
           %base-file-systems))))

(define (operating-system-authorize os)
  "Authorize the OS with the public signing key of the current machine."
  (if (file-exists? %public-key-file)
      (operating-system
        (inherit os)
        (services
         (modify-services (operating-system-user-services os)
           (guix-service-type
            config => (guix-configuration
                       (inherit config)
                       (authorized-keys
                        (cons*
                         (local-file %public-key-file)
                         (guix-configuration-authorized-keys config))))))))
      (raise-exception
       (formatted-message (G_ "no signing key '~a'. \
Have you run 'guix archive --generate-key'?")
                          %public-key-file))))

(define (operating-system-root-file-system-type os)
  "Return the root file system type of the operating system OS."
  (let ((root-fs (find (lambda (file-system)
                         (equal? "/" (file-system-mount-point file-system)))
                       (operating-system-file-systems os))))
    (if (file-system? root-fs)
        (file-system-type root-fs)
        (raise-exception
         (formatted-message
          (G_ "could not determine root file system type"))))))


;;;
;;; Helper functions.
;;;

(define (escape-backticks str)
  "Escape all backticks in STR."
  (string-replace-substring str "`" "\\`"))



;;;
;;; Hetzner configuration.
;;;

(define-record-type* <hetzner-configuration> hetzner-configuration
  make-hetzner-configuration hetzner-configuration? this-hetzner-configuration
  (allow-downgrades? hetzner-configuration-allow-downgrades? ; boolean
                     (default #f))
  (api hetzner-configuration-api ; <hetzner-api>
       (default (hetzner-api)))
  (authorize? hetzner-configuration-authorize? ; boolean
              (default #t))
  (build-locally? hetzner-configuration-build-locally? ; boolean
                  (default #t))
  (delete? hetzner-configuration-delete? ; boolean
           (default #f))
  (labels hetzner-configuration-labels ; list of strings
          (default '()))
  (location hetzner-configuration-location ; #f | string
            (default "fsn1"))
  (server-type hetzner-configuration-server-type ; string
               (default "cx42"))
  (ssh-public-key hetzner-configuration-ssh-public-key ; public-key | string
                  (thunked)
                  (default (public-key-from-file (hetzner-configuration-ssh-key this-hetzner-configuration)))
                  (sanitize
                   (lambda (value)
                     (if (string? value) (public-key-from-file value) value))))
  (ssh-key hetzner-configuration-ssh-key
           (default #f))) ; #f | string

(define (hetzner-configuration-ssh-key-fingerprint config)
  "Return the SSH public key fingerprint of CONFIG as a string."
  (and-let* ((pubkey (hetzner-configuration-ssh-public-key config))
             (hash (get-public-key-hash pubkey 'md5)))
    (bytevector->hex-string hash)))

(define (hetzner-configuration-ssh-key-public config)
  "Return the SSH public key of CONFIG as a string."
  (let ((public-key (hetzner-configuration-ssh-public-key config)))
    (format #f "ssh-~a ~a" (get-key-type public-key)
            (public-key->string public-key))))


;;;
;;; Hetzner Machine.
;;;

(define (hetzner-machine-delegate target server)
  "Return the delegate machine that uses SSH for deployment."
  (let* ((config (machine-configuration target))
         ;; Get the operating system WITHOUT the provenance service to avoid a
         ;; duplicate symlink conflict in the store.
         (os ((@@ (gnu machine) %machine-operating-system) target)))
    (machine
     (inherit target)
     (operating-system
       (if (hetzner-configuration-authorize? config)
           (operating-system-authorize os)
           os))
     (environment managed-host-environment-type)
     (configuration
      (machine-ssh-configuration
       (allow-downgrades? (hetzner-configuration-allow-downgrades? config))
       (authorize? (hetzner-configuration-authorize? config))
       (build-locally? (hetzner-configuration-build-locally? config))
       (host-name (hetzner-server-public-ipv4 server))
       (identity (hetzner-configuration-ssh-key config))
       (system (hetzner-server-system server)))))))

(define (hetzner-machine-location machine)
  "Find the location of MACHINE on the Hetzner API."
  (let* ((config (machine-configuration machine))
         (expected (hetzner-configuration-location config)))
    (find (lambda (location)
            (equal? expected (hetzner-location-name location)))
          (hetzner-api-locations
           (hetzner-configuration-api config)
           #:params `(("name" . ,expected))))))

(define (hetzner-machine-server-type machine)
  "Find the server type of MACHINE on the Hetzner API."
  (let* ((config (machine-configuration machine))
         (expected (hetzner-configuration-server-type config)))
    (find (lambda (server-type)
            (equal? expected (hetzner-server-type-name server-type)))
          (hetzner-api-server-types
           (hetzner-configuration-api config)
           #:params `(("name" . ,expected))))))

(define (hetzner-machine-validate-api-token machine)
  "Validate the Hetzner API authentication token of MACHINE."
  (let* ((config (machine-configuration machine))
         (api (hetzner-configuration-api config)))
    (unless (hetzner-api-token api)
      (raise-exception
       (formatted-message
        (G_ "Hetzner Cloud access token was not provided. \
This may be fixed by setting the environment variable GUIX_HETZNER_API_TOKEN \
to one procured from \
https://docs.hetzner.com/cloud/api/getting-started/generating-api-token"))))))

(define (hetzner-machine-validate-configuration-type machine)
  "Raise an error if MACHINE's configuration is not an instance of
<hetzner-configuration>."
  (let ((config (machine-configuration machine))
        (environment (environment-type-name (machine-environment machine))))
    (unless (and config (hetzner-configuration? config))
      (raise-exception
       (formatted-message (G_ "unsupported machine configuration '~a' \
for environment of type '~a'")
                          config
                          environment)))))

(define (hetzner-machine-validate-server-type machine)
  "Raise an error if the server type of MACHINE is not supported."
  (unless (hetzner-machine-server-type machine)
    (let* ((config (machine-configuration machine))
           (api (hetzner-configuration-api config)))
      (raise-exception
       (formatted-message
        (G_ "server type '~a' not supported~%~%\
Available server types:~%~%~a~%~%For more details and prices, see: ~a")
        (hetzner-configuration-server-type config)
        (string-join
         (map (lambda (type)
                (format #f " - ~a: ~a, ~a ~a cores, ~a GB mem, ~a GB disk"
                        (colorize-string
                         (hetzner-server-type-name type)
                         (color BOLD))
                        (hetzner-server-type-architecture type)
                        (hetzner-server-type-cores type)
                        (hetzner-server-type-cpu-type type)
                        (hetzner-server-type-memory type)
                        (hetzner-server-type-disk type)))
              (hetzner-api-server-types api))
         "\n")
        "https://www.hetzner.com/cloud#pricing")))))

(define (hetzner-machine-validate-location machine)
  "Raise an error if the location of MACHINE is not supported."
  (unless (hetzner-machine-location machine)
    (let* ((config (machine-configuration machine))
           (api (hetzner-configuration-api config)))
      (raise-exception
       (formatted-message
        (G_ "server location '~a' not supported~%~%\
Available locations:~%~%~a~%~%For more details, see: ~a")
        (hetzner-configuration-location config)
        (string-join
         (map (lambda (location)
                (format #f " - ~a: ~a, ~a"
                        (colorize-string
                         (hetzner-location-name location)
                         (color BOLD))
                        (hetzner-location-description location)
                        (hetzner-location-country location)))
              (hetzner-api-locations api))
         "\n")
        "https://www.hetzner.com/cloud#locations")))))

(define (hetzner-machine-validate machine)
  "Validate the Hetzner MACHINE."
  (hetzner-machine-validate-configuration-type machine)
  (hetzner-machine-validate-api-token machine)
  (hetzner-machine-validate-location machine)
  (hetzner-machine-validate-server-type machine))

(define (hetzner-machine-bootstrap-os-form machine server)
  "Return the form to bootstrap an operating system on SERVER."
  (let* ((os (machine-operating-system machine))
         (system (hetzner-server-system server))
         (arm? (equal? "arm" (hetzner-server-architecture server)))
         (x86? (equal? "x86" (hetzner-server-architecture server)))
         (root-fs-type (operating-system-root-file-system-type os)))
    `(operating-system
       (host-name ,(operating-system-host-name os))
       (timezone "Etc/UTC")
       (bootloader (bootloader-configuration
                    (bootloader ,(cond (arm? 'grub-efi-bootloader)
                                       (x86? 'grub-bootloader)))
                    (targets ,(cond (arm? '(list "/boot/efi"))
                                    (x86? '(list "/dev/sda"))))
                    (terminal-outputs '(console))))
       (initrd-modules (append
                        ,(cond (arm? '(list "sd_mod" "virtio_scsi"))
                               (x86? '(list "virtio_scsi")))
                        %base-initrd-modules))
       (file-systems ,(cond
                       (arm? `(cons* (file-system
                                       (mount-point "/")
                                       (device "/dev/sda1")
                                       (type ,root-fs-type))
                                     (file-system
                                       (mount-point "/boot/efi")
                                       (device "/dev/sda15")
                                       (type "vfat"))
                                     %base-file-systems))
                       (x86? `(cons* (file-system
                                       (mount-point "/")
                                       (device "/dev/sda1")
                                       (type ,root-fs-type))
                                     %base-file-systems))))
       (services
        (cons* (service dhcp-client-service-type)
               (service openssh-service-type
                        (openssh-configuration
                         (openssh openssh-sans-x)
                         (permit-root-login 'prohibit-password)))
               %base-services)))))

(define (rexec-verbose session cmd)
  "Execute a command CMD on the remote side and print output.  Return two
values: list of output lines returned by CMD and its exit code."
  (let* ((channel (open-remote-input-pipe session cmd))
         (result  (let loop ((line   (read-line channel))
                             (result '()))
                    (if (eof-object? line)
                        (reverse result)
                        (begin
                          (display line)
                          (newline)
                          (loop (read-line channel)
                                (cons line result))))))
         (exit-status (channel-get-exit-status channel)))
    (close channel)
    (values result exit-status)))

(define (hetzner-machine-ssh-key machine)
  "Find the SSH key for MACHINE on the Hetzner API."
  (let* ((config (machine-configuration machine))
         (expected (hetzner-configuration-ssh-key-fingerprint config)))
    (find (lambda (ssh-key)
            (equal? expected (hetzner-ssh-key-fingerprint ssh-key)))
          (hetzner-api-ssh-keys
           (hetzner-configuration-api config)
           #:params `(("fingerprint" . ,expected))))))

(define (hetzner-machine-ssh-key-create machine)
  "Create the SSH key for MACHINE on the Hetzner API."
  (let ((name (machine-display-name machine)))
    (format #t "creating ssh key for '~a'...\n" name)
    (let* ((config (machine-configuration machine))
           (api (hetzner-configuration-api config))
           (ssh-key (hetzner-api-ssh-key-create
                     (hetzner-configuration-api config)
                     (hetzner-configuration-ssh-key-fingerprint config)
                     (hetzner-configuration-ssh-key-public config)
                     #:labels (hetzner-configuration-labels config))))
      (format #t "successfully created ssh key for '~a'\n" name)
      ssh-key)))

(define (hetzner-machine-server machine)
  "Find the Hetzner server for MACHINE."
  (let ((config (machine-configuration machine)))
    (find (lambda (server)
            (equal? (machine-display-name machine)
                    (hetzner-server-name server)))
          (hetzner-api-servers
           (hetzner-configuration-api config)
           #:params `(("name" . ,(machine-display-name machine)))))))

(define (hetzner-machine-create-server machine)
  "Create the Hetzner server for MACHINE."
  (let* ((config (machine-configuration machine))
         (name (machine-display-name machine))
         (server-type (hetzner-configuration-server-type config)))
    (format #t "creating '~a' server for '~a'...\n" server-type name)
    (let* ((ssh-key (hetzner-machine-ssh-key machine))
           (api (hetzner-configuration-api config))
           (server (hetzner-api-server-create
                    api
                    (machine-display-name machine)
                    (list ssh-key)
                    #:labels (hetzner-configuration-labels config)
                    #:location (hetzner-configuration-location config)
                    #:server-type (hetzner-configuration-server-type config)))
           (architecture (hetzner-server-architecture server)))
      (format #t "successfully created '~a' ~a server for '~a'\n"
              server-type architecture name)
      server)))

(define (wait-for-ssh address ssh-key)
  "Block until a SSH session can be made as 'root' with SSH-KEY at ADDRESS."
  (format #t "connecting via SSH to '~a' using '~a'...\n" address ssh-key)
  (let loop ()
    (catch #t
      (lambda ()
        (open-ssh-session address #:user "root" #:identity ssh-key
                          #:strict-host-key-check? #f))
      (lambda args
        (let ((msg (cadr args)))
          (if (formatted-message? msg)
              (format #t "~a\n"
                      (string-trim-right
                       (apply format #f
                              (formatted-message-string msg)
                              (formatted-message-arguments msg))
                       #\newline))
              (format #t "~a" args))
          (sleep 5)
          (loop))))))

(define (hetzner-machine-wait-for-ssh machine server)
  "Wait for SSH connection to be established with the specified machine."
  (wait-for-ssh (hetzner-server-public-ipv4 server)
                (hetzner-configuration-ssh-key
                 (machine-configuration machine))))

(define (hetzner-machine-authenticate-host machine server)
  "Add the host key of MACHINE to the list of known hosts."
  (let ((ssh-session (hetzner-machine-wait-for-ssh machine server)))
    (write-known-host! ssh-session)))

(define (hetzner-machine-enable-rescue-system machine server)
  "Enable the rescue system on the Hetzner SERVER for MACHINE."
  (let* ((name (machine-display-name machine))
         (config (machine-configuration machine))
         (api (hetzner-configuration-api config))
         (ssh-keys (list (hetzner-machine-ssh-key machine))))
    (format #t "enabling rescue system on '~a'...\n" name)
    (let ((action (hetzner-api-server-enable-rescue-system api server ssh-keys)))
      (format #t "successfully enabled rescue system on '~a'\n" name)
      action)))

(define (hetzner-machine-power-on machine server)
  "Power on the Hetzner SERVER for MACHINE."
  (let* ((name (machine-display-name machine))
         (config (machine-configuration machine))
         (api (hetzner-configuration-api config)))
    (format #t "powering on server for '~a'...\n" name)
    (let ((action (hetzner-api-server-power-on api server)))
      (format #t "successfully powered on server for '~a'\n" name)
      action)))

(define (hetzner-machine-ssh-run-script ssh-session name content)
  (let ((sftp-session (make-sftp-session ssh-session)))
    (rexec ssh-session (format #f "rm -f ~a" name))
    (rexec ssh-session (format #f "mkdir -p ~a" (dirname name)))
    (call-with-remote-output-file
     sftp-session name
     (lambda (port)
       (display content port)))
    (sftp-chmod sftp-session name 755)
    (let ((lines exit-code (rexec-verbose ssh-session
                                          (format #f "~a 2>&1" name))))
      (if (zero? exit-code)
          lines
          (raise-exception
           (formatted-message
            (G_ "failed to run script '~a' on machine, exit code: '~a'")
            name exit-code))))))

;; Prevent compiler from inlining this function, so we can mock it in tests.
(set! hetzner-machine-ssh-run-script hetzner-machine-ssh-run-script)

(define (hetzner-machine-rescue-install-os machine ssh-session server)
  (let ((name (machine-display-name machine))
        (os (hetzner-machine-bootstrap-os-form machine server)))
    (format #t "installing guix operating system on '~a'...\n" name)
    (hetzner-machine-ssh-run-script
     ssh-session "/tmp/guix/deploy/hetzner-machine-rescue-install-os"
     (format #f "#!/usr/bin/env bash
set -eo pipefail
mount /dev/sda1 /mnt
mkdir -p /mnt/boot/efi
mount /dev/sda15 /mnt/boot/efi

mkdir --parents /mnt/root/.ssh
chmod 700 /mnt/root/.ssh
cp /root/.ssh/authorized_keys /mnt/root/.ssh/authorized_keys
chmod 600 /mnt/root/.ssh/authorized_keys

# Small instance don't have much disk space.  Bind mount the store of the
# rescue system to the tmp directory of the new Guix system.
mkdir -p /mnt/tmp/gnu/store
mkdir -p /gnu/store
mount --bind /mnt/tmp/gnu/store /gnu/store

apt-get install guix --assume-yes
cat > /tmp/guix/deploy/hetzner-os.scm << EOF
(use-modules (gnu) (guix utils))
(use-package-modules ssh)
(use-service-modules base networking ssh)
(use-system-modules linux-initrd)
~a
EOF
guix system init --verbosity=2 /tmp/guix/deploy/hetzner-os.scm /mnt"
             (escape-backticks (format #f "~y" os))))
    (format #t "successfully installed guix operating system on '~a'\n" name)))

(define (hetzner-machine-reboot machine server)
  "Reboot the Hetzner SERVER for MACHINE."
  (let* ((name (machine-display-name machine))
         (config (machine-configuration machine))
         (api (hetzner-configuration-api config)))
    (format #t "rebooting server for '~a'...\n" name)
    (let ((action (hetzner-api-server-reboot api server)))
      (format #t "successfully rebooted server for '~a'\n" name)
      action)))

(define (hetzner-machine-rescue-partition machine ssh-session)
  "Setup the partitions of the Hetzner server for MACHINE using SSH-SESSION."
  (let* ((name (machine-display-name machine))
         (os (machine-operating-system machine))
         (root-fs-type (operating-system-root-file-system-type os)))
    (format #t "setting up partitions on '~a'...\n" name)
    (hetzner-machine-ssh-run-script
     ssh-session "/tmp/guix/deploy/hetzner-machine-rescue-partition"
     (format #f "#!/usr/bin/env bash
set -eo pipefail
growpart /dev/sda 1 || true
~a
fdisk -l /dev/sda"
             (cond
              ((equal? "btrfs" root-fs-type)
               (format #f "mkfs.btrfs -L ~a -f /dev/sda1" root-label))
              ((equal? "ext4" root-fs-type)
               (format #f "mkfs.ext4 -L ~a -F /dev/sda1" root-label))
              (else (raise-exception
                     (formatted-message
                      (G_ "unsupported root file system type '~a'")
                      root-fs-type))))))
    (format #t "successfully setup partitions on '~a'\n" name)))

(define (hetzner-machine-rescue-install-packages machine ssh-session)
  "Install packages on the Hetzner server for MACHINE using SSH-SESSION."
  (let ((name (machine-display-name machine)))
    (format #t "installing rescue system packages on '~a'...\n" name)
    (hetzner-machine-ssh-run-script
     ssh-session "/tmp/guix/deploy/hetzner-machine-rescue-install-packages"
     (format #f "#!/usr/bin/env bash
set -eo pipefail
apt-get update
apt-get install cloud-initramfs-growroot --assume-yes"))
    (format #t "successfully installed rescue system packages on '~a'\n" name)))

(define (hetzner-machine-delete machine server)
  "Delete the Hetzner server for MACHINE."
  (let* ((name (machine-display-name machine))
         (config (machine-configuration machine))
         (api (hetzner-configuration-api config)))
    (format #t "deleting server for '~a'...\n" name)
    (let ((action (hetzner-api-server-delete api server)))
      (format #t "successfully deleted server for '~a'\n" name)
      action)))

(define (hetzner-machine-provision machine)
  "Provision a server for MACHINE on the Hetzner Cloud service."
  (with-exception-handler
      (lambda (exception)
        (let ((config (machine-configuration machine))
              (server (hetzner-machine-server machine)))
          (when (and server (hetzner-configuration-delete? config))
            (hetzner-machine-delete machine server))
          (raise-exception exception)))
    (lambda ()
      (let ((server (hetzner-machine-create-server machine)))
        (hetzner-machine-enable-rescue-system machine server)
        (hetzner-machine-power-on machine server)
        (let ((ssh-session (hetzner-machine-wait-for-ssh machine server)))
          (hetzner-machine-rescue-install-packages machine ssh-session)
          (hetzner-machine-rescue-partition machine ssh-session)
          (hetzner-machine-rescue-install-os machine ssh-session server)
          (hetzner-machine-reboot machine server)
          (sleep 5)
          (hetzner-machine-authenticate-host machine server)
          server)))
    #:unwind? #t))

(define (machine-not-provisioned machine)
  (formatted-message
   (G_ "no server provisioned for machine '~a' on the Hetzner Cloud service")
   (machine-display-name machine)))


;;;
;;; Remote evaluation.
;;;

(define (hetzner-remote-eval machine exp)
  "Internal implementation of 'machine-remote-eval' for MACHINE instances with
an environment type of 'hetzner-environment-type'."
  (hetzner-machine-validate machine)
  (let ((server (hetzner-machine-server machine)))
    (unless server (raise-exception (machine-not-provisioned machine)))
    (machine-remote-eval (hetzner-machine-delegate machine server) exp)))



;;;
;;; System deployment.
;;;

(define (deploy-hetzner machine)
  "Internal implementation of 'deploy-machine' for 'machine' instances with an
environment type of 'hetzner-environment-type'."
  (hetzner-machine-validate machine)
  (unless (hetzner-machine-ssh-key machine)
    (hetzner-machine-ssh-key-create machine))
  (let ((server (or (hetzner-machine-server machine)
                    (hetzner-machine-provision machine))))
    (deploy-machine (hetzner-machine-delegate machine server))))



;;;
;;; Roll-back.
;;;

(define (roll-back-hetzner machine)
  "Internal implementation of 'roll-back-machine' for MACHINE instances with an
environment type of 'hetzner-environment-type'."
  (hetzner-machine-validate machine)
  (let ((server (hetzner-machine-server machine)))
    (unless server (raise-exception (machine-not-provisioned machine)))
    (roll-back-machine (hetzner-machine-delegate machine server))))



;;;
;;; Environment type.
;;;

(define hetzner-environment-type
  (environment-type
   (machine-remote-eval hetzner-remote-eval)
   (deploy-machine deploy-hetzner)
   (roll-back-machine roll-back-hetzner)
   (name 'hetzner-environment-type)
   (description "Provisioning of virtual machine servers on the Hetzner Cloud
service.")))
