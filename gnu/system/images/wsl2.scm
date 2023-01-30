;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Alex Griffin <a@ajgrf.com>
;;; Copyright © 2022 Mathieu Othacehe <othacehe@gnu.org>
;;; Copyright © 2022 dan <i@dan.games>
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

(define-module (gnu system images wsl2)
  #:use-module (gnu bootloader)
  #:use-module (gnu image)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu services)
  #:use-module (gnu services base)
  #:use-module (gnu system)
  #:use-module (gnu system image)
  #:use-module (gnu system shadow)
  #:use-module (guix build-system trivial)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (fsdg-compatible))
  #:export (wsl-boot-program
            wsl-os
            wsl2-image))

(define (wsl-boot-program user)
  "Program that runs the system boot script, then starts a login shell as
USER."
  (program-file
   "wsl-boot-program"
   (with-imported-modules '((guix build syscalls))
     #~(begin
         (use-modules (guix build syscalls))
         (unless (file-exists? "/run/current-system")
           (let ((shepherd-socket "/var/run/shepherd/socket"))
             ;; Clean up this file so we can wait for it later.
             (when (file-exists? shepherd-socket)
               (delete-file shepherd-socket))

             ;; Child process boots the system and is replaced by shepherd.
             (when (zero? (primitive-fork))
               (let* ((system-generation
                       (readlink "/var/guix/profiles/system"))
                      (system (readlink
                               (string-append
                                (if (absolute-file-name? system-generation)
                                    ""
                                    "/var/guix/profiles/")
                                system-generation))))
                 (setenv "GUIX_NEW_SYSTEM" system)
                 (execl #$(file-append guile-3.0 "/bin/guile")
                        "guile"
                        "--no-auto-compile"
                        (string-append system "/boot"))))

             ;; Parent process waits for shepherd before continuing.
             (while (not (file-exists? shepherd-socket))
               (sleep 1))))

         (let* ((pw (getpw #$user))
                (shell (passwd:shell pw))
                (sudo #+(file-append sudo "/bin/sudo"))
                (args (cdr (command-line)))
                (uid (passwd:uid pw))
                (gid (passwd:gid pw))
                (runtime-dir (string-append "/run/user/"
                                            (number->string uid))))
           ;; Save the value of $PATH set by WSL.  Useful for finding
           ;; Windows binaries to run with WSL's binfmt interop.
           (setenv "WSLPATH" (getenv "PATH"))

           ;; /run is mounted with the nosuid flag by WSL.  This prevents
           ;; running the /run/setuid-programs.  Remount it without this flag
           ;; as a workaround.  See:
           ;; https://github.com/microsoft/WSL/issues/8716.
           (mount #f "/run" #f
                  MS_REMOUNT
                  #:update-mtab? #f)

           ;; Create XDG_RUNTIME_DIR for the login user.
           (unless (file-exists? runtime-dir)
             (mkdir runtime-dir)
             (chown runtime-dir uid gid))
           (setenv "XDG_RUNTIME_DIR" runtime-dir)

           ;; Start login shell as user.
           (apply execl sudo "sudo"
                  "--preserve-env=WSLPATH,XDG_RUNTIME_DIR"
                  "-u" #$user
                  "--"
                  shell "-l" args))))))

(define dummy-package
  (package
    (name "dummy")
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:target #f
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((out (assoc-ref %outputs "out"))
                          (dummy (string-append out "/dummy")))
                     (mkdir-p out)
                     (call-with-output-file dummy
                       (const #t))))))
    (home-page #f)
    (synopsis #f)
    (description #f)
    (license (fsdg-compatible "dummy"))))

(define dummy-bootloader
  (bootloader
   (name 'dummy-bootloader)
   (package dummy-package)
   (configuration-file "/dev/null")
   (configuration-file-generator
    (lambda (. _rest)
      (plain-file "dummy-bootloader" "")))
   (installer #~(const #t))))

(define dummy-kernel dummy-package)

(define (dummy-initrd . _rest)
  (plain-file "dummy-initrd" ""))

(define-public wsl-os
  (operating-system
    (host-name "gnu")
    (timezone "Etc/UTC")
    (bootloader
     (bootloader-configuration
      (bootloader dummy-bootloader)))
    (kernel dummy-kernel)
    (initrd dummy-initrd)
    (initrd-modules '())
    (firmware '())
    (file-systems '())
    (users (cons* (user-account
                   (name "guest")
                   (group "users")
                   (supplementary-groups '("wheel")) ; allow use of sudo
                   (password "")
                   (comment "Guest of GNU"))
                  (user-account
                   (inherit %root-account)
                   (shell (wsl-boot-program "guest")))
                  %base-user-accounts))
    (services
     (list
      (service guix-service-type)
      (service special-files-service-type
               `(("/bin/sh" ,(file-append bash "/bin/bash"))
                 ("/bin/mount" ,(file-append util-linux "/bin/mount"))
                 ("/usr/bin/env" ,(file-append coreutils "/bin/env"))))))))

(define wsl2-image
  (image
   (inherit
    (os->image wsl-os
               #:type wsl2-image-type))
   (name 'wsl2-image)))

wsl2-image
