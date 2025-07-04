;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020-2022, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020 Jan (janneke) Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2025 Efraim Flashner <efraim@flashner.co.il>
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

;;; This file returns a manifest containing packages which are needed by the
;;; installer.

(use-modules (guix packages)
             ((gnu services xorg) #:select (%default-xorg-modules))
             (guix utils)
             (srfi srfi-1))

(define* (package->manifest-entry* package system
                                   #:key target)
  "Return a manifest entry for PACKAGE on SYSTEM, optionally cross-compiled to
TARGET."
  (manifest-entry
    (inherit (package->manifest-entry package))
    (name (string-append (package-name package) "." system
                         (if target
                             (string-append "." target)
                             "'")))
    (item (with-parameters ((%current-system system)
                            (%current-target-system target))
            package))))

(define %system-packages
  ;; Key packages proposed by the Guix System installer.
  (append (map specification->package
               '("guix" "shepherd"
                 "gnome" "xfce" "mate"
                 "icewm" "openbox" "awesome"
                 "i3-wm" "i3status" "dmenu" "st"
                 "ratpoison" "xterm"
                 "emacs" "emacs-exwm" "emacs-desktop-environment"
                 "openssh" "tor" "ntp" "gpm"
                 "connman" "network-manager" "wpa-supplicant" "isc-dhcp" "cups"
                 "linux-libre" "grub-hybrid"))
          %default-xorg-modules))

(define %bootloader-packages
  ;; The bootloaders offered by the Guix System installer.
  (append
    (map specification->package
         '("grub-pc" "grub-minimal" "grub-efi"))
    ;; Add all the u-boot packages.
    ;; TODO: Filter by target.
    (if (or (target-arm32?)
            (target-aarch64?)
            (target-riscv64?))
        `(,@(fold-packages
              (lambda (package lst)
                (if (string-prefix? "u-boot-"
                                    (package-name package))
                    (cons package lst)
                    lst))
              (list)))
        '())))

(define %filesystem-packages
  ;; The installer offers to create filesystems which are then needed.
  ;; See also: (gnu system linux-initrd)
  (cons* (@ (gnu packages linux) e2fsck/static)
         (@ (gnu packages disk) fatfsck/static)
         (@ (gnu packages file-systems) bcachefs/static)
         (@ (gnu packages linux) btrfs-progs/static)
         (@ (gnu packages file-systems) jfs_fsck/static)
         (@ (gnu packages linux) ntfsfix/static)
         (@ (gnu packages linux) f2fs-fsck/static)
         (@ (gnu packages linux) xfs_repair/static)
         (map specification->package
              '("lvm2-static"
                "cryptsetup-static"
                "mdadm-static"))))


;;;
;;; Manifests.
;;;

(define %system-manifest
  (manifest
    (append
      ;; The linux-libre-*-generic kernel is commonly used on some architectures.
      (cond
        ((target-aarch64?)
         (list (package->manifest-entry (@ (gnu packages linux)
                                           linux-libre-arm64-generic))))
        ((target-riscv64?)
         (list (package->manifest-entry (@ (gnu packages linux)
                                           linux-libre-riscv64-generic))))
        ((target-arm32?)
         (list (package->manifest-entry (@ (gnu packages linux)
                                           linux-libre-arm-generic))))
        (else '()))

      ;; Some of %SYSTEM-PACKAGES are currently unsupported on some
      ;; systems--e.g., GNOME on 32-bit, due to Rust.  Filter them out.
      (filter-map (lambda (package)
                    (and (supported-package? package (%current-system))
                         (package->manifest-entry package)))
                  (append %system-packages
                          %bootloader-packages
                          %filesystem-packages)))))

%system-manifest
