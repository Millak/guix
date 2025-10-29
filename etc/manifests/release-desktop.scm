;;; GNU Guix --- Functional package management for GNU
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
             (gnu packages)
             ((gnu services xorg) #:select (%default-xorg-modules))
             (guix profiles)
             (guix utils)
             (srfi srfi-1))

(define %desktop-packages
  (append
   %default-xorg-modules
   (map specification->package
        (list
         ;; etc-service
         "net-base"
         "tzdata"
         ;; boot-file-system-utilities-service
         "e2fsprogs"
         ;; plasma-deskto-sservice-type
         "plasma"
         ;; gnome-desktop-service
         "gnome"
         ;; xfce-desktop-service
         "xfce"
         ;; mate-desktop-service
         "mate"
         ;; set-xorg-configuration-service
         "font-alias"
         "font-misc-misc"
         "font-adobe75dpi"
         "xorg-server"
         ;; screen-locker-service
         "slock"
         "xlockmore"
         ;; mtp-service
         "libmtp"
         ;; sane-service
         "sane"
         "sane-backends"
         ;; mount-setuid-helpers-service
         "nfs-utils"
         "ntfs-3g"
         ;; guix-artwork-service
         "guix-backgrounds"
         "guix-icons"
         ;; vte-integration-service
         "vte"
         ;; network-manager-applet-service
         "network-manager-applet"
         ;; modem-manager-service
         "modem-manager"
         ;; usb-modeswitch-service
         "usb-modeswitch"
         "usb-modeswitch-data"
         ;; avahi-service
         "avahi"
         ;; udisks-service
         "udisks"
         ;; cups-pk-helper-service
         "cups-pk-helper"
         ;; colord-service
         "colord"
         ;; geoclue-service
         "geoclue"
         ;; polkit-service
         "polkit"
         ;; elogind-service
         "elogind"
         ;; dbus-service
         ;; ntp-service
         "ntp"
         ;; alsa-service
         "alsa-plugins"
         ;; mingetty-service
         "mingetty"
         ;; etc-bashrc-d-service
         "bash-completion"
         ;; udev-service
         "eudev"
         "lvm2"
         "alsa-utils"
         "crda"
         ;; sysctl-service
         "procps"
         ;; special-files-service
         "coreutils"
         "grub-pc"
         "less"
         "mg"
         "nano"
         "nvi"
         "man-db"
         "info-reader"
         "kbd"
         "guile-readline"
         "guile-colorized"
         "pciutils"
         "usbutils"
         "util-linux-with-udev"
         "kmod"
         "isc-dhcp"
         "iproute2"
         "wget"
         "nss-certs"
         "iw"
         "wireless-tools"
         "psmisc"
         "which"
         "guile"
         "findutils"
         "grep"
         "sed"
         "diffutils"
         "patch"
         "gawk"
         "tar"
         "gzip"
         "bzip2"
         "lzip"
         "xz"
         "zstd"
         ;; packages
         "sway"
         "foot"
         "wmenu"
         "icewm"
         "openbox"
         "awesome"
         "i3-wm"
         "i3status"
         "dmenu"
         "st"
         "ratpoison"
         "xterm"
         "emacs"
         "emacs-exwm"
         "emacs-desktop-environment"))))

(define %desktop-manifest
  (manifest
   ;; Some of %SYSTEM-PACKAGES are currently unsupported on some
   ;; systems--e.g., GNOME on 32-bit, due to Rust.  Filter them out.
   (filter-map (lambda (package)
                 (and (supported-package? package (%current-system))
                      (package->manifest-entry package)))
               (append %desktop-packages))))

%desktop-manifest
