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

;;; This file returns a manifest containing packages which should all cross-compile.
;;; We use the packages included by default in an OS configuration since that is
;;; (probably) the most likely install method for some architectures.

(use-modules (gnu packages)
             (guix packages)
             (guix profiles)
             (guix platform)
             ((gnu services xorg) #:select (%default-xorg-modules))
             ((gnu system) #:prefix gnu-system:
                           #:select (%base-packages %base-packages-linux))
             (guix utils)
             (guix gexp)
             (srfi srfi-1)
             (srfi srfi-26))

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

(define %base-packages
  ;; Packages that must be substitutable on all the platforms Guix supports.
  ;; Use the %base-packages from (gnu system) since they are included by default
  ;; in the packages field of the operating system configuration.
  (cons* (specification->package "guix")
         gnu-system:%base-packages))

(define %base-packages/hurd
  ;; Remove the packages from %base-packages-linux and some of the packages
  ;; from the other package sets.
  (fold delete %base-packages
        (append (map specification->package
                     '("e2fsprogs" "kbd" "iproute2" "iw" "wireless-tools"))
                %base-packages-linux)))

(define %system-packages
  ;; Key packages proposed by the Guix System installer.
  (map specification->package
       '("openssh" "tor" "ntp" "gpm"
         "connman" "network-manager" "wpa-supplicant" "isc-dhcp" "cups"
         "linux-libre" "grub-hybrid")))

(define %system-gui-packages
  ;; Key packages proposed by the Guix System installer.
  (append (map specification->package
               '("enlightenment"
                 ;; build system `python' does not support cross builds
                 ;"gnome" "xfce" "mate" "openbox"
                 "awesome"
                 "i3-wm" "i3status" "dmenu" "st"
                 "ratpoison" "xterm"
                 ;; build system `emacs' does not support cross builds
                 ;"emacs-exwm" "emacs-desktop-environment"
                 "emacs"))
          %default-xorg-modules))

(define %packages-to-cross-build
  ;; Packages that must be cross-buildable from x86_64-linux.
  ;; FIXME: Add (@ (gnu packages gcc) gcc) when <https://bugs.gnu.org/40463>
  ;; is fixed.
  (append (list (@ (gnu packages guile) guile-3.0/pinned))
          (map specification->package
               '("coreutils" "grep" "sed" "findutils" "diffutils" "patch"
                 "gawk" "gettext" "gzip" "xz" "zstd"
                 "hello" "zlib"))))

(define %packages-to-cross-build-for-mingw
  ;; Many things don't build for MinGW.  Restrict to what's known to work
  ;; to test that the cross-compiler itself works.
  (map specification->package '("hello")))


;;;
;;; Manifests.
;;;

;; As per the Guix Survey of 2024 we only worry about cross compiling from
;; x86_64-linux (98% of users) and not from others (aarch64 with 19%).
(define %cross-manifest
  (manifest
    (append-map (lambda (target)
                  (map (cut package->manifest-entry* <> "x86_64-linux"
                            #:target target)
                       (if (target-mingw? target)
                           %packages-to-cross-build-for-mingw
                           %packages-to-cross-build)))
                (fold delete (targets)
                      '(;; Disable cross-compilation to self:
                        "x86_64-linux-gnu"

                        ;; mips64el commonly targets a different architecture
                        ;; revision than we targeted in Guix.
                        "mips64el-linux-gnu"

                        ;; Ignore bare-metal targets.
                        "avr"
                        "or1k-elf"
                        "xtensa-ath9k-elf")))))

(define %cross-system
  (manifest
    (cons*
      ;; Include a couple of extra kernels that are commonly used:
      (package->manifest-entry* (@ (gnu packages linux)
                                   linux-libre-arm64-generic)
                                "x86_64-linux"
                                #:target "aarch64-linux-gnu")
      (package->manifest-entry* (@ (gnu packages linux)
                                   linux-libre-arm-generic)
                                "x86_64-linux"
                                #:target "arm-linux-gnueabihf")
      (package->manifest-entry* (@ (gnu packages linux)
                                   linux-libre-riscv64-generic)
                                "x86_64-linux"
                                #:target "riscv64-linux-gnu")
      (append-map (lambda (target)
                    (map (cut package->manifest-entry* <> "x86_64-linux"
                              #:target target)
                         (append %base-packages
                                 %system-packages
                                 ;; With a graphical environment:
                                 (if (or (target-x86-32? target)
                                         (target-aarch64? target))
                                     %system-gui-packages
                                     '()))))
                  (fold delete (map platform-system->target (systems))
                        '(;; Disable cross-compilation to self:
                          "x86_64-linux-gnu"

                          ;; Ignore obsolete systems, as in (gnu ci).
                          "mips64el-linux-gnu"
                          "powerpc-linux-gnu"))))))

;; Return the union of all the manifests.
(concatenate-manifests (list %cross-manifest
                             %cross-system))
