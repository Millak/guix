;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2022, 2024-2025 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2017 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2017, 2019 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020, 2024 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2022 Josselin Poiret <dev@jpoiret.xyz>
;;; Copyright © 2023 Herman Rimm <herman@rimm.ee>
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

(define-module (gnu system install)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (guix build-system trivial)
  #:use-module (gnu bootloader)
  #:use-module (gnu bootloader u-boot)
  #:use-module (guix gexp)
  #:use-module (guix store)
  #:use-module (guix modules)
  #:use-module ((guix packages) #:select (package-version supported-package?))
  #:autoload   (guix channels) (channel? channel-commit)
  #:use-module (guix platform)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module ((guix channels)
                #:select (%default-guix-channel
                          channel
                          channel-commit))
  #:use-module (gnu installer)
  #:use-module (gnu system locale)
  #:use-module (gnu services avahi)
  #:use-module (gnu services dbus)
  #:use-module (gnu services networking)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services ssh)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bootloaders)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cryptsetup)
  #:use-module (gnu packages disk)
  #:use-module (gnu packages file-systems)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match)
  #:export (installation-os
            make-installation-os
            a20-olinuxino-lime-installation-os
            a20-olinuxino-lime2-emmc-installation-os
            a20-olinuxino-micro-installation-os
            bananapi-m2-ultra-installation-os
            am335x-evm-installation-os
            beaglebone-black-installation-os
            mx6cuboxi-installation-os
            nintendo-nes-classic-edition-installation-os
            novena-installation-os
            orangepi-r1-plus-lts-rk3328-installation-os
            firefly-rk3399-installation-os
            pine64-plus-installation-os
            pinebook-installation-os
            rock64-installation-os
            rock-4c-plus-installation-os
            rockpro64-installation-os
            rk3399-puma-installation-os
            wandboard-installation-os
            os-with-u-boot))

;;; Commentary:
;;;
;;; This module provides an 'operating-system' definition for use on images
;;; for USB sticks etc., for the installation of the GNU system.
;;;
;;; Code:


;;;
;;; Documentation service.
;;;

(define %installation-node-names
  ;; Translated name of the "System Installation" node of the manual.  Ideally
  ;; we'd extract it from the 'guix-manual' gettext domain, but that one is
  ;; usually not available at run time, hence this hack.
  '(("de" . "Systeminstallation")
    ("en" . "System Installation")
    ("es" . "Instalación del sistema")
    ("fr" . "Installation du système")
    ("it" . "Installazione del Sistema")
    ("pt_BR" . "Instalação do sistema")
    ("ru" . "Установка системы")
    ("zh_CN" . "系统安装")))

(define (log-to-info tty user)
  "Return a script that spawns the Info reader on the right section of the
manual."
  (program-file "log-to-info"
                #~(let* ((tty      (open-file #$(string-append "/dev/" tty)
                                              "r0+"))
                         (locale   (cadr (command-line)))
                         (language (string-take locale
                                                (string-index locale #\_)))
                         (with-region (string-take locale
                                                   (string-index
                                                    locale
                                                    (char-set #\. #\/ #\@))))
                         (infodir  "/run/current-system/profile/share/info")
                         (per-lang (lambda (code)
                                     (string-append infodir "/guix." code
                                                    ".info.gz")))
                         (file ((@ (srfi srfi-1) find) file-exists?
                                (list (per-lang with-region)
                                      (per-lang language)
                                      (string-append infodir
                                                     "/guix.info.gz"))))
                         (node     (or (assoc-ref '#$%installation-node-names
                                                  with-region)
                                       (assoc-ref '#$%installation-node-names
                                                  language)
                                       "System Installation")))
                    (redirect-port tty (current-output-port))
                    (redirect-port tty (current-error-port))
                    (redirect-port tty (current-input-port))

                    (let ((pw (getpwnam #$user)))
                      (setgid (passwd:gid pw))
                      (setuid (passwd:uid pw)))

                    ;; 'gunzip' is needed to decompress the doc.
                    (setenv "PATH" (string-append #$gzip "/bin"))

                    ;; Change this process' locale so that command-line
                    ;; arguments to 'info' are properly encoded.
                    (catch #t
                      (lambda ()
                        (setlocale LC_ALL locale)
                        (setenv "LC_ALL" locale))
                      (lambda _
                        ;; Sometimes LOCALE itself is not available.  In that
                        ;; case pick the one UTF-8 locale that's known to work
                        ;; instead of failing.
                        (setlocale LC_ALL "en_US.utf8")
                        (setenv "LC_ALL" "en_US.utf8")))

                    (execl #$(file-append info-reader "/bin/info")
                           "info" "-d" infodir "-f" file "-n" node))))

(define (documentation-shepherd-service tty)
  (list (shepherd-service
         (provision (list (symbol-append 'term- (string->symbol tty))))
         (requirement '(user-processes host-name udev virtual-terminal))
         (start #~(lambda* (#:optional (locale "en_US.utf8"))
                    (fork+exec-command
                     (list #$(log-to-info tty "documentation") locale)
                     #:environment-variables
                     `("GUIX_LOCPATH=/run/current-system/locale"
                       "TERM=linux"))))
         (stop #~(make-kill-destructor)))))

(define %documentation-users
  ;; User account for the Info viewer.
  (list (user-account (name "documentation")
                      (system? #t)
                      (group "nogroup")
                      (home-directory "/var/empty"))))

(define documentation-service-type
  ;; Documentation viewer service.
  (service-type (name 'documentation)
                (extensions
                 (list (service-extension shepherd-root-service-type
                                          documentation-shepherd-service)
                       (service-extension account-service-type
                                          (const %documentation-users))))
                (description "Run the Info reader on a tty.")))


(define %backing-directory
  ;; Sub-directory used as the backing store for copy-on-write.
  "/tmp/guix-inst")

(define cow-store-service-type
  (shepherd-service-type
   'cow-store
   (lambda _
     (define (import-module? module)
       ;; Since we don't use deduplication support in 'populate-store', don't
       ;; import (guix store deduplication) and its dependencies, which
       ;; includes Guile-Gcrypt.
       (and (guix-module-name? module)
            (not (equal? module '(guix store deduplication)))))

     (shepherd-service
      (requirement '(root-file-system user-processes))
      (provision '(cow-store))
      (documentation
       "Make the store copy-on-write, with writes going to \
the given target.")

      ;; This is meant to be explicitly started by the user.
      (auto-start? #f)

      (modules `((gnu build install)
                 ,@%default-modules))
      (start
       (with-imported-modules (source-module-closure
                               '((gnu build install))
                               #:select? import-module?)
         #~(case-lambda
             ((target)
              (mount-cow-store target #$%backing-directory)
              target)
             (else
              ;; Do nothing, and mark the service as stopped.
              #f))))
      (stop #~(lambda (target)
                ;; Delete the temporary directory, but leave everything
                ;; mounted as there may still be processes using it since
                ;; 'user-processes' doesn't depend on us.  The 'user-file-systems'
                ;; service will unmount TARGET eventually.
                (delete-file-recursively
                 (string-append target #$%backing-directory))))))
   (description "Make the store copy-on-write, with writes going to \
the given target.")))

(define (cow-store-service)
  "Return a service that makes the store copy-on-write, such that writes go to
the user's target storage device rather than on the RAM disk."
  ;; See <http://bugs.gnu.org/18061> for the initial report.
  (service cow-store-service-type 'mooooh!))


(define (/etc/configuration-files _)
  "Return a list of tuples representing configuration templates to add to
/etc."
  (define directory
    (computed-file "configuration-templates"
                   (with-imported-modules '((guix build utils))
                     #~(begin
                         (mkdir #$output)
                         (for-each (lambda (file target)
                                     (copy-file file
                                                (string-append #$output "/"
                                                               target)))
                                   '(#$(local-file "examples/bare-bones.tmpl")
                                     #$(local-file "examples/beaglebone-black.tmpl")
                                     #$(local-file "examples/desktop.tmpl")
                                     #$(local-file "examples/lightweight-desktop.tmpl"))
                                   '("bare-bones.scm"
                                     "beaglebone-black.scm"
                                     "desktop.scm"
                                     "lightweight-desktop.scm"))
                         #t))))

  `(("configuration" ,directory)))

(define configuration-template-service-type
  (service-type (name 'configuration-template)
                (extensions
                 (list (service-extension etc-service-type
                                          /etc/configuration-files)))
                (description "Install the operating system configuration file
templates under @file{/etc/configuration}.")))

(define %configuration-template-service
  (service configuration-template-service-type #t))


(define %nscd-minimal-caches
  ;; Minimal in-memory caching policy for nscd.
  (list (nscd-cache (database 'hosts)
                    (positive-time-to-live (* 3600 12))

                    ;; Do not cache lookup failures at all since they are
                    ;; quite likely (for instance when someone tries to ping a
                    ;; host before networking is functional.)
                    (negative-time-to-live 0)

                    (persistent? #f)
                    (max-database-size (* 5 (expt 2 20)))))) ;5 MiB


;; These define a service to load the uvesafb kernel module with the
;; appropriate options.  The GUI installer needs it when the machine does not
;; support Kernel Mode Setting.  Otherwise kmscon is missing /dev/fb0.
(define (uvesafb-shepherd-service _)
  (define modprobe
    (program-file "modprobe-wrapper"
                  #~(begin
                      ;; Use a wrapper because shepherd 0.9.3 won't let us
                      ;; pass environment variables to the child process:
                      ;; <https://issues.guix.gnu.org/60106>.
                      (setenv "LINUX_MODULE_DIRECTORY"
                              "/run/booted-system/kernel/lib/modules")
                      (apply execl #$(file-append kmod "/bin/modprobe")
                             "modprobe" (cdr (command-line))))))

  (list (shepherd-service
         (documentation "Load the uvesafb kernel module if needed.")
         (provision '(maybe-uvesafb))
         (requirement '(file-systems))
         (start #~(lambda ()
                    (or (file-exists? "/dev/fb0")
                        (invoke #+modprobe
                                "uvesafb"
                                (string-append "v86d=" #$v86d "/sbin/v86d")
                                "mode_option=1024x768"))))
         (respawn? #f)
         (one-shot? #t))))

(define uvesafb-service-type
  (service-type
   (name 'uvesafb)
   (extensions
    (list (service-extension shepherd-root-service-type
                             uvesafb-shepherd-service)))
   (description
    "Load the @code{uvesafb} kernel module with the right options.")
   (default-value #t)))

(define %installation-login-pam-service
;;    Custom 'login' pam.d rule. It is based on the original one,
;; but includes possibility to print different motd. This motd
;; is useful for headless consoles.
  (let* ((unix (pam-entry
                 (control "required")
                 (module "pam_unix.so")))
         (env  (pam-entry ; to honor /etc/environment.
                 (control "required")
                 (module "pam_env.so")))
         (motd (plain-file "motd" "
\x1b[1;37mWelcome to the installation of GNU Guix!\x1b[0m

\x1b[2m\
Using this shell, you can carry out the installation process \"manually.\"
Access documentation at any time by pressing Alt-F2.\x1b[0m
"))
         (console-motd (plain-file "console-motd" "
\x1b[1;37mWelcome to the installation of GNU Guix!\x1b[0m

You are in a headless console. If you can use a display, you should see the
graphical installer on TTY1, it's opened by default. In case you cannot use a
display, you can carry out the installation process in this shell \"manually\"
or by starting the installer, using `guix-system-installer` command.

\x1b[2mYou can access the Guix documentation using `info \"(guix)\"`.\x1b[0m
"))
         (show-motd (program-file "show-motd"
                                  #~(begin
                                      (use-modules (ice-9 textual-ports))

                                      (call-with-input-file
                                          (if (file-exists?
                                               (string-append
                                                "/tmp/console_"
                                                (basename (getenv "PAM_TTY"))))
                                              #$console-motd
                                              #$motd)
                                        (lambda (port)
                                          (display (get-string-all port))))))))
    (pam-service
      (name "login")
      (account (list unix))
      (auth (list (pam-entry
                    (control "required")
                    (module "pam_unix.so")
                    (arguments '("nullok")))))
      (password (list (pam-entry
                        (control "required")
                        (module "pam_unix.so")
                        (arguments '("sha512" "shadow")))))
      (session `(,(pam-entry
                    (control "optional")
                    (module "pam_exec.so")
                    (arguments
                     (list "type=open_session" "stdout" show-motd)))
                 ,(pam-entry       ;to fill in /proc/self/loginuid
                    (control "required")
                    (module "pam_loginuid.so"))
                 ,env ,unix)))))

(define %installation-console-login
;;    Make a custom 'login' wrapper for execution in console
;; terminal. It instructs pam.d login rule to print different
;; motd.
  (program-file
   "login-with-motd"
   #~(begin
       (call-with-output-file
           (string-append "/tmp/console_"
                          (basename (ttyname (current-output-port))))
         (const #t))

       (apply execlp
              #$(file-append shadow "/bin/login")
              (command-line)))))

(define* (installer-command-package installer-program
                                    #:key executable)
  (package
    (name executable)
    (version "0")
    (source #f)
    (build-system trivial-build-system)
    (arguments
     (list #:builder (with-imported-modules '((guix build utils))
                       #~(begin
                           (use-modules (guix build utils))

                           (mkdir-p (string-append #$output "/bin"))

                           (symlink #$installer-program
                                    (string-append #$output "/bin/" #$executable))))))
    (home-page #f)
    (synopsis "Provides the Guix System installer as run-guix-installer command")
    (description "Provides the Guix System installer as run-guix-installer command.")
    (license license:gpl3+)))

(define* (%installation-services
          #:key
          (system (or (and=>
                       (%current-target-system)
                       platform-target->system)
                      (%current-system)))
          (guix-for-system (current-guix)))
  ;; List of services of the installation system.
  (define (normal-tty tty)
    (service mingetty-service-type
             (mingetty-configuration (tty tty)
                                     (auto-login "root")
                                     (login-pause? #t))))

  (define bare-bones-os
    (load "examples/bare-bones.tmpl"))

  (define installer
    (installer-program
     #:guix-for-installer guix-for-system))

  (append
   ;; Generic services
   (list (service virtual-terminal-service-type)

         (service kmscon-service-type
                  (kmscon-configuration
                    (virtual-terminal "tty1")
                    (login-program installer)))

         (simple-service 'installer-login
                         pam-root-service-type
                         (list %installation-login-pam-service))

         ;; Documentation.  The manual is in UTF-8, but
         ;; 'console-font-service' sets up Unicode support and loads a font
         ;; with all the useful glyphs like em dash and quotation marks.
         (service documentation-service-type "tty2")

         ;; Documentation add-on.
         %configuration-template-service

         ;; A bunch of 'root' ttys.
         (normal-tty "tty3")
         (normal-tty "tty4")
         (normal-tty "tty5")
         (normal-tty "tty6")

         ;; The usual services.
         (service shepherd-system-log-service-type)

         ;; Use the Avahi daemon to discover substitute servers on the local
         ;; network.  It can be faster than fetching from remote servers.
         (service avahi-service-type)

         ;; Allow runninng the installer command on headless setups.
         (simple-service 'installer-program-profile
                         profile-service-type
                         (list (installer-command-package
                                installer
                                #:executable "guix-system-installer")))

         ;; The build daemon.
         (service guix-service-type
                  (guix-configuration
                    ;; Register the default substitute server key(s) as
                    ;; trusted to allow the installation process to use
                    ;; substitutes by default.
                    (authorize-key? #t)

                    ;; Install and run the current Guix rather than an older
                    ;; snapshot.
                    (guix guix-for-system)))

         ;; Start udev so that useful device nodes are available.
         ;; Use device-mapper rules for cryptsetup & co; enable the CRDA for
         ;; regulations-compliant WiFi access.
         (service udev-service-type
                  (udev-configuration
                    (rules (list lvm2 crda))))

         ;; Add the 'cow-store' service, which users have to start manually
         ;; since it takes the installation directory as an argument.
         (cow-store-service)

         ;; Install Unicode support and a suitable font.
         (service console-font-service-type
                  (map (match-lambda
                         ("tty2"
                          ;; Use a font that contains characters such as
                          ;; curly quotes as found in the manual.
                          '("tty2" . "LatGrkCyr-8x16"))
                         (tty
                          ;; Use a font that doesn't have more than 256
                          ;; glyphs so that we can use colors with varying
                          ;; brightness levels (see note in setfont(8)).
                          `(,tty . "lat9u-16")))
                       '("tty1" "tty2" "tty3" "tty4" "tty5" "tty6")))

         ;; To facilitate copy/paste.
         (service gpm-service-type)

         ;; Add an SSH server to facilitate remote installs.
         (service openssh-service-type
                  (openssh-configuration
                    (port-number 22)
                    (permit-root-login #t)
                    ;; The root account is passwordless, so make sure
                    ;; a password is set before allowing logins.
                    (allow-empty-passwords? #f)
                    (password-authentication? #t)

                    ;; Don't start it upfront.
                    (%auto-start? #f)))

         ;; Since this is running on a USB stick with a overlayfs as the root
         ;; file system, use an appropriate cache configuration.
         (service nscd-service-type
                  (nscd-configuration
                    (caches %nscd-minimal-caches)))

         ;; Having /bin/sh is a good idea.  In particular it allows Tramp
         ;; connections to this system to work.
         (service special-files-service-type
                  `(("/bin/sh" ,(file-append bash "/bin/sh"))))

         ;; Loopback device, needed by OpenSSH notably.
         (service static-networking-service-type
                  (list %loopback-static-networking))

         (service wpa-supplicant-service-type)
         (service dbus-root-service-type)
         (service connman-service-type
                  (connman-configuration
                    (disable-vpn? #t)))

         ;; Keep a reference to BARE-BONES-OS to make sure it can be
         ;; installed without downloading/building anything.  Also keep the
         ;; things needed by 'profile-derivation' to minimize the amount of
         ;; download.
         (service gc-root-service-type
                  (append
                   (list bare-bones-os
                         (libc-utf8-locales-for-target system)
                         texinfo
                         guile-3.0)
                   %default-locale-libcs)))

   ;; Specific system services

   ;; AArch64 has a better detection of consoles, mainly because device
   ;; trees are utilized.  On x86_64, the detection is usually done
   ;; through BIOS and consoles do not get registered to /proc/console.
   ;; The only way they would is if the user used console linux argument.
   `(,@(if (target-aarch64? system)
           (list (service agetty-service-type
                          (agetty-configuration (tty #f)
                                                (auto-login "root")
                                                (login-pause? #t)
                                                (login-program
                                                 %installation-console-login))))
           '()))

   ;; Machines without Kernel Mode Setting (those with many old and
   ;; current AMD GPUs, SiS GPUs, ...) need uvesafb to show the GUI
   ;; installer.  Some may also need a kernel parameter like nomodeset
   ;; or vga=793, but we leave that for the user to specify in GRUB.
   `(,@(if (supported-package? v86d system)
           (list (service uvesafb-service-type))
           '()))))

(define %issue
  ;; Greeting.
  "
\x1b[1;37mThis is an installation image of the GNU system.  Welcome.\x1b[0m

\x1b[1;33mUse Alt-F2 for documentation.\x1b[0m
")

(define %installer-disk-utilities
  ;; A well-rounded set of packages for interacting with disks, partitions and
  ;; file systems, included with the Guix installation image.
  (list parted gptfdisk ddrescue
        ;; Use the static LVM2 because it's already pulled in by the installer.
        lvm2-static
        ;; We used to provide fdisk from GNU fdisk, but as of version 2.0.0a
        ;; it pulls Guile 1.8, which takes unreasonable space; furthermore
        ;; util-linux's fdisk is already available, in %base-packages-linux.
        cryptsetup mdadm
        dosfstools
        btrfs-progs
        e2fsprogs
        f2fs-tools
        jfsutils
        xfsprogs))

(define* (%installation-initrd-modules
          #:key
          (system (or (and=>
                       (%current-target-system)
                       platform-target->system)
                      (%current-system))))
  ;; AArch64 currently lacks a lot of modules necessary
  ;; for booting from USB sticks, hard disks or
  ;; CDROMs. Those are built-in in x86_64 kernel.
  `(,@(if (target-aarch64? system)
          '("sr_mod" "sd_mod"
            "usb_common" "usbcore"
            ;; USB 3.0
            "xhci_pci" "xhci_hcd"
            "xhci_plat_hcd"
            ;; More USB controllers
            "dwc3" "dwc3_pci"
            ;; Allwinner
            "phy_sun4i_usb" "phy_sun50i_usb3"
            ;; Qualcomm
            "dwc3_qcom"
            "phy_qcom_qmp_usb" "phy_qcom_qusb2"
            "phy_qcom_snps_femto_v2" "phy_qcom_usb_hs"
            "phy_qcom_usb_ss"
            ;; Texas Instruments
            "phy_tusb1210"
            ;; Rockchip
            "phy_rockchip_usb"
            "phy_rockchip_inno_usb2" "phy_rockchip_typec"
            ;; NXP i.MX
            "phy_fsl_imx8mq_usb"
            ;; Generic
            "phy_generic"
            ;; "thunderbolt" "typec_thunderbolt"
            ;; USB 2.0
            "dwc2"
            "ehci_pci" "ehci_hcd"
            "ehci_platform"
            ;; USB 1.1
            "ohci_pci" "ohci_hcd"
            ;; SD cards
            "mmc_hsq" "mmc_spi"
            "mmc_core" "mmc_block"
            "sdhci" "sdhci_pci"
            "mmc_spi"
            "sdhci_acpi"
            ;; Platform specific SD card
            "dw_mmc" "dw_mmc-pltfm" "dw_mmc-rockchip"
            "sunxi_mmc" "sdhci_pltfm" "sdhci_msm"
            "sdhci_of_arasan" "sdhci_of_esdhc" "sdhci_brcmstb"
            "sdhci_tegra" "phy_rockchip_emmc"
            "sdhci_esdhc_imx")
          '())
    ,@%base-initrd-modules))

(define* (make-installation-os #:key
                               ;; Version displayed in the GRUB entry name.
                               (grub-displayed-version
                                (package-version guix))
                               ;; Whether to use efi-only installation.
                               ;; When #f, use hybrid grub that sets up
                               ;; both legacy boot and efi.
                               (efi-only? #f))
  ;; The operating system used on installation images for USB sticks etc.
  (operating-system
    (host-name "gnu")
    (timezone "Europe/Paris")
    (locale "en_US.utf8")
    (name-service-switch %mdns-host-lookup-nss)

    (initrd-modules (%installation-initrd-modules))

    (bootloader (if efi-only?
                    (bootloader-configuration
                      (bootloader grub-efi-bootloader)
                      (targets '("/boot/efi")))
                    (bootloader-configuration
                      (bootloader grub-bootloader)
                      (targets '("/dev/sda")))))
    (label (string-append "GNU Guix installation " grub-displayed-version))

    ;; XXX: The AMD Radeon driver is reportedly broken, which makes kmscon
    ;; non-functional:
    ;; <https://lists.gnu.org/archive/html/guix-devel/2019-03/msg00441.html>.
    ;; Thus, blacklist it.
    (kernel-arguments '("quiet" "modprobe.blacklist=radeon,amdgpu"))

    (file-systems
     ;; Note: the disk image build code overrides this root file system with
     ;; the appropriate one.
     (append %base-live-file-systems

             ;; XXX: This should be %BASE-FILE-SYSTEMS but we don't need
             ;; elogind's cgroup file systems.
             (list %pseudo-terminal-file-system
                   %shared-memory-file-system
                   %efivars-file-system
                   %immutable-store)))

    (users (list (user-account
                   (name "guest")
                   (group "users")
                   (supplementary-groups '("wheel")) ; allow use of sudo
                   (password "")
                   (comment "Guest of GNU"))))

    (issue %issue)
    (services (%installation-services))

    ;; We don't need setuid programs, except for 'passwd', which can be handy
    ;; if one is to allow remote SSH login to the machine being installed.
    (privileged-programs (list (privileged-program
                                 (program (file-append shadow "/bin/passwd"))
                                 (setuid? #t))))

    (pam-services
     ;; Explicitly allow for empty passwords.
     (base-pam-services #:allow-empty-passwords? #t))

    (packages (append
               (list glibc             ; for 'tzselect' & co.
                     fontconfig
                     font-dejavu font-gnu-unifont

                     ;; Mostly so xrefs to its manual work.
                     (bootloader-package
                      (bootloader-configuration-bootloader bootloader)))
               %installer-disk-utilities
               %base-packages))))

(define installation-os (make-installation-os))

(define* (os-with-u-boot os board #:key (bootloader-target "/dev/mmcblk0")
                         (triplet "arm-linux-gnueabihf"))
  "Given OS, amend it with the u-boot bootloader for BOARD,
installed to BOOTLOADER-TARGET (a drive), compiled for TRIPLET.

If you want a serial console, make sure to specify one in your
operating-system's kernel-arguments (\"console=ttyS0\" or similar)."
  (operating-system (inherit os)
    (bootloader (bootloader-configuration
                 (bootloader (bootloader (inherit u-boot-bootloader)
                              (package (make-u-boot-package board triplet))))
                 (targets (list bootloader-target))))))

(define* (embedded-installation-os bootloader bootloader-target tty
                                   #:key (extra-modules '()))
  "Return an installation os for embedded systems.
The initrd gets the extra modules EXTRA-MODULES.
A getty is provided on TTY.
The bootloader BOOTLOADER is installed to BOOTLOADER-TARGET."
  (operating-system
    (inherit installation-os)
    (bootloader (bootloader-configuration
                 (bootloader bootloader)
                 (targets (list bootloader-target))))
    (kernel linux-libre)
    (kernel-arguments
     (cons (string-append "console=" tty)
           (operating-system-user-kernel-arguments installation-os)))
    (initrd-modules (append extra-modules %base-initrd-modules))))

(define am335x-evm-installation-os
  (embedded-installation-os u-boot-am335x-evm-bootloader
                            "/dev/sda"
                            "ttyO0"
                            #:extra-modules
                            ;; This module is required to mount the sd card.
                            '("omap_hsmmc")))

(define beaglebone-black-installation-os
  (embedded-installation-os u-boot-beaglebone-black-bootloader
                            "/dev/sda"
                            "ttyO0"
                            #:extra-modules
                            ;; This module is required to mount the sd card.
                            '("omap_hsmmc")))


(define a20-olinuxino-lime-installation-os
  (embedded-installation-os u-boot-a20-olinuxino-lime-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define a20-olinuxino-lime2-emmc-installation-os
  (embedded-installation-os u-boot-a20-olinuxino-lime2-bootloader
                            "/dev/mmcblk1" ; eMMC storage
                            "ttyS0"))

(define a20-olinuxino-micro-installation-os
  (embedded-installation-os u-boot-a20-olinuxino-micro-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define bananapi-m2-ultra-installation-os
  (embedded-installation-os u-boot-bananapi-m2-ultra-bootloader
                            "/dev/mmcblk1" ; eMMC storage
                            "ttyS0"))

(define firefly-rk3399-installation-os
  (embedded-installation-os u-boot-firefly-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card/eMMC (SD priority) storage
                            "ttyS2")) ; UART2 connected on the Pi2 bus

(define mx6cuboxi-installation-os
  (embedded-installation-os u-boot-mx6cuboxi-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttymxc0"))

(define novena-installation-os
  (embedded-installation-os u-boot-novena-bootloader
                            "/dev/mmcblk1" ; SD card storage
                            "ttymxc1"))

(define nintendo-nes-classic-edition-installation-os
  (embedded-installation-os u-boot-nintendo-nes-classic-edition-bootloader
                            "/dev/mmcblk0" ; SD card (solder it yourself)
                            "ttyS0"))

(define orangepi-r1-plus-lts-rk3328-installation-os
  (embedded-installation-os u-boot-orangepi-r1-plus-lts-rk3328-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define pine64-plus-installation-os
  (embedded-installation-os u-boot-pine64-plus-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define pinebook-installation-os
  (embedded-installation-os u-boot-pinebook-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define rock64-installation-os
  (embedded-installation-os u-boot-rock64-rk3328-bootloader
                            "/dev/mmcblk0" ; SD card/eMMC (SD priority) storage
                            "ttyS2")) ; UART2 connected on the Pi2 bus

(define rock-4c-plus-installation-os
  (embedded-installation-os u-boot-rock-4c-plus-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS2")) ;; Default UART as per the Linux DTS.

(define rockpro64-installation-os
  (embedded-installation-os u-boot-rockpro64-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card/eMMC (SD priority) storage
                            "ttyS2")) ; UART2 connected on the Pi2 bus

(define rk3399-puma-installation-os
  (embedded-installation-os u-boot-puma-rk3399-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttyS0"))

(define wandboard-installation-os
  (embedded-installation-os u-boot-wandboard-bootloader
                            "/dev/mmcblk0" ; SD card storage
                            "ttymxc0"))

;; Return the default os here so 'guix system' can consume it directly.
installation-os

;;; install.scm ends here
