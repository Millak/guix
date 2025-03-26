;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2017, 2020, 2022, 2023, 2025 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017, 2019 Hartmut Goebel <h.goebel@crazy-compilers.com>
;;; Copyright © 2018, 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2023 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2017, 2019 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019 Tim Gesthuizen <tim.gesthuizen@yahoo.de>
;;; Copyright © 2019 David Wilson <david@daviwil.com>
;;; Copyright © 2020, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Reza Alizadeh Majd <r.majd@pantherx.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021, 2022 muradm <mail@muradm.net>
;;; Copyright © 2023 Bruno Victal <mirai@makinata.eu>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2024 45mg <45mg.writes@gmail.com>
;;; Copyright © 2024 Raven Hallsby <karl@hallsby.com>
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

(define-module (gnu services desktop)
  #:use-module (gnu services)
  #:use-module (gnu services shepherd)
  #:use-module (gnu services base)
  #:use-module (gnu services configuration)
  #:use-module (gnu services dbus)
  #:use-module (gnu services avahi)
  #:use-module (gnu services xorg)
  #:use-module (gnu services networking)
  #:use-module (gnu services sound)
  #:use-module ((gnu system file-systems)
                #:select (%control-groups
                          %elogind-file-systems
                          file-system))
  #:autoload   (gnu services sddm) (sddm-service-type)
  #:use-module (gnu system)
  #:use-module (gnu system privilege)
  #:use-module (gnu system shadow)
  #:use-module (gnu system uuid)
  #:use-module (gnu system pam)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages kde)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages kde-plasma)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages xfce)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages scanner)
  #:use-module (gnu packages suckless)
  #:use-module (gnu packages sugar)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages lxqt)
  #:use-module (gnu packages mate)
  #:use-module (gnu packages nfs)
  #:use-module (gnu packages enlightenment)
  #:use-module (guix deprecation)
  #:use-module (guix records)
  #:use-module (guix packages)
  #:use-module (guix store)
  #:use-module (guix ui)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix modules)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-26)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:export (<upower-configuration>
            upower-configuration
            upower-configuration?
            upower-configuration-upower
            upower-configuration-watts-up-pro?
            upower-configuration-poll-batteries?
            upower-configuration-ignore-lid?
            upower-configuration-use-percentage-for-policy?
            upower-configuration-percentage-low
            upower-configuration-percentage-critical
            upower-configuration-percentage-action
            upower-configuration-time-low
            upower-configuration-time-critical
            upower-configuration-time-action
            upower-configuration-critical-power-action

            upower-service-type

            udisks-configuration
            udisks-configuration?
            udisks-service  ; deprecated
            udisks-service-type

            gvfs-configuration
            gvfs-configuration?
            gvfs-service-type

            colord-service-type

            geoclue-application
            geoclue-configuration
            geoclue-configuration?
            %standard-geoclue-applications
            geoclue-service  ; deprecated
            geoclue-service-type

            bluetooth-service-type
            bluetooth-configuration
            bluetooth-configuration?
            bluetooth-service  ; deprecated

            elogind-configuration
            elogind-configuration?
            elogind-service  ; deprecated
            elogind-service-type

            %gdm-file-system
            gdm-file-system-service

            %fontconfig-file-system
            fontconfig-file-system-service

            accountsservice-service-type
            accountsservice-service  ; deprecated

            cups-pk-helper-service-type
            sane-service-type

            gnome-desktop-configuration
            gnome-desktop-configuration?
            gnome-desktop-configuration-core-services
            gnome-desktop-configuration-shell
            gnome-desktop-configuration-utilities
            gnome-desktop-configuration-extra-packages
            gnome-desktop-configuration-polkit-ignorelist
            gnome-desktop-configuration-udev-ignorelist
            gnome-desktop-configuration-keyring
            gnome-desktop-service
            gnome-desktop-service-type

            mate-desktop-configuration
            mate-desktop-configuration?
            mate-desktop-service-type

            lxqt-desktop-configuration
            lxqt-desktop-configuration?
            lxqt-desktop-service-type

            sugar-desktop-configuration
            sugar-desktop-configuration?
            sugar-desktop-service-type

            plasma-desktop-configuration
            plasma-desktop-configuration?
            plasma-desktop-service-type

            xfce-desktop-configuration
            xfce-desktop-configuration?
            xfce-desktop-service-type

            x11-socket-directory-service ;deprecated
            x11-socket-directory-service-type

            enlightenment-desktop-configuration
            enlightenment-desktop-configuration?
            enlightenment-desktop-service-type

            inputattach-configuration
            inputattach-configuration?
            inputattach-service-type

            polkit-wheel-service

            gnome-keyring-configuration
            gnome-keyring-configuration?
            gnome-keyring-service-type

            seatd-configuration
            seatd-service-type

            %desktop-services))

;;; Commentary:
;;;
;;; This module contains service definitions for a "desktop" environment.
;;;
;;; Code:


;;;
;;; Helpers.
;;;

(define (bool value)
  (if value "true\n" "false\n"))

(define (package-direct-input-selector tree)
  "Return a procedure that selects TREE from the inputs of PACKAGE.  If TREE
is a list, it recursively searches it until it locates the last item of TREE."
  (lambda (package)
    (let loop ((tree (if (pair? tree)
                         tree
                         (list tree)))
               (package package))
      (if (null? tree)
          package
          (loop (cdr tree)
                (car (assoc-ref (package-direct-inputs package)
                                (car tree))))))))


;;;
;;; Upower D-Bus service.
;;;

(define-record-type* <upower-configuration>
  upower-configuration make-upower-configuration
  upower-configuration?
  (upower                     upower-configuration-upower
                              (default upower))
  (watts-up-pro?              upower-configuration-watts-up-pro?
                              (default #f))
  (poll-batteries?            upower-configuration-poll-batteries?
                              (default #t))
  (ignore-lid?                upower-configuration-ignore-lid?
                              (default #f))
  (use-percentage-for-policy? upower-configuration-use-percentage-for-policy?
                              (default #t))
  (percentage-low             upower-configuration-percentage-low
                              (default 20))
  (percentage-critical        upower-configuration-percentage-critical
                              (default 5))
  (percentage-action          upower-configuration-percentage-action
                              (default 2))
  (time-low                   upower-configuration-time-low
                              (default 1200))
  (time-critical              upower-configuration-time-critical
                              (default 300))
  (time-action                upower-configuration-time-action
                              (default 120))
  (critical-power-action      upower-configuration-critical-power-action
                              (default 'hybrid-sleep)))

(define* upower-configuration-file
  ;; Return an upower-daemon configuration file.
  (match-lambda
    (($ <upower-configuration> upower
        watts-up-pro? poll-batteries? ignore-lid? use-percentage-for-policy?
        percentage-low percentage-critical percentage-action time-low
        time-critical time-action critical-power-action)
     (plain-file "UPower.conf"
                 (string-append
                  "[UPower]\n"
                  "EnableWattsUpPro=" (bool watts-up-pro?)
                  "NoPollBatteries=" (bool (not poll-batteries?))
                  "IgnoreLid=" (bool ignore-lid?)
                  "UsePercentageForPolicy=" (bool use-percentage-for-policy?)
                  "PercentageLow=" (number->string percentage-low) "\n"
                  "PercentageCritical=" (number->string percentage-critical) "\n"
                  "PercentageAction=" (number->string percentage-action) "\n"
                  "TimeLow=" (number->string time-low) "\n"
                  "TimeCritical=" (number->string time-critical) "\n"
                  "TimeAction=" (number->string time-action) "\n"
                  "CriticalPowerAction=" (match critical-power-action
                                           ('hybrid-sleep "HybridSleep")
                                           ('hibernate "Hibernate")
                                           ('power-off "PowerOff"))
                  "\n")))))

(define %upower-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/upower")))

(define (upower-dbus-service config)
  (list (wrapped-dbus-service (upower-configuration-upower config)
                              "libexec/upowerd"
                              `(("UPOWER_CONF_FILE_NAME"
                                 ,(upower-configuration-file config))))))

(define (upower-shepherd-service config)
  "Return a shepherd service for UPower with CONFIG."
  (let ((upower (upower-configuration-upower config))
        (config (upower-configuration-file config)))
    (list (shepherd-service
           (documentation "Run the UPower power and battery monitor.")
           (provision '(upower-daemon))
           (requirement '(user-processes dbus-system udev))

           (start #~(make-forkexec-constructor
                     (list (string-append #$upower "/libexec/upowerd"))
                     #:environment-variables
                     (list (string-append "UPOWER_CONF_FILE_NAME="
                                          #$config))))
           (stop #~(make-kill-destructor))
           (actions (list (shepherd-configuration-action config)))))))

(define upower-service-type
  (let ((upower-package (compose list upower-configuration-upower)))
    (service-type (name 'upower)
                  (description
                   "Run @command{upowerd}, a system-wide monitor for power
consumption and battery levels, with the given configuration settings.  It
implements the @code{org.freedesktop.UPower} D-Bus interface, and is notably
used by GNOME.")
                  (extensions
                   (list (service-extension dbus-root-service-type
                                            upower-dbus-service)
                         (service-extension shepherd-root-service-type
                                            upower-shepherd-service)
                         (service-extension activation-service-type
                                            (const %upower-activation))
                         (service-extension udev-service-type
                                            upower-package)

                         ;; Make the 'upower' command visible.
                         (service-extension profile-service-type
                                            upower-package)))
                  (default-value (upower-configuration)))))


;;;
;;; GeoClue D-Bus service.
;;;

(define* (geoclue-application name #:key (allowed? #t) system? (users '()))
  "Configure default GeoClue access permissions for an application.  NAME is
the Desktop ID of the application, without the .desktop part.  If ALLOWED? is
true, the application will have access to location information by default.
The boolean SYSTEM? value indicates that an application is a system component
or not.  Finally USERS is a list of UIDs of all users for which this
application is allowed location info access.  An empty users list means all
users are allowed."
  (string-append
   "[" name "]\n"
   "allowed=" (bool allowed?)
   "system=" (bool system?)
   "users=" (string-join users ";") "\n"))

(define %standard-geoclue-applications
  (list (geoclue-application "gnome-datetime-panel" #:system? #t)
        (geoclue-application "epiphany" #:system? #f)
        (geoclue-application "firefox" #:system? #f)))

;; TODO: Use define-configuration and export accessors.
(define-record-type* <geoclue-configuration>
  geoclue-configuration make-geoclue-configuration
  geoclue-configuration?
  (geoclue geoclue-configuration-geoclue
           (default geoclue))
  (whitelist geoclue-configuration-whitelist
             (default '()))
  (wifi-geolocation-url
   geoclue-configuration-wifi-geolocation-url
   ;; Mozilla geolocation service:
   (default "https://location.services.mozilla.com/v1/geolocate?key=geoclue"))
  (submit-data? geoclue-configuration-submit-data?
                (default #f))
  (wifi-submission-url
   geoclue-configuration-wifi-submission-url
   (default "https://location.services.mozilla.com/v1/submit?key=geoclue"))
  (submission-nick geoclue-configuration-submission-nick
                   (default "geoclue"))
  (applications geoclue-configuration-applications
                (default %standard-geoclue-applications)))

(define* (geoclue-configuration-file config)
  "Return a geoclue configuration file."
  (plain-file "geoclue.conf"
              (string-append
               "[agent]\n"
               "whitelist="
               (string-join (geoclue-configuration-whitelist config)
                            ";") "\n"
               "[wifi]\n"
               "url=" (geoclue-configuration-wifi-geolocation-url config) "\n"
               "submit-data=" (bool (geoclue-configuration-submit-data? config))
               "submission-url="
               (geoclue-configuration-wifi-submission-url config) "\n"
               "submission-nick="
               (geoclue-configuration-submission-nick config)
               "\n"
               (string-join (geoclue-configuration-applications config)
                            "\n"))))

(define (geoclue-dbus-service config)
  (list (wrapped-dbus-service (geoclue-configuration-geoclue config)
                              "libexec/geoclue"
                              `(("GEOCLUE_CONFIG_FILE"
                                 ,(geoclue-configuration-file config))))))

(define %geoclue-accounts
  (list (user-group (name "geoclue") (system? #t))
        (user-account
         (name "geoclue")
         (group "geoclue")
         (system? #t)
         (comment "GeoClue daemon user")
         (home-directory "/var/empty")
         (shell "/run/current-system/profile/sbin/nologin"))))

(define geoclue-service-type
  (service-type (name 'geoclue)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          geoclue-dbus-service)
                       (service-extension account-service-type
                                          (const %geoclue-accounts))))
                (description "Run the @command{geoclue} location service.
This service provides a D-Bus interface to allow applications to request
access to a user's physical location, and optionally to add information to
online location databases.")
                (default-value (geoclue-configuration))))

(define-deprecated
  (geoclue-service #:key (geoclue geoclue)
                   (whitelist '())
                   (wifi-geolocation-url
                    ;; Mozilla geolocation service:
                    "https://location.services.mozilla.com/v1/geolocate?key=geoclue")
                   (submit-data? #f)
                   (wifi-submission-url
                    "https://location.services.mozilla.com/v1/submit?key=geoclue")
                   (submission-nick "geoclue")
                   (applications %standard-geoclue-applications))
  geoclue-service-type
  "Return a service that runs the @command{geoclue} location service.  This
service provides a D-Bus interface to allow applications to request access to
a user's physical location, and optionally to add information to online
location databases.  By default, only the GNOME date-time panel and the Icecat
and Epiphany web browsers are able to ask for the user's location, and in the
case of Icecat and Epiphany, both will ask the user for permission first.  See
@uref{https://wiki.freedesktop.org/www/Software/GeoClue/, the geoclue web
site} for more information."
  (service geoclue-service-type
           (geoclue-configuration
            (geoclue geoclue)
            (whitelist whitelist)
            (wifi-geolocation-url wifi-geolocation-url)
            (submit-data? submit-data?)
            (wifi-submission-url wifi-submission-url)
            (submission-nick submission-nick)
            (applications applications))))


;;;
;;; Bluetooth.
;;;

(define-record-type* <bluetooth-configuration>
  bluetooth-configuration make-bluetooth-configuration
  bluetooth-configuration?
  (bluez bluetooth-configuration-bluez (default bluez))

  ;;; [General]
  (name bluetooth-configuration-name (default "BlueZ"))
  (class bluetooth-configuration-class (default #x000000))
  (discoverable-timeout
   bluetooth-configuration-discoverable-timeout (default 180))
  (always-pairable? bluetooth-configuration-always-pairable? (default #f))
  (pairable-timeout bluetooth-configuration-pairable-timeout (default 0))

  ;;; MAYBE: Exclude into separate <device-id> record-type?
  (device-id bluetooth-configuration-device-id (default #f))
  (reverse-service-discovery?
   bluetooth-configuration-reverse-service-discovery (default #t))
  (name-resolving? bluetooth-configuration-name-resolving? (default #t))
  (debug-keys? bluetooth-configuration-debug-keys? (default #f))

  ;;; Possible values:
  ;;; 'dual, 'bredr, 'le
  (controller-mode bluetooth-configuration-controller-mode (default 'dual))

  ;;; Possible values:
  ;;; 'off, 'single, 'multiple
  (multi-profile bluetooth-configuration-multi-profile (default 'off))
  (fast-connectable? bluetooth-configuration-fast-connectable? (default #f))

  ;;; Possible values:
  ;;; for LE mode: 'off, 'network/on, 'device
  ;;; for Dual mode: 'off, 'network/on', 'device, 'limited-network, 'limited-device
  ;;; Source: https://git.kernel.org/pub/scm/bluetooth/bluez.git/tree/src/main.conf#n68
  (privacy bluetooth-configuration-privacy (default 'off))

  ;;; Possible values:
  ;;; 'never, 'confirm, 'always
  (just-works-repairing
   bluetooth-configuration-just-works-repairing (default 'never))
  (temporary-timeout bluetooth-configuration-temporary-timeout (default 30))
  (refresh-discovery? bluetooth-configuration-refresh-discovery (default #t))

  ;;; Possible values: #t, #f, (uuid <uuid>)
  ;;; Possible UUIDs:
  ;;; d4992530-b9ec-469f-ab01-6c481c47da1c (BlueZ Experimental Debug)
  ;;; 671b10b5-42c0-4696-9227-eb28d1b049d6 (BlueZ Experimental Simultaneous Central and Peripheral)
  ;;; 15c0a148-c273-11ea-b3de-0242ac130004 (BlueZ Experimental LL privacy)
  ;;; 330859bc-7506-492d-9370-9a6f0614037f (BlueZ Experimental Bluetooth Quality Report)
  ;;; a6695ace-ee7f-4fb9-881a-5fac66c629af (BlueZ Experimental Offload Codecs)
  ;;; Source: https://git.kernel.org/pub/scm/bluetooth/bluez.git/tree/src/main.conf#n110
  (experimental bluetooth-configuration-experimental (default #f))
  (remote-name-request-retry-delay
   bluetooth-configuration-remote-name-request-retry-delay (default 300))

  ;;; [BR]
  (page-scan-type bluetooth-configuration-page-scan-type (default #f))
  (page-scan-interval bluetooth-configuration-page-scan-interval (default #f))
  (page-scan-window bluetooth-configuration-page-scan-window (default #f))
  (inquiry-scan-type bluetooth-configuration-inquiry-scan-type (default #f))
  (inquiry-scan-interval bluetooth-configuration-inquiry-scan-interval (default #f))
  (inquiry-scan-window bluetooth-configuration-inquiry-scan-window (default #f))
  (link-supervision-timeout bluetooth-configuration-link-supervision-timeout (default #f))
  (page-timeout bluetooth-configuration-page-timeout (default #f))
  (min-sniff-interval bluetooth-configuration-min-sniff-interval (default #f))
  (max-sniff-interval bluetooth-configuration-max-sniff-interval (default #f))

  ;;; [LE]
  (min-advertisement-interval
   bluetooth-configuration-min-advertisement-interval (default #f))
  (max-advertisement-interval
   bluetooth-configuration-max-advertisement-interval (default #f))
  (multi-advertisement-rotation-interval
   bluetooth-configuration-multi-advertisement-rotation-interval (default #f))
  (scan-interval-auto-connect
   bluetooth-configuration-scan-interval-auto-connect (default #f))
  (scan-window-auto-connect
   bluetooth-configuration-scan-window-auto-connect (default #f))
  (scan-interval-suspend
   bluetooth-configuration-scan-interval-suspend (default #f))
  (scan-window-suspend
   bluetooth-configuration-scan-window-suspend (default #f))
  (scan-interval-discovery
   bluetooth-configuration-scan-interval-discovery (default #f))
  (scan-window-discovery
   bluetooth-configuration-scan-window-discovery (default #f))
  (scan-interval-adv-monitor
   bluetooth-configuration-scan-interval-adv-monitor (default #f))
  (scan-window-adv-monitor
   bluetooth-configuration-scan-window-adv-monitor (default #f))
  (scan-interval-connect
   bluetooth-configuration-scan-interval-connect (default #f))
  (scan-window-connect
   bluetooth-configuration-scan-window-connect (default #f))
  (min-connection-interval
   bluetooth-configuration-min-connection-interval (default #f))
  (max-connection-interval
   bluetooth-configuration-max-connection-interval (default #f))
  (connection-latency
   bluetooth-configuration-connection-latency (default #f))
  (connection-supervision-timeout
   bluetooth-configuration-connection-supervision-timeout (default #f))
  (autoconnect-timeout
   bluetooth-configuration-autoconnect-timeout (default #f))
  (adv-mon-allowlist-scan-duration
   bluetooth-configuration-adv-mon-allowlist-scan-duration (default 300))
  (adv-mon-no-filter-scan-duration
   bluetooth-configuration-adv-mon-no-filter-scan-duration (default 500))
  (enable-adv-mon-interleave-scan?
   bluetooth-configuration-enable-adv-mon-interleave-scan (default #t))

  ;;; [GATT]
  ;;; Possible values: 'yes, 'no, 'always
  (cache bluetooth-configuration-cache (default 'always))

  ;;; Possible values: 7 ... 16, 0 (don't care)
  (key-size bluetooth-configuration-key-size (default 0))

  ;;; Possible values: 23 ... 517
  (exchange-mtu bluetooth-configuration-exchange-mtu (default 517))

  ;;; Possible values: 1 ... 5
  (att-channels bluetooth-configuration-att-channels (default 3))

  ;;; [AVDTP]
  ;;; Possible values: 'basic, 'ertm
  (session-mode bluetooth-configuration-session-mode (default 'basic))

  ;;; Possible values: 'basic, 'streaming
  (stream-mode bluetooth-configuration-stream-mode (default 'basic))

  ;;; [Policy]
  (reconnect-uuids bluetooth-configuration-reconnect-uuids (default '()))
  (reconnect-attempts bluetooth-configuration-reconnect-attempts (default 7))
  (reconnect-intervals bluetooth-configuration-reconnect-intervals
                       (default (list 1 2 4 8 16 32 64)))
  (auto-enable? bluetooth-configuration-auto-enable? (default #f))
  (resume-delay bluetooth-configuration-resume-delay (default 2))

  ;;; [AdvMon]
  ;;; Possible values:
  ;;; "0x00", "0xFF",
  ;;; "N = 0x00" ... "N = 0xFF"
  ;;; Source: https://git.kernel.org/pub/scm/bluetooth/bluez.git/tree/src/main.conf#n286
  (rssi-sampling-period bluetooth-configuration-rssi-sampling-period
                        (default #xFF)))

(define (bluetooth-configuration-file config)
  "Return a configuration file for the systemd bluetooth service, as a string."
  (string-append
   "[General]"
   "\nName = " (bluetooth-configuration-name config)
   "\nClass = " (string-append
                 "0x"
                 (format #f "~6,'0x" (bluetooth-configuration-class config)))
   "\nDiscoverableTimeout = " (number->string
                               (bluetooth-configuration-discoverable-timeout
                                config))
   "\nAlwaysPairable = " (bool (bluetooth-configuration-always-pairable?
                                config))
   "\nPairableTimeout = " (number->string
                           (bluetooth-configuration-pairable-timeout
                            config))
   (if (bluetooth-configuration-device-id config)
       (string-append "\nDeviceID = " (bluetooth-configuration-device-id config))
       "")
   "\nReverseServiceDiscovery = " (bool
                                   (bluetooth-configuration-reverse-service-discovery
                                    config))
   "\nNameResolving = " (bool (bluetooth-configuration-name-resolving? config))
   "\nDebugKeys = " (bool (bluetooth-configuration-debug-keys? config))
   "\nControllerMode = " (symbol->string
                          (bluetooth-configuration-controller-mode config))
   "\nMultiProfile = " (symbol->string (bluetooth-configuration-multi-profile
                                        config))
   "\nFastConnectable = " (bool (bluetooth-configuration-fast-connectable? config))
   "\nPrivacy = " (symbol->string (bluetooth-configuration-privacy config))
   "\nJustWorksRepairing = " (symbol->string
                              (bluetooth-configuration-just-works-repairing config))
   "\nTemporaryTimeout = " (number->string
                            (bluetooth-configuration-temporary-timeout config))
   "\nRefreshDiscovery = " (bool (bluetooth-configuration-refresh-discovery config))
   "\nExperimental = " (let ((experimental (bluetooth-configuration-experimental config)))
                         (cond ((or (eq? experimental #t)
                                    (eq? experimental #f)) (bool experimental))
                               ((list? experimental)
                                (string-join (map uuid->string experimental) ","))))
   "\nRemoteNameRequestRetryDelay = " (number->string
                                       (bluetooth-configuration-remote-name-request-retry-delay
                                        config))
   "\n[BR]"
   (if (bluetooth-configuration-page-scan-type config)
       (string-append
        "\nPageScanType = "
        (number->string (bluetooth-configuration-page-scan-type config)))
       "")
   (if (bluetooth-configuration-page-scan-interval config)
       (string-append
        "\nPageScanInterval = "
        (number->string (bluetooth-configuration-page-scan-interval config)))
       "")
   (if (bluetooth-configuration-page-scan-window config)
       (string-append
        "\nPageScanWindow = "
        (number->string (bluetooth-configuration-page-scan-window config)))
       "")
   (if (bluetooth-configuration-inquiry-scan-type config)
       (string-append
        "\nInquiryScanType = "
        (number->string (bluetooth-configuration-inquiry-scan-type config)))
       "")
   (if (bluetooth-configuration-inquiry-scan-interval config)
       (string-append
        "\nInquiryScanInterval = "
        (number->string (bluetooth-configuration-inquiry-scan-interval config)))
       "")
   (if (bluetooth-configuration-inquiry-scan-window config)
       (string-append
        "\nInquiryScanWindow = "
        (number->string (bluetooth-configuration-inquiry-scan-window config)))
       "")
   (if (bluetooth-configuration-link-supervision-timeout config)
       (string-append
        "\nLinkSupervisionTimeout = "
        (number->string (bluetooth-configuration-link-supervision-timeout config)))
       "")
   (if (bluetooth-configuration-page-timeout config)
       (string-append
        "\nPageTimeout = "
        (number->string (bluetooth-configuration-page-timeout config)))
       "")
   (if (bluetooth-configuration-min-sniff-interval config)
       (string-append
        "\nMinSniffInterval = "
        (number->string (bluetooth-configuration-min-sniff-interval config)))
       "")
   (if (bluetooth-configuration-max-sniff-interval config)
       (string-append
        "\nMaxSniffInterval = "
        (number->string (bluetooth-configuration-max-sniff-interval config)))
       "")

   "\n[LE]"
   (if (bluetooth-configuration-min-advertisement-interval config)
       (string-append
        "\nMinAdvertisementInterval = "
        (number->string (bluetooth-configuration-min-advertisement-interval config)))
       "")
   (if (bluetooth-configuration-max-advertisement-interval config)
       (string-append
        "\nMaxAdvertisementInterval = "
        (number->string (bluetooth-configuration-max-advertisement-interval config)))
       "")
   (if (bluetooth-configuration-multi-advertisement-rotation-interval config)
       (string-append
        "\nMultiAdvertisementRotationInterval = "
        (number->string
         (bluetooth-configuration-multi-advertisement-rotation-interval config)))
       "")
   (if (bluetooth-configuration-scan-interval-auto-connect config)
       (string-append
        "\nScanIntervalAutoConnect = "
        (number->string (bluetooth-configuration-scan-interval-auto-connect config)))
       "")
   (if (bluetooth-configuration-scan-window-auto-connect config)
       (string-append
        "\nScanWindowAutoConnect = "
        (number->string (bluetooth-configuration-scan-window-auto-connect config)))
       "")
   (if (bluetooth-configuration-scan-interval-suspend config)
       (string-append
        "\nScanIntervalSuspend = "
        (number->string (bluetooth-configuration-scan-interval-suspend config)))
       "")
   (if (bluetooth-configuration-scan-window-suspend config)
       (string-append
        "\nScanWindowSuspend = "
        (number->string (bluetooth-configuration-scan-window-suspend config)))
       "")
   (if (bluetooth-configuration-scan-interval-discovery config)
       (string-append
        "\nScanIntervalDiscovery = "
        (number->string (bluetooth-configuration-scan-interval-discovery config)))
       "")
   (if (bluetooth-configuration-scan-window-discovery config)
       (string-append
        "\nScanWindowDiscovery = "
        (number->string (bluetooth-configuration-scan-window-discovery config)))
       "")
   (if (bluetooth-configuration-scan-interval-adv-monitor config)
       (string-append
        "\nScanIntervalAdvMonitor = "
        (number->string (bluetooth-configuration-scan-interval-adv-monitor config)))
       "")
   (if (bluetooth-configuration-scan-window-adv-monitor config)
       (string-append
        "\nScanWindowAdvMonitor = "
        (number->string (bluetooth-configuration-scan-window-adv-monitor config)))
       "")
   (if (bluetooth-configuration-scan-interval-connect config)
       (string-append
        "\nScanIntervalConnect = "
        (number->string (bluetooth-configuration-scan-interval-connect config)))
       "")
   (if (bluetooth-configuration-scan-window-connect config)
       (string-append
        "\nScanWindowConnect = "
        (number->string (bluetooth-configuration-scan-window-connect config)))
       "")
   (if (bluetooth-configuration-min-connection-interval config)
       (string-append
        "\nMinConnectionInterval = "
        (number->string (bluetooth-configuration-min-connection-interval config)))
       "")
   (if (bluetooth-configuration-max-connection-interval config)
       (string-append
        "\nMaxConnectionInterval = "
        (number->string (bluetooth-configuration-max-connection-interval config)))
       "")
   (if (bluetooth-configuration-connection-latency config)
       (string-append
        "\nConnectionLatency = "
        (number->string (bluetooth-configuration-connection-latency config)))
       "")
   (if (bluetooth-configuration-connection-supervision-timeout config)
       (string-append
        "\nConnectionSupervisionTimeout = "
        (number->string (bluetooth-configuration-connection-supervision-timeout config)))
       "")
   (if (bluetooth-configuration-autoconnect-timeout config)
       (string-append
        "\nAutoconnecttimeout = "
        (number->string (bluetooth-configuration-autoconnect-timeout config)))
       "")
   "\nAdvMonAllowlistScanDuration = " (number->string
                                       (bluetooth-configuration-adv-mon-allowlist-scan-duration
                                        config))
   "\nAdvMonNoFilterScanDuration = " (number->string
                                      (bluetooth-configuration-adv-mon-no-filter-scan-duration
                                       config))
   "\nEnableAdvMonInterleaveScan = " (number->string
                                      (if (eq? #t
                                               (bluetooth-configuration-enable-adv-mon-interleave-scan
                                                config))
                                          1 0))

   "\n[GATT]"
   "\nCache = " (symbol->string (bluetooth-configuration-cache config))
   "\nKeySize = " (number->string (bluetooth-configuration-key-size config))
   "\nExchangeMTU = " (number->string (bluetooth-configuration-exchange-mtu config))
   "\nChannels = " (number->string (bluetooth-configuration-att-channels config))

   "\n[AVDTP]"
   "\nSessionMode = " (symbol->string (bluetooth-configuration-session-mode config))
   "\nStreamMode = " (symbol->string (bluetooth-configuration-stream-mode config))

   "\n[Policy]"
   (let ((uuids (bluetooth-configuration-reconnect-uuids config)))
     (if (not (eq? '() uuids))
         (string-append
          "\nReconnectUUIDs = "
          (string-join (map uuid->string uuids) ","))
         ""))
   "\nReconnectAttempts = " (number->string
                             (bluetooth-configuration-reconnect-attempts config))
   "\nReconnectIntervals = " (string-join
                              (map number->string
                                   (bluetooth-configuration-reconnect-intervals
                                    config))
                              ",")
   "\nAutoEnable = " (bool (bluetooth-configuration-auto-enable?
                            config))
   "\nResumeDelay = " (number->string (bluetooth-configuration-resume-delay config))

   "\n[AdvMon]"
   "\nRSSISamplingPeriod = " (string-append
                              "0x"
                              (format #f "~2,'0x"
                                      (bluetooth-configuration-rssi-sampling-period config)))))

(define (bluetooth-directory config)
  (computed-file "etc-bluetooth"
                 #~(begin
                     (mkdir #$output)
                     (chdir #$output)
                     (call-with-output-file "main.conf"
                       (lambda (port)
                         (display #$(bluetooth-configuration-file config)
                                  port))))))

(define (bluetooth-shepherd-service config)
  "Return a shepherd service for @command{bluetoothd}."
  (shepherd-service
   (provision '(bluetooth))
   (requirement '(user-processes dbus-system udev))
   (documentation "Run the bluetoothd daemon.")
   (start #~(make-forkexec-constructor
             (list #$(file-append (bluetooth-configuration-bluez config)
                                  "/libexec/bluetooth/bluetoothd"))))
   (stop #~(make-kill-destructor))))

(define bluetooth-service-type
  (service-type
   (name 'bluetooth)
   (extensions
    (list (service-extension dbus-root-service-type
                             (compose list bluetooth-configuration-bluez))
          (service-extension udev-service-type
                             (compose list bluetooth-configuration-bluez))
          (service-extension etc-service-type
                             (lambda (config)
                               `(("bluetooth"
                                  ,(bluetooth-directory config)))))
          (service-extension shepherd-root-service-type
                             (compose list bluetooth-shepherd-service))))
   (default-value (bluetooth-configuration))
   (description "Run the @command{bluetoothd} daemon, which manages all the
Bluetooth devices and provides a number of D-Bus interfaces.")))

(define-deprecated (bluetooth-service #:key (bluez bluez) (auto-enable? #f))
  bluetooth-service-type
  "Return a service that runs the @command{bluetoothd} daemon, which manages
all the Bluetooth devices and provides a number of D-Bus interfaces.  When
AUTO-ENABLE? is true, the bluetooth controller is powered automatically at
boot, which can be useful when using a bluetooth keyboard or mouse.
"
  (service bluetooth-service-type
           (bluetooth-configuration
            (bluez bluez)
            (auto-enable? auto-enable?))))


;;;
;;; Colord D-Bus service.
;;;

(define %colord-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/colord")
      (let ((user (getpwnam "colord")))
        (chown "/var/lib/colord"
               (passwd:uid user) (passwd:gid user)))))

(define %colord-accounts
  (list (user-group (name "colord") (system? #t))
        (user-account
         (name "colord")
         (group "colord")
         (system? #t)
         (comment "colord daemon user")
         (home-directory "/var/empty")
         (shell (file-append shadow "/sbin/nologin")))))

(define colord-service-type
  (service-type (name 'colord)
                (extensions
                 (list (service-extension account-service-type
                                          (const %colord-accounts))
                       (service-extension activation-service-type
                                          (const %colord-activation))

                       ;; Colord is a D-Bus service that dbus-daemon can
                       ;; activate.
                       (service-extension dbus-root-service-type list)

                       ;; Colord provides "color device" rules for udev.
                       (service-extension udev-service-type list)

                       ;; It provides polkit "actions".
                       (service-extension polkit-service-type list)))
                (default-value colord)
                (description
                 "Run @command{colord}, a system service with a D-Bus
interface to manage the color profiles of input and output devices such as
screens and scanners.")))


;;;
;;; UDisks.
;;;

(define-record-type* <udisks-configuration>
  udisks-configuration make-udisks-configuration
  udisks-configuration?
  (udisks   udisks-configuration-udisks
            (default udisks)))

(define %udisks-activation
  (with-imported-modules '((guix build utils))
    #~(begin
        (use-modules (guix build utils))

        (let ((run-dir "/var/run/udisks2"))
          (mkdir-p run-dir)
          (chmod run-dir #o700)))))

(define udisks-service-type
  (let ((udisks-package (lambda (config)
                          (list (udisks-configuration-udisks config)))))
    (service-type (name 'udisks)
                  (extensions
                   (list (service-extension polkit-service-type
                                            udisks-package)
                         (service-extension dbus-root-service-type
                                            udisks-package)
                         (service-extension udev-service-type
                                            udisks-package)
                         (service-extension activation-service-type
                                            (const %udisks-activation))

                         ;; Profile 'udisksctl' & co. in the system profile.
                         (service-extension profile-service-type
                                            udisks-package)))
                  (description "Run UDisks, a @dfn{disk management} daemon
that provides user interfaces with notifications and ways to mount/unmount
disks.  Programs that talk to UDisks include the @command{udisksctl} command,
part of UDisks, and GNOME Disks.")
                  (default-value (udisks-configuration)))))

(define-deprecated (udisks-service #:key (udisks udisks))
  udisks-service-type
  "Return a service for @uref{http://udisks.freedesktop.org/docs/latest/,
UDisks}, a @dfn{disk management} daemon that provides user interfaces with
notifications and ways to mount/unmount disks.  Programs that talk to UDisks
include the @command{udisksctl} command, part of UDisks, and GNOME Disks."
  (service udisks-service-type
           (udisks-configuration (udisks udisks))))



;;;
;;; GVfs virtual file system.
;;;

(define-record-type* <gvfs-configuration>
  gvfs-configuration make-gvfs-configuration
  gvfs-configuration?
  (gvfs gvfs-package (default gvfs)))

(define gvfs-service-type
  (service-type (name 'gvfs)
                (extensions
                 (list
                  (service-extension profile-service-type
                                     (compose list gvfs-package))
                  ;; Required for gvfs-udisks2-volume-monitor.
                  (service-extension udisks-service-type (const #t))))
                (description
                 "Make GVfs virtual file systems (Trash, SFTP, SMB, HTTP,
and many other) available for GIO applications.")
                (default-value (gvfs-configuration))))


;;;
;;; Elogind login and seat management service.
;;;

(define-record-type* <elogind-configuration> elogind-configuration
  make-elogind-configuration
  elogind-configuration?
  (elogind                          elogind-package
                                    (default elogind))
  (kill-user-processes?             elogind-kill-user-processes?
                                    (default #f))
  (kill-only-users                  elogind-kill-only-users
                                    (default '()))
  (kill-exclude-users               elogind-kill-exclude-users
                                    (default '("root")))
  (inhibit-delay-max-seconds        elogind-inhibit-delay-max-seconds
                                    (default 5))
  (handle-power-key                 elogind-handle-power-key
                                    (default 'poweroff))
  (handle-suspend-key               elogind-handle-suspend-key
                                    (default 'suspend))
  (handle-hibernate-key             elogind-handle-hibernate-key
                                    (default 'hibernate))
  (handle-lid-switch                elogind-handle-lid-switch
                                    (default 'suspend))
  (handle-lid-switch-docked         elogind-handle-lid-switch-docked
                                    (default 'ignore))
  (handle-lid-switch-external-power elogind-handle-lid-switch-external-power
                                    (default *unspecified*))
  (power-key-ignore-inhibited?      elogind-power-key-ignore-inhibited?
                                    (default #f))
  (suspend-key-ignore-inhibited?    elogind-suspend-key-ignore-inhibited?
                                    (default #f))
  (hibernate-key-ignore-inhibited?  elogind-hibernate-key-ignore-inhibited?
                                    (default #f))
  (lid-switch-ignore-inhibited?     elogind-lid-switch-ignore-inhibited?
                                    (default #t))
  (holdoff-timeout-seconds          elogind-holdoff-timeout-seconds
                                    (default 30))
  (idle-action                      elogind-idle-action
                                    (default 'ignore))
  (idle-action-seconds              elogind-idle-action-seconds
                                    (default (* 30 60)))
  (runtime-directory-size-percent   elogind-runtime-directory-size-percent
                                    (default 10))
  (runtime-directory-size           elogind-runtime-directory-size
                                    (default #f))
  (remove-ipc?                      elogind-remove-ipc?
                                    (default #t))

  (suspend-state                    elogind-suspend-state
                                    (default '("mem" "standby" "freeze")))
  (suspend-mode                     elogind-suspend-mode
                                    (default '()))
  (hibernate-state                  elogind-hibernate-state
                                    (default '("disk")))
  (hibernate-mode                   elogind-hibernate-mode
                                    (default '("platform" "shutdown")))
  (hybrid-sleep-state               elogind-hybrid-sleep-state
                                    (default '("disk")))
  (hybrid-sleep-mode                elogind-hybrid-sleep-mode
                                    (default
                                      '("suspend" "platform" "shutdown")))
  (hibernate-delay-seconds          elogind-hibernate-delay-seconds
                                    (default *unspecified*))
  (suspend-estimation-seconds       elogind-suspend-estimation-seconds
                                    (default *unspecified*))
  (system-sleep-hook-files          elogind-system-sleep-hook-files
                                    (default '()))
  (system-shutdown-hook-files       elogind-system-shutdown-hook-files
                                    (default '()))
  (allow-power-off-interrupts?      elogind-allow-power-off-interrupts?
                                    (default #f))
  (allow-suspend-interrupts?        elogind-allow-suspend-interrupts?
                                    (default #f))
  (broadcast-power-off-interrupts?  elogind-broadcast-power-off-interrupts?
                                    (default #t))
  (broadcast-suspend-interrupts?    elogind-broadcast-suspend-interrupts?
                                    (default #t)))

(define (elogind-configuration-file config)
  (define (yesno x)
    (match x
      (#t "yes")
      (#f "no")
      (_ (error "expected #t or #f, instead got:" x))))
  (define char-set:user-name
    (string->char-set "abcdefghijklmnopqrstuvwxyz0123456789_-"))
  (define (valid-list? l pred)
    (and-map (lambda (x) (string-every pred x)) l))
  (define (user-name-list users)
    (unless (valid-list? users char-set:user-name)
      (error "invalid user list" users))
    (string-join users " "))
  (define (enum val allowed)
    (unless (memq val allowed)
      (error "invalid value" val allowed))
    (symbol->string val))
  (define (non-negative-integer x)
    (unless (exact-integer? x) (error "not an integer" x))
    (when (negative? x) (error "negative number not allowed" x))
    (number->string x))
  (define (maybe-non-negative-integer x)
    (or (and (unspecified? x) x)
        (non-negative-integer x)))
  (define handle-actions
    '(ignore poweroff reboot halt kexec suspend hibernate hybrid-sleep suspend-then-hibernate lock))
  (define (handle-action x)
    (if (unspecified? x)
        x                               ;let the unspecified value go through
        (enum x handle-actions)))
  (define (sleep-list tokens)
    (unless (valid-list? tokens char-set:user-name)
      (error "invalid sleep list" tokens))
    (string-join tokens " "))
  (define-syntax ini-file-clause
    (syntax-rules ()
      ;; Produce an empty line when encountering an unspecified value.  This
      ;; is better than an empty string value, which can, in some cases, cause
      ;; warnings such as "Failed to parse handle action setting".
      ((_ config (prop (parser getter)))
       (let ((value (parser (getter config))))
         (if (unspecified? value)
             ""
             (string-append prop "=" value "\n"))))
      ((_ config str)
       (if (unspecified? str)
           ""
           (string-append str "\n")))))
  (define-syntax-rule (ini-file config file clause ...)
    (plain-file file (string-append (ini-file-clause config clause) ...)))
  (ini-file
   config "logind.conf"
   "[Login]"
   ("KillUserProcesses" (yesno elogind-kill-user-processes?))
   ("KillOnlyUsers" (user-name-list elogind-kill-only-users))
   ("KillExcludeUsers" (user-name-list elogind-kill-exclude-users))
   ("InhibitDelayMaxSec" (non-negative-integer elogind-inhibit-delay-max-seconds))
   ("HandlePowerKey" (handle-action elogind-handle-power-key))
   ("HandleSuspendKey" (handle-action elogind-handle-suspend-key))
   ("HandleHibernateKey" (handle-action elogind-handle-hibernate-key))
   ("HandleLidSwitch" (handle-action elogind-handle-lid-switch))
   ("HandleLidSwitchDocked" (handle-action elogind-handle-lid-switch-docked))
   ("HandleLidSwitchExternalPower" (handle-action elogind-handle-lid-switch-external-power))
   ("PowerKeyIgnoreInhibited" (yesno elogind-power-key-ignore-inhibited?))
   ("SuspendKeyIgnoreInhibited" (yesno elogind-suspend-key-ignore-inhibited?))
   ("HibernateKeyIgnoreInhibited" (yesno elogind-hibernate-key-ignore-inhibited?))
   ("LidSwitchIgnoreInhibited" (yesno elogind-lid-switch-ignore-inhibited?))
   ("HoldoffTimeoutSec" (non-negative-integer elogind-holdoff-timeout-seconds))
   ("IdleAction" (handle-action elogind-idle-action))
   ("IdleActionSec" (non-negative-integer elogind-idle-action-seconds))
   ("RuntimeDirectorySize"
    (identity
     (lambda (config)
       (match (elogind-runtime-directory-size-percent config)
         (#f (non-negative-integer (elogind-runtime-directory-size config)))
         (percent (string-append (non-negative-integer percent) "%"))))))
   ("RemoveIPC" (yesno elogind-remove-ipc?))
   "[Sleep]"
   ("SuspendState" (sleep-list elogind-suspend-state))
   ("SuspendMode" (sleep-list elogind-suspend-mode))
   ("HibernateState" (sleep-list elogind-hibernate-state))
   ("HibernateMode" (sleep-list elogind-hibernate-mode))
   ("HybridSleepState" (sleep-list elogind-hybrid-sleep-state))
   ("HybridSleepMode" (sleep-list elogind-hybrid-sleep-mode))
   ("HibernateDelaySec" (maybe-non-negative-integer elogind-hibernate-delay-seconds))
   ("SuspendEstimationSec" (maybe-non-negative-integer elogind-suspend-estimation-seconds))
   ("AllowPowerOffInterrupts" (yesno elogind-allow-power-off-interrupts?))
   ("AllowSuspendInterrupts" (yesno elogind-allow-suspend-interrupts?))
   ("BroadcastPowerOffInterrupts" (yesno elogind-broadcast-power-off-interrupts?))
   ("BroadcastSuspendInterrupts" (yesno elogind-broadcast-suspend-interrupts?))))

(define (elogind-etc-directory config)
  "Return the /etc/elogind directory for CONFIG."
  (with-imported-modules (source-module-closure '((guix build utils)))
    (computed-file
     "etc-elogind"

     #~(begin
         (use-modules (guix build utils))

         (define sleep-directory (string-append #$output "/system-sleep/"))
         (define shutdown-directory (string-append #$output "/system-shutdown/"))

         (define (copy-script file directory)
           "Copy FILE into DIRECTORY, giving rx (500) permissions."
           (let ((dest (string-append directory "/" (basename file))))
             (mkdir-p directory)
             (copy-file file dest)
             (chmod dest #o500)))

         (mkdir-p #$output)            ;in case neither directory gets created
         (for-each (lambda (f)
                     (copy-script f sleep-directory))
                   '#$(elogind-system-sleep-hook-files config))
         (for-each (lambda (f)
                     (copy-script f shutdown-directory))
                   '#$(elogind-system-shutdown-hook-files config))))))

(define (elogind-dbus-service config)
  "Return a @file{org.freedesktop.login1.service} file that tells D-Bus how to
\"start\" elogind.  In practice though, our elogind is started when booting by
shepherd.  Thus, the @code{Exec} line of this @file{.service} file does not
explain how to start elogind; instead, it spawns a wrapper that waits for the
@code{elogind} shepherd service.  This avoids a race condition where both
@command{shepherd} and @command{dbus-daemon} would attempt to start elogind."
  ;; For more info on the elogind startup race, see
  ;; <https://issues.guix.gnu.org/55444>.

  (define elogind
    (elogind-package config))

  (define wrapper
    (program-file "elogind-dbus-shepherd-sync"
                  (with-imported-modules '((gnu services herd))
                    #~(begin
                        (use-modules (gnu services herd)
                                     (srfi srfi-34))

                        (guard (c ((service-not-found-error? c)
                                   (format (current-error-port)
                                           "no elogind shepherd service~%")
                                   (exit 1))
                                  ((shepherd-error? c)
                                   (format (current-error-port)
                                           "elogind shepherd service not \
started~%")
                                   (exit 2)))
                          (wait-for-service 'elogind))))))

  (define build
    (with-imported-modules '((guix build utils))
      #~(begin
          (use-modules (guix build utils)
                       (ice-9 match))

          (define service-directory
            "/share/dbus-1/system-services")

          (mkdir-p (dirname (string-append #$output service-directory)))
          (copy-recursively (string-append #$elogind service-directory)
                            (string-append #$output service-directory))
          (symlink (string-append #$elogind "/etc") ;for etc/dbus-1
                   (string-append #$output "/etc"))
          ;; Also expose the D-Bus policy configurations (.conf) files, now
          ;; installed under '/share' instead of the legacy '/etc' prefix.
          (symlink (string-append #$elogind "/share/dbus-1/system.d")
                   (string-append #$output "/share/dbus-1/system.d"))

          ;; Replace the "Exec=" line of the 'org.freedesktop.login1.service'
          ;; file with one that refers to WRAPPER instead of elogind.
          (match (find-files #$output "\\.service$")
            ((file)
             (substitute* file
               (("Exec[[:blank:]]*=.*" _)
                (string-append "Exec=" #$wrapper "\n"))))))))

  (list (computed-file "elogind-dbus-service-wrapper" build)))

(define (pam-extension-procedure config)
  "Return an extension for PAM-ROOT-SERVICE-TYPE that ensures that all the PAM
services use 'pam_elogind.so', a module that allows elogind to keep track of
logged-in users (run 'loginctl' to see elogind's world view of users and
seats.)"
  (define pam-elogind
    (pam-entry
     (control "required")
     (module (file-append (elogind-package config)
                          "/lib/security/pam_elogind.so"))))

  (list (pam-extension
         (transformer
          (lambda (pam)
            (pam-service
             (inherit pam)
             (session (cons pam-elogind (pam-service-session pam))))))
         (shepherd-requirements '(elogind)))))

(define (elogind-shepherd-service config)
  "Return a Shepherd service to start elogind according to @var{config}."
  (define config-file
    (elogind-configuration-file config))

  (list (shepherd-service
         (requirement '(user-processes dbus-system))
         (provision '(elogind))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (elogind-package config)
                                        "/libexec/elogind/elogind"))
                   #:environment-variables
                   (list (string-append "ELOGIND_CONF_FILE="
                                        #$config-file))))
         (stop #~(make-kill-destructor))
         (actions (list (shepherd-configuration-action config-file))))))

(define elogind-service-type
  (service-type (name 'elogind)
                (extensions
                 (list (service-extension dbus-root-service-type
                                          elogind-dbus-service)
                       (service-extension udev-service-type
                                          (compose list elogind-package))
                       (service-extension polkit-service-type
                                          (compose list elogind-package))

                       ;; Start elogind from the Shepherd rather than waiting
                       ;; for bus activation.  This ensures that it can handle
                       ;; events like lid close, etc.
                       (service-extension shepherd-root-service-type
                                          elogind-shepherd-service)

                       ;; Provide the 'loginctl' command.
                       (service-extension profile-service-type
                                          (compose list elogind-package))

                       ;; Extend PAM with pam_elogind.so.
                       (service-extension pam-root-service-type
                                          pam-extension-procedure)

                       ;; Install sleep/shutdown hook files.
                       (service-extension etc-service-type
                                          (lambda (config)
                                            `(("elogind"
                                               ,(elogind-etc-directory config)))))

                       ;; We need /run/user, /run/systemd, etc.
                       (service-extension file-system-service-type
                                          (const %elogind-file-systems))))
                (default-value (elogind-configuration))
                (description "Run the @command{elogind} login and seat
management service.  The @command{elogind} service integrates with PAM to
allow other system components to know the set of logged-in users as well as
their session types (graphical, console, remote, etc.).  It can also clean up
after users when they log out.")))

(define-deprecated (elogind-service #:key (config (elogind-configuration)))
  elogind-service-type
  "Return a service that runs the @command{elogind} login and seat management
service.  The @command{elogind} service integrates with PAM to allow other
system components to know the set of logged-in users as well as their session
types (graphical, console, remote, etc.).  It can also clean up after users
when they log out."
  (service elogind-service-type config))


;;;
;;; Fontconfig and other desktop file-systems.
;;;

(define %fontconfig-file-system
  (file-system
    (device "none")
    (mount-point "/var/cache/fontconfig")
    (type "tmpfs")
    (flags '(read-only))
    (check? #f)))

(define %gdm-file-system
  (file-system
    (device "none")
    (mount-point "/var/lib/gdm")
    (type "tmpfs")
    (check? #f)))

;; The global fontconfig cache directory can sometimes contain stale entries,
;; possibly referencing fonts that have been GC'd, so mount it read-only.
;; As mentioned https://debbugs.gnu.org/cgi/bugreport.cgi?bug=36924#8 and
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=38046#10 and elsewhere.
(define fontconfig-file-system-service
  (simple-service 'fontconfig-file-system
                  file-system-service-type
                  (list %fontconfig-file-system)))

;; Avoid stale caches and stale user IDs being reused between system
;; reconfigurations, which would crash GDM and render the system unusable.
;; GDM doesn't require persisting anything valuable there anyway.
(define gdm-file-system-service
  (simple-service 'gdm-file-system
                  file-system-service-type
                  (list %gdm-file-system)))


;;;
;;; AccountsService service.
;;;

(define %accountsservice-activation
  #~(begin
      (use-modules (guix build utils))
      (mkdir-p "/var/lib/AccountsService")))

(define accountsservice-service-type
  (service-type (name 'accountsservice)
                (extensions
                 (list (service-extension activation-service-type
                                          (const %accountsservice-activation))
                       (service-extension dbus-root-service-type list)
                       (service-extension polkit-service-type list)))
                (default-value accountsservice)
                (description "Run AccountsService, a system service available
over D-Bus that can list available accounts, change their passwords, and so
on.  AccountsService integrates with PolicyKit to enable unprivileged users to
acquire the capability to modify their system configuration.")))

(define-deprecated
  (accountsservice-service #:key (accountsservice accountsservice))
  accountsservice-service-type
  "Return a service that runs AccountsService, a system service that
can list available accounts, change their passwords, and so on.
AccountsService integrates with PolicyKit to enable unprivileged users to
acquire the capability to modify their system configuration.
@uref{https://www.freedesktop.org/wiki/Software/AccountsService/, the
accountsservice web site} for more information."
  (service accountsservice-service-type accountsservice))


;;;
;;; cups-pk-helper service.
;;;

(define cups-pk-helper-service-type
  (service-type
   (name 'cups-pk-helper)
   (description
    "PolicyKit helper to configure CUPS with fine-grained privileges.")
   (extensions
    (list (service-extension dbus-root-service-type list)
          (service-extension polkit-service-type list)))
   (default-value cups-pk-helper)))


;;;
;;; Scanner access via SANE.
;;;

(define %sane-accounts
  ;; The '60-libsane.rules' udev rules refers to the "scanner" group.
  (list (user-group (name "scanner") (system? #t))))

(define sane-service-type
  (service-type
   (name 'sane)
   (description
    "This service provides access to scanners @i{via}
@uref{http://www.sane-project.org, SANE} by installing the necessary udev
rules.")
   (default-value sane-backends-minimal)
   (extensions
    (list (service-extension udev-service-type list)
          (service-extension account-service-type
                             (const %sane-accounts))))))


;;;
;;; gnome-keyring-service-type
;;;

(define-record-type* <gnome-keyring-configuration> gnome-keyring-configuration
  make-gnome-keyring-configuration
  gnome-keyring-configuration?
  (keyring gnome-keyring-package (default gnome-keyring))
  (pam-services gnome-keyring-pam-services (default '(("gdm-password" . login)
                                                      ("passwd" . passwd)))))

(define (pam-gnome-keyring config)
  ;; CONFIG may be either a <gnome-desktop-configuration> or a
  ;; <gnome-keyring-configuration>> record, when using the
  ;; gnome-keyring-service-type on its own.
  (let ((config (if (gnome-desktop-configuration? config)
                    (gnome-desktop-configuration-keyring
                     config)
                    config)))
    (match config
      (#f '())                          ;explicitly disabled by user
      (_
       (define (%pam-keyring-entry . arguments)
         (pam-entry
          (control "optional")
          (module (file-append (gnome-keyring-package config)
                               "/lib/security/pam_gnome_keyring.so"))
          (arguments arguments)))

       (list
        (pam-extension
         (transformer
          (lambda (service)
            (case (assoc-ref (gnome-keyring-pam-services config)
                             (pam-service-name service))
              ((login)
               (pam-service
                (inherit service)
                (auth (append (pam-service-auth service)
                              (list (%pam-keyring-entry))))
                (session (append (pam-service-session service)
                                 (list (%pam-keyring-entry "auto_start"))))))
              ((passwd)
               (pam-service
                (inherit service)
                (password (append (pam-service-password service)
                                  (list (%pam-keyring-entry))))))
              (else service))))))))))

(define gnome-keyring-service-type
  (service-type
   (name 'gnome-keyring)
   (extensions (list
                (service-extension pam-root-service-type pam-gnome-keyring)))
   (default-value (gnome-keyring-configuration))
   (description "Return a service, that extends PAM with entries using
@code{pam_gnome_keyring.so}, unlocking a user's login keyring when they log in
or setting its password with passwd.")))


;;;
;;; GNOME desktop service.
;;;

(define-maybe/no-serialization package)

(define (gnome-keyring-configuration-or-#f? value)
  (or (gnome-keyring-configuration? value)
      (not value)))

(define (extract-propagated-inputs package)
  ;; Drop input labels.  Attempt to support outputs.
  (map
   (match-lambda
     ((_ (? package? pkg)) pkg)
     ((_ (? package? pkg) output) (list pkg output)))
   (package-propagated-inputs package)))

(define-configuration/no-serialization gnome-desktop-configuration
  (core-services
   (list-of-packages (extract-propagated-inputs gnome-meta-core-services))
   "A list of packages that the GNOME Shell and applications may rely on.")
  (shell
   (list-of-packages (extract-propagated-inputs gnome-meta-core-shell))
   "A list of packages that constitute the GNOME Shell, without applications.")
  (utilities
   (list-of-packages (extract-propagated-inputs gnome-meta-core-utilities))
   "A list of packages that serve as applications to use on top of the \
GNOME Shell.")
  (gnome (maybe-package) "Deprecated.  Do not use.")
  (extra-packages
   (list-of-packages (extract-propagated-inputs gnome-essential-extras))
   "A list of GNOME-adjacent packages to also include.  This field is intended
for users to add their own packages to their GNOME experience.  Note, that it
already includes some packages that are considered essential by some (most?)
GNOME users.")
  (udev-ignorelist
   (list-of-strings '())
   "A list of regular expressions denoting udev rules or hardware file names
provided by any package that should not be installed.  By default, every udev
rule and hardware file specified by any package referenced in the other fields
are installed.")
  (polkit-ignorelist
   (list-of-strings '())
   "A list of regular expressions denoting polkit rules provided by any package
that should not be installed.  By default, every polkit rule added by any package
referenced in the other fields are installed.")
  (keyring
   (gnome-keyring-configuration-or-#f (gnome-keyring-configuration))
   "A <gnome-keyring-configuration> record used to better integrate the GNOME
keyring with the system.  Refer to the documentation of the
@code{gnome-keyring-service-type} for more information.  If you'd rather avoid
integrating the GNOME keyring, you can set this to @code{#f}."))

(define (gnome-package gnome name)
  "Return the package NAME among the GNOME package inputs.  NAME can be a
single name or a tree-like, e.g. @code{'(\"gnome-boxes\" \"spice-gtk\")} to
denote the spice-gtk input of the gnome-boxes input of the GNOME meta-package."
  ((package-direct-input-selector name) gnome))

(define (gnome-packages gnome names)
  "Return the package NAMES among the GNOME package inputs."
  (map (cut gnome-package gnome <>) names))

(define (gnome-udev-configuration-files config)
  "Return the GNOME udev rules and hardware files as computed from its
dependencies by filtering out the ignorelist."
  (list
   (computed-file
    "gnome-udev-configurations"
    (with-imported-modules
        (source-module-closure '((guix build utils)
                                 (guix build union)))
      #~(begin
          (use-modules (guix build utils)
                       (guix build union))
          ;; If rules.d or hwdb.d is not a proper directory but a symlink,
          ;; then it will not be possible to delete individual files in this
          ;; directory.
          (union-build #$output
                       (search-path-as-list
                        (list "lib/udev" "libexec/udev")
                        (list #$@(gnome-profile config #:transitive? #t)))
                       #:create-all-directories? #t)
          (for-each
           (lambda (pattern)
             (for-each
              delete-file-recursively
              (find-files #$output pattern)))
           (list #$@(gnome-desktop-configuration-udev-ignorelist config))))))))

(define (gnome-polkit-settings config)
  "Return the list of GNOME dependencies that provide polkit actions and
rules."
  (list
   (computed-file
    "gnome-polkit-settings"
    (with-imported-modules
        (source-module-closure '((guix build utils)
                                 (guix build union)))
      #~(let ((output (string-append #$output "/share/polkit-1")))
          (use-modules (guix build utils)
                       (guix build union))
          (mkdir-p (dirname output))
          (union-build output
                       (search-path-as-list
                        (list "share/polkit-1")
                        (list #$@(gnome-profile config #:transitive? #t)))
                       #:create-all-directories? #t)
          (for-each
           (lambda (pattern)
             (for-each
              delete-file-recursively
              (find-files output pattern)))
           (list #$@(gnome-desktop-configuration-polkit-ignorelist config))))))))

(define* (gnome-profile config #:key transitive?)
  "Return the list of the packages specified in CONFIG.  When TRANSITIVE? is
#t, also include their transitive propagated inputs.  If there are transitive
inputs using non-default outputs, they are returned as gexp-input objects."
  (define gnome-packages
    (append
     (gnome-desktop-configuration-core-services config)
     (gnome-desktop-configuration-shell config)
     (gnome-desktop-configuration-utilities config)
     (let ((gnome-meta (gnome-desktop-configuration-gnome config)))
       (if (maybe-value-set? gnome-meta)
           (begin
             (warning
              (gnome-desktop-configuration-source-location config)
              (G_ "Using a meta-package for gnome-desktop is discouraged.~%"))
             (list gnome-meta))
           (list)))
     (gnome-desktop-configuration-extra-packages config)))
  (if transitive?
      (append gnome-packages
              (append-map (compose (cut map (match-lambda ;discard labels
                                              ((_ pkg) pkg)
                                              ((_ pkg out)
                                               (gexp-input pkg out)))
                                        <>)
                                   package-transitive-propagated-inputs)
                          gnome-packages))
      gnome-packages))

(define (gnome-setuid-programs config)
  "Return the list of setuid programs found within the packages specified in
CONFIG, a <gnome-desktop-configuration> object."
  ;; spice-gtk provides polkit actions for USB redirection in GNOME Boxes; set
  ;; its usb-acl-helper script setuid automatically when the gnome-boxes or
  ;; spice-gtk packages are added to one of the gnome-desktop-configuration
  ;; fields.
  (let* ((gnome-packages (gnome-profile config #:transitive? #t))
         (spice-gtk (find (compose (cut string=? "spice-gtk" <>)
                                   package-name
                                   (match-lambda ;disregard potential output
                                     ((? package? p) p)
                                     ((? gexp-input? p)
                                      (gexp-input-thing p))))
                          gnome-packages))
         (files `(,@(if spice-gtk
                        (list (file-append
                               spice-gtk
                               "/libexec/spice-client-glib-usb-acl-helper"))
                        '()))))
    (map file-like->setuid-program files)))

(define gnome-desktop-service-type
  (service-type
   (name 'gnome-desktop)
   (extensions
    (list (service-extension udev-service-type
                             gnome-udev-configuration-files)
          (service-extension pam-root-service-type
                             pam-gnome-keyring)
          (service-extension polkit-service-type
                             gnome-polkit-settings)
          (service-extension privileged-program-service-type
                             gnome-setuid-programs)
          (service-extension profile-service-type
                             gnome-profile)))
   (default-value (gnome-desktop-configuration))
   (description "Run the GNOME desktop environment.")))


;;;
;;; MATE Desktop service.
;;; TODO: Add mate-screensaver.

(define-record-type* <mate-desktop-configuration> mate-desktop-configuration
  make-mate-desktop-configuration
  mate-desktop-configuration?
  (mate-package mate-package (default mate)))

(define (mate-polkit-extension config)
  "Return the list of packages for CONFIG's MATE package that extend polkit."
  (let ((mate (mate-package config)))
    (map (lambda (input)
           ((package-direct-input-selector input) mate))
         '("mate-system-monitor"                  ;kill, renice processes
           "mate-settings-daemon"                 ;date/time settings
           "mate-power-manager"                   ;modify brightness
           "mate-control-center"                  ;RandR, display properties FIXME
           "mate-applets"))))                     ;CPU frequency scaling

(define mate-desktop-service-type
  (service-type
   (name 'mate-desktop)
   (extensions
    (list (service-extension polkit-service-type
                             mate-polkit-extension)
          (service-extension profile-service-type
                             (compose list
                                      mate-package))))
   (default-value (mate-desktop-configuration))
   (description "Run the MATE desktop environment.")))


;;;
;;; XFCE desktop service.
;;;

(define-record-type* <xfce-desktop-configuration> xfce-desktop-configuration
  make-xfce-desktop-configuration
  xfce-desktop-configuration?
  (xfce xfce-package (default xfce)))

(define (xfce-polkit-settings config)
  "Return the list of XFCE dependencies that provide polkit actions and
rules."
  (let ((xfce (xfce-package config)))
    (map (lambda (name)
           ((package-direct-input-selector name) xfce))
         '("thunar"
           "xfce4-power-manager"))))

(define (xfce-pam-services config)
  (list (unix-pam-service "xfce4-screensaver")))

(define xfce-desktop-service-type
  (service-type
   (name 'xfce-desktop)
   (extensions
    (list (service-extension polkit-service-type
                             xfce-polkit-settings)
          (service-extension pam-root-service-type
                             xfce-pam-services)
          (service-extension profile-service-type
                             (compose list xfce-package))))
   (default-value (xfce-desktop-configuration))
   (description "Run the Xfce desktop environment.")))


;;;
;;; Lxqt desktop service.
;;;

(define-record-type* <lxqt-desktop-configuration> lxqt-desktop-configuration
  make-lxqt-desktop-configuration
  lxqt-desktop-configuration?
  (lxqt lxqt-package
        (default lxqt)))

(define (lxqt-polkit-settings config)
  "Return the list of LXQt dependencies that provide polkit actions and
rules."
  (let ((lxqt (lxqt-package config)))
    (map (lambda (name)
           ((package-direct-input-selector name) lxqt))
         '("lxqt-admin"))))

(define lxqt-desktop-service-type
  (service-type
   (name 'lxqt-desktop)
   (extensions
    (list (service-extension polkit-service-type
                             lxqt-polkit-settings)
          (service-extension profile-service-type
                             (compose list lxqt-package))))
   (default-value (lxqt-desktop-configuration))
   (description "Run LXQt desktop environment.")))


;;;
;;; Sugar desktop service.
;;;

(define-record-type* <sugar-desktop-configuration> sugar-desktop-configuration
  make-sugar-desktop-configuration
  sugar-desktop-configuration?
  (sugar sugar-package (default sugar))
  (gobject-introspection
   sugar-gobject-introspection (default gobject-introspection))
  (activities
   sugar-activities (default (list sugar-help-activity))))

(define (sugar-polkit-settings config)
  "Return the list of packages that provide polkit actions and rules."
  (list (sugar-package config)))

(define sugar-desktop-service-type
  (service-type
   (name 'sugar-desktop)
   (extensions
    (list (service-extension polkit-service-type
                             sugar-polkit-settings)
          (service-extension profile-service-type
                             (lambda (config)
                               (cons* (sugar-package config)
                                      (sugar-gobject-introspection config)
                                      (sugar-activities config))))))
   (default-value (sugar-desktop-configuration))
   (description "Run the Sugar desktop environment.")))


;;;
;;; X11 socket directory service
;;;

(define x11-socket-directory-service-type
  (let ((x11-socket-directory-shepherd-service
         (shepherd-service
          (documentation "Create @file{/tmp/.X11-unix} for XWayland.")
          (requirement '(file-systems))
          (provision '(x11-socket-directory))
          (one-shot? #t)
          (start #~(lambda _
                     (let ((directory "/tmp/.X11-unix"))
                       (mkdir-p directory)
                       (chmod directory #o1777)))))))
    (service-type
     (name 'x11-socket-directory-service)
     (extensions
      (list
       (service-extension shepherd-root-service-type
                          (compose
                           list
                           (const x11-socket-directory-shepherd-service)))))
     (default-value #f) ; no default value required
     (description
      "Create @file{/tmp/.X11-unix} for XWayland.  When using X11, libxcb
takes care of creating that directory however, when using XWayland, we
need to create it beforehand."))))

(define-deprecated x11-socket-directory-service
  x11-socket-directory-service-type
  ;; Return a service that creates /tmp/.X11-unix.  When using X11, libxcb
  ;; takes care of creating that directory.  However, when using XWayland, we
  ;; need to create beforehand.  Thus, create it unconditionally here.
  (service x11-socket-directory-service-type))


;;;
;;; Enlightenment desktop service.
;;;

(define-record-type* <enlightenment-desktop-configuration>
  enlightenment-desktop-configuration make-enlightenment-desktop-configuration
  enlightenment-desktop-configuration?
  ;; <package>
  (enlightenment        enlightenment-package
                        (default enlightenment)))

(define (enlightenment-privileged-programs enlightenment-desktop-configuration)
  (match-record enlightenment-desktop-configuration
      <enlightenment-desktop-configuration>
    (enlightenment)
    (map file-like->setuid-program
         (list (file-append enlightenment
                            "/lib/enlightenment/utils/enlightenment_sys")
               (file-append enlightenment
                            "/lib/enlightenment/utils/enlightenment_system")
               (file-append enlightenment
                            "/lib/enlightenment/utils/enlightenment_ckpasswd")))))

(define enlightenment-desktop-service-type
  (service-type
   (name 'enlightenment-desktop)
   (extensions
    (list (service-extension dbus-root-service-type
                             (compose list
                                      (package-direct-input-selector
                                       "efl")
                                      enlightenment-package))
          (service-extension udev-service-type
                             (compose list
                                      (package-direct-input-selector
                                        "ddcutil")
                                      enlightenment-package))
          (service-extension privileged-program-service-type
                             enlightenment-privileged-programs)
          (service-extension profile-service-type
                             (compose list
                                      enlightenment-package))))
   (default-value (enlightenment-desktop-configuration))
   (description
    "Return a service that adds the @code{enlightenment} package to the system
profile, and extends dbus with the ability for @code{efl} to generate
thumbnails and privileges the programs which enlightenment needs to function
as expected.")))

;;;
;;; KDE Plasma desktop service.
;;;

(define-record-type* <plasma-desktop-configuration> plasma-desktop-configuration
  make-plasma-desktop-configuration
  plasma-desktop-configuration?
  (plasma-package plasma-package (default plasma)))

(define (plasma-polkit-settings config)
  "Return the list of KDE Plasma dependencies that provide polkit actions and
rules."
  (let ((plasma-plasma (plasma-package config)))
    (map (lambda (name)
           ((package-direct-input-selector name) plasma-plasma))
         '("plasma-desktop"
           "plasma-workspace"
           "plasma-disks"
           "kinfocenter"
           "libksysguard"
           "ktexteditor"
           "powerdevil"
           "plasma-firewall"))))

(define (plasma-dbus-service config)
  "Return the list of KDE Plasma dependencies that provide D-Bus services."
  (let ((plasma-plasma (plasma-package config)))
    (map (lambda (name)
           ((package-direct-input-selector name) plasma-plasma))
         '("plasma-desktop"
           "plasma-workspace"
           "kactivitymanagerd"
           "plasma-disks"
           "kdeplasma-addons"
           "kinfocenter"
           "libksysguard"
           "kpmcore"
           "ktexteditor"
           "kwalletmanager"
           "powerdevil"
           "bluedevil"
           "kwallet"
           "plasma-firewall"))))

;; see https://bugs.kde.org/show_bug.cgi?id=456210
;; if `kde' no exits, fallback to `other', and then unlock lockscreen not work,
;; so add it.
(define (plasma-pam-services config)
  (list (unix-pam-service "kde")))

(define plasma-desktop-service-type
  (service-type
   (name 'plasma-desktop)
   (description "Run the KDE Plasma desktop environment.")
   (default-value (plasma-desktop-configuration))
   (extensions
    (list (service-extension polkit-service-type
                             plasma-polkit-settings)
          (service-extension dbus-root-service-type
                             plasma-dbus-service)
          (service-extension pam-root-service-type
                             plasma-pam-services)
          (service-extension profile-service-type
                             (compose list
                                      plasma-package))))))


;;;
;;; inputattach-service-type
;;;

(define-record-type* <inputattach-configuration>
  inputattach-configuration
  make-inputattach-configuration
  inputattach-configuration?
  (device-type inputattach-configuration-device-type
               (default "wacom"))
  (device inputattach-configuration-device
          (default "/dev/ttyS0"))
  (baud-rate inputattach-configuration-baud-rate
             (default #f))
  (log-file inputattach-configuration-log-file
            (default #f)))

(define inputattach-shepherd-service
  (match-lambda
    (($ <inputattach-configuration> type device baud-rate log-file)
     (let ((args (append (if baud-rate
                             (list "--baud" (number->string baud-rate))
                             '())
                         (list (string-append "--" type)
                               device))))
       (list (shepherd-service
              (provision '(inputattach))
              (requirement '(user-processes udev))
              (documentation "inputattach daemon")
              (start #~(make-forkexec-constructor
                        (cons (string-append #$inputattach
                                             "/bin/inputattach")
                              (quote #$args))
                        #:log-file #$log-file))
              (stop #~(make-kill-destructor))))))))

(define inputattach-service-type
  (service-type
   (name 'inputattach)
   (extensions
    (list (service-extension shepherd-root-service-type
                             inputattach-shepherd-service)))
   (default-value (inputattach-configuration))
   (description "Return a service that runs inputattach on a device and
dispatches events from it.")))


;;;
;;; polkit-wheel-service -- Allow wheel group to perform admin actions
;;;

(define polkit-wheel
  (file-union
   "polkit-wheel"
   `(("share/polkit-1/rules.d/wheel.rules"
      ,(plain-file
        "wheel.rules"
        "polkit.addAdminRule(function(action, subject) {
    return [\"unix-group:wheel\"];
});
")))))

(define polkit-wheel-service
  (simple-service 'polkit-wheel polkit-service-type (list polkit-wheel)))


;;;
;;; seatd-service-type -- minimal seat management daemon
;;;

(define (seatd-group-sanitizer group-or-name)
  (match group-or-name
    ((? user-group? group) group)
    ((? string? group-name) (user-group (name group-name) (system? #t)))
    (_ (leave (G_ "seatd: '~a' is not a valid group~%") group-or-name))))

(define-record-type* <seatd-configuration> seatd-configuration
  make-seatd-configuration
  seatd-configuration?
  (seatd seatd-package (default seatd))
  (group seatd-group                    ; string | <user-group>
         (default "seat")
         (sanitize seatd-group-sanitizer))
  (socket seatd-socket (default "/run/seatd.sock"))
  (logfile seatd-logfile (default "/var/log/seatd.log"))
  (loglevel seatd-loglevel (default "info")))

(define (seatd-shepherd-service config)
  (list (shepherd-service
         (documentation "Minimal seat management daemon")
         (requirement '(user-processes))
         ;; TODO: once cgroups is separate dependency
         ;; here we should depend on it rather than elogind
         (provision '(seatd elogind))
         (start #~(make-forkexec-constructor
                   (list #$(file-append (seatd-package config) "/bin/seatd")
                         "-g" #$(user-group-name (seatd-group config)))
                   #:environment-variables
                   (list (string-append "SEATD_LOGLEVEL="
                                        #$(seatd-loglevel config))
                         (string-append "SEATD_DEFAULTPATH="
                                        #$(seatd-socket config)))
                   #:log-file #$(seatd-logfile config)))
         (stop #~(make-kill-destructor)))))

(define seatd-accounts
  (match-lambda (($ <seatd-configuration> _ group) (list group))))

(define seatd-environment
  (match-lambda
    (($ <seatd-configuration> _ _ socket)
     `(("SEATD_SOCK" . ,socket)))))

(define seatd-service-type
  (service-type
   (name 'seatd)
   (description "Seat management takes care of mediating access
to shared devices (graphics, input), without requiring the
applications needing access to be root.")
   (extensions
    (list
     (service-extension account-service-type seatd-accounts)
     (service-extension session-environment-service-type seatd-environment)
     ;; TODO: once cgroups is separate dependency we should not mount it here
     ;; for now it is mounted here, because elogind mounts it
     (service-extension file-system-service-type (const %control-groups))
     (service-extension shepherd-root-service-type seatd-shepherd-service)))
   (default-value (seatd-configuration))))


;;;
;;; The default set of desktop services.
;;;

(define* (desktop-services-for-system #:optional
                                      (system (or (%current-target-system)
                                                  (%current-system))))
  ;; List of services typically useful for a "desktop" use case.

  ;; Since GDM depends on Rust and Rust is not available on all platforms,
  ;; use SDDM as the fall-back display manager.
  ;; TODO: Switch the condition to use (supported-package? "rust") and make
  ;; a news entry about the change.
  (cons* (if (string-prefix? "x86_64" system)
             (service gdm-service-type)
             (service sddm-service-type))

         ;; Screen lockers are a pretty useful thing and these are small.
         (service screen-locker-service-type
                  (screen-locker-configuration
                   (name "slock")
                   (program (file-append slock "/bin/slock"))))
         (service screen-locker-service-type
                  (screen-locker-configuration
                   (name "xlock")
                   (program (file-append xlockmore "/bin/xlock"))))

         ;; Add udev rules for MTP devices so that non-root users can access
         ;; them.
         (simple-service 'mtp udev-service-type (list libmtp))
         ;; Add udev rules for scanners.
         (service sane-service-type)
         ;; Add polkit rules, so that non-root users in the wheel group can
         ;; perform administrative tasks (similar to "sudo").
         polkit-wheel-service

         ;; Allow desktop users to also mount NTFS and NFS file systems
         ;; without root.
         (simple-service 'mount-setuid-helpers privileged-program-service-type
                         (map file-like->setuid-program
                              (list (file-append nfs-utils "/sbin/mount.nfs")
                               (file-append ntfs-3g "/sbin/mount.ntfs-3g"))))

         ;; This is a volatile read-write file system mounted at /var/lib/gdm,
         ;; to avoid GDM stale cache and permission issues.
         gdm-file-system-service

         ;; The global fontconfig cache directory can sometimes contain
         ;; stale entries, possibly referencing fonts that have been GC'd,
         ;; so mount it read-only.
         fontconfig-file-system-service

         ;; NetworkManager and its applet.
         (service network-manager-service-type)
         (service wpa-supplicant-service-type)    ;needed by NetworkManager
         (simple-service 'network-manager-applet
                         profile-service-type
                         (list network-manager-applet))
         (service modem-manager-service-type)
         (service usb-modeswitch-service-type)

         ;; The D-Bus clique.
         (service avahi-service-type)
         (service udisks-service-type)
         (service upower-service-type)
         (service accountsservice-service-type)
         (service cups-pk-helper-service-type)
         (service colord-service-type)
         (service geoclue-service-type)
         (service polkit-service-type)
         (service elogind-service-type)
         (service dbus-root-service-type)

         (service ntp-service-type)

         (service x11-socket-directory-service-type)

         (service pulseaudio-service-type)
         (service alsa-service-type)

         %base-services))

(define-syntax %desktop-services
  (identifier-syntax (desktop-services-for-system)))

;;; desktop.scm ends here
