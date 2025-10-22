;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2019, 2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2020, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Leo Famulari <leo@famulari.name>
;;; Copyright © 2023 Denys Nykula <vegan@libre.net.ua>
;;; Copyright © 2025 Florian Pelz <pelzflorian@pelzflorian.de>
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

(define-module (gnu installer services)
  #:use-module (guix records)
  #:use-module (guix read-print)
  #:use-module (guix utils)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-1)
  #:export (system-service?
            system-service-name
            system-service-type
            system-service-recommended?
            system-service-snippet
            system-service-packages

            desktop-system-service?
            system-service-none

            %system-services
            system-services->configuration))

(define-syntax-rule (G_ str)
  ;; In this file, translatable strings are annotated with 'G_' so xgettext
  ;; catches them, but translation happens later on at run time.
  str)

(define-record-type* <system-service>
  system-service make-system-service
  system-service?
  (name            system-service-name)           ;string
  (type            system-service-type)           ;'desktop|'networking|…
  (recommended?    system-service-recommended?    ;Boolean
                   (default #f))
  (snippet         system-service-snippet         ;list of sexps
                   (default '()))
  (packages        system-service-packages        ;list of sexps
                   (default '())))

(define system-service-none
  (system-service
   (name (G_ "None"))
   (type 'network-management)
   (snippet '())))

(define (%system-services)
  (let-syntax ((desktop-environment (syntax-rules ()
                                      ((_ fields ...)
                                       (system-service
                                        (type 'desktop)
                                        fields ...)))))
    (list
     ;; This is the list of desktop environments supported as services.
     (desktop-environment
      (name "GNOME")
      (snippet '((service gnome-desktop-service-type))))
     (desktop-environment
      (name "Xfce")
      (snippet '((service xfce-desktop-service-type))))
     (desktop-environment
      (name "MATE")
      (snippet '((service mate-desktop-service-type))))
     (desktop-environment
      (name "Icewm")
      (snippet '((specification->package "icewm"))))
     (desktop-environment
      (name "Openbox")
      (packages '((specification->package "openbox"))))
     (desktop-environment
      (name "awesome")
      (packages '((specification->package "awesome"))))
     (desktop-environment
      (name "i3")
      (packages (map (lambda (package)
                       `(specification->package ,package))
                     '("i3-wm" "i3status" "dmenu" "st"))))
     (desktop-environment
      (name "ratpoison")
      (packages '((specification->package "ratpoison")
                  (specification->package "xterm"))))
     (desktop-environment
      (name "Emacs EXWM")
      (packages '((specification->package "emacs")
                  (specification->package "emacs-exwm")
                  (specification->package "emacs-desktop-environment"))))

     ;; Networking.
     (system-service
      (name (G_ "OpenSSH secure shell daemon (sshd)"))
      (type 'networking)
      (snippet `(,(vertical-space 1)
                 ,(comment
                   (G_ "\
;; To configure OpenSSH, pass an 'openssh-configuration'
;; record as a second argument to 'service' below.\n"))
                 ,(if (target-hurd?)
                      '(service openssh-service-type
                                (openssh-configuration
                                 (openssh openssh-sans-x)))
                      '(service openssh-service-type)))))
     (system-service
      (name (G_ "Tor anonymous network router"))
      (type 'networking)
      (snippet '((service tor-service-type))))

     ;; Miscellaneous system administration services.
     (system-service
       (name (G_ "Network time service (NTP), to set the clock automatically"))
       (type 'administration)
       (recommended? (not (target-hurd?)))
       (snippet '((service ntp-service-type))))
     (system-service
       (name (G_ "GPM mouse daemon, to use the mouse on the console"))
       (type 'administration)
       (snippet '((service gpm-service-type))))

     ;; Network connectivity management.
     (system-service
      (name (G_ "NetworkManager network connection manager"))
      (type 'network-management)
      (snippet '((service network-manager-service-type)
                 (service wpa-supplicant-service-type))))
     (system-service
      (name (G_ "Connman network connection manager"))
      (type 'network-management)
      (snippet '((service connman-service-type)
                 (service wpa-supplicant-service-type))))
     (system-service
      (name (G_ "DHCP client (dynamic IP address assignment)"))
      (type 'network-management)
      (snippet '((service dhcpcd-service-type))))
     (system-service
      (name (G_ "Static networking service."))
      (type 'network-management)
      (snippet `((service
                  static-networking-service-type
                  (list %loopback-static-networking
                        (static-networking
                         (addresses
                          (list
                           (network-address
                            (device "eth0")
                            ,(comment (G_ ";; Fill-in your IP.\n"))
                            (value "192.168.178.10/24"))))
                         (routes
                          (list (network-route
                                 (destination "default")
                                 ,(comment (G_ ";; Fill-in your gateway IP.\n"))
                                 (gateway "192.168.178.1"))))
                         (requirement '())
                         (provision '(networking))
                         ,(comment (G_ ";; Fill-in your nameservers.\n"))
                         (name-servers '("192.168.178.1"))))))))

     ;; Dealing with documents.
     (system-service
      (name (G_ "CUPS printing system (no Web interface by default)"))
      (type 'document)
      (snippet '((service cups-service-type)))))))

(define (desktop-system-service? service)
  "Return true if SERVICE is a desktop environment service."
  (eq? 'desktop (system-service-type service)))

(define (system-services->configuration services)
  "Return the configuration field for SERVICES."
  (let* ((snippets (append-map system-service-snippet services))
         (packages (append-map system-service-packages services))
         (desktop? (find desktop-system-service? services))
         (base     (if desktop?
                       (if (target-hurd?)
                           '%desktop-services/hurd
                           '%desktop-services)
                       (if (target-hurd?)
                           '%base-services/hurd
                           '%base-services)))
         (native-console-font (match (false-if-exception
                                      (string-take (getenv "LANGUAGE") 2))
                                ((or "el" ;easier with more complete Greek font
                                     ;; These need Cyrillic:
                                     "be" "bg" "kk" "ky"
                                     "mk" "mn" "ru" "sr" "tg" "uk")
                                 "LatGrkCyr-8x16")
                                (_ #f)))
         (services (if native-console-font
                       `(modify-services ,base
                          (console-font-service-type
                           config => (map (lambda (tty)
                                            (cons (car tty)
                                                  ,native-console-font))
                                          config)))
                       base))
         (service-heading (list (vertical-space 1)
                                (comment (G_ "\
;; Below is the list of system services.  To search for available
;; services, run 'guix system search KEYWORD' in a terminal.\n"))))
         (package-heading (list (vertical-space 1)
                                (comment (G_ "\
;; Packages installed system-wide.  Users can also install packages
;; under their own account: use 'guix search KEYWORD' to search
;; for packages and 'guix install PACKAGE' to install a package.\n")))))

    (if (null? snippets)
        `(,@(if (null? packages)
                (if (target-hurd?)
                    `(,@package-heading
                      (packages %base-packages/hurd))
                    '())
                `(,@package-heading
                  (packages (append (list ,@packages)
                                    ,(if (target-hurd?)
                                         '%base-packages/hurd
                                         '%base-packages)))))

          ,@service-heading
          (services ,services))
        `(,@(if (null? packages)
                (if (target-hurd?)
                    `(,@package-heading
                      (packages %base-packages/hurd))
                    '())
                `(,@package-heading
                  (packages (append (list ,@packages)
                                    ,(if (target-hurd?)
                                         '%base-packages/hurd
                                         '%base-packages)))))

          ,@service-heading
          (services (append (list ,@snippets

                                  ,@(if desktop?
                                        ;; XXX: Assume 'keyboard-layout' is in
                                        ;; scope.
                                        `((set-xorg-configuration
                                           (xorg-configuration
                                            (keyboard-layout keyboard-layout))))
                                        '()))

                            ,(vertical-space 1)
                            ,(comment (G_ "\
;; This is the default list of services we
;; are appending to.\n"))
                            ,services))))))
