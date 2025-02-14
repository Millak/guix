;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024 Herman Rimm <herman@rimm.ee>
;;; Copyright © 2025 Sharlatan Hellseher <sharlatanus@gmail.com>
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

(define-module (gnu packages printers)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages avahi)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt))

;; This is a module for packages related to printer-like devices, but not
;; related to CUPS.

(define-public ipp-usb
  (package
    (name "ipp-usb")
    (version "0.9.28")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OpenPrinting/ipp-usb")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "068rfy8arjxhgwll46bfq0x3ypi05qjd6gv0zqynfrk2zcwn4f3y"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/OpenPrinting/ipp-usb"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'install-manual-page
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (let* ((man (string-append #$output "/share/man/man8")))
                  (mkdir-p man)
                  (invoke "go-md2man"
                          "-in" "ipp-usb.8.md"
                          "-out" (string-append man "/ipp-usb.8"))))))
          (add-after 'install 'install-udev-rules
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (substitute* "systemd-udev/71-ipp-usb.rules"
                  (("systemd") "shephard")
                  ((", ENV\\{SYSTEMD_WANTS\\}\\+=\"ipp-usb.service\"") ""))
                (install-file "systemd-udev/71-ipp-usb.rules"
                              (string-append #$output "/lib/udev/rules.d"))))))))
    (inputs
     (list avahi libusb))
    (native-inputs
     (list go-github-com-openprinting-goipp
           go-github-com-go-md2man
           pkg-config))
    (home-page "https://github.com/OpenPrinting/ipp-usb")
    (synopsis "HTTP reverse proxy, backed by the IPP-over-USB connection")
    (description
     "ipp-usb implements an HTTP reverse proxy, backed by the IPP-over-USB
connection to the device. This is because IPP-over-USB implementations which
simply relay a TCP connection to USB do not work.")
    (license license:bsd-2)))

(define-public robocut
  (package
    (name "robocut")
    (version "1.0.11")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Timmmm/robocut")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dp9cssyik63yvkk35s51v94a873x751iqg93qzd8dpqkmz5z8gn"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'configure
            (lambda _
              (substitute* "Robocut.pro"
                (("/usr/") (string-append #$output "/")))
              (invoke "qmake" (string-append "PREFIX=" #$output)))))))
    (inputs
     (list libusb qtbase-5 qtsvg-5))
    (native-inputs
     (list pkg-config qtbase-5))
    (synopsis "Graphical program to drive plotting cutters")
    (description
     "Robocut is a simple graphical program that allows you to cut graphics
with Graphtec and Sihouette plotting cutters using an SVG file as its input.")
    (home-page "http://robocut.org")
    (license license:gpl3+)))
