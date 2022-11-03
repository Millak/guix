;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Ludovic Courtès <ludo@gnu.org>
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
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (gnu installer hardware)
  #:use-module (gnu build linux-modules)
  #:use-module (guix i18n)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-71)
  #:export (unsupported-pci-device?
            pci-device-description))

(define %unsupported-linux-modules
  ;; List of Linux modules that are useless without non-free firmware.
  ;;
  ;; Currently only drivers for PCI devices are listed.  USB devices such as
  ;; "btintel" would require support to list USB devices and read the USB
  ;; device ID database.  Punt for now as this is usually less critical.
  ;;
  ;; This list is currently manually maintained based on information on
  ;; non-free firmware available from
  ;; <https://packages.debian.org/search?keywords=firmware&searchon=names&suite=stable&section=all>.
  '(;; WiFi.
    "brcmfmac"
    "ipw2100"
    "ipw2200"
    "iwlwifi"
    "mwl8k"
    "rtl8188ee"
    "rtl818x_pci"
    "rtl8192ce"
    "rtl8192de"
    "rtl8192ee"

    ;; Ethernet.
    "bnx2"
    "bnx2x"
    "liquidio"

    ;; Graphics.
    "amdgpu"
    "radeon"

    ;; Multimedia.
    "ivtv"))

(define unsupported-pci-device?
  ;; Arrange to load the module alias database only once.
  (let ((aliases (delay (known-module-aliases))))
    (lambda (device)
      "Return true if DEVICE is known to not be supported by free software."
      (any (lambda (module)
             (member module %unsupported-linux-modules))
           (matching-modules (pci-device-module-alias device)
                             (force aliases))))))

(define (pci-device-description pci-database)
  "Return a procedure that, given a PCI device, returns a string describing
it."
  (define (with-fallback lookup)
    (lambda (vendor-id id)
      (let ((vendor name (lookup vendor-id id)))
        (values (or vendor (number->string vendor-id 16))
                (or name (number->string id 16))))))

  (define pci-lookup
    (with-fallback (load-pci-device-database pci-database)))

  (lambda (device)
    (let ((vendor name (pci-lookup (pci-device-vendor device)
                                   (pci-device-id device))))
      (if (network-pci-device? device)
          ;; TRANSLATORS: The two placeholders are the manufacturer
          ;; and name of a PCI device.
          (format #f (G_ "~a ~a (networking device)")
                  vendor name)
          (string-append vendor " " name)))))
