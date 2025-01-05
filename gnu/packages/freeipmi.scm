;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2017, 2020, 2021, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages freeipmi)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix licenses)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gnupg))

(define-public freeipmi
  (package
    (name "freeipmi")
    (version "1.6.15")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://gnu/freeipmi/freeipmi-"
                                 version ".tar.gz"))
             (sha256
              (base32
               "1bwc5gz3985fly84ap1yq8jkddkf6s5px2dinmswxx9r8qsrr4nn"))))
    (build-system gnu-build-system)
    (arguments
     (append
      (if (and (%current-target-system)
               (target-riscv64?))
          (list #:phases
                #~(modify-phases %standard-phases
                    (add-after 'unpack 'update-config-scripts
                      (lambda* (#:key inputs native-inputs #:allow-other-keys)
                        ;; Replace outdated config.guess and config.sub.
                        (for-each (lambda (file)
                                    (install-file
                                     (search-input-file
                                      (or native-inputs inputs)
                                      (string-append "/bin/" file)) "config"))
                                  '("config.guess" "config.sub"))))))
          '())
             (list #:configure-flags
                   #~'(#$(string-append
                          "CFLAGS=-g -O2"
                          " -Wno-error=implicit-function-declaration"
                          " -Wno-error=incompatible-pointer-types")
                       "--disable-static"
                       #$@(if (%current-target-system)
                              ;; We cannot check for these devices
                              ;; when cross compiling.
                              `("ac_cv_file__dev_random=yes"
                                "ac_cv_file__dev_urandom=yes")
                              '())))))
    (native-inputs
     (if (and (%current-target-system)
              (target-riscv64?))
         (list config)
         '()))
    (inputs
     (list libgcrypt))
    (home-page "https://www.gnu.org/software/freeipmi/")
    (synopsis "Platform management, including sensor and power monitoring")
    (description
     "GNU FreeIPMI is a collection of in-band and out-of-band IPMI software
in accordance with the IPMI v1.5/2.0 specification.  These programs provide a
set of interfaces for platform management.  Common functionality includes
sensor monitoring, system event monitoring, power control and
serial-over-LAN.")
    (license gpl3+)))
