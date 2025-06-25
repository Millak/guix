;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 Tomas Volf <~@wolfsden.cz>
;;; Copyright © 2023 Raven Hallsby <karl@hallsby.com>
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

;;;; Commentary:

;;; Power-related packages.

;;;; Code:

(define-module (gnu packages power)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages man)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python-xyz)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public apcupsd
  (package
    (name "apcupsd")
    (version "3.14.14")
    (source (origin
              (method url-fetch)
              (uri
               (string-append
                "mirror://sourceforge/" name "/" name " - Stable/" version
                "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0rwqiyzlg9p0szf3x6q1ppvrw6f6dbpn2rc5z623fk3bkdalhxyv"))))
    (outputs '("out" "doc"))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list
         ;; The configure script ignores --prefix for most of the file names.
         (string-append "--exec-prefix=" #$output)
         (string-append "--mandir=" #$output "/share/man")
         (string-append "--sbindir=" #$output "/sbin")
         (string-append "--sysconfdir=" #$output "/etc/apcupsd")
         (string-append "--with-halpolicydir=" #$output "/share/halpolicy")

         ;; Put us into the version string.
         "--with-distname=GNU Guix"
         "--disable-install-distdir"

         ;; State directories.
         "--localstatedir=/var"
         "--with-log-dir=/var/log"
         "--with-pid-dir=/run"
         "--with-lock-dir=/run/apcupsd/lock"
         "--with-nologin=/run/apcupsd"
         "--with-pwrfail-dir=/run/apcupsd"

         ;; Configure requires these, but we do not use the genenerated
         ;; apcupsd.conf, so in order to reduce dependencies of the package,
         ;; provide fake values.
         (string-append "ac_cv_path_SHUTDOWN=/nope")
         (string-append "ac_cv_path_APCUPSD_MAIL=/nope")
         ;; While `wall' is not expanded anywhere, it still is searched for.
         ;; See https://sourceforge.net/p/apcupsd/mailman/message/59128628/ .
         (string-append "ac_cv_path_WALL=/nope")

         ;; Enable additional drivers.
         "--enable-usb"
         "--enable-modbus-usb")
      #:tests? #f                       ; There are no tests.
      #:modules (cons '(ice-9 ftw) %default-gnu-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'configure 'remove-time-from-manual
            (lambda _
              ;; Do not bake the date and time of the build into the manual.
              (substitute* "doc/manual/manual.rst"
                (("\\| \\|date\\| \\|time\\|") ""))
              (substitute* "autoconf/variables.mak.in"
                (("^(RST2HTMLOPTS = .*) --time (.*)" all pref suff)
                 (string-append pref " " suff)))))
          (add-after 'build 'build-manual
            (lambda _
              (invoke "make" "-C" "doc/manual" "manual.html")))
          (add-after 'install-license-files 'move-doc
            (lambda _
              (let ((target (string-append #$output:doc
                                           "/share/doc/"
                                           (strip-store-file-name #$output))))
                (mkdir-p target)
                (for-each (lambda (f)
                            (copy-file (string-append "doc/manual/" f)
                                       (string-append target "/" f)))
                          (scandir "doc/manual"
                                   (lambda (f)
                                     (or (string-suffix? ".png" f)
                                         (string-suffix? ".html" f))))))))
          ;; If sending mails is required, use proper mail program.
          (add-after 'install 'remove-smtp
            (lambda _
              (delete-file (string-append #$output "/sbin/smtp"))))
          ;; The configuration files and scripts are not really suitable for
          ;; Guix, and our service provides its own version anyway.  So delete
          ;; these to make sure `apcupsd' and `apctest' executed without any
          ;; arguments fail.  `apctest' actually segfaults, but only after
          ;; printing an error.
          (add-after 'install 'remove-etc-apcupsd
            (lambda _
              (delete-file-recursively
               (string-append #$output "/etc/apcupsd")))))))
    (native-inputs (list mandoc pkg-config python-docutils-0.19 util-linux))
    (inputs (list libusb libusb-compat))
    (home-page "http://www.apcupsd.org")
    (synopsis "Daemon for controlling APC UPSes")
    (description "@command{apcupsd} can be used for power management and
controlling most of @acronym{APC, American Power Conversion}’s @acronym{UPS,
Uninterruptible Power Supply} models.  @command{apcupsd} works with most of
APC’s Smart-UPS models as well as most simple signalling models such a
Back-UPS, and BackUPS-Office.")
    (license license:gpl2)))
