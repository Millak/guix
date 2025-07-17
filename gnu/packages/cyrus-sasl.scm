;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2013 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages cyrus-sasl)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages dbm)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages tls)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public cyrus-sasl
  (package
    (name "cyrus-sasl")
    (version "2.1.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cyrusimap/cyrus-sasl"
                                  "/releases/download/cyrus-sasl-" version
                                  "/cyrus-sasl-" version ".tar.gz"))
              (sha256
               (base32
                "135kbgyfpa1mwqp5dm223yr6ddzi4vjm7cr414d7rmhys2mwdkvw"))))
    (build-system gnu-build-system)
    (inputs (list gdbm libxcrypt mit-krb5 openssl))
    (arguments
     (list
      #:configure-flags #~(list (string-append
                                 "CFLAGS=-g -O2"
                                 " -Wno-error=implicit-function-declaration")
                                (string-append "--with-plugindir="
                                               (assoc-ref %outputs "out")
                                               "/lib/sasl2")
                                ;; When cross-compiling the build system is
                                ;; unable to determine whether SPNEGO is
                                ;; supported; Kerberos does, so enable it.
                                #$@(if (%current-target-system)
                                       '("ac_cv_gssapi_supports_spnego=yes")
                                       '()))

      ;; The 'plugins' directory has shared source files, such as
      ;; 'plugin_common.c'.  When building the shared libraries there, libtool
      ;; ends up doing "ln -s plugin_common.lo plugin_common.o", which can
      ;; fail with EEXIST when building things in parallel.
      #:parallel-build? #f))
    (synopsis "Simple Authentication Security Layer implementation")
    (description
     "SASL (Simple Authentication Security Layer) is an Internet
standards-track method for remote computers to authenticate.  The Cyrus SASL
library makes supporting various SASL mechanisms easy for both client and
server writers.")
    (license (license:non-copyleft "file://COPYING"
                                   "See COPYING in the distribution."))
    (home-page "https://cyrusimap.org/sasl/")))
