;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015-2016, 2018, 2024 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2016, 2018 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2020 Vincent Legoll <vincent.legoll@gmail.com>
;;; Copyright © 2022 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages dico)
  #:use-module (guix packages)
  #:use-module ((guix licenses) #:select (gpl3+))
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages python)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages gsasl)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages wordnet))

(define-public dico
  (package
    (name "dico")
    (version "2.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/dico/dico-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1xvahrav8aml90qcj4cj3a33y0n7nm1k0ywgks1zy2q91v2qk2vj"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags (list (string-append "--with-guile-site-dir=" %output
                                              "/share/guile/site/2.0")
                               "--disable-static")
       #:phases (modify-phases %standard-phases
                  (add-before 'check 'silence-guile
                    (lambda _
                      ;; Guile is too talkative, which disturbs the test
                      ;; infrastructure.  Gag it.
                      (setenv "GUILE_AUTO_COMPILE" "0")
                      (setenv "GUILE_WARN_DEPRECATED" "no"))))))
    (native-inputs (list groff))
    (inputs
     (list m4                           ;used at run time
           pcre
           python-wrapper
           guile-2.2
           gsasl
           readline
           zlib
           wordnet
           libxcrypt                              ;for 'crypt'
           libltdl))
    (home-page "https://www.gnu.org.ua/software/dico/")
    (synopsis "Implementation of DICT server (RFC 2229)")
    (description
     "GNU Dico implements a flexible dictionary server and client according to
RFC 2229 (DICT Server).  It is able to access any database available,
regardless of format, thanks to its modular structure.  New modules may be
written in C, Guile or Python.  Dico also includes a command-line client,
which may be used to query remote dictionary databases.")
    (license gpl3+)))
