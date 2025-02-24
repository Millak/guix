;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018, 2020, 2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020, 2022 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages authentication)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public libcotp
  (package
    (name "libcotp")
    (version "3.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/paolostivanin/libcotp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lw15blzzds3qq4ydhi6fsk189p1rbvzy5fzz4r3fv6wlmcyyprs"))))
    (build-system cmake-build-system)
    (arguments `(#:tests? #f))
    ;; TODO: tests:
    ;; Tests can be built with -DBUILD_TESTS=on.
    ;; Tests don't have a general `check` target so they have to be run manually.
    ;; Tests require `criterion`, which is not included in guix and has several
    ;; bundled dependencies.
    (inputs (list libgcrypt))
    (native-inputs (list pkg-config))
    (home-page "https://github.com/paolostivanin/libcotp")
    (synopsis "One-Time Passwords")
    (description "This package provides a library to generate
@acronym{HOTP, HMAC-base One-Time Password}s as specified in RFC 4226 and
@acronym{TOTP, Time-based One-Time Password}s as specified in RFC 6238.")
    (license license:asl2.0)))

(define-public oath-toolkit
  (package
    (name "oath-toolkit")
    (version "2.6.11")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://download.savannah.nongnu.org/releases/"
                           name "/" name "-" version ".tar.gz"))
       (sha256
        (base32 "1d1c3r1jhd72l5ppsfa9wvvm8kffzs4k2v2qn0xc9x26bd52llgw"))))
    (build-system gnu-build-system)
    (arguments
     ;; TODO ‘--enable-pskc’ causes xmlsec-related test suite failures.
     `(#:configure-flags
       (list "--enable-pam"
             "--enable-pskc"
             "--with-xmlsec-crypto-engine=openssl")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'delete-static-libraries
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (for-each delete-file (find-files lib "\\.a$"))))))))
    (native-inputs
     (list pkg-config
           ;; XXX: Perhaps this should be propagated from xmlsec.
           libltdl))
    (inputs
     (list linux-pam openssl xmlsec-openssl))
    (home-page "https://www.nongnu.org/oath-toolkit/")
    (synopsis "@acronym{OTP, one-time password} components")
    (description
     "The @acronym{OATH, Open AuTHentication} Toolkit provides various
components for building @acronym{OTP, One-Time Password} authentication systems:

@itemize
@item @command{oathtool}, a command-line tool for generating & validating OTPs.
@item @code{liboath}, a C library for OATH handling.
@item @command{pskctool}, a command-line tool for manipulating secret key
files in the @acronym{PSKC, Portable Symmetric Key Container} format
described in RFC6030.
@item @code{libpskc}, a shared and static C library for PSKC handling.
@item @code{pam_oath}, a PAM module for pluggable login authentication.
@end itemize

Supported technologies include the event-based @acronym{HOTP, Hash-based Message
Authentication Code One-Time Password} algorithm (RFC4226), the time-based
@acronym{TOTP, Time-based One-Time Password} algorithm (RFC6238), and
PSKC (RFC6030) to manage secret key data.")
    (license (list license:lgpl2.1+     ; the libraries (liboath/ & libpskc/)
                   license:gpl3+))))    ; the tools (everything else)

(define-public oauth2l
  (package
    (name "oauth2l")
    (version "1.3.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/oauth2l")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gm5pbgmz3p0zk5s4gvslp8ixhak3d35pfm7wrw5yk2rcdffr5li"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 (delete-file-recursively "vendor")))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/google/oauth2l"
      #:test-flags
      #~(list "-skip"
              ;; Tests requiring network access.
              (string-join
               (list "Test2LOFlow/curl._2lo"
                     "Test3LOFlow/curl._3lo"
                     "Test3LOFlow/fetch._3lo_cached"
                     "Test3LOLoopbackFlow/curl._3lo_loopback"
                     "Test3LOLoopbackFlow/fetch._3lo_loopback_cached"
                     "TestCLI/info._invalid_token"
                     "TestSSOFlow/fetch._sso"
                     "TestSSOFlow/fetch._sso._old_interface"
                     "TestServiceAccountImpersonationFlow/fetch._sso.*"
                     "TestStsFlow/fetch._2lo._sts"
                     "TestStsFlow/fetch._sso._sts")
               "|"))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "HOME" "/tmp"))))))
    (native-inputs
     (list go-github-com-google-uuid
           go-github-com-jessevdk-go-flags
           go-golang-org-x-oauth2))
    (home-page "https://github.com/google/oauth2l")
    (synopsis "Simple CLI for interacting with Google API authentication")
    (description
     "@code{oauth2l} (pronounced ``oauth tool'') is a simple command-line tool
for working with @url{https://developers.google.com/identity/protocols/OAuth2,
Google OAuth 2.0} written in Go.  Its primary use is to fetch and print OAuth
2.0 access tokens, which can be used with other command-line tools and
scripts.")
    (license license:asl2.0)))

(define-public yubico-pam
  (package
    (name "yubico-pam")
    (version "2.27")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Yubico/yubico-pam")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hb773zlf11xz4bwmsqv2mq5d4aq2g0crdr5cp9xwc4ivi5gd4kg"))))
    (build-system gnu-build-system)
    (arguments
     ;; The pam_test fails because ykclient fails to build a Curl handle.
     '(#:make-flags '("TESTS=util_test")))
    (inputs
     (list linux-pam libyubikey ykclient yubikey-personalization))
    (native-inputs
     (list autoconf automake libtool asciidoc pkg-config))
    (home-page "https://developers.yubico.com/yubico-pam")
    (synopsis "Yubico pluggable authentication module")
    (description "The Yubico PAM module provides an easy way to integrate the
YubiKey into your existing user authentication infrastructure.")
    (license license:bsd-2)))

(define-public pamtester
  (package
    (name "pamtester")
    (version "0.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://sourceforge/pamtester/pamtester/"
             version "/pamtester-" version ".tar.gz"))
       (sha256
        (base32 "1mdj1wj0adcnx354fs17928yn2xfr1hj5mfraq282dagi873sqw3"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list linux-pam))
    (home-page "https://pamtester.sourceforge.net/")
    (synopsis "Utility for testing pluggable authentication modules (PAM) facility")
    (description
     "Pamtester is a tiny utility program to test the pluggable authentication
modules (PAM) facility, specifically designed to help PAM module authors to
intensively test their own modules.")
    (license license:bsd-3)))
