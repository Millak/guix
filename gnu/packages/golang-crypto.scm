;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2020 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2020, 2022 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2021 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2021 BonfaceKilz <me@bonfacemunyoki.com>
;;; Copyright © 2021 Collin J. Doering <collin@rekahsoft.ca>
;;; Copyright © 2021 LibreMiami <packaging-guix@libremiami.org>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2021 Sarah Morgensen <iskarian@mgsn.dev>
;;; Copyright © 2021 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2024 Jean Simard <woshilapin@tuziwo.info>
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

(define-module (gnu packages golang-crypto)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system go)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages password-utils)
  #:use-module (gnu packages specifications))

;;; Commentary:
;;;
;;; Golang modules (libraries) related to Cryptography: encryption algorithms,
;;; hashing functions, TLS, key management, digital signatures, password
;;; hashing etc.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

;;;
;;; Libraries:
;;;

(define-public go-c2sp-org-cctv-age
  (package
    (name "go-c2sp-org-cctv-age")
    (version "0.0.0-20240306222714-3ec4d716e805")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/C2SP/CCTV")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00bk05ca94lm3b029ycwj0krmg2gfjv1c3pc7dvq9gmwwzr564v5"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Sub folders containing different projects with their own
            ;; licenses.
            (for-each delete-file-recursively
                      (list "ML-KEM" "RFC6979" "ed25519" "jq255"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "c2sp.org/CCTV/age"
      #:unpack-path "c2sp.org/CCTV"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://c2sp.org/CCTV/age")
    (synopsis "Community Cryptography Test Vectors")
    (description
     "This package provides a large set of test vectors for the age file
encryption format, as well as a framework to easily generate them.

The test suite can be applied to any age implementation, regardless of the
language it's implemented in, and the level of abstraction of its
interface.  For the simplest, most universal integration, the implementation
can just attempt to decrypt the test files, check the operation only succeeds
if expect is success, and compare the decrypted payload.  Test vectors
involving unimplemented features (such as passphrase encryption or armoring)
can be ignored.")
    ;; age/internal/LICENSE: Redistribution and use in source and binary
    ;; forms, with or without modification, are permitted provided that the
    ;; following conditions are met
    ;;
    ;; age/README: The vectors in the testdata folder are available under the
    ;; terms of the Zero-Clause BSD (reproduced below), CC0 1.0, or Unlicense
    ;; license, to your choice.
    (license license:cc0)))

(define-public go-filippo-io-age
  (package
    (name "go-filippo-io-age")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/age")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1dms32lxqgjipmlisng7dmy1sdw0qscj43x9lmpadyzbzc64lhrv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "filippo.io/age"
      #:phases
      #~(modify-phases %standard-phases
          ;; FIXME: src/c2sp.org/CCTV/age/age.go:13:12: pattern testdata:
          ;; cannot embed directory testdata: contains no embeddable files
          ;;
          ;; This happens due to Golang can't determine the valid directory of
          ;; the module which is sourced during setup environment phase, but
          ;; easy resolved after coping to expected directory "vendor" within
          ;; the current package, see details in Golang source:
          ;;
          ;; - URL: <https://github.com/golang/go/blob/>
          ;; - commit: 82c14346d89ec0eeca114f9ca0e88516b2cda454
          ;; - file: src/cmd/go/internal/load/pkg.go#L2059
          (add-before 'build 'copy-input-to-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (mkdir "vendor")
                (copy-recursively
                 (string-append
                  #$(this-package-native-input "go-c2sp-org-cctv-age")
                  "/src/c2sp.org")
                 "vendor/c2sp.org"))))
          (add-before 'install 'remove-vendor-directory
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "vendor")))))))
    (native-inputs
     (list go-c2sp-org-cctv-age
           go-github-com-rogpeppe-go-internal))
    (propagated-inputs
     (list go-golang-org-x-sys
           go-golang-org-x-term
           go-golang-org-x-crypto
           go-filippo-io-edwards25519))
    (home-page "https://filippo.io/age")
    (synopsis "Secure file encryption tool, format, and Go library")
    (description
     "This package implements file encryption according to the
@{age-encryption.org/v1, https://age-encryption.org/v1} specification.
It features small explicit keys, no configuration options, and Unix-style
composability.")
    (license license:bsd-3)))

(define-public go-filippo-io-edwards25519
  (package
    (name "go-filippo-io-edwards25519")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/edwards25519")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01m8hpaj0cwp250f7b0din09cf8j6j5y631grx67qfhvfrmwr1zr"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "filippo.io/edwards25519"))
    (home-page "https://filippo.io/edwards25519")
    (synopsis "Group logic for the twisted Edwards curve")
    (description "This package implements the edwards25519 elliptic curve in
Go, exposing the necessary APIs to build a wide array of higher-level
primitives.")
    (license license:bsd-3)))

(define-public go-github-com-99designs-keyring
  (package
    (name "go-github-com-99designs-keyring")
    (version "1.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/99designs/keyring")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mkvy7scyq07rkqhabfmkd8imcm4h9y7zj9palj04znpihpixa5m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/99designs/keyring"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-failing-tests
            (lambda* (#:key tests? unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (substitute* (find-files "." "\\_test.go$")
                  ;; Disable test requring running DBus.
                  (("TestLibSecretKeysWhenEmpty")
                   "OffTestLibSecretKeysWhenEmpty")
                  (("TestLibSecretKeysWhenNotEmpty")
                   "OffTestLibSecretKeysWhenNotEmpty")
                  (("TestLibSecretGetWhenEmpty")
                   "OffTestLibSecretGetWhenEmpty")
                  (("TestLibSecretGetWhenNotEmpty")
                   "OffTestLibSecretGetWhenNotEmpty")
                  (("TestLibSecretRemoveWhenEmpty")
                   "OffTestLibSecretRemoveWhenEmpty")
                  (("TestLibSecretRemoveWhenNotEmpty")
                   "OffTestLibSecretRemoveWhenNotEmpty"))))))))
    (native-inputs
     (list gnupg go-github-com-stretchr-testify password-store))
    (propagated-inputs
     (list go-github-com-dvsekhvalnov-jose2go
           go-github-com-godbus-dbus
           go-github-com-gsterjov-go-libsecret
           go-github-com-mitchellh-go-homedir
           go-github-com-mtibben-percent
           go-golang-org-x-sys
           go-golang-org-x-term))
    (home-page "https://github.com/99designs/keyring")
    (synopsis "Go library providing a uniform interface for various secure
credential stores")
    (description
     "Keyring provides utility functions for and a common interface to a range
of secure credential storage services.  Originally developed as part of AWS
Vault, a command line tool for securely managing AWS access from developer
workstations.

Currently Keyring supports the following backends: macOS/OSX Keychain, Windows
pcredential store, Pass, Secret Service, KDE Wallet, Encrypted File.")
    (license license:expat)))

(define-public go-github-com-aead-chacha20
  (let ((commit "8b13a72661dae6e9e5dea04f344f0dc95ea29547")
        (revision "0"))
    (package
      (name "go-github-com-aead-chacha20")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aead/chacha20")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0gbmgq5kbqmbyrsav57ql4jzbvqvp1q7yvcd5fl3wf5g94iyv56r"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/aead/chacha20"))
      (propagated-inputs
       (list go-golang-org-x-sys))
      (home-page "https://github.com/aead/chacha20")
      (synopsis "ChaCha20 and XChaCha20 stream ciphers")
      (description "ChaCha is a stream cipher family created by Daniel
Bernstein.  The most common ChaCha variant is ChaCha20 (20 rounds).  ChaCha20
is standardized in RFC 7539.")
      (license license:expat))))

(define-public go-github-com-aperturerobotics-jacobsa-crypto
  (let ((commit "b1eb679742a8deed015a4406384eea6bd985d08a")
        (revision "0"))
    (package
      (name "go-github-com-aperturerobotics-jacobsa-crypto")
      (version (git-version "1.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/aperturerobotics/jacobsa-crypto")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "16dxigj8m6q18xqsy72iq287rh4fw0y0b9yqlw0qkclb8379n1z2"))))
      (build-system go-build-system)
      (arguments
       (list #:import-path "github.com/aperturerobotics/jacobsa-crypto"
             ;; Source-only package.
             #:tests? #f
             #:phases
             #~(modify-phases %standard-phases
                 ;; Source-only package.
                 (delete 'build))))
      (home-page "https://github.com/aperturerobotics/jacobsa-crypto")
      (synopsis "Cryptography missing from the Go standard library")
      (description "This repository contains Go packages related to
cryptographic standards that are not included in the Go standard library.")
      (license license:asl2.0))))

(define-public go-github-com-btcsuite-btcd-btcec
  (let ((commit "67e573d211ace594f1366b4ce9d39726c4b19bd0")
        (revision "0"))
    (package
      (name "go-github-com-btcsuite-btcd-btcec")
      (version (git-version "0.12.0-beta" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/btcsuite/btcd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "04s92gsy71w1jirlr5lkk9y6r5cparbas7nmf6ywbp7kq7fn8ajn"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/btcsuite/btcd"
         #:import-path "github.com/btcsuite/btcd/btcec"))
      (native-inputs
       (list go-github-com-davecgh-go-spew))
      (home-page "https://github.com/btcsuite/btcd")
      (synopsis "Elliptic curve cryptography to work with Bitcoin")
      (description "Package @command{btcec} implements elliptic curve
cryptography needed for working with Bitcoin (secp256k1 only for now).  It is
designed so that it may be used with the standard crypto/ecdsa packages
provided with Go.  A comprehensive suite of test is provided to ensure proper
functionality.  Package @command{btcec} was originally based on work from
ThePiachu which is licensed under the same terms as Go, but it has
significantly diverged since then.  The @command{btcsuite} developers original
is licensed under the liberal ISC license.

Although this package was primarily written for btcd, it has intentionally
been designed so it can be used as a standalone package for any projects
needing to use secp256k1 elliptic curve cryptography.")
      (license license:isc))))

(define-public go-github-com-bwesterb-go-ristretto
  (package
    (name "go-github-com-bwesterb-go-ristretto")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bwesterb/go-ristretto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h508v790wk6g8jq0gh18296xl87vmgc4fhwnac7mk6i5g3mz6v4"))))
    (build-system go-build-system)
    (arguments
     (list #:unpack-path "github.com/bwesterb/go-ristretto"
           #:import-path "github.com/bwesterb/go-ristretto/edwards25519"))
    (home-page "https://github.com/bwesterb/go-ristretto")
    (synopsis "Operations on the Ristretto prime-order group")
    (description "This is a pure Go implementation of the group operations on
the Ristretto prime-order group built from Edwards25519.")
    (license license:expat)))

(define-public go-github-com-cespare-xxhash
  (package
    (name "go-github-com-cespare-xxhash")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cespare/xxhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qyzlcdcayavfazvi03izx83fvip8h36kis44zr2sg7xf6sx6l4x"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cespare/xxhash"))
    (propagated-inputs
     (list go-github-com-spaolacci-murmur3
           go-github-com-oneofone-xxhash))
    (home-page "https://github.com/cespare/xxhash")
    (synopsis "Go implementation of xxHash")
    (description
     "Package xxhash implements the 64-bit variant of @code{xxHash} (XXH64) as
described at @url{https://xxhash.com/}.")
    (license license:expat)))

(define-public go-github-com-cespare-xxhash-v2
  (package
    (inherit go-github-com-cespare-xxhash)
    (name "go-github-com-cespare-xxhash-v2")
    (version "2.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cespare/xxhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1f3wyr9msnnz94szrkmnfps9wm40s5sp9i4ak0kl92zcrkmpy29a"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "xxhashbench"))))
    (arguments
     (list
      #:import-path "github.com/cespare/xxhash/v2"
      #:phases
      #~(modify-phases %standard-phases
          (replace 'check
            (lambda* (#:key inputs #:allow-other-keys #:rest args)
              (unless
                  ;; The tests fail when run with gccgo.
                  (false-if-exception (search-input-file inputs "/bin/gccgo"))
                (apply (assoc-ref %standard-phases 'check) args)))))))
    (propagated-inputs '())))

(define-public go-github-com-chmduquesne-rollinghash
  (let ((commit "9a5199be7309f50c496efc87d29bd08788605ae7")
        (revision "1"))
    (package
      (name "go-github-com-chmduquesne-rollinghash")
      (version (git-version "4.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/chmduquesne/rollinghash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1gkdgsgrmwagwyp4lmd4k11mbwi8f1yw9c9rhnkmav87gy1k84jr"))))
      (build-system go-build-system)
      (arguments
       (list
        #:import-path "github.com/chmduquesne/rollinghash/"
        #:phases
        #~(modify-phases %standard-phases
            ;; XXX: Run all tests, workaround for go-build-system's lack of Go
            ;; modules support.
            (replace 'check
              (lambda* (#:key tests? import-path #:allow-other-keys)
                (when tests?
                  (with-directory-excursion (string-append "src/" import-path)
                    (invoke "go" "test" "-v" "./..."))))))))
      (propagated-inputs
       (list go-code-cloudfoundry-org-bytefmt))
      (home-page "https://github.com/chmduquesne/rollinghash")
      (synopsis "Rolling hashes in Go")
      (description
       "This package provides a Go implementation of several rolling hashes.")
      (license license:expat))))

(define-public go-github-com-cloudflare-circl
  (package
    (name "go-github-com-cloudflare-circl")
    (version "1.3.6")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudflare/circl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hk5svprcjrj6k4mg4kd732pnb658llqv04z6xrcl5v77jda2kd"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/cloudflare/circl"))
    (propagated-inputs
     (list go-github-com-bwesterb-go-ristretto
           go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://blog.cloudflare.com/introducing-circl")
    (synopsis "Cloudflare Interoperable Reusable Cryptographic Library")
    (description "CIRCL (Cloudflare Interoperable, Reusable Cryptographic
Library) is a collection of cryptographic primitives written in Go.  The goal
of this library is to be used as a tool for experimental deployment of
cryptographic algorithms targeting Post-Quantum (PQ) and Elliptic Curve
Cryptography (ECC).")
    (license license:bsd-3)))

(define-public go-github-com-dvsekhvalnov-jose2go
  (package
    (name "go-github-com-dvsekhvalnov-jose2go")
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dvsekhvalnov/jose2go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pzfmv2dxb3m455bi1ks4q3i0dcw1sazxk8k96wrgpkwgglyxj3n"))))
    (build-system go-build-system)
    (native-inputs
     (list go-gopkg-in-check-v1))
    (arguments
     '(#:import-path "github.com/dvsekhvalnov/jose2go"))
    (home-page "https://github.com/dvsekhvalnov/jose2go")
    (synopsis "Go implementation of Javascript Object Signing and Encryption spec")
    (description "This package provides a Go library for generating, decoding,
and encrypting JSON Web Tokens (JWT).  It relies only on the standard
library.")
    (license license:expat)))

(define-public go-github-com-emersion-go-bcrypt
  (package
    (name "go-github-com-emersion-go-bcrypt")
    (version "0.0.0-20170822072041-6e724a1baa63")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-bcrypt")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pchrgs05w30iqbh4d6iys4wvlyajsdwchp5mkf59amgsbyjaqgm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-bcrypt"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/emersion/go-bcrypt")
    (synopsis "Extract of bcrypt from golang.org/x/crypto/bcrypt")
    (description
     "This package provides an extract @code{bcrypt} from
@code{golang.org/x/crypto/bcrypt}.")
    (license license:bsd-3)))

(define-public go-github-com-emersion-go-pgpmail
  (package
    (name "go-github-com-emersion-go-pgpmail")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-pgpmail")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fiqpdwxnfba2cgij7j83dfqc0zz4mq95x15wicgm5f3vjr1xg5h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/emersion/go-pgpmail"))
    (propagated-inputs
     (list go-github-com-emersion-go-message
           go-github-com-protonmail-go-crypto
           go-golang-org-x-text))
    (home-page "https://github.com/emersion/go-pgpmail")
    (synopsis "PGP mail encryption for Go")
    (description "The pgpmail package implements PGP encryption for e-mail
messages.")
    (license license:expat)))

(define-public go-github-com-flynn-noise
  (package
    (name "go-github-com-flynn-noise")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/flynn/noise")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j6phxyqx06wcqxjpin696fkp85s76qcp3i2f7fv6q2fb6618f6y"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/flynn/noise"))
    (propagated-inputs
     (list go-gopkg-in-check-v1 go-golang-org-x-crypto))
    (home-page "https://github.com/flynn/noise")
    (synopsis "Go implementation of the Noise protocol framework")
    (description "@code{noise} implements the Noise protocol framework.  Noise
is a low-level framework for building crypto protocols.  Noise protocols
support mutual and optional authentication, identity hiding, forward secrecy,
zero round-trip encryption, and other advanced features.")
    (license license:bsd-3)))

(define-public go-github-com-gaukas-godicttls
  (package
    (name "go-github-com-gaukas-godicttls")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gaukas/godicttls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n9i0b9nbwq7ms36r34kfc346prrif78hhp55gmbkvlgvsc3m2af"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/gaukas/godicttls"))
    (home-page "https://github.com/gaukas/godicttls")
    (synopsis "Dictionary for TLS")
    (description "This package provides a dictionary for TLS written in Go
providing bidirectional mapping values to their names, plus enum convenience
for values.")
    (license license:bsd-3)))

(define-public go-github-com-go-asn1-ber-asn1-ber
  (package
    (name "go-github-com-go-asn1-ber-asn1-ber")
    (version "1.5.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/go-asn1-ber/asn1-ber")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0xa1s1q2is9fr02pvrc9sq8zfq9ba6gk64yg1ncglppp30f50q52"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/go-asn1-ber/asn1-ber"))
    (home-page "https://github.com/go-asn1-ber/asn1-ber")
    (synopsis "ASN.1 BER encoding and decoding in Go")
    (description
     "This package provides @acronym{Abstract Syntax Notation One, ASN.1} BER
encoding and decoding in the Go language.")
    (license license:expat)))

(define-public go-github-com-golang-jwt-jwt
  (package
    (name "go-github-com-golang-jwt-jwt")
    (version "3.2.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang-jwt/jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hq8wz11g6kddx9ab0icl5h3k4lrivk1ixappnr5db2ng2wjks9c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/golang-jwt/jwt"))
    (home-page "https://github.com/golang-jwt/jwt")
    (synopsis "Go implementation of JSON Web Tokens")
    (description
     "This package provides a Go implementation of
@url{https://datatracker.ietf.org/doc/html/rfc7519, JSON Web Tokens} and
supports the parsing and verification as well as the generation and signing of
JSON Web Tokens.  The currently supported signing algorithms are HMAC SHA,
RSA, RSA-PSS, and ECDSA, though hooks are present for adding your own.")
    (license license:expat)))

(define-public go-github-com-golang-jwt-jwt-v4
  (package
    (inherit go-github-com-golang-jwt-jwt)
    (name "go-github-com-golang-jwt-jwt-v4")
    (version "4.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang-jwt/jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m7c9lwlmd0lnn0hyby1rb3f4nwn4xcjgca218frj0hi0krqn8kp"))))
    (arguments
     (list
      #:import-path "github.com/golang-jwt/jwt/v4"))))

(define-public go-github-com-golang-jwt-jwt-v5
  (package
    (inherit go-github-com-golang-jwt-jwt-v4)
    (name "go-github-com-golang-jwt-jwt-v5")
    (version "5.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang-jwt/jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0px12zhdmzqjj5zlcr136rcsilpmi4chiz6arxv49q372j4nhmia"))))
    (arguments
     (list
      #:import-path "github.com/golang-jwt/jwt/v5"))))

(define-public go-github-com-google-go-tpm
  (package
    (name "go-github-com-google-go-tpm")
    (version "0.9.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/google/go-tpm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c5j5cvwl45ka93nknmv454ivd7kp9n8yql19gr6z01z0s1ph7sg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:import-path "github.com/google/go-tpm"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Break cycle:
          ;; github.com/google/go-tpm/tpm2/transport/simulator/simulator.go ->
          ;; github.com/google/go-tpm-tools -> github.com/google/go-tpm.
          ;; Consider to add required inputs on dependent package.
          (delete 'build)
          (delete 'check))))
    (home-page "https://github.com/google/go-tpm")
    (synopsis "Go-TPM Legacy TPM 2.0 library")
    (description
     "This package provides a functionality to communicate directly with a
@acronym{Trusted Platform Module, TPM} device.  The libraries don't implement
the entire spec for neither 1.2 nor 2.0.

Included submodules:
@itemize
@item @code{tpm} - TPM 1.2 client library
@item @code{tpm2} - TPM 2.0 client library.
@item @code{direct} - the prototype \"TPMDirect\" TPM 2.0 API, which is
intended to (eventually) be 1:1 with the TPM 2.0 spec
@end itemize")
    (license license:asl2.0)))

;; It's not public for purpose, as it contains a lot of golang modules which
;; may be inherited from the single source, but the package itself does not
;; have to be installed directly or linked to other packages..
(define go-github-com-gxed-hashland
  (package
    (name "go-github-com-gxed-hashland")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gxed/hashland")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b921dh9i6zw7y8jfzwvrmdbhnwid12a5z1zjawslfq2vvsajwmm"))))
    (build-system go-build-system)
    ;; Source-only package.
    (arguments
     (list
      #:tests? #f
      #:import-path "github.com/gxed/hashland"
      #:phases
      #~(modify-phases %standard-phases (delete 'build))))
    (home-page "https://github.com/gxed/hashland")
    (synopsis "Collection of hash functions")
    (description
     "This package provides a source of Hashland - a collection of hash
functions and functionality to test them.  It aggregates various Golang
libraries.")
    (license license:expat)))

;; TODO: Inherit from the go-github-com-gxed-hashland
(define-public go-github-com-gxed-hashland-keccakpg
  (let ((commit "d9f6b97f8db22dd1e090fd0bbbe98f09cc7dd0a8")
        (revision "0"))
    (package
      (name "go-github-com-gxed-hashland-keccakpg")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/gxed/hashland")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1q23y4lacsz46k9gmgfw4iwwydw36j2601rbidmmswl94grpc386"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "github.com/gxed/hashland"
         #:import-path "github.com/gxed/hashland/keccakpg"))
      (home-page "https://github.com/gxed/hashland")
      (synopsis "Implements the Keccak (SHA-3) hash algorithm in Go")
      (description "Package @command{keccak} implements the Keccak (SHA-3)
hash algorithm.  See http://keccak.noekeon.org.")
      (license license:expat))))

(define-public go-github-com-gxed-hashland-murmur3
  (package
    (inherit go-github-com-gxed-hashland)
    (name "go-github-com-gxed-hashland-murmur3")
    (arguments
     (list
      #:import-path "github.com/gxed/hashland/murmur3"
      #:unpack-path "github.com/gxed/hashland"))
    (synopsis "Golang implementation of MurmurHash3 algorithm")
    (description
     "This package provides a native Go implementation of
@url{https://en.wikipedia.org/wiki/MurmurHash, Austin Appleby's third
MurmurHash} revision (aka MurmurHash3).")))

(define-public go-github-com-jcmturner-aescts-v2
  (package
    (name "go-github-com-jcmturner-aescts-v2")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jcmturner/aescts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yrdiisdhcqfs8jpicc30dfmbqzxhkmbayn902xrgwkndky8w7l1"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/jcmturner/aescts/v2"))
    (propagated-inputs (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jcmturner/aescts")
    (synopsis "Encrypt and decrypt data in Go using AES CipherText Stealing")
    (description "This package provides AES Cipher Block Chaining CipherText
Stealing encryption and decryption methods.")
    (license license:asl2.0)))

(define-public go-github-com-libp2p-go-libp2p-crypto
  (let ((commit "7240b40a3ddc47c4d17c15baabcbe45e5219171b")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-crypto")
      (version (git-version "2.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-crypto")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0qwpy57qv5143l9dlfwfvpqsxdd2i4zwnawx1w4pmgxxim3nw1wb"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-crypto"))
      (native-inputs
       (list go-github-com-btcsuite-btcd-btcec
             go-github-com-gogo-protobuf
             go-github-com-minio-sha256-simd
             go-golang-org-x-crypto))
      (home-page
       "https://github.com/libp2p/go-libp2p-crypto")
      (synopsis "Various cryptographic utilities used by IPFS")
      (description "Various cryptographic utilities used by IPFS")
      (license license:expat))))

(define-public go-github-com-libp2p-go-libp2p-peer
  (let ((commit "993d742bc29dcf4894b7730ba610fd78900be76c")
        (revision "0"))
    (package
      (name "go-github-com-libp2p-go-libp2p-peer")
      (version (git-version "2.3.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/libp2p/go-libp2p-peer")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1h96qjdi0i1wbr0jliap2903mycphas3ny0zdrm77yca9plcnphh"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/libp2p/go-libp2p-peer"))
      (propagated-inputs
       (list go-github-com-btcsuite-btcd-btcec
             go-github-com-gogo-protobuf
             go-github-com-libp2p-go-libp2p-crypto
             go-github-com-multiformats-go-multihash))
      (home-page "https://github.com/libp2p/go-libp2p-peer")
      (synopsis "PKI based identities for use in go-libp2p")
      (description "PKI based identities for use in @command{go-libp2p}.")
      (license license:expat))))

(define-public go-github-com-marten-seemann-chacha20
  (package
    (name "go-github-com-marten-seemann-chacha20")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marten-seemann/chacha20")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x1j4cvbap45zk962qkjalc1h3axhzzdy9cdzhcjmprmm1ql4gjm"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/marten-seemann/chacha20"))
    (home-page "https://github.com/marten-seemann/chacha20")
    (synopsis "ChaCha20 in Go")
    (description "This package is an external copy of the Go standard
library's internal ChaCha20 package.")
    (license license:bsd-3)))

(define-public go-github-com-marten-seemann-qtls
  (package
    (name "go-github-com-marten-seemann-qtls")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/marten-seemann/qtls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dz60y98nm7l70hamq0v2vrs2dspyr5yqhnrds2dfh7hchxvq76j"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/marten-seemann/qtls"
       ;; The test suite requires networking.
       #:tests? #f))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/marten-seemann/qtls")
    (synopsis "TLS 1.3 with QUIC in Go")
    (description "This package provides @code{qtls}, a QUIC-capable variant of
the Go standard library's TLS 1.3 implementation.")
    (license license:bsd-3)))

(define-public go-github-com-nats-io-jwt-v2
  (package
    (name "go-github-com-nats-io-jwt-v2")
    (version "2.5.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wcqbfyd3b4qdspmf72cpsbi0y2a4b1qd0cv3qvhh17d1h1a6zib"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/nats-io/jwt/v2"
           #:unpack-path "github.com/nats-io/jwt"))
    (propagated-inputs (list go-github-com-nats-io-nkeys))
    (home-page "https://github.com/nats-io/jwt")
    (synopsis "Go library signing JWT tokens with NKeys for the NATS ecosystem")
    (description
     "This library is a JWT implementation that uses nkeys to digitally sign
JWT tokens.  Nkeys use Ed25519 to provide authentication of JWT claims.")
    (license license:asl2.0)))

(define-public go-github-com-nats-io-nkeys
  (package
    (name "go-github-com-nats-io-nkeys")
    (version "0.4.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/nats-io/nkeys")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0779m4nn6n0ql23wnk50ybddslvb84mwx036gf7yw6ckmm4yybxs"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/nats-io/nkeys"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/nats-io/nkeys")
    (synopsis "Go library implementing public-key system for NATS ecosystem")
    (description
     "This package is an Ed25519 based public-key signature system that
simplifies keys and seeds and performs signing and verification.")
    (license license:asl2.0)))

(define-public go-github-com-minio-blake2b-simd
  (let ((commit "3f5f724cb5b182a5c278d6d3d55b40e7f8c2efb4")
        (revision "0"))
    (package
      (name "go-github-com-minio-blake2b-simd")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/minio/blake2b-simd")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0b6jbnj62c0gmmfd4zdmh8xbg01p80f13yygir9xprqkzk6fikmd"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/minio/blake2b-simd"))
      (home-page "https://github.com/minio/blake2b-simd")
      (synopsis "Fast hashing in pure Go of BLAKE2b with SIMD instructions")
      (description "This package was initially based on the pure go BLAKE2b
implementation of Dmitry Chestnykh and merged with the (cgo dependent) AVX
optimized BLAKE2 implementation (which in turn is based on the official
implementation.  It does so by using Go's Assembler for amd64 architectures
with a golang only fallback for other architectures.

In addition to AVX there is also support for AVX2 as well as SSE.  Best
performance is obtained with AVX2 which gives roughly a 4X performance
increase approaching hashing speeds of 1GB/sec on a single core.")
      (license license:asl2.0))))

(define-public go-github-com-minio-highwayhash
  (package
    (name "go-github-com-minio-highwayhash")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/highwayhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1inrix7720273ccynxcyi7xsgc55cskxrw7gwn08qkmdj9xdxqai"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/minio/highwayhash"))
    (propagated-inputs (list go-golang-org-x-sys))
    (home-page "https://github.com/minio/highwayhash")
    (synopsis "HighwayHash library for Go")
    (description
     "This package implements the pseudo-random-function (PRF) HighwayHash.
HighwayHash is a fast hash function designed to defend hash-flooding attacks
or to authenticate short-lived messages.")
    (license license:asl2.0)))

(define-public go-github-com-minio-sha256-simd
  (package
    (name "go-github-com-minio-sha256-simd")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/sha256-simd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1j0iqsckm97g4l79vd4mc7apbmkdar23jpzqpnpdhwpfd834j8lp"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/minio/sha256-simd"))
    (home-page "https://github.com/minio/sha256-simd")
    (synopsis "Accelerate SHA256 computations in pure Go")
    (description "Accelerate SHA256 computations in pure Go using AVX512 and
AVX2 for Intel and ARM64 for ARM.  On AVX512 it provides an up to 8x
improvement (over 3 GB/s per core) in comparison to AVX2.

This package is designed as a replacement for @command{crypto/sha256}.  For
Intel CPUs it has two flavors for AVX512 and AVX2 (AVX/SSE are also
supported).  For ARM CPUs with the Cryptography Extensions, advantage is taken
of the SHA2 instructions resulting in a massive performance improvement.

This package uses Golang assembly.  The AVX512 version is based on the Intel's
\"multi-buffer crypto library for IPSec\" whereas the other Intel
implementations are described in \"Fast SHA-256 Implementations on Intel
Architecture Processors\" by J. Guilford et al.")
    (license license:asl2.0)))

(define-public go-github-com-multiformats-go-multihash
  (package
    (name "go-github-com-multiformats-go-multihash")
    (version "0.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/multiformats/go-multihash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ydh94083888xl2r4d1grzgqf3c818mkmdpj008jkh6h7m56wc4w"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/multiformats/go-multihash"
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'copy-multibase-specs
                 (lambda* (#:key import-path #:allow-other-keys)
                   (copy-recursively
                    (string-append #$(this-package-native-input
                                      "specification-multihash")
                                   "/share/multihash/")
                    (string-append "src/" import-path "/spec/multihash/"))
                   (copy-recursively
                    (string-append #$(this-package-native-input
                                      "specification-multicodec")
                                   "/share/multicodec/")
                    (string-append "src/" import-path "/spec/multicodec/")))))))
    (native-inputs
     (list specification-multihash
           specification-multicodec))
    (propagated-inputs
     (list go-github-com-minio-sha256-simd
           go-github-com-mr-tron-base58
           go-github-com-multiformats-go-varint
           go-github-com-spaolacci-murmur3
           go-golang-org-x-crypto
           go-lukechampine-com-blake3))
    (home-page "https://github.com/multiformats/go-multihash")
    (synopsis "Multihash implementation in Go")
    (description "Multihash implementation in Go.")
    (license license:expat)))

(define-public go-github-com-oneofone-xxhash
  (package
    (name "go-github-com-oneofone-xxhash")
    (version "1.2.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OneOfOne/xxhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f98qk83l2fhpclvrgyxsa9b8m4pipf11fah85bnjl01wy4lvybw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/OneOfOne/xxhash"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-benchmarks
            (lambda* (#:key import-path #:allow-other-keys)
              (delete-file-recursively
               (string-append "src/" import-path "/benchmarks")))))))
    (home-page "https://github.com/OneOfOne/xxhash")
    (synopsis "Go implementation of xxHash")
    (description
     "This is a native Go implementation of the
@url{https://github.com/Cyan4973/xxHash, xxHash} algorithm, an extremely fast
non-cryptographic hash algorithm, working at speeds close to RAM limits.")
    (license license:asl2.0)))

(define-public go-github-com-operatorfoundation-ed25519
  (let ((commit "b22b4bd3ddef042eec45f3ee135cd40281fde2b4")
        (revision "0"))
    (package
      (name "go-github-com-operatorfoundation-ed25519")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/OperatorFoundation/ed25519")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0xrzqrjlghkgd1cy5rj4khryn4f59vas2vzrxc6d8jpj5ijf3xkv"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/OperatorFoundation/ed25519"
         #:phases
         (modify-phases %standard-phases
           (add-before 'install 'remove-test-data
             (lambda* (#:key import-path #:allow-other-keys)
               (delete-file-recursively
                (string-append "src/" import-path "/testdata"))
               #t)))))
      (native-inputs
       (list go-golang-org-x-crypto))
      (home-page "https://github.com/OperatorFoundation/ed25519")
      (synopsis "Ed25519 for go")
      (description "Package ed25519 implements the Ed25519 signature
algorithm.")
      (license license:bsd-3))))

(define-public go-github-com-pion-randutil
  (package
    (name "go-github-com-pion-randutil")
    (version "v0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pion/randutil")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "098isjyvyb8jhrrr57xi45g5m35vb1l92dm5wcy7g2q9x55lvxg5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pion/randutil"))
    (home-page "https://github.com/pion/randutil")
    (synopsis "Helper library for cryptographic and mathmatical randoms")
    (description
     "This package provides primitives for generating random values.")
    (license license:expat)))

(define-public go-github-com-protonmail-go-crypto
  (package
    (name "go-github-com-protonmail-go-crypto")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/go-crypto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "11q94983r6zjrdvflpikms4773a9s5vb9gg4qw1rj5800yhhah0n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/go-crypto"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Workaround for go-build-system's lack of Go modules
          ;; support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-github-com-cloudflare-circl
           go-golang-org-x-crypto))
    (home-page "https://github.com/ProtonMail/go-crypto")
    (synopsis "Fork of x/crypto with up-to-date OpenPGP implementation")
    (description
     "This package provides cryptography for Go.  This version of the package
is a fork that adds a more up-to-date OpenPGP implementation.  It is
completely backwards compatible with @code{golang.org/x/crypto}, the official
package.")
    (license license:bsd-3)))

(define-public go-github-com-quic-go-qtls-go1-20
  (package
    (name "go-github-com-quic-go-qtls-go1-20")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/qtls-go1-20")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "069rknxpg7d0dmxc4akq2mw7wm5bi0420nshykf2iclvmbcg9ajh"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: panic: qtls.ClientSessionState doesn't match, with Golang 1.20+.
      #:go go-1.20
      #:import-path "github.com/quic-go/qtls-go1-20"))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://github.com/quic-go/qtls-go1-20")
    (synopsis "TLS 1.3 for QUIC")
    (description "Go standard library TLS 1.3 implementation, modified for
QUIC.  For Go 1.20.")
    (license license:expat)))

(define-public go-github-com-refraction-networking-utls
  (package
    (name "go-github-com-refraction-networking-utls")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/refraction-networking/utls")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1iywar5vqsml4b177k2nkcxmjm8mw92g3p112yjsrpmikiwpwpyw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/refraction-networking/utls"
       #:tests? #f))                    ;requires internet access
    (propagated-inputs
     (list go-github-com-andybalholm-brotli
           go-github-com-cloudflare-circl
           go-github-com-gaukas-godicttls
           go-github-com-klauspost-compress
           go-github-com-quic-go-quic-go
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys))
    (home-page "https://github.com/refraction-networking/utls")
    (synopsis "Fork of the Go standard TLS library, providing low-level access
to the ClientHello for mimicry purposes")
    (description "uTLS is a fork of “crypto/tls”, which provides ClientHello
fingerprinting resistance, low-level access to handshake, fake session tickets
and some other features.  Handshake is still performed by “crypto/tls”, this
library merely changes ClientHello part of it and provides low-level access.")
    (license license:bsd-3)))

(define-public go-github-com-rfjakob-eme
  (package
    (name "go-github-com-rfjakob-eme")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/rfjakob/eme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yrbhvy0337mf12fp8p4sy8ry8r3w2qfdf8val5hj07p2lri0cqk"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/rfjakob/eme"))
    (home-page "https://github.com/rfjakob/eme")
    (synopsis "EME for Go")
    (description "EME (ECB-Mix-ECB or, clearer, Encrypt-Mix-Encrypt) is a
wide-block encryption mode developed by Halevi and Rogaway.")
    (license license:expat)))

(define-public go-github-com-riobard-go-bloom
  (let ((commit "cdc8013cb5b3eb0efebec85f0e904efccac42df9")
        (revision "0"))
    (package
      (name "go-github-com-riobard-go-bloom")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/riobard/go-bloom")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "10a8ixh6zw52df2imxrzgxi82zc1j5hqnv5smjp818qwdn1a1rhj"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/riobard/go-bloom"))
      (home-page "https://github.com/riobard/go-bloom")
      (synopsis "Bloom filter in Go")
      (description "Go-Bloom implements bloom filter using double hashing.")
      (license license:asl2.0))))

(define-public go-github-com-shadowsocks-go-shadowsocks2
  (package
    (name "go-github-com-shadowsocks-go-shadowsocks2")
    ;; Version > 0.1.3 requires go-toolchain v1.16.
    (version "0.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shadowsocks/go-shadowsocks2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1wzy3ml4ld83iawcl6p313bskzs6zjhz8vlg8kpwgn71cnbv4pvi"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/shadowsocks/go-shadowsocks2"))
    (propagated-inputs
     (list go-github-com-riobard-go-bloom
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-golang-org-x-sys
           go-golang-org-x-text))
    (home-page "https://github.com/shadowsocks/go-shadowsocks2")
    (synopsis "Shadowsocks tunnel proxy")
    (description "Go-ShadowSocks is a Go implementation of the Shadowsocks
tunnel proxy protocol.")
    (license license:asl2.0)))

(define-public go-github-com-xanzy-ssh-agent
  (package
    (name "go-github-com-xanzy-ssh-agent")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/xanzy/ssh-agent")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1chjlnv5d6svpymxgsr62d992m2xi6jb5lybjc5zn1h3hv1m01av"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/xanzy/ssh-agent"))
    (native-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/xanzy/ssh-agent/")
    (synopsis "Control ssh-agent from Go")
    (description "Package agent implements the ssh-agent protocol, and
provides both a client and a server.  The client can talk to a standard
ssh-agent that uses UNIX sockets, and one could implement an alternative
ssh-agent process using the sample server.")
    (license license:asl2.0)))

(define-public go-gitlab-com-yawning-edwards25519-extra
  (let ((commit "2149dcafc266f66d2487f45b156f6397f9c4760b")
        (revision "0"))
    (package
      (name "go-gitlab-com-yawning-edwards25519-extra")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.com/yawning/edwards25519-extra")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "08mz1qyi8ig515hh5blnzxhiwsav564ah7mzyhvmr6i48ndhhv98"))))
      (build-system go-build-system)
      (arguments
       '(#:unpack-path "gitlab.com/yawning/edwards25519-extra"
         #:phases
         (modify-phases %standard-phases
           (replace 'build
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'build)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "gitlab.com/yawning/edwards25519-extra/elligator2"
                 "gitlab.com/yawning/edwards25519-extra/h2c"
                 "gitlab.com/yawning/edwards25519-extra/internal/montgomery"
                 "gitlab.com/yawning/edwards25519-extra/vrf"))))
           (replace 'check
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'check)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "gitlab.com/yawning/edwards25519-extra/elligator2"
                 "gitlab.com/yawning/edwards25519-extra/h2c"
                 "gitlab.com/yawning/edwards25519-extra/internal/montgomery"
                 "gitlab.com/yawning/edwards25519-extra/vrf"))))
           (replace 'install
             (lambda arguments
               (for-each
                (lambda (directory)
                  (apply (assoc-ref %standard-phases 'install)
                         `(,@arguments #:import-path ,directory)))
                (list
                 "gitlab.com/yawning/edwards25519-extra/elligator2"
                 "gitlab.com/yawning/edwards25519-extra/h2c"
                 "gitlab.com/yawning/edwards25519-extra/internal/montgomery"
                 "gitlab.com/yawning/edwards25519-extra/vrf")))))))
      (propagated-inputs (list go-golang-org-x-crypto
                               go-filippo-io-edwards25519))
      (home-page "https://gitlab.com/yawning/edwards25519-extra")
      (synopsis "edwards25519-extra")
      (description "This package provides extensions to the Go standard
library's Ed25519 and curve25519 implementations, primarily extracted from
@@url{https://github.com/oasisprotocol/curve25519-voi,curve25519-voi}.  This
package is intended for interoperability with the standard library and the
@@url{https://filippo.io/edwards25519,edwards25519} package as much as
possible.")
      (license license:bsd-3))))

(define-public go-github-com-youmark-pkcs8
  (package
    (name "go-github-com-youmark-pkcs8")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/youmark/pkcs8")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ckdrwa5rmp2c85936qd9d0gzrnrvqfg0297ansz5frdhg6fc6nq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/youmark/pkcs8"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/youmark/pkcs8")
    (synopsis "Functions to parse and convert private keys in PKCS#8 format")
    (description
     "@code{pkcs8} implements functions to process private keys in PKCS#8
format, as defined in RFC 5208 and RFC 5958.  It can handle both unencrypted
PKCS#8 PrivateKeyInfo format and EncryptedPrivateKeyInfo format with
PKCS#5 (v2.0) algorithms.")
    (license license:expat)))

(define-public go-lukechampine-com-blake3
  (package
    (name "go-lukechampine-com-blake3")
    (version "1.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/lukechampine/blake3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yxdwp8dpnnq2wbwsxlkbq570i99sc6781y39czjxi9jh9z5nw55"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "lukechampine.com/blake3"))
    (propagated-inputs
     (list go-github-com-klauspost-cpuid))
    (home-page "https://pkg.go.dev/lukechampine.com/blake3")
    (synopsis "Implementation of the BLAKE3 cryptographic hash function")
    (description "@code{blake3} implements the BLAKE3 cryptographic hash
function.  In addition to the pure-Go implementation, this package also
contains AVX-512 and AVX2 routines (generated by avo) that greatly increase
performance for large inputs and outputs.")
    (license license:expat)))

(define-public go-torproject-org-pluggable-transports-goptlib
  (package
    (name "go-torproject-org-pluggable-transports-goptlib")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://git.torproject.org/pluggable-transports/goptlib")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lh938194hvkf8pqgnxwf6hvjv9rv0j3kasi07r2ckrj8sxzk4jc"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "git.torproject.org/pluggable-transports/goptlib.git"))
    (home-page "https://gitweb.torproject.org/pluggable-transports/goptlib.git/")
    (synopsis "Go pluggable transports library")
    (description "GoPtLib is a library for writing Tor pluggable transports in
Go.")
    (license license:cc0)))

;;;
;;; Executables:
;;;

(define-public age
  (package
    (inherit go-filippo-io-age)
    (name "age")
    (arguments
     (list
      #:install-source? #f
      #:import-path "filippo.io/age/cmd/age"
      #:unpack-path "filippo.io/age"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-failing-test-data-files
            ;; FIXME: testdata/output_file.txt:49: unknown command "ttyin"
            ;; age: error: input and output file are the same: "inputcopy"
            ;; age: error: input and output file are the same: "./inputcopy"
            ;; age: error: input and output file are the same: "keycopy"
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list "testdata/scrypt.txt"
                                "testdata/output_file.txt"
                                "testdata/encrypted_keys.txt"
                                "testdata/terminal.txt"))))))))))

(define-public age-keygen
  (package
    (inherit go-filippo-io-age)
    (name "age-keygen")
    (arguments
     `(#:import-path "filippo.io/age/cmd/age-keygen"
       #:unpack-path "filippo.io/age"
       #:install-source? #f))))

(define-public go-keyring
  (package
    (inherit go-github-com-99designs-keyring)
    (name "go-keyring")
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/99designs/keyring/cmd/keyring"
      #:unpack-path "github.com/99designs/keyring"))
    (description
     (string-append (package-description go-github-com-99designs-keyring)
                    "  This package provides an command line interface (CLI)
tool."))))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
