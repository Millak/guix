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
;;; Copyright © 2022, 2023, 2025 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Benjamin <benjamin@uvy.fr>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
;;; Copyright © 2023 Jack Hill <jackhill@jackhill.us>
;;; Copyright © 2023 Miguel Ángel Moreno <mail@migalmoreno.com>
;;; Copyright © 2023 conses <contact@conses.eu>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Jean Simard <woshilapin@tuziwo.info>
;;; Copyright © 2024 Jesse Eisses <jesse@eisses.email>
;;; Copyright © 2024 Superfly Johnson <superfly.johnson@yahoo.com>
;;; Copyright © 2024 Troy Figiel <troy@troyfigiel.com>
;;; Copyright © 2025 Roman Scherer <roman@burningswell.com>
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
  #:use-module (gnu packages security-token)
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
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/age")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "183iqzmdwk4b35vxrdg2gdzd4277yr5bgbgl9brqv3w1dap5v4pm"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Age source bundles manpages already. Seems OK not to rebuild
            ;; them with ronn, they are pretty auditable.
            (with-directory-excursion "doc"
              (for-each delete-file '("age.1.html"
                                      "age.1.ronn"
                                      "age-keygen.1.html"
                                      "age-keygen.1.ronn")))))))
    (build-system go-build-system)
    (arguments
     (list
      #:build-flags #~(list (string-append "-ldflags=-X main.Version="
                                           #$version))
      #:embed-files #~(list "armor.*" "header_crlf" "hmac_.*" "scrypt.*"
                            "stanza_.*" "stream_.*" "version_unsupported"
                            "x25519.*" "x25519_.*")
      #:import-path "filippo.io/age"))
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
@url{https://age-encryption.org/v1} specification.  It features small explicit
keys, no configuration options, and Unix-style composability.")
    (license license:bsd-3)))

(define-public go-filippo-io-edwards25519
  (package
    (name "go-filippo-io-edwards25519")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FiloSottile/edwards25519")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b2c4iv13mfa0dydr8wawpnnrxgwl7mxzhryfrkjxrgwad8gas5k"))))
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
      #:test-flags
      #~(list "-skip" (string-join
                       (list "TestLibSecretKeysWhenEmpty"
                             "TestLibSecretKeysWhenNotEmpty"
                             "TestLibSecretGetWhenEmpty"
                             "TestLibSecretGetWhenNotEmpty"
                             "TestLibSecretRemoveWhenEmpty"
                             "TestLibSecretRemoveWhenNotEmpty")
                       "|"))))
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
  (package
    (name "go-github-com-aead-chacha20")
    (version "0.0.0-20180709150244-8b13a72661da")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aead/chacha20")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gbmgq5kbqmbyrsav57ql4jzbvqvp1q7yvcd5fl3wf5g94iyv56r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aead/chacha20"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://github.com/aead/chacha20")
    (synopsis "ChaCha20 and XChaCha20 stream ciphers")
    (description
     "ChaCha is a stream cipher family created by Daniel Bernstein.  The most
common ChaCha variant is ChaCha20 (20 rounds).  ChaCha20 is standardized in
RFC 7539.")
    (license license:expat)))

(define-public go-github-com-aead-ecdh
  (package
    (name "go-github-com-aead-ecdh")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/aead/ecdh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0b0ps5wzm0q0skzikp91l8slgaw5s9z42g4wnmc69am5gw7h4mpd"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/aead/ecdh"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/aead/ecdh")
    (synopsis "Elliptic Cureves Deffie-Hellman key exchange implementation in Golang")
    (description
     "Package ecdh implements the Diffie-Hellman key exchange using elliptic
curves (ECDH).  It directly provides ECDH implementations for the NIST curves
P224, P256, P384, and Bernstein's Cruve25519.  The same logic is available in
Go 1.20 @code{crypto/ecdh} standard package.")
    (license license:expat)))

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

;; XXX: Deprecated in upstream: This repository has been archived by the owner
;; on Nov 10, 2020. It is now read-only.
;; Consider to remove when nothing is depend on it.
(define-public go-github-com-apparentlymart-go-openvpn-mgmt
  (let ((commit "4d2ce95ae600ee04eeb020ee0997aabb82752210")
        (revision "0"))
    (package
      (name "go-github-com-apparentlymart-go-openvpn-mgmt")
      (version (git-version "0.0.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/apparentlymart/go-openvpn-mgmt")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1dn431jnswg5ns1ah10wswnw6wiv48zq21zr5xp1178l4waswj7k"))))
      (build-system go-build-system)
      (arguments
       (list
        #:skip-build? #t
        #:import-path "github.com/apparentlymart/go-openvpn-mgmt"))
      (home-page "https://github.com/apparentlymart/go-openvpn-mgmt")
      (synopsis "Go client library for OpenVPN's management protocol")
      (description
       "Go-OpenVPN-Mgmt implements a client for the OpenVPN management
interface.  It can be used to monitor and control an OpenVPN process running
with its management port enabled.")
      (license license:expat))))

(define-public go-github-com-blanu-dust
  (package
    (name "go-github-com-blanu-dust")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/blanu/Dust")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lya21w06ramq37af5hdiafbrv5k1csjm7k7m00v0bfxg3ni01bs"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Examples are all broken.
            (delete-file-recursively "modelgen/examples")
            ;; Fix module path in test file.
            (substitute* "go/huffman/huffman_test.go"
              (("github.com/blanu/Dust/go/DustModel/huffman")
               "github.com/blanu/Dust/go/huffman"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/blanu/Dust"))
    (propagated-inputs
     (list go-github-com-operatorfoundation-ed25519
           go-github-com-op-go-logging
           go-golang-org-x-crypto))
    (home-page "https://github.com/blanu/Dust")
    (synopsis "Censorship-resistant internet transport protocol")
    (description
     "Dust is an Internet protocol designed to resist a number of attacks
currently in active use to censor Internet communication.  While adherence to
the theoretical maxims of cryptographic security is observed where possible,
the focus of Dust is on real solutions to real attacks.")
    (license
     (list
      ;; Skein.
      license:bsd-2
      ;; Others.
      license:expat))))

(define-public go-github-com-bradenhilton-cityhash
  (package
    (name "go-github-com-bradenhilton-cityhash")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bradenhilton/cityhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rfmbis47m42w05inqmph7jk6kza79miq9ifqlsdiax38b684yky"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/bradenhilton/cityhash"))
    (home-page "https://github.com/bradenhilton/cityhash")
    (synopsis "Google CityHash version 1 in Golang")
    (description
     "This package provides a Go implementation of Google City Hash,
originated from https://github.com/zhenjl/cityhash and
https://github.com/zentures/cityhash projects.")
    (license license:expat)))

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
     (list
      #:import-path "github.com/bwesterb/go-ristretto"))
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
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cespare/xxhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fhpn8iwb0p44sqi1hflgxpvy83krpi8gd0dd66m7756wszy3g6r"))
       (modules '((guix build utils)))
       (snippet '(delete-file-recursively "xxhashbench"))))
    (arguments
     (list
      #:import-path "github.com/cespare/xxhash/v2"
      #:test-subdirs #~(list "xxhsum/..." ".")
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
    (version "1.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudflare/circl")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pfxg0iqai760arvbkznwkb6w2w7gginqpzr49s419dp73kr99hj"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodule(s) with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/cloudflare/circl/pke/kyber/internal/common/asm
            ;; - github.com/cloudflare/circl/sign/internal/dilithium/asm
            ;; - github.com/cloudflare/circl/simd/keccakf1600/internal/asm
            (for-each delete-file-recursively
                      (list "pke/kyber/internal/common/asm"
                            "sign/internal/dilithium/asm"
                            "simd/keccakf1600/internal/asm"))))))
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

(define-public go-github-com-cloudwego-base64x
  (package
    (name "go-github-com-cloudwego-base64x")
    (version "0.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cloudwego/base64x")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lgs28mj5w350vp6pazz2265hx2kab3kbjw7vnk0w1skslxbj8kx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/cloudwego/base64x"))
    (native-inputs
     (list go-github-com-davecgh-go-spew
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-bytedance-sonic-loader
           go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/cloudwego/base64x")
    (synopsis "Drop-in replacement of the std @code{encoding/base64} library")
    (description
     "This package provides a drop-in replacement of the Golang standard
@code{encoding/base64} library.")
    (license (list license:asl2.0 license:asl2.0))))

(define-public go-github-com-davidlazar-go-crypto
  (package
    (name "go-github-com-davidlazar-go-crypto")
    (version "0.0.0-20200604182044-b73af7476f6c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/davidlazar/go-crypto")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10lbh8ask8hswgz2bavi6gq00dqc3y7apvkha1dhnbicwj9jqf38"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/davidlazar/go-crypto"
      #:phases
      #~(modify-phases %standard-phases
          ;; XXX: Run all tests, workaround for go-build-system's lack of Go
          ;; modules support.
          (delete 'build)
          (replace 'check
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (when tests?
                (with-directory-excursion (string-append "src/" import-path)
                  (invoke "go" "test" "-v" "./..."))))))))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/davidlazar/go-crypto")
    (synopsis "Cryptographic packages for Golang")
    (description
     "This package produces a collection of cryptographic utilities,
including the following:
@itemize
@item @code{drbg}: a cryptographically secure pseudorandom number generator as
specified in
@url{https://nvlpubs.nist.gov/nistpubs/SpecialPublications/NIST.SP.800-90Ar1.pdf,NIST SP
800-90A}
@item @code{encoding/base32}: a compact base32 encoder
@item @code{secretkey}: user-friendly secret keys that can be used with
secretbox
@item @code{salsa20}: a streaming interface (cipher.Stream) for the Salsa20
stream cipher
@item @code{poly1305}: a streaming interface (hash.Hash) for the Poly1305
one-time authenticator as specified in
@url{http://cr.yp.to/mac/poly1305-20050329.pdf, poly1305}
@end itemize")
    (license license:expat)))

(define-public go-github-com-dchest-siphash
  (package
    (name "go-github-com-dchest-siphash")
    (version "1.2.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dchest/siphash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1d6vbg5i5r6pgfk3vh93a20jdj67lgr17dk2iml7fffw67i25a2c"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/dchest/siphash"))
    (home-page "https://github.com/dchest/siphash")
    (synopsis "Go library for pseudorandom functions")
    (description
     "SipHash is a family of pseudorandom functions (PRFs) optimized
for speed on short messages.")
    (license license:cc0)))

(define-public go-github-com-decred-dcrd-crypto-blake256
  (package
    (name "go-github-com-decred-dcrd-crypto-blake256")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/decred/dcrd")
             (commit (go-version->git-ref version #:subdir "crypto/blake256"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mjzlyz2a3516g46kv421nacjd7p4g9l8ih4i7xijvsi480s5pja"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packed as separated
            ;; packages.
            (for-each
             delete-file-recursively
             (list "addrmgr" "bech32" "blockchain/standalone" "certgen"
                   "chaincfg/chainhash" "chaincfg" "connmgr" "container/lru"
                   "crypto/blake256/internal/_asm" "crypto/rand"
                   "crypto/ripemd160" "database" "dcrec/edwards" "dcrec" "dcrjson"
                   "dcrutil" "gcs" "hdkeychain" "math/uint256" "mixing" "peer"
                   "rpc/jsonrpc/types" "rpcclient" "txscript" "wire"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/decred/dcrd/crypto/blake256"
      #:unpack-path "github.com/decred/dcrd"))
    (home-page "https://github.com/decred/dcrd")
    (synopsis "BLAKE-256/BLAKE-224 crypto hash functions implementation")
    (description
     "Package blake256 implements BLAKE-256 and BLAKE-224 with SSE2, SSE4.1,
and AVX acceleration and zero allocations.")
    (license license:isc)))

(define-public go-github-com-decred-dcrd-dcrec-secp256k1-v4
  (package
    (name "go-github-com-decred-dcrd-dcrec-secp256k1-v4")
    (version "4.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/decred/dcrd")
             (commit (go-version->git-ref version #:subdir "dcrec/secp256k1"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "19yqrrspm6n1x7wa1chqj0j95bc5w02ygddr06ajzf6x7i7q09q5"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packed as separated
            ;; packages.  Delete any intersecting ones to prevent "Permission
            ;; denied" when they are in use as inputs.
            (for-each delete-file-recursively (list "crypto/blake256"))))))
    (build-system go-build-system)
    (arguments
     (list
      ;; It needs to be github.com/decred/dcrd/dcrec/secp256k1/v4
      #:import-path "github.com/decred/dcrd/dcrec/secp256k1"
      #:unpack-path "github.com/decred/dcrd"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'adjust-import-path
            (lambda* (#:key import-path #:allow-other-keys)
              (let* ((origin (string-append #$output "/src/" import-path) )
                     (tmp (string-append #$output "/src/" "_tmp"))
                     (import-path (string-append origin "/v4")))
                (format #t "Adjusting paths: ~a~%~a~%~a~%" origin tmp import-path)
                (mkdir tmp)
                (copy-recursively origin tmp)
                (delete-file-recursively origin)
                (mkdir-p import-path)
                (rename-file tmp import-path)))))))
    (propagated-inputs
     (list go-github-com-decred-dcrd-crypto-blake256))
    (home-page "https://github.com/decred/dcrd")
    (synopsis "Optimized secp256k1 elliptic curve operations")
    (description
     "This package provides an optimized pure Go implementation of elliptic
curve cryptography operations over the secp256k1 curve as well as data
structures and functions for working with public and private secp256k1 keys as
cpecified in the @url{https://www.secg.org/sec2-v2.pdf} standard.

In addition, sub packages are provided to produce, verify, parse, and
serialize ECDSA signatures and EC-Schnorr-DCRv0 (a custom Schnorr-based
signature scheme specific to Decred) signatures.  See the README.md files in
the relevant sub packages for more details about those aspects.")
    (license license:isc)))

(define-public go-github-com-dgryski-dgoogauth
  (package
    (name "go-github-com-dgryski-dgoogauth")
    (version "0.0.0-20190221195224-5a805980a5f3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/dgoogauth")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ma09ghwwizxaby37jmd9xjp6560p8lp29qqi8g4xw1d35h9nhny"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/dgoogauth"))
    (home-page "https://github.com/dgryski/dgoogauth")
    (synopsis "Google Authenticator for Golang")
    (description
     "This is a Go implementation of the Google Authenticator library as
specified in @url{https://www.rfc-editor.org/rfc/rfc4226, RFC 4226} and
@url{https://www.rfc-editor.org/rfc/rfc6238, RFC 6238}.")
    (license license:asl2.0)))

(define-public go-github-com-dgryski-go-farm
  (package
    (name "go-github-com-dgryski-go-farm")
    (version "0.0.0-20200201041132-a6ae2369ad13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-farm")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qbz4a4fv3853ix974x02q1129kc4xxf0c92ib5sdpsq04zjbqv8"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/go-farm"))
    (home-page "https://github.com/dgryski/go-farm")
    (synopsis "Farmhash implementation in Golang")
    (description
     "FarmHash provides hash functions for strings and other data.  The
functions mix the input bits thoroughly but are not suitable for cryptography.
It is implemented as a mechanical translation of the non-SSE4/non-AESNI hash
functions from @url{https://github.com/google/farmhash,Google's FarmHash}.")
    (license license:expat)))

(define-public go-github-com-dgryski-go-metro
  (package
    (name "go-github-com-dgryski-go-metro")
    (version "0.0.0-20250106013310-edb8663e5e33")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-metro")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0had4wbhhghr3rbm5v4dcj73nlq49k1mpklqn9gkmzkkxfs8hf6z"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/dgryski/go-metro"))
    (home-page "https://github.com/dgryski/go-metro")
    (synopsis "Go translation of MetroHash")
    (description
     "This package provides a Go translation of the
@url{https://github.com/jandrewrogers/MetroHash, reference C++ code for
MetroHash}, a high quality, high performance hash algorithm.")
    (license license:expat)))

(define-public go-github-com-dgryski-go-mph
  (package
    (name "go-github-com-dgryski-go-mph")
    (version "0.0.0-20211217222804-81a8625fb7ed")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-mph")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10q8l4jdzqf54bnnxka2jk6qzayri3ijv51knn1n0iimfric8w9g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/go-mph"))
    (propagated-inputs
     (list go-github-com-dgryski-go-metro))
    (home-page "https://github.com/dgryski/go-mph")
    (synopsis "Go minimal perfect hash function")
    (description
     "This package implements a hash/displace minimal perfect hash function.")
    (license license:expat)))

(define-public go-github-com-dgryski-go-rendezvous
  (package
    (name "go-github-com-dgryski-go-rendezvous")
    (version "0.0.0-20200823014737-9f7001d12a5f")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/dgryski/go-rendezvous")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hhdbsm5k19kh1fyxs4aibza9jylils4p3555lr8xalhj2iz3zlz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/dgryski/go-rendezvous"))
    (home-page "https://github.com/dgryski/go-rendezvous")
    (synopsis "Hashing functions for Golang")
    ;; Project provides no README.
    (description
     "This package provides a hashing function.")
    (license license:expat)))

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

(define-public go-github-com-elithrar-simple-scrypt
  (package
    (name "go-github-com-elithrar-simple-scrypt")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/elithrar/simple-scrypt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xkyw6gdy9cxj7l20cmd97axcsbf0jmcfw94c4gyy1hnd4drszzf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/elithrar/simple-scrypt"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/elithrar/simple-scrypt")
    (synopsis "Generating, comparing and inspecting password hashes library")
    (description
     "Package scrypt provides a convenience wrapper around Go's existing
@code{crypto/scrypt} package that makes it easier to securely derive strong
keys from weak inputs (i.e.  user passwords).  The package provides password
generation, constant-time comparison and parameter upgrading for scrypt
derived keys.")
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

(define-public go-github-com-gliderlabs-ssh
  (package
    (name "go-github-com-gliderlabs-ssh")
    (version "0.3.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gliderlabs/ssh")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01svn6n2i7gb3j4wvjh3d7xyh3n0kxm5cda2kg9vgpl1l3bbsvqm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gliderlabs/ssh"))
    (propagated-inputs
     (list go-github-com-anmitsu-go-shlex
           go-golang-org-x-crypto))
    (home-page "https://github.com/gliderlabs/ssh")
    (synopsis "SSH servers in Golang")
    (description
     "Package ssh wraps the crypto/ssh package with a higher-level API for
building SSH servers.  The goal of the API was to make it as simple as using
net/http, so the API is very similar.")
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
    (version "5.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/golang-jwt/jwt")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "13l2p4g9f7bpz0nf2sc4r5ms06ps7bs7z1vpzzv943q094pjs0yw"))))
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

(define-public go-github-com-gsterjov-go-libsecret
  (package
    (name "go-github-com-gsterjov-go-libsecret")
    (version "0.0.0-20161001094733-a6f4afe4910c")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gsterjov/go-libsecret")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09zaiadnll83vs22ib89agg7anj0blw5fywvmckxllsgif6ak6v7"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gsterjov/go-libsecret"))
    (propagated-inputs
     (list go-github-com-godbus-dbus))
    (home-page "https://github.com/gsterjov/go-libsecret")
    (synopsis "Manage secrets via the @code{Secret Service} DBus API")
    (description
     "This native Go library manages secrets via the freedesktop.org
@code{Secret Service} DBus interface.")
    (license license:expat)))

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

(define-public go-github-com-gxed-hashland-keccakpg
  (package
    (name "go-github-com-gxed-hashland-keccakpg")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gxed/hashland")
             (commit (go-version->git-ref version
                                          #:subdir "keccakpg"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b921dh9i6zw7y8jfzwvrmdbhnwid12a5z1zjawslfq2vvsajwmm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gxed/hashland/keccakpg"
      #:unpack-path "github.com/gxed/hashland"))
    (home-page "https://github.com/gxed/hashland")
    (synopsis "Implements the Keccak (SHA-3) hash algorithm in Go")
    (description
     "Package @command{keccak} implements the Keccak (SHA-3) hash algorithm.
See http://keccak.noekeon.org.")
    (license license:expat)))

(define-public go-github-com-gxed-hashland-murmur3
  (package
    (name "go-github-com-gxed-hashland-murmur3")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/gxed/hashland")
             (commit (go-version->git-ref version
                                          #:subdir "murmur3"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1b921dh9i6zw7y8jfzwvrmdbhnwid12a5z1zjawslfq2vvsajwmm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/gxed/hashland/murmur3"
      #:unpack-path "github.com/gxed/hashland"))
    (home-page "https://github.com/gxed/hashland")
    (synopsis "Golang implementation of MurmurHash3 algorithm")
    (description
     "This package provides a native Go implementation of
@url{https://en.wikipedia.org/wiki/MurmurHash, Austin Appleby's third
MurmurHash} revision (aka MurmurHash3).")
    (license license:bsd-3)))

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

(define-public go-github-com-jedisct1-go-minisign
  (package
    (name "go-github-com-jedisct1-go-minisign")
    (version "0.2.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jedisct1/go-minisign")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "15wxg55q95kfany1zmzaazm6dzmd9k4jzigmmscwyavdbkb8ng5b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jedisct1/go-minisign"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/jedisct1/go-minisign")
    (synopsis "Minisign verification library for Golang")
    (description
     "A Golang library to verify Minisign signatures.")
    (license license:expat)))

(define-public go-github-com-jphastings-jwker
  (package
    (name "go-github-com-jphastings-jwker")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jphastings/jwker")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nb487c2cfazxwqghq5a8iz8gyi0hhajc39c260f0n6d3ib1798g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jphastings/jwker"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (home-page "https://github.com/jphastings/jwker")
    (synopsis "PEM -> JWK conversion tool")
    (description
     "This package implements a functionality to convert keys between the
@acronym{PEM, Privacy-Enhanced Mail} and @acronym{JWK, JSON Web Key} file
formats.")
    (license license:expat)))

(define-public go-github-com-jzelinskie-whirlpool
  (package
    (name "go-github-com-jzelinskie-whirlpool")
    (version "0.0.0-20201016144138-0675e54bb004")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/jzelinskie/whirlpool")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w74h9dz8pkwal3aqymymsq2zgl7d16dw1kxa7dfkad167g3s1mz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/jzelinskie/whirlpool"))
    (home-page "https://github.com/jzelinskie/whirlpool")
    (synopsis "Cryptographic hashing library")
    (description
     "Package whirlpool implements the ISO/IEC 10118-3:2004 whirlpool
cryptographic hash as specified in
@url{http://www.larc.usp.br/~pbarreto/WhirlpoolPage.html}.")
    (license license:bsd-3)))

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

(define-public go-github-com-miekg-pkcs11
  (package
    (name "go-github-com-miekg-pkcs11")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/miekg/pkcs11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w403kmqxf8w25aap4901nrm7wj0wf95lrwwv1nvdjnwi8jjgapz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/miekg/pkcs11"
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda _
              (setenv "SOFTHSM_LIB" (format #f "~a/lib/softhsm/libsofthsm2.so"
                                            #$(this-package-native-input "softhsm"))))))))
    (native-inputs
     (list softhsm))
    (home-page "https://github.com/miekg/pkcs11")
    (synopsis "PKCS #11 wrapper for Golang")
    (description
     "This package provides an implementation of the
@url{https://en.wikipedia.org/wiki/PKCS_11, PKCS#11} API.  It wraps the
library closely, but uses Go idiom where it makes sense.  It has been tested
with SoftHSM.")
    (license license:bsd-3)))

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

(define-public go-github-com-minio-crc64nvme
  (package
    (name "go-github-com-minio-crc64nvme")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/crc64nvme")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hk0v474ligna49qh4rvwnqxv7w6ka5grdlb7fczh4kzhx80x536"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/crc64nvme"))
    (propagated-inputs (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/minio/crc64nvme")
    (synopsis "CRC64 checksums using carryless-multiplication")
    (description
     "This package calculates CRC64 checksums using carryless-multiplication
accelerated with SIMD instructions for both ARM and x86.  The code is based on
the @code{https://github.com/awesomized/crc64fast-nvme.git, crc64fast-nvme}
package in Rust.")
    (license license:asl2.0)))

(define-public go-github-com-minio-highwayhash
  (package
    (name "go-github-com-minio-highwayhash")
    (version "1.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/highwayhash")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14g2x266h8sxs0yynmcl2r2mnhx55rfzgykw4hygm7i1vpfrikg4"))))
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

(define-public go-github-com-minio-md5-simd
  (package
    (name "go-github-com-minio-md5-simd")
    (version "1.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/minio/md5-simd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qj8ipifbdg3ppilyqj8zy68f72rmqy8flli1vch3fibrbw8vpd0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/minio/md5-simd"))
    (propagated-inputs
     (list go-github-com-klauspost-cpuid-v2))
    (home-page "https://github.com/minio/md5-simd")
    (synopsis "Accelerate MD5 computations in pure Golang")
    (description
     "This is a SIMD accelerated MD5 package, allowing up to either 8 (AVX2)
or 16 (AVX512) independent MD5 sums to be calculated on a single CPU core.

@code{md5-simd} integrates a similar mechanism as described in
@code{minio/sha256-simd} for making it easy for clients to take advantages of
the parallel nature of the MD5 calculation.  This will result in reduced
overall CPU load.")
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

(define-public go-github-com-mr-tron-base58
  (package
    (name "go-github-com-mr-tron-base58")
    (version "1.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mr-tron/base58")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ngxfpaa26p53lciz9vf2gn21l77kz8pcm2asxbv0l87g6xwqp7h"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/mr-tron/base58"))
    (home-page "https://github.com/mr-tron/base58")
    (synopsis "Fast implementation of base58 encoding on Golang")
    (description
     "Fast implementation of base58 encoding on Golang.  A trivial
@command{big.Int} encoding benchmark results in 6 times faster encoding and 8
times faster decoding.")
    (license license:expat)))

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

;; XXX: Deprecated in upstream: This repository has been archived by the owner
;; on May 1, 2024. It is now read-only.
;; Consider to remove when nothing is depend on it.
(define-public go-github-com-operatorfoundation-obfs4
  (package
    (name "go-github-com-operatorfoundation-obfs4")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/obfs4")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s730xagdxs66wfh65hb5v9a5h01q5ncic3pyij0a043scagizgr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/OperatorFoundation/obfs4"
      #:test-subdirs #~(list "common/..."
                             "proxy_dialers/..."
                             "transports/obfs4/...")))
    (propagated-inputs
     (list go-github-com-dchest-siphash
           go-github-com-operatorfoundation-ed25519
           go-github-com-willscott-goturn
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-torproject-org-pluggable-transports-goptlib))
    (home-page "https://github.com/OperatorFoundation/obfs4")
    (synopsis "Network obfourscator to scramble network traffic")
    (description
     "Obfs4 is a look-like nothing obfuscation protocol that incorporates
ideas and concepts from Philipp Winter's ScrambleSuit protocol.

The notable differences between ScrambleSuit and obfs4 are:
@itemize
@item The handshake always does a full key exchange (no such thing as a Session
Ticket Handshake).
@item The handshake uses the Tor Project's ntor handshake with public keys
obfuscated via the Elligator 2 mapping.
@item The link layer encryption uses NaCl secret boxes (Poly1305/XSalsa20).
@end itemize")
    (license license:bsd-2)))

(define-public go-github-com-operatorfoundation-shapeshifter-ipc
  (package
    (name "go-github-com-operatorfoundation-shapeshifter-ipc")
    (version "2.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OperatorFoundation/shapeshifter-ipc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1q1fcnllg462nfca16s5mr0n2jh92x3hj946qnaqc682phjz04lg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:tests? #f ; all tests fail with error: undefined: Args.
      #:import-path "github.com/OperatorFoundation/shapeshifter-ipc"))
    (home-page "https://github.com/OperatorFoundation/shapeshifter-ipc")
    (synopsis "Go implementation of the Pluggable Transports IPC protocol")
    (description
     "Shapeshifter-IPC is a library for Go implementing the IPC
protocol from the Pluggable Transports 2.0 specification.")
    (license license:expat)))

(define-public go-github-com-operatorfoundation-shapeshifter-ipc-v3
  (package
    (inherit go-github-com-operatorfoundation-shapeshifter-ipc)
    (name "go-github-com-operatorfoundation-shapeshifter-ipc-v3")
    (version "3.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/OperatorFoundation/shapeshifter-ipc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dvvls7v40krq26nzn2f1q55628i3zff4by1ib2wad9pyhb88rg0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; all tests fail with error: undefined: Args.
      #:import-path "github.com/OperatorFoundation/shapeshifter-ipc/v3"
      #:unpack-path "github.com/OperatorFoundation/shapeshifter-ipc"))))

(define-public go-github-com-operatorfoundation-shapeshifter-transports
  (package
    (name "go-github-com-operatorfoundation-shapeshifter-transports")
    (version "3.0.12")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/OperatorFoundation/shapeshifter-transports")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f1hzhk3q2fgqdg14zlg3z0s0ib1y9xwj89qnjk95b37zbgqjgsb"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; When parallel, tests fail with error: Failed to start listener:listen
      ;; tcp 127.0.0.1:1235: bind: address already in use.
      #:parallel-tests? #f
      #:skip-build? #t
      #:import-path "github.com/OperatorFoundation/shapeshifter-transports"
      #:test-flags
      #~(list "-skip" (string-join
                  (list
                   ;; Tests fail in "Optimizer" module.
                   "TestObfs4Transport_Dial"
                   "TestOptimizerObfs4Transport_Dial"
                   "TestOptimizerTransportFirstDial"
                   "TestOptimizerTransportRandomDial"
                   "TestOptimizerTransportRotateDial"
                   "TestOptimizerTransportTrackDial"
                   "TestOptimizerTransportMinimizeDialDurationDial"
                   ;; Tests fail in "Replicant" module.
                   "TestMarshalConfigs"
                   "TestMarshalConfigs"
                   "TestMarshalSilverRandomEnumeratedConfigs"
                   "TestFactoryMonotoneRandomEnumerated"
                   ;; Tests fail in "meeklite" module.
                   "TestMeeklite"
                   "TestFactoryMeeklite"
                   ;; Test fails in "meekserver/v2" module.
                   "TestMeekServerListen2"
                   ;; Test fails in "obfs4" module.
                   "TestObfs4"
                   "TestObfs4Factory"
                  ;; Tests fail in "shadow" module.
                  "TestShadow"
                  "TestShadowTransport")
                  "|"))
      #:test-subdirs
      #~(list
         ;; All tests fail with error: invalid memory address or nil pointer
         ;; dereference.
         ;; "transports/Dust/..."
         "transports/Optimizer/..."
         "transports/Replicant/..."
         "transports/meeklite/..."
         ;; All tests fail with error:  misplaced +build comment.
         ;; "transports/meekserver/v3/..."
         "transports/meekserver/v2/..."
         "transports/obfs2/..."
         "transports/obfs4/..."
         "transports/shadow/...")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-aead-chacha20
           go-github-com-blanu-dust
           go-github-com-deckarep-golang-set
           go-github-com-kataras-golog
           go-github-com-mufti1-interconv
           go-github-com-opentracing-opentracing-go
           go-github-com-operatorfoundation-monolith-go-1.0.4
           go-github-com-operatorfoundation-obfs4
           go-github-com-operatorfoundation-shapeshifter-ipc
           go-github-com-shadowsocks-go-shadowsocks2
           go-golang-org-x-crypto
           go-golang-org-x-net
           go-torproject-org-pluggable-transports-goptlib))
    (home-page "https://github.com/OperatorFoundation/shapeshifter-transports")
    (synopsis "Go implementation of Pluggable Transports")
    (description "Shapeshifter-Transports is a set of Pluggable Transports
implementing the Go API from the Pluggable Transports 2.0 specification.
Each transport implements a different method of shapeshifting network traffic.
The goal is for application traffic to be sent over the network in a shapeshifted
form that bypasses network filtering, allowing the application to work on
networks where it would otherwise be blocked or heavily throttled.")
    (license license:expat)))

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
    (synopsis "Helper library for cryptographic and mathematical randoms")
    (description
     "This package provides primitives for generating random values.")
    (license license:expat)))

(define-public go-github-com-pjbgf-sha1cd
  (package
    (name "go-github-com-pjbgf-sha1cd")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/pjbgf/sha1cd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1g07kp07kvbsmpdrrv0r805vw4rr1mp77vx06m31nxvnp1s42zwi"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/pjbgf/sha1cd"))
    (home-page "https://github.com/pjbgf/sha1cd")
    (synopsis "sha1cd")
    (description
     "Package sha1cd implements collision detection based on the whitepaper
Counter-cryptanalysis from Marc Stevens.  The original ubc implementation was
done by Marc Stevens and Dan Shumow, and can be found at:
@@url{https://github.com/cr-marcstevens/sha1collisiondetection,https://github.com/cr-marcstevens/sha1collisiondetection}.")
    (license license:asl2.0)))

(define-public go-github-com-protonmail-go-crypto
  (package
    (name "go-github-com-protonmail-go-crypto")
    (version "1.1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/go-crypto")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0kcan2bw548cn6pm282zyddysv400dankcsrdanha7qmxqki34c0"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ProtonMail/go-crypto"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
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
    (synopsis "Fork of the Go standard TLS library")
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

(define-public go-github-com-sean--seed
  (package
    (name "go-github-com-sean--seed")
    (version "0.0.0-20170313163322-e2103e2c3529")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sean-/seed")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0glir8jxi1w7aga2jwdb63pp1h8q4whknili7xixsqzwyy716125"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/sean-/seed"))
    (home-page "https://github.com/sean-/seed")
    (synopsis "Seed random number generator")
    (description
     "Boiler-plate to securely @url{https://en.wikipedia.org/wiki/Random_seed,
seed} Go's random number generator (if possible).")
    (license license:expat)))

(define-public go-github-com-shadowsocks-go-shadowsocks2
  (package
    (name "go-github-com-shadowsocks-go-shadowsocks2")
    (version "0.1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shadowsocks/go-shadowsocks2")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0n24h5ffgc3735y0mmp6dbhxdfm9fk13i26fqxxw7i75qnmvjvyg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shadowsocks/go-shadowsocks2"))
    (propagated-inputs
     (list go-github-com-riobard-go-bloom
           go-golang-org-x-crypto))
    (home-page "https://github.com/shadowsocks/go-shadowsocks2")
    (synopsis "Shadowsocks tunnel proxy")
    (description "Go-ShadowSocks is a Go implementation of the Shadowsocks
tunnel proxy protocol.")
    (license license:asl2.0)))

(define-public go-github-com-shogo82148-go-shuffle
  (package
    (name "go-github-com-shogo82148-go-shuffle")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/shogo82148/go-shuffle")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0z5n5jp57b68pq70wkrmw9z3vibjnq7b7f6i62pjhh1a83kknfg1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/shogo82148/go-shuffle"))
    (home-page "https://github.com/shogo82148/go-shuffle")
    (synopsis "Shuffling slices and user-defined collections")
    (description
     "Package shuffle provides primitives for shuffling slices and
user-defined collections.")
    (license license:expat)))

(define-public go-github-com-skeema-knownhosts
  (package
    (name "go-github-com-skeema-knownhosts")
    (version "1.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/skeema/knownhosts")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1i74wqingiflrrvpzhahwdly9f8c27i2far1qxkszi7aswhpj956"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/skeema/knownhosts"))
    (propagated-inputs (list go-golang-org-x-crypto))
    (home-page "https://github.com/skeema/knownhosts")
    (synopsis "Go SSH known_hosts wrapper with host key lookup")
    (description
     "Package knownhosts is a thin wrapper around @code{golang.org/x/crypto/ssh/knownhosts},
adding the ability to obtain the list of host key algorithms for a known
host.")
    (license license:asl2.0)))

(define-public go-github-com-spaolacci-murmur3
  (package
    (name "go-github-com-spaolacci-murmur3")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spaolacci/murmur3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1lv3zyz3jy2d76bhvvs8svygx66606iygdvwy5cwc0p5z8yghq25"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/spaolacci/murmur3"))
    (home-page "https://github.com/spaolacci/murmur3")
    (synopsis "Native MurmurHash3 Go implementation")
    (description
     "Native Go implementation of Austin Appleby's third MurmurHash
revision (aka MurmurHash3).  Reference algorithm has been slightly hacked as
to support the streaming mode required by Go's standard Hash interface.")
    (license license:bsd-3)))

(define-public go-github-com-tjfoc-gmsm
  (package
    (name "go-github-com-tjfoc-gmsm")
    (version "1.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tjfoc/gmsm")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "18x1g555a3i86rkjrlxa6h6j3j87vhx480dqnv9hdij6cy3zph7i"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/tjfoc/gmsm"
      #:test-subdirs
      #~(list ;; "gmtls/..." ; requires go-google-golang-org-grpc
              "pkcs12/..."
              "sm2/..."
              "sm3/..."
              "sm4/..."
              "x509/...")
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'pre-check
            (lambda* (#:key import-path #:allow-other-keys)
              ;; Tests need to write to that files.
              (with-directory-excursion (string-append "src/" import-path)
                (make-file-writable "sm3/ifile"))))
          (add-after 'check 'post-check
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                ;; Remove modified testdata just in case.
                (delete-file-recursively "sm3/ifile")))))))
    (propagated-inputs
     (list go-github-com-golang-protobuf
           go-golang-org-x-crypto
           go-golang-org-x-net
           #;go-google-golang-org-grpc)) ; not packed yet
    (home-page "https://github.com/tjfoc/gmsm")
    ;; Project's README is in Chinese Mandarin, translated with
    ;; auto translator and corrected manually.
    (synopsis "ShangMi (SM) cipher suites for Golang")
    (description
     "This package provides @url{https://en.wikipedia.org/wiki/SM4_(cipher),
ShāngMì 4} cipher suites implementation (GM SM2/3/4).

Main functions:
@itemize
@item @code{SM2} national secret elliptic curve algorithm library
@item @code{SM3} national secret hash algorithm library
@item @code{SM4} national secret block cipher algorithm library
@end itemize")
    (license license:asl2.0)))

(define-public go-github-com-twmb-murmur3
  (package
    (name "go-github-com-twmb-murmur3")
    (version "1.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twmb/murmur3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "064bbgbgc45i3m9b3rqyw09g0nlrjs7dq1k716i5f06zjjpr56wg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twmb/murmur3"))
    (home-page "https://github.com/twmb/murmur3")
    (synopsis "Native MurmurHash3 Go implementation")
    (description
     "Native Go implementation of Austin Appleby's third MurmurHash
revision (aka MurmurHash3). Reference algorithm has been slightly hacked as to
support the streaming mode required by Go's standard Hash interface.")
    (license license:bsd-3)))

(define-public go-github-com-twpayne-go-pinentry
  (package
    (name "go-github-com-twpayne-go-pinentry")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twpayne/go-pinentry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ixvvfd7ywsgj0jk1plb4h2g5bpmw86qc89m02c184jh5ndawhip"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/twpayne/go-pinentry"))
    (native-inputs
     (list go-github-com-alecthomas-assert-v2
           go-github-com-golang-mock))
    (propagated-inputs
     (list go-github-com-rs-zerolog))
    (home-page "https://github.com/twpayne/go-pinentry")
    (synopsis "Golang client to GnuPG's pinentry")
    (description
     "Package pinentry provides a client to @code{GnuPG's} pinentry.")
    (license license:expat)))

(define-public go-github-com-twpayne-go-pinentry-v4
  (package
    (inherit go-github-com-twpayne-go-pinentry)
    (name "go-github-com-twpayne-go-pinentry-v4")
    (version "4.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/twpayne/go-pinentry")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z9h2lg5xd3g6dd9avcfx0nnwdybr2yibmsw7akl6pz0cls3sf63"))))
    (arguments
     (list
      #:import-path "github.com/twpayne/go-pinentry/v4"))))

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

(define-public go-github-com-xdg-go-pbkdf2
  (package
    (name "go-github-com-xdg-go-pbkdf2")
    (version "1.0.0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://github.com/xdg-go/pbkdf2")
               (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1nipijy5xkdnfyhkp5ryrjzm14si1i2v2xyfmblf84binwkbr8jh"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/xdg-go/pbkdf2"))
    (home-page "https://github.com/xdg-go/pbkdf2")
    (synopsis "Go implementation of PBKDF2")
    (description
     "Package pbkdf2 implements password-based key derivation using the PBKDF2
algorithm described in @url{https://rfc-editor.org/rfc/rfc2898.html,RFC 2898}
and @url{https://rfc-editor.org/rfc/rfc8018.html,RFC 8018}.")
    (license license:asl2.0)))

(define-public go-github-com-youmark-pkcs8
  (package
    (name "go-github-com-youmark-pkcs8")
    (version "1.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/youmark/pkcs8")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "17pnl7b0ml4595cmxhramnc7ry6df6f4zisvaafxj4r7ravx8i7c"))))
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

(define-public go-github-com-zeebo-blake3
  (package
    (name "go-github-com-zeebo-blake3")
    (version "0.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/blake3")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "117p973ccgalaqg7byj0qcd1xapysplql9np1sr9jkca500khcgf"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/zeebo/blake3/avo
            (delete-file-recursively "avo")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/blake3"))
    (native-inputs (list go-github-com-zeebo-assert))
    (propagated-inputs
     (list go-github-com-klauspost-cpuid-v2
           go-github-com-zeebo-pcg))
    (home-page "https://github.com/zeebo/blake3")
    (synopsis "Pure Go implementation of BLAKE3")
    (description
     "@code{blake3} is an implementation of
@url{https://en.wikipedia.org/wiki/BLAKE_(hash_function)#BLAKE3, BLAKE3} with AVX2
and SSE4.1 acceleration.")
    (license license:cc0)))

(define-public go-github-com-zeebo-blake3-avo
  (package
    (name "go-github-com-zeebo-blake3-avo")
    (version "0.0.0-20240814144702-1a8215cf69be")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/blake3")
             (commit (go-version->git-ref version
                                          #:subdir "avo"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "117p973ccgalaqg7byj0qcd1xapysplql9np1sr9jkca500khcgf"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/blake3/avo"
      #:unpack-path "github.com/zeebo/blake3"))
    (propagated-inputs (list go-github-com-mmcloughlin-avo))
    (home-page "https://github.com/zeebo/blake3")
    (synopsis "BLAKE3 Avo integration")
    (description
     "This package provides a @url{https://github.com/mmcloughlin/avo, avo}
vectorized version of BLAKE3 implementation in Golang.")
    (license license:cc0)))

(define-public go-github-com-zeebo-pcg
  (package
    (name "go-github-com-zeebo-pcg")
    (version "1.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/zeebo/pcg")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02vyy2zc4jdcyf22dxw8dxcp1gwzy8j5qd6yxw324qyh2w557nh5"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/zeebo/pcg"))
    (native-inputs (list go-github-com-zeebo-assert))
    (home-page "https://github.com/zeebo/pcg")
    (synopsis "PCG random number generator")
    (description
     "@code{pcg} is a random number generator that uses
@url{https://en.wikipedia.org/wiki/Permuted_congruential_generator, Permuted
Congruential Generator} (PCG) algorithm.")
    (license license:cc0)))

(define-public go-gitlab-com-nyarla-go-crypt
  (package
    (name "go-gitlab-com-nyarla-go-crypt")
    (version "0.0.0-20160106005555-d9a5dc2b789b")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/nyarla/go-crypt.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0249hbwvhy0xywi9b5k8964km27pvfkr3jvliy3azri6vnyvkkx1"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.com/nyarla/go-crypt"))
    (home-page "https://gitlab.com/nyarla/go-crypt")
    (synopsis "Implementation of @code{crypt(3)} in Golang")
    (description
     "Package crypt is a implementation of crypt(3) by golang, originated from
https://code.google.com/p/go-crypt.")
    (license license:bsd-3)))

(define-public go-gitlab-com-yawning-bsaes-git
  (package
    (name "go-gitlab-com-yawning-bsaes-git")
    (version "0.0.0-20190805113838-0a714cd429ec")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/yawning/bsaes.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w4g9phpb2f02dpkyd0ixp8jw9v42lnjljj4ysfz8im15abskwdn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.com/yawning/bsaes.git"))
    (propagated-inputs
     (list go-golang-org-x-sys))
    (home-page "https://gitlab.com/yawning/bsaes")
    (synopsis "AES implementation in Golang")
    (description
     "The @code{bsaes} is a portable pure-Go constant time AES implementation
based on the code from @url{https://bearssl.org/,BearSSL}.  On appropriate
systems, with a sufficiently recent Go runtime, it will transparently call
crypto/aes when NewCipher is invoked.")
    (license license:expat)))

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
      (synopsis "Extensions to Go standard library' Ed25519 and curve25519 implementation")
      (description "This package provides extensions to the Go standard
library's Ed25519 and curve25519 implementations, primarily extracted from
@@url{https://github.com/oasisprotocol/curve25519-voi,curve25519-voi}.  This
package is intended for interoperability with the standard library and the
@@url{https://filippo.io/edwards25519,edwards25519} package as much as
possible.")
      (license license:bsd-3))))

(define-public go-gitlab-com-yawning-obfs4-git
  (package
    (name "go-gitlab-com-yawning-obfs4-git")
    (version "0.0.0-20231012084234-c3e2d44b1033")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/yawning/obfs4.git")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1kv62161jf28v1d31avlc0f5iyk5ar06zlk3zdw99hyyfqjiasdr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.com/yawning/obfs4.git"
      #:phases
      #~(modify-phases %standard-phases
          (delete 'build)))) ; no go files in project's root
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-github-com-dchest-siphash
           go-gitlab-com-yawning-edwards25519-extra
           go-gitlab-torproject-org-tpo-anti-censorship-pluggable-transports-goptlib
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://gitlab.com/yawning/obfs4")
    (synopsis "Network traffic obfourscator library")
    (description
     "This package implements functionality based on ideas and concepts from
Philipp Winter's
@url{https://www.cs.kau.se/philwint/scramblesuit/,ScrambleSuit} protocol.
The notable differences between ScrambleSuit and obfs4:
@itemize
@item the handshake always does a full key exchange (no such thing as a
Session Ticket Handshake)
@item the handshake uses the Tor Project's ntor handshake with public keys
obfuscated via the Elligator 2 mapping
@item the link layer encryption uses NaCl secret boxes (Poly1305/XSalsa20)
@end itemize")
    (license license:gpl3+)))

(define-public go-gitlab-com-yawning-utls-git
  (package
    (name "go-gitlab-com-yawning-utls-git")
    (version "0.0.12-1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/yawning/utls.git")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f4m5g6yc0kn2s457gy98id4rr4m4z56y1nsxzx3xl04n408aimx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "gitlab.com/yawning/utls.git"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Test requiring network setup.
                       (list "TestHandshakeClientCertECDSA"
                             "TestHandshakeServerECDHEECDSAAES"
                             "TestVerifyHostname"
                             ;; Fails with error: misamtch on read.
                             "TestCipherSuiteCertPreferenceECDSA/TLSv12"
                             "TestUTLSHandshakeClientParrotGolang"
                             ;; Fails with error: expected "key size too small
                             ;; for PSS signature".
                             "TestKeyTooSmallForRSAPSS"
                             ;; Time bomb tests, certs are not valid after
                             ;; certain date.
                             "TestResumption/TLSv12"
                             "TestResumption/TLSv13")
                       "|"))))
    (propagated-inputs
     (list go-github-com-dsnet-compress
           go-gitlab-com-yawning-bsaes-git
           go-golang-org-x-crypto
           go-golang-org-x-net))
    (home-page "https://gitlab.com/yawning/utls")
    (synopsis "Alternative fork of @code{github.com/refraction-networking/utls}")
    (description
     "This provides a fork of
@url{https://github.com/refraction-networking/utls,uTLS} for the specific
purpose of improving obfs4proxy's meek_lite transport.")
    (license license:bsd-3)))

(define-public go-go-mau-fi-libsignal
  (package
    (name "go-go-mau-fi-libsignal")
    (version "0.1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             ;; Original project:
             ;;   <https://github.com/RadicalApp/libsignal-protocol-go>
             ;; The first fork:
             ;;   <https://github.com/crossle/libsignal-protocol-go>
             ;;
             ;; It is the second fork as seen in
             ;; <https://pkg.go.dev/go.mau.fi/libsignal>.
             (url "https://github.com/tulir/libsignal-protocol-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pd2kijza7dr5nbgfw176ca1r3rmgpx8h22gqjp557awxqhw9lzr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "go.mau.fi/libsignal"))
    (propagated-inputs
     (list go-filippo-io-edwards25519
           go-golang-org-x-crypto
           go-google-golang-org-protobuf))
    (home-page "https://go.mau.fi/libsignal")
    (synopsis "Go implementation of the Signal protocol for WhatsApp")
    (description
     "Libsignal-protocol-go is a Go implementation of the Signal Client
Protocol.")
    (license license:gpl3)))

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

(define-public go-software-sslmate-com-src-go-pkcs12
  (package
    (name "go-software-sslmate-com-src-go-pkcs12")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/SSLMate/go-pkcs12")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0bmkv9nxflgr5sbizpm737rbarmi8iyxny6pwdmqk0jzrg5ppd85"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "software.sslmate.com/src/go-pkcs12"))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/SSLMate/go-pkcs12")
    (synopsis "Encoding and decoding PKCS#12 files")
    (description
     "Package pkcs12 implements some of PKCS#12 (also known as P12 or PFX).
It is intended for decoding DER-encoded P12/PFX files for use with the
@code{crypto/tls} package, and for encoding P12/PFX files for use by legacy
applications which do not support newer formats.  Since PKCS#12 uses weak
encryption primitives, it SHOULD NOT be used for new applications.")
    (license license:bsd-3)))

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
  (package/inherit go-filippo-io-age
    (name "age")
    (arguments
     (substitute-keyword-arguments
         (package-arguments go-filippo-io-age)
       ((#:tests? _ #t) #f)
       ((#:install-source? _ #t) #f)
       ((#:unpack-path _ "") "filippo.io/age")
       ((#:phases phases '%standard-phases)
        #~(modify-phases #$phases
            (replace 'build
              (lambda* (#:key import-path #:allow-other-keys #:rest arguments)
                (for-each
                 (lambda (cmd)
                   (apply (assoc-ref #$phases 'build)
                          `(,@arguments
                            #:import-path ,(string-append import-path cmd))))
                 (list "/cmd/age"
                       "/cmd/age-keygen"))))
            (add-after 'install 'install-man-pages
              (lambda _
                (let ((man (string-append #$output "/man/man1/")))
                  (install-file "src/filippo.io/age/doc/age.1" man)
                  (install-file "src/filippo.io/age/doc/age-keygen.1" man))))))))
    (native-inputs (package-propagated-inputs go-filippo-io-age))
    (propagated-inputs '())
    (inputs '())
    (description
     (string-append (package-description go-filippo-io-age)
                    "\nThis package provides a command line interface (CLI)
tools."))))

(define-public age-keygen
  (deprecated-package "age-keygen" age))

(define-public go-jwker
  (package/inherit go-github-com-jphastings-jwker
    (name "go-jwker")
    (arguments
     (list
      #:tests? #f
      #:install-source? #f
      #:build-flags
      #~(list (string-append "-ldflags=-X main.version="
                             #$(package-version this-package)))
      #:unpack-path "github.com/jphastings/jwker"
      #:import-path "github.com/jphastings/jwker/cmd/jwker"))))

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

(define-public ssh-to-pgp
  (package
    (name "ssh-to-pgp")
    (version "1.1.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Mic92/ssh-to-pgp")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xaj6pnk5y2flnxm57j9bpdpll9vhg1rbjj4v3a7hn1gginxpprx"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "github.com/Mic92/ssh-to-pgp"
      ;; failed: No secret key
      #:test-flags #~(list "-skip" "TestCli")))
    (native-inputs
     (list gnupg
           go-github-com-protonmail-go-crypto
           go-golang-org-x-crypto))
    (home-page "https://github.com/Mic92/ssh-to-pgp")
    (synopsis "Convert SSH RSA keys to GPG keys")
    (description "This package provides @code{ssh-to-pgp}: a Go command line
+utility to convert SSH RSA keys to GPG keys.")
    (license license:expat)))

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
