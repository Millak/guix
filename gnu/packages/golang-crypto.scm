;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019 Vagrant Cascadian <vagrant@debian.org>
;;; Copyright © 2019, 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
;;; Copyright © 2022 (unmatched-parenthesis <paren@disroot.org>
;;; Copyright © 2022, 2023 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2023 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2023 Clément Lassieur <clement@lassieur.org>
;;; Copyright © 2023 Felix Lechner <felix.lechner@lease-up.com>
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
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-check))

;;; Commentary:
;;;
;;; Golang modules (libraries) related to Cryptography: encryption algorithms,
;;; hashing functions, TLS, key management, digital signatures, password
;;; hashing etc.
;;;
;;; Please: Try to add new module packages in alphabetic order.
;;;
;;; Code:

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

(define-public go-github-com-emersion-go-pgpmail
  (package
    (name "go-github-com-emersion-go-pgpmail")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/emersion/go-pgpmail")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ar26b0apw5bxn58qfn1a79cxigbmrqm1irh1rb7x57fydihc7wm"))))
    (build-system go-build-system)
    (arguments
     (list ;; tests don't support our version of protonmail/go-crypto; see
      ;; <https://github.com/emersion/go-pgpmail/issues/12>
      #:tests? #f
      #:import-path "github.com/emersion/go-pgpmail"))
    (propagated-inputs
     (list go-github-com-emersion-go-message
           go-github-com-protonmail-go-crypto
           go-golang-org-x-crypto
           go-golang-org-x-text))
    (home-page "https://github.com/emersion/go-pgpmail")
    (synopsis "PGP mail encryption for Go")
    (description "The pgpmail package implements PGP encryption for e-mail
messages.")
    (license license:expat)))

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
      (native-inputs
       (list go-github-com-btcsuite-btcd-btcec
             go-github-com-gogo-protobuf
             go-github-com-gxed-hashland-keccakpg
             go-github-com-libp2p-go-libp2p-crypto
             go-github-com-minio-blake2b-simd
             go-github-com-minio-sha256-simd
             go-github-com-mr-tron-base58
             go-github-com-multiformats-go-multihash
             go-github-com-spaolacci-murmur3
             go-golang-org-x-crypto))
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

(define-public go-github-com-multiformats-go-multihash
  (let ((commit "97cdb562a04c6ef66d8ed40cd62f8fbcddd396d6")
        (revision "0"))
    (package
      (name "go-github-com-multiformats-go-multihash")
      (version (git-version "1.0.8" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/multiformats/go-multihash")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "02wd9akrwy4y5m0nig9m24p14bjjgb4n1djydrq8cm4yhbvjrrk0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/multiformats/go-multihash"))
      (native-inputs
       (list go-github-com-mr-tron-base58
             go-github-com-gxed-hashland-keccakpg
             go-github-com-minio-blake2b-simd
             go-github-com-minio-sha256-simd
             go-github-com-spaolacci-murmur3
             go-golang-org-x-crypto))
      (home-page "https://github.com/multiformats/go-multihash")
      (synopsis "Multihash implementation in Go")
      (description "Multihash implementation in Go.")
      (license license:expat))))

(define-public go-github-com-quic-go-qtls-go1-20
  (package
    (name "go-github-com-quic-go-qtls-go1-20")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/quic-go/qtls-go1-20")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fl3yv1w8cygag3lav45vvzb4k9i72p92x13wcq0xn13wxirzirn"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/quic-go/qtls-go1-20"
      #:go go-1.20))
    (propagated-inputs
     (list go-golang-org-x-crypto
           go-golang-org-x-sys))
    (home-page "https://github.com/quic-go/qtls-go1-20")
    (synopsis "TLS 1.3 for QUIC")
    (description "Go standard library TLS 1.3 implementation, modified for
QUIC.  For Go 1.20.")
    (license license:expat)))

(define-public go-github-com-protonmail-go-crypto
  (package
    (name "go-github-com-protonmail-go-crypto")
    (version "0.0.0-20220623141421-5afb4c282135")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ProtonMail/go-crypto")
             (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05qxdbn8wdk901z5kw2r3jdrag58nxlcsy0p8xd6rq0d71sw94wy"))))
    (build-system go-build-system)
    (arguments
     (list #:import-path "github.com/ProtonMail/go-crypto"
           #:tests? #f ; Source-only package.
           #:phases
           #~(modify-phases %standard-phases
               ;; Source-only package.
               (delete 'build))))
    (propagated-inputs
     (list go-golang-org-x-crypto))
    (home-page "https://github.com/ProtonMail/go-crypto")
    (synopsis "Fork of x/crypto with up-to-date OpenPGP implementation")
    (description "This package provides cryptography for Go.  This version of
the package is a fork that adds a more up-to-date OpenPGP implementation.  It
is completely backwards compatible with @code{golang.org/x/crypto}, the
official package.")
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
       #:go ,go-1.20
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


;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
