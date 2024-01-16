;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2020 Leo Famulari <leo@famulari.name>
;;; Copyright © 2021 Raghav Gururajan <rg@raghavgururajan.name>
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

;;;
;;; Avoid adding new packages to the end of this file. To reduce the chances
;;; of a merge conflict, place them above by existing packages with similar
;;; functionality or similar names.
;;;
