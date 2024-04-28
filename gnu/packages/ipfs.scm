;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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

(define-module (gnu packages ipfs)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells))

(define-public go-github-com-ipfs-go-cid
  (package
    (name "go-github-com-ipfs-go-cid")
    (version "0.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-cid")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gfd5dg0shj2daraai2kkf8sg24jp5cr6dsv857wp4q1ni612a23"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.21
      #:import-path "github.com/ipfs/go-cid"))
    (propagated-inputs
     (list go-github-com-multiformats-go-multihash
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-varint))
    (home-page "https://github.com/ipfs/go-cid")
    (synopsis "Content ID v1 implemented in Go")
    (description
     "Implementation in Go of the @url{https://github.com/ipld/cid, CID spec}.  It is
used in @code{go-ipfs} and related packages to refer to a typed hunk of data.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-cmdkit-files
  (let ((commit
          "386fcf8f18a185ec121676665fe2d9574496048d")
        (revision "0"))
    (package
      (name "go-github-com-ipfs-go-ipfs-cmdkit-files")
      (version (git-version "1.1.3" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://github.com/ipfs/go-ipfs-cmdkit")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "0qk6fshgdmhp8dip2ksm13j6nywi41m9mn0czkvmw6b697z85l2r"))))
      (build-system go-build-system)
      (arguments
       `(#:go ,go-1.16
         #:unpack-path "github.com/ipfs/go-ipfs-cmdkit"
         #:import-path "github.com/ipfs/go-ipfs-cmdkit/files"))
      (home-page "https://github.com/ipfs/go-ipfs-cmdkit")
      (synopsis "Shared types, functions and values for go-ipfs")
      (description "@command{cmdkit} offers some types, functions and values
that are shared between @command{go-ipfs/commands} and its rewrite
@command{go-ipfs-cmds}.")
      (license license:expat))))

(define-public go-github-com-ipfs-go-ipfs-util
  (package
    (name "go-github-com-ipfs-go-ipfs-util")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-ipfs-util")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0x80c6a50zcv473xx0b39sz2xkwpiw3nmmjf51k5x7a4rx0rgvx4"))))
    (build-system go-build-system)
    (propagated-inputs (list go-github-com-mr-tron-base58
                             go-github-com-multiformats-go-multihash))
    (arguments
     (list
      #:go go-1.21
      #:import-path "github.com/ipfs/go-ipfs-util"))
    (home-page "https://github.com/ipfs/go-ipfs-util")
    (synopsis "Common utilities used by @code{go-ipfs} and related packages")
    (description
     "Common utilities used by @code{go-ipfs} and other related Go packages.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-api
  (let ((commit
          "dafc2a13a4389ac1a6c2786e34ab70a4f26d3a3f")
        (revision "0"))
    (package
      (name "go-github-com-ipfs-go-ipfs-api")
      (version (git-version "1.3.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ipfs/go-ipfs-api")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "06kibnwb037sqynk99j07wm8alvxwx3bari9gdax4jv93396kycj"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "github.com/ipfs/go-ipfs-api"
         ;; TODO: Tests fail, might need network access.
         #:tests? #f))
      (native-inputs
       (list go-github-com-ipfs-go-ipfs-cmdkit-files
             go-github-com-libp2p-go-libp2p-metrics
             go-github-com-libp2p-go-flow-metrics
             go-github-com-libp2p-go-libp2p-peer
             go-github-com-libp2p-go-libp2p-protocol
             go-github-com-libp2p-go-libp2p-crypto
             go-github-com-mitchellh-go-homedir
             go-github-com-multiformats-go-multiaddr
             go-github-com-multiformats-go-multiaddr-net
             go-github-com-btcsuite-btcd-btcec
             go-github-com-gogo-protobuf
             go-github-com-minio-blake2b-simd
             go-github-com-minio-sha256-simd
             go-github-com-mr-tron-base58
             go-github-com-multiformats-go-multihash
             go-golang-org-x-crypto
             go-github-com-spaolacci-murmur3
             go-github-com-gxed-hashland-keccakpg
             go-github-com-whyrusleeping-tar-utils
             go-github-com-cheekybits-is))
      (home-page "https://github.com/ipfs/go-ipfs-api")
      (synopsis "Unofficial Go interface to IPFS's HTTP API")
      (description "An unofficial Go interface to IPFS's HTTP API")
      (license license:expat))))

(define-public gx
  (package
    (name "gx")
    (version "0.14.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/gx")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sk20kv3rfsnizgwmcmmr69jb1b2iwzqh9wwwd6wg6x0pnqm8swc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/whyrusleeping/gx"))
    (native-inputs
     (list go-github-com-blang-semver
           go-github-com-gxed-hashland-keccakpg
           go-github-com-ipfs-go-ipfs-api
           go-github-com-ipfs-go-ipfs-cmdkit-files
           go-github-com-libp2p-go-flow-metrics
           go-github-com-libp2p-go-libp2p-crypto
           go-github-com-libp2p-go-libp2p-metrics
           go-github-com-libp2p-go-libp2p-peer
           go-github-com-libp2p-go-libp2p-protocol
           go-github-com-minio-blake2b-simd
           go-github-com-minio-sha256-simd
           go-github-com-mitchellh-go-homedir
           go-github-com-mr-tron-base58
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multiaddr-net
           go-github-com-multiformats-go-multihash
           go-github-com-spaolacci-murmur3
           go-github-com-whyrusleeping-tar-utils
           go-github-com-btcsuite-btcd-btcec
           go-github-com-gogo-protobuf
           go-github-com-sabhiram-go-gitignore
           go-github-com-urfave-cli
           go-github-com-whyrusleeping-json-filter
           go-github-com-whyrusleeping-progmeter
           go-github-com-whyrusleeping-stump
           go-golang-org-x-crypto))
    (home-page "https://github.com/whyrusleeping/gx")
    (synopsis "Package management tool using IPFS")
    (description "@command{gx} is a packaging tool built around the
distributed, content addressed file system IPFS.  It aims to be flexible,
powerful and simple.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-gx-util
  (package
    (inherit gx)
    (name "go-github-com-whyrusleeping-gx-util")
    (arguments
     '(#:unpack-path "github.com/whyrusleeping/gx"
       #:import-path "github.com/whyrusleeping/gx/gxutil"))))

(define-public gx-go
  (package
    (name "gx-go")
    (version "1.9.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/whyrusleeping/gx-go")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0fdy4b3ymqw6hzvvjwq37mfrdmizc8lxm53axw93n3x6118na9jc"))))
    (build-system go-build-system)
    (arguments
     '(#:import-path "github.com/whyrusleeping/gx-go"))
    (native-inputs
     (list go-github-com-whyrusleeping-gx-util
           go-github-com-kr-fs
           go-github-com-gxed-hashland-keccakpg
           go-github-com-ipfs-go-ipfs-api
           go-github-com-ipfs-go-ipfs-cmdkit-files
           go-github-com-libp2p-go-flow-metrics
           go-github-com-libp2p-go-libp2p-crypto
           go-github-com-libp2p-go-libp2p-metrics
           go-github-com-libp2p-go-libp2p-peer
           go-github-com-libp2p-go-libp2p-protocol
           go-github-com-minio-blake2b-simd
           go-github-com-minio-sha256-simd
           go-github-com-mitchellh-go-homedir
           go-github-com-mr-tron-base58
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multiaddr-net
           go-github-com-multiformats-go-multihash
           go-github-com-spaolacci-murmur3
           go-github-com-whyrusleeping-tar-utils
           go-github-com-btcsuite-btcd-btcec
           go-github-com-gogo-protobuf
           go-github-com-sabhiram-go-gitignore
           go-github-com-urfave-cli
           go-github-com-whyrusleeping-progmeter
           go-github-com-whyrusleeping-stump
           go-golang-org-x-crypto))
    (home-page "https://github.com/whyrusleeping/gx-go")
    (synopsis "Golang subtool for the @command{gx} package manager")
    (description "A subtool for the @command{gx} package manager for packages
written in Go.")
    (license license:expat)))

(define-public kubo
  (package
    (name "kubo")
    (version "0.28.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append
             "https://dist.ipfs.io/kubo/v" version
             "/kubo-source.tar.gz"))
       (sha256
        (base32 "01lk6dd2j660rykchc3ggi9cln92cngz5ijlldsaj5mxnajlvbwy"))
       (file-name (string-append name "-" version "-source"))
       (modules '((guix build utils)))
       (snippet '(for-each delete-file-recursively
                           ;; TODO: unbundle the rest as well
                           '("vendor/bazil.org"
                             "vendor/github.com/alecthomas"
                             "vendor/github.com/benbjohnson"
                             "vendor/github.com/beorn7/perks"
                             "vendor/github.com/blang"
                             "vendor/github.com/cespare"
                             "vendor/github.com/davecgh"
                             "vendor/github.com/dustin"
                             "vendor/github.com/flynn"
                             "vendor/github.com/francoispqt"
                             "vendor/github.com/fsnotify"
                             "vendor/github.com/gogo"
                             "vendor/github.com/golang/groupcache"
                             "vendor/github.com/golang/snappy"
                             "vendor/github.com/google/uuid"
                             "vendor/github.com/gorilla"
                             "vendor/github.com/hashicorp"
                             "vendor/github.com/ipfs/go-cid"
                             "vendor/github.com/jackpal"
                             "vendor/github.com/jbenet"
                             "vendor/github.com/julienschmidt"
                             "vendor/github.com/klauspost"
                             "vendor/github.com/mattn"
                             "vendor/github.com/mgutz"
                             "vendor/github.com/minio"
                             "vendor/github.com/mitchellh"
                             "vendor/github.com/mr-tron"
                             "vendor/github.com/multiformats"
                             "vendor/github.com/opentracing"
                             "vendor/github.com/pbnjay"
                             "vendor/github.com/pkg"
                             "vendor/github.com/pmezard"
                             "vendor/github.com/prometheus/client_golang"
                             "vendor/github.com/prometheus/client_model"
                             "vendor/github.com/prometheus/common"
                             "vendor/github.com/prometheus/procfs"
                             "vendor/github.com/spaolacci"
                             "vendor/github.com/stretchr"
                             "vendor/github.com/syndtr"
                             "vendor/github.com/whyrusleeping/go-sysinfo"
                             "vendor/go.uber.org"
                             "vendor/golang.org"
                             "vendor/gopkg.in"
                             "vendor/lukechampine.com")))))
    (build-system go-build-system)
    (arguments
     (list
      #:unpack-path "github.com/ipfs/kubo"
      #:import-path "github.com/ipfs/kubo/cmd/ipfs"
      #:go go-1.21
      #:phases
      #~(modify-phases %standard-phases
          ;; https://github.com/ipfs/kubo/blob/master/docs/command-completion.md
          (add-after 'install 'install-bashcompletion
            (lambda _
              (let ((completiondir (string-append #$output
                                                  "/etc/bash_completion.d")))
                (mkdir-p completiondir)
                (with-output-to-file (string-append completiondir "/ipfs")
                  (lambda _
                    (invoke #$(if (%current-target-system)
                                  "ipfs"
                                  #~(string-append #$output "/bin/ipfs"))
                            "commands" "completion" "bash")))))))))
    (inputs (list ;; Direct requirements as seen in kubo's go.mod file.
                  ;;
                  ;; XXX: Uncomment out when package is available in Guix,
                  ;; otherwise it will be sourced from provided vendor
                  ;; directory.
                  ;;
                  go-bazil-org-fuse
                  ;;go-contrib-go-opencensus-io-exporter-prometheus
                  go-github-com-benbjohnson-clock
                  go-github-com-blang-semver-v4
                  ;;go-github-com-cenkalti-backoff-v4
                  ;;go-github-com-ceramicnetwork-go-dag-jose
                  ;;go-github-com-cheggaaa-pb
                  ;;go-github-com-coreos-go-systemd-v22
                  go-github-com-dustin-go-humanize
                  ;;go-github-com-elgris-jsondiff
                  ;;go-github-com-facebookgo-atomicfile
                  go-github-com-fsnotify-fsnotify
                  go-github-com-google-uuid
                  go-github-com-hashicorp-go-multierror
                  ;;go-github-com-ipfs-boxo
                  ;;go-github-com-ipfs-go-block-format
                  go-github-com-ipfs-go-cid
                  ;;go-github-com-ipfs-go-cidutil
                  ;;go-github-com-ipfs-go-datastore
                  ;;go-github-com-ipfs-go-detect-race
                  ;;go-github-com-ipfs-go-ds-badger
                  ;;go-github-com-ipfs-go-ds-flatfs
                  ;;go-github-com-ipfs-go-ds-leveldb
                  ;;go-github-com-ipfs-go-ds-measure
                  ;;go-github-com-ipfs-go-fs-lock
                  ;;go-github-com-ipfs-go-ipfs-cmds
                  ;;go-github-com-ipfs-go-ipld-cbor
                  ;;go-github-com-ipfs-go-ipld-format
                  ;;go-github-com-ipfs-go-ipld-git
                  ;;go-github-com-ipfs-go-ipld-legacy
                  ;;go-github-com-ipfs-go-log
                  ;;go-github-com-ipfs-go-log-v2
                  ;;go-github-com-ipfs-go-metrics-interface
                  ;;go-github-com-ipfs-go-metrics-prometheus
                  ;;go-github-com-ipfs-go-unixfsnode
                  ;;go-github-com-ipfs-shipyard-nopfs
                  ;;go-github-com-ipfs-shipyard-nopfs-ipfs
                  ;;go-github-com-ipld-go-car
                  ;;go-github-com-ipld-go-car-v2
                  ;;go-github-com-ipld-go-codec-dagpb
                  ;;go-github-com-ipld-go-ipld-prime
                  go-github-com-jbenet-go-random
                  go-github-com-jbenet-go-temp-err-catcher
                  go-github-com-jbenet-goprocess
                  go-github-com-julienschmidt-httprouter
                  ;;go-github-com-libp2p-go-doh-resolver
                  ;;go-github-com-libp2p-go-libp2p
                  ;;go-github-com-libp2p-go-libp2p-http
                  ;;go-github-com-libp2p-go-libp2p-kad-dht
                  ;;go-github-com-libp2p-go-libp2p-kbucket
                  ;;go-github-com-libp2p-go-libp2p-pubsub
                  ;;go-github-com-libp2p-go-libp2p-pubsub-router
                  ;;go-github-com-libp2p-go-libp2p-record
                  ;;go-github-com-libp2p-go-libp2p-routing-helpers
                  ;;go-github-com-libp2p-go-libp2p-testing
                  ;;go-github-com-libp2p-go-socket-activation
                  go-github-com-mitchellh-go-homedir
                  go-github-com-multiformats-go-multiaddr-0.12
                  go-github-com-multiformats-go-multiaddr-dns
                  go-github-com-multiformats-go-multibase
                  go-github-com-multiformats-go-multicodec
                  go-github-com-multiformats-go-multihash
                  go-github-com-opentracing-opentracing-go
                  go-github-com-pbnjay-memory
                  go-github-com-pkg-errors
                  go-github-com-prometheus-client-golang
                  go-github-com-stretchr-testify
                  go-github-com-syndtr-goleveldb-leveldb
                  ;;go-github-com-tidwall-gjson
                  ;;go-github-com-tidwall-sjson
                  go-github-com-whyrusleeping-go-sysinfo
                  ;;go-github-com-whyrusleeping-multiaddr-filter
                  ;;go-go-opencensus-io
                  ;;go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                  ;;go-go-opentelemetry-io-contrib-propagators-autoprop
                  ;;go-go-opentelemetry-io-otel
                  ;;go-go-opentelemetry-io-otel-sdk
                  ;;go-go-opentelemetry-io-otel-trace
                  go-go-uber-org-dig
                  go-go-uber-org-fx
                  go-go-uber-org-multierr
                  go-go-uber-org-zap
                  go-golang-org-x-crypto
                  go-golang-org-x-exp-2023
                  go-golang-org-x-mod
                  go-golang-org-x-sync
                  go-golang-org-x-sys
                  go-google-golang-org-protobuf
                  go-gopkg-in-yaml-v3

                  ;;
                  ;; A list of indirect dependencies requiring for the vendored models.
                  ;; XXX: Remove them when all of the vendored packages are available.
                  ;;
                  go-github-com-alecthomas-units
                  go-github-com-flynn-noise
                  go-github-com-francoispqt-gojay
                  go-github-com-gogo-protobuf
                  go-github-com-golang-groupcache
                  go-github-com-gorilla-mux
                  go-github-com-gorilla-websocket
                  go-github-com-hashicorp-golang-lru
                  go-github-com-hashicorp-golang-lru-v2
                  go-github-com-jackpal-go-nat-pmp
                  go-github-com-klauspost-compress
                  go-github-com-mattn-go-runewidth
                  go-github-com-mgutz-ansi
                  go-github-com-multiformats-go-multiaddr-fmt
                  go-github-com-multiformats-go-multistream
                  go-golang-org-x-oauth2
                  go-golang-org-x-term
                  go-golang-org-x-text
                  go-golang-org-x-xerrors
                  go-gopkg-in-square-go-jose-v2))
    (native-inputs
     (append (if (%current-target-system)
                 (list this-package)
                 '())
             (list python-minimal-wrapper zsh)))
    (home-page "https://ipfs.tech")
    (synopsis "Go implementation of IPFS, a peer-to-peer hypermedia protocol")
    (description "IPFS is a global, versioned, peer-to-peer file system.  It
combines good ideas from Git, BitTorrent, Kademlia, SFS, and the Web.  It is
like a single bittorrent swarm, exchanging git objects.  IPFS provides an
interface as simple as the HTTP web, but with permanence built in.  You can
also mount the world at @code{/ipfs}.")
    (license license:expat)))

(define-public go-ipfs
  (deprecated-package "go-ipfs" kubo))
