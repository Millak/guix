;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023, 2024, 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024-2025 Sharlatan Hellseher <sharlatanus@gmail.com>
;;; Copyright © 2024 Giacomo Leidi <goodoldpaul@autistici.org>
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
  #:use-module (guix utils)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
  #:use-module (gnu packages golang-maths)
  #:use-module (gnu packages golang-web)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages prometheus)
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages specifications))

(define-public go-github-com-ceramicnetwork-go-dag-jose
  (package
    (name "go-github-com-ceramicnetwork-go-dag-jose")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ceramicnetwork/go-dag-jose")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g34dwlnq07zg176bdhp2hcg1hg5l55s0a6hk4kiq37vm01w68j7"))
       (snippet
        #~(begin (use-modules (guix build utils))
                 ;; Delete git submodule.
                 (delete-file-recursively ".ipld")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ceramicnetwork/go-dag-jose"
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
    (native-inputs
     (list go-github-com-stretchr-testify
           go-github-com-frankban-quicktest
           go-github-com-warpfork-go-testmark))
    (propagated-inputs
     (list go-github-com-go-jose-go-jose-v4
           go-github-com-ipfs-go-cid
           go-github-com-ipld-go-ipld-prime
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-multihash
           go-golang-org-x-crypto
           go-pgregory-net-rapid))
    (home-page "https://github.com/ceramicnetwork/go-dag-jose")
    (synopsis "Implementation of the IPLD dag-jose codec")
    (description
     "This is an implementation of the IPLD
@@url{https://ipld.io/specs/codecs/dag-jose/spec/,dag-jose codec}.")
    (license license:expat)))

(define-public go-github-com-ipfs-bbloom
  (package
    (name "go-github-com-ipfs-bbloom")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/bbloom")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0dcdn7nlysynl7yrbivv8m7j83jq7pabhcff8mvfjdk583rgnkp2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/bbloom"))
    (home-page "https://github.com/ipfs/bbloom")
    (synopsis "Fast bit set Bloom filter")
    (description
     "This package implements a fast bloom filter with real @code{bitset} and
JSONMarshal/JSONUnmarshal to store/reload the Bloom filter.")
    (license (list license:expat             ; bbloom.go
                   license:public-domain)))) ; siphash.go

(define-public go-github-com-ipfs-boxo
  (package
    (name "go-github-com-ipfs-boxo")
    (version "0.33.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/boxo")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03lr70406w95xqiin07ph0d2x8hcfdj1k1r3f1yq8lv3g5j9wapp"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packed as
            ;; separated packages:
            ;;
            ;; - github.com/ipfs/boxo/cmd/boxo-migrate
            ;; - github.com/ipfs/boxo/cmd/deprecator
            ;; - github.com/ipfs/boxo/examples
            (for-each delete-file-recursively
                      (list "cmd" "examples"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:embed-files #~(list "sorted-network-list.bin")
      #:import-path "github.com/ipfs/boxo"
      #:test-flags
      #~(list "-skip" (string-join
                       ;; Network access is required.
                       (list "TestAddNewDNSResolver"
                             "TestOverrideDNSDefaults")
                       "|"))))
    (native-inputs
     (list go-github-com-libp2p-go-libp2p-kad-dht-bootstrap
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-alecthomas-units
           go-github-com-cespare-xxhash-v2
           go-github-com-crackcomm-go-gitignore
           go-github-com-cskr-pubsub
           go-github-com-dustin-go-humanize
           go-github-com-filecoin-project-go-clock
           go-github-com-gabriel-vasile-mimetype
           go-github-com-gammazero-chanqueue
           go-github-com-gammazero-deque
           go-github-com-google-uuid
           go-github-com-gorilla-mux
           go-github-com-hashicorp-golang-lru-v2
           go-github-com-ipfs-bbloom
           go-github-com-ipfs-go-bitfield
           go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-cidutil
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-detect-race
           go-github-com-ipfs-go-ipfs-delay
           go-github-com-ipfs-go-ipfs-redirects-file
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipfs-go-ipld-legacy
           go-github-com-ipfs-go-log-v2
           go-github-com-ipfs-go-metrics-interface
           go-github-com-ipfs-go-peertaskqueue
           go-github-com-ipfs-go-test
           go-github-com-ipfs-go-unixfsnode
           go-github-com-ipld-go-car-v2
           go-github-com-ipld-go-codec-dagpb
           go-github-com-ipld-go-ipld-prime
           go-github-com-libp2p-go-buffer-pool
           go-github-com-libp2p-go-doh-resolver
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-record
           go-github-com-libp2p-go-libp2p-routing-helpers
           go-github-com-libp2p-go-libp2p-testing
           go-github-com-libp2p-go-msgio
           go-github-com-miekg-dns
           go-github-com-mr-tron-base58
           go-github-com-multiformats-go-base32
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multiaddr-dns
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash
           go-github-com-multiformats-go-multistream
           go-github-com-polydawn-refmt
           go-github-com-prometheus-client-golang
           go-github-com-samber-lo
           go-github-com-slok-go-http-metrics
           go-github-com-spaolacci-murmur3
           go-github-com-whyrusleeping-base32
           go-github-com-whyrusleeping-chunker
           go-go-opencensus-io
           go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracegrpc
           go-go-opentelemetry-io-otel-exporters-otlp-otlptrace-otlptracehttp
           go-go-opentelemetry-io-otel-exporters-stdout-stdouttrace
           go-go-opentelemetry-io-otel-exporters-zipkin
           go-go-opentelemetry-io-otel-sdk
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-multierr
           go-go-uber-org-zap
           go-golang-org-x-exp
           go-golang-org-x-oauth2
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-google-golang-org-protobuf))
    (home-page "https://github.com/ipfs/boxo")
    (synopsis "Collection of reference libraries for building IPFS applications")
    (description
     "This package provides a set of libraries for building IPFS applications
and implementations in Golang.

Included subpackaged:
@itemize
@item @code{bitswap} - implementation of the bitswap protocol, the data
trading module for ipfs

@item @code{blockservice} - implements a BlockService interface that
provides a single GetBlock/AddBlock interface that seamlessly retrieves data
either locally or from a remote peer through the exchange

@item @code{blockstore} - implements a thin wrapper over a datastore,
giving a clean interface for Getting and Putting block objects

@item @code{datastore/dshelp} - provides utilities for parsing and creating
datastore keys used by go-ipfs

@item @code{exchange} - defines the IPFS exchange interface

@item @code{files} - file interfaces and utils used in Golang implementations
of IPFS

@item @code{filestore} - implements a Blockstore which is able to read certain
blocks of data directly from its original location in the filesystem

@item @code{ipld/unixfs} - provides additinoal @code{importer}, @code{io},
@code{mod}, @code{hamt}, @code{archive} and @code{test} packages

@item @code{ipld/merkledag} - implements the IPFS Merkle DAG data structures

@item @code{ipns} - reference implementation of the IPNS Record and
Verification specification

@item @code{mfs} - implements an in memory model of a mutable IPFS filesystem

@item @code{path} - contains utilities to work with IPFS paths
@end itemize")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipfs-go-bitfield
  (package
    (name "go-github-com-ipfs-go-bitfield")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-bitfield")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zhgwdg2kizhk0hb9q5p0pwrwldd2pacz8l1pnapxh6qm3fqs663"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-bitfield"))
    (home-page "https://github.com/ipfs/go-bitfield")
    (synopsis "Allocated up-front Bitfield for Golang")
    (description
     "This package implements a functionality similar to standard
@code{big.Int} with some optimizations to use in IPFS.")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipfs-go-block-format
  (package
    (name "go-github-com-ipfs-go-block-format")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-block-format")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pd8ww06ss922g3w2fgi3w0q66y2mkb9b2q9x5qxabrjj65xranz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-block-format"))
    (propagated-inputs
     (list go-github-com-multiformats-go-multihash
           go-github-com-multiformats-go-varint
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-ipfs-util))
    (home-page "https://github.com/ipfs/go-block-format")
    (synopsis "Set of interfaces for CID addressable blocks of data")
    (description
     "Package @code{blocks} contains the lowest level of @acronym{IPLD,
InterPlanetary Linked Data} data structures.  A block is raw data accompanied
by a @acronym{Content Identifiers,CID}.  The CID contains the multihash
corresponding to the block.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-blockservice
  (package
    (name "go-github-com-ipfs-go-blockservice")
    (version "0.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-blockservice")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1pg6mj0iwlisp0sk9dng9663vvxbcnxjmbb62nkdfaf4dkbs920c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-blockservice"
      ;; Mock tests intoruduce cycle with Boxo (go-libipfs), run just a
      ;; portion of tests.
      #:test-subdirs #~(list ".")))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ipfs-blockstore
           go-github-com-ipfs-go-ipfs-blocksutil
           go-github-com-ipfs-go-ipfs-delay
           go-github-com-ipfs-go-ipfs-exchange-interface
           go-github-com-ipfs-go-ipfs-exchange-offline
           go-github-com-ipfs-go-ipfs-routing
           go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipfs-go-log-v2
           go-github-com-ipfs-go-verifcid
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-trace))
    (home-page "https://github.com/ipfs/go-blockservice")
    (synopsis "Combines local and remote storage seamlessly")
    (description
     "Package blockservice implements a @code{BlockService} interface that
provides a single @code{GetBlock/AddBlock} interface that seamlessly retrieves
data either locally or from a remote peer through the exchange.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-cidutil
  (package
    (name "go-github-com-ipfs-go-cidutil")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-cidutil")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j18wf42rfxrrh2fjdbjsjvjqxwgvg46b9wl6y5ig22fx5hvpm1n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-cidutil"))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-cidutil")
    (synopsis "Utility functions and types for working with CIDs")
    (description
     "@code{go-cidutil} implements various utilities and helper functions for working
with @url{https://github.com/ipld/cid, CIDs}.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ds-badger4
  (package
    (name "go-github-com-ipfs-go-ds-badger4")
    (version "0.1.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-badger4")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gyzywd74cb8jl0zr8b3fjjmd8rsabwk5rj17nhagigps4971h1a"))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.23
      #:import-path "github.com/ipfs/go-ds-badger4"))
    (native-inputs
     (list go-github-com-stretchr-testify
           go-go-uber-org-zap))
    (propagated-inputs
     (list go-github-com-dgraph-io-badger-v4
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-detect-race
           go-github-com-ipfs-go-log-v2))
    (home-page "https://github.com/ipfs/go-ds-badger4")
    (synopsis "Datastore implementation using Badger v4 as backend")
    (description
     "This package implements a Badger v4 (a key-value database) backed
datastore for IPFS.")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipfs-go-ds-dynamodb
  (package
    (name "go-github-com-ipfs-go-ds-dynamodb")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-dynamodb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1z155ajpx52v71hsdjz5950z1w0qmp6c7lsqmsa9qyn7x21g7p0g"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; tests need Docker or local DynamoDB Java app
      #:import-path "github.com/ipfs/go-ds-dynamodb"))
    (propagated-inputs
     (list go-github-com-aws-aws-sdk-go
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-log-v2))
    (home-page "https://github.com/ipfs/go-ds-dynamodb")
    (synopsis "DynamoDB datastore implementation")
    (description
     "This is an implementation of @url{https://github.com/ipfs/go-datastore,
go-datastore} that is backed by @code{DynamoDB}.

ddbds includes support for optimized prefix queries.  When you setup your
table's key schema correctly and register it with ddbds, then incoming queries
that match the schema will be converted into DynamoDB queries instead of table
scans, enabling high performance, ordered, high-cardinality prefix queries.")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipfs-go-ds-flatfs
  (package
    (name "go-github-com-ipfs-go-ds-flatfs")
    (version "0.5.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-flatfs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m5fxxdn2zj9i9nm8fdyll4wcgaknfg3vj07959mj06hwc9r3z3c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ds-flatfs"))
    (propagated-inputs
     (list go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-log
           go-github-com-jbenet-goprocess))
    (home-page "https://github.com/ipfs/go-ds-flatfs")
    (synopsis "Datastore with sharded directories and flat files")
    (description
     "Package flatfs is a Datastore implementation that stores all objects in
a two-level directory structure in the local file system, regardless of the
hierarchy of the keys.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ds-measure
  (package
    (name "go-github-com-ipfs-go-ds-measure")
    (version "0.2.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-measure")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14p3681sb1h81qxkwblngvy72yrd73drcvamgc3v7za734l9f54b"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ds-measure"))
    (propagated-inputs
     (list go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-metrics-interface))
    (home-page "https://github.com/ipfs/go-ds-measure")
    (synopsis "Datastore implementation that keeps metrics on all calls made")
    (description
     "Package measure provides a Datastore wrapper that records metrics using
@url{https://github.com/ipfs/go-metrics-interface}.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ds-pebble
  (package
    (name "go-github-com-ipfs-go-ds-pebble")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-pebble")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0l0abcknray9hvk5j3vdiybgjk7yn6j3awznpy46j08g97z88ljw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ds-pebble"))
    (propagated-inputs
     (list go-github-com-cockroachdb-pebble-v2
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-log-v2))
    (home-page "https://github.com/ipfs/go-ds-pebble")
    (synopsis "Pebble-backed datastore")
    (description
     "This is a simple adapter to plug in
@url{https://github.com/cockroachdb/pebble, cockroachdb/pebble} as a backend
anywhere that accepts a @url{https://github.com/ipfs/go-datastore,
go-datastore}.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-ipfs-go-fs-lock
  (package
    (name "go-github-com-ipfs-go-fs-lock")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-fs-lock")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14m5038067px6vvqyvx6449f0f62nj8d0p5hkf9rj8ykfahs80ff"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-fs-lock"))
    (propagated-inputs
     (list go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-log-v2
           go-go4-org
           go-golang-org-x-sys))
    (home-page "https://github.com/ipfs/go-fs-lock")
    (synopsis "Filesystem based locking")
    (description
     "This package implements a filesystem based locking.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-blockstore
  (package
    (name "go-github-com-ipfs-go-ipfs-blockstore")
    (version "1.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-blockstore")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a3a0fm8k8njdlq2w795qff01piadjfp6r5r2hww69fxqsplln9l"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-blockstore"))
    (propagated-inputs
     (list go-github-com-hashicorp-golang-lru
           go-github-com-ipfs-bbloom
           go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore-0.6
           go-github-com-ipfs-go-ipfs-ds-help
           go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipfs-go-log
           go-github-com-ipfs-go-metrics-interface
           go-github-com-multiformats-go-multihash
           go-go-uber-org-atomic))
    (home-page "https://github.com/ipfs/go-ipfs-blockstore")
    (synopsis "Caching wrapper over a IPFS datastore")
    (description
     "@code{go-ipfs-blockstore} implements a thin wrapper over an IPFS
datastore, giving a clean interface for getting and putting block objects.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-blocksutil
  (package
    (name "go-github-com-ipfs-go-ipfs-blocksutil")
    (version "0.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-blocksutil")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g6b4b2b5wp5r0dh20qdfdm76qnh421y8lgz4381r02q2flh57dv"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-blocksutil"))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format))
    (home-page "https://github.com/ipfs/go-ipfs-blocksutil")
    (synopsis "Utility functions for working with IPFS blocks")
    (description
     "This package provides an utility functions for working with
@url{https://github.com/ipfs/go-block-format, IPFS blocks}.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-cmds
  (package
    (name "go-github-com-ipfs-go-ipfs-cmds")
    (version "0.14.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-cmds")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0npgcwzxzgvygl9r9h5kbnfl1dh0hygmwk1jj1hwznyvj47x6lwl"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-cmds"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (propagated-inputs
     (list go-github-com-ipfs-boxo
           go-github-com-ipfs-go-log
           go-github-com-rs-cors
           go-github-com-texttheater-golang-levenshtein
           go-golang-org-x-term))
    (home-page "https://github.com/ipfs/go-ipfs-cmds")
    (synopsis "IPFS commands package")
    (description
     "Package cmds helps building both standalone and client-server
applications.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-delay
  (package
    (name "go-github-com-ipfs-go-ipfs-delay")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-delay")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a5acj622sk1hibnh893mya4h86nsy1dan0wlh9q444c04iqpviw"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-delay"))
    (home-page "https://github.com/ipfs/go-ipfs-delay")
    (synopsis "Configurable delays to other objects")
    (description
     "This package implements a threadsafe configurable delays to other
objects.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-ds-help
  (package
    (name "go-github-com-ipfs-go-ipfs-ds-help")
    (version "1.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-ds-help")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xmn9pdyrcim9ahqs9pkh0c9ac71gilb3pb48kcagq8zxf22i4bj"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-ds-help"))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-multiformats-go-base32
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-ipfs-ds-help")
    (synopsis "Utilities for parsing and creating datastore keys")
    (description
     "@code{go-ipfs-ds-help} provides utilities for parsing and creating datastore
keys used by @code{go-ipfs} (Kubo).")
    (license license:expat)))

(define-public go-github-com-ipfs-go-datastore
  (package
    (name "go-github-com-ipfs-go-datastore")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-datastore")
              (commit (string-append "v" version))))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/ipfs/go-datastore/fuzz
            (delete-file-recursively "fuzz")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02n38i09f8ffr894fzlsl80ahf32mpap5q004acz9cdg9a67pdz3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-datastore"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key tests? import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file-recursively
                          (list "examples"))))))))
    (native-inputs
     (list go-gopkg-in-check-v1))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-ipfs-go-detect-race
           go-github-com-ipfs-go-ipfs-delay
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-multierr))
    (home-page "https://github.com/ipfs/go-datastore")
    (synopsis "Key-value datastore interfaces")
    (description
     "Datastore is a generic layer of abstraction for data store and database access.
It is a simple API with the aim to enable application development in a
datastore-agnostic way, allowing datastores to be swapped seamlessly without
changing application code.  Thus, one can leverage different datastores with
different strengths without committing the application to one datastore
throughout its lifetime.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-datastore-0.6
  (package
    (inherit go-github-com-ipfs-go-datastore)
    (name "go-github-com-ipfs-go-datastore")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-datastore")
              (commit (string-append "v" version))))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/ipfs/go-datastore/fuzz
            (delete-file-recursively "fuzz")))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xbhh3gm7bgd2d1p821w8gmbh87aix1g1ynhbl7gjaxxyhrsh68n"))))
    (propagated-inputs
     (list go-github-com-google-uuid
           go-github-com-ipfs-go-detect-race
           go-github-com-ipfs-go-ipfs-delay
           go-github-com-jbenet-goprocess
           go-go-uber-org-multierr
           go-golang-org-x-xerrors))))

(define-public go-github-com-ipfs-go-ds-badger
  (package
    (name "go-github-com-ipfs-go-ds-badger")
    (version "0.3.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-badger")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0ywq8cs6yf5vqfbw6rd5pw79sqljd87qi0ykv4gg736s7g7mkjr6"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Tests time out, figure out workaround.
      #:tests? #f
      #:import-path "github.com/ipfs/go-ds-badger"))
    (propagated-inputs
     (list go-github-com-dgraph-io-badger
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-log-v2
           go-github-com-jbenet-goprocess))
    (home-page "https://github.com/ipfs/go-ds-badger")
    (synopsis "Datastore implementation using badger as backend")
    (description
     "This package provides a @acronym{DS, Data Store} implementation backed
by @url{https://dgraph.io/docs/badger,Badger}.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ds-leveldb
  (package
    (name "go-github-com-ipfs-go-ds-leveldb")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ds-leveldb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1hi7vmjpzzh00zcf0638rvgiqj4j8difz5kzr0pilr0z6zcb7dq3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ds-leveldb"))
    (propagated-inputs
     (list go-github-com-ipfs-go-datastore go-github-com-syndtr-goleveldb))
    (home-page "https://github.com/ipfs/go-ds-leveldb")
    (synopsis "Implementation of go-datastore using leveldb")
    (description
     "This package implements the
@url{https://github.com/ipfs/go-datastore,go-datastore} interface using a
LevelDB backend.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-detect-race
  (package
    (name "go-github-com-ipfs-go-detect-race")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-detect-race")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rqb0q66d7z852j5mhlr025dz698c44w014g4mx587amr1rvwqna"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-detect-race"))
    (home-page "https://github.com/ipfs/go-detect-race")
    (synopsis "Detect if compiled with race")
    (description "Check if the race detector is running.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-exchange-interface
  (package
    (name "go-github-com-ipfs-go-ipfs-exchange-interface")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-exchange-interface")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h5jizhjq4yz9sikqc6yhv5gsb8fgv67v0qjzagyhfznfx8kwv1d"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-exchange-interface"))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid))
    (home-page "https://github.com/ipfs/go-ipfs-exchange-interface")
    (synopsis "The IPFS Exchange interface")
    (description
     "@code{go-ipfs-exchange-interface} defines the IPFS exchange interface.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-exchange-offline
  (package
    (name "go-github-com-ipfs-go-ipfs-exchange-offline")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-exchange-offline")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ad28xbqbxc93ckjnlifbk7p58qis7ayfpndav33n6b4sq6s2a0r"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-exchange-offline"))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ipfs-blockstore
           go-github-com-ipfs-go-ipfs-blocksutil
           go-github-com-ipfs-go-ipfs-exchange-interface
           go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-ipld-format))
    (home-page "https://github.com/ipfs/go-ipfs-exchange-offline")
    (synopsis "Offline IPFS exchange implementation")
    (description
     "This package implements an object that implements the exchange interface
but returns nil values to every request.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-pq
  (package
    (name "go-github-com-ipfs-go-ipfs-pq")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-pq")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zi177lysf6f2zap9l0yxcw4r2xvjiji1yb1jzdmiif2ahvi0v84"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-pq"
      ;; By using Go version higher than 1.21 one test keep failing with
      ;; error: the values were not returned in sorted order.
      #:test-flags #~(list "-skip" "TestCorrectnessOfPop")))
    (home-page "https://github.com/ipfs/go-ipfs-pq")
    (synopsis "Priority queue used by go-ipfs")
    (description "Package pq implements a priority queue.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-redirects-file
  (package
    (name "go-github-com-ipfs-go-ipfs-redirects-file")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-redirects-file")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "16lisd5jbniang85jzfwfigw0kmz73913fsrfj49nh92mpw50qpz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipfs-redirects-file"))
    (native-inputs
     (list go-github-com-tj-assert))
    (propagated-inputs
     (list go-github-com-pkg-errors
           go-github-com-ucarion-urlpath))
    (home-page "https://github.com/ipfs/go-ipfs-redirects-file")
    (synopsis "IPFS Web Gateway _redirects file format parser")
    (description
     "Package redirects provides Netlify style _redirects file format
parsing.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-routing
  (package
    (name "go-github-com-ipfs-go-ipfs-routing")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipfs-routing")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0j5pyp35z0jnqzz7iiknqx3234swqrpqz478ng8al3b6gqaza9ks"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t
      #:import-path "github.com/ipfs/go-ipfs-routing"
      ;; One test fails with error: panic: protobuf tag not enough fields in
      ;; Record.state.
      #:test-flags #~(list "-skip" "TestOfflineRouterStorage")))
    (native-inputs
     (list go-github-com-libp2p-go-libp2p-testing))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ipfs-delay
           go-github-com-ipfs-go-ipfs-ds-help
           go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-log
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-record
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-ipfs-routing")
    (synopsis "P2P routing implementations used in IPFS")
    (description
     "This package provides P2P routing implementations for IPFS
development.")
    (license license:expat)))

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
      #:import-path "github.com/ipfs/go-ipfs-util"))
    (home-page "https://github.com/ipfs/go-ipfs-util")
    (synopsis "Common utilities used by @code{go-ipfs} and related packages")
    (description
     "Common utilities used by @code{go-ipfs} and other related Go packages.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipld-cbor
  (package
    (name "go-github-com-ipfs-go-ipld-cbor")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipld-cbor")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0yxk4sbf1fk9aaizzpz3h30049wqvaz0s3jnbdd5akhj7wg89h21"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipld-cbor"))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-ipld-format
           go-github-com-multiformats-go-multihash
           go-github-com-polydawn-refmt
           go-github-com-whyrusleeping-cbor-gen))
    (home-page "https://github.com/ipfs/go-ipld-cbor")
    (synopsis "@acronym{Concise Binary Object Representation, CBOR}
implementation of @code{go-ipld-format}")
    (description
     "An implementation of a @url{https://cbor.io/, CBOR} encoded merkledag object.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipld-format
  (package
    (name "go-github-com-ipfs-go-ipld-format")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipld-format")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0zl172ncmx9h5z2p3d0j1377xm9glw4zfyamks31p0pvvx2kyn7c"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipld-format"))
    (propagated-inputs
     (list go-github-com-multiformats-go-multihash
           go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid))
    (home-page "https://github.com/ipfs/go-ipld-format")
    (synopsis "IPLD Node and Resolver interfaces in Go")
    (description
     "@code{go-ipld-format} is a set of interfaces that a type needs to implement in
order to be a part of the @acronym{IPLD, InterPlanetary Linked Data} merkle-forest.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipld-git
  (package
    (name "go-github-com-ipfs-go-ipld-git")
    (version "0.1.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipld-git")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1v52qzgmx7qym0qzkzkry2kfj58f9hh7c8qycg74sqbd9zb1ynjj"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: It requires .git/objects, check if it's applicable to generate
      ;; git repo during check phase with make-test-repo.sh.
      #:tests? #f
      #:import-path "github.com/ipfs/go-ipld-git"
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-test-data-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (for-each delete-file
                          (list "testdata.tar.gz"
                                "codecov.yml"
                                "make-test-repo.sh"))))))))
    (propagated-inputs
     (list go-github-com-multiformats-go-multihash
           go-github-com-ipld-go-ipld-prime
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-block-format))
    (home-page "https://github.com/ipfs/go-ipld-git")
    (synopsis "IPLD handlers for git objects")
    (description
     "This is an IPLD codec which handles git objects.  Objects are transformed into
IPLD graph as detailed below.  Objects are demonstrated here using both
@url{https://ipld.io/docs/schemas/,IPLD Schemas} and example JSON forms.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipld-legacy
  (package
    (name "go-github-com-ipfs-go-ipld-legacy")
    (version "0.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-ipld-legacy")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0sc2zc3lyxy30fzynwdpfrl8jhh1ynwixn1crrv8hzn93yix6550"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ipld-legacy"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipld-go-ipld-prime))
    (home-page "https://github.com/ipfs/go-ipld-legacy")
    (synopsis "Translation layer for IPLD legacy code")
    (description
     "@code{go-ipld-format} is a translation layer between
@code{go-ipld-prime} nodes and @code{go-ipld-format} legacy interface.")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipfs-go-merkledag
  (package
    (name "go-github-com-ipfs-go-merkledag")
    (version "0.11.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-merkledag")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0g1hrk2iw7gvk5qyv5avcc2idkc13w8agz6d7390bwjk18bdd5gr"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-merkledag"
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests requireing not packaged "github.com/ipfs/go-bitswap".
          (add-before 'check 'remove-failing-test-files
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file "merkledag_test.go")))))))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-blockservice
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ipfs-blockstore
           go-github-com-ipfs-go-ipfs-exchange-offline
           go-github-com-ipfs-go-ipfs-util
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipfs-go-ipld-legacy
           go-github-com-ipfs-go-log-v2
           go-github-com-ipld-go-codec-dagpb
           go-github-com-ipld-go-ipld-prime
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-merkledag")
    (synopsis "IPFs merkledag service implementation")
    (description
     "Package merkledag implements the IPFS Merkle @acronym{Directed Acyclic
Graphs, DAG} data structures as specified in
@url{https://docs.ipfs.tech/concepts/merkle-dag}.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-metrics-interface
  (package
    (name "go-github-com-ipfs-go-metrics-interface")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-metrics-interface")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0d7jh9aclmxq4z5hynn4lc0ab8c8rip92xkc224vjw2y7kg6jyvg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-metrics-interface"))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-multiformats-go-base32
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-metrics-interface")
    ;; XXX: The project neither has no a proper description, nor a README, see
    ;; <https://github.com/ipfs/go-metrics-interface/issues/1>.
    (synopsis "Metrics interface for IPFS")
    (description
     "Metrics interface for IPFS (Kubo).")
    (license license:expat)))

(define-public go-github-com-ipfs-go-metrics-prometheus
  (package
    (name "go-github-com-ipfs-go-metrics-prometheus")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-metrics-prometheus")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "12bvlg1pnwxyfn8paxf0viky5mv5isdql29mflvjhgs5g48mnc3s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-metrics-prometheus"))
    (propagated-inputs
     (list go-github-com-ipfs-go-log-v2
           go-github-com-ipfs-go-metrics-interface
           go-github-com-prometheus-client-golang))
    (home-page "https://github.com/ipfs/go-metrics-prometheus")
    (synopsis "Prometheus bindings for go-metrics-interface")
    (description
     "This package provides Prometheus bindings used in IPFS development.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-peertaskqueue
  (package
    (name "go-github-com-ipfs-go-peertaskqueue")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-peertaskqueue")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0mz6kj0d23p7hfmcrh8l0khj6gx9f58v2ls5336im6yw4vkzr711"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-peertaskqueue"))
    (propagated-inputs
     (list go-github-com-filecoin-project-go-clock
           go-github-com-ipfs-go-ipfs-pq
           go-github-com-libp2p-go-libp2p))
    (home-page "https://github.com/ipfs/go-peertaskqueue")
    (synopsis "Prioritized queue of abstract tasks distributed among peers")
    ;; Project has nothing in README, see
    ;; <https://github.com/ipfs/go-peertaskqueue/issues/1>.
    (description
     "This package implements a functionality to prioritize queue of abstract
tasks distributed among peers.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-ipfs-go-test
  (package
    (name "go-github-com-ipfs-go-test")
    (version "0.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-test")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h2lns6xl874m175l692qwsrwrv7jcx54ncqygjy9l3910ny2dg2"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-test"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-libp2p-go-libp2p
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-test")
    (synopsis "Testing utilty library for IPFS")
    (description
     "This package profides a test utility code used across many different
IPFS related projects.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-ipfs-go-unixfsnode
  (package
    (name "go-github-com-ipfs-go-unixfsnode")
    (version "1.10.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-unixfsnode")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0rn4x8f9a1ipwz0rg19ylbyh2s3q81vpnlan9yjscnfxx1r6gdbg"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-unixfsnode"
      ;; Full test suites requires Box, which introduce cycle.
      #:test-subdirs #~(list ".")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list ;; go-github-com-ipfs-boxo ; introduce cycle
      go-github-com-ipfs-go-bitfield
      go-github-com-ipfs-go-cid
      go-github-com-ipfs-go-ipld-format
      go-github-com-ipfs-go-test
      go-github-com-ipld-go-car-v2
      go-github-com-ipld-go-codec-dagpb
      go-github-com-ipld-go-ipld-prime
      go-github-com-multiformats-go-multicodec
      go-github-com-multiformats-go-multihash
      go-github-com-spaolacci-murmur3
      go-google-golang-org-protobuf))
    (home-page "https://github.com/ipfs/go-unixfsnode")
    (synopsis "ADL IPLD prime node implementation of protobuf")
    (description
     "This is an IPLD ADL that provides string based pathing for protobuf
nodes.  The top level node behaves like a map where @code{LookupByString}
returns the Hash property on the Link in the protobufs list of Links whos Name
property matches the key.  This should enable selector traversals that work
based of paths.")
    (license (list license:asl2.0 license:expat))))

;; XXX: This repository has been archived by the owner on Jun 20, 2023. It is
;; now read-only.  We highly recommend switching to the maintained version at
;; https://github.com/ipfs/boxo/tree/main/verifcid.  It's still in use by some
;; dependencies chain for unbundled inputs in Kubo.
(define-public go-github-com-ipfs-go-verifcid
  (package
    (name "go-github-com-ipfs-go-verifcid")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-verifcid")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05i6wp2nln0mlr1pivmva7j6bwa09k7jl04acx1lw65h4d9nxsjm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-verifcid"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipfs/go-verifcid")
    (synopsis "CID security code")
    (description
     "@code{go-verifcid} provides @url{https://github.com/ipld/cid, CID} security
code prior to it getting merged into @code{go-cid}.")
    ;; This library is dual-licensed under Apache 2.0 and MIT terms:
    ;; LICENSE-MIT and LICENSE-APACHE.
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipfs-shipyard-nopfs
  (package
    (name "go-github-com-ipfs-shipyard-nopfs")
    (version "0.0.14")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs-shipyard/nopfs")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qbp7hny0v6n74agh1ym98ndnnr53aq1hd6ybm5q214fah7k23r7"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/ipfs-shipyard/nopfs/ipfs
            ;; - github.com/ipfs-shipyard/nopfs/nopfs-kubo-plugin
            (for-each delete-file-recursively
                      (list "ipfs" "nopfs-kubo-plugin"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs-shipyard/nopfs"))
    (propagated-inputs
     (list go-github-com-fsnotify-fsnotify
           go-github-com-ipfs-boxo
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-log-v2
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash
           go-go-uber-org-multierr
           go-gopkg-in-yaml-v3))
    (home-page "https://github.com/ipfs-shipyard/nopfs")
    (synopsis "Content-blocking-layer capabilities for IPFS")
    (description
     "Package nopfs implements content blocking for the IPFS stack.")
    (license license:asl2.0)))

(define-public go-github-com-ipfs-shipyard-nopfs-ipfs
  (package
    (name "go-github-com-ipfs-shipyard-nopfs-ipfs")
    (version "0.25.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs-shipyard/nopfs")
              (commit (go-version->git-ref version
                                           #:subdir "ipfs"))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "00lwizzdfdx6kynxddal3all6q9dhwqanpkw0d0vxlwik4nkvxa5"))
       (modules '((guix build utils)
                  (ice-9 ftw)
                  (srfi srfi-26)))
       (snippet
        #~(begin
            ;; XXX: 'delete-all-but' is copied from the turbovnc package.
            ;; Consider to implement it as re-usable procedure in
            ;; guix/build/utils or guix/build-system/go.
            (define (delete-all-but directory . preserve)
              (with-directory-excursion directory
                (let* ((pred (negate (cut member <>
                                          (cons* "." ".." preserve))))
                       (items (scandir "." pred)))
                  (for-each (cut delete-file-recursively <>) items))))
            (delete-all-but "." "ipfs")))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "sorted-network-list.bin")
      #:import-path "github.com/ipfs-shipyard/nopfs/ipfs"
      #:unpack-path "github.com/ipfs-shipyard/nopfs"))
    (propagated-inputs
     (list go-github-com-ipfs-boxo
           go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-log-v2
           go-github-com-ipfs-shipyard-nopfs
           go-github-com-ipld-go-ipld-prime
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-kad-dht))
    (home-page "https://github.com/ipfs-shipyard/nopfs")
    (synopsis "IPFS library helpers and wrappers")
    (description
     "Package ipfs provides wrapper implementations of key layers in the
go-ipfs stack to enable content-blocking.")
    (license license:asl2.0)))

(define-public go-github-com-ipld-go-car
  (package
    (name "go-github-com-ipld-go-car")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipld/go-car")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c9mmvwwhl86m0jv7cp3xaay7bpp9nzq3by3r75z2hyz341zvb2c"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/ipld/go-car/v2
            (delete-file-recursively "v2")
            ;; To reduce cycles
            (delete-file-recursively "cmd")))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipld/go-car"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-ipld-cbor
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipfs-go-merkledag
           go-github-com-ipld-go-codec-dagpb
           go-github-com-ipld-go-ipld-prime
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/ipld/go-car")
    (synopsis "Content addressible archive utility")
    (description
     "Package car allows inspecting and reading CARv1 files, described at
@url{https://ipld.io/specs/transport/car}.")
    ;; This library is dual-licensed under Apache 2.0 and MIT terms.
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipld-go-car-v2
  (package
    (inherit go-github-com-ipld-go-car)
    (name "go-github-com-ipld-go-car-v2")
    (version "2.14.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipld/go-car")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08zkz73r76va2hvrg6gixc0nfdwwsigdncsl9h6b5ibb11zwa0gn"))))
    (arguments
     (list
      ;; Version 2 is part of the same source and located in "v2" directory.
      #:import-path "github.com/ipld/go-car/v2"
      #:unpack-path "github.com/ipld/go-car"
      #:phases
      #~(modify-phases %standard-phases
          ;; Tests requiring github.com/ipfs/go-unixfsnode and indrodusing
          ;; cylcle.
          (add-before 'check 'remove-failing-test-files
            (lambda* (#:key unpack-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" unpack-path)
                (for-each delete-file
                          (list "v2/selective_test.go"
                                "v2/writer_test.go"))))))))
    (propagated-inputs
     (list go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-ipld-cbor
           go-github-com-ipfs-go-ipld-format
           go-github-com-ipld-go-codec-dagpb
           go-github-com-ipld-go-ipld-prime
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash
           go-github-com-multiformats-go-varint
           go-github-com-petar-gollrb
           go-github-com-whyrusleeping-cbor
           go-golang-org-x-exp))
    (description
     "Package car allows inspecting and reading CARv2 files, described at
@url{https://ipld.io/specs/transport/car}.")))

(define-public go-github-com-ipld-go-codec-dagpb
  (package
    (name "go-github-com-ipld-go-codec-dagpb")
    (version "1.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipld/go-codec-dagpb")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1jbrwbgr222wsi95gdflbj350csja6k8vphdq7c9bm50ipr8bvkq"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipld/go-codec-dagpb"))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid
           go-github-com-ipld-go-ipld-prime
           go-google-golang-org-protobuf))
    (home-page "https://github.com/ipld/go-codec-dagpb/")
    (synopsis "Implementation of the DAG-PB spec for Go")
    (description
     "An implementation of the @url{https://ipld.io/, IPLD DAG-PB} spec for
@code{go-ipld-prime}.")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-ipld-go-ipld-prime
  (package
    (name "go-github-com-ipld-go-ipld-prime")
    (version "0.21.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipld/go-ipld-prime")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1ycb08h0hvq3mw3sbjkjzp5sfcxmss155jxiv5gjg7myxvzk91ja"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipld/go-ipld-prime"
      ;; XXX: More input requires to cover all shipped go packages.
      #:test-subdirs #~(list ".")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'copy-ipld-specfs
            (lambda* (#:key import-path #:allow-other-keys)
              (copy-recursively
               (string-append #$(this-package-native-input
                                 "specification-ipld")
                              "/share/ipld/")
               (string-append "src/" import-path "/ipld")))))))
    (native-inputs
     (list go-github-com-frankban-quicktest
           go-github-com-warpfork-go-testmark
           specification-ipld))
    (propagated-inputs
     (list go-github-com-google-go-cmp
           go-github-com-ipfs-go-block-format
           go-github-com-ipfs-go-cid
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash
           go-github-com-polydawn-refmt
           go-gopkg-in-yaml-v2))
    (home-page "https://github.com/ipld/go-ipld-prime")
    (synopsis "Golang interfaces for the IPLD Data Model")
    (description
     "@code{go-ipld-prime} is an implementation of the IPLD spec interfaces, a
batteries-included codec implementations of IPLD for CBOR and JSON, and tooling for
basic operations on IPLD objects (traversals, etc).")
    (license license:expat)))

(define-public go-github-com-ipfs-go-log-v2
  (package
    (name "go-github-com-ipfs-go-log-v2")
    (version "2.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-log")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1yh3sw8knpy364h8h8rqw193whnjd6fbc13cxh6zs29z3x2a7aqa"))))
    (build-system go-build-system)
    (propagated-inputs
     (list go-github-com-mattn-go-isatty
           go-go-uber-org-multierr
           go-go-uber-org-zap))
    (arguments
     (list
      #:import-path "github.com/ipfs/go-log/v2"))
    (home-page "https://github.com/ipfs/go-log")
    (synopsis "Logging library used by @code{go-ipfs}")
    (description
     "@code{go-log} wraps @url{https://github.com/uber-go/zap, zap} to
provide a logging facade.  @code{go-log} manages logging instances and allows for
their levels to be controlled individually.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-log
  (package
    (inherit go-github-com-ipfs-go-log-v2)
    (name "go-github-com-ipfs-go-log")
    (version "1.0.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/go-log")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0gj2yqrv6wgpkv6f9c1anmw5kwg59plv0jrcxb3zmjrnk8fsn1jr"))))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-github-com-ipfs-go-log-v2
           go-github-com-opentracing-opentracing-go
           go-go-uber-org-zap))
    (arguments
     (list
      #:import-path "github.com/ipfs/go-log"))))

(define-public go-github-com-ipshipyard-p2p-forge
  (package
    (name "go-github-com-ipshipyard-p2p-forge")
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipshipyard/p2p-forge")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0qh820v738fn1k5scd99v230s4xcz26wg2s41farirhdjx8lmzc6"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; TODO: Enalbe when all missing inputs are packaged, using for Kubo as
      ;; source only package.
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/ipshipyard/p2p-forge"))
    (propagated-inputs
     (list go-github-com-aws-aws-sdk-go
           go-github-com-caddyserver-certmagic
           ;; go-github-com-coredns-caddy
           ;; go-github-com-coredns-coredns
           go-github-com-felixge-httpsnoop
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ds-badger4
           go-github-com-ipfs-go-ds-dynamodb
           go-github-com-ipfs-go-log-v2
           go-github-com-joho-godotenv
           go-github-com-letsencrypt-pebble-v2
           go-github-com-libp2p-go-libp2p
           go-github-com-mholt-acmez-v3
           go-github-com-miekg-dns
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multiaddr-dns
           go-github-com-multiformats-go-multibase
           go-github-com-prometheus-client-golang
           go-github-com-slok-go-http-metrics
           go-go-uber-org-zap))
    (home-page "https://github.com/ipshipyard/p2p-forge")
    (synopsis "Authoritative DNS server for distributing DNS subdomains to libp2p peers")
    (description
     "This package provides an Authoritative DNS server and HTTP+libp2p API
for distributing DNS subdomains with CA-signed TLS certificates to libp2p
peers.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-libp2p-go-cidranger
  (package
    (name "go-github-com-libp2p-go-cidranger")
    (version "1.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-cidranger")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05hzlk5hx7qna5znr3q1crr0qb7h8yrv1v96pj015dh0kbdkdaba"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Check if the most of the tests may be enabled:
      ;; src/github.com/libp2p/go-cidranger/trie_test.go:557:8: cannot use
      ;; 4294967295 (untyped int constant) as int value in assignment
      ;; (overflows).
      #:tests? (and (target-64bit?)
                    (not (%current-target-system)))
      #:import-path "github.com/libp2p/go-cidranger"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-detect-race))
    (home-page "https://github.com/libp2p/go-cidranger")
    (synopsis "Fast IP to CIDR lookup in Golang")
    (description
     "Fast IP to @url{https://en.wikipedia.org/wiki/Classless_Inter-Domain_Routing,
CIDR} block(s) lookup using trie in Golang, inspired by
@url{https://vincent.bernat.im/en/blog/2017-ipv4-route-lookup-linux, IPv4
route lookup Linux}.  Possible use cases include detecting if a IP address is
from published cloud provider CIDR blocks (e.g. 52.95.110.1 is contained in
published AWS Route53 CIDR 52.95.110.0/24), IP routing rules, etc.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-doh-resolver
  (package
    (name "go-github-com-libp2p-go-doh-resolver")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-doh-resolver")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0asni7f3gd65bjfqz99fqchz9y75cpgmfwkkhsbq0y2dydagw666"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-doh-resolver"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-log-v2
           go-github-com-miekg-dns
           go-github-com-multiformats-go-multiaddr-dns))
    (home-page "https://github.com/libp2p/go-doh-resolver")
    (synopsis "DNS over HTTPS resolver")
    (description
     "This package provides an implementation DNS over HTTPS resolver as
specified in @url{https://datatracker.ietf.org/doc/html/rfc8484, RFC 8484}.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p
  (package
    (name "go-github-com-libp2p-go-libp2p")
    (version "0.43.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "02708nf651cx1ls9k4k0275w8ld4349kzp1yyv35m45phbvzd2zf"))
       (modules '((guix build utils)))
       (snippet
        #~(begin
            ;; Submodules with their own go.mod files and packaged separately:
            ;;
            ;; - github.com/libp2p/go-libp2p/scripts/test_analysis
            ;; - github.com/libp2p/go-libp2p/test-plans/m/v2
            (for-each delete-file-recursively
                      (list "scripts/test_analysis" "test-plans"))))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "sorted-network-list.bin")
      #:import-path "github.com/libp2p/go-libp2p"
      ;; XXX: Check how to enable the most of the tests, see GitHub Actions
      ;; workflow files of the project.
      #:test-subdirs #~(list "core/..." ".")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'remove-examples
            (lambda* (#:key import-path #:allow-other-keys)
              (with-directory-excursion (string-append "src/" import-path)
                (delete-file-recursively "examples")))))))
    (native-inputs
     (list go-github-com-libp2p-go-libp2p-testing
           go-github-com-stretchr-testify
           go-go-uber-org-mock
           go-go-uber-org-goleak))
    (propagated-inputs
     (list go-github-com-benbjohnson-clock
           go-github-com-davidlazar-go-crypto
           go-github-com-decred-dcrd-dcrec-secp256k1-v4
           go-github-com-flynn-noise
           go-github-com-google-gopacket
           go-github-com-gorilla-websocket
           go-github-com-hashicorp-golang-lru-arc-v2
           go-github-com-hashicorp-golang-lru-v2
           go-github-com-huin-goupnp
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-log-v2
           go-github-com-jackpal-go-nat-pmp
           go-github-com-jbenet-go-temp-err-catcher
           go-github-com-klauspost-compress
           go-github-com-koron-go-ssdp
           go-github-com-libp2p-go-buffer-pool
           go-github-com-libp2p-go-flow-metrics
           go-github-com-libp2p-go-libp2p-asn-util
           go-github-com-libp2p-go-msgio
           go-github-com-libp2p-go-netroute
           go-github-com-libp2p-go-reuseport
           go-github-com-libp2p-go-yamux-v5
           go-github-com-libp2p-zeroconf-v2
           go-github-com-marten-seemann-tcp
           go-github-com-mikioh-tcpinfo
           go-github-com-mr-tron-base58
           go-github-com-multiformats-go-base32
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multiaddr-dns
           go-github-com-multiformats-go-multiaddr-fmt
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-multicodec
           go-github-com-multiformats-go-multihash
           go-github-com-multiformats-go-multistream
           go-github-com-multiformats-go-varint
           go-github-com-pbnjay-memory
           go-github-com-pion-datachannel
           go-github-com-pion-ice-v4
           go-github-com-pion-logging
           go-github-com-pion-sctp
           go-github-com-pion-stun
           go-github-com-pion-webrtc-v4
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-github-com-quic-go-quic-go
           go-github-com-quic-go-webtransport-go
           go-go-uber-org-fx
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-time
           go-golang-org-x-tools
           go-google-golang-org-protobuf))
    (home-page "https://github.com/libp2p/go-libp2p")
    (synopsis "LIBP2P networking stack implementation in Golang")
    (description
     "This package provides a networking stack and library modularized out of
@url{https://github.com/ipfs/ipfs,The IPFS Project} as specified in
@url{https://github.com/libp2p/specs,libp2p}.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-gostream
  (package
    (name "go-github-com-libp2p-go-libp2p-gostream")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-gostream")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1zq5vhjj590azc1gwwiyqilf9ifxp8nz4hziz0fxs5ly2xhjj1mw"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Broken upstream, see
      ;; <https://github.com/libp2p/go-libp2p-gostream/issues/90>
      #:tests? #f
      #:import-path "github.com/libp2p/go-libp2p-gostream"
      ;; src/github.com/libp2p/go-libp2p-asn-util/asn.go:12:12: pattern
      ;; sorted-network-list.bin: cannot embed irregular file
      ;; sorted-network-list.bin
      #:embed-files #~(list "sorted-network-list\\.bin")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-libp2p-go-libp2p
           go-github-com-multiformats-go-multiaddr))
    (home-page "https://github.com/libp2p/go-libp2p-gostream")
    (synopsis "Golang @code{net} wrappers for libp2p")
    (description
     "This package provides a replacement of the standard @code{net} stack in
Go with @url{https://github.com/libp2p/libp2p, LibP2P} streams.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-http
  (package
    (name "go-github-com-libp2p-go-libp2p-http")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-http")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0v65xjj1ljpx9bfwqivgncc6imrykw1dd8m2zvaqiygv0dirzkzn"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; XXX: Broken upstream, see
      ;; <https://github.com/libp2p/go-libp2p-http/issues/94>
      #:tests? #f
      #:import-path "github.com/libp2p/go-libp2p-http"
      ;; src/github.com/libp2p/go-libp2p-asn-util/asn.go:12:12: pattern
      ;; sorted-network-list.bin: cannot embed irregular file
      ;; sorted-network-list.bin
      #:embed-files #~(list "sorted-network-list\\.bin")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-gostream
           go-github-com-multiformats-go-multiaddr))
    (home-page "https://github.com/libp2p/go-libp2p-http")
    (synopsis "HTTP on top of libp2p")
    (description
     "Package p2phttp allows to serve HTTP endpoints and make HTTP requests
through @url{https://github.com/libp2p/libp2p, LibP2P} using Go's standard
@code{http} and @code{net} stacks.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-kad-dht
  (package
    (name "go-github-com-libp2p-go-libp2p-kad-dht")
    (version "0.33.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-kad-dht")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1m263kjax7zyn3k3xgiamh6fcfl4724hm64z6mvmlslcx5zvmmzz"))))
    (build-system go-build-system)
    (arguments
     (list
      #:embed-files #~(list "sorted-network-list.bin")
      #:import-path "github.com/libp2p/go-libp2p-kad-dht"
      ;; Error: "[]" should have 3 item(s), but has 0
      #:test-flags #~(list "-skip" "TestIPDiversityFilter/Different_IPv6_blocks")))
    (native-inputs
     (list go-github-com-ipfs-go-test
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-google-gopacket
           go-github-com-google-uuid
           go-github-com-hashicorp-golang-lru
           go-github-com-ipfs-boxo
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-detect-race
           go-github-com-ipfs-go-log-v2
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-kbucket
           go-github-com-libp2p-go-libp2p-record
           go-github-com-libp2p-go-libp2p-routing-helpers
           go-github-com-libp2p-go-libp2p-testing
           go-github-com-libp2p-go-libp2p-xor
           go-github-com-libp2p-go-msgio
           go-github-com-libp2p-go-netroute
           go-github-com-multiformats-go-base32
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-multihash
           go-github-com-multiformats-go-multistream
           go-github-com-whyrusleeping-go-keyspace
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-metric
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-multierr
           go-go-uber-org-zap
           go-gonum-org-v1-gonum
           go-google-golang-org-protobuf))
    (home-page "https://github.com/libp2p/go-libp2p-kad-dht")
    (synopsis "Kademlia DHT implementation on go-libp2p")
    (description
     "Package dht implements a distributed hash table that satisfies the IPFS
routing interface as specified in @url{https://github.com/libp2p/specs,
kab-dht}.  This DHT is modeled after Kademlia with S/Kademlia modifications.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-kad-dht-bootstrap
  (hidden-package
   (package/inherit go-github-com-libp2p-go-libp2p-kad-dht
     (arguments
      (list
       #:skip-build? #t
       #:tests? #f
       #:import-path "github.com/libp2p/go-libp2p-kad-dht"))
     (propagated-inputs
      (list go-github-com-libp2p-go-libp2p-kbucket
            go-github-com-hashicorp-golang-lru
            go-github-com-whyrusleeping-go-keyspace
            go-gonum-org-v1-gonum)))))

(define-public go-github-com-libp2p-go-libp2p-kbucket
  (package
    (name "go-github-com-libp2p-go-libp2p-kbucket")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-kbucket")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1p764b38xm0v5w3h8df14480j6y0fxxlsxwqizqchf0ivr3fr634"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t ; cycles with Boxo
      #:import-path "github.com/libp2p/go-libp2p-kbucket"
      ;; src/github.com/libp2p/go-libp2p-asn-util/asn.go:12:12: pattern
      ;; sorted-network-list.bin: cannot embed irregular file
      ;; sorted-network-list.bin
      #:embed-files #~(list "sorted-network-list\\.bin")
      ;; Run portion of tests to bypath cycle with Boxo.
      #:test-subdirs #~(list "peerdiversity/...")))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-ipfs-go-log-v2
           go-github-com-libp2p-go-cidranger
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-asn-util
           go-github-com-minio-sha256-simd
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/libp2p/go-libp2p-kbucket")
    (synopsis "Kbucket implementation for use as a routing table")
    (description
     "Package kbucket implements a kademlia k-bucket routing table.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-pubsub
  (package
    (name "go-github-com-libp2p-go-libp2p-pubsub")
    (version "0.13.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-pubsub")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14afjrgfbmlrnjd5xnl70ff95mbvfxp9n8mx3hrw8069bcpzvm2k"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; TODO: Tests may need some adjustments as they take quite a lot of
      ;; time to finish.
      #:tests? #f
      #:import-path "github.com/libp2p/go-libp2p-pubsub"
      ;; src/github.com/libp2p/go-libp2p-asn-util/asn.go:12:12: pattern
      ;; sorted-network-list.bin: cannot embed irregular file
      ;; sorted-network-list.bin
      #:embed-files #~(list "sorted-network-list\\.bin")))
    (native-inputs
     (list go-github-com-libp2p-go-libp2p-testing
           go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-benbjohnson-clock
           go-github-com-gogo-protobuf
           go-github-com-ipfs-go-log-v2
           go-github-com-libp2p-go-buffer-pool
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-msgio
           go-github-com-multiformats-go-multiaddr
           go-github-com-multiformats-go-varint
           go-go-uber-org-zap))
    (home-page "https://github.com/libp2p/go-libp2p-pubsub")
    (synopsis "PubSub implementation for go-libp2p")
    (description
     "This package provides facilities for the Publish/Subscribe pattern of
message propagation, also known as overlay multicast.  The implementation
provides topic-based pubsub, with pluggable routing algorithms.")
    (license (list license:asl2.0 license:expat))))

(define-public go-github-com-libp2p-go-libp2p-pubsub-router
  (package
    (name "go-github-com-libp2p-go-libp2p-pubsub-router")
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-pubsub-router")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "01mxb8gi7myidnyfg8yqb445lbwqmgncvh7rcwx6n8av84afplx4"))))
    (build-system go-build-system)
    (arguments
     (list
      #:tests? #f ; XXX: tests hang, check why
      #:import-path "github.com/libp2p/go-libp2p-pubsub-router"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-gogo-protobuf
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ipfs-ds-help
           go-github-com-ipfs-go-log-v2
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-pubsub
           go-github-com-libp2p-go-libp2p-record
           go-github-com-libp2p-go-msgio
           go-golang-org-x-sync))
    (home-page "https://github.com/libp2p/go-libp2p-pubsub-router")
    (synopsis "PubSub libp2p router library")
    (description
     "This package implements a IPNS over PubSub for @code{libp2p}.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-record
  (package
    (name "go-github-com-libp2p-go-libp2p-record")
    (version "0.3.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-record")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0hl3khlsxagypf18cgx0rd2hnlnpg2vhcrn4g7m1xrkimgj57696"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-libp2p-record"))
    (native-inputs
     (list go-github-com-ipfs-go-test
           go-google-golang-org-protobuf))
    (propagated-inputs
     (list go-github-com-libp2p-go-libp2p
           go-github-com-multiformats-go-multihash))
    (home-page "https://github.com/libp2p/go-libp2p-record")
    (synopsis "Signed records for use with routing systems")
    (description
     "This package implements signed records for use wit IPFS routing
systems.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-routing-helpers
  (package
    (name "go-github-com-libp2p-go-libp2p-routing-helpers")
    (version "0.7.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-routing-helpers")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0s0gzxfhmvywk4w78j1ga1ha7f948csfyflpxz8la1cp64238f01"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-libp2p-routing-helpers"))
    (native-inputs
     (list go-github-com-stretchr-testify))
    (propagated-inputs
     (list go-github-com-hashicorp-errwrap
           go-github-com-hashicorp-go-multierror
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-log
           go-github-com-jorropo-jsync
           go-github-com-libp2p-go-libp2p
           go-github-com-libp2p-go-libp2p-record
           go-github-com-multiformats-go-multibase
           go-github-com-multiformats-go-multihash
           go-go-opentelemetry-io-otel
           go-go-opentelemetry-io-otel-trace
           go-go-uber-org-multierr))
    (home-page "https://github.com/libp2p/go-libp2p-routing-helpers")
    (synopsis
     "Collection of helper types for composing different types of routers")
    ;; XXX: Project lacks any documentation.
    (description
     "This package provides a collection of helper types for composing
different types of routers.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-testing
  (package
    (name "go-github-com-libp2p-go-libp2p-testing")
    (version "0.12.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-testing")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08n17mqskdj5il6lws53wk1zsf9d8c55f58bap1rjngf2d669p39"))))
    (build-system go-build-system)
    (arguments
     (list
      #:skip-build? #t ; to break cycle with  go-github-com-libp2p-go-libp2p
      #:tests? #f
      #:import-path "github.com/libp2p/go-libp2p-testing"))
    (home-page "https://github.com/libp2p/go-libp2p-testing")
    (synopsis "Test toolbox for go-libp2p modules")
    (description
     "This package provides a testing toolbox for go-libp2p modules.")
    (license (list license:expat license:asl2.0))))

(define-public go-github-com-libp2p-go-libp2p-xor
  (package
    (name "go-github-com-libp2p-go-libp2p-xor")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-libp2p-xor")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0p4mwz0q0zbj8p1s04hmpy0w0znfxz3b7x28dv7cz0cg6wqvfqvk"))))
    (build-system go-build-system)
    (arguments
     (list
      ;; Do not build or test to bypass cycle with Boxo.
      #:skip-build? #t
      #:tests? #f
      #:import-path "github.com/libp2p/go-libp2p-xor"))
    (propagated-inputs
     (list go-github-com-libp2p-go-libp2p-kbucket))
    (home-page "https://github.com/libp2p/go-libp2p-xor")
    (synopsis "Xor Trie implementation for libp2p")
    (description
     "This package implements XOR tries.  An XOR trie is a trie for
equal-length bit strings.  XOR tries support efficient set operations, as well
as distance-based operations with respect to the XOR metric.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-socket-activation
  (package
    (name "go-github-com-libp2p-go-socket-activation")
    (version "0.1.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/libp2p/go-socket-activation")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cqxzmjfg7838xifs07kigys9icardwlj1wl426mzgzmbwn6pg5s"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/libp2p/go-socket-activation"))
    (propagated-inputs
     (list go-github-com-coreos-go-systemd-v22
           go-github-com-ipfs-go-log
           go-github-com-multiformats-go-multiaddr))
    (home-page "https://github.com/libp2p/go-socket-activation")
    (synopsis "Multiaddr backed systemd socket activation")
    (description
     "This package provides access to sockets registered by the system's init
daemon as described in
@url{http://0pointer.de/blog/projects/socket-activation}.")
    (license license:expat)))

(define-public go-github-com-whyrusleeping-cbor-gen
  (package
    (name "go-github-com-whyrusleeping-cbor-gen")
    (version "v0.0.0-20230818171029-f91ae536ca25")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/whyrusleeping/cbor-gen")
              (commit (go-version->git-ref version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "08by7pqh4fcwf2va01iif75yqkfssi6d48334404mmv9jmhzim60"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/whyrusleeping/cbor-gen"))
    (propagated-inputs
     (list go-github-com-ipfs-go-cid
           go-github-com-google-go-cmp
           go-golang-org-x-xerrors))
    (home-page "https://github.com/whyrusleeping/cbor-gen")
    (synopsis "Codegen for CBOR codecs on the specified types")
    (description
     "Basic utilities to generate fast path @url{https://cbor.io/, CBOR} codecs for
types.")
    (license license:expat)))

(define-public kubo
  (package
    (name "kubo")
    (version "0.36.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "https://github.com/ipfs/kubo")
              (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "080ixpahm2hsc7vsipcjlymxagvz9s3n1dc7nn7zfl2z4fwwhdhm"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:embed-files #~(list "sorted-network-list.bin" ".*\\.css" ".*\\.html")
      #:unpack-path "github.com/ipfs/kubo"
      #:import-path "github.com/ipfs/kubo/cmd/ipfs"
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
    (inputs (list go-bazil-org-fuse
                  go-contrib-go-opencensus-io-exporter-prometheus
                  go-github-com-anmitsu-go-shlex
                  go-github-com-blang-semver-v4
                  go-github-com-caddyserver-certmagic
                  go-github-com-cenkalti-backoff-v4
                  go-github-com-ceramicnetwork-go-dag-jose
                  go-github-com-cheggaaa-pb
                  go-github-com-cockroachdb-pebble-v2
                  go-github-com-coreos-go-systemd-v22
                  go-github-com-dustin-go-humanize
                  go-github-com-elgris-jsondiff
                  go-github-com-facebookgo-atomicfile
                  go-github-com-filecoin-project-go-clock
                  go-github-com-fsnotify-fsnotify
                  go-github-com-google-uuid
                  go-github-com-hashicorp-go-version
                  go-github-com-ipfs-boxo
                  go-github-com-ipfs-go-block-format
                  go-github-com-ipfs-go-cid
                  go-github-com-ipfs-go-cidutil
                  go-github-com-ipfs-go-datastore
                  go-github-com-ipfs-go-detect-race
                  go-github-com-ipfs-go-ds-badger
                  go-github-com-ipfs-go-ds-flatfs
                  go-github-com-ipfs-go-ds-leveldb
                  go-github-com-ipfs-go-ds-measure
                  go-github-com-ipfs-go-ds-pebble
                  go-github-com-ipfs-go-fs-lock
                  go-github-com-ipfs-go-ipfs-cmds
                  go-github-com-ipfs-go-ipld-cbor
                  go-github-com-ipfs-go-ipld-format
                  go-github-com-ipfs-go-ipld-git
                  go-github-com-ipfs-go-ipld-legacy
                  go-github-com-ipfs-go-log-v2
                  go-github-com-ipfs-go-metrics-interface
                  go-github-com-ipfs-go-metrics-prometheus
                  go-github-com-ipfs-go-test
                  go-github-com-ipfs-go-unixfsnode
                  go-github-com-ipfs-shipyard-nopfs
                  go-github-com-ipfs-shipyard-nopfs-ipfs
                  go-github-com-ipld-go-car-v2
                  go-github-com-ipld-go-codec-dagpb
                  go-github-com-ipld-go-ipld-prime
                  go-github-com-ipshipyard-p2p-forge
                  go-github-com-jbenet-go-temp-err-catcher
                  go-github-com-jbenet-goprocess
                  go-github-com-julienschmidt-httprouter
                  go-github-com-libp2p-go-doh-resolver
                  go-github-com-libp2p-go-libp2p
                  go-github-com-libp2p-go-libp2p-http
                  go-github-com-libp2p-go-libp2p-kad-dht
                  go-github-com-libp2p-go-libp2p-kbucket
                  go-github-com-libp2p-go-libp2p-pubsub
                  go-github-com-libp2p-go-libp2p-pubsub-router
                  go-github-com-libp2p-go-libp2p-record
                  go-github-com-libp2p-go-libp2p-routing-helpers
                  go-github-com-libp2p-go-libp2p-testing
                  go-github-com-libp2p-go-socket-activation
                  go-github-com-multiformats-go-multiaddr
                  go-github-com-multiformats-go-multiaddr-dns
                  go-github-com-multiformats-go-multibase
                  go-github-com-multiformats-go-multicodec
                  go-github-com-multiformats-go-multihash
                  go-github-com-opentracing-opentracing-go
                  go-github-com-pbnjay-memory
                  go-github-com-prometheus-client-golang
                  go-github-com-stretchr-testify
                  go-github-com-syndtr-goleveldb
                  go-github-com-tidwall-gjson
                  go-github-com-tidwall-sjson
                  go-github-com-whyrusleeping-go-sysinfo
                  go-github-com-whyrusleeping-multiaddr-filter
                  go-go-opencensus-io
                  go-go-opentelemetry-io-contrib-instrumentation-net-http-otelhttp
                  go-go-opentelemetry-io-contrib-propagators-autoprop
                  go-go-opentelemetry-io-otel
                  go-go-opentelemetry-io-otel-sdk
                  go-go-opentelemetry-io-otel-trace
                  go-go-uber-org-dig
                  go-go-uber-org-fx
                  go-go-uber-org-multierr
                  go-go-uber-org-zap
                  go-golang-org-x-crypto
                  go-golang-org-x-exp
                  go-golang-org-x-mod
                  go-golang-org-x-sync
                  go-golang-org-x-sys
                  go-google-golang-org-protobuf))
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

(define-public spritely-libp2p-daemon
  (let ((version "0.1")
        (commit "f10d8c4bad2a50d6e481c0b57231741d079ffedb")
        (revision "0"))
    (package
      (name "spritely-libp2p-daemon")
      (version (git-version version revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
                (url "https://gitlab.com/spritely/go-libp2p-daemon.git")
                (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0bk2610rlg8d4lla7h7sic9drv1syabfbr7kbg2yqqbhiwg0v2br"))))
      (build-system go-build-system)
      (arguments
       (list
        #:embed-files #~(list "sorted-network-list.bin")
        #:install-source? #f
        #:import-path "gitlab.com/spritely/spritely-libp2p-daemon"))
      (native-inputs
       (list go-github-com-libp2p-go-libp2p
             go-github-com-multiformats-go-multiaddr
             go-github-com-stretchr-testify))
      (home-page "https://gitlab.com/spritely/go-libp2p-daemon")
      (synopsis "Simple daemon to connect over libp2p")
      (description "This package provides a very simple daemon to interface to
from other programming languages.  It's designed to fulfill the needs of
Spritely's Goblins library to support libp2p as a netlayer.")
      (license license:asl2.0))))
