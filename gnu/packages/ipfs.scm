;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
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
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-check)
  #:use-module (gnu packages golang-compression)
  #:use-module (gnu packages golang-crypto)
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
    (version "0.5.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-ds-flatfs")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1mrgl6kslq4d4zfpdyxvqxz8brhm8wphvnp916pippn7z63sayj3"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-ds-flatfs"))
    (propagated-inputs
     (list go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-log
           go-github-com-jbenet-goprocess))
    (home-page "https://github.com/ipfs/go-ds-flatfs")
    (synopsis "Datastore implementation using sharded directories and flat files to store data")
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
           go-github-com-ipfs-go-datastore
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
     "@code{go-ipfs-blockstore} implements a thin wrapper over an IPFS datastore,
giving a clean interface for getting and putting block objects.")
    (license license:expat)))

(define-public go-github-com-ipfs-go-ipfs-blocksutil
  ;; Use the latest commit from the "master" branch to fix the build with
  ;; go-1.21, see <https://github.com/ipfs/go-ipfs-blocksutil/issues/25>.
  (let ((commit "ce0497f5ee55c479db98905aec8ff56c27aad2a2")
        (revision "0"))
    (package
      (name "go-github-com-ipfs-go-ipfs-blocksutil")
      (version (git-version "0.0.1" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/ipfs/go-ipfs-blocksutil")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1ya6376wphp51rv48nmv4jw3x0mf6ym5yx1650fbkp5l5crqpdb8"))))
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
      (license license:expat))))

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
    (version "0.6.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-datastore")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xbhh3gm7bgd2d1p821w8gmbh87aix1g1ynhbl7gjaxxyhrsh68n"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/ipfs/go-datastore"
      #:test-subdirs
      #~(list "autobatch/..."
              "delayed/..."
              "examples/..."
              "failstore/..."
              ;; "fuzz/..." ; introduces cycle, for CLI
              "keytransform/..."
              "mount/..."
              "namespace/..."
              "query/..."
              "retrystore/..."
              "scoped/..."
              "sync/..."
              "test/..."
              "trace/..."
              ".")
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
           go-github-com-jbenet-goprocess
           go-go-uber-org-multierr
           go-golang-org-x-xerrors))
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

(define-public go-github-com-ipfs-go-ds-badger
  (package
    (name "go-github-com-ipfs-go-ds-badger")
    (version "0.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-ds-badger")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "06hn79airlqrgsbsppin98swbqwz58ji659fyrk1wivp4iz2na3h"))))
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
      #:import-path "github.com/ipfs/go-ipfs-pq"))
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
      #:go go-1.21
      #:import-path "github.com/ipfs/go-ipld-legacy"))
    (native-inputs (list go-github-com-stretchr-testify))
    (propagated-inputs (list go-github-com-ipfs-go-block-format
                             go-github-com-ipfs-go-cid
                             go-github-com-ipfs-go-ipld-format
                             go-github-com-ipld-go-ipld-prime))
    (home-page "https://github.com/ipfs/go-ipld-legacy")
    (synopsis "Translation layer for IPLD legacy code")
    (description
     "@code{go-ipld-format} is a translation layer between @code{go-ipld-prime} nodes
and @code{go-ipld-format} legacy interface.")
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

(define-public go-github-com-ipfs-go-metrics-prometheus
  (package
    (name "go-github-com-ipfs-go-metrics-prometheus")
    (version "0.0.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-metrics-prometheus")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1gyh2g912lrwghs2f5alh42dgwsbbdg1wan5vw8s0a2ni0avsfib"))))
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
    (native-inputs
     (list go-github-com-filecoin-project-go-clock))
    (propagated-inputs
     (list go-github-com-ipfs-go-ipfs-pq
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
      #:go go-1.21
      #:import-path "github.com/ipld/go-codec-dagpb"))
    (propagated-inputs (list go-github-com-ipfs-go-cid
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

(define-public go-github-com-ipfs-go-metrics-interface
  (package
    (name "go-github-com-ipfs-go-metrics-interface")
    (version "0.0.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ipfs/go-metrics-interface")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09xc71175sfnqlizkbw066jagnbag9ihvs240z6g6dm2yx3w5xgy"))))
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

(define-public go-github-com-libp2p-go-libp2p
  (package
    (name "go-github-com-libp2p-go-libp2p")
    (version "0.36.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-libp2p")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0fmalwb0g0nykd1v22nm5gmif9mvapshsja8w1ihlm8ahbqq9vb2"))
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
           go-github-com-ipfs-go-cid
           go-github-com-ipfs-go-datastore
           go-github-com-ipfs-go-ds-badger
           go-github-com-ipfs-go-ds-leveldb
           go-github-com-ipfs-go-log-v2
           go-github-com-jbenet-go-temp-err-catcher
           go-github-com-klauspost-compress
           go-github-com-libp2p-go-buffer-pool
           go-github-com-libp2p-go-flow-metrics
           go-github-com-libp2p-go-libp2p-asn-util
           go-github-com-libp2p-go-msgio
           go-github-com-libp2p-go-nat
           go-github-com-libp2p-go-netroute
           go-github-com-libp2p-go-reuseport
           go-github-com-libp2p-go-yamux-v4
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
           go-github-com-pion-ice-v2
           go-github-com-pion-logging
           go-github-com-pion-sctp
           go-github-com-pion-stun
           go-github-com-pion-webrtc-v3
           go-github-com-prometheus-client-golang
           go-github-com-prometheus-client-model
           go-github-com-quic-go-quic-go
           go-github-com-quic-go-webtransport-go
           go-github-com-raulk-go-watchdog
           go-go-uber-org-fx
           go-go-uber-org-zap
           go-golang-org-x-crypto
           go-golang-org-x-exp
           go-golang-org-x-sync
           go-golang-org-x-sys
           go-golang-org-x-tools
           go-google-golang-org-protobuf))
    (home-page "https://github.com/libp2p/go-libp2p")
    (synopsis "LIBP2P networking stack implementation in Golang")
    (description
     "This package provides a networking stack and library modularized out of
@url{https://github.com/ipfs/ipfs,The IPFS Project} as specified in
@url{https://github.com/libp2p/specs,libp2p}.")
    (license license:expat)))

(define-public go-github-com-libp2p-go-libp2p-kbucket
  (package
    (name "go-github-com-libp2p-go-libp2p-kbucket")
    (version "0.6.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/libp2p/go-libp2p-kbucket")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "03xla0mhb17lh7syv0x4hvg2i0q1r8d6ym6rmjgf1z3z955znx6l"))))
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
    (version "0.31.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append
             "https://dist.ipfs.io/kubo/v" version
             "/kubo-source.tar.gz"))
       (sha256
        (base32 "0271rh54xbwikbllzsjnkjlj29cb2xy5lnhia47qlf2ymvl48kvf"))
       (file-name (string-append name "-" version "-source"))
       (modules '((guix build utils)))
       (snippet '(for-each delete-file-recursively
                           ;; TODO: unbundle the rest as well
                           '("vendor/bazil.org"
                             "vendor/github.com/AndreasBriese"
                             "vendor/github.com/alecthomas"
                             "vendor/github.com/benbjohnson"
                             "vendor/github.com/beorn7"
                             "vendor/github.com/blang"
                             "vendor/github.com/cenkalti"
                             "vendor/github.com/ceramicnetwork"
                             "vendor/github.com/cespare"
                             "vendor/github.com/cheggaaa"
                             "vendor/github.com/containerd"
                             "vendor/github.com/coreos"
                             "vendor/github.com/davecgh"
                             "vendor/github.com/davidlazar"
                             "vendor/github.com/decred"
                             "vendor/github.com/dgraph-io"
                             "vendor/github.com/docker"
                             "vendor/github.com/dustin"
                             "vendor/github.com/elastic"
                             "vendor/github.com/elgris"
                             "vendor/github.com/facebookgo"
                             "vendor/github.com/felixge"
                             "vendor/github.com/flynn"
                             "vendor/github.com/francoispqt"
                             "vendor/github.com/fsnotify"
                             "vendor/github.com/gabriel-vasile"
                             "vendor/github.com/go-kit"
                             "vendor/github.com/go-logfmt"
                             "vendor/github.com/go-logr"
                             "vendor/github.com/go-task"
                             "vendor/github.com/godbus"
                             "vendor/github.com/gogo"
                             "vendor/github.com/golang"
                             "vendor/github.com/google"
                             "vendor/github.com/gorilla"
                             "vendor/github.com/hashicorp"
                             "vendor/github.com/huin"
                             "vendor/github.com/ipfs/go-bitfield"
                             "vendor/github.com/ipfs/go-block-format"
                             "vendor/github.com/ipfs/go-cid"
                             "vendor/github.com/ipfs/go-cidutil"
                             "vendor/github.com/ipfs/go-datastore"
                             "vendor/github.com/ipfs/go-detect-race"
                             "vendor/github.com/ipfs/go-ds-badger"
                             "vendor/github.com/ipfs/go-ds-leveldb"
                             "vendor/github.com/ipfs/go-ipfs-delay"
                             "vendor/github.com/ipfs/go-ipfs-redirects-file"
                             "vendor/github.com/ipfs/go-ipfs-util"
                             "vendor/github.com/ipfs/go-ipld-cbor"
                             "vendor/github.com/ipfs/go-ipld-format"
                             "vendor/github.com/ipfs/go-ipld-git"
                             "vendor/github.com/ipfs/go-ipld-legacy"
                             "vendor/github.com/ipfs/go-log"
                             "vendor/github.com/ipfs/go-metrics-interface"
                             "vendor/github.com/ipfs/go-verifcid"
                             "vendor/github.com/ipld/go-codec-dagpb"
                             "vendor/github.com/ipld/go-ipld-prime"
                             "vendor/github.com/jackpal"
                             "vendor/github.com/jbenet"
                             "vendor/github.com/julienschmidt"
                             "vendor/github.com/klauspost"
                             "vendor/github.com/koron"
                             "vendor/github.com/libp2p/go-buffer-pool"
                             "vendor/github.com/libp2p/go-cidranger"
                             "vendor/github.com/libp2p/go-doh-resolver"
                             "vendor/github.com/libp2p/go-flow-metrics"
                             "vendor/github.com/libp2p/go-libp2p"
                             "vendor/github.com/libp2p/go-libp2p-asn-util"
                             "vendor/github.com/libp2p/go-msgio"
                             "vendor/github.com/libp2p/go-nat"
                             "vendor/github.com/libp2p/go-netroute"
                             "vendor/github.com/libp2p/go-reuseport"
                             "vendor/github.com/libp2p/go-socket-activation"
                             "vendor/github.com/libp2p/go-yamux"
                             "vendor/github.com/libp2p/zeroconf"
                             "vendor/github.com/marten-seemann"
                             "vendor/github.com/mattn"
                             "vendor/github.com/mgutz"
                             "vendor/github.com/miekg"
                             "vendor/github.com/mikioh"
                             "vendor/github.com/minio"
                             "vendor/github.com/mitchellh"
                             "vendor/github.com/mr-tron"
                             "vendor/github.com/multiformats"
                             "vendor/github.com/onsi"
                             "vendor/github.com/opencontainers"
                             "vendor/github.com/opentracing"
                             "vendor/github.com/pbnjay"
                             "vendor/github.com/pion"
                             "vendor/github.com/pkg"
                             "vendor/github.com/pmezard"
                             "vendor/github.com/polydawn"
                             "vendor/github.com/prometheus"
                             "vendor/github.com/quic-go"
                             "vendor/github.com/raulk"
                             "vendor/github.com/rs"
                             "vendor/github.com/spaolacci"
                             "vendor/github.com/stretchr"
                             "vendor/github.com/syndtr"
                             "vendor/github.com/tidwall"
                             "vendor/github.com/ucarion"
                             "vendor/github.com/whyrusleeping"
                             "vendor/go.uber.org"
                             "vendor/golang.org"
                             "vendor/google.golang.org/protobuf"
                             "vendor/gopkg.in"
                             "vendor/lukechampine.com")))))
    (build-system go-build-system)
    (arguments
     (list
      #:go go-1.22
      #:embed-files #~(list "sorted-network-list.bin")
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
                  go-github-com-cenkalti-backoff-v4
                  go-github-com-ceramicnetwork-go-dag-jose
                  go-github-com-cheggaaa-pb
                  go-github-com-coreos-go-systemd-v22
                  go-github-com-dustin-go-humanize
                  go-github-com-elgris-jsondiff
                  go-github-com-facebookgo-atomicfile
                  go-github-com-fsnotify-fsnotify
                  go-github-com-google-uuid
                  go-github-com-hashicorp-go-multierror
                  go-github-com-hashicorp-go-version
                  ;;go-github-com-ipfs-boxo
                  go-github-com-ipfs-go-block-format
                  go-github-com-ipfs-go-cid
                  go-github-com-ipfs-go-cidutil
                  go-github-com-ipfs-go-datastore
                  go-github-com-ipfs-go-detect-race
                  go-github-com-ipfs-go-ds-badger
                  ;;go-github-com-ipfs-go-ds-flatfs
                  go-github-com-ipfs-go-ds-leveldb
                  ;;go-github-com-ipfs-go-ds-measure
                  ;;go-github-com-ipfs-go-fs-lock
                  ;;go-github-com-ipfs-go-ipfs-cmds
                  go-github-com-ipfs-go-ipld-cbor
                  go-github-com-ipfs-go-ipld-format
                  go-github-com-ipfs-go-ipld-git
                  go-github-com-ipfs-go-ipld-legacy
                  go-github-com-ipfs-go-log
                  go-github-com-ipfs-go-log-v2
                  go-github-com-ipfs-go-metrics-interface
                  ;;go-github-com-ipfs-go-metrics-prometheus
                  ;;go-github-com-ipfs-go-unixfsnode
                  ;;go-github-com-ipfs-shipyard-nopfs
                  ;;go-github-com-ipfs-shipyard-nopfs-ipfs
                  ;;go-github-com-ipld-go-car
                  ;;go-github-com-ipld-go-car-v2
                  go-github-com-ipld-go-codec-dagpb
                  go-github-com-ipld-go-ipld-prime
                  go-github-com-jbenet-go-random
                  go-github-com-jbenet-go-temp-err-catcher
                  go-github-com-jbenet-goprocess
                  go-github-com-julienschmidt-httprouter
                  go-github-com-libp2p-go-doh-resolver
                  go-github-com-libp2p-go-libp2p
                  ;;go-github-com-libp2p-go-libp2p-http
                  ;;go-github-com-libp2p-go-libp2p-kad-dht
                  ;;go-github-com-libp2p-go-libp2p-kbucket
                  ;;go-github-com-libp2p-go-libp2p-pubsub
                  ;;go-github-com-libp2p-go-libp2p-pubsub-router
                  ;;go-github-com-libp2p-go-libp2p-record
                  ;;go-github-com-libp2p-go-libp2p-routing-helpers
                  ;;go-github-com-libp2p-go-libp2p-testing
                  go-github-com-libp2p-go-socket-activation
                  go-github-com-mitchellh-go-homedir
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
                  go-golang-org-x-exp
                  go-golang-org-x-mod
                  go-golang-org-x-sync
                  go-golang-org-x-sys
                  go-google-golang-org-protobuf

                  ;;
                  ;; A list of indirect dependencies required for the vendored
                  ;; models.
                  ;; XXX: Remove them when all of the vendored packages are
                  ;; available.
                  ;;
                  go-github-com-alecthomas-units              ; github.com/ipfs/boxo
                  go-github-com-cespare-xxhash
                  go-github-com-felixge-httpsnoop             ; go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp
                  go-github-com-gabriel-vasile-mimetype       ; github.com/ipfs/boxo
                  go-github-com-go-logr-stdr                  ; go.opentelemetry.io/otel
                  go-github-com-golang-groupcache             ; go.opencensus.io/trace
                  go-github-com-google-gopacket               ; github.com/libp2p/go-libp2p-kad-dht
                  go-github-com-gorilla-mux                   ; github.com/ipfs/boxo
                  go-github-com-hashicorp-golang-lru          ; github.com/libp2p/go-libp2p-kad-dht
                  go-github-com-hashicorp-golang-lru-v2       ; github.com/ipfs/boxo
                  go-github-com-ipfs-go-bitfield              ; github.com/ipfs/boxo
                  go-github-com-ipfs-go-ipfs-redirects-file   ; github.com/ipfs/boxo
                  go-github-com-ipfs-go-verifcid              ; github.com/ipfs/go-blockservice
                  go-github-com-libp2p-go-cidranger           ; github.com/libp2p/go-libp2p-kbucket
                  go-github-com-libp2p-go-libp2p-asn-util     ; github.com/libp2p/go-libp2p-kbucket
                  go-github-com-libp2p-go-msgio               ; github.com/libp2p/go-libp2p-kad-dht
                  go-github-com-prometheus-statsd-exporter    ; contrib.go.opencensus.io/exporter/prometheus
                  go-github-com-rs-cors                       ; github.com/ipfs/go-ipfs-cmds
                  go-github-com-whyrusleeping-base32          ; github.com/ipfs/boxo
                  go-github-com-whyrusleeping-cbor            ; github.com/ipld/go-car
                  go-github-com-whyrusleeping-chunker         ; github.com/ipfs/boxo
                  go-github-com-whyrusleeping-go-keyspace     ; github.com/libp2p/go-libp2p-kad-dht
                  go-golang-org-x-oauth2                      ; github.com/ipfs/boxo
                  go-golang-org-x-term                        ; github.com/ipfs/go-ipfs-cmds
                  go-golang-org-x-xerrors                     ; github.com/whyrusleeping/cbor-gen
                  ))
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
