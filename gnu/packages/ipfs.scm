;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2019, 2020 Martin Becze <mjbecze@riseup.net>
;;; Copyright © 2020, 2021, 2022 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023, 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Sharlatan Hellseher <sharlatanus@gmail.com>
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
  #:use-module (gnu packages python)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages specifications))

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
InterPlanetary Linked Data} data structures. A block is raw data accompanied
by a @acronym{Content Identifiers,CID}. The CID contains the multihash
corresponding to the block.")
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
       `(#:go ,@(if (supported-package? go-1.16)
                    `(,go-1.16)
                    `(,gccgo-11))
         #:unpack-path "github.com/ipfs/go-ipfs-cmdkit"
         #:import-path "github.com/ipfs/go-ipfs-cmdkit/files"))
      (home-page "https://github.com/ipfs/go-ipfs-cmdkit")
      (synopsis "Shared types, functions and values for go-ipfs")
      (description "@command{cmdkit} offers some types, functions and values
that are shared between @command{go-ipfs/commands} and its rewrite
@command{go-ipfs-cmds}.")
      (license license:expat))))

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
      #:import-path "github.com/ipfs/go-datastore"))
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
    (synopsis "A cbor implementation of the @code{go-ipld-format}")
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
      #:unpack-path "github.com/ipld/go-ipld-prime/"
      #:import-path "github.com/ipld/go-ipld-prime/"
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
     (list go-github-com-google-go-cmp-cmp
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
           go-github-com-multiformats-go-multiaddr-0.12))
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
           go-github-com-google-go-cmp-cmp
           go-golang-org-x-xerrors))
    (home-page "https://github.com/whyrusleeping/cbor-gen")
    (synopsis "Codegen for CBOR codecs on the specified types")
    (description
     "Basic utilities to generate fast path @url{https://cbor.io/, CBOR} codecs for
types.")
    (license license:expat)))

;; XXX: No updates for 4 years, and depends on dated Golang modules which
;; require go-1.16 to compile, see
;; <https://github.com/whyrusleeping/gx/issues/247>.
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

;; XXX: No updates for 4 years, and depends on dated Golang modules which
;; require go-1.16 to compile, see
;; <https://github.com/whyrusleeping/gx-go/issues/65>.
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
    (version "0.29.0")
    (source
     (origin
       (method url-fetch/tarbomb)
       (uri (string-append
             "https://dist.ipfs.io/kubo/v" version
             "/kubo-source.tar.gz"))
       (sha256
        (base32 "121zm4k0wz2iqrl65c7bdg5d2bvz3hvj4pavk5178dyd1p49bl5r"))
       (file-name (string-append name "-" version "-source"))
       (modules '((guix build utils)))
       (snippet '(for-each delete-file-recursively
                           ;; TODO: unbundle the rest as well
                           '("vendor/bazil.org"
                             "vendor/github.com/alecthomas"
                             "vendor/github.com/benbjohnson"
                             "vendor/github.com/beorn7/perks"
                             "vendor/github.com/blang"
                             "vendor/github.com/cenkalti"
                             "vendor/github.com/cespare"
                             "vendor/github.com/cheggaaa"
                             "vendor/github.com/coreos"
                             "vendor/github.com/davecgh"
                             "vendor/github.com/docker"
                             "vendor/github.com/dustin"
                             "vendor/github.com/elgris"
                             "vendor/github.com/facebookgo"
                             "vendor/github.com/felixge"
                             "vendor/github.com/flynn"
                             "vendor/github.com/francoispqt"
                             "vendor/github.com/fsnotify"
                             "vendor/github.com/gabriel-vasile"
                             "vendor/github.com/go-logr/logr"
                             "vendor/github.com/go-task"
                             "vendor/github.com/godbus"
                             "vendor/github.com/gogo"
                             "vendor/github.com/golang/groupcache"
                             "vendor/github.com/golang/snappy"
                             "vendor/github.com/google/gopacket"
                             "vendor/github.com/google/pprof"
                             "vendor/github.com/google/uuid"
                             "vendor/github.com/gorilla"
                             "vendor/github.com/hashicorp"
                             "vendor/github.com/ipfs/go-block-format"
                             "vendor/github.com/ipfs/go-cid"
                             "vendor/github.com/ipfs/go-cidutil"
                             "vendor/github.com/ipfs/go-datastore"
                             "vendor/github.com/ipfs/go-detect-race"
                             "vendor/github.com/ipfs/go-ipfs-delay"
                             "vendor/github.com/ipfs/go-ipfs-util"
                             "vendor/github.com/ipfs/go-ipld-cbor"
                             "vendor/github.com/ipfs/go-ipld-format"
                             "vendor/github.com/ipfs/go-ipld-git"
                             "vendor/github.com/ipfs/go-ipld-legacy"
                             "vendor/github.com/ipfs/go-log"
                             "vendor/github.com/ipfs/go-metrics-interface"
                             "vendor/github.com/ipld/go-codec-dagpb"
                             "vendor/github.com/ipld/go-ipld-prime"
                             "vendor/github.com/jackpal"
                             "vendor/github.com/jbenet"
                             "vendor/github.com/julienschmidt"
                             "vendor/github.com/klauspost"
                             "vendor/github.com/libp2p/go-socket-activation"
                             "vendor/github.com/mattn"
                             "vendor/github.com/mgutz"
                             "vendor/github.com/miekg"
                             "vendor/github.com/minio"
                             "vendor/github.com/mitchellh"
                             "vendor/github.com/mr-tron"
                             "vendor/github.com/multiformats"
                             "vendor/github.com/onsi"
                             "vendor/github.com/opentracing"
                             "vendor/github.com/pbnjay"
                             "vendor/github.com/pkg"
                             "vendor/github.com/pmezard"
                             "vendor/github.com/polydawn"
                             "vendor/github.com/prometheus/client_golang"
                             "vendor/github.com/prometheus/client_model"
                             "vendor/github.com/prometheus/common"
                             "vendor/github.com/prometheus/procfs"
                             "vendor/github.com/quic-go/quic-go"
                             "vendor/github.com/spaolacci"
                             "vendor/github.com/stretchr"
                             "vendor/github.com/syndtr"
                             "vendor/github.com/tidwall"
                             "vendor/github.com/whyrusleeping"
                             "vendor/go.uber.org"
                             "vendor/golang.org"
                             "vendor/gopkg.in"
                             "vendor/lukechampine.com")))))
    (build-system go-build-system)
    (arguments
     (list
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
                  ;;go-github-com-ceramicnetwork-go-dag-jose
                  go-github-com-cheggaaa-pb
                  go-github-com-coreos-go-systemd-v22
                  go-github-com-dustin-go-humanize
                  go-github-com-elgris-jsondiff
                  go-github-com-facebookgo-atomicfile
                  go-github-com-fsnotify-fsnotify
                  go-github-com-google-uuid
                  go-github-com-hashicorp-go-multierror
                  ;;go-github-com-ipfs-boxo
                  go-github-com-ipfs-go-block-format
                  go-github-com-ipfs-go-cid
                  go-github-com-ipfs-go-cidutil
                  go-github-com-ipfs-go-datastore
                  go-github-com-ipfs-go-detect-race
                  ;;go-github-com-ipfs-go-ds-badger
                  ;;go-github-com-ipfs-go-ds-flatfs
                  ;;go-github-com-ipfs-go-ds-leveldb
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
                  go-github-com-libp2p-go-socket-activation
                  go-github-com-mitchellh-go-homedir
                  go-github-com-multiformats-go-multiaddr-0.12
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
                  go-github-com-docker-go-units               ; github.com/containerd/cgroups
                  go-github-com-felixge-httpsnoop             ; go.opentelemetry.io/contrib/instrumentation/net/http/otelhttp
                  go-github-com-flynn-noise                   ; github.com/libp2p/go-libp2p
                  go-github-com-gabriel-vasile-mimetype       ; github.com/ipfs/boxo
                  go-github-com-go-logr-logr                  ; go.opentelemetry.io/otel
                  go-github-com-golang-groupcache             ; go.opencensus.io/trace
                  go-github-com-google-gopacket               ; github.com/libp2p/go-libp2p-kad-dht
                  go-github-com-gorilla-mux                   ; github.com/ipfs/boxo
                  go-github-com-gorilla-websocket             ; github.com/libp2p/go-libp2p
                  go-github-com-hashicorp-golang-lru          ; github.com/libp2p/go-libp2p-kad-dht
                  go-github-com-hashicorp-golang-lru-v2       ; github.com/ipfs/boxo
                  go-github-com-jackpal-go-nat-pmp            ; github.com/libp2p/go-nat
                  go-github-com-klauspost-compress            ; github.com/libp2p/go-libp2p
                  go-github-com-multiformats-go-multiaddr-fmt ; github.com/libp2p/go-libp2p
                  go-github-com-multiformats-go-multistream   ; github.com/libp2p/go-libp2p
                  go-github-com-quic-go-quic-go               ; github.com/libp2p/go-libp2p
                  go-github-com-whyrusleeping-base32          ; github.com/ipfs/boxo
                  go-github-com-whyrusleeping-cbor            ; github.com/ipld/go-car
                  go-github-com-whyrusleeping-chunker         ; github.com/ipfs/boxo
                  go-github-com-whyrusleeping-go-keyspace     ; github.com/libp2p/go-libp2p-kad-dht
                  go-golang-org-x-oauth2                      ; github.com/ipfs/boxo
                  go-golang-org-x-term                        ; github.com/ipfs/go-ipfs-cmds
                  go-golang-org-x-xerrors                     ; github.com/whyrusleeping/cbor-gen
                  go-gopkg-in-square-go-jose-v2               ; github.com/ceramicnetwork/go-dag-jose
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
