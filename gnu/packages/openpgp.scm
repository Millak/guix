;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2020 Justus Winter <justus@sequoia-pgp.org>
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

(define-module (gnu packages openpgp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages web))

(define-public libtmcg
  (package
    (name "libtmcg")
    (version "1.3.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/libtmcg/libTMCG-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "179b5jx3mqs9hgsj8cfwk6x8qib60kw9szk9fkz6s1gl3v83mnyx"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags '("--enable-silent-rules")))
    (inputs (list gmp libgcrypt))
    (synopsis
     "C++ library for creating secure and fair online card games")
    (description
     "The library provides a sort of useful classes, algorithms, and
high-level protocols to support an application programmer in writing such
software.  The most remarkable feature is the absence of a trusted third
party (TTP), i.e. neither a central game server nor trusted hardware
components are necessary.

The corresponding cryptographic problem, actually called Mental Poker, has
been studied since 1979 (Shamir, Rivest, and Adleman) by many authors.
LibTMCG provides the first practical implementation of such protocols.")
    (home-page "https://www.nongnu.org/libtmcg/")
    (license license:gpl2+)))

(define-public dkgpg
  (package
    (name "dkgpg")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://savannah/dkgpg/dkgpg-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1hpfg7akd5icj49i03z74hp9zj0xwl90bndn0hnw0hpb8lk7qcxg"))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags
                 '("--enable-silent-rules")
                 ;; https://savannah.nongnu.org/bugs/?58772
                 #:parallel-tests? #f))
    (inputs (list bzip2 gmp libgcrypt libtmcg zlib))
    (synopsis
     "Distributed Key Generation and Threshold Cryptography for OpenPGP")
    (description
     "The Distributed Privacy Guard (DKGPG) implements Distributed Key
Generation (DKG) and Threshold Cryptography for OpenPGP.  The generated public
keys are compatible with the standard and thus can be used by any
RFC4880-compliant application (e.g. GnuPG).  The main purposes of this
software are distributing power among multiple parties, eliminating single
points of failure, and increasing the difficulty of side-channel attacks on
private key material.

DKGPG consists of a bunch of simple command-line programs.  The current
implementation is in experimental state and should NOT be used in production
environments.")
    (home-page "https://www.nongnu.org/dkgpg/")
    (license license:gpl2+)))

(define-public rnp
  (let ((day-of-release "2022-09-22"))
    (package
      (name "rnp")
      (version "0.16.2")
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/rnpgp/rnp")
                      (commit (string-append "v" version))))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "13z5kxm48a72w4m2crwgdjdng4a4pwxsd72r2z3a4pcakfp2swi8"))))
      (build-system cmake-build-system)
      (arguments `(#:configure-flags
                   '("-DBUILD_SHARED_LIBS=on"
                     "-DBUILD_TESTING=on"
                     "-DDOWNLOAD_GTEST=off"
                     "-DDOWNLOAD_RUBYRNP=off")
                   #:phases
                   (modify-phases %standard-phases
                     (add-after 'unpack 'patch-tests
                       (lambda _
                         (substitute* "src/tests/support.cpp"
                           (("\"cp\"") (search-input-file inputs "/bin/cp")))))
                     (replace 'check
                       (lambda* (#:key tests? #:allow-other-keys)
                         (when tests?
                           ;; Some OpenPGP certificates used by the tests expire.
                           ;; To work around that, set the time to roughly the
                           ;; release date.
                           (invoke "faketime" ,day-of-release "make" "test")))))))
      (native-inputs
       (list gnupg       ; for tests
             googletest  ; for tests
             libfaketime ; for tests
             pkg-config
             python))
      (inputs (list botan bzip2 json-c zlib))
      (synopsis
       "RFC4880-compliant OpenPGP library written in C++")
      (description
       "Set of OpenPGP (RFC4880) tools that works on Linux, *BSD and macOS as a
replacement of GnuPG.  It is maintained by Ribose after being forked from
NetPGP, itself originally written for NetBSD.

librnp is the library used by rnp for all OpenPGP functions, useful for
developers to build against.  It is a “real” library, not a wrapper like GPGME
of GnuPG.")
      (home-page "https://www.rnpgp.org/")
      (license
       ;; RNP contains code written by Ribose and code derived from netpgp.
       (list
        ;; Ribose's BSD 2-Clause License and NetBSD's BSD 2-Clause License
        ;; (netpgp).
        license:bsd-2
        ;; Nominet UK's Apache 2.0 Licence (netpgp).
        license:asl2.0
        ;; Nominet UK's BSD 3-Clause License (netpgp).
        license:bsd-3)))))
