;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016, 2018, 2020 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2019, 2021 Marius Bakke <marius@gnu.org>
;;; Copyright © 2023 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2026 Anderson Torres <anderson.torres.8519@gmail.com>
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

(define-module (gnu packages adns)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages pkg-config)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix utils))

(define-public adns
  (package
    (name "adns")
    (version "1.6.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
              (url "git://git.chiark.greenend.org.uk/~ianmdlvl/adns.git")
              (commit (string-append "adns-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0ln3l5j7wb1xazrzl76p9xar52p6l2k1cwy7jazxw1acl71k9h5n"))))
    (build-system gnu-build-system)
    (arguments
     (list
      ;; INFO: Tests expect real name resolution to work.
      #:tests? #f
      #:configure-flags
      #~(list
         ;; Make sure the programs under bin/ find libadns.so.
         (string-append "LDFLAGS=-Wl,-rpath -Wl,"
                        #$output:out
                        "/lib"))))
    (native-inputs
     (list m4))
    (home-page "https://www.gnu.org/software/adns/")
    (synopsis "Asynchronous DNS client library and utilities")
    (description
     "GNU adns is a C library that provides easy-to-use @acronym{DNS, Domain
Name System} resolution functionality.  The library is asynchronous, allowing
several concurrent calls.  The package also includes several command-line
utilities for use in scripts.")
    (license license:gpl3+)))

(define-public c-ares
  (package
    (name "c-ares")
    (version "1.34.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/c-ares/c-ares/releases/download/v"
                    version "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "0br2msk3bpl5myhjp9vr5j2scpspvbg2fpnz69dcrr4ycpnxnf7s"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'filter-live-tests
            (lambda _
              ;; Filter tests that require internet access.
              (setenv "GTEST_FILTER" "-*.Live*:*.FamilyV4*")))
          #$@(if (system-hurd?)
                 #~((add-after 'unpack 'skip-tests
                      (lambda _
                        (substitute* "test/ares-test-main.cc"
                          (("(^| )main *\\(.*" all)
                           (string-append all "  exit (77);\n")))))
                    (add-after 'filter-live-tests 'filter-hurd-tests
                      (lambda _
                        (setenv "GTEST_FILTER"
                                (string-append
                                 (getenv "GTEST_FILTER")
                                 ":.*Basic/2"
                                 ":.*CancelImmediate/2"
                                 ":.*CancelImmediateGetHostByAddr/2"
                                 ":.*CancelLater/1"
                                 ":.*FamilyUnspecified/2"
                                 ":.*FamilyV6/2"
                                 ":.*GetAddrInfoParallelLookups/1"
                                 ":.*GetHostByAddrDestroy/2"
                                 ":.*GetHostByNameCNAMENoData/2"
                                 ":.*GetHostByNameDestroyAbsolute/2"
                                 ":.*GetHostByNameDestroyRelative/2"
                                 ":.*GetHostByNameParallelLookups/1"
                                 ":.*HostAlias/2"
                                 ":.*HostAliasMissing/2"
                                 ":.*HostAliasMissingFile/2"
                                 ":.*NotImplResponse/2"
                                 ":.*RefusedResponse/2"
                                 ":.*Resend/1"
                                 ":.*RetryWithoutEDNS/2"
                                 ":.*SearchDomains/2"
                                 ":.*SearchDomainsBare/2"
                                 ":.*SearchDomainsServFailOnAAAA/2"
                                 ":.*SearchDomainsWithResentReply/1"
                                 ":.*SearchHighNdots/2"
                                 ":.*SearchNoDataThenFail/2"
                                 ":.*SearchNoDataThenNoDataBare/2"
                                 ":.*SearchNoDataThenSuccess/2"
                                 ":.*ServFailResponse/2"
                                 ":.*SimpleQuery/2"
                                 ":.*SockCallback/2"
                                 ":.*SockConfigureCallback/2"
                                 ":.*SortListV4/2"
                                 ":.*SortListV6/2"
                                 ":.*ThirdServer/2"
                                 ":.*TruncationRetry/1"
                                 ":.*UnspecifiedFamilyCname6A4/2"
                                 ":.*UnspecifiedFamilyV4/2"
                                 ":.*UnspecifiedFamilyV6/2")))))
                 #~()))))
    (native-inputs
     (list pkg-config))
    (home-page "https://c-ares.haxx.se/")
    (synopsis "C library for asynchronous DNS requests")
    (description
     "C-ares is a C library that performs DNS requests and name resolution
asynchronously.  It is intended for applications which need to perform DNS
queries without blocking, or need to perform multiple DNS queries in parallel.
The primary examples of such applications are servers which communicate with
multiple clients and programs with graphical user interfaces.")
    (license (license:x11-style "https://c-ares.haxx.se/license.html"))))

(define-public c-ares-for-node-lts
  (hidden-package c-ares))

(define-public c-ares-for-node-bootstrap
  (hidden-package
   (package
     (inherit c-ares)
     (version "1.18.1")
     (source (origin
               (method url-fetch)
               (uri (string-append
                     "https://c-ares.haxx.se/download/c-ares-" version
                     ".tar.gz"))
               (sha256
                (base32
                 "1kxviskwsaa7dcgscvssxa8ps88pdq7kq4z93gxvz7sam2l54z8s")))))))

;; gRPC requires a c-ares built with CMake in order to get the .cmake modules.
;; We can not build c-ares itself with CMake because that would introduce a
;; circular dependency through nghttp2.
;; XXX: It would be nice if we could extract the modules somehow and make them
;; work with the "normal" c-ares package instead of building a whole new library.
(define-public c-ares/cmake
  (hidden-package
   (package
     (inherit c-ares)
     (build-system cmake-build-system)
     (arguments
      `(;; XXX: Tests require name resolution (the normal variant runs no tests).
        #:tests? #f)))))
