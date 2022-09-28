;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2022 Sharlatan Hellseher <sharlatanus@gmail.com>
;;;
;;; This file is part of GNU Guix.
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This module provides package definitions related for High Availability (HA)
;; software, which come as foundation to create clusterization and load
;; balancing of services.

;;; Code:

(define-module (gnu packages high-availability)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages hardware)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rsync)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module ((guix licenses)
                #:prefix license:))

(define-public libqb
  (package
    (name "libqb")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ClusterLabs/libqb/releases/download/v"
                    version "/libqb-" version ".tar.xz"))
              (sha256
               (base32
                "071k916vz9ppyb69rpk792fzjs3nf3chakn10i496scgiqh49rzi"))))
    (build-system gnu-build-system)
    (native-inputs (list autoconf automake libtool libxml2 pkg-config))
    (home-page "https://clusterlabs.github.io/libqb/")
    (synopsis
     "Library providing high performance logging, tracing, ipc, and poll")
    (description
     "Libqb is a library with the primary purpose of providing
high-performance, reusable features for client-server architecture, such as
logging, tracing, inter-process communication (IPC), and polling.  Libqb is
not intended to be an all-encompassing library, but instead provide focused
APIs that are highly tuned for maximum performance for client-server
applications.")
    (license license:lgpl2.1)))

(define-public kronosnet
  (package
    (name "kronosnet")
    (version "1.24")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/kronosnet/kronosnet")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1b8rz7f2h3scrq0xcqz58ckzsvv08g31j5jgy2v4i6w87r9c75lw"))))
    (build-system gnu-build-system)
    (arguments
     ;; XXX: Multiple tests failed. Tests
     ;; require very complex environment and for some of them root privileges to
     ;; set network configuration. It has it's own CI based on Jenkis
     ;; https://ci.kronosnet.org/.
     (list #:tests? #f
           #:configure-flags #~'("--disable-static")
           #:phases #~(modify-phases %standard-phases
                        (add-before 'bootstrap 'fix-version-gen
                          (lambda _
                            (call-with-output-file ".tarball-version"
                              (lambda (port)
                                (display #$version port))))))))
    (native-inputs (list autoconf
                         automake
                         doxygen
                         libtool
                         net-tools
                         pkg-config))
    (inputs (list lksctp-tools
                  libnl
                  libqb
                  libxml2
                  lz4
                  lzo
                  nss
                  nspr
                  openssl
                  xz
                  zlib
                  `(,zstd "lib")))
    (home-page "https://kronosnet.org/")
    (synopsis "Network abstraction layer designed for High Availability")
    (description
     "Kronosnet, often referred to as @code{knet}, is a network
 abstraction layer designed for High Availability use cases, where redundancy,
 security, fault tolerance and fast fail-over are the core requirements of
 your application.

 Kronosnet is the new underlying network protocol for Linux HA components
 (Corosync), that features ability to use multiple links between nodes,
 active/active and active/passive link failover policies, automatic link
 recovery, FIPS compliant encryption (nss and/or openssl), automatic PMTUd and
 in general better performances compared to the old network protocol.")
    (license (list license:gpl2+ license:lgpl2.1+))))
