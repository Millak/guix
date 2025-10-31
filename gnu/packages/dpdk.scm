;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2024 Kai Jellinghaus <kaij@j10-labs.com>
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

(define-module (gnu packages dpdk)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix gexp)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public dpdk
  (package
    (name "dpdk")
    (version "25.07")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/DPDK/dpdk")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12bzmaiw4f4hz62grsp7bid0z2rpibwc21j62bfwmz4j3zci0w81"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config python-minimal python-pyelftools))
    (inputs (list
             numactl ;; for libnuma
             libarchive
             zlib
             jansson
             libxcrypt
             libpcap
             libbpf
             openssl
             libbsd))
    (arguments
     (list
      #:configure-flags
      #~(list "-Dplatform=generic"
              "-Dtests=false")
      ;; tests require /sys/devices/system/.
      #:tests? #f))
    (license
     (list
      ;; some files are additionally lgpl-2.1, but only in combination with bsd-3
      license:bsd-3 ; core components
      license:gpl2 ; kernel components
      license:expat ; lib/eal/windows/include/dirent.h, drivers/net/gve/base/*
      license:bsd-2 ; lib/eal/windows/include/getopt.h
      license:isc)) ; lib/eal/windows/getopt.c (dual licensed to bsd-2)
    (synopsis "Data Plane Development Kit")
    (description "DPDK is a set of libraries and drivers for fast
packet processing.")
    (properties `((tunable? . #t)))
    (home-page "https://www.dpdk.org/")))
