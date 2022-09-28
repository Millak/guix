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
    ;; NOTE: We are using a Release Candidate version (for 2.0) here because
    ;; of the linker issues with the previous release.
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ClusterLabs/libqb/releases/download/v"
                    version "/libqb-" version ".tar.xz"))
              (sha256
               (base32
                "008vvw504kh40br5v2xkqavnp9vpmjvf768faqzv1d00fd53ingn"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config libxml2))
    (home-page "https://clusterlabs.github.io/libqb/")
    (synopsis "Library providing high performance logging, tracing, ipc, and poll")
    (description "Libqb is a library with the primary purpose of providing
high-performance, reusable features for client-server architecture, such as
logging, tracing, inter-process communication (IPC), and polling.  Libqb is
not intended to be an all-encompassing library, but instead provide focused
APIs that are highly tuned for maximum performance for client-server
applications.")
    (license license:lgpl2.1)))
