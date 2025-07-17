;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2018, 2019, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2025 Janneke Nieuwenhuizen <janneke@gnu.org>
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

(define-module (gnu packages rrdtool)
  #:use-module (guix git-download)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xml)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages))

(define-public rrdtool
  (package
    (name "rrdtool")
    (version "1.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/oetiker/rrdtool-1.x")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h9zv9yxvcf86g4zp4g3k45vk1np8zaqisk9ixgxkc58b6xx5xh8"))))
    (build-system gnu-build-system)
    (inputs
     (list cairo
           freetype
           glib
           libxml2
           pango
           python))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           groff
           libtool
           pkg-config
           ;; For tests.
           bc
           perl                         ; will also build Perl bindings
           tzdata-for-tests))
    (arguments
     (list
      #:disallowed-references (list (gexp-input tzdata-for-tests))
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'prepare-test-environment
            (lambda* (#:key inputs #:allow-other-keys)
              (setenv "TZDIR"
                      (search-input-directory inputs "share/zoneinfo"))))
          (add-after 'install 'remove-native-input-references
            (lambda _
              (let ((examples (string-append #$output
                                             "/share/rrdtool/examples")))
                ;; Drop shebangs from examples to avoid depending on native-input
                ;; perl.  It's clear from context and extension how to run them.
                (substitute* (find-files examples "\\.pl$")
                  (("^#!.*") ""))))))))
    (home-page "https://oss.oetiker.ch/rrdtool/")
    (synopsis "Time-series data storage and display system")
    (description
     "The Round Robin Database Tool (RRDtool) is a system to store and display
time-series data (e.g. network bandwidth, machine-room temperature, server
load average).  It stores the data in Round Robin Databases (RRDs), a very
compact way that will not expand over time.  RRDtool processes the extracted
data to enforce a certain data density, allowing for useful graphical
representation of data values.")
    (license license:gpl2+))) ; with license exception that allows combining
                              ; with many other licenses.
