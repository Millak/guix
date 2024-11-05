;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2019–2021 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
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

(define-module (gnu packages antivirus)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public clamav
  (package
    (name "clamav")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri
               (list
                (string-append "https://www.clamav.net/downloads/production/"
                               "clamav-" version ".tar.gz")
                (string-append "https://github.com/Cisco-Talos/clamav/"
                               "releases/download/clamav-" version
                               "/clamav-" version ".tar.gz")))
              (sha256
               (base32
                "1n3a87niad76h3mn3qxq9379gppdjqpkhwb9qkbb79irmj0ff653"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (delete-file "Cargo.lock")
                  (for-each
                   delete-file
                   (find-files ".cargo/vendor" ".cargo-checksum\\.json"))
                  (for-each delete-file-recursively
                            '("win32"                  ; unnecessary
                              "libclamunrar"))))))     ; non-free license
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "-DENABLE_MILTER=OFF" "-DENABLE_UNRAR=OFF")
      #:imported-modules `((guix build cargo-utils)
                           ,@%cmake-build-system-modules)
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'inject-rust-onenote
            (lambda _
              (substitute* "libclamav_rust/Cargo.toml"
                (("onenote_parser = .*")
                 "onenote_parser = \"0.3.1\"\n"))))
          (add-after 'patch-source-shebangs 'patch-cargo-checksums
            (lambda _
              (use-modules
               (srfi srfi-1) (ice-9 ftw) (guix build cargo-utils))
              (with-directory-excursion ".cargo/vendor"
                (for-each generate-all-checksums
                          (delete "." (delete ".." (scandir ".")))))))
          (add-after 'unpack 'skip-clamd-tests
            ;; XXX: The check?_clamd tests fail inside the build
            ;; chroot, but pass outside.
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (substitute* "unit_tests/CMakeLists.txt"
                  (("clamd_test\\.py" test)
                   (string-append
                    test " -k \"not test_clamd_08_VirusEvent\"")))))))))
    (native-inputs
     (list check ; for tests
           pkg-config
           python-minimal
           python-pytest
           rust
           (list rust "cargo")))
    (inputs
      (list bzip2
            curl
            json-c
            libressl
            libxml2
            ncurses
            pcre2
            zlib))
    (home-page "https://www.clamav.net")
    (synopsis "Antivirus engine")
    (description
     "Clam AntiVirus is an anti-virus toolkit, designed especially for e-mail
scanning on mail gateways.  It provides a number of utilities including a
flexible and scalable multi-threaded daemon, a command line scanner, and
advanced tool for automatic database updates.  The core of the package is an
anti-virus engine available in the form of a shared library.")
    (properties `((release-monitoring-url
                   . "https://github.com/Cisco-Talos/clamav/releases")))
    (license (list license:gpl2+        ;ClamAV itself
                   license:lgpl2.1      ;libclamav/mspack.[ch]
                   license:public-domain ;libclamav/7z/*, libclamav/rijndael.[ch], etc...
                   (package-license bzip2) ;modified bzip2 source in libclamav/nsis
                   license:bsd-2        ;several files in libclamav
                   license:bsd-3        ;libclamav/{regex,qsort.c,swf.[ch]
                   license:ncsa         ;libclamav/c++/PointerTracking.cpp
                   license:zlib         ;libclamav/inf*.h
                   license:x11          ;libclamav/lzw
                   (license:non-copyleft "libclamav/strlcat.c") ;"OpenBSD" license
                   license:asl2.0       ;libclamav/yara*
                   license:expat))))    ;shared/getopt.[ch]
