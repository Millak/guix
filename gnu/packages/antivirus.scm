;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Eric Bavier <bavier@posteo.net>
;;; Copyright © 2018 Christopher Baines <mail@cbaines.net>
;;; Copyright © 2023 Jakob Kirsch <jakob.kirsch@web.de>
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
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages rust-apps)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public clamav
  (package
    (name "clamav")
    (version "1.4.3")
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
                "041j6jhll4lz7xhissh0094cfvhyi9jkbxcfa5dv6ra77nzwlx6q"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file-recursively
                            '(".cargo"                 ; vendored rust inputs
                              "win32"                  ; unnecessary
                              "libclamunrar"))))))     ; non-free license
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags ''("-DENABLE_MILTER=OFF" "-DENABLE_UNRAR=OFF")
      #:imported-modules `(,@%cmake-build-system-modules
                           ,@%cargo-build-system-modules)
      #:modules '(((guix build cargo-build-system) #:prefix cargo:)
                  (guix build cmake-build-system)
                  (guix build utils))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'prepare-cargo-build-system
            (lambda args
              (for-each
               (lambda (phase)
                 (format #t "Running cargo phase: ~a~%" phase)
                 (apply (assoc-ref cargo:%standard-phases phase)
                        #:vendor-dir ".cargo/vendor"
                        #:cargo-target #$(cargo-triplet)
                        args))
               '(unpack-rust-crates
                 configure
                 check-for-pregenerated-files
                 patch-cargo-checksums))))
          (add-after 'prepare-cargo-build-system 'patch-rust-requirements
            (lambda _
              (substitute* "libclamav_rust/Cargo.toml"
                ;; We make sure we use their fork.
                (("onenote_parser = .*")
                 "onenote_parser = \"*\"\n"))))
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
     (append
      (list pkg-config
            python-minimal
            python-pytest
            rust
            `(,rust "cargo"))
      (or (and=> (%current-target-system)
                 (compose list make-rust-sysroot))
          '())))
    (inputs
     (cons* bzip2
            check                       ;For tests.
            curl
            json-c
            libressl
            libxml2
            ncurses
            pcre2
            zlib
            (cargo-inputs 'clamav)))
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

(define-public yara
  (package
    (name "yara")
    (version "4.5.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/VirusTotal/yara")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1qaw1zv618jkqa5g39p1sdv8s6a7q23ayqfrqv0bj2z1g4nmn95g"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'patch-tests
            (lambda _
              (substitute* "tests/test-rules.c"
                (("/bin/sh")
                 (string-append #$(this-package-input "bash-minimal")
                                "/bin/sh"))))))))
    (inputs (list openssl bash-minimal))
    (native-inputs (list autoconf automake libtool protobuf pkg-config))
    (home-page "https://github.com/VirusTotal/yara")
    (synopsis "Pattern matching swiss knife")
    (description
     "YARA is a tool aimed at helping malware researchers to identify and
classify malware samples.  With YARA you can create rules that evaluate
conditions based on textual and binary patterns.  This package also provides
an executable to scan files, folders, and running processes and report those
that match said rules.")
    (license license:bsd-3)))
