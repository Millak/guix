;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015, 2017, 2018, 2019, 2020 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2017, 2019-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2016, 2017, 2018, 2019, 2020 Nikita <nikita@n0.is>
;;; Copyright © 2016–2020, 2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Alex Vong <alexvong1995@gmail.com>
;;; Copyright © 2019 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2022 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2023 Adam Faiz <adam.faiz@disroot.org>
;;; Copyright © 2023, 2024 Maxim Cournoyer <maxim@guixotic.coop>
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

(define-module (gnu packages gnunet)
  #:use-module (gnu packages)
  #:use-module (gnu packages apparmor)
  #:use-module (gnu packages base)
  #:use-module (gnu packages file)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages groff)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages libidn)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libunistring)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages music)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages package-management)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages text-editors)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages backup)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix deprecation)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson))

(define-public libextractor
  (package
    (name "libextractor")
    (version "1.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnu/libextractor/libextractor-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0mgprmwdhdwq9xhfxfhcncd304425nvcc4zi8ci5f0nja4n333xv"))
              (patches
               (search-patches "libextractor-tidy-support.patch"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "static"))               ; 420 KiB .a files
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-ltdl="
                                  #$(this-package-input "libltdl")))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'force-reconfigure
                 (lambda _
                   (delete-file "configure")))
               (add-after 'install 'move-static-libraries
                 (lambda* (#:key outputs #:allow-other-keys)
                   ;; Move static libraries to the "static" output.
                   (let* ((out    #$output)
                          (lib    (string-append out "/lib"))
                          (slib   (string-append #$output:static "/lib")))
                     (mkdir-p slib)
                     (for-each (lambda (file)
                                 (install-file file slib)
                                 (delete-file file))
                               (find-files lib "\\.a$"))))))))
    (native-inputs
     (list autoconf-2.71
           automake
           gettext-minimal
           libtool
           pkg-config
           texinfo))
    (inputs
     (list bzip2
           exiv2
           file                         ;libmagic, for the MIME plug-in
           flac
           gdk-pixbuf
           giflib
           glib
           gst-plugins-base
           gstreamer
           libapparmor
           libarchive
           libgsf
           libjpeg-turbo
           libltdl
           libmp4v2
           libmpeg2
           libogg
           libsmf
           libtiff
           libvorbis
           rpm
           tidy-html
           zlib))
    (synopsis "Library to extract meta-data from media files")
    (description
     "GNU libextractor is a library for extracting metadata from files.  It
supports a very large number of file formats, including audio files, document
files, and archive files.  Each file format is implemented as a plugin, so
new formats can be added easily.  The package also contains a command-line
tool to extract metadata from a file and print the results.")
    (license license:gpl3+)
    (home-page "https://www.gnu.org/software/libextractor/")))

(define-public libmicrohttpd
  (package
   (name "libmicrohttpd")
   (version "1.0.2")
   (source (origin
            (method url-fetch)
            (uri (string-append "mirror://gnu/libmicrohttpd/libmicrohttpd-"
                                version ".tar.gz"))
            (sha256
             (base32
              "1ix843yqhcl8d5gh5652pw2scx4p5n836ca80ymms5rl136lycnz"))))
   (build-system gnu-build-system)
   (arguments
    (list #:configure-flags
          #~(list "--disable-static")))
   (inputs
    (list curl gnutls/dane libgcrypt openssl zlib))
   (synopsis "C library implementing an HTTP 1.1 server")
   (description
    "GNU libmicrohttpd is a small, embeddable HTTP server implemented as a
C library.  It makes it easy to run an HTTP server as part of another
application.  The library is fully HTTP 1.1 compliant.  It can listen on
multiple ports, supports four different threading models, and supports
IPv6.  It also features security features such as basic and digest
authentication and support for SSL3 and TLS.")
   (license license:lgpl2.1+)
   (home-page "https://www.gnu.org/software/libmicrohttpd/")))

(define-public gnunet
  (package
    (name "gnunet")
    (version "0.25.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://gnu/gnunet/gnunet-" version
                           ".tar.gz"))
       (sha256
        (base32
         "0p4gcv67rh9832vgi5k31pgpq6hmv6198dfmbyfizyaprlb6qcr1"))))
    (build-system meson-build-system)
    (inputs
     (list bluez
           glpk
           curl
           gnutls/dane
           gstreamer
           jansson
           libextractor
           libidn2
           libgcrypt
           libjpeg-turbo
           libltdl
           libmicrohttpd
           libogg
           libsodium
           libunistring
           opus
           pulseaudio
           sqlite
           zbar
           zlib))
    (native-inputs
     (list curl
           openssl
           pkg-config
           python
           python-sphinx
           python-sphinx-rtd-theme
           xxd
           which))
    (arguments
     (list
      ;; Only running util tests until the p2p tests stop being flaky.
      ;; See <https://bugs.gnunet.org/view.php?id=10430#c25964>.
      #:test-options #~(list "--suite=util")
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'disable-network-tests
            (lambda _
              (substitute* "src/cli/gns/meson.build"
                (("'test_gns_box_sbox',") "")
                (("'test_dns2gns',") ""))))
          (add-before 'check 'set-env-var-for-tests
            (lambda _
              (setenv "LANG" "en_US.UTF-8")))
          ;; Swap 'check and 'install phases and add installed binaries to $PATH.
          (add-before 'check 'set-path-for-check
            (lambda _
              (setenv "GNUNET_PREFIX" (string-append #$output "/lib"))
              (setenv "PATH" (string-append (getenv "PATH") ":"
                                            #$output "/bin"))))
          (delete 'check)
          (add-after 'install 'check
            (assoc-ref %standard-phases 'check)))))
    (synopsis "Secure, decentralized, peer-to-peer networking framework")
    (description
     "GNUnet is a framework for secure peer-to-peer networking.  The
high-level goal is to provide a strong foundation of free software for a
global, distributed network that provides security and privacy.  GNUnet in
that sense aims to replace the current internet protocol stack.  Along with
an application for secure publication of files, it has grown to include all
kinds of basic applications for the foundation of a GNU internet.

For reliable NAT traversal, also install the @var{miniupnpc} package.")
    (license license:agpl3+)
    (home-page "https://www.gnunet.org/en/")))

(define-public gnunet-scheme
  (package
    (name "gnunet-scheme")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.gnunet.org/git/gnunet-scheme.git")
                    ;; Go three commits beyond the v0.3 tag, as these three
                    ;; commits work-around
                    ;; <https://debbugs.gnu.org/cgi/bugreport.cgi?bug=49623>.
                    (commit "f5dc44e66373c29f1c84ea89d8080939a8dfbfd2")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0kvqbqijfyp3fhsqjyzwd7b3cm5khwv557wq196mv6rx47aaivgd"))
              (modules '((guix build utils)))
              (snippet
               ;; Unbundle dependencies.  TODO: build-aux/test-driver.scm
               ;; is bundled too, but it's not yet automatically copied by
               ;; autoreconf -i.
               #~(delete-file "build-aux/config.rpath"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               ;; For reproducibility, do not insert real timestamps in the PDF.
               (add-after 'unpack 'reproducible-timestamp
                 (lambda _
                   (substitute* "Makefile.am"
                     (("\\$\\(TEXMACS_CONVERT\\)")
                      "faketime -m -f '1970-01-01 00:00:00' $(TEXMACS_CONVERT)")))))))
    (inputs (list guile-3.0)) ;for pkg-config
    (propagated-inputs (list guile-bytestructures guile-gcrypt guile-pfds
                             guile-fibers))
    (native-inputs (list guile-3.0 ;as a compiler
                         ;; for cross-compilation, the guile inputs need to be
                         ;; native-inputs as well.
                         guile-bytestructures
                         guile-gcrypt
                         guile-pfds
                         guile-fibers
                         libfaketime
                         automake
                         autoconf
                         pkg-config
                         texmacs
                         xvfb-run ;for documentation
                         guile-quickcheck)) ;for tests
    (synopsis "Guile implementation of GNUnet client libraries")
    (description
     "This package provides Guile modules for connecting to various GNUnet
services.  It also has infrastructure for writing new GNUnet services and
connecting to them and can be used from multi-threaded environments.  It is
not to be confused with @code{guile-gnunet} -- @code{guile-gnunet} supports a
different set of services.

The following services are supported:

@itemize
@item NSE (network size estimation)
@item DHT (distributed hash table)
@item CADET (secure end-to-end communication between arbitrary peers)
@end itemize")
    ;; Most code is licensed as AGPL and a few modules are licensed as LGPL
    ;; or GPL.  Documentation is licensed as GFDL.
    (license (list license:agpl3+ license:gpl3+ license:fdl1.3+ license:lgpl3+))
    (home-page "https://git.gnunet.org/gnunet-scheme.git")))
