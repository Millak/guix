;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2015 Federico Beffa <beffa@fbengineering.ch>
;;; Copyright © 2016, 2017, 2018 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Theodoros Foradis <theodoros@foradis.org>
;;; Copyright © 2019 Jens Mølgaard <jens@zete.tk>
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

(define-module (gnu packages upnp)
  #:use-module (gnu packages)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages video)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix utils)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix licenses)
  #:use-module (guix packages)
  #:use-module (ice-9 match))

(define-public miniupnpc
  (package
    (name "miniupnpc")
    (version "2.1.20190824")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://miniupnp.tuxfamily.org/files/"
                           "miniupnpc-" version ".tar.gz"))
       (sha256
        (base32 "0rg1i51lnyq8zgflhcg981kq4348vgq03ndmbgiv7knd1vmfzb8z"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("python" ,python-2)))
    (arguments
     ;; The build system does not use a configure script but depends on
     ;; `make'.  Hence we should pass parameters to `make' instead and remove
     ;; the configure phase.
     '(#:make-flags
       (list
        (string-append "SH=" (assoc-ref %build-inputs "bash") "/bin/sh")
        (string-append "INSTALLPREFIX=" (assoc-ref %outputs "out"))
        "CC=gcc"

        ;; Allow executables to find libminiupnpc.so.
        (string-append "LDFLAGS=-Wl,-rpath="
                       (assoc-ref %outputs "out") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'qualify-paths
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "external-ip.sh"
               (("upnpc")
                (string-append (assoc-ref outputs "out") "/bin/upnpc")))
             #t)))))
    (home-page "http://miniupnp.free.fr/")
    (synopsis "UPnP protocol client library")
    (description
     "The MiniUPnPc client library facilitates access to the services provided
by any @dfn{Universal Plug and Play} (UPnP) @dfn{Internet Gateway Device} (IGD)
present on the network.  In UPnP terminology, MiniUPnPc is a UPnP Control Point.

It is useful whenever an application needs to listen for incoming connections
while running behind a UPnP-enabled router or firewall.  Such applications
include peer-to-peer applications, active-mode FTP clients, DCC file transfers
over IRC, instant messaging, network games, and most server software.")
    (license
     (x11-style "file://LICENSE" "See 'LICENSE' file in the distribution"))))

(define-public monero-miniupnpc
  ;; This package is the bundled version of miniupnpc used with monero.
  ;; Monero-project has been maintaining its own version of the package since
  ;; release 0.12.2.0.  It includes security fixes not included in upstream
  ;; releases.
  (let ((revision "0")
        (commit "6a63f9954959119568fbc4af57d7b491b9428d87"))
    (package
      (inherit miniupnpc)
      (name "miniupnpc-monero")
      (version (string-append "2.1-monero-0.12.3.0-" revision "."
                              (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/monero-project/miniupnp/")
                      (commit commit)))
                (sha256
                 (base32
                  "0s67zcz978iapjlq30yy9dl8qda9xhrl3jdi5f99cnbglh5gy16a"))
                (file-name (string-append name "-" version "-checkout"))
                (modules '((guix build utils)))
                (snippet
                 '(begin
                    ;; Delete miniupnp subprojects except for miniupnpc.
                    (for-each
                     delete-file-recursively
                     '("minissdpd" "miniupnpc-async" "miniupnpc-libevent"
                       "miniupnpd" ))
                    #t))))
      (arguments
       (substitute-keyword-arguments (package-arguments miniupnpc)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'build 'change-directory
               (lambda _
                 (chdir "miniupnpc")
                 #t))
             (add-after 'change-directory 'chmod-header-file
               (lambda _
                 (chmod "miniupnpc.h" #o644)
                 #t)))))))))

(define-public libupnp
  (package
    (name "libupnp")
    (version "1.6.25")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://sourceforge/pupnp/pupnp/libUPnP%20"
                          version "/" name "-" version ".tar.bz2"))
      (sha256
       (base32
        "0hzsd7rvfa87b4hxg9yj4xhdfxx9sp09r9sqdl3mqhvmcyw018y5"))))
    (build-system gnu-build-system)
    (arguments
     ;; The tests require a network device capable of multicasting which is
     ;; not available in the build environment. See
     ;; https://lists.gnu.org/archive/html/guix-devel/2015-01/msg00312.html.
     `(#:tests? #f
       #:configure-flags '("--enable-ipv6")))
    (home-page "http://pupnp.sourceforge.net")
    (synopsis "Portable SDK for UPnP Devices")
    (description
     "The portable SDK for UPnP Devices (libupnp) provides developers with an
API and code for building control points, devices, and bridges that are
compliant with Version 1.0 of the Universal Plug and Play Device Architecture
Specification and support several operating systems like Linux, *BSD, Solaris
and others.")
    (license bsd-3)))

(define-public readymedia
  (package
    (name "readymedia")
    (version "1.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.code.sf.net/p/minidlna/git")
             (commit (string-append
                      "v"
                      (string-map (match-lambda
                                    (#\. #\_)
                                    (chr chr))
                                  version)))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "160915yv38k0p5zmyncs12kkbbcd8m8fk9jq70fkfd5x6dz40xm4"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("automake" ,automake)
       ("autoconf" ,autoconf)
       ("gettext" ,gettext-minimal)))
    (inputs
     `(("libexif" ,libexif)
       ("libjpeg" ,libjpeg)
       ("libid3tag" ,libid3tag)
       ("flac" ,flac)
       ("libvorbis" ,libvorbis)
       ("sqlite" ,sqlite)
       ("ffmpeg" ,ffmpeg)))
    (home-page "https://sourceforge.net/projects/minidlna/")
    (synopsis "DLNA/UPnP-AV media server")
    (description "ReadyMedia (formerly known as MiniDLNA) is a simple media
server, which serves multimedia content to compatible clients on the network.
It aims to be fully compliant with DLNA and UPnP-AV standards.")
    (license gpl2)))
