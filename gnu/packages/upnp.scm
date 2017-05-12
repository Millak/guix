;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 Sree Harsha Totakura <sreeharsha@totakura.in>
;;; Copyright © 2016, 2017 Tobias Geerinckx-Rice <me@tobias.gr>
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
  #:use-module (gnu packages python)
  #:use-module (guix build-system gnu)
  #:use-module (guix download)
  #:use-module (guix licenses)
  #:use-module (guix packages))

(define-public miniupnpc
  (package
    (name "miniupnpc")
    (version "2.0.20170421")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://miniupnp.tuxfamily.org/files/"
                           name "-" version ".tar.gz"))
       (sha256
        (base32 "0n11m2wq812zms5b21h8ihw1kbyaihj9nqjiida0hskf4dmw4m13"))))
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
                (string-append (assoc-ref outputs "out") "/bin/upnpc"))))))))
    (home-page "http://miniupnp.free.fr/")
    (synopsis "UPnP protocol client library")
    (description
     "The MiniUPnPc client library facilitates access to the services provided
by any Universal Plug and Play (UPnP) Internet Gateway Device (IGD) present on
the network.  In UPnP terminology, MiniUPnPc is a UPnP Control Point.  It is
useful whenever an application needs to listen for incoming connections while
running behind a UPnP-enabled router or firewall.  Such applications include
peer-to-peer applications, active-mode FTP clients, DCC file transfers over
IRC, instant messaging, network games, and most server software.")
    (license
     (x11-style "file://LICENSE" "See 'LICENSE' file in the distribution"))))
