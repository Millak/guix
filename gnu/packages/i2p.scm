;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Jakob L. Kreuze <zerodaysfordays@sdf.org>
;;; Copyright © 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2021 Solene Rapenne <solene@perso.pw>
;;; Copyright © 2021 Efraim Flashner <efraim@flashner.co.il>
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

(define-module (gnu packages i2p)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages upnp)
  #:use-module (guix gexp)
  #:use-module (guix packages)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:))

(define-public i2pd
  (package
    (name "i2pd")
    (version "2.50.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/PurpleI2P/i2pd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1vr251mgffawi3rj51dzlnv3fs1ssz6gl17qbsyhfr5fcd7s0hc5"))))
    (build-system cmake-build-system)
    (native-inputs (list check pkg-config))
    (inputs
     (list boost miniupnpc openssl zlib))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-S" #$source "/build")
              "-DWITH_UPNP=ON"
              "-DBUILD_SHARED_LIBS=ON"
              "-DBUILD_TESTING=ON")))
    (home-page "https://i2pd.website/")
    (synopsis "Router for an end-to-end encrypted and anonymous internet")
    (description "i2pd is a client for the anonymous I2P network, upon which
applications for file sharing, web browsing, instant messaging, and more are
built. i2pd allows people from all around the world to communicate and share
information securely without restrictions.")
    (license license:bsd-3)))
