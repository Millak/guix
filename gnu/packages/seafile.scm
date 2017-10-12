;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Jonathan Brielmaier <jonathan.brielmaier@web.de>
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

(define-module (gnu packages seafile)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (guix build-system gnu)
  ;#:use-module (guix utils)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages web))

(define-public libsearpc
  (package
    (name "libsearpc")
    (version "3.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/haiwen/libsearpc/archive/v"
                          version "-latest.tar.gz"))
      (sha256
       (base32
        "143x6ycn1gb8qyy3rbydjgs1wm9k0i1lv82csdnsxmxpyji5zd43"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib)
       ("gobject-introspection" ,gobject-introspection)
       ("jansson" ,jansson)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python-2.7" ,python-2.7)
       ("python-simplejson" ,python-simplejson)))
    (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'autogen
            (lambda _
              (zero? (system* "sh" "autogen.sh")))))))
    (home-page "http://www.example.com")
    (synopsis "bla")
    (description
     "blubb")
    (license license:asl2.0)))

(define-public ccnet
  (package
    (name "ccnet")
    (version "6.1.1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "https://github.com/haiwen/ccnet/archive/v"
                          version ".tar.gz"))
      (sha256
       (base32
        "0662v5a11mzkr8bwsid75nraz4vi9jfs3wwin26bfx87225znnll"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("glib" ,glib)
      ; ("gobject-introspection" ,gobject-introspection)
       ("jansson" ,jansson)
       ("libevent" ,libevent)
       ("libtool" ,libtool)
       ("libsearpc" ,libsearpc)
       ("openssl" ,openssl)
       ("pkg-config" ,pkg-config)
       ("python-2.7" ,python-2.7)
      ; ("python-simplejson" ,python-simplejson)
       ("sqlite" ,sqlite)
       ("util-linux" ,util-linux)
       ("vala" ,vala)))
    (arguments
      '(#:phases
        (modify-phases %standard-phases
          (add-after 'unpack 'autogen
            (lambda _
              (zero? (system* "sh" "autogen.sh")))))))
    (home-page "http://www.example.com")
    (synopsis "bla")
    (description
     "blubb")
    (license license:gpl2))) ; exception for openssl
