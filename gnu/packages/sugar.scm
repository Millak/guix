;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2023 Ricardo Wurmus <rekado@elephly.net>
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

(define-module (gnu packages sugar)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages xorg)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system python)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (guix gexp))

(define-public sugar-toolkit-gtk3
  (package
    (name "sugar-toolkit-gtk3")
    (version "0.120")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/sugarlabs/sugar-toolkit-gtk3")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wqanc38zplyiq1vxda4bj1n0xd78zqlwml6lzklsrbz923llykz"))))
    (build-system glib-or-gtk-build-system)
    (arguments
     (list
      #:imported-modules
      `(,@%glib-or-gtk-build-system-modules
        (guix build python-build-system))
      #:modules
      '((guix build glib-or-gtk-build-system)
        ((guix build python-build-system) #:prefix python:)
        (guix build utils))
      #:phases
      '(modify-phases %standard-phases
         (add-after 'unpack 'patch-build-system
           (lambda _
             (substitute* "autogen.sh"
               (("^\"\\$srcdir/configure" m)
                (string-append "#" m)))))
         (add-after 'glib-or-gtk-wrap 'python-and-gi-wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (search-input-file outputs "bin/sugar-activity3")
               `("GUIX_PYTHONPATH" = (,(getenv "GUIX_PYTHONPATH")
                                      ,(python:site-packages inputs outputs)))
               `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (inputs
     (list alsa-lib
           libice
           libsm
           libx11
           libxfixes
           libxi
           python))
    (propagated-inputs
     ;; The gi typelib files are needed by users of this library.
     (list gdk-pixbuf
           gobject-introspection
           gtk+
           (librsvg-for-system)

           ;; This package is used as a Python library by users, so these must
           ;; be propagated.
           python-dbus
           python-decorator
           python-pygobject
           python-six))
    (native-inputs
     (list autoconf automake
           gettext-minimal
           glib
           (list glib "bin")
           intltool
           libtool
           pkg-config))
    (home-page "https://www.sugarlabs.org/")
    (synopsis "GTK+ widgets and services for Sugar components")
    (description "Sugar Toolkit provides services and a set of GTK+ widgets to
build activities and other Sugar components.  This is the GTK+ 3 binding of
the Sugar Toolkit.")
    (license license:lgpl2.1+)))
