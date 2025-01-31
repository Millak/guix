;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2025 宋文武 <iyzsong@envs.net>
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

(define-module (gnu packages gambas)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix build-system gnu)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix packages)
  #:use-module (gnu packages )
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages mail)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public gambas
  (package
    (name "gambas")
    (version "3.20.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/gambas/gambas")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1fw3nark7vaw4vbmdvaaqb0an4838k0hhwwvc3dqd7f2033d41xq"))
       (modules '((guix build utils)))
       (snippet
        ;; Fix desktop file creation, upstream commit bbaa7b40.
        '(substitute* "comp/src/gb.gui.base/.src/_Gui.class"
           (("If sDir Begins [(]User\\.Home & \"/\"[)]" all)
            (string-append all " And If Access(sDir, gb.Write)"))))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:bootstrap-scripts #~'("reconf-all")
      ;; XXX: Gambas executables expect a '#! /usr/bin/env gbr3' shebang.
      #:patch-shebangs? #f
      #:configure-flags
      #~(list
         (string-append "--with-inotify-includes="
                        #$(this-package-input "glibc")
                        "/include")
         (string-append "--with-crypt-libraries="
                        #$(this-package-input "libxcrypt")
                        "/lib")
         (string-append "--with-gmp-includes="
                        #$(this-package-input "gmp")
                        "/include")
         (string-append "--with-gmp-libraries="
                        #$(this-package-input "gmp")
                        "/lib")
         (string-append "--with-mysql-includes="
                        #$(this-package-input "mysql")
                        "/include/mysql")
         (string-append "--with-mysql-libraries="
                        #$(this-package-input "mysql")
                        "/lib")
         (string-append "--with-postgresql-includes="
                        #$(this-package-input "postgresql")
                        "/include")
         (string-append "--with-postgresql-libraries="
                        #$(this-package-input "postgresql")
                        "/lib")
         (string-append "--with-bzlib2-includes="
                        #$(this-package-input "bzip2")
                        "/include")
         (string-append "--with-bzlib2-libraries="
                        #$(this-package-input "bzip2")
                        "/lib"))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'qualify-paths
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "main/gbc/gbc.c"
                (("msgfmt -o")
                 (string-append (search-input-file inputs "bin/msgfmt")
                                " -o")))))
          (add-after 'install 'install-desktop-file
            (lambda* (#:key outputs #:allow-other-keys)
              (let* ((out (assoc-ref outputs "out"))
                     (appdir (string-append out "/share/applications"))
                     (icondir (string-append out "/share/pixmaps")))
                (install-file "app/desktop/gambas3.desktop"
                              (string-append appdir "/gambas3.desktop"))
                (install-file "app/desktop/gambas3.png"
                              (string-append icondir "/gambas3.png"))))))))
    (native-inputs
     (list autoconf
           automake
           libtool
           gettext-minimal
           pkg-config))
    (inputs
     (list alure
           bzip2
           cairo
           curl
           dbus
           gdk-pixbuf
           gettext-minimal
           glew
           glibc
           glu
           gmime
           gmp
           gsl
           gst-plugins-base
           gstreamer
           gtk+
           imlib2
           libffi
           libice
           libsm
           libxcrypt
           libxml2
           libxslt
           libxtst
           (list zstd "lib")
           mesa
           mysql
           ncurses
           openal
           openssl
           pcre2
           poppler
           postgresql
           qtbase
           qtbase-5
           qtsvg
           qtsvg-5
           qtwebengine
           qtwebengine-5
           qtx11extras
           sdl2
           sdl2-image
           sdl2-mixer
           sdl2-ttf
           sqlite
           unixodbc
           webkitgtk-for-gtk3
           zlib))
    (home-page "https://gambas.sourceforge.net/")
    (synopsis "Object-oriented language and development platform")
    (description
     "GAMBAS is a graphical development environment and platform based on a
BASIC interpreter.  It is inspired by Visual Basic and Java.")
    (license license:gpl2+)))
