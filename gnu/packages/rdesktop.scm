;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2017 Thomas Danckaert <post@thomasdanckaert.be>
;;; Copyright © 2018, 2020 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018, 2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2019 Leo Famulari <leo@famulari.name>
;;; Copyright © 2019 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
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

(define-module (gnu packages rdesktop)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages nettle)
  #:use-module (gnu packages opencl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages security-token)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml))

(define-public rdesktop
  (package
    (name "rdesktop")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rdesktop/rdesktop/"
                                  "releases/download/v" version "/rdesktop-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1222f2srlq16bydhy44gph997iajg39sl774xxh9jdwi4cqjyg27"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags (list ;; XXX: optional dependencies missing
                               "--disable-credssp"
                               "--disable-smartcard")
       #:tests? #f))                    ; No 'check' target
    (native-inputs
     (list pkg-config))
    (inputs
     (list gnutls libx11 libxcursor nettle))
    (home-page "https://www.rdesktop.org/")
    (synopsis "Client for Windows Terminal Services")
    (description
     "rdesktop is a client for Microsoft's Windows Remote Desktop Services,
capable of natively speaking Remote Desktop Protocol (RDP).  It allows users
to remotely control a user's Windows desktop.")
    (license license:gpl3+)))

(define-public freerdp
  (package
    (name "freerdp")
    (version "2.11.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/FreeRDP/FreeRDP")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0h7yxjnl4zgl07ilh7dzbig8r7phll0wid72hm92jav6s4q75v63"))))
    (build-system cmake-build-system)
    (native-inputs
     (list docbook-xml
           docbook-xsl
           glib
           libxml2
           libxslt
           pkg-config
           xmlto))
    (inputs
     (list alsa-lib
           cairo
           cups
           dbus
           ffmpeg-4
           gsm
           lame
           libjpeg-turbo
           libusb
           libx11
           libxkbfile
           libxcursor
           libxdamage
           libxext
           libxi
           libxv
           libxrandr
           libxrender
           libxinerama
           libxshmfence
           opencl-headers
           openh264
           opensles
           openssl
           pcsc-lite ; for smartcard support
           pulseaudio
           zlib))
    (propagated-inputs (list libxkbcommon openssl wayland))
    (arguments
     (list #:build-type "RELEASE"
           #:configure-flags
           #~(list "-DWITH_JPEG=ON"
                   #$@(if (target-x86-64?)
                          #~("-DWITH_SSE2=ON")
                          #~())
                   "-DWITH_PULSE=ON"
                   "-DWITH_CUPS=ON"
                   "-DBUILD_TESTING=ON"
                   "-DWITH_SERVER=ON" ;build servers
                   "-DWITH_SHADOW=ON" ;build shadow server
                   "-DWITH_PROXY=ON")))
    (home-page "https://www.freerdp.com")
    (synopsis "Remote Desktop Protocol implementation")
    (description "FreeRDP implements Microsoft's Remote Desktop Protocol.
It consists of the @code{xfreerdp} client, libraries for client and server
functionality, and Windows Portable Runtime (WinPR), a portable implementation
of parts of the Windows API.")
    (license license:asl2.0)))

(define-public xrdp
  (package
    (name "xrdp")
    (version "0.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/neutrinolabs/xrdp/releases/download/v"
                    version "/xrdp-" version ".tar.gz"))
              (sha256
               (base32
                "15nhfh8lxwf0jlmq6knh3851sp6njps9nhl8n2867il7mcr9gmsj"))))
    (build-system gnu-build-system)
    (inputs (list check
                  fuse
                  imlib2
                  lame
                  libjpeg-turbo
                  libx11
                  libxfixes
                  libxml2
                  libxpm
                  libxrandr
                  libxslt
                  libxt
                  linux-pam
                  openssl
                  pixman
                  python
                  python-libxml2))
    (native-inputs
     (append
       (list bison
             cmocka
             flex
             gettext-minimal
             intltool)
       (if (target-x86?)
           (list nasm)
           '())
       (list pkg-config
             pixman)))
    (arguments
     (list #:configure-flags #~(list "--enable-strict-locations=yes"
                                     "--enable-fuse=yes"
                                     "--enable-mp3lame=yes"
                                     "--enable-pixman=yes"
                                     "--enable-imlib2=yes"
                                     "--enable-pam-config=unix"
                                     "--enable-ipv6=yes")
     #:phases
     #~(modify-phases %standard-phases
       (add-after 'unpack 'set-cflags-file-offset-bit-64
         (lambda _
               (setenv "CFLAGS"
                   "-D_FILE_OFFSET_BITS=64"))))))
    (home-page "https://www.xrdp.org")
    (synopsis "Remote Desktop Protocol (RDP) server")
    (description
     "Xrdp provides a graphical login to remote machines using
Microsoft Remote Desktop Protocol (RDP).  Xrdp accepts connections from a
variety of RDP clients:
@itemize
@item FreeRDP
@item rdesktop
@item KRDC
@item NeutrinoRDP
@item Windows MSTSC (Microsoft Terminal Services Client, aka mstsc.exe)
@item Microsoft Remote Desktop (found on Microsoft Store, which is distinct from MSTSC).
@end itemize")
    (license license:asl2.0)))

(define-public xorgxrdp
  (package
    (name "xorgxrdp")
    (version "0.10.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/neutrinolabs/xorgxrdp")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1dcxn0v88426j4n7irhy6h3qb21202v5xs1vr7j8xvs3sxihc2f7"))))
    (build-system gnu-build-system)
    (inputs (list check
                  imlib2
                  libx11
                  libxfixes
                  libxfont2
                  libxml2
                  libxpm
                  libxrandr
                  libxslt
                  libxt
                  pixman
                  xdpyinfo
                  xorg-server
                  xrdp))
    (native-inputs
     (append
       (list autoconf
             automake
             intltool
             libtool)
       (if (target-x86?)
           (list nasm)
           '())
       (list pkg-config
             pixman)))
    (arguments
     (list #:configure-flags #~(list "--enable-strict-locations=yes"
                                     (string-append "XRDP_CFLAGS=-I"
                                                    #$(this-package-input
                                                       "xrdp") "/common"))))
    (home-page "https://github.com/neutrinolabs/xorgxrdp")
    (synopsis "Xorg drivers for xrdp")
    (description
     "xorgxrdp is a collection of modules to be used with a pre-existing X.Org
install to make the X server act like X11rdp.  Unlike X11rdp, you don't have to
recompile the whole X Window System.  Instead, additional modules are installed
to a location where the existing Xorg installation would pick them.")
    (license license:x11)))
