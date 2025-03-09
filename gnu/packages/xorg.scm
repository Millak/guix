;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2017, 2018, 2020 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014, 2015 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015 Eric Dvorsak <eric@dvorsak.fr>
;;; Copyright © 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Cyrill Schenkel <cyrill.schenkel@gmail.com>
;;; Copyright © 2016, 2017, 2019-2024 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Nikita <nikita@n0.is>
;;; Copyright © 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2016 David Craven <david@craven.ch>
;;; Copyright © 2016, 2017 John Darrington <jmd@gnu.org>
;;; Copyright © 2017-2022 Marius Bakke <marius@gnu.org>
;;; Copyright © 2017, 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2017, 2020 Arun Isaac <arunisaac@systemreboot.net>
;;; Copyright © 2018–2022 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2018 Kei Kebreau <kkebreau@posteo.net>
;;; Copyright © 2018, 2020, 2022 Oleg Pykhalov <go.wigust@gmail.com>
;;; Copyright © 2018 Benjamin Slade <slade@jnanam.net>
;;; Copyright © 2019 nee <nee@cock.li>
;;; Copyright © 2019 Yoshinori Arai <kumagusu08@gmail.com>
;;; Copyright © 2019 Mathieu Othacehe <m.othacehe@gmail.com>
;;; Copyright © 2020 Liliana Marie Prikler <liliana.prikler@gmail.com>
;;; Copyright © 2020 Florian Pelz <pelzflorian@pelzflorian.de>
;;; Copyright © 2020, 2021 Michael Rohleder <mike@rohleder.de>
;;; Copyright © 2020, 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2020 Jean-Baptiste Note <jean-baptiste.note@m4x.org>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Nicolò Balzarotti <nicolo@nixo.xyz>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2021 Matthew James Kraai <kraai@ftbfs.org>
;;; Copyright © 2021 Maxime Devos <maximedevos@telenet.be>
;;; Copyright © 2021 qblade <qblade@protonmail.com>
;;; Copyright © 2021 Lu Hui <luhux76@gmail.com>
;;; Copyright © 2022 Petr Hodina <phodina@protonmail.com>
;;; Copyright © 2023 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2023, 2024 Janneke Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2023, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2023, 2024 Kaelyn Takata <kaelyn.alexi@protonmail.com>
;;; Copyright © 2024 Nicolas Graves <ngraves@ngraves.fr>
;;; Copyright © 2025 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2025 宋文武 <iyzsong@envs.net>
;;; Copyright © 2025 B. Wilson <elaexuotee@wilsonb.com>
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

(define-module (gnu packages xorg)
  #:use-module (guix gexp)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system haskell)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix utils)
  #:use-module (gnu packages)
  #:use-module (gnu packages aidc)
  #:use-module (gnu packages anthy)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages digest)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fonts)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnupg)
  #:use-module (gnu packages gperf)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages haskell-apps)
  #:use-module (gnu packages haskell-check)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages inkscape)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages libedit)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages onc-rpc)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages perl-check)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages spice)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xdisorg))

;; When cross-compiling certain packages, "--disable-malloc0returnsnull"
;; needs to be passed.  Otherwise, the configure script will try to run a
;; binary for the host on the build machine.
(define (malloc0-flags)
  (if (%current-target-system)
      ;; At least on glibc-based systems, malloc(0) evaluates to a non-NULL
      ;; pointer (except in out-of-memory situations).  On other systems,
      ;; --enable-malloc0returnsnull might be required instead.
      '("--disable-malloc0returnsnull")
      '()))

;; packages without propagated input
;; (rationale for this separation: The packages in PROPAGATED_INPUTS need to
;; be defined first, the split makes book-keeping easier.)


;; compiles only on macos
;; (define-public applewmproto


(define xorg-cf-files
  ;; The xorg-cf-files package contains the data files for the imake utility,
  ;; defining the known settings for a wide variety of platforms (many of which
  ;; have not been verified or tested in over a decade), and for many of the
  ;; libraries formerly delivered in the X.Org monolithic releases.
  ;;
  ;; License: x11, see COPYING
  (origin
    (method url-fetch)
    (uri "mirror://xorg/individual/util/xorg-cf-files-1.0.8.tar.xz")
    (sha256
     (base32 "1f8primgb6qw3zy7plbsj4a1kdhdqb04xpdys520zaygxxfra23l"))))

(define-public imake
  (package
    (name "imake")
    (version "1.0.10")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://xorg/individual/util/imake-"
                          version ".tar.xz"))
      (sha256
       (base32 "1xgcsamfij22ggc4p8anvvihwyf4adg6gjdd6v7m9cypm37cppkm"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list xorg-cf-files xorgproto))
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-data
                 (lambda* (#:key inputs outputs #:allow-other-keys)
                   (let ((out (assoc-ref outputs "out"))
                         (unpack (assoc-ref %standard-phases 'unpack))
                         (patch-source-shebangs
                          (assoc-ref %standard-phases 'patch-source-shebangs)))
                     (mkdir "xorg-cf-files")
                     (with-directory-excursion "xorg-cf-files"
                       (unpack #:source #$xorg-cf-files)
                       (patch-source-shebangs #:source #$xorg-cf-files)
                       (substitute* '("mingw.cf" "Imake.tmpl" "nto.cf" "os2.cf"
                                      "linux.cf" "Amoeba.cf" "cygwin.cf")
                         (("/bin/sh") (which "bash")))
                       (invoke "./configure"
                               (string-append "SHELL=" (which "bash"))
                               (string-append "--prefix=" out))
                       (invoke "make" "install"))))))))
    (home-page "https://www.x.org/")
    (synopsis "Source code configuration and build system")
    (description
     "Imake is a deprecated source code configuration and build system which
has traditionally been supplied by and used to build the X Window System in
X11R6 and previous releases.  As of the X Window System X11R7 release, the X
Window system has switched to using GNU autotools as the primary build system,
and the Imake system is now deprecated, and should not be used by new software
projects.  Software developers are encouraged to migrate software to the GNU
autotools system.")
    (license license:x11)))

(define-public lndir
  (package
    (name "lndir")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/util/"
                    "lndir-" version ".tar.xz"))
              (sha256
               (base32
                "1nsd23kz6iqxfcis3432zq01i54n98b94m2gcsay1k3mamx5fr9v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list xorgproto))
    (home-page "https://www.x.org/")
    (synopsis "Symlink directory into tree")
    (description "Create a shadow directory of symbolic links to another
directory tree.")
    (license license:x11)))

(define-public bdftopcf
  (package
    (name "bdftopcf")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/util/"
                           "bdftopcf-" version ".tar.xz"))
       (sha256
        (base32 "0fjjn1z0cbsmhxkms93w73j2jbzf9f3xgbnjvnisl3rk0icvwq5w"))))
    (build-system gnu-build-system)
    (inputs
      (list libxfont2))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Convert X font from BDF to PCF")
    (description
     "BDFtoPCF is a font compiler for the X server and font server.  It
converts X font from Bitmap Distribution Format to Portable Compiled Format
which can be read by any architecture.")
    (license license:x11)))

(define-public xorgproto
  (package
    (name "xorgproto")
    (version "2024.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/proto"
                                  "/xorgproto-" version ".tar.xz"))
              (sha256
               (base32
                "0nfbbi4j130m2gxzp20hp642xizbbl68jpbzahiq8nw183yja8ip"))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; To get util-macros in (almost?) all package inputs.
     (list util-macros))
    (home-page "https://cgit.freedesktop.org/xorg/proto/xorgproto")
    (synopsis "Xorg protocol headers")
    (description
     "This package provides the headers and specification documents defining
the core protocol and (many) extensions for the X Window System.")
    (license license:x11)))

(define-public bigreqsproto
  (package
    (name "bigreqsproto")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/bigreqsproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07hvfm84scz8zjw14riiln2v4w03jlhp756ypwhq27g48jmic8a6"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg BigReqsProto protocol headers")
    (description
     "Big Requests Extension defines a protocol to enable the use of
requests that exceed 262140 bytes in length.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public compositeproto
  (package
    (name "compositeproto")
    (version "0.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/compositeproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1z0crmf669hirw4s7972mmp8xig80kfndja9h559haqbpvq5k4q4"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg CompositeProto protocol headers")
    (description
     "Composite Extension contains header files and documentation for
the damage protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public damageproto
  (package
    (name "damageproto")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/damageproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0nzwr5pv9hg7c21n995pdiv0zqhs91yz3r8rn3aska4ykcp12z2w"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DamageProto protocol headers")
    (description
     "Damage Extension contains header files and documentation for
the damage protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public dmxproto
  (package
    (name "dmxproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/dmxproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "02b5x9dkgajizm8dqyx2w6hmqx3v25l67mgf35nj6sz0lgk52877"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DMXProto protocol headers")
    (description
     "DMX (Distributed Multihead X) Extension defines a protocol for clients
to access a front-end proxy X server that controls multiple back-end X
servers making up a large display.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public dri2proto
  (package
    (name "dri2proto")
    (version "2.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/dri2proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "015az1vfdqmil1yay5nlsmpf6cf7vcbpslxjb72cfkzlvrv59dgr"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DRI2Proto protocol headers")
    (description
     "Direct Rendering Infrastructure 2 Extension defines a protocol to
securely allow user applications to access the video hardware without
requiring data to be passed through the X server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public dri3proto
  (package
    (name "dri3proto")
    (version "1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/dri3proto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0x609xvnl8jky5m8jdklw4nymx3irkv32w99dfd8nl800bblkgh1"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DRI3Proto protocol headers")
    (description
     "Direct Rendering Infrastructure 3 Extension provides mechanisms to
translate between direct rendered buffers and X pixmaps.  When combined with
the Present extension, a complete direct rendering solution for OpenGL is
provided.")
    (license (license:x11-style "file://dri3proto.h"
                                "See 'dri3proto.h' in the distribution."))
    (properties `((superseded . ,xorgproto)))))

(define-public %app-defaults-dir "/lib/X11/app-defaults")

(define-public editres
  (package
    (name "editres")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/" name "-"
               version
               ".tar.xz"))
        (sha256
         (base32 "1imk7mgdc3q9lf058xisajij374x8r31ynvqmwbs9khfdxx3zz6d"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     (list libxaw libxkbfile libxmu libxt))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Tool to browse and edit X Toolkit resource specifications")
    (description
     "Editres is a tool that allows users and application developers to view
the full widget hierarchy of any X Toolkit application that speaks the Editres
protocol.  In addition, editres will help the user construct resource
specifications, allow the user to apply the resource to the application and
view the results dynamically.  Once the user is happy with a resource
specification editres will append the resource string to the user's X
Resources file.")
    (license license:x11)))

(define-public encodings
  (package
    (name "encodings")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://xorg/individual/font/encodings-"
                            version ".tar.xz"))
        (sha256
          (base32 "0xg99nmpvik6vaz4h03xay7rx0r3bf5a8azkjlpa3ksn2xi3rwcz"))))
    (build-system gnu-build-system)
    (inputs
      (list mkfontscale))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg font encoding files")
    (description "Xorg font encoding files.")
    (license license:public-domain)))

(define-public fonttosfnt
  (package
    (name "fonttosfnt")
    (version "1.2.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/"
                           "fonttosfnt-" version ".tar.gz"))
       (sha256
        (base32 "03yzsfzxd3ik126fnh2sn7fsyxsvjv83bzj946w3c4qh2y6aqh34"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list freetype libfontenc xorgproto))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Wrap a bitmap font in a sfnt (ttf or otb) wrapper")
    (description "@command{fonttosfnt} wraps a set of bdf or pcf bitmap fonts
in a sfnt (TrueType or OpenType) wrapper.")
    (license license:expat)))

(define-public font-adobe100dpi
  (package
    (name "font-adobe100dpi")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-adobe-100dpi-" version ".tar.xz"))
              (sha256 "1kwwbaiqnfm3pcysy9gw0g9xhpgmhjcd6clp7zajhqq5br2gyymn")))
    (build-system gnu-build-system)
    (inputs
      (list bdftopcf font-util mkfontdir))
    (native-inputs
      (list pkg-config))
    (arguments
      `(#:configure-flags (list
        ;; install fonts into subdirectory of package output instead of
        ;; font-util-?.?.?/share/fonts/X11
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg adobe-100dpi fonts")
    (description "Xorg adobe-100dpi fonts.")
    (license license:x11)))

(define-public font-adobe75dpi
  (package
    (name "font-adobe75dpi")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-adobe-75dpi-" version ".tar.xz"))
              (sha256 "04drk4wi176524lxjwfrnkr3dwz1hysabqfajpj6klfypqnsd08j")))
    (build-system gnu-build-system)
    (inputs
      (list bdftopcf font-util mkfontdir))
    (native-inputs
      (list pkg-config))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg adobe-75dpi fonts")
    (description "Xorg adobe-75dpi fonts.")
    (license license:x11)))

;; non-free license
;; (define-public font-adobe-utopia100dpi
;; (define-public font-adobe-utopia75dpi
;; (define-public font-adobe-utopia-type1

(define-public font-alias
  (package
    (name "font-alias")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-alias-" version ".tar.xz"))
              (sha256 "0vkb5mybc0fjfq29lgf5w1b536bwifzkyj8ad9iy7q3kpcby52cz")))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'install 'install-fonts-dir
                   ;; The X font server will not add directories to the font
                   ;; path unless they contain a "fonts.dir" file, so add some
                   ;; dummy files.
                   (lambda* (#:key outputs #:allow-other-keys)
                     (let ((out (assoc-ref outputs "out")))
                       (for-each (lambda (d)
                                   (call-with-output-file
                                       (string-append out "/share/fonts/X11"
                                                      "/" d "/fonts.dir")
                                     (lambda (p)
                                       (format p "0~%"))))
                                 '("75dpi" "100dpi" "misc" "cyrillic"))
                       #t))))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg font aliases")
    (description
     "This package provides some common aliases for Xorg fonts.
For example: @code{6x10}, @code{9x15bold}, etc.")
    (license license:x11)))

(define-public font-arabic-misc
  (package
    (name "font-arabic-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-arabic-misc-" version ".tar.xz"))
              (sha256 "0rrlcqbyx9y7hnhbkjir8rs6jkfqyalj1zvhr8niv2n7a8dydzs6")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg arabic-misc font")
    (description "Xorg arabic-misc font.")
    (license license:x11)))

;; non-free license
;; (define-public font-bh100dpi
;; (define-public font-bh75dpi
;; (define-public font-bh-lucidatypewriter100dpi
;; (define-public font-bh-lucidatypewriter75dpi
;; (define-public font-bh-ttf
;; (define-public font-bh-type1
;; (define-public font-bitstream100dpi
;; (define-public font-bitstream75dpi

(define-public font-cronyx-cyrillic
  (package
    (name "font-cronyx-cyrillic")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-cronyx-cyrillic-" version ".tar.xz"))
              (sha256 "12dpsvif85z1m6jvq9g91lmzj0rll5rh3871mbvdpzyb1p7821yw")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg cronyx-cyrillic font")
    (description "Xorg cronyx-cyrillic font.")
    (license license:x11)))

;; no license
;; (define-public font-cursor-misc

;; non-free license
;; (define-public font-daewoo-misc

(define-public font-dec-misc
  (package
    (name "font-dec-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-dec-misc-" version ".tar.xz"))
              (sha256 "1xqs2qg21h5xg519810hw4bvykjdpf0xgk0xwp0bxy4g3lh6inc2")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg dec-misc font")
    (description "Xorg dec-misc font.")
    (license license:x11)))

;; non-free license
;; (define-public font-ibm-type1

(define-public font-isas-misc
  (package
    (name "font-isas-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-isas-misc-" version ".tar.xz"))
              (sha256 "1z1qqi64hbp297f6ryiswa4ikfn7mcwnb8nadyglni6swsxrbra7")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg isas-misc font")
    (description "Xorg isas-misc font.")
    (license license:x11)))

;; non-free license
;; (define-public font-jis-misc

(define-public font-micro-misc
  (package
    (name "font-micro-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-micro-misc-" version ".tar.xz"))
              (sha256 "0hzryqyml0bzzw91vqdmzdlb7dm18jmyz0mxy6plks3sppbbkq1f")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg micro-misc font")
    (description "Xorg micro-misc font.")
    (license license:public-domain)))

(define-public font-misc-cyrillic
  (package
    (name "font-misc-cyrillic")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-misc-cyrillic-" version ".tar.xz"))
              (sha256 "14z9x174fidjn65clkd2y1l6pxspmvphizap9a8h2h06adzil0kn")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg misc-cyrillic fonts")
    (description "Xorg misc-cyrillic fonts.")
    (license license:x11)))

(define-public font-misc-ethiopic
  (package
    (name "font-misc-ethiopic")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-misc-ethiopic-" version ".tar.xz"))
              (sha256 "04mnd620s9wkdid9wnf181yh5vf0n7l096nc3z4zdvm1w7kafja7")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir mkfontscale))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg misc-ethiopic fonts")
    (description "Xorg misc-ethiopic fonts.")
    (license license:x11)))

;; non-free license
;; (define-public font-misc-meltho

(define-public font-misc-misc
  (package
    (name "font-misc-misc")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-misc-misc-" version ".tar.xz"))
              (sha256 "1vcgc6lbc53fqaz8alhxcb6f231hhvj9hn2nkzg1mclbymhy7avr")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir font-util bdftopcf))
    (native-inputs
     (list pkg-config))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg misc-misc fonts")
    (description "Xorg misc-misc fixed fonts.")
    (license license:public-domain)))

(define-public font-mutt-misc
  (package
    (name "font-mutt-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-mutt-misc-" version ".tar.xz"))
              (sha256 "095vd33kqd157j6xi4sjxwdsjpwpgqliifa8nkybq8rcw7s5j8xi")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg mutt-misc fonts")
    (description "Xorg mutt-misc fonts.")
    (license license:x11)))

(define-public font-schumacher-misc
  (package
    (name "font-schumacher-misc")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-schumacher-misc-" version ".tar.xz"))
              (sha256 "0w40lr214n39al449fnm4k1bpyj3fjrhz2yxqd6a6m8yvc69z14b")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir font-util bdftopcf))
    (native-inputs
      (list pkg-config))
    (arguments
      `(#:configure-flags (list
        (string-append "--with-fontrootdir=" %output "/share/fonts/X11"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg schumacher-misc fonts")
    (description "Xorg schumacher-misc fonts.")
    (license license:x11)))

(define-public font-screen-cyrillic
  (package
    (name "font-screen-cyrillic")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-screen-cyrillic-" version ".tar.xz"))
              (sha256 "1h75zn1rp7bdv6av4cnrajpaq6fkd7dx1lc7aijpw32qrnw8nxcg")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg screen-cyrillic fonts")
    (description "Xorg screen-cyrillic fonts.")
    (license license:x11)))

(define-public font-sony-misc
  (package
    (name "font-sony-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-sony-misc-" version ".tar.xz"))
              (sha256 "0swlhjmmagrfkip4i9yq7cr56hains1j41mjs05nxc6c7y19zc76")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg sony-misc fonts")
    (description "Xorg sony-misc fonts.")
    (license license:x11)))

(define-public font-sun-misc
  (package
    (name "font-sun-misc")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-sun-misc-" version ".tar.xz"))
              (sha256 "17yvhk1hlajm3q57r09q8830zz7cnckrg8hgzajgyyljdl8xv16x")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg sun-misc fonts")
    (description "Xorg sun-misc fonts.")
    (license license:x11)))

(define-public font-util
  (package
    (name "font-util")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://xorg/individual/font/font-util-"
                            version ".tar.xz"))
        (sha256
          (base32 "15fcd3rzlgm51fwgfapg6vzyd1kkd28rj149xq7iajqr7h9697sw"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg font utilities")
    (description
     "Xorg font package creation/installation utilities.")
    (license license:x11)))

(define-public font-winitzki-cyrillic
  (package
    (name "font-winitzki-cyrillic")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-winitzki-cyrillic-" version ".tar.xz"))
              (sha256 "1a4pz8f7hz6nn9xirz2k1j81ykl3lwrpi1ydmzipciy15l984v9v")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir bdftopcf))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg winitzki-cyrillic font")
    (description "Xorg winitzki-cyrillic font.")
    (license license:public-domain)))

(define-public font-xfree86-type1
  (package
    (name "font-xfree86-type1")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/font/"
                                  "font-xfree86-type1-" version ".tar.xz"))
              (sha256 "0ds8xbgxy9h0bqn2p38vylfzn8cqkp7n51kwmw1c18ayi9w2qg59")))
    (build-system gnu-build-system)
    (inputs
      (list mkfontdir mkfontscale))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg xfree86-type1 font")
    (description "Xorg xfree86-type1 font.")
    (license license:x11)))

(define-public fontsproto
  (package
    (name "fontsproto")
    (version "2.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/fontsproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1f2sdsd74y34nnaf4m1zlcbhyv8xb6irnisc99f84c4ivnq4d415"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg FontsProto protocol headers")
    (description
     "Fonts Extension contains header files and documentation for
the fonts protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public glproto
  (package
    (name "glproto")
    (version "1.4.17")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/glproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0h5ykmcddwid5qj6sbrszgkcypwn3mslvswxpgy2n2iixnyr9amd"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg GLProto protocol headers")
    (description
     "OpenGL Extension defines a protocol for the client to send 3D
rendering commands to the X server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public ghc-xcb-types
  (package
    (name "ghc-xcb-types")
    (version "0.13.0")
    (source
     (origin
       (method url-fetch)
       (uri (hackage-uri "xcb-types" version))
       (sha256
        (base32 "0qdfj4d83b1fjmlysqncgi65ldf3qnrsj4lync95mgbaq2kzxj2r"))))
    (build-system haskell-build-system)
    (properties '((upstream-name . "xcb-types")))
    (inputs (list ghc-xml))
    (home-page "http://community.haskell.org/~aslatter/code/xcb-types")
    (synopsis "Parse XML files used by the XCB project")
    (description
     "This package provides types which mirror the structures used in the XCB code
generation XML files and parses these XML files into Haskell data structures.")
    (license license:bsd-3)))

(define-public iceauth
  (package
    (name "iceauth")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/iceauth-"
               version
               ".tar.xz"))
        (sha256
         (base32 "0ad0kbr5bfdk9na3jmjpg26gd6hwv4lxja2nkdwxrybal9yzpvix"))))
    (build-system gnu-build-system)
    (inputs
      (list libice))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "ICE authority file utility")
    (description
     "ICEAuth program is used to edit and display the authorization
information used in connecting with ICE (Inter-Client Exchange).  It
operates very much like the xauth program for X11 connection
authentication records.")
    (license license:x11)))

(define-public inputproto
  (package
    (name "inputproto")
    (version "2.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/inputproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07gk7v006zqn3dcfh16l06gnccy7xnqywf3vl9c209ikazsnlfl9"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg InputProto protocol headers")
    (description
     "Input Extension defines a protocol to provide additional input
devices management such as graphic tablets.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public kbproto
  (package
    (name "kbproto")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/kbproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0mxqj1pzhjpz9495vrjnpi10kv2n1s4vs7di0sh3yvipfq5j30pq"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg KBProto protocol headers")
    (description
     "X Keyboard (XKB) Extension defines a protocol to provide a number
of new capabilities and controls for text keyboards.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

;; requires applewmproto, which compiles only on macos
;; (define-public libapplewm

(define-public libdmx
  (package
    (name "libdmx")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libdmx-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0kzprd1ak3m3042m5hra50nsagswciis9p21ckilyaqbidmf591m"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
      (list xorgproto libxext libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg DMX library")
    (description
     "DMX (Distributed Multihead X) extension library.")
    (license license:x11)))

(define-public libxshmfence
  (package
    (name "libxshmfence")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/lib/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "046y7zn8agp8kdr1lg11yyvpx90i9sjf77h25jhgx5msd84xz96l"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (native-inputs (list pkg-config))
    (inputs (list xorgproto))
    (home-page "https://www.x.org/")
    (synopsis "Xorg shared memory fences library")
    (description
     "This library provides an interface to shared-memory fences for
synchronization between the X server and direct-rendering clients.")

    ;; Same license as libevdev.
    (license (license:x11-style "file://COPYING"))))

(define-public libfontenc
  (package
    (name "libfontenc")
    (version "1.1.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libfontenc-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1ihlznj4m49jn1887cr86qqhrrlghvbfj7bbh230svi30pac60kv"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
      (list zlib xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/lib/libfontenc")
    (synopsis "Xorg font encoding library")
    (description "Xorg font encoding library.")
    (license license:x11)))

(define-public libfs
  (package
    (name "libfs")
    (version "1.0.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "mirror://xorg/individual/lib/libFS-" version ".tar.xz"))
        (sha256
          (base32 "0xrv9x5v6km7ib3d5k9xr704xkhfvigh8i507mb9i706hqybvawv"))))
    (build-system gnu-build-system)
    (inputs
      (list xtrans xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Font Service client library")
    (description
     "Font Service client library is used by clients of X Font
Servers (xfs), such as xfsinfo, fslsfonts, and the X servers
themselves.")
    (license license:x11)))

(define-public libpciaccess
  (package
    (name "libpciaccess")
    (version "0.18.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libpciaccess-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0xpslrjnfrc1a7y8f8qwnd3wq24ndpj2q77ds12mbnwand239x2a"))))
    (build-system meson-build-system)
    (arguments
     (list
      ;; Make sure libpciaccess can read compressed 'pci.ids' files as
      ;; provided by pciutils.
      #:configure-flags
      #~(list "-Dzlib=enabled"
              (string-append "-Dpci-ids="
                             (assoc-ref %build-inputs "pciutils")
                             "/share/hwdata"))))
    (inputs
     (list pciutils))                   ;for 'pci.ids.gz'
    (propagated-inputs
     (list zlib))                       ;in Requires.private of pciaccess.pc
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg PCI access library")
    (description "Xorg Generic PCI access library.")
    (license license:x11)))

;;; A static libpciaccess is required by hurd and netdde.
(define-public libpciaccess-static
  (package/inherit libpciaccess
    (name "libpciaccess-static")
    (arguments
     (substitute-keyword-arguments (package-arguments libpciaccess)
       ((#:configure-flags flags)
        #~(cons "-Ddefault_library=static" #$flags))))
    (propagated-inputs
     (list zlib (list zlib "static")))))

(define-public libpthread-stubs
  (package
    (name "libpthread-stubs")
    (version "0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/xcb/libpthread-stubs-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1g224hyy694jch54357zc895z46r90xs193hg4m7rfnfxinmdnjr"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Library with pthread stubs")
    (description
     "This library provides weak aliases for pthread functions not
provided in libc or otherwise available by default.  Libraries like
libxcb rely on pthread stubs to use pthreads optionally, becoming
thread-safe when linked to libpthread, while avoiding any performance
hit when running single-threaded.")
    (license license:x11)))

(define-public libsm
  (package
    (name "libsm")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libSM-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1g10pn1zhcyc1ys8skp6p46gn6qgkif8kqdwlz1hsrzgllny3y9a"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     ;; In Require.private of sm.pc
     (list libice
           (list util-linux "lib")
           xorgproto))
    (inputs (list xtrans))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Session Management library")
    (description "Xorg Session Management library.")
    (license license:x11)))

(define-public libwindowswm
  (package
    (name "libwindowswm")
    (version "1.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libWindowsWM-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1p0flwb67xawyv6yhri9w17m1i4lji5qnd0gq8v1vsfb8zw7rw15"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto libxext libx11 windowswmproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg WindowsWM library")
    (description
     "Cygwin/X rootless window management extension.
WindowsWM is a simple library designed to interface with the Windows-WM
extension.  This extension allows X window managers to better interact
with the Cygwin XWin server when running X11 in a rootless mode.")
    (license license:x11)))

(define-public libxcomposite
  (package
    (name "libxcomposite")
    (version "0.4.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXcomposite-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "11rcvk380l5540gfqy9p8mbzw3l1p5g8l214p870f28smvqbqh7y"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     ;; xcomposite.pc refers to all these.
      (list xorgproto libxfixes libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Composite library")
    (description
     "Client library for the Composite extension to the X11 protocol.")
    (license license:x11)))

(define-public libxdmcp
  (package
    (name "libxdmcp")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXdmcp-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1312l8x3asib77wgf123w3nbabnky61mb6pnmmqapbf350l259fq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs (list xorgproto)) ;required by xdmcp.pc
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Display Manager Control Protocol library")
    (description "Xorg Display Manager Control Protocol library.")
    (license license:x11)))

(define-public libxft
  (package
    (name "libxft")
    (version "2.3.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXft-"
               version
               ".tar.gz"))
        (sha256
          (base32
            "1a3qybcvhdshgj1wynhsahdnrlmfv2wrjklyh1j2whj4v3i8zr1j"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     ;; xft.pc refers to all these.
     (list libxrender
           freetype
           fontconfig))
    (inputs
      (list libx11 xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg FreeType library")
    (description
     "Xorg FreeType library connects X applications with the FreeType font
rasterization library.  Xft uses fontconfig to locate fonts so it has no
configuration files.")
    (license license:x11)))

(define-public libxkbfile
  (package
    (name "libxkbfile")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/lib/libxkbfile-"
             version
             ".tar.xz"))
       (sha256
        (base32
         "1v2bhw1q1cj3wjfs0igq393iz10whcavbyxlm3k9xfvsk7m3xdm9"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     ;; Required in xkbfile.pc.
     (list libx11 kbproto))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XKB file handling library")
    (description "Xorg XKB file handling library.")
    (license license:x11)))

(define-public libxmu
  (package
    (name "libxmu")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXmu-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1cp82iz7yki63iykvb3alwy4nwy01k2axi5rqpyfafca4j9pgcpw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     ;; In Requires or Requires.private of xmu.pc
     (list libx11 libxt xorgproto libxext))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xmu library")
    (description
     "Xmu library contains miscellaneous utilities and is not part of the
Xlib standard.  It is intended to support clients in the Xorg distribution;
vendors may choose not to distribute this library if they wish.  Therefore,
applications developers who depend on this library should be prepared to
treat it as part of their software base when porting.")
    (license license:x11)))

(define-public libxpm
  (package
    (name "libxpm")
    (version "3.5.17")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXpm-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "0hvf49qy55gwldpwpw7ihcmn5i2iinpjh2rbha63hzcy060izcv4"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
      (list libxt xorgproto libxext))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XPM library")
    (description "XPM (X Pixmap) image file format library.")
    (license license:x11)))

(define-public libxres
  (package
    (name "libxres")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXres-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0pvlzahqd8fcyq10wi7ipbxvgrg93hn0vqsymhw7b6sb93rlcx4s"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (inputs
      (list xorgproto libxext libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Resource extension library")
    (description "X Resource extension library.")
    (license license:x11)))

(define-public libxscrnsaver
  (package
    (name "libxscrnsaver")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXScrnSaver-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1zi0r6mqa1g0hhsp02cdsjcxmsbipiv0v65c1h4pl84fydcjikbm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static"
                           ;; This fixes cross compiling.
                           ,@(if (%current-target-system)
                               '("--enable-malloc0returnsnull=yes")
                               '()))))
    (propagated-inputs
      (list libx11 libxext xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Screen Saver library")
    (description "X11 Screen Saver extension client library.")
    (license license:x11)))

(define-public libxxf86dga
  (package
    (name "libxxf86dga")
    (version "1.1.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/lib/"
                           "libXxf86dga-" version ".tar.xz"))
       (sha256
        (base32 "03wqsxbgyrdbrhw8fk3fxc9nk8jnwz5537ym2yif73w0g5sl4i5y"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     (list xorgproto))
    (inputs
     (list libx11 libxext))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XFree86-DGA library")
    (description "Client library for the XFree86-DGA extension.")
    (license license:x11)))

(define-public luit
  (package
    (name "luit")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/luit-"
             version
             ".tar.bz2"))
       (sha256
        (base32
         "0dn694mk56x6hdk6y9ylx4f128h5jcin278gnw2gb807rf3ygc1h"))
       ;; See https://bugs.freedesktop.org/show_bug.cgi?id=47792;
       ;; should become obsolete with the next release.
       (patches (search-patches "luit-posix.patch"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-localealiasfile="
                             (search-input-file
                              %build-inputs "share/X11/locale/locale.alias")))))
    (inputs
     (list libfontenc libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Convert terminal I/O from legacy encodings to UTF-8")
    (description
     "Luit is a filter that can be run between an arbitrary application and
a UTF-8 terminal emulator such as xterm.  It will convert application
output from the locale's encoding into UTF-8, and convert terminal
input from UTF-8 into the locale's encoding.")
    (license license:x11)))

(define-public makedepend
  (package
    (name "makedepend")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/makedepend-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1m1dg32z22fw9shg0wsxw7dvdcnfm16gq9qxpkfxixpzb6vdxl4j"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg makedepend utility")
    (description
     "Makedepend is an utility for creating dependencies in makefiles.")
    (license license:x11)))

(define-public mkfontscale
  (package
    (name "mkfontscale")
    (version "1.2.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/mkfontscale-"
               version
               ".tar.xz"))
        (sha256
          (base32 "0pp7dyfrrkrqxslk9q8660k0h4swaqlixsnnph2fxb7i8k1ws899"))))
    (build-system gnu-build-system)
    (inputs
      (list zlib xorgproto freetype libfontenc))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Create an index of scalable font files for X server")
    (description
     "MkFontScale creates the @code{fonts.scale} and @code{fonts.dir} index
files used by the legacy X11 font system.")
    (license license:x11)))

(define-public presentproto
  (package
    (name "presentproto")
    (version "1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/presentproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1f96dlgfwhsd0834z8ydjzjnb0cwha5r6lxgia4say4zhsl276zn"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg PresentProto protocol headers")
    (description
     "Present Extension provides a way for applications to update their
window contents from a pixmap in a well defined fashion, synchronizing
with the display refresh and potentially using a more efficient
mechanism than copying the contents of the source pixmap.")
    (license (license:x11-style "file://presentproto.h"
                                "See 'presentproto.h' in the distribution."))
    (properties `((superseded . ,xorgproto)))))

(define-public printproto
  (package
    (name "printproto")
    (version "1.0.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/printproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "06liap8n4s25sgp27d371cc7yg9a08dxcr3pmdjp761vyin3360j"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg PrintProto protocol headers")
    (description
     "Print Extension defines a protocol for a portable,
network-transparent printing system.")
    (license license:x11)))

(define xcffibgen
  (package
    (name "xcffibgen")
    (version "1.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/tych0/xcffib")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0wz6zlaqsmpw7ahaadpd7m5n5c5b2ya30xwsana4j5ljygdvzqp9"))))
    (build-system haskell-build-system)
    (native-inputs
     (list ghc-hunit ghc-test-framework ghc-test-framework-hunit))
    (inputs
     (list libxcb
           ghc
           ghc-filemanip
           ghc-split
           ghc-language-python
           ghc-attoparsec
           ghc-either
           ghc-optparse-applicative
           ghc-xcb-types))
    (home-page "https://github.com/tych0/xcffib")
    (synopsis "Build tool for python-xcbffib bindings")
    (description
     "This is an internal package that provides a build tool to
generate code for the @code{python-xcbffib} package.")
    (license license:expat)))

(define-public python-xcffib
  (package
    (name "python-xcffib")
    (version "1.5.0")
    (source (package-source xcffibgen))
    (build-system pyproject-build-system)
    (native-inputs
     (list pkg-config
           python-setuptools
           python-wheel
           which
           xcb-proto
           xcffibgen))
    (inputs
     (list libxcb))
    (propagated-inputs
     (list python-cffi ; used at run time
           python-six))
    (arguments
     (list
      ;; Tests seem to require version 3.12 of Python.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'generate-bindings
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "Makefile"
                (("^GEN=.*")
                 (format #f "GEN=~a~%"
                         (search-input-file inputs "/bin/xcffibgen"))))
              (invoke "make" "xcffib")))
          (add-before 'build 'fix-libxcb-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "xcffib/__init__.py"
                (("ctypes\\.util\\.find_library\\(\"xcb\"\\)")
                 (format #f "~s"
                         (search-input-file inputs "/lib/libxcb.so.1")))))))))
    (home-page "https://github.com/tych0/xcffib")
    (synopsis "XCB Python bindings")
    (description
     "Xcffib is a replacement for xpyb, an XCB Python bindings.  It adds
support for Python 3 and PyPy.  It is based on cffi.")
    (license license:expat)))

(define-public randrproto
  (package
    (name "randrproto")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/randrproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0s4496z61y5q45q20gldwpf788b9nsa8hb13gnck1mwwwwrmarsc"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg RandRProto protocol headers")
    (description
     "Resize and Rotate Extension defines a protocol for clients to
dynamically change X screens, so as to resize, rotate and reflect the root
window of a screen.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public recordproto
  (package
    (name "recordproto")
    (version "1.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/recordproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w3kgr1zabwf79bpc28dcnj0fpni6r53rpi82ngjbalj5s6m8xx7"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg RecordProto protocol headers")
    (description
     "Record Extension defines a protocol for the recording and playback
of user actions in the X Window System.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public renderproto
  (package
    (name "renderproto")
    (version "0.11.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/renderproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0dr5xw6s0qmqg0q5pdkb4jkdhaja0vbfqla79qh5j1xjj9dmlwq6"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg RenderProto protocol headers")
    (description
     "Rendering Extension defines a protocol for a digital image composition
as the foundation of a new rendering model within the X Window System.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public resourceproto
  (package
    (name "resourceproto")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/resourceproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0638iyfiiyjw1hg3139pai0j6m65gkskrvd9684zgc6ydcx00riw"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg ResourceProto protocol headers")
    (description
     "Resource Extension defines a protocol that allows a client to
query the X server about its usage of various resources.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public scrnsaverproto
  (package
    (name "scrnsaverproto")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/scrnsaverproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0rfdbfwd35d761xkfifcscx56q0n56043ixlmv70r4v4l66hmdwb"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg ScrnSaverProto protocol headers")
    (description
     "Screen Saver Extension defines a protocol to control screensaver
features and to query screensaver info on specific windows.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public sessreg
  (package
    (name "sessreg")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/sessreg-" version ".tar.xz"))
       (sha256
        (base32 "1hmc9wsfgl2wmy0kccwa4brxbv02w5wiz5hrz72dsz87x1fwsah2"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Register X sessions in system utmp/utmpx databases")
    (description
     "SessReg is a simple program for managing utmp/wtmp entries for X
sessions.  It was originally written for use with xdm, but may also be
used with other display managers such as gdm or kdm.")
    (license license:x11)))

(define-public setxkbmap
  (package
    (name "setxkbmap")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/setxkbmap-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1pps0x66512y3f7v6xgnb6gjbllsgi4q5zxmjcdiv60fsia8b3dy"))))
    (build-system gnu-build-system)
    (inputs
      (list libxkbfile libxrandr xkeyboard-config libx11))
    (native-inputs
      (list pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-config-root="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Set the keyboard using the X Keyboard Extension")
    (description
     "Setxkbmap is an X11 client to change the keymaps in the X server
for a specified keyboard to use the layout determined by the options
listed on the command line.")
    (license license:x11)))

(define-public smproxy
  (package
    (name "smproxy")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/smproxy-"
                           version ".tar.xz"))
       (sha256
        (base32 "01gkz4n2pfxiklzzx3ghnm9shx3626jcriwvrs3pvawxrhvr5aaa"))))
    (build-system gnu-build-system)
    (inputs
      (list libxt libxmu))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Session Manager Proxy")
    (description
     "SMProxy allows X applications that do not support X11R6 session
management to participate in an X11R6 session.")
    (license license:x11)))

(define-public util-macros
  (package
    (name "util-macros")
    (version "1.20.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/util/util-macros-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "13ifr0dabci130xk04kp9bq16g9kbyzf8x1mgdyjsrsglbmnkhls"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-makefile-in
           (lambda _
             (substitute* "Makefile.in"
               ;; Install xorg-macros.pc in PREFIX/lib/pkgconfig,
               ;; not PREFIX/share/pkgconfig.
               (("\\$\\(datadir\\)/pkgconfig") "$(libdir)/pkgconfig"))
             #t))
         (add-after 'install 'post-install-cleanup
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion out
                 (delete-file "share/util-macros/INSTALL")
                 (rmdir "share/util-macros"))
               #t))))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg autoconf macros")
    (description
     "This package provides a set of autoconf macros used by the
configure.ac scripts in other Xorg modular packages, and is needed to
generate new versions of their configure scripts with autoconf.")
    (license license:x11)))

(define-public videoproto
  (package
    (name "videoproto")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/videoproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "00m7rh3pwmsld4d5fpii3xfk5ciqn17kkk38gfpzrrh8zn4ki067"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg VideoProto protocol headers")
    (description
     "Video Extension provides a protocol for a video output mechanism,
mainly to rescale video playback in the video controller hardware.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public windowswmproto
  (package
    (name "windowswmproto")
    (version "1.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/windowswmproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0syjxgy4m8l94qrm03nvn5k6bkxc8knnlld1gbllym97nvnv0ny0"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg WindowsWMProto protocol headers")
    (description
     "WindowsWM Extension defines a protocol, used for coordination between
an X11 server and the Microsoft Windows native window manager.  WindowsWM
is only intended to be used on Cygwin when running a rootless XWin
server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public x11perf
  (package
    (name "x11perf")
    (version "1.7.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/test/x11perf-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0a0qm7y9qyyma4pd8plpm5npglcgd43z0bwkk2lp2qg9n220vy14"))))
    (build-system gnu-build-system)
    (inputs
      (list libx11 libxft libxmu libxrender))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X server performance benchmarker")
    (description
     "X11Perf is a simple performance benchmarker for the Xorg X server.")
    (license license:x11)))

(define-public xauth
  (package
    (name "xauth")
    (version "1.1.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xauth-" version
                           ".tar.xz"))
       (sha256
        (base32 "0cwxzq7lv932rcfad3baanr541qcjjdhyrhxy0bzjcp3pac581z7"))))
    (build-system gnu-build-system)
    (inputs
     (list libxmu libxext libxau libx11))
    (native-inputs
     (list cmdtest pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X authority file utility")
    (description
     "XAuth program is used to edit and display the authorization
information used in connecting to the X server.")
    (license license:x11)))

(define-public xbacklight
  (package
    (name "xbacklight")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xbacklight-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1vp890ic26y4k2l0haw94z4nim3j7gp3g9w5flw2zj0qdw70phyl"))))
    (build-system gnu-build-system)
    (inputs
     (list libxcb xcb-util))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Control display backlight")
    (description
     "Xbacklight is used to adjust the backlight brightness where
supported.  It uses the RandR extension to find all outputs on the X
server supporting backlight brightness control and changes them all in
the same way.")
    (license license:x11)))

(define-public xbitmaps
  (package
    (name "xbitmaps")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/data/xbitmaps-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0yhgrllia3lbqx9b21w31w4sppx1a9ggrk62hrys2ckqi1aasv5d"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X bitmaps")
    (description
     "xbitmaps provides basic bitmaps (little pictures) used by some
legacy X clients.")
    (license license:x11)))

(define-public xcalc
  (package
    (name "xcalc")
    (version "1.1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/"
                           "xcalc-" version ".tar.gz"))
       (sha256
        (base32 "1ixvkkar851zb8d4vlqyrih80xrq5mpwlkb9yzbj2dwxry5vg8nx"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases (modify-phases %standard-phases
                  (add-after
                   'configure 'mutate-makefile
                   (lambda _
                     (substitute* "Makefile"
                       (("^appdefaultdir = .*$")
                        (string-append "appdefaultdir = " %output
                                       ,%app-defaults-dir "\n"))))))))
    (inputs
     (list libxaw))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Hand calculator for the X Window system")
    (description "Xcalc is a scientific calculator desktop accessory that can
emulate a TI-30 or an HP-10C.")
    (license license:x11)))

(define-public xcb-proto
  (package
    (name "xcb-proto")
    (version "1.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://xcb.freedesktop.org/dist/xcb-proto-"
               version ".tar.xz"))
        (sha256
         (base32
          "130lc8jx43s83496nc8jn47zixjcp4abgsz69pvrjiqg279aq6rc"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config python-minimal-wrapper))
    (home-page "https://xcb.freedesktop.org/")
    (synopsis "XML-XCB protocol descriptions")
    (description
     "XCB-Proto provides the XML-XCB protocol descriptions that libxcb
uses to generate the majority of its code and API.  XCB-Proto is
separated from libxcb to allow reuse by other projects, such as
additional language bindings, protocol dissectors, or documentation
generators.

XCB-Proto also contains language-independent Python libraries that are
used to parse an XML description and create objects used by Python code
generators in individual language bindings.")
    (license license:x11)))

(define-public xcmiscproto
  (package
    (name "xcmiscproto")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xcmiscproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1pyjv45wivnwap2wvsbrzdvjc5ql8bakkbkrvcv6q9bjjf33ccmi"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XCMiscProto protocol headers")
    (description
     "XC-MISC Extension defines a protocol that provides Xlib two ways
to query the server for available resource IDs.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xcmsdb
  (package
    (name "xcmsdb")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xcmsdb-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0f5wddi707cjqm21hynckkqr12mpjqn3dq9fm5gb11w19270di2y"))))
    (build-system gnu-build-system)
    (inputs
      (list libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Device Color Characterization utility")
    (description
     "XCMSDB is used to load, query, or remove Device Color
Characterization data stored in properties on the root window of the
screen as specified in section 7, Device Color Characterization, of the
X11 Inter-Client Communication Conventions Manual (ICCCM).")
    (license license:x11)))

(define-public xcursor-themes
  (package
    (name "xcursor-themes")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/data/xcursor-themes-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1j3qfga5llp8g702n7mivvdvfjk7agsgnbglbfh99n13i3sfiflm"))))
    (build-system gnu-build-system)
    (inputs
      (list libxcursor xcursorgen))
    (native-inputs
      (list pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-cursordir="
                            (assoc-ref %outputs "out")
                            "/share/icons"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Default Xorg cursors")
    (description
     "This package provides a default set of cursor themes for the Xorg
X server: @code{handhelds}, @code{redglass} and @code{whiteglass}.")
    (license license:x11)))

(define-public hackneyed-x11-cursors
  (package
    (name "hackneyed-x11-cursors")
    (version "0.8.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/Enthymeme/hackneyed-x11-cursors.git")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1mjwbny4rid9dzz6xfb8l5rkwki41sfhdp970cf3w2pi9kyg1njs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ;no test suite
       #:make-flags (list (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'set-inkscape-environment-variable
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((inkscape (search-input-file inputs "/bin/inkscape")))
               (setenv "INKSCAPE" inkscape))))
         (add-before 'build 'placate-inkscape-warnings
           (lambda _
             (setenv "HOME" (getcwd))))
         (add-after 'build 'generate-black-cursors
           (lambda* (#:key make-flags parallel-build #:allow-other-keys)
             (let ((build (assoc-ref %standard-phases 'build))
                   (make-flags/extended
                    `(,@make-flags
                      "THEME_NAME=Hackneyed-Dark"
                      "COMMON_SOURCE=theme/common-dark.svg"
                      "RSVG_SOURCE=theme/right-handed-dark.svg"
                      "LSVG_SOURCE=theme/left-handed-dark.svg")))
               (build #:make-flags make-flags/extended
                      #:parallel-build parallel-build))))
         (add-after 'install 'install-black-cursors
           (lambda* (#:key make-flags #:allow-other-keys)
             (apply invoke `("make" "install" ,@make-flags
                             "THEME_NAME=Hackneyed-Dark")))))))
    (native-inputs (list imagemagick inkscape/stable xcursorgen))
    (home-page "https://gitlab.com/Enthymeme/hackneyed-x11-cursors")
    (synopsis "Classic cursor theme for X11")
    (description "Hackneyed is a scalable cursor theme mildly resembling old
Windows 3.x cursors.  The cursors are available in white and black colors.  A
left-handed version of the cursors is also included.")
    (license license:x11)))

(define-public xcursorgen
  (package
    (name "xcursorgen")
    (version "1.0.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xcursorgen-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "16yc82k4vp7icmf9247z4v38r65pdf032mrpzxj5wa2fggi3rcrj"))))
    (build-system gnu-build-system)
    (inputs
      (list libxcursor libpng))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Create an X cursor file from PNG images")
    (description
     "XCursorGen prepares X11 cursor sets for use with libXcursor.")
    (license license:x11)))

(define-public xdpyinfo
  (package
    (name "xdpyinfo")
    (version "1.3.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xdpyinfo-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0aw2yhx4ys22231yihkzhnw9jsyzksl4yyf3sx0689npvf0sbbd8"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto
            libx11
            libxxf86vm
            libxxf86dga
            libxtst
            libxrender
            libxinerama
            libxi
            libxcomposite
            libdmx))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg display information utility")
    (description
     "Xdpyinfo is used to display information about an X server: the
capabilities of a server, the predefined values for various parameters
used in communicating between clients and the server, and the different
types of screens, visuals, and X11 protocol extensions that are
available.")
    (license license:x11)))

(define-public xdriinfo
  (package
    (name "xdriinfo")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xdriinfo-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0d7p9fj3znq0av9pjgi2kphqaz5w7b9hxlz63zbxs69bknp8p0yx"))))
    (build-system gnu-build-system)
    (inputs
      (list mesa libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Query DRI configuration information")
    (description
     "XDRIInfo is used to query configuration information of X11
DRI (Direct Rendering Infrastructure) drivers.")
    (license license:x11)))

(define-public xev
  (package
    (name "xev")
    (version "1.2.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xev-"
                           version ".tar.xz"))
       (sha256
        (base32 "1mq7332kgisd9yq0w0rv11vhwhgpkmpg7pfdlyn776dc13hcbqb1"))))
    (build-system gnu-build-system)
    (arguments `(#:configure-flags ',(malloc0-flags)))
    (inputs
      (list libxrender libxrandr xorgproto libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xev")
    (synopsis "Print contents of X events")
    (description
     "XEv creates a window and then asks the X server to send it X11
events whenever anything happens to the window (such as it being moved,
resized, typed in, clicked in, etc.).  You can also attach it to an
existing window.  It is useful for seeing what causes events to occur
and to display the information that they contain; it is essentially a
debugging and development tool, and should not be needed in normal
usage.")
    (license license:x11)))

(define-public xextproto
  (package
    (name "xextproto")
    (version "7.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xextproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1c2vma9gqgc2v06rfxdiqgwhxmzk2cbmknwf1ng3m76vr0xb5x7k"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XExtProto protocol headers")
    (description
     "XExtProto provides the following extensions: DOUBLE-BUFFER, DPMS,
Extended-Visual-Information, Generic Event Extension, LBX, MIT-SHM,
MIT-SUNDRY-NONSTANDARD, Multi-Buffering, SECURITY, SHAPE, SYNC, TOG-CUP,
XC-APPGROUP, XTEST.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public libevdev
  (package
    (name "libevdev")
    (version "1.13.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/" name "/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32
         "1gygrc20d7cw70fdb5mp0ns3pmy8skggyl1mbpfyp3i0hv7amwdb"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")))
    (native-inputs (list python))
    (home-page "https://www.freedesktop.org/wiki/Software/libevdev/")
    (synopsis "Wrapper library for evdev devices")
    (description
     "libevdev is a wrapper library for evdev devices. it moves the common
tasks when dealing with evdev devices into a library and provides a library
interface to the callers, thus avoiding erroneous ioctls, etc.

The eventual goal is that libevdev wraps all ioctls available to evdev
devices, thus making direct access unnecessary.")
    (license (license:x11-style "file://COPYING"))))

(define-public xf86-input-evdev
  (package
    (name "xf86-input-evdev")
    (version "2.11.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-evdev-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "058k0xdf4hkn8lz5gx4c08mgbzvv58haz7a32axndhscjgg2403k"))))
    (build-system gnu-build-system)
    (inputs
      `(("udev" ,eudev)
        ("libevdev" ,libevdev)
        ("mtdev" ,mtdev)
        ("xorg-server" ,xorg-server)))
    (native-inputs (list pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Generic input driver for X server")
    (description
     "xf86-input-evdev is a generic input driver for the Xorg X server.
This driver supports all input devices that the kernel knows about,
including most mice, keyboards, tablets and touchscreens.")
    (license license:x11)))

(define-public xf86-input-libinput
  (package
    (name "xf86-input-libinput")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/driver/"
                    name "-" version ".tar.xz"))
              (sha256
               (base32
                "1rl06l0gdqmc4v08mya93m74ana76b7s3fzkmq8ylm3535gw6915"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--with-sdkdir="
                            %output "/include/xorg"))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("libinput" ,libinput-minimal)
       ("xorg-server" ,xorg-server)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Input driver for X server based on libinput")
    (description
     "xf86-input-libinput is an input driver for the Xorg X server based
on libinput.  It is a thin wrapper around libinput, so while it does
provide all features that libinput supports it does little beyond.")
    (license (list license:x11          ; only install-sh
                   license:expat))))    ; everything else

(define-public xf86-input-joystick
  (package
    (name "xf86-input-joystick")
    (version "1.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-joystick-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1lnc6cvrg81chb2hj3jphgx7crr4ab8wn60mn8f9nsdwza2w8plh"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Joystick input driver for X server")
    (description
     "xf86-input-joystick is a joystick input driver for the Xorg X server.
It is used to control the pointer with a joystick device.")
    (license license:x11)))

(define-public xf86-input-keyboard
  (package
    (name "xf86-input-keyboard")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-keyboard-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0mvwxrnkq0lzhjr894p420zxffdn34nc2scinmp7qd1hikr51kkp"))))
    (build-system gnu-build-system)
    ;; Linux is no longer supported since 2.0.0, use libinput or evdev instead.
    (supported-systems '("i586-gnu" "x86_64-gnu"))
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Keyboard input driver for X server")
    (description
     "xf86-input-keyboard is a keyboard input driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-input-mouse
  (package
    (name "xf86-input-mouse")
    (version "1.9.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-mouse-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0s4rzp7aqpbqm4474hg4bz7i7vg3ir93ck2q12if4lj3nklqmpjg"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Mouse input driver for X server")
    (description
     "xf86-input-mouse is a mouse input driver for the Xorg X server.
This driver supports four classes of mice: serial, bus and PS/2 mice,
and additional mouse types supported by specific operating systems, such
as USB mice.")
    (license license:x11)))

(define-public xf86-input-synaptics
  (package
    (name "xf86-input-synaptics")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-synaptics-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1hmm3g6ab4bs4hm6kmv508fdc8kr2blzb1vsz1lhipcf0vdnmhp0"))))
    (build-system gnu-build-system)
    (inputs (list libx11 libxi libevdev mtdev xorg-server))
    (native-inputs (list pkg-config))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-sdkdir="
                            (assoc-ref %outputs "out")
                            "/include/xorg")
             (string-append "--with-xorg-conf-dir="
                            (assoc-ref %outputs "out")
                            "/share/X11/xorg.conf.d"))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Touchpad input driver for X server")
    (description
     "xf86-input-synaptics is a touchpad driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-input-void
  (package
    (name "xf86-input-void")
    (version "1.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-input-void-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "11bqy2djgb82c1g8ylpfwp3wjw4x83afi8mqyn5fvqp03kidh4d2"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Void (null) input driver for X server")
    (description
     "xf86-input-void is a null input driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-amdgpu
  (package
    (name "xf86-video-amdgpu")
    (version "23.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/driver/"
                           "xf86-video-amdgpu-" version ".tar.xz"))
       (sha256
        (base32 "0qf0kjh6pww5abxmqa4c9sfa2qq1hq4p8qcgqpfd1kpkcvmg012g"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "AMD Radeon video driver for X server")
    (description
     "xf86-video-amdgpu is an AMD Radeon video driver for the Xorg
X server.")
    (license license:x11)))

(define-public xf86-video-ark
  (package
    (name "xf86-video-ark")
    (version "0.7.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-ark-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "0p88blr3zgy47jc4aqivc6ypj4zq9pad1cl70wwz9xig29w9xk2s"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Ark Logic video driver for X server")
    (description
     "xf86-video-ark is an Ark Logic video driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-armada-novena
  (package
    (name "xf86-video-armada-novena")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/novena-next/xf86-video-armada.git")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01h27ck9s9rxyi9n8v9lqpd9aacln0w5pd5zlfgj740060q3wf30"))))
    (build-system gnu-build-system)
    (arguments
     (list #:configure-flags
           #~(list "--disable-vivante" "--disable-etnaviv"
                   (string-append "--with-etnaviv-source="
                                  (assoc-ref %build-inputs "etna-viv")))
           #:phases
           #~(modify-phases %standard-phases
               (replace 'bootstrap
                 (lambda _
                   (mkdir "m4")
                   (invoke "autoreconf" "--install"))))))
    (inputs
     (list xorg-server libdrm-armada-novena))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("automake" ,automake)
       ("autoconf" ,autoconf)
       ("libtool" ,libtool)
       ("etna-viv" ,(origin
                      (method git-fetch)
                      (uri (git-reference
                            (url "https://github.com/laanwj/etna_viv.git")
                            (commit "d4c87f61bdcd15f98d4bdbea8c34788bb71fbad9")))
                      (sha256
                       (base32
                        "0qfzwsmcq1lcwng707vlfql8nnkqqyhspfydxpfs03gn8w0l2cm2"))))))
    (synopsis "Xorg driver for Freescale i.MX6")
    (description "This package provides Xorg support for Freescale i.MX6.")
    (home-page "https://github.com/novena-next/xf86-video-armada")
    (license license:gpl2)))

(define-public xf86-video-ast
  (package
    (name "xf86-video-ast")
    (version "1.2.0")
    (home-page "https://cgit.freedesktop.org/xorg/driver/xf86-video-ast/")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/driver/xf86-video-ast-"
                    version ".tar.xz"))
              (sha256
               (base32
                "14sx6dm0nmbf1fs8cazmak0aqjpjpv9wv7v09w86ff04m7f4gal6"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (synopsis "ASpeed Technologies video driver for X server")
    (description
     "xf86-video-ast is an ASpeed Technologies video driver for the Xorg
X server.")
    (license license:x11)))

(define-public xf86-video-ati
  ;; The current release is too old to build with our inputs.
  (let ((commit "5eba006e4129e8015b822f9e1d2f1e613e252cda")
        (revision "1"))
    (package
      (name "xf86-video-ati")
      (version (git-version "19.1.0" revision commit))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/xorg/driver/xf86-video-ati.git")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "1n49wx0v13jh8vv17sxgrmmpi1mk3n2wph07jfmxmzqahpcn4lkn"))))
      (build-system gnu-build-system)
      (inputs (list mesa xorgproto xorg-server))
      (native-inputs
       (list pkg-config autoconf automake libtool))
      (home-page "https://www.x.org/wiki/")
      (synopsis "ATI Radeon video driver for X server")
      (description
       "xf86-video-ati is an ATI Radeon video driver for the Xorg
X server.")
      (license license:x11))))

(define-public xf86-video-cirrus
  (package
    (name "xf86-video-cirrus")
    (version "1.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-cirrus-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "00b468w01hqjczfqz42v2vqhb14db4wazcqi1w29lgfyhc0gmwqf"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Cirrus Logic video driver for X server")
    (description
     "xf86-video-cirrus is a Cirrus Logic video driver for the Xorg
X server.")
    (license license:x11)))

(define-public xf86-video-dummy
  (package
    (name "xf86-video-dummy")
    (version "0.4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/driver/"
                           "xf86-video-dummy-" version ".tar.xz"))
       (sha256
        (base32 "1byzsdcnlnzvkcqrzaajzc3nzm7y7ydrk9bjr4x9lx8gznkj069m"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Dummy video driver for X server")
    (description
     "Virtual/offscreen frame buffer driver for the Xorg X server.")
    ;; per https://lists.freedesktop.org/archives/xorg/2020-June/060316.html
    (license license:x11)))

(define-public xf86-video-fbdev
  (package
    (name "xf86-video-fbdev")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-fbdev-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "11zk8whari4m99ad3w30xwcjkgya4xbcpmg8710q14phkbxw0aww"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Framebuffer device video driver for X server")
    (description
     "xf86-video-fbdev is a video driver for the Xorg X server for
framebuffer device.")
    (license license:x11)))

(define-public xf86-video-freedreno
  (let ((commit "ccba8f89995de7d5e1b216e580b789c4cda05035"))
    (package
      (name "xf86-video-freedreno")
      (version (string-append "1.4.0-1-" (string-take commit 7)))
      (source
       (origin
         ;; there's no current tarball
         (method git-fetch)
         (uri (git-reference
               (url (string-append "https://anongit.freedesktop.org/git/xorg/"
                                   "driver/xf86-video-freedreno.git"))
               (commit commit)))
         (sha256
          (base32
           "0bl9m1agi793lcddv94j8afzw1xc9w810q91mbq0n3dscbbcr9nh"))
         (file-name (string-append name "-" version))))
      (build-system gnu-build-system)
      (inputs
       `(("libdrm" ,libdrm)
         ("mesa" ,mesa)
         ("udev" ,eudev)
         ("xorg-server" ,xorg-server)
         ("zlib" ,zlib)))
      (native-inputs
       (list pkg-config autoconf automake libtool))
       ;; This driver is only supported on ARM systems.
      (supported-systems '("armhf-linux" "aarch64-linux"))
      (arguments
       `(#:configure-flags
         (list (string-append "--with-xorg-conf-dir="
                              (assoc-ref %outputs "out")
                              "/share/X11/xorg.conf.d"))
         #:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               ;; autogen.sh calls configure unconditionally.
               (invoke "autoreconf" "-vfi"))))))
      (home-page "https://www.x.org/wiki/")
      (synopsis "Adreno video driver for X server")
      (description
       "xf86-video-freedreno is a 2D graphics driver for the Xorg X server.
It supports a variety of Adreno graphics chipsets.")
      (license license:x11))))

(define-public xf86-video-geode
  (package
    (name "xf86-video-geode")
    (version "2.11.21")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/driver/xf86-video-geode-"
                           version ".tar.xz"))
       (sha256
        (base32 "07lzbyxss0m5i4j58z43zri2baawci9q1ykv1g828wqi2hzsqml2"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (supported-systems
     ;; This driver is only supported on i686 systems.
     (filter (lambda (system) (string-prefix? "i686-" system))
             %supported-systems))
    (home-page "https://www.x.org/wiki/")
    (synopsis "AMD Geode GX/LX video driver for X server")
    (description
     "xf86-video-geode is an Xorg X server video driver for the AMD
Geode GX and LX processors.  The GX component supports both XAA and EXA
for graphics acceleration.  The LX component supports EXA, including
compositing.  Both support Xv overlay and dynamic rotation with XRandR.")
    (license license:x11)))

;; Driver for obsolete graphics cards, depends on libglide:
;; http://sourceforge.net/projects/glide/ ,
;; last updated in 2003, and which does not compile out of the box any more.
;; (define-public xf86-video-glide
;;   (package
;;     (name "xf86-video-glide")
;;     (version "1.2.0")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (string-append
;;                "mirror://xorg/X11R7.7/src/everything/xf86-video-glide-"
;;                version
;;                ".tar.bz2"))
;;         (sha256
;;           (base32
;;             "0byapm9mnpqk3wijfnnan3d22ii5cw6dmg4xn1625iiz89j5vs1l"))))
;;     (build-system gnu-build-system)
;;     (inputs `(("pkg-config" ,pkg-config)
;;               ("xorg-server" ,xorg-server)))
;;     (home-page "https://www.x.org/wiki/")
;;     (synopsis "Glide video driver for X server")
;;     (description
;;      "xf86-video-glide is a Glide video driver for the Xorg X server.")
;;     (license license:x11)))

(define-public xf86-video-i128
  (package
    (name "xf86-video-i128")
    (version "1.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-i128-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "0imwmkam09wpp3z3iaw9i4hysxicrrax7i3p0l2glgp3zw9var3h"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "I128 video driver for X server")
    (description
     "xf86-video-i128 is an I128 (Imagine 128) video driver for the Xorg
X server.")
    (license license:x11)))

(define-public xf86-video-intel
  (let ((commit "31486f40f8e8f8923ca0799aea84b58799754564")
        (revision "18"))
    (package
      (name "xf86-video-intel")
      (version (git-version "2.99.917" revision commit))
      (source
       (origin
         ;; there's no current tarball
         (method git-fetch)
         (uri (git-reference
               (url "https://gitlab.freedesktop.org/xorg/driver/xf86-video-intel.git")
               (commit commit)))
         (sha256
          (base32
           "0lv6vlx9gvp34pidmx4gxgb0qjf0km0gabbaxw141nnvj1azv94y"))
         (file-name (git-file-name name version))))
      (build-system gnu-build-system)
      (inputs `(("mesa" ,mesa)
                ("udev" ,eudev)
                ("libx11" ,libx11)
                ("libxfont" ,libxfont2)
                ("xorg-server" ,xorg-server)))
      (native-inputs
       (list pkg-config autoconf automake libtool))
      (supported-systems
       ;; This driver is only supported on Intel systems.
       (filter (lambda (system) (or (string-prefix? "i686-" system)
                                    (string-prefix? "x86_64-" system)))
               %supported-systems))
      (arguments
       '(#:configure-flags '("--with-default-accel=uxa")))
      (home-page "https://www.x.org/wiki/")
      (synopsis "Intel video driver for X server")
      (description
       "xf86-video-intel is a 2D graphics driver for the Xorg X server.
It supports a variety of Intel graphics chipsets.")
      (license license:x11))))

(define-public xf86-video-mach64
  (package
    (name "xf86-video-mach64")
    (version "6.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-mach64-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "0917498pgm629v84h7ddsmhqa6nc3z2msbv1x9nxrlkwl93npffn"))))
    (build-system gnu-build-system)
    (inputs (list mesa xorgproto xorg-server))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Mach64 video driver for X server")
    (description
     "xf86-video-mach64 is a video driver for the Xorg X server.
This driver is intended for all ATI video adapters based on the Mach64
series or older chipsets, providing maximum video function within
hardware limitations.  The driver is also intended to optionally provide
the same level of support for generic VGA or 8514/A adapters.")
    (license license:x11)))

(define-public xf86-video-mga
  (package
    (name "xf86-video-mga")
    (version "2.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-mga-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "0wxbcgg5i4yq22pbc50567877z8irxhqzgl3sk6vf5zs9szmvy3v"))))
    (build-system gnu-build-system)
    (inputs (list mesa xorgproto xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Matrox video driver for X server")
    (description
     "xf86-video-mga is a Matrox video driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-neomagic
  (package
    (name "xf86-video-neomagic")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-neomagic-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "153lzhq0vahg3875wi8hl9rf4sgizs41zmfg6hpfjw99qdzaq7xn"))))
    (build-system gnu-build-system)
    (inputs (list xorgproto xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "NeoMagic video driver for X server")
    (description
     "xf86-video-neomagic is a NeoMagic video driver for the Xorg X server.")
    (license license:x11)))

;; This driver depends on XAA which has been removed from xorg-server.

;; (define-public xf86-video-newport
;;   (package
;;     (name "xf86-video-newport")
;;     (version "0.2.4")
;;     (source
;;       (origin
;;         (method url-fetch)
;;         (uri (string-append
;;                "mirror://xorg/individual/driver/xf86-video-newport-"
;;                version
;;                ".tar.bz2"))
;;         (sha256
;;           (base32
;;             "1yafmp23jrfdmc094i6a4dsizapsc9v0pl65cpc8w1kvn7343k4i"))))
;;     (build-system gnu-build-system)
;;     (inputs `(("xorg-server" ,xorg-server)))
;;     (native-inputs `(("pkg-config" ,pkg-config)))
;;     (home-page "https://www.x.org/wiki/")
;;     (synopsis "Newport video driver for X server")
;;     (description
;;      "xf86-video-newport is an Xorg X server video driver for the SGI
;; newport cards.")
;;     (license license:x11)))

(define-public xf86-video-nv
  (package
    (name "xf86-video-nv")
    (version "2.1.23")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-nv-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "1jlap6xjn4pfwg9ab8fxm5mwf4dqfywp70bgc0071m7k66jbv3f6"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "NVIDIA video driver for X server")
    (description
     "This package contains Xorg support for the NVIDIA GeForce 8 series of
graphics processors.

There are a few caveats of which to be aware: the XVideo extension is not
supported, and the RENDER extension is not accelerated by this driver.")
    (license license:x11)))

(define-public xf86-video-nouveau
  (package
    (name "xf86-video-nouveau")
    (version "1.0.18")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/driver/xf86-video-nouveau-"
                           version ".tar.xz"))
       (sha256
        (base32 "1z9bqfr81w2ahq2vyphgjbl0ffxndrpxb4mqqly76srjcj7d6ap8"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://nouveau.freedesktop.org")
    (synopsis "NVIDIA video driver for X server")
    (description
     "This package provides modern, high-quality Xorg drivers for NVIDIA
graphics cards.")
    (license license:x11)))

(define-public xf86-video-qxl
  (package
    (name "xf86-video-qxl")
    (version "0.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append
                "mirror://xorg/individual/driver/"
                "xf86-video-qxl-" version ".tar.xz"))
              (sha256
               (base32
                "0pwncx60r1xxk8kpp9a46ga5h7k7hjqf14726v0gra27vdc9blra"))))
    (build-system gnu-build-system)
    (inputs
      (list libxfont2 spice-protocol xorg-server xorgproto))
    (native-inputs
      (list pkg-config))
    (synopsis "Qxl video driver for X server")
    (description "xf86-video-qxl is a video driver for the Xorg X server.
This driver is intended for the spice qxl virtio device.")
    (home-page "https://www.spice-space.org")
    (license license:x11)))

(define-public xf86-video-r128
  (package
    (name "xf86-video-r128")
    (version "6.13.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/driver/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0igpfgls5nx4sz8a7yppr42qi37prqmxsy08zqbxbv81q9dfs2zj"))))
    (build-system gnu-build-system)
    (inputs (list mesa xorgproto xorg-server))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "ATI Rage 128 video driver for X server")
    (description
     "xf86-video-r128 is a video driver for the Xorg X server.
This driver is intended for ATI Rage 128 based cards.")
    (license license:x11)))

(define-public xf86-video-savage
  (package
    (name "xf86-video-savage")
    (version "2.4.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-savage-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "1bqhgldb6yahpgav7g7cyc4kl5pm3mgkq8w2qncj36311hb92hb7"))))
    (build-system gnu-build-system)
    (inputs (list mesa xorgproto xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Savage video driver for X server")
    (description
     "xf86-video-savage is an S3 Savage video driver for the Xorg X server.")
    (license license:x11)))

(define-public xf86-video-siliconmotion
  (package
    (name "xf86-video-siliconmotion")
    (version "1.7.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-siliconmotion-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "1h4g2mqxshaxii416ldw0aqy6cxnsbnzayfin51xm2526dw9q18n"))
        (patches (search-patches "xf86-video-siliconmotion-fix-ftbfs.patch"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Silicon Motion video driver for X server")
    (description
     "xf86-video-siliconmotion is a Silicon Motion video driver for the
Xorg X server.")
    (license license:x11)))

(define-public xf86-video-sis
  (package
    (name "xf86-video-sis")
    (version "0.12.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-sis-"
               version
               ".tar.bz2"))
        (sha256
         (base32
          "0nrs6cjldlhakx5987fiiggjrlzilsbdc7l9pz22x1iwslbkz78i"))))
    (build-system gnu-build-system)
    (inputs (list mesa xorgproto xorg-server))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Sis video driver for X server")
    (description
     "xf86-video-SiS is a SiS video driver for the Xorg X server.
This driver supports SiS chipsets of 300/315/330/340 series.")
    (license license:bsd-3)))

(define-public xf86-video-tdfx
  (package
    (name "xf86-video-tdfx")
    (version "1.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-tdfx-"
               version
               ".tar.bz2"))
        (sha256
          (base32
           "0qc5wzwf1n65si9rc37bh224pzahh7gp67vfimbxs0b9yvhq0i9g"))))
    (build-system gnu-build-system)
    (inputs (list mesa xorgproto xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "3Dfx video driver for X server")
    (description
     "xf86-video-tdfx is a 3Dfx video driver for the Xorg X server.")
    (license license:x11)))

;; no license
;; (define-public xf86-video-v4l

(define-public xf86-video-vesa
  (package
    (name "xf86-video-vesa")
    (version "2.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-vesa-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1ccvaigb1f1kz8nxxjmkfn598nabd92p16rx1g35kxm8n5qjf20h"))))
    (build-system gnu-build-system)
    (inputs (list xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "VESA video driver for X server")
    (description
     "xf86-video-vesa is a generic VESA video driver for the Xorg
X server.")
    (license license:x11)))

(define-public v86d
  (package
    (name "v86d")
    (version "0.1.10")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mjanusz/v86d")
             (commit (string-append name "-" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1c4iiggb5r9i2hxhk8c6q1m2vpfva39l1w33fsfkrz6fav6x34pp"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           ;; remove bundled x86emu
           (for-each delete-file
                     (filter (lambda (name) ;keep customized Makefile
                               (not (string-suffix? "Makefile" name)))
                             (find-files "libs/x86emu")))
           ;; remove non-working vbetest utility program (it is unnecessary)
           (delete-file "libs/lrmi-0.10/vbe.h")
           (delete-file "libs/lrmi-0.10/vbetest.c")
           #t))))

    ;; We keep the bundled copy of the Linux Real Mode Interface lrmi-0.10,
    ;; because it includes fixes missing from upstream lrmi.  We do not use
    ;; libx86, because we already use x86emu with the more current lrmi.

    (inputs `(("xorg-server-sources" ,(package-source xorg-server)) ;for x86emu
              ("xorgproto" ,xorgproto))) ;upstream x86emu uses X11/Xfuncproto.h
    (outputs '("out" ;main v86d helper
               "testvbe")) ;test program for listing video modes
    (supported-systems '("i686-linux" "x86_64-linux"))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;there are no tests
       #:modules ((guix build utils)
                  (guix build gnu-build-system)
                  (ice-9 popen))
       #:phases
       (modify-phases %standard-phases
         ;; Replace the bundled x86emu with its upstream copy from Xorg-server:
         (add-after 'unpack 'unpack-x86emu-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (begin
               (format #t "decompressing x86emu source code~%")
               (with-directory-excursion "libs"
                 (let ((srcs (assoc-ref inputs "xorg-server-sources"))
                       (tar-binary (search-input-file inputs "/bin/tar")))
                   (invoke tar-binary "xvf" srcs "--strip-components=3"
                           "--wildcards" "*/hw/xfree86/x86emu/")
                   ;; extract license:
                   (with-directory-excursion "x86emu"
                     (invoke tar-binary "xvf" srcs "--strip-components=1"
                             "--wildcards" "*/COPYING"))
                   #t)))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (setenv "CC" (which "gcc"))
               (setenv "DESTDIR" out)
               (invoke "./configure" "--with-x86emu"))))
         (add-after 'build 'build-testvbe
           (lambda _
             (invoke "make" "testvbe")))
         (add-after 'install 'install-testvbe
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((testvbe (assoc-ref outputs "testvbe"))
                   (olddest (getenv "DESTDIR")))
               (setenv "DESTDIR" testvbe)
               (invoke "make" "install_testvbe")
               (setenv "DESTDIR" olddest)
               #t)))
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc-dir (string-append out "/share/doc/v86d")))
               (mkdir-p doc-dir)
               (copy-file "README"
                          (string-append doc-dir "/README"))
               (copy-file "libs/lrmi-0.10/README"
                          (string-append doc-dir "/README.lrmi"))
               (copy-file "libs/x86emu/COPYING"
                          (string-append doc-dir "/COPYING.xorg-server.x86emu"))
               #t))))))
    (home-page "https://github.com/mjanusz/v86d")
    (synopsis "Userspace helper for uvesafb")
    (description
     "v86d provides a backend for kernel drivers that need to execute x86 BIOS
code.  The code is executed in a controlled environment and the results are
passed back to the kernel via the netlink interface.  v86d is required by the
uvesafb Linux kernel module that provides an fbdev framebuffer when Kernel
Mode Setting is unavailable.  It can be a last resort when no other Xorg X
server driver works.")
    (license (list license:gpl2
                   license:x11)))) ;for bundled lrmi and x86emu

(define-public xf86-video-vmware
  (package
    (name "xf86-video-vmware")
    (version "13.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-vmware-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "06mq7spifsrpbwq9b8kn2cn61xq6mpkq6lvh4qi6xk2yxpjixlxf"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11
           libxext
           llvm
           mesa ; for xatracker
           xorg-server))
    (native-inputs
     (list eudev pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "VMware SVGA video driver for X server")
    (description
     "xf86-video-vmware is a VMware SVGA video driver for the Xorg X server.")
    ;; This package only makes sense on some architectures.
    (supported-systems (list "x86_64-linux" "i686-linux"))
    (license license:x11)))

(define-public xf86-video-voodoo
  (package
    (name "xf86-video-voodoo")
    (version "1.2.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/driver/xf86-video-voodoo-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "00pn5826aazsdipf7ny03s1lypzid31fmswl8y2hrgf07bq76ab2"))))
    (build-system gnu-build-system)
    (inputs (list xorgproto xorg-server))
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Voodoo/Voodoo2 video driver for X server")
    (description
     "xf86-video-voodoo is a Voodoo video driver for the Xorg X server.")
    (license license:x11)))

;; Only relevant for the frame buffer on BSD systems.
;; (define-public xf86-video-wsfb

(define-public xf86bigfontproto
  (package
    (name "xf86bigfontproto")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86bigfontproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0j0n7sj5xfjpmmgx6n5x556rw21hdd18fwmavp95wps7qki214ms"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86BigFontProto protocol headers")
    (description
     "XFree86 Bigfont Extension contains header files and documentation
for the XF86BigFontProto protocol.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xf86dgaproto
  (package
    (name "xf86dgaproto")
    (version "2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86dgaproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0l4hx48207mx0hp09026r6gy9nl3asbq0c75hri19wp1118zcpmc"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86DGAProto protocol headers")
    (description
     "XFree86 Direct Graphic Access Extension defines a protocol for
direct linear framebuffer access.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xf86driproto
  (package
    (name "xf86driproto")
    (version "2.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86driproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "07v69m0g2dfzb653jni4x656jlr7l84c1k39j8qc8vfb45r8sjww"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86DRIProto protocol headers")
    (description
     "XFree86 Direct Rendering Infrastructure Extension defines a
protocol to allow user applications to access the video hardware without
requiring data to be passed through the X server.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xf86vidmodeproto
  (package
    (name "xf86vidmodeproto")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xf86vidmodeproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0w47d7gfa8zizh2bshdr2rffvbr4jqjv019mdgyh6cmplyd4kna5"))))
    (build-system gnu-build-system)
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86VidModeProto protocol headers")
    (description
     "XFree86 Video Mode Extension defines a protocol for dynamically
configuring modelines and gamma.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xgamma
  (package
    (name "xgamma")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xgamma-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "13xw2fqp9cs7xj3nqi8khqxv81rk0dd8khp59xgs2lw9bbldly8w"))))
    (build-system gnu-build-system)
    (inputs
      (list libxxf86vm libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Alter a monitor's gamma correction")
    (description
     "XGamma is used to query and alter the gamma correction of a
monitor via the X video mode extension.")
    (license license:x11)))


(define-public xhost
  (package
    (name "xhost")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xhost-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0ib66h78ykc4zki4arh8hkcsgk1mk8yyy0ay5sdb2d908qqvb1pa"))))
    (build-system gnu-build-system)
    (inputs
      (list libxmu libxau libx11))
    (native-inputs
      (list gettext-minimal pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xhost")
    (synopsis "Xorg server access control utility")
    (description
     "XHost is used to manage the list of host names or user names
allowed to make connections to the X server.")
    (license license:x11)))

(define-public xineramaproto
  (package
    (name "xineramaproto")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xineramaproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ns8abd27x7gbp4r44z3wc5k9zqxxj8zjnazqpcyr4n17nxp8xcp"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XineramaProto protocol headers")
    (description
     "Xinerama Extension allows clients to query information about multiple
physical screens controlled by a single X server that appear as a single
screen to core X11 protocol operations.

This extension provides a common network protocol for querying information
which may be provided by different underlying screen combination
technologies in the X server, such as the original Xinerama multiplexer, or
alternative implementations like XRandR or TwinView.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public xinput
  (package
    (name "xinput")
    (version "1.6.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xinput-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1j2pf28c54apr56v1fmvprp657n6x4sdrv8f24rx3138cl6x015d"))))
    (build-system gnu-build-system)
    (inputs
      (list libxrender
            libxrandr
            libxinerama
            libxext
            libxi
            libx11
            xorgproto))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Configure input devices for X server")
    (description
     "XInput is used to configure and test XInput devices.")
    (license license:x11)))

(define-public xkbcomp-intermediate        ;used as input for xkeyboard-config
  (package
    (name "xkbcomp-intermediate")
    (version "1.4.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xkbcomp-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0xqzz209m9i43jbyrf2lh4xdbyhzzzn9mis2f2c32kplwla82a0a"))))
    (build-system gnu-build-system)
    (inputs
     (list xorgproto libxkbfile libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Compile XKB keyboard description")
    (description
     "xkbcomp keymap compiler converts a description of an XKB keymap
into one of several output formats.  The most common use for xkbcomp is
to create a compiled keymap file (.xkm extension) which can be read
directly by XKB- capable X servers or utilities.

X Keyboard (XKB) Extension essentially replaces the core protocol
definition of keyboard.  The extension makes possible to clearly and
explicitly specify most aspects of keyboard behaviour on per-key basis
and to more closely track the logical and physical state of the
keyboard.  It also includes a number of keyboard controls designed to
make keyboards more accessible to people with physical impairments.")
    (license license:x11)

    ;; The only reason this package is public is to make sure it's built and
    ;; published by the continuous integration tool.
    (properties '((hidden? . #t)))))

(define-public xkbcomp ; using xkeyboard-config as input
  (package (inherit xkbcomp-intermediate)
    (name "xkbcomp")
    (properties '())
    (inputs
      `(,@(package-inputs xkbcomp-intermediate)
        ("xkeyboard-config" ,xkeyboard-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-config-root="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb"))))))

(define-public xkbevd
  (package
    (name "xkbevd")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkbevd-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0gh73dsf4ic683k9zn2nj9bpff6dmv3gzcb3zx186mpq9kw03d6r"))))
    (build-system gnu-build-system)
    (inputs
      (list libxkbfile libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "XKB event daemon demo")
    (description
     "XKB event daemon listens for the specified XKB events and executes
requested commands if they occur.")
    (license license:x11)))

(define-public xkbprint
  (package
    (name "xkbprint")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append  "mirror://xorg/individual/app/"
                             "xkbprint-" version ".tar.xz"))
        (sha256
          (base32 "1k2rm8lvc2klcdz2s3mymb9a2ahgwqwkgg67v3phv7ij6304jkqw"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 libxkbfile xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Visualise an XKB keyboard layout description")
    (description
     "The @command{xkbprint} utility visualises (``prints'') an XKB keyboard
description as printable or encapsulated PostScript.  It accepts any compiled
keymap (@file{.xkm}) file that includes a geometry description, or can obtain
one from a running X server.")
    (license license:x11)))

(define-public xkbutils
  (package
    (name "xkbutils")
    (version "1.0.6")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xkbutils-"
                           version ".tar.xz"))
       (sha256
        (base32 "0pp2bsksblvvw0fx667k2bl5sm0baj7pp2cjvq0vmk093vpbp8ii"))))
    (build-system gnu-build-system)
    (inputs
      (list libxt libxaw xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "XKB utilities")
    (description
     "XKBUtils is a collection of small utilities for X Keyboard (XKB)
extension to the X11 protocol.  It includes:

- xkbbell: generate XKB bell events;

- xkbvleds: display the state of LEDs on an XKB keyboard in a window;

- xkbwatch: reports changes in the XKB keyboard state.")
    (license license:x11)))

(define-public xkeyboard-config
  (package
    (name "xkeyboard-config")
    (version "2.38")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "ftp://ftp.freedesktop.org/pub/xorg//individual/data/xkeyboard-config/"
                    "xkeyboard-config-" version ".tar.xz"))
              (sha256
               (base32
                "0wn8asnbz111194ksi2mysa6ikn4kqgd9rpfydl8icc6mcdsk406"))))
    (build-system meson-build-system)
    (inputs (list libx11 xkbcomp-intermediate))
    (native-inputs (list gettext-minimal libxslt perl pkg-config python))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XKB configuration files")
    (description
     "xkeyboard-config provides a database for X Keyboard (XKB) Extension.
There are five components that define a complete keyboard mapping:
symbols, geometry, keycodes, compat, and types; these five components
can be combined together using the @code{rules} component of this database.")
    (license license:x11)))

(define-public xkill
  (package
    (name "xkill")
    (version "1.0.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xkill-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "01xrmqw498hqlhn6l1sq89s31k6sjf6xlij6a08pnrvmqiwama75"))))
    (build-system gnu-build-system)
    (inputs
      (list libxmu libx11))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Kill a client by its X resource")
    (description
     "XKill is used to force the X server to close connections to
clients.  This program is very dangerous, but is useful for aborting
programs that have displayed undesired windows on a user's screen.")
    (license license:x11)))

(define-public xlsatoms
  (package
    (name "xlsatoms")
    (version "1.1.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xlsatoms-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1dviriynilkw0jwl0s2h8y95pwh8cxj95cnmllkd6rn0args3gzl"))))
    (build-system gnu-build-system)
    (inputs
      (list libxcb))
    (native-inputs
      (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xlsatoms")
    (synopsis "List interned X server atoms")
    (description
     "XLsAtoms is used to list the interned atoms defined on X server.")
    (license license:x11)))

(define-public xlsclients
  (package
    (name "xlsclients")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xlsclients-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1qxsav5gicsfwv1dqlcfpj47vy9i30i7iysrfx5aql02wxbyxfk8"))))
    (build-system gnu-build-system)
    (inputs
      (list libxcb))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "List client applications running on a display")
    (description
     "XLsClients is used to list information about the client programs
running on X server.")
    (license license:x11)))

(define-public xlsfonts
  (package
    (name "xlsfonts")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/app/xlsfonts-"
             version
             ".tar.xz"))
       (sha256
        (base32
         "1kdnzyrm1wqwylghavn9lqi0h4lwf9ifkcv3zikbi176mjg90zw0"))))
    (build-system gnu-build-system)
    (inputs
     (list xorgproto libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "List fonts available from an X server")
    (description
     "xlsfonts lists fonts available from an X server via the X11 core
protocol.")
    (license license:x11)))

(define-public xfontsel
  (package
    (name "xfontsel")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/app/xfontsel-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1j827aiv7lsr2y7jgmv6pb5wmr2l3r3nd2pys0z2a0bpi9jqcjvs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     (list libx11 libxaw libxmu libxt))
    (native-inputs
     (list gettext-minimal pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Browse and select X font names")
    (description
     "XFontSel provides a simple way to display the X11 core protocol fonts
known to your X server, examine samples of each, and retrieve the X Logical
Font Description (XLFD) full name for a font.")
    (license license:x11)))

(define-public xfd
  (package
    (name "xfd")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/app/xfd-"
                    version ".tar.xz"))
              (sha256
               (base32
                "1zbnj0z28dx2rm2h7pjwcz7z1jnl28gz0v9xn3hs2igxcvxhyiym"))))
    (build-system gnu-build-system)
    (arguments
       `(#:configure-flags
         (list (string-append "--with-appdefaultdir="
                              %output ,%app-defaults-dir))))
    (inputs
     `(("fontconfig" ,fontconfig)
       ("libx11" ,libx11)
       ("libxaw" ,libxaw)
       ("libxft" ,libxft)
       ("libxmu" ,libxmu)
       ("libxrender" ,libxrender)
       ("libxkbfile" ,libxkbfile)))
    (native-inputs
     `(("gettext" ,gettext-minimal)
       ("pkg-config" ,pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display all the characters in an X font")
    (description
     "XFD (X Font Display) package provides an utility that displays a
window containing the name of the font being displayed, a row of command
buttons, several lines of text for displaying character metrics, and a grid
containing one glyph per cell.")
    (license license:x11)))

(define-public xmodmap
  (package
    (name "xmodmap")
    (version "1.0.11")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xmodmap-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "10byhzdfv1xckqc3d2v52xg1ggxn5j806x4450l3ig5hyxl82bws"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xmodmap")
    (synopsis "Modify keymaps and button mappings on X server")
    (description
     "Xmodmap is used to display and edit the keyboard modifier map and
keymap table that are used by client programs running on X server to
convert event keycodes into keysyms.  It is usually run from the user's
session startup script to configure the keyboard according to personal
tastes.")
    (license license:x11)))

;; no license
;; (define-public xorg-docs

(define-public xorg-sgml-doctools
  (package
    (name "xorg-sgml-doctools")
    (version "1.12.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/doc/xorg-sgml-doctools-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0vvdnl1x82mr2phcq9z6dg94mas56zdmbm6lmkaqjkkbf3058p8a"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg SGML documentation tools")
    (description
    "This package provides a common set of SGML entities and XML/CSS style
sheets used in building/formatting the documentation provided in other
Xorg packages.  It's typically only needed by people building from
source who want to produce formatted documentation from their builds, or
those who have installed the HTML version of the documentation, which
refers to the included common xorg.css stylesheet.")
    (license license:x11)))

(define-public xpr
  (package
    (name "xpr")
    (version "1.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xpr-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1hyf6mc2l7lzkf21d5j4z6glg9y455hlsg8lv2lz028k6gw0554b"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto libxmu libx11))
    (native-inputs
        (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Print an X window dump from xwd")
    (description
     "XPr takes as input a window dump file produced by xwd and formats
it for output on various types of printers.")
    (license license:x11)))

(define-public xprop
  (package
    (name "xprop")
    (version "1.2.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xprop-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1vk7kl10ykgqp2iw4m1q4j1zrbkxibd1rdb9cjgl6yzgnyny52fn"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11 xorgproto))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display X server properties")
    (description
     "xprop is used to display and/or set window and font properties of
an X server.")
    (license license:x11)))

(define-public xrandr
  (package
    (name "xrandr")
    (version "1.5.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xrandr-"
               version ".tar.xz"))
        (sha256
          (base32
            "0744kfafd98q2zswyzva837qgvmdpfv80ilnp7x4fhdpmmk7bpgq"))))
    (build-system gnu-build-system)
    (inputs
      (list libxrender libxrandr xorgproto libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Command line interface to X RandR extension")
    (description
     "xrandr - primitive command line interface to X11 Resize, Rotate,
and Reflect (RandR) extension.")
    (license license:x11)))

(define-public xrdb
  (package
    (name "xrdb")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://xorg/individual/app/xrdb-"
                            version ".tar.xz"))
        (sha256
          (base32 "1x1ka0zbcw66a06jvsy92bvnsj9vxbvnq1hbn1az4f0v4fmzrx9i"))))
    (build-system gnu-build-system)
    (inputs
      (list libxmu libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xrdb")
    (synopsis "X server resource database utility")
    (description
     "XRDB is used to get or set the contents of the RESOURCE_MANAGER
property on the root window of screen 0, or the SCREEN_RESOURCES
property on the root window of any or all screens, or everything
combined.  You would normally run this program from your X startup
file.")
    (license license:x11)))

(define-public xrefresh
  (package
    (name "xrefresh")
    (version "1.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xrefresh-"
                           version ".tar.xz"))
       (sha256
        (base32 "0pwb5c9g3xxs70gc35hahxq2ky8261pw9n04j01a6dffbqnmkv99"))))
    (build-system gnu-build-system)
    (inputs
      (list libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Refresh all or part of an X screen")
    (description
     "Xrefresh is a simple X program that causes all or part of your
screen to be repainted.  This is useful when system messages have messed
up your screen.")
    (license license:x11)))

(define-public xset
  (package
    (name "xset")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xset-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0bsyyx3k32k9vpb8x3ks7hlfr03nm0i14fv3cg6n4f2vcdajsscz"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto libxmu libxext libx11))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "User preference utility for X server")
    (description
     "XSet is used to set various user preference options of the display.")
    (license license:x11)))

(define-public xsetroot
  (package
    (name "xsetroot")
    (version "1.1.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xsetroot-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1l9qcv4mldj70slnmfg56nv7yh9j9ca1x795bl26whmlkrdb90b0"))))
    (build-system gnu-build-system)
    (inputs
      (list libxmu libxcursor xbitmaps))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Root window parameter setting utility for X server")
    (description
     "XSetRoot allows you to tailor the appearance of the root window on
a display running X server.")
    (license license:x11)))

(define-public xtrans
  (package
    (name "xtrans")
    (version "1.5.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/xtrans-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0v7qvys42md6nvm16xynhb3b8zz534fc60wg0hfi7ab48zivyp2w"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Network Transport layer library")
    (description
     "Xtrans is a library of code that is shared among various X packages to
handle network protocol transport in a modular fashion, allowing a single
place to add new transport types.  It is used by the X server, libX11,
libICE, the X font server, and related components.")
    (license license:x11)))

(define-public xvinfo
  (package
    (name "xvinfo")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xvinfo-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0164qpbjmxxa1rbvh6ay1iz2qnp9hl1745k9pk6195kdnbn73piy"))))
    (build-system gnu-build-system)
    (inputs
      (list libxext libxv libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xvinfo")
    (synopsis "Print out X-Video extension adaptor information")
    (description
     "XVInfo is used to print out the capabilities of any video adaptors
associated with the display that are accessible through the X-Video
extension.")
    (license license:x11)))

(define-public xwd
  (package
    (name "xwd")
    (version "1.0.9")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "mirror://xorg/individual/app/xwd-"
                            version ".tar.xz"))
        (sha256
          (base32 "0gxx3y9zlh13jgwkayxljm6i58ng8jc1xzqv2g8s7d3yjj21n4nw"))))
    (build-system gnu-build-system)
    (inputs
      (list libxt libxkbfile xorgproto))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Dump current contents of X window or screen to file")
    (description
     "Xwd is used to store window images in a specially formatted dump
file.  This file can then be read by various other X utilities for
redisplay, printing, editing, formatting, archiving, image processing,
etc.  The target window is selected by clicking the pointer in the
desired window.  The keyboard bell is rung once at the beginning of the
dump and twice when the dump is completed.")
    (license license:x11)))

(define-public xwininfo
  (package
    (name "xwininfo")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwininfo-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0gr5m4lyvkil3cl63zf0sw7bq5qgraqrnvddk6xgk3a42xy8j61m"))))
    (build-system gnu-build-system)
    (inputs
      (list libx11 xorgproto))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Window information utility for X server")
    (description
     "XWinInfo is used to print out information about windows on an X server.
Various information is displayed depending on which options are selected.")
    (license license:x11)))

(define-public xwud
  (package
    (name "xwud")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/xwud-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "07n6q1z33sjkx8lx8lbd26m8ri5gi145k3mz39kmyykdngdbwp75"))))
    (build-system gnu-build-system)
    (inputs
      (list xorgproto libx11))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display an X window dump from xwd")
    (description
     "Xwud is used to display in a window an image saved in a specially
formatted dump file, such as produced by xwd.")
    (license license:x11)))

(define-public xorg-rgb
  (package
    (name "xorg-rgb")
    (version "1.1.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/rgb-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0yykkr3py1sny3cwrrwp63wm9klcjhkqjzqnd1v62ajvdvsxf0zw"))))
    (build-system gnu-build-system)
    (inputs
     (list xorgproto))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "X color name database")
    (description
     "This package provides the X color name database.")
    (license license:x11)))

;; packages of height 1 in the propagated-inputs tree

(define-public fixesproto
  (package
    (name "fixesproto")
    (version "5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/fixesproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "1ki4wiq2iivx5g4w5ckzbjbap759kfqd72yg18m3zpbb4hqkybxs"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg FixesProto protocol headers")
    (description
     "Fixes Extension makes changes to many areas of the protocol to resolve
issues raised by application interaction with core protocol mechanisms that
cannot be adequately worked around on the client side of the wire.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

(define-public libxdamage
  (package
    (name "libxdamage")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXdamage-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "04axzdx75w0wcb4na7lfpa0ai0fddw60dmg7cigs7z32a8gkqwsj"))))
    (build-system gnu-build-system)
    (propagated-inputs
      ;; These are all in the Requires or Requires.private field of xdamage.pc
      (list libxfixes xorgproto libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Damage Extension library")
    (description "Xorg library for the XDamage extension.")
    (license license:x11)))

(define-public libxext
  (package
    (name "libxext")
    (version "1.3.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXext-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0lwpx0b7lid47pff6dagp5h63bi0b3gsy05lqpyhbr4l76i9zdgd"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))             ;man pages represent 40% of the total
    (arguments
     `(#:configure-flags
       (list "--disable-static"
             (string-append "--mandir="
                            (assoc-ref %outputs "doc")
                            "/share/man")
             ,@(malloc0-flags))))
    (propagated-inputs
      (list xorgproto))
    (inputs
      (list libxau libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Common extensions library")
    (description
     "Library for common extensions to the X11 protocol.")
    (license license:x11)))

(define-public libxinerama
  (package
    (name "libxinerama")
    (version "1.1.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXinerama-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0p08q8q1wg0sixhizl2l1i935bk6x3ckj3bdd6qqr0n1zkqd352h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static" ,@(malloc0-flags))))
    (propagated-inputs
      (list xorgproto))
    (inputs
      (list libxext libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xinerama protocol library")
    (description "API for Xinerama extension to X11 protocol.")
    (license license:x11)))

(define-public libxp
  (package
    (name "libxp")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://xorg/individual/lib/libXp-" version ".tar.xz"))
       (sha256
        (base32 "197iklxwyd4naryc6mzv0g5qi1dy1apxk9w9k3yshd1ax2wf668z"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list printproto))
    (inputs
      (list libx11 libxext))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Print Client library")
    (description "Xorg Print Client library.")
    (license license:x11)))

(define-public libxrender
  (package
    (name "libxrender")
    (version "0.9.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXrender-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "15qv3lbxyx61x55lwmafpy8idb836is82i1213bchfcblj6i4cmq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-static" ,@(malloc0-flags))
       ,@(if (and (%current-target-system)
                  (target-riscv64?))
           `(#:phases
             (modify-phases %standard-phases
               (add-after 'unpack 'update-config
                 (lambda* (#:key native-inputs #:allow-other-keys)
                   (install-file
                     (search-input-file native-inputs "/bin/config.sub") ".")
                   (install-file
                     (search-input-file native-inputs "/bin/config.guess") ".")))))
           '())))
    (propagated-inputs
      (list xorgproto))
    (inputs
      (list libx11))
    (native-inputs
     (append
       (if (and (%current-target-system)
                (target-riscv64?))
         (list config)
         '())
       (list pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Render Extension library")
    (description "Library for the Render Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxtst
  (package
    (name "libxtst")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXtst-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0hljblisw72fk60y7zf9214ydn7lk32kj43cf12af2bhp4jlq3dm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")
       ,@(if (and (target-riscv64?)
                  (%current-target-system))
             `(#:phases
               (modify-phases %standard-phases
                 (add-after 'unpack 'update-config
                   (lambda* (#:key native-inputs inputs #:allow-other-keys)
                     (for-each (lambda (file)
                                 (install-file
                                  (search-input-file
                                   (or native-inputs inputs)
                                   (string-append "/bin/" file)) "."))
                               '("config.guess" "config.sub"))))))
             '())))
    (propagated-inputs
     (list libxi xorgproto))
    (inputs
     (list libx11))
    (native-inputs
     (append (if (and (target-riscv64?)
                      (%current-target-system))
               (list config)
               '())
             (list pkg-config)))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg library for Xtest and Record extensions")
    (description
     "libXtst provides the Xlib-based client API for the XTEST & RECORD
extensions.

The XTEST extension is a minimal set of client and server extensions
required to completely test the X11 server with no user intervention.  This
extension is not intended to support general journaling and playback of user
actions.

The RECORD extension supports the recording and reporting of all core X
protocol and arbitrary X extension protocol.")
    (license license:x11)))

(define-public libxv
  (package
    (name "libxv")
    (version "1.0.13")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXv-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0m9pl0xh0bv9y1x46d8a52bj46fsnyhzwa6qjg8zihg1b04r2d3x"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '(,@(malloc0-flags) "--disable-static")))
    (propagated-inputs
     (list xorgproto))
    (inputs
      (list libxext libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XVideo Extension library")
    (description "Library for the X Video Extension to the X11 protocol.")
    (license license:x11)))

(define-public mkfontdir
  (package
    (name "mkfontdir")
    (version "1.0.7")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/app/mkfontdir-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0c3563kw9fg15dpgx4dwvl12qz6sdqdns1pxa574hc7i5m42mman"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-configure
           (lambda* (#:key inputs native-inputs #:allow-other-keys)
             ;; Replace outdated config.sub and config.guess:
             (for-each (lambda (file)
                         (install-file (string-append
                                        (assoc-ref
                                         (or native-inputs inputs) "automake")
                                        "/share/automake-"
                                        ,(version-major+minor
                                          (package-version automake))
                                        "/" file) "."))
                       '("config.sub" "config.guess"))
             #t))
         (add-after 'install 'wrap-mkfontdir
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/mkfontdir")
               `("PATH" ":" prefix
                 (,(dirname
                    (search-input-file inputs "/bin/mkfontscale"))))))))))
    (inputs
      (list bash-minimal mkfontscale))
    (native-inputs
     (list pkg-config automake)) ;For up to date 'config.guess' and 'config.sub'.
    (home-page "https://www.x.org/wiki/")
    (synopsis "Create an index of X font files in a directory")
    (description
     "MkFontDir creates the @code{fonts.dir} files needed by the legacy X
server core font system.  The current implementation is a simple wrapper
script around the mkfontscale program.")
    (license license:x11)))

(define-public xproto
  (package
    (name "xproto")
    (version "7.0.31")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/proto/xproto-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0ivpxz0rx2a7nahkpkhfgymz7j0pwzaqvyqpdgw9afmxl1yp9yf6"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list util-macros)) ; to get util-macros in (almost?) all package inputs
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg X11Proto protocol headers")
    (description
     "XProto provides the headers and specification documents defining
the X Window System Core Protocol, Version 11.

It also includes a number of headers that aren't purely protocol related,
but are depended upon by many other X Window System packages to provide
common definitions and porting layer.")
    (license license:x11)
    (properties `((superseded . ,xorgproto)))))

;; packages of height 2 in the propagated-inputs tree

(define-public libice
  (package
    (name "libice")
    (version "1.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libICE-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "09c656nqkz3dpik012d2cwmd5a2dkxqgjpcq2v3v6pi22ka4wklp"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
      (list xorgproto))
    (inputs (list xtrans))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Inter-Client Exchange library")
    (description "Xorg Inter-Client Exchange library.")
    (license license:x11)))

(define-public libxau
  (package
    (name "libxau")
    (version "1.0.12")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXau-"
               version ".tar.xz"))
        (sha256
          (base32
            "1yy0gx3psxyjcj284xhh44labav7b5zs7gcrks9xi6nklggy9l3l"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
      (list xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Authorization library")
    (description
     "libXau provides an authorization library for individual access to
an X Window System display.")
    (license license:x11)))

(define-public libxfixes
  (package
    (name "libxfixes")
    (version "6.0.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXfixes-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0n1dq2mi60i0c06i7j6lq64cq335ir2l89yj0amj3529s8ygk5dn"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
      (list xorgproto))
    (inputs
      (list libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Fixes Extension library")
    (description "Library for the XFixes Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxfont
  (package
    (name "libxfont")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXfont-"
               version
               ".tar.bz2"))
        (sha256
          (base32
            "0hiji1bvpl78aj3a3141hkk353aich71wv8l5l2z51scfy878zqs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
      (list freetype libfontenc xorgproto))
    (inputs
      (list zlib xtrans))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Font handling library")
    (description
     "libXfont provides the core of the legacy X11 font system, handling the
index files (fonts.dir, fonts.alias, fonts.scale), the various font file
formats, and rasterizing them.  It is used by the X servers, the X Font
Server (xfs), and some font utilities (bdftopcf for instance), but should
not be used by normal X11 clients.  X11 clients access fonts via either the
new API's in libXft, or the legacy API's in libX11.")
    (license license:x11)))

(define-public libxfont2
  (package
    (inherit libxfont)
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/lib/libXfont2-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "1ywfwpirvi7fmd3my3cb6szzd9c4p7xy7s1kjjv6k1x4xgyq4ywb"))))))

(define-public libxi
  (package
    (name "libxi")
    (version "1.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXi-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0161qsac0dgvkkcihpm2062p1lk2l5mj4i7smd713qnnadg5bq6h"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))             ;man pages represent 28% of the total
    (arguments
     `(#:configure-flags (list "--disable-static"
                               (string-append "--mandir="
                                              (assoc-ref %outputs "doc")
                                              "/share/man")
                               ,@(malloc0-flags))))
    (propagated-inputs
      (list xorgproto libx11 libxext libxfixes))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Input Extension library")
    (description "Library for the XInput Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxrandr
  (package
    (name "libxrandr")
    (version "1.5.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXrandr-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1lxlqd9ffjr1myfpyk91594n1h07ck6121m6ba8qajjz6xjv1m8s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static" ,@(malloc0-flags))))
    (propagated-inputs
      ;; In accordance with xrandr.pc.
      (list libx11 libxext libxrender xorgproto))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Resize and Rotate Extension library")
    (description
     "Library for the Resize and Rotate Extension to the X11 protocol.")
    (license license:x11)))

(define-public libxvmc
  (package
    (name "libxvmc")
    (version "1.0.14")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/lib/libXvMC-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1nayf8qck0b1xb88dirdbvj7clr18wq1dxs73zwbpzdsnsv9xgp4"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '(,@(malloc0-flags) "--disable-static")))
    (propagated-inputs
      (list libxv))
    (inputs
      (list xorgproto libxext libx11))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XvMC library")
    (description "Xorg XvMC library.")
    (license license:x11)))

(define-public libxxf86vm
  (package
    (name "libxxf86vm")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXxf86vm-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "1qryzfzf3qr2xx1sipdn8kw310zs4ygpzgh4mm4m87fffd643bwn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static" ,@(malloc0-flags))))
    (propagated-inputs
      (list libxext xorgproto))
    (inputs
      (list libx11))
    (native-inputs
       (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XF86 Video Mode Extension library")
    (description
     "Library for the XFree86 Video Mode Extension Extension to the X11
protocol.")
    (license license:x11)))

;; packages of height 3 in the propagated-inputs tree

(define-public libxcb
  (package
    (name "libxcb")
    (version "1.17.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://xcb.freedesktop.org/dist/"
                            "libxcb-" version ".tar.xz"))
        (sha256
          (base32
           "0mbdkajqhg0j0zjc9a2z1qyv9mca797ihvifc9qyl3vijscvz7jr"))))
    (build-system gnu-build-system)
    (outputs '("out" "doc"))                      ;5.5 MiB of man pages
    (propagated-inputs
      (append (list libxau libxdmcp)
              (if (target-hurd?)
                  (list libpthread-stubs)         ;still checked on Hurd
                  '())))
    (inputs
      (list xcb-proto
            check))                               ;for tests
    (native-inputs
     (list pkg-config python-minimal-wrapper))
    (arguments
     `(#:configure-flags (list "--enable-xkb"
                               "--disable-static"
                               (string-append "--mandir="
                                              (assoc-ref %outputs "doc")
                                              "/share/man"))
       #:phases ,(if (target-hurd?)
                     '(modify-phases %standard-phases
                        (add-after 'unpack 'fix-PATH_MAX
                          (lambda _
                            ;; Hurd doesn't define PATH_MAX.
                            (substitute* "src/xcb_util.c"
                              (("PATH_MAX") "4096")))))
                     '%standard-phases)))
    (home-page "https://xcb.freedesktop.org/")
    (synopsis "The X C Binding (XCB) library")
    (description
     "libxcb provides an interface to the X Window System protocol,
which replaces the current Xlib interface.  It has several advantages
over Xlib, including:

- size: small, simple library, and lower memory footprint;

- latency hiding: batch several requests and wait for the replies later;

- direct protocol access: interface and protocol correspond exactly;

- proven thread support: transparently access XCB from multiple threads;

- easy extension implementation: interfaces auto-generated from XML-XCB.")
    (license license:x11)))

(define-public libxcvt
  (package
    (name "libxcvt")
    (version "0.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.x.org/releases/individual"
                                  "/lib/libxcvt-" version ".tar.xz"))
              (sha256
               (base32
                "009f8kr53cv7lzsg4507cgnk9vxyrm8lgnnn6vx7vpk7hy59jad9"))))
    (build-system meson-build-system)
    (home-page "https://gitlab.freedesktop.org/xorg/lib/libxcvt")
    (synopsis "VESA Coordinated Video Timings (CVT) library")
    (description "@code{libxcvt} is a library providing a standalone version
of the X server implementation of the VESA Coordinated Video Timings (CVT)
standard timing modelines generator.  @code{libxcvt} also provides a
standalone version of the command line tool @command{cvt} copied from the Xorg
implementation and is meant to be a direct replacement to the version provided
by the Xorg server.")
    (license license:x11)))

(define-public xorg-server
  (package
    (name "xorg-server")
    (version "21.1.15")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://xorg.freedesktop.org/archive/individual"
                           "/xserver/xorg-server-" version ".tar.xz"))
       (sha256
        (base32
         "12g0g9ksswzx1kgn23gvrpa570fnpkdkmw1dfqjjg4422a884744"))
       (patches
        (list
         ;; See:
         ;;   https://lists.fedoraproject.org/archives/list/devel@lists.
         ;;      fedoraproject.org/message/JU655YB7AM4OOEQ4MOMCRHJTYJ76VFOK/
         (origin
           (method url-fetch)
           (uri (string-append
                 "http://pkgs.fedoraproject.org/cgit/rpms/xorg-x11-server.git"
                 "/plain/06_use-intel-only-on-pre-gen4.diff"))
           (sha256
            (base32
             "0mm70y058r8s9y9jiv7q2myv0ycnaw3iqzm7d274410s0ik38w7q"))
           (file-name "xorg-server-use-intel-only-on-pre-gen4.diff"))))))
    (build-system gnu-build-system)
    (propagated-inputs
     ;; The following libraries are required by xorg-server.pc.
     (list libpciaccess libxcvt mesa pixman xorgproto))
    (inputs
     `(("udev" ,eudev)
       ("dbus" ,dbus)
       ("libdmx" ,libdmx)
       ("libepoxy" ,libepoxy)
       ("libgcrypt" ,libgcrypt)
       ("libxau" ,libxau)
       ("libxaw" ,libxaw)
       ("libxdmcp" ,libxdmcp)
       ("libxfixes" ,libxfixes)
       ("libxfont2" ,libxfont2)
       ("libxkbfile" ,libxkbfile)
       ("libxrender" ,libxrender)
       ("libxres" ,libxres)
       ("libxshmfence" ,libxshmfence)
       ("libxt" ,libxt)
       ("libxv" ,libxv)
       ("xkbcomp" ,xkbcomp)
       ("xkeyboard-config" ,xkeyboard-config)
       ("xtrans" ,xtrans)
       ("zlib" ,zlib)
       ;; Inputs for Xephyr
       ("xcb-util" ,xcb-util)
       ("xcb-util-image" ,xcb-util-image)
       ("xcb-util-keysyms" ,xcb-util-keysyms)
       ("xcb-util-renderutil" ,xcb-util-renderutil)
       ("xcb-util-wm" ,xcb-util-wm)))
    (native-inputs
     `(("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:configure-flags
       (list (string-append "--with-xkb-path="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             (string-append "--with-xkb-output="
                            "/tmp") ; FIXME: This is a bit doubtful; where should
                                        ; the compiled keyboard maps go?
             (string-append "--with-xkb-bin-directory="
                            (assoc-ref %build-inputs "xkbcomp")
                            "/bin")
             ;; By default, it ends up with invalid '${prefix}/...', causes:
             ;;   _FontTransOpen: Unable to Parse address ${prefix}/share/...
             ;; It's not used anyway, so set it to empty.
             "--with-default-font-path="

             ;; Enable the X security extensions (ssh -X).
             "--enable-xcsecurity"

             ;; The default is to use "uname -srm", which captures the kernel
             ;; version and makes builds non-reproducible.
             "--with-os-name=GNU"
             "--with-os-vendor=Guix"    ; not strictly needed, but looks nice

             ;; For the log file, etc.
             "--localstatedir=/var"
             ;; For sddm.
             "--enable-kdrive"
             "--enable-xephyr")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'pre-configure
           (lambda _
             (substitute* (find-files "." "\\.c$")
               (("/bin/sh") (which "sh")))

             ;; Don't try to 'mkdir /var'.
             (substitute* "hw/xfree86/Makefile.in"
               (("\\$\\(MKDIR_P\\).*logdir.*")
                "true\n"))

             ;; Strip timestamps that would otherwise end up in the 'Xorg'
             ;; binary.
             (substitute* "configure"
               (("^BUILD_DATE=.*$")
                "BUILD_DATE=19700101\n")
               (("^BUILD_TIME=.*$")
                "BUILD_TIME=000001\n")))))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg implementation of the X Window System")
    (description
     "This package provides the Xorg X server itself.
The X server accepts requests from client programs to create windows, which
are (normally rectangular) 'virtual screens' that the client program can
draw into.

Windows are then composed on the actual screen by the X server (or by a
separate composite manager) as directed by the window manager, which usually
communicates with the user via graphical controls such as buttons and
draggable titlebars and borders.")
    (license license:x11)))

;; This package is intended to be used when building GTK+.
;; Note: It's currently marked as "hidden" to avoid having two non-eq?
;; packages with the same name and version.
(define-public xorg-server-for-tests
  (hidden-package
   (package
     (inherit xorg-server)
     (version "21.1.15")
     (source
      (origin
        (method url-fetch)
        (uri (string-append "https://xorg.freedesktop.org/archive/individual"
                            "/xserver/xorg-server-" version ".tar.xz"))
        (sha256
         (base32
          "12g0g9ksswzx1kgn23gvrpa570fnpkdkmw1dfqjjg4422a884744"))
        (patches
         (list
          ;; See:
          ;;   https://lists.fedoraproject.org/archives/list/devel@lists.
          ;;      fedoraproject.org/message/JU655YB7AM4OOEQ4MOMCRHJTYJ76VFOK/
          (origin
            (method url-fetch)
            (uri (string-append
                  "http://pkgs.fedoraproject.org/cgit/rpms/xorg-x11-server.git"
                  "/plain/06_use-intel-only-on-pre-gen4.diff"))
            (sha256
             (base32
              "0mm70y058r8s9y9jiv7q2myv0ycnaw3iqzm7d274410s0ik38w7q"))
            (file-name "xorg-server-use-intel-only-on-pre-gen4.diff")))))))))

(define-public eglexternalplatform
  (package
    (name "eglexternalplatform")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/NVIDIA/eglexternalplatform")
         (commit version)))
       (file-name
        (git-file-name name version))
       (sha256
        (base32 "1cq8j2ymjpxpdcwnmcj0h5fgi3i1l8hns3vgw10rigwljrmn8ixp"))))
    (build-system meson-build-system)
    (synopsis "EGL External Platform interface")
    (description "EGLExternalPlatform is an specification of the EGL External
Platform interface for writing EGL platforms and their interactions with modern
window systems on top of existing low-level EGL platform implementations.  This
keeps window system implementation specifics out of EGL drivers by using
application-facing EGL functions.")
    (home-page "https://github.com/NVIDIA/eglexternalplatform")
    (license license:expat)))

(define-public egl-wayland
  (package
    (name "egl-wayland")
    (version "1.1.17")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/NVIDIA/egl-wayland")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1w0b53157ql5pp6hawqcy1c1c8lchk21gpc01p6lxgwvl3dgjn7y"))))
    (build-system meson-build-system)
    (native-inputs
     (cons* libglvnd ;needed for headers
            mesa-headers pkg-config
            (if (%current-target-system)
              (list pkg-config-for-build wayland wayland-protocols)
              '())))
    (inputs
     (list mesa wayland wayland-protocols))
    (propagated-inputs
     (list eglexternalplatform))
    (synopsis "EGLStream-based Wayland external platform")
    (description "EGL-Wayland is an implementation of a EGL External Platform
library to add client-side Wayland support to EGL on top of EGLDevice and
EGLStream families of extensions.")
    (home-page "https://github.com/NVIDIA/egl-wayland")
    (license license:expat)))

(define-public xorg-server-xwayland
  (package
    (name "xorg-server-xwayland")
    (version "24.1.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://xorg.freedesktop.org/archive/individual"
                           "/xserver/xwayland-" version ".tar.xz"))
       (sha256
        (base32
         "1x1lmw1br3dxxfppfny1vkmk2l2vk5248i3k05smb7w1mgdphsnr"))))
    (inputs (list font-dejavu
                  dbus
                  egl-wayland
                  eudev
                  libfontenc
                  libdrm
                  libepoxy
                  libgcrypt
                  libtirpc
                  libxcvt
                  libxfont2
                  libxkbfile
                  pixman
                  wayland
                  wayland-protocols
                  xkbcomp
                  xkeyboard-config
                  xorgproto
                  xtrans))
    (native-inputs (cons pkg-config
                         (if (%current-target-system)
                           (list pkg-config-for-build
                                 wayland
                                 wayland-protocols)
                           '())))
    (properties '((upstream-name . "xwayland")))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "-Dxkb_dir="
                            (assoc-ref %build-inputs "xkeyboard-config")
                            "/share/X11/xkb")
             (string-append "-Dxkb_bin_dir="
                            (assoc-ref %build-inputs "xkbcomp") "/bin")
             ;; The build system insist on providing a default font path; give
             ;; that of dejavu, the same used for our fontconfig package.
             (string-append "-Ddefault_font_path="
                            (assoc-ref %build-inputs "font-dejavu")
                            "/share/fonts")
             "-Dxkb_output_dir=/tmp"
             (format #f "-Dbuilder_string=\"Build ID: ~a ~a\"" ,name ,version)
             "-Dxcsecurity=true"
             "-Ddri3=true"
             "-Dglamor=true"
             ;; For the log file, etc.
             "--localstatedir=/var")
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'patch-/bin/sh
                    (lambda _
                      (substitute* (find-files "." "\\.c$")
                        (("/bin/sh") (which "sh"))))))))
    (synopsis "Xorg server with Wayland backend")
    (description "Xwayland is an X server for running X clients under
Wayland.")
    (home-page "https://www.x.org/wiki/")
    (license license:x11)))

;; packages of height 4 in the propagated-inputs tree

(define-public libx11
  (package
    (name "libx11")
    (version "1.8.10")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/lib/libX11-"
                           version ".tar.xz"))
       (sha256
        (base32
         "0lywvwsz92j7isglvw2227g3na4ghyspvsvblpf43ns7jfnksfrb"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                  ;8 MiB of man pages + XML
    (arguments
     `(#:configure-flags
       (list (string-append "--mandir="
                            (assoc-ref %outputs "doc")
                            "/share/man")
             "--disable-static"
             ,@(malloc0-flags))))
    (propagated-inputs
     (list xorgproto libxcb))
    (inputs
     (list xtrans))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Core X11 protocol client library")
    (description "Xorg Core X11 protocol client library.")
    (license license:x11)))

;; packages of height 5 in the propagated-inputs tree

(define-public libxcursor
  (package
    (name "libxcursor")
    (version "1.2.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/lib/libXcursor-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1h62narayrhrkqalrmx7z3s6yppw1acbp5id3skrvrygshnl1sgx"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list "--disable-static"
              ;; Set default path, used when XCURSOR_PATH is unset.
              (string-append "--with-cursorpath="
                             (string-join
                              '("~/.local/share/icons" "~/.icons"
                                "~/.guix-profile/share/icons"
                                "/run/current-system/profile/share/icons"
                                "/usr/share/icons")
                              ":")))))
    (propagated-inputs
     (list libx11 libxrender libxfixes xorgproto))
    (native-inputs
     (list pkg-config))
    ;; FIXME: The search path below won't be very effective until the bugs
    ;; <http://bugs.gnu.org/20255> and <http://bugs.gnu.org/22138> are solved.
    (native-search-paths
     (list (search-path-specification
            (variable "XCURSOR_PATH")
            (files '("share/icons")))))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Cursor management library")
    (description "Xorg Cursor management library.")
    (license license:x11)))

(define-public libxt
  (package
    (name "libxt")
    (version "1.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXt-"
               version
               ".tar.xz"))
        (sha256
          (base32
           "120jjd6l7fjdxy5myrc1dmc0cwpqa18a97hrbg0d9x146frp99z0"))
        (patches (search-patches "libxt-guix-search-paths.patch"))))
    (build-system gnu-build-system)
    (outputs '("out"
               "doc"))                            ;2 MiB of man pages + XML
    (arguments
     `(#:configure-flags
       (list (string-append "--mandir="
                            (assoc-ref %outputs "doc")
                            "/share/man")
             "--disable-static"
             ,@(malloc0-flags))))
    (propagated-inputs
      (list libx11 libice libsm))
    (inputs
      (list libx11))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg XToolkit Intrinsics library")
    (description "Xorg XToolkit Intrinsics library.")
    (license license:x11)))

(define-public libxaw
  (package
    (name "libxaw")
    (version "1.0.16")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXaw-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "13wwqfwaahm6dh35w0nkvw32x3li2s0glsks34ggh267ahmmf7bk"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
      (list libxext libxmu libxpm libxt))
    (inputs
      (list xorgproto))
    (native-inputs
      (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xaw library")
    (description
     "Xaw is the X Athena Widget Set based on the X Toolkit
Intrinsics (Xt) Library.")
    (license license:x11)))

(define-public twm
  (package
    (name "twm")
    (version "1.0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/twm-"
                           version ".tar.xz"))
       (sha256
        (base32
         "1r5gfv1gvcjn39v7n6znpnvifwhlw2zf8gfrxq8vph84vva03wma"))))
    (build-system gnu-build-system)
    (inputs
     (list libxt libxmu libxext xorgproto))
    (native-inputs
     (list bison pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Tab Window Manager for the X Window System")
    (description "Twm is a window manager for the X Window System.
It provides titlebars, shaped windows, several forms of icon management,
user-defined macro functions, click-to-type and pointer-driven
keyboard focus, and user-specified key and pointer button bindings.")
    (license license:x11)))

(define-public xcb-util
  (package
    (name "xcb-util")
    (version "0.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "04p54r0zjc44fpw1hdy4rhygv37sx2vr2lllxjihykz5v2xkpgjs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     (list libxcb))
    (native-inputs
     (list pkg-config))
    (home-page "https://cgit.freedesktop.org/xcb/util/")
    (synopsis "Core XCB utility functions")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util module provides the following libraries:

- aux: Convenient access to connection setup and some core requests.

- atom: Standard core X atom constants and atom caching.

- event: Some utilities that have little to do with events any more.")
    (license license:x11)))

(define-public xcb-util-cursor
  (package
    (name "xcb-util-cursor")
    (version "0.1.5")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://xcb.freedesktop.org/dist/"
                           "xcb-util-cursor-" version ".tar.xz"))
       (sha256
        (base32 "0mrwcrm6djbd5zdvqb5v4wr87bzawnaacyqwwhfghw09ssq9kbqc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (native-inputs
     (list m4 pkg-config))
    (inputs
     (list libxcb))
    (propagated-inputs
     (list xcb-util-renderutil xcb-util-image))
    (home-page "https://cgit.freedesktop.org/xcb/util-cursor/")
    (synopsis "Port of libxcursor")
    (description "XCB-util-cursor is a port of libxcursor.")
    (license
     ; expat license  with added clause regarding advertising
     (license:non-copyleft
      "file://COPYING"
      "See COPYING in the distribution."))))

(define-public xcb-util-errors
  (package
    (name "xcb-util-errors")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://xcb.freedesktop.org/dist/"
                           "xcb-util-errors-" version ".tar.xz"))
       (sha256
        (base32 "0mzkh3xj1n690dw8hrdhyjykd71ib0ls9n5cgf9asna2k1xwha2n"))))
    (build-system gnu-build-system)
    (outputs '("out"))
    (inputs
     (list util-macros xcb-proto))
    (propagated-inputs
     (list libxcb))
    (native-inputs
     `(("m4" ,m4)
       ("python" ,python-wrapper)
       ("pkg-config" ,pkg-config)
       ,@(if (%current-target-system)
             `(("libxcb" ,libxcb))
             `())))
    (home-page "https://cgit.freedesktop.org/xcb/util-errors/")
    (synopsis "XCB helper library for printing information about X11 errors")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-errors module provides a utility library that gives human
readable names to error codes, event codes, and also to major and minor
numbers.")
    (license license:x11)))

(define-public xcb-util-image
  (package
    (name "xcb-util-image")
    (version "0.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0g8dwknrlz96k176qxh8ar84x9kpppci9b978zyp24nvvbjqxbfc"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     (list libxcb))
    (inputs
     (list xcb-util))
    (native-inputs
     (list pkg-config))
    (home-page "https://cgit.freedesktop.org/xcb/util-image/")
    (synopsis "XCB port of Xlib's XImage and XShmImage")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-image module provides the following library:

- image: Port of Xlib's XImage and XShmImage functions.")
    (license license:x11)))

(define-public xcb-util-keysyms
  (package
    (name "xcb-util-keysyms")
    (version "0.4.1")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "0f66snk179hmp8ppgv1zp9y7pl1vzn52znpikm1fsaj1ji90l9kw"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--disable-static")))
    (propagated-inputs
     (list libxcb))
    (native-inputs
     (list pkg-config))
    (home-page "https://cgit.freedesktop.org/xcb/util-keysyms/")
    (synopsis "Standard X constants and conversion to/from keycodes")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-keysyms module provides the following library:

- keysyms: Standard X key constants and conversion to/from keycodes.")
    (license license:x11)))

(define-public xcb-util-renderutil
  (package
    (name "xcb-util-renderutil")
    (version "0.3.10")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "1fh4dnlwlqyccrhmmwlv082a7mxc7ss7vmzmp7xxp39dwbqd859y"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")
       ,@(if (and (%current-target-system)
                  (target-riscv64?))
             `(#:phases
               (modify-phases %standard-phases
                 (add-after 'unpack 'update-config-scripts
                   (lambda* (#:key inputs native-inputs #:allow-other-keys)
                     ;; Replace outdated config.guess and config.sub.
                     (for-each (lambda (file)
                                 (install-file
                                  (search-input-file
                                   (or native-inputs inputs)
                                   (string-append "/bin/" file)) "."))
                               '("config.guess" "config.sub"))))))
             '())))
    (propagated-inputs
     (list libxcb))
    (native-inputs
     (append (if (and (%current-target-system)
                      (target-riscv64?))
                 (list config)
                 '())
             (list pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util-renderutil/")
    (synopsis "Convenience functions for the Render extension")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-renderutil module provides the following library:

- renderutil: Convenience functions for the Render extension.")
    (license license:x11)))

(define-public xcb-util-wm
  (package
    (name "xcb-util-wm")
    (version "0.4.2")
    (source (origin
             (method url-fetch)
             (uri (string-append "mirror://xorg/individual/xcb/"
                                 name "-" version ".tar.xz"))
             (sha256
              (base32
               "02wai17mxfbvlnj4l4bjbvah97rccdivzvd7mrznhr32s0hlxhv2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags '("--disable-static")
       ,@(if (and (target-riscv64?)
                  (%current-target-system))
           `(#:phases
             (modify-phases %standard-phases
               (add-after 'unpack 'update-config-scripts
                 (lambda* (#:key inputs native-inputs #:allow-other-keys)
                   ;; Replace outdated config.guess and config.sub.
                   (for-each (lambda (file)
                               (install-file
                                (search-input-file
                                 (or native-inputs inputs)
                                 (string-append "/bin/" file)) "."))
                             '("config.guess" "config.sub"))))))
           '())))
    (propagated-inputs
     (list libxcb))
    (native-inputs
     (append (if (and (target-riscv64?)
                      (%current-target-system))
               (list config)
               '())
             (list m4 pkg-config)))
    (home-page "https://cgit.freedesktop.org/xcb/util-wm/")
    (synopsis "Client and window-manager helpers for ICCCM and EWMH")
    (description
     "The XCB util modules provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

The XCB util-wm module provides the following libraries:

- ewmh: Both client and window-manager helpers for EWMH.

- icccm: Both client and window-manager helpers for ICCCM.")
    (license license:x11)))

(define-public xinit
  (package
    (name "xinit")
    (version "1.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://xorg/individual/app/xinit-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0npx32mi035xkz26gp7raps5xsq4v7rgayy1q794h4d3lqhryh46"))
              (patches
                (search-patches "xinit-startx-mcookie-path.patch"))))
    (build-system gnu-build-system)
    (inputs (list libx11 util-linux xorgproto))
    (native-inputs (list pkg-config))
    (propagated-inputs (list xauth))
    (home-page "https://www.x.org/")
    (synopsis "Commands to start the X Window server")
    (description
     "The xinit program is used to start the X Window System server and a
first client program on systems that are not using a display manager such as
xdm.  This package also provides the @code{startx} command, which provides a
user-friendly mechanism to start the X server.")
    (license license:x11)))

;; package outside the x.org system proper of height 5

(define-public libxaw3d
  (package
    (name "libxaw3d")
    (version "1.6.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "mirror://xorg/individual/lib/libXaw3d-"
               version
               ".tar.xz"))
        (sha256
          (base32
            "0il6vkm357c23wa39prhkaskrgxg080fggcbyaqidmjs2cvkw754"))))
    (build-system gnu-build-system)
    (propagated-inputs
      (list libxext libxmu libxt))
    (inputs
     (list libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Xorg Xaw3d library")
    (description
     "Xaw is the X 3D Athena Widget Set based on the X Toolkit
Intrinsics (Xt) Library.")
    (license license:x11)))

(define-public libxpresent
  (package
    (name "libxpresent")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://xorg/individual/lib/libXpresent-"
                    version
                    ".tar.xz"))
              (sha256
               (base32
                "06r34v7z3jb0x7l5ghlc1g82gjjp5ilq5p6j11galv86bagdyr5r"))))
    (inputs
     (list libx11 xorgproto libxext libxfixes libxrandr))
    (native-inputs
     (list pkg-config))
    (build-system gnu-build-system)
    (home-page "https://gitlab.freedesktop.org/xorg/lib/libxpresent")
    (synopsis "Xlib-compatible API for the Present extension")
    (description "This package provides a Xlib-based library for the X Present
Extension.")
    (license license:x11)))

(define-public xclock
  (package
    (name "xclock")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.x.org/releases/individual/app/"
                           "xclock-" version ".tar.xz"))
       (sha256
        (base32 "0b3l1zwz2b1cn46f8pd480b835j9anadf929vqpll107iyzylz6z"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     (list libxmu
           libx11
           libxaw
           libxrender
           libxft
           libxkbfile))
    (native-inputs
     (list pkg-config))
    (home-page "https://gitlab.freedesktop.org/xorg/app/xclock")
    (synopsis "Analog / digital clock for X")
    (description "The xclock program displays the time in analog or digital
form.")
    (license (license:x11-style "file://COPYING" "See COPYING for details."))))

(define-public xmag
  (package
    (name "xmag")
    (version "1.0.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xmag-"
                           version ".tar.xz"))
       (sha256
        (base32 "0clm0vm35lkcir5w3bkypax9j57vyzkl9l89qqxbanvr7mc3qv9j"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--with-appdefaultdir="
                             #$output #$%app-defaults-dir))))
    (inputs
     (list libxaw))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Display or capture a magnified part of a X11 screen")
    (description "Xmag displays and captures a magnified snapshot of a portion
of an X11 screen.")
    (license license:x11)))

(define-public xmessage
  (package
    (name "xmessage")
    (version "1.0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/app/xmessage-"
                           version ".tar.xz"))
       (sha256
        (base32 "0mh3lclzh82l4wkwg1d9gflnm1irjydihg30gqfxcwmpl2vwqgvh"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-appdefaultdir="
                            %output ,%app-defaults-dir))))
    (inputs
     (list libxaw))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/wiki/")
    (synopsis "Displays a message or query in a window")
    (description
     "Xmessage displays a message or query in a window.   The user can click
on a button to dismiss it or can select one of several buttons
to answer a question.  Xmessage can also exit after a specified time.")
    (license license:x11)))

(define-public xterm
  (package
    (name "xterm")
    (version "397")
    (source
     (origin
       (method url-fetch)
       (uri (list
             (string-append "https://invisible-mirror.net/archives/xterm/"
                            "xterm-" version ".tgz")
             (string-append "ftp://ftp.invisible-island.net/xterm/"
                            "xterm-" version ".tgz")))
       (sha256
        (base32 "0ql6mvs2amjfdarpqwzi4dcnvkgn6wr52kh7b3nfqi5skhmp96rf"))
       (patches
         (search-patches "xterm-370-explicit-xcursor.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags '("--enable-wide-chars" "--enable-load-vt-fonts"
                           "--enable-i18n" "--enable-doublechars"
                           "--enable-luit" "--enable-mini-luit"
                           "X_EXTRA_LIBS=-lXcursor")
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'patch-file-names
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "uxterm"
                 (("([ `\\|])(sh|sed|awk|xmessage) " _ prefix command)
                  (string-append prefix (which command) " "))
                 (("(`|\"|LANG=C )(locale) " _ prefix command)
                  (string-append prefix (which command) " "))
                 (("=xterm")
                  (string-append "=" out "/bin/xterm")))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("luit" ,luit)
       ("libXft" ,libxft)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("ncurses" ,ncurses)
       ("libICE" ,libice)
       ("libSM" ,libsm)
       ("libX11" ,libx11)
       ("libXcursor" ,libxcursor)
       ("libXext" ,libxext)
       ("libXt" ,libxt)
       ("xorgproto" ,xorgproto)
       ("libXaw" ,libxaw)))
    (home-page "https://invisible-island.net/xterm/")
    (synopsis "Terminal emulator for the X Window System")
    (description
     "The xterm program is a terminal emulator for the X Window System.  It
provides DEC VT102/VT220 (VTxxx) and Tektronix 4014 compatible terminals for
programs that cannot use the window system directly.")
    (license license:x11)))

(define-public perl-x11-xcb
  (package
    (name "perl-x11-xcb")
    (version "0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://cpan/authors/id/M/MS/MSTPLBG/"
                    "X11-XCB-" version ".tar.gz"))
              (sha256
               (base32
                "1rn8g0yy82v5zp12rhxic332dvqs63l7mykg028ngvccs7rllipp"))))
    (build-system perl-build-system)
    (arguments
     '(;; Disable parallel build to prevent a race condition.
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-perl-search-path
           (lambda _
             (setenv "PERL5LIB"
                     (string-append (getcwd) ":"
                                    (getenv "PERL5LIB")))))
         (add-before 'build 'patch-Makefile
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile"
               ;; XXX: Without this hack, attempts at using XCB.so fails with
               ;; an error such as "XCB.so: undefined symbol: xcb_xinerama_id"
               (("^LDDLFLAGS = ")
                (string-append "LDDLFLAGS = "
                               "-lxcb -lxcb-util -lxcb-xinerama -lxcb-icccm "))))))
       ;; Tests require a running X11 server.
       #:tests? #f))
    (native-inputs
     (list perl-extutils-depends perl-extutils-pkgconfig
           perl-module-install perl-test-deep perl-test-exception))
    (propagated-inputs
     (list perl-data-dump
           perl-mouse
           perl-mousex-nativetraits
           perl-try-tiny
           perl-xml-descent
           perl-xml-simple
           perl-xs-object-magic))
    (inputs
     (list libxcb xcb-proto xcb-util xcb-util-wm))
    (home-page "https://metacpan.org/release/X11-XCB")
    (synopsis "Perl bindings for libxcb")
    (description
     "These bindings wrap @code{libxcb} (a C library to speak with X11,
in many cases better than @code{Xlib}), and provides an object oriented
interface to its methods (using @code{Mouse}).")
    (license license:perl-license)))

(define-public perl-x11-protocol
  (package
    (name "perl-x11-protocol")
    (version "0.56")
    (source (origin
             (method url-fetch)
             (uri (string-append
                   "mirror://cpan/authors/id/S/SM/SMCCAM/X11-Protocol-"
                   version ".tar.gz"))
             (sha256
              (base32
               "1dq89bh6fqv7l5mbffqcismcljpq5f869bx7g8lg698zgindv5ny"))))
    (build-system perl-build-system)
    (arguments '(#:tests? #f))          ;tests require a running x server
    (synopsis "Raw interface to X Window System servers")
    (description
     "X11::Protocol is a client-side interface to the X11 Protocol, allowing
perl programs to display windows and graphics on X11 servers.")
    (home-page "https://metacpan.org/release/X11-Protocol")
    ;; From the package README: "you can redistribute and/or modify it under
    ;; the same terms as Perl itself. (As an exception, the file
    ;; Keysyms.pm,which is derived from a file in the standard X11
    ;; distribution, has another, less restrictive copying policy, as do some
    ;; of the extension modules in the directory Protocol/Ext: see those files
    ;; for details)."
    (license license:perl-license)))

(define-public perl-x11-protocol-other
  (package
    (name "perl-x11-protocol-other")
    (version "31")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "mirror://cpan/authors/id/K/KR/KRYDE/X11-Protocol-Other-"
             version ".tar.gz"))
       (sha256
        (base32 "1x3kvic52jgp2mvd5wzrqrprqi82cdk8l4075v8b33ksvj9mjqiw"))))
    (build-system perl-build-system)
    (native-inputs
     (list perl-encode-hanextra perl-module-util))
    (propagated-inputs
     (list perl-x11-protocol))
    (home-page "https://metacpan.org/release/X11-Protocol-Other")
    (synopsis "Miscellaneous helpers for @code{X11::Protocol} connections")
    (description
     "@code{X11::Protocol::Other} contains window manager related functions for
use by client programs, as per the @dfn{ICCCM} (Inter-Client Communication
Conventions Manual) and some of the @dfn{EWMH}
(Extended Window Manager Hints).")
    (license license:gpl3+)))

(define-public xcompmgr
  (package
    (name "xcompmgr")
    (version "1.1.10")
    (source
     (origin
       ;; There's no current tarball.
       (method git-fetch)
       (uri (git-reference
             (url "https://anongit.freedesktop.org/git/xorg/app/xcompmgr.git")
             (commit (string-append "xcompmgr-" version))))
       (sha256
        (base32 "11bi5xa3wkvjl1v5xpfi6c9fhxsxx6smivwwj24zq5gidl72xjhk"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list util-macros pkg-config autoconf automake))
    (inputs
     (list libx11
           libxext
           libxcomposite
           libxfixes
           libxdamage
           libxrender))
    (synopsis "X Compositing manager using RENDER")
    (description "xcompmgr is a sample compositing manager for X servers
supporting the XFIXES, DAMAGE, RENDER, and COMPOSITE extensions.  It enables
basic eye-candy effects.")
    (home-page "https://cgit.freedesktop.org/xorg/app/xcompmgr/")
    (license (license:x11-style
              "https://cgit.freedesktop.org/xorg/app/xcompmgr/tree/COPYING"))))

(define-public xpra
  (package
    (name "xpra")
    (version "6.2.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
           (url "https://github.com/Xpra-org/xpra.git")
           (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0nfvvlz6kjs2sgl71j79jn7wnvbv1q1iqb0dz5qp9yf8j810r00x"))
       (patches (search-patches "xpra-6.0-systemd-run.patch"
                                "xpra-6.1-install_libs.patch"))))
    (build-system python-build-system)
    (inputs
     (list bash-minimal                 ; for wrap-program
           ;; Essential dependencies.
           libjpeg-turbo
           libwebp
           ffmpeg
           libx11
           libxrandr
           libxtst
           libxfixes
           libxkbfile
           libxcomposite
           libxdamage
           libxext
           libxres
           lz4
           gtk+
           python-pycairo
           python-pygobject
           xauth
           xorg-server
           xf86-video-dummy
           xf86-input-mouse
           xxhash
           python-pillow
           ;; Optional dependencies.
           libx264
           x265
           libvpx
           python-rencode               ; For speed.
           python-numpy
           python-pyopengl              ; Drawing acceleration.
           python-pyopengl-accelerate   ; Same.
           python-paramiko              ; Tunneling over SSH.
           python-dbus                  ; For desktop notifications.
           dbus                         ; For dbus-launch command.
           python-lz4                   ; Faster compression than zlib.
           python-netifaces
           python-pycups))
    (native-inputs (list pkg-config pandoc python-cython-3))
    (arguments
     (list
      #:configure-flags #~(list "--without-Xdummy"
                                "--without-Xdummy_wrapper"
                                "--with-opengl"
                                "--without-debug"
                                "--without-strict") ; Ignore compiler warnings.
      #:modules '((guix build python-build-system)
                  (guix build utils))
      ;; Do not run test-cases.  This would rebuild all modules and they seem
      ;; to require python2.
      #:tests? #f
      #:phases
      #~(modify-phases %standard-phases
          ;; Must pass the same flags as 'install, otherwise enabled modules may
          ;; not be built.
          (replace 'build
            (lambda* (#:key configure-flags #:allow-other-keys)
              (apply invoke (append (list "python" "setup.py" "build")
                                    configure-flags))))
          (add-before 'install 'fix-paths
            (lambda* (#:key inputs #:allow-other-keys)
              ;; Fix binary paths.
              (substitute* '("xpra/scripts/config.py"
                             "xpra/x11/vfb_util.py")
                (("\"Xvfb\"")
                 (format #f "~s" (search-input-file inputs "bin/Xvfb")))
                (("\"Xorg\"")
                 (format #f "~s" (search-input-file inputs "bin/Xorg")))
                (("\"xauth\"")
                 (format #f "~s" (search-input-file inputs "bin/xauth"))))
              ;; Fix directory of config files.
              (substitute* '("xpra/scripts/config.py"
                             "xpra/platform/posix/paths.py")
                (("\"/etc/xpra/?\"")
                 (string-append "\"" #$output "/etc/xpra/\"")))
              ;; XXX: Stolen from (gnu packages linux)
              (define (append-to-file name body)
                (let ((file (open-file name "a")))
                  (display body file)
                  (close-port file)))
              ;; Add Xorg module paths.
              (append-to-file
               "fs/etc/xpra/xorg.conf"
               (string-append
                "\nSection \"Files\"\nModulePath \""
                #$(this-package-input "xf86-video-dummy") "/lib/xorg/modules,"
                #$(this-package-input "xf86-input-mouse") "/lib/xorg/modules,"
                #$(this-package-input "xorg-server") "/lib/xorg/modules\"\n"
                "EndSection\n\n"))
              (substitute* '("xpra/scripts/config.py"
                             "fs/etc/xpra/conf.d/60_server.conf.in"
                             "tests/unittests/unit/server/mixins/notification_test.py")
                ;; The trailing -- is intentional, so we only replace it inside
                ;; a command line.
                (("dbus-launch --")
                 (string-append (search-input-file inputs "/bin/dbus-launch")
                                " --")))
              ;; /run/user does not exist on guix system.
              (substitute* "./xpra/scripts/config.py"
                (("socket-dir.*: \"\",")
                 "socket-dir\"        : \"~/.xpra\","))))
          ;; GTK3 will not be found, if GI can’t find its typelibs.
          (add-after 'install 'wrap-program
            (lambda* (#:key outputs #:allow-other-keys)
              ;; XXX: only export typelibs in inputs
              (wrap-program (search-input-file outputs "bin/xpra")
                `("GI_TYPELIB_PATH" = (,(getenv "GI_TYPELIB_PATH")))))))))
    (home-page "https://www.xpra.org/")
    (synopsis "Remote access to individual applications or full desktops")
    (description "Xpra is a persistent remote display server and client for
forwarding applications and desktop screens.  It gives you remote access to
individual applications or full desktops.  On X11, it is also known as
``@command{screen} for X11'': it allows you to run programs, usually on a
remote host, direct their display to your local machine, and then to
disconnect from these programs and reconnect from the same or another machine,
without losing any state.  It can also be used to forward full desktops from
X11 servers, Windows, or macOS.")
    (license license:gpl2+)))

(define-public uim
  (package
    (name "uim")
    (version "1.8.8")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/uim/uim/releases/download/"
                           version "/uim-" version ".tar.bz2"))
       (sha256
        (base32
         "1p7sl0js47ja4glmax93ci59h02ipqw3wxkh4f1qgaz5qjy9nn9l"))))
    (build-system gnu-build-system)
    (inputs
     (list anthy libedit libxft m17n-lib ncurses))
    (native-inputs
     `(("emacs" ,emacs-minimal)
       ("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%default-gnu-imported-modules
                           (guix build emacs-utils))
       #:configure-flags
       (list "--with-anthy-utf8"
             ;; Set proper runpath
             (string-append "LDFLAGS=-Wl,-rpath=" %output "/lib")
             "CFLAGS=-O2 -g -fcommon")
       #:phases
       (modify-phases %standard-phases
         ;; Set path of uim-el-agent and uim-el-helper-agent executables
         (add-after 'configure 'configure-uim-el
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (emacs-substitute-variables "emacs/uim-var.el"
                 ("uim-el-agent" (string-append out "/bin/uim-el-agent"))
                 ("uim-el-helper-agent" (string-append out "/bin/uim-el-helper-agent"))))
             #t))
         (add-after 'install 'make-autoloads
           (lambda* (#:key outputs #:allow-other-keys)
             (emacs-generate-autoloads
              ,name (string-append (assoc-ref outputs "out")
                                   "/share/emacs/site-lisp/uim-el"))
             #t)))))
    (home-page "https://github.com/uim/uim")
    (synopsis "Multilingual input method framework")
    (description "Uim is a multilingual input method library and environment.
It provides a simple, easily extensible and high code-quality input method
development platform, and useful input method environment for users of desktop
and embedded platforms.")
    (license (list license:lgpl2.1+ ; scm/py.scm, pixmaps/*.{svg,png} (see pixmaps/README)
                   license:gpl2+ ; scm/pinyin-big5.scm
                   license:gpl3+ ; scm/elatin-rules.cm
                   license:public-domain ; scm/input-parse.scm, scm/match.scm
                   ;; gtk2/toolbar/eggtrayicon.{ch},
                   ;; qt3/chardict/kseparator.{cpp,h},
                   ;; qt3/pref/kseparator.{cpp,h}
                   license:lgpl2.0+
                   ;; pixmaps/*.{svg,png} (see pixmaps/README),
                   ;; all other files
                   license:bsd-3))))

(define-public uim-gtk
  (package/inherit uim
    (name "uim-gtk")
    (inputs
     `(("gtk" ,gtk+)
       ("gtk" ,gtk+-2)
       ,@(package-inputs uim)))
    (arguments
     (substitute-keyword-arguments (package-arguments uim)
       ((#:configure-flags configure-flags)
        (append configure-flags (list "CFLAGS=-O2 -g -fcommon")))))
    (synopsis "Multilingual input method framework (GTK+ support)")))

(define-public uim-qt
  (package/inherit uim
    (name "uim-qt")
    (inputs
     `(("qt" ,qtbase-5)
       ("qtx11extras" ,qtx11extras)
       ,@(package-inputs uim)))
    (arguments
     (substitute-keyword-arguments (package-arguments uim)
       ((#:configure-flags configure-flags)
        (append configure-flags (list "--with-qt5-immodule"
                                      "--with-qt5"
                                      "CPPFLAGS=-fcommon")))))
    (synopsis "Multilingual input method framework (Qt support)")))

(define-public keynav
  (package
    (name "keynav")
    (version "0.20110708.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "http://http.debian.net/debian/pool/main/k/keynav/keynav_"
             version ".orig.tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1gizjhji3yspxxxvb90js3z1bv18rbf5phxg8rciixpj3cccff8z"))))
    (build-system gnu-build-system)
    (inputs
     (list cairo
           glib
           libx11
           libxext
           libxinerama
           libxtst
           xdotool))
    (native-inputs
     (list pkg-config))
    (arguments
     `(#:tests? #f ;No tests.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'setenv
           (lambda _
             (setenv "CC" (which "gcc"))
             #t))
         (add-after 'unpack 'patch-keynav
           (lambda _
             (substitute* "keynav.c"
               (("xdo_symbol_map") "xdo_get_symbol_map")
               (("xdo_window_setclass") "xdo_set_window_class")
               (("xdo_window_get_active") "xdo_get_window_at_mouse")
               (("xdo_click") "xdo_click_window")
               (("xdo_mouseup") "xdo_mouse_up")
               (("xdo_mousedown") "xdo_mouse_down")
               (("xdo_mousemove") "xdo_move_mouse")
               (("xdo_mousemove_relative") "xdo_move_mouse_relative")
               (("xdo_mouselocation") "xdo_get_mouse_location")
               (("xdo_mouse_wait_for_move_to") "xdo_wait_for_mouse_move_to")
               (("xdo_keysequence_up") "xdo_send_keysequence_window_up")
               (("xdo_keysequence_down") "xdo_send_keysequence_window_down"))
             #t))
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "keynav" (string-append out "/bin"))
               (install-file "keynavrc" (string-append out "/etc")))
             #t)))))
    (home-page "https://www.semicomplete.com/projects/keynav/")
    (synopsis "Keyboard-driven mouse cursor mover")
    (description
     "Keynav makes your keyboard a fast mouse cursor mover.  You can move the
cursor to any point on the screen with a few key strokes.  It also simulates
mouse click.  You can do everything mouse can do with a keyboard.")
    (license license:bsd-3)))

(define-public transset
  (package
    (name "transset")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.x.org/releases/individual/app/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "1hw0sacfa2r18zv1jj47y5blrsb3b5w0yrp5yq9bj3d434k5lhsp"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs (list libxcomposite libxdamage libxrender))
    (synopsis "Set the transparency of X11 windows")
    (description "@command{transset} is a simple program for X servers
supporting the XFIXES, DAMAGE, and COMPOSITE extensions.  It lets the
user set the transparency on a window.")
    (home-page "https://gitlab.freedesktop.org/xorg/app/transset")
    (license license:x11)))

(define-public bdfresize
  (package
    (name "bdfresize")
    (version "1.5-11")
    (source (origin
              ;; Former upstream at
              ;; <http://openlab.ring.gr.jp/efont/dist/tools/bdfresize/>
              ;; vanished so use Debian, which in practice is the new
              ;; upstream.
              (method git-fetch)
              (uri (git-reference
                    (url "https://salsa.debian.org/debian/bdfresize.git")
                    (commit (string-append "debian/" version))))
              (sha256
               (base32
                "0n3i29wicak8n10vkkippym8yw4ir8f7a263a8rwb8q16wqrxx85"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each make-file-writable (find-files "."))

                  ;; Remove broken declaration.
                  (substitute* "charresize.c"
                    (("char\t\\*malloc\\(\\);")
                     ""))

                  ;; Remove old configury that doesn't support modern
                  ;; command-line options, new architectures, etc.
                  (for-each delete-file
                            '("configure" "install-sh"
                              "missing" "mkinstalldirs"))
                  #t))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config autoconf automake))
    (synopsis "Resize fonts in the BDF format")
    (description
     "This package provides @command{bdfresize}, a command to magnify or
reduce fonts in the Glyph Bitmap Distribution Format (BDF).  It produces BDF
output.")
    (home-page "https://tracker.debian.org/pkg/bdfresize")
    (license license:gpl2+)) )

(define-public console-setup
  (package
    (name "console-setup")
    (version "1.212")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://salsa.debian.org/installer-team/console-setup.git")
             (commit version)))
       (sha256
        (base32 "1g1riswqalhk95i81xmy4dzwp1wwfiapjjiv0x06cp8bnc2rdxbq"))
       (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     (list #:make-flags
           #~(list (string-append "SHELL=" (assoc-ref %build-inputs "bash")
                                  "/bin/bash")
                   (string-append "prefix=" #$output))
           #:tests? #f                  ; no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (add-after 'unpack 'patch-file-names
                 (lambda* (#:key inputs #:allow-other-keys)
                   ;; 'ckbcomp' calls out to 'cat' (!).  Give it the right file
                   ;; name.
                   (substitute* '("Keyboard/ckbcomp")
                     (("\"cat ")
                      (format #f "\"~a "
                              (search-input-file inputs "bin/cat"))))))
               (add-before 'build 'make-doubled-bdfs
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (apply invoke "make" "-C" "Fonts" "doubled_bdfs"
                          make-flags)))
               (replace 'install
                 (lambda* (#:key make-flags #:allow-other-keys)
                   (apply invoke "make" "install-linux"
                          make-flags)))
               (add-after 'install 'install-bdf2psf
                 (lambda* (#:key outputs #:allow-other-keys)
                   (install-file "Fonts/bdf2psf"
                                 (string-append #$output "/bin"))
                   (install-file "man/bdf2psf.1"
                                 (string-append #$output "/share/man/man1")))))))
    (native-inputs
     (list pkg-config
           bdftopcf
           bdfresize
           sharutils                    ; for 'uuencode'
           perl))
    (inputs
     (list bash-minimal
           coreutils
           perl))                       ; used by 'ckbcomp'
    (synopsis "Set up the Linux console font and keyboard")
    (description
     "console-setup provides the console with the same keyboard
configuration scheme that the X Window System has.  In particular, the
@command{ckbcomp} program compiles an XKB keyboard description to a keymap
suitable for @command{loadkeys} or @command{kbdcontrol}.  As a result, there
is no need to duplicate or change the console keyboard files just to make
simple customizations.

Besides the keyboard, the package also configures the font on the console.  It
includes a rich collection of fonts and supports several languages that would
otherwise be unsupported on the console (such as Armenian, Georgian, Lao, and
Thai).")
    (home-page "https://salsa.debian.org/installer-team/console-setup/")

    ;; Most of the code is GPLv2+; the Expat license applies to 'setupcon' and
    ;; 'ckbcomp-mini'.  The installed precompiled keyboard files are covered
    ;; by simple permissive licenses.  See the 'COPYRIGHT' file.
    (license (list license:gpl2+
                   license:expat))))

(define-public xcur2png
  (package
    (name "xcur2png")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/eworm-de/xcur2png")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0858wn2p14bxpv9lvaz2bz1rk6zk0g8zgxf8iy595m8fqv4q2fya"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list libpng libxcursor))
    (synopsis "Decode X cursors")
    (description
     "xcur2png is a program decomposes an X cursor into a set of PNG images and
a configuration file reusable by xcursorgen.")
    (home-page "https://github.com/eworm-de/xcur2png")
    (license license:gpl3+)))

(define-public gccmakedep
  (package
    (name "gccmakedep")
    (version "1.0.4")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://xorg/individual/util/gccmakedep-"
                           version ".tar.xz"))
       (sha256
        (base32 "1di5rxv930nhf5ls07fx04l0lqsakxycplmrx4kc6qzvd63zk22h"))))
    (build-system gnu-build-system)
    (synopsis "Create dependencies in makefiles using 'gcc -M'")
    (description
     "@command{gccmakedep} is a deprecated program which calls @code{gcc -M}
to output Makefile rules describing the dependencies of each source file, so
that Make knows which object files must be recompiled when a dependency has
changed.")
    (home-page "https://gitlab.freedesktop.org/xorg/util/gccmakedep")
    (license license:x11)))

(define-public xdialog
  (package
    (name "xdialog")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://xdialog.free.fr/Xdialog-"
                                  version ".tar.bz2"))
              (sha256
               (base32
                "16jqparb33lfq4cvd9l3jgd7fq86fk9gv2ixc8vgqibid6cnhi0x"))))
    (native-inputs
     (list pkg-config))
    (inputs
     `(("glib" ,glib)
       ("gettext" ,gettext-minimal)
       ("gtk" ,gtk+-2)))
    (arguments
     `(#:configure-flags '("--with-gtk2")))
    (build-system gnu-build-system)
    (home-page "http://xdialog.free.fr/")
    (synopsis "Convert a terminal program into a program with an X interface")
    (description "X11 replacement for the text util dialog Xdialog is designed
to be a drop-in replacement for the dialog and cdialog programs.  It converts
any terminal-based program into a program with an X interface.  The dialogs
are easier to see and use, and Xdialog adds more functionality such as a help
button and box, a treeview, an editbox, file and directory selectors, a range
box, and a calendar.  It uses GTK+, and will match your desktop theme.")
    (license license:gpl2+)))

(define-public xvfb-run
  (package
    (name "xvfb-run")
    (version "21.1.7-1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://debian/pool/main/x/xorg-server/"
                           "xorg-server_" version ".diff.gz"))
       (sha256
        (base32 "1073m4gzn8yv9kn70fbyq8a2xckgz0wljjr2w7i2bsrg767h29gd"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (replace 'unpack
            ;; Apply the source patch to an empty directory.
            (lambda* (#:key inputs #:allow-other-keys)
              (let*  ((diff.gz (basename #$source))
                      (diff (substring diff.gz 0 (string-rindex diff.gz #\.))))
                (mkdir "source")
                (chdir "source")
                (copy-file #$source diff.gz)
                (invoke "gunzip" diff.gz)
                (invoke "patch" "-Np1" "-i" diff)
                (chdir "debian/local"))))
          (delete 'configure)           ; nothing to configure
          (replace 'build
            (lambda* (#:key inputs #:allow-other-keys)
              (chmod "xvfb-run" #o755)
              (substitute* "xvfb-run"
                (("(\\(| )(fmt|stty|awk|cat|kill|getopt|mktemp|touch|rm|mcookie)"
                  _ prefix command)
                 (string-append prefix (which command)))
                ;; These also feature in UI messages, so be more strict.
                (("(AUTHFILE |command -v |exec )(Xvfb|xauth)"
                  _ prefix command)
                 (string-append prefix
                                (search-input-file
                                 inputs (string-append "bin/" command)))))))
          (replace 'check
            ;; There are no tests included.  Here we test whether we can run
            ;; a simple client and whether xvfb-run --help succeeds
            ;; without xvfb-run itself relying on $PATH.
            (lambda* (#:key tests? #:allow-other-keys)
              (when tests?
                (let ((old-PATH (getenv "PATH"))
                      (xterm (which "xterm")))
                  (unsetenv "PATH")
                  (invoke "./xvfb-run" xterm "-e" "true")
                  (invoke "./xvfb-run" "--help")
                  (setenv "PATH" old-PATH)))))
          (replace 'install
            (lambda _
              (let ((bin (string-append #$output "/bin"))
                    (man (string-append #$output "/share/man/man1")))
                (install-file "xvfb-run" bin)
                (install-file "xvfb-run.1" man)))))))
    (inputs
     (list util-linux                   ; for getopt
           xauth
           xorg-server))
    (native-inputs
     (list xterm))                      ; for the test
    ;; This script is not part of the upstream xorg-server.  It is provided only
    ;; as a patch added to Debian's package.
    (home-page "https://packages.debian.org/sid/xorg-server-source")
    (synopsis "Run X11 client or command in a virtual X server environment")
    (description
     "The @command{xvfb-run} wrapper simplifies running commands and scripts
within a virtual X server environment.  It sets up an X authority file or uses
an existing user-specified one, writes a cookie to it, and then starts the
@command{Xvfb} X server as a background process.  It also takes care of killing
the server and cleaning up before returning the exit status of the command.")
    (license (list license:x11          ; the script
                   license:gpl2+))))    ; the man page

(define-public setroot
  (package
    (name "setroot")
    (version "2.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ttzhou/setroot")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0w95828v0splk7bj5kfacp4pq6wxpyamvyjmahyvn5hc3ycq21mq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "DESTDIR=" (assoc-ref %outputs "out"))
             "PREFIX="
             "xinerama=1")
       #:tests? #f                       ; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list imlib2-1.7 libx11 libxinerama))
    (home-page "https://github.com/ttzhou/setroot")
    (synopsis "Simple X background setter inspired by imlibsetroot and feh")
    (description "Setroot is a lightweight X background setter with feh's
syntax without its image viewing capabilities.  It supports multiple monitors
and can restore previously set wallpapers and options.")
    (license license:gpl3+)))
