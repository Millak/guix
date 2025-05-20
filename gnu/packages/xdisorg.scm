;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2014, 2015, 2016 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2014, 2015, 2016 Alex Kost <alezost@gmail.com>
;;; Copyright © 2013, 2015, 2017-2019, 2021-2022 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2015, 2016 Mathieu Lirzin <mthl@gnu.org>
;;; Copyright © 2015 Alexander I.Grafov <grafov@gmail.com>
;;; Copyright © 2015 Andy Wingo <wingo@igalia.com>
;;; Copyright © 2015 xd1le <elisp.vim@gmail.com>
;;; Copyright © 2015 Florian Paul Schmidt <mista.tapas@gmx.net>
;;; Copyright © 2016 Christine Lemmer-Webber <cwebber@dustycloud.org>
;;; Copyright © 2016, 2018 Ricardo Wurmus <rekado@elephly.net>
;;; Copyright © 2016-2021, 2023 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Leo Famulari <leo@famulari.name>
;;; Copyright © 2016, 2017, 2019, 2020 Marius Bakke <mbakke@fastmail.com>
;;; Copyright © 2016 Petter <petter@mykolab.ch>
;;; Copyright © 2017 Mekeor Melire <mekeor.melire@gmail.com>
;;; Copyright © 2017 Nikita <nikita@n0.is>
;;; Copyright © 2017–2021, 2024 Tobias Geerinckx-Rice <me@tobias.gr>
;;; Copyright © 2017 Marek Benc <dusxmt@gmx.com>
;;; Copyright © 2017 Mike Gerwitz <mtg@gnu.org>
;;; Copyright © 2018 Thomas Sigurdsen <tonton@riseup.net>
;;; Copyright © 2018, 2019 Rutger Helling <rhelling@mykolab.com>
;;; Copyright © 2018, 2019 Pierre Neidhardt <mail@ambrevar.xyz>
;;; Copyright © 2018 Nam Nguyen <namn@berkeley.edu>
;;; Copyright © 2019 Wiktor Żelazny <wzelazny@vurv.cz>
;;; Copyright © 2019 Kyle Andrews <kyle.c.andrews@gmail.com>
;;; Copyright © 2019, 2020 Josh Holland <josh@inv.alid.pw>
;;; Copyright © 2019, 2021 Tanguy Le Carrour <tanguy@bioneland.org>
;;; Copyright © 2020, 2021 Guillaume Le Vaillant <glv@posteo.net>
;;; Copyright © 2020 David Wilson <david@daviwil.com>
;;; Copyright © 2020 Ivan Vilata i Balaguer <ivan@selidor.net>
;;; Copyright © 2020, 2021 Brice Waegeneire <brice@waegenei.re>
;;; Copyright © 2020 Damien Cassou <damien@cassou.me>
;;; Copyright © 2020 John Soo <jsoo1@asu.edu>
;;; Copyright © 2020 Boris A. Dekshteyn <boris.dekshteyn@gmail.com>
;;; Copyright © 2020 Alex McGrath <amk@amk.ie>
;;; Copyright © 2020 Ivan Kozlov <kanichos@yandex.ru>
;;; Copyright © 2020 Brett Gilio <brettg@gnu.org>
;;; Copyright © 2020 Gabriel Arazas <foo.dogsquared@gmail.com>
;;; Copyright © 2020 James Smith <jsubuntuxp@disroot.org>
;;; Copyright © 2020 B. Wilson <elaexuotee@wilsonb.com>
;;; Copyright © 2020, 2021, 2023, 2024 Zheng Junjie <873216071@qq.com>
;;; Copyright © 2021, 2022, 2023 Maxim Cournoyer <maxim.cournoyer@gmail.com>
;;; Copyright © 2021, 2022, 2024 Nicolas Goaziou <mail@nicolasgoaziou.fr>
;;; Copyright © 2021 Xinglu Chen <public@yoctocell.xyz>
;;; Copyright © 2021 Renzo Poddighe <renzo@poddighe.nl>
;;; Copyright © 2021 Paul A. Patience <paul@apatience.com>
;;; Copyright © 2021 Niklas Eklund <niklas.eklund@posteo.net>
;;; Copyright © 2021 Calum Irwin <calumirwin1@gmail.com>
;;; Copyright © 2021 Nikita Domnitskii <nikita@domnitskii.me>
;;; Copyright © 2021 ikasero <ahmed@ikasero.com>
;;; Copyright © 2021 Felix Gruber <felgru@posteo.net>
;;; Copyright © 2021 jgart <jgart@dismail.de>
;;; Copyright © 2022, 2024 John Kehayias <john.kehayias@protonmail.com>
;;; Copyright © 2022 Jai Vetrivelan <jaivetrivelan@gmail.com>
;;; Copyright © 2022 Derek Chuank <derekchuank@outlook.com>
;;; Copyright © 2022, 2023 Wamm K. D. <jaft.r@outlook.com>
;;; Copyright © 2022 Tobias Kortkamp <tobias.kortkamp@gmail.com>
;;; Copyright © 2023 Yovan Naumovski <yovan@gorski.stream>
;;; Copyright © 2023 Jake Leporte <jakeleporte@outlook.com>
;;; Copyright © 2023 Hilton Chain <hako@ultrarare.space>
;;; Copyright © 2022 Mehmet Tekman <mtekman89@gmail.com>
;;; Copyright © 2024 Artyom V. Poptsov <poptsov.artyom@gmail.com>
;;; Copyright © 2024 Igor Goryachev <igor@goryachev.org>
;;; Copyright © 2024, 2025 Ashish SHUKLA <ashish.is@lostca.se>
;;; Copyright © 2024 Spencer Peters <spencerpeters@protonmail.com>
;;; Copyright © 2024 Jakob Kirsch <jakob.kirsch@web.de>
;;; Copyright © 2025 Evgeny Pisemsky <mail@pisemsky.site>
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

(define-module (gnu packages xdisorg)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system copy)
  #:use-module (guix build-system glib-or-gtk)
  #:use-module (guix build-system go)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system meson)
  #:use-module (guix build-system python)
  #:use-module (guix build-system pyproject)
  #:use-module (guix build-system scons)
  #:use-module (guix download)
  #:use-module (guix gexp)
  #:use-module (guix git-download)
  #:use-module (guix hg-download)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix utils)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages algebra)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages build-tools)
  #:use-module (gnu packages check)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cpp)
  #:use-module (gnu packages crypto)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages file)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gawk)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages golang-build)
  #:use-module (gnu packages golang-xyz)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages guile)
  #:use-module (gnu packages guile-xyz)
  #:use-module (gnu packages haskell-xyz)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages kde-frameworks)
  #:use-module (gnu packages libbsd)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages man)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pciutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-check)
  #:use-module (gnu packages python-build)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages terminals)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages wm)
  #:use-module (gnu packages webkit)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages pretty-print)
  #:use-module (gnu packages)
  #:use-module (ice-9 match))

;; packages outside the x.org system proper

(define-public xtitle
  (package
    (name "xtitle")
    (version "0.4.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/xtitle")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0f5070k2bwarghl1vq886pl52xck1x5p7x3qhlfchsc2y3dcqms9"))))
    (build-system gnu-build-system)
    (inputs
     (list libxcb xcb-util xcb-util-wm))
    (arguments
     `(#:tests? #f                      ;no test suite
       #:make-flags (list (string-append "CC=" ,(cc-for-target))
                          (string-append "PREFIX=" %output))
       #:phases (modify-phases %standard-phases (delete 'configure))))
    (home-page "https://github.com/baskerville/xtitle")
    (synopsis "Output X window titles")
    (description
     "If arguments are given, @code{xtitle} outputs the title of each
specified window, otherwise it outputs the title of the active window.  With
@emph{snoop} mode on, it continuously monitors the specified windows and
outputs when titles change.")
    (license license:unlicense)))

(define-public xvkbd
  (package
    (name "xvkbd")
    (version "4.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "http://t-sato.in.coocan.jp/xvkbd/xvkbd-"
                           version ".tar.gz"))
       (sha256
        (base32 "1x5yldv9y99cw5hzzs73ygdn1z80zns9hz0baa355r711zghfbcm"))))
    (build-system gnu-build-system)
    (arguments
     (list #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'remove-bugs
                 ;; Both variables are inexplicably but explicitly set to
                 ;; $(pkg-config --variable=VARIABLE xt) instead of our own.
                 (lambda _
                   (substitute* "Makefile.in"
                     (("^(appdefaultdir|datarootdir) = .*" _ variable)
                      (string-append variable " = @" variable "@\n"))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libxaw libxmu libxtst))
    (home-page "http://t-sato.in.coocan.jp/xvkbd/")
    (synopsis "Virtual computer keyboard for the X Window System")
    (description
     "The @acronym{xvkbd, X virtual keyboard} displays a drawing of a computer
keyboard in a window on the screen.  Clicking on its keys sends the
corresponding keystroke(s) to other X clients, as if typed on a physical
keyboard.

This is useful for systems without keyboard hardware but with a pointing device,
such as kiosk terminals or handheld devices with touch screens.

A limited number of keyboard layouts are available, as is dictionary completion.
You can also use xvkbd to send a series of predetermined keystrokes from the
command line, without displaying a keyboard at all.")
    (license license:gpl2+)))

(define-public aquamarine
  (package
    (name "aquamarine")
    (version "0.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/aquamarine")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "01lmzmb5bzphichbyim7iy04405af5mqcqf8ki3if4wdxkdmbfn9"))))
    (build-system cmake-build-system)
    (arguments
     (list #:cmake cmake-next
           ;; TODO: Figure out what's expected in the test environment.
           #:tests? #f))
    (native-inputs
     (list gcc-14 hyprwayland-scanner pkg-config))
    (inputs
     (list eudev
           hwdata
           hyprutils
           libdisplay-info
           libglvnd
           libinput-minimal
           libseat
           mesa
           pixman
           wayland
           wayland-protocols))
    (home-page "https://github.com/hyprwm/aquamarine")
    (synopsis "Linux rendering backend library")
    (description
     "Aquamarine is a C++-only Linux rendering backend library.  It provides
basic abstractions for an application to render on a Wayland session (in a
window) or a native DRM session.  It is agnostic of the rendering API (Vulkan
/ OpenGL).")
    (license license:bsd-3)))

(define-public arandr
  (package
    (name "arandr")
    (version "0.1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://christian.amsuess.com/tools/arandr"
                                  "/files/arandr-" version ".tar.gz"))
              (sha256
               (base32
                "00mfhaqjxx4m3y0ml44infpbp500prs031vhawwjp0dvk0vbxjz4"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  ;; Do not record a timestamp and file name in gzipped man
                  ;; pages (this is equivalent to 'gzip --no-name'.)
                  (substitute* "setup.py"
                    (("gzip\\.open\\(gzfile, 'w', 9\\)")
                     "gzip.GzipFile('', 'wb', 9, open(gzfile, 'wb'), 0.)"))))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "screenlayout/xrandr.py"
               (("\"xrandr\"") (string-append "\"" (assoc-ref inputs "xrandr")
                                              "/bin/xrandr\"")))))
         (add-after 'install 'wrap-gi-typelib
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out               (assoc-ref outputs "out"))
                   (gi-typelib-path   (getenv "GI_TYPELIB_PATH")))
               (wrap-program (string-append out "/bin/arandr")
                 `("GI_TYPELIB_PATH" ":" prefix (,gi-typelib-path)))))))
       #:tests? #f)) ;no tests
    (inputs (list bash-minimal gtk+ python-pycairo python-pygobject xrandr))
    (native-inputs (list gettext-minimal python-docutils))
    (home-page "https://christian.amsuess.com/tools/arandr/")
    (synopsis "Another RandR graphical user interface")
    ;; TRANSLATORS: "X11 resize-and-rotate" should not be translated.
    (description "ARandR is designed to provide a simple visual front end for
the X11 resize-and-rotate (RandR) extension.  Relative monitor positions are
shown graphically and can be changed in a drag-and-drop way.  Configurations
are saved as executable shell scripts which can be loaded without using this
program.")
    (license license:gpl3+)))

(define-public autorandr
  (package
    (name "autorandr")
    (version "1.15")
    (home-page "https://github.com/phillipberndt/autorandr")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url home-page)
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1n4cmgisk1p199zny8zrdpfrbakchd6pvpkp9vzqqdw2f75iylzh"))))
    (build-system python-build-system)
    (native-inputs
     (list pkg-config))
    (inputs
     (list xrandr libxcb))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'configure
            (lambda* (#:key inputs outputs #:allow-other-keys)
              (let ((xrandr (search-input-file inputs "/bin/xrandr")))
                (substitute* "contrib/etc/xdg/autostart/autorandr.desktop"
                  (("/usr") #$output))
                (substitute* "autorandr.py"
                  (("popen\\(\"xrandr") (string-append "popen(\"" xrandr))
                  (("\\[\"xrandr") (string-append "[\"" xrandr)))
                (substitute* "contrib/autorandr_launcher/autorandr_launcher.c"
                  (("/usr/bin/autorandr")
                   (string-append #$output "/bin/autorandr")))
                (setenv "CC" "gcc"))))
          (add-after 'install 'install-contrib
            (lambda* (#:key outputs #:allow-other-keys)
              (invoke "make"
                      (string-append "DESTDIR=" #$output)
                      "PREFIX="
                      "BASH_COMPLETIONS_DIR=etc/bash_completion.d"
                      "install"
                      "TARGETS=autorandr launcher manpage bash_completion"))))))
    (synopsis "Auto-detect connected displays and load appropriate setup")
    (description "Autorandr wraps around xrandr to help with X11
multi-screen configuration management.  It allows the user to create profiles
for various multi-screen setups.  Autorandr automatically detects the profiles
that can be activated based on the connected hardware.  Hook scripts can be
used to further tweak the behaviour of the different profiles.")
    (license license:gpl3+)))

(define-public bemenu
  (package
    (name "bemenu")
    (version "0.6.23")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Cloudef/bemenu")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0a4ihg1v1yyfwi4qq2zybhy39vw3nikd5qjrd5x3nxgjd0knmynj"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f ; there are no tests
      #:make-flags
      #~(list (string-append "CC=" #$(cc-for-target))
              "CFLAGS=-O2 -fPIC"
              (string-append "LDFLAGS=-Wl,-rpath=" #$output "/lib")
              (string-append "PREFIX=" #$output))
      #:phases
      #~(modify-phases %standard-phases
          (delete 'configure))))         ; no configure script
    (inputs
     (list cairo
           libx11
           libxkbcommon
           libxinerama
           ncurses
           pango
           wayland
           wayland-protocols))
    (native-inputs
     (list doxygen pkg-config scdoc))
    (home-page "https://github.com/Cloudef/bemenu")
    (synopsis "Dynamic menu library and client program inspired by dmenu")
    (description
     "bemenu is a dynamic menu which allows the user to flexibly select from a
list of options (usually programs to launch).  It renders the menu graphically
with X11 or Wayland, or in a text terminal with ncurses.")
    (license (list license:gpl3+ ; client program[s] and other sources
                   license:lgpl3+))))   ; library and bindings

(define-public cliphist
  (package
    (name "cliphist")
    (version "0.5.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/sentriz/cliphist")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1cbhrw9vk8c0in9yyhlp0k9rldgjwbcj00d7vqh69p3igznhdgsk"))))
    (build-system go-build-system)
    (arguments
     (list
      #:install-source? #f
      #:import-path "go.senan.xyz/cliphist"))
    (native-inputs
     (list go-github-com-rogpeppe-go-internal
           go-go-etcd-io-bbolt
           go-go-senan-xyz-flagconf
           go-golang-org-x-image))
    (home-page "https://github.com/sentriz/cliphist")
    (synopsis "Clipboard history manager for wayland with support for images")
    (description
     "A Wayland clipboard history manager.  It can write clipboard changes to
a history file, recall history with any picker which accepts input from
stdin (including dmenu, rofi, and wofi), copy and past both images and text,
and preserve leading and trailing whitespace.")
    (license license:gpl3)))

(define-public copyq
  (package
    (name "copyq")
    (version "8.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hluk/CopyQ")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "029s1pdp177fnrq5qrwjmd5pf1672l5jhq99is1lczrxi6bsf2qk"))))
    (build-system cmake-build-system)
    (arguments
     (list
      #:configure-flags #~(list "-DCMAKE_BUILD_TYPE=Release")
      #:tests? #f)) ; Test suite is a rather manual process.
    (inputs
     (list qtbase-5
           qtscript
           qtsvg-5
           qtx11extras
           qtdeclarative-5
           qtwayland-5
           wayland
           knotifications-5))
    (native-inputs
     (list extra-cmake-modules qttools-5))
    (synopsis "Clipboard manager with advanced features")
    (description "CopyQ is clipboard manager with editing and scripting
features.  CopyQ monitors system clipboard and saves its content in customized
tabs.  Saved clipboard can be later copied and pasted directly into any
application.")
    (home-page "https://hluk.github.io/CopyQ/")
    (license license:gpl3+)))

(define-public xkeysnail
  (package
    (name "xkeysnail")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "xkeysnail" version))
       (sha256
        (base32
         "1xyqp6yqxcwmxaqj86qcsiz0ly7bwr0a2w835myz909irhip3ngf"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))                    ;tests need /dev/uinput
    (inputs
     (list python-appdirs python-evdev python-inotify-simple python-xlib
           python-six))
    (home-page "https://github.com/mooz/xkeysnail")
    (synopsis "Keyboard remapping tool for the X11 environment")
    (description
     "Xkeysnail is an X environment keyboard remapping tool, featuring
high-level and flexible remapping mechanisms.  It affects the low-level
layers (evdev and uinput), making remapping work in almost all the places.")
    (license license:gpl3+)))           ; see README.md (no licence headers)

(define-public xkb-switch
  (package
    (name "xkb-switch")
    (version "1.8.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/grwlf/xkb-switch")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1sd6ihgsswp6hjm1i4y092n4gl3gj0bc22grz4n7iy43mwphi40d"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))                    ;no test target
    (inputs
     (list libx11 libxkbfile))
    (home-page "https://github.com/grwlf/xkb-switch")
    (synopsis "Switch your X keyboard layouts from the command line")
    (description
     "xkb-switch is a C++ program that queries and changes the XKB layout
state.")
    (license license:gpl3+)))

(define-public xkblayout
  ;; Upstream doesn't have any version numbers
  (let ((version "0.0.0")
        (revision "0")
        (commit "c0851b0f4bc9bc1a07240605baac8e50abe63fa8"))
    (package
      (name "xkblayout")
      (version (git-version version revision commit))
      (home-page "https://gitlab.freedesktop.org/whot/xkblayout")
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url home-page)
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32 "0057988l5l7pmwg7dp6cqvj5l4lr0g5z3wq189g6kz36l9rmh675"))))
      (build-system python-build-system)
      (synopsis "XKB layout template generator")
      (description "xkblayout is a CLI application to generate templates for
a new XKB layout, either in the user's home directory or the system directory.")
      (license license:gpl3+))))

(define-public xclip
  (package
    (name "xclip")
    (version "0.13")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/astrand/xclip")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0q0hmvcjlv8arhh1pzhja2wglyj6n7z209jnpnzd281kqqv4czcs"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f))                              ;there is no test suite
    (native-inputs
     (list autoconf automake))
    (inputs (list libxmu libxt))
    (home-page "https://github.com/astrand/xclip")
    (synopsis "Command line interface to X11 clipboard")
    (description "Xclip is a command line interface to the X11 clipboard.  It
can also be used for copying files, as an alternative to sftp/scp, thus
avoiding password prompts when X11 forwarding has already been setup.")
    (license license:gpl2+)))

(define-public libxkbcommon
  (package
    (name "libxkbcommon")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://xkbcommon.org/download/libxkbcommon-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "0awwz5pg9x5bj0d7dpg4a7bd4gl6k55mlpxwb12534fkrpn19p0f"))))
    (outputs '("out" "doc"))
    (build-system meson-build-system)
    (inputs
     (list libx11
           libxcb
           libxml2
           wayland
           wayland-protocols
           xkeyboard-config))
    (native-inputs
     (append
       (list bison doxygen pkg-config python
             ;; wayland-scanner is required at build time.
             wayland)
       (if (%current-target-system)
         (list pkg-config-for-build)
         '())))
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "-Dxkb-config-root="
                             (search-input-directory
                              %build-inputs "share/X11/xkb"))
              (string-append "-Dx-locale-root="
                             (search-input-directory
                              %build-inputs "share/X11/locale")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'move-doc
            (lambda _
              (let ((old (string-append #$output "/share/doc"))
                    (new (string-append #$output:doc "/share/doc")))
                (mkdir-p (dirname new))
                (rename-file old new))))
          (add-after 'install 'symlink-pc
            ;; in Requires.private of xkbregistry.pc
            ;; XXX: Symlink libxml-2.0.pc in order to avoid putting
            ;; libxml2 as a propagated input.
            (lambda _
              (let ((stem "/lib/pkgconfig/libxml-2.0.pc"))
                (symlink (string-append #$(this-package-input "libxml2") stem)
                         (string-append #$output stem))))))))
    (home-page "https://xkbcommon.org/")
    (synopsis "Library to handle keyboard descriptions")
    (description "Xkbcommon is a library to handle keyboard descriptions,
including loading them from disk, parsing them and handling their
state.  It is mainly meant for client toolkits, window systems, and other
system applications; currently that includes Wayland, kmscon, GTK+, Qt,
Clutter, and more.  Despite the name, it is not currently used by anything
X11 (yet).")
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))
    (properties '((cpe-name . "xkbcommon")))))

(define-public libxkbcommon-1.5
  (package
    (inherit libxkbcommon)
    (version "1.5.0")
    (source (origin
              (inherit (package-source libxkbcommon))
              (method url-fetch)
              (uri (string-append "https://xkbcommon.org/download/libxkbcommon-"
                                  version ".tar.xz"))
              (sha256
               (base32
                "05z08rpa464x8myjxddhix7jp9jcmakd7xrybx4hz8dwpg2123sn"))))))

(define-public libxkbcommon-1.8
  (package
    (inherit libxkbcommon)
    (version "1.8.1")
    (source (origin
              (inherit (package-source libxkbcommon))
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xkbcommon/libxkbcommon")
                    (commit (string-append "xkbcommon-" version))))
              (file-name (git-file-name (package-name libxkbcommon) version))
              (sha256
               (base32
                "0fz6mf99lyp7x6g6v33210hhpykbg32fjmckyvxfpd805cza0xrj"))))
    (arguments
     (substitute-keyword-arguments (package-arguments libxkbcommon)
       ((#:configure-flags flags #~(list))
        #~(cons "-Denable-docs=true" #$flags))))
    (native-inputs (modify-inputs (package-native-inputs libxkbcommon)
                     (append xorg-server  ;; Xvfb for tests
                             xkbcomp)))))   ;; xkbcomp for tests

(define-public libfakekey
  (package
    (name "libfakekey")
    (version "0.3")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
              (url "https://git.yoctoproject.org/git/libfakekey")
              (commit version)))
        (file-name (git-file-name name version))
        (sha256
         (base32 "1jw1d4wc1ysiijirc7apnz3sryrxbl9akgb92mh06dvfkz2nblj0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list "AM_LDFLAGS=-lX11")
       #:phases
       (modify-phases %standard-phases
         (replace 'bootstrap
           ;; ./autogen.sh calls ./configure before shebangs have been patched.
           (lambda _
             (invoke "autoreconf" "-vfi"))))))
    (native-inputs
     (list pkg-config
           ;; For bootstrapping from git.
           autoconf automake libtool))
    (inputs
     (list libxtst libx11))
    (home-page "https://www.yoctoproject.org/tools-resources/projects/matchbox")
    (synopsis "X virtual keyboard library")
    (description
     "Libfakekey is a virtual keyboard library for X.")
    (license license:gpl2)))

(define-public xdotool
  (package
    (name "xdotool")
    (version "3.20211022.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
              "https://github.com/jordansissel/xdotool/releases/download/v"
              version "/xdotool-" version ".tar.gz"))
        (sha256
          (base32
           "1nlsbwsdsgys607f00sc8xgb7l7cdzsb14avsg5fly3dvv7zmw4n"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f ; Test suite requires a lot of black magic
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys #:rest args)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/lib"))
               (setenv "PREFIX" out)
               (setenv "LDFLAGS" (string-append "-Wl,-rpath=" out "/lib"))
               (setenv "CC" "gcc")
               #t))))))
    (native-inputs (list perl)) ; for pod2man
    (inputs (list libx11
                  libxext
                  libxi
                  libxinerama
                  libxtst
                  libxkbcommon))
    (home-page "https://www.semicomplete.com/projects/xdotool/")
    (synopsis "Fake keyboard/mouse input, window management, and more")
    (description "Xdotool lets you simulate keyboard input and mouse activity,
move and resize windows, etc.  It does this using X11's XTEST extension and
other Xlib functions.  Additionally, you can search for windows and move,
resize, hide, and modify window properties like the title.  If your window
manager supports it, you can use xdotool to switch desktops, move windows
between desktops, and change the number of desktops.")
    (license license:bsd-3)))

(define-public xdo
  (package
    (name "xdo")
    (version "0.5.7")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/xdo")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1h3jrygcjjbavdbkpx2hscsf0yf97gk487lzjdlvymd7dxdv9hy9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags
       (list (string-append "CC=" ,(cc-for-target))
             (string-append "PREFIX=" %output))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (inputs
     (list libxcb xcb-util-wm xcb-util))
    (home-page "https://github.com/baskerville/xdo")
    (synopsis "Small X utility to perform elementary actions on windows")
    (description
     "Apply the given action to the given windows.  If no window IDs and no
options are given, the action applies to the focused window.")
    (license license:bsd-2)))

(define-public xeyes
  (package
    (name "xeyes")
    (version "1.3.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.x.org/releases/individual/app/"
                           name "-" version ".tar.xz"))
       (sha256
        (base32 "08rhfp5xlmdbyxkvxhgjxdn6vwzrbrjyd7jkk8b7wi1kpw0ccl09"))))
    (build-system gnu-build-system)
    (inputs
      (list libxext libxi libxmu libxrender libxt))
    (native-inputs
     (list pkg-config))
    (home-page "https://www.x.org/")    ; no dedicated Xeyes page exists
    (synopsis "Follow-the-mouse X demo")
    (description "Xeyes is a demo program for x.org.  It shows eyes
following the mouse.")
    (license license:x11)))


(define-public pixman
  (package
    (name "pixman")
    (version "0.42.2")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append
         "https://www.cairographics.org/releases/pixman-"
         version ".tar.gz"))
       (sha256
        (base32 "0pk298iqxqr64vk3z6nhjwr6vjg1971zfrjkqy5r9zd2mppq057a"))
       (patches
        (search-patches
         "pixman-CVE-2016-5296.patch"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list
        "--disable-static"
        "--enable-timers"
        "--enable-gnuplot")))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libpng zlib))
    (synopsis "Low-level pixel manipulation library")
    (description "Pixman is a low-level software library for pixel
manipulation, providing features such as image compositing and trapezoid
rasterisation.")
    (home-page "http://www.pixman.org/")
    (license license:expat)))

(define-public libdrm
  (package
    (name "libdrm")
    (version "2.4.124")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://dri.freedesktop.org/libdrm/libdrm-"
                    version ".tar.xz"))
              (sha256
               (base32
                "0yd37j3c1qz9gmwvv7vwqgss8aiizypsg8hn9fpsyjnac4zjjdmc"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           (match (%current-system)
             ((or "armhf-linux" "aarch64-linux")
              #~(list "-Dexynos=enabled"
                      "-Domap=enabled"
                      "-Detnaviv=enabled"
                      "-Dtegra=enabled"
                      "-Dfreedreno-kgsl=true"))
             (_ ''()))
           #:phases
           #~(modify-phases %standard-phases
               ;; A typo in a previous upstream commit disabled building
               ;; libdrm_intel by default on supported platforms.  This was
               ;; fixed by the following change in upstream commit
               ;; 8a933c778a0eb36526bf3fc8a289e25add9ff8b0.
               ;; TODO: Remove on next update of libdrm.
               (add-after 'unpack 'build-intel-by-default
                 (lambda _
                   (substitute* "meson.build"
                     (("system\\(\\)\\.startswith")
                      "cpu_family().startswith"))))
               (replace 'check
                 (lambda* (#:key tests? #:allow-other-keys)
                   (when tests?
                     (invoke "meson" "test" "--timeout-multiplier" "5")))))))
    (propagated-inputs
     (list libpciaccess))
    (native-inputs
     (list pkg-config))
    (home-page "https://dri.freedesktop.org/wiki/")
    (synopsis "Direct rendering userspace library")
    (description "The Direct Rendering Infrastructure, also known as the DRI,
is a framework for allowing direct access to graphics hardware under the
X Window System in a safe and efficient manner.  It includes changes to the
X server, to several client libraries, and to the kernel (DRM, Direct
Rendering Manager).  The most important use for the DRI is to create fast
OpenGL implementations providing hardware acceleration for Mesa.
Several 3D accelerated drivers have been written to the DRI specification,
including drivers for chipsets produced by 3DFX, AMD (formerly ATI), Intel
and Matrox.")
    (license license:x11)))

(define-public libdrm-armada-novena
  (package
    (name "libdrm-armada-novena")
    (version "2.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/novena-next/libdrm-armada.git")
                     (commit (string-append "novena/" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d770rxn1zzndzxxh9bcmn2p9yapvmyam0rvyjhv5ngq527ykm4p"))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config automake autoconf libtool))
    (inputs
     (list libdrm))
    (synopsis "libdrm for i.MX6 graphics")
    (description "This package provides an accelerated X server library for
i.MX6.")
    (home-page "https://github.com/novena-next/libdrm-armada")
    (license license:gpl2+)))

(define-public mtdev
  (package
    (name "mtdev")
    (version "1.1.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "http://bitmath.org/code/mtdev/mtdev-"
               version ".tar.bz2"))
        (sha256
         (base32
          "1q700h9dqcm3zl6c3gj0qxxjcx6ibw2c51wjijydhwdcm26v5mqm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       '("--disable-static")
       ,@(if (and (or (target-riscv64?)
                      (target-aarch64?))
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
                                   (string-append "/bin/" file)) "./config-aux"))
                             '("config.guess" "config.sub"))))))
           '())))
    (native-inputs
     (if (and (or (target-riscv64?)
                  (target-aarch64?))
              (%current-target-system))
       (list config)
       '()))
    (home-page "https://bitmath.org/code/mtdev/")
    (synopsis "Multitouch protocol translation library")
    (description "Mtdev is a stand-alone library which transforms all
variants of kernel MT events to the slotted type B protocol.  The events
put into mtdev may be from any MT device, specifically type A without
contact tracking, type A with contact tracking, or type B with contact
tracking.")
    (license license:x11)))

(define-public startup-notification
  (package
    (name "startup-notification")
    (version "0.12")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.freedesktop.org/software/" name
                           "/releases/" name "-" version ".tar.gz"))
       (sha256
        (base32
         "0jmyryrpqb35y9hd5sgxqy2z0r1snw7d3ljw0jak0n0cjdz1yf9w"))))
    (build-system gnu-build-system)
    (arguments
     (if (and (or (target-riscv64?)
                  (target-aarch64?))
              (%current-target-system))
         (list
          #:configure-flags #~(list
                               ;; when cross-compilation, skip realloc checking
                               "lf_cv_sane_realloc=yes"
                               (string-append "PKG_CONFIG=" #$(pkg-config-for-target)))
          #:phases
          #~(modify-phases %standard-phases
              (add-after 'unpack 'update-config-scripts
                (lambda* (#:key inputs native-inputs #:allow-other-keys)
                  ;; Replace outdated config.guess and config.sub.
                  (for-each (lambda (file)
                              (install-file
                               (search-input-file
                                (or native-inputs inputs)
                                (string-append "/bin/" file)) "."))
                            '("config.guess" "config.sub"))))))
         '()))
    (native-inputs (append (if (and (or (target-riscv64?)
                                        (target-aarch64?))
                                    (%current-target-system))
                               (list config)
                               '())
                           (list pkg-config)))
    (inputs
     (list libx11 xcb-util))
    (home-page "https://www.freedesktop.org/wiki/Software/startup-notification/")
    (synopsis "Application startup notification and feedback library")
    (description
     "Startup-notification contains a reference implementation of the startup
notification protocol.  The reference implementation is mostly under an X Window
System style license, and has no special dependencies.")
    ;; Most of the code is provided under x11 license.
    (license license:lgpl2.0+)))

(define-public wmctrl
  (package
    (name "wmctrl")
    (version "1.07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://sites.google.com/site/tstyblo/wmctrl/wmctrl-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1afclc57b9017a73mfs9w7lbdvdipmf9q0xdk116f61gnvyix2np"))
              (patches (search-patches "wmctrl-64-fix.patch"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--mandir="
                            (assoc-ref %outputs "out")
                            "/share/man"))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list libx11 libxmu glib))
    (home-page "http://tomas.styblo.name/wmctrl/")
    (synopsis "Command-line tool to control X window managers")
    (description
     "Wmctrl interacts with an X window manager that is compatible
with the EWMH/NetWM specification.  It can query the window manager for
information, and request for certain window management actions (resize and
move windows, switch between desktops, etc.).")
    (license license:gpl2+)))

(define-public scrot
  (package
    (name "scrot")
    (version "1.11.1")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/resurrecting-open-source-projects/scrot")
         (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0pvp44lb2lrvm11x92vzxwl21hmcbld55kk8wjqa5k1kjg6syj9i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf autoconf-archive automake pkg-config))
    (inputs
     (list giblib
           imlib2
           libbsd
           libx11
           libxcomposite
           libxext
           libxfixes
           libxinerama))
    (home-page "https://github.com/resurrecting-open-source-projects/scrot")
    (synopsis "Command-line screen capture utility for X Window System")
    (description
     "Scrot saves a screenshot of a full screen, a window or a part
of the screen selected by mouse.")
    ;; This license removes a clause about X Consortium from the original
    ;; X11 license.
    (license (license:x11-style "file://COPYING"
                                "See 'COPYING' in the distribution."))))

(define-public slop
  (package
    (name "slop")
    (version "7.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/naelstrof/slop")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1xaka98vka5kh3wmby68ifwi6rp0985dj13fgs96bw8a1z3m1l1d"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f)) ; no "check" target
    (inputs
     (list glew
           glm
           icu4c
           libxext
           libxrender
           mesa))
    (home-page "https://github.com/naelstrof/slop")
    (synopsis "Select a region and print its bounds to stdout")
    (description
     "slop (Select Operation) is a tool that queries for a selection from a
user and prints the region to stdout.  It grabs the mouse and turns it into a
crosshair, lets the user click and drag to make a selection (or click on a
window) while drawing a pretty box around it, then finally prints the
selection's dimensions to stdout.")
    (license license:gpl3+)))

(define-public maim
  (package
    (name "maim")
    (version "5.8.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/naelstrof/maim")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0n5fa0vf33wmw50fhxv5sj94rv1a0m9gbczlgnic2yyak546mmpy"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))            ; no "check" target
    (inputs
     (list glm
           libjpeg-turbo
           libpng
           libxcomposite
           libxfixes
           libxrandr
           libwebp
           mesa
           slop
           zlib))
    (home-page "https://github.com/naelstrof/maim")
    (synopsis "Screenshot utility for X Window System")
    (description
     "maim (Make Image) is a tool that takes screenshots of your desktop and
saves it in any format.  Along with a full screen, it allows you to capture a
predefined region or a particular window.  Also, it makes it possible to
include cursor in the resulting image.")
    (license license:gpl3+)))

(define-public unclutter
  (package
    (name "unclutter")
    (version "8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://ftp.x.org/contrib/utilities/unclutter-"
                    version ".tar.Z"))
              (sha256
               (base32
                "0ahrr5z6wxqqfyihm112hnq0859zlxisrb3y5232zav58j6sfmdq"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (man1 (string-append out "/share/man/man1")))
               (mkdir-p bin)
               (mkdir-p man1)
               (invoke "make" "install" "install.man"
                       (string-append "BINDIR=" bin)
                       (string-append "MANDIR=" man1))))))))
    (inputs (list libx11))
    (home-page "http://ftp.x.org/contrib/utilities/")
    (synopsis "Hide idle mouse cursor")
    (description
     "Unclutter is a program which runs permanently in the background of an
X11 session.  It checks on the X11 pointer (cursor) position every few
seconds, and when it finds it has not moved (and no buttons are pressed
on the mouse, and the cursor is not in the root window) it creates a
small sub-window as a child of the window the cursor is in.  The new
window installs a cursor of size 1x1 but a mask of all 0, i.e. an
invisible cursor.  This allows you to see all the text in an xterm or
xedit, for example.  The human factors crowd would agree it should make
things less distracting.")
    (license license:public-domain)))

(define-public unclutter-xfixes
  (package
    (name "unclutter-xfixes")
    (version "1.5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/Airblader/unclutter-xfixes")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "148m4wx8v57s3l2wb69y9imb00y8ca2li27hsxibwnl1wrkb7z4b"))))
    (build-system gnu-build-system)
    (arguments `(#:tests? #f
                 #:make-flags
                 (list ,(string-append "CC=" (cc-for-target))
                       (string-append "PREFIX=" (assoc-ref %outputs "out")))
                 #:phases
                 (modify-phases %standard-phases
                   (delete 'configure))))
    (inputs
     (list libx11 libev libxfixes libxi))
    (native-inputs
     (list asciidoc pkg-config))
    (home-page "https://github.com/Airblader/unclutter-xfixes")
    (synopsis "Hide idle mouse cursor")
    (description
     "unclutter-xfixes is a rewrite of the popular tool unclutter, but
using the x11-xfixes extension.  This means that this rewrite doesn't
use fake windows or pointer grabbing and hence causes less problems
with window managers and/or applications.

Unclutter is a program which runs permanently in the background of an
X11 session.  It checks on the X11 pointer (cursor) position every few
seconds, and when it finds it has not moved (and no buttons are pressed
on the mouse, and the cursor is not in the root window) it creates a
small sub-window as a child of the window the cursor is in.  The new
window installs a cursor of size 1x1 but a mask of all 0, i.e. an
invisible cursor.  This allows you to see all the text in an xterm or
xedit, for example.  The human factors crowd would agree it should make
things less distracting.")
    (license license:expat)))

(define-public xautomation
  (package
    (name "xautomation")
    (version "1.09")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.hoopajoo.net/static/projects/"
                           "xautomation-" version ".tar.gz"))

       (sha256
        (base32
         "03azv5wpg65h40ip2kk1kdh58vix4vy1r9bihgsq59jx2rhjr3zf"))))
    (build-system gnu-build-system)
    (inputs
     (list libpng libx11 libxi libxtst))
    (native-inputs
     (list xorgproto))
    (synopsis "Tools to automate tasks in X such as detecting on screen images")
    (description
     "Xautomation can control X from the command line for scripts, and
do visual scraping to find things on the screen.  The control interface
allows mouse movement, clicking, button up/down, key up/down, etc, and
uses the XTest extension so you don't have the annoying problems that
xse has when apps ignore sent events.  The visgrep program can find
images inside of images and reports the coordinates, allowing programs
to find buttons, etc, on the screen to click on.")
    (home-page "https://www.hoopajoo.net/projects/xautomation.html")
    (license license:gpl2+)))

(define-public xbanish
  (package
    (name "xbanish")
    (version "1.8")
    (home-page "https://github.com/jcs/xbanish")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page)
                                  (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "12mjwn8hvrrhwyg3wi20bqr6k8d57xf9m5qr3s4nn511dcksh04g"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no tests
       #:make-flags (list ,(string-append "CC=" (cc-for-target))
                          (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases (modify-phases %standard-phases
                  (delete 'configure)))) ; no configure script
    (inputs
     (list libx11 libxfixes libxi libxt))
    (synopsis "Banish the mouse cursor")
    (description
     "@command{xbanish} hides the mouse cursor when you start typing, and
shows it again when the mouse cursor moves or a mouse button is pressed.")
    (license license:bsd-3)))

(define-public xlockmore
  (package
    (name "xlockmore")
    (version "5.72")
    (source (origin
              (method url-fetch)
              (uri (list (string-append "http://sillycycle.com/xlock/"
                                        "xlockmore-" version ".tar.xz")
                         ;; Previous releases are moved to a subdirectory.
                         (string-append "http://sillycycle.com/xlock/"
                                        "recent-releases/"
                                        "xlockmore-" version ".tar.xz")))
              (sha256
               (base32
                "0wn77ri29rhk1w3wik2hpqb8lks1pyqzdk6d583mawxfcxcrf4fm"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:configure-flags
      #~(list (string-append "--enable-appdefaultdir="
                             #$output
                             "/lib/X11/app-defaults"))
      #:tests? #f))                     ;no such thing as a test suite
    (inputs
     (list libx11 libxcrypt libxext libxt linux-pam))
    (home-page "https://sillycycle.com/xlockmore.html")
    (synopsis "Screen locker for the X Window System")
    (description
     "XLockMore is a classic screen locker and screen saver for the
X Window System.")
    (license (license:non-copyleft #f "See xlock.c.")
             ))) ; + GPLv2 in modes/glx/biof.c.

(define-public xtrlock
  (package
    (name "xtrlock")
    (version "2.15")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://debian/pool/main/x/xtrlock/xtrlock_" version
                    ".tar.xz"))
              (sha256
               (base32
                "0mgpysbvipd5h6x6zz4hng6b13gp3qjnpgny3azyj8k8dv85bppw"))))
    (build-system gnu-build-system)
    (arguments
     ;; LDLIBS are required for the package to build.
     ;; CFLAGS are required for it to use the shadow file, and to
     ;; be compatible with multi-touch devices.
     (list #:make-flags
           #~'("CFLAGS=-O2 -g -Wall -DSHADOW_PWD -DMULTITOUCH"
               "LDLIBS=-lX11 -lcrypt -lXi")
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)
               (delete 'check)
               (add-after 'unpack 'rename-makefile
                 (lambda _
                   (rename-file "Makefile.noimake" "Makefile")
                   (rename-file "xtrlock.man" "xtrlock.1")))
               (replace 'install
                 (lambda* (#:key outputs #:allow-other-keys)
                   (let* ((out (assoc-ref outputs "out")))
                     (install-file "xtrlock"
                                   (string-append out "/bin/"))
                     (install-file "xtrlock.1"
                                   (string-append out
                                                  "/share/man/man1/"))))))))
    (inputs (list libx11 libxcrypt libxi libxfixes))
    (home-page "https://packages.debian.org/sid/xtrlock")
    (synopsis "Minimal X display lock program")
    (description
     "xtrlock locks the X server till the user enters their password at the
keyboard.  While xtrlock is running, the mouse and keyboard are grabbed and
the mouse cursor becomes a padlock.  Output displayed by X programs, and
windows put up by new X clients, continue to be visible, and any new output is
displayed normally.  The mouse and keyboard are returned when the user types
their password, followed by Enter or Newline.  If an incorrect password is
entered the bell is sounded.  Pressing Backspace or Delete erases one
character of a password partially typed; pressing Escape or Clear clears
anything that has been entered.")
    (license license:gpl2+)))

(define-public xosd
  (package
    (name "xosd")
    (version "2.2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/libxosd/libxosd/xosd-" version "/"
                    name "-" version ".tar.gz"))
              (sha256
               (base32
                "025m7ha89q29swkc7s38knnbn8ysl24g2h5s7imfxflm91psj7sg"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       (list (string-append "--mandir=" %output "/share/man"))))
    (inputs
     (list libx11 libxt libxext libxinerama))
    (home-page "https://sourceforge.net/projects/libxosd/")
    (synopsis "X On Screen Display")
    (description
     "XOSD provides a C library and a simple utility (osd_cat) for displaying
transparent text on your screen.")
    (license license:gpl2+)))

(define-public wob
  (package
    (name "wob")
    (version "0.15.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/francma/wob/releases/download/"
                           version "/wob-" version ".tar.gz"))
       (sha256
        (base32 "1632y0yr1ig5nihs6kqyvfi117815zszhnfvaabm97qkf5blkj5p"))))
    (build-system meson-build-system)
    (native-inputs
     (list pkg-config scdoc))
    (inputs
     (list libinih libseccomp wayland wayland-protocols))
    (home-page "https://github.com/francma/wob")
    (synopsis "Lightweight overlay bar for Wayland")
    (description
     "Wob, or Wayland Overlay Bar, is a lightweight overlay volume,
backlight, progress, or anything bar for Wayland.")
    (license license:isc)))

(define-public xbindkeys
  (package
    (name "xbindkeys")
    (version "1.8.7")
    (source (origin
              (method url-fetch)
              ;; Download from the savannah mirror list fails
              (uri (string-append
                    "http://www.nongnu.org/xbindkeys/xbindkeys-"
                    version
                    ".tar.gz"))
              (sha256
               (base32
                "1wl2vc5alisiwyk8m07y1ryq8w3ll9ym83j27g4apm4ixjl8d6x2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'sanitise-shebang
           ;; This wish script uses a strange double shebang that escapes our
           ;; patch-shebangs phase.  Assume that it's unnecessary & replace it.
           (lambda _
             (substitute* "xbindkeys_show"
               (("^#!.*|^exec wish.*") "")
               (("^# \\\\") (string-append "#!" (which "wish"))))
             #t))
         (add-after 'unpack 'patch-references
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "xbindkeys_show"
                 (("\"(xbindkeys)\"" _ command)
                  (format #f "\"~a/bin/~a\"" out command)))
               #t))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list guile-2.2 libx11 tk))
    (home-page "https://www.nongnu.org/xbindkeys/")
    (synopsis "Associate a combination of keys with a shell command")
    (description
     "XBindKeys is a program that allows you to launch shell commands with
your keyboard or your mouse under the X Window System.  It links commands to
keys or mouse buttons, using a configuration file.  It's independent of the
window manager and can capture all keyboard keys (ex: Power, Wake...).  It
optionally supports a Guile-based configuration file layout, which enables you
to access all XBindKeys internals, so you can have key combinations, double
clicks or timed double clicks take actions.  Also all functions that work in
Guile will work for XBindKeys.")
    (license license:gpl2+)))

(define-public sxhkd
  (package
    (name "sxhkd")
    (version "0.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/baskerville/sxhkd")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1winwzdy9yxvxnrv8gqpigl9y0c2px27mnms62bdilp4x6llrs9r"))))
    (build-system gnu-build-system)
    (inputs
     (list libxcb xcb-util xcb-util-keysyms xcb-util-wm))
    (arguments
     (list #:phases #~(modify-phases %standard-phases (delete 'configure))
           #:tests? #f  ; no check target
           #:make-flags
           #~(list (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output)
                   ;; Keep the documentation where the build system installs LICENSE.
                   (string-append "DOCPREFIX=" #$output
                                  "/share/doc/" #$name "-" #$version))))
    (home-page "https://github.com/baskerville/sxhkd")
    (synopsis "Simple X hotkey daemon")
    (description "sxhkd is a simple X hotkey daemon with a powerful and
compact configuration syntax.")
    (license license:bsd-2)))

(define-public rxvt-unicode
  (package
    (name "rxvt-unicode")
    (version "9.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://dist.schmorp.de/rxvt-unicode/Attic/"
                                  name "-" version ".tar.bz2"))
              (patches (search-patches "rxvt-unicode-fix-cursor-position.patch"))
              (sha256
               (base32
                "1s3jcvac40zzp03fvmhjsdpsjx0gb1wk54qz74zhzzj9q75kz8da"))))
    (build-system gnu-build-system)
    (arguments
     ;; This sets the destination when installing the necessary terminal
     ;; capability data, which are not provided by 'ncurses'.  See
     ;; https://lists.gnu.org/archive/html/bug-ncurses/2009-10/msg00031.html
     (list #:configure-flags #~(list "--enable-256-color")
           #:make-flags #~(list (string-append "TERMINFO=" #$output "/share/terminfo"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'install 'install-desktop-entries
                 (lambda _
                   (for-each (lambda (exec name)
                               (make-desktop-entry-file
                                (string-append #$output "/share/applications/"
                                               exec ".desktop")
                                #:type "Application"
                                #:name name
                                #:comment '((#f #$(package-synopsis this-package)))
                                #:exec exec
                                #:try-exec exec
                                #:icon "utilities-terminal"
                                #:categories '("System" "TerminalEmulator")))
                             '("urxvt" "urxvtc")
                             '("rxvt-unicode" "rxvt-unicode (client)")))))))
    (inputs
     (list libptytty libxft libx11 libxt libxext))
    (native-inputs
     (list ncurses ;trigger the installation of terminfo data
           perl pkg-config))
    ;; FIXME: This should only be located in 'ncurses'.  Nonetheless it is
    ;; provided for usability reasons.  See <https://bugs.gnu.org/22138>.
    (native-search-paths
     (list (search-path-specification
            (variable "TERMINFO_DIRS")
            (files '("share/terminfo")))))
    (home-page "http://software.schmorp.de/pkg/rxvt-unicode.html")
    (synopsis "Rxvt clone with XFT and unicode support")
    (description "Rxvt-unicode (urxvt) is a colour vt102 terminal emulator
intended as an xterm replacement for users who do not require features such as
Tektronix 4014 emulation and toolkit-style configurability.  It supports
unicode, XFT and may be extended with Perl plugins.  It also comes with a
client/daemon pair that lets you open any number of terminal windows from
within a single process.")
    (license license:gpl3+)))

(define-public xcape
  (package
    (name "xcape")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/alols/xcape")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09a05cxgrip6nqy1qmwblamp2bhknqnqmxn7i2a1rgxa0nba95dm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; no check target
       ;; no configure script
       #:phases (modify-phases %standard-phases (delete 'configure))
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "MANDIR=/share/man/man1"
                          ,(string-append "CC=" (cc-for-target)))))
    (inputs
     (list libxtst libx11))
    (native-inputs
     (list pkg-config))
    (home-page "https://github.com/alols/xcape")
    (synopsis "Use a modifier key in X.org as another key")
    (description
     "This utility for X.org uses a modifier key as another key when
pressed and released on its own.  The default behaviour is to generate the
Escape key when Left Control is pressed and released on its own.")
    (license license:gpl3+)))

(define-public libwacom
  (package
    (name "libwacom")
    (version "2.13.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/linuxwacom/libwacom")
                    (commit (string-append "libwacom-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0i4svxcg606ph72pv9img3gaw3svi827d2c6bdphlxk7cz81x51q"))))
    (build-system meson-build-system)
    (arguments
     (list
      #:configure-flags #~(list "--default-library=shared")))
    (native-inputs
     (list pkg-config
           ;; For tests.
           python
           python-libevdev
           python-pytest
           python-pyudev))
    (inputs
     (list gtk+
           eudev
           libxml2
           python
           python-libevdev
           python-pyudev))
    (propagated-inputs
     ;; libwacom.pc 'Requires' these:
     (list glib libevdev libgudev))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Helper library for graphics tablet settings")
    (description
     "Libwacom is a library to help implement graphics tablet settings.  It is
intended to be used by client-programs that need model identification.  It is
already being used by the gnome-settings-daemon and the GNOME Control Center
Wacom tablet applet.")
    (license license:hpnd)))

(define-public xf86-input-wacom
  (package
    (name "xf86-input-wacom")
    (version "1.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/linuxwacom/xf86-input-wacom/releases/download/"
             "xf86-input-wacom-" version "/"
             "xf86-input-wacom-" version ".tar.bz2"))
       (sha256
        (base32 "0pb8f8fvwm7vy5p3h8nc7x7fb68kpdkscz0j79xzsaxwn8435hhk"))))
    (arguments
     (list #:configure-flags
           #~(list (string-append "--with-sdkdir=" #$output "/include/xorg")
                   (string-append "--with-xorg-conf-dir=" #$output
                                  "/share/X11/xorg.conf.d"))))
    (build-system gnu-build-system)
    (native-inputs (list pkg-config))
    (inputs (list xorg-server libxrandr libxinerama libxi eudev))
    (home-page "https://linuxwacom.github.io/")
    (synopsis "Wacom input driver for X")
    (description "The xf86-input-wacom driver is the wacom-specific X11 input
driver for the X.Org X Server version 1.7 and later (X11R7.5 or later).")
    (license license:x11)))

(define-public redshift
  (package
    (name "redshift")
    (version "1.12")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://github.com/jonls/redshift/"
                       "releases/download/v" version
                       "/redshift-" version ".tar.xz"))
       (sha256
        (base32
         "1fi27b73x85qqar526dbd33av7mahca2ykaqwr7siqiw1qqcby6j"))))
    (build-system gnu-build-system)
    (arguments
     `(#:imported-modules (,@%default-gnu-imported-modules
                           (guix build python-build-system))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'split-outputs
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (gtk (assoc-ref outputs "gtk"))
                   (desktop-file "/share/applications/redshift-gtk.desktop"))
               (mkdir-p (string-append gtk "/bin"))
               (link (string-append out "/bin/redshift-gtk")
                     (string-append gtk "/bin/redshift-gtk"))
               (delete-file (string-append out "/bin/redshift-gtk"))
               (copy-recursively (string-append out "/lib")
                                 (string-append gtk "/lib"))
               (delete-file-recursively (string-append out "/lib"))
               (mkdir-p (string-append gtk "/share/applications"))
               (link (string-append out desktop-file)
                     (string-append gtk desktop-file))
               (delete-file (string-append out desktop-file))
               (with-directory-excursion (string-append out "/share")
                 (for-each (lambda (dir)
                             (copy-recursively
                              (string-append out "/share/" dir)
                              (string-append gtk "/share/" dir))
                             (delete-file-recursively dir))
                           '("appdata" "icons"))))))
         (add-after 'split-outputs 'wrap
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((gtk (assoc-ref outputs "gtk"))
                    (site-packages (@ (guix build python-build-system)
                                      site-packages))
                    (site (site-packages inputs outputs)))
               (wrap-program (string-append gtk "/bin/redshift-gtk")
                 `("GUIX_PYTHONPATH" ":" prefix
                   (,(string-append site ":" (getenv "GUIX_PYTHONPATH"))))
                 `("GI_TYPELIB_PATH" ":" prefix (,(getenv "GI_TYPELIB_PATH"))))))))))
    (outputs '("out" "gtk"))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list bash-minimal
           libdrm
           libx11
           libxcb
           libxxf86vm
           glib ;for Geoclue2 support
           ;; To build the GTK3 GUI, we need these.
           gtk+
           python
           python-pygobject
           python-pyxdg))
    (home-page "https://github.com/jonls/redshift")
    (synopsis "Adjust the color temperature of your screen")
    (description
     "Redshift adjusts the color temperature according to the position of the
sun.  A different color temperature is set during night and daytime.  During
twilight and early morning, the color temperature transitions smoothly from
night to daytime temperature to allow your eyes to slowly adapt.  At night the
color temperature should be set to match the lamps in your room.")
    (license license:gpl3+)))

(define-public redshift-wayland
  (let ((commit "7da875d34854a6a34612d5ce4bd8718c32bec804")
        (revision "1"))
    (package
      (name "redshift-wayland")
      (version (string-append "1.12-"
                              revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/minus7/redshift")
                      (commit commit)))
                (file-name (string-append name "-" version))
                (sha256
                 (base32
                  "0nbkcw3avmzjg1jr1g9yfpm80kzisy55idl09b6wvzv2sz27n957"))))
      (build-system gnu-build-system)
      (arguments
       '(#:phases (modify-phases %standard-phases
                    (add-after 'install 'create-desktop-file
                      (lambda* (#:key outputs #:allow-other-keys)
                        ;; For the GeoClue provider to work, a .desktop file
                        ;; needs to be provided.  A template is available,
                        ;; but it only gets installed when the GUI is enabled.
                        ;; Install it manually for this Wayland variant.
                        (let* ((out (assoc-ref outputs "out"))
                               (desktop-file
                                (string-append
                                 out "/share/applications/redshift.desktop")))
                          (mkdir-p (dirname desktop-file))
                          (copy-file "data/applications/redshift.desktop.in"
                                     desktop-file)
                          (substitute* desktop-file
                            (("^_") ""))
                          #t))))))
      (native-inputs
       (list autoconf automake libtool pkg-config intltool))
      (inputs
       (list libdrm
             libx11
             libxcb
             libxxf86vm
             glib ; for Geoclue2 support
             wayland))
      (home-page "https://github.com/minus7/redshift")
      (synopsis "Adjust the color temperature of your screen (with Wayland support)")
      (description
       "Redshift adjusts the color temperature according to the position of the
sun.  A different color temperature is set during night and daytime.  During
twilight and early morning, the color temperature transitions smoothly from
night to daytime temperature to allow your eyes to slowly adapt.  At night the
color temperature should be set to match the lamps in your room.

This is a fork with added support for Wayland using the wlr-gamma-control
protocol.")
      (license license:gpl3+))))

(define-public xwhite
  (package
    (name "xwhite")
    (version "0.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/derekchuank/xwhite/"
                                  "releases/download/v" version
                                  "/xwhite-" version ".tar.gz"))
              (sha256
               (base32
                "0jbnlj5a91ib4anprmylqqnbv9wa73cr7fsc1s54df0a0w5yq8sz"))))
    (build-system meson-build-system)
    (arguments
     `(#:tests? #f)) ;No test suite.
    (native-inputs (list pkg-config))
    (inputs (list libxrandr))
    (home-page "https://github.com/derekchuank/xwhite")
    (synopsis "Adjust the color balance")
    (description
     "@command{xwhite} is a command line tool for adjusting the colour
balance of screen.  It is based on xrandr's gamma correction and brightness adjustment.
As such, it can only be used for X displays and not Wayland displays.  It is typically
used for tuning the color balance and color temperature.  It has a similar function as
@command{redshift -P -g R:G:B -O temperature}, but @command{xwhite} is more flexible
in that it does not keep the white color fixed, suitable for setting the white color
to an arbitrary balanced color.")
    (license license:gpl2)))

(define-public gammastep
  (package
    (name "gammastep")
    (version "2.0.9")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://gitlab.com/chinstrap/gammastep")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rcciccnwhxh97wlr9gcirdxv33za369jsrgrfzcp3042824pm8i"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'install 'wrap-python-and-typelib
            (lambda _
              ;; Gammastep GUI needs Typelib files from GTK and access to
              ;; Python libraries.
              (wrap-program (string-append #$output "/bin/gammastep-indicator")
                `("PYTHONPATH" ":" prefix (,(getenv "GUIX_PYTHONPATH")))
                `("GI_TYPELIB_PATH" ":" prefix
                  (,(getenv "GI_TYPELIB_PATH")))))))))
    (native-inputs
     (list autoconf
           automake
           gettext-minimal
           intltool
           libtool
           pkg-config))
    (inputs
     (list bash-minimal
           glib
           gtk+
           libappindicator
           libdrm
           libx11
           libxxf86vm
           libxcb
           python
           python-pygobject
           python-pyxdg
           wayland))
    (home-page "https://gitlab.com/chinstrap/gammastep")
    (synopsis "Adjust the color temperature of your screen")
    (description
     "Gammastep automatically adjusts the color temperature of your
screen according to your surroundings.  This may help your eyes hurt
less if you are working in front of the screen at night.")
    (license license:gpl3)))

(define-public xscreensaver
  (package
    (name "xscreensaver")
    (version "6.08")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.jwz.org/xscreensaver/xscreensaver-"
                       version ".tar.gz"))
       (sha256
        (base32 "18vnbs2ns42cgnnsvwn0zh98wcfzxf2k9mib5x5zkv6f4njjpxaw"))
       (modules '((guix build utils)))
       (snippet
        ;; 'configure.ac' checks for $ac_unrecognized_opts and exits if it's
        ;; non-empty.  Since the default 'configure' phases passes options
        ;; that may or may not be recognized, such as '--enable-fast-install',
        ;; these need to be tolerated, hence this snippet.
        '(substitute* "configure"
           (("exit 2") "true")))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f                      ; no check target
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'adjust-gtk-resource-paths
           (lambda _
             (substitute* '("driver/Makefile.in" "po/Makefile.in.in")
               (("@GTK_DATADIR@") "@datadir@")
               (("@PO_DATADIR@") "@datadir@"))
             #t)))
       #:configure-flags '("--with-pam"

                           ;; Don't check /proc/interrupts in the build
                           ;; environment to avoid non-deterministic failures
                           ;; of the 'configure' script.
                           "--without-proc-interrupts"

                           "--without-readdisplay")
       #:make-flags (list (string-append "AD_DIR="
                                         (assoc-ref %outputs "out")
                                         "/lib/X11/app-defaults"))))
    (native-inputs
     (list pkg-config intltool))
    (inputs
     (list libx11
           libxcrypt
           libxext
           libxi
           libxt
           libxft
           libxmu
           libxpm
           libglade
           libxml2
           libsm
           libjpeg-turbo
           linux-pam
           pango
           gtk+
           perl
           cairo
           bc
           libxrandr
           glu
           `(,glib "bin")))
    (home-page "https://www.jwz.org/xscreensaver/")
    (synopsis "Classic screen saver suite supporting screen locking")
    (description
     "xscreensaver is a popular screen saver collection with many entertaining
demos.  It also acts as a nice screen locker.")
    ;; xscreensaver doesn't have a single copyright file and instead relies on
    ;; source comment headers, though most files have the same lax
    ;; permissions.  To reduce complexity, we're pointing at Debian's
    ;; breakdown of the copyright information.
    (license (license:non-copyleft
              (string-append
               "http://metadata.ftp-master.debian.org/changelogs/"
               "/main/x/xscreensaver/xscreensaver_5.36-1_copyright")))))

(define-public xssproxy
  (package
    (name "xssproxy")
    (version "1.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/vincentbernat/xssproxy")
                    (commit (string-append "v" version))))
              (file-name (string-append name "-" version "-checkout"))
              (sha256
               (base32
                "19i5npmbd2kbs96wp2bb79crlwkcf9n0n8cd4v1myvczb0iwbz1q"))))
    (build-system gnu-build-system)
    (arguments `(#:make-flags `("bindir=/bin"
                                "man1dir=/share/man/man1"
                                ,(string-append "DESTDIR=" (assoc-ref %outputs "out"))
                                ,,(string-append "CC=" (cc-for-target)))
                 #:phases (modify-phases %standard-phases
                            (delete 'configure)
                            (delete 'check))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list glib libx11 libxext libxscrnsaver dbus))
    (synopsis "Forward freedesktop.org Idle Inhibition Service calls to Xss")
    (description "xssproxy implements the @code{org.freedesktop.ScreenSaver}
D-Bus interface described in the Idle Inhibition Service Draft by the
freedesktop.org developers.  The inhibition of the screensaver is then
controlled using the XScreenSaverSuspend function from the Xss (X11 Screen
Saver extension) library.")
    (home-page "https://github.com/timakro/xssproxy")
    (license license:expat)))

(define-public xsel
  ;; The 1.2.0 release no longer compiles with GCC 8 and upper, see:
  ;; https://github.com/kfish/xsel/commit/d88aa9a8dba9228e6780d6bb5a5720a36f854918.
  (let ((commit "062e6d373537c60829fa9b5dcddbcd942986b3c3")
        (revision "1"))
    (package
      (name "xsel")
      (version (git-version "1.2.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/kfish/xsel")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0fbf80zsc22vcqp59r9fdx4icxhrkv7l3lphw83326jrmkzy6kri"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (replace 'bootstrap
             (lambda _
               (invoke "autoreconf" "-vfi"))))))
      (native-inputs (list autoconf automake libtool))
      (inputs
       (list libxt))
      (home-page "https://www.vergenet.net/~conrad/software/xsel/")
      (synopsis "Manipulate X selection and/or clipboard from the command line")
      (description
       "XSel is a command-line program for getting and setting the contents of
the X selection and/or clipboard.  Normally this is only accessible by manually
highlighting information and pasting it with the middle mouse button.

XSel reads from standard input and writes to standard output by default, but can
also follow a growing file, display contents, delete entries and more.")
      (license (license:x11-style "file://COPYING"
                                  "See COPYING in the distribution.")))))

(define-public xdpyprobe
  (package
    (name "xdpyprobe")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alezost/" name
                                  "/releases/download/v" version
                                  "/" name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h09wd2qcg08rj5hcakvdh9q01hkrj8vxly94ax3ch2x06lm0zq8"))))
    (build-system gnu-build-system)
    (inputs
     (list libx11))
    (home-page "https://github.com/alezost/xdpyprobe")
    (synopsis "Probe X server for connectivity")
    (description
     "Xdpyprobe is a tiny C program whose only purpose is to probe a
connectivity of the X server running on a particular @code{DISPLAY}.")
    (license license:gpl3+)))

(define-public ulauncher
  (package
    (name "ulauncher")
    (version "6.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Ulauncher/Ulauncher")
                    (commit "1e68d47473f8e77d375cb4eca644c3cda68ed7e9")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1c2czlrsf5aq8c88qliqbnqvf04q9cnjc1j6hivqa0w260mzjll1"))))
    (build-system python-build-system)
    (arguments
     (list #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-libX11
                          (lambda* (#:key inputs #:allow-other-keys)
                            (substitute* "ulauncher/utils/xinit.py"
                              (("libX11.so.6")
                               (search-input-file inputs "/lib/libX11.so")))))
                        (add-after 'unpack 'fix-usr
                          (lambda _
                            (substitute* "setup.py"
                              (("\\{sys.prefix\\}")
                               (string-append #$output)))))
                        (add-after 'unpack 'fix-os-release
                          (lambda _
                            (define (touch file)
                              (call-with-output-file file
                                (const #t)))
                            (let* ((hard-path "/etc/os-release")
                                   (fixed-path (string-append #$output
                                                              hard-path)))
                              ;; Make it relative
                              ;; Update hardcoded path to something
                              ;; within the build enviroment.
                              (substitute* "ulauncher/utils/environment.py"
                                ((hard-path)
                                 fixed-path))
                              ;; Create directory for the dummy file.
                              (mkdir-p (string-append #$output "/etc"))
                              (touch fixed-path))))
                        (add-before 'check 'env-setup
                          ;; The test require access to home to put temporary files.
                          (lambda _
                            (setenv "HOME"
                                    (getcwd)))))))
    (native-inputs (list intltool python-distutils-extra python-mock))
    (inputs (list libx11 python-levenshtein python-pycairo))
    (propagated-inputs (list keybinder libwnck gsettings-desktop-schemas
                             python-pygobject webkitgtk-with-libsoup2))
    (home-page "https://ulauncher.io")
    (synopsis "Application launcher for Linux")
    (description
     "Ulauncher is a fast application launcher for Linux.  It is written in
Python, using GTK+, and features: App Search (fuzzy matching), Calculator,
Extensions, Shortcuts, File browser mode and Custom Color Themes.")
    (license license:gpl3+)))

(define-public rofi
  (package
    (name "rofi")
    (version "1.7.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/davatorium/rofi/"
                                  "releases/download/"
                                  version "/rofi-" version ".tar.xz"))
              (sha256
               (base32
                "0pk5a38rhci6mm0p9zjrmb7ixczhbdwqirw840h682rf9660mn9a"))
              (snippet
               #~(begin
                   ;; Delete pre-generated files.
                   (for-each delete-file
                             (list "lexer/theme-lexer.c"
                                   "lexer/theme-parser.c"
                                   "lexer/theme-parser.h"
                                   "resources/resources.c"
                                   "resources/resources.h"))))))
    (build-system gnu-build-system)
    (native-inputs
     (list bison
           check
           flex
           `(,glib "bin")
           pkg-config))
    (inputs
     (list libjpeg-turbo
           (librsvg-for-system)
           libxcb
           libxkbcommon
           startup-notification
           xcb-util
           xcb-util-cursor
           xcb-util-keysyms
           xcb-util-wm
           xcb-util-xrm))
    (propagated-inputs
     (list cairo glib pango))           ;in Requires.private of rofi.pc
    (native-search-paths
     ;; This is where rofi will search for plugins by default.
     (list (search-path-specification
            (variable "ROFI_PLUGIN_PATH")
            (files '("lib/rofi")))))
    (arguments
     `(#:parallel-tests? #f             ; fails in some circumstances
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'adjust-tests
           (lambda _
             (substitute* '("test/helper-expand.c")
               (("~root") "/root")
               (("~") "")
               (("g_get_home_dir\\(\\)") "\"/\"")))))))
    (home-page "https://github.com/davatorium/rofi")
    (synopsis "Application launcher")
    (description "Rofi is a minimalist application launcher.  It memorizes which
applications you regularly use and also allows you to search for an application
by name.")
    (license license:expat)))

(define-public rofi-wayland
  (let ((base rofi))
    (package
      (inherit rofi)
      (name "rofi-wayland")
      (version "1.7.8+wayland1")
      (source (origin
                (method url-fetch)
                (uri (string-append "https://github.com/lbonn/rofi"
                                    "/releases/download/" version
                                    "/rofi-" version ".tar.xz"))
                (sha256
                 (base32
                  "0wr6qdyd9wkgqaa4vq8czz4fd2shngbw83b2ll284ahm3mwhq2da"))))
      (build-system meson-build-system)
      (inputs
       (modify-inputs (package-inputs base)
         (append wayland wayland-protocols)))
      (description
       "Rofi is a minimalist application launcher.  It memorizes which
applications you regularly use and also allows you to search for an
application by name.  This package, @code{rofi-wayland}, provides additional
wayland support."))))

(define-public rofi-calc
  (package
    (name "rofi-calc")
    (version "2.3.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/svenstaro/rofi-calc")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "041i50rbk7cdbrmn43hz4kx4ijdzff4pw1jv2symwfn07z9a6f30"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          ;; Don't try to install directly to rofi, instead install
          ;; to lib/rofi to match rofi's search-path ROFI_PLUGIN_PATH.
          (add-after 'unpack 'patch-plugindir
            (lambda _
              (substitute* "Makefile.am"
                (("plugindir=\\$\\{rofi_PLUGIN_INSTALL_DIR\\}\\/")
                 "plugindir=${libdir}/rofi/"))))
          (add-after 'unpack 'patch-qalc-path
            (lambda* (#:key inputs #:allow-other-keys)
              (substitute* "src/calc.c"
                (("\"qalc\"")
                 (string-append "\""
                                (search-input-file inputs "bin/qalc")
                                "\""))))))))
    (inputs
     (list cairo libqalculate rofi))
    (native-inputs
     (list autoconf automake libtool pkg-config))
    (home-page
     "https://github.com/svenstaro/rofi-calc")
    (synopsis "Do live calculations in rofi with qalc")
    (description
     "@code{rofi-calc} is a rofi plugin that uses qalculate's @code{qalc} to parse
natural language input and provide results.")
    (license license:expat)))

(define-public tint2
  (package
    (name "tint2")
    (version "17.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://gitlab.com/o9000/" name "/"))
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "123apmgs6x2zfv1q57dyl4mwqf0vsw5ndh5jsg6p3fvhr66l1aja"))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                      ;no test target
    (inputs
     (list gtk+
           imlib2
           (librsvg-for-system)
           libxcomposite
           libxdamage
           libxft
           libxinerama
           libxrandr
           startup-notification))
    (native-inputs
     (list gettext-minimal pkg-config))
    (home-page "https://gitlab.com/o9000/tint2")
    (synopsis "Lightweight task bar")
    (description
     "Tint2 is a simple task bar made for modern X window managers.  It was
specifically made for Openbox but it should also work with other window
managers (GNOME, KDE, XFCE etc.).

The taskbar includes transparency and color settings for the font, icons,
border, and background.  It also supports multihead setups, customized mouse
actions, a built-in clock, a battery monitor and a system tray.")
    (license license:gpl2)))

(define-public stalonetray
  (package
    (name "stalonetray")
    (version "0.8.5")
    (source
     (origin
       (method git-fetch)
       (uri
        (git-reference
         (url "https://github.com/kolbusa/stalonetray")
         (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "074wy1xppfycillbxq6fwrq87ik9glc95083df5vgm20mhzni7pz"))))
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-docbook-root
            (lambda _
              (substitute* "configure.ac"
                (("AC_SUBST\\(DOCBOOK_ROOT\\)" all)
                 (string-append "DOCBOOK_ROOT="
                                #$(this-package-native-input "docbook-xsl")
                                "/xml/xsl/docbook-xsl-"
                                #$(package-version (this-package-native-input "docbook-xsl"))
                                "; " all))))))))
    (inputs (list libx11 libxpm))
    (native-inputs (list autoconf automake docbook-xsl libxslt))
    (build-system gnu-build-system)
    (home-page "https://kolbusa.github.io/stalonetray")
    (synopsis "Standalone freedesktop.org and KDE systray implementation")
    (description
     "Stalonetray is a stand-alone freedesktop.org and KDE system
tray (notification area) for X Window System/X11 (e.g. X.Org or XFree86).  It
has full XEMBED support and minimal dependencies: an X11 lib only.  Stalonetray
works with virtually any EWMH-compliant window manager.")
    (license license:gpl2+)))

(define-public tofi
  (package
    (name "tofi")
    (version "0.9.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/philj56/tofi")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1paknsgfsgan27lqwhb2ndsk4gi8ciq9r49b0fpbbdwxk7ljk2cn"))
              (patches (search-patches "tofi-32bit-compat.patch"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config scdoc))
    (inputs (list cairo
                  harfbuzz
                  libxkbcommon
                  pango
                  wayland
                  wayland-protocols))
    (home-page "https://github.com/philj56/tofi")
    (synopsis "Application launcher for Wayland")
    (description
     "Tofi is a Dmenu and Rofi replacement for wlroots-based Wayland
compositors such as Sway.")
    (license license:expat)))

(define-public dzen
  (let ((commit "488ab66019f475e35e067646621827c18a879ba1")
        (revision "1"))
    (package
     (name "dzen")
     (version (string-append "0.9.5-" ; Taken from `config.mk`.
                             revision "." (string-take commit 7)))
     (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/robm/dzen")
                    (commit commit)))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0y47d6ii87vf4a517gi4fh0yl06f8b085sra77immnsasbq9pxnw"))))
     (build-system gnu-build-system)
     (arguments
      `(#:tests? #f ; No test suite.
        #:make-flags ; Replacement for `config.mk`.
        (list
         (string-append "VERSION = " ,version)
         (string-append "PREFIX = " %output)
         "MANPREFIX = ${PREFIX}/share/man"
         "INCS = -I."
         "LIBS = -lc -lX11 -lXinerama -lXpm $(shell pkg-config --libs xft)"
         "CFLAGS = -Wall -Os ${INCS} -DVERSION=\\\"${VERSION}\\\"\
         -DDZEN_XINERAMA -DDZEN_XPM -DDZEN_XFT $(shell pkg-config --cflags xft)"
         "LDFLAGS = ${LIBS}"
         "CC = gcc"
         "LD = ${CC}")
        #:phases
        (modify-phases %standard-phases
          (delete 'configure) ; No configuration script.
          ;; Use own make-flags instead of `config.mk`.
          (add-before 'build 'dont-include-config-mk
            (lambda _
              (substitute* "Makefile" (("include config.mk") ""))
              #t)))))
     (inputs
      (list libx11 libxft libxpm libxinerama))
     (native-inputs (list pkg-config))
     (synopsis "General purpose messaging, notification and menuing program for X11")
     (description "Dzen is a general purpose messaging, notification and menuing
program for X11.  It was designed to be fast, tiny and scriptable in any language.")
     (home-page "https://github.com/robm/dzen")
     (license license:expat))))

(define-public xftwidth
  (package
    (name "xftwidth")
    (version "20170402")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
	     (url "http://github.com/vixus0/xftwidth")
	     (commit "35ff963908d41a8a6a7101c434c88255728025ee")))
       (sha256
	(base32
	 "1jwl25785li24kbp0m1wxfwk4dgxkliynn03nsj813cjr34kq16h"))
       (file-name (string-append name "-" version "-checkout"))))
    (build-system gnu-build-system)
    (inputs `(("freetype" ,freetype)
              ("libx11" ,libx11)
              ("fontconfig" ,fontconfig)
              ("libxft" ,libxft)))
    (native-inputs
     (list pkg-config))
    (arguments
     '(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'fix-makefile ; /usr/bin doesn't show up in PATH
           (lambda _ (substitute* "Makefile" (("usr/") "")) #t))
         (delete 'check) ; no check included in Makefile
         (delete 'configure))
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "DESTDIR=" out)))))
    (home-page "https://github.com/vixus0/xftwidth")
    (synopsis "Calculator for determining pixel widths of displayed text using Xft fonts")
    (description "xftwidth is a small C program for calculating the pixel
widths of displayed text using Xft fonts.  It is especially useful in scripts
for displaying text in graphical panels, menus, popups, and notification
windows generated using dzen.  These scripts are often used in conjunction
with minimalistic tiling window managers such as herbstluftwm and bspwm.")
    (license license:expat)))

(define-public xcb-util-xrm
  (package
    (name "xcb-util-xrm")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/Airblader/xcb-util-xrm/releases"
                    "/download/v" version "/xcb-util-xrm-" version ".tar.bz2"))
              (sha256
               (base32
                "118cj1ybw86pgw0l5whn9vbg5n5b0ijcpx295mwahzi004vz671h"))
              (modules '((guix build utils)))
              (snippet
               ;; Drop bundled m4.
               '(begin
                  (delete-file-recursively "m4")
                  #t))))
    (build-system gnu-build-system)
    (native-inputs
     (list pkg-config m4 libx11)) ; for tests
    (inputs
     (list libxcb xcb-util libx11))
    (home-page "https://github.com/Airblader/xcb-util-xrm")
    (synopsis "XCB utility functions for the X resource manager")
    (description
     "The XCB util module provides a number of libraries which sit on
top of libxcb, the core X protocol library, and some of the extension
libraries.  These experimental libraries provide convenience functions
and interfaces which make the raw X protocol more usable.  Some of the
libraries also provide client-side code which is not strictly part of
the X protocol but which has traditionally been provided by Xlib.

XCB util-xrm module provides the following libraries:

- xrm: utility functions for the X resource manager.")
    (license license:x11)))

(define-public xcalib
  (package
    (name "xcalib")
    (version "0.10")
    (home-page "https://github.com/OpenICC/xcalib")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "05fzdjmhiafgi2jf0k41i3nm0837a78sb6yv59cwc23nla8g0bhr"))
              (patches
               (list
                ;; Add missing documentation for the new --output option.
                ;; This upstream patch can be removed on the next update.
                (origin
                  (method url-fetch)
                  (uri (string-append
                        home-page "/commit/"
                        "ae03889b91fe984b18e925ad2b5e6f2f7354e058.patch"))
                  (file-name "xcalib-update-man-page.patch")
                  (sha256
                   (base32
                    "0f7b4d5484x4b9n1bwhqmar0kcaa029ffff7bp3xpr734n1qgqb6")))))))
    (build-system cmake-build-system)
    (arguments
     '(#:tests? #f))                    ; no test suite
    (inputs (list libx11 libxext libxrandr libxxf86vm))
    (synopsis "Tiny monitor calibration loader for XFree86 (or X.org)")
    (description "xcalib is a tiny tool to load the content of vcgt-Tags in ICC
profiles to the video card's gamma ramp.  It does work with most video card
drivers except the generic VESA driver.  Alter brightness, contrast, RGB, and
invert colors on a specific display/screen.")
    (license license:gpl2)))

(define-public nxbelld
  (package
    (name "nxbelld")
    (version "0.1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/dusxmt/nxbelld")
                    (commit version)))
              (sha256
               (base32
                "04qwhmjs51irinz5mjlxdb3dc6vr79dqmc5fkj80x1ll3ylh5n3z"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments '(#:configure-flags `("--enable-sound"
                                     "--enable-wave"
                                     "--enable-alsa")))
    (native-inputs (list autoconf automake pkg-config perl))
    (inputs (list libx11 alsa-lib))
    (synopsis "Daemon that performs an action every time the X11 bell is rung")
    (description "nxbelld is a tiny utility to aid people who either don't
like the default PC speaker beep, or use a sound driver that doesn't have
support for the PC speaker.  The utility performs a given action every time
the X bell is rung.  The actions nxbelld can currently perform include running
a specified program, emulating the PC speaker beep using the sound card (default),
or playing a PCM encoded WAVE file.")
    (home-page "https://github.com/dusxmt/nxbelld")
    (license license:gpl3+)))

(define-public xautolock
  (package
    (name "xautolock")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://www.ibiblio.org/pub/linux/X11/"
                                  "screensavers/xautolock-" version ".tgz"))
              (sha256
               (base32
                "18jd3k3pvlm5x1adyqw63z2b3f4ixh9mfvz9asvnskk3fm8jgw0i"))))
    (build-system gnu-build-system)
    (native-inputs
     (list imake))
    (inputs
     (list libx11 libxext libxscrnsaver))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((imake (assoc-ref inputs "imake"))
                   (out   (assoc-ref outputs "out")))
               ;; Generate Makefile
               (invoke "xmkmf")
               (substitute* "Makefile"
                 ;; These imake variables somehow remain undefined
                 (("DefaultGcc2[[:graph:]]*Opt") "-O2")
                 ;; Reset a few variable defaults that are set in imake templates
                 ((imake) out)
                 (("(MANPATH = )[[:graph:]]*" _ front)
                  (string-append front out "/share/man")))
               ;; Old BSD-style 'union wait' is unneeded (defining
               ;; _USE_BSD did not seem to fix it)
               (substitute* "src/engine.c"
                 (("union wait  status") "int status = 0"))
               #t)))
         (add-after 'install 'install/man
           (lambda _
             (invoke "make" "install.man"))))))
    (home-page "https://ibiblio.org/pub/Linux/X11/screensavers/")
    (synopsis "Program launcher for idle X sessions")
    (description "Xautolock monitors input devices under the X Window
System, and launches a program of your choice if there is no activity after
a user-configurable period of time.")
    (license license:gpl2)))

(define-public screen-message
  (package
    (name "screen-message")
    (version "0.26")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://www.joachim-breitner.de/archive/screen-message"
                    "/screen-message-" version ".tar.gz"))
              (sha256
               (base32
                "0dwgm4z3dfk6xz41w8xiv0hmnwr74gf3ykb91b090hc4ffwsf4mw"))))
    (build-system gnu-build-system)
    (inputs `(("gtk3" ,gtk+)
              ("gdk" ,gdk-pixbuf)
              ("pango" ,pango)))
    (native-inputs (list pkg-config))
    (arguments
     ;; The default configure puts the 'sm' binary in games/ instead of bin/ -
     ;; this fixes it:
     `(#:make-flags (list (string-append "execgamesdir=" %output "/bin"))))
    (synopsis "Print messages on your screen")
    (description "@code{screen-message} is a tool for displaying text on
your screen.  It will make the text as large as possible and display it
with black color on a white background (colors are configurable on the
commandline).")
    (home-page "https://www.joachim-breitner.de/projects#screen-message")
    (license license:gpl2+)))

(define-public xss-lock
  ;; xss-lock does not seem to be maintained any longer, but the last commits
  ;; fix important issues so we package them.
  (let ((version "0.3.0")
        (revision "1")
        (commit "1e158fb20108058dbd62bd51d8e8c003c0a48717"))
    (package
      (name "xss-lock")
      (version (git-version version revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://bitbucket.org/raymonad/xss-lock.git")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "10hx7k7ga8g08akwz8qrsvj8iqr5nd4siiva6sjx789jvf0sak7r"))))
      (build-system cmake-build-system)
      (inputs (list glib xcb-util))
      (native-inputs
       (list python-docutils pkg-config))
      (arguments
       `(#:tests? #f))
      (synopsis "Use external screen locker on events")
      (description "@code{xss-lock} listens to X signals to fire up a
user-defined screensaver.  In effect this automatically locks the
screen when closing a laptop lid or after a period of user inactivity (as set
with @code{xset s TIMEOUT}).  The notifier command, if specified, is executed
first.  Additionally, xss-lock uses the inhibition logic to lock the screen
before the system goes to sleep.")
      (home-page "https://bitbucket.org/raymonad/xss-lock")
      (license license:expat))))

(define-public physlock
  (package
    (name "physlock")
    (version "13")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/xyb3rt/physlock")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1mz4xxjip5ldiw9jgfq9zvqb6w10bcjfx6939w1appqg8f521a7s"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:phases
           #~(modify-phases %standard-phases
               (delete 'configure)

               (add-after 'unpack 'fix-makefile
                 (lambda _
                   (substitute* "main.c" ; remove extra newline in the prompt
                     (("(fprintf.vt.ios, .%s.)\\n(., options->prompt)" all start end)
                      (string-append start end)))
                   (substitute* "Makefile" (("-m 4755 -o root -g root") "")))))

           #:make-flags
           #~(list "HAVE_SYSTEMD=0" "HAVE_ELOGIND=1"
                   (string-append "CC=" #$(cc-for-target))
                   (string-append "PREFIX=" #$output))))
    (native-inputs (list linux-pam elogind))
    (synopsis "Screen lock utility")
    (description
     "@command{physlock} locks all virtual terminals at once, only allowing the
user of the active session (the user logged into the foreground virtual
terminal) to unlock the computer.  It is an alternative to @command{vlock -an},
written to overcome vlock's limitations regarding hibernate and suspend.")
    (home-page "https://github.com/xyb3rt/physlock")
    (license license:gpl2+)))

(define-public python-pyclip
  (package
    (name "python-pyclip")
    (version "0.7.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/spyoungtech/pyclip")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "10q698snlid09jmlvsqlljj2r0w6cixncjcgbhnjqp7w0hva8wyj"))))
    (build-system pyproject-build-system)
    (arguments
     (list
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'check 'start-xorg-server
            (lambda _
              (system "Xvfb :1 &")
              (setenv "DISPLAY" ":1"))))))
    (native-inputs
     (list python-pytest
           python-setuptools
           python-wheel
           xorg-server-for-tests))
    (propagated-inputs
     (list wl-clipboard
           xclip))
    (home-page "https://github.com/spyoungtech/pyclip")
    (synopsis "Cross-platform clipboard module for Python")
    (description
     "This library implements cross-platform clipboard utilities supporting
both binary and text data.")
    (license license:asl2.0)))

(define-public python-pyperclip
  (package
    (name "python-pyperclip")
    (version "1.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyperclip" version))
        (sha256
         (base32
          "0mxzm43z2anr55gyz7awagvam4d5c2rlxhp9hjyg0d29n2l58lhh"))))
    (build-system python-build-system)
    (arguments
     '(#:tests? #f)) ; Not clear how to make tests pass.
    (inputs
     (list xclip xsel))
    (home-page "https://github.com/asweigart/pyperclip")
    (synopsis "Python clipboard module")
    (description
     "Pyperclip is a clipboard module for Python, handling copy/pasting from
the X11 clipboard")
    (license license:bsd-3)))

(define-public numlockx
  (package
    (name "numlockx")
    (version "1.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    ;; It seems that upstream is gone.
                    (url "https://github.com/rg3/numlockx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1w49fayhwzn5rx0z1q2lrvm7z8jrd34lgb89p853a024bixc3cf2"))))
    (build-system gnu-build-system)
    (inputs
     (list xorg-server))
    (home-page "https://github.com/rg3/numlockx")
    (synopsis "Turns on the numlock key in X11")
    (description "@command{numlockx} is a tiny program that lets you turn on
the numlock key in X11.  It can be called from the user's initialization files
to automatically turn it on on login.")
    (license license:expat)))

(define-public xrandr-invert-colors
  (package
    (name "xrandr-invert-colors")
    (version "0.02")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zoltanp/xrandr-invert-colors")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gk1fgxb2kjyr78xn8m0ckjdic99ras7msa67piwnhj3j4scg1ih"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f ; there are none
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           ;; It's simpler to install the single binary ourselves than to patch
           ;; the Makefile's install target into working.
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin")))
               (mkdir-p bin)
               (copy-file "xrandr-invert-colors.bin"
                          (string-append bin "/xrandr-invert-colors"))))))))
    (inputs
     (list libxrandr))
    (home-page "https://github.com/zoltanp/xrandr-invert-colors")
    (synopsis "Invert display colors")
    (description "This package provides a small utility for inverting the
colors on all monitors attached to an XRandR-capable X11 display server.")
    (license license:gpl3+)))

(define-public sct
  (package
    (name "sct")
    (version "0.5")
    (source
     (origin
       (method url-fetch)
       (uri
        (string-append "https://www.umaxx.net/dl/sct-"
                       version ".tar.gz"))
       (sha256
        (base32 "0lrhx771iccbw04wrhj0ygids1pzmjfc4hvklm30m3p3flvhqf0m"))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f                      ; no test suite
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'fix-sctd-paths
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (coreutils (assoc-ref inputs "coreutils"))
                   (inetutils (assoc-ref inputs "inetutils"))
                   (sed (assoc-ref inputs "sed")))
               (substitute* "sctd.sh"
                 (("\\$\\(which sct\\)") (string-append out "/bin/sct"))
                 (("date") (string-append coreutils "/bin/date"))
                 (("printf") (string-append coreutils "/bin/printf"))
                 (("sleep") (string-append coreutils "/bin/sleep"))
                 (("logger") (string-append inetutils "/bin/logger"))
                 (("sed") (string-append sed "/bin/sed"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "sct" (string-append out "/bin"))
               (install-file "sctd.sh" (string-append out "/bin"))
               (install-file "sct.1" (string-append out "/man/man1"))
               (install-file "sctd.1" (string-append out "/man/man1"))
               (rename-file (string-append out "/bin/sctd.sh")
                            (string-append out "/bin/sctd"))
               #t))))))
    (inputs
     (list coreutils ; sctd uses "date", "printf" and "sleep"
           inetutils ; sctd uses "logger"
           libxrandr
           sed)) ; sctd uses "sed"
    (home-page "https://www.umaxx.net")
    (synopsis "Set the color temperature of the screen")
    (description "@code{sct} is a lightweight utility to set the color
temperature of the screen.")
    (license (license:non-copyleft "file://sct.c")))) ; "OpenBSD" license

(define-public xsecurelock
  (package
    (name "xsecurelock")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/google/xsecurelock/releases"
                    "/download/v" version "/xsecurelock-" version ".tar.gz"))
              (sha256
               (base32 "1i7vhzysirr5kra15vd501b79k0jgs11lkb9ck3hx6vicxm204d3"))))
    (build-system gnu-build-system)
    (arguments
     '(#:configure-flags
       '("--with-pam-service-name=login"
         "--with-xkb"
         "--with-default-authproto-module=/run/privileged/bin/authproto_pam")))
    (native-inputs
     (list pandoc pkg-config))
    (inputs
     (list fontconfig
           libx11
           libxcomposite
           libxext
           libxfixes
           libxft
           libxmu
           libxrandr
           libxscrnsaver
           linux-pam))
    (home-page "https://github.com/google/xsecurelock")
    (synopsis "X11 screen lock utility with the primary goal of security")
    (description "@code{xsecurelock} is an X11 screen locker which uses
a modular design to avoid the usual pitfalls of screen locking utility design.

As a consequence of this design, you shouldn't use the usual screen locker
service with @code{xsecurelock}.  Instead, add a helper binary to your
@code{operating-system}'s @code{privileged-programs} field:

@example
(privileged-programs
 (cons*
  (privileged-program
   (program (file-append xsecurelock \"/libexec/xsecurelock/authproto_pam\"))
   (setuid? #t))
  %default-privileged-programs))
@end example")
    (license license:asl2.0)))

(define-public wl-color-picker
  (package
    (name "wl-color-picker")
    (version "1.3")
    (home-page "https://github.com/jgmdev/wl-color-picker")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url home-page)
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0h5b8qfwri7a1invk8dran3436ac37x6r8fic3l5cxqj5rgnky4n"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan '(("wl-color-picker.sh" "bin/wl-color-picker")
                        ("wl-color-picker.png" "share/pixmaps/")
                        ("wl-color-picker.svg"
                         "share/icons/hicolor/scalable/apps/")
                        ("wl-color-picker.desktop" "share/applications/"))
       #:phases (modify-phases %standard-phases
                  (add-after 'install 'wrap-script
                    (lambda* (#:key outputs #:allow-other-keys)
                      (wrap-program (string-append (assoc-ref outputs "out")
                                                   "/bin/wl-color-picker")
                                    `("PATH" =
                                      (,(getenv "PATH")))))))))
    (inputs (list coreutils-minimal
                  bash-minimal
                  grim
                  hicolor-icon-theme
                  imagemagick
                  slurp
                  wl-clipboard
                  zenity))
    (synopsis "Wayland color picker")
    (description
     "@command{wl-color-picker} is a script that provides color picker for
Wayland and @code{wlroots} by leveraging @command{grim} and @command{slurp}.")
    (license license:expat)))

(define-public wl-clipboard
  (package
    (name "wl-clipboard")
    (version "2.2.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/bugaevc/wl-clipboard")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "09l6dv3qsqscbr98vxi4wg4rkx0wlhnsc40n21mx3ds6balmg105"))))
    (build-system meson-build-system)
    (arguments
     (list #:configure-flags
           #~(list (string-append "-Dzshcompletiondir=" #$output
                                  "/share/zsh/site-functions")
                   (string-append "-Dfishcompletiondir=" #$output
                                  "/share/fish/vendor_completions.d"))
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-bash-completion-dir
                 (lambda _
                   (substitute* "completions/bash/meson.build"
                     (("bash_completion_dir =.*")
                      (string-append "bash_completion_dir = "
                                     "join_paths(get_option('sysconfdir'), "
                                     "'bash_completion.d')\n")))))
               (add-after 'unpack 'patch-file-names
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* (find-files "src" "\\.c$")
                     (("\"(cat)\"" _ command)
                      (string-append "\"" (assoc-ref inputs "coreutils")
                                     "/bin/" command "\""))
                     (("\"xdg-mime\"")
                      (string-append "\"" (assoc-ref inputs "xdg-utils")
                                     "/bin/xdg-mime\""))))))))
    (native-inputs
     (list pkg-config))
    (inputs
     (list coreutils wayland wayland-protocols xdg-utils))
    (home-page "https://github.com/bugaevc/wl-clipboard")
    (synopsis "Command-line copy/paste utilities for Wayland")
    (description "Wl-clipboard is a set of command-line copy/paste utilities for
Wayland.")
    (license license:gpl3+)))

(define-public wl-clipboard-x11
  (package
    (name "wl-clipboard-x11")
    (version "5")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/brunelli/wl-clipboard-x11")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1y7jv7rps0sdzmm859wn2l8q4pg2x35smcrm7mbfxn5vrga0bslb"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("src/" "bin/")
         ("man/" "man/man1"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'wrap-binary
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((out          (assoc-ref outputs "out"))
                   (wl-clipboard (assoc-ref inputs "wl-clipboard")))
               (wrap-program (string-append out "/bin/wl-clipboard-x11")
                `("PATH" prefix (,(string-append wl-clipboard "/bin")))))))
         (add-after 'wrap-binary 'symlink-utilities
           ;; As seen in the Makefile.
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/")))
               (symlink "wl-clipboard-x11" (string-append bin "xclip"))
               (symlink "wl-clipboard-x11" (string-append bin "xsel"))))))))
    (inputs
     (list bash-minimal wl-clipboard))
    (home-page "https://github.com/brunelli/wl-clipboard-x11")
    (synopsis "Use wl-clipboard as a drop-in replacement to X11 clipboard tools")
    (description "This package provides a wrapper script around
@code{x11-clipboard} to use it as a clipboard on X11 also.  It also contains
helper scripts for @code{xclip} and @code{xsel} to assist with the transition.")
    (license license:gpl3+)))

(define-public autocutsel
  (package
    (name "autocutsel")
    (version "0.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sigmike/autocutsel"
                                  "/releases/download/" version "/"
                                  "autocutsel-" version ".tar.gz"))
              (sha256
               (base32
                "05zb85imp42birvrc320q20r98qddc5vxx169dnl753l5za0czpi"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f)) ; no "check" target
    (native-inputs (list libx11 libxaw))
    (home-page "https://www.nongnu.org/autocutsel/")
    (synopsis "Automated X11 clipboard and cutbuffer synchronization")
    (description "@code{autocutsel} tracks changes in the server's cutbuffer
and clipboard selection.  When the clipboard is changed, it updates the
cutbuffer.  When the cutbuffer is changed, it owns the clipboard selection.
The cutbuffer and clipboard selection are always synchronized.")
    (license license:gpl2+)))

(define-public jgmenu
  (package
    (name "jgmenu")
    (version "4.4.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/johanmalm/jgmenu")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1a9irlrpa3mi3101cn9hi1ch5k1v4p0h83ni5j63qmlc5g7pcbsh"))))
    (build-system gnu-build-system)
    (native-inputs
     (list cppcheck perl pkg-config))
    (inputs
     (list cairo
           glib
           librsvg
           libx11
           libxml2
           libxrandr
           pango))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "CC" ,(cc-for-target))
             (invoke "./configure"
                     (string-append "--prefix=" (assoc-ref outputs "out"))))))))
    (synopsis "Simple X11 menu")
    (description
     "This is a simple menu for X11 designed for scripting and tweaking.  It
can optionally use some appearance settings from XSettings, tint2 and GTK.")
    (home-page "https://jgmenu.github.io/")
    (license license:gpl2)))

(define-public x-resize
  (package
    (name "x-resize")
    (version "0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://gitlab.com/Apteryks/x-resize")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0bkzc57p8j7cfpcw36hp7bwn16mfby9p1kgfq1lijfqq7d56ps0x"))))
    (build-system gnu-build-system)
    (arguments
     (list
      #:tests? #f                       ;no test suite
      #:modules `(((guix build guile-build-system)
                   #:select (target-guile-effective-version))
                  ,@%default-gnu-imported-modules
                  (srfi srfi-26))
      #:phases
      (with-imported-modules `((guix build guile-build-system)
                               ,@%default-gnu-imported-modules)
        #~(modify-phases %standard-phases
            (add-after 'install 'wrap
              (lambda* (#:key inputs #:allow-other-keys)
                (let* ((version (target-guile-effective-version))
                       (site-ccache (string-append "/lib/guile/" version
                                                   "/site-ccache"))
                       (site (string-append "/share/guile/site/" version))
                       (dep-path
                        (lambda (env path)
                          (list env '=
                                (map (cut string-append <> path)
                                     (list #$output
                                           #$(this-package-input
                                              "guile-lib")
                                           #$(this-package-input
                                              "guile-udev"))))))
                       (bin (string-append #$output "/bin/")))
                  (wrap-program (string-append bin "x-resize")
                    (dep-path "GUILE_LOAD_PATH" site)
                    (dep-path "GUILE_LOAD_COMPILED_PATH" site-ccache)
                    (dep-path "GUILE_EXTENSIONS_PATH" "/lib")))))))))
    (native-inputs (list autoconf automake guile-3.0 pkg-config))
    (inputs (list guile-lib guile-udev xrandr))
    (home-page "https://gitlab.com/Apteryks/x-resize/")
    (synopsis "Dynamic display resizing leveraging udev events")
    (description "The @command{x-resize} command detects physical display
resolution changes via udev and invokes the @command{xrandr} command to
reconfigure the active display resolution accordingly.  It can be used to
implement dynamic resize support for desktop environments that lack native
support such as Xfce.")
    (license license:gpl3+)))

(define-public xwallpaper
  (package
    (name "xwallpaper")
    (version "0.7.3")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stoeckmann/xwallpaper")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1rsv42cl0s149sbpdxz9yqqjip3si95jv3dglwzrcm7pjfg7519v"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake pkg-config))
    (inputs
     (list libjpeg-turbo
           libpng
           libxpm
           pixman
           xcb-util
           xcb-util-image))
    (home-page "https://github.com/stoeckmann/xwallpaper")
    (synopsis "Wallpaper setting utility for X")
    (description
     "The xwallpaper utility allows you to set image files as your X
wallpaper.  JPEG, PNG, and XPM file formats are supported.

The wallpaper is also advertised to programs which support semi-transparent
backgrounds.")
    (license license:isc)))

(define-public xwrits
  (package
    (name "xwrits")
    (version "2.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://www.lcdf.org/~eddietwo/xwrits/"
                           "xwrits-" version ".tar.gz"))
       (sha256
        (base32 "1n7y0fqpcvmzznvbsn14hzy5ddaa3lilm8aw6ckscqndnh4lijma"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'install 'install-docs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (doc (string-append out "/share/doc/xwrits")))
               (install-file "GESTURES" doc)
               (install-file "README" doc)
               #t))))))
    (inputs
     (list libx11 libxinerama))
    (home-page "https://www.lcdf.org/~eddietwo/xwrits/")
    (synopsis "Reminds you to take wrist breaks")
    (description "Xwrits reminds you to take wrist breaks for prevention or
management of repetitive stress injuries.  When you should take a break, it
pops up an X window, the warning window.  You click on the warning window,
then take a break.  The window changes appearance while you take the break.
It changes again when your break is over.  Then you just resume typing.
Xwrits hides itself until you should take another break.")
    (license license:gpl2)))

(define-public wlrctl
  (package
    (name "wlrctl")
    (version "0.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~brocellous/wlrctl")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1g8w7nypxvfnisxwg7jsmafg6hl433p00b5rbpgwcr6v444xqq76"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config scdoc))
    (inputs (list libxkbcommon wayland))
    (home-page "https://git.sr.ht/~brocellous/wlrctl")
    (synopsis "Command line utility for wlroots automation and extensions")
    (description
     "wlrctl is a command line utility for miscellaneous wlroots Wayland
extensions.  At this time, wlrctl supports the foreign-toplevel-mangement
(window/toplevel command), virtual-keyboard (keyboard command), and
virtual-pointer (pointer command) protocols.")
    (license license:expat)))

(define-public xsettingsd
  (package
    (name "xsettingsd")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/derat/xsettingsd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "14gnkz18dipsa2v24f4nm9syxaa7g21iqjm7y65jn849ka2jr1h8"))))
    (build-system cmake-build-system)
    (arguments
     (list #:configure-flags #~(list "-DBUILD_TESTING=ON")
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'disable-problematic-tests
                          (lambda _
                            (substitute* "config_parser_test.cc"
                              ;; This test fails for unknown reasons (see:
                              ;; https://github.com/derat/xsettingsd/issues/30).
                              (("TEST\\(CharStreamTest, Basic")
                               "TEST(CharStreamTest, DISABLED_Basic")))))))
    (inputs (list libx11))
    (native-inputs (list pkg-config googletest))
    (home-page "https://github.com/derat/xsettingsd")
    (synopsis "Xorg settings daemon")
    (description "@command{xsettingsd} is a lightweight daemon that provides settings to
Xorg applications via the XSETTINGS specification.  It is used for defining
font and theme settings when a complete desktop environment (GNOME, KDE) is
not running.  With a simple @file{.xsettingsd} configuration file one can avoid
configuring visual settings in different UI toolkits separately.")
    (license license:bsd-3)))

(define-public clipnotify
  (package
    (name "clipnotify")
    (version "1.0.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/cdown/clipnotify")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1v3ydm5ljy8z1savmdxrjyx7a5bm5013rzw80frp3qbbjaci0wbg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out  (assoc-ref outputs "out"))
                    (bin  (string-append out "/bin"))
                    (doc  (string-append %output "/share/doc/" ,name "-" ,version)))
               (install-file "clipnotify" bin)
               (install-file "README.md" doc)
               #t))))
       #:make-flags
       (list ,(string-append "CC=" (cc-for-target)))
       #:tests? #f))                    ; no test suite
    (inputs
     `(("libx11" ,libx11)
       ("libXfixes" ,libxfixes)))
    (home-page "https://github.com/cdown/clipnotify")
    (synopsis "Notify on new X clipboard events")
    (description "@command{clipnotify} is a simple program that, using the
XFIXES extension to X11, waits until a new selection is available and then
exits.

It was primarily designed for clipmenu, to avoid polling for new selections.

@command{clipnotify} doesn't try to print anything about the contents of the
selection, it just exits when it changes.  This is intentional -- X11's
selection API is verging on the insane, and there are plenty of others who
have already lost their sanity to bring us xclip/xsel/etc.  Use one of those
tools to complement clipnotify.")
    (license license:public-domain)))

(define-public clipmenu
  (let ((commit "7c34ace1fbab76eb1c1dc9b30dd4ac1a7fe4b90b")
        (revision "1"))
    (package
      (name "clipmenu")
      (version (string-append "6.2.0-"
                              revision "." (string-take commit 7)))
      (source
       (origin
         (method git-fetch)
         (uri (git-reference
               (url "https://github.com/cdown/clipmenu")
               (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1403sw49ccb8xsd8v611fzp0csaglfz8nmz3wcjsk8x11h9jvxwy"))))
      (build-system gnu-build-system)
      (arguments
       (list
        #:phases
        #~(modify-phases %standard-phases
            (delete 'configure)
            (delete 'build)
            (replace 'install
              (lambda _
                (let ((bin (string-append #$output "/bin"))
                      (doc (string-append #$output "/share/doc/"
                                          #$name "-" #$version)))
                  (install-file "clipdel" bin)
                  (install-file "clipmenu" bin)
                  (install-file "clipmenud" bin)
                  (install-file "clipfsck" bin)
                  (install-file "clipctl" bin)
                  (install-file "README.md" doc))))
            (add-after 'install 'wrap-script
              (lambda* (#:key inputs outputs #:allow-other-keys)
                (let* ((out               (assoc-ref outputs "out"))
                       (clipnotify        (assoc-ref inputs "clipnotify"))
                       (coreutils-minimal (assoc-ref inputs "coreutils-minimal"))
                       (gawk              (assoc-ref inputs "gawk"))
                       (util-linux        (assoc-ref inputs "util-linux"))
                       (xdotool           (assoc-ref inputs "xdotool"))
                       (xsel              (assoc-ref inputs "xsel"))
                       (guile             (search-input-file inputs "bin/guile")))
                  (for-each
                   (lambda (prog)
                     (wrap-script (string-append out "/bin/" prog)
                       #:guile guile
                       `("PATH" ":" prefix
                         ,(map (lambda (dir)
                                 (string-append dir "/bin"))
                               (list clipnotify coreutils-minimal
                                     gawk util-linux xdotool xsel)))))
                   '("clipmenu" "clipmenud" "clipdel" "clipfsck" "clipctl")))))
            (replace 'check
              (lambda* (#:key inputs outputs #:allow-other-keys)
                ;; substitute a shebang appearing inside a string (the test
                ;; file writes this string to a temporary file):
                (substitute* "tests/test-clipmenu"
                  (("#!/usr/bin/env bash")
                   (string-append "#!" (which "bash"))))
                (invoke "tests/test-clipmenu"))))))
      (inputs
       (list clipnotify
             coreutils-minimal
             gawk
             guile-3.0 ; for wrap-script
             util-linux
             xdotool
             xsel))
      (home-page "https://github.com/cdown/clipmenu")
      (synopsis "Simple clipboard manager using dmenu or rofi and xsel")
      (description "Start @command{clipmenud}, then run @command{clipmenu} to
select something to put on the clipboard.

When @command{clipmenud} detects changes to the clipboard contents, it writes
them out to the cache directory.  @command{clipmenu} reads the cache directory
to find all available clips and launches @command{dmenu} (or @command{rofi},
depending on the value of @code{CM_LAUNCHER}) to let the user select a clip.
After selection, the clip is put onto the PRIMARY and CLIPBOARD X selections.")
      (license license:public-domain))))

(define-public clipman
  (package
    (name "clipman")
    (version "1.6.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/chmouel/clipman")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "033l2hy46r2zjy8dllcmkjxidhnqac9kfh4wkq9hfvim9imp5a4m"))))
    (build-system go-build-system)
    (arguments
     (list
      #:import-path "github.com/yory8/clipman"
      #:install-source? #f
      #:phases
      #~(modify-phases %standard-phases
          (add-before 'build 'patch-wl-copy-path
            (lambda _
              (substitute* "src/github.com/yory8/clipman/main.go"
                (("\"wl-copy\"")
                 (string-append "\"" (which "wl-copy") "\""))))))))
    (inputs
     (list go-github-com-kballard-go-shellquote
           go-gopkg-in-alecthomas-kingpin-v2
           libnotify
           wl-clipboard))
    (synopsis "Basic clipboard manager with support for persisting copy buffers")
    (description
     "A clipboard manager for Wayland that relies on an external selector,
such as @code{wofi}, @code{bemenu}, @code{dmenu}, or @code{rofi}.

Run the binary in your session by adding @command{exec wl-paste -t text --watch
clipman store} (or @command{exec wl-paste -t text --watch clipman store 1>>
PATH/TO/LOGFILE 2>&1 &} to log errors) at the beginning of wherever you
initialize programs.")
    (home-page "https://github.com/chmouel/clipman")
    (license license:gpl3)))

(define-public kbdd
  (package
    (name "kbdd")
    (version "0.7.1")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/qnikst/kbdd")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "0qkq75grbd4wkx4nlvswgavpijk9ad0pzqyj89a0ayjsbsn36pqy"))))
    (build-system gnu-build-system)
    (native-inputs
     (list autoconf automake
           dbus-glib ;; for dbus-binding-tool
           `(,glib "bin") pkg-config))
    (inputs
     (list dbus-glib glib libx11))
    (home-page "https://github.com/qnikst/kbdd")
    (synopsis "Per-window keyboard layout switching daemon for X")
    (description "@command{kbdd} is a simple keyboard layout switching
program, which is designed to run in an X11 session and remember
keyboard layouts on a per-window basis.  That can be very handy for a
user of a non-US keyboard who does not want to jump through layouts back
and forth while typing in terminals (mostly in a latin alphabet) and
some kind of chat (in native language).

@command{kbdd} also supports D-Bus signals, which makes it possible to
create layout indicator widgets.")
    (license license:bsd-2)))

(define-public j4-dmenu-desktop
  (package
    (name "j4-dmenu-desktop")
    (version "3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/enkore/j4-dmenu-desktop")
                    (commit (string-append "r" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "15jlb0si77sndnl5annys0dq2gkwwvzn1d2jwjay9i3xdivzmfb2"))))
    (build-system meson-build-system)
    (native-inputs
     (list catch2-3 pkg-config))
    (inputs (list spdlog-1.15))
    (arguments
     (list
      #:configure-flags
      #~(list #$(string-append
                 "-Denable-tests="
                 (if (%current-target-system)
                     "false"
                     "true")))
      #:phases
      #~(modify-phases %standard-phases
          (add-after 'unpack 'fix-shell-path-in-tests
            (lambda _
              (substitute* (list "tests/ShellUnquote.cc"
                                 "tests/TestFieldCodes.cc")
                (("/bin/sh")
                 (which "sh"))))))))
    (synopsis "Fast desktop menu")
    (description
     "j4-dmenu-desktop is a replacement for i3-dmenu-desktop.  Its purpose
is to find @file{.desktop} files and offer you a menu to start an application
using @command{dmenu}.")
    (home-page "https://github.com/enkore/j4-dmenu-desktop")
    (license license:gpl3+)))

(define-public fuzzel
  (package
    (name "fuzzel")
    (version "1.12.0")
    (home-page "https://codeberg.org/dnkl/fuzzel")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url home-page) (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hsw98v499bz2nxvlw3ykq5qyyad3ci5a7x723r3cl84brabqrp3"))))
    (build-system meson-build-system)
    (arguments
     (list #:build-type "release"
           #:configure-flags #~(list "-Denable-cairo=enabled"
                                     "-Dpng-backend=libpng"
                                     "-Dsvg-backend=librsvg")))
    (native-inputs (append (list pkg-config scdoc tllist)
                           (if (%current-target-system)
                               ;; for wayland-scanner
                               (list pkg-config-for-build
                                     wayland)
                               (list))))
    (inputs (list cairo
                  fcft
                  fontconfig
                  libpng
                  libxkbcommon
                  (librsvg-for-system) ;if librsvg is not used, bundled nanosvg is used
                  pixman
                  wayland
                  wayland-protocols))
    (synopsis "Wayland-native application launcher")
    (description
     "@command{fuzzel} is a Wayland-native application launcher, similar to
rofi's drun mode.  It has Emacs key bindings and remembers frequently launched
applications.  The font and colors can be configured.")
    (license (list license:expat ;fuzzel
                   license:zlib)))) ;; bundled nanosvg

(define-public fyi
  (package
    (name "fyi")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://codeberg.org/dnkl/fyi")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1hm0xr807fgv19dmb9204033hpnpsdanq985j49l8i72gj214sah"))))
    (build-system meson-build-system)
    (native-inputs (list pkg-config))
    (inputs (list dbus))
    (home-page "https://codeberg.org/dnkl/fyi")
    (synopsis "Lightweight alternative to @code{notify-send}")
    (description
     "@acronym{FYI, For Your Information} is a command line utility to send
desktop notifications to the user via a notification daemon implementing XDG
desktop notifications.")
    (license license:expat)))

(define-public wofi
  (package
    (name "wofi")
    (version "1.4.1")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                    (url "https://hg.sr.ht/~scoopta/wofi")
                    (changeset (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1z2pmmwq2h3lfsvdazjiz9s3978rcqan7dqdk5iwk4sz2m96irv9"))))
    (build-system meson-build-system)
    (arguments
     (list #:glib-or-gtk? #t))
    (native-inputs
     (list pkg-config))
    (inputs
     (list gtk+ wayland))
    (synopsis "Launcher/menu program for wayland")
    (description
     "Wofi is a launcher/menu program for wlroots based wayland compositors
such as sway, similar to @command{rofi}.")
    (home-page "https://hg.sr.ht/~scoopta/wofi")
    (license license:gpl3+)))

(define-public nwg-launchers
  (package
    (name "nwg-launchers")
    (version "0.7.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/nwg-piotr/nwg-launchers")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0hq2qiqxvrw3g515ywcb676ljc8mdw3pyslgxr3vahizfljah1pv"))))
    (build-system meson-build-system)
    (native-inputs (list nlohmann-json pkg-config))
    (inputs (list gtk-layer-shell gtkmm-3 (librsvg-for-system)))
    (home-page "https://github.com/nwg-piotr/nwg-launchers")
    (synopsis "Application launchers for wlroots")
    (description
     "This package provides an application grid, button bar, and dmenu
applications for Sway and other wlroots-based Wayland compositors.")
    (license license:gpl3+)))

(define-public dex
  (package
    (name "dex")
    (version "0.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url (string-append "https://github.com/jceb/dex"))
                    (commit (string-append "v" version))))
              (sha256
               (base32
                "03aapcywnz4kl548cygpi25m8adwbmqlmwgxa66v4156ax9dqs86"))
              (file-name (git-file-name name version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f))
    (inputs
     (list python))
    (native-inputs
     (list python-sphinx))
    (home-page "https://github.com/jceb/dex")
    (synopsis "Execute DesktopEntry files")
    (description
     "@command{dex}, @dfn{DesktopEntry Execution}, is a program to generate
and execute @file{.desktop} files of the Application type.")
    (license license:gpl3+)))

(define-public sx
  (package
    (name "sx")
    (version "2.1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/Earnestly/sx")
                    (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xv15m30nhcknasqiybj5wwf7l91q4a4jf6xind8x5x00c6br6nl"))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f                      ; no tests
       #:make-flags
       (let ((out (assoc-ref %outputs "out")))
         (list (string-append "PREFIX=" out)))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'refer-to-xauth
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "sx"
               (("\\bxauth\\b" command)
                (string-append (assoc-ref inputs "xauth") "/bin/" command)))))
         (delete 'configure))))         ; no configure script
    (inputs
     (list xauth))
    (home-page "https://github.com/Earnestly/sx")
    (synopsis "Start an xorg server")
    (description
     "@command{sx} is a simple alternative to both @command{xinit} and
@command{startx} for starting an Xorg server.")
    (license license:x11)))

(define-public hsetroot
  (package
    (name "hsetroot")
    (version "1.0.5")
    (home-page "https://github.com/himdel/hsetroot")
    (source (origin
              (method git-fetch)
              (file-name (git-file-name name version))
              (uri (git-reference
                    (url home-page)
                    (commit version)))
              (sha256
               (base32
                "1jbk5hlxm48zmjzkaq5946s58rqwg1v1ds2sdyd2ba029hmvr722"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       (list
        ,(string-append "CC=" (cc-for-target))
        (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'mkdir-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p (string-append out "/bin"))))))))
    (inputs
     (list libx11 imlib2 libxinerama))
    (native-inputs
     (list pkg-config))
    (synopsis "Imlib2-based wallpaper changer")
    (description
     "The @command{hsetroot} command composes wallpapers for X.
This package is the fork of hsetroot by Hyriand.")
    (license license:gpl2+)))

(define-public hyprcursor
  (package
    (name "hyprcursor")
    (version "0.1.12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprcursor")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1blclwsk0p0990pk1ac9kvnlpafb6kh2knimb8bm25hqpr0xxy92"))))
    (build-system cmake-build-system)
    (arguments (list #:tests? #f))      ;TODO: No themes currently packaged.
    (native-inputs (list gcc-14 pkg-config))
    (inputs (list cairo hyprlang (librsvg-for-system) libzip tomlplusplus))
    (home-page "https://standards.hyprland.org/hyprcursor/")
    (synopsis "Cursor theme format")
    (description
     "This package provides Hyprland cursor format, library and utilities.")
    (license license:bsd-3)))

(define-public hyprlock
  (package
   (name "hyprlock")
   (version "0.8.2")
   (source
    (origin
     (method git-fetch)
     (uri (git-reference
           (url "https://github.com/hyprwm/hyprlock")
           (commit (string-append "v" version))))
     (file-name (git-file-name name version))
     (sha256
      (base32 "1wrndp1bkyfp741mgvjflkbq4pvdlccrh6xaz41y1f9967j8magm"))))
   (build-system cmake-build-system)
   (arguments
    `(#:cmake ,cmake-next
      #:phases
      (modify-phases %standard-phases
                     ;; remove when fixed
                     (add-after 'unpack 'fixgldiscover
                       (lambda* (#:key inputs native-inputs #:allow-other-keys)
                         (substitute* "CMakeLists.txt"
                          (("  opengl")
                           "  gl")
                          (("OpenGL REQUIRED")
                           "OpenGL REQUIRED COMPONENTS GLES2 EGL")))))
      #:tests? #f)) ;; no test
   (native-inputs (list gcc-14 pkg-config))
   (inputs (list cairo
                 file
                 hyprgraphics
                 hyprlang
                 hyprutils
                 hyprwayland-scanner
                 libdrm
                 libjpeg-turbo
                 libwebp
                 libxkbcommon
                 linux-pam
                 mesa
                 pango
                 sdbus-c++
                 wayland
                 wayland-protocols))
   (home-page "https://hyprland.org/")
   (synopsis "Hyprland's screen locking utility")
   (description
    "This package provides Hyprland's simple, yet multi-threaded and
GPU-accelerated screen locking utility.")
   (license license:bsd-3)))

(define-public hyprpaper
  (package
   (name "hyprpaper")
   (version "0.7.5")
   (source (origin
            (method git-fetch)
            (uri (git-reference
                  (url "https://github.com/hyprwm/hyprpaper")
                  (commit (string-append "v" version))))
            (file-name (git-file-name name version))
            (sha256
             (base32
              "0j3hbqfx40cjxkvaiqzfij8pgblg2hyv9lbbjjh4iahciwgh7623"))))
   (build-system cmake-build-system)
   (arguments
    `(#:tests? #f ;; no test
      #:cmake ,cmake-next))
   (native-inputs (list gcc-14 pkg-config))
   (inputs
    (list cairo
          file
          hyprgraphics
          hyprlang
          hyprutils
          hyprwayland-scanner
          libglvnd
          mesa
          pango
          wayland
          wayland-protocols))
   (home-page "https://hyprland.org/")
   (synopsis "Wallpaper utility for Hyprland")
   (description
    "Hyprpaper is a blazing fast wallpaper utility for Hyprland with the ability to
dynamically change wallpapers through sockets.  It will work on all wlroots-based
compositors, though.")
   (license license:bsd-3)))

(define-public hyprpicker
  (package
    (name "hyprpicker")
    (version "0.4.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/hyprwm/hyprpicker")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11sk2kkwqvahp0rf51mr7bax8n2ypqqgydi5hi53aagjsl4xxbdm"))))
    (build-system cmake-build-system)
    (arguments
     (list #:tests? #f                  ;No tests.
           #:phases
           #~(modify-phases %standard-phases
               (add-after 'unpack 'fix-path
                 (lambda* (#:key inputs #:allow-other-keys)
                   (substitute* "src/clipboard/Clipboard.cpp"
                     (("wl-copy" cmd)
                      (search-input-file
                       inputs (string-append "bin/" cmd)))))))))
    (native-inputs (list gcc-14 hyprwayland-scanner pkg-config))
    (inputs
     (list cairo
           hyprutils
           libjpeg-turbo
           libxkbcommon
           pango
           wayland
           wayland-protocols
           wl-clipboard))
    (home-page "https://github.com/hyprwm/hyprpicker")
    (synopsis "Wayland color picker compatible with @code{wlroots}")
    (description
     "This package provides a @code{wlroots}-compatible Wayland color picker.")
    (license license:bsd-3)))

(define-public jumpapp
  (package
    (name "jumpapp")
    (version "1.2")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/mkropat/jumpapp")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "05shgw4agkhgk5vbvc05s12q7m0djc0m4qrd328hmis37bxp9j7n"))))
    (build-system gnu-build-system)
    (arguments `(#:phases
                 (modify-phases %standard-phases
                   (delete 'configure)
                   (delete 'check)
                   (add-before 'install 'set-prefix-in-makefile
                     (lambda* (#:key outputs #:allow-other-keys)
                       (let ((out (assoc-ref outputs "out")))
                         (substitute* "Makefile"
                           (("PREFIX =.*")
                            (string-append "PREFIX = " out "\n")))))))))
    (propagated-inputs
     (list wmctrl xdotool xprop))
    (native-inputs
     (list pandoc perl))
    (synopsis "Run-or-raise application switcher for any X11 desktop")
    (description
     "Bind a key for any given application that will launch the application,
if it's not already running, or focus the application's window,if it is running.
Pressing the key again will cycle to the application's next window,
if there's more than one.")
    (home-page "https://github.com/mkropat/jumpapp")
    (license license:expat)))

(define-public xkbset
  (package
    (name "xkbset")
    (version "0.8")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/stephenmontgomerysmith/xkbset")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "1xa6sgvnwynl2qrjnsppvb2vg4p5v1pisrfhrmnlymw1fzhh6s9p"))))
    (build-system gnu-build-system)
    (inputs
     (list bash-minimal libx11 perl perl-tk))
    (arguments
     `(#:tests? #f                      ; There are none.
       #:make-flags
       `(,,(string-append "CC=" (cc-for-target))
         ,(string-append "X11PREFIX=" %output)
         ,(string-append "X11BASE=" (assoc-ref %build-inputs "libx11"))
         ,(string-append "INSTALL_MAN1=" %output "/share/man/man1"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'create-install-directories
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (mkdir-p out)
               (with-directory-excursion out
                 (for-each mkdir-p '("bin" "share/man/man1"))))
             #t))
         (add-after 'install 'wrap-perl-script
           (lambda* (#:key outputs #:allow-other-keys)
             (wrap-program (string-append (assoc-ref outputs "out")
                                          "/bin/xkbset-gui")
               `("PERL5LIB" ":" prefix (,(getenv "PERL5LIB"))))))
         (replace 'install-license-files
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "COPYRIGHT"
                           (string-append
                            (assoc-ref outputs "out")
                            "/share/doc/" ,name "-" ,version)))))))
    (home-page "https://stephenmontgomerysmith.github.io/software/#xkbset")
    (synopsis "User-preference utility for XKB extensions for X")
    (description
     "This is a program to help manage many of the XKB features of the X Window
System.  This includes such features as MouseKeys, AccessX, StickyKeys,
BounceKeys, and SlowKeys.  It includes a graphical program to help with
MouseKeys-acceleration management.")
    (license license:bsd-3)))

(define-public wlsunset
  (package
    (name "wlsunset")
    (version "0.4.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://git.sr.ht/~kennylevinsen/wlsunset/")
             (commit version)))
       (file-name (git-file-name name version))
       (sha256
        (base32
         "1n60b2qq4ihw0g2dwisrvsvbhpml4mlv42425j0r7xhlm4w93z2k"))))
    (build-system meson-build-system)
    (inputs
     (list wayland wayland-protocols))
    (native-inputs
     (list pkg-config scdoc))
    (synopsis "Day/night gamma adjustments for Wayland compositors")
    (home-page "https://sr.ht/~kennylevinsen/wlsunset/")
    (description
     "Wlsunset adjusts gamma based on day-night cycles on Wayland compositors
that support @samp{wlr-gamma-control-unstable-v1}.  It is also known as a blue
light filter or night light.")
    (license license:expat)))

(define-public ydotool
  (package
    (name "ydotool")
    (version "1.0.4")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/ReimuNotMoe/ydotool")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "0y2n1r6hx0ar1yn636a7j4ywm17qqji84ryhdina3kriwx3sgmij"))))
    (build-system cmake-build-system)
    (arguments '(#:tests? #f))          ; no tests
    (native-inputs (list scdoc))
    (home-page "https://github.com/ReimuNotMoe/ydotool")
    (synopsis "Generic Linux command-line automation tool (no X!)")
    (description "@code{ydotool} is a Linux command-line tool that simulates
keyboard input, mouse actions, etc.  programmatically or manually.")
    (license license:agpl3+)))

(define-public wvkbd
  (package
    (name "wvkbd")
    (version "0.14.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://git.sr.ht/~proycon/wvkbd")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1aha9ylzbkhbf45172l3wyp65kqj6zs5gxqyj62ahj3gp944wmbb"))))
    (build-system gnu-build-system)
    (arguments
     (list #:tests? #f ;no tests
           #:make-flags #~(list (string-append "CC="
                                               #$(cc-for-target))
                                (string-append "PREFIX="
                                               #$output))
           #:phases #~(modify-phases %standard-phases
                        (add-after 'unpack 'fix-cross-compile
                          (lambda _
                            (substitute* "Makefile"
                              (("pkg-config")
                               #$(pkg-config-for-target)))))
                        (delete 'configure))))
    (native-inputs (list wayland ;for wayland-scanner
                         pkg-config))
    (inputs (list libxkbcommon cairo pango harfbuzz wayland))
    (home-page "https://git.sr.ht/~proycon/wvkbd")
    (synopsis "On-screen keyboard for wlroots compositors")
    (description
     "This package provides on-screen keyboard for wlroots compositors with
the following features:

@itemize
@item Typing, modifier locking, layout switching
@item Positive visual feedback on key presses
@item On-the-fly layout and keymap switching
@item Custom color schemes
@item International layouts (cyrillic, arabic)
@item Emoji support
@item Compose key for character variants (e.g. diacritics)
@item Show/hide keyboard on signals (SIGUSR1 = hide, SIGUSR2 = show, SIGRTMIN = toggle)
@item Automatic portrait/landscape detection and subsequent layout switching
@end itemize")
    (license (list license:expat  ;3 files under Expat license (see 'LICENSE')
                   license:gpl3+))))              ;the rest is GPLv3+

(define-public xforms
  ;; The latest stable release is ancient (2014) and fails with a linker
  ;; error, so use the last commit.
  (let ((revision "1")
        (commit "2c1a9f151baf50887a517280645ec23379fb96f8"))
    (package
      (name "xforms")
      (version (git-version "1.3.0" revision commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://git.savannah.gnu.org/git/xforms.git/")
                      (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "12qc1j5g03n2zigvbwilx2zszr8sgv5wd259js7cwf8ffw4lzjf2"))))
      (build-system gnu-build-system)
      (arguments
       (list #:phases #~(modify-phases %standard-phases
                          (add-after 'unpack 'patch-doc-makefile
                            (lambda _
                              (substitute* "doc/Makefile.am"
                                (("/bin/mkdir")
                                 "mkdir")))))
             #:configure-flags #~(list "--enable-docs")))
      (native-inputs (list autoconf
                           automake
                           libtool
                           texinfo
                           texi2html
                           (texlive-local-tree (list texlive-epsf
                                                     texlive-texinfo))
                           imagemagick))
      (propagated-inputs (list libx11 libxpm libjpeg-turbo))
      (home-page "http://xforms-toolkit.org/")
      (synopsis "GUI toolkit for X based on the X11 Xlib library")
      (description
       "XForms is a graphical user interface toolkit for X based on the X11
Xlib library.  It allows you to create windows, containing all kinds of
widgets (buttons, sliders, browsers, menus etc.) with a few lines of code and
then attach actions to the widgets, i.e., have some function called when a
button is pressed.  To make this even easier XForms comes with a program
called @code{fdesign} that allows you to design a GUI for a program directly
on the screen and which then writes out the necessary C code for it.")
      (license license:lgpl2.1+))))

(define-public xforms-gl
  (package/inherit xforms
    (name "xforms-gl")
    (propagated-inputs (modify-inputs (package-propagated-inputs xforms)
                         (append mesa)))
    (synopsis
     "GUI toolkit for X based on the X11 Xlib library, with OpenGL support")))

(define-public xxkb
  (package
    (name "xxkb")
    (version "1.11.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/xxkb/"
                           name "-" version "-src.tar.gz"))
       (sha256
        (base32
         "0hl1i38z9xnbgfjkaz04vv1n8xbgfg88g5z8fyzyb2hxv2z37anf"))))
    (build-system gnu-build-system)
    (inputs (list libx11
                  libxext
                  libxt
                  libxpm))
    (native-inputs
     (list imake))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((imake (assoc-ref inputs "imake"))
                   (out   (assoc-ref outputs "out")))
               (invoke "xmkmf")
               (substitute* "Makefile"
                 ((imake) out)
                 (("(MANPATH = )[[:graph:]]*" _ front)
                  (string-append front out "/share/man"))
                 (("XAPPLOADDIR = /etc/X11/app-defaults")
                  (string-append "XAPPLOADDIR = " out "/lib/X11/app-defaults"))
                 (("ETCX11DIR = /etc/X11")
                  (string-append "ETCX11DIR = " out "/etc/X11")))
               #t))))))
    (home-page "https://xxkb.sourceforge.net/")
    (synopsis "Keyboard layout indicator and switcher")
    (description
     "The xxkb program is a keyboard layout switcher and indicator.  Unlike
the programs that reload keyboard maps and use their own hot-keys, xxkb is
a simple GUI for XKB (X KeyBoard extension) and just sends commands to and
accepts events from XKB.  That means that it will work with the existing
setup of your X Server without any modifications.")
    (license license:artistic2.0)))
